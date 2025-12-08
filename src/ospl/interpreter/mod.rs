#[derive(Debug)]
pub enum StatementControl {
    Default,
    EarlyReturn(Value),
    Break,
    Continue,
}

use super::*;
use std::rc::Rc;
use std::cell::RefCell;

pub mod context;
pub use context::*;

/// executes and evaluates OSPL ASTs
pub struct Interpreter;

pub mod function;
pub mod controlflow;
pub mod assignop;
impl Interpreter {
    // TODO: optimize this ugly shit
    pub fn error_stmt(span: &SpannedStatement, msg: &str) -> ! {
        panic!(
            "{}:{}: {}",
            &*span.filename,
            span.line,  // line numbers start at 1, not 0..
            msg,
        )
    }

    pub fn error_expr(span: &SpannedExpr, msg: &str) -> ! {
        panic!(
            "{}:{}:{}: {}",
            &*span.filename,
            span.line,  // line numbers start at 1, not 0..
            span.column,
            msg,
        )
    }

    fn solve_for_avbk(
        ctx: &Rc<RefCell<Context>>,
        a: &SpannedExpr,
        b: &SpannedExpr,
    ) -> (Rc<RefCell<Value>>, String) {
        // Resolve a_value (av)
        let a_value: Rc<RefCell<Value>> = Self::expr(ctx.clone(), a);

        // Resolve b_key (bk)
        let b_key = Self::expr(ctx.clone(), b)
            .borrow()

            // safe to clone, we literally could not care less about
            // mutability here, we only care about the ID
            .into_id();

        (a_value, b_key)
    }

    fn execute_if_body(body: &Block, newctx: Rc<RefCell<Context>>) -> Option<StatementControl> {
        for stmt in &body.stmts {
            match Self::stmt(newctx.clone(), &stmt) {
                StatementControl::Default => {}
                ctrl => return Some(ctrl)
            }
        }
        None
    }

    // TODO: span Expr too
    fn property_access(ctx: Rc<RefCell<Context>>, a: &SpannedExpr, b: &SpannedExpr) -> Rc<RefCell<Value>> {
        let (a_value, b_key) = Self::solve_for_avbk(&ctx, a, b);

        match &*a_value.borrow() {
            Value::Tuple(t) => {
                // if our key is a special tuple attribute
                match b_key.as_str() {
                    "len" => return Rc::new(
                        RefCell::new(
                            Value::QuadrupleWord(
                                t.len() as u64
                            )
                        )
                    ),
                    _ => {}
                }

                // set current instance
                ctx.borrow_mut().current_instance = Some(a_value.clone());

                let idx: usize = b_key.parse::<usize>()
                    .unwrap_or_else(|e| Self::error_expr(b, &format!("failed to parse tuple index (is it not an integer?): {}", e)));

                return t
                    .get(idx)
                    .cloned()
                    .unwrap_or_else(|| Rc::new(RefCell::new(Value::Undefined)))
            },

            Value::Mixmap { ordered, keyed } => {
                // if our key is a special attribute
                match b_key.as_str() {
                    // lengths
                    "len" => return Rc::new(
                        RefCell::new(
                            Value::QuadrupleWord(
                                ordered.len() as u64 + keyed.len() as u64
                            )
                        )
                    ),
                    "npos" => return Rc::new(RefCell::new(Value::QuadrupleWord(ordered.len() as u64))),
                    "nkey" => return Rc::new(RefCell::new(Value::QuadrupleWord(keyed.len() as u64))),

                    // TODO: stop doing this
                    // convert keys to object (dunno why I'm doing this here and not on the fucking typecast.. ehsy!)
                    // not final, probs get removed in 0.1.0 or 1.0.0
                    "keys" => return Rc::new(RefCell::new(Value::Object {
                        symbols: keyed.clone()
                    })),

                    // convert positionals to object (dunno why I'm doing this here and not on the fucking typecast.. ehsy!)
                    // not final, probs get removed in 0.1.0 or 1.0.0
                    "positionals" => return Rc::new(RefCell::new(Value::Tuple(ordered.clone()))),

                    _ => {}
                }

                // ==== NORMAL CASE ====
                // set current instance
                ctx.borrow_mut().current_instance = Some(a_value.clone());

                return if let Ok(idx) = b_key.parse::<usize>() {
                    // Rc clone
                    ordered
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| Rc::new(RefCell::new(Value::Undefined)))
                } else {
                    // Rc clone
                    keyed
                        .get(&b_key)
                        .cloned()
                        .unwrap_or_else(|| Rc::new(RefCell::new(Value::Undefined)))
                }
            },

            Value::Object { symbols } => {
                // if our key is a special attribute
                match b_key.as_str() {
                    "len" | "nkey" => return Rc::new(RefCell::new(Value::QuadrupleWord(symbols.len() as u64))),
                    _ => {}
                }

                // ==== NORMAL CASE ====
                // set current instance
                ctx.borrow_mut().current_instance = Some(a_value.clone());

                return symbols
                    .get(&b_key)
                    .cloned()
                    .unwrap_or_else(|| Rc::new(RefCell::new(Value::Undefined)))
                 // .unwrap_or_else(|| Self::error_expr(b, &format!("key not found: {}", b_key)))
            },

            Value::Module { context } =>
                return context
                    .borrow()
                    .get(&b_key)
                    .unwrap_or_else(|| Self::error_expr(b, &format!("key not found: {}", b_key))),

            Value::ForeignLib { library } => {
                return Rc::new(RefCell::new(Value::ForeignSymbol {
                    library: library.clone(),
                    symbol: b_key,
                }));
            }

            Value::ForeignSymbol { library, symbol } => {
                // allow chained property access: treat as namespace under symbol
                let combined = format!("{}::{}", symbol, b_key);
                return Rc::new(RefCell::new(Value::ForeignSymbol {
                    library: library.clone(),
                    symbol: combined,
                }));
            },

            Value::String(s) => {
                if let Ok((a, b)) = scan_fmt::scan_fmt!(&b_key, "{}_{}", usize, usize) {
                    return Rc::new(RefCell::new(Value::String(s[a..b].to_string())))
                }

                if let Ok(a) = &b_key.parse::<usize>() {
                    let actual_a = *a;
                    return Rc::new(RefCell::new(Value::String(s[actual_a..actual_a+1].to_string())))
                }

                match b_key.as_str() {
                    "len" => return Rc::new(RefCell::new(Value::QuadrupleWord(s.len() as u64))),
                    "to_upper" => return Rc::new(RefCell::new(Value::String(s.to_uppercase()))),
                    "to_lower" => return Rc::new(RefCell::new(Value::String(s.to_lowercase()))),
                    "lines" => return Rc::new(RefCell::new(Value::Tuple(
                        s
                            .split('\n')
                            .map(|x| Rc::new(RefCell::new(Value::String(x.to_string()))))
                            .collect()
                    ))),
                    _ => panic!("type `str` has no property {}", b_key)
                }
            }

            _ => Self::error_expr(a, &format!("property access is not supported on values of this type"))  // TODO: show more info for this error
            // _ => a_value.clone()
        }
    }

    /// Evaluates an `Expr`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context to evaluate this expression in.
    ///           This context is mutated
    /// * `expr` - The `Expr` to evaluate.
    /// 
    /// # Returns
    /// 
    /// The value of this Expr, as `Value`
    pub fn expr(ctx: Rc<RefCell<Context>>, expr: &SpannedExpr) -> Rc<RefCell<Value>> {
        return match &expr.expr {
            // if it's a literal we can just unwrap the inner value
            Expr::Literal(v) => Rc::new(RefCell::new(v.clone())),

            // it it's a variable, we try to get this variable
            // cloning here doesn't ACTUALLY clone the value, it just
            // increments its reference count.
            Expr::Variable(left) => {
                let id = Self::expr(ctx.clone(), &left).borrow().into_id();
                // check if the ID is a *SPECIAL* literal
                match id.as_ref() {
                    // get OS platform
                    "OSPL_current_os" => Rc::new(RefCell::new(Value::String(std::env::consts::OS.to_string()))),
                    "OSPL_current_arch" => Rc::new(RefCell::new(Value::String(std::env::consts::ARCH.to_string()))),
                    "OSPL_current_family" => Rc::new(RefCell::new(Value::String(std::env::consts::FAMILY.to_string()))),
                    _ => ctx.borrow()
                            .get(&id)
                            .unwrap_or_else(|| Self::error_expr(expr, &format!("variable not found: {}", id)))
                            .clone()  // cheap Rc clone
                }
            }

            Expr::Property(a, b) => Self::property_access(ctx, &*a, &*b),

            // if it's a function call, we go and handle that
            Expr::FunctionCall { left, args } => {
                let function = Self::expr(ctx.clone(), &*left);
                // remember the instance the callee was resolved from so argument
                // evaluation doesn't overwrite it (e.g., nested property accesses)
                let call_instance = ctx.borrow().current_instance.clone();

                let mut new_args: Vec<Rc<RefCell<Value>>> = Vec::with_capacity(args.len());
                for arg in args {
                    let value = Self::expr(ctx.clone(), arg);
                    new_args.push(value);
                    // restore the original call instance after each arg
                    ctx.borrow_mut().current_instance = call_instance.clone();
                }

                // colton says: dunno how this works so I can't optimize it without kevin's help..
                match &*function.borrow() {
                    Value::ForeignFn { library, symbol, .. } => {
                        let mut args_clone = Vec::new();
                        for arg in &new_args {
                            let values = arg.borrow().as_values();
                            for v in values {
                                args_clone.push(v.borrow().clone());
                            }
                        }

                        let mut ctx_mut = ctx.borrow_mut();
                        let result = crate::ospl::ffi::call_foreign_function(
                            &mut ctx_mut.ffi_registry,
                            library,
                            symbol,
                            &args_clone,
                        ).unwrap_or_else(|e| Self::error_expr(expr, &format!("CFFI function call failed: {}", e)));
                        return Rc::new(RefCell::new(result));
                    }
                    _ => {}
                }

                return Self::do_call(Some(ctx.clone()), function, new_args)
                    .unwrap_or_else(|e| Self::error_expr(expr, &format!("error during call: {:?}", e)))
                    .unwrap_or_else(|| Rc::new(RefCell::new(Value::Null)))
            }

            // if it's an operation, we do some dispath
            Expr::BinaryOp { left, right, op } => {
                // sorry to bring back trauma from C++
                // I just needed a variable name...

                // make a copy here because we don't want to mess with the original,
                // as binaryops don't modify the original data
                let lvalue: Value = Self::expr(ctx.clone(), &*left).borrow().clone();
                let rvalue: Value = Self::expr(ctx.clone(), &*right).borrow().clone();

                // dispatch the correct op
                return Rc::new(RefCell::new(match op.as_str() {
                    // normal ones
                    "+" => lvalue + rvalue,  // these return the same type as the lhs.
                    "-" => lvalue - rvalue,
                    "*" => lvalue * rvalue,
                    "/" => lvalue / rvalue,

                    // comparison
                    "==" => Value::Bool(lvalue == rvalue),  // these are stupid and don't do that
                    "!=" => Value::Bool(lvalue != rvalue),
                    "<" => Value::Bool(lvalue < rvalue),
                    ">" => Value::Bool(lvalue > rvalue),
                    ">=" => Value::Bool(lvalue >= rvalue),
                    "<=" => Value::Bool(lvalue <= rvalue),

                    // logical and/or/xor
                    "&" => Value::Bool(lvalue.truthiness() && rvalue.truthiness()),
                    "|" => Value::Bool(lvalue.truthiness() || rvalue.truthiness()),
                    "^" => Value::Bool(lvalue.truthiness() ^ rvalue.truthiness()),

                    // bitwise and/or/xor
                    "&&" => lvalue & rvalue,
                    "||" => lvalue | rvalue,
                    "^^" => lvalue ^ rvalue,

                    // shifting
                    ">>" => lvalue >> rvalue,
                    "<<" => lvalue << rvalue,

                    _ => Self::error_expr(expr, "I don't know how to preform '{}'!")
                }))
            },

            Expr::UnaryOp { left, op } => {
                // make a copy here because we don't want to mess with the original,
                // as unaryops don't modify the original data
                let value: Value = Self::expr(ctx.clone(), &left).borrow().clone();

                return Rc::new(RefCell::new(
                    match &**op {
                        "!" => Value::Bool(!value.truthiness()),  // invert truthiness
                        "!!" => todo!("implement !!"),
                        "typeof" => todo!("implement typeof"),
                        _ => unimplemented!("idk what {} is in unary ops", op)
                    }
                ))
            },

            Expr::Ref(inner_expr) => {
                let value = Self::expr(ctx.clone(), &*inner_expr);
                return Rc::new(RefCell::new(Value::Ref(value)))
            },

            Expr::Deref(inner_expr) => {
                let evaluated = Self::expr(ctx.clone(), &*inner_expr);
                let borrowed = evaluated.borrow();

                match &*borrowed {
                    Value::Ref(inner_rc) => Rc::clone(inner_rc),      // normal OSPL Ref

                    // VERY BAD IDEA
                    Value::QuadrupleWord(mistake) => {
                        unsafe {
                            let ptr = (*mistake) as *mut u8;
                            Rc::new(
                                RefCell::new(
                                    Value::Byte(
                                        *ptr
                                    )
                                )
                            )
                        }
                    }
                    other => panic!("derefed non-ref value: {:?}", other),
                }
            }

            Expr::TupleLiteral(inner_exprs) => {
                let mut values: Vec<Rc<RefCell<Value>>> = Vec::new();
                for expr in inner_exprs {
                    let val = Self::expr(ctx.clone(), &*expr.borrow());
                    values.push(val);
                }
                return Rc::new(
                    RefCell::new(
                        Value::Tuple(values)
                    )
                );
            },

            Expr::MixmapLiteral { positional, keyed } => {
                let mut new_keyed: HashMap<Rc<String>, Rc<RefCell<Value>>> = HashMap::new();
                for (k, ex) in keyed {
                    let val = Self::expr(ctx.clone(), &*ex.borrow());

                    // `new_keyed`'s `k` here is guaranteed to live as long as `keyed`'s `k` does here.
                    // what did I just type? I don't know!
                    new_keyed.insert(k.clone(), val);  // FIXME: replace keys with Rc<str> or &str and remove the clone
                }

                let mut new_ordered: Vec<Rc<RefCell<Value>>> = Vec::new();
                for ex in positional {
                    let val = Self::expr(ctx.clone(), &*ex.borrow());
                    new_ordered.push(val);
                }

                // compute new positional
                return Rc::new(
                    RefCell::new(
                        Value::Mixmap {
                            ordered: new_ordered,
                            keyed: new_keyed
                        }
                    )
                )
            },

            Expr::ObjectLiteral(hm) => {
                let mut new_hm: HashMap<Rc<String>, Rc<RefCell<Value>>> = HashMap::new();
                for (k, ex) in hm {
                    let val = Self::expr(ctx.clone(), &*ex.borrow());
                    new_hm.insert(k.clone(), val);
                }

                return Rc::new(
                    RefCell::new(
                        Value::Object { symbols: new_hm }
                    )
                )
            },

            Expr::RealFnLiteral { spec, body } => {
                return Rc::new(
                    RefCell::new(
                        Value::RealFn {
                            ctx: ctx.clone(),
                            spec: spec.clone(),  // performance cost is acceptable here (we don't run this function very often.. or at least no good programmer should...)
                            body: body.clone(), 
                        }
                    )
                )
            },

            Expr::Import { ast, .. } => {
                // create a new context for this module
                let newctx =
                    Rc::new(
                        RefCell::new(
                            Context::new(
                                None
                            )
                        )
                    );

                // UGLY HACK: steal the FFI registry from the parent module
                // THIS IS ALSO VERY SLOW
                // **DISABLED** because it doesn't work
                //newctx.borrow_mut().ffi_registry = ctx.borrow().ffi_registry.clone();

                // evaluate all that shit
                for stmt in ast {
                    // we don't care about the return here
                    Self::stmt(newctx.clone(), &stmt);
                };

                // put it into an object
                let object = Value::Module {
                    // this line is not efficient! Too bad!
                    context: newctx
                };

                return Rc::new(
                    RefCell::new(object)
                );
            },

            // not run often so preformance here is pretty much moot
            Expr::ForeignFunctionLiteral { library, symbol, arg_types, return_type } => {
                let mut ctx_mut = ctx.borrow_mut();
                let registry = &mut ctx_mut.ffi_registry;

                if !registry.has_library(&library) {
                    panic!("library '{}' not loaded; use import to load it", library);
                }

                registry.register_function(
                    library.clone(),
                    symbol.clone(),
                    symbol.clone(),
                    arg_types.clone(),
                    return_type.clone(),
                ).unwrap_or_else(|e| Self::error_expr(expr, &format!("failed to register C FFI function (why is this written twice in the code?): {}", e)));

                return Rc::new(RefCell::new(Value::ForeignFn {
                    library: library.clone(),
                    symbol: symbol.clone(),
                    arg_types: arg_types.clone(),
                    return_type: return_type.clone(),
                }));
            },

            // called even LESS so NOBODY fucking cares about preformance here
            Expr::CffiLoad { path } => {
                let path_str = Self::expr(ctx.clone(), &path).borrow().clone();  // TODO: maybe stop this

                let mut ctx_mut = ctx.borrow_mut();
                ctx_mut.ffi_registry.load_library(path_str.into_id(), &path_str.into_id())
                    .unwrap_or_else(|e| panic!("Failed to load library '{}': {}", path_str, e));

                return Rc::new(RefCell::new(Value::ForeignLib { library: path_str.into_id() }));
            },

            Expr::CffiFn { target, arg_types, return_type } => {
                let target_value = Self::expr(ctx.clone(), &*target);
                let (library, symbol) = match &*target_value.borrow() {
                    Value::ForeignSymbol { library, symbol } => (library.clone(), symbol.clone()),
                    Value::ForeignFn { library, symbol, .. } => (library.clone(), symbol.clone()),
                    _ => panic!("CFFI_Fn target must be a foreign symbol"),
                };

                let mut ctx_mut = ctx.borrow_mut();
                let registry = &mut ctx_mut.ffi_registry;

                if !registry.has_library(&library) {
                    panic!("library '{}' not loaded; use CFFI_Load first", library);
                }

                registry.register_function(
                    library.clone(),
                    symbol.clone(),
                    symbol.clone(),
                    arg_types.clone(),
                    return_type.clone(),
                ).unwrap_or_else(|e| Self::error_expr(expr, &format!("CFFI function registration failed: {}", e)));

                return Rc::new(RefCell::new(Value::ForeignFn {
                    library,
                    symbol,
                    arg_types: arg_types.clone(),
                    return_type: return_type.clone(),
                }));
            },

            // THIS IS AWFUL
            Expr::TypeCast { left, into, mode: TypeCastMode::Convert } => {
                let value = Self::expr(ctx.clone(), &*left);
                let thing = match (&*value.borrow(), &into) {
                    // stupid
                    (Value::Byte(b), Type::String) => Value::String(String::from(*b as char)),
                    (Value::SignedDoubleWord(a), Type::QuadrupleWord) => Value::QuadrupleWord(*a as u64),
                    (Value::SignedDoubleWord(a), Type::DoubleWord) => Value::DoubleWord(*a as u32),
                    (Value::Byte(a), Type::DoubleWord) => Value::DoubleWord(*a as u32),

                    (Value::SignedQuadrupleWord(a), Type::QuadrupleWord) => Value::QuadrupleWord(*a as u64),
                    (Value::QuadrupleWord(a), Type::String) => Value::String(a.to_string()),

                    (Value::String(s), Type::Byte) => Value::Byte(s.parse::<u8>().expect("failed to cast str to byte, invalid u8")),
                    (Value::String(s), Type::DoubleWord) => Value::DoubleWord(s.parse::<u32>().expect("failed to cast str to byte, invalid u32")),

                    (Value::String(s), Type::Tuple { .. }) => Value::Tuple(
                        s
                        .as_bytes()
                        .iter()
                        .map(|x| Rc::new(RefCell::new(Value::Byte(*x))))
                        .collect()
                    ),

                    // more will be added as needed
                    _ => panic!("idk how to cast {:?} into {:?}", value, into)
                };

                return Rc::new(
                    RefCell::new(
                        thing
                    )
                )
            },

            Expr::TypeCast { mode: TypeCastMode::Reinterpret, .. } => {
                unimplemented!("reinterpret cast is not yet implemented.")
            },

            Expr::TypeCast { left, into, mode: TypeCastMode::PointerReinterpret } => {
                unsafe {
                    let value = Self::expr(ctx.clone(), &*left);

                    let thing = match (&*value.borrow(), &into) {
                        // quadruple word is a ptr
                        (Value::QuadrupleWord(a), Type::String) => casts::ptr_to_string(*a as *const u8),
                        (Value::QuadrupleWord(a), Type::Tuple { length: Some(length) }) => casts::ptr_to_tuple(*a as *const u8, *length),
                        _ => panic!("idk how to cast {:?} into {:?}", value, into)
                    };

                    return Rc::new(
                        RefCell::new(
                            thing
                        )
                    )
                }
            },

            Expr::DeepCopy(of) => {
                let value = Self::expr(ctx.clone(), &*of);
                return Rc::new(
                    RefCell::new(
                        value.borrow().deep_clone()
                    )
                );
            }
        }
    }

    /// Executes a `Statement`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context in which to execute the statement.
    ///           This context is mutated.
    /// * `stmt` - The `SpannedStatement` to execute.
    /// 
    /// # Returns
    /// 
    /// The control flow of the statement as `StatementControl`
    pub fn stmt(ctx: Rc<RefCell<Context>>, span: &SpannedStatement) -> StatementControl {
        match &span.stmt {
            Statement::Assign { left, right } => {
                let var: Rc<RefCell<Value>> = Self::expr(ctx.clone(), left);
                let lit: Rc<RefCell<Value>> = Self::expr(ctx.clone(), right);

                *var.borrow_mut() = lit.borrow().clone();  // here
                return StatementControl::Default
            },

            Statement::AssignOp { left, right, op } => {
                // TODO: maybe do something here idk
                let var: Rc<RefCell<Value>> = Self::expr(ctx.clone(), left);
                let lit: Rc<RefCell<Value>> = Self::expr(ctx.clone(), right);
                let v: std::cell::RefMut<'_, Value> = var.borrow_mut();
                let x: std::cell::Ref<'_, Value> = lit.borrow();

                match &**op {
                    "+=" => Self::handle_add_assign(v, x),
                    "-=" => Self::handle_sub_assign(v, x),
                    "*=" => Self::error_stmt(&span, "TODO: implement"),
                    "/=" => Self::error_stmt(&span, "TODO: implement"),
                    "%=" =>Self::error_stmt(&span, "TODO: implement"),

                    "||=" => Self::error_stmt(&span, "TODO: implement"),
                    "&&=" => Self::error_stmt(&span, "TODO: implement"),
                    "^^=" => Self::error_stmt(&span, "TODO: implement"),
                    "<<=" => Self::error_stmt(&span, "TODO: implement"),
                    ">>=" => Self::error_stmt(&span, "TODO: implement"),
                    _ => Self::error_stmt(&span, "unknown operation")
                }
                return StatementControl::Default
            },
            
            Statement::Declaration { left, right } => {
                let var: String = Self::expr(ctx.clone(), left)
                    .borrow()
                    .into_id();

                let lit: Rc<RefCell<Value>> = Self::expr(ctx.clone(), right);

                //ctx.borrow_mut().set(&var, lit.borrow().clone());
                ctx.borrow_mut().declare(&var, lit);
                return StatementControl::Default
            }

            Statement::Expression(x) => {
                Self::expr(ctx.clone(), x);
                return StatementControl::Default
            },

            Statement::Return(x) => {
                return StatementControl::EarlyReturn(
                    Self::expr(ctx, x)
                        .borrow()
                        .clone()
                )
            },
            Statement::Break => {
                return StatementControl::Break
            },

            Statement::Continue => {
                return StatementControl::Continue
            },

            Statement::If { condition, on_true, on_false } => {
                let condition_return: bool = Self::expr(ctx.clone(), condition)
                    .borrow()
                    .truthiness();

                let newctx: Rc<RefCell<Context>> =
                    Rc::new(RefCell::new(Context::new(Some(ctx.clone()))));

                if condition_return {
                    if let Some(control) = Self::execute_if_body(on_true, newctx) {
                        return control;
                    }
                } else if let Some(block) = on_false {
                    if let Some(control) = Self::execute_if_body(block, newctx) {
                        return control;
                    }
                }
                return StatementControl::Default
            }

            // nice to have for testing
            Statement::Print { thing } => {
                let to_print = Self::expr(ctx, thing);
                print!("{}", to_print.borrow());
                return StatementControl::Default
            }

            // not even I know why this works, if it does at all!
            Statement::Check { matching, cases } =>
                return Self::preform_check(ctx.clone(), matching, cases),

            Statement::Select { matching, cases } => 
                return Self::preform_select(ctx.clone(), matching, cases),

            Statement::Loop(body) => match Self::do_loop(ctx, body) {
                StatementControl::Break => return StatementControl::Default,
                StatementControl::EarlyReturn(v) => return StatementControl::EarlyReturn(v),  // may or may not fucking work
                _ => unreachable!("you might actually be stupid")
            },

            Statement::Delete { .. } => {
                todo!("(re)implement delete... COLTON!");
            },

            Statement::ImportLib { name, path } => {
                match ctx.borrow_mut().ffi_registry.load_library(name.clone(), &path) {
                    Ok(_) => {},
                    Err(e) => Self::error_stmt(
                        span, 
                        &format!("Failed to load library: {}", e)  // ehsy?
                    ),
                }
                return StatementControl::Default;
            },

            Statement::Memcopy { address, value } => {
                let a = Self::expr(ctx.clone(), address);
                let addr = a.borrow().as_usize();
                let v = Self::expr(ctx.clone(), value);
                unsafe {
                    casts::write_value_to_memory(addr, v);
                }
                return StatementControl::Default;
            },
        }
    }

    /// Executes a `Block`
    /// 
    /// # Arguments
    /// 
    /// * `ctx` - The context in which to execute the block.
    ///           This context is mutated.
    /// * `body` - The `Block` to execute
    /// 
    /// # Returns
    /// 
    /// The return of this block, if there is one, as `Option<Value>`
    pub fn block(ctx: Rc<RefCell<Context>>, body: &Block) -> Option<Rc<RefCell<Value>>> {
        for stmt in &body.stmts {
            let control: StatementControl = Self::stmt(ctx.clone(), &stmt);
            match control {
                StatementControl::EarlyReturn(x) => return Some(Rc::new(RefCell::new(x))),
                StatementControl::Break => panic!("tried to break outside a loop!"),
                StatementControl::Continue => panic!("tried to continue outside a loop!"),
                StatementControl::Default => continue
            }
        };

        return None
    }
}
