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
pub mod object;

impl Interpreter {
    fn solve_for_avbk(
        ctx: &Rc<RefCell<Context>>,
        a: Box<Expr>,
        b: Box<Expr>,
    ) -> (Rc<RefCell<Value>>, String) {
        // Resolve a_value (av)
        let a_value: Rc<RefCell<Value>> = Self::expr(ctx.clone(), *a);

        // Resolve b_key (bk)
        let b_key = Self::expr(ctx.clone(), *b)
            .borrow()

            // safe to clone, we literally could not care less about
            // mutability here, we only care about the ID
            .clone()
            .into_id();

        (a_value, b_key)
    }

    fn execute_if_body(body: Block, newctx: Rc<RefCell<Context>>) -> Option<StatementControl> {
        for stmt in body.0 {
            match Self::stmt(newctx.clone(), stmt) {
                StatementControl::Default => {}
                ctrl => return Some(ctrl)
            }
        }
        None
    }

    fn property_access(ctx: Rc<RefCell<Context>>, a: Box<Expr>, b: Box<Expr>) -> Rc<RefCell<Value>> {
        let (a_value, b_key) = Self::solve_for_avbk(&ctx, a, b);

        match &*a_value.borrow() {
            Value::Tuple(t) => {
                // set current instance
                ctx.borrow_mut().current_instance = Some(Rc::downgrade(&a_value.clone()));

                let idx: usize = b_key.parse::<usize>().expect(">//< non-integer tuple index");

                return t
                    .get(idx)
                    .expect(">//< bad tuple index")
                    .clone()
            },

            Value::Mixmap { ordered, keyed } => {
                // set current instance
                ctx.borrow_mut().current_instance = Some(Rc::downgrade(&a_value.clone()));

                return if let Ok(idx) = b_key.parse::<usize>() {
                    // Rc clone
                    ordered
                        .get(idx)
                        .expect(">//< bad mixmap index")
                        .clone()
                } else {
                    // Rc clone
                    keyed
                        .get(&b_key)
                        .expect(">//< bad key")
                        .clone()
                }
            }
            Value::Object { symbols } => {
                // set current instance
                ctx.borrow_mut().current_instance = Some(Rc::downgrade(&a_value.clone()));

                return symbols
                    .get(b_key.as_str())
                    .expect(
                        &format!(
                            "failed to retrive key `{}` in object: `{:#?}`",
                            b_key,
                            symbols
                        )
                    )
                    .clone()
            },
            Value::Module { context } => return context
                .borrow()
                .get(&b_key)
                .expect("expected key or something in the module idfk man WE NEED TO SHIP ALREADY FUCKJKAHDSJKHAKJSHD"),
            _ => return a_value.clone()  // Already Rc<RefCell<Value>>
        };
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
    pub fn expr(ctx: Rc<RefCell<Context>>, expr: Expr) -> Rc<RefCell<Value>> {
        return match expr {
            // if it's a literal we can just unwrap the inner value
            Expr::Literal(v) => Rc::new(RefCell::new(v)),

            // it it's a variable, we try to get this variable
            // cloning here doesn't ACTUALLY clone the value, it just
            // increments its reference count.
            Expr::Variable(left) => {
                let id = Self::expr(ctx.clone(), *left).borrow().clone().into_id();

                ctx.borrow()
                    .get(&id)
                    .unwrap_or_else(|| panic!("oh no `{}` no exist!", id))
                    .clone() // cheap Rc clone
            }

            Expr::Property(a, b) => Self::property_access(ctx, a, b),

            // if it's a function call, we go and handle that
            Expr::FunctionCall { left, args } => {
                let function = Self::expr(ctx.clone(), *left);
                let new_args: Vec<Rc<RefCell<Value>>> = args
                    .iter()
                    .map(|arg| Self::expr(ctx.clone(), arg.clone()).clone())
                    .collect();

                return Self::do_call(Some(ctx.clone()), function, new_args)
                    .unwrap_or_else(|| Rc::new(
                        RefCell::new(
                            Value::Null
                        )
                    )
                )
            }

            // if it's an operation, we do some dispath
            Expr::BinaryOp { left, right, op } => {
                // sorry to bring back trauma from C++
                // I just needed a variable name...

                // make a copy here because we don't want to mess with the original,
                // as binaryops don't modify the original data
                let lvalue: Value = Self::expr(ctx.clone(), *left).borrow().clone();
                let rvalue: Value = Self::expr(ctx.clone(), *right).borrow().clone();

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

                    _ => panic!(">//< I don't know how to preform '{}'!", op)
                }))
            },

            Expr::Ref(inner_expr) => {
                let value = Self::expr(ctx.clone(), *inner_expr);
                return Rc::new(RefCell::new(Value::Ref(value)))
            },

            Expr::Deref(inner_expr) => {
                let evaluated = Self::expr(ctx.clone(), *inner_expr);

                let inner_rc = {
                    let borrowed = evaluated.borrow();       // Ref<Value>
                    if let Value::Ref(inner) = &*borrowed { // match the Value inside
                        Rc::clone(inner)                     // clone the Rc<Value> inside Ref
                    } else {
                        panic!("derefed non-ref value");
                    }
                };

                return inner_rc; // Rc<Value>
            },

            Expr::Construct(inner_expr) => {
                let evaluated = Self::expr(ctx.clone(), *inner_expr);

                return Self::class_construct(ctx, evaluated)
            },

            Expr::TupleLiteral(inner_exprs) => {
                let mut values: Vec<Rc<RefCell<Value>>> = Vec::new();
                for expr in inner_exprs {
                    let val = Self::expr(ctx.clone(), expr.borrow().clone());
                    values.push(val);
                }
                return Rc::new(
                    RefCell::new(
                        Value::Tuple(values)
                    )
                );
            },

            Expr::ClassLiteral { parents, symbols } => {
                // compute symbols
                let mut new_symbols: HashMap<String, Rc<RefCell<Value>>> = HashMap::new();
                for (id, symbol) in symbols {
                    let val = Self::expr(ctx.clone(), symbol.borrow().clone());
                    new_symbols.insert(id, val);
                }

                // compute parents
                let mut new_parents: Vec<Rc<RefCell<Value>>> = Vec::new();
                for parent in &*parents {
                    let val = Self::expr(ctx.clone(), parent.borrow().clone());
                    new_parents.push(val);
                }

                return Rc::new(
                    RefCell::new(
                        Value::Class {
                            parents: new_parents,
                            symbols: new_symbols
                        }
                    )
                )
            },

            Expr::MixmapLiteral { positional, keyed } => {
                let mut new_keyed: HashMap<String, Rc<RefCell<Value>>> = HashMap::new();
                for (k, ex) in keyed {
                    let val = Self::expr(ctx.clone(), ex.borrow().clone());
                    new_keyed.insert(k, val);
                }

                let mut new_ordered: Vec<Rc<RefCell<Value>>> = Vec::new();
                for ex in positional {
                    let val = Self::expr(ctx.clone(), ex.borrow().clone());
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
                let mut new_hm: HashMap<String, Rc<RefCell<Value>>> = HashMap::new();
                for (k, ex) in hm {
                    let val = Self::expr(ctx.clone(), ex.borrow().clone());
                    new_hm.insert(k, val);
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
                            ctx: Rc::downgrade(&ctx),
                            spec,
                            body
                        }
                    )
                )
            },

            Expr::Import(ast) => {
                // create a new context for this module
                let newctx =
                    Rc::new(
                        RefCell::new(
                            Context::new(
                                None
                            )
                        )
                    );


                // evaluate all that shit
                for stmt in ast {
                    // we don't care about the return here
                    Self::stmt(newctx.clone(), stmt);
                };

                // put it into an object
                let object = Value::Module {
                    // this line is not efficient! Too bad!
                    context: newctx
                };

                return Rc::new(
                    RefCell::new(object)
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
    /// * `stmt` - The `Statement` to execute.
    /// 
    /// # Returns
    /// 
    /// The control flow of the statement as `StatementControl`
    pub fn stmt(ctx: Rc<RefCell<Context>>, stmt: Statement) -> StatementControl {
        match stmt {
            Statement::Assign { left, right } => {
                let var: Rc<RefCell<Value>> = Self::expr(ctx.clone(), *left);
                let lit: Rc<RefCell<Value>> = Self::expr(ctx.clone(), *right);

                *var.borrow_mut() = lit.borrow().clone();
                return StatementControl::Default
            },
            
            Statement::Declaration { left, right } => {
                let var: String = Self::expr(ctx.clone(), *left)
                    .borrow()
                    .into_id();

                let lit: Rc<RefCell<Value>> = Self::expr(ctx.clone(), *right);

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
                let to_print = Self::expr(ctx, *thing);
                print!("{}", to_print.borrow());
                return StatementControl::Default
            }

            // not even I know why this works, if it does at all!
            Statement::Check { matching, cases } =>
                return Self::preform_check(ctx.clone(), matching, cases),

            Statement::Select { matching, cases } => 
                return Self::preform_select(ctx.clone(), matching, cases),

            Statement::Loop(body) => match Self::do_loop(ctx, *body) {
                StatementControl::Break => return StatementControl::Default,
                StatementControl::EarlyReturn(v) => return StatementControl::EarlyReturn(v),  // may or may not fucking work
                _ => unreachable!("you might actually be stupid")
            },

            Statement::Delete { left } => {
                match *left {
                    Expr::Variable(v) => ctx.borrow_mut().delete(
                        &Self::expr(ctx.clone(), *v).borrow().into_id()
                    ),
                    _ => panic!("I don't know how the hell to delete that")
                };
                return StatementControl::Default;
            }
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
    pub fn block(ctx: Rc<RefCell<Context>>, body: Block) -> Option<Rc<RefCell<Value>>> {
        for stmt in body.0 {
            let control: StatementControl = Self::stmt(ctx.clone(), stmt);
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
