#[derive(Debug)]
pub enum StatementControl {
    Default,
    EarlyReturn(Value),
    Break(Value),
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
pub mod loops;
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

            Expr::Property(a, b) => {
                let (a_value, b_key) = Self::solve_for_avbk(&ctx, a, b);

                let val: Rc<RefCell<Value>> = match &*a_value.borrow() {
                    Value::Tuple(t) => {
                        let idx: usize = b_key.parse::<usize>().expect(">//< non-integer tuple index");

                        t
                            .get(idx)
                            .expect(">//< bad tuple index")
                            .clone()
                    },
                    Value::Mixmap { ordered, keyed } => {
                        if let Ok(idx) = b_key.parse::<usize>() {
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
                    _ => a_value.clone()  // Already Rc<RefCell<Value>>
                };

                return val
            }

            // if it's a function call, we go and handle that
            Expr::FunctionCall { left, args } => {
                let function = Self::expr(ctx.clone(), *left);
                let new_args: Vec<Rc<RefCell<Value>>> = args
                    .iter()
                    .map(|arg| Self::expr(ctx.clone(), arg.clone()).clone())
                    .collect();

                    return Self::do_function_call(ctx, function, new_args)
                        .unwrap_or_else(|| Rc::new(RefCell::new(Value::Null)))
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
                    "+" => lvalue + rvalue,  // these return the same type as the lhs.
                    "-" => lvalue - rvalue,
                    "*" => lvalue * rvalue,
                    "/" => lvalue / rvalue,
                    "==" => Value::Bool(lvalue == rvalue),  // these are stupid and don't do that
                    "!=" => Value::Bool(lvalue != rvalue),
                    _ => unreachable!(">//< I don't know how to preform '{}'!", op)
                }))
            },

            // it it's a loop, we can handle that
            Expr::Loop(body) => match Self::do_loop(ctx, *body) {
                StatementControl::Break(v) => Rc::new(RefCell::new(v)),
                StatementControl::EarlyReturn(v) => Rc::new(RefCell::new(v)),
                _ => unreachable!(">//< I WAS THREE SHEDLETSKIES AWAY HOW DID BRO HIT ME??!")
            },

            Expr::Ref(inner_expr) => {
                let value = Self::expr(ctx.clone(), *inner_expr);
                return Rc::new(RefCell::new(Value::Ref(value)))
            },

            Expr::Deref(inner_expr) => {
                let evaluated = Self::expr(ctx.clone(), *inner_expr); // Rc<RefCell<Value>>

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
                let evaluated = Self::expr(ctx.clone(), *inner_expr); // Rc<RefCell<Value>>

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
            
            Statement::VarDeclaration { left, right } => {
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
            Statement::Break(x) => {
                return StatementControl::Break(
                    Self::expr(ctx, x)
                    .borrow()
                    .clone()
                )
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
        };
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
                StatementControl::Break(_) => panic!("tried to break outside a loop!"),
                StatementControl::Continue => panic!("tried to continue outside a loop!"),
                StatementControl::Default => continue
            }
        };

        return None
    }
}

