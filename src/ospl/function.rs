/// a function spec
#[derive(Debug, Clone)]
pub enum Subspec {
    Bind(String),
    Ignore,
    Destruct(Vec<Subspec>),
    Rest(String),
}
