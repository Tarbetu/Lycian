pub trait Resolver {
    type Product;

    fn resolve(&self, name: &str) -> Result<Self::Product, ResolutionError>;
}

pub enum ResolutionError {}
