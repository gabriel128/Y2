mod ast;
mod passes;

pub struct CompileError {}

pub type Result<T> = std::result::Result<T, CompileError>;

fn main() {
    println!("Hello, world!");
}
