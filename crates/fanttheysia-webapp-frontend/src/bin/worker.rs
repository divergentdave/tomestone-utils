use fanttheysia_webapp_frontend::SyntaxChecker;
use gloo_worker::Registrable;

fn main() {
    SyntaxChecker::registrar().register();
}
