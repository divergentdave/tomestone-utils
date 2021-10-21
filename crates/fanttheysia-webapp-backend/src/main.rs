use async_std::task::block_on;

fn main() -> tide::Result<()> {
    let app = tide::new();
    block_on(async {
        app.listen("127.0.0.1:3000").await?;
        Ok(())
    })
}
