mod lsp;

use std::io;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut reader = io::BufReader::new(stdin.lock());
    let stdout = io::stdout();
    let mut writer = io::BufWriter::new(stdout.lock());
    let mut server = lsp::Server::new();
    server.run(&mut reader, &mut writer)
}
