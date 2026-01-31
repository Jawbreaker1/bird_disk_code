mod cli;

use cli::args::{
    contains_help_flag, parse_command, BUILD_HELP, CHECK_HELP, FMT_HELP, HELP, RUN_HELP, TEST_HELP,
};
use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();

    if args.is_empty() || matches!(args[0].as_str(), "-h" | "--help") {
        print!("{HELP}");
        return;
    }

    if args[0] == "--version" {
        println!("birddiskc 0.1.0");
        return;
    }

    if args[0] == "fmt" && contains_help_flag(&args[1..]) {
        print!("{FMT_HELP}");
        return;
    }
    if args[0] == "check" && contains_help_flag(&args[1..]) {
        print!("{CHECK_HELP}");
        return;
    }
    if args[0] == "build" && contains_help_flag(&args[1..]) {
        print!("{BUILD_HELP}");
        return;
    }
    if args[0] == "run" && contains_help_flag(&args[1..]) {
        print!("{RUN_HELP}");
        return;
    }
    if args[0] == "test" && contains_help_flag(&args[1..]) {
        print!("{TEST_HELP}");
        return;
    }

    let command = match parse_command(&args) {
        Ok(command) => command,
        Err(message) => {
            eprintln!("error: {message}");
            eprintln!();
            eprintln!("{HELP}");
            process::exit(2);
        }
    };

    if let Err(message) = cli::execute(command) {
        if message.contains('\n') {
            eprintln!("{message}");
        } else {
            eprintln!("error: {message}");
        }
        process::exit(2);
    }
}
