#![cfg(feature = "cli")]

use clap::{ArgAction, Command, arg, crate_name, crate_version, error::ErrorKind};

fn main() {
    let mut cmd = Command::new(crate_name!())
        .max_term_width(80)
        .version(crate_version!())
        .about("Generates recursive descent parsers for Rust using LL(1) grammars.")
        .arg(arg!(-c --check "Only check the file for errors"))
        .arg(arg!(-f --format "Format the llw file"))
        .arg(arg!(--preserve-comments "Preserve comments when formatting"))
        .arg(arg!(-g --graph "Output a graphviz file for the grammar"))
        .arg(arg!(-s --short "Use short diagnostics"))
        .arg(arg!(-v --verbose "Sets the level of verbosity").action(ArgAction::Count))
        .arg(
            arg!(-o --output <FILE> "Sets the output directory")
                .default_value(".")
                .required(false),
        )
        .arg(arg!(<INPUT> "Sets the input file to use"))
        .after_help("Report bugs to <https://github.com/0x2a-42/lelwel>.");

    let matches = cmd.get_matches_mut();

    let input = matches.get_one::<String>("INPUT").unwrap();
    let output = matches.get_one::<String>("output").unwrap();
    if matches.get_flag("format") {
        let preserve_comments = matches.get_flag("preserve-comments");
        match lelwel::format_llw_file(input, output, preserve_comments) {
            Ok(success) => std::process::exit(if success { 0 } else { 1 }),
            Err(e) => cmd.error(ErrorKind::InvalidValue, format!("{e}")).exit(),
        }
    } else {
        match lelwel::compile(
            input,
            output,
            matches.get_flag("check"),
            matches.get_count("verbose"),
            matches.get_flag("graph"),
            matches.get_flag("short"),
        ) {
            Ok(success) => std::process::exit(if success { 0 } else { 1 }),
            Err(e) => cmd.error(ErrorKind::InvalidValue, format!("{e}")).exit(),
        }
    }
}
