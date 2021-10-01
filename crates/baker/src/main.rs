#[macro_use]
extern crate clap;

use baker_pkg_loader::PkgLoader;
use clap::Arg;
use std::{collections::HashSet, path::PathBuf};

fn main() -> std::io::Result<()> {
    let app = app_from_crate!()
        .about("A code generator manager tht reads protobuf files and layer programs to generate code.")
        .args(&[
            Arg::with_name("inputs")
            .help("Main input files.")
            .required(true)
            .index(1),
        ]);

    let matches = app.get_matches();

    let inputs = matches.values_of_lossy("inputs").unwrap_or_default();

    if inputs.is_empty() {
        eprintln!("No input file received!");
        std::process::exit(1);
    }

    let mut loader = PkgLoader::new();
    let inputs: HashSet<_> = inputs.into_iter().map(PathBuf::from).collect();

    for inp in &inputs {
        loader.add_entry_point(&inp);
    }

    let _ = dbg!(loader.load());

    Ok(())
}
