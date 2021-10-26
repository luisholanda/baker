#[macro_use]
extern crate clap;

use std::{collections::HashSet, io, path::PathBuf};

use baker_codegen::{Codegen, CodegenRequest};
use baker_ir_merger::IrMerger;
use baker_layer::{Layer, LayerRequest};
use baker_pkg_loader::PkgLoader;
use clap::Arg;

fn main() -> std::io::Result<()> {
    let app = app_from_crate!()
        .about("A code generator manager tht reads protobuf files and layer programs to generate code.")
        .args(&[
            Arg::with_name("layers")
            .long("layers")
            .help("Layers to apply, in order")
            .required(true)
            .multiple(true)
            .min_values(1),
            Arg::with_name("codegen")
            .long("codegen")
            .help("Codegen executable to use to generate the desired concrete language")
            .required(true)
            .min_values(1)
            .max_values(1),
            Arg::with_name("inputs")
            .help("Main input files.")
            .required(true)
            .min_values(1)
            .index(1),
        ]);

    let matches = app.get_matches();

    let inputs = matches.values_of_lossy("inputs").unwrap_or_default();
    let layers = matches.values_of_lossy("layers").unwrap_or_default();
    let codegen = matches.value_of_lossy("codegen").unwrap_or_default();

    let mut loader = PkgLoader::new();
    let inputs: HashSet<_> = inputs.into_iter().map(PathBuf::from).collect();

    for inp in &inputs {
        loader.add_entry_point(&inp);
    }

    loader.load().expect("failed to load package graph");
    loader.check_undefined_names();
    let graph = dbg!(loader.graph());

    let request = LayerRequest {
        packages: Some(graph.into_pb()),
    };

    let mut merger = IrMerger::default();
    for layer in &layers {
        if let Some(layer) = Layer::new(layer) {
            let resp = layer.execute(&request)?;
            for file in resp.ir_files {
                merger.add_ir_file_def(file);
            }
        } else {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Could not find layer '{}'", layer),
            ));
        }
    }

    let files = merger.into_files();
    {
        let req = CodegenRequest {
            packages: request.packages,
            ir_files: files,
            output_folder: std::env::current_dir()?.display().to_string(),
        };

        let codegen = Codegen::new(&codegen).unwrap();
        codegen.execute(&req)?
    };

    Ok(())
}
