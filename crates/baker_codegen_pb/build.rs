const PROTOS: &[&str] = &["../../protos/baker/codegen/v1/codegen.proto"];

fn main() -> std::io::Result<()> {
    let mut prost_build = prost_build::Config::new();

    prost_build.protoc_arg("--experimental_allow_proto3_optional");
    prost_build.compile_protos(PROTOS, &["../../protos"])?;

    for proto in PROTOS {
        println!("cargo:rerun-if-changed={}", proto);
    }

    Ok(())
}
