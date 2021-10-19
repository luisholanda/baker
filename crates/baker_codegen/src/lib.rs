use std::{
    io::{self, Read, Write},
    path::PathBuf,
    process::{Child, Command, Stdio},
};

pub use baker_codegen_pb::CodegenRequest;
use baker_codegen_pb::CodegenResponse;
use prost::Message;

/// A code generation layer.
///
/// Responsible for generating the IR that will be passed to the final code
/// generator.
pub struct Codegen {
    exec: PathBuf,
}

impl Codegen {
    /// Create a new layer from the executable name.
    pub fn new(exec: &str) -> Option<Self> {
        let full_exec = which::which(exec).ok()?;

        Some(Self { exec: full_exec })
    }

    /// Execute the layer with the given request.
    pub fn execute(&self, req: &CodegenRequest) -> io::Result<CodegenResponse> {
        let mut layer_proc = self.spawn()?;

        do_layer_flow(&mut layer_proc, req)
    }

    fn cmd(&self) -> Command {
        let mut cmd = Command::new(&self.exec);
        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit());

        cmd
    }

    fn spawn(&self) -> io::Result<Child> {
        self.cmd().spawn()
    }
}

fn do_layer_flow(proc: &mut Child, req: &CodegenRequest) -> io::Result<CodegenResponse> {
    let mut buf = req.encode_to_vec();

    proc.stdin.take().unwrap().write_all(&buf)?;

    buf.clear();

    let status = proc.wait()?;
    if !status.success() {
        io::Error::new(io::ErrorKind::Other, "Codegen failed to execute");
    }

    let n = proc.stdout.take().unwrap().read_to_end(&mut buf)?;

    CodegenResponse::decode(&mut &buf[..n])
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))
}
