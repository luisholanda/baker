LAYERS := $(shell find layers/ -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
CODEGENS := $(shell find CODEGENS/ -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
BAKER := target/debug/baker

LAYER_BINS := $(addprefix target/debug/, $(LAYERS))
CODEGEN_BINS := $(addprefix target/debug/, $(CODEGEN))

PROTO_SRCS := $(shell find protos -type f)
BAKER_SRCS := $(shell find crates -type f)
LAYER_SRCS := $(shell find layers -type f)
CODEGEN_SRCS := $(shell find codegens -type f)

EXAMPLE_PROTO := examples/blog/api/v1/api.proto

example: $(BAKER) $(LAYER_BINS) $(CODEGEN_BINS)
	$(BAKER) $(EXAMPLE_PROTO) --layers $(LAYER_BINS)

$(BAKER): $(BAKER_SRCS) $(PROTO_SRCS)
	cargo build --bin baker

$(LAYER_BINS): $(LAYER_SRCS)
	cargo build --manifest-path layers/*/Cargo.toml

$(CODEGEN_BINS): $(CODEGEN_SRCS)
	cargo build --manifest-path codegen/*/Cargo.toml
