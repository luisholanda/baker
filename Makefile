LAYERS := $(shell find layers/ -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
CODEGENS := $(shell find codegens/ -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
BAKER := target/debug/baker

LAYER_BINS := rust_types \
							rust_serde \
							rust_diesel
LAYER_BINS := $(addprefix target/debug/, $(LAYER_BINS))
CODEGEN_BINS := $(addprefix target/debug/, $(CODEGENS))

PROTO_SRCS := $(shell find protos -type f)
BAKER_SRCS := $(shell find crates -type f)
LAYER_SRCS := $(shell find layers -type f)
CODEGEN_SRCS := $(shell find codegens -type f)

EXAMPLE_PROTO := examples/blog/api/v1/api.proto
RUST_CODEGEN := target/debug/rust_codegen

example: $(BAKER) $(LAYER_BINS) $(CODEGEN_BINS)
	$(BAKER) $(EXAMPLE_PROTO) --layers $(LAYER_BINS) --codegen=$(RUST_CODEGEN)

$(BAKER): $(BAKER_SRCS) $(PROTO_SRCS)
	cargo build --bin baker

$(LAYER_BINS): $(LAYER_SRCS) $(PROTO_SRCS)
	cargo build $(addprefix -p , $(LAYERS))

$(CODEGEN_BINS): $(CODEGEN_SRCS) $(PROTO_SRCS)
	cargo build $(addprefix -p , $(CODEGENS))
