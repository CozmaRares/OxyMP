all: run

run:
	cargo run $(ARGS)

trace:
	RUST_BACKTRACE=1 $(MAKE) run $(ARGS)

check:
	for crate in $(shell find . -name 'Cargo.toml' -exec dirname {} \;); do \
		(echo "Checking $$crate" && cd $$crate && cargo clippy -- -Dwarnings); \
	done

expand:
	@FILE_NAME=expand-$(shell date +"%Y-%m-%d-%H-%M").rs && \
	cargo expand $(ARGS) > $$FILE_NAME && \
	bat $$FILE_NAME && \
	rm $$FILE_NAME

.PHONY: all run trace check expand
