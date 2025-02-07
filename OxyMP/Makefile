CRATES = $(shell find . -maxdepth 3 -name 'Cargo.toml' -exec dirname {} \; | sort)

all: run

run:
	cargo run $(ARGS)

trace:
	RUST_BACKTRACE=1 $(MAKE) run $(ARGS)

check:
	@for crate in $(CRATES); do \
		(echo "Checking $$crate" && cd $$crate && cargo clippy -- -Dwarnings); \
	done

update:
	@for crate in $(CRATES); do \
		(echo "Updating $$crate" && cd $$crate && cargo update --color always); \
	done

test:
	@for crate in $(CRATES); do \
		if [ -d "$$crate/tests" ]; then \
			(echo "Testing $$crate" && cd "$$crate" && cargo test); \
		fi \
	done

expand:
	@FILE_NAME=expand-$(shell date +"%Y-%m-%d-%H-%M").rs && \
	cargo expand $(ARGS) > $$FILE_NAME && \
	bat $$FILE_NAME && \
	rm $$FILE_NAME

.PHONY: all run trace check expand
