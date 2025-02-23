CRATES = $(shell find . -maxdepth 3 -name 'Cargo.toml' -exec dirname {} \; | sort)
CRATES_WITH_TESTS = oxymp oxymp/oxymp-rd

all: run

run:
	@cargo run $(ARGS)

trace:
	@RUST_BACKTRACE=1 $(MAKE) run $(ARGS)

check:
	@for crate in $(CRATES); do \
		(echo "Checking $$crate" && cd $$crate && cargo clippy -- -Dwarnings); \
	done

update:
	@for crate in $(CRATES); do \
		(echo "Updating $$crate" && cd $$crate && cargo update --color always); \
	done

test:
	@for crate in $(CRATES_WITH_TESTS); do \
		(echo "Testing $$crate" && cd $$crate && cargo test --color always); \
	done

expand:
	@FILE_NAME=expand-$(shell date +"%Y-%m-%d-%H-%M").rs && \
	cargo expand $(ARGS) > $$FILE_NAME && \
	bat $$FILE_NAME && \
	rm $$FILE_NAME

.PHONY: all run trace check expand
