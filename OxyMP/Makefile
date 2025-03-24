all: run

run:
	@cargo run $(ARGS)

trace:
	@RUST_BACKTRACE=1 $(MAKE) run $(ARGS)

check:
	@cd oxymp && cargo clippy -- -Dwarnings); \

update:
	@cd oxymp && cargo update --color always

test:
	@cd oxymp && cargo test --color always

expand:
	@FILE_NAME=expand-$(shell date +"%Y-%m-%d-%H-%M").rs && \
	cargo expand $(ARGS) > $$FILE_NAME && \
	bat $$FILE_NAME && \
	rm $$FILE_NAME

.PHONY: all run trace check expand
