all: run

run:
	@cargo run $(ARGS)

trace:
	@RUST_BACKTRACE=1 $(MAKE) run $(ARGS)

check:
	@cd oxymp && cargo clippy -- -Dwarnings

update:
	@cd oxymp && cargo update --color always

test:
	@cd oxymp && cargo test --color always

expand:
	@FILE_NAME=expand-$(shell date +"%Y-%m-%d-%H-%M").rs && \
	cargo expand $(ARGS) > $$FILE_NAME && \
	bat $$FILE_NAME && \
	rm $$FILE_NAME

count:
	@cd oxymp/src && cloc .

fmt:
	@cd oxymp && cargo fmt

clean:
	@cargo clean
	@cd oxymp && cargo clean

doc:
	@cd oxymp && cargo doc --open

.PHONY: all run trace check update test expand count fmt clean doc
