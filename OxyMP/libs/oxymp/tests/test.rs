mod trybuild_tests {
    const TESTS_DIR: &'static str = "tests/trybuild";

    type TestPaths = Vec<&'static str>;
    pub struct Tests {
        namespace: Option<&'static str>,
        passing: TestPaths,
        failing: TestPaths,
    }
    impl Tests {
        pub fn new() -> Tests {
            Tests {
                namespace: None,
                passing: vec![],
                failing: vec![],
            }
        }

        pub fn namespace(mut self, namespace: &'static str) -> Self {
            self.namespace = Some(namespace);
            self
        }

        pub fn pass(mut self, path: &'static str) -> Self {
            self.passing.push(path);
            self
        }

        pub fn fail(mut self, path: &'static str) -> Self {
            self.failing.push(path);
            self
        }
    }
    type CompiledTestPaths = Vec<String>;
    struct CompiledTests {
        pub passing: CompiledTestPaths,
        pub failing: CompiledTestPaths,
    }

    impl Tests {
        fn compile_test_paths(self) -> CompiledTests {
            let dir = match self.namespace {
                Some(namespace) => format!("{}/{}", TESTS_DIR, namespace),
                None => format!("{}", TESTS_DIR),
            };

            let passing = self
                .passing
                .into_iter()
                .map(|path| format!("{}/{}.rs", dir, path))
                .collect();

            let failing = self
                .failing
                .into_iter()
                .map(|path| format!("{}/{}.rs", dir, path))
                .collect();

            CompiledTests { passing, failing }
        }

        pub fn run(self) {
            let t = trybuild::TestCases::new();

            let compiled = self.compile_test_paths();

            compiled.passing.iter().for_each(|path| t.pass(path));
            compiled
                .failing
                .iter()
                .for_each(|path| t.compile_fail(path));
        }
    }
}

#[test]
fn test_tokens() {
    trybuild_tests::Tests::new()
        .namespace("tokens")
        .pass("correct")
        .fail("incorrect-derives")
        .fail("incorrect-fields")
        .fail("incorrect-attributes")
        .pass("public-tokens")
        .fail("private-tokens")
        .run();
}

#[test]
fn test_rewrite_libs() {
    trybuild_tests::Tests::new().pass("rewrite-libs").run();
}
