const TESTS_DIR: &'static str = "tests";

type TestPaths = Vec<&'static str>;
struct Tests {
    namespace: &'static str,
    passing: TestPaths,
    failing: TestPaths,
}
impl Tests {
    fn new(namespace: &'static str) -> Tests {
        Tests {
            namespace,
            passing: vec![],
            failing: vec![],
        }
    }

    fn pass(mut self, path: &'static str) -> Self {
        self.passing.push(path);
        self
    }

    fn fail(mut self, path: &'static str) -> Self {
        self.failing.push(path);
        self
    }
}

type CompiledTestPaths = Vec<String>;
struct CompiledTests {
    passing: CompiledTestPaths,
    failing: CompiledTestPaths,
}

impl Tests {
    fn compile_test_paths(self) -> CompiledTests {
        let passing = self
            .passing
            .into_iter()
            .map(|path| format!("{}/{}/{}.rs", TESTS_DIR, self.namespace, path))
            .collect();

        let failing = self
            .failing
            .into_iter()
            .map(|path| format!("{}/{}/{}.rs", TESTS_DIR, self.namespace, path))
            .collect();

        CompiledTests { passing, failing }
    }

    fn run(self, tests: trybuild::TestCases) {
        let compiled = self.compile_test_paths();

        compiled.passing.iter().for_each(|path| tests.pass(path));
        compiled
            .failing
            .iter()
            .for_each(|path| tests.compile_fail(path));
    }
}

#[test]
fn test_tokens() {
    let tests = trybuild::TestCases::new();

    Tests::new("tokens")
        .pass("correct")
        .fail("incorrect-derives")
        .fail("incorrect-fields")
        .fail("incorrect-attributes")
        .run(tests);
}
