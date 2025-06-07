// const TESTS_DIR: &'static str = "tests/trybuild";
//
// type TestPaths = Vec<&'static str>;
// struct Tests {
//     namespace: Option<&'static str>,
//     passing: TestPaths,
//     failing: TestPaths,
// }
// impl Tests {
//     fn new() -> Tests {
//         Tests {
//             namespace: None,
//             passing: vec![],
//             failing: vec![],
//         }
//     }
//
//     fn namespace(mut self, namespace: &'static str) -> Self {
//         self.namespace = Some(namespace);
//         self
//     }
//
//     fn pass(mut self, path: &'static str) -> Self {
//         self.passing.push(path);
//         self
//     }
//
//     fn fail(mut self, path: &'static str) -> Self {
//         self.failing.push(path);
//         self
//     }
// }
// type CompiledTestPaths = Vec<String>;
// struct CompiledTests {
//     passing: CompiledTestPaths,
//     failing: CompiledTestPaths,
// }
//
// impl Tests {
//     fn compile_test_paths(self) -> CompiledTests {
//         let dir = match self.namespace {
//             Some(namespace) => format!("{}/{}", TESTS_DIR, namespace),
//             None => format!("{}", TESTS_DIR),
//         };
//
//         let passing = self
//             .passing
//             .into_iter()
//             .map(|path| format!("{}/{}.rs", dir, path))
//             .collect();
//
//         let failing = self
//             .failing
//             .into_iter()
//             .map(|path| format!("{}/{}.rs", dir, path))
//             .collect();
//
//         CompiledTests { passing, failing }
//     }
//
//     fn run(self) {
//         let t = ::trybuild::TestCases::new();
//
//         let compiled = self.compile_test_paths();
//
//         compiled.passing.iter().for_each(|path| t.pass(path));
//         compiled
//             .failing
//             .iter()
//             .for_each(|path| t.compile_fail(path));
//     }
// }
//
// #[test]
// fn test_tokens() {
//     Tests::new()
//         .namespace("tokens")
//         .pass("correct")
//         .fail("incorrect-fields")
//         .fail("incorrect-marker-usage")
//         .fail("incorrect-variants")
//         .pass("public-tokens")
//         .fail("private-tokens")
//         .run();
// }
//
// #[test]
// fn test_independent() {
//     Tests::new()
//         .namespace("independent")
//         .fail("unknown-marker")
//         .run();
// }
//
// #[test]
// fn test_integration() {
//     Tests::new()
//         .namespace("integration")
//         .pass("no-prelude")
//         .run();
// }
