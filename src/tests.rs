use std::thread;

type EmptyFn = fn() -> ();
type BeforeEachFn = fn() -> Vec<String>;

pub struct TestFixture<'a> {
    setup: Option<&'a EmptyFn>,
    teardown: Option<&'a EmptyFn>,
    before_each: Option<BeforeEachFn>,
    after_each: Option<&'a EmptyFn>
}

impl<'a> TestFixture<'a> {
    fn new() -> TestFixture<'a> {
        TestFixture {
            setup: None,
            teardown: None,
            before_each: None,
            after_each: None,
        }
    }
}

pub trait TestFn {
    fn run(&self, Vec<String>) -> ();
}

impl<F> TestFn for F
    where F: Fn() -> () {
    fn run(&self, args: Vec<String>) {
        (*self)()
    }
}

pub fn test_runner(tests: Vec<&TestFn>, fixture: TestFixture) {
    if let Some(setup) = fixture.setup {
        setup()
    }

    let before_each = match fixture.before_each {
        Some(f) => f,
        None => (|| { Vec::new() }) as fn() -> Vec<String>,
    };

    tests.iter().map(|&t| {
        let before_each_copy = before_each.clone();
        let handle = thread::spawn(move || {
            t.run(before_each_copy());
        });

        if let Err(e) = handle.join() {
            panic!(e);
        }

        if let Some(after_each) = fixture.after_each {
            after_each();
        }
    });

    if let Some(teardown) = fixture.teardown {
        teardown();
    }
}
