use std::sync::{Arc, Mutex, StaticMutex, MUTEX_INIT};
use std::thread;

static TEST_LOCK: StaticMutex = MUTEX_INIT;

#[export_macro]
macro_rules! test {
    ($name: expr, $func: ident) => {
        (String::from($name), box Self::$func)
    }
}

pub type TestList<T> = Vec<(String, Box<Fn(&mut T)+Send>)>;

pub trait TestFixture {
    fn setup(&mut self) -> () {}
    fn teardown(&mut self) -> () {}
    fn before_each(&mut self) -> () {}
    fn after_each(&mut self) -> () {}
    fn tests(&self) -> TestList<Self> {
        Vec::new()
    }
}

pub fn test_fixture_runner<T: TestFixture + Send + 'static>(fixture: T) {
    let _handle = TEST_LOCK.lock().unwrap();
    let fixture_arc = Arc::new(Mutex::new(fixture));
    test_fixture_inner(fixture_arc);
}

fn test_fixture_inner<T: TestFixture + Send + 'static>(fixture: Arc<Mutex<T>>) {
    let mut has_failure = false;

    fixture.lock().unwrap().setup();

    let tests = fixture.lock().unwrap().tests();
    for (t_name, t) in tests.into_iter() {

        let fixture = fixture.clone();

        let handle = thread::Builder::new()
            .name(t_name.clone())
            .spawn(move || {
                let mut fixture = fixture.lock().unwrap();

                fixture.before_each();
                t(&mut *fixture);
                fixture.after_each();
            })
            .unwrap();

        let result = handle.join();

        if let Err(_) = result {
            println!("{} ... FAILED!", t_name);
            has_failure = true;
        } else {
            println!("{} ... ok", t_name);
        }
    }

    fixture.lock().unwrap().teardown();
    assert!(!has_failure, "Some tests failed.");
}
