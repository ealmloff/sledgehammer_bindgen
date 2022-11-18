use sledgehammer_bindgen::bindgen;

fn main() {
    #[bindgen]
    extern "C" {
        fn foo(i: i32, s: &str) {
            "
            document.createElement(s);
            "
        }
        fn bar() {
            "
            console.log('hello world')
            "
        }
    }
}
