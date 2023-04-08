use sledgehammer_bindgen::bindgen;
use web_sys::Node;

fn main() {
    #[bindgen]
    mod js {
        struct Channel;

        const JS: &str = r#"const nodes = [document.getElementById("main")];
        export function get_node(id){
            return nodes[id];
        }"#;

        extern "C" {
            #[wasm_bindgen]
            fn get_node(u1: u8, u2: u16, u3: u32) -> Node;
        }

        fn num(u1: u8, u2: u16, u3: u32) {
            "console.log($u1$, $u2$, $u3$);"
        }
    }

    let mut channel1 = Channel::default();
    channel1.num(0, 0, 0);
    channel1.flush();
}
