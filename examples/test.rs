use std::num::NonZeroUsize;

use sledgehammer_bindgen::bindgen;
use ux::*;

fn main() {
    #[bindgen]
    extern "C" {
        fn initialize() {
            "let nodes = [document.getElementById(\"main\")];"
        }
        fn set_attribute(id: u24, name: &str<u8, name_cache>, value: &str<u16>) -> i32 {
            "nodes[id].setAttribute(name, value);"
        }
        fn set_attribute_ns(
            id: u24,
            name: &str<u8, name_cache>,
            namespace: &str<u8, ns_cache>,
            value: &str<u16>,
        ) {
            "nodes[id].setAttributeNS(namespace, name, value);"
        }
        fn create_element(id: u24, name: &str<u8, name_cache>) {
            "nodes[id]=document.createElement(name);"
        }
        fn create_element_ns(id: u24, name: &str<u8, name_cache>, namespace: &str<u8, ns_cache>) {
            "nodes[id]=document.createElementNS(namespace, name);"
        }
        fn create_text_node(id: u24, text: &str<u16>) {
            "nodes[id]=document.createTextNode(text);"
        }
        fn append_child(parent: u24, child: u24) {
            "nodes[parent].appendChild(nodes[child]);"
        }
        fn remove_child(parent: u24, child: u24) {
            "nodes[parent].removeChild(nodes[child]);"
        }
        fn insert_before(parent: u24, child: u24) {
            "nodes[parent].before(nodes[child]);"
        }
        fn replace_child(parent: u24, child: u24) {
            "nodes[parent].after(nodes[child]);"
        }
        fn set_text_content(id: u24, text: &str<u16>) {
            "nodes[id].textContent = text;"
        }
        fn remove(id: u24) {
            "nodes[id].remove();"
        }
        fn replace(id: u24, new_id: u24) {
            "nodes[id].replaceWith(nodes[new_id]);"
        }
        fn clone(id: u24, into: u24) {
            "nodes[into]= nodes[id].cloneNode();"
        }
        fn test(path: &[u8<u8>], id: u24) {
            "console.log(path, id);"
        }
    }
    let mut channel1 = Channel::default();
    channel1.create_element(1u8.into(), "div");
    channel1.create_element_ns(2u8.into(), "svg", "http://www.w3.org/2000/svg");
    let mut channel2 = Channel::default();
    channel2.append(channel1);
    channel2.append_child(0u8.into(), 1u8.into());
    channel2.append_child(1u8.into(), 2u8.into());
    channel2.test(&[1, 2, 3], 4u8.into());
    channel2.flush();
}
