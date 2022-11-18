use sledgehammer_bindgen::bindgen;
use ux::*;

fn main() {
    #[bindgen]
    extern "C" {
        fn initialize(nodes: u8) {
            "nodes = [];"
        }
        fn set_attribute(id: u20, name: &str<6>, value: &str<16>) -> i32 {
            "nodes[id].setAttribute(name, value);"
        }
        fn set_attribute_ns(id: u20, name: &str<6>, namespace: &str<6>, value: &str<16>) {
            "nodes[id].setAttributeNS(namespace, name, value);"
        }
        fn create_element(id: u20, name: &str<8>) {
            "nodes[id]=document.createElement(name);"
        }
        fn create_element_ns(name: &str<8>, namespace: &str<8>) {
            "nodes[id]=document.createElementNS(namespace, name);"
        }
        fn create_text_node(id: u20, text: &str<12>) {
            "nodes[id]=document.createTextNode(text);"
        }
        fn append_child(parent: u20, child: u20) {
            "nodes[parent].appendChild(nodes[child]);"
        }
        fn remove_child(parent: u20, child: u20) {
            "nodes[parent].removeChild(nodes[child]);"
        }
        fn insert_before(parent: u20, child: u20) {
            "nodes[parent].before(nodes[child]);"
        }
        fn replace_child(parent: u20, child: u20) {
            "nodes[parent].after(nodes[child]);"
        }
        fn set_text_content(id: u20, text: &str<12>) {
            "nodes[id].textContent = text;"
        }
        fn remove(id: u20) {
            "nodes[id].remove();"
        }
        fn replace(id: u20, new_id: u20) {
            "nodes[id].replaceWith(nodes[new_id]);"
        }
        fn clone(id: u20, into: u20) {
            "nodes[into]= nodes[id].cloneNode();"
        }
    }
}
