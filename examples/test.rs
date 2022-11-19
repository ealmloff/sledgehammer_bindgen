use sledgehammer_bindgen::bindgen;
use ux::*;

fn main() {
    #[bindgen]
    extern "C" {
        fn initialize() {
            "let nodes = [document.getElementById(\"main\")];"
        }
        fn set_attribute(id: u24, name: &str<1>, value: &str<2>) -> i32 {
            "nodes[id].setAttribute(name, value);"
        }
        fn set_attribute_ns(id: u24, name: &str<1>, namespace: &str<1>, value: &str<2>) {
            "nodes[id].setAttributeNS(namespace, name, value);"
        }
        fn create_element(id: u24, name: &str<1>) {
            "nodes[id]=document.createElement(name);"
        }
        fn create_element_ns(id: u24, name: &str<1>, namespace: &str<1>) {
            "nodes[id]=document.createElementNS(namespace, name);"
        }
        fn create_text_node(id: u24, text: &str<2>) {
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
        fn set_text_content(id: u24, text: &str<2>) {
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
    }
    let mut channel = Channel::default();
    channel.create_element(1u8.into(), "div");
    channel.create_element_ns(2u8.into(), "svg", "http://www.w3.org/2000/svg");
    channel.append_child(0u8.into(), 1u8.into());
    channel.append_child(1u8.into(), 2u8.into());
    channel.flush();
}
