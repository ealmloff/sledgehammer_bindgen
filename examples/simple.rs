use sledgehammer_bindgen::bindgen;

fn main() {
    #[bindgen]
    mod js {
        struct Channel;

        fn num(u1: u8, u2: u16, u3: u32) {
            "console.log($u1$, $u2$, $u3$);"
        }
    }

    let mut channel1 = Channel::default();
    channel1.num(0, 0, 0);
    channel1.flush();
}
