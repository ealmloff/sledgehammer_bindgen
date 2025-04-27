#![allow(unused)]
use std::{
    cell::Cell,
    time::{Duration, Instant},
};

use sledgehammer_bindgen::bindgen;
use wasm_bindgen::prelude::wasm_bindgen;
use web_sys::{console, Node};
use wry::{http::Response, WebViewBuilder};

#[wasm_bindgen(module = "examples/typed_elements.js")]
extern "C" {
    #[wasm_bindgen]
    pub type TypedElements;
}

#[cfg(any(target_os = "android", target_os = "windows"))]
const INDEX_PATH: &str = "http://dioxus.index.html";

#[cfg(not(any(target_os = "android", target_os = "windows")))]
const INDEX_PATH: &str = "dioxus://index.html";

const TYPED_JS: &str = include_str!("./typed_elements.js");

#[bindgen]
mod js {
    #[extends(TypedElements)]
    struct Channel;

    fn create_element(id: u16, element_id: u8) {
        "this.nodes[$id$]=document.createElement(this.els[$element_id$]);"
    }

    fn set_attribute(id: u16, attribute_id: u8, val: impl Writable<u8>) {
        "this.nodes[$id$].setAttribute(this.attrs[$attribute_id$],$val$);"
    }

    fn remove_attribute(id: u16, attribute_id: u8) {
        "this.nodes[$id$].removeAttribute(this.attrs[$attribute_id$]);"
    }

    fn append_child(id: u16, id2: u16) {
        "this.nodes[$id$].appendChild(this.nodes[$id2$]);"
    }

    fn insert_before(parent: u16, id: u16, id2: u16) {
        "this.nodes[$parent$].insertBefore(this.nodes[$id$],this.nodes[$id2$]);"
    }

    fn set_text(id: u16, text: impl Writable<u8>) {
        "this.nodes[$id$].textContent=$text$;"
    }

    fn remove(id: u16) {
        "this.nodes[$id$].remove();"
    }

    fn replace(id: u16, id2: u16) {
        "this.nodes[$id$].replaceWith(this.nodes[$id2$]);"
    }

    fn clone(id: u16, id2: u16) {
        "this.nodes[$id2$]=this.nodes[$id$].cloneNode(true);"
    }

    fn first_child(id: u16) {
        "this.node[id]=this.node[id].firstChild;"
    }

    fn next_sibling(id: u16) {
        "this.node[id]=this.node[id].nextSibling;"
    }
}

#[allow(non_camel_case_types)]
#[repr(u8)]
enum Element {
    a,
    abbr,
    acronym,
    address,
    applet,
    area,
    article,
    aside,
    audio,
    b,
    base,
    bdi,
    bdo,
    bgsound,
    big,
    blink,
    blockquote,
    body,
    br,
    button,
    canvas,
    caption,
    center,
    cite,
    code,
    col,
    colgroup,
    content,
    data,
    datalist,
    dd,
    del,
    details,
    dfn,
    dialog,
    dir,
    div,
    dl,
    dt,
    em,
    embed,
    fieldset,
    figcaption,
    figure,
    font,
    footer,
    form,
    frame,
    frameset,
    h1,
    head,
    header,
    hgroup,
    hr,
    html,
    i,
    iframe,
    image,
    img,
    input,
    ins,
    kbd,
    keygen,
    label,
    legend,
    li,
    link,
    main,
    map,
    mark,
    marquee,
    menu,
    menuitem,
    meta,
    meter,
    nav,
    nobr,
    noembed,
    noframes,
    noscript,
    object,
    ol,
    optgroup,
    option,
    output,
    p,
    param,
    picture,
    plaintext,
    portal,
    pre,
    progress,
    q,
    rb,
    rp,
    rt,
    rtc,
    ruby,
    s,
    samp,
    script,
    section,
    select,
    shadow,
    slot,
    small,
    source,
    spacer,
    span,
    strike,
    strong,
    style,
    sub,
    summary,
    sup,
    table,
    tbody,
    td,
    template,
    textarea,
    tfoot,
    th,
    thead,
    time,
    title,
    tr,
    track,
    tt,
    u,
    ul,
    var,
    video,
    wbr,
    xmp,
}

#[allow(non_camel_case_types)]
#[repr(u8)]
enum Attribute {
    ccept_charset,
    accept,
    accesskey,
    action,
    align,
    allow,
    alt,
    aria_atomic,
    aria_busy,
    aria_controls,
    aria_current,
    aria_describedby,
    aria_description,
    aria_details,
    aria_disabled,
    aria_dropeffect,
    aria_errormessage,
    aria_flowto,
    aria_grabbed,
    aria_haspopup,
    aria_hidden,
    aria_invalid,
    aria_keyshortcuts,
    aria_label,
    aria_labelledby,
    aria_live,
    aria_owns,
    aria_relevant,
    aria_roledescription,
    r#async,
    autocapitalize,
    autocomplete,
    autofocus,
    autoplay,
    background,
    bgcolor,
    border,
    buffered,
    capture,
    challenge,
    charset,
    checked,
    cite,
    class,
    code,
    codebase,
    color,
    cols,
    colspan,
    content,
    contenteditable,
    contextmenu,
    controls,
    coords,
    crossorigin,
    csp,
    data,
    datetime,
    decoding,
    default,
    defer,
    dir,
    dirname,
    disabled,
    download,
    draggable,
    enctype,
    enterkeyhint,
    r#for,
    form,
    formaction,
    formenctype,
    formmethod,
    formnovalidate,
    formtarget,
    headers,
    height,
    hidden,
    high,
    href,
    hreflang,
    http_equiv,
    icon,
    id,
    importance,
    inputmode,
    integrity,
    intrinsicsize,
    ismap,
    itemprop,
    keytype,
    kind,
    label,
    lang,
    language,
    list,
    loading,
    r#loop,
    low,
    manifest,
    max,
    maxlength,
    media,
    method,
    min,
    minlength,
    multiple,
    muted,
    name,
    novalidate,
    open,
    optimum,
    pattern,
    ping,
    placeholder,
    poster,
    preload,
    radiogroup,
    readonly,
    referrerpolicy,
    rel,
    required,
    reversed,
    role,
    rows,
    rowspan,
    sandbox,
    scope,
    scoped,
    selected,
    shape,
    size,
    sizes,
    slot,
    span,
    spellcheck,
    src,
    srcdoc,
    srclang,
    srcset,
    start,
    step,
    style,
    summary,
    tabindex,
    target,
    title,
    translate,
    r#type,
    usemap,
    value,
    width,
    wrap,
}

fn main() -> wry::Result<()> {
    use tao::{
        event::{Event, WindowEvent},
        event_loop::{ControlFlow, EventLoop},
        window::WindowBuilder,
    };

    let event_loop = EventLoop::new();
    let window = WindowBuilder::new().build(&event_loop).unwrap();

    let first_request = Cell::new(true);
    let webview = WebViewBuilder::new()
        .with_url(INDEX_PATH)
        .with_asynchronous_custom_protocol(
            "dioxus".into(),
            move |_webview_id, request, responder| {
                if first_request.get() {
                    first_request.set(false);
                    let html = format!(
                        r#"
                    <head>
                    </head>
                    <body>
                        <div id="main"></div>
                        <script>
                            {}
                            {}
                            let channel = new RawChannel();
                            function wait_for_request() {{
                                fetch(new Request("{INDEX_PATH}"))
                                    .then(response => {{
                                        response.arrayBuffer()
                                            .then(bytes => {{
                                                channel.run_from_bytes(bytes);
                                                wait_for_request();
                                            }});
                                    }})
                            }}

                            // Wait for the page to load
                            window.onload = function() {{
                                wait_for_request();
                            }}
                        </script>
                    </body>
                    "#,
                        TYPED_JS,
                        GENERATED_JS.replace("export", "")
                    );
                    responder.respond(
                        Response::builder()
                            .header("Access-Control-Allow-Origin", "*")
                            .header("Content-Type", "text/html")
                            .body(html.as_bytes().to_vec())
                            .unwrap(),
                    );
                    return;
                }

                let mut channel1 = Channel::default();
                let main = 0;
                let node1 = 1;
                let node2 = 2;
                for _ in 0..rand::random::<u8>() {
                    channel1.create_element(node1, Element::div as u8);
                    channel1.create_element(node2, Element::span as u8);
                    channel1.append_child(node1, node2);
                    let rand1 = rand::random::<u8>();
                    let rand2 = rand::random::<u8>();
                    channel1.set_text(
                        node2,
                        format_args!("{}+{}={}", rand1, rand2, rand1 as usize + rand2 as usize),
                    );
                    channel1.append_child(main, node1);
                }

                let data = channel1.export_memory();
                let data: Vec<_> = data.collect();
                println!("{:?}", data);

                channel1.reset();

                std::thread::spawn(move || {
                    std::thread::sleep(Duration::from_millis(100));
                    responder.respond(Response::new(data));
                });
            },
        )
        .build(&window)
        .unwrap();

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        if let Event::WindowEvent {
            event: WindowEvent::CloseRequested,
            ..
        } = event
        {
            *control_flow = ControlFlow::Exit
        }
    });
}
