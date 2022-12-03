<div align="center">
  <h1>sledgehammer bindgen</h1>
</div>
<div align="center">
  <!-- Crates version -->
  <a href="https://crates.io/crates/sledgehammer_bindgen">
    <img src="https://img.shields.io/crates/v/sledgehammer_bindgen.svg?style=flat-square"
    alt="Crates.io version" />
  </a>
  <!-- Downloads -->
  <a href="https://crates.io/crates/sledgehammer_bindgen">
    <img src="https://img.shields.io/crates/d/sledgehammer_bindgen.svg?style=flat-square"
      alt="Download" />
  </a>
  <!-- docs -->
  <a href="https://docs.rs/sledgehammer_bindgen">
    <img src="https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square"
      alt="docs.rs docs" />
  </a>
</div>

# What is Sledgehammer Bindgen?
Sledgehammer bindgen provides faster rust batched bindings for js code.

# How does this compare to wasm-bindgen:
- wasm-bindgen is a lot more general it allows returning values and passing around a lot more different types of values. For most users wasm-bindgen is a beter choice. Sledgehammer is specifically designed for web frameworks that want low-level, fast access to the dom.

- You can use sledgehammer bindgen with wasm-bindgen. See the docs and examples for more information.

# Why is it fast?

## String decoding

- Decoding strings are expensive to decode, but the cost doesn't change much with the size of the string. Wasm-bindgen calls TextDecoder.decode for every string. Sledgehammer only calls TextEncoder.decode once per batch.

- If the string is small, it is faster to decode the string in javascript to avoid the constant overhead of TextDecoder.decode

- See this benchmark: https://jsbench.me/4vl97c05lb/5

## String Caching

- You can cache strings in javascript to avoid decoding the same string multiple times.
- If the string is static sledgehammer will hash by pointer instead of by value.

## Byte encoded operations

- In sledgehammer every operation is encoded as a sequence of bytes packed into an array. Every operation takes 1 byte plus whatever data is required for it.

- Each operation is encoded in a batch of four as a u32. Getting a number from an array buffer has a high constant cost, but getting a u32 instead of a u8 is not more expensive. Sledgehammer bindgen reads the u32 and then splits it into the 4 individual bytes. It will shuffle and pack the bytes into as few buckets as possible and try to inline reads into js.

- See this benchmark: https://jsbench.me/csl9lfauwi/2
