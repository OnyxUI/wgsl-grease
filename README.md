# wgsl-grease

`wgsl-grease` is a small library written for Onyx that generates Rust bindings for WGSL. This library is heavily inspired by [`wgsl-bindgen`](https://github.com/Swoorup/wgsl-bindgen) and [`naga-to-tokenstream`](https://github.com/LucentFlux/naga-to-tokenstream) and a lot of code has been borrowed from `wgsl-bindgen`.

The reason we chose to make our own over using a pre-existing solution like [`include-wgsl-oil`](https://github.com/LucentFlux/include-wgsl-oil) or `wgsl-bindgen` is because they just weren't flexible in the areas we would have liked.
For example, `include-wgsl-oil` does not support bytemuck, which we use for Onyx, and although `wgsl-bindgen` supports bytemuck, it does not have the customisability to allow for dynmic buffer binding types (and also has a signifcant number of dependencies).
This crate is simply written to work for Onyx and not much more. That being said, there is much room for future improvements to make this crate more general purpose like `wgsl-bindgen`, such as:

-   Better error handling as there are lots panics
-   Custom preprocessing for things like attributes
-   Ability to have the output code generate to other types like glam

This crate is a stand-in until [WESL](https://github.com/wgsl-tooling-wg/wesl-spec) developed.
