# ob-base64

Decode and interpret base64 as images, binary (hexl) or raw (plaintext
possibly).

## How to

1. Have `org-mode` enabled in a buffer and create a source block
2. Check `example.org` or documentation for `org-babel-execute:base64`
3. Execute the block with `C-c C-c`

## Customization

By default the rendering is set to happen internally, with an option to use an
external viewer (such as `xviewer`) and can be changed to any other binary.

Make sure to check related `defconst` parts of the file to customize or the
`ob-base64` customize group.
