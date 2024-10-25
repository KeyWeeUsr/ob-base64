# ob-base64
[![MELPA][melpa-badge]][melpa-package]
[![MELPA Stable][melpa-stable-badge]][melpa-stable-package]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

Decode and interpret base64 as images, binary (hexl) or raw (plaintext
possibly).

## How to

Install it from [Melpa](https://melpa.org/#/getting-started) or clone and
install manually, then:

1. Have `org-mode` enabled in a buffer and create a source block
2. Check `example.org` or documentation for `org-babel-execute:base64`
3. Execute the block with `C-c C-c`

## Customization

By default the rendering is set to happen internally, with an option to use an
external viewer (such as `xviewer`) and can be changed to any other binary.

Make sure to check related `defconst` parts of the file to customize or the
`ob-base64` customize group.

[melpa-badge]: https://melpa.org/packages/ob-base64-badge.svg
[melpa-package]: https://melpa.org/#/ob-base64
[melpa-stable-badge]: https://stable.melpa.org/packages/ob-base64-badge.svg
[melpa-stable-package]: https://stable.melpa.org/#/ob-base64
[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
