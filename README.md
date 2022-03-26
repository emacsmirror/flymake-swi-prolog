# flymake-swi-prolog

Flymake backend for SWI-Prolog using [diagnostics.pl](https://git.sr.ht/~eshel/diagnostics.pl)

![diagnostics](./flymake-swi-prolog.png)

## Perquisites

Install SWI-Prolog, then install the `diagnostics` package:

```sh
$ swipl -g "pack_install(diagnostics)" -t halt
```

## Installation

Load `flymake-swi-prolog.el` into Emacs and `require` it:
```lisp
(require 'flymake-swi-prolog)
```

## Installation

Just open a file in `prolog-mode` and you should be ready to go.


## Contributing

For questions, requests, patches, please write to
[~eshel/dev@lists.sr.ht](mailto:~eshel/dev@lists.sr.ht) specifying the name of this repository in
mail subject, e.g. `[PATCH flymake-swi-prolog] fix it`.
