# flymake-swi-prolog

Flymake backend for SWI-Prolog using [diagnostics.pl](https://git.sr.ht/~eshel/diagnostics.pl)

![diagnostics](./flymake-swi-prolog.png)

## Perquisites

Install SWI-Prolog, e.g. from [here](https://www.swi-prolog.org/Download.html).

## Installation

Load `flymake-swi-prolog.el` into Emacs and add the following forms to your configuration:
```lisp
(require 'flymake-swi-prolog)
(add-hook 'prolog-mode-hook #'flymake-swi-prolog-setup-backend)
```

Install the required Prolog `diagnostics` package with either:
```lisp
M-x flymake-swi-prolog-ensure-backend
```

or run from the command line:

```sh
$ swipl -g "pack_install(diagnostics)" -t halt
```

## Usage

Just open a file in `prolog-mode` and you should be ready to go.


## Contributing

For questions, requests, patches, please write to
[~eshel/dev@lists.sr.ht](mailto:~eshel/dev@lists.sr.ht) specifying the name of this repository in
mail subject, e.g. `[PATCH flymake-swi-prolog] fix it`.
