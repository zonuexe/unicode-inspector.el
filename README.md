# 🔎 unicode-inspector.el

`unicode-inspector.el` is an Emacs package that provides detailed information about Unicode characters. It allows you to interactively inspect strings and view their code points, names, and other properties for each character.

## Requirements

This package requires the following:

* **Emacs 29.1** or later.
* [vui.el][]: A declarative, component-based UI framework for Emacs.

> [!NOTE]
> Since vui.el is hosted on MELPA, please ensure that you have [MELPA](https://melpa.org/#/getting-started) added to your package-archives in your Emacs configuration to resolve this dependency.

[vui.el]: https://github.com/d12frosted/vui.el

## Installation

You can install `unicode-inspector` using `package-vc-install` (available in Emacs 29.1 and later). Run the following code in your Emacs:

```el
(package-vc-install
 '(unicode-inspector :url "https://github.com/zonuexe/unicode-inspector.el.git"
                     :main-file "unicode-inspector.el"))
```

## Customization

You can customize the behavior of `unicode-inspector` by adding the following configuration to your [Emacs Initialization File][]:

```el
(with-eval-after-load 'unicode-inspector
  (setopt unicode-inspector-unique-input t))
```

[Emacs Initialization File]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

### `unicode-inspector-unique-input`

If non-nil, the inspector filters out duplicate characters from the input and displays each unique character only once.

## Copyright

This package is licensed under [GNU General Public License, version 3](https://www.gnu.org/licenses/gpl-3.0).

    unicode-inspector.el  Copyright (C) 2025  USAMI Kenta

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

## Unicode Data Notice

[`unicode-inspector-blocks.el`](unicode-inspector-blocks.el) is derived from the Unicode data and is subject to the [Unicode Terms of Use](https://www.unicode.org/terms_of_use.html) and the [License](https://www.unicode.org/license.txt).
