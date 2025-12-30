# Changelog

All notable changes of the `unicode-inspector.el` are documented in this file using the [Keep a Changelog](https://keepachangelog.com/) principles.

<!-- ## Unreleased -->

## [2025-12-30] 0.0.4

### Added

* Add `unicode-inspector-block-table` and `unicode-inspector-block-list` commands with completion.
* Add configurable `unicode-inspector-display-replacements` and `unicode-inspector-control-replacements`.

### Changed

* Renamed the block codepoint list command to `unicode-inspector-block-list`.
* Re-opening existing inspector/list buffers now reuses or recreates buffers to avoid widget errors.
* Character display now uses replacement strings (some via `display` properties) and applies `font-lock-escape-face` for long labels.
* Added replacements for common invisible, zero-width, and bidi control characters.
* `unicode-inspector` now shows Code before Char, with `U+` codepoint formatting.

## [2025-12-30] 0.0.3

### Added

* Add a block table view per Unicode block, plus a codepoint list view with Name search.
* Add clickable block table cells that jump to the codepoint list filtered by Name.
* Add ASCII control code names and glyph display for control characters.
* Add `unicode-inspector-char-face` and `unicode-inspector-block-table-char-face` for styling.
* Add `unicode-inspector-show-trailing-whitespace` to control trailing whitespace display.
* Add `unicode-inspector-mode` with <kbd>q</kbd> bound to `quit-window`.

### Changed

* Block entries now provide both table navigation and a PDF link button with icons when available.

## [2025-12-30] 0.0.2

### Added

* Add `unicode-inspector-blocks.el` data file sourced from Unicode Blocks.txt.
* Add `unicode-inspector-unique-input` custom variable for de-duplicating inspected characters.

### Changed

* "Block" column now links to [Unicode chart](https://unicode.org/charts/) PDFs.

## [2025-12-29] 0.0.1

### Added

* The `unicode-inspector` command has been implemented with basic behavior.
