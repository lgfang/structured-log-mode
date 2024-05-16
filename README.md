# Structured Log Mode #

An Emacs minor mode that displays log files formatted as JSON Lines in a more
human-friendly format.

While JSON Lines format is easy to parse by machines, it is not very human
friendly due to the extra punctuation and keys. This mode tries to make it
easier to read by
- hiding the punctuation and keys;
- pretty printing the line at cursor in an associated side window.

## Screenshots ##

Below are screenshots of viewing a MongoDB log file without and with this mode.
- Before the `structured-log-mode` is enabled:
  ![Before](./screenshots/screenshot-off.png)

- After the `structured-log-mode` is enabled:
  ![After](./screenshots/screenshot-on.png)

## Prerequisites ##

- Emacs 29.1 or later

  This package uses the Emacs built-in packages `treesit` and the
  `json-ts-mode`, which are available since Emacs 29.1. Too double check if your
  Emacs has built in `treesit` support, please evaluate `(treesit-available-p)`.

## Installation ##

1. Git clone or download this repository.

2. Edit your Emacs init file to load it.

    If you are using `use-package`, you can add the following code to your init
    file:

        ``` emacs-lisp
        (use-package structured-log-mode
          :load-path "/path/to/structured-log-mode"
          :commands structured-log-mode)

        (use-package json-ts-mode
          :mode "\\.jsonl?\\'" "mongod*\\.log"
          )
        ```

    Note that the second `use-package` is optional. It tells Emacs to open
    MongoDB log files using `json-ts-mode`.

    If you are not using `use-package`, please add the following code to your
    init file:

        ``` emacs-lisp
        (add-to-list 'load-path "/path/to/structured-log-mode")
        (require 'structured-log-mode)
        ```

3. If haven't done yet, install the corresponding tree-sitter grammar by running
   `M-x treesit-install-grammars` and selecting `json` from the list.

4. Restart Emacs or run `M-x load-file` on your init file.

### Large Files ###

This mode processes the contents in the current window only. Hence, it is able
to handle large files. On my laptop, **after disabling any features which could
potentially slow down Emacs**, at the least, it is able to handle a 1GB `mongod`
log file without issues.

I put the following configuration in my `init.el` to ensure that Emacs loads
large files with minimal features to prevent Emacs from slowing or freezing.

``` emacs-lisp
  (defun lgf-huge-file-hook ()
    "Open huge files with minimum features.

Huge files (normally log files) can make Emacs sluggish or even
freeze. This hook tells Emacs to open such files with the
`fundamental-mode' and turn off any extra features which cannot
handle large files. In addition, it makes the buffer read only to
avoid accidental modifications."
    (when (> (buffer-size) (* 1024 1024 16)) ; 16 MB
      (fundamental-mode)
      (buffer-disable-undo)
      (which-function-mode -1)
      (if (fboundp 'highlight-parentheses-mode) (highlight-parentheses-mode -1))
      (setq buffer-read-only t)
      ))
  (add-hook 'find-file-hook 'lgf-huge-file-hook)
```

## Usage ##

1. Open a log file that is formatted as JSON Lines.

2. Enable the `structured-log-mode` by running `M-x structured-log-mode`.

3. The log file should now be displayed in a more human-friendly format.

   *Note* If some lines in the window are not processed, please press `Ctrl-l`
   (or evaluate `(recenter-top-bottom)`) to refresh.

4. Move the cursor to a line to see the corresponding JSON data in the side
   window.

5. Press `c-c c-s` to show all the original contents. Press `c-s c-h` to hide
   again.

6. Run `M-x structured-log-mode` again to disable the mode.


## Customization ##

- `structlog-timer-delay` - the idle time (delay in seconds) before updating the
  side window.

## TODO ##

- [ ] Better whitespace handling.
- [ ] Highlight according to log level.
- [ ] Add customization options.
