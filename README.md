# Smart Shift
**Smart Shift** is a minor mode for conveniently shift the line/region to the left/right by the current major mode indentation width.

## Installation

### Melpa
Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install. The package name is `smart-shift`.

### Manual

```lisp
(add-to-list 'load-path "/path/to/smart-shift")
(require 'smart-shift)
(global-smart-shift-mode 1)
```

## Customizing
**Smart Shift** will infer the indentation level of current major mode, if none of major modes listed below match, use the `tab-width` as default.

It can also be set to a number explictly.

```lisp
(setq smart-shift-indentation-level 2)
```
Or, for some major mode we haven't support, add following snippets to your config file. Test it and send a PR.
```lisp
(eval-after-load 'your-major-mode
  '(progn
     (add-to-list 'smart-shift-mode-alist
                  '(major-mode-or-derived-mode . customize-base-offset))))
```

### Supported major modes
- lisp-mode
- emacs-lisp-mode
- c-mode
- c++-mode
- objc-mode
- java-mode
- idl-mode
- pike-mode
- awk-mode
- ruby-mode
- python-mode
- swift-mode
- js-mode
- js2-mode
- coffee-mode
- css-mode
- scss-mode
- slim-mode
- html-mode
- web-mode
- sh-mode
- yaml-mode
- text-mode
- markdown-mode
- fundamental-mode

## Interactive commands

Command              | Keybinding | Description
---------------------|------------|--------------------------------------------------------
   smart-shift-left  | `C-c [`    | Shift the line or region ARG times to the left.
   smart-shift-right | `C-c ]`    | Shift the line or region ARG times to the right.

After invoking `smart-shift-left` or `smart-shift-right` the first time, you can simply hit `[` or `]` to continuously shift to left or right, respectively.

If you use the [key-chord](http://www.emacswiki.org/emacs/key-chord.el) like me. I strongly recommend you add the following snippets:

```lisp
(key-chord-define-global "<<" 'smart-shift-left)
(key-chord-define-global ">>" 'smart-shift-right)
```

## Contribute
Forks and pull requests are welcome!
