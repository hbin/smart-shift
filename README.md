# Smart Shift
**Smart Shift** is a minor mode for conveniently shift the line/region to the left/right by the current moajor mode indentation width.

## Installation
### Melpa(Coming soon...)
Once you have setup [Melpa](http://melpa.milkbox.net/#/getting-started) you can use `package-install` command to install. The package name is `smart-shift`.

### Manual

```lisp
(add-to-list 'load-path "/path/to/smart-shift.el")
(require 'smart-shift)
(smart-shift-mode 1)
```

## Customizing
**Smart Shift** will infer the indentation level of current major mode, if none of major modes listed below match, use the `tab-width` as default.

It can also be explictly set to a number or a function called without arguments and evaluting to a number.

```lisp
(setq smart-shift-indentation-level 2)
```

### Supported major modes
- ruby-mode
- js-mode
- coffee-mode
- css-mode
- scss-mode
- yaml-mode
- c-mode
- sh-mode
- slim-mode
- python-mode
- html-mode
- web-mode

## Interactive commands

Command              | Keybinding | Description
---------------------|------------|--------------------------------------------------------
   smart-shift-left  | `C-c [`    | Shift the line or region ARG times to the left.
   smart-shift-right | `C-c ]`    | Shift the line or region ARG times to the right.


## Contribute
Fork and pull request!
