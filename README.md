# sbcli

A better REPL for SBCL. It handles errors greacefully, is not too verbose,
and has readline capabilities, including multiline input and reset.

## Installation

For most cases, calling `./install.sh` should suffice. It will
install `sbcli` into `$PREFIX/bin`, with the environment variable `PREFIX`
defaulting to `/usr/local`. If you are using Mac and having
issues with `cl-readline` see the [installation notes for
cl-readline](https://github.com/mrkkrp/cl-readline#installation).

## Dependencies

`sbcli` depends on [Quicklisp](http://quicklisp.org/) and
[cl-readline](https://github.com/mrkkrp/cl-readline).
If you have Quicklisp installed, cl-readline will be installed
on `sbcli`s first launch. `sbcli` assumes that Quicklisp is
installed under `~/quicklisp`. This value can be overridden
by setting `$QUICKLISP_HOME`.

## Usage

You should be able to launch `sbcli` by just typing `sbcli`. Once you’re in the
REPL, you can edit normally with readline capabilities. Hitting tab will
autocomplete (note that if there is more than one possibility, you’ll have to
hit tab twice).

Typing `,help` will list all available special commands.

Typing `,def` will give you an overview over what you’ve defined.

Typing `,h symbol` will enter inspection mode for a symbol. Typing `CTRL-D`
exits inspection mode.

Typing `,q`, `CTRL-D`, or `CTRL-C` will exit the REPL.

Typing `,r` resets the environment.

Typing `,d symbol` dumps the disassembly of a symbol.

Typing `,t <expression>` prints the type returned by an expression.

Typing `,s filename` will save all of the expressions that were typed and
evaluated to the file specified, in the format:

```lisp
(+ 1 2) ; => 3
(* 6 10) ; => 60
; *last-result* contains the last result
(+ 2 *last-result*) ; => 62
```

Of course, depending on your result, this can result in very long lines, or
break if your result contains newlines. Use at your own peril for now!

## Customization

If you want you can add customizations to `sbcli`. On startup
it will load `~/.config/sbcli/init.lisp` if it exists, honoring
the value of `$XDG_CONFIG_HOME`.
Placing the init file at `~/.sbclirc` is considered deprecated.
You can execute arbitrary code there, two of
the more interesting values to set are `*prompt*` and `*ret*`.
Check out an example resource file
[here](https://github.com/hellerve/sbcli/blob/master/examples/.sbclirc).

### Exposed Variables

For reference, here is a complete list of the
variables we expose and their default values:

```lisp
;;; these are constants and cannot be changed

;; the name and version of the REPL
+repl-name+    ; => "Veit's REPL for SBCL"
+repl-version+ ; => "0.1.3"
+home-directory+ ; => "~/.config/sbcli/"
+init-file+    ; => "~/.config/sbcli/init.lisp"

;;; these are variables you can customize

;; the startup and shutdown messages
*welcome-msg* ; => "Veit's REPL for SBCL version 0.1.3"
*goodbye-msg* ; => "Bye for now."

;; the prompt and continuation prompt variables
*prompt*       ; => "sbcl> "
*prompt2*      ; => "....> "

;; the return value prompt
*ret*          ; => "=> "

;; where to store the history or nil to not store it
*hist-file*    ; => "~/.config/sbcli/history"

;; the list of all special commands
*special*

;;; these are variables you shouldn't customize

;; the last result and history variables
*last-result*  ; => nil
*hist*         ; => ()
```

---

Have fun!
