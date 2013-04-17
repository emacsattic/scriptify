# About #

Scriptify makes it extremely easy to go from text in buffer to runnable script.

`M-x scriptify` does the following:

* insert shebang in the beginning of the buffer (if it is not already there)
* chop off extension (if any) of file
* move file to special directory for scripts (optional)
* set executable bit on

## Installation ##

If you use [el-get](https://github.com/dimitri/el-get), use this recipe:

```lisp
(:name scriptify
       :type github
       :pkgname "vderyagin/scriptify")
```

## Customization ##

`M-x customize-group RET scriptify RET`

* `scriptify-scripts-directory` variable contains path to directory you want to
put scripts in, presumably it should be in `PATH` enviroment variable. Set
`scriptify-scripts-directory` to `nil` in order not to move files anywhere.

* `scriptify-shebang-alist` variable is an alist that contains mapping from
  major-mode symbols to shebang strings (or string-returning functions).
