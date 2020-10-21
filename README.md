This is a vscode extension for formatting your `Racket` code files (forked from [Shuumatsu/racket-pretty-printer](https:/github.com/Shuumatsu/racket-pretty-printer)).

## Features

Format your racket code using DrRacket's indentation settings or Racket's pretty-function.
Use command `format-racket` or turn on "format on save" to use this extension.

Before:
```
(+ 1
2
  3)
```

After (indentation mode):
```
(+ 1
   2
   3)
```

After (pretty-print mode):
```
(+ 1 2 3)
```

## Requirements

Make sure you have `racket` installed and placed in path.

## Extension Settings

```
{
  "racket-formatter.formatingMode": ["indentation", "pretty print"]
}
```

**Enjoy!**
