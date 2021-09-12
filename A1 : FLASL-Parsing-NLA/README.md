## How to Run:

### flasl to ast

To convert `arg-inp.flasl` to `arg.sml`, run the command:  
`make flasl2ast`
or  
`sml flasl2ast.sml arg-inp.flasl arg.sml`  
  
  
This will create a new file `arg.sml` which consists an element of Argument transformed from `arg-inp.flasl` text.

### ast to flasl

To convert back from `arg.sml` to `arg-out.flasl`, run the command:
`make ast2flasl`
or  
`sml ast2flasl.sml arg.sml arg-out.flasl`
  

This will create a new file `arg-out.flasl` which is of format `flasl`. it may not be the exact same of `arg-inp.flasl` but AST of both files will be same. To confirm this, we do `validation`.

### Validation
For validation, we again transform the newly created `arg-out.flasl` file to AST and save it to `arg2.sml`.
Then we can check that both files `arg.sml` and `arg2.sml` are exact same.

Commands:
`make validation`
or  
```
sml flasl2ast.sml arg-out.flasl arg2.sml
diff arg.sml arg2.sml
```

### Clean:
use `make clean` to remove unwanted files.
use `make clean-output` to remove `arg.sml`, `arg-out.flasl` and `arg2.sml`.

Thanks  
Manoj Kumar  
2018CS50411  