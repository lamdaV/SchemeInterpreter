# Scheme Interpreter

A Scheme Interprester built in Scheme to interpret Scheme.

## Authors

David Lam<br/>
Adam Finer

## Branch Details
 
There are two branches: `master` and `pass-by-reference`. 

The `master` branch contains most major milestone work for the class. The `pass-by-reference` branch contains an experimental feature in which our interpreter allowed the usage of 2 unique features not native to `scheme`: passing-by-reference and lexical addressing. To accomplish this, our interpreter heavily uses the ADT `chez-scheme` provides: `Box`.

Passing-by-reference feature allows an individual to create a `scheme` procedure such that certain parameters were to be taken as references rather than values. We accomplished this by creating a special `parameter` datatype and updating our parser to handle `ref` procedure parameters (i.e. `(lambda ((ref x)) ...)`). We also updated our `environment` datatype to store a vector of `Boxes`. Therefore, whenver our `closures` required a `ref` parameter, it would be passed the `Box` itself. Otherwise, the parameter would get the value within the box instead.

As for lexical addressing, we simply updated our parsing pipeline. After parsing the inital expressions and syntax expanding the parsed expressions, we then lexically addressed the expanded expression before attempting to evaluate it. This served to help improve our lookup time as we now know the lexical depth and lexical position within the chains of local environments or global environment if the variable was free.

## Zipping Files

As the project grew, it became obnoxious to zip the proper files to turn it in to the grading server. To overcome this entirely, David wrote `zip.js`, a NodeJS script, that will find all `.ss` or `.scm` files within a specified source directory and zip it all up into `interpreter.zip`.

To zip the files, run the following command:

```
  node zip.js <source_directory>
```

Currently, this repo is setup with an npm scripts that will default the `source_directory` to be `src`. To execute this, run the following command:

```
  npm start
```
