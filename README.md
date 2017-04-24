# Scheme interpreter

A Scheme Interprester built in Scheme to interpret Scheme.

## Authors

David Lam Adam Finer

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
