const JSZip = require("jszip");
const fs = require("fs");

const zip = new JSZip();

const SCHEME_FILE_EXTENSIONS = [".ss", ".scm"];

const projectFiles = ["main.ss", "apply-proc.ss", "chez-init.ss", "datatypes.ss", "env.ss", "evaluator.ss", "interpreter.ss", "parse.ss", "syntax-expand.ss"];
const numberOfFiles = projectFiles.length;

let readCounter = 0;

function getProjectFiles() {
  return new Promise((resolve, reject) => {
    fs.readdir(".", (error, files) => {
      if (error) {
        return reject(error);
      } else {
        return resolve(files.filter((file) => {
          return SCHEME_FILE_EXTENSIONS.some((extension) => {
            return file.endsWith(extension);
          });
        }));
      }
    });
  });
}

function readToZip(files) {
  const numberOfFiles = files.length;
  let readCounter = 0;

  return new Promise((resolve, reject) => {
    files.forEach((file) => {
      fs.readFile(file, (error, data) => {
        if (error) {
          return reject(error);
        } else {
          zip.file(file, data);
          readCounter++;

          if (readCounter === numberOfFiles) resolve();
        }
      });
    });
  });
}

function zipFiles() {
  console.log("[ INFO ] : Read Completed");
  console.log("[ INFO ] : Zip Start");
  zip.generateNodeStream({type: "nodebuffer", streamFiles: true})
    .pipe(fs.createWriteStream("interpreter.zip"))
    .on("finish", () => {console.log("[ INFO ] : Zip Completed")});
}

function displayError(error) {
  console.log("[ ERROR ] : ", error.message);
}

getProjectFiles()
  .then(readToZip)
  .catch(displayError)
  .then(zipFiles)
  .catch(displayError);
