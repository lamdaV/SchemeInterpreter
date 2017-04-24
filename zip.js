const JSZip = require("jszip");
const fs = require("fs");
const path = require("path");

const zip = new JSZip();
const SCHEME_FILE_EXTENSIONS = [".ss", ".scm"];

const DIRECTORY = process.argv[2];
if (process.argv[2]) {
  console.log("[ INFO ] : source directory at ", DIRECTORY);
} else {
  console.log("[ ERROR ] : usage: node zip.js <source_directory>");
  return;
}

function getProjectFiles() {
  return new Promise((resolve, reject) => {
    fs.readdir(DIRECTORY, (error, files) => {
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
      fs.readFile(path.resolve(DIRECTORY, file), (error, data) => {
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
