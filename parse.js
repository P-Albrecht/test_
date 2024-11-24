
const require = createRequire(import.meta.url);
const express = require('express')
const app = express();

var cors = require('cors');
app.use(cors());

export const PRINT = false;

export function changeTemplateCrodox (project, fileContent) {
    let error = false;
    let errorMessage = [];
    let parsers = [];

    let stack = constructStack(fileContent);
    let parserResult = parse(stack)
    parsers = parserResult.parsers
    error = parserResult.error;
    errorMessage = parserResult.errorMessage;

    let saveFileObject;

    if (!error) {
        for (let i=0; i<saveFileObject.length; i++) {
            if (saveFileObject[i].project === project) {
                saveFileObject[i].parsers = parsers;
                saveFileObject[i].crodoxText = fileContent
            }
        }

    }
    return { error:error, errorMessage:errorMessage}
}  

export function addTemplate (project, repo, fileContent) {
    let error = false;
    let errorMessage = [];
    let parsers = [];

    let parserResult = parse(stack)
    parsers = parserResult.parsers
    errorMessage = parserResult.errorMessage;

    if (!error) {
        let saveCrodoxTemplateResult = saveCrodoxTemplate(project, repo, parsers, fileContent);
        errorMessage = errorMessage.concat(saveCrodoxTemplateResult.errorMessage);
    }
    return { error:error, errorMessage:errorMessage}
}  

export function getTemplates () {
    let saveFileObject;
    try {
        saveFileObject = JSON.parse(saveFileString);
        return {error:false, errorMessage:[], templates:templates}
    } catch (e) {
        return {error:true, errorMessage:[{locations:[], message:e}], templates:[]}
    }
}  
