const PRINT = false;

export class Graph {

    constructor(bridges) {
        this.AdjList = new Map();
        this.idOffset = 0;
        this.bridges = bridges;
        this.fileList = [];
        this.openFileDependencies = [];
    }


    extract() {
        let objectList = [];
        let keys = this.AdjList.keys();
        for (let i of keys){
            objectList.push({key:i, object:this.AdjList.get(i)})
        }
        return {objectList:objectList, idOffset:this.idOffset, bridges:this.bridges, fileList:this.fileList, openFileDependencies:this.openFileDependencies}
    }

    reconstruct(graph) {
        this.AdjList = new Map();

        for (let i=0; i<graph.objectList.length; i++) {
            this.AdjList.set(graph.objectList[i].key, graph.objectList[i].object);
        }

        this.idOffset = graph.idOffset;
        this.bridges = graph.bridges;
        this.fileList = graph.fileList;
        this.openFileDependencies = graph.openFileDependencies;
    }

    addFileObjects(fileObjects) {
        let keyList = [];
        fileObjects.forEach(file => {
            let idOffset = this.idOffset;
            let fileChildren = [];
            for (let i=0; i<file.tree.length; i++) {
                fileChildren.push(idOffset+1+file.tree[i].id);
            }
            this.addVertex(idOffset, {title:'file', file:file.file, fileBridge:file.fileBridge, fileText:file.fileText, variables:[], children:[...fileChildren], dependencies:[], varDeps:[], subBodyVars:[]}, idOffset);
            keyList.push(idOffset);
            this.fileList.push({id:idOffset, path:file.file, fileText:file.fileText});
            
            idOffset++;
            
            let objects = file.objectList;
            let nodeList = [];
            for (let i=0; i<objects.length; i++) {
                nodeList.push(objects[i]);          
                
                objects[i].children = objects[i].children.map(item => item + idOffset);
                this.addVertex((idOffset+i), objects[i], (idOffset-1));
                keyList.push(idOffset+i);
            }

            this.idOffset = idOffset+objects.length;
        });

        //---------------------------------------------------------------------------------------------------------
        for (let i=0; i<keyList.length; i++) {
            let childrenList = this.AdjList.get(keyList[i]).children;
            for (let j=0; j<childrenList.length; j++) {
                this.AdjList.get(childrenList[j]).parent = keyList[i];
                this.AdjList.get(childrenList[j]).siblings = childrenList;
            }
        }
        this.subbodyVarsPushup(keyList);
        this.setLocalVarSelection(keyList);
        this.setLocalFlowHistory(keyList);
        this.setVariableDependencies(keyList);
        //---------------------------------------------------------------------------------------------------------

        let keys = this.AdjList.keys();
        for (let i of keys) this.AdjList.get(i).dependencies = [...this.AdjList.get(i).varDeps];
        this.setBridgeDependencies();
        this.setPushupDependencies ();
        this.setChildDependencies ();
    }


    addVertex(v, object, fileId) {


        let object_ = {
            title:object.title, 
            file:object.file,                           //!!!!!!!!!!!!!!!!!!!!
            fileBridge:object.fileBridge, 
            //fileText:object.fileText, 
            variables:object.variables, 
            //children:object.children, 
            //dependencies:object.dependencies, 
        }


        this.AdjList.set(v, {dependencies:[], varDeps:[], object:{...object_}, fileId:fileId, parent:-1, siblings:[], children:[...object.children], localVar:[], secondaryVariables:[], localFlowHistory:[]});
    }

    setLocalFlowHistory (keyList) {
        let list = []
        for (let i=0; i<keyList.length; i++) {
            if (this.AdjList.get(keyList[i]).parent === -1) {
                list.push(keyList[i]);
                this.walkFileTree(keyList[i], []);
            }
        }
    }

    setVariableDependencies (keyList) {
        for (let i=0; i<keyList.length; i++) {
            let key = keyList[i];
            let localVar = this.AdjList.get(key).localVar;
            let localFlowHistory = this.AdjList.get(key).localFlowHistory
            let variables = this.AdjList.get(key).object.variables;
            for (let j = 0; j<variables.length; j++) {
                let varTitle = variables[j].varTitle;   
                if (varTitle.indexOf('"')===-1 && !varTitle.startsWith("!") && !varTitle.endsWith("!") && varTitle===varTitle.toLowerCase()) {
                    if (varTitle.indexOf('"') === -1) {
                        if (varTitle.startsWith("'") && varTitle.endsWith("'")) {
                            // 'title'
                            varTitle = varTitle.substring(1, varTitle.length-1);
                        } else {
                            if (varTitle.endsWith(':global')) {
                                // global
                                varTitle = varTitle.substring(0, varTitle.length-7);
                            } else if (varTitle.endsWith(':up')) { 
                                // up
                                varTitle = varTitle.substring(0, varTitle.length-3);
                            } else if (varTitle.endsWith(':down')) { 
                                // down
                                varTitle = varTitle.substring(0, varTitle.length-5);            
                            } else if (varTitle.endsWith(':siblings')) { 
                                // siblings
                                varTitle = varTitle.substring(0, varTitle.length-9);
                            }
                        }
                    
                    } 
                }
            }
        }
    }

    setBridgeDependencies () {
        let bridgeConnections = [];

        this.openFileDependencies = [] //Todo:change to key list

        let keys = this.AdjList.keys();
        for (let i of keys) {
            let element = this.AdjList.get(i);
            let variables = element.object.variables;
            for (let j = 0; j<variables.length; j++) {
                if (variables[j].varTitle.indexOf('"') !== -1) {
                    // "title"
                    let match = false;
                    let matchUnpares = false;

                    /////////////////////////////////////////////////////////////////////////////
                    let text = variables[j].text.substring(1, variables[j].text.length-1);
                    let realPath = convertRelativePath(text, this.AdjList.get(this.AdjList.get(i).fileId).object.file);
                    /////////////////////////////////////////////////////////////////////////////

                    if (!match&&!matchUnpares) console.log('id:' + i + '  §§§§§§§§§§    varTitle:' + variables[j].varTitle + '  text:' + text + '    no Match !!!');
                }
            }
        }

        for (let i=0; i<bridgeConnections.length; i++) {      
            let element1 = this.AdjList.get(bridgeConnections[i].objectId);
            let element2 = this.AdjList.get(bridgeConnections[i].fileId);
            for (let j = 0; j <  element1.object.variables.length; j++) {
                let varTitle = element1.object.variables[j].varTitle.toLowerCase();
                if (varTitle.indexOf('"') === -1) {
                    varTitle = varTitle.toLowerCase();
                    if (varTitle.indexOf(':')!==-1) varTitle = varTitle.substring(0,varTitle.indexOf(':'));

                    let fileObject = element2.localVar.find((variable) => (variable.varTitle.toLowerCase()===varTitle && variable.text===element1.object.variables[j].text));
                    if (fileObject!==undefined) {
                        if (PRINT) console.log('id:' + bridgeConnections[i].objectId + '  §§§§§§§§§§    depId:' + fileObject.id + '                    ' + JSON.stringify(fileObject) + '   ' + varTitle);
                        this.AdjList.get(bridgeConnections[i].objectId).dependencies.push(fileObject.id);
                    }
                }
            }
        }
    }
    
    setPushupDependencies () {
        let fileDependencies = []
        let keys = this.AdjList.keys();
        for (let i of keys) {
            let variables = this.AdjList.get(i).object.variables;
            for (let j=0; j<variables.length; j++) {
                let varTitle = variables[j].varTitle;
                if (varTitle.indexOf('"')===-1 && varTitle.startsWith("!") && varTitle.endsWith("!")) {
                    // !title!
                    varTitle = varTitle.substring(1, varTitle.length-1);
                    let objectDeclaration = varTitle.substring(varTitle.indexOf('[')+1,varTitle.indexOf(']'));
                    let varDeclaration = varTitle.substring(varTitle.indexOf(']')+1);
                    let declaredObjects = [];
                    if (objectDeclaration!=='') declaredObjects = objectDeclaration.split('|');
                    /////////////////////////////////////////////////////////////////
                    let dependencies = this.getSelection([i]);
                    /////////////////////////////////////////////////////////////////
                    for (let k=0; k<dependencies.length; k++) {
                        if (declaredObjects.length===0 || declaredObjects.indexOf(this.AdjList.get(dependencies[k]).object.title)!==-1) {
                            let secondaryVariables = this.AdjList.get(dependencies[k]).secondaryVariables;
                            for (let l=0; l<secondaryVariables.length; l++) {
                                let match = false;
                                let subBodyObjectsVariables = this.AdjList.get(secondaryVariables[l]).object.variables
                                if (match) {
                                    console.log('id:' + i + '  !!!!!!!!    varTitle:' + secondaryVariables[l] + '  text:' + variables[j].text);
                                    if (this.AdjList.get(i).dependencies.indexOf(secondaryVariables[l])) this.AdjList.get(i).dependencies.push(secondaryVariables[l]);
                                }
                            }
                        }
                    }
                }
            }
        }
        return fileDependencies;
    }


    getSelection(selectedOrg) {
        let selected = [...selectedOrg]
        let selection = [];
        while (selected.length>0) {
            let pop = selected.pop();
            let dependencies = [...this.AdjList.get(pop).dependencies].filter(a => (selection.indexOf(a)===-1 && selected.indexOf(a)===-1))
        }
        return selection;
    }

    getFileDependencies(selection) {
        let fileDependencies = [];
        for (let i=0; i<this.openFileDependencies.length; i++) {
            if (selection.indexOf(this.openFileDependencies[i].id)!==-1) fileDependencies.push(this.openFileDependencies[i]);
        }
        return fileDependencies;
    }

    printTree() {
        let list = []
        let keys = this.AdjList.keys();
        for (let i of keys) {
            if (this.AdjList.get(i).parent === -1) {
                list.push(i)
            }
        }
        let indents = Array(list.length).fill(0)

        while (0 < list.length) {
            let pop = list.pop();
            let indent = indents.pop();
            let element = this.AdjList.get(pop);
            let children = element.children;
              
            let stringA = ' ' + Array.from(' '.repeat(8*indent)).join('') + JSON.stringify(pop) + '  ';
            let stringB = '' + element.object.title;
            let stringC = '';
            for (let j = 0; j <  element.object.variables.length; j++) {
                stringC = stringC + ' ' + element.object.variables[j].text + '(' + element.object.variables[j].varTitle + ')';
            }
            //let stringD = '';
            //for (let j = 0; j <  element.secondaryVariables.length; j++) {
            //    stringD = stringD + ' ' + element.secondaryVariables[j].text + '(' + element.secondaryVariables[j].id + ')';
            //}
            let stringD = '';
            for (let j = 0; j<element.secondaryVariables.length; j++) {
                stringD = stringD + ' ' + this.AdjList.get(element.secondaryVariables[j]).object.title + '(' + element.secondaryVariables[j] + ')';
            }
            //let stringF = JSON.stringify(element.localFlowHistory);
            let stringF = JSON.stringify(element.dependencies);

            if (stringA.length>50) stringA = stringA.substring(0,50-3) + '...'
            if (stringB.length>20) stringB = stringB.substring(0,20-3) + '...'
            if (stringC.length>50) stringC = stringC.substring(0,50-3) + '...'
            if (stringD.length>60) stringD = stringD.substring(0,40-3) + '...'
            if (stringF.length>20) stringF = stringF.substring(0,20-3) + '...'
            
            let string = ''
            string = string + stringA + Array.from('-'.repeat(50-stringA.length)).join('') + '  ';
            string = string + stringB + Array.from(' '.repeat(20-stringB.length)).join('') + '  ';
            string = string + stringC + Array.from(' '.repeat(50-stringC.length)).join('') + '  ';
            string = string + stringD + Array.from(' '.repeat(40-stringD.length)).join('') + '  ';
            string = string + stringF + Array.from(' '.repeat(20-stringF.length)).join('') + '  ';
            console.log(string);
            for (let j = 0; j <  element.localVar.length; j++) {
                //console.log(Array.from(' '.repeat(stringA.length)).join('') + '|  ' + element.localVar[j].text + ' [' + element.localVar[j].varTitle + '|' + element.localVar[j].id + ']');
            }

            let sortList = this.sortList(children)
            list = list.concat(sortList.reverse());
            indents = indents.concat(Array(children.length).fill(indent+1));
        }
    }
}

function convertRelativePath(relativePath, file) {
    let dir = file.substring(0, file.lastIndexOf('/'));
    let pathString = relativePath;
    if (pathString.startsWith('./')) {
        pathString = dir + pathString.substring(1)
    } else if (pathString.startsWith('../') && dir.lastIndexOf('/') > -1) {             //maybe not necessary
        let locationSplit = dir.split('/')
        while(pathString.startsWith('../')) {
            pathString = pathString.substring(3);
            locationSplit.pop();
        }
        if (locationSplit.length>0) pathString = locationSplit.join('/') + '/' + pathString
    }
    return pathString;
}



