import { getTemplates, changeTemplateCrodox } from './crodoxParser.js';
import { collectionGrammarSelection } from './inputParser.js';
import { Graph } from './graph.js';
import { appyAllTemplates, readFile } from './inputParser.js';


export function makeConnections (objects) {

    let g = new Graph_(objects);
    return g.graphSelection()
}

export function makeTreeStructure(treeData) {
    let tList = []
    treeData.forEach(element => {
        tList.push({name:element.path, sha:element.sha, type:element.type});
    });
    tList.sort(function(a, b){return b.name.length - a.name.length});

    let tree = []
    for (let i = 0; i < tList.length; i ++) {
        let isChild = false
        let j = i+1;
        while (j < tList.length && !isChild) {
            if (tList[i].name.startsWith(tList[j].name + '/') && tList[j].type == 'tree') {
                if (tList[j].children == undefined) {
                    tList[j].children = []
                }
                tList[j].children.push(tList[i]);
                isChild = true;
            }
            j ++;
        }

        if (!isChild) {
            tree.push(tList[i])
        }
    }
    return tree;
}


export function setTreeStructure(treeData) {
    let tList = []
    treeData.forEach(element => {
        tList.push({name:element.path, path:element.path, sha:element.sha, type:element.type, children:[]});
    });
    tList.sort(function(a, b){return b.name.length - a.name.length});

    let tree = []
    for (let i = 0; i < tList.length; i ++) {
        let isChild = false
        let j = i+1;
        while (j < tList.length && !isChild) {
            if (tList[i].name.startsWith(tList[j].name + '/') && tList[j].type == 'tree') {
                //tList[j].children.push(tList[i]);
                tList[j].children.push({name:tList[i].name.substring(tList[j].name.length+1), path:tList[i].name, sha:tList[i].sha, children:tList[i].children});
                isChild = true;
            }
            j ++;
        }

        if (!isChild) {
            //tree.push(tList[i])
            tree.push({name:tList[i].name, path:tList[i].name, sha:tList[i].sha, children:tList[i].children})
        }
    }
    return tree;
}


const port = process.env.Port || 3000;
app.listen(port, () => console.log('Listening on port ', port, '...'));
