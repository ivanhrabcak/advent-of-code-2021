const fs = require('fs')
const FILENAME = 'input.txt'
 
const result = fs.readFileSync(FILENAME, 'utf-8')
                .split('\n')
                .map(x => parseInt(x.replace('\n', '')))
                .reduce((acc, e, i, arr) => { 
                    if (i != 0) { return e > arr[i-1] ? acc + 1 : acc } 
                    else { return acc } 
                }, 0)

console.log(result)
