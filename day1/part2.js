const fs = require('fs')
const FILENAME = 'input.txt'

const data = fs.readFileSync(FILENAME, 'utf-8')
                .split('\n')
                .map(x => parseInt(x.replace('\n', '')))

let one = -1
let two = -1
let three = -1

const result = Array.apply(null, { length: data.length }).map(x => 0)

data.forEach((n) => {
    if (one == -1) { one++ }
    else if (one == 0) { one++; two++ }
    else { one++; two++; three++ }

    [one, two, three].forEach(v => {
        if (v != -1) {
            result[v] += n
        } 
    })
})

console.log(result.reduce((acc, v, i, arr) => {
    if (i == 0) {
        return acc
    }

    if (arr[i] > arr[i-1]) {
        return acc + 1
    }

    return acc
}, 0))
