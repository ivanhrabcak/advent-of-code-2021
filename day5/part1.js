const fs = require('fs');

const file = fs.readFileSync('input.txt').toString();

let points = [];

file.split("\n")
            .map((x) => {
                const [start, end] = x.split(' -> ');
                return [
                    start.split(',').map(x => +x),
                    end.split(',').map(x => +x)
                ]
            })
            .map((x) => {
                const [start, end] = x;
                const [startX, startY] = start;
                const [endX, endY] = end;

                // diagonal line
                if (startX != endX && startY != endY) {
                    return undefined;
                }

                const points = []
                if (startX > endX) {
                    for (let i = startX; i != endX; i--) {
                        points.push([[i, startY], [endX, endY]]);
                    }
                }
                else if (startX < endX) {
                    for (let i = startX; i != endX; i++) {
                        points.push([i, startY], [endX, endY]);
                    }
                }
                else if (startY > endY) {
                    for (let j = startY; j != endY; j--) {
                        points.push([startX, j], [endX, endY]);
                    }
                }
                else if (startY < endY) {
                    for (let j = startY; j != endY; j++) {
                        points.push([startX, j], [endX, endY]);
                    }
                }

                return points;
            })
            .filter(x => x !== undefined).forEach(x => x.forEach(y => points.push(y)));

const markedPoints = {};
for (let point of points) {
    const p = `${point[0]}-${point[1]}`
    if (markedPoints[p] === undefined) {
        markedPoints[p] = 1;
    }
    else {
        markedPoints[p] += 1;
    }
}

console.log(Object.keys(markedPoints).filter((x) => markedPoints[x] != 1).length)