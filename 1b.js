const fs = require("fs")

const directions = fs.readFileSync("1.txt", "UTF-8")
  .split("\n")
  .map(x => parseInt(x))
  .filter(x => !Number.isNaN(x))

const solve = (directions) => {
  const found = {}
  let i = 0
  let current = 0
  while (true) {
    let next = directions[(i++) % directions.length] + current
    if (found[next]) {
      return next
    }
    found[next] = true
    current = next
  }
}

console.log(solve(directions))
