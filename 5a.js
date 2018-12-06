function solve(str) {
  var chars = str.split("")
  var i = 0;
  while (i < chars.length) {
    if (i < chars.length - 1 && reacts(chars[i], chars[i+1])) {
      chars.splice(i, 2)
      i--
    } else {
      i++
    }
  }
  return chars.join("")
}

function reacts(a, b) {
  return a != b && a.toLowerCase() == b.toLowerCase()
}

console.log(solve("dabAcCaCBAcCcaDA"))
