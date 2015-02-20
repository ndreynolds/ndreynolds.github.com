function initCell() {
  return Math.random() > 0.8;
}

function initGrid(x, y, val) {
  var i, j, row, grid = [];
  val = val || initCell;
  for (i = 0; i < y; i++) {
    row = []; 
    for (j = 0; j < x; j++) {
      row.push(typeof val === 'function' ? val() : val);
    }
    grid.push(row);
  }
  return grid;
}

function neighbors(grid, x, y) {
  var xMax = grid[0].length,
      yMax = grid.length,
      xInc = x + 1 == xMax ? 0 : x + 1,
      xDec = x - 1 < 0 ? xMax - 1 : x - 1,
      yInc = y + 1 == yMax ? 0 : y + 1,
      yDec = y - 1 < 0 ? yMax - 1 : y - 1;

  return [
    grid[y][xInc],    grid[y][xDec],
    grid[yInc][x],    grid[yDec][x],
    grid[yDec][xDec], grid[yDec][xInc],
    grid[yInc][xDec], grid[yInc][xInc]
  ];
}

function aliveNeighbors(grid, x, y) {
  return neighbors(grid, x, y).filter(function(n) { return n; });
}

function nextState(grid, x, y) {
  var liveCount = aliveNeighbors(grid, x, y).length,
      alive = grid[y][x];
  if (alive) 
    return liveCount === 2 || liveCount === 3;
  else
    return liveCount === 3;
}

function nextGeneration(grid) {
  var newGrid = initGrid(grid[0].length, grid.length);
  for (y = 0; y < grid.length; y++) {
    for (x = 0; x < grid[0].length; x++) {
      newGrid[y][x] = nextState(grid, x, y);
    }
  }
  return newGrid;
}

function drawCell(ctx, grid, x, y, res) {
  res = res || 5;
  var cx = x * res,
      cy = y * res,
      alive = grid[y][x];

  ctx[alive ? 'fillRect' : 'clearRect'](cx, cy, res - 1, res - 1);
}
