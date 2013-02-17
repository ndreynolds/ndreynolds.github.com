// Generated by CoffeeScript 1.3.3
(function() {
  var Cell, Grid, _ref;

  Cell = (function() {

    function Cell(x, y, alive) {
      this.x = x;
      this.y = y;
      this.alive = alive;
      this.nextState = this.alive;
    }

    Cell.prototype.applyNextState = function() {
      return this.alive = this.nextState;
    };

    return Cell;

  })();

  Grid = (function() {

    function Grid(w, h, freq) {
      var x, y;
      this.w = w;
      this.h = h;
      if (freq == null) {
        freq = 0.4;
      }
      this.generation = 0;
      this.population = 0;
      this.cells = (function() {
        var _i, _ref, _results;
        _results = [];
        for (y = _i = 0, _ref = h - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; y = 0 <= _ref ? ++_i : --_i) {
          _results.push((function() {
            var _j, _ref1, _results1;
            _results1 = [];
            for (x = _j = 0, _ref1 = w - 1; 0 <= _ref1 ? _j <= _ref1 : _j >= _ref1; x = 0 <= _ref1 ? ++_j : --_j) {
              _results1.push(new Cell(x, y, Math.random() > freq));
            }
            return _results1;
          })());
        }
        return _results;
      })();
    }

    Grid.prototype.cellAt = function(x, y) {
      return this.cells[y][x];
    };

    Grid.prototype.eachCell = function(f) {
      var cell, x, y, _i, _ref, _results;
      _results = [];
      for (y = _i = 0, _ref = this.h - 1; 0 <= _ref ? _i <= _ref : _i >= _ref; y = 0 <= _ref ? ++_i : --_i) {
        _results.push((function() {
          var _j, _ref1, _results1;
          _results1 = [];
          for (x = _j = 0, _ref1 = this.w - 1; 0 <= _ref1 ? _j <= _ref1 : _j >= _ref1; x = 0 <= _ref1 ? ++_j : --_j) {
            cell = this.cellAt(x, y);
            _results1.push(f(cell, this.neighborsOf(cell)));
          }
          return _results1;
        }).call(this));
      }
      return _results;
    };

    Grid.prototype.nextGeneration = function() {
      var _this = this;
      this.eachCell(function(cell, neighbors) {
        var liveNeighbors, n, _ref;
        liveNeighbors = (function() {
          var _i, _len, _results;
          _results = [];
          for (_i = 0, _len = neighbors.length; _i < _len; _i++) {
            n = neighbors[_i];
            if (n.alive) {
              _results.push(n);
            }
          }
          return _results;
        })();
        if (cell.alive) {
          return cell.nextState = (_ref = liveNeighbors.length) === 2 || _ref === 3;
        } else {
          return cell.nextState = liveNeighbors.length === 3;
        }
      });
      this.population = 0;
      this.eachCell(function(cell) {
        cell.applyNextState();
        if (cell.alive) {
          return _this.population += 1;
        }
      });
      return this.generation += 1;
    };

    Grid.prototype.neighborsOf = function(cell) {
      return this.horizontalTo(cell).concat(this.diagonalTo(cell).concat(this.verticalTo(cell)));
    };

    Grid.prototype.horizontalTo = function(cell) {
      if (cell.x === this.w - 1) {
        return [this.cellAt(cell.x - 1, cell.y)];
      }
      if (cell.x === 0) {
        return [this.cellAt(cell.x + 1, cell.y)];
      }
      return [this.cellAt(cell.x - 1, cell.y), this.cellAt(cell.x + 1, cell.y)];
    };

    Grid.prototype.diagonalTo = function(cell) {
      var diagonals;
      diagonals = [];
      if (cell.y > 0) {
        if (cell.x > 0) {
          diagonals.push(this.cellAt(cell.x - 1, cell.y - 1));
        }
        if (cell.x < this.w - 1) {
          diagonals.push(this.cellAt(cell.x + 1, cell.y - 1));
        }
      }
      if (cell.y < this.h - 1) {
        if (cell.x > 0) {
          diagonals.push(this.cellAt(cell.x - 1, cell.y + 1));
        }
        if (cell.x < this.w - 1) {
          diagonals.push(this.cellAt(cell.x + 1, cell.y + 1));
        }
      }
      return diagonals;
    };

    Grid.prototype.verticalTo = function(cell) {
      if (cell.y === this.h - 1) {
        return [this.cellAt(cell.x, cell.y - 1)];
      }
      if (cell.y === 0) {
        return [this.cellAt(cell.x, cell.y + 1)];
      }
      return [this.cellAt(cell.x, cell.y - 1), this.cellAt(cell.x, cell.y + 1)];
    };

    return Grid;

  })();

  _ref = [Grid, Cell], this.Grid = _ref[0], this.Cell = _ref[1];

  this.run = function(w, h) {
    var grid, i, _i;
    grid = new Grid(w, h);
    for (i = _i = 1; _i <= 10; i = ++_i) {
      grid.nextGeneration();
    }
    return console.log(grid);
  };

}).call(this);
