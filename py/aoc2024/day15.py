"""
Day 15: Warehouse Woes
"""

SAMPLE_INPUT_1 = """
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
"""
SAMPLE_INPUT_2 = """
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
"""
SAMPLE_INPUT_3 = """
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
"""


class _Warehouse:
    def __init__(self, grid: str):
        self.grid = [list(line) for line in grid.strip("\n").splitlines()]
        for y, line in enumerate(self.grid):
            for x, char in enumerate(line):
                if char == "@":
                    self.pos = y, x

    def __str__(self) -> str:
        return "\n".join("".join(line) for line in self.grid)

    def double(self):
        return _Warehouse(
            "\n".join(
                "".join("@." if c == "@" else "[]" if c == "O" else c + c for c in line)
                for line in self.grid
            )
        )

    def move(self, char):
        match char:
            case "^":
                dy, dx = -1, 0
            case "v":
                dy, dx = 1, 0
            case "<":
                dy, dx = 0, -1
            case ">":
                dy, dx = 0, 1
            case _:
                return
        levels = []
        front = {self.pos}
        while front:
            levels.append(front)
            front = {(y + dy, x + dx) for y, x in front}
            for y, x in list(front):
                match self.grid[y][x]:
                    case ".":
                        front.remove((y, x))
                    case "[" if dy:
                        front.add((y, x + 1))
                    case "]" if dy:
                        front.add((y, x - 1))
                    case "O" | "[" | "]":
                        pass
                    case _:
                        return
        for front in levels[::-1]:
            for y, x in front:
                self.grid[y + dy][x + dx] = self.grid[y][x]
                self.grid[y][x] = "."
        self.pos = self.pos[0] + dy, self.pos[1] + dx

    def gps(self) -> int:
        return sum(
            100 * y + x
            for y, line in enumerate(self.grid)
            for x, char in enumerate(line)
            if char == "O" or char == "["
        )


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_2)
    2028
    >>> part1(SAMPLE_INPUT_1)
    10092
    """
    warehouse, moves = data.split("\n\n", maxsplit=1)
    warehouse = _Warehouse(warehouse)
    for move in moves:
        warehouse.move(move)
    return warehouse.gps()


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_3)
    618
    >>> part2(SAMPLE_INPUT_1)
    9021
    """
    warehouse, moves = data.split("\n\n", maxsplit=1)
    warehouse = _Warehouse(warehouse).double()
    for move in moves:
        warehouse.move(move)
    return warehouse.gps()


parts = (part1, part2)
