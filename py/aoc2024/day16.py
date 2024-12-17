"""
Day 16: Reindeer Maze
"""

from heapq import heappop, heappush

SAMPLE_INPUT_1 = """
###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"""
SAMPLE_INPUT_2 = """
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
"""


def _parse(data: str) -> tuple[set[tuple[int, int]], tuple[int, int], tuple[int, int]]:
    maze = set()
    for y, line in enumerate(data.splitlines()):
        for x, char in enumerate(line):
            match char:
                case "#":
                    maze.add((y, x))
                case "S":
                    start = y, x
                case "E":
                    end = y, x
    return maze, start, end


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_1)
    7036
    >>> part1(SAMPLE_INPUT_2)
    11048
    """
    maze, start, end = _parse(data)
    queue, visited = [(0, *start, 0, 1)], set()
    while queue:
        score, y, x, dy, dx = heappop(queue)
        if (y, x, dy, dx) in visited:
            continue
        if (y, x) == end:
            return score
        visited.add((y, x, dy, dx))
        for score, y, x, dy, dx in (
            (score + 1, y + dy, x + dx, dy, dx),
            (score + 1001, y - dx, x + dy, -dx, dy),
            (score + 1001, y + dx, x - dy, dx, -dy),
        ):
            if (y, x) not in maze:
                heappush(queue, (score, y, x, dy, dx))


def part2(data: str) -> tuple[int, int]:
    """
    >>> part2(SAMPLE_INPUT_1)
    45
    >>> part2(SAMPLE_INPUT_2)
    64
    """
    maze, start, end = _parse(data)
    best, acc = None, set()
    queue, visited = [(0, *start, 0, 1, {start})], {}
    while queue:
        score, y, x, dy, dx, path = heappop(queue)
        if best is not None and score > best:
            break
        if (y, x, dy, dx) in visited and score > visited[(y, x, dy, dx)]:
            continue
        visited[(y, x, dy, dx)] = score
        if (y, x) == end:
            best = score
            acc |= path
        for score, y, x, dy, dx in (
            (score + 1, y + dy, x + dx, dy, dx),
            (score + 1001, y - dx, x + dy, -dx, dy),
            (score + 1001, y + dx, x - dy, dx, -dy),
        ):
            if (y, x) not in maze and (y, x) not in path:
                heappush(queue, (score, y, x, dy, dx, path | {(y, x)}))
    return len(acc)


parts = (part1, part2)
