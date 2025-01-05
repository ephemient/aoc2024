"""
Day 16: Reindeer Maze
"""

import math
from collections import defaultdict
from enum import IntFlag, auto
from heapq import heappop, heappush
from typing import Generator

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


class _Move(IntFlag):
    FORWARD = auto()
    LEFT = auto()
    RIGHT = auto()


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


def _explore(maze, start) -> Generator[tuple[int, int, int, int, int, _Move]]:
    queue, visited = [(0, *start, 0, 1, _Move(0))], {}
    while queue:
        score, y, x, dy, dx, move = heappop(queue)
        last_score = visited.get((y, x, dy, dx), math.inf)
        if last_score < score:
            continue
        yield score, y, x, dy, dx, move
        if last_score == score:
            continue
        visited[(y, x, dy, dx)] = score
        for score, y, x, dy, dx, move in (
            (score + 1, y + dy, x + dx, dy, dx, _Move.FORWARD),
            (score + 1001, y - dx, x + dy, -dx, dy, _Move.LEFT),
            (score + 1001, y + dx, x - dy, dx, -dy, _Move.RIGHT),
        ):
            if (y, x) not in maze:
                heappush(queue, (score, y, x, dy, dx, move))


def part1(data: str) -> int | None:
    """
    >>> part1(SAMPLE_INPUT_1)
    7036
    >>> part1(SAMPLE_INPUT_2)
    11048
    """
    maze, start, end = _parse(data)
    for score, y, x, *_ in _explore(maze, start):
        if (y, x) == end:
            return score
    return None


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_1)
    45
    >>> part2(SAMPLE_INPUT_2)
    64
    """
    maze, start, end = _parse(data)
    best, paths = None, defaultdict(lambda: _Move(0))
    for score, y, x, dy, dx, move in _explore(maze, start):
        if best is not None and best < score:
            break
        paths[(y, x, dy, dx)] |= move
        if (y, x) == end:
            best = score
    stack = [(*end, *d) for d in ((0, 1), (1, 0), (0, -1), (-1, 0))]
    visited = set(stack)
    while stack:
        y, x, dy, dx = stack.pop()
        moves = paths[(y, x, dy, dx)]
        y, x = y - dy, x - dx
        for dy, dx, move in (
            (dy, dx, _Move.FORWARD),
            (dx, -dy, _Move.LEFT),
            (-dx, dy, _Move.RIGHT),
        ):
            key = y, x, dy, dx
            if moves & move and key not in visited:
                visited.add(key)
                stack.append(key)
    visited = {(y, x) for y, x, *_ in visited}
    return len({(y, x) for y, x, *_ in visited})


parts = (part1, part2)
