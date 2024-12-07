"""
Day 7:
"""

from typing import Callable, Iterable

SAMPLE_INPUT = """
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"""


def _solve(data: str, op: Callable[[int, int], Iterable[int]]) -> int:
    total = 0
    for line in filter(None, data.splitlines()):
        lhs, rhs = line.split(":", maxsplit=1)
        lhs, rhs = int(lhs), [int(value) for value in rhs.split()]

        def ok(x, remaining):
            y = remaining[-1]
            return (
                x == y
                if len(remaining) == 1
                else any(ok(z, remaining[:-1]) for z in op(x, y))
            )

        if ok(lhs, rhs):
            total += lhs
    return total


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    3749
    """

    def op(x, y):
        values = []
        if x >= y:
            values.append(x - y)
        if not x % y:
            values.append(x // y)
        return values

    return _solve(data, op)


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    11387
    """

    def op(x, y):
        values = []
        if x >= y:
            values.append(x - y)
        if not x % y:
            values.append(x // y)
        x, y = str(x), str(y)
        if x.endswith(y) and len(x) > len(y):
            values.append(int(x[: -len(y)]))
        return values

    return _solve(data, op)


parts = (part1, part2)
