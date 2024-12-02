"""
Day 2: Red-Nosed Reports
"""

from typing import Generator

SAMPLE_INPUT = """
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"""


def _parse(data: str) -> Generator[list[int]]:
    for line in data.splitlines():
        line = line.strip()
        if line:
            yield [int(level) for level in line.split()]


def _issafe1(report: list[int]) -> bool:
    decreasing, increasing = False, False
    for x, y in zip(report, report[1:]):
        delta = x - y
        if -3 <= delta <= -1:
            decreasing = True
            if increasing:
                break
        elif 1 <= delta <= 3:
            increasing = True
            if decreasing:
                break
        else:
            break
    else:
        return True
    return False


def _issafe2(report: list[int]) -> bool:
    report2 = report[:-1].copy()
    for i in range(len(report2), 0, -1):
        if _issafe1(report2):
            return True
        report2[i - 1] = report[i]
    return _issafe1(report2)


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    2
    """
    return sum(map(_issafe1, _parse(data)))


def part2(data) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    4
    """
    return sum(map(_issafe2, _parse(data)))


parts = (part1, part2)
