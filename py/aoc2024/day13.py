"""
Day 13: Claw Contraption
"""

import re
from typing import Generator

SAMPLE_INPUT = """
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
"""

_pattern = re.compile(r"\d+")

def _parse(data: str) -> Generator[tuple[int, int, int, int, int, int]]:
    return zip(*((int(match.group(0)) for match in _pattern.finditer(data)),) * 6)

def _solve(ax: int, ay: int, bx: int, by: int, x: int, y: int) -> int:
    anumerator = x * by - y * bx
    adenominator = ax * by - bx * ay
    bnumerator = x * ay - y * ax
    bdenominator = ay * bx - by * ax
    if anumerator % adenominator or bnumerator % bdenominator:
        return 0
    a = anumerator // adenominator
    b = bnumerator // bdenominator
    return 3 * a + b

def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    480
    """
    return sum(_solve(ax, ay, bx, by, x, y) for ax, ay, bx, by, x, y in _parse(data))

def part2(data: str) -> int:
    return sum(_solve(ax, ay, bx, by, x + 10000000000000, y + 10000000000000) for ax, ay, bx, by, x, y in _parse(data))

parts = (part1, part2)
