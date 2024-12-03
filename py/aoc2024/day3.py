"""
Day 3: Mull It Over
"""

import re

SAMPLE_INPUT_1 = """
xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
"""
SAMPLE_INPUT_2 = """
xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
"""

_pattern1 = re.compile(r"mul\((\d+),(\d+)\)")
_pattern2 = re.compile(r"(do\(\))|(don't\(\))|mul\((\d+),(\d+)\)")


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_1)
    161
    """
    return sum(int(m.group(1)) * int(m.group(2)) for m in _pattern1.finditer(data))


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_2)
    48
    """
    enabled, total = True, 0
    for m in _pattern2.finditer(data):
        if m.group(1):
            enabled = True
        elif m.group(2):
            enabled = False
        elif enabled:
            total += int(m.group(3)) * int(m.group(4))
    return total


parts = (part1, part2)
