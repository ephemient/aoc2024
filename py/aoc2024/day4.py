"""
Day 4: Ceres Search
"""

SAMPLE_INPUT = """
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
"""


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT)
    18
    """
    lines = data.splitlines()
    return sum(
        all(
            (y2 := y + i * dy) in range(len(lines))
            and (x2 := x + i * dx) in range(len(lines[y2]))
            and lines[y2][x2] == c
            for i, c in enumerate("XMAS")
        )
        for y, line in enumerate(lines)
        for x in range(len(line))
        for dx in range(-1, 2)
        for dy in range(-1, 2)
    )


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT)
    9
    """
    lines = data.splitlines()
    return sum(
        "".join(chars) in ("MMASS", "MSAMS", "SMASM", "SSAMM")
        for above, line, below in zip(lines, lines[1:], lines[2:])
        for chars in zip(above, above[2:], line[1:], below, below[2:])
    )


parts = (part1, part2)
