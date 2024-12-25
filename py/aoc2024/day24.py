"""
Day 24: Crossed Wires
"""

SAMPLE_INPUT_1 = """
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
"""
SAMPLE_INPUT_2 = """
x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
"""
SAMPLE_INPUT_3 = """
x00: 0
x01: 1
x02: 0
x03: 1
x04: 0
x05: 1
y00: 0
y01: 0
y02: 1
y03: 1
y04: 0
y05: 1

x00 AND y00 -> z05
x01 AND y01 -> z02
x02 AND y02 -> z01
x03 AND y03 -> z03
x04 AND y04 -> z04
x05 AND y05 -> z00
"""


def _parse(data: str) -> tuple[dict[str, bool], dict[str, tuple[str, str, str]]]:
    values, wires = {}, {}
    for line in data.splitlines():
        if ": " in line:
            key, value = line.split(": ")
            values[key] = bool(int(value))
        elif " -> " in line:
            value, key = line.split(" -> ")
            wires[key] = tuple(value.split())
    return values, wires


def part1(data: str) -> int:
    """
    >>> part1(SAMPLE_INPUT_1)
    4
    >>> part1(SAMPLE_INPUT_2)
    2024
    """
    values, wires = _parse(data)

    def eval(key: str) -> bool:
        if key in values:
            return values[key]
        match wires[key]:
            case lhs, "AND", rhs:
                value = eval(lhs) & eval(rhs)
            case lhs, "OR", rhs:
                value = eval(lhs) | eval(rhs)
            case lhs, "XOR", rhs:
                value = eval(lhs) ^ eval(rhs)
        values[key] = value
        return value

    return sum(eval(key) << int(key[1:]) for key in wires if key.startswith("z"))


def part2(data: str) -> int:
    """
    >>> part2(SAMPLE_INPUT_3)  # doctest: +SKIP
    'z00,z01,z02,z05'
    """
    _, wires = _parse(data)
    wires: dict[tuple[str, str, str], str] = dict(
        (tuple(sorted(value)), key) for key, value in wires.items()
    )
    carry, acc = None, set()

    def get(a: str | None, op: str, b: str | None) -> str | None:
        return None if a is None or b is None else wires.get(tuple(sorted((a, op, b))))

    def swap(a, b):
        assert a is not None and b is not None
        if a == b:
            return
        assert a not in acc and b not in acc
        acc.update((a, b))
        for key, value in wires.items():
            if value == a:
                wires[key] = b
            elif value == b:
                wires[key] = a

    first = True
    for z in sorted(key for key in list(wires.values()) if key.startswith("z")):
        x, y = "x" + z[1:], "y" + z[1:]
        if carry is None:
            assert first
            add = get(x, "XOR", y)
            swap(add, z)
            carry = wires.get(tuple(sorted((x, "AND", y))))
            first = False
        elif (halfadd := get(x, "XOR", y)) is None:
            swap(carry, z)
            carry = None
        else:
            if (fulladd := get(carry, "XOR", halfadd)) is None:
                alternative = get(x, "AND", y)
                swap(alternative, halfadd)
                halfadd = alternative
                fulladd = get(carry, "XOR", halfadd)
            swap(fulladd, z)
            overflow1 = get(x, "AND", y)
            overflow2 = get(halfadd, "AND", carry)
            carry = get(overflow1, "OR", overflow2)
    return ",".join(sorted(acc))


parts = (part1, part2)
