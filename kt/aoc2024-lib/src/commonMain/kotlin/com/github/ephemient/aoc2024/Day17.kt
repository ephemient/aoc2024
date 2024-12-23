package com.github.ephemient.aoc2024

class Day17 internal constructor(input: String, machine: (IntList) -> Machine) {
    constructor(input: String) : this(input, ::MachineImpl)

    private val a: Long
    private val b: Long
    private val c: Long
    private val program = IntList()
    init {
        var a = 0L
        var b = 0L
        var c = 0L
        for (match in pattern.findAll(input)) {
            match.groups[1]?.run { a = value.toLong() }
            match.groups[2]?.run { b = value.toLong() }
            match.groups[3]?.run { c = value.toLong() }
            match.groups[4]?.run { value.split(',').mapTo(program) { it.toLong() } }
        }
        this.a = a
        this.b = b
        this.c = c
    }
    private val machine = machine(program)

    fun part1() = machine(a, b, c)

    fun part2(): Long {
        var candidates = listOf(0L)
        while (candidates.isNotEmpty()) {
            candidates = buildList {
                for (base in candidates) {
                    for (a in 8 * base..8 * base + 7) {
                        val output = machine(a, b, c)
                        if (program == output) return a
                        if (output.size < program.size &&
                            output == program.subList(program.size - output.size, program.size)
                        ) add(a)
                    }
                }
            }
        }
        TODO()
    }

    class IntList : AbstractMutableList<Long>() {
        private var values = longArrayOf()

        override var size: Int = 0
            private set

        private fun checkIndex(index: Int, size: Int = this.size) {
            if (index !in 0..<size) throw IndexOutOfBoundsException("Index $index out of bounds for length $size")
        }

        override fun get(index: Int): Long {
            checkIndex(index)
            return values[index]
        }

        override fun removeAt(index: Int): Long {
            checkIndex(index)
            return values[index].also { values.copyInto(values, index, index + 1, --size) }
        }

        override fun set(index: Int, element: Long): Long {
            checkIndex(index)
            return values[index].also { values[index] = element }
        }

        override fun add(index: Int, element: Long) {
            checkIndex(index, size = size + 1)
            if (values.size == size) values = values.copyOf(if (size == 0) 1 else 2 * size)
            values.copyInto(values, index + 1, index, size++)
            values[index] = element
        }

        override fun toString(): String = indices.joinToString(",") { this[it].toString() }
    }

    interface Machine {
        operator fun invoke(a: Long, b: Long, c: Long): IntList
    }

    private class MachineImpl(private val program: IntList) : Machine {
        override fun invoke(a: Long, b: Long, c: Long) = IntList().apply {
            var a = a
            var b = b
            var c = c
            var ip = 0
            while (ip in program.indices) {
                val instruction = program[ip]
                val operand = program[ip + 1]
                val combo = when (operand) {
                    in 0..3 -> operand
                    4L -> a
                    5L -> b
                    6L -> c
                    else -> TODO()
                }
                when (instruction) {
                    0L -> a = a shr combo.toInt()
                    1L -> b = b xor operand
                    2L -> b = combo and 7
                    3L -> if (a != 0L) {
                        ip = operand.toInt()
                        continue
                    }
                    4L -> b = b xor c
                    5L -> add(combo and 7L)
                    6L -> b = a shr combo.toInt()
                    7L -> c = a shr combo.toInt()
                    else -> TODO()
                }
                ip += 2
            }
        }
    }

    companion object {
        private val pattern = """Register A: (\d+)|Register B: (\d+)|Register C: (\d+)|Program: ([\d,]*)""".toRegex()
    }
}
