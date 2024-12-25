package com.github.ephemient.aoc2024

class Day24(input: String) {
    private val initialValues: Map<String, Boolean>
    private val connections: Map<String, Gate>
    init {
        val initialValues = mutableMapOf<String, Boolean>()
        val connections = mutableMapOf<String, Gate>()
        for (line in input.lineSequence()) {
            val (key, value, lhs, op, rhs, dst) = (pattern.matchEntire(line) ?: continue).destructured
            if (value.isNotEmpty()) {
                initialValues[key] = value.toInt() != 0
            } else if (op.isNotEmpty()) {
                connections[dst] = Gate(lhs, BinaryOperator.valueOf(op), rhs)
            }
        }
        this.initialValues = initialValues
        this.connections = connections
    }

    fun part1() = buildMap {
        putAll(initialValues)
        val get = DeepRecursiveFunction<String, Boolean> { key ->
            getOrPut(key) {
                val (lhs, op, rhs) = connections.getValue(key)
                val left = callRecursive(lhs)
                val right = callRecursive(rhs)
                when (op) {
                    BinaryOperator.AND -> left and right
                    BinaryOperator.OR -> left or right
                    BinaryOperator.XOR -> left xor right
                }
            }
        }
        for (key in connections.keys) get.invoke(key)
    }.filterKeys { it.startsWith("z") }.entries.sortedBy { it.key }.foldRight(0L) { (_, value), acc ->
        2 * acc + if (value) 1 else 0
    }

    fun part2() = buildSet {
        val connections = connections.entries.associateByTo(mutableMapOf(), { it.value }, { it.key })
        var carry: String? = null
        var first = true
        for (z in this@Day24.connections.keys.filter { it.startsWith("z") }.sorted()) {
            val x = z.replaceFirstChar { 'x' }
            val y = z.replaceFirstChar { 'y' }
            if (carry != null) {
                var halfAdd = connections[Gate(x, BinaryOperator.XOR, y)]
                if (halfAdd == null) {
                    if (carry != z) {
                        check(add(carry) and add(z))
                        connections.swap(carry, z)
                    }
                    carry = null
                } else {
                    val fullAdd = connections[Gate(halfAdd, BinaryOperator.XOR, carry)] ?: run {
                        val alternative = connections.getValue(Gate(x, BinaryOperator.AND, y))
                        check(add(halfAdd!!) and add(alternative))
                        connections.swap(halfAdd!!, alternative)
                        halfAdd = alternative
                        connections.getValue(Gate(alternative, BinaryOperator.XOR, carry!!))
                    }
                    if (fullAdd != z) {
                        check(add(fullAdd) and add(z))
                        connections.swap(fullAdd, z)
                    }
                    carry = connections[Gate(x, BinaryOperator.AND, y)]?.let { overflow1 ->
                        connections[Gate(halfAdd!!, BinaryOperator.AND, carry!!)]?.let { overflow2 ->
                            connections[Gate(overflow1, BinaryOperator.OR, overflow2)]
                        }
                    }
                }
            } else {
                check(first)
                first = false
                val add = connections.getValue(Gate(x, BinaryOperator.XOR, y))
                if (add != z) {
                    check(add(add) and add(z))
                    connections.swap(add, z)
                }
                carry = connections[Gate(x, BinaryOperator.AND, y)]
            }
        }
    }.sorted().joinToString(",")

    private enum class BinaryOperator {
        AND, OR, XOR
    }

    private class Gate(lhs: String, val operator: BinaryOperator, rhs: String) {
        val lhs = minOf(lhs, rhs)
        val rhs = maxOf(lhs, rhs)

        override fun equals(other: Any?): Boolean =
            other is Gate && operator == other.operator && lhs == other.lhs && rhs == other.rhs

        override fun hashCode(): Int = (lhs.hashCode() * 31 + operator.hashCode()) * 31 + rhs.hashCode()

        override fun toString(): String = "Gate(lhs=$lhs, operator=$operator, rhs=$rhs)"

        operator fun component1() = lhs
        operator fun component2() = operator
        operator fun component3() = rhs
    }

    companion object {
        private val pattern = """(\w+): ([01])|(\w+) (AND|OR|XOR) (\w+) -> (\w+)""".toRegex()

        private fun <K, V> MutableMap<K, V>.swap(u: V, v: V) {
            val iterator = iterator()
            for (entry in iterator) {
                when (entry.value) {
                    u -> entry.setValue(v)
                    v -> entry.setValue(u)
                }
            }
        }
    }
}
