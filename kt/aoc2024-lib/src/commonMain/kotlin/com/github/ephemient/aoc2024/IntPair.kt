package com.github.ephemient.aoc2024

data class IntPair(val first: Int, val second: Int) {
    override fun toString(): String = "($first, $second)"
}

infix fun Int.to(other: Int): IntPair = IntPair(this, other)
