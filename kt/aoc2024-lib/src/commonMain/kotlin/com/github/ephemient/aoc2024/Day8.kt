package com.github.ephemient.aoc2024

class Day8(input: String) {
    private val height: Int
    private val width: Int
    private val antennae: Map<Char, List<IntPair>>
    init {
        val lines = input.trimEnd().lines()
        height = lines.size
        width = lines.maxOfOrNull { it.length } ?: 0
        antennae = buildMap<Char, MutableList<IntPair>> {
            for ((y, line) in lines.withIndex()) {
                for ((x, char) in line.withIndex()) {
                    if (char != '.') getOrPut(char) { mutableListOf() }.add(y to x)
                }
            }
        }
    }

    private fun MutableCollection<IntPair>.addIfInRange(y: Int, x: Int): Boolean =
        if (y in 0..<height && x in 0..<width) {
            add(y to x)
            true
        } else false

    private fun solve(allMultiples: Boolean): Int = buildSet {
        for ((char, points) in antennae) {
            for (p0 in points) {
                for (p1 in points) {
                    if (p0 == p1) continue
                    val dy = p1.first - p0.first
                    val dx = p1.second - p0.second
                    if (!allMultiples) {
                        addIfInRange(p1.first + dy, p1.second + dx)
                    } else {
                        var i = 0
                        while (addIfInRange(p1.first + i * dy, p1.second + i * dx)) i++
                    }
                }
            }
        }
    }.size

    fun part1() = solve(false)

    fun part2() = solve(true)
}
