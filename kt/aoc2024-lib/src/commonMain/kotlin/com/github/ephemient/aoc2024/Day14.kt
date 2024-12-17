package com.github.ephemient.aoc2024

class Day14(input: String) {
    private val robots = input.lineSequence().mapNotNull {
        val (x, y, vx, vy) = pattern.matchEntire(it)?.destructured ?: return@mapNotNull null
        Robot(x.toInt(), y.toInt(), vx.toInt(), vy.toInt())
    }.toList()

    fun part1(width: Int = WIDTH, height: Int = HEIGHT): Int {
        val quadrants = IntArray(4)
        for (robot in robots) {
            val x = (robot.x + robot.vx * 100).mod(width)
            val y = (robot.y + robot.vy * 100).mod(height)
            quadrants[
                (if (x < width / 2) 0 else if (x > width / 2) 1 else continue) or
                    if (y < height / 2) 0 else if (y > height / 2) 2 else continue
            ]++
        }
        return quadrants.fold(1, Int::times)
    }

    fun part2() = (0..<WIDTH * HEIGHT).map { t ->
        val positions = robots.mapTo(mutableSetOf()) { robot ->
            (robot.x + robot.vx * t).mod(WIDTH) to (robot.y + robot.vy * t).mod(HEIGHT)
        }.sortedWith(compareBy(IntPair::second, IntPair::first))
        var consecutive = 1
        var maxConsecutive = 0
        for ((i, pos) in positions.withIndex()) {
            if (pos.first + 1 to pos.second == positions.getOrNull(i + 1)) {
                consecutive += 1
            } else {
                maxConsecutive = maxOf(consecutive, maxConsecutive)
                consecutive = 1
            }
        }
        t to maxConsecutive
    }.maxBy(IntPair::second).first

    private data class Robot(val x: Int, val y: Int, val vx: Int, val vy: Int)

    companion object {
        private const val WIDTH = 101
        private const val HEIGHT = 103
        private val pattern = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".toRegex()
    }
}
