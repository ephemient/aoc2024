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

    fun part2(): Int {
        val x = (0..<WIDTH).maxBy { t ->
            val halves = IntArray(2)
            for (robot in robots) {
                val x = (robot.x + robot.vx * t).mod(WIDTH)
                halves[if (x < WIDTH / 2) 0 else if (x > WIDTH / 2) 1 else continue]++
            }
            halves.max()
        }
        val y = (0..<HEIGHT).maxBy { t ->
            val halves = IntArray(2)
            for (robot in robots) {
                val y = (robot.y + robot.vy * t).mod(HEIGHT)
                halves[if (y < HEIGHT / 2) 0 else if (y > HEIGHT / 2) 1 else continue]++
            }
            halves.max()
        }
        egcd(WIDTH, HEIGHT).let { (s, t, g) ->
            require(g == 1)
            WIDTH * s + HEIGHT * t == g
        }
        return (x + (y - x) * INVERSE * WIDTH).mod(WIDTH * HEIGHT)
    }

    private data class Robot(val x: Int, val y: Int, val vx: Int, val vy: Int)

    companion object {
        private const val WIDTH = 101
        private const val HEIGHT = 103
        private val INVERSE = egcd(WIDTH, HEIGHT).let { (s, _, g) ->
            require(g == 1)
            s
        }
        private val pattern = """p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)""".toRegex()
    }
}
