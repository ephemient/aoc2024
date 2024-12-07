package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day2
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day2Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(2)
    }

    @Benchmark
    fun part1() = Day2(input).part1()

    @Benchmark
    fun part2() = Day2(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day2 = Day2(input)
        bh.consume(day2.part1())
        bh.consume(day2.part2())
    }
}
