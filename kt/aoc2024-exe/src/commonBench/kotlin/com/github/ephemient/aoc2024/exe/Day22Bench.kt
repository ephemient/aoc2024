package com.github.ephemient.aoc2024.exe

import com.github.ephemient.aoc2024.Day22
import kotlinx.benchmark.Benchmark
import kotlinx.benchmark.Blackhole
import kotlinx.benchmark.Scope
import kotlinx.benchmark.Setup
import kotlinx.benchmark.State

@State(Scope.Benchmark)
class Day22Bench {
    private lateinit var input: String

    @Setup
    fun setup() {
        input = getDayInput(22)
    }

    @Benchmark
    fun part1() = Day22(input).part1()

    @Benchmark
    fun part2() = Day22(input).part2()

    @Benchmark
    fun solve(bh: Blackhole) {
        val day22 = Day22(input)
        bh.consume(day22.part1())
        bh.consume(day22.part2())
    }
}