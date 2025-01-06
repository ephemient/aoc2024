package com.github.ephemient.aoc2024

fun gcd(x: Long, y: Long): Long {
    var a = x
    var b = y
    while (b != 0L) a = b.also { b = a.mod(b) }
    return a
}

fun gcd(x: Int, y: Int): Int {
    var a = x
    var b = y
    while (b != 0) a = b.also { b = a.mod(b) }
    return a
}

fun egcd(x: Int, y: Int): Triple<Int, Int, Int> {
    if (y == 0) return Triple(1, 0, x)
    val q = x.floorDiv(y)
    val r = x.mod(y)
    val (s, t, g) = egcd(y, r)
    return Triple(t, s - q * t, g)
}

fun egcd(x: Long, y: Long): Triple<Long, Long, Long> {
    if (y == 0L) return Triple(1, 0, x)
    val q = x.floorDiv(y)
    val r = x.mod(y)
    val (s, t, g) = egcd(y, r)
    return Triple(t, s - q * t, g)
}

fun lcm(x: Int, y: Int): Int = x / gcd(x, y) * y

fun lcm(x: Long, y: Long): Long = x / gcd(x, y) * y
