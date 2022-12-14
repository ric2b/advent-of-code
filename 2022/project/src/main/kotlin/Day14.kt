package day14

import kotlin.math.max
import kotlin.math.min

data class Pos(val x: Int, val y: Int)

fun part1(input: String): Int {
    val sandSource = Pos(500, 0)
    val rockPaths: List<List<Pos>> = input.trimEnd().lines().map { rawRockPath ->
        rawRockPath.split(" -> ").map {
            val (x, y) = it.split(",").map { coord -> coord.toInt() }
            Pos(x, y)
        }
    }

    val rocks: MutableSet<Pos> = mutableSetOf()
    rockPaths.forEach { rockPath ->
        rockPath.zip(rockPath.drop(1)).forEach { (a, b) ->
            if (a.x != b.x) {
                for (i in min(a.x, b.x)..max(a.x, b.x)) { rocks.add(Pos(i, a.y)) }
            }

            if (a.y != b.y) {
                for (j in min(a.y, b.y)..max(a.y, b.y)) { rocks.add(Pos(a.x, j)) }
            }
        }
    }

    val rockBaseY = rocks.maxOf { it.y }
    val initialRocks = rocks.size

    while (true) {
        var sand = sandSource
        while (sand.y <= rockBaseY) {
            sand = if (Pos(sand.x, sand.y + 1) !in rocks) {
                       Pos(sand.x, sand.y + 1)
                    } else if (Pos(sand.x - 1, sand.y + 1) !in rocks) {
                        Pos(sand.x - 1, sand.y + 1)
                    } else if (Pos(sand.x + 1, sand.y + 1) !in rocks) {
                        Pos(sand.x + 1, sand.y + 1)
                    } else {
                        rocks.add(sand)
                        break
                    }
        }
        if (sand.y >= rockBaseY) break
    }

    return rocks.size - initialRocks
}

fun part2(input: String): Int {
    val sandSource = Pos(500, 0)
    val rockPaths: List<List<Pos>> = input.trimEnd().lines().map { rawRockPath ->
        rawRockPath.split(" -> ").map {
            val (x, y) = it.split(",").map { coord -> coord.toInt() }
            Pos(x, y)
        }
    }

    val rocks: MutableSet<Pos> = mutableSetOf()
    rockPaths.forEach { rockPath ->
        rockPath.zip(rockPath.drop(1)).forEach { (a, b) ->
            if (a.x != b.x) {
                for (i in min(a.x, b.x)..max(a.x, b.x)) { rocks.add(Pos(i, a.y)) }
            }

            if (a.y != b.y) {
                for (j in min(a.y, b.y)..max(a.y, b.y)) { rocks.add(Pos(a.x, j)) }
            }
        }
    }

    val rockBaseY = rocks.maxOf { it.y }
    val initialRocks = rocks.size

    while (true) {
        var sand = sandSource

        while (true) {
            sand = if (Pos(sand.x, sand.y + 1) !in rocks && sand.y + 1 < 2 + rockBaseY) {
                Pos(sand.x, sand.y + 1)
            } else if (Pos(sand.x - 1, sand.y + 1) !in rocks && sand.y + 1 < 2 + rockBaseY) {
                Pos(sand.x - 1, sand.y + 1)
            } else if (Pos(sand.x + 1, sand.y + 1) !in rocks && sand.y + 1 < 2 + rockBaseY) {
                Pos(sand.x + 1, sand.y + 1)
            } else {
                rocks.add(sand)
                break
            }
        }

        if (sand == sandSource) break
    }

    return rocks.size - initialRocks
}