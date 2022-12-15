package day15

import kotlin.math.abs
import kotlin.math.max
import kotlin.math.min

data class Pos(val x: Int, val y: Int)
data class Region(val center: Pos, val distance: Int) {
    infix fun contains(position: Pos): Boolean {
        val dx = abs(center.x - position.x)
        val dy = abs(center.y - position.y)
        return dx + dy <= distance
    }

    fun rangeAtY(y: Int): IntRange {
        val xRange = distance - abs(y - center.y)
        return if (xRange > 0) (center.x - xRange)..(center.x + xRange) else IntRange.EMPTY
    }

    fun pointsAtY(y: Int): Set<Pos> = rangeAtY(y).map { Pos(it, y) }.toSet()
}

data class Sensor(val position: Pos, val closestBeacon: Pos) {
    fun emptyRegion(): Region {
        val dx = abs(position.x - closestBeacon.x)
        val dy = abs(position.y - closestBeacon.y)
        val distance = dx + dy

        return Region(position, distance)
    }
}

fun part1(input: String, y: Int = 2000000): Int {
    val sensors: Set<Sensor> = input.trimEnd().lines().map { line ->
        val (sx, sy, bx, by) = Regex("[-\\d]+").findAll(line).flatMap { it.groupValues }.map { it.toInt() }.toList()
        Sensor(Pos(sx, sy), Pos(bx, by))
    }.toSet()

    // Simpler approach
    //    val rangesAtY = sensors.map { it.emptyRegion().rangeAtY(y) }
    //    val rangeAtY = (rangesAtY.minOf { it.start }..rangesAtY.maxOf { it.endInclusive })
    //    val beaconsAtY = sensors.map { it.closestBeacon }.filter { it.y == y }.toSet().size
    //
    //    return rangeAtY.count { x -> rangesAtY.any { it.contains(x) } } - beaconsAtY

    // Faster approach
    val sortedRangesAtY = sensors.map { it.emptyRegion().rangeAtY(y) }.filter { !it.isEmpty() }.sortedBy { it.start }
    val reducedRangesAtY = mutableListOf(sortedRangesAtY.first())

    sortedRangesAtY.drop(1).forEach {
        val last = reducedRangesAtY.last()

        if (it.start <= last.endInclusive) {
            reducedRangesAtY.removeLast()
            reducedRangesAtY.add(last.start..max(it.endInclusive, last.endInclusive))
        } else {
            reducedRangesAtY.add(it)
        }
    }

    return reducedRangesAtY.sumOf { it.endInclusive - it.start }
}

fun part2(input: String, range: IntRange = 0..4000000): Long {
    val sensors: Set<Sensor> = input.trimEnd().lines().map { line ->
        val (sx, sy, bx, by) = Regex("[-\\d]+").findAll(line).flatMap { it.groupValues }.map { it.toInt() }.toList()
        Sensor(Pos(sx, sy), Pos(bx, by))
    }.toSet()

    val emptyRegions = sensors.map { it.emptyRegion() }

    emptyRegions.forEach { region ->
        val dxRange = -min(region.distance, region.center.x)..min(region.distance, range.endInclusive - region.center.x)
        dxRange.forEach { dx ->
            val dy = (region.distance + 1 - dx)

            listOf(Pos(region.center.x + dx, region.center.y + dy), Pos(region.center.x + dx, region.center.y - dy))
                .filter { neighbour -> neighbour.y in range }
                .forEach { neighbour ->
                    if (emptyRegions.none { it contains neighbour }) {
                        return 4000000 * neighbour.x.toLong() + neighbour.y.toLong()
                    }
                }
        }
    }

    throw IllegalArgumentException()
}