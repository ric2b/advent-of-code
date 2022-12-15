package day15

import kotlin.math.abs

data class Pos(val x: Int, val y: Int)
data class Region(val center: Pos, val distance: Int) {
    infix fun contains(position: Pos): Boolean {
        val dx = abs(center.x - position.x)
        val dy = abs(center.y - position.y)
        return dx + dy <= distance
    }

    fun pointsAtY(y: Int): Set<Pos> {
        return (-distance..distance)
            .filter { dx -> y in center.y-(distance - abs(dx))..center.y+distance-abs(dx) }
            .map { dx -> Pos(center.x + dx, y) }
            .toSet()
    }
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

    return sensors.flatMap { it.emptyRegion().pointsAtY(y).minus(it.closestBeacon) }.toSet().size
}

fun part2(input: String, range: IntRange = 0..4000000): Long {
    val sensors: Set<Sensor> = input.trimEnd().lines().map { line ->
        val (sx, sy, bx, by) = Regex("[-\\d]+").findAll(line).flatMap { it.groupValues }.map { it.toInt() }.toList()
        Sensor(Pos(sx, sy), Pos(bx, by))
    }.toSet()

    val emptyRegions = sensors.map { it.emptyRegion() }

    emptyRegions.forEach {
        (-it.distance+1..it.distance+1).forEach { dx ->
            listOf(
                Pos(it.center.x + dx, it.center.y + (it.distance+1 - dx)),
                Pos(it.center.x + dx, it.center.y - (it.distance+1 - dx))
            )
                .filter { neighbour -> neighbour.x in range && neighbour.y in range }
                .forEach { neighbour ->
                    if (emptyRegions.none { r -> r contains neighbour }) {
                        return 4000000 * neighbour.x.toLong() + neighbour.y.toLong()
                    }
                }
        }
    }

    throw IllegalArgumentException()
}