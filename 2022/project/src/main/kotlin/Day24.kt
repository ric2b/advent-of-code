package day24

import kotlin.math.max

data class Pos(val y: Int, val x: Int) {
    fun neighbours() = listOf(Pos(y, x + 1), Pos(y + 1, x), this, Pos(y, x - 1) , Pos(y - 1, x))
}

data class Blizzard(val initialPosition: Pos, val direction: Char) {
    fun positionAt(minute: Int, mapEnd: Pos): Pos {
        val height = mapEnd.y
        val width = mapEnd.x
        return when(direction) {
            '>' -> Pos(initialPosition.y, (initialPosition.x - 1 + minute).mod(width - 1) + 1)
            '<' -> Pos(initialPosition.y, (initialPosition.x - 1 - minute).mod(width - 1) + 1)
            '^' -> Pos((initialPosition.y - 1 - minute).mod(height - 1) + 1, initialPosition.x)
            'v' -> Pos((initialPosition.y - 1 + minute).mod(height - 1) + 1, initialPosition.x)
            else -> throw IllegalArgumentException()
        }
    }
}

fun part1(input: String): Int {
    val inputLines = input.trimEnd().lines()
    val start = Pos(0, 1)
    val end = Pos(inputLines.lastIndex, inputLines.last().lastIndex - 1)
    val blizzards = inputLines.flatMapIndexed { y, row ->
        row.mapIndexedNotNull { x, location ->
            if (location in listOf('<', '>', '^', 'v')) Blizzard(Pos(y, x), location) else null
        }
    }.toSet()

    return bfs(start, end, blizzards)
}

fun part2(input: String): Int {
    val inputLines = input.trimEnd().lines()
    val start = Pos(0, 1)
    val end = Pos(inputLines.lastIndex, inputLines.last().lastIndex - 1)
    val blizzards = inputLines.flatMapIndexed { y, row ->
        row.mapIndexedNotNull { x, location ->
            if (location in listOf('<', '>', '^', 'v')) Blizzard(Pos(y, x), location) else null
        }
    }.toSet()

    val timeToGoal = bfs(start, end, blizzards)
    val timeToGetSnack = bfs(end, start, blizzards, initialMinute = timeToGoal)
    return bfs(start, end, blizzards, initialMinute = timeToGetSnack)
}

fun bfs(start: Pos, end: Pos, blizzards: Set<Blizzard>, initialMinute: Int = 0): Int {
    val blizzardRowIndex = blizzards.filter { it.direction in listOf('<', '>') }.groupBy { it.initialPosition.y }
    val blizzardColumnIndex = blizzards.filter { it.direction in listOf('^', 'v') }.groupBy { it.initialPosition.x }
    val mapEnd = Pos(max(start.y, end.y), max(start.x, end.x) + 1)

    val queue: MutableList<Pair<Pos, Int>> = mutableListOf(Pair(start, initialMinute))
    val visited: MutableSet<Pair<Pos, Int>> = mutableSetOf(Pair(start, initialMinute))

    while (queue.isNotEmpty()) {
        val (position, minute) = queue.removeFirst()

        if (position == end) {
            return minute
        }

        position.neighbours()
            .filter { it == start || it == end || (it.x in 1 until mapEnd.x && it.y in 1 until mapEnd.y) }
            .filter { Pair(it, minute + 1) !in visited }
            .filter { neighbour -> blizzardRowIndex[neighbour.y]?.none { it.positionAt(minute + 1, mapEnd) == neighbour } ?: true }
            .filter { neighbour -> blizzardColumnIndex[neighbour.x]?.none { it.positionAt(minute + 1, mapEnd) == neighbour } ?: true }
            .forEach { neighbour ->
                visited.add(Pair(neighbour, minute + 1))
                queue.add(Pair(neighbour, minute + 1))
            }
    }

    throw IllegalArgumentException()
}
