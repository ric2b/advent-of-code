package day23

data class Pos(val x: Int, val y: Int) {
    fun desiredMove(elfPositions: Set<Pos>, round: Int): Pos {
        return if(!hasNeighbours(elfPositions)) {
            this
        } else {
            proposals.indices.firstNotNullOfOrNull { i -> proposals[(round + i) % proposals.size](elfPositions) } ?: this
        }
    }

    private val proposals: Array<(Set<Pos>) -> Pos?> = arrayOf(
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x + it, y - 1)) }) Pos(x, y - 1) else null },
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x + it, y + 1)) }) Pos(x, y + 1) else null },
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x - 1, y + it)) }) Pos(x - 1, y) else null },
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x + 1, y + it)) }) Pos(x + 1, y) else null },
    )

    private fun hasNeighbours(elfPositions: Set<Pos>): Boolean {
        for (dx in -1..1) {
            for (dy in -1..1) {
                if (!(dx == 0 && dy == 0) && elfPositions.contains(Pos(x + dx, y + dy)))  {
                    return true
                }
            }
        }
        return false
    }
}

fun part1(input: String): Int {
    val elves = gameOfAdvent(input, iterationLimit = 10).second
    val area = (1 + elves.maxOf { it.y } - elves.minOf { it.y }) * (1 + elves.maxOf { it.x } - elves.minOf { it.x })
    return area - elves.count()
}

fun part2(input: String) = gameOfAdvent(input).first

fun gameOfAdvent(input: String, iterationLimit: Int = Int.MAX_VALUE): Pair<Int, Set<Pos>> {
    var elves: Set<Pos> = input.trimEnd().lines().flatMapIndexed { y, line ->
        line.toList().mapIndexedNotNull { x, value -> if(value == '#') Pos(x, y) else null }
    }.toSet()

    for (round in generateSequence(0) { it + 1 }) {
        if (round >= iterationLimit) return Pair(round, elves)

        val desiredMoves = elves.groupBy { it.desiredMove(elves, round) }
        val elvesMoving = desiredMoves.filterValues { it.size == 1 }.keys
        val elvesStaying = desiredMoves.values.filter { it.size > 1 }.flatten()
        val newPositions: Set<Pos> = elvesMoving + elvesStaying

        if (newPositions == elves) {
            return Pair(round + 1, elves)
        } else {
            elves = newPositions
        }
    }

    throw IllegalStateException()
}
