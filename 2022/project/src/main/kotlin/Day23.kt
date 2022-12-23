package day23

data class Pos(val x: Int, val y: Int) {
    fun desiredMove(elfPositions: Set<Pos>, round: Int): Pos {
        if(neighbours().none { elfPositions.contains(it) }) return this

        proposals.indices.forEach { i ->
            val proposed = proposals[(i + round) % proposals.size](elfPositions)
            if (proposed != this ) return proposed
        }
        return this
    }

    private val proposals: Array<(Set<Pos>) -> Pos> = arrayOf(
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x + it, y - 1)) }) Pos(x, y - 1) else this },
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x + it, y + 1)) }) Pos(x, y + 1) else this },
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x - 1, y + it)) }) Pos(x - 1, y) else this },
        { elfPositions -> if ((-1..1).none { elfPositions.contains(Pos(x + 1, y + it)) }) Pos(x + 1, y) else this },
    )

    private fun neighbours(): List<Pos> {
        return (-1..1).flatMap { dy ->
            (-1..1).mapNotNull { dx -> if (dx != 0 || dy != 0) Pos(x+dx, y+dy) else null }
        }
    }
}

fun part1(input: String): Int {
    var elves: Set<Pos> = input.trimEnd().lines().flatMapIndexed { y, line ->
        line.toList().mapIndexedNotNull { x, value ->
            if(value == '#') Pos(x, y) else null
        }
    }.toSet()

    repeat(10) { round ->
        val proposedMovesToFrom: MutableMap<Pos, Pos?> = mutableMapOf()
        elves.forEach { proposedMovesToFrom.merge(it.desiredMove(elves, round), it) { _, _ -> null } }

        val newPositions = elves.filterNot { proposedMovesToFrom.values.contains(it) }.toMutableSet()
        newPositions.addAll(proposedMovesToFrom.keys)

        elves = newPositions
    }

    val size = (1 + elves.maxOf { it.y } - elves.minOf { it.y }) * (1 + elves.maxOf { it.x } - elves.minOf { it.x })
    return size - elves.size
}

fun part2(input: String): Int {
    var elves: Set<Pos> = input.trimEnd().lines().flatMapIndexed { y, line ->
        line.toList().mapIndexedNotNull { x, value ->
            if(value == '#') Pos(x, y) else null
        }
    }.toSet()

    for (round in generateSequence(0) { it + 1 }) {
        val proposedMovesToFrom: MutableMap<Pos, Pos?> = mutableMapOf()
        elves.forEach { proposedMovesToFrom.merge(it.desiredMove(elves, round), it) { _, _ -> null } }

        val newPositions = elves.filterNot { proposedMovesToFrom.values.contains(it) }.toMutableSet()
        newPositions.addAll(proposedMovesToFrom.keys)

        if (newPositions == elves) {
            return round + 1
        } else {
            elves = newPositions
        }
    }

    throw IllegalStateException()
}
