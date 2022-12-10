package day05

fun part1(input: String): String {
    val (rawStacks, rawMoves) = input.trimEnd().split("\n\n").map {it.lines() }

    val stackIndexes = rawStacks.last().mapIndexedNotNull { index, c -> c.digitToIntOrNull()?.let { index } }
    val stacks: List<MutableList<Char>> = List(stackIndexes.size) { i ->
        rawStacks.dropLast(1).mapNotNull { level ->
            val stackValue = level[stackIndexes[i]]
            if (stackValue != ' ') stackValue else null
        }.toMutableList()
    }

    val moves = rawMoves.map { rawMove ->
        Regex("move (\\d+) from (\\d+) to (\\d+)")
            .findAll(rawMove).flatMap { it.groupValues }.toList().drop(1)
            .map { it.toInt() }
    }

    moves.forEach { (n, from, to) ->
        repeat(n) { stacks[to - 1].add(0, stacks[from - 1].removeFirst())  }
    }

    return stacks.map { it[0] }.joinToString("")
}

fun part2(input: String): String {
    val (rawStacks, rawMoves) = input.trimEnd().split("\n\n").map {it.lines() }

    val stackIndexes = rawStacks.last().mapIndexedNotNull { index, c -> c.digitToIntOrNull()?.let { index } }
    val stacks: List<MutableList<Char>> = List(stackIndexes.size) { i ->
        rawStacks.dropLast(1).mapNotNull { level ->
            val stackValue = level[stackIndexes[i]]
            if (stackValue != ' ') stackValue else null
        }.toMutableList()
    }

    val moves = rawMoves.map { rawMove ->
        Regex("move (\\d+) from (\\d+) to (\\d+)")
            .findAll(rawMove).flatMap { it.groupValues }.toList().drop(1)
            .map { it.toInt() }
    }

    moves.forEach { (n, from, to) ->
        val pickedUp = stacks[from - 1].slice(0 until n)
        repeat(n) { stacks[from - 1].removeFirst() }
        stacks[to - 1].addAll(0, pickedUp)
    }

    return stacks.map { it[0] }.joinToString("")
}