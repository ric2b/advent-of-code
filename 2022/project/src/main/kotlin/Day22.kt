package day22

data class Position(var x: Int, var y: Int, var direction: Int) {
    fun location(map: Array<CharArray>) = map.getOrNull(y)?.getOrNull(x)
}

val validLocations = listOf('.', '#')

fun part1(input: String): Int {
    val (rawMap, rawInstructions) = input.trimEnd().split("\n\n")
    val board: Array<CharArray> = rawMap.lines().map(String::toCharArray).toTypedArray()
    val instructions = Regex("\\d+|[LR]").findAll(rawInstructions).map { it.groupValues.first() }.map { it.toIntOrNull() ?: it }.toList()

    var position = board.first().indexOfFirst { it == '.' }.let { Position(it, 0, 0) }

    instructions.forEach { step ->
        when (step) {
            "R" -> position.direction = (position.direction + 1).mod(4)
            "L" -> position.direction = (position.direction - 1).mod(4)
            is Int -> for(delta in 1..step) {
                val previousPosition = position.copy()

                when(position.direction) {
                    0 -> {
                        position.x += 1
                        if (position.location(board) !in validLocations) {
                            position.x = board[position.y].indexOfFirst { it in validLocations }
                        }
                    }
                    1 -> {
                        position.y += 1
                        if (position.location(board) !in validLocations) {
                            position.y = board.indexOfFirst { it.getOrNull(position.x) in validLocations }
                        }
                    }
                    2 -> {
                        position.x -= 1
                        if (position.location(board) !in validLocations) {
                            position.x = board[position.y].indexOfLast { it in validLocations }
                        }
                    }
                    3 -> {
                        position.y -= 1
                        if (position.location(board) !in validLocations) {
                            position.y = board.indexOfLast { it.getOrNull(position.x) in validLocations }
                        }
                    }
                }

                if (position.location(board) == '#') {
                    position = previousPosition
                    break
                }
            }
            else -> throw IllegalArgumentException()
        }
    }

    return 1000 * (position.y + 1) + 4 * (position.x + 1) + position.direction
}

fun changeFace(position: Position): Position {
    val faceRow =  position.y / 50
    val faceColumn = position.x / 50
    val direction = listOf("right", "down", "left", "up")[position.direction]
    return when {
        // 1 to 2 - automatic
        // 1 to 3 - automatic
        // 1 to 4
        faceRow == 0 && direction == "left" -> Position(0, 149 - position.y,0)
        // 1 to 6
        faceColumn == 1 && direction == "up" -> Position(0, position.x + 100, 0)

        // 2 to 1 - automatic
        // 2 to 3
        faceColumn == 2 && direction == "down" -> Position(99, position.x - 50,2)
        // 2 to 5
        faceRow == 0 && direction == "right" -> Position(99, 149 - position.y, 2)
        // 2 to 6
        faceColumn == 2 && direction == "up" -> Position(position.x - 100, 199, 3)

        // 3 to 1 - automatic
        // 3 to 2
        faceRow == 1 && direction == "right" -> Position(position.y + 50, 49,3)
        // 3 to 4
        faceRow == 1 && direction == "left" -> Position(position.y - 50, 100,1)
        // 3 to 5 - automatic

        // 4 to 1
        faceRow == 2 && direction == "left" -> Position(50, 149 - position.y,0)
        // 4 to 3
        faceColumn == 0 && direction == "up" -> Position(50, position.x + 50,0)
        // 4 to 5 - automatic
        // 4 to 6 - automatic

        // 5 to 2
        faceRow == 2 && direction == "right" -> Position(149, 149 - position.y,2)
        // 5 to 3 - automatic
        // 5 to 4 - automatic
        // 5 to 6
        faceColumn == 1 && direction == "down" -> Position(49, position.x + 100,2)

        // 6 to 1
        faceRow == 3 && direction == "left" -> Position(position.y - 100, 0,1)
        // 6 to 2
        faceColumn == 0 && direction == "down" -> Position(position.x + 100, 0,1)
        // 6 to 4 - automatic
        // 6 to 5
        faceRow == 3 && direction == "right" -> Position(position.y - 100, 149,3)

        else -> throw IllegalArgumentException()
    }
}

fun part2(input: String): Int {
    val (rawMap, rawInstructions) = input.trimEnd().split("\n\n")
    val board: Array<CharArray> = rawMap.lines().map(String::toCharArray).toTypedArray()
    val instructions = Regex("\\d+|[LR]").findAll(rawInstructions).map { it.groupValues.first() }.map { it.toIntOrNull() ?: it }.toList()

    var position = board.first().indexOfFirst { it == '.' }.let { Position(it, 0, 0) }

    instructions.forEach { step ->
        when (step) {
            "R" -> position.direction = (position.direction + 1).mod(4)
            "L" -> position.direction = (position.direction - 1).mod(4)
            is Int -> for(delta in 1..step) {
                val previousPosition = position.copy()

                when(position.direction) {
                    0 -> position.x += 1
                    1 -> position.y += 1
                    2 -> position.x -= 1
                    3 -> position.y -= 1
                }

                if (position.location(board) !in validLocations) {
                    position = changeFace(position)
                }

                if (position.location(board) == '#') {
                    position = previousPosition
                    break
                }
            }
            else -> throw IllegalArgumentException()
        }
    }

    return 1000 * (position.y + 1) + 4 * (position.x + 1) + position.direction
}
