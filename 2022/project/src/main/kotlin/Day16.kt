package day16

import kotlin.math.max

typealias ValveId = String
data class Valve(val id: ValveId, val flowRate: Long, val connectedTo: List<ValveId>) {
    var isOpen: Boolean = false
}

val moveToValues: MutableMap<Triple<Valve, Set<Valve>, Int>, Long> = mutableMapOf()

fun moveToValue(graph: Map<ValveId, Valve>, currentValve: Valve, openValves: Set<Valve>, currentMinute: Int): Long {
    return moveToValues.getOrPut(Triple(currentValve, openValves, currentMinute)) {
        if (currentMinute >= 30) return@getOrPut 0

        val pressureReleasedThisMinute = openValves.sumOf { it.flowRate }
        val valueOfMoving = currentValve.connectedTo.maxOf { moveToValue(graph, graph.getValue(it), openValves, currentMinute + 1) }

        if (currentValve in openValves || currentValve.flowRate == 0L) {
           return@getOrPut pressureReleasedThisMinute + valueOfMoving
        }

        val valueOfOpening = moveToValue(graph, currentValve, openValves = openValves + currentValve, currentMinute + 1)

        return@getOrPut pressureReleasedThisMinute + max(valueOfMoving, valueOfOpening)
    }
}

fun part1(input: String): Long {
    val graph: Map<ValveId, Valve> = input.trimEnd().lines().associate { line ->
        Regex("Valve (?<valve>\\w+) has flow rate=(\\d+); tunnel[s]? lead[s]? to valve[s]? ([\\w,\\s]+)+")
            .findAll(line).first().let {
                val (_, valveId, flowRate, rawConnectedValves) = it.groupValues
                valveId to Valve(valveId, flowRate.toLong(), rawConnectedValves.split(", "))
            }
    }

    var current = graph.getValue("AA")
    var releasedPressure = 0L

    for (minute in 1..30) {
        println("== Minute $minute ==")

        val openValves = graph.values.filter { it.isOpen }.toSet()
        val releasedThisMinute = openValves.sumOf { it.flowRate }
        releasedPressure += releasedThisMinute

        println("Valves ${openValves.map { it.id }} are open, releasing $releasedThisMinute pressure")

        if (graph.values.any { !it.isOpen && it.flowRate > 0 }) {
            val neighbours = current.connectedTo.map { graph.getValue(it) }

            val bestNeighbor = neighbours.maxByOrNull { moveToValue(graph, it, openValves, minute ) }!!

            val valueOfMoving =  moveToValue(graph, bestNeighbor, openValves, minute)
            val valueOfOpening = moveToValue(graph, current, openValves = openValves + current, minute)

            if (current.isOpen || valueOfMoving > valueOfOpening) {
                println("You move to valve ${bestNeighbor.id}.")
                current = bestNeighbor
            } else {
                println("You open valve ${current.id}.")
                current.isOpen = true
            }
        }
    }

    return releasedPressure
}

typealias Locations = List<Valve>
typealias LocationsId = Set<Valve>
val movesWithElephant: MutableMap<Triple<LocationsId, Set<Valve>, Int>, Long> = mutableMapOf()
fun moveWithElephant(graph: Map<ValveId, Valve>, currentLocations: Locations, openValves: Set<Valve>, currentMinute: Int): Long {
    return movesWithElephant.getOrPut(Triple(currentLocations.toSet(), openValves, currentMinute)) {
        if (currentMinute >= 30) return@getOrPut 0
        if (graph.values.all { it.isOpen || it.flowRate == 0L }) return@getOrPut (30 - currentMinute) * openValves.sumOf { it.flowRate }

        val (yourLocation: Valve, elephantLocation: Valve) = currentLocations
        val (yourMoves: List<Valve>, elephantMoves: List<Valve>) = currentLocations.map { location -> location.connectedTo.map { graph.getValue(it) } + location }
        val moveCombinations: List<Pair<Valve, Valve>> = yourMoves.flatMap { yourMove -> elephantMoves.map { elephantMove -> Pair(yourMove, elephantMove) } }

        moveCombinations.maxOf { (yourMove: Valve, elephantMove: Valve) ->
            val newOpenValves: MutableSet<Valve> = openValves.toMutableSet()
            if (yourLocation == yourMove) newOpenValves.add(yourMove)
            if (elephantLocation == elephantMove) newOpenValves.add(elephantMove)

            val pressureReleasedThisMinute = openValves.sumOf { it.flowRate }
            pressureReleasedThisMinute + moveWithElephant(graph, listOf(yourMove, elephantMove), newOpenValves, currentMinute + 1)
        }
    }
}

fun part2(input: String): Long {
    val graph: Map<ValveId, Valve> = input.trimEnd().lines().associate { line ->
        Regex("Valve (?<valve>\\w+) has flow rate=(\\d+); tunnel[s]? lead[s]? to valve[s]? ([\\w,\\s]+)+")
            .findAll(line).first().let {
                val (_, valveId, flowRate, rawConnectedValves) = it.groupValues
                valveId to Valve(valveId, flowRate.toLong(), rawConnectedValves.split(", "))
            }
    }

    val locations: MutableList<Valve> = mutableListOf(graph.getValue("AA"), graph.getValue("AA"))
    var releasedPressure = 0L

    for (minute in 1..30) {
        println("== Minute $minute ==")

        if (minute < 5) {
            println("Teaching the elefant")
            continue
        }

        val openValves = graph.values.filter { it.isOpen }.toSet()
        val releasedThisMinute = openValves.sumOf { it.flowRate }
        releasedPressure += releasedThisMinute

        println("Valves ${openValves.map { it.id }} are open, releasing $releasedThisMinute pressure")

        if (graph.values.any { !it.isOpen && it.flowRate > 0 }) {
            val (yourLocation, elephantLocation) = locations

            val (yourMoves, elephantMoves) = locations.map { location -> location.connectedTo.map { graph.getValue(it) } + location }
            val moveCombinations: List<Pair<Valve, Valve>> = yourMoves.flatMap { yourMove -> elephantMoves.map { elephantMove -> Pair(yourMove, elephantMove) } }

            val (yourMove, elephantMove) = moveCombinations.maxByOrNull { (yourMove, elephantMove) ->
                val newOpenValves: MutableSet<Valve> = openValves.toMutableSet()
                if (yourLocation == yourMove) newOpenValves.add(yourMove)
                if (elephantLocation == elephantMove) newOpenValves.add(elephantMove)

                moveWithElephant(graph, listOf(yourMove, elephantMove), newOpenValves, minute)
            }!!

            if (yourLocation != yourMove) {
                println("You move to valve ${yourMove.id}.")
                locations[0] = yourMove
            } else {
                println("You open valve ${yourLocation.id}.")
                yourLocation.isOpen = true
            }

            if (elephantLocation != elephantMove) {
                println("The elephant moves to valve ${elephantMove.id}.")
                locations[1] = elephantMove
            } else {
                println("The elephant opens valve ${elephantLocation.id}.")
                elephantLocation.isOpen = true
            }
        }
    }

    return releasedPressure
}