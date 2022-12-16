package day16

import kotlin.math.max

typealias ValveId = String
data class Valve(val id: ValveId, val flowRate: Long, val connectedTo: List<ValveId>, var isOpen: Boolean = false)

val moveToValues: MutableMap<Triple<Valve, Set<Valve>, Int>, Long> = mutableMapOf()

fun moveToValue(graph: Map<ValveId, Valve>, currentValve: Valve, openValves: Set<Valve>, currentMinute: Int): Long {
    return moveToValues.getOrPut(Triple(currentValve, openValves, currentMinute)) {
        if (currentMinute >= 30) return 0

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

fun part2(input: String): Long {
    throw NotImplementedError()
}