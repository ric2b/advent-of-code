
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Day16KtTest {
    private val testInput =
        """
            Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
            Valve BB has flow rate=13; tunnels lead to valves CC, AA
            Valve CC has flow rate=2; tunnels lead to valves DD, BB
            Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
            Valve EE has flow rate=3; tunnels lead to valves FF, DD
            Valve FF has flow rate=0; tunnels lead to valves EE, GG
            Valve GG has flow rate=0; tunnels lead to valves FF, HH
            Valve HH has flow rate=22; tunnel leads to valve GG
            Valve II has flow rate=0; tunnels lead to valves AA, JJ
            Valve JJ has flow rate=21; tunnel leads to valve II
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day16.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(1651, day16.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(1775, day16.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(1707, day16.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(2351, day16.part2(myInput))
}
