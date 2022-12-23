import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test

@Disabled
class Day23KtTest {
    private val testInput =
        """
            ....#..
            ..###.#
            #...#.#
            .#...##
            #.###..
            ##.#.##
            .#..#..
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day23.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(110, day23.part1(testInput))

    @Test
    fun `test part 1 with smaller example`() {
        val smallerExample =
            """
                .....
                ..##.
                ..#..
                .....
                ..##.
                .....
            """.trimIndent()
        assertEquals(25, day23.part1(smallerExample))
    }

    @Test
    fun `test part 1 with my input`() = assertEquals(3925, day23.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(20, day23.part2(testInput))

    @Disabled // TODO: too slow
    @Test
    fun `test part 2 with my input`() = assertEquals(903, day23.part2(myInput))
}
