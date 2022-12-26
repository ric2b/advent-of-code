import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test

class Day22KtTest {
    private val testInput =
        """
                    ...#
                    .#..
                    #...
                    ....
            ...#.......#
            ........#...
            ..#....#....
            ..........#.
                    ...#....
                    .....#..
                    .#......
                    ......#.
            
            10R5L5R10L4R5L5
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day22.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(6032, day22.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(191010, day22.part1(myInput))

    @Disabled("no patience")
    @Test
    fun `test part 2 with example`() = assertEquals(5031, day22.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(55364, day22.part2(myInput))
}
