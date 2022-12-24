import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Disabled
import org.junit.jupiter.api.Test

@Disabled
class Day24KtTest {
    private val testInput =
        """
            #.######
            #>>.<^<#
            #.<..<<#
            #>v.><>#
            #<^v^^>#
            ######.#
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day24.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(18, day24.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(314, day24.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(54, day24.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(896, day24.part2(myInput))
}
