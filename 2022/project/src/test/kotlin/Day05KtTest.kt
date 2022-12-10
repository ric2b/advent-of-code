import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day05KtTest {
    private val testInput =
        """
                [D]    
            [N] [C]    
            [Z] [M] [P]
             1   2   3 
            
            move 1 from 2 to 1
            move 3 from 1 to 3
            move 2 from 2 to 1
            move 1 from 1 to 2
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day05.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals("CMZ", day05.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals("GFTNRBZPF", day05.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals("MCD", day05.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals("VRQWPDSGP", day05.part2(myInput))
}