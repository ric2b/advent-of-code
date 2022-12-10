import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class Day07KtTest {
    private val testInput =
        """
            $ cd /
            $ ls
            dir a
            14848514 b.txt
            8504156 c.dat
            dir d
            $ cd a
            $ ls
            dir e
            29116 f
            2557 g
            62596 h.lst
            $ cd e
            $ ls
            584 i
            $ cd ..
            $ cd ..
            $ cd d
            $ ls
            4060174 j
            8033020 d.log
            5626152 d.ext
            7214296 k
        """.trimIndent()
    val myInput = this::class.java.classLoader.getResource("day07.txt")!!.readText()

    @Test
    fun `test part 1 with example`() = assertEquals(95437, day07.part1(testInput))

    @Test
    fun `test part 1 with my input`() = assertEquals(1182909, day07.part1(myInput))

    @Test
    fun `test part 2 with example`() = assertEquals(24933642, day07.part2(testInput))

    @Test
    fun `test part 2 with my input`() = assertEquals(2832508, day07.part2(myInput))
}