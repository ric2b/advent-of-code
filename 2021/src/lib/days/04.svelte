<script>
    const board_width = 5, board_height = 5;

    function parse(raw_input) {
        const lines = raw_input.split('\n');
        const draws = lines[0].split(',').map(Number);
        const board_lines = lines.slice(1).filter(l => l !== '');

        const raw_boards = Array(board_lines.length / board_height).fill().map((e, i) => {
            return board_lines.slice(i * board_height, (i+1) * board_height);
        })

        const boards = raw_boards.map(b => b.map(l => l.trimLeft().split(/\s+/).map(Number)));
        return { draws, boards }
    }

    function part1({draws, boards}) {
        for (let i = 0; i < draws.length; i++) {
            const draw_set = new Set(draws.slice(0, i+1));
            const winning_board = boards.find(board => is_bingo(draw_set, board))
            if (winning_board) {
                return draws[i] * missing_numbers(draw_set, winning_board).reduce((a, b) => a + b);
            }
        }
    }

    function part2({draws, boards}) {
        for (let i = draws.length; i >= 0; i--) {
            const draw_set = new Set(draws.slice(0, i));
            const last_winning_board = boards.find(board => !is_bingo(draw_set, board))
            if (last_winning_board) {
                draw_set.add(draws[i])
                return draws[i] * missing_numbers(draw_set, last_winning_board).reduce((a, b) => a + b);
            }
        }
    }

    const is_bingo = (draws, board) => {
        const line_bingo = board.some(line => line.every(n => draws.has(n)));
        const column_bingo = transpose(board).some(column => column.every(n => draws.has(n)));
        return line_bingo || column_bingo;
    }

    const missing_numbers = (draws, board) => board.flatMap(line => line.filter(n => !draws.has(n)));
    const transpose = m => m[0].map((x,i) => m.map(x => x[i]));

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
	$: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
