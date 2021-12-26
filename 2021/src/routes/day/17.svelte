<script context="module">
    export async function load({ stuff }) { return { props: stuff }; }
</script>

<script>
    function parse(raw_input) {
        const line = raw_input.split('\n').filter(l => l !== '')[0];
        const raw_points = line.match(/x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)/).slice(1);
        const [x1, x2, y1, y2] = raw_points.map(Number);
        return { x1, x2, y1, y2 };
    }

    function part1({ x1, x2, y1, y2 }) {
        let highest_y = -Infinity;
        let best_init_velocity;

        for (let i_dx = 1; i_dx <= x2; i_dx++) {
            for (let i_dy = 1; i_dy <= Math.max(Math.abs(y1), Math.abs(y2)); i_dy++) {
                let x = 0, y = 0, dx = i_dx, dy = i_dy, hit = false, current_highest_y = -Infinity;

                while ((x < x2 && (dy > 0 || y > y1))) {
                    x += dx;
                    y += dy;
                    dx -= Math.sign(dx);
                    dy -= 1;

                    hit = x1 <= x && x <= x2 && y1 <= y && y <= y2;
                    current_highest_y = Math.max(y, current_highest_y);

                    if (hit && current_highest_y > highest_y) {
                        highest_y = current_highest_y;
                        best_init_velocity = { i_dx, i_dy };
                    }
                }
            }
        }

        return highest_y;
    }

    function part2({ x1, x2, y1, y2 }) {
        const valid_init_velocities = [];

        const abs_y = Math.max(Math.abs(y1), Math.abs(y2));

        for (let i_dx = 1; i_dx <= x2; i_dx++) {
            for (let i_dy = -abs_y; i_dy <= abs_y; i_dy++) {
                let x = 0, y = 0, dx = i_dx, dy = i_dy;

                while ((x < x2 && (dy > 0 || y > y1))) {
                    x += dx;
                    y += dy;
                    dx -= Math.sign(dx);
                    dy -= 1;

                    if(x1 <= x && x <= x2 && y1 <= y && y <= y2) {
                        valid_init_velocities.push({ i_dx, i_dy });
                        break;
                    }
                }
            }
        }

        return valid_init_velocities.length;
    }

    export let raw_input;
    export let set_part1_result;
    export let set_part2_result;

    $: input = parse($raw_input);
    $: set_part1_result(part1(input));
    $: set_part2_result(part2(input));
</script>
