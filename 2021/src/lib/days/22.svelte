<script>
    function parse(raw_input) {
        const raw_steps = raw_input.split('\n').filter(l => l !== '');
        return raw_steps.map(s => {
            const groups = s.match(/(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)/).slice(1);
            const [action, ...raw_dimensions] = groups;
            const [min_x, max_x, min_y, max_y, min_z, max_z] = raw_dimensions.map(Number);
            return { action, cuboid : { min_x, max_x, min_y, max_y, min_z, max_z } };
        });
    }

    function part1(steps) {
        const enabled_cubes = new Set();

        steps.forEach(({ action, cuboid }) => {
            cubes(cuboid).filter(cube => cube.every(dimension => -50 <= dimension && dimension <= 50)).forEach(cube => {
                if (action === 'on') {
                    enabled_cubes.add(JSON.stringify(cube));
                } else {
                    enabled_cubes.delete(JSON.stringify(cube));
                }
            });
        });

        return enabled_cubes.size;
    }

    function part2(input) {
        return 42;
    }

    function cubes({ min_x, max_x, min_y, max_y, min_z, max_z }) {
        const points = [];
        for (let x = min_x; x <= max_x; x++) {
            for (let y = min_y; y <= max_y; y++) {
                for (let z = min_z; z <= max_z; z++) {
                    points.push([x, y, z]);
                }
            }
        }
        return points;
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
