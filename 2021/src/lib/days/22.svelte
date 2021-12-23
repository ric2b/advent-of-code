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
        const enabled_cubes = new Array(100*100*100).fill(false);

        steps.forEach(({ action, cuboid }, i) => {
            console.log(`step ${i}`);

            const { min_x, max_x, min_y, max_y, min_z, max_z } = cuboid;
            if([min_x, max_x, min_y, max_y, min_z, max_z].some(d => d < -50 || d > 50)) return;

            cubes(cuboid).forEach(([x, y, z]) => {
                enabled_cubes[(x + 50) + (y + 50) * 100 + (z + 50) * 100 * 100] = action === 'on';
            });
        });

        return enabled_cubes.filter(on => on === true).length;
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
