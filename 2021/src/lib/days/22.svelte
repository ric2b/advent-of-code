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

    function part2(steps) {
        let areas = new Map();

        for (const { action, cuboid } of steps) {
            const updated_areas = new Map();
            const s_cuboid = JSON.stringify(cuboid);

            if (action === 'on') updated_areas.set(s_cuboid, (updated_areas.get(s_cuboid) || 0) + 1);

            for (const [s_area, value] of areas) {
                const area = JSON.parse(s_area);
                if (intersects(cuboid, area)) {
                    const s_intersection_area = JSON.stringify(get_intersection_area(cuboid, area));
                    updated_areas.set(s_intersection_area, (updated_areas.get(s_intersection_area) || 0) - value);
                }
            }

            for (const [s_area, value] of updated_areas) { areas.set(s_area, (areas.get(s_area) || 0) + value); }
        }
        return [...areas].map(([s_area, value]) => get_area(JSON.parse(s_area)) * value).reduce((a, b) => a + b);
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

    function intersects(a, b) {
        return a.min_x <= b.max_x && a.max_x >= b.min_x
            && a.min_y <= b.max_y && a.max_y >= b.min_y
            && a.min_z <= b.max_z && a.max_z >= b.min_z;
    }

    function get_intersection_area(a, b) {
        return {
            min_x: Math.max(a.min_x, b.min_x),
            max_x: Math.min(a.max_x, b.max_x),
            min_y: Math.max(a.min_y, b.min_y),
            max_y: Math.min(a.max_y, b.max_y),
            min_z: Math.max(a.min_z, b.min_z),
            max_z: Math.min(a.max_z, b.max_z),
        };
    }

    function get_area({ min_x, max_x, min_y, max_y, min_z, max_z }) {
        return (1 + max_x - min_x) * (1 + max_y - min_y) * (1 + max_z - min_z);
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
