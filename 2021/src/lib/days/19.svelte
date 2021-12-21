<script>
    function parse(raw_input) {
        const scanner_reports = raw_input.split('\n\n');
        return scanner_reports.map(s => s.split('\n').slice(1).filter(l => l !== '').map(b => b.split(',').map(Number)));
    }

    function part1(all_reports) {
        const { beacons } = locate_scanners_and_beacons(all_reports);
        return new Set(beacons.map(JSON.stringify)).size;
    }

    function part2(all_reports) {
        const { scanners } = locate_scanners_and_beacons(all_reports);

        let current_max = -Infinity;
        for (let i = 0; i < scanners.length; i++) {
            for (let j = i; j < scanners.length; j++) {
                current_max = Math.max(current_max, manhattan_distance(scanners[i], scanners[j]));
            }
        }

        return current_max;
    }

    // https://www.reddit.com/r/adventofcode/comments/rjpf7f/2021_day_19_solutions/hp8pkmb/
    // https://www.reddit.com/r/adventofcode/comments/rjpf7f/2021_day_19_solutions/hp8qck3/
    function locate_scanners_and_beacons(all_reports) {
        const beacons = Array.from(all_reports)[0];
        const remaining = Array.from(all_reports).slice(1);
        const scanners = new Set([JSON.stringify([0,0,0])]);

        console.log(`locating scanners`);
        while (remaining.length) {
            for (let i = 0; i < remaining.length; i++) {
                console.log(`scanner ${i} of ${remaining.length}`)
                for (const o of orientations) {
                    const counter = new Map();
                    for (const [x2, y2, z2] of remaining[i].map(r => transformation(o, r))) {
                        for (const [x1, y1, z1] of beacons) {
                            const translation_vector = [x1 - x2, y1 - y2, z1 - z2];
                            const s_t_vector = JSON.stringify(translation_vector);
                            counter.set(s_t_vector, (counter.get(s_t_vector) || 0) + 1);
                        }
                    }

                    const [v_max, c_max] = [...counter].reduce(([v_max, c_max], [v, c]) => c > c_max ? [v, c] : [v_max, c_max])
                    if (c_max >= 12) {
                        console.log(`found ${v_max}`);
                        const target = JSON.parse(v_max);
                        scanners.add(JSON.stringify(target));
                        beacons.push(...remaining[i].map(b => translation(target, transformation(o, b))));
                        remaining.splice(i, 1);
                        break;
                    }
                }
            }
        }

        return { scanners: [...scanners].map(JSON.parse), beacons };
    }

    function manhattan_distance([x1, y1, z1], [x2, y2, z2]) {
        return Math.abs(x1 - x2) + Math.abs(y1 - y2) + Math.abs(z1 - z2);
    }

    const orientations = [
            [[1, 0, 0], [0, 1, 0], [0, 0, 1]],
            [[-1, 0, 0], [0, -1, 0], [0, 0, 1]],
            [[-1, 0, 0], [0, 1, 0], [0, 0, -1]],
            [[1, 0, 0], [0, -1, 0], [0, 0, -1]],

            [[-1, 0, 0], [0, 0, 1], [0, 1, 0]],
            [[1, 0, 0], [0, 0, -1], [0, 1, 0]],
            [[1, 0, 0], [0, 0, 1], [0, -1, 0]],
            [[-1, 0, 0], [0, 0, -1], [0, -1, 0]],

            [[0, -1, 0], [1, 0, 0], [0, 0, 1]],
            [[0, 1, 0], [-1, 0, 0], [0, 0, 1]],
            [[0, 1, 0], [1, 0, 0], [0, 0, -1]],
            [[0, -1, 0], [-1, 0, 0], [0, 0, -1]],

            [[0, 1, 0], [0, 0, 1], [1, 0, 0]],
            [[0, -1, 0], [0, 0, -1], [1, 0, 0]],
            [[0, -1, 0], [0, 0, 1], [-1, 0, 0]],
            [[0, 1, 0], [0, 0, -1], [-1, 0, 0]],

            [[0, 0, 1], [1, 0, 0], [0, 1, 0]],
            [[0, 0, -1], [-1, 0, 0], [0, 1, 0]],
            [[0, 0, -1], [1, 0, 0], [0, -1, 0]],
            [[0, 0, 1], [-1, 0, 0], [0, -1, 0]],

            [[0, 0, -1], [0, 1, 0], [1, 0, 0]],
            [[0, 0, 1], [0, -1, 0], [1, 0, 0]],
            [[0, 0, 1], [0, 1, 0], [-1, 0, 0]],
            [[0, 0, -1], [0, -1, 0], [-1, 0, 0]],
        ];

    function translation([a, b, c], [x, y, z]) {
        return [x+a, y+b, z+c];
    }

    function transformation([[a, b, c],[d, e, f], [g, h, i]], [x, y, z]) {
        return [a*x + b*y + c*z, d*x + e*y + f*z, g*x + h*y + i*z];
    }

    export let raw_input;
    export let part1_result;
    export let part2_result;

    $: input = parse(raw_input);
    $: part1_result = part1(input);
    $: part2_result = part2(input);
</script>
