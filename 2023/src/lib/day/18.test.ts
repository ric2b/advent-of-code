import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './18';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			R 6 (#70c710)
			D 5 (#0dc571)
			L 2 (#5713f0)
			D 2 (#d2c081)
			R 2 (#59c680)
			D 2 (#411b91)
			L 5 (#8ceee2)
			U 2 (#caa173)
			L 1 (#1b58a2)
			U 2 (#caa171)
			R 2 (#7807d2)
			U 3 (#a77fa3)
			L 2 (#015232)
			U 2 (#7a21e3)
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(62);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/18.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(39194);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			.|...\\....
			|.-.\\.....
			.....|-...
			........|.
			..........
			.........\\
			..../.\\\\..
			.-.-/..|..
			.|....-|.\\
			..//.|....
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(952408144115);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '18.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(7635);
	});
});
