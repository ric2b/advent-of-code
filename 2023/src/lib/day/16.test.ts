import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './16';

describe('part 1', () => {
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

		expect(part1(raw_input)).toBe(46);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/16.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(7307);
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

		expect(part2(raw_input)).toBe(51);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '16.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(7635);
	});
});
