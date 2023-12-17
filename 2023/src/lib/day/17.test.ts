import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './17';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
			2413432311323
			3215453535623
			3255245654254
			3446585845452
			4546657867536
			1438598798454
			4457876987766
			3637877979653
			4654967986887
			4564679986453
			1224686865563
			2546548887735
			4322674655533
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(102);
	});

	it('calculates the right value for custom example 1a', () => {
		const raw_input = `
			241
			311
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(5);
	});

	it('calculates the right value for custom example 1b', () => {
		const raw_input = `
			2413
			3215
			3255
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(16);
	});

	it('calculates the right value for custom example 1c', () => {
		const raw_input = `
			24134
			32154
			32552
			34465
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(22);
	});

	it('calculates the right value for custom example 1d', () => {
		const raw_input = `
			241343
			321545
			325524
			344658
			454665
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(33);
	});

	it('calculates the right value for custom example 1e', () => {
		const raw_input = `
			2413432
			3215453
			3255245
			3446585
			4546657
			1438598
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(46);
	});

	it('calculates the right value for custom example 1f', () => {
		const raw_input = `
			24134323113
			32154535356
			32552456542
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(43);
	});

	it('calculates the right value for custom example 2', () => {
		const raw_input = `
			11111
			29991
			21111
			11111
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(9);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/17.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		// 1015 too high
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
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '17.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(7635);
	});
});
