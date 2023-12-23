import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import {part1, part1graph, part2} from './23';

describe('part 1', () => {
	it('calculates the right value for example', () => {
		const raw_input = `
			#.#####################
			#.......#########...###
			#######.#########.#.###
			###.....#.>.>.###.#.###
			###v#####.#v#.###.#.###
			###.>...#.#.#.....#...#
			###v###.#.#.#########.#
			###...#.#.#.......#...#
			#####.#.#.#######.#.###
			#.....#.#.#.......#...#
			#.#####.#.#.#########v#
			#.#...#...#...###...>.#
			#.#.#v#######v###.###v#
			#...#.>.#...>.>.#.###.#
			#####v#.#.###v#.#.###.#
			#.....#...#...#.#.#...#
			#.#########.###.#.#.###
			#...###...#...#...#.###
			###.###.#.###v#####v###
			#...#...#.#.>.>.#.>.###
			#.###.###.#.###.#.#v###
			#.....###...###...#...#
			#####################.#
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(94);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/23.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(2130);
	});
});

describe('part 1 graph', () => {
	it('calculates the right value for example', () => {
		const raw_input = `
			#.#####################
			#.......#########...###
			#######.#########.#.###
			###.....#.>.>.###.#.###
			###v#####.#v#.###.#.###
			###.>...#.#.#.....#...#
			###v###.#.#.#########.#
			###...#.#.#.......#...#
			#####.#.#.#######.#.###
			#.....#.#.#.......#...#
			#.#####.#.#.#########v#
			#.#...#...#...###...>.#
			#.#.#v#######v###.###v#
			#...#.>.#...>.>.#.###.#
			#####v#.#.###v#.#.###.#
			#.....#...#...#.#.#...#
			#.#########.###.#.#.###
			#...###...#...#...#.###
			###.###.#.###v#####v###
			#...#...#.#.>.>.#.>.###
			#.###.###.#.###.#.#v###
			#.....###...###...#...#
			#####################.#
        `.replace(/^[ \t]+/gm, '');

		expect(part1graph(raw_input)).toBe(94);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/23.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1graph(raw_input)).toBe(2130);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			#.#####################
			#.......#########...###
			#######.#########.#.###
			###.....#.>.>.###.#.###
			###v#####.#v#.###.#.###
			###.>...#.#.#.....#...#
			###v###.#.#.#########.#
			###...#.#.#.......#...#
			#####.#.#.#######.#.###
			#.....#.#.#.......#...#
			#.#####.#.#.#########v#
			#.#...#...#...###...>.#
			#.#.#v#######v###.###v#
			#...#.>.#...>.>.#.###.#
			#####v#.#.###v#.#.###.#
			#.....#...#...#.#.#...#
			#.#########.###.#.#.###
			#...###...#...#...#.###
			###.###.#.###v#####v###
			#...#...#.#.>.>.#.>.###
			#.###.###.#.###.#.#v###
			#.....###...###...#...#
			#####################.#
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(154);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			#.#######
			#.......#
			#######.#
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(8);
	});

	it('calculates the right value for example 3', () => {
		const raw_input = `
			#.#####################
			#.......#########...###
			#######.#########.#.###
			###.....#.>.>.###.#.###
			###v#####.#v#.###.#.###
			###.>.....#.#.....#...#
			#####################.#
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(46);
	});

	it('calculates the right value for example 4', () => {
		const raw_input = `
			#.#######
			#.......#
			#.#####.#
			#.......#
			#######.#
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(10);
	});

	it('calculates the right value for example 5', () => {
		const raw_input = `
			#.#########
			#.........#
			#.#####.#.#
			#.........#
			#########.#
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(16);
	});

	it('calculates the right value for example 6', () => {
		const raw_input = `
			#.#########
			#.........#
			#.#.#.#.#.#
			#.........#
			#########.#
		`.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(20);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '23.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		// 5018 too low
		expect(part2(raw_input)).toBe(6710);
	});
});
