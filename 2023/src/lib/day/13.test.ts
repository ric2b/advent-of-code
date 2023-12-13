import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './13';

describe('part 1', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
            #.##..##.
			..#.##.#.
			##......#
			##......#
			..#.##.#.
			..##..##.
			#.#.##.#.
			
			#...##..#
			#....#..#
			..##..###
			#####.##.
			#####.##.
			..##..###
			#....#..#
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(405);
	});

	it('calculates the right value for example 1a', () => {
		const raw_input = `
            #.##..##.
			..#.##.#.
			##......#
			##......#
			..#.##.#.
			..##..##.
			#.#.##.#.
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(5);
	});

	it('calculates the right value for example 1b', () => {
		const raw_input = `
			#...##..#
			#....#..#
			..##..###
			#####.##.
			#####.##.
			..##..###
			#....#..#
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(400);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			##.#.#####.....
			##...##.###.###
			##...##.###.###
			##.#.#####.....
			.##...#.#...#..
			##..#...#..##..
			###.######....#
			.##.######..###
			#...#...###....
			..#.....#.##...
			....#......#.##
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(200);
	});

	it('calculates the right value for example 3', () => {
		const raw_input = `
			###.#....#.
			#..########
			...##.##.##
			.#..#....#.
			.###..##..#
			.#.#.#..#.#
			.#.#.#..#.#
			.###..##..#
			.#..#....#.
			...##.##.##
			#..########
			#.#.#....#.
			##.#......#
			#.....##...
			##.##....##
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(7);
	});

	it('calculates the right value for example 4', () => {
		const raw_input = `
			..#####..#..#..##
			...##.####..####.
			##...#...####...#
			....###........##
			##.####.##..##.##
			##..####......###
			###..#..#.##.#..#
			...######....####
			##...#.#.#..#.#.#
			...####..####...#
			....#.#...##...#.
			###.###.#....#.##
			..#..#....##....#
			..####.#..##..#.#
			##..#............
        `.replace(/^[ \t]+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/13.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(28651);
	});
});

describe('part 2', () => {
	it('calculates the right value for the example', () => {
		const raw_input = `
            Time:      7  15   30
            Distance:  9  40  200
        `.replace(/^[ \t]+/gm, '');

		expect(part2(raw_input)).toBe(1);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '13.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(1);
	});
});
