import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './10';

describe('part 1', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			.....
			.S-7.
			.|.|.
			.L-J.
			.....
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(4);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			..F7.
			.FJ|.
			SJ.L7
			|F--J
			LJ...
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(8);
	});

	it('calculates the right value for example 3', () => {
		const raw_input = `
			L-J
			7S7
			LJL
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(3);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/10.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(6864);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			...........
			.S-------7.
			.|F-----7|.
			.||.....||.
			.||.....||.
			.|L-7.F-J|.
			.|..|.|..|.
			.L--J.L--J.
			...........
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(4);
	});

	it('calculates the right value for example 2', () => {
		const raw_input = `
			..........
			.S------7.
			.|F----7|.
			.||....||.
			.||....||.
			.|L-7F-J|.
			.|..||..|.
			.L--JL--J.
			..........
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(4);
	});

	it('calculates the right value for example 3', () => {
		const raw_input = `
			.F----7F7F7F7F-7....
			.|F--7||||||||FJ....
			.||.FJ||||||||L7....
			FJL7L7LJLJ||LJ.L-7..
			L--J.L7...LJS7F-7L7.
			....F-J..F7FJ|L7L7L7
			....L7.F7||L7|.L7L7|
			.....|FJLJ|FJ|F7|.LJ
			....FJL-7.||.||||...
			....L---J.LJ.LJLJ...
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(8);
	});

	it('calculates the right value for example 4', () => {
		const raw_input = `
			FF7FSF7F7F7F7F7F---7
			L|LJ||||||||||||F--J
			FL-7LJLJ||||||LJL-77
			F--JF--7||LJLJ7F7FJ-
			L---JF-JLJ.||-FJLJJ7
			|F|F-JF---7F7-L7L|7|
			|FFJF7L7F-JF7|JL---7
			7-L-JL7||F7|L7F-7F7|
			L.L7LFJ|||||FJL7||LJ
			L7JLJL-JLJLJL--JLJ.L
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(10);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '10.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(349);
	});
});
