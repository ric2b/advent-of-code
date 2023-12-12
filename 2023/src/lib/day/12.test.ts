import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './12';

describe('part 1', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			???.### 1,1,3
			.??..??...?##. 1,1,3
			?#?#?#?#?#?#?#? 1,3,1,6
			????.#...#... 4,1,1
			????.######..#####. 1,6,5
			?###???????? 3,2,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(21);
	});

	it('calculates the right value for example 1.1', () => {
		const raw_input = `
			???.### 1,1,3
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.1b', () => {
		const raw_input = `
			?? 2
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.1c', () => {
		const raw_input = `
			?? 1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(2);
	});

	it('calculates the right value for example 1.1cd', () => {
		const raw_input = `
			?? 1,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(0);
	});

	it('calculates the right value for example 1.1d', () => {
		const raw_input = `
			??? 1,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.1de', () => {
		const raw_input = `
			#?# 1,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.1e', () => {
		const raw_input = `
			?.? 1,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.1f', () => {
		const raw_input = `
			.?. 1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.2', () => {
		const raw_input = `
			.??..??...?##. 1,1,3
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(4);
	});

	it('calculates the right value for example 1.3', () => {
		const raw_input = `
			?#?#?#?#?#?#?#? 1,3,1,6
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.3b', () => {
		const raw_input = `
			?#?#?# 1,1,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.3c', () => {
		const raw_input = `
			?#?##?# 1,2,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.4', () => {
		const raw_input = `
			????.#...#... 4,1,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 1.5', () => {
		const raw_input = `
			????.######..#####. 1,6,5
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(4);
	});

	it('calculates the right value for example 1.6', () => {
		const raw_input = `
			?###???????? 3,2,1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(10);
	});


	it('calculates the right value for example 2', () => {
		const raw_input = `
			. 1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(0);
	});

	it('calculates the right value for example 3', () => {
		const raw_input = `
			# 1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 4', () => {
		const raw_input = `
			.# 1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 5', () => {
		const raw_input = `
			#. 1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 6', () => {
		const raw_input = `
			## 2
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 7', () => {
		const raw_input = `
			.## 2
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 8', () => {
		const raw_input = `
			##. 2
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for example 9', () => {
		const raw_input = `
			? 1
        `.replace(/^\t+/gm, '');

		expect(part1(raw_input)).toBe(1);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static/inputs/12.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part1(raw_input)).toBe(7916);
	});
});

describe('part 2', () => {
	it('calculates the right value for example 1', () => {
		const raw_input = `
			???.### 1,1,3
			.??..??...?##. 1,1,3
			?#?#?#?#?#?#?#? 1,3,1,6
			????.#...#... 4,1,1
			????.######..#####. 1,6,5
			?###???????? 3,2,1
        `.replace(/^\t+/gm, '');

		expect(part2(raw_input)).toBe(525152);
	});

	it('calculates the right value for the input', () => {
		const filePath = path.resolve(process.cwd(), 'static', 'inputs', '12.txt');
		const raw_input = readFileSync(filePath, 'utf8');

		expect(part2(raw_input)).toBe(37366887898686);
	});
});
