import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import path from 'path';

import { part1, part2 } from './03';

describe('part 1', () => {
    it('calculates the right value for the example', () => {
        const raw_input = `
            467..114..
            ...*......
            ..35..633.
            ......#...
            617*......
            .....+.58.
            ..592.....
            ......755.
            ...$.*....
            .664.598..
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(4361);
    });

    it('calculates the right value for example 2', () => {
        const raw_input = `
            ........
            .24..4..
            ......*.
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(4);
    });


    it('calculates the right value for example 3', () => {
        const raw_input = `
            ........
            .24$-4..
            ......*.
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(28);
    });

    it('calculates the right value for example 4', () => {
        const raw_input = `
            11....11
            ..$..$..
            11....11
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(44);
    });

    it('calculates the right value for example 5', () => {
        const raw_input = `
            $......$
            .1....1.
            .1....1.
            $......$
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(4);
    });

    it('calculates the right value for example 6', () => {
        const raw_input = `
            $......$
            .11..11.
            .11..11.
            $......$
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(44);
    });

    it('calculates the right value for example 7', () => {
        const raw_input = `
            $11
            ...
            11$
            ...
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(22);
    });

    it('calculates the right value for example 8', () => {
        const raw_input = `
            $..
            .11
            .11
            $..
            ..$
            11.
            11.
            ..$
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(44);
    });

    it('calculates the right value for example 9', () => {
        const raw_input = `
            11.$.
        `
            .replace(/^\s+/gm, '');

        expect(part1(raw_input)).toBe(0);
    });

    it('calculates the right value for the input', () => {
        const filePath = path.resolve(process.cwd(), 'static/inputs/03.txt');
        const raw_input = readFileSync(filePath, 'utf8');

        // 310231 low
        // 527438 high
        // 521724 not right
        expect(part1(raw_input)).toBe(521515);
    });
});

describe('part 2', () => {
    it('calculates the right value for the example', () => {
        const raw_input = `
            467..114..
            ...*......
            ..35..633.
            ......#...
            617*......
            .....+.58.
            ..592.....
            ......755.
            ...$.*....
            .664.598..
        `
            .replace(/^\s+/gm, '');

        expect(part2(raw_input)).toBe(467835);
    });

    it('calculates the right value for the input', () => {
        const filePath = path.resolve(process.cwd(), 'static', 'inputs', '03.txt');
        const raw_input = readFileSync(filePath, 'utf8');

        expect(part2(raw_input)).toBe(69527306);
    });
});
