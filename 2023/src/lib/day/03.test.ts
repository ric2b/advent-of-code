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
            Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
            Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
            Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
            Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
            Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
        `
            .replace(/^\s+/gm, '');

        expect(part2(raw_input)).toBe(2286);
    });

    it('calculates the right value for the input', () => {
        const filePath = path.resolve(process.cwd(), 'static', 'inputs', '03.txt');
        const raw_input = readFileSync(filePath, 'utf8');

        expect(part2(raw_input)).toBe(72706);
    });
});
