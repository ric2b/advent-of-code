export function part1(raw_input: string, step_limit: number = 64): number {
	const bricks: Brick[] = raw_input.trim().split('\n').map((line, i) => {
		const [raw_start, raw_end] = line.split('~');
		const [start, end] = line.split('~').map(coords => {
			return new Location(...coords.split(',').map(Number));
		})
		return new Brick(start, end, i);
	});

	const settled_bricks = settle(bricks);

	const can_be_disintegrated = settled_bricks.filter(brick => can_disintegrate(settled_bricks, brick));

	return can_be_disintegrated.length;
}

export function part2(raw_input: string, step_limit: number = 64): number {
	return 2;
}

function can_disintegrate(settled_bricks: Brick[], brick: Brick): boolean {
	const bricks_on_next_layer = settled_bricks.filter(other => other.height_offset == brick.top());

	if (bricks_on_next_layer.length == 0) {
		return true;
	}

	const other_bricks_ending_on_same_layer = settled_bricks.filter(other => {
		return other.id != brick.id && other.top() == brick.top()
	});

	return bricks_on_next_layer.some(top_brick => {
		return other_bricks_ending_on_same_layer.some(support_brick => support_brick.overlap(top_brick));
	});
}

function maxBy<T>(array: Array<T>, fn: (fn: T) => number): T | undefined {
	if (array.length == 0) return undefined;
	return array.reduce((max, element) => fn(element) > fn(max) ? element : max);
}

function settle(falling_bricks: Brick[]): Brick[] {
	const bricks = falling_bricks.toSorted((a, b) => a.height_offset - b.height_offset);
	const max_x = bricks.flatMap(brick => [brick.start.x, brick.end.x]).reduce((a, b) => Math.max(a, b));
	const max_y = bricks.flatMap(brick => [brick.start.y, brick.end.y]).reduce((a, b) => Math.max(a, b));

	// Last level could be a sorted collection for more fast. might not be enough if each point only has a few bricks...
	const settled_xy_index: Brick[][][] = Array.from(Array(max_x + 1), () => Array.from(Array(max_y + 1), () => []));
	const settled_bricks: Brick[] = [];

	while (bricks.length > 0) {
		const current_brick = bricks.shift();
		let new_height_offset = 1;

		for (const x of current_brick.x_range) {
			for (const y of current_brick.y_range) {
				const upper_brick: Brick = maxBy(settled_xy_index[x][y], brick => brick.top())
				if (upper_brick != undefined) {
					new_height_offset = Math.max(new_height_offset, upper_brick.top());
				}

				settled_xy_index[x][y].push(current_brick);
			}
		}

		current_brick.height_offset = new_height_offset;
		settled_bricks.unshift(current_brick);
	}
	return settled_bricks;
}

class Brick {
	public readonly id: number;

	public readonly start: Location;
	public readonly end: Location;
	public readonly height: number;

	public readonly x_range: Range;
	public readonly y_range: Range;

	public height_offset: number;
	// public readonly key: string;

	constructor(start: Location, end: Location, id: string) {
		this.id = id;
		// start.z is always <= end.z in the example and the input
		this.height_offset = Math.min(start.z, end.z);
		this.height = 1 + Math.abs(end.z - start.z);

		this.start = start.with_z(start.z - this.height_offset);
		this.end = end.with_z(end.z - this.height_offset);

		this.x_range = new Range(this.start.x, this.end.x);
		this.y_range = new Range(this.start.y, this.end.y);
		// this.key = Location.build_key(start, end);
	}

	top(): number {
		return this.height_offset + this.height;
	}

	overlap(other: Brick): boolean {
		return this.x_range.overlap(other.x_range) || this.y_range.overlap(other.y_range);
	}

	volume(): number {
		return (1 + Math.abs(this.end.x - this.start.x))
			* (1 + Math.abs(this.end.y - this.start.y))
			* (1 + Math.abs(this.end.z - this.start.z));
	}
}

class Location {
	public readonly x: number;
	public readonly y: number;
	public readonly z: number;
	public readonly key: string;

	constructor(x: number, y: number, z: number) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.key = Location.build_key(x, y, z);
	}

	with_z(z: number): Location {
		return new Location(this.x, this.y, z);
	}

	static build_key(x: number, y: number, z: number): string {
		return `${x},${y},${z}`;
	}

	static from_key(key: string): Location {
		const [x, y, z] = key.split(',').map(Number)
		return new Location(x, y, z);
	}
}

class Range {
	public readonly start: number;
	public readonly end: number;

	constructor(start: number, end: number) {
		this.start = Math.min(start, end);
		this.end = Math.max(start, end);
	}

	overlap(other: Range): boolean {
		return this.start <= other.end && this.end >= other.start;
	}

	*iterator() {
		for (let i = this.start; i <= this.end; i++) {
			yield i;
		}
	}

	[Symbol.iterator]() {
		return this.iterator();
	}
}