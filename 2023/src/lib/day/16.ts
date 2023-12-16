export function part1(raw_input: string): number {
	const grid = raw_input
		.trim()
		.split('\n')
		.map((line) => line.split(''));

	const initial_beam = new Beam(new Vector(-1, 0), new Vector(1, 0));

	return bounce_beam(grid, initial_beam).size;
}

export function part2(raw_input: string): number {
	const grid = raw_input
		.trim()
		.split('\n')
		.map((line) => line.split(''));

	const height = grid.length;
	const width = grid[0].length;

	const initial_beams = [];
	for (let row = 0; row < height; row++) {
		initial_beams.push(new Beam(new Vector(-1, row), new Vector(1, 0)));
		initial_beams.push(new Beam(new Vector(width, row), new Vector(-1, 0)));
	}

	for (let col = 0; col < width; col++) {
		initial_beams.push(new Beam(new Vector(col, -1), new Vector(0, 1)));
		initial_beams.push(new Beam(new Vector(col, height), new Vector(0, -1)));
	}

	return Math.max(...initial_beams.map((beam) => bounce_beam(grid, beam).size));
}

export function part_1_grid(raw_input: string): string[] {
	const grid = raw_input
		.trim()
		.split('\n')
		.map((line) => line.split(''));

	const initial_beam = new Beam(new Vector(-1, 0), new Vector(1, 0));
	bounce_beam(grid, initial_beam).forEach((location) => {
		const [x, y] = location.split(',').map(Number);
		grid[y][x] = '*';
	});

	return grid.map((row) => row.map((c) => (c == '*' ? c : ' ')).join(''));
}

function bounce_beam(grid: string[][], initial_beam: Beam): Set<string> {
	const beams = [initial_beam];
	const energized_locations: Set<string> = new Set();
	const beam_cache: Set<string> = new Set();

	while (beams.length > 0) {
		const beam: Beam = beams.pop();
		const new_location = beam.move();
		const location_type: string | undefined = grid[new_location.y]?.[new_location.x];
		if (location_type != undefined) {
			const beam_key = beam.key();
			if (!beam_cache.has(beam_key)) {
				beam_cache.add(beam_key);
				energized_locations.add(new_location.key());
				const new_beams = beam.interact_with(location_type);
				beams.push(...new_beams);
			}
		}
	}

	return energized_locations;
}

class Vector {
	public readonly x: number;
	public readonly y: number;

	constructor(x: number, y: number) {
		this.x = x;
		this.y = y;
	}

	add(other: Vector): Vector {
		return new Vector(this.x + other.x, this.y + other.y);
	}

	key(): string {
		return `${this.x},${this.y}`;
	}
}

class Beam {
	public location: Vector;
	public direction: Vector;

	constructor(location: Vector, direction: Vector) {
		this.location = location;
		this.direction = direction;
	}

	key(): string {
		return `${this.location.key()};${this.direction.key()}`;
	}

	move(): Vector {
		this.location = this.location.add(this.direction);
		return this.location;
	}

	interact_with(location_type: string): Beam[] {
		const beams: Beam[] = [this];
		switch (location_type) {
			case '.':
				break;
			case '\\':
				// 0, -1 -> -1, 0
				// 0, 1 -> 1, 0
				// -1, 0 -> 0, -1
				// 1, 0 -> 0, 1
				this.direction = new Vector(this.direction.y, this.direction.x);
				break;
			case '/':
				// 0, -1 -> 1, 0
				// 0, 1 -> -1, 0
				// -1, 0 -> 0, 1
				// 1, 0 -> 0, -1
				this.direction = new Vector(-this.direction.y, -this.direction.x);
				break;
			case '-':
				if (this.direction.y != 0) {
					this.direction = new Vector(-1, 0);
					beams.push(new Beam(this.location, new Vector(1, 0)));
				}
				break;
			case '|':
				if (this.direction.x != 0) {
					this.direction = new Vector(0, -1);
					beams.push(new Beam(this.location, new Vector(0, 1)));
				}
				break;
		}
		return beams;
	}
}
