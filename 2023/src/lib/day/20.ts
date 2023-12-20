export function part1(raw_input: string): number {
	const modules: Map<string, Module> = parse_modules(raw_input);

	const button: Button = new Button(modules.get('broadcaster'));

	// for (let i = 0; i < 1; i++) {
	for (let i = 0; i < 1000; i++) {
		let queue: string[] = button.pulse();
		while (queue.length > 0) {
			queue = queue.flatMap(module_id => modules.get(module_id)?.pulse());
		}
	}

	const low_pulse_count = [...modules.values()].reduce((low_pulses, module) => low_pulses + module.low_pulses, 0)
	const high_pulse_count = [...modules.values()].reduce((high_pulses, module) => high_pulses + module.high_pulses, 0)

	return low_pulse_count * high_pulse_count;
}

export function part2(raw_input: string): number {
	return 2;
}

function parse_modules(raw_input: string): Map<string, Module> {
	const modules: Map<string, Module> = new Map();

	raw_input.trim().split('\n').forEach(line => {
		const [raw_source, raw_sinks] = line.split(' -> ');

		const [, module_type, module_id] = /(?<type>broadcaster|%|&)(?<id>\w+)?/.exec(raw_source);
		const sinks = raw_sinks.split(', ');

		const module = ((module_type) => {
			switch (module_type) {
				case 'broadcaster':
					return new Broadcaster(modules, sinks);
				case '%':
					return new FlipFlop(modules, module_id, sinks);
				case '&':
					return new NAnd(modules, module_id, sinks);
			}
		})(module_type);

		module_type == 'broadcaster' ? modules.set('broadcaster', module) : modules.set(module_id, module);
	})

	modules.forEach((module, module_id) => {
		module.sinks.forEach(sink_id => {
			if (!modules.has(sink_id)) {
				modules.set(sink_id, new Output(modules, sink_id))
			}

			modules.get(sink_id)?.sources.push(module_id);
		})
	})

	return modules;
}

class Button {
	private readonly broadcaster_module: Module;
	constructor(broadcaster_module: Module) {
		this.broadcaster_module = broadcaster_module;
	}

	pulse() {
		console.log(`button signaling broadcaster with false`);
		this.broadcaster_module.signal('button', false);
		return ['broadcaster'];
	}
}

class Module {
	public readonly modules: Map<string, Module>;
	public readonly module_id: string;

	public readonly sources: string[];
	public readonly sinks: string[];

	public signals: [string, boolean][];

	public low_pulses: number = 0;
	public high_pulses: number = 0;

	constructor(modules: Map<string, Module>, module_id: string, sinks: string[] = []) {
		this.modules = modules;
		this.module_id = module_id;
		this.signals = [];
		this.sources = [];
		this.sinks = sinks;
	}

	signal(module_id: string, value: boolean) {
		value ? this.high_pulses++ : this.low_pulses++;
		this.signals.push([module_id, value]);
	}

	generate_pulse(): boolean | undefined {
		throw new Error('Not implemented');
	}

	pulse(): string[] {
		const value = this.generate_pulse();
		if (value === undefined) {
			return [];
		}

		this.sinks.forEach(sink => {
			console.log(`${this.module_id} signaling ${sink} with ${value}`);
			this.modules.get(sink)?.signal(this.module_id, value);
		});

		return this.sinks;
	}
}

class Broadcaster extends Module {
	constructor(modules: Map<string, Module>, sinks: string[] = []) {
		super(modules, 'broadcaster', sinks);
	}

	generate_pulse(): boolean | undefined {
		if (this.signals.length == 0) {
			return undefined;
		}

		const [module_id, signal] = this.signals.shift();
		return signal;
	}
}

class FlipFlop extends Module {
	public state: boolean = false;

	generate_pulse(): boolean | undefined {
		if (this.signals.length == 0) {
			return undefined;
		}

		const [module_id, signal] = this.signals.shift();

		if (signal === false) {
			this.state = !this.state;
			return this.state;
		}

		return undefined;
	}
}

class NAnd extends Module {
	public readonly last_signals_per_module: Map<string, boolean> = new Map();

	generate_pulse(): boolean | undefined {
		if (this.signals.length == 0) {
			return undefined;
		}

		const [module_id, signal] = this.signals.shift();
		this.last_signals_per_module.set(module_id, signal);

		this.sources.forEach(module_id => {
			if (!this.last_signals_per_module.has(module_id)) {
				this.last_signals_per_module.set(module_id, false);
			}
		});

		const value: boolean = ![...this.last_signals_per_module.values()].every((v: boolean) => v);

		return value;
	}
}

class Output extends Module {
	generate_pulse() {
		this.signals = []
		return undefined;
	}
}