// px{a<2006:qkq,m>2090:A,rfg}
// pv{a>1716:R,A}
// lnx{m>1548:A,A}
// rfg{s<537:gd,x>2440:R,A}
// qs{s>3448:A,lnx}
// qkq{x<1416:A,crn}
// crn{x>2662:A,R}
// in{s<1351:px,qqz}
// qqz{s>2770:qs,m<1801:hdj,R}
// gd{a>3333:R,R}
// hdj{m>838:A,pv}
//
// {x=787,m=2655,a=1222,s=2876}
// {x=1679,m=44,a=2067,s=496}
// {x=2036,m=264,a=79,s=2244}
// {x=2461,m=1339,a=466,s=291}
// {x=2127,m=1623,a=2188,s=1013}

type WorkflowId = string

interface Rule {
	condition?: Condition;
	match_result: 'A' | 'R' | WorkflowId;
}

interface Condition {
	rating_type: Type;
	comparison: '<' | '>';
	value: number;
}

type Rating = Map<Type, number>
type Type = 'x'|'m'|'a'|'s';

export function part1(raw_input: string): number {
	const [raw_workflows, raw_ratings] = raw_input.trim().split('\n\n')

	const workflows: Map<WorkflowId, Rule[]> = new Map();
	raw_workflows.split('\n').forEach(line => {
		const [, id, raw_rules] = /(?<id>\w+)\{(?<raw_rules>[\w,:<>]+)\}/.exec(line);
		const rules: Rule[] = raw_rules.split(',').map(raw_rule => {
			const rule_regex = /((?<rating_type>[xmas])(?<comparison>[<>])(?<value>\d+):)?(?<match_result>\w+)/;
			const [,,rating_type, comparison, value, match_result] = rule_regex.exec(raw_rule);
			const condition = { rating_type, comparison, value };
			return rating_type != undefined ? { condition, match_result } : { match_result };
		})
		workflows.set(id, rules);
	});

	const ratings: Rating[] = raw_ratings.split('\n').map(line => {
		const rating: Rating = new Map();
		line.slice(1, -1).split(',').forEach(raw_rating => {
			const [type, raw_value]: [Type, string] = raw_rating.split('=')
			rating.set(type, Number(raw_value));
		});
		return rating;
	});

	return ratings.map(rating => apply_workflow(workflows, rating)).reduce((a, b) => a + b);
}

export function part2(raw_input: string): number {
	return 2;
}

function apply_workflow(workflows: Map<string, Rule[]>, rating: Rating, workflow_id = 'in'): number {
	for (const rule: Rule of workflows.get(workflow_id)) {
		let result;

		if (rule.condition == undefined) {
			result = rule.match_result;
		} else {
			const type_value = rating.get(rule.condition.rating_type);

			if (rule.condition.comparison == '<' && type_value < rule.condition.value) {
				result = rule.match_result;
			}
			if (rule.condition.comparison == '>' && type_value > rule.condition.value) {
				result = rule.match_result;
			}
		}

		if (result == 'A') {
			return [...rating.values()].reduce((a, b) => a + b);
		} else if (result == 'R') {
			return 0;
		} else if (result != undefined) {
			return apply_workflow(workflows, rating, result);
		}
	}

	throw new Error('bug');
}