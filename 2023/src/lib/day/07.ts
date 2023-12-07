export function part1(raw_input: string): number {
	const hands = raw_input.trim().split('\n').map(s => {
		const [, hand, raw_bet] = /([AKQJT2-9]{5}) (\d+)/g.exec(s)
		return [hand, Number(raw_bet)]
	})

	const ranked_hands = hands.sort(([hand_a, bet_a], [hand_b, bet_b]) => {
		const rank_a = rank(hand_a)
		const rank_b = rank(hand_b)

		if (rank_a == rank_b) {
			for (let i = 0; i < hand_a.length; i++) {
				const card_ranking_a = card_ranking(hand_a[i])
				const card_ranking_b = card_ranking(hand_b[i])

				if (card_ranking_a != card_ranking_b) {
					return card_ranking_a - card_ranking_b;
				}
			}
			throw new Error("Same hand");
		}

		return rank_a - rank_b;
	})

	const scores = ranked_hands.map(([hand, bet], i) => {
		const score = (i + 1) * bet;
		return score;
	}).reduce((a, b) => a + b)

	return scores;
}

export function part2(raw_input: string): number {
	const hands = raw_input.trim().split('\n').map(s => {
		const [, hand, raw_bet] = /([AKQJT2-9]{5}) (\d+)/g.exec(s)
		return [hand, Number(raw_bet)]
	})

	const ranked_hands = hands.sort(([hand_a, bet_a], [hand_b, bet_b]) => {
		const rank_a = rank(hand_a, true)
		const rank_b = rank(hand_b, true)

		if (rank_a == rank_b) {
			for (let i = 0; i < hand_a.length; i++) {
				const card_ranking_a = card_ranking_joker(hand_a[i])
				const card_ranking_b = card_ranking_joker(hand_b[i])

				if (card_ranking_a != card_ranking_b) {
					return card_ranking_a - card_ranking_b;
				}
			}
			throw new Error("Same hand");
		}

		return rank_a - rank_b;
	})

	const scores = ranked_hands.map(([hand, bet], i) => {
		const score = (i + 1) * bet;
		return score;
	}).reduce((a, b) => a + b)

	return scores;
}

function rank(hand: string, joker: boolean = false): number {
	const counts = new Map();
	for (const card of hand) {
		const current_count = counts.has(card) ? counts.get(card) : 0
		counts.set(card, current_count + 1)
	}

	if (joker && counts.has('J')) {
		const most_valuable_card = [...counts.keys()].sort((card_a, card_b) => {
			if (card_a == 'J') { return 1 }
			if (card_b == 'J') { return -1 }
			return counts.get(card_b) - counts.get(card_a);
		})[0];

		if (most_valuable_card != 'J') {
			const real_count = counts.get(most_valuable_card);
			counts.set(most_valuable_card, real_count + counts.get('J'));
			counts.delete('J');
		}
	}

	const type = Math.max(...counts.values());

	if (type == 3 && [...counts.values()].includes(2)) {
		return 3.5; // full house
	}

	if (type == 2 && [...counts.values()].filter(v => v == 2).length == 2) {
		return 2.5; // two pair
	}

	return type;
}

function card_ranking(card: string): number {
	const card_ranking = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'];
	return card_ranking.indexOf(card);
}

function card_ranking_joker(card: string): number {
	const card_ranking = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A'];
	return card_ranking.indexOf(card);
}