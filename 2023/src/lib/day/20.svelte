<script>
	export let raw_input;

	import { part1, part2 } from './20';

	$: p1 = part1(raw_input);
	$: p2 = part2(raw_input);
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
<p class="aoc_yellow">Part 2: {p2}</p>

<img alt="mermaid js chart" src="https://kroki.io/mermaid/svg/eNptVUGSnDAMvM8rJreZw1Z-kFOekNtmD4MJprBhwHgdl13797SQg0E1x24LqS21RWeff1X_cP766-el-fT-OV3f3n5cG_d8tOqx-j_u9n4AH_dL4Ai_vv_-5tfvH_8Ju-Rsl6-vi1k33HoEtB4ByW9EsDkHi4CoRYZCzE3Oc0MB7YaXlPOSKm5Uzo2qOPmck684GCQMhhIy0UNSvxwSRiSMFWsk1JRwHoVk3XDGAZIHBBQcWwTEFgFL5AyQrEmys1xhQoUJ2C58bnBugBeu0HS4QgecuGmhR4G-4rlDgblDAZs2wiCBoQQFB0d3dAhouo1wLmfnSGIUAckxgUsHunTBhxI8l0nnPGkq4UWGnts0ImCkgIJrF0YtCM8iBnwx0BdeitKGrwVRhkQVfHATq_S4lqdrFRzQuNBV7JHAx4oNJTCHBHvjCt4lHdx8dpk82EvIgwHaBvfioEfNnmo2SviNh2dhWOsrrm0ZuJEr7LSSnRa2Q0Kl5Cqus9NcIYacY6BbcqMtDGuHiusHge2iYFBFBm152C7ebi7e75eVHW7RDUvdKLhKNJwxoo9xM4MRFQrhIcmTJNWIgNaKVxassE8h9ofeMZ5nLIb5gPeUkxYqC6HReU2dH5zwVyGWEaMZSWXZHehLmCquH4xeEM69JlSL1rZkesYRrYzUyn4R6yqI7RRkG4bXRI8KPVWYZ84I0YlEh15OmwkFPynyk5pEnwqxPzNjzs9s7s6dL3gfbsEOO93ZQ_xICkYUKMT-JHz5K1gavqVVsZ4HUfC-GQrefysF7xu04P0x-nBWFIxw29yIuRVixFxGmosd5IJkokPKjlIuSf4F0uk_8g9WnFfh"/>

<style>
	img {
		max-width: 100vh;
		max-height: 100%;
		height: auto;
	}
</style>