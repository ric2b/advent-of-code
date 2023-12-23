<script>
	export let raw_input;

	import { part1, part2 } from './23';

	$: p1 = part1(raw_input);
	$: p2 = part2(raw_input);
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
<p class="aoc_yellow">Part 2: {p2}</p>

<img alt="mermaid js chart" src="https://kroki.io/mermaid/svg/eNp9V7uOFDEQzO8rNjwkI03b87ATIj6BbHUBQkJ3EgIJkEj243FX704_bAhue7VV7me5Z-7rtx9_vrx-_vn78unj06_f_OX9-w8XSsu1_7089Q_-4ba124W2tD1f-fPl3RMbRdLyDL75vRxLB1paS4fYWLCut0tuiej5CtMxWIAt3y6lpdyerzAOpL3eLtuWqLuFcSj7dWmCIl53nCsHzpXDgXntyR4NXmEcipghXbCA7hlHC-Fo8WDZuAtUEvW4Yh2OwKEcoYlvPr10r9xEWI-vmQm5pMIlw3oC4ofC0NSz06EuBTN1dKW096MwDq11aKeCVPdh9uiNbZhNSUHiimofXkdhPIoBx4YYfM1DWkLTdsZhGJwyuydKW2MCW0_Y8qzdxgHnF8QgPDOuIQEl5MKNy0faEICtJyCBWD8qtSK3alKQuPF7QXEwHl1HHSvK4w53EoM1V91thxMjYAUqgvEoCyX4xdRVCyElRTP1s7Wk2nsJ49HpKBXfxl5Iv-0c4qANYQGhpYPdw3oCBhXDI5LOIgjVot37fqTao8M4lF2HG2KOrvtwayW-Ufi4UZTAXWVpVhKJVvIEuQJRoYqj8SE9DN7IITwVTrR23w2R2z2uHuRFGeqC1V0VdKQoWlLS0TOGcSAaFpRg0VG-8KGOQ04nmKFt6mu1w2IdTjkPQzbotsdeQGVWfcOaOAkyRQ668Bjli6fU2Zw51DmJUPaJZV4GxAXhKFsLI_PQbglgBRZ3rBKOu0JbE4W2gNfpzTMBOL2wF9Bfc7X8uBXFy0VBaBgHbv_1y9MMVYNldpW_FIrSwqv3kIaK9bg0PMxKcU4sCAnZnyX9E8TrR_-ZeB2IdTiVWWYG32Yal6npMIdtonh-EKg8HonnQ_sRY61zLRsvvHOGNO9Uo7pBNYaR5cr0pyYvkfuX6AWpjNrkgHo1_K1VcF0q1gUCiPWE6cKQclQnQYEGznh14jeqPmuxkTC7WjYAjzvoXxLRjRY0bmCkN9RnCayWf7uXjRTWivhR_-NCNYQ2U7MhYAJDiPusrQpGsRlO2ZjS-7vyfz3yJVAo71NZS1LmzX_YNWRfRYdhWkJbJt0Wpsph3CpKaHWao2EgiaGhd6q5v7Nt_WBIKyb3ynA4k6HWe2dNyyd386Rcvr59f_v1ehXz8hcaZTQK"/>

<style>
    img {
        max-width: 100vh;
        max-height: 100%;
        height: auto;
    }
</style>