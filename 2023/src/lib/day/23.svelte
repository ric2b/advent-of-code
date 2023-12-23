<script>
	export let raw_input;

	import { part1, part2 } from './23';

	$: p1 = part1(raw_input);
	$: p2 = part2(raw_input);
</script>

<p class="aoc_yellow">Part 1: {p1}</p>
<p class="aoc_yellow">Part 2: {p2}</p>

<img alt="mermaid js chart" src="https://kroki.io/mermaid/svg/eNp1Vctu3DAMvO9X-LgFtIApWq9LT_2E3hY5FAWCBChaoAnQy358yRmltmz3kIw2M0uRQ4p5_vHrz_eXb7_fp69fLm_vfrjdPk8S5ov92Pn2SO0xSQrpevffT58uDmC0zEa1sKhxDluyLo8ptiByvQOMA4Js8TFpC7Fd74CBlFwfU0pBLCzAWCC_mkFqAallIONiGZWGrwKMBYLNEaQKSJWB1OTFiAaxyEQvBwcIluiCqEHdCqAJkD7vFgu_SMh2N2Bgaz2kjRxYsYeu5px9FTCybqXM9tl9BnpmOLCu08xXXqIHEAmpucBxFKR4UhkPLE2zC0pIEDjSdi29YxYgK-IDRnY5-o5x6dXlwyzAA958njpSoGBGai0UFwBNgCR6asbnEqplDhhYL3znOy_hdyXT2Co0tsoooG87W0yXe2ln72NlfVZ38wLktNnVi4ZikQEDKc7u_IaqtzseSqYzm6pUgsxeFg8maVYeR9WrUuQF2HLR59gMEIEljlta0jFtGsfIp73yFGmnxa4mtjEG-CQ49ueVD71aWWFeu1YlDY1F-0rZWbKS2Br2Z_EHRhx4UR_gwqKJ_f10QVnO5mTl44dA9OOJahklstTTrvTTaoGobTt_LP0Ahz0ljs1cMVRQEEcBXpt_tFKJvmvKP17meGjCho7YVL40zUniXtCXVWtcVs0T4E19go4jsqFx_6GArcBHDL2xTaoMTx3pdtbL7lVf9G6ipb7MbiIOHEPZ7stDkTzwf1A9bydPPcZ_esULXTM9v_58fXu5E57-AhyXnsY="/>

<style>
    img {
        max-width: 100vh;
        max-height: 100%;
        height: auto;
    }
</style>