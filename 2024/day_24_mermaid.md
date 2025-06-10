flowchart TD
x23 & y23 --> gate_mhq[AND]
gate_mhq[AND] --> mhq
x33 & y33 --> gate_pvc[AND]
gate_pvc[AND] --> pvc
y32 & x32 --> gate_rjq[AND]
gate_rjq[AND] --> rjq
x31 & y31 --> gate_kbv[XOR]
gate_kbv[XOR] --> kbv
y29 & x29 --> gate_wft[XOR]
gate_wft[XOR] --> wft
y28 & x28 --> gate_wgd[AND]
gate_wgd[AND] --> wgd
x27 & y27 --> gate_jns[AND]
gate_jns[AND] --> jns
x27 & y27 --> gate_mcv[XOR]
gate_mcv[XOR] --> mcv
y25 & x25 --> gate_tvb[AND]
gate_tvb[AND] --> tvb
y24 & x24 --> gate_tbn[AND]
gate_tbn[AND] --> tbn
y22 & x22 --> gate_nrq[AND]
gate_nrq[AND] --> nrq
x22 & y22 --> gate_jkq[XOR]
gate_jkq[XOR] --> jkq
y18 & x18 --> gate_sdp[AND]
gate_sdp[AND] --> sdp
y17 & x17 --> gate_nhh[AND]
gate_nhh[AND] --> nhh
y17 & x17 --> gate_nqm[XOR]
gate_nqm[XOR] --> nqm
x15 & y15 --> gate_srk[XOR]
gate_srk[XOR] --> srk
y14 & x14 --> gate_dwt[AND]
gate_dwt[AND] --> dwt
y13 & x13 --> gate_ggg[AND]
gate_ggg[AND] --> ggg
x12 & y12 --> gate_pps[XOR]
gate_pps[XOR] --> pps
y11 & x11 --> gate_rvg[AND]
gate_rvg[AND] --> rvg
x10 & y10 --> gate_ptj[AND]
gate_ptj[AND] --> ptj
x09 & y09 --> gate_bsm[AND]
gate_bsm[AND] --> bsm
y06 & x06 --> gate_cnm[AND]
gate_cnm[AND] --> cnm
x06 & y06 --> gate_bgq[XOR]
gate_bgq[XOR] --> bgq
x05 & y05 --> gate_chs[XOR]
gate_chs[XOR] --> chs
y03 & x03 --> gate_ssj[AND]
gate_ssj[AND] --> ssj
y03 & x03 --> gate_fpk[XOR]
gate_fpk[XOR] --> fpk
y01 & x01 --> gate_gpk[XOR]
gate_gpk[XOR] --> gpk
y00 & x00 --> gate_spq[AND]
gate_spq[AND] --> spq
gpk & spq --> gate_bvk[AND]
gate_bvk[AND] --> bvk
y01 & x01 --> gate_khh[AND]
gate_khh[AND] --> khh
bvk & khh --> gate_nmw[OR]
gate_nmw[OR] --> nmw
x02 & y02 --> gate_bgp[XOR]
gate_bgp[XOR] --> bgp
nmw & bgp --> gate_bcr[AND]
gate_bcr[AND] --> bcr
y02 & x02 --> gate_psw[AND]
gate_psw[AND] --> psw
bcr & psw --> gate_dfh[OR]
gate_dfh[OR] --> dfh
fpk & dfh --> gate_ccr[AND]
gate_ccr[AND] --> ccr
ssj & ccr --> gate_qfk[OR]
gate_qfk[OR] --> qfk
y04 & x04 --> gate_pvj[XOR]
gate_pvj[XOR] --> pvj
qfk & pvj --> gate_ndp[AND]
gate_ndp[AND] --> ndp
y04 & x04 --> gate_nfq[AND]
gate_nfq[AND] --> nfq
ndp & nfq --> gate_rgv[OR]
gate_rgv[OR] --> rgv
chs & rgv --> gate_jhv[AND]
gate_jhv[AND] --> jhv
y05 & x05 --> gate_nph[AND]
gate_nph[AND] --> nph
jhv & nph --> gate_wdh[OR]
gate_wdh[OR] --> wdh
bgq & wdh --> gate_dgh[AND]
gate_dgh[AND] --> dgh
cnm & dgh --> gate_pmv[OR]
gate_pmv[OR] --> pmv
x07 & y07 --> gate_gqg[XOR]
gate_gqg[XOR] --> gqg
pmv & gqg --> gate_dbm[AND]
gate_dbm[AND] --> dbm
y07 & x07 --> gate_jft[AND]
gate_jft[AND] --> jft
dbm & jft --> gate_qtg[OR]
gate_qtg[OR] --> qtg
x08 & y08 --> gate_knh[XOR]
gate_knh[XOR] --> knh
qtg & knh --> gate_ccd[AND]
gate_ccd[AND] --> ccd
y08 & x08 --> gate_cst[AND]
gate_cst[AND] --> cst
ccd & cst --> gate_cbk[OR]
gate_cbk[OR] --> cbk
x09 & y09 --> gate_nbm[XOR]
gate_nbm[XOR] --> nbm
cbk & nbm --> gate_vsc[AND]
gate_vsc[AND] --> vsc
bsm & vsc --> gate_fjn[OR]
gate_fjn[OR] --> fjn
x10 & y10 --> gate_htc[XOR]
gate_htc[XOR] --> htc
fjn & htc --> gate_dtg[AND]
gate_dtg[AND] --> dtg
ptj & dtg --> gate_spg[OR]
gate_spg[OR] --> spg
x11 & y11 --> gate_hhm[XOR]
gate_hhm[XOR] --> hhm
spg & hhm --> gate_wvv[AND]
gate_wvv[AND] --> wvv
rvg & wvv --> gate_wdg[OR]
gate_wdg[OR] --> wdg
pps & wdg --> gate_vdc[XOR]
gate_vdc[XOR] --> vdc
wdg & pps --> gate_kcp[AND]
gate_kcp[AND] --> kcp
vdc & kcp --> gate_fbk[OR]
gate_fbk[OR] --> fbk
x13 & y13 --> gate_tcq[XOR]
gate_tcq[XOR] --> tcq
fbk & tcq --> gate_hvg[AND]
gate_hvg[AND] --> hvg
ggg & hvg --> gate_csf[OR]
gate_csf[OR] --> csf
x14 & y14 --> gate_bck[XOR]
gate_bck[XOR] --> bck
csf & bck --> gate_qsg[AND]
gate_qsg[AND] --> qsg
dwt & qsg --> gate_sjm[OR]
gate_sjm[OR] --> sjm
srk & sjm --> gate_fwg[AND]
gate_fwg[AND] --> fwg
x15 & y15 --> gate_gtf[AND]
gate_gtf[AND] --> gtf
fwg & gtf --> gate_rrj[OR]
gate_rrj[OR] --> rrj
x16 & y16 --> gate_mft[XOR]
gate_mft[XOR] --> mft
rrj & mft --> gate_tnt[AND]
gate_tnt[AND] --> tnt
x16 & y16 --> gate_khn[AND]
gate_khn[AND] --> khn
tnt & khn --> gate_fmf[OR]
gate_fmf[OR] --> fmf
nqm & fmf --> gate_tgq[AND]
gate_tgq[AND] --> tgq
nhh & tgq --> gate_jmr[OR]
gate_jmr[OR] --> jmr
y18 & x18 --> gate_nhm[XOR]
gate_nhm[XOR] --> nhm
jmr & nhm --> gate_ptd[AND]
gate_ptd[AND] --> ptd
sdp & ptd --> gate_rtg[OR]
gate_rtg[OR] --> rtg
y19 & x19 --> gate_kfd[XOR]
gate_kfd[XOR] --> kfd
rtg & kfd --> gate_dmb[AND]
gate_dmb[AND] --> dmb
x19 & y19 --> gate_crv[AND]
gate_crv[AND] --> crv
dmb & crv --> gate_ddw[OR]
gate_ddw[OR] --> ddw
x20 & y20 --> gate_fht[XOR]
gate_fht[XOR] --> fht
ddw & fht --> gate_qfn[AND]
gate_qfn[AND] --> qfn
x20 & y20 --> gate_fct[AND]
gate_fct[AND] --> fct
qfn & fct --> gate_rsc[OR]
gate_rsc[OR] --> rsc
x21 & y21 --> gate_bbn[XOR]
gate_bbn[XOR] --> bbn
rsc & bbn --> gate_nhn[XOR]
gate_nhn[XOR] --> nhn
jkq & nhn --> gate_vkg[AND]
gate_vkg[AND] --> vkg
nrq & vkg --> gate_mmw[OR]
gate_mmw[OR] --> mmw
x23 & y23 --> gate_ncf[XOR]
gate_ncf[XOR] --> ncf
mmw & ncf --> gate_rkf[AND]
gate_rkf[AND] --> rkf
rkf & mhq --> gate_rqd[OR]
gate_rqd[OR] --> rqd
x24 & y24 --> gate_cns[XOR]
gate_cns[XOR] --> cns
rqd & cns --> gate_grb[AND]
gate_grb[AND] --> grb
tbn & grb --> gate_jhd[OR]
gate_jhd[OR] --> jhd
tvb & jhd --> gate_rcb[AND]
gate_rcb[AND] --> rcb
x25 & y25 --> gate_khg[XOR]
gate_khg[XOR] --> khg
rcb & khg --> gate_mfp[OR]
gate_mfp[OR] --> mfp
x26 & y26 --> gate_spf[XOR]
gate_spf[XOR] --> spf
mfp & spf --> gate_gqb[AND]
gate_gqb[AND] --> gqb
y26 & x26 --> gate_csh[AND]
gate_csh[AND] --> csh
gqb & csh --> gate_mbp[OR]
gate_mbp[OR] --> mbp
mcv & mbp --> gate_npn[AND]
gate_npn[AND] --> npn
jns & npn --> gate_jbv[OR]
gate_jbv[OR] --> jbv
x28 & y28 --> gate_kjg[XOR]
gate_kjg[XOR] --> kjg
jbv & kjg --> gate_vjg[AND]
gate_vjg[AND] --> vjg
wgd & vjg --> gate_nnq[OR]
gate_nnq[OR] --> nnq
wft & nnq --> gate_stm[AND]
gate_stm[AND] --> stm
y29 & x29 --> gate_tjt[AND]
gate_tjt[AND] --> tjt
stm & tjt --> gate_gsj[OR]
gate_gsj[OR] --> gsj
x30 & y30 --> gate_ftw[XOR]
gate_ftw[XOR] --> ftw
gsj & ftw --> gate_pwp[AND]
gate_pwp[AND] --> pwp
y30 & x30 --> gate_nmj[AND]
gate_nmj[AND] --> nmj
pwp & nmj --> gate_ctw[OR]
gate_ctw[OR] --> ctw
kbv & ctw --> gate_qhg[AND]
gate_qhg[AND] --> qhg
x31 & y31 --> gate_gnc[AND]
gate_gnc[AND] --> gnc
qhg & gnc --> gate_fqc[OR]
gate_fqc[OR] --> fqc
x32 & y32 --> gate_cpt[XOR]
gate_cpt[XOR] --> cpt
fqc & cpt --> gate_nfh[AND]
gate_nfh[AND] --> nfh
rjq & nfh --> gate_wcs[OR]
gate_wcs[OR] --> wcs
x33 & y33 --> gate_jbr[XOR]
gate_jbr[XOR] --> jbr
wcs & jbr --> gate_gst[XOR]
gate_gst[XOR] --> gst
pvc & gst --> gate_kmm[OR]
gate_kmm[OR] --> kmm
x34 & y34 --> gate_ptk[XOR]
gate_ptk[XOR] --> ptk
kmm & ptk --> gate_vnr[AND]
gate_vnr[AND] --> vnr
y34 & x34 --> gate_krb[AND]
gate_krb[AND] --> krb
vnr & krb --> gate_dbf[OR]
gate_dbf[OR] --> dbf
y35 & x35 --> gate_dsb[XOR]
gate_dsb[XOR] --> dsb
dbf & dsb --> gate_z35[XOR]
gate_z35[XOR] --> z35
x41 & y41 --> gate_qss[AND]
gate_qss[AND] --> qss
x42 & y42 --> gate_djs[AND]
gate_djs[AND] --> djs
y42 & x42 --> gate_vbq[XOR]
gate_vbq[XOR] --> vbq
y39 & x39 --> gate_cbf[AND]
gate_cbf[AND] --> cbf
y39 & x39 --> gate_fgc[XOR]
gate_fgc[XOR] --> fgc
x38 & y38 --> gate_tvm[XOR]
gate_tvm[XOR] --> tvm
x37 & y37 --> gate_hbr[XOR]
gate_hbr[XOR] --> hbr
x36 & y36 --> gate_jbq[AND]
gate_jbq[AND] --> jbq
x36 & y36 --> gate_fcf[XOR]
gate_fcf[XOR] --> fcf
x35 & y35 --> gate_jdk[AND]
gate_jdk[AND] --> jdk
dbf & dsb --> gate_pmr[AND]
gate_pmr[AND] --> pmr
jdk & pmr --> gate_spk[OR]
gate_spk[OR] --> spk
fcf & spk --> gate_vmj[AND]
gate_vmj[AND] --> vmj
jbq & vmj --> gate_qfp[OR]
gate_qfp[OR] --> qfp
hbr & qfp --> gate_mjr[AND]
gate_mjr[AND] --> mjr
y37 & x37 --> gate_kpb[AND]
gate_kpb[AND] --> kpb
mjr & kpb --> gate_pkw[OR]
gate_pkw[OR] --> pkw
tvm & pkw --> gate_tvc[AND]
gate_tvc[AND] --> tvc
x38 & y38 --> gate_dcc[AND]
gate_dcc[AND] --> dcc
tvc & dcc --> gate_dvm[OR]
gate_dvm[OR] --> dvm
fgc & dvm --> gate_nfn[AND]
gate_nfn[AND] --> nfn
cbf & nfn --> gate_fvh[OR]
gate_fvh[OR] --> fvh
y40 & x40 --> gate_pgc[XOR]
gate_pgc[XOR] --> pgc
fvh & pgc --> gate_hdq[AND]
gate_hdq[AND] --> hdq
x40 & y40 --> gate_gdm[AND]
gate_gdm[AND] --> gdm
hdq & gdm --> gate_fdv[OR]
gate_fdv[OR] --> fdv
x41 & y41 --> gate_dfs[XOR]
gate_dfs[XOR] --> dfs
fdv & dfs --> gate_wtt[AND]
gate_wtt[AND] --> wtt
qss & wtt --> gate_bwv[OR]
gate_bwv[OR] --> bwv
vbq & bwv --> gate_tgk[AND]
gate_tgk[AND] --> tgk
djs & tgk --> gate_ccp[OR]
gate_ccp[OR] --> ccp
tvb & jhd --> gate_z25[XOR]
gate_z25[XOR] --> z25
tvm & pkw --> gate_z38[XOR]
gate_z38[XOR] --> z38
x44 & y44 --> gate_vwd[XOR]
gate_vwd[XOR] --> vwd
y43 & x43 --> gate_pkj[AND]
gate_pkj[AND] --> pkj
y43 & x43 --> gate_hhg[XOR]
gate_hhg[XOR] --> hhg
hhg & ccp --> gate_msp[AND]
gate_msp[AND] --> msp
pkj & msp --> gate_kvv[OR]
gate_kvv[OR] --> kvv
vwd & kvv --> gate_mgw[AND]
gate_mgw[AND] --> mgw
nmw & bgp --> gate_z02[XOR]
gate_z02[XOR] --> z02
bbn & rsc --> gate_cdc[AND]
gate_cdc[AND] --> cdc
x21 & y21 --> gate_stq[AND]
gate_stq[AND] --> stq
cdc & stq --> gate_z21[OR]
gate_z21[OR] --> z21
fht & ddw --> gate_z20[XOR]
gate_z20[XOR] --> z20
gqg & pmv --> gate_z07[XOR]
gate_z07[XOR] --> z07
bwv & vbq --> gate_z42[XOR]
gate_z42[XOR] --> z42
kbv & ctw --> gate_z31[XOR]
gate_z31[XOR] --> z31
mmw & ncf --> gate_z23[XOR]
gate_z23[XOR] --> z23
wft & nnq --> gate_z29[XOR]
gate_z29[XOR] --> z29
rrj & mft --> gate_z16[XOR]
gate_z16[XOR] --> z16
y00 & x00 --> gate_z00[XOR]
gate_z00[XOR] --> z00
nhm & jmr --> gate_z18[XOR]
gate_z18[XOR] --> z18
kfd & rtg --> gate_z19[XOR]
gate_z19[XOR] --> z19
cpt & fqc --> gate_z32[XOR]
gate_z32[XOR] --> z32
spq & gpk --> gate_z01[XOR]
gate_z01[XOR] --> z01
x12 & y12 --> gate_z12[AND]
gate_z12[AND] --> z12
mfp & spf --> gate_z26[XOR]
gate_z26[XOR] --> z26
spg & hhm --> gate_z11[XOR]
gate_z11[XOR] --> z11
fmf & nqm --> gate_z17[XOR]
gate_z17[XOR] --> z17
fvh & pgc --> gate_z40[XOR]
gate_z40[XOR] --> z40
fcf & spk --> gate_z36[XOR]
gate_z36[XOR] --> z36
ptk & kmm --> gate_z34[XOR]
gate_z34[XOR] --> z34
jbr & wcs --> gate_z33[AND]
gate_z33[AND] --> z33
kvv & vwd --> gate_z44[XOR]
gate_z44[XOR] --> z44
srk & sjm --> gate_z15[XOR]
gate_z15[XOR] --> z15
bgq & wdh --> gate_z06[XOR]
gate_z06[XOR] --> z06
jbv & kjg --> gate_z28[XOR]
gate_z28[XOR] --> z28
ccp & hhg --> gate_z43[XOR]
gate_z43[XOR] --> z43
ftw & gsj --> gate_z30[XOR]
gate_z30[XOR] --> z30
x44 & y44 --> gate_bds[AND]
gate_bds[AND] --> bds
fjn & htc --> gate_z10[XOR]
gate_z10[XOR] --> z10
mgw & bds --> gate_z45[OR]
gate_z45[OR] --> z45
pvj & qfk --> gate_z04[XOR]
gate_z04[XOR] --> z04
jkq & nhn --> gate_z22[XOR]
gate_z22[XOR] --> z22
chs & rgv --> gate_z05[XOR]
gate_z05[XOR] --> z05
knh & qtg --> gate_z08[XOR]
gate_z08[XOR] --> z08
hbr & qfp --> gate_z37[XOR]
gate_z37[XOR] --> z37
dfs & fdv --> gate_z41[XOR]
gate_z41[XOR] --> z41
dvm & fgc --> gate_z39[XOR]
gate_z39[XOR] --> z39
cns & rqd --> gate_z24[XOR]
gate_z24[XOR] --> z24
dfh & fpk --> gate_z03[XOR]
gate_z03[XOR] --> z03
fbk & tcq --> gate_z13[XOR]
gate_z13[XOR] --> z13
mcv & mbp --> gate_z27[XOR]
gate_z27[XOR] --> z27
nbm & cbk --> gate_z09[XOR]
gate_z09[XOR] --> z09
bck & csf --> gate_z14[XOR]
gate_z14[XOR] --> z14
