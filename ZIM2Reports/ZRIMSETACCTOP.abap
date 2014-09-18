*----------------------------------------------------------------------*
*   INCLUDE ZRIMSETACCTOP                                              *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*   TABLE Declare                                                      *
*----------------------------------------------------------------------*
tables : ztbkpf,             " Import Cost Header.
         ztbdiv,             " Import Cost Distribution of P/O
         ztreqhd,            " Import Request Header Table
         ztreqit,            " Import Request Item Table
         ztbl,               " B/L Header Table
         ztblit,             " B/L Item Table
         ztsetac,            " Settled Account Management Table
         ekpo,               " P/O Item Table
         ekbe,               " G/R History Table
         ekbz,               " G/R History Item Table.
         t685t,              " Conditions: Types: Texts.
         mbew,               " Material Valuation
         usr01,              " User master record (runtime data)
         t001w,              " Plants/Branches
         t001k,              " Valuation area
         marv,               " Material Control Record
         t030,               " Standard Accounts Table
         ztimimg11,          " Account No. Management Table
         ztimimg00.          " Basic Config.

data : w_tabix         like  sy-tabix,
       w_date          like  sy-datum,
       w_ym(6)         type  c,
       w_imdno         like  ztbdiv-zfimdno,
       w_belnr         like  ekbz-belnr,
       w_chk           type  c,
       w_line          type  i,
       w_mod           type  i,
       l_menge(17)     type  c,
       l_meins(5)      type  c,
       w_lc_text(20)   type  c,
       w_bl_text(20)   type  c,
       w_err_chk(1)    type  c,
       w_set_qty       like  ekpo-menge,
       w_to_qty        like  ekpo-menge,
       w_settle_amount like  ekbz-dmbtr,
       w_minus_amount  like  ekbz-dmbtr,
       w_price_amount  like  ekbz-dmbtr,
       w_count         type  i,
       w_act_acc       like  ztbdiv-akont,
       w_pld_acc       like  ztbdiv-akont,
       w_co_acc        like  ztbdiv-akont,
       w_pri_acc       like  ztbdiv-akont,
       w_text(18)      type  c,
       w_char_date(8)  type  c,
       w_budat         like  sy-datum,
       w_day(2)        type  c,
       w_lcno(20)      type  c,
       w_blno(20)      type  c,
       temp_wrbtr(16)  type  c,
       w_max_seq       like  ztsetac-zfseq,
       w_gjahr         like  bkpf-gjahr,
       include(8)      type  c.

* Total Line Variable.
data : w_tot_actual    like  ekbz-dmbtr,
       w_tot_pland     like  ekbz-dmbtr,
       w_tot_cha       like  ekbz-dmbtr,
       w_set_actual    like  ekbz-dmbtr,
       w_set_pland     like  ekbz-dmbtr,
       w_tset_actual   like  ekbz-dmbtr,
       w_tset_pland    like  ekbz-dmbtr,
       w_nset          like  ekbz-dmbtr.

*-----------------------------------------------------------------------
* Internal Table Define
*-----------------------------------------------------------------------
data: begin of it_po occurs 0,
   ebeln    like     ztreqit-ebeln,
   ebelp    like     ztreqit-ebelp,
   MATNR    like     ekpo-MATNR,
end of it_po.

data: begin of it_bsis occurs 0,
   zuonr    like       bsis-zuonr,
end of it_bsis.

data : begin of it_div occurs 0,
       ebeln           like   ztbdiv-ebeln,      " PO Header.
       ebelp           like   ztbdiv-ebelp,      " PO Item.
       hkont           like   bsis-hkont,
       matnr           like   ztbdiv-matnr,      " Material
       zfimdno         like   ztbdiv-zfimdno,    " Import Doc. No.
       bukrs           like   ztbdiv-bukrs,      " Company Code
       zfacdo          like   ztbkpf-zfacdo,     " Account No
       zffiyr          like   ztbkpf-zffiyr,     " Account Year
       cstgrp          like   ztbdiv-zfcstgrp,   " Cost Group.
       kschl           like   ztbdiv-cond_type,  " Condition
       wrbtr           like   ztbdiv-wrbtr,      " Amount
       dmbtr           like   ztbdiv-dmbtr,      " Amount(Local)
       hwaer           like   ztbdiv-hwaer,      " Currency
       menge           like   ztbdiv-menge,      " Quantity
       meins           like   ztbdiv-meins.      " Unit of measure
data : end   of it_div.

data : begin of it_ekbz occurs 0,
       ebeln           like   ekbz-ebeln,        " PO Header
       ebelp           like   ekbz-ebelp,        " PO Item
       gjahr           like   ekbz-gjahr,        " Account Year
       belnr           like   ekbz-belnr,        " Account No
       hswae           like   ekbz-hswae,        " Currency
       meins           like   ekpo-meins,        " Unit of Measure
       kschl           like   ekbz-kschl,        " Condition
       dmbtr           like   ekbz-dmbtr,        " Amount(Local)
       menge           like   ekbz-menge,        " Quanitity.
       shkzg           like   ekbz-shkzg.        " Debit/Credit Type
data : end   of it_ekbz.

data : begin of it_set occurs 0,
       ebeln           like   ztsetac-ebeln,
       ebelp           like   ztsetac-ebelp,
       hkont           like   bsis-hkont,
       dmbtr           like   bsis-dmbtr,
       zfsetqty        like   ztsetac-zfsetqty,
       zfsetlc         like   ztsetac-zfsetlc,
       zfsetbl         like   ztsetac-zfsetbl,
       zfpldlc         like   ztsetac-zfpldlc,
       zfpldbl         like   ztsetac-zfpldbl.
data : end   of it_set.

data : begin of it_tab occurs 0,
       ebeln           like   ekbz-ebeln,
       ebelp           like   ekbz-ebelp,
       matnr           like   ekpo-matnr,
       knttp           like   ekpo-knttp,
       lc_qty          like   ekpo-menge,
       bl_qty          like   ekpo-menge,
       gr_qty          like   ekpo-menge,
       to_qty          like   ekpo-menge,
       meins           like   ekpo-meins,
       waers           like   ztbdiv-waers,
       lc_act          like   ekbz-dmbtr,
       bl_act          like   ekbz-dmbtr,
       lc_pld          like   ekbz-dmbtr,
       bl_pld          like   ekbz-dmbtr,
       lc_cha          like   ekbz-dmbtr,
       bl_cha          like   ekbz-dmbtr,
       lc_set_ac       like   ekbz-dmbtr,
       bl_set_ac       like   ekbz-dmbtr,
       lc_set_pl       like   ekbz-dmbtr,
       bl_set_pl       like   ekbz-dmbtr,
       lc_nset         like   ekbz-dmbtr,
       bl_nset         like   ekbz-dmbtr,
       lc_tset_ac      like   ekbz-dmbtr,
       bl_tset_ac      like   ekbz-dmbtr,
       lc_tset_pl      like   ekbz-dmbtr,
       bl_tset_pl      like   ekbz-dmbtr,
       check(1)        type   c.
data : end   of  it_tab.

* Message Table.
data:   begin of return occurs 0.
       include  structure  bdcmsgcoll.
       data : icon       like bal_s_dmsg-%_icon,
              messtxt(255) type c.
data:   end   of return.

data : begin of it_setac occurs 0,
       ebeln           like   ztsetac-ebeln,
       ebelp           like   ztsetac-ebelp,
       zfsetym         like   ztsetac-zfsetym,
       zfsetlc         like   ztsetac-zfsetlc,
       zfsetbl         like   ztsetac-zfsetbl,
       zfpldlc         like   ztsetac-zfpldlc,
       zfpldbl         like   ztsetac-zfpldbl,
       zfsetamt        like   ztsetac-zfsetamt,
       waers           like   ztsetac-waers,
       zfsetqty        like   ztsetac-zfsetqty,
       meins           like   ztsetac-meins,
       zfsetyn         like   ztsetac-zfsetyn.
data : end   of it_setac.
