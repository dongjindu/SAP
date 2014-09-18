*----------------------------------------------------------------------*
*   INCLUDE ZAPP717A_HOURLY_TOP                                        *
*----------------------------------------------------------------------*
REPORT  zapp717a_hourly_backflush MESSAGE-ID zmpp.

TABLES : ztpp_bfst,ausp,cabn,plaf.
TYPES : ppc_comp_conf.
*Internal table declations
DATA  : it_bfst LIKE ztpp_bfst OCCURS 0 WITH HEADER LINE,
        wa_bfst LIKE it_bfst.
DATA  : it_resb LIKE resb OCCURS 0 WITH HEADER LINE,
        it_resbt LIKE it_resb OCCURS 0 WITH HEADER LINE.

DATA: ls_ppc_comp_conf TYPE zppc_comp_conf,
      lt_ppc_comp_conf TYPE zppc_comp_conf.
DATA: lt_mdpmx LIKE mdpm OCCURS 0 WITH HEADER LINE.
DATA: lt_act LIKE zppc_act_conf OCCURS 0 WITH HEADER LINE,
      lt_mat LIKE zppc_mat_conf OCCURS 0 WITH HEADER LINE.

*Routing
DATA: BEGIN OF it_routing OCCURS 0,
        matnr TYPE mara-matnr,
        dispo TYPE marc-dispo,
        plnnr TYPE plko-plnnr,
        plnal TYPE plko-plnal,
        vornr TYPE plpo-vornr,
        arbpl TYPE crhd-arbpl,
        sortb TYPE crhd-sortb,
        lar02 TYPE plpo-lar02,  "Activity type
        vgw02 TYPE plpo-vgw02,  "Standard value
        vge02 TYPE plpo-vge02,  "Unit of measure for the standard value
        lar03 TYPE plpo-lar03,  "Activity Type
        vgw03 TYPE plpo-vgw03,  "Standard value
        vge03 TYPE plpo-vge03,  "Unit of measure for the standard value
      END OF it_routing.

DATA : l_matnr LIKE plaf-matnr,
       l_verid LIKE plaf-verid,
       l_dispo LIKE plaf-dispo,
       l_plnng LIKE mkal-plnng,
       l_alnag LIKE mkal-alnag.

DATA : val_table LIKE zspp_vin_value OCCURS 0 WITH HEADER LINE.

*Input internal table activity
DATA : it_act LIKE zppc_act_conf OCCURS 0 WITH HEADER LINE,
       mod_id TYPE pvs_paguid,
       re_id TYPE pvs_paguid.
DATA : BEGIN OF it_plpo OCCURS 0,
           plnkn LIKE plpo-plnkn,
           num(2) TYPE n,
         END OF it_plpo.

DATA : w_int TYPE i,
       l_atinn LIKE ausp-atinn,
       p_flg(2),
       p_e,
       p_char(16),
       p_rp(2) TYPE c VALUE 'RP',
       gr_ind,
       gi_ind,
       l_text(22),
       l_text1(22),
       l_text2(8),
       f_date(8),
       g_date(11), "LIKE sy-datum,
       g_sortb LIKE crhd-sortb.
DATA : l_steus LIKE plpo-steus,     "Control key
       l_usr01 LIKE plpo-usr01.      "Backflush point
DATA  : act_type  TYPE ppeui_pname VALUE 'HMMA_MAN',
        act_type1 TYPE ppeui_pname VALUE 'HMMA_MCH'.

DATA : num(2) TYPE n,
       z_num(2) TYPE n,
       t_num TYPE i, r_num TYPE i.
*Structure for Batch log
DATA: i_ztca_if_log LIKE ztca_if_log.
DATA : z_total LIKE ztca_if_log-total,
       z_succ LIKE ztca_if_log-zsucc,
       z_fail LIKE ztca_if_log-zsucc.
DATA : g_arbpl TYPE pvs_res_name,
       g_mname TYPE ppeui_pname VALUE 'HMMA_MAN',
       g_cname TYPE ppeui_pname VALUE 'HMMA_MCH',
       plan_char(12) TYPE c VALUE 'P_PLAN_ORDER'.
