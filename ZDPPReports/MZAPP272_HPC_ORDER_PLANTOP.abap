*&---------------------------------------------------------------------*
*& Include MZAPP272_HPC_ORDER_PLANTOP                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
program  sapmzapp272_hpc_order_plan message-id zmpp.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
type-pools vrm .
data: it_list_app272  type vrm_values,
      wa_value_app272 type vrm_value,
      wa_name_app272  type vrm_value-text.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables: ztbm_abxpcldt,
        ztbm_abxplidt,
        ztbm_abxplcdt,
        ztbm_abxplbdt.
*----------------------------------------------------------------------*
* TABLE-CONTROLS
*----------------------------------------------------------------------*
controls: tc_app272_01 type tableview using screen 3110.
controls: tc_app272_02 type tableview using screen 3111.
controls: tc_app272_03 type tableview using screen 3112.
*----------------------------------------------------------------------*
* INTERNEL TABLES
*----------------------------------------------------------------------*
data: begin of it_pcld_app272 occurs 0,
        carx type ztbm_abxpcldt-carx,
        gubn type ztbm_abxpcldt-gubn,
        hpcc type ztbm_abxpcldt-hpcc,
        upgn type ztbm_abxpcldt-upgn,
        head type ztbm_abxpcldt-head,
        saya type ztbm_abxpcldt-saya,
        aloc type ztbm_abxpcldt-aloc,
        torq type ztbm_abxpcldt-torq,
        torn type ztbm_abxpcldt-torn,
        torx type ztbm_abxpcldt-torx,
        mark type ztbm_abxpcldt-mark,
        secu type ztbm_abxpcldt-secu,
        lprt type ztbm_abxpcldt-lprt,
        oprt type ztbm_abxpcldt-oprt,
        depo type ztbm_abxpcldt-depo,
        loca type ztbm_abxpcldt-loca,
        colm type ztbm_abxpcldt-colm,
        opt1 type ztbm_abxpcldt-opt1,
        opt2 type ztbm_abxpcldt-opt2,
        opt3 type ztbm_abxpcldt-opt3,
        opt4 type ztbm_abxpcldt-opt4,
        zpp_user type ztbm_abxpcldt-zpp_user,
      end of it_pcld_app272.
data: begin of it_app272_01 occurs 0,
        p001 type c,
        p002 type c,
        p003 type c,
        p004 type c,
        p005 type c,
        p006 type c,
        p007 type c,
        p008 type c,
        p009 type c,
        p010 type c,
        p011 type c,
        p012 type c,
        p013 type c,
        p014 type c,
        p015 type c,
        p016 type c,
        p017 type c,
        p018 type c,
        p019 type c,
        p020 type c,
        p021 type c,
        p022 type c,
        p023 type c,
        p024 type c,
        p025 type c,
        p026 type c,
        p027 type c,
        p028 type c,
        p029 type c,
        p030 type c,
      end of it_app272_01.
data: begin of it_app272_02 occurs 0,
        p001 type c,
        p002 type c,
        p003 type c,
        p004 type c,
        p005 type c,
        p006 type c,
        p007 type c,
        p008 type c,
        p009 type c,
      end of it_app272_02.
data: begin of it_app272_03 occurs 0,
        p001 type c,
        p002 type c,
        p003 type c,
        p004 type c,
        p005 type c,
        p006 type c,
        p007 type c,
        p008 type c,
        p009 type c,
        p010 type c,
        p011 type c,
      end of it_app272_03.
*----------------------------------------------------------------------*
* DATAS
*----------------------------------------------------------------------*
data: ok_code type sy-ucomm,
      okcode_02  type sy-ucomm.
data: sv_prog_app272                 like sy-repid,
      sv_dynnr_app272                like sy-dynnr.
data: p_maker_app272(3),
      p_plnt_app272 like ztbm_abxplidt-plnt,
      p_carx_app272 like ztbm_abxplidt-carx,
      p_gubn_app272 like ztbm_abxplidt-gubn,
      p_hpcc_app272 like ztbm_abxplidt-hpcc.
data: wa_y_app272,
      wa_t_app272,
      wa_p_app272,
      wa_m_app272,
      wa_d_app272,
      wa_s_app272,
      wa_h_app272,
      wa_x_app272,
      wa_clm1_app272(4),
      wa_clm2_app272(4),
      wa_urgen_app272(10).
