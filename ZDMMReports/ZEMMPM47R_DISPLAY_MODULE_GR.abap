************************************************************************
* Program Name      : ZEMMPM47R_DISPLAY_MODULE_GR
* Author            : Byung-sung, Bae
* Creation Date     : 2004.08.09
* Specifications By : Byung-sung, Bae
* Pattern           : Report 1-1
* Development Request No : UD1K911262
* Addl Documentation:
* Description       : Display GR Status for Module
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zemmpm47r_display_module_gr NO STANDARD PAGE HEADING
                                      LINE-SIZE  255.
INCLUDE: <icon>.
*----- Tables
TABLES: ztmm_assy_cost1,
        ztmm_cp_color,
        t001w,
        mara,
        eina,
        a018,
        t024,
        lfa1.

*----- Internal Tables
DATA: BEGIN OF it_sub OCCURS 0,
        vtype     LIKE   ztmm_assy_cost1-vtype,   "Vehicle type
        matnr     LIKE   ztmm_assy_cost1-vtype,   "Material
        upgvc     LIKE   mara-matnr,              "UPG-VC
        pref      LIKE   ztbm_abxduldt-pref,      "BOM Item Number
        comp      LIKE   ztbm_abxduldt-comp,      "BOM Component
        maktx     LIKE   makt-maktx,              "Description
        lifnr     LIKE   lfa1-lifnr,              "Vendor
        amount    TYPE   f,                       "Component Amount
        qnty      LIKE   ztbm_abxduldt-qnty,      "Quantity
        stgb      LIKE   ztbm_abxduldt-stgb,      "End item type
        unit      LIKE   ztbm_abxduldt-unit,      "Unit of measure(BOM)
        meins     LIKE   mara-meins,              "UoM(sub)
        kmein     LIKE   konp-kmein,              "UoM(Info)
        datab     LIKE   sy-datum,                "Valid on(sub)
        datbi     LIKE   sy-datum,                "Valid to(Sub)
        netpr     LIKE   ekpo-netpr,              "Component Amount
        peinh     LIKE   ekpo-peinh,              "Component Price Unit
        waers     LIKE   t001-waers,              "Currency
        kzust     LIKE   konh-kzust,              "Reason code
        dmbtr     LIKE   mseg-dmbtr,
        sts,                                      "Status
        msg(100),                                  "Message
      END   OF it_sub.

DATA: it_itab LIKE zsmm_module_gr OCCURS 0 WITH HEADER LINE.

*----- Global variables
DATA: wa_sub LIKE it_sub.

DATA: wa_vtype_f   LIKE   ztmm_assy_cost1-vtype,
      wa_vtype_t   LIKE   ztmm_assy_cost1-vtype,
      wa_mcode_f   LIKE   ztmm_assy_cost1-mcode,
      wa_mcode_t   LIKE   ztmm_assy_cost1-mcode,
      wa_lifnr_f   LIKE   ztmm_assy_cost1-lifnr,
      wa_lifnr_t   LIKE   ztmm_assy_cost1-lifnr,
      wa_matnr_f   LIKE   mara-matnr,
      wa_matnr_t   LIKE   mara-matnr,
      wa_ekgrp_f   LIKE   t024-ekgrp,
      wa_ekgrp_t   LIKE   t024-ekgrp,
      wa_budat_f   LIKE   sy-datum,
      wa_budat_t   LIKE   sy-datum,
      wa_matnr(6),
      wa_status_flag(6),
      wa_tot_page  TYPE   i,
      wa_format_flg.

*----- Constants
DATA: c_ekorg LIKE ekko-ekorg VALUE 'PU01',
      c_kschl LIKE konh-kschl VALUE 'PB00',
      c_no_cond TYPE   i             VALUE   3,"Condition does not exist
      c_del_con TYPE   i             VALUE   4,"Condition was deleted
      c_no_info TYPE   i             VALUE   5,"InfoRecord dosn't exist
      c_uom_err TYPE   i             VALUE   6,"Incorrect UoM
      c_no_matl TYPE   i             VALUE   7,"M/M does not exist.
      c_exist   TYPE   i             VALUE   8."Info Record is exist.

*****// ALV //**********************************************************
TYPE-POOLS: slis.
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gt_fc       TYPE slis_t_fieldcat_alv,
      g_fieldcat_s LIKE LINE OF gt_fieldcat,
      gs_layout   TYPE slis_layout_alv,
      gs_print    TYPE slis_print_alv,
      gt_sort     TYPE slis_t_sortinfo_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_header   TYPE slis_t_listheader,
      gt_header_detail  TYPE slis_t_listheader,
      gt_colinfo_table TYPE slis_t_specialcol_alv. "line color.

* hierarchy(simple)
DATA : g_tabname_header       TYPE slis_tabname,       "header part
       g_tabname_item         TYPE slis_tabname,       "detail list
       gs_keyinfo             TYPE slis_keyinfo_alv.   "relation key

* return
DATA : g_exit_caused_by_caller  TYPE c,
       gs_exit_caused_by_user   TYPE slis_exit_by_user.

DATA: col_pos TYPE i,
      cnt     TYPE i,
      g_save  TYPE c,
      g_repid LIKE sy-repid,
      g_variant LIKE disvariant.
DATA: gt_extab TYPE slis_t_extab WITH HEADER LINE.

DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE ztpp_alc_prod   .
DATA: END OF it_data.

CONSTANTS : c_pss TYPE slis_formname VALUE 'PF_STATUS_SET',
            c_uc  TYPE slis_formname VALUE 'USER_COMMAND',
            c_top TYPE slis_formname VALUE 'TOP_OF_PAGE'.

*----- Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_werks LIKE t001w-werks OBLIGATORY DEFAULT 'P001'.

PARAMETERS: p_vtype LIKE ztmm_assy_cost1-vtype OBLIGATORY.
SELECT-OPTIONS: s_mcode FOR  ztmm_assy_cost1-mcode
                             NO-EXTENSION NO INTERVALS,
                s_lifnr FOR  lfa1-lifnr
                             NO-EXTENSION NO INTERVALS,
                s_matnr FOR  mara-matnr NO-EXTENSION NO INTERVALS,
                s_ekgrp FOR  ztmm_assy_cost1-ekgrp
                             NO-EXTENSION NO INTERVALS,
                s_budat FOR  sy-datum NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK bl1.

*----- Initialization
INITIALIZATION.
  PERFORM initialization_rtn.

*----- Input value check & read data
AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM check_input_value.
  PERFORM read_data.

*----- Top-of-page
TOP-OF-PAGE.
  PERFORM top_of_page.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM display_detail_header.

START-OF-SELECTION.
  PERFORM  build_events.
  PERFORM  build_fieldcat    USING  'IT_ITAB'.
  PERFORM  build_layout      USING  'X'   space   space.
  PERFORM  build_comment     USING  gt_header[].
  PERFORM  start_grid_viewer TABLES  it_itab.

AT LINE-SELECTION.
  PERFORM double_click_rtn.

AT USER-COMMAND.
  CASE sy-ucomm.
    WHEN 'SUB_INFO'.
      PERFORM display_sub_info_record.
    WHEN 'SUB_MSG'.
      MESSAGE s000(zz) WITH it_sub-msg(50) it_sub-msg+50(50).
      CLEAR: it_sub.
    WHEN 'EXCEL'.
      PERFORM excel_download_rtn.
    WHEN 'PAGE'.
      PERFORM display_total_page.
  ENDCASE.
*&---------------------------------------------------------------------*
*&      Form  initialization_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization_rtn.
  s_budat-sign   = 'I'.
  s_budat-option = 'BT'.
  s_budat-high   = sy-datum.
  CONCATENATE sy-datum(6) '01' INTO s_budat-low.

  APPEND s_budat.
ENDFORM.                    " initialization_rtn
*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
  PERFORM check_werks.
  PERFORM check_vtype.
  PERFORM check_mcode.
  PERFORM check_lifnr.
  PERFORM check_matnr.
  PERFORM check_ekgrp.
  PERFORM check_budat.
ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  read_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  EXEC SQL PERFORMING APPEND_ITAB.
    SELECT A.WERKS, A.LIFNR, E.NAME1,
           A.MATNR, D.MAKTX, A.ZBUDAT,
           A.MBLNR, A.ZEILE, A.MJAHR,
           DECODE(A.BWART,'101',A.MENGE,'102',A.MENGE * -1,
                          '122',A.MENGE * -1,'123',A.MENGE),
           A.MEINS, F.EKGRP,
           F.WAERS, (H.DMBTR / A.MENGE), 1,
           DECODE(H.BWART,'101',H.DMBTR,'102',H.DMBTR * -1,
                          '122',H.DMBTR * -1,'123',H.DMBTR),
           A.BWART, A.LFBNR,
           A.LFPOS, A.LFBJA, F.AEDAT,
           C.EBELN, C.EBELP
      INTO :IT_ITAB-WERKS, :IT_ITAB-LIFNR, :IT_ITAB-NAME1,
           :IT_ITAB-MATNR, :IT_ITAB-MAKTX, :IT_ITAB-BUDAT,
           :IT_ITAB-MBLNR, :IT_ITAB-ZEILE, :IT_ITAB-MJAHR,
           :IT_ITAB-MENGE, :IT_ITAB-MEINS, :IT_ITAB-EKGRP,
           :IT_ITAB-WAERS, :IT_ITAB-NETPR, :IT_ITAB-PEINH,
           :IT_ITAB-DMBTR, :IT_ITAB-BWART, :IT_ITAB-LFBNR,
           :IT_ITAB-LFPOS, :IT_ITAB-LFBJA, :IT_ITAB-AEDAT,
           :IT_ITAB-EBELN, :IT_ITAB-EBELP
      FROM MSEG A, MARA B, EKPO C, MAKT D, LFA1 E,
           EKKO F, EKPO G, EKBE H
     WHERE A.MANDT    =       :SY-MANDT
       AND A.WERKS    =       :P_WERKS
       AND A.BWART    IN      ('101','102','122','123')
       AND A.ZBUDAT   BETWEEN :WA_BUDAT_F AND :WA_BUDAT_T
       AND A.LIFNR    BETWEEN :WA_LIFNR_F AND :WA_LIFNR_T
       AND A.MATNR    LIKE    :WA_MATNR
       AND A.MATNR    BETWEEN :WA_MATNR_F AND :WA_MATNR_T
       AND B.MANDT    =       :SY-MANDT
       AND B.MANDT    =       A.MANDT
       AND B.MATNR    =       A.MATNR
       AND B.MTART    =       'ROH'
       AND C.MANDT    =       A.MANDT
       AND C.EBELN    =       A.EBELN
       AND C.EBELP    =       A.EBELP
       AND D.MANDT(+) =       B.MANDT
       AND D.MATNR(+) =       B.MATNR
       AND D.SPRAS(+) =       :SY-LANGU
       AND E.MANDT    =       A.MANDT
       AND E.LIFNR    =       A.LIFNR
       AND F.MANDT    =       C.MANDT
       AND F.EBELN    =       C.EBELN
       AND F.EKGRP    BETWEEN :WA_EKGRP_F AND :WA_EKGRP_T
       AND G.MANDT    =       A.MANDT
       AND G.EBELN    =       A.EBELN
       AND G.EBELP    =       A.EBELP
       AND H.MANDT    =       G.MANDT
       AND H.EBELN    =       G.EBELN
       AND H.EBELP    =       G.EBELP
       AND H.GJAHR    =       A.GJAHR
       AND H.BELNR    =       A.MBLNR
       AND H.BUZEI    =       A.ZEILE
       AND H.BWART    =       A.BWART
       AND H.BEWTP    =       'E'
  ENDEXEC.
ENDFORM.                    " read_data
*&---------------------------------------------------------------------*
*&      Form  check_vtype
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_vtype.
  TRANSLATE p_vtype TO UPPER CASE.
ENDFORM.                    " check_vtype
*&---------------------------------------------------------------------*
*&      Form  check_mcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mcode.
  IF s_mcode-low EQ ' '.
    wa_mcode_t = 'ZZ'.
    CONCATENATE p_vtype '%' INTO wa_matnr.
  ELSE.
    wa_mcode_f = wa_mcode_t = s_mcode-low.
    CONCATENATE p_vtype s_mcode-low '%' INTO wa_matnr.
  ENDIF.
ENDFORM.                    " check_mcode
*&---------------------------------------------------------------------*
*&      Form  check_lifnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_lifnr.
  SELECT SINGLE * FROM lfa1 WHERE lifnr IN s_lifnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m03.
  ENDIF.

  IF s_lifnr-low EQ ' '.
    wa_lifnr_t = 'ZZZZZZZZZZ'.
  ELSE.
    wa_lifnr_f = wa_lifnr_t = s_lifnr-low.
  ENDIF.
ENDFORM.                    " check_lifnr
*&---------------------------------------------------------------------*
*&      Form  check_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_matnr.
  SELECT SINGLE * FROM mara WHERE matnr IN s_matnr.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.

  IF s_matnr-low EQ ' '.
    wa_matnr_t = 'ZZZZZZZZZZZZZZZZZZ'.
  ELSE.
    wa_matnr_t = wa_matnr_f = s_matnr-low.
  ENDIF.
ENDFORM.                    " check_matnr
*&---------------------------------------------------------------------*
*&      Form  check_ekgrp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ekgrp.
  SELECT SINGLE * FROM t024 WHERE ekgrp IN s_ekgrp.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  IF s_ekgrp-low EQ ' '.
    wa_ekgrp_t = 'ZZZ'.
  ELSE.
    wa_ekgrp_t = wa_ekgrp_f = s_ekgrp-low.
  ENDIF.
ENDFORM.                    " check_ekgrp
*&---------------------------------------------------------------------*
*&      Form  check_budat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_budat.
  DATA: lw_day TYPE i.

  IF s_budat-low > s_budat-high.
    MESSAGE e000(zz) WITH text-m05.
  ENDIF.

  lw_day = s_budat-high - s_budat-low.
  IF lw_day > 31.
    MESSAGE e000(zz) WITH text-m06.
  ENDIF.

  IF     s_budat-low EQ '00000000' AND s_budat-high EQ '00000000'.
    wa_budat_t = '99991231'.
  ELSEIF s_budat-low EQ '00000000' AND s_budat-high NE '00000000'.
    wa_budat_t = s_budat-high.
  ELSEIF s_budat-low NE '00000000' AND s_budat-high EQ '00000000'.
    wa_budat_f = wa_budat_t = s_budat-low.
  ELSEIF s_budat-low NE '00000000' AND s_budat-high NE '00000000'.
    wa_budat_f = s_budat-low.
    wa_budat_t = s_budat-high.
  ENDIF.
ENDFORM.                    " check_budat
*&---------------------------------------------------------------------*
*&      Form  APPEND_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_itab.
  SELECT SINGLE *
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  c_kschl
     AND matnr =  it_itab-matnr
     AND lifnr =  it_itab-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab =  it_itab-aedat.
  IF sy-subrc EQ 0.
    SELECT SINGLE kzust
      INTO it_itab-kzust
      FROM konh
     WHERE knumh = a018-knumh
       AND kschl = c_kschl.
  ENDIF.

  MOVE: it_itab-matnr+3(2) TO it_itab-mcode.
  APPEND it_itab.
  CLEAR: it_itab.
ENDFORM.                    " APPEND_ITAB
*&---------------------------------------------------------------------*
*&      Form  build_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_events.
  REFRESH gt_events.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = gt_events.

  PERFORM modify_gt_events
          TABLES  gt_events
          USING :
*            slis_ev_pf_status_set c_pss,
*            slis_ev_user_command  c_uc,
            slis_ev_top_of_page   c_top.
ENDFORM.                    " build_events
*&---------------------------------------------------------------------*
*&      Form  modify_gt_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_EVENTS  text
*      -->P_SLIS_EV_TOP_OF_PAGE  text
*      -->P_C_TOP  text
*----------------------------------------------------------------------*
FORM modify_gt_events TABLES p_events_t LIKE gt_events
                      USING  p_form p_value.

  DATA: ls_event TYPE slis_alv_event.

  READ TABLE  p_events_t  WITH KEY  name = p_form
                          INTO ls_event.
  IF sy-subrc EQ 0.
    MOVE     p_value     TO   ls_event-form.
    MODIFY   p_events_t  FROM ls_event INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " modify_gt_events

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0292   text
*----------------------------------------------------------------------*
FORM build_fieldcat USING p_intab.
  CLEAR   : gt_fieldcat, gt_fc.
  REFRESH : gt_fieldcat, gt_fc.

  g_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            i_program_name     = g_repid
            i_internal_tabname = p_intab
            i_inclname         = g_repid
       CHANGING
            ct_fieldcat        = gt_fc.

  PERFORM setting_fieldcat TABLES gt_fieldcat USING :
                                  'S' 'WERKS'       ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '04',
                                  'E' 'SELTEXT_L'   'PLANT',
*
                                  'S' 'MCODE'       ' ',
                                  ' ' 'JUST'        'C',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '02',
                                  'E' 'SELTEXT_L'   'Module Code' ,

                                  'S' 'LIFNR'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'Supplier',

                                  'S' 'NAME1'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '15',
                                  'E' 'SELTEXT_L'   'S/Description',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         'X',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '18',
                                  'E' 'SELTEXT_L'   'Module No',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '20',
                                  'E' 'SELTEXT_L'   'M/Description',

                                  'S' 'BUDAT'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'GR Date',

                                  'S' 'BWART'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '3',
                                  'E' 'SELTEXT_L'   'M/Type',

                                  'S' 'MENGE'    ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '16',
                                  'E' 'SELTEXT_L'   'GR Qty',

                                  'S' 'MEINS'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '3',
                                  'E' 'SELTEXT_L'   'Unit of measure',

                                  'S' 'DMBTR'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '16',
                                  'E' 'SELTEXT_L'   'Amount',

                                  'S' 'MBLNR'       ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'GR No',

                                  'S' 'ZEILE'    ' ',
                                  ' ' 'JUST'        'L',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '4',
                                  'E' 'SELTEXT_L'   'Item',

                                  'S' 'NETPR'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'Module Price',

                                  'S' 'PEINH'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '6',
                                  'E' 'SELTEXT_L'   'Price unit',

                                  'S' 'KZUST'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '3',
                                  'E' 'SELTEXT_L'   'Reason',

                                  'S' 'EKGRP'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '3',
                                  'E' 'SELTEXT_L'   'P/Group',

                                  'S' 'WAERS'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '4',
                                  'E' 'SELTEXT_L'   'Currency',

                                  'S' 'LFBNR'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'Reference Doc.',

                                  'S' 'LFPOS'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '4',
                                  'E' 'SELTEXT_L'   'Reference item',

                                  'S' 'LFBJA'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '4',
                                  'E' 'SELTEXT_L'   'Reference year',

                                  'S' 'EBELN'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '10',
                                  'E' 'SELTEXT_L'   'P/O Number',

                                  'S' 'EBELP'       ' ',
                                  ' ' 'JUST'        'R',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'DDICTXT'     'L',
                                  ' ' 'OUTPUTLEN'   '5',
                                  'E' 'SELTEXT_L'   'Item'.

ENDFORM.                    " build_fieldcat
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_FIELDCAT  text
*      -->P_0936   text
*      -->P_0937   text
*      -->P_0938   text
*----------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat LIKE gt_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.

  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR : g_fieldcat_s.
    READ TABLE gt_fc INTO g_fieldcat_s
                     WITH KEY fieldname  = p_field.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'G_FIELDCAT_S-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO cnt.
    g_fieldcat_s-col_pos = cnt.
    APPEND g_fieldcat_s TO p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  build_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0296   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_layout USING p_cb p_color p_sum.
  CLEAR gs_layout.

  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = ' '.
  gs_layout-default_item      = 'X'.
* check box
  IF p_cb = 'X'.
    gs_layout-box_fieldname    = 'CHKBOX'.
  ENDIF.
* line color
  IF p_color = 'X'.
    gs_layout-coltab_fieldname = 'COLOR'.
  ENDIF.
* sum
  IF p_sum = 'X'.
    gs_layout-totals_text       = 'TOT'.
  ENDIF.
ENDFORM.                    " build_layout
*&---------------------------------------------------------------------*
*&      Form  build_comment
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment USING    p_gt_header TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        ls_color TYPE slis_specialcol_alv,
        l_dsnam LIKE t024d-dsnam,
        l_h_dsnam LIKE t024d-dsnam,
        l_time(8),
        l_date(10),
        l_hdate(10).

* Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.
  APPEND ls_line TO p_gt_header.

* Sub-title of HEADER
  ls_line-typ  = 'S'.
  CONCATENATE text-h02 p_werks
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

  ls_line-typ  = 'S'.
  CONCATENATE text-h03 p_vtype
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

  ls_line-typ  = 'S'.
  CONCATENATE text-h07 s_mcode-low
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

  ls_line-typ  = 'S'.
  CONCATENATE text-h04 s_lifnr-low
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

  ls_line-typ  = 'S'.
  CONCATENATE text-h05 s_matnr-low
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

  ls_line-typ  = 'S'.
  CONCATENATE text-h06 s_ekgrp-low
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.

  ls_line-typ  = 'S'.
  WRITE  sy-datum  TO  l_date.
  CONCATENATE text-h08 l_date
         INTO ls_line-info SEPARATED BY ' '.
  APPEND ls_line TO p_gt_header.
ENDFORM.                    " build_comment
*&---------------------------------------------------------------------*
*&      Form  start_grid_viewer
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*----------------------------------------------------------------------*
FORM start_grid_viewer TABLES p_intab STRUCTURE zsmm_module_gr.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************

  wa_status_flag = 'BASE'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
*            i_background_id          = 'ALV_BACKGROUND' "HEADER? ??
*            i_bypassing_buffer       = 'X'
            i_callback_program       = g_repid
            i_callback_pf_status_set = 'SET_STATUS'
            is_layout                = gs_layout
            it_fieldcat              = gt_fieldcat[]
            i_callback_top_of_page   = 'TOP_OF_PAGE'
            i_callback_user_command  = 'USER_COMMAND'
            i_save                   = 'A'
            is_variant               = g_variant
            it_events                = gt_events[]
            is_print                 = gs_print
            it_list_commentary       = gt_header
       IMPORTING
            e_exit_caused_by_caller  = g_exit_caused_by_caller
            es_exit_caused_by_user   = gs_exit_caused_by_user
       TABLES
            t_outtab                 = p_intab.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " start_grid_viewer
*&---------------------------------------------------------------------*
*&      Form  CHECK_WERKS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_werks.
  SELECT SINGLE * FROM t001w WHERE werks = p_werks.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m07.
  ENDIF.
ENDFORM.                    " CHECK_WERKS
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM top_of_page.
  CASE   wa_status_flag.
    WHEN 'BASE'.
      CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
           EXPORTING
                it_list_commentary = gt_header.
    WHEN OTHERS.
      PERFORM display_detail_header.
*      CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*           EXPORTING
*                it_list_commentary = gt_header.
  ENDCASE.
ENDFORM.                    " top_of_page

*---------------------------------------------------------------------*
*       FORM set_status                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  CASE wa_status_flag.
    WHEN 'BASE'.
      SET PF-STATUS 'BASE'.
    WHEN 'DETAIL'.
      SET PF-STATUS 'DETAIL'.
  ENDCASE.
ENDFORM.                    "

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
  READ TABLE it_itab INDEX selfield-tabindex.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  CASE ucomm.
    WHEN '&DETAIL'.
      CALL SCREEN 9000.
    WHEN '&HISTORY'.
      PERFORM display_history_rtn.
    WHEN '&GR'.
      PERFORM display_gr_document.
    WHEN '&PO'.
      PERFORM display_po_document.
    WHEN '&REF'.
      PERFORM display_refrence_document.
    WHEN '&MO_INFO'.
      PERFORM display_module_info.
    WHEN '&IC1'.
      CASE selfield-fieldname.
        WHEN 'MATNR'.
          PERFORM display_module_info.
        WHEN 'MBLNR' OR 'ZEILE' OR 'MJAHR'.
          PERFORM display_gr_document.
        WHEN 'LFBNR' OR 'LFBJA' OR 'LFPOS'.
          PERFORM display_refrence_document.
        WHEN 'EBELN' OR 'EBELP'.
          PERFORM display_po_document.
        WHEN OTHERS.
          CALL SCREEN 9000.
      ENDCASE.
  ENDCASE.
ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  display_detail_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_rtn.
  CLEAR: gt_events,        gt_events[],
         gt_sort,          gt_sort[],
         gt_fieldcat,      gt_fieldcat[],
         gt_header_detail, gt_header_detail[].

  wa_status_flag = 'DETAIL'.
  PERFORM read_detail_data.
  PERFORM display_detail.

  wa_status_flag = 'BASE'.
ENDFORM.                    " display_detail_rtn
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat_detail USING p_intab.
ENDFORM.                    " build_fieldcat_detail
*&---------------------------------------------------------------------*
*&      Form  read_detail_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_detail_data.
  CLEAR: ztmm_assy_cost1.
  SELECT SINGLE * FROM ztmm_assy_cost1 WHERE vtype =  it_itab-matnr(3)
                                         AND mcode =  it_itab-matnr+3(2)
                                         AND lifnr =  it_itab-lifnr
                                         AND datab <= it_itab-aedat
                                         AND datbi >= it_itab-aedat.

  CLEAR: wa_sub, it_sub, it_sub[].
  EXEC SQL PERFORMING APPEND_SUB_DETAIL.
    SELECT /*+ ORDERED*/
           G.COMP,           C.PREF,           C.COMP,
           E.MAKTX,          C.QNTY,           C.UNIT,
           B.MEINS,          C.DATUV,          C.DATUB,
           C.STGB
      INTO :WA_SUB-UPGVC,    :WA_SUB-PREF,     :WA_SUB-COMP,
           :WA_SUB-MAKTX,    :WA_SUB-QNTY,     :WA_SUB-UNIT,
           :WA_SUB-MEINS,    :WA_SUB-DATAB,    :WA_SUB-DATBI,
           :WA_SUB-STGB
      FROM ZTBM_ABXDULDT G,
           ZTBM_ABXDULDT C, MARA B,  MAKT D,  MAKT E
     WHERE G.MANDT    =  :SY-MANDT
       AND G.MTNO     =  :IT_ITAB-MATNR
       AND G.PLNT     =  :P_WERKS
       AND G.USAG     =  '2'
       AND G.ALTN     =  '01'
       AND G.DATUV    <  :IT_ITAB-AEDAT
       AND G.DATUB    >= :IT_ITAB-AEDAT
       AND B.MANDT(+) =  G.MANDT
       AND B.MATNR(+) =  G.MTNO
       AND B.LVORM(+) =  ' '
       AND C.MANDT(+) =  G.MANDT
       AND C.MTNO(+)  =  G.COMP
       AND C.PLNT(+)  =  G.PLNT
       AND C.USAG(+)  =  '2'
       AND C.ALTN(+)  =  '01'
       AND C.DATUV(+) <  :IT_ITAB-AEDAT
       AND C.DATUB(+) >= :IT_ITAB-AEDAT
       AND D.MANDT(+) =  B.MANDT
       AND D.MATNR(+) =  B.MATNR
       AND D.SPRAS(+) =  :SY-LANGU
       AND E.MANDT(+) =  C.MANDT
       AND E.MATNR(+) =  C.COMP
       AND E.SPRAS(+) =  :SY-LANGU
     ORDER BY C.MTNO, C.PREF
  ENDEXEC.
ENDFORM.                    " read_detail_data
*&---------------------------------------------------------------------*
*&      Form  APPEND_SUB_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_sub_detail.
  DATA: lw_continue VALUE 'X'.

  PERFORM check_cockpit_module_color USING lw_continue.

  CHECK lw_continue EQ 'X'.

  PERFORM check_rtn.
  PERFORM append_sub_price.

  CLEAR: it_sub, wa_sub.
ENDFORM.                    " APPEND_SUB_DETAIL
*&---------------------------------------------------------------------*
*&      Form  alv_function_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_function_detail.

ENDFORM.                    " alv_function_detail
*&---------------------------------------------------------------------*
*&      Form  build_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_event.

ENDFORM.                    " build_event
*&---------------------------------------------------------------------*
*&      Form  build_sort_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sort_detail.

ENDFORM.                    " build_sort_detail
*&---------------------------------------------------------------------*
*&      Form  comment_build_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_HEADER_DETAIL[]  text
*----------------------------------------------------------------------*
FORM comment_build_detail USING  lt_top_of_page TYPE slis_t_listheader.

ENDFORM.                    " comment_build_detail
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
*----- Check Material Master
  IF wa_sub-maktx IS INITIAL.
    MOVE: c_no_matl TO wa_sub-sts.
    EXIT.
  ENDIF.

*----- Check Info Record Deletion
  CLEAR: eina, a018.
  SELECT SINGLE lifnr
    INTO wa_sub-lifnr
    FROM eina AS a INNER JOIN eine AS b
      ON a~infnr = b~infnr
   WHERE a~matnr = wa_sub-comp
     AND a~urzzt = 'SUB'
     AND a~loekz = ' '
     AND b~werks = ' '
     AND b~ekorg = c_ekorg
     AND b~loekz = ' '.
  IF sy-subrc NE 0.
    MOVE: c_no_info TO wa_sub-sts.
    EXIT.
  ENDIF.

*----- Read submaterial price
  SELECT SINGLE knumh
    INTO (a018-knumh)
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  wa_sub-comp
     AND lifnr =  wa_sub-lifnr
     AND ekorg =  c_ekorg
     AND esokz =  '0'
     AND datab <= it_itab-aedat
     AND datbi >= it_itab-aedat.
  IF sy-subrc NE 0.
    MOVE: c_no_cond TO wa_sub-sts.
    EXIT.
  ENDIF.

  SELECT SINGLE kbetr kpein kmein kzust
    INTO (wa_sub-netpr,wa_sub-peinh,
          wa_sub-kmein,wa_sub-kzust)
    FROM zvmm_info_condi
   WHERE knumh = a018-knumh
     AND kschl = c_kschl
     AND loevm_ko = ' '.
  IF sy-subrc NE 0.
    MOVE: c_no_cond TO wa_sub-sts.
    EXIT.
  ENDIF.

*----- A sub material's UoM must be 'EA'.
*----- If UoM is not 'EA', display error message.
  IF NOT ( ( wa_sub-meins EQ wa_sub-kmein AND
             wa_sub-meins EQ wa_sub-unit  AND
             wa_sub-kmein EQ wa_sub-unit )   ).
    MOVE: c_uom_err TO wa_sub-sts.
  ENDIF.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  append_sub_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_sub_price.
  CLEAR: it_sub.

  CASE wa_sub-sts.
    WHEN c_no_info.
      MOVE: text-b02 TO wa_sub-msg.
    WHEN c_no_cond.
      MOVE: text-b03 TO wa_sub-msg.
    WHEN c_uom_err.
      MOVE: text-b01 TO wa_sub-msg.
    WHEN c_no_matl.
      MOVE: text-b07 TO wa_sub-msg.
  ENDCASE.

  MOVE: it_itab-matnr(3) TO wa_sub-vtype,
        it_itab-matnr    TO wa_sub-matnr.

  IF it_sub-peinh EQ 0.
    it_sub-peinh = 1.
  ENDIF.

  MOVE: wa_sub TO it_sub.

  it_sub-amount = it_sub-qnty * it_sub-netpr / it_sub-peinh.

  APPEND it_sub.
  CLEAR: it_sub, wa_sub.
ENDFORM.                    " append_sub_price
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DETAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail.
  NEW-PAGE LINE-SIZE 138 LINE-COUNT 90.

  LOOP AT it_sub.
    NEW-LINE.

    IF sy-linno EQ 90. ULINE. ENDIF.

    AT NEW upgvc.
      PERFORM set_format_detail.
    ENDAT.

    PERFORM display_line_detail.

    AT NEW upgvc.
      PERFORM display_upgvc.
    ENDAT.

    AT LAST. ULINE. ENDAT.
  ENDLOOP.

  CLEAR: it_sub.
ENDFORM.                    " DISPLAY_DETAIL
*&---------------------------------------------------------------------*
*&      Form  set_format_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_format_detail.
  IF wa_format_flg EQ 'X'.
    CLEAR: wa_format_flg.
  ELSE.
    wa_format_flg = 'X'.
  ENDIF.
ENDFORM.                    " set_format_detail
*&---------------------------------------------------------------------*
*&      Form  display_line_detail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_line_detail.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 1      '|' NO-GAP,
          (18) space NO-GAP, '|' NO-GAP,
               it_sub-pref NO-GAP RIGHT-JUSTIFIED, '|' NO-GAP.

  IF it_sub-msg IS INITIAL.
    WRITE: (4) icon_green_light  AS ICON NO-GAP,'|' NO-GAP.
  ELSE.
    WRITE: (4) icon_red_light    AS ICON HOTSPOT NO-GAP,'|' NO-GAP.
  ENDIF.

  IF wa_format_flg EQ 'X'.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  WRITE: (18) it_sub-comp NO-GAP, '|' NO-GAP,
         (25) it_sub-maktx NO-GAP, '|' NO-GAP,
              it_sub-datab NO-GAP, '|' NO-GAP,
              it_sub-datbi NO-GAP, '|' NO-GAP,
         (04) it_sub-qnty UNIT it_sub-unit NO-GAP, '|' NO-GAP,
              it_sub-unit NO-GAP, '|' NO-GAP,
         (09) it_sub-netpr CURRENCY it_sub-waers NO-GAP,
              '|' NO-GAP,
              it_sub-peinh NO-GAP, '|' NO-GAP,
              it_sub-kzust NO-GAP, '|' NO-GAP,
         (10) it_sub-dmbtr CURRENCY it_sub-waers NO-GAP, '|'.

  HIDE: it_sub.
ENDFORM.                    " display_line_detail
*&---------------------------------------------------------------------*
*&      Form  display_upgvc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_upgvc.
  FORMAT COLOR COL_KEY INTENSIFIED ON.

  WRITE: 2(18) it_sub-upgvc.
ENDFORM.                    " display_upgvc
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DETAIL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_detail_header.
  DATA: lw_dmbtr LIKE mseg-dmbtr.

  lw_dmbtr = it_itab-netpr - ztmm_assy_cost1-asytr.

  WRITE: AT /1(sy-linsz) text-h13 CENTERED.

  SKIP.
  WRITE:/2   text-h03, (03) it_itab-matnr(3).
  WRITE:/2   text-h04,      it_itab-lifnr,      it_itab-name1,
         65  text-h14,      it_itab-netpr CURRENCY it_itab-waers.
  WRITE:/2   text-h05, (18) it_itab-matnr, (25) it_itab-maktx,
         65  text-h15,     ztmm_assy_cost1-asytr CURRENCY it_itab-waers,
         118 text-h18,      it_itab-aedat.
  WRITE:/2   text-h06, (18) it_itab-ekgrp,
         65  text-h16,      lw_dmbtr CURRENCY it_itab-waers,
         121 text-h19,      sy-uzeit.
  WRITE:/2   text-h08, (18) it_itab-budat,
         65  text-h17,      it_itab-waers,
         121 text-h20, (04) sy-pagno NO-GAP,'/',(4) wa_tot_page NO-ZERO.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  ULINE.
  WRITE:/    text-h21 NO-GAP, text-h22.
  ULINE.

  MOVE: sy-pagno TO wa_tot_page.
ENDFORM.                    " DISPLAY_DETAIL_HEADER
*&---------------------------------------------------------------------*
*&      Module  display_detail  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_detail OUTPUT.
  SUPPRESS DIALOG.
  LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
  SET PF-STATUS 'DETAIL'.

  PERFORM display_detail_rtn.
ENDMODULE.                 " display_detail  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-ucomm.
    WHEN '%EX' OR 'RW'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GR_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_gr_document.
  SET PARAMETER ID 'MBN' FIELD it_itab-mblnr.
  SET PARAMETER ID 'MJA' FIELD it_itab-mjahr.

  CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_GR_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PO_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_po_document.
  SET PARAMETER ID 'BES' FIELD it_itab-ebeln.

  CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_PO_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REFRENCE_DOCUMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_refrence_document.
  SET PARAMETER ID 'MBN' FIELD it_itab-lfbnr.
  SET PARAMETER ID 'MJA' FIELD it_itab-lfbja.

  CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_REFRENCE_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_HISTORY_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_history_rtn.
  SUBMIT zemmpm48r_module_assy_cost
    WITH s_vtype = it_itab-matnr(3)
    WITH s_mcode = it_itab-matnr+3(2)
     AND RETURN.
ENDFORM.                    " DISPLAY_HISTORY_RTN
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MODULE_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_module_info.
  SET PARAMETER ID 'LIF' FIELD it_itab-lifnr.
  SET PARAMETER ID 'MAT' FIELD it_itab-matnr.
  SET PARAMETER ID 'EKO' FIELD c_ekorg.
  SET PARAMETER ID 'WRK' FIELD ''.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_MODULE_INFO
*&---------------------------------------------------------------------*
*&      Form  display_sub_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_sub_info_record.
  CHECK NOT it_sub IS INITIAL.

  SET PARAMETER ID 'LIF' FIELD it_sub-lifnr.
  SET PARAMETER ID 'MAT' FIELD it_sub-comp.
  SET PARAMETER ID 'EKO' FIELD c_ekorg.
  SET PARAMETER ID 'WRK' FIELD ''.

  CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

  CLEAR: it_sub.
ENDFORM.                    " display_sub_info_record
*&---------------------------------------------------------------------*
*&      Form  excel_download_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_download_rtn.
  DATA: BEGIN OF lt_download OCCURS 0,
          upgvc LIKE zsmm_sub_detail-upgvc,
          sposn LIKE zsmm_sub_detail-sposn,
          idnrk LIKE zsmm_sub_detail-idnrk,
          maktx LIKE zsmm_sub_detail-maktx,
          kmpmg(16),
          kmpme LIKE zsmm_sub_detail-kmpme,
          datab(10),
          datbi(10),
          netpr(14),
          peinh LIKE zsmm_sub_detail-peinh,
          kzust LIKE zsmm_sub_detail-kzust,
          dmbtr(16),
          waers LIKE zsmm_sub_detail-waers,
          lifnr LIKE zsmm_sub_detail-lifnr,
          msg LIKE zsmm_sub_detail-msg,
        END   OF lt_download.

  LOOP AT it_sub.
    MOVE-CORRESPONDING it_sub TO lt_download.

    WRITE: it_sub-netpr CURRENCY it_sub-waers
                           TO lt_download-netpr,
           it_sub-dmbtr CURRENCY it_sub-waers
                           TO lt_download-dmbtr,
           it_sub-qnty  UNIT it_sub-meins
                           TO lt_download-kmpmg,
           it_sub-datab TO lt_download-datab,
           it_sub-datbi TO lt_download-datbi.

    APPEND lt_download.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename                = 'C:\TEMP\SUB_PRICE.XLS'
            filetype                = 'DAT'
       TABLES
            data_tab                = lt_download
       EXCEPTIONS
            invalid_filesize        = 1
            invalid_table_width     = 2
            invalid_type            = 3
            no_batch                = 4
            unknown_error           = 5
            gui_refuse_filetransfer = 6
            customer_error          = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CLEAR: it_sub.
ENDFORM.                    " excel_download_rtn
*&---------------------------------------------------------------------*
*&      Form  display_total_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_total_page.
  DO wa_tot_page TIMES.
    READ LINE 7 OF PAGE sy-index.
    MODIFY LINE 7 OF PAGE sy-index
                     FIELD VALUE wa_tot_page FROM wa_tot_page.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " display_total_page
*&---------------------------------------------------------------------*
*&      Form  DOUBLE_CLICK_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM double_click_rtn.
  DATA: lw_field(40).

  CHECK NOT it_sub IS INITIAL.

  GET CURSOR FIELD lw_field.

  CASE lw_field.
    WHEN 'IT_SUB-COMP'.
      PERFORM display_sub_info_record.
    WHEN 'ICON_RED_LIGHT'.
      MESSAGE s000(zz) WITH it_sub-msg(50) it_sub-msg+50(50).
  ENDCASE.

  CLEAR: it_sub.
ENDFORM.                    " DOUBLE_CLICK_RTN
*&---------------------------------------------------------------------*
*&      Form  check_cockpit_module_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_cockpit_module_color USING p_continue.
  DATA: lw_int_key(3).         " Internal Key Color

  CHECK it_itab-matnr+3(2) EQ 'CP' AND
        wa_sub-stgb        EQ 'U'.

  MOVE: it_itab-matnr+10(3) TO lw_int_key.

  SELECT SINGLE * FROM ztmm_cp_color WHERE copit EQ it_itab-matnr
                                       AND inkey EQ lw_int_key
                                       AND submt EQ wa_sub-comp
                                       AND datab <  it_itab-aedat
                                       AND datbi >= it_itab-aedat.
  IF sy-subrc NE 0.
    CLEAR: p_continue.
  ENDIF.
ENDFORM.                    " check_cockpit_module_color
