************************************************************************
* Program Name : ZRMMPM05R_GI_ABNORMAL
* Created by   : Min-su Park
* Created on   : 2003.08.07.
* Pattern      :
* Description  : GI(abnormal) List
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.08.08.     Min-su Park      UD1K901873     Initial Coding       *
*&---------------------------------------------------------------------*
* Date            Developer        RequestNo      Description
* 2004.02.04.     Jaesung Lee     UD1K906987    Changed condition logic
* Condition logic changed: plant leavel =>  Purchasing group
* a017 table => a018 table
* 10/03/2006      Manju           UD1K922375    Add Supply area to
*                                               selection screen &
*                                               Report Output.
*&---------------------------------------------------------------------*

REPORT  zrmmpm05r_gi_abnormal.

*Definition Table         .
TABLES : zvmm_giabnormal, mkpf, mseg, marc, v_fmifi.

*ALV Definition.
TYPE-POOLS: slis.
DATA:   wa_events      TYPE slis_t_event                              ,
        w_repid LIKE sy-repid                                         ,
        wa_sort     TYPE slis_t_sortinfo_alv                          ,
        it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE         ,
        w_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE' ,
        wa_list_top_of_page TYPE slis_t_listheader                    ,
        w_print_p TYPE slis_print_alv                                 .

DATA : it_zsmm_rmmpm05 LIKE zsmm_rmmpm05 OCCURS 0 WITH HEADER LINE.

* Begin of changes - UD1K922375
data : begin of IT_RCODE occurs 0,
          BWART like T157D-bwart,
          GRUND like T157D-GRUND,
          GRTXT like T157E-GRTXT,
        end of IT_RCODE.
* End of changes - UD1K922375


DATA : w_layout   TYPE slis_layout_alv,
       w_line TYPE slis_listheader,
       w_top_of_page TYPE slis_t_listheader.

*--- Macro
DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
END-OF-DEFINITION.


SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS :
      s_werks FOR mseg-werks OBLIGATORY, "Plant
      s_lgort FOR mseg-lgort OBLIGATORY, "Storage Location
      s_mtart FOR zvmm_giabnormal-mtart, "Material Type
      s_mblnr FOR mseg-mblnr           , "GI No.
      s_budat FOR mkpf-budat           , "GI Date.
      s_matnr FOR mseg-matnr           , "Material No.
      s_dispo FOR marc-dispo OBLIGATORY, "Manager
      s_bwart FOR mseg-bwart           , "Mvmt Type
      s_grund FOR mseg-grund           , "Reason Code
      s_sakto FOR mseg-sakto           , "Account
      s_kostl FOR mseg-kostl           , "Cost Center
      s_fipex FOR v_fmifi-fipex        , "Budget
      s_wempf FOR mseg-wempf,           "OS&D Tag No.
      s_VSPVB for MARC-VSPVB,           "Supply Area / UD1K922375
      S_AUFNR for MSEG-AUFNR.           "Order Number
SELECTION-SCREEN END OF BLOCK block1.


*---
INITIALIZATION.
  s_mtart-low = 'ROH'.
  APPEND s_mtart.

*---
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_bwart-low.
  PERFORM possble_bwart.

*---
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_bwart-high.
  PERFORM possble_bwart.

*---
START-OF-SELECTION.
  PERFORM make_basic_data.
  PERFORM alv_field_build.

  SORT it_zsmm_rmmpm05 BY werks lgort bwart grund matnr.

*** print paramter   ****************************************
  w_print_p-no_coverpage = 'X'.
  w_print_p-no_print_listinfos = 'X'.
  w_print_p-no_change_print_params = 'X'.
  w_print_p-no_print_selinfos = 'X'.
*************************************************************

  w_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = w_repid
            i_structure_name   = 'ZSMM_RMMPM05'
*            it_sort            = wa_sort[]
            it_events          = wa_events[]
            is_layout          = w_layout
            it_fieldcat        = it_fieldcat[]
            is_print           = w_print_p
       TABLES
            t_outtab           = it_zsmm_rmmpm05.

END-OF-SELECTION.
  CLEAR : s_bwart, s_bwart[].





*&---------------------------------------------------------------------*
*&      Form  MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_basic_data.
*---
  DATA : w_rem TYPE i.

  READ TABLE s_bwart INDEX 1.

  IF sy-subrc <> 0.
    PERFORM get_mvt_type_condition.
  ENDIF.

  LOOP AT s_werks.
** Furong on 02/08/12
*    IF s_werks-low = 'E001' OR s_werks-high = 'E001'.
    IF s_werks-low = 'E001' OR s_werks-high = 'E001'
    or s_werks-low = 'E002' OR s_werks-high = 'E002'.
** End
      LOOP AT s_lgort.
        IF s_mtart-low = 'ROH' OR s_mtart-high = 'ROH'.
        s_mtart-sign = 'I'. s_mtart-option = 'EQ'. s_mtart-low = 'HALB'.
          APPEND s_mtart.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*---
* Read Reason Code Text
  select BWART GRUND GRTXT into table it_RCODE   "UD1K922375
         from T157E where SPRAS = sy-langu.      "UD1K922375

  CLEAR : it_zsmm_rmmpm05[].

  SELECT * FROM zvmm_giabnormal
           INTO CORRESPONDING FIELDS OF TABLE it_zsmm_rmmpm05
          WHERE werks  IN s_werks  "Plnat
            AND lgort  IN s_lgort  "Storage Location
            AND mtart  IN s_mtart  "Material Type
            AND mblnr  IN s_mblnr  "GI No.
            AND budat  IN s_budat  "GI Date.
            AND matnr  IN s_matnr  "Material No.
            AND dispo  IN s_dispo  "Manager
            AND bwart  IN s_bwart  "Mvmt Type
            AND grund  IN s_grund  "Reason Code
            AND sakto  IN s_sakto  "Account
            AND kostl  IN s_kostl  "Cost Center
            AND wempf  IN s_wempf  "OS&D Tag No
            and VSPVB  IN s_VSPVB  "Supply Area - UD1K922375
            and AUFNR  in S_AUFNR. "Order Number- UD1K922375

*---
  LOOP AT it_zsmm_rmmpm05.
    READ TABLE s_fipex INDEX 1.
    IF sy-subrc = 0.
      SELECT SINGLE fipex
               INTO it_zsmm_rmmpm05-fipex
               FROM v_fmifi
              WHERE fistl NOT IN ('BS', 'PL')
                AND fipex IN s_fipex
                AND awref = it_zsmm_rmmpm05-mblnr.
      IF sy-subrc <> 0.
        DELETE it_zsmm_rmmpm05.
        CONTINUE.
      ENDIF.
    ELSE.
      SELECT SINGLE fipex
            INTO it_zsmm_rmmpm05-fipex
            FROM v_fmifi
           WHERE fistl NOT IN ('BS', 'PL')
             AND fipex IN s_fipex
             AND awref = it_zsmm_rmmpm05-mblnr.
    ENDIF.
*Get Unit Price
    DATA : knumh LIKE a017-knumh.
*&<<<<-----------------------------------------------------------------*
* insert by jaesung lee on 04/02/2004
* business logic changed . table a017 = > a018
*&<<<<-----------------------------------------------------------------*
    SELECT SINGLE knumh
             INTO knumh
             FROM a018
            WHERE kappl = 'M'
              AND kschl = 'PB00'
              AND matnr = it_zsmm_rmmpm05-matnr
*               AND WERKS = IT_ZSMM_RMMPM05-WERKS
              AND datab >= it_zsmm_rmmpm05-budat
              AND datbi <= it_zsmm_rmmpm05-budat.


    SELECT SINGLE kbetr
             INTO it_zsmm_rmmpm05-netpr
             FROM konp
            WHERE kappl = 'M'
              AND kschl = 'PB00'
              AND knumh = knumh.

    w_rem = it_zsmm_rmmpm05-bwart MOD 2.
    IF w_rem = 0.
      it_zsmm_rmmpm05-menge = it_zsmm_rmmpm05-menge * -1.
      it_zsmm_rmmpm05-dmbtr = it_zsmm_rmmpm05-dmbtr * -1.
    ENDIF.
* Read Reason Code TEXT Begin of changes - UD1K922375
    read table it_RCODE with key bwart = it_zsmm_rmmpm05-bwart
                                 grund = it_zsmm_rmmpm05-grund.
    if sy-subrc eq 0.
      it_zsmm_rmmpm05-GRTXT   = it_rcode-GRTXT.
    endif.             "END  of changes - UD1K922375


    MODIFY it_zsmm_rmmpm05.
  ENDLOOP.

*---
  SORT it_zsmm_rmmpm05 BY werks lgort bwart grund matnr.
ENDFORM.                    " MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_field_build.
  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[], wa_list_top_of_page[].
  PERFORM fieldcat_init  USING it_fieldcat[].
  PERFORM eventtab_build USING wa_events[].
  PERFORM comment_build  USING wa_list_top_of_page[].
*  PERFORM sort_build     USING wa_sort[].
ENDFORM.                    " ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: pos TYPE i.
*GI No.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MBLNR'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'GI No.'.
  ls_fieldcat-seltext_m     = 'GI No.'.
  ls_fieldcat-seltext_s     = 'GI No.'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Item No.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ZEILE'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Item No.'.
  ls_fieldcat-seltext_m     = 'Item No.'.
  ls_fieldcat-seltext_s     = 'Item No.'.
  ls_fieldcat-outputlen     = '4'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Plant.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'WERKS'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = 'X'.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Plant'.
  ls_fieldcat-seltext_m     = 'Plant'.
  ls_fieldcat-seltext_s     = 'Plant'.
  ls_fieldcat-outputlen     = '4'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Storage Location.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'LGORT'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = 'X'.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'S/Loc'.
  ls_fieldcat-seltext_m     = 'S/Loc'.
  ls_fieldcat-seltext_s     = 'S/Loc'.
  ls_fieldcat-outputlen     = '4'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*POC
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DISPO'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'POC'.
  ls_fieldcat-seltext_m     = 'POC'.
  ls_fieldcat-seltext_s     = 'POC'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

* Begin of changes - UD1K922375
*Supply Area
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'VSPVB'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Supply Area'.
  ls_fieldcat-seltext_m     = 'Supply Area'.
  ls_fieldcat-seltext_s     = 'Supply Area'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.
* End of changes - UD1K922375

*Mvmt Type
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'BWART'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = 'X'.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Mvt'.
  ls_fieldcat-seltext_m     = 'Mvt'.
  ls_fieldcat-seltext_s     = 'Mvt'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Material No.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MATNR'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Material No'.
  ls_fieldcat-seltext_m     = 'Material No'.
  ls_fieldcat-seltext_s     = 'Material No'.
  ls_fieldcat-outputlen     = '18'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Material Description.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
* LS_FIELDCAT-TABNAME       = 'IT_ZSMM_RMMPM05'.
  ls_fieldcat-fieldname     = 'MAKTX'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Material Description'.
  ls_fieldcat-seltext_m     = 'Material Description'.
  ls_fieldcat-seltext_s     = 'Material Description'.
  ls_fieldcat-outputlen     = '40'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*GI Qty
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MENGE'.
  ls_fieldcat-ref_fieldname = 'ZMENGE_D'.
  ls_fieldcat-do_sum        = 'X'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = 'MEINS'.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'GI Qty'.
  ls_fieldcat-seltext_m     = 'GI Qty'.
  ls_fieldcat-seltext_s     = 'GI Qty'.
*  LS_FIELDCAT-ROLLNAME      = 'GI Qty'.
  ls_fieldcat-outputlen     = '13'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.


*UM
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MEINS'.
  ls_fieldcat-ref_fieldname = 'ZMEINS'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'UM'.
  ls_fieldcat-seltext_m     = 'UM'.
  ls_fieldcat-seltext_s     = 'UM'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Unit Price
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'NETPR'.
  ls_fieldcat-ref_fieldname = 'ZNETPR'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = 'WAERS'.
  ls_fieldcat-seltext_l     = 'Unit Price'.
  ls_fieldcat-seltext_m     = 'Unit Price'.
  ls_fieldcat-seltext_s     = 'Unit Price'.
  ls_fieldcat-outputlen     = '11'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*GI Amount
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DMBTR'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-do_sum        = 'X'.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = 'WAERS'.
  ls_fieldcat-seltext_l     = 'GI Amount'.
  ls_fieldcat-seltext_m     = 'GI Amount'.
  ls_fieldcat-seltext_s     = 'GI Amount'.
  ls_fieldcat-outputlen     = '13'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Reason Code
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'GRUND'.
  ls_fieldcat-ref_fieldname = 'GRUND'.
  ls_fieldcat-key           = 'X'.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'RC'.
  ls_fieldcat-seltext_m     = 'RC'.
  ls_fieldcat-seltext_s     = 'RC'.
  ls_fieldcat-outputlen     = '4'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*  Begin of changes - UD1K922375
* Reason Code Text
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'GRTXT'.
  ls_fieldcat-ref_fieldname = 'GRTXT'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'RC TEXT'.
  ls_fieldcat-seltext_m     = 'RC TEXT'.
  ls_fieldcat-seltext_s     = 'RC TEXT'.
  ls_fieldcat-outputlen     = '20'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.
*  End of changes - UD1K922375

*OS&D Tag No.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'WEMPF'.
  ls_fieldcat-ref_fieldname = 'ZWEMPF'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'OS&D Tag No'.
  ls_fieldcat-seltext_m     = 'OS&D Tag No'.
  ls_fieldcat-seltext_s     = 'OS&D Tag No'.
  ls_fieldcat-outputlen     = '12'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Cost Center
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'KOSTL'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Cost Center'.
  ls_fieldcat-seltext_m     = 'Cost Center'.
  ls_fieldcat-seltext_s     = 'Cost Center'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.


* Begin of changes - UD1K922375
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'AUFNR'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Order Number'.
  ls_fieldcat-seltext_m     = 'Order Number'.
  ls_fieldcat-seltext_s     = 'Order Number'.
  ls_fieldcat-outputlen     = '12'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.
* end of changes -  UD1K922375

*Account
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
* LS_FIELDCAT-TABNAME       = 'IT_ZSMM_RMMPM05'.
  ls_fieldcat-fieldname     = 'SAKTO'.
  ls_fieldcat-ref_fieldname = 'ZSAKTO'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Account'.
  ls_fieldcat-seltext_m     = 'Account'.
  ls_fieldcat-seltext_s     = 'Account'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Budget
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
* LS_FIELDCAT-TABNAME       = 'IT_ZSMM_RMMPM05'.
  ls_fieldcat-fieldname     = 'FIPEX'.
  ls_fieldcat-ref_fieldname = 'ZFM_FIPEX'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Budget'.
  ls_fieldcat-seltext_m     = 'Budget'.
  ls_fieldcat-seltext_s     = 'Budget'.
  ls_fieldcat-outputlen     = '24'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*GI Date
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'BUDAT'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'GI Date'.
  ls_fieldcat-seltext_m     = 'GI Date'.
  ls_fieldcat-seltext_s     = 'GI Date'.
  ls_fieldcat-outputlen     = '8'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Doc Creation Date
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'BLDAT'.
  ls_fieldcat-ref_fieldname = ''.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Created'.
  ls_fieldcat-seltext_m     = 'Created'.
  ls_fieldcat-seltext_s     = 'Created'.
  ls_fieldcat-outputlen     = '8'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Manager
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
* LS_FIELDCAT-TABNAME       = 'IT_ZSMM_RMMPM05'.
  ls_fieldcat-fieldname     = 'USNAM'.
  ls_fieldcat-ref_fieldname = 'ZUSNAM'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Created by'.
  ls_fieldcat-seltext_m     = 'Created by'.
  ls_fieldcat-seltext_s     = 'Created by'.
  ls_fieldcat-outputlen     = '12'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Curr
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
* LS_FIELDCAT-TABNAME       = 'IT_ZSMM_RMMPM05'.
  ls_fieldcat-fieldname     = 'WAERS'.
  ls_fieldcat-no_out        = 'X'.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Curr
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
* LS_FIELDCAT-TABNAME       = 'IT_ZSMM_RMMPM05'.
  ls_fieldcat-fieldname     = 'FISTL'.
  ls_fieldcat-no_out        = 'X'.
  APPEND ls_fieldcat TO  rt_fieldcat.

ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM eventtab_build USING lt_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = lt_events.

  READ TABLE lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE w_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO lt_events.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-100.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-002 s_werks-low s_werks-high,
      'S' text-003 s_lgort-low s_lgort-high,
      'S' text-004 s_mblnr-low s_mblnr-high,
      'S' text-005 s_budat-low s_budat-high,
      'S' text-006 s_matnr-low s_matnr-high,
      'S' text-007 s_dispo-low s_dispo-high,
      'S' text-008 s_bwart-low s_bwart-high,
      'S' text-009 s_grund-low s_grund-high,
      'S' text-010 s_sakto-low s_sakto-high,
      'S' text-011 s_kostl-low s_kostl-high.
ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
*---
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = w_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  POSSBLE_BWART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM possble_bwart.
*---
  DATA : it_return LIKE ddshretval OCCURS 10 WITH HEADER LINE.
  DATA : it_dfies LIKE dfies OCCURS 10 WITH HEADER LINE.
  DATA : BEGIN OF it_bwart OCCURS 0,
           bwart LIKE t156t-bwart,
           sobkz LIKE t156t-sobkz,
           btext LIKE t156t-btext,
           index(04)             ,
         END OF it_bwart.

  SELECT * FROM t156t
           INTO CORRESPONDING FIELDS OF TABLE it_bwart
          WHERE kzvbr = space
            AND bwart IN ('201', '202', '309', '310',
                          '551', '552', '555', '556',
                          '905', '906')
            AND spras = sy-langu.

  LOOP AT it_bwart.
    CONCATENATE it_bwart-bwart it_bwart-sobkz INTO it_bwart-index.
    MODIFY it_bwart.
  ENDLOOP.

  CLEAR: it_dfies, it_dfies[].
  it_dfies-fieldname = 'BWART'.
  it_dfies-position  = 1.
  it_dfies-offset    = 0.
  it_dfies-intlen    = 3.
  it_dfies-outputlen = 3.
  it_dfies-scrtext_s = 'MvT'.
  APPEND it_dfies.
  CLEAR it_dfies.

  CLEAR: it_dfies.
  it_dfies-fieldname = 'SOBKZ'.
  it_dfies-position  = 2.
  it_dfies-offset    = 3.
  it_dfies-intlen    = 1.
  it_dfies-outputlen = 1.
  it_dfies-scrtext_s = 'S'.
  APPEND it_dfies.
  CLEAR it_dfies.

  CLEAR: it_dfies.
  it_dfies-fieldname = 'BTEXT'.
  it_dfies-position  = 3.
  it_dfies-offset    = 4.
  it_dfies-intlen    = 20.
  it_dfies-outputlen = 20.
  it_dfies-scrtext_s = 'Text'.
  APPEND it_dfies.
  CLEAR it_dfies.

  CLEAR: it_dfies.
  it_dfies-fieldname = 'INDEX'.
  it_dfies-position  = 4.
  it_dfies-offset    = 24.
  it_dfies-intlen    = 4.
  it_dfies-outputlen = 4.
  it_dfies-scrtext_s = 'INDEX'.
  APPEND it_dfies.
  CLEAR it_dfies.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*   DDIC_STRUCTURE         = ' '
      retfield               = 'BWART'
*   PVALKEY                = ' '
      dynpprog               = 'ZRMMPM06R_MON_GI_ABNORMAL'
      dynpnr                 = sy-dynnr
      dynprofield            = 'BWART'
*   STEPL                  = 0
*   WINDOW_TITLE           =
*   VALUE                  = ' '
      value_org              = 'S'
*   MULTIPLE_CHOICE        = ' '
*   DISPLAY                = ' '
*   CALLBACK_PROGRAM       = ' '
*   CALLBACK_FORM          = ' '
    TABLES
      value_tab              = it_bwart
      field_tab              = it_dfies
*   RETURN_TAB             =
*   DYNPFLD_MAPPING        =
* EXCEPTIONS
*   PARAMETER_ERROR        = 1
*   NO_VALUES_FOUND        = 2
*   OTHERS                 = 3
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " POSSBLE_BWART
*&---------------------------------------------------------------------*
*&      Form  SORT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SORT[]  text
*----------------------------------------------------------------------*
FORM sort_build USING  e06_lt_sort TYPE slis_t_sortinfo_alv.
  DATA: ls_sort TYPE slis_sortinfo_alv.
  ls_sort-fieldname = 'WERKS'.
  ls_sort-spos      = 1.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
*  LS_SORT-GROUP     = 'G1'.
  APPEND ls_sort TO e06_lt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'LGORT'.
  ls_sort-spos      = 2.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
*  LS_SORT-GROUP     = 'G1'.
  APPEND ls_sort TO e06_lt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'BWART'.
  ls_sort-spos      = 3.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
*  LS_SORT-GROUP     = 'G1'.
  APPEND ls_sort TO e06_lt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'GRUND'.
  ls_sort-spos      = 4.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
*  LS_SORT-GROUP     = 'G1'.
  APPEND ls_sort TO e06_lt_sort.

  CLEAR ls_sort.
  ls_sort-fieldname = 'MATNR'.
  ls_sort-spos      = 5.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
*  LS_SORT-GROUP     = 'G1'.
  APPEND ls_sort TO e06_lt_sort.
ENDFORM.                    " SORT_BUILD
*&---------------------------------------------------------------------*
*&      Form  GET_MVT_TYPE_CONDITION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_mvt_type_condition.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '201'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '202'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '309'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '310'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '551'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '552'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '555'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '556'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '905'.
  APPEND s_bwart.
  s_bwart-sign = 'I'. s_bwart-option = 'EQ'. s_bwart-low = '906'.
  APPEND s_bwart.
ENDFORM.                    " GET_MVT_TYPE_CONDITION
