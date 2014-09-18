************************************************************************
* Program Name : ZRMMPM16R_ITEMMASTER
* Created by   : Min-su Park
* Created on   : 2003.08.22.
* Pattern      :
* Description  : Item Master List
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.08.25.     Min-su Park      UD1K901873     Initial Coding       *
************************************************************************

REPORT  zrmmpm16r_itemmaster.

*ALV Definition.
TYPE-POOLS: slis.

DATA:   wa_events   TYPE slis_t_event       ,
        w_repid LIKE sy-repid               ,
        wa_sort     TYPE slis_t_sortinfo_alv,
        it_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
        w_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE',
        wa_list_top_of_page TYPE slis_t_listheader.

*Data Definition.
TABLES : mara, mard, stpo, stas, marc, mlgt, ztmm_mast.

*---
DATA   : BEGIN OF it_itemmaster OCCURS 0,
           matnr LIKE zvmm_itemmaster-matnr, "Material No.
           maktx LIKE zvmm_itemmaster-maktx, "Material Description
           bismt LIKE zvmm_itemmaster-bismt, "ALC/HPC Code
           datuv LIKE stpob-datuv          , "Effective in*
           datub LIKE stpob-datub          , "Effective out*
           lgtyp LIKE zvmm_itemmaster-lgtyp, "Storage Type
           zline LIKE zvmm_itemmaster-zline, "Line
           ztime LIKE zvmm_itemmaster-ztime, "W/S
           rh_lh LIKE zvmm_itemmaster-rh_lh, "RH/LH
           profl LIKE zvmm_itemmaster-profl, "Source
           lgbzo LIKE ekpo-lgbzo           , "Group Gate/Gate*
           lifnr LIKE eina-lifnr           , "Vendor*
*          Feeding Cycle ?
*          Feeding Type  ?
           bstme LIKE zvmm_itemmaster-bstme, "Cont. Type
           menge LIKE stpo-menge           , "Usage*
           breit LIKE zvmm_itemmaster-breit, "W
           laeng LIKE zvmm_itemmaster-laeng, "L
           hoehe LIKE zvmm_itemmaster-hoehe, "H
           umren LIKE marm-umren           , "QTY/Cont*
           werks LIKE marc-werks           ,
           mtart LIKE mara-mtart           ,
           lgort LIKE mard-lgort           ,
           feedr LIKE  ztmm_mast-feedr     ,
         END OF it_itemmaster.

DATA : subrc LIKE sy-subrc.

DATA : w_layout   TYPE slis_layout_alv.


*SELECT-OPTIONS.
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_werks FOR marc-werks           , "Plant
                 s_matnr FOR mara-matnr           , "Material
                 s_mtart FOR mara-mtart           , "Material Type
                 s_lgort FOR mard-lgort           , "Storage Location
                 s_lgtyp FOR mlgt-lgtyp           , "Storage Type
                 s_feedr FOR ztmm_mast-feedr      , "Feeder
                 s_zline FOR ztmm_mast-zline      , "Line
                 s_ztime FOR ztmm_mast-ztime      , "W/S
                 s_profl FOR mara-profl           . "Source
SELECTION-SCREEN END OF BLOCK block1.


*EVENTS.
INITIALIZATION.
  s_mtart-low = 'ROH'.
  APPEND s_mtart.

AT SELECTION-SCREEN.
  PERFORM make_basic_data.
  PERFORM alv_field_build.

START-OF-SELECTION.

  MOVE : 'X' TO w_layout-colwidth_optimize.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program = w_repid
            it_events          = wa_events[]
            it_fieldcat        = it_fieldcat[]
       TABLES
            t_outtab           = it_itemmaster.


*&---------------------------------------------------------------------*
*&      Form  MAKE_BASIC_DATA
*&-------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_basic_data.
*GET View Data.
  CLEAR it_itemmaster[].
  SELECT * FROM mara
           INTO CORRESPONDING FIELDS OF TABLE it_itemmaster
          WHERE matnr IN s_matnr
            AND mtart IN s_mtart
            AND profl IN s_profl.

*GET Another Data.
  LOOP AT it_itemmaster.
*   SELECTION CHK
    PERFORM selection_chk.
    CHECK subrc = 0.
*   GET Material Txt
    PERFORM get_material_txt.
*   GET Effetive in/out Data.
    PERFORM get_effective_io.
*   GET Group Gate/Gate Data. "LGBZO
    PERFORM get_gate_data.
*   GET Vendor Data.
    PERFORM get_vendor_data.
*   GET Usage Data.
    PERFORM get_usage_data.
**   GET Lgtyp ,Table ZTMM_MAST data
*    PERFORM GET_LGTYP_ZTMM_MAST.
*   GET QTY/Cont Data.
    PERFORM get_qty_cont.
    MODIFY it_itemmaster.
    CLEAR it_itemmaster.
  ENDLOOP.
ENDFORM.                    " MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_EFFECTIVE_IO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_effective_io.

  DATA BEGIN OF stpob_tmp OCCURS 1.
          INCLUDE STRUCTURE stpob.
  DATA END   OF stpob_tmp.
  CLEAR : stpo, stas.
  SELECT SINGLE * FROM stpo
                 WHERE idnrk = it_itemmaster-matnr
                   AND lkenz <> 'X'.
  CHECK sy-subrc = 0.
  SELECT SINGLE stlal FROM stas
                      INTO stas-stlal
                     WHERE stlty = stpo-stlty
                       AND stlnr = stpo-stlnr
                       AND stlkn = stpo-stlkn
                       AND stasz = stpo-stpoz.
  CHECK sy-subrc = 0.
  CALL FUNCTION 'CSGU_READ_STPO'
       EXPORTING
            estlty = stpo-stlty
            estlnr = stpo-stlnr
            estlal = stas-stlal
            estvkn = stpo-stvkn
            edatuv = stpo-datuv
       TABLES
            wa     = stpob_tmp.
  IF sy-subrc EQ 0.
    READ TABLE stpob_tmp INDEX 1.
    it_itemmaster-datuv = stpob_tmp-datuv.
    it_itemmaster-datub = stpob_tmp-datub.
  ENDIF.
ENDFORM.                    " GET_EFFECTIVE_IO
*&---------------------------------------------------------------------*
*&      Form  GET_GATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_gate_data.
  DATA : lgbzo LIKE ekpo-lgbzo.
  SELECT SINGLE lgbzo
         INTO lgbzo
         FROM ekpo
        WHERE matnr = it_itemmaster-matnr
          AND ebeln IN ( select BSART
                            from EKKO
                           WHERE bsart = 'SA' ).
  IF sy-subrc = 0. it_itemmaster-lgbzo = lgbzo. ENDIF.
ENDFORM.                    " GET_GATE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_VENDOR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_vendor_data.
  DATA : lifnr LIKE eina-lifnr.
  SELECT SINGLE lifnr
           INTO lifnr
           FROM eina
          WHERE matnr = it_itemmaster-matnr
            AND loekz <> 'X'.
  IF sy-subrc = 0. it_itemmaster-lifnr = lifnr. ENDIF.
ENDFORM.                    " GET_VENDOR_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_USAGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_usage_data.
  CLEAR : stpo.
  SELECT SINGLE menge INTO it_itemmaster-menge
                      FROM stpo
                     WHERE idnrk = it_itemmaster-matnr.
ENDFORM.                    " GET_USAGE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_QTY_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_qty_cont.
  SELECT SINGLE umren
          INTO it_itemmaster-umren
          FROM marm
         WHERE matnr = it_itemmaster-matnr
           AND meinh = it_itemmaster-bstme.
ENDFORM.                    " GET_QTY_CONT
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
*  PERFORM BUILD   USING WA_SORT[].
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

*Material Number
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MATNR'.
  ls_fieldcat-ref_fieldname = 'MATNR'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Material Number'.
  ls_fieldcat-seltext_m     = 'Material Number'.
  ls_fieldcat-seltext_s     = 'Material Number'.
  ls_fieldcat-outputlen     = '18'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Material Description
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MAKTX'.
  ls_fieldcat-ref_fieldname = 'MAKTX'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Material Description'.
  ls_fieldcat-seltext_m     = 'Material Description'.
  ls_fieldcat-seltext_s     = 'Material Description'.
  ls_fieldcat-outputlen     = '30'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*ALC/HPC Code
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'BISMT'.
  ls_fieldcat-ref_fieldname = 'BISMT'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'ALC/HPC Code'.
  ls_fieldcat-seltext_m     = 'ALC/HPC Code'.
  ls_fieldcat-seltext_s     = 'ALC/HPC Code'.
  ls_fieldcat-outputlen     = '18'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Effective in
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DATUV'.
  ls_fieldcat-ref_fieldname = 'DATUV'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Effective in'.
  ls_fieldcat-seltext_m     = 'Effective in'.
  ls_fieldcat-seltext_s     = 'Effective in'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Effective out
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'DATUB'.
  ls_fieldcat-ref_fieldname = 'DATUB'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Effective out'.
  ls_fieldcat-seltext_m     = 'Effective out'.
  ls_fieldcat-seltext_s     = 'Effective out'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Storage Type
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'LGTYP'.
  ls_fieldcat-ref_fieldname = 'LGTYP'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Storage Type'.
  ls_fieldcat-seltext_m     = 'Storage Type'.
  ls_fieldcat-seltext_s     = 'Storage Type'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Line
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ZLINE'.
  ls_fieldcat-ref_fieldname = 'ZLINE'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Line'.
  ls_fieldcat-seltext_m     = 'Line'.
  ls_fieldcat-seltext_s     = 'Line'.
  ls_fieldcat-outputlen     = '1'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*W/S
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ZTIME'.
  ls_fieldcat-ref_fieldname = 'ZTIME'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'W/S'.
  ls_fieldcat-seltext_m     = 'W/S'.
  ls_fieldcat-seltext_s     = 'W/S'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*LH/RH
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'RH_LH'.
  ls_fieldcat-ref_fieldname = 'RH_LH'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'LH/RH'.
  ls_fieldcat-seltext_m     = 'LH/RH'.
  ls_fieldcat-seltext_s     = 'LH/RH'.
  ls_fieldcat-outputlen     = '2'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Source
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'PROFL'.
  ls_fieldcat-ref_fieldname = 'PROFL'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Source'.
  ls_fieldcat-seltext_m     = 'Source'.
  ls_fieldcat-seltext_s     = 'Source'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Gate
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'LGBZO'.
  ls_fieldcat-ref_fieldname = 'LGBZO'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Gate'.
  ls_fieldcat-seltext_m     = 'Gate'.
  ls_fieldcat-seltext_s     = 'Gate'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Vendor
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'LIFNR'.
  ls_fieldcat-ref_fieldname = 'LIFNR'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Vendor'.
  ls_fieldcat-seltext_m     = 'Vendor'.
  ls_fieldcat-seltext_s     = 'Vendor'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Cont.Type
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'BSTME'.
  ls_fieldcat-ref_fieldname = 'BSTME'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Cont.Type'.
  ls_fieldcat-seltext_m     = 'Cont.Type'.
  ls_fieldcat-seltext_s     = 'Cont.Type'.
  ls_fieldcat-outputlen     = '3'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Usage
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'MENGE'.
  ls_fieldcat-ref_fieldname = 'MENGE'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Usage'.
  ls_fieldcat-seltext_m     = 'Usage'.
  ls_fieldcat-seltext_s     = 'Usage'.
  ls_fieldcat-outputlen     = '15'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*W
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'BREIT'.
  ls_fieldcat-ref_fieldname = 'BREIT'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'W'.
  ls_fieldcat-seltext_m     = 'W'.
  ls_fieldcat-seltext_s     = 'W'.
  ls_fieldcat-outputlen     = '15'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*L
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'LAENG'.
  ls_fieldcat-ref_fieldname = 'LAENG'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'L'.
  ls_fieldcat-seltext_m     = 'L'.
  ls_fieldcat-seltext_s     = 'L'.
  ls_fieldcat-outputlen     = '15'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*H
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'HOEHE'.
  ls_fieldcat-ref_fieldname = 'HOEHE'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'H'.
  ls_fieldcat-seltext_m     = 'H'.
  ls_fieldcat-seltext_s     = 'H'.
  ls_fieldcat-outputlen     = '15'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*QTY/Cont
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'UMREN'.
  ls_fieldcat-ref_fieldname = 'UMREN'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'QTY/Cont'.
  ls_fieldcat-seltext_m     = 'QTY/Cont'.
  ls_fieldcat-seltext_s     = 'QTY/Cont'.
  ls_fieldcat-outputlen     = '5'.
  ls_fieldcat-no_out        = ''.
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
*
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
  DATA: ls_line TYPE slis_listheader.
  DATA: info_txt(30).

  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-100.
  APPEND ls_line TO lt_top_of_page.

*Plant Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'    .
  info_txt+5(4)  = s_werks-low .
  info_txt+10(2) = 'To'      .
  info_txt+13(4) = s_werks-high.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Plant:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.

*Storage Location Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From' .
  info_txt+5(4)  = s_lgort-low.
  info_txt+10(2) = 'To' .
  info_txt+13(4) = s_lgort-high.
  CLEAR ls_line.
  ls_line-typ    = 'S'.
  ls_line-key    = 'Storage Location:'.
  ls_line-info   = info_txt.
  APPEND ls_line TO lt_top_of_page.

*Storage Type Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'.
  info_txt+5(3)  = s_lgtyp-low.
  info_txt+10(2) = 'To'.
  info_txt+13(3) = s_lgtyp-high.
  CLEAR ls_line.
  ls_line-typ    = 'S'.
  ls_line-key    = 'Storage Type:'.
  ls_line-info   = info_txt.
  APPEND ls_line TO lt_top_of_page.

*Feeder Selection Range Display.
  CLEAR info_txt.
  info_txt+0(4)  = 'From'.
  info_txt+5(5)  = s_feedr-low.
  info_txt+11(2) = 'To'.
  info_txt+14(5) = s_feedr-high.
  CLEAR ls_line.
  ls_line-typ    = 'S'.
  ls_line-key    = 'Feeder:'.
  ls_line-info   = info_txt.
  APPEND ls_line TO lt_top_of_page.

*Line Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'.
  info_txt+5(1)  = s_zline-low.
  info_txt+10(2) = 'To'.
  info_txt+13(1) = s_zline-high.
  CLEAR ls_line.
  ls_line-typ    = 'S'.
  ls_line-key    = 'Line:'.
  ls_line-info   = info_txt.
  APPEND ls_line TO lt_top_of_page.

*W/S Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'.
  info_txt+5(3)  = s_ztime-low.
  info_txt+10(2) = 'To'.
  info_txt+13(3) = s_ztime-high.
  CLEAR ls_line.
  ls_line-typ    = 'S'.
  ls_line-key    = 'W/S:'.
  ls_line-info   = info_txt.
  APPEND ls_line TO lt_top_of_page.

*Source Selection Range Display
  CLEAR info_txt.
  info_txt+0(4)  = 'From'.
  info_txt+5(3)  = s_profl-low.
  info_txt+10(2) = 'To'.
  info_txt+13(3) = s_profl-high.
  CLEAR ls_line.
  ls_line-typ    = 'S'.
  ls_line-key    = 'Source:'.
  ls_line-info   = info_txt.
  APPEND ls_line TO lt_top_of_page.
ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = wa_list_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_txt.
  SELECT SINGLE maktx
           INTO it_itemmaster-maktx
           FROM makt
          WHERE matnr = it_itemmaster-matnr
            AND spras = sy-langu.
ENDFORM.                    " GET_MATERIAL_TXT
*&---------------------------------------------------------------------*
*&      Form  GET_LGTYP_ZTMM_MAST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_lgtyp_ztmm_mast.

ENDFORM.                    " GET_LGTYP_ZTMM_MAST
*&---------------------------------------------------------------------*
*&      Form  SELECTION_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selection_chk .
*WERKS
  SELECT SINGLE werks
           INTO it_itemmaster-werks
           FROM marc
          WHERE matnr = it_itemmaster-matnr.
  IF it_itemmaster-werks IN s_werks.

  ELSE.
    DELETE it_itemmaster.
    CLEAR it_itemmaster.
    subrc = 4.
  ENDIF.
  CHECK subrc = 0.

*LGORT
  SELECT SINGLE lgort
           INTO it_itemmaster-lgort
           FROM mard
          WHERE matnr = it_itemmaster-matnr
            AND werks = it_itemmaster-werks
            AND lgort NE '9999'.
  IF it_itemmaster-lgort IN s_lgort.
  ELSE.
    DELETE it_itemmaster.
    CLEAR it_itemmaster.
    subrc = 4.
  ENDIF.
  CHECK subrc = 0.

*LGTYP
  SELECT SINGLE lgtyp
           INTO it_itemmaster-lgtyp
           FROM mlgt
          WHERE matnr = it_itemmaster-matnr
            AND lgnum = 'P01'.
  IF it_itemmaster-lgtyp IN s_lgtyp.
  ELSE.
    DELETE it_itemmaster.
    CLEAR it_itemmaster.
    subrc = 4.
  ENDIF.
  CHECK subrc = 0.

*FEEDR, ZLINE, ZTIME
  SELECT SINGLE feedr zline ztime rh_lh
           INTO (it_itemmaster-feedr,
                 it_itemmaster-zline,
                 it_itemmaster-ztime,
                 it_itemmaster-rh_lh)
           FROM ztmm_mast
          WHERE matnr = it_itemmaster-matnr
            AND werks = it_itemmaster-werks.
  IF NOT it_itemmaster-feedr IN s_feedr OR
     NOT it_itemmaster-zline IN s_zline OR
     NOT it_itemmaster-ztime IN s_ztime.
    DELETE it_itemmaster.
    CLEAR it_itemmaster.
    subrc = 4.
  ELSE.

  ENDIF.
ENDFORM.                    " SELECTION_CHK
