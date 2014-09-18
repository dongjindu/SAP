************************************************************************
* Program Name      : ZEMMPM40E_KDPO
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.11.24.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : KD Parts Purchase Order Creation
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.24.     Sung-Tae Lim     UD1K901864     Initial Coding
* 04/13/2005      Furong Wang      UD1K915538     MTART = ROH or ROH1
*                                                 in get data form
*                                                 ORDER UNIT CHANGED
*                                                 FORM UNIT_ISO TO UNIT
* 08/19/2005      shiva            UD1K916602     Change the selection
*                                  date for given date's week instead of
*                                                 that date only.
* 10/06/2005      Shiva            UD1K917871     Create one PO for
*                                         same material with diff. date.
************************************************************************

REPORT zemmpm40e_kdpo NO STANDARD PAGE HEADING
                      LINE-SIZE 132
                      LINE-COUNT 64(1)
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**--- Tables, Views & Structures
TABLES : ztmm_kdpo_log.

**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
         lifnr LIKE eina-lifnr,
         matkl LIKE mara-matkl,
         matnr LIKE mara-matnr,
*         meins LIKE mara-meins,
         BSTME like mara-BSTME,
         pwwrk LIKE plaf-pwwrk,
         lgort LIKE plaf-lgort,
         gsmng LIKE plaf-gsmng,
         ekgrp LIKE ekko-ekgrp,
         ebeln LIKE ekko-ebeln,
         pedtr LIKE plaf-pedtr,     " order finish date -> del. date
         INFNR LIKE EINA-INFNR,
         linecolor(4),     " ALV Color
         message(100),
       END OF it_itab.

DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE,
       it_item_copy LIKE it_itab OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_ekpo OCCURS 0,
         matnr LIKE ekpo-matnr,
       END OF it_ekpo.

DATA: IT_EINE LIKE EINE OCCURS 0 WITH HEADER LINE.

DATA : st_itab LIKE it_itab.

RANGES : r_matkl FOR mara-matkl.


**--- BAPI
DATA : st_po_header LIKE bapiekkoc,
       it_po_items LIKE bapiekpoc OCCURS 0 WITH HEADER LINE,
       it_po_item_schedules LIKE bapieket OCCURS 0 WITH HEADER LINE,
       it_return LIKE bapireturn OCCURS 0 WITH HEADER LINE.

**--- Variables
DATA : w_index TYPE i,
       w_purchaseorder LIKE ekko-ebeln,
       w_ekpo_lines TYPE i,
       w_item_lines TYPE i.

**--- Constants
CONSTANTS : c_bukrs LIKE t001-bukrs VALUE 'H201',
            c_bsart LIKE ekko-bsart VALUE 'KD',
            c_ekorg LIKE ekko-ekorg VALUE 'PU01',
            c_plscn LIKE plaf-plscn VALUE '900',
            c_mtart LIKE mara-mtart VALUE 'ROH',
            c_paart LIKE plaf-paart VALUE 'NB',
            c_profl LIKE mara-profl VALUE 'K',
            c_sobes LIKE plaf-sobes VALUE '0'.


**--- Macro
DEFINE append_fieldcat.
  &1 = &1 + 1.
  w_fieldcat-col_pos    = &1.
  w_fieldcat-fieldname  = &2.
  w_fieldcat-outputlen  = &3.
  w_fieldcat-seltext_l  = &4.
  w_fieldcat-seltext_m  = &4.
  w_fieldcat-seltext_s  = &4.
  w_fieldcat-datatype   = &5.
  w_fieldcat-key        = &6.
  w_fieldcat-qfieldname = &7.
  w_fieldcat-cfieldname = &8.
  append w_fieldcat.
  clear : w_fieldcat.
END-OF-DEFINITION.

DEFINE append_sortcat.
  w_sortcat-spos      = &1.
  w_sortcat-fieldname = &2.
  w_sortcat-tabname   = &3.
  w_sortcat-up        = &4.
  w_sortcat-subtot    = &5.
  append w_sortcat.
  clear : w_sortcat.
END-OF-DEFINITION.

**---
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
SELECT-OPTIONS : s_matnr FOR mara-matnr,
                 s_psttr FOR plaf-psttr OBLIGATORY NO-EXTENSION
                                        NO INTERVALS DEFAULT sy-datum,
                 s_matkl FOR mara-matkl.
*                 s_pertr FOR plaf-pertr OBLIGATORY NO-EXTENSION
*                                        NO INTERVALS DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK block1.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.
  PERFORM get_data.

**---
END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM create_po.
    PERFORM update_log.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.


**---





*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*---
  data: begin of wa_unique_val,
          lifnr like lfa1-lifnr,
          matnr like mara-matnr,
          werks like marc-werks,
          lgort like mard-lgort,
          gsmng like plaf-gsmng,
        end of wa_unique_val.
  data: it_unique_val like table of wa_unique_val.
  data: w_sel_date like sy-datum,
        w_wk_strdt like sy-datum,
        w_wk_enddt like sy-datum.

  field-symbols: <fs_unival> like line of it_unique_val.

  w_sel_date = s_psttr-low.

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
   EXPORTING
     DATE    = w_sel_date
   IMPORTING
*   WEEK          =
     MONDAY  = w_wk_strdt
     SUNDAY  = w_wk_enddt.

*  PERFORM make_matkl_range.
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

  SELECT b~matkl
         b~matnr
         gsmng
         pwwrk
         lgort
         lifnr
** INSERT BY FURONG ****
         b~BSTME
*         b~meins
** END OF INSERT ******
         pedtr     " order finish date -> delivery date
         INFNR
                 INTO CORRESPONDING FIELDS OF TABLE it_temp
                 FROM plaf AS a INNER JOIN mara AS b
                   ON a~mandt EQ b~mandt
                  AND a~matnr EQ b~matnr
                      INNER JOIN eina AS c
                         ON a~mandt EQ c~mandt
                        AND a~matnr EQ c~matnr
                WHERE plscn EQ c_plscn
*                  AND psttr IN s_psttr
                  and psttr between w_wk_strdt and w_wk_enddt
*                  AND mtart EQ c_mtart
                  AND b~matkl IN s_matkl
                  AND paart EQ c_paart
                  AND profl EQ c_profl
                  AND loekz EQ space
                  AND sobes EQ c_sobes
                  AND a~matnr in s_matnr
                  AND ( MTART EQ 'ROH' OR MTART EQ 'ROH1' ).

*----
  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_itab.
**--- get Purchase Group
*    CLEAR : marc.
*    SELECT SINGLE ekgrp INTO it_itab-ekgrp
*                        FROM marc
*                       WHERE matnr EQ it_itab-matnr
*                         AND werks EQ it_itab-pwwrk.
*--- get Storage Location
    IF it_itab-lgort IS INITIAL.
      CLEAR : mard.
      SELECT SINGLE lgort INTO it_itab-lgort
                          FROM mard
                         WHERE matnr EQ it_itab-matnr
                           AND werks EQ it_itab-pwwrk
                           AND lgort NE '9999'.
    ENDIF.
    COLLECT it_itab.
    CLEAR : it_temp, it_itab.
  ENDLOOP.

  sort it_itab by lifnr matkl matnr pedtr.
*  SELECT * INTO TABLE IT_EINE FROM EINE FOR ALL ENTRIES IN IT_ITAB
*                                WHERE INFNR = IT_ITAB-INFNR.
*  ---
  loop at it_itab.
    wa_unique_val-lifnr = it_itab-lifnr.
    wa_unique_val-matnr = it_itab-matnr.
    wa_unique_val-werks = it_itab-pwwrk.
    wa_unique_val-lgort = it_itab-lgort.
    wa_unique_val-gsmng = it_itab-gsmng.
    collect wa_unique_val into it_unique_val.
  endloop.

  refresh: it_temp.
  loop at it_unique_val assigning <fs_unival>.
    read table it_itab with key lifnr = <fs_unival>-lifnr
                          matnr = <fs_unival>-matnr
                          pwwrk = <fs_unival>-werks
                          lgort = <fs_unival>-lgort.
    if sy-subrc ne 0.
    else.
      it_temp = it_itab.
      it_temp-gsmng = <fs_unival>-gsmng.
      append it_temp.
      clear it_temp.
    endif.
  endloop.
  refresh: it_itab.
  it_itab[] = it_temp[].
  clear it_itab.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  create_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_po.
*---
  LOOP AT it_itab.
    CLEAR : st_itab.
    MOVE-CORRESPONDING it_itab TO st_itab.
    AT NEW matkl.
      CLEAR : st_po_header, it_po_items, it_po_items[],
              it_po_item_schedules, it_po_item_schedules[],
              it_return, it_return[], w_index, w_purchaseorder,
              it_item_copy, it_item_copy[].
    ENDAT.
    PERFORM fill_item_schedule.
    AT END OF matkl.
      PERFORM fill_header.
      PERFORM call_bapi_create_po.
      PERFORM modify_itab.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " create_po

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
*---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-006.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.
ENDFORM.                    " comment_build

*&---------------------------------------------------------------------*
*&      Form  make_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_alv_grid.
*---
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program      = w_program
*            i_callback_user_command = 'USER_COMMAND'
*            i_structure_name        = 'ZSMM_GR_LIST'
            is_layout               = w_layout
            it_fieldcat             = w_fieldcat[]
            it_events               = w_eventcat[]
            it_sort                 = w_sortcat[]
            i_save                  = 'A'
       TABLES
            t_outtab                = it_itab
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  call_bapi_create_po
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bapi_create_po.
  CALL FUNCTION 'BAPI_PO_CREATE'
    EXPORTING
      po_header                        = st_po_header
*     PO_HEADER_ADD_DATA               =
*     HEADER_ADD_DATA_RELEVANT         =
*     PO_ADDRESS                       =
      skip_items_with_error            = 'X'
*     ITEM_ADD_DATA_RELEVANT           =
    IMPORTING
      purchaseorder                    = w_purchaseorder
    TABLES
      po_items                         = it_po_items
*     PO_ITEM_ADD_DATA                 =
      po_item_schedules                = it_po_item_schedules
*     PO_ITEM_ACCOUNT_ASSIGNMENT       =
*     PO_ITEM_TEXT                     =
      return                           = it_return
*     PO_LIMITS                        =
*     PO_CONTRACT_LIMITS               =
*     PO_SERVICES                      =
*     PO_SRV_ACCASS_VALUES             =
*     PO_SERVICES_TEXT                 =
*     PO_BUSINESS_PARTNER              =
*     EXTENSIONIN                      =
*     POADDRDELIVERY                   =
            .
ENDFORM.                    " call_bapi_create_po

*&---------------------------------------------------------------------*
*&      Form  fill_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_header.
*---
  MOVE : sy-datum      TO st_po_header-doc_date,
         c_bsart       TO st_po_header-doc_type,
         c_bukrs       TO st_po_header-co_code,
         c_ekorg       TO st_po_header-purch_org,
         st_itab-ekgrp TO st_po_header-pur_group,
         st_itab-lifnr TO st_po_header-vendor.

*--- if purchasing group is empty
  IF st_itab-ekgrp IS INITIAL.
    LOOP AT it_item_copy.
      CLEAR : marc.
      SELECT SINGLE ekgrp INTO st_po_header-pur_group
                          FROM marc
                         WHERE matnr EQ it_item_copy-matnr
                           AND werks EQ it_item_copy-pwwrk.
      IF sy-subrc EQ 0 AND st_po_header-pur_group NE space.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " fill_header

*&---------------------------------------------------------------------*
*&      Form  fill_item_schedule
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_item_schedule.
*---
  w_index = w_index + 1.

  MOVE : w_index       TO it_po_items-po_item,
         st_itab-matnr TO it_po_items-material,
         st_itab-matnr TO it_po_items-pur_mat,
         st_itab-lgort TO it_po_items-store_loc,
         st_itab-pwwrk TO it_po_items-plant,
** INSERT BY FURONG ****
         st_itab-BSTME TO it_po_items-unit.
*         st_itab-MEINS TO it_po_items-po_unit_iso.
** END OF INSERT ***
  APPEND it_po_items.
  CLEAR : it_po_items.

*---
  MOVE : w_index       TO it_po_item_schedules-po_item,

         w_index       TO it_po_item_schedules-serial_no,
*         sy-datum      TO it_po_item_schedules-deliv_date,
         st_itab-pedtr TO it_po_item_schedules-deliv_date,
         st_itab-gsmng TO it_po_item_schedules-quantity.

  APPEND it_po_item_schedules.
  CLEAR : it_po_item_schedules.

*---
  MOVE-CORRESPONDING st_itab TO it_item_copy.
  APPEND it_item_copy.
  CLEAR : it_item_copy.
ENDFORM.                    " fill_item_schedule

*&---------------------------------------------------------------------*
*&      Form  modify_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_itab.
*---
  CLEAR : w_ekpo_lines, w_item_lines, it_ekpo, it_ekpo[].

  READ TABLE it_return WITH KEY type = 'S'
                                code = '06017'.

  IF sy-subrc EQ 0 AND w_purchaseorder NE space.
    DO.
      SELECT matnr INTO CORRESPONDING FIELDS OF TABLE it_ekpo
                   FROM ekpo
                  WHERE ebeln EQ w_purchaseorder.
      IF sy-subrc EQ 0.
        EXIT.
      ENDIF.
    ENDDO.
    DESCRIBE TABLE it_ekpo LINES w_ekpo_lines.
    DESCRIBE TABLE it_item_copy LINES w_item_lines.
    IF w_ekpo_lines EQ w_item_lines.
      MOVE : w_purchaseorder   TO it_itab-ebeln,
             c_green           TO it_itab-linecolor,
             it_return-message TO it_itab-message.
      MOVE : st_po_header-pur_group TO it_itab-ekgrp.
      MODIFY it_itab TRANSPORTING ebeln
                                  linecolor
                                  message
                                  ekgrp   WHERE lifnr EQ st_itab-lifnr
                                            AND matkl EQ st_itab-matkl.
    ELSE.
      LOOP AT it_item_copy.
        READ TABLE it_ekpo WITH KEY matnr = it_item_copy-matnr.
        IF sy-subrc EQ 0.
          MOVE : w_purchaseorder   TO it_itab-ebeln,
                 c_green           TO it_itab-linecolor,
                 it_return-message TO it_itab-message.
        ELSE.
          MOVE : space             TO it_itab-ebeln,
                 c_red             TO it_itab-linecolor.
          CONCATENATE text-m02 it_item_copy-matnr text-m03
                               INTO it_itab-message SEPARATED BY space.
        ENDIF.
        MOVE : st_po_header-pur_group TO it_itab-ekgrp.
        MODIFY it_itab TRANSPORTING
                       ebeln
                       linecolor
                       message
                       ekgrp   WHERE lifnr EQ st_itab-lifnr
                                 AND matkl EQ st_itab-matkl
                                 AND matnr EQ it_item_copy-matnr.
      ENDLOOP.
    ENDIF.
  ELSE.
    MOVE : space             TO it_itab-ebeln,
           c_red             TO it_itab-linecolor.
    READ TABLE it_return INDEX 1.
    MOVE : it_return-message TO it_itab-message.
    MOVE : st_po_header-pur_group TO it_itab-ekgrp.
    MODIFY it_itab TRANSPORTING ebeln
                                linecolor
                                message
                                ekgrp     WHERE lifnr EQ st_itab-lifnr
                                            AND matkl EQ st_itab-matkl.
  ENDIF.

ENDFORM.                    " modify_itab

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat.
**--- &1 : position       &2 : field name       &3 : field length
**--- &4 : description    &5 : field type       &6 : key
**--- &7 : qty field      &8 : color
  append_fieldcat :
    w_col_pos 'EBELN'     10 'PO No.'         'CHAR' 'X' ''      '',
    w_col_pos 'LIFNR'     10 'Vendor No.'     'CHAR' ''  ''      '',
    w_col_pos 'MATKL'      9 'Mat Group'      'CHAR' ''  ''      '',
    w_col_pos 'MATNR'     18 'Material No.'   'CHAR' ''  ''      '',
    w_col_pos 'EKGRP'      3 'Purch Group'    'CHAR' ''  ''      '',
    w_col_pos 'GSMNG'     12 'Quantity'       'QUAN' ''  'BSTME' '',
    w_col_pos 'BSTME'      3 'UoM'            'CHAR' ''  ''      '',
    w_col_pos 'PWWRK'      4 'Plant'          'CHAR' ''  ''      '',
    w_col_pos 'LGORT'      4 'StLoc.'         'CHAR' ''  ''      '',
    w_col_pos 'MESSAGE'   80 'Message'        'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat

*&---------------------------------------------------------------------*
*&      Form  build_sortcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat.
*---
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'EBELN' 'IT_ITAB' 'X' '',
                   '2' 'MATKL' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  make_matkl_range
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_matkl_range.
*---
  CLEAR : r_matkl, r_matkl[].

  MOVE : 'I'        TO r_matkl-sign,
         'EQ'       TO r_matkl-option,
         'NF-KD'    TO r_matkl-low.
  APPEND r_matkl.

  MOVE : 'I'        TO r_matkl-sign,
         'EQ'       TO r_matkl-option,
         'CM-KD'    TO r_matkl-low.
  APPEND r_matkl.

  MOVE : 'I'        TO r_matkl-sign,
         'EQ'       TO r_matkl-option,
         'NF-KD-EN' TO r_matkl-low.
  APPEND r_matkl.

  MOVE : 'I'        TO r_matkl-sign,
         'EQ'       TO r_matkl-option,
         'NF-KD-TM' TO r_matkl-low.
  APPEND r_matkl.

  MOVE : 'I'        TO r_matkl-sign,
         'EQ'       TO r_matkl-option,
         'CM-KD-EN' TO r_matkl-low.
  APPEND r_matkl.

  MOVE : 'I'        TO r_matkl-sign,
         'EQ'       TO r_matkl-option,
         'CM-KD-TM' TO r_matkl-low.
  APPEND r_matkl.
ENDFORM.                    " make_matkl_range

*&---------------------------------------------------------------------*
*&      Form  update_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_log.
*---
  CONSTANTS : c_nro_nr_09 VALUE '09' LIKE inri-nrrangenr.

  DATA : it_ztmm_kdpo LIKE ztmm_kdpo_log OCCURS 0 WITH HEADER LINE.

  DATA : w_nro_object VALUE 'ZMMNRO0002' LIKE inri-object.

  CLEAR : it_ztmm_kdpo, it_ztmm_kdpo[].

  LOOP AT it_itab.
    MOVE-CORRESPONDING it_itab TO it_ztmm_kdpo.
    MOVE : it_itab-linecolor   TO it_ztmm_kdpo-color,
           it_itab-message     TO it_ztmm_kdpo-messa.
    PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                     w_nro_object    "NRO Object
                            CHANGING it_ztmm_kdpo-logno_h.
    COMMIT WORK.
    MOVE : sy-tcode            TO it_ztmm_kdpo-ztcode,
           sy-repid            TO it_ztmm_kdpo-zprogramm.
    it_ztmm_kdpo-ernam = it_ztmm_kdpo-aenam = sy-uname.
    it_ztmm_kdpo-erdat = it_ztmm_kdpo-aedat = sy-datum.
    it_ztmm_kdpo-erzet = it_ztmm_kdpo-aezet = sy-uzeit.
    APPEND it_ztmm_kdpo.
  ENDLOOP.

  MODIFY ztmm_kdpo_log FROM TABLE it_ztmm_kdpo.
ENDFORM.                    " update_log

*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_W_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next USING    p_c_nro_nr_09
                              p_w_nro_object
                     CHANGING p_w_zdocno.
*---
  CLEAR : p_w_zdocno.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_c_nro_nr_09
            object                  = p_w_nro_object
       IMPORTING
            number                  = p_w_zdocno
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
ENDFORM.                    " number_get_next
