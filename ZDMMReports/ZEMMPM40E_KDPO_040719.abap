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
*
*
************************************************************************

REPORT zemmpm40e_kdpo NO STANDARD PAGE HEADING
                      LINE-SIZE 132
                      LINE-COUNT 64(1)
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**--- Tables, Views & Structures

**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
         lifnr LIKE eina-lifnr,
         group(1),
         matkl LIKE mara-matkl,
         matnr LIKE mara-matnr,
         meins LIKE mara-meins,
         pwwrk LIKE plaf-pwwrk,
         lgort LIKE plaf-lgort,
         gsmng LIKE plaf-gsmng,
         ekgrp LIKE ekko-ekgrp,
         ebeln LIKE ekko-ebeln,
         pedtr LIKE plaf-pedtr,     " order finish date -> del. date
         linecolor(4),     " ALV Color
         message(100),
       END OF it_itab.

DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE.

DATA : st_itab LIKE it_itab.

RANGES : r_matkl FOR mara-matkl.


**--- BAPI
DATA : st_po_header LIKE bapiekkoc,
       it_po_items LIKE bapiekpoc OCCURS 0 WITH HEADER LINE,
       it_po_item_schedules LIKE bapieket OCCURS 0 WITH HEADER LINE,
       it_return LIKE bapireturn OCCURS 0 WITH HEADER LINE.

**--- Variables
DATA : w_index TYPE i,
       w_purchaseorder LIKE ekko-ebeln.

**--- Constants
CONSTANTS : c_bukrs LIKE t001-bukrs VALUE 'H201',
            c_bsart LIKE ekko-bsart VALUE 'KD',
            c_ekorg LIKE ekko-ekorg VALUE 'PU01',
            c_plscn LIKE plaf-plscn VALUE '900',
            c_mtart LIKE mara-mtart VALUE 'ROH',
            c_paart LIKE plaf-paart VALUE 'NB'.


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
  CLEAR : it_itab, it_itab[], it_temp, it_temp[].

  PERFORM make_matkl_range.

  SELECT b~matkl
         b~matnr
         gsmng
         pwwrk
         lgort
         lifnr
         b~meins
         pedtr     " order finish date -> delivery date
*         ekgrp
                 INTO CORRESPONDING FIELDS OF TABLE it_temp
                 FROM plaf AS a INNER JOIN mara AS b
                   ON a~mandt EQ b~mandt
                  AND a~matnr EQ b~matnr
                      INNER JOIN eina AS c
                         ON a~mandt EQ c~mandt
                        AND a~matnr EQ c~matnr
                WHERE plscn EQ c_plscn
                  AND psttr IN s_psttr
*                  AND pertr IN s_pertr
                  AND mtart EQ c_mtart
                  AND b~matkl IN r_matkl
                  AND b~matkl IN s_matkl
                  AND paart EQ c_paart.

*---
  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_itab.
    CASE it_temp-matkl.
      WHEN 'NF-KD'.
        MOVE : 'A' TO it_itab-group.
      WHEN 'CM-KD'.
        MOVE : 'B' TO it_itab-group.
*      WHEN 'NF-KD-UN'.
*        MOVE : 'A' TO it_itab-group.
*      WHEN 'NF-KD-CO'.
*        MOVE : 'B' TO it_itab-group.
*      WHEN 'CM-KD-UN'.
*        MOVE : 'C' TO it_itab-group.
*      WHEN 'CM-KD-CO'.
*        MOVE : 'D' TO it_itab-group.
      WHEN 'NF-KD-EN' OR 'NF-KD-TM'.
        MOVE : 'E' TO it_itab-group.
      WHEN 'CM-KD-EN' OR 'CM-KD-TM'.
        MOVE : 'F' TO it_itab-group.
    ENDCASE.
*    MOVE : 'A01'               TO it_itab-ekgrp.
*--- get Purchase Group
    CLEAR : marc.
    SELECT SINGLE ekgrp INTO it_itab-ekgrp
                        FROM marc
                       WHERE matnr EQ it_itab-matnr
                         AND werks EQ it_itab-pwwrk.
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

  SORT it_itab BY lifnr group matkl matnr.
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
    AT NEW group.
      CLEAR : st_po_header, it_po_items, it_po_items[],
              it_po_item_schedules, it_po_item_schedules[],
              it_return, it_return[], w_index, w_purchaseorder.
    ENDAT.
    PERFORM fill_item_schedule.
    AT END OF group.
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
*     SKIP_ITEMS_WITH_ERROR            = 'X'
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
         st_itab-meins TO it_po_items-po_unit_iso,
         st_itab-meins TO it_po_items-po_unit_iso.

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
  READ TABLE it_return WITH KEY type = 'S'
                                code = '06017'.

  IF sy-subrc EQ 0 AND w_purchaseorder NE space.
    MOVE : w_purchaseorder   TO it_itab-ebeln,
           c_green           TO it_itab-linecolor,
           it_return-message TO it_itab-message.
  ELSE.
    MOVE : space             TO it_itab-ebeln,
           c_red             TO it_itab-linecolor.
    READ TABLE it_return INDEX 1.
    MOVE : it_return-message TO it_itab-message.
  ENDIF.

*---
  MODIFY it_itab TRANSPORTING ebeln
                              linecolor
                              message
                                        WHERE lifnr EQ st_itab-lifnr
                                          AND matkl EQ st_itab-matkl.
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
    w_col_pos 'GSMNG'     12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'      3 'UoM'            'CHAR' ''  ''      '',
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

*  MOVE : 'I'        TO r_matkl-sign,
*         'EQ'       TO r_matkl-option,
*         'NF-KD-UN' TO r_matkl-low.
*  APPEND r_matkl.
*
*  MOVE : 'I'        TO r_matkl-sign,
*         'EQ'       TO r_matkl-option,
*         'NF-KD-CO' TO r_matkl-low.
*  APPEND r_matkl.
*
*  MOVE : 'I'        TO r_matkl-sign,
*         'EQ'       TO r_matkl-option,
*         'CM-KD-UN' TO r_matkl-low.
*  APPEND r_matkl.
*
*  MOVE : 'I'        TO r_matkl-sign,
*         'EQ'       TO r_matkl-option,
*         'CM-KD-CO' TO r_matkl-low.
*  APPEND r_matkl.

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
