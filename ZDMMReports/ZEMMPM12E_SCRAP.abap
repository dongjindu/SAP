************************************************************************
* Program Name      : ZEMMPM12E_SCRAP
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.10.23.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : Component Scrap GI
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.23.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************


REPORT zemmpm12e_scrap NO STANDARD PAGE HEADING
                      LINE-SIZE 132
                      LINE-COUNT 64(1)
                      MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

**--- Tables, Views & Structures

**--- Internal Tables
DATA : BEGIN OF it_itab OCCURS 0,
*         lifnr LIKE mseg-lifnr,
         matnr LIKE mseg-matnr,
         werks LIKE mseg-werks,
         lgort LIKE mseg-lgort,
         erfmg LIKE mseg-erfmg,     " 261/262 posting qty
         menge LIKE mseg-menge,     " scrap qty
         mhdlp LIKE mara-mhdlp,
         meins LIKE mseg-meins,
         mblnr LIKE mseg-mblnr,
         mjahr LIKE mseg-mjahr,
         tanum LIKE ltak-tanum,
         messa(80),
         linecolor(4),     " ALV Color
       END OF it_itab.

DATA : BEGIN OF it_temp OCCURS 0,
         matnr LIKE mseg-matnr,
         erfmg LIKE mseg-erfmg,
         bwart LIKE mseg-bwart,
         werks LIKE mseg-werks,
         lgort LIKE mseg-lgort,
         meins LIKE mseg-meins,
       END OF it_temp.

DATA : BEGIN OF it_write_temp OCCURS 0.
        INCLUDE STRUCTURE it_itab.
DATA :   wmrel(1),
       END OF it_write_temp.

DATA : it_write      LIKE it_write_temp OCCURS 0 WITH HEADER LINE,
       it_itab_copy  LIKE it_itab OCCURS 0 WITH HEADER LINE.

*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

*--- BAPI
DATA : w_goodsmvt_header  LIKE bapi2017_gm_head_01,
       w_goodsmvt_code    LIKE bapi2017_gm_code,
       w_goodsmvt_headret LIKE bapi2017_gm_head_ret,
       w_materialdocument LIKE bapi2017_gm_head_ret-mat_doc,
       w_matdocumentyear  LIKE bapi2017_gm_head_ret-doc_year,
       it_goodsmvt_item
            LIKE TABLE OF bapi2017_gm_item_create  WITH HEADER LINE,
       it_goodsmvt_serialnumber
            LIKE TABLE OF bapi2017_gm_serialnumber WITH HEADER LINE,
       it_return
            LIKE TABLE OF bapiret2                 WITH HEADER LINE.

**--- Variables
DATA : w_mode LIKE ctu_params-dismode VALUE 'A',
       w_lines TYPE i,
       w_subrc LIKE sy-subrc.

**--- Constants
CONSTANTS : c_100 TYPE i VALUE 100,
            c_gm_code LIKE w_goodsmvt_code-gm_code VALUE '03',
            c_bwart_907 LIKE mseg-bwart VALUE '907'.


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
PARAMETERS : p_budat LIKE mkpf-budat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK block1.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].
  PERFORM set_posting_date.

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
    PERFORM posting_document.
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
**---
  DATA : l_datum TYPE d.

  CLEAR : it_temp, it_temp[], it_itab, it_itab[], l_datum.

*--- already posting check
  l_datum = p_budat + 1.
  SELECT SINGLE b~mblnr INTO mseg-mblnr
                        FROM mkpf AS a INNER JOIN mseg AS b
                          ON a~mandt EQ b~mandt
                         AND a~mblnr EQ b~mblnr
                         AND a~mjahr EQ b~mjahr
                       WHERE budat EQ l_datum
                         AND bwart IN ('907', '908').

  CHECK sy-subrc NE 0.

*--- get data
  SELECT matnr     " material
         erfmg     " quantity
         bwart     " movement type
         werks     " plant
         lgort     " storage location
         meins     " unit of measure
               INTO CORRESPONDING FIELDS OF TABLE it_temp
               FROM mkpf AS a INNER JOIN mseg AS b
                 ON a~mandt EQ b~mandt
                AND a~mblnr EQ b~mblnr
                AND a~mjahr EQ b~mjahr
              WHERE budat EQ p_budat
                AND bwart IN ('261', '262').

*---
  LOOP AT it_temp.
    MOVE-CORRESPONDING it_temp TO it_itab.
    CASE it_temp.
      WHEN '261'.
      WHEN '262'.
        it_itab-erfmg = it_itab-erfmg * -1.
    ENDCASE.
    COLLECT it_itab.
    CLEAR : it_temp, it_itab.
  ENDLOOP.

*--- calculate scrap percentage
  DATA : l_mhdlp(8) TYPE p DECIMALS 5.

  LOOP AT it_itab.
    CLEAR : mara-mhdlp, l_mhdlp.
    SELECT SINGLE mhdlp INTO mara-mhdlp
                        FROM mara
                       WHERE matnr EQ it_itab-matnr.
    MOVE : mara-mhdlp TO it_itab-mhdlp.
    l_mhdlp = ( it_itab-erfmg * mara-mhdlp ) / 100.
    PERFORM call_round_function USING l_mhdlp.
    MOVE : l_mhdlp TO it_itab-menge.
    MODIFY it_itab.
  ENDLOOP.

*---
  DESCRIBE TABLE it_itab LINES w_lines.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  posting_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_document.
*---
  DATA : l_tabix LIKE sy-tabix,
         l_mod TYPE i.

  CLEAR : it_write_temp, it_write_temp[], it_write, it_write[].

  LOOP AT it_itab.
    MOVE : sy-tabix TO l_tabix.
    l_mod = l_tabix MOD c_100.
    PERFORM append_write_table.
    PERFORM append_bapi_structure.
    IF l_mod EQ 0 OR l_tabix EQ w_lines.
      PERFORM gi_posting USING w_subrc.
      IF w_subrc EQ 0 AND w_materialdocument NE space.
        PERFORM to_create.
      ENDIF.
      APPEND LINES OF it_write_temp TO it_write.
      CLEAR : it_write_temp[], w_subrc.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " posting_document

*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM comment_build.
**---
  CLEAR : w_line.
  w_line-typ  = 'H'.
  w_line-info = text-002.
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
**---
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program      = w_program
*            i_callback_user_command = 'USER_COMMAND'
            is_layout               = w_layout
            it_fieldcat             = w_fieldcat[]
            it_events               = w_eventcat[]
            it_sort                 = w_sortcat[]
            i_save                  = 'A'
       TABLES
            t_outtab                = it_write
       EXCEPTIONS
            program_error           = 1
            OTHERS                  = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  set_posting_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_posting_date.
*---
  p_budat = sy-datum - 1.
ENDFORM.                    " set_posting_date

*&---------------------------------------------------------------------*
*&      Form  gi_posting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gi_posting USING p_subrc.
*---

  CLEAR : w_goodsmvt_header, w_goodsmvt_code, w_goodsmvt_headret,
          w_materialdocument, w_matdocumentyear,
          it_return, it_return[].

  MOVE : sy-datum  TO w_goodsmvt_header-pstng_date,
         sy-datum  TO w_goodsmvt_header-doc_date,
         c_gm_code TO w_goodsmvt_code-gm_code.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
       EXPORTING
            goodsmvt_header  = w_goodsmvt_header
            goodsmvt_code    = w_goodsmvt_code
       IMPORTING
            goodsmvt_headret = w_goodsmvt_headret
            materialdocument = w_materialdocument
            matdocumentyear  = w_matdocumentyear
       TABLES
            goodsmvt_item    = it_goodsmvt_item
            return           = it_return.

  READ TABLE it_return WITH KEY type = 'E'.

  MOVE : sy-subrc TO p_subrc.

  IF p_subrc EQ 0.
    ROLLBACK WORK.
    MOVE : c_red             TO it_write_temp-linecolor,
           it_return-message TO it_write_temp-messa.
    MODIFY it_write_temp TRANSPORTING linecolor messa
                                WHERE matnr NE space.
  ELSE.
    COMMIT WORK AND WAIT.
    MOVE : c_green            TO it_write_temp-linecolor,
           it_return-message  TO it_write_temp-messa,
           w_materialdocument TO it_write_temp-mblnr,
           w_matdocumentyear  TO it_write_temp-mjahr.
    MODIFY it_write_temp TRANSPORTING linecolor messa mblnr
                                WHERE matnr NE space.
  ENDIF.

*---
  CLEAR : it_goodsmvt_item[].
ENDFORM.                    " gi_posting

*&---------------------------------------------------------------------*
*&      Form  to_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM to_create.
*---
  DATA : l_text(80).

  CLEAR : w_subrc.

  PERFORM check_transfer_order_condition USING w_subrc.

  IF w_subrc EQ 0.
    CASE t333-abild.
      WHEN '1'.     " Stock Placement
        PERFORM transfer_order_posting_to_wm.
      WHEN '2'.     " Stock Removal
        PERFORM transfer_order_posting_from_wm.
    ENDCASE.
    IF sy-subrc EQ 0 AND sy-msgty EQ 'S'.
      READ TABLE it_mess WITH KEY msgid = 'L3'
                                  msgnr = '016'.
      IF sy-subrc EQ 0.
        MOVE : it_mess-msgv1 TO it_write_temp-tanum.
        MODIFY it_write_temp TRANSPORTING tanum
                                    WHERE wmrel NE space.
      ENDIF.
    ELSE.
      PERFORM get_error_message USING l_text.
      MOVE : l_text TO it_write_temp-messa.
      MODIFY it_write_temp TRANSPORTING messa
                                  WHERE wmrel NE space.
    ENDIF.
  ELSE.
  ENDIF.
ENDFORM.                    " to_create

*&---------------------------------------------------------------------*
*&      Form  append_write_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_write_table.
*---
  CLEAR : it_write_temp.

  MOVE-CORRESPONDING it_itab TO it_write_temp.
  APPEND it_write_temp.
ENDFORM.                    " append_write_table

*&---------------------------------------------------------------------*
*&      Form  append_bapi_structure
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_bapi_structure.
*---
  CLEAR : it_goodsmvt_item.

  MOVE : it_itab-matnr TO it_goodsmvt_item-material,
         it_itab-werks TO it_goodsmvt_item-plant,
         it_itab-lgort TO it_goodsmvt_item-stge_loc,
         c_bwart_907   TO it_goodsmvt_item-move_type,
         it_itab-erfmg TO it_goodsmvt_item-entry_qnt.

  IF it_itab-werks EQ 'P001'.
    MOVE : 'CP001'     TO it_goodsmvt_item-orderid.
  ELSEIF it_itab-werks EQ 'E001'.
    MOVE : 'CE001'     TO it_goodsmvt_item-orderid.
** for E002
  ELSEIF it_itab-werks EQ 'E002'.
    MOVE : 'CE001'     TO it_goodsmvt_item-orderid.
** END E002
  ENDIF.

  APPEND it_goodsmvt_item.
ENDFORM.                    " append_bapi_structure

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
    w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'WERKS'  4 'Plant'          'CHAR' ''  ''      '',
    w_col_pos 'LGORT'  4 'Storage Loc'    'CHAR' ''  ''      '',
    w_col_pos 'ERFMG' 12 'Quantity'       'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'  3 'UoM'            'CHAR' ''  ''      '',
    w_col_pos 'MBLNR' 10 'Document No'    'CHAR' ''  ''      '',
    w_col_pos 'TANUM' 10 'T/O No'         'CHAR' ''  ''      '',
    w_col_pos 'MESSA' 80 'Message'        'CHAR' ''  ''      ''.
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
**--- &1 : position     &2 : field name     &3 : tab name
**--- &4 : up           &5 : sub total
  append_sortcat : '1' 'MATNR' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  check_transfer_order_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM check_transfer_order_condition USING    p_w_subrc.
*---
  CLEAR : ltbk.

  SELECT SINGLE * FROM ltbk
                 WHERE mblnr EQ w_materialdocument
                   AND mjahr EQ w_matdocumentyear.

  IF sy-subrc EQ 0.
    CLEAR : t333.

    SELECT SINGLE * FROM t333
                   WHERE lgnum EQ ltbk-lgnum
                     AND bwlvs EQ ltbk-bwlvs.

    MOVE : sy-subrc TO p_w_subrc.
  ELSE.
    MOVE : sy-subrc TO p_w_subrc.
  ENDIF.
ENDFORM.                    " check_transfer_order_condition

*&---------------------------------------------------------------------*
*&      Form  transfer_order_posting_to_wm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_order_posting_to_wm.
*---
  DATA : l_tabix LIKE sy-tabix.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], it_itab_copy,
          it_itab_copy[].

*---
  LOOP AT it_write_temp.
    MOVE : sy-tabix TO l_tabix.
    SELECT SINGLE * FROM t320
                   WHERE werks EQ it_write_temp-werks
                     AND lgort EQ it_write_temp-lgort.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING it_write_temp TO it_itab_copy.
      APPEND it_itab_copy.
      CLEAR : it_itab_copy.
      MOVE : 'X' TO it_write_temp-wmrel.
      MODIFY it_write_temp TRANSPORTING wmrel WHERE matnr NE space.
    ENDIF.
  ENDLOOP.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
                         ' '  'RL02B-MBLNR'     w_materialdocument,
                         ' '  'RL02B-MJAHR'     w_matdocumentyear,
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=MRKA'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=TPAL'.

  LOOP AT it_itab_copy.
    PERFORM get_wm_info.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0104',
                           ' '  'RL03T-LETY2'     'BB',
                           ' '  'RL03T-LGTY2'     pkhd-lgtyp,
                           ' '  'BDC_OKCODE'      '/00'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0104',
                           ' '  'BDC_OKCODE'      '=TAH1'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0102',
                           ' '  'LTAP-NLTYP'      pkhd-lgtyp,
                           ' '  'LTAP-NLPLA'      pkhd-lgpla,
                           ' '  'BDC_OKCODE'      '/00'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0104',
                           ' '  'BDC_OKCODE'      '=TATB'.
  ENDLOOP.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=BU'.

  CALL TRANSACTION 'LT06' USING it_bdc
                          MODE w_mode
                          UPDATE 'S'
                          MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.
ENDFORM.                    " transfer_order_posting_to_wm

*&---------------------------------------------------------------------*
*&      Form  transfer_order_posting_from_wm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_order_posting_from_wm.
*---
  DATA : l_tabix LIKE sy-tabix.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], it_itab_copy,
          it_itab_copy[].

*---
  LOOP AT it_write_temp.
    MOVE : sy-tabix TO l_tabix.
    SELECT SINGLE * FROM t320
                   WHERE werks EQ it_write_temp-werks
                     AND lgort EQ it_write_temp-lgort.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING it_write_temp TO it_itab_copy.
      APPEND it_itab_copy.
      CLEAR : it_itab_copy.
      MOVE : 'X' TO it_write_temp-wmrel.
      MODIFY it_write_temp TRANSPORTING wmrel WHERE matnr NE space.
    ENDIF.
  ENDLOOP.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
                         ' '  'RL02B-MBLNR'     w_materialdocument,
                         ' '  'RL02B-MJAHR'     w_matdocumentyear,
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=MRKA'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=TTYP'.

  LOOP AT it_itab_copy.
    PERFORM get_wm_info.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0105',
                           ' '  'BDC_OKCODE'      '=TAH2'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0102',
                           ' '  'LTAP-VLTYP'      pkhd-lgtyp,
                           ' '  'LTAP-VLPLA'      pkhd-lgpla,
                           ' '  'BDC_OKCODE'      '/00'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0105',
                           ' '  'BDC_OKCODE'      '=TATB'.
  ENDLOOP.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=BU'.

  CALL TRANSACTION 'LT06' USING it_bdc
                          MODE w_mode
                          UPDATE 'S'
                          MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.
ENDFORM.                    " transfer_order_posting_from_wm

*&---------------------------------------------------------------------*
*&      Form  get_wm_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_wm_info.
*---
  CLEAR : pkhd.

  SELECT SINGLE * FROM pkhd
                 WHERE matnr EQ it_itab_copy-matnr.
ENDFORM.                    " get_wm_info

*&---------------------------------------------------------------------*
*&      Form  get_error_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_error_message USING p_text.
*---
  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    CLEAR : p_text.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = it_mess-msgid
              msgnr               = it_mess-msgnr
              msgv1               = it_mess-msgv1
              msgv2               = it_mess-msgv2
              msgv3               = it_mess-msgv3
              msgv4               = it_mess-msgv4
         IMPORTING
              message_text_output = p_text.
  ENDIF.
ENDFORM.                    " get_error_message

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1047   text
*      -->P_1048   text
*      -->P_1049   text
*----------------------------------------------------------------------*
FORM dynpro USING    dynbegin
                     name
                     value.
  IF dynbegin = 'X'.
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-program,
           value TO it_bdc-dynpro,
           'X'   TO it_bdc-dynbegin.
    APPEND it_bdc.
  ELSE .
    CLEAR : it_bdc.
    MOVE : name  TO it_bdc-fnam,
           value TO it_bdc-fval.
    APPEND it_bdc.
  ENDIF.
ENDFORM.                    " dynpro

*&---------------------------------------------------------------------*
*&      Form  call_round_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MHDLP  text
*----------------------------------------------------------------------*
FORM call_round_function USING    p_l_mhdlp.
*---
  CALL FUNCTION 'ROUND'
       EXPORTING
            decimals = 0
            input    = p_l_mhdlp
            sign     = '+'
       IMPORTING
            output   = p_l_mhdlp.
ENDFORM.                    " call_round_function
