************************************************************************
* Program Name      : ZPPA802_SCRAP_GI
* Author            : Furong Wang
* Creation Date     : 04/08/2006
* Specifications By :
* Addl Documentation:
* Description       : Scrap GI Posting
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************
REPORT ZPPA802_SCRAP_GI NO STANDARD PAGE HEADING
                        LINE-SIZE 132
                        LINE-COUNT 64(1)
                        MESSAGE-ID zmpp.
TYPE-POOLS : slis.

TABLES: ztpp_osdp, mseg.

DATA : BEGIN OF it_itab OCCURS 0,
*       model LIKE ztpp_osdp-model,
*       body_ser LIKE ztpp_osdp-body_ser,
*       porder  LIKE ztpp_osdp-porder,
*       resb_no LIKE ztpp_osdp-resb_no,
*       resb_item LIKE ztpp_osdp-resb_item,
       matnr LIKE ztpp_osdp-comp_no,
       lgort LIKE ztpp_osdp-supply_area,
       erfmg LIKE ztpp_osdp-quantity,
       meins LIKE ztpp_osdp-meins,
       revivied_date LIKE ztpp_osdp-revivied_date,
       mblnr LIKE mseg-mblnr,
*       mjahr LIKE MSEG-mjahr,
       gi_status LIKE ztpp_osdp-gi_status,
       processed(1),
       messa(80),
       linecolor(4),     " ALV Color
      END OF it_itab.

DATA : BEGIN OF it_write_temp OCCURS 0.
        INCLUDE STRUCTURE it_itab.
DATA : END OF it_write_temp.

DATA : it_write      LIKE it_write_temp OCCURS 0 WITH HEADER LINE,
       it_itab_copy  LIKE it_itab OCCURS 0 WITH HEADER LINE.

DATA:  c_yell(4)  VALUE 'C310',
       c_green(4) VALUE 'C510',
       c_red(4)   VALUE 'C610'.

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
DATA : w_lines TYPE i,
       w_subrc LIKE sy-subrc.

*--- ALV
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line TYPE slis_listheader,
       w_layout   TYPE slis_layout_alv.


**--- Constants
CONSTANTS : c_werks LIKE mseg-werks VALUE 'P001',
            c_gm_code LIKE w_goodsmvt_code-gm_code VALUE '03',
            c_bwart_551 LIKE mseg-bwart VALUE '551'.

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
PARAMETERS : p_post LIKE sy-datum OBLIGATORY.
PARAMETERS : p_doc LIKE sy-datum OBLIGATORY.
select-options: s_matnr for mseg-matnr.
PARAMETERS : p_new(1) DEFAULT 'X'.
selection-screen begin of line.
selection-screen comment (31) text-t02.
PARAMETERS : p_aufnr LIKE mseg-aufnr.
selection-screen comment 55(20) text-t01.
selection-screen end of line.
PARAMETERS : p_giline type i default 1.
SELECTION-SCREEN END OF BLOCK block1.

INITIALIZATION.
  PERFORM set_para_data.

START-OF-SELECTION.
  PERFORM get_data.

END-OF-SELECTION.
  IF it_itab[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM posting_document.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

  DATA: lt_itab LIKE TABLE OF it_itab WITH HEADER LINE.
  DATA : l_datum TYPE d.

  CLEAR : it_itab, it_itab[].
  IF p_new = 'X'.
    SELECT comp_no AS matnr SUPPLY_AREA AS lgort
           quantity AS erfmg meins revivied_date
      INTO CORRESPONDING FIELDS OF TABLE lt_itab
      FROM ztpp_osdp
      WHERE comp_no in s_matnr
        and revivied_date = p_post
        AND ( gi_status = space or gi_status is null ).
  ELSE.
    SELECT comp_no AS matnr SUPPLY_AREA AS lgort
           quantity AS erfmg meins revivied_date
      INTO CORRESPONDING FIELDS OF TABLE lt_itab
     FROM ztpp_osdp
     WHERE comp_no in s_matnr
       and revivied_date = p_post
       AND gi_status = 'E'.
  ENDIF.

  SORT lt_itab BY matnr LGORT.
  LOOP AT lt_itab.
    MOVE lt_itab TO it_itab.
    COLLECT it_itab.
    CLEAR: lt_itab, it_itab.
  ENDLOOP.
  REFRESH: lt_itab[].
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

  DATA : l_tabix LIKE sy-tabix,
         l_mod TYPE i.

  CLEAR : it_write_temp, it_write_temp[], it_write, it_write[].

  LOOP AT it_itab.
    MOVE : sy-tabix TO l_tabix.
    l_mod = l_tabix MOD p_giline.
    PERFORM append_write_table.
    PERFORM append_bapi_structure.
    it_itab-processed = 'Y'.
    MODIFY it_itab.
    IF l_mod EQ 0 OR l_tabix EQ w_lines.
      PERFORM gi_posting USING w_subrc.
      APPEND LINES OF it_write_temp TO it_write.
      CLEAR : it_write_temp[], w_subrc.
    ENDIF.
  ENDLOOP.
  PERFORM update_table.

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

  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
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
FORM set_para_data.
  p_post = sy-datum.
  p_doc = p_post.
  p_aufnr = 'CP001'.
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

  CLEAR : w_goodsmvt_header, w_goodsmvt_code, w_goodsmvt_headret,
          w_materialdocument, w_matdocumentyear,
          it_return, it_return[].

  MOVE : p_doc  TO w_goodsmvt_header-pstng_date,
         p_doc   TO w_goodsmvt_header-doc_date,
         'Scrap Car' to w_goodsmvt_header-HEADER_TXT,
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
    concatenate it_return-message 'hlw' into it_return-message.
    MOVE : c_red             TO it_write_temp-linecolor,
           it_return-message TO it_write_temp-messa,
           'E' TO it_write_temp-gi_status,
           ' ' TO it_write_temp-processed.
    MODIFY it_write_temp TRANSPORTING linecolor messa
                                      gi_status
                                WHERE matnr NE space.
    MODIFY it_itab FROM it_write_temp
                          TRANSPORTING gi_status processed
                          WHERE processed = 'Y'.

  ELSE.
    COMMIT WORK AND WAIT.
    MOVE : c_green            TO it_write_temp-linecolor,
           it_return-message  TO it_write_temp-messa,
           w_materialdocument TO it_write_temp-mblnr,
*           w_matdocumentyear  TO it_write_temp-mjahr,
           'S' TO it_write_temp-gi_status,
           ' ' TO it_write_temp-processed.
    MODIFY it_write_temp TRANSPORTING mblnr linecolor messa
                                      gi_status
                                      where matnr ne space.
    MODIFY it_itab FROM it_write_temp
                         TRANSPORTING gi_status processed
                                      mblnr
                         WHERE processed = 'Y'.
  ENDIF.

  CLEAR : it_goodsmvt_item[].
ENDFORM.                    " gi_posting

*&---------------------------------------------------------------------*
*&      Form  append_write_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_write_table.

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
  DATA:  l_werks_1(1).

  MOVE : it_itab-matnr TO it_goodsmvt_item-material,
         c_werks TO it_goodsmvt_item-plant,
         it_itab-LGORT TO it_goodsmvt_item-stge_loc,
         c_bwart_551   TO it_goodsmvt_item-move_type,
         it_itab-erfmg TO it_goodsmvt_item-entry_qnt,
         '0002' TO it_goodsmvt_item-MOVE_REAS.

  MOVE : p_aufnr     TO it_goodsmvt_item-orderid.

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
    w_col_pos 'LGORT'  4 'Storage Loc'    'CHAR' ''  ''      '',
    w_col_pos 'ERFMG' 12 'GI Qty'         'QUAN' ''  'MEINS' '',
    w_col_pos 'MEINS'  3 'UoM'            'CHAR' ''  ''      '',
    w_col_pos 'MBLNR' 12 'Docement No'    'CHAR' ''  ''      '',
    w_col_pos 'GI_STATUS' 4 'Status'      'CHAR' ''  ''      '',
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
  append_sortcat : '1' 'MATNR' 'IT_WRITE' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  get_error_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_error_message USING p_text.
**---
*  READ TABLE it_mess WITH KEY msgtyp = 'E'.
*
*  IF sy-subrc EQ 0.
*    CLEAR : p_text.
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*         EXPORTING
*              msgid               = it_mess-msgid
*              msgnr               = it_mess-msgnr
*              msgv1               = it_mess-msgv1
*              msgv2               = it_mess-msgv2
*              msgv3               = it_mess-msgv3
*              msgv4               = it_mess-msgv4
*         IMPORTING
*              message_text_output = p_text.
*  ENDIF.
*ENDFORM.                    " get_error_message
*
*
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.

  LOOP AT it_itab.
    IF p_new = 'X'.
*      UPDATE ztpp_osdp SET gi_status = it_itab-gi_status
*                           mblnr = it_itab-mblnr
*                     WHERE model = it_itab-model
*                         AND body_ser = it_itab-body_ser
*                         AND porder = it_itab-porder
*                         AND resb_no = it_itab-resb_no
*                         AND resb_item = it_itab-resb_item
*                         AND comp_no = it_itab-matnr
*                         AND supply_area = it_itab-lgort
*                         AND gi_status = ' '.
      UPDATE ztpp_osdp SET gi_status = it_itab-gi_status
                            mblnr = it_itab-mblnr
                      WHERE comp_no = it_itab-matnr
                        AND supply_area = it_itab-lgort
                        AND revivied_date = it_itab-revivied_date
                        AND ( gi_status = space or gi_status is null ).

    ELSE.
      UPDATE ztpp_osdp SET gi_status = it_itab-gi_status
                            mblnr = it_itab-mblnr
                      WHERE comp_no = it_itab-matnr
                        AND supply_area = it_itab-lgort
                        AND revivied_date = it_itab-revivied_date
                        AND gi_status = 'E'.

    ENDIF.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.                    " UPDATE_TABLE
