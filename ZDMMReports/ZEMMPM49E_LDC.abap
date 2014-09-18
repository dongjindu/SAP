************************************************************************
* Program Name      : ZEMMPM49E_LDC
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.08.13.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K911878
* Addl Documentation:
* Description       : LDC Creation
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.08.13.     Sung-Tae Lim     UD1K911878     Initial Coding
*
*
************************************************************************

REPORT zemmpm49e_ldc NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID zmmm.


**---
INCLUDE : zrmmpmxxr_incl.

TABLES : ztmm_ldc_log.


**--- Internal Tables
DATA : it_itab LIKE a018 OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_info_item OCCURS 0,
         kappl LIKE konp-kappl,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         kpein LIKE konp-kpein,
         konwa LIKE konp-konwa,
       END OF it_info_item.

DATA : BEGIN OF it_writ OCCURS 0,
         lifnr LIKE a018-lifnr,
         matnr LIKE a018-matnr,
         datab LIKE a018-datab,
         datbi LIKE a018-datbi,
         messa(80),
         linecolor(4),     " ALV Color
       END OF it_writ.

DATA : it_ztmm_ldc_log LIKE ztmm_ldc_log OCCURS 0 WITH HEADER LINE.


**--- Variables


**--- Constants
CONSTANTS : c_kappl LIKE a018-kappl VALUE 'M',
            c_kschl_pb00 LIKE a018-kschl VALUE 'PB00',
            c_kschl_fra1 LIKE a018-kschl VALUE 'FRA1',
            c_kschl_zoth LIKE a018-kschl VALUE 'ZOTH',
            c_kschl_zoti LIKE a018-kschl VALUE 'ZOTI',
            c_esokz LIKE a018-esokz VALUE '0'.

*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

DATA : w_mode LIKE ctu_params-dismode VALUE 'N'.


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

DEFINE append_top.
  clear : w_line.
  if not &3 is initial or not &4 is initial.
    w_line-typ   = &1.
    w_line-key   = &2.
    concatenate &3 '~' &4 into w_line-info separated by space.
    append w_line to w_top_of_page.
  endif.
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

PARAMETERS : p_ekorg LIKE t024e-ekorg DEFAULT 'PU01'.
SELECT-OPTIONS : s_lifnr FOR lfa1-lifnr OBLIGATORY,
                 s_mtart FOR mara-mtart OBLIGATORY,
                 s_matnr FOR mara-matnr,
                 s_profl FOR mara-profl DEFAULT 'K'.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-002.
SELECT-OPTIONS : s_budat FOR konh-datab OBLIGATORY DEFAULT sy-datum
                                        NO-EXTENSION.
SELECTION-SCREEN SKIP.
PARAMETERS : p_kzust LIKE konh-kzust.
SELECTION-SCREEN SKIP.
PARAMETERS : p_fra1  LIKE konp-kbetr,
             p_zoth  LIKE konp-kbetr,
             p_zoti  LIKE konp-kbetr.
SELECTION-SCREEN END OF BLOCK block2.

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
    PERFORM create_info_record.
    PERFORM comment_build.
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
  CLEAR : it_itab, it_itab[].

  IF NOT s_budat-high IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
             FROM a018
            WHERE kappl EQ c_kappl
              AND kschl EQ c_kschl_pb00
              AND lifnr IN s_lifnr
              AND matnr IN s_matnr
              AND ekorg EQ p_ekorg
              AND esokz EQ c_esokz
              AND ( datbi IN s_budat
                 OR datab IN s_budat
                 OR datab LE s_budat-low AND
                    datbi GE s_budat-high ).
*              AND datbi GE s_budat-low.    " valid end
**            AND datab LE s_budat-low.    " valid from
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_itab
             FROM a018
            WHERE kappl EQ c_kappl
              AND kschl EQ c_kschl_pb00
              AND lifnr IN s_lifnr
              AND matnr IN s_matnr
              AND ekorg EQ p_ekorg
              AND esokz EQ c_esokz
              AND datbi GE s_budat-low    " valid end
              AND datab LE s_budat-low.    " valid from
  ENDIF.

*---
  LOOP AT it_itab.
    CLEAR : mara.
    SELECT SINGLE matnr INTO mara-matnr
                        FROM mara
                       WHERE matnr EQ it_itab-matnr
                         AND profl IN s_profl.
    IF sy-subrc NE 0.
      DELETE it_itab.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  create_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_info_record.
*---
  CLEAR : it_message, it_message[], it_writ, it_writ[],
          it_ztmm_ldc_log, it_ztmm_ldc_log[].

  LOOP AT it_itab.
    CLEAR : it_writ, it_ztmm_ldc_log.
    PERFORM get_info_item_condition.
    SORT it_info_item BY kschl DESCENDING.
    PERFORM change_info_condition.
    MOVE : it_itab-lifnr TO it_writ-lifnr,
           it_itab-matnr TO it_writ-matnr.
    IF ( s_budat-low GE it_itab-datab AND
         s_budat-low LE it_itab-datbi ).
      MOVE : s_budat-low   TO it_writ-datab.
    ELSE.
      MOVE : it_itab-datab TO it_writ-datab.
    ENDIF.

    IF NOT s_budat-high IS INITIAL.
      IF ( s_budat-low  GE it_itab-datab AND
           s_budat-high LE it_itab-datbi ).
        MOVE : s_budat-high TO it_writ-datbi.
*        WRITE : s_budat-high  TO l_datbi.
      ELSE.
        MOVE : it_itab-datbi TO it_writ-datbi.
*        WRITE : it_itab-datbi TO l_datbi.
      ENDIF.
    ELSE.
      MOVE : it_itab-datbi TO it_writ-datbi.
*      WRITE : it_itab-datbi TO l_datbi.
    ENDIF.

*    MOVE : it_itab-datbi TO it_writ-datbi.
    APPEND it_writ.
*---
    MOVE : p_ekorg       TO it_ztmm_ldc_log-ekorg,
           it_itab-lifnr TO it_ztmm_ldc_log-lifnr,
           it_itab-matnr TO it_ztmm_ldc_log-matnr.
    IF ( s_budat-low GE it_itab-datab AND
         s_budat-low LE it_itab-datbi ).
      MOVE : s_budat-low   TO it_ztmm_ldc_log-datab.
    ELSE.
      MOVE : it_itab-datab TO it_ztmm_ldc_log-datab.
    ENDIF.

    IF NOT s_budat-high IS INITIAL.
      IF ( s_budat-low  GE it_itab-datab AND
           s_budat-high LE it_itab-datbi ).
        MOVE : s_budat-high TO it_ztmm_ldc_log-datbi.
*        WRITE : s_budat-high  TO l_datbi.
      ELSE.
        MOVE : it_itab-datbi TO it_ztmm_ldc_log-datbi.
*        WRITE : it_itab-datbi TO l_datbi.
      ENDIF.
    ELSE.
      MOVE : it_itab-datbi TO it_ztmm_ldc_log-datbi.
*      WRITE : it_itab-datbi TO l_datbi.
    ENDIF.

*    MOVE : it_itab-datbi TO it_ztmm_ldc_log-datbi.

    MOVE : p_kzust       TO it_ztmm_ldc_log-kzust,
           p_fra1        TO it_ztmm_ldc_log-zfra1,
           p_zoth        TO it_ztmm_ldc_log-zzoth,
           p_zoti        TO it_ztmm_ldc_log-zzoti,
           '%'           TO it_ztmm_ldc_log-konwa.
    it_ztmm_ldc_log-erdat = it_ztmm_ldc_log-aedat = sy-datum.
    it_ztmm_ldc_log-erzet = it_ztmm_ldc_log-aezet = sy-uzeit.
    it_ztmm_ldc_log-ernam = it_ztmm_ldc_log-aenam = sy-uname.
    APPEND it_ztmm_ldc_log.
  ENDLOOP.

*---
  MODIFY ztmm_ldc_log FROM TABLE it_ztmm_ldc_log.
ENDFORM.                    " create_info_record

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
  w_line-info = text-003.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

*---
  append_top :
      'S' text-004 p_ekorg ' ',
      'S' text-005 s_lifnr-low s_lifnr-high,
      'S' text-006 s_mtart-low s_mtart-high,
      'S' text-007 s_matnr-low s_matnr-high,
      'S' text-008 s_profl-low s_profl-high.

  CLEAR : w_line.
  APPEND INITIAL LINE TO w_top_of_page.

  append_top :
      'S' text-009 s_budat-low s_budat-high.
*      'S' text-010 p_kzust ' '.

  CLEAR : w_line.
  MOVE : 'S'      TO w_line-typ,
         text-010 TO w_line-key,
         p_kzust  TO w_line-info.
  APPEND w_line TO w_top_of_page.

  DATA : l_text_temp(20).

  CLEAR : w_line.
  MOVE : 'S'      TO w_line-typ,
         text-011 TO w_line-key.
  WRITE p_fra1    TO l_text_temp.
  CONCATENATE l_text_temp '%' INTO w_line-info.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  MOVE : 'S'      TO w_line-typ,
         text-012 TO w_line-key.
  WRITE p_zoth    TO l_text_temp.
  CONCATENATE l_text_temp '%' INTO w_line-info.
  APPEND w_line TO w_top_of_page.

  CLEAR : w_line.
  MOVE : 'S'      TO w_line-typ,
         text-013 TO w_line-key.
  WRITE p_zoti    TO l_text_temp.
  CONCATENATE l_text_temp '%' INTO w_line-info.
  APPEND w_line TO w_top_of_page.
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
  MOVE : 'LINECOLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize.

  PERFORM build_fieldcat.
  PERFORM build_sortcat.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
       EXPORTING
            i_callback_program = w_program
            is_layout          = w_layout
            it_fieldcat        = w_fieldcat[]
            it_events          = w_eventcat[]
            it_sort            = w_sortcat[]
            i_save             = 'A'
       TABLES
            t_outtab           = it_writ
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  get_info_item_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_item_condition.
*---
  CLEAR : it_info_item, it_info_item[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_info_item
           FROM konp
          WHERE knumh EQ it_itab-knumh
            AND loevm_ko EQ space.
ENDFORM.                    " get_info_item_condition

*&---------------------------------------------------------------------*
*&      Form  change_info_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_info_condition.
*---
  SORT it_info_item BY kschl DESCENDING.

  DATA : l_field01(20),
         l_datab(10),
         l_datbi(10),
         l_kbetr(13),
         l_kpein(05),
         l_kzust LIKE konh-kzust,
         l_kbetr_temp LIKE konp-kbetr.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], l_field01,
          l_datab, l_datbi, l_kbetr.

*---
  PERFORM dynpro USING : 'X'  'SAPMM06I'        '0100',
                         ' '  'EINA-LIFNR'      it_itab-lifnr,
                         ' '  'EINA-MATNR'      it_itab-matnr,
                         ' '  'EINE-EKORG'      it_itab-ekorg,
                         ' '  'EINE-WERKS'      space,
                         ' '  'EINA-INFNR'      space,
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPMM06I'        '0101',
                         ' '  'BDC_OKCODE'      '=KO'.

  PERFORM dynpro USING : 'X'  'SAPLV14A'        '0102',
                         ' '  'BDC_OKCODE'      '=NEWD'.

  IF ( s_budat-low GE it_itab-datab AND
       s_budat-low LE it_itab-datbi ).
    WRITE : s_budat-low   TO l_datab.
  ELSE.
    WRITE : it_itab-datab TO l_datab.
  ENDIF.

  IF NOT s_budat-high IS INITIAL.
    IF ( s_budat-low  GE it_itab-datab AND
         s_budat-high LE it_itab-datbi ).
      WRITE : s_budat-high  TO l_datbi.
    ELSE.
      WRITE : it_itab-datbi TO l_datbi.
    ENDIF.
  ELSE.
    WRITE : it_itab-datbi TO l_datbi.
  ENDIF.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'RV13A-DATAB'     l_datab,
                         ' '  'RV13A-DATBI'     l_datbi.

  READ TABLE it_info_item WITH KEY kschl = c_kschl_pb00.

  IF sy-subrc EQ 0.
    CLEAR : l_kbetr.
    WRITE : it_info_item-kbetr TO l_kbetr
                               CURRENCY it_info_item-konwa.
    move: it_info_item-kpein to l_kpein.

    PERFORM dynpro USING : ' '  'KONP-KBETR(01)'  l_kbetr,
                           ' '  'KONP-KPEIN(01)'  l_kpein.
    DELETE it_info_item WHERE kschl EQ c_kschl_pb00.
*---
    MOVE : it_info_item-kbetr TO it_ztmm_ldc_log-kbetr,
           it_info_item-konwa TO it_ztmm_ldc_log-waers.
  ENDIF.

*---
  LOOP AT it_info_item.
    CLEAR : l_kbetr_temp.
    IF it_info_item-konwa EQ '%'.
      IF it_info_item-kschl EQ c_kschl_fra1.
        MOVE : p_fra1 TO l_kbetr.
      ELSEIF it_info_item-kschl EQ c_kschl_zoth.
        MOVE : p_zoth TO l_kbetr.
      ELSEIF it_info_item-kschl EQ c_kschl_zoti.
        MOVE : p_zoti TO l_kbetr.
      ELSE.
        l_kbetr_temp = it_info_item-kbetr / 10.
        MOVE : l_kbetr_temp TO l_kbetr.
      ENDIF.
    ELSE.
      WRITE : it_info_item-kbetr TO l_kbetr
                                 CURRENCY it_info_item-konwa.
    ENDIF.

    IF sy-tabix NE 1.
      PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201'.
    ENDIF.

    PERFORM dynpro USING : ' '  'KONP-KSCHL(02)'  it_info_item-kschl,
                           ' '  'KONP-KBETR(02)'  l_kbetr,
                           ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                           ' '  'BDC_OKCODE'      '=EINF'.
  ENDLOOP.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'BDC_OKCODE'      '=KDAT'.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0200',
                         ' '  'KONH-KZUST'      p_kzust,
                         ' '  'BDC_OKCODE'      '=SICH'.

*---
  CALL TRANSACTION 'ME12' USING it_bdc
                          MODE w_mode
                          UPDATE 'S'
                          MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.

  DATA : l_messa(80).

  CLEAR : it_mess, l_messa.

  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    MOVE : c_red             TO it_writ-linecolor,
           it_mess-msgtyp    TO it_ztmm_ldc_log-msgty,
           c_red             TO it_ztmm_ldc_log-linec.
    PERFORM get_message USING    it_mess-msgid
                                 it_mess-msgnr
                                 it_mess-msgv1
                                 it_mess-msgv2
                                 it_mess-msgv3
                                 it_mess-msgv4
                        CHANGING l_messa.
  ENDIF.

  READ TABLE it_mess WITH KEY msgtyp = 'S'.

  IF sy-subrc EQ 0.
    MOVE : c_green           TO it_writ-linecolor,
           it_mess-msgtyp    TO it_ztmm_ldc_log-msgty,
           c_green           TO it_ztmm_ldc_log-linec.
    PERFORM get_message USING    it_mess-msgid
                                 it_mess-msgnr
                                 it_mess-msgv1
                                 it_mess-msgv2
                                 it_mess-msgv3
                                 it_mess-msgv4
                        CHANGING l_messa.
  ENDIF.

  MOVE : l_messa TO it_writ-messa,
         l_messa TO it_ztmm_ldc_log-messa.
ENDFORM.                    " change_info_condition

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0511   text
*      -->P_ENDFORM  text
*----------------------------------------------------------------------*
FORM dynpro USING    dynbegin
                     name
                     value.
*---
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
*&      Form  get_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MESS_MSGID  text
*      -->P_IT_MESS_MSGNR  text
*      -->P_IT_MESS_MSGV1  text
*      -->P_IT_MESS_MSGV2  text
*      -->P_IT_MESS_MSGV3  text
*      -->P_IT_MESS_MSGV4  text
*      <--P_L_MESSA  text
*----------------------------------------------------------------------*
FORM get_message USING    p_msgid
                          p_msgnr
                          p_msgv1
                          p_msgv2
                          p_msgv3
                          p_msgv4
                 CHANGING p_l_messa.
*---
  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            msgid               = p_msgid
            msgnr               = p_msgnr
            msgv1               = p_msgv1
            msgv2               = p_msgv2
            msgv3               = p_msgv3
            msgv4               = p_msgv4
       IMPORTING
            message_text_output = p_l_messa.
ENDFORM.                    " get_message

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
    w_col_pos 'LIFNR' 10 'Vendor'         'CHAR' 'X' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X' ''      '',
    w_col_pos 'DATAB' 10 'Valid On'       'DATS' ''  ''      '',
    w_col_pos 'DATBI' 10 'Valid To'       'DATS' ''  ''      '',
*    w_col_pos 'KSCHL' 04 'Cond Record'    'CHAR' ''  ''      '',
**    w_col_pos 'KBETR' 12 'Amount'         'CURR' ''  ''      'KONWA',
*    w_col_pos 'KBETR' 12 'Amount'         'CURR' ''  ''      '',
*    w_col_pos 'KONWA' 05 'Currency'       'CUKY' ''  ''      '',
*    w_col_pos 'KZUST' 03 'Reason Code'    'CHAR' ''  ''      '',
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

ENDFORM.                    " build_sortcat
