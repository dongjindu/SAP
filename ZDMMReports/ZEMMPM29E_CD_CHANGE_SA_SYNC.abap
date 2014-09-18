************************************************************************
* Program Name      : ZEMMPM29E_CD_CHANGE_SA
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.05.14.
* Specifications By : Sung-Tae, Lim
* Pattern           :
* Development Request No : UD1K910127
* Addl Documentation:
* Description       : Condition Change in Scheduling Agreement
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.05.14.     Sung-Tae Lim     UD1K910127     Initial Coding
* 2005.03.01      Furong Wang      UD1K914692     Validity date
* 2005.03.17      Furong Wang      UD1K914850     Update from all info
**                                                records once condition
**                                                changed
**11/07/05        FR Wang                         Synchronize SA price
*                                                 data with Info record
*                                                 by vendor and material
* 01/13/06        FR Wang                         Disable actual run
************************************************************************


REPORT zemmpm29e_cd_change_s NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64(1)
                             MESSAGE-ID zmmm.
**---
INCLUDE : zrmmpmxxr_incl.

**--- Internal Tables
DATA : BEGIN OF it_temp OCCURS 0,
         ebeln LIKE ekko-ebeln,
         ebelp LIKE ekpo-ebelp,
         matnr LIKE ekpo-matnr,
         werks LIKE ekpo-werks,
         lgort LIKE ekpo-lgort,
         datab LIKE a018-datab,
         datbi LIKE a018-datbi,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         konwa LIKE konp-konwa,
         kzust LIKE konh-kzust,
         messa(80),
         linecolor(4),     " ALV Color
       END OF it_temp.

DATA : it_itab LIKE it_temp OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_sa OCCURS 0,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         matnr LIKE ekpo-matnr,
         werks LIKE ekpo-werks,
         lgort LIKE ekpo-lgort,
         lifnr LIKE ekko-lifnr,
         etfz1 LIKE ekpo-etfz1,
         bstyp LIKE ekko-bstyp,
         bukrs LIKE ekko-bukrs,
         bsart LIKE ekko-bsart,
         ekorg LIKE ekko-ekorg,
         ekgrp LIKE ekko-ekgrp,
         kdatb LIKE ekko-kdatb,
         kdate LIKE ekko-kdate,
       END OF it_sa.

DATA : it_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.
DATA : it_a016 LIKE a016 OCCURS 0 WITH HEADER LINE.

DATA : it_konp LIKE konp OCCURS 0 WITH HEADER LINE.

DATA : it_konh LIKE konh OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_change OCCURS 0,
         objectclas LIKE cdhdr-objectclas,
         objectid   LIKE cdhdr-objectid,
*        changenr   LIKE cdhdr-changenr,
       END OF it_change.

DATA : it_info_head LIKE konh OCCURS 0 WITH HEADER LINE.
DATA : it_info_head_temp LIKE konh OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_info_item OCCURS 0,
         kappl LIKE konp-kappl,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         kpein LIKE konp-kpein,
         konwa LIKE konp-konwa,
         lifnr LIKE konp-lifnr,
       END OF it_info_item.

DATA : it_sacond_item LIKE it_info_item OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_info_a018_temp OCCURS 0,
      lifnr LIKE ekko-lifnr,
      ebeln LIKE ekko-ebeln,
      matnr LIKE ekpo-matnr,
      datbi LIKE a018-datbi,
      datab LIKE a018-datab,
      in_kbetr LIKE konp-kbetr,
      in_kpein LIKE konp-kpein,
      sa_kpein LIKE konp-kpein,
      sa_kbetr LIKE konp-kbetr,
      mess(40),
      color(4),
      END OF it_info_a018_temp.

DATA : it_info_a018 LIKE it_info_a018_temp OCCURS 0 WITH HEADER LINE.
DATA : it_info_output LIKE it_info_a018_temp OCCURS 0 WITH HEADER LINE.

RANGES : r_budat FOR mkpf-budat.

DATA : w_subrc LIKE sy-subrc.

*--- constants
CONSTANTS : c_uzeit_000000 TYPE t VALUE '000000',
            c_uzeit_035959 TYPE t VALUE '035959'.

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
*PARAMETERS :     p_lifnr LIKE ekko-lifnr OBLIGATORY.
SELECT-OPTIONS: s_lifnr FOR ekko-lifnr OBLIGATORY.
SELECT-OPTIONS : s_matnr FOR ekpo-matnr.
PARAMETERS: p_datum LIKE sy-datum DEFAULT sy-datum OBLIGATORY.
PARAMETERS: p_run AS CHECKBOX DEFAULT 'X'.
***

*PARAMETERS :     p_budat LIKE mkpf-budat OBLIGATORY DEFAULT sy-datum.
*SELECT-OPTIONS : s_ebeln FOR ekko-ebeln.
SELECTION-SCREEN END OF BLOCK block1.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].


**---
TOP-OF-PAGE.
  PERFORM top_of_page.


**---
START-OF-SELECTION.
** ADDED ON 1/13/06
  IF P_RUN IS INITIAL.
     MESSAGE E009 WITH 'Please select TEST RUN only for this moment'.
  endif.
  p_run = 'X'.
** END

  PERFORM get_data.
  PERFORM get_info_sa.
**---
END-OF-SELECTION.
  IF it_sa[] IS INITIAL OR it_info_a018[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    IF p_run = 'X'.
      PERFORM comment_build.     " USING w_top_of_page[].
      PERFORM display_alv_grid.
    ELSE.
      PERFORM change_conditions.
      PERFORM comment_build.     " USING w_top_of_page[].
      PERFORM make_alv_grid.
    ENDIF.
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
*--- get scheduling agreement
  PERFORM get_scheduling_agreement.

*--- get changed info. record from change document
*  PERFORM get_changed_info_record.
ENDFORM.                    " get_data

*&---------------------------------------------------------------------*
*&      Form  change_conditions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_conditions.
*--- get info record condition header from konh

  CLEAR : it_message, it_message[], it_itab, it_itab[].

  LOOP AT it_info_head.
    PERFORM read_sa_with_key.
    CHECK sy-subrc EQ 0.
    PERFORM get_info_item_condition.
    PERFORM get_sa_item_condition.
    SORT it_info_item BY kschl DESCENDING.
    SORT it_sacond_item BY kschl DESCENDING.
    CHECK it_info_item[] NE it_sacond_item[].
    PERFORM change_sa_condition.
  ENDLOOP.

ENDFORM.                    " change_conditions

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
  CLEAR : w_line, w_top_of_page, w_top_of_page[].
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
*---
  CLEAR: w_fieldcat, w_fieldcat[], w_sortcat, w_sortcat[].
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
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  get_scheduling_agreement
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_scheduling_agreement.
*---
  CLEAR : it_sa, it_sa[].

  SELECT b~ebeln
         ebelp
         matnr
         werks
         lgort
         lifnr
         etfz1
         a~bstyp
         a~bukrs
         bsart
         ekorg
         ekgrp
         kdatb
         kdate
               INTO CORRESPONDING FIELDS OF TABLE it_sa
               FROM ekko AS a INNER JOIN ekpo AS b
                 ON a~mandt EQ b~mandt
                AND a~ebeln EQ b~ebeln
              WHERE a~lifnr IN s_lifnr
                AND a~bstyp EQ 'L'
                AND a~loekz EQ space
                AND b~loekz EQ space
                AND elikz EQ space
*                AND ( kdatb LE sy-datum
                  AND kdate GE sy-datum.
ENDFORM.                    " get_scheduling_agreement

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
    w_col_pos 'EBELN' 10 'SA Number'      'CHAR' 'X' ''      '',
    w_col_pos 'EBELP' 05 'Item Number'    'NUMC' 'X' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' ''  ''      '',
    w_col_pos 'WERKS' 04 'Plant'          'CHAR' ''  ''      '',
    w_col_pos 'LGORT' 04 'S/L'            'CHAR' ''  ''      '',
    w_col_pos 'DATAB' 10 'Start Date'     'DATS' ''  ''      '',
    w_col_pos 'DATBI' 10 'End Date'       'DATS' ''  ''      '',
    w_col_pos 'KSCHL' 04 'Cond Record'    'CHAR' ''  ''      '',
*    w_col_pos 'KBETR' 12 'Amount'         'CURR' ''  ''      'KONWA',
    w_col_pos 'KBETR' 12 'Amount'         'CURR' ''  ''      '',
    w_col_pos 'KONWA' 05 'Currency'       'CUKY' ''  ''      '',
    w_col_pos 'KZUST' 03 'Reason Code'    'CHAR' ''  ''      '',
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
  append_sortcat : '1' 'EBELN' 'IT_ITAB' 'X' '',
                   '2' 'EBELP' 'IT_ITAB' 'X' ''.
ENDFORM.                    " build_sortcat

*&---------------------------------------------------------------------*
*&      Form  change_sa_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_sa_condition.
*---
  SORT it_info_item BY kschl DESCENDING.

  DATA : l_field01(20),
         l_datab(10),
         l_datbi(10),
         l_kbetr(13),
         l_kpein(5),
         l_kzust LIKE konh-kzust,
         l_kbetr_temp LIKE konp-kbetr.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], l_field01,
          l_datab, l_datbi, l_kbetr.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0205',
                         ' '  'RM06E-EVRTN'     it_sa-ebeln,
                         ' '  'BDC_OKCODE'      '=AB'.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0220',
                         ' '  'RM06E-EBELP'     it_sa-ebelp,
                         ' '  'BDC_OKCODE'      '/00'.

  CONCATENATE 'RM06E-TCSELFLAG(' it_sa-ebelp+3(2) ')' INTO l_field01.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0220',
                         ' '  l_field01         'X',
                         ' '  'BDC_OKCODE'      '=KO'.

  PERFORM dynpro USING : 'X'  'SAPLV14A'        '0102',
                         ' '  'BDC_OKCODE'      '=NEWD'.

  DATA : l_99991231 TYPE d VALUE '99991231'.

  WRITE : it_info_head-datab TO l_datab,
*          l_99991231         TO l_datbi.
          it_info_head-datbi TO l_datbi.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'RV13A-DATAB'     l_datab,
                         ' '  'RV13A-DATBI'     l_datbi.

*---
  READ TABLE it_info_item WITH KEY kschl = 'PB00'.

  IF sy-subrc EQ 0.
    CLEAR : l_kbetr, l_kpein.
    WRITE : it_info_item-kbetr TO l_kbetr
                               CURRENCY it_info_item-konwa.
    WRITE : it_info_item-kpein TO l_kpein.
    PERFORM dynpro USING : ' '  'KONP-KBETR(01)'  l_kbetr,
                           ' '  'BDC_CURSOR'      'KONP-KPEIN(01)',
                           ' '  'KONP-KPEIN(01)'  l_kpein.
    MOVE : it_sa-ebeln        TO it_itab-ebeln,
           it_sa-ebelp        TO it_itab-ebelp,
           it_sa-matnr        TO it_itab-matnr,
           it_sa-werks        TO it_itab-werks,
           it_sa-lgort        TO it_itab-lgort,
           it_info_head-datab TO it_itab-datab,
           it_info_head-datbi TO it_itab-datbi,
           it_info_item-kschl TO it_itab-kschl,
           it_info_item-kbetr TO it_itab-kbetr,
           it_info_item-konwa TO it_itab-konwa.
    APPEND it_itab.
    CLEAR : it_itab.
    DELETE it_info_item WHERE kschl EQ 'PB00'.
  ENDIF.

*---
  LOOP AT it_info_item.
    CLEAR : l_kbetr_temp.
    IF it_info_item-konwa EQ '%'.
      l_kbetr_temp = it_info_item-kbetr / 10.
      MOVE : l_kbetr_temp TO l_kbetr.
    ELSE.
      WRITE : it_info_item-kbetr TO l_kbetr
                                 CURRENCY it_info_item-konwa.
    ENDIF.
    IF sy-tabix EQ 1.
*      PERFORM dynpro USING : ' '  'KONP-KBETR(01)'  l_kbetr.
*    ELSEIF sy-tabix EQ 2.
      PERFORM dynpro USING : ' '  'KONP-KSCHL(02)'  it_info_item-kschl,
                             ' '  'KONP-KBETR(02)'  l_kbetr,
                             ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                             ' '  'BDC_OKCODE'      '=PDAT'.
*                             ' '  'BDC_OKCODE'      '=EINF'.
    ELSE.
      PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                             ' '  'KONP-KSCHL(02)'  it_info_item-kschl,
                             ' '  'KONP-KBETR(02)'  l_kbetr,
                             ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                             ' '  'BDC_OKCODE'      '=PDAT'.
*                             ' '  'BDC_OKCODE'      '=EINF'.
    ENDIF.
    IF it_info_item-lifnr <> ' '.
      PERFORM dynpro USING : 'X'  'SAPMV13A'        '0300',
*                             ' '  'BDC_CURSOR'      'KONP-LINFR',
                             ' '  'BDC_OKCODE'      '/00',
                             ' '  'KONP-LIFNR'      it_info_item-lifnr,
                             ' '  'BDC_CURSOR'      'KONP-KBETR',
                              ' '  'BDC_OKCODE'      '=BACK'.
    ELSE.
      PERFORM dynpro USING : 'X'  'SAPMV13A'        '0300',
                              ' '  'BDC_OKCODE'      '=BACK'.

    ENDIF.
    PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                           ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                           ' '  'BDC_OKCODE'      '=EINF'.

    MOVE : it_sa-ebeln        TO it_itab-ebeln,
           it_sa-ebelp        TO it_itab-ebelp,
           it_sa-matnr        TO it_itab-matnr,
           it_sa-werks        TO it_itab-werks,
           it_sa-lgort        TO it_itab-lgort,
           it_info_head-datab TO it_itab-datab,
           it_info_head-datbi TO it_itab-datbi,
           it_info_item-kschl TO it_itab-kschl,
           it_info_item-kbetr TO it_itab-kbetr,
           it_info_item-konwa TO it_itab-konwa.
    IF it_itab-konwa EQ '%'.
      it_itab-kbetr = it_itab-kbetr / 10.
    ENDIF.
    APPEND it_itab.
    CLEAR : it_itab.
  ENDLOOP.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'BDC_OKCODE'      '=KDAT'.

  CLEAR : l_kzust.
  MOVE : it_info_head-kzust TO l_kzust.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0200',
                         ' '  'KONH-KZUST'      l_kzust,
                         ' '  'BDC_OKCODE'      '=BACK'.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0220',
                         ' '  'BDC_OKCODE'      '=BU'.

  PERFORM dynpro USING : 'X'  'SAPLSPO1'        '0300',
                         ' '  'BDC_OKCODE'      '=YES'.

*---
  CALL TRANSACTION 'ME32L' USING it_bdc
                           MODE w_mode
                           UPDATE 'S'
                           MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.

  DATA : l_messa(80).

  CLEAR : it_mess, l_messa.

  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    MOVE : c_red             TO it_itab-linecolor.
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
    MOVE : c_green           TO it_itab-linecolor.
    PERFORM get_message USING    it_mess-msgid
                                 it_mess-msgnr
                                 it_mess-msgv1
                                 it_mess-msgv2
                                 it_mess-msgv3
                                 it_mess-msgv4
                        CHANGING l_messa.
  ENDIF.

  MOVE : l_kzust TO it_itab-kzust,
         l_messa TO it_itab-messa.

  MODIFY it_itab TRANSPORTING kzust
                              messa
                              linecolor
                                    WHERE ebeln EQ it_sa-ebeln
                                      AND ebelp EQ it_sa-ebelp.
ENDFORM.                    " change_sa_condition

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0955   text
*      -->P_0956   text
*      -->P_0957   text
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
*&      Form  get_changed_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_changed_info_record.
**---
*  CLEAR : it_change, it_change[].
*
*  SELECT DISTINCT
*         objectclas
*         objectid
**        changenr
*                  INTO CORRESPONDING FIELDS OF TABLE it_change
*                  FROM cdhdr
*                 WHERE objectclas EQ 'COND_A'
*                   AND udate EQ p_budat
*                   AND tcode IN ('ME11', 'ME12', 'ME13').
*ENDFORM.                    " get_changed_info_record

*&---------------------------------------------------------------------*
*&      Form  get_info_condition_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_sa.
*---
  CLEAR : it_info_head, it_info_head[].
  CLEAR : it_info_head_temp, it_info_head_temp[].
  CLEAR : it_a018, it_a018[], it_a016, it_a016[].
  CLEAR: it_info_a018, it_info_a018[]. " it_sa_a016,it_sa_a016[].
  DATA: it_konp LIKE TABLE OF konp WITH HEADER LINE.
  DATA: l_sa_count TYPE i,
        l_kbetr LIKE konp-kbetr,
        l_kpein LIKE konp-kpein,
        l_lifnr LIKE ekko-lifnr,
        l_ebeln LIKE ekko-ebeln,
        l_matnr LIKE ekpo-matnr,
        wa_knumh LIKE konp-knumh.
  DATA: l_in_kbetr TYPE p,
        l_sa_kbetr TYPE p.

  SORT it_sa BY lifnr matnr.
  SELECT * INTO TABLE it_a018 FROM a018
       WHERE kappl = 'M'
         AND kschl = 'PB00'
         AND lifnr IN s_lifnr
         AND ekorg = 'PU01'
         AND matnr IN s_matnr
         AND datbi >= p_datum.

  SELECT * INTO TABLE it_konp FROM konp FOR ALL ENTRIES IN it_a018
        WHERE knumh = it_a018-knumh
          AND kschl = 'PB00'
          AND loevm_ko = space.

  SORT it_a018 BY lifnr matnr datbi.

  LOOP AT it_a018.
    MOVE-CORRESPONDING it_a018 TO it_info_a018.
    it_info_a018-color = c_red.
    READ TABLE it_konp WITH KEY knumh = it_a018-knumh.
    IF sy-subrc NE 0.
      CLEAR: it_info_a018.
      CONTINUE.
    ENDIF.
    IF it_konp-kschl = 'PB00'.
      l_in_kbetr = it_konp-kbetr / it_konp-kpein.
      it_info_a018-in_kbetr = it_konp-kbetr.
    ELSE.
      it_info_a018-in_kbetr = it_konp-kbetr.
    ENDIF.
    it_info_a018-in_kpein = it_konp-kpein.

    CLEAR: l_sa_count, l_kbetr, l_kpein.
    LOOP AT it_sa WHERE lifnr = it_a018-lifnr
                    AND matnr = it_a018-matnr.
      l_sa_count = l_sa_count + 1.
    ENDLOOP.
    CASE l_sa_count.
      WHEN 1.
        READ TABLE it_sa WITH KEY lifnr = it_a018-lifnr
                                  matnr = it_a018-matnr.
        it_info_a018-ebeln = it_sa-ebeln.
        SELECT SINGLE knumh INTO wa_knumh
                      FROM a016
                     WHERE kappl EQ 'M'
                       AND kschl EQ 'PB00'
                       AND evrtn EQ it_sa-ebeln
                       AND evrtp EQ it_sa-ebelp
                       AND datbi EQ it_a018-datbi
                       AND datab EQ it_a018-datab.
        IF sy-subrc EQ 0.
          SELECT SINGLE kbetr kpein INTO (l_kbetr, l_kpein) FROM konp
                 WHERE knumh = wa_knumh
                   AND kschl = 'PB00'
                   AND loevm_ko = space.
          IF sy-subrc EQ 0.
            IF it_konp-kschl = 'PB00'.
              l_sa_kbetr = l_kbetr / l_kpein.
              it_info_a018-sa_kbetr = l_kbetr.
            ELSE.
              it_info_a018-sa_kbetr = l_kbetr.
            ENDIF.
            IF l_in_kbetr = l_sa_kbetr.
              it_info_a018-mess = 'OK'.
              CLEAR: it_info_a018-color.
            ELSE.
              it_info_a018-mess = 'Price Different'.
            ENDIF.
            it_info_a018-sa_kpein = l_kpein.
          ELSE.
            it_info_a018-mess = 'Not Found in SA Condition'.
          ENDIF.
        ELSE.
          it_info_a018-mess = 'Validity Period Different'.
        ENDIF.
      WHEN 0.
*        it_info_a018-mess = 'No SA Found'.
        CLEAR: it_info_a018.
        CONTINUE.
      WHEN OTHERS.
        it_info_a018-mess = 'Multiple SA (not compare)'.
    ENDCASE.
    APPEND it_info_a018.
    CLEAR: it_info_a018.
  ENDLOOP.

*  l_lifnr = '*****'.
*  l_ebeln = '*****'.
*  l_MATNR = '*****'.
*  LOOP AT it_info_a018.
*    MOVE it_info_a018 TO it_info_output.
*    IF l_lifnr = it_info_a018-lifnr.
*      CLEAR: it_info_output-lifnr.
*    ELSE.
*      l_lifnr = it_info_a018-lifnr.
*    ENDIF.
*    IF l_ebeln = it_info_a018-ebeln.
*      CLEAR: it_info_output-ebeln.
*    ELSE.
*      l_ebeln = it_info_a018-ebeln.
*    ENDIF.
*    IF l_matnr = it_info_a018-matnr.
*      CLEAR: it_info_output-matnr.
*    ELSE.
*      l_matnr = it_info_a018-matnr.
*    ENDIF.
*    APPEND it_info_output.
*    CLEAR: it_info_output, it_info_a018.
*  ENDLOOP.

  it_info_output[] = it_info_a018[].

** generate it_info_head**
*  SELECT * INTO TABLE it_info_head FROM konh
*         FOR ALL ENTRIES IN it_a018
*         WHERE knumh = it_a018-knumh.

  DATA: wa_info_head LIKE konh.

  LOOP AT it_a018.
    SELECT SINGLE * INTO wa_info_head FROM konh
            WHERE knumh = it_a018-knumh.
    IF sy-subrc = 0.
      wa_info_head-datab = it_a018-datab.
      wa_info_head-datbi = it_a018-datbi.
      APPEND wa_info_head TO it_info_head.
    ENDIF.
    CLEAR: wa_info_head.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_info_head COMPARING knumh.
  CLEAR: it_a018, it_a018[].

ENDFORM.                    " get_info_condition_header

*&---------------------------------------------------------------------*
*&      Form  read_sa_with_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_sa_with_key.
*---
  CLEAR : it_sa.
  READ TABLE it_sa WITH KEY lifnr = it_info_head-vakey(10)
                            matnr = it_info_head-vakey+10(18)
                            ekorg = it_info_head-vakey+28(04).
ENDFORM.                    " read_sa_with_key

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
          WHERE knumh EQ it_info_head-knumh
            AND loevm_ko EQ space.
ENDFORM.                    " get_info_item_condition

*&---------------------------------------------------------------------*
*&      Form  get_sa_item_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sa_item_condition.
*---
  DATA : l_knumh LIKE konh-knumh.

  CLEAR : it_sacond_item, it_sacond_item[], konh.

  SELECT SINGLE knumh INTO l_knumh
                      FROM a016
                     WHERE kappl EQ 'M'
                       AND kschl EQ 'PB00'
                       AND evrtn EQ it_sa-ebeln
                       AND evrtp EQ it_sa-ebelp
                       AND datbi EQ it_info_head-datbi.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_sacond_item
           FROM konp
          WHERE knumh EQ l_knumh.
ENDFORM.                    " get_sa_item_condition

*&---------------------------------------------------------------------*
*&      Form  display_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_grid.
*---
  MOVE : 'COLOR' TO w_layout-info_fieldname,
         'X'         TO w_layout-colwidth_optimize.
*         'X'         TO w_layout-zebra.

  PERFORM build_fieldcat_display.
  PERFORM build_sortcat_display.

  CLEAR : w_program.

  MOVE : sy-repid TO w_program.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program       = w_program
*            i_callback_pf_status_set = 'SET_STATUS'
*            i_callback_user_command  = 'USER_COMMAND'
            is_layout                = w_layout
            it_fieldcat              = w_fieldcat[]
            it_events                = w_eventcat[]
            it_sort                  = w_sortcat[]
            i_save                   = 'A'
       TABLES
            t_outtab                 = it_info_output
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

ENDFORM.                    " display_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.
  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'LIFNR'.
  w_sortcat-tabname        = 'IT_INFO_OUTPUT'.
  w_sortcat-up             = 'X'.
  w_sortcat-subtot         = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'MATNR'.
  w_sortcat-tabname        = 'IT_INFO_OUTPUT'.
  w_sortcat-subtot         = 'X'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

*  append_sortcat : '1' 'LIFNR' 'IT_INFO_OUTPUT' 'X' 'X'.
ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_fieldcat_display.
  append_fieldcat :
    w_col_pos 'LIFNR' 10 'Vendor Number'  'CHAR' 'X' ''      '',
    w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X'  ''      '',
    w_col_pos 'DATAB' 10 'Start Date'     'DATS' ''  ''      '',
    w_col_pos 'DATBI' 10 'End Date'       'DATS' ''  ''      '',
    w_col_pos 'IN_KBETR' 12 'IF Amount'   'CURR' ''  ''      '',
    w_col_pos 'IN_KPEIN' 5 'Per'          'DEC' ' '  ''      '',
    w_col_pos 'EBELN' 10 'SA Number'      'CHAR' ' ' ''      '',
    w_col_pos 'SA_KBETR' 12 'SA Amount'   'CURR' ''  ''      '',
    w_col_pos 'SA_KPEIN' 5 'Per'          'DEC' ' '  ''      '',
    w_col_pos 'MESS' 40 'Message'         'CHAR' ''  ''      ''.
ENDFORM.                    " build_fieldcat_display
