************************************************************************
* Program Name      : ZEMMPM29E_CD_CHANGE_SA
* Author            : Furong Wang
* Creation Date     : 01/2006
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Condition Change in Scheduling Agreement
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 05/24/06        Furong Wang                     Condition ZPXX not
**                                                included
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
         kpein like konp-kpein,
         konwa LIKE konp-konwa,
         kzust LIKE konh-kzust,
         messa(80),
         linecolor(4),     " ALV Color
       END OF it_temp.

DATA : it_itab LIKE it_temp OCCURS 0 WITH HEADER LINE.

DATA: w_last_run LIKE sy-datum,
      w_last_time LIKE sy-uzeit,
      w_curr_time LIKE sy-uzeit,
      w_error(1).

DATA : BEGIN OF it_sa OCCURS 0,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         lifnr LIKE ekko-lifnr,
         matnr LIKE ekpo-matnr,
         werks LIKE ekpo-werks,
         lgort LIKE ekpo-lgort,
         etfz1 LIKE ekpo-etfz1,
         bstyp LIKE ekko-bstyp,
         bukrs LIKE ekko-bukrs,
         bsart LIKE ekko-bsart,
         ekorg LIKE ekko-ekorg,
         ekgrp LIKE ekko-ekgrp,
         kdatb LIKE ekko-kdatb,
         kdate LIKE ekko-kdate,
       END OF it_sa.

DATA : it_sa_all LIKE TABLE OF it_sa WITH HEADER LINE.

DATA : it_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.
DATA : it_a016 LIKE a016 OCCURS 0 WITH HEADER LINE.

DATA : it_konp LIKE konp OCCURS 0 WITH HEADER LINE.

DATA : it_konh LIKE konh OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_change OCCURS 0,
         objectclas LIKE cdhdr-objectclas,
         objectid   LIKE cdhdr-objectid,
         udate      LIKE cdhdr-udate,
         utime      LIKE cdhdr-utime,
       END OF it_change.

DATA : BEGIN OF it_info_item OCCURS 0,
         kappl LIKE konp-kappl,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         kpein LIKE konp-kpein,
         konwa LIKE konp-konwa,
         lifnr LIKE konp-lifnr,
         kzust LIKE konh-kzust,
       END OF it_info_item.

DATA : it_sacond_item LIKE it_info_item OCCURS 0 WITH HEADER LINE.

DATA: it_ztmm_ifsa_log LIKE TABLE OF ztmm_ifsa_log WITH HEADER LINE.

DATA:   it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE.

RANGES : r_budat FOR mkpf-budat.

DATA : w_subrc LIKE sy-subrc.

DATA: it_t683s LIKE TABLE OF t683s WITH HEADER LINE.

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

DATA : w_mode LIKE ctu_params-dismode VALUE 'N'.  "'A'.


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
SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-004.
*PARAMETERS: p_rerun AS CHECKBOX USER-COMMAND ucom.
PARAMETERS: p_send AS CHECKBOX DEFAULT 'X' USER-COMMAND ucom.
PARAMETERS: p_email(40) DEFAULT 'SAPRICEVAL' MODIF ID md3.
*selection-screen skip.
*parameters: r_rd1 radiobutton GROUP GP1 default 'X'.
*parameters: r_rd2 radiobutton GROUP GP1.
*
SELECTION-SCREEN END OF BLOCK block1.

SELECTION-SCREEN BEGIN OF BLOCK block2 WITH FRAME TITLE text-001.
PARAMETERS :     p_budat LIKE mkpf-budat OBLIGATORY
                 DEFAULT sy-datum MODIF ID md1.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln MODIF ID md1.
SELECTION-SCREEN END OF BLOCK block2.

SELECTION-SCREEN BEGIN OF BLOCK block3 WITH FRAME TITLE text-005.
parameters:      p_del type i default 60.
*SELECT-OPTIONS : s_lifnr FOR ekko-lifnr MODIF ID md2.
*SELECT-OPTIONS : s_matnr FOR ekpo-matnr MODIF ID md2.
SELECTION-SCREEN END OF BLOCK block3.

**---
INITIALIZATION.
  PERFORM event_build USING w_eventcat[].

AT SELECTION-SCREEN OUTPUT.
*  PERFORM select_screen_output.
**---
TOP-OF-PAGE.
  PERFORM top_of_page.

**---
START-OF-SELECTION.
*if r_rd1 = 'X'.
*  IF p_rerun IS INITIAL.
  PERFORM check_data.
  CHECK w_error IS INITIAL.
  PERFORM get_data.
*  ELSE.
*    PERFORM get_reprcs_data.
*  ENDIF.

  IF it_sa_all[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM check_cond_price.
    PERFORM change_conditions.
    PERFORM update_info_status.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM make_alv_grid.
  ENDIF.

  perform purge_table.
*ELSE.
*  SET PARAMETER ID 'VIEWNAME' FIELD 'ZTMM_CH_SA'.
*  CALL TRANSACTION 'SM30' AND SKIP FIRST SCREEN.
*ENDIF.
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
*-- get scheduling agreement
  PERFORM get_scheduling_agreement.

*-- get changed info. record from change document
  PERFORM get_changed_info_record.

*-- get condition record
  PERFORM get_info_a018.
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
  DATA: l_count TYPE n.
  DATA: it_info_item_bk LIKE TABLE OF it_info_item WITH HEADER LINE.

  CLEAR : it_message, it_message[], it_itab, it_itab[].
  CLEAR: it_ztmm_ifsa_log, it_ztmm_ifsa_log[].
  LOOP AT it_a018.
    PERFORM read_sa_with_key.
    IF sy-subrc NE 0.
      UPDATE ztmm_if_price SET sareslt = 'N'
         WHERE matnr = it_a018-matnr
           AND lifnr = it_a018-lifnr.
      IF sy-subrc EQ 0.
        COMMIT WORK.
      ELSE.
        ROLLBACK WORK.
      ENDIF.
      CONTINUE.
    ENDIF.
    PERFORM get_info_item_condition.
    CLEAR: l_count.
    DESCRIBE TABLE it_sa LINES l_count.
    IF l_count > 1.
      CLEAR: it_info_item_bk, it_info_item_bk[].
      it_info_item_bk[] = it_info_item[].
      LOOP AT it_sa.
        it_info_item[] = it_info_item_bk[].
        PERFORM get_sa_item_condition.
        SORT it_info_item BY kschl DESCENDING.
        SORT it_sacond_item BY kschl DESCENDING.
        CHECK it_info_item[] NE it_sacond_item[].
        PERFORM change_sa_condition.
      ENDLOOP.
    ELSE.
      READ TABLE it_sa INDEX 1.
      PERFORM get_sa_item_condition.
      SORT it_info_item BY kschl DESCENDING.
      SORT it_sacond_item BY kschl DESCENDING.
      CHECK it_info_item[] NE it_sacond_item[].
      PERFORM change_sa_condition.
    ENDIF.
  ENDLOOP.

  IF NOT it_ztmm_ifsa_log[] IS INITIAL.
    MODIFY ztmm_ifsa_log FROM TABLE it_ztmm_ifsa_log.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    IF p_send = 'X'.
      PERFORM send_email.
    ENDIF.
  ENDIF.

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
  CLEAR : it_sa_all, it_sa_all[].

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
               INTO CORRESPONDING FIELDS OF TABLE it_sa_all
               FROM ekko AS a INNER JOIN ekpo AS b
                 ON a~mandt EQ b~mandt
                AND a~ebeln EQ b~ebeln
               WHERE a~ebeln IN s_ebeln
                AND a~bstyp EQ 'L'
                AND a~loekz EQ space
                AND b~loekz EQ space
                AND elikz EQ space.
*                AND ( kdatb LE sy-datum
** changed on 07/11/2006
*                  AND kdate GE sy-datum.
** end of change
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
    w_col_pos 'KPEIN' 04 'Per'            'DEC' ''  ''      '',
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

  DATA:  it_ztmm_if_price LIKE TABLE OF ztmm_if_price WITH HEADER LINE.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], l_field01,
          l_datab, l_datbi, l_kbetr.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0205',
                         ' '  'RM06E-EVRTN'     it_sa-ebeln,
                         ' '  'BDC_OKCODE'      '=AB'.
*
*  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0220',
*                         ' '  'RM06E-EBELP'     it_sa-ebelp,
*                         ' '  'BDC_OKCODE'      '/00'.

  CONCATENATE 'RM06E-TCSELFLAG(' it_sa-ebelp+3(2) ')' INTO l_field01.

  PERFORM dynpro USING : 'X'  'SAPMM06E'        '0220',
                         ' '  l_field01         'X',
                         ' '  'BDC_OKCODE'      '=KO'.

  PERFORM dynpro USING : 'X'  'SAPLV14A'        '0102',
                         ' '  'BDC_OKCODE'      '=NEWD'.

  DATA : l_99991231 TYPE d VALUE '99991231'.

  WRITE : it_a018-datab TO l_datab,
*          l_99991231         TO l_datbi.
          it_a018-datbi TO l_datbi.

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
           it_a018-datab TO it_itab-datab,
           it_a018-datbi TO it_itab-datbi,
           it_info_item-kschl TO it_itab-kschl,
           it_info_item-kbetr TO it_itab-kbetr,
           it_info_item-kpein TO it_itab-kpein,
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
      WRITE : it_info_item-kpein TO l_kpein.
    ENDIF.
    IF sy-tabix EQ 1.
*      PERFORM dynpro USING : ' '  'KONP-KBETR(01)'  l_kbetr.
*    ELSEIF sy-tabix EQ 2.
*      PERFORM dynpro USING : ' '  'KONP-KBETR(01)'  l_kbetr,
*                              ' '  'BDC_CURSOR'      'KONP-KPEIN(01)',
*                              ' '  'KONP-KPEIN(01)'  l_kpein.
*
      PERFORM dynpro USING : ' '  'KONP-KSCHL(02)'  it_info_item-kschl,
                             ' '  'KONP-KBETR(02)'  l_kbetr,
** Changed by furong on 05/25/06
                             ' '  'BDC_CURSOR'      'KONP-KPEIN(02)',
                             ' '  'KONP-KPEIN(02)'  l_kpein,
*                             ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
** end of change
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
           it_a018-datab TO it_itab-datab,
           it_a018-datbi TO it_itab-datbi,
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
  MOVE : it_info_item-kzust TO l_kzust.

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
** update ztmm_if_price table
    CLEAR: it_ztmm_ifsa_log.
    MOVE-CORRESPONDING it_a018 TO it_ztmm_ifsa_log.
    it_ztmm_ifsa_log-zuser = sy-uname.
    it_ztmm_ifsa_log-zresult = 'E'.
    it_ztmm_ifsa_log-zbdat = sy-datum.
    it_ztmm_ifsa_log-ztime = sy-uzeit.
    it_ztmm_ifsa_log-zmsg = l_messa.
    APPEND it_ztmm_ifsa_log.

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
*    IF p_rerun = 'X'.
*      UPDATE ztmm_ifsa_log SET: zresult = 'S'
*                                zbdat = sy-datum
*                                ztime = sy-uzeit
*                     WHERE lifnr = it_a018-lifnr
*                       AND matnr = it_a018-matnr
*                       AND knumh = it_a018-knumh.
*      IF sy-subrc = 0.
*        COMMIT WORK.
*      ELSE.
*        ROLLBACK WORK.
*      ENDIF.
*    ENDIF.
    CLEAR: it_ztmm_if_price, it_ztmm_if_price[].
    SELECT * INTO TABLE it_ztmm_if_price
        FROM ztmm_if_price
       WHERE matnr = it_a018-matnr
         AND lifnr = it_a018-lifnr.
    SORT it_ztmm_if_price DESCENDING BY inf_d inf_time.
    READ TABLE it_ztmm_if_price INDEX 1.
    it_ztmm_if_price-sareslt = 'S'.
    MODIFY ztmm_if_price FROM it_ztmm_if_price.
    IF sy-subrc = 0.
      COMMIT WORK.
    ELSE.
      ROLLBACK WORK.
    ENDIF.

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
FORM get_info_a018.
*---
  CLEAR : it_a018, it_a018[].

  LOOP AT it_change.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_a018
             FROM a018
            WHERE knumh EQ it_change-objectid
              AND datbi >= sy-datum.
*              and datab ge sy-datum.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM it_a018.
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
  CLEAR : it_sa, it_sa[].

  LOOP AT it_sa_all WHERE lifnr = it_a018-lifnr
                      AND matnr = it_a018-matnr
                      AND ekorg = it_a018-ekorg.
    it_sa = it_sa_all.
    APPEND it_sa.
    CLEAR: it_sa.
  ENDLOOP.
*  READ TABLE it_sa WITH KEY lifnr = it_a018-lifnr
*                            matnr = it_a018-matnr
*                            ekorg = it_a018-ekorg.
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

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_info_item
  SELECT a~kappl a~kschl kbetr kpein konwa lifnr kzust
         INTO TABLE it_info_item
         FROM konp AS a
         INNER JOIN konh AS b
         ON a~knumh = b~knumh
         WHERE a~knumh EQ it_a018-knumh
           AND loevm_ko EQ space.

  LOOP AT it_info_item.
    READ TABLE it_t683s WITH KEY
                      kschl = it_info_item-kschl
                      kalsm = 'RM0002'.
    IF it_t683s-kstat = ' ' OR
        ( it_t683s-kstat = 'X' AND it_t683s-kvsl1 <> ' ' ).
      CONTINUE.
    ELSE.
      DELETE it_info_item.
    ENDIF.
  ENDLOOP.
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
                       AND datbi EQ it_a018-datbi.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_sacond_item
           FROM konp
          WHERE knumh EQ l_knumh.

  LOOP AT it_sacond_item.
    READ TABLE it_t683s WITH KEY
                kschl = it_sacond_item-kschl
                kalsm = 'RM0000'.
    IF it_t683s-kstat = ' ' OR
        ( it_t683s-kstat = 'X' AND it_t683s-kvsl1 <> ' ' ).
      CONTINUE.
    ELSE.
      DELETE it_sacond_item.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_sa_item_condition
**&---------------------------------------------------------------------
*
**&      Form  build_sortcat_display
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM build_sortcat_display.
*  w_sortcat-spos           = 1.
*  w_sortcat-fieldname      = 'LIFNR'.
*  w_sortcat-tabname        = 'IT_INFO_OUTPUT'.
*  w_sortcat-up             = 'X'.
*  w_sortcat-subtot         = 'X'.
*  APPEND w_sortcat.
*
*  w_sortcat-spos           = 2.
*  w_sortcat-fieldname      = 'MATNR'.
*  w_sortcat-tabname        = 'IT_INFO_OUTPUT'.
*  w_sortcat-subtot         = 'X'.
*  w_sortcat-up             = 'X'.
*  APPEND w_sortcat.
*
**  append_sortcat : '1' 'LIFNR' 'IT_INFO_OUTPUT' 'X' 'X'.
*ENDFORM.                    " build_sortcat_display
**&---------------------------------------------------------------------
*
**&      Form  build_fieldcat_display
**&---------------------------------------------------------------------
*
**       text
**----------------------------------------------------------------------
*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
*
*FORM build_fieldcat_display.
*  append_fieldcat :
*    w_col_pos 'LIFNR' 10 'Vendor Number'  'CHAR' 'X' ''      '',
*    w_col_pos 'MATNR' 18 'Material'       'CHAR' 'X'  ''      '',
*    w_col_pos 'DATAB' 10 'Start Date'     'DATS' ''  ''      '',
*    w_col_pos 'DATBI' 10 'End Date'       'DATS' ''  ''      '',
*    w_col_pos 'IN_KBETR' 12 'IF Amount'   'CURR' ''  ''      '',
*    w_col_pos 'IN_KPEIN' 5 'Per'          'DEC' ' '  ''      '',
*    w_col_pos 'EBELN' 10 'SA Number'      'CHAR' ' ' ''      '',
*    w_col_pos 'SA_KBETR' 12 'SA Amount'   'CURR' ''  ''      '',
*    w_col_pos 'SA_KPEIN' 5 'Per'          'DEC' ' '  ''      '',
*    w_col_pos 'MESS' 40 'Message'         'CHAR' ''  ''      ''.
*ENDFORM.                    " build_fieldcat_display
*&---------------------------------------------------------------------*
*&      Form  get_changed_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_changed_info_record.

  CLEAR : it_change, it_change[].

  SELECT DISTINCT
         objectclas
         objectid
         udate
         utime
*        changenr
                  INTO CORRESPONDING FIELDS OF TABLE it_change
                  FROM cdhdr
                 WHERE objectclas EQ 'COND_A'
                   AND udate BETWEEN w_last_run AND p_budat
                   AND tcode IN ('ME11', 'ME12', 'ME13').
  LOOP AT it_change.
    CASE it_change-udate.
      WHEN w_last_run.
        IF it_change-utime < w_last_time.
          DELETE it_change.
        ENDIF.
      WHEN p_budat.
        IF it_change-utime > w_curr_time.
          DELETE it_change.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " get_changed_info_record
*&---------------------------------------------------------------------*
*&      Form  check_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  DATA: lt_ztmm_ch_sa LIKE TABLE OF ztmm_ch_sa WITH HEADER LINE.

  SELECT * INTO TABLE lt_ztmm_ch_sa FROM ztmm_ch_sa
    WHERE prg_id = sy-repid.

  SORT lt_ztmm_ch_sa DESCENDING BY info_date info_time.
  READ TABLE lt_ztmm_ch_sa INDEX 1.
  w_last_run = lt_ztmm_ch_sa-info_date.
  w_last_time = lt_ztmm_ch_sa-info_time.

  IF w_last_run > p_budat.
    MESSAGE i999 WITH text-003 w_last_run.
    w_error = 'X'.
  ENDIF.
  IF w_last_run IS INITIAL.
    w_last_run = p_budat.
  ENDIF.

  GET TIME FIELD w_curr_time.
  w_curr_time = w_curr_time - 300.

ENDFORM.                    " check_date
*&---------------------------------------------------------------------*
*&      Form  update_info_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_info_status.
  DATA: lw_ztmm_ch_sa LIKE ztmm_ch_sa.

  lw_ztmm_ch_sa-prg_id = sy-repid.
  lw_ztmm_ch_sa-info_date = p_budat.
  lw_ztmm_ch_sa-info_time = w_curr_time.
  lw_ztmm_ch_sa-UNAME = SY-UNAME.
  INSERT ztmm_ch_sa FROM lw_ztmm_ch_sa.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " update_info_status
*&---------------------------------------------------------------------*
*&      Form  get_reprcs_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM get_reprcs_data.
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_a018
*     FROM ztmm_ifsa_log
*     WHERE lifnr IN s_lifnr
*       AND matnr IN s_matnr
*       AND zresult = 'E'.
*  IF it_a018[] IS INITIAL.
*    MESSAGE s999 WITH text-m02.
*  ENDIF.
*ENDFORM.                    " get_reprcs_data
*&---------------------------------------------------------------------*
*&      Form  select_screen_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM select_screen_output.
*  LOOP AT SCREEN.
*    IF screen-group1 = 'MD1'.
*      IF p_rerun = 'X'.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*    IF screen-group1 = 'MD2'.
*       IF p_rerun = ' '.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*    IF screen-group1 = 'MD3'.
*      IF p_send = ' '.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " select_screen_output
*&---------------------------------------------------------------------*
*&      Form  send_email
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email.

  DATA: l_subject(40) TYPE c VALUE 'Sch Agreement Price Update Error'.

  DATA:   it_packing_list LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
          it_contents LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          it_receivers LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
          it_attachment LIKE solisti1 OCCURS 0 WITH HEADER LINE,
          gd_cnt TYPE i,
          gd_sent_all(1) TYPE c,
          gd_doc_data LIKE sodocchgi1,
          gd_error TYPE sy-subrc.

  PERFORM populate_data_for_output.

  gd_doc_data-doc_size = 1.

* Populate the subject/generic message attributes
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = sy-repid.
  gd_doc_data-obj_descr = l_subject.
  gd_doc_data-sensitivty = 'F'.

* Describe the body of the message
  CLEAR it_packing_list.
  REFRESH it_packing_list.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num = 0.
  it_packing_list-body_start = 1.
  DESCRIBE TABLE it_mail LINES it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  APPEND it_packing_list.

* Add the recipients email address
  CLEAR it_receivers.
  REFRESH it_receivers.
  it_receivers-receiver = p_email.
  it_receivers-rec_type = 'C'.
  it_receivers-com_type = 'INT'.
  it_receivers-notif_del = 'X'.
  it_receivers-notif_ndel = 'X'.
  APPEND it_receivers.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = gd_doc_data
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'

    IMPORTING
      sent_to_all                = gd_sent_all
    TABLES
      packing_list               = it_packing_list
      contents_txt               = it_mail
      receivers                  = it_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

* Store function module return code

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

  gd_error = sy-subrc.

ENDFORM.                    " send_email

*---------------------------------------------------------------------*
*       FORM populate_data_for_output                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM populate_data_for_output.
  DATA: l_message TYPE so_text255.
  CLEAR: it_mail,it_mail[].

  APPEND 'Vendor    Material          ST Date End Date Con info'
       TO it_mail.

  LOOP AT it_ztmm_ifsa_log.
    CONCATENATE it_ztmm_ifsa_log-lifnr it_ztmm_ifsa_log-matnr
       INTO l_message SEPARATED BY space.
    CONCATENATE l_message it_ztmm_ifsa_log-datab it_ztmm_ifsa_log-datbi
      it_ztmm_ifsa_log-knumh
    INTO l_message SEPARATED BY space.
    APPEND l_message TO it_mail.
    CLEAR: it_mail, l_message.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_cond_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_cond_price.

  SELECT * INTO TABLE it_t683s FROM t683s
  WHERE kvewe = 'A'
    AND kappl = 'M'
    AND ( kalsm = 'RM0000' OR kalsm = 'RM0002' ).

*  delete adjacent deplicates from it_t683s.
ENDFORM.   " get_cond_price
*&---------------------------------------------------------------------*
*&      Form  purge_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form purge_table.
  data: l_date like sy-datum.
  l_date = sy-datum - p_del.
  delete from ZTMM_CH_SA where INFO_DATE < l_date.
  if sy-subrc = 0.
    commit work.
  else.
    rollback work.
  endif.
  delete from ZTMM_IFSA_LOG where ZBDAT < l_date.
  if sy-subrc = 0.
    commit work.
  else.
    rollback work.
  endif.

endform.                    " purge_table
