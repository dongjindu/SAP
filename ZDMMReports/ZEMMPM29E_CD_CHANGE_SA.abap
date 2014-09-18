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
**01/13/2006      Furong Wang                     Updated last change
*                                                 and valid date in the
*                                                 future
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

DATA : it_konp LIKE konp OCCURS 0 WITH HEADER LINE.

DATA : it_konh LIKE konh OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_change OCCURS 0,
         objectclas LIKE cdhdr-objectclas,
         objectid   LIKE cdhdr-objectid,
*        changenr   LIKE cdhdr-changenr,
       END OF it_change.

DATA : it_info_head LIKE konh OCCURS 0 WITH HEADER LINE.
DATA : it_info_head_temp LIKE konh OCCURS 0 WITH HEADER LINE.
DATA : it_info_head_new LIKE konh OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_info_item OCCURS 0,
         kappl LIKE konp-kappl,
         kschl LIKE konp-kschl,
         kbetr LIKE konp-kbetr,
         konwa LIKE konp-konwa,
         lifnr LIKE konp-lifnr,
         kpein like konp-kpein,
       END OF it_info_item.

DATA : it_sacond_item LIKE it_info_item OCCURS 0 WITH HEADER LINE.


RANGES : r_budat FOR mkpf-budat.


DATA : w_subrc LIKE sy-subrc.


DATA:   it_mail TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
                  WITH HEADER LINE.

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
PARAMETERS :     p_budat LIKE mkpf-budat OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln.
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
  IF it_change[] IS INITIAL OR it_sa[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM change_conditions.
    PERFORM comment_build.     " USING w_top_of_page[].
    PERFORM send_email_to_users.
*    PERFORM UPDATE_LOG_INFO.
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
*--- get scheduling agreement
  PERFORM get_scheduling_agreement.

*--- get changed info. record from change document
  PERFORM get_changed_info_record.
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
  PERFORM get_info_condition_header.

*---
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
            t_outtab           = it_itab
       EXCEPTIONS
            program_error      = 1
            OTHERS             = 2.
ENDFORM.                    " make_alv_grid

*&---------------------------------------------------------------------*
*&      Form  help_spmon
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM help_spmon.
***--- This Subroutine copied from Include program 'RMCS0F0M'
*
*  DATA: BEGIN OF mf_dynpfields OCCURS 1.
*          INCLUDE STRUCTURE dynpread.
*  DATA: END   OF mf_dynpfields.
*  DATA: mf_returncode   LIKE sy-subrc,
*        mf_monat        LIKE isellist-month,
*        mf_hlp_repid    LIKE sy-repid.
*  FIELD-SYMBOLS: <mf_feld>.
*
** Wert von Dynpro lesen
*  GET CURSOR FIELD mf_dynpfields-fieldname.
*  APPEND mf_dynpfields.
*  mf_hlp_repid = sy-repid.
*  DO 2 TIMES.
*    CALL FUNCTION 'DYNP_VALUES_READ'
*         EXPORTING
*              dyname               = mf_hlp_repid
*              dynumb               = sy-dynnr
*         TABLES
*              dynpfields           = mf_dynpfields
*         EXCEPTIONS
*              invalid_abapworkarea = 01
*              invalid_dynprofield  = 02
*              invalid_dynproname   = 03
*              invalid_dynpronummer = 04
*              invalid_request      = 05
*              no_fielddescription  = 06
*              undefind_error       = 07.
*    IF sy-subrc = 3.
**     Aktuelles Dynpro ist Wertemengenbild
*      mf_hlp_repid = 'SAPLALDB'.
*    ELSE.
*      READ TABLE mf_dynpfields INDEX 1.
**     Unterstriche durch Blanks ersetzen
*      TRANSLATE mf_dynpfields-fieldvalue USING '_ '.
*      EXIT.
*    ENDIF.
*  ENDDO.
*  IF sy-subrc = 0.
**   Konvertierung ins interne Format
*    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
*         EXPORTING
*              input         = mf_dynpfields-fieldvalue
*         IMPORTING
*              output        = mf_monat
*         EXCEPTIONS
*              error_message = 1.
*    IF mf_monat IS INITIAL.
**     Monat ist initial => Vorschlagswert aus akt. Datum ableiten
*      mf_monat = sy-datlo(6).
*    ENDIF.
*    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
*         EXPORTING
*              actual_month               = mf_monat
*         IMPORTING
*              selected_month             = mf_monat
*              return_code                = mf_returncode
*         EXCEPTIONS
*              factory_calendar_not_found = 01
*              holiday_calendar_not_found = 02
*              month_not_found            = 03.
*    IF sy-subrc = 0 AND mf_returncode = 0.
**     ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <MF_FELD>. " ==>> note 148804
**     <MF_FELD> = MF_MONAT.
*      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
*           EXPORTING
*                input  = mf_monat
*           IMPORTING
*                output = mf_dynpfields-fieldvalue.
*      COLLECT mf_dynpfields.
*      CALL FUNCTION 'DYNP_VALUES_UPDATE'
*           EXPORTING
*                dyname               = mf_hlp_repid
*                dynumb               = sy-dynnr
*           TABLES
*                dynpfields           = mf_dynpfields
*           EXCEPTIONS
*                invalid_abapworkarea = 01
*                invalid_dynprofield  = 02
*                invalid_dynproname   = 03
*                invalid_dynpronummer = 04
*                invalid_request      = 05
*                no_fielddescription  = 06
*                undefind_error       = 07. "<<== note 148804
*    ENDIF.
*  ENDIF.
ENDFORM.                    " help_spmon

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
              WHERE a~ebeln IN s_ebeln
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
         l_kzust LIKE konh-kzust,
         l_kbetr_temp LIKE konp-kbetr,
         l_kpein(5).

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
    CLEAR : l_kbetr.
    WRITE : it_info_item-kbetr TO l_kbetr
                               CURRENCY it_info_item-konwa.
    l_kpein = it_info_item-kpein.
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
FORM get_changed_info_record.
*---
  CLEAR : it_change, it_change[].

  SELECT DISTINCT
         objectclas
         objectid
*        changenr
                  INTO CORRESPONDING FIELDS OF TABLE it_change
                  FROM cdhdr
                 WHERE objectclas EQ 'COND_A'
                   AND udate EQ p_budat
                   AND tcode IN ('ME11', 'ME12', 'ME13').
ENDFORM.                    " get_changed_info_record

*&---------------------------------------------------------------------*
*&      Form  get_info_condition_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_info_condition_header.
*---
  DATA:  l_vakey LIKE konh-vakey,
         l_date LIKE sy-datum.

  CLEAR : it_info_head, it_info_head[].
  CLEAR : it_info_head_new, it_info_head_new[].
  CLEAR : it_info_head_temp, it_info_head_temp[].
  CLEAR : it_a018, it_a018[].

  LOOP AT it_change.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_info_head_temp
             FROM konh
            WHERE knumh EQ it_change-objectid
              AND datbi GE sy-datum.
*              and datab ge sy-datum.
  ENDLOOP.
*** CHANGED

  SELECT * INTO TABLE it_info_head FROM konh FOR ALL ENTRIES IN
               it_info_head_temp WHERE vakey = it_info_head_temp-vakey.

  SELECT * INTO TABLE it_a018 FROM a018 FOR ALL ENTRIES IN
               it_info_head WHERE knumh = it_info_head-knumh.

  SORT it_a018 BY knumh.
  LOOP AT it_info_head.
    READ TABLE it_a018 WITH KEY knumh = it_info_head-knumh.
    IF sy-subrc <> 0.
      DELETE it_info_head.
    ENDIF.
  ENDLOOP.
*** END OF CHANGE
  SORT it_info_head BY vakey datab DESCENDING.

  l_vakey = '****'.
  LOOP AT it_info_head.
    IF it_info_head-vakey <> l_vakey.
      l_vakey = it_info_head-vakey.
      MOVE it_info_head TO it_info_head_new.
      APPEND it_info_head_new.
      CLEAR: it_info_head, it_info_head_new.
    ELSE.
      IF it_info_head-datab >= sy-datum.
        MOVE it_info_head TO it_info_head_new.
        APPEND it_info_head_new.
        CLEAR: it_info_head, it_info_head_new.
      ENDIF.
    ENDIF.
  ENDLOOP.

  it_info_head[] = it_info_head_new[].

  SORT it_info_head BY knumh.
*  SORT it_info_head BY datab DESCENDING.
*  DELETE it_info_head FROM 2.

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
  LOOP AT it_info_item.
    IF it_info_item-KSCHL+0(2) = 'ZP'.
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
                       AND datbi EQ it_info_head-datbi.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_sacond_item
           FROM konp
          WHERE knumh EQ l_knumh.
ENDFORM.                    " get_sa_item_condition
*&---------------------------------------------------------------------*
*&      Form  send_email_to
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email_to_users.
  PERFORM send_out.
ENDFORM.           " send_email_t_ousers

*---------------------------------------------------------------------*
*       FORM send_out                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM send_out.
  DATA: l_subject(40) TYPE c VALUE  'Schdule Agreement Price Updated',
        l_email(40)   TYPE c VALUE 'furongwang@hmmausa.com' .

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
  it_receivers-receiver = l_email.
  it_receivers-rec_type = 'U'.
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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  populate_data_for_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM populate_data_for_output.
  DATA: l_message TYPE so_text255,
        l_kbetr(13).

  APPEND 'PO Number' TO it_mail.
  CLEAR: it_mail.
  LOOP AT it_itab.
    l_kbetr = it_itab-kbetr.
    CONCATENATE it_itab-ebeln it_itab-ebelp it_itab-matnr it_itab-werks
      it_itab-lgort it_itab-datab INTO l_message SEPARATED BY space.
    CONCATENATE l_message it_itab-datbi it_itab-kschl l_kbetr
      it_itab-konwa it_itab-kzust INTO l_message SEPARATED BY space.
    APPEND l_message TO it_mail.
    CLEAR: it_mail, l_message, l_kbetr.
  ENDLOOP.
ENDFORM.                    " populate_data_for_output
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LOG_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_log_info.
  DATA: lt_ztmm_ch_sa LIKE ztmm_ch_sa.
*  L_PRG_ID LIKE ZTMM_CH_SA-PRG_ID,
*        L_RUN_DATE LIKE ZTMM_CH_SA-RUN_DATE.
*  LT_ZTMM_CH_SA-PRG_ID = SY-REPID.
*   LT_ZTMM_CH_SA-RUN-DATE = p_budat.

  COMMIT WORK.
ENDFORM.                    " UPDATE_LOG_INFO
