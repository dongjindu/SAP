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
*
*
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
         vakey LIKE konh-vakey,
       END OF it_sa.

DATA : it_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.

DATA : it_konp LIKE konp OCCURS 0 WITH HEADER LINE.

DATA : it_konh LIKE konh OCCURS 0 WITH HEADER LINE.

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
**--- insert by stlim (2004/06/02)
PARAMETERS :     p_budat LIKE mkpf-budat OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS : s_ebeln FOR ekko-ebeln.
**--- end of insert
**--- blocked by stlim (2004/06/02)
*PARAMETERS : p_spmon LIKE s031-spmon DEFAULT sy-datum.
*SELECT-OPTIONS : s_budat FOR mkpf-budat,
*                 s_ebeln FOR ekko-ebeln.
**--- end of block
SELECTION-SCREEN END OF BLOCK block1.

***---
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_spmon.
*  PERFORM help_spmon.


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
  IF it_a018[] IS INITIAL.
    MESSAGE s999 WITH text-m01.
  ELSE.
    PERFORM change_conditions.
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
*--- get date
  PERFORM get_date_from_month.

*--- get scheduling agreement
  PERFORM get_scheduling_agreement.

*--- get konh
  PERFORM get_konh.

*--- get a018
  PERFORM get_a018.

*---
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
*---
  CLEAR : it_message, it_message[], it_itab, it_itab[].

  LOOP AT it_a018.
    CLEAR : it_sa.
    READ TABLE it_sa WITH KEY lifnr = it_a018-lifnr
                              matnr = it_a018-matnr
                              ekorg = it_a018-ekorg.
    CHECK sy-subrc EQ 0.     " if SA exist.
    PERFORM check_a016.
*    CHECK sy-subrc NE 0.     " if SA condition does not exist.
    PERFORM get_konp.
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
                AND ( kdatb LE sy-datum
                  AND kdate GE sy-datum ).

  LOOP AT it_sa.
    MOVE : it_sa-lifnr(10) TO it_sa-vakey(10),
           it_sa-matnr(18) TO it_sa-vakey+10(18),
           it_sa-ekorg(04) TO it_sa-vakey+28(04),
           '0'             TO it_sa-vakey+32(01).
    MODIFY it_sa.
  ENDLOOP.
ENDFORM.                    " get_scheduling_agreement

*&---------------------------------------------------------------------*
*&      Form  get_date_from_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_date_from_month.
**---
*  CLEAR : r_budat, r_budat[].
*
*  MOVE : 'I'     TO r_budat-sign,
*         'BT'    TO r_budat-option.
*
*  CONCATENATE p_spmon '01' INTO r_budat-low.
*
*  CALL FUNCTION 'MM_LAST_DAY_OF_MONTHS'
*       EXPORTING
*            day_in            = r_budat-low
*       IMPORTING
*            last_day_of_month = r_budat-high
*       EXCEPTIONS
*            day_in_no_date    = 1
*            OTHERS            = 2.
*
*  APPEND r_budat.
ENDFORM.                    " get_date_from_month

*&---------------------------------------------------------------------*
*&      Form  get_a018
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_a018.
*---
  CHECK NOT it_konh[] IS INITIAL.

  CLEAR : it_a018, it_a018[].

*  IF s_budat[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_a018
           FROM a018
            FOR ALL ENTRIES IN it_konh
          WHERE kappl EQ 'M'
            AND kschl EQ 'PB00'
            AND lifnr EQ it_konh-vakey(10)
            AND matnr EQ it_konh-vakey+10(18)
            AND ekorg EQ it_konh-vakey+28(04)
            AND esokz EQ '0'.
*            AND datbi EQ it_konh-datbi.
*            AND datab IN r_budat.
*  ELSE.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_a018
*             FROM a018
*              FOR ALL ENTRIES IN it_sa
*            WHERE kappl EQ 'M'
*              AND kschl EQ 'PB00'
*              AND lifnr EQ it_sa-lifnr
*              AND matnr EQ it_sa-matnr
*              AND ekorg EQ it_sa-ekorg
*              AND esokz EQ '0'
*              AND datab IN s_budat.
*  ENDIF.
ENDFORM.                                                    " get_a018

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
*&      Form  get_konp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_konp.
*---
  CLEAR : it_konp, it_konp[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_konp
           FROM konp
          WHERE knumh EQ it_a018-knumh.
ENDFORM.                    " get_konp

*&---------------------------------------------------------------------*
*&      Form  check_a016
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_a016.
*---
  CLEAR : a016, w_subrc.

  SELECT SINGLE * INTO a016
                  FROM a016
                 WHERE kappl EQ 'M'
                   AND kschl EQ 'PB00'
                   AND evrtn EQ it_sa-ebeln
                   AND evrtp EQ it_sa-ebelp
*                   AND datbi EQ it_a018-datbi
                   AND datab EQ it_a018-datab.

  MOVE : sy-subrc TO w_subrc.
ENDFORM.                    " check_a016

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
  DATA : l_field01(20),
         l_datab(10),
         l_datbi(10),
         l_kbetr(13),
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

  WRITE : it_a018-datab TO l_datab,
          l_99991231    TO l_datbi.
*          it_a018-datbi TO l_datbi.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'RV13A-DATAB'     l_datab,
                         ' '  'RV13A-DATBI'     l_datbi.

  LOOP AT it_konp.
    CLEAR : l_kbetr_temp.
    IF it_konp-konwa EQ '%'.
      l_kbetr_temp = it_konp-kbetr / 10.
      MOVE : l_kbetr_temp TO l_kbetr.
    ELSE.
      WRITE : it_konp-kbetr TO l_kbetr CURRENCY it_konp-konwa.
    ENDIF.
    IF sy-tabix EQ 1.
      PERFORM dynpro USING : " 'X'  'SAPMV13A'        '0201',
                             ' '  'KONP-KBETR(01)'  l_kbetr.
*                             ' '  'KONP-KPEIN(01)'  it_konp-kpein,
*                             ' '  'KONP-KMEIN(01)'  it_konp-kmein.
    ELSEIF sy-tabix EQ 2.
      PERFORM dynpro USING : " 'X'  'SAPMV13A'        '0201',
                             ' '  'KONP-KSCHL(02)'  it_konp-kschl,
                             ' '  'KONP-KBETR(02)'  l_kbetr,
*                             ' '  'KONP-KONWA(02)'  it_konp-konwa,
                             ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                             ' '  'BDC_OKCODE'      '=EINF'.
    ELSE.
      PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                             ' '  'KONP-KSCHL(02)'  it_konp-kschl,
                             ' '  'KONP-KBETR(02)'  l_kbetr.
*                             ' '  'KONP-KONWA(02)'  it_konp-konwa.
      PERFORM dynpro USING : " 'X'  'SAPMV13A'        '0201',
                             ' '  'BDC_CURSOR'      'KONP-KSCHL(02)',
                             ' '  'BDC_OKCODE'      '=EINF'.
    ENDIF.
    MOVE : it_sa-ebeln   TO it_itab-ebeln,
           it_sa-ebelp   TO it_itab-ebelp,
           it_sa-matnr   TO it_itab-matnr,
           it_sa-werks   TO it_itab-werks,
           it_sa-lgort   TO it_itab-lgort,
           it_a018-datab TO it_itab-datab,
           it_a018-datbi TO it_itab-datbi,
           it_konp-kschl TO it_itab-kschl,
           it_konp-kbetr TO it_itab-kbetr,
           it_konp-konwa TO it_itab-konwa.
    APPEND it_itab.
    CLEAR : it_itab.
  ENDLOOP.

  PERFORM dynpro USING : 'X'  'SAPMV13A'        '0201',
                         ' '  'BDC_OKCODE'      '=KDAT'.

  CLEAR : konh, l_kzust.
  SELECT SINGLE kzust INTO l_kzust
                      FROM konh
                     WHERE knumh EQ it_a018-knumh.

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
*&      Form  get_konh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_konh.
*---
  CHECK NOT it_sa[] IS INITIAL.

*---
  CLEAR : it_konh, it_konh[].

  IF sy-batch NE space.
    IF sy-uzeit GE c_uzeit_000000 AND
       sy-uzeit LE c_uzeit_035959.
      p_budat = p_budat - 1.
    ENDIF.
  ENDIF.

*---
  CLEAR : it_konh, it_konh[].

*  IF s_budat[] IS INITIAL.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_konh
           FROM konh
            FOR ALL ENTRIES IN it_sa
          WHERE erdat EQ p_budat     " IN r_budat
            AND kvewe EQ 'A'
            AND kotabnr EQ '018'
            AND kappl EQ 'M'
            AND kschl EQ 'PB00'
            AND vakey EQ it_sa-vakey.
*  ELSE.
*    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_konh
*             FROM konh
*              FOR ALL ENTRIES IN it_sa
*            WHERE erdat EQ p_budat     " IN s_budat
*              AND kvewe EQ 'A'
*              AND kotabnr EQ '018'
*              AND kappl EQ 'M'
*              AND kschl EQ 'PB00'
*              AND vakey EQ it_sa-vakey.
*  ENDIF.
ENDFORM.                    " get_konh
