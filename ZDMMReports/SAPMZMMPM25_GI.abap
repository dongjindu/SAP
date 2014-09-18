************************************************************************
* Program Name      : SAPMZMMPM25_GI
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.10.09.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : MM Goods Issue with WM
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.09.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT  sapmzmmpm25_gi MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

TYPE-POOLS : cxtab.

TABLES : migo_cust_fields.

**---
DATA : BEGIN OF it_itab OCCURS 0,
         matnr LIKE mseg-matnr,
         erfmg LIKE mseg-erfmg,
         erfme LIKE mseg-erfme,
         lgtyp LIKE mseg-lgtyp,
         lgpla LIKE mseg-lgpla,
         lgort LIKE mseg-lgort,
         werks LIKE mseg-werks,
         wempf LIKE mseg-wempf,
         grund LIKE mseg-grund,
       END OF it_itab.

DATA : BEGIN OF it_itab_copy OCCURS 0.
        INCLUDE STRUCTURE it_itab.
DATA : END OF it_itab_copy.

DATA : it_text(220) OCCURS 0 WITH HEADER LINE.

**---
DATA : w_okcode LIKE sy-ucomm,
       w_save_okcode LIKE sy-ucomm,
       w_index LIKE sy-index,
       w_loopc LIKE sy-loopc,
       w_top_line TYPE i VALUE 1,
       w_lines TYPE i,
       w_selected(1).

DATA : xkzbew      TYPE c VALUE ' ', "WA-Pos. im WE-Beleg manuell erf.
       old_bwartwa LIKE rm07m-bwart,    "gem. WA-Bewegungsart
       old_sobkz   LIKE mseg-sobkz,
       old_bwartwe LIKE rm07m-bwart,    "gem. WE-Bewegungsart
       anf_bild    TYPE c VALUE ' ', "Anforderungsbild
       xkzbw       TYPE c,               " KzBwa ist relevant
       xfausw      TYPE c.               " Feldauswahl

DATA : w_mode LIKE ctu_params-dismode VALUE 'N'.

DATA : w_erfmg LIKE mseg-erfmg.

DATA : w_mblnr LIKE mseg-mblnr,
       w_tanum LIKE ltak-tanum,
       w_subrc LIKE sy-subrc.

DATA : w_complete(1),
       w_answer(1).

DATA : w_refer TYPE i,
       w_string LIKE dm07m-fausw,
       w_werks_hide(1).

DATA : w_text_kostl(50),
       w_text_aufnr(50),
       wa_cobl LIKE cobl.

DATA : w_ccntr_mand(1),
       w_order_mand(1).


*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

**--- Column Invisible
DATA : w_tab TYPE cxtab_column.

**---
CONSTANTS : BEGIN OF hlp,
              b(4) TYPE c VALUE 'HLPB',
              f(4) TYPE c VALUE 'HLPF',
              g(4) TYPE c VALUE 'HLPG',
              m(4) TYPE c VALUE 'HLPM',
              r(4) TYPE c VALUE 'HLPR',
              v(4) TYPE c VALUE 'HLPV',
            END OF hlp.

CONSTANTS : c_mode_on(1) VALUE '1',
            c_mode_off(1) VALUE '0',
            c_plus(1) VALUE '+',
            c_punc(1) VALUE '.',
            c_mb1a LIKE sy-tcode VALUE 'MB1A',
*            c_kokrs like ska1-kokrs value 'H201',
            c_bukrs LIKE t001-bukrs VALUE 'H201'.

CONSTANTS : c_grund(5) VALUE 'GRUND'.




*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CASE sy-dynnr.
    WHEN '9000'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          LEAVE TO SCREEN 0.
      ENDCASE.
    WHEN '9100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          IF w_complete EQ space AND NOT it_itab[] IS INITIAL.
            PERFORM exit_comfirm USING w_answer.
            CASE w_answer.
              WHEN 'J'.
                PERFORM save_document.
              WHEN 'N'.
                LEAVE TO TRANSACTION sy-tcode.
              WHEN OTHERS.
            ENDCASE.
          ELSE.
            LEAVE TO SCREEN 0.
          ENDIF.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  status_scrcom  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_scrcom OUTPUT.
**---
  CASE sy-dynnr.
    WHEN '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN '9100'.
      READ TABLE it_message WITH KEY msgtyp = 'E'.
      IF sy-subrc EQ 0.
        SET PF-STATUS '9100'.
      ELSE.
        SET PF-STATUS '9100' EXCLUDING 'LOG'.
      ENDIF.
      SET TITLEBAR  '9100'.
  ENDCASE.
ENDMODULE.                 " status_scrcom  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  datum_hilfe  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE datum_hilfe INPUT.
* DATA-Definitionen
  DATA: dynp_name  LIKE sy-repid.
  DATA: dynp_numb  LIKE sy-dynnr.
  DATA: field_name LIKE am61k-fname.
  DATA: help_date  LIKE sy-datum.
  DATA: loc_werks  LIKE t001w-werks.
  DATA: BEGIN OF dynp_fields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF dynp_fields.
  FIELD-SYMBOLS: <date>.

* Festwerte
  dynp_name = sy-repid.
  dynp_numb = sy-dynnr.
  dynp_fields-fieldname = 'RM07M-WERKS'.
  APPEND dynp_fields.

* Werk vom Dynpro lesen
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname     = dynp_name
            dynumb     = dynp_numb
       TABLES
            dynpfields = dynp_fields
       EXCEPTIONS
            OTHERS     = 1.
  IF sy-subrc IS INITIAL.
    READ TABLE dynp_fields INDEX 1.
    MOVE dynp_fields-fieldvalue TO loc_werks.
  ENDIF.

* Werkskalender lesen
  CLEAR t001w.
  SELECT SINGLE * FROM t001w WHERE werks = loc_werks.

* F4-Datumsauswahl
  CALL FUNCTION 'GET_DATE_FROM_CALID'
       EXPORTING
            calid      = t001w-fabkl
            repid      = dynp_name
            dynnr      = dynp_numb
       IMPORTING
            field_name = field_name
            new_date   = help_date.

* Selektiertes Datum übernehmen
  ASSIGN TABLE FIELD (field_name) TO <date>.
  MOVE help_date TO <date>.
ENDMODULE.                 " datum_hilfe  INPUT

*&---------------------------------------------------------------------*
*&      Module  bwart_hilfe  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE bwart_hilfe INPUT.
  DATA: BEGIN OF xdynpread OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END OF xdynpread.
  DATA: loc_repid LIKE sy-repid.
  DATA: loc_dynnr LIKE sy-dynnr.
  loc_repid = sy-repid.
  loc_dynnr = sy-dynnr.
  IF t158-kzbew IS INITIAL OR xkzbew = 1.
    MOVE 'RM07M-BWARTWA' TO xdynpread-fieldname.
  ELSE.
    MOVE 'RM07M-BWARTWE' TO xdynpread-fieldname.
  ENDIF.
  APPEND xdynpread.
  MOVE 'RM07M-SOBKZ' TO xdynpread-fieldname.
  APPEND xdynpread.
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            dyname               = loc_repid
            dynumb               = loc_dynnr
       TABLES
            dynpfields           = xdynpread
       EXCEPTIONS
            invalid_abapworkarea = 1
            invalid_dynprofield  = 2
            invalid_dynproname   = 3
            invalid_dynpronummer = 4
            invalid_request      = 5
            no_fielddescription  = 6
            invalid_parameter    = 7
            undefind_error       = 8
            OTHERS               = 9.
  LOOP AT xdynpread.
    IF xdynpread-fieldname EQ 'RM07M-SOBKZ'.
      rm07m-sobkz = xdynpread-fieldvalue.
    ELSE.
      rm07m-bwart = xdynpread-fieldvalue.
    ENDIF.
  ENDLOOP.

  IF t158-kzbew IS INITIAL OR xkzbew = 1.
    IF old_bwartwa IS INITIAL.
      old_bwartwa = rm07m-bwart.
      old_sobkz = rm07m-sobkz.
    ENDIF.
  ELSE.
    IF old_bwartwe IS INITIAL.
      old_bwartwe = rm07m-bwart.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'MB_SELECT_MOVEMENT_TYPE'
       EXPORTING
            tcode    = t158-tcode
            hilfe    = hlp-m
            anfbi    = anf_bild
            xkzbw    = xkzbw
            xkzbew   = xkzbew
            kzbew    = t158-kzbew
            ext_call = 'X'
       IMPORTING
            bwart    = rm07m-bwart
            sobkz    = rm07m-sobkz.
  IF NOT rm07m-bwart IS INITIAL.
    IF t158-kzbew IS INITIAL OR xkzbew = 1.
      rm07m-bwartwa = rm07m-bwart.
    ELSE.
      rm07m-bwartwe = rm07m-bwart.
    ENDIF.
  ELSE.
    IF t158-kzbew IS INITIAL OR xkzbew = 1.
      rm07m-bwart = rm07m-bwartwa = old_bwartwa.
      rm07m-sobkz = old_sobkz.
      SET PARAMETER ID 'BWA' FIELD rm07m-bwart.
    ELSE.
      rm07m-bwart = rm07m-bwartwe = old_bwartwe.
      SET PARAMETER ID 'BWE' FIELD rm07m-bwart.
    ENDIF.
  ENDIF.
* HIER DYNP_VALUES_UPDATE, DA PBO ab 3.1 NICHT MEHR PROZESSIERT WIRD
  LOOP AT xdynpread.
    IF xdynpread-fieldname EQ 'RM07M-SOBKZ'.
      xdynpread-fieldvalue = rm07m-sobkz.
    ELSE.
      xdynpread-fieldvalue = rm07m-bwart.
    ENDIF.
    MODIFY xdynpread.
  ENDLOOP.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            dyname     = loc_repid
            dynumb     = loc_dynnr
       TABLES
            dynpfields = xdynpread
       EXCEPTIONS
            OTHERS     = 4.
ENDMODULE.                 " bwart_hilfe  INPUT

*&---------------------------------------------------------------------*
*&      Module  transaction_init  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE transaction_init OUTPUT.
**---
  IF t158 IS INITIAL.
    SELECT SINGLE * FROM t158 WHERE tcode EQ c_mb1a.
  ENDIF.

***---
*  IF rm07m-bwartwa IS INITIAL.
*    MOVE : '311' TO rm07m-bwartwa.
*  ENDIF.

**---
  IF mkpf-bldat IS INITIAL.
    MOVE : sy-datum TO mkpf-bldat.
  ENDIF.

**---
  IF mkpf-budat IS INITIAL.
    MOVE : sy-datum TO mkpf-budat.
  ENDIF.

**---
  MOVE : 'X' TO anf_bild.
ENDMODULE.                 " transaction_init  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  check_input_value_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_input_value_scr9000 INPUT.
**---
  IF NOT rm07m-bwartwa IS INITIAL.
    CLEAR : t156.
    SELECT SINGLE * FROM t156
                   WHERE bwart EQ rm07m-bwartwa.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'RM07M-BWARTWA'.
      MESSAGE e146(m7) WITH text-003 rm07m-bwartwa.
    ELSE.
      CLEAR : t156t, dm07m-btext.
      SELECT SINGLE btext INTO dm07m-btext
                          FROM t156t
                         WHERE spras EQ sy-langu
                           AND bwart EQ rm07m-bwartwa.
      MOVE : rm07m-bwartwa TO msegk-bwart.
    ENDIF.
  ENDIF.

*---
  IF NOT rm07m-werks IS INITIAL.
    CLEAR : t001w.
    SELECT SINGLE * FROM t001w
                   WHERE werks EQ rm07m-werks.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'RM07M-WERKS'.
      MESSAGE e146(m7) WITH text-001 rm07m-werks.
    ENDIF.
  ENDIF.

*---
  IF NOT rm07m-lgort IS INITIAL.
    CLEAR : t001l.
    SELECT SINGLE * FROM t001l
                   WHERE werks EQ rm07m-werks
                     AND lgort EQ rm07m-lgort.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'RM07M-LGORT'.
      MESSAGE e146(m7) WITH text-002 rm07m-lgort.
    ENDIF.
  ENDIF.
ENDMODULE.                 " check_input_value_scr9000  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_scrcom  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_scrcom INPUT.
**---
  MOVE : w_okcode TO w_save_okcode.

  CLEAR : w_okcode.

  CASE sy-dynnr.
    WHEN '9000'.
      CASE w_save_okcode.
        WHEN 'ENTER'.
          PERFORM get_field_selection.
          CLEAR : w_ccntr_mand, w_order_mand.
*--- if cost center field is mandatory : 201/202
          IF t156b-fausw+9(1) EQ '+'.         " mandatory
            MOVE : 'X' TO w_ccntr_mand.
          ELSEIF t156b-fausw+9(1) EQ '.'.     " optional
            MOVE : 'O' TO w_ccntr_mand.
          ELSEIF t156b-fausw+9(1) EQ '-'.     " supress
            MOVE : ' ' TO w_ccntr_mand.
          ENDIF.
*--- if order field is mandatory : 261/262
          IF t156b-fausw+10(1) EQ '+'.        " mandatory
            MOVE : 'X' TO w_order_mand.
          ELSEIF t156b-fausw+10(1) EQ '.'.    " optional
            MOVE : 'O' TO w_order_mand.
          ELSEIF t156b-fausw+10(1) EQ '-'.    " supress
            MOVE : ' ' TO w_order_mand.
          ENDIF.
          CALL SCREEN 9100.
      ENDCASE.
    WHEN '9100'.
      CASE w_save_okcode.
        WHEN 'SAVE'.
          CLEAR : w_save_okcode.
          IF it_itab[] IS INITIAL.
            MESSAGE w061(m7).
          ELSE.
            PERFORM save_document.
          ENDIF.
        WHEN 'ITEM'.
          CLEAR : w_save_okcode.
          PERFORM new_items.
        WHEN 'LOG'.
          CLEAR : w_save_okcode.
          PERFORM display_error_log.
        WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
          PERFORM table_control_page_scrol USING w_save_okcode
                                                 w_top_line
                                                 w_lines
                                                 w_loopc.
*          PERFORM step_loop_page_scroll USING w_save_okcode
*                                              w_top_line
*                                              w_lines
*                                              w_loopc.
          CLEAR : w_save_okcode.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " user_command_scrcom  INPUT

*&---------------------------------------------------------------------*
*&      Module  display_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_scr9100 OUTPUT.
**---
  w_index = w_top_line + sy-stepl - 1.

  READ TABLE it_itab INDEX w_index.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_itab TO mseg.
    PERFORM get_material_desc USING mseg-matnr.
  ELSE.
    IF mseg-werks EQ space.
      MOVE : rm07m-werks TO mseg-werks.
    ENDIF.
    IF mseg-lgort EQ space.
      MOVE : rm07m-lgort TO mseg-lgort.
    ENDIF.
    IF mseg-grund EQ space.
      MOVE : rm07m-grund TO mseg-grund.
    ENDIF.
  ENDIF.

  MOVE w_index               TO mseg-zeile.

*--- set movement type
  MOVE : rm07m-bwartwa       TO mseg-bwart.

*--- set Warehouse Number
  CLEAR : t320.
  SELECT SINGLE lgnum INTO mseg-lgnum
                      FROM t320
                     WHERE werks EQ mseg-werks
                       AND lgort EQ mseg-lgort.

*---
  MOVE : sy-loopc TO w_loopc.
ENDMODULE.                 " display_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  display_count_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_count_scr9100 OUTPUT.
**---
  DESCRIBE TABLE it_itab LINES w_lines.

  MOVE : w_lines     TO rm07m-posnm,
         w_top_line  TO rm07m-posnr.
ENDMODULE.                 " display_count_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  step_loop_page_scroll
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SAVE_OKCODE  text
*      -->P_W_TOP_LINE  text
*      -->P_W_LINES  text
*      -->P_W_LOOPC  text
*----------------------------------------------------------------------*
FORM step_loop_page_scroll USING    p_save_okcode
                                    p_top_line
                                    p_lines
                                    p_loopc.
*---
  CALL FUNCTION 'SCROLLING_IN_TABLE'
       EXPORTING
            entry_act             = p_top_line
            entry_from            = 1
            entry_to              = p_lines
*           LAST_PAGE_FULL        = 'X'
            loops                 = p_loopc
            ok_code               = p_save_okcode
            overlapping           = 'X'
       IMPORTING
            entry_new             = p_top_line
       EXCEPTIONS
            no_entry_or_page_act  = 1
            no_entry_to           = 2
            no_ok_code_or_page_go = 3
            OTHERS                = 4.
ENDFORM.                    " step_loop_page_scroll

*&---------------------------------------------------------------------*
*&      Form  save_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_document.
**---
  IF w_mblnr EQ space AND w_tanum EQ space.
    PERFORM material_document_posting.
  ENDIF.

*---
  IF sy-subrc EQ 0 AND sy-msgty EQ 'S'.
    READ TABLE it_mess WITH KEY msgid = 'M7'
                                msgnr = '060'.
    IF sy-subrc EQ 0.
      CLEAR : w_mblnr, w_subrc.
      MOVE : it_mess-msgv1 TO w_mblnr.
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
            CLEAR : w_tanum.
            MOVE : it_mess-msgv1 TO w_tanum.
            MESSAGE s021 WITH w_mblnr w_tanum.
            LEAVE TO TRANSACTION sy-tcode.
          ENDIF.
        ELSE.
          PERFORM display_error_message.
        ENDIF.
      ELSE.
        MESSAGE s060(m7) WITH w_mblnr.
        LEAVE TO TRANSACTION sy-tcode.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM display_error_message.
  ENDIF.

*---
  MOVE : 'X' TO w_complete.
ENDFORM.                    " save_document

*&---------------------------------------------------------------------*
*&      Module  check_input_scr9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_input_scr9100 INPUT.
**---
  w_index = w_top_line + sy-stepl - 1.

  READ TABLE it_itab INDEX w_index.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING mseg TO it_itab.
    IF it_itab-matnr NE space.
      MODIFY it_itab INDEX w_index.
    ELSE.
      DELETE it_itab INDEX w_index.
    ENDIF.
  ELSE.
    MOVE-CORRESPONDING mseg TO it_itab.
    IF it_itab-matnr NE space.
      APPEND it_itab.
    ENDIF.
  ENDIF.

*---
  CHECK mseg-matnr NE space.

  IF mseg-erfmg IS INITIAL.
    SET CURSOR FIELD 'MSEG-ERFMG' LINE sy-stepl.
    MESSAGE e999 WITH text-004.
  ENDIF.

  IF mseg-erfme IS INITIAL.
    SET CURSOR FIELD 'MSEG-ERFME' LINE sy-stepl.
    MESSAGE e999 WITH text-005.
  ENDIF.

  IF mseg-grund IS INITIAL.
    IF t156b-fausw+62(1) EQ '+'.
      SET CURSOR FIELD 'MSEG-GRUND' LINE sy-stepl.
      MESSAGE e999 WITH text-011.
    ENDIF.
  ENDIF.

*  IF mseg-lgtyp IS INITIAL.
*    SET CURSOR FIELD 'MSEG-LGTYP' LINE sy-stepl.
*    MESSAGE e999 WITH text-006.
*  ENDIF.
*
*  IF mseg-lgpla IS INITIAL.
*    SET CURSOR FIELD 'MSEG-LGPLA' LINE sy-stepl.
*    MESSAGE e999 WITH text-007.
*  ENDIF.
ENDMODULE.                 " check_input_scr9100  INPUT

*&---------------------------------------------------------------------*
*&      Form  material_document_posting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM material_document_posting.
**---
  DATA : l_bldat(10),
         l_budat(10),
         l_erfmg(12),
         l_okcode(4).

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[],
          it_message, it_message[].

  WRITE : mkpf-bldat TO l_bldat,
          mkpf-budat TO l_budat.

  PERFORM dynpro USING : 'X'  'SAPMM07M'        '0400',
                         ' '  'MKPF-BLDAT'      l_bldat,
                         ' '  'MKPF-BUDAT'      l_budat,
                         ' '  'RM07M-MTSNR'     rm07m-mtsnr,
                         ' '  'MKPF-BKTXT'      mkpf-bktxt,
                         ' '  'RM07M-BWARTWA'   rm07m-bwartwa,
                         ' '  'RM07M-SOBKZ'     rm07m-sobkz,
                         ' '  'RM07M-WERKS'     rm07m-werks.

  IF rm07m-grund IS INITIAL.
    PERFORM dynpro USING : ' '  'RM07M-GRUND'     space.
  ELSE.
    PERFORM dynpro USING : ' '  'RM07M-GRUND'     rm07m-grund.
  ENDIF.

  PERFORM dynpro USING : ' '  'RM07M-LGORT'     rm07m-lgort,
                         ' '  'BDC_OKCODE'      '=NPE'.

  LOOP AT it_itab.
    MOVE : '=NPE' TO l_okcode.
    AT LAST.
      MOVE : '=BU'  TO l_okcode.
    ENDAT.
    CLEAR : l_erfmg.
    WRITE : it_itab-erfmg UNIT it_itab-erfme TO l_erfmg.
    PERFORM dynpro USING : 'X'  'SAPMM07M'        '0410',
                           ' '  'MSEG-MATNR'      it_itab-matnr,
                           ' '  'MSEG-ERFMG'      l_erfmg,
                           ' '  'MSEG-ERFME'      it_itab-erfme,
                           ' '  'MSEG-WERKS'      it_itab-werks,
                           ' '  'MSEG-LGORT'      it_itab-lgort,
*    IF w_werks_hide EQ space.
*      PERFORM dynpro USING : ' '  'MSEG-UMWRK'      msegk-umwrk.
*    ENDIF.
*    PERFORM dynpro USING : ' '  'MSEG-UMLGO'      msegk-umlgo,
                           ' '  'DM07M-KONTO'     msegk-konto,
                           ' '  'MSEG-WEMPF'      it_itab-wempf.

*--- reason for movement field selection check

*--- insert by stlim (2004/07/27)
    CLEAR : migo_cust_fields.
    SELECT SINGLE * FROM migo_cust_fields
                   WHERE bwart EQ rm07m-bwartwa
                     AND fieldname EQ c_grund.
    IF sy-subrc EQ 0.
      PERFORM dynpro USING : ' '  'MSEG-GRUND'      it_itab-grund.
    ENDIF.
*--- end of insert

*--- blocked by stlim (2004/07/27)
*    IF t156b-fausw+62(1) EQ '+'.
*      PERFORM dynpro USING : ' '  'MSEG-GRUND'      it_itab-grund.
*    ENDIF.
*--- end of block

    PERFORM dynpro USING : ' '  'BDC_SUBSCR'      'SAPMM07M',
                           ' '  'BDC_SUBSCR'      'SAPLKACB',
*                           ' '  'DKACB-FMORE'     'X',
                           ' '  'BDC_OKCODE'      l_okcode.

*---
    IF w_ccntr_mand NE space OR w_order_mand NE space.
      PERFORM dynpro USING : 'X'  'SAPLKACB'        '0002'.
    ENDIF.

*--- if cost center field mandatory or optional
    IF w_ccntr_mand EQ 'X' OR w_ccntr_mand EQ 'O'.
      PERFORM dynpro USING : ' '  'COBL-KOSTL'      cobl-kostl.
    ENDIF.

*--- if order field mandatory or optional
    IF w_order_mand EQ 'X' OR w_order_mand EQ 'O'.
*    IF w_order_mand NE space.
      PERFORM dynpro USING : ' '  'COBL-AUFNR'      cobl-aufnr.
    ENDIF.

    PERFORM dynpro USING : ' '  'BDC_SUBSCR'      'SAPLKACB',
                           ' '  'BDC_OKCODE'      '=ENTE'.
  ENDLOOP.

  CALL TRANSACTION 'MB1A' USING it_bdc
                          MODE w_mode
                          UPDATE 'S'
                          MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.
ENDFORM.                    " material_document_posting

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
  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], it_itab_copy,
          it_itab_copy[].

*---
  SELECT SINGLE * FROM t320
                 WHERE werks EQ msegk-umwrk
                   AND lgort EQ msegk-umlgo.
  IF sy-subrc EQ 0.
    it_itab_copy[] = it_itab[].
  ELSE.
    LOOP AT it_itab.
      SELECT SINGLE * FROM t320
                     WHERE werks EQ it_itab-werks
                       AND lgort EQ it_itab-lgort.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING it_itab TO it_itab_copy.
        APPEND it_itab_copy.
        CLEAR : it_itab_copy.
      ENDIF.
    ENDLOOP.
  ENDIF.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
                         ' '  'RL02B-MBLNR'     w_mblnr,
                         ' '  'RL02B-MJAHR'     sy-datum(4),
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=MRKA'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=TTYP'.

  LOOP AT it_itab_copy.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0105',
                           ' '  'BDC_OKCODE'      '=TAH2'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0102',
                           ' '  'LTAP-VLTYP'      it_itab_copy-lgtyp,
                           ' '  'LTAP-VLPLA'      it_itab_copy-lgpla,
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
*&      Module  check_matnr_scr9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_matnr_scr9100 INPUT.
**---
  CHECK mseg-matnr NE space.

  CLEAR : mara.
  SELECT SINGLE * FROM mara
                 WHERE matnr EQ mseg-matnr.

  IF sy-subrc NE 0.
    SET CURSOR FIELD 'MSEG-MATNR' LINE sy-stepl.
    MESSAGE e005(mgvnum) WITH mseg-matnr.
  ELSE.
*---
    IF mseg-lgort IS INITIAL.
      SET CURSOR FIELD 'MSEG-LGORT' LINE sy-stepl.
      MESSAGE e018(m7) WITH text-002.
    ENDIF.
*---
    CLEAR : mard.
    SELECT SINGLE * FROM mard
                   WHERE matnr EQ mseg-matnr
                     AND werks EQ mseg-werks
                     AND lgort EQ mseg-lgort.
    IF sy-subrc NE 0.
      SET CURSOR FIELD 'MSEG-MATNR' LINE sy-stepl.
      MESSAGE e006(m7) WITH mseg-matnr mseg-werks mseg-lgort.
    ENDIF.
*---
    IF mseg-erfme EQ space.
      MOVE : mara-meins TO mseg-erfme.
    ELSE.
      MOVE : mseg-erfmg TO w_erfmg.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
           EXPORTING
                input                = w_erfmg
                matnr                = mseg-matnr
                meinh                = mseg-erfme
                meins                = mara-meins
           IMPORTING
                output               = w_erfmg
           EXCEPTIONS
                conversion_not_found = 1
                input_invalid        = 2
                material_not_found   = 3
                meinh_not_found      = 4
                meins_missing        = 5
                no_meinh             = 6
                output_invalid       = 7
                overflow             = 8
                OTHERS               = 9.
*      CLEAR : marm.
*      SELECT SINGLE * FROM marm
*                     WHERE matnr EQ mseg-matnr
*                       AND meinh EQ mseg-erfme.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'MSEG-ERFME' LINE sy-stepl.
        MESSAGE e026(m7) WITH mseg-erfme mara-meins.
      ENDIF.
    ENDIF.
  ENDIF.

*--- set Warehouse Number
  CLEAR : t320.
  SELECT SINGLE lgnum INTO mseg-lgnum
                      FROM t320
                     WHERE werks EQ mseg-werks
                       AND lgort EQ mseg-lgort.

  IF sy-subrc EQ 0 AND mseg-lgtyp NE space.
    CLEAR : t301.
    SELECT SINGLE * FROM t301
                   WHERE lgnum EQ mseg-lgnum
                     AND lgtyp EQ mseg-lgtyp.
    IF sy-subrc EQ 0 AND mseg-lgpla NE space.
      CLEAR : lagp.
      SELECT SINGLE * FROM lagp
                     WHERE lgnum EQ t301-lgnum
                       AND lgtyp EQ t301-lgtyp
                       AND lgpla EQ mseg-lgpla.
      IF sy-subrc NE 0.
        SET CURSOR FIELD 'MSEG-LGTYP' LINE sy-stepl.
        MESSAGE e058(00) WITH lagp-lgnum lagp-lgtyp lagp-lgpla.
      ENDIF.
    ELSE.
      SET CURSOR FIELD 'MSEG-LGPLA' LINE sy-stepl.
      MESSAGE e058(00) WITH t301-lgnum t301-lgtyp.
    ENDIF.
*  ELSE.
*    CLEAR : t320.
*    SELECT SINGLE * FROM t320
*                   WHERE werks EQ msegk-umwrk
*                     AND lgort EQ msegk-umlgo.
*    IF sy-subrc NE 0.
*      CLEAR : mseg-lgnum, mseg-lgtyp, mseg-lgpla.
*    ENDIF.
  ENDIF.

*--- display storage type & storage bin of material master
  IF mseg-lgtyp EQ space OR mseg-lgpla EQ space.
    CLEAR : t320.
    SELECT SINGLE lgnum INTO mseg-lgnum
                        FROM t320
                       WHERE werks EQ mseg-werks
                         AND lgort EQ mseg-lgort.

    CLEAR : mlgt.
    SELECT SINGLE lgtyp
                  lgpla INTO (mseg-lgtyp, mseg-lgpla)
                        FROM mlgt
                       WHERE matnr EQ mseg-matnr
                         AND lgnum EQ mseg-lgnum
                         AND lvorm EQ space.
  ENDIF.
ENDMODULE.                 " check_matnr_scr9100  INPUT

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1085   text
*      -->P_1086   text
*      -->P_1087   text
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
*&      Module  control_screen_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE control_screen_scr9100 OUTPUT.
**---
  LOOP AT SCREEN.
    IF screen-name EQ 'MSEG-LGNUM'.
      screen-invisible = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " control_screen_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  new_items
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_items.
*---
  w_top_line = w_lines + 1.
ENDFORM.                    " new_items

*&---------------------------------------------------------------------*
*&      Form  display_error_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_error_log.
*---
  DATA : l_text(80).

  CLEAR : it_text, it_text[].

  LOOP AT it_message.
    CLEAR : l_text.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = it_message-msgid
              msgnr               = it_message-msgnr
              msgv1               = it_message-msgv1
              msgv2               = it_message-msgv2
              msgv3               = it_message-msgv3
              msgv4               = it_message-msgv4
         IMPORTING
              message_text_output = l_text.
    MOVE : l_text TO it_text.
    APPEND it_text.
    CLEAR : it_text.
  ENDLOOP.

*---
  CALL FUNCTION 'Z_FMM_TEXTEDIT'
    EXPORTING
*         EDIT               =
      title              = 'Error Log'
      line_length        = 120
      start_column       = 10
      start_row          = 3
      end_column         = 130
      end_row            = 10
    TABLES
      texttab            = it_text.
ENDFORM.                    " display_error_log

*&---------------------------------------------------------------------*
*&      Form  display_error_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_error_message.
*---
  DATA : l_text(80).

  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    CLEAR : l_text.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = it_mess-msgid
              msgnr               = it_mess-msgnr
              msgv1               = it_mess-msgv1
              msgv2               = it_mess-msgv2
              msgv3               = it_mess-msgv3
              msgv4               = it_mess-msgv4
         IMPORTING
              message_text_output = l_text.

    MESSAGE e999 WITH l_text.
  ENDIF.

ENDFORM.                    " display_error_message

*&---------------------------------------------------------------------*
*&      Form  exit_comfirm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ANSWER  text
*----------------------------------------------------------------------*
FORM exit_comfirm USING    p_w_answer.
*---
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
*     DEFAULTOPTION        = 'Y'
      textline1            = text-008
      textline2            = text-009
      titel                = text-010
*     START_COLUMN         = 25
*     START_ROW            = 6
*     CANCEL_DISPLAY       = 'X'
    IMPORTING
      answer               = p_w_answer.
ENDFORM.                    " exit_comfirm

*&---------------------------------------------------------------------*
*&      Module  field_selection_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE field_selection_scr9100 OUTPUT.
*---
  CLEAR : w_text_kostl, wa_cobl.

  WRITE : mkpf-budat TO wa_cobl-budat,
          mkpf-bldat TO wa_cobl-bldat,
          c_bukrs    TO wa_cobl-bukrs.
*          cobl-kostl TO wa_cobl-kostl.

*--- get cobl-kostl(cost center) text
  PERFORM conversion_exit_alpha USING cobl-kostl.
  MOVE : cobl-kostl TO wa_cobl-kostl.
  CALL FUNCTION 'COBL_READ_MASTER_DATA_TEXT'
       EXPORTING
            field_name  = 'KOSTL'
            i_cobl      = wa_cobl
       IMPORTING
            description = w_text_kostl.

*--- get cobl-aufnr(order) text
  CLEAR : w_text_aufnr, wa_cobl.
  PERFORM conversion_exit_alpha USING cobl-aufnr.
  MOVE : cobl-aufnr TO wa_cobl-aufnr.
  CALL FUNCTION 'COBL_READ_MASTER_DATA_TEXT'
       EXPORTING
            field_name  = 'AUFNR'
            i_cobl      = wa_cobl
       IMPORTING
            description = w_text_aufnr.

  LOOP AT SCREEN.
    IF screen-name EQ 'COBL-KOSTL'.
      IF w_ccntr_mand EQ 'X'.
        screen-input = 1.
        screen-required = 1.
        screen-invisible = 0.
      ELSEIF w_ccntr_mand EQ 'O'.     " optional
        screen-input = 1.
        screen-required = 0.
        screen-invisible = 0.
      ELSEIF w_ccntr_mand EQ ' '.     " suppress
        screen-input = 0.
        screen-required = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    IF screen-name EQ 'COBL-AUFNR'.
      IF w_order_mand EQ 'X'.
        screen-input = 1.
        screen-required = 1.
        screen-invisible = 0.
      ELSEIF w_order_mand EQ 'O'.     " optional
        screen-input = 1.
        screen-required = 0.
        screen-invisible = 0.
      ELSEIF w_order_mand EQ ' '.     " suppress
        screen-input = 0.
        screen-required = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

**---
*  loop at screen.
*    if screen-group1 gt 0.
*      clear : w_refer, w_string.
*      w_refer = screen-group1 - 1.
*      move : t156b-fausw to w_string.
*      shift w_string by w_refer places circular.
*      case w_string(1).
*        when c_plus.     " required
*          screen-required = c_mode_on.
*        when c_punc.     " optional
*          screen-required = c_mode_off.
*        when others.     " supress
*          screen-input = c_mode_off.
*          screen-invisible = c_mode_on.
*      endcase.
*      shift w_string by w_refer places circular right.
*    endif.
*    modify screen.
*  endloop.
ENDMODULE.                 " field_selection_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  check_mvt_type_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_mvt_type_scr9000 INPUT.
*---
  CLEAR : t158b.

  SELECT SINGLE * FROM t158b
                 WHERE tcode EQ c_mb1a
                   AND bwart EQ rm07m-bwartwa.

  IF sy-subrc NE 0.
    SET CURSOR FIELD 'RM07M-BWARTWA'.
    MESSAGE e096(m7) WITH rm07m-bwartwa.
  ENDIF.
ENDMODULE.                 " check_mvt_type_scr9000  INPUT

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
  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], it_itab_copy,
          it_itab_copy[].

*---
  SELECT SINGLE * FROM t320
                 WHERE werks EQ msegk-umwrk
                   AND lgort EQ msegk-umlgo.
  IF sy-subrc EQ 0.
    it_itab_copy[] = it_itab[].
  ELSE.
    LOOP AT it_itab.
      SELECT SINGLE * FROM t320
                     WHERE werks EQ it_itab-werks
                       AND lgort EQ it_itab-lgort.
      IF sy-subrc EQ 0.
        MOVE-CORRESPONDING it_itab TO it_itab_copy.
        APPEND it_itab_copy.
        CLEAR : it_itab_copy.
      ENDIF.
    ENDLOOP.
  ENDIF.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
                         ' '  'RL02B-MBLNR'     w_mblnr,
                         ' '  'RL02B-MJAHR'     sy-datum(4),
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=MRKA'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=TPAL'.

  LOOP AT it_itab_copy.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0104',
                           ' '  'RL03T-LETY2'     'BB',
                           ' '  'RL03T-LGTY2'     it_itab_copy-lgtyp,
                           ' '  'BDC_OKCODE'      '/00'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0104',
                           ' '  'BDC_OKCODE'      '=TAH1'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0102',
                           ' '  'LTAP-NLTYP'      it_itab_copy-lgtyp,
                           ' '  'LTAP-NLPLA'      it_itab_copy-lgpla,
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
*&      Form  check_transfer_order_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_transfer_order_condition USING p_subrc.
*---
  CLEAR : ltbk.

  SELECT SINGLE * FROM ltbk
                 WHERE mblnr EQ w_mblnr
                   AND mjahr EQ sy-datum(4).

  IF sy-subrc EQ 0.
    CLEAR : t333.

    SELECT SINGLE * FROM t333
                   WHERE lgnum EQ ltbk-lgnum
                     AND bwlvs EQ ltbk-bwlvs.

    MOVE : sy-subrc TO p_subrc.
  ELSE.
    MOVE : sy-subrc TO p_subrc.
  ENDIF.
ENDFORM.                    " check_transfer_order_condition

*&---------------------------------------------------------------------*
*&      Module  check_gl_account  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_gl_account INPUT.
*---
  CHECK NOT msegk-konto IS INITIAL.

  CLEAR : t001, ska1.

  SELECT SINGLE ktopl INTO t001-ktopl
                      FROM t001
                     WHERE bukrs EQ c_bukrs.

  PERFORM conversion_exit_alpha USING msegk-konto.

  SELECT SINGLE * FROM ska1
                 WHERE ktopl EQ t001-ktopl
                   AND saknr EQ msegk-konto.

  IF sy-subrc NE 0.
    SET CURSOR FIELD 'MSEGK-KONTO'.
    MESSAGE e054(m7) WITH msegk-konto t001-ktopl.
  ENDIF.

*--- check SETID
  CHECK ( msegk-bwart EQ '201' OR msegk-bwart EQ '202' ).

  DATA : it_node_tab LIKE grpobjects OCCURS 0 WITH HEADER LINE,
         it_valu_tab LIKE grpvalues OCCURS 0 WITH HEADER LINE.

  DATA : l_cinfo LIKE grphinfo.

  RANGES : r_konto FOR msegk-konto.

  CLEAR : it_node_tab, it_node_tab[], it_valu_tab, it_valu_tab[],
          r_konto, r_konto[].

  CALL FUNCTION 'K_HIERARCHY_TABLES_READ'
       EXPORTING
            e_class                     = '0000'
            e_setid                     = '0000H201_MM_GI_OTHER'
            e_kokrs                     = 'H201'
            e_mandt                     = sy-mandt
       TABLES
            t_nodes                     = it_node_tab
            t_values                    = it_valu_tab
       CHANGING
            c_info                      = l_cinfo
            c_overwrite                 = sy-datar
       EXCEPTIONS
            no_controlling_area         = 1
            no_chart_of_account         = 2
            different_controlling_areas = 3
            different_chart_of_accounts = 4
            set_not_found               = 5
            illegal_field_replacement   = 6
            illegal_table_replacement   = 7
            fm_raise                    = 8
            convert_error               = 9
            no_overwrite_standard_hier  = 10
            no_bukrs_for_kokrs          = 11
            OTHERS                      = 12.

  LOOP AT it_valu_tab.
    MOVE : 'I'               TO r_konto-sign,
           'BT'              TO r_konto-option,
           it_valu_tab-vfrom TO r_konto-low,
           it_valu_tab-vto   TO r_konto-high.
    APPEND r_konto.
    CLEAR : r_konto.
  ENDLOOP.

  IF NOT msegk-konto IN r_konto.
    MESSAGE e999 WITH text-m01.
  ENDIF.
ENDMODULE.                 " check_gl_account  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_cost_center  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_cost_center INPUT.
*---
  CLEAR : csks, tka02.

  SELECT SINGLE kokrs INTO tka02-kokrs
                      FROM tka02
                     WHERE bukrs EQ c_bukrs.

  PERFORM conversion_exit_alpha USING cobl-kostl.

  SELECT SINGLE * FROM csks
                 WHERE kokrs EQ tka02-kokrs
                   AND kostl EQ cobl-kostl
                   AND datbi GE mkpf-budat
                   AND datab LE mkpf-budat.

  IF sy-subrc NE 0.
    SET CURSOR FIELD 'COBL-KOSTL'.
    MESSAGE e222(ki) WITH tka02-kokrs cobl-kostl mkpf-budat.
  ENDIF.
ENDMODULE.                 " check_cost_center  INPUT

*&---------------------------------------------------------------------*
*&      Form  get_field_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_field_selection.
*---
  CLEAR : t156b.

  SELECT SINGLE * FROM t156b
                 WHERE bwart EQ rm07m-bwartwa
                   AND sobkz EQ rm07m-sobkz.
ENDFORM.                    " get_field_selection

*&---------------------------------------------------------------------*
*&      Module  check_order  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_order INPUT.
*---
  CLEAR : aufk.

  PERFORM conversion_exit_alpha USING cobl-aufnr.

  SELECT SINGLE * FROM aufk
                 WHERE aufnr EQ cobl-aufnr.

  IF sy-subrc NE 0.
    SET CURSOR FIELD 'COBL-AUFNR'.
    MESSAGE e104(ko) WITH cobl-aufnr.
  ENDIF.
ENDMODULE.                 " check_order  INPUT
