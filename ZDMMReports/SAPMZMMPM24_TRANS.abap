************************************************************************
* Program Name      : SAPMZMMPM24_TRANS
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.09.30.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : MM Stock Transfer with WM
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.09.30.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT  sapmzmmpm24_trans MESSAGE-ID zmmm.

**---
INCLUDE : zrmmpmxxr_incl.

TYPE-POOLS : cxtab.

**---
DATA : BEGIN OF it_itab OCCURS 0,
         matnr LIKE mseg-matnr,
         erfmg LIKE mseg-erfmg,
         erfme LIKE mseg-erfme,
         lgtyp LIKE mseg-lgtyp,
         lgpla LIKE mseg-lgpla,
         lgort LIKE mseg-lgort,
         werks LIKE mseg-werks,
         tanum LIKE ltak-tanum,     " T/O Number
         lgtyp_344 LIKE mseg-lgtyp,
         lgpla_344 LIKE mseg-lgpla,
         rsnum LIKE resb-rsnum,
         rspos LIKE resb-rspos,
       END OF it_itab.

DATA : BEGIN OF it_itab_copy OCCURS 0.
        INCLUDE STRUCTURE it_itab.
DATA : END OF it_itab_copy.

DATA : BEGIN OF it_lqua OCCURS 0,
         lgtyp LIKE lqua-lgtyp,
         lgpla LIKE lqua-lgpla,
         gesme LIKE lqua-gesme,
       END OF it_lqua.

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

DATA : w_lqua_lines TYPE i.

DATA : w_loop_check(1).

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
            c_mb1b LIKE sy-tcode VALUE 'MB1B',
            c_lgtyp_dnr LIKE lqua-lgtyp VALUE 'DNR',
            c_lgpla_cc  LIKE lqua-lgpla VALUE 'CC-DNR',
            c_lgpla_pl  LIKE lqua-lgpla VALUE 'PLANT-DNR',
            c_bwlvs_977 LIKE ltak-bwlvs VALUE '977'.

*---
DATA: BEGIN OF seltab1 OCCURS 10,
         rsnum LIKE rkpf-rsnum,
         rspos LIKE resb-rspos,
         rsart LIKE resb-rsart,
         xexpl,
      END OF seltab1.

DATA : xinit      TYPE c.               " Hilfsfeld Res.Suche

RANGES rwerks FOR rm07m-werks.
RANGES rmatnr FOR rm07m-matnr.
RANGES rbdter FOR rm07m-bdter.
RANGES reindt FOR rm07m-eindt.
RANGES rlifnr FOR am07m-lifnr.
RANGES rreswk FOR am07m-reswk.
RANGES rean11 FOR rm07m-ean11.
RANGES ridnlf FOR rm07m-idnlf.
RANGES rmatnv FOR am07m-matnv.
RANGES rpstyp FOR vm07m-pstyp.
RANGES: rtknum FOR rm07m-tknum,        "WE zum Transport
        rvlief FOR rm07m-vlief.

CONSTANTS : x TYPE c VALUE 'X'.

DATA : index_z    LIKE sy-index.

DATA : w_copy_resb(1).





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
    WHEN '9200'.
      CASE sy-ucomm.
        WHEN 'BACK' OR 'EXIT' OR 'CANC'.
          LEAVE TO SCREEN 0.
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
    WHEN '9200'.
      SET PF-STATUS '9200'.
      SET TITLEBAR  '9200'.
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
    SELECT SINGLE * FROM t158 WHERE tcode EQ c_mb1b.
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
*          PERFORM check_input_value.
          PERFORM get_field_selection.
          CLEAR : w_werks_hide.
          IF t156b-fausw+56(1) EQ '-'.     " if plant field is supress
            MOVE : rm07m-werks TO msegk-umwrk.
            MOVE : 'X' TO w_werks_hide.
          ENDIF.
          CALL SCREEN 9100.
        WHEN 'RESB'.
          CLEAR : w_save_okcode.
          PERFORM copy_reservation.
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
    WHEN '9200'.
      CASE w_save_okcode.
        WHEN 'ENTER'.
          CLEAR : w_save_okcode.
        WHEN 'COPY'.
          CLEAR : w_save_okcode.
      ENDCASE.
  ENDCASE.

*---
  IF sy-dynnr EQ '9000' AND w_save_okcode EQ 'ENTER'
                        AND w_copy_resb NE space.
    CLEAR : w_save_okcode.
    PERFORM get_field_selection.
    CLEAR : w_werks_hide.
    IF t156b-fausw+56(1) EQ '-'.     " if plant field is supress
*      MOVE : rm07m-werks TO msegk-umwrk.
      MOVE : 'X' TO w_werks_hide.
    ENDIF.
    CALL SCREEN 9100.
    CLEAR : w_copy_resb.
  ENDIF.
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
  ENDIF.

  MOVE w_index               TO mseg-zeile.

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
*&      Module  check_receiv_sloc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_receiv_sloc INPUT.
***---
*  IF NOT msegk-umlgo IS INITIAL.
*    CLEAR : t001l.
*    SELECT SINGLE * FROM t001l
*                   WHERE werks EQ rm07m-werks
*                     AND lgort EQ msegk-umlgo.
*    IF sy-subrc NE 0.
*      SET CURSOR FIELD 'MSEGK-UMLGO'.
*      MESSAGE e146(m7) WITH text-002 msegk-umlgo.
*    ENDIF.
*  ENDIF.
ENDMODULE.                 " check_receiv_sloc  INPUT

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
  IF rm07m-bwartwa EQ '344' OR rm07m-bwartwa EQ '343'.
    PERFORM create_transfer_order.
*  ELSEIF rm07m-bwartwa EQ '343'.
*    PERFORM modify_itab.
  ENDIF.

*---
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
          WHEN '3'.     " Single Item
            PERFORM transfer_order_posting_single.
        ENDCASE.
        IF sy-subrc EQ 0 AND sy-msgty EQ 'S'.
          READ TABLE it_mess WITH KEY msgid = 'L3'
                                      msgnr = '016'.
          IF sy-subrc EQ 0.
            CLEAR : w_tanum.
            MOVE : it_mess-msgv1 TO w_tanum.
            MESSAGE s021 WITH w_mblnr w_tanum.
**--- create Transfer Order
*            IF rm07m-bwartwa EQ '343'.
*              PERFORM create_transfer_order.
*            ENDIF.
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

  DATA : goodsmvt_header  LIKE bapi2017_gm_head_01,
         goodsmvt_code    LIKE bapi2017_gm_code,
         goodsmvt_headret LIKE bapi2017_gm_head_ret,
         materialdocument LIKE bapi2017_gm_head_ret-mat_doc,
         matdocumentyear  LIKE bapi2017_gm_head_ret-doc_year,
         goodsmvt_item    LIKE bapi2017_gm_item_create OCCURS 0
                                                       WITH HEADER LINE,
         return           LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[],
          it_message, it_message[].

*---
  IF w_copy_resb NE space.

    MOVE : mkpf-bldat TO goodsmvt_header-doc_date,
           mkpf-budat TO goodsmvt_header-pstng_date.

    MOVE : '04' TO goodsmvt_code-gm_code.

    LOOP AT it_itab.
      MOVE : it_itab-erfmg TO goodsmvt_item-entry_qnt,
             it_itab-rsnum TO goodsmvt_item-reserv_no,
             it_itab-rspos TO goodsmvt_item-res_item,
             it_itab-werks TO goodsmvt_item-plant,
             it_itab-lgort TO goodsmvt_item-stge_loc.
      APPEND goodsmvt_item.
    ENDLOOP.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
         EXPORTING
              goodsmvt_header  = goodsmvt_header
              goodsmvt_code    = goodsmvt_code
         IMPORTING
              goodsmvt_headret = goodsmvt_headret
              materialdocument = materialdocument
              matdocumentyear  = matdocumentyear
         TABLES
              goodsmvt_item    = goodsmvt_item
              return           = return.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.

    IF materialdocument NE space.
      MOVE : 0                TO sy-subrc,
             'S'              TO sy-msgty,
             'M7'             TO it_mess-msgid,
             '060'            TO it_mess-msgnr,
             materialdocument TO it_mess-msgv1,
             matdocumentyear  TO it_mess-msgv2.
      APPEND it_mess.
    ELSE.
      READ TABLE return INDEX 1.
      MOVE : 4                 TO sy-subrc,
             'E'               TO sy-msgty,
             return-type       TO it_mess-msgtyp,
             sy-langu          TO it_mess-msgspra,
             return-id         TO it_mess-msgid,
             return-number     TO it_mess-msgnr,
             return-message_v1 TO it_mess-msgv1,
             return-message_v2 TO it_mess-msgv2,
             return-message_v3 TO it_mess-msgv3,
             return-message_v4 TO it_mess-msgv4.
      APPEND it_mess.
    ENDIF.

  ELSE.

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
                             ' '  'MSEG-SGTXT'      it_itab-tanum. "T/O
      IF w_werks_hide EQ space.
        PERFORM dynpro USING : ' '  'MSEG-UMWRK'      msegk-umwrk.
      ENDIF.
      PERFORM dynpro USING : ' '  'MSEG-UMLGO'      msegk-umlgo,
*                           ' '  'MSEG-WEMPF'      msegk-wempf,
                             ' '  'BDC_SUBSCR'      'SAPMM07M',
                             ' '  'BDC_SUBSCR'      'SAPLKACB',
                             ' '  'DKACB-FMORE'     'X',
                             ' '  'BDC_OKCODE'      l_okcode.
      PERFORM dynpro USING : 'X'  'SAPLKACB'        '0002',
                             ' '  'BDC_SUBSCR'      'SAPLKACB',
                             ' '  'BDC_OKCODE'      '=ENTE'.
    ENDLOOP.

    CALL TRANSACTION 'MB1B' USING it_bdc
                            MODE w_mode
                            UPDATE 'S'
                            MESSAGES INTO it_mess.

    APPEND LINES OF it_mess TO it_message.

  ENDIF.
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
  ELSE.
    CLEAR : t320.
    SELECT SINGLE * FROM t320
                   WHERE werks EQ msegk-umwrk
                     AND lgort EQ msegk-umlgo.
    IF sy-subrc NE 0.
      CLEAR : mseg-lgnum, mseg-lgtyp, mseg-lgpla.
    ENDIF.
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

**---
*  IF w_copy_resb NE space.
*    w_index = w_top_line + sy-stepl - 1.
*    READ TABLE it_itab INDEX w_index.
*    IF sy-subrc NE 0.
*      LOOP AT SCREEN.
*        screen-input = 0.
*        MODIFY SCREEN.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
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
  ENDIF.

  MESSAGE e999 WITH l_text.
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
  LOOP AT SCREEN.
    IF screen-group1 GT 0.
      CLEAR : w_refer, w_string.
      w_refer = screen-group1 - 1.
      MOVE : t156b-fausw TO w_string.
      SHIFT w_string BY w_refer PLACES CIRCULAR.
      CASE w_string(1).
        WHEN c_plus.     " required
          screen-required = c_mode_on.
        WHEN c_punc.     " optional
          screen-required = c_mode_off.
        WHEN OTHERS.     " supress
          screen-input = c_mode_off.
          screen-invisible = c_mode_on.
      ENDCASE.
      SHIFT w_string BY w_refer PLACES CIRCULAR RIGHT.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " field_selection_scr9100  OUTPUT

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
*&      Module  check_mvt_type_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_mvt_type_scr9000 INPUT.
*---
  IF w_okcode EQ 'ENTER'.
    PERFORM check_input_value.
  ENDIF.

*---
  CHECK rm07m-bwartwa NE space.

  CLEAR : t158b.

  SELECT SINGLE * FROM t158b
                 WHERE tcode EQ c_mb1b
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
    CLEAR : lubu.

    SELECT SINGLE * FROM lubu
                   WHERE mblnr EQ w_mblnr
                     AND mjahr EQ sy-datum(4).

    IF sy-subrc EQ 0.
      CLEAR : t333.

      SELECT SINGLE * FROM t333
                     WHERE lgnum EQ lubu-lgnum
                       AND bwlvs EQ lubu-bwlvs.
      MOVE : sy-subrc TO p_subrc.
    ELSE.
      MOVE : sy-subrc TO p_subrc.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_transfer_order_condition

*&---------------------------------------------------------------------*
*&      Form  TRANSFER_ORDER_POSTING_SINGLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_order_posting_single.
*---
  DATA : l_lines TYPE i,
         l_erfmg(15),
         l_field(15),
         l_tabix(2) TYPE n.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], it_itab_copy,
          it_itab_copy[], l_lines.

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
  DESCRIBE TABLE it_itab_copy LINES l_lines.

*--- initial screen
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
                         ' '  'RL02B-MBLNR'     w_mblnr,
                         ' '  'RL02B-MJAHR'     sy-datum(4),
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

  CASE l_lines.
    WHEN 1.
*--- read LUBU / LQUA
      READ TABLE it_itab_copy INDEX 1.
      PERFORM read_lubu_lqua.
      CASE w_lqua_lines.
        WHEN 1.
        WHEN OTHERS.
          IF rm07m-bwartwa EQ '344'.     " OR rm07m-bwart EQ '343'.
            READ TABLE it_lqua WITH KEY lgtyp = it_itab_copy-lgtyp_344
                                        lgpla = it_itab_copy-lgpla_344.
          ELSE.
            READ TABLE it_lqua WITH KEY lgtyp = it_itab_copy-lgtyp
                                        lgpla = it_itab_copy-lgpla.
          ENDIF.
          MOVE : sy-tabix TO l_tabix.
*--- check quantity
          PERFORM check_quants_quantity CHANGING l_tabix.
*---
          CONCATENATE 'RL03T-SELMG(' l_tabix ')' INTO l_field.
*--- quants selection screen
          PERFORM quants_selection_screen.
*--- quantity input screen
          CLEAR : l_erfmg.
          WRITE : it_itab_copy-erfmg TO l_erfmg UNIT it_itab_copy-erfme.
          PERFORM dynpro USING : 'X'  'SAPML03T'         '0142',
                                 ' '  'BDC_SUBSCR'       'SAPML03T',
                                 ' '  l_field            l_erfmg,
                                 ' '  'BDC_OKCODE'       '/00'.
          PERFORM dynpro USING : 'X'  'SAPML03T'         '0142',
                                 ' '  'BDC_SUBSCR'       'SAPML03T',
*                                 ' '  'BDC_OKCODE'       '=TAUH'.
*          PERFORM dynpro USING : 'X'  'SAPML03T'         '0102',
*                                 ' '  'LTAP-LETYP'       'BB'.
*          IF rm07m-bwartwa EQ '344'.     " OR rm07m-bwartwa EQ '343'.
*            PERFORM dynpro USING: ' '  'LTAP-NLTYP'
*                                               it_itab_copy-lgtyp_344,
*                                  ' '  'LTAP-NLPLA'
*                                               it_itab_copy-lgpla_344.
*          ELSE.
*           PERFORM dynpro USING: ' '  'LTAP-NLTYP'  it_itab_copy-lgtyp,
*                                 ' '  'LTAP-NLPLA'  it_itab_copy-lgpla.
*          ENDIF.
*          PERFORM dynpro USING : ' '  'BDC_OKCODE'       '/00'.
*          PERFORM dynpro USING : 'X'  'SAPML03T'         '0142',
*                                 ' '  'BDC_SUBSCR'       'SAPML03T',
                                 ' '  'BDC_OKCODE'       '=BU'.
      ENDCASE.
    WHEN OTHERS.
*--- item selection screen
      PERFORM dynpro USING : 'X'  'SAPMSSY0'         '0120',
                             ' '  'BDC_OKCODE'       '=TAH'.
      LOOP AT it_itab_copy.
*--- read LUBU / LQUA
        PERFORM read_lubu_lqua.
        IF rm07m-bwartwa EQ '344'.     " OR rm07m-bwartwa EQ '343'.
          READ TABLE it_lqua WITH KEY lgtyp = it_itab_copy-lgtyp_344
                                      lgpla = it_itab_copy-lgpla_344.
        ELSE.
          READ TABLE it_lqua WITH KEY lgtyp = it_itab_copy-lgtyp
                                      lgpla = it_itab_copy-lgpla.
        ENDIF.
        MOVE : sy-tabix TO l_tabix.
*--- check quantity
        PERFORM check_quants_quantity CHANGING l_tabix.
*---
        CONCATENATE 'RL03T-SELMG(' l_tabix ')' INTO l_field.
*--- check how many quants exist, if there are more than 2,
*--- follow logic is executed, if not, skip.
        CHECK w_lqua_lines GE 2.
*--- quants selection screen
        PERFORM quants_selection_screen.
*--- quantity input screen
        CLEAR : l_erfmg.
        WRITE : it_itab_copy-erfmg TO l_erfmg UNIT it_itab_copy-erfme.
        PERFORM dynpro USING : 'X'  'SAPML03T'         '0142',
                               ' '  'BDC_SUBSCR'       'SAPML03T',
                               ' '  l_field            l_erfmg,
                               ' '  'BDC_OKCODE'       '/00'.
        PERFORM dynpro USING : 'X'  'SAPML03T'         '0142',
                               ' '  'BDC_SUBSCR'       'SAPML03T',
*                               ' '  'BDC_OKCODE'       '=TAUH'.
*        PERFORM dynpro USING : 'X'  'SAPML03T'         '0102',
*                               ' '  'LTAP-LETYP'       'BB'.
*        IF rm07m-bwartwa EQ '344'.     " OR rm07m-bwart EQ '343'.
*          PERFORM dynpro USING : ' '  'LTAP-NLTYP'
*                                              it_itab_copy-lgtyp_344,
*                                 ' '  'LTAP-NLPLA'
*                                              it_itab_copy-lgpla_344.
*        ELSE.
*          PERFORM dynpro USING: ' '  'LTAP-NLTYP'  it_itab_copy-lgtyp,
*                                ' '  'LTAP-NLPLA'  it_itab_copy-lgpla.
*        ENDIF.
*        PERFORM dynpro USING : ' '  'BDC_OKCODE'       '/00'.
*        PERFORM dynpro USING : 'X'  'SAPML03T'         '0142',
*                               ' '  'BDC_SUBSCR'       'SAPML03T',
                               ' '  'BDC_OKCODE'       '=BU'.
      ENDLOOP.
*--- report back screen
      PERFORM dynpro USING : 'X'  'SAPMSSY0'       '0120',
                             ' '  'BDC_OKCODE'     '=BAKK'.
  ENDCASE.

*---
  CALL TRANSACTION 'LT06' USING it_bdc
                          MODE w_mode
                          UPDATE 'S'
                          MESSAGES INTO it_mess.

  APPEND LINES OF it_mess TO it_message.
ENDFORM.                    " TRANSFER_ORDER_POSTING_SINGLE

*&---------------------------------------------------------------------*
*&      Form  read_lubu_lqua
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_lubu_lqua.
*---
  CLEAR : lubu, lqua.

  SELECT SINGLE * FROM lubu
                 WHERE mjahr EQ sy-datum(4)
                   AND mblnr EQ w_mblnr.

*---
  CLEAR : it_lqua, it_lqua[], w_lqua_lines.

  SELECT lgtyp
         lgpla
         SUM( gesme ) INTO CORRESPONDING FIELDS OF TABLE it_lqua
                      FROM lqua
                     WHERE matnr EQ it_itab_copy-matnr
                       AND werks EQ it_itab_copy-werks
                       AND lgort EQ it_itab_copy-lgort
                       AND bestq EQ lubu-bstq1
                       AND gesme GT 0
                  GROUP by lgtyp lgpla
                  ORDER BY lgtyp lgpla ASCENDING.

*---
  DESCRIBE TABLE it_lqua LINES w_lqua_lines.
ENDFORM.                    " read_lubu_lqua

*&---------------------------------------------------------------------*
*&      Form  quants_selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM quants_selection_screen.
*---
  PERFORM dynpro USING : 'X'  'SAPML03T'         '0143',
                         ' '  'BDC_OKCODE'       '=MRKA'.
  PERFORM dynpro USING : 'X'  'SAPML03T'         '0143',
                         ' '  'BDC_OKCODE'       '=QULI'.
ENDFORM.                    " quants_selection_screen

*&---------------------------------------------------------------------*
*&      Form  check_quants_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_quants_quantity CHANGING p_l_tabix.
*---
  DATA : l_gesme LIKE it_lqua-gesme.

  CLEAR : l_gesme, w_loop_check.

  IF it_lqua-gesme GE it_itab_copy-erfmg.
*    MOVE : p_l_tabix TO p_l_tabix.
  ELSE.
    LOOP AT it_lqua.
      IF it_lqua-gesme GE it_itab_copy-erfmg.
        MOVE : sy-tabix TO p_l_tabix.
        EXIT.
      ELSE.
        l_gesme = l_gesme + it_lqua-gesme.
        IF l_gesme GE it_itab_copy-erfmg.
          MOVE : sy-tabix TO p_l_tabix,
                 'X'      TO w_loop_check.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " check_quants_quantity

*&---------------------------------------------------------------------*
*&      Form  create_transfer_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_transfer_order.
*---
  DATA : l_subrc LIKE sy-subrc,
         l_mess LIKE it_mess,
         l_tabix LIKE sy-tabix,
         l_lgpla LIKE lqua-lgpla.

  LOOP AT it_itab.
    MOVE : sy-tabix TO l_tabix.
*--- WM relevent check
    PERFORM read_t320 USING msegk-umwrk msegk-umlgo.
    IF sy-subrc NE 0.
      PERFORM read_t320 USING it_itab-werks it_itab-lgort.
      CHECK sy-subrc EQ 0.
    ENDIF.
*---
    PERFORM call_function TABLES it_mess
                          USING  l_subrc l_lgpla.
    IF l_subrc EQ 0.
      CLEAR : l_mess.
      READ TABLE it_mess INTO l_mess WITH KEY msgtyp = 'S'.
      MOVE : l_mess-msgv1 TO it_itab-tanum.
    ELSE.
      PERFORM display_error_message.
    ENDIF.
    MOVE : c_lgtyp_dnr TO it_itab-lgtyp_344,
           l_lgpla     TO it_itab-lgpla_344.
    MODIFY it_itab INDEX l_tabix.
  ENDLOOP.
ENDFORM.                    " create_transfer_order

*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_function TABLES   ext_bdcmsgcoll STRUCTURE bdcmsgcoll
                   CHANGING p_subrc p_lgpla.
*---
  DATA : l_lgpla LIKE lqua-lgpla.

  CLEAR : p_lgpla.

  IF it_itab-lgtyp EQ '421' OR it_itab-lgtyp EQ '422' OR
     it_itab-lgtyp EQ '411'.
    MOVE : c_lgpla_cc TO p_lgpla.
  ELSE.
    MOVE : c_lgpla_pl TO p_lgpla.
  ENDIF.

*---
  DATA : l_lgnum_001     TYPE bdcdata-fval,
         l_bwlvs_002     TYPE bdcdata-fval,
         l_matnr_003     TYPE bdcdata-fval,
         l_anfme_004     TYPE bdcdata-fval,
         l_werks_005     TYPE bdcdata-fval,
         l_lgort_006     TYPE bdcdata-fval,
         l_anfme_007     TYPE bdcdata-fval,
         l_altme_008     TYPE bdcdata-fval,
         l_vltyp_009     TYPE bdcdata-fval,
         l_vlpla_010     TYPE bdcdata-fval,
         l_nltyp_011     TYPE bdcdata-fval,
         l_nlpla_012     TYPE bdcdata-fval,
         l_refnr_013     TYPE bdcdata-fval,
         l_bestq_014     TYPE bdcdata-fval.

  CLEAR : l_lgnum_001, l_bwlvs_002, l_matnr_003, l_anfme_004,
          l_werks_005, l_lgort_006, l_anfme_007, l_altme_008,
          l_vltyp_009, l_vlpla_010, l_nltyp_011, l_nlpla_012,
          l_refnr_013, l_bestq_014.

  MOVE : mseg-lgnum    TO l_lgnum_001,
         c_bwlvs_977   TO l_bwlvs_002,
         it_itab-matnr TO l_matnr_003,
         it_itab-erfmg TO l_anfme_004,
         it_itab-werks TO l_werks_005,
         it_itab-lgort TO l_lgort_006,
         it_itab-erfmg TO l_anfme_007,
         it_itab-erfme TO l_altme_008.

  IF rm07m-bwartwa EQ '344'.
    MOVE : it_itab-lgtyp TO l_vltyp_009,     " source type
           it_itab-lgpla TO l_vlpla_010,     " source bin
           c_lgtyp_dnr   TO l_nltyp_011,     " destination type (DNR)
           p_lgpla       TO l_nlpla_012.     " destination bin
  ELSEIF rm07m-bwartwa EQ '343'.
    MOVE : c_lgtyp_dnr   TO l_vltyp_009,     " source type (DNR)
           p_lgpla       TO l_vlpla_010,     " source bin
           it_itab-lgtyp TO l_nltyp_011,     " destination type
           it_itab-lgpla TO l_nlpla_012.     " destination bin
    MOVE : 'S'           TO l_bestq_014.     " special stock
*  ELSE.
*    MOVE : it_itab-lgtyp_344 TO l_vltyp_009,
*           it_itab-lgpla_344 TO l_vlpla_010,
*           it_itab-lgtyp     TO l_nltyp_011,
*           it_itab-lgpla     TO l_nlpla_012.
  ENDIF.

  MOVE : space         TO l_refnr_013.

  CONDENSE : l_lgnum_001, l_bwlvs_002, l_matnr_003, l_anfme_004,
             l_werks_005, l_lgort_006, l_anfme_007, l_altme_008,
             l_vltyp_009, l_vlpla_010, l_nltyp_011, l_nlpla_012,
             l_refnr_013, l_bestq_014.

  CALL FUNCTION 'Z_FMM_6012_01'
       EXPORTING
            mode      = 'N'
            lgnum_001 = l_lgnum_001
            bwlvs_002 = l_bwlvs_002
            matnr_003 = l_matnr_003
            anfme_004 = l_anfme_004
            werks_005 = l_werks_005
            lgort_006 = l_lgort_006
            anfme_007 = l_anfme_007
            altme_008 = l_altme_008
            vltyp_009 = l_vltyp_009
            vlpla_010 = l_vlpla_010
            nltyp_011 = l_nltyp_011
            nlpla_012 = l_nlpla_012
            refnr_013 = l_refnr_013
            bestq_014 = l_bestq_014
       IMPORTING
            subrc     = p_subrc
       TABLES
            messtab   = ext_bdcmsgcoll.

  COMMIT WORK.
ENDFORM.                    " call_function

*&---------------------------------------------------------------------*
*&      Form  read_t320
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MSEGK_UMWRK  text
*      -->P_MSEGK_UMLGO  text
*----------------------------------------------------------------------*
FORM read_t320 USING    p_werks
                        p_lgort.
*---
  CLEAR : t320.
  SELECT SINGLE * FROM t320
                 WHERE werks EQ p_werks
                   AND lgort EQ p_lgort.
ENDFORM.                                                    " read_t320

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
  DATA : l_tabix LIKE sy-tabix.

  LOOP AT it_itab.
    MOVE : sy-tabix TO l_tabix.
*--- WM relevent check
    PERFORM read_t320 USING msegk-umwrk msegk-umlgo.
    IF sy-subrc NE 0.
      PERFORM read_t320 USING it_itab-werks it_itab-lgort.
      CHECK sy-subrc EQ 0.
    ENDIF.
    IF it_itab-lgtyp EQ '421' OR it_itab-lgtyp EQ '422' OR
       it_itab-lgtyp EQ '411'.
      MOVE : c_lgtyp_dnr TO it_itab-lgtyp_344,
             c_lgpla_cc  TO it_itab-lgpla_344.
    ELSE.
      MOVE : c_lgtyp_dnr TO it_itab-lgtyp_344,
             c_lgpla_pl  TO it_itab-lgpla_344.
    ENDIF.
    MODIFY it_itab INDEX l_tabix.
  ENDLOOP.
ENDFORM.                    " modify_itab

*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_value.
*--- check movement type
  IF rm07m-bwartwa EQ space.
    SET CURSOR FIELD 'RM07M-BWARTWA'.
    MESSAGE e018(m7) WITH text-m01.
  ENDIF.

*--- check plant
  IF rm07m-werks EQ space.
    SET CURSOR FIELD 'RM07M-WERKS'.
    MESSAGE e018(m7) WITH text-m02.
  ENDIF.
ENDFORM.                    " check_input_value

*&---------------------------------------------------------------------*
*&      Form  copy_reservation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_reservation.
*---
  CALL SCREEN 9200 STARTING AT 17   1
                   ENDING   AT 66  19.

  IF NOT seltab1[] IS INITIAL.
    MOVE : 'ENTER' TO w_save_okcode,
           'X'     TO w_copy_resb.
  ENDIF.
ENDFORM.                    " copy_reservation

*&---------------------------------------------------------------------*
*&      Module  eingabe_konr  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE eingabe_konr INPUT.
*--- this logic copied from standard program (SAPMM07M)
  CLEAR seltab1.
  REFRESH seltab1.
  IF NOT rm07m-rsnum IS INITIAL.
    MOVE-CORRESPONDING rm07m TO seltab1.
    IF NOT seltab1-rspos IS INITIAL.
      seltab1-xexpl = x.
    ENDIF.
    APPEND seltab1.
  ENDIF.

*  CHECK rm07m-rsnum IS INITIAL.
  IF rm07m-rsnum IS INITIAL.
    EXPORT seltab1 TO MEMORY ID 'SELTAB1'.
    CLEAR xinit.
    REFRESH rwerks.
    IF rm07m-matnr IS INITIAL
    AND ( ( NOT rm07m-kostl IS INITIAL OR NOT rm07m-aufnr IS INITIAL OR
            NOT rm07m-projn IS INITIAL OR NOT rm07m-nplnr IS INITIAL OR
            NOT rm07m-anln1 IS INITIAL OR NOT rm07m-umwrk IS INITIAL OR
               NOT rm07m-kdauf IS INITIAL )
              OR NOT rm07m-xkont IS INITIAL ).
      " nur dann macht RM07RKON
      " was sinnvolles
      IF NOT rm07m-werks IS INITIAL.
        rwerks-low    = rm07m-werks.
        rwerks-sign   = 'I'.
        rwerks-option = 'EQ'.
        APPEND rwerks.
      ENDIF.
      REFRESH rbdter.
      IF NOT rm07m-bdter IS INITIAL.
        rbdter-low    = rm07m-bdter.
        rbdter-sign   = 'I'.
        PERFORM h_option_setzen.
        APPEND rbdter.
      ENDIF.
      SUBMIT rm07rkon AND RETURN WITH rk_kostl INCL rm07m-kostl
                                 WITH rk_projn INCL rm07m-projn
                                 WITH rk_nplnr INCL rm07m-nplnr
                                 WITH    vornr INCL rm07m-vornr
                                 WITH rk_anln1 INCL rm07m-anln1
                                 WITH rk_anln2 INCL rm07m-anln2
                                 WITH rk_umwrk INCL rm07m-umwrk
                                 WITH rk_umlgo INCL rm07m-umlgo
                                 WITH rk_aufnr INCL rm07m-aufnr
                                 WITH rk_kdauf INCL rm07m-kdauf
                                 WITH rk_kdpos INCL rm07m-kdpos
                                 WITH rk_kdein INCL rm07m-kdein
                                 WITH rk_xkont INCL rm07m-xkont
                                 WITH werks    IN   rwerks
                                 WITH bdter    IN   rbdter
                                 WITH xcall    INCL x
*                               WITH kzneb    INCL am07m-kzneb
*                               WITH xstorno  INCL t063f-xnbwa
                                 WITH sobkz    INCL rm07m-sobkz.
    ELSE.
      IF rm07m-werks IS INITIAL.
        xinit = x.
      ENDIF.
      REFRESH rwerks.
      IF NOT rm07m-werks IS INITIAL.
        rwerks-low    = rm07m-werks.
        rwerks-sign   = 'I'.
        rwerks-option = 'EQ'.
        APPEND rwerks.
      ENDIF.
      REFRESH rmatnr.
      IF NOT rm07m-matnr IS INITIAL.
        rmatnr-low    = rm07m-matnr.
        rmatnr-sign   = 'I'.
        rmatnr-option = 'EQ'.
        APPEND rmatnr.
      ENDIF.
      REFRESH rbdter.
      IF NOT rm07m-bdter IS INITIAL.
        rbdter-low    = rm07m-bdter.
        rbdter-sign   = 'I'.
        PERFORM h_option_setzen.
        APPEND rbdter.
      ENDIF.
      SUBMIT rm07rmat AND RETURN WITH rm_matnr IN   rmatnr
                                 WITH rm_werks IN   rwerks
                                 WITH rm_bdter IN   rbdter
                                 WITH kostl    INCL rm07m-kostl
                                 WITH aufnr    INCL rm07m-aufnr
                                 WITH projn    INCL rm07m-projn
                                 WITH nplnr    INCL rm07m-nplnr
                                 WITH vornr    INCL rm07m-vornr
                                 WITH anln1    INCL rm07m-anln1
                                 WITH anln2    INCL rm07m-anln2
                                 WITH umwrk    INCL rm07m-umwrk
                                 WITH umlgo    INCL rm07m-umlgo
                                 WITH kdauf    INCL rm07m-kdauf
                                 WITH kdpos    INCL rm07m-kdpos
                                 WITH kdein    INCL rm07m-kdein
                                 WITH xkont    INCL rm07m-xkont
                                 WITH xcall    INCL x
                                 WITH xinit    INCL xinit
*                               WITH xstorno  INCL t063f-xnbwa
*                               WITH kzneb    INCL am07m-kzneb
                                 WITH sobkz    INCL rm07m-sobkz.
    ENDIF.
*  IF NOT abbrechen IS INITIAL.
*    PERFORM abbrechen.
*  ENDIF.
    IMPORT seltab1 FROM MEMORY ID 'SELTAB1'.
    DESCRIBE TABLE seltab1 LINES index_z.
    IF index_z IS INITIAL.
      MESSAGE e083(m7).
    ENDIF.
    SORT seltab1 BY rsnum rspos.
* PERFORM SUCHFELDER_INIT.
  ENDIF.
ENDMODULE.                 " eingabe_konr  INPUT

*&---------------------------------------------------------------------*
*&      Form  h_option_setzen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM h_option_setzen.
*--- this logic copied from standard program
  rbdter-option = rm07m-option_mb.
  CASE rbdter-option.
    WHEN '='.
      rbdter-option = 'EQ'.
    WHEN '<>'.
      rbdter-option = 'NE'.
    WHEN '>'.
      rbdter-option = 'GT'.
    WHEN '>='.
      rbdter-option = 'GE'.
    WHEN '<'.
      rbdter-option = 'LT'.
    WHEN '<='.
      rbdter-option = 'LE'.
    WHEN 'CP'.
      rbdter-option = 'EQ'.
    WHEN 'NP'.
      rbdter-option = 'EQ'.
    WHEN '  '.
      rbdter-option = 'EQ'.
  ENDCASE.
ENDFORM.                    " h_option_setzen

*&---------------------------------------------------------------------*
*&      Module  reservierung_lesen  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE reservierung_lesen INPUT.
*---
  DATA : BEGIN OF it_bwart OCCURS 0,
           bwart LIKE mseg-bwart,
         END OF it_bwart.

  DATA : l_lines TYPE i.

  CLEAR : it_itab, it_itab[], it_bwart, it_bwart[], l_lines.

  LOOP AT seltab1.
    CLEAR : rkpf, resb.
    SELECT SINGLE * INTO rkpf
                    FROM rkpf
                   WHERE rsnum EQ seltab1-rsnum.
    MOVE : rkpf-bwart TO it_bwart-bwart,
           rkpf-umwrk TO msegk-umwrk,
           rkpf-umlgo TO msegk-umlgo.
    COLLECT it_bwart.
    SELECT * INTO resb
             FROM resb
            WHERE rsnum EQ seltab1-rsnum
              AND kzear EQ space.
      MOVE-CORRESPONDING resb TO it_itab.
      APPEND it_itab.
    ENDSELECT.
  ENDLOOP.

  IF it_itab[] IS INITIAL.
    CLEAR : seltab1, seltab1[].
    MESSAGE e064(m7) WITH rm07m-rsnum.
  ENDIF.

  DESCRIBE TABLE it_bwart LINES l_lines.

  IF l_lines GT 2.
    MESSAGE e999 WITH text-m03.
  ENDIF.

*---
  DATA : BEGIN OF it_mard OCCURS 0,
           matnr LIKE mard-matnr,
           werks LIKE mard-werks,
           lgort LIKE mard-lgort,
         END OF it_mard.

  DATA : l_mard_lines TYPE i.

  LOOP AT it_itab.
*--- get warehouse number
    CLEAR : t320.
    SELECT SINGLE lgnum INTO mseg-lgnum
                        FROM t320
                       WHERE werks EQ it_itab-werks
                         AND lgort EQ it_itab-lgort.
*--- get storage bin & number
    CLEAR : mlgt.
    SELECT SINGLE lgtyp
                  lgpla INTO (it_itab-lgtyp, it_itab-lgpla)
                        FROM mlgt
                       WHERE matnr EQ it_itab-matnr
                         AND lgnum EQ mseg-lgnum
                         AND lvorm EQ space.
*--- get storage location, if S/L does not exist.
    IF it_itab-lgort EQ space.
      CLEAR : mard, it_mard, it_mard[].
      SELECT matnr
             werks
             lgort INTO CORRESPONDING FIELDS OF TABLE it_mard
                   FROM mard
                  WHERE matnr EQ it_itab-matnr
                    AND werks EQ it_itab-werks.
      DESCRIBE TABLE it_mard LINES l_mard_lines.
      IF l_mard_lines EQ 1.
        CLEAR : it_mard.
        READ TABLE it_mard INDEX 1.
        MOVE : it_mard-lgort TO it_itab-lgort.
      ENDIF.
    ENDIF.
    MODIFY it_itab.
  ENDLOOP.

*---
  CLEAR : it_bwart.

  READ TABLE it_bwart INDEX 1.

  MOVE : it_bwart-bwart TO rm07m-bwartwa,
         rm07m-bwartwa  TO msegk-bwart.

  CLEAR : t156t, dm07m-btext.
  SELECT SINGLE btext INTO dm07m-btext
                      FROM t156t
                     WHERE spras EQ sy-langu
                       AND bwart EQ rm07m-bwartwa.

*---
  LEAVE TO SCREEN 0.
ENDMODULE.                 " reservierung_lesen  INPUT
