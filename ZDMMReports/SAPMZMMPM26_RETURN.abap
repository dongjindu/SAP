************************************************************************
* Program Name      : SAPMZMMPM26_RETURN
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.11.11.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : MM Return to Vendor with WM
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.11.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT  sapmzmmpm26_return MESSAGE-ID zmmm.

**---
INCLUDE : fm06lu02,
          zrmmpmxxr_incl.

TYPE-POOLS : cxtab.

**---
DATA : BEGIN OF it_itab OCCURS 0,
         ebeln LIKE ekpo-ebeln,
         ebelp LIKE ekpo-ebelp,
         matnr LIKE mseg-matnr,
         bwart LIKE mseg-bwart,
         shkzg LIKE mseg-shkzg,
         erfmg LIKE mseg-erfmg,
         erfme LIKE mseg-erfme,
         lgnum LIKE mseg-lgnum,
         lgtyp LIKE mseg-lgtyp,
         lgpla LIKE mseg-lgpla,
         lgort LIKE mseg-lgort,
         werks LIKE mseg-werks,
         wempf LIKE mseg-wempf,
         grund LIKE mseg-grund,
         lfbja LIKE mseg-lfbja,
         lfbnr LIKE mseg-lfbnr,
         lfpos LIKE mseg-lfpos,
         zeile LIKE mseg-zeile,
         xselk LIKE rm07m-xselk,
       END OF it_itab.

*DATA : BEGIN OF it_itab OCCURS 0.
*        INCLUDE STRUCTURE wueb.
*DATA :   xselk LIKE rm07m-xselk,
*         grund LIKE mseg-grund,
*         lgtyp LIKE mseg-lgtyp,
*         lgpla LIKE mseg-lgpla,
*         wempf LIKE mseg-wempf,
*         zeile LIKE mseg-zeile,
*       END OF it_itab.

DATA : it_temp LIKE it_itab OCCURS 0 WITH HEADER LINE,
       it_temp_m LIKE it_itab OCCURS 0 WITH HEADER LINE,
       it_itab_copy LIKE it_itab OCCURS 0 WITH HEADER LINE.

DATA : it_xekbe  LIKE ekbe OCCURS 0 WITH HEADER LINE,
       it_xekbes LIKE ekbes OCCURS 0 WITH HEADER LINE,
       it_xekbez LIKE ekbez OCCURS 0 WITH HEADER LINE,
       it_xekbnk LIKE ekbnk OCCURS 0 WITH HEADER LINE,
       it_xekbz  LIKE ekbz OCCURS 0 WITH HEADER LINE.

DATA : it_text(220) OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF it_ebeln OCCURS 0,
         ebeln LIKE ekpo-ebeln,
       END OF it_ebeln.

DATA : BEGIN OF it_xebefu OCCURS 25.
        INCLUDE STRUCTURE ebefu.
DATA : END OF it_xebefu.

DATA : it_xebefu_temp LIKE it_xebefu OCCURS 0 WITH HEADER LINE.


*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

*----- BAPI
DATA : st_goodsmvt_header  LIKE bapi2017_gm_head_01,
       st_goodsmvt_code    LIKE bapi2017_gm_code,
       st_goodsmvt_headret LIKE bapi2017_gm_head_ret,
       materialdocument LIKE bapi2017_gm_head_ret-mat_doc,
       matdocumentyear  LIKE bapi2017_gm_head_ret-doc_year,
       it_goodsmvt_item    LIKE bapi2017_gm_item_create OCCURS 0
                                                    WITH HEADER LINE,
       it_return           LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

**---
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

**---
DATA : pstyp      LIKE vm07m-pstyp.     " Hilfsfeld für LB

DATA : w_okcode LIKE sy-ucomm,
       w_save_okcode LIKE sy-ucomm,
       w_index LIKE sy-index,
       w_loopc LIKE sy-loopc,
       w_top_line TYPE i VALUE 1,
       w_lines TYPE i,
       w_selected(1).

DATA : w_mode LIKE ctu_params-dismode VALUE 'N'.

DATA : w_complete(1),
       w_answer(1).

DATA : w_mblnr LIKE mseg-mblnr,
       w_tanum LIKE ltak-tanum,
       w_subrc LIKE sy-subrc.

DATA : w_ebeln_lines TYPE i.

**---
CONSTANTS : u TYPE c VALUE 'U'.

CONSTANTS : c_mb0a LIKE sy-tcode VALUE 'MB0A'.

CONSTANTS: BEGIN OF kzbwa,
             01 LIKE t156-kzbwa VALUE '01',   "Umlagerbestellung
             02 LIKE t156-kzbwa VALUE '02',   "Stückliste WA
             03 LIKE t156-kzbwa VALUE '03',   "Cross Company
             04 LIKE t156-kzbwa VALUE '04',   "Cross Company 1 Schritt
             05 LIKE t156-kzbwa VALUE '05',   "Umlagerbestell. 1 Schritt
             06 LIKE t156-kzbwa VALUE '06',   "Umlagerbestell. 2 Schritt
                                              "über BAPI
             07 LIKE t156-kzbwa VALUE '07',   "Umbewertung Charge
             b3 LIKE t156-kzbwa VALUE 'B3',   "3rd party Brazil
             sz LIKE t156-kzbwa VALUE 'SZ',   "strukt Material zerlegen
           END OF kzbwa.

CONSTANTS : c_bwart_122 LIKE mseg-bwart VALUE '122',
            c_mvt_ind LIKE it_goodsmvt_item-mvt_ind VALUE 'B'. " PO





*&---------------------------------------------------------------------*
*&      Module  transaction_init  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE transaction_init OUTPUT.
**---
  IF mkpf-budat IS INITIAL.
    MOVE : sy-datum TO mkpf-budat.
  ENDIF.

  IF mkpf-bldat IS INITIAL.
    MOVE : sy-datum TO mkpf-bldat.
  ENDIF.

  IF rm07m-bwartwe IS INITIAL.
    MOVE : '122' TO rm07m-bwartwe.
  ENDIF.
ENDMODULE.                 " transaction_init  OUTPUT

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
      READ TABLE it_return WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        SET PF-STATUS '9100'.
      ELSE.
        READ TABLE it_message WITH KEY msgtyp = 'E'.
        IF sy-subrc EQ 0.
          SET PF-STATUS '9100'.
        ELSE.
          SET PF-STATUS '9100' EXCLUDING 'LOG'.
        ENDIF.
      ENDIF.
      SET TITLEBAR '9100'.
  ENDCASE.
ENDMODULE.                 " status_scrcom  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
**---
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
            PERFORM exit_comfirm(sapmzmmpm30_canc) USING w_answer.
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
*&      Form  save_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_document.
**---
  READ TABLE it_itab WITH KEY xselk = 'X'.

  IF sy-subrc EQ 0.
    LOOP AT it_itab WHERE xselk NE space.
      IF it_itab-grund EQ space.
        MESSAGE e999 WITH text-006.
        EXIT.
      ENDIF.
    ENDLOOP.

    PERFORM posting_return_to_vendor.
  ELSE.
    MESSAGE w061(m7).
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.

*---
  MOVE : 'X' TO w_complete.
ENDFORM.                    " save_document

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
          PERFORM best_suchen_ranges.
          PERFORM best_suchen_best.
          IF NOT it_itab[] IS INITIAL.
            CALL SCREEN 9100.
          ENDIF.
      ENDCASE.
    WHEN '9100'.
      CASE w_save_okcode.
        WHEN 'SAVE'.
          CLEAR : w_save_okcode.
          PERFORM save_document.
        WHEN 'SALL'.
          CLEAR : w_save_okcode.
          PERFORM select_deselect_all USING 'X'.
        WHEN 'DALL'.
          CLEAR : w_save_okcode.
          PERFORM select_deselect_all USING ' '.
        WHEN 'LOG'.
          CLEAR : w_save_okcode.
          PERFORM display_error_log.
        WHEN 'P--' OR 'P-' OR 'P+' OR 'P++'.     " Page Scroll
          PERFORM table_control_page_scrol(sapmzmmpm30_canc)
                                           USING w_save_okcode
                                                 w_top_line
                                                 w_lines
                                                 w_loopc.
          CLEAR : w_save_okcode.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " user_command_scrcom  INPUT

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0186   text
*----------------------------------------------------------------------*
FORM select_deselect_all USING    p_value.
**---
  MOVE : p_value TO it_itab-xselk.

  MODIFY it_itab TRANSPORTING xselk WHERE ebeln GE space.
ENDFORM.                    " select_deselect_all

*&---------------------------------------------------------------------*
*&      Form  display_error_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_error_log.
*--- BDC Error
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

*--- BAPI Error
  READ TABLE it_return WITH KEY type = 'E'.

  MOVE : it_return-message TO it_text.
  APPEND it_text.

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
*&      Form  best_suchen_best
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM best_suchen_best.
*---
  CLEAR : wueb, wueb[].

  MOVE : 'X' TO wueb-calkz,
         'X' TO wueb-nrcfd.

  EXPORT wueb-calkz wueb-nrcfd wueb TO MEMORY ID 'WUEB'.

  IF am07m-matnv IS INITIAL.
    IF NOT am07m-lifnr IS INITIAL.
*      SUBMIT zrm06el00 AND RETURN WITH el_lifnr INCL am07m-lifnr
      SUBMIT rm06el00 AND RETURN WITH el_lifnr INCL am07m-lifnr
                                 WITH s_werks  IN   rwerks
                                 WITH s_pstyp  IN   rpstyp
                                 WITH s_eindt  IN   reindt
                                 WITH selpa    INCL t156-selpa
                                 WITH s_idnlf  IN   ridnlf
                                 WITH s_ean11  IN   rean11
                                 WITH listu    EQ   'BEST'.
    ELSE.
      SUBMIT rm06ew00 AND RETURN WITH ew_reswk IN   rreswk
                                 WITH s_werks  IN   rwerks
                                 WITH s_pstyp  IN   rpstyp
                                 WITH s_eindt  IN   reindt  "357116
                                 WITH selpa    INCL t156-selpa
                                 WITH listu    EQ   'BEST'.
    ENDIF.
  ELSE.
    REFRESH rmatnv.
    rmatnv-low    = am07m-matnv.
    rmatnv-sign   = 'I'.
    rmatnv-option = 'EQ'.
    APPEND rmatnv.
    IF pstyp EQ u.
      CLEAR rwerks.
      REFRESH rwerks.
    ENDIF.
    IF t156-kzbwa NE kzbwa-02.
* PFAFFP: begin: Umlagerbestellung in einem Schritt
*     IF T156-KZBWA = KZBWA-01.
      IF t156-kzbwa = kzbwa-01 OR t156-kzbwa = kzbwa-05.
* end
        CLEAR rwerks.
        REFRESH rwerks.
      ENDIF.
      SUBMIT rm06em00 AND RETURN WITH em_matnr  IN   rmatnv
                                 WITH  s_lifnr  IN   rlifnr
                                 WITH  em_werks IN   rwerks
                                 WITH  s_reswk  IN   rreswk
                                 WITH  selpa    INCL t156-selpa
                                 WITH  s_pstyp  IN   rpstyp
                                 WITH  s_eindt  IN   reindt
                                 WITH  s_idnlf  IN   ridnlf
                                 WITH  s_ean11  IN   rean11
                                 WITH  listu    EQ   'BEST'.
    ELSE.
      SUBMIT rm07lb00 AND RETURN WITH matnr  =   am07m-matnv
                                 WITH werks IN   rreswk.

    ENDIF.
  ENDIF.

  IMPORT wueb-calkz wueb-nrcfd wueb FROM MEMORY ID 'WUEB'.

*---
  CLEAR : it_temp, it_temp[], it_temp_m, it_temp_m[],
          it_itab, it_itab[], it_xebefu, it_xebefu[],
          it_xebefu_temp, it_xebefu_temp[].

  LOOP AT wueb.
    PERFORM get_goods_receipt_info.
  ENDLOOP.

  DELETE it_xebefu WHERE wemng EQ 0.

  DATA : l_zeile TYPE i.

  LOOP AT it_xebefu.
    MOVE-CORRESPONDING it_xebefu TO it_itab.
    MOVE : it_xebefu-wemng       TO it_itab-erfmg,
           it_xebefu-bstme       TO it_itab-erfme.
    l_zeile = l_zeile + 1.
    MOVE : l_zeile       TO it_itab-zeile,
           'X'           TO it_itab-xselk,
           rm07m-bwartwe TO it_itab-bwart,
           rm07m-grund   TO it_itab-grund.
    PERFORM set_wm_information.
    APPEND it_itab.
    CLEAR : it_xebefu, it_itab.
  ENDLOOP.


*  LOOP AT wueb.
*    SELECT * INTO mseg
*             FROM mseg
*            WHERE ebeln EQ wueb-ebeln
*              AND ebelp EQ wueb-ebelp.
*      it_temp-ebeln = it_temp_m-ebeln = mseg-ebeln.
*      it_temp-ebelp = it_temp_m-ebelp = mseg-ebelp.
*      it_temp-matnr = it_temp_m-matnr = mseg-matnr.
*      it_temp-bwart = it_temp_m-bwart = mseg-bwart.
*      it_temp-shkzg = it_temp_m-shkzg = mseg-shkzg.
*      it_temp-erfmg = it_temp_m-erfmg = mseg-erfmg.
*      it_temp-erfme = it_temp_m-erfme = mseg-erfme.
**      it_temp-lgtyp = it_temp_m-lgtyp = mseg-lgtyp.
**      it_temp-lgpla = it_temp_m-lgpla = mseg-lgpla.
*      it_temp-lgort = it_temp_m-lgort = mseg-lgort.
*      it_temp-werks = it_temp_m-werks = mseg-werks.
*      IF mseg-shkzg EQ 'S'.
*        APPEND it_temp.
*      ELSEIF mseg-shkzg EQ 'H'.
*        APPEND it_temp_m.
*      ENDIF.
*    ENDSELECT.
*  ENDLOOP.
*
*  DATA : l_zeile TYPE i.
*
*  LOOP AT it_temp.
*    MOVE-CORRESPONDING it_temp TO it_itab.
*    LOOP AT it_temp_m WHERE ebeln EQ it_temp-ebeln
*                        AND ebelp EQ it_temp-ebelp.
*      IF it_temp-erfmg EQ it_temp_m-erfmg.
*        it_temp-erfmg = it_temp-erfmg - it_temp_m-erfmg.
*        DELETE it_temp_m.
*        CONTINUE.
*      ENDIF.
*      IF it_temp-erfmg GT it_temp_m-erfmg.
*        it_temp-erfmg = it_temp-erfmg - it_temp_m-erfmg.
*        DELETE it_temp_m.
*        CONTINUE.
*      ENDIF.
*    ENDLOOP.
*    MOVE : it_temp-erfmg TO it_itab-erfmg.
*    IF NOT it_itab-erfmg IS INITIAL.
*      l_zeile = l_zeile + 1.
*      MOVE : l_zeile       TO it_itab-zeile,
*             'X'           TO it_itab-xselk,
*             rm07m-bwartwe TO it_itab-bwart,
*             rm07m-grund   TO it_itab-grund.
*      PERFORM set_wm_information.
*      APPEND it_itab.
*    ENDIF.
*    CLEAR : it_temp, it_itab.
*  ENDLOOP.


**---
*
*  CLEAR : it_itab, it_itab[], l_zeile, it_temp, it_temp[].
*
*  LOOP AT wueb.
*    CLEAR : ekpo.
*    SELECT SINGLE werks
*                  lgort
*                  meins
*                  webre INTO (ekpo-werks, ekpo-lgort, ekpo-meins,
*                              ekpo-webre)
*                        FROM ekpo
*                       WHERE ebeln EQ wueb-ebeln
*                         AND ebelp EQ wueb-ebelp.
*    PERFORM me_read_history USING wueb-ebeln wueb-ebelp ekpo-webre.
*    LOOP AT it_xekbe.
*      l_zeile = l_zeile + 1.
*      MOVE-CORRESPONDING wueb TO it_temp.
*      MOVE : 'X'            TO it_temp-xselk,
*             l_zeile        TO it_temp-zeile,
*             it_xekbe-menge TO it_temp-erfmg,
*             ekpo-werks     TO it_temp-werks,
*             ekpo-lgort     TO it_temp-lgort,
*             ekpo-meins     TO it_temp-erfme.
*      MOVE : it_xekbe-bwart TO it_temp-bwart.
*      APPEND it_temp.
*      CLEAR : it_temp.
*    ENDLOOP.
*  ENDLOOP.
ENDFORM.                    " best_suchen_best

*&---------------------------------------------------------------------*
*&      Module  bwart_we  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE bwart_we INPUT.
*---
  CLEAR : t158b.

  SELECT SINGLE * FROM t158b
                 WHERE tcode EQ c_mb0a
                   AND bwart EQ rm07m-bwartwe.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE e096(m7) WITH rm07m-bwartwe.
  ENDIF.

*---
  CLEAR : t156.

  SELECT SINGLE * FROM t156
                 WHERE bwart EQ rm07m-bwartwe.
ENDMODULE.                 " bwart_we  INPUT

*&---------------------------------------------------------------------*
*&      Form  best_suchen_ranges
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM best_suchen_ranges.
*---
  REFRESH rpstyp.
  IF NOT pstyp IS INITIAL.
    rpstyp-low    = pstyp.
    rpstyp-sign   = 'I'.
    rpstyp-option = 'EQ'.
    APPEND rpstyp.
  ENDIF.

  REFRESH rlifnr.
  IF NOT am07m-lifnr IS INITIAL.
    rlifnr-low    = am07m-lifnr.
    rlifnr-sign   = 'I'.
    rlifnr-option = 'EQ'.
    APPEND rlifnr.
  ENDIF.

  REFRESH ridnlf.
  IF NOT rm07m-idnlf IS INITIAL.
    ridnlf-low    = rm07m-idnlf.
    ridnlf-sign   = 'I'.
    ridnlf-option = 'EQ'.
    APPEND ridnlf.
  ENDIF.

  REFRESH rean11.
  IF NOT rm07m-ean11 IS INITIAL.
    rean11-low    = rm07m-ean11.
    rean11-sign   = 'I'.
    rean11-option = 'EQ'.
    APPEND rean11.
  ENDIF.

  CLEAR rreswk.
  REFRESH rreswk.
  IF NOT am07m-reswk IS INITIAL.
    rreswk-low    = am07m-reswk.
    rreswk-sign   = 'I'.
    rreswk-option = 'EQ'.
    APPEND rreswk.
  ENDIF.

  CLEAR rwerks.
  REFRESH rwerks.
  IF NOT rm07m-werks IS INITIAL.
    rwerks-low    = rm07m-werks.
    rwerks-sign   = 'I'.
    rwerks-option = 'EQ'.
    APPEND rwerks.
  ENDIF.

  REFRESH reindt.
  IF NOT rm07m-eindt IS INITIAL.
    IF rm07m-eindb IS INITIAL.
      reindt-low    = rm07m-eindt.
      reindt-sign   = 'I'.
      reindt-option = 'EQ'.
    ELSE.
      reindt-low    = rm07m-eindt.
      reindt-high   = rm07m-eindb.
      reindt-sign   = 'I'.
      reindt-option = 'BT'.
    ENDIF.
    APPEND reindt.
  ELSEIF NOT rm07m-eindb IS INITIAL.
    CLEAR reindt-low.
    reindt-high   = rm07m-eindb.
    reindt-sign   = 'I'.
    reindt-option = 'BT'.
    APPEND reindt.
  ENDIF.

  CLEAR pstyp.
ENDFORM.                    " best_suchen_ranges

*&---------------------------------------------------------------------*
*&      Module  check_date  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_date INPUT.
*---
  IF mkpf-bldat IS INITIAL.
    MESSAGE e999 WITH text-m01.
  ENDIF.

  IF mkpf-budat IS INITIAL.
    MESSAGE e669(f5).
  ENDIF.

**---
*  IF mkpf-bldat GT sy-datum.
*    MESSAGE w192(f5).
*  ENDIF.
*
**---
*  IF mkpf-bldat(4) NE mkpf-budat(4).
*    MESSAGE w193(f5).
*  ENDIF.
ENDMODULE.                 " check_date  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_input_scr9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_input_scr9000 INPUT.
*--- check plant
  IF rm07m-werks NE space.
    CLEAR : t001w.
    SELECT SINGLE * FROM t001w
                   WHERE werks EQ rm07m-werks.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-m02.
    ENDIF.
  ENDIF.

*--- check storage location
  IF rm07m-werks NE space AND rm07m-lgort NE space.
    CLEAR : t001l.
    SELECT SINGLE * FROM t001l
                   WHERE werks EQ rm07m-werks
                     AND lgort EQ rm07m-lgort.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-m03.
    ENDIF.
  ENDIF.

*--- check reason for movement
  IF rm07m-bwartwe NE space AND rm07m-grund NE space.
    CLEAR : t157d.
    SELECT SINGLE * FROM t157d
                   WHERE bwart EQ rm07m-bwartwe
                     AND grund EQ rm07m-grund.
    IF sy-subrc NE 0.
      MESSAGE e999 WITH text-m04.
    ENDIF.
  ENDIF.
ENDMODULE.                 " check_input_scr9000  INPUT

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
    MOVE : it_itab-xselk       TO rm07m-xselk.
    PERFORM get_material_desc USING mseg-matnr.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.

*---
  MOVE : sy-loopc TO w_loopc.
ENDMODULE.                 " display_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  control_screen_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE control_screen_scr9100 OUTPUT.
*---
  LOOP AT SCREEN.
    IF screen-name EQ 'MSEG-LGNUM'.
      screen-invisible = 1.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " control_screen_scr9100  OUTPUT

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
    MOVE : rm07m-xselk      TO it_itab-xselk,
           mseg-grund       TO it_itab-grund.
    MODIFY it_itab INDEX w_index.
  ENDIF.

*---
  CHECK rm07m-xselk NE space.

  IF mseg-erfmg IS INITIAL.
    SET CURSOR FIELD 'MSEG-ERFMG' LINE sy-stepl.
    MESSAGE e999 WITH text-004.
  ENDIF.

  IF mseg-lgort IS INITIAL.
    SET CURSOR FIELD 'MSEG-LGORT' LINE sy-stepl.
    MESSAGE e999 WITH text-005.
  ENDIF.

*  IF mseg-grund IS INITIAL.
*    SET CURSOR FIELD 'MSEG-GRUND' LINE sy-stepl.
*    MESSAGE e999 WITH text-006.
*  ENDIF.

*  IF mseg-lgtyp IS INITIAL.
*    SET CURSOR FIELD 'MSEG-LGTYP' LINE sy-stepl.
*    messgae e999 with text-007.
*  ENDIF.
*
*  IF mseg-lgpla IS INITIAL.
*    SET CURSOR FIELD 'MSEG-LGPLA' LINE sy-stepl.
*    MESSAGE e999 WITH text-008.
*  ENDIF.
ENDMODULE.                 " check_input_scr9100  INPUT

*&---------------------------------------------------------------------*
*&      Form  me_read_history
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WUEB_EBELN  text
*      -->P_WUEB_EBELP  text
*----------------------------------------------------------------------*
FORM me_read_history USING    p_ebeln
                              p_ebelp
                              p_webre.
*---
  CLEAR : it_xekbe, it_xekbe[], it_xekbes, it_xekbes[], it_xekbez,
          it_xekbez[], it_xekbnk, it_xekbnk[], it_xekbz, it_xekbz[].

  CALL FUNCTION 'ME_READ_HISTORY'
       EXPORTING
            ebeln  = p_ebeln
            ebelp  = p_ebelp
            webre  = p_webre
       TABLES
            xekbe  = it_xekbe
            xekbes = it_xekbes
            xekbez = it_xekbez
            xekbnk = it_xekbnk
            xekbz  = it_xekbz.
ENDFORM.                    " me_read_history

*&---------------------------------------------------------------------*
*&      Form  posting_return_to_vendor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM posting_return_to_vendor.
**---
  CLEAR : st_goodsmvt_header, st_goodsmvt_code, st_goodsmvt_headret,
          materialdocument, matdocumentyear, it_goodsmvt_item,
          it_goodsmvt_item[], it_return, it_return[], it_ebeln,
          it_ebeln[].

*--- header
  MOVE : mkpf-budat TO st_goodsmvt_header-pstng_date,
         mkpf-bldat TO st_goodsmvt_header-doc_date.

*--- code
  MOVE : '01' TO st_goodsmvt_code-gm_code.

*--- append items
  LOOP AT it_itab WHERE xselk NE space.
    MOVE : it_itab-matnr     TO it_goodsmvt_item-material,
           it_itab-werks     TO it_goodsmvt_item-plant,
           it_itab-lgort     TO it_goodsmvt_item-stge_loc,
           rm07m-bwartwe     TO it_goodsmvt_item-move_type,
           am07m-lifnr       TO it_goodsmvt_item-vendor,
           it_itab-erfmg     TO it_goodsmvt_item-entry_qnt,
           it_itab-erfme     TO it_goodsmvt_item-entry_uom,
           it_itab-erfme     TO it_goodsmvt_item-entry_uom_iso,
           it_itab-ebeln     TO it_goodsmvt_item-po_number,
           it_itab-ebelp     TO it_goodsmvt_item-po_item,
           c_mvt_ind         TO it_goodsmvt_item-mvt_ind,
           it_itab-grund     TO it_goodsmvt_item-move_reas.
    MOVE : it_itab-lfbja     TO it_goodsmvt_item-ref_doc_yr,
           it_itab-lfbnr     TO it_goodsmvt_item-ref_doc,
           it_itab-lfpos     TO it_goodsmvt_item-ref_doc_it.
    APPEND it_goodsmvt_item.
    CLEAR : it_goodsmvt_item.
*---
    MOVE : it_itab-ebeln TO it_ebeln.
    COLLECT it_ebeln.
  ENDLOOP.

*---
  CLEAR : w_ebeln_lines.
  DESCRIBE TABLE it_ebeln LINES w_ebeln_lines.

*--- call BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
       EXPORTING
            goodsmvt_header  = st_goodsmvt_header
            goodsmvt_code    = st_goodsmvt_code
       IMPORTING
            goodsmvt_headret = st_goodsmvt_headret
            materialdocument = materialdocument
            matdocumentyear  = matdocumentyear
       TABLES
            goodsmvt_item    = it_goodsmvt_item
            return           = it_return.

  IF NOT st_goodsmvt_headret IS INITIAL AND it_return[] IS INITIAL.
*--- call BAPI Commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
         EXPORTING
              wait = 'X'.
    PERFORM check_transfer_order_condition USING w_subrc.
    IF w_subrc EQ 0.     " if mat'l doc. related with transfer order
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
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*               EXPORTING
*                    wait = 'X'.
          CLEAR : w_tanum.
          MOVE : it_mess-msgv1 TO w_tanum.
          MESSAGE s021 WITH st_goodsmvt_headret-mat_doc w_tanum.
          LEAVE TO TRANSACTION sy-tcode.
        ENDIF.
      ELSE.
        PERFORM display_bdc_error_message.
      ENDIF.
    ELSE.                " if mat'l doc. not related with transfer order
      MESSAGE s060(m7) WITH st_goodsmvt_headret-mat_doc.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    PERFORM display_bapi_error_message.
  ENDIF.
ENDFORM.                    " posting_return_to_vendor

*&---------------------------------------------------------------------*
*&      Form  check_transfer_order_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM check_transfer_order_condition USING    p_subrc.
*---
  CLEAR : ltbk.

  SELECT SINGLE * FROM ltbk
                 WHERE mblnr EQ st_goodsmvt_headret-mat_doc
                   AND mjahr EQ st_goodsmvt_headret-doc_year.

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
          it_itab_copy[], it_message, it_message[].

*---
  LOOP AT it_itab WHERE xselk NE space.
    SELECT SINGLE * FROM t320
                   WHERE werks EQ it_itab-werks
                     AND lgort EQ it_itab-lgort.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING it_itab TO it_itab_copy.
      APPEND it_itab_copy.
      CLEAR : it_itab_copy.
    ENDIF.
  ENDLOOP.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
                   ' '  'RL02B-MBLNR'     st_goodsmvt_headret-mat_doc,
                   ' '  'RL02B-MJAHR'     st_goodsmvt_headret-doc_year,
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
  LOOP AT it_itab WHERE xselk NE space.
    SELECT SINGLE * FROM t320
                   WHERE werks EQ it_itab-werks
                     AND lgort EQ it_itab-lgort.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING it_itab TO it_itab_copy.
      APPEND it_itab_copy.
      CLEAR : it_itab_copy.
    ENDIF.
  ENDLOOP.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
                    ' '  'RL02B-MBLNR'     st_goodsmvt_headret-mat_doc,
                    ' '  'RL02B-MJAHR'     st_goodsmvt_headret-doc_year,
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

*--- if multi PO
  IF w_ebeln_lines GT 1.
    PERFORM dynpro USING : 'X'  'SAPMSSY0'        '0120',
                           ' '  'BDC_OKCODE'      '=MRKA'.
    PERFORM dynpro USING : 'X'  'SAPMSSY0'        '0120',
                           ' '  'BDC_OKCODE'      '=TAH'.
  ELSE.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                           ' '  'BDC_SUBSCR'      'SAPML03T',
                           ' '  'BDC_OKCODE'      '=MRKA'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                           ' '  'BDC_SUBSCR'      'SAPML03T',
                           ' '  'BDC_OKCODE'      '=TTYP'.
  ENDIF.

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
*&      Form  display_bapi_error_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_bapi_error_message.
*---
  READ TABLE it_return WITH KEY type = 'E'.

  MESSAGE e999 WITH it_return-message.
ENDFORM.                    " display_bapi_error_message

*&---------------------------------------------------------------------*
*&      Form  display_bdc_error_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_bdc_error_message.
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
ENDFORM.                    " display_bdc_error_message

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1959   text
*      -->P_1960   text
*      -->P_1961   text
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
*&      Module  check_storage_bin  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_storage_bin INPUT.
**---
  CHECK rm07m-xselk NE space.

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
ENDMODULE.                 " check_storage_bin  INPUT

*&---------------------------------------------------------------------*
*&      Form  set_wm_information
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_wm_information.
**---
  CLEAR : t320.
  SELECT SINGLE lgnum INTO it_itab-lgnum
                      FROM t320
                     WHERE werks EQ it_itab-werks
                       AND lgort EQ it_itab-lgort.
*---
  CLEAR : mlgt.
  SELECT SINGLE lgtyp
                lgpla INTO (it_itab-lgtyp, it_itab-lgpla)
                      FROM mlgt
                     WHERE matnr EQ it_itab-matnr
                       AND lgnum EQ it_itab-lgnum
                       AND lvorm EQ space.
ENDFORM.                    " set_wm_information

*&---------------------------------------------------------------------*
*&      Form  get_goods_receipt_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_goods_receipt_info.
*---
  CLEAR : it_xebefu_temp, it_xebefu_temp[].

  CALL FUNCTION 'ME_READ_ITEM_GOODS_RECEIPT'
       EXPORTING
            budat          = mkpf-budat
            ebeln          = wueb-ebeln
            ebelp          = wueb-ebelp
            lfbnr          = ' '
            lfgja          = '0000'
            lfpos          = '0000'
            shkzg          = 'H'
            i_bldat        = mkpf-budat
       TABLES
            xebefu         = it_xebefu_temp
       EXCEPTIONS
            not_found_any  = 1
            not_found_one  = 2
            not_valid_any  = 3
            not_valid_one  = 4
            enqueue_failed = 5
            OTHERS         = 6.

  APPEND LINES OF it_xebefu_temp TO it_xebefu.
ENDFORM.                    " get_goods_receipt_info
