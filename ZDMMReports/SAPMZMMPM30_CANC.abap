************************************************************************
* Program Name      : SAPMZMMPM30_CANC
* Author            : Sung-Tae, Lim
* Creation Date     : 2003.10.23.
* Specifications By : Sung-Tae, Lim
* Development Request No : UD1K901864
* Addl Documentation:
* Description       : MM Material Document Cancel with WM
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.10.23.     Sung-Tae Lim     UD1K901864     Initial Coding
*
*
************************************************************************

REPORT  sapmzmmpm30_canc MESSAGE-ID zmmm.

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
         wempf LIKE mseg-wempf,
       END OF it_itab.

DATA : BEGIN OF it_mkpf OCCURS 0.
        INCLUDE STRUCTURE mkpf.
DATA : END OF it_mkpf.

DATA : BEGIN OF it_mseg OCCURS 0,
         xselk LIKE rm07m-xselk,
         zzeile LIKE mseg-zeile.
        INCLUDE STRUCTURE mseg.
DATA :   nltyp LIKE ltap-nltyp,
         nlber LIKE ltap-nlber,
         nlpla LIKE ltap-nlpla,
       END OF it_mseg.

DATA : BEGIN OF it_ymseg OCCURS 0.
        INCLUDE STRUCTURE it_mseg.
DATA : END OF it_ymseg.

DATA : it_text(220) OCCURS 0 WITH HEADER LINE.

**---
DATA : w_okcode LIKE sy-ucomm,
       w_save_okcode LIKE sy-ucomm,
       w_index LIKE sy-index,
       w_loopc LIKE sy-loopc,
       w_top_line TYPE i VALUE 1,
       w_lines TYPE i,
       w_selected(1).

DATA : w_mode LIKE ctu_params-dismode VALUE 'N'.

DATA : w_erfmg LIKE mseg-erfmg.

DATA : w_mblnr LIKE mseg-mblnr,
       w_tanum LIKE ltak-tanum,
       w_subrc LIKE sy-subrc,
       w_mbst_subrc LIKE sy-subrc,
       w_mbst_mblnr LIKE mseg-mblnr.

DATA : w_complete(1),
       w_answer(1).

DATA : w_refer TYPE i,
       w_string LIKE dm07m-fausw,
       w_werks_hide(1).

DATA : w_text_kostl(50),
       wa_cobl LIKE cobl.

DATA : w_budat LIKE mkpf-budat.

DATA : w_mseg_lines TYPE i,
       w_ymseg_lines TYPE i.

DATA : w_text(100).

*----- BDC
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_message LIKE it_mess OCCURS 0 WITH HEADER LINE.

*----- BAPI
DATA : BEGIN OF it_return OCCURS 0.
        INCLUDE STRUCTURE bapiret2.
DATA : END OF it_return.

DATA : BEGIN OF it_goodsmvt_matdocitem OCCURS 0.
        INCLUDE STRUCTURE bapi2017_gm_item_04.
DATA : END OF it_goodsmvt_matdocitem.

DATA : BEGIN OF st_goodsmvt_headret.
        INCLUDE STRUCTURE bapi2017_gm_head_ret.
DATA : END OF st_goodsmvt_headret.

**--- Column Invisible
DATA : w_tab TYPE cxtab_column.

**--- Constants
CONSTANTS : c_mbst LIKE sy-tcode VALUE 'MBST',
            c_bukrs LIKE t001-bukrs VALUE 'H201',
            c_mb1b LIKE sy-tcode VALUE 'MB1B'.






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
*&      Form  save_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_document.
**---
  READ TABLE it_mseg WITH KEY xselk = 'X'.

  IF sy-subrc EQ 0.
    PERFORM material_document_cancel.
  ELSE.
    MESSAGE w061(m7).
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.

*---
  MOVE : 'X' TO w_complete.
ENDFORM.                    " save_document

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
ENDMODULE.                 " transaction_init  OUTPUT

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
*          PERFORM get_material_document.
          CALL SCREEN 9100.
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
          PERFORM table_control_page_scrol USING w_save_okcode
                                                 w_top_line
                                                 w_lines
                                                 w_loopc.
          CLEAR : w_save_okcode.
      ENDCASE.
  ENDCASE.
ENDMODULE.                 " user_command_scrcom  INPUT

*&---------------------------------------------------------------------*
*&      Form  get_material_document
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_document.
**---
  DATA : l_tabix LIKE sy-tabix,
         l_index LIKE sy-index.

  DATA : l_vltyp LIKE ltap-vltyp,
         l_vlber LIKE ltap-vlber,
         l_vlpla LIKE ltap-vlpla,
         l_nltyp LIKE ltap-nltyp,
         l_nlber LIKE ltap-nlber,
         l_nlpla LIKE ltap-nlpla.

  DATA : l_posnn LIKE vbfa-posnn.

  CLEAR : it_mseg, it_mseg[], w_mseg_lines, it_ymseg, it_ymseg[].

  IF rm07m-mjahr IS INITIAL.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mseg
             FROM mseg
            WHERE mblnr EQ rm07m-mblnr
              AND xauto EQ space.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mseg
             FROM mseg
            WHERE mblnr EQ rm07m-mblnr
              AND mjahr EQ rm07m-mjahr
              AND xauto EQ space.
  ENDIF.

*---
  DESCRIBE TABLE it_mseg LINES w_mseg_lines.

*--- check Material Doc. is Stock Placement or Stock Removal
  DATA : l_abild LIKE t333-abild.

  READ TABLE it_mseg INDEX 1.

  PERFORM check_prev_matl_doc USING it_mseg-mblnr it_mseg-mjahr
                              CHANGING l_abild.

*---
  LOOP AT it_mseg.
    MOVE : sy-tabix      TO l_tabix.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_ymseg
             FROM mseg
            WHERE sjahr EQ it_mseg-mjahr
              AND smbln EQ it_mseg-mblnr
              AND smblp EQ it_mseg-zeile.
    IF sy-subrc EQ 0.
      DELETE it_mseg.
    ELSE.
      l_index = l_index + 1.
*--- get destination storage information
      PERFORM get_source_dest USING    it_mseg-tbnum it_mseg-tbpos
                              CHANGING l_vltyp l_vlber l_vlpla
                                       l_nltyp l_nlber l_nlpla.
*--- if reference Mat'l Doc. is Stock Placement,
*---   Mat'l Doc. to Cancel will be Stock Removal.
*---   The Same as Reverse...
      IF l_abild EQ '1'.    " Stock Placement (Prev. Doc. Status)
        MOVE : l_nltyp TO it_mseg-lgtyp,
               l_nlpla TO it_mseg-lgpla.
      ELSEIF l_abild EQ '2'.     " Stock Removal (Prev. Doc. Status)
        MOVE : l_vltyp TO it_mseg-lgtyp,
               l_vlpla TO it_mseg-lgpla.
      ELSEIF l_abild EQ '0'.     " if Transfer Requirement not created
        CLEAR : vbfa, l_posnn.
        MOVE : it_mseg-zeile TO l_posnn.
        PERFORM conversion_exit_alpha USING l_posnn.
        SELECT SINGLE * INTO vbfa
                        FROM vbfa
                       WHERE vbeln EQ it_mseg-mblnr
                         AND posnn EQ l_posnn
                         AND vbtyp_n EQ 'R'.
        CLEAR : ltap.
        SELECT SINGLE vltyp vlpla
                      nltyp nlpla INTO (ltap-vltyp, ltap-vlpla,
                                        ltap-nltyp, ltap-nlpla)
                                  FROM ltak AS a INNER JOIN ltap AS b
                                    ON a~mandt EQ b~mandt
                                   AND a~tanum EQ b~tanum
                                 WHERE a~vbeln EQ vbfa-vbelv
                                   AND b~posnr EQ vbfa-posnv.
        IF it_mseg-vschn NE space.
          MOVE : ltap-nltyp TO it_mseg-lgtyp,
                 ltap-nlpla TO it_mseg-lgpla.
        ELSEIF it_mseg-nschn NE space.
          MOVE : ltap-vltyp TO it_mseg-lgtyp,
                 ltap-vlpla TO it_mseg-lgpla.
        ENDIF.
      ENDIF.
*---
      MOVE : 'X'           TO it_mseg-xselk,
             it_mseg-zeile TO it_mseg-zzeile,
             l_index       TO it_mseg-zeile.
      MODIFY it_mseg INDEX l_tabix.
    ENDIF.
  ENDLOOP.

*---
  DESCRIBE TABLE it_ymseg LINES w_ymseg_lines.

*--- already all items canceled/reserved
  IF w_mseg_lines EQ w_ymseg_lines.
    MESSAGE e067(m7) WITH rm07m-mblnr.
  ENDIF.

*--- already some items cancelled/reserved
  IF ( w_mseg_lines NE w_ymseg_lines ) AND w_ymseg_lines NE 0.
    MESSAGE s020(m7) WITH w_ymseg_lines w_mseg_lines.
  ENDIF.

*--- get next movement type : ex) 201 -> 202
  CLEAR : t158, t156n, t156t, dm07m-btext.

  READ TABLE it_mseg INDEX 1.

  SELECT SINGLE * FROM t158
                 WHERE tcode EQ c_mbst.

  SELECT SINGLE * FROM t156n
                 WHERE fcode EQ t158-fcode
                   AND bwart EQ it_mseg-bwart.

  SELECT SINGLE btext INTO dm07m-btext
                      FROM t156t
                     WHERE spras EQ sy-langu
                       AND bwart EQ t156n-bwart_next.

  MOVE : t156n-bwart_next TO msegk-bwart,
         it_mseg-sobkz    TO msegk-sobkz,
         it_mseg-sakto    TO msegk-konto,
         it_mseg-kostl    TO cobl-kostl.

*---
  CLEAR : t156, t158b.

  SELECT SINGLE * FROM t158b
                 WHERE tcode EQ c_mb1b
                   AND bwart EQ msegk-bwart.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE * FROM t156
                 WHERE bwart EQ msegk-bwart.

  IF t156-xstbw EQ space.
    MOVE : text-001 TO rm07m-tuwrk,
           text-002 TO rm07m-tulgo.
  ELSE.     " if movement type is reversal
    MOVE : text-003 TO rm07m-tuwrk,
           text-004 TO rm07m-tulgo.
  ENDIF.

  MOVE : it_mseg-umwrk TO msegk-umwrk,
         it_mseg-umlgo TO msegk-umlgo.
ENDFORM.                    " get_material_document

*&---------------------------------------------------------------------*
*&      Module  beleg_suchen  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE beleg_suchen INPUT.
**--- this logic copied from standard program : SAPMM07M
**---                                           MM07MH00_BELEG_SUCHEN
  DATA: flag(1).
  flag  = 'X'.
  EXPORT flag TO MEMORY ID 'MB51_FLAG'.
*  CALL TRANSACTION 'MB51'.
  CALL TRANSACTION 'ZMME98'.
  GET PARAMETER ID 'MBN' FIELD rm07m-mblnr.
  GET PARAMETER ID 'MJA' FIELD rm07m-mjahr.
ENDMODULE.                 " beleg_suchen  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_material_doc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_material_doc INPUT.
**---
  IF rm07m-mblnr IS INITIAL.
    MESSAGE e018(m7) WITH text-m01.
  ELSE.
    CLEAR : it_mkpf, it_mkpf[].
    IF rm07m-mjahr IS INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mkpf
               FROM mkpf
              WHERE mblnr EQ rm07m-mblnr.
      IF sy-subrc NE 0.
        MESSAGE e063(m7) WITH rm07m-mblnr.
      ENDIF.
    ELSE.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE it_mkpf
               FROM mkpf
              WHERE mblnr EQ rm07m-mblnr
                AND mjahr EQ rm07m-mjahr.
      IF sy-subrc NE 0.
        MESSAGE e062(m7) WITH rm07m-mblnr rm07m-mjahr.
      ENDIF.
    ENDIF.
    SELECT SINGLE mblnr INTO mseg-mblnr
                        FROM mseg
                       WHERE sjahr EQ rm07m-mjahr
                         AND smbln EQ rm07m-mblnr
                         AND bwart EQ '102'.
    IF sy-subrc EQ 0.
      MESSAGE e999 WITH text-011 rm07m-mblnr text-m02.
    ENDIF.
  ENDIF.

*---
  PERFORM get_material_document.
ENDMODULE.                 " check_material_doc  INPUT

*&---------------------------------------------------------------------*
*&      Module  check_input_scr9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_input_scr9100 INPUT.
**---
  w_index = w_top_line + sy-stepl - 1.

  READ TABLE it_mseg INDEX w_index.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING mseg TO it_mseg.
    MOVE : rm07m-xselk      TO it_mseg-xselk.
    MODIFY it_mseg INDEX w_index.
  ENDIF.

*---
ENDMODULE.                 " check_input_scr9100  INPUT

*&---------------------------------------------------------------------*
*&      Form  select_deselect_all
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0358   text
*----------------------------------------------------------------------*
FORM select_deselect_all USING    p_value.
**---
  MOVE : p_value TO it_mseg-xselk.

  MODIFY it_mseg TRANSPORTING xselk WHERE mblnr GE space.
ENDFORM.                    " select_deselect_all

*&---------------------------------------------------------------------*
*&      Form  material_document_cancel
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM material_document_cancel.
**---
  DATA : l_mjahr LIKE mkpf-mjahr,
         l_budat(10),
         l_lines TYPE i.

***--- BAPI Logic
*  IF st_goodsmvt_headret-mat_doc EQ space AND w_tanum EQ space.
*
*    CLEAR : it_return, it_return[].
*
*    LOOP AT it_mseg WHERE xselk NE space.
*      MOVE : it_mseg-zzeile TO it_goodsmvt_matdocitem-matdoc_item.
*      APPEND it_goodsmvt_matdocitem.
*    ENDLOOP.
*
*    IF rm07m-mjahr IS INITIAL.
*      MOVE : sy-datum(4) TO l_mjahr.
*    ELSE.
*      MOVE : rm07m-mjahr TO l_mjahr.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*      EXPORTING
*        materialdocument          = rm07m-mblnr
*        matdocumentyear           = l_mjahr
*        goodsmvt_pstng_date       = w_budat
**     GOODSMVT_PR_UNAME         =
*      IMPORTING
*        goodsmvt_headret          = st_goodsmvt_headret
*      TABLES
*        return                    = it_return
*        goodsmvt_matdocitem       = it_goodsmvt_matdocitem.
*
*  ENDIF.
***--- BAPI Logic

**--- BDC Logic
  IF w_mbst_mblnr EQ space AND w_tanum EQ space.
    CLEAR : it_bdc, it_bdc[], it_mess, it_mess[],
            w_mbst_subrc, w_mbst_mblnr, l_budat.

    IF rm07m-mjahr IS INITIAL.
      MOVE : sy-datum(4) TO l_mjahr.
    ELSE.
      MOVE : rm07m-mjahr TO l_mjahr.
    ENDIF.

    WRITE : w_budat TO l_budat.

*---
    PERFORM dynpro USING : 'X'  'SAPMM07M'        '0460',
                           ' '  'MKPF-BUDAT'      l_budat,
                           ' '  'RM07M-MBLNR'     rm07m-mblnr,
                           ' '  'RM07M-MJAHR'     l_mjahr.

    IF NOT rm07m-grund IS INITIAL.
      PERFORM dynpro USING : ' '  'RM07M-GRUND'     rm07m-grund.
    ENDIF.

    PERFORM dynpro USING : ' '  'BDC_OKCODE'      '/00'.

*--- deselect all
    PERFORM dynpro USING : 'X'  'SAPMM07M'        '0221',
                           ' '  'BDC_OKCODE'      '=SELN'.

*---
    DESCRIBE TABLE it_mseg LINES l_lines.

*---
    LOOP AT it_mseg WHERE xselk NE space.
      PERFORM dynpro USING : 'X'  'SAPMM07M'        '0221',
                             ' '  'BDC_OKCODE'      '=KLA'.
      IF l_lines EQ 1.
      ELSE.
        PERFORM dynpro USING : 'X'  'SAPMM07M'        '1501',
                               ' '  'RM07M-ZEILE'     it_mseg-zeile,
                               ' '  'BDC_OKCODE'      '=OK'.
      ENDIF.
      PERFORM dynpro USING : 'X'  'SAPMM07M'        '0221',
                             ' '  'RM07M-XSELK(01)' 'X'.
    ENDLOOP.

*---
    PERFORM dynpro USING : 'X'  'SAPMM07M'        '0221',
                           ' '  'BDC_OKCODE'      '=BU'.

    CALL TRANSACTION 'MBST' USING it_bdc
                            MODE w_mode
                            UPDATE 'S'
                            MESSAGES INTO it_mess.

    MOVE : sy-subrc TO w_mbst_subrc.
  ENDIF.
**--- BDC Logic

**---
  READ TABLE it_mess WITH KEY msgid = 'M7'
                              msgnr = '060'.

  IF w_mbst_subrc EQ 0 AND sy-msgty EQ 'S' AND sy-subrc EQ 0.
    MOVE : it_mess-msgv1 TO w_mbst_mblnr.
*  IF NOT st_goodsmvt_headret IS INITIAL AND it_return[] IS INITIAL.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*         EXPORTING
*              wait = 'X'.
    PERFORM check_transfer_order_condition USING w_mbst_mblnr l_mjahr
                                                 w_subrc.
    IF w_subrc EQ 0.     " if mat'l doc. related with transfer order
      CASE t333-abild.
        WHEN '1'.     " Stock Placement
          PERFORM transfer_order_posting_to_wm USING l_mjahr.
        WHEN '2'.     " Stock Removal
          PERFORM transfer_order_posting_from_wm USING l_mjahr.
      ENDCASE.
      IF sy-subrc EQ 0 AND sy-msgty EQ 'S'.
        READ TABLE it_mess WITH KEY msgid = 'L3'
                                    msgnr = '016'.
        IF sy-subrc EQ 0.
          CLEAR : w_tanum.
          MOVE : it_mess-msgv1 TO w_tanum.
*          MESSAGE s021 WITH st_goodsmvt_headret-mat_doc w_tanum.
          MESSAGE s021 WITH w_mbst_mblnr w_tanum.
          LEAVE TO TRANSACTION sy-tcode.
        ENDIF.
      ELSE.
        PERFORM display_bdc_error_message USING w_text.
        CONCATENATE text-011 w_mbst_mblnr text-012 '('
                             w_text ')' INTO w_text SEPARATED BY space.
        MESSAGE e999 WITH w_text.
      ENDIF.
    ELSE.                " if mat'l doc. not related with transfer order
*      MESSAGE s060(m7) WITH st_goodsmvt_headret-mat_doc.
      MESSAGE s060(m7) WITH w_mbst_mblnr.
      LEAVE TO TRANSACTION sy-tcode.
    ENDIF.
  ELSE.
    APPEND LINES OF it_mess TO it_message.
    PERFORM display_bdc_error_message USING w_text.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    PERFORM display_bapi_error_message.
  ENDIF.
ENDFORM.                    " material_document_cancel

*&---------------------------------------------------------------------*
*&      Module  display_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_scr9100 OUTPUT.
**---
  w_index = w_top_line + sy-stepl - 1.

  READ TABLE it_mseg INDEX w_index.

  IF sy-subrc EQ 0.
    MOVE-CORRESPONDING it_mseg TO mseg.
    MOVE : it_mseg-xselk       TO rm07m-xselk.
    PERFORM get_material_desc USING mseg-matnr.
*    MOVE : it_mseg-nltyp       TO mseg-lgtyp,
**           it_mseg-nlber       TO mseg-vlber,
*           it_mseg-nlpla       TO mseg-lgpla.
  ELSE.
    EXIT FROM STEP-LOOP.
  ENDIF.

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
  DESCRIBE TABLE it_mseg LINES w_lines.

  MOVE : w_lines     TO rm07m-posnm,
         w_top_line  TO rm07m-posnr.
ENDMODULE.                 " display_count_scr9100  OUTPUT

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
    IF st_goodsmvt_headret-mat_doc NE space AND w_tanum EQ space.
      IF screen-name EQ 'MSEG-LGTYP' OR screen-name EQ 'MSEG-LGPLA'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " control_screen_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  field_selection_scr9100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE field_selection_scr9100 OUTPUT.
*--- get cost center description
  CLEAR : w_text_kostl, wa_cobl.

  READ TABLE it_mkpf INDEX 1.

  WRITE : it_mkpf-budat TO wa_cobl-budat,
          it_mkpf-bldat TO wa_cobl-bldat,
          c_bukrs    TO wa_cobl-bukrs.
*          cobl-kostl TO wa_cobl-kostl.

  PERFORM conversion_exit_alpha USING cobl-kostl.

  MOVE : cobl-kostl TO wa_cobl-kostl.

  CALL FUNCTION 'COBL_READ_MASTER_DATA_TEXT'
       EXPORTING
            field_name  = 'KOSTL'
            i_cobl      = wa_cobl
       IMPORTING
            description = w_text_kostl.

*--- if transfer posing, receiving/issuing plant/SLoc. display
  SELECT SINGLE * FROM t158b
                 WHERE tcode EQ c_mb1b
                   AND bwart EQ msegk-bwart.

  IF sy-subrc EQ 0.
    LOOP AT SCREEN.
      CHECK screen-group1 EQ '057' OR screen-group1 EQ '058'.
      screen-invisible = 0.
      screen-active = 1.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " field_selection_scr9100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  check_date  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_date INPUT.
*---
  IF mkpf-budat IS INITIAL.
    MESSAGE e669(f5).
  ELSE.
    MOVE : mkpf-budat TO w_budat.
  ENDIF.
ENDMODULE.                 " check_date  INPUT

*&---------------------------------------------------------------------*
*&      Form  check_transfer_order_condition
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM check_transfer_order_condition USING p_mblnr p_mjahr p_subrc.
*---
  CLEAR : ltbk.

  SELECT SINGLE * FROM ltbk
                 WHERE mblnr EQ p_mblnr
                   AND mjahr EQ p_mjahr.

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
FORM transfer_order_posting_to_wm USING p_mjahr.
*---
  DATA : BEGIN OF it_mseg_copy OCCURS 0.
          INCLUDE STRUCTURE it_mseg.
  DATA : END OF it_mseg_copy.

  DATA : l_vltyp LIKE ltap-vltyp,
         l_vlber LIKE ltap-vlber,
         l_vlpla LIKE ltap-vlpla,
         l_nltyp LIKE ltap-nltyp,
         l_nlber LIKE ltap-nlber,
         l_nlpla LIKE ltap-nlpla.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], it_mseg_copy,
          it_mseg_copy[].

*--- except which not related with WMS and not selected
  LOOP AT it_mseg WHERE xselk NE space     " selected
                    AND xblvs NE space.    " ind. WMS posting
    MOVE-CORRESPONDING it_mseg TO it_mseg_copy.
    APPEND it_mseg_copy.
    CLEAR : it_mseg_copy.
  ENDLOOP.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
*                   ' '  'RL02B-MBLNR'     st_goodsmvt_headret-mat_doc,
*                   ' '  'RL02B-MJAHR'     st_goodsmvt_headret-doc_year,
                         ' '  'RL02B-MBLNR'     w_mbst_mblnr,
                         ' '  'RL02B-MJAHR'     p_mjahr,
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=MRKA'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=TPAL'.

*---
  LOOP AT it_mseg_copy.
*    PERFORM get_source_dest USING it_mseg_copy-tbnum it_mseg_copy-tbpos
*                            CHANGING l_vltyp l_vlber l_vlpla
*                                     l_nltyp l_nlber l_nlpla.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0104',
                           ' '  'RL03T-LETY2'     'BB',
*                           ' '  'RL03T-LGTY2'     l_nltyp,
                           ' '  'RL03T-LGTY2'     it_mseg_copy-lgtyp,
                           ' '  'BDC_OKCODE'      '/00'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0104',
                           ' '  'BDC_OKCODE'      '=TAH1'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0102',
*                           ' '  'LTAP-NLTYP'      l_nltyp,
*                           ' '  'LTAP-NLPLA'      l_nlpla,
                           ' '  'LTAP-NLTYP'      it_mseg_copy-lgtyp,
                           ' '  'LTAP-NLPLA'      it_mseg_copy-lgpla,
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
FORM transfer_order_posting_from_wm USING p_mjahr.
*---
  DATA : BEGIN OF it_mseg_copy OCCURS 0.
          INCLUDE STRUCTURE it_mseg.
  DATA : END OF it_mseg_copy.

  DATA : l_vltyp LIKE ltap-vltyp,
         l_vlber LIKE ltap-vlber,
         l_vlpla LIKE ltap-vlpla,
         l_nltyp LIKE ltap-nltyp,
         l_nlber LIKE ltap-nlber,
         l_nlpla LIKE ltap-nlpla.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], it_mseg_copy,
          it_mseg_copy[].

*--- except which not related with WMS and not selected
  LOOP AT it_mseg WHERE xselk NE space     " selected
                    AND xblvs NE space.    " ind. WMS posting
    MOVE-CORRESPONDING it_mseg TO it_mseg_copy.
    APPEND it_mseg_copy.
    CLEAR : it_mseg_copy.
  ENDLOOP.

*---
  PERFORM dynpro USING : 'X'  'SAPML02B'        '0203',
*                   ' '  'RL02B-MBLNR'     st_goodsmvt_headret-mat_doc,
*                   ' '  'RL02B-MJAHR'     st_goodsmvt_headret-doc_year,
                         ' '  'RL02B-MBLNR'     w_mbst_mblnr,
                         ' '  'RL02B-MJAHR'     p_mjahr,
                         ' '  'RL02B-DUNKL'     'H',
                         ' '  'BDC_OKCODE'      '/00'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=MRKA'.

  PERFORM dynpro USING : 'X'  'SAPML03T'        '0132',
                         ' '  'BDC_SUBSCR'      'SAPML03T',
                         ' '  'BDC_OKCODE'      '=TTYP'.

*---
  LOOP AT it_mseg_copy.
*    PERFORM get_source_dest USING it_mseg_copy-tbnum it_mseg_copy-tbpos
*                            CHANGING l_vltyp l_vlber l_vlpla
*                                     l_nltyp l_nlber l_nlpla.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0105',
                           ' '  'BDC_OKCODE'      '=TAH2'.
    PERFORM dynpro USING : 'X'  'SAPML03T'        '0102',
**                           ' '  'LTAP-VLTYP'      l_vltyp,
**                           ' '  'LTAP-VLPLA'      l_vlpla,
*                           ' '  'LTAP-VLTYP'      l_nltyp,
*                           ' '  'LTAP-VLPLA'      l_nlpla,
                           ' '  'LTAP-VLTYP'      it_mseg_copy-lgtyp,
                           ' '  'LTAP-VLPLA'      it_mseg_copy-lgpla,
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
FORM display_bdc_error_message USING p_text.
*---

  READ TABLE it_mess WITH KEY msgtyp = 'E'.

  IF sy-subrc EQ 0.
    CLEAR : w_text.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
         EXPORTING
              msgid               = it_mess-msgid
              msgnr               = it_mess-msgnr
              msgv1               = it_mess-msgv1
              msgv2               = it_mess-msgv2
              msgv3               = it_mess-msgv3
              msgv4               = it_mess-msgv4
         IMPORTING
              message_text_output = w_text.
  ENDIF.

*  CONCATENATE text-011 st_goodsmvt_headret-mat_doc text-012 '('
*                       l_text ')' INTO l_text SEPARATED BY space.
ENDFORM.                    " display_bdc_error_message

*&---------------------------------------------------------------------*
*&      Form  get_source_dest
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MSEG_COPY_TBNUM  text
*      -->P_IT_MSEG_COPY_TBPOS  text
*      <--P_L_VLTYP  text
*      <--P_L_VLBER  text
*      <--P_L_VLPLA  text
*      <--P_L_NLTYP  text
*      <--P_L_NLBER  text
*      <--P_L_NLPLA  text
*----------------------------------------------------------------------*
FORM get_source_dest USING    p_mseg_copy_tbnum
                              p_mseg_copy_tbpos
                     CHANGING p_vltyp
                              p_vlber
                              p_vlpla
                              p_nltyp
                              p_nlber
                              p_nlpla.
*---
  SELECT SINGLE vltyp vlber vlpla     " source
                nltyp nlber nlpla     " destination
                      INTO (p_vltyp, p_vlber, p_vlpla,
                            p_nltyp, p_nlber, p_nlpla)
                      FROM ltak AS a INNER JOIN ltap AS b
                        ON a~mandt EQ b~mandt
                       AND a~tanum EQ b~tanum
                     WHERE tbnum EQ p_mseg_copy_tbnum
                       AND tbpos EQ p_mseg_copy_tbpos.
ENDFORM.                    " get_source_dest

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1397   text
*      -->P_1398   text
*      -->P_1399   text
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
*&      Form  check_prev_matl_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_MSEG_MBLNR  text
*      -->P_IT_MSEG_MJAHR  text
*      <--P_L_ABILD  text
*----------------------------------------------------------------------*
FORM check_prev_matl_doc USING    p_it_mseg_mblnr
                                  p_it_mseg_mjahr
                         CHANGING p_l_abild.
*---
  CLEAR : ltbk.

  SELECT SINGLE * FROM ltbk
                 WHERE mblnr EQ p_it_mseg_mblnr
                   AND mjahr EQ p_it_mseg_mjahr.

  IF sy-subrc EQ 0.
    CLEAR : t333.

    SELECT SINGLE abild INTO p_l_abild
                        FROM t333
                       WHERE lgnum EQ ltbk-lgnum
                         AND bwlvs EQ ltbk-bwlvs.
*
*    move : sy-subrc to p_subrc.
  ELSE.
*    move : sy-subrc to p_subrc.
    MOVE : '0' TO p_l_abild.
  ENDIF.
ENDFORM.                    " check_prev_matl_doc
