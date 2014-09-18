************************************************************************
* Program Name      : ZEMMPM30E_INFO_RECORD
* Author            : Sung-Tae, Lim
* Creation Date     : 2004.02.26.
* Specifications By : Sung-Tae, Lim
* Pattern           :
* Development Request No : UD1K907463
* Addl Documentation:
* Description       : Info. Record
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2004.02.23.     Sung-Tae Lim     UD1K907463     Initial Coding
* 5/4/2012        t-code is deleted by APM Monitoring
************************************************************************

REPORT zemmpm30e_info_record NO STANDARD PAGE HEADING
                             LINE-SIZE 255
                             LINE-COUNT 64(1)
                             MESSAGE-ID zmmm.


**---
INCLUDE : zrmmpmxxr_incl.


**---
DATA : BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF it_bdc.

DATA : BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA : END OF it_mess.

DATA : it_ztmm_cond LIKE ztmm_cond OCCURS 0 WITH HEADER LINE.

**---
DATA : w_mode LIKE ctu_params-dismode VALUE 'A',
       w_kschl01 LIKE ztmm_cond-menge,
       w_kschl02 LIKE ztmm_cond-menge,
       w_kschl03 LIKE ztmm_cond-menge,
       w_meins01 LIKE mara-meins,
       w_meins02 LIKE mara-meins,
       w_meins03 LIKE mara-meins.

DATA : w_lifnr LIKE lfa1-lifnr,
       w_matnr LIKE mara-matnr,
       w_ekorg LIKE t024e-ekorg,
       w_infnr LIKE eina-infnr.

DATA : w_subrc LIKE sy-subrc,
       w_okcode LIKE sy-ucomm,
       w_save_okcode LIKE sy-ucomm.

**---
CONSTANTS : c_kschl01 LIKE konp-kschl VALUE 'ZP01',
            c_kschl02 LIKE konp-kschl VALUE 'ZP02',
            c_kschl03 LIKE konp-kschl VALUE 'ZP03'.

**---
START-OF-SELECTION.
  PERFORM call_info_record.
  PERFORM call_cbo_screen.






*&---------------------------------------------------------------------*
*&      Form  call_info_record
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_info_record.
**---
  DATA : l_tcode LIKE sy-tcode.

  CLEAR : it_bdc, it_bdc[], it_mess, it_mess[], l_tcode.

  PERFORM dynpro USING : 'X'  'SAPMM06I'        '0100'.

  CASE sy-tcode.
    WHEN 'ZME11'.
      MOVE : 'ME11' TO l_tcode.
    WHEN 'ZME12'.
      MOVE : 'ME12' TO l_tcode.
    WHEN 'ZME13'.
      MOVE : 'ME13' TO l_tcode.
  ENDCASE.

*---
  CALL TRANSACTION l_tcode USING it_bdc
                           MODE w_mode
                           UPDATE 'S'
                           MESSAGES INTO it_mess.
ENDFORM.                    " call_info_record

*&---------------------------------------------------------------------*
*&      Form  call_cbo_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_cbo_screen.
**---
  CASE sy-tcode.
    WHEN 'ZME11'.
      PERFORM setting_create.
    WHEN 'ZME12'.
      PERFORM setting_change.
    WHEN 'ZME13'.
      PERFORM setting_display.
  ENDCASE.

**---
  CHECK w_subrc EQ 0.

  CALL SCREEN 9000 STARTING AT  25  4
                   ENDING   AT 100 13.
ENDFORM.                    " call_cbo_screen

*&---------------------------------------------------------------------*
*&      Module  status_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
**---
  IF sy-tcode EQ 'ZME13'.
    SET PF-STATUS '9000' EXCLUDING 'SAVE'.
  ELSE.
    SET PF-STATUS '9000'.
  ENDIF.

  SET TITLEBAR  '9000'.
ENDMODULE.                 " status_9000  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
**---
  CASE sy-ucomm.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Form  dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0059   text
*      -->P_0060   text
*      -->P_0061   text
*----------------------------------------------------------------------*
FORM dynpro USING    dynbegin
                     name
                     value.
**---
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
*&      Form  setting_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_create.
*---
  READ TABLE it_mess WITH KEY msgid = '06'
                              msgnr = '312'.

  IF sy-subrc EQ 0.
    MOVE : '4' TO w_subrc.
  ELSE.
    PERFORM setting_display.
  ENDIF.
ENDFORM.                    " setting_create

*&---------------------------------------------------------------------*
*&      Form  setting_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_change.
*---
  PERFORM setting_display.
ENDFORM.                    " setting_change

*&---------------------------------------------------------------------*
*&      Form  setting_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM setting_display.
*---
  PERFORM get_parameter_ids.

  IF NOT w_infnr IS INITIAL.
    CLEAR : v_eina.
    SELECT SINGLE matnr
                  lifnr
                  ekorg INTO (w_matnr, w_lifnr, w_ekorg)
                        FROM v_eina
                       WHERE infnr EQ w_infnr.
    MOVE : sy-subrc TO w_subrc.
    CHECK w_subrc EQ 0.
    PERFORM get_ztmm_cond USING w_matnr w_lifnr w_ekorg.
  ELSE.
    CLEAR : v_eina.
    SELECT SINGLE * INTO v_eina
                    FROM v_eina
                   WHERE matnr EQ w_matnr
                     AND lifnr EQ w_lifnr
                     AND ekorg EQ w_ekorg
                     AND werks EQ space.
    MOVE : sy-subrc TO w_subrc.
    CHECK w_subrc EQ 0.
    PERFORM get_ztmm_cond USING v_eina-matnr v_eina-lifnr v_eina-ekorg.
  ENDIF.
ENDFORM.                    " setting_display

*&---------------------------------------------------------------------*
*&      Form  get_parameter_ids
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_parameter_ids.
*---
  CLEAR : w_lifnr, w_matnr, w_ekorg, w_infnr.

  GET PARAMETER ID 'LIF' FIELD w_lifnr.
  GET PARAMETER ID 'MAT' FIELD w_matnr.
  GET PARAMETER ID 'EKO' FIELD w_ekorg.
  GET PARAMETER ID 'INF' FIELD w_infnr.
ENDFORM.                    " get_parameter_ids

*&---------------------------------------------------------------------*
*&      Module  display_description  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_description OUTPUT.
*---
  PERFORM get_material_desc USING ztmm_cond-matnr.
  PERFORM get_vendor_desc USING ztmm_cond-lifnr.
  PERFORM get_pur_org_desc USING ztmm_cond-ekorg.
ENDMODULE.                 " display_description  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  get_ztmm_cond
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_MATNR  text
*      -->P_W_LIFNR  text
*      -->P_W_EKORG  text
*----------------------------------------------------------------------*
FORM get_ztmm_cond USING    p_w_matnr
                            p_w_lifnr
                            p_w_ekorg.
*---
  CLEAR : ztmm_cond, it_ztmm_cond, it_ztmm_cond[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztmm_cond
           FROM ztmm_cond
          WHERE matnr EQ p_w_matnr
            AND lifnr EQ p_w_lifnr
            AND ekorg EQ p_w_ekorg.

  READ TABLE it_ztmm_cond INDEX 1.

  MOVE : it_ztmm_cond-matnr TO ztmm_cond-matnr,
         it_ztmm_cond-lifnr TO ztmm_cond-lifnr,
         it_ztmm_cond-ekorg TO ztmm_cond-ekorg.

*---
  IF it_ztmm_cond[] IS INITIAL.
    MOVE : p_w_matnr TO ztmm_cond-matnr,
           p_w_lifnr TO ztmm_cond-lifnr,
           p_w_ekorg TO ztmm_cond-ekorg.
    CLEAR : mara.
    SELECT SINGLE meins INTO mara-meins
                        FROM mara
                       WHERE matnr EQ p_w_matnr.
  ENDIF.

*--- Packaging Amortization Cost
  READ TABLE it_ztmm_cond WITH KEY kschl = c_kschl01.
  IF sy-subrc EQ 0.
    MOVE : it_ztmm_cond-menge TO w_kschl01,
           it_ztmm_cond-meins TO w_meins01.
  ELSE.
    MOVE : mara-meins TO w_meins01.
  ENDIF.

*--- Tooling Amortization Cost
  READ TABLE it_ztmm_cond WITH KEY kschl = c_kschl02.
  IF sy-subrc EQ 0.
    MOVE : it_ztmm_cond-menge TO w_kschl02,
           it_ztmm_cond-meins TO w_meins02.
  ELSE.
    MOVE : mara-meins TO w_meins02.
  ENDIF.

*--- Development Amortization Cost
  READ TABLE it_ztmm_cond WITH KEY kschl = c_kschl03.
  IF sy-subrc EQ 0.
    MOVE : it_ztmm_cond-menge TO w_kschl03,
           it_ztmm_cond-meins TO w_meins03.
  ELSE.
    MOVE : mara-meins TO w_meins03.
  ENDIF.
ENDFORM.                    " get_ztmm_cond

*&---------------------------------------------------------------------*
*&      Module  screen_control  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_control OUTPUT.
*---
  IF sy-tcode EQ 'ZME13'.
    LOOP AT SCREEN.
      screen-input = 0.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.
ENDMODULE.                 " screen_control  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
*---
  MOVE : w_okcode TO w_save_okcode.

  CLEAR : w_okcode.

  CASE w_save_okcode.
    WHEN 'SAVE'.
      CASE sy-tcode.
        WHEN 'ZME11'.
          PERFORM save_create.
          MODIFY ztmm_cond FROM TABLE it_ztmm_cond.
          MESSAGE s999 WITH text-m01.
        WHEN 'ZME12'.
          IF it_ztmm_cond[] IS INITIAL.
            PERFORM save_create.
          ELSE.
            PERFORM save_change.
          ENDIF.
          MODIFY ztmm_cond FROM TABLE it_ztmm_cond.
          MESSAGE s999 WITH text-m01.
      ENDCASE.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  save_create
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_create.
*---
  MOVE : ztmm_cond-matnr TO it_ztmm_cond-matnr,
         ztmm_cond-lifnr TO it_ztmm_cond-lifnr,
         ztmm_cond-ekorg TO it_ztmm_cond-ekorg.
  MOVE : sy-datum        TO it_ztmm_cond-erdat,
         sy-uzeit        TO it_ztmm_cond-erzet,
         sy-uname        TO it_ztmm_cond-ernam.
  MOVE : sy-datum        TO it_ztmm_cond-aedat,
         sy-uzeit        TO it_ztmm_cond-aezet,
         sy-uname        TO it_ztmm_cond-aenam.
  MOVE : c_kschl01 TO it_ztmm_cond-kschl,
         w_kschl01 TO it_ztmm_cond-menge,
         w_meins01 TO it_ztmm_cond-meins.
  APPEND it_ztmm_cond.
  MOVE : c_kschl02 TO it_ztmm_cond-kschl,
         w_kschl02 TO it_ztmm_cond-menge,
         w_meins02 TO it_ztmm_cond-meins.
  APPEND it_ztmm_cond.
  MOVE : c_kschl03 TO it_ztmm_cond-kschl,
         w_kschl03 TO it_ztmm_cond-menge,
         w_meins03 TO it_ztmm_cond-meins.
  APPEND it_ztmm_cond.
ENDFORM.                    " save_create

*&---------------------------------------------------------------------*
*&      Form  save_change
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_change.
*---
  LOOP AT it_ztmm_cond.
    CASE it_ztmm_cond-kschl.
      WHEN c_kschl01.
        MOVE : w_kschl01 TO it_ztmm_cond-menge.
      WHEN c_kschl02.
        MOVE : w_kschl02 TO it_ztmm_cond-menge.
      WHEN c_kschl03.
        MOVE : w_kschl03 TO it_ztmm_cond-menge.
    ENDCASE.
    MOVE : sy-datum      TO it_ztmm_cond-aedat,
           sy-uzeit      TO it_ztmm_cond-aezet,
           sy-uname      TO it_ztmm_cond-aenam.
    MODIFY it_ztmm_cond INDEX sy-tabix.
  ENDLOOP.
ENDFORM.                    " save_change
