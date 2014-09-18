REPORT zhr_vac_payment_upload MESSAGE-ID zmhr
     NO STANDARD PAGE HEADING LINE-SIZE 105 .

* Program Name      : ZHR_VAC_PAYMENT_UPLOAD
* Author            : Yongping
* Creation Date     : 2005.01.15.
* Specifications By : Naveen
* Pattern           :
* Development Request No :UD1K913853
* Addl Documentation:
* Description       : Upload the retaining vacation payment
*
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
*****************************************************************
*GLOBAL DATA
*****************************************************************

DATA: g_message(60).
DATA: s_no_exist.
DATA: g_date(10)    TYPE c.
DATA: s_error.
DATA: i_processed   TYPE i.
CONTROLS: tc1       TYPE TABLEVIEW USING SCREEN 100.
DATA: ok_code       LIKE sy-ucomm,
      save_code     LIKE sy-ucomm.

****************************************************************
*INTERNAL TABLES
****************************************************************
DATA: BEGIN OF it_person OCCURS 0,
       pernr   LIKE pa0002-pernr,
       cname   LIKE pa0002-cname,
       amount  LIKE q0015-betrg,
       begda   LIKE p0015-begda,
       updat(7) TYPE c,
       messg(80) TYPE c,
      END OF it_person.
DATA: intern TYPE alsmex_tabline OCCURS 0 WITH HEADER LINE.

*FOR BDC
DATA: BEGIN OF bdcdata OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

DATA: BEGIN OF it_message OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_message.
DATA: BEGIN OF it_mess OCCURS 0,
        msgty LIKE sy-msgty,
        msgtx(120) TYPE c,
      END OF it_mess.


*****************************************************************
*END OF  DATA DECLARATION
*****************************************************************


*****************************************************************
*SELECTION-SCREEN
*****************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-100.
PARAMETERS:
 p_file  LIKE rlgrap-filename DEFAULT 'C:\      .XLS' OBLIGATORY,
 p_filety LIKE rlgrap-filetype DEFAULT 'DAT' NO-DISPLAY.
SELECTION-SCREEN SKIP.
PARAMETERS: p_date LIKE sy-datum OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS: P_DES  LIKE P0015-ZUORD.
SELECTION-SCREEN SKIP.


SELECTION-SCREEN END OF BLOCK b2.

**********************************************************************
*END OF SELECTION SCREEN
**********************************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM at_sel_screen_on_value_request USING p_file .

AT SELECTION-SCREEN.
* CONVERT THE DATE FOR EXTERNAL DATE FORMAT
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = p_date
       IMPORTING
            date_external            = g_date
       EXCEPTIONS
            date_internal_is_invalid = 1.


START-OF-SELECTION.
  PERFORM read_person_pmnt.
  PERFORM display_data.
*  IF p_test = 'X'.
*    PERFORM update_bdc.
*  ENDIF.

END-OF-SELECTION.
*  PERFORM write_output.



*&---------------------------------------------------------------------*
*&      FORM read_person_PMNT
*&---------------------------------------------------------------------*
*       Read the persons specified
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_person_pmnt.
  PERFORM upload_file.
  PERFORM get_pay_date.
ENDFORM.                    " read_person_pmnt
*&---------------------------------------------------------------------*
*&      Form  UPDATE_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_bdc.
  DATA: i_total TYPE i.
  DATA: i_percent TYPE i.
  DATA: answer.

  IF s_no_exist = 'X'.
    EXIT.
  ENDIF.
* CHECK IF THE DATA HAS BEEN SAVED
* READ OF ALL OR SOME DATA HAVE BEEN SAVED.
  READ TABLE it_person WITH KEY updat = 'SUCCESS'.
  IF sy-subrc EQ 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
     EXPORTING
      titel = 'make selection ;;'(b01)
      diagnosetext1 = 'ALL OR SOME DATA HAS BEEN SAVED. ;;->B03'(b02)
      diagnosetext2 = 'ONLY UNSAVED DATA CAN BE SAVED AGAIN. '(b03)
*      DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
      textline1 = 'ARE YOU SURE YOU WANT TO SAVE ?? ;;->B06'(b05)
*      TEXTLINE2 = 'new planning alternative? ;;'(B06)
     IMPORTING answer = answer.

    IF answer NE 'J'.
      EXIT.
    ENDIF.
  ELSE.
    CLEAR answer.

*   CONFIRM UPDATE
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
     EXPORTING
      titel = 'make selection ;;'(b01)
      diagnosetext1 = 'The data will be save to database. ;;->B03'(b02)
*      DIAGNOSETEXT2 = 'alternative for the specified ;;->B04'(B03)
*      DIAGNOSETEXT3 = 'selection criteria. ;;'(B04)
      textline1 = 'Are you sure you want to save?? ;;->B06'(b05)
*      TEXTLINE2 = 'new planning alternative? ;;'(B06)
     IMPORTING answer = answer.

    IF answer NE 'J'.
      EXIT.
    ENDIF.
  ENDIF.
*
* UPDATE THE DATABASE
  DESCRIBE TABLE it_person LINES i_total.
  IF i_total NE 0.
    i_processed = 0.
    LOOP AT it_person.
      CLEAR: bdcdata, bdcdata[].
*     CHECK IF THIS RECORD HAS BEEN SAVED.
      IF it_person-updat = 'SUCCESS'.
        CONTINUE.
      ENDIF.
      PERFORM do_bdc USING it_person.
      i_processed = i_processed + 1.
      i_percent = ( i_processed * 100 ) / i_total .
      PERFORM progress_indicator USING i_percent.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " UPDATE_BDC
*&---------------------------------------------------------------------*
*&      Form  DO_BDC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM do_bdc USING p_person LIKE it_person.
  DATA: l_update TYPE c.
  PERFORM fill_bdcdata USING p_person .
  PERFORM call_transaction USING 'PA30' l_update.
  IF l_update = 'S'.
    p_person-updat = 'SUCCESS'.
    MODIFY it_person FROM p_person TRANSPORTING updat messg.
  ELSE.
    p_person-updat = 'FAIL'.
    MODIFY it_person FROM p_person TRANSPORTING updat messg.
  ENDIF.

ENDFORM.                    " DO_BDC
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0194   text
*----------------------------------------------------------------------*
FORM call_transaction USING p_tcode p_update.
  DATA: l_msgstr(100) TYPE c.

  CALL TRANSACTION p_tcode
           USING bdcdata
           MODE 'N'
           UPDATE 'S'
           MESSAGES INTO it_message.
* ckeck the massge
  IF sy-subrc = 0.
    p_update = 'S'.
    CLEAR: it_person-messg.
  ELSE.
    p_update = 'F'.

    PERFORM rkc_msg_string USING l_msgstr.

    it_mess-msgty = sy-msgty.
    it_mess-msgtx = l_msgstr.
    APPEND it_mess.
    it_person-messg = it_mess-msgtx.
  ENDIF.

ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  FILL_BDCDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_bdcdata USING p_person LIKE it_person.
  DATA: l_amount(11) TYPE c.

  WRITE it_person-amount TO l_amount .

  PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RP50G-PERNR'
                                it_person-pernr.
*perform bdc_field       using 'RP50G-TIMR6'
*                              'X'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RP50G-CHOIC'.
  PERFORM bdc_field       USING 'RP50G-CHOIC'
                                '0015'.
  PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=INS'.
  PERFORM bdc_field       USING 'RP50G-PERNR'
                                it_person-pernr.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RP50G-ENDDA'.
  PERFORM bdc_field       USING 'RP50G-TIMR6'
                                'X'.
  PERFORM bdc_field       USING 'RP50G-BEGDA'
                                g_date.
  PERFORM bdc_field       USING 'RP50G-ENDDA'
                                g_date.
  PERFORM bdc_field       USING 'RP50G-CHOIC'
                                '0015'.
  PERFORM bdc_field       USING 'RP50G-SUBTY'
                                '1295'.
  PERFORM bdc_dynpro      USING 'MP001500' '2000'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                              'Q0015-BETRG'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPD'.
  PERFORM bdc_field       USING 'P0015-LGART'
                                '1295'.
  PERFORM bdc_field       USING 'Q0015-BETRG'
                                l_amount.
  PERFORM bdc_field       USING 'P0015-WAERS'
                                'USD'.
  PERFORM bdc_field       USING 'P0015-BEGDA'
                                g_date.
  PERFORM bdc_field       USING 'P0015-ZUORD'
                                P_DES.



  PERFORM bdc_dynpro      USING 'MP001500' '2000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.

ENDFORM.                    " FILL_BDCDATA

*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
*       SERACH THE MESSAGE OF BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM rkc_msg_string CHANGING p_msg.
  DATA: lw_msg LIKE cfgnl-msglin.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            id      = sy-msgid
            mtype   = sy-msgty
            number  = sy-msgno
            par1    = sy-msgv1
            par2    = sy-msgv2
            par3    = sy-msgv3
            par4    = sy-msgv4
       IMPORTING
            msg_lin = lw_msg
       EXCEPTIONS
            OTHERS  = 1.
*  CONCATENATE 'Update failed for' it_person-pernr
*    INTO lw_msg SEPARATED BY space.
  CONCATENATE it_person-pernr lw_msg INTO lw_msg
     SEPARATED BY space.
  MOVE: lw_msg TO p_msg.
ENDFORM.                    " RKC_MSG_STRING

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  WRITE_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_output.


  PERFORM write_header.

  IF s_no_exist = 'X' OR
     s_no_exist = 'N'.
    WRITE: / g_message.
    EXIT.
  ENDIF.

  PERFORM write_data.

ENDFORM.                    " WRITE_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_header.
  DATA: i_count TYPE i.

  DESCRIBE  TABLE it_person LINES i_count.
  WRITE: /5 'Total Persons: ', i_count.

  WRITE AT /5(93) sy-uline .

  WRITE :/5 sy-vline NO-GAP.
  FORMAT COLOR COL_HEADING.
  WRITE:    text-010  NO-GAP,
           sy-vline NO-GAP, text-020  NO-GAP,
           sy-vline NO-GAP, text-030  NO-GAP,
           sy-vline NO-GAP, text-050  NO-GAP,
           sy-vline NO-GAP, text-060  NO-GAP,
           sy-vline NO-GAP, text-040  NO-GAP.

  WRITE:   sy-vline NO-GAP.
  FORMAT COLOR OFF.
  WRITE AT /5(93) sy-uline .

ENDFORM.                    " WRITE_HEADER

*&---------------------------------------------------------------------*
*&      Form  write_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_data.
  DATA: i_count TYPE i.
  DATA: i_mod TYPE i.

  SORT it_person BY pernr.

  i_count = 0.
  LOOP AT it_person.
    WRITE :/5 sy-vline NO-GAP, 7(15) it_person-pernr  NO-GAP,
*            sy-vline NO-GAP, 23(15) it_person-begda  NO-GAP,
*            sy-vline NO-GAP, 39(15) it_person-endda  NO-GAP,
            sy-vline NO-GAP, 55(15) g_date       NO-GAP,
            sy-vline NO-GAP, 71(15) g_date      NO-GAP,
            sy-vline NO-GAP, 87(10) it_person-updat  NO-GAP,
            sy-vline NO-GAP.
    i_count = i_count + 1.
    i_mod = i_count MOD 5.
    IF i_mod = 0.
      WRITE AT /5(93) sy-uline.
    ENDIF.
  ENDLOOP.
  IF i_mod NE 0.
    WRITE AT /5(93) sy-uline.
  ENDIF.
* WRITE THE MESSAGE
  DESCRIBE TABLE it_mess LINES i_count.
  IF i_count NE 0.
    SKIP.
    ULINE.
    WRITE: / 'The Following Error Occurs: '.
    ULINE.
    i_count = 1.
    LOOP AT it_mess.
      WRITE: /1(4) i_count,6(120) it_mess-msgtx.
      i_count = i_count + 1.
    ENDLOOP.
    ULINE.
  ENDIF.

ENDFORM.                    " write_data
*&---------------------------------------------------------------------*
*&      Form  PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_PERCENT  text
*----------------------------------------------------------------------*
FORM progress_indicator USING    p_percent.
  DATA: l_text(40).
  DATA: i_mod TYPE i.

  l_text = p_percent.
  CONDENSE l_text.
  i_mod = p_percent MOD 5.
  IF i_mod = 0.
    CONCATENATE l_text '% PROCESSED' INTO l_text.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              text = l_text.
  ENDIF.
ENDFORM.                    " PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*&      Form  AT_SEL_SCREEN_ON_VALUE_REQUEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0040   text
*----------------------------------------------------------------------*
FORM at_sel_screen_on_value_request USING def_path LIKE rlgrap-filename.


  DATA: tmp_filename LIKE rlgrap-filename.
  DATA: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  DATA: fieldln TYPE i.
  FIELD-SYMBOLS: <tmp_sym>.


  fieldln = strlen( def_path ) - 1.
  ASSIGN def_path+fieldln(1) TO <tmp_sym>.
  IF <tmp_sym> = '/' OR <tmp_sym> = '\'.
    CLEAR <tmp_sym>.
  ENDIF.

  CALL FUNCTION 'F4_FILENAME'
       EXPORTING
            program_name  = sy-cprog
            dynpro_number = sy-dynnr
            field_name    = ' '
       IMPORTING
            file_name     = tmp_filename.

  IF sy-subrc = 0.
    p_file = tmp_filename.
  ELSE.
    MESSAGE e000 WITH 'FILE SELECT WINDOW OPEN ERROR!'.
  ENDIF.


ENDFORM.                    " AT_SEL_SCREEN_ON_VALUE_REQUEST

*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            filename                = p_file
            i_begin_col             = 1
            i_begin_row             = 2
            i_end_col               = 3
            i_end_row               = 3000
       TABLES
            intern                  = intern
       EXCEPTIONS
            inconsistent_parameters = 1
            upload_ole              = 2
            OTHERS                  = 3.
  IF sy-subrc NE 0.
    s_error = 'X'.
    g_message = 'EXEL file upload error!'.
    MESSAGE i000 WITH g_message.
  ENDIF.
*

  LOOP AT intern.
    CASE intern-col.
      WHEN 1. it_person-pernr = intern-value.
      WHEN 2. it_person-cname = intern-value.
      WHEN 3.
        it_person-amount = intern-value.
        APPEND it_person. CLEAR it_person.
    ENDCASE.

  ENDLOOP.

ENDFORM.                    " UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 100.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'STATUS_100'.
  SET TITLEBAR 'TITLE_100'.
  DESCRIBE TABLE it_person LINES tc1-lines.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TC1_PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc1_pai INPUT.
  MODIFY it_person INDEX tc1-current_line.
ENDMODULE.                 " TC1_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  save_code = ok_code.
  CLEAR: ok_code.
  CASE save_code.
    WHEN 'EXIT' OR 'CANCEL'.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
      LEAVE PROGRAM.
    WHEN 'CHANGE'.
      PERFORM update_bdc.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  GET_PAY_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pay_date.
  LOOP AT it_person.
    it_person-begda = p_date.
    MODIFY it_person.
  ENDLOOP.
ENDFORM.                    " GET_PAY_DATE
