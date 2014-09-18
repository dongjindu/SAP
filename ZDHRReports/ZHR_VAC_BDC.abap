REPORT zhr_vac_bdc MESSAGE-ID zmpp
     NO STANDARD PAGE HEADING LINE-SIZE 105 .

* Program Name      : ZBMR905_SORTSTRING_CHECK
* Author            : Yongping
* Creation Date     : 2004.12.07.
* Specifications By : Naveen
* Pattern           :
* Development Request No :UD1K913412
* Addl Documentation:
* Description       : Upload the vacation days for specified days
*                     and people
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
TABLES: pa0002.



DATA: g_message(60).
DATA: s_no_exist(1).
DATA: g_date_low(10) TYPE c,
      g_date_high(10) TYPE c.
DATA: i_processed TYPE i.
****************************************************************
*INTERNAL TABLES
****************************************************************
DATA: BEGIN OF it_person OCCURS 0,
       pernr LIKE pa0001-pernr,
       begda LIKE pa0001-begda,
       endda LIKE pa0001-endda,
       updat(7) TYPE c,
      END OF it_person.


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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-100.
SELECT-OPTIONS: p_pernr FOR pa0002-pernr.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS: p_date FOR sy-datum OBLIGATORY.
SELECTION-SCREEN SKIP.
PARAMETERS:     p_test AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.
**********************************************************************
*END OF SELECTION SCREEN
**********************************************************************

AT SELECTION-SCREEN OUTPUT.
  p_date-low = '20050705'.
  p_date-high = '20050708'.
  p_date-option = 'BT'.
  p_date-sign = 'I'.
  APPEND p_date.

AT SELECTION-SCREEN.
* CONVERT THE DATE FOR EXTERNAL DATE FORMAT
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = p_date-low
       IMPORTING
            date_external            = g_date_low
       EXCEPTIONS
            date_internal_is_invalid = 1.
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
            date_internal            = p_date-high
       IMPORTING
            date_external            = g_date_high
       EXCEPTIONS
            date_internal_is_invalid = 1.



START-OF-SELECTION.
  PERFORM read_person.
  IF p_test = 'X'.
    PERFORM update_bdc.
  ENDIF.

END-OF-SELECTION.
  PERFORM write_output.



*&---------------------------------------------------------------------*
*&      Form  READ_PERSON
*&---------------------------------------------------------------------*
*       Read the persons specified
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_person.
  IF p_date-high LT sy-datum.
*    MESSAGE i000 WITH text-001.
*    EXIT.
  ENDIF.
  SELECT pernr begda  endda
   INTO TABLE it_person
   FROM pa0001
   WHERE pernr IN p_pernr     AND
         begda LE p_date-low  AND
         endda GE p_date-high AND
         persg NE '5'         AND
         persg NE '9'
   ORDER BY pernr.
  IF sy-subrc <> 0.
    s_no_exist = 'X'.
    g_message = 'NO VALID PERSON EXIST!'.
  ENDIF.
ENDFORM.                    " READ_PERSON
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
  IF s_no_exist = 'X'.
    EXIT.
  ENDIF.
  DESCRIBE TABLE it_person LINES i_total.
  IF i_total NE 0.
    i_processed = 0.
    LOOP AT it_person.
      CLEAR: bdcdata, bdcdata[].
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
    MODIFY it_person FROM p_person TRANSPORTING updat.
  ELSE.
    p_person-updat = 'FAIL'.
    MODIFY it_person FROM p_person TRANSPORTING updat.
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
  ELSE.
    p_update = 'F'.

    PERFORM rkc_msg_string USING l_msgstr.

    it_mess-msgty = sy-msgty.
    it_mess-msgtx = l_msgstr.
    APPEND it_mess.

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


  PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=INS'.
  PERFORM bdc_field       USING 'RP50G-PERNR'
                                p_person-pernr.
  PERFORM bdc_field       USING 'RP50G-BEGDA'
                                g_date_low.
  PERFORM bdc_field       USING 'RP50G-ENDDA'
                                g_date_high.
  PERFORM bdc_field       USING 'RP50G-CHOIC'
                                '2001'.
  PERFORM bdc_field       USING 'RP50G-SUBTY'
                                '1023'.

  PERFORM bdc_dynpro      USING 'MP200000' '2001'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPD'.
  PERFORM bdc_field       USING 'P2001-BEGDA'
                                g_date_low.
  PERFORM bdc_field       USING 'P2001-ENDDA'
                                g_date_high.

  PERFORM bdc_dynpro      USING 'MP200000' '2001'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_dynpro      USING 'MP200000' '2001'.
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
            sy-vline NO-GAP, 23(15) it_person-begda  NO-GAP,
            sy-vline NO-GAP, 39(15) it_person-endda  NO-GAP,
            sy-vline NO-GAP, 55(15) g_date_low       NO-GAP,
            sy-vline NO-GAP, 71(15) g_date_high      NO-GAP,
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
