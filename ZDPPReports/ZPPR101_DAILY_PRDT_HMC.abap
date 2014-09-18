************************************************************************
* Program Name      : ZPPR101_DAILY_PRDT_HMC
* Author            : Furong Wang
* Creation Date     : 01/21/2006
* Specifications By : MY Hur
* Development Request No : Mr. MY Hur
* Addl Documentation:
* Description       : Daily production summary (to HMC)
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT zppr101_daily_prdt_hmc NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID zmpp.

*---// Constants
*
CONSTANTS: c_filename LIKE rlgrap-filename VALUE
                      '/usr/sap/EDI_SAP/HMMA_pmt01tb'.

*---// For FTP file creation
DATA: w_filename LIKE rlgrap-filename.


DATA: BEGIN OF it_data OCCURS 0,
*      CR_DATE like ztpp_wosum2-CR_DATE,
      nation LIKE ztpp_wosum2-nation,
      mi LIKE ztpp_wosum2-mi,
      ocn LIKE ztpp_wosum2-ocn,
      dealer LIKE ztpp_wosum2-dealer,
      region LIKE ztpp_wosum2-region,
      rp08aq LIKE ztpp_wosum2-rp08aq,
      rp09aq LIKE ztpp_wosum2-rp09aq,
      rp15aq LIKE ztpp_wosum2-rp15aq,
      rp08cq LIKE ztpp_wosum2-rp08cq,
      rp09cq LIKE ztpp_wosum2-rp09cq,
      rp10cq LIKE ztpp_wosum2-rp10cq,
      rp11cq LIKE ztpp_wosum2-rp11cq,
      rp12cq LIKE ztpp_wosum2-rp12cq,
      rp13cq LIKE ztpp_wosum2-rp13cq,
      rp14cq LIKE ztpp_wosum2-rp14cq,
      rp15cq LIKE ztpp_wosum2-rp15cq,
      END OF it_data.

DATA: it_ztpp_pmt01tb LIKE TABLE OF ztpp_pmt01tb WITH HEADER LINE.

DATA: w_count TYPE i,
      w_last_day like sy-datum.

DATA: BEGIN OF it_out OCCURS 0,
      zdate(8),
      zcomp(1),
      znati(5),
      zdivi(1),
      zusee(1),
      zmodl(3),
      zbmdl(12),
      zocn(4),
      zregi(2),
      zresu(5),
      zestc(5),
      zcntr(5),
      zorem(5),
      zcdat(8),
      END OF it_out.

TABLES: ztpp_wosum2
.
*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS: p_datum LIKE sy-datum.
SELECT-OPTIONS: s_nation FOR ztpp_wosum2-nation.
SELECT-OPTIONS: s_mi FOR ztpp_wosum2-mi.
PARAMETERS: p_run AS CHECKBOX default 'X'.
SELECTION-SCREEN END OF BLOCK bl1.

INITIALIZATION.
  PERFORM init_data.

START-OF-SELECTION.

CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
  EXPORTING
    day_in                  = p_datum
  IMPORTING
   LAST_DAY_OF_MONTH       = w_last_day
 EXCEPTIONS
   DAY_IN_NOT_VALID        = 1
   OTHERS                  = 2
          .
IF sy-subrc <> 0.
 MESSAGE e001 with 'Date error'.
ENDIF.

IF P_RUN = 'X' and w_last_day <> p_datum.
  SUBMIT ZAPP903R_DAILY_PRODUCTION WITH p_WDATE = p_datum
                                   AND return.

  SUBMIT ZAPP106_WOSUM2_DAILY AND RETURN.
ENDIF.

  PERFORM read_data.
  DESCRIBE TABLE it_data LINES w_count.
  IF w_count <= 0.
    MESSAGE i001 WITH text-m01.
    EXIT.
  ENDIF.
  PERFORM process_data.
  PERFORM send_data.

*&---------------------------------------------------------------------*
*&      Form  SEND_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_data.
  PERFORM write_file.
ENDFORM.                    " SEND_RTN
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM write_file.
  DATA: l_char(8).

  l_char = sy-datum.

*  CONCATENATE c_filename l_char '.text'
*         INTO w_filename.

  CONCATENATE c_filename '.text'
           INTO w_filename.
  OPEN DATASET w_filename IN TEXT MODE FOR OUTPUT.
  IF sy-subrc <> 0.
    MESSAGE e000 WITH text-m12.
  ENDIF.

  LOOP AT it_out.
    OPEN DATASET w_filename IN TEXT MODE FOR APPENDING.
    TRANSFER it_out TO w_filename.
  ENDLOOP.

  IF sy-subrc EQ 0.
    PERFORM save_data.
  ENDIF.

  CLOSE DATASET w_filename.

  IF sy-subrc = 0.

    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
         EXPORTING
              titel        = 'Information'
              textline1    = text-m11
              textline2    = w_filename
              start_column = 25
              start_row    = 6.
  ENDIF.
  CLEAR: it_out, it_out[].

ENDFORM.                    " WRITE_FILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
  CLEAR: it_data, it_data[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_data
  FROM ztpp_wosum2
  WHERE cr_date = p_datum
    AND nation IN s_nation
    AND mi IN s_mi.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_new(1),
        w_modl(2),
        l_rp08aq LIKE ztpp_wosum2-rp08aq,
        l_rp09aq LIKE ztpp_wosum2-rp09aq,
        l_rp15aq LIKE ztpp_wosum2-rp15aq,
        l_rp08cq LIKE ztpp_wosum2-rp08cq,
        l_rp0814cq LIKE ztpp_wosum2-rp08cq.
  DATA: l_nation LIKE ztpp_wosum2-nation,
  l_mi LIKE ztpp_wosum2-mi,
  l_ocn LIKE ztpp_wosum2-ocn,
  l_dealer LIKE ztpp_wosum2-dealer.

  CLEAR: it_out, it_out[].
  SORT it_data BY nation mi ocn dealer.
  READ TABLE it_data INDEX 1.
  l_nation = it_data-nation.
  l_mi = it_data-mi.
  l_ocn = it_data-ocn.
  l_dealer = it_data-dealer.

  LOOP AT it_data.
    IF l_nation <> it_data-nation OR
       l_mi <> it_data-mi OR
       l_ocn <> it_data-ocn OR
       l_dealer <> it_data-dealer.

      it_out-zdate = p_datum.
      it_out-zcomp = 'H'.
      it_out-znati = 'HMMA'.
      it_out-zdivi = 'P'.
      it_out-zcdat = sy-datum.

      CASE l_nation.
        WHEN 'B28'.
          it_out-zusee = 'D'.
          it_out-ZCNTR = l_rp09aq.
          it_out-zestc = l_rp08cq.
        WHEN OTHERS.
          it_out-zusee = 'E'.
          it_out-ZCNTR = l_rp15aq.
          it_out-zestc = l_rp0814cq.
      ENDCASE.
      it_out-zmodl = l_mi+0(3).
      w_modl = it_out-zmodl+0(2).
      IF w_modl = 'CR'.
        it_out-zmodl = 'CRA'.
      ENDIF.
      it_out-zbmdl = l_mi.

      it_out-zocn = l_ocn.
      it_out-zregi = l_dealer.

      it_out-zresu = l_rp08aq.
*      it_out-zestc = l_rp0915cq.

      IF it_out-zresu+4(1) = ' '.
        SHIFT it_out-zresu RIGHT.
      ENDIF.
      IF it_out-zestc+4(1) = ' '.
        SHIFT it_out-zestc RIGHT.
      ENDIF.

       it_out-zorem = '    0'.

*      IF l_rp08aq > 0 OR l_rp0915cq > 0.
      APPEND it_out.
*      ENDIF.
      l_nation = it_data-nation.
      l_mi = it_data-mi.
      l_ocn = it_data-ocn.
      l_dealer = it_data-dealer.
      CLEAR: it_out,l_rp08aq, l_rp09aq, l_rp15aq.
      clear: l_rp08cq,l_rp0814cq.
    ENDIF.

    l_rp08aq = l_rp08aq + it_data-rp08aq.    " sign off qty
    l_rp09aq = l_rp09aq + it_data-rp09aq.    " ship in qty
    l_rp15aq = l_rp15aq + it_data-rp15aq.    " ship in qty
    l_rp08cq = l_rp08cq + it_data-rp08cq.

    l_rp0814cq = l_rp0814cq + it_data-rp08cq
                   + it_data-rp09cq + it_data-rp10cq
                   + it_data-rp11cq + it_data-rp12cq
                   + it_data-rp13cq + it_data-rp14cq.


*    l_rp0915cq = l_rp0915cq
*                   + it_data-rp09cq + it_data-rp10cq
*                   + it_data-rp11cq + it_data-rp12cq
*                   + it_data-rp13cq + it_data-rp14cq
*                   + it_data-rp15cq.
    CLEAR: it_data.
  ENDLOOP.
  IF l_nation <> it_data-nation OR
    l_mi <> it_data-mi OR
    l_ocn <> it_data-ocn OR
    l_dealer <> it_data-dealer.

    it_out-zdate = p_datum.
    it_out-zcomp = 'H'.
    it_out-znati = 'HMMA'.
    it_out-zdivi = 'P'.
    it_out-zcdat = sy-datum.

    CASE l_nation.
       WHEN 'B28'.
          it_out-zusee = 'D'.
          it_out-ZCNTR = l_rp09aq.
          it_out-zestc = l_rp08cq.
        WHEN OTHERS.
          it_out-zusee = 'E'.
          it_out-ZCNTR = l_rp15aq.
          it_out-zestc = l_rp0814cq.
      ENDCASE.

    it_out-zmodl = l_mi+0(3).
    w_modl = it_out-zmodl+0(2).
    IF w_modl = 'CR'.
      it_out-zmodl = 'CRA'.
    ENDIF.
    it_out-zbmdl = l_mi.

    it_out-zocn = l_ocn.
    it_out-zregi = l_dealer.

    it_out-zresu = l_rp08aq.

      IF it_out-zresu+4(1) = ' '.
      SHIFT it_out-zresu RIGHT.
    ENDIF.
    IF it_out-zestc+4(1) = ' '.
      SHIFT it_out-zestc RIGHT.
    ENDIF.

    it_out-zorem = '    0'.

*      IF l_rp08aq > 0 OR l_rp0915cq > 0.
    APPEND it_out.
*      ENDIF.
*    CLEAR: it_out,l_rp08aq, l_rp0814cq, l_rp15aq.

  ENDIF.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
  p_datum = sy-datum - 1.
ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  CLEAR: it_ztpp_pmt01tb, it_ztpp_pmt01tb[].
  LOOP AT it_out.
    MOVE-CORRESPONDING it_out TO it_ztpp_pmt01tb.
    it_ztpp_pmt01tb-mandt = sy-mandt.
    APPEND it_ztpp_pmt01tb.
    CLEAR: it_ztpp_pmt01tb, it_out..
  ENDLOOP.
  DELETE FROM ztpp_pmt01tb WHERE zdate = p_datum.
  MODIFY ztpp_pmt01tb FROM TABLE it_ztpp_pmt01tb.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " save_data
