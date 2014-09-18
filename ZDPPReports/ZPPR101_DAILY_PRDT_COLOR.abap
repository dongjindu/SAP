************************************************************************
* Program Name      : ZPPR101_DAILY_PRDT_COLOR
* Author            : Furong Wang
* Creation Date     : 10/17/2006
* Specifications By : MY Hur
* Development Request No : Mr. MY Hur
* Addl Documentation:
* Description       : Daily production summary (to HMC)
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZPPR101_DAILY_PRDT_COLOR NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID ZMPP.

*---// Constants
*
*CONSTANTS: C_FILENAME LIKE RLGRAP-FILENAME VALUE
*                      '/usr/sap/EDI_SAP/HMMA_pmt01tb'.

*---// For FTP file creation
*DATA: W_FILENAME LIKE RLGRAP-FILENAME.

DATA: BEGIN OF IT_DATA OCCURS 0,
*      CR_DATE like ztpp_wosum2-CR_DATE,
      NATION LIKE ZTPP_WOSUM2-NATION,
      MI LIKE ZTPP_WOSUM2-MI,
      OCN LIKE ZTPP_WOSUM2-OCN,
      DEALER LIKE ZTPP_WOSUM2-DEALER,
      EXTC LIKE ZTPP_WOSUM2-EXTC,
      INTC LIKE ZTPP_WOSUM2-INTC,
      REGION LIKE ZTPP_WOSUM2-REGION,
      RP08AQ LIKE ZTPP_WOSUM2-RP08AQ,
      RP09AQ LIKE ZTPP_WOSUM2-RP09AQ,
      RP15AQ LIKE ZTPP_WOSUM2-RP15AQ,
      RP08CQ LIKE ZTPP_WOSUM2-RP08CQ,
      RP09CQ LIKE ZTPP_WOSUM2-RP09CQ,
      RP10CQ LIKE ZTPP_WOSUM2-RP10CQ,
      RP11CQ LIKE ZTPP_WOSUM2-RP11CQ,
      RP12CQ LIKE ZTPP_WOSUM2-RP12CQ,
      RP13CQ LIKE ZTPP_WOSUM2-RP13CQ,
      RP14CQ LIKE ZTPP_WOSUM2-RP14CQ,
      RP15CQ LIKE ZTPP_WOSUM2-RP15CQ,
      END OF IT_DATA.

DATA: IT_PMT01TB_COL LIKE TABLE OF ZTPP_PMT01TB_COL WITH HEADER LINE.

DATA: W_COUNT TYPE I.
*      W_LAST_DAY LIKE SY-DATUM.

DATA: BEGIN OF IT_OUT OCCURS 0,
      ZDATE(8),
      ZCOMP(1),
      ZNATN(5),
      ZDIVI(1),
      ZUSEE(1),
      ZMODL(3),
      ZBMDL(12),
      ZOCN(4),
      ZEXTC(3),
      ZINTC(3),
      ZREGI(2),
      ZRESU(5),
      ZESTC(5),
      ZCNTR(5),
      ZOREM(5),
      ZCDAT(8),
      END OF IT_OUT.

TABLES: ZTPP_WOSUM2
.
*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
PARAMETERS: P_DATUM LIKE SY-DATUM.
SELECT-OPTIONS: S_NATION FOR ZTPP_WOSUM2-NATION.
SELECT-OPTIONS: S_MI FOR ZTPP_WOSUM2-MI.
*PARAMETERS: p_run AS CHECKBOX DEfault ' ' .
SELECTION-SCREEN END OF BLOCK BL1.

INITIALIZATION.
  PERFORM INIT_DATA.

START-OF-SELECTION.

*CALL FUNCTION 'SG_PS_GET_LAST_DAY_OF_MONTH'
*  EXPORTING
*    day_in                  = p_datum
*  IMPORTING
*   LAST_DAY_OF_MONTH       = w_last_day
* EXCEPTIONS
*   DAY_IN_NOT_VALID        = 1
*   OTHERS                  = 2
*          .
*IF sy-subrc <> 0.
* MESSAGE e001 with 'Date error'.
*ENDIF.

*IF P_RUN = 'X' and w_last_day <> p_datum.
*  SUBMIT ZAPP903R_DAILY_PRODUCTION WITH p_WDATE = p_datum
*                                   AND return.
*
*  SUBMIT ZAPP106_WOSUM2_DAILY AND RETURN.
*ENDIF.

  PERFORM READ_DATA.
  DESCRIBE TABLE IT_DATA LINES W_COUNT.
  IF W_COUNT <= 0.
    MESSAGE I001 WITH TEXT-M01.
    EXIT.
  ENDIF.
  PERFORM PROCESS_DATA.
*  PERFORM SEND_DATA.
  perform save_data.
*&---------------------------------------------------------------------*
*&      Form  SEND_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SEND_DATA.
*  PERFORM WRITE_FILE.
*ENDFORM.                    " SEND_RTN
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM WRITE_FILE.
*  DATA: L_CHAR(8).
*
*  L_CHAR = SY-DATUM.
*
**  CONCATENATE c_filename l_char '.text'
**         INTO w_filename.
*
*  CONCATENATE C_FILENAME '.text'
*           INTO W_FILENAME.
*  OPEN DATASET W_FILENAME IN TEXT MODE FOR OUTPUT.
*  IF SY-SUBRC <> 0.
*    MESSAGE E000 WITH TEXT-M12.
*  ENDIF.
*
*  LOOP AT IT_OUT.
*    OPEN DATASET W_FILENAME IN TEXT MODE FOR APPENDING.
*    TRANSFER IT_OUT TO W_FILENAME.
*  ENDLOOP.
*
*  IF SY-SUBRC EQ 0.
*    PERFORM SAVE_DATA.
*  ENDIF.
*
*  CLOSE DATASET W_FILENAME.
*
*  IF SY-SUBRC = 0.
*
*    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
*         EXPORTING
*              TITEL        = 'Information'
*              TEXTLINE1    = TEXT-M11
*              TEXTLINE2    = W_FILENAME
*              START_COLUMN = 25
*              START_ROW    = 6.
*  ENDIF.
*  CLEAR: IT_OUT, IT_OUT[].
*
*ENDFORM.                    " WRITE_FILE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  CLEAR: IT_DATA, IT_DATA[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_DATA
  FROM ZTPP_WOSUM2
  WHERE CR_DATE = P_DATUM
    AND NATION IN S_NATION
    AND MI IN S_MI.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: L_NEW(1),
        W_MODL(2),
        L_RP08AQ LIKE ZTPP_WOSUM2-RP08AQ,
        L_RP09AQ LIKE ZTPP_WOSUM2-RP09AQ,
        L_RP15AQ LIKE ZTPP_WOSUM2-RP15AQ,
        L_RP08CQ LIKE ZTPP_WOSUM2-RP08CQ,
        L_RP0814CQ LIKE ZTPP_WOSUM2-RP08CQ.
  DATA: L_NATION LIKE ZTPP_WOSUM2-NATION,
        L_MI LIKE ZTPP_WOSUM2-MI,
        L_OCN LIKE ZTPP_WOSUM2-OCN,
        L_DEALER LIKE ZTPP_WOSUM2-DEALER,
        L_EXTC LIKE ZTPP_WOSUM2-EXTC,
        L_INTC LIKE ZTPP_WOSUM2-INTC,
        L_NATN(5).

  CLEAR: IT_OUT, IT_OUT[].
  SORT IT_DATA BY NATION DEALER MI OCN EXTC INTC.
  READ TABLE IT_DATA INDEX 1.
  L_NATION = IT_DATA-NATION.
  L_MI = IT_DATA-MI.
  L_OCN = IT_DATA-OCN.
  L_DEALER = IT_DATA-DEALER.
  L_EXTC = IT_DATA-EXTC.
  L_INTC = IT_DATA-INTC.

  CONCATENATE L_NATION L_DEALER INTO L_NATN.
  LOOP AT IT_DATA.
    IF L_NATION <> IT_DATA-NATION OR
       L_DEALER <> IT_DATA-DEALER OR
       L_MI <> IT_DATA-MI OR
       L_OCN <> IT_DATA-OCN OR
       L_EXTC <> IT_DATA-EXTC OR
       L_INTC <> IT_DATA-INTC.

      IT_OUT-ZDATE = P_DATUM.
      IT_OUT-ZCOMP = 'H'.
      IT_OUT-ZDIVI = 'P'.
      IT_OUT-ZCDAT = SY-DATUM.

      CASE L_NATION.
        WHEN 'B28'.
          IT_OUT-ZUSEE = 'D'.
          IT_OUT-ZCNTR = L_RP09AQ.
          IT_OUT-ZESTC = L_RP08CQ.
        WHEN OTHERS.
          IT_OUT-ZUSEE = 'E'.
          IT_OUT-ZCNTR = L_RP15AQ.
          IT_OUT-ZESTC = L_RP0814CQ.
      ENDCASE.
      IT_OUT-ZMODL = L_MI+0(3).
      W_MODL = IT_OUT-ZMODL+0(2).
** CHANGED BY FURONG ON 12/21/07 REQUESTED BY DANIEL
      CASE W_MODL.
      WHEN 'CR'.
        IT_OUT-ZMODL = 'CRA'.
      WHEN 'EM'.
        IT_OUT-ZMODL = 'EMF'.
      ENDCASE.
*      IF W_MODL = 'CR'.
*        IT_OUT-ZMODL = 'CRA'.
*      ENDIF.
** END OF CHANGE
      IT_OUT-ZBMDL = L_MI.
      IT_OUT-ZOCN = L_OCN.
      IT_OUT-ZEXTC = L_EXTC.
      IT_OUT-ZINTC = L_INTC.

      IT_OUT-ZREGI = L_DEALER.
      IT_OUT-ZRESU = L_RP08AQ.
*      it_out-zestc = l_rp0915cq.
      IF IT_OUT-ZRESU+4(1) = ' '.
        SHIFT IT_OUT-ZRESU RIGHT.
      ENDIF.
      IF IT_OUT-ZESTC+4(1) = ' '.
        SHIFT IT_OUT-ZESTC RIGHT.
      ENDIF.
      IT_OUT-ZOREM = '    0'.
      IT_OUT-ZNATN = L_NATN.
*      IF l_rp08aq > 0 OR l_rp0915cq > 0.
      APPEND IT_OUT.
*      ENDIF.
      L_NATION = IT_DATA-NATION.
      L_MI = IT_DATA-MI.
      L_OCN = IT_DATA-OCN.
      L_DEALER = IT_DATA-DEALER.
      L_EXTC = IT_DATA-EXTC.
      L_INTC = IT_DATA-INTC.
      CONCATENATE L_NATION L_DEALER INTO L_NATN.
      CLEAR: IT_OUT,L_RP08AQ, L_RP09AQ, L_RP15AQ.
      CLEAR: L_RP08CQ,L_RP0814CQ.
    ENDIF.

    L_RP08AQ = L_RP08AQ + IT_DATA-RP08AQ.    " sign off qty
    L_RP09AQ = L_RP09AQ + IT_DATA-RP09AQ.    " ship in qty
    L_RP15AQ = L_RP15AQ + IT_DATA-RP15AQ.    " ship in qty
    L_RP08CQ = L_RP08CQ + IT_DATA-RP08CQ.

    L_RP0814CQ = L_RP0814CQ + IT_DATA-RP08CQ
                   + IT_DATA-RP09CQ + IT_DATA-RP10CQ
                   + IT_DATA-RP11CQ + IT_DATA-RP12CQ
                   + IT_DATA-RP13CQ + IT_DATA-RP14CQ.


*    l_rp0915cq = l_rp0915cq
*                   + it_data-rp09cq + it_data-rp10cq
*                   + it_data-rp11cq + it_data-rp12cq
*                   + it_data-rp13cq + it_data-rp14cq
*                   + it_data-rp15cq.
    CLEAR: IT_DATA.
  ENDLOOP.
  IF L_NATION <> IT_DATA-NATION OR
     L_DEALER <> IT_DATA-DEALER OR
     L_MI <> IT_DATA-MI OR
     L_OCN <> IT_DATA-OCN OR
     L_EXTC <> IT_DATA-EXTC OR
     L_INTC <> IT_DATA-INTC.

     IT_OUT-ZDATE = P_DATUM.
     IT_OUT-ZCOMP = 'H'.
     IT_OUT-ZNATN = 'HMMA'.
     IT_OUT-ZDIVI = 'P'.
     IT_OUT-ZCDAT = SY-DATUM.

    CASE L_NATION.
      WHEN 'B28'.
        IT_OUT-ZUSEE = 'D'.
        IT_OUT-ZCNTR = L_RP09AQ.
        IT_OUT-ZESTC = L_RP08CQ.
      WHEN OTHERS.
        IT_OUT-ZUSEE = 'E'.
        IT_OUT-ZCNTR = L_RP15AQ.
        IT_OUT-ZESTC = L_RP0814CQ.
    ENDCASE.

    IT_OUT-ZMODL = L_MI+0(3).
    W_MODL = IT_OUT-ZMODL+0(2).
    IF W_MODL = 'CR'.
      IT_OUT-ZMODL = 'CRA'.
    ENDIF.
    IT_OUT-ZBMDL = L_MI.
    IT_OUT-ZOCN = L_OCN.
    IT_OUT-ZEXTC = L_EXTC.
    IT_OUT-ZINTC = L_INTC.
    IT_OUT-ZREGI = L_DEALER.
    IT_OUT-ZRESU = L_RP08AQ.
    IF IT_OUT-ZRESU+4(1) = ' '.
      SHIFT IT_OUT-ZRESU RIGHT.
    ENDIF.
    IF IT_OUT-ZESTC+4(1) = ' '.
      SHIFT IT_OUT-ZESTC RIGHT.
    ENDIF.
    IT_OUT-ZOREM = '    0'.
    IT_OUT-ZNATN = L_NATN.
    APPEND IT_OUT.
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
FORM INIT_DATA.
  P_DATUM = SY-DATUM - 1.
ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  CLEAR: IT_PMT01TB_COL, IT_PMT01TB_COL[].
  LOOP AT IT_OUT.
    MOVE-CORRESPONDING IT_OUT TO IT_PMT01TB_COL.
    IT_PMT01TB_COL-MANDT = SY-MANDT.
    APPEND IT_PMT01TB_COL.
    CLEAR: IT_PMT01TB_COL, IT_OUT..
  ENDLOOP.
  DELETE FROM ZTPP_PMT01TB_COL WHERE ZDATE = P_DATUM.
  MODIFY ZTPP_PMT01TB_COL FROM TABLE IT_PMT01TB_COL.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " save_data
