*----------------------------------------------------------------------*
*   INCLUDE ZSSD05U_HMA_COMMERCIAL_INV_F01                             *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
FORM MODIFY_DATA USING RC
                       US_SCREEN.

  PERFORM MAKE_HEAD.
  PERFORM MAKE_ITEM.
  PERFORM CALL_SM USING RC.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  MAKE_HEAD
*&---------------------------------------------------------------------*
FORM MAKE_HEAD.
  REFRESH : IT_INV_H.
  CLEAR   : IT_INV_H.

  SELECT SINGLE *
         FROM VBRK
        WHERE VBELN EQ NAST-OBJKY.

  IF VBRK-FKART = 'ZVF2' or
     vbrk-fkart = 'ZSL2'.
    W_SIGN = ''.
    IT_INV_H-Z112 = ''.
  ELSE. "ZVRE
    W_SIGN = '-'.
    IT_INV_H-Z112 = 'Credit Memo'.
  ENDIF.
* requested by Lance Young added by chris
* add purchase order number in the interface

  select single  bstnk into l_BSTNK
   from vbak as a inner join vbfa as b
     on a~vbeln = b~vbelv
   where b~vbeln = vbrk-vbeln
     and ( vbtyp_n = 'M' or
           vbtyp_n = 'N' )
     AND VBTYP_V = 'C'.
   IF SY-SUBRC EQ 0.
     IT_INV_H-BSTNK = L_BSTNK.
   ENDIF.
* end of add on 06/17/2005

* 1
*                 12345678901234567890123456789012345 "35
  IT_INV_H-Z011 = 'HYUNDAI MOTOR MANUFACTURING ALABAMA'.
  IT_INV_H-Z012 = '700 Hyundai Boulevard, AL36105'.
  IT_INV_H-Z013 = ''.

* 2
  SELECT SINGLE * FROM KNA1 WHERE KUNNR EQ VBRK-KUNAG.
  IT_INV_H-Z021 = KNA1-NAME1.
  IT_INV_H-Z022 = KNA1-STRAS.
  CONCATENATE KNA1-ORT01 KNA1-PSTLZ INTO IT_INV_H-Z023 SEPARATED BY ' '.
  SELECT SINGLE * FROM T005T WHERE SPRAS = 'EN'
                             AND   LAND1 = KNA1-LAND1.
  IT_INV_H-Z024 = T005T-LANDX.

* CHANGED BY CHRIS-ADDING PAYMENT TERMS

   SELECT SINGLE A~ZTERM B~TEXT1
    INTO (IT_INV_H-ZTERM, IT_INV_H-TEXT1)
    FROM  KNB1 AS A INNER JOIN T052U AS B
      ON  A~ZTERM = B~ZTERM
    WHERE B~SPRAS = 'EN' AND
          A~KUNNR = KNA1-KUNNR.

* END OF CHANGE ON 01/25/2005
* 3
  IT_INV_H-Z031 = IT_INV_H-Z021.
  IT_INV_H-Z032 = IT_INV_H-Z022.
  IT_INV_H-Z033 = IT_INV_H-Z023.
  IT_INV_H-Z034 = IT_INV_H-Z024.

* 4
  IT_INV_H-Z041 = 'Montgomery, U.S.A.'.

* 5
  IT_INV_H-Z051 = 'Montgomery, U.S.A.'.

* 6  "TBD
* 7  "TBD

* 8
* B28 + YYMMDD (Nation Code + 040217) / YYMMDD
**  PERFORM MAKE_SEQ USING 'B28' W_CNT.
  PERFORM MAKE_EDATE USING W_EDATE.
**  CONCATENATE 'B28' SY-DATUM+2(6)
**              INTO IT_INV_H-Z081.
**  CONCATENATE IT_INV_H-Z081 '/' W_EDATE
**              INTO IT_INV_H-Z081 SEPARATED BY ''.
  CONCATENATE VBRK-VBELN '/' W_EDATE
              INTO IT_INV_H-Z081 SEPARATED BY '  '.

* 9  "ITEM

* 10
  IT_INV_H-Z101 = ''. "TBD

* 11
**  IT_INV_H-Z111 = 'TO THE ORDER OF HYUNDAI MOTOR AMERICA'.

* 12
  IT_INV_H-Z121 = 'HMA  U.S.A.  MADE IN U.S.A.'. "FIXED IN FORM

* 13
  IT_INV_H-Z130 = 'EXW U.S.A.'.
  CONCATENATE VBRK-INCO1 VBRK-INCO2 INTO IT_INV_H-Z131 SEPARATED BY ' '.
  IT_INV_H-Z131 = 'HYUNDAI VEHICLE'.

* 14  "ITEM
* 15  "ITEM
* 16  "ITEM
ENDFORM.                    " MAKE_HEAD
*&---------------------------------------------------------------------*
*&      Form  MAKE_SEQ
*&---------------------------------------------------------------------*
**FORM MAKE_SEQ USING NATION SEQ.
**  TABLES : ZTSD_INV_SEQ.
**  DATA   : W_KEY(9).
**
**  CONCATENATE NATION SY-DATUM+2(6) INTO W_KEY.
**
**  SELECT SINGLE *
**         FROM ZTSD_INV_SEQ
**        WHERE ZKEY EQ W_KEY.
**  IF SY-SUBRC = 0.
**    ZTSD_INV_SEQ-ZSEQ = ZTSD_INV_SEQ-ZSEQ + 1.
**    MODIFY ZTSD_INV_SEQ.
**    SEQ = ZTSD_INV_SEQ-ZSEQ.
**  ELSE.
**    ZTSD_INV_SEQ-ZKEY = W_KEY.
**    ZTSD_INV_SEQ-ZSEQ = '1'.
**    INSERT ZTSD_INV_SEQ.
**    SEQ = ZTSD_INV_SEQ-ZSEQ.
**  ENDIF.
**ENDFORM.                    " MAKE_SEQ
*&---------------------------------------------------------------------*
*&      Form  MAKE_EDATE
*&---------------------------------------------------------------------*
FORM MAKE_EDATE USING EDATE.
**  TABLES : T247.

  WRITE VBRK-FKDAT TO EDATE.

**  SELECT SINGLE * FROM T247
**                 WHERE SPRAS EQ 'EN'
**                 AND   MNR   EQ VBRK-FKDAT+4(2).
**
**  REPLACE VBRK-FKDAT+4(2) WITH T247-KTX INTO EDATE.
ENDFORM.                    " MAKE_EDATE
*&---------------------------------------------------------------------*
*&      Form  MAKE_EDATE2
*&---------------------------------------------------------------------*
FORM MAKE_EDATE2 USING EDATE.
  TABLES : T247.

  WRITE H9_D TO EDATE.

  SELECT SINGLE * FROM T247
                 WHERE SPRAS EQ 'EN'
                 AND   MNR   EQ H9_D+4(2).

  REPLACE H9_D+4(2) WITH T247-KTX INTO EDATE.
ENDFORM.                    " MAKE_EDATE2
*&---------------------------------------------------------------------*
*&      Form  MAKE_ITEM
*&---------------------------------------------------------------------*
FORM MAKE_ITEM.
  REFRESH : IT_INV_I, IT_LIST, IT_LIST_MODEL, IT_LIST_MONTH.
  CLEAR   : IT_INV_I, IT_LIST, IT_LIST_MODEL, IT_LIST_MONTH.

  SELECT *
         FROM VBRP
        WHERE VBELN EQ NAST-OBJKY.
*    IT_LIST-MODEL = VBRP-MATNR+6(3).
    SELECT SINGLE * FROM ZTSD_VEH_MOD WHERE ZK = VBRP-MATNR+6(3).
    IF SY-SUBRC = 0.
      IT_LIST-MODEL = ZTSD_VEH_MOD-ZM.
    ENDIF.
    IT_LIST-MONTH = VBRP-AUBEL+1(4).
    IT_LIST-MATNR = VBRP-MATNR.
*    IT_LIST-ARKTX = VBRP-ARKTX.
**    SELECT SINGLE * FROM VBAK WHERE VBELN = VBRP-AUBEL.
**    IF SY-SUBRC = 0.
**      IF VBAK-BSTNK+14(2) = 'A1' OR
**         VBAK-BSTNK+14(2) = 'K1' OR
**         VBAK-BSTNK+14(2) = 'U1'.
**        CONCATENATE IT_LIST-MODEL '(MICA)'
**                    INTO IT_LIST-ARKTX SEPARATED BY SPACE.
**      ELSE.
        IT_LIST-ARKTX = IT_LIST-MODEL.
**      ENDIF.
**    ENDIF.
    IT_LIST-FKIMG = VBRP-FKIMG.
    IT_LIST-VRKME = VBRP-VRKME.
    IT_LIST-NETWR = VBRP-NETWR.

    MOVE-CORRESPONDING IT_LIST TO IT_LIST_MODEL.
    COLLECT IT_LIST_MODEL.

    MOVE-CORRESPONDING IT_LIST TO IT_LIST_MONTH.
    COLLECT IT_LIST_MONTH.

    COLLECT IT_LIST. CLEAR IT_LIST.
  ENDSELECT.

  SORT : IT_LIST, IT_LIST_MODEL, IT_LIST_MONTH.

  W_FIRST = 'Y'.
  H9 = SY-DATUM+2(4).
  LOOP AT IT_LIST_MODEL.
    LOOP AT IT_LIST_MONTH WHERE MODEL = IT_LIST_MODEL-MODEL.
      LOOP AT IT_LIST WHERE MODEL = IT_LIST_MONTH-MODEL
                      AND   MONTH = IT_LIST_MONTH-MONTH.
        IF W_FIRST = 'Y'.
          IT_INV_I-MONTH = IT_LIST-MONTH.
          W_FIRST = 'N'.
          IF H9 > IT_INV_I-MONTH.
            H9 = IT_INV_I-MONTH.
          ENDIF.
        ELSE.
          IT_INV_I-MONTH = ''.
        ENDIF.
        PERFORM MOVE_LINE.
      ENDLOOP.
      W_FIRST = 'Y'.
      PERFORM MOVE_MONTH_TOT.
    ENDLOOP.
    PERFORM MOVE_MODEL_TOT.
  ENDLOOP.
  PERFORM MOVE_LAST_TOT.
**  LOOP AT IT_LIST_MODEL.
**    LOOP AT IT_LIST WHERE MODEL = IT_LIST_MODEL-MODEL.
**      PERFORM MOVE_LINE.
**    ENDLOOP.
**    PERFORM MOVE_MODEL_TOT.
**  ENDLOOP.
**  PERFORM MOVE_LAST_TOT.
ENDFORM.                    " MAKE_ITEM
*&---------------------------------------------------------------------*
*&      Form  MOVE_LINE
*&---------------------------------------------------------------------*
FORM MOVE_LINE.
  IT_INV_I-MATNR = IT_LIST-MATNR+6(12).
  IT_INV_I-ARKTX = IT_LIST-ARKTX.

  WRITE IT_LIST-FKIMG TO IT_INV_I-FKIMG UNIT IT_LIST-VRKME.
  IT_INV_I-UNIT = ''.
  IF IT_LIST-FKIMG NE 0.
    W_PRICE = IT_LIST-NETWR / IT_LIST-FKIMG.
  ELSE.
    W_PRICE = 0.
  ENDIF.
  WRITE W_PRICE       TO IT_INV_I-EXW   CURRENCY VBRK-WAERK.
  WRITE IT_LIST-NETWR TO IT_INV_I-NETWR CURRENCY VBRK-WAERK.

*  CONCATENATE W_SIGN IT_INV_I-FKIMG INTO IT_INV_I-FKIMG.
*  CONCATENATE W_SIGN IT_INV_I-NETWR INTO IT_INV_I-NETWR.
  IF W_SIGN = '-'.
    PERFORM SIGN USING IT_INV_I-FKIMG.
    PERFORM SIGN USING IT_INV_I-NETWR.
  ENDIF.

  APPEND IT_INV_I. CLEAR IT_INV_I.
ENDFORM.                    " MOVE_LINE
*&---------------------------------------------------------------------*
*&      Form  MOVE_MONTH_TOT
*&---------------------------------------------------------------------*
FORM MOVE_MONTH_TOT.
  IT_INV_I-GUBUN = 'S'.
* IT_INV_I-MATNR = 'yymm Subtotal'.
  CONCATENATE IT_LIST_MONTH-MONTH 'Subtotal' INTO IT_INV_I-MATNR
                                             SEPARATED BY ' '.
  IT_INV_I-ARKTX = IT_LIST_MONTH-MODEL.

  WRITE IT_LIST_MONTH-FKIMG TO IT_INV_I-FKIMG UNIT IT_LIST_MONTH-VRKME.
  IT_INV_I-UNIT = 'UNITS'.
  IT_INV_I-EXW  = 'US'.
  WRITE IT_LIST_MONTH-NETWR TO IT_INV_I-NETWR CURRENCY VBRK-WAERK.

*  CONCATENATE W_SIGN IT_INV_I-FKIMG INTO IT_INV_I-FKIMG.
*  CONCATENATE W_SIGN IT_INV_I-NETWR INTO IT_INV_I-NETWR.
  IF W_SIGN = '-'.
    PERFORM SIGN USING IT_INV_I-FKIMG.
    PERFORM SIGN USING IT_INV_I-NETWR.
  ENDIF.

  APPEND IT_INV_I. CLEAR IT_INV_I.
ENDFORM.                    " MOVE_MONTH_TOT
*&---------------------------------------------------------------------*
*&      Form  MOVE_MODEL_TOT
*&---------------------------------------------------------------------*
FORM MOVE_MODEL_TOT.
  IT_INV_I-GUBUN = 'S'.
  IT_INV_I-MATNR = 'Total'.
  IT_INV_I-ARKTX = IT_LIST_MODEL-MODEL.

  WRITE IT_LIST_MODEL-FKIMG TO IT_INV_I-FKIMG UNIT IT_LIST_MODEL-VRKME.
  IT_INV_I-UNIT = 'UNITS'.
  IT_INV_I-EXW  = 'US'.
  WRITE IT_LIST_MODEL-NETWR TO IT_INV_I-NETWR CURRENCY VBRK-WAERK.

*  CONCATENATE W_SIGN IT_INV_I-FKIMG INTO IT_INV_I-FKIMG.
*  CONCATENATE W_SIGN IT_INV_I-NETWR INTO IT_INV_I-NETWR.
  IF W_SIGN = '-'.
    PERFORM SIGN USING IT_INV_I-FKIMG.
    PERFORM SIGN USING IT_INV_I-NETWR.
  ENDIF.

  APPEND IT_INV_I. CLEAR IT_INV_I.


* SUM IN HEADER *&*
  IF W_CHK <> 'Y'.
    IT_INV_H-Z132 = IT_LIST_MODEL-MODEL.
    CONCATENATE 'HYUNDAI' IT_LIST_MODEL-MODEL INTO IT_INV_H-Z134
                                              SEPARATED BY ' '.
    WRITE IT_LIST_MODEL-FKIMG TO IT_INV_H-Z141 UNIT IT_LIST_MODEL-VRKME.
    WRITE IT_LIST_MODEL-NETWR TO IT_INV_H-Z161 CURRENCY VBRK-WAERK.

*    CONCATENATE W_SIGN IT_INV_H-Z141 INTO IT_INV_H-Z141.
*    CONCATENATE W_SIGN IT_INV_H-Z161 INTO IT_INV_H-Z161.
    IF W_SIGN = '-'.
      PERFORM SIGN USING IT_INV_H-Z141.
      PERFORM SIGN USING IT_INV_H-Z161.
    ENDIF.

    W_CHK = 'Y'.
  ELSE.
    IT_INV_H-Z133 = IT_LIST_MODEL-MODEL.
    CONCATENATE 'HYUNDAI' IT_LIST_MODEL-MODEL INTO IT_INV_H-Z135
                                              SEPARATED BY ' '.
    WRITE IT_LIST_MODEL-FKIMG TO IT_INV_H-Z142 UNIT IT_LIST_MODEL-VRKME.
    WRITE IT_LIST_MODEL-NETWR TO IT_INV_H-Z162 CURRENCY VBRK-WAERK.

*    CONCATENATE W_SIGN IT_INV_H-Z142 INTO IT_INV_H-Z142.
*    CONCATENATE W_SIGN IT_INV_H-Z162 INTO IT_INV_H-Z162.
    IF W_SIGN = '-'.
      PERFORM SIGN USING IT_INV_H-Z142.
      PERFORM SIGN USING IT_INV_H-Z162.
    ENDIF.
  ENDIF.
ENDFORM.                    " MOVE_MODEL_TOT
*&---------------------------------------------------------------------*
*&      Form  MOVE_LAST_TOT
*&---------------------------------------------------------------------*
FORM MOVE_LAST_TOT.
  LOOP AT IT_LIST.
    SUM.
  ENDLOOP.

  IT_INV_I-GUBUN = 'L'.
  IT_INV_I-MATNR = 'TOTAL'.
  IT_INV_I-ARKTX = 'EXW'.

  WRITE IT_LIST-FKIMG TO IT_INV_I-FKIMG UNIT     IT_LIST-VRKME.
  IT_INV_I-UNIT = 'UNITS'.
  IT_INV_I-EXW  = 'US'.
  WRITE IT_LIST-NETWR TO IT_INV_I-NETWR CURRENCY VBRK-WAERK.

*  CONCATENATE W_SIGN IT_INV_I-FKIMG INTO IT_INV_I-FKIMG.
*  CONCATENATE W_SIGN IT_INV_I-NETWR INTO IT_INV_I-NETWR.
  IF W_SIGN = '-'.
    PERFORM SIGN USING IT_INV_I-FKIMG.
    PERFORM SIGN USING IT_INV_I-NETWR.
  ENDIF.

  APPEND IT_INV_I. CLEAR IT_INV_I.

* SUM IN HEADER *&*

**  SELECT SINGLE * FROM LIKP WHERE VBELN EQ VBRP-VGBEL.
**  IF SY-SUBRC = 0.
**    IT_INV_H-Z061 = LIKP-LIFEX.
**
**    WRITE LIKP-WADAT_IST TO IT_INV_H-Z071.
**
**    SELECT SINGLE * FROM TVROT WHERE SPRAS = 'E'
**                               AND   ROUTE = LIKP-ROUTE.
**    IF SY-SUBRC = 0.
**      IT_INV_H-Z051 = TVROT-BEZEI.
**    ENDIF.
**  ENDIF.

  WRITE IT_LIST-FKIMG TO IT_INV_H-Z143 UNIT     IT_LIST-VRKME.
  WRITE IT_LIST-NETWR TO IT_INV_H-Z163 CURRENCY VBRK-WAERK.

*  CONCATENATE W_SIGN IT_INV_H-Z143 INTO IT_INV_H-Z143.
*  CONCATENATE W_SIGN IT_INV_H-Z163 INTO IT_INV_H-Z163.
  IF W_SIGN = '-'.
    PERFORM SIGN USING IT_INV_H-Z143.
    PERFORM SIGN USING IT_INV_H-Z163.
  ENDIF.

  H9+4(2) = '15'.
  CONCATENATE '20' H9 INTO H9.
  H9_D = H9.
  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING
      MONTHS        = -1
      OLDDATE       = H9_D
    IMPORTING
      NEWDATE       = H9_D.
  PERFORM MAKE_EDATE2 USING W_EDATE2.
  CONCATENATE VBRK-VBELN '/' W_EDATE2
              INTO IT_INV_H-Z091 SEPARATED BY '  '.

  MODIFY IT_INV_H INDEX 1.
ENDFORM.                    " MOVE_LAST_TOT
*&---------------------------------------------------------------------*
*&      Form  SIGN
*&---------------------------------------------------------------------*
FORM SIGN USING TEXT.
  DATA : POS TYPE I.

  POS = 2.

  DO 100 TIMES.
    IF TEXT+POS(1) CA '123456789'.
      POS = POS - 2.
      EXIT.
    ELSE.
      POS = POS + 1.
    ENDIF.
  ENDDO.

  TEXT+POS(1) = '-'.
ENDFORM.                    " SIGN
*&---------------------------------------------------------------------*
*&      Form  CALL_SM
*&---------------------------------------------------------------------*
FORM CALL_SM USING RC.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = 'ZSSD05_HMA_COMMERCIAL_INV_SCR'
      IMPORTING
      FM_NAME            = FUNC_MOD_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.
  RC = SY-SUBRC.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.


*  CONTROL_PARAMETERS-NO_DIALOG = 'X'.

*  OUTPUT_OPTIONS-TDNOPREV = 'X'.
  OUTPUT_OPTIONS-TDNEWID = 'X'.
*  OUTPUT_OPTIONS-TDIMMED = 'X'.
*  OUTPUT_OPTIONS-TDDELETE = 'X'.

  CLEAR JOB_OUTPUT_INFO.

  CALL FUNCTION FUNC_MOD_NAME
    EXPORTING
      CONTROL_PARAMETERS = CONTROL_PARAMETERS
      OUTPUT_OPTIONS     = OUTPUT_OPTIONS
*      USER_SETTINGS      = ''
      IT_INV_H           = IT_INV_H
    IMPORTING
      JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
    TABLES
      IT_INV_I           = IT_INV_I
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  RC = SY-SUBRC.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.


  W_SPOOLID[] = JOB_OUTPUT_INFO-SPOOLIDS[].
  READ TABLE W_SPOOLID INDEX 1.
  SRCSPOOLID = W_SPOOLID.

  CHECK SRCSPOOLID NE 0.

  CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
    EXPORTING
      SRC_SPOOLID              = SRCSPOOLID
      NO_DIALOG                = ' '
    IMPORTING
      PDF_BYTECOUNT            = NUMBYTES
      PDF_SPOOLID              = PDFSPOOLID
    TABLES
      PDF                      = PDF
    EXCEPTIONS
      ERR_NO_OTF_SPOOLJOB      = 1
      ERR_NO_SPOOLJOB          = 2
      ERR_NO_PERMISSION        = 3
      ERR_CONV_NOT_POSSIBLE    = 4
      ERR_BAD_DSTDEVICE        = 5
      USER_CANCELLED           = 6
      ERR_SPOOLERROR           = 7
      ERR_TEMSEERROR           = 8
      ERR_BTCJOB_OPEN_FAILED   = 9
      ERR_BTCJOB_SUBMIT_FAILED = 10
      ERR_BTCJOB_CLOSE_FAILED  = 11.
  RC = SY-SUBRC.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.


  CONCATENATE 'c:\IC_' 'B28' SY-DATUM+2(6) '.pdf'
              into W_FILE.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      BIN_FILESIZE            = NUMBYTES
      FILENAME                = W_FILE
      FILETYPE                = 'BIN'
*   IMPORTING
*     FILELENGTH              =
    TABLES
      DATA_TAB                = PDF
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1
      FILE_WRITE_ERROR        = 2
      INVALID_FILESIZE        = 3
      INVALID_TYPE            = 4
      NO_BATCH                = 5
      UNKNOWN_ERROR           = 6
      INVALID_TABLE_WIDTH     = 7
      GUI_REFUSE_FILETRANSFER = 8
      CUSTOMER_ERROR          = 9
      OTHERS                  = 10.
  RC = SY-SUBRC.
ENDFORM.                    " CALL_SM
