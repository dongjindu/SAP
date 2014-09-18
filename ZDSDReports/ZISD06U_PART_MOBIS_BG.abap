************************************************************************
* Program Name      : ZISD06U_PART_MOBIS_BG
* Author            : jun ho choi
* Creation Date     : 2003.11.13.
* Specifications By : jun ho choi
* Pattern           : 5-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Uploading Part files and storage within SAP
*                          Costum Tables.

*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZISD06U_PART_MOBIS NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMSD.


*
TABLES : ZTSD_PART_INF,
         ZTSD_PART_HST,
         ZTSD_PART_PRC,
         ZTSD_PART_SUP,
         KNA1,
         ADRC.

CONSTANTS: LOCAL_CURRENCY(3) TYPE C VALUE 'USD'.
*
DATA : BEGIN OF IT_INF OCCURS 0,
       RECORD(61),
       END OF IT_INF.

DATA : BEGIN OF IT_HST OCCURS 0,
       RECORD(37),
       END OF IT_HST.

DATA : BEGIN OF IT_PRC OCCURS 0,
       RECORD(58),
       END OF IT_PRC.

DATA : BEGIN OF IT_SUP OCCURS 0,
       RECORD(33),
       END OF IT_SUP.

DATA : W_CNT TYPE I,
       W_VEND(6),
       W_DATE LIKE SY-DATUM.

FIELD-SYMBOLS : <FS>.
DATA : FIELD(20).


*
DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD06_01'.

DATA : BEGIN OF IT_LIST OCCURS 0,
       FILE1 LIKE RLGRAP-FILENAME,
       FILE2 LIKE RLGRAP-FILENAME,
       FILE3 LIKE RLGRAP-FILENAME,
       FILE4 LIKE RLGRAP-FILENAME,
       END OF IT_LIST.


*
START-OF-SELECTION.
  PERFORM UPLOAD_FILE.


*
END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_FILE.
  DATA : DSN(30).

  IMPORT IT_LIST FROM DATABASE INDX(ZS) ID VARIANT.

  READ TABLE IT_LIST INDEX 1.

  IF NOT IT_LIST-FILE1 IS INITIAL.
  DSN = IT_LIST-FILE1.

  OPEN DATASET DSN IN TEXT MODE FOR INPUT.

  DO.
    READ DATASET DSN INTO IT_INF-RECORD.
    IF SY-SUBRC = 0.
      APPEND IT_INF.
    ELSE.
*      IF SY-SUBRC = 4.
*        WRITE:/ IT_INF-RECORD.
        EXIT.
*      ENDIF.
    ENDIF.
  ENDDO.

  CLOSE DATASET DSN.
  IF SY-SUBRC EQ 0.
    MESSAGE I000 WITH TEXT-M02 '(Infomation)'.
    PERFORM PROCESS_INF.
  ENDIF.
  ENDIF.

  IF NOT IT_LIST-FILE2 IS INITIAL.
  DSN = IT_LIST-FILE2.

  OPEN DATASET DSN IN TEXT MODE FOR INPUT.

  DO.
    READ DATASET DSN INTO IT_HST-RECORD.
    IF SY-SUBRC = 0.
      APPEND IT_HST.
    ELSE.
*      IF SY-SUBRC = 4.
*        WRITE:/ IT_HST-RECORD.
        EXIT.
*      ENDIF.
    ENDIF.
  ENDDO.

  CLOSE DATASET DSN.
  IF SY-SUBRC EQ 0.
    MESSAGE I000 WITH TEXT-M02 '(History)'.
    PERFORM PROCESS_HST.
  ENDIF.
  ENDIF.

  IF NOT IT_LIST-FILE3 IS INITIAL.
  DSN = IT_LIST-FILE3.

  OPEN DATASET DSN IN TEXT MODE FOR INPUT.

  DO.
    READ DATASET DSN INTO IT_PRC-RECORD.
    IF SY-SUBRC = 0.
      APPEND IT_PRC.
    ELSE.
*      IF SY-SUBRC = 4.
*        WRITE:/ IT_PRC-RECORD.
        EXIT.
*      ENDIF.
    ENDIF.
  ENDDO.

  CLOSE DATASET DSN.
  IF SY-SUBRC EQ 0.
    MESSAGE I000 WITH TEXT-M02 '(Price)'.
    PERFORM PROCESS_PRC.
  ENDIF.
  ENDIF.

  IF NOT IT_LIST-FILE4 IS INITIAL.
  DSN = IT_LIST-FILE4.

  OPEN DATASET DSN IN TEXT MODE FOR INPUT.

  DO.
    READ DATASET DSN INTO IT_SUP-RECORD.
    IF SY-SUBRC = 0.
      APPEND IT_SUP.
    ELSE.
*      IF SY-SUBRC = 4.
*        WRITE:/ IT_SUP-RECORD.
        EXIT.
*      ENDIF.
    ENDIF.
  ENDDO.

  CLOSE DATASET DSN.
  IF SY-SUBRC EQ 0.
    MESSAGE I000 WITH TEXT-M02 '(Suply history)'.
    PERFORM PROCESS_SUP.
  ENDIF.
  ENDIF.
ENDFORM.                    " UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_INF
*&---------------------------------------------------------------------*
FORM PROCESS_INF.
  DESCRIBE TABLE IT_INF LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M03 '(Infomation)'.
  ELSE.
    LOOP AT IT_INF.
    SELECT SINGLE *
           FROM ZTSD_PART_INF
          WHERE ZCPTN EQ IT_INF-RECORD+0(15).
    IF SY-SUBRC = 0.
      ZTSD_PART_INF-ZUSE  = IT_INF-RECORD+15(1).
      ZTSD_PART_INF-ZPTNA = IT_INF-RECORD+16(45).
      MODIFY ZTSD_PART_INF.
    ELSE.
      ZTSD_PART_INF-ZCPTN = IT_INF-RECORD+0(15).
      ZTSD_PART_INF-ZUSE  = IT_INF-RECORD+15(1).
      ZTSD_PART_INF-ZPTNA = IT_INF-RECORD+16(45).
      ZTSD_PART_INF-ZERDA = SY-DATUM.
      ZTSD_PART_INF-ERZET = SY-UZEIT.
      ZTSD_PART_INF-ERNAM = SY-UNAME.
      INSERT ZTSD_PART_INF.
    ENDIF.
    ENDLOOP.
    MESSAGE I000 WITH TEXT-M07 '(Infomation)'.
  ENDIF.
ENDFORM.                    " PROCESS_INF
*&---------------------------------------------------------------------*
*&      Form  PROCESS_HST
*&---------------------------------------------------------------------*
FORM PROCESS_HST.
  DESCRIBE TABLE IT_HST LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M03 '(History)'.
  ELSE.
    LOOP AT IT_HST.
    CLEAR W_VEND.
    PERFORM GET_VEND USING IT_HST-RECORD+16(4).
    IF W_VEND IS INITIAL.
    MESSAGE I000 WITH 'Vendor not found' IT_HST-RECORD+16(4).
    ELSE.
    SELECT SINGLE *
           FROM ZTSD_PART_HST
          WHERE ZCPTN EQ IT_HST-RECORD+0(15)
          AND   ZSORC EQ IT_HST-RECORD+15(1)
          AND   ZVEND EQ W_VEND
          AND   ZEFFM EQ IT_HST-RECORD+20(8).
    IF SY-SUBRC = 0.
      ZTSD_PART_HST-ZEFTO = '99991231'. "IT_HST-RECORD+28(8).
      ZTSD_PART_HST-ZEFFG = IT_HST-RECORD+36(1).
      MODIFY ZTSD_PART_HST.
    ELSE.
      ZTSD_PART_HST-ZCPTN = IT_HST-RECORD+0(15).
      ZTSD_PART_HST-ZSORC = IT_HST-RECORD+15(1).
      ZTSD_PART_HST-ZVEND = W_VEND.
      ZTSD_PART_HST-ZEFFM = IT_HST-RECORD+20(8).
      ZTSD_PART_HST-ZEFTO = '99991231'. "IT_HST-RECORD+28(8).
      ZTSD_PART_HST-ZEFFG = IT_HST-RECORD+36(1).
      ZTSD_PART_HST-ZERDA = SY-DATUM.
      ZTSD_PART_HST-ERZET = SY-UZEIT.
      ZTSD_PART_HST-ERNAM = SY-UNAME.
      INSERT ZTSD_PART_HST.
      ""
      SELECT *
             FROM ZTSD_PART_HST
            WHERE ZCPTN EQ IT_HST-RECORD+0(15)
            AND   ZSORC EQ IT_HST-RECORD+15(1)
            AND   ZVEND EQ W_VEND
            AND   ZEFFM NE IT_HST-RECORD+20(8).
      ENDSELECT.
      IF SY-SUBRC = 0.
        W_DATE = IT_HST-RECORD+20(8).
        W_DATE = W_DATE - 1.
        ZTSD_PART_HST-ZEFTO = W_DATE.
        MODIFY ZTSD_PART_HST.
      ENDIF.
      ""
    ENDIF.
    ENDIF.
    ENDLOOP.
    MESSAGE I000 WITH TEXT-M07 '(History)'.
  ENDIF.
ENDFORM.                    " PROCESS_HST
*&---------------------------------------------------------------------*
*&      Form  PROCESS_PRC
*&---------------------------------------------------------------------*
FORM PROCESS_PRC.
  data: l_flag .

  DESCRIBE TABLE IT_PRC LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M03 '(Price)'.
  ELSE.
    LOOP AT IT_PRC.
    SELECT SINGLE *
           FROM ZTSD_PART_PRC
          WHERE ZCPTN EQ IT_PRC-RECORD+0(15)
          AND   ZSORC EQ IT_PRC-RECORD+15(1)
          AND   ZMOD  EQ IT_PRC-RECORD+16(5)
          AND   ZMYFM EQ IT_PRC-RECORD+21(4)
          AND   ZEFFM EQ IT_PRC-RECORD+25(8).
    IF SY-SUBRC = 0.
      ZTSD_PART_PRC-ZMYTO = IT_PRC-RECORD+33(4).
      ZTSD_PART_PRC-ZEFTO = '99991231'. "IT_PRC-RECORD+37(8).
      ZTSD_PART_PRC-ZEFFG = IT_PRC-RECORD+45(1).
      ZTSD_PART_PRC-ZCURR = IT_PRC-RECORD+46(3).
      ZTSD_PART_PRC-ZUNPR = IT_PRC-RECORD+49(9).
      ZTSD_PART_PRC-ZUNPR = ZTSD_PART_PRC-ZUNPR / 100.
      MODIFY ZTSD_PART_PRC.
    ELSE.
      ZTSD_PART_PRC-ZCPTN = IT_PRC-RECORD+0(15).
      ZTSD_PART_PRC-ZSORC = IT_PRC-RECORD+15(1).
      ZTSD_PART_PRC-ZMOD  = IT_PRC-RECORD+16(5).
      ZTSD_PART_PRC-ZMYFM = IT_PRC-RECORD+21(4).
      ZTSD_PART_PRC-ZEFFM = IT_PRC-RECORD+25(8).
      ZTSD_PART_PRC-ZMYTO = IT_PRC-RECORD+33(4).
      ZTSD_PART_PRC-ZEFTO = '99991231'. "IT_PRC-RECORD+37(8).
      ZTSD_PART_PRC-ZEFFG = IT_PRC-RECORD+45(1).
      ZTSD_PART_PRC-ZCURR = IT_PRC-RECORD+46(3).
      ZTSD_PART_PRC-ZUNPR = IT_PRC-RECORD+49(9).
      ZTSD_PART_PRC-ZUNPR = ZTSD_PART_PRC-ZUNPR / 100.

*-->requested by Mr.Ko changed by chris
*   do the currency conversion
    perform currency_coversion using ZTSD_PART_PRC-ZCURR
                                     local_currency
                                     ZTSD_PART_PRC-ZUNPR
                                     ZTSD_PART_PRC-ZEFFM
                                     L_FLAG.

      if l_flag ne 'X'.
        ZTSD_PART_PRC-ZCURR = LOCAL_CURRENCY.
      ENDIF.
*-->end of change on 06/22/2005

      ZTSD_PART_PRC-ZERDA = SY-DATUM.
      ZTSD_PART_PRC-ERZET = SY-UZEIT.
      ZTSD_PART_PRC-ERNAM = SY-UNAME.
      INSERT ZTSD_PART_PRC.
      ""
      SELECT *
             FROM ZTSD_PART_PRC
            WHERE ZCPTN EQ IT_PRC-RECORD+0(15)
            AND   ZSORC EQ IT_PRC-RECORD+15(1)
            AND   ZMOD  EQ IT_PRC-RECORD+16(5)
            AND   ZMYFM EQ IT_PRC-RECORD+21(4)
            AND   ZEFFM NE IT_PRC-RECORD+25(8).
      ENDSELECT.
      IF SY-SUBRC = 0.
        W_DATE = IT_PRC-RECORD+25(8).
        W_DATE = W_DATE - 1.
        ZTSD_PART_PRC-ZEFTO = W_DATE.
        MODIFY ZTSD_PART_PRC.
      ENDIF.
      ""
    ENDIF.
    ENDLOOP.
    IF L_FLAG = 'X'.
      MESSAGE I000 WITH 'No currency conversion rate exist'.
    ENDIF.
    MESSAGE I000 WITH TEXT-M07 '(Price)'.
  ENDIF.
ENDFORM.                    " PROCESS_PRC
*&---------------------------------------------------------------------*
*&      Form  PROCESS_SUP
*&---------------------------------------------------------------------*
FORM PROCESS_SUP.
  DESCRIBE TABLE IT_SUP LINES W_CNT.
  IF W_CNT = 0.
    MESSAGE I000 WITH TEXT-M03 '(Suply history)'.
  ELSE.
    LOOP AT IT_SUP.
    CLEAR W_VEND.
    PERFORM GET_VEND USING IT_SUP-RECORD+16(4).
    IF W_VEND IS INITIAL.
    MESSAGE I000 WITH 'Vendor not found' IT_SUP-RECORD+16(4).
    ELSE.
    SELECT SINGLE *
           FROM ZTSD_PART_SUP
          WHERE ZCPTN EQ IT_SUP-RECORD+0(15)
          AND   ZSORC EQ IT_SUP-RECORD+15(1)
          AND   ZVEND EQ W_VEND
          AND   ZSQYY EQ IT_SUP-RECORD+20(4). "6 YYYYMM
    IF SY-SUBRC = 0.
      CONCATENATE 'ZTSD_PART_SUP-ZQTY' IT_SUP-RECORD+24(2) INTO FIELD.
      ASSIGN  (FIELD) TO <FS>.
      <FS> = IT_SUP-RECORD+26(7).

      MODIFY ZTSD_PART_SUP.
    ELSE.
      ZTSD_PART_SUP-ZCPTN = IT_SUP-RECORD+0(15).
      ZTSD_PART_SUP-ZSORC = IT_SUP-RECORD+15(1).
      ZTSD_PART_SUP-ZVEND = W_VEND.
      ZTSD_PART_SUP-ZSQYY = IT_SUP-RECORD+20(4).

      CONCATENATE 'ZTSD_PART_SUP-ZQTY' IT_SUP-RECORD+24(2) INTO FIELD.
      ASSIGN  (FIELD) TO <FS>.
      <FS> = IT_SUP-RECORD+26(7).

      ZTSD_PART_SUP-ZERDA = SY-DATUM.
      ZTSD_PART_SUP-ERZET = SY-UZEIT.
      ZTSD_PART_SUP-ERNAM = SY-UNAME.
      INSERT ZTSD_PART_SUP.
    ENDIF.
    ENDIF.
    ENDLOOP.
    MESSAGE I000 WITH TEXT-M07 '(Suply history)'.
  ENDIF.
ENDFORM.                    " PROCESS_SUP
*&---------------------------------------------------------------------*
*&      Form  GET_VEND
*&---------------------------------------------------------------------*
FORM GET_VEND USING M_VEND.
  SELECT SINGLE *
         FROM ADRC
        WHERE SORT2 EQ M_VEND.
  CHECK SY-SUBRC = 0.
  SELECT SINGLE *
         FROM KNA1
        WHERE ADRNR EQ ADRC-ADDRNUMBER.
  CHECK SY-SUBRC = 0.
  W_VEND = KNA1-KUNNR.
ENDFORM.                    " GET_VEND
*&---------------------------------------------------------------------*
*&      Form  currency_coversion
*&---------------------------------------------------------------------*
*    convert the foreign currency into USD
*----------------------------------------------------------------------*
*      -->P_ZTSD_PART_PRC_ZCURR  text
*      -->P_ZTSD_PART_PRC_ZUNPR  text
*----------------------------------------------------------------------*
form currency_coversion using    p_fcurr
                                 p_lcurr
                                 p_amount
                                 p_date
                                 P_FLAG.
   DATA LT_TCURR LIKE TCURR OCCURS 0 WITH HEADER LINE.
   data: l_date like tcurr-gdatu.

   l_date = p_date.
   CLEAR: P_FLAG.

   if p_fCURR = P_LCURR.
     EXIT.
   ENDIF.

   SELECT *
     INTO corresponding fields of table lt_tcurr
     FROM TCURR
     WHERE KURST   = 'M'
       AND FCURR   = P_fCURR
       AND TCURR   = p_lcurr
       AND GDATU   gt l_date.


   IF SY-SUBRC = 0.
     sort lt_tcurr by gdatu .
     read table lt_tcurr index 1.
     if lt_tcurr-ukurs lt 0.
       P_AMOUNT = - P_AMOUNT / LT_TCURR-UKURS .
     else.
       P_AMOUNT = P_AMOUNT * LT_TCURR-UKURS .
     endif.
   ELSE.
     P_FLAG = 'X'.
   ENDIF.

endform.                    " currency_coversion
