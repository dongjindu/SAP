*&---------------------------------------------------------------------*
*& Report  ZRIMINAMT                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : PO ITEM 별 비용 현황                                  *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2002.07.11                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : PO 별로 원가금액(비용실적)을 DISPLAY 한다.            *
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMINAMT     MESSAGE-ID ZIM
                      LINE-SIZE 103
                      NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          ZRIMINTOP.

DATA : BEGIN OF   IT_TAB OCCURS 0,
       EBELN      LIKE EKBZ-EBELN,           " PO.
       EBELP      LIKE EKBZ-EBELP,           " PO ITEM.
       ZFCSTGRP   LIKE ZTBDIV-ZFCSTGRP,      " 비용그룹.
       ZFCD       LIKE ZTBDIV-ZFCD,          " 비용코드.
       ZFCDNM     LIKE ZTIMIMG08-ZFCDNM,     " 비용명.
       MATNR      LIKE ZTBDIV-MATNR,         " 자재.
       MAKTX      LIKE MAKT-MAKTX,           " 자재명.
       MENGE      LIKE ZTBDIV-MENGE,         " 비용수량.
       GRMENGE    LIKE ZTIVHSTIT-GRMENGE,    " 입고수량.
       MEINS      LIKE ZTBDIV-MEINS,         " 수량단위.
       WRBTR      LIKE ZTBDIV-WRBTR,         " 발생비용금액.
       WAERS      LIKE ZTBDIV-WAERS,         " 통화.
       INAMT      LIKE ZTBDIV-ZFAMT.         " 미착금액.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS    FOR ZTREQHD-BUKRS NO-EXTENSION
                                                NO INTERVALS,
                   S_EBELN    FOR ZTREQIT-EBELN,   " Vendor
                   S_EBELP    FOR ZTREQIT-EBELP,   " 지불?
                   S_MATNR    FOR ZTREQIT-MATNR,
                   S_REQTY    FOR ZTREQHD-ZFREQTY, " Terms Of Payment
                   S_LIFNR    FOR ZTREQHD-LIFNR,
                   S_EKORG    FOR EKKO-EKORG,
                   S_EKGRP    FOR EKKO-EKGRP,
                   S_WERKS    FOR ZTREQHD-ZFWERKS,
                   S_DATE     FOR ZTIVHST-BUDAT.
SELECTION-SCREEN END OF BLOCK B1.

*>> 초기값 SETTING.
INITIALIZATION.                          " 초기값 SETTING
  SET  TITLEBAR  'ZIMY11'.               " GUI TITLE  SETTING
* Title Text Write
TOP-OF-PAGE.
   PERFORM P1000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> 수입 CONFIGURATION CHECK
   PERFORM P1000_CONFIG_CHECK   USING  W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.   EXIT. ENDIF.

*>> DATA SELECT!
   PERFORM P2000_READ_TEXT      USING  W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.  EXIT.  ENDIF.

*-----------------------------------------------------------------------
* END OF SELECTION ?
*-----------------------------------------------------------------------
END-OF-SELECTION.
   PERFORM P3000_DATA_WRITE     USING  W_ERR_CHK.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.

   DATA : L_TEXT(30).
   GET CURSOR FIELD L_TEXT.
   CASE  L_TEXT.
      WHEN 'IT_TAB-EBELN'   OR  'IT_TAB-EBELP'.
           SET PARAMETER ID 'BES'  FIELD IT_TAB-EBELN.
           CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
   ENDCASE.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
*------- Abbrechen (CNCL) ----------------------------------------------
      WHEN 'CNCL'.
         SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
      WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
      WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
      WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
      WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
      WHEN 'WESU'.
      WHEN OTHERS.
   ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFCSTMD NE 'S' AND ZTIMIMG00-ZFCSTMD NE 'P'.
     W_ERR_CHK = 'Y'.   MESSAGE S573.   EXIT.
  ENDIF.

ENDFORM.                    " P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_READ_TEXT USING    W_ERR_CHK.

   REFRESH : IT_TAB, IT_REQ.
   MOVE  'N'      TO  W_ERR_CHK.

*>> PO NUMBER GET!
   PERFORM P3000_READ_REDATA.
   IF W_ERR_CHK EQ 'Y'.
      MESSAGE  S738.
      EXIT.
   ENDIF.

*>> 비용 관련 자료 GET!
   PERFORM P3000_READ_CSTDATA.
   IF W_ERR_CHK EQ 'Y'.
      MESSAGE  S738.
      EXIT.
   ENDIF.

ENDFORM.                    " P2000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_READ_REDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_READ_REDATA.

*>> 수입의뢰 된 PO 자료 SELECT!
   SELECT  B~EBELN  B~EBELP SUM( B~MENGE ) AS MENGE
                            MAX( B~MEINS ) AS MEINS
   INTO    CORRESPONDING FIELDS OF TABLE IT_REQ
   FROM    ZTREQHD  AS  A  INNER  JOIN  ZTREQIT  AS  B
   ON      A~ZFREQNO    EQ   B~ZFREQNO
   WHERE   B~EBELN      IN   S_EBELN
   AND     A~BUKRS      IN   S_BUKRS
   AND     B~EBELP      IN   S_EBELP
   AND     B~MATNR      IN   S_MATNR
   AND     A~ZFREQTY    IN   S_REQTY
   AND     A~LIFNR      IN   S_LIFNR
   GROUP BY
           B~EBELN B~EBELP.

   CLEAR  W_LINE.
   DESCRIBE TABLE IT_REQ LINES W_LINE.
   IF W_LINE EQ 0.
      MOVE  'Y'   TO  W_ERR_CHK.
      EXIT.
   ENDIF.

*>> 입고가 되었거나 분할입고 인 자료 SELECT.
   CLEAR : W_MENGE.
   LOOP  AT  IT_REQ.
      W_TABIX  =  SY-TABIX.

      " 입고기간 CHECK.
      SELECT COUNT( * )      INTO  W_COUNT
      FROM   ZTIVHST  AS  A  INNER JOIN  ZTIVHSTIT AS B
      ON     A~ZFIVNO        EQ    B~ZFIVNO
      AND    A~ZFIVHST       EQ    B~ZFIVHST
      WHERE  B~EBELN         EQ    IT_REQ-EBELN
      AND    B~EBELP         EQ    IT_REQ-EBELP
      AND    A~BUDAT         IN    S_DATE
      AND    B~ZFGRST        EQ    'Y'.

      IF W_COUNT IS INITIAL OR SY-SUBRC NE 0.
         DELETE  IT_REQ  INDEX  W_TABIX.
         CONTINUE.
      ENDIF.

      " 입고수량 CHECK.
      SELECT SUM( GRMENGE )  INTO  W_MENGE
      FROM   ZTIVHSTIT
      WHERE  EBELN           EQ    IT_REQ-EBELN
      AND    EBELP           EQ    IT_REQ-EBELP
      AND    ZFGRST          EQ    'Y' .

      IF SY-SUBRC NE 0 OR W_MENGE EQ 0.
         DELETE  IT_REQ  INDEX  W_TABIX.
         CONTINUE.
      ENDIF.

      MOVE  :  W_MENGE  TO  IT_REQ-GRMENGE.
      MODIFY  IT_REQ  INDEX  W_TABIX.
   ENDLOOP.

   CLEAR  W_LINE.
   DESCRIBE TABLE IT_REQ LINES W_LINE.
   IF W_LINE EQ 0.
      MOVE  'Y'   TO  W_ERR_CHK.
      EXIT.
   ENDIF.

ENDFORM.                    " P3000_READ_REDATA
*&---------------------------------------------------------------------*
*&      Form  P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ 비용 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /                                          SY-VLINE NO-GAP,
            '자재                     '      NO-GAP,
            '             '                  NO-GAP, SY-VLINE NO-GAP,
            'P/O - Item      '               NO-GAP, SY-VLINE NO-GAP,
            '비용                '           NO-GAP, SY-VLINE NO-GAP,
            '            금액        '       NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.                    " P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_READ_CSTDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_READ_CSTDATA.

*>> 수입의뢰 비용 POSTING 자료 SELECT!
   SELECT B~EBELN  B~EBELP  B~ZFCSTGRP B~ZFCD B~MATNR B~MENGE
          B~MEINS  B~WRBTR  B~WAERS
   INTO CORRESPONDING FIELDS OF TABLE IT_TAB
   FROM    ZTBKPF       AS  A  INNER  JOIN  ZTBDIV  AS  B
   ON      A~BUKRS      EQ  B~BUKRS
   AND     A~GJAHR      EQ  B~GJAHR
   AND     A~BELNR      EQ  B~BELNR
   FOR     ALL ENTRIES  IN  IT_REQ
   WHERE   B~EBELN      EQ  IT_REQ-EBELN
   AND     B~EBELP      EQ  IT_REQ-EBELP
   AND     A~ZFPOSYN    EQ  'Y'.

   CLEAR  W_LINE.
   DESCRIBE TABLE IT_TAB LINES W_LINE.
   IF W_LINE EQ 0.
      MOVE  'Y'   TO  W_ERR_CHK.
      EXIT.
   ENDIF.

*>> 원가 반영 금액 COMPUTE.
   CLEAR : W_TABIX,  IT_TAB.
   LOOP  AT  IT_TAB.
      W_TABIX  =  SY-TABIX.

      READ  TABLE  IT_REQ  WITH KEY  EBELN  =  IT_TAB-EBELN
                                     EBELP  =  IT_TAB-EBELP.

      " 비용 COMPUTE.
      IF IT_REQ-GRMENGE  GE  IT_TAB-MENGE.
         MOVE  IT_TAB-WRBTR   TO  IT_TAB-INAMT.
      ELSE.
         IT_TAB-INAMT  =  ( IT_REQ-GRMENGE / IT_TAB-MENGE )
                                           * IT_TAB-WRBTR.
         IT_TAB-INAMT  =  IT_TAB-WRBTR - IT_TAB-INAMT.
      ENDIF.
      MOVE   IT_REQ-GRMENGE   TO   IT_TAB-GRMENGE.

      " 자재명 GET.
      CLEAR : MAKT.
      SELECT SINGLE * FROM  MAKT
             WHERE  MATNR   EQ   IT_TAB-MATNR
             AND    SPRAS   EQ   SY-LANGU.

      " 비용명 GET.
      CLEAR : ZTIMIMG08.
      SELECT SINGLE * FROM  ZTIMIMG08
             WHERE  ZFCDTY  EQ    IT_TAB-ZFCSTGRP
             AND    ZFCD    EQ    IT_TAB-ZFCD.

      MOVE  :  MAKT-MAKTX         TO    IT_TAB-MAKTX,
               ZTIMIMG08-ZFCDNM   TO    IT_TAB-ZFCDNM.

      MODIFY  IT_TAB  INDEX  W_TABIX.

   ENDLOOP.

*>> 물대비용 GET.
   CLEAR : SV_EBELN, SV_EBELP.
   SORT IT_TAB BY EBELN EBELP.

   LOOP  AT  IT_TAB.

      IF SV_EBELN NE IT_TAB-EBELN OR SV_EBELP NE IT_TAB-EBELP.

         IF SY-TABIX NE 1.

            SELECT  SUM( B~ZFIVAMK )  INTO   IT_TAB_AMT-INAMT
            FROM    ZTCIVHD  AS  A  INNER JOIN  ZTCIVIT  AS  B
            ON      A~ZFCIVRN       EQ    B~ZFCIVRN
            WHERE   A~ZFIVST        EQ    'Y'
            AND     B~EBELN         EQ    SV_EBELN
            AND     B~EBELP         EQ    SV_EBELP.

            IF SY-SUBRC NE 0 OR IT_TAB_AMT-INAMT EQ 0.
               CONTINUE.
            ENDIF.

            MOVE  :  SV_EBELN         TO    IT_TAB_AMT-EBELN,
                     SV_EBELP         TO    IT_TAB_AMT-EBELP,
                     '000'            TO    IT_TAB_AMT-ZFCSTGRP,
                     '000'            TO    IT_TAB_AMT-ZFCD,
                     '물대'           TO    IT_TAB_AMT-ZFCDNM,
                     SV_MATNR         TO    IT_TAB_AMT-MATNR,
                     SV_MAKTX         TO    IT_TAB_AMT-MAKTX,
                     'KRW'            TO    IT_TAB_AMT-WAERS.

            APPEND  IT_TAB_AMT.
         ENDIF.

         MOVE : IT_TAB-EBELN  TO  SV_EBELN,
                IT_TAB-EBELP  TO  SV_EBELP,
                IT_TAB-MATNR  TO  SV_MATNR,
                IT_TAB-MAKTX  TO  SV_MAKTX.

      ENDIF.

   ENDLOOP.

* 물대 -> IT_TAB.
  LOOP  AT  IT_TAB_AMT.
     MOVE-CORRESPONDING  IT_TAB_AMT  TO  IT_TAB.
     APPEND  IT_TAB.
  ENDLOOP.

* 자재별 비용코드별 SUM .
   SORT  IT_TAB  BY  MATNR  ZFCSTGRP  ZFCD.

   LOOP  AT  IT_TAB.

      IF SV_MATNR NE IT_TAB-MATNR  OR  SV_CSTGRP  NE  IT_TAB-ZFCSTGRP OR
         SV_ZFCD  NE IT_TAB-ZFCD.

         IF SY-TABIX  NE  1.
            PERFORM  P3000_IT_TO_SUM.
         ENDIF.

         MOVE : IT_TAB-MATNR     TO  SV_MATNR,
                IT_TAB-ZFCSTGRP  TO  SV_CSTGRP,
                IT_TAB-ZFCD      TO  SV_ZFCD,
                IT_TAB-MAKTX     TO  SV_MAKTX,
                IT_TAB-ZFCDNM    TO  SV_CDNM,
                IT_TAB-WAERS     TO  SV_WAERS.
         CLEAR  SV_AMT.

      ENDIF.

      SV_AMT  =  SV_AMT  +  IT_TAB-INAMT.

      AT LAST.
         PERFORM  P3000_IT_TO_SUM.
      ENDAT.

   ENDLOOP.

* 비용코드별 SUM.
   SORT  IT_TAB_SUM BY ZFCSTGRP ZFCD.
   CLEAR : SV_CSTGRP, SV_ZFCD.
   LOOP  AT  IT_TAB_SUM.

      IF SV_CSTGRP NE  IT_TAB_SUM-ZFCSTGRP OR
         SV_ZFCD NE IT_TAB_SUM-ZFCD.

         IF SY-TABIX NE 1.
            PERFORM  P3000_IT_TO_TOTAL.
         ENDIF.

         MOVE : IT_TAB_SUM-ZFCSTGRP  TO  SV_CSTGRP,
                IT_TAB_SUM-ZFCD      TO  SV_ZFCD,
                IT_TAB_SUM-ZFCDNM    TO  SV_CDNM,
                IT_TAB_SUM-WAERS     TO  SV_WAERS.
         CLEAR  SV_AMT.

      ENDIF.

      SV_AMT = SV_AMT + IT_TAB_SUM-INAMT.

      AT LAST.
         PERFORM  P3000_IT_TO_TOTAL.
      ENDAT.
   ENDLOOP.

ENDFORM.                    " P3000_READ_CSTDATA
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

   CLEAR : SV_MATNR, SV_EBELN, SV_EBELP.
   SORT  IT_TAB  BY  MATNR EBELN  EBELP ZFCSTGRP ZFCD.

   CLEAR  :  IT_TAB.

   MOVE  'N'    TO  W_ERR_CHK.
   SET PF-STATUS 'ZIMY11'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMY11'.           " GUI TITLE SETTING..

   LOOP  AT  IT_TAB.

      " 자재별 비용코드별 SUM WRITE.
      IF SV_MATNR NE IT_TAB-MATNR.
         IF SY-TABIX NE 1.
            PERFORM  P3000_SUM_WRITE USING SV_MATNR.
         ENDIF.
         MOVE  : IT_TAB-MATNR  TO  SV_MATNR,
                 IT_TAB-EBELN  TO  SV_EBELN,
                 IT_TAB-EBELP  TO  SV_EBELP.

         SV_CHK = '1'.
      ENDIF.

      IF SV_EBELN NE IT_TAB-EBELN.
         IF SY-TABIX EQ 1.
            SV_CHK = '1'.
         ELSE.
            SV_CHK = '2'.
         ENDIF.
         MOVE : IT_TAB-EBELN  TO  SV_EBELN,
                IT_TAB-EBELP  TO  SV_EBELP.
      ENDIF.

      IF SV_EBELP NE IT_TAB-EBELP.
         IF SY-TABIX EQ 1.
            SV_CHK = '1'.
         ELSE.
            SV_CHK = '2'.
         ENDIF.
         MOVE : IT_TAB-EBELP  TO  SV_EBELP.
      ENDIF.

*>> LINE WRITE.
      PERFORM   P3000_LINE_WRITE.
      CLEAR  SV_CHK.

      AT LAST.
         PERFORM P3000_SUM_WRITE USING SV_MATNR.
         PERFORM P3000_TOTAL_WRITE.
      ENDAT.

   ENDLOOP.


ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE                NO-GAP.
  IF SV_CHK  EQ  '1'.
     WRITE : IT_TAB-MATNR       NO-GAP,
             (20) IT_TAB-MAKTX  NO-GAP,
             SY-VLINE           NO-GAP,
             IT_TAB-EBELN       NO-GAP,
             '-'                NO-GAP,
             IT_TAB-EBELP       NO-GAP,
             SY-VLINE           NO-GAP.
  ELSEIF SV_CHK EQ '2'.
     WRITE : '                ' NO-GAP,
             '                ' NO-GAP,
             '      '           NO-GAP,
             SY-VLINE           NO-GAP,
             IT_TAB-EBELN       NO-GAP,
             '-'                NO-GAP,
             IT_TAB-EBELP       NO-GAP,
             SY-VLINE           NO-GAP.
  ELSE.
  WRITE : '                ' NO-GAP,
             '                ' NO-GAP,
             '      '           NO-GAP,
             SY-VLINE           NO-GAP,
             '                ' NO-GAP,
             SY-VLINE           NO-GAP.
  ENDIF.

  WRITE : (20)IT_TAB-ZFCDNM     NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-INAMT CURRENCY IT_TAB-WAERS NO-GAP,
          IT_TAB-WAERS          NO-GAP,
          SY-VLINE              NO-GAP.

   HIDE  IT_TAB.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_SUM_WRITE USING P_MATNR.

  CLEAR : W_CNT, SV_TOT.

  WRITE SY-ULINE.

  LOOP  AT  IT_TAB_SUM  WHERE  MATNR  EQ  P_MATNR.

     W_CNT = W_CNT + 1.
     W_MOD = W_CNT MOD 2.

     IF W_MOD EQ 0.
        FORMAT RESET.
        FORMAT COLOR COL_TOTAL INTENSIFIED ON.
     ELSE.
        FORMAT RESET.
        FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
     ENDIF.

     IF W_CNT EQ 1.
        WRITE : / SY-VLINE              NO-GAP,
                '                  '    NO-GAP,
                '자재별 비용 소계    '  NO-GAP,
                '           '           NO-GAP,
                '      '                NO-GAP.
     ELSE.
        WRITE : / SY-VLINE              NO-GAP,
                '                    '  NO-GAP,
                '                  '    NO-GAP,
                '          '            NO-GAP,
                '       '               NO-GAP.
     ENDIF.

     WRITE : SY-VLINE                NO-GAP,
             (20)IT_TAB_SUM-ZFCDNM   NO-GAP,
             SY-VLINE                NO-GAP,
             IT_TAB_SUM-INAMT  CURRENCY  IT_TAB_SUM-WAERS  NO-GAP,
             IT_TAB_SUM-WAERS        NO-GAP,
             SY-VLINE                NO-GAP.

     SV_TOT  =  SV_TOT + IT_TAB_SUM-INAMT.

  ENDLOOP.

  " 자재별 비용 TOTAL WRITE.
     FORMAT RESET.
     FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

     WRITE : / SY-VLINE              NO-GAP,
            '                  '    NO-GAP,
            '자재별 소계        '   NO-GAP,
            '           '           NO-GAP,
            '       '               NO-GAP,
            SY-VLINE                NO-GAP,
            '                    '  NO-GAP,
            SY-VLINE                NO-GAP,
            SV_TOT  CURRENCY  'KRW' NO-GAP,
            'KRW  '                 NO-GAP,
             SY-VLINE               NO-GAP.

  IF W_CNT GT 0.
     WRITE SY-ULINE.
  ENDIF.

ENDFORM.                    " P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_IT_TO_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_IT_TO_SUM.

   CLEAR : IT_TAB_SUM.

   MOVE : SV_MATNR     TO  IT_TAB_SUM-MATNR,
          SV_CSTGRP    TO  IT_TAB_SUM-ZFCSTGRP,
          SV_ZFCD      TO  IT_TAB_SUM-ZFCD,
          SV_AMT       TO  IT_TAB_SUM-INAMT,
          SV_MAKTX     TO  IT_TAB_SUM-MAKTX,
          SV_CDNM      TO  IT_TAB_SUM-ZFCDNM,
          SV_WAERS     TO  IT_TAB_SUM-WAERS.
   APPEND  IT_TAB_SUM.

ENDFORM.                    " P3000_IT_TO_SUM
*&---------------------------------------------------------------------*
*&      Form  P3000_IT_TO_TOTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_IT_TO_TOTAL.

   CLEAR : IT_TAB_TOTAL.

   MOVE : SV_CSTGRP    TO  IT_TAB_TOTAL-ZFCSTGRP,
          SV_ZFCD      TO  IT_TAB_TOTAL-ZFCD,
          SV_AMT       TO  IT_TAB_TOTAL-INAMT,
          SV_CDNM      TO  IT_TAB_TOTAL-ZFCDNM,
          SV_WAERS     TO  IT_TAB_TOTAL-WAERS.
   APPEND  IT_TAB_TOTAL.

ENDFORM.                    " P3000_IT_TO_TOTAL
*&---------------------------------------------------------------------*
*&      Form  P3000_TOTAL_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TOTAL_WRITE.

  CLEAR : W_CNT, SV_TOT.

  LOOP  AT  IT_TAB_TOTAL.

     W_CNT = W_CNT + 1.
     W_MOD = W_CNT MOD 2.

     IF W_MOD EQ 0.
        FORMAT RESET.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
     ELSE.
        FORMAT RESET.
        FORMAT COLOR COL_NORMAL INTENSIFIED ON.
     ENDIF.

     IF W_CNT EQ 1.
        WRITE : / SY-VLINE              NO-GAP,
                '                  '    NO-GAP,
                '비용 합계    '         NO-GAP,
                '           '           NO-GAP,
                '             '         NO-GAP.
     ELSE.
        WRITE : / SY-VLINE              NO-GAP,
                '                    '  NO-GAP,
                '                  '    NO-GAP,
                '          '            NO-GAP,
                '       '               NO-GAP.
     ENDIF.

     WRITE : SY-VLINE                  NO-GAP,
             (20)IT_TAB_TOTAL-ZFCDNM   NO-GAP,
             SY-VLINE                  NO-GAP,
             IT_TAB_TOTAL-INAMT  CURRENCY  IT_TAB_TOTAL-WAERS  NO-GAP,
             IT_TAB_TOTAL-WAERS        NO-GAP,
             SY-VLINE                  NO-GAP.

     SV_TOT  =  SV_TOT + IT_TAB_TOTAL-INAMT.

  ENDLOOP.

  " 자재별 비용 TOTAL WRITE.
     FORMAT RESET.
     FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.

     WRITE : / SY-VLINE              NO-GAP,
            '                  '    NO-GAP,
            '합계               '   NO-GAP,
            '           '           NO-GAP,
            '       '               NO-GAP,
            SY-VLINE                NO-GAP,
            '                    '  NO-GAP,
            SY-VLINE                NO-GAP,
            SV_TOT  CURRENCY  'KRW' NO-GAP,
            'KRW  '                 NO-GAP,
             SY-VLINE               NO-GAP.

  IF W_CNT GT 0.
     WRITE SY-ULINE.
  ENDIF.


ENDFORM.                    " P3000_TOTAL_WRITE
