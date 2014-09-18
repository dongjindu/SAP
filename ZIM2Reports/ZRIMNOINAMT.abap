*&---------------------------------------------------------------------*
*& Report  ZRIMNOINAMT                                                 *
*&---------------------------------------------------------------------*
*&  프로그램명 : PO ITEM 별 미착금액 현황                              *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2002.07.10                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : PO 별로 미착금액(비용미착)을 DISPLAY 한다.            *
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMCSTCHA    MESSAGE-ID ZIM
                      LINE-SIZE 164
                      NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          ZRIMNOINTOP.

DATA : BEGIN OF   IT_TAB OCCURS 0,
       EBELN      LIKE EKBZ-EBELN,           " PO.
       EBELP      LIKE EKBZ-EBELP,           " PO ITEM.
       ZFCSTGRP   LIKE ZTBDIV-ZFCSTGRP,      " 비용그룹.
       ZFCD       LIKE ZTBDIV-ZFCD,          " 비용코드.
       ZFCDNM     LIKE ZTIMIMG08-ZFCDNM,     " 비용명.
       MATNR      LIKE ZTBDIV-MATNR,         " 자재.
       MAKTX      LIKE MAKT-MAKTX,           " 자재명.
       OPAMT      LIKE ZTBDIV-WRBTR,         " 수입의뢰 비용.
       CUR        LIKE ZTREQHD-WAERS,        " CURRENCY.
       MENGE      LIKE ZTBDIV-MENGE,         " 비용수량.
       GRMENGE    LIKE ZTIVHSTIT-GRMENGE,    " 입고수량.
       GRAMT      LIKE ZTBDIV-WRBTR,         " 입고금액.
       REAMT      LIKE ZTBDIV-WRBTR,         " 미착잔액.
       MEINS      LIKE ZTBDIV-MEINS,         " 수량단위.
       WRBTR      LIKE ZTBDIV-WRBTR,         " 발생비용금액.
       WAERS      LIKE ZTBDIV-WAERS,         " 통화.
       NOAMT      LIKE ZTBDIV-ZFAMT.         " 미착금액.
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
                   S_WERKS    FOR ZTREQHD-ZFWERKS.
SELECTION-SCREEN END OF BLOCK B1.

*>> 초기값 SETTING.
INITIALIZATION.                          " 초기값 SETTING
  SET  TITLEBAR  'ZIMY10'.               " GUI TITLE  SETTING

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
   SELECT  B~EBELN  B~EBELP        SUM( B~MENGE ) AS MENGE
           MAX( B~MEINS ) AS MEINS MAX( A~WAERS ) AS CUR
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

*>> 입고가 안되었거나 분할입고 인 자료 SELECT.
   LOOP  AT  IT_REQ.
      W_TABIX  =  SY-TABIX.

      SELECT SUM( GRMENGE )  INTO  W_MENGE
      FROM   ZTIVHSTIT
      WHERE  EBELN           EQ    IT_REQ-EBELN
      AND    EBELP           EQ    IT_REQ-EBELP
      AND    ZFGRST          EQ    'Y' .

      IF SY-SUBRC EQ 0 AND W_MENGE GT 0.
         IF IT_REQ-MENGE  LE  W_MENGE.
            DELETE  IT_REQ  INDEX  W_TABIX.
            CONTINUE.
         ENDIF.
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
  WRITE : /70  '[ 미착 비용 현황 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /                                          SY-VLINE NO-GAP,
            'P/O - Item      '               NO-GAP, SY-VLINE NO-GAP,
            '자재                     '      NO-GAP,
            '             '                  NO-GAP, SY-VLINE NO-GAP,
            '비용                '           NO-GAP, SY-VLINE NO-GAP,
            '    비용미착금액        '       NO-GAP, SY-VLINE NO-GAP,
            'OPEN 통화'                      NO-GAP, SY-VLINE NO-GAP,
            '       OPEN 금액'               NO-GAP, SY-VLINE NO-GAP,
            '        입고금액'               NO-GAP, SY-VLINE NO-GAP,
            '            잔액'               NO-GAP, SY-VLINE NO-GAP.

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

*>> 미착금액 COMPUTE.
   CLEAR : W_TABIX,  IT_TAB.
   LOOP  AT  IT_TAB.
      W_TABIX  =  SY-TABIX.

      READ  TABLE  IT_REQ  WITH KEY  EBELN  =  IT_TAB-EBELN
                                     EBELP  =  IT_TAB-EBELP.

      " PO-ITEM DATA GET.
      CLEAR : EKPO.
      SELECT SINGLE *  FROM EKPO  WHERE EBELN EQ IT_TAB-EBELN
                                  AND   EBELP EQ IT_TAB-EBELP.

      MOVE  IT_REQ-MENGE   TO  IT_TAB-GRMENGE.

      " 미착비용만 SELECT.
      IF IT_REQ-GRMENGE  GT  0.
         IF IT_REQ-GRMENGE  EQ  IT_TAB-MENGE.
            DELETE  IT_TAB  INDEX  W_TABIX.
            CONTINUE.
         ENDIF.
      ENDIF.

      " 미착비용 COMPUTE.
      IF IT_REQ-GRMENGE  EQ  0.
         MOVE  IT_TAB-WRBTR   TO  IT_TAB-NOAMT.
      ELSE.
         IT_TAB-NOAMT  =  ( IT_REQ-GRMENGE / IT_TAB-MENGE )
                                           * IT_TAB-WRBTR.
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

      " OPEN 금액, 입고금액 COMPUTE.
      IF EKPO-BPUMN NE 0 AND EKPO-PEINH NE 0.
         IT_TAB-OPAMT = IT_REQ-MENGE   *  ( EKPO-BPUMZ / EKPO-BPUMN )
                                       *  ( EKPO-NETPR / EKPO-PEINH ).

         IT_TAB-GRAMT = IT_TAB-GRMENGE *  ( EKPO-BPUMZ / EKPO-BPUMN )
                                       *  ( EKPO-NETPR / EKPO-PEINH ).

      ENDIF.

      IT_TAB-REAMT = IT_TAB-OPAMT - IT_TAB-GRAMT.

      MOVE  :  IT_REQ-CUR         TO    IT_TAB-CUR,
               MAKT-MAKTX         TO    IT_TAB-MAKTX,
               ZTIMIMG08-ZFCDNM   TO    IT_TAB-ZFCDNM.

      MODIFY  IT_TAB  INDEX  W_TABIX.
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

   SORT  IT_TAB  BY  EBELN  EBELP.

   CLEAR  :  IT_TAB.

   MOVE  'N'    TO  W_ERR_CHK.
   SET PF-STATUS 'ZIMY10'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMY10'.           " GUI TITLE SETTING..

   LOOP  AT  IT_TAB.

      IF SV_EBELN NE IT_TAB-EBELN.
         IF SY-TABIX NE 1.
            PERFORM  P3000_SUM_WRITE.
            SV_CHK = 'X'.
            CLEAR : SV_AMT, SV_WAERS, SV_CUR,
                    SV_OPAMT, SV_GRAMT, SV_REAMT.
         ENDIF.
         MOVE : IT_TAB-EBELN  TO  SV_EBELN,
                IT_TAB-WAERS  TO  SV_WAERS.

         SV_OPAMT  =  SV_OPAMT  +  IT_TAB-OPAMT.
         SV_GRAMT  =  SV_GRAMT  +  IT_TAB-GRAMT.
         SV_REAMT  =  SV_REAMT  +  IT_TAB-REAMT.

         TOT_OPAMT =  TOT_OPAMT +  IT_TAB-OPAMT.
         TOT_GRAMT =  TOT_GRAMT +  IT_TAB-GRAMT.
         TOT_REAMT =  TOT_REAMT +  IT_TAB-REAMT.

      ELSEIF SV_EBELP NE IT_TAB-EBELP.
         SV_OPAMT  =  SV_OPAMT  +  IT_TAB-OPAMT.
         SV_GRAMT  =  SV_GRAMT  +  IT_TAB-GRAMT.
         SV_REAMT  =  SV_REAMT  +  IT_TAB-REAMT.

         TOT_OPAMT =  TOT_OPAMT +  IT_TAB-OPAMT.
         TOT_GRAMT =  TOT_GRAMT +  IT_TAB-GRAMT.
         TOT_REAMT =  TOT_REAMT +  IT_TAB-REAMT.
      ENDIF.

      IF SV_EBELP NE IT_TAB-EBELP.
         SV_CHK = 'X'.
         MOVE : IT_TAB-EBELP  TO  SV_EBELP.
      ENDIF.


*>> LINE WRITE.
      PERFORM   P3000_LINE_WRITE.
      CLEAR  SV_CHK.

      SV_AMT  =  SV_AMT   +  IT_TAB-NOAMT.
      TOT_AMT =  TOT_AMT  +  IT_TAB-NOAMT.

      MOVE  IT_TAB-WAERS  TO  SV_WAERS.
      MOVE  IT_TAB-CUR    TO  SV_CUR.
      AT LAST.
         PERFORM P3000_SUM_WRITE.
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
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE                NO-GAP.
  IF SV_CHK  EQ  'X'.
     WRITE : IT_TAB-EBELN       NO-GAP,
             '-'                NO-GAP,
             IT_TAB-EBELP       NO-GAP,
             SY-VLINE           NO-GAP,
             IT_TAB-MATNR       NO-GAP,
             (20) IT_TAB-MAKTX  NO-GAP,
             SY-VLINE           NO-GAP.
  ELSE.
     WRITE : '                ' NO-GAP,
             SY-VLINE           NO-GAP,
             '                ' NO-GAP,
             '                ' NO-GAP,
             '      '           NO-GAP,
             SY-VLINE           NO-GAP.
  ENDIF.

  WRITE : (20)IT_TAB-ZFCDNM     NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-NOAMT CURRENCY IT_TAB-WAERS NO-GAP,
          IT_TAB-WAERS          NO-GAP,
          SY-VLINE              NO-GAP,
          '  '                  NO-GAP,
          IT_TAB-CUR            NO-GAP,
          '  '                  NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-OPAMT CURRENCY IT_TAB-CUR   NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-GRAMT CURRENCY IT_TAB-CUR   NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-REAMT CURRENCY IT_TAB-CUR   NO-GAP,
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
FORM P3000_SUM_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE                NO-GAP,
          '                '  COLOR COL_NORMAL INTENSIFIED ON NO-GAP.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE : SY-VLINE                NO-GAP,
          '소      계'            NO-GAP,
          '               '       NO-GAP,
          '                '      NO-GAP,
          '          '            NO-GAP,
          '        '              NO-GAP,
          SY-VLINE                NO-GAP,
          SV_AMT    CURRENCY      SV_WAERS  NO-GAP,
          SV_WAERS                NO-GAP,
          SY-VLINE                NO-GAP,
          '  '                    NO-GAP,
          SV_CUR                  NO-GAP,
          '  '                    NO-GAP,
          SY-VLINE                NO-GAP,
          SV_OPAMT   CURRENCY     SV_CUR    NO-GAP,
          SY-VLINE                NO-GAP,
          SV_GRAMT   CURRENCY     SV_CUR    NO-GAP,
          SY-VLINE                NO-GAP,
          SV_REAMT   CURRENCY     SV_CUR    NO-GAP,
          SY-VLINE                NO-GAP.

  WRITE SY-ULINE.

ENDFORM.                    " P3000_SUM_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TOTAL_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_GROUP INTENSIFIED OFF.

  WRITE:/ SY-VLINE                NO-GAP,
          '                 '     NO-GAP,
          '합      계'            NO-GAP,
          '               '       NO-GAP,
          '                '      NO-GAP,
          '          '            NO-GAP,
          '        '              NO-GAP,
          SY-VLINE                NO-GAP,
          TOT_AMT    CURRENCY     SV_WAERS  NO-GAP,
          SV_WAERS                NO-GAP,
          SY-VLINE                NO-GAP,
          '  '                    NO-GAP,
          SV_CUR                  NO-GAP,
          '  '                    NO-GAP,
          SY-VLINE                NO-GAP,
          TOT_OPAMT  CURRENCY     SV_CUR    NO-GAP,
          SY-VLINE                NO-GAP,
          TOT_GRAMT  CURRENCY     SV_CUR    NO-GAP,
          SY-VLINE                NO-GAP,
          TOT_REAMT  CURRENCY     SV_CUR    NO-GAP,
          SY-VLINE                NO-GAP.
  WRITE SY-ULINE.

ENDFORM.                    " P3000_SUM_WRITE
