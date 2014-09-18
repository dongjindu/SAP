*&---------------------------------------------------------------------
*& Report       ZRIMBKLCCSTDOC
*&---------------------------------------------------------------------
*&  프로그램명 : 은행별 L/C 관련 제비용 일계표.
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.10.10
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용]
*&
*&---------------------------------------------------------------------
REPORT ZRIMBKLCCSTDOC  MESSAGE-ID ZIM
                       LINE-SIZE 134
                       NO STANDARD PAGE HEADING.
TABLES: ZTREQHD,ZTREQST,ZTBKPF,ZTBSEG,ZTBL,ZTIDS,DD07T,
        ZTLG,ZTIMIMG08,LFA1, ZTIMIMG00.

*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       EBELN      LIKE    ZTREQHD-EBELN,      " PO  NO
       ZFOPNNO    LIKE    ZTREQST-ZFOPNNO,    " L/C NO
       ZFREQTY    LIKE    ZTREQHD-ZFREQTY,    " 결제구분.
       REQTY      LIKE    DD07T-DDTEXT,       " 결제구분.
       ZFOPNDT    LIKE    ZTREQST-ZFOPNDT,    " 개설일.
       ZFACDO     LIKE    ZTBKPF-ZFACDO,      " 회계번호.
       ZFRVSX     LIKE    ZTBKPF-ZFRVSX,      " 역기표..
       ZFCD       LIKE    ZTBSEG-ZFCD,        " 관리코드.
       ZFCDNM     LIKE    ZSBSEG-ZFCDNM,      " 관리코드명.
       WRBTR      LIKE    ZTBSEG-WRBTR,        "금액.
       MWSKZ      LIKE    ZTBKPF-MWSKZ,       " 세금코드.
       ZFSHNO     LIKE    ZTBL-ZFSHNO,        " 선적차수.
       BUKRS      LIKE    ZTBKPF-BUKRS,
       BELNR      LIKE    ZTBKPF-BELNR,
       GJAHR      LIKE    ZTBKPF-GJAHR,
       ZFIMDNO    LIKE    ZTBSEG-ZFIMDNO,     " 수입문서번호.
       LIFNR      LIKE    ZTBKPF-LIFNR,       " 구매처.
       NAME1      LIKE    LFA1-NAME1,         " 거래선.
       ZFCSTGRP   LIKE    ZTBSEG-ZFCSTGRP.    " 비용그룹.
      DATA : END OF IT_TAB.

DATA : W_STCD2   LIKE    LFA1-STCD2,         " 사업자등록번호.
       W_ZFACTNO LIKE    ZTIMIMG02-ZFACTNO.  " 계좌번호.

DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_CHECK_PAGE(1) TYPE C,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX.
DATA:   W_ZFAMDNO     LIKE ZTREQST-ZFAMDNO,
        P_BUKRS       LIKE ZTREQHD-BUKRS.
DATA:   W_TITLE(1).
DATA    S_WRBTR    LIKE    ZTBSEG-WRBTR.        "SUB금액.

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: S_BUKRS   FOR ZTBKPF-BUKRS
                                NO-EXTENSION NO INTERVALS,
                  S_OPBN FOR ZTREQHD-ZFOPBN, " 은행코드
                  S_BUDAT FOR ZTBKPF-BUDAT OBLIGATORY DEFAULT SY-DATUM,
                  S_EBELN   FOR ZTREQHD-EBELN,
                  S_OPNNO   FOR ZTREQST-ZFOPNNO.
 SELECTION-SCREEN END OF BLOCK B1.
 INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_INIT.

*title Text Write
W_TITLE = 'Y'.
TOP-OF-PAGE.
 PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* 주 테이블 SELECT
   PERFORM   P1000_READ_DATA    USING  W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.
      MESSAGE S738.  EXIT.
   ENDIF.
* 레포트 Write
   PERFORM  P3000_DATA_WRITE.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
     WHEN 'DISP'.
       IF W_TABIX IS INITIAL.
          MESSAGE S962.
       ELSE.
          IF NOT IT_TAB-BELNR IS INITIAL.
              PERFORM P2000_DISPLAY_COST_DOCUMENT USING  IT_TAB-BUKRS
                                                         IT_TAB-GJAHR
                                                         IT_TAB-BELNR.
          ELSE.
             MESSAGE S962.
          ENDIF.

       ENDIF.
    WHEN 'DSIM'.
       IF W_TABIX IS INITIAL.
          MESSAGE S962.
          EXIT.
       ENDIF.
       PERFORM P2000_SHOW_LC USING  IT_TAB-ZFIMDNO.
   ENDCASE.
   CLEAR : W_TABIX, IT_TAB.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  IF W_TITLE EQ 'Y'.
     SKIP 2.
     WRITE:/40 '[Daily Import Request Expense by payee]'
                       COLOR COL_HEADING INTENSIFIED OFF.
     SKIP 2.
     CLEAR W_TITLE.
  ENDIF.

  WRITE:/ 'Payee:',IT_TAB-LIFNR,IT_TAB-NAME1,116 'Date:',SY-DATUM.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE NO-GAP,(10) 'Purchase Ord.'          NO-GAP,
          SY-VLINE NO-GAP,(20) 'Approve No'             NO-GAP,
          SY-VLINE NO-GAP,(20) 'Payment Type'           NO-GAP,
          SY-VLINE NO-GAP,(10) 'Open Date'              NO-GAP,
          SY-VLINE NO-GAP,(10) 'Account Doc.'           NO-GAP,
          SY-VLINE NO-GAP,(30) 'Charge Category'        NO-GAP,
          SY-VLINE NO-GAP,(17) '          Amount '      NO-GAP,
          SY-VLINE NO-GAP,(03) 'TAX'                    NO-GAP,
          SY-VLINE NO-GAP,(04) 'Seq'                    NO-GAP,
          SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------
FORM P2000_AUTHORITY_CHECK USING    W_ERR_CHK.

   W_ERR_CHK = 'N'.
*----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_BL_MGT'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME 'B/L Doc Transaction'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK

*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
            FROM ZTBKPF AS H INNER JOIN ZTBSEG AS I
             ON H~BUKRS = I~BUKRS
            AND H~BELNR = I~BELNR
            AND H~GJAHR = I~GJAHR
         WHERE  H~ZFPOSYN  = 'Y'             " 전기여부.
           AND  H~BUKRS    IN S_BUKRS
           AND  H~BUDAT    IN S_BUDAT        " 전표전기일..
           AND  I~ZFCSTGRP = '003'           " 수입의뢰비용.
           AND  H~LIFNR    IN  S_OPBN.       " 은행코드.
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.
*>> 수입의뢰--------------------------------------------
     CLEAR ZTREQHD.
     SELECT SINGLE *
         FROM  ZTREQHD
         WHERE ZFREQNO = IT_TAB-ZFIMDNO
           AND ZFOPNNO   IN  S_OPNNO       " 신용장승인번호.
           AND EBELN     IN S_EBELN.
     IF SY-SUBRC NE 0.
        DELETE IT_TAB INDEX W_TABIX.
        CONTINUE.
     ENDIF.
     CLEAR ZTREQST.
     SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO
        FROM ZTREQST
       WHERE  ZFREQNO = IT_TAB-ZFIMDNO.
     SELECT SINGLE *
        FROM ZTREQST
       WHERE ZFREQNO  = IT_TAB-ZFIMDNO
         AND ZFAMDNO  = W_ZFAMDNO.

      MOVE: ZTREQHD-ZFOPNNO  TO IT_TAB-ZFOPNNO,
            ZTREQHD-EBELN   TO IT_TAB-EBELN,
            ZTREQHD-ZFREQTY TO IT_TAB-ZFREQTY,
            ZTREQST-ZFOPNDT  TO IT_TAB-ZFOPNDT.
*--------------------------------------------------------
*>> 관리 코드.
     IF  IT_TAB-ZFCD = '2AD'     " L/G보증료 "
         OR IT_TAB-ZFCD = '2AE'. " L/G발급수수료.
         PERFORM P3000_READ_DATA_ZTLG USING IT_TAB-ZFIMDNO
                                   CHANGING IT_TAB-ZFSHNO.
     ENDIF.
     CLEAR LFA1.
     SELECT SINGLE *
            FROM  LFA1
           WHERE  LIFNR = IT_TAB-LIFNR.
     IT_TAB-NAME1 = LFA1-NAME1.
     PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDREQTY'
                               IT_TAB-ZFREQTY
                               CHANGING  IT_TAB-REQTY.
     CLEAR ZTIMIMG08.
     SELECT SINGLE *
            FROM ZTIMIMG08
            WHERE ZFCDTY  = '003'
              AND ZFCD    = IT_TAB-ZFCD.
     IT_TAB-ZFCDNM  =  ZTIMIMG08-ZFCDNM.

*> 역기표시..
     IF IT_TAB-ZFRVSX EQ 'X'.
        IT_TAB-WRBTR = IT_TAB-WRBTR * -1.
     ENDIF.

     MODIFY IT_TAB INDEX W_TABIX.

  ENDLOOP.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

   SET TITLEBAR  'ZIMR84'.
   SET PF-STATUS 'ZIMR84'.
   CLEAR W_COUNT.
   SORT IT_TAB BY LIFNR EBELN ZFOPNNO ZFCD.
   LOOP AT IT_TAB.
        W_TABIX = SY-TABIX.
        ON CHANGE OF IT_TAB-LIFNR.
        IF SY-TABIX NE 1.
            PERFORM P3000_SUBTOTAL_WRITE.
            NEW-PAGE.
        ENDIF.
        ENDON.
        PERFORM   P3000_LINE_WRITE.
        AT LAST.
           PERFORM P3000_SUBTOTAL_WRITE.
           PERFORM P3000_LAST_WRITE.
        ENDAT.
   ENDLOOP.

   CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR84'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------
*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE NO-GAP,(10) IT_TAB-EBELN   NO-GAP, " P/O No
          SY-VLINE NO-GAP,(20) IT_TAB-ZFOPNNO NO-GAP, " L/C No
          SY-VLINE NO-GAP,(20) IT_TAB-REQTY   NO-GAP, " 결제구분
          SY-VLINE NO-GAP,(10) IT_TAB-ZFOPNDT  NO-GAP, " 개설일
          SY-VLINE NO-GAP,(10) IT_TAB-ZFACDO  NO-GAP, " 회계번호
          SY-VLINE NO-GAP,(30) IT_TAB-ZFCDNM  NO-GAP, " 비용종류
          SY-VLINE NO-GAP,(17) IT_TAB-WRBTR
                               CURRENCY 'KRW' NO-GAP, " 금액
          SY-VLINE NO-GAP,(03) IT_TAB-MWSKZ   NO-GAP, " TAX
          SY-VLINE NO-GAP,(04) IT_TAB-ZFSHNO  NO-GAP, " 선적차수
          SY-VLINE.
  HIDE: IT_TAB,W_TABIX.
  W_COUNT = W_COUNT + 1.
  ADD IT_TAB-WRBTR TO S_WRBTR.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------
*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_LAST_WRITE.

 FORMAT RESET.
 FORMAT COLOR COL_TOTAL INTENSIFIED ON.
 SUM.
 WRITE:/ SY-VLINE NO-GAP,(10) 'Total'  NO-GAP, " P/O No
                          (05) W_COUNT,(15)'Case'  NO-GAP, " L/C No
                          (21) ' '  NO-GAP, " 결제구분
                          (11) ' '  NO-GAP, " 개설일
                          (11) ' '  NO-GAP, " 회계번호
                          (31) ' '  NO-GAP, " 비용종류
          SY-VLINE NO-GAP,(17) IT_TAB-WRBTR
                               CURRENCY 'USD' NO-GAP, " 금액
          SY-VLINE NO-GAP, (03) ' '   NO-GAP, " TAX
                           (05) '  '  NO-GAP, " 선적차수
          SY-VLINE.
 WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------
*
*&      Form  P2000_DISPLAY_COST_DOCUMENT
*&---------------------------------------------------------------------
*
FORM P2000_DISPLAY_COST_DOCUMENT USING    P_BUKRS
                                          P_GJAHR
                                          P_BELNR.

 SET  PARAMETER ID  'BUK'       FIELD   P_BUKRS.
 SET  PARAMETER ID  'GJR'       FIELD   P_GJAHR.
 SET  PARAMETER ID  'ZPBENR'    FIELD   P_BELNR.
 CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P2000_DISPLAY_COST_DOCUMENT
*&---------------------------------------------------------------------
*
*&      Form  P3000_READ_DATA_ZTLG
*&---------------------------------------------------------------------
*
FORM P3000_READ_DATA_ZTLG USING    P_ZFREQNO
                         CHANGING  P_NAME.

*>> B/L 차수를 가져 오기 위해.
  SELECT SINGLE *
      FROM  ZTLG
      WHERE ZFREQNO = P_ZFREQNO.
  IF  SY-SUBRC EQ 0.
      CLEAR ZTBL.
      SELECT SINGLE *
             FROM ZTBL
            WHERE ZFBLNO = ZTLG-ZFBLNO.
      IF SY-SUBRC EQ 0.
        P_NAME = ZTBL-ZFSHNO.
      ENDIF.
  ENDIF.

ENDFORM.                    " P3000_READ_DATA_ZTLG
*&---------------------------------------------------------------------
*
*&      Form  P3000_SUBTOTAL_WRITE
*&---------------------------------------------------------------------
*
FORM P3000_SUBTOTAL_WRITE.

 FORMAT RESET.
 FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
 WRITE:/ SY-ULINE.
 WRITE:/ SY-VLINE NO-GAP,(10) 'Payee Sum'  NO-GAP, " P/O No
                          (21) ' '  NO-GAP, " L/C No
                          (21) ' '  NO-GAP, " 결제구분
                          (11) ' '  NO-GAP, " 개설일
                          (11) ' '  NO-GAP, " 회계번호
                          (31) ' '  NO-GAP, " 비용종류
          SY-VLINE NO-GAP,(17) S_WRBTR
                               CURRENCY 'USD' NO-GAP, " 금액
          SY-VLINE NO-GAP,(03) ' '   NO-GAP, " TAX
                          (05) ' '  NO-GAP, " 선적차수
          SY-VLINE.

 WRITE:/ SY-ULINE.
 CLEAR: S_WRBTR.

ENDFORM.                    " P3000_SUBTOTAL_WRITE
*&---------------------------------------------------------------------
*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------
FORM P2000_SHOW_LC USING    P_ZFREQNO.

  CLEAR ZTREQST.
  SELECT MAX( ZFAMDNO ) INTO W_ZFAMDNO
      FROM ZTREQST
     WHERE  ZFREQNO = P_ZFREQNO.


   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD W_ZFAMDNO.

   IF W_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

   CLEAR : ZTIMIMG00, P_BUKRS.
   SELECT SINGLE * FROM ZTIMIMG00.
   IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
      MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
   ENDIF.

*>> 회사코드 SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
