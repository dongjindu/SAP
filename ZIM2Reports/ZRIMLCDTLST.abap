*&---------------------------------------------------------------------*
*& Report  ZRIMLCDTLST
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입실적                                              *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.04                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMLCDTLST    MESSAGE-ID ZIM
                     LINE-SIZE 111
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       EBELN       LIKE ZTREQHD-EBELN,           " 구매승인번호.
       ZFREQNO     LIKE ZTREQHD-ZFREQNO,         " 수입의뢰 번호.
       ZFREQTY     LIKE ZTREQHD-ZFREQTY,         " 결제구분.
       REQTY        LIKE DD07T-DDTEXT,            "
       ZFLCKN      LIKE ZTREQHD-ZFLCKN,          " PAYMENDT TERMS.
       LCKN        LIKE DD07T-DDTEXT,            "
       WAERS       LIKE ZTREQHD-WAERS,           " Currency
       ZFBENI      LIKE ZTREQHD-ZFBENI,          " Beneficairy
       NAME1       LIKE LFA1-NAME1,              " Name 1
       ZFOPBN      LIKE ZTREQHD-ZFOPBN,          " Open Bank
       NAME2       LIKE LFA1-NAME1,              " Name 1
       ZFAMDNO     LIKE ZTREQST-ZFAMDNO,         " AMEND 회차.
       ZFUSD       LIKE ZTREQST-ZFUSD,           " USD Currency
       ZFOPNNO     LIKE ZTREQST-ZFOPNNO,         " L/C NO
       ZFOPNDT     LIKE ZTREQST-ZFOPNDT,         " 개설일.
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM.        " USD 환산금액.
DATA : END OF IT_TAB.
*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

INCLUDE   ZRIMLCDLSTTOP.
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                                NO INTERVALS,
                    S_RLDT    FOR ZTREQST-ZFOPNDT,  " 개설일.
                    S_OPBN    FOR ZTREQHD-ZFOPBN,   " 개설은행.
                    S_WERKS   FOR ZTREQHD-ZFWERKS NO-EXTENSION
                                           NO INTERVALS,  " 대표 plant
                    S_WAERS   FOR  ZTREQHD-WAERS
                                           NO INTERVALS,
                    S_EKORG   FOR ZTREQST-EKORG ,
                    S_REQTY   FOR ZTREQHD-ZFREQTY NO INTERVALS,
                    S_BACD    FOR ZTREQHD-ZFBACD NO INTERVALS,
                    S_EKGRP   FOR ZTREQST-EKGRP.
SELECTION-SCREEN END OF BLOCK B1.
* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*  테이블 SELECT
   PERFORM   P1000_GET_ZTREQHD      USING   W_ERR_CHK.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.
*>> SORT 선택.
      WHEN 'STUP' OR 'STDN'.         " SORT 선택.
         W_FIELD_NM = 'ZFOPBN'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
      WHEN 'DISP1'.
         PERFORM P2000_DISPLAY_PO USING IT_TAB-EBELN.
      WHEN 'DISP2'.
         PERFORM P2000_DISPLAY_LC USING IT_TAB-ZFREQNO IT_TAB-ZFAMDNO.
      WHEN OTHERS.
  ENDCASE.
  CLEAR : IT_TAB, W_TABIX.
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

 SET  TITLEBAR 'DTLC'.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /50  '[ Open Result List ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  IF NOT S_RLDT[] IS INITIAL.
     WRITE : /3 'Period:', S_RLDT-LOW,'~',S_RLDT-HIGH.
  ENDIF.
  IF NOT S_WAERS-LOW IS INITIAL.
     WRITE : /3 'Currecy:', S_WAERS-LOW.
  ENDIF.
  IF NOT S_OPBN-LOW IS INITIAL.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
      WHERE LIFNR = S_OPBN-LOW.
    WRITE : /3 'Open Bank:', S_OPBN-LOW,LFA1-NAME1.
  ENDIF.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  WRITE : / SY-VLINE, (28)  'L/C NO',
            SY-VLINE, (20)  'Payment Terms',
            SY-VLINE, (30)  'Open Bank',
            SY-VLINE, (20)  'Open Amount',SY-VLINE.

  FORMAT COLOR COL_HEADING INTENSIFIED OFF.

  WRITE :/  SY-VLINE, (10)  'P/O NO' NO-GAP,
            SY-VLINE NO-GAP, (05)  'AmdNo' NO-GAP,
            SY-VLINE, (10)  'Open Date',
            SY-VLINE, (20)  'Payment Type',
            SY-VLINE, (30)  'Vendor',
            SY-VLINE, (20)  'Convert Amount(USD)',  SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
  AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
           ID 'ACTVT' FIELD '*'.

  IF SY-SUBRC NE 0.
      MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
      W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTREQHD
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTREQHD   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
  REFRESH: IT_TAB.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
      FROM ZTREQHD AS R INNER JOIN ZTREQST AS I
      ON R~ZFREQNO = I~ZFREQNO
      WHERE   I~ZFDOCST  = 'O'             " 문서상태.
        AND  R~ZFWERKS  IN  S_WERKS        "  플랜트.
        AND  R~BUKRS    IN  S_BUKRS
        AND  I~ZFOPNDT  IN  S_RLDT         "  개설일.
        AND  I~EKORG    IN  S_EKORG        " 구매담당자.
        AND  I~EKGRP    IN  S_EKGRP        " 구매그룹.
        AND  R~WAERS    IN  S_WAERS        " 통화.
        AND  R~ZFREQTY  IN  S_REQTY
        AND  R~ZFBACD   IN  S_BACD
        AND  R~ZFOPBN   IN  S_OPBN         " 개설은.
        AND  R~ZFREQTY  IN  ('DA', 'DP', 'LC', 'TT').  " 결제구분

  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.EXIT.
  ENDIF.
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDREQTY' IT_TAB-ZFREQTY
                              CHANGING IT_TAB-REQTY  .
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDLCKN ' IT_TAB-ZFLCKN
                              CHANGING IT_TAB-LCKN.
    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
      WHERE LIFNR = IT_TAB-ZFBENI.
    IT_TAB-NAME1 = LFA1-NAME1.

    CLEAR LFA1.
    SELECT SINGLE *
      FROM LFA1
      WHERE LIFNR = IT_TAB-ZFOPBN.
    IT_TAB-NAME2 = LFA1-NAME1.
    MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_GET_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'DTLC'.           " GUI STATUS SETTING
   SET  TITLEBAR 'DTLC'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      W_LINE = W_LINE + 1.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LAST_WRITE.
      ENDAT.
   ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

   MOVE 0 TO SY-LSIND.

   W_PAGE = 1.
   W_LINE = 1.
   W_COUNT = 0.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
   PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_TOTAL ON.
   WRITE: / SY-ULINE.
   SUM.
   IF NOT S_WAERS[] IS INITIAL.
      WRITE : / SY-VLINE, (28)  'Open Amount', " L/C NO,
                SY-VLINE, (20)  '',    " Payment Terms,
                SY-VLINE, (30)  '',    " 개설은행',
                SY-VLINE, (20)  IT_TAB-ZFOPAMT CURRENCY
                             S_WAERS-LOW,SY-VLINE.
   ENDIF.
   WRITE : / SY-VLINE, (28)  'Covnert Amount(USD):', " L/C NO,
            SY-VLINE, (20)  '',    " Payment Terms,
            SY-VLINE, (30)  '',    " 개설은행',
            SY-VLINE, (20)  IT_TAB-ZFUSDAM CURRENCY
                            'USD',SY-VLINE.
   FORMAT COLOR OFF.
   WRITE: / SY-ULINE.
   IF W_COUNT GT 0.
      WRITE : / 'Total', W_COUNT, 'Case'.
   ENDIF.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : / SY-VLINE, (28)  IT_TAB-ZFOPNNO, " L/C NO,
            SY-VLINE, (20)  IT_TAB-LCKN,    " Payment Terms,
            SY-VLINE, (30)  IT_TAB-NAME2,    " 개설은행',
            SY-VLINE, (03)  IT_TAB-WAERS,
                      (16)  IT_TAB-ZFOPAMT CURRENCY
                            IT_TAB-WAERS,SY-VLINE.
  HIDE: IT_TAB, W_TABIX.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE :/  SY-VLINE, (10)  IT_TAB-EBELN NO-GAP,
            SY-VLINE NO-GAP, (05) IT_TAB-ZFAMDNO NO-GAP,
            SY-VLINE, (10)  IT_TAB-ZFOPNDT,  " 개설일.
            SY-VLINE, (20)  IT_TAB-REQTY,    " 결제구분',
            SY-VLINE, (30)  IT_TAB-NAME1,    " Vendor',
            SY-VLINE, (20)  IT_TAB-ZFUSDAM
            CURRENCY 'USD', SY-VLINE.
   HIDE: IT_TAB, W_TABIX.
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO P_ZFAMDNO.

   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
   EXPORT 'BES'           TO MEMORY ID 'BES'.
   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
   EXPORT 'ZPAMDNO'       TO MEMORY ID 'ZPAMDNO'.

   IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ENDIF.

ENDFORM.                    " P2000_SHOW_LC

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

*  REFRESH IT_TAB_DOWN.
*  LOOP AT IT_TAB.
*    CLEAR IT_TAB_DOWN.
*    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
*    WRITE : IT_TAB-MENGE   UNIT     IT_TAB-MEINS TO IT_TAB_DOWN-MENGE,
*            IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD TO IT_TAB_DOWN-ZFUSDAM
*,
*            IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS TO IT_TAB_DOWN-ZFOPAMT
*.
*    APPEND IT_TAB_DOWN.
*  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_PO
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_PO USING    P_EBELN.

  SET PARAMETER ID 'BSP' FIELD ''.
  SET PARAMETER ID 'BES' FIELD P_EBELN.
  CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DISPLAY_PO
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_LC
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_LC USING    P_ZFREQNO
                               P_ZFAMDNO.

  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.

  IF P_ZFAMDNO = '00000'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ELSE.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_DISPLAY_LC
