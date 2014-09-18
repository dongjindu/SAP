*&---------------------------------------------------------------------*
*& Report  ZRIMMSCNC                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 조출/체선료 회계 전표 취소                            *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2001.04.23                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMMSCNC    MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.
INCLUDE: <ICON>,
         ZRIMBDCCOM.

*-----------------------------------------------------------------------
* 사용할 TABLE DECLRAE
*-----------------------------------------------------------------------
TABLES : ZTMSHD,          "모선 Header
         ZTMSCST,         "조출/체선
         ZTIMIMG11,
         ZVT001W,
         BKPF,
         BSEG,
         BSIS,
         UF05A,
         COBL,
         LFA1,
         SPOP,
         ZSMSCST.

*-----------------------------------------------------------------------
* 조출/체선 INTERNAL TABLE & VARIABLE DECLARE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS  0,
       MARK           TYPE     C,
       ZFMSNM         LIKE     ZTMSHD-ZFMSNM,       " 모선명.
       ZFMSNO         LIKE     ZTMSCST-ZFMSNO,
       ZFAPRTC        LIKE     ZTMSCST-ZFAPRTC,     " 도착항.
       ZFETA          LIKE     ZTMSCST-ZFETA,       " 도착일.
       LIFNR          LIKE     ZTMSCST-LIFNR,       " 공급사(선사).
       NAME1          LIKE     LFA1-NAME1,          " 공급사명.
       ZFCARGO        LIKE     ZTMSCST-ZFCARGO,     " 하역사.
       NAME2(18)      TYPE     C,          " 하역사명.
       ZFCCGB         LIKE     ZTMSCST-ZFCCGB,      " 조/출 구분.
       ZFCCNM(4)      TYPE     C,                   " 조/출 명.
       ZFVAT          LIKE     ZTMSCST-ZFVAT,
       ZFSHAMT        LIKE     ZTMSCST-ZFSHAMT,     " 조출/체선료.
       ZFUNCO         LIKE     ZTMSCST-ZFUNCO,      " 미수금.
       ZFNOPY         LIKE     ZTMSCST-ZFNOPY,      " 미지급.
       ZFOCDT         LIKE     ZTMSCST-ZFOCDT,      " 발생일.
       BUKRS          LIKE     ZTMSCST-BUKRS,       " 회사코드.
       BELNR          LIKE     ZTMSCST-BELNR,       " 전표번호.
       GJAHR          LIKE     ZTMSCST-GJAHR,       " 회계년도.
       BELNRV         LIKE     ZTMSCST-BELNRV,      " vat 전표번호.
       GJAHRV         LIKE     ZTMSCST-GJAHRV,      " vat 회계년도.
       ZFPSDT         LIKE     ZTMSCST-ZFPSDT.      " Posting Date.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
*>> MESSAGE 출력용.
TABLES : BAL_S_DMSG.
*>>> ERROR 처리용.
DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
       INCLUDE  STRUCTURE  BDCMSGCOLL.
       DATA : ICON       LIKE BAL_S_DMSG-%_ICON,
              MESSTXT(255) TYPE C,
              BUKRS        LIKE ZTMSCST-BUKRS,
              GJAHR        LIKE ZTMSCST-GJAHR,
              BELNR        LIKE ZTMSCST-BELNR.
DATA : END OF IT_ERR_LIST.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
* SELECT RECORD
*-----------------------------------------------------------------------
DATA: BEGIN OF  IT_SELECTED OCCURS 0,
      BUKRS          LIKE     ZTMSCST-BUKRS,
      GJAHR          LIKE     ZTMSCST-GJAHR,
      BELNR          LIKE     ZTMSCST-BELNR,
      GJAHRV         LIKE     ZTMSCST-GJAHRV,
      BELNRV         LIKE     ZTMSCST-BELNR,
END OF IT_SELECTED.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
*>> LOCKED OBJECT.
DATA:    BEGIN OF IT_LOCKED OCCURS 0,
         ZFMSNO         LIKE     ZTMSCST-ZFMSNO,
         ZFAPRTC        LIKE     ZTMSCST-ZFAPRTC.     " 도착항.
DATA     END OF IT_LOCKED.
*-----------------------------------------------------------------------

*-----------------------------------------------------------------------
DATA:    BEGIN OF IT_ZTMSCST OCCURS 0.
         INCLUDE STRUCTURE ZTMSCST.
DATA     END OF IT_ZTMSCST.
*-----------------------------------------------------------------------

*
DATA : W_LINE         TYPE     I,
       W_COUNT        TYPE     I,
       TEXTLEN        TYPE     I,
       TEMP_WRBTR(16),
       TEMP_WRBTR1(16),
       W_MOD          TYPE     I,
       W_PROC_CNT     TYPE     I,
       W_LIFNR_NM(24) TYPE     C,
       ANTWORT        TYPE     C,
       OPTION(1)      TYPE     C,
       CANCEL_OPTION  TYPE     C,
       RADIO_NONE     TYPE     C,
       RADIO_ALL      TYPE     C,
       RADIO_ERROR    TYPE     C,
       INCLUDE(8)     TYPE     C,             "
       W_FIELD_NM     LIKE     DD03D-FIELDNAME,   " 필드?
       W_PSDT         LIKE     ZTMSCST-ZFPSDT,
       W_OCDT         LIKE     ZTMSCST-ZFOCDT,
       W_ZFMSNM       LIKE     ZTMSHD-ZFMSNM,
       W_NOAMT        LIKE     ZTMSCST-ZFSHAMT,
       W_SY_SUBRC     LIKE     SY-SUBRC,
       W_ERR_CNT      TYPE     I,
       ZFFIYR         LIKE     ZTRECST-ZFFIYR,
       ZFACDO         LIKE     ZTRECST-ZFACDO,
       OK-CODE        LIKE     SY-UCOMM,
       W_TABIX        LIKE     SY-TABIX.

*>> SORT COMMON INCLUDE.
INCLUDE ZRIMSORTCOM.

*--------------------------------------------------------------------
* 검색조건 WINDOW CREATE
*--------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR  ZTMSCST-BUKRS,   "회사.
                   S_GJAHR   FOR  ZTMSCST-GJAHR,   "전표연도.
                   S_BELNR   FOR  ZTMSCST-BELNR,   "전표번호.
                   S_MSNM    FOR  ZTMSHD-ZFMSNM,   "모선명.
                   S_ETA     FOR  ZTMSCST-ZFETA,   "도착일.
                   S_LIFNR   FOR  ZTMSCST-LIFNR,   "공급사(선사).
                   S_CARGO   FOR  ZTMSCST-ZFCARGO, "하역사.
                   S_OCDT    FOR  ZTMSCST-ZFOCDT,  "발생일.
                   S_PSDT    FOR  ZTMSCST-ZFPSDT,  "회계일자.
*>> 2001.03.20 KSB INSERT
                   S_ZFCCGB  FOR  ZTMSCST-ZFCCGB.  ">조체선 구분.
SELECTION-SCREEN END OF BLOCK B1.

*-----------------------------------------------------------------------
* 화면 INITIALIZATION
*-----------------------------------------------------------------------
INITIALIZATION.
   SET  TITLEBAR 'ZIMMS3'.           " GUI TITLE SETTING..

*-----------------------------------------------------------------------
* REPORT WRITE 시 PAGE의 TOP EVENT
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
     PERFORM P2000_TITLE_WRITE.
  ENDIF.

*-----------------------------------------------------------------------
* START-OF-SELECTION 절..
*-----------------------------------------------------------------------
START-OF-SELECTION.

   SET PF-STATUS 'ZIMMS3'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMMS3'.           " GUI TITLE SETTING..
   PERFORM P1000_READ_TEXT.

   IF W_LINE EQ 0. MESSAGE S738. EXIT. ENDIF.

*-----------------------------------------------------------------------
* SELECT 이후의 EVENT.
*-----------------------------------------------------------------------
END-OF-SELECTION.
   CHECK W_LINE  GT 0.
   PERFORM P3000_DATA_WRITE.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
         PERFORM P3000_SELECT_RECORD   USING   SY-UCOMM.
*      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
*        W_FIELD_NM = 'ZFREQDT'.
*        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*        PERFORM HANDLE_SORT TABLES  IT_TAB
*                            USING   SY-UCOMM.
      WHEN 'POST'.
         CLEAR : ANTWORT, W_PROC_CNT, W_SUBRC, W_PSDT, W_OCDT.
         PERFORM P3000_POST_LINE_COUNT.
         IF W_PROC_CNT < 1. MESSAGE S766. EXIT. ENDIF.
         PERFORM P3000_EXEC_MAT_INVOICE_VERIFY.
         IF NOT ( OK-CODE EQ 'YES'  OR  OK-CODE EQ 'ENTR' ).
            EXIT.
         ENDIF.
         PERFORM P3000_CANC_MAT_INVOICE_VERIFY.
         DESCRIBE  TABLE IT_ERR_LIST     LINES  W_LINE.
           IF W_LINE GT 0.
              INCLUDE = 'POPU'.
              CALL SCREEN 0100 STARTING AT  05   3
                               ENDING   AT  100 12.
              CLEAR : INCLUDE.
           ENDIF.
           PERFORM   P1000_READ_TEXT.
           MESSAGE S826 WITH W_PROC_CNT.
           IF W_LINE LE 0.  LEAVE TO SCREEN 0.    ENDIF.
           PERFORM RESET_LIST.

      WHEN 'DPDO'.
            PERFORM P3000_POST_LINE_COUNT.
            IF W_PROC_CNT GT 1.
               MESSAGE S965.
               EXIT.
            ENDIF.
            IF W_PROC_CNT EQ 0.
               MESSAGE S766.
               EXIT.
            ENDIF.
            PERFORM  P2000_FI_DOC_DISPLAY   USING IT_SELECTED-BELNR
                                                  IT_SELECTED-BUKRS
                                                  IT_SELECTED-GJAHR.


      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT.
            IF W_LINE EQ 0.    LEAVE TO SCREEN 0.    ENDIF.
            PERFORM RESET_LIST.
            IF W_LINE EQ 0.    LEAVE TO SCREEN 0.    ENDIF.
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
   CLEAR : IT_TAB.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.
   CASE INCLUDE.
      WHEN 'POPU'.
         IF NOT IT_ERR_LIST-MSGTYP IS INITIAL.
            MESSAGE ID IT_ERR_LIST-MSGID TYPE IT_ERR_LIST-MSGTYP
                    NUMBER IT_ERR_LIST-MSGNR
                    WITH   IT_ERR_LIST-MSGV1
                           IT_ERR_LIST-MSGV2
                           IT_ERR_LIST-MSGV3
                           IT_ERR_LIST-MSGV4.
         ENDIF.
         CLEAR : IT_ERR_LIST.
     WHEN OTHERS.
  ENDCASE.




*&---------------------------------------------------------------------*
*&      Form  P2000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       화면상에 TITLE WRITE 한다.
*----------------------------------------------------------------------*
FORM P2000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[ 조출/체선료 Posting Cancel ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',        SY-VLINE NO-GAP,
            '모선명    '          NO-GAP,
            '          '          NO-GAP,
            '          '          NO-GAP, SY-VLINE NO-GAP,
            '발생일    '          NO-GAP, SY-VLINE NO-GAP,
            '구분'                NO-GAP, SY-VLINE NO-GAP,
            '조출/체선료       '  NO-GAP, SY-VLINE NO-GAP,
            '미수/미지급 금액   ' NO-GAP, SY-VLINE NO-GAP,
            '회계년도-번호  '     NO-GAP, SY-VLINE NO-GAP,
            'Posting Date'        NO-GAP, SY-VLINE NO-GAP,
            / SY-VLINE, ' ',      SY-VLINE NO-GAP,
            '도착일    '          NO-GAP,
            '             '       NO-GAP, SY-VLINE NO-GAP,
            '도착항'              NO-GAP, SY-VLINE NO-GAP,
            '공급사    '          NO-GAP,
            '          '          NO-GAP,
            '              '      NO-GAP, SY-VLINE NO-GAP,
            'VAT                ' NO-GAP, SY-VLINE NO-GAP,
            '하역사    '          NO-GAP,
            '          '          NO-GAP,
            '        '            NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE NO-GAP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TEXT.

* 조회조건에 해당하는 자료 SELECT.
  SELECT B~ZFMSNM  A~ZFOCDT A~ZFCCGB  A~ZFSHAMT A~ZFUNCO
         A~ZFNOPY  A~GJAHR  A~BELNR   A~ZFPSDT  A~ZFETA
         A~ZFAPRTC A~LIFNR  A~ZFCARGO A~ZFVAT   A~ZFMSNO
         A~GJAHRV  A~BELNRV A~BUKRS
  INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
  FROM   ZTMSCST AS A INNER JOIN ZTMSHD AS B
  ON     A~ZFMSNO  EQ  B~ZFMSNO
  WHERE  A~BUKRS   IN  S_BUKRS
  AND    A~GJAHR   IN  S_GJAHR
  AND    A~BELNR   IN  S_BELNR
  AND    B~ZFMSNM  IN  S_MSNM
  AND    A~ZFETA   IN  S_ETA
  AND    A~ZFOCDT  IN  S_OCDT
  AND    A~ZFPSDT  IN  S_PSDT
  AND    A~LIFNR   IN  S_LIFNR
  AND    A~ZFCARGO IN  S_CARGO
  AND    A~ZFCCGB  IN  S_ZFCCGB
  AND    A~GJAHR   NE  SPACE
  AND    A~BELNR   NE  SPACE .

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  SORT  IT_TAB BY ZFMSNM ZFETA.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE.
  CLEAR : W_COUNT, IT_TAB, W_ZFMSNM.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    W_COUNT = W_COUNT + 1.

* 공급사명 SELECT.
    SELECT SINGLE  NAME1  INTO   IT_TAB-NAME1
    FROM   LFA1           WHERE  LIFNR = IT_TAB-LIFNR.

    MOVE IT_TAB-NAME1 TO W_LIFNR_NM.

* 하역사명 SELECT.
    SELECT SINGLE NAME1   INTO   IT_TAB-NAME2
    FROM   LFA1           WHERE  LIFNR = IT_TAB-ZFCARGO.

* 조출구분에 따른 TEXT SETTING.
    IF IT_TAB-ZFCCGB EQ 'A'.
       MOVE '조출' TO IT_TAB-ZFCCNM.
       MOVE IT_TAB-ZFNOPY TO W_NOAMT.
    ELSE.
       MOVE '체선' TO IT_TAB-ZFCCNM.
       MOVE IT_TAB-ZFUNCO TO W_NOAMT.
    ENDIF.

    PERFORM P3000_LINE_WRITE.
  ENDLOOP.
  CLEAR : IT_TAB.
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

     FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

     WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       IT_TAB-ZFMSNM     NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFOCDT     NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFCCNM     NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFSHAMT    CURRENCY 'KRW'
                         NO-GAP, SY-VLINE NO-GAP.
    IF IT_TAB-ZFCCGB = 'D'.
       WRITE :  ' '        COLOR COL_POSITIVE INTENSIFIED OFF NO-GAP.
       WRITE :  W_NOAMT    CURRENCY 'KRW'
                           COLOR COL_POSITIVE INTENSIFIED OFF
                           NO-GAP, SY-VLINE NO-GAP.
    ELSE.
       WRITE :  ' '        COLOR COL_NEGATIVE INTENSIFIED OFF NO-GAP.
       WRITE :  W_NOAMT    CURRENCY 'KRW'
                           COLOR COL_NEGATIVE INTENSIFIED OFF
                           NO-GAP, SY-VLINE NO-GAP.
    ENDIF.

    WRITE : IT_TAB-GJAHR      NO-GAP,
       '-'               NO-GAP,
       IT_TAB-BELNR      NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFPSDT     NO-GAP,
       '  '              NO-GAP, SY-VLINE NO-GAP.
    HIDE    IT_TAB.

   FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
   WRITE: / SY-VLINE, ' '  ,SY-VLINE NO-GAP,
       IT_TAB-ZFETA      NO-GAP,
       '             '   NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFAPRTC    NO-GAP,
       '   '             NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-LIFNR      NO-GAP,
       W_LIFNR_NM        NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFVAT      CURRENCY 'KRW' NO-GAP, SY-VLINE NO-GAP,
       IT_TAB-ZFCARGO    NO-GAP,
       IT_TAB-NAME2      NO-GAP, SY-VLINE NO-GAP.
   HIDE    IT_TAB.
   WRITE / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_SELECT_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UCOMM  text
*----------------------------------------------------------------------*
FORM P3000_SELECT_RECORD USING    P_SY_UCOMM.

DATA : WL_MARK.

   IF P_SY_UCOMM EQ 'MKAL'. WL_MARK = 'X'.
   ELSEIF P_SY_UCOMM EQ 'MKLO'. CLEAR : WL_MARK. ENDIF.

   DO.
      CLEAR MARKFIELD.
      READ LINE SY-INDEX FIELD VALUE MARKFIELD.
      IF SY-SUBRC NE 0. EXIT. ENDIF.
      MODIFY CURRENT LINE FIELD VALUE MARKFIELD FROM WL_MARK.
   ENDDO.

ENDFORM.                    " P3000_SELECT_RECORD
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0001 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

   IF OPTION = '1'.
      SET CURSOR FIELD 'SPOP-OPTION1'.
   ELSE.
      SET CURSOR FIELD 'SPOP-OPTION2'.
   ENDIF.


ENDMODULE.                 " SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

  AUTHORITY-CHECK OBJECT 'ZM_BDC_MGT'
                  ID 'ACTVT' FIELD '*'.
  W_SY_SUBRC = SY-SUBRC.

  LOOP AT SCREEN.
    IF SCREEN-NAME = 'SPOP-OPTION_CAN'.
      IF CANCEL_OPTION = SPACE.
        SCREEN-ACTIVE = 0.
      ENDIF.
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-TEXTLINE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE'.                   "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE1'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE2'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ELSEIF SCREEN-NAME = 'SPOP-DIAGNOSE3'.                  "B20K058946
      SCREEN-LENGTH = TEXTLEN.                              "B20K058946
    ENDIF.

    IF W_SY_SUBRC NE 0 AND SY-DYNNR EQ '3515'.
       IF SCREEN-NAME(10) EQ 'RADIO_NONE'  OR
          SCREEN-NAME(09) EQ 'RADIO_ALL'   OR
          SCREEN-NAME(11) EQ 'RADIO_ERROR' OR
          SCREEN-NAME(06) EQ 'BLOCK2'.
          SCREEN-INVISIBLE = 1.
       ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCR0002 INPUT.

  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCR0002  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'N'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
       ANTWORT = 'Y'.
  ENDCASE.

  IF ANTWORT EQ 'Y' OR ANTWORT EQ 'N'.
     SET SCREEN 0.   LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_EXEC_MAT_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_EXEC_MAT_INVOICE_VERIFY.

  MOVE 'Initial Value' TO SPOP-TITEL.
*  MOVE 'X'             TO RADIO_NONE.
  IF BSIS-BUDAT IS INITIAL.
     MOVE SY-DATUM    TO BSIS-BUDAT.
  ENDIF.

  CALL SCREEN 0010 STARTING AT 15 1
                   ENDING   AT 52 6.


ENDFORM.                    " P3000_EXEC_MAT_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*&      Form  P3000_POST_LINE_COUNT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_POST_LINE_COUNT.

  REFRESH : IT_SELECTED, IT_ERR_LIST.
  CLEAR     W_PROC_CNT.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
* 이미 회계처리한 자료는 SKIP.
       MOVE : IT_TAB-BUKRS     TO IT_SELECTED-BUKRS,       " 전표
              IT_TAB-GJAHR     TO IT_SELECTED-GJAHR,
              IT_TAB-BELNR     TO IT_SELECTED-BELNR,
              IT_TAB-GJAHRV    TO IT_SELECTED-GJAHRV,
              IT_TAB-BELNRV    TO IT_SELECTED-BELNRV.
       APPEND  IT_SELECTED.
       W_PROC_CNT  =  W_PROC_CNT  +  1.
    ENDIF.
  ENDDO.

ENDFORM.                    " P3000_POST_LINE_COUNT
*&---------------------------------------------------------------------*
*&      Form  P3000_BDC_SCREEN_CALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_BDC_SCREEN_CALL.
  REFRESH : IT_LOCKED, IT_ZTMSCST.
  CLEAR : W_PROC_CNT.

  SELECT SINGLE * FROM ZTIMIMG11.

  LOOP AT IT_SELECTED.
     SELECT SINGLE * FROM  ZTMSCST
                     WHERE BUKRS    EQ   IT_SELECTED-BUKRS
                     AND   GJAHR    EQ   IT_SELECTED-GJAHR
                     AND   BELNR    EQ   IT_SELECTED-BELNR.

*>> 오류 검증....
     IF ZTMSCST-BUKRS IS INITIAL.
        MESSAGE S167 WITH '회사코드'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF ZTMSCST-ZTERM IS INITIAL.
        MESSAGE S167 WITH '지급 조건'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF ZTMSCST-LIFNR IS INITIAL.
        MESSAGE S167 WITH '공급사 코드'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF ZTMSCST-ZFCARGO IS INITIAL.
        MESSAGE S167 WITH '하역사 코드'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
     IF ZTMSCST-ZFWERKS IS INITIAL.
        MESSAGE S167 WITH '플랜트'.
        PERFORM  P2000_SINGLE_MESS_MAKE.
        CONTINUE.
     ENDIF.
*>> LOCK
     PERFORM  P2000_SET_LOCK_MODE   USING ZTMSCST-ZFMSNO
                                          'L'   W_SUBRC.
     IF W_SUBRC EQ 0.
        MOVE: ZTMSCST-ZFMSNO  TO IT_LOCKED-ZFMSNO,
              ZTMSCST-ZFAPRTC TO IT_LOCKED-ZFAPRTC.
        APPEND IT_LOCKED.
*>> INTERNAL TABLE APPENDING.
        MOVE-CORRESPONDING  ZTMSCST   TO   IT_ZTMSCST.
        APPEND IT_ZTMSCST.
     ELSE.
        PERFORM  P2000_SINGLE_MESS_MAKE.
     ENDIF.

     REFRESH : BDCDATA.
     CLEAR : W_SUBRC, BDCDATA.

     IF ZTMSCST-BELNR IS INITIAL.
        IF ZTMSCST-ZFCCGB EQ 'A'.          ">조출료.
**>> BDC DATA MAKE(조출).
           IF ZTMSCST-ZFVAT EQ 0.   ">> 부가세가 존재하지 않을경우.
                 PERFORM P2000_BDC_DATA_MAKE_1.
           ELSE.                    ">> 부가세가 존재할 경우.
                 PERFORM P2000_BDC_DATA_MAKE_2.
           ENDIF.
        ELSEIF ZTMSCST-ZFCCGB EQ 'D'.      ">체선료.
**>> BDC DATA MAKE(체선).
           PERFORM P2000_BDC_DATA_MAKE.
        ENDIF.
*>>PARAMETER CLEAR.
        SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
        SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.
* 한건씩 BDC SCREEN CALL.
        PERFORM P2000_CALL_TRANSACTION  USING 'FB01'
                                        CHANGING W_SUBRC.
        IF W_SUBRC NE 0.      ">> ERROR 발생시.
           PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
           ADD    1    TO    W_ERR_CNT.
           CONTINUE.
        ELSE.                 ">> SUCCESS 시.
           GET PARAMETER ID 'BLN' FIELD ZFACDO.        " 전표번호.
           GET PARAMETER ID 'GJR' FIELD ZFFIYR.        " 회계년도.
*>> 전표번호가 전달되지 않을 경우.
           IF ZFACDO IS INITIAL AND ZFFIYR IS INITIAL.
*>>> 오류..(사용자 종결 등....)
              MESSAGE S494.
              PERFORM  P2000_SINGLE_MESS_MAKE.
              CONTINUE.
           ENDIF.
           READ TABLE IT_ZTMSCST WITH KEY ZFMSNO  = ZTMSCST-ZFMSNO
                                          ZFAPRTC = ZTMSCST-ZFAPRTC.
           IF SY-SUBRC EQ 0.
              MOVE : ZFACDO         TO     IT_ZTMSCST-BELNR,
                     ZFFIYR         TO     IT_ZTMSCST-GJAHR,
                     ZSMSCST-ZFPSDT TO     IT_ZTMSCST-ZFPSDT,
                     ZSMSCST-ZFOCDT TO     IT_ZTMSCST-ZFOCDT,
                     SY-UNAME       TO     IT_ZTMSCST-UNAM,
                     SY-DATUM       TO     IT_ZTMSCST-UDAT.
              MODIFY IT_ZTMSCST  INDEX   SY-TABIX.
           ENDIF.
           PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
     ENDIF.
*-----------------------------------------------------------------------
* 조출료이면서, 부가세가 존재할 경우.
*-----------------------------------------------------------------------
     IF ZTMSCST-BELNRV IS INITIAL AND ">전표번호.
        ZTMSCST-ZFCCGB EQ 'A'     AND ">조출료.
        ZTMSCST-ZFVAT  NE  0.         ">> 부가세가 존재하지 않을경우.
        PERFORM P2000_BDC_DATA_MAKE_3.
*>>PARAMETER CLEAR.
        SET PARAMETER ID 'BLN' FIELD ''.        " 전표번호.
        SET PARAMETER ID 'GJR' FIELD ''.        " 회계년도.
* 한건씩 BDC SCREEN CALL.
        PERFORM P2000_CALL_TRANSACTION  USING 'FB01'
                                        CHANGING W_SUBRC.
        IF W_SUBRC NE 0.      ">> ERROR 발생시.
           PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
           ADD    1    TO    W_ERR_CNT.
           CONTINUE.
        ELSE.                 ">> SUCCESS 시.
           GET PARAMETER ID 'BLN' FIELD ZFACDO.        " 전표번호.
           GET PARAMETER ID 'GJR' FIELD ZFFIYR.        " 회계년도.
*>> 전표번호가 전달되지 않을 경우.
           IF ZFACDO IS INITIAL AND ZFFIYR IS INITIAL.
*>>> 오류..(사용자 종결 등....)
              MESSAGE S494.
              PERFORM  P2000_SINGLE_MESS_MAKE.
              CONTINUE.
           ENDIF.
           READ TABLE IT_ZTMSCST WITH KEY ZFMSNO  = ZTMSCST-ZFMSNO
                                          ZFAPRTC = ZTMSCST-ZFAPRTC.
           IF SY-SUBRC EQ 0.
              MOVE : ZFACDO         TO     IT_ZTMSCST-BELNRV,
                     ZFFIYR         TO     IT_ZTMSCST-GJAHRV,
                     ZSMSCST-ZFPSDT TO     IT_ZTMSCST-ZFVPSDT,
                     SY-UNAME       TO     IT_ZTMSCST-UNAM,
                     SY-DATUM       TO     IT_ZTMSCST-UDAT.
              MODIFY IT_ZTMSCST  INDEX   SY-TABIX.
           ENDIF.
           PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.
        ENDIF.
     ENDIF.
     ADD    1    TO    W_PROC_CNT.
  ENDLOOP.

  MODIFY ZTMSCST FROM TABLE IT_ZTMSCST.

*>> UNLOCKED.
  LOOP AT IT_LOCKED.
     PERFORM  P2000_SET_LOCK_MODE   USING IT_LOCKED-ZFMSNO
                                          'U'    W_SUBRC.
  ENDLOOP.


ENDFORM.                    " P3000_BDC_SCREEN_CALL
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_COUNT = 0.
  PERFORM   P2000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0100 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' ).
     EXIT.
  ENDIF.

   IF ZSMSCST-ZFOCDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFOCDT'.
   ENDIF.
   IF ZSMSCST-ZFPSDT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZSMSCST' 'ZFPSDT'.
   ENDIF.
   IF BSEG-GSBER IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'BSEG' 'GSBER'.
   ENDIF.

   IF COBL-KOSTL IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'COBL' 'KOSTL'.
   ENDIF.

   IF COBL-PRCTR IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'COBL' 'PRCTR'.
   ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE.
*>> 모선 헤더.
  SELECT SINGLE * FROM ZTMSHD
                  WHERE ZFMSNO EQ ZTMSCST-ZFMSNO.
*>> 문서번호.
  CONCATENATE ZTMSCST-ZFMSNO '-' ZTMSCST-ZFAPRTC INTO BKPF-XBLNR.
*>> 모선명칭.
  MOVE : ZTMSHD-ZFMSNM TO BKPF-BKTXT.
*--------------------------------------------------------------------
* J_1BT001WV ===> ZVT001W VIEW CREATE
*  Database View가 아니기 때문에 Select시 오류가 발생하는 것 같음????
*--------------------------------------------------------------------
  CLEAR : ZVT001W.
  SELECT SINGLE * FROM ZVT001W
                  WHERE WERKS EQ ZTMSCST-ZFWERKS.

  SELECT SINGLE * FROM LFA1
                  WHERE LIFNR EQ ZTMSCST-ZFCARGO.

  REFRESH : BDCDATA.
* 초기화면 FIELD
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0100',
      ' ' 'BKPF-BLDAT'   ZSMSCST-ZFOCDT,     " Document Date
      ' ' 'BKPF-BLART'  'SA',                " Type
      ' ' 'BKPF-BUKRS'   ZTMSCST-BUKRS,      " Company Code
      ' ' 'BKPF-BUDAT'   ZSMSCST-ZFPSDT,     " Posting Date
      ' ' 'BKPF-WAERS'  'KRW',               " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  BKPF-XBLNR,          " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  BKPF-BKTXT,          " 전표헤더텍스트.
      ' ' 'RF05A-PARGB' SPACE,               " 관계사 사업영역.
      ' ' 'RF05A-NEWBS' '01',                " Posting Key
      ' ' 'RF05A-NEWKO'  LFA1-KUNNR,         " Account(하역사).
      ' ' 'RF05A-NEWUM'  SPACE,              "다음 개별항목특별 G/L지시.
      ' ' 'RF05A-NEWBW'  SPACE,              " 자산거래유형.
      ' ' 'BDC_OKCODE'  '/00'.               " ENTER

*>> 미수금.
  WRITE ZTMSCST-ZFUNCO    CURRENCY 'KRW' TO TEMP_WRBTR.

* NEXT SCREEN.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0301',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
      ' ' 'BSEG-ZTERM'  ZTMSCST-ZTERM,         " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '미수금',              " 텍스트.
      ' ' 'RF05A-NEWBS' '31',                  " Posting Key
      ' ' 'RF05A-NEWKO' ZTMSCST-LIFNR,         " ACCOUNT(체선료)
      ' ' 'BDC_OKCODE'  '/00'.                 " ENTER

*>> 체선료(미지급).
  WRITE ZTMSCST-ZFNOPY    CURRENCY 'KRW' TO TEMP_WRBTR.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0302',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,          " Amount
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'COBL-KOSTL'  COBL-KOSTL,            " Cost center.
*      ' ' 'COBL-PRCTR'  COBL-PRCTR,            " 손익센터.
      ' ' 'BSEG-SGTXT'  '체선료(미지급금)',    " 텍스트.
      ' ' 'RF05A-NEWBS' '40',                  " Posting Key
      ' ' 'RF05A-NEWKO' ZTIMIMG11-ZFMSCST4,    " ACCOUNT(영업외 비용)
      ' ' 'BDC_OKCODE'  '/00         '.        " ENTER.

*>> 영업외 비용.
  WRITE ZTMSCST-ZFCST    CURRENCY 'KRW' TO TEMP_WRBTR.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0300',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
*      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
*      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
*      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
*      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
*      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'BSEG-ZTERM'  ZTMSCST-ZTERM,         " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
*      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '영업외 비용',         " 텍스트.
      ' ' 'BDC_OKCODE'  '=BU'.                 " ENTER

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   BSEG-GSBER,    " 사업영역.TEST
      ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
      ' ' 'COBL-PRCTR'   COBL-PRCTR,    " 손익센터.
      ' ' 'BDC_OKCODE'   '/00'.         " ENTER

ENDFORM.                    " P2000_BDC_DATA_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTMSCST_ZFMSNO  text
*----------------------------------------------------------------------*
FORM P2000_SET_LOCK_MODE USING    P_ZFMSNO
                                  PA_MODE
                                  W_SUBRC.
  IF PA_MODE EQ 'L'.
     CALL FUNCTION 'ENQUEUE_EZ_ZTMSHD'
         EXPORTING
              ZFMSNO = P_ZFMSNO
         EXCEPTIONS
              OTHERS  = 1.

    W_SUBRC = SY-SUBRC.

    IF W_SUBRC <> 0.
       MESSAGE S510 WITH SY-MSGV1 '모선'
                         P_ZFMSNO ''
                    RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF PA_MODE EQ 'U'.

     CALL FUNCTION 'DEQUEUE_EZ_ZTMSHD'
         EXPORTING
              ZFMSNO = P_ZFMSNO.

    W_SUBRC = SY-SUBRC.
  ENDIF.

ENDFORM.                    " P2000_SET_LOCK_MODE
*&---------------------------------------------------------------------*
*&      Form  P2000_SINGLE_MESS_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SINGLE_MESS_MAKE.

   REFRESH : MESSTAB.
   MOVE : 'E'                 TO     MESSTAB-MSGTYP,
          SY-MSGID            TO     MESSTAB-MSGID,
          SY-MSGNO            TO     MESSTAB-MSGNR,
          SY-MSGV1            TO     MESSTAB-MSGV1,
          SY-MSGV2            TO     MESSTAB-MSGV2,
          SY-MSGV3            TO     MESSTAB-MSGV3,
          SY-MSGV4            TO     MESSTAB-MSGV4.
   APPEND MESSTAB.

   PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST.

*>> UNLOCKED.
*   LOOP AT IT_LOCKED.
*      PERFORM  P2000_SET_LOCK_MODE   USING IT_LOCKED-ZFMSNO
*                                           'U'    W_SUBRC.
*   ENDLOOP.


ENDFORM.                    " P2000_SINGLE_MESS_MAKE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST.

   LOOP AT RETURN.

      MOVE : RETURN-TYPE          TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID            TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER        TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1    TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2    TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3    TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4    TO     IT_ERR_LIST-MSGV4.

       CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
                   MSGID               = IT_ERR_LIST-MSGID
                   MSGNR               = IT_ERR_LIST-MSGNR
                   MSGV1               = IT_ERR_LIST-MSGV1
                   MSGV2               = IT_ERR_LIST-MSGV2
                   MSGV3               = IT_ERR_LIST-MSGV3
                   MSGV4               = IT_ERR_LIST-MSGV4
            IMPORTING
                   MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

      CASE IT_ERR_LIST-MSGTYP.
         WHEN 'E'.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
         WHEN 'I'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'S'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'W'.
            MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
         WHEN OTHERS.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      ENDCASE.

      APPEND  IT_ERR_LIST.

   ENDLOOP.
*>> UNLOCKED.
*   LOOP AT IT_LOCKED.
*      PERFORM  P2000_SET_LOCK_MODE   USING IT_LOCKED-ZFMSNO
*                                           'U'    W_SUBRC.
*   ENDLOOP.


ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Module  D0200_STATUS_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0200_STATUS_SCR0200 OUTPUT.
  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH 'Message LIST'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.


ENDMODULE.                 " D0200_STATUS_SCR0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0200_LIST_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0200_LIST_CHECK_SCR0200 INPUT.
   LEAVE TO LIST-PROCESSING.

   CASE INCLUDE.
      WHEN 'POPU'.
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(96),    /   SY-VLINE NO-GAP,
                   '유형'   NO-GAP,     SY-VLINE NO-GAP,
                   '메세지 텍스트',  94 SY-VLINE NO-GAP,
                   'T'      NO-GAP,     SY-VLINE,
                 / SY-ULINE(96).
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4) NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(87) NO-GAP,
                      SY-VLINE NO-GAP.

            CASE IT_ERR_LIST-MSGTYP.
               WHEN 'E'.
                  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
               WHEN 'W'.
                  FORMAT COLOR COL_KEY      INTENSIFIED OFF.
               WHEN 'I'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
               WHEN 'S'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
            ENDCASE.

            WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
*                   / SY-ULINE(96).
            HIDE:IT_ERR_LIST.
         ENDLOOP.
         WRITE : / SY-ULINE(96).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0200_LIST_CHECK_SCR0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE_1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE_1.
*>> 모선 헤더.
  SELECT SINGLE * FROM ZTMSHD
                  WHERE ZFMSNO EQ ZTMSCST-ZFMSNO.
*>> 문서번호.
  CONCATENATE ZTMSCST-ZFMSNO '-' ZTMSCST-ZFAPRTC INTO BKPF-XBLNR.
*>> 모선명칭.
  MOVE : ZTMSHD-ZFMSNM TO BKPF-BKTXT.
*--------------------------------------------------------------------
* J_1BT001WV ===> ZVT001W VIEW CREATE
*  Database View가 아니기 때문에 Select시 오류가 발생하는 것 같음????
*--------------------------------------------------------------------
  CLEAR : ZVT001W.
  SELECT SINGLE * FROM ZVT001W
                  WHERE WERKS EQ ZTMSCST-ZFWERKS.
  SELECT SINGLE * FROM LFA1
                  WHERE LIFNR EQ ZTMSCST-LIFNR.


  REFRESH : BDCDATA.
* 초기화면 FIELD
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0100',
      ' ' 'BKPF-BLDAT'   ZSMSCST-ZFOCDT,     " Document Date
      ' ' 'BKPF-BLART'  'SA',                " Type
      ' ' 'BKPF-BUKRS'   ZTMSCST-BUKRS,      " Company Code
      ' ' 'BKPF-BUDAT'   ZSMSCST-ZFPSDT,     " Posting Date
      ' ' 'BKPF-WAERS'  'KRW',               " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  BKPF-XBLNR,          " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  BKPF-BKTXT,          " 전표헤더텍스트.
      ' ' 'RF05A-PARGB' SPACE,               " 관계사 사업영역.
      ' ' 'RF05A-NEWBS' '01',                " Posting Key
      ' ' 'RF05A-NEWKO'  LFA1-KUNNR,         " Account(공급사).
      ' ' 'RF05A-NEWUM'  SPACE,              "다음 개별항목특별 G/L지시.
      ' ' 'RF05A-NEWBW'  SPACE,              " 자산거래유형.
      ' ' 'BDC_OKCODE'  '/00'.               " ENTER

*>> 미수금.
  WRITE ZTMSCST-ZFUNCO    CURRENCY 'KRW' TO TEMP_WRBTR.
* NEXT SCREEN.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0301',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
      ' ' 'BSEG-ZTERM'  ZTMSCST-ZTERM,         " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '미수금(조출료)',      " 텍스트.
      ' ' 'RF05A-NEWBS' '31',                  " Posting Key
      ' ' 'RF05A-NEWKO' ZTMSCST-ZFCARGO,       " ACCOUNT(하역사)
      ' ' 'BDC_OKCODE'  '/00'.                 " ENTER

*>> (미지급-하역사).
  WRITE ZTMSCST-ZFNOPY    CURRENCY 'KRW' TO TEMP_WRBTR.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0302',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,          " Amount
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'COBL-KOSTL'  COBL-KOSTL,            " Cost center.
*      ' ' 'COBL-PRCTR'  COBL-PRCTR,            " 손익센터.
      ' ' 'BSEG-SGTXT'  '하역사(미지급금)',    " 텍스트.
      ' ' 'RF05A-NEWBS' '50',                  " Posting Key
      ' ' 'RF05A-NEWKO' ZTIMIMG11-ZFMSCST3,    " ACCOUNT(영업외 수익)
      ' ' 'BDC_OKCODE'  '/00         '.        " ENTER.

*>> 영업외 수익.
  WRITE ZTMSCST-ZFPROF    CURRENCY 'KRW' TO TEMP_WRBTR.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0300',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
*      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
*      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
*      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
*      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
*      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'BSEG-ZTERM'  ZTMSCST-ZTERM,         " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
*      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '영업외 수익',         " 텍스트.
      ' ' 'BDC_OKCODE'  '=BU'.                 " ENTER

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   BSEG-GSBER,    " 사업영역.TEST
      ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
      ' ' 'COBL-PRCTR'   COBL-PRCTR,    " 손익센터.
      ' ' 'BDC_OKCODE'   '/00'.         " ENTER

ENDFORM.                    " P2000_BDC_DATA_MAKE_1
*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE_2.
*>> 모선 헤더.
  SELECT SINGLE * FROM ZTMSHD
                  WHERE ZFMSNO EQ ZTMSCST-ZFMSNO.
*>> 문서번호.
  CONCATENATE ZTMSCST-ZFMSNO '-' ZTMSCST-ZFAPRTC INTO BKPF-XBLNR.
*>> 모선명칭.
  MOVE : ZTMSHD-ZFMSNM TO BKPF-BKTXT.
*--------------------------------------------------------------------
* J_1BT001WV ===> ZVT001W VIEW CREATE
*  Database View가 아니기 때문에 Select시 오류가 발생하는 것 같음????
*--------------------------------------------------------------------
  CLEAR : ZVT001W.
  SELECT SINGLE * FROM ZVT001W
                  WHERE WERKS EQ ZTMSCST-ZFWERKS.
  SELECT SINGLE * FROM LFA1
                  WHERE LIFNR EQ ZTMSCST-LIFNR.

*-----------------------------------------------------------------------
*>> 부가세가 존재할 경우, 오류가 발생하므로..
*   2001.03.30  KSB INSERT
  ZTMSCST-MWSKZ = 'V0'.
*-----------------------------------------------------------------------

  REFRESH : BDCDATA.
* 초기화면 FIELD
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0100',
      ' ' 'BKPF-BLDAT'   ZSMSCST-ZFOCDT,     " Document Date
      ' ' 'BKPF-BLART'  'SA',                " Type
      ' ' 'BKPF-BUKRS'   ZTMSCST-BUKRS,      " Company Code
      ' ' 'BKPF-BUDAT'   ZSMSCST-ZFPSDT,     " Posting Date
      ' ' 'BKPF-WAERS'  'KRW',               " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  BKPF-XBLNR,          " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  BKPF-BKTXT,          " 전표헤더텍스트.
      ' ' 'RF05A-PARGB' SPACE,               " 관계사 사업영역.
      ' ' 'RF05A-NEWBS' '01',                " Posting Key
      ' ' 'RF05A-NEWKO'  LFA1-KUNNR,         " Account(공급사).
      ' ' 'RF05A-NEWUM'  SPACE,              "다음 개별항목특별 G/L지시.
      ' ' 'RF05A-NEWBW'  SPACE,              " 자산거래유형.
      ' ' 'BDC_OKCODE'  '/00'.               " ENTER

*>> 미수금.
  WRITE ZTMSCST-ZFUNCO    CURRENCY 'KRW' TO TEMP_WRBTR.
* NEXT SCREEN.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0301',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
*      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
      ' ' 'BSEG-ZTERM'  ZTMSCST-ZTERM,         " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '미수금(조출료)',      " 텍스트.
      ' ' 'RF05A-NEWBS' '50',                  " Posting Key
      ' ' 'RF05A-NEWKO' ZTIMIMG11-ZFMSCST3,    " ACCOUNT(영업외 수익)
      ' ' 'BDC_OKCODE'  '/00'.                 " ENTER

*>> 영업외 수익.
  WRITE ZTMSCST-ZFPROF    CURRENCY 'KRW' TO TEMP_WRBTR.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0300',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
*      ' ' 'BSEG-WMWST'  SPACE,                 " Tax
*      ' ' 'BKPF-XMWST'  SPACE,                 " 세금을 자동으로 계산.
*      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
*      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
*      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'BSEG-ZTERM'  ZTMSCST-ZTERM,         " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
*      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '영업외 수익',         " 텍스트.
      ' ' 'BDC_OKCODE'  '=BU'.                 " ENTER

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   BSEG-GSBER,    " 사업영역.TEST
      ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
      ' ' 'COBL-PRCTR'   COBL-PRCTR,    " 손익센터.
      ' ' 'BDC_OKCODE'   '/00'.         " ENTER

ENDFORM.                    " P2000_BDC_DATA_MAKE_2
*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE_3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE_3.
*>> 모선 헤더.
  SELECT SINGLE * FROM ZTMSHD
                  WHERE ZFMSNO EQ ZTMSCST-ZFMSNO.
*>>>
*  SELECT SINGLE * FROM  ZTMSCST
*                 WHERE ZFMSNO   EQ   IT_SELECTED-ZFMSNO
*                 AND   ZFAPRTC  EQ   IT_SELECTED-ZFAPRTC.

*>> 문서번호.
  CONCATENATE ZTMSCST-ZFMSNO '-' ZTMSCST-ZFAPRTC INTO BKPF-XBLNR.
*>> 모선명칭.
  MOVE : ZTMSHD-ZFMSNM TO BKPF-BKTXT.
*--------------------------------------------------------------------
* J_1BT001WV ===> ZVT001W VIEW CREATE
*  Database View가 아니기 때문에 Select시 오류가 발생하는 것 같음????
*--------------------------------------------------------------------
  CLEAR : ZVT001W.
  SELECT SINGLE * FROM ZVT001W
                  WHERE WERKS EQ ZTMSCST-ZFWERKS.

*  SELECT SINGLE * FROM LFA1
*                  WHERE LIFNR EQ ZTMSCST-ZFCARGO.

  REFRESH : BDCDATA.
* 초기화면 FIELD
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0100',
      ' ' 'BKPF-BLDAT'   ZSMSCST-ZFOCDT,     " Document Date
      ' ' 'BKPF-BLART'  'SA',                " Type
      ' ' 'BKPF-BUKRS'   ZTMSCST-BUKRS,      " Company Code
      ' ' 'BKPF-BUDAT'   ZSMSCST-ZFPSDT,     " Posting Date
      ' ' 'BKPF-WAERS'  'KRW',               " Currency
      ' ' 'BKPF-KURSF'  '',                  " 환율.
      ' ' 'BKPF-BELNR'  SPACE,               " 회계전표번호.
      ' ' 'BKPF-WWERT'  SPACE,               " 환산일.
      ' ' 'BKPF-XBLNR'  BKPF-XBLNR,          " 참조문서번호.
      ' ' 'BKPF-BVORG'  SPACE,               " 회사코드간 거래번호.
      ' ' 'BKPF-BKTXT'  BKPF-BKTXT,          " 전표헤더텍스트.
      ' ' 'RF05A-PARGB' SPACE,               " 관계사 사업영역.
*      ' ' 'RF05A-NEWBS' '40',                " Posting Key
*      ' ' 'RF05A-NEWKO' ZTIMIMG11-ZFMSCST4,  " ACCOUNT(영업외 비용)
      ' ' 'RF05A-NEWBS' '31',                " Posting Key
      ' ' 'RF05A-NEWKO'  ZTMSCST-ZFCARGO,    " ACCOUNT(미지급금).
      ' ' 'RF05A-NEWUM'  SPACE,              "다음 개별항목특별 G/L지시.
      ' ' 'RF05A-NEWBW'  SPACE,              " 자산거래유형.
      ' ' 'BDC_OKCODE'  '/00'.               " ENTER

*>> 조출료(미지급).
  WRITE ZTMSCST-ZFNOPY    CURRENCY 'KRW' TO TEMP_WRBTR.
*  WRITE ZTMSCST-ZFVAT    CURRENCY 'KRW' TO TEMP_WRBTR1.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0302',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
*      ' ' 'BSEG-WMWST'  TEMP_WRBTR1,           " Tax
      ' ' 'BKPF-XMWST'  'X',                   " 세금을 자동으로 계산.
      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'COBL-KOSTL'  COBL-KOSTL,            " Cost center.
*      ' ' 'COBL-PRCTR'  COBL-PRCTR,            " 손익센터.
      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '조출료(미지급금)',    " 텍스트.
      ' ' 'RF05A-NEWBS' '40',                  " Posting Key
      ' ' 'RF05A-NEWKO' ZTIMIMG11-ZFMSCST4,    " ACCOUNT(영업외 비용)
      ' ' 'BDC_OKCODE'  '/00'.                 " enter

*>> 영업외 비용.
  WRITE ZTMSCST-ZFNOPY   CURRENCY 'KRW' TO TEMP_WRBTR.
  PERFORM P2000_DYNPRO USING :
      'X' 'SAPMF05A'    '0300',
      ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
*      ' ' 'BSEG-WMWST'  'X',                  " Tax
*      ' ' 'BKPF-XMWST'  'X',                   " 세금을 자동으로 계산.
      ' ' 'BSEG-MWSKZ'  ZTMSCST-MWSKZ,         " Tax Code
      ' ' 'BSEG-BUPLA'  ZVT001W-J_1BBRANCH,    " Business Place
*      ' ' 'BSEG-SECCO'  SPACE,                 " 섹션코드.
*      ' ' 'BSEG-GSBER'  BSEG-GSBER,            " Business Area
*      ' ' 'BSEG-ZTERM'  ZTMSCST-ZTERM,         " Payment Term
*      ' ' 'BSEG-EMPFB'  L_ZFPAY,               " Payee
*      ' ' 'BSEG-ZLSPR'  'B',                   " 지급보류.
      ' ' 'BSEG-SGTXT'  '영업외 비용',         " 텍스트.
*      ' ' 'RF05A-NEWBS' '31',                  " Posting Key
*      ' ' 'RF05A-NEWKO'  ZTMSCST-ZFCARGO,      " ACCOUNT(미지급금).
      ' ' 'BDC_OKCODE'  '=BU'.                 " SAVE

  PERFORM P2000_DYNPRO USING :
      'X' 'SAPLKACB'     '0002',
      ' ' 'COBL-GSBER'   BSEG-GSBER,    " 사업영역.TEST
      ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
      ' ' 'COBL-PRCTR'   COBL-PRCTR,    " 손익센터.
      ' ' 'BDC_OKCODE'   '/00'.         " ENTER

ENDFORM.                    " P2000_BDC_DATA_MAKE_3
*&---------------------------------------------------------------------*
*&      Form  P2000_GET_SELECT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_GET_SELECT_LINE.
DATA : L_ZFMSNO  LIKE   ZTMSCST-ZFMSNO,
       L_ZFAPRTC LIKE   ZTMSCST-ZFAPRTC.
  CLEAR W_PROC_CNT.

  MOVE : IT_TAB-ZFMSNO   TO   L_ZFMSNO,
         IT_TAB-ZFAPRTC  TO   L_ZFAPRTC.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
       SELECT SINGLE * FROM ZTMSCST
              WHERE    ZFMSNO  EQ   IT_TAB-ZFMSNO
              AND      ZFAPRTC EQ   IT_TAB-ZFAPRTC.
       W_PROC_CNT  =  W_PROC_CNT  +  1.
    ENDIF.
  ENDDO.

  IF W_PROC_CNT EQ 0.
     IF NOT L_ZFMSNO IS INITIAL.
        SELECT SINGLE * FROM ZTMSCST
               WHERE    ZFMSNO  EQ   L_ZFMSNO
               AND      ZFAPRTC EQ   L_ZFAPRTC.
        W_PROC_CNT  =  W_PROC_CNT  +  1.
     ENDIF.
  ENDIF.

ENDFORM.                    " P2000_GET_SELECT_LINE
*&---------------------------------------------------------------------*
*&      Form  P2000_DOC_CHANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DOC_CHANGE.
  SPOP-TITEL = '조출/체선료 상태변경 및 조회'.
  OPTION = 1.

  SELECT SINGLE * FROM ZTMSHD
         WHERE  ZFMSNO   EQ   ZTMSCST-ZFMSNO.

  CALL SCREEN 0300 STARTING AT 5  3
                   ENDING   AT 85 18.

ENDFORM.                    " P2000_DOC_CHANGE
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCR0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCR0300 INPUT.
  ANTWORT = 'N'.
  SET SCREEN 0.   LEAVE SCREEN.
ENDMODULE.                 " USER_EXIT_SCR0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  P3000_CANC_MAT_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_CANC_MAT_INVOICE_VERIFY.
  CLEAR  W_PROC_CNT.
  LOOP AT IT_SELECTED.
*>> 부가세 CANCEL.
     IF IT_SELECTED-BELNRV  NE  SPACE.
         CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
              EXPORTING
                P_ZFIMDTY        =     'MS'
                P_BUKRS          =     IT_SELECTED-BUKRS
                INVOICEDOCNUMBER =     IT_SELECTED-BELNRV
                FISCALYEAR       =     IT_SELECTED-GJAHRV
                REASONREVERSAL   =     UF05A-STGRD
                POSTINGDATE      =     BSIS-BUDAT
                P_GUBUN          =     'V'
             IMPORTING
                INVOICEDOCNUMBER_REVERSAL =     ZFACDO
                FISCALYEAR_REVERSAL       =     ZFFIYR
             TABLES
                RETURN           =     RETURN
             EXCEPTIONS
                LIV_ERROR        =     4.

         IF SY-SUBRC NE 0.           ">> 오류 발생시...
*            IF RETURN[] IS INITIAL.
*              PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
*           ELSE.
               PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
*           ENDIF.
            ADD    1    TO    W_ERR_CNT.
         ELSE.
            PERFORM  P2000_MULTI_MSG_MAKE    TABLES  IT_ERR_LIST.
         ENDIF.
      ENDIF.
*>> 조출/체선료 CANCEL.
      CALL FUNCTION 'ZIM_BAPI_COST_INVOICES_CANCEL'
           EXPORTING
             P_ZFIMDTY        =     'MS'
             P_BUKRS          =     IT_SELECTED-BUKRS
             INVOICEDOCNUMBER =     IT_SELECTED-BELNR
             FISCALYEAR       =     IT_SELECTED-GJAHR
             REASONREVERSAL   =     UF05A-STGRD
             POSTINGDATE      =     BSIS-BUDAT
             P_GUBUN          =     'B'
          IMPORTING
             INVOICEDOCNUMBER_REVERSAL =     ZFACDO
             FISCALYEAR_REVERSAL       =     ZFFIYR
          TABLES
             RETURN           =     RETURN
          EXCEPTIONS
             LIV_ERROR        =     4.

      IF SY-SUBRC NE 0.           ">> 오류 발생시...
*         IF RETURN[] IS INITIAL.
*           PERFORM  P2000_MESSAGE_MAKE    TABLES  IT_ERR_LIST.
*        ELSE.
            PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST.
*         ENDIF.
         ADD    1    TO    W_ERR_CNT.
      ELSE.
         PERFORM  P2000_MULTI_MSG_MAKE    TABLES  IT_ERR_LIST.
         ADD    1    TO    W_PROC_CNT.
      ENDIF.

  ENDLOOP.

ENDFORM.                    " P3000_CANC_MAT_INVOICE_VERIFY
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ERR_LIST  text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST.

   LOOP AT  RETURN.

      MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
             RETURN-ID           TO     IT_ERR_LIST-MSGID,
             RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
             RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
             RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
             RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
             RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
             RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
             IT_SELECTED-BUKRS   TO     IT_ERR_LIST-BUKRS,
             IT_SELECTED-GJAHR   TO     IT_ERR_LIST-GJAHR,
             IT_SELECTED-BELNR   TO     IT_ERR_LIST-BELNR.

      CASE IT_ERR_LIST-MSGTYP.
         WHEN 'E'.
            MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
         WHEN 'I'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'S'.
            MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
         WHEN 'W'.
            MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
      ENDCASE.

      APPEND  IT_ERR_LIST.

   ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0010 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF UF05A-STGRD IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
     PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0010 INPUT.

  IF OK-CODE NE 'YES'.
     SET SCREEN 0.
     LEAVE SCREEN.
  ENDIF.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
     WHEN 'POPU'.
        SET TITLEBAR 'POPU' WITH '오류 LIST'.
     WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

   LEAVE TO LIST-PROCESSING.
   CASE INCLUDE.
      WHEN 'POPU'.
         FORMAT COLOR COL_HEADING INTENSIFIED OFF.
         WRITE : / SY-ULINE(105), / SY-VLINE NO-GAP,
                   '유형'   NO-GAP, SY-VLINE NO-GAP,
                   ' 문서번호 ' NO-GAP, SY-VLINE NO-GAP,
                   '메세지 텍스트', 103 SY-VLINE NO-GAP,
                   'T'      NO-GAP, SY-VLINE,
                 / SY-ULINE(105).
*         MESSAGE
         LOOP AT IT_ERR_LIST.
            W_MOD  =  SY-TABIX MOD 2.
            FORMAT RESET.
            IF W_MOD EQ 0.
               FORMAT COLOR COL_NORMAL  INTENSIFIED ON.
            ELSE.
               FORMAT COLOR COL_NORMAL  INTENSIFIED OFF.
            ENDIF.
            WRITE : / SY-VLINE NO-GAP, IT_ERR_LIST-ICON(4)   NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-BELNR       NO-GAP,
                      SY-VLINE NO-GAP, IT_ERR_LIST-MESSTXT(85) NO-GAP,
                      SY-VLINE NO-GAP.

            CASE IT_ERR_LIST-MSGTYP.
               WHEN 'E'.
                  FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
               WHEN 'W'.
                  FORMAT COLOR COL_KEY      INTENSIFIED OFF.
               WHEN 'I'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
               WHEN 'S'.
                  FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
            ENDCASE.

            WRITE : IT_ERR_LIST-MSGTYP(1) NO-GAP, SY-VLINE NO-GAP.
*                   / SY-ULINE(96).
            HIDE:IT_ERR_LIST.
         ENDLOOP.
         WRITE : / SY-ULINE(105).
         CLEAR : IT_ERR_LIST.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_FI_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SELECTED_ZFACDO  text
*      -->P_IT_SELECTED_BUKRS  text
*      -->P_IT_SELECTED_ZFFIYR  text
*----------------------------------------------------------------------*
FORM P2000_FI_DOC_DISPLAY USING    P_BELNR
                                   P_BUKRS
                                   P_GJAHR.
  IF P_BELNR IS INITIAL.
     MESSAGE S814.
     EXIT.
  ENDIF.
  IF P_BUKRS IS INITIAL.
     MESSAGE S167 WITH '회사코드'.
     EXIT.
  ENDIF.
  IF P_GJAHR IS INITIAL.
     MESSAGE S167 WITH '회계년도'.
     EXIT.
  ENDIF.

  SET PARAMETER ID 'BLN' FIELD P_BELNR.
  SET PARAMETER ID 'BUK' FIELD P_BUKRS.
  SET PARAMETER ID 'GJR' FIELD P_GJAHR.

  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.


ENDFORM.                    " P2000_FI_DOC_DISPLAY
