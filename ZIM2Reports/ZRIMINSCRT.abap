*&---------------------------------------------------------------------*
*& Report  ZRIMINSCRT                                                  *
*&---------------------------------------------------------------------*
*&  Program : Insurance document Multi-Create Program                  *
*&  Created by : 강석봉 INFOLINK Ltd.                                  *
*&  Created on : 2000.04.12                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*& [Change Information]
*&---------------------------------------------------------------------*
REPORT  ZRIMINSCRT   MESSAGE-ID ZIM
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

TABLES : ZTINS.
*-----------------------------------------------------------------------
* INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,                        " MARK
       UPDATE_CHK TYPE C,                        " DB Update Y/N
       ZFOPNDT    LIKE ZTREQST-ZFOPNDT,          " Open Date
       ZFREQDT    LIKE ZTREQST-ZFREQDT,          " Requested Open Date
       ZFMAUD     LIKE ZTREQHD-ZFMAUD,           " Material Requested DT
       EBELN      LIKE ZTREQHD-EBELN,            " Purchasing document
       ZFREQNO    LIKE ZTREQHD-ZFREQNO,          " Import Request No
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
       ZFOPAMT1(18) TYPE C,                      " Open Amount
       WAERS      LIKE ZTREQST-WAERS,            " Currency
       ZFUSDAM1(18) TYPE C,                      " USD Convert Amount
       ZFUSD      LIKE ZTREQST-ZFUSD,            " USD Currency
       ZFREQTY    LIKE ZTREQST-ZFREQTY,          " Import Type
       ZFMATGB    LIKE ZTREQHD-ZFMATGB,          " Material Type
       ZFBACD     LIKE ZTREQHD-ZFBACD,           " Before/After
       EKORG      LIKE ZTREQST-EKORG,            " Purchasing organizati
       EKGRP      LIKE ZTREQST-EKGRP,            " Purchasing group
       ZTERM      LIKE ZTREQHD-ZTERM,            " Terms of Payment
       ZFWERKS    LIKE ZTREQHD-ZFWERKS,          " Plant
       ERNAM      LIKE ZTREQST-ERNAM,            " Purchasing Person
       LIFNR      LIKE ZTREQHD-LIFNR,            " Vendor Code
       NAME1(17),                                " Name 1
       ZFBENI     LIKE ZTREQHD-ZFBENI,           " Beneficairy
       NAME2(17),                                " Name 1
       ZFOPBN     LIKE ZTREQHD-ZFBENI,           " Open Bank
       NAME3(17),                                " Name 1
       ZFRLST2    LIKE ZTREQST-ZFRLST2,          " Open Release Status
       ZFRLDT2    LIKE ZTREQST-ZFRLDT2,          " Open Release Date
       ZFRLNM2    LIKE ZTREQST-ZFRLNM2,          " Open Release Person
       ZFCLOSE    LIKE ZTREQHD-ZFCLOSE,          " Close Yes/No
       ZFRLST1    LIKE ZTREQST-ZFRLST1,          " P/O Release Y/N
       ZFSPRT(18) TYPE C,                        " Loading Port
       ZFAPRT(18) TYPE C,                        " Arriving Port
       INCO1      LIKE ZTREQHD-INCO1,            " Incoterms
       ZFTRANS    LIKE ZTREQHD-ZFTRANS,          " VIA
       ZFLEVN     LIKE ZTREQHD-ZFLEVN,           " Loan Organization
       NAME4(11),                                " Name 1
       ZFREF1(11),                               " remark
       ZFOPAMT    LIKE ZTREQST-ZFOPAMT,          " Open Amount
       ZFUSDAM    LIKE ZTREQST-ZFUSDAM.          " USD Convert Amount
DATA : END OF IT_TAB.

DATA : W_ZFAMDNO       LIKE    ZTINS-ZFAMDNO.
*-----------------------------------------------------------------------
* Tables & Variance Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.
INCLUDE   ZRIMBDCCOM.
INCLUDE   ZRIMSORTCOM.
INCLUDE   ZRIMUTIL01.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                            NO INTERVALS,
                S_OPNDT   FOR ZTREQST-ZFOPNDT,
                S_OPBN    FOR ZTREQHD-ZFOPBN,
                S_WERKS   FOR ZTREQHD-ZFWERKS,
                S_EKORG   FOR ZTREQST-EKORG.
SELECT-OPTIONS: S_EBELN   FOR ZTREQHD-EBELN,
                S_ZFBENI  FOR ZTREQHD-ZFBENI,
                S_EKGRP   FOR ZTREQST-EKGRP,
                S_REQNO   FOR ZTREQHD-ZFREQNO.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN.
  DISP_MODE = 'N'.

* PARAMETER 초기값 Setting
INITIALIZATION.
  PERFORM   P2000_SET_PARAMETER.
  PERFORM   P1000_SET_BUKRS.

* Title Text Write
TOP-OF-PAGE.
  IF SY-LANGU EQ '3'.
     PERFORM   P3000_TITLE_WRITE.
  ELSE.
     PERFORM   P3000_TITLE_WRITE_EN.
  ENDIF.
*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 권한 검증 함수.
  PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 파라메타 설정.
  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* RELEASE 여부 CHECK.
  PERFORM    P1000_GET_RELEASE_DATA.

* 구매의뢰 테이블 SELECT.
  PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 Text Table SELECT.
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write.
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'STUP' OR 'STDN'.         " SORT 선택?
      W_FIELD_NM = 'ZFOPBN'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
    WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.

    WHEN 'DISP'.          " L/C 조?
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'CRTD'.          " insurance Document
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES NE 0.
        PERFORM P2000_POPUP_MESSAGE.
        IF W_BUTTON_ANSWER EQ '1'.
          PERFORM  P2000_BDC_DATA_MAKE.
          PERFORM  P1000_RESET_LIST.
*               LEAVE TO SCREEN 0.
        ENDIF.
      ELSE.
        MESSAGE E032.
      ENDIF.
    WHEN 'LGSD'.          " insurance Document
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES NE 0.
        PERFORM P2000_SEND_MESSAGE.     " 메세지 박?
        IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
          PERFORM  P2000_BDC_DATA_MAKE1.
          PERFORM  P1000_RESET_LIST.
*               LEAVE TO SCREEN 0.
        ENDIF.
      ELSE.
        MESSAGE E032.
      ENDIF.
    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
      PERFORM  P1000_RESET_LIST.
    WHEN OTHERS.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM40'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /51  '[ Insurance 생성 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 112 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '걔설확정'    ,  SY-VLINE NO-GAP,
            'P/O Number'    NO-GAP,  SY-VLINE NO-GAP,
            'CUR. '         NO-GAP,  SY-VLINE NO-GAP,
 '    개설 금액     '       NO-GAP,  SY-VLINE NO-GAP,
            'Ty'            NO-GAP,  SY-VLINE NO-GAP,
            'Mat'           NO-GAP,  SY-VLINE NO-GAP,
            'Pay.'          NO-GAP,  SY-VLINE NO-GAP,
    '     선  적  지     '  NO-GAP,  SY-VLINE NO-GAP,
            'Inc'           NO-GAP,  SY-VLINE NO-GAP,
            'Vendor    '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            ' 차입기관  '   NO-GAP,  SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '자재납기'    ,  SY-VLINE NO-GAP,
            '수입의뢰No'    NO-GAP,  SY-VLINE NO-GAP,
            '     '         NO-GAP,  SY-VLINE NO-GAP,
 '   USD 환산금액   '       NO-GAP,  SY-VLINE NO-GAP,
            'TT'            NO-GAP,  SY-VLINE NO-GAP,
            'PGr'           NO-GAP,  SY-VLINE NO-GAP,
            'Plnt'          NO-GAP,  SY-VLINE NO-GAP,
    '     도  착  지     '  NO-GAP,  SY-VLINE NO-GAP,
            'VIA'           NO-GAP,  SY-VLINE NO-GAP,
            'Bene.     '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            ' 참조사항  '   NO-GAP,  SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.

* 수입 CONFIG 관련 정보 GET.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFINMT NE '1'.
     MESSAGE  S111(ZIM1).
     W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

  IF DISP_MODE NE 'N'.            " BACKGROUND이 아닐 경?
    AUTHORITY-CHECK OBJECT 'ZM_BDC_MGT'
            ID 'ACTVT' FIELD '*'.

    IF SY-SUBRC NE 0.
      MESSAGE S960 WITH SY-UNAME 'BDC 관리자'.
      W_ERR_CHK = 'Y'.   EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.
*  IF P_NAME IS INITIAL.       P_NAME  =  '%'.      ENDIF.
*  concatenate p_name '%' into p_name.
ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
FORM P1000_GET_ZVREQHD_ST   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO TABLE IT_ZVREQ FROM ZVREQHD_ST
                               WHERE ZFOPNDT    IN     S_OPNDT
                               AND   BUKRS      IN     S_BUKRS
                               AND   ZFOPBN     IN     S_OPBN
                               AND   ZFWERKS    IN     S_WERKS
                               AND   EKORG      IN     S_EKORG
*                               and   ernam      like   p_name
                               AND   EBELN      IN     S_EBELN
*                               and   lifnr      in     s_lifnr
                               AND   ZFBENI     IN     S_ZFBENI
                               AND   EKGRP      IN     S_EKGRP
                               AND   ZFREQNO    IN     S_REQNO
                               AND   ZFRLST1    IN     R_ZFRLST1
*                               and   zfdocst eq 'O'
*>> 내륙운송일 경우.
*                               and   zfreqty ne 'PU'
*                               and   zfreqty ne 'LO'
                               AND ( ZFINSYN EQ 'A'
                               OR    ZFINSYN EQ 'Z' )
                               AND   ZFRTNYN EQ SPACE
                               AND   ZFCLOSE EQ SPACE
                               AND   ZFINSYN NE SPACE.

  IF SY-SUBRC NE 0.               " Not Found?
    W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.
  REFRESH : IT_TAB.

  SORT  IT_ZVREQ BY ZFREQNO ZFAMDNO DESCENDING.

  LOOP AT IT_ZVREQ.
    W_TABIX = SY-TABIX.
    READ TABLE IT_TAB WITH KEY ZFREQNO = IT_ZVREQ-ZFREQNO.
    IF SY-SUBRC EQ 0.
      CONTINUE.
    ENDIF.

*-----------------------------------------------------------------------
* 보험 문서 조?
*-----------------------------------------------------------------------
    SELECT COUNT( * ) INTO W_COUNT
                           FROM ZTINS
                           WHERE ZFREQNO EQ IT_ZVREQ-ZFREQNO.
    IF W_COUNT GT 0.    CONTINUE.     ENDIF.
*     SELECT MAX( ZFINSEQ )  INTO W_ZFINSEQ
*                            FROM ZTINS
*                            WHERE ZFREQNO EQ IT_ZVREQ-ZFREQNO.

*     SELECT MAX( ZFAMDNO )  INTO W_ZFAMDNO
*                            FROM ZTINS
*                            WHERE ZFREQNO EQ IT_ZVREQ-ZFREQNO.
**                           AND   ZFINSEQ EQ W_ZFINSEQ.

*     SELECT SINGLE * FROM ZTINS
*                            WHERE ZFREQNO EQ IT_ZVREQ-ZFREQNO
**                           AND   ZFINSEQ EQ W_ZFINSEQ
*                            AND   ZFAMDNO EQ W_ZFAMDNO.

*     IF SY-SUBRC EQ 0 AND ZTINS-ZFDOCST NE 'O'.
*        CONTINUE.
*     ENDIF.
* MOVE

    MOVE-CORRESPONDING IT_ZVREQ  TO  IT_TAB.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-LIFNR
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
      WHEN 01.     MESSAGE E022.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
    ENDCASE.
    MOVE: LFA1-NAME1   TO   IT_TAB-NAME1.
*-----------------------------------------------------------------------
* Bene. MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFBENI
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
      WHEN 01.     MESSAGE E022.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
    ENDCASE.
    MOVE: LFA1-NAME1   TO   IT_TAB-NAME2.

*-----------------------------------------------------------------------
* Opeb Bank. MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFOPBN
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
*        WHEN 01.     MESSAGE E022.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    IT_TAB-ZFOPBN.
    ENDCASE.
    MOVE: LFA1-NAME1   TO   IT_TAB-NAME3.
*-----------------------------------------------------------------------
* 차입기관   MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFLEVN
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    CASE SY-SUBRC.
*        WHEN 01.     MESSAGE E022.
      WHEN 02.     MESSAGE E950.
      WHEN 03.     MESSAGE E020   WITH    IT_TAB-ZFLEVN.
    ENDCASE.
    MOVE: LFA1-NAME1   TO   IT_TAB-NAME4.

    WRITE : IT_TAB-ZFOPAMT  CURRENCY IT_TAB-WAERS TO IT_TAB-ZFOPAMT1,
            IT_TAB-ZFUSDAM  CURRENCY IT_TAB-ZFUSD TO IT_TAB-ZFUSDAM1.

    APPEND  IT_TAB.
  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE < 1.
    W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.

  SORT  IT_TAB  BY ZFREQNO ZFAMDNO.

ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  READ TABLE IT_ZVREQ INDEX 1.

  SELECT SINGLE *
           FROM ZTIMIMGTX
          WHERE BUKRS = IT_ZVREQ-BUKRS.

  IF ZTIMIMGTX-ZFEDIYN EQ 'X'
      AND  ZTIMIMGTX-APPCIP EQ 'X'
        AND  ZTIMIMGTX-ZAPPEND EQ 'X'.
    SET PF-STATUS 'ZIM06'.           " GUI STATUS SETTING
    SET  TITLEBAR 'ZIM40'.           " GUI TITLE SETTING..
  ELSE.
    SET PF-STATUS 'ZIM06_NE'.        " GUI STATUS SETTING
    SET  TITLEBAR 'ZIM40'.           " GUI TITLE SETTING..
  ENDIF.

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
    W_LINE = W_LINE + 1.
    PERFORM P2000_PAGE_CHECK.
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
  IF SY-LANGU EQ '3'.
     PERFORM   P3000_TITLE_WRITE.
  ELSE.
     PERFORM   P3000_TITLE_WRITE_EN.
  ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFREQNO LIKE ZTREQST-ZFREQNO,
        ZFAMDNO LIKE ZTREQST-ZFAMDNO,
        ZFRLST1 LIKE ZTREQST-ZFRLST1,
        ZFRLST2 LIKE ZTREQST-ZFRLST2.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFREQNO  TO ZFREQNO,
         IT_TAB-ZFAMDNO  TO ZFAMDNO,
         IT_TAB-ZFRLST1  TO ZFRLST1,
         IT_TAB-ZFRLST2  TO ZFRLST2.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO,
             IT_TAB-ZFRLST1  TO IT_SELECTED-ZFRLST1,
             IT_TAB-ZFRLST2  TO IT_SELECTED-ZFRLST2.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
             ZFAMDNO TO IT_SELECTED-ZFAMDNO,
             ZFRLST1 TO IT_SELECTED-ZFRLST1,
             ZFRLST2 TO IT_SELECTED-ZFRLST2.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                    " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / 'Total', W_COUNT, 'Case'.
  ENDIF.

ENDFORM.                    " P3000_LAST_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

* IF SY-UCOMM EQ 'FRGS' OR SY-UCOMM EQ 'FRGR'.
*    IF IT_TAB-MARK EQ 'X' AND IT_TAB-UPDATE_CHK EQ 'U'.
*       MARKFIELD = 'X'.
*    ELSE.
*       CLEAR : MARKFIELD.
*    ENDIF.
* ENDIF.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPNDT NO-GAP,            " 개설확정?
       SY-VLINE NO-GAP,
       IT_TAB-EBELN   NO-GAP,            " 구매문?
       SY-VLINE NO-GAP,
       IT_TAB-WAERS NO-GAP,              " currency
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPAMT  CURRENCY IT_TAB-WAERS NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQTY NO-GAP,            " 결제 구?
       SY-VLINE,
       IT_TAB-ZFMATGB,                   " 자재 구?
       SY-VLINE NO-GAP,
       IT_TAB-ZTERM NO-GAP,              " Payment Terms
       SY-VLINE NO-GAP,
       IT_TAB-ZFSPRT  NO-GAP,               " 선적?
    85 SY-VLINE NO-GAP,
       IT_TAB-INCO1   NO-GAP,               " Incoterms
       SY-VLINE NO-GAP,
       IT_TAB-LIFNR   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-NAME1   NO-GAP,
   118 SY-VLINE NO-GAP.
  WRITE : IT_TAB-NAME4 NO-GAP, SY-VLINE.         " 차입기관?

* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',
       SY-VLINE NO-GAP,
       IT_TAB-ZFMAUD,                       " 자재납기?
    16 SY-VLINE NO-GAP,
       IT_TAB-ZFREQNO NO-GAP,               " 수입의?
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSD NO-GAP,                 " 통화 단?
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSDAM  CURRENCY IT_TAB-ZFUSD NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBACD,                       " 사전 / 사후 구?
       SY-VLINE NO-GAP,
       IT_TAB-EKGRP NO-GAP,                 " Purchasing Group
       SY-VLINE NO-GAP,
       IT_TAB-ZFWERKS NO-GAP,               " 대표 plant
       SY-VLINE NO-GAP,
       IT_TAB-ZFAPRT  NO-GAP,             " 도착?
    85 SY-VLINE,
       IT_TAB-ZFTRANS,                    " VIA
       SY-VLINE NO-GAP,
       IT_TAB-ZFBENI  NO-GAP,             " Beneficiary
       SY-VLINE NO-GAP,
       IT_TAB-NAME2   NO-GAP,
   118 SY-VLINE NO-GAP,
       IT_TAB-ZFREF1 NO-GAP,               " 연락사?
       SY-VLINE NO-GAP.
* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.

  CALL  FUNCTION  'POPUP_TO_CONFIRM'
        EXPORTING
            TITLEBAR        = 'Insurance Creation Confirm'
            DIAGNOSE_OBJECT = ''
            TEXT_QUESTION   =
                 'Do you want to creat insurance?'
            TEXT_BUTTON_1   = 'Confirm'
            TEXT_BUTTON_2   = 'No'
            DEFAULT_BUTTON  = '1'
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN    = 30
            START_ROW       = 8
        IMPORTING
            ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.
  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  EXPORT 'BES'           TO MEMORY ID 'BES'.
  EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
  EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.

  CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P1000_RESET_LIST
*&---------------------------------------------------------------------*
FORM P1000_RESET_LIST.
* 구매의뢰 테이블 SELECT
  PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
  PERFORM RESET_LIST.

ENDFORM.                    " P1000_RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE
*&---------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE.
  REFRESH : BDCDATA.
  LOOP AT IT_SELECTED.
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.

    REFRESH : BDCDATA.
* 초기화면 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '4100'.
* 초기화면 FIELD
    PERFORM P2000_DYNPRO USING :
         ' ' 'ZSREQHD-ZFOPNNO' '',                  " 문서번?
         ' ' 'ZSREQHD-EBELN'   '',                  " P/O No.
         ' ' 'ZSREQHD-ZFREQNO' IT_SELECTED-ZFREQNO, " Import No.
         ' ' 'BDC_OKCODE'      '/00'.               " ENTER
* 주화면   CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '4101'.
* 저?
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'SAVE'.
* 저장 확인 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '0001'.
* 저?
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION
    PERFORM P2000_CALL_TRANSACTION  USING 'ZIM41'
                                    CHANGING  W_SUBRC.
* 오류 발생시 LOG
    IF W_SUBRC NE 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_BDC_DATA_MAKE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_RELEASE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_RELEASE_DATA.
  REFRESH : R_ZFRLST1.

  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE E961.   EXIT.
  ENDIF.
*-----------------------------------------------------------------------
* 구매 릴리즈 사용 여?
*-----------------------------------------------------------------------
  CLEAR R_ZFRLST1.
  IF  ZTIMIMG00-ZFRELYN1 EQ 'X'.
    MOVE: 'I'      TO R_ZFRLST1-SIGN,
          'EQ'     TO R_ZFRLST1-OPTION,
          'R'      TO R_ZFRLST1-LOW.
    APPEND R_ZFRLST1.
  ELSE.
    MOVE: 'I'      TO R_ZFRLST1-SIGN,
          'EQ'     TO R_ZFRLST1-OPTION,
          'N'      TO R_ZFRLST1-LOW.
    APPEND R_ZFRLST1.
  ENDIF.

ENDFORM.                    " P1000_GET_RELEASE_DATA
*&---------------------------------------------------------------------*
*&      Form  P2000_SEND_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SEND_MESSAGE.

  CALL  FUNCTION  'POPUP_TO_CONFIRM'
        EXPORTING
            TITLEBAR        = '보험부보 신청 확인'
            DIAGNOSE_OBJECT = ''
            TEXT_QUESTION   =
                 '적하보험 부보신청 작업을 계속 진행하시겠습니까?'
            TEXT_BUTTON_1   = '확    인'
            TEXT_BUTTON_2   = '아 니 오'
            DEFAULT_BUTTON  = '1'
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN    = 30
            START_ROW       = 8
        IMPORTING
            ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_SEND_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_BDC_DATA_MAKE1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BDC_DATA_MAKE1.

  REFRESH : BDCDATA.

  LOOP AT IT_SELECTED.
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.

    REFRESH : BDCDATA.
* 초기화면 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '4100'.
* 초기화면 FIELD
    PERFORM P2000_DYNPRO USING :
         ' ' 'ZSREQHD-ZFOPNNO' '',                  " 문서번?
         ' ' 'ZSREQHD-EBELN'   '',                  " P/O No.
         ' ' 'ZSREQHD-ZFREQNO' IT_SELECTED-ZFREQNO, " Import No.
         ' ' 'BDC_OKCODE'      '/00'.               " ENTER
* 주화면   CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '4101'.
* 저?
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'LGIS'.
* 저장 확인 CALL
    PERFORM P2000_DYNPRO USING 'X' 'SAPMZIM00' '0001'.
* 저?
    PERFORM P2000_DYNPRO USING ' ' 'BDC_OKCODE' 'YES'.
* CALL TRANSACTION
    PERFORM P2000_CALL_TRANSACTION  USING 'ZIM41'
                                    CHANGING  W_SUBRC.
* 오류 발생시 LOG
    IF W_SUBRC NE 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
      MESSAGE ID SY-MSGID TYPE 'I' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_BDC_DATA_MAKE1
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

*>> Company code SET.
    MOVE: 'I'          TO S_BUKRS-SIGN,
          'EQ'         TO S_BUKRS-OPTION,
          P_BUKRS      TO S_BUKRS-LOW.
    APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE_EN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TITLE_WRITE_EN.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /51  '[ Insurance Creation Object ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 112 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            'Open DT '   ,  SY-VLINE NO-GAP,
            'P/O Number'    NO-GAP,  SY-VLINE NO-GAP,
            'CUR. '         NO-GAP,  SY-VLINE NO-GAP,
 '   Open Amount    '       NO-GAP,  SY-VLINE NO-GAP,
            'Ty'            NO-GAP,  SY-VLINE NO-GAP,
            'Mat'           NO-GAP,  SY-VLINE NO-GAP,
            'Pay.'          NO-GAP,  SY-VLINE NO-GAP,
    '   Loading Port     '  NO-GAP,  SY-VLINE NO-GAP,
            'Inc'           NO-GAP,  SY-VLINE NO-GAP,
            'Vendor    '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            ' Loan Org. '   NO-GAP,  SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            'Mat. Req'    ,  SY-VLINE NO-GAP,
            'Imp.Req.No'    NO-GAP,  SY-VLINE NO-GAP,
            '     '         NO-GAP,  SY-VLINE NO-GAP,
 ' USD Conver Amount'       NO-GAP,  SY-VLINE NO-GAP,
            'TT'            NO-GAP,  SY-VLINE NO-GAP,
            'PGr'           NO-GAP,  SY-VLINE NO-GAP,
            'Plnt'          NO-GAP,  SY-VLINE NO-GAP,
    '   Arriving Port    '  NO-GAP,  SY-VLINE NO-GAP,
            'VIA'           NO-GAP,  SY-VLINE NO-GAP,
            'Bene.     '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            'Reference  '   NO-GAP,  SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE_EN
