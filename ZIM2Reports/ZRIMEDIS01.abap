*&---------------------------------------------------------------------*
*& Report  ZRIMEDIS01                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : EDI SEND(L/C EDI SEND)                                *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.07.09                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMEDIS01  MESSAGE-ID ZIM
                     LINE-SIZE 132
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,              " MARK
       UPDATE_CHK TYPE C,              " DB 반영 여부...
       ZFAPPDT    LIKE ZTREQST-ZFAPPDT," 요개설일?
       ZFREQDT    LIKE ZTREQST-ZFREQDT," 요개설일?
       ZFMAUD     LIKE ZTREQHD-ZFMAUD, " 자재납기?
       EBELN      LIKE ZTREQHD-EBELN,  " Purchasing document
       ZFREQNO    LIKE ZTREQHD-ZFREQNO," 수입의뢰 번?
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO," Amend Seq.
       ZFOPAMT1(18) TYPE C,            " 개설금액 TEXT
       WAERS      LIKE ZTREQST-WAERS,  " Currency
       ZFUSDAM1(18) TYPE C,            " USD 환산금액 TEXT
       ZFUSD      LIKE ZTREQST-ZFUSD,  " USD Currency
       ZFREQTY    LIKE ZTREQST-ZFREQTY," 결제구?
       ZFMATGB    LIKE ZTREQHD-ZFMATGB," 자재구?
       ZFBACD     LIKE ZTREQHD-ZFBACD, " 사전/사후 구?
       EKORG      LIKE ZTREQST-EKORG,  " Purchasing organizati
       EKGRP      LIKE ZTREQST-EKGRP,  " Purchasing group
       ZTERM      LIKE ZTREQHD-ZTERM,  " Terms of Payment
       ZFWERKS    LIKE ZTREQHD-ZFWERKS," 대표 Plant
       ERNAM      LIKE ZTREQST-ERNAM,  " 구매담자.
       LIFNR      LIKE ZTREQHD-LIFNR,  " Vendor Code
       NAME1(17),                      " Name 1
       ZFBENI     LIKE ZTREQHD-ZFBENI, " Beneficairy
       NAME2(17),                      " Name 1
       ZFOPBN     LIKE ZTREQHD-ZFBENI, " Open Bank
       NAME3(11),                      " Name 1
       ZFRLST2    LIKE ZTREQST-ZFRLST2," 개설 Release 상?
       ZFRLDT2    LIKE ZTREQST-ZFRLDT2," 개설 Release 일?
       ZFRLNM2    LIKE ZTREQST-ZFRLNM2," 개설 Release 담당?
       ZFCLOSE    LIKE ZTREQHD-ZFCLOSE," 수입의뢰 종료여?
       ZFRLST1    LIKE ZTREQST-ZFRLST1," 구매 Release 상?
       ZFSPRT(18) TYPE C,              " 선적?
       ZFAPRT(18) TYPE C,              " 도착?
       INCO1      LIKE ZTREQHD-INCO1,  " Incoterms
       ZFTRANS    LIKE ZTREQHD-ZFTRANS," VIA
       ZFLEVN     LIKE ZTREQHD-ZFLEVN, " 차입기?
       NAME4(11),                      " Name 1
       ZFREF1(11),                     " remark
       ZFOPAMT    LIKE ZTREQST-ZFOPAMT," 개설금?
       ZFUSDAM    LIKE ZTREQST-ZFUSDAM," USD 환산금?
       ZFDOCST    LIKE ZTREQST-ZFDOCST,
       ZFEDIST    LIKE ZTREQST-ZFEDIST,
       ZFEDICK    LIKE ZTREQST-ZFEDICK.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " 구매 Released  Report Data Define용 Include

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

INCLUDE   ZRIMUTIL01.                  " Utility function 모?

INCLUDE   ZRIMBDCCOM.                  " 수입의뢰 BDC 공통 Include

*------ EDI
DATA : W_OK_CODE    LIKE   SY-UCOMM,
       W_ZFDHENO         LIKE   ZTDHF1-ZFDHENO,
       W_ZFCDDOC         LIKE   ZTCDF1-ZFCDDOC,
       W_ZFDHSRO         LIKE   ZTDHF1-ZFDHSRO,
       W_ZFDHREF         LIKE   ZTDHF1-ZFDHREF.

DATA  W_ERR_MSG(100)   TYPE C.

DATA : W_EDI_RECORD(65535),
       W_FILENAME   LIKE  ZTDHF1-FILENAME,
       MI_HANDLE    TYPE  I.
DATA: BEGIN OF IT_EDIFILE OCCURS 0,
      W_RECORD   LIKE     W_EDI_RECORD,
      END OF IT_EDIFILE.
DATA: BEGIN OF MTAB_DATA OCCURS 0,
      LINE(132)   TYPE C,
END OF MTAB_DATA.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.               " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                            NO INTERVALS,
                S_APPDT   FOR ZTREQST-ZFAPPDT,  " 개설신청일.
                S_OPBN    FOR ZTREQHD-ZFOPBN,   " 개설은행.
                S_MATGB   FOR ZTREQHD-ZFMATGB,  " 자재구분.
                S_REQTY   FOR ZTREQHD-ZFREQTY   " 수입의뢰 Type
                              NO INTERVALS,
                S_WERKS   FOR ZTREQHD-ZFWERKS,  " 대표 plant
                S_EKORG   FOR ZTREQST-EKORG.    " Purch. Org.
PARAMETERS :    P_NAME    LIKE USR02-BNAME.     " 담당자.
SELECT-OPTIONS: S_EBELN   FOR ZTREQHD-EBELN,    " P/O Number
                S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                S_ZFBENI  FOR ZTREQHD-ZFBENI,   " Beneficiary
                S_EKGRP   FOR ZTREQST-EKGRP,    " Purch. Grp.
                S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 관리번호.
                S_AMDNO   FOR ZTREQST-ZFAMDNO.  " 변경번호.
PARAMETERS :    P_UNAME  LIKE SY-UNAME          " 사용자 ID.
                              OBLIGATORY.
*MODIFIED BY JSY----------------------------------------------------
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.               " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
PARAMETERS : P_NOOPEN   AS CHECKBOX.
PARAMETERS : P_OPEN     AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-003.
PARAMETERS : P_OK       AS CHECKBOX.
PARAMETERS : P_NOTOK    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B3.
*
*-----------------------------------------------------------------------
* L/C 릴리즈 상태 SELECT 조건 PARAMETER
*-----------------------------------------------------------------------
SELECT-OPTIONS : S_STATUS FOR ZTREQST-ZFRLST1 NO INTERVALS NO-DISPLAY.
SELECT-OPTIONS : S_STATU2 FOR ZTREQST-ZFRLST2 NO INTERVALS NO-DISPLAY.
SELECT-OPTIONS : S_EDIST  FOR ZTREQST-ZFDOCST NO INTERVALS NO-DISPLAY.
SELECT-OPTIONS : S_EDICK  FOR ZTREQST-ZFEDICK NO INTERVALS NO-DISPLAY.

* PARAMETER 초기값 Setting
INITIALIZATION.                        " 초기값 SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

* screen Selection
AT SELECTION-SCREEN ON S_BUKRS.
  PERFORM P2000_COMPANY_CODE_CHECK USING S_BUKRS.

* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.         " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* Import System Config Check
  PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.

* 파라메타 설정.
  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 구매의뢰 테이블 SELECT
  PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  W_OK_CODE = SY-UCOMM.
  CASE SY-UCOMM.
* SORT 선택?
    WHEN 'STUP' OR 'STDN'.             " SORT 선택시.
      W_FIELD_NM = 'ZFOPBN'.
      ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
      PERFORM HANDLE_SORT TABLES  IT_TAB
                          USING   SY-UCOMM.
* 전체 선택 및 선택해제.
    WHEN 'MKAL' OR 'MKLO'.             " 전체 선택 및 선택해제.
      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
    WHEN 'DISP'.                       " L/C 조?
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES EQ 1.
        READ TABLE IT_SELECTED INDEX 1.
        PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO
                                    IT_SELECTED-ZFAMDNO.
      ELSEIF W_SELECTED_LINES GT 1.
        MESSAGE E965.
      ENDIF.
    WHEN 'FRGS' OR 'FRGR'.     " EDI FILE CREATE / EDI FILE 취소.
      PERFORM P2000_MULTI_SELECTION.
      IF W_SELECTED_LINES NE 0.
        PERFORM P2000_POPUP_MESSAGE.   " 메세지 박스.
        IF W_BUTTON_ANSWER EQ '1'.     " 확인일 경우.
          PERFORM P3000_DATA_UPDATE USING W_OK_CODE. " 데이타 반영.
          LEAVE TO SCREEN 0.
        ENDIF.
      ENDIF.
    WHEN 'DOWN'.                       " FILE DOWNLOAD....
      PERFORM P3000_DOWNLOAD_EDI_FILE.
*           PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'REFR'.
* 구매의뢰 테이블 SELECT
      PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
* 레포트 관련 Text Table SELECT
      PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM RESET_LIST.
    WHEN OTHERS.
  ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIM06'.               " TITLE BAR
  P_NOOPEN = 'X'.                      " CREATE 대상.
  CLEAR : P_OPEN.                      " CREATE CANCLE 대상.

  P_OK = 'X'.                          " EDI CHECK BIT : OK
  CLEAR : P_NOTOK.                     " EDI CHECK BIT : NOT OK

  MOVE 'KHNP'   TO    P_BUKRS.

  GET PARAMETER ID 'BUK'  FIELD  P_BUKRS.

  REFRESH : S_REQTY.

  IF NOT P_BUKRS IS INITIAL.
    SELECT SINGLE * FROM ZTIMIMGTX
           WHERE   BUKRS  EQ  P_BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE S949 WITH P_BUKRS.
    ELSE.
      IF ZTIMIMGTX-ZFEDIYN NE 'X'.
        MESSAGE S990 WITH P_BUKRS.
      ELSE.
        IF ZTIMIMGTX-APP700 EQ 'X' OR ZTIMIMGTX-APP707 EQ 'X'.
          MOVE : 'I'      TO        S_REQTY-SIGN,
                 'EQ'     TO        S_REQTY-OPTION,
                 'LC'     TO        S_REQTY-LOW,
                 SPACE    TO        S_REQTY-HIGH.
          APPEND S_REQTY.
        ENDIF.
        IF ZTIMIMGTX-LOCAPP EQ 'X' OR ZTIMIMGTX-LOCAMR EQ 'X'.
          MOVE : 'I'      TO        S_REQTY-SIGN,
                 'EQ'     TO        S_REQTY-OPTION,
                 'LO'     TO        S_REQTY-LOW,
                 SPACE    TO        S_REQTY-HIGH.
          APPEND S_REQTY.
        ENDIF.
*        IF ZTIMIMGTX-PAYORD EQ 'X'.
*          MOVE : 'I'      TO        S_REQTY-SIGN,
*                 'EQ'     TO        S_REQTY-OPTION,
*                 'TT'     TO        S_REQTY-LOW,
*                 SPACE    TO        S_REQTY-HIGH.
*          APPEND S_REQTY.
*        ENDIF.
        IF ZTIMIMGTX-APPPUR EQ 'X'.
          MOVE : 'I'      TO        S_REQTY-SIGN,
                 'EQ'     TO        S_REQTY-OPTION,
                 'PU'     TO        S_REQTY-LOW,
                 SPACE    TO        S_REQTY-HIGH.
          APPEND S_REQTY.
        ENDIF.

      ENDIF.
    ENDIF.
  ELSE.
    MOVE : 'I'      TO        S_REQTY-SIGN,
           'EQ'     TO        S_REQTY-OPTION,
           'LC'     TO        S_REQTY-LOW,
           SPACE    TO        S_REQTY-HIGH.
    APPEND S_REQTY.
    MOVE : 'I'      TO        S_REQTY-SIGN,
           'EQ'     TO        S_REQTY-OPTION,
           'LO'     TO        S_REQTY-LOW,
           SPACE    TO        S_REQTY-HIGH.
    APPEND S_REQTY.
    MOVE : 'I'      TO        S_REQTY-SIGN,
           'EQ'     TO        S_REQTY-OPTION,
           'PU'     TO        S_REQTY-LOW,
           SPACE    TO        S_REQTY-HIGH.
    APPEND S_REQTY.
*    MOVE : 'I'      TO        S_REQTY-SIGN,
*           'EQ'     TO        S_REQTY-OPTION,
*           'TT'     TO        S_REQTY-LOW,
*           SPACE    TO        S_REQTY-HIGH.
*    APPEND S_REQTY.
  ENDIF.

  MOVE  SY-UNAME    TO  P_UNAME.

ENDFORM.                               " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

  SKIP 2.

  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[ L/C EDI 송신 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 101 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '개설예정'    ,  SY-VLINE NO-GAP,
            ' 구매문서 '    NO-GAP,  SY-VLINE NO-GAP,
            'CUR. '         NO-GAP,  SY-VLINE NO-GAP,
 '    개설 금액     '       NO-GAP,  SY-VLINE NO-GAP,
            'Ty'            NO-GAP,  SY-VLINE NO-GAP,
            'Mat'           NO-GAP,  SY-VLINE NO-GAP,
            'Pay.'          NO-GAP,  SY-VLINE NO-GAP,
    '     선  적  지     '  NO-GAP,  SY-VLINE NO-GAP,
            'Inc'           NO-GAP,  SY-VLINE NO-GAP,
            'Vendor    '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            'D'             NO-GAP,  SY-VLINE NO-GAP,
            ' 차입기관  '   NO-GAP,  SY-VLINE NO-GAP.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '자재납기'    ,  SY-VLINE NO-GAP,
            '수입의뢰No'    NO-GAP,  SY-VLINE NO-GAP,
            'Amend'         NO-GAP,  SY-VLINE NO-GAP,
 '   USD 환산금액   '       NO-GAP,  SY-VLINE NO-GAP,
            'TT'            NO-GAP,  SY-VLINE NO-GAP,
            'PGr'           NO-GAP,  SY-VLINE NO-GAP,
            'Plnt'          NO-GAP,  SY-VLINE NO-GAP,
    '     도  착  지     '  NO-GAP,  SY-VLINE NO-GAP,
            'VIA'           NO-GAP,  SY-VLINE NO-GAP,
            'Bene.     '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            'E'             NO-GAP,  SY-VLINE NO-GAP,
            ' 개설은행  '   NO-GAP,  SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                               " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.

  IF P_NOOPEN IS INITIAL AND P_OPEN IS INITIAL.
    W_ERR_CHK = 'Y'.   MESSAGE S351.   EXIT.
  ENDIF.

  IF P_OK IS INITIAL AND P_NOTOK IS INITIAL.
    W_ERR_CHK = 'Y'.   MESSAGE S352.   EXIT.
  ENDIF.

  IF P_NAME IS INITIAL.       P_NAME  =  '%'.      ENDIF.
*-----------------------------------------------------------------------
* 구매 릴리즈 사용 여부.
*-----------------------------------------------------------------------
  IF  ZTIMIMG00-ZFRELYN1 EQ 'X'.
    MOVE: 'I'      TO S_STATUS-SIGN,
          'EQ'     TO S_STATUS-OPTION,
          'R'      TO S_STATUS-LOW.
    APPEND S_STATUS.
  ELSE.
    MOVE: 'I'      TO S_STATUS-SIGN,
          'EQ'     TO S_STATUS-OPTION,
          'N'      TO S_STATUS-LOW.
    APPEND S_STATUS.
  ENDIF.
*-----------------------------------------------------------------------
* 개설 릴리즈 사용 여부.
*-----------------------------------------------------------------------
  IF  ZTIMIMG00-ZFRELYN2 EQ 'X'.
    MOVE: 'I'      TO S_STATU2-SIGN,
          'EQ'     TO S_STATU2-OPTION,
          'R'      TO S_STATU2-LOW.
    APPEND S_STATU2.
  ELSE.
    MOVE: 'I'      TO S_STATU2-SIGN,
          'EQ'     TO S_STATU2-OPTION,
          'N'      TO S_STATU2-LOW.
    APPEND S_STATU2.
  ENDIF.
*-----------------------------------------------------------------------
* EDI CREATE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_NOOPEN EQ 'X'.
    MOVE: 'I'      TO S_EDIST-SIGN,
          'EQ'     TO S_EDIST-OPTION,
          'N'      TO S_EDIST-LOW.
    APPEND S_EDIST.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CALCLE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_OPEN EQ 'X'.
    MOVE: 'I'      TO S_EDIST-SIGN,
          'EQ'     TO S_EDIST-OPTION,
          'S'      TO S_EDIST-LOW.
    APPEND S_EDIST.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CHECK BIT  SETTING
*-----------------------------------------------------------------------
  IF P_OK EQ 'X'.
    MOVE: 'I'      TO S_EDICK-SIGN,
          'EQ'     TO S_EDICK-OPTION,
          'O'      TO S_EDICK-LOW.
    APPEND S_EDICK.
  ENDIF.

*-----------------------------------------------------------------------
* EDI CALCLE 대상 SETTING
*-----------------------------------------------------------------------
  IF P_NOTOK EQ 'X'.
    MOVE: 'I'      TO S_EDICK-SIGN,
          'EQ'     TO S_EDICK-OPTION,
          'X'      TO S_EDICK-LOW.
    APPEND S_EDICK.
  ENDIF.

ENDFORM.                               " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
FORM P1000_GET_ZVREQHD_ST   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                     " Error Bit Setting

  SELECT * INTO TABLE IT_ZVREQ FROM ZVREQHD_ST
                               WHERE BUKRS      EQ     P_BUKRS
                               AND   ZFREQNO    IN     S_REQNO
                               AND   ZFAMDNO    IN     S_AMDNO
                               AND   ZFAPPDT    IN     S_APPDT
                               AND   ZFOPBN     IN     S_OPBN
                               AND   ZFMATGB    IN     S_MATGB
                               AND   ZFREQTY    IN     S_REQTY
                               AND   ZFWERKS    IN     S_WERKS
                               AND   EKORG      IN     S_EKORG
                               AND   ERNAM      LIKE   P_NAME
                               AND   ZFRLST1    IN     S_STATUS
                               AND   ZFRLST2    IN     S_STATU2
                               AND   EBELN      IN     S_EBELN
                               AND   LIFNR      IN     S_LIFNR
                               AND   ZFBENI     IN     S_ZFBENI
                               AND   EKGRP      IN     S_EKGRP
                               AND   ZFRVDT     GT     '00000000'
                               AND   ZFDOCST    IN    ('N', 'R')
                               AND   ZFEDIST    IN     S_EDIST
                               AND   ZFEDICK    IN     S_EDICK
                               AND   ZFCLOSE    EQ     SPACE.
*                              AND   LOEKZ      EQ     SPACE.

  IF SY-SUBRC NE 0.                    " Not Found?
    W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.

ENDFORM.                               " P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK           USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S961.   LEAVE TO SCREEN 0.
  ENDIF.

  SET PARAMETER ID 'BUK'  FIELD  P_BUKRS.

ENDFORM.                               " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.
  REFRESH : IT_TAB.

  LOOP AT IT_ZVREQ.

    W_TABIX = SY-TABIX.

    MOVE-CORRESPONDING IT_ZVREQ  TO  IT_TAB.
    MOVE : IT_ZVREQ-ZFLEVN       TO  IT_TAB-ZFLEVN,
           IT_ZVREQ-ZFOPBN       TO  IT_TAB-ZFOPBN.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CLEAR : LFA1.
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
    CLEAR : LFA1.
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
    CLEAR : LFA1.
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
    CLEAR : LFA1.
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
ENDFORM.                               " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'ZIM06'.               " GUI STATUS SETTING
  SET  TITLEBAR 'ZIM06'.               " GUI TITLE SETTING..

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

  LOOP AT IT_TAB.
*      W_LINE = W_LINE + 1.
*      PERFORM P2000_PAGE_CHECK.
    PERFORM P3000_LINE_WRITE.

    AT LAST.
      PERFORM P3000_LAST_WRITE.
    ENDAT.

  ENDLOOP.

ENDFORM.                               " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  RESET_LIST
*&---------------------------------------------------------------------*
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.         " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.

ENDFORM.                               " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
FORM P2000_MULTI_SELECTION.

  DATA: INDEX   TYPE P,
        ZFREQNO LIKE ZTREQST-ZFREQNO,
        ZFAMDNO LIKE ZTREQST-ZFAMDNO,
        ZFRLST1 LIKE ZTREQST-ZFRLST1,
        ZFRLST2 LIKE ZTREQST-ZFRLST2,
        ZFDOCST LIKE ZTREQST-ZFDOCST,
        ZFEDIST LIKE ZTREQST-ZFEDIST,
        ZFEDICK LIKE ZTREQST-ZFEDICK,
        ZFREQTY LIKE ZTREQHD-ZFREQTY.

  REFRESH IT_SELECTED.
  CLEAR W_SELECTED_LINES.

  MOVE : W_LIST_INDEX    TO INDEX,
         IT_TAB-ZFREQNO  TO ZFREQNO,
         IT_TAB-ZFAMDNO  TO ZFAMDNO,
         IT_TAB-ZFRLST1  TO ZFRLST1,
         IT_TAB-ZFRLST2  TO ZFRLST2,
         IT_TAB-ZFDOCST  TO ZFDOCST,
         IT_TAB-ZFEDIST  TO ZFEDIST,
         IT_TAB-ZFEDICK  TO ZFEDICK,
         IT_TAB-ZFREQTY  TO ZFREQTY.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF. " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
      MOVE : IT_TAB-ZFREQNO  TO IT_SELECTED-ZFREQNO,
             IT_TAB-ZFAMDNO  TO IT_SELECTED-ZFAMDNO,
             IT_TAB-ZFRLST1  TO IT_SELECTED-ZFRLST1,
             IT_TAB-ZFRLST2  TO IT_SELECTED-ZFRLST2,
             IT_TAB-ZFDOCST  TO IT_SELECTED-ZFDOCST,
             IT_TAB-ZFEDIST  TO IT_SELECTED-ZFEDIST,
             IT_TAB-ZFREQTY  TO IT_SELECTED-ZFREQTY,
             IT_TAB-ZFEDICK  TO IT_SELECTED-ZFEDICK.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ENDIF.
  ENDDO.

  IF W_SELECTED_LINES EQ 0.
    IF INDEX GT 0.
      MOVE : ZFREQNO TO IT_SELECTED-ZFREQNO,
             ZFAMDNO TO IT_SELECTED-ZFAMDNO,
             ZFRLST1 TO IT_SELECTED-ZFRLST1,
             ZFRLST2 TO IT_SELECTED-ZFRLST2,
             ZFDOCST TO IT_SELECTED-ZFDOCST,
             ZFEDIST TO IT_SELECTED-ZFEDIST,
             ZFREQTY TO IT_SELECTED-ZFREQTY,
             ZFEDICK TO IT_SELECTED-ZFEDICK.

      APPEND IT_SELECTED.
      ADD 1 TO W_SELECTED_LINES.
    ELSE.
      MESSAGE S962.
    ENDIF.
  ENDIF.

ENDFORM.                               " P2000_MULTI_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_PAGE_CHECK.

  IF W_LINE >= 53.
    WRITE : / SY-ULINE.
    W_PAGE = W_PAGE + 1.    W_LINE = 0.
    NEW-PAGE.
  ENDIF.

ENDFORM.                               " P2000_PAGE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  IF W_COUNT GT 0.
    FORMAT RESET.
    WRITE : / '총', W_COUNT, '건'.
  ENDIF.


ENDFORM.                               " P3000_LAST_WRITE
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
       IT_TAB-ZFAPPDT NO-GAP,          " 개설신청일.
       SY-VLINE NO-GAP,
       IT_TAB-EBELN   NO-GAP,          " 구매문서.
       SY-VLINE NO-GAP,
       IT_TAB-WAERS NO-GAP,            " currency
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQTY NO-GAP,          " 결제 구분.
       SY-VLINE,
       IT_TAB-ZFMATGB,                 " 자재 구분.
       SY-VLINE NO-GAP,
       IT_TAB-ZTERM NO-GAP,            " Payment Terms
       SY-VLINE NO-GAP,
       IT_TAB-ZFSPRT  NO-GAP,          " 선적항.
    85 SY-VLINE NO-GAP,
       IT_TAB-INCO1   NO-GAP,          " Incoterms
       SY-VLINE NO-GAP,
       IT_TAB-LIFNR   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-NAME1   NO-GAP,
   118 SY-VLINE NO-GAP.

  CASE IT_TAB-ZFDOCST.
    WHEN 'N'.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    WHEN 'R'.
      FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
    WHEN OTHERS.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDCASE.
  WRITE : IT_TAB-ZFDOCST NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  WRITE : IT_TAB-NAME4 NO-GAP, SY-VLINE.         " 차입기관?

* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',
       SY-VLINE NO-GAP,
*       IT_TAB-NAME3 ,                       " 개설 은행.
       IT_TAB-ZFMAUD,                  " 자재납기일.
    16 SY-VLINE NO-GAP,
       IT_TAB-ZFREQNO NO-GAP,          " 수입의?
       SY-VLINE NO-GAP,
       IT_TAB-ZFAMDNO NO-GAP,          " 통화 단위.
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSDAM  CURRENCY IT_TAB-ZFUSD NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBACD,                  " 사전 / 사후 구분.
       SY-VLINE NO-GAP,
       IT_TAB-EKGRP NO-GAP,            " Purchasing Group
       SY-VLINE NO-GAP,
       IT_TAB-ZFWERKS NO-GAP,          " 대표 plant
       SY-VLINE NO-GAP,
       IT_TAB-ZFAPRT  NO-GAP,          " 도착항.
    85 SY-VLINE,
       IT_TAB-ZFTRANS,                 " VIA
       SY-VLINE NO-GAP,
       IT_TAB-ZFBENI  NO-GAP,          " Beneficiary
       SY-VLINE NO-GAP,
       IT_TAB-NAME2   NO-GAP,
   118 SY-VLINE NO-GAP.

  CASE IT_TAB-ZFEDICK.
    WHEN 'O'.
      FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
    WHEN 'X'.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    WHEN OTHERS.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDCASE.
  WRITE : IT_TAB-ZFEDICK NO-GAP, SY-VLINE NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*  WRITE : IT_TAB-ZFREF1 NO-GAP, SY-VLINE.         " 연락사항.
  WRITE : IT_TAB-NAME3  NO-GAP, SY-VLINE.         " 개설 은행.
* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                               " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_UNRELEASE_CHECK
*&---------------------------------------------------------------------*
FORM P2000_UNRELEASE_CHECK USING    P_ZFREQNO.
* Amend 존재여부 체?

* Invoice 체?

ENDFORM.                               " P2000_UNRELEASE_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.
  DATA : TEXT100(100) TYPE  C.
  IF W_OK_CODE EQ 'FRGS'.
    TEXT100 = 'EDI FILE CREATE 작업을 계속 진행하시겠습니까?'.
  ELSEIF W_OK_CODE EQ 'FRGR'.
    TEXT100 = 'EDI FILE CANCLE 작업을 계속 진행하시겠습니까?'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR              = 'EDI FILE CREATE/CANCLE 확인'
            DIAGNOSE_OBJECT       = ''
            TEXT_QUESTION         = TEXT100
            TEXT_BUTTON_1         = '확    인'
            TEXT_BUTTON_2         = '아 니 오'
            DEFAULT_BUTTON        = '1'
            DISPLAY_CANCEL_BUTTON = 'X'
            START_COLUMN          = 30
            START_ROW             = 8
       IMPORTING
            ANSWER                = W_BUTTON_ANSWER.

ENDFORM.                               " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE   USING   W_GUBUN.

  DATA : L_REQTY   LIKE   ZTREQHD-ZFREQTY,
         L_RETURN  LIKE   SY-SUBRC,
         O_ZTREQST LIKE   ZTREQST,
         L_COUNT   TYPE   I.

  REFRESH : IT_EDIFILE.
  CLEAR : L_REQTY, IT_EDIFILE, L_COUNT.
  SORT IT_SELECTED BY ZFREQTY ZFREQNO ZFAMDNO.

  LOOP AT IT_SELECTED.
    W_TABIX = SY-TABIX.
*>>> 진행상태바..
    LINE = ( SY-TABIX / W_SELECTED_LINES ) * 100.
    OUT_TEXT = 'JOB PROGRESS %99999%%'.
    REPLACE '%99999%' WITH LINE INTO OUT_TEXT.
    PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE.
*>> EDI BIT CHECK
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE
      IF IT_SELECTED-ZFEDICK EQ 'X'.
        MESSAGE I119 WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO.
        CONTINUE.
      ENDIF.
      IF IT_SELECTED-ZFDOCST NE 'N'.
        MESSAGE I104 WITH IT_SELECTED-ZFREQNO
                          IT_SELECTED-ZFAMDNO IT_SELECTED-ZFDOCST.
        CONTINUE.
      ENDIF.
    ELSEIF W_GUBUN EQ 'FRGR'.          " EDI CANCLE
      IF IT_SELECTED-ZFDOCST NE 'R'.
        MESSAGE I104 WITH IT_SELECTED-ZFREQNO
                          IT_SELECTED-ZFAMDNO IT_SELECTED-ZFDOCST.
        CONTINUE.
      ENDIF.
    ENDIF.

*>>> 수입의뢰 헤더, 상태 테이블 조회...
    SELECT SINGLE * FROM ZTREQHD
                    WHERE ZFREQNO EQ IT_SELECTED-ZFREQNO.

    SELECT SINGLE * FROM ZTREQST
                    WHERE ZFREQNO EQ IT_SELECTED-ZFREQNO
                    AND   ZFAMDNO EQ IT_SELECTED-ZFAMDNO.
*>> 변경이력..
    O_ZTREQST = ZTREQST.
*>>  개설은행 조회.
    SELECT SINGLE * FROM LFA1
                    WHERE LIFNR   EQ ZTREQHD-ZFOPBN.
*>>>  EDI 식별자 조회.
    IF LFA1-BAHNS IS INITIAL.
       MESSAGE I274 WITH ZTREQHD-ZFOPBN.
       CONTINUE.
    ENDIF.

* LOCK CHECK
    PERFORM   P2000_LOCK_MODE_SET  USING    'L'
                                            IT_SELECTED-ZFREQNO
                                            IT_SELECTED-ZFAMDNO
                                            L_RETURN.
    CHECK L_RETURN EQ 0.

*>>> EDI용 FIELD CREATE.
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE

      PERFORM   P3000_FILE_CREATE.
*>>> READY KOREA LTD. SAM-FILE WRITE FUNCTION
      CALL FUNCTION 'ZIM_EDI_SAMFILE_WRITE'
           EXPORTING
                ZFCDDOC     = W_ZFCDDOC
                BUKRS       = ZTREQHD-BUKRS
           IMPORTING
                W_FILENAME  =  W_FILENAME
           TABLES
                EDIFILE = IT_EDIFILE.
      REFRESH : IT_EDIFILE.

      PERFORM  P3000_FILE_TRANSFER.
      IF W_ERR_CHK EQ 'Y'.  CONTINUE.  ENDIF.

    ELSE.
      CALL FUNCTION 'ZIM_EDI_SAMFILE_DELETE'
           EXPORTING
                ZFDHENO = ZTREQST-ZFDOCNO.
    ENDIF.

    ADD 1   TO    L_COUNT.             "---> 마지막을 알?

* 상태 변?
*>>>>> 문서취소일 경우, 이전 EDI문서관리번호를 가지고 있기 위해.....
    IF W_GUBUN EQ 'FRGR'.
      MOVE ZTREQST-ZFDOCNO  TO  W_ZFDHENO.
    ENDIF.

    MOVE : SY-UNAME    TO    ZTREQST-UNAM,
           SY-DATUM    TO    ZTREQST-UDAT,
           W_ZFDHENO   TO    ZTREQST-ZFDOCNO.
    IF W_GUBUN EQ 'FRGS'.              " EDI CREATE
      MOVE : 'R'     TO    ZTREQST-ZFDOCST,
             'S'     TO    ZTREQST-ZFEDIST.
    ELSEIF W_GUBUN EQ 'FRGR'.          " EDI CANCLE
      MOVE : 'N'     TO    ZTREQST-ZFDOCST,
             'N'     TO    ZTREQST-ZFEDIST.
    ENDIF.
*>>> 변경...
    UPDATE ZTREQST.

* CHANGE DOCUMENT
    CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
         EXPORTING
              W_ZFREQNO = ZTREQST-ZFREQNO
              W_ZFAMDNO = ZTREQST-ZFAMDNO
              N_ZTREQST = ZTREQST
              O_ZTREQST = O_ZTREQST.

*>>> UNLOCK SETTTING.
    PERFORM   P2000_LOCK_MODE_SET  USING    'U'
                                             IT_SELECTED-ZFREQNO
                                             IT_SELECTED-ZFAMDNO
                                             L_RETURN.

    L_REQTY = IT_SELECTED-ZFREQTY.
  ENDLOOP.

ENDFORM.                               " P3000_DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO P_ZFAMDNO.
  SET PARAMETER ID 'BES'       FIELD ''.
  SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
  SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
  SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.

  IF P_ZFAMDNO EQ '00000'.
    CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
  ELSE.
    CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                               " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCK_MODE_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_LOCK_MODE_SET USING    VALUE(P_MODE)
                                  VALUE(P_REQNO)
                                  VALUE(P_AMDNO)
                                  P_RETURN.
* LOCK CHECK
  IF P_MODE EQ 'L'.
    CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = P_REQNO
              ZFAMDNO = P_AMDNO
         EXCEPTIONS
              OTHERS  = 1.

    MOVE SY-SUBRC     TO     P_RETURN.
    IF SY-SUBRC NE 0.
      MESSAGE I510 WITH SY-MSGV1 'Import Document' P_REQNO P_AMDNO
                   RAISING DOCUMENT_LOCKED.
    ENDIF.
  ELSEIF P_MODE EQ 'U'.
    CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
         EXPORTING
              ZFREQNO = P_REQNO
              ZFAMDNO = P_AMDNO.
  ENDIF.
ENDFORM.                               " P2000_LOCK_MODE_SET

*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FILE_CREATE.
*>> DOCUMENT TYPE DETERMINE
  CASE IT_SELECTED-ZFREQTY.
    WHEN 'LC'.
      IF IT_SELECTED-ZFAMDNO IS INITIAL.
        W_ZFCDDOC = 'APP700'.
      ELSE.
        W_ZFCDDOC = 'APP707'.
      ENDIF.
    WHEN 'LO'.
      IF IT_SELECTED-ZFAMDNO IS INITIAL.
        W_ZFCDDOC = 'LOCAPP'.
      ELSE.
        W_ZFCDDOC = 'LOCAMR'.
      ENDIF.
    WHEN 'TT'.
      W_ZFCDDOC = 'PAYORD'.
    WHEN OTHERS.  EXIT.
  ENDCASE.

*>>> FIELD MOVE
  W_ZFDHSRO = LFA1-BAHNS.              " 식별자.
  W_ZFDHREF = ZTREQHD-EBELN.           " 참조번호.
  MOVE '-'             TO W_ZFDHREF+10(1).
  MOVE ZTREQST-ZFAMDNO TO W_ZFDHREF+11(5).
  W_ZFDHENO = ZTREQST-ZFDOCNO.         " 문서번호.

*>>> EDI 관리번호 SETTING
  CALL FUNCTION 'ZIM_EDI_NUMBER_GET_NEXT'
      EXPORTING
           W_ZFCDDOC = W_ZFCDDOC
           W_ZFDHSRO = W_ZFDHSRO
           W_ZFDHREF = W_ZFDHREF
           W_BUKRS   = ZTREQHD-BUKRS
      CHANGING
           W_ZFDHENO = W_ZFDHENO
      EXCEPTIONS
           DB_ERROR  = 4
           NO_TYPE   = 8.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

  CLEAR : W_EDI_RECORD.
*-----------------------------------------------------------------------
* ITEM DATA CREATE
*-----------------------------------------------------------------------
  CASE IT_SELECTED-ZFREQTY.
    WHEN 'LC'.
      IF ZTREQST-ZFAMDNO = '00000'.
        CALL FUNCTION 'ZIM_MAT_APP700_EDI_DOC'
             EXPORTING
                  W_ZFREQNO    = ZTREQHD-ZFREQNO
                  W_ZFDHENO    = W_ZFDHENO
                  W_BAHNS      = LFA1-BAHNS
             IMPORTING
                  W_EDI_RECORD = W_EDI_RECORD
             EXCEPTIONS
                  CREATE_ERROR = 4.
      ELSE.
        CALL FUNCTION 'ZIM_MAT_APP707_EDI_DOC'
             EXPORTING
                  W_ZFREQNO    = ZTREQHD-ZFREQNO
                  W_ZFAMDNO    = ZTREQST-ZFAMDNO
                  W_ZFDHENO    = W_ZFDHENO
                  W_BAHNS      = LFA1-BAHNS
             IMPORTING
                  W_EDI_RECORD = W_EDI_RECORD
             EXCEPTIONS
                  CREATE_ERROR = 4.
      ENDIF.
    WHEN 'LO'.
      IF ZTREQST-ZFAMDNO = '00000'.
        CALL FUNCTION 'ZIM_LG_LOCAPP_EDI_DOC'
             EXPORTING
                  W_ZFREQNO    = ZTREQHD-ZFREQNO
                  W_ZFDHENO    = W_ZFDHENO
                  W_BAHNS      = W_LFA1-BAHNS
             IMPORTING
                  W_EDI_RECORD = W_EDI_RECORD
             EXCEPTIONS
                  CREATE_ERROR = 4.
      ELSE.
        CALL FUNCTION 'ZIM_LG_LOCAMR_EDI_DOC'
             EXPORTING
                  W_ZFREQNO    = ZTREQHD-ZFREQNO
                  W_ZFAMDNO    = ZTREQST-ZFAMDNO
                  W_ZFDHENO    = W_ZFDHENO
                  W_BAHNS      = W_LFA1-BAHNS
             IMPORTING
                  W_EDI_RECORD = W_EDI_RECORD
             EXCEPTIONS
                  CREATE_ERROR = 4.
      ENDIF.
    WHEN 'PU'.

    WHEN 'TT'.
      CALL FUNCTION 'ZIM_LG_PAYORD_EDI_DOC'
           EXPORTING
                W_ZFREQNO    = ZTREQHD-ZFREQNO
                W_ZFDHENO    = W_ZFDHENO
                W_BAHNS      = W_LFA1-BAHNS
           IMPORTING
                W_EDI_RECORD = W_EDI_RECORD
           EXCEPTIONS
                CREATE_ERROR = 4.
    WHEN 'DA' OR 'DP'.
    WHEN OTHERS.
  ENDCASE.

  CASE SY-SUBRC.
    WHEN  4.    MESSAGE E118 WITH   W_ZFDHENO.
    WHEN  8.    MESSAGE E117 WITH   W_ZFCDDOC.
  ENDCASE.

*>>> INTERNAL TABLE WRITE....
  IT_EDIFILE-W_RECORD = W_EDI_RECORD.
  APPEND IT_EDIFILE.

ENDFORM.                               " P3000_FILE_CREATE
*&---------------------------------------------------------------------*
*&      Form  P2000_COMPANY_CODE_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_COMPANY_CODE_CHECK USING    P_BUKRS.

  IF NOT P_BUKRS IS INITIAL.
    SELECT SINGLE * FROM ZTIMIMGTX
           WHERE   BUKRS  EQ  P_BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E949 WITH P_BUKRS.
    ELSE.
      IF ZTIMIMGTX-ZFEDIYN NE 'X'.
        MESSAGE E990 WITH P_BUKRS.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " P2000_COMPANY_CODE_CHECK

*&---------------------------------------------------------------------*
*&      Form  P4000_CREATE_DATA
*&---------------------------------------------------------------------*
FORM P4000_CREATE_DATA.

  DATA:  W_SUBRC     LIKE    SY-SUBRC.
  W_ZFDHREF = IT_SELECTED-ZFREQNO.

  SET UPDATE TASK LOCAL.
  CALL FUNCTION 'ZIM_MAT_APP700_EDI_DOC'
       EXPORTING
            W_ZFREQNO    = IT_SELECTED-ZFREQNO
            W_ZFDHENO    = W_ZFDHENO
            W_BAHNS      = W_LFA1-BAHNS
       IMPORTING
*            W_ERR_MSG    = W_ERR_MSG
            W_EDI_RECORD = W_EDI_RECORD
*       TABLES
*            IT_TAB       = IT_TAB_DOWN
       EXCEPTIONS
            CREATE_ERROR = 4.
*            NOT_FILLED   = 5.
  MOVE SY-SUBRC TO W_SUBRC.

  IF W_SUBRC NE 0.                     ">> 오류 발생시...
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.

  CASE W_SUBRC.
    WHEN  4.    MESSAGE E020 WITH   ''." W_ZFDHENO.
    WHEN  5.    MESSAGE E977 WITH   W_ERR_MSG.
    WHEN  8.    MESSAGE E021 WITH   ''." W_ZFCDDOC.
  ENDCASE.

ENDFORM.                               " P4000_CREATE_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_DOWNLOAD_EDI_FILE
*&---------------------------------------------------------------------*
FORM P3000_DOWNLOAD_EDI_FILE.

  PERFORM P2000_MULTI_SELECTION.
  IF W_SELECTED_LINES EQ 0.
*    MESSAGE S766.
    EXIT.
  ELSE.
    LOOP AT IT_SELECTED.
      PERFORM P4000_CREATE_DATA.
      MOVE W_EDI_RECORD TO IT_TAB_DOWN.
      APPEND IT_TAB_DOWN.
      PERFORM P3000_TO_CLIENT_PC_DOWNLOAD.
    ENDLOOP.
  ENDIF.

ENDFORM.                               " P3000_DOWNLOAD_EDI_FILE

*&---------------------------------------------------------------------*
*&      Form  P3000_TO_CLIENT_PC_DOWNLOAD
*&---------------------------------------------------------------------*
FORM P3000_TO_CLIENT_PC_DOWNLOAD.
  DATA: WL_FILENAME LIKE RLGRAP-FILENAME.

  CONCATENATE 'C:\' 'APP700' SY-DATUM SY-UZEIT '.txt'
         INTO WL_FILENAME.
  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = WL_FILENAME
            FILETYPE = 'ASC'
       TABLES
            DATA_TAB = IT_TAB_DOWN.

ENDFORM.                               " P3000_TO_CLIENT_PC_DOWNLOAD
*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_CONNECT .

   PERFORM P2000_SHOW_BAR USING 'File Server에 로그인 중입니다...' 0.

DATA: mc_password(20) TYPE c ,
      mc_userid(20)   TYPE c ,
      mi_key          TYPE i VALUE 26101957,
      mi_pwd_len      TYPE i.

   DESCRIBE FIELD MC_PASSWORD LENGTH MI_PWD_LEN.
*
**-- FTP_CONNECT requires an encrypted password to work
   MC_PASSWORD = 'edi_int'.
   CALL 'AB_RFC_X_SCRAMBLE_STRING'
      ID 'SOURCE' FIELD MC_PASSWORD ID 'KEY'         FIELD MI_KEY
      ID 'SCR'    FIELD 'X'         ID 'DESTINATION' FIELD MC_PASSWORD
      ID 'DSTLEN' FIELD MI_PWD_LEN.

  CALL FUNCTION 'FTP_CONNECT'
       EXPORTING
           USER            = 'edi_int'
           PASSWORD        = MC_PASSWORD
           HOST            = '10.135.9.34'
           RFC_DESTINATION = 'SAPFTP'
      IMPORTING
           HANDLE          = MI_HANDLE
      EXCEPTIONS
           NOT_CONNECTED   = 1
           OTHERS          = 2.

   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S201(04) WITH ZTIMIMGTX-HOST.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S202(04) WITH ZTIMIMGTX-UNAME ZTIMIMGTX-HOST.
   ENDCASE.

ENDFORM.                    " P2000_FTP_CONNECT
*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_DISCONNECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_DISCONNECT .

*> FTP DISCONNET.
   CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
            HANDLE   =   MI_HANDLE
      EXCEPTIONS
            OTHERS   =   1.

ENDFORM.                    " P2000_FTP_DISCONNECT
*&---------------------------------------------------------------------*
*&      Form  P3000_FILE_TRANSFER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_FILE_TRANSFER .

*>> EDI SERVER로 FTP CONNECT.
   PERFORM  P2000_FTP_CONNECT.
   IF W_ERR_CHK  EQ  'Y'.  EXIT.  ENDIF.

*>> FILE TRANSFER.
   PERFORM  P2000_FTP_COMMAND.

*>> EDI SERVER FTP DISCONNECT.
   PERFORM  P2000_FTP_DISCONNECT.

ENDFORM.                    " P3000_FILE_TRANSFER

*&---------------------------------------------------------------------*
*&      Form  P2000_FTP_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_FTP_COMMAND .
DATA: L_STRLEN           TYPE I,
      L_START            TYPE I,
      L_END              TYPE I,
      L_LEN              TYPE I,
      L_POSITION1        TYPE I,
      L_POSITION2        TYPE I,
      L_FIRST_CHK        TYPE C VALUE 'N',
      L_COMMAND(90)      TYPE C.

   CONCATENATE 'cd' 'edi_send' INTO L_COMMAND SEPARATED BY SPACE.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.

   CLEAR : L_COMMAND.
   CONCATENATE ZTIMIMGTX-ZFPATH '/' W_FILENAME INTO L_COMMAND.
   CONCATENATE 'put' L_COMMAND W_FILENAME
                     INTO L_COMMAND SEPARATED BY SPACE.

*> INBOUND DIRECTORY로 이동.
   CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
           HANDLE        = MI_HANDLE
           COMMAND       = L_COMMAND
        TABLES
           DATA          = MTAB_DATA
        EXCEPTIONS
           TCPIP_ERROR   = 1
           COMMAND_ERROR = 2
           DATA_ERROR    = 3
           OTHERS        = 4.

* do some error checking.
   CASE SY-SUBRC.
      WHEN 0.
         DELETE DATASET  W_FILENAME.
      WHEN 1.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S101(04) WITH ZTIMIMGTX-HOST.   EXIT.
      WHEN 2.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S200(04).      EXIT.
      WHEN 3.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S207(04).      EXIT.
      WHEN 4.
         MOVE 'Y'        TO     W_ERR_CHK.
         MESSAGE S208(04).      EXIT.
   ENDCASE.


ENDFORM.                    " P2000_FTP_COMMAND
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
