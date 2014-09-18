*&---------------------------------------------------------------------*
*& Report  ZRIMREL03                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 구매 Release ( Approve ) 수입의뢰 Amend Documents     *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2000.04.17                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Config에서 구매 Released 여부를 Check하여야만 한다.
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMREL03    MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.


*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK       TYPE C,                        " MARK
       W_GB01(1)  TYPE C VALUE ';',
       UPDATE_CHK TYPE C,                        " DB 반영 여부...
       W_GB02(1)  TYPE C VALUE ';',
       ZFREQDT    LIKE ZTREQST-ZFREQDT,          " 요개설일?
       W_GB03(1)  TYPE C VALUE ';',
       ZFMAUD     LIKE ZTREQHD-ZFMAUD,           " 자재납기?
       W_GB04(1)  TYPE C VALUE ';',
       EBELN      LIKE ZTREQHD-EBELN,            " Purchasing document
       W_GB05(1)  TYPE C VALUE ';',
       ZFREQNO    LIKE ZTREQHD-ZFREQNO,          " 수입의뢰 번?
       W_GB06(1)  TYPE C VALUE ';',
       ZFAMDNO    LIKE ZTREQST-ZFAMDNO,          " Amend Seq.
       W_GB07(1)  TYPE C VALUE ';',
       ZFOPAMT1(18) TYPE C,                      " 개설금액 TEXT
       W_GB08(1)  TYPE C VALUE ';',
       WAERS      LIKE ZTREQST-WAERS,            " Currency
       W_GB09(1)  TYPE C VALUE ';',
       ZFUSDAM1(18) TYPE C,                      " USD 환산금액 TEXT
       W_GB10(1)  TYPE C VALUE ';',
       ZFUSD      LIKE ZTREQST-ZFUSD,            " USD Currency
       W_GB11(1)  TYPE C VALUE ';',
       ZFREQTY    LIKE ZTREQST-ZFREQTY,          " 결제구?
       W_GB12(1)  TYPE C VALUE ';',
       ZFBACD     LIKE ZTREQHD-ZFBACD,           " 사전/사후 구?
       W_GB13(1)  TYPE C VALUE ';',
       EKORG      LIKE ZTREQST-EKORG,            " Purchasing organizati
       W_GB14(1)  TYPE C VALUE ';',
       EKGRP      LIKE ZTREQST-EKGRP,            " Purchasing group
       W_GB15(1)  TYPE C VALUE ';',
       ZFWERKS    LIKE ZTREQHD-ZFWERKS,          " 대표 Plant
       W_GB16(1)  TYPE C VALUE ';',
       ERNAM      LIKE ZTREQST-ERNAM,            " 구매담?
       W_GB17(1)  TYPE C VALUE ';',
       LIFNR      LIKE ZTREQHD-LIFNR,            " Vendor Code
       W_GB18(1)  TYPE C VALUE ';',
*      NAME1(33)  TYPE C,                        " Name 1
       NAME1      LIKE LFA1-NAME1,               " Name 1
       W_GB19(1)  TYPE C VALUE ';',
       ZFBENI     LIKE ZTREQHD-ZFBENI,           " Beneficairy
       W_GB20(1)  TYPE C VALUE ';',
       NAME2      LIKE LFA1-NAME1,               " Name 1
       W_GB21(1)  TYPE C VALUE ';',
       ZFRLST1    LIKE ZTREQST-ZFRLST1,          " 의뢰 Release 상?
       W_GB22(1)  TYPE C VALUE ';',
       ZFRLDT1    LIKE ZTREQST-ZFRLDT1,          " 의뢰 Release 일?
       W_GB23(1)  TYPE C VALUE ';',
       ZFRLNM1    LIKE ZTREQST-ZFRLNM1,          " 의뢰 Release 담당?
       W_GB24(1)  TYPE C VALUE ';',
       ZFCLOSE    LIKE ZTREQHD-ZFCLOSE,          " 수입의뢰 종료여?
       W_GB25(1)  TYPE C VALUE ';',
       ZFRLST2    LIKE ZTREQST-ZFRLST2,          " 개설 Release 상?
       W_GB40(1)  TYPE C VALUE ';',
       ZFOPAMT    LIKE ZTREQST-ZFOPAMT,         " 개설금?
       W_GB41(1)  TYPE C VALUE ';',
       ZFUSDAM    LIKE ZTREQST-ZFUSDAM,          " USD 환산금?
       W_GB26(1)  TYPE C VALUE ';'.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMPRELTOP.    " 구매 Released  Report Data Define용 Include

INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include

INCLUDE   ZRIMUTIL01.     " Utility function 모?


*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_EBELN   FOR ZTREQHD-EBELN,    " Purchasing document
                   S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 관리번?
                   S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                   S_ZFBENI  FOR ZTREQHD-ZFBENI,   " Beneficiary
                   S_ZFMAUD  FOR ZTREQHD-ZFMAUD,   " 자재납?
                   S_REQDT   FOR ZTREQST-ZFREQDT,  " 요개설일?
                   S_CDAT    FOR ZTREQST-CDAT,     " Created on
                   S_WERKS   FOR ZTREQHD-ZFWERKS,  " Plant
                   S_MATNR   FOR ZTREQIT-MATNR.    " Mateial code
   PARAMETERS :    P_EKGRP   LIKE ZTREQST-EKGRP,
                   P_EKORG   LIKE ZTREQST-EKORG,
                   P_ERNAM   LIKE ZTREQST-ERNAM.
   SELECT-OPTIONS: S_REQTY   FOR ZTREQHD-ZFREQTY.  " 수입의뢰 Type
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   PARAMETERS : P_NOOPEN   AS CHECKBOX.
   PARAMETERS : P_OPEN     AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.
*
*-----------------------------------------------------------------------
* L/C 릴리즈 상태 SELECT 조건 PARAMETER
*-----------------------------------------------------------------------
SELECT-OPTIONS : S_STATUS FOR ZTREQST-ZFRLST1 NO INTERVALS NO-DISPLAY.
SELECT-OPTIONS : S_STATU2 FOR ZTREQST-ZFRLST2 NO INTERVALS NO-DISPLAY.

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
* Import System Config Check
   PERFORM   P2000_CONFIG_CHECK        USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 권한 검증 함?
   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 파라메타 설?
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
   CASE SY-UCOMM.
* SORT 선택?
      WHEN 'STUP' OR 'STDN'.         " SORT 선택?
         W_FIELD_NM = 'ZFREQDT'.
         ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
         PERFORM HANDLE_SORT TABLES  IT_TAB
                             USING   SY-UCOMM.
* 전체 선택 및 선택해?
      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
         PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'DISP' OR 'DIS1'.    " L/C 조회 및 AMEND
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES EQ 1.
            READ TABLE IT_SELECTED INDEX 1.
            PERFORM P2000_SHOW_LC USING IT_SELECTED-ZFREQNO
                                        IT_SELECTED-ZFAMDNO.
         ELSEIF W_SELECTED_LINES GT 1.
            MESSAGE E965.
         ENDIF.
      WHEN 'RESV'.          " 릴리즈 + 저?
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            PERFORM P3000_RELEASED_UPDATE USING 'R'.
            LOOP AT IT_TAB WHERE UPDATE_CHK EQ 'U'.
               EXIT.
            ENDLOOP.
            IF SY-SUBRC EQ 0.
                PERFORM P2000_POPUP_MESSAGE.     " 메세지 박?
                IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
                   PERFORM P3000_DATA_UPDATE.    " 데이타 반?
                   PERFORM P2000_DATA_UNLOCK.    " Unlocking
                   LEAVE TO SCREEN 0.
                ENDIF.
            ELSE.
                MESSAGE E032.
            ENDIF.
         ELSE.
            MESSAGE E032.
         ENDIF.
      WHEN 'FRGS'.          " 릴리?
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            PERFORM P3000_RELEASED_UPDATE USING 'R'.
            PERFORM RESET_LIST.
         ENDIF.
      WHEN 'FRGR'.          " 릴리즈 취?
         PERFORM P2000_MULTI_SELECTION.
         IF W_SELECTED_LINES NE 0.
            PERFORM P3000_RELEASED_UPDATE USING 'C'.
            PERFORM RESET_LIST.
         ENDIF.
      WHEN 'SAVE'.          " FILE DOWNLOAD....
         LOOP AT IT_TAB WHERE UPDATE_CHK EQ 'U'.
            EXIT.
         ENDLOOP.
         IF SY-SUBRC EQ 0.
             PERFORM P2000_POPUP_MESSAGE.     " 메세지 박?
             IF W_BUTTON_ANSWER EQ '1'.       " 확인일 경?
                PERFORM P3000_DATA_UPDATE.    " 데이타 반?
                PERFORM P2000_DATA_UNLOCK.    " Unlocking
                LEAVE TO SCREEN 0.
             ENDIF.
         ELSE.
             MESSAGE E032.
         ENDIF.
      WHEN 'DOWN'.          " FILE DOWNLOAD....
           PERFORM P3000_TO_PC_DOWNLOAD.
      WHEN 'REFR'.
          LOOP AT IT_TAB WHERE UPDATE_CHK EQ 'U'.
             EXIT.
          ENDLOOP.
          IF SY-SUBRC EQ 0.
             PERFORM P2000_REFRESH_POPUP_MESSAGE.
             IF W_BUTTON_ANSWER EQ '1'.       " 저장 후 빠져나감.
                PERFORM P3000_DATA_UPDATE.    " 데이타 반?
                PERFORM P2000_DATA_UNLOCK.    " Unlocking
* 구매의뢰 테이블 SELECT
                PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
                PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
                PERFORM RESET_LIST.
            ELSEIF W_BUTTON_ANSWER EQ '2'.    " 저장하지 않고 빠져나감.
                PERFORM P2000_DATA_UNLOCK.    " Unlocking
* 구매의뢰 테이블 SELECT
                PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
                PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
                IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
                PERFORM RESET_LIST.
            ENDIF.
         ELSE.
* 구매의뢰 테이블 SELECT
            PERFORM   P1000_GET_ZVREQHD_ST      USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
            PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
         ENDIF.
      WHEN 'BAC1' OR 'EXIT' OR 'CANC'.
         LOOP AT IT_TAB WHERE UPDATE_CHK EQ 'U'.  " 갱신할 데이타 존재?
            EXIT.
         ENDLOOP.

         IF SY-SUBRC EQ 0.                     " DATA 존재?
            PERFORM P2000_EXIT_POPUP_MESSAGE.  " 메세지 박?
            IF W_BUTTON_ANSWER EQ '1'.         " 저장 후 빠져나가시 선?
               PERFORM P3000_DATA_UPDATE.      " 데이타 반?
               LEAVE TO SCREEN 0.              " 종?
            ELSEIF W_BUTTON_ANSWER EQ '2'.     " 저장하지 않고 빠져나감.
              LEAVE TO SCREEN 0.
            ENDIF.
         ELSE.
            LEAVE TO SCREEN 0.                " 종?
         ENDIF.
      WHEN OTHERS.
   ENDCASE.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM14'.          " TITLE BAR
  P_NOOPEN = 'X'.                 " 릴리즈 대?
  CLEAR : P_OPEN.                 " 릴리즈 취소 대?
ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /53  '[ 수입의뢰 Amend 요청 대상 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM, 101 'Page : ', W_PAGE.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '요개설일'    ,  SY-VLINE NO-GAP,
            'P/O Number'    NO-GAP,  SY-VLINE NO-GAP,
            'CUR. '         NO-GAP,  SY-VLINE NO-GAP,
 '    개설 금액     '       NO-GAP,  SY-VLINE NO-GAP,
            'Ty'            NO-GAP,  SY-VLINE NO-GAP,
            'POrg'          NO-GAP,  SY-VLINE NO-GAP,
            ' 대표 Plant '  NO-GAP,  SY-VLINE NO-GAP,
            'Vendor    '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            'S'             NO-GAP,  SY-VLINE NO-GAP.
*            'Release Date'   NO-GAP,  SY-VLINE NO-GAP,
*            'Release 담당자' NO-GAP,  SY-VLINE.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE, ' ',  SY-VLINE,
            '자재납기'    ,  SY-VLINE NO-GAP,
            '수입의뢰No'    NO-GAP,  SY-VLINE NO-GAP,
            '     '         NO-GAP,  SY-VLINE NO-GAP,
 '   USD 환산금액   '       NO-GAP,  SY-VLINE NO-GAP,
            'TT'            NO-GAP,  SY-VLINE NO-GAP,
            'PGrp'          NO-GAP,  SY-VLINE NO-GAP,
            '  구매담당  '  NO-GAP,  SY-VLINE NO-GAP,
            'Bene.     '    NO-GAP,  SY-VLINE NO-GAP,
            'Name',              118 SY-VLINE NO-GAP,
            'C'             NO-GAP,  SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.

ENDFORM.                    " P3000_TITLE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
FORM P2000_AUTHORITY_CHECK          USING   W_ERR_CHK.

   W_ERR_CHK = 'N'.
*-----------------------------------------------------------------------
*  해당 화면 AUTHORITY CHECK
*-----------------------------------------------------------------------
*   AUTHORITY-CHECK OBJECT 'ZI_LC_REL'
*           ID 'ACTVT' FIELD '*'.
*
*   IF SY-SUBRC NE 0.
*      MESSAGE S960 WITH SY-UNAME '의뢰 Release 트랜잭션'.
*      W_ERR_CHK = 'Y'.   EXIT.
*   ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.

  IF P_NOOPEN IS INITIAL AND P_OPEN IS INITIAL.
     W_ERR_CHK = 'Y'.   MESSAGE S008.   EXIT.
  ENDIF.

  IF P_EKORG IS INITIAL.       P_EKORG = '%'.      ENDIF.
  IF P_EKGRP IS INITIAL.       P_EKGRP = '%'.      ENDIF.
  IF P_ERNAM IS INITIAL.       P_ERNAM = '%'.      ENDIF.
*-----------------------------------------------------------------------
* 릴리즈 대상 SETTING
*-----------------------------------------------------------------------
  IF P_NOOPEN EQ 'X'.
     MOVE: 'I'      TO S_STATUS-SIGN,
           'EQ'     TO S_STATUS-OPTION,
           'N'      TO S_STATUS-LOW.
     APPEND S_STATUS.

     MOVE: 'I'      TO S_STATUS-SIGN,
           'EQ'     TO S_STATUS-OPTION,
           'C'      TO S_STATUS-LOW.
     APPEND S_STATUS.
  ENDIF.

*-----------------------------------------------------------------------
* 릴리즈 취소 대상 SETTING
*-----------------------------------------------------------------------
  IF P_OPEN EQ 'X'.
     MOVE: 'I'      TO S_STATUS-SIGN,
           'EQ'     TO S_STATUS-OPTION,
           'R'      TO S_STATUS-LOW.
     APPEND S_STATUS.
  ENDIF.

* 개설 RELEASE BIT 사용여부 CHECK....
  IF ZTIMIMG00-ZFRELYN2 EQ 'X'.
     MOVE: 'I'      TO S_STATU2-SIGN,
           'EQ'     TO S_STATU2-OPTION,
           'N'      TO S_STATU2-LOW.
     APPEND S_STATU2.
     MOVE: 'I'      TO S_STATU2-SIGN,
           'EQ'     TO S_STATU2-OPTION,
           'C'      TO S_STATU2-LOW.
     APPEND S_STATU2.
  ELSE.
     MOVE: 'I'      TO S_STATU2-SIGN,
           'EQ'     TO S_STATU2-OPTION,
           'R'      TO S_STATU2-LOW.
     APPEND S_STATU2.
  ENDIF.


ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
FORM P1000_GET_ZVREQHD_ST   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting

  SELECT * INTO TABLE IT_ZVREQ FROM ZVREQHD_ST
                               WHERE EBELN      IN     S_EBELN
                               AND   ZFREQNO    IN     S_REQNO
                               AND   LIFNR      IN     S_LIFNR
                               AND   ZFBENI     IN     S_ZFBENI
                               AND   ZFMAUD     IN     S_ZFMAUD
                               AND   ZFREQDT    IN     S_REQDT
                               AND   CDAT       IN     S_CDAT
                               AND   ZFWERKS    IN     S_WERKS
*                              AND   MATNR      IN     S_MATNR
                               AND   ZFREQTY    IN     S_REQTY
                               AND   ZFRLST1    IN     S_STATUS
                               AND   ZFRLST2    IN     S_STATU2
                               AND ( ZFRVDT     EQ     '00000000'
                               OR    ZFRVDT     EQ     SPACE
                               OR    ZFRVDT     EQ     '        ' )
                               AND   EKORG      LIKE   P_EKORG
                               AND   EKGRP      LIKE   P_EKGRP
                               AND   ERNAM      LIKE   P_ERNAM
                               AND   ZFAMDNO    GT     '00000'
                               AND   ZFDOCST    EQ     'N'
                               AND   ZFCLOSE    EQ     SPACE.
*                              AND   LOEKZ      EQ     SPACE.

  IF SY-SUBRC NE 0.               " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S009.    EXIT.
  ENDIF.

ENDFORM.                    " P1000_GET_ZVREQHD_ST
*&---------------------------------------------------------------------*
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
FORM P2000_CONFIG_CHECK           USING   W_ERR_CHK.
  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFRELYN3 IS INITIAL.
     W_ERR_CHK = 'Y'.   MESSAGE S959.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.
   REFRESH : IT_TAB.

   LOOP AT IT_ZVREQ.

      W_TABIX = SY-TABIX.
* Material Code Check
*      SELECT COUNT( * ) INTO W_COUNT FROM ZTREQIT
*                        WHERE ZFREQNO  EQ  IT_ZVREQ-ZFREQNO
*                        AND   MATNR    IN  S_MATNR.
*      IF W_COUNT < 1.
*         DELETE IT_ZVREQ INDEX W_TABIX.
*         CONTINUE.
*      ENDIF.

      MOVE-CORRESPONDING IT_ZVREQ  TO  IT_TAB.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CLEAR : LFA1.
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-LIFNR
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      CASE SY-SUBRC.
         WHEN 01.     MESSAGE E022.
         WHEN 02.     MESSAGE E950.
         WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
      ENDCASE.
      MOVE: LFA1-NAME1   TO   IT_TAB-NAME1.
*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
      CLEAR : LFA1.
      CALL FUNCTION 'READ_LFA1'
           EXPORTING
                 XLIFNR          = IT_TAB-ZFBENI
           IMPORTING
                 XLFA1           = LFA1
           EXCEPTIONS
                 KEY_INCOMPLETE  = 01
                 NOT_AUTHORIZED  = 02
                 NOT_FOUND       = 03.

      CASE SY-SUBRC.
         WHEN 01.     MESSAGE E022.
         WHEN 02.     MESSAGE E950.
         WHEN 03.     MESSAGE E020   WITH    IT_TAB-LIFNR.
      ENDCASE.
      MOVE: LFA1-NAME1   TO   IT_TAB-NAME2.

      WRITE : IT_TAB-ZFOPAMT  CURRENCY IT_TAB-WAERS TO IT_TAB-ZFOPAMT1,
              IT_TAB-ZFUSDAM  CURRENCY IT_TAB-ZFUSD TO IT_TAB-ZFUSDAM1.

      APPEND  IT_TAB.
   ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIM14'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM14'.           " GUI TITLE SETTING..

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
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
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
     WRITE : / '총', W_COUNT, '건'.
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
       IT_TAB-ZFREQDT NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-EBELN   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-WAERS NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFOPAMT  CURRENCY IT_TAB-WAERS NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQTY NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-EKORG   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFWERKS NO-GAP,               " 대표 Plant
       73 SY-VLINE NO-GAP,
       IT_TAB-LIFNR   NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-NAME1   NO-GAP,
   118 SY-VLINE NO-GAP.

  CASE IT_TAB-ZFRLST1.
     WHEN 'N'.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
     WHEN 'R'.
        FORMAT COLOR COL_TOTAL    INTENSIFIED OFF.
     WHEN 'C'.
        FORMAT COLOR COL_GROUP    INTENSIFIED OFF.
     WHEN OTHERS.
        FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
  ENDCASE.
  WRITE : IT_TAB-ZFRLST1 NO-GAP, SY-VLINE NO-GAP.
* hide
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE:/ SY-VLINE, ' ',
       SY-VLINE NO-GAP,
       IT_TAB-ZFMAUD NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFREQNO NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSD NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFUSDAM  CURRENCY IT_TAB-ZFUSD NO-GAP,
       SY-VLINE NO-GAP,
       IT_TAB-ZFBACD,
       SY-VLINE NO-GAP,
       IT_TAB-EKGRP,
       SY-VLINE NO-GAP,
       IT_TAB-ERNAM   NO-GAP,               " 구매 담?
       SY-VLINE NO-GAP,
       IT_TAB-ZFBENI  NO-GAP,               " Beneficiary
       SY-VLINE NO-GAP,
       IT_TAB-NAME2   NO-GAP,
   118 SY-VLINE NO-GAP,
       IT_TAB-ZFCLOSE NO-GAP,
       SY-VLINE.

* stored value...
  MOVE SY-TABIX  TO W_LIST_INDEX.
  HIDE: W_LIST_INDEX, IT_TAB.
  MODIFY IT_TAB INDEX SY-TABIX.
  W_COUNT = W_COUNT + 1.

  WRITE : / SY-ULINE.
ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_RELEASED_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_RELEASED_UPDATE USING    RELEASED.

  LOOP  AT   IT_SELECTED.
    IF RELEASED EQ 'R'.
       IF IT_SELECTED-ZFRLST1 EQ 'R'.
          MESSAGE I028 WITH IT_SELECTED-ZFREQNO.
          CONTINUE.
       ENDIF.
    ELSEIF RELEASED EQ 'C'.
       IF IT_SELECTED-ZFRLST1 EQ 'C'.
          MESSAGE I029 WITH IT_SELECTED-ZFREQNO.
          CONTINUE.
       ENDIF.
    ENDIF.

    READ TABLE IT_TAB WITH KEY  ZFREQNO = IT_SELECTED-ZFREQNO
                                ZFAMDNO = IT_SELECTED-ZFAMDNO.
    IF SY-SUBRC NE 0.
       MESSAGE I030 WITH IT_SELECTED-ZFREQNO.
       CONTINUE.
    ENDIF.
    W_TABIX = SY-TABIX.

*-----------------------------------------------------------------------
*   후속 작업 체크....
*-----------------------------------------------------------------------
    IF RELEASED EQ 'C'.
       SELECT SINGLE * FROM ZTREQST
                       WHERE ZFREQNO EQ IT_SELECTED-ZFREQNO
                       AND   ZFAMDNO EQ IT_SELECTED-ZFAMDNO.
       IF NOT ZTREQST-ZFRVDT IS INITIAL.
          MESSAGE I333 WITH IT_SELECTED-ZFREQNO
                            IT_SELECTED-ZFAMDNO.
          CONTINUE.
       ENDIF.
    ENDIF.

    IF IT_TAB-UPDATE_CHK NE 'U'.
*-----------------------------------------------------------------------
* lock checking...
*-----------------------------------------------------------------------
       CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
          EXPORTING
             ZFREQNO                =     IT_TAB-ZFREQNO
             ZFAMDNO                =     IT_TAB-ZFAMDNO
          EXCEPTIONS
              OTHERS        = 1.
       IF SY-SUBRC <> 0.
          MESSAGE I510 WITH SY-MSGV1 'Import Document'
                            IT_TAB-ZFREQNO IT_TAB-ZFAMDNO
                  RAISING DOCUMENT_LOCKED.
          CONTINUE.
       ENDIF.
    ENDIF.

    IT_TAB-ZFRLST1 = RELEASED.              " 릴리즈 지?
    IT_TAB-UPDATE_CHK = 'U'.                " DB 반영 여?
    IT_TAB-MARK = 'X'.

    IF RELEASED EQ 'R'.
       IT_TAB-ZFRLDT1 = SY-DATUM.            " 릴리즈 일?
       IT_TAB-ZFRLNM1 = SY-UNAME.          " 릴리즈 담당자.
    ELSEIF RELEASED EQ 'N'.
       CLEAR : IT_TAB-ZFRLDT1, IT_TAB-ZFRLNM1.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.
    W_UPDATE_CNT = W_UPDATE_CNT + 1.
  ENDLOOP.

ENDFORM.                    " P3000_RELEASED_UPDATE
*&---------------------------------------------------------------------*
*&      Form  P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_POPUP_MESSAGE.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = '릴리즈(승인)작업 저장 확인'
             DIAGNOSE_OBJECT = ''
             TEXT_QUESTION   =
                      '릴리즈(승인) 저장 작업을 계속 진행하시겠습니까?'
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_UPDATE
*&---------------------------------------------------------------------*
FORM P3000_DATA_UPDATE.

  LOOP AT IT_TAB   WHERE UPDATE_CHK EQ 'U'.

     CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_RELEASE'
        EXPORTING
           W_ZFREQNO     =    IT_TAB-ZFREQNO
           W_ZFAMDNO     =    IT_TAB-ZFAMDNO
           W_ZFRLST1     =    IT_TAB-ZFRLST1
           W_ZFRLST2     =    ''.

* 수입의뢰 상태 table Select
*     SELECT SINGLE * FROM   ZTREQST
*                     WHERE  ZFREQNO EQ IT_TAB-ZFREQNO
*                     AND    ZFAMDNO EQ IT_TAB-ZFAMDNO.
*
*-----------------------------------------------------------------------
* 이전 data를 Temp Table로 Move
*-----------------------------------------------------------------------

* 변경 데이타 Move
*     MOVE : IT_TAB-ZFRLST1  TO  ZTREQST-ZFRLST1,     " 릴리즈 상?
*            IT_TAB-ZFRLDT1  TO  ZTREQST-ZFRLDT1,     " 릴리즈 일?
*            IT_TAB-ZFRLNM1  TO  ZTREQST-ZFRLNM1.     " 담당?
*
*     UPDATE ZTREQST.                                 " DATA UPDATE
*     IF SY-SUBRC EQ 0.
*-----------------------------------------------------------------------
* 변경이력 작?
*-----------------------------------------------------------------------
*        PERFORM  SET_LC_HEADER_CHANGE_DOCUMENT.      " 변경 이?
*     ELSE.
*        MESSAGE E031 WITH ZTREQHD-ZFREQNO.
*        ROLLBACK WORK.                               " 오류?
*     ENDIF.
*
  ENDLOOP.
*
* IF SY-SUBRC EQ 0.
*    COMMIT WORK.                                   " 정상적인 경?
* ENDIF.

ENDFORM.                    " P3000_DATA_UPDATE

*&---------------------------------------------------------------------*
*&      Form  P2000_REFRESH_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_REFRESH_POPUP_MESSAGE.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = '리스트 REFRESH 확인'
             DIAGNOSE_OBJECT = ''
            TEXT_QUESTION = '먼저 릴리즈(승인) 작업을 저장하시겠습니까?'
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.
ENDFORM.                    " P2000_REFRESH_POPUP_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_POPUP_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_POPUP_MESSAGE.

   CALL  FUNCTION  'POPUP_TO_CONFIRM'
         EXPORTING
             TITLEBAR        = '리스트종료 확인'
             DIAGNOSE_OBJECT = ''
            TEXT_QUESTION = '먼저 릴리즈(승인) 작업을 저장하시겠습니까?'
             TEXT_BUTTON_1   = '확    인'
             TEXT_BUTTON_2   = '아 니 오'
             DEFAULT_BUTTON  = '1'
             DISPLAY_CANCEL_BUTTON = 'X'
             START_COLUMN    = 30
             START_ROW       = 8
         IMPORTING
             ANSWER          =  W_BUTTON_ANSWER.

ENDFORM.                    " P2000_EXIT_POPUP_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_UNLOCK
*&---------------------------------------------------------------------*
FORM P2000_DATA_UNLOCK.

  LOOP AT IT_TAB   WHERE UPDATE_CHK EQ 'U'.

     CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
          EXPORTING
               ZFREQNO                =     IT_TAB-ZFREQNO
               ZFAMDNO                =     IT_TAB-ZFAMDNO.

  ENDLOOP.

ENDFORM.                    " P2000_DATA_UNLOCK
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO   P_ZFAMDNO.
   SET PARAMETER ID 'BES'       FIELD ''.
   SET PARAMETER ID 'ZPOPNNO'   FIELD ''.
   SET PARAMETER ID 'ZPREQNO'   FIELD P_ZFREQNO.
   SET PARAMETER ID 'ZPAMDNO'   FIELD P_ZFAMDNO.
*   EXPORT 'BES'           TO MEMORY ID 'BES'.
*   EXPORT 'ZPREQNO'       TO MEMORY ID 'ZPREQNO'.
*   EXPORT 'ZPOPNNO'       TO MEMORY ID 'ZPOPNNO'.
   IF SY-UCOMM EQ 'DISP'.
      CALL TRANSACTION 'ZIM13' AND SKIP  FIRST SCREEN.
   ELSEIF SY-UCOMM EQ 'DIS1'.
      CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.
   ENDIF.
ENDFORM.                    " P2000_SHOW_LC
