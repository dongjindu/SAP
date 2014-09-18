*&---------------------------------------------------------------------*
*& Report  ZRIMACTLIST                                                 *
*&---------------------------------------------------------------------*
*&     PROGRAM : Actual arrival date List                              *
*&        Name : Chul-Woo Nam                                          *
*&        Date : 2003.12.08                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMACTLIST  MESSAGE-ID ZIM
                     LINE-SIZE 130
                     NO STANDARD PAGE HEADING.

TABLES : ZTBL,
         EKKO,
         ZTREQHD,
         ZTIMIMG00,
         LFA1,
         EKPO,
         ZSACTLIST,
         SPOP.

*> TABLE CONTROL.
CONTROLS  TC_1100 TYPE TABLEVIEW USING SCREEN 1100.

DATA   : G_PARM_LINE       LIKE SY-TABIX,
         W_ERR_CHK(1)      TYPE C,
         W_TABIX           LIKE SY-TABIX,
         P_BUKRS           LIKE ZTIMIMG00-ZFBUKRS,
         G_PARAM_LINE      TYPE   I,
         W_LOOPLINES       LIKE SY-LOOPC,
         W_COUNTER1        LIKE SY-LOOPC,
         W_COUNTER         LIKE SY-LOOPC,
         OK-CODE           LIKE SY-UCOMM,
         W_OK_CODE         LIKE SY-UCOMM,
         F(20)             TYPE C,             " Field Name Alias
         LINE              TYPE I,
         W_ROWMARK         TYPE C,
         W_GUBUN           TYPE C,
         ANTWORT           TYPE C,
         W_COUNT           TYPE I,             " 전체 COUNT
         CANCEL_OPTION     TYPE C,
         OPTION(1)         TYPE C,
         TEXTLEN           TYPE I.

DATA : IT_ZSACTLIST       LIKE ZSACTLIST OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSACTLIST_ORG   LIKE ZSACTLIST OCCURS 0 WITH HEADER LINE.

*-----------------------------------------------------------------------
* B/L 입수내역 리스트용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
        ZFREBELN     LIKE ZTBL-ZFREBELN,           "대표 PO번호
        ZFBLNO       LIKE ZTBL-ZFBLNO,             "B/L관리번호
        ZFSHNO       LIKE ZTBL-ZFSHNO,             "선적차수
        ZFPOYN       LIKE ZTBL-ZFPOYN,             "유환여부
        ZFETD        LIKE ZTBL-ZFETD,              "ETD
        ZFCARNM      LIKE ZTBL-ZFCARNM,            "선명
        ZFSPRT       LIKE ZTBL-ZFSPRT,             "선적항
        EKGRP        LIKE ZTBL-EKGRP,              "구매그룹
        W_EKGRP(20)  TYPE C,
        LIFNR        LIKE ZTBL-LIFNR,              "Vendor
        W_LIFNR(30)  TYPE C,
        ZFHBLNO      LIKE ZTBL-ZFHBLNO,            "House B/L
        ZFRGDSR      LIKE ZTBL-ZFRGDSR,            "대표품명
        ZFETA        LIKE ZTBL-ZFETA,              "ETA
        ZFVIA        LIKE ZTBL-ZFVIA,              "VIA
        ZFAPRT       LIKE ZTBL-ZFAPRT,             "도착항
        ZFFORD       LIKE ZTBL-ZFFORD,             "선사
        ZFRETA       LIKE ZTBL-ZFRETA,
        W_RETA(8)    TYPE C,
        W_ZFFORD(30) TYPE C,
        ZFBENI       LIKE ZTBL-ZFBENI,             "Beneficiary
        W_ZFBENI(30) TYPE C,
        ZFREQNO      LIKE ZTREQHD-ZFREQNO,         "수입의뢰관리번호.
        ZFREQTY      LIKE ZTREQHD-ZFREQTY,         "결제구분.
        ZFOPNNO      LIKE ZTREQHD-ZFOPNNO,         "신용장번호.
        W_PO(20)     TYPE C,
        W_DOM_TEXT(20) TYPE C.
DATA : END OF IT_TAB.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
*INCLUDE   ZRIMPRELTOP.    " 구매 Released  Report Data Define용 Include
*INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTBL-BUKRS NO INTERVALS
                                            NO-EXTENSION,
                   S_EBELN   FOR ZTBL-ZFREBELN,
                   S_REQTY   FOR ZTREQHD-ZFREQTY,  " 결제구분.
                   S_HBLNO   FOR ZTBL-ZFHBLNO,     " House B/L No.
                   S_BLADT   FOR ZTBL-ZFBLADT      " B/L 입수일.
                             NO-EXTENSION,
                   S_RPTTY   FOR ZTBL-ZFRPTTY,
                   S_EKGRP   FOR ZTBL-EKGRP,       " 구매그룹.
*                   S_ZFTRCK  FOR ZTBL-ZFTRCK,      " TRUCKER
                   S_ETA     FOR ZTBL-ZFETA        " ETA
                             NO-EXTENSION,
                   S_SPRTC   FOR ZTBL-ZFSPRTC      " 선적항
                             NO INTERVALS.
   PARAMETERS :    P_VIA     LIKE ZTBL-ZFVIA.      " VIA
   SELECT-OPTIONS: S_FORD    FOR ZTBL-ZFFORD.      " Forwarder
   PARAMETERS :    P_POYN    LIKE ZTBL-ZFPOYN.     " 유환여부
   SELECT-OPTIONS: S_SHTY    FOR ZTBL-ZFSHTY,      " 해상운송구분
                   S_WERKS   FOR ZTBL-ZFWERKS.     " 대표 PLANT
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   PARAMETERS : P_ZFRETA    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

* PARAMETER 초기값 Setting..
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P1000_SET_BUKRS.
  PERFORM   P2000_SET_PARAMETER.

* Title Text Write..
*TOP-OF-PAGE.
*  PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* 파라메타 설정.
  PERFORM   P2000_SET_SELETE_OPTION   USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 구매의뢰 테이블 SELECT.
  PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 관련 Text Table SELECT..
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* NCW Insert
  CALL SCREEN 1100.

* Report Write..
*  PERFORM   P3000_DATA_WRITE          USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM25'.          " TITLE BAR
ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
FORM P2000_SET_SELETE_OPTION   USING    W_ERR_CHK.
*
  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF P_VIA  IS INITIAL.  P_VIA  = '%'.   ENDIF.
  IF P_POYN IS INITIAL.  P_POYN = '%'.   ENDIF.

ENDFORM.                    " P2000_SET_SELETE_OPTION
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    W_ERR_CHK.

  CLEAR : IT_TAB.

  LOOP AT IT_TAB.

    W_TABIX = SY-TABIX.

    MOVE  IT_TAB-ZFRETA  TO  IT_TAB-W_RETA.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
*-----------------------------------------------------------------------
    CLEAR : LFA1.
    CALL FUNCTION 'READ_LFA1'
         EXPORTING
              XLIFNR         = IT_TAB-ZFFORD
         IMPORTING
              XLFA1          = LFA1
         EXCEPTIONS
              KEY_INCOMPLETE = 01
              NOT_AUTHORIZED = 02
              NOT_FOUND      = 03.

    MOVE: LFA1-NAME1   TO   IT_TAB-W_ZFFORD.

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

    MOVE: LFA1-NAME1   TO   IT_TAB-W_LIFNR.

*-----------------------------------------------------------------------
* VENDOR MASTER SELECT( LFA1 )
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

    MOVE: LFA1-NAME1   TO   IT_TAB-W_ZFBENI.

*-----------------------------------------------------------------------
* T024 SELECT( 구매그룹)
*-----------------------------------------------------------------------
    SELECT SINGLE EKNAM INTO IT_TAB-W_EKGRP
      FROM T024
     WHERE EKGRP = IT_TAB-EKGRP.
*    CASE IT_TAB1-ZFCUST.
*      WHEN '1'.
*        MOVE 'Declaration Creation' TO IT_TAB1-W_ZFCUST.
*      WHEN '2'.
*        MOVE 'object to declare'    TO IT_TAB1-W_ZFCUST.
*      WHEN '3'.
*        MOVE 'in declaring'         TO IT_TAB1-W_ZFCUST.
*      WHEN 'Y'.
*        MOVE 'Completed Clearance'  TO IT_TAB1-W_ZFCUST.
*      WHEN 'N'.
*        MOVE 'Not object to clear'  TO IT_TAB1-W_ZFCUST.
*    ENDCASE.
    MODIFY IT_TAB INDEX SY-TABIX.
  ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_LC
*&---------------------------------------------------------------------*
FORM P2000_SHOW_LC USING    P_ZFREQNO.
  SET PARAMETER ID 'ZPHBLNO'   FIELD ''.
  SET PARAMETER ID 'ZPBLNO'    FIELD P_ZFREQNO.
  EXPORT 'ZPBLNO'        TO MEMORY ID 'ZPBLNO'.
  EXPORT 'ZPHBLNO'       TO MEMORY ID 'ZPHBLNO'.

* JSY 주석처리 2003.04.01
* READ ZTIMIMG00.
*  SELECT SINGLE * FROM ZTIMIMG00.
*  IF ZTIMIMG00-BLSTYN EQ 'X'.
  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.
*  ELSE.
*     CALL TRANSACTION 'ZIM22' AND SKIP  FIRST SCREEN.
*  ENDIF.

* 구매의뢰 테이블 SELECT
  PERFORM   P1000_GET_IT_TAB          USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
* 레포트 관련 Text Table SELECT
  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
*  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*  PERFORM RESET_LIST.
ENDFORM.                    " P2000_SHOW_LC
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
FORM P1000_GET_IT_TAB USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.                " Error Bit Setting
  REFRESH : IT_TAB.  CLEAR : IT_TAB.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE IT_TAB  FROM ZTBL
*                               WHERE ZFBLSDT    IN     S_BLSDT
                             WHERE   BUKRS      IN     S_BUKRS
*                               AND   ZFBLSDT    NE     SPACE
                               AND   ZFBLADT    IN     S_BLADT
*                               AND   ZFTRCK     IN     S_ZFTRCK
                               AND   ZFETA      IN     S_ETA
                               AND   ZFWERKS    IN     S_WERKS
                               AND   ZFSPRTC    IN     S_SPRTC
                               AND   ZFREBELN   IN     S_EBELN
                               AND   ZFHBLNO    IN     S_HBLNO
                               AND   EKGRP      IN     S_EKGRP
*                               AND   ZFBLST     IN     S_ZFBLST
                               AND   ZFRPTTY    IN     S_RPTTY
                               AND   ZFVIA      LIKE   P_VIA
                               AND   ZFFORD     IN     S_FORD
                               AND   ZFPOYN     LIKE   P_POYN
                               AND   ZFSHTY     IN     S_SHTY
                               AND   ZFWERKS    IN     S_WERKS.

  IF SY-SUBRC NE 0.                   " Not Found?
     W_ERR_CHK = 'Y'.  MESSAGE S966.    EXIT.
  ENDIF.

  IF P_ZFRETA IS INITIAL.             " 입력되어 있지 않다면
     LOOP AT IT_TAB.
       W_TABIX = SY-TABIX.

*  "DOMAIN - 유환여부.
       PERFORM  GET_DD07T USING 'ZDPOYN' IT_TAB-ZFPOYN
                       CHANGING   IT_TAB-W_DOM_TEXT.

       CONCATENATE IT_TAB-ZFREBELN '-' IT_TAB-ZFSHNO INTO IT_TAB-W_PO.

       IF IT_TAB-ZFRETA IS INITIAL.
          MODIFY IT_TAB INDEX W_TABIX.
       ELSE.
          DELETE IT_TAB INDEX W_TABIX.
       ENDIF.

     ENDLOOP.
  ELSE.                               " 입력된 것만 체크
     LOOP AT IT_TAB.
       W_TABIX = SY-TABIX.

       IF IT_TAB-ZFRETA IS INITIAL.
          DELETE IT_TAB INDEX W_TABIX.
       ELSE.
          MODIFY IT_TAB INDEX W_TABIX.
       ENDIF.

     ENDLOOP.
  ENDIF.

* 화면상의 INTERNAL TABLE에 MOVE.
*  IT_ZSACTLIST[]     =  IT_TAB[].
*  IT_ZSACTLIST_ORG[] =  IT_TAB[].

   LOOP AT IT_TAB.
     MOVE-CORRESPONDING IT_TAB TO IT_ZSACTLIST.
     APPEND IT_ZSACTLIST.
   ENDLOOP.

ENDFORM.                    " P1000_GET_IT_TAB

*&---------------------------------------------------------------------*
*&      Form  GET_DD07T_SELECT
*&---------------------------------------------------------------------*
FORM GET_DD07T USING    P_DOMNAME
                        P_FIELD
               CHANGING P_W_NAME.
  CLEAR : DD07T, P_W_NAME.

  IF P_FIELD IS INITIAL.   EXIT.   ENDIF.

  SELECT * FROM DD07T WHERE DOMNAME     EQ P_DOMNAME
                      AND   DDLANGUAGE  EQ SY-LANGU
                      AND   AS4LOCAL    EQ 'A'
                      AND   DOMVALUE_L  EQ P_FIELD
                      ORDER BY AS4VERS DESCENDING.
    EXIT.
  ENDSELECT.

  P_W_NAME   = DD07T-DDTEXT.
  TRANSLATE P_W_NAME TO UPPER CASE.
ENDFORM.                    " GET_DD07T_SELECT
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
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

*&---------------------------------------------------------------------*
*&      Form  p2000_dsma
*&---------------------------------------------------------------------*
FORM P2000_DSMA USING    EBELN EBELP.
  DATA: MM03_START_SICHT(15) TYPE C  VALUE 'BDEKLPQSVXZA'.
  SELECT SINGLE *
           FROM EKPO
          WHERE EBELN = EBELN
            AND EBELP = EBELP.
  SET PARAMETER ID 'MAT' FIELD EKPO-MATNR.
  SET PARAMETER ID 'BUK' FIELD EKPO-BUKRS.
  SET PARAMETER ID 'WRK' FIELD EKPO-WERKS.
  SET PARAMETER ID 'LAG' FIELD ''.
  SET PARAMETER ID 'MXX' FIELD MM03_START_SICHT.
  CALL TRANSACTION 'MM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " p2000_dsma
*&---------------------------------------------------------------------*
*&      Form  p2000_DSPO
*&---------------------------------------------------------------------*
FORM P2000_DSPO USING    EBELN.
  SELECT SINGLE * FROM EKKO
         WHERE    EBELN EQ EBELN.
  IF SY-SUBRC EQ 0.

    IF EKKO-BSTYP EQ 'K'.
      SET PARAMETER ID 'CTR' FIELD EBELN.
      CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
    ELSEIF EKKO-BSTYP EQ 'L'.
      SET PARAMETER ID 'SAG' FIELD EBELN.
      CALL TRANSACTION 'ME33L' AND SKIP  FIRST SCREEN.
    ELSE.
      SET PARAMETER ID 'BSP' FIELD ''.
      EXPORT 'BSP' TO MEMORY ID 'BSP'.
      SET PARAMETER ID 'BES' FIELD EBELN.
      EXPORT 'BES'  TO MEMORY ID 'BES'.
      CALL TRANSACTION 'ME23N' AND SKIP  FIRST SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.                    " p2000_DSPO
*&---------------------------------------------------------------------*
*&      Form  P2000_DSBL
*&---------------------------------------------------------------------*
FORM P2000_DSBL USING    ZFBLNO.

  SET PARAMETER ID 'ZPBLNO'  FIELD ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DSBL
*&---------------------------------------------------------------------*
*&      Form  P2000_DSIB
*&---------------------------------------------------------------------*
FORM P2000_DSIB USING    VBELN.

  SET PARAMETER ID 'VLM' FIELD VBELN.
  CALL TRANSACTION 'VL33N' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_DSIB
*&---------------------------------------------------------------------*
*&      Form  P2000_SAV1
*&---------------------------------------------------------------------*
FORM P2000_SAV1 USING    P_IT_TAB_ZFBLNO.

*    MOVE IT_TAB-ZFRETA


ENDFORM.                    " P2000_SAV1
*&---------------------------------------------------------------------*
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
MODULE PF_STATUS_SCRCOM OUTPUT.

   SET PF-STATUS 'ZIM25N'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM25'.           " GUI TITLE SETTING..

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1100 OUTPUT.

  DESCRIBE TABLE IT_TAB LINES G_PARM_LINE.        " LINE 수 GET
  TC_1100-LINES = G_PARM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  FILL_TC_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
MODULE FILL_TC_SCR1100 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_1100-CURRENT_LINE GT TC_1100-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
* Internal Table Read ( Line별 )
  READ TABLE IT_TAB INDEX TC_1100-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING  IT_TAB  TO  ZSACTLIST.
  ENDIF.

ENDMODULE.                 " FILL_TC_SCR1100  OUTPUT

*INCLUDE ZRIMZCTLIST.
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE GET_LINE_SCR1100 INPUT.
  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1100-CURRENT_LINE + LINE - 1.
ENDMODULE.                 " GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE TC_1100_UPDATE_SCR1100 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSACTLIST INDEX TC_1100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

*  MOVE-CORRESPONDING ZSBKCF TO IT_ZSACTLIST.
  MOVE      W_ROWMARK       TO IT_ZSACTLIST-ZFMARK.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_ZSACTLIST   INDEX W_TABIX.
*  ELSE.
*     APPEND  IT_ZSACTLIST.
  ENDIF.

ENDMODULE.                 " TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_SCR1100 INPUT.

   CASE SY-UCOMM.
      WHEN 'CANC'.
           PERFORM P2000_CANCEL_MESSAGE.
      WHEN 'EXIT' OR 'BACK'.
           CLEAR  W_GUBUN.
           PERFORM P2000_SET_MODIFY_CHECK.
           IF W_LOOPLINES >  0 .
              PERFORM P2000_EXIT_MESSAGE.
              IF ANTWORT EQ  'Y'.
                 PERFORM P2000_SAVE_PROCESS.
              ELSE.
                 LEAVE TO SCREEN 0.                " 종?
              ENDIF.
           ELSE.
              LEAVE TO SCREEN 0.                " 종?
           ENDIF.
      WHEN 'SAVE'.
           PERFORM P2000_SAVE_MESSAGE.
           IF ANTWORT EQ  'Y'.
              PERFORM P2000_SAVE_PROCESS.
              LEAVE TO SCREEN 0.
           ENDIF.
      WHEN 'SHBL'.
           CLEAR W_COUNT.
           LOOP AT IT_ZSACTLIST WHERE ZFMARK = 'X'.
              ADD 1 TO W_COUNT.
           ENDLOOP.
           CASE W_COUNT.
              WHEN 1.
                 READ TABLE IT_ZSACTLIST WITH KEY ZFMARK = 'X'.
                 PERFORM  P2000_BL_DOC_DISPLAY
                                       USING  IT_ZSACTLIST-ZFBLNO.
              WHEN 0.
                 IF LINE GT 0.
                    READ TABLE IT_ZSACTLIST INDEX LINE.
                    IF SY-SUBRC EQ 0.
                       PERFORM  P2000_BL_DOC_DISPLAY
                                       USING  IT_ZSACTLIST-ZFBLNO.
                    ELSE.
                       MESSAGE S962.
                    ENDIF.
                 ELSE.
                    MESSAGE S962.
                 ENDIF.
              WHEN OTHERS.
                 MESSAGE S965.
           ENDCASE.

      WHEN 'SHPO'.
           CLEAR W_COUNT.
           LOOP AT IT_ZSACTLIST WHERE ZFMARK = 'X'.
              ADD 1 TO W_COUNT.
           ENDLOOP.
           CASE W_COUNT.
              WHEN 1.
                 READ TABLE IT_ZSACTLIST WITH KEY ZFMARK = 'X'.
*                 PERFORM P2000_DSPO USING IT_TAB-ZFREBELN.
                 PERFORM  P2000_PO_DOC_DISPLAY
                                       USING  IT_ZSACTLIST-ZFREBELN.
              WHEN 0.
                 IF LINE GT 0.
                    READ TABLE IT_ZSACTLIST INDEX LINE.
                    IF SY-SUBRC EQ 0.
                       PERFORM  P2000_PO_DOC_DISPLAY
                                       USING  IT_ZSACTLIST-ZFREBELN.
                    ELSE.
                       MESSAGE S962.
                    ENDIF.
                 ELSE.
                    MESSAGE S962.
                 ENDIF.
              WHEN OTHERS.
                 MESSAGE S965.
           ENDCASE.
      WHEN OTHERS.
   ENDCASE.

ENDMODULE.                 " USER_COMMAND_SCR1100  INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

     PERFORM P2000_MESSAGE_BOX USING 'Cancel Confirmation'
                         'Do end without save the changed contents.'
                         'Do you want to end?'
                         'N'
                         '2'.

  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      MESSAGE  S957.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_CANCEL_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

     PERFORM P2000_MESSAGE_BOX USING 'End Confirmation'
                          'Do not save the entering data.'
                          'Do you want to end after save?'
                          'Y'
                          '1'.

ENDFORM.                    " P2000_EXIT_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
FORM P2000_MESSAGE_BOX USING    TITLE  LIKE SPOP-TITEL
                                TEXT1  LIKE SPOP-TEXTLINE1
                                TEXT2  LIKE SPOP-TEXTLINE2
                                CANCEL LIKE CANCEL_OPTION
                                DEFAULT LIKE OPTION.

  SPOP-TITEL = TITLE.
  SPOP-TEXTLINE1 = TEXT1.
  SPOP-TEXTLINE2 = TEXT2.
  IF CANCEL EQ 'Y'.
    CANCEL_OPTION = 'Y'.
  ELSE.
    CLEAR : CANCEL_OPTION.
  ENDIF.
  OPTION = DEFAULT.
  TEXTLEN = 40.

  CALL SCREEN 0001 STARTING AT 30 6
                   ENDING   AT 78 10.

  IF ANTWORT = 'C'.                                         " Cancel
    SET SCREEN SY-DYNNR.
  ENDIF.

ENDFORM.                    " P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_PROCESS
*&---------------------------------------------------------------------*
FORM P2000_SAVE_PROCESS.

  LOOP AT IT_TAB.
    CLEAR : ZTBL.

***INSERT BY FURONG
    SELECT SINGLE *
      FROM ZTBL
     WHERE ZFBLNO = IT_TAB-ZFBLNO
       and ZFREBELN = IT_TAB-ZFREBELN.

*    SELECT SINGLE *
*      FROM ZTBL
*     WHERE ZFREBELN = IT_TAB-ZFREBELN.

*** END INSERT

    IF SY-SUBRC EQ 0.
       MOVE IT_TAB-ZFRETA TO ZTBL-ZFRETA.
       MODIFY ZTBL.
    ENDIF.

  ENDLOOP.

***INSERT BY FURONG
  COMMIT WORK.
*** END INSERT
ENDFORM.                    " P2000_SAVE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.

     PERFORM P2000_MESSAGE_BOX USING 'Save Cnofirmation'
                          'Do save the entered detail.'
                          'Do you want to save?'
                          'Y'
                          '1'.

ENDFORM.                    " P2000_SAVE_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM P2000_SET_MODIFY_CHECK.

  DESCRIBE TABLE IT_ZSACTLIST        LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSACTLIST_ORG    LINES W_COUNTER1.
  W_LOOPLINES = 0.

*  LOOP AT IT_ZSACTLIST_ORG.
*  READ TABLE IT_ZSACTLIST WITH KEY ZFREQNO = IT_ZSACTLIST_ORG-ZFREQNO.
*       IF SY-SUBRC EQ 0.
*          IF IT_ZSACTLIST_ORG-ZFOPBN NE IT_ZSACTLIST-ZFOPBN.
*             W_LOOPLINES = 1. EXIT.
*          ENDIF.
*       ENDIF.
*  ENDLOOP.

ENDFORM.                    " P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
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
MODULE MODIFY_SCREEN_SCR0001 OUTPUT.

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
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " MODIFY_SCREEN_SCR0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0001 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.
    WHEN 'ENTR'.   ANTWORT = 'Y'.
    WHEN 'YES'.    ANTWORT = 'Y'.
    WHEN 'NO'.     ANTWORT = 'N'.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
       ANTWORT = 'Y'.
  ENDCASE.

  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " GET_OK_CODE_SCR0001  INPUT
*&---------------------------------------------------------------------*
*&      Module  ZFRETA_CHECK_SCR1100  INPUT
*&---------------------------------------------------------------------*
MODULE ZFRETA_CHECK_SCR1100 INPUT.

* Checking the Actual arrival date with ETD
  IF ZSACTLIST-ZFRETA LT ZSACTLIST-ZFETD.
     MESSAGE E977 WITH
     'The actual arrival date is less than ETD'.
  ENDIF.

  READ TABLE IT_ZSACTLIST INDEX TC_1100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

*> 2001.07.09 ksb modify
*  MOVE-CORRESPONDING ZSBKCF TO IT_ZSBKCF.
  MOVE ZSACTLIST-ZFRETA         TO IT_ZSACTLIST-ZFRETA.

*  CLEAR : IT_ZSBKCF-NAME1.
*  SELECT SINGLE NAME1 INTO IT_ZSBKCF-NAME1 FROM LFA1
*                      WHERE LIFNR EQ IT_ZSBKCF-ZFOPBN.

*> 2001.07.09 ksb modify
*  MOVE : IT_ZSBKCF-NAME1 TO  ZSBKCF-NAME1.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_ZSACTLIST   INDEX W_TABIX.
*  ELSE.
*     APPEND  IT_ZSBKCF.
  ENDIF.

*>> NCW Insert
  CLEAR : W_SY_SUBRC, W_TABIX.

  READ TABLE IT_TAB INDEX TC_1100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

  MOVE IT_ZSACTLIST-ZFRETA         TO IT_TAB-ZFRETA.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_TAB  INDEX W_TABIX.
*  ELSE.
*     APPEND  IT_ZSBKCF.
  ENDIF.

ENDMODULE.                 " ZFRETA_CHECK_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_PO_DOC_DISPLAY USING S_EBELN.

  SELECT SINGLE * FROM EKKO
         WHERE EBELN EQ S_EBELN.

  CASE EKKO-BSTYP.
     WHEN 'L'.
        SET PARAMETER ID 'SAG' FIELD S_EBELN.
        CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
     WHEN 'K'.
        SET PARAMETER ID 'CTR' FIELD S_EBELN.
        CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
     WHEN OTHERS.
        SET PARAMETER ID 'BES' FIELD S_EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

   ENDCASE.

ENDFORM.                    " P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
FORM P2000_BL_DOC_DISPLAY USING ZFBLNO .

  SET PARAMETER ID 'ZPBLNO'  FIELD ZFBLNO.
  SET PARAMETER ID 'ZPHBLNO' FIELD ''.

  CALL TRANSACTION 'ZIM23' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_BL_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
