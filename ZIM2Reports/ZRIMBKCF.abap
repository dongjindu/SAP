*&---------------------------------------------------------------------*
*& Report  ZRIMBKCF                                                    *
*&---------------------------------------------------------------------*
*&  프로그램명 : 은행배정 PROGRAM                                      *
*&      작성자 : 나현주 INFOLINK Ltd.                                  *
*&      작성일 : 2000.03.05                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : Config에서 구매 Released 여부, 은행 배정 여부를       *
*&               CHECK하여서 DATA를 SELECT한다.
*&---------------------------------------------------------------------*
*& [변경내용]
*&    2001.08.21 강석봉 수정작업.
*&    1. 문서별 TEXT SETTING 로직 추가.
*&    2. 문서별 SELECT/SAVE 로직 FUNCTION으로 바꿈.
*&    3. 문서별 EDI CHECK BIT 로직 추가.
*&    4. 문서별 CHANGE DOCUMENT 로직 추가.
*&---------------------------------------------------------------------*
REPORT  ZRIMBKCF    MESSAGE-ID  ZIM
                     LINE-SIZE   120
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Tables 및 변수 Define
*-----------------------------------------------------------------------
INCLUDE   ZRIMBKCFTOP.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                               NO INTERVALS,
                   S_EBELN   FOR ZTREQHD-EBELN,    " Purchasing document
                   S_REQNO   FOR ZTREQHD-ZFREQNO,  " 수입의뢰 관리번?
                   S_LIFNR   FOR ZTREQHD-LIFNR,    " vendor
                   S_ZFBENI  FOR ZTREQHD-ZFBENI,   " Beneficiary
                   S_ZFMAUD  FOR ZTREQHD-ZFMAUD,   " 자재납?
                   S_REQDT   FOR ZTREQST-ZFREQDT,  " 요개설일?
                   S_CDAT    FOR ZTREQST-CDAT,     " Created on
                   S_WERKS   FOR ZTREQHD-ZFWERKS.   " Plant
   PARAMETERS :    P_EKGRP   LIKE ZTREQST-EKGRP,
                   P_EKORG   LIKE ZTREQST-EKORG,
                   P_ERNAM   LIKE ZTREQST-ERNAM.
   SELECT-OPTIONS: S_REQTY   FOR ZTREQHD-ZFREQTY.  " 수입의뢰 Type
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
   PARAMETERS : P_BKCF    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P1000_SET_BUKRS.            " Default 회사코드 SET.
   PERFORM   P2000_SET_PARAMETER.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
* DATA SELECT.
   PERFORM  P2000_BKCF_READ        USING  W_ERR_CHK.
   IF W_ERR_CHK = 'Y'.  EXIT.   ENDIF.

   CALL SCREEN 1100.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.
  SET  TITLEBAR 'ZIM10'.          " TITLE BAR
  CLEAR  P_BKCF.
ENDFORM.                    " P2000_SET_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  P2000_BKCF_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_BKCF_READ USING    P_W_ERR_CHK.
   RANGES : R_REQTY   FOR   ZTREQHD-ZFREQTY  OCCURS   10.

   MOVE 'N'  TO  W_ERR_CHK.

   SELECT SINGLE * FROM ZTIMIMG00.
   IF SY-SUBRC NE 0. MESSAGE S963. LEAVE TO SCREEN 0.   ENDIF.

* 은행배정작업을 하지 않을 경우는...
   IF ZTIMIMG00-ZFBKYN NE 'X'.
      MESSAGE S475. EXIT.
   ELSE.
      IF ZTIMIMG00-LCBKYN EQ 'X'.           "> MASTER L/C
         MOVE : 'I'       TO     R_REQTY-SIGN,
                'EQ'      TO     R_REQTY-OPTION,
                'LC'      TO     R_REQTY-LOW,
                SPACE     TO     R_REQTY-HIGH.
         APPEND R_REQTY.
      ENDIF.
      IF ZTIMIMG00-LOBKYN EQ 'X'.           "> LOCAL L/C
         MOVE : 'I'       TO     R_REQTY-SIGN,
                'EQ'      TO     R_REQTY-OPTION,
                'LO'      TO     R_REQTY-LOW,
                SPACE     TO     R_REQTY-HIGH.
         APPEND R_REQTY.
      ENDIF.
      IF ZTIMIMG00-PUBKYN EQ 'X'.           "> 구매승인서.
         MOVE : 'I'       TO     R_REQTY-SIGN,
                'EQ'      TO     R_REQTY-OPTION,
                'PU'      TO     R_REQTY-LOW,
                SPACE     TO     R_REQTY-HIGH.
         APPEND R_REQTY.
      ENDIF.
      IF ZTIMIMG00-DABKYN EQ 'X'.
         MOVE : 'I'       TO     R_REQTY-SIGN,
                'EQ'      TO     R_REQTY-OPTION,
                'DA'      TO     R_REQTY-LOW,
                SPACE     TO     R_REQTY-HIGH.
         APPEND R_REQTY.
      ENDIF.
      IF ZTIMIMG00-DPBKYN EQ 'X'.
         MOVE : 'I'       TO     R_REQTY-SIGN,
                'EQ'      TO     R_REQTY-OPTION,
                'DP'      TO     R_REQTY-LOW,
                SPACE     TO     R_REQTY-HIGH.
         APPEND R_REQTY.
      ENDIF.
      IF ZTIMIMG00-TTBKYN EQ 'X'.
         MOVE : 'I'       TO     R_REQTY-SIGN,
                'EQ'      TO     R_REQTY-OPTION,
                'TT'      TO     R_REQTY-LOW,
                SPACE     TO     R_REQTY-HIGH.
         APPEND R_REQTY.
      ENDIF.
      IF ZTIMIMG00-GSMBKYN EQ 'X'.
         MOVE : 'I'       TO     R_REQTY-SIGN,
                'EQ'      TO     R_REQTY-OPTION,
                'GS'      TO     R_REQTY-LOW,
                SPACE     TO     R_REQTY-HIGH.
         APPEND R_REQTY.
      ENDIF.
   ENDIF.

* 수입의뢰 승인 여부에 따라서...
   IF ZTIMIMG00-ZFRELYN1  =   'X'.
      W_YN1     =   'R'.
      W_YN2     =   'R'.
   ELSE.
      W_YN1     =   'N'.
      W_YN2     =   'C'.
   ENDIF.
   IF P_EKORG IS INITIAL.
      P_EKORG = '%'.
   ELSE.
      CONCATENATE P_EKORG '%' INTO P_EKORG.
   ENDIF.
   IF P_EKGRP IS INITIAL.
      P_EKGRP = '%'.
   ELSE.
      CONCATENATE P_EKGRP '%' INTO P_EKGRP.
   ENDIF.
   IF P_ERNAM IS INITIAL.
      P_ERNAM = '%'.
   ELSE.
      CONCATENATE P_ERNAM '%' INTO P_ERNAM.
   ENDIF.

* 개설 승인여부에 따라서...
   IF ZTIMIMG00-ZFRELYN2  =   'X'.
      SELECT  *
      INTO    CORRESPONDING FIELDS OF TABLE IT_TAB
      FROM    ZTREQHD AS H INNER JOIN ZTREQST AS I
      ON      H~ZFREQNO   EQ   I~ZFREQNO
      WHERE   H~EBELN     IN   S_EBELN
      AND     H~ZFREQNO   IN   S_REQNO
      AND     H~LIFNR     IN   S_LIFNR
      AND     H~ZFBENI    IN   S_ZFBENI
      AND     H~ZFMAUD    IN   S_ZFMAUD
      AND     H~ZFWERKS   IN   S_WERKS
      AND     H~ZFREQTY   IN   S_REQTY
      AND     H~ZFREQTY   IN   R_REQTY
      AND     I~ZFREQDT   IN   S_REQDT
      AND     I~CDAT      IN   S_CDAT
      AND     I~EKORG     LIKE P_EKORG
      AND     I~EKGRP     LIKE P_EKGRP
      AND     I~ZFOPNNM   LIKE P_ERNAM
      AND     I~ZFAMDNO   EQ   '00000'
      AND   ( I~ZFRLST1   EQ   W_YN1
      OR      I~ZFRLST1   EQ   W_YN2 )
      AND   ( I~ZFRLST2   EQ   'N'
      OR      I~ZFRLST2   EQ   'C' )
      AND     H~ZFUSDAM   >=   ZTIMIMG00-ZFUSDAM.
*      AND   ( H~ZFREQTY   EQ   'LC'
*      OR      H~ZFREQTY   EQ   'GS' ).
   ELSE.
      SELECT  *
      INTO    CORRESPONDING FIELDS OF TABLE IT_TAB
      FROM    ZTREQHD AS H INNER JOIN ZTREQST AS I
      ON      H~ZFREQNO   EQ   I~ZFREQNO
      WHERE   H~EBELN     IN   S_EBELN
      AND     H~ZFREQNO   IN   S_REQNO
      AND     H~LIFNR     IN   S_LIFNR
      AND     H~ZFBENI    IN   S_ZFBENI
      AND     H~ZFMAUD    IN   S_ZFMAUD
      AND     H~ZFWERKS   IN   S_WERKS
      AND     H~ZFREQTY   IN   S_REQTY
      AND     H~ZFREQTY   IN   R_REQTY
      AND     I~ZFREQDT   IN   S_REQDT
      AND     I~CDAT      IN   S_CDAT
      AND     I~EKORG     LIKE P_EKORG
      AND     I~EKGRP     LIKE P_EKGRP
      AND     I~ZFOPNNM   LIKE P_ERNAM
      AND     I~ZFAMDNO   EQ   '00000'
      AND   ( I~ZFRLST1   EQ   W_YN1
      OR      I~ZFRLST1   EQ   W_YN2 )
      AND   ( I~ZFRLST2   EQ   'N'
      OR      I~ZFRLST2   EQ   'C' )
*      AND     I~ZFRVDT    EQ   SPACE
      AND     H~ZFUSDAM   >=   ZTIMIMG00-ZFUSDAM.
*      AND   ( H~ZFREQTY   EQ   'LC'
*      OR      H~ZFREQTY   EQ   'GS' ).
  ENDIF.

* 은행재배정 여부 CHECK!
  IF P_BKCF IS INITIAL.
     DELETE IT_TAB WHERE ZFOPBN NE SPACE.
  ENDIF.

  DESCRIBE TABLE IT_TAB LINES W_COUNT.
  IF W_COUNT = 0. MESSAGE S738. W_ERR_CHK = 'Y'. ENDIF.
  CLEAR W_TABIX.

  LOOP AT IT_TAB.
     IF IT_TAB-ZFOPBN NE SPACE.
        W_TABIX  =  SY-TABIX.
        SELECT SINGLE NAME1
        INTO   IT_TAB-NAME1
        FROM   LFA1
        WHERE  LIFNR  =  IT_TAB-ZFOPBN.
        IF SY-SUBRC EQ 0.
           MODIFY IT_TAB INDEX W_TABIX.
        ENDIF.
     ENDIF.
  ENDLOOP.
  SORT IT_TAB BY EBELN ZFREQDT.

* 화면상의 INTERNAL TABLE에 MOVE.
  IT_ZSBKCF[]     =  IT_TAB[].
  IT_ZSBKCF_ORG[] =  IT_TAB[].
ENDFORM.                    " P2000_BKCF_READ
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR1100 OUTPUT.

  DESCRIBE TABLE IT_ZSBKCF LINES G_PARM_LINE.   " LINE 수 GET
  TC_1100-LINES = G_PARM_LINE.                     " LINE 수 정?

ENDMODULE.                 " TOTAL_LINE_GET_SCR1100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.
  SET SCREEN 0. LEAVE SCREEN.
ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_LINE_SCR1100 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_1100-CURRENT_LINE + LINE - 1.


ENDMODULE.                 " GET_LINE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.

   MOVE OK-CODE TO W_OK_CODE.
   CLEAR : OK-CODE.

ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  ZFOPBK_CHECK_SCR1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ZFOPBK_CHECK_SCR1100 INPUT.

  READ TABLE IT_ZSBKCF INDEX TC_1100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

*> 2001.07.09 ksb modify
*  MOVE-CORRESPONDING ZSBKCF TO IT_ZSBKCF.
  MOVE ZSBKCF-ZFOPBN         TO IT_ZSBKCF-ZFOPBN.

  CLEAR : IT_ZSBKCF-NAME1.
  SELECT SINGLE NAME1 INTO IT_ZSBKCF-NAME1 FROM LFA1
                      WHERE LIFNR EQ IT_ZSBKCF-ZFOPBN.

*> 2001.07.09 ksb modify
*  MOVE : IT_ZSBKCF-NAME1 TO  ZSBKCF-NAME1.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_ZSBKCF   INDEX W_TABIX.
*  ELSE.
*     APPEND  IT_ZSBKCF.
  ENDIF.

ENDMODULE.                 " ZFOPBK_CHECK_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TC_1100_UPDATE_SCR1100 INPUT.
*-----------------------------------------------------------------------
* 조회 MODE시 MODULE EXIT.
*-----------------------------------------------------------------------
  READ TABLE IT_ZSBKCF  INDEX TC_1100-CURRENT_LINE.
  W_SY_SUBRC = SY-SUBRC.
  W_TABIX    = SY-TABIX.

*  MOVE-CORRESPONDING ZSBKCF TO IT_ZSBKCF.
  MOVE      W_ROWMARK       TO IT_ZSBKCF-ZFMARK.

  IF W_SY_SUBRC EQ 0.
     MODIFY IT_ZSBKCF   INDEX W_TABIX.
*  ELSE.
*     APPEND  IT_ZSBKCF.
  ENDIF.

ENDMODULE.                 " TC_1100_UPDATE_SCR1100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_SCR1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
      WHEN 'SHLC'.
           CLEAR W_COUNT.
           LOOP AT IT_ZSBKCF WHERE ZFMARK = 'X'.
              ADD 1 TO W_COUNT.
           ENDLOOP.
           CASE W_COUNT.
              WHEN 1.
                 READ TABLE IT_ZSBKCF WITH KEY ZFMARK = 'X'.
                 PERFORM  P2000_LC_DOC_DISPLAY
                                       USING  IT_ZSBKCF-ZFREQNO ''.
              WHEN 0.
                 IF LINE GT 0.
                    READ TABLE IT_ZSBKCF INDEX LINE.
                    IF SY-SUBRC EQ 0.
                       PERFORM  P2000_LC_DOC_DISPLAY
                                       USING  IT_ZSBKCF-ZFREQNO ''.
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
           LOOP AT IT_ZSBKCF WHERE ZFMARK = 'X'.
              ADD 1 TO W_COUNT.
           ENDLOOP.
           CASE W_COUNT.
              WHEN 1.
                 READ TABLE IT_ZSBKCF WITH KEY ZFMARK = 'X'.
                 PERFORM  P2000_PO_DOC_DISPLAY
                                       USING  IT_ZSBKCF-EBELN.
              WHEN 0.
                 IF LINE GT 0.
                    READ TABLE IT_ZSBKCF INDEX LINE.
                    IF SY-SUBRC EQ 0.
                       PERFORM  P2000_PO_DOC_DISPLAY
                                       USING  IT_ZSBKCF-EBELN.
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
*&      Module  PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PF_STATUS_SCRCOM OUTPUT.

   SET PF-STATUS 'ZIM10'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIM10'.           " GUI TITLE SETTING..

ENDMODULE.                 " PF_STATUS_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_GUBUN  text
*----------------------------------------------------------------------*
FORM P2000_SET_MODIFY_CHECK.

  DESCRIBE TABLE IT_ZSBKCF        LINES W_COUNTER.
  DESCRIBE TABLE IT_ZSBKCF_ORG    LINES W_COUNTER1.
  W_LOOPLINES = 0.

  LOOP AT IT_ZSBKCF_ORG.
     READ TABLE IT_ZSBKCF WITH KEY ZFREQNO = IT_ZSBKCF_ORG-ZFREQNO.
       IF SY-SUBRC EQ 0.
          IF IT_ZSBKCF_ORG-ZFOPBN NE IT_ZSBKCF-ZFOPBN.
             W_LOOPLINES = 1. EXIT.
          ENDIF.
       ENDIF.
  ENDLOOP.

ENDFORM.                    " P2000_SET_MODIFY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SAVE_PROCESS.
* 데이타를 Insert
  LOOP AT IT_ZSBKCF.
     READ TABLE IT_ZSBKCF_ORG WITH KEY ZFREQNO = IT_ZSBKCF-ZFREQNO.
     IF SY-SUBRC EQ 0.
        IF IT_ZSBKCF-ZFOPBN NE IT_ZSBKCF_ORG-ZFOPBN.
           PERFORM P3000_LOCK_PROCESS USING IT_ZSBKCF-ZFREQNO.
           READ TABLE IT_LOCK WITH KEY ZFREQNO = IT_ZSBKCF-ZFREQNO.
           IF SY-SUBRC NE 0.
              MOVE IT_ZSBKCF-ZFREQNO TO IT_LOCK-ZFREQNO.
              MOVE IT_ZSBKCF-ZFOPBN  TO IT_LOCK-ZFOPBN.
              APPEND IT_LOCK.
           ENDIF.
        ENDIF.
     ENDIF.
  ENDLOOP.
* LOCK INTERNAL TABLE READ 하여서 실제 DB에 UPDATE.
  LOOP AT IT_LOCK.
    CLEAR : ZTREQHD, *ZTREQHD, ZTREQST, *ZTREQST.
    CALL FUNCTION 'ZIM_GET_REQ_DOC_HEADER'
       EXPORTING
            ZFREQNO         = IT_LOCK-ZFREQNO
            ZFAMDNO         = '00000'
       IMPORTING
            W_ZTREQHD       = ZTREQHD
            W_ZTREQST       = ZTREQST
       TABLES
            IT_ZTREQORJ     = IT_ZTREQORJ
            IT_ZTREQORJ_ORG = IT_ZTREQORJ_ORG
            IT_ZSREQIL      = IT_ZSREQIL
            IT_ZSREQIL_ORG  = IT_ZSREQIL_ORG
       EXCEPTIONS
            NOT_FOUND       = 4
            NOT_INPUT       = 8.

*>PO SELECT...
     SELECT SINGLE * FROM EKKO
                     WHERE EBELN EQ ZTREQHD-EBELN.

     CALL FUNCTION 'ZIM_GET_REQ_DOC_ITEM'
         EXPORTING
            EBELN            =   ZTREQHD-EBELN
            KNUMV            =   EKKO-KNUMV
*            KPOSN            =   '000000'
            KSCHL            =   ZTIMIMG00-ZFKSCHL3  ">Installing CHG.
            ZFREQNO          =   ZTREQHD-ZFREQNO
*         IMPORTING
*            W_ITEM_CNT      =   W_ITEM_CNT
*            W_TOT_AMOUNT    =   W_TOT_AMOUNT
         TABLES
            IT_ZSREQIT      =   IT_ZSREQIT
            IT_ZSREQIT_ORG  =   IT_ZSREQIT_ORG
         EXCEPTIONS
            NOT_FOUND      =   1
            NOT_INPUT      =   2
            NO_REFERENCE   =   3
            NO_AMOUNT      =   4.

    IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING ZTREQHD TO *ZTREQHD.
        MOVE-CORRESPONDING ZTREQST TO *ZTREQST.
*>> Bank code Move.
        MOVE IT_LOCK-ZFOPBN        TO  ZTREQHD-ZFOPBN.

        W_SUBRC = SY-SUBRC.

*>> DOCUMENT 개설은행. (2001/06/18 KSB INSERT)
       IF W_SUBRC EQ 0.
          CALL FUNCTION 'ZIM_GET_COMPANY_DATA'
               EXPORTING
                  BUKRS       =     ZTREQHD-BUKRS
                  IMTRD       =     ZTREQHD-IMTRD
               IMPORTING
*                  XZTIMIMGTX  =     ZTIMIMGTX
                  OZTIMIMGTX  =     ZTIMIMGTX
               EXCEPTIONS
                  NOT_FOUND   =     0.

          CASE ZTREQHD-ZFREQTY.
             WHEN 'LC'.
                CALL FUNCTION 'ZIM_GET_MASTER_LC_DATA'
                     EXPORTING
                        ZFREQNO           =       ZTREQHD-ZFREQNO
                     IMPORTING
                        W_ZTMLCHD         =       ZTMLCHD
                        W_ZTMLCSG2        =       ZTMLCSG2
                        W_ZTMLCSG910      =       ZTMLCSG910
                     TABLES
                        IT_ZSMLCSG7G      =       IT_ZSMLCSG7G
                        IT_ZSMLCSG7O      =       IT_ZSMLCSG7O
                        IT_ZSMLCSG8E      =       IT_ZSMLCSG8E
                        IT_ZSMLCSG9O      =       IT_ZSMLCSG9O
                        IT_ZSMLCSG7G_ORG  =       IT_ZSMLCSG7G_ORG
                        IT_ZSMLCSG7O_ORG  =       IT_ZSMLCSG7O_ORG
                        IT_ZSMLCSG8E_ORG  =       IT_ZSMLCSG8E_ORG
                        IT_ZSMLCSG9O_ORG  =       IT_ZSMLCSG9O_ORG
                     EXCEPTIONS
                        NOT_FOUND     =       4
                        NOT_INPUT     =       8.

                MOVE-CORRESPONDING  ZTMLCHD    TO   *ZTMLCHD.
                MOVE-CORRESPONDING  ZTMLCSG2   TO   *ZTMLCSG2.
                MOVE-CORRESPONDING  ZTMLCSG910 TO   *ZTMLCSG910.

                PERFORM  P2000_BANK_TEXT_MOVE.
                PERFORM  P2000_SET_SHIPING_TEXT.
                PERFORM  P2000_MASTER_LC_CHECK.

                CALL FUNCTION 'ZIM_MASTER_LC_MODIFY'
                     EXPORTING
                        W_OK_CODE        = 'SAVE'
                        ZFREQNO          = ZTREQHD-ZFREQNO
                        ZFAMDNO          = ZTREQST-ZFAMDNO
                        ZFSTATUS         = 'U'
                        W_ZTREQHD        = ZTREQHD
                        W_ZTREQHD_OLD    = *ZTREQHD
                        W_ZTREQST        = ZTREQST
                        W_ZTREQST_OLD    = *ZTREQST
                        W_ZTMLCHD        = ZTMLCHD
                        W_ZTMLCHD_OLD    = *ZTMLCHD
                        W_ZTMLCSG2       = ZTMLCSG2
                        W_ZTMLCSG2_OLD   = *ZTMLCSG2
                        W_ZTMLCSG910     = ZTMLCSG910
                        W_ZTMLCSG910_OLD = *ZTMLCSG910
                     TABLES
                        IT_ZSMLCSG7G     = IT_ZSMLCSG7G
                        IT_ZSMLCSG7O     = IT_ZSMLCSG7O
                        IT_ZSMLCSG8E     = IT_ZSMLCSG8E
                        IT_ZSMLCSG9O     = IT_ZSMLCSG9O
                        IT_ZSREQIT       = IT_ZSREQIT
                        IT_ZSREQIT_OLD   = IT_ZSREQIT_ORG
                        IT_ZTREQORJ      = IT_ZTREQORJ
                        IT_ZTREQORJ_OLD  = IT_ZTREQORJ_ORG
                        IT_ZSREQIL       = IT_ZSREQIL
                        IT_ZSREQIL_OLD   = IT_ZSREQIL_ORG
                     EXCEPTIONS
                        ERROR_UPDATE.


             WHEN 'LO'.
                CALL FUNCTION 'ZIM_GET_LOCAL_LC_DATA'
                     EXPORTING
                        ZFREQNO        = ZTREQHD-ZFREQNO
                     IMPORTING
                        W_ZTLLCHD      = ZTLLCHD
                        W_ZTLLCSG23    = ZTLLCSG23
                     TABLES
                        IT_ZSLLCOF     = IT_ZSLLCOF
                        IT_ZSLLCOF_ORG = IT_ZSLLCOF_ORG
                     EXCEPTIONS
                        NOT_FOUND      = 4
                        NOT_INPUT      = 8.

                CASE SY-SUBRC.
                   WHEN 4.
                      MESSAGE E018 WITH ZTREQHD-ZFREQNO.
                   WHEN 8.
                      MESSAGE E019.
                ENDCASE.
*----------------------------------------------------------------------
* 변경내역 확인을 위?
*----------------------------------------------------------------------
                *ZTLLCHD    = ZTLLCHD.
                *ZTLLCSG23  = ZTLLCSG23.

                PERFORM  P2000_BANK_TEXT_MOVE.
                PERFORM  P2000_LOCA_LC_CHECK.

                CALL FUNCTION 'ZIM_LOCAL_LC_MODIFY'
                     EXPORTING
                        W_OK_CODE       = 'SAVE'
                        ZFREQNO         = ZTREQHD-ZFREQNO
                        ZFAMDNO         = ZTREQST-ZFAMDNO
                        ZFSTATUS        = 'U'
                        W_ZTREQHD       = ZTREQHD
                        W_ZTREQHD_OLD   = *ZTREQHD
                        W_ZTREQST       = ZTREQST
                        W_ZTREQST_OLD   = *ZTREQST
                        W_ZTLLCHD       = ZTLLCHD
                        W_ZTLLCHD_OLD   = *ZTLLCHD
                        W_ZTLLCSG23     = ZTLLCSG23
                        W_ZTLLCSG23_OLD = *ZTLLCSG23
                     TABLES
                        IT_ZSLLCOF      = IT_ZSLLCOF
                        IT_ZSREQIT      = IT_ZSREQIT
                        IT_ZSREQIT_OLD  = IT_ZSREQIT_ORG
                        IT_ZTREQORJ     = IT_ZTREQORJ
                        IT_ZTREQORJ_OLD = IT_ZTREQORJ_ORG
                        IT_ZSREQIL      = IT_ZSREQIL
                        IT_ZSREQIL_OLD  = IT_ZSREQIL_ORG
                     EXCEPTIONS
                        ERROR_UPDATE.


             WHEN 'PU'.
                CALL FUNCTION 'ZIM_GET_PURCH_DOC_DATA'
                     EXPORTING
                        ZFREQNO          = ZTREQHD-ZFREQNO
                        ZFAMDNO          = ZTREQST-ZFAMDNO
                     IMPORTING
                        W_ZTPUR          = ZTPUR
                     TABLES
                        IT_ZSPURSG1      = IT_ZSPURSG1
                        IT_ZSPURSG1G     = IT_ZSPURSG1G
                        IT_ZSPURSG4      = IT_ZSPURSG4
                        IT_ZSPURSG1_ORG  = IT_ZSPURSG1_ORG
                        IT_ZSPURSG1G_ORG = IT_ZSPURSG1G_ORG
                        IT_ZSPURSG4_ORG  = IT_ZSPURSG4_ORG
                     EXCEPTIONS
                        NOT_FOUND        = 4
                        NOT_INPUT        = 8.

                CASE SY-SUBRC.
                   WHEN 4.
                      MESSAGE E054 WITH ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO.
                   WHEN 8.
                      MESSAGE E019.
                ENDCASE.
*----------------------------------------------------------------------
* 변경내역 확인을 위?
*----------------------------------------------------------------------
                *ZTPUR    = ZTPUR.

                PERFORM  P2000_BANK_TEXT_MOVE.
                PERFORM  P2000_APPPUR_CHECK.

                CALL FUNCTION 'ZIM_PURCH_DOC_MODIFY'
                     EXPORTING
                        W_OK_CODE       = 'SAVE'
                        ZFREQNO         = ZTREQHD-ZFREQNO
                        ZFAMDNO         = ZTREQST-ZFAMDNO
                        ZFSTATUS        = 'U'
                        W_ZTREQHD       = ZTREQHD
                        W_ZTREQHD_OLD   = *ZTREQHD
                        W_ZTREQST       = ZTREQST
                        W_ZTREQST_OLD   = *ZTREQST
                        W_ZTPUR         = ZTPUR
                        W_ZTPUR_OLD     = *ZTPUR
                     TABLES
                        IT_ZSPURSG1     = IT_ZSPURSG1
                        IT_ZSPURSG1G    = IT_ZSPURSG1G
                        IT_ZSPURSG4     = IT_ZSPURSG4
                        IT_ZSREQIT      = IT_ZSREQIT
                        IT_ZSREQIT_OLD  = IT_ZSREQIT_ORG
                        IT_ZTREQORJ     = IT_ZTREQORJ
                        IT_ZTREQORJ_OLD = IT_ZTREQORJ_ORG
                        IT_ZSREQIL      = IT_ZSREQIL
                        IT_ZSREQIL_OLD  = IT_ZSREQIL_ORG
                     EXCEPTIONS
                        ERROR_UPDATE.


*             WHEN 'TT'.
*                CALL FUNCTION 'ZIM_GET_PAYORD_DOC_DATA'
*                     EXPORTING
*                        ZFREQNO        = ZTREQHD-ZFREQNO
*                     IMPORTING
*                        W_ZTTTHD       = ZTTTHD
*                     TABLES
*                        IT_ZSTTSG5     = IT_ZSTTSG5
*                        IT_ZSTTSG5_ORG = IT_ZSTTSG5_ORG
*                     EXCEPTIONS
*                        NOT_FOUND      = 4
*                        NOT_INPUT      = 8.

               CASE SY-SUBRC.
                   WHEN 4.
                      MESSAGE E054 WITH ZTREQHD-ZFREQNO ZTREQST-ZFAMDNO.
                   WHEN 8.
                      MESSAGE E019.
                ENDCASE.

*-----------------------------------------------------------------------
* 변경내역 확인을 위?
*-----------------------------------------------------------------------
                *ZTTTHD   = ZTTTHD.
                PERFORM  P2000_BANK_TEXT_MOVE.
                PERFORM  P2000_PAYORD_CHECK.

                CALL FUNCTION 'ZIM_PAYORD_DOC_MODIFY'
                     EXPORTING
                        W_OK_CODE       = 'SAVE'
                        ZFREQNO         = ZTREQHD-ZFREQNO
                        ZFAMDNO         = ZTREQST-ZFAMDNO
                        ZFSTATUS        = 'U'
                        W_ZTREQHD       = ZTREQHD
                        W_ZTREQHD_OLD   = *ZTREQHD
                        W_ZTREQST       = ZTREQST
                        W_ZTREQST_OLD   = *ZTREQST
*                        W_ZTTTHD        = ZTTTHD
*                        W_ZTTTHD_OLD    = *ZTTTHD
                     TABLES
*                        IT_ZSTTSG5      = IT_ZSTTSG5
                        IT_ZSREQIT      = IT_ZSREQIT
                        IT_ZSREQIT_OLD  = IT_ZSREQIT_ORG
                        IT_ZTREQORJ     = IT_ZTREQORJ
                        IT_ZTREQORJ_OLD = IT_ZTREQORJ_ORG
                        IT_ZSREQIL      = IT_ZSREQIL
                        IT_ZSREQIL_OLD  = IT_ZSREQIL_ORG
                     EXCEPTIONS
                        ERROR_UPDATE.

             WHEN OTHERS.
                PERFORM  P2000_BANK_TEXT_MOVE.

                CALL FUNCTION 'ZIM_OTHER_DOC_MODIFY'
                     EXPORTING
                        W_OK_CODE       = 'SAVE'
                        ZFREQNO         = ZTREQHD-ZFREQNO
                        ZFAMDNO         = ZTREQST-ZFAMDNO
                        ZFSTATUS        = 'U'
                        W_ZTREQHD       = ZTREQHD
                        W_ZTREQHD_OLD   = *ZTREQHD
                        W_ZTREQST       = ZTREQST
                        W_ZTREQST_OLD   = *ZTREQST
                     TABLES
                        IT_ZSREQIT      = IT_ZSREQIT
                        IT_ZSREQIT_OLD  = IT_ZSREQIT_ORG
                        IT_ZSREQIL      = IT_ZSREQIL
                        IT_ZSREQIL_OLD  = IT_ZSREQIL_ORG
                        IT_ZTREQORJ     = IT_ZTREQORJ
                        IT_ZTREQORJ_OLD = IT_ZTREQORJ_ORG
                     EXCEPTIONS
                        ERROR_UPDATE.
          ENDCASE.
       ENDIF.

       IF W_SUBRC EQ 0.
          PERFORM P3000_UNLOCK_PROCESS  USING IT_LOCK-ZFREQNO.
       ENDIF.
    ENDIF.
  ENDLOOP.

  IT_ZSBKCF_ORG[] = IT_ZSBKCF[].

ENDFORM.                    " P2000_SAVE_PROCESS
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
*       text
*----------------------------------------------------------------------*
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
*&      Form  P2000_EXIT_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_EXIT_MESSAGE.

  IF SY-LANGU EQ 'KO'.
     PERFORM P2000_MESSAGE_BOX USING '종료 확인'
                          '현재 입력내역을 저장하지 않습니다.'
                          '저장 후 종료하시겠습니까?'
                          'Y'
                          '1'.
  ELSE.
     PERFORM P2000_MESSAGE_BOX USING 'End Confirmation'
                          'Do not save the entering data.'
                          'Do you want to end after save?'
                          'Y'
                          '1'.
  ENDIF.

ENDFORM.                    " P2000_EXIT_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0797   text
*      -->P_0798   text
*      -->P_0799   text
*      -->P_0800   text
*      -->P_0801   text
*----------------------------------------------------------------------*
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
*&      Form  P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CANCEL_MESSAGE.

  IF SY-LANGU EQ 'KO'.
     PERFORM P2000_MESSAGE_BOX USING '취소 확인'
                                  '변경된 내용을 저장없이 종료됩니다.'
                                  '종료하시겠습니까?'
                                  'N'
                                  '2'.
  ELSE.
     PERFORM P2000_MESSAGE_BOX USING 'Cancel Confirmation'
                         'Do end without save the changed contents.'
                         'Do you want to end?'
                         'N'
                         '2'.
  ENDIF.

  CASE ANTWORT.
    WHEN 'Y'.                                               " Yes...
      MESSAGE  S957.
      LEAVE TO SCREEN 0.  " " PROGRAM LEAVING
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_CANCEL_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SAVE_MESSAGE.
  IF SY-LANGU EQ 'KO'.
     PERFORM P2000_MESSAGE_BOX USING '저장 확인'
                                  '입력된 내역을 저장합니다.'
                                  '저장하시겠습니까?'
                                  'Y'
                                  '1'.
  ELSE.
     PERFORM P2000_MESSAGE_BOX USING 'Save Cnofirmation'
                          'Do save the entered detail.'
                          'Do you want to save?'
                          'Y'
                          '1'.
  ENDIF.
ENDFORM.                    " P2000_SAVE_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSBKCF_ZFREQNO  text
*      -->P_WHEN  text
*      -->P_OTHERS  text
*----------------------------------------------------------------------*
FORM P2000_LC_DOC_DISPLAY USING    P_ZFREQNO
                                   P_ZFOPNNO.

  IF P_ZFREQNO IS INITIAL AND  P_ZFOPNNO IS INITIAL.
     MESSAGE E063.
  ENDIF.

  SET PARAMETER ID 'BES' FIELD ''.
  EXPORT 'BES'  TO MEMORY ID 'BES'.
  SET PARAMETER ID 'ZPREQNO' FIELD P_ZFREQNO.
  EXPORT 'ZPREQNO'  TO MEMORY ID 'ZPREQNO'.
  SET PARAMETER ID 'ZPOPNNO' FIELD P_ZFOPNNO.
  EXPORT 'ZPOPNNO'  TO MEMORY ID 'ZPOPNNO'.

  CALL TRANSACTION 'ZIM03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " P2000_LC_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSBKCF_EBELN  text
*----------------------------------------------------------------------*
FORM P2000_PO_DOC_DISPLAY USING    P_EBELN.

  SELECT SINGLE * FROM EKKO
         WHERE EBELN EQ P_EBELN.

  CASE EKKO-BSTYP.
     WHEN 'L'.
        SET PARAMETER ID 'SAG' FIELD P_EBELN.
        CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.
     WHEN 'K'.
        SET PARAMETER ID 'CTR' FIELD P_EBELN.
        CALL TRANSACTION 'ME33K' AND SKIP  FIRST SCREEN.
     WHEN OTHERS.
        SET PARAMETER ID 'BES' FIELD P_EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

   ENDCASE.
ENDFORM.                    " P2000_PO_DOC_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  P3000_LOCK_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LOCK_PROCESS USING P_ZFREQNO.

  CALL FUNCTION 'ENQUEUE_EZ_IM_ZTREQDOC'
       EXPORTING
                ZFREQNO  =  P_ZFREQNO
                ZFAMDNO  =  '00000'
       EXCEPTIONS
                OTHERS   =  1.

    IF SY-SUBRC <> 0.
      MESSAGE E510 WITH SY-MSGV1 'Import Document'
                                 ZTREQHD-ZFREQNO P_ZFREQNO
                                 RAISING DOCUMENT_LOCKED.

    ENDIF.

ENDFORM.                    " P3000_LOCK_PROCESS

*&---------------------------------------------------------------------*
*&      Form  P3000_UNLOCK_PROCESS
*&---------------------------------------------------------------------*
FORM P3000_UNLOCK_PROCESS USING P_ZFREQNO.

  CALL FUNCTION 'DEQUEUE_EZ_IM_ZTREQDOC'
       EXPORTING
                ZFREQNO = P_ZFREQNO
                ZFAMDNO = '00000'.

ENDFORM.                    " P3000_UNLOCK_PROCESS

*&---------------------------------------------------------------------*
*&      Form  P2000_BANK_TEXT_MOVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BANK_TEXT_MOVE.

  DATA : L_NAME1(255),
         L_NAME2(255),
         L_NAME3(255),
         L_NAME4(255).

  CLEAR : W_LFA1, W_ADRC.

  IF NOT ZTREQHD-ZFOPBN IS INITIAL.
     CALL FUNCTION 'ZIM_GET_VENDOR_ADDRESS_FORMAT'
          EXPORTING
               LIFNR     = ZTREQHD-ZFOPBN
          IMPORTING
               NAME1     = L_NAME1
               NAME2     = L_NAME2
               NAME3     = L_NAME3
               NAME4     = L_NAME4
               P_LFA1    = W_LFA1
               P_ADRC    = W_ADRC
          EXCEPTIONS
               NO_INPUT  = 01
               NOT_FOUND = 03.
     CASE SY-SUBRC.
        WHEN 01.     MESSAGE I025.
        WHEN 03.     MESSAGE I020   WITH   ZTREQHD-ZFOPBN.
     ENDCASE.
   ENDIF.

  CASE ZTREQHD-ZFREQTY.
    WHEN 'LC'.
      MOVE : ZTREQHD-ZFOPBN TO ZTMLCHD-ZFOPBN. " 개설은행 거래처코?
*      MOVE : W_LFA1-NAME4    TO   ZTMLCHD-ZFOBNM,     " 은행명..
      MOVE : W_LFA1-NAME1    TO   ZTMLCHD-ZFOBNM,     " 은행명..
*            W_LFA1-NAME2    TO   ZTMLCHD-ZFOBBR,     " 지점?
             SPACE           TO   ZTMLCHD-ZFOBBR,     " 지점?
             W_LFA1-TELF1    TO   ZTMLCHD-ZFOBPH,     " 전화번?
*            W_LFA1-BAHNS    TO   ZTMLCHD-ZFOPBNCD." 수발신식별자.
             W_LFA1-KRAUS    TO   ZTMLCHD-ZFOPBNCD." 한국은행부여코드.
    WHEN 'LO'.
      MOVE : ZTREQHD-ZFOPBN TO ZTLLCHD-ZFOPBN. " 개설은행 거래처코?

      MOVE : W_LFA1-NAME1    TO   ZTLLCHD-ZFOBNM,     " 은행?
*             W_LFA1-NAME2    TO   ZTLLCHD-ZFOBBR,     " 지점?
              SPACE           TO   ZTLLCHD-ZFOBBR,     " 지점?
*             W_LFA1-KRAUS    TO   ZTLLCHD-ZFOPBNCD. " 한국은행부여코드.
              W_LFA1-BAHNS    TO   ZTLLCHD-ZFOPBNCD. " 수발신식별자.
    WHEN 'PU'.
      MOVE : ZTREQHD-ZFOPBN TO ZTPUR-ZFACBN.   " 개설은행 거래처코?
      MOVE : W_LFA1-NAME1    TO   ZTPUR-ZFACNM,       " 은행명.
*            W_LFA1-NAME2    TO   ZTPUR-ZFACBR,       " 지점명.
             SPACE           TO   ZTPUR-ZFACBR,       " 지점전화번호.
*            W_LFA1-BAHNS    TO   ZTPUR-ZFACBNCD.  " 수발신식별자.
             W_LFA1-KRAUS    TO   ZTPUR-ZFACBNCD.  " 한국은행부여코드.
    WHEN 'TT'.
      MOVE : ZTREQHD-ZFOPBN TO ZTTTHD-ZFOPBN. " 개설은행 거래처코?
      MOVE : W_LFA1-NAME1    TO   ZTTTHD-ZFOBNM,      " 은행명.
*            W_LFA1-NAME2    TO   ZTTTHD-ZFOBBR,      " 지점명.
             SPACE           TO   ZTTTHD-ZFOBBR,      " 지점명.
             W_LFA1-BAHNS    TO   ZTTTHD-ZFOPBNCD. " 수발신식별자.
*            W_LFA1-KRAUS    TO   ZTTTHD-ZFOPBNCD. " 한국은행부여코드.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_BANK_TEXT_MOVE
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_SHIPING_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_SET_SHIPING_TEXT.

    CASE ZTREQHD-ZFTRANS.
      WHEN 'A'.                                             " AIR
        ZTMLCSG910-ZFAIRYN = 'X'.
        CASE ZTREQHD-INCO1.
          WHEN 'EXW' OR 'FAS' OR 'FCA' OR 'FOB'.
            ZTMLCSG910-ZFAIRAC = '32'.
          WHEN OTHERS.
            ZTMLCSG910-ZFAIRAC = '31'.
        ENDCASE.
        ZTMLCSG910-ZFAIRAN = ZTIMIMGTX-ZFAPPNM.
        IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
          IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
            ZTMLCSG910-ZFAIRC1 = W_LFA1-NAME1.
            ZTMLCSG910-ZFAIRC2 = ''.
          ELSE.
            ZTMLCSG910-ZFAIRC1 = ZTMLCSG910-ZFOCEC1.
            ZTMLCSG910-ZFAIRC2 = ZTMLCSG910-ZFOCEC2.
          ENDIF.
        ENDIF.
        ZTMLCSG910-ZFOCEYN = ''.
        ZTMLCSG910-ZFOCEC1 = ''.
        ZTMLCSG910-ZFOCEC2 = ''.
        ZTMLCSG910-ZFOCEAC = ''.
        ZTMLCSG910-ZFOCEAN = ''.
      WHEN 'O'.                                             " OCEAN
        ZTMLCSG910-ZFOCEYN = 'X'.
*-----------------------------------------------------------------------
* 2000/04/20 강나형 대리 DEFINE
*-----------------------------------------------------------------------
        CASE ZTREQHD-INCO1.
          WHEN 'EXW' OR 'FAS' OR 'FCA' OR 'FOB'.
            ZTMLCSG910-ZFOCEAC = '32'.
          WHEN OTHERS.
            ZTMLCSG910-ZFOCEAC = '31'.
        ENDCASE.
        ZTMLCSG910-ZFOCEAN = ZTIMIMGTX-ZFAPPNM.
        IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
          IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
            ZTMLCSG910-ZFOCEC1 = W_LFA1-NAME1.
            ZTMLCSG910-ZFOCEC2 = ''.
          ELSE.
            ZTMLCSG910-ZFOCEC1 = ZTMLCSG910-ZFAIRC1.
            ZTMLCSG910-ZFOCEC2 = ZTMLCSG910-ZFAIRC2.
          ENDIF.
        ENDIF.
        ZTMLCSG910-ZFAIRYN = ''.
        ZTMLCSG910-ZFAIRC1 = ''.
        ZTMLCSG910-ZFAIRC2 = ''.
        ZTMLCSG910-ZFAIRAC = ''.
        ZTMLCSG910-ZFAIRAN = ''.
      WHEN 'B'.      " AIR + OCEAN
*-----------------------------------------------------------------------
* 2000/04/20 강나형 대리 DEFINE
*-----------------------------------------------------------------------
        CASE ZTREQHD-INCO1.
          WHEN 'EXW' OR 'FAS' OR 'FCA' OR 'FOB'.
            ZTMLCSG910-ZFOCEAC = '32'.
            ZTMLCSG910-ZFAIRAC = '32'.
          WHEN OTHERS.
            ZTMLCSG910-ZFOCEAC = '31'.
            ZTMLCSG910-ZFAIRAC = '31'.
        ENDCASE.
        ZTMLCSG910-ZFOCEYN = 'X'.
        ZTMLCSG910-ZFOCEAN = ZTIMIMGTX-ZFAPPNM.
        ZTMLCSG910-ZFAIRYN = 'X'.
        ZTMLCSG910-ZFAIRAN = ZTIMIMGTX-ZFAPPNM.
        IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
          ZTMLCSG910-ZFAIRC1 = W_LFA1-NAME4.
        ENDIF.
        IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
          ZTMLCSG910-ZFOCEC1 = W_LFA1-NAME4.
        ENDIF.
      WHEN OTHERS.
        CLEAR : ZTMLCSG910-ZFOCEYN, ZTMLCSG910-ZFOCEAC,
                ZTMLCSG910-ZFOCEAN, ZTMLCSG910-ZFAIRYN,
                ZTMLCSG910-ZFAIRAC, ZTMLCSG910-ZFAIRAN,
                ZTMLCSG910-ZFAIRC1, ZTMLCSG910-ZFOCEC1,
                ZTMLCSG910-ZFAIRC2, ZTMLCSG910-ZFOCEC2.
    ENDCASE.

ENDFORM.                    " P2000_SET_SHIPING_TEXT
*&---------------------------------------------------------------------*
*&      Form  P2000_MASTER_LC_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MASTER_LC_CHECK.
  ZTREQST-ZFEDICK = 'O'.
* 개설방법.
  IF ZTMLCHD-ZFOPME IS INITIAL.
    ZTREQST-ZFEDICK = 'X'. EXIT.
  ENDIF.
* 수수료 부담자.
  IF ZTMLCHD-ZFCHG  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'. EXIT.
  ENDIF.
* 개설신청일.
  IF ZTREQST-ZFAPPDT IS INITIAL.
    ZTREQST-ZFEDICK = 'X'. EXIT.
  ENDIF.
* 개설은행 코드.
  IF ZTREQHD-ZFOPBN  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'. EXIT.
  ENDIF.
* 개설은행명.
  IF ZTMLCHD-ZFOBNM  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'. EXIT.
  ELSE.
    ASSIGN ZTMLCHD-ZFOBNM       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCHD-ZFOBBR       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* EDI 식별자.
  IF W_LFA1-BAHNS IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 은행점포코드..
* IF W_LFA1-KRAUS IS INITIAL.
  IF ZTMLCHD-ZFOPBNCD IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ENDIF.

* 개설의뢰인.
  IF ZTMLCSG2-ZFAPPNM IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ELSE.
    ASSIGN ZTMLCSG2-ZFAPPNM     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFAPPAD1    TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFAPPAD2    TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFAPPAD3    TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFTELNO     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* 수익자  코드.
  IF ZTMLCHD-ZFBENI  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 수익자  명.
  IF ZTMLCSG2-ZFBENI1 IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ELSE.
    ASSIGN ZTMLCSG2-ZFBENI1       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFBENI2     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFBENI3     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFBENI4     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTMLCSG2-ZFBENIA     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* 전자서명.
  IF ZTMLCSG2-ZFELENM IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
  IF ZTMLCSG2-ZFELEID IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
  ASSIGN ZTMLCSG2-ZFELENM     TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG2-ZFREPRE     TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG2-ZFELEID     TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG2-ZFELEAD1    TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG2-ZFELEAD2    TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 유효기간.
  IF ZTMLCHD-ZFEXDT  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 유효기일 장소.
  IF ZTMLCHD-ZFEXPL  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
  ASSIGN ZTMLCHD-ZFEXPL       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 부가조건.
  ASSIGN ZTMLCHD-ZFAAMT1        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCHD-ZFAAMT2        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCHD-ZFAAMT3        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCHD-ZFAAMT4        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 선적항.
  IF ZTMLCHD-ZFSPRT  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ENDIF.
  ASSIGN ZTMLCHD-ZFSPRT       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 도착항.
  IF ZTMLCHD-ZFAPRT  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTMLCHD-ZFAPRT       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 최종선적일자.
  IF ZTMLCHD-ZFLTSD  IS INITIAL.
    IF ZTMLCHD-ZFSHPR1 IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.    EXIT.
    ENDIF.
  ENDIF.
  ASSIGN ZTMLCHD-ZFSHPR1      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCHD-ZFSHPR2      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCHD-ZFSHPR3      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 가격조건.
  IF ZTMLCHD-INCO1   IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
  ASSIGN ZTMLCHD-ZFINCP       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 주요 구비서류.
  IF    ( ZTMLCSG910-ZFCOMYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFOCEYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFAIRYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFINYN    IS INITIAL ) AND
        ( ZTMLCSG910-ZFPACYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFCEOYN   IS INITIAL ) AND
        ( ZTMLCSG910-ZFOTDYN   IS INITIAL ).
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 상업송장.
  IF ZTMLCSG910-ZFCOMYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFNOCOM IS INITIAL ).  " 미체크시 자동 체?
       ZTMLCSG910-ZFCOMYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFNOCOM IS INITIAL.
       ZTREQST-ZFEDICK = 'X'.      EXIT.
    ENDIF.
  ENDIF.
* ocean bill
  IF ZTMLCSG910-ZFOCEYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFOCEC1 IS INITIAL )
       AND NOT ( ZTMLCSG910-ZFOCEAC IS INITIAL )
       AND NOT ( ZTMLCSG910-ZFOCEAN IS INITIAL ).
       ZTMLCSG910-ZFOCEYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFOCEC1 IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.   EXIT.
    ENDIF.
    PERFORM P2000_PAY_YN_CHECK(SAPMZIM00)
                               USING ZTMLCSG910-ZFOCEAC. "운임지불여?
    CHECK : ZTREQST-ZFEDICK    NE 'X'.
    IF ZTMLCSG910-ZFOCEAN IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.      EXIT.
    ENDIF.
  ENDIF.
  ASSIGN ZTMLCSG910-ZFOCEC1   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG910-ZFOCEC2   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG910-ZFOCEAN   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* AIR BILL
  IF ZTMLCSG910-ZFAIRYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFAIRC1 IS INITIAL )
       AND NOT ( ZTMLCSG910-ZFAIRAC IS INITIAL )
       AND NOT ( ZTMLCSG910-ZFAIRAN IS INITIAL ).
      ZTMLCSG910-ZFAIRYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFAIRC1 IS INITIAL.
       ZTREQST-ZFEDICK = 'X'.   EXIT.
    ENDIF.
    PERFORM P2000_PAY_YN_CHECK(SAPMZIM00)
                               USING ZTMLCSG910-ZFAIRAC. "운임지불여?
    CHECK : ZTREQST-ZFEDICK NE 'X'.
    IF ZTMLCSG910-ZFAIRAN IS INITIAL.
       ZTREQST-ZFEDICK = 'X'.   EXIT.
    ENDIF.
  ENDIF.
  ASSIGN ZTMLCSG910-ZFAIRC1   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG910-ZFAIRC2   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG910-ZFAIRAN   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 보험 부건.
  IF ZTMLCSG910-ZFINYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFINCO1 IS INITIAL ).
       ZTMLCSG910-ZFINYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFINCO1 IS INITIAL.
       ZTREQST-ZFEDICK = 'X'.  EXIT.
    ENDIF.
  ENDIF.
  CHECK : ZTREQST-ZFEDICK NE 'X'.
*----------------------------------------------------------------------
  ASSIGN ZTMLCSG910-ZFINCO1   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTMLCSG910-ZFINCO2   TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* PACKING LIST
  IF ZTMLCSG910-ZFPACYN IS INITIAL.
    IF NOT ( ZTMLCSG910-ZFNOPAC IS INITIAL ).
       ZTMLCSG910-ZFPACYN = 'X'.
    ENDIF.
  ELSE.
    IF ZTMLCSG910-ZFNOPAC IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.  EXIT.
    ENDIF.
  ENDIF.
* 기타 구비서류.
  DESCRIBE TABLE IT_ZSMLCSG8E LINES G_PARAM_LINE.   " 기타 구비서?
  IF ZTMLCSG910-ZFOTDYN EQ 'X'.
    IF G_PARAM_LINE EQ 0.
       ZTREQST-ZFEDICK = 'X'.  EXIT.
    ENDIF.
  ELSE.
    IF G_PARAM_LINE > 0.
       ZTMLCSG910-ZFOTDYN = 'X'.
    ENDIF.
  ENDIF.
  LOOP AT IT_ZSMLCSG8E.
    ASSIGN IT_ZSMLCSG8E-ZFOACD1 TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDLOOP.
* SHIPMENT BY
  IF ZTMLCHD-ZFADCD1 IS INITIAL.
    IF NOT ( ZTMLCHD-ZFCARR IS INITIAL ).
       ZTMLCHD-ZFADCD1 = 'X'.   EXIT.
    ENDIF.
  ELSE.
    IF ZTMLCHD-ZFCARR IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.  EXIT.
    ENDIF.
  ENDIF.
  ASSIGN ZTMLCHD-ZFCARR       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

* 기타 구비서류.
  DESCRIBE TABLE IT_ZSMLCSG9O LINES G_PARAM_LINE.   " 기타 구비서류.
  IF ZTMLCHD-ZFADCD5 EQ 'X'.
    IF G_PARAM_LINE EQ 0.
       ZTREQST-ZFEDICK = 'X'.  EXIT.
    ENDIF.
  ELSE.
    IF G_PARAM_LINE > 0.
       ZTMLCHD-ZFADCD5 = 'X'.
    ENDIF.
  ENDIF.
  LOOP AT IT_ZSMLCSG9O.
    ASSIGN IT_ZSMLCSG9O-ZFODOC1 TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDLOOP.

* L/C 자재 명세 ...
  LOOP AT IT_ZSMLCSG7G WHERE LOEKZ NE 'X'.
    ASSIGN IT_ZSMLCSG7G-ZFDSOG1 TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_MASTER_LC_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_TEXT_FIELD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_TEXT_FIELD_CHECK.

  PERFORM P2000_SPACE_CUT(SAPMZIM00) USING <FS_F>.

ENDFORM.                    " P2000_TEXT_FIELD_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_LOCA_LC_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_LOCA_LC_CHECK.

  ZTREQST-ZFEDICK = 'O'.

* 개설신청?
  IF ZTREQST-ZFAPPDT IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 개설은행 코?
  IF ZTREQHD-ZFOPBN  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.

* 개설은행?
  IF ZTLLCHD-ZFOBNM  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ELSE.
    ASSIGN ZTLLCHD-ZFOBNM       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCHD-ZFOBBR       TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.

* EDI 식별?
  IF W_LFA1-BAHNS IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 은행점포코?
  IF W_LFA1-KRAUS IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.

* 개설의뢰?
  IF ZTLLCSG23-ZFAPPNM1 IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ELSE.
    ASSIGN ZTLLCSG23-ZFAPPNM1     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCSG23-ZFAPPNM2     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCSG23-ZFAPPNM3     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* 수혜?
  IF ZTLLCSG23-ZFBENI1  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ELSE.
    ASSIGN ZTLLCSG23-ZFBENI1      TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCSG23-ZFBENI2      TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCSG23-ZFBENI3      TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* 내국신용장 종?
  IF ZTLLCHD-ZFLLCTY    IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT
.
  ENDIF.
* 개설근거별 용?
  IF ZTLLCHD-ZFUSG      IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT
.
  ENDIF.
* 매도확약서 번?
  LOOP AT IT_ZSLLCOF.
    IF IT_ZSLLCOF-ZFOFFER    IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.   EXIT.
    ENDIF.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ENDIF.
* 선적서류 제시기?
  IF ZTLLCHD-ZFDPRP     IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ENDIF.
* 물품인도기?
  IF ZTLLCHD-ZFGDDT     IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ENDIF.
* 유효기?
  IF ZTLLCHD-ZFEXDT      IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ENDIF.
* 대표 공급물품?
  IF ZTLLCHD-ZFGDSC1     IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.  EXIT.
  ELSE.
    ASSIGN ZTLLCHD-ZFGDSC1        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCHD-ZFGDSC2        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCHD-ZFGDSC3        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCHD-ZFGDSC4        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTLLCHD-ZFGDSC5        TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* 주요구비서?

  IF ZTLLCSG23-ZFNOBIL IS INITIAL AND ZTLLCSG23-ZFNOINV IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 기타 정?
  ASSIGN ZTLLCHD-ZFETC1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFETC2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFETC3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFETC4         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFETC5         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 신용장 계약서 번?
  ASSIGN ZTLLCHD-ZFDCNO         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 수출 상대?
  ASSIGN ZTLLCHD-ZFEXPR1        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFEXPR2        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFEXPR3        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 전자서?
  IF ZTLLCSG23-ZFELENM IS INITIAL AND
     ZTLLCSG23-ZFELEID IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTLLCSG23-ZFELENM      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCSG23-ZFREPRE      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCSG23-ZFELEID      TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 발행은?
  ASSIGN ZTLLCHD-ZFISBN         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFISBNB        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 대표수출물품?
  ASSIGN ZTLLCHD-ZFEXGNM1       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFEXGNM2       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFEXGNM3       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFEXGNM4       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTLLCHD-ZFEXGNM5       TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

ENDFORM.                    " P2000_LOCA_LC_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_APPPUR_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_APPPUR_CHECK.

  DESCRIBE TABLE IT_ZSPURSG1 LINES W_COUNT.
  IF W_COUNT LE 0.
    ZTREQST-ZFEDICK = 'X'.      EXIT.
  ENDIF.

  ZTREQST-ZFEDICK = 'O'.

* 승인은?
  IF ZTPUR-ZFACBN    IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 개설은행?
  IF ZTPUR-ZFACNM  IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ELSE.
    ASSIGN ZTPUR-ZFACNM         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFACBR         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* EDI 식별?
  SELECT SINGLE * FROM LFA1 WHERE LIFNR EQ ZTPUR-ZFACBN.
  IF LFA1-BAHNS IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 은행점포코?
  IF LFA1-KRAUS IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.

* 신청?
  IF ZTPUR-ZFAPPNM1     IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ELSE.
    ASSIGN ZTPUR-ZFAPPNM1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFAPPNM2         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFAPPNM3         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFAPPAD1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFAPPAD2         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFAPPAD3         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* 공급?
  IF ZTPUR-ZFVENNM1     IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ELSE.
*     IF ZTPUR-ZFVENID  IS INITIAL.
*        ZTREQST-ZFEDICK = 'X'. MESSAGE I167 WITH 'Supplier ID'.   EXIT.
*     ENDIF.
    ASSIGN ZTPUR-ZFVENNM1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFVENNM2         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFVENID          TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFVENAD1         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFVENAD2         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
    ASSIGN ZTPUR-ZFVENAD3         TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDIF.
* 공급물품구?
  IF ZTPUR-ZFGDCD       IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.    EXIT.
  ENDIF.
* 물품명?
  LOOP AT IT_ZSPURSG1.
    ASSIGN IT_ZSPURSG1-ZFHSDESC   TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDLOOP.
* 근거서?
  LOOP AT IT_ZSPURSG4.
    IF IT_ZSPURSG4-ZFSDOC       IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.    EXIT
.
    ENDIF.
    IF IT_ZSPURSG4-ZFSDNO       IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.    EXIT.
    ENDIF.
    IF SY-TABIX EQ 1.
      IF IT_ZSPURSG4-WAERS        IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.   EXIT.
      ENDIF.
      IF IT_ZSPURSG4-ZFGOAMT IS INITIAL.
        ZTREQST-ZFEDICK = 'X'.   EXIT.
      ENDIF.
    ENDIF.
    ASSIGN IT_ZSPURSG4-ZFSDNO     TO <FS_F>.
    PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 전자서?
  IF ZTPUR-ZFELEAD1    IS INITIAL AND
     ZTPUR-ZFELEID     IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTPUR-ZFELEAD1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTPUR-ZFELEAD2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTPUR-ZFELEID          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

ENDFORM.                    " P2000_APPPUR_CHECK

*&---------------------------------------------------------------------*
*&      Form  P2000_PAYORD_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_PAYORD_CHECK.
  ZTREQST-ZFEDICK = 'O'.
* 입금관련 서류번?
  DESCRIBE TABLE IT_ZSTTSG5 LINES W_COUNT.
  IF W_COUNT LE 0.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 신청일?
  IF ZTREQST-ZFAPPDT IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 지급지서서 용?
  IF ZTTTHD-ZFBUSFUN IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ELSE.
    IF ZTTTHD-ZFBUSFUN EQ '2AJ' AND ZTTTHD-ZFCOMMTY IS INITIAL.
      ZTREQST-ZFEDICK = 'X'.   EXIT.
    ELSEIF ZTTTHD-ZFBUSFUN NE '2AJ' AND NOT ZTTTHD-ZFCOMMTY IS INITIAL.
      ZTREQST-ZFEDICK = 'X'. EXIT.
    ENDIF.
  ENDIF.
* 부가수수료 담당?
  IF ZTTTHD-ZFCFRG IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
* 지급의뢰?
  IF ZTTTHD-ZFAPPNM IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFAPPNM         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFAPPAD1        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFAPPAD2        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFAPPAD3        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFTELNO         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 수익?
  IF ZTTTHD-ZFBENI1 IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFBENI1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBENI2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBENI3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 지급의뢰인 은?
  IF ZTTTHD-ZFOBNM IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  IF ZTTTHD-ZFOBAK IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFOBNM          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOBAK          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOBBR          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOPBNCD        TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

* 수익자 은?
  IF ZTTTHD-ZFBENM IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  IF ZTTTHD-ZFOBAK1 IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFOBNM          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFOBAK1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBEBR          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFBENCD         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 전자서?
  IF ZTTTHD-ZFELENM IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  IF ZTTTHD-ZFELEID IS INITIAL.
    ZTREQST-ZFEDICK = 'X'.   EXIT.
  ENDIF.
  ASSIGN ZTTTHD-ZFELENM         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFREPRE         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFELEID         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 송금애?
  ASSIGN ZTTTHD-ZFSEND1         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND2         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND3         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND4         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFSEND5         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 기타정?
  ASSIGN ZTTTHD-ZFETC1          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC2          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC3          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC4          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
  ASSIGN ZTTTHD-ZFETC5          TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.
* 기타 번?
  ASSIGN ZTTTHD-ZFETCNO         TO <FS_F>.
  PERFORM P2000_TEXT_FIELD_CHECK.   CHECK : ZTREQST-ZFEDICK NE 'X'.

ENDFORM.                    " P2000_PAYORD_CHECK
*&---------------------------------------------------------------------*
*&      Module  HELP_ZTERM_SCR1100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE HELP_ZTERM_SCR1100 INPUT.

  LOOP AT SCREEN.
    IF SCREEN-NAME EQ 'ZSBKCF-ZTERM'.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF SCREEN-INPUT EQ '1'.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSBKCF-ZTERM
              I_XSHOW       = ' '
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ELSE.
    CALL FUNCTION 'FI_F4_ZTERM'
         EXPORTING
              I_KOART       = 'K'
              I_ZTERM       = ZSBKCF-ZTERM
              I_XSHOW       = 'X'
         IMPORTING
              E_ZTERM       = T052-ZTERM
         EXCEPTIONS
              NOTHING_FOUND = 01.
  ENDIF.

  IF SY-SUBRC NE 0.
*   message e177 with ekko-zterm.
    MESSAGE S177(06) WITH ZSBKCF-ZTERM.
    EXIT.
  ENDIF.

  IF T052-ZTERM NE SPACE.
     ZSBKCF-ZTERM = T052-ZTERM.
  ENDIF.

ENDMODULE.                 " HELP_ZTERM_SCR1100  INPUT
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
