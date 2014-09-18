*&---------------------------------------------------------------------*
*& Report  ZRIMLCPMTLST                                                *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입OPEN 실적(종류별)                                 *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.09.28                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMPMTLST   MESSAGE-ID ZIM
                     LINE-SIZE 148
                     NO STANDARD PAGE HEADING.
TABLES: ZTREQHD,ZTREQST,ZTIMIMG00.
*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_REQHD OCCURS 0,
       ZFREQTY     LIKE ZTREQHD-ZFREQTY,         " 결제조건.
       ZFBACD      LIKE ZTREQHD-ZFBACD,          " 사전사후.
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       MTZFOPAMT   LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       DPZFOPAMT   LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       DAZFOPAMT   LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       TTBZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사전개설금액.
       TTAZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사후개설금액.
       WAERS       LIKE ZTREQST-WAERS,
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       ZFUSD       LIKE ZTREQST-ZFUSD.
DATA : END OF IT_REQHD.
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 통화별합계금액.
       MTZFOPAMT   LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       DPZFOPAMT   LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       DAZFOPAMT   LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       TTBZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사전개설금액.
       TTAZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사후개설금액.
       WAERS       LIKE ZTREQST-WAERS,           " 이게 KEY 값이다.
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM.         " USD 환산금액.
DATA : END OF IT_TAB.
DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_CHECK_PAGE(1) TYPE C,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX,
        P_BUKRS       LIKE ZTREQHD-BUKRS.
*>> 가격조건별 환산금액.
DATA : MTZFUSDAM   LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       DPZFUSDAM   LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       DAZFUSDAM   LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       TTBZFUSDAM  LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       TTAZFUSDAM  LIKE ZTREQST-ZFUSDAM.         " USD 환산금액.

DATA:  GUBUN       LIKE ZTREQHD-ZFREQTY,
       GUBUN2       LIKE ZTREQHD-ZFBACD.
DATA  CURSORFIELD(20).

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS NO-EXTENSION
                                              NO INTERVALS,
                   S_RLDT    FOR ZTREQST-ZFOPNDT OBLIGATORY,  " 개설일.
                   S_WERKS   FOR ZTREQHD-ZFWERKS NO-EXTENSION
                                           NO INTERVALS,  " 대표 plant
                   S_EKORG   FOR ZTREQST-EKORG,
                   S_EKGRP   FOR ZTREQST-EKGRP.

SELECTION-SCREEN END OF BLOCK B1.
* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
   PERFORM   P1000_SET_BUKRS.
   PERFORM   P2000_SET_PARAMETER.

* Title Text Write
TOP-OF-PAGE.
   PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 권한 검증 함수.
*   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*  테이블 SELECT
   PERFORM   P1000_GET_ZTREQHD      USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'. MESSAGE S738.   EXIT.    ENDIF.

* 레포트 Write
   PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
 CASE SY-UCOMM.
    WHEN 'DTLC'.
       IF W_TABIX IS INITIAL.
           MESSAGE S962.    EXIT.
       ENDIF.
*       PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-WAERS.
     WHEN OTHERS.
  ENDCASE.
  CLEAR: IT_TAB, W_TABIX.
*---------------------------------------------------------------------
* AT LINE-SELECTION.
*----------------------------------------------------------------------

AT LINE-SELECTION.

   GET CURSOR FIELD CURSORFIELD.
     IF SY-SUBRC EQ 0.
       IF W_TABIX IS INITIAL.
           MESSAGE S962.EXIT.
       ENDIF.
       CASE CURSORFIELD.
          WHEN 'IT_TAB-MTZFOPAMT'.
              IF IT_TAB-MTZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'LC' TO GUBUN.
          WHEN 'IT_TAB-DAZFOPAMT'.
              IF IT_TAB-DAZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'DA' TO GUBUN.
          WHEN 'IT_TAB-DPZFOPAMT'.
               IF IT_TAB-DPZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'DP' TO GUBUN.
          WHEN  'IT_TAB-TTAZFOPAMT'.
               IF IT_TAB-TTAZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'TT' TO GUBUN.
              MOVE 'A'  TO GUBUN2.
          WHEN  'IT_TAB-TTBZFOPAMT'.
              IF IT_TAB-TTBZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'TT' TO GUBUN.
              MOVE 'B'  TO GUBUN2.
          WHEN  'IT_TAB-ZFOPAMT'.
              IF IT_TAB-ZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              CLEAR:  GUBUN,GUBUN2.
*>> USD 환산금액.
           WHEN 'MTZFUSDAM'.
              IF MTZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
               MOVE 'LC' TO GUBUN.

          WHEN 'DPZFUSAM'.
              IF DPZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'DP' TO GUBUN.

          WHEN 'DAZFUSDAM'.
              IF DAZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'DA' TO GUBUN.

          WHEN  'TTAZFUSDAM'.
              IF TTAZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'TT' TO GUBUN.
              MOVE 'A' TO GUBUN2.

          WHEN  'TTBZFUSDAM'.
              IF TTBZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'TT' TO GUBUN.
              MOVE 'B' TO GUBUN2.

          WHEN  'IT_TAB-ZFUSDAM'.
              IF IT_TAB-ZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              CLEAR: GUBUN,GUBUN2.
          WHEN OTHERS.
       ENDCASE.
       PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-WAERS GUBUN GUBUN2.

   ELSE.
      MESSAGE S962.EXIT.
   ENDIF.
   CLEAR : IT_TAB,MTZFUSDAM, DAZFUSDAM,DPZFUSDAM,TTAZFUSDAM,
           TTBZFUSDAM,
           IT_TAB-ZFUSDAM,W_TABIX.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR26'.          " TITLE BAR

  MOVE : 'I'               TO  S_RLDT-SIGN,
         'BT'              TO  S_RLDT-OPTION,
         SY-DATUM          TO  S_RLDT-HIGH.
  CONCATENATE SY-DATUM(6) '01' INTO S_RLDT-LOW.
  APPEND S_RLDT.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /65  '[ Open result list by payment type ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Period: ',S_RLDT-LOW,'~',S_RLDT-HIGH .

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, (04)  'Cur.',
            SY-VLINE, (17)  'Master L/C',
            SY-VLINE, (17)  'DP',
            SY-VLINE, (17)  'DA',
            SY-VLINE, (17)  'Advanced T/T',
            SY-VLINE, (17)  'After T/T',
            SY-VLINE, (17)  'Currency Sum',
            SY-VLINE, (17)  'EQU DL Total',
            SY-VLINE.
  WRITE:/ SY-ULINE.

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
  REFRESH: IT_TAB,IT_REQHD.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_REQHD
            FROM ZTREQHD AS R INNER JOIN ZTREQST AS I
             ON R~ZFREQNO = I~ZFREQNO
        WHERE   I~ZFDOCST  = 'O'              " 문서상태.
           AND  R~BUKRS    IN  S_BUKRS
           AND  R~ZFWERKS  IN  S_WERKS        "
           AND  I~ZFOPNDT  IN  S_RLDT         "  개설일.
           AND  I~EKORG    IN  S_EKORG        " 구매담당자.
           AND  I~EKGRP    IN  S_EKGRP        " 구매그룹.
           AND  R~ZFREQTY  IN  ('DA', 'DP', 'LC', 'TT'). " 결제구분

  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

  CLEAR : MTZFUSDAM, DPZFUSDAM,DAZFUSDAM,TTBZFUSDAM,TTAZFUSDAM.
  LOOP AT IT_REQHD.
      W_TABIX = SY-TABIX.
      CASE IT_REQHD-ZFREQTY.
         WHEN  'LC'.	" Master L/C(신용장)
             MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-MTZFOPAMT.
             ADD   IT_REQHD-ZFUSDAM TO  MTZFUSDAM.
         WHEN  'DA'.	" Documents against Acceptance(인수인도조건)
             MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-DAZFOPAMT.
             ADD   IT_REQHD-ZFUSDAM TO  DAZFUSDAM.
         WHEN  'DP'.	" Documents against Payment(지급인도조건)
            MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-DPZFOPAMT.
            ADD   IT_REQHD-ZFUSDAM TO   DPZFUSDAM.
         WHEN  'TT'.	" Telegraphic Transfer(전신환송금방식).
             CASE IT_REQHD-ZFBACD.
               WHEN 'A'." 사후.
                 MOVE  IT_REQHD-ZFOPAMT TO IT_REQHD-TTAZFOPAMT.
                 ADD  IT_REQHD-ZFUSDAM TO  TTAZFUSDAM.
               WHEN 'B'." 사전.
                 MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-TTBZFOPAMT.
                 ADD  IT_REQHD-ZFUSDAM TO  TTBZFUSDAM.
               WHEN OTHERS.
             ENDCASE.
        WHEN OTHERS.
      ENDCASE.
      MODIFY IT_REQHD INDEX W_TABIX.
      MOVE-CORRESPONDING IT_REQHD TO IT_TAB.
      COLLECT IT_TAB.
  ENDLOOP.

ENDFORM.                    " P1000_GET_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMR26'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR26'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LAST_WRITE.
         PERFORM P3000_RATE_WRITE.
      ENDAT.
   ENDLOOP.
    CLEAR : IT_TAB,MTZFUSDAM, DAZFUSDAM,DPZFUSDAM,TTAZFUSDAM,
           TTBZFUSDAM,
           IT_TAB-ZFUSDAM,W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_TOTAL OFF.
   SUM.
   WRITE :/ SY-VLINE,(04) 'EQU DL',      " 통화',
            SY-VLINE,(17) MTZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) DPZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) DAZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) TTBZFUSDAM CURRENCY 'USD',
            SY-VLINE,(17) TTAZFUSDAM CURRENCY 'USD',
            SY-VLINE, (17) '',
            SY-VLINE,(17) IT_TAB-ZFUSDAM CURRENCY 'USD',"
            SY-VLINE.
   HIDE : MTZFUSDAM, DAZFUSDAM,DPZFUSDAM,TTAZFUSDAM,
         TTBZFUSDAM,
         IT_TAB-ZFUSDAM,W_TABIX.

   WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

   WRITE :/ SY-VLINE,(04) IT_TAB-WAERS ,      " 통화',
            SY-VLINE,(17) IT_TAB-MTZFOPAMT CURRENCY IT_TAB-WAERS,
            SY-VLINE,(17) IT_TAB-DPZFOPAMT CURRENCY IT_TAB-WAERS,
            SY-VLINE,(17) IT_TAB-DAZFOPAMT CURRENCY IT_TAB-WAERS,
            SY-VLINE,(17) IT_TAB-TTBZFOPAMT CURRENCY IT_TAB-WAERS,
            SY-VLINE,(17) IT_TAB-TTAZFOPAMT CURRENCY IT_TAB-WAERS,  "
            SY-VLINE,(17) IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS,  "
            SY-VLINE,(17) IT_TAB-ZFUSDAM CURRENCY 'USD'," I
            SY-VLINE.
* hide
   HIDE: IT_TAB,W_TABIX.
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_RATE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_RATE_WRITE.

   DATA: L_MTRATE TYPE P DECIMALS 2,
         L_DPRATE TYPE P DECIMALS 2,
         L_DARATE TYPE P DECIMALS 2,
         L_TTBRATE TYPE P DECIMALS 2,
         L_TTARATE TYPE P DECIMALS 2.
**>> 비율
   IF  NOT IT_TAB-ZFUSDAM IS  INITIAL.
         L_MTRATE  = MTZFUSDAM / IT_TAB-ZFUSDAM  * 100.
         L_DPRATE  = DPZFUSDAM / IT_TAB-ZFUSDAM  * 100.
         L_DARATE  = DAZFUSDAM / IT_TAB-ZFUSDAM  * 100.
         L_TTBRATE = TTBZFUSDAM / IT_TAB-ZFUSDAM * 100.
         L_TTARATE = TTAZFUSDAM / IT_TAB-ZFUSDAM  * 100.
   ENDIF.
   FORMAT RESET.
   FORMAT COLOR COL_TOTAL ON.

   WRITE :/ SY-VLINE,(04) 'Ratio',      " 통화',
            SY-VLINE,(17) L_MTRATE ,
            SY-VLINE,(17) L_DPRATE ,
            SY-VLINE,(17) L_DARATE ,
            SY-VLINE,(17) L_TTBRATE ,
            SY-VLINE,(17) L_TTARATE ,
            SY-VLINE,(17) '',
            SY-VLINE,(17) '',
            SY-VLINE.
   WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_RATE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_TO_DISP_DETAIL
*&---------------------------------------------------------------------*
FORM P2000_TO_DISP_DETAIL USING    P_WAERS P_REQTY  P_BACD.

  DATA: SELTAB     TYPE TABLE OF RSPARAMS,
        SELTAB_WA  LIKE LINE OF SELTAB.
*>> 회사코드.
  IF NOT S_BUKRS[] IS INITIAL.
    MOVE: 'S_BUKRS'  TO SELTAB_WA-SELNAME,
          'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
          'I'        TO SELTAB_WA-SIGN,
          S_BUKRS-OPTION TO SELTAB_WA-OPTION,
          S_BUKRS-LOW    TO SELTAB_WA-LOW,
          S_BUKRS-HIGH   TO SELTAB_WA-HIGH.
    APPEND SELTAB_WA TO SELTAB.
  ENDIF.

*>> 구매그룹.
  IF NOT S_EKGRP[] IS INITIAL.
    MOVE: 'S_EKGRP'  TO SELTAB_WA-SELNAME,
          'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
          'I'        TO SELTAB_WA-SIGN,
          S_EKGRP-OPTION TO SELTAB_WA-OPTION,
          S_EKGRP-LOW    TO SELTAB_WA-LOW,
          S_EKGRP-HIGH   TO SELTAB_WA-HIGH.
    APPEND SELTAB_WA TO SELTAB.
  ENDIF.
**>> 구매조직.
  IF NOT S_EKORG[] IS INITIAL.
     MOVE: 'S_EKORG'  TO SELTAB_WA-SELNAME,
             'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
             'I'        TO SELTAB_WA-SIGN,
         S_EKORG-OPTION TO SELTAB_WA-OPTION,
         S_EKORG-LOW    TO SELTAB_WA-LOW,
         S_EKORG-HIGH   TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
  ENDIF.
*>> 개설일.
  LOOP AT S_RLDT.
      MOVE: 'S_RLDT'         TO SELTAB_WA-SELNAME,
            'S'              TO SELTAB_WA-KIND,      " SELECT-OPTION
            S_RLDT-SIGN      TO SELTAB_WA-SIGN,
            S_RLDT-OPTION    TO SELTAB_WA-OPTION,
            S_RLDT-LOW       TO SELTAB_WA-LOW,
            S_RLDT-HIGH      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
  ENDLOOP.
*>> PLANT.
  IF NOT S_WERKS[] IS INITIAL.
     MOVE: 'S_WERKS'  TO SELTAB_WA-SELNAME,
         'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
         'I'        TO SELTAB_WA-SIGN,
         'EQ'       TO SELTAB_WA-OPTION,
         S_WERKS-LOW   TO SELTAB_WA-LOW,
         SPACE      TO SELTAB_WA-HIGH.
     APPEND SELTAB_WA TO SELTAB.
  ENDIF.

  IF NOT P_WAERS IS INITIAL.
      MOVE: 'S_WAERS'  TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_WAERS   TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
  ENDIF.
  IF NOT P_REQTY IS INITIAL.
      MOVE: 'S_REQTY'  TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_REQTY    TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
  ENDIF.
  IF NOT P_BACD IS INITIAL.
      MOVE: 'S_BACD'  TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_BACD     TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
  ENDIF.

  SUBMIT ZRIMLCDTLST
          WITH  SELECTION-TABLE SELTAB
          AND RETURN.


ENDFORM.                    " P2000_TO_DISP_DETAIL
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
