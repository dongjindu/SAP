*&---------------------------------------------------------------------*
*& Report  ZRIMBKLST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입OPEN 실적(은행별)                                 *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.10.04                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMBKLST   MESSAGE-ID ZIM
                     LINE-SIZE 159
                     NO STANDARD PAGE HEADING.
TABLES: ZTREQHD,ZTREQST,LFA1, ZTIMIMG00.
*-----------------------------------------------------------------------
* 수입의뢰 릴리즈용 INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_REQHD OCCURS 0,
       ZFREQTY     LIKE ZTREQHD-ZFREQTY,         " 결제조건.
       ZFBACD      LIKE ZTREQHD-ZFBACD,          " 사전사후.
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       ZFOPBN      LIKE ZTREQHD-ZFOPBN,          " 이게 KEY 값이다.
       DEMZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       EURZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       GBPZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       JPYZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사전개설금액.
       USDZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사후개설금액.
       ETCZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사후개설금액.
       WAERS       LIKE ZTREQST-WAERS,
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       ZFUSD       LIKE ZTREQST-ZFUSD.
DATA : END OF IT_REQHD.
DATA : BEGIN OF IT_TAB OCCURS 0,
       ZFOPAMT     LIKE ZTREQST-ZFOPAMT,         " 통화별합계금액.
       DEMZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       EURZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       GBPZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 개설금액.
       JPYZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사전개설금액.
       USDZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사후개설금액.
       ETCZFOPAMT  LIKE ZTREQST-ZFOPAMT,         " 사후개설금액.
       ZFOPBN      LIKE ZTREQHD-ZFOPBN,          " 이게 KEY 값이다.
       NAME1       LIKE LFA1-NAME1,              " 개설은행.
       ZFUSDAM     LIKE ZTREQST-ZFUSDAM.         " USD 환산금액.

*      ZFUSD       LIKE ZTREQST-ZFUSD.
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
*>> 통화별환산금액.
DATA:  DEMZFUSDAM  LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       EURZFUSDAM  LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       GBPZFUSDAM  LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       JPYZFUSDAM  LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       USDZFUSDAM  LIKE ZTREQST-ZFUSDAM,         " USD 환산금액.
       ETCZFUSDAM  LIKE ZTREQST-ZFUSDAM.         " USD 환산금액.
DATA: CURSORFIELD(20).
DATA: GUBUN(05).

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_BUKRS   FOR ZTREQHD-BUKRS   NO-EXTENSION
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
          WHEN 'IT_TAB-DEMZFOPAMT'.
              IF IT_TAB-DEMZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'DEM' TO GUBUN.
          WHEN 'IT_TAB-EURZFOPAMT'.
              IF IT_TAB-EURZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'EUR' TO GUBUN.
          WHEN 'IT_TAB-GBPZFOPAMT'.
               IF IT_TAB-GBPZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'GBP' TO GUBUN.
          WHEN  'IT_TAB-JPYZFOPAMT'.
               IF IT_TAB-JPYZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'JPY' TO GUBUN.
          WHEN  'IT_TAB-USDZFOPAMT'.
              IF IT_TAB-USDZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'USD' TO GUBUN.
          WHEN  'IT_TAB-ZFOPAMT'.
              IF IT_TAB-ZFOPAMT IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              CLEAR  GUBUN.
*>> USD 환산금액.
           WHEN 'DEMZFUSDAM'.
              IF DEMZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
               MOVE 'DEM' TO GUBUN.
              CLEAR IT_TAB-ZFOPBN.
          WHEN 'EURZFUSAM'.
              IF EURZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'EUR' TO GUBUN.
              CLEAR IT_TAB-ZFOPBN.
          WHEN 'GBPZFUSDAM'.
              IF GBPZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'GBP' TO GUBUN.
              CLEAR IT_TAB-ZFOPBN.
          WHEN  'JPYZFUSDAM'.
              IF JPYZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'JPY' TO GUBUN.
              CLEAR IT_TAB-ZFOPBN.
          WHEN  'USDZFUSDAM'.
              IF USDZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'USD' TO GUBUN.
              CLEAR IT_TAB-ZFOPBN.
          WHEN  'IT_TAB-ZFUSDAM'.
              IF IT_TAB-ZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              CLEAR IT_TAB-ZFOPBN.
              WHEN 'ETCZFUSDAM'.
              IF ETCZFUSDAM IS INITIAL.
                 MESSAGE S962.EXIT.
              ENDIF.
              MOVE 'ETC' TO GUBUN.
              CLEAR IT_TAB-ZFOPBN.
          WHEN OTHERS.
       ENDCASE.
       PERFORM P2000_TO_DISP_DETAIL USING IT_TAB-ZFOPBN GUBUN.
   ELSE.
      MESSAGE S962.EXIT.
   ENDIF.
   CLEAR : IT_TAB,DEMZFUSDAM, EURZFUSDAM,GBPZFUSDAM,JPYZFUSDAM,
           USDZFUSDAM,
           ETCZFUSDAM,IT_TAB-ZFUSDAM,W_TABIX.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR27'.          " TITLE BAR

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
  WRITE : /65  '[ Open result list by opening bank ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : /3 'Period: ',S_RLDT-LOW,'~',S_RLDT-HIGH .

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, (15)  'Bank',
            SY-VLINE, (17)  'DEM',
            SY-VLINE, (17)  'EUR',
            SY-VLINE, (17)  'GBP',
            SY-VLINE, (17)  'JPY',
            SY-VLINE, (17)  'USD',
            SY-VLINE, (17)  'Others Currency',
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
  CLEAR:DEMZFUSDAM,EURZFUSDAM,GBPZFUSDAM,JPYZFUSDAM,
        USDZFUSDAM,ETCZFUSDAM.

  LOOP AT IT_REQHD.
      W_TABIX = SY-TABIX.
      MOVE:  IT_REQHD-ZFOPBN  TO IT_TAB-ZFOPBN.
      MOVE   IT_REQHD-ZFUSDAM TO IT_TAB-ZFUSDAM.

      CASE IT_REQHD-WAERS.
         WHEN  'DEM'.
           MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-DEMZFOPAMT.
           ADD  IT_REQHD-ZFUSDAM TO DEMZFUSDAM.
        WHEN  'EUR'.	"
           MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-EURZFOPAMT.
           ADD  IT_REQHD-ZFUSDAM TO EURZFUSDAM.
        WHEN  'GBP'.	"
           MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-GBPZFOPAMT.
           ADD  IT_REQHD-ZFUSDAM TO GBPZFUSDAM.
        WHEN  'JPY'.	"
           MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-JPYZFOPAMT.
           ADD  IT_REQHD-ZFUSDAM  TO JPYZFUSDAM.

        WHEN 'USD'.
           MOVE  IT_REQHD-ZFOPAMT TO  IT_REQHD-USDZFOPAMT.
           ADD  IT_REQHD-ZFUSDAM TO USDZFUSDAM.
        WHEN OTHERS.
           ADD  IT_REQHD-ZFUSDAM TO  ETCZFUSDAM.
      ENDCASE.
      MODIFY IT_REQHD INDEX W_TABIX.
      MOVE-CORRESPONDING IT_REQHD TO IT_TAB.
      COLLECT IT_TAB.
  ENDLOOP.

  LOOP AT IT_TAB.
     W_TABIX = SY-TABIX.

     CLEAR LFA1.
     SELECT SINGLE *
        FROM LFA1
       WHERE LIFNR  = IT_TAB-ZFOPBN.
     IT_TAB-NAME1 = LFA1-NAME1.
     MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_GET_ZTREQST
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

   SET PF-STATUS 'ZIMR27'.           " GUI STATUS SETTING
   SET  TITLEBAR 'ZIMR27'.           " GUI TITLE SETTING..

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   LOOP AT IT_TAB.
      W_TABIX = SY-TABIX.
      PERFORM P3000_LINE_WRITE.
      AT LAST.
         PERFORM P3000_LAST_WRITE.
         PERFORM P3000_RATE_WRITE.
      ENDAT.
   ENDLOOP.
   CLEAR: IT_TAB,W_TABIX,
          DEMZFUSDAM, EURZFUSDAM,GBPZFUSDAM,JPYZFUSDAM,USDZFUSDAM,
          ETCZFUSDAM,IT_TAB-ZFUSDAM.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_TOTAL OFF.
   SUM.
   WRITE :/ SY-VLINE,(15) 'EQU DL',      " 통화',
            SY-VLINE,(17) DEMZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) EURZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) GBPZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) JPYZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) USDZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) ETCZFUSDAM CURRENCY   'USD',
            SY-VLINE,(17) IT_TAB-ZFUSDAM CURRENCY 'USD',"
            SY-VLINE.
   HIDE:  DEMZFUSDAM, EURZFUSDAM,GBPZFUSDAM,JPYZFUSDAM,USDZFUSDAM,
          ETCZFUSDAM,IT_TAB-ZFUSDAM,W_TABIX.
   WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

   WRITE :/ SY-VLINE,(15) IT_TAB-NAME1,      " 은행',
            SY-VLINE,(17) IT_TAB-DEMZFOPAMT CURRENCY 'DEM',
            SY-VLINE,(17) IT_TAB-EURZFOPAMT CURRENCY 'EUR',
            SY-VLINE,(17) IT_TAB-GBPZFOPAMT CURRENCY 'GBP',
            SY-VLINE,(17) IT_TAB-JPYZFOPAMT CURRENCY 'JPY',
            SY-VLINE,(17) IT_TAB-USDZFOPAMT CURRENCY 'USD',
            SY-VLINE,(17) '',
            SY-VLINE,(17) IT_TAB-ZFUSDAM CURRENCY 'USD',
            SY-VLINE.
* hide
   HIDE: IT_TAB, W_TABIX.
   W_COUNT = W_COUNT + 1.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_RATE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_RATE_WRITE.

  DATA: L_DEMRATE TYPE P DECIMALS 2,
        L_EURRATE TYPE P DECIMALS 2,
        L_GBPRATE TYPE P DECIMALS 2,
        L_JPYRATE TYPE P DECIMALS 2,
        L_USDRATE TYPE P DECIMALS 2,
        L_ETCRATE TYPE P DECIMALS 2.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL ON.

**>> 비율
  IF  NOT IT_TAB-ZFUSDAM IS  INITIAL.
        L_DEMRATE  = DEMZFUSDAM / IT_TAB-ZFUSDAM  * 100.
        L_EURRATE  = EURZFUSDAM / IT_TAB-ZFUSDAM  * 100.
        L_GBPRATE  = GBPZFUSDAM / IT_TAB-ZFUSDAM  * 100.
        L_JPYRATE  = JPYZFUSDAM / IT_TAB-ZFUSDAM * 100.
        L_USDRATE  = USDZFUSDAM / IT_TAB-ZFUSDAM  * 100.
        L_ETCRATE  = ETCZFUSDAM / IT_TAB-ZFUSDAM  * 100.

  ENDIF.
  WRITE :/ SY-VLINE,(15) 'Ratio',      " 통화',
           SY-VLINE,(17) L_DEMRATE ,
           SY-VLINE,(17) L_EURRATE ,
           SY-VLINE,(17) L_GBPRATE ,
           SY-VLINE,(17) L_JPYRATE ,
           SY-VLINE,(17) L_USDRATE ,
           SY-VLINE,(17) L_ETCRATE ,
           SY-VLINE,(17) '',
           SY-VLINE.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_RATE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_TO_DISP_DETAIL
*&---------------------------------------------------------------------*
FORM P2000_TO_DISP_DETAIL USING    P_ZFOPBN P_WAERS.

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
*>> 구매조직.
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

  IF NOT P_ZFOPBN IS INITIAL.
      MOVE: 'S_OPBN'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'I'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            P_ZFOPBN   TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
      APPEND SELTAB_WA TO SELTAB.
  ENDIF.
  IF P_WAERS EQ 'ETC'.
     MOVE: 'S_WAERS'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'E'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            'DEM'      TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
     APPEND SELTAB_WA TO SELTAB.
     MOVE: 'S_WAERS'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'E'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            'EUR'      TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
     APPEND SELTAB_WA TO SELTAB.
     MOVE: 'S_WAERS'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'E'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            'GBP'      TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
     APPEND SELTAB_WA TO SELTAB.
     MOVE: 'S_WAERS'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'E'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            'JPY'      TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
     APPEND SELTAB_WA TO SELTAB.
     MOVE: 'S_WAERS'   TO SELTAB_WA-SELNAME,
            'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
            'E'        TO SELTAB_WA-SIGN,
            'EQ'       TO SELTAB_WA-OPTION,
            'USD'      TO SELTAB_WA-LOW,
            SPACE      TO SELTAB_WA-HIGH.
     APPEND SELTAB_WA TO SELTAB.

  ELSE.
      IF NOT P_WAERS  IS INITIAL.
          MOVE: 'S_WAERS'   TO SELTAB_WA-SELNAME,
                'S'        TO SELTAB_WA-KIND,      " SELECT-OPTION
                'I'        TO SELTAB_WA-SIGN,
                'EQ'       TO SELTAB_WA-OPTION,
                P_WAERS    TO SELTAB_WA-LOW,
                SPACE      TO SELTAB_WA-HIGH.
          APPEND SELTAB_WA TO SELTAB.
      ENDIF.
  ENDIF.
  SUBMIT ZRIMLCDTLST
          WITH  SELECTION-TABLE SELTAB
          AND RETURN.
ENDFORM.
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
