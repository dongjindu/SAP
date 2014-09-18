*&---------------------------------------------------------------------*
*& Report          ZRIMBLAIRCST                                        *
*&---------------------------------------------------------------------*
*&  프로그램명 : 항공 운임 검토서                                      *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.07                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

PROGRAM  ZRIMBLAIRCST  MESSAGE-ID ZIM
                     LINE-SIZE 105
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

INCLUDE ZRIMBLAIRCSTTOP.
INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*-----------------------------------------------------------------------
* Selection Screen .
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
*>> 검색조건
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS :   P_ZFBLNO  LIKE  ZTBL-ZFBLNO
                         OBLIGATORY MEMORY ID ZPBLNO.    " 출고번호.
SELECTION-SCREEN END OF BLOCK B1.

*---------------------------------------------------------------------*
* EVENT INITIALIZATION.
*---------------------------------------------------------------------*
INITIALIZATION.                                 " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.
  SET TITLEBAR 'BLCST'.
*---------------------------------------------------------------------*
* EVENT AT SELECTION-SCREEN.
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  SELECT SINGLE *  FROM ZTBL
                 WHERE ZFBLNO = P_ZFBLNO.
  IF ZTBL-ZFVIA = 'VSL'.
    MESSAGE E451(ZIM1) WITH 'Ocean'.
  ENDIF.

*---------------------------------------------------------------------*
* EVENT START-OF-SELECTION.
*---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE .
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*  CLEAR : IT_TAB.
*-----------------------------------------------------------------------
* EVENT AT USER-COMMAND.
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
*    WHEN 'STUP' OR 'STDN'.              " SORT 선택?
*      IF IT_TAB-ZFTRNO IS INITIAL.
*        MESSAGE S962.
*      ELSE.
*        W_FIELD_NM = 'ZFTRNO'.
*        ASSIGN W_FIELD_NM   TO <SORT_FIELD>.
*        PERFORM HANDLE_SORT TABLES  IT_TAB
*                            USING   SY-UCOMM.
*      ENDIF.
*    WHEN 'MKAL' OR 'MKLO'.          " 전체 선택 및 선택해제.
*      PERFORM P2000_SELECT_RECORD   USING   SY-UCOMM.
*
    WHEN 'REFR'.
      PERFORM   P1000_READ_TEXT  USING W_ERR_CHK.
      IF W_ERR_CHK EQ 'Y'.    LEAVE TO SCREEN 0.    ENDIF.
      PERFORM   RESET_LIST.

    WHEN 'BAC1' OR 'EXIT' OR 'CANC'.    " 종료.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
*  CLEAR IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.


ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    P_W_ERR_CHK.

*>> 선적내용, 운임.
  PERFORM P1000_READ_TRHD.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*>> 부대비용.
  PERFORM P1000_READ_COST.

ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  SET PF-STATUS 'BLCST'.

  CLEAR : W_OTH_WT, W_OTH_WTM, W_DF_CHA,  W_UP_CHA,
          W_REMARK, W_SUB_TOT, W_SUB_KRW, W_GRD_TOT, W_GRD_KRW.

*>> 운임 Head.
  PERFORM P3000_HEAD_WRITE.

ENDFORM.                    " P3000_DATA_WRITE
*&----------------------------------------------------------------------
*&      Form  RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
* 레포트 Write
  PERFORM   P3000_DATA_WRITE .

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TRHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_TRHD .

*> B/L 내용.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF ST_HEAD
                  FROM ZTBL
                 WHERE ZFBLNO = P_ZFBLNO.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S738.
    EXIT.
  ENDIF.

*> 반입정보.
  IF ST_HEAD-ZFGMNO IS INITIAL.
    SELECT SINGLE ZFINDT ZFTBLNO ZFGMNO ZFMSN ZFHSN
             INTO (ST_HEAD-ZFINDT, ST_HEAD-ZFTBLNO,
                   ST_HEAD-ZFGMNO, ST_HEAD-ZFMSN, ST_HEAD-ZFHSN)
             FROM ZTBLINR_TMP
            WHERE ZFBLNO = P_ZFBLNO.
  ELSE.
    SELECT SINGLE ZFINDT ZFTBLNO
             INTO (ST_HEAD-ZFINDT, ST_HEAD-ZFTBLNO)
             FROM ZTBLINR_TMP
            WHERE ZFBLNO = P_ZFBLNO.
  ENDIF.
*> 요율상 선적지명.
  IF   NOT ST_HEAD-ZFCDTY   IS INITIAL
  AND  NOT ST_HEAD-ZFCD   IS INITIAL.
    SELECT SINGLE ZFCDNM INTO ST_HEAD-W_AREA
             FROM ZTIMIMG08
            WHERE ZFCDTY = ST_HEAD-ZFCDTY
              AND ZFCD   = ST_HEAD-ZFCD.
  ENDIF.

*>> 수요 사업소명.
  IF NOT ST_HEAD-ZFWERKS IS INITIAL.
    SELECT SINGLE NAME1 INTO ST_HEAD-W_WERKS
           FROM   T001W
           WHERE  WERKS  EQ ST_HEAD-ZFWERKS
           AND    SPRAS  EQ SY-LANGU.
  ENDIF.

ENDFORM.                    " P1000_READ_TRHD

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_COST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_COST .

*> 세부내역.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB_COST
          FROM ZTBLCST
         WHERE ZFBLNO     = ST_HEAD-ZFBLNO
           AND ZFCSQ      > '10000'.
*           AND ZFCKAMT    NE 0.

  LOOP AT IT_TAB_COST.
    W_TABIX = SY-TABIX.
    SELECT SINGLE ZFCDNM INTO IT_TAB_COST-ZFCDNM
                  FROM ZTIMIMG08
                 WHERE ZFCDTY = '004'
                   AND ZFCD   = IT_TAB_COST-ZFCSCD.

    MODIFY IT_TAB_COST INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_COST
*&---------------------------------------------------------------------*
*&      Form  P3000_HEAD_WRITE
*&---------------------------------------------------------------------*
FORM P3000_HEAD_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /40  '[ 항공 운임 검토서 ]'
               COLOR COL_HEADING INTENSIFIED OFF.

  SKIP 2.
*> 1. 선적내용.
  WRITE : '1. 선적내용 ( 화물관리번호 : ', ST_HEAD-ZFGMNO, '-',
           ST_HEAD-ZFMSN, '-', ST_HEAD-ZFHSN, ' )'.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '구    분' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) '내    용' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (15) '구    분' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) '내    용' CENTERED NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '계약번호(차수)' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (10) ST_HEAD-ZFREBELN    NO-GAP,
            ' ( ' NO-GAP, ST_HEAD-ZFSHNO NO-GAP, ' )' NO-GAP,
         53 SY-VLINE NO-GAP, (15) '선  기  명' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-ZFCARNM     NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '입항일/반입일' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (17) ST_HEAD-ZFRETA    NO-GAP,
            SY-VLINE NO-GAP, (17) ST_HEAD-ZFINDT    NO-GAP,
            SY-VLINE NO-GAP, (05) ' '               NO-GAP,
            SY-VLINE NO-GAP, (09) 'MASTER' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-ZFMBLNO   NO-GAP,
            SY-VLINE NO-GAP.
  WRITE : /(53) SY-ULINE, (04) 'B/L' CENTERED NO-GAP,
           (47) SY-ULINE.

  WRITE : / SY-VLINE NO-GAP, (15) '반입 번호' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-ZFTBLNO   NO-GAP,
            SY-VLINE NO-GAP, (05) ' '               NO-GAP,
            SY-VLINE NO-GAP, (09) 'HOUSE'  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-ZFHBLNO   NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '선 적 일'  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-ZFETD     NO-GAP,
            SY-VLINE NO-GAP, (15) '인도방법'  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-INCO1     NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '선 적 지'  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-ZFSPRT     NO-GAP,
            SY-VLINE NO-GAP, (15) '도 착 지'  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-ZFAPRT     NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '요율상 선적지' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (35) ST_HEAD-W_AREA     NO-GAP,
            SY-VLINE NO-GAP, (15) '환     율'     CENTERED NO-GAP,
            SY-VLINE NO-GAP, (04) 'KRW/'            NO-GAP,
       (05) ST_HEAD-ZFTRCUR   NO-GAP,
       (12) ST_HEAD-ZFEXRTT RIGHT-JUSTIFIED      NO-GAP,
            '( ' NO-GAP, (10) ST_HEAD-ZFEXDTT NO-GAP, ' )' NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '운임 계산중량' CENTERED NO-GAP,
            SY-VLINE NO-GAP, '중량:' NO-GAP.
*>> 중량변경.
  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      INPUT  = ST_HEAD-ZFTOWTM
    IMPORTING
      OUTPUT = W_OTH_WTM.

  IF W_OTH_WTM EQ 'KG'.
    W_OTH_WT = ST_HEAD-ZFTOWT * ( 22 / 10 ).
    WRITE : (17) ST_HEAD-ZFTOWT UNIT ST_HEAD-ZFTOWTM
                         RIGHT-JUSTIFIED NO-GAP,
            (03) ST_HEAD-ZFTOWTM NO-GAP.
    CLEAR : W_OTH_WTM.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        INPUT  = 'LB'
      IMPORTING
        OUTPUT = W_OTH_WTM.
    WRITE : (17) W_OTH_WT UNIT W_OTH_WTM
                         RIGHT-JUSTIFIED NO-GAP,
            (03) W_OTH_WTM NO-GAP.
    CLEAR : W_OTH_WTM, W_OTH_WT.
  ELSEIF W_OTH_WTM EQ 'LB'.
    W_OTH_WT = ST_HEAD-ZFTOWT / ( 22 / 10 ).
    CLEAR : W_OTH_WTM.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
      EXPORTING
        INPUT  = 'KG'
      IMPORTING
        OUTPUT = W_OTH_WTM.
    WRITE : (17) W_OTH_WT UNIT W_OTH_WTM
                         RIGHT-JUSTIFIED NO-GAP,
            (03) W_OTH_WTM NO-GAP.
    CLEAR : W_OTH_WTM, W_OTH_WT.
    WRITE : (17) ST_HEAD-ZFTOWT UNIT ST_HEAD-ZFTOWTM
                         RIGHT-JUSTIFIED NO-GAP,
            (03) ST_HEAD-ZFTOWTM NO-GAP.

  ENDIF.
  WRITE : 105 SY-VLINE NO-GAP.
  ULINE.
  SKIP .

*> 2. 운임.
  WRITE : '2. 운임'.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '항    목' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (21) '중    량' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (21) '단    가' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (17) '요    율' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (25) '운    임' CENTERED NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
*-> 협의 운임을 사용하지 않을때.
  IF ST_HEAD-ZFCHARGE IS INITIAL.
    WRITE : / SY-VLINE NO-GAP, (15) '기    본' CENTERED NO-GAP,
              SY-VLINE NO-GAP,
         (18) ST_HEAD-ZFTOWT UNIT ST_HEAD-ZFTOWTM
                             RIGHT-JUSTIFIED NO-GAP,
         (03) ST_HEAD-ZFTOWTM NO-GAP, SY-VLINE NO-GAP,
         (16) ST_HEAD-ZFNETPR1 CURRENCY ST_HEAD-ZFTRCUR
                             RIGHT-JUSTIFIED NO-GAP,
         (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP,
         (14) ST_HEAD-ZFMRATE RIGHT-JUSTIFIED NO-GAP,
         (03) ' %' NO-GAP, SY-VLINE NO-GAP.
    CLEAR :W_DF_CHA.
    W_DF_CHA =  ST_HEAD-ZFTOWT * ST_HEAD-ZFNETPR1
                     * ST_HEAD-ZFMRATE / 100.
    WRITE : (20) W_DF_CHA   CURRENCY ST_HEAD-ZFTRCUR
                                RIGHT-JUSTIFIED NO-GAP,
          (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP.
    ULINE.
    WRITE : / SY-VLINE NO-GAP, (15) '차 상 위' CENTERED NO-GAP,
              SY-VLINE NO-GAP,
         (18) ST_HEAD-ZFUPWT UNIT ST_HEAD-ZFTOWTM
                             RIGHT-JUSTIFIED NO-GAP,
         (03) ST_HEAD-ZFTOWTM NO-GAP, SY-VLINE NO-GAP,
         (16) ST_HEAD-ZFNETPR2 CURRENCY ST_HEAD-ZFTRCUR
                             RIGHT-JUSTIFIED NO-GAP,
         (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP,
         (14) ST_HEAD-ZFMRATE RIGHT-JUSTIFIED NO-GAP,
         (03) ' %' NO-GAP, SY-VLINE NO-GAP.
    CLEAR :W_UP_CHA.
    W_UP_CHA =  ST_HEAD-ZFUPWT * ST_HEAD-ZFNETPR2
                     * ST_HEAD-ZFMRATE / 100.
    WRITE : (20) W_UP_CHA   CURRENCY ST_HEAD-ZFTRCUR
                                RIGHT-JUSTIFIED NO-GAP,
          (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP.
    ULINE.
    WRITE : / SY-VLINE NO-GAP, (15) '운임계산' CENTERED NO-GAP,
              SY-VLINE NO-GAP.
    IF ST_HEAD-ZFDFUP IS INITIAL.
      WRITE : (18) ST_HEAD-ZFTOWT UNIT ST_HEAD-ZFTOWTM
                               RIGHT-JUSTIFIED NO-GAP,
              (03) ST_HEAD-ZFTOWTM NO-GAP, SY-VLINE NO-GAP,
              (16) ST_HEAD-ZFNETPR1 CURRENCY ST_HEAD-ZFTRCUR
                               RIGHT-JUSTIFIED NO-GAP,
              (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP,
              (14) ST_HEAD-ZFMRATE RIGHT-JUSTIFIED NO-GAP,
              (03) ' %' NO-GAP, SY-VLINE NO-GAP,
              (20) W_DF_CHA   CURRENCY ST_HEAD-ZFTRCUR
                                  RIGHT-JUSTIFIED NO-GAP,
              (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP.
    ELSE.
      WRITE : (18) ST_HEAD-ZFUPWT UNIT ST_HEAD-ZFTOWTM
                               RIGHT-JUSTIFIED NO-GAP,
              (03) ST_HEAD-ZFTOWTM NO-GAP, SY-VLINE NO-GAP,
              (16) ST_HEAD-ZFNETPR2 CURRENCY ST_HEAD-ZFTRCUR
                               RIGHT-JUSTIFIED NO-GAP,
              (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP,
              (14) ST_HEAD-ZFMRATE RIGHT-JUSTIFIED NO-GAP,
              (03) ' %' NO-GAP, SY-VLINE NO-GAP,
              (20) W_UP_CHA   CURRENCY ST_HEAD-ZFTRCUR
                                  RIGHT-JUSTIFIED NO-GAP,
              (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP.
    ENDIF.
*--> 협의운임을 사용할때.
  ELSEIF ST_HEAD-ZFCHARGE EQ 'X'.
    WRITE : / SY-VLINE NO-GAP, (15) '협    의' CENTERED NO-GAP,
              SY-VLINE NO-GAP,
         (18) ST_HEAD-ZFTOWT UNIT ST_HEAD-ZFTOWTM
                             RIGHT-JUSTIFIED NO-GAP,
         (03) ST_HEAD-ZFTOWTM NO-GAP, SY-VLINE NO-GAP,
         (16) ST_HEAD-ZFFRE CURRENCY ST_HEAD-ZFTRCUR
                             RIGHT-JUSTIFIED NO-GAP,
         (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP,
         (14) ST_HEAD-ZFMRATE RIGHT-JUSTIFIED NO-GAP,
         (03) ' %' NO-GAP, SY-VLINE NO-GAP.
    CLEAR :W_DF_CHA.
    W_DF_CHA =  ST_HEAD-ZFTOWT * ST_HEAD-ZFFRE
                     * ST_HEAD-ZFMRATE / 100.
    WRITE : (20) W_DF_CHA   CURRENCY ST_HEAD-ZFTRCUR
                                RIGHT-JUSTIFIED NO-GAP,
          (05) ST_HEAD-ZFTRCUR NO-GAP, SY-VLINE NO-GAP.
  ENDIF.
*-->  실지급.
  ULINE.
  READ TABLE IT_TAB_COST WITH KEY ZFCSCD = 'ABC'.
  W_GRD_TOT = IT_TAB_COST-ZFCAMT.
  W_GRD_KRW = IT_TAB_COST-ZFCKAMT.
  IF SY-SUBRC EQ '0'.
    WRITE : / SY-VLINE NO-GAP, (15) '실 지 급' CENTERED NO-GAP,
              SY-VLINE NO-GAP,
           58(16) IT_TAB_COST-ZFCAMT CURRENCY IT_TAB_COST-WAERS
                             RIGHT-JUSTIFIED NO-GAP,
             (05) IT_TAB_COST-WAERS NO-GAP, '( ' NO-GAP,
             (18) IT_TAB_COST-ZFCKAMT   CURRENCY 'KRW'
                                   RIGHT-JUSTIFIED NO-GAP,
             (05) 'KRW' NO-GAP, ')' NO-GAP, SY-VLINE NO-GAP.
  ENDIF.
  ULINE.
  SKIP .

*> 3.부대비용.
  WRITE : '3. 부대비용'.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '항    목' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (43) '금    액' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (43) '비    고' CENTERED NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.

  LOOP AT IT_TAB_COST WHERE ZFCSCD NE 'ABC'.
    W_SUB_TOT = W_SUB_TOT + IT_TAB_COST-ZFCAMT.
    W_SUB_KRW = W_SUB_KRW + IT_TAB_COST-ZFCKAMT.
    WRITE : / SY-VLINE NO-GAP,
         (15) IT_TAB_COST-ZFCDNM CENTERED NO-GAP,
              SY-VLINE NO-GAP,
         (16) IT_TAB_COST-ZFCAMT CURRENCY IT_TAB_COST-WAERS
                               RIGHT-JUSTIFIED NO-GAP,
         (05) IT_TAB_COST-WAERS NO-GAP, '/' NO-GAP,
         (16) IT_TAB_COST-ZFCKAMT   CURRENCY 'KRW'
                                     RIGHT-JUSTIFIED NO-GAP,
         (05) 'KRW' NO-GAP, SY-VLINE NO-GAP,
         (43)  W_REMARK INPUT ON NO-GAP,
              SY-VLINE NO-GAP.
    ULINE.
  ENDLOOP.
  W_GRD_TOT = W_GRD_TOT + W_SUB_TOT.
  W_GRD_KRW = W_GRD_KRW + W_SUB_KRW.
  WRITE : / SY-VLINE NO-GAP, (15) '소     계' CENTERED NO-GAP,
            SY-VLINE NO-GAP,
       (38) W_SUB_TOT CURRENCY ST_HEAD-ZFTRCUR
                             RIGHT-JUSTIFIED NO-GAP,
       (05) ST_HEAD-ZFTRCUR NO-GAP, '(' NO-GAP,
       (37) W_SUB_KRW   CURRENCY 'KRW'
                                   RIGHT-JUSTIFIED NO-GAP,
       (05) 'KRW' NO-GAP, ')' NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  SKIP .
*> 4.운임 합계.
  WRITE : '4. 운임합계'.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '금     액' CENTERED NO-GAP,
            SY-VLINE NO-GAP,
       (38) W_GRD_TOT CURRENCY ST_HEAD-ZFTRCUR
                             RIGHT-JUSTIFIED NO-GAP,
       (05) ST_HEAD-ZFTRCUR NO-GAP, '(' NO-GAP,
       (37) W_GRD_KRW   CURRENCY 'KRW'
                                   RIGHT-JUSTIFIED NO-GAP,
       (05) 'KRW' NO-GAP, ')' NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.

  SKIP .
*>> 결재라인.
  WRITE :  20(51) SY-ULINE,
          /20 SY-VLINE NO-GAP, (04) '결'     CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '직  원' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '과  장' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '부  장' CENTERED NO-GAP,
              SY-VLINE NO-GAP,
          /20 SY-VLINE NO-GAP, (4) '  ' CENTERED NO-GAP,
       25(46) SY-ULINE NO-GAP,
          /20 SY-VLINE NO-GAP, (04) '  ' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
              SY-VLINE NO-GAP,
          /20 SY-VLINE NO-GAP, (04) '재' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
              SY-VLINE NO-GAP,
      /20(51) SY-ULINE.
  SKIP 2 .
ENDFORM.                    " P3000_HEAD_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_VENDOR
*&---------------------------------------------------------------------*
FORM P1000_GET_VENDOR USING    P_LIFNR
                      CHANGING P_NAME1.
  DATA : L_TEXT(35).

  CLEAR : P_NAME1, W_LFA1.
  IF P_LIFNR IS INITIAL.
    EXIT.
  ENDIF.

* VENDOR MASTER SELECT( LFA1 )----------------------->
  CALL FUNCTION 'READ_LFA1'
    EXPORTING
      XLIFNR         = P_LIFNR
    IMPORTING
      XLFA1          = W_LFA1
    EXCEPTIONS
      KEY_INCOMPLETE = 01
      NOT_AUTHORIZED = 02
      NOT_FOUND      = 03.

  CASE SY-SUBRC.
    WHEN 01.     MESSAGE I025.
    WHEN 02.     MESSAGE E950.
    WHEN 03.     MESSAGE E020   WITH    P_LIFNR.
  ENDCASE.
*   TRANSLATE W_LFA1 TO UPPER CASE.
  MOVE: W_LFA1-NAME1   TO   L_TEXT.
  TRANSLATE L_TEXT TO UPPER CASE.
  P_NAME1 = L_TEXT.

ENDFORM.                    " P1000_GET_VENDOR
