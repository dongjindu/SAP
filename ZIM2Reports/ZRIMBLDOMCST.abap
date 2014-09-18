*&---------------------------------------------------------------------*
*& Report          ZRIMBLDOMCST                                        *
*&---------------------------------------------------------------------*
*&  프로그램명 : 하역운송비 검토서                                     *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.11                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

PROGRAM  ZRIMBLDOMCST  MESSAGE-ID ZIM
                     LINE-SIZE 105
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

INCLUDE ZRIMBLDOMCSTTOP.
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

*>> 선적내용.
  PERFORM P1000_READ_BLHD.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*>> 하역운송비 산출내역.
  PERFORM P1000_READ_COST.

ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  SET PF-STATUS 'BLCST'.

  CLEAR : W_REMARK, W_SUB_VAT, W_SUB_KRW, W_GRD_TOT.

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
*&      Form  P1000_READ_BLHD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_BLHD .

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
  SELECT SINGLE ZFINDT ZFTBLNO ZFHSCL
           INTO (ST_HEAD-ZFINDT, ST_HEAD-ZFTBLNO, ST_HEAD-ZFHSCL)
           FROM ZTBLINR_TMP
          WHERE ZFBLNO = P_ZFBLNO.

*>  화물종류.
  PERFORM  GET_DD07T_SELECT USING 'ZDHSCL' ST_HEAD-ZFHSCL
                            CHANGING   ST_HEAD-W_HSCL W_SUBRC.
* Forwarder
  IF NOT ST_HEAD-ZFFORD IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ST_HEAD-ZFFORD
                                CHANGING   ST_HEAD-W_FORD.
  ENDIF.
* 운임지불업체.
  IF NOT ST_HEAD-ZFHAYEK IS INITIAL.
    PERFORM  P1000_GET_VENDOR   USING      ST_HEAD-ZFHAYEK
                                CHANGING   ST_HEAD-W_HAYEK.
  ENDIF.

ENDFORM.                    " P1000_READ_BLHD

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
           AND ZFCSQ      <= '10000'.
*           AND ZFCKAMT    NE 0.

  LOOP AT IT_TAB_COST.
    W_TABIX = SY-TABIX.
    SELECT SINGLE ZFCDNM INTO IT_TAB_COST-ZFCDNM
                  FROM ZTIMIMG08
                 WHERE ZFCDTY = '005'
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
  WRITE : /40  '[ 하역운송비 검토서 ]'
               COLOR COL_HEADING INTENSIFIED OFF.

  SKIP 2.
*> HEAD.
  WRITE : '계약번호 : ',           ST_HEAD-ZFREBELN ,
          '          선적차수 : ', ST_HEAD-ZFSHNO.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (10) '반입번호'      CENTERED NO-GAP,
            SY-VLINE NO-GAP, (13) ST_HEAD-ZFTBLNO  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (10) 'B/L No.'        CENTERED NO-GAP,
            SY-VLINE NO-GAP, (25) ST_HEAD-ZFHBLNO  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (10) '입항일자'       CENTERED NO-GAP,
            SY-VLINE NO-GAP, (30) ST_HEAD-ZFRETA   CENTERED NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (10) '선 기 명'  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (50) ST_HEAD-ZFCARNM NO-GAP,
            SY-VLINE NO-GAP, (10) '화물종류'    CENTERED NO-GAP,
            SY-VLINE NO-GAP, (30)  ST_HEAD-W_HSCL  NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP,
       (10) '수    량' CENTERED NO-GAP, SY-VLINE NO-GAP,
       (10) ST_HEAD-ZFPKCN  UNIT ST_HEAD-ZFPKCNM
                         RIGHT-JUSTIFIED NO-GAP NO-ZERO,
       (03) ST_HEAD-ZFPKCNM NO-GAP, SY-VLINE NO-GAP,
       (10) '중    량' CENTERED NO-GAP, SY-VLINE NO-GAP,
       (22) ST_HEAD-ZFNEWT  UNIT ST_HEAD-ZFNEWTM
                         RIGHT-JUSTIFIED NO-GAP,
       (03) ST_HEAD-ZFNEWTM NO-GAP, SY-VLINE NO-GAP,
       (10) '용    적' CENTERED NO-GAP, SY-VLINE NO-GAP,
       (27) ST_HEAD-ZFTOVL  UNIT ST_HEAD-ZFTOVLM
                         RIGHT-JUSTIFIED NO-GAP,
       (03) ST_HEAD-ZFTOVLM NO-GAP, SY-VLINE NO-GAP.
  ULINE.
  SKIP .

*> 산출내역.
  WRITE : '< 하역/운송비 산출내역 >'.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (15) '항    목' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (25) '금    액' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (61) '비    고' CENTERED NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.

  LOOP AT IT_TAB_COST.
    PERFORM P3000_WRITE_COST.
    ULINE.
  ENDLOOP.
*>> 합계.
  WRITE : / SY-VLINE NO-GAP,
       (15) '합     계'  CENTERED NO-GAP,
            SY-VLINE NO-GAP,
       (25) W_SUB_KRW   CURRENCY 'KRW'
                             RIGHT-JUSTIFIED NO-GAP,
            SY-VLINE NO-GAP,
       (61) W_REMARK INPUT ON NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  SKIP.

  W_GRD_TOT =  W_SUB_VAT + W_SUB_KRW.

*> 업체별 합계.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (41) '업    체' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (20) '공급가액' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (19) '부 가 세' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (20) '계'       CENTERED NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (10) '하    역' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (30) ST_HEAD-W_HAYEK NO-GAP,
            SY-VLINE NO-GAP,
       (20) W_SUB_KRW CURRENCY 'KRW' RIGHT-JUSTIFIED NO-GAP,
            SY-VLINE NO-GAP,
       (19) W_SUB_VAT CURRENCY 'KRW' RIGHT-JUSTIFIED NO-GAP,
            SY-VLINE NO-GAP,
       (20) W_GRD_TOT CURRENCY 'KRW' RIGHT-JUSTIFIED NO-GAP,
            SY-VLINE NO-GAP.
  ULINE.
  WRITE : / SY-VLINE NO-GAP, (10) '운    송' CENTERED NO-GAP,
            SY-VLINE NO-GAP, (30) ST_HEAD-W_FORD NO-GAP,
            SY-VLINE NO-GAP,
       (20) '0 ' RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
       (19) '0 ' RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP,
       (20) '0 ' RIGHT-JUSTIFIED NO-GAP, SY-VLINE NO-GAP.
  ULINE.
  SKIP .

*>> 결재라인.
*  WRITE :  20(51) SY-ULINE,
*          /20 SY-VLINE NO-GAP, (04) '결'     CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '직  원' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '과  장' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '부  장' CENTERED NO-GAP,
*              SY-VLINE NO-GAP,
*          /20 SY-VLINE NO-GAP, (4) '  ' CENTERED NO-GAP,
*       25(46) SY-ULINE NO-GAP,
*          /20 SY-VLINE NO-GAP, (04) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP,
*          /20 SY-VLINE NO-GAP, (04) '재' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*              SY-VLINE NO-GAP,
*      /20(51) SY-ULINE.
*  SKIP 2 .
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
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_COST
*&---------------------------------------------------------------------*
FORM P3000_WRITE_COST .
  WRITE : / SY-VLINE NO-GAP,
       (15) IT_TAB_COST-ZFCDNM CENTERED NO-GAP,
            SY-VLINE NO-GAP,
       (25) IT_TAB_COST-ZFCKAMT   CURRENCY 'KRW'
                                   RIGHT-JUSTIFIED NO-GAP,
            SY-VLINE NO-GAP,
       (61)  W_REMARK INPUT ON NO-GAP,
            SY-VLINE NO-GAP.
  W_SUB_VAT = W_SUB_VAT + IT_TAB_COST-ZFVAT.
  W_SUB_KRW = W_SUB_KRW + IT_TAB_COST-ZFCKAMT.
  CLEAR IT_TAB_COST.

ENDFORM.                    " P3000_WRITE_COST
