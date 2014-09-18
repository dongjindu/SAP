*&---------------------------------------------------------------------*
*& Report  ZRIMBWPRT                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 외자적송명세서                                        *
*&      작성자 : 이채경 INFOLINK Ltd.                                  *
*&      작성일 : 2001.08.25                                            *
*$     적용회사: LG 화학
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMBWPRT   MESSAGE-ID ZIM
                     LINE-SIZE 120
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------
INCLUDE   ZRIMBWLSTTOP.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: P_IVNO  LIKE ZTBWHD-ZFIVNO,
              P_GISEQ LIKE ZTBWHD-ZFGISEQ.

SELECTION-SCREEN END OF BLOCK B1.
* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.
* Title Text Write
*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

* 권한 검증 함수.
*   PERFORM   P2000_AUTHORITY_CHECK     USING   W_ERR_CHK.
*   IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*  테이블 SELECT
  PERFORM   P1000_GET_ZTBW      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'. MESSAGE S738.   EXIT.    ENDIF.
* 레포트 Write
  PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.  EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
*AT USER-COMMAND.
*   CASE SY-UCOMM.


*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'BWPRT'.          " TITLE BAR

ENDFORM.                    " P2000_SET_PARAMETER
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
      MESSAGE S960 WITH SY-UNAME 'Request Release Transaction'.
      W_ERR_CHK = 'Y'.   EXIT.
  ENDIF.

ENDFORM.                    " P2000_AUTHORITY_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_ZTBW
*&---------------------------------------------------------------------*
FORM P1000_GET_ZTBW   USING   W_ERR_CHK.

  W_ERR_CHK = 'N'.
  SELECT SINGLE *
     FROM  ZTBWHD
     WHERE ZFIVNO   = P_IVNO
       AND ZFGISEQ  = P_GISEQ.
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'. EXIT.
  ENDIF.
  SELECT SINGLE *
      FROM LFA1
      WHERE LIFNR = ZTBWHD-ZFTRCO.
  REFRESH IT_BWIT.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BWIT
           FROM ZTBWIT
           WHERE ZFIVNO  = P_IVNO
             AND ZFGISEQ = P_GISEQ.
  PERFORM   P1000_GET_TEXT.

ENDFORM.                    " P1000_GET_ZTBW.
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  SET PF-STATUS 'BWPRT'.           " GUI STATUS SETTING
  SET  TITLEBAR 'BWPRT'.           " GUI TITLE SETTING..
  PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  SKIP 2.

  WRITE:/45  '외 자  적 송  명 세 서'.
  WRITE:/45  '----------------------'.
  ULINE AT 70(51).
  WRITE:/70 SY-VLINE,72 ' 결제'  CENTERED,
         80 SY-VLINE,82 ' 기안'  CENTERED,
         90 SY-VLINE,92 ' 심의'  CENTERED,
        100 SY-VLINE,102 ' 확정' CENTERED,
        110 SY-VLINE,112 ' 고객' CENTERED,
        120 SY-VLINE.
  ULINE AT /70(51).

  WRITE:/70 SY-VLINE,72 ' 통관' CENTERED,
         80 SY-VLINE,82 '',
         90 SY-VLINE,92 '',
        100 SY-VLINE,102 '',
        110 SY-VLINE,112 '',
        120 SY-VLINE.
  ULINE AT /70(51).

  WRITE:/70 SY-VLINE,72 ' 인수' CENTERED,
         80 SY-VLINE,82 '',
         90 SY-VLINE,92 '',
        100 SY-VLINE,102 '',
        110 SY-VLINE,112 '',
        120 SY-VLINE.
  ULINE AT /70(51).

  WRITE:/70 SY-VLINE,72 ' 일자' CENTERED,
         80 SY-VLINE,82 '',
         90 SY-VLINE,92 '',
        100 SY-VLINE,102 '',
        110 SY-VLINE,112 '',
        120 SY-VLINE.
  ULINE AT /70(51).

  SKIP 2.
  WRITE:/ '적송일:',ZTBWHD-ZFGIDT,105 '통관부서보관용'.
  WRITE :/ SY-ULINE(120).
  WRITE :/ SY-VLINE,(03) 'No' CENTERED,
           SY-VLINE,(12) 'P/O No'CENTERED,
           SY-VLINE,(16) 'B/L No'CENTERED,
           SY-VLINE,(10) '자재코드'CENTERED,
           SY-VLINE,(35) '품명 및 규격'CENTERED ,
           SY-VLINE,(04) '단위' CENTERED,
           SY-VLINE,(18) '출고수량' CENTERED,
           SY-VLINE.
  WRITE:/ SY-ULINE(120).

  PERFORM P2000_IT_LINE_WRITE.
  SKIP 1.
  WRITE :/ SY-ULINE(120).
  WRITE :/ SY-VLINE,'차량번호'CENTERED,
           SY-VLINE,ZTBWHD-ZFCARNO CENTERED,
           SY-VLINE,'운송사',SY-VLINE,LFA1-NAME1 CENTERED,
           SY-VLINE,'운전기사 확인',SY-VLINE,ZTBWHD-ZFDRVNM CENTERED,
           120 SY-VLINE.
  WRITE :/ SY-ULINE(120).
  SKIP 1.
  ULINE AT /70(51).
  WRITE :/20 '상기와 같이 송품하오니 확인하여 주시기 바랍니다.',
          70 SY-VLINE, 72 '비 고',
          79 SY-VLINE,81 ZTBWHD-ZFRMK1+0(38),120 SY-VLINE.
  ULINE AT /70(51).

  CLEAR  T001W.
  SELECT SINGLE *
         FROM  T001W
         WHERE  WERKS = ZTBWHD-WERKS.

  ULINE AT /1(65).
  WRITE:  70 SY-VLINE,72 '',79 SY-VLINE,120 SY-VLINE.
  WRITE:/ SY-VLINE NO-GAP, '발송자' NO-GAP,
          SY-VLINE NO-GAP, ZTBWHD-ZFSENDER NO-GAP,
          SY-VLINE NO-GAP, '도착시간'NO-GAP,
          SY-VLINE NO-GAP, '    시', '   분'NO-GAP,
          SY-VLINE NO-GAP, '출발시간'NO-GAP,
          SY-VLINE NO-GAP, '    시', '   분'NO-GAP,SY-VLINE.
  WRITE:  70 SY-VLINE,72 '도착지',79 SY-VLINE,81 T001W-NAME1,
          120 SY-VLINE.
  ULINE AT /1(65).
  WRITE:  70 SY-VLINE,72 ' ',79 SY-VLINE,120 SY-VLINE.
  WRITE:/ SY-VLINE NO-GAP, '인수자' NO-GAP,
          SY-VLINE NO-GAP, (12)''   NO-GAP,
          SY-VLINE NO-GAP, '도착시간'NO-GAP,
          SY-VLINE NO-GAP, '    시', '   분'NO-GAP,
          SY-VLINE NO-GAP, '출발시간'NO-GAP,
          SY-VLINE NO-GAP, '    시', '   분'NO-GAP,SY-VLINE.
  WRITE:  70 SY-VLINE,72 '',79 SY-VLINE,120 SY-VLINE.
  ULINE AT /1(65).
  ULINE AT 70(51).

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

*  REFRESH IT_TAB_DOWN.
*  LOOP AT IT_TAB.
*    CLEAR IT_TAB_DOWN.
*    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
*    WRITE : IT_TAB-MENGE   UNIT     IT_TAB-MEINS TO IT_TAB_DOWN-MENGE,
*        IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD TO IT_TAB_DOWN-ZFUSDAM,
*         IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS TO IT_TAB_DOWN-ZFOPAMT.
*    APPEND IT_TAB_DOWN.
*  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  P2000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P2000_IT_LINE_WRITE.

  LOOP AT IT_BWIT WHERE ZFIVNO  = ZTBWHD-ZFIVNO
                    AND ZFGISEQ = ZTBWHD-ZFGISEQ.
       W_TABIX = SY-TABIX.
       PERFORM  P3000_IT_LINE_WRITE.
  ENDLOOP.

ENDFORM.                    " P2000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_IT_LINE_WRITE.

  CLEAR: ZTBL.
  SELECT  SINGLE *
     FROM ZTBL
    WHERE ZFBLNO = IT_BWIT-ZFBLNO.
  WRITE :/  SY-VLINE,(03) IT_BWIT-ZFIVDNO+2(3) ,
            SY-VLINE,(12) IT_BWIT-W_EBELN ,
            SY-VLINE,(16) ZTBL-ZFHBLNO,
            SY-VLINE,(10) IT_BWIT-MATNR ,      " 자재번호'
            SY-VLINE,(35) IT_BWIT-TXZ01 ,      " 내역,
            SY-VLINE,(04) IT_BWIT-MEINS ,
            SY-VLINE,(18) IT_BWIT-GIMENGE
                          UNIT IT_BWIT-MEINS , " 출고수량
            SY-VLINE.
  WRITE:/ SY-ULINE(120).

ENDFORM.                    " P3000_IT_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_TEXT
*&---------------------------------------------------------------------*
FORM P1000_GET_TEXT.

  LOOP AT IT_BWIT.
      W_TABIX = SY-TABIX.

      IF NOT ZTBWHD-ZFSHNO IS INITIAL.
          CONCATENATE IT_BWIT-EBELN '-' ZTBWHD-ZFSHNO
                      INTO IT_BWIT-W_EBELN.
      ELSE.
          MOVE  IT_BWIT-EBELN  TO  IT_BWIT-W_EBELN.
      ENDIF.

      MODIFY  IT_BWIT INDEX W_TABIX.

  ENDLOOP.

ENDFORM.                    " P1000_GET_TEXT
