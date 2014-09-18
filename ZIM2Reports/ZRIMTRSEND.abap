*&---------------------------------------------------------------------*
*& Report           ZRIMTRSEND                                         *
*&---------------------------------------------------------------------*
*&  프로그램명 : 발송증표(보세창고출고)                                *
*&      작성자 : 정승연 INFOLINK Ltd.                                  *
*&      작성일 : 2002.11.05                                            *
*&     적용회사: 한수원.
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

PROGRAM  ZRIMTRSEND  MESSAGE-ID ZIM
                     LINE-SIZE 139
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* Include
*-----------------------------------------------------------------------

INCLUDE   ZRIMTRSENDTOP.
INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*-----------------------------------------------------------------------
* Selection Screen .
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
*>> 검색조건
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS :   S_ZFTRNO  FOR  ZTTRHD-ZFTRNO.

SELECTION-SCREEN END OF BLOCK B1.

*---------------------------------------------------------------------*
* EVENT INITIALIZATION.
*---------------------------------------------------------------------*
INITIALIZATION.                                 " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.
  SET TITLEBAR 'TRPL'.


*---------------------------------------------------------------------*
* EVENT TOP-OF-PAGE.
*---------------------------------------------------------------------*
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...

*---------------------------------------------------------------------*
* EVENT START-OF-SELECTION.
*---------------------------------------------------------------------*
START-OF-SELECTION.


  PERFORM   P1000_READ_TEXT           USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE .
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
  CLEAR : IT_TAB.
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
  CLEAR IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.


ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.

    SELECT SINGLE * FROM ZTTRHD
                   WHERE ZFTRNO = IT_TAB-ZFTRNO.

    SKIP 2.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
    WRITE : /60  '[ 발   송   증   표 ]'
                 COLOR COL_HEADING INTENSIFIED OFF.

    SKIP 2.
    WRITE : ' 수송지시번호 : ' NO-GAP, ZTTRHD-ZFTRNO NO-GAP,
             104 SY-ULINE,
            /104 SY-VLINE NO-GAP, (4) '결'      CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) '직  원' CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) '과  장' CENTERED NO-GAP,
*                 SY-VLINE NO-GAP, (14) '부  장' CENTERED NO-GAP,
                 SY-VLINE NO-GAP,
            / ' 수   령   처 : ' NO-GAP, IT_TAB-W_WERKS NO-GAP,
             104 SY-VLINE NO-GAP, (4) '  ' CENTERED NO-GAP,
             109 SY-ULINE NO-GAP,
            /104 SY-VLINE NO-GAP, (4) ' '  CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) ' ' CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) ' ' CENTERED NO-GAP,
*                 SY-VLINE NO-GAP, (14) ' ' CENTERED NO-GAP,
                 SY-VLINE NO-GAP,
           / ' 계 약  번 호 : ' NO-GAP, IT_TAB-EBELN NO-GAP,
             104 SY-VLINE NO-GAP, (4) ' '  CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) ' ' CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) ' ' CENTERED NO-GAP,
*                 SY-VLINE NO-GAP, (14) ' ' CENTERED NO-GAP,
                 SY-VLINE NO-GAP,
            /104 SY-VLINE NO-GAP, (4) '재'  CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
                 SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
*                 SY-VLINE NO-GAP, (14) '  ' CENTERED NO-GAP,
                 SY-VLINE NO-GAP,
            / ' 수 송  기 간 : ', ZTTRHD-ZFGIDT,
              ' 부터 ', ZTTRHD-ZFDRDT, ' 까지',
            104 SY-ULINE.
    SKIP 2 .
  WRITE : ' 수송 품목 명세서 '.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE NO-GAP, (08) '일련번호'       CENTERED NO-GAP,
*            SY-VLINE NO-GAP, (10) '계약 번호'      CENTERED NO-GAP,
            SY-VLINE NO-GAP, (24) 'House B/L No.'  CENTERED NO-GAP,
            SY-VLINE NO-GAP, (20) '자재식별번호'   CENTERED NO-GAP,
            SY-VLINE NO-GAP, (40) '품명 및 규격'   CENTERED NO-GAP,
            SY-VLINE NO-GAP, (20) '출고    수량'   CENTERED NO-GAP,
            SY-VLINE NO-GAP, (20) '중        량'   CENTERED NO-GAP,
            SY-VLINE.

  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------*
FORM P1000_READ_TEXT USING    P_W_ERR_CHK.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
           FROM ZTTRIT
          WHERE ZFTRNO IN S_ZFTRNO.

  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.
    MESSAGE S738.
    EXIT.
  ENDIF.

  LOOP AT IT_TAB.
    W_TABIX  = SY-TABIX.
*> 수송처.
    IF NOT IT_TAB-WERKS IS INITIAL.
      SELECT SINGLE NAME1 INTO IT_TAB-W_WERKS
                          FROM T001W
                         WHERE WERKS = IT_TAB-WERKS.
    ENDIF.

*>> House B/L, B/L 중량 가져오기.
    IF NOT IT_TAB-ZFBLNO IS INITIAL.
       SELECT SINGLE ZFHBLNO ZFNEWT ZFNEWTM
              INTO (IT_TAB-ZFHBLNO, IT_TAB-ZFNEWT, IT_TAB-ZFNEWTM)
              FROM ZTBL
             WHERE ZFBLNO = IT_TAB-ZFBLNO.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.

  ENDLOOP.
ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE.

  SET PF-STATUS 'TRPL'.
  SORT IT_TAB BY ZFTRNO WERKS EBELN ZFBLNO MATNR.

  W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.
  LOOP AT IT_TAB.
    ON CHANGE OF IT_TAB-ZFTRNO
              OR IT_TAB-WERKS
              OR IT_TAB-EBELN.
      IF SY-TABIX NE 1.
        PERFORM P3000_SUB_TOTAL.
        PERFORM   P3000_TAIL_WRITE.
        ADD 1 TO W_PAGE.
        CLEAR W_LINE.
        REFRESH IT_TOT.
        NEW-PAGE.
      ENDIF.
    ENDON.
    W_LINE = W_LINE + 1.
    PERFORM P3000_LINE_WRITE.
  ENDLOOP.

  PERFORM P3000_SUB_TOTAL.
  PERFORM   P3000_TAIL_WRITE.

ENDFORM.                    " P3000_DATA_WRITE
*&----------------------------------------------------------------------
*&      Form  RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE = 1.
  W_LINE = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE .

ENDFORM.                    " RESET_LIST
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  TEMP = W_LINE MOD 2.
  IF TEMP EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ENDIF.

  WRITE : / SY-VLINE NO-GAP, (08) W_LINE RIGHT-JUSTIFIED NO-GAP,
*            SY-VLINE NO-GAP, (10) IT_TAB-EBELN     NO-GAP,
            SY-VLINE NO-GAP, (24) IT_TAB-ZFHBLNO   NO-GAP,
            SY-VLINE NO-GAP, (20) IT_TAB-MATNR     NO-GAP,
            SY-VLINE NO-GAP, (40) IT_TAB-TXZ01     NO-GAP,
            SY-VLINE NO-GAP,
            (17) IT_TAB-GIMENGE UNIT IT_TAB-MEINS
                                  RIGHT-JUSTIFIED    NO-GAP,
            (03) IT_TAB-MEINS                        NO-GAP,
            SY-VLINE NO-GAP,
            (17) IT_TAB-ZFNEWT  UNIT IT_TAB-ZFNEWTM INPUT ON
                                  RIGHT-JUSTIFIED    NO-GAP,
            (03) IT_TAB-ZFNEWTM                      NO-GAP,
            SY-VLINE NO-GAP.
 ULINE.

  CLEAR IT_TOT.                       " TOTAL.
  MOVE : IT_TAB-MEINS    TO IT_TOT-MEINS,
         IT_TAB-GIMENGE  TO IT_TOT-GIMENGE.
  COLLECT IT_TOT.

ENDFORM.                    " P3000_LINE_WRITE
*&----------------------------------------------------------------------
*&      Form P3000_SUB_TOTAL
*&----------------------------------------------------------------------
FORM P3000_SUB_TOTAL.

    FORMAT RESET.

    LOOP AT IT_TOT.
    IF SY-TABIX EQ '1'.
       WRITE : SY-VLINE NO-GAP, (08) '합   계' CENTERED NO-GAP.
    ELSE.
      WRITE : SY-VLINE NO-GAP.
    ENDIF.
      WRITE : 98(17) IT_TOT-GIMENGE  UNIT IT_TOT-MEINS
                              RIGHT-JUSTIFIED NO-GAP,
                (03) IT_TOT-MEINS NO-GAP, 139 SY-VLINE.
      NEW-LINE.
    ENDLOOP.
    WRITE : SY-ULINE.

ENDFORM.                    " SUB_TOTAL
*&---------------------------------------------------------------------*
*&      Form  P3000_TAIL_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_TAIL_WRITE.

 SKIP 2.
* WRITE :   ' 총 포장수 : ',
*          (13) ZTTRHD-ZFPKCN UNIT ZTTRHD-ZFPKCNM NO-ZERO
*                  RIGHT-JUSTIFIED , ZTTRHD-ZFPKCNM,
*         / ' 총 중  량 : ',
*          (13) ZTTRHD-ZFTOWT UNIT ZTTRHD-ZFTOWTM NO-ZERO
*                  RIGHT-JUSTIFIED , ZTTRHD-ZFTOWTM,
*         / ' 총 용  량 : ',
*          (13) ZTTRHD-ZFTOVL UNIT ZTTRHD-ZFTOVLM NO-ZERO
*                  RIGHT-JUSTIFIED , ZTTRHD-ZFTOVLM.
* SKIP 2.
 WRITE : ' 위와 같이 수송된 품목에 대한 내역을 확안합니다.'.
 SKIP.
 WRITE : ' 수령자 :                ',
         ' 소속 :               ',
         ' 성명 :               '.

ENDFORM.                    " P3000_TAIL_WRITE
