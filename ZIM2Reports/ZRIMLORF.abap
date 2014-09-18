*&---------------------------------------------------------------------*
*& Report  ZRIMLORF                                                    *
*&---------------------------------------------------------------------*
*&ABAP Name : ZRIMLORF                                                 *
*&Created by: 나신호 INFOLINK.Ltd                                      *
*&Created on: 07/20/2000                                               *
*&Version   : 1.0                                                      *
*&---------------------------------------------------------------------*
* 재원별 L/C 거래실적을 보여준다.
* Report ZRIMLORC에서 SUBMIT 으로 호출 된다.
*&---------------------------------------------------------------------*

REPORT  ZRIMLORF       NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 125
                       LINE-COUNT 65.

TABLES : ZTREQHD,                      " 수입의뢰 Header
         ZTREQST.                      " 수입의뢰 상태(Status)

DATA : BEGIN OF IT_TAB1 OCCURS 0,
               ZFJEWGB     LIKE ZTREQHD-ZFJEWGB,
               ZFMATGB     LIKE ZTREQHD-ZFMATGB,
               ZFREQNO     LIKE ZTREQHD-ZFREQNO.
DATA : END   OF IT_TAB1.

DATA : BEGIN OF IT_TAB2 OCCURS 0,
               ZFJEWGB     LIKE ZTREQHD-ZFJEWGB,
               COUNT1(5)   TYPE  I,
               OPAMT1      LIKE ZTREQST-ZFOPAMT,      " 원자?
               COUNT2(5)   TYPE  I,
               OPAMT2      LIKE ZTREQST-ZFOPAMT,      " 시설?
               COUNT3(5)   TYPE  I,
               OPAMT3      LIKE ZTREQST-ZFOPAMT,      " 상?
               COUNTS(5)   TYPE  I,
               OPAMTS      LIKE ZTREQST-ZFOPAMT.      " 합?
DATA : END   OF IT_TAB2.

DATA : W_SUBRC  LIKE  SY-SUBRC,
       W_AMDNO  LIKE  ZTREQST-ZFAMDNO,
       W_OPAMT  LIKE  ZTREQST-ZFOPAMT,
       W_CNT(1),
       W_TEXT(21)  TYPE  C.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
   SELECT-OPTIONS: S_WERKS    FOR ZTREQHD-ZFWERKS,   "Plant
                   S_OPNDT    FOR ZTREQST-ZFOPNDT OBLIGATORY.   "개설?
   SELECTION-SCREEN SKIP.
SELECTION-SCREEN END   OF BLOCK B1.

START-OF-SELECTION.
  PERFORM P1000_READ_DATA.
  IF W_SUBRC = 4.
     MESSAGE S191 WITH '수입의뢰문서'.  EXIT.
  ENDIF.

  PERFORM P1000_CHECK_DATA.

  PERFORM P1000_WRITE_DATA.

TOP-OF-PAGE.
  PERFORM P1000_TOP_PAGE.


*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA.
   SELECT B~ZFREQNO A~ZFJEWGB A~ZFMATGB
    INTO (IT_TAB1-ZFREQNO, IT_TAB1-ZFJEWGB, IT_TAB1-ZFMATGB)
     FROM ZTREQST AS B INNER JOIN ZTREQHD AS A
       ON B~ZFREQNO = A~ZFREQNO
    WHERE B~ZFOPNDT IN S_OPNDT AND
          B~ZFAMDNO = '00000' AND
          A~ZFWERKS IN S_WERKS.
    APPEND IT_TAB1.  CLEAR IT_TAB1.
   ENDSELECT.

   IF SY-SUBRC <> 0.  W_SUBRC = 4.   EXIT.    ENDIF.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_CHECK_DATA.
  SORT IT_TAB1.

  LOOP AT IT_TAB1.
    SELECT MAX( ZFAMDNO ) INTO W_AMDNO FROM ZTREQST
     WHERE ZFREQNO = IT_TAB1-ZFREQNO AND ZFOPNDT > '1'.

    SELECT SINGLE ZFUSDAM  INTO W_OPAMT FROM ZTREQST
     WHERE ZFREQNO = IT_TAB1-ZFREQNO AND ZFAMDNO = W_AMDNO.

    MOVE IT_TAB1-ZFJEWGB TO  IT_TAB2-ZFJEWGB.

    CASE IT_TAB1-ZFMATGB.
      WHEN '1' OR '2' OR '3'.
           MOVE 1           TO  IT_TAB2-COUNT1.
           MOVE W_OPAMT     TO  IT_TAB2-OPAMT1.
      WHEN '4'.
           MOVE 1           TO  IT_TAB2-COUNT2.
           MOVE W_OPAMT     TO  IT_TAB2-OPAMT2.
      WHEN '5'.
           MOVE 1           TO  IT_TAB2-COUNT3.
           MOVE W_OPAMT     TO  IT_TAB2-OPAMT3.
    ENDCASE.

    MOVE 1           TO  IT_TAB2-COUNTS.
    MOVE W_OPAMT     TO  IT_TAB2-OPAMTS.

    COLLECT IT_TAB2.  CLEAR IT_TAB2.

  ENDLOOP.

ENDFORM.                    " P1000_CHECK_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_DATA.
  SORT IT_TAB2.
  LOOP AT IT_TAB2.
    IF W_CNT = 1.
       W_CNT = 2.   FORMAT COLOR 2 INTENSIFIED OFF.
    ELSE.
       W_CNT = 1.   FORMAT COLOR 2 INTENSIFIED ON.
    ENDIF.

    CLEAR : W_TEXT.
    CASE IT_TAB2-ZFJEWGB.
      WHEN '1'.    W_TEXT = '자기자금'.
      WHEN '2'.    W_TEXT = '외화대출'.
      WHEN '3'.    W_TEXT = '연지급(USANCE)'.
      WHEN '4'.    W_TEXT = '분할지급수입'.
      WHEN '5'.    W_TEXT = '금융리스'.
      WHEN '6'.    W_TEXT = '운용리스'.
      WHEN '7'.    W_TEXT = '외화사채'.
      WHEN '8'.    W_TEXT = '기    타'.
      WHEN OTHERS. W_TEXT = '*******'.
    ENDCASE.

    WRITE:/'|' NO-GAP,
           (15) W_TEXT NO-GAP, '|',
           (05) IT_TAB2-COUNT1 NO-GAP, '|',
           (18) IT_TAB2-OPAMT1 CURRENCY 'USD' NO-GAP, '|',
           (05) IT_TAB2-COUNT2 NO-GAP, '|',
           (18) IT_TAB2-OPAMT2 CURRENCY 'USD' NO-GAP, '|',
           (05) IT_TAB2-COUNT3 NO-GAP, '|',
           (18) IT_TAB2-OPAMT3 CURRENCY 'USD' NO-GAP, '|',
           (05) IT_TAB2-COUNTS NO-GAP, '|',
           (18) IT_TAB2-OPAMTS CURRENCY 'USD' NO-GAP, '|'.

    AT LAST.
       SUM.
       W_TEXT = '     총    계'.
       ULINE.
       FORMAT COLOR 3 INTENSIFIED OFF.
       WRITE:/'|' NO-GAP,
              (15) W_TEXT NO-GAP, '|',
              (05) IT_TAB2-COUNT1 NO-GAP, '|',
              (18) IT_TAB2-OPAMT1 CURRENCY 'USD' NO-GAP, '|',
              (05) IT_TAB2-COUNT2 NO-GAP, '|',
              (18) IT_TAB2-OPAMT2 CURRENCY 'USD' NO-GAP, '|',
              (05) IT_TAB2-COUNT3 NO-GAP, '|',
              (18) IT_TAB2-OPAMT3 CURRENCY 'USD' NO-GAP, '|',
              (05) IT_TAB2-COUNTS NO-GAP, '|',
              (18) IT_TAB2-OPAMTS CURRENCY 'USD' NO-GAP, '|'.
       ULINE.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " P1000_WRITE_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_TOP_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TOP_PAGE.
  SKIP 1.
  WRITE:/45 '   재 원 별 L/C 개 설 실 적   ' COLOR 1.
  WRITE:/105 'DATE :', SY-DATUM.
  WRITE:/105 'PAGE :', SY-PAGNO.

  WRITE:/6 '개설기간 :', S_OPNDT-LOW, '-', S_OPNDT-HIGH,
        90 'Currency : USD'.

  ULINE.
  FORMAT COLOR 1 INTENSIFIED OFF.
  WRITE:/'|' NO-GAP,
         (15) '     구    분' NO-GAP, '|',
         (05) ' 건수' NO-GAP, '|',
         (18) '원 자 재 금 액 ' RIGHT-JUSTIFIED NO-GAP, '|',
         (05) ' 건수' NO-GAP, '|',
         (18) '시 설 재 금 액 ' RIGHT-JUSTIFIED NO-GAP, '|',
         (05) ' 건수' NO-GAP, '|',
         (18) '상  품  금  액 ' RIGHT-JUSTIFIED NO-GAP, '|',
         (05) ' 건수' NO-GAP, '|',
         (18) '소  계  금 액 ' RIGHT-JUSTIFIED NO-GAP, '|'.
  ULINE.

ENDFORM.                    " P1000_TOP_PAGE
