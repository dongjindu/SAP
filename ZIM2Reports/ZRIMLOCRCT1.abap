*&---------------------------------------------------------------------*
*& Report  ZRIMLOCRCT1
*&---------------------------------------------------------------------*
*&  프로그램명 : 내국신용장 물품수령 증명서                            *
*&      작성자 : 김영광 LG-EDS                                         *
*&      작성일 : 2001.07.16                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : 내국신용장물품수령증명서                              *
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*

REPORT  ZRIMLOCRCT1   MESSAGE-ID ZIM
                      LINE-SIZE 120
                      NO STANDARD PAGE HEADING.

TABLES: ZTRED, ZTREDSG1.

*-----------------------------------------------------------------------
* 물품수령 증명서 INTERNAL TABLE
*-----------------------------------------------------------------------

DATA : IT_TAB1   LIKE ZTREDSG1 OCCURS 0 WITH HEADER LINE.

DATA : W_ERR_CHK(1)      TYPE C,
       W_SUBRC           LIKE SY-UCOMM,
       W_TEM1(27).

*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 2.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_REDNO  LIKE ZTRED-ZFREDNO
                MEMORY ID  ZPREDNO. "OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                     " 초기값 SETTING
    SET  TITLEBAR 'ZIM24'.          " TITLE BAR

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.
*  테이블 SELECT
    PERFORM   P1000_READ_DATA USING W_ERR_CHK.

    IF W_ERR_CHK EQ 'Y'.
        MESSAGE S738.
        EXIT.
    ENDIF.

* 레포트 Write
    PERFORM   P3000_DATA_WRITE.

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  W_ERR_CHK        text
*----------------------------------------------------------------------*
FORM P1000_READ_DATA USING W_ERR_CHK.
** ZTRED
*    REFRESH IT_TAB.

    SELECT SINGLE *
      FROM ZTRED
     WHERE ZFREDNO = P_REDNO.

    IF SY-SUBRC NE 0.               " Not Found?
        W_ERR_CHK = 'Y'.
    ENDIF.

** ZTREDSG1
    REFRESH IT_TAB1.

    SELECT *
      FROM ZTREDSG1
     WHERE ZFREDNO = ZTRED-ZFREDNO .

    IF SY-SUBRC EQ 0.
        CLEAR IT_TAB1.
        MOVE-CORRESPONDING ZTREDSG1 TO IT_TAB1.
        APPEND IT_TAB1.
    ENDIF.

    ENDSELECT.

ENDFORM.                    " P1000_READ_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE .
    SET PF-STATUS  'PF24'.
    SET TITLEBAR   'ZIM24'.          " TITLE BAR

    PERFORM P3000_LINE_WRITE.

ENDFORM.                    " P3000_DATA_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.
    SKIP 2.
    WRITE : 53 '(주) L G  화 학'.

    WRITE : / SY-ULINE.
    WRITE : / SY-VLINE,
   35  '내   국   신   용   장   물   품   수   령   증   명  서',
              120 SY-VLINE,
            / SY-VLINE,                              120 SY-VLINE,
            / SY-VLINE, 40 '( 발급번호:', ZTRED-ZFISNO , 82 ' )',
              120 SY-VLINE.
    WRITE : / SY-ULINE.

    WRITE : / SY-VLINE, 6 '물품공급자', 20 SY-VLINE, ZTRED-ZFSCONM,
              120 SY-VLINE,
            / SY-ULINE,
            / SY-VLINE, 5 '물품인수일자', 20 SY-VLINE, ZTRED-ZFREVDT,
              70 SY-VLINE, 74 '물품인수금액', 90 SY-VLINE,
              ZTRED-ZFREAMFC,
              ZTRED-ZFREAMF CURRENCY ZTRED-ZFREAMFC
                                            LEFT-JUSTIFIED,
              120 SY-VLINE,
            / SY-ULINE,
            / SY-VLINE, 5 '인수물품명세', 20 SY-VLINE,
              32  '품  명', 49  SY-VLINE,
              56  '규  격', 68  SY-VLINE,
              74  '수  량', 85  SY-VLINE,
              91  '단  가', 101 SY-VLINE,
              108 '금  액', 120 SY-VLINE,
            / SY-VLINE,   20  SY-ULINE(120).

    LOOP AT IT_TAB1.
        WRITE:/ SY-VLINE, 20 SY-VLINE,
               IT_TAB1-MAKTX,   49 SY-VLINE,
               50  IT_TAB1-ZFGOSD1, 68 SY-VLINE,
               69  IT_TAB1-ZFQUN UNIT IT_TAB1-ZFQUNM, 85 SY-VLINE,
               86  IT_TAB1-NETPR  CURRENCY IT_TAB1-ZFNETPRC,
                                   101 SY-VLINE,
               102 IT_TAB1-ZFREAM CURRENCY IT_TAB1-ZFREAMC,
               120 SY-VLINE,
              / SY-VLINE, 20 SY-ULINE(120), 120 SY-VLINE.

        AT LAST.
            SUM.
            WRITE: / SY-VLINE, 20 SY-VLINE,
                   94 'TOTAL: ',IT_TAB1-ZFREAM
                                 CURRENCY IT_TAB1-ZFREAMC,
                   120 SY-VLINE.
        ENDAT.
    ENDLOOP.

    WRITE:/ SY-ULINE,
          / SY-VLINE, 45 '관  련  내  국  신  용  장  내  용',
            120 SY-VLINE,
          / SY-ULINE,
          / SY-VLINE, 6 '개설은행', 20 SY-VLINE, 30 '신용장번호',
            50 SY-VLINE, 62 '금   액', 80 SY-VLINE, 87 '인도기일',
            100 SY-VLINE, 106 '유효기일', 120 SY-VLINE,
          / SY-ULINE,
          / SY-VLINE, ZTRED-ZFOBNM, 20 SY-VLINE, ZTRED-ZFLLCON,
            50 SY-VLINE, ZTRED-ZFOPAMFC,
            ZTRED-ZFOPAMF CURRENCY ZTRED-ZFOPAMFC LEFT-JUSTIFIED,
            80 SY-VLINE, 86 ZTRED-ZFGDDT, 100 SY-VLINE,
            105 ZTRED-ZFEXDT,
            120 SY-VLINE,
          / SY-ULINE,
          / SY-VLINE, 7 '기  타', 20 SY-VLINE, 120 SY-VLINE.

          IF NOT ZTRED-ZFREMK1 IS INITIAL.
              WRITE: 20 SY-VLINE, 21 ZTRED-ZFREMK1, 120 SY-VLINE.
          ENDIF.

          IF NOT ZTRED-ZFREMK2 IS INITIAL.
              WRITE: 20 SY-VLINE, 21 ZTRED-ZFREMK2, 120 SY-VLINE.
          ENDIF.

          IF NOT ZTRED-ZFREMK3 IS INITIAL.
              WRITE: 20 SY-VLINE, 21 ZTRED-ZFREMK3, 120 SY-VLINE.
          ENDIF.

          IF NOT ZTRED-ZFREMK4 IS INITIAL.
              WRITE: 20 SY-VLINE, 21 ZTRED-ZFREMK4, 120 SY-VLINE.
          ENDIF.

          IF NOT ZTRED-ZFREMK5 IS INITIAL.
              WRITE: 20 SY-VLINE, 21 ZTRED-ZFREMK5, 120 SY-VLINE.
          ENDIF.

          WRITE:/ SY-ULINE, SY-VLINE, 120 SY-VLINE,
                / SY-VLINE, 8 '위 물품을 틀림없이 수령하였음을 증명함.',
                  120 SY-VLINE,
                / SY-VLINE, 120 SY-VLINE,
                / SY-VLINE, 120 SY-VLINE,
                / SY-VLINE, 60 '발급일자', 72 ZTRED-ZFISUDT(4),
                                           76 '년',
                                           80 ZTRED-ZFISUDT+4(2),
                                           82 '월',
                                           85 ZTRED-ZFISUDT+6(2),
                                           87 '일',
                  120 SY-VLINE,
                / SY-VLINE, 120 SY-VLINE,
                / SY-VLINE, 120 SY-VLINE,
                / SY-VLINE, 120 SY-VLINE,
                  60 '물품수령인', 72  ZTRED-ZFRCHNM, 120 SY-VLINE,
                / SY-VLINE, 5 SY-ULINE(36), 120 SY-VLINE,
                / SY-VLINE, 5 SY-VLINE, 10 '유효기일 및 Nego일이 경과',
                  40 SY-VLINE, 120 SY-VLINE,
                / SY-VLINE, 5 SY-VLINE, 12 '하여도 Nego에 동의함.',
                  40 SY-VLINE, 120 SY-VLINE,
                / SY-VLINE, 5 SY-ULINE(36), 120 SY-VLINE,
                / SY-ULINE.

          WRITE:/ SY-VLINE, 5 TEXT-002, 120 SY-VLINE,
                / SY-VLINE, 2 TEXT-003, 120 SY-VLINE,
                / SY-VLINE, 2 TEXT-004, 120 SY-VLINE,
                / SY-VLINE, 2 TEXT-005, 120 SY-VLINE,
                / SY-VLINE, 2 TEXT-006, 120 SY-VLINE,
                / SY-VLINE, 2 TEXT-007, 120 SY-VLINE,
                / SY-VLINE, 2 TEXT-008, 120 SY-VLINE,
                / SY-VLINE, 2 TEXT-009, 120 SY-VLINE,
                / SY-VLINE, 120 SY-VLINE,
                / SY-ULINE.

ENDFORM.                    " P3000_LINE_WRITE
