*&---------------------------------------------------------------------
*& Report  ZRIMCURSTA
*&---------------------------------------------------------------------
*& 프로그램명 : ZRIMCURSTA
*&     작성자 : 맹성호 INFOLINK.Ltd
*&     작성일 : 01/08/2002
*&---------------------------------------------------------------------
*& Desc.      : 통화별 수입 실적을 월별로 보여준다.
*&---------------------------------------------------------------------

REPORT  ZRIMCURSTA     NO STANDARD PAGE HEADING
                       MESSAGE-ID ZIM
                       LINE-SIZE 277
                       LINE-COUNT 65.

TABLES : ZTIDS.                                    " 수입면허.

DATA : BEGIN OF IT_TAB OCCURS  0,
       ZFSTAMC          LIKE    ZTIDS-ZFSTAMC,     " 결제금액 통화.
       ZFSTAMT          LIKE    ZTIDS-ZFSTAMT,     " 결제금액.
       ZFIDSDT          LIKE    ZTIDS-ZFIDSDT,     " 신고수리일.
       BUKRS            LIKE    ZTIDS-BUKRS.       " 회사코드.
DATA : END  OF IT_TAB.

*----------------------------------------------------------------------
* 변수 선언.
*----------------------------------------------------------------------
DATA : W_ERR_CHK          TYPE   C,
       W_SUBRC            LIKE   SY-SUBRC,
       W_PAGE             TYPE   I,               " Page Counter
       W_LINE             TYPE   I,               " 페이지당 Line Count
       W_FLAG             TYPE   C,
       W_COUNT            TYPE   I,               " 전체 COUNT
       W_COLOR            TYPE   I,
       W_LIST_INDEX       LIKE   SY-TABIX,
       W_TABIX            LIKE   SY-TABIX,        " Table Index
       W_CNT(1),
       W_TEXT(20)         TYPE   C,
       W_TOTSUM(20)       TYPE   C,
       W_LINES            TYPE   I,
       SV_JUL             TYPE   C,
       SV_CHK             TYPE   C,
       SV_BUKRS           LIKE   ZTIDS-BUKRS,
       SV_STAMC           LIKE   ZTIDS-ZFSTAMC,
       SV_STAMT1          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT2          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT3          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT4          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT5          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT6          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT7          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT8          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT9          LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT10         LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT11         LIKE   ZTIDS-ZFSTAMT,
       SV_STAMT12         LIKE   ZTIDS-ZFSTAMT,
       SV_STAMTS          LIKE   ZTIDS-ZFSTAMT.

*----------------------------------------------------------------------
* Selection Screen 절.
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.

   SELECT-OPTIONS : S_BUKRS  FOR ZTIDS-BUKRS, "회사코드.
                    S_STAMC  FOR ZTIDS-ZFSTAMC,       " 결제통화.
                    S_IDSDT  FOR ZTIDS-ZFIDSDT.       " 기간.
   SELECTION-SCREEN SKIP.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
* Top-Of-Page.
*----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...

*----------------------------------------------------------------------
* Initialization.
*----------------------------------------------------------------------
INITIALIZATION.                                  " 초기값 SETTING
   SET  TITLEBAR 'ZIMR51'.                       " GUI TITLE SETTING..
   PERFORM P1000_INITIALIZATION.

*----------------------------------------------------------------------
* Start Of Selection 절.
*----------------------------------------------------------------------
START-OF-SELECTION.

  IF S_IDSDT-LOW(04) NE S_IDSDT-HIGH(04).
     MESSAGE S597.  EXIT.
  ENDIF.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_DATA        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_WRITE_DATA.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.

      WHEN 'REFR'.
            PERFORM   P1000_READ_DATA        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종료.
      WHEN OTHERS.

   ENDCASE.

*&---------------------------------------------------------------------
*&      Form  P1000_INITIALIZATION
*&---------------------------------------------------------------------
FORM P1000_INITIALIZATION.
  CONCATENATE SY-DATUM(4) '0101' INTO S_IDSDT-LOW.
  S_IDSDT-HIGH = SY-DATUM.
  APPEND S_IDSDT.
ENDFORM.                          " P1000_INITIALIZATION

*&---------------------------------------------------------------------
*&      Form  P1000_READ_DATA
*&---------------------------------------------------------------------
FORM P1000_READ_DATA  USING W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   REFRESH : IT_TAB.

   SELECT ZFSTAMT  ZFIDSDT  BUKRS  ZFSTAMC
   INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
   FROM   ZTIDS
   WHERE  BUKRS         IN    S_BUKRS
   AND    ZFIDSDT       IN    S_IDSDT
   AND    ZFSTAMC       IN    S_STAMC
   AND    ZFSTAMC       NE    SPACE.

   DESCRIBE TABLE IT_TAB LINES W_LINE.
   IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

   SORT  IT_TAB BY  ZFSTAMC.

ENDFORM.                    " P1000_READ_DATA

*&----------------------------------------------------------------------
*&      Form  P3000_WRITE_DATA
*&----------------------------------------------------------------------
FORM P3000_WRITE_DATA.

  CLEAR IT_TAB.

   SET PF-STATUS 'ZIMR51'.                   " GUI STATUS SETTING
   SET TITLEBAR  'ZIMR51'.                   " GUI TITLE SETTING

  LOOP AT IT_TAB.

     IF SY-TABIX EQ 1.
        MOVE : IT_TAB-ZFSTAMC    TO  SV_STAMC.
     ENDIF.

* 각각의 구분별, 실적금액별로 SUM.
     IF SV_STAMC NE IT_TAB-ZFSTAMC.
        PERFORM  P2000_SUM_LINE_WRITE.
        MOVE : IT_TAB-ZFSTAMC     TO SV_STAMC.
        CLEAR : SV_STAMT1, SV_STAMT2,     SV_STAMT3,  SV_STAMT4,
                SV_STAMT5, SV_STAMT6,     SV_STAMT7,  SV_STAMT8,
                SV_STAMT9, SV_STAMT10,    SV_STAMT11, SV_STAMT12,
                SV_STAMTS.
     ENDIF.

* 월별 수입실적.
     IF IT_TAB-ZFIDSDT+4(02) = '01'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT1.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '02'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT2.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '03'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT3.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '04'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT4.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '05'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT5.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '06'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT6.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '07'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT7.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '08'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT8.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '09'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT9.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '10'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT10.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '11'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT11.
     ENDIF.
     IF IT_TAB-ZFIDSDT+4(02) = '12'.
        ADD : IT_TAB-ZFSTAMT    TO  SV_STAMT12.
     ENDIF.
     ADD : IT_TAB-ZFSTAMT   TO  SV_STAMTS.

  ENDLOOP.
  PERFORM  P2000_SUM_LINE_WRITE.

ENDFORM.                              " P3000_WRITE_DATA
*&----------------------------------------------------------------------
*&      Form  RESET_LIST
*&----------------------------------------------------------------------
FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE  = 1.
  W_LINE  = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.        " 헤더 출력...
* 레포트 Write
  PERFORM   P3000_WRITE_DATA.

ENDFORM.                              " RESET_LIST

*&---------------------------------------------------------------------
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------

FORM P2000_PAGE_CHECK.

   IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.

ENDFORM.                              " P2000_Page_Check

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------

FORM P3000_TITLE_WRITE.
   SKIP 1.

   WRITE : /80 '   [ 통화별 월간 통관실적 현황 ]   ' COLOR 1.
   SKIP 1.
   WRITE : /250 'DATE :', SY-DATUM.
   WRITE : /6 '개설기간 :', S_IDSDT-LOW, '-', S_IDSDT-HIGH,
           250 'PAGE :', SY-PAGNO.
   WRITE : / SY-ULINE.

   FORMAT COLOR 1 INTENSIFIED OFF.
   WRITE : / SY-VLINE,
         (10) '통  화'        CENTERED,        SY-VLINE,
         (17) '1  월'         CENTERED,        SY-VLINE,
         (17) '2  월'         CENTERED,        SY-VLINE,
         (17) '3  월'         CENTERED,        SY-VLINE,
         (17) '4  월'         CENTERED,        SY-VLINE,
         (17) '5  월'         CENTERED,        SY-VLINE,
         (17) '6  월'         CENTERED,        SY-VLINE,
         (17) '7  월'         CENTERED,        SY-VLINE,
         (17) '8  월'         CENTERED,        SY-VLINE,
         (17) '9  월'         CENTERED,        SY-VLINE,
         (17) '10 월'         CENTERED,        SY-VLINE,
         (17) '11 월'         CENTERED,        SY-VLINE,
         (17) '12 월'         CENTERED,        SY-VLINE,
         (20) '합  계'        CENTERED,        SY-VLINE.
   WRITE : / SY-ULINE.

ENDFORM.                              " FORM P3000_TITLE_WRITE
*&----------------------------------------------------------------------
*&      Form  P2000_SUM_LINE_WRITE
*&----------------------------------------------------------------------
FORM P2000_SUM_LINE_WRITE.

   WRITE : / SY-VLINE,
          (10) SV_STAMC,                            SY-VLINE,
          (17) SV_STAMT1       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT2       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT3       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT4       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT5       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT6       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT7       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT8       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT9       CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT10      CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT11      CURRENCY SV_STAMC,   SY-VLINE,
          (17) SV_STAMT12      CURRENCY SV_STAMC,   SY-VLINE,
          (20) SV_STAMTS       CURRENCY SV_STAMC,   SY-VLINE.
   WRITE : / SY-ULINE.

ENDFORM.                              " P2000_SUM_LINE_WRITE
