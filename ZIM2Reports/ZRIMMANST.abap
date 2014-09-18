*&---------------------------------------------------------------------*
*& Report  ZRIMMANST                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : 수입 제조원 현황.                                     *
*&      작성자 : 맹성호 INFOLINK Ltd.                                  *
*&      작성일 : 2001.11.16                                            *
*&---------------------------------------------------------------------*
*&   DESC.    : 수입 물품의 제조원을 자재별, 벤더별로 구분하여 현황을.
*&              파악한다.
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMMANST    MESSAGE-ID ZIM
                     LINE-SIZE 114
                     NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------
* TABLE 및 INTERNAL TABLE, 변수 Define
*----------------------------------------------------------------------
TABLES : ZTREQHD,
         ZTREQIT,
         LFA1,
         ZTREQST,
         EKPO,
         MAKT.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MATNR           LIKE   ZTREQIT-MATNR,        " 자재번호.
       MFRNR           LIKE   ZTREQIT-MFRNR,
       LIFNR           LIKE   ZTREQHD-LIFNR,        " Vendor
       ZFREQNO         LIKE   ZTREQIT-ZFREQNO,      " 수입의뢰번호.
       NAME3(25)       TYPE   C,                    " 제조원명.
       NAME2(25)       TYPE   C,                    " 자재명.
       LAND1           LIKE   LFA1-LAND1,           " 국가코드.
       NAME1(25)       TYPE   C,
       ZFOPNDT         LIKE   ZTREQST-ZFOPNDT,      " 개설일.
       EMATN           LIKE   EKPO-EMATN,           " 재고상품번호.
       EBELN           LIKE   EKPO-EBELN,           " P/O No.
       CODE(10)        TYPE   C.

DATA : END OF IT_TAB.

*DATA : BEGIN OF IT_LAND1 OCCURS 0,
*       LAND1          LIKE   T005T-LAND1,           " 국가코드.
*       LANDX          LIKE   T005T-LANDX.
*DATA : END   OF IT_LAND1.

*----------------------------------------------------------------------
* 변수 선언.
*----------------------------------------------------------------------
DATA : W_ERR_CHK         TYPE  C,
       W_SUBRC           LIKE  SY-SUBRC,
       W_PAGE            TYPE  I,                  " Page Counter
       W_LINE            TYPE  I,                  " Line Count
       W_FLAG            TYPE  C,
       W_COUNT           TYPE  I,                  " 전체 COUNT
       W_COLOR           TYPE  I,
       W_LIST_INDEX      LIKE  SY-TABIX,
       W_TABIX           LIKE  SY-TABIX,           " Table Index
       W_LINES           TYPE  I,
       W_SELECTED_LINES  TYPE  P,                  " 선택 Line Count
       W_JUL             TYPE  C,
       W_TEXT(20)        TYPE  C,
       WRITE_CHK         TYPE  C,
       W_MOD             TYPE  I,
       W_LIFNR           LIKE  LFA1-LIFNR,
       SV_LIFNR          LIKE  ZTREQHD-LIFNR,
       SV_MATNR          LIKE  ZTREQIT-MATNR,
       SV_MFRNR          LIKE  ZTREQIT-MFRNR.

INCLUDE   ZRIMUTIL01.                        " Utility function 모듈.
INCLUDE   ZRIMSORTCOM.                       " Sort를 위한 Include

*----------------------------------------------------------------------
* Selection Screen 절.
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

   SELECT-OPTIONS: S_MATNR  FOR ZTREQIT-MATNR,     " 자재번호.
                   S_MFRNR  FOR ZTREQIT-MFRNR,     " 제조원.
                   S_LIFNR  FOR ZTREQHD-LIFNR,     " 벤더.
                   S_OPNDT  FOR ZTREQST-ZFOPNDT.   " 개설일.
   SELECTION-SCREEN SKIP.

   PARAMETER : R_MAT   RADIOBUTTON GROUP GP1,      " 자재별.
               R_MAN   RADIOBUTTON GROUP GP1,      " 제조원별.
               R_VEN   RADIOBUTTON GROUP GP1.      " 벤더별.

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------
* Initialization.
*----------------------------------------------------------------------
INITIALIZATION.                                 " 초기값 SETTING
   SET  TITLEBAR 'ZIMR06'.                      " GUI TITLE SETTING..
   PERFORM   P2000_INIT.

*----------------------------------------------------------------------
* Top-Of-Page.
*----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 헤더 출력...

*----------------------------------------------------------------------
* Start Of Selection 절.
*----------------------------------------------------------------------
START-OF-SELECTION.

* 레포트 관련 TEXT TABLE SELECT
  PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

* 레포트 Write
  PERFORM   P3000_DATA_WRITE       USING W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.

      WHEN 'REFR'.
            PERFORM   P1000_READ_TEXT        USING W_ERR_CHK.
            IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.
            PERFORM RESET_LIST.
      WHEN 'BACK' OR 'EXIT' OR 'CANC'.
            LEAVE TO SCREEN 0.                " 종료.

      WHEN OTHERS.
   ENDCASE.
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------

FORM P2000_INIT.
*  P_V = 'X'.
*  P_Y = 'X'.

ENDFORM.                                     " P2000_INIT

*----------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------

FORM P3000_DATA_WRITE USING     W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   SET PF-STATUS 'ZIMR06'.                   " GUI STATUS SETTING
   SET TITLEBAR  'ZIMR06'.                   " GUI TITLE SETTING

   W_PAGE = 1.     W_LINE = 0.     W_COUNT = 0.

   IF R_MAT = 'X'.
      LOOP  AT  IT_TAB.

         W_COUNT = W_COUNT + 1.
         W_MOD   = W_COUNT MOD 2.

         IF SY-TABIX = 1.
            R_MAT = 'X'.
            MOVE  IT_TAB-MATNR  TO  SV_MATNR.
         ENDIF.

         IF IT_TAB-MATNR NE SV_MATNR.
            MOVE  IT_TAB-MATNR  TO  SV_MATNR.
            CLEAR WRITE_CHK.
            NEW-PAGE.
            R_MAT = 'X'.
         ENDIF.

         PERFORM  P3000_LINE_WRITE.

       ENDLOOP.
   ELSEIF R_MAN = 'X'.
      LOOP  AT  IT_TAB.

         W_COUNT = W_COUNT + 1.
         W_MOD   = W_COUNT MOD 2.

         IF SY-TABIX = 1.
            R_MAN = 'X'.
            MOVE  IT_TAB-MFRNR  TO  SV_MFRNR.
         ENDIF.

         IF IT_TAB-MFRNR NE SV_MFRNR.
            MOVE  IT_TAB-MFRNR  TO  SV_MFRNR.
            CLEAR WRITE_CHK.
            NEW-PAGE.
            R_MAN = 'X'.
         ENDIF.

         PERFORM  P3000_LINE_WRITE.

       ENDLOOP.
   ELSEIF R_VEN = 'X'.
      LOOP  AT  IT_TAB.

         W_COUNT = W_COUNT + 1.
         W_MOD   = W_COUNT MOD 2.

         IF SY-TABIX = 1.
            R_VEN = 'X'.
            MOVE  IT_TAB-LIFNR  TO  SV_LIFNR.
         ENDIF.

         IF IT_TAB-LIFNR NE SV_LIFNR.
            MOVE  IT_TAB-LIFNR  TO  SV_LIFNR.
            CLEAR WRITE_CHK.
            NEW-PAGE.
            R_VEN = 'X'.
         ENDIF.

         PERFORM  P3000_LINE_WRITE.

       ENDLOOP.

   ENDIF.

ENDFORM.                                       " P3000_Data_Write

*&---------------------------------------------------------------------
*&      Form  P2000_PAGE_CHECK
*&---------------------------------------------------------------------

FORM P2000_PAGE_CHECK.

   IF W_LINE >= 53.
      WRITE : / SY-ULINE.
      W_PAGE = W_PAGE + 1.    W_LINE = 0.
      NEW-PAGE.
   ENDIF.

ENDFORM.                                      " P2000_Page_Check

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------

FORM P3000_TITLE_WRITE.
   SKIP 1.
   IF R_MAT = 'X'.

      WRITE : /45 '  [ 수입 자재별 제조원 현황 ] ' COLOR 1.
      SKIP 1.
      WRITE : /95 'DATE :', SY-DATUM.

      CLEAR : MAKT.
      SELECT SINGLE *
        FROM MAKT   WHERE MATNR EQ SV_MATNR
                    AND   SPRAS EQ SY-LANGU.

      WRITE : /4  '자재번호 :', SV_MATNR, MAKT-MAKTX,
             95 'PAGE :', SY-PAGNO.
      WRITE : / SY-ULINE.
      FORMAT COLOR 1 INTENSIFIED OFF.
      WRITE : / SY-VLINE,
                (10) '제조원코드',               SY-VLINE,
                (25) '제조원 명' CENTERED,       SY-VLINE,
                (10) '벤더코드',                 SY-VLINE,
                (25) '벤더 명'   CENTERED,       SY-VLINE,
                (08) '국가코드'  CENTERED,       SY-VLINE,
                (17) 'MPN No.'   CENTERED,       SY-VLINE.

      WRITE : / SY-ULINE.

   ELSEIF  R_MAN = 'X'.
     WRITE : /45 '   수입 제조원별 벤더 현황  ' COLOR 1.
     SKIP 1.
     WRITE : /95 'DATE :', SY-DATUM.

     CLEAR : LFA1.
     SELECT SINGLE *
       FROM ZTREQIT   WHERE MFRNR  EQ SV_MFRNR.

     WRITE : /4  '제 조 원 :', SV_MFRNR, LFA1-NAME1,
             95 'PAGE :', SY-PAGNO.
     WRITE : / SY-ULINE.
     FORMAT COLOR 1 INTENSIFIED OFF.
     WRITE : / SY-VLINE,
               (10) '벤더코드',                SY-VLINE,
               (25) '벤더 명'   CENTERED,      SY-VLINE,
               (08) '국가코드'  CENTERED,      SY-VLINE,
               (10) '자재번호',                SY-VLINE,
               (25) '자재 명'   CENTERED,      SY-VLINE,
               (17) 'MPN No.'   CENTERED,      SY-VLINE.

     WRITE : / SY-ULINE.

  ELSEIF  R_VEN = 'X'.
     WRITE : /45 '   수입 벤더별 제조원 현황  ' COLOR 1.
     SKIP 1.
     WRITE : /95 'DATE :', SY-DATUM.

     CLEAR : LFA1.
     SELECT SINGLE *
       FROM LFA1   WHERE LIFNR  EQ  SV_LIFNR.

     WRITE : /4  '벤 더 :', SV_LIFNR, LFA1-NAME1,
              95 'PAGE :', SY-PAGNO.
     WRITE : / SY-ULINE.
     FORMAT COLOR 1 INTENSIFIED OFF.
     WRITE : / SY-VLINE,
               (10) '제조원코드',              SY-VLINE,
               (25) '제조원 명' CENTERED,      SY-VLINE,
               (08) '국가코드'  CENTERED,      SY-VLINE,
               (10) '자재번호',                SY-VLINE,
               (25) '자재 명'   CENTERED,      SY-VLINE,
               (17) 'MPN No.'   CENTERED,      SY-VLINE.

     WRITE : / SY-ULINE.

  ENDIF.

ENDFORM.                                   " Form P3000_TITLE_WRITE
*&---------------------------------------------------------------------
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------

FORM P3000_LINE_WRITE.

   FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

   FORMAT RESET.
   FORMAT COLOR COL_NORMAL INTENSIFIED ON.

*   SELECT SINGLE MATNR INTO IT_TAB-BMATN
*     FROM MARA
*    WHERE BMATN  EQ IT_TAB-BMATN
*      AND MFRNR  EQ IT_TAB-MFRNR.

   IF R_MAT = 'X'.

     WRITE : / SY-VLINE,
            (10) IT_TAB-MATNR,               SY-VLINE,
            (25) IT_TAB-NAME3,               SY-VLINE,
            (10) IT_TAB-LIFNR,               SY-VLINE,
            (25) IT_TAB-NAME1,               SY-VLINE,
            (08) IT_TAB-LAND1   CENTERED,    SY-VLINE,
            (17) IT_TAB-EMATN   CENTERED,    SY-VLINE.
     WRITE : / SY-ULINE.

   ELSEIF  R_MAN = 'X'.
     WRITE : / SY-VLINE,
            (10) IT_TAB-LIFNR,               SY-VLINE,
            (25) IT_TAB-NAME1,               SY-VLINE,
            (08) IT_TAB-LAND1   CENTERED,    SY-VLINE,
            (10) IT_TAB-MATNR,               SY-VLINE,
            (25) IT_TAB-NAME2,               SY-VLINE,
            (17) IT_TAB-EMATN   CENTERED,    SY-VLINE.
     WRITE : / SY-ULINE.

  ELSEIF  R_VEN = 'X'.
     WRITE : / SY-VLINE,
            (10) IT_TAB-MFRNR,               SY-VLINE,
            (25) IT_TAB-NAME3,               SY-VLINE,
            (08) IT_TAB-LAND1   CENTERED,    SY-VLINE,
            (10) IT_TAB-MATNR,               SY-VLINE,
            (25) IT_TAB-NAME2,               SY-VLINE,
            (17) IT_TAB-EMATN   CENTERED,    SY-VLINE.
     WRITE : / SY-ULINE.

  ENDIF.

* Stored value...
   MOVE SY-TABIX  TO W_LIST_INDEX.
   HIDE IT_TAB.
*   W_COUNT = W_COUNT + 1.

*   WRITE: / SY-ULINE.

ENDFORM.                                     " P3000_Line_Write

*&---------------------------------------------------------------------
*&      Form  RESET_LIST
*&---------------------------------------------------------------------

FORM RESET_LIST.

  MOVE 0 TO SY-LSIND.

  W_PAGE  = 1.
  W_LINE  = 1.
  W_COUNT = 0.
  PERFORM   P3000_TITLE_WRITE.               " 헤더 출력...
* 레포트 Write
  PERFORM   P3000_DATA_WRITE  USING   W_ERR_CHK.

ENDFORM.                                     " Reset_List

*&---------------------------------------------------------------------
*&      Form  P1000_READ_TEXT
*&---------------------------------------------------------------------

FORM P1000_READ_TEXT    USING W_ERR_CHK.

   MOVE 'N' TO W_ERR_CHK.

   REFRESH : IT_TAB.

   IF R_MAT = 'X'.

      SELECT B~MATNR A~LIFNR  B~MFRNR    MAX( D~EMATN ) AS EMATN
             MAX( B~MEINS )   AS MEINS   MAX( B~TXZ01 ) AS NAME2
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
      FROM ( ZTREQHD  AS  A   INNER JOIN  ZTREQIT AS B
      ON     A~ZFREQNO        EQ    B~ZFREQNO )
      INNER  JOIN     ZTREQST AS    C
      ON     A~ZFREQNO        EQ    C~ZFREQNO
      INNER  JOIN     EKPO    AS    D
      ON     A~EBELN          EQ    D~EBELN
      AND    B~EBELP          EQ    D~EBELP
      WHERE  B~MFRNR          IN    S_MFRNR
      AND    B~MATNR          IN    S_MATNR
      AND    A~LIFNR          IN    S_LIFNR
      AND    C~ZFOPNDT        IN    S_OPNDT
      GROUP  BY
             B~MATNR B~MFRNR  A~LIFNR.

   ELSEIF R_MAN = 'X'.

      SELECT A~LIFNR  B~MATNR  B~MFRNR  MAX( D~EMATN ) AS EMATN
             MAX( B~MEINS )   AS MEINS  MAX( B~TXZ01 ) AS NAME2
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
      FROM ( ( ZTREQHD  AS  A   INNER JOIN  ZTREQIT AS B
      ON     A~ZFREQNO        EQ    B~ZFREQNO )
      INNER  JOIN     ZTREQST AS    C
      ON     A~ZFREQNO        EQ    C~ZFREQNO )
      INNER  JOIN     EKPO    AS    D
      ON     B~EBELN          EQ    D~EBELN
      AND    B~EBELP          EQ    D~EBELP
      WHERE  B~MFRNR          IN    S_MFRNR
      AND    B~MATNR          IN    S_MATNR
      AND    A~LIFNR          IN    S_LIFNR
      AND    C~ZFOPNDT        IN    S_OPNDT
      GROUP  BY
             B~MFRNR  A~LIFNR  B~MATNR .

   ELSEIF R_VEN = 'X'.

      SELECT A~LIFNR B~MATNR  B~MFRNR   MAX( D~EMATN ) AS EMATN
             MAX( B~MEINS )   AS MEINS  MAX( B~TXZ01 ) AS NAME2
      INTO   CORRESPONDING FIELDS OF TABLE IT_TAB
      FROM ( ( ZTREQHD  AS  A   INNER JOIN  ZTREQIT AS B
      ON     A~ZFREQNO        EQ    B~ZFREQNO )
      INNER  JOIN     ZTREQST AS    C
      ON     A~ZFREQNO        EQ    C~ZFREQNO )
      INNER  JOIN     EKPO    AS    D
      ON     B~EBELN          EQ    D~EBELN
      AND    B~EBELP          EQ    D~EBELP
      WHERE  B~MFRNR          IN    S_MFRNR
      AND    B~MATNR          IN    S_MATNR
      AND    A~LIFNR          IN    S_LIFNR
      AND    C~ZFOPNDT        IN    S_OPNDT
      GROUP  BY
             A~LIFNR B~MFRNR B~MATNR  .
   ENDIF.

      DESCRIBE TABLE IT_TAB LINES W_LINE.
      IF W_LINE EQ 0. W_SUBRC = 4. EXIT. ENDIF.

*>> INTERNAL TABLE 에 VENDOR 명, 제조원명, 자재명 DISPLAY
      LOOP AT IT_TAB.

         W_TABIX = SY-TABIX.

         SELECT SINGLE NAME1 LAND1
           INTO (IT_TAB-NAME3, IT_TAB-LAND1)
           FROM LFA1   WHERE LIFNR EQ IT_TAB-MFRNR.

*         SELECT SINGLE MAKTX INTO IT_TAB-NAME2
*          FROM MAKT   WHERE MATNR EQ IT_TAB-MATNR
*                      AND   SPRAS EQ SY-LANGU.

         SELECT SINGLE NAME1 INTO IT_TAB-NAME1
           FROM LFA1   WHERE LIFNR EQ IT_TAB-LIFNR.

         MODIFY IT_TAB INDEX W_TABIX.

      ENDLOOP.

ENDFORM.                    " P1000_READ_TEXT

*&---------------------------------------------------------------------
*&      Form  P2000_CONFIG_CHECK
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------

FORM P2000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
*  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
     W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

*  IF ZTIMIMG00-ZFCGYN IS INITIAL.
*     W_ERR_CHK = 'Y'.   MESSAGE S573.   EXIT.
*  ENDIF.


ENDFORM.                    " P2000_CONFIG_CHECK
