*
* Andy
*
REPORT ZR01FIR_TB NO STANDARD PAGE HEADING                ".
                                    LINE-SIZE 120           " Size 120
                                    LINE-COUNT 90.          " 너비 90
"
* Table 정의
TABLES: GLT0,                          " 계정월합
        VWBEKI,
        T001,                          " 회사코드
        SKAT.                          " 계정과목명

* 임시 Structure
DATA: BEGIN OF IMSI,
        BUKRS             LIKE  GLT0-BUKRS,               "
        CHK               TYPE  C,
        RACCT             LIKE  GLT0-RACCT,
        TXT20(40)         TYPE  C,
        S_GUMAK           LIKE  GLT0-HSL12,
        H_GUMAK           LIKE  GLT0-HSL12,
        TOT               LIKE  GLT0-HSL12,
      END   OF IMSI.

* Internal Table
DATA  BEGIN OF itab OCCURS 05.         " Internal
INCLUDE  STRUCTURE  IMSI.
DATA  END   OF itab.

*
DATA: BEGIN OF ZGLT0 OCCURS 05,
        BUKRS        LIKE     GLT0-BUKRS,
        RYEAR        LIKE     GLT0-RYEAR,
        RACCT        LIKE     GLT0-RACCT,
        DRCRK        LIKE     GLT0-DRCRK,
        HSL01        LIKE     GLT0-HSL01,
        HSL02        LIKE     GLT0-HSL02,
        HSL03        LIKE     GLT0-HSL03,
        HSL04        LIKE     GLT0-HSL04,
        HSL05        LIKE     GLT0-HSL05,
        HSL06        LIKE     GLT0-HSL06,
        HSL07        LIKE     GLT0-HSL07,
        HSL08        LIKE     GLT0-HSL08,
        HSL09        LIKE     GLT0-HSL09,
        HSL10        LIKE     GLT0-HSL10,
        HSL11        LIKE     GLT0-HSL11,
        HSL12        LIKE     GLT0-HSL12,
      END   OF ZGLT0.
DATA:
      P-S_GUMAK(21)      TYPE  C,
      P-H_GUMAK(21)      TYPE  C,
      P-TOT(21)          TYPE  C,
      STR                TYPE  C   VALUE  '-',
      P-TXT20(40)        TYPE  C,
      D-RPMAX(02)        TYPE  P,
      D_BUTXT            LIKE  T001-BUTXT,
      P-GYE(10)          TYPE  C,
      COUNT1(02)         TYPE  N,
      HEADER_LINE(120)   TYPE  C,
      G_C                LIKE T001-Waers.


*SELECTION-SCREEN   SKIP 1.
PARAMETERS:        BUKRS         LIKE  GLT0-BUKRS MEMORY ID BUK.
*SELECTION-SCREEN   SKIP 1.
PARAMETERS:        RYEAR         LIKE  GLT0-RYEAR MEMORY ID GJR.
SELECT-OPTIONS:    RPMAX         FOR   GLT0-RPMAX.
*SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF   BLOCK BL1
                            WITH  FRAME
                            TITLE TEXT-001.
SELECT-OPTIONS:    RACCT         FOR   SKAT-SAKNR.

SELECTION-SCREEN END   OF   BLOCK BL1.
PARAMETERS : p_file LIKE RLGRAP-FILENAME DEFAULT
  'c:\temp\tb.xls'.

*--------------------------------------------------------------------
START-OF-SELECTION.
  select single * from t001 where bukrs = BUKRS.
  check sy-subrc = 0.
  D_BUTXT = T001-BUTXT.
  G_C     = T001-Waers.

  REFRESH ZGLT0.
  CLEAR ZGLT0.

  SELECT BUKRS RYEAR RACCT DRCRK SUM( HSL01 ) SUM( HSL02 ) SUM( HSL03 )
       SUM( HSL04 ) SUM( HSL05 ) SUM( HSL06 ) SUM( HSL07 ) SUM( HSL08 )
       SUM( HSL09 ) SUM( HSL10 ) SUM( HSL11 ) SUM( HSL12 ) INTO
       (ZGLT0-BUKRS, ZGLT0-RYEAR, ZGLT0-RACCT, ZGLT0-DRCRK, ZGLT0-HSL01,
        ZGLT0-HSL02, ZGLT0-HSL03, ZGLT0-HSL04, ZGLT0-HSL05, ZGLT0-HSL06,
        ZGLT0-HSL07, ZGLT0-HSL08, ZGLT0-HSL09, ZGLT0-HSL10, ZGLT0-HSL11,
        ZGLT0-HSL12) FROM GLT0
        WHERE  BUKRS  =  BUKRS         " 회사코드
        AND    RYEAR  =  RYEAR         " 회계년도
        AND    RACCT  IN RACCT         " 계정번호가
        GROUP BY BUKRS RYEAR RACCT DRCRK.
    COLLECT ZGLT0.
  ENDSELECT.

END-OF-SELECTION.
  PERFORM  READ_PROCESS.               " 작업을 수행
  PERFORM  OUTPUT_PROCESS.             " 리스트출력


************************************************************************
***  (SHIFT+PF1) Execute Download
************************************************************************
AT pf13.
  PERFORM data_download.

*--------------------------------------------------------------------
TOP-OF-PAGE.
* SET PF-STATUS weiter.

*  FORMAT   INTENSIFIED  OFF.
*  SKIP 4.
*  WRITE:   'Monthly Balance'            TO  HEADER_LINE.
*  WRITE:/  HEADER_LINE CENTERED.
*  WRITE:   '========================'  TO  HEADER_LINE.
*  WRITE:/  HEADER_LINE CENTERED.
*
** PRINT-CONTROL CPI 18.
*
*  SKIP 3.
  WRITE:/  'Date:', SY-DATUM  DD/MM/YYYY,
        40 'Company:', BUKRS, D_BUTXT,
        80 'Time:', SY-UZEIT  USING EDIT MASK '__:__:__'.

  WRITE:/  'Fiscal Year:', RYEAR.

  IF  RPMAX-LOW  =  RPMAX-HIGH.
    D-RPMAX = RPMAX-LOW.
    WRITE: 40 'Period:' NO-GAP, RPMAX-LOW NO-GAP.
  ELSE.
    D-RPMAX = RPMAX-LOW.
    WRITE: 40 'Period:' NO-GAP, D-RPMAX.
    D-RPMAX = RPMAX-HIGH.
    WRITE:    '~',     D-RPMAX.
  ENDIF.

  IF  RACCT-LOW  =  RACCT-HIGH.
    WRITE: 80 'Account:', RACCT-LOW.
  ELSE.
    WRITE: 80 'Account:', RACCT-LOW.
    WRITE:    '~',      RACCT-HIGH.
  ENDIF.

  WRITE:/  SY-ULINE(117).              " 밑줄짝..
  WRITE:/  ' ' NO-GAP,
           'Account' CENTERED,          " 계정코드
           12 'Description' CENTERED,          " 계정명
           69 'Debit'   RIGHT-JUSTIFIED,   " 차변
           90 'Credit'  RIGHT-JUSTIFIED,   " 대변
           111 'Inc/Dec' RIGHT-JUSTIFIED.   " 증감

  WRITE:/  SY-ULINE(117).              " 밑줄짝..

END-OF-PAGE.
  WRITE:/  SY-ULINE(117).              " 밑줄짝..
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.

  REFRESH: itab.
  CLEAR:   itab.

*----------------------------------------------
*  SELECT * FROM GLT0
*    WHERE  BUKRS  =  BUKRS             " 회사코드
*    AND    RYEAR  =  RYEAR             " 회계년도
*    AND    RACCT  IN RACCT.            " 계정번호가
*    PERFORM  IMSI_PROCESS.             " 구조저장
*    PERFORM  itab_PROCESS.             "
*  ENDSELECT.
  LOOP AT ZGLT0.
    IF SY-SUBRC <> 0. EXIT. ENDIF.
    PERFORM  IMSI_PROCESS.             " 구조저장
    PERFORM  itab_PROCESS.

  ENDLOOP.

  CLEAR  SY-SUBRC.
ENDFORM.                               " READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  IMSI_PROCESS
*&---------------------------------------------------------------------*
FORM IMSI_PROCESS.
  DATA: COUNT(02)  TYPE N,
        l_acct(10) type c.


  CLEAR: IMSI, COUNT, COUNT1.

  IMSI-BUKRS  =  ZGLT0-BUKRS.          " 회사코드
  IMSI-RACCT  =  ZGLT0-RACCT.          " 계정번호

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = ZGLT0-RACCT
       IMPORTING
            output = l_ACCT
       EXCEPTIONS
            OTHERS = 1.

  IMSI-CHK    =  l_acct(01).    " first number

  IF RPMAX-HIGH IS INITIAL. RPMAX-HIGH = RPMAX-LOW. ENDIF.

  COUNT  = RPMAX-HIGH - RPMAX-LOW + 1.
  COUNT1 = RPMAX-LOW.

  DO COUNT TIMES.
*   CHECK  sy-index  IN  rpmax.        " 속하는지를 체크
    PERFORM   DONDA_PROCESS.           " 값을 이동
    COUNT1 = COUNT1 + 1.
  ENDDO.

  IMSI-TOT    =  IMSI-S_GUMAK - IMSI-H_GUMAK.     " 증감

ENDFORM.                               " IMSI_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DONDA_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DONDA_PROCESS.

  CASE  ZGLT0-DRCRK.                   "
    WHEN  'S'.
      CASE COUNT1.
        WHEN  1.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL01.
        WHEN  2.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL02.
        WHEN  3.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL03.
        WHEN  4.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL04.
        WHEN  5.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL05.
        WHEN  6.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL06.
        WHEN  7.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL07.
        WHEN  8.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL08.
        WHEN  9.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL09.
        WHEN 10.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL10.
        WHEN 11.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL11.
        WHEN 12.  IMSI-S_GUMAK  =  IMSI-S_GUMAK  +  ZGLT0-HSL12.
      ENDCASE.
    WHEN  'H'.                         " 대변
      CASE COUNT1.
        WHEN  1.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL01 * -1 ).
        WHEN  2.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL02 * -1 ).
        WHEN  3.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL03 * -1 ).
        WHEN  4.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL04 * -1 ).
        WHEN  5.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL05 * -1 ).
        WHEN  6.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL06 * -1 ).
        WHEN  7.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL07 * -1 ).
        WHEN  8.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL08 * -1 ).
        WHEN  9.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL09 * -1 ).
        WHEN 10.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL10 * -1 ).
        WHEN 11.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL11 * -1 ).
        WHEN 12.  IMSI-H_GUMAK  =  IMSI-H_GUMAK  + ( ZGLT0-HSL12 * -1 ).
      ENDCASE.
  ENDCASE.

ENDFORM.                               " DONDA_PROCESS

*&---------------------------------------------------------------------*
*&      Form  itab_PROCESS
*&---------------------------------------------------------------------*
FORM itab_PROCESS.
  IF    IMSI-S_GUMAK  IS   INITIAL  AND
        IMSI-H_GUMAK  IS   INITIAL.
    EXIT.
  ENDIF.

  MOVE-CORRESPONDING IMSI  TO itab.

*  MOVE: IMSI-BUKRS    TO   itab-BUKRS, " 회사코드
*        IMSI-CHK      TO   itab-CHK,   " CHK
*        IMSI-RACCT    TO   itab-RACCT, " 계정코드
*        IMSI-TXT20    TO   itab-TXT20, " 계정명
*        IMSI-S_GUMAK  TO   itab-S_GUMAK,          " 차변금액
*        IMSI-H_GUMAK  TO   itab-H_GUMAK,          " 대변금액
*        IMSI-TOT      TO   itab-TOT.   " 증감

  APPEND  itab.

ENDFORM.                               " itab_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SKAT_PROCESS
*&---------------------------------------------------------------------*
FORM SKAT_PROCESS USING    P_IMSI_RACCT.

  SELECT SINGLE TXT50 FROM SKAT INTO itab-TXT20            " 계정내


    WHERE  KTOPL  =  t001-ktopl
      and  SPRAS  = sy-langu
      AND  SAKNR  =  P_IMSI_RACCT.     " 계정번호

  CLEAR  SY-SUBRC.

ENDFORM.                               " SKAT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  OUTPUT_PROCESS
*&---------------------------------------------------------------------*
FORM OUTPUT_PROCESS.
  SORT itab BY CHK RACCT.              " CHK RACCT 소트

  LOOP AT itab.
    IF  SY-SUBRC  <>  0.   EXIT.  ENDIF.

    AT END OF RACCT.
      SUM.
      PERFORM  LINE_WRITE_PROCESS.     " LINE을 찍는다
    ENDAT.

    AT END OF CHK.
      SUM.
      PERFORM  TOT_WRITE_PROCESS.      " 합계를 찍는다.
    ENDAT.

    AT LAST.
      SUM.
      PERFORM  HAP-WRITE_PROCESS.
    ENDAT.

  ENDLOOP.

ENDFORM.                               " OUTPUT_PROCESS

*&---------------------------------------------------------------------*
*&      Form  LINE_WRITE_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LINE_WRITE_PROCESS.
  FORMAT   RESET.
  CLEAR:   P-H_GUMAK, P-S_GUMAK, P-TOT.
  PERFORM  SKAT_PROCESS USING  itab-RACCT.

  P-TXT20   =  itab-TXT20.             " 과목명저장

  WRITE:/  ' ' NO-GAP,
           itab-RACCT COLOR COL_KEY NO-GAP,
           P-TXT20    COLOR 1  LEFT-JUSTIFIED.

  FORMAT   COLOR COL_NORMAL.

  WRITE: itab-S_GUMAK CURRENCY g_c  TO  P-S_GUMAK,
         itab-H_GUMAK CURRENCY g_c  TO  P-H_GUMAK,
         itab-TOT     CURRENCY g_c  TO  P-TOT.

  SHIFT    P-S_GUMAK UP TO STR CIRCULAR.
  SHIFT    P-H_GUMAK UP TO STR CIRCULAR.
  SHIFT    P-TOT     UP TO STR CIRCULAR.

  CONDENSE: P-S_GUMAK NO-GAPS,
            P-H_GUMAK NO-GAPS,
            P-TOT     NO-GAPS.

  WRITE:   P-S_GUMAK     RIGHT-JUSTIFIED,
           P-H_GUMAK     RIGHT-JUSTIFIED,
           P-TOT         RIGHT-JUSTIFIED.

  CLEAR  SY-SUBRC.

ENDFORM.                               " LINE_WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  TOT_WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM TOT_WRITE_PROCESS.
  CLEAR:   P-H_GUMAK, P-S_GUMAK, P-TOT.

  CASE  itab-CHK.
    WHEN 1.  P-GYE  =  '  Asset'.
    WHEN 2.  P-GYE  =  'Liabili'.
    WHEN 3.  P-GYE  =  'Capital'.
    WHEN 5.  P-GYE  =  'SlsPrft'.
    WHEN 6.  P-GYE  =  ' Overhd'.
    WHEN 7.  P-GYE  =  'OthRvEx'.
    WHEN 9.  P-GYE  =  'Clearng'.
  ENDCASE.

  FORMAT   RESET.

  WRITE:   'Sub Total'    TO   P-TXT20.
  WRITE:/  SY-ULINE(117).
  WRITE:/  ' ' NO-GAP,
           P-GYE         COLOR COL_TOTAL NO-GAP,
           P-TXT20       COLOR 1 LEFT-JUSTIFIED.

  FORMAT   COLOR COL_NORMAL.

  WRITE: itab-S_GUMAK CURRENCY g_c  TO  P-S_GUMAK,
         itab-H_GUMAK CURRENCY g_c  TO  P-H_GUMAK,
         itab-TOT     CURRENCY g_c  TO  P-TOT.

  SHIFT    P-S_GUMAK UP TO STR CIRCULAR.
  SHIFT    P-H_GUMAK UP TO STR CIRCULAR.
  SHIFT    P-TOT     UP TO STR CIRCULAR.

  CONDENSE: P-S_GUMAK NO-GAPS,
            P-H_GUMAK NO-GAPS,
            P-TOT     NO-GAPS.

  WRITE:   P-S_GUMAK     RIGHT-JUSTIFIED,
           P-H_GUMAK     RIGHT-JUSTIFIED,
           P-TOT         RIGHT-JUSTIFIED.

  CLEAR  SY-SUBRC.

  FORMAT   RESET.
  WRITE:/  SY-ULINE(117).

ENDFORM.                               " TOT_WRITE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  HAP-WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM HAP-WRITE_PROCESS.
  CLEAR:   P-H_GUMAK, P-S_GUMAK, P-TOT.

  FORMAT   RESET.

  WRITE:   'S U M'    TO   P-TXT20.
  WRITE:/  SY-ULINE(117).
  WRITE:/  ' ' NO-GAP,
           '          '  COLOR COL_TOTAL NO-GAP,
           P-TXT20       COLOR 1 LEFT-JUSTIFIED.

  FORMAT   COLOR COL_NORMAL.

  WRITE: itab-S_GUMAK CURRENCY g_c  TO  P-S_GUMAK,
         itab-H_GUMAK CURRENCY g_c  TO  P-H_GUMAK,
         itab-TOT     CURRENCY g_c  TO  P-TOT.

  SHIFT    P-S_GUMAK UP TO STR CIRCULAR.
  SHIFT    P-H_GUMAK UP TO STR CIRCULAR.
  SHIFT    P-TOT     UP TO STR CIRCULAR.

  CONDENSE: P-S_GUMAK NO-GAPS,
            P-H_GUMAK NO-GAPS,
            P-TOT     NO-GAPS.

  WRITE:   P-S_GUMAK     RIGHT-JUSTIFIED,
           P-H_GUMAK     RIGHT-JUSTIFIED,
           P-TOT         RIGHT-JUSTIFIED.

  CLEAR  SY-SUBRC.

  FORMAT   RESET.
  WRITE:/  SY-ULINE(117).

ENDFORM.                               " HAP-WRITE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  data_download
*&---------------------------------------------------------------------*
FORM data_download.
* EDIT_TABLE_WITH_EXCEL
* SEND_TABLE_TO_EXCEL

  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            filename = p_file
            filetype = 'WK1'
       TABLES
            data_tab = itab.

  write:/ p_file, ' is created...'.
ENDFORM.                    " data_download
