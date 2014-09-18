REPORT  ZR01FIR_JR NO STANDARD PAGE HEADING.
TABLES: BKPF, BSEG, ZFI01,RF05L, USR21, ADCP, VBKPF.
DATA: BEGIN OF REC OCCURS 10.
        INCLUDE STRUCTURE ZFI01.
DATA: END OF REC.

DATA : SELKZ,SELKZ1.
DATA : BEGIN OF SAVE OCCURS 100,
        BUKRS LIKE ZFI01-BUKRS,    "회사 코드
        DATE    LIKE ZFI01-CPUDT,  "시스템 date
        SABUN(08),                     "user_id.
        DEPARTMENT(30),                "소속부서명
        SLIP    LIKE ZFI01-BELNR,  "전표번호
        CHASU   LIKE ZFI01-CHASU,  "차수
        GUBUN   LIKE ZFI01-GUBUN,  "GUBUN출력분
        SELKZ   LIKE ZFI01-SELKZ,  "전표check 유무
        BUDAT   LIKE ZFI01-BUDAT,  "전표전기일자
       END OF SAVE.
DATA: BEGIN OF BUZTAB OCCURS 20,
      BUKRS(4),
      BELNR(10),
      GJAHR(4),
      BUZEI(3),
      FLAEN(10),
      END OF BUZTAB.
DATA: TCODE(4),
      BUZTAB-ZEILE        LIKE SY-TABIX,
      X_NOCHANGE(1)       TYPE C,
      X_COMMIT(1)         TYPE C.
DATA : MARKFIELD1(1) TYPE C,
       MARKFIELD2(1) TYPE C.
DATA : TEMP-SELKZ LIKE ZFI01-SELKZ,"전표check 유무
       TEMP-DATE  LIKE ZFI01-CPUDT,"시스템 date
       TEMP-BUDAT LIKE ZFI01-BUDAT,"전기일자
       TEMP-CHASU LIKE ZFI01-CHASU,"차수
       TEMP-SABUN LIKE ZFI01-USNAM,"user_id.
       FCODE(4),
       SEL_FIELD(20),
       TEMP-TEXT(25).
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
PARAMETERS: p_buk LIKE ZFI01-BUKRS MEMORY ID BUK.    "회사구분
SELECT-OPTIONS: p_user  FOR ZFI01-USNAM, "사번입력
                p_date  FOR SY-DATUM.   "시스템 출력일자
SELECTION-SCREEN END OF BLOCK B1.

PARAMETERS: p_ini(1) type c default ' '.  "init

************************************************************************
START-OF-SELECTION.
*  SET PF-STATUS 'BASE'.                "화면 선택,ICON-DISPLAY 기
*  IF p_date-high IS INITIAL.  p_date-high = p_date-low. ENDIF.
*  IF p_user-high IS INITIAL.  p_user-high = p_user-low. ENDIF.
  "입력부문에서 high 부문에 입력값이 없을경우
  REFRESH SAVE.
  CLEAR   SAVE.
  SELECT * FROM ZFI01
    WHERE ( BUKRS    =  p_buk )
      AND ( CPUDT    IN p_date )
      AND ( USNAM    IN p_user )
      AND   ( BLART <> 'KK' ).
*    AND   ( CPUDT >= p_date-LOW
*    AND     CPUDT <= p_date-HIGH )
*    AND   ( USNAM >= p_user-LOW
*    AND     USNAM <= p_user-HIGH )
    PERFORM GENERATE_DATA1.
  ENDSELECT.
  IF SY-SUBRC <> 0.
    SKIP 10.
    WRITE : 20 'No Data'.
    EXIT.
  ENDIF.
  WRITE:  / ,
          / ,
         /10  'Data selected...',
         /10  'Proceed.'.

END-OF-SELECTION.

************************************************************************
TOP-OF-PAGE.
  WRITE:  /  ' '.

************************************************************************
TOP-OF-PAGE DURING LINE-SELECTION.
  FORMAT INTENSIFIED OFF.
  FORMAT RESET.
  CASE FCODE.
    WHEN 'Z001'.
      MOVE 'Register documentary evidence' TO TEMP-TEXT.
      WRITE: / '               No Documentary evidance List'.
      WRITE: / '               ============================'.
      WRITE: /
 '--------------------------------------------------------------------'.
      FORMAT COLOR COL_TOTAL.
      WRITE: /
         '|User I.D | Name                         |',
            'System date |Verify|CHK|'.
      WRITE:
 '--------------------------------------------------------------------'.
    WHEN 'Z003'.
      MOVE 'Change Documentary evidance' TO TEMP-TEXT.
      WRITE: / '                      Confimed List  '.
      WRITE: / '                    =================='.
      WRITE: /
 '--------------------------------------------------------------------'.
      FORMAT COLOR COL_TOTAL.
      WRITE: /
         '|User I.D | Name                         |',
            'System date |Verify|CHK|'.
      WRITE:
 '----------------------------------------------------------------'.
    WHEN 'LINE'.
      WRITE: /15 TEMP-TEXT.
      WRITE: / '             ======================'.
*fixme
*      WRITE: / '----------------------------------------------',
*             / '|회사| 입력일   |USER_ID |전표번호  |차수|BA
      write: / '----------------------------------------------'.
  ENDCASE.

************************************************************************
***  (SHIFT+PF1) No documentary evidence
************************************************************************
AT pf13.
  MOVE 'Z001' TO FCODE.
  CLEAR   SAVE.
  REFRESH SAVE.
  SELECT * FROM ZFI01
    WHERE ( BUKRS    = p_buk )
      AND ( CPUDT    IN p_date )
      AND ( USNAM    IN p_user )
      AND ( SELKZ      = ' ' )
      AND ( BLART <> 'KK' ).
    PERFORM GENERATE_DATA1.
  ENDSELECT.
  PERFORM WRITE_SAVE_DATA.

* Register documentary evidence
AT pf16.
  DO.
    CLEAR MARKFIELD1.
    READ LINE SY-INDEX
         FIELD VALUE MARKFIELD1.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    CHECK MARKFIELD1 NE SPACE.
    SELECT * FROM ZFI01
      WHERE ( BUKRS    = p_buk )
      AND   ( CPUDT  = SAVE-DATE )
      AND   ( USNAM  = SAVE-SABUN )
      AND   ( CHASU      = SAVE-CHASU ).
      ZFI01-SELKZ = '*'.
      MODIFY ZFI01.
    ENDSELECT.

    MODIFY CURRENT LINE
    FIELD VALUE  MARKFIELD1 FROM SPACE
                 MARKFIELD2 FROM '*'
                 LINE FORMAT RESET.
  ENDDO.

*F5 : Display documentary evidence
AT pf17.
  MOVE 'Z003' TO FCODE.
  CLEAR   SAVE.
  REFRESH SAVE.
  SELECT * FROM ZFI01
    WHERE ( BUKRS    = p_buk )
      AND ( CPUDT    IN p_date )
      AND ( USNAM    IN p_user )
      AND ( BLART <> 'KK' )
      AND ( SELKZ      = '*' ).
    PERFORM GENERATE_DATA1.
  ENDSELECT.
  PERFORM WRITE_SAVE_DATA.

*F6: Initialize
At pf18.
  check p_ini = 'X'.
  DELETE FROM ZFI01
    WHERE BUKRS = p_buk.

*F7: Change
At pf19.
  SELECT * FROM BKPF
    WHERE BUKRS = p_buk
      AND CPUDT IN p_date
      AND USNAM IN p_user
      and STBLG = space.

* check data
    select single * from zfi01
      WHERE BUKRS = bkpf-bukrs
        AND CPUDT = bkpf-cpudt
        AND belnr = bkpf-belnr.
    if sy-subrc <> 0.
      move-corresponding bkpf to zfi01.
      select single * from bseg
            WHERE BUKRS = bkpf-bukrs
              and gjahr = bkpf-gjahr
              and belnr = bkpf-belnr
              and koart = 'K'
              and SHKZG = 'H'.  " Credit
      check sy-subrc = 0.
      zfi01-WRBTR = bseg-WRBTR.
      zfi01-line  = bseg-buzei.
      insert zfi01.
    endif.
  endselect.

*F8 : Change
At pf20.
  DO.
    CLEAR MARKFIELD1.
    READ LINE SY-INDEX
         FIELD VALUE MARKFIELD1.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    CHECK MARKFIELD1 NE SPACE.
    SELECT * FROM ZFI01
      WHERE ( BUKRS    = p_buk )
      AND   ( CPUDT  = SAVE-DATE )
      AND   ( USNAM  = SAVE-SABUN )
      AND   ( CHASU      = SAVE-CHASU ).
      ZFI01-SELKZ = ' '.
      MODIFY ZFI01.
    ENDSELECT.

    MODIFY CURRENT LINE
    FIELD VALUE  MARKFIELD1 FROM '*'
                 MARKFIELD2 FROM SPACE
                 LINE FORMAT RESET.
  ENDDO.


AT LINE-SELECTION.
  GET CURSOR FIELD SEL_FIELD.
  CASE SEL_FIELD.
    WHEN 'SAVE-SLIP'.
      SELECT SINGLE * FROM VBKPF
             WHERE BUKRS = SAVE-BUKRS
               AND BELNR = SAVE-SLIP
               AND GJAHR = SAVE-BUDAT(4).
      IF SY-SUBRC = SPACE.
        SET PARAMETER ID 'BLN' FIELD SAVE-SLIP.
        SET PARAMETER ID 'BUK' FIELD SAVE-BUKRS.
        SET PARAMETER ID 'GJR' FIELD SAVE-BUDAT(04).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSE.
        MOVE 'FBL1' TO TCODE.
        BUZTAB-ZEILE = SY-LILLI - 5.
        CALL DIALOG 'RF_ZEILEN_ANZEIGE' AND SKIP FIRST SCREEN
             EXPORTING
                  BUZTAB
                  BUZTAB-ZEILE
                  TCODE        FROM TCODE
                  X_NOCHANGE
             IMPORTING
                  BUZTAB
                  X_COMMIT .
      ENDIF.
    WHEN OTHERS.
      MOVE 'LINE' TO FCODE.
      PERFORM SELECT_SAVE_LIST.
  ENDCASE.
*  CLEAR FCODE.
*  IF SY-LISTI = '1'.
*    MOVE 'LINE' TO FCODE.
*    PERFORM SELECT_SAVE_LIST.
*  ELSEIF SY-LISTI = '2'.
*  ENDIF.
*&---------------------------------------------------------------------*
*&      Form GENERATE_DATA1
*&---------------------------------------------------------------------*
FORM GENERATE_DATA1.
*  MOVE  zfi01-bUKRS    TO save-bUKRS.     "회사 코드
  MOVE  ZFI01-CPUDT  TO SAVE-DATE. "시스템 date
  MOVE  ZFI01-USNAM  TO SAVE-SABUN."user_id.
  SELECT * FROM USR21
    WHERE  BNAME  =  SAVE-SABUN.
    SELECT  * FROM  ADCP
      WHERE  PERSNUMBER  =  USR21-PERSNUMBER
      AND    ADDRNUMBER  =  USR21-ADDRNUMBER.
      SAVE-DEPARTMENT      =  ADCP-DEPARTMENT.   " 부서명을 옮긴
    ENDSELECT.
  ENDSELECT.

*  MOVE  zfi01-WRBTR      TO save-WRBTR.       "전표번호
*  MOVE  zfi01-BELNR    TO save-slip.        "전표번호
  MOVE  ZFI01-CHASU      TO SAVE-CHASU.       "전표차수
*  MOVE  zfi01-GUBUN      TO save-GUBUN.       "GUBUN출력
  MOVE  ZFI01-SELKZ      TO SAVE-SELKZ.       "전표check 유무

  COLLECT SAVE.
ENDFORM.                               " GENERATE_DATA1
*&---------------------------------------------------------------------*
*&      Form  WRITE_SAVE_DATA
*&---------------------------------------------------------------------*
FORM WRITE_SAVE_DATA.
  SORT SAVE  BY SABUN CHASU DATE .
  WRITE: / ' '.
  LOOP AT SAVE.
    WRITE: /  ' ',
              SAVE-SABUN,              "user_id.
          15  SAVE-DEPARTMENT,         "사용자명
              SAVE-DATE,               "system date
          60  SAVE-CHASU,              "출력차수
          65  SAVE-SELKZ,              "check field
          67  MARKFIELD1 AS CHECKBOX,
              MARKFIELD2.
    HIDE : SAVE-SABUN, SAVE-DATE, SAVE-CHASU, SAVE-SELKZ, SAVE-SLIP.
  ENDLOOP.

ENDFORM.                               " WRITE_SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form  SELECT_SAVE_LIST
*&---------------------------------------------------------------------*
FORM SELECT_SAVE_LIST.
  CLEAR: TEMP-DATE,
         TEMP-SABUN,
         TEMP-CHASU,
         TEMP-SELKZ.
  MOVE: SAVE-DATE TO TEMP-DATE,
        SAVE-SABUN TO TEMP-SABUN,
        SAVE-CHASU TO TEMP-CHASU,
        SAVE-SELKZ TO TEMP-SELKZ.
  CLEAR   SAVE.
  REFRESH: SAVE,
           BUZTAB.
  SELECT * FROM ZFI01
    WHERE ( BUKRS    = p_buk )
    AND   ( CPUDT  = TEMP-DATE )
    AND   ( USNAM  = TEMP-SABUN )
    AND   ( CHASU      = TEMP-CHASU )
    AND   ( SELKZ      = TEMP-SELKZ )
    AND   ( BLART <> 'KK' ).
    PERFORM GENERATE_DATA2.
  ENDSELECT.
  SORT SAVE DESCENDING BY SABUN CHASU DATE SLIP.            "19990713
  WRITE: / ' '.
  REFRESH BUZTAB.
  LOOP AT SAVE.
    CLEAR BUZTAB.
    BUZTAB-BUKRS = SAVE-BUKRS.
    BUZTAB-BELNR = SAVE-SLIP.
    BUZTAB-GJAHR = SAVE-BUDAT(4).
    BUZTAB-BUZEI = '001'.
    APPEND BUZTAB.
    WRITE: /  ' ' NO-GAP,
              SAVE-BUKRS,              "회사 코드
              SAVE-DATE,               "시스템 date
              SAVE-SABUN,              "user_id.
              SAVE-SLIP HOTSPOT COLOR COL_NORMAL,       "전표번호
           40 SAVE-CHASU,              "출력차수
           44 SAVE-GUBUN.              "GUBUN 출력구분
*           47 SAVE-SELKZ,              "check field
*           49 MARKFIELD1 AS CHECKBOX,
*              MARKFIELD2.
    HIDE : SAVE-SLIP,  SAVE-BUKRS, SAVE-DATE, SAVE-BUDAT.

  ENDLOOP.

ENDFORM.                               " SELECT_SAVE_LIST

*&---------------------------------------------------------------------*
*&      Form  GENERATE_DATA2
*&---------------------------------------------------------------------*
FORM GENERATE_DATA2.
  MOVE  ZFI01-BUKRS    TO SAVE-BUKRS.     "회사 코드
  MOVE  ZFI01-CPUDT  TO SAVE-DATE. "시스템 date
  MOVE  ZFI01-BUDAT  TO SAVE-BUDAT. "전기일자
  MOVE  ZFI01-USNAM  TO SAVE-SABUN."user_id.
  MOVE  ZFI01-BELNR    TO SAVE-SLIP.        "전표번호
  MOVE  ZFI01-CHASU      TO SAVE-CHASU.       "전표차수
  MOVE  ZFI01-GUBUN      TO SAVE-GUBUN.       "GUBUN출력
  MOVE  ZFI01-SELKZ      TO SAVE-SELKZ.       "전표check 유무

  APPEND SAVE.

ENDFORM.                               " GENERATE_DATA2
