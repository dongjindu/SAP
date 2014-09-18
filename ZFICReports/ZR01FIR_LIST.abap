**********************************************************************
* Program name: ZR01FIR_LIST
* Description: 전표조회
* Date/Author: Andy Choi
* Table Updates: 없음
* Input Parameters: (For Includes/ Function Modules)
* Output Parameters: (For Includes/Function Modules)
* Return Codes:
* Special Logic:
* Includes:
**********************************************************************
* MODIFICATION LOG
**********************************************************************
*    Date    Programmer  Request #     Description
* ---------- ----------- --------------------------------------------*
* dd/mm/yy   xxxxxxxxxxx nnnnn         New Program
**********************************************************************
REPORT ZR01FIR_LIST MESSAGE-ID ZFI
                 NO STANDARD PAGE HEADING
*                 LINE-SIZE 142.
                 LINE-SIZE 180.

*                 LINE-COUNT 41.
**********************************************************************
* T A B L E S
**********************************************************************
TABLES: BKPF,                          "회계전표헤더
*       VBKPF,     "예비전기에 대한 전표헤더
        BSEG,                          "회계전표세그멘트
        USR21,                         "사용자이름 주소키 지정
        ADCP,                          "사원/주소지정 (중앙주
        ADRP,      "사원 (중앙주소관리) "2000.05.08 JI.PARK
        T001,                          "회사코드
        T003T,                         "전표유형텍스트
        T007S,                         "세금코드명
        T030K,                         "세금계정결정
        T059ZT,                        "텍스트테이블: 원천세코
        CSKT,                          "코스트센터 텍스트
        KNA1,                          "고객마스터의 일반데이
        LFA1,                          "구매처마스터 (일반섹션
        SKAT,      "G/L 계정 마스터레코드 (계정과목표: 내
        BSEC,                          "일회용계정데이타 전표
        VBSEC,     "예비전기 일회용데이타 전표세그멘트
        VBSEGA,    "자산전표임시저장에 대한 전표세그멘
        VBSEGD,    "고객전표임시저장에 대한 전표세그멘
        VBSEGK,    "구매처전표임시저장에 대한 전표세그
        VBSEGS,    "G/L 계정 전표임시저장에 대한 전표세그
        VBSET,     "세금전표임시저장에 대한 전표세그멘
        WITH_ITEM, "원천세유형 및 FI 개별항목별 원천세정
        ITCPO,                         "SAPscript 출력 인터페이스
        VBKPF,                         "예비전기에 대한 전표헤
        USR01.                         "사용자마스터레코드

**********************************************************************
* DATA
**********************************************************************
DATA: BEGIN OF STAB,
        NAME LIKE TSP03D-NAME,         "2000번 화면의 screen buffer
        PADEST LIKE TSP03D-PADEST,
END OF STAB.
DATA: OK_CODE  LIKE  SY-UCOMM.

* DYNAMIC WHERE절 인터널테이블
DATA: BSTAT_TAB(1) TYPE C OCCURS 0 WITH HEADER LINE,
      AND(3),
      AND1(3).

* 회계전표헤더를 저장할 인터널테이블
DATA: BEGIN OF HDTAB OCCURS 0,
      BUKRS LIKE BKPF-BUKRS,           "회사코드
      BELNR LIKE BKPF-BELNR,           "회계전표번호
      GJAHR LIKE BKPF-GJAHR,           "회계연도
      BLART LIKE BKPF-BLART,           "전표유형
      LTEXT LIKE T003T-LTEXT,          "전표유형내역 (#7)
      BLDAT LIKE BKPF-BLDAT,           "전표내 증빙일
      BUDAT LIKE BKPF-BUDAT,           "전표전기일
      USNAM LIKE BKPF-USNAM,           "사용자이름
      PPNAM LIKE BKPF-PPNAM,       "해당 전표를 임시저장하는
      PERSNUMBER LIKE USR21-PERSNUMBER,"사원번호
      ADDRNUMBER LIKE USR21-ADDRNUMBER,"주소번호
      DEPARTMENT LIKE ADCP-DEPARTMENT, "작성부서 (#8)
      DEPARTMENT1 LIKE ADCP-DEPARTMENT,"전기부서 (#8)
      WAERS LIKE BKPF-WAERS,           "통화키
      BSTAT LIKE BKPF-BSTAT,           "전표상태
      AUSBK LIKE BKPF-AUSBK,           "거래발생회사코드 (#6)
      MARK  TYPE C,                    "선택여부
      SGTXT LIKE BSEG-SGTXT,           "품목텍스트
      DMBTR1(12) TYPE P,
*      DMBTR1(12) TYPE I,
      HKONT LIKE BSEG-HKONT,           "G/L 계정번호
      DMBTR_SUM(12) TYPE P,            "차변금액 합계
*      DMBTR_SUM(12) TYPE I,            "차변금액 합계
      STAT_DOC(8) TYPE C,              "전기/미전기 표시
      TXT20 LIKE SKAT-TXT20,           "G/L 계정내역
      BKTXT LIKE BKPF-BKTXT,           "Header Text
      XBLNR LIKE BKPF-XBLNR,           "Ref.Doc.No.
      STBLG LIKE BKPF-STBLG,           "Reverse Doc.
      END OF HDTAB.

* LINE ITEM을 저장할 인터널테이블
DATA: BEGIN OF LITAB OCCURS 0,
      BELNR LIKE BKPF-BELNR,           "회계전표번호
      BUZEI LIKE BSEG-BUZEI,           "회계전표내 개별항목번
      BUZEIP LIKE BSEG-BUZEI,          "개별항목 조회우선순위
      SHKZG LIKE BSEG-SHKZG,           "차변/대변지시자
      SGTXT LIKE BSEG-SGTXT,           "품목텍스트
      MWSKZ LIKE BSEG-MWSKZ,           "세금코드
      KOART LIKE BSEG-KOART,           "계정유형
      TXT20 LIKE SKAT-TXT20,           "G/L 계정내역 (#6)
      KTOPL LIKE T001-KTOPL,           "계정과목표(미승인)
      KONTS LIKE T030K-KONTS,          "G/L 계정번호(미승인)
      SAKNR LIKE BSEG-SAKNR,           "G/L 계정번호
      HKONT LIKE BSEG-HKONT,           "총계정원장계정
      V_SAKNR LIKE BSEG-SAKNR,         "출력할 계정과목
      WRBTR LIKE BSEG-WRBTR,           "전표통화금액
      DMBTR LIKE BSEG-DMBTR,           "현지통화로 표시한 금액
      DMBTR1(12) TYPE P,
*      DMBTR1(12) TYPE I,
      DMBTR_SUM(12) TYPE P,            "차변금액 합계
*      DMBTR_SUM(12) TYPE I,            "차변금액 합계
* 임시전표이면서 세금자동계산(VBKPF-XMWST = 'X')인경우
      TAX           LIKE  BSET-FWSTE,  "TAX
      GVTYP LIKE BSEG-GVTYP,           "손익계산서 유형
      XNEGP LIKE BSEG-XNEGP,           "Minus
      END OF LITAB.

DATA: BEGIN OF LTAB OCCURS 0,
      BSCHL LIKE BSEG-BSCHL,           "회계전표세그멘트
      MWSKZ LIKE BSEG-MWSKZ,           "세금코드
      SHKZG LIKE BSEG-SHKZG,           "차변/대변지시자
      TAX   LIKE  BSET-FWSTE,
      END OF LTAB.

* 세금코드 NO
DATA: CNT_LITAB(3) TYPE N,
      CNT_LITAB_P(3) TYPE N.

* 고객, 구매처 구분을 위한 인터널테이블
* DATA: FTAB LIKE LITAB OCCURS 0 WITH HEADER LINE.

* 작성부서에 맞는 조건 구하기
DATA: BEGIN OF BNAME_TAB OCCURS 0,
         USNAME     LIKE BKPF-USNAM,
         DEPARTMENT LIKE ADCP-DEPARTMENT,
      END OF BNAME_TAB.

DATA: PERSNUMBER LIKE USR21-PERSNUMBER.

* 세금코드 NO
*DATA: CNT_LITAB(3) TYPE N,
*      CNT_LITAB_P(3) TYPE N.

* SORT를 위한 필드이름
DATA: FIELD_NAME(30) TYPE C,
      FIELD_VALUE(40) TYPE C.
DATA: TAB(15) TYPE C,
      FLD(15) TYPE C.
DATA : CNT_FLAG_I TYPE I,
       TOT_FLAG_I TYPE I.
*       CNT_FLAG(3) TYPE C,
*       TOT_FLAG(3) TYPE C.

*.. 2000.05.08 탐색도움말을 위한 데이터
DATA: SCR_FIELDS LIKE DYNPREAD OCCURS 1 WITH HEADER LINE.

DATA : I_SELECTFIELD   LIKE  HELP_INFO-FIELDNAME,
       I_FIELDS     LIKE  HELP_VALUE OCCURS 1 WITH HEADER LINE,
       I_SELECT_VALUE  LIKE  HELP_INFO-FLDVALUE,
       I_IND      LIKE  SY-TABIX.
DATA T_VBKPF LIKE VBKPF OCCURS 0 WITH HEADER LINE.

* count
DATA    CNT         TYPE I.

* 차/대 합계 체크
DATA:   SUM_DMBTR_S(08) TYPE P,            "원화금액
        SUM_DMBTR_H(08) TYPE P,
        SUM_WRBTR_S(09) TYPE P DECIMALS 2, "외화금액
        SUM_WRBTR_H(09) TYPE P DECIMALS 2.
DATA  P_DMBTR  LIKE LITAB-DMBTR1.
DATA  P_DMBTR1 LIKE LITAB-DMBTR1.
DATA: V_MEG(60) TYPE C,
      LINE      TYPE N.

* 세금코드 테이블
DATA: BEGIN OF TAXTAB OCCURS 0,
      BELNR LIKE BKPF-BELNR,           "회계전표번호
      MWSKZ LIKE BSEG-MWSKZ,           "세금코드
      END OF TAXTAB.

DATA: V_MWSKZ      LIKE BSEG-MWSKZ,    "세금코드
      CNT_TAX(3)   TYPE N,
      CNT_TAX1(3)  TYPE N.

* EXPORT FIELDS
DATA: BEGIN OF REQ OCCURS 0,
          BELNR    LIKE BKPF-BELNR,
          BUKRS    LIKE BKPF-BUKRS,
          GJAHR    LIKE BKPF-GJAHR,
          NAME     LIKE TSP03D-NAME,
          PADEST   LIKE TSP03D-PADEST,
          X(1),
      END OF REQ.

**********************************************************************
* SELECTION SCREEN ( SELECT-OPTIONS & PARAMETERS )
**********************************************************************
PARAMETERS: BUKRS LIKE BKPF-BUKRS MEMORY ID BUK.
**PARAMETERS: BUKRS LIKE BKPF-BUKRS MODIF ID BUK.
SELECT-OPTIONS: BELNR FOR BKPF-BELNR.
SELECT-OPTIONS: GJAHR FOR BKPF-GJAHR.
SELECTION-SCREEN SKIP.

*BLOCK B1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
*SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_BNAME RADIOBUTTON GROUP RDG4.
SELECTION-SCREEN COMMENT  3(14) c001.   "사용자
PARAMETERS P_DEPART RADIOBUTTON GROUP RDG4.
SELECTION-SCREEN COMMENT 20(14) c002.   "부서
SELECTION-SCREEN END OF LINE.
PARAMETERS: BNAME LIKE USR02-BNAME DEFAULT SY-UNAME.
PARAMETERS: DEPARTMT LIKE ADCP-DEPARTMENT.
SELECTION-SCREEN END OF BLOCK B1.

*BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE t003.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS P_POST3 RADIOBUTTON GROUP RDG3.
SELECTION-SCREEN COMMENT  3(14) c007.   "미승인
PARAMETERS P_POST2 RADIOBUTTON GROUP RDG3.
SELECTION-SCREEN COMMENT 20(14) c006.   "승인
PARAMETERS P_POST1 RADIOBUTTON GROUP RDG3.
SELECTION-SCREEN COMMENT 37(14) c008.   "전체
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: BLART FOR BKPF-BLART,
                XBLNR FOR BKPF-XBLNR,
                BKTXT FOR BKPF-BKTXT,
                BLDAT FOR BKPF-BLDAT,
                BUDAT FOR BKPF-BUDAT,
                CPUDT FOR BKPF-CPUDT.
SELECTION-SCREEN END OF BLOCK B2.

*BLOCK B3.
SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME.
PARAMETERS: P_STATS AS CHECKBOX.
*PARAMETERS: P_STATS AS CHECKBOX MODIF ID 001.
SELECTION-SCREEN END OF BLOCK B3.

*BLOCK B4.
SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME.
PARAMETERS: P_SIZE TYPE I DEFAULT '100'.
SELECTION-SCREEN END OF BLOCK B4.

**********************************************************************
*
*
* INITIALIZATION
*
**********************************************************************
*
*
INITIALIZATION.
*.. 2000.05.08 JI.PARK
*.. log-in 한 사람의 부서를 default로 한다.
  SELECT SINGLE PERSNUMBER INTO PERSNUMBER
    FROM USR21
    WHERE BNAME = SY-UNAME.

  SELECT SINGLE DEPARTMENT INTO DEPARTMT
    FROM ADCP
    WHERE PERSNUMBER = PERSNUMBER.

* Initial screen text
  t003 = 'Selection Options'.

  c001 = 'User'.
  c002 = 'Department'.

  c006 = 'Approved'.
  c007 = 'Parked'.
  c008 = 'All'.

  P_POST1 = 'X'.
**********************************************************************
*
*
* AT SELECTION-SCREEN
**********************************************************************
*
*
AT SELECTION-SCREEN.
*  TRANSLATE DEPARTMT TO UPPER CASE.
  IF P_DEPART = 'X'.
    SELECT PERSNUMBER FROM ADCP
      INTO PERSNUMBER
      WHERE DEPARTMENT = DEPARTMT.
    ENDSELECT.

    IF SY-SUBRC <> 0 AND DEPARTMT <> ''.
      SET CURSOR FIELD 'DEPARTMT'.
      MESSAGE E000 WITH 'No entry of department'.
    ENDIF.
  ELSE.
    SELECT BNAME FROM USR21
        INTO BNAME
        WHERE BNAME = BNAME.
    ENDSELECT.

    IF SY-SUBRC <> 0 AND BNAME <> ''.
      SET CURSOR FIELD 'BNAME'.
      MESSAGE E000 WITH 'No entry of user'.
    ENDIF.


  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR DEPARTMT.
*IF P_DEPART = 'X'.
  PERFORM SELECT_F4_DEPARTMT.
*ELSE.
*ENDIF.


**********************************************************************
*
*
* START-OF-SELECTION
*
**********************************************************************
*
*
START-OF-SELECTION.
* CONSTANTS BUKRS(4) VALUE 'L100'.     "회사코드를 'L100'으로 한

  CLEAR: BNAME_TAB, BNAME_TAB[],
         HDTAB, HDTAB[].
***important***********************************************
*  SET PF-STATUS 'MENU'.
***************************************************
  PERFORM SELECT_DATA.

**********************************************************************
*
*
* END-OF-SELECTION
*
**********************************************************************
*
*
END-OF-SELECTION.
  PERFORM WRITE_DATA.

**********************************************************************
*
*
* TOP-OF-PAGE
*
**********************************************************************
*
*
TOP-OF-PAGE.
  PERFORM TOP_PAGE.

TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_PAGE.
**********************************************************************
* END OF PAGE
**********************************************************************
*END-OF-PAGE.

**********************************************************************

**********************************************************************
* AT LINE SELECTION
**********************************************************************
AT LINE-SELECTION.
* 더블클릭 했을경우 전표조회를 한다. (2000.02.24추가)
  PERFORM GET_DETAIL.

**********************************************************************
* AT USER COMMAND
**********************************************************************
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'PICK'.
*      PERFORM GET_DETAIL.
*"REFRESH
    WHEN 'REFR'.
      SY-LSIND = 0.
      PERFORM SELECT_DATA.
      PERFORM WRITE_DATA.
*프린터변경
    WHEN 'PRIN'.
      CALL SCREEN 2000 STARTING AT 40 10      .
*임시전표 전기
    WHEN 'BUCH'.                       "
      PERFORM UPDATE_MARK_FIELD.
      PERFORM POSTING_DOC.
*전표발행
    WHEN 'MAIN'.
      CLEAR: CNT_LITAB_P, CNT_LITAB.

      PERFORM UPDATE_MARK_FIELD.
      PERFORM HANDLING_SAPSCRIPT.
*전체선택
    WHEN 'MARK'.
      LOOP AT HDTAB.
        HDTAB-MARK = 'X'.
        MODIFY HDTAB.
      ENDLOOP.
      PERFORM WRITE_DATA.
      SY-LSIND = SY-LSIND - 1.
*Deselect
    WHEN 'DMAR'.
      LOOP AT HDTAB.
        HDTAB-MARK = SPACE.
        MODIFY HDTAB.
      ENDLOOP.
      PERFORM WRITE_DATA.
      SY-LSIND = SY-LSIND - 1.
    WHEN 'SORT'. "정렬
      PERFORM UPDATE_MARK_FIELD.
      CLEAR: FIELD_NAME, TAB, FLD, FIELD_VALUE.
      GET CURSOR FIELD FIELD_NAME VALUE FIELD_VALUE.
      PERFORM GET_FIELD_TITEL.
      SPLIT FIELD_NAME AT '-' INTO TAB FLD.
      SORT HDTAB BY (FLD).
      PERFORM WRITE_DATA.
      SY-LSIND = SY-LSIND - 1.
    WHEN 'TROS'."정렬
      PERFORM UPDATE_MARK_FIELD.
      CLEAR: FIELD_NAME, TAB, FLD, FIELD_VALUE.
      GET CURSOR FIELD FIELD_NAME VALUE FIELD_VALUE.
      PERFORM GET_FIELD_TITEL.
      SPLIT FIELD_NAME AT '-' INTO TAB FLD.
      SORT HDTAB BY (FLD) DESCENDING.
      PERFORM WRITE_DATA.
      SY-LSIND = SY-LSIND - 1.
  ENDCASE.
**********************************************************************
* FORMS
**********************************************************************

*&--------------------------------------------------------------------
*
*&      Form  SELECT_DATA
*&--------------------------------------------------------------------
*
FORM SELECT_DATA.
  REFRESH: BNAME_TAB, HDTAB, LITAB.
  CLEAR:   BNAME_TAB, HDTAB, LITAB.

* header 구하기.
  PERFORM GET_HEADER_DATA.

* LINE ITEM 구하기.
  PERFORM GET_LINE_ITEM_DATA.

ENDFORM.                               " SELECT_DATA
*&--------------------------------------------------------------------
*
*&      Form  GET_HEADER_DATA
*&--------------------------------------------------------------------
*
FORM GET_HEADER_DATA.
  DATA: CHK_DATA.
  CLEAR CNT.
* 작성부서에 맞는 조건을 구하기.
*  BREAK-POINT.
  IF DEPARTMT <> '' AND P_DEPART = 'X'.
    SELECT PERSNUMBER FROM ADCP
        INTO PERSNUMBER
        WHERE DEPARTMENT = DEPARTMT.

      SELECT BNAME FROM USR21 INTO BNAME_TAB-USNAME
        WHERE PERSNUMBER    = PERSNUMBER.
        BNAME_TAB-DEPARTMENT = DEPARTMT.
        APPEND BNAME_TAB.
      ENDSELECT.
    ENDSELECT.
  ELSEIF BNAME <> '' AND P_BNAME = 'X'..
    SELECT BNAME FROM USR01 INTO BNAME_TAB-USNAME
      WHERE BNAME    = BNAME.
      APPEND BNAME_TAB.
    ENDSELECT.
  ENDIF.
  IF DEPARTMT = '' AND P_DEPART = 'X' OR
        BNAME = '' AND P_BNAME = 'X'.
* 부서를 입력하지 않으면, 모두 선택.
* 조건에 맞는 데이터 회계전표헤더 테이블(BKPF)에서
*.. 2.최대적중수(p_size)만큼 선택한다.
    IF     P_POST1 = 'X'.              "전체
      PERFORM ALL_SELECT_NO_USER.
    ELSEIF P_POST2 = 'X'.              "승인
      PERFORM CONFIRM_SELECT_NO_USER.
    ELSEIF P_POST3 = 'X'.              "미승인
      PERFORM NO_CONFIRM_SELECT_NO_USER.
    ENDIF.
  ELSE.
    LOOP AT BNAME_TAB.
      HDTAB-DEPARTMENT = BNAME_TAB-DEPARTMENT.
* 조건에 맞는 데이터 회계전표헤더 테이블(BKPF)에서
*.. 2.최대적중수(p_size)만큼 선택한다.
      IF     P_POST1 = 'X'.              "전체
        PERFORM ALL_SELECT.
      ELSEIF P_POST2 = 'X'.              "승인
        PERFORM CONFIRM_SELECT.
      ELSEIF P_POST3 = 'X'.              "미승인
        PERFORM NO_CONFIRM_SELECT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF CNT => P_SIZE.
    MESSAGE S999 WITH 'Select just ' P_SIZE ' entries.'.
*    EXIT.
  ELSE.
    MESSAGE S999 WITH '' CNT ' is selected'.
  ENDIF.
*  SORT HDTAB BY BUDAT BELNR. "전기일
  SORT HDTAB BY BLDAT BELNR. "증빙일
*  SORT HDTAB BY BELNR.

  CLEAR: BNAME_TAB, BNAME_TAB[],
         BSTAT_TAB, BSTAT_TAB[].
ENDFORM.                               " GET_HEADER_DATA
*&--------------------------------------------------------------------
*
*&      Form  NO_CONFIRM_SELECT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM NO_CONFIRM_SELECT.
  SELECT       BUKRS         BELNR        GJAHR        BLART
               BLDAT         BUDAT        USNAM
               WAERS         BSTAT        BKTXT        XBLNR
    INTO (HDTAB-BUKRS, HDTAB-BELNR, HDTAB-GJAHR, HDTAB-BLART,
          HDTAB-BLDAT, HDTAB-BUDAT, HDTAB-USNAM,
          HDTAB-WAERS, HDTAB-BSTAT, HDTAB-BKTXT,HDTAB-XBLNR)
    FROM VBKPF
*   UP TO P_SIZE ROWS                                     " 2
    WHERE BUKRS = BUKRS
      AND USNAM = BNAME_TAB-USNAME
      AND BSTAT IN ('V', 'W')
      AND GJAHR IN GJAHR
      AND BLART IN BLART
      AND XBLNR IN XBLNR
      AND BLDAT IN BLDAT
      AND CPUDT IN CPUDT
      AND BKTXT IN BKTXT
      AND BUDAT IN BUDAT
      AND BELNR IN BELNR
*      ORDER BY BUKRS BLDAT DESCENDING.
      ORDER BY BUKRS BLDAT ASCENDING.
    IF SY-SUBRC = 0.
      PERFORM GET_DOCUMENT_TYPE.
      PERFORM GET_DEPARTMENT.
      APPEND HDTAB.
      CLEAR  HDTAB.
      CNT = CNT + 1.
      IF CNT => P_SIZE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                               " NO_CONFIRM_SELECT

*&--------------------------------------------------------------------
*
*&      Form  ALL_SELECT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM ALL_SELECT.
* 전기전표 + 미전기전표
  SELECT       BUKRS         BELNR        GJAHR        BLART
               BLDAT         BUDAT        USNAM        PPNAM
               WAERS         BSTAT        BKTXT        XBLNR
               STBLG
    INTO (HDTAB-BUKRS, HDTAB-BELNR, HDTAB-GJAHR, HDTAB-BLART,
          HDTAB-BLDAT, HDTAB-BUDAT, HDTAB-USNAM, HDTAB-PPNAM,
          HDTAB-WAERS, HDTAB-BSTAT, HDTAB-BKTXT, HDTAB-XBLNR,
          HDTAB-STBLG)
    FROM BKPF
*   UP TO P_SIZE ROWS                                     " 2
    WHERE BUKRS = BUKRS
      AND GJAHR IN GJAHR
      AND BLART IN BLART
      AND XBLNR IN XBLNR
      AND BKTXT IN BKTXT
      AND BLDAT IN BLDAT
      AND BUDAT IN BUDAT
      AND CPUDT IN CPUDT
      AND BELNR IN BELNR
      AND ( ( ( BSTAT IN ('A', 'B', 'S', '') AND
              USNAM = BNAME_TAB-USNAME AND PPNAM = '' ) OR " 전기전
            ( BSTAT IN ('A', 'B', 'S', '') AND
              PPNAM = BNAME_TAB-USNAME AND PPNAM <> '' ) )

"전기임시전표
*
      OR  ( BSTAT IN ('V','W') AND USNAM = BNAME_TAB-USNAME  ) )
*      ORDER BY BUKRS BLDAT DESCENDING.
      ORDER BY BUKRS BLDAT ASCENDING.

    IF SY-SUBRC = 0.
      IF P_STATS = 'X' OR
         P_STATS <> 'X' AND HDTAB-STBLG =''.
        PERFORM GET_DOCUMENT_TYPE.
        PERFORM GET_DEPARTMENT.
        APPEND HDTAB.
        CNT = CNT + 1.
      ENDIF.
      CLEAR  HDTAB.
      IF CNT => P_SIZE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                               " ALL_SELECT
*&--------------------------------------------------------------------
*
*&      Form  CONFIRM_SELECT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM CONFIRM_SELECT.
* 전기전표
  SELECT       BUKRS         BELNR        GJAHR        BLART
               BLDAT         BUDAT        USNAM        PPNAM
               WAERS         BSTAT        BKTXT        XBLNR
               STBLG
    INTO (HDTAB-BUKRS, HDTAB-BELNR, HDTAB-GJAHR, HDTAB-BLART,
          HDTAB-BLDAT, HDTAB-BUDAT, HDTAB-USNAM, HDTAB-PPNAM,
          HDTAB-WAERS, HDTAB-BSTAT, HDTAB-BKTXT, HDTAB-XBLNR,
          HDTAB-STBLG)
    FROM BKPF
*    UP TO P_SIZE ROWS                                     " 2
    WHERE BUKRS = BUKRS
      AND BSTAT IN ('A', 'B', 'S',  '')
      AND GJAHR IN GJAHR
      AND BLART IN BLART
      AND XBLNR IN XBLNR
      AND BKTXT IN BKTXT
      AND BLDAT IN BLDAT
      AND BUDAT IN BUDAT
      AND CPUDT IN CPUDT
      AND BELNR IN BELNR
      AND ( ( USNAM = BNAME_TAB-USNAME AND PPNAM = '' )" 전기전표
      OR  (   PPNAM = BNAME_TAB-USNAME AND PPNAM <> '' ) )
*      ORDER BY BUKRS BLDAT DESCENDING.
      ORDER BY BUKRS BLDAT ASCENDING.

    "전기된 임시전표
    IF SY-SUBRC = 0.
      IF P_STATS = 'X' OR
         P_STATS <> 'X' AND HDTAB-STBLG =''.
        PERFORM GET_DOCUMENT_TYPE.
        PERFORM GET_DEPARTMENT.
        APPEND HDTAB.
        CNT = CNT + 1.
      ENDIF.
      CLEAR  HDTAB.

      IF CNT => P_SIZE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                               " CONFIRM_SELECT
*&--------------------------------------------------------------------
*&      Form  NO_CONFIRM_SELECT WITHOUT USER
*---------------------------------------------------------------------
FORM NO_CONFIRM_SELECT_NO_USER.
  SELECT       BUKRS         BELNR        GJAHR        BLART
               BLDAT         BUDAT        USNAM
               WAERS         BSTAT        BKTXT        XBLNR
    INTO (HDTAB-BUKRS, HDTAB-BELNR, HDTAB-GJAHR, HDTAB-BLART,
          HDTAB-BLDAT, HDTAB-BUDAT, HDTAB-USNAM,
          HDTAB-WAERS, HDTAB-BSTAT, HDTAB-BKTXT, HDTAB-XBLNR)
    FROM VBKPF
*    UP TO P_SIZE ROWS                                     " 2
    WHERE BUKRS = BUKRS
      AND BSTAT IN ('V', 'W')
      AND GJAHR IN GJAHR
      AND BLART IN BLART
      AND XBLNR IN XBLNR
      AND BKTXT IN BKTXT
      AND BLDAT IN BLDAT
      AND BUDAT IN BUDAT
      AND CPUDT IN CPUDT
      AND BELNR IN BELNR
*      ORDER BY BUKRS BLDAT DESCENDING.
      ORDER BY BUKRS BLDAT ASCENDING.
    IF SY-SUBRC = 0.
      PERFORM GET_DOCUMENT_TYPE.
      PERFORM GET_DEPARTMENT.
      APPEND HDTAB.
      CLEAR  HDTAB.
      CNT = CNT + 1.
      IF CNT => P_SIZE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                               " NO_CONFIRM_SELECT

*&--------------------------------------------------------------------
*&      Form  ALL_SELECT WITHOUT USER
*&--------------------------------------------------------------------
FORM ALL_SELECT_NO_USER.
* 전기전표 + 미전기전표
  SELECT       BUKRS         BELNR        GJAHR        BLART
               BLDAT         BUDAT        USNAM        PPNAM
               WAERS         BSTAT        BKTXT        XBLNR
               STBLG
    INTO (HDTAB-BUKRS, HDTAB-BELNR, HDTAB-GJAHR, HDTAB-BLART,
          HDTAB-BLDAT, HDTAB-BUDAT, HDTAB-USNAM, HDTAB-PPNAM,
          HDTAB-WAERS, HDTAB-BSTAT, HDTAB-BKTXT, HDTAB-XBLNR,
          HDTAB-STBLG)
    FROM BKPF
*    UP TO P_SIZE ROWS                                     " 2
    WHERE BUKRS = BUKRS
      AND GJAHR IN GJAHR
      AND BLART IN BLART
      AND XBLNR IN XBLNR
      AND BKTXT IN BKTXT
      AND BLDAT IN BLDAT
      AND BUDAT IN BUDAT
      AND CPUDT IN CPUDT
      AND BELNR IN BELNR
      AND ( ( BSTAT IN ('A', 'B', 'S', '') )  OR
            ( BSTAT IN ('V','W' ) ) )
*      ORDER BY BUKRS BLDAT DESCENDING.
      ORDER BY BUKRS BLDAT ASCENDING.

    IF SY-SUBRC = 0.
      IF P_STATS = 'X' OR
         P_STATS <> 'X' AND HDTAB-STBLG =''.
        PERFORM GET_DOCUMENT_TYPE.
        PERFORM GET_DEPARTMENT.
        APPEND HDTAB.
        CNT = CNT + 1.
      ENDIF.
      CLEAR  HDTAB.
      IF CNT => P_SIZE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                               " ALL_SELECT
*&--------------------------------------------------------------------
*&      Form  CONFIRM_SELECT WITHOUT USER
*&--------------------------------------------------------------------
FORM CONFIRM_SELECT_NO_USER.
* 전기전표
  SELECT       BUKRS         BELNR        GJAHR        BLART
               BLDAT         BUDAT        USNAM        PPNAM
               WAERS         BSTAT        BKTXT        XBLNR
               STBLG
    INTO (HDTAB-BUKRS, HDTAB-BELNR, HDTAB-GJAHR, HDTAB-BLART,
          HDTAB-BLDAT, HDTAB-BUDAT, HDTAB-USNAM, HDTAB-PPNAM,
          HDTAB-WAERS, HDTAB-BSTAT, HDTAB-BKTXT, HDTAB-XBLNR,
          HDTAB-STBLG)
    FROM BKPF
*    UP TO P_SIZE ROWS                                     " 2
    WHERE BUKRS = BUKRS
      AND BSTAT IN ('A', 'B', 'S',  '')
      AND GJAHR IN GJAHR
      AND BLART IN BLART
      AND XBLNR IN XBLNR
      AND BKTXT IN BKTXT
      AND BLDAT IN BLDAT
      AND BUDAT IN BUDAT
      AND CPUDT IN CPUDT
      AND BELNR IN BELNR
*      ORDER BY BUKRS BLDAT DESCENDING.
      ORDER BY BUKRS BLDAT ASCENDING.

    "전기된 임시전표
    IF SY-SUBRC = 0.
      IF P_STATS = 'X' OR
         P_STATS <> 'X' AND HDTAB-STBLG =''.
        PERFORM GET_DOCUMENT_TYPE.
        PERFORM GET_DEPARTMENT.
        APPEND HDTAB.
        CNT = CNT + 1.
      ENDIF.
      CLEAR  HDTAB.
      IF CNT => P_SIZE.
        EXIT.
      ENDIF.
    ENDIF.
  ENDSELECT.
ENDFORM.                               " CONFIRM_SELECT

*&--------------------------------------------------------------------
*
*&      Form  GET_DOCUMENT_TYPE
*&--------------------------------------------------------------------
*
FORM GET_DOCUMENT_TYPE.
* 전표유형 내역 구하기.
  SELECT SINGLE LTEXT FROM T003T
    INTO HDTAB-LTEXT
    WHERE SPRAS = sy-langu
      AND BLART = HDTAB-BLART.
ENDFORM.                               " GET_DOCUMENT_TYPE
*&--------------------------------------------------------------------
*&      Form  GET_DEPARTMENT
*&--------------------------------------------------------------------
FORM GET_DEPARTMENT.
  SELECT SINGLE PERSNUMBER INTO PERSNUMBER
    FROM USR21
    WHERE BNAME = HDTAB-USNAM.

  SELECT SINGLE DEPARTMENT INTO HDTAB-DEPARTMENT
    FROM ADCP
    WHERE PERSNUMBER = PERSNUMBER.
  IF HDTAB-BSTAT = '' OR HDTAB-BSTAT = 'A' OR HDTAB-BSTAT = 'B'
  OR HDTAB-BSTAT = 'S'.
    HDTAB-DEPARTMENT1 = HDTAB-DEPARTMENT.

    SELECT SINGLE PERSNUMBER ADDRNUMBER FROM USR21
      INTO (HDTAB-PERSNUMBER, HDTAB-ADDRNUMBER)
      WHERE BNAME = HDTAB-PPNAM.

    SELECT SINGLE DEPARTMENT FROM ADCP
      INTO HDTAB-DEPARTMENT
      WHERE ADDRNUMBER = HDTAB-ADDRNUMBER "주소
        AND PERSNUMBER = HDTAB-PERSNUMBER.  "사원번호
*                DATE_FROM                       "유효일에서 시작
*                NATION                          "국제주소버젼 ID
    IF SY-SUBRC <> 0.
      HDTAB-DEPARTMENT = HDTAB-DEPARTMENT1.
    ELSEIF SY-SUBRC = 0.

      SELECT SINGLE PERSNUMBER ADDRNUMBER FROM USR21
        INTO (HDTAB-PERSNUMBER, HDTAB-ADDRNUMBER)
        WHERE BNAME = HDTAB-USNAM.

      SELECT SINGLE DEPARTMENT FROM ADCP
        INTO HDTAB-DEPARTMENT1
        WHERE ADDRNUMBER = HDTAB-ADDRNUMBER "주소
          AND PERSNUMBER = HDTAB-PERSNUMBER.  "사원번호
*                DATE_FROM                       "유효일에서 시작
*                NATION                          "국제주소버젼 ID

    ENDIF.
  ELSE.
* 임시전표를 전기하게 되면, 작성한 사람은 usnam 에서
* usnam에는 전기한 사람이 저장된다.
    HDTAB-PPNAM = HDTAB-USNAM.
    HDTAB-USNAM = ''.
  ENDIF.

ENDFORM.                               " GET_DEPARTMENT
*&--------------------------------------------------------------------
*
*&      Form  GET_LINE_ITEM_DATA
*&--------------------------------------------------------------------
*
FORM GET_LINE_ITEM_DATA.
  CLEAR: LITAB, LITAB[].
  LOOP AT HDTAB.

* 조건에 맞는 회계전표세그먼트(BSEG) TABLE의 DATA 를 가
*    SELECT              BELNR        SGTXT        DMBTR        WRBTR
*                        HKONT        SHKZG        MWSKZ        BUZEI
*      APPENDING CORRESPONDING FIELDS OF TABLE LITAB
*      FROM BSEG
*      WHERE BUKRS = HDTAB-BUKRS
*        AND BELNR = HDTAB-BELNR
*        AND GJAHR = HDTAB-GJAHR        "현재연도를 회계연도로
*        AND WRBTR NE 0.

    SELECT              BELNR        SGTXT        DMBTR        WRBTR
                        HKONT        SHKZG        MWSKZ        BUZEI
                        KOART        GVTYP        XNEGP
            INTO (LITAB-BELNR, LITAB-SGTXT, LITAB-DMBTR, LITAB-WRBTR,
                  LITAB-HKONT, LITAB-SHKZG, LITAB-MWSKZ, LITAB-BUZEI,
                  LITAB-KOART, LITAB-GVTYP, LITAB-XNEGP)
            FROM BSEG
            WHERE BUKRS = HDTAB-BUKRS
              AND BELNR = HDTAB-BELNR
              AND GJAHR = HDTAB-GJAHR
              AND WRBTR NE 0.
      CASE LITAB-KOART.
        WHEN 'A' OR 'M'.
          LITAB-BUZEIP = 1.
        WHEN 'S'.
          IF LITAB-GVTYP <> ''.
            LITAB-BUZEIP = 2.
          ELSE.
            LITAB-BUZEIP = 3.
          ENDIF.
        WHEN 'K' OR 'D'.
          LITAB-BUZEIP = 4.
      ENDCASE.
      PERFORM HANDLING_DATA.
      APPEND LITAB.
    ENDSELECT.

    IF HDTAB-BSTAT = 'V' OR HDTAB-BSTAT = 'W'.
* 미승인 임시전표일 때.
* 미승인 전표이면 전기부서에 Dummy를 넣는다.
      HDTAB-DEPARTMENT1 = ''.
      MODIFY HDTAB.

      SELECT              BELNR        SGTXT        DMBTR        WRBTR
                          HKONT        SHKZG        MWSKZ        BUZEI
            INTO (LITAB-BELNR, LITAB-SGTXT, LITAB-DMBTR, LITAB-WRBTR,
                  LITAB-HKONT, LITAB-SHKZG, LITAB-MWSKZ, LITAB-BUZEI)
              FROM VBSEGK "(구매처전표임시저장에 대한 전표
              WHERE AUSBK = HDTAB-BUKRS
                AND BELNR = HDTAB-BELNR
                AND GJAHR = HDTAB-GJAHR
                AND WRBTR NE 0.

        LITAB-KOART = 'K'.
        LITAB-BUZEIP = 4.
        APPEND LITAB.
      ENDSELECT.

      SELECT              BELNR        SGTXT        DMBTR        WRBTR
                          HKONT        SHKZG        MWSKZ        BUZEI
            INTO (LITAB-BELNR, LITAB-SGTXT, LITAB-DMBTR, LITAB-WRBTR,
                  LITAB-HKONT, LITAB-SHKZG, LITAB-MWSKZ, LITAB-BUZEI)
              FROM VBSEGD "(고객전표임시저장에 대한 전표세
              WHERE AUSBK = HDTAB-BUKRS
                AND BELNR = HDTAB-BELNR
                AND GJAHR = HDTAB-GJAHR
                AND WRBTR NE 0.

        LITAB-KOART = 'D'.
        LITAB-BUZEIP = 4.
        APPEND LITAB.
      ENDSELECT.

*      SELECT             BELNR        SGTXT        DMBTR       WRBTR
*                         HKONT        SHKZG        MWSKZ       BUZEI
*        APPENDING CORRESPONDING FIELDS OF TABLE LITAB
*        FROM VBSEGA
*        WHERE AUSBK = HDTAB-BUKRS
*          AND BELNR = HDTAB-BELNR
*          AND GJAHR = HDTAB-GJAHR      "현재연도를 회계연도로
*          AND WRBTR NE 0.
      SELECT              BELNR        SGTXT        DMBTR        WRBTR
                          HKONT        SHKZG        MWSKZ        BUZEI
              INTO (LITAB-BELNR, LITAB-SGTXT, LITAB-DMBTR, LITAB-WRBTR
,
                    LITAB-HKONT, LITAB-SHKZG, LITAB-MWSKZ,
LITAB-BUZEI)
              FROM VBSEGA "(자산전표임시저장에 대한 전표세
              WHERE AUSBK = HDTAB-BUKRS
                AND BELNR = HDTAB-BELNR
                AND GJAHR = HDTAB-GJAHR
                AND WRBTR NE 0.

        LITAB-KOART = 'A'.
        LITAB-BUZEIP = 1.
        APPEND LITAB.
      ENDSELECT.

*      SELECT              BELNR        SGTXT        DMBTR       WRBTR
*                          SAKNR        SHKZG        MWSKZ       BUZEI
*        APPENDING CORRESPONDING FIELDS OF TABLE LITAB
*        FROM VBSEGS
*        WHERE AUSBK = HDTAB-BUKRS
*          AND BELNR = HDTAB-BELNR
*          AND GJAHR = HDTAB-GJAHR      "현재연도를 회계연도로
*          AND WRBTR NE 0.
      SELECT              BELNR        SGTXT        DMBTR        WRBTR
                          SAKNR        SHKZG        MWSKZ        BUZEI
              INTO (LITAB-BELNR, LITAB-SGTXT, LITAB-DMBTR, LITAB-WRBTR
,
                    LITAB-HKONT, LITAB-SHKZG, LITAB-MWSKZ,
LITAB-BUZEI)
              FROM VBSEGS "(G/L자산전표임시저장에 대한 전표
              WHERE AUSBK = HDTAB-BUKRS
                AND BELNR = HDTAB-BELNR
                AND GJAHR = HDTAB-GJAHR
                AND WRBTR NE 0.

        LITAB-KOART = 'S'.
        LITAB-BUZEIP = 3.
        APPEND LITAB.
      ENDSELECT.

      PERFORM HANDLING_DATA_LINE.
    ELSE.
*
    ENDIF.

* 전표금액
    PERFORM UPDATE_DMBTR_SUM.

  ENDLOOP.
*  SORT LITAB BY BELNR BUZEI.
  SORT LITAB BY BELNR BUZEIP WRBTR DESCENDING BUZEI.
ENDFORM.                               " GET_LINE_ITEM_DATA
*&--------------------------------------------------------------------
*   승인전표
*&      Form  HANDLING_DATA
*&--------------------------------------------------------------------
FORM HANDLING_DATA.
* G/L 계정내역 구하기
  SELECT SINGLE KTOPL FROM T001
    INTO LITAB-KTOPL
    WHERE BUKRS = HDTAB-BUKRS.

  IF LITAB-SAKNR <> ''.
    LITAB-V_SAKNR = LITAB-SAKNR.
  ELSEIF LITAB-HKONT <> ''.
    LITAB-V_SAKNR = LITAB-HKONT.
  ENDIF.
  SELECT SINGLE TXT20 FROM SKAT
    INTO LITAB-TXT20
    WHERE SPRAS = sy-langu
      AND KTOPL = LITAB-KTOPL
      AND SAKNR = LITAB-V_SAKNR.

  LITAB-DMBTR1 = CEIL( LITAB-DMBTR ).
*    LITAB-DMBTR1 = CEIL( LITAB-DMBTR * 100 ).
  PERFORM GET_DMBTR_SUM.        " 전표금액합계
ENDFORM.                               " HANDLING_DATA
*&--------------------------------------------------------------------
*&      Form  GET_DMBTR_SUM
*&--------------------------------------------------------------------
FORM GET_DMBTR_SUM.
* 차변금액을 합한다.
  IF LITAB-SHKZG = 'S' OR HDTAB-BSTAT = 'S' . "비망
    P_DMBTR = P_DMBTR + LITAB-DMBTR1.
  ENDIF.
* Minus 금액을 고려하여 Neting 한다.
  IF  HDTAB-BSTAT = 'S' OR
     ( LITAB-SHKZG = 'S' AND LITAB-XNEGP <> 'X' ).
    P_DMBTR1 = P_DMBTR1 + LITAB-DMBTR1.
  ELSEIF LITAB-SHKZG = 'H' AND LITAB-XNEGP = 'X'.
    P_DMBTR1 = P_DMBTR1 - LITAB-DMBTR1.
  ENDIF.
ENDFORM.
*&--------------------------------------------------------------------
*  미승인 전표
*&      Form  HANDLING_DATA_LINE
*&--------------------------------------------------------------------
*
FORM HANDLING_DATA_LINE.
  CLEAR : CNT_TAX, CNT_TAX1.
* 세금라인아이템 구하기. (2000.02.16 수정)
  SORT  LITAB BY BELNR BUZEI.
  LOOP AT LITAB WHERE BELNR = HDTAB-BELNR.
    CNT_TAX = CNT_TAX + 1.
    MOVE-CORRESPONDING LITAB TO TAXTAB.
    COLLECT TAXTAB.
  ENDLOOP.
  IF CNT_TAX < LITAB-BUZEI.
    CNT_TAX = LITAB-BUZEI.
  ENDIF.
  CNT_TAX1 = CNT_TAX + 1.   " 항목갯수 + 1

  CLEAR  VBKPF.
  SELECT SINGLE XMWST INTO VBKPF-XMWST
           FROM VBKPF
           WHERE BUKRS = HDTAB-BUKRS
             AND BELNR = HDTAB-BELNR
             AND GJAHR = HDTAB-GJAHR.
  IF VBKPF-XMWST = 'X'.
    PERFORM  TAX_DATA_2.              "세금이 자동으로 계산된
  ELSE.
    PERFORM  TAX_DATA_1.              "VBSET 에 TAX 존재
  ENDIF.
  LOOP AT LITAB WHERE BELNR = HDTAB-BELNR.
* G/L 계정내역 구하기
    SELECT SINGLE KTOPL FROM T001
      INTO LITAB-KTOPL
      WHERE BUKRS = HDTAB-BUKRS.
    IF LITAB-BUZEI > CNT_TAX.          " 세금코드라인아이템일
      SELECT SINGLE KONTS FROM T030K
        INTO LITAB-KONTS
        WHERE KTOPL = LITAB-KTOPL
          AND MWSKZ = LITAB-MWSKZ.

      SELECT SINGLE SAKNR FROM SKAT
        INTO LITAB-SAKNR
        WHERE SPRAS = sy-langu
          AND KTOPL = LITAB-KTOPL
          AND SAKNR = LITAB-KONTS.

      SELECT SINGLE TXT20 FROM SKAT
        INTO LITAB-TXT20
        WHERE SPRAS = sy-langu
          AND KTOPL = LITAB-KTOPL
          AND SAKNR = LITAB-SAKNR.

* 계정과목
      LITAB-V_SAKNR = LITAB-SAKNR.
      LITAB-BUZEIP = 3.
      LITAB-KOART = 'S'.
    ELSEIF LITAB-BUZEI <= CNT_TAX.     " 일반일 경우
      SELECT SINGLE KTOPL FROM T001
        INTO LITAB-KTOPL
        WHERE BUKRS = HDTAB-BUKRS.

* 계정과목 (2000.02.16 추가)
      IF LITAB-SAKNR <> ''.
        LITAB-V_SAKNR = LITAB-SAKNR.
      ELSEIF LITAB-HKONT <> ''.
        LITAB-V_SAKNR = LITAB-HKONT.
      ENDIF.

      SELECT SINGLE TXT20 FROM SKAT
        INTO LITAB-TXT20
        WHERE SPRAS = sy-langu
          AND KTOPL = LITAB-KTOPL
          AND SAKNR = LITAB-V_SAKNR.
      IF LITAB-KOART = 'S'.
        SELECT SINGLE GVTYP FROM SKA1
           INTO LITAB-GVTYP
           WHERE KTOPL = LITAB-KTOPL
           AND SAKNR = LITAB-V_SAKNR.
        IF LITAB-GVTYP <> ''.
          LITAB-BUZEIP = 2.
        ENDIF.
      ENDIF.
    ENDIF.

*    LITAB-DMBTR1 = CEIL( LITAB-DMBTR * 100 ).
    LITAB-DMBTR1 = CEIL( LITAB-DMBTR ).
    PERFORM GET_DMBTR_SUM.        " 전표금액합계
    MODIFY LITAB.
    CLEAR LITAB.
  ENDLOOP.
ENDFORM.                               " HANDLING_DATA_LINE
*&--------------------------------------------------------------------
*
*&      Form  TAX_DATA_2
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM TAX_DATA_2.
  REFRESH LTAB.
  CLEAR   LTAB.
* 같은전표에 세금코드명칭이 같은건은 합쳐서 표기
* 전표번호+전표SEGMENT(PK)+세금코드값
  LOOP AT LITAB  WHERE BELNR = HDTAB-BELNR
                   AND MWSKZ <> ' '
                   AND KOART <> 'K'
                   AND KOART <> 'D'.
*                   AND KOART = ' '.         "계정유형이 K,D 만
    CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
         EXPORTING
              I_BUKRS = HDTAB-BUKRS
              I_MWSKZ = LITAB-MWSKZ
              I_WAERS = 'KRW'
              I_WRBTR = LITAB-WRBTR
         IMPORTING
              E_FWSTE = LITAB-TAX.

    IF LITAB-TAX NE 0.
      LITAB-DMBTR = LITAB-DMBTR - LITAB-TAX.   "TAX를 뺀 금액으로
      MODIFY LITAB.
    ENDIF.
    MOVE-CORRESPONDING LITAB TO LTAB.
    COLLECT LTAB.
    CLEAR   LTAB.
  ENDLOOP.
  LOOP AT LTAB WHERE TAX > 0.
    LITAB-BELNR = HDTAB-BELNR.
    LITAB-MWSKZ = V_MWSKZ.
    LITAB-BUZEI = CNT_TAX1.
    CNT_TAX1 = CNT_TAX1 + 1.
    APPEND LITAB.
  ENDLOOP.
ENDFORM.                    " TAX_DATA_2
*&--------------------------------------------------------------------
*
*&      Form  TAX_DATA_1
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM TAX_DATA_1.
  SELECT SHKZG MWSKZ FWSTE HWSTE FROM VBSET
      INTO (LITAB-SHKZG, V_MWSKZ, LITAB-WRBTR, LITAB-DMBTR)
      WHERE AUSBK = HDTAB-BUKRS
        AND BELNR = HDTAB-BELNR
        AND GJAHR = HDTAB-GJAHR
        AND FWSTE NE 0.

    READ TABLE TAXTAB WITH KEY BELNR = HDTAB-BELNR
                               MWSKZ = V_MWSKZ.

    IF SY-SUBRC = 0.
      LITAB-BELNR = HDTAB-BELNR.
      APPEND LITAB.
    ENDIF.
  ENDSELECT.
ENDFORM.                    " TAX_DATA_1
*&--------------------------------------------------------------------
*
*&      Form  UPDATE_DMBTR_SUM
*&--------------------------------------------------------------------
FORM UPDATE_DMBTR_SUM.
  LOOP AT LITAB WHERE BELNR = HDTAB-BELNR.
    LITAB-DMBTR_SUM = P_DMBTR.
    IF P_DMBTR1 <> 0. " Minus전기 고려한 금액
*       LITAB-DMBTR_SUM = P_DMBTR1.
    ENDIF.
    MODIFY LITAB. CLEAR LITAB.
  ENDLOOP.
  CLEAR: P_DMBTR, P_DMBTR1..
ENDFORM.                               " GET_CENTER

*&--------------------------------------------------------------------
*&      Form  WRITE_DATA
*&--------------------------------------------------------------------
FORM WRITE_DATA.
  DATA: POS(3) TYPE C,
        MOD TYPE I,
        PAGENO TYPE I,
        ROWNO TYPE I,
        TXT(6).

  DATA: CHK_COLOR TYPE C.
*  ROWNO = SY-TABIX.
  LOOP AT HDTAB.

    WRITE: /1 '|', 2 HDTAB-MARK AS CHECKBOX.
    FORMAT COLOR COL_KEY INTENSIFIED OFF.
    WRITE: 3 '|', 4 HDTAB-BELNR.

    IF CHK_COLOR = 'X'.
      FORMAT COLOR COL_NORMAL INTENSIFIED ON.
      CHK_COLOR = ''.
    ELSE.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
      CHK_COLOR = 'X'.
    ENDIF.

    READ TABLE LITAB WITH KEY BELNR = HDTAB-BELNR.
    HDTAB-DMBTR_SUM = LITAB-DMBTR_SUM.
    HDTAB-SGTXT  = LITAB-SGTXT.
    HDTAB-HKONT  = LITAB-HKONT.
    HDTAB-TXT20  = LITAB-TXT20.
    IF HDTAB-BSTAT = '' OR HDTAB-BSTAT = 'A' OR HDTAB-BSTAT = 'B'.
      HDTAB-STAT_DOC = 'POST'.
    ELSEIF HDTAB-BSTAT = 'V' OR HDTAB-BSTAT = 'W'.
      HDTAB-STAT_DOC = 'PARK'.
    ELSEIF HDTAB-BSTAT = 'S'.
      HDTAB-STAT_DOC = 'NOTE'.

    ENDIF.

    MODIFY HDTAB.

    CLEAR LITAB.

    WRITE: 14 '|', 15 HDTAB-DEPARTMENT(15),
           30 '|', 31 HDTAB-BKTXT(13),
           44 '|', 45 HDTAB-STAT_DOC,
           49 '|', 50 HDTAB-BLDAT,
           60 '|', 61 HDTAB-HKONT+4(6),
           68 '|', 69 HDTAB-TXT20,
           90 '|', 91 HDTAB-SGTXT(40),
          132 '|', 133(16) HDTAB-DMBTR_SUM,
          150 '|', 151 HDTAB-BLART,
          153 '|', 154 HDTAB-PPNAM,
          166 '|', 167 HDTAB-USNAM,
          179 '|'.
*      PERFORM PUT_MINUS USING HDTAB-DMBTR1.
  ENDLOOP.
  WRITE: / SY-ULINE(179).
  FORMAT RESET.
ENDFORM.                               " WRITE_DATA
*&--------------------------------------------------------------------
*
*&      Form  HANDLING_SAPSCRIPT
*&--------------------------------------------------------------------
*
FORM HANDLING_SAPSCRIPT.
* SAPSCRIPT.
  ITCPO-TDIMMED = 'X'.
  ITCPO-TDDELETE = 'X'.

  IF STAB-PADEST NE ' '.        "프린터 정의가되면 정의된 프
    ITCPO-TDDEST = STAB-PADEST.
  ENDIF.

  REFRESH REQ.
  CLEAR   REQ.
  LOOP AT HDTAB WHERE MARK = 'X'.
    REQ-BELNR = HDTAB-BELNR.
    REQ-BUKRS = HDTAB-BUKRS.
    REQ-GJAHR = HDTAB-GJAHR.
    REQ-NAME  = STAB-NAME.
    REQ-PADEST = STAB-PADEST.
    APPEND REQ.
    CLEAR  REQ.
  ENDLOOP.
  DESCRIBE TABLE REQ LINES LINE.

  CHECK LINE NE 0.        "출력할 자료가 1건이상

  EXPORT REQ TO MEMORY ID 'ZRFIGLR23'.
  SUBMIT ZRFIGLR23     AND RETURN.
ENDFORM.                               " HANDLING_SAPSCRIPT
*&--------------------------------------------------------------------
*
*&      Form  PUT_MINUS
*&--------------------------------------------------------------------
*
FORM PUT_MINUS USING PU_AMOUNT.
  DATA: UNIT_LEN TYPE I,
        CHK_ZERO TYPE I,
        I_COMMA  TYPE I,
        D_LEN(1),
        U_LEN(1),
        I_LEN(1),
        I_LENT(1),
        AMOUNT(20) TYPE C.

  IF PU_AMOUNT < 0.

    WRITE AT 79(20) PU_AMOUNT DECIMALS 0 NO-SIGN.
    AMOUNT = PU_AMOUNT.
    SHIFT AMOUNT LEFT DELETING LEADING SPACE.
    SEARCH AMOUNT FOR '-'.
    I_LENT = SY-FDPOS - 3.
    U_LEN = CEIL( I_LENT / 3 ).
    D_LEN = I_LENT / 3.
    CHK_ZERO = U_LEN - D_LEN.
    I_COMMA = CEIL( I_LENT / 3 ).
    IF CHK_ZERO <> 0.
      I_COMMA = I_COMMA + 1.
    ENDIF.
    I_LEN = ( 20 - I_LENT ) + 79 - I_COMMA - 1.
    WRITE AT I_LEN '▲'.
  ELSE.
    WRITE AT 79(20) PU_AMOUNT DECIMALS 0 NO-SIGN.
  ENDIF.

ENDFORM.                               " PUT_MINUS
*&--------------------------------------------------------------------
*
*&      Form  TOP_PAGE
*&--------------------------------------------------------------------
*
FORM TOP_PAGE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING.
  WRITE: / SY-ULINE(179).
  WRITE: /1 '|', 3 '|', 4(10) 'Doc No' CENTERED,
         14 '|', 15(16) 'Depart.' CENTERED,
         30 '|', 31(13)  'Hd.Text'   CENTERED,
         44 '|', 45(6)  'Status'   CENTERED,
         52 '|', 53(10) 'Doc.Date' CENTERED,
         64 '|', 65(20) 'Account Text' CENTERED,
         86 '|', 87(40) 'Text' CENTERED,
        128 '|', 129(20) 'Amount' RIGHT-JUSTIFIED,
        150 '|', 151(2)  'DT',
        153 '|', 154(12)  'Parked by' CENTERED,
        166 '|', 167(12)  'Entered by' CENTERED,
        179 '|'.
  WRITE: / SY-ULINE(179).
ENDFORM.                               " TOP_PAGE
*&--------------------------------------------------------------------
*&      Form  UPDATE_MARK_FIELD
*&--------------------------------------------------------------------
FORM UPDATE_MARK_FIELD.
  DATA: MOD    TYPE I,                 "페이지 넘어갈 때마다
        ROWNO  TYPE I,                 "현재 페이지에서의 라인
        PAGENO TYPE I,                 "현재 페이지번호
        TABIX  TYPE I.                 "계산된 인터널테이블의
  CLEAR: ROWNO, PAGENO, TABIX.
  DO.
    ROWNO = ROWNO  + 1.
    CHECK ROWNO >= 4.                  "헤더는 건너뛴다.
    TABIX = ROWNO - 3.
    READ LINE ROWNO FIELD VALUE HDTAB-MARK.
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.

    IF HDTAB-MARK = 'X'.
      CLEAR HDTAB.
      READ TABLE HDTAB INDEX TABIX.
      HDTAB-MARK = 'X'.
    ELSE.
      CLEAR HDTAB.
      READ TABLE HDTAB INDEX TABIX.
      HDTAB-MARK = SPACE.
    ENDIF.
    MODIFY HDTAB INDEX TABIX.
    CLEAR HDTAB.
  ENDDO.

ENDFORM.                               " UPDATE_MARK_FIELD
*&--------------------------------------------------------------------
*
*&      Form  GET_FIELD_TITEL
*&--------------------------------------------------------------------
*
FORM GET_FIELD_TITEL.
  SHIFT FIELD_VALUE LEFT DELETING LEADING SPACE.

  IF FIELD_VALUE = '전표번호'.
    FIELD_NAME = 'HDTAB-BELNR'.
  ELSEIF FIELD_VALUE = '현재상태'.
    FIELD_NAME = 'HDTAB-STAT_DOC'.
  ELSEIF FIELD_VALUE = '전기일자'.
    FIELD_NAME = 'HDTAB-BUDAT'.
  ELSEIF FIELD_VALUE = '내  용'.
    FIELD_NAME = 'HDTAB-SGTXT'.
*  ELSEIF FIELD_VALUE = '금  액'.
*    FIELD_NAME = 'HDTAB-DMBTR1'.
  ELSEIF FIELD_VALUE = 'DT'.
    FIELD_NAME = 'HDTAB-BLART'.
  ELSEIF FIELD_VALUE = '임시저장인'.
    FIELD_NAME = 'HDTAB-PPNAM'.
  ELSEIF FIELD_VALUE = '입력인'.
    FIELD_NAME = 'HDTAB-USNAM'.
  ELSEIF FIELD_VALUE = '텍스트'.
    FIELD_NAME = 'HDTAB-BKTXT'.
  ENDIF.
ENDFORM.                               " GET_FIELD_TITEL
*&--------------------------------------------------------------------
*
*&      Form  GET_DETAIL
*&--------------------------------------------------------------------
*
FORM GET_DETAIL.
  CASE SY-LSIND.
    WHEN 1.
      DATA : LINE LIKE SY-INDEX,
             READ_LINE LIKE SY-INDEX.

      GET CURSOR LINE LINE.
      READ_LINE = LINE - 3.
      READ TABLE HDTAB INDEX READ_LINE.

      SET PARAMETER ID 'BLN' FIELD HDTAB-BELNR.
      SET PARAMETER ID 'BLP' FIELD HDTAB-BELNR.
      SET PARAMETER ID 'BUK' FIELD HDTAB-BUKRS.
      SET PARAMETER ID 'GJR' FIELD HDTAB-GJAHR.

      IF HDTAB-BSTAT    = 'A' OR HDTAB-BSTAT    = 'B' OR   "전기
         HDTAB-BSTAT    = ' ' OR HDTAB-BSTAT    = 'S'.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSE.                            "미전기
        CALL TRANSACTION 'FBV3' AND SKIP FIRST SCREEN.
      ENDIF.

*     CALL TRANSACTION 'FBV0' AND SKIP FIRST SCREEN.
  ENDCASE.

ENDFORM.                               " GET_DETAIL
*&-------------------------------------------------------------------*
*&      Form  SELECT_F4_DEPARTMT
*&--------------------------------------------------------------------
*
FORM SELECT_F4_DEPARTMT.
  DATA : BEGIN OF I_VALUES2 OCCURS 0,
           DEPARMENT LIKE  ADCP-DEPARTMENT,
         END OF I_VALUES2.

  CLEAR : SCR_FIELDS, SCR_FIELDS[].
  SCR_FIELDS-FIELDNAME = 'DEPARTMT'.
  APPEND SCR_FIELDS.

*  CALL FUNCTION 'DYNP_VALUES_READ'
*       EXPORTING
*            DYNAME     = SY-CPROG      "'ZRFIBTS11'
*            DYNUMB     = SY-DYNNR
*       TABLES
*            DYNPFIELDS = SCR_FIELDS.
*
*  READ TABLE SCR_FIELDS INDEX 1.
*  DEPARTMT = SCR_FIELDS-FIELDVALUE.

*  IF DEPARTMT IS INITIAL.
*    MESSAGE S000 WITH '부서를 입력하십시요'.
*    EXIT.
*  ENDIF.
*
*  CHECK NOT DEPARTMT IS INITIAL.

  CLEAR: I_SELECTFIELD, I_FIELDS, I_SELECT_VALUE, I_IND.
  REFRESH: I_FIELDS, I_VALUES2.

  SELECT DISTINCT DEPARTMENT INTO I_VALUES2-DEPARMENT
                     FROM ADCP.
    APPEND I_VALUES2.
  ENDSELECT.

  IF SY-DBCNT EQ 0.
    MESSAGE S000 WITH 'Not exist'.
    EXIT.
  ENDIF.

  I_FIELDS-TABNAME = 'ADCP'.
  I_FIELDS-FIELDNAME = 'DEPARTMENT'.
  I_FIELDS-SELECTFLAG = 'X'.
  APPEND I_FIELDS.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            SELECTFIELD                  = I_SELECTFIELD
       IMPORTING
            IND                          = I_IND
            SELECT_VALUE                 = I_SELECT_VALUE
       TABLES
            FIELDS                       = I_FIELDS
            FULL_TABLE                   = I_VALUES2
       EXCEPTIONS
            FULL_TABLE_EMPTY             = 1
            NO_TABLESTRUCTURE_GIVEN      = 2
            NO_TABLEFIELDS_IN_DICTIONARY = 3
            MORE_THEN_ONE_SELECTFIELD    = 4
            NO_SELECTFIELD               = 5
            OTHERS                       = 6.

  DEPARTMT = I_SELECT_VALUE.

ENDFORM.                               " SELECT_F4_DEPARTMT
*&--------------------------------------------------------------------
*
*&      Form  POSTING_DOC
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*---------------------------------------------------------------------
*
FORM POSTING_DOC.
  LOOP AT HDTAB WHERE MARK = 'X'.
    SELECT SINGLE * INTO T_VBKPF FROM VBKPF
      WHERE AUSBK = HDTAB-AUSBK
        AND BUKRS = HDTAB-BUKRS
        AND BELNR = HDTAB-BELNR
        AND GJAHR = HDTAB-GJAHR.
    APPEND T_VBKPF.
  ENDLOOP.

  CALL FUNCTION 'PRELIMINARY_POSTING_POST_ALL'
       EXPORTING
            SYNCH   = 'X'
*            BUPBI   = 'X'
       TABLES
            T_VBKPF = T_VBKPF.
ENDFORM.                               " POSTING_DOC
*&--------------------------------------------------------------------
*
*&      Module  INIT_2000  OUTPUT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
MODULE INIT_2000 OUTPUT.
  SELECT SINGLE * FROM USR01 WHERE BNAME = SY-UNAME.
*
  IF SY-SUBRC = 0.
    IF STAB-NAME = ' '.
      MOVE USR01-SPLD TO STAB-PADEST.
      SELECT SINGLE NAME INTO STAB-NAME
             FROM   TSP03D
             WHERE  PADEST = USR01-SPLD.
    ENDIF.
  ENDIF.
ENDMODULE.                             " INIT_2000  OUTPUT
*&--------------------------------------------------------------------
*
*&      Module  STATUS_2000  OUTPUT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
MODULE STATUS_2000 OUTPUT.
  SET PF-STATUS 'PF200'.
  SET TITLEBAR '200'.
ENDMODULE.                             " STATUS_2000  OUTPUT
*&--------------------------------------------------------------------
*
*&      Module  USER_COMMAND_2000  INPUT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
MODULE USER_COMMAND_2000 INPUT.
  CASE OK_CODE.
    WHEN 'PRE'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                             " USER_COMMAND_2000  INPUT
*&--------------------------------------------------------------------
*
*&      Module  GET_PRINTER  INPUT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
MODULE GET_PRINTER INPUT.
  TABLES: TSP03D.
  DATA: BEGIN OF FULL_TABLE OCCURS 0,
           NAME   LIKE TSP03D-NAME,
           PADEST LIKE TSP03D-PADEST,
        END OF FULL_TABLE.
  DATA: SELECTFIELD LIKE HELP_INFO-FIELDNAME.
  DATA: TITEL(40) TYPE C, CD(2), INDEX.
  DATA: IND LIKE SY-TABIX.
  DATA: BEGIN OF FIELDS OCCURS 10.
          INCLUDE STRUCTURE HELP_VALUE.
  DATA: END OF FIELDS.
  DATA: BEGIN OF NAMELIST OCCURS 0,
           FELDNAME(21),
        END OF NAMELIST.

  REFRESH: FULL_TABLE, FIELDS, NAMELIST.
  CLEAR:   FULL_TABLE, FIELDS, NAMELIST.

  SELECT NAME PADEST INTO (FULL_TABLE-NAME, FULL_TABLE-PADEST)
              FROM TSP03D
              WHERE NAME NE ' '.
    APPEND FULL_TABLE.
    CLEAR  FULL_TABLE.
  ENDSELECT.
  SELECTFIELD = '출력장치'.
  REFRESH NAMELIST.
  NAMELIST-FELDNAME = 'TSP03D-NAME'.
  APPEND NAMELIST.
  SELECTFIELD = '단축이름'.
  NAMELIST-FELDNAME = 'TSP03D-PADEST'.
  APPEND NAMELIST.
  REFRESH FIELDS.

  CALL FUNCTION 'TRANSFER_NAMES_TO_FIELDS'
       EXPORTING
            SELECTFIELD        = SELECTFIELD
       TABLES
            FIELDS             = FIELDS
            NAMELIST           = NAMELIST
       EXCEPTIONS
            WRONG_FORMAT_GIVEN = 01.

  TITEL = 'PRINTER 선택'.

  CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
       EXPORTING
            DISPLAY                      = ' '
            SELECTFIELD                  = SELECTFIELD
            TITEL                        = TITEL
       IMPORTING
            IND                          = IND
       TABLES
            FIELDS                       = FIELDS
            FULL_TABLE                   = FULL_TABLE
       EXCEPTIONS
            FULL_TABLE_EMPTY
            NO_TABLESTRUCTURE_GIVEN
            NO_TABLEFIELDS_IN_DICTIONARY
            MORE_THEN_ONE_SELECTFIELD
            NO_SELECTFIELD.

  IF SY-SUBRC EQ 0.
    READ TABLE FULL_TABLE INDEX IND.
    STAB-NAME =  FULL_TABLE-NAME.
    STAB-PADEST = FULL_TABLE-PADEST.
  ENDIF.
ENDMODULE.                             " GET_PRINTER  INPUT
*&--------------------------------------------------------------------
*
*&      Module  GET_PRINTER_NAME  INPUT
*&--------------------------------------------------------------------
*
*       text
*---------------------------------------------------------------------
*
MODULE GET_PRINTER_NAME INPUT.
  SELECT SINGLE    PADEST INTO STAB-PADEST
              FROM TSP03D
             WHERE NAME = STAB-NAME.
  IF SY-SUBRC NE 0.
    MESSAGE E000 WITH 'No printer'.
  ENDIF.
ENDMODULE.                 " GET_PRINTER_NAME  INPUT
