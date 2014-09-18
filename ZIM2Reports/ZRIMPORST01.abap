*&---------------------------------------------------------------------*
*& Report  ZRIMPORST01                                                 *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  프로그램명 :[Report] 구매실적 현황표(통화별)                       *
*&      작성자 : 이승준                                                *
*&      작성일 : 2001.09.22                                            *
*&---------------------------------------------------------------------*
*&  DESC.      :  일정기간 구매실적을 통화별 조회,각 통화별 구매실적이
*&                차지하는 비율을 LIST로 보여줌.
*&---------------------------------------------------------------------*
*& [변경내용]
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*


REPORT  ZRIMPORST01  MESSAGE-ID ZIM
                     LINE-SIZE 150
                     NO STANDARD PAGE HEADING.

TABLES : EKBE,                     "구매문서별 이력
         EKKO,                     "구매문서헤더
         ZTREQHD,                  "수입의뢰 해더.
         ZTREQIT,                  "수입의뢰 ITEM.
         T001W.                    "사업부명.

*>>> 변수 DEFINE.
DATA   : SI_LINE TYPE I VALUE 150,
         W_ERR_CHK TYPE C VALUE 'N',   "ERROR CHECK.
         W_LINE  TYPE I,               "IT_TAB LINE 수.
         W_SUM   LIKE ZTREQHD-ZFLASTAM, "원화 소계용.
         W_TOTAL LIKE ZTREQHD-ZFLASTAM, "원화 총계용.
         W_WERKS LIKE EKPO-WERKS,     "사업장.
         W_MOD LIKE SY-TABIX,        "홀짝.
         W_TABIX LIKE SY-TABIX.

DATA : BEGIN OF IT_TAB OCCURS 0,

      EBELN   LIKE EKBE-EBELN,    "10구매문서번호
      EBELP   LIKE EKBE-EBELP,     "NUMC5구매문서 품목번호
      BUDAT   LIKE EKBE-BUDAT,     "DATS8전표전기일(기간)
      DMBTR   LIKE EKBE-DMBTR,     "CURR13.2현지통화금액(원화)
      WRBTR   LIKE EKBE-WRBTR,     "CURR13.2전표통화금액(외화)
      WAERS   LIKE EKBE-WAERS,     "CUKY5통화키
      WERKS   LIKE EKBE-WERKS,     "4플랜트(사업장)
      BUKRS   LIKE EKKO-BUKRS,     "4회사코드
      BSTYP   LIKE EKKO-BSTYP,     "1구매문서범주
      BSART   LIKE EKKO-BSART,     "4구매문서유형
      ZFREQNO LIKE ZTREQIT-ZFREQNO,"10수입의뢰 관리번호
      ZFITMNO LIKE ZTREQIT-ZFITMNO,"NUMC5수입문서 품목번호

      ZFREQTY LIKE ZTREQHD-ZFREQTY."2수입의뢰 Type(결제구분)

DATA : END OF IT_TAB.

DATA : BEGIN OF IT_TEMP1 OCCURS 0,

      EBELN   LIKE EKBE-EBELN,     "10구매문서번호
      EBELP   LIKE EKBE-EBELP,     "NUMC5구매문서 품목번호
      BUDAT   LIKE EKBE-BUDAT,     "DATS8전표전기일(기간)
      DMBTR   LIKE EKBE-DMBTR,     "CURR13.2현지통화금액(원화)
      WRBTR   LIKE EKBE-WRBTR,     "CURR13.2전표통화금액(외화)
      WAERS   LIKE EKBE-WAERS,     "CUKY5통화키
      WERKS   LIKE EKBE-WERKS,     "4플랜트(사업장)
      BUKRS   LIKE EKKO-BUKRS,     "4회사코드
      BSTYP   LIKE EKKO-BSTYP,     "1구매문서범주
      BSART   LIKE EKKO-BSART.     "4구매문서유형
*      ZFREQNO LIKE ZTREQIT-ZFREQNO,"10수입의뢰 관리번호
*      ZFITMNO LIKE ZTREQIT-ZFITMNO,"NUMC5수입문서 품목번호
*      ZFREQNO LIKE ZTREQHD-ZFREQNO,"10수입의뢰 관리번호
*      ZFREQTY LIKE ZTREQHD-ZEREQTY,"2수입의뢰 Type(결제구분)

DATA : END OF IT_TEMP1.

DATA : BEGIN OF IT_TEMP2 OCCURS 0,

      EBELN   LIKE EKBE-EBELN,    "10구매문서번호
      EBELP   LIKE EKBE-EBELP,     "NUMC5구매문서 품목번호
      BUDAT   LIKE EKBE-BUDAT,     "DATS8전표전기일(기간)
      DMBTR   LIKE EKBE-DMBTR,     "CURR13.2현지통화금액(원화)
      WRBTR   LIKE EKBE-WRBTR,     "CURR13.2전표통화금액(외화)
      WAERS   LIKE EKBE-WAERS,     "CUKY5통화키
      WERKS   LIKE EKBE-WERKS,     "4플랜트(사업장)
      BUKRS   LIKE EKKO-BUKRS,     "4회사코드
      BSTYP   LIKE EKKO-BSTYP,     "1구매문서범주
      BSART   LIKE EKKO-BSART,     "4구매문서유형
      ZFREQNO LIKE ZTREQIT-ZFREQNO,"10수입의뢰 관리번호
      ZFITMNO LIKE ZTREQIT-ZFITMNO."NUMC5수입문서 품목번호
*      ZFREQNO LIKE ZTREQHD-ZFREQNO,"10수입의뢰 관리번호
*      ZFREQTY LIKE ZTREQHD-ZEREQTY,"2수입의뢰 Type(결제구분)

DATA : END OF IT_TEMP2.

DATA : BEGIN OF IT_COL OCCURS 0,

      DMBTR   LIKE EKBE-DMBTR,     "CURR13.2현지통화금액(원화)
      WRBTR   LIKE EKBE-WRBTR,     "CURR13.2전표통화금액(외화)
      WAERS   LIKE EKBE-WAERS,     "CUKY5통화키
      WERKS   LIKE EKBE-WERKS,     "4플랜트(사업장)
      BUKRS   LIKE EKKO-BUKRS.     "4회사코드

DATA : END OF IT_COL.

DATA : BEGIN OF IT_MAIN OCCURS 0,
**>>"4플랜트(사업장)-->COL에서 미리 가져옴
      WERKS   LIKE EKBE-WERKS,
      USD TYPE P DECIMALS 2,
      JPY TYPE P DECIMALS 2,
      DEM TYPE P DECIMALS 2,
      CHF TYPE P DECIMALS 2,
      CFP TYPE P DECIMALS 2,
      GBP TYPE P DECIMALS 2,
*>>OTHER:불필요.단 이가격의 원화값들의 합을 W_OTHER에 입력요.
      W_USD TYPE P DECIMALS 2,
      W_JPY TYPE P DECIMALS 2,
      W_DEM TYPE P DECIMALS 2,
      W_CHF TYPE P DECIMALS 2,
      W_CFP TYPE P DECIMALS 2,
      W_GBP TYPE P DECIMALS 2,
      W_OTHER TYPE P DECIMALS 2,
*>>W_TOTAL:W_로 되어있는 값들의 합.
      W_TOTAL TYPE P DECIMALS 2.
*>> %은 WRITE시에 W_### / W_TOTAL * 100으로 출력.
DATA : END OF IT_MAIN.

*----------------------------------------------------------------------*
*          SELECTION-SCREEN                                            *
*----------------------------------------------------------------------*

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
      S_BUKRS   FOR  EKKO-BUKRS NO-EXTENSION NO INTERVALS OBLIGATORY,
                                     "4회사코드
      S_BUDAT   FOR  EKBE-BUDAT,     "DATS8전표전기일(기간)
      S_BSTYP   FOR  EKKO-BSTYP,     "1구매문서범주
      S_BSART   FOR  EKKO-BSART.     "4구매문서유형

SELECTION-SCREEN END OF BLOCK B1.

*----------------------------------------------------------------------*
*          INITIALIZATION.                                             *
*----------------------------------------------------------------------*

INITIALIZATION.                          " 초기값 SETTING
  SET TITLEBAR 'ZIMR72'.
  CLEAR : IT_TEMP1, IT_TEMP2, IT_TAB, IT_COL, IT_MAIN.
  REFRESH : IT_TEMP1, IT_TEMP2, IT_TAB, IT_COL, IT_MAIN.

*----------------------------------------------------------------------*
*          START-OF-SELECTION.                                         *
*----------------------------------------------------------------------*
START-OF-SELECTION.

***> READ TABLE
**>JOIN으로 EKBE와 EKKO로 MAKE.
  PERFORM   P1000_READ_TEMP1.
**>ZTREQIT에서 ZFREQNO추가.
  PERFORM   P1000_READ_TEMP2.
**>ZTREQHD에서 수입의뢰 TYPE LO,PU SKIP.
  PERFORM   P1000_READ_TAB.
**>WERKS,WAERS기준으로 COLLECT.
  PERFORM   P1000_READ_COL.
**>출력 형식으로 TABLE 형성.-->형성시 MAIN에 WERKS를 먼저 넣은후
**                             MAIN TAB을 LOOP 하면서 COL TAB을
**                             WERKS KEY로 비교하면서 READ TABLE.
**                             CASE 등으로 MOVE MAIN.
  PERFORM   P1000_READ_MAIN.

*>>> SORT-GROUP:WERKS EBERN TXZ01 MENGE.
  SORT IT_TAB BY WERKS.

*>>> WRITE LIST
  PERFORM   P3000_WRITE_IT.

*----------------------------------------------------------------------*
*           TOP-OF-PAGE                                                *
*----------------------------------------------------------------------*
TOP-OF-PAGE.
  FORMAT RESET.
  WRITE : /50 '[  구매실적 현황표(통화별)  ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE :/75 '기간 : '.
  IF NOT S_BUDAT IS INITIAL.
    WRITE : S_BUDAT-LOW ,' ~ ', S_BUDAT-HIGH.
  ENDIF.
  SKIP.
  FORMAT COLOR 1 INTENSIFIED ON.
    WRITE :/ SY-ULINE(SI_LINE).
    WRITE :/
             SY-VLINE NO-GAP, (10) '사업부'  NO-GAP,
             SY-VLINE NO-GAP, (10) 'USD'  NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) 'JPY'  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) 'DEM'  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) 'CHF'  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) 'CFP'  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) 'GBP'  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) '기타' NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (17) ''     NO-GAP,
             AT SI_LINE SY-VLINE NO-GAP.
    FORMAT COLOR 1 INTENSIFIED OFF.
    WRITE :/
             SY-VLINE NO-GAP, (10) ''      NO-GAP,
             SY-VLINE NO-GAP, (10) '원화'  NO-GAP,
             SY-VLINE NO-GAP, (6)  '%'    NO-GAP,
             SY-VLINE NO-GAP, (10) '원화'  NO-GAP,
             SY-VLINE NO-GAP, (5)  '%'    NO-GAP,
             SY-VLINE NO-GAP, (10) '원화'  NO-GAP,
             SY-VLINE NO-GAP, (5)  '%'    NO-GAP,
             SY-VLINE NO-GAP, (10) '원화'  NO-GAP,
             SY-VLINE NO-GAP, (5)  '%'    NO-GAP,
             SY-VLINE NO-GAP, (10) '원화'  NO-GAP,
             SY-VLINE NO-GAP, (5)  '%'    NO-GAP,
             SY-VLINE NO-GAP, (10) '원화'  NO-GAP,
             SY-VLINE NO-GAP, (5)  '%'    NO-GAP,
             SY-VLINE NO-GAP, (10) '원화'  NO-GAP,
             SY-VLINE NO-GAP, (6)  '%'    NO-GAP,
             SY-VLINE NO-GAP, (17) '수입계'  NO-GAP,
             AT SI_LINE SY-VLINE NO-GAP.
    WRITE :/ SY-ULINE(SI_LINE).


*----------------------------------------------------------------------*
*           AT USER-COMMAND                                            *
*----------------------------------------------------------------------
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEMP1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TEMP1.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TEMP1
           FROM EKKO AS O JOIN EKBE AS B
           ON O~EBELN EQ B~EBELN
           WHERE      B~BUDAT IN S_BUDAT
             AND      O~BUKRS IN S_BUKRS
             AND      O~BSTYP IN S_BSTYP
             AND      O~BSART IN S_BSART.
  IF SY-SUBRC NE 0.  MESSAGE S738. EXIT. ENDIF.

ENDFORM.                    " P1000_READ_TEMP1
*
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TEMP2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TEMP2.

  LOOP AT IT_TEMP1.
     SELECT * FROM ZTREQIT
         INTO CORRESPONDING FIELDS OF IT_TEMP2
*         FOR ALL ENTRIES IN IT_TEMP1
         WHERE EBELN EQ IT_TEMP1-EBELN
           AND EBELP EQ IT_TEMP1-EBELP.
     MOVE-CORRESPONDING IT_TEMP1 TO IT_TEMP2.
     APPEND IT_TEMP2.
     ENDSELECT.
   ENDLOOP.
  IF SY-SUBRC NE 0.  MESSAGE S738. EXIT. ENDIF.

ENDFORM.                    " P1000_READ_TEMP2
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_TAB.

  LOOP AT IT_TEMP2.
     SELECT * FROM ZTREQHD
         INTO CORRESPONDING FIELDS OF IT_TAB
*         FOR ALL ENTRIES IN IT_TEMP2
         WHERE ZFREQNO EQ IT_TEMP2-ZFREQNO
           AND ZFREQTY NE 'PU'
           AND ZFREQTY NE 'LO'.
      MOVE-CORRESPONDING IT_TEMP2 TO IT_TAB.
      APPEND IT_TAB.
      ENDSELECT.
  ENDLOOP.
  IF SY-SUBRC NE 0.  MESSAGE S738. EXIT. ENDIF.

ENDFORM.                    " P1000_READ_TAB
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_COL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_COL.

  LOOP AT IT_TAB.
    MOVE-CORRESPONDING IT_TAB TO IT_COL.
    COLLECT IT_COL.
  ENDLOOP.

ENDFORM.                    " P1000_READ_COL
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_MAIN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_MAIN.
  SORT IT_COL BY WERKS.
*>>  WERKS가 다를 때만 COL에서 뽑아서 MAIN에 넣는다.
  LOOP AT IT_COL.

      ON CHANGE OF IT_COL-WERKS.
      MOVE IT_COL-WERKS TO IT_MAIN-WERKS.
      APPEND IT_MAIN.
      ENDON.

  ENDLOOP.
*>>  COL의 나열 형태를 PRINT할 수 있도록 MAIN의 형태로 전환한다.
  LOOP AT IT_MAIN.
    W_TABIX = SY-TABIX.
    READ TABLE IT_COL WITH KEY WERKS = IT_MAIN-WERKS.

    CASE IT_COL-WAERS.
       WHEN 'USD'.
         MOVE IT_COL-DMBTR TO IT_MAIN-W_USD.
         MOVE IT_COL-WRBTR TO IT_MAIN-USD.

       WHEN 'JPY'.
         MOVE IT_COL-DMBTR TO IT_MAIN-W_JPY.
         MOVE IT_COL-WRBTR TO IT_MAIN-JPY.

       WHEN 'DEM'.
         MOVE IT_COL-DMBTR TO IT_MAIN-W_DEM.
         MOVE IT_COL-WRBTR TO IT_MAIN-DEM.

       WHEN 'CHF'.
         MOVE IT_COL-DMBTR TO IT_MAIN-W_CHF.
         MOVE IT_COL-WRBTR TO IT_MAIN-CHF.

       WHEN 'CFP'.
         MOVE IT_COL-DMBTR TO IT_MAIN-W_CFP.
         MOVE IT_COL-WRBTR TO IT_MAIN-CFP.

       WHEN 'GBP'.
         MOVE IT_COL-DMBTR TO IT_MAIN-W_GBP.
         MOVE IT_COL-WRBTR TO IT_MAIN-GBP.

       WHEN OTHERS.
         MOVE IT_COL-DMBTR TO IT_MAIN-W_OTHER.

    ENDCASE.
    IT_MAIN-W_TOTAL = IT_MAIN-W_USD + IT_MAIN-W_JPY + IT_MAIN-W_DEM +
                IT_MAIN-W_CHF + IT_MAIN-W_CFP + IT_MAIN-W_GBP +
                                               IT_MAIN-W_OTHER .
    MODIFY IT_MAIN INDEX W_TABIX.
  ENDLOOP.


ENDFORM.                    " P1000_READ_MAIN
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_IT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_IT.
  DATA : L_USD LIKE IT_MAIN-USD,
         L_JPY LIKE IT_MAIN-JPY,
         L_DEM LIKE IT_MAIN-DEM,
         L_CHF LIKE IT_MAIN-CHF,
         L_CFP LIKE IT_MAIN-CFP,
         L_GBP LIKE IT_MAIN-GBP,
         L_OTHER LIKE IT_MAIN-W_OTHER.
  SET TITLEBAR 'ZIMR72'.
*  SET PF-STATUS 'ZIMR72'.
  LOOP AT IT_MAIN.
    W_TABIX = SY-TABIX.
    FORMAT RESET.
**>사업부명 끌고오기
    PERFORM P1000_GET_NAME.

    FORMAT COLOR 2 INTENSIFIED ON.
*    WRITE :/ SY-ULINE(SI_LINE).
    WRITE :/
             SY-VLINE NO-GAP, (10) IT_MAIN-WERKS  NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-USD  NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-JPY NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-DEM  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-CHF  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-CFP  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-GBP  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) ''  NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (17) ''  NO-GAP,
             AT SI_LINE SY-VLINE NO-GAP.
    FORMAT COLOR 2 INTENSIFIED OFF.
*>> %
    L_USD = IT_MAIN-W_USD / IT_MAIN-W_TOTAL * 100.
    L_JPY = IT_MAIN-W_JPY / IT_MAIN-W_TOTAL * 100.
    L_DEM = IT_MAIN-W_DEM / IT_MAIN-W_TOTAL * 100.
    L_CHF = IT_MAIN-W_CHF / IT_MAIN-W_TOTAL * 100.
    L_CFP = IT_MAIN-W_CFP / IT_MAIN-W_TOTAL * 100.
    L_GBP = IT_MAIN-W_GBP / IT_MAIN-W_TOTAL * 100.
    L_OTHER = IT_MAIN-W_OTHER / IT_MAIN-W_TOTAL * 100.
    WRITE :/
             SY-VLINE NO-GAP, (10) T001W-NAME1    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_USD  NO-GAP,
             SY-VLINE NO-GAP, (6)  L_USD            NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_JPY  NO-GAP,
             SY-VLINE NO-GAP, (5)  L_JPY   NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_DEM  NO-GAP,
             SY-VLINE NO-GAP, (5)  L_DEM    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_CHF  NO-GAP,
             SY-VLINE NO-GAP, (5)  L_CHF     NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_CFP  NO-GAP,
             SY-VLINE NO-GAP, (5)  L_CFP    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_GBP  NO-GAP,
             SY-VLINE NO-GAP, (5)  L_GBP    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_OTHER  NO-GAP,
             SY-VLINE NO-GAP, (6)  L_OTHER    NO-GAP,
             SY-VLINE NO-GAP, (17) IT_MAIN-W_TOTAL  NO-GAP,
             AT SI_LINE SY-VLINE NO-GAP.
    WRITE :/ SY-ULINE(SI_LINE).
    AT LAST.
      SUM.

      FORMAT RESET.
      FORMAT COLOR 3 INTENSIFIED ON.
*      WRITE :/ SY-ULINE(SI_LINE).
      WRITE :/
             SY-VLINE NO-GAP, (10) '외화총계'  NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-USD  NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-JPY NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-DEM  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-CHF  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-CFP  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-GBP  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) ''  NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (17) ''  NO-GAP,
             AT SI_LINE SY-VLINE NO-GAP.
      FORMAT COLOR 3 INTENSIFIED OFF.
      WRITE :/
             SY-VLINE NO-GAP, (10) '원화총계'  NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_USD  NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_JPY NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_DEM  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_CHF  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_CFP  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_GBP  NO-GAP,
             SY-VLINE NO-GAP, (5)  ''    NO-GAP,
             SY-VLINE NO-GAP, (10) IT_MAIN-W_OTHER  NO-GAP,
             SY-VLINE NO-GAP, (6)  ''    NO-GAP,
             SY-VLINE NO-GAP, (17) IT_MAIN-W_TOTAL  NO-GAP,
             AT SI_LINE SY-VLINE NO-GAP.
      WRITE :/ SY-ULINE(SI_LINE).
      FORMAT RESET.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " P3000_WRITE_IT
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_GET_NAME.

SELECT SINGLE * FROM T001W
      WHERE WERKS = IT_MAIN-WERKS.

ENDFORM.                    " P1000_GET_NAME
