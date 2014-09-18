*&---------------------------------------------------------------------
*& Report  ZRIMBSEGDOC
*&---------------------------------------------------------------------
*&  프로그램명 : 수입원자재 회계문서 일계표.
*&      작성자 : 이채경 INFOLINK Ltd.
*&      작성일 : 2001.09.27
*&---------------------------------------------------------------------
*&   DESC.     :
*&---------------------------------------------------------------------
*& [변경내용]
*&---------------------------------------------------------------------
REPORT  ZRIMBSEGLST  MESSAGE-ID ZIM
                     LINE-SIZE 118
                     NO STANDARD PAGE HEADING.
TABLES: ZTBKPF,ZTBSEG,ZTBL,ZTIDS,ZTIMIMG10,LFA1,ZTIV,ZTCUCLIV,
        T001,ZTIMIMG02,ZTIMIMG08, ZTIMIMG00.
*----------------------------------------------------------------------
*  리스트용 INTERNAL TABLE
*----------------------------------------------------------------------

DATA : BEGIN OF IT_TAB OCCURS 0,
       BUKRS     LIKE    ZTBKPF-BUKRS,
       BELNR     LIKE    ZTBKPF-BELNR,
       GJAHR     LIKE    ZTBKPF-GJAHR,
       WAERS     LIKE    ZTBKPF-WAERS,
       ZFIMDNO   LIKE    ZTBSEG-ZFIMDNO,    " 수입문서번호.
       ZFACDO    LIKE    ZTBKPF-ZFACDO,      " 회계전표번호.
       LIFNR     LIKE    ZTBKPF-LIFNR,       " 구매처.
       ZFRVSX    LIKE    ZTBKPF-ZFRVSX,      " 역기표.
       NAME1     LIKE    LFA1-NAME1,         " 거래선.
       ZFCSTGRP  LIKE    ZTBSEG-ZFCSTGRP,    " 비용그룹.
       CSTGRP    LIKE    DD07T-DDTEXT,
       ZFCLNO    LIKE    ZTBKPF-ZFCLNO,      " 업무가불전표.
       CLNO(10)  TYPE    C,
       WRBTR     LIKE    ZTBSEG-WRBTR,       " 금액.
       MWSKZ     LIKE    ZTBSEG-MWSKZ,       " 세금코드.
       ZFCD      LIKE    ZTBSEG-ZFCD,        "
       NEWBS     LIKE    ZTBSEG-NEWBS,
       SHKZG     LIKE    ZTBSEG-SHKZG,
       ZFCDNM    LIKE    ZTIMIMG08-ZFCDNM,
       BUPLA     LIKE    ZTBKPF-BUPLA,       " 귀속사업장.
       CPUDT     LIKE    ZTBKPF-CPUDT,       " 전표입력일.
       CPUTM     LIKE    ZTBKPF-CPUTM,       " 전표입력시간.
       UTME      LIKE    ZTBKPF-UTME,        " 최종변경시간.
       ZFCNAME   LIKE    ZTBKPF-ZFCNAME,     " 담당자.
       ZFREBELN  LIKE    ZTBL-ZFREBELN,      " P/O NO
       ZFHBLNO   LIKE    ZTBL-ZFHBLNO,       " B/L NO
       ZFNEWT    LIKE    ZTBL-ZFNEWT,        " 총중량.
       ZFNEWTM   LIKE    ZTBL-ZFNEWTM,        " 총중량.
       ZFTOVL    LIKE    ZTBL-ZFTOVL,        " 총용적.
       ZFTOVLM   LIKE    ZTBL-ZFTOVLM,        " 총용적.
       ZFSPRTC   LIKE    ZTBL-ZFSPRTC,       " 선적항.
       ZFCARC    LIKE    ZTBL-ZFCARC,        " 선적국.
       ZFSHTY    LIKE    ZTBL-ZFSHTY,        " 화물형태.
       INCO1     LIKE    ZTBL-INCO1,         " 가격조건.
       ZF20FT    LIKE    ZTBL-ZF20FT,                       " 20FEET
       ZF40FT    LIKE    ZTBL-ZF40FT.                       " 40FEET
DATA : END OF IT_TAB.

DATA : W_STCD2   LIKE    LFA1-STCD2,         " 사업자등록번호.
       W_ZFACTNO LIKE    ZTIMIMG02-ZFACTNO.  " 계좌번호.

DATA :  W_ERR_CHK     TYPE C,
        W_LCOUNT      TYPE I,
        W_FIELD_NM    TYPE C,
        W_PAGE        TYPE I,
        W_CHECK_PAGE(1) TYPE C,
        W_LINE        TYPE I,
        W_COUNT       TYPE I,
        W_TABIX       LIKE SY-TABIX,
        P_BUKRS       LIKE ZTBL-BUKRS.
DATA: CURSORFIELD(20).

*----------------------------------------------------------------------
* Selection Screen ?
*----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.


SELECT-OPTIONS: S_BUKRS   FOR  ZTBKPF-BUKRS  NO-EXTENSION NO
                                             INTERVALS,
                S_DAT     FOR  ZTBKPF-CPUDT ,
                S_UNAM    FOR  ZTBKPF-ZFCNAME. "OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_INIT.
  PERFORM   P1000_SET_BUKRS.

*title Text Write
  W_CHECK_PAGE = 'X'.

TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*----------------------------------------------------------------------
* START OF SELECTION ?
*----------------------------------------------------------------------
START-OF-SELECTION.
* B/L 테이블 SELECT
  PERFORM   P1000_READ_DATA    USING  W_ERR_CHK.
  IF W_ERR_CHK = 'Y'.
    MESSAGE S738.  EXIT.
  ENDIF.
* 레포트 Write
  PERFORM  P3000_DATA_WRITE.
  IF W_ERR_CHK EQ 'Y'.    EXIT.    ENDIF.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'DISP'.
      IF W_TABIX IS INITIAL.
        MESSAGE S962.
      ELSE.
        IF NOT IT_TAB-BELNR IS INITIAL.
          PERFORM P2000_DISPLAY_COST_DOCUMENT USING  IT_TAB-BUKRS
                                                     IT_TAB-GJAHR
                                                     IT_TAB-BELNR.
        ELSE.
          MESSAGE S962.
        ENDIF.

      ENDIF.
  ENDCASE.

  CLEAR : W_TABIX, IT_TAB.

*---------------------------------------------------------------------
* AT LINE-SELECTION.
*----------------------------------------------------------------------
AT LINE-SELECTION.
  IF W_TABIX IS INITIAL.
    MESSAGE S962.
  ELSE.
    IF NOT IT_TAB-BELNR IS INITIAL.
      PERFORM P2000_DISPLAY_COST_DOCUMENT USING  IT_TAB-BUKRS
                                                 IT_TAB-GJAHR
                                                 IT_TAB-BELNR.
    ELSE.
      MESSAGE S962.
    ENDIF.

  ENDIF.

  CLEAR : W_TABIX, IT_TAB.

*&---------------------------------------------------------------------
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------
FORM P3000_TITLE_WRITE.

  SKIP 2.
  WRITE:/45  'Daily Balance(Account Document)'.
  SKIP 2.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------
*&      Form  P1000_GET_IT_TAB
*&---------------------------------------------------------------------
FORM P1000_READ_DATA USING W_ERR_CHK.

  W_ERR_CHK = 'N'.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_TAB
            FROM ZTBKPF AS R INNER JOIN ZTBSEG AS I
             ON R~BUKRS = I~BUKRS
            AND R~BELNR = I~BELNR
            AND R~GJAHR = I~GJAHR
         WHERE  ( R~ZFPOSYN EQ 'Y'              " 전기여부.
           OR     R~ZFPOSYN EQ 'P' )
           AND  R~UDAT     IN  S_DAT           " 기준일.
           AND  R~ZFCNAME  IN  S_UNAM
           AND  R~ZFCSTGRP NE  '003'.
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

  SORT IT_TAB BY ZFACDO ZFCNAME CPUDT.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    IF IT_TAB-ZFCSTGRP EQ '006'.
      CLEAR ZTIV.
      SELECT  SINGLE *
          FROM  ZTIV
          WHERE ZFIVNO = IT_TAB-ZFIMDNO.
      IT_TAB-ZFIMDNO = ZTIV-ZFBLNO.
    ENDIF.
*---------------B/L DATA MOVE--------------------------------------
    CLEAR ZTBL.
    SELECT SINGLE *
        FROM ZTBL
       WHERE ZFBLNO  = IT_TAB-ZFIMDNO.
    IF SY-SUBRC NE 0.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ENDIF.
    MOVE:   ZTBL-ZFREBELN  TO  IT_TAB-ZFREBELN, " P/O NO
            ZTBL-ZFHBLNO   TO  IT_TAB-ZFHBLNO,  " B/L NO
            ZTBL-ZFNEWT    TO  IT_TAB-ZFNEWT,   " 총중량.
            ZTBL-ZFNEWTM   TO  IT_TAB-ZFNEWTM,  " 총중량.
            ZTBL-ZFTOVL    TO  IT_TAB-ZFTOVL,   " 총용적.
            ZTBL-ZFTOVLM   TO  IT_TAB-ZFTOVLM,  " 총용적.
            ZTBL-ZFSPRTC   TO  IT_TAB-ZFSPRTC,  " 선적항.
            ZTBL-ZFCARC    TO  IT_TAB-ZFCARC,   " 선적국.
            ZTBL-ZFSHTY    TO  IT_TAB-ZFSHTY,   " 화물형태.
            ZTBL-INCO1     TO  IT_TAB-INCO1,    " 가격조건.
            ZTBL-ZF20FT    TO  IT_TAB-ZF20FT,               " 20FEET
            ZTBL-ZF40FT    TO  IT_TAB-ZF40FT.               " 40FEET
    CLEAR LFA1.
    SELECT SINGLE *
           FROM  LFA1
           WHERE LIFNR = IT_TAB-LIFNR.
    IF SY-SUBRC EQ 0.
      MOVE LFA1-NAME1 TO IT_TAB-NAME1.
    ENDIF.
    IF IT_TAB-ZFCLNO IS INITIAL.
      IT_TAB-CLNO = 'Credit on'.
    ELSE.
      IT_TAB-CLNO = 'Advanced Payment'.
    ENDIF.
*>> 텍스트 요소.
    PERFORM  GET_DD07T_SELECT(SAPMZIM00) USING 'ZDCSTGRP'
                                         IT_TAB-ZFCSTGRP
                              CHANGING   IT_TAB-CSTGRP.
    CLEAR ZTIMIMG08.
    SELECT SINGLE *
           FROM ZTIMIMG08
          WHERE ZFCDTY   =  IT_TAB-ZFCSTGRP
            AND ZFCD     =  IT_TAB-ZFCD.
    IF SY-SUBRC EQ 0.
      MOVE  ZTIMIMG08-ZFCDNM TO IT_TAB-ZFCDNM.
    ENDIF.

    IF IT_TAB-ZFRVSX EQ 'X'.
      IT_TAB-WRBTR = IT_TAB-WRBTR * -1.
      IT_TAB-CLNO = 'Reverse'.
    ENDIF.

    MODIFY IT_TAB INDEX W_TABIX.
  ENDLOOP.

ENDFORM.                    " P1000_READ_DATA
*&---------------------------------------------------------------------
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------
FORM P3000_DATA_WRITE .

  SET TITLEBAR  'ZIMR81'.
  SET PF-STATUS 'ZIMR81'.

  SORT IT_TAB BY ZFCNAME UTME.
  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    ON CHANGE OF IT_TAB-ZFIMDNO.
      PERFORM   P3000_HEAD_WRITE.
    ENDON.
    PERFORM   P3000_ITEM_WRITE.

  ENDLOOP.
  WRITE:/ SY-ULINE.
  CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------
*&      Form  P2000_INIT
*&---------------------------------------------------------------------
FORM P2000_INIT.

  SET TITLEBAR  'ZIMR81'.

ENDFORM.                    " P2000_INIT
*&---------------------------------------------------------------------*
*&      Form  P3000_HEAD_WRITE
*&---------------------------------------------------------------------*
FORM P3000_HEAD_WRITE.

  WRITE:/ SY-ULINE.
  WRITE:/ SY-VLINE,'Pur. Order:' , IT_TAB-ZFREBELN,
                  (01) '',
                   'B/L N/o:', IT_TAB-ZFHBLNO,
                   'Crt. on:', IT_TAB-CPUDT,IT_TAB-CPUTM,
                  (01) '',
                   'Crt. by:',  IT_TAB-ZFCNAME,
          118 SY-VLINE NO-GAP.

  WRITE:/ SY-VLINE,'Loading:',          IT_TAB-ZFSPRTC,
                   'Loading Country:',  IT_TAB-ZFCARC,
                   'Cargo type:',IT_TAB-ZFSHTY,
                   'Incoterms:', IT_TAB-INCO1,
                   (01) '',
                   '20Ct:',  IT_TAB-ZF20FT,
                   '40Ct:',  IT_TAB-ZF40FT,
          118 SY-VLINE .
  WRITE:/ SY-ULINE.
  WRITE:/(04) '',
         (12) 'Account Doc.'   CENTERED,
         (18)'Charge Category' CENTERED,
         (15) 'Amount'         CENTERED,
         (20)'Vendor'          CENTERED,
         (15)'payment method'  CENTERED,
         (10)'Tax'             CENTERED,
         (15)'Business Place'  CENTERED,
         (04)''.
  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_HEAD_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_ITEM_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ITEM_WRITE.

  WRITE:/(04) '',
        (12) IT_TAB-ZFACDO CENTERED,    "  회계문서번호',
        (18) IT_TAB-ZFCDNM CENTERED,  " '비용항목',
        (15) IT_TAB-WRBTR CURRENCY 'KRW',	
        (20) IT_TAB-NAME1 CENTERED,        "'거래선',
        (15) IT_TAB-CLNO  CENTERED,        "지불방법',
        (10) IT_TAB-MWSKZ CENTERED,         "세금코드',
        (15) IT_TAB-BUPLA CENTERED.         "속사업장'.
  HIDE: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_ITEM_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_COST_DOCUMENT
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_COST_DOCUMENT USING    P_BUKRS
                                          P_GJAHR
                                          P_BELNR.

  SET  PARAMETER ID  'BUK'       FIELD   P_BUKRS.
  SET  PARAMETER ID  'GJR'       FIELD   P_GJAHR.
  SET  PARAMETER ID  'ZPBENR'    FIELD   P_BELNR.
  CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P2000_DISPLAY_COST_DOCUMENT
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> Company Code SET.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
