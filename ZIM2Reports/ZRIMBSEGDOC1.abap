*&---------------------------------------------------------------------
*& Report  ZRIMBSEGDOC
*&---------------------------------------------------------------------
*&  프로그램명 : Freight Daily List.
*&      작성자 : Nashinho INFOLINK Ltd.
*&      작성일 : 2001.09.27
*&---------------------------------------------------------------------
*&   DESC.     :
*&---------------------------------------------------------------------
*& [변경내용]
*&---------------------------------------------------------------------
REPORT  ZRIMBSEGLST  MESSAGE-ID ZIM
                     LINE-SIZE 118
                     NO STANDARD PAGE HEADING.
TABLES: ZTBKPF, ZTBSEG, ZTBL, ZTIDS, ZTIMIMG10, LFA1, ZTIV, ZTCUCLIV,
        EKKO, T001, ZTIMIMG02, ZTIMIMG08, ZTIMIMG00.

TYPE-POOLS : SLIS.
*&---------------------------------------------------------------------*
*&  리스트용 INTERNAL TABLE
*&---------------------------------------------------------------------*

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
       ZFREBELN  LIKE    EKKO-EBELN,         " P/O NO
       ZFHBLNO   LIKE    ZTBL-ZFHBLNO,       " B/L NO
       ZFNEWT    LIKE    ZTBL-ZFNEWT,        " 총중량.
       ZFNEWTM   LIKE    ZTBL-ZFNEWTM,        " 총중량.
       ZFTOVL    LIKE    ZTBL-ZFTOVL,        " 총용적.
       ZFTOVLM   LIKE    ZTBL-ZFTOVLM,       " 총용적.
       ZFSPRTC   LIKE    ZTBL-ZFSPRTC,       " 선적항.
       ZFCARC    LIKE    ZTBL-ZFCARC,        " 선적국.
       ZFSHTY    LIKE    ZTBL-ZFSHTY,        " 화물형태.
       INCO1     LIKE    ZTBL-INCO1,         " 가격조건.
       ZF20FT    LIKE    ZTBL-ZF20FT,        " 20 Feet
       ZF40FT    LIKE    ZTBL-ZF40FT,        " 40 Feet
       ZF20FHQ   LIKE    ZTBL-ZF20FHQ,       " 20 Feet H.Q.
       ZF40FHQ   LIKE    ZTBL-ZF40FHQ.       " 40 Feet H.Q.
DATA : END OF IT_TAB.

DATA : W_STCD2   LIKE    LFA1-STCD2,         " 사업자등록번호.
       W_ZFACTNO LIKE    ZTIMIMG02-ZFACTNO.  " 계좌번호.

DATA :  W_ERR_CHK       TYPE C,
        W_LCOUNT        TYPE I,
        W_FIELD_NM      TYPE C,
        W_PAGE          TYPE I,
        W_CHECK_PAGE(1) TYPE C,
        W_LINE          TYPE I,
        W_COUNT         TYPE I,
        W_TABIX         LIKE SY-TABIX,
        P_BUKRS         LIKE ZTBL-BUKRS.
DATA: CURSORFIELD(20).

*>> Declaration of variable for ALV Display.
DATA: G_REPID LIKE SY-REPID.
DATA: G_LAYOUT          TYPE SLIS_LAYOUT_ALV.
DATA: G_STATUS          TYPE SLIS_FORMNAME VALUE 'P2000_ALV_PF_STATUS'.
DATA: GT_FIELDCAT       TYPE SLIS_T_FIELDCAT_ALV.
DATA: GT_SORT           TYPE SLIS_T_SORTINFO_ALV.
DATA: LS_FIELDCAT       TYPE SLIS_FIELDCAT_ALV.
DATA: LS_SORT           TYPE SLIS_SORTINFO_ALV.
DATA: POS               TYPE I.
DATA: G_SAVE(1)         TYPE C.
DATA: G_VARIANT         LIKE DISVARIANT.
DATA: G_USER_COMMAND    TYPE SLIS_FORMNAME VALUE 'P2000_ALV_COMMAND'.

*&---------------------------------------------------------------------*
* Selection Screen Clause
*&---------------------------------------------------------------------*
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
           AND  ( R~ZFCSTGRP NE '003' AND R~ZFCSTGRP NE '006' ).
  IF SY-SUBRC NE 0.  W_ERR_CHK = 'Y'. EXIT.  ENDIF.

  SORT IT_TAB BY ZFACDO ZFCNAME CPUDT.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
*    IF IT_TAB-ZFCSTGRP EQ '006'.
*      CLEAR ZTIV.
*      SELECT  SINGLE *
*          FROM  ZTIV
*          WHERE ZFIVNO = IT_TAB-ZFIMDNO.
*      IT_TAB-ZFIMDNO = ZTIV-ZFBLNO.
*    ENDIF.
*---------------B/L DATA MOVE--------------------------------------
    CLEAR ZTBL.
    SELECT SINGLE *
        FROM ZTBL
       WHERE ZFBLNO  = IT_TAB-ZFIMDNO.
    IF SY-SUBRC NE 0.
      DELETE IT_TAB INDEX W_TABIX.
      CONTINUE.
    ENDIF.
    MOVE:   ZTBL-ZFREBELN  TO  IT_TAB-ZFREBELN,      " P/O No
            ZTBL-ZFHBLNO   TO  IT_TAB-ZFHBLNO,       " B/L No.
            ZTBL-ZFNEWT    TO  IT_TAB-ZFNEWT,        " 총중량.
            ZTBL-ZFNEWTM   TO  IT_TAB-ZFNEWTM,       " 총중량.
            ZTBL-ZFTOVL    TO  IT_TAB-ZFTOVL,        " 총용적.
            ZTBL-ZFTOVLM   TO  IT_TAB-ZFTOVLM,       " 총용적.
            ZTBL-ZFSPRTC   TO  IT_TAB-ZFSPRTC,       " 선적항.
            ZTBL-ZFCARC    TO  IT_TAB-ZFCARC,        " 선적국.
            ZTBL-ZFSHTY    TO  IT_TAB-ZFSHTY,        " 화물형태.
            ZTBL-INCO1     TO  IT_TAB-INCO1,         " 가격조건.
            ZTBL-ZF20FT    TO  IT_TAB-ZF20FT,        " 20 Feet.
            ZTBL-ZF40FT    TO  IT_TAB-ZF40FT,        " 40 Feet.
            ZTBL-ZF20FHQ   TO  IT_TAB-ZF20FHQ,       " 20 Feet H.Q.
            ZTBL-ZF40FHQ   TO  IT_TAB-ZF40FHQ.       " 40 Feet H.Q.
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
  SET PF-STATUS 'ZIMY81A'.

  PERFORM P3000_APPEND_FIELDCAT.      " ALV Report TiTle.

*  SORT IT_TAB BY ZFCNAME UTME.
  G_REPID = SY-REPID.
  DATA: SLIS_FORMNAME(30)  TYPE C.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = G_REPID
            IS_LAYOUT                = G_LAYOUT
            IT_FIELDCAT              = GT_FIELDCAT[]
            IT_SORT                  = GT_SORT[]
            I_CALLBACK_PF_STATUS_SET = G_STATUS
*            I_CALLBACK_TOP_OF_PAGE   = SLIS_FORMNAME
*            I_HTML_HEIGHT_TOP        = 2
            I_CALLBACK_USER_COMMAND = G_USER_COMMAND
            I_GRID_TITLE             = 'Freight Daily List'
            I_SAVE                   = G_SAVE
            IS_VARIANT               = G_VARIANT
*            I_SCREEN_START_COLUMN = 1000
*            I_SCREEN_START_LINE = 30
       TABLES
            T_OUTTAB           = IT_TAB
       EXCEPTIONS
            PROGRAM_ERROR      = 1
            OTHERS             = 2.
  IF SY-SUBRC <> 0.
    MESSAGE E977 WITH 'An Error occured during Grid Dispaly .'.
  ENDIF.

*  LOOP AT IT_TAB.
*    W_TABIX = SY-TABIX.
*    ON CHANGE OF IT_TAB-ZFIMDNO.
*      PERFORM   P3000_HEAD_WRITE.
*    ENDON.
*    PERFORM   P3000_ITEM_WRITE.
*
*  ENDLOOP.
*  WRITE:/ SY-ULINE.

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
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
FORM P2000_ALV_PF_STATUS USING  EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'ZIMY81A'.
ENDFORM.                    " P2000_ALV_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
FORM P2000_ALV_COMMAND USING R_UCOMM      LIKE SY-UCOMM
                              RS_SELFIELD TYPE SLIS_SELFIELD.
  CASE R_UCOMM.
    READ TABLE IT_TAB INDEX RS_SELFIELD-TABINDEX.
*>> Display Material Master.
    WHEN 'DSCD'.
      PERFORM P2000_DSCD USING IT_TAB-BELNR IT_TAB-BUKRS IT_TAB-GJAHR.
*>> Display Purchase Order.
    WHEN 'DSFI'.
      PERFORM P2000_DSFI USING IT_TAB-ZFACDO IT_TAB-BUKRS IT_TAB-GJAHR.
*>> Display Bill of Lading.
    WHEN 'DSBL'.
*      PERFORM P2000_DSBL USING IT_TAB-ZFBLNO.
*>> Display Inbound Delivery.
    WHEN 'DSIB'.
*      PERFORM P2000_DSIB USING IT_TAB-VBELN.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_ALV_COMMAND
*&---------------------------------------------------------------------*
*&      Form  P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
FORM P3000_APPEND_FIELDCAT.

  CLEAR: GT_FIELDCAT, GT_SORT, POS.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFCARC'.
  LS_FIELDCAT-SELTEXT_M      = 'Load.Cntry'.
  LS_FIELDCAT-OUTPUTLEN      = 3.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'ZFCARC'.
  LS_FIELDCAT-KEY            = 'X'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFSPRTC'.
  LS_FIELDCAT-SELTEXT_M      = 'Load.Port'.
  LS_FIELDCAT-OUTPUTLEN      = 3.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'ZFSPRTC'.
  LS_FIELDCAT-KEY            = 'X'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'INCO1'.
  LS_FIELDCAT-SELTEXT_M      = 'Incoterms'.
  LS_FIELDCAT-OUTPUTLEN      = 4.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'INCO1'.
  LS_FIELDCAT-KEY            = 'X'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFHBLNO'.
  LS_FIELDCAT-SELTEXT_M      = 'House B/L No.'.
  LS_FIELDCAT-OUTPUTLEN      = 18.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_FIELDCAT-FIX_COLUMN     = '0'.
  LS_FIELDCAT-KEY            = 'X'.
  LS_SORT-FIELDNAME          = 'ZFHBLNO'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFIMDNO'.
  LS_FIELDCAT-SELTEXT_M      = 'B/L No.'.
  LS_FIELDCAT-OUTPUTLEN      = 10.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_FIELDCAT-KEY            = 'X'.
  LS_SORT-FIELDNAME          = 'ZFIMDNO'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFREBELN'.
  LS_FIELDCAT-SELTEXT_M      = 'P/O No.'.
  LS_FIELDCAT-OUTPUTLEN      = 10.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'ZFREBELN'.
  LS_FIELDCAT-KEY            = 'X'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFACDO'.
  LS_FIELDCAT-SELTEXT_M      = 'Charge Doc.'.
  LS_FIELDCAT-OUTPUTLEN      = 10.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFCDNM'.
  LS_FIELDCAT-SELTEXT_M      = 'Charge Category'.
  LS_FIELDCAT-OUTPUTLEN      = 18.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'WRBTR'.
  LS_FIELDCAT-SELTEXT_M      = 'Amount'.
  LS_FIELDCAT-OUTPUTLEN      = 14.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'NAME1'.
  LS_FIELDCAT-SELTEXT_M      = 'Vendor'.
  LS_FIELDCAT-OUTPUTLEN      = 18.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'MWSKZ'.
  LS_FIELDCAT-SELTEXT_M      = 'Tax'.
  LS_FIELDCAT-OUTPUTLEN      = 3.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZF20FT'.
  LS_FIELDCAT-SELTEXT_M      = '20Ft'.
  LS_FIELDCAT-OUTPUTLEN      = 2.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZF40FT'.
  LS_FIELDCAT-SELTEXT_M      = '40Ft'.
  LS_FIELDCAT-OUTPUTLEN      = 2.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZF20FHQ'.
  LS_FIELDCAT-SELTEXT_M      = '45Ft'.
  LS_FIELDCAT-OUTPUTLEN      = 2.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZF40FHQ'.
  LS_FIELDCAT-SELTEXT_M      = '40HQ'.
  LS_FIELDCAT-OUTPUTLEN      = 2.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'CPUDT'.
  LS_FIELDCAT-SELTEXT_M      = 'Created on'.
  LS_FIELDCAT-OUTPUTLEN      = 10.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'CPUDT'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

  CLEAR: LS_FIELDCAT, LS_SORT.
  POS = POS + 1.
  LS_FIELDCAT-COL_POS        = POS.
  LS_FIELDCAT-FIELDNAME      = 'ZFCNAME'.
  LS_FIELDCAT-SELTEXT_M      = 'Created by'.
  LS_FIELDCAT-OUTPUTLEN      = 10.
  LS_FIELDCAT-EMPHASIZE      = 'C200'.
  LS_SORT-FIELDNAME          = 'ZFCNAME'.
  APPEND LS_SORT     TO GT_SORT.
  APPEND LS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " P3000_APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  P2000_DSCD
*&---------------------------------------------------------------------*
FORM P2000_DSCD USING    BELNR  BUKRS  GJAHR.

  SET PARAMETER ID 'ZPBENR' FIELD BELNR.
  SET PARAMETER ID 'BUK'    FIELD BUKRS.
  SET PARAMETER ID 'GJR'    FIELD GJAHR.
  CALL TRANSACTION 'ZIMY3'.

ENDFORM.                    " P2000_DSCD
*&---------------------------------------------------------------------*
*&      Form  P2000_DSFI
*&---------------------------------------------------------------------*
FORM P2000_DSFI USING  BELNR BUKRS GJAHR.

  SET PARAMETER ID 'BLN' FIELD BELNR.
  SET PARAMETER ID 'BUK' FIELD BUKRS.
  SET PARAMETER ID 'GJR' FIELD GJAHR.
  CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

ENDFORM.                    " P2000_DSFI
