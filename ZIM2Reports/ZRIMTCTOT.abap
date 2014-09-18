*&---------------------------------------------------------------------*
*& Report  ZRIMTCTOT                                                   *
*&---------------------------------------------------------------------*
*&  프로그램명 : Invoice 집계 - 마감전표집계(LO, PU)                   *
*&      작성자 : 강석봉 INFOLINK Ltd.                                  *
*&      작성일 : 2001.01.30                                            *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&
*&---------------------------------------------------------------------*
REPORT  ZRIMTCTOT    MESSAGE-ID ZIM
                     LINE-SIZE 138
                     NO STANDARD PAGE HEADING.

*-----------------------------------------------------------------------
* 사용할 TABLE DECLRAE
*-----------------------------------------------------------------------
TABLES : BKPF,            "회계전표 Header
         BSEG,            "회계전표 Body
         EKKO,            "구매오더 헤더.
         EKPO,            "구매문서 품목.
         EKAB,            "릴리즈 문서.
         RBKP,            "문서헤더: 송장입고.
         ZTREQHD,         "수입의뢰 Header
         ZTREQST,         "수입의뢰 Status
         ZTVTIV,          "세금계산서용 Invoice
         ZTVTIVIT,        "세금계산서용 Invoice Item
         ZTIMIMG00,
         ZTIMIMG11,       "G/R, I/V 비용처리를 위한 Configuration
         ZTCIVHD,         "Commercial I/V Header
         ZTCIVIT,         "Commercial I/V Item
         ZTCIVHST,
         ZTREQIT.
*-----------------------------------------------------------------------
* COMMERCIAL I/V NO SELECT INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_CIV OCCURS 0,
       ZFCIVRN         LIKE   ZTCIVHD-ZFCIVRN.      " Posting Date
DATA : END OF IT_CIV.

*-----------------------------------------------------------------------
* COMMERCIAL I/V HISTORY SELECT INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_CIVHST OCCURS 0,
       ZFCIVRN         LIKE   ZTCIVHST-ZFCIVRN,
       ZFCIVHST        LIKE   ZTCIVHST-ZFCIVHST.
DATA : END OF IT_CIVHST.
*-----------------------------------------------------------------------
* COMMERCIAL I/V 전표번호 SELECT INTERNAL TABLE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_CIVNO OCCURS 0,
       BUKRS         LIKE   ZTCIVHST-BUKRS,
       GJAHR         LIKE   ZTCIVHST-GJAHR,
       BELNR         LIKE   ZTCIVHST-BELNR,
       AWKEY         LIKE   BKPF-AWKEY.
DATA : END OF IT_CIVNO.

*-----------------------------------------------------------------------
* 세금계산서용 IV INTERNAL TABLE & VARIABLE DECLARE
*-----------------------------------------------------------------------
DATA : BEGIN OF IT_TAB OCCURS 0,
       MARK            TYPE   C,
       ZFPOSDT         LIKE   ZTVTIV-ZFPOSDT,      " Posting Date
       ZFDODT          LIKE   ZTVTIV-ZFDODT,       " Document Date
       LIFNR           LIKE   ZTVTIV-LIFNR,        " Vendor
       LIFNR_NM(20)    TYPE   C,                   " Vendor Name
       EBELN           LIKE   ZTVTIVIT-EBELN,      " P/O No
       EBELP           LIKE   ZTVTIVIT-EBELP,
       ZFREQTY         LIKE   ZTVTIVIT-ZFREQTY,    " L/C Type
       ZFGFDYR         LIKE   ZTVTIV-ZFGFDYR,      " 물대전표 연?
       ZFGFDNO         LIKE   ZTVTIV-ZFGFDNO,      " 물대전표 번?
       BUKRS           LIKE   ZTVTIV-BUKRS,
       ZFIVAMT         LIKE   ZTVTIVIT-ZFIVAMT,    " Invoice 금?
       ZFIVAMC         LIKE   ZTVTIVIT-ZFIVAMC,    " Invoice 금액 통?
       CHK_YN(6)       TYPE   C.
DATA : END OF IT_TAB.

DATA : BEGIN OF IT_BKPF OCCURS 0.
       INCLUDE  STRUCTURE  BKPF.
DATA : END   OF IT_BKPF.

DATA : BEGIN OF IT_ZTREQHD OCCURS 0.
       INCLUDE  STRUCTURE  ZTREQHD.
DATA : END   OF IT_ZTREQHD.

DATA : BEGIN OF IT_ZTREQIT OCCURS 0.
       INCLUDE  STRUCTURE  ZTREQIT.
DATA : END   OF IT_ZTREQIT.

DATA : BEGIN OF IT_BSEG OCCURS 0.
       INCLUDE  STRUCTURE  BSEG.
DATA : END   OF IT_BSEG.

DATA : BEGIN OF IT_IV OCCURS 0.
       INCLUDE  STRUCTURE  ZTVTIV.
DATA   CHK_YN   TYPE     C.
DATA   DATA_YN  TYPE     C.
DATA : END   OF IT_IV.

DATA : BEGIN OF IT_VTIV OCCURS 0.
       INCLUDE  STRUCTURE  ZTVTIV.
DATA   CHK_YN   TYPE     C.
DATA   DATA_YN  TYPE     C.
DATA : END   OF IT_VTIV.

DATA : BEGIN OF IT_IVIT OCCURS 0.
       INCLUDE  STRUCTURE  ZTVTIVIT.
DATA : END   OF IT_IVIT.

DATA : BEGIN OF IT_VTIVIT OCCURS 0.
       INCLUDE  STRUCTURE  ZTVTIVIT.
DATA : END   OF IT_VTIVIT.


DATA : BEGIN OF IT_TERM OCCURS 0.
       INCLUDE  STRUCTURE  ZTIMIMG01.
DATA : END   OF IT_TERM.

DATA  MARKFIELD.

DATA : XEKAB      LIKE EKAB,
       W_LOOP_CNT TYPE I,
       W_COUNT    TYPE I,
       W_PROC_CNT TYPE I,
       W_DATA_CNT TYPE I,
       W_ERR_CHK  TYPE C,
       W_TABIX    LIKE SY-TABIX,
       W_ZFREQNO  LIKE ZTREQHD-ZFREQNO,
       W_NAME     LIKE LFA1-NAME1,
       W_VTNO     LIKE ZTVTIV-ZFVTNO.

RANGES  R_TERM   FOR    ZTIMIMG01-ZTERM    OCCURS 10.

*INCLUDE   ZRIMSORTCOM.    " 수입의뢰 Report Sort를 위한 Include
INCLUDE   ZRIMUTIL01.     " Utility function 모음.

*-----------------------------------------------------------------------
* 검색조건 WINDOW CREATE
*--------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 1 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   PARAMETERS :    P_BUKRS   LIKE BKPF-BUKRS
                                  OBLIGATORY,      " Company Code.
*                                 DEFAULT  'C100',
                   P_BLART   LIKE BKPF-BLART
*                                  OBLIGATORY       " 문서 종류.
                                  DEFAULT  'I1'.
   SELECT-OPTIONS: S_BUDAT   FOR  BKPF-BUDAT
                                  OBLIGATORY,      " Posting Date
                   S_BLDAT   FOR  BKPF-BLDAT,
*                                  OBLIGATORY,      " Document Date
                   S_LIFNR   FOR  BSEG-LIFNR,      " Vendor
                   S_EBELN   FOR  BSEG-EBELN,      " Purchase Document.
                   S_CIVNO   FOR  ZTCIVHD-ZFCIVNO, " Commercial I/V
                   S_CIVRN   FOR  ZTCIVHD-ZFCIVRN, " Commercial I/V
                   S_OPNNO   FOR  ZTREQHD-ZFOPNNO. " 승인번호(L/C)

  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(12) TEXT-003, POSITION 33.
     PARAMETERS : P_YEAR1   LIKE BKPF-GJAHR.        " 회계년?
     SELECTION-SCREEN : POSITION  38.
     PARAMETERS : P_NO1     LIKE BKPF-BELNR.
     SELECTION-SCREEN : COMMENT 52(2) TEXT-004, POSITION  58.
     PARAMETERS : P_YEAR2     LIKE BKPF-GJAHR.
     SELECTION-SCREEN : POSITION  63.
     PARAMETERS : P_NO2     LIKE BKPF-BELNR.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN SKIP 1.                        " 1 LINE SKIP

  SELECTION-SCREEN : BEGIN OF LINE, COMMENT 1(14) TEXT-002, POSITION 33.
     PARAMETERS : P_YN    AS CHECKBOX.              " 재작업여부..
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK B1.

* 회사코드 검증.
AT SELECTION-SCREEN ON P_BUKRS.

   SELECT  COUNT( * )
   INTO    W_DATA_CNT
   FROM    T001
   WHERE   BUKRS    =    P_BUKRS.

   IF  W_DATA_CNT  =  0.
       MESSAGE E451.
       EXIT.
   ENDIF.

* 회계년도, 전표번호 MIN, MAX SETTING!
AT SELECTION-SCREEN ON P_YEAR1.

   IF P_YEAR1 IS INITIAL.
      SELECT MIN( GJAHR )
      INTO   P_YEAR1
      FROM   BKPF
      WHERE  BLART EQ P_BLART
      AND    BUDAT IN S_BUDAT.
   ENDIF.

AT SELECTION-SCREEN ON P_YEAR2.

   IF P_YEAR2   IS INITIAL.
      SELECT MAX( GJAHR )
      INTO   P_YEAR2
      FROM   BKPF
      WHERE  BLART = P_BLART
      AND    BUDAT IN S_BUDAT.
   ENDIF.

AT SELECTION-SCREEN ON P_NO1.

   IF P_NO1 IS INITIAL.
      SELECT MIN( BELNR )
      INTO   P_NO1
      FROM   BKPF
      WHERE  GJAHR = P_YEAR1
      AND    BLART = P_BLART
      AND    BUDAT IN S_BUDAT.
    ENDIF.

AT SELECTION-SCREEN ON P_NO2.

    IF P_NO2   IS INITIAL.
       SELECT MAX( BELNR )
       INTO   P_NO2
       FROM   BKPF
       WHERE  GJAHR = P_YEAR2
       AND    BLART = P_BLART
       AND    BUDAT IN S_BUDAT.
    ENDIF.

INITIALIZATION.
  PERFORM P1000_INIT_DATA.

*-----------------------------------------------------------------------
* REPORT WRITE 시 PAGE의 TOP EVENT
*-----------------------------------------------------------------------
TOP-OF-PAGE.
  PERFORM P2000_TITLE_WRITE.

*-----------------------------------------------------------------------
* START-OF-SELECTION 절..
*-----------------------------------------------------------------------
START-OF-SELECTION.

*---------------------------------------------------
*> IMG 체크 ( 2001/10/08 KSB INSERT )
  IF ZTIMIMG00-LOGRIV EQ 'G' OR ZTIMIMG00-LOGRIV EQ 'X'.
     PERFORM P1000_READ_STANDARD_DATA.
  ELSE.
     PERFORM P1000_READ_IMPORT_DATA.
  ENDIF.
  PERFORM P1000_SORT_BKPF_DATA.
  PERFORM P1000_READ_CONTRACT.
* PERFORM P1000_READ_IV_DATA.

* 검색 해당 DATA 여부 CHECK.
  IF  W_LOOP_CNT  <  1.
      MESSAGE   S738.
      EXIT.
  ENDIF.

*-----------------------------------------------------------------------
* SELECT 이후의 EVENT.
*-----------------------------------------------------------------------
END-OF-SELECTION.

* Title Text Write.
  SET TITLEBAR 'ZIMA0N'.
  SET PF-STATUS 'ZIMA0N'.

  PERFORM P2000_SORT_DATA.
  PERFORM P2000_TAB_DATA.
  PERFORM P2000_DATA_WRITE.

*----------------------------------------------------------------------
* User Command
*----------------------------------------------------------------------
AT USER-COMMAND.

   CASE SY-UCOMM.

      WHEN 'MKAL' OR 'MKLO'.         " 전체 선택 및 선택해?
            PERFORM P3000_SELECT_RECORD   USING   SY-UCOMM.
      WHEN 'REFL'.                    " 세금계산서용 Invoice 조?
            PERFORM P3000_SAVE_DATA.
            IF  W_PROC_CNT   =  0.
                MESSAGE  S766.
                EXIT.
            ENDIF.
            PERFORM  P4000_MODIFY_LINE USING  SY-UCOMM.
            MESSAGE S813  WITH W_DATA_CNT.
      WHEN 'DELA'.                    " 세금계산서용 Invoice 조?
            PERFORM  P3000_DEL_DATA.
            IF W_PROC_CNT  =  0.
               MESSAGE  S766.
               EXIT.
            ENDIF.
            PERFORM  P4000_MODIFY_LINE  USING  SY-UCOMM.
            MESSAGE S450  WITH W_DATA_CNT.
      WHEN OTHERS.

   ENDCASE.

   CLEAR IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IMPORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_IMPORT_DATA.

* COMMERCIAL I/V DATA SELECT.
    SELECT DISTINCT H~ZFCIVRN  INTO CORRESPONDING FIELDS OF TABLE IT_CIV
    FROM   ZTCIVHD AS H INNER JOIN ZTCIVIT AS I
    ON     H~ZFCIVRN    EQ    I~ZFCIVRN
    WHERE  H~BUKRS      EQ    P_BUKRS
    AND    H~BLART      EQ    P_BLART
    AND    H~ZFCIVRN    IN    S_CIVRN
    AND    H~ZFCIVNO    IN    S_CIVNO
    AND    H~ZFMAVN     IN    S_LIFNR
*    AND   ( H~ZFREQTY   EQ    'PU'   OR  H~ZFREQTY  EQ  'LO' ).
*   AND   ( H~ZFREQTY   EQ    'PU'   OR  H~ZFREQTY  EQ  'LO' ).
   AND     H~ZFREQTY    EQ    'LO'.

*해당 COMMERCIAL IV NO의 MAX( SEQ ) SELECT!
    SELECT  H~ZFCIVRN  MAX( H~ZFCIVHST ) AS ZFCIVHST
    INTO    CORRESPONDING FIELDS OF TABLE IT_CIVHST
    FROM    ZTCIVHST AS H INNER JOIN ZTCIVHD AS I
    ON      H~ZFCIVRN    EQ    I~ZFCIVRN
    WHERE   I~ZFCIVRN    IN    S_CIVRN
    AND     I~ZFCIVNO    IN    S_CIVNO
    AND     I~ZFMAVN     IN    S_LIFNR
*    AND    ( I~ZFREQTY   EQ    'PU'   OR  I~ZFREQTY  EQ  'LO' )
    AND     I~ZFREQTY    EQ    'LO'
    GROUP BY
            H~ZFCIVRN.

*전표번호 SELECT!
    SELECT  BUKRS  GJAHR  BELNR
    INTO    CORRESPONDING FIELDS OF TABLE IT_CIVNO
    FROM    ZTCIVHST
    FOR     ALL  ENTRIES  IN  IT_CIVHST
    WHERE   ZFCIVRN       EQ  IT_CIVHST-ZFCIVRN
    AND     ZFCIVHST      EQ  IT_CIVHST-ZFCIVHST.

    CLEAR  W_TABIX.
    LOOP  AT  IT_CIVNO.
       W_TABIX  =  SY-TABIX.
       MOVE  IT_CIVNO-BELNR  TO  IT_CIVNO-AWKEY(10).
       MOVE  IT_CIVNO-GJAHR  TO  IT_CIVNO-AWKEY+10(4).
       MODIFY  IT_CIVNO INDEX  W_TABIX.
    ENDLOOP.

* BKPF TABLE READ.
    SELECT  *
    INTO    CORRESPONDING FIELDS OF TABLE IT_BKPF
    FROM    BKPF
    FOR     ALL  ENTRIES  IN  IT_CIVNO
    WHERE   BLART  EQ       P_BLART
    AND     BUKRS  EQ       P_BUKRS
    AND     BUDAT  IN       S_BUDAT
    AND     BLDAT  IN       S_BLDAT
    AND (   STBLG  EQ       SPACE         ">역분개전표번호.
    OR      STBLG  IS       NULL )
    AND     AWKEY  EQ       IT_CIVNO-AWKEY
    AND     GJAHR  BETWEEN  P_YEAR1  AND  P_YEAR2
    AND     BELNR  BETWEEN  P_NO1    AND  P_NO2  .

    IF SY-SUBRC NE 0.
       MESSAGE S738.
       EXIT.
    ENDIF.

* BSEG TABLE DATA READ.
    SELECT  *
    INTO    CORRESPONDING FIELDS OF TABLE IT_BSEG
    FROM    BSEG
    FOR ALL ENTRIES  IN       IT_BKPF
    WHERE   BUKRS    EQ       IT_BKPF-BUKRS
    AND     BELNR    EQ       IT_BKPF-BELNR
    AND     GJAHR    EQ       IT_BKPF-GJAHR
    AND ( ( BSCHL    EQ       '31'
    AND     AUGBL    EQ       SPACE      )
    OR    ( BSCHL    EQ       '86'
    AND     EBELN    IN       S_EBELN    ) ).

    IF SY-SUBRC NE 0.
       MESSAGE S738.
       EXIT.
    ENDIF.

ENDFORM.                    " P1000_READ_BKPF_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_SORT_BKPF_DATA
*&---------------------------------------------------------------------*
*       INTERNAL TABLE 의 DATA를 SORT한다.
*----------------------------------------------------------------------*
FORM P1000_SORT_BKPF_DATA.

   SORT IT_BKPF   BY  BUKRS  BELNR  GJAHR.
   SORT IT_BSEG   BY  BUKRS  BELNR  GJAHR  BUZEI.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P1000_READ_IV_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_IV_DATA.
*>> 2001.03.29 KSB INSERT
DATA : L_DATA_CHK,
       L_SUBRC      LIKE   SY-SUBRC.

     REFRESH  IT_IV.
     REFRESH  IT_IVIT.
     REFRESH  IT_TAB.
     W_LOOP_CNT  =  0.

     LOOP AT IT_BKPF.

       CLEAR  IT_IV.

       MOVE  IT_BKPF-GJAHR   TO  IT_IV-ZFGFDYR.
       MOVE  IT_BKPF-BELNR   TO  IT_IV-ZFGFDNO.
       MOVE  IT_BKPF-BUKRS   TO  IT_IV-BUKRS.
       MOVE  IT_BKPF-BUDAT   TO  IT_IV-ZFPOSDT.
       MOVE  IT_BKPF-BLDAT   TO  IT_IV-ZFDODT.
       MOVE  IT_BKPF-KURSF   TO  IT_IV-ZFEXRT.
       MOVE  '         '     TO  IT_IV-ZFVTNO.
       MOVE  '         '     TO  IT_IV-ZFREDNO.
       MOVE  SY-UNAME        TO  IT_IV-ERNAM.
       MOVE  SY-DATUM        TO  IT_IV-CDAT.
       MOVE  SY-UNAME        TO  IT_IV-UNAM.
       MOVE  SY-DATUM        TO  IT_IV-UDAT.

       READ TABLE   IT_BSEG WITH KEY BUKRS = IT_BKPF-BUKRS
                                     BELNR = IT_BKPF-BELNR
                                     GJAHR = IT_BKPF-GJAHR
                                     BSCHL = '31'        .
       IF SY-SUBRC  EQ   0 .
          MOVE IT_BSEG-LIFNR         TO IT_IV-LIFNR.
       ELSE.
          CONTINUE.
       ENDIF.

*>> 후속작업 진행 여부 확인(세금계사서 편성 여부).
       CLEAR  W_VTNO.
       SELECT SINGLE  ZFVTNO
       INTO   W_VTNO
       FROM   ZTVTIV
       WHERE  ZFGFDYR = IT_BKPF-GJAHR
       AND    ZFGFDNO = IT_BKPF-BELNR
       AND    BUKRS   = IT_BKPF-BUKRS.
       L_SUBRC = SY-SUBRC.

       IF L_SUBRC  =  0  AND  NOT W_VTNO IS INITIAL.
          CONTINUE.
       ENDIF.

*>> 2001.03.29 KSB INSERT
       L_DATA_CHK = 'N'.
       LOOP AT IT_BSEG WHERE  BUKRS = IT_BKPF-BUKRS
                       AND    BELNR = IT_BKPF-BELNR
                       AND    GJAHR = IT_BKPF-GJAHR
                       AND    BSCHL = '86'
                       AND    BUZID = 'W'.

         CLEAR IT_IVIT.

* 구매승인서, LOCAL LC 는 PO와 1:1 관계.
         SELECT MAX( ZFREQNO )
         INTO   W_ZFREQNO
         FROM   ZTREQHD
         WHERE  EBELN  =  IT_BSEG-EBELN.

         IF W_ZFREQNO IS INITIAL.
            CONTINUE.
         ENDIF.

         IF ZTIMIMG00-LOGRIV EQ 'G' OR ZTIMIMG00-LOGRIV EQ 'X'.
            SELECT SINGLE *
            FROM   ZTREQHD
            WHERE  ZFREQNO =  W_ZFREQNO
            AND    ZFOPNNO IN S_OPNNO
            AND    ZFREQTY =  'LO'.
         ELSE.
            SELECT SINGLE *
            FROM   ZTREQHD
            WHERE  ZFREQNO =  W_ZFREQNO
            AND    ZFOPNNO IN S_OPNNO
            AND  ( ZFREQTY =  'LO'
            OR     ZFREQTY =  'PU'  ).
         ENDIF.

         IF SY-SUBRC  NE  0.
            CONTINUE.
         ENDIF.

*---------------------------------------------------
*> 문서상태 체크 ( 2001/10/08 KSB INSERT )
         SELECT SINGLE * FROM ZTREQST
                WHERE    ZFREQNO EQ ZTREQHD-ZFREQNO
                AND      ZFAMDNO EQ
                       ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                         WHERE ZFREQNO EQ ZTREQHD-ZFREQNO ).
         IF ZTREQST-ZFDOCST NE 'O'.
            CONTINUE.
         ENDIF.
*---------------------------------------------------

*>> 수입의뢰 ITEM SELECT
         SELECT SINGLE * FROM ZTREQIT
         WHERE  EBELN    EQ   IT_BSEG-EBELN
         AND    EBELP    EQ   IT_BSEG-EBELP.

         MOVE IT_BSEG-WERKS    TO IT_IVIT-ZFWERKS.
         MOVE W_ZFREQNO        TO IT_IVIT-ZFREQNO.
         MOVE IT_BSEG-EBELN    TO IT_IVIT-EBELN.
         MOVE IT_BSEG-EBELP    TO IT_IVIT-EBELP.
         MOVE ZTREQHD-ZFREQTY  TO IT_IVIT-ZFREQTY.
         MOVE ZTREQIT-ZFITMNO  TO IT_IVIT-ZFITMNO.
         MOVE IT_BSEG-GJAHR    TO IT_IVIT-ZFGFDYR.
         MOVE IT_BSEG-BELNR    TO IT_IVIT-ZFGFDNO.
         MOVE IT_BSEG-EBELP    TO IT_IVIT-ZFIVDNO.
         MOVE IT_BSEG-BUKRS    TO IT_IVIT-BUKRS.
         MOVE IT_BSEG-MENGE    TO IT_IVIT-ZFQUN.
         MOVE IT_BSEG-MEINS    TO IT_IVIT-ZFQUNM.

         SELECT SINGLE *
         FROM   EKPO
         WHERE  EBELN = IT_BSEG-EBELN
         AND    EBELP = IT_BSEG-EBELP.

         MOVE EKPO-NETPR         TO IT_IVIT-NETPR.
         MOVE EKPO-PEINH         TO IT_IVIT-PEINH.
         MOVE IT_BSEG-MATNR      TO IT_IVIT-MATNR.
         MOVE IT_BKPF-WAERS      TO IT_IVIT-ZFIVAMC.
         MOVE IT_BSEG-BPRME      TO IT_IVIT-BPRME.
         MOVE 'KRW'              TO IT_IVIT-ZFKRW.

* 후속작업 여부 CHECK.

         W_COUNT  =  0.
*         CLEAR  W_VTNO.
*
*         SELECT SINGLE  ZFVTNO
*         INTO   W_VTNO
*         FROM   ZTVTIV
*         WHERE  ZFGFDYR = IT_BSEG-GJAHR
*         AND    ZFGFDNO = IT_BSEG-BELNR
*         AND    BUKRS   = IT_BSEG-BUKRS.
*
         IF  L_SUBRC  =  0.
             IF  W_VTNO  IS  INITIAL.
                 IT_IV-DATA_YN  =  'N'.
             ELSE.
                 IT_IV-DATA_YN  =  'Y'.
             ENDIF.
         ELSE.
             IT_IV-DATA_YN  =  'N'.
         ENDIF.

* 재작업 여부로 인한  DATA의 INTERNAL TABLE INSERT 여부 결정.
         SELECT COUNT( DISTINCT ZFIVDNO )
         INTO   W_COUNT
         FROM   ZTVTIVIT
         WHERE  ZFGFDYR = IT_BSEG-GJAHR
         AND    ZFGFDNO = IT_BSEG-BELNR
         AND    ZFIVDNO = IT_BSEG-EBELP.

         IF  W_COUNT  >  0.
             IF P_YN  NE  'X'.
                CONTINUE.
             ELSE.
                IT_IV-CHK_YN    = 'Y'.
             ENDIF.
         ENDIF.

* PO의 ITEM 별로 금액을 SUM.
         SELECT  *
         FROM    BSEG
         WHERE   BUKRS   =  IT_BSEG-BUKRS
         AND     BELNR   =  IT_BSEG-BELNR
         AND     GJAHR   =  IT_BSEG-GJAHR
         AND     EBELN   =  IT_BSEG-EBELN
         AND     EBELP   =  IT_BSEG-EBELP.

           IF  BSEG-SHKZG   EQ  'S'.
               IT_IVIT-ZFIVAMT  =  IT_IVIT-ZFIVAMT  +   BSEG-WRBTR.
               IT_IVIT-ZFKAMT   =  IT_IVIT-ZFKAMT   +   BSEG-DMBTR.
           ELSE.
               IT_IVIT-ZFIVAMT  =  IT_IVIT-ZFIVAMT  -   BSEG-WRBTR.
               IT_IVIT-ZFKAMT   =  IT_IVIT-ZFKAMT   -   BSEG-DMBTR.
           ENDIF.

         ENDSELECT.

*>> 2001.03.29 KSB MODIFY
*         READ TABLE IT_TERM WITH KEY ZTERM = ZTREQHD-ZTERM.
         MOVE ZTREQHD-ZFREQTY TO IT_IVIT-ZFREQTY.
*         READ TABLE IT_TERM WITH KEY ZTERM = IT_BSEG-ZTERM.

*         IF SY-SUBRC EQ 0.
*            MOVE IT_TERM-ZFREQTY TO IT_IVIT-ZFREQTY.
*         ELSE.
*            CONTINUE.
*         ENDIF.

*>> 2001.03.29 KSB INSERT
         L_DATA_CHK = 'Y'.
         APPEND IT_IVIT.
         W_LOOP_CNT  =  W_LOOP_CNT  +   1.

       ENDLOOP.
*>> 2001.03.29 KSB INSERT
       IF L_DATA_CHK = 'Y'.
          APPEND IT_IV.
       ENDIF.
    ENDLOOP.

ENDFORM.                    " P1000_READ_IV_DATA

*&---------------------------------------------------------------------*
*&      Form  P2000_SORT_DATA
*&---------------------------------------------------------------------*
*       INTERNAL TABLE 의 DATA를 SORT한다.
*----------------------------------------------------------------------*
FORM P2000_SORT_DATA.

   SORT IT_IV    BY  ZFGFDYR ZFGFDNO.
   SORT IT_IVIT  BY  ZFGFDYR ZFGFDNO ZFIVDNO.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P2000_TAB_DATA
*&---------------------------------------------------------------------*
*       INTERNAL TABLE 의 DATA를 WRITE한다.
*----------------------------------------------------------------------*
FORM P2000_TAB_DATA.

    CLEAR    IT_TAB.
    REFRESH  IT_TAB.

    LOOP  AT  IT_IV.

      CLEAR  IT_TAB.
*>> 2001.03.29 KSB INSERT
      ON CHANGE OF IT_IV-ZFGFDYR OR
                   IT_IV-ZFGFDNO OR
                   IT_IV-BUKRS.
         SELECT  SINGLE NAME1
         INTO    W_NAME
         FROM    LFA1
         WHERE   LIFNR   =   IT_IV-LIFNR .

         LOOP  AT  IT_IVIT  WHERE  ZFGFDYR  =  IT_IV-ZFGFDYR
                            AND    ZFGFDNO  =  IT_IV-ZFGFDNO
                            AND    BUKRS    =  IT_IV-BUKRS.

           IT_TAB-ZFPOSDT  =  IT_IV-ZFPOSDT.
           IT_TAB-ZFDODT   =  IT_IV-ZFDODT.
           IT_TAB-LIFNR    =  IT_IV-LIFNR.
           IT_TAB-LIFNR_NM =  W_NAME.
           IT_TAB-ZFGFDYR  =  IT_IV-ZFGFDYR.
           IT_TAB-ZFGFDNO  =  IT_IV-ZFGFDNO.
           IT_TAB-BUKRS    =  IT_IV-BUKRS.
           IT_TAB-EBELN    =  IT_IVIT-EBELN.
           IT_TAB-EBELP    =  IT_IVIT-EBELP.
           IT_TAB-ZFREQTY  =  IT_IVIT-ZFREQTY.
           IT_TAB-ZFIVAMT  =  IT_IVIT-ZFIVAMT.
           IT_TAB-ZFIVAMC  =  IT_IVIT-ZFIVAMC.

           IF  IT_IV-CHK_YN  =  'Y'.
               IT_TAB-CHK_YN =  '기집계'.
           ELSE.
               IT_TAB-CHK_YN =  '      '.
           ENDIF.

           APPEND IT_TAB.

       ENDLOOP.
     ENDON.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P2000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       화면상에 TITLE WRITE 한다.
*----------------------------------------------------------------------*
FORM P2000_TITLE_WRITE.

  SKIP 2.
  FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
  WRITE : /55  '[ 세금계산서용 Invoice 집계 ]'
               COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / 'Date : ', SY-DATUM.
  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-VLINE, ' ',  SY-VLINE NO-GAP,
            'Posting Date '                  NO-GAP, SY-VLINE NO-GAP,
            'Document Date'                  NO-GAP, SY-VLINE NO-GAP,
            '회계전표번호   '                NO-GAP, SY-VLINE NO-GAP,
            'Vendor'                         NO-GAP,
            '                        '       NO-GAP, SY-VLINE NO-GAP,
            'P/O No    '                     NO-GAP, SY-VLINE NO-GAP,
            '품목 '                          NO-GAP, SY-VLINE NO-GAP,
            'Type'                           NO-GAP, SY-VLINE NO-GAP,
            'Invoice 금액'                   NO-GAP,
            '            '                   NO-GAP, SY-VLINE NO-GAP,
            '기집계여부'                     NO-GAP, SY-VLINE NO-GAP.

  WRITE : / SY-ULINE NO-GAP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  P2000_DATA_WRITE
*&---------------------------------------------------------------------*
*       화면상에 DATA를 WRITE 한다.
*----------------------------------------------------------------------*
FORM P2000_DATA_WRITE.

   LOOP  AT  IT_TAB.

     ON CHANGE OF IT_TAB-ZFGFDYR OR
                  IT_TAB-ZFGFDNO OR
                  IT_TAB-BUKRS.

        FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

        WRITE:/ SY-VLINE, MARKFIELD  AS CHECKBOX,
          SY-VLINE NO-GAP,
          IT_TAB-ZFPOSDT    NO-GAP,  " Posting Date
          '   '             NO-GAP,
          SY-VLINE NO-GAP,
          IT_TAB-ZFDODT     NO-GAP,  " Document Date
          '   '             NO-GAP,
          SY-VLINE NO-GAP,
          IT_TAB-ZFGFDYR    NO-GAP, " 회계전표연?
          '-'               NO-GAP,
          IT_TAB-ZFGFDNO    NO-GAP, " 회계전표번?
          SY-VLINE NO-GAP,
          IT_TAB-LIFNR      NO-GAP, " Vendor
          IT_TAB-LIFNR_NM   NO-GAP,
          SY-VLINE NO-GAP,
          '          '      NO-GAP,  SY-VLINE  NO-GAP,
          '     '           NO-GAP,  SY-VLINE  NO-GAP,
          '    '            NO-GAP,  SY-VLINE  NO-GAP,
          '                        '   NO-GAP,
          SY-VLINE  NO-GAP,
          IT_TAB-CHK_YN     NO-GAP,
          '    '            NO-GAP, SY-VLINE NO-GAP.
          HIDE    IT_TAB.

        LOOP AT  IT_TAB  WHERE  ZFGFDYR  =  IT_TAB-ZFGFDYR
                         AND    ZFGFDNO  =  IT_TAB-ZFGFDNO.

           FORMAT COLOR COL_NORMAL INTENSIFIED ON.

           WRITE:/ SY-VLINE,
              '  '              NO-GAP, SY-VLINE  NO-GAP,
              '             '   NO-GAP, SY-VLINE  NO-GAP,
              '             '   NO-GAP, SY-VLINE  NO-GAP,
              '               ' NO-GAP, SY-VLINE  NO-GAP,
              '                              '    NO-GAP,
              SY-VLINE NO-GAP,
              IT_TAB-EBELN      NO-GAP, " P/O No
              SY-VLINE NO-GAP,
              IT_TAB-EBELP      NO-GAP, "품목번?
              SY-VLINE NO-GAP,
              IT_TAB-ZFREQTY    NO-GAP, " L/C Type
              '  '              NO-GAP,
              SY-VLINE NO-GAP,
              IT_TAB-ZFIVAMC    NO-GAP, " Invoice 통?
               IT_TAB-ZFIVAMT CURRENCY IT_TAB-ZFIVAMC NO-GAP, "
              SY-VLINE NO-GAP,
              '          '            NO-GAP, SY-VLINE NO-GAP.
              HIDE    IT_TAB.
         ENDLOOP.
         WRITE : / SY-ULINE.
      ENDON.

      AT  LAST.
           WRITE : / '총', W_LOOP_CNT, '건'.
      ENDAT.

    ENDLOOP.
    CLEAR IT_TAB.

ENDFORM.                    " P2000_WRITE_IV_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_SELECT_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_SELECT_RECORD USING    P_SY_UCOMM.

DATA : WL_MARK.

   IF P_SY_UCOMM EQ 'MKAL'.
      WL_MARK = 'X'.
   ELSEIF P_SY_UCOMM EQ 'MKLO'.
      CLEAR : WL_MARK.
   ENDIF.

   DO.
      CLEAR MARKFIELD.
      READ LINE SY-INDEX FIELD VALUE MARKFIELD.
      IF SY-SUBRC NE 0.
         EXIT.
      ENDIF.
      MODIFY CURRENT LINE FIELD VALUE MARKFIELD FROM WL_MARK.
   ENDDO.

ENDFORM.                    " P3000_SELECT_RECORD

*&---------------------------------------------------------------------*
*&      Form  P3000_SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_SAVE_DATA.
DATA : L_LINE   TYPE    I.

  CLEAR  W_PROC_CNT.
  CLEAR  W_DATA_CNT.

  REFRESH : IT_VTIV, IT_VTIVIT.

  SET UPDATE TASK LOCAL.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.

    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING

    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).
         W_PROC_CNT  =  W_PROC_CNT  +  1.

         READ TABLE IT_IV WITH KEY ZFGFDYR  =  IT_TAB-ZFGFDYR
                                   ZFGFDNO  =  IT_TAB-ZFGFDNO
                                   BUKRS     = IT_TAB-BUKRS.
         IF SY-SUBRC  EQ   0.
            W_TABIX = SY-TABIX.
            IF  IT_IV-DATA_YN  =  'Y'.
                CONTINUE.
            ENDIF.
* 후속 작업 여부에 따라서 DELETE 여부 결정.
            IF  IT_IV-CHK_YN  =  'Y'.
*                DELETE  FROM   ZTVTIV
*                        WHERE  ZFGFDYR   =  IT_IV-ZFGFDYR
*                        AND    ZFGFDNO   =  IT_IV-ZFGFDNO
*                        AND    BUKRS     =  IT_IV-BUKRS.

                DELETE  FROM   ZTVTIVIT
                        WHERE  ZFGFDYR   =  IT_IV-ZFGFDYR
                        AND    ZFGFDNO   =  IT_IV-ZFGFDNO
                        AND    BUKRS     =  IT_IV-BUKRS.
                IF SY-SUBRC NE 0.
                   ROLLBACK WORK.
                   MESSAGE E953(ZIM1) WITH '아이템'.
                ENDIF.
            ENDIF.
            IT_IV-MANDT   = SY-MANDT.
            MOVE-CORRESPONDING IT_IV TO IT_VTIV.
            APPEND IT_VTIV.

         ELSE.
             CONTINUE.
         ENDIF.

         MODIFY IT_IV  INDEX  W_TABIX.

         L_LINE = 0.
         LOOP  AT  IT_IVIT  WHERE    ZFGFDYR  =  IT_IV-ZFGFDYR
                            AND      ZFGFDNO  =  IT_IV-ZFGFDNO
                            AND      BUKRS    =  IT_IV-BUKRS.
            W_TABIX = SY-TABIX.
*            PERFORM   P3000_MOVE_DATA.
            ADD 1 TO L_LINE.
            IT_IVIT-ZFIVDNO = L_LINE * 10.
            IT_IVIT-MANDT   = SY-MANDT.
            MODIFY IT_IVIT  INDEX  W_TABIX.

            MOVE-CORRESPONDING IT_IVIT TO IT_VTIVIT.
            APPEND IT_VTIVIT.
*            ZTVTIVIT-ZFIVDNO = W_TABIX * 10.
*           INSERT  ZTVTIVIT.
         ENDLOOP.

*         INSERT  ZTVTIV.
         W_DATA_CNT  =  W_DATA_CNT  +  1.
    ENDIF.
  ENDDO.

  MODIFY  ZTVTIV     FROM  TABLE IT_VTIV.
  IF SY-SUBRC NE 0.
     ROLLBACK WORK.
     MESSAGE E952(ZIM1) WITH '헤더'.
  ENDIF.

  MODIFY  ZTVTIVIT   FROM  TABLE IT_VTIVIT.
  IF SY-SUBRC NE 0.
     ROLLBACK WORK.
     MESSAGE E952(ZIM1) WITH '아이템'.
  ENDIF.

  COMMIT WORK.

  LOOP AT IT_VTIV.
     READ TABLE IT_IV WITH KEY  ZFGFDYR  =  IT_VTIV-ZFGFDYR
                                ZFGFDNO  =  IT_VTIV-ZFGFDNO
                                BUKRS    =  IT_VTIV-BUKRS.
     W_TABIX = SY-TABIX.
     IF SY-SUBRC EQ 0.
        IT_IV-CHK_YN = 'Y'.
        MODIFY IT_IV INDEX W_TABIX.
     ENDIF.
  ENDLOOP.


ENDFORM.                    " P3000_SAVE_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_DEL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_DEL_DATA.

  CLEAR  W_PROC_CNT.
  CLEAR  W_DATA_CNT.

  SET UPDATE TASK LOCAL.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).

         W_PROC_CNT  =  W_PROC_CNT  +  1.

         READ TABLE IT_IV WITH KEY ZFGFDYR  =  IT_TAB-ZFGFDYR
                                   ZFGFDNO  =  IT_TAB-ZFGFDNO
                                   BUKRS    =  IT_TAB-BUKRS.
         W_TABIX = SY-TABIX.
         IF SY-SUBRC  EQ   0.
* 후속작업이 있는 경우는 삭제불가.
            IF  IT_IV-DATA_YN =  'Y'.
                CONTINUE.
            ENDIF.
            IF  IT_IV-CHK_YN  =  'Y'.
                DELETE  FROM   ZTVTIV
                        WHERE  ZFGFDYR   =  IT_IV-ZFGFDYR
                        AND    ZFGFDNO   =  IT_IV-ZFGFDNO
                        AND    BUKRS     =  IT_IV-BUKRS.
                IF SY-SUBRC NE 0.
                   ROLLBACK WORK.
                   MESSAGE E953(ZIM1) WITH '헤더'.
                ENDIF.

                DELETE  FROM   ZTVTIVIT
                        WHERE  ZFGFDYR   =  IT_IV-ZFGFDYR
                        AND    ZFGFDNO   =  IT_IV-ZFGFDNO
                        AND    BUKRS     =  IT_IV-BUKRS.
                IF SY-SUBRC NE 0.
                   ROLLBACK WORK.
                   MESSAGE E953(ZIM1) WITH '아이템'.
                ENDIF.
                W_DATA_CNT  =  W_DATA_CNT  +  1.

                IT_IV-CHK_YN = 'N'.
                MODIFY IT_IV INDEX W_TABIX.
            ELSE.
                CONTINUE.
            ENDIF.
         ENDIF.
    ENDIF.
  ENDDO.

  COMMIT WORK.

ENDFORM.                    " P3000_DEL_DATA

*&---------------------------------------------------------------------*
*&      Form  P3000_MOVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P3000_MOVE_DATA.

  CLEAR ZTVTIV.
  CLEAR ZTVTIVIT.

* ZTVTIV TABLE 로 DATA MOVE.
  MOVE  IT_IV-BUKRS      TO   ZTVTIV-BUKRS.
  MOVE  IT_IV-ZFGFDYR    TO   ZTVTIV-ZFGFDYR.
  MOVE  IT_IV-ZFGFDNO    TO   ZTVTIV-ZFGFDNO.
  MOVE  IT_IV-BUKRS      TO   ZTVTIV-BUKRS.
  MOVE  IT_IV-ZFREDNO    TO   ZTVTIV-ZFREDNO.
  MOVE  IT_IV-ZFVTNO     TO   ZTVTIV-ZFVTNO.
  MOVE  IT_IV-LIFNR      TO   ZTVTIV-LIFNR.
  MOVE  IT_IV-ZFPOSDT    TO   ZTVTIV-ZFPOSDT.
  MOVE  IT_IV-ZFDODT     TO   ZTVTIV-ZFDODT.
  MOVE  IT_IV-ZFEXRT     TO   ZTVTIV-ZFEXRT.
  MOVE  IT_IV-ERNAM      TO   ZTVTIV-ERNAM.
  MOVE  IT_IV-CDAT       TO   ZTVTIV-CDAT.
  MOVE  IT_IV-UNAM       TO   ZTVTIV-UNAM.
  MOVE  IT_IV-UDAT       TO   ZTVTIV-UDAT.

* ZTVTIVIT TABLE 로 DATA MOVE.
  MOVE  SY-MANDT         TO   ZTVTIVIT-MANDT.
  MOVE  IT_IVIT-ZFWERKS  TO   ZTVTIVIT-ZFWERKS.
  MOVE  IT_IVIT-ZFREQTY  TO   ZTVTIVIT-ZFREQTY.
  MOVE  IT_IVIT-EBELN    TO   ZTVTIVIT-EBELN.
  MOVE  IT_IVIT-EBELP    TO   ZTVTIVIT-EBELP.
  MOVE  IT_IVIT-ZFGFDYR  TO   ZTVTIVIT-ZFGFDYR.
  MOVE  IT_IVIT-ZFGFDNO  TO   ZTVTIVIT-ZFGFDNO.
  MOVE  IT_IVIT-BUKRS    TO   ZTVTIVIT-BUKRS.
  MOVE  IT_IVIT-ZFIVDNO  TO   ZTVTIVIT-ZFIVDNO.
  MOVE  IT_IVIT-MATNR    TO   ZTVTIVIT-MATNR.
  MOVE  IT_IVIT-ZFQUN    TO   ZTVTIVIT-ZFQUN.
  MOVE  IT_IVIT-ZFQUNM   TO   ZTVTIVIT-ZFQUNM.
  MOVE  IT_IVIT-NETPR    TO   ZTVTIVIT-NETPR.
  MOVE  IT_IVIT-PEINH    TO   ZTVTIVIT-PEINH.
  MOVE  IT_IVIT-BPRME    TO   ZTVTIVIT-BPRME.
  MOVE  IT_IVIT-ZFIVAMT  TO   ZTVTIVIT-ZFIVAMT.
  MOVE  IT_IVIT-ZFIVAMC  TO   ZTVTIVIT-ZFIVAMC.
  MOVE  IT_IVIT-ZFKAMT   TO   ZTVTIVIT-ZFKAMT.
  MOVE  IT_IVIT-ZFKRW    TO   ZTVTIVIT-ZFKRW.

ENDFORM.                   " P3000_MOVE_DATA

*&---------------------------------------------------------------------*
*&      Form  P4000_MODIFY_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P4000_MODIFY_LINE USING    P_SY_UCOMM.

  DO.
    CLEAR MARKFIELD.
    READ LINE SY-INDEX FIELD VALUE MARKFIELD.
    IF SY-SUBRC NE 0.   EXIT.   ENDIF.        " EXIT CHECKING
    IF ( MARKFIELD EQ 'x' ) OR ( MARKFIELD EQ 'X' ).

       IF P_SY_UCOMM  =  'REFL'.
          MODIFY CURRENT LINE FIELD VALUE IT_TAB-CHK_YN FROM '기집계'.
       ELSE.
          MODIFY CURRENT LINE FIELD VALUE IT_TAB-CHK_YN FROM '      '.
       ENDIF.

    ENDIF.

  ENDDO.

ENDFORM.                    " P4000_MODIFY_LINE
*&---------------------------------------------------------------------*
*&      Form  P1000_INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_INIT_DATA.

*>> Title Bar Set
  SET TITLEBAR 'ZIMA0N'.

  GET PARAMETER  ID 'BUK' FIELD P_BUKRS.

*>> Posting Date Set!
  CONCATENATE SY-DATUM(6) '01' INTO S_BUDAT-LOW.
  S_BUDAT-HIGH = SY-DATUM.
  APPEND S_BUDAT.

  SELECT SINGLE * FROM ZTIMIMG00.
  IF SY-SUBRC NE 0.
     MESSAGE S963.
     LEAVE PROGRAM.
  ENDIF.

ENDFORM.                    " P1000_INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_STANDARD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_STANDARD_DATA.

   CLEAR : W_LOOP_CNT.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_BKPF
   FROM BKPF
   WHERE BUKRS  EQ P_BUKRS       ">회사코드.
   AND   BLART  EQ P_BLART       ">문서종류
   AND   BUDAT  IN S_BUDAT       ">Posting Date
   AND   BLDAT  IN S_BLDAT       ">Document Date
   AND ( STBLG  EQ SPACE         ">역분개전표번호.
   OR    STBLG  IS NULL )
   AND   GJAHR  BETWEEN  P_YEAR1  AND  P_YEAR2
   AND   BELNR  BETWEEN  P_NO1    AND  P_NO2  .

   IF SY-SUBRC NE 0.
      MESSAGE S738.
      EXIT.
   ENDIF.

* BSEG TABLE DATA READ.
    SELECT  *
    INTO    CORRESPONDING FIELDS OF TABLE IT_BSEG
    FROM    BSEG
    FOR ALL ENTRIES  IN       IT_BKPF
    WHERE   BUKRS    EQ       IT_BKPF-BUKRS
    AND     BELNR    EQ       IT_BKPF-BELNR
    AND     GJAHR    EQ       IT_BKPF-GJAHR
    AND ( ( BSCHL    EQ       '31'
    AND     AUGBL    EQ       SPACE      )
    OR    ( BSCHL    EQ       '86'
    AND     EBELN    IN       S_EBELN    ) ).

ENDFORM.                    " P1000_READ_STANDARD_DATA
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_CONTRACT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_READ_CONTRACT.
*>> 2001.03.29 KSB INSERT
DATA : L_DATA_CHK,
       L_SUBRC      LIKE   SY-SUBRC,
       L_AWKEY      LIKE   BKPF-AWKEY,
       L_BELNR      LIKE   RBKP-BELNR,
       L_GJAHR      LIKE   RBKP-GJAHR.

     REFRESH  IT_IV.
     REFRESH  IT_IVIT.
     REFRESH  IT_TAB.
     W_LOOP_CNT  =  0.

   LOOP AT IT_BKPF.

      CLEAR  IT_IV.

*----------------------------------------------------
*----> 송장 취소문서 미반영토록 수정.
*----> 2001.12.05 KSB MODIFY...
*----------------------------------------------------
      MOVE : IT_BKPF-AWKEY(10)   TO L_BELNR,
             IT_BKPF-AWKEY+10(4) TO L_GJAHR.


      SELECT SINGLE * FROM RBKP
             WHERE    BELNR  EQ  L_BELNR
             AND      GJAHR  EQ  L_GJAHR.
      IF SY-SUBRC NE 0.
         CONTINUE.
      ELSE.
         IF NOT RBKP-STBLG IS INITIAL.
            CONTINUE.
         ENDIF.
      ENDIF.
*----------------------------------------------------


      MOVE  IT_BKPF-GJAHR   TO  IT_IV-ZFGFDYR.
      MOVE  IT_BKPF-BELNR   TO  IT_IV-ZFGFDNO.
      MOVE  IT_BKPF-BUKRS   TO  IT_IV-BUKRS.
      MOVE  IT_BKPF-BUDAT   TO  IT_IV-ZFPOSDT.
      MOVE  IT_BKPF-BLDAT   TO  IT_IV-ZFDODT.
      MOVE  IT_BKPF-KURSF   TO  IT_IV-ZFEXRT.
      MOVE  '         '     TO  IT_IV-ZFVTNO.
      MOVE  '         '     TO  IT_IV-ZFREDNO.
      MOVE  SY-UNAME        TO  IT_IV-ERNAM.
      MOVE  SY-DATUM        TO  IT_IV-CDAT.
      MOVE  SY-UNAME        TO  IT_IV-UNAM.
      MOVE  SY-DATUM        TO  IT_IV-UDAT.



      READ TABLE   IT_BSEG WITH KEY BUKRS = IT_BKPF-BUKRS
                                    BELNR = IT_BKPF-BELNR
                                    GJAHR = IT_BKPF-GJAHR
                                    BSCHL = '31'        .
      IF SY-SUBRC  EQ   0 .
         MOVE IT_BSEG-LIFNR         TO IT_IV-LIFNR.
      ELSE.
         CONTINUE.
      ENDIF.

*>> 후속작업 진행 여부 확인(세금계사서 편성 여부).
      CLEAR  W_VTNO.
      SELECT SINGLE  ZFVTNO
      INTO   W_VTNO
      FROM   ZTVTIV
      WHERE  ZFGFDYR = IT_BKPF-GJAHR
      AND    ZFGFDNO = IT_BKPF-BELNR
      AND    BUKRS   = IT_BKPF-BUKRS.
      L_SUBRC = SY-SUBRC.

      IF L_SUBRC  =  0  AND  NOT W_VTNO IS INITIAL.
         CONTINUE.
      ENDIF.
*>> 2001.03.29 KSB INSERT
      L_DATA_CHK = 'N'.
      LOOP AT IT_BSEG WHERE  BUKRS = IT_BKPF-BUKRS
                      AND    BELNR = IT_BKPF-BELNR
                      AND    GJAHR = IT_BKPF-GJAHR
                      AND    BSCHL = '86'
                      AND    BUZID = 'W'.

         CLEAR IT_IVIT.
*> 구매오더 헤더 SELECT.
         SELECT SINGLE * FROM EKKO
                WHERE EBELN EQ IT_BSEG-EBELN.
         IF SY-SUBRC NE 0.
            CONTINUE.
         ENDIF.

*---------------------------------------------------------------------
*> 일괄계약일 경우.
*---------------------------------------------------------------------
         IF EKKO-BSTYP EQ 'K'.
* 릴리즈 PO 발췌..
            SELECT SINGLE * INTO XEKAB
                   FROM  EKAB
                   WHERE EBELN EQ IT_BSEG-EBELN
                   AND   EBELP EQ IT_BSEG-EBELP.

            IF SY-SUBRC NE 0.
               CONTINUE.
            ENDIF.

            SELECT * INTO  TABLE IT_ZTREQIT
                     FROM  ZTREQIT
                     WHERE EBELN EQ XEKAB-KONNR
                     AND   EBELP EQ XEKAB-KTPNR.
            IF SY-SUBRC NE 0.
               CONTINUE.
            ENDIF.
*---------------------------------------------------------------------
*> 구매오더 및 납품일정계약일 경우.
*---------------------------------------------------------------------
         ELSEIF EKKO-BSTYP EQ 'F' OR
                EKKO-BSTYP EQ 'L'.
            SELECT * INTO  TABLE IT_ZTREQIT
                     FROM  ZTREQIT
                     WHERE EBELN EQ IT_BSEG-EBELN
                     AND   EBELP EQ IT_BSEG-EBELP.

*-----------------------------------------------------
*> 계약을 참조한 경우인가 검증.
*-----------------------------------------------------
            IF SY-SUBRC NE 0.
               SELECT SINGLE * INTO XEKAB
                      FROM  EKAB
                      WHERE EBELN EQ IT_BSEG-EBELN
                      AND   EBELP EQ IT_BSEG-EBELP.
               IF SY-SUBRC NE 0.
                  CONTINUE.
               ENDIF.
               SELECT * INTO  TABLE IT_ZTREQIT
                        FROM  ZTREQIT
                        WHERE EBELN EQ XEKAB-KONNR
                        AND   EBELP EQ XEKAB-KTPNR.
               IF SY-SUBRC NE 0.
                  CONTINUE.
               ENDIF.
            ENDIF.
*-----------------------------------------------------

         ELSE.
            CONTINUE.
         ENDIF.

*-------------------------------------------------------------------
*> 자재 아이템 조회.
*-------------------------------------------------------------------
         LOOP AT IT_ZTREQIT.
            CLEAR : IT_IVIT-ZFIVAMT, IT_IVIT-ZFKAMT.
            SELECT SINGLE * FROM ZTREQHD
                   WHERE  ZFREQNO EQ IT_ZTREQIT-ZFREQNO
                   AND    ZFOPNNO IN S_OPNNO
                   AND    ZFREQTY EQ 'LO'.
            IF SY-SUBRC  NE  0.
               CONTINUE.
            ENDIF.
*---------------------------------------------------
*> 문서상태 체크 ( 2001/10/08 KSB INSERT )
            SELECT SINGLE * FROM ZTREQST
                   WHERE    ZFREQNO EQ ZTREQHD-ZFREQNO
                   AND      ZFAMDNO EQ
                          ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                            WHERE ZFREQNO EQ ZTREQHD-ZFREQNO ).

            IF ZTREQST-ZFDOCST NE 'O'.
               CONTINUE.
            ENDIF.
*---------------------------------------------------
            MOVE IT_BSEG-WERKS       TO IT_IVIT-ZFWERKS.
            MOVE W_ZFREQNO           TO IT_IVIT-ZFREQNO.
            MOVE IT_BSEG-EBELN       TO IT_IVIT-EBELN.
            MOVE IT_BSEG-EBELP       TO IT_IVIT-EBELP.
            MOVE ZTREQHD-ZFREQTY     TO IT_IVIT-ZFREQTY.
            MOVE IT_ZTREQIT-ZFITMNO  TO IT_IVIT-ZFITMNO.
            MOVE IT_BSEG-GJAHR       TO IT_IVIT-ZFGFDYR.
            MOVE IT_BSEG-BELNR       TO IT_IVIT-ZFGFDNO.
            MOVE IT_BSEG-EBELP       TO IT_IVIT-ZFIVDNO.
            MOVE IT_BSEG-BUKRS       TO IT_IVIT-BUKRS.
            MOVE IT_BSEG-MENGE       TO IT_IVIT-ZFQUN.
            MOVE IT_BSEG-MEINS       TO IT_IVIT-ZFQUNM.

            SELECT SINGLE * FROM   EKPO
                   WHERE  EBELN = IT_BSEG-EBELN
                   AND    EBELP = IT_BSEG-EBELP.

            MOVE EKPO-NETPR         TO IT_IVIT-NETPR.
            MOVE EKPO-PEINH         TO IT_IVIT-PEINH.
            MOVE IT_BSEG-MATNR      TO IT_IVIT-MATNR.
            MOVE IT_BKPF-WAERS      TO IT_IVIT-ZFIVAMC.
            MOVE IT_BSEG-BPRME      TO IT_IVIT-BPRME.
            MOVE 'KRW'              TO IT_IVIT-ZFKRW.

* 후속작업 여부 CHECK.
            W_COUNT  =  0.
            IF  L_SUBRC  =  0.
                IF  W_VTNO  IS  INITIAL.
                    IT_IV-DATA_YN  =  'N'.
                ELSE.
                    IT_IV-DATA_YN  =  'Y'.
                ENDIF.
            ELSE.
                IT_IV-DATA_YN  =  'N'.
            ENDIF.

* 재작업 여부로 인한  DATA의 INTERNAL TABLE INSERT 여부 결정.
            SELECT COUNT( DISTINCT ZFIVDNO )
            INTO   W_COUNT
            FROM   ZTVTIVIT
            WHERE  ZFGFDYR = IT_BSEG-GJAHR
            AND    ZFGFDNO = IT_BSEG-BELNR
            AND    ZFIVDNO = IT_BSEG-EBELP.

            IF  W_COUNT  >  0.
                IF P_YN  NE  'X'.
                   CONTINUE.
                ELSE.
                   IT_IV-CHK_YN    = 'Y'.
                ENDIF.
            ENDIF.

* PO의 ITEM 별로 금액을 SUM.
            SELECT  *
            FROM    BSEG
            WHERE   BUKRS   =  IT_BSEG-BUKRS
            AND     BELNR   =  IT_BSEG-BELNR
            AND     GJAHR   =  IT_BSEG-GJAHR
            AND     BUZEI   =  IT_BSEG-BUZEI
            AND     EBELN   =  IT_BSEG-EBELN
            AND     EBELP   =  IT_BSEG-EBELP.

               IF  BSEG-SHKZG   EQ  'S'.
                   IT_IVIT-ZFIVAMT  =  IT_IVIT-ZFIVAMT  +   BSEG-WRBTR.
                   IT_IVIT-ZFKAMT   =  IT_IVIT-ZFKAMT   +   BSEG-DMBTR.
               ELSE.
                  IT_IVIT-ZFIVAMT  =  IT_IVIT-ZFIVAMT  -   BSEG-WRBTR.
                  IT_IVIT-ZFKAMT   =  IT_IVIT-ZFKAMT   -   BSEG-DMBTR.
              ENDIF.

         ENDSELECT.

         MOVE ZTREQHD-ZFREQTY TO IT_IVIT-ZFREQTY.

*>> 2001.03.29 KSB INSERT
         L_DATA_CHK = 'Y'.
         APPEND IT_IVIT.
         W_LOOP_CNT  =  W_LOOP_CNT  +   1.

       ENDLOOP.
*>> 2001.03.29 KSB INSERT
       IF L_DATA_CHK = 'Y'.
          APPEND IT_IV.
       ENDIF.
      ENDLOOP.

   ENDLOOP.


ENDFORM.                    " P1000_READ_CONTRACT
