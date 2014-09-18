*&---------------------------------------------------------------------*
*& Report  ZRIMBSTLST                                                  *
*&---------------------------------------------------------------------*
*&  프로그램명 : 보세창고 재고현황                                     *
*&      작성자 : 나현주                                                *
*&      작성일 : 2002.11.08                                            *
*&     적용회사: 한수원                                                *
*&---------------------------------------------------------------------*
*&   DESC.     :
*&
*&---------------------------------------------------------------------*
*& [변경내용]
*&---------------------------------------------------------------------*
REPORT  ZRIMBSTLST   MESSAGE-ID ZIM
*                     LINE-SIZE 93
                     NO STANDARD PAGE HEADING.

TABLES: ZTBLINR,
        ZTBLOUR,
        ZTBL,
        ZTBLIT,
        ZTIVIT,
        ZTBLINR_TMP,
        ZTTRHD,
        ZTTRIT,
        ZTIMIMG03.

DATA: CURSORFIELD(20).

DATA: LS_DAY_ATTRIBUTES      LIKE CASDAYATTR OCCURS 0
                                  WITH HEADER LINE.

*----------------------------------------------------------------------*
* 인터널테이블.
*----------------------------------------------------------------------*
DATA : BEGIN OF IT_INR OCCURS 0,
       COUNT   TYPE I,
       ZFINDT  LIKE ZTBLINR_TMP-ZFINDT,
       MATNR   LIKE ZTBLIT-MATNR,
       INMENGE LIKE ZTBLIT-BLMENGE,
       MEINS   LIKE ZTBLIT-MEINS.
DATA : END OF IT_INR.

DATA : BEGIN OF IT_OUR OCCURS 0,
       COUNT   TYPE I,
       ZFOUDT  LIKE ZTBLOUR-ZFOTDT,
       MATNR   LIKE ZTBLIT-MATNR,
       OUMENGE LIKE ZTBLIT-BLMENGE,
       MEINS   LIKE ZTBLIT-MEINS.
DATA : END OF IT_OUR.

DATA : BEGIN OF IT_CC OCCURS 0,
       COUNT   TYPE I,
       ZFCCDT  LIKE ZTIDS-ZFIDSDT,
       MATNR   LIKE ZTIVIT-MATNR,
       CCMENGE LIKE ZTIVIT-CCMENGE,
       MEINS   LIKE ZTIVIT-MEINS.
DATA : END OF IT_CC.

DATA : COUNT1     TYPE I,                  " 반입.
       COUNT1_TMP TYPE I,                  " 미참조반입
       ZFINWT     LIKE ZTBLINR-ZFINWT,     " 반입중량.
       ZFINWT_TMP LIKE ZTBLINR_TMP-ZFTOWT, " 미참조반입중량.
       COUNT2     TYPE I,                  " 반출.
       ZFOUWT     LIKE ZTBLOUR-ZFOUWT,     " 반출중량.
       COUNT3     TYPE I,                  " 재고.
       INVWT      LIKE ZTBLOUR-ZFOUWT,     " 재고중량.
       P_MATNR    LIKE ZTBLIT-MATNR,       " 자재.
       P_TXZ01    LIKE ZTBLIT-TXZ01,       " 자재내역.
       P_MEINS    LIKE ZTBLIT-MEINS.       " 자재단위.

DATA : BEGIN OF IT_TAB OCCURS 0,
       MATNR      LIKE ZTBLIT-MATNR,
       TXZ01      LIKE ZTBLIT-TXZ01,
       MEINS      LIKE ZTBLIT-MEINS,
       ZFOTDT     LIKE ZTBLOUR-ZFOTDT,
       IN_CNT     TYPE I,
       IN_MENGE   LIKE ZTBLIT-BLMENGE,
       CU_CNT     TYPE I,
       CU_MENGE   LIKE ZTBLIT-BLMENGE,
       OU_CNT     TYPE I,
       OU_MENGE   LIKE ZTBLIT-BLMENGE,
       NC_MENGE   LIKE ZTBLIT-BLMENGE,
       CC_MENGE   LIKE ZTBLIT-BLMENGE,
       TT_MENGE   LIKE ZTBLIT-BLMENGE.
DATA : END   OF IT_TAB.

DATA : BEGIN OF IT_ST OCCURS 0,
       MATNR      LIKE ZTBLIT-MATNR,
       TXZ01      LIKE ZTBLIT-TXZ01,
       MEINS      LIKE ZTBLIT-MEINS,
       ZFOTDT     LIKE ZTBLOUR-ZFOTDT,
       IN_CNT     TYPE I,
       IN_MENGE   LIKE ZTBLIT-BLMENGE,
       CU_CNT     TYPE I,
       CU_MENGE   LIKE ZTBLIT-BLMENGE,
       OU_CNT     TYPE I,
       OU_MENGE   LIKE ZTBLIT-BLMENGE,
       NC_MENGE   LIKE ZTBLIT-BLMENGE,
       CC_MENGE   LIKE ZTBLIT-BLMENGE,
       TT_MENGE   LIKE ZTBLIT-BLMENGE.
DATA : END   OF IT_ST.

DATA : BEGIN OF IT_INR_TMP  OCCURS 0,
       ZFREBELN   LIKE ZTBL-ZFREBELN,
       ZFSHNO     LIKE ZTBL-ZFSHNO,
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,
       ZFINRNO    LIKE ZTBLINR_TMP-ZFINRNO,
       BLMENGE    LIKE ZTBLIT-BLMENGE,
       MEINS      LIKE ZTBLIT-MEINS,
       ZFETA      LIKE ZTBLINR_TMP-ZFETA.
DATA : END   OF IT_INR_TMP.

DATA : BEGIN OF IT_CC_TMP  OCCURS 0,
       ZFREBELN   LIKE ZTBL-ZFREBELN,
       ZFSHNO     LIKE ZTBL-ZFSHNO,
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,
       ZFIDRNO    LIKE ZTIDS-ZFIDRNO,
       CCMENGE    LIKE ZTIVIT-CCMENGE,
       MEINS      LIKE ZTIVIT-MEINS,
       ZFETA      LIKE ZTBL-ZFETA.
DATA : END   OF IT_CC_TMP.

DATA : BEGIN OF IT_OUR_TMP  OCCURS 0,
       ZFREBELN   LIKE ZTBL-ZFREBELN,
       ZFSHNO     LIKE ZTBL-ZFSHNO,
       ZFHBLNO    LIKE ZTBL-ZFHBLNO,
       ZFGINO(18) TYPE C,
       GIMENGE    LIKE ZTTRIT-GIMENGE,
       MEINS      LIKE ZTIVIT-MEINS.
DATA : END   OF IT_OUR_TMP.

DATA:  S_LOC   LIKE ZTBLINR_TMP-ZFLOC,
       S_TOWT  LIKE ZTBLINR_TMP-ZFTOWT,
       S_COUNT TYPE I.

DATA: W_TABIX   LIKE SY-TABIX,
      W_LINE    TYPE I,
      W_CHK     TYPE C,
      W_ERR_CHK TYPE C,
      W_MOD     TYPE I,
      W_COUNT   TYPE I,
      W_IN      LIKE ZTBLIT-BLMENGE,
      W_OU      LIKE ZTBLIT-BLMENGE,
      W_CC      LIKE ZTBLIT-BLMENGE,
      W_TOT_NC  LIKE ZTBLIT-BLMENGE,
      W_TOT_CC  LIKE ZTBLIT-BLMENGE,
      W_TOT_ST  LIKE ZTBLIT-BLMENGE.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

PARAMETERS:     P_MATGB LIKE ZTBL-ZFMATGB OBLIGATORY.       " 자재구분.
SELECT-OPTIONS: S_OTDT  FOR ZTBLOUR-ZFOTDT NO-EXTENSION     " 기준일.
                                           OBLIGATORY.
SELECT-OPTIONS: S_MATNR FOR ZTBLIT-MATNR.                   " 자재.
PARAMETERS:     P_ABNAR LIKE ZTBLINR-ZFABNAR OBLIGATORY.    " 보세구역.
SELECTION-SCREEN END OF BLOCK B1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_ABNAR.
  PERFORM   P1000_CODE_HELP  USING  P_ABNAR  'P_ABNAR'.

* PARAMETER 초기값 Setting
INITIALIZATION.                          " 초기값 SETTING
  PERFORM   P2000_SET_PARAMETER.
* Title Text Write
TOP-OF-PAGE.
  PERFORM   P3000_TITLE_WRITE.                  " 해더 출력...

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

*  테이블 SELECT
  PERFORM   P1000_GET_DATA      USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'. MESSAGE S738.   EXIT.    ENDIF.

* 레포트 Write
  NEW-PAGE LINE-SIZE 179 NO-HEADING .

  PERFORM   P3000_DATA_WRITE       USING   W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.  EXIT.    ENDIF.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.
  CASE SY-UCOMM.

    WHEN 'DOWN'.          " FILE DOWNLOAD....
      PERFORM P3000_CREATE_DOWNLOAD_FILE.
*           PERFORM P3000_TO_PC_DOWNLOAD.
    WHEN 'INV_DETL'.
    WHEN OTHERS.
  ENDCASE.

*---------------------------------------------------------------------
* AT LINE-SELECTION.
*----------------------------------------------------------------------
AT LINE-SELECTION.
  IF IT_TAB-ZFOTDT IS INITIAL.
    MESSAGE S962.
  ELSE.
    GET CURSOR FIELD CURSORFIELD.
    IF SY-SUBRC EQ 0.
      CASE CURSORFIELD.
        WHEN 'IT_TAB-IN_CNT' OR 'IT_TAB-IN_MENGE'.

           IF IT_TAB-IN_CNT IS INITIAL.
              MESSAGE S962.EXIT.
           ENDIF.

           PERFORM P1000_READ_ZTBLINR USING IT_TAB-ZFOTDT
                                            IT_TAB-MATNR
                                            P_ABNAR.

        WHEN 'IT_TAB-CU_CNT' OR 'IT_TAB-CU_MENGE'.
           IF IT_TAB-CU_CNT IS INITIAL.
              MESSAGE S962.EXIT.
          ENDIF.
          PERFORM P1000_READ_DATE_ZTIDS   USING IT_TAB-ZFOTDT
                                                P_ABNAR
                                                IT_TAB-MATNR.

        WHEN 'IT_TAB-OU_CNT' OR 'IT_TAB-OU_MENGE'.

           IF IT_TAB-OU_CNT IS INITIAL.
              MESSAGE S962. EXIT.
           ENDIF.
           PERFORM P1000_READ_DATE_ZTBLOUR USING IT_TAB-ZFOTDT
                                                 IT_TAB-MATNR
                                                 P_ABNAR.

        WHEN OTHERS.
          MESSAGE S962.
      ENDCASE.
    ELSE.
      MESSAGE S962.
    ENDIF.

  ENDIF.
  CLEAR : IT_TAB.

*&---------------------------------------------------------------------*
*&      Form  P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
FORM P2000_SET_PARAMETER.

  SET  TITLEBAR 'ZIMR68'.          " TITLE BAR
  MOVE :    'I'          TO  S_OTDT-SIGN,
            'BT'         TO  S_OTDT-OPTION,
            SY-DATUM     TO  S_OTDT-HIGH.
  CONCATENATE SY-DATUM(6) '01' INTO S_OTDT-LOW.
  APPEND S_OTDT.

ENDFORM.                    " P2000_SET_PARAMETER
*&---------------------------------------------------------------------*
*&      Form  P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_TITLE_WRITE.
  SKIP 2.
  WRITE : /80 '[ 보세창고재고현황 ]'.
  SKIP 1.
  WRITE : /3 '조회기간',S_OTDT-LOW,'~',S_OTDT-HIGH.
  WRITE:/3 '장치장 부호',P_ABNAR.

  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : / SY-ULINE.
  WRITE : / SY-VLINE NO-GAP,
         (40) '자재     ' NO-GAP CENTERED
                          COLOR COL_NORMAL INTENSIFIED OFF,
                                           SY-VLINE NO-GAP,
         (10) '         ' NO-GAP COLOR COL_NORMAL INTENSIFIED OFF,
                                           SY-VLINE NO-GAP,
         (23) '반입'  NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (23) '통관'  NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (23) '반출'  NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (53) '재고'  NO-GAP CENTERED,   SY-VLINE NO-GAP.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE :/ SY-VLINE   NO-GAP,
          (40) '   '  NO-GAP,            SY-VLINE NO-GAP,
          (10) '   '  NO-GAP,            SY-VLINE NO-GAP,
          53 SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,
         (40) '         ' NO-GAP CENTERED
                          COLOR COL_NORMAL INTENSIFIED OFF,
                                           SY-VLINE NO-GAP,
         (10) '         ' NO-GAP COLOR COL_NORMAL INTENSIFIED OFF,
                                           SY-VLINE NO-GAP,
         (05) '건수'   NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (17) '수량'   NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (05) '건수'   NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (17) '수량'   NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (05) '건수'   NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (17) '수량'   NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (17) '미통관' NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (17) '통관'   NO-GAP CENTERED,   SY-VLINE NO-GAP,
         (17) '합계'   NO-GAP CENTERED,   SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING      W_ERR_CHK.

  CLEAR : P_MATNR, W_IN, W_OU, W_CC, W_TOT_NC, W_TOT_CC, W_TOT_ST.

  SET PF-STATUS 'ZIMR68'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMR68'.           " GUI TITLE SETTING..

  SORT  IT_TAB  BY  MATNR  ZFOTDT.

  LOOP AT IT_TAB.
    W_TABIX = SY-TABIX.
    W_MOD = SY-TABIX MOD 2.
    IF IT_TAB-MATNR  NE  P_MATNR.

       W_CHK  =  'Y'.
       " 자재명 GET!
       SELECT SINGLE MAKTX  INTO  P_TXZ01
       FROM   MAKT
       WHERE  SPRAS         EQ    SY-LANGU
       AND    MATNR         EQ    IT_TAB-MATNR.

       IF SY-TABIX NE 1.
          PERFORM  P3000_TOTAL_WRITE.
          CLEAR : W_IN, W_OU, W_CC, W_TOT_NC, W_TOT_CC, W_TOT_ST.
       ENDIF.
    ELSE.
       CLEAR : W_CHK.
    ENDIF.

    PERFORM P3000_LINE_WRITE.
    P_MATNR  =  IT_TAB-MATNR.
    P_MEINS  =  IT_TAB-MEINS.

    " 자재별 TOTAL SUM.
    ADD  :  IT_TAB-IN_MENGE    TO   W_IN,
            IT_TAB-OU_MENGE    TO   W_OU,
            IT_TAB-CU_MENGE    TO   W_CC.

    AT LAST.
*      PERFORM P3000_LAST_WRITE.
      PERFORM P3000_TOTAL_WRITE.
    ENDAT.

  ENDLOOP.
  CLEAR: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LAST_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LAST_WRITE.

  WRITE:/ SY-ULINE.

ENDFORM.                    " P3000_LAST_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  IF W_CHK EQ 'Y'.
     WRITE : / SY-VLINE NO-GAP,
            (18) IT_TAB-MATNR NO-GAP,
            (1)  ' '          NO-GAP,
            (21) P_TXZ01      NO-GAP,  SY-VLINE NO-GAP.
   ELSE.
      WRITE : / SY-VLINE NO-GAP,
            (40) '        '   NO-GAP,  SY-VLINE NO-GAP.
   ENDIF.

  IF W_CHK = 'Y'.
    WRITE : (10) '전월이월'   NO-GAP,  SY-VLINE NO-GAP.
  ELSE.
    WRITE : (10) IT_TAB-ZFOTDT NO-GAP, SY-VLINE NO-GAP.
  ENDIF.
  WRITE : (05) IT_TAB-IN_CNT   NO-GAP, SY-VLINE  NO-GAP,
          (17) IT_TAB-IN_MENGE UNIT IT_TAB-MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (05) IT_TAB-CU_CNT   NO-GAP, SY-VLINE  NO-GAP,
          (17) IT_TAB-CU_MENGE UNIT IT_TAB-MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (05) IT_TAB-OU_CNT   NO-GAP, SY-VLINE  NO-GAP,
          (17) IT_TAB-OU_MENGE UNIT IT_TAB-MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (17) IT_TAB-NC_MENGE UNIT IT_TAB-MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (17) IT_TAB-CC_MENGE UNIT IT_TAB-MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (17) IT_TAB-TT_MENGE UNIT IT_TAB-MEINS NO-GAP,
                                       SY-VLINE  NO-GAP.
  HIDE: IT_TAB, W_TABIX.

ENDFORM.                    " P3000_LINE_WRITE

*&---------------------------------------------------------------------*
*&      Form  P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
FORM P3000_CREATE_DOWNLOAD_FILE.

*  REFRESH IT_TAB_DOWN.
*  LOOP AT IT_TAB.
*    CLEAR IT_TAB_DOWN.
*    MOVE-CORRESPONDING IT_TAB TO IT_TAB_DOWN.
*    WRITE : IT_TAB-MENGE   UNIT     IT_TAB-MEINS TO IT_TAB_DOWN-MENGE,
*        IT_TAB-ZFUSDAM CURRENCY IT_TAB-ZFUSD TO IT_TAB_DOWN-ZFUSDAM,
*         IT_TAB-ZFOPAMT CURRENCY IT_TAB-WAERS TO IT_TAB_DOWN-ZFOPAMT.
*    APPEND IT_TAB_DOWN.
*  ENDLOOP.

ENDFORM.                    " P3000_CREATE_DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&  FORM P1000_READ_ZTBLINR USING    P_ZFOTDT.
*&---------------------------------------------------------------------*
FORM P1000_READ_ZTBLINR USING    P_ZFOTDT P_MATNR P_ZFABNAR .

  CLEAR : IT_TAB-TXZ01.

  SELECT SINGLE MAKTX   INTO  IT_TAB-TXZ01
  FROM   MAKT
  WHERE  SPRAS     EQ   SY-LANGU
  AND    MATNR     EQ   P_MATNR.

  PERFORM P3000_ZTBLINR_TITILE.

  SELECT A~ZFREBELN  A~ZFSHNO  A~ZFHBLNO  B~ZFINRNO
         C~BLMENGE   C~MEINS   B~ZFETA
  INTO   CORRESPONDING FIELDS OF TABLE IT_INR_TMP
  FROM ( ZTBL  AS  A  INNER JOIN  ZTBLINR_TMP AS B
  ON     A~ZFBLNO     EQ          B~ZFBLNO         )
  INNER  JOIN  ZTBLIT AS C
  ON     B~ZFBLNO     EQ          C~ZFBLNO
  WHERE  B~ZFINDT     EQ          P_ZFOTDT
  AND    B~ZFABNAR    EQ          P_ZFABNAR
  AND    C~MATNR      EQ          P_MATNR.

  LOOP  AT  IT_INR_TMP.
    PERFORM P3000_ZTBLINR_WRITE.
  ENDLOOP.

  WRITE : / SY-ULINE.

ENDFORM.                    " P1000_READ_ZTBLINR
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLINR_TITILE
*&---------------------------------------------------------------------*
FORM P3000_ZTBLINR_TITILE.
  NEW-PAGE LINE-SIZE 94 NO-HEADING .

  SKIP 2.
  WRITE : /40 '[반입상세내역]'.
  SKIP 1.
  WRITE :/ '반입일:',IT_TAB-ZFOTDT,
         / '자재  :',IT_TAB-MATNR ,  IT_TAB-TXZ01.
  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(13) 'P/O No'    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(24) 'HOUSE B/L' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(18) '반입번호'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '수량'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(05) '단위'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(10) '실입항일'  NO-GAP CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_ZTBLINR_TITILE
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLINR_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZTBLINR_WRITE.
  DATA : W_TEXT(15) TYPE C.

  SET PF-STATUS 'DTINR'.           " GUI STATUS SETTING
  SET  TITLEBAR 'DTINR' WITH 'Carry-in'. " GUI TITLE SETTING..

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  CONCATENATE IT_INR_TMP-ZFREBELN '-' IT_INR_TMP-ZFSHNO INTO W_TEXT.
  WRITE :/ SY-VLINE NO-GAP,(13) W_TEXT NO-GAP,
           SY-VLINE NO-GAP,(24) IT_INR_TMP-ZFHBLNO   NO-GAP,
           SY-VLINE NO-GAP,(18) IT_INR_TMP-ZFINRNO   NO-GAP,
           SY-VLINE NO-GAP,(17) IT_INR_TMP-BLMENGE
                           UNIT IT_INR_TMP-MEINS     NO-GAP,
           SY-VLINE NO-GAP,(05) IT_INR_TMP-MEINS     NO-GAP,
           SY-VLINE NO-GAP,(10) IT_INR_TMP-ZFETA     NO-GAP,
           SY-VLINE.

ENDFORM.                    " P3000_ZTBLINR_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATE_ZTBLOUR
*&---------------------------------------------------------------------*
FORM P1000_READ_DATE_ZTBLOUR USING    P_ZFOTDT P_MATNR P_ZFABNAR.

  PERFORM P3000_ZTBLOUR_TITILE.
  W_COUNT = 0.

  IF P_MATGB EQ '3'  OR  P_MATGB  EQ '4'  OR  P_MATGB EQ '5'.
     SELECT  A~ZFREBELN    A~ZFSHNO     A~ZFHBLNO
             B~ZFTRNO      AS  ZFGINO   B~GIMENGE   B~MEINS
     INTO    CORRESPONDING FIELDS OF TABLE IT_OUR_TMP
     FROM  ( ZTBL   AS  A  INNER  JOIN  ZTTRIT  AS  B
     ON      A~ZFBLNO      EQ           B~ZFBLNO       )
     INNER   JOIN  ZTTRHD  AS   C
     ON      B~ZFTRNO      EQ           C~ZFTRNO
     WHERE   C~ZFGIDT      EQ           P_ZFOTDT
     AND     B~MATNR       EQ           P_MATNR .
  ELSE.
     SELECT  A~ZFREBELN    A~ZFSHNO     A~ZFHBLNO
             B~ZFOURNO     AS  ZFGINO   C~BLMENGE  AS  GIMENGE  C~MEINS
     INTO    CORRESPONDING FIELDS OF TABLE IT_OUR_TMP
     FROM  ( ZTBL  AS  A   INNER  JOIN  ZTBLOUR  AS B
     ON      A~ZFBLNO      EQ          B~ZFBLNO       )
     INNER   JOIN   ZTBLIT AS          C
     ON      A~ZFBLNO      EQ          C~ZFBLNO
     WHERE   B~ZFABNAR     EQ          P_ZFABNAR
     AND     B~ZFOTDT      EQ          P_ZFOTDT
     AND     C~MATNR       EQ          P_MATNR.
  ENDIF.

  LOOP  AT  IT_OUR_TMP.

    W_COUNT = W_COUNT + 1.
    W_MOD = W_COUNT MOD 2.
    PERFORM P3000_ZTBLOUR_WRITE.

  ENDLOOP.

  WRITE : / SY-ULINE.

ENDFORM.                    " P1000_READ_DATE_ZTBLOUR
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLOUR_TITILE
*&---------------------------------------------------------------------*
FORM P3000_ZTBLOUR_TITILE.
  NEW-PAGE LINE-SIZE 83 NO-HEADING .

  SKIP 2.
  WRITE : /40 '[반출상세내역]'.
  SKIP 1.
  WRITE :/ '반출일:',IT_TAB-ZFOTDT,
         / '자재  :',IT_TAB-MATNR ,  IT_TAB-TXZ01.
  WRITE :/  SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(13) 'P/O No'        NO-GAP CENTERED,
            SY-VLINE NO-GAP,(24) 'House B/L No'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(18) '출고번호'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '수량'          NO-GAP CENTERED,
            SY-VLINE NO-GAP,(05) '단위'          NO-GAP CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_ZTBLOUR_TITILE
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTBLOUR_WRITE
*&---------------------------------------------------------------------*
FORM P3000_ZTBLOUR_WRITE.
  DATA : W_TEXT(15) TYPE C.

  SET PF-STATUS 'DTINR'.           " GUI STATUS SETTING
  SET  TITLEBAR 'DTINR' WITH 'Carry-out'. " GUI TITLE SETTING..

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  CONCATENATE IT_OUR_TMP-ZFREBELN '-' IT_OUR_TMP-ZFSHNO INTO W_TEXT.
  WRITE :/ SY-VLINE NO-GAP,(13) W_TEXT NO-GAP,
           SY-VLINE NO-GAP,(24) IT_OUR_TMP-ZFHBLNO  NO-GAP,
           SY-VLINE NO-GAP,(18) IT_OUR_TMP-ZFGINO   NO-GAP,
           SY-VLINE NO-GAP,(17) IT_OUR_TMP-GIMENGE
                             UNIT IT_OUR_TMP-MEINS  NO-GAP,
           SY-VLINE NO-GAP,(05) IT_OUR_TMP-MEINS    NO-GAP,
           SY-VLINE.

ENDFORM.                    " P3000_ZTBLOUR_WRITE

*&---------------------------------------------------------------------*
*&      Form  P1000_CODE_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_ABNAR  text
*----------------------------------------------------------------------*
FORM P1000_CODE_HELP USING    P_ABNAR P_FIELDNAME.
  DATA : L_DISPLAY.

  DATA: DYNPROG            LIKE SY-REPID,
        DYNNR              LIKE SY-DYNNR,
        WINDOW_TITLE(30)   TYPE C.
*>> 비용코드 HELP.
  DATA : BEGIN OF IT_COST_HELP OCCURS 0,
         ZFBNAR    LIKE ZTIMIMG03-ZFBNAR,
         ZFBNARM   LIKE ZTIMIMG03-ZFBNARM,
         END OF IT_COST_HELP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
             FROM   ZTIMIMG03.

  IF SY-SUBRC NE 0.
    MESSAGE S406.
    EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
*  W_FIELDNAME = 'ZTBSEG-ZFCD'.
*  W_FIELDNAME = P_FIELDNAME.
  WINDOW_TITLE = '보세창고 코드 Help'.
  CLEAR: L_DISPLAY.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
           EXPORTING
**                RETFIELD        = 'OTYPE'
                RETFIELD        = 'ZFBNAR'
                DYNPPROG        = DYNPROG
                DYNPNR          = DYNNR
                DYNPROFIELD     = P_FIELDNAME
                WINDOW_TITLE    = WINDOW_TITLE
                VALUE_ORG       = 'S'
*                DISPLAY         = L_DISPLAY
           TABLES
                VALUE_TAB       = IT_COST_HELP
           EXCEPTIONS
                PARAMETER_ERROR = 1
                NO_VALUES_FOUND = 2
                OTHERS          = 3.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_CODE_HELP
*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_GET_DATA  USING    W_ERR_CHK.

  DATA : FROM_DATE LIKE    SY-DATUM,
         TO_DATE   LIKE    SY-DATUM.
  CLEAR: W_COUNT, P_MATNR.

  W_ERR_CHK = 'N'.

  FROM_DATE = S_OTDT-LOW.
  TO_DATE   = S_OTDT-HIGH.

* GET DATE DATA
  CALL FUNCTION 'DAY_ATTRIBUTES_GET'
       EXPORTING
             DATE_FROM                  = FROM_DATE
             DATE_TO                    = TO_DATE
             LANGUAGE                   = SY-LANGU
       TABLES
             DAY_ATTRIBUTES             = LS_DAY_ATTRIBUTES
       EXCEPTIONS
             FACTORY_CALENDAR_NOT_FOUND = 1
             HOLIDAY_CALENDAR_NOT_FOUND = 2
             DATE_HAS_INVALID_FORMAT    = 3
             DATE_INCONSISTENCY         = 4
             OTHERS                     = 5.
  IF SY-SUBRC <> 0.
    W_ERR_CHK = 'N'.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    EXIT.
  ENDIF.

  REFRESH: IT_INR, IT_OUR, IT_TAB.

  " 반입 DATA GET.
  SELECT A~ZFINDT AS ZFINDT  B~MATNR AS MATNR  MAX( B~MEINS ) AS MEINS
         COUNT( * )       AS  COUNT
         SUM( B~BLMENGE ) AS  INMENGE
  INTO   CORRESPONDING FIELDS OF TABLE IT_INR
  FROM   ZTBLINR_TMP  AS  A   INNER  JOIN  ZTBLIT AS B
  ON     A~ZFBLNO         EQ  B~ZFBLNO
  WHERE  A~ZFABNAR        EQ  P_ABNAR
  AND    A~ZFMATGB        EQ  P_MATGB
  AND    B~MATNR          IN  S_MATNR
  AND    A~ZFINDT         IN  S_OTDT
  GROUP  BY
         B~MATNR  A~ZFINDT.

  " 발전자재, 기타인 경우는 수송처리.
  IF P_MATGB EQ '3' OR P_MATGB EQ '5'.
     " 출고 DATA GET.
     SELECT A~ZFGIDT AS ZFOUDT B~MATNR AS MATNR MAX( B~MEINS ) AS MEINS
            COUNT( * )       AS  COUNT
            SUM( B~GIMENGE ) AS  OUMENGE
     INTO   CORRESPONDING FIELDS OF TABLE IT_OUR
     FROM ( ZTTRHD  AS  A   INNER  JOIN  ZTTRIT AS B
     ON     A~ZFTRNO         EQ  B~ZFTRNO )
     INNER  JOIN  ZTBL  AS   C
     ON     B~ZFBLNO         EQ  C~ZFBLNO
     WHERE  C~ZFMATGB        EQ  P_MATGB
     AND    A~ZFGIDT         IN  S_OTDT
     AND    B~MATNR          IN  S_MATNR
     GROUP  BY
            B~MATNR  A~ZFGIDT.
  " 주기기, 보조기기, 원전연료는 수송 프로세스 제외.
  ELSE.
    " 반출 DATA GET.
    SELECT A~ZFOTDT AS ZFOUDT B~MATNR AS MATNR MAX( B~MEINS ) AS MEINS
           COUNT( * )       AS  COUNT
           SUM( B~BLMENGE ) AS  OUMENGE
    INTO   CORRESPONDING FIELDS OF TABLE IT_OUR
    FROM ( ZTBLOUR  AS  A   INNER  JOIN  ZTBLIT AS B
    ON     A~ZFBLNO         EQ  B~ZFBLNO )
    INNER  JOIN  ZTBL AS  C
    ON     B~ZFBLNO         EQ  C~ZFBLNO
    WHERE  A~ZFABNAR        EQ  P_ABNAR
    AND    C~ZFMATGB        EQ  P_MATGB
    AND    A~ZFOTDT         IN  S_OTDT
    AND    B~MATNR          IN  S_MATNR
    GROUP  BY
           B~MATNR  A~ZFOTDT.
  ENDIF.

  " 통관자료 GET
  SELECT A~ZFIDSDT AS ZFCCDT  B~MATNR AS MATNR  MAX( B~MEINS ) AS MEINS
         COUNT( * )        AS  COUNT
         SUM( B~CCMENGE )  AS  CCMENGE
  INTO   CORRESPONDING FIELDS  OF TABLE IT_CC
  FROM ( ( ZTIDS  AS  A  INNER  JOIN  ZTIVIT AS  B
  ON     A~ZFIVNO          EQ  B~ZFIVNO   )
  INNER  JOIN  ZTIMIMG03   AS  C
  ON     A~ZFBNARCD        EQ  C~ZFBNARCD )
  INNER  JOIN  ZTBL        AS  D
  ON     A~ZFBLNO          EQ  D~ZFBLNO
  WHERE  C~ZFBNAR          EQ  P_ABNAR
  AND    D~ZFMATGB         EQ  P_MATGB
  AND    B~MATNR           IN  S_MATNR
  AND    A~ZFIDSDT         IN  S_OTDT
  GROUP BY
         B~MATNR  A~ZFIDSDT.

  LOOP AT LS_DAY_ATTRIBUTES.

    CLEAR : IT_TAB.
    MOVE LS_DAY_ATTRIBUTES-DATE TO IT_TAB-ZFOTDT.

    " 반입정보 INSERT.
    LOOP  AT  IT_INR  WHERE ZFINDT  EQ  LS_DAY_ATTRIBUTES-DATE.
       CLEAR : IT_TAB.
       MOVE : LS_DAY_ATTRIBUTES-DATE  TO  IT_TAB-ZFOTDT,
              IT_INR-COUNT            TO  IT_TAB-IN_CNT,
              IT_INR-INMENGE          TO  IT_TAB-IN_MENGE,
              IT_INR-MATNR            TO  IT_TAB-MATNR,
              IT_INR-MEINS            TO  IT_TAB-MEINS.

       APPEND  IT_TAB.
    ENDLOOP.

    " 반출정보.
    LOOP  AT  IT_OUR  WHERE ZFOUDT EQ  LS_DAY_ATTRIBUTES-DATE.
       CLEAR : IT_TAB.
       MOVE : LS_DAY_ATTRIBUTES-DATE   TO  IT_TAB-ZFOTDT,
              IT_OUR-COUNT             TO  IT_TAB-OU_CNT,
              IT_OUR-OUMENGE           TO  IT_TAB-OU_MENGE,
              IT_OUR-MATNR             TO  IT_TAB-MATNR,
              IT_OUR-MEINS             TO  IT_TAB-MEINS.
       READ  TABLE  IT_INR  WITH KEY MATNR  = IT_OUR-MATNR
                                     ZFINDT = IT_OUR-ZFOUDT.
       IF SY-SUBRC EQ 0.
          W_TABIX  =  SY-TABIX.
          MODIFY  IT_TAB  INDEX  W_TABIX.
       ELSE.
          APPEND  IT_TAB.
       ENDIF.
    ENDLOOP.

    " 통관정보.
    LOOP  AT  IT_CC  WHERE ZFCCDT EQ  LS_DAY_ATTRIBUTES-DATE.
       CLEAR : IT_TAB.
       MOVE : LS_DAY_ATTRIBUTES-DATE   TO  IT_TAB-ZFOTDT,
              IT_CC-COUNT              TO  IT_TAB-CU_CNT,
              IT_CC-CCMENGE            TO  IT_TAB-CU_MENGE,
              IT_CC-MATNR              TO  IT_TAB-MATNR,
              IT_CC-MEINS              TO  IT_TAB-MEINS.
       READ  TABLE  IT_INR  WITH KEY MATNR  = IT_CC-MATNR
                                     ZFINDT = IT_CC-ZFCCDT.
       IF SY-SUBRC EQ 0.
          W_TABIX  =  SY-TABIX.
          MODIFY  IT_TAB  INDEX  W_TABIX.
       ELSE.
          READ  TABLE IT_OUR  WITH KEY MATNR  = IT_CC-MATNR
                                       ZFOUDT = IT_CC-ZFCCDT.
          IF SY-SUBRC EQ 0.
             W_TABIX  =  SY-TABIX.
             MODIFY  IT_TAB  INDEX  W_TABIX.
          ELSE.
             APPEND  IT_TAB.
          ENDIF.
       ENDIF.
    ENDLOOP.

  ENDLOOP.

  " 재고 정보.
  LOOP  AT  IT_TAB.
     W_TABIX  =  SY-TABIX.
     IT_TAB-NC_MENGE  =  IT_TAB-IN_MENGE  -  IT_TAB-CU_MENGE.
     IT_TAB-CC_MENGE  =  IT_TAB-CU_MENGE  -  IT_TAB-OU_MENGE.
     IT_TAB-TT_MENGE  =  IT_TAB-NC_MENGE  +  IT_TAB-CC_MENGE.
     MODIFY  IT_TAB  INDEX  W_TABIX.
  ENDLOOP.

  SORT  IT_TAB  BY  MATNR.

  " 이월재고 GET!
  LOOP  AT  IT_TAB.

     IF P_MATNR  NE  IT_TAB-MATNR.
        CLEAR : W_IN, W_OU, W_CC, W_TOT_NC, W_TOT_CC, W_TOT_ST,
                IT_TAB-TXZ01.
        W_TABIX  =  SY-TABIX.

        " FROM 전 까지의 반입량 SUM
        SELECT SUM( B~BLMENGE )  INTO  W_IN
        FROM   ZTBLINR_TMP  AS  A   INNER  JOIN  ZTBLIT AS B
        ON     A~ZFBLNO         EQ  B~ZFBLNO
        WHERE  A~ZFABNAR        EQ  P_ABNAR
        AND    B~MATNR          EQ  IT_TAB-MATNR
        AND    A~ZFINDT         LT  FROM_DATE.

        " FROM 전 까지의 반출량 SUM
        IF P_MATGB EQ '3' OR P_MATGB EQ '4' OR P_MATGB EQ '5'.
           " 출고 DATA GET.
           SELECT SUM( B~GIMENGE )  INTO  W_OU
           FROM   ZTTRHD  AS  A   INNER  JOIN  ZTTRIT AS B
           ON     A~ZFTRNO         EQ  B~ZFTRNO
           WHERE  B~MATNR          EQ  IT_TAB-MATNR
           AND    A~ZFGIDT         LT  FROM_DATE.
        ELSE.
           " 반출 DATA GET.
           SELECT SUM( B~BLMENGE )   INTO  W_OU
           FROM   ZTBLOUR  AS  A   INNER  JOIN  ZTBLIT AS B
           ON     A~ZFBLNO         EQ  B~ZFBLNO
           WHERE  A~ZFABNAR        EQ  P_ABNAR
           AND    A~ZFOTDT         LT  FROM_DATE
           AND    B~MATNR          EQ  IT_TAB-MATNR.
        ENDIF.

        " FROM 전 까지의 통관량 SUM
        SELECT SUM( B~CCMENGE )   INTO  W_CC
        FROM ( ZTIDS  AS  A  INNER  JOIN  ZTIVIT AS  B
        ON     A~ZFIVNO          EQ  B~ZFIVNO   )
        INNER  JOIN  ZTIMIMG03   AS  C
        ON     A~ZFBNARCD        EQ  C~ZFBNARCD
        WHERE  C~ZFBNAR          EQ  P_ABNAR
        AND    B~MATNR           EQ  IT_TAB-MATNR
        AND    A~ZFIDSDT         LT  FROM_DATE.

        " 미통관 이월재고량.
        W_TOT_NC  =  W_IN  -  W_CC.
        " 통관 이월재고량.
        W_TOT_CC  =  W_CC  -  W_OU.
        " 이월재고량.
        W_TOT_ST  =  W_TOT_NC  +  W_TOT_CC.

        MOVE : IT_TAB-MATNR    TO  IT_ST-MATNR,
               IT_TAB-MEINS    TO  IT_ST-MEINS,
               W_IN            TO  IT_ST-IN_MENGE,
               W_OU            TO  IT_ST-OU_MENGE,
               W_CC            TO  IT_ST-CU_MENGE,
               W_TOT_NC        TO  IT_ST-NC_MENGE,
               W_TOT_CC        TO  IT_ST-CC_MENGE,
               W_TOT_ST        TO  IT_ST-TT_MENGE.
        APPEND  IT_ST.
     ENDIF.
     MOVE   IT_TAB-MATNR  TO  P_MATNR.
  ENDLOOP.

  " 전월재고 -> INTERNAL TABLE APPEND.
  LOOP  AT  IT_ST.
     MOVE-CORRESPONDING  IT_ST  TO  IT_TAB.
     APPEND   IT_TAB.
  ENDLOOP.

ENDFORM.                     " P1000_GET_DATA
*&---------------------------------------------------------------------*
*&      Form  P3000_TOTAL_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_TOTAL_WRITE .

  " 합계 SUM
  W_TOT_NC  =  W_IN  -  W_CC.
  W_TOT_CC  =  W_CC  -  W_OU.
  W_TOT_ST  =  W_TOT_NC +  W_TOT_CC.

  FORMAT RESET.

  WRITE : SY-ULINE.

  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE : / SY-VLINE NO-GAP,
         (51) '소계 '   CENTERED NO-GAP,  SY-VLINE NO-GAP.

  WRITE : (05) '     '         NO-GAP, SY-VLINE  NO-GAP,
          (17) W_IN  UNIT P_MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (05) '    '          NO-GAP, SY-VLINE  NO-GAP,
          (17) W_CC  UNIT P_MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (05) '  '           NO-GAP, SY-VLINE  NO-GAP,
          (17) W_OU  UNIT P_MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (17) W_TOT_NC UNIT P_MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (17) W_TOT_CC UNIT P_MEINS NO-GAP,
                                       SY-VLINE  NO-GAP,
          (17) W_TOT_ST UNIT P_MEINS NO-GAP,
                                       SY-VLINE  NO-GAP.
  WRITE : SY-ULINE.

ENDFORM.                    " P3000_TOTAL_WRITE
*&---------------------------------------------------------------------*
*&      Form  P1000_READ_DATE_ZTIDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_READ_DATE_ZTIDS  USING    P_ZFOTDT
                                     P_ABNAR
                                     P_MATNR.

  CLEAR : IT_TAB-TXZ01.

  SELECT SINGLE MAKTX   INTO  IT_TAB-TXZ01
  FROM   MAKT
  WHERE  SPRAS     EQ   SY-LANGU
  AND    MATNR     EQ   P_MATNR.

  PERFORM P3000_ZTIDS_TITLE.

  SELECT A~ZFREBELN  A~ZFSHNO  A~ZFHBLNO  B~ZFIDRNO
         C~CCMENGE   C~MEINS   A~ZFETA
  INTO   CORRESPONDING FIELDS OF TABLE IT_CC_TMP
  FROM ( ( ZTBL  AS  A  INNER JOIN  ZTIDS  AS B
  ON     A~ZFBLNO     EQ          B~ZFBLNO         )
  INNER  JOIN  ZTIVIT AS C
  ON     B~ZFBLNO     EQ          C~ZFBLNO         )
  INNER  JOIN  ZTIMIMG03  AS  D
  ON     B~ZFBNARCD   EQ          D~ZFBNARCD
  WHERE  B~ZFIDSDT    EQ          P_ZFOTDT
  AND    D~ZFBNAR     EQ          P_ABNAR
  AND    C~MATNR      EQ          P_MATNR.

  LOOP  AT  IT_CC_TMP.
    PERFORM P3000_ZTIDS_WRITE.
  ENDLOOP.

  WRITE : / SY-ULINE.

ENDFORM.                    " P1000_READ_DATE_ZTIDS
*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDS_TITLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_ZTIDS_TITLE .

  NEW-PAGE LINE-SIZE 90 NO-HEADING .

  SKIP 2.
  WRITE : /40 '[통관상세내역]'.
  SKIP 1.
  WRITE :/ '통관일:',IT_TAB-ZFOTDT,
         / '자재  :',IT_TAB-MATNR ,  IT_TAB-TXZ01.
  WRITE : / SY-ULINE.
  FORMAT RESET.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE : / SY-VLINE NO-GAP,(13) 'P/O No'    NO-GAP CENTERED,
            SY-VLINE NO-GAP,(24) 'HOUSE B/L' NO-GAP CENTERED,
            SY-VLINE NO-GAP,(14) '수리번호'  NO-GAP CENTERED,
            SY-VLINE NO-GAP,(17) '수량'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(05) '단위'      NO-GAP CENTERED,
            SY-VLINE NO-GAP,(10) '실입항일'  NO-GAP CENTERED,
            SY-VLINE.
  WRITE : / SY-ULINE.

ENDFORM.                    " P3000_ZTIDS_TITLE

*&---------------------------------------------------------------------*
*&      Form  P3000_ZTIDS_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_ZTIDS_WRITE .

  DATA : W_TEXT(15) TYPE C.

  SET PF-STATUS 'DTINR'.             " GUI STATUS SETTING
  SET  TITLEBAR 'DTINR' WITH 'Clearance'. " GUI TITLE SETTING..

  IF W_MOD EQ 0.
    FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  ELSE.
    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
  ENDIF.

  CONCATENATE IT_CC_TMP-ZFREBELN '-' IT_CC_TMP-ZFSHNO INTO W_TEXT.
  WRITE :/ SY-VLINE NO-GAP,(13) W_TEXT NO-GAP,
           SY-VLINE NO-GAP,(24) IT_CC_TMP-ZFHBLNO   NO-GAP,
           SY-VLINE NO-GAP,(14) IT_CC_TMP-ZFIDRNO   NO-GAP,
           SY-VLINE NO-GAP,(17) IT_CC_TMP-CCMENGE
                           UNIT IT_CC_TMP-MEINS     NO-GAP,
           SY-VLINE NO-GAP,(05) IT_CC_TMP-MEINS     NO-GAP,
           SY-VLINE NO-GAP,(10) IT_CC_TMP-ZFETA     NO-GAP,
           SY-VLINE.

ENDFORM.                    " P3000_ZTIDS_WRITE
