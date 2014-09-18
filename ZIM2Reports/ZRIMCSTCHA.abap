*&---------------------------------------------------------------------*
*& Report  ZRIMCSTCHA                                                  *
*&---------------------------------------------------------------------*
*&  Program : 원가반영 금액 대비 수입비용 금?
*&     Name : Na Hyun-Ju INFOLINK Ltd.                                *
*&     Date : 2001.06.08                                            *
*&---------------------------------------------------------------------*
*&   DESC.     : STANDARD의 CONDITION TYPE 이 지정되어 원가에 반영금액 *
*&               대비 수입 ACTURE 비용 조회                            *
*&---------------------------------------------------------------------*
REPORT  ZRIMCSTCHA    MESSAGE-ID ZIM
                      LINE-SIZE 124
                      NO STANDARD PAGE HEADING.
*-----------------------------------------------------------------------
* Tables & Variable Define
*-----------------------------------------------------------------------
INCLUDE : <ICON>,
          ZRIMCSTCHATOP.

DATA : BEGIN OF   IT_TAB OCCURS 0,
       EBELN      LIKE EKBZ-EBELN,
       EBELP      LIKE EKBZ-EBELP,
       VGABE      LIKE EKBZ-VGABE,
       BEWTP      LIKE T163C-BEWTP,
       BEWTK      LIKE T163C-BEWTK,
       BEWTL      LIKE T163C-BEWTL,
       KSCHL      LIKE EKBZ-KSCHL,
       TEXT(20)   TYPE C,
       BUKRS      LIKE EKKO-BUKRS,
       GJAHR      LIKE EKBZ-GJAHR,
       BELNR      LIKE EKBZ-BELNR,
       MENGE      LIKE EKBZ-BPMNG,
       MEINS      LIKE EKPO-MEINS,
       DMBTR      LIKE EKBZ-DMBTR,
       WAERS      LIKE EKBZ-WAERS,
       SORT       TYPE C.
DATA : END OF IT_TAB.

DATA : BEGIN OF   IT_TAB1 OCCURS 0,
       EBELN      LIKE EKBZ-EBELN,
       EBELP      LIKE EKBZ-EBELP,
       WERKS      LIKE EKPO-WERKS,
       SORT       TYPE C,
       TEXT(20)   TYPE C,
       BEWTP      LIKE T163C-BEWTP,
       VGABE      LIKE EKBZ-VGABE,
       KSCHL      LIKE EKBZ-KSCHL,
       DMBTR      LIKE EKBZ-DMBTR,
       WAERS      LIKE EKBZ-WAERS.
DATA : END OF IT_TAB1.

DATA : P_BUKRS    LIKE ZTIMIMG00-ZFBUKRS.
*-----------------------------------------------------------------------
* Selection Screen
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS    FOR ZTREQHD-BUKRS NO-EXTENSION
                                             NO INTERVALS,
                S_EBELN    FOR ZTREQIT-EBELN,   " P/O
                S_EBELP    FOR ZTREQIT-EBELP,   " P/O-ITEM
                S_MATNR    FOR ZTREQIT-MATNR,   " 자재.
                S_REQTY    FOR ZTREQHD-ZFREQTY, " 결제TYPE
                S_LIFNR    FOR ZTREQHD-LIFNR,   " VENDOR
                S_EKORG    FOR EKKO-EKORG,      " 구매조직.
                S_EKGRP    FOR EKKO-EKGRP,      " 구매그룹.
                S_WERKS    FOR ZTREQHD-ZFWERKS. " 플랜트.
SELECTION-SCREEN END OF BLOCK B1.

*>> Initial value SETTING.
INITIALIZATION.                          " 초기값 SETTING
  PERFORM  P1000_SET_BUKRS.
  SET  TITLEBAR  'ZIMY5'.               " GUI TITLE  SETTING
* Title Text Write
TOP-OF-PAGE.
  IF INCLUDE NE 'POPU'.
    PERFORM P1000_TITLE_WRITE.
  ENDIF.

*-----------------------------------------------------------------------
* START OF SELECTION
*-----------------------------------------------------------------------
START-OF-SELECTION.

*>> Import CONFIGURATION CHECK
  PERFORM P1000_CONFIG_CHECK   USING  W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.   EXIT. ENDIF.

*>> DATA SELECT!
  PERFORM P2000_READ_TEXT      USING  W_ERR_CHK.
  IF W_ERR_CHK EQ 'Y'.  EXIT.  ENDIF.

*-----------------------------------------------------------------------
* END OF SELECTION
*-----------------------------------------------------------------------
END-OF-SELECTION.
  PERFORM P3000_DATA_WRITE     USING  W_ERR_CHK.

*-----------------------------------------------------------------------
* User Command
*-----------------------------------------------------------------------
AT USER-COMMAND.

  CASE SY-UCOMM.
    WHEN 'FB03'.
      IF IT_TAB-BELNR IS INITIAL.
        MESSAGE S252. EXIT.
      ENDIF.
      PERFORM P2000_SHOW_SL USING IT_TAB-GJAHR
                                  IT_TAB-BELNR.
    WHEN 'ANAL'.
      CLEAR : SV_EBELN, SV_EBELP.
      MOVE IT_TAB-EBELN  TO  SV_EBELN.
      MOVE IT_TAB-EBELP  TO  SV_EBELP.
      INCLUDE = 'POPU'.
      CALL SCREEN 0100 STARTING AT  10   3
                       ENDING   AT  134   20.
      CLEAR : INCLUDE.
*------- Abbrechen (CNCL) ----------------------------------------------
    WHEN 'CNCL'.
      SET SCREEN 0.    LEAVE SCREEN.
*------- Suchen (SUCH) -------------------------------------------------
    WHEN 'SUCH'.
*------- Sortieren nach Feldbezeichnung (SORB) -------------------------
    WHEN 'SORB'.
*------- Sortieren nach Feldname (SORF) --------------------------------
    WHEN 'SORF'.
*------- Techn. Name ein/aus (TECH) ------------------------------------
    WHEN 'TECH'.
*------- Weiter suchen (WESU) ------------------------------------------
    WHEN 'WESU'.
    WHEN OTHERS.
  ENDCASE.

*-----------------------------------------------------------------------
*&   Event AT LINE-SELECTION
*-----------------------------------------------------------------------
AT LINE-SELECTION.

  DATA : L_TEXT(30).
  IF INCLUDE NE 'POPU'.
    GET CURSOR FIELD L_TEXT.
    CASE  L_TEXT.
      WHEN 'IT_TAB-EBELN'   OR  'IT_TAB-EBELP'.
        SET PARAMETER ID 'BES'  FIELD IT_TAB-EBELN.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P1000_CONFIG_CHECK USING    W_ERR_CHK.

  W_ERR_CHK = 'N'.
* Import Config Select
  SELECT SINGLE * FROM ZTIMIMG00.
* Not Found
  IF SY-SUBRC NE 0.
    W_ERR_CHK = 'Y'.   MESSAGE S961.   EXIT.
  ENDIF.

  IF ZTIMIMG00-ZFCSTMD NE 'S' AND ZTIMIMG00-ZFCSTMD NE 'P'.
    W_ERR_CHK = 'Y'.   MESSAGE S573.   EXIT.
  ENDIF.

ENDFORM.                    " P1000_CONFIG_CHECK
*&---------------------------------------------------------------------*
*&      Form  P2000_READ_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P2000_READ_TEXT USING    W_ERR_CHK.

  REFRESH : IT_TAB, IT_PO, IT_REQ, IT_DIV, IT_EKBZ.
  MOVE  'N'      TO  W_ERR_CHK.

*>> PO NUMBER GET!
  PERFORM P3000_READ_REDATA.
  IF W_ERR_CHK EQ 'Y'.
    MESSAGE  S738.
    EXIT.
  ENDIF.

*>> Related cost data GET!
  PERFORM P3000_READ_CSTDATA.
*>> LIST UP INTERNAL TABLE INSERT
  PERFORM P3000_WRITE_TAB.

ENDFORM.                    " P2000_READ_TEXT

*&---------------------------------------------------------------------*
*&      Form  P3000_READ_REDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_READ_REDATA.

*>> Import request PO data SELECT!
  SELECT  B~EBELN  B~EBELP
  INTO    CORRESPONDING FIELDS OF TABLE IT_REQ
  FROM    ZTREQHD  AS  A  INNER  JOIN  ZTREQIT  AS  B
  ON      A~ZFREQNO    EQ   B~ZFREQNO
  WHERE   B~EBELN      IN   S_EBELN
  AND     A~BUKRS      IN   S_BUKRS
  AND     B~EBELP      IN   S_EBELP
  AND     B~MATNR      IN   S_MATNR
  AND     A~ZFREQTY    IN   S_REQTY
  AND     A~LIFNR      IN   S_LIFNR.

  CLEAR  W_LINE.
  DESCRIBE TABLE IT_REQ LINES W_LINE.
  IF W_LINE EQ 0.
    MOVE  'Y'   TO  W_ERR_CHK.
    EXIT.
  ENDIF.

*>>  Accurate DATA SELECT using import request PO
  SELECT  B~EBELN   B~EBELP  B~MEINS  A~BUKRS  B~WERKS
  INTO    CORRESPONDING FIELDS OF TABLE IT_PO
  FROM    EKKO   AS  A  INNER  JOIN  EKPO  AS  B
  ON      A~EBELN       EQ     B~EBELN
  FOR     ALL  ENTRIES  IN     IT_REQ
  WHERE   B~EBELN       EQ     IT_REQ-EBELN
  AND     B~EBELP       EQ     IT_REQ-EBELP
  AND     A~EKORG       IN     S_EKORG
  AND     A~EKGRP       IN     S_EKGRP
  AND     B~WERKS       IN     S_WERKS.

ENDFORM.                    " P3000_READ_REDATA
*&---------------------------------------------------------------------*
*&      Form  P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_TITLE_WRITE.
  IF SY-LANGU EQ '3'.
  ELSE.
    SKIP 2.
    FORMAT COLOR COL_BACKGROUND INTENSIFIED OFF.
    WRITE : /50  '[ Analysis on price difference ]'
                 COLOR COL_HEADING INTENSIFIED OFF.
    WRITE : / 'Date : ', SY-DATUM.
    WRITE : / SY-ULINE.
    FORMAT COLOR COL_HEADING INTENSIFIED ON.
    WRITE : /                                          SY-VLINE NO-GAP,
              'P/O - Item      '               NO-GAP, SY-VLINE NO-GAP,
              'Condition category       '      NO-GAP, SY-VLINE NO-GAP,
              'Condition type           '      NO-GAP, SY-VLINE NO-GAP,
              'Documnt no'                     NO-GAP, SY-VLINE NO-GAP,
              '         Quantity   '           NO-GAP, SY-VLINE NO-GAP,
              '          Amount     '          NO-GAP, SY-VLINE NO-GAP.
    WRITE : / SY-ULINE NO-GAP.
  ENDIF.
ENDFORM.                    " P1000_TITLE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_READ_CSTDATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_READ_CSTDATA.

*>> 수입의뢰 비용 POSTING 자료 SELECT!
  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_DIV
  FROM    ZTBKPF       AS  A  INNER  JOIN  ZTBDIV  AS  B
  ON      A~BUKRS      EQ  B~BUKRS
  AND     A~GJAHR      EQ  B~GJAHR
  AND     A~BELNR      EQ  B~BELNR
  FOR     ALL ENTRIES  IN  IT_PO
  WHERE   B~EBELN      EQ  IT_PO-EBELN
  AND     B~EBELP      EQ  IT_PO-EBELP
  AND     A~ZFPOSYN    EQ  'Y'.

*>> 수입의뢰 비용 POSTING( LIV ) 자료 SELECT!
  DESCRIBE TABLE IT_DIV LINES W_LINE.
  IF W_LINE GT  0.
    SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_EKBZ
    FROM    EKBZ
    FOR     ALL ENTRIES  IN  IT_DIV
    WHERE   EBELN        EQ  IT_DIV-EBELN
    AND     EBELP        EQ  IT_DIV-EBELP.
  ELSE.
    SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_EKBZ
    FROM    EKBZ
    FOR     ALL ENTRIES  IN  IT_PO
    WHERE   EBELN        EQ  IT_PO-EBELN
    AND     EBELP        EQ  IT_PO-EBELP.
  ENDIF.

*>> 물대 비용 POSTING( LIV ) 자료 SELECT!
  SELECT  *  INTO CORRESPONDING FIELDS OF TABLE IT_EKBE
  FROM    EKBE
  FOR     ALL ENTRIES  IN  IT_PO
  WHERE   EBELN        EQ  IT_PO-EBELN
  AND     EBELP        EQ  IT_PO-EBELP
  AND   ( BEWTP        EQ  'Q'
  OR      BEWTP        EQ  'R'
  OR      BEWTP        EQ  'N' ).

ENDFORM.                    " P3000_READ_CSTDATA
*&---------------------------------------------------------------------*
*&      Form  P3000_WRITE_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_WRITE_TAB.

*>> 수입비용들 중에서 비용(AP POSTING)되는 자료만 LIST UP
  LOOP  AT  IT_DIV  WHERE  ZFDCSTX NE 'X'.

    CLEAR  IT_TAB.
    MOVE-CORRESPONDING  IT_DIV  TO  IT_TAB.

*> 역기표 데이타--> 음수처리. 강석봉 추가 작업....START.
    IF IT_DIV-ZFRVSX EQ 'X'.
      IT_DIV-WRBTR = IT_DIV-WRBTR * -1.
      IT_DIV-DMBTR = IT_DIV-DMBTR * -1.
      IT_DIV-MENGE = IT_DIV-MENGE * -1.
    ENDIF.
*> 역기표 데이타--> 음수처리. 강석봉 추가 작업....END.

    MOVE  IT_DIV-ZFCSTGRP   TO  IT_TAB-BEWTK.
    MOVE  IT_DIV-ZFCD       TO  IT_TAB-KSCHL.
    MOVE  '4'               TO  IT_TAB-SORT.
    MOVE  IT_DIV-ZFACDO     TO  IT_TAB-BELNR.
    MOVE  IT_DIV-ZFFIYR     TO  IT_TAB-GJAHR.
*      MOVE  IT_DIV-WRBTR      TO  IT_TAB-DMBTR.
    MOVE  IT_DIV-DMBTR      TO  IT_TAB-DMBTR.
    MOVE  IT_DIV-HWAER      TO  IT_TAB-WAERS.
    MOVE  IT_DIV-MENGE      TO  IT_TAB-MENGE.

*>> 비용 그룹명 DISPLAY.
    IF SY-LANGU EQ '3'.
      CASE    IT_DIV-ZFCSTGRP.
        WHEN '003'.
          MOVE '수입의뢰 비용'    TO  IT_TAB-BEWTL.
        WHEN '004'.
          MOVE 'B/L 관련비용'     TO  IT_TAB-BEWTL.
        WHEN '005'.
          MOVE '내륙운송관련비용' TO  IT_TAB-BEWTL.
        WHEN '006'.
          MOVE '통관관련비용'     TO  IT_TAB-BEWTL.
        WHEN '009'.
          MOVE '수송관련비용'     TO  IT_TAB-BEWTL.
      ENDCASE.
    ELSE.
      CASE    IT_DIV-ZFCSTGRP.
        WHEN '003'.
          MOVE 'Import Request'   TO  IT_TAB-BEWTL.
        WHEN '004'.
          MOVE 'Oversea trans.'   TO  IT_TAB-BEWTL.
        WHEN '005'.
          MOVE 'Bonded transport' TO  IT_TAB-BEWTL.
        WHEN '006'.
          MOVE 'Clearance'        TO  IT_TAB-BEWTL.
        WHEN '009'.
          MOVE 'Inland trans'     TO  IT_TAB-BEWTL.
      ENDCASE.
    ENDIF.
*>>비용명 SELECT.
    SELECT  SINGLE  *
    FROM    ZTIMIMG08
    WHERE   ZFCDTY  EQ  IT_DIV-ZFCSTGRP
    AND     ZFCD    EQ  IT_DIV-ZFCD.

    MOVE  ZTIMIMG08-ZFCDNM    TO   IT_TAB-TEXT.
    MOVE  ZTIMIMG08-COND_TYPE TO   IT_TAB-KSCHL.
    APPEND  IT_TAB.

  ENDLOOP.

*>> 전표 TABLE에서 LIST UP.
  LOOP  AT  IT_EKBZ.

    CLEAR  IT_TAB.
    MOVE-CORRESPONDING  IT_EKBZ  TO  IT_TAB.
*>> 수량 단위 GET
    READ TABLE IT_PO WITH KEY EBELN  =  IT_EKBZ-EBELN
                              EBELP  =  IT_EKBZ-EBELP.
    MOVE  IT_PO-MEINS  TO  IT_TAB-MEINS.
    MOVE  IT_PO-BUKRS  TO  IT_TAB-BUKRS.

*>> SORT 순서 SETTING
    IF IT_EKBZ-BEWTP  EQ  'F'.
      MOVE  '1'      TO   IT_TAB-SORT.
    ELSEIF  IT_EKBZ-BEWTP  EQ  'M'.
      MOVE  '2'      TO   IT_TAB-SORT.
    ELSEIF  IT_EKBZ-BEWTP  EQ  'C'.
      MOVE  '3'      TO   IT_TAB-SORT.
    ENDIF.

*>> 차변/ 대변 계정에 맞게끔 금액 MOVE.
    IF IT_EKBZ-VGABE EQ '1'.
      IF IT_EKBZ-SHKZG EQ 'H'.
        MOVE  IT_EKBZ-DMBTR   TO  IT_TAB-DMBTR.
        MOVE  IT_EKBZ-MENGE   TO  IT_TAB-MENGE.
      ELSE.
        IT_TAB-DMBTR  =  IT_EKBZ-DMBTR * ( -1 ).
        IT_TAB-MENGE  =  IT_EKBZ-MENGE * ( -1 ).
      ENDIF.
    ELSEIF IT_EKBZ-VGABE EQ '2'.
      IF IT_EKBZ-SHKZG EQ 'H'.
        IT_TAB-DMBTR  =  IT_EKBZ-DMBTR * ( -1 ).
        IT_TAB-MENGE  =  IT_EKBZ-MENGE * ( -1 ).
      ELSE.
        MOVE  IT_EKBZ-DMBTR  TO  IT_EKBZ-DMBTR.
        MOVE  IT_EKBZ-MENGE  TO  IT_EKBZ-MENGE.
      ENDIF.
    ENDIF.
*>> 조건 범주 SETTING
    SELECT  SINGLE BEWTK BEWTL
    INTO    (IT_TAB-BEWTK, IT_TAB-BEWTL)
    FROM    T163C
    WHERE   SPRAS         EQ    SY-LANGU
    AND     BEWTP         EQ    IT_TAB-BEWTP.
*>> 조건 유형 SETTING
    SELECT  SINGLE VTEXT  INTO  IT_TAB-TEXT
    FROM    T685T
    WHERE   SPRAS       EQ     SY-LANGU
    AND     KVEWE       EQ     'A'
    AND     KAPPL       EQ     'M'
    AND     KSCHL       EQ     IT_TAB-KSCHL.

    MOVE  IT_EKBZ-HSWAE  TO    IT_TAB-WAERS.
    APPEND  IT_TAB.
  ENDLOOP.

*>> 전표 TABLE(물대)에서 LIST UP.
  LOOP  AT  IT_EKBE.

    CLEAR  IT_TAB.
*>> PO ITEM의 CONDITION TYPE 정의되지 않은 항목은 LIST UP에서 빼기.
    READ TABLE IT_EKBZ WITH KEY EBELN = IT_EKBE-EBELN
                                EBELP = IT_EKBE-EBELP.
    IF SY-SUBRC NE 0. CONTINUE. ENDIF.

    MOVE-CORRESPONDING  IT_EKBE  TO  IT_TAB.
*>> 수량 단위 GET
    READ TABLE IT_PO WITH KEY EBELN  =  IT_EKBE-EBELN
                              EBELP  =  IT_EKBE-EBELP.
    MOVE  IT_PO-MEINS  TO  IT_TAB-MEINS.
    MOVE  IT_PO-BUKRS  TO  IT_TAB-BUKRS.

*>> SORT 순서 SETTING
    IF  IT_EKBE-BEWTP  NE 'N'.
      MOVE  '0'      TO   IT_TAB-SORT.
    ELSE.
      MOVE  '9'      TO   IT_TAB-SORT.
    ENDIF.
*>> 차변/ 대변 계정에 맞게끔 금액 MOVE.
    IF IT_EKBE-SHKZG EQ 'S'.
      MOVE  IT_EKBE-DMBTR   TO  IT_TAB-DMBTR.
      MOVE  IT_EKBE-MENGE   TO  IT_TAB-MENGE.
    ELSE.
      IT_TAB-DMBTR  =  IT_EKBE-DMBTR * ( -1 ).
      IT_TAB-MENGE  =  IT_EKBE-MENGE * ( -1 ).
    ENDIF.
*>> 조건 범주 SETTING
    SELECT  SINGLE BEWTK BEWTL
    INTO    (IT_TAB-BEWTK, IT_TAB-BEWTL)
    FROM    T163C
    WHERE   SPRAS         EQ    SY-LANGU
    AND     BEWTP         EQ    IT_TAB-BEWTP.

    MOVE  IT_EKBE-HSWAE  TO    IT_TAB-WAERS.
    APPEND  IT_TAB.
  ENDLOOP.

  SORT  IT_TAB  BY  EBELN  EBELP  SORT BELNR.

  DESCRIBE  TABLE  IT_TAB  LINES  W_COUNT.
  IF W_COUNT  LE  0.
    MESSAGE  S966.
    W_ERR_CHK  =  'N'.
  ENDIF.

ENDFORM.                    " P3000_WRITE_TAB
*&---------------------------------------------------------------------*
*&      Form  P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ERR_CHK  text
*----------------------------------------------------------------------*
FORM P3000_DATA_WRITE USING    W_ERR_CHK.

  CLEAR  :  IT_TAB,  SV_MENGE,  SV_DMBTR,  SV_SUM1,
            SV_SUM2, SV_SUM3,   SV_SUM4,   SV_CHK.

  MOVE  'N'    TO  W_ERR_CHK.
  SET PF-STATUS 'ZIMY5'.           " GUI STATUS SETTING
  SET  TITLEBAR 'ZIMY5'.           " GUI TITLE SETTING..

  LOOP  AT  IT_TAB.

    IF SY-TABIX EQ 1.
      MOVE  IT_TAB-SORT   TO  SV_SORT.
      MOVE  IT_TAB-EBELN  TO  SV_EBELN.
      MOVE  IT_TAB-EBELP  TO  SV_EBELP.
      MOVE  'X'           TO  SV_CHK.
    ENDIF.

*>> 비용 GROUP 별로 SUM WRITE.
    IF  IT_TAB-EBELN EQ  SV_EBELN  AND
        IT_TAB-EBELP EQ  SV_EBELP  AND
        IT_TAB-SORT  NE  SV_SORT.
      PERFORM   P3000_SUM_WRITE.
      SV_MENGE  =  0.
      SV_DMBTR  =  0.
      MOVE  IT_TAB-SORT  TO  SV_SORT.
    ENDIF.
*>> PO ITEM 별로 가격차 WRITE.
    IF  IT_TAB-EBELN  NE  SV_EBELN  OR
        IT_TAB-EBELP  NE  SV_EBELP.
      PERFORM   P3000_SUM_WRITE.
      WRITE : / SY-ULINE NO-GAP.
      CLEAR : SV_SUM1,  SV_SUM2, SV_SUM3, SV_SUM4,
              SV_MENGE, SV_DMBTR.
      MOVE  IT_TAB-EBELN  TO  SV_EBELN.
      MOVE  IT_TAB-EBELP  TO  SV_EBELP.
      MOVE  IT_TAB-SORT   TO  SV_SORT.
      MOVE  'X'           TO  SV_CHK.
    ENDIF.
*>> LINE WRITE.
    PERFORM   P3000_LINE_WRITE.
    CLEAR  SV_CHK.

*>> 비용 GROUP 별로 SUM.
    CASE  IT_TAB-SORT.
      WHEN  '1'.
        ADD  IT_TAB-DMBTR  TO  SV_SUM1.
      WHEN  '2'.
        ADD  IT_TAB-DMBTR  TO  SV_SUM2.
      WHEN  '3'.
        ADD  IT_TAB-DMBTR  TO  SV_SUM3.
      WHEN  '4'.
        ADD  IT_TAB-DMBTR  TO  SV_SUM4.
    ENDCASE.
    ADD  IT_TAB-MENGE   TO  SV_MENGE.
    ADD  IT_TAB-DMBTR   TO  SV_DMBTR.
    MOVE IT_TAB-MEINS   TO  SV_MEINS.
    MOVE IT_TAB-WAERS   TO  SV_WAERS.

    AT LAST.
      PERFORM P3000_SUM_WRITE.
      WRITE : / SY-ULINE NO-GAP.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " P3000_DATA_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_LINE_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE                NO-GAP.
  IF SV_CHK  EQ  'X'.
    WRITE : IT_TAB-EBELN       NO-GAP,
            '-'                NO-GAP,
            IT_TAB-EBELP       NO-GAP,
            SY-VLINE           NO-GAP.
  ELSE.
    WRITE : '                ' NO-GAP,
            SY-VLINE           NO-GAP.
  ENDIF.

  WRITE : IT_TAB-BEWTK          NO-GAP,
          ' '                   NO-GAP,
          IT_TAB-BEWTL          NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-KSCHL          NO-GAP,
          ' '                   NO-GAP,
          IT_TAB-TEXT           NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-BELNR          NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-MENGE UNIT     IT_TAB-MEINS  NO-GAP,
          IT_TAB-MEINS          NO-GAP,
          SY-VLINE              NO-GAP,
          IT_TAB-DMBTR CURRENCY IT_TAB-WAERS  NO-GAP,
          IT_TAB-WAERS          NO-GAP,
          SY-VLINE              NO-GAP.

  HIDE  IT_TAB.

ENDFORM.                    " P3000_LINE_WRITE
*&---------------------------------------------------------------------*
*&      Form  P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_SUM_WRITE.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE                NO-GAP,
          '                '  COLOR COL_NORMAL INTENSIFIED ON NO-GAP.
  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  IF SY-LANGU EQ '3'.
    WRITE : SY-VLINE                NO-GAP,
            '소      계'            NO-GAP.
  ELSE.
    WRITE : SY-VLINE                NO-GAP,
            'Sub-total '            NO-GAP.
  ENDIF.
  WRITE : '               '       NO-GAP,
          '                '      NO-GAP,
          '          '            NO-GAP,
          '           '           NO-GAP,
          SY-VLINE                NO-GAP,
          SV_MENGE   UNIT         SV_MEINS  NO-GAP,
          SV_MEINS                NO-GAP,
          SY-VLINE                NO-GAP,
          SV_DMBTR   CURRENCY     SV_WAERS  NO-GAP,
          SV_WAERS                NO-GAP,
          SY-VLINE                NO-GAP.

*>> 가격 차이 WRITE.
  IF SV_SORT EQ '2'.
    SV_CHA  =  SV_SUM1  -  SV_SUM2.
    FORMAT RESET.

    WRITE:/ SY-VLINE                NO-GAP,
            '                ' COLOR COL_NORMAL INTENSIFIED ON NO-GAP.

    IF SV_CHA GT 0.
      FORMAT COLOR COL_POSITIVE  INTENSIFIED OFF.
    ELSEIF SV_CHA LT 0.
      FORMAT COLOR COL_NEGATIVE  INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR COL_KEY       INTENSIFIED OFF.
    ENDIF.

*     FORMAT RESET.
*     FORMAT COLOR COL_KEY INTENSIFIED OFF.
    IF SY-LANGU EQ '3'.
      WRITE : SY-VLINE                NO-GAP,
              '가격차이  '            NO-GAP.
    ELSE.
      WRITE : SY-VLINE                NO-GAP,
              'Price difference'      NO-GAP.
    ENDIF.
    WRITE : '               '       NO-GAP,
            '          '            NO-GAP,
            '          '            NO-GAP,
            '           '           NO-GAP,
            SY-VLINE                NO-GAP,
            '            '          NO-GAP,
            '        '              NO-GAP,
            SY-VLINE                NO-GAP,
            SV_CHA    CURRENCY     SV_WAERS  NO-GAP,
            SV_WAERS                NO-GAP,
            SY-VLINE                NO-GAP.

  ENDIF.

  IF SV_SORT EQ '4'.

    FORMAT RESET.
*     FORMAT COLOR COL_KEY INTENSIFIED OFF.

    SV_CHA  =  SV_SUM3  -  SV_SUM4.

    WRITE:/ SY-VLINE                NO-GAP,
            '                ' COLOR COL_NORMAL INTENSIFIED ON NO-GAP.

    IF SV_CHA GT 0.
      FORMAT COLOR COL_POSITIVE  INTENSIFIED OFF.
    ELSEIF SV_CHA LT 0.
      FORMAT COLOR COL_NEGATIVE  INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR COL_KEY       INTENSIFIED OFF.
    ENDIF.

*     FORMAT RESET.
*     FORMAT COLOR COL_KEY INTENSIFIED OFF.
    IF SY-LANGU EQ '3'.
      WRITE : SY-VLINE                NO-GAP,
              '가격차이  '            NO-GAP.
    ELSE.
      WRITE : SY-VLINE                NO-GAP,
              'Price difference'      NO-GAP.
    ENDIF.
    WRITE : '               '       NO-GAP,
            '          '            NO-GAP,
            '          '            NO-GAP,
            '           '           NO-GAP,
            SY-VLINE                NO-GAP,
            '            '          NO-GAP,
            '        '              NO-GAP,
            SY-VLINE                NO-GAP,
            SV_CHA    CURRENCY     SV_WAERS  NO-GAP,
            SV_WAERS                NO-GAP,
            SY-VLINE                NO-GAP.

  ENDIF.

ENDFORM.                    " P3000_SUM_WRITE
*&---------------------------------------------------------------------*
*&      Form  P2000_SHOW_SL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TAB_GJAHR  text
*      -->P_IT_TAB_BELNR  text
*----------------------------------------------------------------------*
FORM P2000_SHOW_SL USING    P_GJAHR
                            P_BELNR.

  IF IT_TAB-SORT EQ '0'.
    SET PARAMETER ID 'RBN'  FIELD P_BELNR.
    SET PARAMETER ID 'GJR'  FIELD P_GJAHR.
    CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
  ENDIF.

  IF IT_TAB-SORT  EQ  '1'.
    SET PARAMETER ID 'MBN'  FIELD P_BELNR.
    SET PARAMETER ID 'MJA'  FIELD P_GJAHR.
*>> 입고문서 조회 FUNCTION CALL.
    CALL FUNCTION 'MIGO_DIALOG'
       EXPORTING
          I_ACTION                  = 'A04'
          I_REFDOC                  = 'R02'
          I_NOTREE                  = 'X'
*          I_NO_AUTH_CHECK           =
          I_SKIP_FIRST_SCREEN       = 'X'
*          I_DEADEND                 = 'X'
          I_OKCODE                  = 'OK_GO'
*          I_LEAVE_AFTER_POST        =
*          i_new_rollarea            = 'X'
*          I_SYTCODE                 =
*          I_EBELN                   =
*          I_EBELP                   =
          I_MBLNR                   = P_BELNR
          I_MJAHR                   = P_GJAHR
*          I_ZEILE                   =
       EXCEPTIONS
          ILLEGAL_COMBINATION       = 1
          OTHERS                    = 2.

  ENDIF.

  IF IT_TAB-SORT EQ '2'.
    SET PARAMETER ID 'RBN'  FIELD P_BELNR.
    SET PARAMETER ID 'GJR'  FIELD P_GJAHR.
    CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
  ENDIF.

  IF IT_TAB-SORT  EQ  '3'.
    SET PARAMETER ID 'MBN'  FIELD P_BELNR.
    SET PARAMETER ID 'MJA'  FIELD P_GJAHR.
*>> 입고문서 조회 FUNCTION CALL.
    CALL FUNCTION 'MIGO_DIALOG'
       EXPORTING
          I_ACTION                  = 'A04'
          I_REFDOC                  = 'R02'
          I_NOTREE                  = 'X'
*          I_NO_AUTH_CHECK           =
          I_SKIP_FIRST_SCREEN       = 'X'
*          I_DEADEND                 = 'X'
          I_OKCODE                  = 'OK_GO'
*          I_LEAVE_AFTER_POST        =
*          i_new_rollarea            = 'X'
*          I_SYTCODE                 =
*          I_EBELN                   =
*          I_EBELP                   =
          I_MBLNR                   = P_BELNR
          I_MJAHR                   = P_GJAHR
*          I_ZEILE                   =
       EXCEPTIONS
          ILLEGAL_COMBINATION       = 1
          OTHERS                    = 2.
  ENDIF.

  IF IT_TAB-SORT EQ '4'.
    SET PARAMETER ID 'BUK'     FIELD IT_TAB-BUKRS.
    SET PARAMETER ID 'GJR'     FIELD P_GJAHR.
    SET PARAMETER ID 'BLN'     FIELD P_BELNR.
    CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.
  ENDIF.

  IF IT_TAB-SORT EQ '9'.
    SET PARAMETER ID 'RBN'  FIELD P_BELNR.
    SET PARAMETER ID 'GJR'  FIELD P_GJAHR.
    CALL TRANSACTION 'MIR4' AND SKIP  FIRST SCREEN.
  ENDIF.

ENDFORM.                    " P2000_SHOW_SL
*&---------------------------------------------------------------------*
*&      Module  D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_STATUS_SCR0100 OUTPUT.

  SET PF-STATUS 'STDLISW'.

  CASE INCLUDE.
    WHEN 'POPU'.
      IF SY-LANGU EQ '3'.
        SET TITLEBAR 'POPU' WITH 'Analysis on price difference'.
      ELSE.
        SET TITLEBAR 'POPU' WITH 'Analysis on price difference'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  SUPPRESS DIALOG.

ENDMODULE.                 " D0100_STATUS_SCR0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE D0100_LIST_CHECK_SCR0100 INPUT.

  LEAVE TO LIST-PROCESSING.
  CASE INCLUDE.
    WHEN 'POPU'.
      PERFORM  P1000_WRITE_CST_SCR0100.
  ENDCASE.

ENDMODULE.                 " D0100_LIST_CHECK_SCR0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_CST_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_CST_SCR0100.

  CLEAR :  SV_SUM,  IT_TAB1.
  REFRESH  IT_TAB1.

*>> 물대 금액 SUM
  LOOP  AT  IT_TAB   WHERE  EBELN  EQ  SV_EBELN
                     AND    EBELP  EQ  SV_EBELP
                     AND    SORT   EQ  '0' .

    SV_SUM  =  SV_SUM  +  IT_TAB-DMBTR.
  ENDLOOP.
*>>PLANT GET!
  SELECT SINGLE * FROM EKPO WHERE EBELN EQ SV_EBELN
                            AND   EBELP EQ SV_EBELP.
*>> 조건 문서번호 GET!
  SELECT SINGLE * FROM EKKO WHERE EBELN EQ SV_EBELN.
  MOVE  EKKO-KNUMV  TO  SV_KNUMV.

*>> PLANT 명 GET!
  SELECT SINGLE NAME1  INTO  SV_NAME
  FROM   T001W         WHERE WERKS   EQ  EKPO-WERKS.

*>> WRITE TITLE.
  PERFORM  P1000_WRITE_TITLE_SCR0100.

*>> 물대를 제외한 비용들 GET!
  LOOP  AT  IT_TAB   WHERE  EBELN  EQ  SV_EBELN
                     AND    EBELP  EQ  SV_EBELP
                     AND    SORT   NE  '0'.

    MOVE : IT_TAB-EBELN  TO  IT_TAB1-EBELN,
           IT_TAB-EBELP  TO  IT_TAB1-EBELP,
           IT_TAB-BEWTP  TO  IT_TAB1-BEWTP,
           IT_TAB-VGABE  TO  IT_TAB1-VGABE,
           IT_TAB-KSCHL  TO  IT_TAB1-KSCHL,
           IT_TAB-TEXT   TO  IT_TAB1-TEXT,
           IT_TAB-DMBTR  TO  IT_TAB1-DMBTR,
           IT_TAB-WAERS  TO  IT_TAB1-WAERS.

*>> 수입의뢰 비용...
    IF IT_TAB-SORT EQ '4'.

      MOVE   '2'            TO  IT_TAB1-VGABE.
*>> 조건 유형 SETTING
      SELECT  SINGLE VTEXT  INTO  IT_TAB1-TEXT
      FROM    T685T
      WHERE   SPRAS       EQ     SY-LANGU
      AND     KVEWE       EQ     'A'
      AND     KAPPL       EQ     'M'
      AND     KSCHL       EQ     IT_TAB-KSCHL.
    ENDIF.

*>> 후속문서.
    IF IT_TAB-SORT EQ '9'.
      IF SY-LANGU EQ '3'.
        MOVE : 'ZZZZ'         TO  IT_TAB1-KSCHL,
               '후속송장처리' TO  IT_TAB1-TEXT,
               '2'            TO  IT_TAB1-VGABE.
      ELSE.
        MOVE : 'ZZZZ'         TO  IT_TAB1-KSCHL,
               'Follow I/V'   TO  IT_TAB1-TEXT,
               '2'            TO  IT_TAB1-VGABE.

      ENDIF.
    ENDIF.

    APPEND  IT_TAB1.
  ENDLOOP.

  SORT  IT_TAB1  BY  KSCHL BEWTP.
  PERFORM  P1000_WRITE_DATA_SCR0100.

ENDFORM.                    " P1000_WRITE_CST_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_TITLE_SCR0100.

  WRITE: /  'P/O no.  :' NO-GAP,
         18 SV_EBELN  NO-GAP,
         '-'             NO-GAP,
            SV_EBELP  NO-GAP.
  WRITE: / 'C/I Amt  :'  NO-GAP,
         15 SV_SUM       COLOR COL_TOTAL INTENSIFIED OFF
                         CURRENCY  SV_WAERS NO-GAP,
            SV_WAERS     COLOR COL_TOTAL INTENSIFIED OFF NO-GAP,
         55 'Plant    :' NO-GAP,
         66 EKPO-WERKS  NO-GAP,
            '-'          NO-GAP,
            SV_NAME      NO-GAP.

  WRITE : / SY-ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE : /                                          SY-VLINE NO-GAP,
            '                    '           NO-GAP, SY-VLINE NO-GAP,
            '        Planned Cost '          NO-GAP, SY-VLINE NO-GAP,
            'Condition(%'                    NO-GAP, SY-VLINE NO-GAP,
            'Reflect (%)'                    NO-GAP, SY-VLINE NO-GAP,
            '         Actual Cost '          NO-GAP, SY-VLINE NO-GAP,
            '  Actual(%)'                    NO-GAP, SY-VLINE NO-GAP,
            '         Defference  '          NO-GAP, SY-VLINE NO-GAP.
  WRITE : / SY-ULINE.

ENDFORM.                    " P1000_WRITE_TITLE_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P1000_WRITE_DATA_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_WRITE_DATA_SCR0100.

  CLEAR : SV_SUM1, SV_SUM2, SV_CHA, SV_PER1, SV_PER2, SV_PER3,
          SV_TOT1, SV_TOT2, SV_TOT3.

  LOOP  AT  IT_TAB1.

    IF SY-TABIX  EQ  1.
      MOVE  IT_TAB1-KSCHL  TO  SV_KSCHL.
      MOVE  IT_TAB1-TEXT   TO  SV_TEXT.
    ENDIF.

    IF IT_TAB1-KSCHL  NE  SV_KSCHL.
*>>조건 유형의 조건율 SELECT
      SELECT * FROM  KONV UP TO 1 ROWS
               WHERE KNUMV EQ SV_KNUMV
               AND   KPOSN EQ SV_EBELP
               AND   KSCHL EQ SV_KSCHL.
      ENDSELECT.
      MOVE  KONV-KBETR  TO  SV_PER3.
      SV_PER3  =  SV_PER3  /  10.

      IF SV_SUM NE 0.
        SV_PER1  =  ( SV_SUM1 / SV_SUM ) * 100.
        SV_PER2  =  ( SV_SUM2 / SV_SUM ) * 100.
      ELSE.
        SV_PER1  =  0.
        SV_PER2  =  0.
      ENDIF.
      SV_CHA   =  SV_SUM1  -  SV_SUM2.
      SV_TOT1  =  SV_TOT1  +  SV_SUM1.
      SV_TOT2  =  SV_TOT2  +  SV_SUM2.
      SV_TOT3  =  SV_TOT3  +  SV_CHA.
      PERFORM  P2000_WRITE_LINE_SCR0100.
      CLEAR : SV_SUM1, SV_SUM2, SV_CHA, SV_PER1, SV_PER2, SV_PER3.
      MOVE  IT_TAB1-KSCHL  TO  SV_KSCHL.
      MOVE  IT_TAB1-TEXT   TO  SV_TEXT.
    ENDIF.

    IF IT_TAB1-VGABE  EQ  '1'.
      SV_SUM1  =  SV_SUM1  +  IT_TAB1-DMBTR.
    ELSEIF IT_TAB1-VGABE EQ '2'.
      SV_SUM2  =  SV_SUM2  +  IT_TAB1-DMBTR.
    ENDIF.

    MOVE  IT_TAB1-WAERS  TO  SV_WAERS.

    AT LAST.
*>> 조건유형의 조건율 SELECT!
      IF  SV_KSCHL  NE  'ZZZZ'.
        SELECT * FROM  KONV UP TO 1 ROWS
                 WHERE KNUMV EQ SV_KNUMV
                 AND   KPOSN EQ SV_EBELP
                 AND   KSCHL EQ SV_KSCHL.
        ENDSELECT.
        MOVE  KONV-KBETR  TO  SV_PER3.
        SV_PER3  =  SV_PER3 / 10.
      ELSE.
        CLEAR  SV_PER3.
      ENDIF.

      IF SV_SUM NE 0.
        SV_PER1  =  ( SV_SUM1 / SV_SUM ) * 100.
        SV_PER2  =  ( SV_SUM2 / SV_SUM ) * 100.
      ELSE.
        SV_PER1  =  0.
        SV_PER2  =  0.
      ENDIF.

      SV_CHA   =  SV_SUM1  -  SV_SUM2.
      SV_TOT1  =  SV_TOT1  +  SV_SUM1.
      SV_TOT2  =  SV_TOT2  +  SV_SUM2.
      SV_TOT3  =  SV_TOT3  +  SV_CHA.
      PERFORM  P2000_WRITE_LINE_SCR0100.
      PERFORM  P2000_WRITE_SUM_SCR0100.
    ENDAT.

  ENDLOOP.

ENDFORM.                    " P1000_WRITE_DATA_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_LINE_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_LINE_SCR0100.

  FORMAT RESET.
  FORMAT COLOR COL_NORMAL INTENSIFIED ON.

  WRITE:/ SY-VLINE              NO-GAP,
          SV_TEXT               NO-GAP,
          SY-VLINE              NO-GAP,
          SV_SUM1  CURRENCY SV_WAERS  NO-GAP,
          SV_WAERS              NO-GAP,
          SY-VLINE              NO-GAP,
          SV_PER3               NO-GAP,
          SY-VLINE              NO-GAP,
          SV_PER1               NO-GAP,
          SY-VLINE              NO-GAP,
          SV_SUM2  CURRENCY SV_WAERS  NO-GAP,
          SV_WAERS              NO-GAP,
          SY-VLINE              NO-GAP,
          SV_PER2               NO-GAP,
          SY-VLINE              NO-GAP,
          SV_CHA   CURRENCY SV_WAERS  NO-GAP,
          SV_WAERS              NO-GAP,
          SY-VLINE              NO-GAP.
  WRITE : / SY-ULINE.

ENDFORM.                    " P2000_WRITE_LINE_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P2000_WRITE_SUM_SCR0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_WRITE_SUM_SCR0100.

  FORMAT RESET.
  FORMAT COLOR COL_TOTAL INTENSIFIED OFF.

  WRITE:/ SY-VLINE              NO-GAP,
          '     TOTAL         ' NO-GAP,
          ' '                   NO-GAP,
          SY-VLINE              NO-GAP,
          SV_TOT1  CURRENCY SV_WAERS  NO-GAP,
          SV_WAERS              NO-GAP,
          SY-VLINE              NO-GAP,
          '           '         NO-GAP,
          SY-VLINE              NO-GAP,
          '           '         NO-GAP,
          SY-VLINE              NO-GAP,
          SV_TOT2  CURRENCY SV_WAERS  NO-GAP,
          SV_WAERS              NO-GAP,
          SY-VLINE              NO-GAP,
          '           '         NO-GAP,
          SY-VLINE              NO-GAP,
          SV_TOT3   CURRENCY SV_WAERS  NO-GAP,
          SV_WAERS              NO-GAP,
          SY-VLINE              NO-GAP.
  WRITE : / SY-ULINE.

ENDFORM.                    " P2000_WRITE_SUM_SCR0100
*&---------------------------------------------------------------------*
*&      Form  P1000_SET_BUKRS
*&---------------------------------------------------------------------*
FORM P1000_SET_BUKRS.

  CLEAR : ZTIMIMG00, P_BUKRS.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF NOT ZTIMIMG00-ZFBUFIX IS INITIAL.
    MOVE  ZTIMIMG00-ZFBUKRS   TO  P_BUKRS.
  ENDIF.

*>> 회사코드 SET.
  MOVE: 'I'          TO S_BUKRS-SIGN,
        'EQ'         TO S_BUKRS-OPTION,
        P_BUKRS      TO S_BUKRS-LOW.
  APPEND S_BUKRS.

ENDFORM.                    " P1000_SET_BUKRS
