************************************************************************
* Program Name      : ZEMM_CREATE_MATL_PLAN_21D
* Author            : Furong
* Creation Date     : 10/10/2006
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       : Create the procution material requirement (866)
*
* Modification Logs
* Date       Developer    RequestNo   Description
* *********************************************************************
* 12/19/2011 Valerian     UD1K953578  HMMA Engine Plant split
*                                     implementation
* *********************************************************************
REPORT ZEMMPM_CREATE_MATL_PLAN_21D MESSAGE-ID ZMMM .

TABLES: RESB,MARA.

*---// Internal tables
DATA: IT_21DAY LIKE ZTMM_PARTS_21DAY OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_RESB OCCURS 0,
        MATNR   LIKE   RESB-MATNR,
        WERKS   LIKE   RESB-WERKS,
        BDTER   LIKE   RESB-BDTER,
        LGORT   LIKE   RESB-LGORT,
        MEINS   LIKE   RESB-MEINS,
        RSNUM   LIKE   RESB-RSNUM,
        BDMNG   LIKE   RESB-BDMNG,
        SORTF   LIKE   RESB-SORTF,
        PRVBE   LIKE   RESB-PRVBE,
      END   OF IT_RESB.

DATA: BEGIN OF IT_BFD OCCURS 0,
        MATNR   LIKE   RESB-MATNR,
        WERKS   LIKE   RESB-WERKS,
        BDMNG   LIKE   RESB-BDMNG,
*        enmng   LIKE   resb-enmng,
*        bal     LIKE   resb-enmng,
*        sortf   LIKE   resb-sortf,
*        prvbe   LIKE   resb-prvbe,
      END   OF IT_BFD.

DATA: IT_RESB_21 LIKE TABLE OF IT_RESB WITH HEADER LINE.

DATA: BEGIN OF IT_DAY OCCURS 21,
        SEQ     TYPE I,
        DATUM   LIKE   SY-DATUM,
      END   OF IT_DAY.

DATA: BEGIN OF IT_MATNR OCCURS 0,
        MATNR   LIKE   MARA-MATNR,
        WERKS   LIKE   MARC-WERKS,
        LIFNR   LIKE   LFA1-LIFNR,
        PROFL   LIKE   MARA-PROFL,
        TEMPB   LIKE   MARA-TEMPB,
        RAUBE   LIKE   MARA-RAUBE,
        DISPO   LIKE   MARC-DISPO,
*        PRVBE   LIKE   RESB-PRVBE,
*        sortf   LIKE   resb-sortf,
      END   OF IT_MATNR.

*---// Work area
DATA: W_DAY_GAP TYPE I.

*---// Ranges
RANGES: R_LGORT   FOR   RESB-LGORT.

*---// For Listbox variable
TYPE-POOLS: VRM.
DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST.

*---> For body part qty calculation
DATA: BEGIN OF IT_INPUT OCCURS 0.
        INCLUDE STRUCTURE ZTPP_INPUT_PLAN.
DATA:   FSC  LIKE MARA-MATNR.
DATA: END OF IT_INPUT.
DATA:  WA_KALID   LIKE KAKO-KALID .
DATA: WA_LASTDATE  LIKE SY-DATUM.
*--->added by chris for body qty


*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
PARAMETERS : "p_werks LIKE t001w-werks   OBLIGATORY DEFAULT 'P001',
             P_LGORT LIKE RESB-LGORT AS LISTBOX VISIBLE LENGTH 14,
             P_DATUM LIKE SY-DATUM      OBLIGATORY DEFAULT SY-DATUM.
SELECTION-SCREEN SKIP.
PARAMETERS: P_TEST AS CHECKBOX.  " MODIF ID TS.
SELECT-OPTIONS:  S_MATNR FOR MARA-MATNR NO-EXTENSION. " MODIF ID TS.
SELECTION-SCREEN END OF BLOCK BL1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SET_LISTBOX.
*  PERFORM screen_modify.

AT SELECTION-SCREEN.
  CHECK SY-UCOMM EQ 'ONLI'.
  PERFORM SET_DAYS.
  PERFORM CHECK_RTN.
  PERFORM READ_DATA.

START-OF-SELECTION.
  PERFORM SET_21DAYS_DATA.
  PERFORM UPDATE_TABLE.

*   adding logic to change the body part qty by using
*   body input plan method. because plan now is scheduled
*   by trim input plan, but the body part requirement
*   should base on the body input plan. Trim part requirement
*   is later than body part requirements

*  commented on 02.12.2014 Victor:  requested by Ashwini/MIT
*  PERFORM CHANGE_BODY_PART_QTY.

*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCREEN_MODIFY.
*  LOOP AT SCREEN.
*   IF SCREEN-GROUP1 = 'TS'.
*    IF screen-name = 'P_TEST'.
*      screen-input = 1.
*      MODIFY SCREEN.
*    ENDIF.
*   ENDIF.
*  ENDLOOP.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
*  PERFORM read_resb.
*  perform read_resb_21times.

** changed on 10/10/06 for performance improvement
*  PERFORM read_resb_plaf.
*  PERFORM read_master.
*  PERFORM read_resb_bfd.
  PERFORM READ_ZRESB.
  PERFORM READ_MASTER.
  PERFORM READ_ZRESB_BFD.
** end of change
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_RESB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_RESB.
  WRITE: / 'Starting selection for resb'.
  SELECT A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
         A~RSNUM A~SORTF A~PRVBE
         SUM( BDMNG ) AS BDMNG
    INTO CORRESPONDING FIELDS OF TABLE IT_RESB
    FROM RESB AS A INNER JOIN MARA AS B
                      ON A~MATNR = B~MATNR
*                    AND b~mtart = 'ROH'
   WHERE A~BDTER >= P_DATUM
*     AND A~BDTER <= WA_LASTDATE
     AND A~LGORT IN R_LGORT
*     AND a~werks EQ p_werks "COMMENT BY CHRIS ON 06/28/2005
     AND ( B~MTART = 'ROH' OR
           B~MTART = 'HALB' )
     AND A~XLOEK EQ SPACE
     AND A~KZEAR >= ' '
   GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
         A~RSNUM A~SORTF A~PRVBE.
  WRITE: / 'Ending selection for resb'.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M02.
  ENDIF.
  SORT IT_RESB BY MATNR.
ENDFORM.                    " READ_RESB
*&---------------------------------------------------------------------*
*&      Form  set_listbox
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LISTBOX.
  PERFORM SET_LISTBOX_LGORT.
ENDFORM.                    " set_listbox
*&---------------------------------------------------------------------*
*&      Form  set_listbox_lgort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_LISTBOX_LGORT.
  CLEAR: NAME, VALUE, LIST.

  NAME = 'P_LGORT'.

  MOVE: SPACE       TO VALUE-KEY,
        'All'       TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  MOVE: 'P500'      TO VALUE-KEY,
        'JIS'       TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  MOVE: 'P400'      TO VALUE-KEY,
        'JIT'       TO VALUE-TEXT.
  APPEND VALUE TO LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = NAME
            VALUES = LIST.
ENDFORM.                    " set_listbox_lgort
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_RTN.
  IF P_LGORT NE SPACE.
    MOVE: 'I'     TO R_LGORT-SIGN,
          'EQ'    TO R_LGORT-OPTION,
          P_LGORT TO R_LGORT-LOW.

    APPEND R_LGORT.
  ENDIF.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  read_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MASTER.

  LOOP AT IT_RESB.
    MOVE: IT_RESB-MATNR TO IT_MATNR-MATNR,
** Changed by Furong on 03/30/10
          IT_RESB-WERKS TO IT_MATNR-WERKS.
** End of change
    COLLECT IT_MATNR.
  ENDLOOP.

  READ TABLE IT_DAY INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M01.
  ENDIF.

  LOOP AT IT_MATNR.
    EXEC SQL.
      SELECT V.MATNR, w.werks, Y.LIFNR, V.PROFL, V.TEMPB, V.RAUBE,
             W.DISPO
        INTO :IT_MATNR
*        FROM MARA V, MARC W, ZVMM_TASK_LIST X, EORD Y
        FROM MARA V, MARC W, EORD Y
       WHERE V.MANDT    =  :SY-MANDT
*         AND V.MTART    =  'ROH'
         AND V.MATNR    =  :IT_MATNR-MATNR
         AND W.MANDT    =  V.MANDT
         AND W.MATNR    =  V.MATNR
** Changed by Furong on 03/30/10
*         AND W.WERKS    =  'P001'
         AND W.WERKS    =  :IT_MATNR-werks
** End of change
*         AND W.WERKS    =  :P_WERKS  "COMEMT BY CHRIS ON /6/28/2005
*         AND X.MANDT(+) =  W.MANDT
*         AND X.PLNNR(+) =  'RP'
*         AND X.USR00(+) =  W.VSPVB
         AND Y.MANDT(+) =  W.MANDT
         AND Y.WERKS(+) =  W.WERKS
         AND Y.MATNR(+) =  W.MATNR
         AND Y.VDATU(+) <= :P_DATUM
         AND Y.BDATU(+) >= :P_DATUM
    ENDEXEC.

    IF IT_MATNR-DISPO IS INITIAL AND IT_MATNR-WERKS = 'E001'.
      SELECT SINGLE DISPO INTO IT_MATNR-DISPO
        FROM MARC
        WHERE MATNR = IT_MATNR-MATNR
          AND WERKS = 'P001'.
    ENDIF.

* BEGIN OF UD1K953578
    IF IT_MATNR-DISPO IS INITIAL AND IT_MATNR-WERKS = 'E002'.
      SELECT SINGLE DISPO INTO IT_MATNR-DISPO
        FROM MARC
        WHERE MATNR = IT_MATNR-MATNR
          AND WERKS = 'P001'.
    ENDIF.
* END OF UD1K953578

    IF IT_MATNR-LIFNR EQ SPACE.
      PERFORM READ_VENDOR_FROM_SA.
    ENDIF.

    IF IT_MATNR-LIFNR EQ SPACE.
      PERFORM READ_VENDOR_FROM_INFO.
    ENDIF.

    MODIFY: IT_MATNR.
  ENDLOOP.

  SORT IT_MATNR BY MATNR.
ENDFORM.                    " read_master
*&---------------------------------------------------------------------*
*&      Form  read_vendor_from_sa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_VENDOR_FROM_SA.
  SELECT SINGLE LIFNR INTO IT_MATNR-LIFNR
    FROM EKKO AS A INNER JOIN EKPO AS B
                      ON A~MANDT EQ B~MANDT
                     AND A~EBELN EQ B~EBELN
   WHERE MATNR   EQ IT_MATNR-MATNR
*     AND werks   EQ p_werks "COMMENT BY CHRIS ON 06/28/2005
     AND A~LOEKZ EQ SPACE
     AND B~LOEKZ EQ SPACE
     AND KDATB   <= SY-DATUM
     AND KDATE   >= SY-DATUM.
ENDFORM.                    " read_vendor_from_sa
*&---------------------------------------------------------------------*
*&      Form  read_vendor_from_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_VENDOR_FROM_INFO.
  DATA: LT_A018 LIKE A018 OCCURS 0 WITH HEADER LINE.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE LT_A018
    FROM A018
   WHERE KAPPL =  'M'
     AND KSCHL =  'PB00'
     AND MATNR =  IT_MATNR-MATNR
     AND EKORG =  'PU01'
     AND ESOKZ =  '0'
     AND DATAB <= P_DATUM
     AND DATBI >= P_DATUM.

  LOOP AT LT_A018.
    SELECT SINGLE A~MATNR INTO IT_MATNR-MATNR
      FROM EINA AS A INNER JOIN EINE AS B
        ON A~INFNR = B~INFNR
     WHERE A~MATNR = IT_MATNR-MATNR
       AND A~LIFNR = LT_A018-LIFNR
       AND A~LOEKZ = ' '
       AND B~WERKS = ' '
       AND B~EKORG = 'PU01'
       AND B~LOEKZ = ' '.
    IF SY-SUBRC NE 0.
      DELETE LT_A018.
    ENDIF.
  ENDLOOP.

  SORT LT_A018 BY DATAB DESCENDING.

  READ TABLE LT_A018 INDEX 1.

  MOVE: LT_A018-LIFNR TO IT_MATNR-LIFNR.
ENDFORM.                    " read_vendor_from_info
*&---------------------------------------------------------------------*
*&      Form  set_21days_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_21DAYS_DATA.
  DATA: LW_INDEX LIKE SY-TABIX.
  DATA: L_MATNR  LIKE MARA-MATNR.

  LOOP AT IT_RESB.
    CLEAR: IT_21DAY.
    IF IT_RESB-BDTER GT WA_LASTDATE.
      CONTINUE.
    ENDIF.
    READ TABLE IT_DAY WITH KEY DATUM = IT_RESB-BDTER
                      BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE IT_21DAY WITH KEY     "werks = p_werks COMMENT BY CHRIS
*                                      ON 06/28/2005
                                MATNR = IT_RESB-MATNR.

    IF SY-SUBRC EQ 0.
      MOVE: SY-TABIX TO LW_INDEX.

      PERFORM APPEND_QUANTITY.

      MODIFY IT_21DAY INDEX LW_INDEX.

    ELSE.
      PERFORM APPEND_OTHER_FIELDS.
      PERFORM APPEND_QUANTITY.
      PERFORM APPEND_PASSDUE_QTY.
      APPEND IT_21DAY.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_21days_data
*&---------------------------------------------------------------------*
*&      Form  APPEND_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_QUANTITY.
  DATA: LW_QUANTITY(50),
        LW_DAY(2) TYPE N,
        LW_BDMNG  LIKE RESB-BDMNG.

  FIELD-SYMBOLS: <QUANTITY>.

  READ TABLE IT_DAY WITH KEY DATUM = IT_RESB-BDTER
                    BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE: IT_RESB-BDMNG TO LW_BDMNG.
  ELSE.
    MOVE: 0             TO LW_BDMNG.
  ENDIF.

  LW_DAY = IT_DAY-SEQ.

  CONCATENATE 'IT_21DAY-D' LW_DAY INTO LW_QUANTITY.
  ASSIGN (LW_QUANTITY) TO <QUANTITY>.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  <QUANTITY> = <QUANTITY> + LW_BDMNG.
ENDFORM.                    " APPEND_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  APPEND_OTHER_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_OTHER_FIELDS.
  READ TABLE IT_MATNR WITH KEY MATNR = IT_RESB-MATNR
                      BINARY SEARCH.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M01.
  ENDIF.

  MOVE: IT_RESB-WERKS     TO IT_21DAY-WERKS,
        IT_MATNR-MATNR    TO IT_21DAY-MATNR,
        IT_MATNR-DISPO    TO IT_21DAY-DISPO,
        P_DATUM           TO IT_21DAY-DATUM,
        IT_MATNR-PROFL    TO IT_21DAY-PROFL,
        IT_RESB-LGORT     TO IT_21DAY-LGORT,
        IT_MATNR-LIFNR    TO IT_21DAY-LIFNR,
*        it_RESB-PRVBE(1)  TO it_21day-shop,
        IT_RESB-PRVBE     TO IT_21DAY-PRVBE,
        IT_RESB-SORTF     TO IT_21DAY-SORTF,
        IT_RESB-MEINS     TO IT_21DAY-MEINS,
        SY-UNAME          TO IT_21DAY-ERNAM,
        SY-DATUM          TO IT_21DAY-ERDAT,
        SY-UZEIT          TO IT_21DAY-ERZET.
  IF     IT_MATNR-RAUBE = '11'.
    IT_21DAY-SHOP = 'B'.
  ELSEIF IT_MATNR-RAUBE = '12'.
    IT_21DAY-SHOP = 'P'.
  ELSEIF IT_MATNR-RAUBE = '13'.
    IT_21DAY-SHOP = 'T'.
  ELSEIF IT_MATNR-RAUBE = '14'.
    IT_21DAY-SHOP = 'E'.
  ENDIF.

ENDFORM.                    " APPEND_OTHER_FIELDS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.
  DELETE FROM ZTMM_PARTS_21DAY WHERE MATNR NE SPACE.

  INSERT ZTMM_PARTS_21DAY FROM TABLE IT_21DAY ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC EQ 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000(ZZ) WITH TEXT-M03.
  ELSE.
    ROLLBACK WORK.
    MESSAGE E000(ZZ) WITH TEXT-M04.
  ENDIF.
ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  change_body_part_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_BODY_PART_QTY.

*  reading the body input plan
  PERFORM READ_BODY_PLAN.
*
*  calculate the body part qty for items with status '00'.
  PERFORM CALCULATE_PART_QTY_00.

*  calculate the body part qty for items with status space
  PERFORM CALCULATE_BODY_PART_QTY.

*  modidy body part qty.
  PERFORM MODIFY_QTY.

ENDFORM.                    " change_body_part_qty
*&---------------------------------------------------------------------*
*&      Form  READ_BODY_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BODY_PLAN.

  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE IT_INPUT
   FROM ZTPP_INPUT_PLAN
   WHERE ( STATUS = '00' OR
         STATUS = SPACE ) AND
         RD01 GE P_DATUM  AND
         RD01 LE WA_LASTDATE.
  IF SY-SUBRC NE 0.
    MESSAGE S009 WITH 'No input plan data for body part calculation'.
  ENDIF.

* reading body parts
  CLEAR: IT_21DAY , IT_21DAY[].
  SELECT WERKS MATNR DATUM DISPO PROFL LGORT LIFNR SHOP
         PRVBE SORTF MEINS ERNAM ERDAT ERZET
    INTO CORRESPONDING FIELDS OF TABLE IT_21DAY
   FROM ZTMM_PARTS_21DAY
   WHERE SHOP  = 'B'
     OR  SORTF = '01'
     OR  SORTF = '02'.

* reduce the it_resb
  LOOP AT IT_RESB.
    READ TABLE IT_21DAY WITH KEY MATNR = IT_RESB-MATNR.
    IF SY-SUBRC NE 0.
      DELETE IT_RESB.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_BODY_PLAN
*&---------------------------------------------------------------------*
*&      Form  calculate_body_part_qty
*&---------------------------------------------------------------------*
*   For blank status cars, there is no planned order yet, so we need
*   to find the same FSC code and find each component qty in FSC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALCULATE_BODY_PART_QTY.
  DATA: L_FSC LIKE MARA-MATNR.
  DATA: WA_INPUT LIKE IT_INPUT.
  DATA: L_TABIX  LIKE SY-TABIX.
  DATA: BEGIN OF LT_FSC OCCURS 0,
          FSC  LIKE MARA-MATNR,
          PART LIKE MARA-MATNR,
          TDATE LIKE SY-DATUM,
          RD01 LIKE SY-DATUM,
          QTY  LIKE PLAF-GSMNG,
          PQTY LIKE PLAF-GSMNG,
          VER  LIKE PLAF-VERID,
        END OF LT_FSC.
  DATA: BEGIN OF LT_PLAF OCCURS 0,
          PLNUM  LIKE PLAF-PLNUM,
          MATNR  LIKE PLAF-MATNR,
          PSTTR  LIKE PLAF-PSTTR,
          GSMNG  LIKE PLAF-GSMNG,
          RSNUM  LIKE PLAF-RSNUM,
          VERID  LIKE PLAF-VERID,
        END OF LT_PLAF.
  DATA: LT_FSC1 LIKE LT_FSC OCCURS 0 WITH HEADER LINE.
  DATA: LT_INPUT LIKE IT_INPUT OCCURS 0 WITH HEADER LINE.
  DATA: WA_FSC LIKE LT_FSC.
  DATA: L_NUM(2) TYPE N.
  DATA: L_TEXT(14).
  DATA: L_VER(2).
  DATA: L_VERSION(3) TYPE N.
  DATA: L_DATE  LIKE SY-DATUM.
  DATA: L_LINE TYPE I.
  FIELD-SYMBOLS: <FIELD>.
*
* get the shceduled items with status space.
  LT_INPUT[] = IT_INPUT[].
  DELETE LT_INPUT WHERE STATUS = '00'.
  DESCRIBE TABLE LT_INPUT LINES L_LINE .
  IF L_LINE EQ 0.
    EXIT.
  ENDIF.


* read the fsc code
  LOOP AT LT_INPUT.
    SELECT SINGLE FSC INTO LT_INPUT-FSC
      FROM ZTPP_WOSUM
      WHERE WO_SER  = LT_INPUT-WORK_ORDER(9)
        AND NATION  = LT_INPUT-WORK_ORDER+9(3)
        AND DEALER  = LT_INPUT-WORK_ORDER+12(2)
        AND EXTC    = LT_INPUT-EXTC
        AND INTC    = LT_INPUT-INTC.
    MODIFY LT_INPUT.
  ENDLOOP.

* summarize by fsc and planed schedule date
  SORT LT_INPUT BY FSC VERS RSNUM.
  CLEAR: L_FSC.
  LOOP AT LT_INPUT.
    L_TABIX = SY-TABIX + 1.
    LT_FSC-QTY = LT_FSC-QTY + 1.
    CLEAR: WA_INPUT .
    READ TABLE LT_INPUT INTO WA_INPUT INDEX L_TABIX.
    IF  LT_INPUT-FSC  NE WA_INPUT-FSC   OR
       LT_INPUT-VERS  NE WA_INPUT-VERS  OR
       LT_INPUT-RD01 NE WA_INPUT-RD01.
      LT_FSC-FSC   = LT_INPUT-FSC.
      LT_FSC-TDATE = LT_INPUT-RSNUM.
      LT_FSC-RD01  = LT_INPUT-RD01.
      LT_FSC-VER   = LT_INPUT-VERS.
      APPEND LT_FSC.
      CLEAR: LT_FSC.
    ENDIF.
  ENDLOOP.
*
* read the planned order
  LOOP AT LT_FSC.
    L_VER = LT_FSC-VER+1(2).
    SELECT PLNUM MATNR PSTTR GSMNG RSNUM VERID
      APPENDING CORRESPONDING FIELDS OF TABLE LT_PLAF
      FROM PLAF
      WHERE MATNR = LT_FSC-FSC
        AND RSNUM NE '0'
        AND PSTTR GE P_DATUM
        AND VERID = L_VER
        AND STLFX NE 'X'.
*        and psttr le wa_lastdate.
  ENDLOOP.
  SORT LT_PLAF BY MATNR PSTTR VERID.
  DELETE ADJACENT DUPLICATES FROM LT_PLAF
    COMPARING MATNR PSTTR VERID.
*
* reduce the it_resb table
  SORT LT_PLAF BY RSNUM.
  LOOP AT IT_RESB .
    READ TABLE LT_PLAF WITH KEY RSNUM = IT_RESB-RSNUM
         BINARY SEARCH.
    IF SY-SUBRC NE 0.
      DELETE IT_RESB.
    ENDIF.
  ENDLOOP.
  SORT IT_RESB BY RSNUM MATNR.
  SORT LT_PLAF BY MATNR VERID.

* get the base body part qty for each fsc
* total qty = (base qty of each fsc)  *  (fsc qty)
  LOOP AT LT_FSC.
    MOVE-CORRESPONDING LT_FSC TO LT_FSC1.
    CLEAR: LT_PLAF.
    L_VER = LT_FSC-VER+1(2).
    READ TABLE LT_PLAF WITH KEY MATNR = LT_FSC-FSC
                                    VERID = L_VER
          BINARY SEARCH .
    IF SY-SUBRC NE 0.   " no planned order
      CONTINUE.
    ENDIF.
    LOOP AT IT_21DAY.

      CLEAR: IT_RESB.

      READ TABLE IT_RESB WITH KEY RSNUM = LT_PLAF-RSNUM
                                  MATNR = IT_21DAY-MATNR
           BINARY SEARCH.
      IF SY-SUBRC = 0.
*        MOVE-CORRESPONDING lt_fsc TO lt_fsc1.
        LT_FSC1-PART = IT_21DAY-MATNR.
        LT_FSC1-PQTY = IT_RESB-BDMNG /
                       LT_PLAF-GSMNG *
                       LT_FSC-QTY .
        APPEND LT_FSC1.
*         else.   "no part reservation
*           move-corResponding lt_fsc to lt_fsc1.
*           lt_fsc1-part = it_21day-matnr.
*           lt_fsc1-pqty = 0.
*           append lt_fsc1.
      ENDIF.
*       else.     "no plan order
*         move-corResponding lt_fsc to lt_fsc1.
*         lt_fsc1-part = it_21day-matnr.
*         lt_fsc1-pqty = 0.
*         append lt_fsc1.
*       endif.
    ENDLOOP.
  ENDLOOP.

* sumarize the date-part qty
  SORT LT_FSC1 BY PART RD01.
  CLEAR: LT_FSC, LT_FSC[].

  LOOP AT LT_FSC1.
    L_TABIX = SY-TABIX + 1.
    LT_FSC-PART = LT_FSC1-PART.
    LT_FSC-RD01 = LT_FSC1-RD01.
    LT_FSC-PQTY = LT_FSC-PQTY + LT_FSC1-PQTY.
    CLEAR: WA_FSC.
    READ TABLE LT_FSC1 INTO WA_FSC INDEX L_TABIX.
    IF LT_FSC1-PART NE WA_FSC-PART OR
       LT_FSC1-RD01 NE WA_FSC-RD01.
      APPEND LT_FSC.
      CLEAR: LT_FSC.
    ENDIF.
  ENDLOOP.

* TRANSFER the part qty TO internal table
  LOOP AT IT_21DAY.
    LOOP AT IT_DAY.
      IF SY-TABIX GT 21.
        EXIT.
      ENDIF.
      L_NUM = IT_DAY-SEQ.
      CONCATENATE 'IT_21DAY-D' L_NUM INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FIELD>.
      CLEAR: LT_FSC.
      READ TABLE LT_FSC WITH KEY PART = IT_21DAY-MATNR
                                 RD01 = IT_DAY-DATUM.
      IF SY-SUBRC = 0 .
        <FIELD> = <FIELD> + LT_FSC-PQTY.
      ELSE.
*        MOVE 0           TO <FIELD>.
      ENDIF.
    ENDLOOP.
    MODIFY IT_21DAY.
    CLEAR: IT_21DAY.
  ENDLOOP.

ENDFORM.                    " calculate_body_part_qty
*&---------------------------------------------------------------------*
*&      Form  modify_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_QTY.
  CHECK NOT IT_21DAY[] IS INITIAL.
  UPDATE ZTMM_PARTS_21DAY FROM TABLE IT_21DAY.
  IF SY-SUBRC = 0.
    COMMIT WORK AND WAIT.
    MESSAGE S000 WITH 'Body part qty updated successfully'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S000 WITH 'Body part qty update failed'.
  ENDIF.
ENDFORM.                    " modify_qty
*&---------------------------------------------------------------------*
*&      Form  set_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DAYS.
  DATA: L_COUNT TYPE I.
  DATA: L_DATE LIKE SY-DATUM.

* reading working calendar
  PERFORM READ_SHOP_CALID  USING WA_KALID.
* first is current inputed date
  IT_DAY-SEQ = 1.
  IT_DAY-DATUM = P_DATUM.
  APPEND IT_DAY.
  L_COUNT = 1.
  L_DATE = P_DATUM .
  DO 20 TIMES.
    L_COUNT  = L_COUNT + 1.
    L_DATE   = L_DATE  + 1.
    PERFORM READ_WORKING_DATE USING '+'  WA_KALID  L_DATE.
    IT_DAY-SEQ     = L_COUNT.
    IT_DAY-DATUM   = L_DATE .
    APPEND IT_DAY.  CLEAR: IT_DAY.
  ENDDO.
  WA_LASTDATE = L_DATE .
ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM READ_SHOP_CALID USING    PA_KALID.
  SELECT SINGLE KALID INTO PA_KALID
    FROM ZVPP_CAPACITY
   WHERE ARBPL = 'T'   .
ENDFORM.                    " READ_SHOP_CALID
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM READ_WORKING_DATE USING  PA_TYPE  PA_KALID  PA_WDATE.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            CORRECT_OPTION               = PA_TYPE
            DATE                         = PA_WDATE
            FACTORY_CALENDAR_ID          = PA_KALID
       IMPORTING
            DATE                         = PA_WDATE
       EXCEPTIONS
            CALENDAR_BUFFER_NOT_LOADABLE = 1
            CORRECT_OPTION_INVALID       = 2
            DATE_AFTER_RANGE             = 3
            DATE_BEFORE_RANGE            = 4
            DATE_INVALID                 = 5
            FACTORY_CALENDAR_NOT_FOUND   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  calculate_part_qty_00
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALCULATE_PART_QTY_00.
  DATA: LT_INPUT LIKE IT_INPUT OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF LT_RSNUM OCCURS 0,
         PLNUM LIKE PLAF-PLNUM,
         RSNUM LIKE RESB-RSNUM,
        END OF LT_RSNUM.
  DATA: LT_RESB  LIKE IT_RESB OCCURS 0 WITH HEADER LINE.
  DATA: LT_RESB1 LIKE IT_RESB OCCURS 0 WITH HEADER LINE.
  DATA: WA_RESB  LIKE IT_RESB.
  DATA: L_TABIX  LIKE SY-TABIX.
  DATA: L_NUM(2) TYPE N.
  DATA: L_TEXT(15).
  DATA: L_LINE TYPE I.
  FIELD-SYMBOLS: <FIELD>.

* get the items with status '00'.
  LT_INPUT[] = IT_INPUT[].
  DELETE LT_INPUT WHERE STATUS = SPACE.
  DESCRIBE TABLE LT_INPUT LINES L_LINE .
  IF L_LINE EQ 0.
    EXIT.
  ENDIF.

* read the reservation number from plaf
  SELECT PLNUM RSNUM  INTO TABLE LT_RSNUM
    FROM PLAF
    FOR ALL ENTRIES IN LT_INPUT
    WHERE PLNUM = LT_INPUT-PLNUM.

* get body parts with the correct reservation number
  LOOP AT IT_RESB.
    CLEAR: LT_RSNUM.
    READ TABLE LT_RSNUM WITH KEY RSNUM = IT_RESB-RSNUM.
    IF SY-SUBRC EQ 0.
      "**get the body input plan date
      READ TABLE LT_INPUT WITH KEY PLNUM = LT_RSNUM-PLNUM.
      IT_RESB-BDTER = LT_INPUT-RD01.
      APPEND IT_RESB TO LT_RESB.
    ENDIF.
  ENDLOOP.

  SORT LT_RESB BY MATNR BDTER.
*
* summarize the qty by part and date
  LOOP AT LT_RESB.
    L_TABIX = SY-TABIX + 1.
    LT_RESB1-BDMNG = LT_RESB1-BDMNG + LT_RESB-BDMNG.
    CLEAR: WA_RESB.
    READ TABLE LT_RESB INTO WA_RESB INDEX L_TABIX.
    IF LT_RESB-MATNR NE WA_RESB-MATNR OR
       LT_RESB-BDTER NE WA_RESB-BDTER.
      LT_RESB1-MATNR = LT_RESB-MATNR.
      LT_RESB1-BDTER = LT_RESB-BDTER.
      APPEND LT_RESB1.
      CLEAR: LT_RESB1.
    ENDIF.
  ENDLOOP.

* tranfer the qty to internal table it_21day.
  LOOP AT IT_21DAY.
    LOOP AT IT_DAY.
      IF SY-TABIX GT 21.
        EXIT.
      ENDIF.
      L_NUM = IT_DAY-SEQ.
      CONCATENATE 'IT_21DAY-D' L_NUM INTO L_TEXT.
      ASSIGN (L_TEXT) TO <FIELD>.
      CLEAR: LT_RESB1.
      READ TABLE LT_RESB1 WITH KEY MATNR = IT_21DAY-MATNR
                                   BDTER = IT_DAY-DATUM.
      IF SY-SUBRC = 0 .
        <FIELD> = <FIELD> + LT_RESB1-BDMNG.
      ELSE.
*        MOVE 0           TO <FIELD>.
      ENDIF.
    ENDLOOP.
    MODIFY IT_21DAY.
    CLEAR: IT_21DAY.
  ENDLOOP.


ENDFORM.                    " calculate_part_qty_00
*&---------------------------------------------------------------------*
*&      Form  read_resb_21times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_RESB_21TIMES.
  DATA: LW_DATE LIKE SY-DATUM.

  LOOP AT IT_DAY.
    CLEAR: IT_RESB_21, IT_RESB_21[].
    LW_DATE = IT_DAY-DATUM.
    SELECT A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
           A~RSNUM A~SORTF A~PRVBE
           SUM( BDMNG ) AS BDMNG
      INTO CORRESPONDING FIELDS OF TABLE IT_RESB_21
      FROM RESB AS A INNER JOIN MARA AS B
                        ON A~MATNR = B~MATNR
*                    AND b~mtart = 'ROH'
     WHERE A~BDTER = LW_DATE
*     AND A~BDTER <= WA_LASTDATE
       AND A~LGORT IN R_LGORT
*     AND a~werks EQ p_werks "COMMENT BY CHRIS ON 06/28/2005
       AND ( B~MTART = 'ROH' OR
             B~MTART = 'HALB' )
       AND A~XLOEK EQ SPACE
       AND A~KZEAR >= ' '
     GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
           A~RSNUM A~SORTF A~PRVBE.

    IF NOT IT_RESB_21[] IS INITIAL.
      APPEND LINES OF IT_RESB_21 TO IT_RESB.
    ENDIF.

  ENDLOOP.
  IF IT_RESB[] IS INITIAL.
    MESSAGE E000(ZZ) WITH TEXT-M02.
  ENDIF.

  SORT IT_RESB BY MATNR.

ENDFORM.                    " read_resb_21times
*&---------------------------------------------------------------------*
*&      Form  read_resb_plaf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_RESB_PLAF.
  SELECT A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
         A~RSNUM A~SORTF A~PRVBE
         SUM( A~BDMNG ) AS BDMNG
    INTO CORRESPONDING FIELDS OF TABLE IT_RESB
    FROM PLAF AS C
    INNER JOIN RESB AS A
    ON C~RSNUM = A~RSNUM
    INNER JOIN MARA AS B
    ON A~MATNR = B~MATNR
   WHERE C~DISPO IN ('V01', 'ME1')
     AND C~PSTTR >= P_DATUM
     AND A~LGORT IN R_LGORT
     AND C~PLSCN = ' '
     AND ( B~MTART = 'ROH' OR
           B~MTART = 'HALB' )
     AND A~XLOEK EQ SPACE
     AND A~KZEAR >= ' '
   GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
         A~RSNUM A~SORTF A~PRVBE.

  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M02.
  ENDIF.
  SORT IT_RESB BY MATNR.
ENDFORM.                    " read_resb_plaf
*&---------------------------------------------------------------------*
*&      Form  append_passdue_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_PASSDUE_QTY.
  READ TABLE IT_BFD WITH KEY MATNR = IT_RESB-MATNR
                             WERKS = IT_RESB-WERKS.
  IF SY-SUBRC = 0.
    IT_21DAY-BFD = IT_BFD-BDMNG.
  ENDIF.
ENDFORM.                    " append_passdue_qty
*&---------------------------------------------------------------------*
*&      Form  read_resb_bfd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_RESB_BFD.
  DATA: LT_BFD LIKE TABLE OF IT_BFD WITH HEADER LINE.

  SELECT A~MATNR A~WERKS
         SUM( A~BDMNG ) AS BDMNG
    INTO TABLE LT_BFD
    FROM PLAF AS C
    INNER JOIN RESB AS A
    ON C~RSNUM = A~RSNUM
    INNER JOIN MARA AS B
    ON A~MATNR = B~MATNR
   WHERE C~DISPO IN ('V01', 'ME1')
     AND C~PSTTR < P_DATUM
     AND A~LGORT IN R_LGORT
     AND C~PLSCN = ' '
     AND ( B~MTART = 'ROH' OR
           B~MTART = 'HALB' )
     AND A~ENMNG = 0
     AND A~XLOEK EQ SPACE
     AND A~KZEAR >= ' '
   GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
         A~RSNUM A~SORTF A~PRVBE.

*  SELECT matnr werks bdmng INTO TABLE lt_bfd
*    FROM resb
*    for all entries in it_matnr
*  WHERE matnr = it_matnr-matnr
*    and bdter < p_datum
*    AND enmng = 0
*    AND xloek EQ space.
*
*  SELECT b~matnr b~werks b~bdmng
*    INTO CORRESPONDING FIELDS OF TABLE lt_bfd
*    FROM ztpp_input_plan AS a
*    INNER JOIN resb AS b
*    ON a~plnum = b~plnum
*     where a~status between '06' and '17'
*     AND b~xloek EQ space
*     and b~enmng = 0.

  LOOP AT LT_BFD.
    IT_BFD = LT_BFD.
*    it_bfd-bal = it_bfd-bdmng - it_bfd-enmng.
    COLLECT IT_BFD.
    CLEAR: IT_BFD, LT_BFD.
  ENDLOOP.

ENDFORM.                    " read_resb_bfd
*&---------------------------------------------------------------------*
*&      Form  read_zresb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ZRESB.
  DATA: WA_DATUM_30 LIKE SY-DATUM.
  DATA: W_MATNR LIKE IT_RESB-MATNR,
        W_WERKS LIKE IT_RESB-WERKS,
        W_BDTER LIKE IT_RESB-BDTER,
        W_RSNUM LIKE IT_RESB-RSNUM.

  WA_DATUM_30 = P_DATUM + 30.
  IF P_TEST = 'X'.
    SELECT A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
         A~RSNUM A~SORTF A~PRVBE
         SUM( BDMNG ) AS BDMNG
    INTO CORRESPONDING FIELDS OF TABLE IT_RESB
    FROM ZRESB_VIEW AS A INNER JOIN MARA AS B
                      ON A~MATNR = B~MATNR
   WHERE A~BDTER >= P_DATUM
     AND A~LGORT IN R_LGORT
     AND A~MATNR IN S_MATNR
   GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
         A~RSNUM A~SORTF A~PRVBE.
  ELSE.
    WRITE: / 'Starting selection for zresb-view (1)'.
    SELECT A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
           A~RSNUM A~SORTF A~PRVBE
            SUM( BDMNG ) AS BDMNG
       INTO CORRESPONDING FIELDS OF TABLE IT_RESB
       FROM ZRESB_VIEW AS A INNER JOIN MARA AS B
            ON ( A~MATNR = B~MATNR AND A~MANDT = B~MANDT )
      WHERE A~BDTER >= P_DATUM
        AND A~LGORT IN R_LGORT
        AND ( B~MTART = 'ROH' OR
              B~MTART = 'HALB' )
      GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
            A~RSNUM A~SORTF A~PRVBE.
    WRITE: / 'Ending selection for zresb-view (1)'.
** for requirement date > current date
    WRITE: / 'Starting selection for resb (2)'.
    SELECT A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
            A~RSNUM A~SORTF A~PRVBE
             SUM( BDMNG ) AS BDMNG
        APPENDING CORRESPONDING FIELDS OF TABLE IT_RESB
        FROM RESB AS A INNER JOIN MARA AS B
                       ON A~MATNR = B~MATNR
       WHERE A~XLOEK = ' '
         AND A~BDART = 'SB'
         AND A~POSTP = 'L'
         AND A~ENMNG <> 0
         AND A~BDTER BETWEEN P_DATUM AND WA_DATUM_30
         AND A~LGORT IN R_LGORT
         AND ( B~MTART = 'ROH' OR
               B~MTART = 'HALB' )
       GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
             A~RSNUM A~SORTF A~PRVBE.
    WRITE: / 'Ending selection for resb (2)'.
    SORT IT_RESB BY MATNR WERKS BDTER RSNUM.
    CLEAR: W_MATNR, W_WERKS, W_BDTER, W_RSNUM.

    LOOP AT IT_RESB.
      IF IT_RESB-MATNR = W_MATNR
        AND IT_RESB-WERKS = W_WERKS
        AND IT_RESB-BDTER = W_BDTER
        AND IT_RESB-RSNUM = W_RSNUM.
        DELETE IT_RESB.
      ELSE.
        W_MATNR = IT_RESB-MATNR.
        W_WERKS = IT_RESB-WERKS.
        W_BDTER = IT_RESB-BDTER.
        W_RSNUM = IT_RESB-RSNUM.
      ENDIF.
    ENDLOOP.
**
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M02.
  ENDIF.
  SORT IT_RESB BY MATNR.
ENDFORM.                    " read_zresb
*&---------------------------------------------------------------------*
*&      Form  read_zresb_bfd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ZRESB_BFD.
  DATA: LT_BFD LIKE TABLE OF IT_BFD WITH HEADER LINE.
  IF P_TEST = 'X'.
    SELECT A~MATNR A~WERKS
          SUM( A~BDMNG ) AS BDMNG
     INTO TABLE LT_BFD
     FROM ZRESB_VIEW AS A
     INNER JOIN MARA AS B
     ON A~MATNR = B~MATNR
    WHERE BDTER < P_DATUM
      AND A~LGORT IN R_LGORT
      AND A~MATNR IN S_MATNR
      GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
          A~SORTF A~PRVBE.
  ELSE.
    WRITE: / 'Starting selection for zresb-view (2)'.
    SELECT A~MATNR A~WERKS
           SUM( A~BDMNG ) AS BDMNG
      INTO TABLE LT_BFD
      FROM ZRESB_VIEW AS A
      INNER JOIN MARA AS B
            ON ( A~MATNR = B~MATNR AND A~MANDT = B~MANDT )
     WHERE BDTER < P_DATUM
       AND A~LGORT IN R_LGORT
       AND ( B~MTART = 'ROH' OR
             B~MTART = 'HALB' )
       GROUP BY A~MATNR A~WERKS A~BDTER A~LGORT A~MEINS
           A~SORTF A~PRVBE.
    WRITE: / 'Ending selection for zresb-view (1)'.
  ENDIF.
  LOOP AT LT_BFD.
    IT_BFD = LT_BFD.
    COLLECT IT_BFD.
    CLEAR: IT_BFD, LT_BFD.
  ENDLOOP.
ENDFORM.                    " read_zresb_bfd
