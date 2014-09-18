*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'INQ'.
      PERFORM SELECT_DATA.
    WHEN 'EXIT'.
      CLEAR: W_NEW, W_REFRESH.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: W_NEW, W_REFRESH.
      LEAVE TO SCREEN 0.
    WHEN 'CAL'.
      IF SY-TCODE = 'ZAPP_ENG_PIR' AND W_PRDT <> '2'.
        PERFORM RECAL_STOCK.
      ELSE.
        MESSAGE I001 WITH 'No Authorization to Access'.
      ENDIF.
    WHEN 'REF'.
      W_REFRESH = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'REP'.
      IF SY-TCODE = 'ZAPP_ENG_PIR' AND W_PRDT <> '2'.
        PERFORM REPLACE_PIR_QTY.
      ELSE.
        MESSAGE I001 WITH 'No Authorization to Access'.
      ENDIF.

*      PERFORM RECAL_STOCK.
    WHEN 'SAVE'.
      IF SY-TCODE = 'ZAPP_ENG_PIR' AND W_PRDT <> '2'.
        PERFORM SAVE_DATA.
      ELSE.
        MESSAGE I001 WITH 'No Authorization to Access'.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_DATA.
  DATA: LT_ENG_PIR LIKE TABLE OF ZTPP_ENG_PIR WITH HEADER LINE.
  DATA: L_CN(2) TYPE N,
        L_QTY1(13),
        L_INDEX LIKE SY-TABIX,
        L_TEXT(35).
  DATA: LS_OUTPUT LIKE IT_OUTPUT.

  REFRESH LT_ENG_PIR.
  LOOP AT IT_OUTPUT.
    L_INDEX = SY-TABIX - 2.
    IF IT_OUTPUT-SEQ = '3'.
      LT_ENG_PIR-MATNR = IT_OUTPUT-MATNR.
** for E002
*      LT_ENG_PIR-WERKS = C_WERKS.
       LT_ENG_PIR-WERKS = w_WERKS.
** END
      LT_ENG_PIR-WDATU = Z_BEG_DATE.
      LT_ENG_PIR-LAEDA = SY-DATUM.
      LT_ENG_PIR-AENAM = SY-UNAME.
      READ TABLE IT_OUTPUT INTO LS_OUTPUT INDEX L_INDEX.
      LOOP AT IT_DAY.
        CONCATENATE 'LS_OUTPUT-QTYD_' IT_DAY-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          LT_ENG_PIR-VEHREQ = <FS01>.
        ENDIF.
        CONCATENATE 'IT_OUTPUT-QTYD_' IT_DAY-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          LT_ENG_PIR-PLNMG = <FS01>.
        ENDIF.
        LT_ENG_PIR-ENTLU = '1'.
        LT_ENG_PIR-PDATU = IT_DAY-DATUM.
        APPEND LT_ENG_PIR.
      ENDLOOP.

      LOOP AT IT_WEEK.
        CONCATENATE 'LS_OUTPUT-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          LT_ENG_PIR-VEHREQ = <FS01>.
        ENDIF.
        CONCATENATE 'IT_OUTPUT-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          LT_ENG_PIR-PLNMG = <FS01>.
        ENDIF.
        LT_ENG_PIR-ENTLU = '2'.
        LT_ENG_PIR-PDATU = IT_WEEK-DATUM.
        APPEND LT_ENG_PIR.
      ENDLOOP.
      CLEAR: LT_ENG_PIR.
    ENDIF.
  ENDLOOP.
  MODIFY ZTPP_ENG_PIR FROM TABLE LT_ENG_PIR.
  IF SY-SUBRC = 0.
    COMMIT WORK.
    MESSAGE S000(ZZ) WITH TEXT-M16.
  ELSE.
    ROLLBACK WORK.
    MESSAGE S000(ZZ) WITH TEXT-M17.
  ENDIF.
ENDFORM.                    " SAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  REPLACE_PIR_QTY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REPLACE_PIR_QTY.
  DATA: LT_CELL TYPE LVC_T_CELL.
  DATA: LW_CELL TYPE LVC_S_CELL.
  DATA: WA_ROW TYPE LVC_S_ROW,
        WA_COL TYPE LVC_S_COL,
        WA_STBL TYPE LVC_S_STBL.
  DATA: L_LINE TYPE I,
        L_TEXT(35),
        L_CN(2) TYPE N,
        L_DW(2),
        L_INDEX LIKE SY-TABIX,
        L_QTY LIKE IT_OUTPUT-QTYD_01.

  CALL METHOD ALV_GRID->GET_SELECTED_CELLS
           IMPORTING ET_CELL = LT_CELL.
  IF LT_CELL[] IS INITIAL.
  ELSE.
    READ TABLE LT_CELL INTO LW_CELL INDEX 1.
    WA_ROW = LW_CELL-ROW_ID.
    WA_COL = LW_CELL-COL_ID.
  ENDIF.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  L_CN = WA_COL-FIELDNAME+5(2).
  L_DW = WA_COL-FIELDNAME+3(2).
  IF L_CN < '00' OR L_CN > '21'.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  L_INDEX = WA_ROW-INDEX - 2.
  READ TABLE IT_OUTPUT INDEX L_INDEX.
  IF SY-SUBRC NE 0 OR IT_OUTPUT-SEQ <> '1'.
    MESSAGE E000(ZZ) WITH TEXT-M13.
  ENDIF.
  CONCATENATE 'IT_OUTPUT-QTY' L_DW L_CN INTO L_TEXT.
  ASSIGN (L_TEXT) TO <FS01>.
  IF SY-SUBRC = 0.
    L_QTY = <FS01>.
    READ TABLE IT_OUTPUT INDEX WA_ROW-INDEX.
    CONCATENATE 'IT_OUTPUT-QTY' L_DW L_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <FS-QTY>.
    IF SY-SUBRC = 0.
      <FS-QTY> = L_QTY.
      MODIFY IT_OUTPUT INDEX WA_ROW-INDEX.
      WA_STBL-ROW = 'X'.
      WA_STBL-COL = 'X'.
      CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = WA_STBL.

      MESSAGE S000(ZZ) WITH 'Data successfully replaced'.
    ENDIF.
  ELSE.
    MESSAGE S000(ZZ) WITH 'Replacement Error'.
  ENDIF.
ENDFORM.                    " REPLACE_PIR_QTY
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ITAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_ITAB.
  DATA: L_QTY TYPE I,
       L_CN(2) TYPE N,
       L_QTY1 LIKE IT_TAB-QTYD_01,
       L_TEXT(35),
       L_INDEX LIKE SY-TABIX.

  LOOP AT IT_OUTPUT.
    IF IT_OUTPUT-SEQ = '3'.
      L_INDEX = SY-TABIX.
      READ TABLE IT_TAB INDEX L_INDEX.
      L_CN = '00'.
      DO 21 TIMES.
        L_CN =  L_CN + 1.
        CONCATENATE 'IT_OUTPUT-QTYD_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          L_QTY = <FS01>.
          L_QTY1 = L_QTY.
        ENDIF.
        CONCATENATE 'IT_TAB-QTYD_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          <FS-QTY> = L_QTY1.
        ENDIF.
      ENDDO.
      L_CN = '03'.
      DO 18 TIMES.
        L_CN =  L_CN + 1.
        CONCATENATE 'IT_OUTPUT-QTYW_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.
        IF SY-SUBRC = 0.
          L_QTY = <FS01>.
          L_QTY1 = L_QTY.
        ENDIF.
        CONCATENATE 'IT_TAB-QTYW_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          <FS-QTY> = L_QTY1.
        ENDIF.
      ENDDO.
      MODIFY IT_TAB INDEX L_INDEX..
    ENDIF.
  ENDLOOP.
ENDFORM.                    " UPDATE_ITAB


*&---------------------------------------------------------------------*
*&      Form  RECAL_STOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM RECAL_STOCK.
  PERFORM UPDATE_ITAB.
  PERFORM UPDATE_STOCK.
ENDFORM.                    " RECAL_STOCK

*&---------------------------------------------------------------------*
*&      Form  UPDATE_STOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_STOCK.
  DATA: WA_TAB1 LIKE IT_TAB,
        WA_TAB2 LIKE IT_TAB,
        WA_TAB3 LIKE IT_TAB,
        L_INDEX LIKE SY-TABIX,
        L_CURR LIKE SY-TABIX,
        L_CN(2) TYPE N,
        L_TEXT(30).

  FIELD-SYMBOLS: <L_REQ>, <L_STOCK>,<L_PIR>.

  SORT IT_TAB BY MATNR SEQ.
  LOOP AT IT_TAB INTO WA_TAB2.

    IF WA_TAB2-SEQ = '2'.
      L_CURR = SY-TABIX.
      L_INDEX = SY-TABIX - 1.
      READ TABLE IT_TAB INTO WA_TAB1 INDEX L_INDEX.
      L_INDEX  = L_INDEX + 2.
      READ TABLE IT_TAB INTO WA_TAB3 INDEX L_INDEX.

      LOOP AT IT_DAY FROM 2 .
        L_CN = IT_DAY-SEQ - 1.
        CONCATENATE 'WA_TAB1-QTYD_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_REQ>.
        IF SY-SUBRC = 0.
          CONCATENATE 'WA_TAB2-QTYD_'  L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <L_STOCK>.
          IF SY-SUBRC = 0.
            CONCATENATE 'WA_TAB3-QTYD_' L_CN INTO L_TEXT.
            ASSIGN (L_TEXT) TO <L_PIR>.
            IF SY-SUBRC = 0.
              CONCATENATE 'WA_TAB2-QTYD_' IT_DAY-SEQ INTO L_TEXT.
              ASSIGN (L_TEXT) TO <FS-QTY>.
              IF SY-SUBRC = 0.
                <FS-QTY> = <L_STOCK> + <L_PIR> - <L_REQ>.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CONCATENATE 'WA_TAB1-QTYD_' W_MAX_DAY_CN INTO L_TEXT.
      ASSIGN (L_TEXT) TO <L_REQ>.
      IF SY-SUBRC = 0.
        CONCATENATE 'WA_TAB2-QTYD_'  W_MAX_DAY_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_STOCK>.
        CONCATENATE 'WA_TAB3-QTYD_' W_MAX_DAY_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_PIR>.
        IF SY-SUBRC = 0.
          WA_TAB2-QTYW_04 = <L_STOCK> + <L_PIR> - <L_REQ>.
        ENDIF.
      ENDIF.
      LOOP AT IT_WEEK FROM 2.
        L_CN = IT_WEEK-SEQ - 1.
        CONCATENATE 'WA_TAB1-QTYW_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_REQ>.
        IF SY-SUBRC = 0.
          CONCATENATE 'WA_TAB2-QTYW_'  L_CN INTO L_TEXT.
          ASSIGN (L_TEXT) TO <L_STOCK>.
          IF SY-SUBRC = 0.
            CONCATENATE 'WA_TAB3-QTYW_' L_CN INTO L_TEXT.
            ASSIGN (L_TEXT) TO <L_PIR>.
            IF SY-SUBRC = 0.
              CONCATENATE 'WA_TAB2-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
              ASSIGN (L_TEXT) TO <FS-QTY>.
              IF SY-SUBRC = 0.
                <FS-QTY> = <L_STOCK> + <L_PIR> - <L_REQ>.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
*      IT_TAB-IF = 'C210'.
*    ENDIF.

      MODIFY IT_TAB FROM WA_TAB2 INDEX L_CURR.

    ENDIF.
  ENDLOOP.
  SORT IT_TAB BY MATNR SEQ.
  PERFORM PREPARE_DISPLAY.
  WA_STBL-ROW = 'X'.
  WA_STBL-COL = 'X'.
  CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
   EXPORTING IS_STABLE = WA_STBL.

  MESSAGE S000(ZZ) WITH 'Data recalculated'.
ENDFORM.                    " UPDATE_STOCK
*&---------------------------------------------------------------------*
*&      Form  GET_INDEL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_INDEL_DATA.
  DATA: BEGIN OF LT_INDEL OCCURS 0,
        MATNR LIKE MARA-MATNR,
        EINDT LIKE EKES-EINDT, "LFDAT
        MENGE LIKE EKES-MENGE, "LFIMG
        DABMG LIKE EKES-DABMG,
        END OF LT_INDEL.
  DATA: LT_INDEL_TEMP LIKE TABLE OF EKES WITH HEADER LINE.


*  DATA: IT_ENG_PIR LIKE TABLE OF ZTPP_ENG_PIR WITH HEADER LINE,
  DATA: LT_ENG_STOCK LIKE TABLE OF ZTPP_ENG_STOCK WITH HEADER LINE.
  DATA: L_MATNR LIKE MARA-MATNR,
        L_CN(2) TYPE N,
        L_MAX_CN(2) TYPE N,
        L_TEXT(30),
        L_MAX_DATE LIKE SY-DATUM,
        L_INDEX LIKE SY-TABIX.
  DATA: WA_TAB1 LIKE IT_TAB,
        WA_TAB3 LIKE IT_TAB.

  FIELD-SYMBOLS : <L_STOCK>, <L_PIR>, <L_REQ>.

*  SELECT * INTO TABLE IT_ENG_PIR
*   FROM ZTPP_ENG_PIR
*   FOR ALL ENTRIES IN IT_TAB_TEMP
*   WHERE MATNR  = IT_TAB_TEMP-MATNR
*     AND WDATU = W_DATUM.
*
*  LOOP AT IT_TAB_TEMP.
*    CLEAR: WA_TAB1, WA_TAB3, IT_TAB, IT_ENG_PIR.
*    REFRESH LT_ENG_STOCK.
*
*    READ TABLE IT_TAB WITH KEY MATNR = IT_TAB_TEMP-MATNR.
*    WA_TAB1 = IT_TAB.
*    CLEAR: IT_TAB.
*
*    LOOP AT IT_DAY.
*      READ TABLE IT_ENG_PIR WITH KEY MATNR = IT_TAB_TEMP-MATNR
*                                     PDATU = IT_DAY-DATUM
*                                     ENTLU = '1'.
*      IF SY-SUBRC = 0.
*        CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS01>.
*        IF SY-SUBRC = 0.
*          <FS01> = IT_ENG_PIR-PLNMG.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    LOOP AT IT_WEEK.
*      READ TABLE IT_ENG_PIR WITH KEY MATNR = IT_TAB_TEMP-MATNR
*                                     PDATU = IT_WEEK-DATUM
*                                     ENTLU = '2'.
*      IF SY-SUBRC = 0.
*        CONCATENATE 'IT_TAB-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
*        ASSIGN (L_TEXT) TO <FS01>.
*        IF SY-SUBRC = 0.
*          <FS01> = IT_ENG_PIR-PLNMG.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

** SELECT A~LIFNR B~MATNR D~EINDT AS LFDAT D~MENGE AS LFIMG D~DABMG
**   INTO CORRESPONDING FIELDS OF TABLE LT_LIPS_TEMP
**   FROM EKKO AS A INNER JOIN EKPO AS B
**   ON A~EBELN = B~EBELN
**   INNER JOIN EKES AS D
**   ON D~EBELN = B~EBELN
**   AND D~EBELP = B~EBELP
**   INNER JOIN VBUK AS C
**   ON D~VBELN = C~VBELN
**   WHERE LIFNR IN S_LIFNR
**     AND MATNR IN S_MATNR
**     AND B~WERKS = P_WERKS
**     AND C~WBSTK = 'A'
**     AND C~VBTYP = '7'
**     AND D~EINDT <= L_END_DATE.

  CLEAR: IT_TAB.
  L_MAX_DATE = Z_MAX_WEEK + 6.
  SELECT D~EBELN D~EBELP ETENS B~MATNR D~EINDT D~MENGE D~DABMG
     INTO CORRESPONDING FIELDS OF TABLE LT_INDEL_TEMP
*   FROM EKKO AS A INNER JOIN EKPO AS B
*   ON A~EBELN = B~EBELN
     FROM EKPO AS B
     INNER JOIN EKES AS D
     ON D~EBELN = B~EBELN
     AND D~EBELP = B~EBELP
     INNER JOIN VBUK AS C
     ON D~VBELN = C~VBELN
     FOR ALL ENTRIES IN IT_TAB_TEMP
     WHERE B~MATNR = IT_TAB_TEMP-MATNR
       AND C~WBSTK = 'A'
       AND C~VBTYP = '7'
       AND D~EINDT BETWEEN Z_BEG_DATE AND L_MAX_DATE.

  LOOP AT LT_INDEL_TEMP.
    MOVE-CORRESPONDING LT_INDEL_TEMP TO LT_INDEL.
    LT_INDEL-MENGE = LT_INDEL-MENGE - LT_INDEL-DABMG.
    COLLECT LT_INDEL.
    CLEAR: LT_INDEL,LT_INDEL_TEMP.
  ENDLOOP.

  CLEAR: LT_INDEL_TEMP[].
  DESCRIBE TABLE IT_WEEK LINES L_MAX_CN.
*  IF NOT LT_INDEL[] IS INITIAL.
  SORT LT_INDEL BY MATNR.
*    READ TABLE LT_INDEL INDEX 1.
*    L_MATNR = LT_INDEL-MATNR.
  LOOP AT IT_TAB_TEMP.
    CLEAR: WA_TAB1, WA_TAB3, IT_TAB, LT_INDEL.
    REFRESH LT_ENG_STOCK.

    READ TABLE IT_TAB WITH KEY MATNR = IT_TAB_TEMP-MATNR.
    WA_TAB1 = IT_TAB.
    CLEAR: IT_TAB.

    LOOP AT LT_INDEL WHERE MATNR = IT_TAB_TEMP-MATNR.
      IF LT_INDEL-EINDT < Z_BEG_WEEK.
        READ TABLE IT_DAY WITH KEY DATUM = LT_INDEL-EINDT.
        IF SY-SUBRC = 0.
          CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
          ASSIGN (L_TEXT) TO <FS01>.
          IF SY-SUBRC = 0.
            <FS01> = <FS01> + LT_INDEL-MENGE.
          ENDIF.
        ELSE.
          PERFORM READ_WORKING_DATE USING '-' L_KALID LT_INDEL-EINDT.
          READ TABLE IT_DAY WITH KEY DATUM = LT_INDEL-EINDT.
          IF SY-SUBRC = 0.
            CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
            ASSIGN (L_TEXT) TO <FS01>.
            IF SY-SUBRC = 0.
              <FS01> = <FS01> + LT_INDEL-MENGE.
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        CLEAR: L_INDEX.
        LOOP AT IT_WEEK.
          IF IT_WEEK-DATUM > LT_INDEL-EINDT.
            L_INDEX = SY-TABIX - 1.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF L_INDEX IS INITIAL.
*            if LT_indel-EINDT > z_max_week.
          L_CN = L_MAX_CN.
        ELSE.
          L_CN = L_INDEX.
        ENDIF.
        CONCATENATE 'IT_TAB-QTYD_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS01>.

        IF SY-SUBRC = 0.
          <FS01> = <FS01> + LT_INDEL-MENGE.
        ENDIF.
      ENDIF.
    ENDLOOP.



    IT_TAB-MATNR = IT_TAB_TEMP-MATNR.
    IT_TAB-SEQ = '3'.

    SELECT * INTO TABLE LT_ENG_STOCK
      FROM ZTPP_ENG_STOCK
  WHERE MATNR = IT_TAB_TEMP-MATNR
     AND BWART BETWEEN '101' AND '102'
    AND BUDAT BETWEEN  W_MTD AND W_D-1.

    LOOP AT LT_ENG_STOCK.
      IF LT_ENG_STOCK-SHKZG = 'H'.
        LT_ENG_STOCK-MENGE = - LT_ENG_STOCK-MENGE.
      ENDIF.
      IT_TAB-MTD = IT_TAB-MTD + LT_ENG_STOCK-MENGE.
      IF LT_ENG_STOCK-BUDAT = W_D-1.
        IT_TAB-D-1 = IT_TAB-D-1 + LT_ENG_STOCK-MENGE.
      ENDIF.
    ENDLOOP.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
    IT_TAB-IF = 'C210'.
*    ENDIF.
    IT_TAB-DESC = 'Del.'.
    WA_TAB3 = IT_TAB.
    APPEND IT_TAB.
    CLEAR: IT_TAB.


    IT_TAB-SEQ = '2'.
    IT_TAB-MATNR = IT_TAB_TEMP-MATNR.
    IT_TAB-QTYD_01 = WA_TAB1-LABST.

    LOOP AT IT_DAY FROM 2 .
      L_CN = IT_DAY-SEQ - 1.
      CONCATENATE 'WA_TAB1-QTYD_' L_CN INTO L_TEXT.
      IF SY-SUBRC = 0.
        ASSIGN (L_TEXT) TO <L_REQ>.
        CONCATENATE 'IT_TAB-QTYD_'  L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_STOCK>.
        CONCATENATE 'WA_TAB3-QTYD_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_PIR>.

        CONCATENATE 'IT_TAB-QTYD_' IT_DAY-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          <FS-QTY> = <L_STOCK> + <L_PIR> - <L_REQ>.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CONCATENATE 'WA_TAB1-QTYD_' W_MAX_DAY_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <L_REQ>.
    CONCATENATE 'IT_TAB-QTYD_'  W_MAX_DAY_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <L_STOCK>.
    CONCATENATE 'WA_TAB3-QTYD_' W_MAX_DAY_CN INTO L_TEXT.
    ASSIGN (L_TEXT) TO <L_PIR>.

    IT_TAB-QTYW_04 = <L_STOCK> + <L_PIR> - <L_REQ>.

    LOOP AT IT_WEEK FROM 2.
      L_CN = IT_WEEK-SEQ - 1.
      CONCATENATE 'WA_TAB1-QTYW_' L_CN INTO L_TEXT.
      IF SY-SUBRC = 0.
        ASSIGN (L_TEXT) TO <L_REQ>.
        CONCATENATE 'IT_TAB-QTYW_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_STOCK>.
        CONCATENATE 'WA_TAB3-QTYW_' L_CN INTO L_TEXT.
        ASSIGN (L_TEXT) TO <L_PIR>.

        CONCATENATE 'IT_TAB-QTYW_' IT_WEEK-SEQ INTO L_TEXT.
        ASSIGN (L_TEXT) TO <FS-QTY>.
        IF SY-SUBRC = 0.
          <FS-QTY> = <L_STOCK> + <L_PIR> - <L_REQ>.
        ENDIF.
      ENDIF.
    ENDLOOP.
*    IF WA_TAB1-IF IS INITIAL.
*    ELSE.
*      IT_TAB-IF = 'C210'.
*    ENDIF.
    IT_TAB-DESC = 'Stock'.
    APPEND IT_TAB.
    CLEAR: IT_TAB.
  ENDLOOP.

  SORT IT_TAB BY MATNR SEQ.

ENDFORM.                    " GET_INDEL_DATA
