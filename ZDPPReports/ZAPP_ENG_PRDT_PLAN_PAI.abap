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
     WHEN 'EXIT'.
      CLEAR: W_NEW, W_REFRESH.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      CLEAR: W_NEW, W_REFRESH.
      LEAVE TO SCREEN 0.
    WHEN 'CAL'.
      PERFORM RECAL_STOCK.
    WHEN 'REF'.
      W_REFRESH = 'X'.
      LEAVE TO SCREEN 0.
    WHEN 'REP'.
      PERFORM REPLACE_PIR_QTY.
*      PERFORM RECAL_STOCK.
    WHEN 'SAVE'.
      PERFORM SAVE_DATA.
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
*      LT_ENG_PIR-WERKS = C_WERKS.
      LT_ENG_PIR-WERKS = w_WERKS.
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
*  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE.
  DATA: LT_CELL TYPE LVC_T_CELL.
  DATA: LW_CELL TYPE LVC_S_CELL.
*   DATA: lt_CELL_type LVC_T_CELL WITH HEADER LINE,
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
*  wa_stbl-COL = 'X'.
      CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
       EXPORTING IS_STABLE = WA_STBL.
*  CALL METHOD ALV_GRID->SET_SELECTED_CELLS
*           exPORTING it_cells = LT_CELL.


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
  DATA: WA_STBL  TYPE LVC_S_STBL.
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
*  wa_stbl-COL = 'X'.
  CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY
   EXPORTING IS_STABLE = WA_STBL.

  MESSAGE S000(ZZ) WITH 'Data recalculated'.
ENDFORM.                    " UPDATE_STOCK
