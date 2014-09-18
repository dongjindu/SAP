*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0060F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INITIALIZATION .
  DATA : L_DATE LIKE SY-DATUM .

  L_DATE = SY-DATUM .
  L_DATE+6(2) = '01' .
  L_DATE = L_DATE - 1 .

  P_LFGJA = L_DATE+0(4) .
  P_LFMON = L_DATE+4(2) .

  G_YYYYMM = L_DATE+0(6) .

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .
  DATA : LT_MARDH LIKE TABLE OF MARDH  WITH HEADER LINE ,
         LT_READ  LIKE TABLE OF MARDH  WITH HEADER LINE .
  DATA : LT_MARA  LIKE TABLE OF MARA  WITH HEADER LINE ,
         LT_T001K LIKE TABLE OF T001K WITH HEADER LINE ,
         LT_T001  LIKE TABLE OF T001  WITH HEADER LINE ,
         LT_MBEW  LIKE TABLE OF MBEW  WITH HEADER LINE .

  DATA : LT_BKPF LIKE TABLE OF BKPF WITH HEADER LINE .
  DATA : L_YYYYMM(6) TYPE N ,
         L_B_YYYYMM(6) TYPE N .

  RANGES : R_BSTAT FOR BKPF-BSTAT ,
           R_BUDAT FOR BKPF-BUDAT .

*. Get Company Code
  SELECT * INTO TABLE LT_T001K
    FROM T001K
   WHERE BWKEY IN S_WERKS .
  IF SY-SUBRC = 0 .
    SELECT * INTO TABLE LT_T001
      FROM T001
      FOR ALL ENTRIES IN LT_T001K
     WHERE BUKRS EQ LT_T001K-BUKRS .
  ENDIF .


  CLEAR : GT_DATA[], GT_DATA .

*. Get row data
  SELECT WERKS
         LGORT
         MATNR
         LFGJA
         LFMON
         LABST
    INTO CORRESPONDING FIELDS OF TABLE LT_MARDH
    FROM MARDH
   WHERE WERKS IN S_WERKS
     AND LGORT IN S_LGORT
     AND MATNR IN S_MATNR .

  L_YYYYMM+0(4) = P_LFGJA .
  L_YYYYMM+4(2) = P_LFMON .

*. GET 2~ Previous Month
  IF L_YYYYMM < G_YYYYMM .

    R_BUDAT = 'IBT' .
    R_BUDAT-LOW+0(6) = L_YYYYMM .
    R_BUDAT-LOW+6(2) = '01' .
    R_BUDAT-HIGH+0(6) = L_YYYYMM .
    R_BUDAT-HIGH+6(2) = '31' .
    APPEND R_BUDAT .

    READ TABLE LT_T001 INDEX 1 .

    SELECT * INTO TABLE LT_BKPF
        UP TO  1 ROWS
      FROM BKPF
     WHERE BUKRS EQ LT_T001-BUKRS
       AND BSTAT IN R_BSTAT
       AND BUDAT IN R_BUDAT .

    READ TABLE LT_BKPF INDEX 1 .
    IF SY-SUBRC = 0 .

      LOOP AT LT_MARDH  .
        L_B_YYYYMM+0(4) = LT_MARDH-LFGJA .
        L_B_YYYYMM+4(2) = LT_MARDH-LFMON .
        IF L_B_YYYYMM > L_YYYYMM .
          DELETE LT_MARDH .
        ENDIF .
      ENDLOOP .

    ENDIF .
  ENDIF .

  SORT LT_MARDH BY WERKS LGORT MATNR LFGJA DESCENDING  LFMON DESCENDING .
  LT_READ[] = LT_MARDH[] .
  SORT LT_READ BY WERKS LGORT MATNR .
  DELETE ADJACENT DUPLICATES FROM LT_READ
                  COMPARING WERKS LGORT MATNR .
  LOOP AT LT_READ .

    READ TABLE LT_MARDH WITH KEY WERKS = LT_READ-WERKS
                                 LGORT = LT_READ-LGORT
                                 MATNR = LT_READ-MATNR
                                 BINARY SEARCH .
    IF SY-SUBRC = 0 .
      MOVE-CORRESPONDING LT_MARDH TO GT_DATA .
      APPEND GT_DATA .
    ENDIF .
  ENDLOOP .

*. No zero stock
  IF P_CHK IS NOT INITIAL .
    DELETE GT_DATA WHERE LABST = 0 .
  ENDIF .

  CHECK GT_DATA[] IS NOT INITIAL .


*. Get moving average Price/Periodic Unit Price
  SELECT * INTO TABLE LT_MBEW
    FROM MBEW
    FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR EQ GT_DATA-MATNR
    AND BWKEY EQ GT_DATA-WERKS .

* Get Mara table
  SELECT * INTO TABLE LT_MARA
    FROM MARA
    FOR ALL ENTRIES IN GT_DATA
  WHERE MATNR EQ GT_DATA-MATNR .

  SORT LT_MBEW BY MATNR BWKEY .
  SORT LT_T001K BY BWKEY .
  SORT LT_T001 BY BUKRS .
  SORT LT_MARA BY MATNR .


  LOOP AT GT_DATA .

    READ TABLE LT_T001K WITH KEY BWKEY = GT_DATA-WERKS
                                 BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GT_DATA-BUKRS = LT_T001K-BUKRS .
    ENDIF .

    READ TABLE LT_MBEW WITH KEY MATNR = GT_DATA-MATNR
                              BWKEY = GT_DATA-WERKS
                              BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GT_DATA-BKLAS = LT_MBEW-BKLAS .
      GT_DATA-VERPR = LT_MBEW-VERPR .
      GT_DATA-PEINH = LT_MBEW-PEINH .
    ENDIF .

    READ TABLE LT_T001 WITH KEY BUKRS = GT_DATA-BUKRS
                                BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GT_DATA-WAERS = LT_T001-WAERS .
    ENDIF .

    READ TABLE LT_MARA WITH KEY MATNR = GT_DATA-MATNR
                                BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GT_DATA-MTART = LT_MARA-MTART .
      GT_DATA-MATKL = LT_MARA-MATKL .
      GT_DATA-MEINS = LT_MARA-MEINS .
    ENDIF .

*. Total Valuation Amount
    IF GT_DATA-PEINH IS NOT INITIAL .
      GT_DATA-ZQKUM = ( GT_DATA-LABST * GT_DATA-VERPR ) / GT_DATA-PEINH .
    ENDIF .

    GT_DATA-ICON = '@5D@'."  yellow

    GT_DATA-LFGJA = P_LFGJA .
    GT_DATA-LFMON = P_LFMON .
    MODIFY GT_DATA .
  ENDLOOP .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_DATA .
  DATA : LT_PMT0006 LIKE TABLE OF ZHKPMT0006 WITH HEADER LINE .

  CHECK GT_DATA[] IS NOT INITIAL .

  SELECT * INTO TABLE LT_PMT0006
    FROM ZHKPMT0006
    FOR ALL ENTRIES IN GT_DATA
   WHERE BUKRS EQ GT_DATA-BUKRS
     AND WERKS EQ GT_DATA-WERKS
     AND LGORT EQ GT_DATA-LGORT
     AND MATNR EQ GT_DATA-MATNR
     AND LFGJA EQ GT_DATA-LFGJA
     AND LFMON EQ GT_DATA-LFMON .

  SORT GT_DATA BY BUKRS WERKS LGORT MATNR LFGJA LFMON .

  LOOP AT LT_PMT0006 .
    READ TABLE GT_DATA WITH KEY BUKRS = LT_PMT0006-BUKRS
                                WERKS = LT_PMT0006-WERKS
                                LGORT = LT_PMT0006-LGORT
                                MATNR = LT_PMT0006-MATNR
                                LFGJA = LT_PMT0006-LFGJA
                                LFMON = LT_PMT0006-LFMON
                                BINARY SEARCH .
    IF SY-SUBRC = 0 .
      GT_DATA-ZIFFLAG   = LT_PMT0006-ZIFFLAG .
      GT_DATA-ZIFRESULT = LT_PMT0006-ZIFRESULT .
      MODIFY GT_DATA INDEX SY-TABIX .
    ENDIF .
  ENDLOOP .

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_ALV_SCREEN .

  PERFORM MAKE_LAYOUT.

  PERFORM MAKE_FIELDCAT.

  PERFORM MAKE_SORTCAT.

  PERFORM CALL_ALV_GRID_FUNCTION.

ENDFORM.                    " DISPLAY_ALV_SCREEN
*&---------------------------------------------------------------------*
*&      Form  MAKE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_LAYOUT .

** Declare Init .
  MOVE : SY-REPID TO G_REPID,
         C_A      TO G_SAVE.

  CLEAR : GS_LAYOUT, GT_EVENT[] .

  MOVE : C_X       TO GS_LAYOUT-COLWIDTH_OPTIMIZE,
         'MARK'    TO GS_LAYOUT-BOX_FIELDNAME,
         "C_X       TO GS_LAYOUT-TOTALS_BEFORE_ITEMS,
         C_X       TO GS_LAYOUT-ZEBRA,
         C_X       TO GS_LAYOUT-CELL_MERGE.


** Declare Event
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      I_LIST_TYPE = 0
    IMPORTING
      ET_EVENTS   = GT_EVENT.

  MOVE: SLIS_EV_USER_COMMAND TO GS_EVENT-NAME,
        SLIS_EV_USER_COMMAND TO GS_EVENT-FORM.
  APPEND GS_EVENT TO GT_EVENT.

  MOVE: SLIS_EV_PF_STATUS_SET TO GS_EVENT-NAME,
        SLIS_EV_PF_STATUS_SET TO GS_EVENT-FORM.
  APPEND GS_EVENT TO GT_EVENT.

**  MOVE: SLIS_EV_TOP_OF_PAGE TO GS_EVENT-NAME,
**        SLIS_EV_TOP_OF_PAGE TO GS_EVENT-FORM.
**  APPEND GS_EVENT TO GT_EVENT.



ENDFORM.                    " MAKE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_FIELDCAT .

  DATA L_TABNAM(10)   TYPE C.

  CLEAR : GT_FIELDCAT, GT_FIELDCAT[], GS_FIELDCAT.
  L_TABNAM = 'GT_DATA'.

  PERFORM MAKE_FIELDCAT_ATT USING:
   'X'  L_TABNAM   'ICON'   'Status'   '04' ' ' ' ' ' ' ' ' 'C' ,
   'X'  L_TABNAM   'BUKRS'  'CO Code'     '04' ' ' ' ' ' ' ' ' ' ' ,
   'X'  L_TABNAM   'WERKS'  'Plant'    '05' ' ' ' ' ' ' ' ' ' ' ,
   'X'  L_TABNAM   'LGORT'  'Sloc'     '10' ' ' ' ' ' ' ' ' ' ' ,

   'X'  L_TABNAM   'MATNR'  'Material'   '18' ' ' ' ' ' ' ' ' ' ' ,

   ' '  L_TABNAM   'LFGJA'  'Year'       '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'LFMON'  'Month'      '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'MEINS'  'Unit'       '04' ' ' ' ' ' ' ' ' 'C' ,
   ' '  L_TABNAM   'LABST'  'UrStock'    '10' '1' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'ZQKUM'  'Amount'     '10' '3' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'WAERS'  'Currency'   '04' ' ' ' ' ' ' ' ' 'C' ,

   ' '  L_TABNAM   'BKLAS'  'VAlCls'      '06' ' ' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'VERPR'  'UPrice'      '10' '3' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'PEINH'  'PrcUint'     '10' ' ' ' ' ' ' ' ' ' ' ,

   ' '  L_TABNAM   'MTART'  'MatType'     '05' ' ' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'MATKL'  'MatGroup'    '05' ' ' ' ' ' ' ' ' ' ' ,

   ' '  L_TABNAM   'ZIFDATE'  'I/F Date' '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'ZIFEMP'  'I/F emp'   '10' ' ' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'ZIFFLAG'  'I/F Flag' '04' ' ' ' ' ' ' ' ' ' ' ,
   ' '  L_TABNAM   'ZIFRESULT'  'I/F Result' '40' ' ' ' ' ' ' ' ' ' ' .

ENDFORM.                    " MAKE_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MAKE_FIELDCAT_ATT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MAKE_FIELDCAT_ATT  USING P_KEY
                              P_TABNAME
                              P_FIELDNAME
                              P_REPTEXT_DDIC
                              P_OUTPUTLEN
                              P_NO_OUT
                              P_NO_ZERO
                              P_EMPHASIZE
                              P_HOTSPOT
                              P_JUST .

  DATA : LS_FIELDCAT  TYPE SLIS_FIELDCAT_ALV.

  CLEAR LS_FIELDCAT.

  LS_FIELDCAT-KEY          = P_KEY.
  LS_FIELDCAT-TABNAME      = P_TABNAME.
  LS_FIELDCAT-FIELDNAME    = P_FIELDNAME.
  LS_FIELDCAT-REPTEXT_DDIC = P_REPTEXT_DDIC.
  LS_FIELDCAT-OUTPUTLEN    = P_OUTPUTLEN.
  LS_FIELDCAT-NO_ZERO      = P_NO_ZERO.
  LS_FIELDCAT-EMPHASIZE    = P_EMPHASIZE.
  LS_FIELDCAT-HOTSPOT      = P_HOTSPOT.
  LS_FIELDCAT-JUST         = P_JUST .

  IF P_NO_OUT = 'X'.
    LS_FIELDCAT-NO_OUT       = P_NO_OUT.
  ENDIF.
  IF P_NO_OUT = '1'.
    LS_FIELDCAT-QFIELDNAME   = 'MEINS'.
*    ls_fieldcat-do_sum       = 'X'.
  ENDIF.
  IF P_NO_OUT = '2'.
    LS_FIELDCAT-HOTSPOT = 'X'.
  ENDIF.

  IF P_NO_OUT = '3'.
    LS_FIELDCAT-CFIELDNAME   = 'WAERS'.
  ENDIF.

  IF P_FIELDNAME = 'MATNR_SORT'.
    LS_FIELDCAT-NO_OUT  = 'X'.
  ENDIF.


  IF P_FIELDNAME = 'EQUNR' .
    LS_FIELDCAT-REF_FIELDNAME  = 'EQUNR' .
    LS_FIELDCAT-REF_TABNAME    = 'EQUI' .
  ENDIF .

  IF P_FIELDNAME = 'AUFNR' .
    LS_FIELDCAT-REF_FIELDNAME  = 'AUFNR' .
    LS_FIELDCAT-REF_TABNAME    = 'AUFM' .
  ENDIF .

  APPEND LS_FIELDCAT TO GT_FIELDCAT.


ENDFORM.                    " MAKE_FIELDCAT_ATT
*&---------------------------------------------------------------------*
*&      Form  MAKE_SORTCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_SORTCAT .

  CLEAR : GT_SORTCAT, GT_SORTCAT[], GS_SORTCAT .

*  MOVE : 1          TO gs_sortcat-spos ,
*       'BUKRS'      TO gs_sortcat-fieldname ,
*       'X'          TO gs_sortcat-up .
*  APPEND gs_sortcat TO gt_sortcat .
*  CLEAR  gs_sortcat .
*
*  MOVE : 2          TO gs_sortcat-spos ,
*       'WERKS'      TO gs_sortcat-fieldname ,
*       'X'          TO gs_sortcat-up .
**               'X'        TO GS_sortcat-subtot .
*  APPEND gs_sortcat TO gt_sortcat .
*  CLEAR  gs_sortcat .

ENDFORM.                    " MAKE_SORTCAT
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_GRID_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_GRID_FUNCTION .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM = G_REPID
*      i_grid_title       = l_title
      I_SAVE             = G_SAVE
      IS_LAYOUT          = GS_LAYOUT
      IT_FIELDCAT        = GT_FIELDCAT[]
      IT_SORT            = GT_SORTCAT[]
      IT_EVENTS          = GT_EVENT[]
      IS_PRINT           = G_SLIS_PRINT
      I_HTML_HEIGHT_TOP  = 0
    TABLES
      T_OUTTAB           = GT_DATA[]
    EXCEPTIONS
      PROGRAM_ERROR      = 1
      OTHERS             = 2.

ENDFORM.                    " CALL_ALV_GRID_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*
*       PF-STATUS
*----------------------------------------------------------------------*
FORM PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.

  DATA: LS_EXTAB TYPE SLIS_EXTAB ,
        LT_EXTAB TYPE SLIS_EXTAB OCCURS 0 .


  SET PF-STATUS 'STANDARD' EXCLUDING LT_EXTAB.

*  SET TITLEBAR 'T1100' WITH l_title.

ENDFORM.                    "PF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RF_UCOMM                                                      *
*  -->  RS_SELFIELD                                                   *
*---------------------------------------------------------------------*
FORM USER_COMMAND USING RF_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA : SELTAB LIKE RSPARAMS OCCURS 0 WITH HEADER LINE.
*-->
  DATA: BEGIN OF LT_MAST OCCURS 0,
        MATNR TYPE MAST-MATNR,
        WERKS TYPE MAST-WERKS,
        STLAN TYPE MAST-STLAN,
      END OF LT_MAST.
*-->
  RS_SELFIELD-COL_STABLE  = 'X' .
  RS_SELFIELD-ROW_STABLE  = 'X' .


  CASE RF_UCOMM .
    WHEN '&IC1' .
      READ TABLE GT_DATA INDEX RS_SELFIELD-TABINDEX .
      CHECK SY-SUBRC = 0 .

      CASE RS_SELFIELD-FIELDNAME.
        WHEN 'MATNR' .
          SET PARAMETER ID 'MAT' FIELD GT_DATA-MATNR .
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
      ENDCASE .

    WHEN 'ZRFC' . "Call RFC
      PERFORM GET_LINE_RFC .
      RS_SELFIELD-REFRESH = C_X .

  ENDCASE .

ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  GET_LINE_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LINE_RFC .

  READ TABLE GT_DATA WITH KEY MARK = C_X .
  IF SY-SUBRC <> 0 .
    MESSAGE S000 WITH 'There is no selected line' .
    EXIT .
  ENDIF .

  SELECT SINGLE * FROM USR01 WHERE BNAME EQ SY-UNAME.

  DATA : LT_DATA LIKE ZHKPMT0006 OCCURS 0 WITH HEADER LINE ,
         LT_SEND LIKE ZHKPMT0006 OCCURS 0 WITH HEADER LINE .

  DATA : LT_PMT0006 LIKE TABLE OF ZHKPMT0006 WITH HEADER LINE .

  LOOP AT GT_DATA  .

    IF GT_DATA-MARK IS INITIAL .
      CONTINUE .
    ENDIF .

    GT_DATA-ZIFFLAG = C_I .
    GT_DATA-ZIFDATE = SY-DATUM .
    CLEAR GT_DATA-ZIFRESULT .
    MOVE-CORRESPONDING GT_DATA TO LT_DATA .
    APPEND LT_DATA .
    MODIFY GT_DATA .

    MOVE-CORRESPONDING GT_DATA TO LT_PMT0006 .
    APPEND LT_PMT0006 .

  ENDLOOP .

*. Update CBO table
  MODIFY ZHKPMT0006 FROM TABLE LT_PMT0006 .
  SORT LT_PMT0006 BY WERKS LGORT MATNR .

  DATA : L_AT     TYPE I ,
         L_TOTAL  TYPE I ,
         L_START  TYPE I ,
         L_END    TYPE I .
  DATA : L_ZQKUM(20) .


  CLEAR :  G_SUCCESS , G_ERROR .

  DESCRIBE TABLE LT_DATA LINES L_TOTAL .

  L_AT = 1000 .

  DO .

    IF L_START IS INITIAL .
      L_START = 1 .
      L_END   = L_AT .
    ELSE .
      L_START = L_END  + 1 .
      L_END   = L_END + L_AT .
    ENDIF .

    IF L_START > L_TOTAL .
      EXIT .
    ENDIF .

    IF L_END > L_TOTAL .
      L_END = L_TOTAL .
    ENDIF .

    CLEAR LT_SEND[] .
    APPEND LINES OF LT_DATA  FROM L_START TO L_END
                 TO LT_SEND .

*. Conversion Price
    LOOP AT LT_SEND .
      IF LT_SEND-WAERS IS NOT INITIAL .
        WRITE LT_SEND-ZQKUM TO L_ZQKUM CURRENCY LT_SEND-WAERS .
        PERFORM SET_WRITE_NO_MASK USING  L_ZQKUM ..
        LT_SEND-ZQKUM = L_ZQKUM .

      ENDIF .
      MODIFY LT_SEND .

      UPDATE ZHKPMT0006
         SET ZIFFLAG = C_P
       WHERE BUKRS = LT_SEND-BUKRS
         AND WERKS = LT_SEND-WERKS
         AND LGORT = LT_SEND-LGORT
         AND MATNR = LT_SEND-MATNR
         AND LFGJA = LT_SEND-LFGJA
         AND LFMON = LT_SEND-LFMON  .

    ENDLOOP .

    COMMIT WORK .

*. Call RFC
    CALL FUNCTION 'ZPM006_MONTH_END_STOCK_GETIS'
      DESTINATION P_DEST
      TABLES
        T_DATA                = LT_SEND
      EXCEPTIONS
        SYSTEM_FAILURE        = 1
        COMMUNICATION_FAILURE = 2.
* ICON
* '@5B@'."  green
* '@5C@'."  red
* '@5D@'."  yellow

    IF SY-SUBRC <> 0 .
      LOOP AT LT_SEND .
        READ TABLE GT_DATA WITH KEY WERKS = LT_SEND-WERKS
                                    LGORT = LT_SEND-LGORT
                                    MATNR = LT_SEND-MATNR
                                    LFGJA = LT_SEND-LFGJA
                                    LFMON = LT_SEND-LFMON  .

        IF SY-SUBRC = 0 .
          GT_DATA-ICON = '@5C@'."  red
          MODIFY GT_DATA INDEX SY-TABIX .

          UPDATE ZHKPMT0006
             SET ZIFFLAG = C_E
                 ZIFRESULT = 'system_failure '
           WHERE BUKRS = LT_SEND-BUKRS
             AND WERKS = LT_SEND-WERKS
             AND LGORT = LT_SEND-LGORT
             AND MATNR = LT_SEND-MATNR
             AND LFGJA = LT_SEND-LFGJA
             AND LFMON = LT_SEND-LFMON .

        ENDIF .

        G_ERROR = G_ERROR + 1.
      ENDLOOP .

    ELSE .

      LOOP AT LT_SEND .
        READ TABLE GT_DATA WITH KEY WERKS = LT_SEND-WERKS
                                    LGORT = LT_SEND-LGORT
                                    MATNR = LT_SEND-MATNR
                                    LFGJA = LT_SEND-LFGJA
                                    LFMON = LT_SEND-LFMON .
        IF SY-SUBRC = 0 .
          IF LT_SEND-ZIFFLAG = 'S' OR
             LT_SEND-ZIFFLAG = 'Z' .
            GT_DATA-ICON = '@5B@'."  green
          ELSE .
            GT_DATA-ICON = '@5C@'."  red
          ENDIF .
          GT_DATA-ZIFFLAG   = LT_SEND-ZIFFLAG .
          GT_DATA-ZIFRESULT = LT_SEND-ZIFRESULT .
          MODIFY GT_DATA INDEX SY-TABIX .
        ENDIF .

        UPDATE ZHKPMT0006
       SET ZIFFLAG   = LT_SEND-ZIFFLAG
           ZIFRESULT = LT_SEND-ZIFRESULT
         WHERE BUKRS = LT_SEND-BUKRS
           AND WERKS = LT_SEND-WERKS
           AND LGORT = LT_SEND-LGORT
           AND MATNR = LT_SEND-MATNR
           AND LFGJA = LT_SEND-LFGJA
           AND LFMON = LT_SEND-LFMON .

        G_SUCCESS = G_SUCCESS + 1 .
      ENDLOOP .

    ENDIF .

  ENDDO .

*.
  IF G_ERROR IS NOT INITIAL .
    MESSAGE S001 WITH 'Error : Transfer.' .
  ELSE .
    MESSAGE S001 WITH 'Has been completed : Transfer.' .
  ENDIF .

ENDFORM.                    " GET_LINE_RFC
*&---------------------------------------------------------------------*
*&      Form  PROCESS_BATCH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_BATCH .

  GT_DATA-MARK = C_X .
  MODIFY GT_DATA FROM GT_DATA TRANSPORTING MARK WHERE MARK = SPACE .

  PERFORM GET_LINE_RFC .

ENDFORM.                    " PROCESS_BATCH
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LOG .

**  ------------------------------------------------------
**  PGM. Name .TEXTXXXXXXXXXXXXXXXX (T-CODE, Program Discription)
**  JOB-Start Time: YYYY.MM.DD HH:MM:SS
**  JOB-End  Time: YYYY.MM.DD HH:MM:SS
**  Data Process count: xxxx EA
**  ------------------------------------------------------
  DATA: L_TABIX_A  TYPE SYTABIX,
        L_TABIX_B  TYPE SYTABIX,
        L_START(22),
        L_END(22).

  WRITE G_JOB_START_DATE TO L_START+0(10).
  WRITE G_JOB_START_TIME TO L_START+11(10).
  WRITE G_JOB_END_DATE   TO L_END+0(10).
  WRITE G_JOB_END_TIME   TO L_END+11(10).

  WRITE:/2 SY-ULINE(109).
  WRITE:/2(29) 'PGM Namr          :', (10) SY-CPROG, (70) SY-TITLE.
  WRITE:/2(29) 'JOB-Start Time    :', (20) L_START.
  WRITE:/2(29) 'JOB-End Time      :', (20) L_END.

  WRITE:/2 SY-ULINE(109).

ENDFORM.                    " DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Form  SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTION_SCREEN .

  IF SY-UCOMM = 'ONLI' .
  ENDIF .

ENDFORM.                    " SELECTION_SCREEN
