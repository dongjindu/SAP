FUNCTION Z_FRF_STOCK_OVERVIEW.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_STOCK) LIKE  ZSRF_STOCK STRUCTURE  ZSRF_STOCK OPTIONAL
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"     VALUE(P_LGTYP) LIKE  MLGT-LGTYP
*"     VALUE(P_LGPLA) LIKE  MLGT-LGPLA
*"  TABLES
*"      T_STOCK_LIST STRUCTURE  ZSRF_STOCK_LIST
*"----------------------------------------------------------------------
  DATA: BEGIN OF LT_LQUA OCCURS 0,
          MATNR TYPE LQUA-MATNR,
          WERKS TYPE LQUA-WERKS,
          LGORT TYPE LQUA-LGORT,
          MEINS TYPE LQUA-MEINS,
          MAKTX TYPE MAKT-MAKTX,
          LGTYP TYPE LQUA-LGTYP,
          LGPLA TYPE LQUA-LGPLA,
          WDATU TYPE LQUA-WDATU,
          GESME TYPE LQUA-GESME,
          LENUM TYPE LQUA-LENUM,
        END OF LT_LQUA.
  RANGES: R_WERKS FOR LQUA-WERKS,
          R_LGTYP FOR LQUA-LGTYP,
          R_MATNR FOR LQUA-MATNR.
  IF NOT I_STOCK-MATNR IS INITIAL.
    R_MATNR-LOW    = I_STOCK-MATNR.
    R_MATNR-SIGN   = 'I'.
    R_MATNR-OPTION = 'EQ'.
    APPEND R_MATNR.
  ENDIF.
  IF NOT I_STOCK-WERKS IS INITIAL.
    R_WERKS-LOW    = I_STOCK-WERKS.
    R_WERKS-SIGN   = 'I'.
    R_WERKS-OPTION = 'EQ'.
    APPEND R_WERKS.
  ENDIF.
  IF NOT I_STOCK-LGTYP IS INITIAL.
    R_LGTYP-LOW    = I_STOCK-LGTYP.
    R_LGTYP-SIGN   = 'I'.
    R_LGTYP-OPTION = 'EQ'.
    APPEND R_LGTYP.
  ENDIF.

  SELECT A~MATNR
         A~WERKS
         A~LGORT
         A~MEINS
         B~MAKTX
         A~LGTYP
         A~LGPLA
         A~WDATU
         A~GESME
         A~LENUM
       FROM LQUA AS A INNER JOIN MAKT AS B
                      ON  A~MATNR EQ B~MATNR
                      AND B~SPRAS EQ SY-LANGU
       INTO TABLE LT_LQUA
       WHERE A~MATNR IN R_MATNR
       AND   A~WERKS IN R_WERKS
       AND   A~LGNUM EQ 'P01'
       AND   A~LGTYP IN R_LGTYP
       AND   A~GESME GT 0.
*       AND   A~WERKS EQ I_STOCK-WERKS
*       AND   A~LGTYP EQ I_STOCK-LGTYP.
  IF SY-SUBRC EQ 0.
    E_MESS   = TEXT-M03.
    ZRESULT  = TEXT-M04.
    LOOP AT LT_LQUA.
      MOVE-CORRESPONDING LT_LQUA TO T_STOCK_LIST.
      WRITE: LT_LQUA-GESME TO T_STOCK_LIST-GESME
                           UNIT LT_LQUA-MEINS NO-ZERO.
      APPEND T_STOCK_LIST. CLEAR T_STOCK_LIST.
    ENDLOOP.
    CASE I_STOCK-SORAD.
      WHEN '1'.  "Sort by Storage Bin
        SORT T_STOCK_LIST BY LGTYP MATNR.
      WHEN '2'.  "Sort by GR Date
        SORT T_STOCK_LIST BY WDATU MATNR LGTYP.
      WHEN '3'.  "Sort by Expiration Date
    ENDCASE.
** added by furong for export source type/bin
    CALL FUNCTION 'Z_MM_LT01_SOURCE_CHECK'
      EXPORTING
        P_MATNR       = I_STOCK-MATNR
*       P_TOQTY       = 0
     IMPORTING
*       E_MESS        =
*       ZRESULT       =
       P_LGTYP       = P_LGTYP
       P_LGPLA       = P_LGPLA.
** end of change
  ELSE.
    E_MESS   = TEXT-M15.
    ZRESULT  = TEXT-M02.
  ENDIF.

ENDFUNCTION.
