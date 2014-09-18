FUNCTION Z_FRF_MM_TRUCK_DELIVERY_SEARCH.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_LIFEX) LIKE  LIKP-LIFEX
*"  EXPORTING
*"     VALUE(E_MESS) TYPE  BAPI_MSG
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_TRUCK_LIST STRUCTURE  ZSRF_TRUCK_SE
*"----------------------------------------------------------------------
  DATA: BEGIN OF LT_LIKP OCCURS 0,
          BORGR TYPE LIKP-BORGR_GRP,
          VBELN TYPE LIKP-VBELN,
        END OF LT_LIKP.

  SELECT LIKP~BORGR_GRP LIKP~VBELN
    INTO TABLE LT_LIKP
    FROM LIKP
      INNER JOIN VBUK
      ON VBUK~VBELN = LIKP~VBELN AND
*         VBUK~KOSTK <> SPACE     AND "Overall picking / putaway status
         VBUK~WBSTK = 'A'         "TotalGdsMvtStat (Not Yet Processed)
    WHERE LIKP~VERUR = I_LIFEX    " 'HMMA1234'.
    AND   VBUK~KOSTK IN ('A', ' ').

  IF SY-SUBRC EQ 0.
    SORT LT_LIKP BY BORGR.
    DELETE ADJACENT DUPLICATES FROM LT_LIKP
                               COMPARING BORGR.
    IF LT_LIKP[] IS INITIAL.
      E_MESS  = TEXT-M01.
      ZRESULT = TEXT-M02.
    ELSE.
      LOOP AT LT_LIKP.
        MOVE-CORRESPONDING LT_LIKP TO T_TRUCK_LIST.
        T_TRUCK_LIST-LIFEX = I_LIFEX.
        APPEND T_TRUCK_LIST. CLEAR T_TRUCK_LIST.
      ENDLOOP.
      E_MESS  = TEXT-M03.
      ZRESULT = TEXT-M04.
    ENDIF.
  ELSE.
    E_MESS  = TEXT-M01.
    ZRESULT = TEXT-M02.
  ENDIF.

ENDFUNCTION.
