FUNCTION Z_FRF_MARD_CHEDK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(ZRESULT) TYPE  ZRESULT
*"  TABLES
*"      T_CHECK STRUCTURE  ZSRF_AMOUNT_CHECK
*"      T_ERROR STRUCTURE  ZSRF_AMOUNT_CHECK
*"----------------------------------------------------------------------
  DATA: BEGIN OF LT_MARD OCCURS 0,
          WERKS TYPE MARD-WERKS,
          LGORT TYPE MARD-LGORT,
          MATNR TYPE MARD-MATNR,
          LABST TYPE MARD-LABST,
        END OF LT_MARD.
  DATA: L_ENMNG TYPE MARD-LABST,
        M_ENMNG(18).

  SELECT WERKS
         LGORT
         MATNR
         LABST
       FROM MARD
       INTO TABLE LT_MARD
       FOR ALL ENTRIES IN T_CHECK
       WHERE WERKS EQ T_CHECK-WERKS
       AND   MATNR EQ T_CHECK-MATNR
       AND   LGORT EQ T_CHECK-LGORT.
  IF SY-SUBRC EQ 0.
    SORT LT_MARD BY WERKS
                    LGORT
                    MATNR.
    LOOP AT T_CHECK.
      READ TABLE LT_MARD WITH KEY WERKS = T_CHECK-WERKS
                                  LGORT = T_CHECK-LGORT
                                  MATNR = T_CHECK-MATNR
                         BINARY SEARCH TRANSPORTING LABST.
      IF SY-SUBRC EQ 0.
        IF T_CHECK-BDMNG GT LT_MARD-LABST.
          L_ENMNG = T_CHECK-BDMNG - LT_MARD-LABST.
          WRITE: L_ENMNG TO M_ENMNG UNIT T_CHECK-ERFME LEFT-JUSTIFIED.
          ZRESULT = '1'.
          T_ERROR-WERKS = T_CHECK-WERKS.
          T_ERROR-LGORT = T_CHECK-LGORT.
          T_ERROR-MATNR = T_CHECK-MATNR.
          CONCATENATE T_ERROR-MATNR
                      'material exceeds the amount in stock by'
                      M_ENMNG 'at' T_ERROR-LGORT 'S/L'
                      INTO T_ERROR-MESSA SEPARATED BY SPACE.
          APPEND T_ERROR. CLEAR T_ERROR.
        ENDIF.

      ELSE.
        ZRESULT = '1'.
        T_ERROR-WERKS = T_CHECK-WERKS.
        T_ERROR-LGORT = T_CHECK-LGORT.
        T_ERROR-MATNR = T_CHECK-MATNR.
        CONCATENATE 'There is no' T_ERROR-MATNR
                    'material in stock at' T_ERROR-LGORT
                    'S/L' INTO T_ERROR-MESSA SEPARATED BY SPACE.
        APPEND T_ERROR. CLEAR T_ERROR.
      ENDIF.

    ENDLOOP.
  ELSE.
    LOOP AT T_CHECK.
      ZRESULT = '1'.
      T_ERROR-WERKS = T_CHECK-WERKS.
      T_ERROR-LGORT = T_CHECK-LGORT.
      T_ERROR-MATNR = T_CHECK-MATNR.
      CONCATENATE 'There is no' T_ERROR-MATNR
                  'material in stock at' T_ERROR-LGORT
                  'S/L' INTO T_ERROR-MESSA SEPARATED BY SPACE.
      APPEND T_ERROR. CLEAR T_ERROR.
    ENDLOOP.
  ENDIF.

  IF ZRESULT EQ '1'.

  ELSE.
    ZRESULT = '0'.
  ENDIF.
ENDFUNCTION.
