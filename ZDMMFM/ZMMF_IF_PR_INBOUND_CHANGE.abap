FUNCTION ZMMF_IF_PR_INBOUND_CHANGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_ZSMM_IF001) LIKE  ZSMM_IF001 STRUCTURE  ZSMM_IF001
*"  TABLES
*"      E_RETURN STRUCTURE  ZSMM_IF017
*"----------------------------------------------------------------------

  CLEAR: V_ZSMM_IF001, V_RETURN.
  MOVE:  I_ZSMM_IF001 TO V_ZSMM_IF001.

*--Unit conversion from VAATZ. 2006.01.18-------------------------------
    SELECT SINGLE MSEHI INTO I_ZSMM_IF001-MEINS
                        FROM T006
                       WHERE ISOCODE = I_ZSMM_IF001-ZMEINS.
*-----------------------------------------------------------------------

*---// interface data type conversion
  PERFORM APPLY_CONVERSION_RULE.

*---// insert entry to table
  PERFORM PR_CHANGE_SAVE_IF_TABLE.
  READ TABLE V_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    E_RETURN[] = V_RETURN[].

    READ TABLE IT_PR_ITEM INDEX 1.
    MOVE: IT_PR_ITEM-TYPE    TO ZTMM_IF005-TYPE,
          IT_PR_ITEM-MESSAGE TO ZTMM_IF005-MESSAGE.

    UPDATE ZTMM_IF005.
    COMMIT WORK AND WAIT.
    EXIT.
  ELSE.
*---// importing value existence check with further processing
    PERFORM CHECK_PARAMETERS_PR_CHANGE.
    READ TABLE V_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      E_RETURN[] = V_RETURN[].

      READ TABLE IT_PR_ITEM INDEX 1.
      MOVE: IT_PR_ITEM-TYPE    TO ZTMM_IF005-TYPE,
            IT_PR_ITEM-MESSAGE TO ZTMM_IF005-MESSAGE.

      UPDATE ZTMM_IF005.
*      INSERT ZTMM_IF006 FROM TABLE IT_PR_ITEM.
      MODIFY ZTMM_IF006 FROM TABLE IT_PR_ITEM.
      COMMIT WORK AND WAIT.

    ELSE.
*---// Mandatory checking
      PERFORM MNADATORY_CHECK.
      READ TABLE V_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        E_RETURN[] = V_RETURN[].

        READ TABLE IT_PR_ITEM INDEX 1.
        MOVE: IT_PR_ITEM-TYPE    TO ZTMM_IF005-TYPE,
              IT_PR_ITEM-MESSAGE TO ZTMM_IF005-MESSAGE,
              SY-DATUM           TO ZTMM_IF005-TRAN_DATE,
              SY-UZEIT           TO ZTMM_IF005-TRAN_TIME,
              SY-UNAME           TO ZTMM_IF005-TRAN_NAME.

        UPDATE ZTMM_IF005.
*        INSERT ZTMM_IF006 FROM TABLE IT_PR_ITEM.
        MODIFY ZTMM_IF006 FROM TABLE IT_PR_ITEM.
        COMMIT WORK AND WAIT.
        EXIT.
      ELSE.
*---// EBAN, EBKN table selection bapi
        PERFORM SELECTION_TABLES_BAPI.
        READ TABLE V_RETURN WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0.
          E_RETURN[] = V_RETURN[].

          READ TABLE IT_PR_ITEM INDEX 1.
          MOVE: IT_PR_ITEM-TYPE    TO ZTMM_IF005-TYPE,
                IT_PR_ITEM-MESSAGE TO ZTMM_IF005-MESSAGE,
                SY-DATUM           TO ZTMM_IF005-TRAN_DATE,
                SY-UZEIT           TO ZTMM_IF005-TRAN_TIME,
                SY-UNAME           TO ZTMM_IF005-TRAN_NAME.

          UPDATE ZTMM_IF005.
*          INSERT ZTMM_IF006 FROM TABLE IT_PR_ITEM.
          MODIFY ZTMM_IF006 FROM TABLE IT_PR_ITEM.
          COMMIT WORK AND WAIT.
          EXIT.
        ELSE.
*---// P/R change data transfer bapi function
          PERFORM CHANGE_DATA_MOVE.
*---// P/R change data transfer bapi function
          PERFORM PR_CHANGE_TRANSFER_BAPI.
          E_RETURN[] = V_RETURN[].
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
