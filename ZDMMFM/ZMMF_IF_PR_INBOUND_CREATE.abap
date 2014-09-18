FUNCTION ZMMF_IF_PR_INBOUND_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZSMM_IF001 STRUCTURE  ZSMM_IF001 OPTIONAL
*"      E_RETURN STRUCTURE  ZSMM_IF017 OPTIONAL
*"----------------------------------------------------------------------

  DATA : L_TABIX   LIKE SY-TABIX,
         TMP_BNFPO LIKE ZTMM_IF005-BNFPO.
  CLEAR: V_ZSMM_IF001, V_RETURN, ZTMM_IF005.
  REFRESH V_RETURN.
  CLEAR: IT_ZTMM_IF005, IT_IF001.
  REFRESH: IT_ZTMM_IF005, IT_IF001.

*--Unit conversion from VAATZ. 2006.01.18-------------------------------
  LOOP AT IT_ZSMM_IF001.
    SELECT SINGLE MSEHI INTO IT_ZSMM_IF001-MEINS
                        FROM T006
                       WHERE ISOCODE = ZTMM_IF005-ZMEINS.
*                     WHERE ISOCODE = IT_ZSMM_IF001-ZMEINS.
    MODIFY IT_ZSMM_IF001.
  ENDLOOP.
*-----------------------------------------------------------------------

  READ TABLE IT_ZSMM_IF001 INDEX  1.
  MOVE IT_ZSMM_IF001-BANFN TO G_BANFN.

*---// interface data type conversion
  LOOP AT IT_ZSMM_IF001.
    L_TABIX = SY-TABIX.
    CLEAR V_ZSMM_IF001.
    MOVE-CORRESPONDING IT_ZSMM_IF001 TO V_ZSMM_IF001.

    PERFORM APPLY_CONVERSION_RULE .

      MOVE-CORRESPONDING V_ZSMM_IF001  TO IT_ZSMM_IF001.
*--Log table ztmm_if005.
      MOVE-CORRESPONDING IT_ZSMM_IF001 TO IT_ZTMM_IF005.
      MOVE : SY-DATUM        TO IT_ZTMM_IF005-TRAN_DATE,
             SY-UZEIT        TO IT_ZTMM_IF005-TRAN_TIME,
             SY-UNAME        TO IT_ZTMM_IF005-TRAN_NAME,
             V_CREATE_FLAG   TO IT_ZTMM_IF005-FLAG.

    IT_ZTMM_IF005-CUNT = IT_ZTMM_IF005-CUNT + 1.
    APPEND IT_ZTMM_IF005.

    MODIFY IT_ZSMM_IF001 INDEX SY-TABIX. CLEAR IT_ZSMM_IF001.
  ENDLOOP.
  IT_IF001[] = IT_ZSMM_IF001[].
*---// insert entry to table
  PERFORM SAVE_IF_TABLE.
  READ TABLE V_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    E_RETURN[] = V_RETURN[].
    EXIT.
  ELSE.
*---// importing value existence check with further processing
    PERFORM CHECK_PR_ALREADY.
    READ TABLE V_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      E_RETURN[] = V_RETURN[].
      EXIT.
    ELSE.
*---// Mandatory checking
      CLEAR:   IT_PR_ITEM, V_RETURN.
      REFRESH: IT_PR_ITEM, V_RETURN.

      LOOP AT IT_ZSMM_IF001.
        CLEAR V_ZSMM_IF001.
        MOVE IT_ZSMM_IF001 TO V_ZSMM_IF001.
        PERFORM MANDATORY_PARAM_CHECK.
      ENDLOOP.

      READ TABLE V_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        E_RETURN[] = V_RETURN[].
        SORT IT_PR_ITEM BY BANFN BNFPO.

        LOOP AT IT_PR_ITEM.
          IF IT_PR_ITEM-BNFPO NE TMP_BNFPO OR
             TMP_BNFPO IS INITIAL.
            READ TABLE IT_ZTMM_IF005 WITH KEY BANFN = IT_PR_ITEM-BANFN
                                              BNFPO = IT_PR_ITEM-BNFPO.

            MOVE: IT_PR_ITEM-TYPE    TO IT_ZTMM_IF005-TYPE,
                  IT_PR_ITEM-MESSAGE TO IT_ZTMM_IF005-MESSAGE.

            MODIFY IT_ZTMM_IF005 INDEX SY-TABIX.
            CLEAR  IT_ZTMM_IF005.
            TMP_BNFPO = IT_PR_ITEM-BNFPO.
          ENDIF.
        ENDLOOP.

        INSERT ZTMM_IF005 FROM TABLE IT_ZTMM_IF005.
        INSERT ZTMM_IF006 FROM TABLE IT_PR_ITEM.
        COMMIT WORK.
        EXIT.
      ELSE.
*---// Data Move internal table
        PERFORM DATA_MOVE_TABLE.
*---// P/R Create and transfer BAPi function
        PERFORM DATA_TRANSFER_BAPI.
        E_RETURN[] = V_RETURN[].
      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
