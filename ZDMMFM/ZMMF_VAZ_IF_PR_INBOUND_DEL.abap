FUNCTION ZMMF_VAZ_IF_PR_INBOUND_DEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_ZSMM_IF081) LIKE  ZSMM_VAZ_IF081 STRUCTURE
*"        ZSMM_VAZ_IF081
*"  TABLES
*"      E_RETURN STRUCTURE  ZSMM_IF017 OPTIONAL
*"----------------------------------------------------------------------
*  Date       Request     Developer      Description
*  10/27/2006 UD1K922772  Manju          Update PR with Reason code text
*                                        from Vaatz.
*"----------------------------------------------------------------------

  CLEAR: V_ZSMM_IF081, V_RETURN, ZTMM_vaz_IF001, ZTMM_IF006, IT_PR_HEAD,
         IT_PR_ITEM.
  REFRESH: IT_PR_HEAD, IT_PR_ITEM.
  MOVE:  I_ZSMM_IF081 TO V_ZSMM_IF081.

*--Unit conversion from VAATZ. 2006.01.18-------------------------------
*  SELECT SINGLE MSEHI INTO I_ZSMM_IF081-MEINS
*                      FROM T006
*                     WHERE ISOCODE = I_ZSMM_IF081-ZMEINS.
**----------------------------------------------------------------------
*-

*---// interface data type conversion
  PERFORM APPLY_CONVERSION_RULE.

*---// Delete P/R number range
  PERFORM CREATE_NUMBER_RANGE.
  READ TABLE V_RETURN WITH KEY TYPE = 'E'.
  IF SY-SUBRC = 0.
    E_RETURN[] = V_RETURN[].
  ELSE.
    PERFORM CHECK_DEL_ALREADY.
    READ TABLE V_RETURN WITH KEY TYPE = 'E'.
    IF SY-SUBRC = 0.
      E_RETURN[] = V_RETURN[].
      EXIT.
    ELSE.
      PERFORM MANDATAORY_CHK_DEL.

      READ TABLE V_RETURN WITH KEY TYPE = 'E'.
      IF SY-SUBRC = 0.
        E_RETURN[] = V_RETURN[].

        READ TABLE IT_DEL INDEX 1.
        MOVE IT_DEL TO IT_PR_HEAD.
        APPEND IT_PR_HEAD.
        CLEAR  IT_PR_HEAD.

*        INSERT ZTMM_IF006 FROM TABLE IT_PR_ITEM.
        MODIFY ZTMM_IF006 FROM TABLE IT_PR_ITEM.
        UPDATE ZTMM_vaz_IF001 FROM TABLE IT_PR_HEAD.
        COMMIT WORK AND WAIT.
        EXIT.

      ELSE.
** Furong on 06/12/14  (
*        PERFORM DATA_DEL_BAPI.
** )
*---// Modification 2006.01.23----------------------------------*
        READ TABLE V_RETURN WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0.
          LOOP AT V_RETURN.
            MOVE-CORRESPONDING V_RETURN TO E_RETURN.
            APPEND E_RETURN.
            CLEAR  E_RETURN.
          ENDLOOP.
        ELSE.
          PERFORM DELETE_PR.
          LOOP AT V_RETURN.
            MOVE-CORRESPONDING V_RETURN TO E_RETURN.
            APPEND E_RETURN.
            CLEAR  E_RETURN.
          ENDLOOP.
        ENDIF.
*----------------------------------------------------------------------*
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
