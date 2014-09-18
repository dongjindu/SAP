FUNCTION ZMMF_IF_PR_INBOUND.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  TABLES
*"      IT_ZSMM_IF001 STRUCTURE  ZSMM_IF001
*"      E_RETURN STRUCTURE  ZSMM_IF017
*"----------------------------------------------------------------------

  DATA  C_ZSMM_IF001 LIKE ZSMM_IF001.
  DATA  IT_RETN LIKE TABLE OF ZSMM_IF017 WITH HEADER LINE.
  DATA  L_FLAG.

  delete ztmm_pr_log from table IT_ZSMM_IF001.
  insert ztmm_pr_log from table IT_ZSMM_IF001 accepting duplicate keys.
  if sy-subrc = 0.
     commit work.
  endif.

  CLEAR:   C_ZSMM_IF001, IT_RETN, L_FLAG.
  REFRESH: IT_RETN.

  READ TABLE IT_ZSMM_IF001 WITH KEY ZFLAG = 'R'.
  IF SY-SUBRC = 0.
    DELETE IT_ZSMM_IF001 WHERE ZFLAG = 'C'.
    L_FLAG = 'X'.
  ENDIF.

  READ TABLE IT_ZSMM_IF001 WITH KEY ZFLAG = 'D'.
  IF SY-SUBRC = 0.
    DELETE IT_ZSMM_IF001 WHERE ZFLAG = 'C'.
    L_FLAG = 'X'.
  ENDIF.

  IF L_FLAG NE 'X'.

    CALL FUNCTION 'ZMMF_IF_PR_INBOUND_CREATE'
      TABLES
        IT_ZSMM_IF001 = IT_ZSMM_IF001
        E_RETURN      = IT_RETN.

    E_RETURN[] = IT_RETN[].

  ELSE.
    LOOP AT IT_ZSMM_IF001.
      IF IT_ZSMM_IF001-ZFLAG = 'R'.

        CLEAR IT_RETN.REFRESH IT_RETN.
        CALL FUNCTION 'ZMMF_IF_PR_INBOUND_CHANGE'
          EXPORTING
            I_ZSMM_IF001 = IT_ZSMM_IF001
          TABLES
            E_RETURN     = IT_RETN.

        LOOP AT IT_RETN.
          MOVE-CORRESPONDING IT_RETN TO E_RETURN.
          APPEND E_RETURN.
        ENDLOOP.
      ELSEIF IT_ZSMM_IF001-ZFLAG = 'D'.

        CLEAR IT_RETN.REFRESH IT_RETN.
        CALL FUNCTION 'ZMMF_IF_PR_INBOUND_DEL'
          EXPORTING
            I_ZSMM_IF001 = IT_ZSMM_IF001
          TABLES
            E_RETURN     = IT_RETN.

        LOOP AT IT_RETN.
          MOVE-CORRESPONDING IT_RETN TO E_RETURN.
          APPEND E_RETURN.
        ENDLOOP.

      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFUNCTION.
