FUNCTION Z_FPP_CHECK_WO_SEQUENCE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(CHECK) TYPE  ZCHECK DEFAULT 'X'
*"     REFERENCE(F_WO) LIKE  ZTPP_WOSUM-WO_SER OPTIONAL
*"     REFERENCE(T_WO) LIKE  ZTPP_WOSUM-WO_SER OPTIONAL
*"  TABLES
*"      T_SEQUENCE STRUCTURE  ZSPP_SEQUENCE OPTIONAL
*"----------------------------------------------------------------------

  CASE CHECK.
    WHEN ' '.                   " ZTPP_PMT07JB_B
         PERFORM CHECK_7JB      TABLES T_SEQUENCE .
    WHEN 'X'.                   " ZTPP_WOSUM
         PERFORM CHECK_ALL      TABLES T_SEQUENCE
                                USING F_WO T_WO.
    WHEN 'V'.                   " EQUI-AUSP
         PERFORM CHECK_VEHICLE  TABLES T_SEQUENCE .
    WHEN 'W'.                   " MARA-AUSP
         PERFORM CHECK_WORDER   TABLES T_SEQUENCE .
  ENDCASE.

  LOOP AT T_SEQUENCE.
    IF T_SEQUENCE-CL_CNT = T_SEQUENCE-WO_CNT AND
       T_SEQUENCE-CL_CNT = T_SEQUENCE-SO_CNT AND
       T_SEQUENCE-CL_SEQ = T_SEQUENCE-SO_SEQ AND
       T_SEQUENCE-CL_SEQ = T_SEQUENCE-WO_SEQ .
       T_SEQUENCE-FLAG   = ' ' .
    ELSE.
       T_SEQUENCE-FLAG   = 'X' .
    ENDIF.
    MODIFY T_SEQUENCE.
  ENDLOOP.
ENDFUNCTION.
