FUNCTION Z_VICOM_GEN_BAPIRETURN1.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(P_MSGTY) LIKE  SY-MSGTY
*"             VALUE(P_MSGID) LIKE  SY-MSGID
*"             VALUE(P_MSGNO) LIKE  SY-MSGNO
*"             VALUE(P_MSGV1) LIKE  SY-MSGV1
*"             VALUE(P_MSGV2) LIKE  SY-MSGV2
*"             VALUE(P_MSGV3) LIKE  SY-MSGV3
*"             VALUE(P_MSGV4) LIKE  SY-MSGV4
*"       EXPORTING
*"             VALUE(P_RETURN) TYPE  BAPIRET1
*"----------------------------------------------------------------------

  PERFORM ZD_GEN_BAPIRETURN1
              USING
                 P_MSGTY
                 P_MSGID
                 P_MSGNO
                 P_MSGV1
                 P_MSGV2
                 P_MSGV3
                 P_MSGV4
              CHANGING
                 P_RETURN.

ENDFUNCTION.
