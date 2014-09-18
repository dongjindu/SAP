FUNCTION ZIM_CHANGE_DOCUMENT_BEFORE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFREQNO) LIKE  ZTREQST-ZFREQNO
*"     REFERENCE(W_ZFAMDNO) LIKE  ZTREQST-ZFAMDNO
*"     REFERENCE(W_ZFDOCST) LIKE  ZTREQST-ZFDOCST OPTIONAL
*"     REFERENCE(W_ZFEDIST) LIKE  ZTREQST-ZFEDIST OPTIONAL
*"----------------------------------------------------------------------
   SELECT SINGLE * INTO O_ZTREQST FROM ZTREQST
                   WHERE ZFREQNO EQ W_ZFREQNO
                   AND   ZFAMDNO EQ W_ZFAMDNO.

   N_ZTREQST = O_ZTREQST.
   IF NOT W_ZFDOCST IS INITIAL.
      MOVE : W_ZFDOCST   TO    N_ZTREQST-ZFDOCST.
   ENDIF.
   IF NOT W_ZFEDIST IS INITIAL.
      MOVE : W_ZFEDIST   TO    N_ZTREQST-ZFEDIST.
   ENDIF.

   MOVE : SY-DATUM    TO    N_ZTREQST-UDAT,
          SY-UNAME    TO    N_ZTREQST-UNAM.

   MOVE-CORRESPONDING  N_ZTREQST  TO  ZTREQST.
   UPDATE ZTREQST.

*   IF NOT W_ZFDOCST IS INITIAL.
*      UPDATE ZTREQST SET: ZFDOCST = W_ZFDOCST
*                          udat    = sy-datum
*                          unam    = sy-uname
*                     WHERE ZFREQNO EQ W_ZFREQNO
*                     AND   ZFAMDNO EQ W_ZFAMDNO.
*   ENDIF.
*   IF NOT W_ZFEDIST IS INITIAL.
*      UPDATE ZTREQST SET: ZFEDIST = W_ZFEDIST
*                          UDAT    = SY-DATUM
*                          UNAM    = SY-UNAME
*                     WHERE ZFREQNO EQ W_ZFREQNO
*                     AND   ZFAMDNO EQ W_ZFAMDNO.
*   ENDIF.
* CHANGE DOCUMENT
  CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_STATUS'
     EXPORTING
        W_ZFREQNO      =     W_ZFREQNO
        W_ZFAMDNO      =     W_ZFAMDNO
        N_ZTREQST      =     N_ZTREQST
        O_ZTREQST      =     O_ZTREQST.

ENDFUNCTION.
