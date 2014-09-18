FUNCTION ZIM_CHANGE_DOCUMENT_OFF_STATUS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFREQNO) LIKE  ZTOFF-ZFREQNO
*"     REFERENCE(W_ZFDOCST) LIKE  ZTOFF-ZFDOCST OPTIONAL
*"     REFERENCE(W_ZFEDIST) LIKE  ZTOFF-ZFEDIST OPTIONAL
*"----------------------------------------------------------------------
   DATA :  N_ZTOFFFTX   LIKE   ZTOFFFTX,
           O_ZTOFFFTX   LIKE   ZTOFFFTX,
           O_ZTOFF      LIKE   ZTOFF,
           N_ZTOFF      LIKE   ZTOFF.

   CLEAR : N_ZTOFFFTX, O_ZTOFFFTX.

   SELECT SINGLE * INTO O_ZTOFF   FROM ZTOFF
                   WHERE ZFREQNO EQ W_ZFREQNO.

   N_ZTOFF   = O_ZTOFF.
   IF NOT W_ZFDOCST IS INITIAL.
      MOVE : W_ZFDOCST   TO    N_ZTOFF-ZFDOCST.
   ENDIF.
   IF NOT W_ZFEDIST IS INITIAL.
      MOVE : W_ZFEDIST   TO    N_ZTOFF-ZFEDIST.
   ENDIF.

   MOVE : SY-DATUM    TO    N_ZTOFF-UDAT,
          SY-UNAME    TO    N_ZTOFF-UNAM.

   MOVE-CORRESPONDING  N_ZTOFF    TO  ZTOFF.
   UPDATE ZTOFF.

  CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_OFF'
     EXPORTING
        UPD_CHNGIND    =     'U'
        N_ZTOFF        =     N_ZTOFF
        O_ZTOFF        =     O_ZTOFF
        N_ZTOFFFTX     =     N_ZTOFFFTX
        O_ZTOFFFTX     =     O_ZTOFFFTX.

ENDFUNCTION.
