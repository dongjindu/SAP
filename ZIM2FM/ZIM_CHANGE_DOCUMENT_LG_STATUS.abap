FUNCTION ZIM_CHANGE_DOCUMENT_LG_STATUS .
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFBLNO) LIKE  ZTLG-ZFBLNO
*"     REFERENCE(W_ZFLGSEQ) LIKE  ZTLG-ZFLGSEQ
*"     REFERENCE(W_ZFDOCST) LIKE  ZTLG-ZFDOCST
*"     REFERENCE(W_ZFEDIST) LIKE  ZTLG-ZFEDIST
*"----------------------------------------------------------------------
   DATA : O_ZTLG    LIKE    ZTLG,
          N_ZTLG    LIKE    ZTLG.

   CLEAR : O_ZTLG, N_ZTLG.

   SELECT SINGLE * INTO O_ZTLG    FROM ZTLG
                   WHERE ZFBLNO  EQ W_ZFBLNO
                   AND   ZFLGSEQ EQ W_ZFLGSEQ.

   N_ZTLG    = O_ZTLG.
   IF NOT W_ZFDOCST IS INITIAL.
      MOVE W_ZFDOCST    TO  N_ZTLG-ZFDOCST.
   ENDIF.
   IF NOT W_ZFEDIST IS INITIAL.
      MOVE W_ZFEDIST    TO  N_ZTLG-ZFEDIST.
   ENDIF.

   MOVE : SY-DATUM    TO    N_ZTINS-UDAT,
          SY-UNAME    TO    N_ZTINS-UNAM.

   MOVE-CORRESPONDING  N_ZTLG    TO   ZTLG.
   UPDATE ZTLG.

* CHANGE DOCUMENT
  CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_LG'
     EXPORTING
        UPD_CHNGIND    =     'U'
        N_ZTLG         =     N_ZTLG
        O_ZTLG         =     O_ZTLG.

ENDFUNCTION.
