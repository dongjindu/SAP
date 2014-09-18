FUNCTION ZIM_GET_SEND_EDI_ID.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(W_ZFCDDOC) TYPE  ZTCDF1-ZFCDDOC
*"     REFERENCE(W_ZFBNARCD) TYPE  ZTIMIMG03-ZFBNARCD
*"  EXPORTING
*"     REFERENCE(W_ZFEDIID) TYPE  ZTIMIMG03-ZFEDIID
*"  EXCEPTIONS
*"      NOT_FOUND
*"      NOT_TYPE
*"----------------------------------------------------------------------

   CLEAR : W_ZFEDIID.

   CASE W_ZFCDDOC.
      WHEN 'CUSCAR' OR 'CUSBRR'.
         SELECT * FROM ZTIMIMG03 UP TO 1 ROWS
                                 WHERE ZFBNARCD EQ W_ZFBNARCD.
         ENDSELECT.

         IF SY-SUBRC EQ 0.
            IF ZTIMIMG03-ZFEDIID IS INITIAL.
               RAISE    NOT_FOUND.
            ELSE.
               W_ZFEDIID = ZTIMIMG03-ZFEDIID.
            ENDIF.
         ELSE.
            RAISE    NOT_FOUND.
         ENDIF.
      WHEN OTHERS.
         RAISE       NOT_TYPE.
   ENDCASE.




ENDFUNCTION.
