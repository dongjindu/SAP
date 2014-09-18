FUNCTION ZIM_IMPORT_DOC_STATUS_CHECK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(ZFREQNO) LIKE  ZTREQHD-ZFREQNO
*"     VALUE(MSG_MODE) TYPE  C DEFAULT 'E'
*"----------------------------------------------------------------------

     SELECT SINGLE * FROM ZTREQST
            WHERE ZFREQNO   EQ   ZFREQNO
            AND   ZFAMDNO   EQ ( SELECT MAX( ZFAMDNO ) FROM ZTREQST
                                 WHERE  ZFREQNO EQ ZFREQNO ).

     CASE ZTREQST-ZFDOCST.
        WHEN 'R'.
          MESSAGE ID 'ZIM' TYPE MSG_MODE NUMBER '104'
                  WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO 'ÀÇ·Ú'.
        WHEN 'A'.
          MESSAGE ID 'ZIM' TYPE MSG_MODE NUMBER '104'
                  WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO 'Amend'.
        WHEN 'C'.
          MESSAGE ID 'ZIM' TYPE MSG_MODE NUMBER '104'
                  WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO 'Cancel'.
        WHEN 'N'.
          MESSAGE ID 'ZIM' TYPE MSG_MODE NUMBER '104'
                  WITH ZTREQST-ZFREQNO ZTREQST-ZFAMDNO 'None'.
     ENDCASE.



ENDFUNCTION.
