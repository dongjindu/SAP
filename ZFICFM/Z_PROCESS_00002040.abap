FUNCTION Z_PROCESS_00002040.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(I_REGUH) LIKE  REGUH STRUCTURE  REGUH
*"       TABLES
*"              T_FIMSG STRUCTURE  FIMSG
*"       CHANGING
*"             VALUE(C_FINAA) LIKE  FINAA STRUCTURE  FINAA
*"----------------------------------------------------------------------

* data declaration for address routines
  TYPE-POOLS szadr.
  DATA: l_addr1_complete TYPE szadr_addr1_complete,
        l_adsmtp_line    TYPE szadr_adsmtp_line.

* default: print payment advice
  c_finaa-nacha = '1'.

* check that address number is available
  IF NOT i_reguh-zadnr IS INITIAL.

*   read complete address of vendor/customer
    CALL FUNCTION 'ADDR_GET_COMPLETE'
         EXPORTING
              addrnumber     = i_reguh-zadnr
         IMPORTING
              addr1_complete = l_addr1_complete
         EXCEPTIONS
              OTHERS         = 4.

    IF sy-subrc EQ 0.

*     check that internet address is available
      READ TABLE l_addr1_complete-adsmtp_tab INTO l_adsmtp_line INDEX 1.
      IF sy-subrc EQ 0
      AND NOT l_adsmtp_line-adsmtp-smtp_addr IS INITIAL.

*       choose message type 'I'nternet and fill email address
        c_finaa-nacha = 'I'.
        c_finaa-intad = l_adsmtp_line-adsmtp-smtp_addr.

      ENDIF.
    ENDIF.
  ENDIF.

* if email was not possible: try fax (message type 2)
  IF c_finaa-nacha NE 'I'.
    IF NOT i_reguh-ztlfx IS INITIAL.
      c_finaa-nacha      = '2'.                 "Fax
      c_finaa-tdschedule = 'IMM'.               "Sofort
      c_finaa-tdteleland = i_reguh-zland.	       "Land der Faxnummer
      c_finaa-tdtelenum  = i_reguh-ztlfx.	       "Faxnummer
      c_finaa-formc      = 'FI_FAX_COVER_A4'.   "SAPscript Deckblatt
    ENDIF.
  ENDIF.

ENDFUNCTION.
