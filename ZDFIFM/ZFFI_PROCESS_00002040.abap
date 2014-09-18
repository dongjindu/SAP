FUNCTION ZFFI_PROCESS_00002040.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REGUH) TYPE  REGUH
*"  TABLES
*"      T_FIMSG STRUCTURE  FIMSG
*"  CHANGING
*"     VALUE(C_FINAA) LIKE  FINAA STRUCTURE  FINAA
*"----------------------------------------------------------------------
 TYPE-POOLS szadr.
  DATA: l_addr1_complete TYPE szadr_addr1_complete,
        l_adsmtp_line    TYPE szadr_adsmtp_line,
        l_xedip          TYPE lfb1-xedip.

* default: print payment advice
  c_finaa-nacha = '1'.

  CHECK i_reguh-rzawe = 'D'.

  CLEAR l_xedip.
  SELECT SINGLE xedip into l_xedip
    FROM lfb1
   WHERE lifnr = i_reguh-lifnr
     AND bukrs = i_reguh-zbukr.

  CHECK l_xedip IS INITIAL.

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

* BEGIN OF EDGK916118
        LOOP AT l_addr1_complete-adsmtp_tab INTO l_adsmtp_line.
          CONCATENATE c_finaa-intad l_adsmtp_line-adsmtp-smtp_addr
                 INTO c_finaa-intad SEPARATED BY SPACE.
        ENDLOOP.
        CONDENSE c_finaa-intad.

        c_finaa-mail_send_addr = 'hmmatreasury@hmmausa.com'.
* END OF EDGK916118

*       choose message type 'I'nternet and fill email address
        c_finaa-nacha = 'I'.
*       c_finaa-intad = l_adsmtp_line-adsmtp-smtp_addr.     "EDGK916118
        c_finaa-namep = 'ZRFI_PAYMENT_ADVICE'.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFUNCTION.
