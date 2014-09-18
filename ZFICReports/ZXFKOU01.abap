*----------------------------------------------------------------------*
*   INCLUDE ZXFKOU01                                                   *
*----------------------------------------------------------------------*
* RFKORD00 : correspondence program
tables: adrc, adr6.
data: l_land1 like kna1-land1,
      l_mail  like adr6-smtp_addr.


if i_koart = 'D'. " customer
  select single * from adrc
           where addrnumber = i_kna1-ADRNR.

  case adrc-DEFLT_COMM.
    when 'FAX'.
      E_FINAA-NACHA = '2'.        " (1 = print, 2 = fax).
      E_FINAA-TDSCHEDULE = 'IMM'. " ( IMM = Fax immediately).
* Country key from master record
      E_FINAA-TDTELELAND = i_kna1-land1.
* Telefax number from master record
      E_FINAA-TDTELENUM  = adrc-fax_number.

    when 'INT'.  " Internet Mail
      select single * from adr6
             where addrnumber = i_kna1-ADRNR.

      E_FINAA-NACHA = 'I'.          " (1 = print, 2 = fax, I = e-mail)
      E_FINAA-INTAD = adr6-SMTP_ADDR.
      E_FINAA-TEXTF = 'PDF'.        " Text format (PDF or ASCII)
  endcase.

elseif i_koart = 'K'. " vendor
  select single * from adrc
           where addrnumber = i_lfa1-ADRNR.
  case adrc-DEFLT_COMM.
    when 'FAX'.
      E_FINAA-NACHA = '2'.        " (1 = print, 2 = fax).
      E_FINAA-TDSCHEDULE = 'IMM'. " ( IMM = Fax immediately).
* Country key from master record
      E_FINAA-TDTELELAND = i_lfa1-land1.
* Telefax number from master record
      E_FINAA-TDTELENUM  = adrc-fax_number.

    when 'INT'.  " Internet Mail
      select single * from adr6
             where addrnumber = i_lfa1-ADRNR.

      E_FINAA-NACHA = 'I'.          " (1 = print, 2 = fax, I = e-mail)
      E_FINAA-INTAD = adr6-SMTP_ADDR.
      E_FINAA-TEXTF = 'PDF'.        " Text format (PDF or ASCII)

  endcase.

endif.
