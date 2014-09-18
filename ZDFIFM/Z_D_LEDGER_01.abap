FUNCTION z_d_ledger_01.
*"----------------------------------------------------------------------
*"*"Global interface:
*"  IMPORTING
*"     VALUE(I_GLUFIX) LIKE  GLUFIX STRUCTURE  GLUFIX
*"     VALUE(I_RGIDL) LIKE  RGIDL STRUCTURE  RGIDL
*"  EXPORTING
*"     VALUE(E_GLUFIX) LIKE  GLUFIX STRUCTURE  GLUFIX
*"----------------------------------------------------------------------
  DATA: l_budat LIKE sy-datum,
        l_fstag LIKE skb1-fstag.

  SELECT SINGLE fstag INTO l_fstag
       FROM skb1 WHERE bukrs = i_glufix-bukrs
                   AND saknr = i_glufix-racct.

  IF l_fstag = 'Y900'.
    concatenate i_rgidl-budat(6) '15' into l_budat.
  ELSE.
    l_budat = i_rgidl-budat.
  ENDIF.

*...... Anfangs- und Enddatum der Periode ermitteln ...................*
  CALL FUNCTION 'G_POSTING_DATE_OF_PERIOD_GET'
       EXPORTING
            period             = i_glufix-poper
            variant            = i_rgidl-periv
            year               = i_glufix-ryear
       IMPORTING
            from_date_orig     = von_datum
            last_normal_period = lastperiod
            to_date            = bis_datum
       EXCEPTIONS
            OTHERS             = 1.

  IF sy-subrc NE 0.
    MESSAGE a153 WITH i_glufix-rldnr i_glufix-bukrs i_rgidl-periv.
  ENDIF.

  IF i_glufix-poper > lastperiod AND   "Sonderperiode
     NOT ( lastperiod IS INITIAL ).
    i_glufix-poper = lastperiod.
  ENDIF.

*...... Wichtung ermitteln (wegen Rundung -> *100) ....................*
  anz_tage = bis_datum - von_datum + 1."total days
  lfd_tage = l_budat - von_datum.     "days from first day
  wichtung = ( anz_tage - lfd_tage ) * 100. "remaining days

*...... Durchschnitte ermitteln .......................................*
  IF i_glufix-tsl NE 0.
    i_glufix-tsl = i_glufix-tsl * wichtung / ( anz_tage * 100 ).
  ENDIF.
  IF i_glufix-hsl NE 0.
    i_glufix-hsl = i_glufix-hsl * wichtung / ( anz_tage * 100 ).
  ENDIF.
  IF i_glufix-ksl NE 0.
    i_glufix-ksl = i_glufix-ksl * wichtung / ( anz_tage * 100 ).
  ENDIF.

  e_glufix = i_glufix.

ENDFUNCTION.
