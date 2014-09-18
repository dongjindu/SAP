*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU08_ZNF                                               *
*----------------------------------------------------------------------*
  l_lfgja = f_matbw-bwdat(4).
  l_poper = f_matbw-bwdat+4(2).
  DATA: l_verpr TYPE verpr.

  CALL FUNCTION 'Z_CO_GET_MAP_IG'
       EXPORTING
            matnr = f_matbw-matnr
            poper = l_poper
            bdatj = l_lfgja
       IMPORTING
            verpr = l_verpr
            peinh = exp_peinh
            retro = sy-locdb.

  exp_preis = l_verpr.

* determine plant (p001, e001)

  IF exp_preis IS INITIAL.
    exp_peinh = '9999'.
    exp_preis = '0.01'.
  ENDIF.
