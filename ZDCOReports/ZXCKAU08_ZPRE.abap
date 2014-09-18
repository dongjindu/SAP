*----------------------------------------------------------------------*
*   INCLUDE ZXCKAU08_ZPRE                                              *
*----------------------------------------------------------------------*

DATA: l_date LIKE sy-datum.
DATA: m_vmgja LIKE marv-vmgja,
      m_vmmon LIKE marv-vmmon,
      m_budat LIKE sy-datum.
DATA: l_mbewh LIKE mbewh,
      l_lfgja LIKE mbewh-lfgja,
      l_lfmon LIKE mbewh-lfmon,
      l_zuonr LIKE ztfi_ctl-zuonr,
      l_noday LIKE ztfi_ctl-noday.

CLEAR: l_zuonr, l_noday.
SELECT SINGLE zuonr noday INTO (l_zuonr, l_noday) FROM ztfi_ctl
  WHERE categ = 'COPC_DI' AND flag  = 'X'.
IF sy-subrc = 0.
  m_budat = '99991231'.  "Force to use exit logic

*Debugging
  IF l_zuonr = f_matbw-matnr.
    BREAK-POINT.
  ENDIF.
ELSE.
*check MM period
  SELECT SINGLE vmgja vmmon INTO (m_vmgja, m_vmmon)
      FROM marv JOIN t001k ON t001k~bukrs = marv~bukrs
      WHERE t001k~bwkey = f_matbw-werks.

  CONCATENATE  m_vmgja  m_vmmon  '01'  INTO  m_budat.
  CALL FUNCTION 'MM_ARRANG_GET_END_OF_MONTH'
       EXPORTING
            i_datum = m_budat
       IMPORTING
            e_datum = m_budat.
ENDIF.

*if valuation for old period, then allow.
CHECK f_matbw-bwdat < m_budat.

* only for past WIP calculation in test mode.
*l_date = f_matbw-bwdat + 30.
*CHECK sy-datum > l_date.

l_lfgja = f_matbw-bwdat(4).
l_lfmon = f_matbw-bwdat+4(2).

*----------------------------
*for past incorrect data...until 03/2006
*check control setting
IF l_noday = 1.
  l_lfmon = l_lfmon + 1.
  IF l_lfmon > 12.
    l_lfmon = 1.
    l_lfgja = l_lfgja + 1.
  ENDIF.
ENDIF.
*----------------------------

SELECT SINGLE * INTO CORRESPONDING FIELDS OF l_mbewh
  FROM mbew
  WHERE matnr = f_matbw-matnr
    AND bwkey = f_matbw-werks
    AND lfgja = l_lfgja
    AND lfmon = l_lfmon.

IF sy-subrc <> 0.
  SELECT * INTO l_mbewh
   FROM mbewh
   WHERE matnr = f_matbw-matnr
     AND bwkey = f_matbw-werks
     AND lfgja = l_lfgja
     AND lfmon <= l_lfmon
   ORDER BY lfgja DESCENDING
            lfmon DESCENDING.
    EXIT.
  ENDSELECT.
  IF sy-subrc <> 0.
    SELECT * INTO l_mbewh
     FROM mbewh
     WHERE matnr = f_matbw-matnr
       AND bwkey = f_matbw-werks
       AND lfgja < l_lfgja
     ORDER BY lfgja DESCENDING
              lfmon DESCENDING.
      EXIT.
    ENDSELECT.
  ENDIF.
ENDIF.

IF sy-subrc = 0.
  exp_peinh = l_mbewh-peinh.
  exp_preis = l_mbewh-stprs.
  exp_waers = f_matbw-waers.
ENDIF.
