*&---------------------------------------------------------------------*
*&  Include           ZXFMYU04
*&---------------------------------------------------------------------*

DATA : L_TABNM(60).

IF I_CAUFVD-AUTYP = '30' AND
  ( I_CAUFVD-AUART = 'PM01' OR
    I_CAUFVD-AUART = 'PM02' OR
    I_CAUFVD-AUART = 'PM03' ).

SELECT SINGLE param_1 FROM tabadrs INTO L_TABNM
  WHERE APPLCLASS = 'FM'
  AND SUBCLASS = '01'
  AND ABADRSTRATID = 'FMOA'
  AND ABADRENV = 'SAP'
  AND STEPID = 'PM'.   "<< Internal Identification

SELECT SINGLE TARGET2 INTO E_FISTL
  FROM (L_TABNM)
  WHERE SOUR1_FROM = 'H201'
  AND SOUR2_FROM = I_CAUFVD-KOSTV
  AND SOUR2_TO = I_CAUFVD-KOSTV.

ENDIF.
