REPORT zz_delete_zzekrs .

DATA: it_zzekrs TYPE STANDARD TABLE OF zzekrs,
      lf_count TYPE i.

PARAMETERS: p_test TYPE xfeld DEFAULT 'X'.

START-OF-SELECTION.
  SELECT * FROM zzekrs INTO TABLE it_zzekrs.
  DESCRIBE TABLE it_zzekrs LINES lf_count.
  IF lf_count > 0.
    IF p_test IS INITIAL.
      DELETE zzekrs FROM TABLE it_zzekrs.
      WRITE: / lf_count,
             'entries in table ZZEKRS are deleted.'.
    ELSE.
      WRITE: / lf_count,
             'entries in table ZZEKRS could be deleted.'.
    ENDIF.
  ELSE.
    WRITE: / 'No entries in table ZZEKRS.'.
  ENDIF.
