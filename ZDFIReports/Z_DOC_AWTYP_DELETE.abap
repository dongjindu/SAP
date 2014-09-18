REPORT z_doc_awtyp_delete .
TABLES: bkpf.

PARAMETERS: p_bukrs LIKE bkpf-bukrs,
            p_belnr LIKE bkpf-belnr,
            p_gjahr LIKE bkpf-gjahr.
PARAMETERS: p_run AS CHECKBOX.
* We delete BKPF-AWTYP, so that the document can be reversed

IF p_run = 'X'.
  UPDATE bkpf SET awtyp = space
       WHERE bukrs = p_bukrs
       AND   belnr = p_belnr
       AND   gjahr = p_gjahr.

  IF sy-subrc = 0.
    WRITE:/ 'You can reverse now'.
    COMMIT WORK.
  ENDIF.


ELSE.
  SELECT SINGLE * FROM bkpf
       WHERE bukrs = p_bukrs
       AND   belnr = p_belnr
       AND   gjahr = p_gjahr.
  WRITE:/ bkpf-belnr.
ENDIF.
