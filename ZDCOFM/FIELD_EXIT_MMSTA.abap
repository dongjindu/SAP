FUNCTION field_exit_mmsta.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(INPUT)
*"  EXPORTING
*"     REFERENCE(OUTPUT)
*"----------------------------------------------------------------------
* Please consult Andy if you have problem!
  DATA: lw_mstae LIKE mara-mstae.

  IMPORT mara-matnr FROM MEMORY ID 'ZMATERIAL'.
  IMPORT marc-werks FROM MEMORY ID 'ZWRK'.
  lw_mstae = input.

  CASE sy-tcode.
    WHEN 'MM02'.
      SELECT SINGLE * FROM mara WHERE matnr = mara-matnr.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      SELECT SINGLE * FROM marc WHERE matnr = mara-matnr
                                  AND werks = marc-werks.
      IF sy-subrc NE 0.
        MESSAGE e000(zz) WITH text-m01.
      ENDIF.

      CHECK mara-mtart EQ 'ROH' OR
            mara-mtart EQ 'ROH1'.

      CASE marc-mmsta.
        WHEN ' '.
          IF lw_mstae EQ '12'.
            MESSAGE e000(zz) WITH text-m02.
          ENDIF.
        WHEN '11'.
          IF lw_mstae EQ '12'.
*         IF marc-mmsta NE lw_mstae.
            MESSAGE e000(zz) WITH text-m02.
          ENDIF.
      ENDCASE.
  ENDCASE.

  output = input.
ENDFUNCTION.
