FUNCTION z_fpp_emission.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(EQUNR) LIKE  EQUI-EQUNR
*"     REFERENCE(EM_DATE) LIKE  SYST-DATUM
*"  EXPORTING
*"     REFERENCE(RCODE) LIKE  ZSPP_VIN_VALUE-ZFLAG
*"  EXCEPTIONS
*"      NO_VEHICLE
*"      NO_ATINN
*"      CHANGE_ERROR
*"----------------------------------------------------------------------
  DATA: l_vals            LIKE TABLE OF zspp_vin_value WITH HEADER LINE,
        L_EMISS           LIKE AUSP-ATINN,
        lw_ausp           LIKE ausp,
        l_EQUI            LIKE EQUI.

  " Check the Vehicle Master
  SELECT SINGLE * INTO l_equi
    FROM equi
   WHERE equnr = equnr .

  IF sy-subrc NE 0.
    RAISE no_vehicle.
  ENDIF.

  " Check the Emission Flag...
  CLEAR: l_emiss.
  PERFORM get_atinn    USING 'P_EMISSION'  l_emiss .

  IF NOT l_emiss IS INITIAL.
    CLEAR: L_VALS, L_VALS[].
    l_vals-atnam = 'P_EMISSION_DATE'.
    l_vals-atwrt = em_date          .    APPEND l_vals.

    SELECT SINGLE * INTO lw_ausp
      FROM ausp
     WHERE objek = equnr
       AND atinn = l_emiss
       AND klart = '002'
       AND atwrt = 'Y'    .

    IF sy-subrc NE 0.
      l_vals-atnam = 'P_EMISSION'.   L_VALS-ATWRT = 'Y'.  APPEND l_vals.
    ENDIF.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              object       = equnr
              mode         = 'W'
         TABLES
              val_table    = l_vals
         EXCEPTIONS
              no_data      = 1
              error_mode   = 2
              error_object = 3
              error_value  = 4
              OTHERS       = 5.

    IF sy-subrc = 0 .
      READ TABLE l_vals INDEX 1.
      IF l_vals-zflag = space  .
        CLEAR: rcode          .
      ELSE.
        rcode = l_vals-zflag  .
      ENDIF.
    ELSE.
      RAISE change_error .
    ENDIF.
  ELSE.
    RAISE no_atinn.
  ENDIF.
ENDFUNCTION.
