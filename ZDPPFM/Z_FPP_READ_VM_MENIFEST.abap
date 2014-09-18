FUNCTION z_fpp_read_vm_menifest.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(EQUIPMENT) TYPE  EQUNR
*"  TABLES
*"      VM_MENIFEST STRUCTURE  ZSPP_VIN_VALUE
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------
  RANGES: lr_atnam FOR cabn-atnam.
  lr_atnam-sign = 'I'.
  lr_atnam-option = 'EQ' .
  MOVE 'P_ENGINE_NO' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_TM_NO' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_KEY_NO' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO1' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO2' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO3' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO4' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO5' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO6' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO7' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO8' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO9' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO10' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO11' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO12' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO13' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO14' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO15' TO lr_atnam-low.
  APPEND lr_atnam.
  MOVE 'P_AIRBAG_NO16' TO lr_atnam-low.
  APPEND lr_atnam.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_vm_new
    FROM ausp AS p INNER JOIN cabn AS b
                     ON p~atinn = b~atinn
    WHERE p~objek = equipment AND
          b~atnam IN lr_atnam AND
          p~klart = '002' .
*
  IF sy-subrc NE 0.
    RAISE no_data.
    EXIT.
  ENDIF.

  MOVE it_vm_new[] TO vm_menifest[].





ENDFUNCTION.
