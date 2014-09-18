FUNCTION z_fpp_read_vm_rp.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(EQUIPMENT) TYPE  EQUNR
*"  TABLES
*"      VM_RP STRUCTURE  ZSPP_VIN_VALUE
*"  EXCEPTIONS
*"      NO_DATA
*"----------------------------------------------------------------------

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_vm_new
    FROM ausp AS P INNER JOIN CABN AS B
                     ON P~ATINN = B~ATINN
    WHERE P~objek = equipment AND
          B~atnam LIKE 'P_RP%' AND
          P~klart = '002' .
*
  IF sy-subrc NE 0.
    RAISE no_data.
    EXIT.
  ENDIF.

  MOVE it_vm_new[] TO vm_rp[].

ENDFUNCTION.
