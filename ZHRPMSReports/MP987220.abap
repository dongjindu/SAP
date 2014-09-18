* OUTPUT modules

*---------------------------------------------------------------------*
*       MODULE INIT_9872                                              *
*---------------------------------------------------------------------*
*       infotype specific initializations                             *
*---------------------------------------------------------------------*
MODULE init_9872 OUTPUT.
* replace with infotype specific coding
  TYPE-POOLS: vrm.

  DATA: l_field   TYPE vrm_id,
        lt_values TYPE vrm_values.

  SELECT role_id AS key role_name AS text
    FROM t77hap_role_t
    INTO TABLE lt_values
    WHERE langu = sy-langu
      AND role_id BETWEEN 'Z0' AND 'Z9'.

  l_field = 'P9872-ROLE_ID'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_field
      values = lt_values.

ENDMODULE.                    "INIT_9872 OUTPUT

* INPUT modules
