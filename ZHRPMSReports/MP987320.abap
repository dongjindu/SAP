* OUTPUT modules

*---------------------------------------------------------------------*
*       MODULE INIT_9873                                              *
*---------------------------------------------------------------------*
*       infotype specific initializations                             *
*---------------------------------------------------------------------*
MODULE init_9873 OUTPUT.
* replace with infotype specific coding
  TYPE-POOLS: vrm.

  DATA: l_field   TYPE vrm_id,
        lt_values TYPE vrm_values.

  SELECT role_id AS key role_name AS text
    FROM t77hap_role_t
    INTO TABLE lt_values
    WHERE langu = sy-langu
** By Furong on 02/27/14  ( " for Hiding Supervisor comment
*      AND role_id BETWEEN 'Z0' AND 'Z9'.
      AND role_id BETWEEN 'Z0' AND 'ZA'.
** ) End on 02/27/14
  l_field = 'P9873-ROLE_ID'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_field
      values = lt_values.
ENDMODULE.                    "INIT_9873 OUTPUT

* INPUT modules
