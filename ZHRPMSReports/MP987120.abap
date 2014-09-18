* OUTPUT modules

*---------------------------------------------------------------------*
*       MODULE INIT_9871                                              *
*---------------------------------------------------------------------*
*       infotype specific initializations                             *
*---------------------------------------------------------------------*
MODULE init_9871 OUTPUT.
* replace with infotype specific coding
  TYPE-POOLS: vrm.

  DATA: l_field   TYPE  vrm_id,
        lt_values TYPE  vrm_values,
        ls_values LIKE LINE OF lt_values.

  SELECT igid AS key igtxt AS text
    FROM zthr_itemg
    INTO TABLE lt_values.

  l_field = 'P9871-IGID'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_field
      values = lt_values.

ENDMODULE.                    "INIT_9871 OUTPUT

* INPUT modules
