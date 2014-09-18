FUNCTION zmmf_if_vendor.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_VENDOR) LIKE  ZSMM_IF007 STRUCTURE  ZSMM_IF007
*"  TABLES
*"      E_RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------
  DATA: l_vendor LIKE zsmm_if007.
  CLEAR l_vendor.

  MOVE: i_vendor TO l_vendor.

  CLEAR : lfa1.
  SELECT SINGLE lifnr INTO lfa1-lifnr FROM lfa1
  WHERE lifnr = l_vendor-lifnr.
*-- create
  IF lfa1-lifnr IS INITIAL.
    CALL FUNCTION 'ZMMF_IF_CREATE_VENDOR'
      EXPORTING
        i_vendor = l_vendor
      TABLES
        e_return = e_return.
*-- change
  ELSE.
    CALL FUNCTION 'ZMMF_IF_CHANGE_VENDOR'
      EXPORTING
        i_vendor = l_vendor
      TABLES
        e_return = e_return.
  ENDIF.

ENDFUNCTION.
