FUNCTION z_mm_bapi_hu_create.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_VBELN) TYPE  VBELN_VL OPTIONAL
*"     REFERENCE(I_BUDAT) TYPE  BUDAT OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMT0034
*"----------------------------------------------------------------------

  DATA : lt_m034 LIKE zmmt0034 OCCURS 0 WITH HEADER LINE,
         *vekp   LIKE vekp.

  CLEAR : lt_m034, lt_m034[].

  LOOP AT it_body.

    CLEAR : *vekp.
    SELECT SINGLE *
    INTO *vekp FROM vekp
    WHERE exidv  EQ it_body-bktxt
      AND status NE '0060'.
    IF sy-subrc NE 0.
      MOVE-CORRESPONDING it_body TO lt_m034.
      APPEND lt_m034. CLEAR lt_m034.
    ENDIF.
  ENDLOOP.

  SORT lt_m034 BY bktxt matnr.

  LOOP AT lt_m034.
    AT NEW matnr.
      CLEAR xhuhd.
      REFRESH xhuitem.
      xhuhd-hu_exid         = lt_m034-bktxt.
      xhuhd-pack_mat        = 'BOX'.
      xhuhd-hu_status_init  = 'C'.
      xhuhd-hu_exid_type    = 'F'.
    ENDAT.

    CLEAR xhuitem.
    xhuitem-hu_item_type     = '1'.
    xhuitem-lower_level_exid =  lt_m034-bktxt.

    CALL FUNCTION 'CONVERSION_EXIT_MATN2_INPUT'
      EXPORTING
        input  = lt_m034-matnr
      IMPORTING
        output = xhuitem-material.

    xhuitem-pack_qty         =  lt_m034-menge.
    xhuitem-plant            =  lt_m034-werks.
    xhuitem-stge_loc         =  lt_m034-lgort.
    xhuitem-gr_date          =  lt_m034-budat.
    APPEND xhuitem.

*    AT END OF matnr.
*      PERFORM pack_material CHANGING e_return-message
*                                     e_return-type.
*    ENDAT.
  ENDLOOP.

ENDFUNCTION.
