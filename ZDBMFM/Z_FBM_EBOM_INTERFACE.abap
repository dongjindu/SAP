FUNCTION z_fbm_ebom_interface.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(E_ZZRET) TYPE  ZZRET
*"     VALUE(E_MSG) TYPE  BAPI_MSG
*"  TABLES
*"      T_BOM_ECM STRUCTURE  ZSBM_EBOM_ECM
*"----------------------------------------------------------------------

  DATA: lt_ebom_ecm LIKE ztbm_ebom_ecm OCCURS 0 WITH HEADER LINE.

  DATA: lw_idoc LIKE ztbm_ebom_ecm-idoc.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZPP_BOM_IF'
       IMPORTING
            number                  = lw_idoc
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            OTHERS                  = 8.
  IF sy-subrc <> 0.
    MOVE: 'E'      TO e_zzret,
          text-m02 TO e_msg.

    EXIT.
  ENDIF.

  LOOP AT T_BOM_ECM.
    CLEAR: lt_ebom_ecm.

    MOVE-CORRESPONDING T_BOM_ECM TO lt_ebom_ecm.
    MOVE: lw_idoc  TO lt_ebom_ecm-idoc,
          1        TO lt_ebom_ecm-bqty,
          sy-tabix TO lt_ebom_ecm-item.

    APPEND lt_ebom_ecm.
  ENDLOOP.

  INSERT ztbm_ebom_ecm FROM TABLE lt_ebom_ecm ACCEPTING DUPLICATE KEYS.
  IF sy-subrc NE 0.
    MOVE: 'E'      TO e_zzret,
          text-m03 TO e_msg.
  ELSE.
    COMMIT WORK AND WAIT.
    MOVE: 'S'      TO e_zzret.
  ENDIF.
ENDFUNCTION.
