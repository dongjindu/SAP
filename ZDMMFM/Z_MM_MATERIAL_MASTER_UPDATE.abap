FUNCTION z_mm_material_master_update.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_BODY) LIKE  ZMMT0037 STRUCTURE  ZMMT0037
*"  EXPORTING
*"     REFERENCE(E_RETURN) LIKE  ZMMS0053 STRUCTURE  ZMMS0053
*"----------------------------------------------------------------------

  DATA : ls_mara LIKE mara.

  CLEAR : g_gubun,  g_check,  g_type,  g_dflag.
  CLEAR : gs_material, st_ret1, g_zseq,
          gs_header, gs_mara, gs_marax,
          gs_marc,   gs_marcx,
          gs_mbew,   gs_mbewx.

  CLEAR : it_return, it_return[],
          gt_makt,   gt_makt[],
          gt_marm,   gt_marm[],
          gt_marmx,  gt_marmx[],
          e_return.

  MOVE-CORRESPONDING i_body TO gs_material.

  CALL FUNCTION 'MARA_SINGLE_READ'
    EXPORTING
      matnr             = gs_material-matnr
    IMPORTING
      wmara             = ls_mara
    EXCEPTIONS
      lock_on_material  = 1
      lock_system_error = 2
      wrong_call        = 3
      not_found         = 4
      OTHERS            = 5.

  MOVE-CORRESPONDING ls_mara TO gs_material.


*-- check material exist in sap.
  PERFORM check_exist_material USING    gs_material-matnr
                               CHANGING st_ret1
                                        g_type
                                        g_dflag
                                        g_check.
*-- work type.
  PERFORM create_material USING gs_material
                                g_type.

*-- Error Check
  READ TABLE it_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    e_return-type    = it_return-type.
    e_return-message = it_return-message.
  else.
    e_return-type    = 'S'.
    e_return-message = 'Success'.
  ENDIF.



ENDFUNCTION.
