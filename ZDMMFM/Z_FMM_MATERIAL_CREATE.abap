FUNCTION z_fmm_material_create.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_BDC_MODE) TYPE  MODE DEFAULT 'N'
*"  TABLES
*"      TA_ZTMM_MAT_02 STRUCTURE  ZTMM_MAT_02
*"----------------------------------------------------------------------
* parameter description : by hj.song 2003.09.18
* 1.ta_ztmm_mat_02    : BDC execute values
*                    Date from EAI -> matnr+maktx+meins+matkl
*                    Data from mat.group table(ztmm_magroup)
*                                  -> include strc ztmm_magroup
* 2.i_bdc_mode       : BDC execution mode
* ---------------------------------------------------------------------
* change history :
*     1. 2003.10.10 by hj.song request by tj.shin
*        -> create material master by storage location level
*     2. 2003.10.16 by hj.song request by tj.shin
*        -> MRP4 view append
*     3. 2003.10.17 by hj.song request by bk.han
*        -> Costing1, 2 view append
*        -> field issue stor.loc, default supply area, stor.loc for EP
*           in MRP 2 view
*     4. 2003.10.29 by hj.song request by Vaatz ebp team
*        -> Processing several line & append interface flag
*     5. 2003.11.27 by hj.song request by sylee
*        -> add create class bapi using F/M Z_FMM_MATERIAL_CREATE
*--------------------------------------------------------------------
* Modification Logs
* Date       Developer    RequestNo   Description
* *********************************************************************
* 12/19/2011 Valerian     UD1K953568  HMMA Engine Plant split
*                                     implementation
* *********************************************************************
  DATA : ls_return       LIKE   bapireturn1,
         ls_return2      LIKE   bapiret2,
         ls_ztmm_mat_02  LIKE   ztmm_mat_02, "material master from Vaatz
         ls_zsmm_mat_01  LIKE   zsmm_mat_01, "bdc structrue
         ls_ztca_if_log  LIKE   ztca_if_log,
         lt_marc         LIKE   TABLE OF marc
                                WITH HEADER LINE,
         ls_ztmm_magroup LIKE   ztmm_magroup.
  "mat.group data from user input
*  CLEAR: ls_ztmm_magroup.
  CLEAR: ls_return, ls_ztmm_mat_02, ls_zsmm_mat_01,
         ls_ztca_if_log,
         lt_marc[], lt_marc,
         ls_ztmm_magroup.

* create material( using BDC )
  LOOP AT ta_ztmm_mat_02 INTO ls_ztmm_mat_02.
* Existence check
    PERFORM existence_check_material
                        USING  ls_ztmm_mat_02-matnr "imp
                               ls_return.           "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'EXT'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.
    IF w_exit_flg EQ 'X'.
      MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
    ENDIF.



    CHECK w_exit_flg NE 'X'.
* Read default value
    PERFORM set_values
                        USING  ls_ztmm_mat_02   "imp
                               ls_zsmm_mat_01.  "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'VAL'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.
    IF w_exit_flg EQ 'X'.
      MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
    ENDIF.



    CHECK w_exit_flg NE 'X'.
* execute 'MM01' when palnt 'P001'.
    PERFORM execute_bdc_mm01_p001
                        USING  ls_zsmm_mat_01   "imp
                               i_bdc_mode.
* making message
    PERFORM making_message
                        USING  ls_return. "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'BDC'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.
    IF w_exit_flg EQ 'X'.
      MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
    ENDIF.



    CHECK w_exit_flg NE 'X'.
* storage location bdc
    PERFORM execute_storage_location
                        USING  ls_zsmm_mat_01  "imp
                               'P001'
                               i_bdc_mode.
* making message
    PERFORM making_message
                        USING  ls_return.  "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'BDC'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.
    IF w_exit_flg EQ 'X'.
      MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
    ENDIF.



    CHECK w_exit_flg NE 'X'.
* execute 'MM01' when palnt 'E001'.
    PERFORM execute_bdc_mm01_e001
                         USING ls_zsmm_mat_01  "imp
                               i_bdc_mode.
* making message
    PERFORM making_message
                         USING ls_return.  "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'BDC'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.
    IF w_exit_flg EQ 'X'.
      MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
    ENDIF.



    CHECK w_exit_flg NE 'X'.
* storage location bdc
    PERFORM execute_storage_location
                        USING ls_zsmm_mat_01  "imp
                              'E001'
                              i_bdc_mode.
* making message
    PERFORM making_message
                        USING ls_return.  "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'BDC'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.

* success
    ls_ztmm_mat_02-mandt   =  sy-mandt.  "client
    ls_ztmm_mat_02-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
    ls_ztmm_mat_02-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
    ls_ztmm_mat_02-zbnam   =  sy-uname.  "BDC User ID
    ls_ztmm_mat_02-zmode   =  'C'.       "C:CREATE
    ls_ztmm_mat_02-zzret   =  'S'.       "success
    ls_ztmm_mat_02-zmsg    =  'Material create'.
    ls_ztmm_mat_02-zresult =  'S'.
    MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.

* BEGIN OF UD1K953568
    CHECK w_exit_flg NE 'X'.
* execute 'MM01' when palnt 'E002'.
    PERFORM execute_bdc_mm01_e002
                         USING ls_zsmm_mat_01  "imp
                               i_bdc_mode.
* making message
    PERFORM making_message
                         USING ls_return.  "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'BDC'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.
    IF w_exit_flg EQ 'X'.
      MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
    ENDIF.



    CHECK w_exit_flg NE 'X'.
* storage location bdc
    PERFORM execute_storage_location
                        USING ls_zsmm_mat_01  "imp
                              'E002'
                              i_bdc_mode.
* making message
    PERFORM making_message
                        USING ls_return.  "exp
* update table & next item
    CLEAR w_exit_flg.
    PERFORM update_error_msg
                        USING  'BDC'          "imp
                               ls_return
                               ls_return2
                               w_exit_flg     "exp
                               ls_ztmm_mat_02.

* success
    ls_ztmm_mat_02-mandt   =  sy-mandt.  "client
    ls_ztmm_mat_02-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
    ls_ztmm_mat_02-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
    ls_ztmm_mat_02-zbnam   =  sy-uname.  "BDC User ID
    ls_ztmm_mat_02-zmode   =  'C'.       "C:CREATE
    ls_ztmm_mat_02-zzret   =  'S'.       "success
    ls_ztmm_mat_02-zmsg    =  'Material create'.
    ls_ztmm_mat_02-zresult =  'S'.
    MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
* END OF UD1K953568
  ENDLOOP.


* material master(from vaatz) temp DB create
  INSERT ztmm_mat_02 FROM TABLE ta_ztmm_mat_02.
  IF  sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

* interface log
  "total count
  DESCRIBE TABLE  ta_ztmm_mat_02 LINES  ls_ztca_if_log-total.
  LOOP AT ta_ztmm_mat_02.
    "success count
    IF     ta_ztmm_mat_02-zzret = 'S'.
      ls_ztca_if_log-zsucc = ls_ztca_if_log-zsucc + 1.
      "error count
    ELSEIF ta_ztmm_mat_02-zzret = 'E'.
      ls_ztca_if_log-error = ls_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  PERFORM interface_log  USING ls_ztca_if_log.


ENDFUNCTION.
