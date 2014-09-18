FUNCTION z_fmm_material_create_bapi.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      TA_ZTMM_MAT_02 STRUCTURE  ZTMM_MAT_02
*"      IT_ZSMM_CHARACTER STRUCTURE  ZSMM_CHARACTER
*"----------------------------------------------------------------------
* comment by hjsong 2003.11.11
*>> Parameters desc
* TA_ZTMM_MAT_02 : data from Vaatz system
*  MATNR    -> material code
*  MAKTX    -> material text
*  MATKL    -> mat.group
*  MTART    -> mat.type(ROH1, ERSA, NLAG)
*  MEINS    -> Unit of measure
*  FLAGx    -> 1 : create, change, delete block 0 : delete
*  CLASS    -> class number
*  CHARACT  -> characteristic of class( need to chracteristic )
*  CLASSIFIC-> short charact(need to po text)
*  ZZRET    -> return flag(s:success, e:error)
* IT_ZSMM_CHARACTER : characteristic of class ( need material master )
*>> Frequency : real time job( one by one )

*&--------------------------------------------------------------------&*
*& Date        User          Transport           Description
*& 08/20/2004  Shiva         UD1K911956       Assign manufacturer id
*&                                            and part number.
*& 11/17/2004  Shiva         UD1K913046      Update X-plant status in
*&                                            Tcode MM01 only.
*&                                          Discard charcteristic value
*&                                          from description.
*& 11/18/2004  Shiva         UD1K913072     Set price unit only in MM01.
*& 12/19/2011  Valerian      UD1K953568     HMMA Engine Plant split
*&                                          implementation
*&--------------------------------------------------------------------&*
  DATA :
         ls_return       LIKE   bapireturn1,
         ls_return2      LIKE   bapiret2,
         ls_ztmm_mat_02  LIKE   ztmm_mat_02, "material master from Vaatz
         ls_zsmm_mat_01  LIKE   zsmm_mat_01, "bdc structrue
         ls_ztca_if_log  LIKE   ztca_if_log, "interface
         ls_ztmm_magroup LIKE   ztmm_magroup,
  "mat.group data from user input
         lw_tcode        TYPE   tcode,
         lc_mode(1)      VALUE 'N'.
  CLEAR:
         ls_return, ls_return2,
         ls_ztmm_mat_02, ls_zsmm_mat_01,
         ls_ztca_if_log, ls_ztmm_magroup,
         it_return[], it_return,
         lw_tcode.
  CLEAR: wa_manu_err.
  REFRESH t_manu_err.
*/Begin of Added by Hakchin(20040330)
  FIELD-SYMBOLS: <gf_ztmm_mat_02> LIKE ta_ztmm_mat_02.
*  DATA: lv_iso_char  LIKE t006-isocode.

  DATA: lv_meins LIKE t006-msehi,
        lV_vaatz_MEINS LIKE t006-msehi,
        L_LN TYPE I.
  DATA: BEGIN OF LT_ISOCODE OCCURS 0,
        MSEHI LIKE T006-MSEHI,
        PRIMARY LIKE T006-PRIMARY,
        END OF LT_ISOCODE.

** Changed on 01/06/14 - Check UOM
  LOOP AT ta_ztmm_mat_02 ASSIGNING <gf_ztmm_mat_02>.
** check if it is internal no
    CLEAR: lv_meins.
    lv_vaatz_meins = <gf_ztmm_mat_02>-meins.
    SELECT SINGLE msehi INTO lv_meins
       FROM t006
      WHERE msehi = lv_vaatz_meins.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
** check ISO and Conversion to sap internal uom
    CLEAR: lv_meins.
    REFRESH LT_ISOCODE.
     SELECT MSEHI PRIMARY INTO TABLE LT_ISOCODE
       FROM t006
      WHERE ISOCODE = lv_vaatz_meins.
     DESCRIBE TABLE LT_ISOCODE LINES L_LN.
     IF L_LN > 0.
       IF L_LN = 1.
          READ TABLE LT_ISOCODE INDEX 1.
          lv_meins = LT_ISOCODE-MSEHI.
       ELSE.
          READ TABLE LT_ISOCODE WITH KEY PRIMARY = 'X'.
          lv_meins = LT_ISOCODE-MSEHI.
       ENDIF.
*        PERFORM unit_of_measure_iso_to_sap
*                            USING    lv_iso_char
*                            CHANGING lv_meins.
       IF lv_meins IS NOT INITIAL.
          <gf_ztmm_mat_02>-meins = lv_meins.
          CONTINUE.
       ENDIF.
    ENDIF.
** check external number
    CLEAR: lv_meins.
    SELECT SINGLE msehi INTO lv_meins
        FROM t006A
       WHERE SPRAS = 'EN'
         AND mseh3 = lv_vaatz_meins.
    IF sy-subrc = 0.
      <gf_ztmm_mat_02>-meins = lv_meins.
    ELSE.
**?? process error ???
    ENDIF.

  ENDLOOP.
*/End of Added by Hakchin(20040330)
** End on 1/06/14

  LOOP AT ta_ztmm_mat_02 INTO ls_ztmm_mat_02.
    IF ls_ztmm_mat_02-flagx =  '1'.
      "Mode from Vaatz(1:create, change 0:delete)
*** Existence check
      PERFORM existence_check_material  USING  ls_ztmm_mat_02-matnr "imp
                                               ls_return.           "exp

      CASE  ls_return-type.
******>>>>>Change<<<<<<******
        WHEN 'S'.
          lw_tcode  =  'MM02'.
*** Read default value
          PERFORM set_values               USING  ls_ztmm_mat_02   "imp
                                                  ls_zsmm_mat_01.  "exp
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'U'.   "U:change
          PERFORM update_error_msg         USING  'VAL'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.

*** execute bapi
          CHECK w_exit_flg NE 'X'.
          PERFORM bapi_material_savedata   USING ls_zsmm_mat_01
                                                 lw_tcode
                                                 ls_return2
                                                 'P001'.
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'U'.   "U:change
          PERFORM update_error_msg         USING  'BAPI'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.


*** execute bapi
          CHECK w_exit_flg NE 'X'.
          PERFORM bapi_material_savedata   USING ls_zsmm_mat_01
                                                 lw_tcode
                                                 ls_return2
                                                 'E001'.
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'U'.   "U:change
          PERFORM update_error_msg         USING  'BAPI'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.

* BEGIN OF UD1K953568
*** execute bapi
          CHECK w_exit_flg NE 'X'.
          PERFORM bapi_material_savedata   USING ls_zsmm_mat_01
                                                 lw_tcode
                                                 ls_return2
                                                 'E002'.
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'U'.   "U:change
          PERFORM update_error_msg         USING  'BAPI'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.
* END OF UD1K953568

*** chnage class bapi
          CHECK w_exit_flg NE 'X'.
          PERFORM create_class    TABLES it_zsmm_character "imp
                                  USING  ls_ztmm_mat_02    "imp
                                         ls_return2.       "exp
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
          PERFORM update_error_msg  USING  'CLASS'        "imp
                                           ls_return
                                           ls_return2
                                           w_exit_flg     "exp
                                           ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.

*** save long text
          CHECK w_exit_flg NE 'X'.
          PERFORM po_long_text    USING ls_ztmm_mat_02
                                        ls_return-type.

*** success
          IF w_exit_flg EQ ''.
            ls_ztmm_mat_02-mandt   =  sy-mandt.  "client
            ls_ztmm_mat_02-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
            ls_ztmm_mat_02-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
            ls_ztmm_mat_02-zbnam   =  sy-uname.  "BDC User ID
            ls_ztmm_mat_02-zmode   =  'U'.        "U:change
            ls_ztmm_mat_02-zzret   =  'S'.       "success
            ls_ztmm_mat_02-zmsg    =  'Material change'.
            ls_ztmm_mat_02-zresult =  'S'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.
******>>>>>Create<<<<<<******
        WHEN 'E'.
          lw_tcode  =  'MM01'.
*** Read default value
          PERFORM set_values               USING  ls_ztmm_mat_02   "imp
                                                  ls_zsmm_mat_01.  "exp
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
          PERFORM update_error_msg         USING  'VAL'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.


*** execute bapi
          CHECK w_exit_flg NE 'X'.
          PERFORM bapi_material_savedata   USING ls_zsmm_mat_01
                                                 lw_tcode
                                                 ls_return2
                                                 'P001'.
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
          PERFORM update_error_msg         USING  'BAPI'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.

*** execute bapi
          CHECK w_exit_flg NE 'X'.
          PERFORM bapi_material_savedata   USING ls_zsmm_mat_01
                                                 lw_tcode
                                                 ls_return2
                                                 'E001'.
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
          PERFORM update_error_msg         USING  'BAPI'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.

* BEGIN OF UD1K953568
*** execute bapi
          CHECK w_exit_flg NE 'X'.
          PERFORM bapi_material_savedata   USING ls_zsmm_mat_01
                                                 lw_tcode
                                                 ls_return2
                                                 'E002'.
* update table & next item
          CLEAR w_exit_flg.
          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
          PERFORM update_error_msg         USING  'BAPI'          "imp
                                                  ls_return
                                                  ls_return2
                                                  w_exit_flg     "exp
                                                  ls_ztmm_mat_02.
          IF w_exit_flg EQ 'X'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.
* END OF UD1K953568

*** create class bapi
          IF ls_ztmm_mat_02-class NE ''.
            CHECK w_exit_flg NE 'X'.
            PERFORM create_class    TABLES it_zsmm_character "imp
                                    USING  ls_ztmm_mat_02    "imp
                                           ls_return2.       "exp
            CLEAR w_exit_flg.
            ls_ztmm_mat_02-zmode  =  'C'.   "C:create
            PERFORM update_error_msg  USING  'CLASS'        "imp
                                             ls_return
                                             ls_return2
                                             w_exit_flg     "exp
                                             ls_ztmm_mat_02.
            IF w_exit_flg EQ 'X'.
              MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
            ENDIF.
          ENDIF.

*** save long text
          CHECK w_exit_flg NE 'X'.
          ls_return-type = 'E'.
          PERFORM po_long_text    USING ls_ztmm_mat_02
                                        ls_return-type.

**/Begin of Added by Hakchin(20040429)
****storage location bdc
*          CHECK w_exit_flg NE 'X'.
*          PERFORM execute_storage_location USING  ls_zsmm_mat_01  "imp
*                                                  'P001'
*                                                  lc_mode.
** making message
*          PERFORM making_message   USING  ls_return.  "exp
*
** update table & next item
*          CLEAR w_exit_flg.
*          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
*          PERFORM update_error_msg  USING  'BDC'          "imp
*                                           ls_return
*                                           ls_return2
*                                           w_exit_flg     "exp
*                                           ls_ztmm_mat_02.
*          IF w_exit_flg EQ 'X'.
*            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
*          ENDIF.
*
*
**** storage location bdc
*          CHECK w_exit_flg NE 'X'.
*          PERFORM execute_storage_location USING  ls_zsmm_mat_01  "imp
*                                                  'E001'
*                                                  lc_mode.
** making message
*          PERFORM making_message   USING  ls_return.  "exp
** update table & next item
*          CLEAR w_exit_flg.
*          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
*          PERFORM update_error_msg  USING  'BDC'          "imp
*                                           ls_return
*                                           ls_return2
*                                           w_exit_flg     "exp
*                                           ls_ztmm_mat_02.
*          IF w_exit_flg EQ 'X'.
*            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
*          ENDIF.
*
*
**/End of Added by Hakchin(20040429)

*** success
          IF w_exit_flg EQ ''.
            ls_ztmm_mat_02-mandt   =  sy-mandt.  "client
            ls_ztmm_mat_02-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
            ls_ztmm_mat_02-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
            ls_ztmm_mat_02-zbnam   =  sy-uname.  "BDC User ID
            ls_ztmm_mat_02-zmode   =  'C'.       "C:CREATE
            ls_ztmm_mat_02-zzret   =  'S'.       "success
            ls_ztmm_mat_02-zmsg    =  'Material create'.
            ls_ztmm_mat_02-zresult =  'S'.
            MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
          ENDIF.
******>>>>>Delete block<<<<<<******
        WHEN 'I'.
          lw_tcode  =  'MM06'.
          PERFORM bapi_material_delete_reuse  USING ls_ztmm_mat_02
                                                    lc_mode
                                                    space. "reuse
* making message
          PERFORM making_message   USING  ls_return.  "exp
* update table & next item
          ls_ztmm_mat_02-zmode  =  'C'.   "C:create
          CLEAR w_exit_flg.
          PERFORM update_error_msg  USING  'BDC'          "imp
                                           ls_return
                                           ls_return2
                                           w_exit_flg     "exp
                                           ls_ztmm_mat_02.
*** success
          IF w_exit_flg EQ ''.
            ls_ztmm_mat_02-mandt   =  sy-mandt.  "client
            ls_ztmm_mat_02-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
            ls_ztmm_mat_02-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
            ls_ztmm_mat_02-zbnam   =  sy-uname.  "BDC User ID
            ls_ztmm_mat_02-zmode   =  'C'.       "C:CREATE
            ls_ztmm_mat_02-zzret    =  'S'.       "success
            ls_ztmm_mat_02-zmsg    =  'Material delete block'.
            ls_ztmm_mat_02-zresult =  'S'.
          ENDIF.
          MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
      ENDCASE.

******>>>>>Deletion<<<<<<******
    ELSEIF ls_ztmm_mat_02-flagx =  '0'.
      lw_tcode  =  'MM06'.
      PERFORM bapi_material_delete_reuse  USING ls_ztmm_mat_02
                                                lc_mode
                                                'X'. "delete
* making message
      PERFORM making_message   USING  ls_return.  "exp
* update table & next item
      ls_ztmm_mat_02-zmode  =  'D'.   "D:delete
      CLEAR w_exit_flg.
      PERFORM update_error_msg  USING  'BDC'          "imp
                                       ls_return
                                       ls_return2
                                       w_exit_flg     "exp
                                       ls_ztmm_mat_02.

*** save long text
      PERFORM po_long_text    USING ls_ztmm_mat_02
                                    ls_return-type.

*** success
      IF w_exit_flg EQ ''.
        ls_ztmm_mat_02-mandt   =  sy-mandt.  "client
        ls_ztmm_mat_02-zbdat   =  sy-datum.  "SAP BDC EXECUTED DATE
        ls_ztmm_mat_02-zbtim   =  sy-uzeit.  "SAP BDC EXECUTED TIME
        ls_ztmm_mat_02-zbnam   =  sy-uname.  "BDC User ID
        ls_ztmm_mat_02-zmode   =  'D'.       "D:delete
        ls_ztmm_mat_02-zzret   =  'S'.       "success
        ls_ztmm_mat_02-zmsg    =  'Material delete'.
        ls_ztmm_mat_02-zresult =  'S'.
      ENDIF.
      MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02.
    ENDIF.

  ENDLOOP.

*&----Error log for Manufacturer id
  IF w_manu_err = 'X'.
    READ TABLE ta_ztmm_mat_02 WITH KEY zzret = 'E'.
    IF sy-subrc EQ 0.
    ELSE.
      CLEAR: ta_ztmm_mat_02, ls_ztmm_mat_02.
      READ TABLE t_manu_err INTO wa_manu_err WITH KEY zzret = 'I'.
      IF sy-subrc EQ 0.
        ls_ztmm_mat_02-zmsg = wa_manu_err-zmsg.
        MODIFY ta_ztmm_mat_02 FROM ls_ztmm_mat_02 TRANSPORTING zmsg
                              WHERE matnr = wa_manu_err-matnr.
      ENDIF.
    ENDIF.
  ENDIF.
  FREE: t_manu_err, t_lfa1.

* material master(from vaatz) temp DB create
  MODIFY ztmm_mat_02 FROM TABLE ta_ztmm_mat_02.
  IF  sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

* interface log
  "total count
  DESCRIBE TABLE ta_ztmm_mat_02 LINES  ls_ztca_if_log-total.
  LOOP AT ta_ztmm_mat_02.
    "success count
    IF     ta_ztmm_mat_02-zzret = 'S'.
      ls_ztca_if_log-zsucc = ls_ztca_if_log-zsucc + 1.
      "error count
    ELSEIF ta_ztmm_mat_02-zzret = 'E'.
      ls_ztca_if_log-error = ls_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.
  ls_ztca_if_log-tcode = lw_tcode.   "Present Transaction Code


  PERFORM interface_log  USING ls_ztca_if_log.



ENDFUNCTION.
