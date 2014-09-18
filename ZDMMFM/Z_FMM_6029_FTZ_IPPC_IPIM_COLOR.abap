FUNCTION Z_FMM_6029_FTZ_IPPC_IPIM_COLOR.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             REFERENCE(IM_SIGNOFFDATE) TYPE  D
*"       TABLES
*"              EXT_ZTMM_6026_01 STRUCTURE  ZTMM_6026_01 OPTIONAL
*"----------------------------------------------------------------------
  DATA: BEGIN OF ls_equnr,
         equnr LIKE equi-equnr, "Equipment number
        END OF ls_equnr.
  DATA: lt_equnr LIKE TABLE OF ls_equnr.
  FIELD-SYMBOLS: <fs_equnr> LIKE LINE OF lt_equnr.
  DATA: ls_ausp LIKE ausp.
  DATA: lt_ausp LIKE TABLE OF ls_ausp.
  FIELD-SYMBOLS: <fs_ausp> LIKE LINE OF lt_ausp.

  DATA: ls_ztmm_6026_01 LIKE ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.
  FIELD-SYMBOLS: <fs_ztmm_6026_01> LIKE LINE OF lt_ztmm_6026_01.



* For FSC Code
  DATA: lv_p_model_year(10).
  DATA: lv_p_destination_code(10).
  DATA: lv_p_mi(10).
  DATA: lv_p_ocn(10).
  DATA: lv_fsccode(30).
* For Color
  DATA: lv_extcolor(10).
  DATA: lv_intcolor(10).
* For Planned Order
  DATA: lv_plannedorder(10).


*/Variables for bapi_plannedorder_get_detail.
  DATA: lt_componentsdata LIKE TABLE OF bapi_pldordcomp_e1,
        lt_capacitydata1 LIKE TABLE OF bapi_pldordcapa_e1,
        lt_capacitydata2 LIKE TABLE OF bapi_pldordcapa_e1,
        lt_capacitydata3 LIKE TABLE OF bapi_pldordcapa_e1,
        ls_return LIKE bapireturn1,
        ls_headerdata LIKE bapiplaf_e1,
        ls_capacityheaderdata1 LIKE bapi_kbko,
        ls_capacityheaderdata2 LIKE bapi_kbko,
        ls_capacityheaderdata3 LIKE bapi_kbko.

  DATA: ls_componentsdata LIKE LINE OF lt_componentsdata.
  FIELD-SYMBOLS: <fs_componentsdata> LIKE LINE OF lt_componentsdata.


*/Conversion: character type for signoffdate.
  DATA: lv_signoffdate(8).
  lv_signoffdate = im_signoffdate.


*/1. Get Equipments by Sign Off Date
*/ Get Internal characteristic.
  DATA: lv_atinn(10) TYPE n.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_RP18_SHOP_DATE'
       IMPORTING
            output = lv_atinn. "Internal characteristic

*/ Get Equipments.
  SELECT objek INTO CORRESPONDING FIELDS OF TABLE lt_ausp
    FROM ausp
    WHERE
          atinn = lv_atinn AND  "Conversion Rule Needed! ('0000001423')
          klart = '002'    AND  "Class type
          atflv = lv_signoffdate.

  LOOP AT lt_ausp ASSIGNING <fs_ausp>.
    MOVE <fs_ausp>-objek TO ls_equnr-equnr.
    APPEND ls_equnr TO lt_equnr.
  ENDLOOP.


*/2 Get Planned Order Number from Equipment number.
* For bapi_objcl_getclasses and bapi_objcl_getdetail.
  DATA: lt_alloclist   LIKE TABLE OF bapi1003_alloc_list,
        ls_alloclist   LIKE LINE OF lt_alloclist,
        lt_bapiret2    LIKE TABLE OF bapiret2.

  DATA:
    lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
    lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
    lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
    ls_allocvalueschar LIKE LINE OF lt_allocvalueschar,
    lv_objectkey       LIKE bapi1003_key-object,
    lv_objecttable     LIKE bapi1003_key-objecttable,
    lv_classnum        LIKE bapi1003_key-classnum,
    lv_classtype       LIKE bapi1003_key-classtype.

  DATA: lv_stlal LIKE mast-stlal.  "Alternative BOM
*
  lv_objecttable = 'EQUI'.
  lv_classtype   = '002'.


* Get Class number
  LOOP AT lt_equnr ASSIGNING <fs_equnr>.
    lv_objectkey = <fs_equnr>-equnr.   "EQUIPMENT NO
    PERFORM bapi_objcl_getclasses
                      TABLES lt_alloclist
                             lt_bapiret2
                      USING  lv_objectkey    "VIN NO(=Equipment No)
                             lv_objecttable  "EQUI
                             lv_classtype.   "002

    READ TABLE lt_alloclist INTO ls_alloclist
                             WITH KEY classnum = 'P_VEHICLE_MASTER'.

    CHECK sy-subrc = 0.
*    lv_classnum    = ls_alloclist-classnum.
    lv_classnum    = 'P_VEHICLE_MASTER'.

* Get Characteristic Value by Class
    PERFORM bapi_objcl_getdetail
                 TABLES lt_allocvaluesnum
                        lt_allocvalueschar  "Characteristic Data
                        lt_allocvaluescurr
                        lt_bapiret2
                 USING  lv_objectkey   "Equipment number
                        lv_objecttable "Table
                        lv_classnum    "Class number
                        lv_classtype.  "Class type

*Get Sign Off Date
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_RP18_ACTUAL_DATE'.  "Sign Off Date
*    w_signoffdate = ls_allocvalueschar-value_char(8).
    " We already got the sign off date from fm parameter.

*Get External Color
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_EXT_COLOR'.  "External Color
    lv_extcolor = ls_allocvalueschar-value_char.

*Get Internal Color
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_INT_COLOR'.  "Internal Color
    lv_intcolor = ls_allocvalueschar-value_char.

*/Begin of Get FSC CODE
*Get Model Year
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_MODEL_YEAR'.  "MODEL YEAR
    lv_p_model_year = ls_allocvalueschar-value_char.

*Get Destination Code
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_DESTINATION_CODE'.  "Destination Code
    lv_p_destination_code = ls_allocvalueschar-value_char.

*Get Model Index
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_MI'.  "Model Index
    lv_p_mi = ls_allocvalueschar-value_char.

*Get O.C.N
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_OCN'.  "O.C.N
    lv_p_ocn = ls_allocvalueschar-value_char.

*Get FSC Code
    CONCATENATE
      lv_p_model_year
      lv_p_destination_code
      lv_p_mi
    INTO lv_fsccode.

    CONCATENATE
      lv_fsccode
      lv_p_ocn
    INTO lv_fsccode SEPARATED BY space.

    CONCATENATE
      lv_fsccode
      lv_extcolor
      lv_intcolor
      lv_signoffdate
    INTO lv_fsccode.
*/End of Get FSC CODE


*Get Planned Order
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_PLAN_ORDER'.  "Planned Order
    lv_plannedorder = ls_allocvalueschar-value_char.

**/Get Alternative BOM
*    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
*             WITH KEY charact = 'P_VERSION'. "Spec Version
*    CONDENSE ls_allocvalueschar-value_char.
**Delete leading zero
*    PERFORM conversion_exit_alpha_output
*                            USING    ls_allocvalueschar-value_char
*                            CHANGING lv_stlal. "Alternative BOM

*/Make lt_ztmm_6026_01
    MOVE: lv_fsccode      TO ls_ztmm_6026_01-ordernumwork.
    MOVE: lv_extcolor     TO ls_ztmm_6026_01-colorext.
    MOVE: lv_intcolor     TO ls_ztmm_6026_01-colorint.
    MOVE: lv_plannedorder TO ls_ztmm_6026_01-plnum.  "Planned Order

    APPEND ls_ztmm_6026_01 TO lt_ztmm_6026_01.

  ENDLOOP.




*/ Data Refine by ordernumwork
  DATA: lt_temp LIKE TABLE OF ztmm_6026_01.

  SORT lt_ztmm_6026_01 BY ordernumwork.
  DATA: lv_ordernumwork LIKE <fs_ztmm_6026_01>-ordernumwork.
  DATA: lv_count TYPE i.

  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    IF lv_ordernumwork IS INITIAL.
      lv_ordernumwork  = <fs_ztmm_6026_01>-ordernumwork.
    ENDIF.

    IF lv_ordernumwork = <fs_ztmm_6026_01>-ordernumwork.
    ELSE.
      lv_ordernumwork = <fs_ztmm_6026_01>-ordernumwork.
      MOVE lv_count TO ls_ztmm_6026_01-matnr_parent_cnt.
      APPEND ls_ztmm_6026_01 TO lt_temp.
      CLEAR: lv_count.
    ENDIF.
    lv_count = lv_count + 1.

    MOVE <fs_ztmm_6026_01> TO ls_ztmm_6026_01.

    AT LAST.
      MOVE lv_count TO <fs_ztmm_6026_01>-matnr_parent_cnt.
      APPEND <fs_ztmm_6026_01> TO lt_temp.
      CLEAR: lv_count.
    ENDAT.
  ENDLOOP.

  SORT lt_temp BY ordernumwork.
*  DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING ordernumwork.


*/---------------------------------------------
  LOOP AT lt_temp ASSIGNING <fs_ztmm_6026_01>.
    PERFORM bapi_plannedorder_get_detail
        TABLES   lt_componentsdata
                 lt_capacitydata1
                 lt_capacitydata2
                 lt_capacitydata3
*        USING    lv_plannedorder
        USING    <fs_ztmm_6026_01>-plnum
        CHANGING ls_return
                 ls_headerdata
                 ls_capacityheaderdata1
                 ls_capacityheaderdata2
                 ls_capacityheaderdata3.

    LOOP AT lt_componentsdata ASSIGNING <fs_componentsdata>.
*    MOVE: lv_fsccode      TO ext_ztmm_6026_01-ordernumwork.
*    MOVE: lv_extcolor     TO ext_ztmm_6026_01-colorext.
*    MOVE: lv_intcolor     TO ext_ztmm_6026_01-colorint.
*    MOVE: lv_plannedorder TO ext_ztmm_6026_01-plnum.  "Planned Order


      MOVE: <fs_ztmm_6026_01>-ordernumwork
                TO ext_ztmm_6026_01-ordernumwork.
      MOVE: <fs_ztmm_6026_01>-colorext TO ext_ztmm_6026_01-colorext.
      MOVE: <fs_ztmm_6026_01>-colorint TO ext_ztmm_6026_01-colorint.
      MOVE: <fs_ztmm_6026_01>-plnum
                TO ext_ztmm_6026_01-plnum.  "Planned Order
      MOVE: <fs_ztmm_6026_01>-matnr_parent_cnt
                TO ext_ztmm_6026_01-matnr_parent_cnt.  "FSC Count



      MOVE: <fs_componentsdata>-material  TO ext_ztmm_6026_01-matnr.
      MOVE: <fs_componentsdata>-req_quan  TO ext_ztmm_6026_01-qtyperlm.
      "This is the required component qty.
      ext_ztmm_6026_01-menge = ext_ztmm_6026_01-qtyperlm *
                               ext_ztmm_6026_01-matnr_parent_cnt.
      MOVE: <fs_componentsdata>-base_uom  TO ext_ztmm_6026_01-meins.
      MOVE: <fs_componentsdata>-matl_desc TO ext_ztmm_6026_01-maktx.
      APPEND ext_ztmm_6026_01.
    ENDLOOP.
  ENDLOOP.

ENDFUNCTION.
