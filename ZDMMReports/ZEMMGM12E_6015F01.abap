*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM15E_6018F01                                          *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
*/1. Case A
  PERFORM get_data_casea.
*/2. Case B
  PERFORM get_data_caseb.
*/3. Case C
  PERFORM get_data_casec.
*  PERFORM get_data_casec_ori.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_data_CASEB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0031   text
*      -->P_0032   text
*----------------------------------------------------------------------*
FORM get_data_caseb.
  CLEAR: it_ztmm_6015_02_ext.
  SELECT mkpf~mblnr AS idnrkmblnr "Number of Material Doc
         mkpf~mjahr AS idnrkmjahr "Material doc. year
         mseg~matnr AS idnrk      "Material
         mseg~werks AS idnrkwerks "Plant
         mseg~meins AS idnrkmeins "Base unit of measure
         mseg~menge AS idnrkmenge "Quantity
         mkpf~budat AS idnrkbudat "Posting date in the document
         mseg~bwart AS idnrkbwart "Movement type
         mseg~grund AS idnrkgrund "Reason for movement
    INTO CORRESPONDING FIELDS OF TABLE it_ztmm_6015_02_ext
    FROM mkpf
    INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr AND
         mseg~bwart  <> '101'    AND  "Not GR to FTZ
         mseg~bwart  <> '261'         "Not Compl.Car GI from FTZ
    INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~profl = 'K'    "KD Material
    WHERE mkpf~budat IN s_period.
  PERFORM make_it_ztmm_6015_02 USING 'B'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  insert_into_ztmm_6015_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM insert_into_ztmm_6015_02.
** App Doc No
*  PERFORM number_get_next USING    w_nro_nr_09     "NRO Interval
*                                   w_nro_object    "NRO Object
*                          CHANGING w_zdocno.     "App Doc No

  DATA: lv_logno_h TYPE num10.
  DATA: lv_logno_d TYPE num10.
* Header Log No.
  PERFORM number_get_next USING    w_nro_nr_00     "NRO Interval
                                   w_nro_object    "NRO Object
                          CHANGING lv_logno_h.     "Log Number Header
  COMMIT WORK.

  LOOP AT it_ztmm_6015_02 ASSIGNING <fs_ztmm_6015_02>.
* Item Log No
    lv_logno_d = lv_logno_d + 1.

    <fs_ztmm_6015_02>-logno_h = lv_logno_h.
    <fs_ztmm_6015_02>-logno_d = lv_logno_d.
  ENDLOOP.

  INSERT ztmm_6015_02 FROM TABLE it_ztmm_6015_02.

ENDFORM.                    " insert_into_ztmm_6015_02
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_09  text
*      -->P_NRO_OBJECT  text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  delete_from_ztmm_6015_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_from_ztmm_6015_02.
  DELETE FROM ztmm_6015_02
    WHERE logno_h <> 'A'.
ENDFORM.                    " delete_from_ztmm_6015_02
*&---------------------------------------------------------------------*
*&      Form  check_kd_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZTMM_6015_02_IDNRK  text
*      <--P_LV_SUBRC  text
*----------------------------------------------------------------------*
FORM check_kd_matnr USING    value(im_matnr) LIKE mara-matnr
                    CHANGING value(ex_subrc) LIKE sy-subrc.
  CLEAR: ex_subrc.
  DATA: lv_profl LIKE mara-profl.

  SELECT SINGLE profl INTO lv_profl
    FROM mara
    WHERE matnr = im_matnr AND
          profl = 'KD'.
  IF sy-subrc = 0.
    CLEAR: ex_subrc.
  ELSE.
    ex_subrc = sy-subrc.
  ENDIF.
ENDFORM.                    " check_kd_matnr
*&---------------------------------------------------------------------*
*&      Form  get_data_casec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0034   text
*      -->P_0035   text
*----------------------------------------------------------------------*
FORM get_data_casec.
  DATA: lv_last_alternative_bom LIKE mast-stlal. "Alternative BOM
  CLEAR: it_ztmm_6015_02_ext.
* For Semifinished products
  SELECT mkpf~mblnr AS srcmblnr "Number of Material Doc
         mkpf~mjahr AS srcmjahr "Material doc. year
         mseg~matnr AS srcmatnr "Material
         mseg~werks AS srcwerks "Plant
         mseg~meins AS srcmeins "Base unit of measure
         mseg~menge AS srcmenge "Quantity
         mkpf~budat AS srcbudat "Posting date in the document
         mseg~bwart AS srcbwart "Movement type
         mseg~grund AS srcgrund "Reason for movement
    INTO CORRESPONDING FIELDS OF TABLE it_ztmm_6015_02_ext
    FROM mkpf   "Header: Material Document
    INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND  "Material doc
         mseg~mjahr = mkpf~mjahr AND  "Material doc. year
         mseg~bwart <> '261'          "Not ( Compl.Car GI from FTZ )
    INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~mtart = 'HALB'     AND "Semifinished products
         mara~profl = 'K'    "KD Material
    WHERE mkpf~budat IN s_period.

* For Finished products
  SELECT mkpf~mblnr AS srcmblnr  "Number of Material Doc
         mkpf~mjahr AS srcmjahr  "Material doc. year
         mseg~matnr AS src_matnr "Material
         mseg~werks AS srcwerks  "Plant
         mseg~meins AS srcmeins  "Base unit of measure
         mseg~menge AS srcmenge  "Quantity
         mkpf~budat AS srcbudat  "Posting date in the document
         mseg~bwart AS srcbwart  "Movement type
         mseg~grund AS srcgrund  "Reason for movement
    APPENDING CORRESPONDING FIELDS OF TABLE it_ztmm_6015_02_ext
    FROM mkpf   "Header: Material Document
    INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND  "Material doc
         mseg~mjahr = mkpf~mjahr AND  "Material doc. year
         mseg~bwart <> '601'          "Not ( GD goods issue:delvy )
    INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~mtart = 'FERT'     AND "Finished products
         mara~profl = 'K'    "KD Material
    WHERE mkpf~budat IN s_period.


  LOOP AT it_ztmm_6015_02_ext ASSIGNING <fs_ztmm_6015_02_ext>.
* Get last alternative bom.
    PERFORM get_last_alternative_bom
                    USING    <fs_ztmm_6015_02_ext>-srcmatnr
                    CHANGING lv_last_alternative_bom.

    PERFORM cs_bom_expl_mat_v2   "similar to /nCS12
             TABLES it_stpox
             USING 'PP01'             "Application ID
                   <fs_ztmm_6015_02_ext>-srcbudat   "Validity date
                   '1'  "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
                   <fs_ztmm_6015_02_ext>-srcmenge
                   "Required quantity (Always '1')
                   'X'    "Multi-level explosion
                   '1'    "Memory use ('1'=on;'0'=off;' '=no reaction)
                   <fs_ztmm_6015_02_ext>-srcmatnr "Source Material
                   lv_last_alternative_bom        "Alternative BOM
                   '1'    "BOM usage (Always 1: Production)
                   <fs_ztmm_6015_02_ext>-srcwerks."Plant

    PERFORM make_it_ztmm_6015_02 USING 'C'.
  ENDLOOP.
ENDFORM.                    " get_data_casec
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getclasses
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM bapi_objcl_getclasses
         TABLES ext_alloclist
                  STRUCTURE bapi1003_alloc_list
                ext_bapiret2
                  STRUCTURE bapiret2
         USING  value(im_objectkey_imp)   LIKE bapi1003_key-object
                value(im_objecttable_imp) LIKE bapi1003_key-objecttable
                value(im_classtype_imp)   LIKE bapi1003_key-classtype.
  CLEAR: ext_alloclist, ext_bapiret2, ext_alloclist[], ext_bapiret2[].
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp         = im_objectkey_imp
      objecttable_imp       = im_objecttable_imp
      classtype_imp         = im_classtype_imp
*   READ_VALUATIONS       =
*   KEYDATE               = SY-DATUM
*   LANGUAGE              = SY-LANGU
    TABLES
      alloclist             = ext_alloclist
*   ALLOCVALUESCHAR       =
*   ALLOCVALUESCURR       =
*   ALLOCVALUESNUM        =
      return                = ext_bapiret2.
ENDFORM.                    "bapi_objcl_getclasses
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getdetail
*&---------------------------------------------------------------------*
FORM bapi_objcl_getdetail
             TABLES ext_allocvaluesnum
                      STRUCTURE bapi1003_alloc_values_num
                    ext_allocvalueschar
                      STRUCTURE bapi1003_alloc_values_char
                    ext_allocvaluescurr
                      STRUCTURE bapi1003_alloc_values_curr
                    ext_bapiret2
                      STRUCTURE bapiret2
             USING  value(im_objectkey)   LIKE bapi1003_key-object
                    value(im_objecttable) LIKE bapi1003_key-objecttable
                    value(im_classnum)    LIKE bapi1003_key-classnum
                    value(im_classtype)   LIKE bapi1003_key-classtype.
  CLEAR: ext_allocvaluesnum,    ext_allocvalueschar,
         ext_allocvaluescurr,   ext_bapiret2,
         ext_allocvaluesnum[],  ext_allocvalueschar[],
         ext_allocvaluescurr[], ext_bapiret2[].
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey              = im_objectkey
      objecttable            = im_objecttable
      classnum               = im_classnum
      classtype              = im_classtype
*   KEYDATE                = SY-DATUM
*   UNVALUATED_CHARS       = ' '
*   LANGUAGE               = SY-LANGU
* IMPORTING
*   STATUS                 =
*   STANDARDCLASS          =
    TABLES
      allocvaluesnum         = ext_allocvaluesnum
      allocvalueschar        = ext_allocvalueschar
      allocvaluescurr        = ext_allocvaluescurr
      return                 = ext_bapiret2.
ENDFORM.                    "bapi_objcl_getdetail
*&---------------------------------------------------------------------*
*&      Form  GET_LAST_Alternative_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_last_alternative_bom
                  USING value(im_matnr)         LIKE mast-matnr
                  CHANGING value(ex_last_stlal) LIKE mast-stlal.
  CLEAR: ex_last_stlal.
  SELECT MAX( stlal ) INTO ex_last_stlal
    FROM mast
    WHERE matnr = im_matnr.
ENDFORM.                    "GET_LAST_Alternative
*&---------------------------------------------------------------------*
*&      Form  cs_bom_expl_mat_v2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STPOX  text
*      -->P_0582   text
*      -->P_<FS_EKET>_EINDT  text
*      -->P_0584   text
*      -->P_<FS_EKET>_OBMNG  text
*      -->P_0586   text
*      -->P_0587   text
*      -->P_<FS_EKET>_MATNR  text
*      -->P_<FS_EKET>_WERKS  text
*----------------------------------------------------------------------*
FORM cs_bom_expl_mat_v2
         TABLES ext_stpox STRUCTURE  stpox
         USING value(p_capid)  LIKE tc04-capid   "Application ID
               value(p_datuv)  LIKE stko-datuv   "Validity date
               value(p_ehndl)  LIKE csdata-xfeld
               "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
               value(p_emeng)  LIKE stko-bmeng   "Required quantity
               value(p_mehrs)  LIKE csdata-xfeld "Multi-level explosion
               "If 'X', then all lower level is exploded.
               value(p_mmory)  LIKE csdata-xfeld
               "Memory use ('1'=on;'0'=off;' '=no reaction)
               value(p_mtnrv)  LIKE mara-matnr   "Material
               value(im_stlal) LIKE mast-stlal   "Alternative BOM
               value(im_stlan) LIKE mast-stlan   "BOM Usage
               value(p_werks)  LIKE  marc-werks. "Plant
  CLEAR: ext_stpox, ext_stpox[].

  CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
   EXPORTING
*       FTREL                       = ' '
*       ALTVO                       = ' '
*       AUFSW                       = ' '
*       AUMGB                       = ' '
*       AUMNG                       = 0
*       AUSKZ                       = ' '
*       AMIND                       = ' '
*       BAGRP                       = ' '
*       BEIKZ                       = ' '
*       BESSL                       = ' '
*       BGIXO                       = ' '
*       BREMS                       = ' '
     capid                       = p_capid                  "'PP01'
*       CHLST                       = ' '
*       COSPR                       = ' '
*       CUOBJ                       = 000000000000000
*       CUOVS                       = 0
*       CUOLS                       = ' '
     datuv                       = p_datuv    " sy-datum
*       DELNL                       = ' '
*       DRLDT                       = ' '
     ehndl                       = p_ehndl    "'1'
     emeng                       = p_emeng    " 10
*       ERSKZ                       = ' '
*       ERSSL                       = ' '
*       FBSTP                       = ' '
*       KNFBA                       = ' '
*       KSBVO                       = ' '
*       MBWLS                       = ' '
*       MKTLS                       = 'X'
*       MDMPS                       = ' '
     mehrs                       = p_mehrs    "'X'
*       MKMAT                       = ' '
*       MMAPS                       = ' '
*       SALWW                       = ' '
*       SPLWW                       = ' '
     mmory                       = p_mmory    "'1'
     mtnrv                       = p_mtnrv                  "'T005'
*       NLINK                       = ' '
*       POSTP                       = ' '
*       RNDKZ                       = ' '
*       RVREL                       = ' '
*       SANFR                       = ' '
*       SANIN                       = ' '
*       SANKA                       = ' '
*       SANKO                       = ' '
*       SANVS                       = ' '
*       SCHGT                       = ' '
*       STKKZ                       = ' '
       stlal                       = im_stlal   "Alternative BOM
       stlan                       = im_stlan   "BOM Usage
*       STPST                       = 0
*       SVWVO                       = 'X'
     werks                       = p_werks                  " 'P001'
*       NORVL                       = ' '
*       MDNOT                       = ' '
*       PANOT                       = ' '
*       QVERW                       = ' '
*       VERID                       = ' '
*       VRSVO                       = 'X'
*     IMPORTING
*       TOPMAT                      =
*       DSTST                       =
    TABLES
      stb                         = ext_stpox
*       MATCAT                      =
   EXCEPTIONS
     alt_not_found               = 1
     call_invalid                = 2
     material_not_found          = 3
     missing_authorization       = 4
     no_bom_found                = 5
     no_plant_data               = 6
     no_suitable_bom_found       = 7
     conversion_error            = 8
     OTHERS                      = 9.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    "CS_BOM_EXPL_MAT_V2
*&---------------------------------------------------------------------*
*&      Form  get_data_casea
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_casea.
**** KD FTZ-Out Quantity Calculation by VPC Completed Car Issue
  DATA: lv_stlal LIKE mast-stlal.  "Alternative BOM

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
*
  lv_objecttable = 'EQUI'.
  lv_classtype   = '002'.

* Get Source Data for Case A
  CLEAR: it_ztmm_6015_02_ext.
  SELECT equnr       "VIN no
         srcmatnr    "Source mat
         srcwerks    "Source mat. plant
         ftzoutdate  "Source mat. FTZ-Out date
         atwre       "Source mat. Ext.color
         atwri       "Source mat. Int.color
    INTO CORRESPONDING FIELDS OF TABLE it_ztmm_6015_02_ext
    FROM ztmm_6015_01
    WHERE ftzoutdate IN s_period.

  CHECK NOT it_ztmm_6015_02_ext IS INITIAL.
  LOOP AT it_ztmm_6015_02_ext ASSIGNING <fs_ztmm_6015_02_ext>.
    lv_objectkey   = <fs_ztmm_6015_02_ext>-equnr."VIN NO(EQUIPMENT NO)

* Get Class number
    PERFORM bapi_objcl_getclasses
                    TABLES lt_alloclist
                           lt_bapiret2
                    USING  lv_objectkey    "VIN NO(=Equipment No)
                           lv_objecttable  "EQUI
                           lv_classtype.   "002
    READ TABLE lt_alloclist INTO ls_alloclist
                             WITH KEY classnum = 'P_VEHICLE_MASTER'.
    CHECK sy-subrc = 0.
    lv_classnum    = ls_alloclist-classnum.
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
    w_signoffdate = ls_allocvalueschar-value_char(8).

*/Get Alternative BOM
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_VERSION'. "Spec Version
    CONDENSE ls_allocvalueschar-value_char.

*Delete leading zero
    PERFORM conversion_exit_alpha_output
                            USING    ls_allocvalueschar-value_char
                            CHANGING lv_stlal. "Alternative BOM

    CALL FUNCTION 'Z_FMM_6015_FTZ_BOM_EXPL'
         EXPORTING
              im_matnr = <fs_ztmm_6015_02_ext>-srcmatnr
              "'2B06AADAFDH6B 3941'
              im_werks = <fs_ztmm_6015_02_ext>-srcwerks     "'P001'
              im_stlan = '1'       "BOM usage (Always 1: Production)
              im_stlal = lv_stlal  "Alternative BOM
              im_atwre = <fs_ztmm_6015_02_ext>-atwre "Ext.Color ('AR' )
              im_atwri = <fs_ztmm_6015_02_ext>-atwri "Int.Color ('TI')
              im_datuv = <fs_ztmm_6015_02_ext>-ftzoutdate
              "Valid from (FTZ-Out Date)
         TABLES
              ext_stpox_alv = it_stpox_alv.
    PERFORM make_it_ztmm_6015_02 USING 'A'.
  ENDLOOP.
ENDFORM.                    " get_data_casea
*&---------------------------------------------------------------------*
*&      Form  make_it_ztmm_6015_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_it_ztmm_6015_02 USING value(im_case).
  DATA: lv_subrc LIKE sy-subrc.
  CASE im_case.
    WHEN 'A'.
      LOOP AT it_stpox_alv INTO wa_stpox_alv.
        CLEAR:  wa_ztmm_6015_02.
* Only KD Material is allowed.
        PERFORM check_kd_matnr USING    wa_ztmm_6015_02-idnrk "Component
                               CHANGING lv_subrc.
        IF lv_subrc <> 0. "Component is not KD Material.
          CONTINUE.
        ENDIF.
        wa_ztmm_6015_02-zcase      = im_case.  "Case
        wa_ztmm_6015_02-srcmatnr   = <fs_ztmm_6015_02_ext>-srcmatnr.
        "'2B06AADAFDH6B 3941'.
        wa_ztmm_6015_02-srcwerks   = <fs_ztmm_6015_02_ext>-srcwerks.
        wa_ztmm_6015_02-srcbwart   = '131'.  "Movement type
        wa_ztmm_6015_02-srcgrund   = space.
        wa_ztmm_6015_02-srcbudat   = w_signoffdate.
        "Sign Off Date(Posting date)
        wa_ztmm_6015_02-idnrk      = wa_stpox_alv-idnrk. "Component
        wa_ztmm_6015_02-idnrkwerks = wa_stpox_alv-werks.
        wa_ztmm_6015_02-idnrkmeins = wa_stpox_alv-meins.
        wa_ztmm_6015_02-idnrkmenge = wa_stpox_alv-menge.
        wa_ztmm_6015_02-idnrkbwart = '131'.  "Movement type
        wa_ztmm_6015_02-idnrkgrund = space.
        wa_ztmm_6015_02-idnrkbudat = w_signoffdate.
        "Sign Off Date(Posting date)
        APPEND wa_ztmm_6015_02 TO it_ztmm_6015_02.
      ENDLOOP.

    WHEN 'B'.
      LOOP AT it_ztmm_6015_02_ext INTO wa_ztmm_6015_02_ext.
        CLEAR:  wa_ztmm_6015_02.

        wa_ztmm_6015_02-zcase      = im_case.  "Case
        wa_ztmm_6015_02-srcmatnr   = space.
        wa_ztmm_6015_02-srcwerks   = space.
        wa_ztmm_6015_02-srcbwart   = space.
        wa_ztmm_6015_02-srcgrund   = space.
        wa_ztmm_6015_02-srcbudat   = space.
        wa_ztmm_6015_02-idnrk      = wa_ztmm_6015_02_ext-idnrk.
        "Component
        wa_ztmm_6015_02-idnrkwerks = wa_ztmm_6015_02_ext-idnrkwerks.
        wa_ztmm_6015_02-idnrkmeins = wa_ztmm_6015_02_ext-idnrkmeins.
        wa_ztmm_6015_02-idnrkmenge = wa_ztmm_6015_02_ext-idnrkmenge.
        wa_ztmm_6015_02-idnrkbwart = wa_ztmm_6015_02_ext-idnrkbwart.
        "Movement type
        wa_ztmm_6015_02-idnrkgrund = wa_ztmm_6015_02_ext-idnrkgrund.
        "Reason for movement
        wa_ztmm_6015_02-idnrkbudat = wa_ztmm_6015_02_ext-idnrkbudat.
        "Posting date

        APPEND wa_ztmm_6015_02 TO it_ztmm_6015_02.
      ENDLOOP.

    WHEN 'C'.
      LOOP AT it_stpox INTO wa_stpox_alv.
        CLEAR:  wa_ztmm_6015_02.
* Only KD Material is allowed.
        PERFORM check_kd_matnr USING    wa_ztmm_6015_02-idnrk "Component
                               CHANGING lv_subrc.
        IF lv_subrc <> 0. "Component is not KD Material.
          CONTINUE.
        ENDIF.
        wa_ztmm_6015_02-zcase      = im_case.  "Case
        wa_ztmm_6015_02-srcmatnr   = <fs_ztmm_6015_02_ext>-srcmatnr.
        "'2B06AADAFDH6B 3941'.
        wa_ztmm_6015_02-srcwerks   = <fs_ztmm_6015_02_ext>-srcwerks.
        wa_ztmm_6015_02-srcbwart   = <fs_ztmm_6015_02_ext>-srcbwart.
        "Movement type
        wa_ztmm_6015_02-srcgrund   = <fs_ztmm_6015_02_ext>-srcgrund.
        wa_ztmm_6015_02-srcbudat   = <fs_ztmm_6015_02_ext>-srcbudat.
        "Posting date
        wa_ztmm_6015_02-idnrk      = wa_stpox-idnrk. "Component
        wa_ztmm_6015_02-idnrkwerks = wa_stpox-werks.
        wa_ztmm_6015_02-idnrkmeins = wa_stpox-meins.
        wa_ztmm_6015_02-idnrkmenge = wa_stpox-menge.
        wa_ztmm_6015_02-idnrkbwart = space.
        "Movement type
        wa_ztmm_6015_02-idnrkgrund = space.
        "Reason for mvt.
        wa_ztmm_6015_02-idnrkbudat = space.
        "Sign Off Date(Posting date)
        APPEND wa_ztmm_6015_02 TO it_ztmm_6015_02.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " make_it_ztmm_6015_02
*&---------------------------------------------------------------------*
*&      Form  conversion_exit_alpha_output
*&---------------------------------------------------------------------*
FORM conversion_exit_alpha_output USING    value(im_input)
                                  CHANGING value(ex_output).
  CLEAR: ex_output.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
       EXPORTING
            input  = im_input
       IMPORTING
            output = ex_output.
ENDFORM.                    "conversion_exit_alpha_output
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_log.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM mask_columns
 TABLES   p_it_fieldcat STRUCTURE it_fieldcat.
* Build the fieldcat according to DDIC structure ZTMM_6019_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZTMM_6015_02'
       CHANGING
            ct_fieldcat      = p_it_fieldcat[].

* Make Column header
  LOOP AT p_it_fieldcat.
    IF p_it_fieldcat-fieldname = 'LOGNO_H'.
      p_it_fieldcat-coltext = 'Log Header'.
    ELSEIF p_it_fieldcat-fieldname = 'LOGNO_D'.
      p_it_fieldcat-coltext = 'Log Item'.
    ELSEIF p_it_fieldcat-fieldname = 'SRCMATNR'.
      p_it_fieldcat-outputlen = 18.
    ELSEIF p_it_fieldcat-fieldname = 'IDNRK'.
      p_it_fieldcat-outputlen = 18.
    ELSEIF p_it_fieldcat-fieldname = 'LOGNO_D'.
      p_it_fieldcat-coltext = 'Log Item'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSDAT'.
      p_it_fieldcat-no_out = 'X'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSTIM'.
      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY p_it_fieldcat.
  ENDLOOP.
ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  get_data_casec_ori
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_casec_ori.

  DATA: lv_last_alternative_bom LIKE mast-stlal.
  CLEAR: it_ztmm_6015_02_ext.
* For Semifinished products
  SELECT mkpf~mblnr AS srcmblnr "Number of Material Doc
         mkpf~mjahr AS srcmjahr "Material doc. year
         mseg~matnr AS srcmatnr "Material
         mseg~werks AS srcwerks "Plant
         mseg~meins AS srcmeins "Base unit of measure
         mseg~menge AS srcmenge "Quantity
         mkpf~budat AS srcbudat "Posting date in the document
         mseg~bwart AS srcbwart "Movement type
         mseg~grund AS srcgrund "Reason for movement
    INTO CORRESPONDING FIELDS OF TABLE it_ztmm_6015_02_ext
    FROM mkpf   "Header: Material Document
    INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND  "Material doc
         mseg~mjahr = mkpf~mjahr AND  "Material doc. year
         mseg~bwart <> '261'          "Not ( Compl.Car GI from FTZ )
    INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~mtart = 'HALB'     AND "Semifinished products
         mara~profl = 'K'    "KD Material
    WHERE mkpf~budat IN s_period.

* For Finished products
  SELECT mkpf~mblnr AS srcmblnr  "Number of Material Doc
         mkpf~mjahr AS srcmjahr  "Material doc. year
         mseg~matnr AS src_matnr "Material
         mseg~werks AS srcwerks  "Plant
         mseg~meins AS srcmeins  "Base unit of measure
         mseg~menge AS srcmenge  "Quantity
         mkpf~budat AS srcbudat  "Posting date in the document
         mseg~bwart AS srcbwart  "Movement type
         mseg~grund AS srcgrund  "Reason for movement
    APPENDING CORRESPONDING FIELDS OF TABLE it_ztmm_6015_02_ext
    FROM mkpf   "Header: Material Document
    INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND  "Material doc
         mseg~mjahr = mkpf~mjahr AND  "Material doc. year
         mseg~bwart <> '601'          "Not ( GD goods issue:delvy )
    INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~mtart = 'FERT'     AND "Finished products
         mara~profl = 'K'    "KD Material
    WHERE mkpf~budat IN s_period.


  LOOP AT it_ztmm_6015_02_ext ASSIGNING <fs_ztmm_6015_02_ext>.
* Get last alternative bom.
    PERFORM get_last_alternative_bom
                    USING    <fs_ztmm_6015_02_ext>-srcmatnr
                    CHANGING lv_last_alternative_bom.

    PERFORM cs_bom_expl_mat_v2   "similar to /nCS12
             TABLES it_stpox
             USING 'PP01'             "Application ID
                   <fs_ztmm_6015_02_ext>-srcbudat   "Validity date
                   '1'  "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
                   <fs_ztmm_6015_02_ext>-srcmenge
                   "Required quantity (Always '1')
                   'X'    "Multi-level explosion
                   '1'    "Memory use ('1'=on;'0'=off;' '=no reaction)
                   <fs_ztmm_6015_02_ext>-srcmatnr "Source Material
                   lv_last_alternative_bom        "Alternative BOM
                   '1'    "BOM usage (Always 1: Production)
                   <fs_ztmm_6015_02_ext>-srcwerks."Plant

    PERFORM make_it_ztmm_6015_02 USING 'C'.
  ENDLOOP.
ENDFORM.                    " get_data_casec_ori
