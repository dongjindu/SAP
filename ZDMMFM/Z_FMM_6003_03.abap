FUNCTION z_fmm_6003_03.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      TA_ZSMM_6003_02 STRUCTURE  ZSMM_6003_02 OPTIONAL
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  CHECK NOT ta_zsmm_6003_02[] IS INITIAL.
  READ TABLE ta_zsmm_6003_02 INDEX 1. "To use header of itab

* VENDOR EXISTENCECHECK
  DATA: lifnr LIKE lfa1-lifnr.
  SELECT SINGLE lifnr INTO lifnr
    FROM lfa1
    WHERE lifnr = ta_zsmm_6003_02-lifnr_001.

  IF sy-subrc <> 0.   "Vendor does not exists..
* Create Vendor
    DATA: lt_zsmm_6003_01 LIKE TABLE OF zsmm_6003_01.
    DATA: ls_zsmm_6003_01 LIKE LINE OF lt_zsmm_6003_01.
    LOOP AT ta_zsmm_6003_02.
      MOVE-CORRESPONDING ta_zsmm_6003_02 TO ls_zsmm_6003_01.
      APPEND ls_zsmm_6003_01 TO lt_zsmm_6003_01.
    ENDLOOP.
    CALL FUNCTION 'Z_FMM_6003_01'
         IMPORTING
              subrc           = subrc
         TABLES
              ta_zsmm_6003_01 = lt_zsmm_6003_01
              messtab         = messtab.
    CLEAR: ta_zsmm_6003_02. REFRESH: ta_zsmm_6003_02.
    LOOP AT lt_zsmm_6003_01 INTO ls_zsmm_6003_01.
      MOVE-CORRESPONDING ls_zsmm_6003_01 TO ta_zsmm_6003_02.
      APPEND ta_zsmm_6003_02.
    ENDLOOP.
  ELSE.                        "Vendor exists
* Change Vendor
    CALL FUNCTION 'Z_FMM_6003_02'
         IMPORTING
              subrc           = subrc
         TABLES
              ta_zsmm_6003_02 = ta_zsmm_6003_02
              messtab         = messtab.
  ENDIF.

**** (Begin)BDC Log to the table ZTLOG
  PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING w_zdocno.     "App Doc No

  CALL FUNCTION 'Z_FMM_6001_03_LOG_TO_ZTABLE'
    IN UPDATE TASK
    EXPORTING
      im_zdocno            = w_zdocno
      im_ztcode            = sy-tcode
      im_zprogramm         = sy-cprog
*            IM_TCODE             =
*            IM_FM_NAME           =
   TABLES
     imt_bdcmsgcoll       = messtab[]
*           IMT_BAPIRET2         =
            .
  COMMIT WORK.
**** (End)BDC Log to the table ZTLOG

ENDFUNCTION.
