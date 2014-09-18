FUNCTION z_fmm_6001_03_log_to_ztable.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_ZDOCNO) TYPE  NUM10
*"     VALUE(IM_ZTCODE) TYPE  TCODE
*"     VALUE(IM_ZPROGRAMM) TYPE  PROGRAMM
*"     VALUE(IM_TCODE) TYPE  TCODE OPTIONAL
*"     VALUE(IM_FM_NAME) TYPE  RS38L_FNAM OPTIONAL
*"  TABLES
*"      IMT_BDCMSGCOLL STRUCTURE  BDCMSGCOLL OPTIONAL
*"      IMT_BAPIRET2 STRUCTURE  BAPIRET2 OPTIONAL
*"----------------------------------------------------------------------
*/ I defined this function module as update module
*  because manually I avoid number range rollback
*  due to other rollback action.


**** (Begin)BDC Log to the table ZTLOG
  DATA: logno_h TYPE num10.
  CLEAR: logno_h.
  IF NOT imt_bdcmsgcoll[] IS INITIAL.
    PERFORM number_get_next USING    '00'
                                     'ZMMNRO0002'
                            CHANGING logno_h.


    PERFORM bdc_log_to_ztlog TABLES imt_bdcmsgcoll
                             USING  logno_h
                                    im_zdocno
                                    im_ztcode
                                    im_zprogramm.
  ENDIF.

**** (End)BDC Log to the table ZTLOG

**** (Begin)BAPI Log to the table ZTLOG
  CLEAR: logno_h.
  IF NOT imt_bapiret2[] IS INITIAL.
    PERFORM number_get_next USING    '00'
                                     'ZMMNRO0002'
                            CHANGING logno_h.
    PERFORM bapi_log_to_ztlog TABLES imt_bapiret2
                              USING  logno_h
                                     im_zdocno
                                     im_ztcode
                                     im_zprogramm
                                     im_tcode
                                     im_fm_name.
  ENDIF.
**** (End)BAPI Log to the table ZTLOG

ENDFUNCTION.
