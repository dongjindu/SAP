FUNCTION z_fmm_nstl_tocre.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(IM_ZDOCNO) TYPE  NUM10
*"  TABLES
*"      IMT_DATA_FOR_TO STRUCTURE  ZSMM_DATA_FOR_TO
*"----------------------------------------------------------------------
  LOOP AT imt_data_for_to ASSIGNING <fs_data_for_to>.
**** Begin of Create TO (/nLT01)
* BDC Processing of /nLT01
    PERFORM bdc_processing_lt01 TABLES   it_bdcmsgcoll
                                USING    im_zdocno
                                CHANGING w_subrc.
* Begin of Change TO Header (/nLT1A)
    IF w_subrc = 0.
      CLEAR: wa_bdcmsgcoll.
      READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                               WITH KEY msgtyp = 'S'.
      CHECK sy-subrc = 0.
      PERFORM bdc_processing_lta1
                       TABLES   it_bdcmsgcoll
                       USING    im_zdocno
                                wa_bdcmsgcoll-msgv1  "TO number
                       CHANGING w_subrc.
    ENDIF.
* End of Change TO Header (/nLT1A)
  ENDLOOP.
ENDFUNCTION.
