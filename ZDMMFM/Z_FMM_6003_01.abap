FUNCTION z_fmm_6003_01.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(SUBRC) LIKE  SYST-SUBRC
*"  TABLES
*"      TA_ZSMM_6003_01 STRUCTURE  ZSMM_6003_01 OPTIONAL
*"      MESSTAB STRUCTURE  BDCMSGCOLL OPTIONAL
*"----------------------------------------------------------------------
  READ TABLE ta_zsmm_6003_01 INDEX 1. "To use header of itab
  subrc = 0.

  ta_zsmm_6003_01-ctu     = 'X'.
  ta_zsmm_6003_01-zmode   = 'N'.
  ta_zsmm_6003_01-zupdate = 'L'.
  ta_zsmm_6003_01-nodata  = '/'.

  ta_zsmm_6003_01-reprf_027 = 'X'.

  PERFORM bdc_nodata      USING ta_zsmm_6003_01-nodata.

* Make bdcdata of MK01(Create Vendor)
  PERFORM open_group      USING ta_zsmm_6003_01-zgroup
                                ta_zsmm_6003_01-zuser
                                ta_zsmm_6003_01-keep
                                ta_zsmm_6003_01-holddate
                                ta_zsmm_6003_01-ctu.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0107'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RF02K-KTOKK'.  "Account group
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '/00'.
  PERFORM bdc_field       USING 'RF02K-LIFNR'   "Vendor
                                ta_zsmm_6003_01-lifnr_001.
  PERFORM bdc_field       USING 'RF02K-EKORG'  "Purchasing organization
                                ta_zsmm_6003_01-ekorg_003.
  PERFORM bdc_field       USING 'RF02K-KTOKK'  "Vendor account group
                                ta_zsmm_6003_01-ktokk_004.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0110'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFA1-TELFX'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.
  PERFORM bdc_field       USING 'LFA1-NAME1'     "Name1
                                ta_zsmm_6003_01-name1_005.
  PERFORM bdc_field       USING 'LFA1-SORTL'     "Search Term
                                ta_zsmm_6003_01-sortl_006.
  PERFORM bdc_field       USING 'LFA1-STRAS'  "House number and street
                                ta_zsmm_6003_01-stras_010.
  PERFORM bdc_field       USING 'LFA1-ORT01'  "City
                                ta_zsmm_6003_01-ort01_012.
  PERFORM bdc_field       USING 'LFA1-PSTLZ'  "Postal code
                                ta_zsmm_6003_01-pstlz_013.
  PERFORM bdc_field       USING 'LFA1-LAND1' "Country key
                                ta_zsmm_6003_01-land1_015.
  PERFORM bdc_field       USING 'LFA1-REGIO' "Region
                                ta_zsmm_6003_01-regio_016.
  PERFORM bdc_field       USING 'LFA1-TELF1'  "First Tel no
                                ta_zsmm_6003_01-telf1_019.
  PERFORM bdc_field       USING 'LFA1-TELFX'  "Fax number
                                ta_zsmm_6003_01-telx1_018.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFA1-KUNNR'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFBK-BANKS(01)'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=VW'.

  PERFORM bdc_dynpro      USING 'SAPMF02K' '0310'.
  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'LFM1-ZTERM'.
  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=UPDA'.
  PERFORM bdc_field       USING 'LFM1-WAERS'
                                ta_zsmm_6003_01-waers_029.
  PERFORM bdc_field       USING 'LFM1-ZTERM'
                                ta_zsmm_6003_01-zterm_030.

  CLEAR: wa_ztca_if_log. "Clear Interface table
  PERFORM bdc_transaction TABLES messtab
  USING                         'MK01'
                                ta_zsmm_6003_01-ctu
                                ta_zsmm_6003_01-zmode
                                ta_zsmm_6003_01-zupdate.
  IF sy-subrc <> 0.
    subrc = sy-subrc.
    ta_zsmm_6003_01-zzret = 'E'.     "Failure
    wa_ztca_if_log-zsucc = 0.        "Success Quantity
    wa_ztca_if_log-error = 1.        "Fail Quantity
*    EXIT.
  ELSE.
    ta_zsmm_6003_01-zzret = 'S'.     "Success
    wa_ztca_if_log-zsucc = 1.        "Success Quantity
    wa_ztca_if_log-error = 0.        "Fail Quantity
  ENDIF.

  MODIFY ta_zsmm_6003_01 INDEX 1.      "Update Internal table

  DATA: logno_h TYPE num10.
  PERFORM number_get_next USING    '00'
                                   'ZMMNRO0002'
                          CHANGING logno_h.

  DATA: ls_ztmm_6003_01 LIKE ztmm_6003_01.
  MOVE-CORRESPONDING ta_zsmm_6003_01 TO ls_ztmm_6003_01.
  MOVE logno_h   TO ls_ztmm_6003_01-logno_h. "Log number

*/ Begin of Time stamp
  MOVE sy-uname  TO ls_ztmm_6003_01-zuser.   "Created User ID
  MOVE sy-datum  TO ls_ztmm_6003_01-zsdat.   "SEND FILE CREATED DATE
  MOVE sy-uzeit  TO ls_ztmm_6003_01-zstim.   "SEND FILE CREATED TIME
  MOVE sy-datum  TO ls_ztmm_6003_01-zedat.   "SAP INTERFACE DATE
  MOVE sy-uzeit  TO ls_ztmm_6003_01-zetim.   "SAP INTERFACE TIME
  MOVE sy-datum  TO ls_ztmm_6003_01-zbdat.   "SAP BDC EXECUTED DATE
  MOVE sy-uzeit  TO ls_ztmm_6003_01-zbtim.   "SAP BDC EXECUTED TIME
  MOVE sy-uname  TO ls_ztmm_6003_01-zbnam.   "BDC User ID
  MOVE 'C'       TO ls_ztmm_6003_01-zmode.   "(Create/Update/Delete)
*Result of the Processing
  MOVE ta_zsmm_6003_01-zzret TO ls_ztmm_6003_01-zresult.
  MOVE space     TO ls_ztmm_6003_01-zmsg.    "Message text
*/ End of Time stamp

* BDC Logging to the tables ZTMM_6003_01 and ZBDCMSGCOLL.
  INSERT INTO ztmm_6003_01 VALUES ls_ztmm_6003_01.
  PERFORM bdc_log_to_zbdcmsgcoll TABLES messtab
                                 USING  logno_h.

***Function Module for Interface Log
*
*Where to be inserted:
* 1. Inbound: When interface table is updated after Standard BDC/BAPI
*             executed.
* 2. Outbound: After calling EAI
*
*====================================================================
*
*Function name : Z_FCA_EAI_INTERFACE_LOG
*
*Import/Export Parameter Structure : ZTCA_IF_LOG
*
*IFDOC   <= Serial No. for Log. Leave as empty
*TCODE   <= Present Transaction Code
*TOTAL   <= Total Execution number
*ZSUCC   <= Successful occurrences(number) for BDC/BAPI Processing
*ERROR   <= Failed occurrences(number) for BDC/BAPI Processing
*ERDAT   <= Created on.
*ERZET   <= Created time.
*ERNAM   <= Creator.
*AEDAT   <= Changed on.
*AEZET   <= Changed time
*AENAM   <= the person who change
  wa_ztca_if_log-tcode = 'MK01'.   "Present Transaction Code
  wa_ztca_if_log-total = 1.        "Total Execution number
  wa_ztca_if_log-erdat = sy-datum. "Created on.
  wa_ztca_if_log-erzet = sy-uname. "Created time.
  wa_ztca_if_log-ernam = sy-uname. "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = wa_ztca_if_log
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

****
  PERFORM close_group USING     ta_zsmm_6003_01-ctu.
ENDFUNCTION.
INCLUDE zbdcrecxy.
