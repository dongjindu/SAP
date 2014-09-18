*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM29I_6026F01                                          *
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
*/ I determined the importance for a program maintenance but not
*/ a performance. (If the performance is more important, you can
*/ also change this program. At this time you may create some
*/ useful views or from several selections to one selection. )
*/ Get data for FIFO Processing

********************************************************
  CLEAR: it_ztmm_6026_01.
*/ 1. RPPC
  PERFORM get_rppc.

*/ 2. RPIM
  PERFORM get_rpim.

*/ 3. RNPC
  PERFORM get_rnpc.

*/ 4. SPPC
  PERFORM get_sppc.

*/ 5. PNIM
  PERFORM get_pnim.

*/ 6. APPC
  PERFORM get_appc.

*/ 7. APIM
  PERFORM get_apim.

*/ 8. ANIM
  PERFORM get_anim.

*/ 9. ANPC
  PERFORM get_anpc.

*/ 10. PPIM
*  PERFORM get_ppim.
*  PERFORM get_ppim_by_color.
  PERFORM get_ppim_new.

*&Added by Shiva                          08/12/04
  select matnr werks from marc
                     into table t_marc
                     where ( beskz = 'E' and lgpro = 'F001' )
                     OR    ( beskz = 'E' and lgpro = 'E110' ).
*&End add

*/ 11.IPPC & 12.IPIM
  IF rb3     = 'X'.   "From Material Documents
    PERFORM get_ippc_ipim_mseg.
  ELSEIF rb4 = 'X'.   "From Planned Oreders
    PERFORM get_ippc_ipim_fsc.  " FSC
*    PERFORM get_ippc_engine.    " Engine etc
*    PERFORM get_ipim_engine.    " Engine etc
    PERFORM get_ippc_ipim_mseg_engine. "Engine etc
  ELSEIF rb5 = 'X'.   "From BOM Explosion
    PERFORM get_ippc_ipim_bom_fsc.
*    PERFORM get_ippc_engine.    " Engine etc
*    PERFORM get_ipim_engine.    " Engine etc
    PERFORM get_ippc_ipim_mseg_engine. "Engine etc

  ENDIF.

*/ 13. IPPC
  PERFORM get_ippc.

*/ 14. IPIM
  PERFORM get_ipim.

*/ 15. INPC
*  PERFORM get_inpc.
*&------------------shiva commented.
*  PERFORM get_inpc_by_onw_matnr.

*/ 16. INIM
*  PERFORM get_inim.
*&------------------shiva commented.
*  PERFORM get_inim_by_onw_matnr.

*/ 17. INPC
  PERFORM get_inpc2.

*/ 18. INIM
  PERFORM get_inim2.

*/ 19. CPPC
  PERFORM get_cppc.

*/ 20. CNPC
  PERFORM get_cnpc.

*/ 21. XPPC
  PERFORM get_xppc.

*/ 22. XPIM
  PERFORM get_xpim.

*/ 23. XNPC
  PERFORM get_xnpc.

*/ 24. XNIM
  PERFORM get_xnim.

*/ 25. SPIM
*  PERFORM get_spim.
  PERFORM get_spim_pp.
  PERFORM get_spim_601.

*/ 26. RNIM
*  PERFORM get_rnim.  "This data is useless.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ztmm_6026_01_LOGNO_H  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
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
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data
          USING value(im_zdocno) TYPE num10
                imt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  STATICS: lv_logno_h TYPE num10.
  DATA: lv_zresult LIKE zsca_if_time_stamp_out-zresult.
  DATA: lv_message TYPE bapi_msg. "Message text (220)
  CONSTANTS : c_dest(10) VALUE 'WMGM01'.

*/ Call Outbound RFC FM
  CALL FUNCTION 'Z_FMM_6026_OUT_MATFIFO'
    DESTINATION              c_dest
    TABLES
      ext_ztmm_6026_01      = imt_ztmm_6026_01
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_message
      system_failure        = 2 MESSAGE lv_message.
  IF sy-subrc NE 0.
    lv_zresult = 'E'.  "Result of the Processing
    MESSAGE s999(zmmm) WITH lv_message.
  ELSE.
    lv_zresult = 'S'.  "Result of the Processing
    lv_message = 'Outbound RFC FM Connected!'(002).
    MESSAGE s999(zmmm) WITH lv_message.
  ENDIF.

*/ Modify it_ztmm_6026_01
*  CLEAR: lv_logno_h.
  LOOP AT imt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
*    PERFORM number_get_next USING    '00'
*                                     'ZMMNRO0002'
*                            CHANGING lv_logno_h.
    lv_logno_h = lv_logno_h + 1.

    <fs_ztmm_6026_01>-zdocno  = im_zdocno.  "App. Doc. No.
    <fs_ztmm_6026_01>-logno_h = lv_logno_h."Logno Header

    <fs_ztmm_6026_01>-zuser   = sy-uname.  "User name
*    <fs_ztmm_6026_01>-zsdat   = .  "Send File Created Date
*    <fs_ztmm_6026_01>-zstim   = .  "Send file Created Time
    <fs_ztmm_6026_01>-zedat   = w_date.    "SAP Interface Date
    <fs_ztmm_6026_01>-zetim   = w_time.    "SAP Interface Time
    <fs_ztmm_6026_01>-zmode   = 'C'.       "Data Characteristic Flag
    <fs_ztmm_6026_01>-zresult = lv_zresult."Result of the Processing
    <fs_ztmm_6026_01>-zmsg    = lv_message."Message text
*    <fs_ztmm_6026_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ztmm_6026_01.
  INSERT ztmm_6026_01 FROM TABLE imt_ztmm_6026_01.
ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fca_eai_interface_log.
*/ Function Module for Interface Log
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

  DATA: lv_total TYPE i.
  DESCRIBE TABLE it_ztmm_6026_01 LINES lv_total.

  CHECK NOT lv_total IS INITIAL.
  CLEAR: wa_ztca_if_log.
  LOOP AT it_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    IF <fs_ztmm_6026_01>-zzret = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF <fs_ztmm_6026_01>-zzret = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  wa_ztca_if_log-tcode = 'ZMMI76'. "Present Transaction Code
  wa_ztca_if_log-total = lv_total. "Total Execution number
  wa_ztca_if_log-erdat = w_date.   "Created on.
  wa_ztca_if_log-erzet = w_time.   "Created time.
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
ENDFORM.                    " z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*&      Form  dsp_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dsp_log.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " dsp_log
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM mask_columns TABLES   p_it_fieldcat STRUCTURE it_fieldcat.
* Build the fieldcat according to DDIC structure ztmm_6026_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ztmm_6026_01'
       CHANGING
            ct_fieldcat      = p_it_fieldcat[].

* Make Column header
  LOOP AT p_it_fieldcat.
    IF p_it_fieldcat-fieldname = 'ZDOCNO'.
      p_it_fieldcat-coltext = 'App.DocNo.'.
    ELSEIF p_it_fieldcat-fieldname = 'LOGNO_H'.
      p_it_fieldcat-coltext = 'Log No.'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSDAT'.
      p_it_fieldcat-no_out = 'X'.
    ELSEIF p_it_fieldcat-fieldname = 'ZSTIM'.
      p_it_fieldcat-no_out = 'X'.
    ELSEIF p_it_fieldcat-fieldname = 'PARTNERID'.
      p_it_fieldcat-coltext = 'PartnerID'.
    ELSEIF p_it_fieldcat-fieldname = 'EFFDATE'.
      p_it_fieldcat-coltext = 'Effective Date'.
    ELSEIF p_it_fieldcat-fieldname = 'TXNCODE'.
      p_it_fieldcat-coltext = 'TxnCode'.
    ELSEIF p_it_fieldcat-fieldname = 'TXNDATE'.
      p_it_fieldcat-coltext = 'TxnDate'.
    ELSEIF p_it_fieldcat-fieldname = 'ORDERNUMRECEIPT'.
      p_it_fieldcat-coltext = 'OrderNumReceipt'.
    ELSEIF p_it_fieldcat-fieldname = 'ORDERNUMWORK'.
      p_it_fieldcat-coltext = 'OrderNumWork'.
    ELSEIF p_it_fieldcat-fieldname = 'ORDERNUMSHIP'.
      p_it_fieldcat-coltext = 'OrderNumShip'.
    ELSEIF p_it_fieldcat-fieldname = 'MATNR'.
      p_it_fieldcat-outputlen = 18.
    ELSEIF p_it_fieldcat-fieldname = 'PTC'.
      p_it_fieldcat-coltext = 'ProductTypeCode'.
    ELSEIF p_it_fieldcat-fieldname = 'PTCSRC'.
      p_it_fieldcat-coltext = 'ProductTypeCodeSource'.
    ELSEIF p_it_fieldcat-fieldname = 'MAKTXSRC'.
      p_it_fieldcat-coltext = 'ProductDescSource'.
    ELSEIF p_it_fieldcat-fieldname = 'NAFTACERTIFIED'.
      p_it_fieldcat-coltext = 'NaftaCertified'.
    ELSEIF p_it_fieldcat-fieldname = 'NAFTACERTIFIEDSC'.
      p_it_fieldcat-coltext = 'NaftaCertifiedSource'.
    ELSEIF p_it_fieldcat-fieldname = 'MEINSSRC'.
      p_it_fieldcat-coltext = 'UOMSource'.
    ELSEIF p_it_fieldcat-fieldname = 'NTGEWSRC'.
      p_it_fieldcat-coltext = 'NetWeightSource'.
    ELSEIF p_it_fieldcat-fieldname = 'GEWEISRC'.
      p_it_fieldcat-coltext = 'WeightUnitSource'.
    ELSEIF p_it_fieldcat-fieldname = 'COUNTRYSHIPTO'.
      p_it_fieldcat-coltext = 'CountryShipTo'.
    ELSEIF p_it_fieldcat-fieldname = 'TRANSPORTID'.
      p_it_fieldcat-coltext = 'TransportID'.
    ELSEIF p_it_fieldcat-fieldname = 'RECEIPTDOCID'.
      p_it_fieldcat-coltext = 'ReceiptDocID'.
    ELSEIF p_it_fieldcat-fieldname = 'EXITDOCID'.
      p_it_fieldcat-coltext = 'ExitDocID'.
    ELSEIF p_it_fieldcat-fieldname = 'ADJRECEIPTDOCID'.
      p_it_fieldcat-coltext = 'AdjReceiptDocID'.
    ELSEIF p_it_fieldcat-fieldname = 'ADJPRODUCTNUM'.
      p_it_fieldcat-coltext = 'AdjProductNum'.
    ELSEIF p_it_fieldcat-fieldname = 'FROMZONEID'.
      p_it_fieldcat-coltext = 'FromZoneID'.
    ELSEIF p_it_fieldcat-fieldname = 'TOZONEID'.
      p_it_fieldcat-coltext = 'ToZoneID'.
    ELSEIF p_it_fieldcat-fieldname = 'MODEOFTRANSPORT'.
      p_it_fieldcat-coltext = 'ModeOfTransport'.
    ELSEIF p_it_fieldcat-fieldname = 'RECEIPTDATE'.
      p_it_fieldcat-coltext = 'ReceiptDate'.
    ELSEIF p_it_fieldcat-fieldname = 'ITNUM'.
      p_it_fieldcat-coltext = 'FullITNum'.
    ELSEIF p_it_fieldcat-fieldname = 'BILLOFLADING'.
      p_it_fieldcat-coltext = 'BillOfLading'.
    ELSEIF p_it_fieldcat-fieldname = 'EXPORTDATE'.
      p_it_fieldcat-coltext = 'ExportDate'.
    ELSEIF p_it_fieldcat-fieldname = 'VALIDFLAG'.
      p_it_fieldcat-coltext = 'ValidFlag'.
    ELSEIF p_it_fieldcat-fieldname = 'ASSIGNMENTFLAG'.
      p_it_fieldcat-coltext = 'AssignmentFlag'.
    ELSEIF p_it_fieldcat-fieldname = 'FIFOFLAG'.
      p_it_fieldcat-coltext = 'FifoFlag'.
    ELSEIF p_it_fieldcat-fieldname = 'DELETEDFLAG'.
      p_it_fieldcat-coltext = 'DeletedFlag'.
    ELSEIF p_it_fieldcat-fieldname = 'KEEPDURINGROLLBA'.
      p_it_fieldcat-coltext = 'KeepDuringRollBack'.
    ELSEIF p_it_fieldcat-fieldname = 'STAWNSRC'.
      p_it_fieldcat-coltext = 'HTSCodeSource'.
    ELSEIF p_it_fieldcat-fieldname = 'STATUSCODE'.
      p_it_fieldcat-coltext = 'StatusCode'.
    ELSEIF p_it_fieldcat-fieldname = 'STATUSCODESRC'.
      p_it_fieldcat-coltext = 'StatusCodeSource'.
    ELSEIF p_it_fieldcat-fieldname = 'SPICODE1'.
      p_it_fieldcat-coltext = 'SpiCode1'.
    ELSEIF p_it_fieldcat-fieldname = 'SPICODE1SRC'.
      p_it_fieldcat-coltext = 'SpiCode1Source'.
    ELSEIF p_it_fieldcat-fieldname = 'SPICODE2'.
      p_it_fieldcat-coltext = 'SpiCode2'.
    ELSEIF p_it_fieldcat-fieldname = 'SPICODE2SRC'.
      p_it_fieldcat-coltext = 'SpiCode2Source'.
    ELSEIF p_it_fieldcat-fieldname = 'LAND1SRC'.
      p_it_fieldcat-coltext = 'CountrySource'.
    ELSEIF p_it_fieldcat-fieldname = 'LIFNRSRC'.
      p_it_fieldcat-coltext = 'VendorSource'.
    ELSEIF p_it_fieldcat-fieldname = 'RELFLAG'.
      p_it_fieldcat-coltext = 'RelationshipFlag'.
    ELSEIF p_it_fieldcat-fieldname = 'RELFLAGSRC'.
      p_it_fieldcat-coltext = 'RelationshipFlagSource'.
    ELSEIF p_it_fieldcat-fieldname = 'HTSINDEX'.
      p_it_fieldcat-coltext = 'HTSIndex'.
    ELSEIF p_it_fieldcat-fieldname = 'HTSINDEXSRC'.
      p_it_fieldcat-coltext = 'HTSIndexSource'.
    ELSEIF p_it_fieldcat-fieldname = 'HTSDESC'.
      p_it_fieldcat-coltext = 'HTSDesc'.
    ELSEIF p_it_fieldcat-fieldname = 'HTSDESCSRC'.
      p_it_fieldcat-coltext = 'HTSDescSource'.
    ELSEIF p_it_fieldcat-fieldname = 'HTSNUM2'.
      p_it_fieldcat-coltext = 'HTSNum2'.
    ELSEIF p_it_fieldcat-fieldname = 'HTSNUM2SRC'.
      p_it_fieldcat-coltext = 'HTSNum2Source'.
    ELSEIF p_it_fieldcat-fieldname = 'VALUESRC'.
      p_it_fieldcat-coltext = 'ValueSource'.
    ELSEIF p_it_fieldcat-fieldname = 'VALUE2'.
      p_it_fieldcat-coltext = 'Value2'.
    ELSEIF p_it_fieldcat-fieldname = 'VALUE2SRC'.
      p_it_fieldcat-coltext = 'Value2Source'.
    ELSEIF p_it_fieldcat-fieldname = 'WAERSSRC'.
      p_it_fieldcat-coltext = 'CurrencySource'.
    ELSEIF p_it_fieldcat-fieldname = 'NETPRUOM'. "
      p_it_fieldcat-coltext = 'NetPr UOM'.
    ELSEIF p_it_fieldcat-fieldname = 'EFFPRUOM'. "
      p_it_fieldcat-coltext = 'EffPr UOM'.
    ELSEIF p_it_fieldcat-fieldname = 'ALTVALUESRC'.
      p_it_fieldcat-coltext = 'AltValueSource'.
    ELSEIF p_it_fieldcat-fieldname = 'ALTVALUE2SRC'.
      p_it_fieldcat-coltext = 'AltValue2Source'.
    ELSEIF p_it_fieldcat-fieldname = 'ALTCURRCODE'.
      p_it_fieldcat-coltext = 'AltCurrCode'.
    ELSEIF p_it_fieldcat-fieldname = 'ALTCURRCODESRC'.
      p_it_fieldcat-coltext = 'AltCurrCodeSource'.
    ELSEIF p_it_fieldcat-fieldname = 'ADVALOREMRATESRC'.
      p_it_fieldcat-coltext = 'AdValoremRateSource'.
    ELSEIF p_it_fieldcat-fieldname = 'SPECIFICRATESRC'.
      p_it_fieldcat-coltext = 'SpecificRateSource'.
    ELSEIF p_it_fieldcat-fieldname = 'UOMCONVFACTORSRC'.
      p_it_fieldcat-coltext = 'UomConvFactorSource'.
    ELSEIF p_it_fieldcat-fieldname = 'ADDUOMCONVFACSRC'.
      p_it_fieldcat-coltext = 'AddUomConvFactorSource'.
    ELSEIF p_it_fieldcat-fieldname = 'RPTQTYUOM'.
      p_it_fieldcat-coltext = 'RptQtyUOM'.
    ELSEIF p_it_fieldcat-fieldname = 'RPTQTYUOMSRC'.
      p_it_fieldcat-coltext = 'RptQtyUomSrc'.
    ELSEIF p_it_fieldcat-fieldname = 'ADDRPTQTYUOM'.
      p_it_fieldcat-coltext = 'AddRptQtyUOM'.
    ELSEIF p_it_fieldcat-fieldname = 'ADDRPTQTYUOMSRC'.
      p_it_fieldcat-coltext = 'AddRptQtyUomSource'.
    ELSEIF p_it_fieldcat-fieldname = 'DOTINDICATOR'.
      p_it_fieldcat-coltext = 'DotIndicator'.
    ELSEIF p_it_fieldcat-fieldname = 'FCCINDICATOR'.
      p_it_fieldcat-coltext = 'FccIndicator'.
    ELSEIF p_it_fieldcat-fieldname = 'FDAINDICATOR'.
      p_it_fieldcat-coltext = 'FdaIndicator'.
    ELSEIF p_it_fieldcat-fieldname = 'INFNR'. "purchasing info record
      p_it_fieldcat-no_out = 'X'.
    ELSEIF p_it_fieldcat-fieldname = 'PROFL'. "MIP/LP/KD
    ELSEIF p_it_fieldcat-fieldname = 'MTART'. "Material type
    ELSEIF p_it_fieldcat-fieldname = 'BWART'. "Movement type
    ELSEIF p_it_fieldcat-fieldname = 'MAT_KDAUF'.
      "Sales order number of valuated sales order stock
    ELSEIF p_it_fieldcat-fieldname = 'MAT_KDPOS'.
      "Sales Order Item of Valuated Sales Order Stock
    ELSEIF p_it_fieldcat-fieldname = 'COLOREXT'.
      p_it_fieldcat-coltext = 'External Color'.
    ELSEIF p_it_fieldcat-fieldname = 'COLORINT'.
      p_it_fieldcat-coltext = 'Internal Color'.
    ELSEIF p_it_fieldcat-fieldname = 'PLNUM'.
      p_it_fieldcat-coltext = 'Planned order number'.
    ELSEIF p_it_fieldcat-fieldname = 'DATE_TIME'.
      p_it_fieldcat-coltext = 'Date_Time'.


*    ELSEIF p_it_fieldcat-fieldname = 'LIFNR'.
*      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_it_fieldcat-fieldname = 'LAND1'.
*      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_it_fieldcat-fieldname = 'NETPR'.
*      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_it_fieldcat-fieldname = 'WAERS'.
*      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY p_it_fieldcat.
  ENDLOOP.
ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  get_netpr_waers
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ztmm_6026_01>_MATNR  text
*      <--P_<FS_ztmm_6026_01>_NETPR  text
*      <--P_<FS_ztmm_6026_01>_WAERS  text
*----------------------------------------------------------------------*
FORM get_netpr_waers USING    value(im_matnr)
                     CHANGING value(ex_netpr)
                              value(ex_waers).
  CLEAR: ex_netpr, ex_waers.
  SELECT SINGLE eine~netpr eine~waers
    INTO (ex_netpr, ex_waers)
    FROM eina
      INNER JOIN eine
      ON eine~infnr = eina~infnr   "Info Record Number
  WHERE eina~matnr = im_matnr.
ENDFORM.                    " get_netpr_waers
*&---------------------------------------------------------------------*
*&      Form  get_netpr_effpr_waers
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6026_01>_MATNR  text
*      <--P_<FS_ZTMM_6026_01>_LIFNR  text
*      <--P_<FS_ZTMM_6026_01>_NETPR  text
*      <--P_<FS_ZTMM_6026_01>_EFFPR  text
*      <--P_<FS_ZTMM_6026_01>_WAERS  text
*----------------------------------------------------------------------*
FORM get_netpr_effpr_waers USING    value(im_matnr)
                     CHANGING value(ex_lifnr)
                              value(ex_netpr)
                              value(ex_effpr)
                              value(ex_waers)
                              value(ex_bpumz)
                              value(ex_bpumn)
                              value(ex_peinh). "Price Unit
  CLEAR: ex_lifnr, ex_netpr, ex_effpr, ex_waers, ex_bpumz, ex_bpumn,
         ex_peinh.
  SELECT SINGLE eina~lifnr eine~netpr eine~effpr eine~waers
                eine~bpumz eine~bpumn eine~peinh
    INTO (ex_lifnr, ex_netpr, ex_effpr, ex_waers,
          ex_bpumz, ex_bpumn, ex_peinh)
    FROM eina
      INNER JOIN eine
      ON eine~infnr = eina~infnr   "Infor Record Number
  WHERE eina~matnr = im_matnr.

ENDFORM.                    " get_netpr_effpr_waers
*&---------------------------------------------------------------------*
*&      Form  get_vendorinfo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6026_01>_LIFNR  text
*      <--P_<FS_ZTMM_6026_01>_NAME1  text
*      <--P_<FS_ZTMM_6026_01>_LAND1  text
*----------------------------------------------------------------------*
FORM get_vendorinfo USING    value(im_lifnr)
                    CHANGING ex_land1.
  SELECT SINGLE
         lfa1~land1    "Country
    INTO (ex_land1)
    FROM lfa1
      INNER JOIN adrc
        ON adrc~addrnumber = lfa1~adrnr
    WHERE lfa1~lifnr = im_lifnr.
ENDFORM.                " get_vendorinfo
*&---------------------------------------------------------------------*
*&      Form  get_rppc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rppc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln as ebeln
      mseg~lifnr        AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart  "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr AND
         mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~profl ='V'   "LP material
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  WHERE
        ( mseg~bwart = '101' OR "GR goods receipt
          mseg~bwart = '501' OR "Receipt w/o PO
          mseg~bwart = '511' ) AND "Delivery w/o charge
        ( mara~mtart ='ROH' OR  "Production Materials
          mara~mtart ='HALB' OR  "Semifinished products
          mara~mtart ='FERT' OR  "Finished products
          mara~mtart ='ROH1' ) AND  "Raw / Sub Material
         mkpf~budat = p_budat
  GROUP BY mseg~matnr
           mseg~lifnr
           mkpf~budat
           mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_rppc
*&---------------------------------------------------------------------*
*&      Form  get_rpim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rpim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
       mseg~ebeln       AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr AND
           mseg~kzbew = 'F'" Movement indicator
           "F: Goods movement for production order
      INNER JOIN mara
        ON mara~matnr = mseg~matnr
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
*/Begin of for Test
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*        "SEA: HMMA ENGINE PLANT ASSY
*/End of for Test
    WHERE
        ( " mseg~bwart = '101' OR "GR goods receipt
          mseg~bwart = '501' OR "Receipt w/o PO
          mseg~bwart = '511'
        )                          AND "Delivery w/o charge
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_rpim
*&---------------------------------------------------------------------*
*&      Form  get_appc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_appc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      mseg~lifnr        AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
        ON mara~matnr = mseg~matnr
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
    WHERE
        ( mseg~bwart = '702' OR  "GI phys.inv.: whse
          mseg~bwart = '712' OR  "GR InvDiff.:wrhouse
*/Begin of Added by Hakchin(20040513)(Need by KBLEE)
          mseg~bwart = '543'     "GI subcon cust stk
*/End of Added by Hakchin(20040513)(Need by KBLEE)
        )                         AND
        ( mara~mtart ='ROH' OR   "Production Materials
          mara~mtart ='HALB' OR  "Semifinished products
          mara~mtart ='FERT' OR  "Finished products
          mara~mtart ='ROH1'     "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
        mkpf~budat = p_budat
     GROUP BY mseg~matnr
              mseg~lifnr
              mkpf~budat
              mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_appc
*&---------------------------------------------------------------------*
*&      Form  get_apim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_apim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'F'" Movement indicator
           "F: Goods movement for production order
      INNER JOIN mara
        ON mara~matnr = mseg~matnr "AND
*           mara~profl ='M'
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( ( ( mseg~bwart = '702' OR    "GI phys.inv.: whse
              mseg~bwart = '712'       "GR InvDiff.:wrhouse
            )                          AND
            ( mara~mtart ='FERT'   OR "Finished products
              ( mara~profl ='M' AND
                ( mara~mtart ='ROH'  OR "Production Materials
                  mara~mtart ='HALB' OR "Semifinished products
                  mara~mtart ='ROH1'    "Raw / Sub Material
                )
              )
            )
          )                      OR
          ( mseg~bwart = '651' AND "GD ret.del.  returns
            mara~mtart ='HALB'     "Semifinished products
          )
        )                                     AND
        mkpf~budat = p_budat
    GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_apim
*&---------------------------------------------------------------------*
*&      Form  get_anim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_anim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln         AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'F'" Movement indicator
           "F: Goods movement for production order
      INNER JOIN mara
        ON mara~matnr = mseg~matnr "AND
*           mara~profl ='M'
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '701' OR    "GR phys.inv.: whse
          mseg~bwart = '711'       "GI InvDiff.:whouse
        )                           AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_anim
*&---------------------------------------------------------------------*
*&      Form  get_anpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_anpc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart  "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
        ON mara~matnr = mseg~matnr
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
    WHERE
        ( mseg~bwart = '701' OR    "GR phys.inv.: whse
          mseg~bwart = '711'       "GI InvDiff.:whouse
        )                         AND
        ( mara~mtart ='ROH' OR     "Production Materials
          mara~mtart ='HALB' OR    "Semifinished products
          mara~mtart ='FERT' OR    "Finished products
          mara~mtart ='ROH1'       "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_anpc
*&---------------------------------------------------------------------*
*&      Form  get_ppim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ppim.
  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE it_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr AND
         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
*      INNER JOIN marc
*      ON marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '131' ) AND "Goods receipt
        ( mara~mtart ='ROH'  OR    "Production Materials
          mara~mtart ='HALB' OR    "Semifinished products
          mara~mtart ='FERT' OR    "Finished products
          mara~mtart ='ROH1' ) AND "Raw / Sub Material
        mkpf~budat = p_budat
    GROUP BY mseg~matnr mkpf~budat.
ENDFORM.                    " get_ppim
*&---------------------------------------------------------------------*
*&      Form  get_pnim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pnim.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr" AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        (
          (
            mseg~bwart = '102' AND "GR for PO   reversal
            mara~mtart ='HALB' AND "Semifinished products
            mara~profl ='M'
          )                           OR
          (
            mseg~bwart = '132'     AND "Goods receipt
            (
              mara~mtart ='FERT' OR "Finished products
              mara~mtart ='HALB'    "Semifinished products
            )                      AND
            mara~profl ='M'
          )
        )                                     AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

*/Make OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    IF <fs_ztmm_6026_01>-bwart = '132'.
      CONCATENATE <fs_ztmm_6026_01>-matnr p_budat
        INTO <fs_ztmm_6026_01>-ordernumwork.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_pnim
*&---------------------------------------------------------------------*
*&      Form  get_ippc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ippc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        as ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
    WHERE
        ( mseg~bwart = '901' OR "
          mseg~bwart = '905'
        )                         AND
        ( mara~mtart ='ROH' OR  "Production Materials
          mara~mtart ='ROH1' OR    "Raw / Sub Material
          mara~mtart ='HALB'    "Raw / Sub Material     ADDED SHIVA
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
    GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_ippc
*&---------------------------------------------------------------------*
*&      Form  get_ipim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ipim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN marc
        ON marc~matnr = mseg~matnr AND
           marc~fevor = 'SEA' "Production scheduler
      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '901' OR "
          mseg~bwart = '905'
        ) AND "
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='ROH1' OR   "Raw / Sub Material
              mara~mtart ='HALB'  "Semifinished products
            )
          )
        )                           AND
        mkpf~budat = p_budat
    GROUP BY mseg~matnr mkpf~budat mseg~ebeln.
**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_ipim
*&---------------------------------------------------------------------*
*&      Form  get_inpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_inpc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
*/Begin of Added by Hakchin(20040414)
      MAX( afpo~matnr ) AS ordernumreceipt
*/End of Added by Hakchin(20040414)
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu

      INNER JOIN afpo   "Order item
        ON afpo~aufnr = mseg~aufnr AND  "Order number
           afpo~posnr = '0001'   "Order Item number

  WHERE
        ( mseg~bwart = '262' )    AND
        ( mara~mtart ='ROH' OR  "Production Materials
          mara~mtart ='ROH1'    "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat.

*/Begin of Added by Hakchin(20040414)
*/OrderNumReceipt
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    CONCATENATE <fs_ztmm_6026_01>-ordernumreceipt p_budat
        INTO <fs_ztmm_6026_01>-ordernumreceipt.
  ENDLOOP.
*/End of Added by Hakchn(20040414)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_inpc
*&---------------------------------------------------------------------*
*&      Form  get_inim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_inim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
*/Begin of Added by Hakchin(20040414)
      MAX( afpo~matnr ) AS ordernumreceipt
*/End of Added by Hakchin(20040414)
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN marc
        ON marc~matnr = mseg~matnr AND
           marc~fevor = 'SEA' "Production scheduler
      "SEA: HMMA ENGINE PLANT ASSY
*/Begin of Addec by Hakchin(20040414)
      INNER JOIN afpo   "Order item
        ON afpo~aufnr = mseg~aufnr AND  "Order number
           afpo~posnr = '0001'   "Order Item number
*/End of Addec by Hakchin(20040414)
    WHERE
        (
          mseg~bwart = '262'     AND
          (
            mara~mtart ='FERT' OR "Finished products
            mara~mtart ='HALB'    "Semifinished products
          )                      AND
          mara~profl ='M'
        )                                     AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat.

*/Begin of Added by Hakchin(20040414)
*/OrderNumReceipt
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    CONCATENATE <fs_ztmm_6026_01>-ordernumreceipt p_budat
        INTO <fs_ztmm_6026_01>-ordernumreceipt.
  ENDLOOP.
*/End of Added by Hakchn(20040414)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_inim
*&---------------------------------------------------------------------*
*&      Form  get_inpc2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_inpc2.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
       mseg~ebeln       as ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  WHERE
        ( mseg~bwart = '902' OR "
          mseg~bwart = '906'
        )                         AND
        ( mara~mtart ='ROH' OR   "Production Materials
          mara~mtart ='HALB' OR  "Semifinished products
          mara~mtart ='FERT' OR  "Finished products
          mara~mtart ='ROH1'     "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                                                    " get_inpc2
*&---------------------------------------------------------------------*
*&      Form  get_inim2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_inim2.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN marc
        ON marc~matnr = mseg~matnr AND
           marc~fevor = 'SEA' "Production scheduler
      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '902' OR "
          mseg~bwart = '906'
        )                          AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~budat = p_budat
    GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                                                    " get_inim2
*&---------------------------------------------------------------------*
*&      Form  get_xppc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_xppc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  WHERE
        ( mseg~bwart = '201' OR "
          mseg~bwart = '551' OR "
          mseg~bwart = '555' OR
          mseg~bwart = '903' OR
          mseg~bwart = '907'
        )                         AND
        ( ( ( mara~mtart ='ROH' OR   "Production Materials
              mara~mtart ='ROH1'     "Raw / Sub Material
            )                       AND
            ( mara~profl = 'K' OR
              mara~profl = 'V'
            )
          )                               OR
          ( mara~mtart ='HALB'      AND
            mara~profl = 'K'
          )
        )                                  AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_xppc
*&---------------------------------------------------------------------*
*&      Form  get_xpim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_xpim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN marc
        ON marc~matnr = mseg~matnr "AND
*           marc~fevor = 'SEA' "Production scheduler
      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '201' OR "
          mseg~bwart = '551' OR "
          mseg~bwart = '555' OR
          mseg~bwart = '991'
        )                         AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                            AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_xpim
*&---------------------------------------------------------------------*
*&      Form  get_xnpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_xnpc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  WHERE
        ( mseg~bwart = '202' OR "
          mseg~bwart = '552' OR "
          mseg~bwart = '556' OR
          mseg~bwart = '904' OR
          mseg~bwart = '908'
        )                        AND
        ( mara~mtart ='ROH' OR  "Production Materials
          mara~mtart ='ROH1'    "Raw / Sub Material
        )                        AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_xnpc
*&---------------------------------------------------------------------*
*&      Form  get_xnim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_xnim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN marc
        ON marc~matnr = mseg~matnr AND
           marc~fevor = 'SEA' "Production scheduler
      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '202' OR
          mseg~bwart = '552' OR
          mseg~bwart = '556' OR
          mseg~bwart = '992'
        )                           AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_xnim
*&---------------------------------------------------------------------*
*&      Form  set_txncode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_txncode.
  CHECK <fs_ztmm_6026_01>-txncode IS INITIAL.
  "K:Knock Down Parts, M:MIP, V:Local Parts
  IF ( <fs_ztmm_6026_01>-bwart = '101' OR
       <fs_ztmm_6026_01>-bwart = '501' OR
       <fs_ztmm_6026_01>-bwart = '511' ) AND
     <fs_ztmm_6026_01>-profl = 'V'.   "LP
    <fs_ztmm_6026_01>-txncode = 'RPPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '501' OR
           <fs_ztmm_6026_01>-bwart = '511' ) AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).
    <fs_ztmm_6026_01>-txncode = 'RPIM'.
  ELSEIF <fs_ztmm_6026_01>-bwart = '102'                 AND
         ( ( ( <fs_ztmm_6026_01>-mtart ='ROH' OR   "Production Materials
               <fs_ztmm_6026_01>-mtart ='ROH1'     "Raw / Sub Material
             )                       AND
             ( <fs_ztmm_6026_01>-profl = 'K' OR
               <fs_ztmm_6026_01>-profl = 'V'
             )
           )                               OR
           ( <fs_ztmm_6026_01>-mtart ='HALB'      AND
             <fs_ztmm_6026_01>-profl = 'K'
           )
         ).
    <fs_ztmm_6026_01>-txncode = 'RNPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '702' OR
           <fs_ztmm_6026_01>-bwart = '712' ) AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' ). "LP
    <fs_ztmm_6026_01>-txncode = 'APPC'.
  ELSEIF ( ( <fs_ztmm_6026_01>-bwart = '702' OR
             <fs_ztmm_6026_01>-bwart = '712'
           )                 AND
           ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
             <fs_ztmm_6026_01>-profl = 'M'  "MIP
           )
         )                       OR
         ( ( <fs_ztmm_6026_01>-bwart = '651'  "GD ret.del.  returns
           )                 AND
           ( <fs_ztmm_6026_01>-mtart = 'HALB' "Semifinished products
           )
         ).
    <fs_ztmm_6026_01>-txncode = 'APIM'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '701' OR
           <fs_ztmm_6026_01>-bwart = '711' OR
           <fs_ztmm_6026_01>-bwart = '543'
         )                                      AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V'    "LP
         ).
    <fs_ztmm_6026_01>-txncode = 'ANPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '701' OR
           <fs_ztmm_6026_01>-bwart = '711' ) AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).
    <fs_ztmm_6026_01>-txncode = 'ANIM'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '101'  OR
           <fs_ztmm_6026_01>-bwart = '131'
         ) AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).
    <fs_ztmm_6026_01>-txncode = 'PPIM'.
  ELSEIF (
           (
             <fs_ztmm_6026_01>-bwart = '102' AND "GR for PO   reversal
             <fs_ztmm_6026_01>-mtart ='HALB' AND "Semifinished products
             <fs_ztmm_6026_01>-profl ='M'
           )                           OR
           (
             <fs_ztmm_6026_01>-bwart = '132'     AND "Goods receipt
             (
               <fs_ztmm_6026_01>-mtart ='FERT' OR "Finished products
               <fs_ztmm_6026_01>-mtart ='HALB'    "Semifinished products
             )                      AND
             <fs_ztmm_6026_01>-profl ='M'
           )
         ).
    <fs_ztmm_6026_01>-txncode = 'PNIM'.
************************************************************************
* Begin of IPPC(261) & IPIM(261)
************************************************************************
**/ Begin of Data from PP
*  ELSEIF ( <fs_ztmm_6026_01>-bwart = space ) AND
*         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
*           <fs_ztmm_6026_01>-profl = 'V' ). "LP
*    <fs_ztmm_6026_01>-txncode = 'IPPC'.
*  ELSEIF ( <fs_ztmm_6026_01>-bwart = space ) AND
*         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
*           <fs_ztmm_6026_01>-profl = 'M'  "MIP
*         ).
*    <fs_ztmm_6026_01>-txncode = 'IPIM'.
**/ End of Data from PP
*
**/ Begin of IPPC(261) Engine
*  ELSEIF ( <fs_ztmm_6026_01>-bwart = '261' ) AND
*         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
*           <fs_ztmm_6026_01>-profl = 'V' ). "LP
*    <fs_ztmm_6026_01>-txncode = 'IPPC'.
**/ Engine of IPPC(261) Engine
*
**/ Begin of IPIM(261) Engine
*  ELSEIF ( <fs_ztmm_6026_01>-bwart = '261' ) AND
*         ( <fs_ztmm_6026_01>-mtart = 'HALB'). "Semifinished products
*    <fs_ztmm_6026_01>-txncode = 'IPIM'.
**/ Engine of IPIM(261) Engine
************************************************************************
* End of IPPC(261) & IPIM(261)
************************************************************************

************************************************************************
* Begin of New IPPC(261) & IPIM(261)
************************************************************************

*/ Begin of Data from PP(20040512)
  ELSEIF ( <fs_ztmm_6026_01>-bwart = space ) AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' ). "LP
    <fs_ztmm_6026_01>-txncode = 'IPPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = space ) AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).
    <fs_ztmm_6026_01>-txncode = 'IPIM'.
*/ End of Data from PP(20040512)

  ELSEIF ( <fs_ztmm_6026_01>-bwart = '261' ) AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' ). "LP
    <fs_ztmm_6026_01>-txncode = 'IPPC'.

  ELSEIF ( <fs_ztmm_6026_01>-bwart = '261' ) AND
          ( <fs_ztmm_6026_01>-mtart = 'HALB' ). "Semifinished products
    <fs_ztmm_6026_01>-txncode = 'IPIM'.
************************************************************************
* End of New IPPC(261) & IPIM(261)
************************************************************************


  ELSEIF ( <fs_ztmm_6026_01>-bwart = '901' OR
           <fs_ztmm_6026_01>-bwart = '905' ) AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' ). "LP
    <fs_ztmm_6026_01>-txncode = 'IPPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '901' OR
           <fs_ztmm_6026_01>-bwart = '905' ) AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).
    <fs_ztmm_6026_01>-txncode = 'IPIM'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '262' ) AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' ) AND "LP
         ( <fs_ztmm_6026_01>-mtart = 'ROH' OR
           <fs_ztmm_6026_01>-mtart = 'ROH1' ).
    <fs_ztmm_6026_01>-txncode = 'INPC'.
  ELSEIF (
           <fs_ztmm_6026_01>-bwart = '262'     AND
           (
             <fs_ztmm_6026_01>-mtart ='FERT' OR "Finished products
             <fs_ztmm_6026_01>-mtart ='HALB'    "Semifinished products
           )                      AND
           <fs_ztmm_6026_01>-profl ='M'
         ).
    <fs_ztmm_6026_01>-txncode = 'INIM'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '902' OR
           <fs_ztmm_6026_01>-bwart = '906' ) AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' ) AND "LP
         ( <fs_ztmm_6026_01>-mtart = 'ROH' OR
           <fs_ztmm_6026_01>-mtart = 'ROH1' ).
    <fs_ztmm_6026_01>-txncode = 'INPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '902' OR
           <fs_ztmm_6026_01>-bwart = '906' ) AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).

    <fs_ztmm_6026_01>-txncode = 'INIM'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '201' OR
           <fs_ztmm_6026_01>-bwart = '551' OR
           <fs_ztmm_6026_01>-bwart = '555' OR
           <fs_ztmm_6026_01>-bwart = '903' OR
           <fs_ztmm_6026_01>-bwart = '907' )         AND
         ( ( ( <fs_ztmm_6026_01>-mtart ='ROH' OR   "Production Materials
               <fs_ztmm_6026_01>-mtart ='ROH1'     "Raw / Sub Material
             )                               AND
             ( <fs_ztmm_6026_01>-profl = 'K' OR
               <fs_ztmm_6026_01>-profl = 'V'
             )
           )                                     OR
           ( <fs_ztmm_6026_01>-mtart ='HALB' AND
             <fs_ztmm_6026_01>-profl = 'K'
           )
         ).
    <fs_ztmm_6026_01>-txncode = 'XPPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '201' OR
           <fs_ztmm_6026_01>-bwart = '551' OR
           <fs_ztmm_6026_01>-bwart = '555' ) AND
*           <fs_ztmm_6026_01>-bwart = '991' ) AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-mtart = 'HALB' OR
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).
    <fs_ztmm_6026_01>-txncode = 'XPIM'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '202' OR
           <fs_ztmm_6026_01>-bwart = '552' OR
           <fs_ztmm_6026_01>-bwart = '556' OR
           <fs_ztmm_6026_01>-bwart = '904' OR
           <fs_ztmm_6026_01>-bwart = '908'
         )                                    AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' )    AND "LP
         ( <fs_ztmm_6026_01>-mtart = 'ROH' OR
           <fs_ztmm_6026_01>-mtart = 'ROH1' ) .
    <fs_ztmm_6026_01>-txncode = 'XNPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '202' OR
           <fs_ztmm_6026_01>-bwart = '552' OR
           <fs_ztmm_6026_01>-bwart = '556' OR
           <fs_ztmm_6026_01>-bwart = '992'
         )                                    AND
         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
           <fs_ztmm_6026_01>-mtart = 'HALB' OR
           <fs_ztmm_6026_01>-profl = 'M'  "MIP
         ).
    <fs_ztmm_6026_01>-txncode = 'XNIM'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '122' ) AND
         ( <fs_ztmm_6026_01>-profl = 'K' OR "KD
           <fs_ztmm_6026_01>-profl = 'V' )   AND "LP
         ( <fs_ztmm_6026_01>-mtart = 'ROH'  OR
           <fs_ztmm_6026_01>-mtart = 'ROH1' OR
           <fs_ztmm_6026_01>-mtart = 'HALB' ).
    <fs_ztmm_6026_01>-txncode = 'SPPC'.
  ELSEIF ( <fs_ztmm_6026_01>-bwart = '601'
         )                                    AND
         ( <fs_ztmm_6026_01>-mtart = 'HALB' "Semifinished products
         ).
    <fs_ztmm_6026_01>-txncode = 'SPIM'.
*  ELSEIF ( <fs_ztmm_6026_01>-bwart = '102'
*         )                                    AND
*         ( <fs_ztmm_6026_01>-mtart = 'FERT' OR "Finished products
*           <fs_ztmm_6026_01>-profl = 'M'       "MIP
*         ).
*    <fs_ztmm_6026_01>-txncode = 'RNIM'.

* CPPC & CNPC is already extract.

  ENDIF.
ENDFORM.                    " set_txncode
*&---------------------------------------------------------------------*
*&      Form  get_ppim_by_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ppim_by_color.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      makt~maktx  AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~lifnr       "Vendor
      mseg~mat_kdauf   "Sales order number of valuated sales order stock
      mseg~mat_kdpos   "Sales Order Item of Valuated Sales Order Stock
      mseg~bwart AS bwart   "Movement type
      mseg~menge AS menge  "Quantity
      mseg~meins AS meins  "Base unit of measure
      mara~profl AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      mara~mtart AS mtart  "Material Type
      mara~ntgew AS ntgew  "Net weight
      mara~gewei AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'F'" Movement indicator
           "F: Goods movement for production order
      INNER JOIN mara
        ON mara~matnr = mseg~matnr "AND
*           mara~profl ='M'
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '101'   OR "GR goods receipt
          mseg~bwart = '131'      "Goods receipt
        )                           AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~budat = p_budat
*    GROUP BY mseg~matnr mkpf~budat.
    ORDER BY mseg~matnr mkpf~budat.

********
  LOOP AT lt_ztmm_6026_01 INTO ls_ztmm_6026_01.
    CLEAR: ls_ztmm_6026_01-colorext, ls_ztmm_6026_01-colorint.
    PERFORM get_characteristicvalue
                   USING    ls_ztmm_6026_01-mat_kdauf
                       "Sales Order Number
                            'COLOREXT'   "External Color
                   CHANGING ls_ztmm_6026_01-colorext.
    PERFORM get_characteristicvalue
                   USING    ls_ztmm_6026_01-mat_kdauf
                       "Sales Order Number
                            'COLORINT'   "Internal Color
                   CHANGING ls_ztmm_6026_01-colorint.

*    IF NOT ( ls_ztmm_6026_01-colorext IS INITIAL OR
*              ls_ztmm_6026_01-colorint IS INITIAL ).
    CONCATENATE ls_ztmm_6026_01-matnr
                ls_ztmm_6026_01-colorext
                ls_ztmm_6026_01-colorint
                p_budat
      INTO ls_ztmm_6026_01-ordernumwork.
*    ENDIF.
    MODIFY lt_ztmm_6026_01 FROM ls_ztmm_6026_01.
  ENDLOOP.

********
*/ Grouping by ordernumwork matnr budat
  DATA: ls_ztmm_6026_01_nocolor LIKE ls_ztmm_6026_01.
  DATA: lt_ztmm_6026_01_nocolor
           LIKE TABLE OF ls_ztmm_6026_01_nocolor.

  DATA: ls_ztmm_6026_01_color LIKE ls_ztmm_6026_01.
  DATA: lt_ztmm_6026_01_color
           LIKE TABLE OF ls_ztmm_6026_01_color.

  DATA: ls_color LIKE ls_ztmm_6026_01.
  DATA: lt_color LIKE TABLE OF ls_color.

  DATA: ls_nocolor LIKE ls_ztmm_6026_01.
  DATA: lt_nocolor LIKE TABLE OF ls_nocolor.

  DATA: lv_ordernumwork LIKE ztmm_6026_01-ordernumwork.
  DATA: lv_matnr LIKE ztmm_6026_01-matnr.

  LOOP AT lt_ztmm_6026_01 INTO ls_ztmm_6026_01.
    IF ls_ztmm_6026_01-ordernumwork IS INITIAL.
      APPEND ls_ztmm_6026_01 TO lt_ztmm_6026_01_nocolor.
    ELSE.
      APPEND ls_ztmm_6026_01 TO lt_ztmm_6026_01_color.
    ENDIF.
  ENDLOOP.

*/ Grouping by matnr budat
  DATA: lv_menge LIKE ztmm_6026_01-menge.
  DATA: lv_budat LIKE ztmm_6026_01-budat.

  SORT lt_ztmm_6026_01_nocolor BY matnr budat.
  LOOP AT lt_ztmm_6026_01_nocolor ASSIGNING <fs_ztmm_6026_01>.
    IF lv_matnr IS INITIAL AND lv_budat IS INITIAL.
      lv_matnr = <fs_ztmm_6026_01>-matnr.
      lv_budat = <fs_ztmm_6026_01>-budat.
    ENDIF.

    IF lv_matnr = <fs_ztmm_6026_01>-matnr.
      IF lv_budat = <fs_ztmm_6026_01>-budat.
      ELSE.
        lv_budat = <fs_ztmm_6026_01>-budat.
        MOVE lv_menge TO ls_ztmm_6026_01-menge.
        APPEND ls_ztmm_6026_01 TO lt_nocolor.
        CLEAR: lv_menge.
      ENDIF.
    ELSE.
      lv_matnr = <fs_ztmm_6026_01>-matnr.
      lv_budat = <fs_ztmm_6026_01>-budat.
      MOVE lv_menge TO ls_ztmm_6026_01-menge.
      APPEND ls_ztmm_6026_01 TO lt_nocolor.
      CLEAR: lv_menge.
    ENDIF.
    lv_menge = lv_menge + <fs_ztmm_6026_01>-menge.

    MOVE <fs_ztmm_6026_01> TO ls_ztmm_6026_01.

    AT LAST.
      MOVE lv_menge TO <fs_ztmm_6026_01>-menge.
      APPEND <fs_ztmm_6026_01> TO lt_nocolor.
      CLEAR: lv_menge.
    ENDAT.
  ENDLOOP.

*/ Grouping by ordernumwork budat
  SORT lt_ztmm_6026_01_color BY ordernumwork budat.
  CLEAR: lv_ordernumwork, lv_budat.
  LOOP AT lt_ztmm_6026_01_color ASSIGNING <fs_ztmm_6026_01>.
    IF lv_ordernumwork IS INITIAL AND lv_budat IS INITIAL.
      lv_ordernumwork = <fs_ztmm_6026_01>-ordernumwork.
      lv_budat = <fs_ztmm_6026_01>-budat.
    ENDIF.

    IF lv_ordernumwork = <fs_ztmm_6026_01>-ordernumwork.
      IF lv_budat = <fs_ztmm_6026_01>-budat.
      ELSE.
        lv_budat = <fs_ztmm_6026_01>-budat.
        MOVE lv_menge TO ls_ztmm_6026_01-menge.
        APPEND ls_ztmm_6026_01 TO lt_color.
        CLEAR: lv_menge.
      ENDIF.
    ELSE.
      lv_ordernumwork = <fs_ztmm_6026_01>-ordernumwork.
      lv_budat = <fs_ztmm_6026_01>-budat.
      MOVE lv_menge TO ls_ztmm_6026_01-menge.
      APPEND ls_ztmm_6026_01 TO lt_color.
      CLEAR: lv_menge.
    ENDIF.
    lv_menge = lv_menge + <fs_ztmm_6026_01>-menge.

    MOVE <fs_ztmm_6026_01> TO ls_ztmm_6026_01.

    AT LAST.
      MOVE lv_menge TO <fs_ztmm_6026_01>-menge.
      APPEND <fs_ztmm_6026_01> TO lt_color.
      CLEAR: lv_menge.
    ENDAT.
  ENDLOOP.

*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_nocolor TO it_ztmm_6026_01.
  APPEND LINES OF lt_color   TO it_ztmm_6026_01.
ENDFORM.                    " get_ppim_by_color
*&---------------------------------------------------------------------*
*&      Form  get_characteristicvalue
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6026_01>_MAT_KDAUF  text
*      <--P_<FS_ZTMM_6026_01>_COLOREXT  text
*----------------------------------------------------------------------*
FORM get_characteristicvalue
             USING    value(im_sono) LIKE vbak-vbeln
                      value(im_color)
             CHANGING value(ex_atwrt).  "Characteristic Value
  CLEAR: ex_atwrt.
*/ Get Internal characteristic.
  DATA: lv_atinn(10) TYPE n. "LIKE ausp-atinn "Internal Character
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = im_color
       IMPORTING
            output = lv_atinn. "Internal characteristic

*/
  SELECT SINGLE
      ausp~atwrt  "Characteristic value
    INTO ex_atwrt
    FROM vbak
      LEFT OUTER JOIN ausp
        ON ausp~objek = vbak~bstnk AND
        "Key of object to be classified  (PO Number)
           ausp~atinn = lv_atinn    AND "Internal characteristic
*         ATZHL =   AND "Characteristic value counter
*         MAFID =   AND "Indicator: object/class
           ausp~klart = '001'             "Class type
*         ADZHL = .
    WHERE vbak~vbeln = im_sono.

ENDFORM.                    " get_characteristicvalue
*&---------------------------------------------------------------------*
*&      Form  get_ippc_ipim_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ippc_ipim_fsc.
  DATA: lt_ztmm_6026_01            LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01            LIKE LINE OF lt_ztmm_6026_01.
  FIELD-SYMBOLS: <fs_ztmm_6026_01> LIKE ls_ztmm_6026_01.

*/ For FSC Components
  CALL FUNCTION 'Z_FMM_6029_FTZ_IPPC_IPIM'
       EXPORTING
            im_signoffdate   = p_budat
       TABLES
            ext_ztmm_6026_01 = lt_ztmm_6026_01.

*/ Append to IT_ZTMM_6026_01
  SORT lt_ztmm_6026_01 BY ordernumwork matnr budat.
  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_ippc_ipim_fsc
*&---------------------------------------------------------------------*
*&      Form  get_sppc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_sppc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'B' "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  WHERE
        ( mseg~bwart = '122'
        )                         AND
        ( mara~mtart ='ROH' OR   "Production Materials
          mara~mtart ='HALB' OR  "Semifinished products
          mara~mtart ='ROH1'     "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_sppc
*&---------------------------------------------------------------------*
*&      Form  get_spim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_spim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '932'
        )                          AND
        ( mara~profl ='M' AND
          ( mara~mtart ='ROH'  OR "Production Materials
            mara~mtart ='HALB' OR "Semifinished products
            mara~mtart ='ROH1'    "Raw / Sub Material
          )
        )                           AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_spim
*&---------------------------------------------------------------------*
*&      Form  get_rnpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rnpc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      mseg~lifnr        AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'B' "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu

  WHERE
         mseg~bwart = '102'                 AND
         ( ( ( mara~mtart ='ROH' OR   "Production Materials
              mara~mtart ='ROH1'     "Raw / Sub Material
             )                       AND
             ( mara~profl = 'K' OR
               mara~profl = 'V'
             )
           )                               OR
           ( mara~mtart ='HALB'      AND
             mara~profl = 'K'
           )
         )                                  AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr
           mseg~lifnr
           mkpf~budat
           mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_rnpc
*&---------------------------------------------------------------------*
*&      Form  get_ippc_engine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ippc_engine.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01 LIKE LINE OF lt_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      makt~maktx  AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~lifnr  AS lifnr  "Vendor
      mseg~bwart  AS bwart   "Movement type
      mseg~menge  AS menge  "Quantity
      mseg~meins  AS meins  "Base unit of measure
      mseg~werks  AS werks  "Plant
      mara~profl  AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      mara~mtart  AS mtart  "Material Type
      mara~ntgew  AS ntgew  "Net weight
      mara~gewei  AS gewei "Weight Unit

      afpo~matnr  AS ordernumwork

      mkpf~budat  AS budat"Posting Date
*    APPENDING CORRESPONDING FIELDS OF TABLE it_ztmm_6026_01
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
        ON mara~matnr = mseg~matnr
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu

      INNER JOIN afpo   "Order item
        ON afpo~aufnr = mseg~aufnr AND  "Order number
           afpo~posnr = '0001'   "Order Item number

    WHERE
        ( mseg~bwart = '261'       "GI for order
        )                         AND
        ( mara~mtart ='ROH' OR     "Production Materials
          mara~mtart ='ROH1'       "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
*        ( mseg~werks = 'E001' "Plant
*        )                         AND
        mkpf~budat = p_budat
*  GROUP BY mseg~matnr mkpf~budat.
    ORDER BY mseg~matnr mkpf~budat.

*/Begin of Added by Hakchin (Temparary)(To avoid FSC)
  DATA: lv_mtart LIKE mara-mtart.
  DATA: lt_ztmm_6026_01_tmp LIKE lt_ztmm_6026_01.
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    SELECT SINGLE mtart INTO lv_mtart
      FROM mara
      WHERE matnr = <fs_ztmm_6026_01>-ordernumwork AND
            mtart = 'FERT'.
    IF sy-subrc <> 0.
      APPEND <fs_ztmm_6026_01> TO lt_ztmm_6026_01_tmp.
    ENDIF.
  ENDLOOP.
  lt_ztmm_6026_01 = lt_ztmm_6026_01_tmp.
*/End of Added by Hakchin (Temparary)

*/OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    CONCATENATE <fs_ztmm_6026_01>-ordernumwork p_budat
        INTO <fs_ztmm_6026_01>-ordernumwork.
  ENDLOOP.


*/Begin of Added by Hakchin(20040329)
*Group by OrderNumWork MATNR with qty in lt_engine.
  DATA: lt_engine       LIKE it_ztmm_6026_01.
  lt_engine = lt_ztmm_6026_01.
  PERFORM itable_group_by_onw_matnr
               USING lt_engine.
*/End of Added by Hakchin(20040329)

*/Get QTYPERLM.
  PERFORM get_qtyperlm
             USING lt_ztmm_6026_01
                   lt_engine.

*/Begin of Added by Hakchin(20040512)
  LOOP AT lt_engine ASSIGNING <fs_ztmm_6026_01>.
    PERFORM get_inforecord_data
               USING    <fs_ztmm_6026_01>-matnr "Material
                        <fs_ztmm_6026_01>-lifnr "Vendor code
               CHANGING <fs_ztmm_6026_01>-land1
                        <fs_ztmm_6026_01>-netpr
                        <fs_ztmm_6026_01>-effpr
                        <fs_ztmm_6026_01>-waers
                        <fs_ztmm_6026_01>-bpumz
                        <fs_ztmm_6026_01>-bpumn
                        <fs_ztmm_6026_01>-peinh.

*/Begin of Added by Hakchin(20040407)
*Adjusted Price
    IF NOT <fs_ztmm_6026_01>-bpumn IS INITIAL.
      <fs_ztmm_6026_01>-netpruom =
      ( <fs_ztmm_6026_01>-netpr * <fs_ztmm_6026_01>-bpumz ) /
      <fs_ztmm_6026_01>-bpumn.

      <fs_ztmm_6026_01>-effpruom =
      ( <fs_ztmm_6026_01>-effpr * <fs_ztmm_6026_01>-bpumz ) /
                                   <fs_ztmm_6026_01>-bpumn.

      IF NOT <fs_ztmm_6026_01>-peinh IS INITIAL.
        <fs_ztmm_6026_01>-netpruom = <fs_ztmm_6026_01>-netpruom /
                                     <fs_ztmm_6026_01>-peinh.

        <fs_ztmm_6026_01>-effpruom = <fs_ztmm_6026_01>-effpruom /
                                     <fs_ztmm_6026_01>-peinh.
      ENDIF.




    ENDIF.
*/End of Added by Hakchin(20040407)

*/ Get HS code & HTSNum Source
* First, We get marc-stawn(HS code) from werks = 'P001'.
* Second, if there is no HS code from werks = 'P001', then
* we try to get HS code from other plants.
* Third, at all efforts, if we don't get any HS code, then
* leave marc-stawn as space.
    PERFORM get_hscode
              USING    <fs_ztmm_6026_01>-matnr
              CHANGING <fs_ztmm_6026_01>-stawn. "HS code
  ENDLOOP.
*/End of Added by Hakchin(20040512)

*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_engine TO it_ztmm_6026_01.
ENDFORM.                    " get_ippc_engine
*&---------------------------------------------------------------------*
*&      Form  get_ipim_engine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ipim_engine.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01 LIKE LINE OF lt_ztmm_6026_01.
  FIELD-SYMBOLS: <fs_ztmm_6026_01> LIKE ls_ztmm_6026_01.
  SELECT
      mseg~matnr  AS matnr "Material number
      makt~maktx  AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~lifnr  AS lifnr  "Vendor
      mseg~bwart  AS bwart   "Movement type
      mseg~menge  AS menge  "Quantity
      mseg~meins  AS meins  "Base unit of measure
      mseg~werks  AS werks  "Plant
      mara~profl  AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      mara~mtart  AS mtart  "Material Type
      mara~ntgew  AS ntgew  "Net weight
      mara~gewei  AS gewei "Weight Unit

      afpo~matnr  AS ordernumwork

      mkpf~budat        AS budat "Posting Date
*    APPENDING CORRESPONDING FIELDS OF TABLE it_ztmm_6026_01
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'F'" Movement indicator
           "F: Goods movement for production order
      INNER JOIN mara
        ON mara~matnr = mseg~matnr "AND
*           mara~profl ='M'
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY

      INNER JOIN afpo
        ON afpo~aufnr = mseg~aufnr AND  "Order number
           afpo~posnr = '0001'   "Order Item number

    WHERE
        ( mseg~bwart = '261'       "GI for order
        )                           AND
        ( mara~profl ='M' AND
          ( mara~mtart ='HALB' ) "Semifinished products
        )                           AND
*        ( mseg~werks = 'E001'      "Plant
*        )                           AND
        mkpf~budat = p_budat
*  GROUP BY mseg~matnr mkpf~budat.
    ORDER BY mseg~matnr mkpf~budat.

*/Begin of Added by Hakchin (Temparary)(To avoid FSC)
  DATA: lv_mtart LIKE mara-mtart.
  DATA: lt_ztmm_6026_01_tmp LIKE lt_ztmm_6026_01.
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    SELECT SINGLE mtart INTO lv_mtart
      FROM mara
      WHERE matnr = <fs_ztmm_6026_01>-ordernumwork AND
            mtart = 'FERT'.
    IF sy-subrc <> 0.
      APPEND <fs_ztmm_6026_01> TO lt_ztmm_6026_01_tmp.
    ENDIF.
  ENDLOOP.
  lt_ztmm_6026_01 = lt_ztmm_6026_01_tmp.
*/End of Added by Hakchin (Temparary)

*/OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    CONCATENATE <fs_ztmm_6026_01>-ordernumwork p_budat
        INTO <fs_ztmm_6026_01>-ordernumwork.
  ENDLOOP.

*/Begin of Added by Hakchin(20040329)
*Group by OrderNumWork MATNR with qty in lt_ztmm_6026_01
  DATA: lt_engine       LIKE it_ztmm_6026_01.
  lt_engine = lt_ztmm_6026_01.
  PERFORM itable_group_by_onw_matnr
               USING lt_engine.
*/End of Added by Hakchin(20040329)

*/Get QTYPERLM.
  PERFORM get_qtyperlm
             USING lt_ztmm_6026_01
                   lt_engine.

*/Begin of Added by Hakchin(20040512)
  LOOP AT lt_engine ASSIGNING <fs_ztmm_6026_01>.
    PERFORM get_inforecord_data
               USING    <fs_ztmm_6026_01>-matnr "Material
                        <fs_ztmm_6026_01>-lifnr "Vendor code
               CHANGING <fs_ztmm_6026_01>-land1
                        <fs_ztmm_6026_01>-netpr
                        <fs_ztmm_6026_01>-effpr
                        <fs_ztmm_6026_01>-waers
                        <fs_ztmm_6026_01>-bpumz
                        <fs_ztmm_6026_01>-bpumn
                        <fs_ztmm_6026_01>-peinh.

*/Begin of Added by Hakchin(20040407)
*Adjusted Price
    IF NOT <fs_ztmm_6026_01>-bpumn IS INITIAL.
      <fs_ztmm_6026_01>-netpruom =
      ( <fs_ztmm_6026_01>-netpr * <fs_ztmm_6026_01>-bpumz ) /
      <fs_ztmm_6026_01>-bpumn.

      <fs_ztmm_6026_01>-effpruom =
      ( <fs_ztmm_6026_01>-effpr * <fs_ztmm_6026_01>-bpumz ) /
                                   <fs_ztmm_6026_01>-bpumn.

      IF NOT <fs_ztmm_6026_01>-peinh IS INITIAL.
        <fs_ztmm_6026_01>-netpruom = <fs_ztmm_6026_01>-netpruom /
                                     <fs_ztmm_6026_01>-peinh.

        <fs_ztmm_6026_01>-effpruom = <fs_ztmm_6026_01>-effpruom /
                                     <fs_ztmm_6026_01>-peinh.
      ENDIF.
    ENDIF.
*/End of Added by Hakchin(20040407)

*/ Get HS code & HTSNum Source
* First, We get marc-stawn(HS code) from werks = 'P001'.
* Second, if there is no HS code from werks = 'P001', then
* we try to get HS code from other plants.
* Third, at all efforts, if we don't get any HS code, then
* leave marc-stawn as space.
    PERFORM get_hscode
              USING    <fs_ztmm_6026_01>-matnr
              CHANGING <fs_ztmm_6026_01>-stawn. "HS code
  ENDLOOP.
*/End of Added by Hakchin(20040512)







*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_engine TO it_ztmm_6026_01.

ENDFORM.                    " get_ipim_engine
*&---------------------------------------------------------------------*
*&      Form  process_data_by_section
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data_by_section.
*/ App. Doc. No.
  PERFORM number_get_next USING    c_nro_nr_09
                                   'ZMMNRO0002'
                          CHANGING w_zdocno.
  COMMIT WORK.

*/Define Local Variable
  DATA: lv_lines     TYPE i.  "No. of itab lines
  DATA: lv_quotient  TYPE i.
  DATA: lv_remainder TYPE i.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  "For Sectional Use of it_ztmm_6026_01
  DATA: lc_sectionsize TYPE i VALUE 1000. "Section Size
  DATA: lv_do_idx TYPE i.  "Index for Do Loop
  DATA: lv_frline TYPE i.  "From Line
  DATA: lv_toline TYPE i.  "To Line

*/Get Line no. of it_data_for_to
  DESCRIBE TABLE it_ztmm_6026_01 LINES lv_lines.
  "I estimate lv_lines = 400,000.
  lv_quotient  = lv_lines DIV lc_sectionsize.
  lv_remainder = lv_lines MOD lc_sectionsize.

*/Process Data by Section
  DO lv_quotient TIMES.
    lv_do_idx = lv_do_idx + 1.
    lv_frline  = ( lv_do_idx - 1 ) * lc_sectionsize + 1.
    lv_toline  = lv_do_idx         * lc_sectionsize.
    CLEAR: lt_ztmm_6026_01.
    APPEND LINES OF it_ztmm_6026_01
      FROM lv_frline TO lv_toline TO lt_ztmm_6026_01.
    PERFORM process_data               "Process Data
                USING w_zdocno
                      lt_ztmm_6026_01.

    APPEND LINES OF lt_ztmm_6026_01
                     TO it_ztmm_6026_01_tmp.
  ENDDO.
  IF NOT lv_remainder IS INITIAL.
    lv_frline  = lv_quotient * lc_sectionsize + 1.
    lv_toline  = lv_lines.
    "(=lv_quotient * lc_sectionsize + lv_remainder)
    CLEAR: lt_ztmm_6026_01.
    APPEND LINES OF it_ztmm_6026_01
      FROM lv_frline TO lv_toline TO lt_ztmm_6026_01.
    PERFORM process_data               "Process Data
                USING w_zdocno
                      lt_ztmm_6026_01.
    APPEND LINES OF lt_ztmm_6026_01
                     TO it_ztmm_6026_01_tmp.
  ENDIF.

  it_ztmm_6026_01 = it_ztmm_6026_01_tmp.

ENDFORM.                    " process_data_by_section
*&---------------------------------------------------------------------*
*&      Form  cs_bom_expl_mat_v2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_STPOX  text
*      -->P_6268   text
*      -->P_P_DATUV  text
*      -->P_6270   text
*      -->P_6271   text
*      -->P_SPACE  text
*      -->P_6273   text
*      -->P_P_MATNR  text
*      -->P_P_STLAL  text
*      -->P_P_STLAN  text
*      -->P_P_WERKS  text
*----------------------------------------------------------------------*
FORM cs_bom_expl_mat_v2
         TABLES ext_stpox STRUCTURE  stpox
         USING p_capid LIKE  tc04-capid  "Application ID
               p_datuv LIKE  stko-datuv  "Validity date
               p_ehndl LIKE  csdata-xfeld
               "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
               p_emeng LIKE  stko-bmeng  "Required quantity
               p_mehrs LIKE  csdata-xfeld  "Multi-level explosion
               "If 'X', then all lower level is exploded.
               p_mmory LIKE  csdata-xfeld
               "Memory use ('1'=on;'0'=off;' '=no reaction)
               p_mtnrv LIKE  mara-matnr   "Material
               p_stlal LIKE mast-stlal   "Alternative BOM
               p_stlan LIKE mast-stlan   "BOM usage
               p_werks LIKE  marc-werks.   "Plant
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
       stlal                       = p_stlal   "Alternative BOM
       stlan                       = p_stlan   "BOM usage
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
*&      Form  get_hscode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      <--P_<FS_ZTMM_6019_01>_STAWN  text
*----------------------------------------------------------------------*
FORM get_hscode
        USING    value(im_matnr)
        CHANGING value(ex_stawn) LIKE marc-stawn.
  CLEAR: ex_stawn.
  SELECT SINGLE stawn
    INTO ex_stawn
    FROM marc
    WHERE matnr = im_matnr AND
          werks = 'P001'.
  IF sy-subrc <> 0.
    SELECT SINGLE stawn
      INTO ex_stawn
      FROM marc
      WHERE matnr = im_matnr.
  ENDIF.
  IF ex_stawn IS INITIAL. ex_stawn = '0000.00.0000'. ENDIF.
ENDFORM.                    " get_hscode
*&---------------------------------------------------------------------*
*&      Form  getstatuscode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM getstatuscode.
  DATA: lv_kbetr LIKE konp-kbetr.
  "Rate (condition amount or percentage) where no scale exists
  DATA: lv_konwa LIKE konp-konwa.
  "Rate unit (currency or percentage)

  read table t_impreq with key ebeln = <fs_ztmm_6026_01>-ebeln
                                       binary search
                                       transporting no fields.
  if sy-subrc eq 0.
    <fs_ztmm_6026_01>-statuscode    = ''.
    <fs_ztmm_6026_01>-statuscodesrc = 'I'.
  else.
    if <fs_ztmm_6026_01>-profl eq 'M'.
      <fs_ztmm_6026_01>-statuscode    = 'F'.
      <fs_ztmm_6026_01>-statuscodesrc = ''.
    else.
      <fs_ztmm_6026_01>-statuscode    = 'D'.
      <fs_ztmm_6026_01>-statuscodesrc = ''.
    endif.
  endif.

*  CHECK NOT <fs_ztmm_6026_01>-stawn IS INITIAL.

**/ Get HS code Rate  (You can view the data from /nMEK3)
**1.
*  DATA: lv_knumh LIKE konp-knumh.
*  SELECT SINGLE knumh INTO lv_knumh
*    FROM a902
*    WHERE kappl = 'M'    AND
*          kschl = 'ZOA1' AND
*          stawn = <fs_ztmm_6026_01>-stawn.
*
**2.
*  SELECT SINGLE kbetr konwa
*    INTO (lv_kbetr, lv_konwa)
*    FROM konp
*    WHERE knumh = lv_knumh.
*  lv_kbetr = lv_kbetr / 10.
*
*
**/ Make StatusCode
*  IF <fs_ztmm_6026_01>-profl = 'K'.  "KD Material
*    IF lv_kbetr =< '2.5'.
*      <fs_ztmm_6026_01>-statuscode = 'P'.
*    ELSE.
*      <fs_ztmm_6026_01>-statuscode = 'N'.
*    ENDIF.
*  ELSEIF <fs_ztmm_6026_01>-profl = 'V'.  "Local Part
*    <fs_ztmm_6026_01>-statuscode = 'D'.
*  ELSEIF <fs_ztmm_6026_01>-profl = 'M'.   "MIP Mat
*    <fs_ztmm_6026_01>-statuscode = 'F'.
*  ENDIF.
*
**/ StatusCode Refine
*  IF <fs_ztmm_6026_01>-land1 = 'US' OR  "USA
*     <fs_ztmm_6026_01>-land1 = 'CA' OR  "CANADA
*     <fs_ztmm_6026_01>-land1 = 'MX'.    "MEXICO
*    <fs_ztmm_6026_01>-statuscode = 'D'. "Domestic
*  ENDIF.
ENDFORM.                    " getstatuscode
*&---------------------------------------------------------------------*
*&      Form  get_qtyperlm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_qtyperlm
        USING imt_ztmm_6026_01 LIKE it_ztmm_6026_01
              iet_itable       LIKE it_ztmm_6026_01.

*  BOM Items (Extended for List Displays)
  DATA: lt_stpox        LIKE TABLE OF stpox.
  DATA: ls_stpox        LIKE LINE OF  lt_stpox.
  FIELD-SYMBOLS: <fs_stpox> LIKE LINE OF lt_stpox.

  DATA: lt_ordernumwork_unique LIKE it_ztmm_6026_01.
  DATA: lt_ordernumwork_matnr  LIKE it_ztmm_6026_01.
  DATA: lv_onw_string          TYPE string.
  DATA: lv_onw_char            LIKE ztmm_6026_01-ordernumwork.

  DATA: lv_bom_src             TYPE matnr.


  lt_ordernumwork_unique = imt_ztmm_6026_01.
  SORT lt_ordernumwork_unique BY ordernumwork.
  DELETE ADJACENT DUPLICATES FROM lt_ordernumwork_unique
                             COMPARING ordernumwork.

  LOOP AT lt_ordernumwork_unique ASSIGNING <fs_ztmm_6026_01>.
    lv_onw_string = <fs_ztmm_6026_01>-ordernumwork.
    SHIFT lv_onw_string RIGHT CIRCULAR BY 8 PLACES.
    lv_onw_char = lv_onw_string.
    lv_bom_src = lv_onw_char+8(*).  "Date Part is deleted
* BOM Explosioin
    PERFORM cs_bom_expl_mat_v2   "similar to /nCS12
             TABLES lt_stpox
             USING 'PP01'     "Application ID
                   p_budat    "Validity date
                   '1' "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
                   '1' "Required quantity
                   'X' "'X':Multi-level explosion
                   '1' "Memory use ('1'=on;'0'=off;' '=no reaction)
                   lv_bom_src    "Source Material
                   ' '       "Alternative BOM
                   '1'       "BOM usage (Always 1: Production)
                   <fs_ztmm_6026_01>-werks.   "Plant
    CLEAR: wa_ztmm_6026_01.
    LOOP AT lt_stpox ASSIGNING <fs_stpox>.
      MOVE <fs_ztmm_6026_01>-ordernumwork
                    TO wa_ztmm_6026_01-ordernumwork.
      MOVE <fs_stpox>-idnrk TO wa_ztmm_6026_01-matnr.
      MOVE <fs_stpox>-meins TO wa_ztmm_6026_01-meins.
      MOVE <fs_stpox>-mngko TO wa_ztmm_6026_01-qtyperlm.
      APPEND wa_ztmm_6026_01 TO lt_ordernumwork_matnr.
    ENDLOOP.
  ENDLOOP.

*/
  LOOP AT iet_itable ASSIGNING <fs_ztmm_6026_01>.
    CLEAR: wa_ztmm_6026_01.
    READ TABLE lt_ordernumwork_matnr INTO wa_ztmm_6026_01
           WITH KEY ordernumwork = <fs_ztmm_6026_01>-ordernumwork
                    matnr        = <fs_ztmm_6026_01>-matnr.
    IF sy-subrc = 0.
      <fs_ztmm_6026_01>-qtyperlm = wa_ztmm_6026_01-qtyperlm.
    ENDIF.
  ENDLOOP.

**/ Delete useless data
*  DELETE iet_itable WHERE qtyperlm IS initial.
ENDFORM.                    " get_qtyperlm
*&---------------------------------------------------------------------*
*&      Form  get_rnim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_rnim.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE it_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN marc
        ON marc~matnr = mseg~matnr AND
           marc~fevor = 'SEA' "Production scheduler
      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '102'
        )                           AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat.

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_rnim
*&---------------------------------------------------------------------*
*&      Form  get_ppim_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ppim_new.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mseg~werks ) AS werks  "Plant
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit

      MAX( mseg~aufnr ) AS aufnr "CO Production Order

      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'F'" Movement indicator
           "F: Goods movement for production order
      INNER JOIN mara
        ON mara~matnr = mseg~matnr "AND
*           mara~profl ='M'
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '101'   OR "GR goods receipt
          mseg~bwart = '131'      "Goods receipt
        )                           AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~budat = p_budat
    GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)


*/Make OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    IF <fs_ztmm_6026_01>-bwart = '131'.
      CONCATENATE <fs_ztmm_6026_01>-matnr
                  p_budat
        INTO <fs_ztmm_6026_01>-ordernumwork.
    ENDIF.
  ENDLOOP.
*/Begin of Added by Hakchin(20040521)
* Get Plannned Order Number (For later IPPC, IPIM gathering)
  PERFORM get_planned_order USING p_budat
                                  lt_ztmm_6026_01.
*/End of Added by Hakchin(20040521)

*/Begin of Added by Hakchin(20040501)
* In order to get IPPC_IPIM materials
  APPEND LINES OF lt_ztmm_6026_01 TO it_ppim.
*/End of Added by Hakchin(20040501)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_ppim_new
*&---------------------------------------------------------------------*
*&      Form  itable_group_by_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM itable_group_by_matnr
      USING iet_itable LIKE it_ztmm_6026_01.
  DATA: lv_serno TYPE i.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <lf_summary> LIKE LINE OF iet_itable.
  DATA: lt_summary LIKE TABLE OF <lf_summary>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.

*/1.Make unique line by MATNR
  lt_summary = iet_itable.
  SORT lt_summary BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_summary
                   COMPARING matnr.

*/2.Grouping by MATNR
  SORT lt_summary BY matnr.
  LOOP AT lt_summary ASSIGNING <lf_summary>.
    CLEAR: lv_serno, <lf_summary>-menge.
    LOOP AT iet_itable ASSIGNING <lf_itable>
                  WHERE matnr = <lf_summary>-matnr.
      lv_serno = lv_serno + 1.  "Occurrence by MATNR
      <lf_summary>-menge = <lf_summary>-menge + <lf_itable>-menge.
    ENDLOOP.
  ENDLOOP.
  iet_itable = lt_summary.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  itable_group_by_onw_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM itable_group_by_onw_matnr
      USING iet_itable LIKE it_ztmm_6026_01.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <lf_summary> LIKE LINE OF iet_itable.
  DATA: lt_summary LIKE TABLE OF <lf_summary>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.

*/1.Make unique line by ORDERNUMWORK, MATNR
  lt_summary = iet_itable.
  SORT lt_summary BY ordernumwork matnr.
  DELETE ADJACENT DUPLICATES FROM lt_summary
                   COMPARING ordernumwork matnr.

*/2.Grouping by ORDERNUMWORK, MATNR
  SORT lt_summary BY ordernumwork matnr.
  LOOP AT lt_summary ASSIGNING <lf_summary>.
*    CLEAR: <lf_summary>-matnr_parent_cnt, <lf_summary>-menge.
    CLEAR: <lf_summary>-menge.
    LOOP AT iet_itable ASSIGNING <lf_itable>
           WHERE ordernumwork   = <lf_summary>-ordernumwork AND
                 matnr          = <lf_summary>-matnr.
*     <lf_summary>-matnr_parent_cnt = <lf_summary>-matnr_parent_cnt + 1.
      "Occurrence by ORDERNUMWORK, MATNR
      <lf_summary>-menge = <lf_summary>-menge + <lf_itable>-menge.
    ENDLOOP.
  ENDLOOP.
  iet_itable = lt_summary.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  delete_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM delete_color
     USING iet_itable LIKE it_ztmm_6026_01.
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.
  DATA:          lv_strlen   TYPE i.
  LOOP AT iet_itable ASSIGNING <lf_itable>.
    CLEAR: lv_strlen.
    lv_strlen = strlen( <lf_itable>-matnr ).
    IF ( lv_strlen = 12 OR lv_strlen = 13 ) AND
      <lf_itable>-mtart = 'ROH'             AND
      <lf_itable>-matnr(1) <> 'B'. "Blank Mat
*Get Color
      DATA: lv_rearcharacters_no TYPE i.
      lv_rearcharacters_no = lv_strlen - 10.
      PERFORM get_rearcharacters USING    <lf_itable>-matnr
                                          lv_rearcharacters_no
                                 CHANGING <lf_itable>-color.
*OrderNumReceipt Color add
      IF NOT ( <lf_itable>-txncode = 'IPPC' OR
         <lf_itable>-txncode = 'IPIM' ).
        CONCATENATE <lf_itable>-ordernumreceipt
                    <lf_itable>-color
              INTO <lf_itable>-ordernumreceipt.
      ENDIF.

*Delete Color
      <lf_itable>-matnr = <lf_itable>-matnr(10).
    ENDIF.
  ENDLOOP.
ENDFORM.                    " delete_color
*&---------------------------------------------------------------------*
*&      Form  get_rearcharacters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LF_ITABLE>_MATNR  text
*      -->P_LV_REARCHARACTERS_NO  text
*      <--P_REARCHARACTERS  text
*----------------------------------------------------------------------*
FORM get_rearcharacters USING    value(im_f)
                                 value(im_rearcharacters_no)
                        CHANGING value(ex_rearcharacters).
  CLEAR: ex_rearcharacters.
  DATA l_offset TYPE i.
  l_offset = strlen( im_f ) - im_rearcharacters_no.
  MOVE im_f+l_offset(im_rearcharacters_no) TO ex_rearcharacters.
ENDFORM.                    "get_rearcharacters
*&---------------------------------------------------------------------*
*&      Form  ordernumreceipt_by_profl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ordernumreceipt_by_profl.
  IF <fs_ztmm_6026_01>-profl = 'K'.   "KD
    CONCATENATE 'GR' 'KD' <fs_ztmm_6026_01>-budat
       INTO <fs_ztmm_6026_01>-ordernumreceipt.
  ELSEIF <fs_ztmm_6026_01>-profl = 'V'.   "LP
    CONCATENATE 'GR' 'LP' <fs_ztmm_6026_01>-budat
       INTO <fs_ztmm_6026_01>-ordernumreceipt.
  ELSEIF  <fs_ztmm_6026_01>-profl = 'M'. "MIP
    CONCATENATE 'GR' 'MIP' <fs_ztmm_6026_01>-budat
       INTO <fs_ztmm_6026_01>-ordernumreceipt.
  ENDIF.
ENDFORM.                    " ordernumreceipt_by_profl
*&---------------------------------------------------------------------*
*&      Form  get_inforecord_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6026_01>_MATNR  text
*      -->P_<FS_ZTMM_6026_01>_LIFNR  text
*      <--P_<FS_ZTMM_6026_01>_NETPR  text
*      <--P_<FS_ZTMM_6026_01>_EFFPR  text
*      <--P_<FS_ZTMM_6026_01>_WAERS  text
*      <--P_<FS_ZTMM_6026_01>_BPUMZ  text
*      <--P_<FS_ZTMM_6026_01>_BPUMN  text
*----------------------------------------------------------------------*
FORM get_inforecord_data
                     USING    value(im_matnr)
                              value(im_lifnr)
                     CHANGING value(ex_land1)
                              value(ex_netpr)
                              value(ex_effpr)
                              value(ex_waers)
                              value(ex_bpumz)
                              value(ex_bpumn)
                              value(ex_peinh).
  CLEAR: ex_land1, ex_netpr, ex_effpr, ex_waers,
         ex_bpumz, ex_bpumn, ex_peinh.

  data: w_datlb like eine-datlb,
        w_lifnr like a018-lifnr,
        w_knumh like a018-knumh,
        w_kbetr like konp-kbetr.

  IF im_lifnr IS INITIAL.
    "This case happens when Vendor is not related to Material Document.
    SELECT SINGLE  lfa1~land1
                   eine~datlb
                   eine~effpr
                   eine~waers
                   eine~bpumz
                   eine~bpumn
                   eine~peinh
                   eina~lifnr
      INTO (ex_land1, w_datlb,
            ex_effpr, ex_waers,
            ex_bpumz, ex_bpumn,
            ex_peinh, w_lifnr)
      FROM eina
        INNER JOIN lfa1
        ON lfa1~lifnr = eina~lifnr "Vendor
        INNER JOIN eine
        ON eine~infnr = eina~infnr   "Info Record Number
    WHERE eina~matnr = im_matnr. " AND
    " eina~lifnr = im_lifnr.
  ELSE.
    w_lifnr = im_lifnr.
    SELECT SINGLE  lfa1~land1
                   eine~datlb
                   eine~effpr
                   eine~waers
                   eine~bpumz
                   eine~bpumn
                   eine~peinh
      INTO (ex_land1, w_datlb,
            ex_effpr, ex_waers,
            ex_bpumz, ex_bpumn,
            ex_peinh)
      FROM eina
        INNER JOIN lfa1
        ON lfa1~lifnr = eina~lifnr "Vendor
        INNER JOIN eine
        ON eine~infnr = eina~infnr   "Info Record Number
    WHERE eina~matnr = im_matnr AND
          eina~lifnr = im_lifnr.
  ENDIF.
  select single knumh from A018
                      into w_knumh
                      where kappl = 'M'
                      and   kschl = 'PB00'
                      and   matnr = im_matnr
                      and   lifnr = w_lifnr
                      and   datab <= w_datlb
                      and   datbi >= w_datlb.
  if sy-subrc ne 0.
    ex_netpr = 0.
    exit.
  endif.
  select single kbetr into w_kbetr
                      from konp
                      where kappl = 'M'
                      and   kschl = 'PB00'
                      and   knumh = w_knumh.

  if sy-subrc eq 0.
    ex_netpr = w_kbetr.
  else.
    ex_netpr = 0.
  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_spim_pp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_spim_pp.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

*/1. Get Equiptment Number by some special date
  DATA: lt_ausp LIKE TABLE OF ausp.
  FIELD-SYMBOLS: <ls_ausp> LIKE LINE OF lt_ausp.
  DATA: lv_budat(8).
  lv_budat = p_budat.
  PERFORM get_equipments_by_date
                     USING lv_budat
                           lt_ausp.
  SORT lt_ausp BY objek.

*/2. Get Characteristic Value
* For bapi_objcl_getclasses.
  DATA:
    lt_bapiret2        LIKE TABLE OF bapiret2,
    lt_allocvaluesnum  LIKE TABLE OF bapi1003_alloc_values_num,
    lt_allocvalueschar LIKE TABLE OF bapi1003_alloc_values_char,
    lt_allocvaluescurr LIKE TABLE OF bapi1003_alloc_values_curr,
    ls_allocvalueschar LIKE LINE OF lt_allocvalueschar,

    lv_objectkey       LIKE bapi1003_key-object,
    lv_objecttable     LIKE bapi1003_key-objecttable
                       VALUE 'EQUI',
    lv_classnum        LIKE bapi1003_key-classnum
                       VALUE 'P_VEHICLE_MASTER',
    lv_classtype       LIKE bapi1003_key-classtype
                       VALUE '002'.

  LOOP AT lt_ausp ASSIGNING <ls_ausp>.
    lv_objectkey = <ls_ausp>-objek.
    PERFORM bapi_objcl_getdetail
                 TABLES lt_allocvaluesnum
                        lt_allocvalueschar  "Characteristic Data
                        lt_allocvaluescurr
                        lt_bapiret2
                 USING  lv_objectkey   "Equipment number
                        lv_objecttable "Table
                        lv_classnum    "Class number
                        lv_classtype.  "Class type

    DATA: lv_fsccode(30).
    DATA: lv_land1 LIKE ztmm_6026_01-land1.
    PERFORM get_fsccode
               TABLES   lt_allocvalueschar
               CHANGING lv_fsccode
                        lv_land1.

    ls_ztmm_6026_01-matnr = lv_fsccode.
    ls_ztmm_6026_01-land1 = lv_land1.
    APPEND ls_ztmm_6026_01 TO lt_ztmm_6026_01.
  ENDLOOP.

*/
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    CLEAR: ls_ztmm_6026_01.
    SELECT SINGLE
         mara~mtart  "Material Type
         mara~ntgew  "Net weight
         mara~gewei  "Weight Unit
         mara~profl  "MIP/LP/KD
         makt~maktx  "Description
    INTO CORRESPONDING FIELDS OF ls_ztmm_6026_01
    FROM mara
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
    WHERE mara~matnr = <fs_ztmm_6026_01>-matnr.
    IF sy-subrc = 0.
      <fs_ztmm_6026_01>-txncode = 'SPIM'.
      <fs_ztmm_6026_01>-mtart   = ls_ztmm_6026_01-mtart.
      <fs_ztmm_6026_01>-menge   = 1.
      <fs_ztmm_6026_01>-meins   = 'EA'.
      <fs_ztmm_6026_01>-ntgew   = ls_ztmm_6026_01-ntgew.
      <fs_ztmm_6026_01>-gewei   = ls_ztmm_6026_01-gewei.
      <fs_ztmm_6026_01>-profl   = ls_ztmm_6026_01-profl.
      <fs_ztmm_6026_01>-maktx   = ls_ztmm_6026_01-maktx.
      <fs_ztmm_6026_01>-budat   = p_budat.

*/ Date & Time.
      <fs_ztmm_6026_01>-udate = w_date.
      <fs_ztmm_6026_01>-utime = w_time.

      CONCATENATE <fs_ztmm_6026_01>-udate
                  'T'
                  <fs_ztmm_6026_01>-utime
                  INTO <fs_ztmm_6026_01>-date_time.

*/ Get HS code & HTSNum Source
* First, We get marc-stawn(HS code) from werks = 'P001'.
* Second, if there is no HS code from werks = 'P001', then
* we try to get HS code from other plants.
* Third, at all efforts, if we don't get any HS code, then
* leave marc-stawn as space.
      PERFORM get_hscode
                USING    <fs_ztmm_6026_01>-matnr
                CHANGING <fs_ztmm_6026_01>-stawn. "HS code
    ENDIF.
  ENDLOOP.

*/ Delete useless data.
  IF NOT lt_ztmm_6026_01 IS INITIAL.
    DELETE lt_ztmm_6026_01 WHERE date_time = space.
  ENDIF.

**Group by matnr with menge(qty) in lt_ztmm_6026_01
*  PERFORM itable_group_by_matnr
*               USING lt_ztmm_6026_01.

*Group by matnr land1 with menge(qty) in lt_ztmm_6026_01
  PERFORM itable_group_by_matnr_land1
               USING lt_ztmm_6026_01.

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_spim_pp
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getdetail
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
  CLEAR: ext_allocvaluesnum,    ext_allocvaluesnum[],
         ext_allocvalueschar,   ext_allocvalueschar[],
         ext_allocvaluescurr,   ext_allocvaluescurr[],
         ext_bapiret2,          ext_bapiret2[].

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
*&      Form  get_equipments_by_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_equipments_by_date
                  USING value(im_date) TYPE char08  "Important
                        ext_ausp       TYPE STANDARD TABLE.
  CLEAR: ext_ausp.
* Get Internal characteristic.
  DATA: lv_atinn25(10) TYPE n.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_RP25_SHOP_DATE'
       IMPORTING
            output = lv_atinn25. "Internal characteristic

  DATA: lv_atinn27(10) TYPE n.
  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            input  = 'P_RP27_SHOP_DATE'
       IMPORTING
            output = lv_atinn27. "Internal characteristic

  SELECT objek INTO CORRESPONDING FIELDS OF TABLE ext_ausp
    FROM ausp
    WHERE
          ( atinn = lv_atinn25 OR
            atinn = lv_atinn27 ) AND
          "Conversion Rule Needed! ('0000001423')
          klart = '002'          AND  "Class type
          atflv = im_date.
ENDFORM.                    " get_equipments_by_date
*&---------------------------------------------------------------------*
*&      Form  get_fsccode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ALLOCVALUESCHAR  text
*      <--P_LV_FSCCODE  text
*----------------------------------------------------------------------*
FORM get_fsccode TABLES   imt_allocvalueschar
                             STRUCTURE bapi1003_alloc_values_char
                 CHANGING value(ex_fsccode)
                          value(ex_land1).
  CLEAR: ex_fsccode, ex_land1.

* For FSC Code
  DATA: lv_p_model_year(10).
  DATA: lv_p_destination_code(10).
  DATA: lv_p_mi(10).
  DATA: lv_p_ocn(10).

*/Begin of Get FSC CODE
*Get Model Year
  READ TABLE imt_allocvalueschar
           WITH KEY charact = 'P_MODEL_YEAR'.  "MODEL YEAR
  IF sy-subrc = 0.
    lv_p_model_year = imt_allocvalueschar-value_char.
  ENDIF.
*Get Destination Code
  READ TABLE imt_allocvalueschar
           WITH KEY charact = 'P_DESTINATION_CODE'.  "Destination Code
  IF sy-subrc = 0.
    lv_p_destination_code = imt_allocvalueschar-value_char.
  ENDIF.
*Get Model Index
  READ TABLE imt_allocvalueschar
           WITH KEY charact = 'P_MI'.  "Model Index
  IF sy-subrc = 0.
    lv_p_mi = imt_allocvalueschar-value_char.
  ENDIF.
*Get O.C.N
  READ TABLE imt_allocvalueschar
           WITH KEY charact = 'P_OCN'.  "O.C.N
  IF sy-subrc = 0.
    lv_p_ocn = imt_allocvalueschar-value_char.
  ENDIF.
*Get FSC Code
  CONCATENATE
    lv_p_model_year
    lv_p_destination_code
    lv_p_mi
  INTO ex_fsccode.

  CONCATENATE
    ex_fsccode
    lv_p_ocn
  INTO ex_fsccode SEPARATED BY space.

*/End of Get FSC CODE
*/Get
  IF lv_p_destination_code(3) = 'B28'.
    ex_land1 = 'US'.
  ELSEIF lv_p_destination_code(3) = 'B06'.
    ex_land1 = 'CA'.
  ELSEIF lv_p_destination_code(3) = 'B20'.
    ex_land1 = 'MX'.
  ENDIF.

ENDFORM.                    " get_fsccode
*&---------------------------------------------------------------------*
*&      Form  get_spim_601_651
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_spim_601.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        AS ebeln
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat "Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
*      INNER JOIN marc
*        ON marc~matnr = mseg~matnr AND
*           marc~fevor = 'SEA' "Production scheduler
*      "SEA: HMMA ENGINE PLANT ASSY
    WHERE
        ( mseg~bwart = '601'     "GD goods issue:delvy
        )                          AND
        ( mara~mtart ='HALB'     "Semifinished products
        )                          AND
        mkpf~budat = p_budat
    GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_spim_601_651
*&---------------------------------------------------------------------*
*&      Form  get_standardprice
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6026_01>_MATNR  text
*      -->P_SPACE  text
*      <--P_<FS_ZTMM_6026_01>_NETPR  text
*      <--P_<FS_ZTMM_6026_01>_WAERS  text
*----------------------------------------------------------------------*
FORM get_standardprice
          USING    value(im_matnr) LIKE mbew-matnr  "Material
                   value(im_bwkey) LIKE mbew-bwkey  "Valuation area
          CHANGING value(ex_stprs) LIKE mbew-stprs  "Standard Price
                   value(ex_waers) LIKE t001-waers. "Currency Key
  CLEAR: ex_stprs, ex_waers.
  IF im_bwkey IS INITIAL.
    SELECT SINGLE werks
      INTO im_bwkey
      FROM marc
      WHERE matnr = im_matnr.
  ENDIF.

  SELECT SINGLE stprs  "Standard Price
    INTO ex_stprs
    FROM mbew    "Material Valuation
    WHERE matnr = im_matnr AND  "Material
          bwkey = im_bwkey.     "Valuation area

  SELECT SINGLE waers  "Currency Key
    INTO ex_waers
    FROM t001    "Company Codes
    WHERE bukrs = 'H201'.    "Company Code
ENDFORM.                    "get_standardprice
*&---------------------------------------------------------------------*
*&      Form  get_data_adjusting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_adjusting.
*/Story
*1. Posting Date has to be From_To. (Already done)
*2. Get Itab with budat, udate, utime
*3. Get Skipped Material Doc using budat, udate, utime
*4. Based on Given Material Doc, Get Summarized Adjusting Data

*1. Posting Date has to be From_To. (Already done)

*2. Get Itab with budat, udate, utime
  DATA: lv_budat LIKE mkpf-budat.
  lv_budat = s_budat-low.

  DO.
    CLEAR: wa_budat_udate_utime.
    SELECT MAX( budat ) AS budat
           MAX( udate ) AS udate
           MAX( utime ) AS utime
      INTO wa_budat_udate_utime
      FROM ztmm_6026_01
      WHERE budat = lv_budat.

    IF NOT wa_budat_udate_utime IS INITIAL.
      APPEND wa_budat_udate_utime TO it_budat_udate_utime.
    ENDIF.

    lv_budat = lv_budat + 1.
    IF lv_budat => s_budat-high. EXIT. ENDIF.
  ENDDO.

  CHECK NOT it_budat_udate_utime IS INITIAL.
*/For RPPC
  PERFORM rppc_adjusting
                USING it_budat_udate_utime.
*/For PPIM
  PERFORM ppim_adjusting
                USING it_budat_udate_utime.
ENDFORM.                    " get_data_adjusting
*&---------------------------------------------------------------------*
*&      Form  refine_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refine_data.
  data  w_nlin type i.
  describe table it_ztmm_6026_01 lines w_nlin.
  if w_nlin ne 0.
    select zfreqno ebeln from ztreqhd
                         into table t_impreq
                         for all entries in it_ztmm_6026_01
                         where ebeln = it_ztmm_6026_01-ebeln.
  endif.

  sort: t_impreq by ebeln.

  LOOP AT it_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.

*/PartnerID
    <fs_ztmm_6026_01>-partnerid = '100300'.

*/ TxnCode
    PERFORM set_txncode.

*/TxnDate&Time
    <fs_ztmm_6026_01>-udate = w_date.
    <fs_ztmm_6026_01>-utime = w_time.
    CONCATENATE <fs_ztmm_6026_01>-udate
                'T'
                <fs_ztmm_6026_01>-utime
                INTO <fs_ztmm_6026_01>-date_time.
    <fs_ztmm_6026_01>-effdate = <fs_ztmm_6026_01>-date_time.
    CONCATENATE
                p_budat
                'T'
                '000000'
                INTO <fs_ztmm_6026_01>-txndate.

*/OrderNumReceipt (GRKDYYYYMMDD, GRLPYYYYMMDD, GRMIPYYYYMMDD)
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'INPC' OR 'INIM'.
        IF <fs_ztmm_6026_01>-bwart = '262'.
          "Already filled!
        ELSE.
          PERFORM ordernumreceipt_by_profl.
        ENDIF.
      WHEN 'RNPC'.
        IF <fs_ztmm_6026_01>-profl = 'K'.   "KD
          CONCATENATE 'GI' 'KD' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumreceipt.
        ELSEIF <fs_ztmm_6026_01>-profl = 'V'.   "LP
          CONCATENATE 'GI' 'LP' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumreceipt.
        ELSEIF  <fs_ztmm_6026_01>-profl = 'M'. "MIP
          CONCATENATE 'GI' 'MIP' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumreceipt.
        ENDIF.
      WHEN 'IPPC' OR 'IPIM'.
        MOVE space TO <fs_ztmm_6026_01>-ordernumreceipt.
      WHEN OTHERS.
        PERFORM ordernumreceipt_by_profl.
    ENDCASE.

*/OrderNumWork (GRKDYYYYMMDD, GRLPYYYYMMDD, GRMIPYYYYMMDD)
*              (GIKDYYYYMMDD, GILPYYYYMMDD, GIMIPYYYYMMDD)
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'PPIM' OR 'PNIM'.
        IF <fs_ztmm_6026_01>-ordernumwork IS INITIAL.
          CONCATENATE 'GR' 'COIL' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumwork.
        ENDIF.
      WHEN 'IPPC' OR 'IPIM' OR 'RNIM'.
        IF <fs_ztmm_6026_01>-ordernumwork IS INITIAL.
          IF <fs_ztmm_6026_01>-profl = 'K'.   "KD
            CONCATENATE 'GI' 'KD' <fs_ztmm_6026_01>-budat
               INTO <fs_ztmm_6026_01>-ordernumwork.
          ELSEIF <fs_ztmm_6026_01>-profl = 'V'.   "LP
            CONCATENATE 'GI' 'LP' <fs_ztmm_6026_01>-budat
               INTO <fs_ztmm_6026_01>-ordernumwork.
          ELSEIF  <fs_ztmm_6026_01>-profl = 'M'. "MIP
            CONCATENATE 'GI' 'MIP' <fs_ztmm_6026_01>-budat
               INTO <fs_ztmm_6026_01>-ordernumwork.
          ENDIF.
        ENDIF.
      WHEN 'INPC' OR 'INIM'.
        IF <fs_ztmm_6026_01>-ordernumwork IS INITIAL.
          IF <fs_ztmm_6026_01>-profl = 'K'.   "KD
            CONCATENATE 'GR' 'KD' <fs_ztmm_6026_01>-budat
               INTO <fs_ztmm_6026_01>-ordernumwork.
          ELSEIF <fs_ztmm_6026_01>-profl = 'V'.   "LP
            CONCATENATE 'GR' 'LP' <fs_ztmm_6026_01>-budat
               INTO <fs_ztmm_6026_01>-ordernumwork.
          ELSEIF  <fs_ztmm_6026_01>-profl = 'M'. "MIP
            CONCATENATE 'GR' 'MIP' <fs_ztmm_6026_01>-budat
               INTO <fs_ztmm_6026_01>-ordernumwork.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

*/OrderNumShip (GIKDYYYYMMDD, GILPYYYYMMDD, GIMIPYYYYMMDD)
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'SPPC'.
        IF <fs_ztmm_6026_01>-profl = 'K'.   "KD
          CONCATENATE 'GI' 'KD' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumship.
        ELSEIF <fs_ztmm_6026_01>-profl = 'V'.   "LP
          CONCATENATE 'GI' 'LP' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumship.
        ELSEIF  <fs_ztmm_6026_01>-profl = 'M'. "MIP
          CONCATENATE 'GI' 'MIP' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumship.
        ENDIF.
      WHEN 'SPIM'.
        CONCATENATE 'SHIPCAR' <fs_ztmm_6026_01>-budat
           INTO <fs_ztmm_6026_01>-ordernumship.
        IF <fs_ztmm_6026_01>-bwart = '601'.
          CONCATENATE 'SHIPAS' <fs_ztmm_6026_01>-budat
             INTO <fs_ztmm_6026_01>-ordernumship.
        ENDIF.
      WHEN 'ANPC'.
        CONCATENATE 'ANPC' <fs_ztmm_6026_01>-budat
           INTO <fs_ztmm_6026_01>-ordernumship.

      WHEN OTHERS.
    ENDCASE.

*/Product Type Code
    IF <fs_ztmm_6026_01>-mtart = 'FERT'.  "Finished products
      <fs_ztmm_6026_01>-ptc = 'IM'.   "Inventory mat.
    ENDIF.

    IF <fs_ztmm_6026_01>-profl = 'K' OR    "KD
       <fs_ztmm_6026_01>-profl = 'V'.      "LP
      <fs_ztmm_6026_01>-ptc = 'PC'.  "Purchased mat.
    ELSEIF <fs_ztmm_6026_01>-profl = 'M'.    "MIP
      <fs_ztmm_6026_01>-ptc = 'IM'.   "Inventory mat.
    ENDIF.

*/NaftaCertifiedSource
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM' OR
           'SPPC' OR 'SPIM'.
        CLEAR: <fs_ztmm_6026_01>-naftacertified.
        <fs_ztmm_6026_01>-naftacertifiedsc = 'I'.
      WHEN OTHERS.
    ENDCASE.

*/QTYPERLM
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'INPC' OR 'INIM'.
        <fs_ztmm_6026_01>-qtyperlm = '1'.
      WHEN 'IPPC'.
        IF <fs_ztmm_6026_01>-qtyperlm IS INITIAL.
          <fs_ztmm_6026_01>-qtyperlm = '1'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

*/CountryShipTo
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'SPPC' OR 'SPIM' OR 'ANPC'.
        <fs_ztmm_6026_01>-countryshipto = 'US'.
      WHEN OTHERS.
    ENDCASE.

*/TransportID  --> Not Sure
    <fs_ztmm_6026_01>-transportid = <fs_ztmm_6026_01>-ordernumreceipt.

*/AdjReceiptDocID
*    IF <fs_ztmm_6026_01>-txncode = 'RNPC'.
*      IF <fs_ztmm_6026_01>-profl = 'K'.   "KD
*        CONCATENATE 'GI' 'KD' <fs_ztmm_6026_01>-budat
*           INTO <fs_ztmm_6026_01>-adjreceiptdocid.
*      ELSEIF <fs_ztmm_6026_01>-profl = 'V'.   "LP
*        CONCATENATE 'GI' 'LP' <fs_ztmm_6026_01>-budat
*           INTO <fs_ztmm_6026_01>-adjreceiptdocid.
*      ENDIF.
*    ENDIF.

*/ModeOfTransport  --> Not Sure
    <fs_ztmm_6026_01>-modeoftransport = 'L'.

*/Constants
    <fs_ztmm_6026_01>-validflag        = 'N'.
    <fs_ztmm_6026_01>-assignmentflag   = 'N'.
    <fs_ztmm_6026_01>-fifoflag         = 'N'.
    <fs_ztmm_6026_01>-deletedflag      = 'N'.
    <fs_ztmm_6026_01>-keepduringrollba = 'N'.

**/ From here, HTS Specific.
*/Begin of Added by Hakchin(20040504)
*    IF NOT ( <fs_ztmm_6026_01>-txncode = 'IPPC' OR
*             <fs_ztmm_6026_01>-txncode = 'IPIM' ).
*/End of Added by Hakchin(20040504)
    IF ( <fs_ztmm_6026_01>-txncode = 'RPPC' OR
         <fs_ztmm_6026_01>-txncode = 'APPC' ).
*Get Net Price, Effective Price and Currency from Info Record
      PERFORM get_inforecord_data
                 USING    <fs_ztmm_6026_01>-matnr "Material
                          <fs_ztmm_6026_01>-lifnr "Vendor code
                 CHANGING <fs_ztmm_6026_01>-land1
                          <fs_ztmm_6026_01>-netpr
                          <fs_ztmm_6026_01>-effpr
                          <fs_ztmm_6026_01>-waers
                          <fs_ztmm_6026_01>-bpumz
                          <fs_ztmm_6026_01>-bpumn
                          <fs_ztmm_6026_01>-peinh.

*      PERFORM get_netpr_effpr_waers
*                   USING    <fs_ztmm_6026_01>-matnr
*                   CHANGING <fs_ztmm_6026_01>-lifnr "Vendor code
*                            <fs_ztmm_6026_01>-netpr
*                            <fs_ztmm_6026_01>-effpr "Effective price
*                            <fs_ztmm_6026_01>-waers
*                            <fs_ztmm_6026_01>-bpumz
*                            <fs_ztmm_6026_01>-bpumn
*                            <fs_ztmm_6026_01>-peinh.
*/Begin of Added by Hakchin(20040407)
*Adjusted Price
      IF NOT <fs_ztmm_6026_01>-bpumn IS INITIAL.
        <fs_ztmm_6026_01>-netpruom =
        ( <fs_ztmm_6026_01>-netpr * <fs_ztmm_6026_01>-bpumz ) /
        <fs_ztmm_6026_01>-bpumn.

        <fs_ztmm_6026_01>-effpruom =
        ( <fs_ztmm_6026_01>-effpr * <fs_ztmm_6026_01>-bpumz ) /
                                     <fs_ztmm_6026_01>-bpumn.
        IF NOT <fs_ztmm_6026_01>-peinh IS INITIAL.
          <fs_ztmm_6026_01>-netpruom = <fs_ztmm_6026_01>-netpruom /
                                       <fs_ztmm_6026_01>-peinh.

          <fs_ztmm_6026_01>-effpruom = <fs_ztmm_6026_01>-effpruom /
                                       <fs_ztmm_6026_01>-peinh.
        ENDIF.
      ENDIF.
*/End of Added by Hakchin(20040407)
    ENDIF.

*/Effective Price Clear
    IF <fs_ztmm_6026_01>-profl = 'M'.
      CLEAR: <fs_ztmm_6026_01>-effpr.
    ENDIF.
*/Begin of Added by Hakchin(20040422)
* When MTART = FERT OR HALB, There is no Net Price.
* So we get standard price instead of net price.
    IF <fs_ztmm_6026_01>-mtart = 'FERT' OR
      ( <fs_ztmm_6026_01>-mtart = 'HALB'
      ).

      PERFORM get_standardprice
                USING    <fs_ztmm_6026_01>-matnr
                         space
                CHANGING <fs_ztmm_6026_01>-netpr
                         <fs_ztmm_6026_01>-waers.

      MOVE: <fs_ztmm_6026_01>-netpr TO <fs_ztmm_6026_01>-netpruom.
    ENDIF.
*/End of Added by Hakchin(20040422)

*/Price Clear
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM' OR 'SPIM'.
      WHEN OTHERS.
        CLEAR: <fs_ztmm_6026_01>-netpr,
               <fs_ztmm_6026_01>-effpr.
    ENDCASE.

*/ Get HS code & HTSNum Source
* First, We get marc-stawn(HS code) from werks = 'P001'.
* Second, if there is no HS code from werks = 'P001', then
* we try to get HS code from other plants.
* Third, at all efforts, if we don't get any HS code, then
* leave marc-stawn as space.
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM' OR
*           'RNPC' OR
           'SPPC' OR 'SPIM' OR 'ANPC' OR
           'ANIM'.
        CLEAR: <fs_ztmm_6026_01>-stawn.
        <fs_ztmm_6026_01>-stawnsrc = 'H'.
      WHEN OTHERS.
        IF <fs_ztmm_6026_01>-stawn IS INITIAL.
          "In IPPC_IPIM_FSC, We already got STAWN.
          PERFORM get_hscode
                    USING    <fs_ztmm_6026_01>-matnr
                    CHANGING <fs_ztmm_6026_01>-stawn. "HS code
        ENDIF.
    ENDCASE.

*/ Country Code
    IF <fs_ztmm_6026_01>-profl = 'V' OR   "LP MAT
        <fs_ztmm_6026_01>-profl = 'K'.    "KD MAT

      PERFORM get_vendorinfo
                      USING    <fs_ztmm_6026_01>-lifnr
                      CHANGING <fs_ztmm_6026_01>-land1.  "Country Key
    ELSEIF <fs_ztmm_6026_01>-profl = 'M' OR   "MIP MAT
           <fs_ztmm_6026_01>-mtart = 'FERT'. "Finished products
      <fs_ztmm_6026_01>-land1 = 'US'.
    ENDIF.

*/StatusCode
    PERFORM getstatuscode.


*/SpiCode1 & SpiCode1 Source
    CASE <fs_ztmm_6026_01>-txncode.
*      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM'.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM' OR
*           'RNPC' OR
           'SPPC' OR 'SPIM' OR 'ANPC' OR 'ANIM'.
        CLEAR: <fs_ztmm_6026_01>-spicode1.
        <fs_ztmm_6026_01>-spicode1src = 'H'.
      WHEN OTHERS.
    ENDCASE.

*/SpiCode2 & SpiCode2 Source
    CASE <fs_ztmm_6026_01>-txncode.
*      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM'.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM' OR
*           'RNPC' OR
           'SPPC' OR 'SPIM' OR 'ANPC' OR 'ANIM'.
        CLEAR: <fs_ztmm_6026_01>-spicode2.
        <fs_ztmm_6026_01>-spicode2src = 'H'.
      WHEN OTHERS.
    ENDCASE.

**/LIFNRSRC
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM'.
*        CLEAR <fs_ztmm_6026_01>-lifnr.
        <fs_ztmm_6026_01>-lifnrsrc = 'I'.
    ENDCASE.

*/Relationship Flag & Relationship Flag Source
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'RPPC' OR 'RNPC' OR 'RPIM' OR 'APPC' OR 'APIM' OR
           'SPIM' OR 'ANIM'.
        CLEAR: <fs_ztmm_6026_01>-relflag.
        <fs_ztmm_6026_01>-relflagsrc = 'M'.
      WHEN OTHERS.

    ENDCASE.

*/HTSIndex Source
    <fs_ztmm_6026_01>-htsindexsrc = 'I'.

*/HTSDesc Source
    <fs_ztmm_6026_01>-htsdescsrc = 'H'.

*/HTSNum2Source
    <fs_ztmm_6026_01>-htsnum2src = 'I'.

*/Value2 Source
    <fs_ztmm_6026_01>-value2src = 'I'.

*/AltValueSource
    <fs_ztmm_6026_01>-altvaluesrc = 'I'.

*/AltValue2Source
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM'.
        <fs_ztmm_6026_01>-altvalue2src = 'I'.
      WHEN OTHERS.
    ENDCASE.

*/ALTCURRCODESRC
    CASE <fs_ztmm_6026_01>-txncode.
      WHEN 'RPPC' OR 'RPIM' OR 'APPC' OR 'APIM'.
        <fs_ztmm_6026_01>-altcurrcodesrc = 'I'.
      WHEN OTHERS.
    ENDCASE.

*/AdValoremRateSource
    <fs_ztmm_6026_01>-advaloremratesrc = 'H'.

*/SpecificRateSource
    <fs_ztmm_6026_01>-specificratesrc = 'H'.

*/UOMConvFactorSource
    <fs_ztmm_6026_01>-uomconvfactorsrc = 'I'.

*/AddUOMConvFactorSource
    <fs_ztmm_6026_01>-adduomconvfacsrc = 'I'.

*/RptQtyUomSrc
    <fs_ztmm_6026_01>-rptqtyuomsrc = 'H'.

*/AddRptQtyUomSrc
    <fs_ztmm_6026_01>-addrptqtyuomsrc = 'H'.

*/Constants
    <fs_ztmm_6026_01>-dotindicator     = 'N'.
    <fs_ztmm_6026_01>-fccindicator     = 'N'.
    <fs_ztmm_6026_01>-fdaindicator     = 'N'.
  ENDLOOP.

*/Begin of Added by Hakchin(2004012)
*Delete Color
  PERFORM delete_color
         USING it_ztmm_6026_01.
*/End of Added by Hakchin(20040412)
ENDFORM.                    " refine_data
*&---------------------------------------------------------------------*
*&      Form  itable_group_by_matnr_land1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM itable_group_by_matnr_land1
      USING iet_itable LIKE it_ztmm_6026_01.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <lf_summary> LIKE LINE OF iet_itable.
  DATA: lt_summary LIKE TABLE OF <lf_summary>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.

*/1.Make unique line by MATNR, LAND1
  lt_summary = iet_itable.
  SORT lt_summary BY matnr land1.
  DELETE ADJACENT DUPLICATES FROM lt_summary
                   COMPARING matnr land1.

*/2.Grouping by MATNR, LAND1
  SORT lt_summary BY matnr land1.
  LOOP AT lt_summary ASSIGNING <lf_summary>.
    CLEAR: <lf_summary>-menge.
    LOOP AT iet_itable ASSIGNING <lf_itable>
           WHERE matnr   = <lf_summary>-matnr AND
                 land1   = <lf_summary>-land1.
      <lf_summary>-menge = <lf_summary>-menge + <lf_itable>-menge.
    ENDLOOP.
  ENDLOOP.
  iet_itable = lt_summary.
ENDFORM.                    " itable_group_by_matnr_land1
*&---------------------------------------------------------------------*
*&      Form  get_skipped_mat_doc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_skipped_mat_doc
         TABLES ext_mkpf
                  STRUCTURE mkpf
         USING value(im_budat) LIKE mkpf-budat
               value(im_udate) LIKE ztmm_6026_01-udate
               value(im_utime) LIKE ztmm_6026_01-utime.
  CLEAR: ext_mkpf, ext_mkpf[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE ext_mkpf
    FROM mkpf
    WHERE ( budat = im_budat AND
            cpudt > im_udate
          )                     OR
          ( budat = im_budat AND
            cpudt = im_udate AND
            cputm > im_utime
          ).
  "CPUDT: Accounting document entry date
  "CPUTM: Time of entry
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_summarized_adjusting_data
*&---------------------------------------------------------------------*
FORM get_summarized_adjusting_data
       TABLES imt_mkpf
                STRUCTURE mkpf
              ext_ztmm_6026_01
                STRUCTURE ztmm_6026_01.
  CLEAR: ext_ztmm_6026_01, ext_ztmm_6026_01[].
*/For RPPC
  SELECT
      mseg~matnr  AS matnr "Material number
      makt~maktx  AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~lifnr        AS lifnr  "Vendor
      mseg~bwart  AS bwart  "Movement type
      mseg~menge  AS menge  "Quantity
      mseg~meins  AS meins  "Base unit of measure
      mara~profl  AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      mara~mtart  AS mtart  "Material Type
      mara~ntgew  AS ntgew  "Net weight
      mara~gewei  AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE ext_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr AND
         mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr AND
         mara~profl ='V'   "LP material
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  FOR ALL entries IN imt_mkpf
  WHERE
        ( mseg~bwart = '101' OR "GR goods receipt
          mseg~bwart = '501' OR "Receipt w/o PO
          mseg~bwart = '511' )    AND "Delivery w/o charge
        ( mara~mtart ='ROH' OR  "Production Materials
          mara~mtart ='HALB' OR  "Semifinished products
          mara~mtart ='FERT' OR  "Finished products
          mara~mtart ='ROH1' )    AND  "Raw / Sub Material
         mkpf~mblnr = imt_mkpf-mblnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  itab_grp_by_budat_matnr_lifnr
*&---------------------------------------------------------------------*
FORM itab_grp_by_budat_matnr_lifnr
      USING iet_itable LIKE it_ztmm_6026_01.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <lf_summary> LIKE LINE OF iet_itable.
  DATA: lt_summary LIKE TABLE OF <lf_summary>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.

*/1.Make unique line by BUDAT, MATNR, LIFNR
  lt_summary = iet_itable.
  SORT lt_summary BY budat matnr lifnr.
  DELETE ADJACENT DUPLICATES FROM lt_summary
                   COMPARING budat matnr lifnr.

*/2.Grouping by BUDAT, MATNR, LIFNR
  SORT lt_summary BY budat matnr lifnr.
  LOOP AT lt_summary ASSIGNING <lf_summary>.
    CLEAR: <lf_summary>-menge.
    LOOP AT iet_itable ASSIGNING <lf_itable>
                  WHERE budat = <lf_summary>-budat AND
                        matnr = <lf_summary>-matnr AND
                        lifnr = <lf_summary>-lifnr.
      <lf_summary>-menge = <lf_summary>-menge + <lf_itable>-menge.
    ENDLOOP.
  ENDLOOP.
  iet_itable = lt_summary.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_ippc_ipim_mseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ippc_ipim_mseg.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01 LIKE LINE OF lt_ztmm_6026_01.

*/1. Get PPIM including CO Production Order using posting date
*Already we got this data as IT_PPIM
*    We set the internal table LT_PPIM with unique key CO
*    Production Order(aufnr).

*/2. Get IPPC_IPIM using movement type = '261' and above extracted
*    CO Production Order.

*/3. Since until now, we can't know exact quantity, we make
* the temperary table with unique parent material(lt_ippc_ipim).
* (This internal table's volumn is smaller than
* the internal table's with unique CO Production Order Number)

*/4. And we do BOM Explosion using this temperaty internal table.
* Getting Usage( this time, we also extract which materials are real
* Child Materials.) and Child Materials' Qty.


*/1. Get PPIM including CO Production Order using posting date
*Already we got this data as IT_PPIM
*    We set the internal table LT_PPIM with unique key CO
*    Production Order(aufnr).

  FIELD-SYMBOLS: <ls_ppim> LIKE LINE OF it_ppim.
  DATA:          lt_ppim   LIKE TABLE OF <ls_ppim>.
  lt_ppim = it_ppim.
  SORT lt_ppim BY aufnr matnr budat. "(aufnr: CO Production Order)
  DELETE ADJACENT DUPLICATES FROM lt_ppim COMPARING aufnr.

*/2. Get IPPC_IPIM using movement type = '261' and above extracted
*    CO Production Order.
*    At this time, we will get the IPPC_IPIM data with looping the above
*    internal table lt_ppim
  DATA: lv_budat_low LIKE mkpf-budat.
  lv_budat_low = p_budat - 20. "Developer can adjust lv_budat_low.

  LOOP AT lt_ppim ASSIGNING <ls_ppim>.
    SELECT
        mseg~matnr  AS matnr "Material number
        MAX( makt~maktx )  AS maktx
        MAX( mseg~mblnr )  AS mblnr   "Number of material document
        MAX( mseg~mjahr )  AS mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
        mseg~ebeln         AS ebeln
        MAX( mseg~lifnr )  AS lifnr  "Vendor
        MAX( mseg~bwart )  AS bwart  "Movement type
*        SUM( mseg~menge )  AS menge  "Quantity
        MAX( mseg~meins )  AS meins  "Base unit of measure
        MAX( mseg~werks )  AS werks  "Plant
        MAX( mara~profl )  AS profl
        "K:Knock Down Parts, M:MIP, V:Local Parts
        MAX( mara~mtart )  AS mtart  "Material Type
        MAX( mara~ntgew )  AS ntgew  "Net weight
        MAX( mara~gewei )  AS gewei  "Weight Unit

        MAX( mseg~aufnr )  AS aufnr  "CO Production Order

        afpo~matnr         AS matnr_parent  "Parent material

*        mkpf~budat  AS budat"Posting Date
      APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
      FROM mkpf
        INNER JOIN mseg
          ON mseg~mblnr = mkpf~mblnr AND
             mseg~mjahr = mkpf~mjahr AND
*           mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
*/Begin of Added by Hakchin(20040501)
             mseg~aufnr = <ls_ppim>-aufnr  "CO Production Order
*/End of Added by Hakchin(20040501)
        INNER JOIN mara
          ON mara~matnr = mseg~matnr
        INNER JOIN makt
          ON makt~matnr = mara~matnr AND
             makt~spras = sy-langu

        INNER JOIN afpo   "Order item
          ON afpo~aufnr = mseg~aufnr AND  "CO Production Order number
             afpo~posnr = '0001'   "Order Item number

      WHERE
          ( mseg~bwart = '261'       "GI for order
          )                                  AND
          ( ( ( mara~mtart ='ROH' OR     "Production Materials
                mara~mtart ='ROH1'       "Raw / Sub Material
              )                     AND
              ( mara~profl = 'K'  OR   "KD
                mara~profl = 'V'       "LP
              )
            )                              OR
            ( ( mara~profl ='M'        "MIP
              )                     AND
              ( mara~mtart ='HALB'   "Semifinished products
              )
            )
          )                                        AND
          (
            mkpf~budat >= lv_budat_low AND
            mkpf~budat <= p_budat
          )
     GROUP BY afpo~matnr mseg~matnr mseg~ebeln. "(Parent mat, Child mat)
  ENDLOOP.

* OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    <fs_ztmm_6026_01>-budat = p_budat.
    CONCATENATE <fs_ztmm_6026_01>-matnr_parent
                <fs_ztmm_6026_01>-budat
        INTO <fs_ztmm_6026_01>-ordernumwork.

  ENDLOOP.

*/Begin of Added by Hakchin(20040504)
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
*/ Get Net Price, Effective Price and Currency from Info Record
    PERFORM get_netpr_effpr_waers
               USING    <fs_ztmm_6026_01>-matnr
               CHANGING <fs_ztmm_6026_01>-lifnr "Vendor code
                        <fs_ztmm_6026_01>-netpr
                        <fs_ztmm_6026_01>-effpr
                        <fs_ztmm_6026_01>-waers
                        <fs_ztmm_6026_01>-bpumz
                        <fs_ztmm_6026_01>-bpumn
                        <fs_ztmm_6026_01>-peinh.

*Adjusted Price
    IF NOT <fs_ztmm_6026_01>-bpumn IS INITIAL.
      <fs_ztmm_6026_01>-netpruom =
      ( <fs_ztmm_6026_01>-netpr * <fs_ztmm_6026_01>-bpumz ) /
      <fs_ztmm_6026_01>-bpumn.

      <fs_ztmm_6026_01>-effpruom =
      ( <fs_ztmm_6026_01>-effpr * <fs_ztmm_6026_01>-bpumz ) /
                                   <fs_ztmm_6026_01>-bpumn.

      IF NOT <fs_ztmm_6026_01>-peinh IS INITIAL.
        <fs_ztmm_6026_01>-netpruom = <fs_ztmm_6026_01>-netpruom /
                                     <fs_ztmm_6026_01>-peinh.

        <fs_ztmm_6026_01>-effpruom = <fs_ztmm_6026_01>-effpruom /
                                     <fs_ztmm_6026_01>-peinh.
      ENDIF.
    ENDIF.


*/ Get HS code & HTSNum Source
* First, We get marc-stawn(HS code) from werks = 'P001'.
* Second, if there is no HS code from werks = 'P001', then
* we try to get HS code from other plants.
* Third, at all efforts, if we don't get any HS code, then
* leave marc-stawn as space.
    PERFORM get_hscode
              USING    <fs_ztmm_6026_01>-matnr
              CHANGING <fs_ztmm_6026_01>-stawn. "HS code
  ENDLOOP.

*/End of Added by Hakchin(20040504)


*/3. And we do BOM Explosion using this temperaty internal table.
* Getting Usage( this time, we also extract which materials are real
* Child Materials.) and Child Materials' Qty.

*/Get QTYPERLM. (i.e. Usage)
  DATA: lt_ippc_ipim LIKE it_ztmm_6026_01.
  lt_ippc_ipim = lt_ztmm_6026_01.

  PERFORM get_qtyperlm
             USING lt_ztmm_6026_01
                   lt_ippc_ipim.

*/Begin of Added by Hakchin(20040521)
* We will get usages of skipped usages.
  FIELD-SYMBOLS: <ls_ippc_ipim> LIKE LINE OF lt_ippc_ipim.
  DATA: lt_ippc_ipim_tmp LIKE lt_ippc_ipim.
  DATA: lv_plnum TYPE plaf-plnum.
  DATA: lv_rsnum TYPE plaf-rsnum.
  LOOP AT lt_ippc_ipim ASSIGNING <ls_ippc_ipim>
                             WHERE qtyperlm IS initial.
    SELECT SINGLE
       plaf~plnum AS plnum
       plaf~rsnum AS rsnum
       resb~bdmng AS qtyperlm
       resb~meins AS meins
      INTO CORRESPONDING FIELDS OF <ls_ippc_ipim>
      FROM plaf
        INNER JOIN resb
        ON resb~rsnum = plaf~rsnum AND
           resb~matnr = <ls_ippc_ipim>-matnr
      WHERE plaf~matnr = <ls_ippc_ipim>-matnr_parent.

  ENDLOOP.
*/End of Added by Hakchin(20040521)

*/Get Child Materials' Qty.
* Child material qty = Child material usage * Parent material qyt.
  LOOP AT it_ppim ASSIGNING <ls_ppim>.
    LOOP AT lt_ippc_ipim ASSIGNING <fs_ztmm_6026_01>
                         WHERE matnr_parent = <ls_ppim>-matnr.

      <fs_ztmm_6026_01>-matnr_parent_cnt = <ls_ppim>-menge.
      <fs_ztmm_6026_01>-menge =
                 <fs_ztmm_6026_01>-qtyperlm * <ls_ppim>-menge.
    ENDLOOP.
  ENDLOOP.


*/Begin of Added by Hakchin(20040515)
*Delete Color
  PERFORM delete_color
         USING lt_ippc_ipim.
*Group by OrderNumWork MATNR with qty in lt_ippc_ipim
  PERFORM itable_group_by_onw_matnr
               USING lt_ippc_ipim.
*/End of Added by Hakchin(20040515)


*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_ippc_ipim TO it_ztmm_6026_01.
ENDFORM.                    " get_ippc_ipim_mseg
*&---------------------------------------------------------------------*
*&      Form  get_cppc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cppc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        as ebeln
      MAX( mseg~ummat ) AS adjproductnum "Contrary Material
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr AND
         mseg~shkzg = 'S'   "S:Debit, H:Credit
*         mseg~kzbew = 'B' "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  WHERE
        ( mseg~bwart = '309' OR
          mseg~bwart = '310'
        )                         AND
        ( mara~mtart ='ROH'  OR  "Production Materials
          mara~mtart ='HALB' OR  "Semifinished products
          mara~mtart ='ROH1'     "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    <fs_ztmm_6026_01>-txncode = 'CPPC'.
  ENDLOOP.

**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_cppc
*&---------------------------------------------------------------------*
*&      Form  get_cnpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cnpc.
  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr "Material number
      MAX( makt~maktx ) AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~ebeln        as ebeln
      MAX( mseg~ummat ) AS adjproductnum "Contrary Material
      MAX( mseg~lifnr ) AS lifnr  "Vendor
      MAX( mseg~bwart ) AS bwart   "Movement type
      SUM( mseg~menge ) AS menge  "Quantity
      MAX( mseg~meins ) AS meins  "Base unit of measure
      MAX( mara~profl ) AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      MAX( mara~mtart ) AS mtart  "Material Type
      MAX( mara~ntgew ) AS ntgew  "Net weight
      MAX( mara~gewei ) AS gewei "Weight Unit
      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr AND
         mseg~shkzg = 'H'   "S:Debit, H:Credit
*         mseg~kzbew = 'B' "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  WHERE
        ( mseg~bwart = '309' OR
          mseg~bwart = '310'
        )                         AND
        ( mara~mtart ='ROH' OR   "Production Materials
          mara~mtart ='HALB' OR  "Semifinished products
          mara~mtart ='ROH1'     "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
         mkpf~budat = p_budat
  GROUP BY mseg~matnr mkpf~budat mseg~ebeln.

  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    <fs_ztmm_6026_01>-txncode = 'CNPC'.
  ENDLOOP.



**/Begin of Added by Hakchin(20040329)
**Delete Color
*  PERFORM delete_color
*         USING lt_ztmm_6026_01.
*
***Group by matnr with menge(qty) in lt_ztmm_6026_01
**  PERFORM itable_group_by_matnr
**               USING lt_ztmm_6026_01.
**/End of Added by Hakchin(20040329)

  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.

ENDFORM.                    " get_cnpc
*&---------------------------------------------------------------------*
*&      Form  get_inpc_by_onw_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_inpc_by_onw_matnr.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01 LIKE LINE OF lt_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr   "Material number
      makt~maktx  AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~lifnr  AS lifnr   "Vendor
      mseg~bwart  AS bwart   "Movement type
      mseg~menge  AS menge   "Quantity
      mseg~meins  AS meins   "Base unit of measure
      mseg~werks  AS werks   "Plant
      mara~profl  AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      mara~mtart  AS mtart  "Material Type
      mara~ntgew  AS ntgew  "Net weight
      mara~gewei  AS gewei "Weight Unit

      afpo~matnr  AS matnr_parent

      mkpf~budat  AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
        ON mseg~mblnr = mkpf~mblnr AND
           mseg~mjahr = mkpf~mjahr "AND
*           mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
      INNER JOIN mara
        ON mara~matnr = mseg~matnr
      INNER JOIN makt
        ON makt~matnr = mara~matnr AND
           makt~spras = sy-langu

      INNER JOIN afpo   "Order item
        ON afpo~aufnr = mseg~aufnr AND  "Order number
           afpo~posnr = '0001'   "Order Item number

    WHERE
        ( mseg~bwart = '262'       "RE for order
        )                         AND
        ( mara~mtart ='ROH' OR     "Production Materials
          mara~mtart ='ROH1'       "Raw / Sub Material
        )                         AND
        ( mara~profl = 'K' OR
          mara~profl = 'V'
        )                         AND
          "K:Knock Down Parts, M:MIP, V:Local Parts
        mkpf~budat = p_budat
*  GROUP BY mseg~matnr mkpf~budat.
    ORDER BY afpo~matnr mseg~matnr mkpf~budat.

*/OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    CONCATENATE <fs_ztmm_6026_01>-matnr_parent p_budat
        INTO <fs_ztmm_6026_01>-ordernumwork.
  ENDLOOP.

*Group by OrderNumWork MATNR with qty in lt_ztmm_6026_01.
  PERFORM itable_group_by_onw_matnr
               USING lt_ztmm_6026_01.


*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_inpc_by_onw_matnr
*&---------------------------------------------------------------------*
*&      Form  get_inim_by_onw_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_inim_by_onw_matnr.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01 LIKE LINE OF lt_ztmm_6026_01.

  SELECT
      mseg~matnr  AS matnr   "Material number
      makt~maktx  AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~lifnr  AS lifnr   "Vendor
      mseg~bwart  AS bwart   "Movement type
      mseg~menge  AS menge   "Quantity
      mseg~meins  AS meins   "Base unit of measure
      mseg~werks  AS werks   "Plant
      mara~profl  AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      mara~mtart  AS mtart  "Material Type
      mara~ntgew  AS ntgew  "Net weight
      mara~gewei  AS gewei "Weight Unit

      afpo~matnr  AS matnr_parent

      mkpf~budat  AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr "AND
*         mseg~kzbew = 'F'" Movement indicator
         "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr "AND
*         mara~profl ='M'
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
      INNER JOIN marc
        ON marc~matnr = mseg~matnr AND
           marc~fevor = 'SEA' "Production scheduler
      "SEA: HMMA ENGINE PLANT ASSY

      INNER JOIN afpo   "Order item
        ON afpo~aufnr = mseg~aufnr AND  "Order number
           afpo~posnr = '0001'   "Order Item number

    WHERE
        (
          mseg~bwart = '262'     AND
          (
            mara~mtart ='FERT' OR "Finished products
            mara~mtart ='HALB'    "Semifinished products
          )                      AND
          mara~profl ='M'
        )                                     AND
        mkpf~budat = p_budat
*  GROUP BY mseg~matnr mkpf~budat.
    ORDER BY afpo~matnr mseg~matnr mkpf~budat.

*/OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    CONCATENATE <fs_ztmm_6026_01>-matnr_parent p_budat
        INTO <fs_ztmm_6026_01>-ordernumwork.
  ENDLOOP.

*Group by OrderNumWork MATNR with qty in lt_ztmm_6026_01.
  PERFORM itable_group_by_onw_matnr
               USING lt_ztmm_6026_01.


*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " get_inim_by_onw_matnr
*&---------------------------------------------------------------------*
*&      Form  get_ippc_ipim_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ippc_ipim_bom_fsc.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01 LIKE LINE OF lt_ztmm_6026_01.

  FIELD-SYMBOLS: <fs_tmp> like line of IT_PPIM.

*/1. Get PPIM using posting date
*Already we got this data as IT_PPIM

*/2. BOM Explosion in LT_PPIM

*/1. Get PPIM using posting date
*Already we got this data as IT_PPIM

*/2. BOM Explosion in LT_PPIM
* Get LT_PPIM
  DATA: lt_ppim LIKE it_ppim.
* lt_ppim = it_ppim.
*&---Check for plant            shiva.
  loop at it_ppim assigning <fs_tmp>.
    read table t_marc with key matnr = <fs_tmp>-matnr
                               werks = <fs_tmp>-werks
                               transporting no fields.
    check sy-subrc eq 0.
    append <fs_tmp> to lt_ppim.
  endloop.
*&---end                         shiva.

*/Begin of Added by Hakchin(20040521).
*  DELETE lt_ppim WHERE mtart <> 'FERT'. "Get Only FSC
*/End of Added by Hakchin(20040521).

  SORT lt_ppim BY matnr.
  DELETE ADJACENT DUPLICATES FROM lt_ppim
                  COMPARING matnr.

  DATA:          lt_ippc_ipim   LIKE         it_ztmm_6026_01.
  FIELD-SYMBOLS: <ls_ippc_ipim> LIKE LINE OF lt_ztmm_6026_01.

  PERFORM get_data_from_bom
                       USING lt_ppim
                             lt_ippc_ipim.
*Delete Color
  PERFORM delete_color
         USING lt_ippc_ipim.

*Group by OrderNumWork MATNR with qty in lt_ippc_ipim
  SORT lt_ippc_ipim BY matnr_parent matnr.
  PERFORM itable_group_by_onw_matnr
               USING lt_ippc_ipim.

*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_ippc_ipim TO it_ztmm_6026_01.

ENDFORM.                    " get_ippc_ipim_bom
*&---------------------------------------------------------------------*
*&      Form  get_data_from_bom
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PPIM  text
*      -->P_LT_IPPC_IPIM  text
*----------------------------------------------------------------------*
FORM get_data_from_bom
                 USING imt_ppim         LIKE it_ztmm_6026_01
                       ext_parent_child LIKE it_ztmm_6026_01.
  CLEAR: ext_parent_child.

  FIELD-SYMBOLS: <ls_ppim> LIKE LINE OF imt_ppim.
*  BOM Items (Extended for List Displays)
  DATA:          lt_stpox        LIKE TABLE OF stpox.
  DATA:          ls_stpox        LIKE LINE OF  lt_stpox.
  FIELD-SYMBOLS: <ls_stpox> LIKE LINE OF lt_stpox.

  DATA: lv_bom_src       TYPE matnr.
  DATA: lv_date TYPE d,
        lv_stlal LIKE mast-stlal.   "Alternative BOM
*&----Shiva.
  data: begin of wa_mat_cqty,
          matnr like mara-matnr,
          menge like stpox-menge,
        end of wa_mat_cqty.
  data: begin of wa_mat_type,
          matnr like mara-matnr,
          mtart like mara-mtart,
          profl like mara-profl,
        end of wa_mat_type.
  data: wa_stpox like stpox.
  data: t_mat_cqty like table of wa_mat_cqty,
        t_mat_type like table of wa_mat_type.
  data: w_noline type i.

  LOOP AT imt_ppim ASSIGNING <ls_ppim>.
    lv_bom_src = <ls_ppim>-matnr.  "Parent Material

*
    PERFORM get_validitydate_alterna_bom
                        USING    <ls_ppim>-plnum
                        CHANGING lv_date
                                 lv_stlal. "Alternative Bom
    IF lv_date IS INITIAL. lv_date = p_budat - 7. ENDIF.

* BOM Explosioin
    PERFORM cs_bom_expl_mat_v2   "similar to /nCS12
             TABLES lt_stpox
             USING 'PP01'     "Application ID
                   lv_date    "Validity date
                   '1' "EWahrHandl. ' 'kein,'1'modPosMg,'2'%ualeEMengen
                   '1' "Required quantity
                   'X' "'X':Multi-level explosion
                   '1' "Memory use ('1'=on;'0'=off;' '=no reaction)
                   lv_bom_src    "Source Material
                   lv_stlal           "Alternative BOM
                   '1'           "BOM usage (Always 1: Production)
                   <ls_ppim>-werks.   "Plant
    CLEAR: wa_ztmm_6026_01.
*&----Sum the quantity of component       "shiva"
    clear: wa_mat_cqty, wa_mat_type.
    refresh: t_mat_cqty, t_mat_type.
    delete lt_stpox where not dumps is initial.
    sort lt_stpox by idnrk.
    loop at lt_stpox into wa_stpox.
      wa_mat_cqty-matnr = wa_stpox-idnrk.
      wa_mat_cqty-menge = wa_stpox-mngko.
      collect wa_mat_cqty into t_mat_cqty.
    endloop.

    sort t_mat_cqty by matnr.

    describe table t_mat_cqty lines w_noline.
    if w_noline gt 0.
      select matnr mtart profl from mara
                               into table t_mat_type
                               for all entries in t_mat_cqty
                               where matnr = t_mat_cqty-matnr.
    endif.
    sort t_mat_type by matnr.
*&---End sum.

*    LOOP AT lt_stpox ASSIGNING <ls_stpox>
*                   WHERE dumps IS initial.  "Phantom item indicator
    loop at lt_stpox assigning <ls_stpox>.
      MOVE   <ls_ppim>-matnr  TO wa_ztmm_6026_01-matnr_parent.
      MOVE   <ls_ppim>-menge  TO wa_ztmm_6026_01-matnr_parent_cnt.

      CONCATENATE wa_ztmm_6026_01-matnr_parent
                  p_budat
                  INTO wa_ztmm_6026_01-ordernumwork.

      MOVE   <ls_stpox>-idnrk TO wa_ztmm_6026_01-matnr.
      MOVE   <ls_stpox>-meins TO wa_ztmm_6026_01-meins.
*&----following line commented by shiva for sum the qty.
*MOVE   <ls_stpox>-mngko TO wa_ztmm_6026_01-qtyperlm.
      read table t_mat_cqty into wa_mat_cqty
                            with key matnr = <ls_stpox>-idnrk
                            binary search.
      if sy-subrc ne 0.
      else.
        wa_ztmm_6026_01-qtyperlm = wa_mat_cqty-menge.
      endif.
*&---End shiva.

*/Begin of Added by Hakchin(20040521)
      MOVE   <ls_stpox>-mtart TO wa_ztmm_6026_01-mtart.
      IF wa_ztmm_6026_01-mtart = 'HALB'.
        read table t_mat_type into wa_mat_type
                              with key matnr = wa_ztmm_6026_01-matnr
                                       mtart = wa_ztmm_6026_01-mtart
                                       binary search.
        if wa_mat_type-profl = 'K'.
          MOVE   'IPPC'           TO wa_ztmm_6026_01-txncode.
          MOVE   'PC'             TO wa_ztmm_6026_01-ptc.
        else.
          MOVE   'IPIM'           TO wa_ztmm_6026_01-txncode.
          MOVE   'IM'             TO wa_ztmm_6026_01-ptc.
        endif.
      ELSE.
        MOVE   'IPPC'           TO wa_ztmm_6026_01-txncode.
        MOVE   'PC'             TO wa_ztmm_6026_01-ptc.
      ENDIF.
      MOVE   <ls_stpox>-ojtxp TO wa_ztmm_6026_01-maktx. "Mat Desc
      MOVE   <ls_stpox>-stawn TO wa_ztmm_6026_01-stawn.
      IF wa_ztmm_6026_01-stawn IS INITIAL.
        wa_ztmm_6026_01-stawn = '0000.00.0000'.
      ENDIF.

      MOVE   <ls_stpox>-werks TO wa_ztmm_6026_01-werks.
*/End of Added by Hakchin(20040521)*/
      wa_ztmm_6026_01-menge = wa_ztmm_6026_01-qtyperlm *
                              wa_ztmm_6026_01-matnr_parent_cnt.

*/ Date & Time.
      wa_ztmm_6026_01-udate = w_date.
      wa_ztmm_6026_01-utime = w_time.
      CONCATENATE wa_ztmm_6026_01-udate  "Present Date
                  'T'
                  wa_ztmm_6026_01-utime  "Present Time
                  INTO wa_ztmm_6026_01-date_time.


      IF NOT <ls_stpox>-sortf IS INITIAL. "Sort string
        APPEND wa_ztmm_6026_01  TO ext_parent_child.
      ENDIF.

    ENDLOOP.
  ENDLOOP.

  free: wa_mat_cqty, t_mat_cqty, wa_mat_type, t_mat_type.
  SORT ext_parent_child BY matnr_parent   matnr.

  DELETE ADJACENT DUPLICATES FROM ext_parent_child
                             COMPARING matnr_parent matnr.

ENDFORM.                    " get_data_from_bom
*&---------------------------------------------------------------------*
*&      Form  get_present_date_time
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_present_date_time.
  w_date = sy-datum.
  w_time = sy-uzeit.
ENDFORM.                    " get_present_date_time
*&---------------------------------------------------------------------*
*&      Form  get_planned_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_planned_order
                  USING value(im_budat) TYPE mkpf-budat
                        iet_ppim LIKE it_ztmm_6026_01.
  DATA: BEGIN OF ls_equnr,
         equnr LIKE equi-equnr, "Equipment number
        END OF ls_equnr.
  DATA: lt_equnr LIKE TABLE OF ls_equnr.
  FIELD-SYMBOLS: <fs_equnr> LIKE LINE OF lt_equnr.

  DATA: ls_ausp LIKE ausp.
  DATA: lt_ausp LIKE TABLE OF ls_ausp.
  FIELD-SYMBOLS: <ls_ausp> LIKE LINE OF lt_ausp.

  FIELD-SYMBOLS: <ls_ppim> LIKE LINE OF iet_ppim.

*/
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


*/Conversion: character type for signoffdate.
  DATA: lv_signoffdate(8).
  lv_signoffdate = im_budat.

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

  LOOP AT lt_ausp ASSIGNING <ls_ausp>.
    MOVE <ls_ausp>-objek TO ls_equnr-equnr.
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

*Get Planned Order
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_PLAN_ORDER'.  "Planned Order
    lv_plannedorder = ls_allocvalueschar-value_char.

*/Get Alternative BOM
    READ TABLE lt_allocvalueschar INTO ls_allocvalueschar
             WITH KEY charact = 'P_VERSION'. "Spec Version
    CONDENSE ls_allocvalueschar-value_char.
*Delete leading zero
    PERFORM conversion_exit_alpha_output
                            USING    ls_allocvalueschar-value_char
                            CHANGING lv_stlal. "Alternative BOM
*/Make lt_ztmm_6026_01
    MOVE: lv_fsccode      TO ls_ztmm_6026_01-matnr.
    MOVE: lv_extcolor     TO ls_ztmm_6026_01-colorext.
    MOVE: lv_intcolor     TO ls_ztmm_6026_01-colorint.
    MOVE: lv_plannedorder TO ls_ztmm_6026_01-plnum.  "Planned Order
    APPEND ls_ztmm_6026_01 TO lt_ztmm_6026_01.
  ENDLOOP.

*/
  LOOP AT iet_ppim ASSIGNING <ls_ppim>.
    READ TABLE lt_ztmm_6026_01 INTO ls_ztmm_6026_01
                     WITH KEY matnr = <ls_ppim>-matnr.
    IF sy-subrc = 0.
      <ls_ppim>-plnum = ls_ztmm_6026_01-plnum.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_planned_order
*&---------------------------------------------------------------------*
*&      Form  bapi_objcl_getclasses
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ALLOCLIST  text
*      -->P_LT_BAPIRET2  text
*      -->P_LV_OBJECTKEY  text
*      -->P_LV_OBJECTTABLE  text
*      -->P_LV_CLASSTYPE  text
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
*&      Form  conversion_exit_alpha_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_ALLOCVALUESCHAR_VALUE_CHAR  text
*      <--P_LV_STLAL  text
*----------------------------------------------------------------------*
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
*&      Form  get_validitydate_alterna_bom
*&---------------------------------------------------------------------*
FORM get_validitydate_alterna_bom
              USING    value(im_plnum) LIKE plaf-plnum
              CHANGING value(ex_date)  LIKE sy-datum   "Valid date
                       value(ex_stlal) LIKE mast-stlal."Alterna BOM
  CLEAR: ex_date, ex_stlal.
  SELECT SINGLE paltr stalt
    INTO (ex_date, ex_stlal)
    FROM plaf
    WHERE plnum = im_plnum.
ENDFORM.                    " get_validitydate_alterna_bom
*&---------------------------------------------------------------------*
*&      Form  get_ippc_ipim_mseg_engine
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ippc_ipim_mseg_engine.
  DATA: lt_ztmm_6026_01 LIKE it_ztmm_6026_01.
  DATA: ls_ztmm_6026_01 LIKE LINE OF lt_ztmm_6026_01.

*/1. Get PPIM including CO Production Order using posting date
*Already we got this data as IT_PPIM
*    We set the internal table LT_PPIM with unique key CO
*    Production Order(aufnr).

*/2. Get IPPC_IPIM using movement type = '261' and above extracted
*    CO Production Order.

*/3. Since until now, we can't know exact quantity, we make
* the temperary table with unique parent material(lt_ippc_ipim).
* (This internal table's volumn is smaller than
* the internal table's with unique CO Production Order Number)

*/4. And we do BOM Explosion using this temperaty internal table.
* Getting Usage( this time, we also extract which materials are real
* Child Materials.) and Child Materials' Qty.


*/1. Get PPIM including CO Production Order using posting date
*Already we got this data as IT_PPIM
*    We set the internal table LT_PPIM with unique key CO
*    Production Order(aufnr).

*&--------------------------------------------------------------------&*
*&                        IMPORTANT       - Shiva
*&   Based on KB Lee's request we are including the logic of checking
*&  5th and 6th digit of material number to read MSEG table.
*&--------------------------------------------------------------------&*
  FIELD-SYMBOLS: <ls_ppim> LIKE LINE OF it_ppim.
  FIELD-SYMBOLS: <ls_6026> LIKE LINE OF it_ppim.
  DATA:          lt_ppim   LIKE TABLE OF <ls_ppim>.

*  lt_ppim = it_ppim.
  loop at it_ppim assigning <ls_6026>.
    if <ls_6026>-matnr+4(2) ne 'XX' and <ls_6026>-matnr+4(2) ne 'XY'.
      append <ls_6026> to lt_ppim.
    endif.
  endloop.
*/Begin of Added by Hakchin(20040521).
  DELETE lt_ppim WHERE mtart = 'FERT'. "Avoid FSC
*/End of Added by Hakchin(20040521).
  SORT lt_ppim BY aufnr matnr budat. "(aufnr: CO Production Order)
  DELETE ADJACENT DUPLICATES FROM lt_ppim COMPARING aufnr.

*/2. Get IPPC_IPIM using movement type = '261' and above extracted
*    CO Production Order.
*    At this time, we will get the IPPC_IPIM data with looping the above
*    internal table lt_ppim
  DATA: lv_budat_low LIKE mkpf-budat.
  lv_budat_low = p_budat - 20. "Developer can adjust lv_budat_low.

  LOOP AT lt_ppim ASSIGNING <ls_ppim>.
    SELECT
        mseg~matnr  AS matnr "Material number
        MAX( makt~maktx )  AS maktx
        MAX( mseg~mblnr )  AS mblnr   "Number of material document
        MAX( mseg~mjahr )  AS mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
        mseg~ebeln         AS ebeln
        MAX( mseg~lifnr )  AS lifnr  "Vendor
        MAX( mseg~bwart )  AS bwart  "Movement type
*        SUM( mseg~menge )  AS menge  "Quantity
        MAX( mseg~meins )  AS meins  "Base unit of measure
        MAX( mseg~werks )  AS werks  "Plant
        MAX( mara~profl )  AS profl
        "K:Knock Down Parts, M:MIP, V:Local Parts
        MAX( mara~mtart )  AS mtart  "Material Type
        MAX( mara~ntgew )  AS ntgew  "Net weight
        MAX( mara~gewei )  AS gewei  "Weight Unit

        MAX( mseg~aufnr )  AS aufnr  "CO Production Order

        afpo~matnr         AS matnr_parent  "Parent material

*        mkpf~budat  AS budat"Posting Date
      APPENDING CORRESPONDING FIELDS OF TABLE lt_ztmm_6026_01
      FROM mkpf
        INNER JOIN mseg
          ON mseg~mblnr = mkpf~mblnr AND
             mseg~mjahr = mkpf~mjahr AND
*           mseg~kzbew = 'B'  "Movement indicator - Goods Mvt for PO
*/Begin of Added by Hakchin(20040501)
             mseg~aufnr = <ls_ppim>-aufnr  "CO Production Order
*/End of Added by Hakchin(20040501)
        INNER JOIN mara
          ON mara~matnr = mseg~matnr
        INNER JOIN makt
          ON makt~matnr = mara~matnr AND
             makt~spras = sy-langu

        INNER JOIN afpo   "Order item
          ON afpo~aufnr = mseg~aufnr AND  "CO Production Order number
             afpo~posnr = '0001'   "Order Item number

      WHERE
          ( mseg~bwart = '261'       "GI for order
          )                                  AND
*&----commented by shiva 08/04/04
          ( ( ( mara~mtart ='ROH' OR     "Production Materials
                mara~mtart ='ROH1'       "Raw / Sub Material
              )                     AND
              ( mara~profl = 'K'  OR   "KD
                mara~profl = 'V'       "LP
              )
            )                              OR
*&----end shiva 08/04/04
            ( ( mara~profl ='M'        "MIP
              )                     AND
              ( mara~mtart ='HALB'   "Semifinished products
              )
            )
          )                         AND   "shiva commented
     (
            mkpf~budat >= lv_budat_low AND
            mkpf~budat <= p_budat
          )
    GROUP BY afpo~matnr mseg~matnr mseg~ebeln. "(Parent mat, Child mat)
  ENDLOOP.

* OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    <fs_ztmm_6026_01>-budat = p_budat.
    CONCATENATE <fs_ztmm_6026_01>-matnr_parent
                <fs_ztmm_6026_01>-budat
        INTO <fs_ztmm_6026_01>-ordernumwork.

  ENDLOOP.

*/3. And we do BOM Explosion using this temperaty internal table.
* Getting Usage( this time, we also extract which materials are real
* Child Materials.) and Child Materials' Qty.

*/Get QTYPERLM. (i.e. Usage)
  DATA: lt_ippc_ipim LIKE it_ztmm_6026_01.
  lt_ippc_ipim = lt_ztmm_6026_01.

  PERFORM get_qtyperlm
             USING lt_ztmm_6026_01
                   lt_ippc_ipim.

*/Begin of Added by Hakchin(20040521)
* We will get usages of skipped usages.
  FIELD-SYMBOLS: <ls_ippc_ipim> LIKE LINE OF lt_ippc_ipim.
  DATA: lt_ippc_ipim_tmp LIKE lt_ippc_ipim.
  DATA: lv_plnum TYPE plaf-plnum.
  DATA: lv_rsnum TYPE plaf-rsnum.
  LOOP AT lt_ippc_ipim ASSIGNING <ls_ippc_ipim>
                             WHERE qtyperlm IS initial.
    SELECT SINGLE
       plaf~plnum AS plnum
       plaf~rsnum AS rsnum
       resb~bdmng AS qtyperlm
       resb~meins AS meins
      INTO CORRESPONDING FIELDS OF <ls_ippc_ipim>
      FROM plaf
        INNER JOIN resb
        ON resb~rsnum = plaf~rsnum AND
           resb~matnr = <ls_ippc_ipim>-matnr
      WHERE plaf~matnr = <ls_ippc_ipim>-matnr_parent.

  ENDLOOP.
*/End of Added by Hakchin(20040521)

*/Get Child Materials' Qty.
* Child material qty = Child material usage * Parent material qyt.
  LOOP AT it_ppim ASSIGNING <ls_ppim>.
    LOOP AT lt_ippc_ipim ASSIGNING <fs_ztmm_6026_01>
                         WHERE matnr_parent = <ls_ppim>-matnr.

      <fs_ztmm_6026_01>-matnr_parent_cnt = <ls_ppim>-menge.
      <fs_ztmm_6026_01>-menge =
                 <fs_ztmm_6026_01>-qtyperlm * <ls_ppim>-menge.
    ENDLOOP.
  ENDLOOP.

*/Begin of Added by Hakchin(20040515)
*Delete Color
  PERFORM delete_color
         USING lt_ippc_ipim.
*Group by OrderNumWork MATNR with qty in lt_ippc_ipim
  PERFORM itable_group_by_onw_matnr
               USING lt_ippc_ipim.
*/End of Added by Hakchin(20040515)


*/ Appending to IT_ZTMM_6026_01
  APPEND LINES OF lt_ippc_ipim TO it_ztmm_6026_01.

ENDFORM.                    " get_ippc_ipim_mseg_engine
*&---------------------------------------------------------------------*
*&      Form  rppc_adjusting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rppc_adjusting
              USING imt_budat_udate_utime LIKE it_budat_udate_utime.

  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

*3. Get Skipped Material Doc using budat, udate, utime
  CHECK NOT imt_budat_udate_utime IS INITIAL.
  DATA: lt_mkpf     LIKE TABLE OF mkpf.
  DATA: lt_mkpf_tmp LIKE TABLE OF mkpf.
  LOOP AT imt_budat_udate_utime INTO wa_budat_udate_utime.
    PERFORM get_skipped_mat_doc
             TABLES lt_mkpf_tmp
             USING  wa_budat_udate_utime-budat
                    wa_budat_udate_utime-udate
                    wa_budat_udate_utime-utime.
    APPEND LINES OF lt_mkpf_tmp TO lt_mkpf.
  ENDLOOP.

*4. Based on Given Material Doc, Get Summarized Adjusting Data
  CHECK NOT lt_mkpf IS INITIAL.
  PERFORM get_summarized_adjusting_data
         TABLES lt_mkpf
                lt_ztmm_6026_01.

*5. Group by budat, matnr, lifnr with menge(qty) in lt_ztmm_6026_01
  CHECK NOT lt_ztmm_6026_01 IS INITIAL.
  PERFORM itab_grp_by_budat_matnr_lifnr
                 USING lt_ztmm_6026_01.

*/
  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " rppc_adjusting
*&---------------------------------------------------------------------*
*&      Form  ppim_adjusting
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ppim_adjusting
              USING imt_budat_udate_utime LIKE it_budat_udate_utime.

  DATA: ls_ztmm_6026_01 LIKE LINE OF it_ztmm_6026_01.
  DATA: lt_ztmm_6026_01 LIKE TABLE OF ls_ztmm_6026_01.

*3. Get Skipped Material Doc using budat, udate, utime
  CHECK NOT imt_budat_udate_utime IS INITIAL.
  DATA: lt_mkpf     LIKE TABLE OF mkpf.
  DATA: lt_mkpf_tmp LIKE TABLE OF mkpf.
  LOOP AT imt_budat_udate_utime INTO wa_budat_udate_utime.
    PERFORM get_skipped_mat_doc
             TABLES lt_mkpf_tmp
             USING  wa_budat_udate_utime-budat
                    wa_budat_udate_utime-udate
                    wa_budat_udate_utime-utime.
    APPEND LINES OF lt_mkpf_tmp TO lt_mkpf.
  ENDLOOP.

*4. Based on Given Material Doc, Get Summarized Adjusting Data
  CHECK NOT lt_mkpf IS INITIAL.
  PERFORM summarized_adjusting_data_ppim
         TABLES lt_mkpf
                lt_ztmm_6026_01.

*5. Group by budat, matnr with menge(qty) in lt_ztmm_6026_01
  CHECK NOT lt_ztmm_6026_01 IS INITIAL.
  PERFORM itab_grp_by_budat_matnr
                 USING lt_ztmm_6026_01.

*/Make OrderNumWork
  LOOP AT lt_ztmm_6026_01 ASSIGNING <fs_ztmm_6026_01>.
    IF <fs_ztmm_6026_01>-bwart = '131'.
      CONCATENATE <fs_ztmm_6026_01>-matnr
                  p_budat
        INTO <fs_ztmm_6026_01>-ordernumwork.
    ENDIF.
  ENDLOOP.

*/
  APPEND LINES OF lt_ztmm_6026_01 TO it_ztmm_6026_01.
ENDFORM.                    " ppim_adjusting
*&---------------------------------------------------------------------*
*&      Form  summarized_adjusting_data_ppim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_MKPF  text
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM summarized_adjusting_data_ppim
       TABLES imt_mkpf
                STRUCTURE mkpf
              ext_ztmm_6026_01
                STRUCTURE ztmm_6026_01.
  CLEAR: ext_ztmm_6026_01, ext_ztmm_6026_01[].
*/For PPIM
  SELECT
      mseg~matnr  AS matnr "Material number
      makt~maktx  AS maktx
*      mseg~mblnr   "Number of material document
*      mseg~mjahr   "Material doc. year
*      mseg~zeile   "Item in material document
      mseg~lifnr        AS lifnr  "Vendor
      mseg~bwart  AS bwart  "Movement type
      mseg~menge  AS menge  "Quantity
      mseg~meins  AS meins  "Base unit of measure
      mara~profl  AS profl
      "K:Knock Down Parts, M:MIP, V:Local Parts
      mara~mtart  AS mtart  "Material Type
      mara~ntgew  AS ntgew  "Net weight
      mara~gewei  AS gewei "Weight Unit

      mseg~aufnr  AS aufnr "CO Production Order

      mkpf~budat        AS budat"Posting Date
    APPENDING CORRESPONDING FIELDS OF TABLE ext_ztmm_6026_01
    FROM mkpf
      INNER JOIN mseg
      ON mseg~mblnr = mkpf~mblnr AND
         mseg~mjahr = mkpf~mjahr
*           mseg~kzbew = 'F'" Movement indicator
           "F: Goods movement for production order
      INNER JOIN mara
      ON mara~matnr = mseg~matnr
*         mara~profl ='M'   "MIP material
      INNER JOIN makt
      ON makt~matnr = mara~matnr AND
         makt~spras = sy-langu
  FOR ALL entries IN imt_mkpf
  WHERE
        ( mseg~bwart = '101'   OR "GR goods receipt
          mseg~bwart = '131'      "Goods receipt
        )                           AND
        ( mara~mtart ='FERT'   OR "Finished products
          ( mara~profl ='M' AND
            ( mara~mtart ='ROH'  OR "Production Materials
              mara~mtart ='HALB' OR "Semifinished products
              mara~mtart ='ROH1'    "Raw / Sub Material
            )
          )
        )                           AND
        mkpf~mblnr = imt_mkpf-mblnr.

ENDFORM.                    " summarized_adjusting_data_ppim
*&---------------------------------------------------------------------*
*&      Form  itab_grp_by_budat_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM itab_grp_by_budat_matnr
      USING iet_itable LIKE it_ztmm_6026_01.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <lf_summary> LIKE LINE OF iet_itable.
  DATA: lt_summary LIKE TABLE OF <lf_summary>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <lf_itable> LIKE LINE OF iet_itable.

*/1.Make unique line by BUDAT, MATNR
  lt_summary = iet_itable.
  SORT lt_summary BY budat matnr.
  DELETE ADJACENT DUPLICATES FROM lt_summary
                   COMPARING budat matnr.

*/2.Grouping by BUDAT, MATNR
  SORT lt_summary BY budat matnr.
  LOOP AT lt_summary ASSIGNING <lf_summary>.
    CLEAR: <lf_summary>-menge.
    LOOP AT iet_itable ASSIGNING <lf_itable>
                  WHERE budat = <lf_summary>-budat AND
                        matnr = <lf_summary>-matnr.
      <lf_summary>-menge = <lf_summary>-menge + <lf_itable>-menge.
    ENDLOOP.
  ENDLOOP.
  iet_itable = lt_summary.
ENDFORM.                    " itab_grp_by_budat_matnr
