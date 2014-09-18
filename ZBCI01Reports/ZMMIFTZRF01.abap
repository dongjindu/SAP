*----------------------------------------------------------------------*
*   INCLUDE ZMMIFTZRF01                                                *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*      -->P_NRO_NR_00  text
*      -->P_NRO_OBJECT  text
*      <--P_WA_ztmm_6026_01_LOGNO_H  text
*----------------------------------------------------------------------*
FORM NUMBER_GET_NEXT

           USING    VALUE(P_NRO_INTERVAL) LIKE INRI-NRRANGENR
                    VALUE(P_NRO_OBJECT)   LIKE INRI-OBJECT
           CHANGING VALUE(P_NRO_NEXT).

  CLEAR: P_NRO_NEXT.

  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = P_NRO_INTERVAL
            OBJECT                  = P_NRO_OBJECT
       IMPORTING
            NUMBER                  = P_NRO_NEXT
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            OTHERS                  = 7.

  IF SY-SUBRC <> 0.
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
FORM PROCESS_DATA
          USING VALUE(IM_ZDOCNO) TYPE NUM10
                IMT_ZTMM_6026_01 LIKE IT_ZTMM_6026_01.

  STATICS: LV_LOGNO_H TYPE NUM10.
  DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
  DATA: LV_MESSAGE TYPE BAPI_MSG. "Message text (220)
  CONSTANTS : C_DEST(10) VALUE 'WMGM01'.

*/ Call Outbound RFC FM
  CALL FUNCTION 'Z_FMM_6026_OUT_MATFIFO'
    DESTINATION              C_DEST
    TABLES
      EXT_ZTMM_6026_01      = IMT_ZTMM_6026_01
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE LV_MESSAGE
      SYSTEM_FAILURE        = 2 MESSAGE LV_MESSAGE.
  IF SY-SUBRC NE 0.
    LV_ZRESULT = 'E'.  "Result of the Processing
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ELSE.
    LV_ZRESULT = 'S'.  "Result of the Processing
    LV_MESSAGE = 'Outbound RFC FM Connected!'(002).
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ENDIF.

*/ Modify it_ztmm_6026_01
*  CLEAR: lv_logno_h.
  LOOP AT IMT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>.
*    PERFORM number_get_next USING    '00'
*                                     'ZMMNRO0002'
*                            CHANGING lv_logno_h.
    LV_LOGNO_H = LV_LOGNO_H + 1.

    <FS_ZTMM_6026_01>-ZDOCNO  = IM_ZDOCNO.  "App. Doc. No.
    <FS_ZTMM_6026_01>-LOGNO_H = LV_LOGNO_H."Logno Header

    <FS_ZTMM_6026_01>-ZUSER   = SY-UNAME.  "User name
*    <fs_ztmm_6026_01>-zsdat   = .  "Send File Created Date
*    <fs_ztmm_6026_01>-zstim   = .  "Send file Created Time
    <FS_ZTMM_6026_01>-ZEDAT   = W_DATE.    "SAP Interface Date
    <FS_ZTMM_6026_01>-ZETIM   = W_TIME.    "SAP Interface Time
    <FS_ZTMM_6026_01>-ZMODE   = 'C'.       "Data Characteristic Flag
    <FS_ZTMM_6026_01>-ZRESULT = LV_ZRESULT."Result of the Processing
    <FS_ZTMM_6026_01>-ZMSG    = LV_MESSAGE."Message text
*    <fs_ztmm_6026_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ztmm_6026_01.
  INSERT ZTMM_6026_01 FROM TABLE IMT_ZTMM_6026_01.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_FCA_EAI_INTERFACE_LOG.
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

  DATA: LV_TOTAL TYPE I.
  DESCRIBE TABLE IT_ZTMM_6026_01 LINES LV_TOTAL.

  CHECK NOT LV_TOTAL IS INITIAL.
  CLEAR: WA_ZTCA_IF_LOG.
  LOOP AT IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>.
    IF <FS_ZTMM_6026_01>-ZZRET = 'S'.
      WA_ZTCA_IF_LOG-ZSUCC = WA_ZTCA_IF_LOG-ZSUCC + 1.
    ELSEIF <FS_ZTMM_6026_01>-ZZRET = 'E'.
      WA_ZTCA_IF_LOG-ERROR = WA_ZTCA_IF_LOG-ERROR + 1.
    ENDIF.
  ENDLOOP.

  WA_ZTCA_IF_LOG-TCODE = 'ZMMI76'. "Present Transaction Code
  WA_ZTCA_IF_LOG-TOTAL = LV_TOTAL. "Total Execution number
  WA_ZTCA_IF_LOG-ERDAT = W_DATE.   "Created on.
  WA_ZTCA_IF_LOG-ERZET = W_TIME.   "Created time.
  WA_ZTCA_IF_LOG-ERNAM = SY-UNAME. "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      I_ZTCA_IF_LOG     = WA_ZTCA_IF_LOG
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     UPDATE_FAILED              = 1
     NUMBER_RANGE_ERROR         = 2
     TCODE_DOES_NOT_EXIST       = 3
     OTHERS                     = 4.
  IF SY-SUBRC <> 0.
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
FORM DSP_LOG.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " dsp_log
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM MASK_COLUMNS TABLES   P_IT_FIELDCAT STRUCTURE IT_FIELDCAT.
* Build the fieldcat according to DDIC structure ztmm_6026_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ztmm_6026_01'
       CHANGING
            CT_FIELDCAT      = P_IT_FIELDCAT[].

* Make Column header
  LOOP AT P_IT_FIELDCAT.
    IF P_IT_FIELDCAT-FIELDNAME = 'ZDOCNO'.
      P_IT_FIELDCAT-COLTEXT = 'App.DocNo.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'LOGNO_H'.
      P_IT_FIELDCAT-COLTEXT = 'Log No.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSDAT'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSTIM'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PARTNERID'.
      P_IT_FIELDCAT-COLTEXT = 'PartnerID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'EFFDATE'.
      P_IT_FIELDCAT-COLTEXT = 'Effective Date'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'TXNCODE'.
      P_IT_FIELDCAT-COLTEXT = 'TxnCode'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'TXNDATE'.
      P_IT_FIELDCAT-COLTEXT = 'TxnDate'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ORDERNUMRECEIPT'.
      P_IT_FIELDCAT-COLTEXT = 'OrderNumReceipt'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ORDERNUMWORK'.
      P_IT_FIELDCAT-COLTEXT = 'OrderNumWork'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ORDERNUMSHIP'.
      P_IT_FIELDCAT-COLTEXT = 'OrderNumShip'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MATNR'.
      P_IT_FIELDCAT-OUTPUTLEN = 18.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PTC'.
      P_IT_FIELDCAT-COLTEXT = 'ProductTypeCode'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PTCSRC'.
      P_IT_FIELDCAT-COLTEXT = 'ProductTypeCodeSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MAKTXSRC'.
      P_IT_FIELDCAT-COLTEXT = 'ProductDescSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'NAFTACERTIFIED'.
      P_IT_FIELDCAT-COLTEXT = 'NaftaCertified'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'NAFTACERTIFIEDSC'.
      P_IT_FIELDCAT-COLTEXT = 'NaftaCertifiedSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MEINSSRC'.
      P_IT_FIELDCAT-COLTEXT = 'UOMSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'NTGEWSRC'.
      P_IT_FIELDCAT-COLTEXT = 'NetWeightSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'GEWEISRC'.
      P_IT_FIELDCAT-COLTEXT = 'WeightUnitSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'COUNTRYSHIPTO'.
      P_IT_FIELDCAT-COLTEXT = 'CountryShipTo'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'TRANSPORTID'.
      P_IT_FIELDCAT-COLTEXT = 'TransportID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RECEIPTDOCID'.
      P_IT_FIELDCAT-COLTEXT = 'ReceiptDocID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'EXITDOCID'.
      P_IT_FIELDCAT-COLTEXT = 'ExitDocID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADJRECEIPTDOCID'.
      P_IT_FIELDCAT-COLTEXT = 'AdjReceiptDocID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADJPRODUCTNUM'.
      P_IT_FIELDCAT-COLTEXT = 'AdjProductNum'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'FROMZONEID'.
      P_IT_FIELDCAT-COLTEXT = 'FromZoneID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'TOZONEID'.
      P_IT_FIELDCAT-COLTEXT = 'ToZoneID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MODEOFTRANSPORT'.
      P_IT_FIELDCAT-COLTEXT = 'ModeOfTransport'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RECEIPTDATE'.
      P_IT_FIELDCAT-COLTEXT = 'ReceiptDate'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ITNUM'.
      P_IT_FIELDCAT-COLTEXT = 'FullITNum'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'BILLOFLADING'.
      P_IT_FIELDCAT-COLTEXT = 'BillOfLading'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'EXPORTDATE'.
      P_IT_FIELDCAT-COLTEXT = 'ExportDate'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'VALIDFLAG'.
      P_IT_FIELDCAT-COLTEXT = 'ValidFlag'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ASSIGNMENTFLAG'.
      P_IT_FIELDCAT-COLTEXT = 'AssignmentFlag'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'FIFOFLAG'.
      P_IT_FIELDCAT-COLTEXT = 'FifoFlag'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'DELETEDFLAG'.
      P_IT_FIELDCAT-COLTEXT = 'DeletedFlag'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'KEEPDURINGROLLBA'.
      P_IT_FIELDCAT-COLTEXT = 'KeepDuringRollBack'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'STAWNSRC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSCodeSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'STATUSCODE'.
      P_IT_FIELDCAT-COLTEXT = 'StatusCode'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'STATUSCODESRC'.
      P_IT_FIELDCAT-COLTEXT = 'StatusCodeSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPICODE1'.
      P_IT_FIELDCAT-COLTEXT = 'SpiCode1'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPICODE1SRC'.
      P_IT_FIELDCAT-COLTEXT = 'SpiCode1Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPICODE2'.
      P_IT_FIELDCAT-COLTEXT = 'SpiCode2'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPICODE2SRC'.
      P_IT_FIELDCAT-COLTEXT = 'SpiCode2Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'LAND1SRC'.
      P_IT_FIELDCAT-COLTEXT = 'CountrySource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'LIFNRSRC'.
      P_IT_FIELDCAT-COLTEXT = 'VendorSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RELFLAG'.
      P_IT_FIELDCAT-COLTEXT = 'RelationshipFlag'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RELFLAGSRC'.
      P_IT_FIELDCAT-COLTEXT = 'RelationshipFlagSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSINDEX'.
      P_IT_FIELDCAT-COLTEXT = 'HTSIndex'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSINDEXSRC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSIndexSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSDESC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSDesc'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSDESCSRC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSDescSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSNUM2'.
      P_IT_FIELDCAT-COLTEXT = 'HTSNum2'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSNUM2SRC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSNum2Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'VALUESRC'.
      P_IT_FIELDCAT-COLTEXT = 'ValueSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'VALUE2'.
      P_IT_FIELDCAT-COLTEXT = 'Value2'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'VALUE2SRC'.
      P_IT_FIELDCAT-COLTEXT = 'Value2Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'WAERSSRC'.
      P_IT_FIELDCAT-COLTEXT = 'CurrencySource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'NETPRUOM'. "
      P_IT_FIELDCAT-COLTEXT = 'NetPr UOM'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'EFFPRUOM'. "
      P_IT_FIELDCAT-COLTEXT = 'EffPr UOM'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ALTVALUESRC'.
      P_IT_FIELDCAT-COLTEXT = 'AltValueSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ALTVALUE2SRC'.
      P_IT_FIELDCAT-COLTEXT = 'AltValue2Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ALTCURRCODE'.
      P_IT_FIELDCAT-COLTEXT = 'AltCurrCode'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ALTCURRCODESRC'.
      P_IT_FIELDCAT-COLTEXT = 'AltCurrCodeSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADVALOREMRATESRC'.
      P_IT_FIELDCAT-COLTEXT = 'AdValoremRateSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPECIFICRATESRC'.
      P_IT_FIELDCAT-COLTEXT = 'SpecificRateSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'UOMCONVFACTORSRC'.
      P_IT_FIELDCAT-COLTEXT = 'UomConvFactorSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADDUOMCONVFACSRC'.
      P_IT_FIELDCAT-COLTEXT = 'AddUomConvFactorSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RPTQTYUOM'.
      P_IT_FIELDCAT-COLTEXT = 'RptQtyUOM'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RPTQTYUOMSRC'.
      P_IT_FIELDCAT-COLTEXT = 'RptQtyUomSrc'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADDRPTQTYUOM'.
      P_IT_FIELDCAT-COLTEXT = 'AddRptQtyUOM'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADDRPTQTYUOMSRC'.
      P_IT_FIELDCAT-COLTEXT = 'AddRptQtyUomSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'DOTINDICATOR'.
      P_IT_FIELDCAT-COLTEXT = 'DotIndicator'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'FCCINDICATOR'.
      P_IT_FIELDCAT-COLTEXT = 'FccIndicator'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'FDAINDICATOR'.
      P_IT_FIELDCAT-COLTEXT = 'FdaIndicator'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'INFNR'. "purchasing info record
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PROFL'. "MIP/LP/KD
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MTART'. "Material type
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'BWART'. "Movement type
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MAT_KDAUF'.
      "Sales order number of valuated sales order stock
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MAT_KDPOS'.
      "Sales Order Item of Valuated Sales Order Stock
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'COLOREXT'.
      P_IT_FIELDCAT-COLTEXT = 'External Color'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'COLORINT'.
      P_IT_FIELDCAT-COLTEXT = 'Internal Color'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PLNUM'.
      P_IT_FIELDCAT-COLTEXT = 'Planned order number'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'DATE_TIME'.
      P_IT_FIELDCAT-COLTEXT = 'Date_Time'.


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
    MODIFY P_IT_FIELDCAT.
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
FORM GET_NETPR_WAERS USING    VALUE(IM_MATNR)
                     CHANGING VALUE(EX_NETPR)
                              VALUE(EX_WAERS).
  CLEAR: EX_NETPR, EX_WAERS.
  SELECT SINGLE EINE~NETPR EINE~WAERS
    INTO (EX_NETPR, EX_WAERS)
    FROM EINA
      INNER JOIN EINE
      ON EINE~INFNR = EINA~INFNR   "Info Record Number
  WHERE EINA~MATNR = IM_MATNR.
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
FORM GET_NETPR_EFFPR_WAERS USING    VALUE(IM_MATNR)
                     CHANGING VALUE(EX_LIFNR)
                              VALUE(EX_NETPR)
                              VALUE(EX_EFFPR)
                              VALUE(EX_WAERS)
                              VALUE(EX_BPUMZ)
                              VALUE(EX_BPUMN)
                              VALUE(EX_PEINH). "Price Unit
  CLEAR: EX_LIFNR, EX_NETPR, EX_EFFPR, EX_WAERS, EX_BPUMZ, EX_BPUMN,
         EX_PEINH.
  SELECT SINGLE EINA~LIFNR EINE~NETPR EINE~EFFPR EINE~WAERS
                EINE~BPUMZ EINE~BPUMN EINE~PEINH
    INTO (EX_LIFNR, EX_NETPR, EX_EFFPR, EX_WAERS,
          EX_BPUMZ, EX_BPUMN, EX_PEINH)
    FROM EINA
      INNER JOIN EINE
      ON EINE~INFNR = EINA~INFNR   "Infor Record Number
  WHERE EINA~MATNR = IM_MATNR.

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
FORM GET_VENDORINFO USING    VALUE(IM_LIFNR)
                    CHANGING EX_LAND1.
  SELECT SINGLE
         LFA1~LAND1    "Country
    INTO (EX_LAND1)
    FROM LFA1
      INNER JOIN ADRC
        ON ADRC~ADDRNUMBER = LFA1~ADRNR
    WHERE LFA1~LIFNR = IM_LIFNR.
ENDFORM.                " get_vendorinfo
*&---------------------------------------------------------------------*
*&      Form  process_data_by_section
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA_BY_SECTION.
*/ App. Doc. No.
  PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09
                                   'ZMMNRO0002'
                          CHANGING W_ZDOCNO.
  COMMIT WORK.

*/Define Local Variable
  DATA: LV_LINES     TYPE I.  "No. of itab lines
  DATA: LV_QUOTIENT  TYPE I.
  DATA: LV_REMAINDER TYPE I.
  DATA: LT_ZTMM_6026_01 LIKE IT_ZTMM_6026_01.
  "For Sectional Use of it_ztmm_6026_01
  DATA: LC_SECTIONSIZE TYPE I VALUE 1000. "Section Size
  DATA: LV_DO_IDX TYPE I.  "Index for Do Loop
  DATA: LV_FRLINE TYPE I.  "From Line
  DATA: LV_TOLINE TYPE I.  "To Line

*/Get Line no. of it_data_for_to
  DESCRIBE TABLE IT_ZTMM_6026_01 LINES LV_LINES.
  "I estimate lv_lines = 400,000.
  LV_QUOTIENT  = LV_LINES DIV LC_SECTIONSIZE.
  LV_REMAINDER = LV_LINES MOD LC_SECTIONSIZE.

*/Process Data by Section
  DO LV_QUOTIENT TIMES.
    LV_DO_IDX = LV_DO_IDX + 1.
    LV_FRLINE  = ( LV_DO_IDX - 1 ) * LC_SECTIONSIZE + 1.
    LV_TOLINE  = LV_DO_IDX         * LC_SECTIONSIZE.
    CLEAR: LT_ZTMM_6026_01.
    APPEND LINES OF IT_ZTMM_6026_01
      FROM LV_FRLINE TO LV_TOLINE TO LT_ZTMM_6026_01.
    PERFORM PROCESS_DATA               "Process Data
                USING W_ZDOCNO
                      LT_ZTMM_6026_01.

    APPEND LINES OF LT_ZTMM_6026_01
                     TO IT_ZTMM_6026_01_TMP.
  ENDDO.
  IF NOT LV_REMAINDER IS INITIAL.
    LV_FRLINE  = LV_QUOTIENT * LC_SECTIONSIZE + 1.
    LV_TOLINE  = LV_LINES.
    "(=lv_quotient * lc_sectionsize + lv_remainder)
    CLEAR: LT_ZTMM_6026_01.
    APPEND LINES OF IT_ZTMM_6026_01
      FROM LV_FRLINE TO LV_TOLINE TO LT_ZTMM_6026_01.
    PERFORM PROCESS_DATA               "Process Data
                USING W_ZDOCNO
                      LT_ZTMM_6026_01.
    APPEND LINES OF LT_ZTMM_6026_01
                     TO IT_ZTMM_6026_01_TMP.
  ENDIF.

  IT_ZTMM_6026_01 = IT_ZTMM_6026_01_TMP.

ENDFORM.                    " process_data_by_section
*&---------------------------------------------------------------------*
*&      Form  get_hscode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_ZTMM_6019_01>_MATNR  text
*      <--P_<FS_ZTMM_6019_01>_STAWN  text
*----------------------------------------------------------------------*
FORM GET_HSCODE
        USING    VALUE(IM_MATNR)
        CHANGING VALUE(EX_STAWN) LIKE MARC-STAWN.
  CLEAR: EX_STAWN.
  SELECT SINGLE STAWN
    INTO EX_STAWN
    FROM MARC
    WHERE MATNR = IM_MATNR AND
          WERKS = 'P001'.
  IF SY-SUBRC <> 0.
    SELECT SINGLE STAWN
      INTO EX_STAWN
      FROM MARC
      WHERE MATNR = IM_MATNR.
  ENDIF.
  IF EX_STAWN IS INITIAL. EX_STAWN = '0000.00.0000'. ENDIF.
ENDFORM.                    " get_hscode
*&---------------------------------------------------------------------*
*&      Form  getstatuscode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM getstatuscode.
*  DATA: lv_kbetr LIKE konp-kbetr.
*  "Rate (condition amount or percentage) where no scale exists
*  DATA: lv_konwa LIKE konp-konwa.
*  "Rate unit (currency or percentage)
*
*  read table t_impreq with key ebeln = <fs_ztmm_6026_01>-ebeln
*                                       binary search
*                                       transporting no fields.
*  if sy-subrc eq 0.
*    <fs_ztmm_6026_01>-statuscode    = ''.
*    <fs_ztmm_6026_01>-statuscodesrc = 'I'.
*  else.
*    if <fs_ztmm_6026_01>-profl eq 'M'.
*      <fs_ztmm_6026_01>-statuscode    = 'F'.
*      <fs_ztmm_6026_01>-statuscodesrc = ''.
*    else.
*      <fs_ztmm_6026_01>-statuscode    = 'D'.
*      <fs_ztmm_6026_01>-statuscodesrc = ''.
*    endif.
*  endif.
*
**  CHECK NOT <fs_ztmm_6026_01>-stawn IS INITIAL.
*
***/ Get HS code Rate  (You can view the data from /nMEK3)
***1.
**  DATA: lv_knumh LIKE konp-knumh.
**  SELECT SINGLE knumh INTO lv_knumh
**    FROM a902
**    WHERE kappl = 'M'    AND
**          kschl = 'ZOA1' AND
**          stawn = <fs_ztmm_6026_01>-stawn.
**
***2.
**  SELECT SINGLE kbetr konwa
**    INTO (lv_kbetr, lv_konwa)
**    FROM konp
**    WHERE knumh = lv_knumh.
**  lv_kbetr = lv_kbetr / 10.
**
**
***/ Make StatusCode
**  IF <fs_ztmm_6026_01>-profl = 'K'.  "KD Material
**    IF lv_kbetr =< '2.5'.
**      <fs_ztmm_6026_01>-statuscode = 'P'.
**    ELSE.
**      <fs_ztmm_6026_01>-statuscode = 'N'.
**    ENDIF.
**  ELSEIF <fs_ztmm_6026_01>-profl = 'V'.  "Local Part
**    <fs_ztmm_6026_01>-statuscode = 'D'.
**  ELSEIF <fs_ztmm_6026_01>-profl = 'M'.   "MIP Mat
**    <fs_ztmm_6026_01>-statuscode = 'F'.
**  ENDIF.
**
***/ StatusCode Refine
**  IF <fs_ztmm_6026_01>-land1 = 'US' OR  "USA
**     <fs_ztmm_6026_01>-land1 = 'CA' OR  "CANADA
**     <fs_ztmm_6026_01>-land1 = 'MX'.    "MEXICO
**    <fs_ztmm_6026_01>-statuscode = 'D'. "Domestic
**  ENDIF.
*ENDFORM.                    " getstatuscode
*&---------------------------------------------------------------------*
*&      Form  itable_group_by_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM ITABLE_GROUP_BY_MATNR
      USING IET_ITABLE LIKE IT_ZTMM_6026_01.
  DATA: LV_SERNO TYPE I.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <LF_SUMMARY> LIKE LINE OF IET_ITABLE.
  DATA: LT_SUMMARY LIKE TABLE OF <LF_SUMMARY>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <LF_ITABLE> LIKE LINE OF IET_ITABLE.

*/1.Make unique line by MATNR
  LT_SUMMARY = IET_ITABLE.
  SORT LT_SUMMARY BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_SUMMARY
                   COMPARING MATNR.

*/2.Grouping by MATNR
  SORT LT_SUMMARY BY MATNR.
  LOOP AT LT_SUMMARY ASSIGNING <LF_SUMMARY>.
    CLEAR: LV_SERNO, <LF_SUMMARY>-MENGE.
    LOOP AT IET_ITABLE ASSIGNING <LF_ITABLE>
                  WHERE MATNR = <LF_SUMMARY>-MATNR.
      LV_SERNO = LV_SERNO + 1.  "Occurrence by MATNR
      <LF_SUMMARY>-MENGE = <LF_SUMMARY>-MENGE + <LF_ITABLE>-MENGE.
    ENDLOOP.
  ENDLOOP.
  IET_ITABLE = LT_SUMMARY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  itable_group_by_onw_matnr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ZTMM_6026_01  text
*----------------------------------------------------------------------*
FORM ITABLE_GROUP_BY_ONW_MATNR
      USING IET_ITABLE LIKE IT_ZTMM_6026_01.
*/For Summary WA & Itab
  FIELD-SYMBOLS: <LF_SUMMARY> LIKE LINE OF IET_ITABLE.
  DATA: LT_SUMMARY LIKE TABLE OF <LF_SUMMARY>.
*/Field Symbol for iet_itable
  FIELD-SYMBOLS: <LF_ITABLE> LIKE LINE OF IET_ITABLE.

*/1.Make unique line by ORDERNUMWORK, MATNR
  LT_SUMMARY = IET_ITABLE.
  SORT LT_SUMMARY BY ORDERNUMWORK MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_SUMMARY
                   COMPARING ORDERNUMWORK MATNR.

*/2.Grouping by ORDERNUMWORK, MATNR
  SORT LT_SUMMARY BY ORDERNUMWORK MATNR.
  LOOP AT LT_SUMMARY ASSIGNING <LF_SUMMARY>.
*    CLEAR: <lf_summary>-matnr_parent_cnt, <lf_summary>-menge.
    CLEAR: <LF_SUMMARY>-MENGE.
    LOOP AT IET_ITABLE ASSIGNING <LF_ITABLE>
           WHERE ORDERNUMWORK   = <LF_SUMMARY>-ORDERNUMWORK AND
                 MATNR          = <LF_SUMMARY>-MATNR.
*     <lf_summary>-matnr_parent_cnt = <lf_summary>-matnr_parent_cnt + 1.
      "Occurrence by ORDERNUMWORK, MATNR
      <LF_SUMMARY>-MENGE = <LF_SUMMARY>-MENGE + <LF_ITABLE>-MENGE.
    ENDLOOP.
  ENDLOOP.
  IET_ITABLE = LT_SUMMARY.
ENDFORM.
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
FORM GET_INFORECORD_DATA
                     USING    VALUE(IM_MATNR)
                              VALUE(IM_LIFNR)
                     CHANGING VALUE(EX_LAND1)
                              VALUE(EX_NETPR)
                              VALUE(EX_EFFPR)
                              VALUE(EX_WAERS)
                              VALUE(EX_BPUMZ)
                              VALUE(EX_BPUMN)
                              VALUE(EX_PEINH).
  CLEAR: EX_LAND1, EX_NETPR, EX_EFFPR, EX_WAERS,
         EX_BPUMZ, EX_BPUMN, EX_PEINH.

  DATA: W_DATLB LIKE EINE-DATLB,
        W_LIFNR LIKE A018-LIFNR,
        W_KNUMH LIKE A018-KNUMH,
        W_KBETR LIKE KONP-KBETR.

  IF IM_LIFNR IS INITIAL.
    "This case happens when Vendor is not related to Material Document.
    SELECT SINGLE  LFA1~LAND1
                   EINE~DATLB
                   EINE~EFFPR
                   EINE~WAERS
                   EINE~BPUMZ
                   EINE~BPUMN
                   EINE~PEINH
                   EINA~LIFNR
      INTO (EX_LAND1, W_DATLB,
            EX_EFFPR, EX_WAERS,
            EX_BPUMZ, EX_BPUMN,
            EX_PEINH, W_LIFNR)
      FROM EINA
        INNER JOIN LFA1
        ON LFA1~LIFNR = EINA~LIFNR "Vendor
        INNER JOIN EINE
        ON EINE~INFNR = EINA~INFNR   "Info Record Number
    WHERE EINA~MATNR = IM_MATNR. " AND
    " eina~lifnr = im_lifnr.
  ELSE.
    W_LIFNR = IM_LIFNR.
    SELECT SINGLE  LFA1~LAND1
                   EINE~DATLB
                   EINE~EFFPR
                   EINE~WAERS
                   EINE~BPUMZ
                   EINE~BPUMN
                   EINE~PEINH
      INTO (EX_LAND1, W_DATLB,
            EX_EFFPR, EX_WAERS,
            EX_BPUMZ, EX_BPUMN,
            EX_PEINH)
      FROM EINA
        INNER JOIN LFA1
        ON LFA1~LIFNR = EINA~LIFNR "Vendor
        INNER JOIN EINE
        ON EINE~INFNR = EINA~INFNR   "Info Record Number
    WHERE EINA~MATNR = IM_MATNR AND
          EINA~LIFNR = IM_LIFNR.
  ENDIF.
  SELECT SINGLE KNUMH FROM A018
                      INTO W_KNUMH
                      WHERE KAPPL = 'M'
                      AND   KSCHL = 'PB00'
                      AND   MATNR = IM_MATNR
                      AND   LIFNR = W_LIFNR
                      AND   DATAB <= W_DATLB
                      AND   DATBI >= W_DATLB.
  IF SY-SUBRC NE 0.
    EX_NETPR = 0.
    EXIT.
  ENDIF.
  SELECT SINGLE KBETR INTO W_KBETR
                      FROM KONP
                      WHERE KAPPL = 'M'
                      AND   KSCHL = 'PB00'
                      AND   KNUMH = W_KNUMH.

  IF SY-SUBRC EQ 0.
    EX_NETPR = W_KBETR.
  ELSE.
    EX_NETPR = 0.
  ENDIF.
ENDFORM.
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
FORM GET_STANDARDPRICE
          USING    VALUE(IM_MATNR) LIKE MBEW-MATNR  "Material
                   VALUE(IM_BWKEY) LIKE MBEW-BWKEY  "Valuation area
          CHANGING VALUE(EX_STPRS) LIKE MBEW-STPRS  "Standard Price
                   VALUE(EX_WAERS) LIKE T001-WAERS. "Currency Key
  CLEAR: EX_STPRS, EX_WAERS.
  IF IM_BWKEY IS INITIAL.
    SELECT SINGLE WERKS
      INTO IM_BWKEY
      FROM MARC
      WHERE MATNR = IM_MATNR.
  ENDIF.

  SELECT SINGLE STPRS  "Standard Price
    INTO EX_STPRS
    FROM MBEW    "Material Valuation
    WHERE MATNR = IM_MATNR AND  "Material
          BWKEY = IM_BWKEY.     "Valuation area

  SELECT SINGLE WAERS  "Currency Key
    INTO EX_WAERS
    FROM T001    "Company Codes
    WHERE BUKRS = 'H201'.    "Company Code
ENDFORM.                    "get_standardprice
*&---------------------------------------------------------------------*
*&      Form  ordernumreceipt_by_profl
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORDERNUMRECEIPT_BY_PROFL USING    P_BWART
                                       P_PROFL
                                       P_GRGI
                                       P_ZTCODE
                              CHANGING P_MATERIAL
                                       P_ORDERNUMRECEIPT
                                       P_TRANSID
                                       P_BOL.
  DATA: W_COLOR(3) TYPE C.

  PERFORM SEPARATE_COLOR USING <FS_MSEG>-MATNR <FS_MSEG>-MTART
                         CHANGING P_MATERIAL W_COLOR.
  CASE P_PROFL.
    WHEN 'K'.   "KD
      IF P_BWART EQ '101'.
       READ TABLE IT_ZTBL INTO WA_ZTBL WITH KEY EBELN = <FS_MSEG>-EBELN
                                                EBELP = <FS_MSEG>-EBELP
                                                XBLNR = <FS_MSEG>-XBLNR.
        IF SY-SUBRC NE 0.
          P_ORDERNUMRECEIPT = <FS_MSEG>-EBELN.
          P_TRANSID         = <FS_MSEG>-EBELN.
          P_BOL             = SPACE.
        ELSE.
          P_ORDERNUMRECEIPT = <FS_MSEG>-EBELN.
          P_TRANSID         = WA_ZTBL-ZFHBLNO.
          P_BOL             = WA_ZTBL-ZFHBLNO.
*          exit.
        ENDIF.
      ELSE.
        IF P_ZTCODE = 'XNPC' OR P_ZTCODE = 'XPPC' OR
           P_ZTCODE = 'ANPC' OR P_ZTCODE = 'APPC'.
          IF ( <FS_MSEG>-MATNR NE <FS_MSEG>-PMATNR ) AND
               ( NOT <FS_MSEG>-PMATNR IS INITIAL ).
            CONCATENATE <FS_MSEG>-PMATNR P_DATE INTO P_ORDERNUMRECEIPT.
          ELSE.
            CONCATENATE P_GRGI 'KD' P_DATE INTO P_ORDERNUMRECEIPT.
          ENDIF.
        ELSE.
          CONCATENATE P_GRGI 'KD' P_DATE INTO P_ORDERNUMRECEIPT.
        ENDIF.
        P_TRANSID = P_ORDERNUMRECEIPT.
        P_BOL = SPACE.
      ENDIF.
    WHEN 'V'.   "LP
      IF P_ZTCODE = 'XNPC' OR P_ZTCODE = 'XPPC' OR
         P_ZTCODE = 'ANPC' OR P_ZTCODE = 'APPC'.
        IF ( <FS_MSEG>-MATNR NE <FS_MSEG>-PMATNR ) AND
              ( NOT <FS_MSEG>-PMATNR IS INITIAL ).
          CONCATENATE <FS_MSEG>-PMATNR P_DATE INTO P_ORDERNUMRECEIPT.
        ELSE.
          CONCATENATE P_GRGI 'LP' P_DATE INTO P_ORDERNUMRECEIPT.
        ENDIF.
      ELSE.
        CONCATENATE P_GRGI 'LP' P_DATE INTO P_ORDERNUMRECEIPT.
      ENDIF.
      P_TRANSID = P_ORDERNUMRECEIPT.
      P_BOL = SPACE.
    WHEN 'M'. "MIP
      IF P_BWART EQ '601'.
      ELSE.
*&--------Add FSC code for tracking purpose.
        IF P_ZTCODE = 'XNPC' OR P_ZTCODE = 'XPPC' OR
           P_ZTCODE = 'ANPC' OR P_ZTCODE = 'APPC'.
          CONCATENATE <FS_MSEG>-PMATNR P_DATE INTO P_ORDERNUMRECEIPT.
        ENDIF.
      ENDIF.
      P_TRANSID = P_ORDERNUMRECEIPT.
      P_BOL = SPACE.
      EXIT.
  ENDCASE.
  IF NOT W_COLOR IS INITIAL.
    CONCATENATE P_ORDERNUMRECEIPT W_COLOR INTO P_ORDERNUMRECEIPT.
    IF P_BOL IS INITIAL.
      P_TRANSID = P_ORDERNUMRECEIPT.
    ENDIF.
  ENDIF.
ENDFORM.                    " ordernumreceipt_by_profl
*&---------------------------------------------------------------------*
*&      Form  get_salepart_ccode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SALEPART_CCODE USING P_TYP.
*&--------------------------------------------------------------------&*
* Material number might start with "Z" for some adjustment reason so
* need to disregard the first character.
*&--------------------------------------------------------------------&*
  DATA: BEGIN OF WA_DEL_ORD,
          VBELN LIKE LIKP-VBELN,
          VGBEL LIKE LIPS-VGBEL,
         END OF WA_DEL_ORD.
  DATA: IT_DEL_ORD LIKE TABLE OF WA_DEL_ORD.

  FIELD-SYMBOLS: <FS_DLORD> LIKE LINE OF IT_DEL_ORD.

  CASE P_TYP.
    WHEN 'R'.
      CLEAR: WA_MSEG.
      SELECT VBELN VGBEL FROM LIPS
                         INTO TABLE IT_DEL_ORD
                         FOR ALL ENTRIES IN IT_MSEG
                      WHERE ( VBELN = IT_MSEG-XBLNR AND BWART = '601' )
                      OR    ( VBELN = IT_MSEG-XBLNR AND BWART = '602' )
                      OR    ( VBELN = IT_MSEG-XBLNR AND BWART = '991' ).

      LOOP AT IT_DEL_ORD ASSIGNING <FS_DLORD>.
*&---update it_mseg.
        WA_MSEG-KDAUF =  <FS_DLORD>-VGBEL.
        MODIFY IT_MSEG FROM WA_MSEG
                       TRANSPORTING KDAUF
                       WHERE XBLNR = <FS_DLORD>-VBELN
                       AND   BWART EQ '601' OR BWART EQ '602'.

        WA_VBAK-VBELN = <FS_DLORD>-VGBEL.
        COLLECT WA_VBAK INTO IT_VBAK.
        CLEAR: WA_MSEG, WA_VBAK.
      ENDLOOP.
  ENDCASE.
  DESCRIBE TABLE IT_VBAK LINES W_LINES.
  IF W_LINES GT 0.
    SELECT VBELN LAND1 INTO TABLE IT_VBPA
                       FROM VBPA
                       FOR ALL ENTRIES IN IT_VBAK
                       WHERE VBELN = IT_VBAK-VBELN
                       AND   PARVW = 'WE'.
    SORT IT_VBPA BY VBELN.
  ENDIF.
  CLEAR: W_LINES.
  FREE IT_VBAK.
ENDFORM.                    " get_salepart_ccode
*&---------------------------------------------------------------------*
*&      Form  separate_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MATNR  text
*      -->P_MTART  text
*      <--P_P_MATERIAL  text
*      <--P_COLOR  text
*----------------------------------------------------------------------*
FORM SEPARATE_COLOR USING    P_MATNR
                             P_MTART
                    CHANGING P_P_MATERIAL
                            P_COLOR.
  DATA: W_MLEN TYPE I,
        W_NCHAR TYPE I.

  W_MLEN = STRLEN( P_MATNR ).


*    IF ( w_mlen GT 11 AND p_mtart = 'ROH' AND
*                          p_matnr(1) <> 'B' ).
*      w_nchar = w_mlen - 10.
*      p_color = p_matnr+10(w_nchar).
*      p_p_material = p_matnr(10).
*    ELSE.
*      p_p_material = p_matnr.
*    ENDIF.
** Changed by Furong for EBOM
  IF P_MATNR(1) NE 'Z'.
    IF P_MATNR+0(2) = 'CR' OR P_MATNR+0(2) = 'EM'.
      IF ( W_MLEN GT 13 AND P_MTART = 'ROH' AND
                            P_MATNR(1) <> 'B' ).
        W_NCHAR = W_MLEN - 12.
        P_COLOR = P_MATNR+12(W_NCHAR).
        P_P_MATERIAL = P_MATNR(12).
      ELSE.
        IF ( W_MLEN GT 11 AND P_MTART = 'ROH' AND
                             P_MATNR(1) <> 'B' ).
          IF  P_MATNR+5(2) <> 'M1'.
            W_NCHAR = W_MLEN - 10.
            P_COLOR = P_MATNR+10(W_NCHAR).
            P_P_MATERIAL = P_MATNR(10).
          ELSE.
            P_P_MATERIAL = P_MATNR.
          ENDIF.
        ELSE.
          P_P_MATERIAL = P_MATNR.
        ENDIF.
      ENDIF.
    ELSE.
      IF ( W_MLEN GT 11 AND P_MTART = 'ROH' AND
                               P_MATNR(1) <> 'B' ).
        W_NCHAR = W_MLEN - 10.
        P_COLOR = P_MATNR+10(W_NCHAR).
        P_P_MATERIAL = P_MATNR(10).
      ELSE.
        P_P_MATERIAL = P_MATNR.
      ENDIF.
    ENDIF.
  ELSE.
    IF ( W_MLEN GT 12 AND P_MTART = 'ROH' AND
                          P_MATNR(2) <> 'B' ).
      W_NCHAR = W_MLEN - 11.
      P_COLOR = P_MATNR+11(W_NCHAR).
      P_P_MATERIAL = P_MATNR(11).
    ELSE.
      P_P_MATERIAL = P_MATNR.
    ENDIF.
  ENDIF.

ENDFORM.                    " separate_color
*&---------------------------------------------------------------------*
*&      Form  adjust_anpc_appc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADJUST_ANPC_APPC.

  DATA: BEGIN OF WA_APPC_ANPC,
          MATNR LIKE MARA-MATNR,
          TXNCODE(4) TYPE C,
          MENGE LIKE MSEG-MENGE,
        END OF WA_APPC_ANPC.
  DATA: W_PREV_MATNR LIKE MARA-MATNR,
        W_ADJ_CNT TYPE I,
        W_CUR_CNT TYPE I,
        W_ADJ_NETQTY LIKE MSEG-MENGE,
        W_ADJ_TXNCODE(4) TYPE C.

  DATA: W_ADJ_LINES TYPE I.

  DATA: IT_APPC_ANPC LIKE TABLE OF WA_APPC_ANPC.

  FIELD-SYMBOLS: <FS_APNPC> LIKE LINE OF IT_APPC_ANPC,
                 <FS_ADJUST> LIKE LINE OF IT_ADJUST.


  REFRESH: IT_ZTMM_6026_01_TMP.

  LOOP AT IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
                          WHERE TXNCODE = 'ANPC' OR TXNCODE = 'APPC'.
    WA_APPC_ANPC-MATNR   = <FS_ZTMM_6026_01>-MATNR.
    WA_APPC_ANPC-TXNCODE = <FS_ZTMM_6026_01>-TXNCODE.
    WA_APPC_ANPC-MENGE   = <FS_ZTMM_6026_01>-MENGE.
    APPEND WA_APPC_ANPC TO IT_APPC_ANPC.
    W_ADJ_CNT = W_ADJ_CNT + 1.
  ENDLOOP.

  SORT IT_APPC_ANPC BY MATNR.

  LOOP AT IT_APPC_ANPC ASSIGNING <FS_APNPC>.
    CASE <FS_APNPC>-TXNCODE.
      WHEN 'APPC'.
        W_APPC_QTY = W_APPC_QTY + <FS_APNPC>-MENGE.
      WHEN 'ANPC'.
        W_ANPC_QTY = W_ANPC_QTY + <FS_APNPC>-MENGE.
    ENDCASE.
*Paul 09/27/11
*{    AT END OF <FS_APNPC>-MATNR.
    AT END OF MATNR.
*}
      IF W_APPC_QTY > W_ANPC_QTY.
        W_ADJ_NETQTY = ( W_APPC_QTY ) - ( W_ANPC_QTY ).
        W_ADJ_TXCODE = 'APPC'.
      ELSE.
        W_ADJ_NETQTY = ( W_ANPC_QTY ) - ( W_APPC_QTY ).
        W_ADJ_TXCODE = 'ANPC'.
      ENDIF.
      WA_ADJUST-MATNR   = <FS_APNPC>-MATNR.
      WA_ADJUST-TXNCODE = W_ADJ_TXCODE.
      WA_ADJUST-MENGE   = W_ADJ_NETQTY.
      APPEND WA_ADJUST TO IT_ADJUST.
      CLEAR: W_APPC_QTY, W_ANPC_QTY, W_ADJ_NETQTY, W_ADJ_TXCODE,
             WA_ADJUST.
    ENDAT.
  ENDLOOP.

  CLEAR: WA_ZTMM_6026_01.
  DELETE IT_ADJUST WHERE MENGE IS INITIAL.

  LOOP AT IT_ADJUST ASSIGNING <FS_ADJUST>.
*&------Since OrderNumship exists only for 'ANPC'.
    READ TABLE IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
                               WITH KEY MATNR = <FS_ADJUST>-MATNR
                                        TXNCODE = <FS_ADJUST>-TXNCODE.
    IF SY-SUBRC NE 0.
    ELSE.
      WA_ZTMM_6026_01 = <FS_ZTMM_6026_01>.
      WA_ZTMM_6026_01-TXNCODE = <FS_ADJUST>-TXNCODE.
      WA_ZTMM_6026_01-MENGE   = <FS_ADJUST>-MENGE.
      APPEND WA_ZTMM_6026_01 TO IT_ZTMM_6026_01_TMP.
    ENDIF.
    CLEAR: WA_ZTMM_6026_01.
  ENDLOOP.

  DELETE IT_ZTMM_6026_01 WHERE TXNCODE = 'ANPC' OR TXNCODE = 'APPC'.

  DESCRIBE TABLE IT_ZTMM_6026_01_TMP LINES W_ADJ_LINES.
  IF W_ADJ_LINES > 0.
    APPEND LINES OF IT_ZTMM_6026_01_TMP TO IT_ZTMM_6026_01.
  ENDIF.

  CLEAR: W_ADJ_LINES.
  FREE: IT_ZTMM_6026_01_TMP.

ENDFORM.                    " adjust_anpc_appc
*&---------------------------------------------------------------------*
*&      Form  bom_exp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->Parallel process result
*----------------------------------------------------------------------*
FORM BOM_EXP USING W_TASKNAME.

  DATA: WA_STPOX_TMP LIKE WA_STPOX1,
        WA_FSC_TMP LIKE STPOX.

  DATA: IT_STPOX_VAL LIKE TABLE OF STPOX,
        IT_STPOX_TMP LIKE TABLE OF WA_STPOX_TMP.
  DATA: W_MATNR LIKE MARA-MATNR,
   W_PMATNR LIKE MARA-MATNR,
        W_DATUV LIKE STKO-DATUV,
        W_BWART LIKE MSEG-BWART.
  DATA:  L_WRK02 LIKE T460A-WRK02.

  RECEIVE RESULTS FROM FUNCTION 'Z_FFTZ_EXP_BOM'
                  IMPORTING P_MATNR     = W_MATNR
                             P_DATUV1 = W_DATUV
                              P_PMATNR = W_PMATNR

                  TABLES    P_STPOX     = IT_STPOX_VAL
                  EXCEPTIONS
                         COMMUNICATION_FAILURE = 1
                         SYSTEM_FAILURE        = 2.
  IF SY-SUBRC NE 0.
    W_EXCEP_FLAG = 'X'.
    EXIT.
  ENDIF.
  W_RCV_JOBS = W_RCV_JOBS + 1.

  LOOP AT IT_STPOX_VAL INTO WA_FSC_TMP.
    IF WA_FSC_TMP-MTART = 'HALB'.
      IF WA_FSC_TMP-SOBSL = 40.
        SELECT SINGLE WRK02 INTO L_WRK02
         FROM T460A
         WHERE WERKS = WA_FSC_TMP-WERKS
           AND SOBSL = '40'.
*        if sy-subrc = 0.
        IT_HALB_ENG-PMATNR = W_MATNR.
        IT_HALB_ENG-MATNR = WA_FSC_TMP-IDNRK.
        IT_HALB_ENG-WERKS = L_WRK02.

*        it_halb_eng-datuv = SY-DATUM.
        IT_HALB_ENG-DATUV = W_DATUV.

        IT_HALB_ENG-PDATUV = W_DATUV.
        IT_HALB_ENG-BDMNG = WA_FSC_TMP-MNGKO.
        IT_HALB_ENG-STLAL = WA_FSC_TMP-STLAL.
        COLLECT IT_HALB_ENG.
      ENDIF.
    ENDIF.
  ENDLOOP.

  APPEND LINES OF IT_STPOX_VAL TO IT_STPOX_TMP.
  DELETE IT_STPOX_TMP WHERE MTART IN R_MTART
                      OR    DUMPS EQ 'X'.
*  wa_stpox_tmp-matnr = w_matnr.
  WA_STPOX_TMP-MATNR = W_PMATNR.
  IF W_MATNR = W_PMATNR.
    MODIFY IT_STPOX_TMP FROM WA_STPOX_TMP TRANSPORTING MATNR
                                        WHERE MATNR IS INITIAL.
  ELSE.
    WA_STPOX_TMP-WERKS = 'P001'.
    MODIFY IT_STPOX_TMP FROM WA_STPOX_TMP TRANSPORTING MATNR WERKS
                                         WHERE MATNR IS INITIAL.

  ENDIF.
*  wa_stpox_tmp-bwart = w_bwart.

  APPEND LINES OF IT_STPOX_TMP TO IT_STPOX1.
  REFRESH: IT_STPOX_VAL, IT_STPOX_TMP.
  CLEAR: W_MATNR, W_BWART, WA_STPOX_TMP.

ENDFORM.                    " bom_exp
*&---------------------------------------------------------------------*
*&      Form  summarize_txncode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMMARIZE_TXNCODE.

  DATA: BEGIN OF WA_CONS_DATA,
          MATNR   LIKE MSEG-MATNR,
          EBELN   LIKE MSEG-EBELN,
          ZFHBLNO LIKE ZTBL-ZFHBLNO,
          ORDNR   LIKE ZTMM_6026_01-ORDERNUMRECEIPT,
          ORDERNUMWORK LIKE  ZTMM_6026_01-ORDERNUMWORK,
          ADPRN   LIKE ZTMM_6026_01-ADJPRODUCTNUM,
          STAWN LIKE  ZTMM_6026_01-STAWN,
          TXNCOD(4) TYPE C,
          MENGE LIKE MSEG-MENGE,
** Furong on 02/13/12
          spicode1 like ZTMM_6026_01-spicode1,
** end on 02/13/12
        END OF WA_CONS_DATA.

  DATA: IT_CONS_DATA LIKE TABLE OF WA_CONS_DATA.
  DATA: W_EBELN LIKE EKKO-EBELN.
  FIELD-SYMBOLS: <FS_CONS_DATA> LIKE LINE OF IT_CONS_DATA.
  REFRESH: IT_ZTMM_6026_01_TMP.

  LOOP AT IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>.
    READ TABLE IT_PO_BOL INTO WA_PO_BOL
                         WITH KEY MATNR   = <FS_ZTMM_6026_01>-MATNR
                               ZFHBLNO = <FS_ZTMM_6026_01>-BILLOFLADING.
    IF SY-SUBRC NE 0.
      W_EBELN = SPACE.
    ELSE.
      W_EBELN = WA_PO_BOL-EBELN.
    ENDIF.
    WA_CONS_DATA-MATNR   = <FS_ZTMM_6026_01>-MATNR.
    WA_CONS_DATA-EBELN   = W_EBELN.
    WA_CONS_DATA-ZFHBLNO = <FS_ZTMM_6026_01>-BILLOFLADING.
    WA_CONS_DATA-ORDNR   = <FS_ZTMM_6026_01>-ORDERNUMRECEIPT.
    WA_CONS_DATA-ORDERNUMWORK  = <FS_ZTMM_6026_01>-ORDERNUMWORK.
    WA_CONS_DATA-ADPRN   = <FS_ZTMM_6026_01>-ADJPRODUCTNUM.
    WA_CONS_DATA-STAWN   = <FS_ZTMM_6026_01>-STAWN.
    WA_CONS_DATA-TXNCOD  = <FS_ZTMM_6026_01>-TXNCODE.
    WA_CONS_DATA-MENGE   = <FS_ZTMM_6026_01>-MENGE.
** furong on 02/13/12
    WA_CONS_DATA-spicode1 = <FS_ZTMM_6026_01>-spicode1.
** end on 02/13/12
    COLLECT WA_CONS_DATA INTO IT_CONS_DATA.
    CLEAR: WA_CONS_DATA, W_EBELN, WA_PO_BOL.
  ENDLOOP.

  SORT IT_ZTMM_6026_01 BY MATNR BILLOFLADING TXNCODE ORDERNUMRECEIPT
     spicode1.

  CLEAR: WA_ZTMM_6026_01.
  LOOP AT IT_CONS_DATA ASSIGNING <FS_CONS_DATA>.
** changed by Furong on 07/31/08
*  READ TABLE IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
*                           WITH KEY MATNR        = <FS_CONS_DATA>-MATNR
*                                  BILLOFLADING = <FS_CONS_DATA>-ZFHBLNO
*                                   TXNCODE      = <FS_CONS_DATA>-TXNCOD
*                                     BINARY SEARCH.

  READ TABLE IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
                           WITH KEY MATNR        = <FS_CONS_DATA>-MATNR
                                  BILLOFLADING = <FS_CONS_DATA>-ZFHBLNO
                                   TXNCODE      = <FS_CONS_DATA>-TXNCOD
                                  ORDERNUMRECEIPT = <FS_CONS_DATA>-ORDNR
** furong on 02/13/12
                                  spicode1 = <FS_CONS_DATA>-spicode1
                                     BINARY SEARCH.
** end on 02/13/12

** end of change on 07/31/08
    IF SY-SUBRC NE 0.
    ELSE.
      WA_ZTMM_6026_01 = <FS_ZTMM_6026_01>.
      WA_ZTMM_6026_01-ORDERNUMRECEIPT = <FS_CONS_DATA>-ORDNR.
      WA_ZTMM_6026_01-ORDERNUMWORK = <FS_CONS_DATA>-ORDERNUMWORK.
      WA_ZTMM_6026_01-ADJPRODUCTNUM = <FS_CONS_DATA>-ADPRN.
      WA_ZTMM_6026_01-STAWN = <FS_CONS_DATA>-STAWN.
      WA_ZTMM_6026_01-MENGE = <FS_CONS_DATA>-MENGE.
** furong on 02/13/12
            WA_ZTMM_6026_01-spicode1 = <FS_CONS_DATA>-spicode1.
** end on 02/13/12
      APPEND WA_ZTMM_6026_01 TO IT_ZTMM_6026_01_TMP.
    ENDIF.
    CLEAR WA_ZTMM_6026_01.
  ENDLOOP.

  REFRESH IT_ZTMM_6026_01.
  IT_ZTMM_6026_01[] = IT_ZTMM_6026_01_TMP[].
  REFRESH: IT_ZTMM_6026_01_TMP.
ENDFORM.                    " summarize_txncode
*&---------------------------------------------------------------------*
*&      Form  fsc_bom_exp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_STPOX  text
*      -->P_=  text
*      -->P_IT_STPOX  text
*      -->P_EXCEPTIONS  text
*      -->P_COMMUNICATION_FAILURE  text
*      -->P_=  text
*      -->P_1  text
*      -->P_SYSTEM_FAILURE  text
*      -->P_=  text
*      -->P_2  text
*      -->P_RESOURCE_FAILURE  text
*      -->P_=  text
*      -->P_3  text
*----------------------------------------------------------------------*
FORM FSC_BOM_EXP USING W_TASKNAME.
** cahnged by Fuorng, for oversize dump issue
*  data: wa_fsc_comp like wa_fsc_stpox1.
  DATA: WA_FSC_TMP LIKE STPOX,
        WA_FSC_COMP_2 LIKE WA_FSC_STPOX2.
** end of change
  DATA: IT_FSC_TMP LIKE TABLE OF STPOX.
*        it_fsc_comp_2 like table of wa_fsc_comp2.
  DATA: W_MATNR LIKE MARA-MATNR,
        W_PMATNR LIKE MARA-MATNR,
        W_PLNUM LIKE PLAF-PLNUM,
        W_VBELN LIKE VBAK-VBELN,
        W_STLAL LIKE STKO-STLAL,
        W_DATUV LIKE STKO-DATUV.

  DATA:  L_WRK02 LIKE T460A-WRK02.

  RECEIVE RESULTS FROM FUNCTION 'Z_FFTZ_EXP_BOM'
                        IMPORTING P_MATNR = W_MATNR
                                  P_STLAL1 = W_STLAL
                                  P_DATUV1 = W_DATUV
                                  P_PMATNR = W_PMATNR
                        TABLES    P_STPOX = IT_FSC_TMP
                       EXCEPTIONS
                             COMMUNICATION_FAILURE  = 1
                             SYSTEM_FIALURE         = 2.
  IF SY-SUBRC NE 0.
    W_EXCEP_FLAG = 'X'.
    EXIT.
  ENDIF.
  W_RCV_JOBS = W_RCV_JOBS + 1.

  LOOP AT IT_FSC_TMP INTO WA_FSC_TMP.
    IF WA_FSC_TMP-MTART = 'HALB'.
      IF WA_FSC_TMP-SOBSL = 40.
        SELECT SINGLE WRK02 INTO L_WRK02
         FROM T460A
         WHERE WERKS = WA_FSC_TMP-WERKS
           AND SOBSL = '40'.
*        if sy-subrc = 0.
        IT_HALB_ENG-PMATNR = W_MATNR.
        IT_HALB_ENG-MATNR = WA_FSC_TMP-IDNRK.
        IT_HALB_ENG-WERKS = L_WRK02.

*        it_halb_eng-datuv = SY-DATUM.
        IT_HALB_ENG-DATUV = W_DATUV.

        IT_HALB_ENG-PDATUV = W_DATUV.
        IT_HALB_ENG-BDMNG = WA_FSC_TMP-MNGKO.
        IT_HALB_ENG-STLAL = WA_FSC_TMP-STLAL.
        COLLECT IT_HALB_ENG.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DELETE IT_FSC_TMP WHERE MTART IN R_MTART
                    OR    DUMPS EQ 'X'
                    OR    DUMPS EQ 'x'.
*  append lines of it_fsc_tmp to it_fsc_comp.
  LOOP AT IT_FSC_TMP INTO WA_FSC_TMP.
    MOVE: WA_FSC_TMP-OJTXB TO WA_FSC_COMP_2-OJTXB,
*          wa_fsc_tmp-pmatnr TO wa_fsc_comp_2-pmatnr,
          WA_FSC_TMP-STLAL TO WA_FSC_COMP_2-STLAL,
          WA_FSC_TMP-LOEKZ TO WA_FSC_COMP_2-LOEKZ,
          WA_FSC_TMP-OJTXP TO WA_FSC_COMP_2-OJTXP,
          WA_FSC_TMP-MTART TO WA_FSC_COMP_2-MTART,
          WA_FSC_TMP-WERKS TO WA_FSC_COMP_2-WERKS,
          WA_FSC_TMP-MMEIN TO WA_FSC_COMP_2-MMEIN,
          WA_FSC_TMP-MNGLG TO WA_FSC_COMP_2-MNGLG,
          WA_FSC_TMP-MNGKO TO WA_FSC_COMP_2-MNGKO,
          WA_FSC_TMP-MSIGN TO WA_FSC_COMP_2-MSIGN,
          WA_FSC_TMP-SOBSL TO WA_FSC_COMP_2-SOBSL,
          WA_FSC_TMP-KZAUS TO WA_FSC_COMP_2-KZAUS,
          WA_FSC_TMP-AUSDT TO WA_FSC_COMP_2-AUSDT,
          WA_FSC_TMP-NFMAT TO WA_FSC_COMP_2-NFMAT,
          WA_FSC_TMP-STAWN TO WA_FSC_COMP_2-STAWN,
          WA_FSC_TMP-MATMK TO WA_FSC_COMP_2-MATMK,
          WA_FSC_TMP-DATUV TO WA_FSC_COMP_2-DATUV,
          WA_FSC_TMP-IDNRK TO WA_FSC_COMP_2-IDNRK,
          WA_FSC_TMP-SORTF TO WA_FSC_COMP_2-SORTF,
          WA_FSC_TMP-MEINS TO WA_FSC_COMP_2-MEINS,
          WA_FSC_TMP-MENGE TO WA_FSC_COMP_2-MENGE,
          WA_FSC_TMP-DATUB TO WA_FSC_COMP_2-DATUB,
          WA_FSC_TMP-DUMPS TO WA_FSC_COMP_2-DUMPS,
          WA_FSC_TMP-SOBMX TO WA_FSC_COMP_2-SOBMX,
          WA_FSC_TMP-MATKL TO WA_FSC_COMP_2-MATKL.
*    IF wa_fsc_tmp-pmatnr IS INITIAL
    WA_FSC_COMP_2-PMATNR = W_MATNR.
    WA_FSC_COMP_2-STLAL1 = W_STLAL.
    WA_FSC_COMP_2-DATUV1 = W_DATUV.
    WA_FSC_COMP_2-PMATNR = W_PMATNR.
*    ENDIF.
*    append it_fsc_comp.
    COLLECT WA_FSC_COMP_2 INTO IT_FSC_STPOX1.
    CLEAR: WA_FSC_TMP, WA_FSC_COMP_2.
  ENDLOOP.


*  wa_fsc_comp-pmatnr = w_matnr.
*  wa_fsc_comp-stlal1 = w_stlal.
*  wa_fsc_comp-datuv1 = w_datuv.
*  modify it_fsc_comp from wa_fsc_comp transporting pmatnr stlal1 datuv1
*  where pmatnr is initial.
*  append lines of it_fsc_comp to it_fsc_stpox1.
  REFRESH: IT_FSC_TMP.   " it_fsc_comp.
  CLEAR: W_MATNR, W_PLNUM, W_VBELN, W_STLAL, W_DATUV.  "wa_fsc_comp

ENDFORM.                    " fsc_bom_exp
*&---------------------------------------------------------------------*
*&      Form  ftz_sum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FTZ_SUM.

  DATA: BEGIN OF WA_SPNM_SUM,
          MATNR  LIKE MARA-MATNR,
          ORDNUM LIKE ZTMM_6026_01-ORDERNUMWORK,
          MENGE LIKE MSEG-MENGE,
        END OF WA_SPNM_SUM.
  DATA: IT_SPNM_SUM LIKE TABLE OF WA_SPNM_SUM.

  LOOP AT IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>.
    WA_SPNM_SUM-MATNR  = <FS_ZTMM_6026_01>-MATNR.
    WA_SPNM_SUM-ORDNUM = <FS_ZTMM_6026_01>-ORDERNUMWORK.
    WA_SPNM_SUM-MENGE  = <FS_ZTMM_6026_01>-MENGE.
    COLLECT WA_SPNM_SUM INTO IT_SPNM_SUM.
    CLEAR WA_SPNM_SUM.
  ENDLOOP.

  REFRESH IT_ZTMM_6026_01_TMP.

*  it_ztmm_6026_01_tmp[] = it_ztmm_6026_01.
*  refresh it_ztmm_6026_01.

  SORT: IT_SPNM_SUM BY MATNR ORDNUM,
        IT_ZTMM_6026_01 BY MATNR ORDERNUMWORK.

  LOOP AT IT_SPNM_SUM INTO WA_SPNM_SUM.
*      read table it_ztmm_6026_01_tmp assigning <fs_ztmm_6026_01>
*                                   with key matnr = wa_spnm_sum-matnr
*                                     ordernumwork = wa_spnm_sum-ordnum
*                                     binary search.
    READ TABLE IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
                                    WITH KEY MATNR = WA_SPNM_SUM-MATNR
                                      ORDERNUMWORK = WA_SPNM_SUM-ORDNUM
                                      BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR: WA_ZTMM_6026_01.
      CONTINUE.
    ELSE.
      WA_ZTMM_6026_01 = <FS_ZTMM_6026_01>.
      WA_ZTMM_6026_01-MENGE = WA_SPNM_SUM-MENGE.
      APPEND WA_ZTMM_6026_01 TO IT_ZTMM_6026_01_TMP.
      CLEAR: WA_ZTMM_6026_01.
    ENDIF.
  ENDLOOP.
  IT_ZTMM_6026_01[] = IT_ZTMM_6026_01_TMP[].
  FREE: IT_ZTMM_6026_01_TMP.
ENDFORM.                    " ftz_sum
*&---------------------------------------------------------------------*
*&      Form  summarize_cppc_cnpc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMMARIZE_CPPC_CNPC.
  DATA: BEGIN OF WA_CPPC_CNPC,
          MATNR LIKE MARA-MATNR,
          TXNCODE(4) TYPE C,
          ADJPRODUCTNUM LIKE ZTMM_6026_01-ADJPRODUCTNUM,
          MENGE LIKE MSEG-MENGE,
        END OF WA_CPPC_CNPC.
*  DATA: W_PREV_MATNR LIKE MARA-MATNR,
*        W_ADJ_CNT TYPE I,
*        W_CUR_CNT TYPE I,
*        W_ADJ_NETQTY LIKE MSEG-MENGE,
*        W_ADJ_TXNCODE(4) TYPE C.

  DATA: W_ADJ_LINES TYPE I,
        L_INDEX LIKE SY-TABIX.

  DATA: IT_CPPC_CNPC LIKE TABLE OF WA_CPPC_CNPC.

  FIELD-SYMBOLS: <FS_TEMP> LIKE LINE OF IT_CPPC_CNPC.

  REFRESH: IT_ZTMM_6026_01_TMP.

  LOOP AT IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
                          WHERE TXNCODE = 'CPPC' OR TXNCODE = 'CNPC'.
    L_INDEX = SY-TABIX.
    IF <FS_ZTMM_6026_01>-MATNR = <FS_ZTMM_6026_01>-ADJPRODUCTNUM.
      DELETE IT_ZTMM_6026_01 INDEX L_INDEX.
    ELSE.
      WA_CPPC_CNPC-MATNR   = <FS_ZTMM_6026_01>-MATNR.
      WA_CPPC_CNPC-TXNCODE = <FS_ZTMM_6026_01>-TXNCODE.
      WA_CPPC_CNPC-ADJPRODUCTNUM   = <FS_ZTMM_6026_01>-ADJPRODUCTNUM.
      WA_CPPC_CNPC-MENGE   = <FS_ZTMM_6026_01>-MENGE.
      COLLECT WA_CPPC_CNPC INTO IT_CPPC_CNPC.
    ENDIF.
  ENDLOOP.

  CLEAR: WA_ZTMM_6026_01.

  LOOP AT IT_CPPC_CNPC ASSIGNING <FS_TEMP>.
    READ TABLE IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
                               WITH KEY MATNR = <FS_TEMP>-MATNR
                                        TXNCODE = <FS_TEMP>-TXNCODE
                              ADJPRODUCTNUM = <FS_TEMP>-ADJPRODUCTNUM.

    IF SY-SUBRC = 0.
      WA_ZTMM_6026_01 = <FS_ZTMM_6026_01>.
      WA_ZTMM_6026_01-MENGE = <FS_TEMP>-MENGE.
      APPEND WA_ZTMM_6026_01 TO IT_ZTMM_6026_01_TMP.
    ENDIF.
  ENDLOOP.

  DELETE IT_ZTMM_6026_01 WHERE TXNCODE = 'CPPC' OR TXNCODE = 'CNPC'.

  DESCRIBE TABLE IT_ZTMM_6026_01_TMP LINES W_ADJ_LINES.
  IF W_ADJ_LINES > 0.
    APPEND LINES OF IT_ZTMM_6026_01_TMP TO IT_ZTMM_6026_01.
  ENDIF.

  CLEAR: W_ADJ_LINES.
  FREE: IT_ZTMM_6026_01_TMP.
ENDFORM.                    " summarize_cppc_cnpc
