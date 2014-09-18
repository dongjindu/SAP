REPORT ZMMIFTZR.
INCLUDE ZMMFZTOP.
*&--------------------------------------------------------------------&*
*&    Program: ZMMIFTZR.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Send FTZ related Goods receipt information.      &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 03/10/2005  Shiva    UD1K914918      initial program.
*& 04/18/2005  Shiva    UD1K915611      Change the logic of adding FSC
*&                                   code for "KD' parts to "MIP" parts.
*& 04/21/2005  Shiva    UD1k915691      Change FTZ tcode for Mvt.type
*&                                   '309' & '310' based on mseg-shkzg.
*& 08/26/2005  Shiva    UD1K917075      FTZ Consumption info for FIFO.
*&                      UD1K917581      add oredr num for grouping.
*& 10/14/2005  Shiva    UD1K917970   Fixed: 1. 'APPC' ord.numship info.
*&                                          2. country of origin value.
*& 10/17/2005  Shiva    UD1K917992      Don't change Transid if BOL
*&                                        exists - FTZ.
*& 10/18/2005  Shiva    UD1K918009   Fixes based on BCV client test.
*& 10/19/2005  Shiva    UD1K918027   Fixes based on BCV client test.
*& 10/20/2005  Shiva    UD1K918058   Fixes based on BCV client test.
*& 10/21/2005  Shiva    UD1K918078   Fixes based on BCV client test.
*& 10/27/2005  Shiva    UD1K918147   Added Mov.type '991' .
*& 10/28/2005  Shiva    UD1K918169   include Mov.type '991' for sales
*&                                   doc. selection for 'Shipto' info.
*  04/10/2006  Furong                for 991, add work order and HTS to
*                                    the consolidation check
*&--------------------------------------------------------------------&*

PARAMETERS: P_DATE LIKE SY-DATUM OBLIGATORY.
SELECT-OPTIONS S_MBLNR FOR MKPF-MBLNR.
PARAMETERS: P_SRVGRP LIKE RZLLITAB-CLASSNAME OBLIGATORY
                     DEFAULT 'PG_FTZ'.

DATA: W_COLOR1(3) TYPE C,
      L_INDEX LIKE SY-INDEX.

DATA: L_EBELN_LOG LIKE MSEG-EBELN,
      L_EBELP_LOG LIKE MSEG-EBELP.
*      L_MBLNR_LOG LIKE MSEG-MBLNR,
*      L_MJAHR_LOG LIKE MSEG-MJAHR.
DATA: BEGIN OF LT_MSEG OCCURS 0,
      EBELN  LIKE MSEG-EBELN,
      EBELP  LIKE MSEG-EBELP,
      ZBUDAT LIKE MSEG-ZBUDAT,
      END OF LT_MSEG.

DATA: BEGIN OF IT_LIST_PRICE OCCURS 0,
      MATNR LIKE MSEG-MATNR,
      EBELN  LIKE MSEG-EBELN,
      EBELP  LIKE MSEG-EBELP,
      STD LIKE MSEG-DMBTR,
      PO  LIKE MSEG-DMBTR,
      INFO LIKE MSEG-DMBTR,
      END OF IT_LIST_PRICE.

DATA: L_NETPRUOM LIKE WA_EINA_PO-KBETR.
DATA: LT_MSEG_AKNH LIKE TABLE OF WA_MSEG WITH HEADER LINE.

RANGES: LR_BWART FOR MSEG-BWART.
PERFORM ASSIGN_VALID_MOVTYPES.


LR_BWART-SIGN = 'I'.
LR_BWART-OPTION = 'EQ'.
LR_BWART-LOW = '601'.
APPEND LR_BWART.
LR_BWART-SIGN = 'I'.
LR_BWART-OPTION = 'EQ'.
LR_BWART-LOW = '602'.
APPEND LR_BWART.
LR_BWART-SIGN = 'I'.
LR_BWART-OPTION = 'EQ'.
LR_BWART-LOW = '991'.
APPEND LR_BWART.


** Changed by Furong on 04/23/09

*SELECT MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW GEWEI XBLNR
*                       XABLN BWART SHKZG WERKS MAT_KDAUF SUM( MENGE )
*                       ERFME UMMAT EBELN EBELP MAKTX
*                        INTO TABLE IT_MSEG
*                        FROM MARA
*                        INNER JOIN MSEG
*                        ON MSEG~MATNR = MARA~MATNR
*                        INNER JOIN MKPF
*                        ON MSEG~MBLNR = MKPF~MBLNR
*                        AND MSEG~MJAHR = MKPF~MJAHR
*                        INNER JOIN MAKT
*                        ON MAKT~MATNR = MARA~MATNR
*                        WHERE ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'ROH1' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'HALB' AND
*                              SPRAS = SY-LANGU AND BWART IN R_BWART )
*                        OR    ( MKPF~MBLNR IN S_MBLNR AND
*                                CPUDT = P_DATE AND MTART = 'FERT' AND
*                              SPRAS = SY-LANGU AND BWART = '991' )
*          GROUP BY MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW
*                   GEWEI XBLNR XABLN BWART SHKZG WERKS MAT_KDAUF ERFME
*                   UMMAT EBELN EBELP MAKTX.

SELECT MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW GEWEI XBLNR
                       XABLN BWART SHKZG WERKS MAT_KDAUF AS KDAUF
                       SUM( MENGE )
                       ERFME UMMAT EBELN EBELP MAKTX
                        INTO TABLE IT_MSEG
                        FROM MARA
                        INNER JOIN MSEG
                        ON MSEG~MATNR = MARA~MATNR
                        INNER JOIN MKPF
                        ON MSEG~MBLNR = MKPF~MBLNR
                        AND MSEG~MJAHR = MKPF~MJAHR
                        INNER JOIN MAKT
                        ON MAKT~MATNR = MARA~MATNR
                        WHERE ( MKPF~MBLNR IN S_MBLNR AND
                                CPUDT = P_DATE AND MTART = 'ROH' AND
                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
                              LGORT <> 'XMIT' )
                        OR    ( MKPF~MBLNR IN S_MBLNR AND
                                CPUDT = P_DATE AND MTART = 'ROH1' AND
                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
                              LGORT <> 'XMIT' )
                        OR    ( MKPF~MBLNR IN S_MBLNR AND
                                CPUDT = P_DATE AND MTART = 'HALB' AND
                              SPRAS = SY-LANGU AND BWART IN R_BWART AND
                              LGORT <> 'XMIT' )
                        OR    ( MKPF~MBLNR IN S_MBLNR AND
                                CPUDT = P_DATE AND MTART = 'FERT' AND
                              SPRAS = SY-LANGU AND BWART = '991' )
          GROUP BY MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW
                   GEWEI XBLNR XABLN BWART SHKZG WERKS MAT_KDAUF ERFME
                   UMMAT EBELN EBELP MAKTX.
** End of change on 04/23/09

** Changed by Furong on 11/03/09

*SELECT MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW GEWEI XBLNR
*                       XABLN BWART SHKZG WERKS MAT_KDAUF  AS KDAUF
*                       SUM( MENGE )
*                       ERFME UMMAT EBELN EBELP MAKTX
*                        INTO TABLE LT_MSEG_AKNH
*                        FROM MARA
*                        INNER JOIN MSEG
*                        ON MSEG~MATNR = MARA~MATNR
*                        INNER JOIN MKPF
*                        ON MSEG~MBLNR = MKPF~MBLNR
*                        AND MSEG~MJAHR = MKPF~MJAHR
*                        INNER JOIN MAKT
*                        ON MAKT~MATNR = MARA~MATNR
*                        WHERE MKPF~MBLNR IN S_MBLNR AND
*                              CPUDT = P_DATE AND MTART = 'HALB' AND
*                              SPRAS = SY-LANGU AND BWART IN LR_BWART
*AND
*                              WERKS = 'E001' AND LGORT = 'E302'
*                              AND MSEG~KUNNR = 'AKNH'
*          GROUP BY MARA~MATNR LIFNR MTART MARA~MEINS PROFL NTGEW
*                   GEWEI XBLNR XABLN BWART SHKZG WERKS MAT_KDAUF ERFME
*                   UMMAT EBELN EBELP MAKTX.
*
*LOOP AT LT_MSEG_AKNH.
*  READ TABLE IT_MSEG INTO <FS_MSEG>
*                     WITH KEY MATNR = LT_MSEG_AKNH-MATNR
*                              LIFNR = LT_MSEG_AKNH-LIFNR
*                              XBLNR = LT_MSEG_AKNH-XBLNR
*                              XABLN = LT_MSEG_AKNH-XABLN
*                              BWART = LT_MSEG_AKNH-BWART
*                              SHKZG = LT_MSEG_AKNH-SHKZG
*                              WERKS = LT_MSEG_AKNH-WERKS
*                              KDAUF = LT_MSEG_AKNH-KDAUF
*                              EBELN = LT_MSEG_AKNH-EBELN
*                              EBELP = LT_MSEG_AKNH-EBELP.
*  IF SY-SUBRC = 0.
*    L_INDEX = SY-INDEX.
*    <FS_MSEG>-MENGE =  <FS_MSEG>-MENGE - LT_MSEG_AKNH-MENGE.
*    IF <FS_MSEG>-MENGE > 0.
*      MODIFY IT_MSEG FROM <FS_MSEG> INDEX L_INDEX TRANSPORTING MENGE.
*    ELSE.
*      DELETE IT_MSEG FROM <FS_MSEG>.
*    ENDIF.
*  ENDIF.
*ENDLOOP.
** End of change on 11/03/09


IF SY-SUBRC NE 0.
  MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH TEXT-001.
  EXIT.
ENDIF.

WA_MSEG-MNGKO = 1.
MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING MNGKO
                      WHERE MNGKO IS INITIAL.
CLEAR WA_MSEG.
*&----For FTZ reporting we just change only the internal table for 991.
WA_MSEG-MTART = 'HALB'.
MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING MTART
                      WHERE BWART = '991'.
CLEAR WA_MSEG.

PERFORM GET_BOM_INFO.
PERFORM GET_SALEPART_CCODE USING 'R'.
SORT: IT_MSEG BY MATNR BWART,
      IT_LFA1 BY LIFNR.
LOOP AT IT_MSEG ASSIGNING <FS_MSEG>.
  CLEAR: W_MATNR.
  WA_ZTMM_6026_01-PARTNERID = '100300'.
  W_DATE = SY-DATUM.
  W_TIME = SY-UZEIT.
  CONCATENATE W_DATE 'T' W_TIME  INTO WA_ZTMM_6026_01-EFFDATE.
  PERFORM GET_TCODE USING <FS_MSEG>-MTART
                          <FS_MSEG>-BWART
                          <FS_MSEG>-SHKZG
                          <FS_MSEG>-PROFL
                          <FS_MSEG>-XABLN
                    CHANGING W_MATNR
                             WA_ZTMM_6026_01-TXNCODE
                             WA_ZTMM_6026_01-ORDERNUMRECEIPT
                             WA_ZTMM_6026_01-ORDERNUMSHIP
                             WA_ZTMM_6026_01-TRANSPORTID
                             WA_ZTMM_6026_01-BILLOFLADING
                             WA_FTZTCODE.
  CONCATENATE P_DATE 'T' '000000' INTO WA_ZTMM_6026_01-TXNDATE.
  IF <FS_MSEG>-BWART = '601' OR <FS_MSEG>-BWART = '991' .
    IF <FS_MSEG>-PROFL = 'M' OR <FS_MSEG>-PPROFL = 'M'.
      CONCATENATE <FS_MSEG>-PMATNR P_DATE
                                   INTO WA_ZTMM_6026_01-ORDERNUMWORK.
    ELSEIF <FS_MSEG>-PROFL = 'V' OR <FS_MSEG>-PPROFL = 'V'.
      WA_ZTMM_6026_01-ORDERNUMWORK = WA_ZTMM_6026_01-ORDERNUMRECEIPT.
      CLEAR: WA_ZTMM_6026_01-ORDERNUMRECEIPT,
             WA_ZTMM_6026_01-TRANSPORTID.
    ENDIF.
  ELSE.
    WA_ZTMM_6026_01-ORDERNUMWORK = SPACE.
  ENDIF.
  CASE WA_ZTMM_6026_01-TXNCODE.
    WHEN 'ANPC'.
      CONCATENATE <FS_MSEG>-PMATNR P_DATE
                                  INTO WA_ZTMM_6026_01-ORDERNUMSHIP.
      WA_ZTMM_6026_01-COUNTRYSHIPTO = 'US'.
    WHEN 'SPPC'.
      CASE <FS_MSEG>-PROFL.
        WHEN 'K'.
          CONCATENATE 'GIKD' P_DATE INTO WA_ZTMM_6026_01-ORDERNUMSHIP.
        WHEN 'V'.
          CONCATENATE 'GILP' P_DATE INTO WA_ZTMM_6026_01-ORDERNUMSHIP.
        WHEN 'M'.
          CONCATENATE 'GIMIP' P_DATE INTO WA_ZTMM_6026_01-ORDERNUMSHIP.
      ENDCASE.
      READ TABLE IT_VBPA INTO WA_VBPA WITH KEY VBELN = <FS_MSEG>-KDAUF
                                                         BINARY SEARCH.
      IF SY-SUBRC NE 0.
       READ TABLE IT_LFA1 INTO WA_LFA1 WITH KEY LIFNR = <FS_MSEG>-LIFNR
                                                          BINARY SEARCH.
        IF SY-SUBRC NE 0.
          WA_ZTMM_6026_01-COUNTRYSHIPTO = SPACE.
        ELSE.
          WA_ZTMM_6026_01-COUNTRYSHIPTO = WA_LFA1-LAND1.
        ENDIF.
      ELSE.
        WA_ZTMM_6026_01-COUNTRYSHIPTO = WA_VBPA-LAND1.
      ENDIF.
    WHEN OTHERS.
      WA_ZTMM_6026_01-ORDERNUMSHIP = SPACE.
  ENDCASE.
  IF NOT W_MATNR IS INITIAL.
    WA_ZTMM_6026_01-MATNR = W_MATNR.
  ELSE.
    WA_ZTMM_6026_01-MATNR = <FS_MSEG>-MATNR.
  ENDIF.
  CASE <FS_MSEG>-PROFL.
    WHEN SPACE.
      IF <FS_MSEG>-MTART = 'FERT'.
        WA_ZTMM_6026_01-PTC = 'IM'.
      ENDIF.
    WHEN 'K' OR 'V'.
      WA_ZTMM_6026_01-PTC = 'PC'.
    WHEN 'M'.
      WA_ZTMM_6026_01-PTC = 'IM'.
  ENDCASE.
  WA_ZTMM_6026_01-PTCSRC = C_ERP_SOURCE.
  WA_ZTMM_6026_01-MAKTX = <FS_MSEG>-MAKTX.
  WA_ZTMM_6026_01-MAKTXSRC = C_ERP_SOURCE.
  WA_ZTMM_6026_01-NAFTACERTIFIED = SPACE.
  WA_ZTMM_6026_01-NAFTACERTIFIEDSC = C_FTZLINK_SOURCE.
  CASE WA_FTZTCODE-SHKZG.
    WHEN 'H'.
      WA_ZTMM_6026_01-MENGE = ABS( <FS_MSEG>-MENGE ).
    WHEN 'S'.
      IF <FS_MSEG>-MENGE GT 0.
        WA_ZTMM_6026_01-MENGE = <FS_MSEG>-MENGE * -1.
      ELSE.
        WA_ZTMM_6026_01-MENGE = <FS_MSEG>-MENGE.
      ENDIF.
  ENDCASE.
  IF WA_ZTMM_6026_01-TXNCODE = 'CNPC'.
    IF WA_ZTMM_6026_01-MENGE GT 0.
      WA_ZTMM_6026_01-MENGE  = WA_ZTMM_6026_01-MENGE  * -1.
    ENDIF.
  ENDIF.
  WA_ZTMM_6026_01-MEINS = <FS_MSEG>-MEINS.
  WA_ZTMM_6026_01-MEINSSRC = C_ERP_SOURCE.
  WA_ZTMM_6026_01-QTYPERLM = ABS( <FS_MSEG>-MNGKO ).

  IF <FS_MSEG>-NTGEW IS INITIAL.
    WA_ZTMM_6026_01-NTGEWSRC = C_FTZLINK_SOURCE.
    WA_ZTMM_6026_01-GEWEISRC = C_FTZLINK_SOURCE.
  ELSE.
    WA_ZTMM_6026_01-NTGEW  = <FS_MSEG>-NTGEW.
    WA_ZTMM_6026_01-NTGEWSRC = C_ERP_SOURCE.
    WA_ZTMM_6026_01-GEWEI = <FS_MSEG>-GEWEI.
    WA_ZTMM_6026_01-GEWEISRC = C_ERP_SOURCE.
  ENDIF.
  PERFORM SEPARATE_COLOR USING <FS_MSEG>-UMMAT <FS_MSEG>-MTART
                         CHANGING <FS_MSEG>-UMMAT W_COLOR1.
  WA_ZTMM_6026_01-ADJPRODUCTNUM = <FS_MSEG>-UMMAT.
*wa_ztmm_6026_01-receiptdocid        "No value passed
*wa_ztmm_6026_01-exitdocid           "No value passed
*wa_ztmm_6026_01-adjreceiptdocid     "No value passed
*wa_ztmm_6026_01-fromzoneid          "No value passed
*wa_ztmm_6026_01-tozoneid            "No value passed
  READ TABLE IT_ZTBL INTO WA_ZTBL WITH KEY EBELN = <FS_MSEG>-EBELN
                                           EBELP = <FS_MSEG>-EBELP
                                           XBLNR = <FS_MSEG>-XBLNR
                                  TRANSPORTING ZFHBLNO ZFRPTTY ZFVIA.
  IF SY-SUBRC NE 0.
    WA_ZTMM_6026_01-MODEOFTRANSPORT = 'L'.
    IF WA_ZTMM_6026_01-PROFL EQ 'M'.
      WA_ZTMM_6026_01-STATUSCODE    = 'F'.
      WA_ZTMM_6026_01-STATUSCODESRC = ''.
    ELSE.
      WA_ZTMM_6026_01-STATUSCODE    = 'D'.
      WA_ZTMM_6026_01-STATUSCODESRC = ''.
    ENDIF.
  ELSE.
    WA_PO_BOL-MATNR = <FS_MSEG>-MATNR.
    WA_PO_BOL-EBELN = <FS_MSEG>-EBELN.
    WA_PO_BOL-ZFHBLNO = WA_ZTBL-ZFHBLNO.
    APPEND WA_PO_BOL TO IT_PO_BOL.
    CLEAR WA_PO_BOL.
    CASE WA_ZTBL-ZFVIA.
      WHEN 'VSL'.
        WA_ZTMM_6026_01-MODEOFTRANSPORT = 'O'.
      WHEN 'AIR'.
        WA_ZTMM_6026_01-MODEOFTRANSPORT = 'A'.
    ENDCASE.
    IF WA_ZTBL-ZFRPTTY = 'F'.
      WA_ZTMM_6026_01-STATUSCODE    = ''.
      WA_ZTMM_6026_01-STATUSCODESRC = 'I'.
    ELSE.
      WA_ZTMM_6026_01-STATUSCODE    = 'I'.
      WA_ZTMM_6026_01-STATUSCODESRC = ''.
    ENDIF.
  ENDIF.
*wa_ztmm_6026_01-receiptdate         "No value passed
*wa_ztmm_6026_01-itnum               "No value passed
*wa_ztmm_6026_01-exportdate          "No value passed
*wa_ztmm_6026_01-manifestqty         "No value passed
  WA_ZTMM_6026_01-VALIDFLAG        = 'N'.
  WA_ZTMM_6026_01-ASSIGNMENTFLAG   = 'N'.
  WA_ZTMM_6026_01-FIFOFLAG         = 'N'.
  WA_ZTMM_6026_01-DELETEDFLAG      = 'N'.
  WA_ZTMM_6026_01-KEEPDURINGROLLBA = 'N'.
*&----------FIFO HTS value
  IF <FS_MSEG>-BWART = '991' AND <FS_MSEG>-XABLN = 'T'.
    WA_ZTMM_6026_01-STAWN    = '9817.85.01'.
    WA_ZTMM_6026_01-STAWNSRC = C_ERP_SOURCE.
  ELSE.
    READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
                           WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
                                          WERKS = <FS_MSEG>-WERKS
                                          TRANSPORTING STAWN.
    IF SY-SUBRC NE 0.
      WA_ZTMM_6026_01-STAWN = SPACE.
      WA_ZTMM_6026_01-STAWNSRC = 'H'.
    ELSE.
      IF WA_MAT_INFO-STAWN IS INITIAL.
        WA_ZTMM_6026_01-STAWN = SPACE.
        WA_ZTMM_6026_01-STAWNSRC = 'H'.
      ELSE.
        WA_ZTMM_6026_01-STAWN = WA_MAT_INFO-STAWN.
        WA_ZTMM_6026_01-STAWNSRC = C_ERP_SOURCE.
      ENDIF.
    ENDIF.
  ENDIF.
  WA_ZTMM_6026_01-SPICODE1SRC = 'H'.
  WA_ZTMM_6026_01-SPICODE2SRC = 'H'.
  WA_ZTMM_6026_01-LIFNRSRC = C_FTZLINK_SOURCE.
  WA_ZTMM_6026_01-RELFLAGSRC = 'M'.
  WA_ZTMM_6026_01-HTSINDEXSRC = C_FTZLINK_SOURCE.
  WA_ZTMM_6026_01-HTSDESCSRC = 'H'.
  WA_ZTMM_6026_01-HTSNUM2SRC = C_FTZLINK_SOURCE.

** changed by Furong on 08/11/08

*  READ TABLE IT_EINA INTO WA_EINA WITH KEY MATNR = <FS_MSEG>-MATNR
*                                                   BINARY SEARCH.
*  IF SY-SUBRC NE 0.
*    READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
*                           WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
*                                          WERKS = <FS_MSEG>-WERKS
*                                          TRANSPORTING STPRS PEINH.
*    IF SY-SUBRC NE 0.
*      WA_ZTMM_6026_01-NETPR = 0.
*      WA_ZTMM_6026_01-EFFPR = 0.
*      WA_ZTMM_6026_01-NETPRUOM = 0.
*      WA_ZTMM_6026_01-EFFPRUOM = 0.
*    ELSE.
*      WA_ZTMM_6026_01-NETPR = WA_MAT_INFO-STPRS.
*      WA_ZTMM_6026_01-EFFPR = WA_MAT_INFO-STPRS.
*      WA_ZTMM_6026_01-NETPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH.
*      WA_ZTMM_6026_01-EFFPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH.
*    ENDIF.
*  ELSE.
*    IF <FS_MSEG>-MEINS NE <FS_MSEG>-ERFME.
*      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
*           EXPORTING
*                INPUT                = WA_EINA-KBETR
*                MATNR                = <FS_MSEG>-MATNR
*                MEINH                = <FS_MSEG>-ERFME
*                MEINS                = <FS_MSEG>-MEINS
*           IMPORTING
*                OUTPUT               = WA_EINA-KBETR
*           EXCEPTIONS
*                CONVERSION_NOT_FOUND = 1
*                INPUT_INVALID        = 2
*                MATERIAL_NOT_FOUND   = 3
*                MEINH_NOT_FOUND      = 4
*                MEINS_MISSING        = 5
*                NO_MEINH             = 6
*                OUTPUT_INVALID       = 7
*                OVERFLOW             = 8
*                OTHERS               = 9.
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*    ENDIF.
*    WA_ZTMM_6026_01-NETPR = WA_EINA-KBETR.
*    WA_ZTMM_6026_01-EFFPR = WA_EINA-EFFPR.
*    IF NOT WA_EINA-BPUMN IS INITIAL.
* WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA-BPUMZ ) /
*                                                          WA_EINA-BPUMN
*.
* WA_ZTMM_6026_01-EFFPRUOM = ( WA_ZTMM_6026_01-EFFPR * WA_EINA-BPUMZ ) /
*                                                          WA_EINA-BPUMN
*.
*      IF NOT WA_EINA-PEINH IS INITIAL.
*    WA_ZTMM_6026_01-NETPRUOM = WA_ZTMM_6026_01-NETPRUOM / WA_EINA-PEINH
*.
*    WA_ZTMM_6026_01-EFFPRUOM = WA_ZTMM_6026_01-EFFPRUOM / WA_EINA-PEINH
*.
*      ENDIF.
*    ENDIF.
*  ENDIF.

* IF WA_EINA-LAND1 IS INITIAL.
*    WA_ZTMM_6026_01-LAND1    = WA_EINA-LAND1.
*    WA_ZTMM_6026_01-LAND1SRC = 'I'.
*  ELSE.
*    WA_ZTMM_6026_01-LAND1    = WA_EINA-LAND1.
*    WA_ZTMM_6026_01-LAND1SRC = SPACE.
*  ENDIF.
*  WA_ZTMM_6026_01-VALUE2SRC = C_FTZLINK_SOURCE.
*  IF WA_EINA-WAERS IS INITIAL.
*    WA_ZTMM_6026_01-WAERSSRC = C_FTZLINK_SOURCE.
*  ELSE.
*    WA_ZTMM_6026_01-WAERS   = WA_EINA-WAERS.
*    WA_ZTMM_6026_01-WAERSSRC = C_ERP_SOURCE..
*  ENDIF.

  CLEAR: L_EBELN_LOG, L_EBELP_LOG, LT_MSEG[].
READ TABLE IT_EINA_PO INTO WA_EINA_PO WITH KEY  MATNR = <FS_MSEG>-MATNR
                                                          BINARY SEARCH.

  IF SY-SUBRC = 0.
** Changed by Furong pn 08/01/08 Requested by Mr.Yoon
    IT_LIST_PRICE-MATNR = <FS_MSEG>-MATNR.
    IT_LIST_PRICE-INFO = WA_EINA_PO-KBETR.
    IT_LIST_PRICE-EBELN = <FS_MSEG>-EBELN.
    READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
                               WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
                                              WERKS = <FS_MSEG>-WERKS
                                              TRANSPORTING STPRS.
    IF SY-SUBRC EQ 0.
      IT_LIST_PRICE-STD = WA_MAT_INFO-STPRS.
    ENDIF.
    APPEND IT_LIST_PRICE.
  ENDIF.
** end of change

*** changed by Furong on09/19/09
*  IF SY-SUBRC NE 0.
** Changed by Furong on 08/13/09 , Requested by Prasa

*    SELECT EBELN EBELP ZBUDAT INTO TABLE LT_MSEG
*         FROM MSEG
*       WHERE BWART = '101'
*         AND MATNR = <FS_MSEG>-MATNR.
*    SORT LT_MSEG BY ZBUDAT DESCENDING.
*    READ TABLE LT_MSEG INDEX 1.

*     IF SY-SUBRC = 0.
*      L_EBELN_LOG = LT_MSEG-EBELN.
*      L_EBELP_LOG = LT_MSEG-EBELP.

*      SELECT SINGLE NETPR BPUMZ BPUMN PEINH UMREN UMREZ INTO
*           (WA_EKPO-NETPR, WA_EKPO-BPUMZ, WA_EKPO-BPUMN, WA_EKPO-PEINH,
*             WA_EKPO-UMREN, WA_EKPO-UMREZ)
*        FROM EKPO
*        WHERE EBELN = L_EBELN_LOG
*          AND EBELP = L_EBELP_LOG.
*
*      IF SY-SUBRC = 0.
*        WA_EINA_PO-KBETR = WA_EKPO-NETPR.
*        WA_EINA_PO-BPUMZ  = WA_EKPO-BPUMZ.
*        WA_EINA_PO-BPUMN = WA_EKPO-BPUMN.
*        WA_EINA_PO-PEINH = WA_EKPO-PEINH.
*        WA_EINA_PO-UMREN = WA_EKPO-UMREN.
*        WA_EINA_PO-UMREz = WA_EKPO-UMREz.
*
*      ELSE.
*        READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
*                              WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
*                                             WERKS = <FS_MSEG>-WERKS
*                                             TRANSPORTING STPRS PEINH.
*        IF SY-SUBRC NE 0.
*          WA_ZTMM_6026_01-NETPR = 0.
*          WA_ZTMM_6026_01-EFFPR = 0.
*          WA_ZTMM_6026_01-NETPRUOM = 0.
*          WA_ZTMM_6026_01-EFFPRUOM = 0.
*        ELSE.
*          WA_EINA_PO-KBETR = WA_MAT_INFO-STPRS.
*          WA_ZTMM_6026_01-NETPR = WA_MAT_INFO-STPRS.
*          WA_ZTMM_6026_01-EFFPR = WA_MAT_INFO-STPRS.
*       WA_ZTMM_6026_01-NETPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*       WA_ZTMM_6026_01-EFFPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*        ENDIF.
*
*      ENDIF.
*    ELSE.
*      READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
*                             WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
*                                            WERKS = <FS_MSEG>-WERKS
*                                            TRANSPORTING STPRS PEINH.
*      IF SY-SUBRC NE 0.
*        WA_ZTMM_6026_01-NETPR = 0.
*        WA_ZTMM_6026_01-EFFPR = 0.
*        WA_ZTMM_6026_01-NETPRUOM = 0.
*        WA_ZTMM_6026_01-EFFPRUOM = 0.
*      ELSE.
*        WA_EINA_PO-KBETR = WA_MAT_INFO-STPRS.
*        WA_ZTMM_6026_01-NETPR = WA_MAT_INFO-STPRS.
*        WA_ZTMM_6026_01-EFFPR = WA_MAT_INFO-STPRS.
*       WA_ZTMM_6026_01-NETPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*       WA_ZTMM_6026_01-EFFPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH
*.
*      ENDIF.
*    ENDIF.
*
*  ELSE.
*** end of change on 09/19/09
*    IF NOT <FS_MSEG>-EBELN IS INITIAL.
*      CLEAR: WA_EKPO.
*      READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN =  <FS_MSEG>-EBELN
*                                              EBELP =  <FS_MSEG>-EBELP
*                                              MATNR =  <FS_MSEG>-MATNR
*                                                     BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        WA_EINA_PO-KBETR = WA_EKPO-NETPR.
*        WA_EINA_PO-BPUMZ  = WA_EKPO-BPUMZ.
*        WA_EINA_PO-BPUMN = WA_EKPO-BPUMN.
** Changed by Furong pn 08/01/08 Requested by Mr.Yoon

** End of change
*      ENDIF.
*    ENDIF.
*  ENDIF.
*** changed by Furong on09/19/09
** Changed by Furong pn 08/01/08 Requested by Mr.Yoon
*        IT_LIST_PRICE-PO = WA_EKPO-NETPR.
*  APPEND IT_LIST_PRICE.
*  CLEAR: IT_LIST_PRICE.
** End of change

*

*
*  IF <FS_MSEG>-MEINS NE <FS_MSEG>-ERFME.
*    CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
*         EXPORTING
*              INPUT                = WA_EINA_PO-KBETR
*              MATNR                = <FS_MSEG>-MATNR
*              MEINH                = <FS_MSEG>-ERFME
*              MEINS                = <FS_MSEG>-MEINS
*         IMPORTING
*              OUTPUT               = WA_EINA_PO-KBETR
*         EXCEPTIONS
*              CONVERSION_NOT_FOUND = 1
*              INPUT_INVALID        = 2
*              MATERIAL_NOT_FOUND   = 3
*              MEINH_NOT_FOUND      = 4
*              MEINS_MISSING        = 5
*              NO_MEINH             = 6
*              OUTPUT_INVALID       = 7
*              OVERFLOW             = 8
*              OTHERS               = 9.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*  ENDIF.
*  WA_ZTMM_6026_01-NETPR = WA_EINA_PO-KBETR.
*  WA_ZTMM_6026_01-EFFPR = WA_EINA_PO-EFFPR.
*  IF NOT WA_EINA_PO-BPUMN IS INITIAL.
*
*** Changed by Furong on 08/06/09 Requested by Prasa
*
**WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA_PO-BPUMZ
*)
**                                                    / WA_EINA_PO-BPUMN
*.
*l_NETPRUOM = WA_EINA_PO-KBETR.
*WA_ZTMM_6026_01-NETPRUOM = ( l_NETPRUOM * WA_EINA_PO-BPUMZ *
*WA_eina_PO-UMREN ) / ( WA_EINA_PO-BPUMN * WA_eina_PO-UMREz ).
**
** end of change
*** end of change on 09/19/09


READ TABLE IT_EINA_PO INTO WA_EINA_PO WITH KEY  MATNR = <FS_MSEG>-MATNR
                                                          BINARY SEARCH.
  IF SY-SUBRC NE 0.
    READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
                           WITH TABLE KEY MATNR = <FS_MSEG>-MATNR
                                          WERKS = <FS_MSEG>-WERKS
                                          TRANSPORTING STPRS PEINH.
    IF SY-SUBRC NE 0.
      WA_ZTMM_6026_01-NETPR = 0.
      WA_ZTMM_6026_01-EFFPR = 0.
      WA_ZTMM_6026_01-NETPRUOM = 0.
      WA_ZTMM_6026_01-EFFPRUOM = 0.
    ELSE.
      WA_ZTMM_6026_01-NETPR = WA_MAT_INFO-STPRS.
      WA_ZTMM_6026_01-EFFPR = WA_MAT_INFO-STPRS.
      WA_ZTMM_6026_01-NETPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH.
      WA_ZTMM_6026_01-EFFPRUOM = WA_MAT_INFO-STPRS / WA_MAT_INFO-PEINH.
    ENDIF.
  ELSE.

    IF NOT <FS_MSEG>-EBELN IS INITIAL.
      CLEAR: WA_EKPO.
      READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN =  <FS_MSEG>-EBELN
                                            EBELP =  <FS_MSEG>-EBELP
                                            MATNR =  <FS_MSEG>-MATNR
                                                   BINARY SEARCH.
      IF SY-SUBRC = 0.
        WA_EINA_PO-KBETR = WA_EKPO-NETPR.
        WA_EINA_PO-BPUMZ  = WA_EKPO-BPUMZ.
        WA_EINA_PO-BPUMN = WA_EKPO-BPUMN.
        WA_EINA_PO-PEINH = WA_EKPO-PEINH.
        WA_EINA_PO-UMREN = WA_EKPO-UMREN.
        WA_EINA_PO-UMREZ = WA_EKPO-UMREZ.
      ENDIF.
    ENDIF.
    IF <FS_MSEG>-MEINS NE <FS_MSEG>-ERFME.
      CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
           EXPORTING
                INPUT                = WA_EINA_PO-KBETR
                MATNR                = <FS_MSEG>-MATNR
                MEINH                = <FS_MSEG>-ERFME
                MEINS                = <FS_MSEG>-MEINS
           IMPORTING
                OUTPUT               = WA_EINA_PO-KBETR
           EXCEPTIONS
                CONVERSION_NOT_FOUND = 1
                INPUT_INVALID        = 2
                MATERIAL_NOT_FOUND   = 3
                MEINH_NOT_FOUND      = 4
                MEINS_MISSING        = 5
                NO_MEINH             = 6
                OUTPUT_INVALID       = 7
                OVERFLOW             = 8
                OTHERS               = 9.
      IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    ENDIF.
    WA_ZTMM_6026_01-NETPR = WA_EINA_PO-KBETR.
    WA_ZTMM_6026_01-EFFPR = WA_EINA_PO-EFFPR.

    IF NOT WA_EINA_PO-BPUMN IS INITIAL.
*WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA_PO-BPUMZ )
*                                                     / WA_EINA_PO-BPUMN
*            .
*WA_ZTMM_6026_01-EFFPRUOM = ( WA_ZTMM_6026_01-EFFPR * WA_EINA_PO-BPUMZ )
*                                                     / WA_EINA_PO-BPUMN
*.

WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA_PO-BPUMZ *
                              WA_EINA_PO-UMREN ) / ( WA_EINA_PO-BPUMN *
                                               WA_EINA_PO-UMREZ ).

WA_ZTMM_6026_01-EFFPRUOM = ( WA_ZTMM_6026_01-EFFPR * WA_EINA_PO-BPUMZ *
                              WA_EINA_PO-UMREN ) / ( WA_EINA_PO-BPUMN *
                                               WA_EINA_PO-UMREZ ).

      IF NOT WA_EINA_PO-PEINH IS INITIAL.
        WA_ZTMM_6026_01-NETPRUOM = WA_ZTMM_6026_01-NETPRUOM /
                                   WA_EINA_PO-PEINH.
        WA_ZTMM_6026_01-EFFPRUOM = WA_ZTMM_6026_01-EFFPRUOM /
                                   WA_EINA_PO-PEINH.
      ENDIF.
    ENDIF.

  ENDIF.

  IF WA_EINA_PO-LAND1 IS INITIAL.
    WA_ZTMM_6026_01-LAND1    = WA_EINA_PO-LAND1.
    WA_ZTMM_6026_01-LAND1SRC = 'I'.
  ELSE.
    WA_ZTMM_6026_01-LAND1    = WA_EINA_PO-LAND1.
    WA_ZTMM_6026_01-LAND1SRC = SPACE.
  ENDIF.
  WA_ZTMM_6026_01-VALUE2SRC = C_FTZLINK_SOURCE.
  IF WA_EINA_PO-WAERS IS INITIAL.
    WA_ZTMM_6026_01-WAERSSRC = C_FTZLINK_SOURCE.
  ELSE.
    WA_ZTMM_6026_01-WAERS   = WA_EINA_PO-WAERS.
    WA_ZTMM_6026_01-WAERSSRC = C_ERP_SOURCE..
  ENDIF.
** End of change on 08/11/08

  WA_ZTMM_6026_01-ALTVALUESRC    = 'I'.
  WA_ZTMM_6026_01-ALTVALUE2SRC   = 'I'.
  WA_ZTMM_6026_01-ALTCURRCODESRC = 'I'.
  WA_ZTMM_6026_01-ADVALOREMRATESRC = 'H'.
  WA_ZTMM_6026_01-SPECIFICRATESRC = 'H'.
  WA_ZTMM_6026_01-UOMCONVFACTORSRC = 'I'.
  WA_ZTMM_6026_01-ADDUOMCONVFACSRC = 'I'.
  WA_ZTMM_6026_01-RPTQTYUOMSRC = 'H'.
  WA_ZTMM_6026_01-ADDRPTQTYUOMSRC = 'H'.
  WA_ZTMM_6026_01-DOTINDICATOR     = 'N'.
  WA_ZTMM_6026_01-FCCINDICATOR     = 'N'.
  WA_ZTMM_6026_01-FDAINDICATOR     = 'N'.
  APPEND WA_ZTMM_6026_01 TO IT_ZTMM_6026_01.
  CLEAR: WA_ZTMM_6026_01, WA_EINA, WA_MAT_INFO,WA_ZTBL,
          WA_LFA1, WA_VBPA.
ENDLOOP.
PERFORM SUMMARIZE_TXNCODE.
** Added by Furong on 11/12/07
PERFORM SUMMARIZE_CPPC_CNPC.
** End of addition
PERFORM ADJUST_ANPC_APPC.
PERFORM PROCESS_DATA_BY_SECTION.
IF SY-BATCH IS INITIAL.
  PERFORM DSP_LOG.
ELSE.
  PERFORM LIST_PRICE.
ENDIF.
INCLUDE ZIMMGM29I_6026CLA.
INCLUDE ZIMMGM29I_6026O01.   "PBO Part
INCLUDE ZIMMGM29I_6026I01.   "PAI Part
INCLUDE ZMMIFTZRF01.

*&---------------------------------------------------------------------*
*&      Form  assign_valid_movtypes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_VALID_MOVTYPES.
  PERFORM ADD_BWART USING '101'.
  PERFORM ADD_BWART USING '102'.
  PERFORM ADD_BWART USING '122'.
  PERFORM ADD_BWART USING '123'.
  PERFORM ADD_BWART USING '201'.
  PERFORM ADD_BWART USING '202'.
  PERFORM ADD_BWART USING '551'.
  PERFORM ADD_BWART USING '552'.
  PERFORM ADD_BWART USING '555'.
  PERFORM ADD_BWART USING '556'.
  PERFORM ADD_BWART USING '903'.
  PERFORM ADD_BWART USING '904'.
  PERFORM ADD_BWART USING '907'.
  PERFORM ADD_BWART USING '908'.
  PERFORM ADD_BWART USING '905'.
  PERFORM ADD_BWART USING '906'.
  PERFORM ADD_BWART USING '702'.
  PERFORM ADD_BWART USING '701'.
  PERFORM ADD_BWART USING '712'.
  PERFORM ADD_BWART USING '711'.
  PERFORM ADD_BWART USING '601'.
  PERFORM ADD_BWART USING '602'.
  PERFORM ADD_BWART USING '309'.
  PERFORM ADD_BWART USING '310'.
  PERFORM ADD_BWART USING '991'.

  CLEAR R_BWART.
ENDFORM.                    " assign_valid_movtypes

*&---------------------------------------------------------------------*
*&      Form  add_bwart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0127   text
*----------------------------------------------------------------------*
FORM ADD_BWART USING P_BWART.

  R_BWART-SIGN = 'I'.
  R_BWART-OPTION = 'EQ'.
  R_BWART-LOW = P_BWART.
  APPEND R_BWART.

ENDFORM.                    " add_bwart
*&---------------------------------------------------------------------*
*&      Form  get_bom_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BOM_INFO.

  DATA: WA_STPOX LIKE STPOX.
  DATA: BEGIN OF WA_BOM_COMP,
          MATNR LIKE MARA-MATNR,
          IDNRK LIKE STPOX-IDNRK,
          WERKS LIKE STPOX-WERKS,
*          bwart like mseg-bwart,
          MTART LIKE STPOX-MTART,
          MNGKO LIKE STPOX-MNGKO,
          MEINS LIKE MARA-MEINS,
          MENGE LIKE MSEG-MENGE,
        END OF WA_BOM_COMP.
  DATA: BEGIN OF WA_A018,
           MATNR LIKE A018-MATNR,
           LIFNR LIKE A018-LIFNR,
           KNUMH LIKE A018-KNUMH,
        END OF WA_A018.
  DATA: BEGIN OF WA_KONP,
          KNUMH LIKE KONP-KNUMH,
          KBETR LIKE KONP-KBETR,
        END OF WA_KONP.
  DATA: BEGIN OF WA_HALB_FERT,
          MATNR LIKE MSEG-MATNR,
          WERKS LIKE MSEG-WERKS,
          MENGE LIKE MSEG-MENGE,
          BWART LIKE MSEG-BWART,
        END OF WA_HALB_FERT.
  DATA: BEGIN OF WA_T460A,
          MATNR LIKE MARC-MATNR,
          WERKS LIKE MARC-WERKS,
          WRK02 LIKE T460A-WRK02,
        END OF WA_T460A.

  DATA: IT_STPOX LIKE TABLE OF WA_STPOX,
        IT_BOM_COMP LIKE TABLE OF WA_BOM_COMP,
        IT_A018 LIKE TABLE OF WA_A018,
        IT_KONP LIKE HASHED TABLE OF WA_KONP WITH UNIQUE KEY KNUMH,
        IT_HALB_FERT LIKE TABLE OF WA_HALB_FERT,
        IT_T460A LIKE TABLE OF WA_T460A,
        IT_MSEG1 LIKE TABLE OF WA_MSEG.

  RANGES: R_NSCRAP FOR MSEG-BWART,
          R_BOM_TOPMAT FOR MARA-MATNR.
  DATA: W_LINES TYPE I,
        W_CLEN  TYPE I,
        W_MAX   TYPE I,
        W_FREE  TYPE I,
        W_NO_TIMES TYPE I,
        W_LP_IDX   TYPE I,
        W_FRM_IDX  TYPE I,
        W_TO_IDX   TYPE I,
        W_REM      TYPE I.

  FIELD-SYMBOLS: <FS_COM> LIKE LINE OF IT_STPOX1,
** Changed by Furong on 08/11/08
*                 <FS_EINA> LIKE LINE OF IT_EINA,
                 <FS_EINA_PO> LIKE LINE OF IT_EINA_PO,
** End of change
                 <FS_HALB_EXP> LIKE LINE OF IT_HALB_EXP,
                 <FS_HALB_ENG> LIKE LINE OF IT_HALB_ENG.

*&---We don't need data of 'HALB' materials with Mov.Type '309' & '310'.
  DELETE IT_MSEG WHERE MTART = 'HALB' AND BWART = '309'
                 OR    MTART = 'HALB' AND BWART = '310'
                 OR    ( MTART = 'HALB' AND BWART = '101' AND
                         EBELN IS INITIAL )
                 OR    ( MTART = 'HALB' AND BWART = '102' AND
                         EBELN IS INITIAL ).

  DESCRIBE TABLE IT_MSEG LINES W_LINES.
  IF W_LINES = 0.
  ELSE.
    SELECT EBELN EBELP ZFHBLNO ZFRPTTY ZFVIA LIKP~VBELN
            INTO TABLE IT_ZTBL
                           FROM LIKP
                           INNER JOIN ZTCIVHD
                           ON ZTCIVHD~ZFCIVNO = LIKP~BOLNR
                           INNER JOIN ZTCIVIT
                           ON ZTCIVIT~ZFCIVRN = ZTCIVHD~ZFCIVRN
                           INNER JOIN ZTBL
                           ON ZTBL~ZFBLNO = ZTCIVIT~ZFBLNO
                           FOR ALL ENTRIES IN IT_MSEG
                           WHERE LIKP~VBELN = IT_MSEG-XBLNR
                           AND   EBELN = IT_MSEG-EBELN
                           AND   EBELP = IT_MSEG-EBELP.
  ENDIF.

  SELECT MARA~MATNR WERKS PROFL NTGEW
                    GEWEI STAWN STPRS PEINH MAKTX
                          INTO TABLE IT_MAT_INFO
                          FROM MARA
                          INNER JOIN MARC
                          ON MARC~MATNR = MARA~MATNR
                          INNER JOIN MBEW
                          ON MBEW~MATNR = MARA~MATNR
                          AND MBEW~BWKEY = MARC~WERKS
                          INNER JOIN MAKT
                          ON MAKT~MATNR = MARA~MATNR
                          WHERE ( MTART = 'ROH' AND SPRAS = SY-LANGU )
                          OR    ( MTART = 'ROH1' AND SPRAS = SY-LANGU ).
  IF SY-SUBRC NE 0.
  ENDIF.

  SELECT LIFNR LAND1 FROM LFA1 INTO TABLE IT_LFA1
                     FOR ALL ENTRIES IN IT_MSEG
                     WHERE LIFNR = IT_MSEG-LIFNR.

  SELECT BWART ZZCOD ZZFLG SHKZG FROM ZTFTZ_TCODE
                                 INTO TABLE IT_FTZTCODE.


** Changed by Furong on 07/10/08

* SELECT MATNR EINA~LIFNR EINE~WERKS EKORG WAERS PEINH
*               BPUMZ BPUMN EFFPR LAND1
*                     INTO TABLE IT_EINA
*                     FROM EINA
*                     INNER JOIN EINE
*                     ON EINE~INFNR = EINA~INFNR
*                     AND EINE~LOEKZ = EINA~LOEKZ
*                     INNER JOIN LFA1
*                     ON LFA1~LIFNR = EINA~LIFNR
*                     FOR ALL ENTRIES IN IT_MSEG
*                     WHERE MATNR = IT_MSEG-MATNR
*                     AND   EINA~LIFNR = IT_MSEG-LIFNR
*                     AND   EINA~LOEKZ = SPACE.

*  DESCRIBE TABLE IT_EINA LINES W_LINES.
*  IF W_LINES = 0.
*  ELSE.
*    SELECT MATNR LIFNR KNUMH INTO TABLE IT_A018
*                             FROM A018
*                             FOR ALL ENTRIES IN IT_EINA
*                             WHERE MATNR = IT_EINA-MATNR
*                             AND   LIFNR = IT_EINA-LIFNR
*                             AND   EKORG = IT_EINA-EKORG
*                             AND   DATAB <= P_DATE
*                             AND   DATBI >= P_DATE
*                             AND   KSCHL = 'PB00'.
*  ENDIF.
*  DESCRIBE TABLE IT_A018 LINES W_LINES.
*  IF W_LINES = 0.
*  ELSE.
*    SORT IT_A018 BY KNUMH.
*    SELECT KNUMH KBETR FROM KONP
*                       INTO TABLE IT_KONP
*                       FOR ALL ENTRIES IN IT_A018
*                       WHERE KNUMH = IT_A018-KNUMH
*                       AND   KSCHL = 'PB00'
*                       AND   LOEVM_KO = SPACE.
*  ENDIF.
  SORT: IT_EINA BY MATNR LIFNR,
        IT_A018 BY MATNR LIFNR.

*  CLEAR W_LINES.
*  W_LINES = 1.
*  LOOP AT IT_EINA ASSIGNING <FS_EINA>.
*
*    READ TABLE IT_A018 INTO WA_A018 WITH KEY MATNR = <FS_EINA>-MATNR
*                                             LIFNR = <FS_EINA>-LIFNR
*                                             BINARY SEARCH
*                                             TRANSPORTING KNUMH.
*    IF SY-SUBRC NE 0.
*      <FS_EINA>-KBETR = 0.
*      CONTINUE.
*    ENDIF.
*   READ TABLE IT_KONP INTO WA_KONP WITH TABLE KEY KNUMH = WA_A018-KNUMH
*.
*    IF SY-SUBRC NE 0.
*      <FS_EINA>-KBETR = 0.
*    ELSE.
*      <FS_EINA>-KBETR = WA_KONP-KBETR.
*    ENDIF.
*  ENDLOOP.

  SELECT MATNR EINA~LIFNR EINE~WERKS EKORG WAERS PEINH
                BPUMZ BPUMN EFFPR LAND1
**                EBELN EBELP
                      INTO TABLE IT_EINA_PO
                      FROM EINA
                      INNER JOIN EINE
                      ON EINE~INFNR = EINA~INFNR
** Changed by Furong on 07/31/08
*                      AND EINE~LOEKZ = EINA~LOEKZ
** End of change on 07/31/08
                      INNER JOIN LFA1
                      ON LFA1~LIFNR = EINA~LIFNR
                      FOR ALL ENTRIES IN IT_MSEG
                      WHERE MATNR = IT_MSEG-MATNR
                      AND EINA~LIFNR = IT_MSEG-LIFNR.
** Changed by Furong on 07/31/08
*                      AND   EINA~LOEKZ = SPACE.
** End of change

  DESCRIBE TABLE IT_EINA_PO LINES W_LINES.
  IF W_LINES = 0.
  ELSE.
    SELECT MATNR LIFNR KNUMH INTO TABLE IT_A018
                             FROM A018
                             FOR ALL ENTRIES IN IT_EINA_PO
                             WHERE MATNR = IT_EINA_PO-MATNR
                             AND   LIFNR = IT_EINA_PO-LIFNR
                             AND   EKORG = IT_EINA_PO-EKORG
                             AND   DATAB <= P_DATE
                             AND   DATBI >= P_DATE
                             AND   KSCHL = 'PB00'.

    SELECT EBELN EBELP MATNR WERKS NETPR  BPUMZ BPUMN
     PEINH UMREN UMREZ
                     INTO TABLE IT_EKPO
                     FROM EKPO
                     FOR ALL ENTRIES IN IT_MSEG
                     WHERE EBELN = IT_MSEG-EBELN
                     AND EBELP = IT_MSEG-EBELP
                     AND MATNR = IT_MSEG-MATNR
                     AND LOEKZ = SPACE.

    SORT IT_EKPO BY EBELN EBELP MATNR.
  ENDIF.
  DESCRIBE TABLE IT_A018 LINES W_LINES.
  IF W_LINES = 0.
  ELSE.
    SORT IT_A018 BY KNUMH.
    SELECT KNUMH KBETR FROM KONP
                       INTO TABLE IT_KONP
                       FOR ALL ENTRIES IN IT_A018
                       WHERE KNUMH = IT_A018-KNUMH
                       AND   KSCHL = 'PB00'
                       AND   LOEVM_KO = SPACE.
    SORT: IT_A018 BY MATNR LIFNR.
  ENDIF.

  CLEAR W_LINES.
  W_LINES = 1.
  LOOP AT IT_EINA_PO ASSIGNING <FS_EINA_PO>.
    IF <FS_EINA_PO>-EBELN IS INITIAL.
    READ TABLE IT_A018 INTO WA_A018 WITH KEY MATNR = <FS_EINA_PO>-MATNR
                                             LIFNR = <FS_EINA_PO>-LIFNR
                                                          BINARY SEARCH
                                                     TRANSPORTING KNUMH.
      IF SY-SUBRC NE 0.
        <FS_EINA_PO>-KBETR = 0.
        CONTINUE.
      ENDIF.
   READ TABLE IT_KONP INTO WA_KONP WITH TABLE KEY KNUMH = WA_A018-KNUMH
                                                                     .
      IF SY-SUBRC NE 0.
        <FS_EINA_PO>-KBETR = 0.
      ELSE.
        <FS_EINA_PO>-KBETR = WA_KONP-KBETR.
      ENDIF.

*    ELSE.
*    READ TABLE IT_EKPO INTO WA_EKPO WITH KEY EBELN = <FS_EINA_PO>-EBELN
*                                             EBELP = <FS_EINA_PO>-EBELP
*                                             MATNR = <FS_EINA_PO>-MATNR
*                                                    BINARY SEARCH.
*      IF SY-SUBRC NE 0.
*        <FS_EINA_PO>-KBETR = 0.
*      ELSE.
*        <FS_EINA_PO>-KBETR = WA_EKPO-NETPR.
*      ENDIF.
    ENDIF.
    CLEAR: WA_EKPO, WA_KONP, WA_A018.
  ENDLOOP.
  SORT IT_EINA_PO BY MATNR LIFNR.

** End of change ON 08/10/08

*&---There shouldn't be transactions with following Mov.type for HALB.
  R_NSCRAP-SIGN = 'E'.
  R_NSCRAP-OPTION = 'EQ'.
  R_NSCRAP-LOW = '101'.
  APPEND R_NSCRAP.
  R_NSCRAP-LOW = '102'.
  APPEND R_NSCRAP.
  R_NSCRAP-LOW = '122'.
  APPEND R_NSCRAP.
  R_NSCRAP-LOW = '123'.
  APPEND R_NSCRAP.
  R_NSCRAP-LOW = '309'.
  APPEND R_NSCRAP.
  R_NSCRAP-LOW = '310'.
  APPEND R_NSCRAP.

  R_MTART-SIGN = 'E'.
  R_MTART-OPTION = 'EQ'.
  R_MTART-LOW = 'ROH'.
  APPEND R_MTART.
  R_MTART-LOW = 'ROH1'.
  APPEND R_MTART.
  CLEAR WA_MSEG.
  LOOP AT IT_MSEG ASSIGNING <FS_MSEG>
                  WHERE ( MTART EQ 'HALB' AND BWART IN R_NSCRAP ).
*&-----To delete from it_mseg.
    R_BOM_TOPMAT-SIGN = 'I'.
    R_BOM_TOPMAT-OPTION = 'EQ'.
    R_BOM_TOPMAT-LOW = <FS_MSEG>-MATNR.
    COLLECT R_BOM_TOPMAT.
*&-----For BOM explosion.
    WA_HALB_EXP-MATNR = <FS_MSEG>-MATNR.
    WA_HALB_EXP-WERKS = <FS_MSEG>-WERKS.
    WA_HALB_EXP-DATUV = P_DATE.
    COLLECT WA_HALB_EXP INTO IT_HALB_EXP.
*&-----For a copy.
    WA_MSEG = <FS_MSEG>.
    APPEND WA_MSEG TO IT_MSEG1.
*&----Materials with Mov.Typ '991'.
    IF <FS_MSEG>-BWART = '991'.
      WA_991_MAT-MATNR = <FS_MSEG>-MATNR.
      COLLECT WA_991_MAT INTO IT_991_MAT.
    ENDIF.
    CLEAR: R_BOM_TOPMAT, WA_HALB_EXP, WA_MSEG, WA_991_MAT.
  ENDLOOP.
  WA_HALB_EXP-MENGE = 1.
  MODIFY IT_HALB_EXP FROM WA_HALB_EXP TRANSPORTING MENGE
                           WHERE MENGE IS INITIAL.

*&----Check Special procurement key to get correct plant for BOM.
  SELECT MATNR MARC~WERKS WRK02 INTO TABLE IT_T460A
                                FROM MARC
                                INNER JOIN T460A
                                ON T460A~WERKS = MARC~WERKS
                                AND T460A~SOBSL = MARC~SOBSL
                               FOR ALL ENTRIES IN IT_HALB_EXP
                                WHERE MATNR EQ IT_HALB_EXP-MATNR
                                AND  MARC~WERKS EQ IT_HALB_EXP-WERKS.
  IF SY-SUBRC NE 0.
  ELSE.
    SORT IT_T460A BY MATNR WERKS.
    LOOP AT IT_HALB_EXP ASSIGNING <FS_HALB_EXP>.
      READ TABLE IT_T460A INTO WA_T460A
                           WITH KEY MATNR = <FS_HALB_EXP>-MATNR
                                    WERKS = <FS_HALB_EXP>-WERKS
                                     BINARY SEARCH.
      IF SY-SUBRC NE 0.
      ELSE.
        IF WA_T460A-WRK02 IS INITIAL.
        ELSE.
          <FS_HALB_EXP>-WERKS = WA_T460A-WRK02.
          WA_MSEG-WERKS       = WA_T460A-WRK02.
          MODIFY IT_MSEG FROM WA_MSEG TRANSPORTING WERKS
                                    WHERE MATNR = <FS_HALB_EXP>-MATNR.
          CLEAR: WA_MSEG, WA_T460A.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DO.
    CALL FUNCTION 'SPBT_INITIALIZE'
         EXPORTING
              GROUP_NAME                     = P_SRVGRP
         IMPORTING
              MAX_PBT_WPS                    = W_MAX
              FREE_PBT_WPS                   = W_FREE
         EXCEPTIONS
              INVALID_GROUP_NAME             = 1
              INTERNAL_ERROR                 = 2
              PBT_ENV_ALREADY_INITIALIZED    = 3
              CURRENTLY_NO_RESOURCES_AVAIL   = 4
              NO_PBT_RESOURCES_FOUND         = 5
              CANT_INIT_DIFFERENT_PBT_GROUPS = 6
              OTHERS                         = 7.
    IF SY-SUBRC <> 0 OR W_FREE = 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
  CLEAR W_LINES.
  DESCRIBE TABLE IT_HALB_EXP LINES W_LINES.

  IF W_LINES > W_FREE.
    W_REM       = W_LINES MOD W_FREE.
    W_NO_TIMES  = W_LINES / W_FREE.
    IF W_REM = 0.
    ELSE.
      W_NO_TIMES = W_NO_TIMES + 1.
    ENDIF.
  ELSE.
    W_NO_TIMES = 1.
  ENDIF.
  W_LP_IDX = 1.
  WHILE W_LP_IDX <= W_NO_TIMES.
    IF W_LP_IDX = 1.
      W_FRM_IDX = W_LP_IDX.
    ELSE.
      W_FRM_IDX = W_TO_IDX + 1.
    ENDIF.
    IF W_LINES > W_FREE.
      W_TO_IDX  = W_LP_IDX * W_FREE.
    ELSE.
      W_TO_IDX = W_LINES.
    ENDIF.
    LOOP AT IT_HALB_EXP ASSIGNING <FS_HALB_EXP>
                                  FROM W_FRM_IDX TO W_TO_IDX.
** changed by Furong on 05/04/2006
      DO.
        CALL FUNCTION 'Z_FFTZ_EXP_BOM'
             STARTING NEW TASK W_TASKNAME
             DESTINATION IN GROUP P_SRVGRP
             PERFORMING BOM_EXP ON END OF TASK
             EXPORTING
                P_CAPID = 'PP01'
                P_DATUV = <FS_HALB_EXP>-DATUV
                P_EMENG = <FS_HALB_EXP>-MENGE
                P_MEHRS = 'X'
                P_MMORY = '1'
                P_MTNRV = <FS_HALB_EXP>-MATNR
                P_STLAN = '1'
                P_WERKS = <FS_HALB_EXP>-WERKS
                P_PAMATNR = <FS_HALB_EXP>-MATNR
             TABLES
                P_STPOX = IT_STPOX
             EXCEPTIONS
                COMMUNICATION_FAILURE = 1
                SYSTEM_FAILURE        = 2
                RESOURCE_FAILURE      = 3.
        CASE SY-SUBRC.
          WHEN 0.
            W_TASKNAME = W_TASKNAME + 1.
            W_SND_JOBS = W_SND_JOBS + 1.
            EXIT.
          WHEN 1 OR 2.
            W_EXCEP_FLAG = 'X'.
          WHEN 3.
            IF W_EXCEP_FLAG = SPACE.
              W_EXCEP_FLAG = 'X'.
              WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.01' SECONDS.
            ELSE.
              WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.1' SECONDS.
            ENDIF.
            IF SY-SUBRC EQ 0.
              CLEAR W_EXCEP_FLAG.
            ELSE.
*            exit.
            ENDIF.
        ENDCASE.
      ENDDO.
** end of change
    ENDLOOP.

* Replace WAIT statement for loop

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

    DO.
      WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
      IF W_RCV_JOBS >= W_SND_JOBS.
        EXIT.
      ENDIF.
    ENDDO.
    W_LP_IDX = W_LP_IDX + 1.
  ENDWHILE.

** Change by Furong on 11/02/2007

  CLEAR: W_SND_JOBS, W_RCV_JOBS.
  LOOP AT IT_HALB_ENG ASSIGNING <FS_HALB_ENG>.
*      r_bom_topmat-sign = 'I'.
*      r_bom_topmat-option = 'EQ'.
*      r_bom_topmat-low = <fs_halb_eng>-matnr.
*      COLLECT r_bom_topmat.
*      CLEAR: r_bom_topmat.
*
    DO.
      CALL FUNCTION 'Z_FFTZ_EXP_BOM'
           STARTING NEW TASK W_TASKNAME
           DESTINATION IN GROUP P_SRVGRP
           PERFORMING BOM_EXP ON END OF TASK
           EXPORTING
              P_CAPID = 'PP01'
              P_DATUV = <FS_HALB_ENG>-DATUV
              P_EMENG = <FS_HALB_ENG>-BDMNG
              P_MEHRS = 'X'
              P_MMORY = '1'
              P_MTNRV = <FS_HALB_ENG>-MATNR
              P_STLAN = '1'
              P_WERKS = <FS_HALB_ENG>-WERKS
              P_PAMATNR = <FS_HALB_ENG>-PMATNR
           TABLES
              P_STPOX = IT_STPOX
           EXCEPTIONS
              COMMUNICATION_FAILURE = 1
              SYSTEM_FAILURE        = 2
              RESOURCE_FAILURE      = 3.
      CASE SY-SUBRC.
        WHEN 0.
          W_TASKNAME = W_TASKNAME + 1.
          W_SND_JOBS = W_SND_JOBS + 1.
          EXIT.
        WHEN 1 OR 2.
          W_EXCEP_FLAG = 'X'.
        WHEN 3.
          IF W_EXCEP_FLAG = SPACE.
            W_EXCEP_FLAG = 'X'.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.01' SECONDS.
          ELSE.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.1' SECONDS.
          ENDIF.
          IF SY-SUBRC EQ 0.
            CLEAR W_EXCEP_FLAG.
          ELSE.
*            exit.
          ENDIF.
      ENDCASE.
    ENDDO.
  ENDLOOP.

  DO.
    WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
    IF W_RCV_JOBS >= W_SND_JOBS.
      EXIT.
    ENDIF.
  ENDDO.
** End of change

  CLEAR: R_BOM_TOPMAT.
  CHECK NOT IT_STPOX1[] IS INITIAL.
  DELETE IT_MSEG WHERE MATNR IN R_BOM_TOPMAT.

  LOOP AT IT_STPOX1 ASSIGNING <FS_COM>.
    WA_BOM_COMP-MATNR = <FS_COM>-MATNR.
    WA_BOM_COMP-IDNRK = <FS_COM>-IDNRK.
    WA_BOM_COMP-WERKS = <FS_COM>-WERKS.
*    wa_bom_comp-bwart = <fs_com>-bwart.
    WA_BOM_COMP-MTART = <FS_COM>-MTART.
    WA_BOM_COMP-MENGE = <FS_COM>-MENGE.
    WA_BOM_COMP-MNGKO = <FS_COM>-MNGKO.
    WA_BOM_COMP-MEINS = <FS_COM>-MEINS.
    COLLECT WA_BOM_COMP INTO IT_BOM_COMP.
  ENDLOOP.
  CLEAR: WA_MSEG, WA_BOM_COMP.
  IF <FS_MSEG> IS ASSIGNED.
    UNASSIGN <FS_MSEG>.
  ENDIF.

  LOOP AT IT_MSEG1 ASSIGNING <FS_MSEG>.
    LOOP AT IT_BOM_COMP INTO WA_BOM_COMP
                        WHERE MATNR = <FS_MSEG>-MATNR
                           AND WERKS = <FS_MSEG>-WERKS.
      READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
                             WITH TABLE KEY MATNR = WA_BOM_COMP-IDNRK
                                            WERKS = WA_BOM_COMP-WERKS.
      IF SY-SUBRC NE 0.
        CLEAR WA_MSEG.
        CONTINUE.
      ENDIF.
      WA_MSEG-MATNR = WA_BOM_COMP-IDNRK.
      WA_MSEG-MTART = WA_BOM_COMP-MTART.
      WA_MSEG-MEINS = WA_BOM_COMP-MEINS.
      WA_MSEG-PROFL = WA_MAT_INFO-PROFL.
      WA_MSEG-NTGEW = WA_MAT_INFO-NTGEW.
      WA_MSEG-GEWEI = WA_MAT_INFO-GEWEI.
      WA_MSEG-BWART = <FS_MSEG>-BWART.
      WA_MSEG-WERKS = WA_BOM_COMP-WERKS.
      WA_MSEG-MENGE = <FS_MSEG>-MENGE * WA_BOM_COMP-MNGKO.
      WA_MSEG-UMMAT = SPACE.
      WA_MSEG-MAKTX = WA_MAT_INFO-MAKTX.
      WA_MSEG-MNGKO = WA_BOM_COMP-MENGE.
      WA_MSEG-PMATNR = WA_BOM_COMP-MATNR.
*    read table it_mseg1 assigning <fs_mseg>
*                         with key matnr = wa_bom_comp-matnr
*                                  bwart = wa_bom_comp-bwart.
*    if sy-subrc ne 0.
*      wa_mseg-lifnr = space.
*      wa_mseg-ebeln = space.
*      wa_mseg-ebelp = space.
*      wa_mseg-shkzg = space.
*    else.
      WA_MSEG-LIFNR = <FS_MSEG>-LIFNR.
      WA_MSEG-EBELN = <FS_MSEG>-EBELN.
      WA_MSEG-EBELP = <FS_MSEG>-EBELP.
      WA_MSEG-SHKZG = <FS_MSEG>-SHKZG.
      WA_MSEG-XBLNR = <FS_MSEG>-XBLNR.
      WA_MSEG-XABLN = <FS_MSEG>-XABLN.
      WA_MSEG-KDAUF = <FS_MSEG>-KDAUF.
      WA_MSEG-ERFME = <FS_MSEG>-ERFME.
      WA_MSEG-PPROFL = <FS_MSEG>-PROFL.
*    endif.
      APPEND WA_MSEG TO IT_MSEG.
      CLEAR: WA_MSEG.
    ENDLOOP.
  ENDLOOP.
  FREE: IT_MSEG1.
  IF <FS_MSEG> IS ASSIGNED.
    UNASSIGN <FS_MSEG>.
  ENDIF.
ENDFORM.                    " get_bom_info
*&---------------------------------------------------------------------*
*&      Form  get_tcode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<FS_MSEG>_MTART  text
*      -->P_<FS_MSEG>_BWART  text
*      -->P_<FS_MSEG>_PROFL  text
*      <--P_W_FTZ_TCODE  text
*----------------------------------------------------------------------*
FORM GET_TCODE USING    P_MTART
                        P_BWART
                        P_SHKZG
                        P_PROFL
                        P_XABLN
               CHANGING P_MATNR
                        P_FTZ_TCODE
                        P_ORDNUM_RECP
                        P_ORDNUM_SHIP
                        P_TID
                        P_BILL
                        P_FTZTCODE STRUCTURE WA_FTZTCODE.

  DATA: P_ACT LIKE ZTFTZ_TCODE-ZZFLG.

  CASE P_MTART.
    WHEN 'ROH' OR 'ROH1'.
      READ TABLE IT_FTZTCODE INTO P_FTZTCODE
                             WITH KEY BWART = P_BWART.
      IF SY-SUBRC NE 0.
      ELSE.
        P_FTZ_TCODE = P_FTZTCODE-ZZCOD.
        P_ACT       = P_FTZTCODE-ZZFLG.
        CASE P_BWART.
          WHEN '601'.
            IF P_PROFL EQ 'M'.
              CLEAR: P_FTZ_TCODE,P_ACT.
              P_FTZ_TCODE = 'SPNM'.
              P_ACT       = 'GI'.
            ENDIF.
          WHEN '309' OR '310'.
            IF P_SHKZG EQ 'H'.
              CLEAR: P_FTZ_TCODE,P_ACT.
              P_FTZ_TCODE = 'CNPC'.
              P_ACT       = 'GI'.
            ENDIF.
          WHEN '991'.
            CASE P_XABLN.
              WHEN SPACE.
                P_FTZ_TCODE = 'XPPC'.
                P_ACT       = 'GI'.
              WHEN 'S' OR 'T'.
                P_FTZ_TCODE = 'SPPC'.
                P_ACT       = 'GR'.
            ENDCASE.
        ENDCASE.
        PERFORM ORDERNUMRECEIPT_BY_PROFL USING P_BWART
                                               P_PROFL
                                               P_ACT
                                               P_FTZ_TCODE
                                         CHANGING P_MATNR
                                                  P_ORDNUM_RECP
                                                  P_TID
                                                  P_BILL.

      ENDIF.
  ENDCASE.
ENDFORM.                    " get_tcode
*&---------------------------------------------------------------------*
*&      Form  list_price
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LIST_PRICE.
  SORT IT_LIST_PRICE BY MATNR EBELN.
  DELETE ADJACENT DUPLICATES FROM IT_LIST_PRICE COMPARING MATNR EBELN.
  WRITE : / 'Price List'.
  WRITE: /(18) 'Material Number'.
  WRITE: (10) 'PO number'.
  WRITE: (18) 'Standard Price'.
  WRITE: (15) 'Info Price'.
  WRITE: (15) 'PO Price'.
  WRITE : / SY-ULINE.
  LOOP AT IT_LIST_PRICE.
    WRITE:/(18) IT_LIST_PRICE-MATNR.
    WRITE:(10) IT_LIST_PRICE-EBELN.
    WRITE:(15) IT_LIST_PRICE-STD.
    WRITE:(15) IT_LIST_PRICE-INFO.
    WRITE:(15) IT_LIST_PRICE-PO.
  ENDLOOP.
ENDFORM.                    " list_price
