************************************************************************
* Program Name      : ZRIM_SAP_TO_FTZ
* Author            : Hyunju Na
* Creation Date     : 2004.02.10.
* Description       : SAP MM -> External SYSTEM(FTZ)
*
************************************************************************
REPORT ZRIM_SAP_TO_FTZ MESSAGE-ID ZIM
                 NO STANDARD PAGE HEADING
                 LINE-SIZE 400.

INCLUDE ZRIMFTZTOP.
INCLUDE ZRIMFTZCLASS.

*-----------------------------------------------------------------------
* Selection Screen ?
*-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_DATUM    FOR SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM P1000_INIT_DATA_SET.

START-OF-SELECTION.

  PERFORM P1000_GET_DATA.

  IF IT_ZTMM_6026_01 IS INITIAL.
     MESSAGE S977  WITH 'There is no data!'.
     EXIT.
  ENDIF.
  PERFORM P1000_PROCESS_DATA.
  PERFORM P1000_EAI_INTERFACE_LOG  USING W_LINE  LV_ZRESULT
                                         W_LINE  W_LINE.

  IF SY-BATCH IS INITIAL.   "Not Backgroung Processing
     PERFORM P1000_DISPLAY_LOG.
  ELSE.
    MESSAGE S999(ZMMM) WITH 'Application Doc. No.'
                            W_ZDOCNO
                            'is created !'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  P1000_GET_DATA
*&---------------------------------------------------------------------*
FORM P1000_GET_DATA.

  ">> 1. Standard Data Select.
  CLEAR: IT_ZTMM_6026_01.
  REFRESH : IT_SEL_DATA, IT_ZTMM_6026_01, IT_ZSGRMT_TEMP.
  CLEAR   : IT_SEL_DATA.

  SELECT A~EBELN A~EBELP A~MATNR          A~MENGE
         A~DMBTR A~WAERS C~VBELV AS XBLNR B~BUDAT
  INTO   CORRESPONDING FIELDS OF TABLE IT_SEL_DATA
  FROM ( MSEG AS A   INNER JOIN VBFA AS C
  ON     A~MBLNR      EQ   C~VBELN
  AND    A~LINE_ID    EQ   C~POSNN        )
  INNER  JOIN MKPF  AS B
  ON     A~MBLNR      EQ   B~MBLNR
  WHERE  B~BUDAT      IN   S_DATUM
  AND    A~BWART      EQ   '101'
  AND    A~SHKZG      EQ   'S'
  AND    C~VBTYP_N    EQ   'R'
  AND    C~VBTYP_V    EQ   '7'
  AND    C~BWART      EQ   '101'
  AND    C~PLMIN      EQ   '+'.

  DESCRIBE TABLE  IT_SEL_DATA LINES W_LINE.
  IF W_LINE LE 0.
     PERFORM P1000_EAI_INTERFACE_LOG  USING W_LINE 'E' W_LINE W_LINE.
     EXIT.
  ENDIF.

  CONCATENATE  SY-DATUM 'T' SY-UZEIT INTO W_TRCL.

  " GR Data -> Interface Data Conversion.
  LOOP AT IT_SEL_DATA.

     " Inbound Delivery Get.
     SELECT SINGLE * FROM LIKP
     WHERE  VBELN    EQ   IT_SEL_DATA-XBLNR.

     " P/O, P/O Item Data
     SELECT SINGLE * FROM EKPO
     WHERE  EBELN    EQ   IT_SEL_DATA-EBELN
     AND    EBELP    EQ   IT_SEL_DATA-EBELP.

     " B/L Data Get.
     SELECT SINGLE B~ZFBLNO  INTO  W_ZFBLNO
     FROM   ZTCIVHD AS  A  INNER  JOIN  ZTCIVIT AS  B
     ON     A~ZFCIVRN      EQ     B~ZFCIVRN
     WHERE  ZFCIVNO        EQ     LIKP-BOLNR
     AND    B~EBELN        EQ     IT_SEL_DATA-EBELN
     AND    B~EBELP        EQ     IT_SEL_DATA-EBELP.

     IF SY-SUBRC NE 0.    CONTINUE.   ENDIF.

     " B/L Data Get
     SELECT SINGLE * FROM ZTBL WHERE ZFBLNO EQ W_ZFBLNO.

     " Transportation Mode
     IF ZTBL-ZFVIA EQ 'VSL' .
        MOVE   'O'   TO  W_TRANS_MODE.
     ELSEIF ZTBL-ZFVIA EQ 'AIR'.
        MOVE   'A'   TO  W_TRANS_MODE.
     ENDIF.

     CLEAR : IT_ZSGRMT, IT_ZSGRHS.

     CLEAR : MAKT, MARA.
     SELECT SINGLE * FROM MAKT
     WHERE  MATNR    EQ   IT_SEL_DATA-MATNR
     AND    SPRAS    EQ   SY-LANGU.

     SELECT SINGLE * FROM MARA
     WHERE  MATNR    EQ   IT_SEL_DATA-MATNR.

     CONCATENATE  IT_SEL_DATA-BUDAT 'T' '000000' INTO W_BUDT.

     " Material Data Set
     MOVE : '100300'            TO IT_ZSGRMT_TEMP-PARTID,   "Partner ID
            W_TRCL              TO IT_ZSGRMT_TEMP-WRTIME,   "Write Time
            'RPPC'              TO IT_ZSGRMT_TEMP-TCODE,    "Tras. Code
            W_BUDT              TO IT_ZSGRMT_TEMP-TRTIME,   "Trans.Time
            IT_SEL_DATA-EBELN   TO IT_ZSGRMT_TEMP-ORNUMRC,  "Received.
            IT_SEL_DATA-MATNR   TO IT_ZSGRMT_TEMP-PARTNUM,  "Part No
            'PC'                TO IT_ZSGRMT_TEMP-PRDTYPE,  "Product
            MAKT-MAKTX          TO IT_ZSGRMT_TEMP-PARTDESC, "Part Desc
            'I'                 TO IT_ZSGRMT_TEMP-NAFLAG,   "NAFTA Flag
            IT_SEL_DATA-MENGE   TO IT_ZSGRMT_TEMP-TXNQTY,   "GR Quantity
            EKPO-MEINS          TO IT_ZSGRMT_TEMP-TXNUNIT,  "Unit
            MARA-NTGEW          TO IT_ZSGRMT_TEMP-WEIGHT,   "Weight
            'KG'                TO IT_ZSGRMT_TEMP-WEIUNIT,  "Weight Unit
            ZTBL-ZFCARC         TO IT_ZSGRMT_TEMP-SHCOUNTRY,"Shipment
            ZTBL-ZFHBLNO        TO IT_ZSGRMT_TEMP-TRANSID,  "Transport
            W_TRANS_MODE        TO IT_ZSGRMT_TEMP-MODTP,    "Mode
            ZTBL-ZFHBLNO        TO IT_ZSGRMT_TEMP-BILL,     "BL
            'N'                 TO IT_ZSGRMT_TEMP-VAFLAG,   "Valid Flag
            'N'                 TO IT_ZSGRMT_TEMP-ASFLAG,   "Assignment
            'N'                 TO IT_ZSGRMT_TEMP-FIFOFLAG, "FIFO Flag
            'N'                 TO IT_ZSGRMT_TEMP-DELETE,   "Delete
            'N'                 TO IT_ZSGRMT_TEMP-KEEPRL.
     APPEND  IT_ZSGRMT_TEMP.

  ENDLOOP.

  ">> B/L , P/O Group.
  SORT IT_ZSGRMT_TEMP BY ORNUMRC PARTNUM BILL TRTIME.
  LOOP AT IT_ZSGRMT_TEMP.

     IF SV_EBELN    NE  IT_ZSGRMT_TEMP-ORNUMRC OR
        SV_MATNR    NE  IT_ZSGRMT_TEMP-PARTNUM OR
        SV_HBLNO    NE  IT_ZSGRMT_TEMP-BILL    OR
        SV_TRTIME   NE  IT_ZSGRMT_TEMP-TRTIME .
        IF SY-TABIX NE  1.
           MOVE : SV_MENGE  TO  IT_ZTMM_6026_01-MENGE.
           APPEND  IT_ZTMM_6026_01.
           CLEAR : IT_ZTMM_6026_01, SV_MENGE.
        ENDIF.
        MOVE : IT_ZSGRMT_TEMP-ORNUMRC(10)  TO  SV_EBELN,
               IT_ZSGRMT_TEMP-PARTNUM(17)  TO  SV_MATNR,
               IT_ZSGRMT_TEMP-BILL         TO  SV_HBLNO,
               IT_ZSGRMT_TEMP-TRTIME       TO  SV_TRTIME.
     ENDIF.

     PERFORM  P2000_SET_DATA.

     SV_MENGE   =   SV_MENGE  +  IT_ZSGRMT_TEMP-TXNQTY.
     AT LAST.
        MOVE : SV_MENGE  TO  IT_ZTMM_6026_01-MENGE.
        APPEND  IT_ZTMM_6026_01.
     ENDAT.
  ENDLOOP.

  L_RATE = 25 / 10.

  ">> HS Data of material SET.
  LOOP AT IT_ZTMM_6026_01.

     W_TABIX     =  SY-TABIX.
     W_STRLEN    =  STRLEN( IT_ZTMM_6026_01-ORDERNUMRECEIPT ).
     W_COLOR_LEN =  W_STRLEN - 10.

     IF W_COLOR_LEN  GT 0.
        CONCATENATE  IT_ZTMM_6026_01-MATNR
                     IT_ZTMM_6026_01-ORDERNUMRECEIPT+10(W_COLOR_LEN)
                     INTO  W_MATNR.
     ELSE.
        MOVE  IT_ZTMM_6026_01-MATNR  TO  W_MATNR.
     ENDIF.

     CLEAR : EKPO.
     SELECT SINGLE * FROM EKPO
     WHERE  EBELN    EQ   IT_ZTMM_6026_01-ORDERNUMRECEIPT(10)
     AND    MATNR    EQ   W_MATNR.

     " HS Data Set.
     CLEAR : MARC.
     SELECT SINGLE * FROM MARC
     WHERE  MATNR    EQ   EKPO-MATNR
     AND    WERKS    EQ   EKPO-WERKS.

    " Tarrif Rate Get.
    CLEAR : A902, KONP.
    SELECT SINGLE * FROM A902
    WHERE  KSCHL    EQ   'ZOA1'
    AND    STAWN    EQ   MARC-STAWN.

    SELECT SINGLE * FROM KONP
    WHERE  KNUMH    EQ   A902-KNUMH
    AND    KSCHL    EQ   'ZOA1'.

    ">> Net Price Change
    IF EKPO-BPUMN IS INITIAL.  EKPO-BPUMN = 1.  ENDIF.
    IT_ZTMM_6026_01-NETPRUOM  = ( EKPO-NETPR / EKPO-PEINH ) *
                                ( EKPO-BPUMZ / EKPO-BPUMN ).

    ">> Base Unit Quanity Get.
    CALL FUNCTION 'CF_UT_UNIT_CONVERSION'
         EXPORTING
              UNIT_NEW_IMP  = EKPO-LMEIN
              UNIT_OLD_IMP  = IT_ZTMM_6026_01-MEINS
              VALUE_OLD_IMP = 1
         IMPORTING
              VALUE_NEW_EXP = NEW_LFIMG
         EXCEPTIONS
              OVERFLOW      = 1
              OTHERS        = 2.
    IT_ZTMM_6026_01-MENGE = IT_ZTMM_6026_01-MENGE * NEW_LFIMG.

    ">> B/L Data Get.
    SELECT SINGLE A~ZFRPTTY  INTO  W_ZFRPTTY
    FROM   ZTBL AS A  INNER JOIN ZTBLIT AS B
    ON     A~ZFBLNO   EQ    B~ZFBLNO
    WHERE  A~ZFHBLNO  EQ    IT_ZTMM_6026_01-BILLOFLADING
    AND    B~EBELN    EQ    EKPO-EBELN
    AND    B~EBELP    EQ    EKPO-EBELP.

    IF W_ZFRPTTY NE 'F'.
       W_STATUS  =  'I'.
       CLEAR : W_STATUS_SRC.
    ELSE.
       CLEAR : W_STATUS.
       W_STATUS_SRC  =  'I'.
    ENDIF.

    MOVE : 'H'                 TO IT_ZTMM_6026_01-STAWNSRC,
           W_STATUS            TO IT_ZTMM_6026_01-STATUSCODE,
           W_STATUS_SRC        TO IT_ZTMM_6026_01-STATUSCODESRC,
           'H'                 TO IT_ZTMM_6026_01-SPICODE1SRC,
           'H'                 TO IT_ZTMM_6026_01-SPICODE2SRC,
           IT_ZTMM_6026_01-COUNTRYSHIPTO TO IT_ZTMM_6026_01-LAND1,
           'I'                 TO IT_ZTMM_6026_01-LIFNRSRC,
           'M'                 TO IT_ZTMM_6026_01-RELFLAGSRC,
           'I'                 TO IT_ZTMM_6026_01-HTSINDEXSRC,
           'H'                 TO IT_ZTMM_6026_01-HTSDESCSRC,
           'I'                 TO IT_ZTMM_6026_01-HTSNUM2SRC,
           EKPO-NETPR          TO IT_ZTMM_6026_01-NETPR,
           'I'                 TO IT_ZTMM_6026_01-VALUE2SRC,
           'USD'               TO IT_ZTMM_6026_01-WAERS,
           'I'                 TO IT_ZTMM_6026_01-ALTVALUESRC,
           'I'                 TO IT_ZTMM_6026_01-ALTVALUE2SRC,
           'I'                 TO IT_ZTMM_6026_01-ALTCURRCODESRC,
           'H'                 TO IT_ZTMM_6026_01-ADVALOREMRATESRC,
            0                  TO IT_ZTMM_6026_01-SPECIFICRATE,
           'H'                 TO IT_ZTMM_6026_01-SPECIFICRATESRC,
           'I'                 TO IT_ZTMM_6026_01-UOMCONVFACTORSRC,
           'I'                 TO IT_ZTMM_6026_01-ADDUOMCONVFACSRC,
           'H'                 TO IT_ZTMM_6026_01-RPTQTYUOMSRC,
           'H'                 TO IT_ZTMM_6026_01-ADDRPTQTYUOMSRC.
    MODIFY IT_ZTMM_6026_01 INDEX W_TABIX.

  ENDLOOP.

ENDFORM.                    " P1000_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  P1000_EAI_INTERFACE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P1000_EAI_INTERFACE_LOG  USING  P_TOTAL     P_RESULT
                                     P_SUCCESS   P_FAIL.

  CLEAR: WA_ZTCA_IF_LOG.

  WA_ZTCA_IF_LOG-TCODE = 'ZIM56'.    "Transaction Code
  WA_ZTCA_IF_LOG-TOTAL = P_TOTAL.    "Total Execution number
  WA_ZTCA_IF_LOG-ZSUCC = P_SUCCESS.  "Success Number
  WA_ZTCA_IF_LOG-ERROR = P_FAIL.     "Fail Number
  WA_ZTCA_IF_LOG-SFLAG = P_RESULT.   "Result.
  WA_ZTCA_IF_LOG-ERDAT = SY-DATUM.   "Created on.
  WA_ZTCA_IF_LOG-ERZET = SY-UNAME.   "Created time.
  WA_ZTCA_IF_LOG-ERNAM = SY-UNAME.   "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
       EXPORTING
            I_ZTCA_IF_LOG        = WA_ZTCA_IF_LOG
       EXCEPTIONS
            UPDATE_FAILED        = 1
            NUMBER_RANGE_ERROR   = 2
            TCODE_DOES_NOT_EXIST = 3
            OTHERS               = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " P1000_EAI_INTERFACE_LOG

*&---------------------------------------------------------------------*
*&      Form  P1000_PROCESS_DATA
*&---------------------------------------------------------------------*
FORM P1000_PROCESS_DATA.

  DATA: LV_LOGNO_H TYPE NUM10.
  DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
  DATA: LV_MESSAGE TYPE BAPI_MSG. "Message text (220)
  CONSTANTS : C_DEST(10) VALUE 'WMGM01'.

*/ Call Outbound RFC FM
  CALL FUNCTION 'Z_FMM_6026_OUT_MATFIFO'
    DESTINATION              C_DEST
    TABLES
      EXT_ZTMM_6026_01      = IT_ZTMM_6026_01
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE LV_MESSAGE
      SYSTEM_FAILURE        = 2 MESSAGE LV_MESSAGE.
  IF SY-SUBRC NE 0.
    LV_ZRESULT = 'E'.  "Result of the Processing
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ELSE.
    LV_ZRESULT = 'S'.  "Result of the Processing
    LV_MESSAGE = 'Outbound RFC FM Succeeded!'(002).
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ENDIF.

  W_DATE =  SY-DATUM.
  W_TIME =  SY-UZEIT.

  PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09
                                   'ZMMNRO0002'
                          CHANGING W_ZDOCNO.
  COMMIT WORK.
  LOOP AT IT_ZTMM_6026_01.

    W_TABIX  =  SY-TABIX.
    PERFORM NUMBER_GET_NEXT USING    '00'
                                     'ZMMNRO0002'
                            CHANGING LV_LOGNO_H.

    IT_ZTMM_6026_01-ZDOCNO  = W_ZDOCNO.
    IT_ZTMM_6026_01-LOGNO_H = LV_LOGNO_H.

    IT_ZTMM_6026_01-ZUSER   = SY-UNAME.
    IT_ZTMM_6026_01-ZEDAT   = W_DATE.
    IT_ZTMM_6026_01-ZETIM   = W_TIME.
    IT_ZTMM_6026_01-ZMODE   = 'C'.
    IT_ZTMM_6026_01-ZRESULT = LV_ZRESULT.
    IT_ZTMM_6026_01-ZMSG    = LV_MESSAGE.
    MODIFY  IT_ZTMM_6026_01  INDEX  W_TABIX.

  ENDLOOP.

*/ Logging to it_ztmm_6026_01.
  INSERT ZTMM_6026_01 FROM TABLE IT_ZTMM_6026_01.


ENDFORM.                    " P1000_PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  get_number_next
*&---------------------------------------------------------------------*
FORM GET_NUMBER_NEXT USING    P_GUBUN
                              P_DOCNO.

  CALL FUNCTION 'ZIM_NUMBER_GET_NEXT'
       EXPORTING
            ZFREQTY         = P_GUBUN
       IMPORTING
            ZFREQNO         = P_DOCNO
       EXCEPTIONS
            NOT_INPUT       = 1
            NOT_TYPE        = 2
            NOT_RANGE       = 3
            NOT_FOUND       = 4
            LOCKED          = 6
            ERROR_DUPLICATE = 8.

ENDFORM.                    " GET_NUMBER_NEXT
*&---------------------------------------------------------------------*
*&      Form  P1000_DISPLAY_LOG
*&---------------------------------------------------------------------*
FORM P1000_DISPLAY_LOG.

  PERFORM P2000_DISPLAY_TAB_SET.
  CALL SCREEN 0100.

ENDFORM.                    " P1000_DISPLAY_LOG
*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.

  IF W_TITLE IS INITIAL.
    W_TITLE = 'Display Data Processing Log'.
  ENDIF.

  CREATE OBJECT CRV_PS
    EXPORTING IM_PS      = 'PS'                "PF-STATUS
              IM_IT_FUNC = IT_FUNC             "Excluding func
              IM_TB      = 'TB'                "TITLEBAR
              IM_TITLE   = W_TITLE.            "TITLE
  CLEAR IT_FUNC.

ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PBO_0100 OUTPUT.

  CC_NAME = 'CC_0100'.
  IF CRV_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT CRV_CUSTOM_CONTAINER
      EXPORTING CONTAINER_NAME = CC_NAME.

    CREATE OBJECT CRV_ALV_GRID
      EXPORTING I_PARENT = CRV_CUSTOM_CONTAINER.

* Set a titlebar for the grid control
    WA_LAYOUT-GRID_TITLE = 'Display Data Processing Log'.

* Set column header
    PERFORM MASK_COLUMNS TABLES IT_FIELDCAT.

* Show ALV Control
    CALL METHOD CRV_ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        I_STRUCTURE_NAME              = 'ZTMM_6026_01'
        IS_LAYOUT                     = WA_LAYOUT   "Title
      CHANGING
        IT_OUTTAB                     = IT_ZTMM_6026_01_OUT
        IT_FIELDCATALOG               = IT_FIELDCAT[]
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4.
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CALL METHOD CRV_ALV_GRID->REFRESH_TABLE_DISPLAY
      EXPORTING
        I_SOFT_REFRESH =  'X'
      EXCEPTIONS
        FINISHED       = 1
        OTHERS         = 2.
    IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDMODULE.                 " PBO_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  MASK_COLUMNS
*&---------------------------------------------------------------------*
*       text
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
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'TXNCODE'.
      P_IT_FIELDCAT-COLTEXT = 'TxnCode'.

    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ORDERNUMRECEIPT'.
      P_IT_FIELDCAT-COLTEXT = 'OrderNumReceipt'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPICODE1SRC'.
      P_IT_FIELDCAT-COLTEXT = 'SpiCode1Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPICODE2SRC'.
      P_IT_FIELDCAT-COLTEXT = 'SpiCode2Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RELFLAGSRC'.
      P_IT_FIELDCAT-COLTEXT = 'RelationshipFlagSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSINDEXSRC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSIndexSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSDESCSRC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSDescSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'HTSNUM2SRC'.
      P_IT_FIELDCAT-COLTEXT = 'HTSNum2Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'VALUE2SRC'.
      P_IT_FIELDCAT-COLTEXT = 'Value2Source'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ALTVALUESRC'.
      P_IT_FIELDCAT-COLTEXT = 'AltValueSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADVALOREMRATESRC'.
      P_IT_FIELDCAT-COLTEXT = 'AdValoremRateSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'SPECIFICRATESRC'.
      P_IT_FIELDCAT-COLTEXT = 'SpecificRateSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'UOMCONVFACTORSRC'.
      P_IT_FIELDCAT-COLTEXT = 'UomConvFactorSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADDUOMCONVFACSRC'.
      P_IT_FIELDCAT-COLTEXT = 'AddUomConvFactorSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'RPTQTYUOMSRC'.
      P_IT_FIELDCAT-COLTEXT = 'RptQtyUomSrc'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ADDRPTQTYUOMSRC'.
      P_IT_FIELDCAT-COLTEXT = 'AddRptQtyUomSource'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MATNR'.
      P_IT_FIELDCAT-OUTPUTLEN = 18.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'DATE_TIME'.
      P_IT_FIELDCAT-COLTEXT = 'Txn Date'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PTC'.
      P_IT_FIELDCAT-COLTEXT = 'ProductTypeCode'.
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
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'DOTINDICATOR'.
      P_IT_FIELDCAT-COLTEXT = 'DotIndicator'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'FCCINDICATOR'.
      P_IT_FIELDCAT-COLTEXT = 'FccIndicator'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'FDAINDICATOR'.
      P_IT_FIELDCAT-COLTEXT = 'FdaIndicator'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'INFNR'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
*    ELSEIF p_it_fieldcat-fieldname = 'LIFNR'.
*      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_it_fieldcat-fieldname = 'LAND1'.
*      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_it_fieldcat-fieldname = 'NETPR'.
*      p_it_fieldcat-no_out = 'X'.
*    ELSEIF p_it_fieldcat-fieldname = 'WAERS'.
*      p_it_fieldcat-no_out = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'TRANSPORTID'.
      P_IT_FIELDCAT-COLTEXT = 'TransportID'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'MODEOFTRANSPORT'.
      P_IT_FIELDCAT-COLTEXT = 'ModeOfTransport'.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY P_IT_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.

  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SY-DYNNR.
    WHEN 0100.
      CASE SAVE_OK_CODE.
        WHEN 'EXIT'.
          LEAVE PROGRAM.
        WHEN 'CANC'.
          LEAVE PROGRAM.
      ENDCASE.
  ENDCASE.

ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  BACK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BACK INPUT.

  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CHECK SAVE_OK_CODE = 'BACK'.
  CASE SY-DYNNR.
    WHEN 0100.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.                 " BACK  INPUT
*&---------------------------------------------------------------------*
*&      Form  P2000_DISPLAY_TAB_SET
*&---------------------------------------------------------------------*
FORM P2000_DISPLAY_TAB_SET.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTMM_6026_01_OUT
  FROM   ZTMM_6026_01
  WHERE  ZEDAT      EQ  W_DATE
  AND    ZETIM      EQ  W_TIME.

ENDFORM.                    " P2000_DISPLAY_TAB_SET
*&---------------------------------------------------------------------*
*&      Form  P2000_SET_DATA
*&---------------------------------------------------------------------*
FORM P2000_SET_DATA.

  CLEAR : W_STRLEN, W_COLOR_LEN.

  W_STRLEN     =  STRLEN( IT_ZSGRMT_TEMP-PARTNUM ).
  IF W_STRLEN GE 12.
     W_COLOR_LEN  =  W_STRLEN - 10.
     CONCATENATE IT_ZSGRMT_TEMP-ORNUMRC
                 IT_ZSGRMT_TEMP-PARTNUM+10(W_COLOR_LEN)
                 INTO IT_ZTMM_6026_01-ORDERNUMRECEIPT.
     MOVE IT_ZSGRMT_TEMP-PARTNUM(10) TO  IT_ZTMM_6026_01-MATNR.
  ELSE.
     MOVE : IT_ZSGRMT_TEMP-ORNUMRC  TO  IT_ZTMM_6026_01-ORDERNUMRECEIPT,
            IT_ZSGRMT_TEMP-PARTNUM  TO  IT_ZTMM_6026_01-MATNR.
  ENDIF.

  MOVE :IT_ZSGRMT_TEMP-PARTID      TO  IT_ZTMM_6026_01-PARTNERID,
        IT_ZSGRMT_TEMP-WRTIME      TO  IT_ZTMM_6026_01-EFFDATE,
        IT_ZSGRMT_TEMP-TCODE       TO  IT_ZTMM_6026_01-TXNCODE,
        IT_ZSGRMT_TEMP-TRTIME      TO  IT_ZTMM_6026_01-TXNDATE,
        IT_ZSGRMT_TEMP-ORNUMWK     TO  IT_ZTMM_6026_01-ORDERNUMWORK,
        IT_ZSGRMT_TEMP-ORNUMSH     TO  IT_ZTMM_6026_01-ORDERNUMSHIP,
        IT_ZSGRMT_TEMP-PRDTYPE     TO  IT_ZTMM_6026_01-PTC,
        IT_ZSGRMT_TEMP-PRFLAG      TO  IT_ZTMM_6026_01-PTCSRC,
        IT_ZSGRMT_TEMP-PARTDESC    TO  IT_ZTMM_6026_01-MAKTX,
        IT_ZSGRMT_TEMP-PDFLAG      TO  IT_ZTMM_6026_01-MAKTXSRC,
        IT_ZSGRMT_TEMP-NAFTA       TO  IT_ZTMM_6026_01-NAFTACERTIFIED,
        IT_ZSGRMT_TEMP-NAFLAG      TO  IT_ZTMM_6026_01-NAFTACERTIFIEDSC,
        IT_ZSGRMT_TEMP-TXNQTY      TO  IT_ZTMM_6026_01-MENGE,
        IT_ZSGRMT_TEMP-TXNUNIT     TO  IT_ZTMM_6026_01-MEINS,
        IT_ZSGRMT_TEMP-TXFLAG      TO  IT_ZTMM_6026_01-MEINSSRC,
        IT_ZSGRMT_TEMP-QTYPER      TO  IT_ZTMM_6026_01-QTYPERLM,
        IT_ZSGRMT_TEMP-WEIGHT      TO  IT_ZTMM_6026_01-NTGEW,
        IT_ZSGRMT_TEMP-WEFLAG      TO  IT_ZTMM_6026_01-NTGEWSRC,
        IT_ZSGRMT_TEMP-WEIUNIT     TO  IT_ZTMM_6026_01-GEWEI,
        IT_ZSGRMT_TEMP-WEUFLAG     TO  IT_ZTMM_6026_01-GEWEISRC,
        IT_ZSGRMT_TEMP-SHCOUNTRY   TO  IT_ZTMM_6026_01-COUNTRYSHIPTO,
        IT_ZSGRMT_TEMP-TRANSID     TO  IT_ZTMM_6026_01-TRANSPORTID,
        IT_ZSGRMT_TEMP-RCDOCID     TO  IT_ZTMM_6026_01-RECEIPTDOCID,
        IT_ZSGRMT_TEMP-EXDOCID     TO  IT_ZTMM_6026_01-EXITDOCID,
        IT_ZSGRMT_TEMP-ADRCDOC     TO  IT_ZTMM_6026_01-ADJRECEIPTDOCID,
        IT_ZSGRMT_TEMP-ADPRDNUM    TO  IT_ZTMM_6026_01-ADJPRODUCTNUM,
        IT_ZSGRMT_TEMP-FRZONEID    TO  IT_ZTMM_6026_01-FROMZONEID,
        IT_ZSGRMT_TEMP-TOZONEID    TO  IT_ZTMM_6026_01-TOZONEID,
        IT_ZSGRMT_TEMP-MODTP       TO  IT_ZTMM_6026_01-MODEOFTRANSPORT,
        IT_ZSGRMT_TEMP-RCTIME      TO  IT_ZTMM_6026_01-RECEIPTDATE,
        IT_ZSGRMT_TEMP-ITNUM       TO  IT_ZTMM_6026_01-ITNUM,
        IT_ZSGRMT_TEMP-BILL        TO  IT_ZTMM_6026_01-BILLOFLADING,
        IT_ZSGRMT_TEMP-EXTIME      TO  IT_ZTMM_6026_01-EXPORTDATE,
        IT_ZSGRMT_TEMP-MANIQTY     TO  IT_ZTMM_6026_01-MANIFESTQTY,
        IT_ZSGRMT_TEMP-VAFLAG      TO  IT_ZTMM_6026_01-VALIDFLAG,
        IT_ZSGRMT_TEMP-ASFLAG      TO  IT_ZTMM_6026_01-ASSIGNMENTFLAG,
        IT_ZSGRMT_TEMP-FIFOFLAG    TO  IT_ZTMM_6026_01-FIFOFLAG,
        IT_ZSGRMT_TEMP-DELETE      TO  IT_ZTMM_6026_01-DELETEDFLAG,
        IT_ZSGRMT_TEMP-KEEPRL      TO  IT_ZTMM_6026_01-KEEPDURINGROLLBA.

ENDFORM.                    " P2000_SET_DATA

*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
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
*&      Form  P1000_INIT_DATA_SET
*&---------------------------------------------------------------------*
FORM P1000_INIT_DATA_SET.

  S_DATUM-LOW    = SY-DATUM - 1.
  S_DATUM-HIGH   = SY-DATUM - 1.
  S_DATUM-SIGN   = 'I'.
  S_DATUM-OPTION = 'BT'.
  APPEND S_DATUM.

ENDFORM.                    " P1000_INIT_DATA_SET
