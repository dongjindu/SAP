FUNCTION ZIM_CHARGE_DOCUMENT_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BUKRS) LIKE  ZTBKPF-BUKRS
*"     VALUE(BELNR) LIKE  ZTBKPF-BELNR
*"     VALUE(GJAHR) LIKE  ZTBKPF-GJAHR
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  EXPORTING
*"     VALUE(INVOICEDOCNUMBER) TYPE  BAPI_INCINV_FLD-INV_DOC_NO
*"     VALUE(FISCALYEAR) TYPE  BAPI_INCINV_FLD-FISC_YEAR
*"  TABLES
*"      IT_FI_DOC STRUCTURE  BAPI_INCINV_FLD OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2
*"      IT_EBELN STRUCTURE  EKKO OPTIONAL
*"  EXCEPTIONS
*"      POST_ERROR
*"----------------------------------------------------------------------
*& Date         Developer          Request        Description
*& 08/23/06     Manju              UD1K921861     Calculate Withholding
*&                                                tax while creating
*&                                                invoice
*-----------------------------------------------------------------------
DATA :W_KUFIX        LIKE   EKKO-KUFIX,
      L_WAERS        LIKE   ZTBKPF-WAERS,
      L_SUBRC        LIKE   SY-SUBRC,
      W_GSBER        LIKE   ZTBDIV-GSBER,
      W_WRBTR        LIKE   ZTBKPF-WRBTR,
      L_ZFPOSYN      LIKE   ZTBKPF-ZFPOSYN,
      L_MENGE(17)    TYPE   C,
      TEMP_DOCNO     LIKE   BAPI_INCINV_FLD-INV_DOC_NO,
      TEMP_YEAR      LIKE   BAPI_INCINV_FLD-FISC_YEAR,
      FI_DOC_NO      LIKE   BAPI_INCINV_FLD-INV_DOC_NO,
      FISC_YEAR      LIKE   BAPI_INCINV_FLD-FISC_YEAR.

*>---------------------------------------------------------------
*> Apply of note
*>---------------------------------------------------------------
DATA: CTU_PARAMS LIKE CTU_PARAMS.
    CTU_PARAMS-DISMODE  = MODE.
    CTU_PARAMS-UPDMODE  = 'V'.
    CTU_PARAMS-CATTMODE = ' '.
    CTU_PARAMS-DEFSIZE  = ' '.
    CTU_PARAMS-RACOMMIT = 'X'.
    CTU_PARAMS-NOBINPT  = 'X'.
    CTU_PARAMS-NOBIEND  = 'X'.
*>---------------------------------------------------------------

DATA  AA_CHK(1).         ">ACCOUNT ASSIGNMENT CHECK.
DATA  INS_CHK(1).        ">Insuarance CONDITION CHECK.

  CLEAR : INVOICEDOCNUMBER, FISCALYEAR, *ZTBKPF,
          HEADERDATA, ITEMDATA,
          TAXDATA, WITHTAXDATA, "UD1K921861
          ZTIMIMG00,
          RETURN,
          ZTREQHD, ZTBL, I_INVOICE, I_CREDITMEMO.

  REFRESH : ITEMDATA, TAXDATA, WITHTAXDATA .

  REFRESH : IT_EBELN.

* change t-code mode with error
  authority-check object 'S_DEVELOP'
    id 'DEVCLASS' dummy
    id 'OBJTYPE'  field 'DEBUG'
    id 'OBJNAME'  dummy
    id 'P_GROUP'  dummy
    id 'ACTVT'    field '03'.
  if  sy-subrc = 0.
    mode = 'E'.
  endif.

*> Import IMG GET.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFBDCYN EQ 'X'.
    MOVE 'A' TO MODE.
  ENDIF.

*> Invoice Verify
  I_INVOICE = 'X'.
  REFRESH : RETURN, IT_MR22.

*> Import System IMG GET.
  SELECT SINGLE * FROM ZTIMIMG00.

*> Charge Document Get.
  CALL FUNCTION 'ZIM_GET_CHARGE_DOCUMENT'
        EXPORTING
             BUKRS                   =    BUKRS
             BELNR                   =    BELNR
             GJAHR                   =    GJAHR
        IMPORTING
             W_ZTBKPF                =    ZTBKPF
        TABLES
             IT_ZSBSEG               =    IT_ZSBSEG
             IT_ZSBSEG_OLD           =    IT_ZSBSEG_OLD
             IT_ZSBDIV               =    IT_ZSBDIV
             IT_ZSBHIS               =    IT_ZSBHIS
        EXCEPTIONS
             NOT_FOUND               =    4
             COMPANDYCODE_NOT_INPUT  =    6
             DOCUMENT_NO_NOT_INPUT   =    8
             FISC_YEAR_NOT_INPUT     =    10.

  CASE SY-SUBRC.
     WHEN 0.
     WHEN 4.
        RAISE   POST_ERROR.
     WHEN 6.
        RAISE   POST_ERROR.
     WHEN 8.
        RAISE   POST_ERROR.
     WHEN 10.
        RAISE   POST_ERROR.
     WHEN OTHERS.
  ENDCASE.

  IF ZTBKPF-ZFPOSYN EQ 'Y'.
     RAISE   POST_ERROR.
  ENDIF.

  MOVE-CORRESPONDING ZTBKPF  TO   *ZTBKPF.

*-----------------------------------------------------------------------
* P/O Item ACCOUNT ASSIGNMENT CHECK.
* EKPO-KNTTP(ACCOUNT ASSIGNMENT) :  K,F,P,A CASE
* IMPORT EXPENSE => T-CODE : FB01.
*-----------------------------------------------------------------------
  CLEAR : AA_CHK.
  IF ZTIMIMG00-ZFCSTMD EQ 'S'.
     LOOP AT IT_ZSBDIV.
        CLEAR : V_KNTTP, V_SAKTO, V_KOSTL, V_ANLN1, V_AUFNR, V_POSID.
        PERFORM P3000_CHECK_PO_AA USING IT_ZSBDIV-EBELN
                                        IT_ZSBDIV-EBELP
                                        V_KNTTP V_SAKTO V_KOSTL
                                        V_ANLN1 V_AUFNR
                                        V_POSID.
        IF NOT V_KNTTP IS INITIAL.
           AA_CHK = 'X'.
           EXIT.
        ENDIF.
    ENDLOOP.
 ENDIF.
*-----------------------------------------------------------------------

  "-----------------------------------------
  " Advanced Payment
  "-----------------------------------------
  IF ZTBKPF-ZFPOSYN EQ 'P'.
     IF ZTBKPF-ZFADVPT EQ 'X'.

        MOVE : ZTBKPF-ZFFIYR TO FISCALYEAR,
               ZTBKPF-ZFACDO TO INVOICEDOCNUMBER.

        PERFORM P3000_CALL_ADVANCE_PAYMENT TABLES RETURN
                                     USING FISCALYEAR
                                           INVOICEDOCNUMBER
                                           MODE
                                           L_SUBRC
                                           AA_CHK
                                           TEMP_DOCNO
                                           TEMP_YEAR.
        IF L_SUBRC EQ 0.
           L_ZFPOSYN = 'Y'.
        ELSE.
           L_ZFPOSYN = 'P'.
           RAISE   POST_ERROR.
        ENDIF.

        CLEAR : ZTBKPF-ZFPYYR, ZTBKPF-ZFPYNO.

        MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
               INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
               TEMP_YEAR          TO     ZTBKPF-ZFCLYR,
               TEMP_DOCNO         TO     ZTBKPF-ZFCLNO,
               L_ZFPOSYN          TO     ZTBKPF-ZFPOSYN,
               SY-UNAME           TO     ZTBKPF-UNAM,
               SY-DATUM           TO     ZTBKPF-UDAT,
               SY-UZEIT           TO     ZTBKPF-UTME.

        PERFORM P3000_ZTBKPF_UPDATE  USING 'U'.
        EXIT.
     ENDIF.
  ENDIF.

  "------------------------------------
  "   VENDOR A/P -> PAYEER A/P
  "------------------------------------
*  IF ZTBKPF-ZFPOSYN EQ 'C'.
*     IF ZTBKPF-ZFPYPT EQ 'X'.
*
*        MOVE : ZTBKPF-ZFFIYR TO FISCALYEAR,
*               ZTBKPF-ZFACDO TO INVOICEDOCNUMBER.
*
*        PERFORM P3000_CALL_DEBIT_PAYMENT TABLES RETURN
*                                   USING FISCALYEAR
*                                         INVOICEDOCNUMBER
*                                         MODE
*                                         L_SUBRC
*                                         AA_CHK
*                                         TEMP_DOCNO
*                                         TEMP_YEAR.
*        IF L_SUBRC EQ 0.
*           L_ZFPOSYN = 'Y'.
*        ELSE.
*           L_ZFPOSYN = 'C'.
*           RAISE   POST_ERROR.
*        ENDIF.
*
*        CLEAR : ZTBKPF-ZFCLYR, ZTBKPF-ZFCLNO.
*
*        MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
*               INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
*               TEMP_YEAR          TO     ZTBKPF-ZFPYYR,
*               TEMP_DOCNO         TO     ZTBKPF-ZFPYNO,
*               L_ZFPOSYN          TO     ZTBKPF-ZFPOSYN,
*               SY-UNAME           TO     ZTBKPF-UNAM,
*               SY-DATUM           TO     ZTBKPF-UDAT,
*               SY-UZEIT           TO     ZTBKPF-UTME.
*
*        PERFORM P3000_ZTBKPF_UPDATE  USING 'U'.
*        EXIT.
*     ENDIF.
*  ENDIF.

*-----------------------------------------------------------------------
* LIV
*-----------------------------------------------------------------------
  IF ( ZTBKPF-ZFDCSTX EQ 'X' OR  NOT ZTBKPF-TBTKZ IS INITIAL ) AND
       AA_CHK IS INITIAL     AND ZTBKPF-ZFPOYN NE 'N'.

     IF ZTIMIMG00-ZFEXFIX EQ 'X'.
        IF ( NOT ZTBKPF-TBTKZ IS INITIAL ) OR  ZTBKPF-ZFDCSTX EQ 'X'.
           "------------------------------------------
           " PO Flag of Fixed exchange rate UNMARK
           "------------------------------------------
           LOOP AT IT_ZSBDIV.
             IF NOT IT_ZSBDIV-EBELN IS INITIAL.
                READ TABLE IT_EBELN WITH KEY EBELN = IT_ZSBDIV-EBELN.
                IF SY-SUBRC NE 0.
                   SELECT SINGLE * FROM EKKO
                          WHERE    EBELN EQ  IT_ZSBDIV-EBELN.
                   IF EKKO-KUFIX EQ 'X'.
                      MOVE:IT_ZSBDIV-EBELN  TO   IT_EBELN-EBELN,
                           EKKO-WKURS       TO   IT_EBELN-WKURS.
                      READ TABLE IT_EBELN WITH KEY
                                            EBELN = IT_ZSBDIV-EBELN.

                      IF SY-SUBRC NE 0. APPEND  IT_EBELN. ENDIF.
                   ENDIF.
                ENDIF.
             ENDIF.
           ENDLOOP.

          "----------------------------------
          " PO flag of fixed exchage rate
          "-----------------------------------
          PERFORM  P3000_PO_FIXED_RATE_CHANGE TABLES  IT_EBELN
                                                      RETURN
                                              USING   ''.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
        ENDIF.
     ENDIF.

     "----------------------------------------------
     " LIV Data Make
     "----------------------------------------------
* Populate BAPI Data
* Begin of changes - UD1K921861
          PERFORM   P2000_MAKE_LIV_DATA.
* End of changes - UD1K921861

     "--------------------------------------------------
     " BAPI'S Function Call
     "--------------------------------------------------
     SET PARAMETER ID 'BLN' FIELD ''.
     SET PARAMETER ID 'GJR' FIELD ''.

     IF ZTIMIMG00-CSREALYN EQ 'X'.
        CALL FUNCTION 'BAPI_INCOMINGINVOICE_PARK'
             EXPORTING
                 HEADERDATA             = HEADERDATA
             IMPORTING
                 INVOICEDOCNUMBER       = INVOICEDOCNUMBER
                 FISCALYEAR             = FISCALYEAR
             TABLES
                 ITEMDATA               = ITEMDATA
                 TAXDATA                = TAXDATA
                 WITHTAXDATA            = WITHTAXDATA    "UD1K921861
             RETURN                     = RETURN.
     ELSE.
        CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
             EXPORTING
                 HEADERDATA             = HEADERDATA
             IMPORTING
                 INVOICEDOCNUMBER       = INVOICEDOCNUMBER
                 FISCALYEAR             = FISCALYEAR
             TABLES
                ITEMDATA                = ITEMDATA
                TAXDATA                 = TAXDATA
                WITHTAXDATA             = WITHTAXDATA     "UD1K921861
                RETURN                  = RETURN.
     ENDIF.

     IF NOT INVOICEDOCNUMBER IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        MESSAGE S260(M8) WITH INVOICEDOCNUMBER.
        MOVE : SY-MSGTY   TO     RETURN-TYPE,
               SY-MSGID   TO     RETURN-ID,
               SY-MSGNO   TO     RETURN-NUMBER,
               SY-MSGV1   TO     RETURN-MESSAGE_V1,
               SY-MSGV2   TO     RETURN-MESSAGE_V2,
               SY-MSGV3   TO     RETURN-MESSAGE_V3,
               SY-MSGV4   TO     RETURN-MESSAGE_V4.

        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                EXPORTING
                      MSGID     = RETURN-ID
                      MSGNR     = RETURN-NUMBER
                      MSGV1     = RETURN-MESSAGE_V1
                      MSGV2     = RETURN-MESSAGE_V2
                      MSGV3     = RETURN-MESSAGE_V3
                      MSGV4     = RETURN-MESSAGE_V4
               IMPORTING
                      MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
        APPEND  RETURN.

        " Subsequent Deblit/Credit.
        IF ZTIMIMG00-ZFEXFIX EQ 'X'.
           IF ( NOT ZTBKPF-TBTKZ IS INITIAL ) OR ZTBKPF-ZFDCSTX EQ 'X'.
              "--------------------------------------------------
              " Fixed Exchange Rate Flag Setting
              "--------------------------------------------------
              PERFORM  P3000_PO_FIXED_RATE_CHANGE TABLES IT_EBELN
                                                         RETURN
                                                  USING  'X'.
           ENDIF.
        ENDIF.
*ANDY
*        W_SUBRC = 0.
        L_SUBRC = 0.
     ELSE.
        LOOP AT RETURN.
           W_TABIX = SY-TABIX.
           CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                  EXPORTING
                        MSGID     = RETURN-ID
                        MSGNR     = RETURN-NUMBER
                        MSGV1     = RETURN-MESSAGE_V1
                        MSGV2     = RETURN-MESSAGE_V2
                        MSGV3     = RETURN-MESSAGE_V3
                        MSGV4     = RETURN-MESSAGE_V4
                 IMPORTING
                        MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
           MODIFY RETURN INDEX W_TABIX.
        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        IF ZTIMIMG00-ZFEXFIX EQ 'X'.
           IF ( NOT ZTBKPF-TBTKZ IS INITIAL ).
              "--------------------------------------------------
              " Fixed Exchange Rate Flag Setting
              "--------------------------------------------------
              PERFORM  P3000_PO_FIXED_RATE_CHANGE TABLES IT_EBELN
                                                         RETURN
                                                  USING  'X'.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
           ENDIF.
        ENDIF.
        RAISE   POST_ERROR.
     ENDIF.
     "---------------------------------------
     " Advanced Payment.
     "---------------------------------------
     CLEAR : FISC_YEAR, FI_DOC_NO.
     IF ZTBKPF-ZFADVPT EQ 'X'.
        PERFORM   P3000_CALL_ADVANCE_PAYMENT TABLES RETURN
                                     USING   FISCALYEAR
                                             INVOICEDOCNUMBER
                                             MODE
                                             L_SUBRC
                                             AA_CHK
                                             TEMP_DOCNO
                                             TEMP_YEAR.
        IF L_SUBRC EQ 0.
           L_ZFPOSYN = 'Y'.
        ELSE.
           L_ZFPOSYN = 'P'.
        ENDIF.

        IF L_ZFPOSYN EQ 'Y'.
           CLEAR : ZTBKPF-ZFPYYR, ZTBKPF-ZFPYNO.

           MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
                  INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
                  TEMP_YEAR          TO     ZTBKPF-ZFCLYR,
                  TEMP_DOCNO         TO     ZTBKPF-ZFCLNO,
                  L_ZFPOSYN          TO     ZTBKPF-ZFPOSYN,
                  SY-UNAME           TO     ZTBKPF-UNAM,
                  SY-DATUM           TO     ZTBKPF-UDAT,
                  SY-UZEIT           TO     ZTBKPF-UTME.

           PERFORM P3000_ZTBKPF_UPDATE  USING 'I'.
        ENDIF.
     ELSEIF ZTBKPF-ZFPYPT IS INITIAL.
        L_ZFPOSYN = 'Y'.
     ENDIF.
     "-------------------------------------
     " Payer <> Vendor
     "-------------------------------------
     CLEAR : FISC_YEAR, FI_DOC_NO.
     IF ZTBKPF-ZFPYPT EQ 'X'.
        PERFORM   P3000_CALL_DEBIT_PAYMENT TABLES RETURN
                                    USING  FISCALYEAR
                                           INVOICEDOCNUMBER
                                           MODE
                                           L_SUBRC
                                           AA_CHK
                                           TEMP_DOCNO
                                           TEMP_YEAR.
        IF L_SUBRC EQ 0.
           L_ZFPOSYN = 'Y'.
        ELSE.
           L_ZFPOSYN = 'C'.
        ENDIF.

        IF L_ZFPOSYN EQ 'Y'.
           CLEAR : ZTBKPF-ZFCLNO, ZTBKPF-ZFCLYR.
           MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
                  INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
                  TEMP_YEAR          TO     ZTBKPF-ZFPYYR,
                  TEMP_DOCNO         TO     ZTBKPF-ZFPYNO,
                  L_ZFPOSYN          TO     ZTBKPF-ZFPOSYN,
                  SY-UNAME           TO     ZTBKPF-UNAM,
                  SY-DATUM           TO     ZTBKPF-UDAT,
                  SY-UZEIT           TO     ZTBKPF-UTME.

           PERFORM P3000_ZTBKPF_UPDATE  USING 'I'.
        ENDIF.
     ELSEIF ZTBKPF-ZFADVPT IS INITIAL.
        L_ZFPOSYN = 'Y'.
     ENDIF.

     IF ZTBKPF-ZFPYPT IS INITIAL AND ZTBKPF-ZFADVPT IS INITIAL.
        IF L_SUBRC EQ 0.
           L_ZFPOSYN = 'Y'.
           MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
                  INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
                  L_ZFPOSYN          TO     ZTBKPF-ZFPOSYN,
                  SY-UNAME           TO     ZTBKPF-UNAM,
                  SY-DATUM           TO     ZTBKPF-UDAT,
                  SY-UZEIT           TO     ZTBKPF-UTME.

           PERFORM P3000_ZTBKPF_UPDATE  USING 'I'.
        ENDIF.
     ENDIF.
*----------------------------------------->
* Import Expense -> FI Document Create
*----------------------------------------->
  ELSE.

    " Local Currency Setting
    PERFORM  P2000_COMPANY_CURR_POST_SET
                           USING  L_WAERS W_KURSF W_WWERT MODE.

    " User Setting Convert.
    PERFORM  P2000_DATE_USER_CONVERT      USING ZTBKPF-BLDAT
                                       CHANGING W_BLDAT.
    PERFORM  P2000_DATE_USER_CONVERT      USING ZTBKPF-BUDAT
                                       CHANGING W_BUDAT.

    IF ZTIMIMG00-CSREALYN EQ 'X'.
       REFRESH : BDCDATA.
       PERFORM P2000_DYNPRO USING :
             'X' 'SAPLF040'    '0100',
             ' ' 'BKPF-BLDAT'  W_BLDAT,
             ' ' 'BKPF-BLART'  ZTBKPF-BLART,
             ' ' 'BKPF-BUKRS'  ZTBKPF-BUKRS,
             ' ' 'BKPF-BUDAT'  W_BUDAT,
             ' ' 'BKPF-MONAT'  ZTBKPF-MONAT,
             ' ' 'BKPF-WAERS'  L_WAERS,
             ' ' 'BKPF-KURSF'  W_KURSF,
             ' ' 'BKPF-WWERT'  W_WWERT,
             ' ' 'BKPF-XBLNR'  ZTBKPF-XBLNR.
    ELSE.
       REFRESH : BDCDATA.
       PERFORM P2000_DYNPRO USING :
             'X' 'SAPMF05A'    '0100',
             ' ' 'BKPF-BLDAT'  W_BLDAT,
             ' ' 'BKPF-BLART'  ZTBKPF-BLART,
             ' ' 'BKPF-BUKRS'  ZTBKPF-BUKRS,
             ' ' 'BKPF-BUDAT'  W_BUDAT,
             ' ' 'BKPF-MONAT'  ZTBKPF-MONAT,
             ' ' 'BKPF-WAERS'  L_WAERS,
             ' ' 'BKPF-KURSF'  W_KURSF,
             ' ' 'BKPF-WWERT'  W_WWERT,
             ' ' 'BKPF-XBLNR'  ZTBKPF-XBLNR.
    ENDIF.
    "-------------------------------------------
    " General FI Document
    "-------------------------------------------
*>> Use Park Document.
    IF ZTIMIMG00-CSREALYN EQ 'X'.
       PERFORM   P3000_SET_AP_POSTING_DATA_NO  USING   W_GSBER.
*>> Not use Park document.
    ELSE.
       PERFORM   P3000_SET_AP_POSTING_DATA     USING   W_GSBER.
    ENDIF.

    IF ZTIMIMG00-CSREALYN EQ 'X'.
       SET PARAMETER ID 'BLP' FIELD ''.
       SET PARAMETER ID 'GJR' FIELD ''.
       REFRESH : MESSTAB.
       CALL TRANSACTION 'F-64'  USING       BDCDATA
                                MODE        MODE
                                MESSAGES    INTO   MESSTAB.

    ELSE.
       SET PARAMETER ID 'BLN' FIELD ''.
       SET PARAMETER ID 'GJR' FIELD ''.
       REFRESH : MESSTAB.
       CALL TRANSACTION 'FB01'  USING        BDCDATA
*                                OPTIONS FROM CTU_PARAMS
                                MODE         MODE
                                MESSAGES    INTO   MESSTAB.
    ENDIF.

    L_SUBRC = SY-SUBRC.

   IF L_SUBRC NE 0.      ">> ERROR ߻.
      LOOP AT MESSTAB.
         MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
                MESSTAB-MSGID   TO     RETURN-ID,
                MESSTAB-MSGNR   TO     RETURN-NUMBER,
                MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
                MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
                MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
                MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = RETURN-ID
                         MSGNR     = RETURN-NUMBER
                         MSGV1     = RETURN-MESSAGE_V1
                         MSGV2     = RETURN-MESSAGE_V2
                         MSGV3     = RETURN-MESSAGE_V3
                         MSGV4     = RETURN-MESSAGE_V4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
      ENDLOOP.
      L_SUBRC = 4.
      RAISE   POST_ERROR.
   ELSE.                 ">> SUCCESS.
      IF ZTIMIMG00-CSREALYN EQ 'X'.
         GET PARAMETER ID 'BLP' FIELD INVOICEDOCNUMBER.
         GET PARAMETER ID 'GJR' FIELD FISCALYEAR.
      ELSE.
         GET PARAMETER ID 'BLN' FIELD INVOICEDOCNUMBER.
         GET PARAMETER ID 'GJR' FIELD FISCALYEAR.
      ENDIF.

      IF INVOICEDOCNUMBER IS INITIAL OR FISCALYEAR IS INITIAL.

         L_SUBRC = 4.
         MOVE : 'E'             TO     RETURN-TYPE,
                'ZIM'           TO     RETURN-ID,
                '494'           TO     RETURN-NUMBER,
                SPACE           TO     RETURN-MESSAGE_V1,
                SPACE           TO     RETURN-MESSAGE_V2,
                SPACE           TO     RETURN-MESSAGE_V3,
                SPACE           TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = RETURN-ID
                         MSGNR     = RETURN-NUMBER
                         MSGV1     = RETURN-MESSAGE_V1
                         MSGV2     = RETURN-MESSAGE_V2
                         MSGV3     = RETURN-MESSAGE_V3
                         MSGV4     = RETURN-MESSAGE_V4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
         RAISE   POST_ERROR.
      ELSE.
         MESSAGE S260(M8) WITH INVOICEDOCNUMBER.
         MOVE : SY-MSGTY   TO     RETURN-TYPE,
                SY-MSGID   TO     RETURN-ID,
                SY-MSGNO   TO     RETURN-NUMBER,
                SY-MSGV1   TO     RETURN-MESSAGE_V1,
                SY-MSGV2   TO     RETURN-MESSAGE_V2,
                SY-MSGV3   TO     RETURN-MESSAGE_V3,
                SY-MSGV4   TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = RETURN-ID
                         MSGNR     = RETURN-NUMBER
                         MSGV1     = RETURN-MESSAGE_V1
                         MSGV2     = RETURN-MESSAGE_V2
                         MSGV3     = RETURN-MESSAGE_V3
                         MSGV4     = RETURN-MESSAGE_V4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
         L_SUBRC = 0.

         "--------------------------------
         " Settlement Account
         "--------------------------------
         PERFORM  P3000_MR22_CALL  USING  INVOICEDOCNUMBER.

         "---------------------------------------------------
         " DB Update
         "---------------------------------------------------
         CLEAR : FISC_YEAR,     FI_DOC_NO,     ZTBKPF-ZFPYYR,
                 ZTBKPF-ZFPYNO, ZTBKPF-ZFCLNO, ZTBKPF-ZFCLYR.
         IF ZTBKPF-ZFADVPT EQ 'X'.
            CLEAR : ZTBKPF-ZFPYYR, ZTBKPF-ZFPYNO.
            PERFORM   P3000_CALL_ADVANCE_PAYMENT TABLES RETURN
                                          USING  FISCALYEAR
                                                 INVOICEDOCNUMBER
                                                 MODE
                                                 L_SUBRC
                                                 AA_CHK
                                                 FI_DOC_NO
                                                 FISC_YEAR.
            IF L_SUBRC NE 0.
               L_ZFPOSYN = 'P'.
            ELSE.
               MOVE : FI_DOC_NO    TO  ZTBKPF-ZFCLNO,
                      FISC_YEAR    TO  ZTBKPF-ZFCLYR.
               L_ZFPOSYN = 'Y'.
            ENDIF.
         ELSEIF ZTBKPF-ZFPYPT EQ 'X'.
            CLEAR : ZTBKPF-ZFCLNO, ZTBKPF-ZFCLYR.
            PERFORM   P3000_CALL_DEBIT_PAYMENT TABLES RETURN
                                        USING  FISCALYEAR
                                               INVOICEDOCNUMBER
                                               MODE
                                               L_SUBRC
                                               AA_CHK
                                               FI_DOC_NO
                                               FISC_YEAR.
           IF L_SUBRC NE 0.
              L_ZFPOSYN = 'C'.
           ELSE.
              MOVE : FI_DOC_NO    TO  ZTBKPF-ZFPYNO,
                     FISC_YEAR    TO  ZTBKPF-ZFPYYR.
              L_ZFPOSYN = 'Y'.
           ENDIF.
        ELSE.
           L_ZFPOSYN = 'Y'.
        ENDIF.

        MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
               INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
               L_ZFPOSYN          TO     ZTBKPF-ZFPOSYN,
               SY-UNAME           TO     ZTBKPF-UNAM,
               SY-DATUM           TO     ZTBKPF-UDAT,
               SY-UZEIT           TO     ZTBKPF-UTME.
        PERFORM P3000_ZTBKPF_UPDATE USING 'I'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.

*-----------------------------------------------------------------------
*> Local Currency Posting Set
*-----------------------------------------------------------------------
FORM  P2000_COMPANY_CURR_POST_SET USING L_WAERS W_KURSF W_WWERT MODE.

DATA: L_SUBRC        LIKE   SY-SUBRC.
*>>> BDC....
    REFRESH : BDCDATA.
* ʱȭ FIELD
    IF ZTBKPF-ZFPCUR EQ 'X'.      ">ȭ .
       CLEAR : W_KURSF.
       L_WAERS = ZTBKPF-HWAER.
       CLEAR : W_WWERT.
    ELSE.
       WRITE : ZTBKPF-KURSF   TO   W_KURSF.
       L_WAERS = ZTBKPF-WAERS.
       WRITE ZTBKPF-WWERT TO W_WWERT.
    ENDIF.
*--------------------------
*>> ...
*    PERFORM P2000_DYNPRO USING :
*          'X' 'SAPMF05A'    '0100',
*          ' ' 'BDC_OKCODE'  '/EB00'.
*
*    PERFORM P2000_DYNPRO USING :
*          'X' 'SAPMF05O'     '2000',
*          ' ' 'RFOPT2-XSNET' 'X',
*          ' ' 'BDC_OKCODE'   '=SICH'.
*
*    PERFORM P2000_DYNPRO USING :
*          'X' 'SAPMF05O'     '2000',
*          ' ' 'BDC_OKCODE'   '=RW'.
*
*    CALL TRANSACTION 'FB01'  USING       BDCDATA
**                             MODE        'N'
*                             MODE        MODE
*                             UPDATE      'V'
*                             MESSAGES     INTO   MESSTAB.   "Insert
*
ENDFORM.

*-----------------------------------------------------------------------
*>VP ݾ  ƾ.
*-----------------------------------------------------------------------
FORM  P3000_SET_VP_POSTING_DATA  USING   W_GSBER.

       READ TABLE IT_ZSBDIV INDEX 1.
       PERFORM P2000_DYNPRO USING :
             ' ' 'RF05A-NEWBS' IT_ZSBDIV-NEWBS, " Ű.
             ' ' 'RF05A-NEWKO' IT_ZSBDIV-NEWKO, " ڵ.
             ' ' 'BDC_OKCODE'  '/00'.
       IF ZTBKPF-ZFPCUR EQ 'X'.      ">ȭ .
          WRITE IT_ZSBDIV-DMBTR TO  TEMP_WRBTR
                                CURRENCY  IT_ZSBDIV-HWAER.
       ELSE.
          WRITE IT_ZSBDIV-WRBTR TO  TEMP_WRBTR
                                CURRENCY  IT_ZSBDIV-WAERS.
       ENDIF.
          PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.

       PERFORM P2000_DYNPRO USING :
             'X' 'SAPMF05A'    '0301',
             ' ' 'BSEG-WRBTR'  TEMP_WRBTR,         " Amount
             ' ' 'BSEG-MWSKZ'  ZTBKPF-MWSKZ,
             ' ' 'BSEG-BUPLA'  IT_ZSBDIV-BUPLA,    " Business Place
             ' ' 'BSEG-ZUONR'  IT_ZSBDIV-ZUONR,
             ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.

       READ TABLE IT_ZSBDIV INDEX 2.
       PERFORM P2000_DYNPRO USING :
             ' ' 'RF05A-NEWBS' IT_ZSBDIV-NEWBS, " Ű.
             ' ' 'RF05A-NEWKO' IT_ZSBDIV-NEWKO, " ڵ.
             ' ' 'BDC_OKCODE'  '/00'.

* NEXT SCREEN.
       IF ZTBKPF-ZFPCUR EQ 'X'.      ">ȭ .
          WRITE IT_ZSBDIV-DMBTR TO  TEMP_WRBTR
                                CURRENCY  IT_ZSBDIV-HWAER.
       ELSE.
          WRITE IT_ZSBDIV-WRBTR TO  TEMP_WRBTR
                                CURRENCY  IT_ZSBDIV-WAERS.
       ENDIF.
       PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.
*>>>>> TESTING   POINT....
       PERFORM P2000_DYNPRO USING :
             'X' 'SAPMF05A'    '0302',
*             ' ' 'BSEG-HKONT'  IT_ZSBDIV-AKONT,
             ' ' 'BSEG-WRBTR'  TEMP_WRBTR,          " Amount(VPݾ).
             ' ' 'BSEG-BUPLA'  IT_ZSBDIV-BUPLA,     " Business Place
             ' ' 'BSEG-GSBER'  IT_ZSBDIV-GSBER,     " Business Area
             ' ' 'BSEG-ZTERM'  IT_ZSBDIV-ZTERM,     " Payment Term
             ' ' 'BSEG-ZLSCH'  ZTBKPF-ZLSCH,        " ޹..
             ' ' 'BSEG-ZUONR'  IT_ZSBDIV-ZUONR,
             ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.    " ؽƮ.
** INSERTED BY FURONG ON 07/20/2005
IF ZTBKPF-ZFOPBN NE ' '.
             PERFORM P2000_DYNPRO USING :
             ' ' 'BSEG-EMPFB'  ZTBKPF-ZFOPBN.
ENDIF.
** END OF INSERTION
* NEXT SCREEN.
       LOOP AT IT_ZSBDIV FROM 3.
          W_TABIX = SY-TABIX.
          PERFORM P2000_DYNPRO USING :
             ' ' 'RF05A-NEWBS' IT_ZSBDIV-NEWBS, " Ű.
             ' ' 'RF05A-NEWKO' IT_ZSBDIV-NEWKO, " ڵ.
             ' ' 'BDC_OKCODE'  '/00'.


          IF W_TABIX NE 3.
             PERFORM P2000_DYNPRO USING :
                'X' 'SAPLKACB'     '0002',
                ' ' 'COBL-GSBER'   W_GSBER,        " .TEST
*                ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
*                ' ' 'COBL-PRCTR'   COBL-PRCTR,    " ͼ.
                ' ' 'BDC_OKCODE'   '/00'.         " ENTER
          ENDIF.

          IF ZTBKPF-ZFPCUR EQ 'X'.      ">ȭ .
             WRITE IT_ZSBDIV-DMBTR TO  TEMP_WRBTR
                                CURRENCY  IT_ZSBDIV-HWAER.
          ELSE.
             WRITE IT_ZSBDIV-WRBTR TO  TEMP_WRBTR
                                   CURRENCY  IT_ZSBDIV-WAERS.
          ENDIF.
          PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.

          PERFORM P2000_DYNPRO USING :
               'X' 'SAPMF05A'    '0300',
               ' ' 'BSEG-WRBTR'  TEMP_WRBTR,            " Amount
*              ' ' 'BSEG-BUPLA'  IT_ZSBDIV-BUPLA,       " Business Place
*              ' ' 'BSEG-GSBER'  IT_ZSBDIV-GSBER,       " Business Area
*              ' ' 'BSEG-ZTERM'  IT_ZSBDIV-ZTERM,       " Payment Term
               ' ' 'BSEG-EBELN'  IT_ZSBDIV-EBELN,
               ' ' 'BSEG-EBELP'  IT_ZSBDIV-EBELP,
               ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
               ' ' 'BSEG-ZUONR'  IT_ZSBDIV-ZUONR,
               ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.       " ؽƮ.

          W_GSBER = IT_ZSBDIV-GSBER.

       ENDLOOP.
       PERFORM P2000_DYNPRO USING :
           ' ' 'BDC_OKCODE'  '=BU'.                 " ENTER

       PERFORM P2000_DYNPRO USING :
          'X' 'SAPLKACB'     '0002',
          ' ' 'COBL-GSBER'   W_GSBER,       " .TEST
*             ' ' 'COBL-KOSTL'   COBL-KOSTL,    " COST CENTER TEST
*             ' ' 'COBL-PRCTR'   COBL-PRCTR,    " ͼ.
          ' ' 'BDC_OKCODE'   '/00'.         " ENTER

ENDFORM.



*-----------------------------------------------------------------------
*>Ϲ A/P ݾ  ƾ.
*-----------------------------------------------------------------------
FORM   P3000_AP_POSTING_DATA_SET   USING   W_GSBER.
DATA: W_WWERT(10),
      L_SUBRC        LIKE   SY-SUBRC,
      W_KURSF(12).

DATA: TEMP_WRBTR(16),
      TEMP_WMWST(16),
      TEMP_DMBTR(16).


  LOOP AT IT_ZSBDIV.
     W_TABIX = SY-TABIX.
*> ݾ.
     IF ZTBKPF-ZFPCUR EQ 'X'.      ">ȭ .
         WRITE IT_ZSBDIV-DMBTR TO  TEMP_WRBTR
               CURRENCY  IT_ZSBDIV-HWAER.
     ELSE.
         WRITE IT_ZSBDIV-WRBTR TO  TEMP_WRBTR
               CURRENCY  IT_ZSBDIV-WAERS.
     ENDIF.
     PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.
*> ΰ.
     IF IT_ZSBDIV-WMWST IS INITIAL.
        CLEAR : TEMP_WMWST.
     ELSE.
        WRITE IT_ZSBDIV-WMWST TO  TEMP_WMWST
              CURRENCY  IT_ZSBDIV-WAERS.
        PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WMWST.
     ENDIF.
*> ȭ.
     IF ZTBKPF-ZFPCUR EQ 'X'.      ">ȭ .
        CLEAR : TEMP_DMBTR.
     ELSE.
        WRITE IT_ZSBDIV-DMBTR TO  TEMP_DMBTR CURRENCY  IT_ZSBDIV-HWAER.
        PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_DMBTR.
     ENDIF.
     IF ZTBKPF-ZFPCUR EQ 'X'.      ">ȭ .
        CLEAR : W_KURSF.
     ELSE.
        WRITE : IT_ZSBDIV-KURSF   TO   W_KURSF.
     ENDIF.

     PERFORM P2000_DYNPRO USING :
        ' ' 'RF05A-NEWBS' IT_ZSBDIV-NEWBS, " Ű.
        ' ' 'RF05A-NEWKO' IT_ZSBDIV-NEWKO, " ڵ.
        ' ' 'BDC_OKCODE'  '/00'.

*---> ʱȭ .
          IF W_TABIX EQ 1.
             PERFORM P2000_DYNPRO USING :
               'X' 'SAPMF05A'    '0302',
               ' ' 'BSEG-HKONT'  IT_ZSBDIV-AKONT,
               ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
*               ' ' 'BSEG-WMWST'  TEMP_WMWST,
               ' ' 'BSEG-ZTERM'  IT_ZSBDIV-ZTERM,
               ' ' 'BSEG-ZLSPR'  ZTBKPF-ZLSPR,
*               ' ' 'BSEG-ZLSCH'  ZTBKPF-ZLSCH,
               ' ' 'BKPF-XMWST'  IT_ZSBDIV-XMWST,
*               ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
               ' ' 'BSEG-BUPLA'  IT_ZSBDIV-BUPLA,
               ' ' 'BSEG-ZFBDT'  IT_ZSBDIV-ZFBDT,
               ' ' 'BSEG-ZUONR'  IT_ZSBDIV-ZUONR,
               ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.
** INSERTED BY FURONG ON 07/20/2005
IF ZTBKPF-ZFOPBN NE ' '.
               PERFORM P2000_DYNPRO USING :
               ' ' 'BSEG-EMPFB'  ZTBKPF-ZFOPBN.
ENDIF.
** END OF INSERTION

            IF ZTBKPF-XMWST NE 'X'.
                PERFORM P2000_DYNPRO USING :
                  ' ' 'BSEG-WMWST'  TEMP_WMWST.
             ENDIF.
*> ȭݾ.
             IF ZTBKPF-ZFPCUR NE 'X'.      ">ȭ .
                PERFORM P2000_DYNPRO USING ' ' 'BSEG-DMBTR' TEMP_DMBTR.
             ENDIF.
          ELSE.
             IF W_TABIX NE 2.
                PERFORM P2000_DYNPRO USING :
                  'X' 'SAPLKACB'    '0002',
                  ' ' 'COBL-GSBER'  W_GSBER, " .
                  ' ' 'BDC_OKCODE'  '=ENTE'.
             ENDIF.

             PERFORM P2000_DYNPRO USING :
               'X' 'SAPMF05A'    '0300',
               ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
               ' ' 'BSEG-EBELN'  IT_ZSBDIV-EBELN,
               ' ' 'BSEG-EBELP'  IT_ZSBDIV-EBELP,
               ' ' 'BSEG-ZUONR'  IT_ZSBDIV-ZUONR,
               ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.

             IF ZTBKPF-ZFPCUR NE 'X'.
                PERFORM P2000_DYNPRO USING :
                           ' ' 'BSEG-DMBTR'  TEMP_DMBTR.
             ENDIF.

          ENDIF.
          W_GSBER = IT_ZSBDIV-GSBER.
       ENDLOOP.

       PERFORM P2000_DYNPRO USING :
               ' ' 'BDC_OKCODE'  '=BU'.

       PERFORM P2000_DYNPRO USING :
              'X' 'SAPLKACB'    '0002',
              ' ' 'COBL-GSBER'  W_GSBER, " .
              ' ' 'BDC_OKCODE'  '=ENTE'.

ENDFORM.

*-----------------------------------------------------------------------
*>  General Import Expense Process
*-----------------------------------------------------------------------
FORM   P3000_SET_AP_POSTING_DATA   USING   W_GSBER.

DATA: W_WWERT(10),
      W_WMWST        LIKE   ZTBKPF-WMWST,
      L_SUBRC        LIKE   SY-SUBRC,
      W_KURSF(12),
      L_MENGE(17),
      L_MEINS(03),
      TEMP_ZSBDIV    LIKE   IT_ZSBDIV.

DATA: TEMP_WRBTR(16),
      TEMP_WMWST(16),
      TEMP_FWBAS(16),
      TEMP_DMBTR(16).

   READ TABLE IT_ZSBDIV INDEX 2.
   MOVE  IT_ZSBDIV-EBELN  TO  W_TEXT.

   LOOP AT IT_ZSBDIV.

      W_TABIX = SY-TABIX.

     CLEAR : V_KNTTP, V_SAKTO, V_KOSTL, V_ANLN1, V_AUFNR, V_POSID.

     " Account Assignment Check.
     PERFORM P3000_CHECK_PO_AA USING IT_ZSBDIV-EBELN
                                     IT_ZSBDIV-EBELP
                                     V_KNTTP V_SAKTO V_KOSTL
                                     V_ANLN1 V_AUFNR
                                     V_POSID.

     IF V_KNTTP EQ 'S'.
        MOVE : IT_ZSBDIV-MATNR   TO  IT_MR22-MATNR,
               IT_ZSBDIV-WERKS   TO  IT_MR22-WERKS,
               IT_ZSBDIV-DMBTR   TO  IT_MR22-DMBTR,
               IT_ZSBDIV-WAERS   TO  IT_MR22-WAERS,
               IT_ZSBDIV-BUKRS   TO  IT_MR22-BUKRS,
               IT_ZSBDIV-BELNR   TO  IT_MR22-BELNR,
               IT_ZSBDIV-GJAHR   TO  IT_MR22-GJAHR,
               IT_ZSBDIV-BUZEI   TO  IT_MR22-BUZEI,
               IT_ZSBDIV-ZFBSEQ  TO  IT_MR22-ZFBSEQ.
        APPEND IT_MR22.
     ENDIF.

     " L/C No, B/L No Get.
     CLEAR : W_LCNO, W_BLNO.
     PERFORM  P2000_ASIGNMENT_GET USING IT_ZSBDIV-ZFCSTGRP
                                        IT_ZSBDIV-ZFIMDNO
                                        IT_ZSBDIV-EBELN
                                        IT_ZSBDIV-EBELP
                               CHANGING W_LCNO
                                        W_BLNO.
     IF W_LCNO IS INITIAL.
        MOVE    'N/A'   TO  W_LCNO.
     ENDIF.

     IF ZTBKPF-ZFCSTGRP EQ '003'.
        CONCATENATE  'LC: '  W_LCNO  INTO  W_TEXT.
     ELSE.
        MOVE  W_BLNO   TO  W_TEXT.
     ENDIF.

     " Amount
     IF ZTBKPF-ZFPCUR EQ 'X'.
        IF W_TABIX EQ 1.
           IF IT_ZSBDIV-DMBTR IS INITIAL.
              W_WMWST  =  IT_ZSBDIV-WMWST.
           ELSE.
              W_WMWST  = IT_ZSBDIV-DMBTR.
           ENDIF.
        ELSE.
           IF IT_ZSBDIV-DMBTR IS INITIAL.
              W_WMWST  =  IT_ZSBDIV-WMWST.
           ELSE.
              W_WMWST = IT_ZSBDIV-DMBTR.
           ENDIF.
        ENDIF.
        WRITE W_WMWST TO  TEMP_WRBTR CURRENCY  IT_ZSBDIV-HWAER.
     ELSE.
        IF W_TABIX EQ 1.
           IF IT_ZSBDIV-WRBTR IS INITIAL.
              W_WMWST = IT_ZSBDIV-WMWST.
           ELSE.
              W_WMWST = IT_ZSBDIV-WRBTR .
           ENDIF.
        ELSE.
           W_WMWST = IT_ZSBDIV-WRBTR.
        ENDIF.
        WRITE W_WMWST TO TEMP_WRBTR CURRENCY  IT_ZSBDIV-WAERS.
     ENDIF.
     PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.

     " Tax Amount.
     IF W_TABIX EQ 1.
        IF IT_ZSBDIV-DMBTR IS INITIAL.
           WRITE  '0'  TO  TEMP_WMWST CURRENCY  IT_ZSBDIV-HWAER.
        ENDIF.
     ELSE.
        WRITE '0' TO  TEMP_WMWST CURRENCY  IT_ZSBDIV-HWAER.
     ENDIF.
     PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WMWST.

     " Taxable Amount(Document Currency)
     WRITE IT_ZSBDIV-FWBAS TO  TEMP_FWBAS CURRENCY  IT_ZSBDIV-HWAER.
     PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_FWBAS.

     " Local Currency
     IF ZTBKPF-ZFPCUR EQ 'X'.
        CLEAR : W_KURSF.
        WRITE IT_ZSBDIV-DMBTR TO TEMP_DMBTR CURRENCY IT_ZSBDIV-HWAER.
        PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_DMBTR.
     ELSE.
        CLEAR : TEMP_DMBTR.
        WRITE : IT_ZSBDIV-KURSF   TO   W_KURSF.
     ENDIF.

     IF W_TABIX EQ 1.
        PERFORM P2000_DYNPRO USING :
            ' ' 'RF05A-NEWKO'  IT_ZSBDIV-NEWKO,
            ' ' 'RF05A-NEWBS'  IT_ZSBDIV-NEWBS,
            ' ' 'BDC_OKCODE'  '/00'.
     ELSE.
        CASE  V_KNTTP.
           WHEN 'K'.
              PERFORM P2000_DYNPRO USING :
                  ' ' 'RF05A-NEWKO'  V_SAKTO,
                  ' ' 'RF05A-NEWBS'  '40',
                  ' ' 'BDC_OKCODE'  '/00'.
           WHEN 'A'.
              PERFORM P2000_DYNPRO USING :
                  ' ' 'RF05A-NEWKO' V_ANLN1,
                  ' ' 'RF05A-NEWBS' '70',
                  ' ' 'RF05A-NEWBW' '100',
                  ' ' 'BDC_OKCODE'  '/00'.
           WHEN 'F'.
              PERFORM P2000_DYNPRO USING :
                  ' ' 'RF05A-NEWKO' V_SAKTO,
                  ' ' 'RF05A-NEWBS' '40',
                  ' ' 'BDC_OKCODE'  '/00'.
           WHEN 'P' OR 'Q' OR 'R'.
              PERFORM P2000_DYNPRO USING :
                  ' ' 'RF05A-NEWKO' V_SAKTO,
                  ' ' 'RF05A-NEWBS' '40',
                  ' ' 'BDC_OKCODE'  '/00'.
           WHEN 'S'.
              PERFORM P2000_DYNPRO USING :
                  ' ' 'RF05A-NEWKO' V_SAKTO,
                  ' ' 'RF05A-NEWBS' IT_ZSBDIV-NEWBS,
                  ' ' 'BDC_OKCODE'  '/00'.
           WHEN OTHERS.
              PERFORM P2000_DYNPRO USING :
                  ' ' 'RF05A-NEWBS' IT_ZSBDIV-NEWBS,
                  ' ' 'RF05A-NEWKO' IT_ZSBDIV-NEWKO,
                  ' ' 'BDC_OKCODE'  '/00'.
        ENDCASE.
     ENDIF.

* Credit Entry Input..
     IF W_TABIX EQ 1.
        W_DYNNR = '0302'.
        PERFORM P2000_DYNPRO USING :
            'X' 'SAPMF05A'    '0302',
            ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
            ' ' 'BSEG-ZLSPR'  ZTBKPF-ZLSPR,
            ' ' 'BSEG-ZLSCH'  ZTBKPF-ZLSCH,
            ' ' 'BSEG-ZUONR'  IT_ZSBDIV-ZUONR,
            ' ' 'BSEG-KIDNO'  W_BLNO,
            ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.
** INSERTED BY FURONG ON 07/20/2005
IF ZTBKPF-ZFOPBN NE ' '.
            PERFORM P2000_DYNPRO USING :
             ' ' 'BSEG-EMPFB'  ZTBKPF-ZFOPBN.
ENDIF.
** END OF INSERTION
        " User Setting Convert.
        PERFORM  P2000_DATE_USER_CONVERT    USING IT_ZSBDIV-ZFBDT
                                         CHANGING W_ZFBDT.

        " Baseline Date
        IF NOT IT_ZSBDIV-ZFBDT IS INITIAL.
           PERFORM P2000_DYNPRO USING ' ' 'BSEG-ZFBDT' W_ZFBDT.
        ENDIF.

        " Local Currency Posting
        IF ZTBKPF-ZFPCUR NE 'X'.
           PERFORM P2000_DYNPRO USING ' ' 'BSEG-DMBTR'  TEMP_DMBTR.
        ENDIF.
     ELSE.
        WRITE IT_ZSBDIV-MENGE  TO  L_MENGE UNIT  IT_ZSBDIV-MEINS.
        WRITE IT_ZSBDIV-MEINS  TO  L_MEINS.

        CASE V_KNTTP.
           WHEN 'K'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0300',
                  ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                  ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
                  ' ' 'BSEG-EBELN'  IT_ZSBDIV-EBELN,
                  ' ' 'BSEG-EBELP'  IT_ZSBDIV-EBELP,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT,
                  ' ' 'BDC_OKCODE'  '=ZK'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPLKACB'   '0002',
                  ' ' 'COBL-KOSTL'  V_KOSTL,
                  ' ' 'BDC_OKCODE'  '=ENTE'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0330',
                  ' ' 'BDC_OKCODE'  '/00'.
              W_DYNNR = '0330'.
           WHEN 'A'.
              PERFORM P2000_DYNPRO USING :
                'X' 'SAPMF05A'    '0305',
                ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
                ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.
              W_DYNNR = '0305'.
           WHEN 'F'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0300',
                  ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                  ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
                  ' ' 'BSEG-EBELN'  IT_ZSBDIV-EBELN,
                  ' ' 'BSEG-EBELP'  IT_ZSBDIV-EBELP,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT,
                  ' ' 'BDC_OKCODE'  '=ZK'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPLKACB'   '0002',
                  ' ' 'COBL-AUFNR'  V_AUFNR,
                  ' ' 'BDC_OKCODE'  '=ENTE'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0330',
                  ' ' 'BDC_OKCODE'  '/00'.
              W_DYNNR = '0330'.
           WHEN 'P' OR 'Q' OR 'R'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0300',
                  ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                  ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
                  ' ' 'COBL-PS_POSID' V_POSID,
                  ' ' 'COBL-KOSTL'  IT_ZSBDIV-KOSTL,
                  ' ' 'BSEG-BUPLA'  IT_ZSBDIV-BUPLA,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT.
                W_DYNNR = '0300'.
           WHEN 'S'.
              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0300',
                  ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                  ' ' 'BSEG-EBELN'  IT_ZSBDIV-EBELN,
                  ' ' 'BSEG-EBELP'  IT_ZSBDIV-EBELP,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT,
                  ' ' 'BSEG-MENGE'  L_MENGE,
                  ' ' 'BSEG-MEINS'  L_MEINS,
                  ' ' 'BDC_OKCODE'  '=ZK'.

              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPLKACB' '0002',
                  ' ' 'COBL-MATNR'  IT_ZSBDIV-MATNR,
                  ' ' 'BDC_OKCODE'  '=ENTE'.

              PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0330',
                  ' ' 'BDC_OKCODE'  '/00'.

           WHEN OTHERS.
              IF ZTBKPF-ZFPOYN EQ 'N'.
                 PERFORM P2000_DYNPRO USING :
                     'X' 'SAPMF05A'    '0300',
                     ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                     ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
                     ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT,
                     ' ' 'BDC_OKCODE'  '=ZK'.

                 PERFORM P2000_DYNPRO USING :
                     'X' 'SAPLKACB' '0002',
                     ' ' 'COBL-KOSTL'  IT_ZSBDIV-KOSTL,
                     ' ' 'BDC_OKCODE'  '=ENTE'.

                 PERFORM P2000_DYNPRO USING :
                     'X' 'SAPMF05A'    '0330',
                     ' ' 'BDC_OKCODE'  '/00'.

              ELSE.
                 PERFORM P2000_DYNPRO USING :
                     'X' 'SAPMF05A'    '0300',
                     ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                     ' ' 'BSEG-EBELN'  IT_ZSBDIV-EBELN,
                     ' ' 'BSEG-EBELP'  IT_ZSBDIV-EBELP,
                     ' ' 'BSEG-MWSKZ'  IT_ZSBDIV-MWSKZ,
                     ' ' 'BSEG-MENGE'  L_MENGE,
                     ' ' 'BSEG-MEINS'  L_MEINS,
                     ' ' 'BSEG-SGTXT'  IT_ZSBDIV-SGTXT,
                     ' ' 'BDC_OKCODE'  '=ZK'.

                 PERFORM P2000_DYNPRO USING :
                     'X' 'SAPLKACB' '0002',
                     ' ' 'COBL-MATNR'  IT_ZSBDIV-MATNR,
                     ' ' 'BDC_OKCODE'  '=ENTE'.

                 PERFORM P2000_DYNPRO USING :
                     'X' 'SAPMF05A'    '0330',
                     ' ' 'BSEG-XREF3'  W_TEXT,
                     ' ' 'BDC_OKCODE'  '/00'.
              ENDIF.
              W_DYNNR = '0330'.
         ENDCASE.

         " Local Currency Usage.
         IF ZTBKPF-ZFPCUR NE 'X'.
            PERFORM P2000_DYNPRO USING ' ' 'BSEG-DMBTR' TEMP_DMBTR.
         ENDIF.

      ENDIF.

      W_GSBER = IT_ZSBDIV-GSBER.
      W_PRCTR = IT_ZSBDIV-PRCTR.
      MOVE: V_KNTTP TO V_KNTTP_OLD,
            V_SAKTO TO V_SAKTO_OLD,
            V_KOSTL TO V_KOSTL_OLD,
            V_ANLN1 TO V_ANLN1_OLD,
            V_AUFNR TO V_AUFNR_OLD,
            V_POSID TO V_POSID_OLD.

      MOVE-CORRESPONDING IT_ZSBDIV TO TEMP_ZSBDIV.

   ENDLOOP.

   " VAT Account GET.
   SELECT SINGLE * FROM ZTIMIMG11 WHERE BUKRS EQ ZTBKPF-BUKRS.

   IF ZTBKPF-WMWST GT 0 AND ZTBKPF-XMWST NE 'X' AND ZTBKPF-WRBTR GT 0.

      " VAT.
      WRITE ZTBKPF-WMWST TO TEMP_WRBTR CURRENCY ZTBKPF-WAERS.

      " Taxable Amount
      W_WMWST = ZTBKPF-WRBTR - ZTBKPF-WMWST.
      WRITE W_WMWST   TO TEMP_FWBAS CURRENCY ZTBKPF-WAERS.

      PERFORM P2000_DYNPRO USING :
              'X' 'SAPMF05A'    '0300',
              ' ' 'RF05A-NEWBS' '40',
              ' ' 'RF05A-NEWKO' ZTIMIMG11-ZFIOCAC6,
              ' ' 'BDC_OKCODE'  '/00'.
      PERFORM P2000_DYNPRO USING :
              'X' 'SAPMF05A'    '0312',
              ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
              ' ' 'BSEG-FWBAS'  TEMP_FWBAS,
              ' ' 'BSEG-BUPLA'  ZTBKPF-BUPLA,
              ' ' 'BSEG-MWSKZ'  ZTBKPF-MWSKZ.
   ENDIF.

   PERFORM P2000_DYNPRO USING :
           ' ' 'BDC_OKCODE'  '=BU'.

ENDFORM.


*> Advanced Payment Process
FORM   P3000_CALL_ADVANCE_PAYMENT  TABLES RETURN STRUCTURE BAPIRET2
                                   USING  FISC_YEAR FI_DOC_NO
                                          MODE      L_SUBRC
                                          AA_CHK
                                          INVOICEDOCNUMBER1
                                          FISCALYEAR1.

DATA : L_AWKEY   LIKE   BKPF-AWKEY.
DATA : L_WRBTR   LIKE   ZTBKPF-WRBTR.
DATA : L_KURSF(09).
DATA : L_BELNR   LIKE   BKPF-BELNR.
DATA : TEMP_WRBTR(16).
*
DATA : L_ZFIMDNO LIKE ZTBSEG-ZFIMDNO.
DATA : L_ZFBLNO  LIKE ZTIV-ZFBLNO.
DATA : L_ZFWERKS LIKE ZTBL-ZFWERKS.
DATA : L_GSBER   LIKE ZTBKPF-GSBER.
DATA : L_BUPLA   LIKE ZTBKPF-BUPLA.
*

   SELECT * FROM T021R
            WHERE EVENT EQ 'SL-AG'
            AND   FELDN EQ 'BELNR'.
   ENDSELECT.
   IF SY-SUBRC EQ 0.
      T021R-SELPS = T021R-SELPS + 1.
      MOVE : 'RF05A-XPOS1('  TO TEMP_FNAM,
             T021R-SELPS     TO TEMP_FNAM+12(2),
             ')'             TO TEMP_FNAM+14(1).
   ENDIF.

*>> ΰ ҷ ó(ؿƮ)
* BA  BA *
*   ƴϰ óμ BA ; .
   L_GSBER = ZTBKPF-BUPLA.
   L_BUPLA = ZTBKPF-GSBER.

   IF ZTBKPF-ZFCSTGRP EQ '006'.
       SELECT SINGLE ZFIMDNO INTO L_ZFIMDNO
                             FROM ZTBSEG
                            WHERE BELNR EQ ZTBKPF-BELNR
                              AND ZFCD EQ '003'.
       IF SY-SUBRC EQ 0.
          SELECT SINGLE ZFBLNO INTO L_ZFBLNO
                               FROM  ZTIV
                              WHERE ZFIVNO  EQ  L_ZFIMDNO.

          SELECT SINGLE ZFWERKS INTO L_ZFWERKS
                                FROM ZTBL
                               WHERE ZFBLNO EQ L_ZFBLNO.

          CALL FUNCTION 'ZIM_GET_USER_BUSINESS_AREA'
             EXPORTING
                UNAME   =    SY-UNAME
                WERKS   =    L_ZFWERKS
             IMPORTING
                GSBER   =    L_GSBER
                BUPLA   =    L_BUPLA.
       ENDIF.
   ENDIF.


   REFRESH : BDCDATA.
   WRITE ZTBKPF-KURSF TO L_KURSF.
*> ʱȭ.
   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0122',
           ' ' 'BKPF-BLDAT'  ZTBKPF-BLDAT,
           ' ' 'BKPF-BUDAT'  ZTBKPF-BUDAT,
*           ' ' 'BKPF-BLART'  ZTBKPF-BLART,
           ' ' 'BKPF-BLART'  'RE',
           ' ' 'BKPF-BUKRS'  ZTBKPF-BUKRS,
           ' ' 'BKPF-WAERS'  ZTBKPF-WAERS,
           ' ' 'BKPF-KURSF'  L_KURSF,
           ' ' 'BKPF-XBLNR'  '',
           ' ' 'BKPF-BKTXT'  '',
           ' ' 'RF05A-AUGTX' '',
           ' ' 'RF05A-XPOS1(4)' 'X',
           ' ' 'RF05A-NEWBS' '39',
           ' ' 'RF05A-NEWKO' ZTBKPF-HKONT,
           ' ' 'RF05A-NEWUM' 'D',
           ' ' 'BDC_OKCODE'  '/00'.

   L_WRBTR = ZTBKPF-WRBTR . ".+ ZTBKPF-WMWST.
   WRITE : L_WRBTR TO TEMP_WRBTR CURRENCY ZTBKPF-WAERS.

   PERFORM P2000_DYNPRO USING :
           'X' 'SAPMF05A'    '0303',
           ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
*           ' ' 'BSEG-WMWST'  ZTBKPF-WMWST,
*           ' ' 'BSEG-DMBTR'  ZTBKPF-DMBTR,
           ' ' 'BSEG-ZFBDT'  ZTBKPF-ZFBDT,
           ' ' 'BSEG-ZUONR'  '',
           ' ' 'BSEG-SGTXT'  ' ü',
*           ' ' 'BSEG-BUPLA'  ZTBKPF-BUPLA,
*           ' ' 'BSEG-GSBER'  ZTBKPF-GSBER,
           ' ' 'BSEG-BUPLA'  L_GSBER,
           ' ' 'BSEG-GSBER'  L_BUPLA,
           ' ' 'BDC_OKCODE'  '=PA'.

   IF ( ZTBKPF-ZFDCSTX EQ 'X' OR      ">ȹۺ .
        NOT ZTBKPF-TBTKZ IS INITIAL ) "> ļ/�?
        AND AA_CHK IS INITIAL. ">ACCOUNT ASSIGNMENT
.

      L_AWKEY = FI_DOC_NO.
      L_AWKEY+10 = FISC_YEAR.

      DO.
         SELECT * FROM  BKPF UP TO 1 ROWS
                  WHERE AWKEY EQ L_AWKEY
                  AND   BLART EQ ZTBKPF-BLART.
         ENDSELECT.
         IF SY-SUBRC EQ 0.
            EXIT.
         ENDIF.
         WAIT UP TO 1 SECONDS.
      ENDDO.

      MOVE : BKPF-BELNR TO L_BELNR.
   ELSE.
      MOVE : FI_DOC_NO  TO L_BELNR.
   ENDIF.

   PERFORM P2000_DYNPRO USING :
              'X' 'SAPMF05A'    '0710',
              ' ' 'RF05A-AGBUK' ZTBKPF-BUKRS,
              ' ' 'RF05A-AGKOA' 'K',
              ' ' 'RF05A-XNOPS' 'X',
              ' ' TEMP_FNAM     'X',
*              ' ' 'RF05A-XPOS1(03)' 'X',
*              ' ' 'RF05A-XNOPS' ' ',
              ' ' 'BDC_OKCODE'  '/00'.

   PERFORM P2000_DYNPRO USING :
                 'X' 'SAPMF05A'        '0731',
                 ' ' 'RF05A-SEL01(01)' L_BELNR,
                 ' ' 'BDC_OKCODE'  '=PA'.

   PERFORM P2000_DYNPRO USING :
           'X' 'SAPDF05X'        '3100',
           ' ' 'BDC_OKCODE'      '=BU'.

    SET PARAMETER ID 'BLN' FIELD ''.        " ǥȣ.
    SET PARAMETER ID 'GJR' FIELD ''.        " ȸ�?

    DO.
       SELECT SINGLE * FROM BKPF
              WHERE BUKRS EQ ZTBKPF-BUKRS
              AND   GJAHR EQ ZTBKPF-GJAHR
              AND   BELNR EQ L_BELNR.
       IF SY-SUBRC EQ 0.
          EXIT.
       ENDIF.
       WAIT UP TO 1 SECONDS.
    ENDDO.

*>> BDC CALL.
    REFRESH:MESSTAB.
    CALL TRANSACTION 'FB05'  USING       BDCDATA
*                             MODE        'A'
                             MODE        MODE
                             UPDATE      'S'
                             MESSAGES    INTO   MESSTAB.

    L_SUBRC = SY-SUBRC.

   IF L_SUBRC NE 0.      ">> ERROR ߻.
      LOOP AT MESSTAB.
         MOVE : MESSTAB-MSGTYP  TO     RETURN-TYPE,
                MESSTAB-MSGID   TO     RETURN-ID,
                MESSTAB-MSGNR   TO     RETURN-NUMBER,
                MESSTAB-MSGV1   TO     RETURN-MESSAGE_V1,
                MESSTAB-MSGV2   TO     RETURN-MESSAGE_V2,
                MESSTAB-MSGV3   TO     RETURN-MESSAGE_V3,
                MESSTAB-MSGV4   TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = RETURN-ID
                         MSGNR     = RETURN-NUMBER
                         MSGV1     = RETURN-MESSAGE_V1
                         MSGV2     = RETURN-MESSAGE_V2
                         MSGV3     = RETURN-MESSAGE_V3
                         MSGV4     = RETURN-MESSAGE_V4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
      ENDLOOP.
      L_SUBRC = 4.
*      RAISE   POST_ERROR.
   ELSE.                 ">> SUCCESS .
      GET PARAMETER ID 'BLN' FIELD INVOICEDOCNUMBER1.
      GET PARAMETER ID 'GJR' FIELD FISCALYEAR1.  " ȸ�?
*>> ǥȣ ޵  .
      IF INVOICEDOCNUMBER1  IS INITIAL OR
         FISCALYEAR1        IS INITIAL.

*>>> ..(  ....)
         L_SUBRC = 4.
         MESSAGE S494.
         MOVE : 'E'             TO     RETURN-TYPE,
                'ZIM'           TO     RETURN-ID,
                '494'           TO     RETURN-NUMBER,
                SPACE           TO     RETURN-MESSAGE_V1,
                SPACE           TO     RETURN-MESSAGE_V2,
                SPACE           TO     RETURN-MESSAGE_V3,
                SPACE           TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = RETURN-ID
                         MSGNR     = RETURN-NUMBER
                         MSGV1     = RETURN-MESSAGE_V1
                         MSGV2     = RETURN-MESSAGE_V2
                         MSGV3     = RETURN-MESSAGE_V3
                         MSGV4     = RETURN-MESSAGE_V4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
         L_SUBRC = 4.
*         RAISE   POST_ERROR.
      ELSE.
         MESSAGE S260(M8) WITH INVOICEDOCNUMBER1.
         MOVE : SY-MSGTY   TO     RETURN-TYPE,
                SY-MSGID   TO     RETURN-ID,
                SY-MSGNO   TO     RETURN-NUMBER,
                SY-MSGV1   TO     RETURN-MESSAGE_V1,
                SY-MSGV2   TO     RETURN-MESSAGE_V2,
                SY-MSGV3   TO     RETURN-MESSAGE_V3,
                SY-MSGV4   TO     RETURN-MESSAGE_V4.

         CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                         MSGID     = RETURN-ID
                         MSGNR     = RETURN-NUMBER
                         MSGV1     = RETURN-MESSAGE_V1
                         MSGV2     = RETURN-MESSAGE_V2
                         MSGV3     = RETURN-MESSAGE_V3
                         MSGV4     = RETURN-MESSAGE_V4
                  IMPORTING
                         MESSAGE_TEXT_OUTPUT = RETURN-MESSAGE.
         APPEND  RETURN.
         L_SUBRC = 0.
      ENDIF.
   ENDIF.

ENDFORM.


*-----------------------------------------------------------
*>> PO CHANGE (Mark of Fixed Exchange Rate)
*-----------------------------------------------------------
FORM  P3000_PO_FIXED_RATE_CHANGE  TABLES IT_EBELN STRUCTURE  EKKO
                                         RETURN   STRUCTURE  BAPIRET2
                                  USING  P_MODE.
DATA : W_KUFIX  LIKE EKKO-KUFIX.

  LOOP AT IT_EBELN.
    CLEAR : BAPIMEPOHEADER,
            BAPIMEPOHEADERX.
    MOVE : 'X'            TO      BAPIMEPOHEADERX-EXCH_RATE,
           'X'            TO      BAPIMEPOHEADERX-EX_RATE_FX,
           IT_EBELN-EBELN TO      BAPIMEPOHEADER-PO_NUMBER,
           IT_EBELN-WKURS TO      BAPIMEPOHEADER-EXCH_RATE,
           P_MODE         TO      BAPIMEPOHEADER-EX_RATE_FX.

    SELECT SINGLE * FROM EKKO
                    WHERE EBELN EQ IT_EBELN-EBELN.
    IF SY-SUBRC EQ 0.
       IF EKKO-WKURS EQ IT_EBELN-WKURS AND
          EKKO-KUFIX EQ P_MODE.
          CONTINUE.
       ENDIF.
    ELSE.
       CONTINUE.
    ENDIF.

    DO.
       CALL FUNCTION 'ZIM_BAPI_PO_CHANGE'
            EXPORTING
               PURCHASEORDER = IT_EBELN-EBELN
               POHEADER      = BAPIMEPOHEADER
               POHEADERX     = BAPIMEPOHEADERX
            TABLES
               RETURN        = XRETURN.

       LOOP AT XRETURN WHERE TYPE EQ 'E'.
          IF P_MODE EQ SPACE.
             MOVE-CORRESPONDING  XRETURN  TO  RETURN.
             APPEND RETURN.
          ENDIF.
       ENDLOOP.
       IF SY-SUBRC EQ 0.
          IF P_MODE EQ SPACE.
             CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
             RAISE POST_ERROR.
          ENDIF.
       ELSE.
          EXIT.
       ENDIF.
       WAIT UP TO 2 SECONDS.
    ENDDO.

*  UPDATE TERM  .
    DO.
        SELECT SINGLE KUFIX INTO W_KUFIX FROM EKKO
         WHERE EBELN = IT_EBELN-EBELN.

        IF W_KUFIX EQ P_MODE.
            EXIT.
        ENDIF.
        WAIT UP TO 1 SECONDS.
    ENDDO.
  ENDLOOP.

ENDFORM.

*--------------------------------------------------------------
*> LIV DATA MAKE...
*--------------------------------------------------------------
FORM   P2000_MAKE_LIV_DATA.
FIELD-SYMBOLS: <F_BP>,
               <F_BA>.

DATA: L_FIELDNM(30) TYPE C.

DATA : W_WRBTR     LIKE  ZTBKPF-DMBTR,
       L_WAERS     LIKE  ZTBKPF-HWAER,
       W_ITEM_TEXT LIKE  BAPI_INCINV_CREATE_ITEM-ITEM_TEXT,
       W_LENGTH    TYPE  I.

*>> HEADER DATA
  IF ZTBKPF-ZFPCUR EQ 'X'.
     WRITE ZTBKPF-DMBTR TO  W_TEXT_AMOUNT  CURRENCY  ZTBKPF-HWAER.
     L_WAERS = ZTBKPF-HWAER.
  ELSE.
     WRITE ZTBKPF-WRBTR TO  W_TEXT_AMOUNT  CURRENCY  ZTBKPF-WAERS.
     L_WAERS = ZTBKPF-WAERS.
  ENDIF.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.

  CLEAR : TCURC.
  SELECT SINGLE * FROM TCURC WHERE WAERS  EQ  L_WAERS.

  MOVE : ZTBKPF-ZTERM    TO  HEADERDATA-PMNTTRMS,
         'X'             TO  HEADERDATA-INVOICE_IND,
         ZTBKPF-BUKRS    TO  HEADERDATA-COMP_CODE,
         ZTBKPF-LIFNR    TO  HEADERDATA-DIFF_INV,
         W_TEXT_AMOUNT   TO  HEADERDATA-GROSS_AMOUNT,
         L_WAERS         TO  HEADERDATA-CURRENCY,
         TCURC-ISOCD     TO  HEADERDATA-CURRENCY_ISO,
         ZTBKPF-XMWST    TO  HEADERDATA-CALC_TAX_IND,
         ZTBKPF-ZLSPR    TO  HEADERDATA-PMNT_BLOCK,
         ZTBKPF-BLDAT    TO  HEADERDATA-DOC_DATE,
         ZTBKPF-BUDAT    TO  HEADERDATA-PSTNG_DATE,
         ZTBKPF-ZFBDT    TO  HEADERDATA-BLINE_DATE,
         SY-UNAME        TO  HEADERDATA-PERSON_EXT,
         ZTBKPF-XBLNR    TO  HEADERDATA-REF_DOC_NO,
         0               TO  HEADERDATA-DSCT_DAYS1,
         0               TO  HEADERDATA-DSCT_DAYS2,
         0               TO  HEADERDATA-NETTERMS,
         0               TO  HEADERDATA-DSCT_PCT1,
         0               TO  HEADERDATA-DSCT_PCT2,
         SPACE           TO  HEADERDATA-IV_CATEGORY.
** inserted by furong on 07/20/2005
 MOVE : ZTBKPF-ZFOPBN    TO  HEADERDATA-PAYEE_PAYER.
** end of insertion

*--------------------------------------------------------------------
*> ITEM
*--------------------------------------------------------------------

  PERFORM PO_GROUP_COST  TABLES IT_ZSBDIV
                                IT_PO_SUM.

  LOOP AT IT_PO_SUM.

     ADD    1    TO     W_LINE.

     ">> Sbusequent Debit/Credit Set Yes/No.
     CLEAR : KONV.
     IF ZTBKPF-TBTKZ IS INITIAL.
        SELECT * FROM KONV UP TO 1 ROWS
              WHERE KNUMV   EQ     IT_PO_SUM-KNUMV
              AND   KPOSN   EQ     IT_PO_SUM-EBELP
              AND   KSCHL   EQ     IT_PO_SUM-COND_TYPE
              AND   KNTYP   EQ     'B'.
        ENDSELECT.
        IF SY-SUBRC NE 0.
           MESSAGE E609 WITH IT_PO_SUM-EBELN IT_PO_SUM-EBELP
                             IT_PO_SUM-COND_TYPE
                        RAISING POST_ERROR.
        ENDIF.
     ENDIF.

     ">> Import Expense Description
     SELECT SINGLE * FROM ZTIMIMG08
            WHERE  ZFCDTY  EQ IT_PO_SUM-ZFCSTGRP
            AND    ZFCD    EQ IT_PO_SUM-ZFCD.

      " L/C No, B/L No Get.
     CLEAR : W_LCNO, W_BLNO.
     PERFORM  P2000_ASIGNMENT_GET USING IT_PO_SUM-ZFCSTGRP
                                        IT_PO_SUM-ZFIMDNO
                                        IT_PO_SUM-EBELN
                                        IT_PO_SUM-EBELP
                               CHANGING W_LCNO
                                        W_BLNO.

     W_LENGTH  =  STRLEN( W_BLNO ).
     CONCATENATE  W_BLNO  IT_PO_SUM-MATNR  INTO  W_ITEM_TEXT
                          SEPARATED BY SPACE.

     IF IT_PO_SUM-TBTKZ IS INITIAL.
        CLEAR : ITEMDATA-DE_CRE_IND.
     ELSE.
        MOVE : 'X'  TO  ITEMDATA-DE_CRE_IND.
     ENDIF.
     IF ZTBKPF-ZFPCUR EQ 'X'.
        WRITE IT_PO_SUM-DMBTR TO  W_TEXT_AMOUNT
                                  CURRENCY  HEADERDATA-CURRENCY.
     ELSE.
        WRITE IT_PO_SUM-WRBTR TO  W_TEXT_AMOUNT
                                  CURRENCY  HEADERDATA-CURRENCY.
     ENDIF.
     PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.

     IF IT_PO_SUM-ZFCSTGRP EQ '006' AND IT_PO_SUM-ZFCD EQ '001'.
     ELSE.
        IF W_TEXT_AMOUNT EQ '0'. CONTINUE. ENDIF.
     ENDIF.

     MOVE : W_LINE                 TO  ITEMDATA-INVOICE_DOC_ITEM,
            IT_PO_SUM-EBELN        TO  ITEMDATA-PO_NUMBER,
            IT_PO_SUM-EBELP        TO  ITEMDATA-PO_ITEM,
            IT_PO_SUM-BELNRH       TO  ITEMDATA-REF_DOC,
            IT_PO_SUM-GJAHRH       TO  ITEMDATA-REF_DOC_YEAR,
            IT_PO_SUM-BUZEIH       TO  ITEMDATA-REF_DOC_IT,
            W_BLNO                 TO  ITEMDATA-ITEM_TEXT,
            IT_PO_SUM-TBTKZ        TO  ITEMDATA-DE_CRE_IND,
            IT_PO_SUM-MWSKZ        TO  ITEMDATA-TAX_CODE,
            SPACE                  TO  ITEMDATA-TAXJURCODE,
            W_TEXT_AMOUNT          TO  ITEMDATA-ITEM_AMOUNT,
            IT_PO_SUM-MENGE        TO  ITEMDATA-QUANTITY,
            IT_PO_SUM-MEINS        TO  ITEMDATA-PO_UNIT,
            SPACE                  TO  ITEMDATA-PO_UNIT_ISO,
            IT_PO_SUM-BPRME        TO  ITEMDATA-PO_PR_UOM,
            SPACE                  TO  ITEMDATA-PO_PR_UOM_ISO,
            IT_PO_SUM-COND_TYPE    TO  ITEMDATA-COND_TYPE,
            W_ITEM_TEXT            TO  HEADERDATA-PAYMT_REF.

     IF IT_PO_SUM-BPRME NE IT_PO_SUM-MEINS.
        CALL FUNCTION 'CF_UT_UNIT_CONVERSION'
             EXPORTING
                MATNR_IMP     = IT_PO_SUM-MATNR
                UNIT_NEW_IMP  = IT_PO_SUM-BPRME
                UNIT_OLD_IMP  = IT_PO_SUM-MEINS
                VALUE_OLD_IMP = 1
             IMPORTING
                VALUE_NEW_EXP = NEW_LFIMG
             EXCEPTIONS
                OVERFLOW      = 1
                OTHERS        = 2.
        W_ZFPRQN = IT_PO_SUM-MENGE * NEW_LFIMG.
     ENDIF.
     MOVE : W_ZFPRQN               TO  ITEMDATA-PO_PR_QNT.

     APPEND ITEMDATA.
  ENDLOOP.

  ">> TAX DATA.
  IF NOT ZTBKPF-WMWST IS INITIAL.
     WRITE ZTBKPF-WMWST TO  W_TEXT_AMOUNT
                            CURRENCY  HEADERDATA-CURRENCY.
     PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.
     CLEAR : TAXDATA.
     MOVE : ZTBKPF-MWSKZ   TO   TAXDATA-TAX_CODE,
            W_TEXT_AMOUNT  TO   TAXDATA-TAX_AMOUNT.
     APPEND  TAXDATA.
  ENDIF.
*  Populate Withholding Tax Data
* Begin of changes - UD1K921861
       refresh it_tab. clear it_tab.
      select * from lfbw into table it_tab
        where  lifnr = ZTBKPF-LIFNR and
               BUKRS = ZTBKPF-BUKRS.
      if not it_tab[] is initial.
      loop at it_tab where WT_SUBJCT eq 'X'.
       WITHTAXDATA-WI_TAX_TYPE = it_tab-WITHT.
       WITHTAXDATA-WI_TAX_CODE = it_tab-WT_WITHCD.
       WITHTAXDATA-WI_TAX_BASE = 0."HEADERDATA-GROSS_AMOUNT.
       WITHTAXDATA-WI_TAX_AMT  =  0 .
       WITHTAXDATA-WI_TAX_WITHHELD_AMT = 0.
       append WITHTAXDATA.
      endloop.
     else.
        refresh  WITHTAXDATA . clear WITHTAXDATA.
     endif.
* End of changes - UD1K921861


  IF NOT ZTBKPF-WMWST IS INITIAL.
     IF ZTBKPF-ZFPCUR EQ 'X'.
        WRITE ZTBKPF-DMBTR TO  W_TEXT_AMOUNT
                                  CURRENCY  HEADERDATA-CURRENCY.
     ELSE.
        WRITE ZTBKPF-WRBTR TO  W_TEXT_AMOUNT
                                  CURRENCY  HEADERDATA-CURRENCY.
     ENDIF.
     PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.

  ENDIF.

ENDFORM.

*--------------------------------------------------------------
*> Import Expense Table Update
*> KSB MODIFY.
*--------------------------------------------------------------
FORM P3000_ZTBKPF_UPDATE  USING P_MODE.

  UPDATE ZTBKPF.

  IF SY-SUBRC EQ 0.
     MOVE-CORRESPONDING  ZTBKPF TO ZTBHIS.
     SELECT MAX( ZFPSTSQ ) INTO ZTBHIS-ZFPSTSQ
            FROM   ZTBHIS
            WHERE  BUKRS    EQ    ZTBKPF-BUKRS
            AND    BELNR    EQ    ZTBKPF-BELNR
            AND    GJAHR    EQ    ZTBKPF-GJAHR.

     IF P_MODE EQ 'U'.
        SELECT SINGLE * FROM ZTBHIS
            WHERE  BUKRS    EQ    ZTBKPF-BUKRS
            AND    BELNR    EQ    ZTBKPF-BELNR
            AND    GJAHR    EQ    ZTBKPF-GJAHR
            AND    ZFPSTSQ  EQ    ZTBHIS-ZFPSTSQ.
     ELSE.
        ADD 1  TO     ZTBHIS-ZFPSTSQ.
     ENDIF.

     MOVE : ZTBKPF-ZFFIYR      TO     ZTBHIS-ZFGJAHR,
            ZTBKPF-ZFACDO      TO     ZTBHIS-ZFBELNR,
            ZTBKPF-ZFDCSTX     TO     ZTBHIS-ZFDCSTX,
            'S'                TO     ZTBHIS-SHKZG,
            'Y'                TO     ZTBHIS-ZFPOSYN,
            SY-MANDT           TO     ZTBHIS-MANDT,
            SY-UNAME           TO     ZTBHIS-ERNAM,
            SY-DATUM           TO     ZTBHIS-CDAT,
            SY-UZEIT           TO     ZTBHIS-CTME.

     IF ZTBKPF-ZFADVPT EQ 'X'.
        MOVE : ZTBKPF-ZFCLYR   TO     ZTBHIS-ZFGJAHR1,
               ZTBKPF-ZFCLNO   TO     ZTBHIS-ZFBELNR1.
     ELSEIF ZTBKPF-ZFPYPT EQ 'X'.
        MOVE : ZTBKPF-ZFPYYR   TO     ZTBHIS-ZFPYYR,
               ZTBKPF-ZFPYNO   TO     ZTBHIS-ZFPYNO.
     ENDIF.

     IF P_MODE EQ 'U'.
        UPDATE   ZTBHIS.
     ELSE.
        INSERT   ZTBHIS.
     ENDIF.

* change document -----------------------------------------------------
     CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBKPF'
          EXPORTING
                 UPD_CHNGIND    =    'U'
                 N_ZTBKPF       =    ZTBKPF
                 O_ZTBKPF       =    *ZTBKPF.
* change document -----------------------------------------------------
   ENDIF.

ENDFORM.
