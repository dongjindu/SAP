FUNCTION ZIM_CHARGE_DOCUMENT_POST_CR.
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
          TAXDATA,    ZTIMIMG00,
          RETURN,
          ZTREQHD, ZTBL, I_INVOICE, I_CREDITMEMO.

  REFRESH : ITEMDATA, TAXDATA.

  REFRESH : IT_EBELN.

*> Import IMG GET.
  SELECT SINGLE * FROM ZTIMIMG00.
  IF ZTIMIMG00-ZFBDCYN EQ 'X'.
    MOVE 'A' TO MODE.
  ENDIF.

*> Charge Document Get.
  CALL FUNCTION 'ZIM_GET_CHARGE_DOCUMENT_CR'
        EXPORTING
             BUKRS                   =    BUKRS
             BELNR                   =    BELNR
             GJAHR                   =    GJAHR
        IMPORTING
             W_ZTBKPF                =    ZTBKPF
        TABLES
             IT_ZSBSEG               =    IT_ZSBSEG
             IT_ZSBSEG_OLD           =    IT_ZSBSEG_OLD
             IT_ZSBSEG1              =    IT_ZSBSEG1
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

  IF ZTBKPF-ZFOVROW NE 'X'.
     RAISE   POST_ERROR.
  ENDIF.
*> Invoice Verify
  I_INVOICE = 'X'.
  REFRESH : RETURN, IT_MR22.

*> Import System IMG GET.
  SELECT SINGLE * FROM ZTIMIMG00.

  MOVE-CORRESPONDING ZTBKPF  TO   *ZTBKPF.

*-----------------------------------------------------------------------
* P/O Item ACCOUNT ASSIGNMENT CHECK.
* EKPO-KNTTP(ACCOUNT ASSIGNMENT) :  K,F,P,A CASE
* IMPORT EXPENSE => T-CODE : FB01.
*-----------------------------------------------------------------------
  LOOP AT IT_ZSBSEG1.

     REFRESH : IT_ZSBDIV1.
     LOOP AT IT_ZSBDIV  WHERE BUZEI  EQ  IT_ZSBSEG1-BUZEI
                        AND   DBUZEI EQ  IT_ZSBSEG1-DBUZEI.
        MOVE-CORRESPONDING IT_ZSBDIV TO  IT_ZSBDIV1.
        APPEND  IT_ZSBDIV1.
     ENDLOOP.
     SORT  IT_ZSBDIV1  BY  BUKRS  GJAHR  BELNR  BUZEI  DBUZEI ZFBSEQ.

     CLEAR : AA_CHK.
     IF ZTIMIMG00-ZFCSTMD EQ 'S'.
        LOOP AT IT_ZSBDIV.
           CLEAR : V_KNTTP, V_SAKTO, V_KOSTL, V_ANLN1, V_AUFNR, V_POSID.
           PERFORM P3000_CHECK_PO_AA USING IT_ZSBDIV1-EBELN
                                           IT_ZSBDIV1-EBELP
                                           V_KNTTP V_SAKTO V_KOSTL
                                           V_ANLN1 V_AUFNR
                                           V_POSID.
           IF NOT V_KNTTP IS INITIAL.
              AA_CHK = 'X'.
              EXIT.
           ENDIF.
        ENDLOOP.
     ENDIF.

     IF ZTBKPF-ZFDCSTX EQ 'X'   AND AA_CHK IS INITIAL
                                AND IT_ZSBSEG1-ZFPOYN NE 'N'.

        "----------------------------------------------
        " LIV Data Make
        "----------------------------------------------
        PERFORM   P2000_MAKE_LIV_DATA_CR.

        "--------------------------------------------------
        " BAPI'S Function Call
        "--------------------------------------------------
        SET PARAMETER ID 'BLN' FIELD ''.
        SET PARAMETER ID 'GJR' FIELD ''.

        REFRESH : XRETURN.
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
                    RETURN                 = XRETURN.
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
                   RETURN                  = XRETURN.
        ENDIF.

        IF NOT INVOICEDOCNUMBER IS INITIAL.
           CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

            MESSAGE S260(M8) WITH INVOICEDOCNUMBER.
           MOVE : SY-MSGTY   TO     XRETURN-TYPE,
                  SY-MSGID   TO     XRETURN-ID,
                  SY-MSGNO   TO     XRETURN-NUMBER,
                  SY-MSGV1   TO     XRETURN-MESSAGE_V1,
                  SY-MSGV2   TO     XRETURN-MESSAGE_V2,
                  SY-MSGV3   TO     XRETURN-MESSAGE_V3,
                  SY-MSGV4   TO     XRETURN-MESSAGE_V4.

           CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                       MSGID     = XRETURN-ID
                       MSGNR     = XRETURN-NUMBER
                       MSGV1     = XRETURN-MESSAGE_V1
                       MSGV2     = XRETURN-MESSAGE_V2
                       MSGV3     = XRETURN-MESSAGE_V3
                       MSGV4     = XRETURN-MESSAGE_V4
                   IMPORTING
                       MESSAGE_TEXT_OUTPUT = XRETURN-MESSAGE.
           APPEND  XRETURN.
           W_SUBRC = 0.
           L_SUBRC = 0.
        ELSE.
           LOOP AT XRETURN.
              W_TABIX = SY-TABIX.
              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                     EXPORTING
                         MSGID     = XRETURN-ID
                         MSGNR     = XRETURN-NUMBER
                         MSGV1     = XRETURN-MESSAGE_V1
                         MSGV2     = XRETURN-MESSAGE_V2
                         MSGV3     = XRETURN-MESSAGE_V3
                         MSGV4     = XRETURN-MESSAGE_V4
                     IMPORTING
                         MESSAGE_TEXT_OUTPUT = XRETURN-MESSAGE.
              MODIFY XRETURN INDEX W_TABIX.
           ENDLOOP.

           CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
           L_SUBRC = 4.
        ENDIF.

        IF L_SUBRC EQ 0.
           L_ZFPOSYN = 'Y'.
           MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
                  INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
                  L_ZFPOSYN          TO     ZTBKPF-ZFPOSYN,
                  SY-UNAME           TO     ZTBKPF-UNAM,
                  SY-DATUM           TO     ZTBKPF-UDAT,
                  SY-UZEIT           TO     ZTBKPF-UTME.

           PERFORM P3000_ZTBKPF_UPDATE_CR  USING 'I'.
        ENDIF.
*----------------------------------------->
* Import Expense -> FI Document Create
*----------------------------------------->
     ELSE.
       " Local Currency Setting
        PERFORM  P2000_COMPANY_CURR_POST_SET_CR
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
        ">> Park Document Using
        IF ZTIMIMG00-CSREALYN EQ 'X'.
           PERFORM  P3000_SET_AP_DATA_NO_CR          USING   W_GSBER.
        ">> Not Use Park Document
        ELSE.
           PERFORM  P3000_SET_AP_POSTING_DATA_CR     USING   W_GSBER.
        ENDIF.

        IF ZTIMIMG00-CSREALYN EQ 'X'.
           SET PARAMETER ID 'BLN' FIELD ''.
           SET PARAMETER ID 'GJR' FIELD ''.
           REFRESH : MESSTAB.
           CALL TRANSACTION 'F-64'  USING       BDCDATA
                                    MODE        MODE
                                    MESSAGES    INTO   MESSTAB.

        ELSE.
           SET PARAMETER ID 'BLP' FIELD ''.
           SET PARAMETER ID 'GJR' FIELD ''.
           REFRESH : MESSTAB.
           CALL TRANSACTION 'FB01'  USING        BDCDATA
                                    MODE         MODE
                                    MESSAGES    INTO   MESSTAB.
        ENDIF.
        REFRESH : XRETURN.
        L_SUBRC = SY-SUBRC.

        IF L_SUBRC NE 0.      ">> ERROR ¹ß»ý½Ã.
           LOOP AT MESSTAB.
              MOVE : MESSTAB-MSGTYP  TO     XRETURN-TYPE,
                     MESSTAB-MSGID   TO     XRETURN-ID,
                     MESSTAB-MSGNR   TO     XRETURN-NUMBER,
                     MESSTAB-MSGV1   TO     XRETURN-MESSAGE_V1,
                     MESSTAB-MSGV2   TO     XRETURN-MESSAGE_V2,
                     MESSTAB-MSGV3   TO     XRETURN-MESSAGE_V3,
                     MESSTAB-MSGV4   TO     XRETURN-MESSAGE_V4.

              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                       MSGID     = XRETURN-ID
                       MSGNR     = XRETURN-NUMBER
                       MSGV1     = XRETURN-MESSAGE_V1
                       MSGV2     = XRETURN-MESSAGE_V2
                       MSGV3     = XRETURN-MESSAGE_V3
                       MSGV4     = XRETURN-MESSAGE_V4
                   IMPORTING
                       MESSAGE_TEXT_OUTPUT = XRETURN-MESSAGE.
              APPEND  XRETURN.
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
              MOVE : 'E'             TO     XRETURN-TYPE,
                     'ZIM'           TO     XRETURN-ID,
                     '494'           TO     XRETURN-NUMBER,
                     SPACE           TO     XRETURN-MESSAGE_V1,
                     SPACE           TO     XRETURN-MESSAGE_V2,
                     SPACE           TO     XRETURN-MESSAGE_V3,
                     SPACE           TO     XRETURN-MESSAGE_V4.

              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                   EXPORTING
                       MSGID     = XRETURN-ID
                       MSGNR     = XRETURN-NUMBER
                       MSGV1     = XRETURN-MESSAGE_V1
                       MSGV2     = XRETURN-MESSAGE_V2
                       MSGV3     = XRETURN-MESSAGE_V3
                       MSGV4     = XRETURN-MESSAGE_V4
                   IMPORTING
                       MESSAGE_TEXT_OUTPUT = XRETURN-MESSAGE.
              APPEND  XRETURN.
              RAISE   POST_ERROR.
           ELSE.
              MESSAGE S260(M8) WITH INVOICEDOCNUMBER.
              MOVE : SY-MSGTY   TO     XRETURN-TYPE,
                     SY-MSGID   TO     XRETURN-ID,
                     SY-MSGNO   TO     XRETURN-NUMBER,
                     SY-MSGV1   TO     XRETURN-MESSAGE_V1,
                     SY-MSGV2   TO     XRETURN-MESSAGE_V2,
                     SY-MSGV3   TO     XRETURN-MESSAGE_V3,
                     SY-MSGV4   TO     XRETURN-MESSAGE_V4.

              CALL FUNCTION 'MESSAGE_TEXT_BUILD'
                    EXPORTING
                        MSGID     = XRETURN-ID
                        MSGNR     = XRETURN-NUMBER
                        MSGV1     = XRETURN-MESSAGE_V1
                        MSGV2     = XRETURN-MESSAGE_V2
                        MSGV3     = XRETURN-MESSAGE_V3
                        MSGV4     = XRETURN-MESSAGE_V4
                  IMPORTING
                        MESSAGE_TEXT_OUTPUT = XRETURN-MESSAGE.
              APPEND  XRETURN.
              L_SUBRC = 0.

              "--------------------------------
              " Settlement Account
              "--------------------------------
              PERFORM  P3000_MR22_CALL USING INVOICEDOCNUMBER.

              "---------------------------------------------------
              " DB Update
              "---------------------------------------------------
              CLEAR : FI_DOC_NO,     ZTBKPF-ZFPYYR,
                      ZTBKPF-ZFPYNO, ZTBKPF-ZFCLNO, ZTBKPF-ZFCLYR.

              MOVE : FISCALYEAR         TO     ZTBKPF-ZFFIYR,
                     INVOICEDOCNUMBER   TO     ZTBKPF-ZFACDO,
                     'Y'                TO     ZTBKPF-ZFPOSYN,
                     SY-UNAME           TO     ZTBKPF-UNAM,
                     SY-DATUM           TO     ZTBKPF-UDAT,
                     SY-UZEIT           TO     ZTBKPF-UTME.

              PERFORM P3000_ZTBKPF_UPDATE_CR USING 'I'.
           ENDIF.
        ENDIF.
     ENDIF.
     LOOP AT XRETURN.
        MOVE-CORRESPONDING  XRETURN  TO  RETURN.
        APPEND RETURN.
     ENDLOOP.
     WAIT UP TO 3 SECONDS.
  ENDLOOP.

ENDFUNCTION.

*-----------------------------------------------------------------------
*> Local Currency Setting
*-----------------------------------------------------------------------
FORM  P2000_COMPANY_CURR_POST_SET_CR USING L_WAERS W_KURSF W_WWERT MODE.

DATA: L_SUBRC        LIKE   SY-SUBRC.
*>>> BDC....
    REFRESH : BDCDATA.
* Initial Screen FIELD
    IF ZTBKPF-ZFPCUR EQ 'X'.      "Local Currency Posting
       CLEAR : W_KURSF.
       L_WAERS = ZTBKPF-HWAER.
       CLEAR : W_WWERT.
    ELSE.
       WRITE : ZTBKPF-KURSF   TO   W_KURSF.
       L_WAERS = ZTBKPF-WAERS.
       WRITE ZTBKPF-WWERT TO W_WWERT.
    ENDIF.
**--------------------------
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
*                             MODE        MODE
*                             UPDATE      'V'
*                             MESSAGES     INTO   MESSTAB.   "Insert

ENDFORM.
*-----------------------------------------------------------------------
*>  General Import Expense Process
*-----------------------------------------------------------------------
FORM   P3000_SET_AP_POSTING_DATA_CR   USING   W_GSBER.

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

   READ TABLE IT_ZSBDIV1 INDEX 2.
   MOVE  IT_ZSBDIV1-EBELN  TO  W_TEXT.

   LOOP AT IT_ZSBDIV1.

       W_TABIX = SY-TABIX.

       CLEAR : V_KNTTP, V_SAKTO, V_KOSTL, V_ANLN1, V_AUFNR, V_POSID.

       " Account Assignment Check.
       PERFORM P3000_CHECK_PO_AA USING IT_ZSBDIV1-EBELN
                                       IT_ZSBDIV1-EBELP
                                       V_KNTTP V_SAKTO V_KOSTL
                                       V_ANLN1 V_AUFNR
                                       V_POSID.

       IF V_KNTTP EQ 'S'.
          MOVE : IT_ZSBDIV1-MATNR   TO  IT_MR22-MATNR,
                 IT_ZSBDIV1-WERKS   TO  IT_MR22-WERKS,
                 IT_ZSBDIV1-DMBTR   TO  IT_MR22-DMBTR,
                 IT_ZSBDIV1-WAERS   TO  IT_MR22-WAERS,
                 IT_ZSBDIV1-BUKRS   TO  IT_MR22-BUKRS,
                 IT_ZSBDIV1-BELNR   TO  IT_MR22-BELNR,
                 IT_ZSBDIV1-GJAHR   TO  IT_MR22-GJAHR,
                 IT_ZSBDIV1-DBUZEI  TO  IT_MR22-BUZEI,
                 IT_ZSBDIV1-ZFBSEQ  TO  IT_MR22-ZFBSEQ.
          APPEND IT_MR22.
       ENDIF.

       " L/C No, B/L No Get.
       CLEAR : W_LCNO, W_BLNO.
       PERFORM  P2000_ASIGNMENT_GET USING '006'
                                          IT_ZSBDIV1-ZFIMDNO
                                          IT_ZSBDIV1-EBELN
                                          IT_ZSBDIV1-EBELP
                                 CHANGING W_LCNO
                                          W_BLNO.
       IF W_LCNO IS INITIAL.
          MOVE  'N/A'  TO  W_LCNO.
       ENDIF.

      IF ZTBKPF-ZFCSTGRP EQ '003'.
         CONCATENATE  'LC: '  W_LCNO  INTO  W_TEXT.
      ELSE.
         MOVE  W_BLNO   TO  W_TEXT.
      ENDIF.

       " Amount
       IF ZTBKPF-ZFPCUR EQ 'X'.
          IF W_TABIX EQ 1.
             IF IT_ZSBSEG1-DMBTR IS INITIAL.
                W_WMWST  =  IT_ZSBSEG1-WMWST.
             ELSE.
                W_WMWST  = IT_ZSBSEG1-DMBTR.
             ENDIF.
          ELSE.
             IF IT_ZSBDIV1-DMBTR IS INITIAL.
                W_WMWST  =  IT_ZSBDIV1-WMWST.
             ELSE.
                W_WMWST  = IT_ZSBDIV1-DMBTR.
             ENDIF.
          ENDIF.
          WRITE W_WMWST TO  TEMP_WRBTR CURRENCY  IT_ZSBDIV1-HWAER.
       ELSE.
          IF W_TABIX EQ 1.
             IF IT_ZSBSEG1-WRBTR IS INITIAL.
                W_WMWST = IT_ZSBSEG1-WMWST.
             ELSE.
                W_WMWST = IT_ZSBSEG1-WRBTR .
             ENDIF.
          ELSE.
             W_WMWST = IT_ZSBDIV1-WRBTR.
          ENDIF.
          WRITE W_WMWST TO TEMP_WRBTR CURRENCY  IT_ZSBDIV1-WAERS.
       ENDIF.
       PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WRBTR.

       IF TEMP_WRBTR EQ '0'.  CONTINUE.  ENDIF.

       " Tax Amount.
       IF W_TABIX EQ 1.
          IF IT_ZSBSEG1-DMBTR IS INITIAL.
             WRITE  '0'  TO  TEMP_WMWST CURRENCY  IT_ZSBDIV1-HWAER.
          ENDIF.
       ELSE.
          WRITE '0' TO  TEMP_WMWST CURRENCY  IT_ZSBDIV1-HWAER.
       ENDIF.
       PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_WMWST.

       " Taxable Amount(Document Currency)
       WRITE IT_ZSBDIV1-FWBAS TO  TEMP_FWBAS CURRENCY  IT_ZSBDIV1-HWAER.
       PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_FWBAS.

       " Local Currency
       IF ZTBKPF-ZFPCUR EQ 'X'.
          CLEAR : W_KURSF.
          WRITE IT_ZSBDIV1-DMBTR TO TEMP_DMBTR CURRENCY ZTBKPF-HWAER.
          PERFORM    P2000_WRITE_NO_MASK  CHANGING  TEMP_DMBTR.
       ELSE.
          CLEAR : TEMP_DMBTR.
          WRITE : IT_ZSBDIV1-KURSF   TO   W_KURSF.
       ENDIF.

       IF W_TABIX EQ 1.
          PERFORM P2000_DYNPRO USING :
              ' ' 'RF05A-NEWKO'  IT_ZSBDIV1-NEWKO,
              ' ' 'RF05A-NEWBS'  IT_ZSBDIV1-NEWBS,
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
                  ' ' 'RF05A-NEWBS' IT_ZSBDIV1-NEWBS,
                  ' ' 'BDC_OKCODE'  '/00'.
             WHEN OTHERS.
                PERFORM P2000_DYNPRO USING :
                  ' ' 'RF05A-NEWBS' IT_ZSBDIV1-NEWBS,
                  ' ' 'RF05A-NEWKO' IT_ZSBDIV1-NEWKO,
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
              ' ' 'BSEG-ZUONR'  IT_ZSBDIV1-ZUONR,
              ' ' 'BSEG-KIDNO'  W_BLNO,
              ' ' 'BSEG-SGTXT'  IT_ZSBDIV1-SGTXT.
** INSERTED BY FURONG ON 07/26/2005
if ZTBKPF-ZFOPBN ne ' '.
              PERFORM P2000_DYNPRO USING :
              ' ' 'BSEG-EMPFB'  ZTBKPF-ZFOPBN.
endif.
** END OF INSERTION

          " User Setting Convert.
          PERFORM  P2000_DATE_USER_CONVERT    USING IT_ZSBDIV1-ZFBDT
                                           CHANGING W_ZFBDT.

          " Baseline Date
          IF NOT IT_ZSBDIV1-ZFBDT IS INITIAL.
             PERFORM P2000_DYNPRO USING ' ' 'BSEG-ZFBDT' W_ZFBDT.
          ENDIF.

          " Local Currency Posting
          IF ZTBKPF-ZFPCUR NE 'X'.
             PERFORM P2000_DYNPRO USING ' ' 'BSEG-DMBTR'  TEMP_DMBTR.
          ENDIF.
       ELSE.
          WRITE IT_ZSBDIV1-MENGE  TO  L_MENGE UNIT  IT_ZSBDIV1-MEINS.
          WRITE IT_ZSBDIV1-MEINS  TO  L_MEINS.

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
                  ' ' 'BSEG-MWSKZ'  IT_ZSBDIV1-MWSKZ,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV1-SGTXT.
                W_DYNNR = '0305'.
             WHEN 'F'.
                PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0300',
                  ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                  ' ' 'BSEG-MWSKZ'  IT_ZSBDIV1-MWSKZ,
                  ' ' 'BSEG-EBELN'  IT_ZSBDIV1-EBELN,
                  ' ' 'BSEG-EBELP'  IT_ZSBDIV1-EBELP,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV1-SGTXT,
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
                  ' ' 'COBL-KOSTL'  IT_ZSBDIV1-KOSTL,
                  ' ' 'BSEG-BUPLA'  IT_ZSBDIV1-BUPLA,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV1-SGTXT.
                W_DYNNR = '0300'.
             WHEN 'S'.
                PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0300',
                  ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                  ' ' 'BSEG-EBELN'  IT_ZSBDIV1-EBELN,
                  ' ' 'BSEG-EBELP'  IT_ZSBDIV1-EBELP,
                  ' ' 'BSEG-SGTXT'  IT_ZSBDIV1-SGTXT,
                  ' ' 'BSEG-MENGE'  L_MENGE,
                  ' ' 'BSEG-MEINS'  L_MEINS,
                  ' ' 'BDC_OKCODE'  '=ZK'.
                PERFORM P2000_DYNPRO USING :
                  'X' 'SAPLKACB'   '0002',
                  ' ' 'COBL-MATNR'  IT_ZSBDIV1-MATNR,
                  ' ' 'BDC_OKCODE'  '=ENTE'.
                PERFORM P2000_DYNPRO USING :
                  'X' 'SAPMF05A'    '0330',
                  ' ' 'BDC_OKCODE'  '/00'.

             WHEN OTHERS.
                  IF ZTBKPF-ZFPOYN EQ 'N'.
                     PERFORM P2000_DYNPRO USING :
                       'X' 'SAPMF05A'    '0300',
                       ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                       ' ' 'BSEG-MWSKZ'  IT_ZSBDIV1-MWSKZ,
                       ' ' 'BSEG-SGTXT'  IT_ZSBDIV1-SGTXT,
                       ' ' 'BDC_OKCODE'  '=ZK'.

                     PERFORM P2000_DYNPRO USING :
                       'X' 'SAPLKACB' '0002',
                       ' ' 'COBL-KOSTL'  IT_ZSBDIV1-KOSTL,
                       ' ' 'BDC_OKCODE'  '=ENTE'.

                     PERFORM P2000_DYNPRO USING :
                        'X' 'SAPMF05A'    '0330',
                        ' ' 'BDC_OKCODE'  '/00'.

                  ELSE.
                     PERFORM P2000_DYNPRO USING :
                       'X' 'SAPMF05A'    '0300',
                       ' ' 'BSEG-WRBTR'  TEMP_WRBTR,
                       ' ' 'BSEG-EBELN'  IT_ZSBDIV1-EBELN,
                       ' ' 'BSEG-EBELP'  IT_ZSBDIV1-EBELP,
                       ' ' 'BSEG-MWSKZ'  IT_ZSBDIV1-MWSKZ,
                       ' ' 'BSEG-MENGE'  L_MENGE,
                       ' ' 'BSEG-MEINS'  L_MEINS,
                       ' ' 'BSEG-SGTXT'  IT_ZSBDIV1-SGTXT,
                       ' ' 'BDC_OKCODE'  '=ZK'.

                     PERFORM P2000_DYNPRO USING :
                       'X' 'SAPLKACB' '0002',
                       ' ' 'COBL-MATNR'  IT_ZSBDIV1-MATNR,
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

      MOVE-CORRESPONDING IT_ZSBDIV1 TO TEMP_ZSBDIV.

   ENDLOOP.

   PERFORM P2000_DYNPRO USING :
           ' ' 'BDC_OKCODE'  '=BU'.

ENDFORM.

*--------------------------------------------------------------
*> LIV DATA MAKE...
*--------------------------------------------------------------
FORM   P2000_MAKE_LIV_DATA_CR.
FIELD-SYMBOLS: <F_BP>,
               <F_BA>.

DATA: L_FIELDNM(30) TYPE C.

DATA : W_WRBTR     LIKE  ZTBKPF-DMBTR,
       L_WAERS     LIKE  ZTBKPF-HWAER,
       W_ITEM_TEXT LIKE  BAPI_INCINV_CREATE_ITEM-ITEM_TEXT,
       W_LENGTH    TYPE  I.

  CLEAR   : HEADERDATA.
  REFRESH : ITEMDATA.

*>> HEADER DATA
  READ TABLE IT_ZSBSEG WITH KEY BUZEI = IT_ZSBSEG1-BUZEI.

  CLEAR : ZTIDSUS, ZTBL.
  SELECT SINGLE * FROM ZTIDSUS
  WHERE  ZFIVNO   EQ   IT_ZSBSEG-ZFIMDNO.

  IF ZTIDSUS-ZFENTP     EQ '06'.
     MOVE  'DOMESTIC'   TO  HEADERDATA-HEADER_TXT.
  ELSEIF ZTIDSUS-ZFENTP EQ '08'.
     MOVE  'NAFTA'      TO  HEADERDATA-HEADER_TXT.
  ELSEIF ZTIDSUS-ZFENTP EQ 'XX'.
     MOVE  'OS&D'       TO  HEADERDATA-HEADER_TXT.
  ELSE.
     MOVE  'EXPORT'     TO  HEADERDATA-HEADER_TXT.
  ENDIF.

  SELECT SINGLE * FROM ZTBL WHERE ZFBLNO EQ IT_ZSBSEG1-ZFIMDNO.

  IF ZTBKPF-ZFPCUR EQ 'X'.
     WRITE  IT_ZSBSEG1-DMBTR TO  W_TEXT_AMOUNT CURRENCY ZTBKPF-HWAER.
     L_WAERS = ZTBKPF-HWAER.
  ELSE.
     WRITE  IT_ZSBSEG1-WRBTR TO  W_TEXT_AMOUNT CURRENCY ZTBKPF-WAERS.
     L_WAERS = ZTBKPF-WAERS.
  ENDIF.
  PERFORM    P2000_WRITE_NO_MASK     CHANGING  W_TEXT_AMOUNT.

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
** Change by Furong on 11/05/10
*         ZTIDSUS-ZFENTNO TO  HEADERDATA-REF_DOC_NO,
        ZTBKPF-XBLNR     TO  HEADERDATA-REF_DOC_NO,
** End of change
         ZTIDSUS-ZFENTNO TO  HEADERDATA-HEADER_TXT,
         ZTBL-ZFHBLNO    TO  HEADERDATA-PAYMT_REF,
         0               TO  HEADERDATA-DSCT_DAYS1,
         0               TO  HEADERDATA-DSCT_DAYS2,
         0               TO  HEADERDATA-NETTERMS,
         0               TO  HEADERDATA-DSCT_PCT1,
         0               TO  HEADERDATA-DSCT_PCT2,
         SPACE           TO  HEADERDATA-IV_CATEGORY.
** inserted by furong on 07/20/2005
  MOVE: ZTBKPF-ZFOPBN    TO  HEADERDATA-PAYEE_PAYER.
** end of insertion


*--------------------------------------------------------------------
*> ITEM
*--------------------------------------------------------------------
   PERFORM PO_GROUP_COST  TABLES  IT_ZSBDIV1
                                  IT_PO_SUM.

   LOOP AT IT_PO_SUM.

     CLEAR : ITEMDATA.

     ADD    1    TO     W_LINE.

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
     ">> Subsequent Debit/Credit
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

     MOVE : W_LINE                  TO  ITEMDATA-INVOICE_DOC_ITEM,
            IT_PO_SUM-EBELN         TO  ITEMDATA-PO_NUMBER,
            IT_PO_SUM-EBELP         TO  ITEMDATA-PO_ITEM,
            IT_PO_SUM-BELNRH        TO  ITEMDATA-REF_DOC,
            IT_PO_SUM-GJAHRH        TO  ITEMDATA-REF_DOC_YEAR,
            IT_PO_SUM-BUZEIH        TO  ITEMDATA-REF_DOC_IT,
            ZTIDSUS-ZFENTNO         TO  ITEMDATA-ITEM_TEXT,
            IT_PO_SUM-TBTKZ         TO  ITEMDATA-DE_CRE_IND,
            ZTBKPF-MWSKZ            TO  ITEMDATA-TAX_CODE,
            SPACE                   TO  ITEMDATA-TAXJURCODE,
            W_TEXT_AMOUNT           TO  ITEMDATA-ITEM_AMOUNT,
            IT_PO_SUM-MENGE         TO  ITEMDATA-QUANTITY,
            IT_PO_SUM-MEINS         TO  ITEMDATA-PO_UNIT,
            SPACE                   TO  ITEMDATA-PO_UNIT_ISO,
            IT_PO_SUM-BPRME         TO  ITEMDATA-PO_PR_UOM,
            SPACE                   TO  ITEMDATA-PO_PR_UOM_ISO,
*            KONV-KSCHL              TO  ITEMDATA-COND_TYPE,
            KONV-STUNR              TO  ITEMDATA-COND_ST_NO,
            KONV-ZAEHK              TO  ITEMDATA-COND_COUNT.

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

ENDFORM.

*--------------------------------------------------------------
*> Import Expense Table Update
*> KSB MODIFY.
*--------------------------------------------------------------
FORM P3000_ZTBKPF_UPDATE_CR  USING P_MODE.

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
