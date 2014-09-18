FUNCTION ZIM_BAPI_COST_INVOICES_CANCEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(P_ZFIMDTY) LIKE  ZTIVCD-ZFIMDTY
*"     REFERENCE(P_BUKRS) LIKE  ZTRECST-BUKRS
*"     VALUE(INVOICEDOCNUMBER) LIKE  BAPI_INCINV_FLD-INV_DOC_NO
*"     REFERENCE(FISCALYEAR) LIKE  BAPI_INCINV_FLD-FISC_YEAR
*"     REFERENCE(REASONREVERSAL) LIKE  BAPI_INCINV_FLD-REASON_REV
*"       OPTIONAL
*"     REFERENCE(POSTINGDATE) LIKE  BAPI_INCINV_FLD-PSTNG_DATE DEFAULT
*"       SY-DATUM
*"     VALUE(P_GUBUN) TYPE  C OPTIONAL
*"     VALUE(ZTBKPF) LIKE  ZTBKPF STRUCTURE  ZTBKPF OPTIONAL
*"     VALUE(MODE) TYPE  C DEFAULT 'N'
*"  EXPORTING
*"     VALUE(INVOICEDOCNUMBER_REVERSAL) LIKE
*"        BAPI_INCINV_FLD-INV_DOC_NO
*"     VALUE(FISCALYEAR_REVERSAL) LIKE  BAPI_INCINV_FLD-FISC_YEAR
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      LIV_ERROR
*"----------------------------------------------------------------------
DATA : L_GUBUN      TYPE   C,
       W_GUBUN      TYPE   C,
       L_DELETE_CHK,
       W_MAX_SEQ    LIKE   ZTBHIS-ZFPSTSQ,
       L_SUBRC      LIKE   SY-SUBRC,
       L_SUBRC1     LIKE   SY-SUBRC,
       INV_DOC_NO   LIKE   BAPI_INCINV_FLD-INV_DOC_NO,
       FISC_YEAR    LIKE   BAPI_INCINV_FLD-FISC_YEAR,
       W_FISCALYEAR LIKE   BAPI_INCINV_FLD-FISC_YEAR,
       W_INVOICE    LIKE   BAPI_INCINV_FLD-INV_DOC_NO.

DATA : BEGIN OF  XRETURN OCCURS 0.
       INCLUDE STRUCTURE  BAPIRET2.
DATA : END OF XRETURN.

DATA: CTU_PARAMS LIKE CTU_PARAMS.

    SELECT SINGLE * FROM ZTIMIMG00.
    IF ZTIMIMG00-ZFBDCYN EQ 'X'.
       MOVE  'A'   TO  MODE.
    ELSE.
       MOVE  'N'   TO  MODE.
    ENDIF.

    CTU_PARAMS-DISMODE  = MODE.
    CTU_PARAMS-UPDMODE  = 'V'.
    CTU_PARAMS-CATTMODE = ' '.
    CTU_PARAMS-DEFSIZE  = ' '.
    CTU_PARAMS-RACOMMIT = 'X'.
    CTU_PARAMS-NOBINPT  = 'X'.
    CTU_PARAMS-NOBIEND  = 'X'.

*--------------------------------------------------------------
*>> INTERNAL TABLE CLEARING.
   CLEAR : IT_ZTRECST, IT_ZTBLCST, IT_ZTCGCST, *ZTBKPF,
           IT_ZTCUCLCST, RETURN,   MESSTAB.

   REFRESH : IT_ZTRECST,   IT_ZTBLCST, IT_ZTCGCST,
             IT_ZTCUCLCST, RETURN,     MESSTAB.
*--------------------------------------------------------------


*--------------------------------------------------------------
   CASE P_ZFIMDTY.
      WHEN 'RD'.
*>> Import Requested Expense
         SELECT * INTO TABLE IT_ZTRECST
                   FROM  ZTRECST
                   WHERE ZFACDO   EQ  INVOICEDOCNUMBER
                   AND   ZFFIYR   EQ  FISCALYEAR
                   AND   BUKRS    EQ  P_BUKRS.
      WHEN 'BL'.
*>> B/L Expense
         SELECT * INTO TABLE IT_ZTBLCST
                  FROM  ZTBLCST
                  WHERE ZFACDO   EQ  INVOICEDOCNUMBER
                  AND   ZFFIYR   EQ  FISCALYEAR
                  AND   BUKRS    EQ  P_BUKRS.
      WHEN 'CW'.
*>> Loading/Unloading Expense
         SELECT * INTO TABLE IT_ZTCGCST
                  FROM  ZTCGCST
                  WHERE BELNR    EQ  INVOICEDOCNUMBER
                  AND   GJAHR    EQ  FISCALYEAR
                  AND   BUKRS    EQ  P_BUKRS.
      WHEN 'CC'.
*>> Clearance Expense
         SELECT * INTO TABLE IT_ZTCUCLCST
                  FROM  ZTCUCLCST
                  WHERE ZFACDO   EQ  INVOICEDOCNUMBER
                  AND   ZFFIYR   EQ  FISCALYEAR
                  AND   BUKRS    EQ  P_BUKRS.
      WHEN 'MS'.
*>> Mothership Expense
         IF P_GUBUN EQ 'B'.
            SELECT * INTO TABLE IT_ZTMSCST
                     FROM  ZTMSCST
                     WHERE BELNR    EQ  INVOICEDOCNUMBER
                     AND   GJAHR    EQ  FISCALYEAR
                     AND   BUKRS    EQ  P_BUKRS.
         ELSEIF P_GUBUN EQ 'V'.
            SELECT * INTO TABLE IT_ZTMSCST
                     FROM  ZTMSCST
                     WHERE BELNRV   EQ  INVOICEDOCNUMBER
                     AND   GJAHRV   EQ  FISCALYEAR
                     AND   BUKRS    EQ  P_BUKRS.
         ENDIF.
*>> Charge DOC CANCEL.
      WHEN 'CS'.
         CLEAR  ZTBKPF.
         SELECT  SINGLE * FROM   ZTBKPF
                 WHERE   BUKRS   EQ  P_BUKRS
                 AND     ZFACDO  EQ  INVOICEDOCNUMBER
                 AND     ZFFIYR  EQ  FISCALYEAR.
         MOVE-CORRESPONDING  ZTBKPF  TO  *ZTBKPF.

      WHEN OTHERS.
         MESSAGE E914 WITH P_ZFIMDTY RAISING LIV_ERROR.
   ENDCASE.

   IF SY-SUBRC NE 0.
      MESSAGE E604 RAISING LIV_ERROR.
   ENDIF.

*>>> LOCK Function Process
   CASE  P_ZFIMDTY.
     WHEN 'RD'.
        LOOP  AT  IT_ZTRECST.
           CALL FUNCTION 'ENQUEUE_EZ_IM_ZTRECST'
                EXPORTING
                   ZFREQNO                =     IT_ZTRECST-ZFREQNO
                EXCEPTIONS
                   OTHERS                 =     1.
           IF SY-SUBRC NE 0.
              MESSAGE E510 WITH SY-MSGV1 'Import Cost Document'
                                IT_ZTRECST-ZFREQNO '00000'
                           RAISING  LIV_ERROR.
           ENDIF.
        ENDLOOP.
     WHEN 'BL'.
        LOOP  AT  IT_ZTBLCST.
           CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBLDOC'
                EXPORTING
                   ZFBLNO               =     IT_ZTBLCST-ZFBLNO
                EXCEPTIONS
                    OTHERS              =     1.
           IF SY-SUBRC NE 0.
              MESSAGE E510 WITH SY-MSGV1 'B/L Document'
                           IT_ZTBLCST-ZFBLNO '00000'
                          RAISING  LIV_ERROR.
           ENDIF.
        ENDLOOP.
     WHEN 'CW'.
        LOOP  AT  IT_ZTCGCST.
           CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCGHD'
                EXPORTING
                   ZFCGNO               =     IT_ZTCGCST-ZFCGNO
                EXCEPTIONS
                    OTHERS              =     1.
           IF SY-SUBRC NE 0.
              MESSAGE E510 WITH SY-MSGV1 '하역 Document'
                           IT_ZTCGCST-ZFCGNO '00000'
                          RAISING  LIV_ERROR.
           ENDIF.
        ENDLOOP.
     WHEN 'CC'.
        LOOP  AT  IT_ZTCUCLCST.
           CALL FUNCTION 'ENQUEUE_EZ_IM_ZTCUCL'
                EXPORTING
                   ZFBLNO               =     IT_ZTCUCLCST-ZFBLNO
                   ZFCLSEQ              =     IT_ZTCUCLCST-ZFCLSEQ
                EXCEPTIONS
                    OTHERS              =     1.
           IF SY-SUBRC NE 0.
              MESSAGE E510 WITH SY-MSGV1 'Customs Document'
                           IT_ZTCUCLCST-ZFBLNO IT_ZTCUCLCST-ZFCLSEQ
                          RAISING  LIV_ERROR.
           ENDIF.
        ENDLOOP.
     WHEN 'MS'.
        LOOP  AT  IT_ZTMSCST.
           CALL FUNCTION 'ENQUEUE_EZ_ZTMSHD'
                EXPORTING
                   ZFMSNO               =     IT_ZTMSCST-ZFMSNO
                EXCEPTIONS
                    OTHERS              =     1.
           IF SY-SUBRC NE 0.
              MESSAGE E510 WITH SY-MSGV1 'Mother ship Document'
                           IT_ZTMSCST-ZFMSNO ''
                          RAISING  LIV_ERROR.
           ENDIF.
        ENDLOOP.
     WHEN 'CS'.
        CALL FUNCTION 'ENQUEUE_EZ_IM_ZTBKPF'
             EXPORTING
                BUKRS                =     ZTBKPF-BUKRS
                BELNR                =     ZTBKPF-BELNR
                GJAHR                =     ZTBKPF-GJAHR
             EXCEPTIONS
                 OTHERS                 =     1.
        IF SY-SUBRC NE 0.
           MESSAGE E510 WITH SY-MSGV1 'Import Cost Document'
                             ZTBKPF-BELNR ZTBKPF-GJAHR
                        RAISING  LIV_ERROR.
        ENDIF.
   ENDCASE.

   "--------------------------------------------------------
   " Advanced Payment Process
   "--------------------------------------------------------
   IF NOT ZTBKPF-ZFCLNO  IS INITIAL AND ZTBKPF-ZFADVPT EQ 'X' AND
      ZTBKPF-ZFPOSYN EQ 'Y'.

      REFRESH : BDCDATA.
      PERFORM P2000_DYNPRO USING :
          'X' 'SAPMF05R'    '0100',
          ' ' 'RF05R-AUGBL' ZTBKPF-ZFCLNO,
          ' ' 'RF05R-BUKRS' ZTBKPF-BUKRS,
          ' ' 'RF05R-GJAHR' ZTBKPF-ZFCLYR,
          ' ' 'BDC_OKCODE'  '=RAGL'.

      PERFORM P2000_DYNPRO USING :
             'X' 'SAPLSPO2'    '0100',
             ' ' 'BDC_OKCODE'  '=OPT2'.

      PERFORM P2000_DYNPRO USING :
             'X' 'SAPMF05R'    '0300',
             ' ' 'RF05R-STGRD' REASONREVERSAL,
             ' ' 'RF05R-BUDAT' POSTINGDATE,
             ' ' 'BDC_OKCODE'  '=ENTR'.

      SET PARAMETER ID 'BLN' FIELD ''.
      SET PARAMETER ID 'GJR' FIELD ''.

      CALL TRANSACTION 'FBRA'  USING        BDCDATA
                               OPTIONS FROM CTU_PARAMS
                               MESSAGES     INTO   MESSTAB.

      L_SUBRC = SY-SUBRC.

      GET PARAMETER ID 'BLN' FIELD INV_DOC_NO.
      GET PARAMETER ID 'GJR' FIELD FISC_YEAR.

      IF INV_DOC_NO NE ZTBKPF-ZFCLNO AND NOT INV_DOC_NO IS INITIAL.
         DESCRIBE TABLE MESSTAB LINES W_LINE.
         DELETE MESSTAB INDEX W_LINE.
         L_SUBRC = 0.
      ENDIF.

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

      IF L_SUBRC NE 0.  RAISE  LIV_ERROR.  ENDIF.

      DO.
        CALL FUNCTION 'ENQUEUE_EFBKPF'
             EXPORTING
                  BELNR          = ZTBKPF-ZFCLNO
                  BUKRS          = ZTBKPF-BUKRS
                  GJAHR          = ZTBKPF-ZFCLYR
             EXCEPTIONS
                  FOREIGN_LOCK   = 1
                  SYSTEM_FAILURE = 2.

        IF SY-SUBRC EQ 0.
           CALL FUNCTION 'DEQUEUE_EFBKPF'
                EXPORTING
                     BELNR          = ZTBKPF-ZFCLNO
                     BUKRS          = ZTBKPF-BUKRS
                     GJAHR          = ZTBKPF-ZFCLYR.
           EXIT.
        ELSE.
           WAIT UP TO 1 SECONDS.
        ENDIF.
     ENDDO.
   ENDIF.

   "----------------------------------------------
   " Vendor -> Payeer
   "----------------------------------------------
   IF NOT ZTBKPF-ZFPYNO  IS INITIAL AND ZTBKPF-ZFPYPT  EQ 'X' AND
      ZTBKPF-ZFPOSYN EQ 'Y'.

      REFRESH : BDCDATA.
      PERFORM P2000_DYNPRO USING :
          'X' 'SAPMF05R'    '0100',
          ' ' 'RF05R-AUGBL' ZTBKPF-ZFPYNO,
          ' ' 'RF05R-BUKRS' ZTBKPF-BUKRS,
          ' ' 'RF05R-GJAHR' ZTBKPF-ZFPYYR,
          ' ' 'BDC_OKCODE'  '=RAGL'.

      PERFORM P2000_DYNPRO USING :
             'X' 'SAPLSPO2'    '0100',
             ' ' 'BDC_OKCODE'  '=OPT2'.

      PERFORM P2000_DYNPRO USING :
             'X' 'SAPMF05R'    '0300',
             ' ' 'RF05R-STGRD' REASONREVERSAL,
             ' ' 'RF05R-BUDAT' POSTINGDATE,
             ' ' 'BDC_OKCODE'  '=ENTR'.

      SET PARAMETER ID 'BLN' FIELD ''.
      SET PARAMETER ID 'GJR' FIELD ''.

      CALL TRANSACTION 'FBRA'  USING        BDCDATA
                               OPTIONS FROM CTU_PARAMS
                               MESSAGES     INTO   MESSTAB.

      L_SUBRC = SY-SUBRC.

      GET PARAMETER ID 'BLN' FIELD INV_DOC_NO.
      GET PARAMETER ID 'GJR' FIELD FISC_YEAR.

      IF INV_DOC_NO NE ZTBKPF-ZFPYNO AND NOT INV_DOC_NO IS INITIAL.
         DESCRIBE TABLE MESSTAB LINES W_LINE.
         DELETE MESSTAB INDEX W_LINE.
         L_SUBRC = 0.
      ENDIF.

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

      IF L_SUBRC NE 0.
         RAISE  LIV_ERROR.
      ENDIF.

      DO.
        CALL FUNCTION 'ENQUEUE_EFBKPF'
             EXPORTING
                  BELNR          = ZTBKPF-ZFCLNO
                  BUKRS          = ZTBKPF-BUKRS
                  GJAHR          = ZTBKPF-ZFCLYR
             EXCEPTIONS
                  FOREIGN_LOCK   = 1
                  SYSTEM_FAILURE = 2.

        IF SY-SUBRC EQ 0.
           CALL FUNCTION 'DEQUEUE_EFBKPF'
                EXPORTING
                     BELNR          = ZTBKPF-ZFCLNO
                     BUKRS          = ZTBKPF-BUKRS
                     GJAHR          = ZTBKPF-ZFCLYR.

           EXIT.
        ELSE.
           WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.
   ENDIF.

   SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBHIS
   FROM   ZTBHIS
   WHERE  BUKRS   EQ  ZTBKPF-BUKRS
   AND    GJAHR   EQ  ZTBKPF-GJAHR
   AND    BELNR   EQ  ZTBKPF-BELNR
   AND    ZFPOSYN EQ  'Y'.

   LOOP AT IT_ZTBHIS.

      MOVE : IT_ZTBHIS-ZFGJAHR  TO  W_FISCALYEAR,
             IT_ZTBHIS-ZFBELNR  TO  W_INVOICE.

      " LIV Document, FI Document
      SELECT * FROM EKBZ UP TO 1 ROWS
               WHERE BELNR EQ W_INVOICE
               AND   GJAHR EQ W_FISCALYEAR.
      ENDSELECT.

      IF SY-SUBRC NE 0.
         SELECT * FROM EKBE UP TO 1 ROWS
               WHERE BELNR EQ W_INVOICE
               AND   GJAHR EQ W_FISCALYEAR.
         ENDSELECT.
      ENDIF.

      IF SY-SUBRC EQ 0.

         L_GUBUN  =  'L'.
         "-------------------------------------------------
         " MM Document Cancel Process
         "-------------------------------------------------
         IF ZTIMIMG00-CSREALYN EQ 'X'.
            CLEAR : RBKP.
            SELECT SINGLE * FROM RBKP WHERE BELNR EQ W_INVOICE
                                      AND   GJAHR EQ W_FISCALYEAR.
            IF RBKP-RBSTAT NE '5'.
               " Park Document Display
               PERFORM P2000_DYNPRO USING :
                       'X' 'SAPLMR1M'    '6150',
                       ' ' 'RBKP-BELNR'  W_INVOICE ,
                       ' ' 'RBKP-GJAHR'  W_FISCALYEAR,
                       ' ' 'BDC_OKCODE'  '=RBAN'.

               " Park Document Change
               PERFORM P2000_DYNPRO USING :
                       'X' 'SAPLMR1M'    '6000',
                       ' ' 'BDC_OKCODE'  '/EPPCH'.

               " Park Document Delete
               PERFORM P2000_DYNPRO USING :
                    'X' 'SAPLMR1M'    '6000',
                    ' ' 'BDC_OKCODE'  '/EDELE'.

               " TRANSACTION CALL.
               CALL TRANSACTION 'MIR4 '  USING       BDCDATA
                                         MODE        MODE
                                         UPDATE      'V'
                                         MESSAGES    INTO   MESSTAB.
               IF SY-SUBRC EQ 0.
                  INVOICEDOCNUMBER_REVERSAL = W_INVOICE.
               ELSE.
                  CLEAR : INVOICEDOCNUMBER_REVERSAL.
               ENDIF.
            ELSE.
               PERFORM P2000_DYNPRO USING :
                       'X' 'SAPMZMM_IV'   '0100',
                       ' ' 'RBKPV-BELNR'  W_INVOICE,
                       ' ' 'RBKPV-GJAHR'  W_FISCALYEAR,
                       ' ' 'BDC_OKCODE'  '=SAVE'.

              " TRANSACTION CALL.
* CTS 주석처리...NHJ
*           CALL TRANSACTION 'ZSMMIV5051' USING       BDCDATA
*                                         OPTIONS FROM CTU_PARAMS
*                                         MESSAGES    INTO   MESSTAB.
               IF SY-SUBRC EQ 0.
                  INVOICEDOCNUMBER_REVERSAL = W_INVOICE.
               ELSE.
                  CLEAR : INVOICEDOCNUMBER_REVERSAL.
               ENDIF.
            ENDIF.
         ELSE.
            CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
                 EXPORTING
                      INVOICEDOCNUMBER           =  W_INVOICE
                      FISCALYEAR                 =  W_FISCALYEAR
                      REASONREVERSAL             =  REASONREVERSAL
                      POSTINGDATE                =  POSTINGDATE
                 IMPORTING
                 INVOICEDOCNUMBER_REVERSAL  =  INVOICEDOCNUMBER_REVERSAL
                      FISCALYEAR_REVERSAL        =  FISCALYEAR_REVERSAL
                 TABLES
                      RETURN                     =  XRETURN.
         ENDIF.

*-----------------------------------------------------------------------
         IF NOT INVOICEDOCNUMBER_REVERSAL IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
            MESSAGE S282(M8) WITH INVOICEDOCNUMBER_REVERSAL.
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
         ELSE.
            LOOP AT XRETURN.
               CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
               RETURN = XRETURN.
               APPEND RETURN.
            ENDLOOP.
            L_SUBRC = 4.
         ENDIF.
      ELSE.
         "----------------------------------------------------------
         " FI Document Cancel
         "----------------------------------------------------------
         L_GUBUN  =  'B'.

         REFRESH : BDCDATA.
         IF ZTIMIMG00-CSREALYN NE 'X'.

            " User Setting Convert.
            PERFORM  P2000_DATE_USER_CONVERT      USING POSTINGDATE
                                               CHANGING W_BUDAT.

            PERFORM P2000_DYNPRO USING :
                'X' 'SAPMF05A'    '0105',
                ' ' 'RF05A-BELNS' W_INVOICE,
                ' ' 'BKPF-BUKRS'  P_BUKRS,
                ' ' 'RF05A-GJAHS' W_FISCALYEAR,
                ' ' 'UF05A-STGRD' REASONREVERSAL,
                ' ' 'BSIS-BUDAT'  W_BUDAT,
                ' ' 'BSIS-MONAT'  SPACE,
                ' ' 'RF05A-VOIDR' SPACE,
                ' ' 'BDC_OKCODE'  '=BU'.

            SET PARAMETER ID 'BLN' FIELD ''.
            SET PARAMETER ID 'GJR' FIELD ''.

            CALL TRANSACTION 'FB08'  USING       BDCDATA
                                     MODE        MODE
                                     UPDATE      'V'
                                     MESSAGES    INTO   MESSTAB.
         ELSE.
            " Park Document Display
            PERFORM P2000_DYNPRO USING :
                'X' 'SAPMF05V'    '0100',
                ' ' 'RF05V-BELNR' W_INVOICE,
                ' ' 'RF05V-BUKRS' P_BUKRS,
                ' ' 'RF05V-GJAHR' W_FISCALYEAR,
                ' ' 'BDC_OKCODE'  '/00'.
            " Park Document Delete
            PERFORM P2000_DYNPRO USING :
                'X' 'SAPLF040'    '0700',
                ' ' 'BDC_OKCODE'  '=BL'.
            " Confirm(Yes)
            PERFORM P2000_DYNPRO USING :
                'X' 'SAPLSPO1'    '0200',
                ' ' 'BDC_OKCODE'  '=YES'.

            CALL TRANSACTION 'FBV0'  USING       BDCDATA
                                     MODE        MODE
                                     UPDATE      'V'
                                     MESSAGES    INTO   MESSTAB.
         ENDIF.
         L_SUBRC = SY-SUBRC.

         IF L_SUBRC NE 0.      ">> ERROR 발생시.
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
         ELSE.                 ">> SUCCESS 시.
            IF ZTIMIMG00-CSREALYN NE 'X'.
               CLEAR : INVOICEDOCNUMBER_REVERSAL, FISCALYEAR_REVERSAL.
               GET PARAMETER ID 'BLN' FIELD INVOICEDOCNUMBER_REVERSAL.
               GET PARAMETER ID 'GJR' FIELD FISCALYEAR_REVERSAL.
            ELSE.
               INVOICEDOCNUMBER_REVERSAL = W_INVOICE.
               FISCALYEAR_REVERSAL       = W_FISCALYEAR.
            ENDIF.

            " Error Occured
            IF INVOICEDOCNUMBER_REVERSAL IS INITIAL.
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
            ELSE.
               IF ZTIMIMG00-CSREALYN EQ 'X'.
                  MESSAGE  S977 WITH
                          'Cancelation is completed succesfully.'.
               ELSE.
                  MESSAGE S282(M8) WITH INVOICEDOCNUMBER_REVERSAL.
               ENDIF.
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
               "--------------------------------------------------
               " Settlement Account Cancel.
               "--------------------------------------------------
               IF P_ZFIMDTY EQ 'CS'.
                  SELECT * FROM ZTBDIV
                  WHERE  BUKRS  EQ    ZTBKPF-BUKRS
                  AND    BELNR  EQ    ZTBKPF-BELNR
                  AND    GJAHR  EQ    ZTBKPF-GJAHR.
                     IF ZTBDIV-ZFSETYN EQ 'X'.
                        MOVE : ZTBDIV-MATNR   TO  IT_MR22-MATNR,
                               ZTBDIV-WERKS   TO  IT_MR22-WERKS,
                               ZTBDIV-WAERS   TO  IT_MR22-WAERS,
                               ZTBDIV-BUKRS   TO  IT_MR22-BUKRS,
                               ZTBDIV-BELNR   TO  IT_MR22-BELNR,
                               ZTBDIV-GJAHR   TO  IT_MR22-GJAHR,
                               ZTBDIV-BUZEI   TO  IT_MR22-BUZEI,
                               ZTBDIV-ZFBSEQ  TO  IT_MR22-ZFBSEQ.
                        IT_MR22-DMBTR = ZTBDIV-DMBTR * -1.
                        APPEND IT_MR22.
                     ENDIF.
                  ENDSELECT.
                  " MR22 CALL.
                  DESCRIBE TABLE IT_MR22 LINES W_LINE.
                  IF W_LINE GT 0.
                     PERFORM  P3000_MR22_CALL USING  INVOICEDOCNUMBER.
                  ENDIF.
               ENDIF.
            ENDIF.
         ENDIF.
      ENDIF.

*--------------------------------------------------------------
*>>> DB UPDATE.(SUCCESS시 UPDATE)
*--------------------------------------------------------------
      IF L_SUBRC EQ 0.
         CASE P_ZFIMDTY.
            WHEN 'RD'.
               LOOP AT IT_ZTRECST.
                  W_TABIX  =  SY-TABIX.
                  MOVE : SPACE       TO   IT_ZTRECST-ZFACDO,
                         SPACE       TO   IT_ZTRECST-ZFFIYR,
                         SY-UNAME    TO   IT_ZTRECST-UNAM,
                         SY-DATUM    TO   IT_ZTRECST-UDAT.

                  MODIFY IT_ZTRECST  INDEX   W_TABIX.
               ENDLOOP.
               MODIFY ZTRECST FROM TABLE IT_ZTRECST.
            WHEN 'BL'.
               LOOP AT IT_ZTBLCST.
                  W_TABIX  =  SY-TABIX.
                  MOVE : SPACE       TO   IT_ZTBLCST-ZFACDO,
                         SPACE       TO   IT_ZTBLCST-ZFFIYR,
                         SY-UNAME    TO   IT_ZTBLCST-UNAM,
                         SY-DATUM    TO   IT_ZTBLCST-UDAT.

                  MODIFY IT_ZTBLCST  INDEX   W_TABIX.
               ENDLOOP.
               MODIFY ZTBLCST FROM TABLE IT_ZTBLCST.
            WHEN 'CG'.
               LOOP AT IT_ZTCGCST.
                  W_TABIX  =  SY-TABIX.
                  MOVE : SPACE       TO   IT_ZTCGCST-BELNR,
                         SPACE       TO   IT_ZTCGCST-GJAHR,
                         SY-UNAME    TO   IT_ZTCGCST-UNAM,
                         SY-DATUM    TO   IT_ZTCGCST-UDAT.

                  MODIFY IT_ZTCGCST  INDEX   W_TABIX.
               ENDLOOP.
               MODIFY ZTCGCST FROM TABLE IT_ZTCGCST.
            WHEN 'CC'.
               LOOP AT IT_ZTCUCLCST.
                  W_TABIX  =  SY-TABIX.
                  MOVE : SPACE       TO   IT_ZTCUCLCST-ZFACDO,
                         SPACE       TO   IT_ZTCUCLCST-ZFFIYR,
                         SY-UNAME    TO   IT_ZTCUCLCST-UNAM,
                         SY-DATUM    TO   IT_ZTCUCLCST-UDAT.

                  MODIFY IT_ZTCUCLCST  INDEX   W_TABIX.
               ENDLOOP.
               MODIFY ZTCUCLCST FROM TABLE IT_ZTCUCLCST.
            WHEN 'MS'.
               IF P_GUBUN EQ 'B'.
                  LOOP AT IT_ZTMSCST.
                     W_TABIX  =  SY-TABIX.
                     MOVE : SPACE       TO   IT_ZTMSCST-BELNR,
                            SPACE       TO   IT_ZTMSCST-GJAHR,
                            SPACE       TO   IT_ZTMSCST-ZFPSDT,
                            SY-UNAME    TO   IT_ZTMSCST-UNAM,
                            SY-DATUM    TO   IT_ZTMSCST-UDAT.

                     MODIFY IT_ZTMSCST  INDEX   W_TABIX.
                  ENDLOOP.
               ELSEIF P_GUBUN EQ 'V'.
                  LOOP AT IT_ZTMSCST.
                     W_TABIX  =  SY-TABIX.
                     MOVE : SPACE       TO   IT_ZTMSCST-BELNRV,
                            SPACE       TO   IT_ZTMSCST-GJAHRV,
                            SPACE       TO   IT_ZTMSCST-ZFVPSDT,
                            SY-UNAME    TO   IT_ZTMSCST-UNAM,
                            SY-DATUM    TO   IT_ZTMSCST-UDAT.

                     MODIFY IT_ZTMSCST  INDEX   W_TABIX.
                  ENDLOOP.
               ENDIF.
               MODIFY ZTMSCST FROM TABLE IT_ZTMSCST.
            WHEN 'CS'.
*>> 전표 번호 CLEAR.\
               SELECT  SINGLE * FROM   ZTBKPF
                       WHERE   BUKRS   EQ  P_BUKRS
                       AND     ZFACDO  EQ  W_INVOICE
                       AND     ZFFIYR  EQ  W_FISCALYEAR.

               MOVE-CORRESPONDING  ZTBKPF  TO  *ZTBKPF.

               MOVE  : SPACE     TO  ZTBKPF-ZFACDO,
                       SPACE     TO  ZTBKPF-ZFFIYR,
                       SPACE     TO  ZTBKPF-ZFCLNO,
                       SPACE     TO  ZTBKPF-ZFCLYR,
                       'N'       TO  ZTBKPF-ZFPOSYN,
                       SY-UNAME  TO  ZTBKPF-UNAM,
                       SY-DATUM  TO  ZTBKPF-UDAT,
                       SY-UZEIT  TO  ZTBKPF-UTME.

               UPDATE  ZTBKPF
               SET     ZFACDO  = ''
                       ZFFIYR  = ''
                       ZFCLNO  = ''
                       ZFCLYR  = ''
                       ZFPOSYN = 'N'
                       UNAM    = SY-UNAME
                       UDAT    = SY-DATUM
                       UTME    = SY-UZEIT
               WHERE   BUKRS   EQ  P_BUKRS
               AND     ZFACDO  EQ  W_INVOICE
               AND     ZFFIYR  EQ  W_FISCALYEAR.

* change document -----------------------------------------------------
              CALL FUNCTION 'ZIM_CHANGE_DOCUMENT_ZTBKPF'
                   EXPORTING
                        UPD_CHNGIND    =    'U'
                        N_ZTBKPF       =    ZTBKPF
                        O_ZTBKPF       =    *ZTBKPF.

*>> 비용 HISTORY TABLE INSERT.
               CLEAR  :  W_MAX_SEQ , W_GUBUN.
               SELECT  MAX( ZFPSTSQ )  INTO  W_MAX_SEQ
               FROM    ZTBHIS
               WHERE   BUKRS         EQ  P_BUKRS
               AND     GJAHR         EQ  ZTBKPF-GJAHR
               AND     BELNR         EQ  ZTBKPF-BELNR.
               IF SY-SUBRC NE 0  OR  W_MAX_SEQ IS INITIAL.
                  W_MAX_SEQ  =  0.
               ENDIF.
               W_MAX_SEQ  =  W_MAX_SEQ  +  1.
               IF L_GUBUN  EQ  'L'.  W_GUBUN  =   'X'.  ENDIF.

               MOVE :  SY-MANDT                   TO  ZTBHIS-MANDT,
                       P_BUKRS                    TO  ZTBHIS-BUKRS,
                       ZTBKPF-GJAHR               TO  ZTBHIS-GJAHR,
                       ZTBKPF-BELNR               TO  ZTBHIS-BELNR,
                       W_MAX_SEQ                  TO  ZTBHIS-ZFPSTSQ,
                       POSTINGDATE                TO  ZTBHIS-BUDAT,
                       'H'                        TO  ZTBHIS-SHKZG,
                       'N'                        TO  ZTBHIS-ZFPOSYN,
                       FISCALYEAR_REVERSAL        TO  ZTBHIS-ZFGJAHR,
                       INVOICEDOCNUMBER_REVERSAL  TO  ZTBHIS-ZFBELNR,
                       FISC_YEAR                  TO  ZTBHIS-ZFGJAHR1,
                       INV_DOC_NO                 TO  ZTBHIS-ZFBELNR1,
                       W_GUBUN                    TO  ZTBHIS-ZFDCSTX,
                       SY-UNAME                   TO  ZTBHIS-ERNAM,
                       SY-DATUM                   TO  ZTBHIS-CDAT,
                       SY-UZEIT                   TO  ZTBHIS-CTME.
                INSERT  ZTBHIS.
                IF SY-SUBRC  NE  0.
                   MESSAGE E208 RAISING LIV_ERROR.
                ENDIF.
                UPDATE  ZTBHIS
                SET     ZFPOSYN   =  'N'
                WHERE   ZFGJAHR   =  W_FISCALYEAR
                AND     ZFBELNR   =  W_INVOICE.
                IF SY-SUBRC NE 0.
                   MESSAGE E208 RAISING LIV_ERROR.
                ENDIF.
            WHEN OTHERS.
         ENDCASE.
      ENDIF.
      WAIT UP TO 2 SECONDS.
   ENDLOOP.

   L_SUBRC1 = SY-SUBRC.

*>>> UNLOCK 기능 추가..
   CASE  P_ZFIMDTY.
     WHEN 'RD'.
        LOOP  AT  IT_ZTRECST.
           CALL FUNCTION 'DEQUEUE_EZ_IM_ZTRECST'
                EXPORTING
                   ZFREQNO                =     IT_ZTRECST-ZFREQNO.
        ENDLOOP.
     WHEN 'BL'.
        LOOP  AT  IT_ZTBLCST.
           CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBLDOC'
                EXPORTING
                   ZFBLNO               =     IT_ZTBLCST-ZFBLNO.
        ENDLOOP.
     WHEN 'CW'.
        LOOP  AT  IT_ZTCGCST.
           CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCGHD'
                EXPORTING
                   ZFCGNO               =     IT_ZTCGCST-ZFCGNO.
        ENDLOOP.
     WHEN 'CC'.
        LOOP  AT  IT_ZTCUCLCST.
           CALL FUNCTION 'DEQUEUE_EZ_IM_ZTCUCL'
                EXPORTING
                   ZFBLNO               =     IT_ZTCUCLCST-ZFBLNO
                   ZFCLSEQ              =     IT_ZTCUCLCST-ZFCLSEQ.
        ENDLOOP.
     WHEN 'MS'.
         LOOP  AT  IT_ZTMSCST.
            CALL FUNCTION 'DEQUEUE_EZ_ZTMSHD'
                 EXPORTING
                    ZFMSNO               =     IT_ZTMSCST-ZFMSNO.
         ENDLOOP.
*>> 비용 DOC 관련 TABLE UNLOCK.
      WHEN 'CS'.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTBKPF'
             EXPORTING
                MANDT                =     ZTBKPF-MANDT
                BUKRS                =     ZTBKPF-BUKRS
                BELNR                =     ZTBKPF-BELNR
                GJAHR                =     ZTBKPF-GJAHR
             EXCEPTIONS
                 OTHERS                 =     1.
   ENDCASE.

   IF L_SUBRC1 NE 0.
      RAISE LIV_ERROR.
   ENDIF.

   IF L_SUBRC EQ 0.
      SELECT SINGLE * FROM ZTIMIMG00.
      SELECT SINGLE * FROM  ZTBSEG
             WHERE BUKRS  EQ  *ZTBKPF-BUKRS
             AND   BELNR  EQ  *ZTBKPF-BELNR
             AND   GJAHR  EQ  *ZTBKPF-GJAHR
             AND   BUZEI  EQ  '001'.

      L_DELETE_CHK = 'N'.

**>> 업무대행비...
      IF ZTIMIMG00-ZFPSMS EQ '2' AND
         ( ( ZTBKPF-ZFCSTGRP  EQ '005' AND ZTBSEG-ZFCD EQ '999' ) OR
           ( ZTBKPF-ZFCSTGRP  EQ '006' AND ZTBSEG-ZFCD EQ '999' ) ).

            CALL    FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                    EXPORTING
                       W_OK_CODE           =   'DELE'
                       BUKRS               =   ZTBKPF-BUKRS
                       GJAHR               =   ZTBKPF-GJAHR
                       ZFSTATUS            =   'D'
                       W_ZTBKPF_OLD        =  *ZTBKPF
                       W_ZTBKPF            =   ZTBKPF
*                    TABLES
*                       IT_ZSBSEG_OLD       =   IT_ZSBSEG_OLD
*                       IT_ZSBSEG           =   IT_ZSBSEG
*                       IT_ZSBDIV           =   IT_ZSBDIV
*                       IT_ZSBHIS           =   IT_ZSBHIS
                    CHANGING
                       BELNR               =   ZTBKPF-BELNR
                    EXCEPTIONS
                       ERROR_UPDATE        =   4.

      ENDIF.


*>보험비용일 경우.
      IF ZTIMIMG00-ZFPSMS EQ '2'   AND ZTIMIMG00-ZFINMT EQ '1' AND
         ZTBKPF-ZFCSTGRP  EQ '003' AND ZTBSEG-ZFCD EQ '1AB'    AND
         ZTIMIMG00-ZFATIC NE 'X'.

         SELECT * FROM ZTINSB
                WHERE    BUKRS  EQ  ZTBKPF-BUKRS
                AND      BELNR  EQ  ZTBKPF-BELNR
                AND      GJAHR  EQ  ZTBKPF-GJAHR.

            MOVE-CORRESPONDING ZTINS TO IT_ZTINS.
            CLEAR : IT_ZTINS-BELNR, IT_ZTINS-GJAHR.
            APPEND IT_ZTINS.

         ENDSELECT.

         IF SY-SUBRC EQ 0.
            MODIFY ZTINS FROM TABLE IT_ZTINS.

            CALL    FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                    EXPORTING
                       W_OK_CODE           =   'DELE'
                       BUKRS               =   ZTBKPF-BUKRS
                       GJAHR               =   ZTBKPF-GJAHR
                       ZFSTATUS            =   'D'
                       W_ZTBKPF_OLD        =  *ZTBKPF
                       W_ZTBKPF            =   ZTBKPF
                    CHANGING
                       BELNR               =   ZTBKPF-BELNR
                    EXCEPTIONS
                       ERROR_UPDATE        =   4.
         ENDIF.
      ENDIF.

      IF ZTIMIMG00-ZFPSMS EQ '2'   AND ZTIMIMG00-ZFINMT EQ '2'   AND
         ZTBKPF-ZFCSTGRP  EQ '005' AND ZTBSEG-ZFCD      EQ '1AB' AND
         ZTIMIMG00-ZFATIC NE 'X'.

         SELECT * FROM ZTINSB
                WHERE    BUKRS  EQ  ZTBKPF-BUKRS
                AND      BELNR  EQ  ZTBKPF-BELNR
                AND      GJAHR  EQ  ZTBKPF-GJAHR.

            MOVE-CORRESPONDING ZTINSB TO IT_ZTINSB.
            CLEAR : IT_ZTINSB-BELNR, IT_ZTINSB-GJAHR.
            APPEND IT_ZTINSB.

         ENDSELECT.

         IF SY-SUBRC EQ 0.
            MODIFY ZTINSB FROM TABLE IT_ZTINSB.

            CALL    FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                    EXPORTING
                       W_OK_CODE           =   'DELE'
                       BUKRS               =   ZTBKPF-BUKRS
                       GJAHR               =   ZTBKPF-GJAHR
                       ZFSTATUS            =   'D'
                       W_ZTBKPF_OLD        =  *ZTBKPF
                       W_ZTBKPF            =   ZTBKPF
                    CHANGING
                       BELNR               =   ZTBKPF-BELNR
                    EXCEPTIONS
                       ERROR_UPDATE        =   4.
         ENDIF.
      ENDIF.

      IF ZTIMIMG00-ZFPSMS NE '1' AND ZTIMIMG00-BLCSTMD EQ 'X'.
         IF ZTBKPF-ZFCSTGRP EQ '004' OR  ZTBKPF-ZFCSTGRP EQ '005'.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTBLCST
                     FROM  ZTBLCST
                     WHERE BUKRS    EQ    *ZTBKPF-BUKRS
                     AND   ZFFIYR   EQ    *ZTBKPF-ZFFIYR
                     AND   ZFACDO   EQ    *ZTBKPF-ZFACDO.
            LOOP AT IT_ZTBLCST.
               W_TABIX = SY-TABIX.
               MOVE : SPACE    TO   IT_ZTBLCST-ZFFIYR,
                      SPACE    TO   IT_ZTBLCST-ZFACDO.
               MODIFY IT_ZTBLCST INDEX  W_TABIX.
            ENDLOOP.
            IF SY-SUBRC EQ 0.
               MODIFY ZTBLCST   FROM TABLE IT_ZTBLCST.

               CALL FUNCTION 'ZIM_CHARGE_DOCUMENT_MODIFY'
                    EXPORTING
                       W_OK_CODE           =   'DELE'
                       BUKRS               =   ZTBKPF-BUKRS
                       GJAHR               =   ZTBKPF-GJAHR
                       ZFSTATUS            =   'D'
                       W_ZTBKPF_OLD        =  *ZTBKPF
                       W_ZTBKPF            =   ZTBKPF
                    CHANGING
                       BELNR               =   ZTBKPF-BELNR
                    EXCEPTIONS
                       ERROR_UPDATE        =   4.
            ENDIF.
         ENDIF.
      ENDIF.
   ENDIF.

   IF L_GUBUN  =  'L'.
      IF L_SUBRC NE 0.
         ROLLBACK WORK.
         RAISE LIV_ERROR.
      ELSE.
         COMMIT WORK.
      ENDIF.
   ELSEIF L_GUBUN  =  'B'.
      IF L_SUBRC NE 0.
         ROLLBACK WORK.
         RAISE LIV_ERROR.
      ELSE.
         COMMIT WORK.
      ENDIF.
   ENDIF.

ENDFUNCTION.
