FUNCTION ZIM_BAPI_INVOICE_CANCEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_ZFCIVRN) LIKE  ZTCIVHD-ZFCIVRN
*"     VALUE(INVOICEDOCNUMBER) LIKE  BAPI_INCINV_FLD-INV_DOC_NO
*"     REFERENCE(FISCALYEAR) LIKE  BAPI_INCINV_FLD-FISC_YEAR
*"     REFERENCE(REASONREVERSAL) LIKE  BAPI_INCINV_FLD-REASON_REV
*"     REFERENCE(POSTINGDATE) LIKE  BAPI_INCINV_FLD-PSTNG_DATE OPTIONAL
*"  EXPORTING
*"     VALUE(INVOICEDOCNUMBER_REVERSAL) LIKE
*"        BAPI_INCINV_FLD-INV_DOC_NO
*"     VALUE(FISCALYEAR_REVERSAL) LIKE  BAPI_INCINV_FLD-FISC_YEAR
*"  TABLES
*"      RETURN STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      LIV_ERROR
*"----------------------------------------------------------------------
DATA : IT_ZTCIVHST  LIKE ZTCIVHST OCCURS 0 WITH HEADER LINE.
DATA : W_ZFCIVHST   LIKE ZTCIVHST-ZFCIVHST.

*>---------------------------------------------------------------
DATA: CTU_PARAMS LIKE CTU_PARAMS.
      CTU_PARAMS-DISMODE  = 'N'.
      CTU_PARAMS-UPDMODE  = 'V'.
      CTU_PARAMS-CATTMODE = ' '.
      CTU_PARAMS-DEFSIZE  = ' '.
      CTU_PARAMS-RACOMMIT = 'X'.
      CTU_PARAMS-NOBINPT  = 'X'.
      CTU_PARAMS-NOBIEND  = 'X'.
*>---------------------------------------------------------------

   SELECT SINGLE * FROM ZTIMIMG00.

*>> KEY VALUE CHECK..
   IF P_ZFCIVRN IS INITIAL.
      MESSAGE E213  RAISING  LIV_ERROR.
   ENDIF.

*>> COMMERCIAL INVOICE HEADER SELECT.
   SELECT SINGLE * FROM   ZTCIVHD
                   WHERE  ZFCIVRN  EQ   P_ZFCIVRN.
   IF SY-SUBRC NE 0.
      MESSAGE  E374 WITH P_ZFCIVRN   RAISING  LIV_ERROR.
   ENDIF.
*>
   SELECT * INTO TABLE IT_ZTCIVHST
            FROM ZTCIVHST
            WHERE ZFCIVRN   EQ  P_ZFCIVRN.
*>
   SELECT MAX( ZFCIVHST ) INTO W_ZFCIVHST
          FROM  ZTCIVHST
          WHERE ZFCIVRN  EQ  P_ZFCIVRN
          AND ( CBELNR   IS  NULL
          OR    CBELNR   EQ  SPACE ).
   IF W_ZFCIVHST IS INITIAL.
      MESSAGE E427   RAISING  LIV_ERROR.
   ENDIF.

   READ TABLE IT_ZTCIVHST WITH KEY ZFCIVHST = W_ZFCIVHST.
   IF NOT ( IT_ZTCIVHST-BELNR EQ INVOICEDOCNUMBER AND
            IT_ZTCIVHST-GJAHR EQ FISCALYEAR ).
      MESSAGE E428    RAISING  LIV_ERROR.
   ENDIF.

*-----------------------------------------------------------------------
* 2002.12.10 PARK 전표 처리로 인한 전표 삭제 BDC 처리.
*-----------------------------------------------------------------------
  IF ZTIMIMG00-CSREALYN EQ 'X'.
     CLEAR : RBKP.
     SELECT SINGLE * FROM RBKP WHERE BELNR EQ INVOICEDOCNUMBER
                               AND   GJAHR EQ FISCALYEAR.
     IF RBKP-RBSTAT NE '5'.
        " 임시전표 조회.
        PERFORM P2000_DYNPRO USING :
                'X' 'SAPLMR1M'    '6150',
                ' ' 'RBKP-BELNR'  INVOICEDOCNUMBER,
                ' ' 'RBKP-GJAHR'  FISCALYEAR,
                ' ' 'BDC_OKCODE'  '=RBAN'.

        " 임시전표 변경.
        PERFORM P2000_DYNPRO USING :
                'X' 'SAPLMR1M'    '6000',
                ' ' 'BDC_OKCODE'  '/EPPCH'.

        " 임시전표 삭제.
        PERFORM P2000_DYNPRO USING :
                'X' 'SAPLMR1M'    '6000',
                ' ' 'BDC_OKCODE'  '/EDELE'.

        " TRANSACTION CALL.
        CALL TRANSACTION 'MIR4'   USING       BDCDATA
                                  OPTIONS FROM CTU_PARAMS
                                  MESSAGES    INTO   MESSTAB.
        W_SUBRC = SY-SUBRC.
     ELSE.
        " 임시전표 조회.
        PERFORM P2000_DYNPRO USING :
                'X' 'SAPMZMM_IV5001' '0100',
                ' ' 'RBKPV-BELNR'    INVOICEDOCNUMBER,
                ' ' 'RBKPV-GJAHR'    FISCALYEAR,
                ' ' 'BDC_OKCODE'     '=SAVE'.

        " TRANSACTION CALL.
* CTS 주석처리....NHJ
*        CALL TRANSACTION 'ZSMMIV5051' USING       BDCDATA
*                                      OPTIONS FROM CTU_PARAMS
*                                      MESSAGES    INTO   MESSTAB.
        W_SUBRC = SY-SUBRC.

     ENDIF.

   ELSE.
*-----------------------------------------------------------------------
*>>>> CANCEL DOCUMENT...
*-----------------------------------------------------------------------
      CALL FUNCTION 'BAPI_INCOMINGINVOICE_CANCEL'
           EXPORTING
                INVOICEDOCNUMBER           =  INVOICEDOCNUMBER
                FISCALYEAR                 =  FISCALYEAR
                REASONREVERSAL             =  REASONREVERSAL
                POSTINGDATE                =  POSTINGDATE
           IMPORTING
                INVOICEDOCNUMBER_REVERSAL  =  INVOICEDOCNUMBER_REVERSAL
                FISCALYEAR_REVERSAL        =  FISCALYEAR_REVERSAL
           TABLES
                RETURN                     =  RETURN.
       IF RETURN[] IS INITIAL.
          W_SUBRC = 0.
       ELSE.
          W_SUBRC = 4.
       ENDIF.
   ENDIF.

*-----------------------------------------------------------------------
   IF W_SUBRC EQ 0.
      COMMIT WORK.

      SELECT * UP TO 1 ROWS
               FROM ZTCIVHST
               WHERE ZFCIVRN   EQ   P_ZFCIVRN
               AND   GJAHR     EQ   FISCALYEAR
               AND   BELNR     EQ   INVOICEDOCNUMBER.

      ENDSELECT.

      IF ZTIMIMG00-CSREALYN EQ 'X'.
         MOVE : SY-DATUM                  TO      ZTCIVHST-CBUDAT,
                INVOICEDOCNUMBER          TO      ZTCIVHST-CBELNR,
                FISCALYEAR                TO      ZTCIVHST-CGJAHR,
                REASONREVERSAL            TO      ZTCIVHST-STGRD,
                SY-UNAME                  TO      ZTCIVHST-UNAM,
                SY-DATUM                  TO      ZTCIVHST-UDAT,
                SY-UZEIT                  TO      ZTCIVHST-UTME.
       ELSE.
          MOVE : POSTINGDATE               TO      ZTCIVHST-CBUDAT,
                INVOICEDOCNUMBER_REVERSAL TO      ZTCIVHST-CBELNR,
                FISCALYEAR_REVERSAL       TO      ZTCIVHST-CGJAHR,
                REASONREVERSAL            TO      ZTCIVHST-STGRD,
                SY-UNAME                  TO      ZTCIVHST-UNAM,
                SY-DATUM                  TO      ZTCIVHST-UDAT,
                SY-UZEIT                  TO      ZTCIVHST-UTME.
      ENDIF.

      UPDATE   ZTCIVHST.
      IF SY-SUBRC NE 0.
         RAISE  LIV_ERROR.
      ENDIF.

      W_ZFCIVHST = W_ZFCIVHST - 1.
      READ TABLE IT_ZTCIVHST WITH KEY ZFCIVHST = W_ZFCIVHST.

      MOVE-CORRESPONDING   ZTCIVHD   TO    *ZTCIVHD.

      IF SY-SUBRC EQ 0.
         IF IT_ZTCIVHST-SHKZG EQ 'S'.      ">차변.
            IF IT_ZTCIVHST-CBELNR IS INITIAL.
               MOVE : 'Y'                TO      ZTCIVHD-ZFIVST.
            ELSE.
               MOVE : 'N'                TO      ZTCIVHD-ZFIVST.
            ENDIF.
         ELSE.
            MOVE : 'N'                TO      ZTCIVHD-ZFIVST.
         ENDIF.
      ELSE.
         MOVE : 'N'                       TO      ZTCIVHD-ZFIVST.
      ENDIF.
      MOVE: SY-UNAME      TO     ZTCIVHD-UNAM,
            SY-DATUM      TO     ZTCIVHD-UDAT.
      UPDATE   ZTCIVHD.
      IF SY-SUBRC NE 0.
         RAISE   LIV_ERROR.
      ENDIF.

*>> 이력발생.
      CALL FUNCTION  'ZIM_CHANGE_DOCUMENT_CIV'
              EXPORTING
                      UPD_CHNGIND    =    'U'
                      N_ZTCIVHD      =    ZTCIVHD
                      O_ZTCIVHD      =    *ZTCIVHD.
   ELSE.
      ROLLBACK WORK.
      RAISE   LIV_ERROR.
   ENDIF.

ENDFUNCTION.
