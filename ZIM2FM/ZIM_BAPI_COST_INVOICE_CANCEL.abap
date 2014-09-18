FUNCTION ZIM_BAPI_COST_INVOICE_CANCEL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(P_ZFIVNO) LIKE  ZTIV-ZFIVNO
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
DATA : IT_ZTIVHST1  LIKE ZTIVHST1 OCCURS 0 WITH HEADER LINE.
DATA : W_ZFCIVHST   LIKE ZTIVHST-ZFCIVHST.

*>> KEY VALUE CHECK..
   IF P_ZFIVNO IS INITIAL.
      MESSAGE E412  RAISING  LIV_ERROR.
   ENDIF.

*>> COMMERCIAL INVOICE HEADER SELECT.
   SELECT SINGLE * FROM   ZTIV
                   WHERE  ZFIVNO  EQ   P_ZFIVNO.
   IF SY-SUBRC NE 0.
      MESSAGE  E413 WITH P_ZFIVNO   RAISING  LIV_ERROR.
   ENDIF.

*>
   SELECT * INTO TABLE IT_ZTIVHST1
            FROM ZTIVHST1
            WHERE ZFIVNO   EQ  P_ZFIVNO.
*>
   SELECT MAX( ZFCIVHST ) INTO W_ZFCIVHST
          FROM  ZTIVHST1
          WHERE ZFIVNO  EQ  P_ZFIVNO
          AND ( CBELNR   IS  NULL
          OR    CBELNR   EQ  SPACE ).
   IF W_ZFCIVHST IS INITIAL.
      MESSAGE E427   RAISING  LIV_ERROR.
   ENDIF.

   READ TABLE IT_ZTIVHST1 WITH KEY ZFCIVHST = W_ZFCIVHST.
   IF NOT ( IT_ZTIVHST1-BELNR EQ INVOICEDOCNUMBER AND
            IT_ZTIVHST1-GJAHR EQ FISCALYEAR ).
      MESSAGE E428    RAISING  LIV_ERROR.
   ENDIF.

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


*-----------------------------------------------------------------------
   IF RETURN[] IS INITIAL.
      COMMIT WORK.

      SELECT * UP TO 1 ROWS
               FROM ZTIVHST1
               WHERE ZFIVNO    EQ   P_ZFIVNO
               AND   GJAHR     EQ   FISCALYEAR
               AND   BELNR     EQ   INVOICEDOCNUMBER.

      ENDSELECT.

   CALL FUNCTION 'ZIM_BDC_DATE_CONVERT_EXTERNAL'
        EXPORTING
           I_DATE     =      ZTIVHST1-CBUDAT
        IMPORTING
            E_DATE     =     CONVERT_DATE3
        EXCEPTIONS
            OTHERS     =     4.

      MOVE : POSTINGDATE               TO      CONVERT_DATE3,
             INVOICEDOCNUMBER_REVERSAL TO      ZTIVHST1-CBELNR,
             FISCALYEAR_REVERSAL       TO      ZTIVHST1-CGJAHR,
             REASONREVERSAL            TO      ZTIVHST1-STGRD,
             SY-UNAME                  TO      ZTIVHST1-UNAM,
             SY-DATUM                  TO      ZTIVHST1-UDAT,
             SY-UZEIT                  TO      ZTIVHST1-UTME.
      UPDATE   ZTIVHST1.
      IF SY-SUBRC NE 0.
         RAISE  LIV_ERROR.
      ENDIF.

      W_ZFCIVHST = W_ZFCIVHST - 1.
      READ TABLE IT_ZTIVHST1 WITH KEY ZFCIVHST = W_ZFCIVHST.
      IF SY-SUBRC EQ 0.
         IF IT_ZTIVHST1-SHKZG EQ 'S'.      ">Â÷º¯.
            IF IT_ZTIVHST1-CBELNR IS INITIAL.
               MOVE : 'Y'                TO      ZTIV-ZFCIVST.
            ELSE.
               MOVE : 'N'                TO      ZTIV-ZFCIVST.
            ENDIF.
         ELSE.
            MOVE : 'N'                TO      ZTIV-ZFCIVST.
         ENDIF.
      ELSE.
         MOVE : 'N'                       TO      ZTIV-ZFCIVST.
      ENDIF.
      UPDATE   ZTIV.
      IF SY-SUBRC NE 0.
         RAISE   LIV_ERROR.
      ENDIF.
   ELSE.
      ROLLBACK WORK.
      RAISE   LIV_ERROR.
   ENDIF.

ENDFUNCTION.
