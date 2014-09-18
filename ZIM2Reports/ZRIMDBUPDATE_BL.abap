*&---------------------------------------------------------------------*
*& Report  ZRIMDBUPDATE                                                *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT  ZRIMDBUPDATE   MESSAGE-ID ZIM          .

TABLES : ZTBLCST,
         ZTBDIV,
         ZTBHIS.

DATA : W_BUKRS   LIKE  ZTBDIV-BUKRS,
       W_GJAHR   LIKE  ZTBDIV-GJAHR,
       W_BELNR   LIKE  ZTBDIV-BELNR,
       W_WRBTR   LIKE  ZTBDIV-WRBTR,
       W_DMBTR   LIKE  ZTBDIV-DMBTR,
       W_HMF     LIKE  ZTIDSUSD-ZFHMAMT,
       W_MPF     LIKE  ZTIDSUSD-ZFMPAMT,
       W_DUTY    LIKE  ZTIDSUSD-ZFDUTY.

SELECTION-SCREEN SKIP 1.                           " 2 LINE SKIP
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
   SELECT-OPTIONS: S_ZFBLNO  FOR ZTBLCST-ZFBLNO  NO-EXTENSION
                                                 NO INTERVALS,
                   S_CSCD    FOR ZTBLCST-ZFCSCD  NO-EXTENSION
                                                 NO INTERVALS.
SELECTION-SCREEN END OF BLOCK B1.

*> Cost group HELP.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_CSCD-LOW.
  PERFORM   P1000_COST_CODE_HELP  USING  S_CSCD-LOW 'S_CSCD-LOW'.

*-----------------------------------------------------------------------
* START OF SELECTION ?
*-----------------------------------------------------------------------
START-OF-SELECTION.

   IF S_ZFBLNO[] IS INITIAL.
      MESSAGE E977 WITH 'Input B/L Doc. No.'.
      EXIT.
   ENDIF.
   IF S_CSCD[] IS INITIAL.
      MESSAGE E977 WITH 'Input Cost Code!'.
      EXIT.
   ENDIF.

   SELECT SINGLE *
     FROM ZTBLCST
    WHERE ZFBLNO  IN  S_ZFBLNO
      AND ZFCSCD  IN  S_CSCD.

   SELECT *
     FROM ZTBLCST
    WHERE ZFBLNO  EQ  ZTBLCST-ZFBLNO
      AND ZFACDO  EQ  ZTBLCST-ZFACDO.

      CLEAR : ZTBLCST-ZFACDO, ZTBLCST-ZFFIYR.
      UPDATE ZTBLCST.

   ENDSELECT.

   IF SY-SUBRC EQ 0.
      MESSAGE S977 WITH 'Update Success!'.
      COMMIT WORK.
   ELSE.
      MESSAGE S977 WITH 'Update Fail!.'.
      ROLLBACK WORK.
   ENDIF.
*&---------------------------------------------------------------------*
*&      Form  P1000_COST_CODE_HELP
*&---------------------------------------------------------------------*
FORM P1000_COST_CODE_HELP USING    P_ZFCD P_FIELDNAME.

  DATA : L_DISPLAY.

  DATA: DYNPROG            LIKE SY-REPID,
        DYNNR              LIKE SY-DYNNR,
        WINDOW_TITLE(30)   TYPE C.
*>> Expense code HELP.
  DATA : BEGIN OF IT_COST_HELP OCCURS 0,
         ZFCD      LIKE ZTIMIMG08-ZFCD,
         ZFCDNM    LIKE ZTIMIMG08-ZFCDNM,
         ZFCD1     LIKE ZTIMIMG08-ZFCD1,
         ZFCD5     LIKE ZTIMIMG08-ZFCD5,
         COND_TYPE LIKE ZTIMIMG08-COND_TYPE,
         END OF IT_COST_HELP.

    SELECT *
           INTO CORRESPONDING FIELDS OF TABLE IT_COST_HELP
           FROM   ZTIMIMG08
           WHERE  ZFCDTY   IN   ('004', '005').

  IF SY-SUBRC NE 0.
    MESSAGE S406.
    EXIT.
  ENDIF.

  DYNPROG = SY-REPID.
  DYNNR   = SY-DYNNR.
  WINDOW_TITLE = 'Cost Code Help'.
  CLEAR: L_DISPLAY.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'ZFCSCD'
            DYNPPROG        = DYNPROG
            DYNPNR          = DYNNR
            DYNPROFIELD     = P_FIELDNAME
            WINDOW_TITLE    = WINDOW_TITLE
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = IT_COST_HELP
       EXCEPTIONS
            PARAMETER_ERROR = 1
            NO_VALUES_FOUND = 2
            OTHERS          = 3.

  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDFORM.                    " P1000_COST_CODE_HELP
