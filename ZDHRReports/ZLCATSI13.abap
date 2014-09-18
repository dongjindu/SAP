*eject
*&---------------------------------------------------------------------*
*&      Module  D2000_MODIFY_ICATSD  INPUT
*&---------------------------------------------------------------------*
*       Change ICATSD                                                  *
*----------------------------------------------------------------------*
MODULE D2000_MODIFY_ICATSD INPUT.
  DATA: UEXT_CALL(1) TYPE C VALUE ' '.
  DATA: UEXT_LINE    LIKE SY-TABIX VALUE 0.
  DATA: UFLG_MARK(1) TYPE C VALUE ' '.
  DATA: UFLG_ENRICH(1) TYPE C VALUE YX.

* handle additional fields
  PERFORM TREAT_ADDITIONAL_FIELDS_INPUT USING CATSD.
* if no capacity then no kapid
  IF CATSD-KAPAR IS INITIAL AND
     CATSD-SPLIT IS INITIAL.
   CLEAR CATSD-KAPID.
  ENDIF.
* process input
  PERFORM MODIFY_ICATSD_AFTER_CHANGE
             USING UEXT_CALL UEXT_LINE UFLG_MARK UFLG_ENRICH CATSD.

ENDMODULE.                             " D2000_MODIFY_ICATSD  INPUT
