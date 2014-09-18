* 46C
* XCF 14.10.02 note 555305
* QWKP9CK042678 19062000  note 311319
*eject
*---------------------------------------------------------------------*
*       FORM D2000_MODIFY_CONTROL                                     *
*---------------------------------------------------------------------*
*       Modify the input control                                      *
*---------------------------------------------------------------------*
FORM D2000_MODIFY_CONTROL1.
  DATA: UADDFIELD(50) TYPE C.
  DATA: UADDCOUNT(2)  TYPE C.

* To ensure proper working of the Table Control User/Administrator
* settings, this module has to be performed always.

* if tcd_configured = space.
*   now the dynpro settings are active
  IF  TC_CATSD-FIXED_COLS NE 0.                             "YIK
    KEEP_TC_CATSD_FIXED_COLS = TC_CATSD-FIXED_COLS.
  ENDIF.                                                    "YIK
  TC_CATSD-LINES = REAL_NUMBER_OF_LINES + 50.
*   make additional day/time columns invisible, protect non-working days
  PERFORM DISP_RELEVANT_COLS_FOR_HOURS USING CTRL-TC_CATSD.
*   Set program
  MPOOL = SY-REPID.
**   Process customer field selection
*  CALL FUNCTION 'FIELD_SELECTION_MODIFY_ALL'
*       EXPORTING
*            MODULPOOL    = MPOOL
*            DYNPROGRUPPE = SY-DYNGR
*            MODE         = 'C'.
 LOOP AT SCREEN.
    IF SCREEN-GROUP1 NE SPACE AND SCREEN-GROUP1 NE '000'.
      CALL FUNCTION 'FIELD_SELECTION_MODIFY_SINGLE'
                    EXPORTING MODULPOOL = 'SAPLCATS'
                              MODULPOOL_CALLING = MPOOL
                              DYNPROGRUPPE = SY-DYNGR
                              MODE  = 'C'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.


  LOOP AT SCREEN.
    IF SCREEN-INVISIBLE = ON.
      PERFORM TABLE_CONTROL USING TC_CATSD SCREEN-NAME OFF.
* adjust table addfields
      IF ADD_FIELDS EQ YX.
        LOOP AT ADDFIELDS WHERE DYNPNAME = SCREEN-NAME.
          ADDFIELDS-DELETE_CATSD = YX.
          MODIFY ADDFIELDS.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.
* modify table control
  PERFORM CONFIGURE_TABLE_CONTROL.

* begin of deletion  note 311319
* PERFORM SET_FIXED_PART_OF_TC USING NCATSD.
* end of deletion  note 311319


* customer fields
  IF ADD_FIELDS NE YX.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD1' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD2' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD3' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD4' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD5' OFF.
* begin of insertion YIK
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD6' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD7' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD8' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD9' OFF.
    PERFORM TABLE_CONTROL USING TC_CATSD 'CATS_ADDFI-FIELD10' OFF.
* end of insertion
  ELSE.
    DO ADDNUMBER TIMES.
      UADDCOUNT = UADDCOUNT + 1.
      UADDFIELD = NCATS_ADDFI_FIELD.
      UADDFIELD+40 = UADDCOUNT.
      CONDENSE UADDFIELD NO-GAPS.
      LOOP AT ADDFIELDS WHERE DYNPNAME = UADDFIELD AND
                              DELETE_CATSD NE YX.
        EXIT.
      ENDLOOP.
      IF SY-SUBRC EQ 0.
        PERFORM TABLE_CONTROL_LENGTH USING TC_CATSD UADDFIELD
                                                  ADDFIELDS-OUTPUTLEN.
      ELSE.
        PERFORM TABLE_CONTROL USING TC_CATSD UADDFIELD OFF.
      ENDIF.
    ENDDO.
  ENDIF.
*   tcd_configured = yx.
* endif.

* Screen modifications depending on application system type
  PERFORM MODIFY_FROM_SYS_TYPE.

* begin of insertion  note 311319
  PERFORM SET_FIXED_PART_OF_TC USING NCATSD.
* end of insertion  note 311319
  PERFORM SET_FIXED_PART_OF_TC USING NCATSW.        "XCF note 555305


ENDFORM.                    " D2000_MODIFY_CONTROL1
