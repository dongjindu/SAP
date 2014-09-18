*eject
*&---------------------------------------------------------------------*
*&      Form  GET_ADDFI_VALUES
*&---------------------------------------------------------------------*
*       get dynamic F4 for additonal fields                            *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ADDFI_VALUES.

  DATA: UVALUE_FIELD(60).
  DATA: UDISPLAY(1) TYPE C.
  DATA: HELP_FIELDNAME LIKE HELP_INFO-FIELDNAME.
  DATA: ULINE LIKE SY-STEPL.
  DATA: UMODE LIKE MODE.
  DATA: RETURN_TAB LIKE DDSHRETVAL                       "XDEP45K020337
                   OCCURS 0 WITH HEADER LINE.            "XDEP45K020337
  DATA: SUBRC LIKE SY-SUBRC.                             "XDEP45K020337

  CHECK ADD_FIELDS EQ YX.
  UMODE = MODE.
  GET CURSOR FIELD UVALUE_FIELD LINE ULINE.
  IF SY-SUBRC = 0 AND NOT ULINE IS INITIAL.
* get the field name
*   if called from CATSW part, replace '_W' with spaces
    REPLACE '_W ' WITH '   ' INTO UVALUE_FIELD.
    IF SY-SUBRC = 0.
* called from CATSW part, set mode = display
      UMODE = FMODE-DISPLAY.
    ENDIF.
    LOOP AT ADDFIELDS WHERE DYNPNAME = UVALUE_FIELD.
      HELP_FIELDNAME = ADDFIELDS-FIELD.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC = 0.
      IF UMODE = FMODE-DISPLAY.
        UDISPLAY = YX.
      ELSE.
        CLEAR UDISPLAY.
      ENDIF.
      REFRESH DYNPFIELDS. CLEAR DYNPFIELDS.
* F4 function module changed                              XDEP45K020337
*     call function 'HELP_VALUES_GET'
*          exporting
*               display      = udisplay
*               fieldname    = help_fieldname
*               tabname      = 'CI_CATSDB'
*          importing
*               select_value = dynpfields-fieldvalue
*          exceptions
*               others       = 1.

      CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
           EXPORTING
                TABNAME           = 'CI_CATSDB'
                FIELDNAME         = HELP_FIELDNAME
*            SEARCHHELP        = ' '
*            SHLPPARAM         = ' '
*            DYNPPROG          = ' '
*            DYNPNR            = ' '
*            DYNPROFIELD       = ' '
*            STEPL             = 0
*            VALUE             = ' '
*            MULTIPLE_CHOICE   = ' '
                DISPLAY           = UDISPLAY
*            CALLBACK_PROGRAM  = ' '
*            CALLBACK_FORM     = ' '
           TABLES
                RETURN_TAB        = RETURN_TAB
           EXCEPTIONS
                FIELD_NOT_FOUND   = 1
                NO_HELP_FOR_FIELD = 2
                INCONSISTENT_HELP = 3
                NO_VALUES_FOUND   = 4
                OTHERS            = 5
                .
      SUBRC = SY-SUBRC.
      LOOP AT RETURN_TAB.
        EXIT.
      ENDLOOP.
      DYNPFIELDS-FIELDVALUE = RETURN_TAB-FIELDVAL.

      IF SUBRC = 0 AND NOT DYNPFIELDS-FIELDVALUE IS INITIAL.
* F4 function module changed  (end)                       XDEP45K020337
        DYNPFIELDS-FIELDNAME = UVALUE_FIELD.
        DYNPFIELDS-STEPL      = ULINE.
        APPEND DYNPFIELDS.
        CALL FUNCTION 'DYNP_VALUES_UPDATE'
             EXPORTING
                  DYNAME               = 'SAPLZCATS'
                  DYNUMB               = '2003'
             TABLES
                  DYNPFIELDS           = DYNPFIELDS
             EXCEPTIONS
                  INVALID_ABAPWORKAREA = 01
                  INVALID_DYNPROFIELD  = 02
                  INVALID_DYNPRONAME   = 03
                  INVALID_DYNPRONUMMER = 04
                  INVALID_REQUEST      = 05
                  NO_FIELDDESCRIPTION  = 06
                  UNDEFIND_ERROR       = 07.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                               " GET_ADDFI_VALUES
*&---------------------------------------------------------------------*
*&      Form  get_ot_values
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ot_values.

  DATA: UVALUE_FIELD  TYPE  DFIES-FIELDNAME.

  DATA: RETURN_TAB LIKE DDSHRETVAL OCCURS 0 WITH HEADER LINE.

  DATA: SUBRC LIKE SY-SUBRC.

  DATA : BEGIN OF ITAB OCCURS 0,
   ot1 type zzstdaz,
  END OF ITAB.

  GET CURSOR FIELD UVALUE_FIELD.
  IF SY-SUBRC = 0.
    itab-ot1 = '0.0'. append itab.
    itab-ot1 = '0.1'. append itab.
    itab-ot1 = '0.2'. append itab.
    itab-ot1 = '0.3'. append itab.
    itab-ot1 = '0.4'. append itab.
    itab-ot1 = '0.5'. append itab.
    itab-ot1 = '0.6'. append itab.
    itab-ot1 = '0.7'. append itab.
    itab-ot1 = '0.8'. append itab.
    itab-ot1 = '0.9'. append itab.
    itab-ot1 = '1.0'. append itab.
    itab-ot1 = '1.1'. append itab.
    itab-ot1 = '1.2'. append itab.
    itab-ot1 = '1.3'. append itab.
    itab-ot1 = '1.4'. append itab.
    itab-ot1 = '1.5'. append itab.
    itab-ot1 = '1.6'. append itab.
    itab-ot1 = '1.7'. append itab.
    itab-ot1 = '1.8'. append itab.
    itab-ot1 = '1.9'. append itab.
    itab-ot1 = '2.0'. append itab.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
         EXPORTING
           retfield = UVALUE_FIELD
           DYNPPROG = SY-REPID
           DYNPNR = SY-DYNNR
*           DYNPROFIELD = 'A'
           VALUE_ORG = 'S'
        tables
          value_tab = ITAB
          RETURN_TAB = RETURN_TAB.

    SUBRC = SY-SUBRC.
    LOOP AT RETURN_TAB.
      EXIT.
    ENDLOOP.
    DYNPFIELDS-FIELDVALUE = RETURN_TAB-FIELDVAL.

    IF SUBRC = 0 AND NOT DYNPFIELDS-FIELDVALUE IS INITIAL.
      DYNPFIELDS-FIELDNAME = UVALUE_FIELD.
      APPEND DYNPFIELDS.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
           EXPORTING
                DYNAME               = 'SAPLZCATS'
                DYNUMB               = '2002'
           TABLES
                DYNPFIELDS           = DYNPFIELDS
           EXCEPTIONS
                INVALID_ABAPWORKAREA = 01
                INVALID_DYNPROFIELD  = 02
                INVALID_DYNPRONAME   = 03
                INVALID_DYNPRONUMMER = 04
                INVALID_REQUEST      = 05
                NO_FIELDDESCRIPTION  = 06
                UNDEFIND_ERROR       = 07.


    endif.

  endif.

ENDFORM.                    " get_ot_values
