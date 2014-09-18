*---------- SUBROUTINES FOR FORMS PRINTING WITH T512P/Q --------------*
* INCLUDE REPORT RPSFRM0S. FORM ROUTINES                              *
***********************************************************************
*   2.2a                                                              *
*   cszk080141                                                        *
**** DEBUG
*   correction: k11k080141
**** DEBUG
***********************************************************************
*   corrections/modifications:                                        *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
*                                                                     *
***********************************************************************
*---------------------------------------------------------------------*
*       FORM TEST_FORM                                                *
*---------------------------------------------------------------------*
*       CHECKS WHETHER THE DESIRED FORM EXISTS IN T512P AND T512Q.    *
*       CHECKS WHETHER VARIABLES CONTAINED IN T512Q-FNAME ARE         *
*       KNOWN.                                                        *
*---------------------------------------------------------------------*
*  -->  LANGU   =  LANGUAGE OF FORM BACKGROUND (T512P)                *
*  -->  MOLGA   =  COUNTRY MODIFIER                                   *
*  -->  FORML   =  FORM NAME                                          *
*  <--  RCODE   =  1 : FORM NOT IN TABLE T512P                        *
*               =  2 : FORM NOT IN TABLE T512Q                        *
*               =  3 : UNKNOWN FIELD IN T512Q                         *
*  <--  FIELD   =  FOR RCODE 1 UND 2, RETURNS THE TABLE WHERE ERROR   *
*                  OCCURRED, FOR RCODE 3, IT CONTAINS THE FIELD NAME  *
*                  IN T512Q THAT IS NOT CORRECT.                      *
*---------------------------------------------------------------------*
FORM TEST_FORM USING LANGU
                     MOLGA
                     FORML
                     RCODE
                     FIELD.

  FIELD-SYMBOLS: <F>.

  SET BLANK LINES ON.                  "Make sure empty lines print
  SELECT * FROM T512P                  "Check whether form exists
                WHERE SPRSL EQ LANGU   "In t512p
                  AND MOLGA EQ MOLGA
                  AND FORML EQ FORML.
    EXIT.
  ENDSELECT.
  IF SY-SUBRC GT 0.                    "If unsuccessful -> say so
    MOVE 1 TO RCODE.
    MOVE FORML TO FIELD.
    EXIT.
  ENDIF.

  SELECT * FROM T512Q                  "Check whether form
                INTO TABLE I512Q       "Exists in t512q
                WHERE MOLGA EQ MOLGA
                  AND FORML EQ FORML.
  IF SY-SUBRC GT 0.                    "If it's not there
    MOVE 2 TO RCODE.                   "-> say so
    MOVE FORML TO FIELD.
    EXIT.
  ENDIF.

  CLEAR: I512Q.                        "Now check the fields in t512q
  LOOP AT I512Q.
    ASSIGN (I512Q-FNAME) TO <F>.
    IF SY-SUBRC GT 0.
      MOVE I512Q-FNAME TO FIELD.
      MOVE 3 TO RCODE.                 "If not, say so
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "TEST_FORM

*---------------------------------------------------------------------*
*       FORM INIT_FORM                                                *
*---------------------------------------------------------------------*
*       INITIALIZES THE INTERNAL TABLES I512Q AND PAGEX. FILLS I512Q  *
*       WITH THE VARIABLE NAMES FROM T512Q. BUILDS THE FORM BACKGROUND*
*       IN THE INTERNAL TABLE PAGEX FROM T512P.                       *
*---------------------------------------------------------------------*
*  -->  LANGU = LANGUAGE OF FORM BACKGROUND                           *
*  -->  MOLGA =  COUNTRY MODIFIER                                   *
*  -->  FORML = FORM NAME IN T512P/Q                                  *
*  <--  RCODE = INDICATES WHETHER ABOVE FUNCTIONS HAVE COMPLETED      *
*               SUCCESSFULLY                                          *
*---------------------------------------------------------------------*
FORM INIT_FORM USING LANGU
                     MOLGA
                     FORML
                     RCODE.
  DATA: LINES LIKE SY-TFILL.           "Number of total lines in pagex

  SET BLANK LINES ON.                  "Make sure empty lines print

  CLEAR: PAGEX, I512Q, LINES.          "Start with a clean slate
  REFRESH: PAGEX, I512Q.
                                       "Fill pagex from t512p
  SELECT * FROM T512P
                WHERE SPRSL EQ LANGU
                  AND MOLGA EQ MOLGA
                  AND FORML EQ FORML.
    ADD 1 TO LINES.                    "How many lines have we
                                       "Appended to pagex
    CLEAR: PAGEX.                      "Clear the line
    WHILE LINES LT T512P-LINNO.        "Add empty lines into pagex
      MOVE SPACE TO PAGEX-TEXT1.       "For line numbers not in t512P
      APPEND PAGEX.
      ADD 1 TO LINES.
    ENDWHILE.
    MOVE T512P-LINDA TO PAGEX-TEXT1.   "Transfer the line from t512P
    APPEND PAGEX.
  ENDSELECT.
  IF SY-SUBRC GT 0.                    "If unsuccessful -> say so
    MOVE SY-SUBRC TO RCODE.
    EXIT.
  ENDIF.

  SELECT * FROM T512Q                  "Now fill i512q
                INTO TABLE I512Q
                WHERE MOLGA EQ MOLGA
                  AND FORML EQ FORML.
  MOVE SY-SUBRC TO RCODE.              "Return the code
ENDFORM.                               "INIT_ITABS

*---------------------------------------------------------------------*
*       FORM PREPARE_FORM                                             *
*---------------------------------------------------------------------*
*       WRITES THE CONTENTS OF THE VARIABLES IN I512Q-FNAME INTO      *
*       PAGEX AT THEIR CORRECT LOCATIONS.                             *
*---------------------------------------------------------------------*
*  <--  RCODE   <> 0   COULDN'T WRITE VARIABLE INTO PAGEX             *
*  <--  FIELD   =      FIELD NAME THAT COULD NOT BE WRITTEN IN PAGEX  *
*---------------------------------------------------------------------*
FORM PREPARE_FORM USING RCODE FIELD.
  FIELD-SYMBOLS: <F>.

  CLEAR: I512Q, FIELD, RCODE.

  LOOP AT I512Q.
    ASSIGN (I512Q-FNAME) TO <F>.       "Set <f> to the variable name
                                       "Contained in i512q-fname
    IF I512Q-OFFLD NE SPACE AND        "If we have to shift, we
       I512Q-OFFLD GT '00'.            "will do so
      SHIFT <F> BY I512Q-OFFLD PLACES.
    ENDIF.
    WRITE <F> TO                       "Write variable where it belongs
        PAGEX+I512Q-FOFFS(I512Q-FLENG)
        INDEX I512Q-LINNO.
    IF SY-SUBRC GT 0.                  "If unsuccessful, return
      MOVE SY-SUBRC TO RCODE.          "Field and subrc and exit
      MOVE I512Q-FNAME TO FIELD.
      EXIT.
    ENDIF.                             "Sy-subrc gt 0
  ENDLOOP.
ENDFORM.                               "PREPARE_FORM.

*---------------------------------------------------------------------*
*       FORM PRINT_FORM                                               *
*---------------------------------------------------------------------*
*       LOOP OVER THE INTERNAL TABLE PAGEX AND PRINT OUT OF THE       *
*       LINES IN THE LENGTH SPECIFIED IN THE 'REPORT' STATEMENT.      *
*---------------------------------------------------------------------*
FORM PRINT_FORM.
  CLEAR: PAGEX.
  LOOP AT PAGEX.                       "Loop over it and write the
*   WRITE: /(SY-LINSZ) PAGEX-TEXT1.    "Lines      PH9K009822
    WRITE AT (SY-LINSZ) PAGEX-TEXT1.   "Lines      PH9K009822
  ENDLOOP.
ENDFORM.                               "PRINT_FORM
*---------- SUBROUTINES FOR FORMS PRINTING WITH T512P/Q --------------*
