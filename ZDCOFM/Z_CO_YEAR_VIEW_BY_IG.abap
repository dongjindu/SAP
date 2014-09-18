FUNCTION Z_CO_YEAR_VIEW_BY_IG.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(START_YEAR) TYPE  GJAHR OPTIONAL
*"  EXPORTING
*"     VALUE(SELECT_VALUE)
*"----------------------------------------------------------------------

  DATA: BEGIN OF FIELDS OCCURS 50.
          INCLUDE STRUCTURE HELP_VALUE.
  DATA: END OF FIELDS.
  DATA: BEGIN OF VALUETAB OCCURS 50,
          LINE(80),
  END OF VALUETAB.

  __CLS FIELDS.__CLS VALUETAB.

* Fill the structure table
  FIELDS-TABNAME = 'ZHLPSTRC'.
  FIELDS-FIELDNAME = 'VALUE'.
  FIELDS-SELECTFLAG = 'X'.
  APPEND FIELDS.CLEAR FIELDS.

* Fill the value table
  DATA : $YEAR TYPE GJAHR,
         $IX TYPE I.

  IF START_YEAR IS INITIAL.
    $YEAR = '2000'.
  ELSE.
    $YEAR = START_YEAR.
  ENDIF.

  DO 100 TIMES.
    VALUETAB-LINE = $YEAR. APPEND VALUETAB.
    ADD 1 TO $YEAR.
  ENDDO.

* Call the help value screen
  CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
       EXPORTING
            CUROW                = $IX
            TITLE_IN_VALUES_LIST = 'Year'
            TITEL                = 'Year'
       IMPORTING
            SELECT_VALUE         = SELECT_VALUE
       TABLES
            FIELDS               = FIELDS
            VALUETAB             = VALUETAB.

ENDFUNCTION.
