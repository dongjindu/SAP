FUNCTION ZTR_GET_BALANCE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS DEFAULT 'H201'
*"     REFERENCE(GJAHR) TYPE  GJAHR DEFAULT SY-DATUM(4)
*"     REFERENCE(MONTH) TYPE  MONTH DEFAULT SY-DATUM+4(2)
*"  TABLES
*"      DATE
*"      DATA STRUCTURE  FDSR
*"      BEGINNING STRUCTURE  FDSR
*"      ENDING STRUCTURE  FDSR
*"----------------------------------------------------------------------

  REFRESH : BEGINNING, ENDING.
  CLEAR   : BEGINNING, ENDING.

  DATA : FROM_DATE     LIKE SY-DATUM.


  CONCATENATE GJAHR MONTH '01' INTO FROM_DATE.

  IF MONTH = '01'.
    DATA : PREV_DATE LIKE SY-DATUM.
    PREV_DATE = FROM_DATE - 1.

*// Table Change [faglflext ==> GLT0].
    PERFORM SELECT_FAGLFLEXT_DATA TABLES BEGINNING
                                   USING BUKRS
                                         PREV_DATE(4)
                                         '13'
                                         FROM_DATE.
  ELSE.
    PERFORM SELECT_FAGLFLEXT_DATA TABLES BEGINNING
                                   USING BUKRS
                                         GJAHR
                                         MONTH
                                         FROM_DATE.
  ENDIF.
  PERFORM ADD_DATA_TO_BALANCE TABLES DATA
                                     BEGINNING
                               USING FROM_DATE.

  PERFORM CALCULATION TABLES DATA
                             BEGINNING
                             ENDING
                             DATE
                       USING FROM_DATE.


ENDFUNCTION.
