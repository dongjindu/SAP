FUNCTION ZTR_GET_PLAN_DATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(GJAHR) TYPE  GJAHR DEFAULT SY-DATUM(4)
*"     REFERENCE(MONTH) TYPE  MONTH DEFAULT SY-DATUM+4(2)
*"     VALUE(FIRST_MONTH) TYPE  CHAR1 DEFAULT 'M'
*"     VALUE(SECOND_MONTH) TYPE  CHAR1 DEFAULT 'M'
*"     VALUE(THIRD_MONTH) TYPE  CHAR1 DEFAULT 'M'
*"     VALUE(LATER) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(ALL_DAY) TYPE  CHAR1 DEFAULT SPACE
*"  TABLES
*"      DATE
*"----------------------------------------------------------------------


  DATA : L_DATE      TYPE D,
         LAST_DATE   TYPE D.

  REFRESH DATE.
  CLEAR   DATE.

* FIRST_MONTH
  CONCATENATE GJAHR MONTH '01' INTO L_DATE.
  PERFORM GET_LAST_DAY_OF_MONTH USING L_DATE LAST_DATE.
  PERFORM GET_DATE_RANGE TABLES DATE
                          USING FIRST_MONTH
                                L_DATE
                                LAST_DATE
                                ALL_DAY.

* SECOND_MONTH
  L_DATE = LAST_DATE + 1.
  PERFORM GET_LAST_DAY_OF_MONTH USING L_DATE LAST_DATE.
  PERFORM GET_DATE_RANGE TABLES DATE
                          USING SECOND_MONTH
                                L_DATE
                                LAST_DATE
                                ALL_DAY.
* THIRD_MONTH
  L_DATE = LAST_DATE + 1.
  PERFORM GET_LAST_DAY_OF_MONTH USING L_DATE LAST_DATE.
  PERFORM GET_DATE_RANGE TABLES DATE
                          USING THIRD_MONTH
                                L_DATE
                                LAST_DATE
                                ALL_DAY.

  IF LATER = 'X'.
    APPEND '99999999' TO DATE.
  ENDIF.



ENDFUNCTION.
