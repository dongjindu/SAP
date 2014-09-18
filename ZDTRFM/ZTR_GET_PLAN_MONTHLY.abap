FUNCTION ZTR_GET_PLAN_MONTHLY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(BUKRS) TYPE  BUKRS DEFAULT 'H201'
*"     REFERENCE(GJAHR) TYPE  GJAHR DEFAULT SY-DATUM(4)
*"     REFERENCE(MONTH) TYPE  MONTH DEFAULT SY-DATUM+4(2)
*"     VALUE(DATUM) TYPE  DATUM OPTIONAL
*"     VALUE(PLANDATA) TYPE  CHAR1 DEFAULT 'X'
*"     VALUE(ZTYPE) TYPE  ZPLAN OPTIONAL
*"     VALUE(SEQNO) TYPE  SEQN3 OPTIONAL
*"  TABLES
*"      DATE
*"      PLAN STRUCTURE  FDSR
*"----------------------------------------------------------------------

  REFRESH : PLAN.
  CLEAR   : PLAN.

  DATA : FROM_DATE     LIKE SY-DATUM.

  CONCATENATE GJAHR MONTH '01' INTO FROM_DATE.

*// DAILY(1) + Planning period(only display).
  IF ZTYPE = C_DAILY AND DATUM IS NOT INITIAL.
*// PLNNING :  PLANDATA='X'.
    PERFORM SELECT_FDSR_DATA TABLES PLAN
                                    DATE
                              USING BUKRS
                                    GJAHR
                                    FROM_DATE
                                    DATUM     "Planning period(only display)
                                    PLANDATA
                                    ZTYPE
                                    SEQNO.
  ELSE.                                       "Plan.
*// "If display==>: DATUM IS INITIAL + PLANDATA=' ')
    PERFORM SELECT_FDSR_DATA TABLES PLAN
                                    DATE
                              USING BUKRS
                                    GJAHR
                                    FROM_DATE
                                    FROM_DATE
                                    PLANDATA
                                    ZTYPE
                                    SEQNO.
  ENDIF.




ENDFUNCTION.
