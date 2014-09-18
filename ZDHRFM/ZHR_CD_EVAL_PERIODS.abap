FUNCTION ZHR_CD_EVAL_PERIODS.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(BONUS_DATE) LIKE  PC261-BONDT DEFAULT '00000000'
*"     VALUE(INPER_MODIF) LIKE  PC261-IPERM
*"     VALUE(INPER) LIKE  PC261-INPER
*"     VALUE(PAY_TYPE) LIKE  PC261-PAYTY DEFAULT ' '
*"     VALUE(PAY_IDENT) LIKE  PC261-PAYID DEFAULT ' '
*"  EXPORTING
*"     REFERENCE(RESULT) TYPE  PAY_T_EVAL_PERIOD
*"  TABLES
*"      RGDIR STRUCTURE  PC261
*"      IABKRS STRUCTURE  ABKRS_CD OPTIONAL
*"  EXCEPTIONS
*"      NO_RECORD_FOUND
*"----------------------------------------------------------------------



ENDFUNCTION.
