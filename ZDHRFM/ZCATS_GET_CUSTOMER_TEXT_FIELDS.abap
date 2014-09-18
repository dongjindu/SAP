FUNCTION ZCATS_GET_CUSTOMER_TEXT_FIELDS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(TCATS_IMP) LIKE  TCATS STRUCTURE  TCATS
*"             VALUE(CATSD_IMP) LIKE  CATSD_EXT STRUCTURE  CATSD_EXT
*"             VALUE(DISPTEXT1_IMP) LIKE  CATSFIELDS-DISPTEXT1
*"             VALUE(DISPTEXT2_IMP) LIKE  CATSFIELDS-DISPTEXT2
*"       EXPORTING
*"             VALUE(DISPTEXT1_EXP) LIKE  CATSFIELDS-DISPTEXT1
*"             VALUE(DISPTEXT2_EXP) LIKE  CATSFIELDS-DISPTEXT2
*"----------------------------------------------------------------------


  CALL CUSTOMER-FUNCTION '009'
       EXPORTING
            TCATS_IMP     = TCATS_IMP
            CATSD_IMP     = CATSD_IMP
            DISPTEXT1_IMP = DISPTEXT1_IMP
            DISPTEXT2_IMP = DISPTEXT2_IMP
       IMPORTING
            DISPTEXT1_EXP = DISPTEXT1_EXP
            DISPTEXT2_EXP = DISPTEXT2_EXP
       EXCEPTIONS
            OTHERS  = 1.



ENDFUNCTION.
