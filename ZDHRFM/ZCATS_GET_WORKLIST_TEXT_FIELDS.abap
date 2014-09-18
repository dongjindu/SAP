FUNCTION ZCATS_GET_WORKLIST_TEXT_FIELDS.
*"----------------------------------------------------------------------
*"*"Local interface:
*"       IMPORTING
*"             VALUE(TCATS_IMP) LIKE  TCATS STRUCTURE  TCATS
*"             VALUE(CATSW_IMP) LIKE  CATSW STRUCTURE  CATSW
*"             VALUE(DISPTEXTW1_IMP) LIKE  CATSFIELDS-DISPTEXTW1
*"             VALUE(DISPTEXTW2_IMP) LIKE  CATSFIELDS-DISPTEXTW2
*"       EXPORTING
*"             VALUE(DISPTEXTW1_EXP) LIKE  CATSFIELDS-DISPTEXTW1
*"             VALUE(DISPTEXTW2_EXP) LIKE  CATSFIELDS-DISPTEXTW2
*"----------------------------------------------------------------------


  CALL CUSTOMER-FUNCTION '010'
       EXPORTING
            TCATS_IMP     = TCATS_IMP
            CATSW_IMP     = CATSW_IMP
            DISPTEXTW1_IMP = DISPTEXTW1_IMP
            DISPTEXTW2_IMP = DISPTEXTW2_IMP
       IMPORTING
            DISPTEXTW1_EXP = DISPTEXTW1_EXP
            DISPTEXTW2_EXP = DISPTEXTW2_EXP
       EXCEPTIONS
            OTHERS  = 1.



ENDFUNCTION.
