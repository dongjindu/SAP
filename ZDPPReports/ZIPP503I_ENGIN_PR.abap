************************************************************************
* Program Name      : ZIPP503I_ENGIN_PR
* Author            : HyungYul, Kim
* Creation Date     : 2003.09.03.
* Specifications By : HyungYul, Kim
* Pattern           : 5.1.4.1
* Development Request No : UD1K902114
* Addl Documentation:
* Description       : Transfer of Engine Production Result
*                     From MES to SAP [BDC & UPDATE]
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT ZIPP503I_ENGIN_PR  NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID ZMPP.

************************************************************************
* INCLUDE
************************************************************************
INCLUDE ZIPP503L_ENGIN_PR_TOP.
INCLUDE ZIPP503L_ENGIN_PR_F.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.

************************************************************************
* TOP-OF-PAGE.
************************************************************************
TOP-OF-PAGE.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
 PERFORM AT_SELECTION_SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
