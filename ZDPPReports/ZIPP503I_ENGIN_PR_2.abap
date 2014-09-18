************************************************************************
* Program Name      : ZIPP503I_ENGIN_PR
* Author            : HyungYul, Kim
* Creation Date     : 2003.09.03.
* Specifications By : HyungYul, Kim
* Pattern           : 5.1.4.1
* Development Request No : UD1K902114
* Addl Documentation:
* Description       : Transfer of Engine Production Result
*                     From MES to SAP [BDC & UPDATE] for new interface 2
*                     copy from ZIPP503L_ENGIN_PR
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
************************************************************************
REPORT ZIPP503I_ENGIN_PR_2  NO STANDARD PAGE HEADING
                          LINE-SIZE 1023
                          MESSAGE-ID ZMPP.

************************************************************************
* INCLUDE
************************************************************************
INCLUDE ZIPP503L_ENGIN_PR_2_TOP.
*INCLUDE ZIPP503L_ENGIN_PR_TOP.
INCLUDE ZIPP503L_ENGIN_PR_2_F_backup.
*INCLUDE ZIPP503L_ENGIN_PR_F.

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
  PERFORM CHECK_BATCH_JOB.
  IF W_BATCH_JOB IS INITIAL.
    PERFORM COPY_TO_ZTPPERM.
    PERFORM EXECUTE_PROCESS.
  ENDIF.
************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
