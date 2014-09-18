************************************************************************
* Program Name      : ZRPM09_BACKGROUND_JOB
* Author            : Myoungho, Park
* Creation Date     : 2004.04.30.
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :   Call Background Job Programs
*
* ZRPM04_BDCH_B   : Update Monthly Average Breakdown Time by Shop
*
* ZRPM06_MTBT_B   : Update Monthly Actual No. of Breakdown by Shop
*
* ZRPM07_PMRO_B   : Update Monthly Actual  Maintenance Ratio by Shop
*
* ZRPM08_PMCO_B   : Update Monthly Actual Maintenance Cost by Shop
*
*                       Runs by background job by Monthly .
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT ZRPM09_BACKGROUND_JOB NO STANDARD PAGE HEADING
                             LINE-SIZE 132
                             LINE-COUNT 64.

INITIALIZATION.

START-OF-SELECTION.

END-OF-SELECTION.

  CALL TRANSACTION 'ZPMR04B' AND SKIP FIRST SCREEN.

  CALL TRANSACTION 'ZPMR06B' AND SKIP FIRST SCREEN .

  CALL TRANSACTION 'ZPMR07B' AND SKIP FIRST SCREEN .

  CALL TRANSACTION 'ZPMR08B' AND SKIP FIRST SCREEN .
