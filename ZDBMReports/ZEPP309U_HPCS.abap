************************************************************************
* Program Name      : ZEPP309U_HPCS
* Author            : Bongsoo, Kim
* Creation Date     : 2003.11.06.
* Specifications By : Bongsoo, Kim
* Development Request No : UD1K903741
* Addl Documentation:
* Description       : EPP309 HPCS update to Work order
*
* Modification Logs
* Date       Developer    RequestNo    Description
*2004/02/20  ZDBM         UD1K907667   HPCS update to Work order.
*2004/12/01  Shiva        UD1K913281   Changed field ZWORK to 18 char.
*                                      Added the logic to pass
*                                      characteristic values for the
*                                      characteristic 'P_WO_HPC_B'.
*03/07/2005 Chris        UD1K914815    Characterics 'P_WO_HPC_Qxx'
*                                      should be upload to WOCL type
*                                      materials
************************************************************************
REPORT ZEPP309U_HPCS
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* INCLUDE
*----------------------------------------------------------------------*
INCLUDE ZEPP309L_HPCS_T.
INCLUDE ZEPP309L_HPCS_F01.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
*INITIALIZATION.
*  PERFORM INITIALIZATION.
*
AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM AT_SEL_SCREEN_ON_VALUE_REQUEST USING P_FILE 'O'.
*AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION_SCREEN.
*
START-OF-SELECTION.
  IF P_RDO1 EQ 'X'.
*************TEST 2003.11.06*********************
*   Classification MM02 BDC PROCESS
    PERFORM READ_PROCESS.
*************TEST 2003.11.06*********************
    PERFORM DATA_PROCESS.

    PERFORM DATA_DIVIDING.

    PERFORM TABLE_ABXHPCDT_INSERT.
*    PERFORM WRITE_PROCESS.
  ELSEIF P_RDO2 EQ 'X'.
***   ZTBM_ABXHPCDT & ZTBM_ABXHPCDT01 TABLE INSERT
**    PERFORM UPLOAD_PROCESS.
**    PERFORM DATA_DIVIDING.
**    PERFORM TABLE_ABXHPCDT_INSERT.
  ENDIF.

END-OF-SELECTION.
