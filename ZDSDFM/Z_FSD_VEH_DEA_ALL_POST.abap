************************************************************************
* Author                 : jun ho choi
* Creation Date          : 2003-11-11
* Specifications By      :
* Development Request No : UD1K901594
* Pattern                : 7-1
* Addl documentation     :
* Description            : Inbound interface from HMA/HAC
*                          (Dealer allocation)
*
*
* Modification Log
* Date       Developer    Request ID Description
*
************************************************************************
FUNCTION Z_FSD_VEH_DEA_ALL_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(RETURN) LIKE  SY-SUBRC
*"----------------------------------------------------------------------

  DATA: EVENTID LIKE TBTCJOB-EVENTID.

  EVENTID = 'ZISD02_01'.

  CALL FUNCTION 'BP_EVENT_RAISE'
    EXPORTING
      EVENTID                      = EVENTID
*     EVENTPARM                    = ' '
*     TARGET_INSTANCE              = ' '
   EXCEPTIONS
     BAD_EVENTID                  = 1
     EVENTID_DOES_NOT_EXIST       = 2
     EVENTID_MISSING              = 3
     RAISE_FAILED                 = 4
     OTHERS                       = 5.

  RETURN = SY-SUBRC.
ENDFUNCTION.
