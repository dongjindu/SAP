************************************************************************
* Program Name      : Z_FSD_MOBIS_ORDER_POST
* Author            : jun ho choi
* Creation Date     : 2003.08.06.
* Specifications By : jun ho choi
* Pattern           : 7-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Mobis will send Part Sales Orders on a
*                          monthly base to the HMMA via the Order File.
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
FUNCTION Z_FSD_MOBIS_ORDER_POST.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(RETURN) LIKE  SY-SUBRC
*"----------------------------------------------------------------------

  DATA: EVENTID LIKE TBTCJOB-EVENTID.

  EVENTID = 'ZISD04_01'.

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
