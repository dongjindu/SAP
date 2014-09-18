************************************************************************
* Program Name      : ZCPP104C_CLASSIFI
* Author            :
* Creation Date     : 2004.01.07.
* Specifications By : HyungYul, Kim
* Pattern           : 1.1
* Development Request No : UD1K902186
* Addl Documentation:
* Description       : Classification Material Master Upload
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZCPP104C_CLASSIFI NO STANDARD PAGE HEADING
                         MESSAGE-ID ZMPP
                         LINE-SIZE 100
                         LINE-COUNT 70.

************************************************************************
* INCLUDE                                                              *
************************************************************************
INCLUDE ZCPP104C_CLASSIFI_TOP.
INCLUDE ZCPP104C_CLASSIFI_F.

************************************************************************
* INITIALIZATION                                                       *
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN                                                  *
************************************************************************
AT SELECTION-SCREEN.
  PERFORM VALUE_REQUEST_MARA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM VALUE_REQUEST_FILE.

************************************************************************
* START-OF-SELECTION.                                                  *
************************************************************************
START-OF-SELECTION.
  SET PF-STATUS 'PF_1000'.
  PERFORM DATA_PROCESS.

************************************************************************
* END-OF-SELECTION                                                     *
************************************************************************
END-OF-SELECTION.
  PERFORM WRITE_PROCESS.

************************************************************************
* AT USER-COMMAND                                                     *
************************************************************************
AT USER-COMMAND.
  PERFORM AT_USER_COMMAND.
