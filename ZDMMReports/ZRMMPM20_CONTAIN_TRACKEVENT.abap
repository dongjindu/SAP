************************************************************************
* Program Name : ZRMMPM20_CONTAIN_TRACK
* Created by   : Min-su Park
* Created on   : 2003.11.19.
* Pattern      :
* Description  : Container Tracking
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.19.     Min-su Park    UD1K901873     Initial Coding
***********************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZRMMPM20_CONTAIN_TRACKEVENT                                *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM GET_BASIC_DATA.

START-OF-SELECTION.
  PERFORM ALV_FIELD_BUILD.
