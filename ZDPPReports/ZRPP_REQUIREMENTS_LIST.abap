***********************************************************************
* Program Name      : ZRPP_REQUIREMENTS_LIST
* Author            : YONGPING LI
* Creation Date     : 2004.10.18.
* Specifications By : CATHERINE SJOLANDER
* Development Request No :UD1K912532
* Addl Documentation:
* Description       : MATERIAL REQUIREMENTS AND AVAILABILITY LIST
*
* Modification Logs
* Date       Developer    RequestNo    Description
*10/28/2004  chris        UD1K912703
*11/04/2004  chris        UD1K912855   ADD 21 Days and 21 Weeeks
*                                      Reqirements Report
*02/08/2005 CHris         UD1K914250   THE MRP HORIZON FIX TO 21 DAYS
*                                      THE LTP HOTIZON BEGIN FROM TODAY
*                                      LTP DISPLAY WEEKS MOVEABLE FROM
*                                      1ST WEEK TO 24 WEEKS WHICH IS DE-
*                                      PEND ON THE LTP ACTUAL DATA, THE
*                                      TOTAL DISPLAY WEEKS IS AT LEAST
*                                      21 WEEKS. THE SAFTY STOCK IS CON-
*                                      SIDERED AS REQUIREMENT, BUT LIST-
*                                      ED IN DETAIL SCREEN AS SEPERATE
*                                      LINE.
*06/29/2005 Chris                      Excluding the stock and safty
*                                      qty for current day's reqirement
*
***********************************************************************

REPORT ZRPP_REQUIREMENTS_LIST MESSAGE-ID ZMPP NO STANDARD PAGE HEADING.

INCLUDE ZRPP_REQUIRMENTS_TOP.

*********************************************************************
**  EVENTS
*********************************************************************
INITIALIZATION.

*----------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM MODIFY_SCREEN.
*----------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LPKD.
  PERFORM MAKE_TYPE_LIST.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VEND.
  PERFORM MAKE_VENDOR_LIST.
*----------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM CHECK_INPUT.

*-----------------------------------------------------*

START-OF-SELECTION.
  PERFORM INIT_VARIABLES.
  PERFORM READ_DATA.
  PERFORM GET_QUANTITY.
  PERFORM PROCESS_DATA.

*-----------------------------------------------------*
END-OF-SELECTION.
*-->OUTPUT RESULT.
  G_REPID = SY-REPID.
  PERFORM  BUILD_EVENTS.
  PERFORM  BUILD_FIELDCAT.
  PERFORM  BUILD_LAYOUT      USING  'X'   'X'   SPACE.
  PERFORM  BUILD_COMMENT     USING  GT_HEADER[].
  PERFORM  BUILD_CELL_COLOR.
  PERFORM START_GRID_VIEWER.


*----------------------------------------------------*
*  FORMS
*----------------------------------------------------*
INCLUDE ZRPP_REQUIRMENTS_F01.    "APPLICATION FORMS
INCLUDE ZRPP_REQUIRMENTS_F02.    "ALV FORMS
