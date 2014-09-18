************************************************************************
* Program Name      : ZRMMGM01R_6005
* Author            : Hakchin Kim
* Creation Date     : 2003.09.01.
* Specifications By : Hakchin Kim
* Pattern           : Report 1-1
* Development Request No : RMMGM01
* Addl Documentation: F/S - RMMGM01 Steel Requirement Summary Report
* Description       : This program shows a summary report of an
*                     aggregated requirements quantity to provide
*                     information to both steel center and press vendors
*                     prior to issuing purchase order for steel material
.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 20031021   Hakkchin Kim              Resource gathering logic change
*
************************************************************************
REPORT  zrmmgm01r_6005.
************************************************************************
INCLUDE zrmmgm01r_6005top.       "Data Declaration
INCLUDE zrmmgm01r_6005cla.       "Class Part
INCLUDE zrmmgm01r_6005o01.       "PBO Part
INCLUDE zrmmgm01r_6005i01.       "PAI Part
INCLUDE zrmmgm01r_6005f01.       "Perform Library.
************************************************************************
START-OF-SELECTION.
  PERFORM get_data.            "get data

  IF IT_zsmm_6005_01 IS INITIAL.
    MESSAGE s999(zmmm) WITH 'There is no data!'(001).
  ELSE.
    CALL SCREEN 0100.                   " Go to Screen 0100
  ENDIF.
