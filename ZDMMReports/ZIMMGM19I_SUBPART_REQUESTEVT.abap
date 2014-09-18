****************************************************************
* Program Name  : ZIMMGM19I_SUBPART_REQUEST
* Created by    : Min-su Park
* Created on    : 2003.11.06.
* Pattern       : Report 1-1
* Description   : SUB PART REQUEST(Outbound)
*                 This program is related to
*                 EMMGM01 BOM Registration Request Program.
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.06.     Min-su Park    UD1K901873     Initial Coding
***************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM19I_SUBPART_REQUESTEVT                               *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM read_data.
  PERFORM execute_rfc_with_eai.

END-OF-SELECTION.
  PERFORM display_data.
  PERFORM create_interface_log.
