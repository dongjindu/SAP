************************************************************************
* Program Name      : Z_FSD_MOBIS_ORDER
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
FUNCTION Z_FSD_MOBIS_ORDER.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(RETURN) LIKE  SY-SUBRC
*"  TABLES
*"      IT_IN STRUCTURE  ZTSD_MOBIS_OR
*"----------------------------------------------------------------------

  LOOP AT IT_IN.
    MOVE-CORRESPONDING IT_IN TO ZTSD_MOBIS_OR.
    MODIFY ZTSD_MOBIS_OR.
  ENDLOOP.

  RETURN = SY-SUBRC.
ENDFUNCTION.
