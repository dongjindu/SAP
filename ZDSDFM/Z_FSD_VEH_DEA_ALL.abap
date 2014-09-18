************************************************************************
* Program Name      : Z_FSD_VEH_DEA_ALL
* Author            : jun ho choi
* Creation Date     : 2003.11.11.
* Specifications By : jun ho choi
* Pattern           : 7-1
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Inbound interface from HMA/HAC
*                          (Dealer allocation)
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
FUNCTION Z_FSD_VEH_DEA_ALL.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  EXPORTING
*"     VALUE(RETURN) LIKE  SY-SUBRC
*"  TABLES
*"      IT_IN2 STRUCTURE  ZTSD_VEH_DEA_ALL
*"----------------------------------------------------------------------

  LOOP AT IT_IN2.
    MOVE-CORRESPONDING IT_IN2 TO ZTSD_VEH_DEA_ALL.
    MODIFY ZTSD_VEH_DEA_ALL.
  ENDLOOP.

  RETURN = SY-SUBRC.
ENDFUNCTION.
