************************************************************************
* Program Name      : ZAPP803M_ZTPPWOSUM_MNTNC
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.02.05.
* Specifications By : B. Choi
* Development Request No : UD1K906972
* Addl Documentation:
* Description       : Maintenance of The Table-ZTPP_WOSUM .
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************


include yapp803l_ztppwosum_mntnc_top.

include yapp803l_ztppwosum_mntnc_o01.
include yapp803l_ztppwosum_mntnc_i01.
include yapp803l_ztppwosum_mntnc_f01.

*-----------------------------------------------------------
start-of-selection.
*-----------------------------------------------------------
  call screen 100.
