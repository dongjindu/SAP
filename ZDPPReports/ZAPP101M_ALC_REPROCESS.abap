************************************************************************
* Program Name      : ZAPP101M_ALC_REPROCESS
* Author            : Kim Gil Hyun (Tonkey)
* Creation Date     : 2003.10.23.
* Specifications By : B. Choi
* Development Request No : Local
* Type :
* Addl Documentation: reference to "ZAPP703C_WORKORDER_MAINT"
* Description       : WORK ORDER REPROCESS FOR ALC
*                                          From HMC's Sales order
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
program  yapp101m_alc_reprocess   message-id zmpp    .

include zapp101i_alc_rep_top.
*include yapp101l_alc_rep_top.
include zapp101i_alc_rep_pbo.
*include yapp101l_alc_rep_pbo.
include zapp101i_alc_rep_pai.
*include yapp101l_alc_rep_pai.
include zapp101i_alc_rep_fob.
*include yapp101l_alc_rep_fob.

******************************************************************
start-of-selection.
******************************************************************

call screen 100.
