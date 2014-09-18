************************************************************************
* Program Name      : ZAPP239M_MITU_STATUS
* Author            : Kim Gil Hyun (Tonkey)
* Creation Date     : 2003.10.14.
* Specifications By : B. Choi
* Development Request No : UD1K905658
* Type :
* Addl Documentation:
* Description       : MITU Status                            *
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

report  yapp239m_mitu_status   message-id  zmpp  .

include yapp239i_top.
*INCLUDE yapp239l_top.

include yapp239i_fob.
*INCLUDE yapp239l_fob.

include yapp239i_pai.
*INCLUDE yapp239l_pai.

include yapp239i_pbo.
*INCLUDE yapp239l_pbo.

****************************************
start-of-selection.
****************************************
  call screen 100.
