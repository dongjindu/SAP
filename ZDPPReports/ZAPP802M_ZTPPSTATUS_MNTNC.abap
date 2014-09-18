************************************************************************
* Program Name      : ZAPP802M_ZTPPSTATUS_MNTNC
* Author            : KIM GIL-HYUN (Tonkey)
* Creation Date     : 2004.02.04.
* Specifications By : B. Choi
* Development Request No : UD1K906913
* Addl Documentation:
* Description       : Maintenance of The Table-ZTPP_STATUS .
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


include yapp802l_ztppstatus_mntnc_top           .                      "

 include yapp802l_ztppstatus_mntnc_o01           .
 include yapp802l_ztppstatus_mntnc_i01           .
 include yapp802l_ztppstatus_mntnc_f01           .

*-----------------------------------------------------------
start-of-selection.
*-----------------------------------------------------------
  call screen 100.
