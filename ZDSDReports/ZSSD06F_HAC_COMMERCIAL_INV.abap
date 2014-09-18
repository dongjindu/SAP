************************************************************************
* Program Name      : ZSSD06F_HAC_COMMERCIAL_INV
* Author            : jun ho choi
* Creation Date     : 2004.02.18.
* Specifications By : jun ho choi
* Pattern           : 5-2
* Development Request No : UD1K906130
* Addl Documentation:
* Description       : HAC COMMERCIAL INVOICE
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT zssd06f_hac_commercial_inv.

INITIALIZATION.
  MESSAGE i000(zmpp) WITH 'DO NOT USE THIS PROGRAM'.
  LEAVE PROGRAM.

  INCLUDE zssd06u_hac_commercial_inv_top.
  INCLUDE zssd06u_hac_commercial_inv_f01.
