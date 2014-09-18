************************************************************************
* Program Name      : ZISD10U_MOBIS_INV
* Author            : jun ho choi
* Creation Date     : 2003.08.27.
* Specifications By : jun ho choi
* Pattern           : 5-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : INVOICE I/F to MOBIS.
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 02/23/2005 100471       UD1K914601   Add extension '.txt'.
* 03/03/2005 CHRIS        UD1K914751   CHANGE THE SUBROUTIN MODIFY_DATA.
*
************************************************************************
REPORT ZISD10U_MOBIS_INV MESSAGE-ID ZMSD.


*
INCLUDE ZISD10U_MOBIS_INV_TOP.
INCLUDE ZISD10U_MOBIS_INV_F01.
