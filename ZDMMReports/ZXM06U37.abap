*----------------------------------------------------------------------*
*   INCLUDE ZXM06U37                                                   *
*----------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(I_UCOMM) TYPE  SY-UCOMM
*"  EXPORTING
*"     VALUE(E_CI_UPDATE) LIKE  SY-CALLD
*"     VALUE(E_UCOMM) LIKE  SY-UCOMM
*"  CHANGING
*"     VALUE(E_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI OPTIONAL
*"----------------------------------------------------------------------

************************************************************************
* Enhancement
* Project Name      : Z_MM_PUR
* Enhancement Name  : MM06E005                                         *
* Function Name     : EXIT_SAPMM06E_008                                *
* Author            : Jaesung-LEE                                      *
* Creation Date     : 2003.10.02.                                      *
* Specifications By : Jaesung-LEE                                      *
* Development Request No :                                             *
* Addl Documentation:                                                  *
* Description       : EMMPM31 Field Exit for PO Enhancement            *
* Export Data to Customer Subscreen for Purchasing Document Header (PBO)
*                                                                      *
* Modification Logs                                                    *
* Date            Developer          RequestNo    Description          *
* 2003.10.01.     Jaesung-LEE                     Initial Coding       *
*                                                                      *
************************************************************************

* save user-command : MESAVE.
*CHECK SY-UNAME = 'JSLEE72'.
*break-point.
*CHECK I_ucomm = 'MESAVE'.  " WHEN SAVE DOCUMENT

e_ucomm = i_ucomm.

e_ci_update = 'X'.   " SAVE PARAMETER

*e_ci_ekko-zzkdwebpo = 'XXXX'.

e_ci_ekko-zzkdwebpo = space.




************************************************************************
* Enhancement
* Project Name      : Z_MM_PUR
* Enhancement Name  : MM06E005                                         *
* Function Name     : EXIT_SAPMM06E_008                                *
* Author            : Sung-Tae Lim                                     *
* Creation Date     : 2003.11.04.                                      *
* Specifications By : Sung-Tae Lim                                     *
* Development Request No :                                             *
* Addl Documentation:                                                  *
* Description       : EMMPM37 Creating ETA Request                     *
* Export Data to Customer Subscreen for Purchasing Document Header (PBO)
*                                                                      *
* Modification Logs                                                    *
* Date            Developer          RequestNo    Description          *
* 2003.11.04.     Sung-Tae Lim                    Initial Coding       *
*                                                                      *
************************************************************************

e_ci_ekko-zzeta = '00000000'.
