*----------------------------------------------------------------------*
*   INCLUDE ZXM06U36                                                   *
*----------------------------------------------------------------------*

*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_EKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_TRTYP)
*"             VALUE(I_CI_EKKO) LIKE  EKKO_CI STRUCTURE  EKKO_CI
*"             VALUE(I_BSTYP) LIKE  EKKO-BSTYP
*"             VALUE(I_NO_SCREEN)
*"             VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"             VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"             VALUE(I_KEKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_AEKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_REKKO) LIKE  EKKO STRUCTURE  EKKO
*"             VALUE(I_EKKO_OLD) LIKE  EKKO STRUCTURE  EKKO OPTIONAL
*"             VALUE(I_VORGA) LIKE  T160-VORGA
*"       TABLES
*"              TEKPO STRUCTURE  BEKPO OPTIONAL
*"              TEKET STRUCTURE  BEKET OPTIONAL
*"              TEKKN STRUCTURE  EKKNU OPTIONAL
*"              TKOMV STRUCTURE  KOMV OPTIONAL
*"----------------------------------------------------------------------

************************************************************************
* Enhancement
* Project Name      : Z_MM_PUR
* Enhancement Name  : MM06E005                                         *
* Function Name     : EXIT_SAPMM06E_006                                *
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

* Export date KDWEB po number

IF NOT i_ekko-zzkdwebpo  IS INITIAL.

  ekko_ci-zzkdwebpo = i_ekko-zzkdwebpo.

ENDIF.

************************************************************************
* Enhancement
* Project Name      : Z_MM_PUR
* Enhancement Name  : MM06E005                                         *
* Function Name     : EXIT_SAPMM06E_006                                *
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

* Export ETA

IF NOT i_ekko-zzeta IS INITIAL.
  ekko_ci-zzeta = i_ekko-zzeta.
ENDIF.
