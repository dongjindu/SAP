*----------------------------------------------------------------------*
*   INCLUDE ZXMG0U02                                                   *
*----------------------------------------------------------------------*
*"  IMPORTING
*"     VALUE(WMARA) LIKE  MARA STRUCTURE  MARA
*"     VALUE(WMARC) LIKE  MARC STRUCTURE  MARC
*"     VALUE(WMARD) LIKE  MARD STRUCTURE  MARD
*"     VALUE(WMBEW) LIKE  MBEW STRUCTURE  MBEW
*"     VALUE(WMLGN) LIKE  MLGN STRUCTURE  MLGN
*"     VALUE(WMLGT) LIKE  MLGT STRUCTURE  MLGT
*"     VALUE(WMVKE) LIKE  MVKE STRUCTURE  MVKE
*"     VALUE(WSTAT) LIKE  MGSTAT STRUCTURE  MGSTAT
*"     VALUE(WMFHM) LIKE  MFHM STRUCTURE  MFHM
*"     VALUE(WMPOP) LIKE  MPOP STRUCTURE  MPOP
*"  TABLES
*"      STEXT STRUCTURE  SHORT_DESC
*"      SSTEUERTAB STRUCTURE  MG03STEUER
*"      SSTEUMMTAB STRUCTURE  MG03STEUMM
*"      WMEINH STRUCTURE  SMEINH
*"      SMEAN_ME_TAB STRUCTURE  MEAN
*"  CHANGING
*"     VALUE(CMARA) LIKE  MARU STRUCTURE  MARU
*"  EXCEPTIONS
*"      APPLICATION_ERROR

*&--------------------------------------------------------------------&*
*&  Date        User     Transport          Description
*&  08/29/2004  Shiva    UD1K912072         Change in validation
*&                       UD1K912076         for Valuation class.
*&--------------------------------------------------------------------&*
* check JIS valuation class with b/f cycle

*If valuation class is blank, then use DB data.
*&-----Shiva commented. Reason: to check valuation class wait till
*&----                  mbew entry is filled.
*if wmbew-bklas = space.
*   select single bklas into wmbew-bklas
*     from  mbew
*     where matnr = wmara-matnr
*       and bwkey = wmarc-WERKS.
*endif.

* UD1K941702
* Disabled by IG.MOON 9/27/2007
* {

**if not wmbew is initial.
**  if WMARA-TEMPB = '11'.
**    if wmbew-BKLAS <> '3005'.
**      message w008(ZFI) RAISING APPLICATION_ERROR.
**    endif.
**  else.
**    if wmbew-BKLAS = '3005'.
**      message w009(ZFI) RAISING APPLICATION_ERROR.
**    endif.
**  endif.
**endif.

* }
