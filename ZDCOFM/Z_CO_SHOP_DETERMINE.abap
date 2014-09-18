FUNCTION Z_CO_SHOP_DETERMINE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(F_TYPPS) TYPE  CKIS-TYPPS OPTIONAL
*"     REFERENCE(F_KOSTL) TYPE  CSKS-KOSTL OPTIONAL
*"     REFERENCE(F_PRCTR) TYPE  MARC-PRCTR OPTIONAL
*"     REFERENCE(F_FEVOR) TYPE  MARC-FEVOR OPTIONAL
*"     REFERENCE(F_WERKS) TYPE  MARC-WERKS OPTIONAL
*"     REFERENCE(F_RAUBE) TYPE  MARA-RAUBE OPTIONAL
*"     REFERENCE(F_DEF) TYPE  C DEFAULT SPACE
*"  EXPORTING
*"     REFERENCE(E_SHOP) TYPE  ZZSHOP
*"----------------------------------------------------------------------
*  Modification Log
*  Date        Developer Issue No    Description
*======================================================================
*  01/12/2012  Valerian  UD1K953679  HMMA Engine Plant split
*                                    implementation
*----------------------------------------------------------------------
  if f_werks = 'E001' OR f_werks = 'E002'.                  "UD1K953679
* if f_werks = 'E001'.                                      "UD1K953679
* { by ig.moon 11/18/2009 {
*    e_shop = 'MXEX'.
    e_shop = F_PRCTR.
* }
  else.
    if f_typps = 'E' .
      if f_kostl <> ''.
        e_shop = f_kostl(4).
      endif.

    elseif f_typps = 'V'.

    elseif f_typps = 'M'.  "Material
      if not f_prctr is initial.
        e_shop = f_prctr.

      else.
        case f_fevor.
          when 'SPB' or 'SPD' or 'SPP'.  "Blank/Panel
            e_shop = 'MXSX'.
          when 'SEA' or 'SEC'.           "3C
* { by ig.moon 11/18/2009 {
            e_shop = F_PRCTR.
*            e_shop = 'MXEX'.
* }
          when others.
*            case f_raube.
*              when 10.
*                e_shop = 'MXSX'.
*              when 11.
*                e_shop = 'MXBX'.
*              when 12.
*                e_shop = 'MXPX'.
*              when 13.
*                e_shop = 'MXTX'.
*              when 14.
*                e_shop = 'MXEX'.
*              when others.
*                e_shop = space.
*            endcase.
        endcase.
      endif.

    endif. "Material
  endif.


* use default if not derived ???
  if f_def = 'X' and e_shop is initial.
    e_shop = 'MXTX'.
  endif.
ENDFUNCTION.
