*"----------------------------------------------------------------------
*FUNCTION EXIT_SAPLFMCU_002 .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_COEP) LIKE  COEP STRUCTURE  COEP
*"             VALUE(I_COBK) LIKE  COBK STRUCTURE  COBK
*"       CHANGING
*"             VALUE(C_FIPEX) LIKE  FMIA-RFIPEX
*"             VALUE(C_FISTL) LIKE  FMIA-RFISTL
*"             VALUE(C_FONDS) LIKE  FMIA-RFONDS
*----------------------------------------------------------------------*
*   INCLUDE ZXFMYU33                                                   *
*----------------------------------------------------------------------*
  data: l_scope like aufk-scope,
        l_xfeld like FMDY-XFELD.


* Only for CO lineitem Reposting
  check i_coep-VRGNG = 'RKU3'.


* statistical order go to cost center..so, no fund derived...
*sample: OR000000100223
  if i_coep-OBJNR_N1(2) = 'OR'.
    select single scope into l_scope from aufk
       where aufnr = i_coep-objnr_n1+2(12).

    if sy-subrc = 0 and l_scope = 'IV'.
* assume FM area = FI Company
      CALL FUNCTION 'FM_CO_ASSIGNMENT_READ_OBJECT'
           EXPORTING
                I_KOKRS            = I_COBK-KOKRS
                I_FIKRS            = I_COEP-BUKRS
                I_OBJNR            = i_coep-objnr_n1
                I_KSTAR            = I_COEP-KSTAR
                I_DATUM            = I_COBK-BUDAT
                I_GJAHR            = I_COBK-GJAHR
           IMPORTING
                E_FONDS            = c_fonds
                E_FICTR            = c_fistl
                E_fipex            = c_fipex
                E_FLG_FOUND_OBJECT = l_xfeld
           EXCEPTIONS
                OTHERS             = 1.
*--- if commitment item is blank, then use account
* (no 0 padding in commitment item)
      IF l_xfeld = 'X' and c_fipex IS INITIAL.
        select single fipos into c_fipex from skb1
          where bukrs = i_coep-bukrs
            and saknr = i_coep-kstar.
      endif.
    endif.
  endif.
