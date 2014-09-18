*----------------------------------------------------------------------*
*   INCLUDE ZXMBCU02                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_MSEG) LIKE  MSEG STRUCTURE  MSEG
*"             VALUE(I_VM07M) LIKE  VM07M STRUCTURE  VM07M
*"             VALUE(I_DM07M) LIKE  DM07M STRUCTURE  DM07M
*"             VALUE(I_MKPF) LIKE  MKPF STRUCTURE  MKPF
*"       EXPORTING
*"             VALUE(E_SGTXT) LIKE  MSEG-SGTXT

if i_mseg-sgtxt is initial.
  e_sgtxt = i_dm07m-maktx.
endif.

*CHECK GR CANCEL.
data: w_mkpf like mkpf,
      w_marv like marv,
      w_date like sy-datum.
data: w_poper like t009b-poper,
      w_bdatj like t009b-bdatj.

tables: ztfi_ctl.
*
data : w_lifex  type lifex.


*0 Standard
*1 Limit
*2 Consignment
*3 Subcontracting
*4 Material unknown
*5 Third-party
*6 Text
*7 Stock transfer
*8 Material group
*9 Service
*C Stock prov.by cust.
*P Return.trans.pack.

* dm07m-XSTBW = 'X'   "REVERSAL MVT
if  i_dm07m-xersy  = 'X'       "ERS
*AND i_dm07m-BSTYP = 'L'    "ONLY S/A
and i_mseg-kzbew   = 'B'       "Goods movement for purchase order
and ( i_mseg-bwart = '102' or i_mseg-bwart = '101' )
and i_mseg-lfbnr <> space.
*and i_vm07m-PSTYP = '0'.    "Exclude Service Entry(9)

  select single * from ztfi_ctl
    where bukrs = i_mseg-bukrs and categ = 'MIGO102' and flag  = 'X'.
  if sy-subrc = 0.
    select single * into w_marv from marv where bukrs = i_mseg-bukrs.

    w_bdatj = w_marv-lfgja.
    w_poper = w_marv-lfmon + ztfi_ctl-noday.
    if w_poper < 1.
      w_poper = 12.
      w_bdatj = w_bdatj - 1.
    endif.

    select single * into w_mkpf from mkpf
      where mblnr = i_mseg-lfbnr and mjahr = i_mseg-lfbja.
    if sy-subrc = 0.
      call function 'FIRST_DAY_IN_PERIOD_GET'
        exporting
          i_gjahr = w_bdatj
          i_poper = w_poper
          i_periv = 'K4'
        importing
          e_date  = w_date.

      if w_date > w_mkpf-budat.
        message e501(zfi).
      endif.
    endif.
  endif.
endif.

** Added by Park On 11/25/13
data ls_ekko     like ekko.
data ls_ekpo     like ekpo.

if i_mseg-bwart = '101'.
  select single *
    into ls_ekko
    from ekko
   where ebeln = i_mseg-ebeln.

  select single *
    into ls_ekpo
    from ekpo
   where ebeln = i_mseg-ebeln
     and ebelp = i_mseg-ebelp.

  if ls_ekko-bstyp = 'F'.
    if ( ls_ekko-bsart = 'ZB' or ls_ekko-bsart = 'FO' ) and
       ls_ekpo-xersy = ''.
      if i_mkpf-xblnr is initial.
        message e042(zmmm).
      endif.

    elseif ls_ekko-bsart = 'NB' and
           ls_ekpo-pstyp <> 9   and
           ls_ekpo-xersy = ''.
      if i_mkpf-xblnr is initial.
        message e042(zmmm).
      endif.
    endif.
  endif.
endif.
** End of Added 11/25/13
* 07/30/2014


*For CKD Engine materials from HMC (SBC3), the Inbound delivery will be created initially and the LIKP-LIFEX
*(External Delivery Number field is used for B/L number) will not be available and hence it will be updated as TBD.
* At the time of B/L creation, the interface RFC will update Inbound delivery if the LIKP-LIFEX is TBD for SBC3 Vendor.
* But this update will be successful if there is no GR for the Inbound Delivery. Hence the GR cannot be made
* if LIKP-LIFEX is TBD for LIFNR = SBC3.

if i_mseg-bwart = '101' and ls_ekko-lifnr = 'SBC3' and ls_ekko-bsart = 'KD'.
  clear w_lifex.
  select lifex into w_lifex from likp where vbeln = i_mkpf-xblnr.
    if  w_lifex = 'TBD'.
      message e997(zmmm) with 'No GR. TBD exists in LIFEX of ASN(CKD Engine)'.
    endif.
  endselect.
endif.
