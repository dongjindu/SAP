*----------------------------------------------------------------------*
*   INCLUDE ZXAUFU01                                                   *
*----------------------------------------------------------------------*
*"       IMPORTING
*"             VALUE(I_ACTVT) LIKE  TACT-ACTVT
*"             VALUE(I_AUFK) LIKE  AUFK STRUCTURE  AUFK
*"       EXCEPTIONS
*"              E_MESSAGE


tables: imzo,   "PI assign
        impr,   "PI master
        jest.   "PI status

data: l_posid like impr-posid,
      l_gjahr like impr-gjahr.

* investment order type
check i_aufk-kokrs = 'H201' and i_aufk-auart = 'P'.

**get order information (489466) for checking creation
data: l_coas  like coas.
data: l_raip1 like raip1.

*I_ACTVT : 32 - Save
if i_actvt = '32'.
  perform get_coas in program sapmkauf changing l_coas.

* LAIPPF00 FORM UPDATE_IMZO
  call function 'AIPP_PROGPOS_PBO'
    exporting
      i_objnr = l_coas-objnr
      i_aktyp = 'H'  "create
    importing
      e_prnam = l_raip1-prnam
      e_posid = l_raip1-posid
      e_gjahr = l_gjahr.

*Get Top PI
  concatenate l_raip1-posid(3) '    0000' into l_posid.

* select top PI master
  select single * from impr
     where posid = l_posid
       and gjahr = l_gjahr.

* if top PI is locked, then error ...
  select single * from jest
     where objnr = impr-objnr
       and stat  = 'I0043'
       and inact = space.
  if sy-subrc = 0.

    call function 'MESSAGE_STORE'
      exporting
        arbgb                   = 'AP'
        msgty                   = 'E'
        txtnr                   = '100'
        msgv1                   = l_raip1-prnam
        msgv2                   = 'Program Closed'
*       ZEILE                   = MESG-ZEILE
        exception_if_not_active = space.
  endif.

else.
* select PI assignment (first assignment)
** change by Furong on 10/18/2005
*  data: l_baprz like IMZO-BAPRZ.
  data: l_baprz like bseg-blnpz,
        l_capex like imtp-capex.

  select single sum( baprz )
    into l_baprz from imzo
   where objnr = i_aufk-objnr.

  select single capex into l_capex
    from imtp
   where prnam = l_raip1-prnam
     and gjahr = l_gjahr.

* check if percentage is not entered for budget category
  if not l_capex is initial.
    if l_baprz = 0.
      call function 'MESSAGE_STORE'
        exporting
          arbgb                   = 'AP'
          msgty                   = 'I'
          txtnr                   = '100'
          msgv1                   = 'Check Budget Category'
          exception_if_not_active = space.
    endif.
  endif.
*
** Get top-PI master
*  select single * from impr
*     where posnr = imzo-posnr
*       and gjahr = imzo-gjahr.
*
*  concatenate impr-posid(3) '    0000' into l_posid.
*  l_gjahr = imzo-gjahr.
endif.

* by Andy Choi
