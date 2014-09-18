*&---------------------------------------------------------------------*
*&  Include           ZXFMDTU01
*&---------------------------------------------------------------------*
*"  IMPORTING
*"     REFERENCE(I_ENVIRONMENT) LIKE  TABADRS-ABADRENV
*"     REFERENCE(I_DERIVATION_DATE) LIKE  SY-DATUM
*"     REFERENCE(I_STEP_ID) TYPE  TABADRS-STEPID
*"     REFERENCE(I_FMDERIVE_SOURCE) LIKE  FMDERIVE STRUCTURE  FMDERIVE
*"  EXPORTING
*"     REFERENCE(E_FMDERIVE_TARGET) LIKE  FMDERIVE STRUCTURE  FMDERIVE
*"     REFERENCE(E_EXIT_IS_ACTIVE)
*"     REFERENCE(E_FAILED)
*"  EXCEPTIONS
*"      DERIVATION_FAILED

data: l_scope like aufk-scope,
      l_objnr like fmii1-objnr,
      l_kostv like csks-kostl,
      l_autyp like aufk-autyp,
      l_auart like aufk-auart,
      l_posid like impr-posid.
tables: imzo.

*select order (investment only)
select single scope autyp auart objnr kostv
   into (l_scope, l_autyp, l_auart, l_objnr, l_kostv)
   from aufk
   where aufnr = i_fmderive_source-order_number
     and auart in ('H', 'P').
if sy-subrc = 0.
  e_exit_is_active = 'X'.

  select single impr~posid into l_posid
    from imzo
    inner join impr
       on imzo~posnr = impr~posnr
    where imzo~objnr = l_objnr.

  if sy-subrc <> 0.
    e_failed = 'X'.
    raise derivation_failed.
  else.
    e_fmderive_target-fund_typ    = l_posid(1).
    e_fmderive_target-fund        = l_posid+1(10).

*determine fund center
    data : fm_tab_name7 type abadrparam.
    data : l_target type fistl.

    select single param_1 from tabadrs into fm_tab_name7
      where applclass = 'FM'
      and subclass = '01'
      and abadrstratid = 'FMOA'
      and abadrenv = 'SAP'
      and step_no = '7'.
    select single target2 into l_target
      from (fm_tab_name7)
     where sour1_from = 'H201'
       and sour2_from = l_kostv
       and valid_from <= sy-datum.

    e_fmderive_target-fund_center = l_target.
  endif.

endif.
