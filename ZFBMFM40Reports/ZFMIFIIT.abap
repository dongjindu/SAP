REPORT ZFMIFIIT .

Tables: FMIFIIT, BKPF.
data: i_fm   like fmifiit occurs 0 with header line,
      i_fm_u like fmifiit occurs 0 with header line,
      wfm    like fmifiit.


* FMIO : find obj# fund/fundctr/commitment/amt_type
* FMIT need to be updated....

include IFIFMCON_CF.
DATA:
  g_t_dummy  LIKE fmifiit OCCURS 0 WITH HEADER LINE,
  l_t_fi     LIKE fmifiit OCCURS 0 WITH HEADER LINE,
*   Dummy
  l_t_fi_del LIKE fmifiit OCCURS 0 WITH HEADER LINE.
DATA:
  fmfi_con_btart_cfold               LIKE fmioi-btart  VALUE '0300'.



parameters: P_FIKRS like  FMIFIIT-FIKRS memory id FIK,
            P_GJAHR like  FMIFIIT-KNGJAHR.

select-options: S_BELNR  for FMIFIIT-KNBELNR.

parameters:
            p_paynr like FMIFIIT-VOBELNR,
            p_payyr like FMIFIIT-VOGJAHR.


parameters: p_mode(1) type c default 'D',
            p_run     as checkbox.
*arameters: p_othyr   as checkbox default 'X'.

case p_mode.
  when 'C'.
    perform create_pay_data.
  when 'D'.
    perform delete_pay_data.

endcase.
*&---------------------------------------------------------------------*
*&      Form  delete_pay_data
*&---------------------------------------------------------------------*
FORM delete_pay_data.
* select not payment cleared.

*  if p_othyr = 'X'.
*    select * from fmifiit
*       into table i_fm
*       where knbelnr in s_belnr
*         and kngjahr = p_gjahr
*         and VOGJAHR <> p_gjahr
*         and fikrs   = p_fikrs
*         and ( btart = '0200' or btart = '0250' ).
**        and PAYFLG  = 'X'.
*  else.
    select * from fmifiit
       into table i_fm
       where knbelnr in s_belnr
         and kngjahr = p_gjahr
         and fikrs   = p_fikrs
         and ( btart = '0200' or btart = '0250' ).
*        and PAYFLG  = 'X'.
* endif.
    select * from fmifiit
       appending table i_fm
       where knbelnr in s_belnr
         and kngjahr = p_gjahr
         and fikrs   = p_fikrs
         and wrttp   = '61'.

  loop at i_fm.
    i_fm-TRBTR = i_fm-TRBTR * -1.
    i_fm-FKBTR = i_fm-FKBTR * -1.
    append i_fm to l_t_fi.
  endloop.

* temp...
  write:/ '*** Delete list...'.
  loop at l_t_fi.
    wfm = l_t_fi.
    write:/ wfm-knbelnr, wfm-kngjahr, wfm-btart, wfm-ZHLDT, wfm-TRBTR.
  endloop.

  check p_run = 'X'.

* delete...
  LOOP AT l_t_fi.
    UPDATE FMIFIIT
    SET PAYFLG  = space WHERE
          knbelnr  = l_t_fi-knbelnr
      AND kngjahr  = l_t_fi-kngjahr
      AND bukrs    = l_t_fi-bukrs
      AND rldnr    = l_t_fi-rldnr.

    DELETE FROM fmifiit WHERE
          knbelnr  = l_t_fi-knbelnr
      AND kngjahr  = l_t_fi-kngjahr
      AND bukrs    = l_t_fi-bukrs
      AND fmbuzei  = l_t_fi-fmbuzei
      AND btart    = l_t_fi-btart
      AND rldnr    = l_t_fi-rldnr
      AND gjahr    = l_t_fi-gjahr.

*    IF sy-subrc <> 0.
*      ROLLBACK WORK.
*      MESSAGE e006(fi) WITH
*        'FMIFIIT'
*        l_t_fi-knbelnr
*        l_t_fi-kngjahr
*        l_t_fi-bukrs.
*    ENDIF.
  ENDLOOP.

  l_t_fi_del[] = l_t_fi[].
  refresh l_t_fi.

  CALL FUNCTION 'FM_TOTALS_UPDATE_FI'
       TABLES
            t_fmifiit     = g_t_dummy
            t_fmifiit_del = l_t_fi_del.
  COMMIT WORK.

ENDFORM.                    " delete_pay_data

*------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  create_pay_data
*&---------------------------------------------------------------------*
FORM create_pay_data.
  data: l_date like bkpf-budat.

* select not payment cleared.
  refresh i_fm.
  select * from fmifiit
     into table i_fm
     where knbelnr in s_belnr
       and kngjahr = p_gjahr
       and fikrs   = p_fikrs
       and BTART   = '0100'
       and wrttp   = '54'
       and PAYFLG  = space.

  loop at i_fm.
    i_fm-VOBELNR  = i_fm-knbelnr.
    i_fm-VOGJAHR  = i_fm-gjahr.
    i_fm-payflg   = 'X'.
    append i_fm  to i_fm_u.

    l_date = i_fm-zhldt.
    if p_paynr <> space.
      select single * from bkpf
         where bukrs = p_fikrs
           and belnr = p_paynr
           and gjahr = p_payyr.
      if sy-subrc = 0.
        l_date = bkpf-BUDAT.
        i_fm-VOBELNR  = p_paynr.
        i_fm-VOGJAHR  = p_payyr.
      endif.
    endif.


* plus commitment...
    i_fm-zhldt = l_date.
    i_fm-STUNR = i_fm-STUNR + 1.
    i_fm-wrttp = '54'.
    i_fm-btart = '0200'.
    i_fm-TRBTR = i_fm-TRBTR * -1.
    i_fm-FKBTR = i_fm-FKBTR * -1.
    append i_fm  to l_t_fi.

* minus payment...
    i_fm-STUNR = i_fm-STUNR + 1.
    i_fm-btart = '0250'.
    i_fm-TRBTR = i_fm-TRBTR * -1.
    i_fm-FKBTR = i_fm-FKBTR * -1.
    i_fm-wrttp = '57'.
    append i_fm  to l_t_fi.
  endloop.


* update...same doc #
  write:/ '*** Update list...'.
  loop at i_fm_u.
    if p_run = 'X'.
      UPDATE fmifiit
      SET PAYFLG  = 'Y'
          VOBELNR = i_fm_u-VOBELNR
          VOGJAHR = i_fm_u-VOGJAHR
      where FMBELNR = i_fm_u-fmbelnr
        and FIKRS   = i_fm_u-FIKRS
        and FMBUZEI = i_fm_u-FMBUZEI
*       and BTART   = i_fm_u-BTART
        and RLDNR   = i_fm_u-RLDNR
        and GJAHR   = i_fm_u-GJAHR
        and STUNR   = i_fm_u-STUNR.
    endif.

    wfm = i_fm_u.
    write:/ wfm-STUNR, wfm-ZHLDT, wfm-TRBTR, wfm-FKBTR,  wfm-WRTTP.
  endloop.

* append...
  write:/ '*** Insert list...'.
  loop at l_t_fi.
    if p_run = 'X'.
      insert  fmifiit from l_t_fi.
      IF sy-subrc <> 0.
        ROLLBACK WORK.
        MESSAGE e006(fi) WITH
          'FMIFIIT'
          l_t_fi-knbelnr
          l_t_fi-kngjahr
          l_t_fi-bukrs.
      ENDIF.
    endif.

    wfm = l_t_fi.
    write:/ wfm-STUNR, wfm-ZHLDT, wfm-TRBTR, wfm-FKBTR,  wfm-WRTTP.
  endloop.

if p_run = 'X'.
    CALL FUNCTION 'FM_TOTALS_UPDATE_FI'
         TABLES
              t_fmifiit     = l_t_fi
              t_fmifiit_del = l_t_fi_del.
    COMMIT WORK.
  endif.

ENDFORM.                    " create_pay_data
