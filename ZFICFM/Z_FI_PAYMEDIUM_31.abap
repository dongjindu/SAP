function z_fi_paymedium_31.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FPAYH) LIKE  FPAYH STRUCTURE  FPAYH
*"     VALUE(I_FPAYHX) LIKE  FPAYHX STRUCTURE  FPAYHX
*"  TABLES
*"      T_FPAYP STRUCTURE  FPAYP
*"      T_PAYMENT_DETAILS STRUCTURE  FPM_PAYD
*"      T_FILE_OUTPUT STRUCTURE  FPM_FILE
*"  EXCEPTIONS
*"      NO_PAYMENT
*"----------------------------------------------------------------------
************************************************************************
* Modification Logs
************************************************************************
* Date        Developer    RequestNo    Description
* 05/10/10    Valerian     UD1K948981   Generate IAT format if payment
*             HIS20094                  Method is 'F'. Otherwise gene-
*                                       rate ACH Format.
* 12/10/10    Valerian     UD1K950765   Get Bank Key and Bank Account
*                                       from HR Table PA0009.
* 07/12/11    Valerian     UD1K952511   Fix the logic to get latest
*                                       record from PA0009.
* 02/22/12    Valerian     UD1K954025   Extend Employee Bank Account
*                                       from 14 to 17 characters
************************************************************************

* by Andy.
  tables: pa0002, pa0001.
  tables: lfa1, adrc.
  data: l_perid like pa0002-perid,
        l_pernr like pa0002-pernr,
        l_ename like pa0001-ename,
        l_upd(1) type c,
        l_bankl type pa0009-bankl,                          "UD1K950765
        l_bankn type pa0009-bankn,                          "UD1K950765
        l_ennda type pa0009-endda,                          "UD1K952511
        l_anzhl type vgpro.

  loop at t_file_output.
* BEGIN OF HIS20094 - Modify for IAT format (Payment Method = 'F')
    if t_file_output(1) = '5' and i_fpayh-rzawe = 'F'.
      t_file_output+4(16) = space.
      t_file_output+20(2) = 'FF'.
      t_file_output+22(16) = space.
      t_file_output+38(2) = 'US'.
      t_file_output+50(3) = 'IAT'.
      t_file_output+63(3) = 'USD'.
      t_file_output+66(3) = 'USD'.
      t_file_output+75(3) = space.
      t_file_output+78(1) = '1'.
      modify t_file_output.
    endif.
* END OF HIS20094

    check t_file_output(1) = '6'.

    clear: l_pernr, l_perid, l_ename, l_upd,
           l_bankl, l_bankn.                                "UD1K950765

    select single stcd1 into l_perid  from lfa1
       where lifnr = i_fpayh-gpa1r.

    if sy-subrc = 0.
      t_file_output+39(15) = l_perid.
      l_upd = 'X'.
    else.
* check HR master
*     l_pernr = T_FILE_OUTPUT+39(8).
*    l_pernr = trim( l_pernr ).
      select single perid into l_perid
         from pa0002
         where pernr = i_fpayh-gpa1r
           and endda >= sy-datum.
      if sy-subrc = 0.
*        select single ename into l_ename
*          from pa0001
*          where pernr = l_pernr.
*        T_FILE_OUTPUT+54(22) = l_ename.
        data : con_field(11).
        clear con_field.
        search '-' for l_perid.
        if sy-subrc <> 0.
          move : l_perid(3)   to con_field(3),
                 '-'          to con_field+3(1),
                 l_perid+3(2) to con_field+4(2),
                 '-'          to con_field+6(1),
                 l_perid+5(4) to con_field+7(4).
        else.
          con_field = l_perid.
        endif.
        t_file_output+39(15) = con_field.
        l_upd = 'X'.
      endif.
    endif.

* BEGIN OF UD1K950765 - Get Bank information from HR
* BEGIN OF UD1K952511
    select max( endda )
      into l_ennda
      from pa0009
     where pernr = i_fpayh-gpa1r
       and subty = '0'
       and sprps = space.
* END OF UD1K952511
    if sy-subrc = 0.                                        "UD1K952511
      select bankl bankn into (l_bankl, l_bankn)
        from pa0009
       up to 1 rows
       where pernr = i_fpayh-gpa1r
         and subty = '0'
         and sprps = space
         and endda = l_ennda.                               "UD1K952511
*        AND endda = '99991231'.                            "UD1K952511
      endselect.
    endif.                                                  "UD1K952511

    if sy-subrc = 0.
      t_file_output+3(9)   = l_bankl.
      t_file_output+12(17) = l_bankn.                       "UD1K954025
*     t_file_output+12(14) = l_bankn.                       "UD1K954025
      l_upd = 'X'.
    endif.

*** 11/07/2013 T00306 - Start
    select anzhl bankl bankn
        into (l_anzhl, l_bankl, l_bankn)
        from pa0009
       up to 1 rows
       where pernr = i_fpayh-gpa1r
         and subty in ('1','9')
         and sprps = space
         and endda = l_ennda.
    endselect.

    if l_anzhl eq 100.
      t_file_output+3(9)   = l_bankl.
      t_file_output+12(17) = l_bankn.                       "UD1K954025
      l_upd = 'X'.
    endif.
*** 11/07/2013 T00306 - End

* END OF UD1K950765

* fill if blank..
    if t_file_output+54(22) = space.
      t_file_output+54(22) = i_fpayh-znme1.
      l_upd = 'X'.
    endif.
    if t_file_output+39(15) = space.
      t_file_output+39(15) = i_fpayh-gpa1r.
    endif.

* BEGIN OF HIS20094 - Modify for IAT format (Payment Method = 'F')
    if i_fpayh-rzawe = 'F'.
      t_file_output+39(35) = t_file_output+12(17).
      t_file_output+12(4) = '0000'.
      t_file_output+16(13) = space.
      t_file_output+74(2) = space.
      t_file_output+76(2) = space.
      t_file_output+78(1) = '1'.
      l_upd = 'X'.
    endif.
* END OF HIS20094

    if l_upd = 'X'.
      modify t_file_output.
    endif.
  endloop.

endfunction.
