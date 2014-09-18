function z_hr_ess_get_emp_ani_day.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(EMPLOYEE_NUMBER) LIKE  BAPI7004-PERNR
*"  TABLES
*"      ZESS_EMP_ANI_DAY STRUCTURE  ZESS_EMP_ANI_DAY
*"      RETURN STRUCTURE  BAPIRETURN
*"----------------------------------------------------------------------

  data : darxx type datar,
         datxx type dardt.

  select single gbdat into zess_emp_ani_day-zdate
  from pa0002 where pernr eq employee_number
                and endda eq '99991231'.
  if sy-subrc eq 0.
    zess_emp_ani_day-value_txt = 'BIRTHDAY'.
    zess_emp_ani_day-zdate_to = zess_emp_ani_day-zdate.
    append zess_emp_ani_day.

    select single * from pa0041
      where pernr eq employee_number
             and endda eq '99991231'.


    do 12 times varying darxx from pa0041-dar01
                              next pa0041-dar02
                varying datxx from pa0041-dat01
                              next pa0041-dat02.
      if darxx eq 'Z1'.
        zess_emp_ani_day-value_txt = 'HIREDATE'.
        zess_emp_ani_day-zdate = datxx.
        zess_emp_ani_day-zdate_to = zess_emp_ani_day-zdate.
        append zess_emp_ani_day.
      endif.

    enddo.


  endif.

* by ig.moon 4/7/2010 {
  select single termn mndat into (zess_emp_ani_day-zdate,
 zess_emp_ani_day-zdate_to)
          from pa0019 where pernr eq employee_number
                      and tmart eq 'ZT'
                      and bvmrk eq ' '
                      and termn <= sy-datum.
  if sy-subrc eq 0.
    if zess_emp_ani_day-zdate_to < sy-datum.
    else.
      zess_emp_ani_day-value_txt = 'TEAMWEAR'.
      append zess_emp_ani_day.
    endif.
  endif.
* }

  read table zess_emp_ani_day index 1.

  if sy-subrc eq 0.

    return-type = 'S'.
    return-message = 'Success!'.
    append return.

  endif.

endfunction.
