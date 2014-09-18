*&---------------------------------------------------------------------*
*& Report  YTEMP_CHECK_ZTSD_UM
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  YTEMP_CHECK_ZTSD_UM.

data: begin of it_err_list OCCURS 0,
        model_code like ztpp_vm-model_code,
        body_no    like ztpp_vm-body_no,
      end   of it_err_list.

select a~model_code a~body_no
  into CORRESPONDING FIELDS OF TABLE it_err_list
  from ztpp_vm as a inner join ztsd_um as b
                       on b~model_code = a~model_code
                      and b~body_no    = a~body_no
 where a~RP_CSTATUS between '00' and '23'
   and a~usg_car    = 'P'
   and b~status     in ('','F')
   and b~wo_serial  ne a~wo_serial.

loop at it_err_list.
  write:/ it_err_list-model_code,
          it_err_list-body_no.
endloop.
