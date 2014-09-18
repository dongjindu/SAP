REPORT ZHR_BT1_11 .

tables: pcl1
       ,pcl2
       ,pa0001
       ,pa0002
       ,pa0003
       .
data:   v_pernr       like pa0003-pernr
       ,v_seqnr(5)    type n
       ,v_count_1     type i value 0
       ,v_count_2     type i value 0
       ,v_ssn         like pa0002-pernr
       .

data: num-rec-read       type i.
data: num-rec-writ       type i.

*data: begin of itab occurs 0,
*        pernr  like pa0001-pernr,
*      end of itab.
*

data: begin of itab occurs 0,
        pernr  like pa0001-pernr,
      end of itab.

data:begin of itab2 occurs 0,
        name like pa0001-ename,
        ssn(13)  type c,
        pernr like pa0001-pernr,
        BT1Y_amt(8) type p decimals 2,
        BT1Q_amt(8) type p decimals 2,
        9011_amt(8)  type p decimals 2,
        end of itab2.

data:begin of state_tab occurs 0,
        state(2)          type c,
        taxau(4)          type c,
        pernr  like pa0003-pernr,
      end of state_tab.

data:begin of rpt_tab occurs 0,
        taxau(4)          type c,
        count             type i,
      end of rpt_tab.

clear num-rec-read.
clear num-rec-writ.

** Furong on 08/31/12 for deactivate the unused program
Initialization.
Leave program.
** End on 08/31/12

*main form
*form dyn1.
*break 103088.
*write: / '    SSN  ', '       Employee#  ', '      /BT1Y ',
write: /  'Employee#  ', '        9011 ',
 '            /BT1Y ', '           /BT1Q '.


 select pernr
   into table itab
   from pa0001
   where    begda > '20040101'.
*where pernr in ('100295','100630','100612','100121','100458','100336',
*'100627','102613','101640') .


 sort itab by pernr.
 delete adjacent duplicates from itab comparing pernr.

 loop at itab.
   num-rec-read = num-rec-read + 1.
   v_pernr = itab-pernr.
   perform process_emp.
 endloop.

* uline.
* write: / 'Number of employee record read:' , num-rec-read.



 sort itab2 by ssn.

 loop at itab2.
 num-rec-writ =  num-rec-writ + 1.
*write: / itab2-ssn, ';',
write: / itab2-pernr, itab2-9011_amt,
itab2-bt1y_amt, itab2-bt1q_amt.


  endloop.
uline.
 Write: / 'Number of employee record written:' , num-rec-writ.
 write: / 'Number of employee record read:' , num-rec-read.


*endform.
* Form Process_emp
*****
* Cluster CU)
* Cluster CU)
include: rpc2cd09.

*cluster RU
include: rpc2ruu0,
         rpc2rx09.

include: rpppxd00,
         rpppxd10,
         rpppxm00.

form process_emp.

*Get Cluster

 rp-init-buffer.
 cd-key-pernr = v_pernr.
 rp-imp-c2-cu.

if rp-imp-cd-subrc <> 0.

 exit.
 endif.

*Get the latest cluster of 2006 for EE.

 Call function 'CD_READ_LAST'
 exporting
 begin_date  = '20061101'
 end_date    = '20061131'

 importing
    out_seqnr   = v_seqnr

 tables
   rgdir       = rgdir
 exceptions
    no_record_found = 1
    others         = 2.

    if sy-subrc <> 0.

 exit.

 endif.

 unpack v_seqnr to rx-key-seqno.
 rx-key-pernr = v_pernr.
 rp-imp-c2-ru.
 if rp-imp-ru-subrc <> 0.
 exit.
 endif.
 clear itab2.


 loop at crt where lgart  = '/BT1' and
                   cumty = 'Y'.
  select perid into itab2-ssn
  from pa0002 where pernr = v_pernr.
endselect.
  select pernr into itab2-pernr
  from pa0001 where pernr = v_pernr.
endselect.

  itab2-BT1y_amt = crt-betrg.
  collect itab2.

 endloop.
 clear itab2.

 loop at crt where lgart  = '/BT1' and
                   cumty = 'Q'.
  select perid into itab2-ssn
  from pa0002 where pernr = v_pernr.
endselect.
  select pernr into itab2-pernr
  from pa0001 where pernr = v_pernr.
endselect.

  itab2-BT1Q_amt = crt-betrg.
  collect itab2.

 endloop.
 clear itab2.

 loop at crt where lgart  = '9011' and
                   cumty = 'Y'.
  select perid into itab2-ssn
  from pa0002 where pernr = v_pernr.
endselect.
  select pernr into itab2-pernr
  from pa0001 where pernr = v_pernr.
endselect.

  itab2-9011_amt = crt-betrg.
  collect itab2.

 endloop.

 endform.
