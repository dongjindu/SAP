REPORT zhr_bt1_10 .

TABLES: pcl1
       ,pcl2
       ,pa0001
       ,pa0002
       ,pa0003
       .
DATA:   v_pernr       LIKE pa0003-pernr
       ,v_seqnr(5)    TYPE n
       ,v_count_1     TYPE i VALUE 0
       ,v_count_2     TYPE i VALUE 0
       ,v_ssn         LIKE pa0002-pernr
       .

DATA: num-rec-read       TYPE i.
DATA: num-rec-writ       TYPE i.

*data: begin of itab occurs 0,
*        pernr  like pa0001-pernr,
*      end of itab.
*

DATA: BEGIN OF itab OCCURS 0,
        pernr  LIKE pa0001-pernr,
      END OF itab.

DATA:BEGIN OF itab2 OCCURS 0,
        name LIKE pa0001-ename,
        ssn(13)  TYPE c,
        pernr LIKE pa0001-pernr,
        bt1y_amt(8) TYPE p DECIMALS 2,
        bt1q_amt(8) TYPE p DECIMALS 2,
        9011_amt(8)  TYPE p DECIMALS 2,
        END OF itab2.

DATA:BEGIN OF state_tab OCCURS 0,
        state(2)          TYPE c,
        taxau(4)          TYPE c,
        pernr  LIKE pa0003-pernr,
      END OF state_tab.

DATA:BEGIN OF rpt_tab OCCURS 0,
        taxau(4)          TYPE c,
        count             TYPE i,
      END OF rpt_tab.

CLEAR num-rec-read.
CLEAR num-rec-writ.

** Furong on 08/31/12 for deactivate the unused program
Initialization.
Leave program.
** End on 08/31/12

*main form
*form dyn1.
*break 103088.
*write: / '    SSN  ', '       Employee#  ', '      /BT1Y ',
WRITE: /  'Employee#  ', '        9011 ',
 '            /BT1Y ', '           /BT1Q '.

SELECT pernr
  INTO TABLE itab
  FROM pa0001
  WHERE    begda > '20040101'.
*where pernr in ('100295','100630','100612','100121','100458','100336',
*'100627','102613','101640') .


SORT itab BY pernr.
DELETE ADJACENT DUPLICATES FROM itab COMPARING pernr.

LOOP AT itab.
  num-rec-read = num-rec-read + 1.
  v_pernr = itab-pernr.
  PERFORM process_emp.
ENDLOOP.

* uline.
* write: / 'Number of employee record read:' , num-rec-read.



SORT itab2 BY ssn.

LOOP AT itab2.
  num-rec-writ =  num-rec-writ + 1.
*write: / itab2-ssn, ';',
  WRITE: / itab2-pernr, itab2-9011_amt,
  itab2-bt1y_amt, itab2-bt1q_amt.


ENDLOOP.
ULINE.
WRITE: / 'Number of employee record written:' , num-rec-writ.
WRITE: / 'Number of employee record read:' , num-rec-read.


*endform.
* Form Process_emp
*****
* Cluster CU)
* Cluster CU)
INCLUDE: rpc2cd09.

*cluster RU
INCLUDE: rpc2ruu0,
         rpc2rx09.

INCLUDE: rpppxd00,
         rpppxd10,
         rpppxm00.

*&---------------------------------------------------------------------*
*&      Form  process_emp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_emp.

*Get Cluster

  rp-init-buffer.
  cd-key-pernr = v_pernr.
  rp-imp-c2-cu.

  IF rp-imp-cd-subrc <> 0.

    EXIT.
  ENDIF.

*Get the latest cluster of 2006 for EE.

  CALL FUNCTION 'CD_READ_LAST'
    EXPORTING
      begin_date      = '20061001'
      end_date        = '20061031'
    IMPORTING
      out_seqnr       = v_seqnr
    TABLES
      rgdir           = rgdir
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.

    EXIT.

  ENDIF.

  UNPACK v_seqnr TO rx-key-seqno.
  rx-key-pernr = v_pernr.
  rp-imp-c2-ru.
  IF rp-imp-ru-subrc <> 0.
    EXIT.
  ENDIF.
  CLEAR itab2.


  LOOP AT crt WHERE lgart  = '/BT1' AND
                    cumty = 'Y'.
    SELECT perid INTO itab2-ssn
    FROM pa0002 WHERE pernr = v_pernr.
    ENDSELECT.
    SELECT pernr INTO itab2-pernr
    FROM pa0001 WHERE pernr = v_pernr.
    ENDSELECT.

    itab2-bt1y_amt = crt-betrg.
    COLLECT itab2.

  ENDLOOP.
  CLEAR itab2.

  LOOP AT crt WHERE lgart  = '/BT1' AND
                    cumty = 'Q'.
    SELECT perid INTO itab2-ssn
    FROM pa0002 WHERE pernr = v_pernr.
    ENDSELECT.
    SELECT pernr INTO itab2-pernr
    FROM pa0001 WHERE pernr = v_pernr.
    ENDSELECT.

    itab2-bt1q_amt = crt-betrg.
    COLLECT itab2.

  ENDLOOP.
  CLEAR itab2.

  LOOP AT crt WHERE lgart  = '9011' AND
                    cumty = 'Y'.
    SELECT perid INTO itab2-ssn
    FROM pa0002 WHERE pernr = v_pernr.
    ENDSELECT.
    SELECT pernr INTO itab2-pernr
    FROM pa0001 WHERE pernr = v_pernr.
    ENDSELECT.

    itab2-9011_amt = crt-betrg.
    COLLECT itab2.

  ENDLOOP.

ENDFORM.                    "process_emp
