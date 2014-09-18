*&---------------------------------------------------------------------*
*& Report  Z_WIP_S20                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

*The report uses three parameters : Viz the assembly, flags 'test'
*as well as the flag 'CHKNCAN'.
*The report does a small check and creates warning messages like
*'Confirmation does not exist for order'->it means confirmation
*archieved'Is this order final confirmed?' -> it means last
*confirmation is not
*a GR confirmation. So the order has
*to be checked manually whether it is really final confirmed
*
*If the selection flag CHKMAN is set and any of these warning messages
*appears in the protocol then the report will not proceed with the CPZP
*updation even if the "Test" flag is not set.
*So if you want to correct the WIP then you should clear both selection
*flags.


report  Z_WIP_CLEAR  line-size 250.                              .
tables: PPC_HEAD, PPC_CONF_ACT_VAR,PPC_ORD_INF,QRP002,
        PPC_ACT, PPC_MAT_DET, PPC_MAT.


select-options:
SO_PROD for PPC_ORD_INF-MATERIALNR no intervals.
parameter: P_TEST(1) default 'X',
           CHKNCAN(1) default 'X'.


data: LT_HDR like PPC_HEAD occurs 0,
      LS_HDR like PPC_HEAD,
      LT_ORD like PPC_ORD_INF occurs 0,
      LS_ORD like PPC_ORD_INF,
      LS_CPZP like CPZP,
      LS_CPZP_TMP like CPZP,
      LT_CPZP like CPZP occurs 0,
      LT_CPZP_TMP like CPZP occurs 0,
      L_ACCASSOBJ type PPC_ACCASSOBJ_INT,
      LF_AUFNR type AUFNR,
      LF_OBJNR type J_OBJNR,
      LF_PKOSA_ERROR type C,
      L_D type IST_MENGE,
      L_MNG1 type IST_MENGE,
      L_MNG2 type IST_MENGE,
      L_PROBLEM(1).

clear L_PROBLEM.
if P_TEST is initial.
*--> Set lock to prevent ppc postings
  perform CONF_MAT_ENQUEUE(SAPLPPC1PR).
  call function 'ENQUEUE_E_PPC_HEAD_TIMS'
    exporting
      MODE_PPC_HEAD  = 'E'
      MANDT          = SY-MANDT
      _SCOPE         = '3'
    exceptions
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      others         = 3.
endif.



select * from PPC_ORD_INF into table LT_ORD
where MATERIALNR in SO_PROD.

* check last confirmation.

check not LT_ORD[] is initial.

select * from PPC_HEAD into table LT_HDR
for all entries in LT_ORD
where ORDERID = LT_ORD-ORDERID.

*sort LT_HDR by ORDERID CONFTIME.
sort LT_HDR by ORDERID  ASCENDING
               CONFTIME DESCENDING .


loop at LT_ORD into LS_ORD.
* check final GR
  read table LT_HDR into LS_HDR with key ORDERID = LS_ORD-ORDERID
       binary search.

  if SY-SUBRC eq 0.
    if LS_HDR-FLG_GR_HEAD is initial.
      write: /1 'Is this order final confirmed?',LS_ORD-ORDERNR.
      L_PROBLEM = 'X'.
    endif.
  else.
    write: /1 'Confirmation does not exist for order', LS_ORD-ORDERNR.
    L_PROBLEM = 'X'.
  endif.

* Checking open processes
  loop at LT_HDR into LS_HDR where orderid = ls_ord-orderid.
*  is there any open process
    if LS_HDR-FLG_SYNCH eq ' ' or LS_HDR-FLG_SYNCH = 'B' or
       LS_HDR-FLG_ASYNCH eq ' ' or LS_HDR-FLG_ASYNCH = 'B' or
       LS_HDR-FLG_ASYNCH_A eq ' ' or LS_HDR-FLG_ASYNCH_A = 'B'.
      write: /1 'Order has still open process', LS_ORD-ORDERNR,
                                                LS_HDR-HEADID.
      L_PROBLEM = 'A'.
    endif.
    delete LT_HDR.
  endloop.
endloop.

uline.

if L_PROBLEM = 'A'.
  write: /1
       'ABORT! Please finish first the open confirmations.(Tr.PPCGO)'.
  exit.
endif.

if not L_PROBLEM is initial and
   not CHKNCAN is initial.
  exit.
endif.

free LT_HDR.

sort LT_ORD by ACCASSOBJ.
delete adjacent duplicates from LT_ORD comparing ACCASSOBJ.


loop at LT_ORD into LS_ORD.
  clear LT_CPZP.
  refresh LT_CPZP.


  L_ACCASSOBJ = LS_ORD-ACCASSOBJ.
*   get the OBJNR
  perform OBJNR_GET(SAPLQRPRP) using    L_ACCASSOBJ
                    changing LF_AUFNR
                             LF_OBJNR
                             LF_PKOSA_ERROR.


*ISTMN
*GMPER
*gmsum
  select * from CPZP into table LT_CPZP
  where OBJNR = LF_OBJNR.

  loop at LT_CPZP into LS_CPZP.
    if LS_CPZP-F_OBJNR ne LS_CPZP_TMP-F_OBJNR.
      clear L_D.
    endif.

    LS_CPZP_TMP = LS_CPZP.
    L_MNG1 = LS_CPZP-ISTMN - LS_CPZP-GMSUM .
    L_MNG2 = LS_CPZP-ISTMN + L_D.

* check the period
    if LS_CPZP-ISTMN < 0 and LS_CPZP-GMSUM = 0.
* 0.160- 0.000   0.000
      clear LS_CPZP_TMP-ISTMN.                  "=>
      clear LS_CPZP_TMP-GMPER.

    elseif LS_CPZP-ISTMN  = 0 and
           LS_CPZP-GMPER = 0 and
           LS_CPZP-GMSUM ne 0.                  "=>
*  0.000  0   1.000
      clear LS_CPZP_TMP-GMSUM.

    elseif L_MNG1 ne 0.                         "=>
*( ls_cpzp-istmn - ls_cpzp-gmsum ) ne 0.      "=>
* 7.200  7.200   7.360

      if LS_CPZP-ISTMN = LS_CPZP-GMPER.
        LS_CPZP_TMP-GMSUM = LS_CPZP-ISTMN.      "=>

        L_D = LS_CPZP-GMSUM - LS_CPZP-ISTMN.

      elseif not L_D is initial and
      L_MNG2 = LS_CPZP-GMSUM.                   "=>
*   ( ls_cpzp-istmn + l_d ) = ls_cpzp-gmsum.  "=>
* 3.040  3.200   3.200

        LS_CPZP_TMP-ISTMN = LS_CPZP-GMPER.
        LS_CPZP_TMP-GMSUM = LS_CPZP-GMPER.

      else.
* all other cases
        LS_CPZP_TMP-GMSUM = LS_CPZP-GMPER.
        LS_CPZP_TMP-ISTMN = LS_CPZP-GMPER.

      endif.

    else.
* all other cases
      LS_CPZP_TMP-GMSUM = LS_CPZP-GMPER.
      LS_CPZP_TMP-ISTMN = LS_CPZP-GMPER.

    endif.


    if LS_CPZP_TMP ne LS_CPZP.
      write: /1 'OBJNR', LS_CPZP-OBJNR, 'F_OBJ', LS_CPZP-F_OBJNR,
          'Per', LS_CPZP-GJPER, 'IST', LS_CPZP-ISTMN, LS_CPZP_TMP-ISTMN,
      'GMP', LS_CPZP-GMPER, LS_CPZP_TMP-GMPER,
      'SUM', LS_CPZP-GMSUM, LS_CPZP_TMP-GMSUM.

      append LS_CPZP_TMP to LT_CPZP_TMP.
    endif.
  endloop.

endloop.


if P_TEST is initial.
  update CPZP from table LT_CPZP_TMP.
  commit work.
*--> dequeue
  perform CONF_MAT_DEQUEUE(SAPLPPC1PR).
  call function 'DEQUEUE_E_PPC_HEAD_TIMS'
    exporting
      MODE_PPC_HEAD = 'E'
      MANDT         = SY-MANDT
      _SCOPE        = '3'.
endif.
