*&---------------------------------------------------------------------*
*& Report  Z_WIP_S20                                                   *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

report  Z_WIP_S20  line-size 250.                              .
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
       EXPORTING
            MODE_PPC_HEAD  = 'E'
            MANDT          = SY-MANDT
            _SCOPE         = '3'
       EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            others         = 3.
endif.



select * from PPC_ORD_INF into table LT_ORD
where MATERIALNR in SO_PROD.

* check last confirmation.
select * from PPC_HEAD into table LT_HDR
for all entries in LT_ORD
where ORDERID = LT_ORD-ORDERID.

sort LT_HDR by ORDERID CONFTIME.

loop at LT_ORD into LS_ORD.
  read table LT_HDR into LS_HDR with key ORDERID = LS_ORD-ORDERID.
  if SY-SUBRC eq 0.
    if LS_HDR-FLG_GR_HEAD is initial.
*      write: /1 'Is this order final confirmed?',LS_ORD-ORDERNR.
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
  data: l_flg(1) type c.
  CALL FUNCTION 'POPUP_TO_DECIDE'
    EXPORTING
*   DEFAULTOPTION           = '1'
      TEXTLINE1               = 'There are open old confirmations'
   TEXTLINE2               =
   'Do you really want to set the WIP to zero?'
*   TEXTLINE3               = ' '
    TEXT_OPTION1            = 'Yes'
    TEXT_OPTION2            = 'No'
*   ICON_TEXT_OPTION1       = ' '
*   ICON_TEXT_OPTION2       = ' '
    TITEL                   = 'Warning'
*   START_COLUMN            = 25
*   START_ROW               = 6
*   CANCEL_DISPLAY          = 'X'
 IMPORTING
   ANSWER                  = L_flg
          .
  if l_flg ne '1'.
    write: /1
         'ABORT! Please finish first the open confirmations.(Tr.PPCGO)'.
    exit.
  endif.
endif.

*if not L_PROBLEM is initial and
*   not CHKNCAN is initial.
*  exit.
*endif.

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
  data: lv_first type char1,
        ls_cpzp_o type cpzp.
  select * from CPZP into table LT_CPZP
  where OBJNR = LF_OBJNR.
  sort lt_cpzp by objnr f_objnr gjper  descending.

  loop at LT_CPZP into LS_CPZP.
    if not ( ls_cpzp_o-objnr = ls_cpzp-objnr and
       ls_cpzp_o-f_objnr = ls_cpzp-f_objnr ).
      lv_first = 'X'.
    endif.
    if lv_first = 'X'.
      ls_cpzp_tmp = ls_cpzp.
      l_d = ls_cpzp-istmn - ls_cpzp-gmsum.
      if l_d ne 0.
        ls_cpzp_tmp-istmn = ls_cpzp-gmsum.
        write: /1 'OBJNR', LS_CPZP-OBJNR, 'F_OBJ', LS_CPZP-F_OBJNR,
          'Per', LS_CPZP-GJPER, 'IST', LS_CPZP-ISTMN, LS_CPZP_TMP-ISTMN,
       'GMP', LS_CPZP-GMPER, LS_CPZP_TMP-GMPER,
       'SUM', LS_CPZP-GMSUM, LS_CPZP_TMP-GMSUM.
        append LS_CPZP_TMP to LT_CPZP_TMP.
      endif.
      if LS_CPZP_TMP ne LS_CPZP.
      endif.
    endif.
    clear lv_first.
    ls_cpzp_o = ls_cpzp.
  endloop.
endloop.


if P_TEST is initial.
  update CPZP from table LT_CPZP_TMP.
  commit work.
*--> dequeue
  perform CONF_MAT_DEQUEUE(SAPLPPC1PR).
  call function 'DEQUEUE_E_PPC_HEAD_TIMS'
       EXPORTING
            MODE_PPC_HEAD = 'E'
            MANDT         = SY-MANDT
            _SCOPE        = '3'.
endif.
