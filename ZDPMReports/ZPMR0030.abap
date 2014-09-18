*&------------------------------------------------------------------
*& Program ID     : ZPMR0030
*& Profram Name   : List & Print of PM Orders
*& Created by     : Myoung ho Park
*& Created on     : 10.17.2005
*& Development ID : ED1K902050
*& Reference Pgm. : ZRPM01_WORK_ORDER(HMMA)
*& Description    : form name : ZPMS0031_C
*&
*& Modification Log
*&====================================================================
*& Date     Developer      Request ID      Description
*& 01/11/2010 Victor    PM20091203-002   Add basic finish date
*&                      added GLTRP field into structure ZSPM0002
*&--------------------------------------------------------------------

report  zpmr0030     no standard page heading
                     line-size 132
                     line-count 64(1)
                     message-id zmpp.                                                .

tables: "zspm0001,   "//PM Order Header
        zspm0002,   "//PM Order Items
        zspm_comp,
        viaufkst,   "//PM Order Selection by Status
        diaufk,     "//PM Order
        rihea,      "//PM hierarchy selection/list screen
        iflo,       "//Functional Location (View)
        viafvc,     "//PM: MaintOperations
        viqmel,     "//Notification Header
        afvc,       "//Operation within an order
        afru,       "//Order completion confirmations
        afko,       "//Order header data PP orders
        zlongtext,
        qmma,       "//Quality notification - activities
        jest,       "//Individual Object Status
        aufk,
        viauf_afvc,
        zspm0006.   "//Inspection Completion

*** internal table for orders list
data : it_temp_order like zspm0002 occurs 0 with header line.
data : it_order      like zspm0002 occurs 0 with header line.
data : it_order_num  like zspm0002 occurs 0 with header line.

data : begin of it_matnr occurs 0,    "04.20.2012 Victor
   matnr like mara-matnr,
   maktx like makt-maktx,
   bdmng like resb-bdmng,
   lgpbe like mard-lgpbe,
   lgort like resb-lgort,
  end of it_matnr.

*** internal table for selected opreations list
data : it_zspm0002 like it_order occurs 0 with header line.



data : st_header  like  bapi_alm_order_header_e.
data : it_partner like bapi_alm_order_partner occurs 0
                                              with header line,
it_operations like bapi_alm_order_operation_e occurs 0
                                              with header line,
it_components like bapi_alm_order_component_e occurs 0
                                              with header line,
it_relations like bapi_alm_order_relation_export occurs 0
                                              with header line,
it_txt like bapi_alm_text occurs 0
                                              with header line,
it_txt_line like bapi_alm_text_lines occurs 0
                                               with header line,
it_texts like bapi_alm_text occurs 0
                                              with header line,
it_text_lines like bapi_alm_text_lines occurs 0
                                               with header line,
it_prts like bapi_alm_order_prt_e occurs 0
                                               with header line,
it_costs_sum like bapi_alm_order_costs_sum_e occurs 0
                                               with header line,
it_costs_details like bapi_alm_order_costs_detail_e occurs 0
                                               with header line,
it_return like  bapiret2 occurs 0
                                               with header line.



*** internal table for All opreations list
data : it_temp like it_order occurs 0 with header line.

*** Header info structure
*DATA : wa_zspm0001 LIKE zspm0001.

data: wa_index like sy-tabix.

*** Range for Maintenance processing stage
ranges: r_iphas  for viaufks-iphas.

** for read long text
data: it_longtext like zlongtext occurs 0 with header line.

data: begin of wa_afko occurs 0,
        aufnr like afko-aufnr,
        vornr like afvc-vornr,
        aufpl like afko-aufpl,
        aplzl like afvc-aplzl,
      end of wa_afko.

data: it_afko like wa_afko occurs 0 with header line.

data: it_tline like tline occurs 0 with header line.


***** Overhaul Inspection List
data : begin of it_inspec occurs 0.
        include structure zspm0005.
data : end of it_inspec.



*** internal table for list
data : begin of it_task occurs 0,
         check.
        include structure diplko.
data : end of it_task.

data : begin of it_plpo occurs 0.
        include structure plpod.
data : end of it_plpo.

data : st_plko like plkod.

** For ALV
type-pools: slis.

data : gv_repid like sy-repid.
data : gv_status       type slis_formname value 'PF_STATUS'.
data : gv_user_command type slis_formname value 'USER_COMMAND'.
data : gv_layout       type slis_layout_alv.
data : it_sort         type slis_t_sortinfo_alv with header line .
data : gv_col_pos type i.

data : it_fieldcat          type slis_t_fieldcat_alv,
       wa_fieldcat          like line of it_fieldcat,
       it_eventcat          type slis_t_event,
       wa_eventcat          like line of it_eventcat.

data : it_events            type slis_t_event,
       it_event_exit      type slis_t_event_exit.
data : it_symbol type icon.

*.. add by sgcho 09.10.2012.
data : it_print type sorted table of ztpm0050 with header line
                with unique key aufnr.

data : begin of it_print_info occurs 0,
       aufnr like ztpm0050-aufnr,
       end of it_print_info.

data : it_print_save like ztpm0050 occurs 0 with header line.
*.. end.
*********** SELECTION-SCREEN ***********************************
****************************************************************
selection-screen begin of block block1 with frame title text-001.

selection-screen begin of line.
****Outstanding (Notification or Order)
parameter : dy_ofn like rihea-dy_ofn.
selection-screen comment 3(29) text-002 for field dy_ofn.
selection-screen position 40.
****Completed (notifications or orders)
parameter : dy_mab like rihea-dy_mab.
selection-screen comment 50(10) text-003 for field dy_mab.
selection-screen end of line.

selection-screen end of block block1.

selection-screen begin of block block2 with frame title text-004.

selection-screen begin of line.
selection-screen comment 1(28) h_shop.
select-options : s_beber  for viqmel-beber no-extension no intervals
                                              obligatory .
selection-screen comment 50(15) t_fing.
selection-screen end of line.

selection-screen begin of line.
selection-screen comment 1(28) h_tplnr.
*select-options : s_stort for viqmel-stort no-extension no intervals.
select-options : s_tplnr for viauf_afvc-tplnr.
*selection-screen comment 50(15) t_ktext.
selection-screen end of line.

select-options : s_auart  for aufk-auart, "zspm_comp-auart-Order type
                 s_datuv  for rihea-termab.   "//Period
*.. modify by sgcho 09.10.2012
*SELECT-OPTIONS : S_STEUS FOR  VIAFVC-STEUS.   "//Control key
*SELECT-OPTIONS :  s_ordate FOR sy-datum,
*                  s_orby FOR aufk-ernam.

select-options :  s_ordate for sy-datum modif id h01,
                  s_orby for aufk-ernam modif id h01.

select-options : s_equnr for viqmel-equnr,
                 s_plgrp for afko-plgrp.
*.. end
selection-screen end of block block2.

*.. add by sgcho 2012.09.10
selection-screen begin of block block3 with frame title text-005.
selection-screen begin of line.
parameters : p_all radiobutton group gr1 default 'X'.
selection-screen comment 3(10) text-006 for field p_all.

selection-screen position 16.
parameters : p_print radiobutton group gr1.
selection-screen comment 18(10) text-007 for field p_print.

selection-screen position 33.
parameters : p_nopnt radiobutton group gr1.
selection-screen comment 35(12) text-008 for field p_nopnt.
selection-screen end of line.
selection-screen end of block block3.
*.. end
******************* INITIALIZATION ********************************
*******************************************************************
*** default check Outstanding
*** Period current month
initialization.
  dy_ofn = 'X'.
*  CONCATENATE SY-DATUM(6) '01' INTO S_DATUV-LOW.
  s_datuv-low = sy-datum - 30.
  s_datuv-high = sy-datum.
  append s_datuv.

  h_shop = text-m05.
  h_tplnr = text-m06.

  s_auart-low = 'PM01'.
  s_auart-high = 'PM03'.
  append s_auart.

*at selection-screen on value-request for s_stort-low.
*  perform stort_inputhelp_f4.

at selection-screen on value-request for s_auart-low.
  perform auart_inputhelp_f4 using 'LOW'.

at selection-screen on value-request for s_auart-high.
  perform auart_inputhelp_f4 using 'HIGH'.

at selection-screen on block block2.
  select single fing into t_fing
                from t357 where beber = s_beber-low.

*  select single ktext into t_ktext
**               FROM t499s WHERE werks = 'P001'
*              from t499s where werks = 'KVA1'
*                          and   stand = s_stort-low.
***************** AT SELECTION-SCREEN ******************************
********************************************************************
at selection-screen.

  case sy-ucomm.
    when 'ONLI'.
      clear : sy-ucomm.
      clear : it_temp_order, it_temp_order[].
      clear : it_order, it_order[].
      perform read_data.
**** { INSERT 2003/12/29...
      perform check_countermeasure.
****  INSERT 2003/12/29... }
*      CALL SCREEN 9000.
      set titlebar 'T_9000'.
*  SET PF-STATUS 'G_9000'.

      gv_repid = sy-repid.

* Preparation of ALV
      perform pre_report_adj.

* Call ALV LIST
      perform call_alv_list.

    when others.
      clear: sy-ucomm.

  endcase.

at selection-screen output.
  if sy-tcode = 'ZPMF003'.
    loop at screen.
*.. modify by sgcho 09.10.2012
*      IF screen-name = 'S_ORDATE-LOW' OR screen-name = 'S_ORDATE-HIGH' OR
*         screen-name = 'S_ORBY-LOW' OR screen-name = 'S_ORBY-HIGH'.
*        screen-invisible = 'X'.
*        screen-active = '0'.
*        MODIFY SCREEN.
*      ENDIF.
      if screen-group1 = 'H01'.
        screen-invisible = '1'.
        screen-active = '0'.
        modify screen.
      endif.
*.. end
    endloop.
  endif.
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form read_data.

  data : wa_rueck like afru-rueck,
         wa_rmzhl like afru-rmzhl.

  if dy_ofn eq ' ' and dy_mab eq ' '.
    message e000(zz) with text-m01.
  endif.

  perform fill_r_iphas.

  if dy_ofn eq 'X' and dy_mab eq ' '.
***  Selection Outstanding (not exits AFRU)
*** I0076         E        DLFL   Deletion flag
*** I0043         E        LKD    Locked
*** I0002         E        REL    Released
*** I0001         E        CRTD   Created
*** I0009         E        CNF    Confirmed

    select distinct a~aufnr a~auart a~stort a~equnr a~vornr
                    b~msgrp c~ktext b~beber b~qmnum b~swerk
                    c~erdat c~gltrp     "add gltrp 01/11/2010
                    a~iedd  b~qmnam
                    c~plnty c~plnnr"
                    d~warpl e~wptxt
             into corresponding fields of table it_temp_order
            from viauf_afvc as a
                  inner join viqmel as b
                  on a~aufnr = b~aufnr
                      inner join caufv as c
                      on b~aufnr = c~aufnr
             inner join mpos as d
               on d~equnr = a~equnr
              and d~qmnum = b~qmnum
               inner join afko as f
                      on a~aufnr = f~aufnr
       inner join mpla as e
               on d~warpl = e~warpl
            where a~auart in s_auart
            and   a~addat in s_datuv
            and   a~iphas in r_iphas
            and   a~tplnr in s_tplnr
            and   b~beber in s_beber
            and  a~equnr in s_equnr
            and f~plgrp in s_plgrp
*            AND   A~STEUS IN S_STEUS
            and   not exists ( select * from jest
                              where objnr = a~objnr
                              and ( ( stat  = 'I0076' and inact <> 'X' )
                              or    ( stat  = 'I0043' and inact <> 'X' )
                              or  ( stat  = 'I0009' and inact <> 'X' ) )
                              )
            and   exists ( select * from jest
                           where objnr = a~objnr
                           and   ( stat  = 'I0002' and inact <> 'X' ) ).

*    IF sy-subrc EQ 0.
    if not it_temp_order[] is initial.
** end on 08/24/2011
      loop at it_temp_order.
        select single rueck  rmzhl  into (wa_rueck, wa_rmzhl)
               from  afru
               where aufnr = it_temp_order-aufnr
               and   vornr = it_temp_order-vornr
               %_hints oracle 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        if sy-subrc eq 0.
          select  single *
                  from afru
                  where aufnr = it_temp_order-aufnr
                  and   vornr = it_temp_order-vornr
                  and   rueck = wa_rueck
                  and   rmzhl = wa_rmzhl
                  and   aueru eq 'X'
                  and   stokz eq ' '
                  and   stzhl eq space.
          if sy-subrc eq 0.

          else.
            move-corresponding it_temp_order to it_order.
            if it_temp_order-equnr is initial.
              move it_temp_order-stort to it_order-equnr.
            endif.
            move ''  to it_order-vornr.
            collect it_order.
          endif.
        else.
          move-corresponding it_temp_order to it_order.
          if it_temp_order-equnr is initial.
            move it_temp_order-stort to it_order-equnr.
          endif.
          move ' '  to it_order-vornr.
*          MOVE ' ' TO IT_ORDER-STAT.
          collect it_order.
        endif.
      endloop.
    else.
      message e000(zz) with text-m02.
    endif.
  elseif dy_ofn eq ' ' and dy_mab eq 'X'.
***  Selection Complete
    select distinct a~aufnr a~auart a~stort a~equnr a~vornr
                    b~msgrp c~ktext b~qmnum a~iedd b~qmnam
                    b~beber b~swerk c~erdat c~plnty c~plnnr
                    c~gltrp     "add gltrp 01/11/2010
            into corresponding fields of table it_temp_order
            from viauf_afvc as a
                  inner join viqmel as b
                  on a~aufnr = b~aufnr
                      inner join caufv as c
                      on b~aufnr = c~aufnr
             inner join afko as e
                      on a~aufnr = e~aufnr
            where a~auart in s_auart
            and   a~addat in s_datuv
            and   a~iphas in r_iphas
            and   a~tplnr in s_tplnr
            and   b~beber in s_beber
            and  a~equnr in s_equnr
             and e~plgrp in s_plgrp
*            AND   A~STEUS IN S_STEUS
            and   not exists ( select * from jest
                       where objnr = a~objnr
                       and   ( ( stat  = 'I0076' and inact <> 'X' )
                       or   ( stat  = 'I0043' and inact <> 'X' ) ) )
            and   exists ( select * from jest
                       where objnr = a~objnr
                       and   ( stat  = 'I0002' and inact <> 'X' ) ).

    if not it_temp_order[] is initial.

      loop at it_temp_order.
        select single rueck  rmzhl  into (wa_rueck, wa_rmzhl)
               from  afru
               where aufnr = it_temp_order-aufnr
               and   vornr = it_temp_order-vornr
               %_hints oracle 'INDEX_DESC(AFRU, "AFRU~Z01")'.

        if sy-subrc eq 0.
          select  single *
                  from afru
                  where aufnr = it_temp_order-aufnr
                  and   vornr = it_temp_order-vornr
                  and   rueck = wa_rueck
                  and   rmzhl = wa_rmzhl
                  and   aueru eq 'X'
                  and   stokz eq ' '
                  and   stzhl eq space.
          if sy-subrc eq 0.
            move-corresponding it_temp_order to it_order.
            if it_temp_order-equnr is initial.
              move it_temp_order-stort to it_order-equnr.
            endif.
            move ''  to it_order-vornr.
            move '' to it_order-stat.
            collect it_order.
          endif.
        endif.
      endloop.
    else.
      message e000(zz) with text-m02.
    endif.

  elseif dy_ofn eq 'X' and dy_mab eq 'X'.
****  Selection Outstanding & Complete
    select distinct a~aufnr a~auart a~stort a~equnr a~vornr
                    b~msgrp c~ktext b~qmnum a~iedd b~qmnam
                    b~beber b~swerk c~erdat c~plnty c~plnnr
                    c~gltrp     "add gltrp 01/11/2010
            into corresponding fields of table it_temp_order
            from viauf_afvc as a
                  inner join viqmel as b
                  on a~aufnr = b~aufnr
                      inner join caufv as c
                      on b~aufnr = c~aufnr
            where a~auart in s_auart
            and   a~addat in s_datuv
            and   a~iphas in r_iphas
            and   a~tplnr in s_tplnr
            and   b~beber in s_beber
            and  a~equnr in s_equnr
*            AND   A~STEUS IN S_STEUS
            and   not exists ( select * from jest
                      where objnr = a~objnr
                      and   ( ( stat  = 'I0076' and inact <> 'X' )
                      or  ( stat  = 'I0043' and inact <> 'X' ) ) )
            and   exists ( select * from jest
                      where objnr = a~objnr
                      and  ( stat  = 'I0002' and inact <> 'X' ) ).

    if sy-subrc ne 0.
      message e000(zz) with text-m02.
    else.
      loop at it_temp_order.
        move-corresponding it_temp_order to it_order.
        if it_temp_order-equnr is initial.
          move it_temp_order-stort to it_order-equnr.
        endif.
        move ''  to it_order-vornr.
*        MOVE '' TO IT_ORDER-STAT.
        collect it_order.
      endloop.
    endif.
  endif.
endform.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9000 output.
  set titlebar 'T_9000'.
*  SET PF-STATUS 'G_9000'.

  gv_repid = sy-repid.

* Preparation of ALV
  perform pre_report_adj.

* Call ALV LIST
  perform call_alv_list.

endmodule.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form pre_report_adj.
* Building Field Cat.
  clear : gv_col_pos, it_fieldcat, it_fieldcat[].

  perform build_fieldcat using
    'IT_ORDER' 'AUFNR'  'X'     space space
    space    '12'     text-c02   space space space space.

  perform build_fieldcat using
    'IT_ORDER' 'KTEXT'  ' '     space space
     space    '40'     text-c06 space space space space.

  perform build_fieldcat using
    'IT_ORDER' 'EQUNR'  ' '     space space
     space    '8'    text-c04  space space space space.

  perform build_fieldcat using
    'IT_ORDER' 'EQKXT'  ' '     space space
     space    '8'    text-c18  space space space space.

  perform build_fieldcat using
    'IT_ORDER' 'BEBER'  ' '     space space
     space    '4'     text-c12 space space space space.

  perform build_fieldcat using
    'IT_ORDER' 'STORT'  ' '     space space
     space    '8'     text-c13 space space space space.


  perform build_fieldcat using
    'IT_ORDER' 'MSGRP'  ' '     space space
     space    '8'    text-c05   space space space space.

  perform build_fieldcat using
    'IT_ORDER' 'AUART'  ' '     space space
     space    '4'     text-c03  space space space space.

*  PERFORM build_fieldcat USING
*    'IT_ORDER' 'STORT'  'X'     space space
*    space    '12'     text-c01  space space space space.

*  PERFORM build_fieldcat USING
*    'IT_ORDER' 'OIEXIST'  ' '     space space
*     space    '4'     text-c10    space space space 'X'.
*
*  PERFORM build_fieldcat USING
*    'IT_ORDER' 'WSEXIST'  ' '     space space
*     space    '4'     text-c11    space space space 'X'.

*.. add by sgcho 09.10.2012
  perform build_fieldcat using
    'IT_ORDER' 'PRINT_FLAG'  ' '     space space
     space    '4'     text-c14       space space space 'X'.

  perform build_fieldcat using
    'IT_ORDER' 'PRINT_USR'  ' '     space space
     space    '10'     text-c15     space space space 'X'.

  perform build_fieldcat using
    'IT_ORDER' 'PRINT_DAT'  ' '     space space
     space    '10'     text-c16     space space space 'X'.

  perform build_fieldcat using
    'IT_ORDER' 'PRINT_TIM'  ' '     space space
     space    '10'     text-c17     space space space 'X'.


  perform build_fieldcat using
    'IT_ORDER' 'WARPL'  ' '     space space
     space    '12'     text-c19     space space space 'X'.

  perform build_fieldcat using
    'IT_ORDER' 'WPTXT'  ' '     space space
     space    '40'     text-c20     space space space 'X'.

*.. end
*** Sort
  sort it_order by aufnr.
  clear: it_order .

*  it_sort-fieldname = 'STORT'.
*  it_sort-up        = 'X'.
*  it_sort-expa      = 'X'.
*  it_sort-subtot    = 'X'.
*  APPEND it_sort.

  it_sort-fieldname = 'AUFNR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.

  it_sort-fieldname = 'KTEXT'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.

  it_sort-fieldname = 'AUART'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.

  it_sort-fieldname = 'EQUNR'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  append it_sort.

*  it_sort-fieldname = 'MSGRP'.
*  it_sort-up        = 'X'.
*  it_sort-expa      = 'X'.
*  it_sort-subtot    = 'X'.
*  APPEND it_sort.
*


*** Set Event
  data : wa_l_event  type slis_alv_event.
  wa_l_event-name = slis_ev_top_of_page.
  wa_l_event-form = 'BASIC_TOP_OF_PAGE'.
  append wa_l_event to it_events.

****
  gv_layout-box_fieldname = 'CHK'.
  gv_layout-zebra = 'X'.
*  GV_LAYOUT-COLWIDTH_OPTIMIZE = 'X'.

endform.                    " PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_alv_list.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program       = gv_repid
      i_callback_pf_status_set = gv_status
      i_callback_user_command  = gv_user_command
      it_fieldcat              = it_fieldcat[]
      it_sort                  = it_sort[]
      i_save                   = 'A'
      it_events                = it_events
      it_event_exit            = it_event_exit  "
      is_layout                = gv_layout
    tables
      t_outtab                 = it_order
    exceptions
      program_error            = 1
      others                   = 2.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " CALL_ALV_LIST
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1197   text
*      -->P_1198   text
*      -->P_1199   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_1203   text
*      -->P_1204   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
form build_fieldcat using    value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110)
                             value(p_0120).


  add 1 to gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-icon        = p_0120.
  wa_fieldcat-col_pos     = gv_col_pos.


  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

endform.                    " BUILD_FIELDCAT
*---------------------------------------------------------------------*
*       FORM BASIC_TOP_OF_PAGE                                        *
*---------------------------------------------------------------------*
*       Display Title                                                 *
*---------------------------------------------------------------------*
form basic_top_of_page.
  data: it_commentary type slis_t_listheader with header line.

  it_commentary-typ = 'S'.
  it_commentary-info = text-t01.
  append it_commentary.

  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      it_list_commentary = it_commentary[].

endform.                    "BASIC_TOP_OF_PAGE
*-----------------------------------------------------------------------
*    FORM PF_STATUS_VAR
*-----------------------------------------------------------------------
form pf_status using  extab type slis_t_extab.
  set pf-status 'STANDARD'  excluding extab. "
*  SET PF-STATUS 'G_9000'  EXCLUDING EXTAB. "
endform.                    "PF_STATUS
*&---------------------------------------------------------------------*
*&      FORM  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       User Command for ALV List
*----------------------------------------------------------------------*
form user_command  using r_ucomm
                         rs_selfield type slis_selfield.

  data: l_objid like crfh-objid,
        l_objty like crfh-objty,
        l_exp like crfhd,
        l_filep type string,
        lt_crtxd like table of crtxd.

  case r_ucomm.

    when 'PREW'.
***  Work Order List (Smartform)
      clear: r_ucomm.
      loop at it_order where chk = 'X'.
        rs_selfield-tabindex = sy-tabix.
*        PERFORM check_order.
        perform call_work_order_list using rs_selfield-tabindex.
*        PERFORM get_spare_list.
        perform review_order.
      endloop.

    when 'PRINT'.
***  Work Order List (Smartform)
      clear: r_ucomm.
*.. Add by sgcho 2012.09.10
      clear : it_print_info, it_print_info[].

      loop at it_order where chk = 'X'.
        rs_selfield-tabindex = sy-tabix.
*        PERFORM check_order.
        perform call_work_order_list using rs_selfield-tabindex.
*        PERFORM get_spare_list.
        perform print_order using it_order-aufnr.

        clear: l_objid, l_objty, l_exp, l_filep.

        select single objid objty into (l_objid, l_objty)
            from afko as a
          inner join affh as b
          on a~aufpl = b~aufpl
          where aufnr = it_order-aufnr.
        if sy-subrc = 0.
          call function 'CF_RF_FHM_MASTER_READ'
            exporting
              objid_imp     = l_objid
              objty_imp     = l_objty
            importing
              crfhd_exp     = l_exp
            tables
              crtxd_exp     = lt_crtxd
            exceptions
              not_found_fhm = 1
              others        = 2.
          if sy-subrc = 0 and l_exp-doknr is not initial.
            select single filep into l_filep
            from draw
            where dokar = 'DRF'
              and doknr =  l_exp-doknr.
            if sy-subrc = 0.
              perform print_check_list using l_filep.
            endif.
          endif.
        endif.
      endloop.
*.. add by sgcho 2012.09.10
      if not it_print_info[] is initial.
        perform update_print_info.
        rs_selfield-refresh    = 'X'.
        rs_selfield-col_stable = 'X'.
        rs_selfield-row_stable = 'X'.

        perform check_countermeasure.
      endif.
*.. end

    when  'F03' or  'F15' or 'F12'.
*    WHEN  'BACK' OR 'EXIT' OR  'CANC'.
      clear: r_ucomm.
*      CALL SCREEN 1000.
      leave to screen 0.

    when '&IC1'.                                            "'&IC1'.
***  Call Transaction 'IW33' : Display PM Order
      clear: r_ucomm.
      read table it_order index rs_selfield-tabindex.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = it_order-aufnr
        importing
          output = it_order-aufnr.

      set parameter id 'ANR' field it_order-aufnr.
      call transaction 'IW33' and skip first screen.

  endcase.

endform.                 " USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  CALL_WORK_ORDER_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form call_work_order_list using p_index.

  data: wa_rueck like afru-rueck,
        wa_rmzhl like afru-rmzhl,
        wa_first_item.
  data: l_cnt type i.

  clear : it_text_lines[].

  read table it_order index p_index.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = it_order-aufnr
    importing
      output = it_order-aufnr.

*  IF sy-subrc EQ 0.
  call function 'BAPI_ALM_ORDER_GET_DETAIL'
    exporting
      number           = it_order-aufnr
    importing
      es_header        = st_header
    tables
      et_partner       = it_partner
      et_operations    = it_operations
      et_components    = it_components
      et_relations     = it_relations
*     et_texts         = it_texts
*     et_text_lines    = it_text_lines
      et_prts          = it_prts
      et_costs_sum     = it_costs_sum
      et_costs_details = it_costs_details
      return           = it_return
*     EXTENSION_IN     =
*     EXTENSION_OUT    =
    .

  data :    lf_txt,
             lf_txt_line type tdline,
             lf_sum_line type tdline.

  loop at it_operations.
    wa_index = sy-tabix.

    clear : it_txt[], it_txt_line[].
    call function 'IBAPI_ALM_TEXT_READ'
      exporting
        iv_aufnr               = it_order-aufnr
        iv_vornr               = it_operations-activity
        iv_uvorn               = it_operations-sub_activity
        iv_spras               = sy-langu
      changing
        ct_text                = it_txt[]
        ct_text_lines          = it_txt_line[]
      exceptions
        object_has_no_longtext = 1
        texttype_not_suppoted  = 2
        text_read_error        = 3
        others                 = 4.

    loop at it_txt_line.
      move-corresponding it_txt_line to it_text_lines.
* 07/11/13 T00306 Start
      it_text_lines-tdformat = wa_index.
* 07/11/13 T00306 End
      append it_text_lines. clear it_text_lines.
    endloop.

* 07/15/13 T00306 Start
    describe table it_text_lines lines l_cnt.

    read table it_text_lines index l_cnt.
    if sy-subrc eq 0.
      if it_text_lines-tdline is initial.
        loop at it_text_lines.
          l_cnt = l_cnt - 1.
          read table it_text_lines index l_cnt.
          if it_text_lines-tdline is initial.
            delete it_text_lines index sy-tabix.
          else.
            exit.
          endif.
        endloop.
      else.
        clear it_txt_line.
        it_txt_line-tdformat = it_text_lines-tdformat.
        append it_txt_line to it_text_lines.
      endif.
    endif.
* 07/15/13 T00306 End

*    PERFORM check_resb_data CHANGING it_order-aufnr
*                                  it_operations-activity
*                                  it_operations-usr10.
    modify it_operations index wa_index
                         transporting usr10.
  endloop.

endform.                    " CALL_WORK_ORDER_LIST
*

*&---------------------------------------------------------------------*
*&      Form  FILL_R_IPHAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_r_iphas.
  refresh r_iphas.

  r_iphas-sign   = 'I'.
  r_iphas-option = 'EQ'.

  r_iphas-low = '2'.                   "FREI
  append r_iphas.
  r_iphas-low = '3'.                   "technisch abgeschlossen
  append r_iphas.
*--- Bei IW48 nur Auftr?e die r?kgemeldet werden k?nen -----------*
  if sy-tcode <> 'IW48'.
    r_iphas-low = '0'.                 "OFFEN
    append r_iphas.
    r_iphas-low = '1'.                 "ZUR?KGEST
    append r_iphas.
    r_iphas-low = '4'.                 "L?chkennzeichen
    append r_iphas.
    r_iphas-low = '6'.                 "kaufm?nisch abgeschlossen(ab46)
    append r_iphas.
  endif.

endform.                    " FILL_R_IPHAS
*&---------------------------------------------------------------------*
*&      Form  CHECK_COUNTERMEASURE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_countermeasure.

*.. Add by sgcho 10.09.2012.
*.. Get Print Info.
  data : l_aufnr like it_order-aufnr,
         l_equnr like eqkt-equnr.

  perform get_print_info.
*.. End
  loop at it_order.
    move : sy-tabix to wa_index.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = it_order-aufnr
      importing
        output = it_order-aufnr.

    call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
      exporting
        input  = it_order-equnr
      importing
        output = it_order-equnr.
*    select single *
*           from viqmel
*           where qwrnum = it_order-qmnum
*           and  not exists ( select * from jest
*                               where objnr   = viqmel~objnr
*                               and   ( stat  = 'I0076'
*                               or      stat  = 'I0043' ) )
**** Added on 04/13/11 requested by tuning team
*      %_hints oracle 'RULE'.
*** End  of addition
*
*    if sy-subrc eq 0.
*      move : '@01@' to it_order-zcounter.
*    endif.
*.. add by sgcho 09.10.2012
    clear : l_aufnr.
    move : it_order-aufnr to l_aufnr.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = l_aufnr
      importing
        output = l_aufnr.

    read table it_print with key aufnr = l_aufnr
                                 binary search.
    if sy-subrc = 0.
      move : it_print-print_flag to it_order-print_flag,
             it_print-print_usr  to it_order-print_usr,
             it_print-print_dat  to it_order-print_dat,
             it_print-print_tim  to it_order-print_tim.
    endif.
*.. end

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = it_order-equnr
      importing
        output = l_equnr.

    select single eqktx into it_order-eqkxt
               from   eqkt
               where  equnr = l_equnr
               and    spras = sy-langu.
*
    modify it_order index wa_index.
  endloop.

*.. Add by sgcho 2012.09.10.
  case 'X'.
    when p_print.
      delete it_order where print_flag = space.
    when p_nopnt.
      delete it_order where print_flag = 'Y'.
  endcase.
*.. end
endform.                    " CHECK_COUNTERMEASURE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_order.

endform.                    " CHECK_ORDER

*** INSERT BY FURONG

form read_longtext tables   p_tline structure tline
                   using    p_thead    structure thead.

  call function 'READ_TEXT'
       exporting
            client                  = sy-mandt
            id                      = p_thead-tdid
            language                = sy-langu
            name                    = p_thead-tdname
            object                  = p_thead-tdobject
            archive_handle          = 0
            local_cat               = ' '
*       IMPORTING
*            HEADER                  =
       tables
            lines                   = p_tline
       exceptions
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            others                  = 8.

  if sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

endform.                    " READ_LONGTEXT
*&---------------------------------------------------------------------*
*&      Form  STORT_INPUTHELP_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*form stort_inputhelp_f4 .
*  data : lv_beber(5).
*  data : progname like sy-repid,
*         dynnum   like sy-dynnr.
*
*  data : lt_dynpfields like dynpread occurs 0 with header line.
*
*  data : begin of lt_499s occurs 0,
*          stand like t499s-stand,
*          ktext like t499s-ktext,
*       end of lt_499s.
*
*  progname = sy-repid.
*  dynnum   = sy-dynnr.
*
*  clear : lt_dynpfields, lt_dynpfields[].
*
*  lt_dynpfields-fieldname = 'S_BEBER-LOW'.
*  append lt_dynpfields.
*
*  call function 'DYNP_VALUES_READ'
*    exporting
*      dyname     = progname
*      dynumb     = dynnum
*    tables
*      dynpfields = lt_dynpfields.
*
*  read table lt_dynpfields index 1.
**  TRANSLATE lt_dynpfields-fieldvalue TO UPPER CASE.
*  concatenate lt_dynpfields-fieldvalue(2) '%' into lv_beber.
*
*  select * into corresponding fields of table lt_499s
*       from t499s
*         where stand like lv_beber.
*  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
*    exporting
*      retfield    = 'STAND'     "
*      dynpprog    = progname
*      dynpnr      = dynnum
*      dynprofield = 'S_STORT-LOW' "
*      value_org   = 'S'
*    tables
*      value_tab   = lt_499s.  "
*endform.                    " STORT_INPUTHELP_F4
*&---------------------------------------------------------------------*
*&      Form  REVIEW_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form review_order .
  perform print_order_form using 'X'
                                 ' '.
endform.                    " REVIEW_ORDER
*&---------------------------------------------------------------------*
*&      Form  PRINT_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form print_order using p_aufnr.
  perform print_order_form using ' '
                                 p_aufnr.
endform.                    " PRINT_ORDER
*&---------------------------------------------------------------------*
*&      Form  PRINT_ORDER_FORM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2288   text
*----------------------------------------------------------------------*
form print_order_form  using    p_review
                                p_aufnr.

  data : st_options  type ssfcompop.
  data : st_device   type ssfctrlop.
*  DATA : wa_formname TYPE tdsfname VALUE 'ZPMS0031_C'.
  data : wa_formname type tdsfname value 'ZSPMWORKORDER_NEW'.
  data : wa_funcname type rs38l_fnam .

*** SAP Smart Forms: Form call
*** Get SmartForm's function name (like : /1BCDWB/SF00000036 )
*** Because funtion name is dependent on system client...
  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname           = wa_formname
    importing
      fm_name            = wa_funcname
    exceptions
      no_form            = 1
      no_function_module = 2
      others             = 3.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  check sy-subrc = 0.

  if p_review eq 'X'.
    st_device-preview    =  'X'.
    st_device-no_dialog  =  'X'.
    st_device-getotf     =  space.
    st_device-langu      =  sy-langu.
    st_options-tdimmed   =  'X'.
    st_options-tddelete  =  'X'.
    st_options-tdnewid   =  'X'.
    st_options-tdnoprev  =  'X'.
    st_options-tdcover   =  'S'.
  else.
    st_device-no_dialog = 'X'.
    st_options-tdnewid  = 'X'.
    st_options-tdimmed  = 'X'.
    st_options-tddelete = 'X'.
  endif.

  data : l_lines type i.

  describe table it_text_lines lines  l_lines.

  while l_lines < 15.
    clear it_text_lines.
    it_text_lines-tdline = '.'.
    append it_text_lines.
    describe table it_text_lines lines  l_lines.
  endwhile.

  call function wa_funcname
    exporting
      control_parameters = st_device
      output_options     = st_options
      i_header           = st_header
    tables
      t_partner          = it_partner
      t_operations       = it_operations
      t_texts            = it_text_lines
    exceptions
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      others             = 5.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*.. add by sgcho 2012.09.10
  else.
    if not p_aufnr is initial.
      move : it_order-aufnr to it_print_info-aufnr.
      append it_print_info. clear it_print_info.
    endif.
*.. end
  endif.

endform.                    " PRINT_ORDER_FORM
*&---------------------------------------------------------------------*
*&      Form  AUART_INPUTHELP_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form auart_inputhelp_f4 using p_feild.
  data : begin of lt_t003o occurs 0,
          auart like t003o-auart,
          txt   like t003p-txt,
         end of lt_t003o.

  select * into corresponding fields of table lt_t003o
       from t003o as a
           inner join t003p as b
           on a~auart = b~auart
         where a~autyp = '30'
         and   a~auart like 'PM%'
         and   b~spras = sy-langu.

  if p_feild = 'LOW'.
    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield    = 'AUART'
        dynpprog    = 'ZPMR0030'
        dynpnr      = '1000'
        dynprofield = 'S_AUART-LOW'
        value_org   = 'S'
      tables
        value_tab   = lt_t003o.
  else.
    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield    = 'AUART'
        dynpprog    = 'ZPMR0030'
        dynpnr      = '1000'
        dynprofield = 'S_AUART-HIGH'
        value_org   = 'S'
      tables
        value_tab   = lt_t003o.
  endif.
endform.                    " AUART_INPUTHELP_F4
*&---------------------------------------------------------------------*
*&      Form  CHECK_OVERHAUL_INSPC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ORDER_AUFNR  text
*      -->P_IT_ORDER_OIEXIT  text
*----------------------------------------------------------------------*
*FORM check_overhaul_inspc  USING    p_aufnr
*                                    p_oiexist
*                                    p_overh
*                                    p_abnum
*                                    p_nplda.
*
*  DATA : lv_aufnr LIKE afih-aufnr,
*         lv_warpl LIKE mpos-warpl,
**         lv_overh LIKE mpos-overh,
*         lv_abnum LIKE mhis-abnum,
*         lv_nplda LIKE mhis-nplda,
*         lv_qmnum LIKE viqmel-qmnum.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*    EXPORTING
*      input  = p_aufnr
*    IMPORTING
*      output = p_aufnr.
*
**** Get plan number of O/H order
*  SELECT SINGLE  warpl abnum INTO (lv_warpl, lv_abnum)
*                 FROM afih
*                 WHERE aufnr = p_aufnr.
**** Get plan date of O/H order
*  SELECT SINGLE nplda INTO lv_nplda
*                FROM mhis
*                WHERE warpl = lv_warpl
*                AND   abnum = lv_abnum.
*
*  IF NOT lv_warpl IS INITIAL.
**    SELECT SINGLE overh  INTO lv_overh
**                  FROM mpos
**                  WHERE warpl = lv_warpl.
**
**    SELECT  SINGLE a~qmnum INTO lv_qmnum
**            FROM viqmel AS a
**                 INNER JOIN mhis AS b
**                 ON a~warpl = b~warpl
**                 AND a~abnum = b~abnum
**            WHERE a~warpl = lv_overh
**            AND   a~qmart = 'M7'
**            AND   b~nplda = lv_nplda.
**    IF sy-subrc EQ 0.
**      p_oiexist = '@01@'.
**      p_overh   = lv_overh.
**      p_abnum   = lv_abnum.
**      p_nplda   = lv_nplda.
**    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " CHECK_OVERHAUL_INSPC
*&---------------------------------------------------------------------*
*&      Form  CHECK_WORK_STAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ORDER_AUFNR  text
*      -->P_IT_ORDER_WSEXIT  text
*----------------------------------------------------------------------*
*FORM check_work_stand  USING    p_aufnr
*                                p_wsexist
*                                p_plnty
*                                p_plnnr
*                                p_plnal.
*
**  DATA : lv_aufnr LIKE afih-aufnr,
**         lv_warpl LIKE mpos-warpl.
***
**
**  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
**    EXPORTING
**      input  = p_aufnr
**    IMPORTING
**      output = p_aufnr.
**
**  SELECT SINGLE  warpl INTO lv_warpl
**                 FROM afih
**                 WHERE aufnr = p_aufnr.
**  IF NOT lv_warpl IS INITIAL.
**    SELECT SINGLE plnty_ws plnnr_ws plnal_ws
**                  INTO (p_plnty, p_plnnr, p_plnal)
**                  FROM mpos
**                  WHERE warpl = lv_warpl.
**    IF p_plnnr NE space.
**      p_wsexist = '@01@'.
**    ELSE.
**      CLEAR p_wsexist.
**    ENDIF.
**  ENDIF.
*ENDFORM.                    " CHECK_WORK_STAND
*&---------------------------------------------------------------------*
*&      Form  CALL_OVERHAUL_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD_TABINDEX  text
*      -->P_1685   text
*----------------------------------------------------------------------*
*FORM call_overhaul_list  USING    p_index.
*  DATA : l_tabix LIKE sy-tabix.
*
*  DATA : lv_aufnr LIKE afih-aufnr,
*         lv_warpl LIKE mpos-warpl,
**         lv_overh LIKE mpos-overh,
*         lv_abnum LIKE mhis-abnum,
*         lv_nplda LIKE mhis-nplda.
*  DATA : l_priokx LIKE t356_t-priokx.
*
*  DATA : l_pos TYPE i, l_tot TYPE i.
*
*  CLEAR : it_inspec, it_inspec[].
*
*  READ TABLE it_order INDEX p_index.
*  IF sy-subrc EQ 0 AND it_order-overh NE space.
*    MOVE-CORRESPONDING it_order TO zspm0006.
*    MOVE it_order-overh TO zspm0006-warpl.
*
*    PERFORM get_beber_text USING zspm0006-swerk
*                                 zspm0006-beber
*                                 zspm0006-beber_txt.
*
*    PERFORM get_stort_text USING zspm0006-swerk
*                                 zspm0006-stort
*                                 zspm0006-stort_txt.
*
*    PERFORM get_equnr_text USING zspm0006-equnr
*                                 zspm0006-equnr_txt.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = it_order-overh
*      IMPORTING
*        output = it_order-overh.
*
****  Overhaul Inspection Description
**    SELECT SINGLE wptxt INTO zspm0006-wptxt
**                   FROM mpla
**                   WHERE warpl = it_order-overh.
**
**    SELECT  *   INTO CORRESPONDING FIELDS OF TABLE it_inspec
**                FROM viqmel AS a
**                     INNER JOIN mhis AS b
**                     ON a~warpl  = b~warpl
**                     AND a~abnum = b~abnum
**                WHERE a~warpl = it_order-overh
**                AND   a~qmart = 'M7'
**                AND   b~nplda = it_order-nplda.
**    IF sy-subrc EQ 0.
**      LOOP AT it_inspec.
**        l_tabix = sy-tabix.
**
**        SELECT SINGLE *
**               INTO CORRESPONDING FIELDS OF it_inspec
**               FROM qmma
**               WHERE qmnum = it_inspec-qmnum.
**
**        SELECT SINGLE eqktx INTO it_inspec-eqkxt
**               FROM   eqkt
**               WHERE  equnr = it_inspec-equnr
**               AND    spras = sy-langu.
**
**        CLEAR: l_priokx.
**        SELECT SINGLE priokx INTO it_inspec-priokx
**               FROM   t356_t
**               WHERE  artpr = 'M7'
**               AND    priok = it_inspec-priok
**               AND    spras = sy-langu.
**
**        IF sy-subrc EQ 0.
**          SEARCH it_inspec-priokx FOR '-'.
**          l_pos = sy-fdpos.
**          it_inspec-priokx1 = it_inspec-priokx+0(l_pos).
**          l_tot = STRLEN( it_inspec-priokx ) - l_pos - 1.
**          l_pos = l_pos + 1.
**          it_inspec-priokx2 = it_inspec-priokx+l_pos(l_tot).
**        ENDIF.
**
**        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
**          EXPORTING
**            input  = it_inspec-equnr
**          IMPORTING
**            output = it_inspec-equnr.
**
**        MODIFY it_inspec INDEX l_tabix.
**      ENDLOOP.
**    ENDIF.
*  ELSE.
*    MESSAGE e000(zz) WITH text-m04 it_order-aufnr text-m10.
*  ENDIF.
*ENDFORM.                    " CALL_OVERHAUL_LIST
*&---------------------------------------------------------------------*
*&      Form  print_overhal_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1706   text
*----------------------------------------------------------------------*
*FORM print_overhal_list  USING    p_review.
*  DATA : st_options  TYPE ssfcompop.
*  DATA : st_device   TYPE ssfctrlop.
*  DATA : wa_formname TYPE tdsfname VALUE 'ZPMS0110'.
*  DATA : wa_funcname TYPE rs38l_fnam .
*
**** SAP Smart Forms: Form call
**** Get SmartForm's function name (like : /1BCDWB/SF00000036 )
**** Because funtion name is dependent on system client...
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname           = wa_formname
**     VARIANT            = ' '
**     DIRECT_CALL        = ' '
*    IMPORTING
*      fm_name            = wa_funcname
*    EXCEPTIONS
*      no_form            = 1
*      no_function_module = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  CHECK sy-subrc = 0.
*
*  IF p_review EQ 'X'.
*    st_device-preview    =  'X'.
*    st_device-no_dialog  =  'X'.
*    st_device-getotf     =  space.
*    st_device-langu      =  sy-langu.
**    ST_OPTIONS-TDDEST    =  'LOCL'.
*    st_options-tdimmed   =  'X'.
*    st_options-tddelete  =  'X'.
*    st_options-tdnewid   =  'X'.
*    st_options-tdnoprev  =  'X'.
**    st_options-tdcover   =  'S'.
*    st_options-tdcover   =  ''.
*  ELSE.
*    st_device-no_dialog  =  'X'.
*    st_options-tdnewid   = 'X'.
*    st_options-tdimmed   = 'X'.
*    st_options-tddelete  = 'X'.
*  ENDIF.
*
*  CALL FUNCTION wa_funcname
*    EXPORTING
*      control_parameters = st_device
*      output_options     = st_options
**     USER_SETTINGS      = 'X'
*      i_header           = zspm0006
*    TABLES
*      it_zspm0005        = it_inspec
*    EXCEPTIONS
*      formatting_error   = 1
*      internal_error     = 2
*      send_error         = 3
*      user_canceled      = 4
*      OTHERS             = 5.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " print_overhal_list
*&---------------------------------------------------------------------*
*&      Form  CALL_WORK_STANDARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RS_SELFIELD_TABINDEX  text
*----------------------------------------------------------------------*
*FORM call_work_standard  USING    p_index.
*  CLEAR : it_plpo, it_plpo[].
*  CLEAR :  st_plko.
*
*  DATA : BEGIN OF it_plas OCCURS 0.
*          INCLUDE STRUCTURE plas.
*  DATA : END OF it_plas.
*
*  READ TABLE it_order INDEX p_index.
*  IF sy-subrc EQ 0 AND it_order-plnty NE space AND
*                       it_order-plnnr NE space.
*
*    MOVE-CORRESPONDING it_order TO st_plko.
*
*    SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_plas
*           FROM plas
*           WHERE plnty = it_order-plnty
*           AND   plnnr = it_order-plnnr
*           AND   plnal = it_order-plnal.
*    IF sy-subrc EQ 0.
*      SELECT  * INTO CORRESPONDING FIELDS OF TABLE it_plpo
*                FROM plpo
*                FOR ALL ENTRIES IN it_plas
*                WHERE plnty = it_plas-plnty
*                AND   plnnr = it_plas-plnnr
*                AND   plnkn = it_plas-plnkn
*                AND   zaehl = it_plas-zaehl
*                AND   loekz = ' '.
*      IF sy-subrc EQ 0.
*
*      ELSE.
*
*      ENDIF.
*    ENDIF.
*  ELSE.
*    MESSAGE e000(zz) WITH text-m04 it_order-aufnr text-m09.
*  ENDIF.
*
*ENDFORM.                    " CALL_WORK_STANDARD
*&---------------------------------------------------------------------*
*&      Form  PRINT_WORK_STANDARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1731   text
*----------------------------------------------------------------------*
*FORM print_work_standard  USING  p_review.
*  DATA : st_options  TYPE ssfcompop.
*  DATA : st_device   TYPE ssfctrlop.
*  DATA : wa_formname TYPE tdsfname VALUE 'ZPMS0070'.
*  DATA : wa_funcname TYPE rs38l_fnam .
*
**** SAP Smart Forms: Form call
**** Get SmartForm's function name (like : /1BCDWB/SF00000036 )
**** Because funtion name is dependent on system client...
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      formname           = wa_formname
**     VARIANT            = ' '
**     DIRECT_CALL        = ' '
*    IMPORTING
*      fm_name            = wa_funcname
*    EXCEPTIONS
*      no_form            = 1
*      no_function_module = 2
*      OTHERS             = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*  CHECK sy-subrc = 0.
*
*  IF p_review EQ 'X'.
*    st_device-preview    =  'X'.
*    st_device-no_dialog  =  'X'.
*    st_device-getotf     =  space.
*    st_device-langu      =  sy-langu.
**    ST_OPTIONS-TDDEST    =  'LOCL'.
*    st_options-tdimmed   =  'X'.
*    st_options-tddelete  =  'X'.
*    st_options-tdnewid   =  'X'.
*    st_options-tdnoprev  =  'X'.
**    st_options-tdcover   =  'S'.
*    st_options-tdcover   =  ''.
*  ELSE.
*    st_device-no_dialog  = 'X'.
*    st_options-tdnewid   = 'X'.
*    st_options-tdimmed   = 'X'.
*    st_options-tddelete  = 'X'.
*  ENDIF.
*
*  CALL FUNCTION wa_funcname
*    EXPORTING
*      control_parameters = st_device
*      output_options     = st_options
*      i_plkod            = st_plko
*    TABLES
*      it_plpod           = it_plpo
*    EXCEPTIONS
*      formatting_error   = 1
*      internal_error     = 2
*      send_error         = 3
*      user_canceled      = 4
*      OTHERS             = 5.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ENDIF.
*
*ENDFORM.                    " PRINT_WORK_STANDARD
*&---------------------------------------------------------------------*
*&      Form  GET_BEBER_TEXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSPM0006_BEBER  text
*      -->P_ZSPM0006_BEBER_TXT  text
*----------------------------------------------------------------------*
form get_beber_text  using    p_swerk
                              p_beber
                              p_beber_txt.
  clear : p_beber_txt.
  select single fing into p_beber_txt
         from t357
         where werks = p_swerk
         and   beber = p_beber.
endform.                    " GET_BEBER_TEXT
*&---------------------------------------------------------------------*
*&      Form  get_equnr_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_INSPEC_EQUNR  text
*      -->P_IT_INSPEC_EQKXT  text
*----------------------------------------------------------------------*
form get_equnr_text  using    p_equnr
                              p_eqkxt.

  data : techobj like  ismpm_techobj.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = p_equnr
    importing
      output = p_equnr.

  call function 'I_SMPM_EQUIPMENT_GET'
    exporting
      equnr   = p_equnr
      langu   = sy-langu
    changing
      techobj = techobj.
*  IF sy-subrc EQ 0.
  p_eqkxt = techobj-eqktx.
*  ENDIF.
endform.                    " get_equnr_text

*&---------------------------------------------------------------------*
*&      Form  GET_PRINT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_print_info .

  data : lt_temp like it_order occurs 0 with header line.
  clear : it_print, it_print[],
          lt_temp,  lt_temp[].

  move : it_order[] to lt_temp[].
  sort lt_temp by aufnr.
  delete adjacent duplicates from lt_temp comparing aufnr.

  check not lt_temp[] is initial.

  loop at lt_temp.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = lt_temp-aufnr
      importing
        output = lt_temp-aufnr.
    modify lt_temp. clear lt_temp.
  endloop.

  select *
    into table it_print
    from ztpm0050
    for all entries in lt_temp
    where aufnr = lt_temp-aufnr.

endform.                    " GET_PRINT_INFO
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PRINT_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_print_info .

  data : l_datum like sy-datum,
         l_uzeit like sy-uzeit,
         l_uname like sy-uname.

  clear : it_print_save, it_print_save.
  clear : l_datum, l_uzeit, l_uname.

  move : sy-datum to l_datum,
         sy-uzeit to l_uzeit,
         sy-uname to l_uname.

  loop at it_print_info.
    move : it_print_info-aufnr to it_print_save-aufnr,
           'Y'                 to it_print_save-print_flag,
           l_datum             to it_print_save-print_dat,
           l_uzeit             to it_print_save-print_tim,
           l_uname             to it_print_save-print_usr.
    read table it_print with key aufnr = it_print_info-aufnr
                        binary search.
    if sy-subrc = 0.
      move : it_print-erdat      to it_print_save-erdat,
             it_print-erzet      to it_print_save-erzet,
             it_print-ernam      to it_print_save-ernam.

      move : l_datum             to it_print_save-aedat,
             l_uzeit             to it_print_save-aezet,
             l_uname             to it_print_save-aenam.
    else.
      move : l_datum             to it_print_save-erdat,
             l_uzeit             to it_print_save-erzet,
             l_uname             to it_print_save-ernam.
    endif.
    append it_print_save. clear it_print_save.
    clear : it_print_info.
  endloop.

  check not it_print_save[] is initial.
  modify ztpm0050 from table it_print_save.
  if sy-subrc = 0.
    commit work and wait.
  else.
    rollback work.
  endif.

endform.                    " UPDATE_PRINT_INFO
*&---------------------------------------------------------------------*
*&      Form  PRINT_CHECK_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILEP  text
*----------------------------------------------------------------------*
form print_check_list  using  p_filep.
  data: excelsheet type ole2_object,
    excel  type ole2_object,
    workbooks  type ole2_object,
    open_workbooks  type ole2_object,
    close_workbooks  type ole2_object,
    sheet  type ole2_object,
    cells  type ole2_object,
    subrc like sy-subrc,
    exl_activate type ole2_object,
    exl_print type ole2_object,
    exl_preview type ole2_object,
    exl_quit type ole2_object,
    exl_saveas type ole2_object,
    e_work type ole2_object.

  create object excel 'EXCEL.APPLICATION'.

  if sy-subrc ne 0.
    message i009 with
    'Error occurred while opening excel'.
  else.
    set property of excel 'DisplayAlerts' = 0.
    set property of  excel 'VISIBLE' = 0.
    call method of
        excel
        'WORKBOOKS' = workbooks.
    call method of
        workbooks
        'OPEN'    = open_workbooks
      exporting
        #1        = p_filep.

    if sy-subrc ne 0.
      message i009 with
      'Error occurred while opening excel'.
    endif.
  endif.
*    IF sheet-handle > 0.
*    FREE OBJECT sheet.
*    sheet-handle = -1.
*  ENDIF.
*
*  CALL METHOD OF
*      excel
*      'Worksheets' = sheet
*    EXPORTING
*      #1           = 1.
*
*  IF sy-subrc = 0.

*  CALL METHOD OF
*      sheet
*      'Activate'.
*
*  IF sheet-handle > 0.
*    FREE OBJECT sheet.
*    sheet-handle = -1.
*  ENDIF.

  get property of excel 'ACTIVEWORKBOOK' = e_work.
  call method of
      e_work
      'PRINTOUT'.
*    EXPORTING
*      #1         = 1.
  if sy-subrc = 0.
    message s000(zz) with 'Successfully Print out'.
  endif.
  free object e_work.
  free object excel.
*endif.
endform.                    " PRINT_CHECK_LIST
