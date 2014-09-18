* Notes
* 361358
* 458592
* 871926
*&--------------------------------------------------------------------*
*$*$----------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000751408                     $*
*$--------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46C          All Support Package Levels                   $*
*$*$----------------------------------------------------------------$*$*
*&--------------------------------------------------------------------*
*& Object          REPS MLCCS_KO88_TREATMENT
*& Object Header   PROG MLCCS_KO88_TREATMENT
*&--------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report  MLCCS_KO88_TREATMENT                                        *
*&                                                                     *
*&---------------------------------------------------------------------*
*SLP6BK201935 created for note 871926
*>>>> END OF INSERTION <<<<<<
...
*&--------------------------------------------------------------------*
*& REPORT MLCCS_KO88_TREATMENT
*&--------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT  MLCCS_KO88_TREATMENT                                        .
*Selection texts:
*   P_DISP = Display existing settings
*   P_DELE = Delete existing settings
*   P_WRITE = Write new settings

*   P_KSTAR = By cost element
*   P_SPRICE = Scale accord. to S-Price CCS
*   P_DELTA  = Scale delta according to S-Price CCS

*   P_KOKRS = Controlling area
*   P_BUKRS = Company code
*   P_GJAHR = Fiscal year
*   P_POPER = Posting period

*   R_AUFNR = Order number
*   R_KSTAR = Cost element

*Text elements:
*   001 = Run mode
*   002 = Special treatment of order balance
*   003 = Control data
*   004 = Objects
************************************************************************
tables: aufk, indx, mlauf, cska.
*-----------------------------------------------------------------------
type-pools: slis.
*-----------------------------------------------------------------------
types: begin of ys_kstar,
         aufnr          like aufk-aufnr,
         kstar          type kstar,
         bukrs          like t001-bukrs,
         treatment_type type c,
       end of ys_kstar,
       yt_kstar type table of ys_kstar,

      begin of s_order_type,
         aufnr like aufk-aufnr,
         bukrs          like t001-bukrs,
         treatment_type type c,
      end   of s_order_type,
      t_order_type type sorted table of s_order_type
                        with unique key aufnr, bukrs,

      s_cska_type like cska,
      t_cska_type type standard table of s_cska_type
                       with default key,

      begin of s_control_data_type,
         kokrs type kokrs,
         gjahr type gjahr,
         poper type poper,
      end   of s_control_data_type.
*-----------------------------------------------------------------------
selection-screen begin of block mode with frame title text-001.
parameters:
  p_disp  type c default 'X' radiobutton group 2 user-command upd,
  p_write type c             radiobutton group 2,
  p_dele  type c             radiobutton group 2.
selection-screen end   of block mode.

selection-screen skip.

selection-screen begin of block control with frame title text-003.
parameters:
  p_kokrs type kokrs memory id cac modif id wri,
  p_bukrs like t001-bukrs memory id buk modif id wri,
  p_gjahr type gjahr memory id gjr modif id wri,
  p_poper type poper memory id 74p modif id wri.
selection-screen end   of block control.

selection-screen skip.

selection-screen begin of block ttyp with frame title text-002.
parameters:
  p_sprice type c default 'X' radiobutton group 1 modif id wri
                  user-command set,
  p_delta  type c             radiobutton group 1 modif id wri,
  p_kstar type  c             radiobutton group 1 modif id wri.
selection-screen end   of block ttyp.

selection-screen skip.

selection-screen begin of block object with frame title text-004.
select-options:
  r_aufnr for aufk-aufnr           modif id wri,
  r_kstar for mlauf-kstar          modif id wri.
selection-screen end   of block object.
*-----------------------------------------------------------------------
constants:
  k_x(1)               type c                value 'X',
  gy_admin_identifier  like ckmlmvadmin-kkey value 'CK88_CCS001',
  gy_admin_identifier2 like ckmlmvadmin-kkey value 'CK88_CCS002',
  gy_take_kstar_hrkft  type c  value 'K',
  gy_take_sprice_delta type c  value 'D',
  gy_take_sprice_split type c  value 'S'.
*-----------------------------------------------------------------------
data: s_control_data type s_control_data_type.
*-----------------------------------------------------------------------

***************************
at selection-screen output.
***************************

*  perform selection_screen_modify.

*******************
start-of-selection.
*******************

*Write/ delete data.
*-------------------

  if not p_write is initial.
    perform write
               tables
                  r_aufnr
                  r_kstar
               using
                  p_kokrs
                  p_bukrs
                  p_poper
                  p_gjahr
                  p_sprice
                  p_delta
                  p_kstar.
  elseif not p_dele is initial.
    perform dele.
  endif.

*Display current data in any case.
*----------------------------------

  perform display.
************************************************************************
* Form write
************************************************************************
form write
*-----------------------------------------------------------------------
        tables
           irt_aufnr       structure r_aufnr
           irt_kstar       structure r_kstar
        using
           ip_kokrs        type kokrs
           ip_bukrs        like t001-bukrs
           ip_poper        type poper
           ip_gjahr        type gjahr
           ip_sprice       type c
           ip_delta        type c
           ip_kstar        type c.
*-----------------------------------------------------------------------
  data: l_error,
        ls_t001              like t001,
        ls_admin_data        like ckmlmvadmin-kdata,
        lt_order             type t_order_type,
        lt_order_new         type t_order_type,
        lt_order_mod         type t_order_type,
        lt_order_keep        type t_order_type,
        ls_control_data      type s_control_data_type.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  clear:   l_error,
           ls_t001,
           ls_admin_data.

  refresh: lt_order,
           lt_order_new,
           lt_order_mod,
           lt_order_keep.

  perform initializations
             using
                ip_kstar
                ip_kokrs
                ip_bukrs
                ip_gjahr
                ip_poper
             changing
                l_error
                ls_control_data
                ls_t001.

  check l_error is initial.

**************************************************
*Activate special treatment on order number level.
**************************************************

  perform write_orders
             tables
                irt_aufnr
             using
                ip_sprice
                ip_delta
             changing
                l_error
                lt_order
                lt_order_new
                lt_order_mod
                lt_order_keep.

  check l_error is initial.

**************************************************
*Activate special treatment on cost element level.
**************************************************

  if ip_sprice is initial and ip_delta is initial.
    perform write_cost_elements
               tables
                  irt_kstar
               using
                  ls_t001
                  lt_order_mod
                  lt_order_new
                  lt_order_keep.

* ansonsten: kstar-Eintrage zu Auftragen loschen!
  else.
    perform clear_cost_elements
               using
                  lt_order_mod.
  endif.

*************************************************
*Create entry in special treatment control table.
*************************************************

  ls_admin_data = ls_control_data.
  call function 'CKML_MGV_ADMIN_MODIFY'
    EXPORTING
      i_kkey  = gy_admin_identifier
      i_kdata = ls_admin_data.
*-----------------------------------------------------------------------
endform.                    " write
************************************************************************
*Form display
************************************************************************
form display.
*-----------------------------------------------------------------------
  data: l_repid         like sy-repid,
        ls_ckmlmvadmin  like ckmlmvadmin,
        ls_fieldcat     type slis_fieldcat_alv,
        ls_layout       type slis_layout_alv,
        lt_events       type slis_t_event,
        lt_fieldcat     type slis_t_fieldcat_alv,
        lt_kstar        type yt_kstar,
        lt_order        type t_order_type,
        lt_display      type yt_kstar.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  clear:   ls_fieldcat,
           ls_ckmlmvadmin,
           ls_layout.

  refresh: lt_events,
           lt_fieldcat,
           lt_kstar,
           lt_order,
           lt_display.

  l_repid = sy-repid.

****************************************************************
*Ceck whether special treatment settings exist for control data.
****************************************************************

  select single * from ckmlmvadmin into ls_ckmlmvadmin
                                   where kkey eq gy_admin_identifier.
  if sy-subrc eq 0.
    s_control_data = ls_ckmlmvadmin-kdata.
  endif.


********************************
*Get special treatment settings.
********************************

*Export/ Import table must always have the same name: 'lt_order' and
*'lt_kstar'.
  import lt_order from database indx(ml) id gy_admin_identifier.
  import lt_kstar from database indx(ml) id gy_admin_identifier2.

**************
*Build outtab.
**************

  sort lt_kstar by aufnr bukrs.

  perform build_outtab
             using
                lt_order
                lt_kstar
             changing
                lt_display.

*********************
*Build field catalog.
*********************

  perform build_field_catalog changing lt_fieldcat.

**************
*Build layout.
**************

  ls_layout-zebra = k_x.

*******************
*Build event table.
*******************

  perform build_event_list changing lt_events.

**********
*Call ALV.
**********

  call function 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fieldcat[]
      it_events          = lt_events
    TABLES
      t_outtab           = lt_display.
*-----------------------------------------------------------------------
endform.                    " display
************************************************************************
* FORM ALV_TOP_OF_LIST
************************************************************************
form alv_top_of_list.
*-----------------------------------------------------------------------
  write: / text-003,
         / text-005, s_control_data-kokrs,
         / text-006, p_bukrs,
         / text-007, s_control_data-poper,
         / text-008, s_control_data-gjahr.

  skip 2.

  write: / text-009.
  write: / text-010,
           text-011,
           text-012.
  write: / text-013.
  write: / text-014.
  skip.
*-----------------------------------------------------------------------
endform.                               " ALV_TOP_OF_LIST
************************************************************************
* Form  dele
************************************************************************
form dele.
*-----------------------------------------------------------------------
  data: lt_order type t_order_type,
        lt_kstar type yt_kstar.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  refresh: lt_order,
           lt_kstar.

************************
*Reset database content.
************************

*Export/ Import table must always have the same name: 'lt_order' and
*'lt_kstar'.
  export lt_order to database indx(ml) id gy_admin_identifier.
  export lt_kstar to database indx(ml) id gy_admin_identifier2.
*-----------------------------------------------------------------------
endform.                    " dele
************************************************************************
*Form write_orders
************************************************************************
form write_orders
*-----------------------------------------------------------------------
        tables
           irt_aufnr     structure r_aufnr
        using
           ip_sprice     type c
           ip_delta      type c
        changing
           c_error       type c
           ct_order      type t_order_type
           ct_order_new  type t_order_type
           ct_order_mod  type t_order_type
           ct_order_keep type t_order_type.
*-----------------------------------------------------------------------
  data: l_all_orders(1)    type c,
        l_tabix_new        like sy-tabix,
        l_empty_order_no   type aufnr,
        ls_order_old       type s_order_type,
        ls_order_new       type s_order_type,
        lt_order_old       type t_order_type,
        lt_order           type t_order_type.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  clear  : l_all_orders,
           ls_order_new,
           ls_order_old.

  refresh: ct_order,
           ct_order_new,
           ct_order_mod,
           ct_order_keep,
           lt_order_old,
           lt_order.

*****************************************************************
*Get order numbers for which special treatment is already active.
*****************************************************************

*Export/Import table must always have the same name: 'lt_order'.
  import lt_order from database indx(ml) id gy_admin_identifier.
  lt_order_old[] = lt_order[].

  loop at lt_order_old into ls_order_old
                       where aufnr eq l_empty_order_no.
    message i101(c+) with text-016 text-017 text-018.

    c_error = k_x.
    exit.
  endloop.
  check c_error is initial.

**************************************************************
*Get order numbers for first actiavation of special treatment.
**************************************************************
  if not r_aufnr[] is initial.

*  It is possible to create an entry in the selection table where aufnr
*  is empty.
    loop at r_aufnr where low = l_empty_order_no.
      l_all_orders = k_x.
      exit.
    endloop.
    if l_all_orders is initial.
      select aufnr bukrs from aufk
                   into corresponding fields of table ct_order_new
                   where aufnr in r_aufnr
                   and bukrs = p_bukrs
                   order by aufnr bukrs.
    endif.
  else.
    l_all_orders = k_x.
  endif.

*Note: l_all_orders = 'X' means that special treatment has to be activa-
*ted for all orders. In that case, a corresponding empty entry must be
*created in the special treatement order number control table.
  if l_all_orders = k_x.
    clear ls_order_new.
    insert ls_order_new into table ct_order_new.
  endif.

********************************************************************
*Set overall special treatement type for the selected order numbers.
********************************************************************

  clear ls_order_new.
  if not ip_sprice is initial.
    ls_order_new-treatment_type = gy_take_sprice_split.
  elseif not ip_delta is initial.
    ls_order_new-treatment_type = gy_take_sprice_delta.
  else.
    ls_order_new-treatment_type = gy_take_kstar_hrkft.
  endif.

  modify ct_order_new from ls_order_new
                           transporting treatment_type
                           where treatment_type eq space.

**************
*Group orders.
**************

*There are three groups of orders.

*1- Orders for which old settings exist but that are not among the
*   orders specified by the selection criteria. (ct_order_keep)
*2- Orders for which old settings exist and that are explicitely among
*   the orders specified by the selection criteria. (ct_order_mod)
*3- Orders for which special treatment is activated for the first time.
*   (ct_order_new)

*Furthermore a complete list of orders for which special treatment set-
*tings exist is written to table ct_order.

  if l_all_orders = k_x.
*  Forget about old settings.
    refresh: ct_order_keep,
             ct_order_mod.
    ct_order[] = ct_order_new[].
  else.
*   Find orders that so far figure both in ct_orders_old and ct_orders_
*   new.
    loop at lt_order_old into ls_order_old.
      read table ct_order_new with key aufnr = ls_order_old-aufnr
                                       bukrs = ls_order_old-bukrs
                        into ls_order_new.
      if sy-subrc eq 0.
        l_tabix_new = sy-tabix.
        insert ls_order_new into table ct_order_mod.
        insert ls_order_new into table ct_order.
*         Delet order from list of new orders.
        delete ct_order_new index l_tabix_new.
      else.
*         Special treatment settings are not to be changed.
        append ls_order_old to ct_order_keep.
        append ls_order_old to ct_order.
      endif.
    endloop.
*  Add newly selected orders to ct_order.
    loop at ct_order_new into ls_order_new.
      insert ls_order_new into table ct_order.
    endloop.
  endif.

*****************************
*Export order list to memory.
*****************************

*Export/Import table must always have the same name: 'lt_order'.
  refresh lt_order.
  lt_order[] = ct_order[].
  export lt_order to database indx(ml) id gy_admin_identifier.
*-----------------------------------------------------------------------
endform.                    " write_orders
************************************************************************
* Form write_cost_elements
************************************************************************
form write_cost_elements
*-----------------------------------------------------------------------
        tables
           irt_kstar     structure r_kstar
        using
           is_t001       like t001
           it_order_mod  type t_order_type
           it_order_new  type t_order_type
           it_order_keep type t_order_type.
*-----------------------------------------------------------------------
  data: ls_cska            type s_cska_type,
        ls_order           type s_order_type,
        ls_kstar           type ys_kstar,
        lt_kstar           type yt_kstar,
        lt_kstar_new       type yt_kstar,
        lt_kstar_old       type yt_kstar,
        lt_cska            type t_cska_type.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  clear:   ls_cska,
           ls_order,
           ls_kstar.

  refresh: lt_cska,
           lt_kstar,
           lt_kstar_new,
           lt_kstar_old.

****************************
*Get possible cost elements.
****************************

  select * from cska into table lt_cska where ktopl eq is_t001-ktopl and
                                              kstar in r_kstar.

****************************************************
*Maintain cost element settings for modified orders.
****************************************************

  loop at it_order_mod into ls_order.
    loop at lt_cska into ls_cska.
      clear ls_kstar.
      ls_kstar-aufnr          = ls_order-aufnr.
      ls_kstar-kstar          = ls_cska-kstar.
      ls_kstar-treatment_type = gy_take_kstar_hrkft.
      append ls_kstar to lt_kstar_new.
    endloop.
  endloop.

***********************************************
*Maintain cost element settings for new orders.
***********************************************

  loop at it_order_new into ls_order.
    loop at lt_cska into ls_cska.
      clear ls_kstar.
      ls_kstar-aufnr          = ls_order-aufnr.
      ls_kstar-kstar          = ls_cska-kstar.
      ls_kstar-treatment_type = gy_take_kstar_hrkft.
      append ls_kstar to lt_kstar_new.
    endloop.
  endloop.

*****************************************************
*Maintain cost element settings for unchanged orders.
*****************************************************

*Export/ Imort table must always have the same name 'lt_kstar'.
  import lt_kstar from database indx(ml) id gy_admin_identifier2.
  lt_kstar_old[] = lt_kstar[].

  loop at it_order_keep into ls_order.
    loop at lt_kstar_old into ls_kstar where aufnr = ls_order-aufnr.
      append ls_kstar to lt_kstar_new.
    endloop.
  endloop.

******************************************************
*Export cost element special treatment settings to db.
******************************************************

*Export/ Imort table must always have the same name 'lt_kstar'.
  refresh: lt_kstar.
  lt_kstar[] = lt_kstar_new[].
  export lt_kstar to database indx(ml) id gy_admin_identifier2.
*-----------------------------------------------------------------------
endform.                    " write_cost_elements
************************************************************************
* Form clear_cost_elements
************************************************************************
form clear_cost_elements
*-----------------------------------------------------------------------
        using
           it_order_mod type t_order_type.
*-----------------------------------------------------------------------
  data: ls_order          type s_order_type,
        lt_kstar          type yt_kstar.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  refresh: lt_kstar.

*******************************
*Get old cost element settings.
*******************************

*Export/ Import table must always have the same name: 'lt_kstar'.
  import lt_kstar from database indx(ml) id gy_admin_identifier2.

******************************
*Update cost element settings.
******************************

  loop at it_order_mod into ls_order.
    delete lt_kstar where aufnr = ls_order-aufnr.
  endloop.
**********************************
*Export new cost element settings.
**********************************

*Export/ Import table must always have the same name: 'lt_kstar'.
  export lt_kstar to database indx(ml) id gy_admin_identifier2.
*-----------------------------------------------------------------------
endform.                    " clear_cost_elements
************************************************************************
* Form  build_field_catalog
************************************************************************
form build_field_catalog
*-----------------------------------------------------------------------
        changing
           ct_fieldcat type slis_t_fieldcat_alv.
*-----------------------------------------------------------------------
  data: ls_fieldcat      type slis_fieldcat_alv.
*-----------------------------------------------------------------------
  clear ls_fieldcat.
  ls_fieldcat-col_pos       = 1.
  ls_fieldcat-fieldname     = 'TREATMENT_TYPE'.
  ls_fieldcat-seltext_l     = 'TREATMENT_TYPE'.
  append ls_fieldcat to ct_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-col_pos       = 2.
  ls_fieldcat-fieldname     = 'AUFNR'.
  ls_fieldcat-ref_fieldname = 'AUFNR'.
  ls_fieldcat-ref_tabname   = 'AUFK'.
  ls_fieldcat-key           = 'X'.
  append ls_fieldcat to ct_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-col_pos       = 3.
  ls_fieldcat-fieldname     = 'BUKRS'.
  ls_fieldcat-ref_fieldname = 'BUKRS'.
  ls_fieldcat-ref_tabname   = 'AUFK'.
  ls_fieldcat-key           = 'X'.
  append ls_fieldcat to ct_fieldcat.

  clear ls_fieldcat.
  ls_fieldcat-col_pos       = 4.
  ls_fieldcat-fieldname     = 'KSTAR'.
  ls_fieldcat-ref_fieldname = 'KSTAR'.
  ls_fieldcat-ref_tabname   = 'MLAUF'.
  ls_fieldcat-key           = 'X'.
  append ls_fieldcat to ct_fieldcat.
*-----------------------------------------------------------------------
endform.                    " build_field_catalog
************************************************************************
* Form  build_event_list
************************************************************************
form build_event_list
*-----------------------------------------------------------------------
        changing
           ct_events type slis_t_event.
*-----------------------------------------------------------------------
  data: ls_events      type slis_alv_event.
*-----------------------------------------------------------------------
  clear ls_events.
  ls_events-name = 'TOP_OF_LIST'.
  ls_events-form = 'ALV_TOP_OF_LIST'.
  append ls_events to ct_events.
*-----------------------------------------------------------------------
endform.                    " build_event_list
************************************************************************
* Form  build_outtab
************************************************************************
form build_outtab
*-----------------------------------------------------------------------
        using
           it_order        type t_order_type
           it_kstar_sorted type yt_kstar
        changing
           ct_display      type yt_kstar.
*-----------------------------------------------------------------------
  data: ls_order             type s_order_type,
        ls_kstar             type ys_kstar,
        ls_display           type ys_kstar.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  clear: ls_order,
         ls_kstar,
         ls_display.

***************************************
*Merge cost element and order settings.
***************************************

  loop at it_order into ls_order.
    clear ls_display.
    ls_display-aufnr          = ls_order-aufnr.
    ls_display-bukrs          = ls_order-bukrs.
    ls_display-treatment_type = ls_order-treatment_type.

    if ls_order-treatment_type ne gy_take_kstar_hrkft.
      append ls_display to ct_display.
    else.
      perform complete_cost_element_data
                 using
                    ls_display
                    it_kstar_sorted
                 changing
                    ct_display.
    endif.
  endloop.

  sort ct_display by treatment_type aufnr bukrs.

*-----------------------------------------------------------------------
endform.                    " build_outtab
************************************************************************
* Form  complete_cost_element_data
************************************************************************
form complete_cost_element_data
*-----------------------------------------------------------------------
        using
           is_display      type ys_kstar
           it_kstar_sorted type yt_kstar
        changing
           ct_display      type yt_kstar.
*-----------------------------------------------------------------------
  data: l_starting_index     like sy-tabix,
        ls_display           type ys_kstar,
        ls_kstar             type ys_kstar.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  clear: l_starting_index,
         ls_kstar.

*Make local copy of is_display.
  ls_display = is_display.

**************************************
*Find cost element settings for order.
**************************************

  read table it_kstar_sorted with key aufnr = ls_display-aufnr
                                      bukrs = ls_display-bukrs
                             binary search
                             transporting no fields.

  if sy-subrc ne 0.
    append ls_display to ct_display.
    exit.
  else.
    l_starting_index = sy-tabix.
  endif.

  loop at it_kstar_sorted from l_starting_index into ls_kstar.
    if ls_kstar-aufnr ne ls_display-aufnr.
      exit.
    endif.

    ls_display-kstar = ls_kstar-kstar.
    append ls_display to ct_display.
  endloop.
*-----------------------------------------------------------------------
endform.                    " complete_cost_element_data
************************************************************************
* Form  selection_screen_modify
************************************************************************
*form selection_screen_modify.
**----------------------------------------------------------------------
*  loop at screen.
*    case screen-group1.
*      when 'WRI'.
*        if not p_write is initial.
*          if screen-name eq '%_P_BUKRS_%_APP_%-TEXT'      or
*             screen-name eq 'P_BUKRS'                     or
*             screen-name eq '%_R_KSTAR_%_APP_%-TEXT'      or
*             screen-name eq '%_R_KSTAR_%_APP_%-OPTI_PUSH' or
*             screen-name eq 'R_KSTAR-LOW'                 or
*             screen-name eq '%_R_KSTAR_%_APP_%-TO_TEXT'   or
*             screen-name eq 'R_KSTAR-HIGH'                or
*             screen-name eq '%_R_KSTAR_%_APP_%-VALU_PUSH' .
**                 Company code and cost element selection only in case
**                 of settlement by cost element.
*            if not p_kstar is initial.
*              screen-active    = '1'.
*              screen-invisible = '0'.
*            else.
*              screen-active    = '0'.
*              screen-invisible = '1'.
*            endif.
*          else.
*            screen-active    = '1'.
*            screen-invisible = '0'.
*          endif.
*        else.
*          screen-active    = '0'.
*          screen-invisible = '1'.
*        endif.
*    endcase.
*    modify screen.
*  endloop.
**----------------------------------------------------------------------
*endform.                    " selection_screen_modify
************************************************************************
* Form initializations
************************************************************************
form initializations
*-----------------------------------------------------------------------
        using
           ip_kstar        type c
           ip_kokrs        type kokrs
           ip_bukrs        like t001-bukrs
           ip_gjahr        type gjahr
           ip_poper        type poper
        changing
           c_error         type c
           cs_control_data type s_control_data_type
           cs_t001         like t001.
*-----------------------------------------------------------------------

*****************
*Initializations.
*****************

  clear: cs_control_data,
         cs_t001.

*********************************
*Set administration control data.
*********************************

  cs_control_data-kokrs = ip_kokrs.
  cs_control_data-gjahr = ip_gjahr.
  cs_control_data-poper = ip_poper.

************************************************************************
*Get company code data if settlement by cost element is to be activated.
************************************************************************

  if not ip_kstar is initial.
    if ip_bukrs is initial.
      message i101(c+) with text-015 '' '' ''.
      c_error = k_x.
      exit.
    else.
      call function 'T001_READ'
        EXPORTING
          bukrs  = ip_bukrs
        IMPORTING
          struct = cs_t001
        EXCEPTIONS
          others = 1.
      if sy-subrc ne 0.
        c_error = k_x.
      endif.
    endif.
  endif.
*-----------------------------------------------------------------------
endform.                    " intializations
*>>>> END OF INSERTION <<<<<<
