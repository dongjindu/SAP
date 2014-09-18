************************************************************************
* Program Name      : ZRQM06R_Q_SCORE_VENDOR_V2
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.23.
* Specifications By : SeungLyong, Lee
* Pattern           : Report 1.2 - Call Screen
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Quality Score report by Vendor
*
* Modification Logs
* Date         Developer    RequestNo     Description
* 09/28/2004     Shiva      UD1K912330   Changed the program as
*                                        per portal design requirement.
*
************************************************************************

REPORT  ZRQM06R_Q_SCORE_VENDOR_V3  NO STANDARD PAGE HEADING   .

*&&& data declaration.  &&&*
*type-pools vrm.     "//value request manager: types & constants
*type-pools cxtab .  "//table_control object type pool
*tables : feld.      "//screen object structure

*-- include program ( include constants or etc)
INCLUDE <icon>.

*-- sap scripts object interface
*tables : thead. "/sapscript: text header

*//tables;(tables : table_name /view "//table description)
tables : ztqm_q_score, "/quality score table - aqm05
         ZTQM_VENDOR_ID,
         mara,         "/material master
         marc,
         lfa1,         "/vendor master
         mseg,         "/document segment: material
         qmel,         "/quality notification
         mkpf.         "/header: material document

*//structures declaration(tables : structure name."/description)
*data: wa_qs_port1 like zsqm_qs_port1.

data: begin of wa_qs_port1 occurs 0,
      ebeln like mseg-ebeln,
      ebelp like mseg-ebelp,
      zbudat like mseg-zbudat.
      include structure zsqm_qs_port1.
data: end of wa_qs_port1.

data: begin of wa_qscore.
        include structure ztqm_q_score.
data:   werks like marc-werks,
        extwg like mara-extwg,
      end of wa_qscore.
*//infotype;()
*//cluster or import parameter;(parameter name)

*//controls(for only screen control element);(tc_ , or ts_)
*-- table control
*controls: tc_9000  type tableview using screen 9000.

*//type (table structure);(ty_ )- table or structure

*-- pf-status : excluding function code table
types: begin of ty_fcode,
        fcode like rsmpe-func,
      end of ty_fcode.

data: it_ex_func type standard table of ty_fcode with
                       non-unique default key initial size 5,
      wa_ex_func type ty_fcode.


*//constants ;(c_) ==> true:'x' or '1' false:space or '0'
constants : c_mark   value 'X'.

**//-- global : used variable just in this program
*-- function control
data : ok_code like sy-ucomm,
       w_name1 like lfa1-name1,
       w_perlow(10) type c,
       w_perhgh(10) type c,
       w_matlow like mara-matnr,
       w_mathgh like mara-matnr.
data : wa_mode(7) type c,
       wa_status(8) type c.
*--

*-- user confirm for pop-up message
data : wa_answer type c.
data : wa_repid like sy-repid.

*-- work area variables in program.(wa_xxxx)
data : wa_first_date like sy-datum,
       wa_last_date  like sy-datum.

data : wa_qmart  type qmart value 'Q2'. "/noti type for defect data

*-- data level control
data :  wa_renewal_flg.

data : wa_int_date type dats. "/date for interface
*DATA FOR VENDOR ID
DATA: WA_VENDOR LIKE ZTQM_VENDOR_ID-LIFNR.
*//data(work area or (internal) structures);(wa_ )(st_)?
*-- screnn field cursor control
data : wa_fldtxt    like feld-name,  "field name variable
       wa_cur_line  like feld-line.  "field line variable

*//internal tables and index fields;(it_), (i_)
*data : it
*/-- internale tables with structure as sama as db
*- quality score by vendor for material document data :collective data
data : it_qs_result like table of wa_qs_port1.

*- quality score by vendor for defect data
* data : it_def like table of wa_qs_port1.
data: it_def like wa_qs_port1 occurs 0 with header line.
*- internal table for collect data of gr/gi
data: begin of wa_it occurs 0,
      matnr like mara-matnr,
      lifnum like qmel-lifnum,
      wa_num type i,
      end of wa_it.
data: wa_it1 like wa_it occurs 0 with header line.
*data : it_grgi  like table of wa_qs_port1."100565 08/31/06
data: begin of IT_GRGI occurs 0,
      ebeln like mseg-ebeln,
      ebelp like mseg-ebelp,
      zbudat like mseg-zbudat.
      include structure zsqm_qs_port1.
data: end of IT_GRGI.

*- internal table for collect data of quality score from  ztqm_q_score
data : it_qscore like table of wa_qscore.

*//ranges; (r_)
*ranges :

*//field symbols; <fs_>
*-- table controls variable(field-symbols)
*field-symbols: <tc>  type cxtab_control. "table control
*"                              table_control object(cxtab)

*//field group;

* control framework basic class
class cl_gui_cfw      definition load.

*// declare reference variables, the container and internal table
data: wa_custom_control    type   scrfname value 'ALV_CONTAINER',
      alv_grid          type ref to cl_gui_alv_grid,
      grid_container    type ref to cl_gui_custom_container.

* predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
class : lcl_event_receiver definition deferred.

data : event_receiver type ref to lcl_event_receiver.

* global variables for attributes or etc of alv grid
data : wa_is_layout type lvc_s_layo. "/the layout structure
data : it_fieldcat type lvc_t_fcat with header line,
       it_sort     type lvc_t_sort with header line.

data: wa_save    type c   value 'A',   "for parameter i_save
*/-   saving options for layouts
*space- layouts cannot be saved.
*'u'  - only user-defined layouts can be saved.
*'x'  - only global layouts can be saved.
*'a'  - both user-defined and global layouts can be saved

      wa_variant type disvariant.      "for parameter is_variant

***//& selection screen definition(parameters select-option)
*-- paramerters : (p_), select-options : (s_)
selection-screen begin of block blk with frame  title text-t01.
parameters: p_lifnr like lfa1-lifnr obligatory modif id VEN .
*- period
select-options : s_period  for ztqm_q_score-issuedat obligatory
                            default wa_first_date to wa_last_date
                                      no-extension .
select-options: s_matnr for mara-matnr .
 parameters:               p_werks like marc-werks.

selection-screen end of block blk .

*-- seclection screen flow logic control event handling
*at selection-screen on ( on end of, on value-request for,
* on help-request for, on radiobutton group, on block output,
* on exit-command )
at selection-screen output.
  set titlebar '1000'.
  select single lifnr from ztqm_vendor_id
                      into p_lifnr
                      where uname = sy-uname.
  if sy-subrc ne 0.
  else.
    loop at screen.
      if screen-group1 = 'VEN'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
  endif.

at selection-screen on block blk .


  check sy-ucomm = 'ONLI'.
*-- get quality score data from db.
  perform get_data_from_db.

  if it_grgi[] is initial.
    message e000(zmqm) with 'No entries!'(E01).
    exit.
  endif.

  select single name1 from lfa1
                      into w_name1
                      where lifnr = p_lifnr.

*-- get text of ext. material group from table twewt
*  perform get_ext_mat_group_text  using s_extwg-low
*                                        wa_ewbez.

start-of-selection.
  check not it_grgi[] is initial or
        not it_def[] is initial or
        not it_qscore[] is initial.
*-- collect data by vendor and matnerial.
  perform collect_data_by.
  write : s_period-low to w_perlow,
          s_period-high to w_perhgh.
  clear wa_qs_port1.
  if not s_matnr is initial.
    w_matlow = s_matnr-low.
    w_mathgh = s_matnr-high.
  endif.
*-- collect data by vendor for basic list display
*  perform collect_dat_by_vendor_basic.

*-- calculate quality score
  perform calculate_qual_score tables it_qs_result.

**-- end of selection.
end-of-selection.
  check not it_grgi[] is initial.
*-- fill text (vendor name).
*  perform fill_text.

  call screen 9000.

*// event handling(except selection screen (flow)event)
load-of-program.

  PERFORM GET_DEFAULT_VENDOR.
*-- get selection data for selection from memory id using import
*  import s_extwg-low  from memory id 'by_vendor_ext'.

*  move :  s_extwg-low   to wa_extwg.

  import wa_int_date  from memory id 'BY_VENDOR'.

  if sy-subrc = 0.  "/import data exist.
    free memory id 'BY_VENDOR'. free memory id 'BY_VENDOR_EXT'.

    wa_first_date = wa_int_date.

    call function 'RP_LAST_DAY_OF_MONTHS'
         EXPORTING
              day_in            = wa_first_date
         IMPORTING
              last_day_of_month = wa_last_date
         EXCEPTIONS
              day_in_no_date    = 1
              others            = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

  else.
    wa_first_date = sy-datum.
    wa_first_date+6(2) = '01'.
    wa_last_date = sy-datum.
  endif.

initialization.


***//macro definitions
*-- macro : macro_name &1 &2
*--           &1 -
*--           &2 -
*  define macro_name.
*  end-of-definition.


****************************************************************
* local classes: definition
****************************************************************
* class lcl_event_receiver: local class to handle events double click,
* toolbar, user command
class lcl_event_receiver definition.

  public section.

    methods:

    handle_double_click
        for event double_click of cl_gui_alv_grid
            importing e_row e_column,

    handle_toolbar
        for event toolbar of cl_gui_alv_grid
            importing e_object e_interactive,

    handle_user_command
        for event user_command of cl_gui_alv_grid
            importing e_ucomm.

  private section.

endclass.

* lcl_event_receiver (definition)
****************************************************************
* local classes: implementation
****************************************************************
* class lcl_event_receiver (implementation)
class lcl_event_receiver implementation.

*-- / double click
  method handle_double_click.

* the event double_click provides parameters for row and column
*   of the click. use row parameter to select a line of the
*   corresponding internal table.
* : e_row-index.

  endmethod.                           "handle_double_click

*-- / handling tollbar control
  method handle_toolbar.

*this event is triggered by the alv each time the toolbar of the control
* needs to be regenerated. to add self-defined functions to the
*toolbar, you trigger the event using method set_toolbar_interactive and
* write an event handler method

    data: ls_toolbar  type stb_button.

*    case wa_level.
*      when c_vendor.
**         append a separator('3') to normal toolbar
*        clear ls_toolbar.
*        move 3 to ls_toolbar-butn_type.
*        append ls_toolbar to e_object->mt_toolbar.
**         append an icon to show detail list of selected item.
*        clear ls_toolbar.
*        move 0 to ls_toolbar-butn_type. "/ button type
*        move 'detail'           to ls_toolbar-function.
*        move icon_detail        to ls_toolbar-icon.
*        move 'show detail'(t12) to ls_toolbar-quickinfo.
*        move 'detail'(t13)      to ls_toolbar-text.
*        move ' '                to ls_toolbar-disabled.
*
*        append ls_toolbar to e_object->mt_toolbar.
*      when others.
*    endcase.
  endmethod.
*-------------------------------------------------------------------

*-- / handling user defined commands for toolbar
  method handle_user_command.

*   in event handler method for event user_command: query your
*   function codes defined in class definition and react accordingly.

    data : lt_rows   type lvc_t_row,
           lw_line_row like line of lt_rows.
    data : lw_lines type i.

    case e_ucomm.
      when 'DETAIL'.

        call method alv_grid->get_selected_rows
                 importing et_index_rows = lt_rows.

        call method cl_gui_cfw=>flush.

        if sy-subrc ne 0.
          wa_repid = sy-repid.
          call function 'POPUP_TO_INFORM'
               EXPORTING
                    titel = wa_repid
                    txt2  = sy-subrc
                    txt1  = text-e01.
        else.
          describe table lt_rows lines lw_lines.
          check lw_lines = 1.     "/check single line selected

          read table lt_rows  index 1 into lw_line_row.

          check not lw_line_row-index is initial.

          perform retriev_detail_data using lw_line_row-index.


          wa_renewal_flg = c_mark.

        endif.

      when others.

    endcase.

  endmethod.                           "handle_user_command

endclass.
*
* lcl_event_receiver (implementation)
*===================================================================

**<<<<<<<<< program main / subroutine / flow logic >>>>>>>>>>>>**
*&-----------------------------------------------------------------*
*&      form  get_data_from_db
*&-----------------------------------------------------------------*
form get_data_from_db.

  data : lt_grgi like table of wa_qs_port1.
  field-symbols: <fs_grgi> like line of lt_grgi.

  refresh: it_grgi,it_def,it_qscore.

*-- get gr/gi data from db
*-- there is no material supplied by multi vendor. "/notice
*// modified by sllee : change sql logic by mr kim 02/13/2004-start
*-   using purchasing source list(table : 'eord')

*  select   b~lifnr g~name1 b~bwart b~meins   b~matnr f~maktx
*          sum( b~menge ) as menge_gr
*       into corresponding fields of table it_zsqm_q_grgi
*         from ( ( ( ( mkpf as a        inner join mseg as b
*            on   a~mblnr = b~mblnr
*             and a~mjahr = b~mjahr ) inner join mara as d
*            on   b~matnr = d~matnr ) inner join marc as e
*            on   d~matnr = e~matnr
*             and b~werks = e~werks ) inner join makt as f
*            on b~matnr = f~matnr   ) left outer join lfa1 as g
*            on b~lifnr = g~lifnr
*           where a~budat in s_period
*             and d~extwg in s_extwg
*             and e~qmatv = c_mark
*             and b~bwart in ('101', '102',            "/gr
*                              '261', '901', '903',    "/gi
*                              '262', '902', '904')
*             and f~spras = sy-langu
*            group by b~lifnr g~name1 b~bwart b~meins b~matnr f~maktx.

  select   g~lifnr h~name1 b~bwart b~meins   b~matnr
  B~mblnr B~EBELN B~EBELP B~ZBUDAT "100565 08/31/06
           e~werks d~extwg f~maktx
          sum( b~menge ) as menge_gr
       into corresponding fields of table it_grgi
         from ( ( ( ( ( mkpf as a        inner join mseg as b
            on   a~mblnr = b~mblnr
             and a~mjahr = b~mjahr ) inner join mara as d
            on   b~matnr = d~matnr ) inner join marc as e
            on   d~matnr = e~matnr
*             and b~werks = e~werks
                                   ) inner join makt as f
            on   b~matnr = f~matnr ) inner join eord as g
            on   e~matnr = g~matnr
             and e~werks = g~werks ) inner join lfa1 as h
            on   g~lifnr = h~lifnr
           where b~zbudat in s_period   "/a~budat in s_period
             and e~qmatv = c_mark
             and b~bwart in ('101', '102',            "/gr
                              '261', '901', '903',    "/gi
                              '262', '902', '904')
             and f~spras = sy-langu
             and g~lifnr = p_lifnr
             and g~matnr in s_matnr
*             and g~zeord = '00001'
            group by g~lifnr h~name1 b~bwart b~meins b~matnr
                     e~werks d~extwg f~maktx B~mblnr B~EBELN B~EBELP
B~ZBUDAT.

*// modified by sllee : change sql logic by mr kim 02/13/2004-end

  check sy-subrc = 0 and not it_grgi[] is initial.
  delete it_grgi where extwg is initial.
  lt_grgi[] = it_grgi[].

  refresh it_grgi.
  clear wa_qs_port1.

  loop at lt_grgi assigning <fs_grgi>.
    move-corresponding <fs_grgi> to wa_qs_port1.
    case <fs_grgi>-bwart.
      when '101'.
*       n/a
      when '102'.
        wa_qs_port1-menge_gr = wa_qs_port1-menge_gr * -1.
      when '261' or '901' or '903'.
        wa_qs_port1-menge_gi = wa_qs_port1-menge_gr.
        clear wa_qs_port1-menge_gr.
      when '262' or '902' or '904'.
        wa_qs_port1-menge_gi = wa_qs_port1-menge_gr.
        wa_qs_port1-menge_gi = wa_qs_port1-menge_gi * -1.
        clear wa_qs_port1-menge_gr.
    endcase.
    clear wa_qs_port1-bwart.
    collect wa_qs_port1 into it_grgi.
  endloop.

*-  get defect data from qmel

select distinct a~lifnum as lifnr e~name1 b~meins b~extwg "c~werks
a~matnr d~maktx
             sum( a~rkmng ) as rkmng
             sum( A~lndwntime ) as lndwntime " 100565 08/31/06
       sum( A~responsive ) as responsive "100565 08/31/06
             into corresponding fields of table it_def
             from  ( ( (  qmel as a inner join mara as b
                on   a~matnr = b~matnr   ) inner join marc as c
                on   b~matnr = c~matnr   ) inner join makt as d
                on   a~matnr = d~matnr   ) inner join lfa1 as e
                on   a~lifnum = e~lifnr
               where
*               c~qmatv = c_mark and
                  a~erdat  in s_period
                 and a~qmart  = wa_qmart    "/'Q2' - fixed
                 and a~matnr in s_matnr
                 and a~lifnum  = p_lifnr
                 and d~spras  = sy-langu
                 AND c~werks = p_werks
                 and a~objnr not in ( select objnr from jest as f
                  where     f~objnr eq a~objnr and
                             f~stat = 'I0076' and Inact = ' '   )

      group by a~lifnum e~name1 b~meins b~extwg "c~werks
      a~matnr d~maktx
      a~lndwntime a~responsive
       HAVING   lndwntime >= 0                               " rl 07_19
   AND    responsive >= 0.

  delete it_def where extwg is initial.

* addition made by 100565 to copy ZQMR22 tp ZQMP22
****Number of Notif with same matnr
select * from qmel into corresponding fields of table wa_it
for all entries in it_def
where matnr = it_def-matnr
and lifnum = it_def-lifnr
and erdat in s_period.

*loop at wa_it.
*add 1 to wa_it-wa_num.
*modify wa_it.
*endloop.
*
*loop at wa_it.
*collect wa_it into wa_it1.
*endloop.

loop at wa_it.
add 1 to wa_it-wa_num.
collect wa_it into wa_it1.
endloop.


loop at it_def.
*** to default responsive
if it_def-lndwntime is initial .
it_def-lndwntime = 100.
endif.

if it_def-lndwntime = 0 .
it_def-lndwntime = 100.
endif.

if it_def-lndwntime <> 100.
it_def-lndwntime = 100 - it_def-lndwntime.
endif.
if it_def-responsive is initial.
it_def-responsive = 100.
endif.
*** to default responsive
read table wa_it1 with key matnr = it_def-matnr
                           lifnum = it_def-lifnr.
it_def-responsive = it_def-responsive / wa_it1-wa_num.
modify it_def.
endloop.
***End select
* End addition


*-- get quality score data from cbo table

 select a~lifnr g~name1 a~meinh a~meins a~matnr b~extwg e~werks f~maktx
            sum( a~linestop ) as linestop
            sum( a~qnt_camp ) as qnt_camp
            sum( a~qnt_salv ) as qnt_salv
            sum( a~qnt_repr ) as qnt_repr
        into corresponding fields of table it_qscore
          from ( ( ( ztqm_q_score as a inner join mara as b
             on  a~matnr = b~matnr ) inner join marc as e
             on  b~matnr = e~matnr ) inner join makt as f
             on  a~matnr = f~matnr ) inner join lfa1 as g
             on  a~lifnr = g~lifnr
            where a~issuedat in s_period
              and a~matnr    in s_matnr
              and a~lifnr = p_lifnr
              and f~spras   = sy-langu
             group by a~lifnr  g~name1 a~meinh  a~meins a~matnr
                      b~extwg e~werks f~maktx.
  delete it_qscore where extwg is initial.
  free lt_grgi.
  if <fs_grgi> is assigned.
    unassign <fs_grgi>.
  endif.
endform.                    " get_data_from_db
*&------------------------------------------------------------------*
*&      form  collect_data_by
*&------------------------------------------------------------------*
form collect_data_by.

  field-symbols: <fs_gen> like line of it_qs_result,
                 <fs_qsc> like line of it_qscore.

  refresh it_qs_result.
  clear wa_qs_port1.

  loop at it_grgi assigning <fs_gen>.
   if it_grgi-lndwntime is initial .
      <fs_gen>-lndwntime = 100.

    endif.
    if it_grgi-RESPONSIVE is initial .
    <fs_gen>-RESPONSIVE = 100.

    endif.

    move-corresponding <fs_gen> to wa_qs_port1.
    move : 'MIN' to wa_qs_port1-meinh.
    collect wa_qs_port1 into it_qs_result.
  endloop.

  if <fs_gen> is assigned.
    unassign <fs_gen>.
  endif.
  clear wa_qs_port1.

  loop at it_def assigning <fs_gen>.


    move-corresponding <fs_gen> to wa_qs_port1.
    move : 'MIN' to wa_qs_port1-meinh.
     collect wa_qs_port1 into it_qs_result.
  endloop.
  clear wa_qs_port1.

  loop at it_qscore assigning <fs_qsc>.
    move-corresponding <fs_qsc> to wa_qs_port1.
    collect wa_qs_port1 into it_qs_result.
  endloop.


endform.                    " collect_data_by
*&------------------------------------------------------------------*
*&      form  calculate_qual_score
*&------------------------------------------------------------------*
form calculate_qual_score  tables pt_qs_v structure wa_qs_port1.

  field-symbols: <qual_score> like line of it_qs_result.

  loop at pt_qs_v assigning <qual_score>.
*-    PPM = ( defect / gi quantity ) * 1,000,000
    if not <qual_score>-menge_gi is initial.
      <qual_score>-q_ppm =
               ( <qual_score>-rkmng / <qual_score>-menge_gi ) * 1000000.
    endif.
*-     quality score
*    <qual_score>-q_score = <qual_score>-q_ppm  +
*            ( <qual_score>-linestop + <qual_score>-qnt_camp
*            + <qual_score>-qnt_salv + <qual_score>-qnt_repr ).

  endloop.

*-- sort by quality score : descending sort
*-- and fill rank(srno) field using loop index

  sort pt_qs_v by q_score descending.
  loop at pt_qs_v assigning <qual_score>.
    move : sy-tabix to <qual_score>-srno.   "/no by q.score descending
  endloop.

endform.                    " calculate_qual_score
*&-----------------------------------------------------------------*
*&      form  fill_text
*&-----------------------------------------------------------------*
form fill_text.
*  data : lw_score_index like sy-tabix.
*
*  loop at it_wa_qs_port1.
*    lw_score_index = sy-tabix.
*
*    select single name1 into it_wa_qs_port1-name1
*       from lfa1
*         where lifnr = it_wa_qs_port1-lifnr.
*
*    modify it_wa_qs_port1 index lw_score_index.
*  endloop.

endform.                    " fill_text
*&------------------------------------------------------------------*
*&      module  status_9000  output
*&------------------------------------------------------------------*
module status_9000 output.
  set pf-status '9000'.
  set titlebar  '9000'.

endmodule.                 " status_9000  output
*&------------------------------------------------------------------*
*&      module  modify_screen_9000  output
*&------------------------------------------------------------------*
module modify_screen_9000 output.
* When material is not specified don't display material at top
  if s_matnr is initial.
    loop at screen.
      if screen-group1 = 'MAT'.
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  endif.
endmodule.                 " modify_screen_9000  output
*&------------------------------------------------------------------*
*&      module  create_alv_object  output
*&------------------------------------------------------------------*
module create_alv_object output.
  if grid_container is initial. "/not created container for alv grid
    clear wa_renewal_flg.
*- create container('grid_container') with custom contro on screen
    create object grid_container
           exporting container_name = wa_custom_control
           exceptions
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.

    if sy-subrc ne 0.
      wa_repid = sy-repid.
      call function 'POPUP_TO_INFORM'
           EXPORTING
                titel = wa_repid
                txt2  = sy-subrc
                txt1  = 'Control can not be created'.
    endif.

*- if the parameter, i_appl_events, is set, the alv grid control
*  registers all events as application events. if the parameter is not
*  set, all events are registered as system events.
    create object alv_grid
           exporting i_parent = grid_container
                     i_appl_events = 'X'.

*-- prepare setting attributes and etc of alv object
    perform set_attributes_alv_grid.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    perform mask_columns_of_alv_grid tables it_fieldcat.

**-- adjust field sort and subtotal to display total of column
    perform set_sort_total_field.

*-- display data on alv grid control using method
*-- 'set_table_for_first_display'
    perform set_table_for_display.

*/-- create object to receive events and link them to handler methods.
*  when the alv control raises the event for the specified instance
*  the corresponding method is automatically called.
    create object event_receiver.
    set handler event_receiver->handle_double_click  for alv_grid.
*-   toolbar control event
    set handler event_receiver->handle_user_command  for alv_grid.
    set handler event_receiver->handle_toolbar       for alv_grid.

*- call method 'set_toolbar_interactive' to raise event toolbar.
    call method alv_grid->set_toolbar_interactive.

    call method cl_gui_control=>set_focus
                        exporting control = alv_grid.

  endif.

  if not grid_container is initial and "/created container for alv grid
     not wa_renewal_flg is initial.
    clear wa_renewal_flg.
*-- prepare setting attributes and etc of alv object
    perform set_attributes_alv_grid.

*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure
    perform mask_columns_of_alv_grid tables it_fieldcat.

**-- adjust field sort and subtotal to display total of column
*    perform set_sort_total_field.

*-- display data on alv grid control using method
*-- 'set_table_for_first_display'
    perform set_table_for_display.

    perform refresh_alv_grid_data_disp.

    call method cl_gui_control=>set_focus
                        exporting control = alv_grid.

  endif.

endmodule.                 " create_alv_object  output
*&---------------------------------------------------------------------*
*&      form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
form set_attributes_alv_grid.
  data : lw_s_dragdrop type lvc_s_dd01. "/ drag&drop control settings

  clear : wa_is_layout, wa_variant.
*//-- set layout structure


  wa_is_layout-language = sy-langu.      "/language key
  wa_is_layout-cwidth_opt = c_mark.     "/optimize column width
*  wa_is_layout-detailtitl = ''.        "/title bar of detail screen
*  wa_is_layout-grid_title = ''.        "/ title bar text
*  wa_is_layout-keyhot      = c_mark.    "/ key columns as hotspot
*  wa_is_layout-no_headers  = c_mark.     "/hide column headings
*  wa_is_layout-no_hgridln  = c_mark.     "/hide horizontal grid lines
*  wa_is_layout-no_vgridln  = c_mark.     "/hide vertical grid lines
*  wa_is_layout-no_merging  = c_mark.     "/disable cell merging
*  wa_is_layout-no_rowmark  = c_mark.     "/disable row selections
*  wa_is_layout-no_toolbar  = c_mark.     "/hide toolbar
  wa_is_layout-numc_total  = c_mark. "/allow totals for numc
*  wa_is_layout-s_dragdrop  = lw_s_dragdrop. "/drag & drop control

  wa_is_layout-sel_mode  = 'A'. "/mode for select col and row
*  wa_is_layout-sgl_clk_hd = c_mark. "/sorts the list whe column clicked

*//-- set variant structure
  wa_variant-report = sy-repid.
  wa_variant-username = sy-uname.

endform.                    " set_attributes_alv_grid
*&----------------------------------------------------------------*
*&      form  mask_columns_of_alv_grid
*&-----------------------------------------------------------------*
form mask_columns_of_alv_grid tables   pt_fieldcat type lvc_t_fcat.

  refresh pt_fieldcat. clear pt_fieldcat.

* build the fieldcat according to ddic structure :
  call function 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZSQM_QS_PORT1'
       CHANGING
            ct_fieldcat      = pt_fieldcat[].


* set field attribute
  loop at pt_fieldcat.

    if pt_fieldcat-fieldname = 'SRNO'.
      pt_fieldcat-key_sel = c_mark.
      pt_fieldcat-key     = c_mark.
      pt_fieldcat-coltext = 'No'(t60).
    elseif pt_fieldcat-fieldname = 'NAME1' or
           pt_fieldcat-fieldname = 'LIFNR' or
           pt_fieldcat-fieldname = 'BWART'.
      pt_fieldcat-no_out = c_mark.
    elseif pt_fieldcat-fieldname = 'MATNR' or
           pt_fieldcat-fieldname = 'MAKTX'.
      pt_fieldcat-key_sel = c_mark.
      pt_fieldcat-key     = c_mark.
    endif.

    if pt_fieldcat-fieldname = 'MENGE_GR'.
      pt_fieldcat-coltext = 'GR Quantity'(t51).
    elseif pt_fieldcat-fieldname = 'MENGE_GI'.
      pt_fieldcat-coltext = 'GI Quantity'(t52).
    elseif pt_fieldcat-fieldname = 'RKMNG'.
      pt_fieldcat-coltext = 'Defect'(t53).
    elseif pt_fieldcat-fieldname = 'Q_PPM'.
      pt_fieldcat-emphasize = 'C711'.
    elseif pt_fieldcat-fieldname = 'LINESTOP'.
      pt_fieldcat-coltext = 'Line Stop'(t54).
    elseif pt_fieldcat-fieldname = 'QNT_CAMP'.
      pt_fieldcat-coltext = 'Campaign'(t55).
    elseif pt_fieldcat-fieldname = 'QNT_SALV'.
      pt_fieldcat-coltext = 'Salvage'(t56).
    elseif pt_fieldcat-fieldname = 'QNT_REPR'.
      pt_fieldcat-coltext = 'Repair'(t57).
    elseif pt_fieldcat-fieldname = 'Q_SCORE'.
      pt_fieldcat-emphasize = 'C310'.
    endif.

    if     ( pt_fieldcat-datatype = 'QUAN'    or
             pt_fieldcat-datatype = 'INT4'      ) and
       not ( pt_fieldcat-fieldname = 'Q_PPM'   or
             pt_fieldcat-fieldname = 'Q_SCORE' or
             pt_fieldcat-fieldname = 'SRNO'     ).

      pt_fieldcat-do_sum = c_mark.
    endif.

    modify pt_fieldcat.
  endloop.

endform.                    " mask_columns_of_alv_grid
*&---------------------------------------------------------------*
*&      module  set_cursor_field  output
*&---------------------------------------------------------------*
module set_cursor_field output.
  set cursor field wa_fldtxt line wa_cur_line.
endmodule.                 " set_cursor_field  output
*&----------------------------------------------------------------*
*&      module  get_cursor_field  input
*&----------------------------------------------------------------*
module get_cursor_field input.
  clear: wa_fldtxt, wa_cur_line.
  get cursor field wa_fldtxt line wa_cur_line.
endmodule.                 " get_cursor_field  input
*&---------------------------------------------------------------------*
*&      module  exit_9000  input
*&---------------------------------------------------------------------*
module exit_9000 input.
  ok_code = sy-ucomm.
  clear sy-ucomm.

  case ok_code.
    when 'EXIT'.
*      if wa_level = c_vendor.
      perform free_alv_grid.
      leave to screen 0.
*      endif.

      perform set_list_level_control.

    when 'RW'.
*      if wa_level = c_vendor.
      perform free_alv_grid.
      leave to screen 0.
*      endif.

      perform set_list_level_control.

    when others.
  endcase.

endmodule.                 " exit_9000  input
*&------------------------------------------------------------------*
*&      form  free_alv_grid
*&------------------------------------------------------------------*
form free_alv_grid.
* on return back to selection screen the alv grid should be cleared
  check not alv_grid is initial.
  call method alv_grid->free.
  call method cl_gui_cfw=>flush.

  if sy-subrc ne 0.
    wa_repid = sy-repid.
    call function 'POPUP_TO_INFORM'
         EXPORTING
              titel = wa_repid
              txt2  = sy-subrc
              txt1  = text-e01.
  endif.

endform.                    " free_alv_grid
*&------------------------------------------------------------------*
*&      module  user_command_9000  input
*&------------------------------------------------------------------*
module user_command_9000 input.
  ok_code = sy-ucomm.
  clear sy-ucomm.
  case ok_code.
* on return back to selection screen the alv grid should be cleared
    when 'BACK'.
*      if wa_level = c_vendor.
      perform free_alv_grid.
      leave to screen 0.
*      endif.
* detail level screen should come back to vendor screen
      perform set_list_level_control.

    when 'REFRESH'.  "/02/16/2004 - sllee requested by mr. moon

      refresh it_grgi.
*-- get quality score data from db.
      perform get_data_from_db.

      if not it_grgi[]  is initial or
         not it_def[]   is initial or
         not it_qscore[] is initial.

*-- collect data by vendor and matnerial.
        perform collect_data_by.

*-- collect data by vendor for basic list display
        perform collect_dat_by_vendor_basic.

*-- calculate quality score
        perform calculate_qual_score tables it_qs_result.

        check not it_grgi[] is initial.
*-- fill text(Vendor name).
*        perform fill_text.

      endif.

*      case wa_level.
*        when c_vendor.

*        when c_material.
*          refresh it_zsqm_q_vend_d.
*-- collect data .
*          perform collect_data_by_detail using wa_sel_vendor-lifnr.
*-- calculate quality score
*          perform calculate_qual_score  tables it_zsqm_q_vend_d.
*        when others.
*      endcase.

      wa_renewal_flg = c_mark.

    when others.
  endcase.

endmodule.                 " user_command_9000  input
*&-----------------------------------------------------------------*
*&      form  get_ext_mat_group_text
*&-------------------------------------------------------------------*
form get_ext_mat_group_text using    p_extwg
                                     p_ewbez.
* get ext material group text
  check not p_extwg is initial.

  select single ewbez into p_ewbez
    from twewt
      where extwg = p_extwg
        and spras = sy-langu.

endform.                    " get_ext_mat_group_text
*&---------------------------------------------------------------------*
*&      form  set_sort_total_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->p_it_sort  text
*      -->p_it_fieldcat  text
*----------------------------------------------------------------------*
form set_sort_total_field.
*----- 01/21/2004 append by bsbae
  clear: it_sort.
  move: 1 to it_sort-spos,
        'Q_SCORE'  to it_sort-fieldname,
        'X'        to it_sort-down.
  append it_sort.
endform.                    " set_sort_total_field
*&------------------------------------------------------------------*
*&      form  retriev_detail_data
*&------------------------------------------------------------------*
form retriev_detail_data using    p_index.

* retrieve detail level data
  data : lw_sel_index   like sy-tabix.
  data : lw_table_lines like sy-tabix.

**- quality score by vendor for material document data :collective data
*data : it_wa_qs_port1 like wa_qs_port1 occurs 0 with header line.
*
**- quality score  alv display : basic and detail list internal tables
*data : it_zsqm_q_vend_b like wa_qs_port1 occurs 0 with header line.
*data : it_zsqm_q_vend_d like wa_qs_port1 occurs 0 with header line.

  lw_sel_index = p_index.

*  refresh it_zsqm_q_vend_d.

*  case wa_level.
*    when c_vendor.
*      clear it_zsqm_q_vend_b.
*      read table it_zsqm_q_vend_b index lw_sel_index.
*      check sy-subrc  = 0.
*
*      move-corresponding it_zsqm_q_vend_b to wa_sel_vendor.
*
*      wa_level = c_material.
*
**-- collect data .
*      perform collect_data_by_detail using wa_sel_vendor-lifnr.
*
**-- calculate quality score
*      perform calculate_qual_score  tables it_zsqm_q_vend_d.

*    when others.

*  endcase.

endform.                    " retriev_detail_data
*&------------------------------------------------------------------*
*&      form  collect_dat_by_vendor_basic
*&------------------------------------------------------------------*
form collect_dat_by_vendor_basic.

*  refresh it_zsqm_q_vend_b.
*
*  loop at it_wa_qs_port1.
*    clear it_zsqm_q_vend_b.
*    move-corresponding it_wa_qs_port1 to it_zsqm_q_vend_b.
*    clear : it_zsqm_q_vend_b-matnr,
*            it_zsqm_q_vend_b-maktx,
*            it_zsqm_q_vend_b-bwart.
*
*    collect it_zsqm_q_vend_b.
*  endloop.

endform.                    " collect_dat_by_vendor_basic
*&---------------------------------------------------------------------*
*&      form  set_table_for_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_table_for_display.

  call method alv_grid->set_table_for_first_display
       exporting i_structure_name = 'ZSQM_QS_PORT1'
                 is_layout        = wa_is_layout
                 i_save           = wa_save
                 is_variant       = wa_variant
                 i_default        = space "c_mark
       changing  it_fieldcatalog  = it_fieldcat[]
                 it_sort          = it_sort[]
                 it_outtab        = it_qs_result[].

endform.                    " set_table_for_display
*&------------------------------------------------------------------*
*&      form  collect_data_by_detail
*&------------------------------------------------------------------*
form collect_data_by_detail using    p_lifnr.
* select detail information by vendor number
*  refresh it_zsqm_q_vend_d.
*
*  loop at it_wa_qs_port1 where lifnr = p_lifnr.
*    clear it_zsqm_q_vend_d.
*    move-corresponding it_wa_qs_port1 to it_zsqm_q_vend_d.
*    clear : it_zsqm_q_vend_d-lifnr,
*            it_zsqm_q_vend_d-name1,
*            it_zsqm_q_vend_d-bwart.
*
*    collect it_zsqm_q_vend_d.
*  endloop.
endform.                    " collect_data_by_detail
*&------------------------------------------------------------------*
*&      form  set_list_level_control
*&------------------------------------------------------------------*
form set_list_level_control.
*  case wa_level.
*    when c_vendor.
*    when c_material.
*      wa_level = c_vendor.
*      wa_renewal_flg = c_mark.
*    when others.
*  endcase.
endform.                    " set_list_level_control
*&------------------------------------------------------------------*
*&      form  refresh_alv_grid_data_disp
*&------------------------------------------------------------------*
form refresh_alv_grid_data_disp.
  call method alv_grid->refresh_table_display
*         exporting
*           is_stable      =
*           i_soft_refresh =
*         exceptions
*           finished       = 1
*           others         = 2
          .
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
               with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " refresh_alv_grid_data_disp
*&---------------------------------------------------------------------*
*&      Form  GET_DEFAULT_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DEFAULT_VENDOR.
  CLEAR WA_VENDOR.
  SELECT SINGLE LIFNR FROM ZTQM_VENDOR_ID  INTO WA_VENDOR WHERE UNAME =
  SY-UNAME.
  IF SY-SUBRC = 0.
    P_LIFNR = WA_VENDOR.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'Q23'.
        SCREEN-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.


  ENDIF.


ENDFORM.                    " GET_DEFAULT_VENDOR
