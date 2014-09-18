*&---------------------------------------------------------------------*
*& Report  ML_MBEW_CONSISTENCY                                         *
*&---------------------------------------------------------------------*
*& report  ML_MBEW_CONSISTENCY                                         *
*&                                                                     *
*&---------------------------------------------------------------------*
*& Checks MBEW (EBEW, OBEW, QBEW) consistency regarding the values of  *
*& MLBWA, MLAST and MLASV in T001K and of T134M-WERTU. Sets/deletes    *
*& MBEW-MLMAA and MLAST if necessary and reports further errors.       *
*&---------------------------------------------------------------------*
*ML Helpdesk 3.5
*U9CK906500 YTP 210604 T_CKMLV not cleared, if no entry found
*ML Helpdesk 3.4
*U9CK906336 YTP 281103 Check BWKEY is productive(CKMLV-MLBWI = 'X')
*                      Check MLAST only when MLMAA = 'X' and not
*                      for material types like UNBW
*ML Helpdesk 1.6
*U9CK902491 PUE 190701 Check period status in case of error 4
*ML Helpdesk 1.4
*U9CK901994 PUE 110501 Change order of warning popups,  no correction of
*                      error 4 (MLAST = '3' though VPRSV = 'V'),
*                      Update with "42" only
*ML Helpdesk 1.2
*U9CK901500 PUE 281200 List output and correction document for ToDo-list
*                      selection with multiple records;
*                      QBEW with ToDo-list selection;
*                      Batch run capability
*4.6C
*XBAK000024 PUE 210600 Creation                                        *

report mlhelp_mbew_consistency no standard page heading.

tables : t001k,
         t134m,
         sscrfields,
         ckmlhd,
         mara,
         mbew,
         ebew,
         obew,
         qbew,
         ckmlpp.

type-pools: ckmh1, slis, ckmv0.

types: begin of obj_key,
         matnr like mbew-matnr,
         bwkey like mbew-bwkey,
         bwtar like mbew-bwtar,
         sobkz like ebew-sobkz,
         vbeln like ebew-vbeln,
         posnr like ebew-posnr,
         lifnr like obew-lifnr,
         pspnr like qbew-pspnr,
       end of obj_key.
types: begin of obj_data,
         kaln1 like mbew-kaln1,
         kalnr like mbew-kalnr,
         bwtty like mbew-bwtty,
         mlmaa like mbew-mlmaa,
         mlast like mbew-mlast,
         vprsv like mbew-vprsv,
         lfgja like mbew-lfgja,
         lfmon like mbew-lfmon,
       end of obj_data.

types: begin of correct.
include type obj_key.
include type obj_data.
types:   errorflag type c,
         corrflag type c,
         error3 type c,
         error4 type c,
         error5 type c,
       end of correct.

types: begin of count,
       table(5) type c,
       rec_checked type i,
       mlmaa_set type i,
       mlmaa_del type i,
       error_2 type i,
       error_3 type i,
       error_4 type i,
       error_5 type i,
       mlast_change type i,
       end of count.

types: obj_correct type correct occurs 100.
types: todo_list type standard table of ckmh1_todo_obj.

field-symbols: <tablec> type count.

data:  f_mara like mara.

data:  lh_doit type c.                 " lh_doit for warning pop-up
data:  i_zahl type i.                  " DB-COMMIT-COUNTER
data:  error_flag type xfeld.

data:  objects_to_correct type obj_correct with header line,
       todo_in  type todo_list with header line,
       todo_out type todo_list with header line.


data:  mbew_old like mbew,
       mbew_new like mbew,
       ebew_old like ebew,
       ebew_new like ebew,
       obew_old like obew,
       obew_new like obew,
       qbew_old like qbew,
       qbew_new like qbew,
       t_ckmlv type sorted table of ckmlv with unique key bwkey
                                          with header line,
       t_t001k type sorted table of t001k with unique key bwkey
                                          with header line,
       t_t134m type sorted table of t134m with unique key bwkey mtart
                                          with header line.

data:  h_cursor type cursor,
       active_table(4) type c.

data:  mbewc type count,
       ebewc type count,
       obewc type count,
       qbewc type count,
       totalc type count.

data:  correct_belnr type standard table of mlrephd-belnr
                          with header line.
data:  ausgz type boole-boole.
data:  gd_alv_corritem_list like mlrepit occurs 0.
data:  ld_mlrepit like mlrepit.
data:  rs_selfield type slis_selfield.
data:  gd_master type boole-boole.
data:  gd_dontpanic like sy-datlo.


***********************************************************************
*{ Selektionsbild
***********************************************************************

selection-screen skip 1.
*{ Frame: Test-run
* Checkbox: Test (default) or write updates
* Checkbox: write (default) or suppress list output
selection-screen begin of block test_frame with frame title text-004.
parameter:  p_test like boole-boole default 'X',
            p_mustck like cki_doc_ml-test no-display,
            p_nolist like boole-boole default ' '.
selection-screen skip 1.

*{ Frame: Todo list: as input and/or output(default)
selection-screen begin of block todo_list with frame title text-003.
parameter: p_todoin(10)    type c,
           p_todout(10)    type c.
selection-screen end of block todo_list.
*{ Frame: Todo list

*{ Frame: mbew-key selection
selection-screen begin of block mbew-key with frame title text-005.
select-options:
       s_matnr for mbew-matnr
                   memory id mat,
       s_bwkey for mbew-bwkey
                   memory id bwk,
       s_bwtar for mbew-bwtar
                   memory id bwt,
       s_kalnr for ckmlhd-kalnr.

selection-screen end of block mbew-key.
*} mbew-key selection

selection-screen end of block test_frame.
*} Frame: Test-run

*}Selektionsbild

at selection-screen output.
*   Fix by Andy
    get parameter id 'DONTPANIC' field gd_dontpanic.
    if gd_dontpanic eq sy-uname.
      gd_master = 'X'.
    endif.

  loop at screen.
    case screen-name.
      when 'P_TEST'.
        if gd_master = 'X'.
          screen-input = '1'.
        else.
          p_test = 'X'.
          screen-input = '0'.
        endif.
        modify screen.
      when others.
    endcase.
  endloop.

at selection-screen.
  if sscrfields-ucomm = '42'.
*   Sitzt der Benutzerparameter?
    get parameter id 'DONTPANIC' field gd_dontpanic.
    if gd_dontpanic eq sy-datlo.
      gd_master = 'X'.
    endif.
  endif.


***********************************************************************
*{ Check MATNR INPUT
***********************************************************************
  if p_todoin is initial.
* no todo-list given as input.
    if  ( s_matnr[] is initial ) and ( s_kalnr[] is initial ).
* if no materials selected: select materials generically
      s_matnr-sign = 'I'.
      s_matnr-option = 'CP'.
      s_matnr-low = '*'.
      append s_matnr.
      write: / text-006.
    endif.
    if  not ( s_kalnr[] is initial ) and not ( s_matnr[] is initial ).
* if both matnrs and kalnrs are selected: use kalnr
      perform popup_dialog                                  "PUE281200
              using    'Y'
                       'Only cost est. no. selection will be used!'
                       'Continue?'
                       'Warning!!'
              changing lh_doit.
      if lh_doit = 'N' or
         lh_doit = 'A'.
        stop.
      endif.
      clear: s_matnr[],
             s_bwkey[],
             s_bwtar[].
      select * from ckmlhd where kalnr in s_kalnr.
        s_matnr-sign = 'I'.
        s_matnr-option = 'EQ'.
        s_matnr-low = ckmlhd-matnr.
        append s_matnr.
        s_bwkey-sign = 'I'.
        s_bwkey-option = 'EQ'.
        s_bwkey-low = ckmlhd-bwkey.
        append s_bwkey.
        s_bwtar-sign = 'I'.
        s_bwtar-option = 'EQ'.
        s_bwtar-low = ckmlhd-bwtar.
        append s_bwtar.
      endselect.
    endif.
  endif.
*} Check MATNR INPUT


***********************************************************************
*{ Check todo-output NOT ALREADY EXISTS
***********************************************************************
  if not p_todout is initial.
    call function 'MLHELP_TODO_READ'
         exporting
              i_todo_ident        = p_todout
              i_check_only        = 'X'
         exceptions
              todo_list_not_found = 1.
    if sy-subrc = 0.
      perform popup_dialog                                  "PUE281200
              using    'Y'
                       'ToDO-List already exists and will be replaced!'
                       'Continue?'
                       'Warning!!'
              changing lh_doit.
      if lh_doit = 'N' or
         lh_doit = 'A'.
        stop.
      endif.

    endif.
  endif.
*} Check todo-output NOT ALREADY EXISTS

***********************************************************************
*{ Read todo-input if chosen as selection
***********************************************************************
  if not p_todoin is initial.
    if  not s_matnr[] is initial or not s_kalnr[] is initial.
* Additional selection was made
      perform popup_dialog                                  "PUE281200
              using    'Y'
                       'Only todo-list selection will be used!'
                       'Continue?'
                       'Warning!!'
              changing lh_doit.
      if lh_doit = 'N' or
         lh_doit = 'A'.
        stop.
      endif.
    endif.
    call function 'MLHELP_TODO_READ'
         exporting
              i_todo_ident        = p_todoin
         importing
              et_todo_tbl         = todo_in[]
         exceptions
              todo_list_not_found = 1.
    if sy-subrc = 1.
      write: 'Input ToDo-List not found!'.
      stop.
    endif.
  endif.
*} Read todo-input if chosen as selection

start-of-selection.

  if p_mustck = 'X'.
    clear p_test.
  endif.

***********************************************************************
*{ Initializations
***********************************************************************
  free:  i_zahl,
         objects_to_correct,
         mbew_old,
         mbew_new,
         ebew_old,
         ebew_new,
         obew_old,
         obew_new,
         qbew_old,
         qbew_new,
         t_t001k,
         t_t134m,
         h_cursor,
         ebewc,
         mbewc,
         obewc,
         qbewc,
         totalc,
         active_table,
         correct_belnr.
  mbewc-table = 'MBEW'.
  ebewc-table = 'EBEW'.
  obewc-table = 'OBEW'.
  qbewc-table = 'QBEW'.
  totalc-table = 'TOTAL'.
  set pf-status 'STD'.
*} Initializations


***********************************************************************
*{ Filling internal tables t_t001k and t_t134m                        *
***********************************************************************
  select * from ckmlv into table t_ckmlv.
  select * from t001k into table t_t001k.
  select * from t134m into table t_t134m.
*} Filling internal tables t_t001k and t_t134m


***********************************************************************
*{ Program processing logic                                           *
***********************************************************************
  if p_todoin is initial.
* New selection from database: open cursor for select from databases
    perform process_mbew.
    perform process_ebew.
    perform process_obew.
    perform process_qbew.
  else.
* todo-list chosen as selection - loop at todo list.
    loop at todo_in.
      if todo_in-obtyp ne 'MB'.
* <3> ggf. für obtyp MA select-single auf ckmlhd einbauen**************
        exit.
      endif.
      if  not todo_in-vbeln is initial.
*       ebew-record.
        perform ebew_single using todo_in
                                  error_flag.
        if not error_flag is initial.
          continue.
        endif.
      elseif not todo_in-lifnr is initial.
*       obew-record.
        perform obew_single using todo_in
                                  error_flag.
        if not error_flag is initial.
          continue.
        endif.
    elseif not todo_in-pspnr is initial.                    "PUE281200
*       qbew-record.
        perform qbew_single using todo_in
                                  error_flag.
        if not error_flag is initial.
          continue.
        endif.
      else.
*       mbew-record.
        perform mbew_single using todo_in
                                  error_flag.
        if not error_flag is initial.
          continue.
        endif.
      endif.
    endloop.
    perform db_end_commit.
  endif.

***********************************************************************
*{ Post ToDo List
***********************************************************************
  if not p_todout is initial.
*   if todo-list output requested, write todo_out to database.
    call function 'MLHELP_TODO_WRITE'
         exporting
              i_replace                = 'X'
              i_todo_ident             = p_todout
              it_todo_tbl              = todo_out[]
*      EXCEPTIONS
*           identifier_already_exist = 1
         .
  endif.
*{ Post ToDo List
*/

***********************************************************************
*{ Write  Statistics.
***********************************************************************
  perform write_statistics.
*{ Write  Statistics
*/

*} Program processing logic
*/

***********************************************************************
* TOP-OF-PAGE                                                         *
***********************************************************************
top-of-page.
  perform write_table_header.

***********************************************************************
* AT USER-COMMAND                                                     *
***********************************************************************
at user-command.
  case sy-ucomm.
    when 'EF02'.
      check ausgz ne space.
      set pf-status 'CORRLIST_ALV' of program 'SAPLMLHELPCORRADMIN'.
      call function 'MLHELP_CORRECTION_SHOW'
        exporting
          i_kjahr                    = sy-datlo(4)
          i_belnr                    = correct_belnr
*     EXCEPTIONS
*       CORRECTION_NOT_FOUND       = 1
*       CORRECTION_IS_EMPTY        = 2
*       OTHERS                     = 3
          .
      if sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.
    when 'BACK'.
      leave.
    when '&IC1'.
      read table gd_alv_corritem_list
           index rs_selfield-tabindex
           into  ld_mlrepit.
      check sy-subrc eq 0.
      call function 'MLHELP_CORRECTION_ITEM_SHOW'
           exporting
                if_mlrepit = ld_mlrepit
           exceptions
                others     = 01.
  endcase.
  clear ausgz.
***********************************************************************
* FORM PROCESS_MBEW                                                   *
***********************************************************************
form process_mbew.
  new-page.
  active_table = 'MBEW'.
  open cursor with hold h_cursor for
      select *  from mbew
                where  matnr in s_matnr
                and    bwtar in s_bwtar
                and    bwkey in s_bwkey.
  do.
    clear: objects_to_correct[].
    fetch next cursor h_cursor
               into  corresponding fields of
               table objects_to_correct
               package size 100.
    if sy-subrc <> 0.
*     Last MBEW records read, DB_COMMIT
      close cursor h_cursor.
      perform db_end_commit.
      exit.
    endif.
    assign mbewc to <tablec>.
    loop at objects_to_correct.
      perform object_check changing objects_to_correct
                                    error_flag.
      if not error_flag is initial.
        continue.
      endif.
*     check single record
      if objects_to_correct-errorflag = 'X'.
*       if record had an error, read complete data and process output
        select single * from mbew
                          where matnr = objects_to_correct-matnr
                          and   bwkey = objects_to_correct-bwkey
                          and   bwtar = objects_to_correct-bwtar.
        perform mbew_output changing mbew
                                     objects_to_correct.
      endif.
    endloop.
  enddo.
endform.


***********************************************************************
* FORM PROCESS_EBEW                                                   *
***********************************************************************
form process_ebew.
  new-page.
  active_table = 'EBEW'.
  open cursor with hold h_cursor for
    select *  from ebew
              where  matnr in s_matnr
              and    bwtar in s_bwtar
              and    bwkey in s_bwkey.
  do.
    clear: objects_to_correct[].
    fetch next cursor h_cursor
                   into  corresponding fields of
                   table objects_to_correct
                   package size 100.
    if sy-subrc <> 0.
*     Last EBEW records read, DB_COMMIT
      close cursor h_cursor.
      perform db_end_commit.
      exit.
    endif.
    assign ebewc to <tablec>.
    loop at objects_to_correct.
      perform object_check changing objects_to_correct
                                    error_flag.
      if not error_flag is initial.
        continue.
      endif.
*     check single record
      if objects_to_correct-errorflag = 'X'.
*       if record had an error, read complete data and process output
        select single * from ebew
                        where matnr = objects_to_correct-matnr
                        and   bwkey = objects_to_correct-bwkey
                        and   bwtar = objects_to_correct-bwtar
                        and   sobkz = objects_to_correct-sobkz
                        and   vbeln = objects_to_correct-vbeln
                        and   posnr = objects_to_correct-posnr.
        perform ebew_output changing ebew
                                     objects_to_correct.
      endif.
    endloop.
  enddo.
endform.

***********************************************************************
* FORM PROCESS_OBEW                                                   *
***********************************************************************
form process_obew.
  new-page.
  active_table = 'OBEW'.
  open cursor with hold h_cursor for
    select *  from obew
              where  matnr in s_matnr
              and    bwtar in s_bwtar
              and    bwkey in s_bwkey.
  do.
    clear: objects_to_correct[].
    fetch next cursor h_cursor
               into  corresponding fields of
               table objects_to_correct
               package size 100.
    if sy-subrc <> 0.
*     Last OBEW records read, DB_COMMIT
      close cursor h_cursor.
      perform db_end_commit.
      exit.
    endif.
    assign obewc to <tablec>.
    loop at objects_to_correct.
      perform object_check changing objects_to_correct
                                    error_flag.
      if not error_flag is initial.
        continue.
      endif.
*     check single record
      if objects_to_correct-errorflag = 'X'.
*       if record had an error, read complete data and process output
        select single * from obew
                        where matnr = objects_to_correct-matnr
                        and   bwkey = objects_to_correct-bwkey
                        and   bwtar = objects_to_correct-bwtar
                        and   sobkz = objects_to_correct-sobkz
                        and   lifnr = objects_to_correct-lifnr.
        perform obew_output changing obew
                                     objects_to_correct.
      endif.
    endloop.
  enddo.
endform.

***********************************************************************
* FORM PROCESS_QBEW                                                   *
***********************************************************************
form process_qbew.
  new-page.
  active_table = 'QBEW'.
  open cursor with hold h_cursor for
    select *  from qbew
              where  matnr in s_matnr
              and    bwtar in s_bwtar
              and    bwkey in s_bwkey.
  do.
    clear: objects_to_correct[].
    fetch next cursor h_cursor
               into  corresponding fields of
               table objects_to_correct
               package size 100.
    if sy-subrc <> 0.
*     Last QBEW records read, DB_COMMIT
      close cursor h_cursor.
      perform db_end_commit.
      exit.
    endif.
    assign qbewc to <tablec>.
    loop at objects_to_correct.
      perform object_check changing objects_to_correct
                                    error_flag.
      if not error_flag is initial.
        continue.
      endif.
*     check single record
      if objects_to_correct-errorflag = 'X'.
*       if record had an error, read complete data and process output
        select single * from qbew
                        where matnr = objects_to_correct-matnr
                        and   bwkey = objects_to_correct-bwkey
                        and   bwtar = objects_to_correct-bwtar
                        and   sobkz = objects_to_correct-sobkz
                        and   pspnr = objects_to_correct-pspnr.
        perform qbew_output changing qbew
                                     objects_to_correct.
      endif.
    endloop.
  enddo.
endform.

***********************************************************************
* FORM MBEW_SINGLE                                                    *
***********************************************************************
form mbew_single using todo type ckmh1_todo_obj
                       err_flag type xfeld.
  clear err_flag.
  clear objects_to_correct.
* read complete data for single todo_list-record
  select single * from mbew
                    where matnr = todo-matnr
                    and   bwkey = todo-bwkey
                    and   bwtar = todo-bwtar.
  move-corresponding mbew to objects_to_correct.
  assign mbewc to <tablec>.
  perform object_check changing objects_to_correct
                                error_flag.
  if not error_flag is initial.
    err_flag = error_flag.
  endif.
* check single record
  if objects_to_correct-errorflag = 'X'.
*   if record had an error, process output
    if active_table <> 'MBEW'.
      active_table = 'MBEW'.
      new-page.
    endif.                                                  "PUE281200
    perform mbew_output changing mbew
                                 objects_to_correct.
  endif.
endform.


***********************************************************************
* FORM EBEW_SINGLE                                                    *
***********************************************************************
form ebew_single using todo type ckmh1_todo_obj
                       err_flag type xfeld.
  clear err_flag.
  clear objects_to_correct.
* read complete data for single todo_list-record
  select single * from ebew
                  where matnr = todo-matnr
                  and   bwkey = todo-bwkey
                  and   bwtar = todo-bwtar
                  and   sobkz = todo-sobkz
                  and   vbeln = todo-vbeln
                  and   posnr = todo-posnr.
  move-corresponding ebew to objects_to_correct.
  assign ebewc to <tablec>.
  perform object_check changing objects_to_correct
                                error_flag.
  if not error_flag is initial.
    err_flag = error_flag.
  endif.
* check single record
  if objects_to_correct-errorflag = 'X'.
*   if record had an error, process output
    if active_table <> 'EBEW'.
      active_table = 'EBEW'.
      new-page.
    endif.                                                  "PUE281200
    perform ebew_output changing ebew
                                 objects_to_correct.
  endif.
endform.

***********************************************************************
* FORM OBEW_SINGLE                                                    *
***********************************************************************
form obew_single using todo type ckmh1_todo_obj
                       err_flag type xfeld.
  clear err_flag.
  clear objects_to_correct.
* read complete data for single todo_list-record
  select single * from obew
                  where matnr = todo-matnr
                  and   bwkey = todo-bwkey
                  and   bwtar = todo-bwtar
                  and   sobkz = todo-sobkz
                  and   lifnr = todo-lifnr.
  move-corresponding obew to objects_to_correct.
  assign obewc to <tablec>.
  perform object_check changing objects_to_correct
                                error_flag.
  if not error_flag is initial.
    err_flag = error_flag.
  endif.
* check single record
  if objects_to_correct-errorflag = 'X'.
*   if record had an error, process output
    if active_table <> 'OBEW'.
      active_table = 'OBEW'.
      new-page.
    endif.                                                  "PUE281200
    perform obew_output changing obew
                                 objects_to_correct.
  endif.
endform.

***********************************************************************
* FORM QBEW_SINGLE                                                    *
***********************************************************************
form qbew_single using todo type ckmh1_todo_obj
                       err_flag type xfeld.
  clear err_flag.
  clear objects_to_correct.
* read complete data for single todo_list-record
  select single * from qbew
                  where matnr = todo-matnr
                  and   bwkey = todo-bwkey
                  and   bwtar = todo-bwtar
                  and   sobkz = todo-sobkz
                  and   pspnr = todo-pspnr.
  move-corresponding qbew to objects_to_correct.
  assign qbewc to <tablec>.
  perform object_check changing objects_to_correct
                                error_flag.
  if not error_flag is initial.
    err_flag = error_flag.
  endif.
* check single record
  if objects_to_correct-errorflag = 'X'.
*   if record had an error, process output
    if active_table <> 'QBEW'.
      active_table = 'QBEW'.
      new-page.
    endif.                                                  "PUE281200
    perform qbew_output changing qbew
                                 objects_to_correct.
  endif.
endform.

***********************************************************************
* FORM MBEW_OUTPUT                                                    *
***********************************************************************
form mbew_output changing t_mbew type mbew
                          obj_to_correct type correct.
  mbew_new = t_mbew.
  mbew_old = t_mbew.
  mbew_new-mlmaa = obj_to_correct-mlmaa.
  mbew_new-mlast = obj_to_correct-mlast.
  if p_test is initial and obj_to_correct-corrflag = 'X'.
*   Database update
    update mbew from mbew_new.
    if i_zahl = 0.
*     First record for new document - open document
      call function 'MLHELP_CORRECTION_CREATE'.
    endif.
*     write document record
    call function 'MLHELP_CORRECTION_PROTOCOL'
         exporting
              if_mbew_before = mbew_old
              if_mbew_after  = mbew_new.
*     DB-commit every 50 corrected records
    perform commit_count.
  endif.
  if p_nolist is initial.
*   List output
    if mbew_old-mlmaa ne mbew_new-mlmaa or
       mbew_old-mlast ne mbew_new-mlast.
*       write mlmaa and mlast change line
      perform write_mbew_key using obj_to_correct.
      write: (12) mbew_old-mlmaa,
             (12) mbew_new-mlmaa,
             (12) mbew_old-mlast,
             (12) mbew_new-mlast.
    endif.
    if obj_to_correct-error3 = 'X'.
      perform write_mbew_key using objects_to_correct.
      write text-026.
*       MLAST not equal to T001K-MLAST though mandatory
    endif.
    if obj_to_correct-error4 = 'X'.
      perform write_mbew_key using objects_to_correct.
      write text-037.
*       MLAST = '3' though VPRSV = 'V'
    endif.
    if obj_to_correct-error5 = 'X'.
      perform write_mbew_key using objects_to_correct.
      write text-027.
*       No cost estimate no. assigned.
    endif.
  endif.
endform.

***********************************************************************
* FORM EBEW_OUTPUT                                                    *
***********************************************************************
form ebew_output changing t_ebew type ebew
                          obj_to_correct type correct.

  if obj_to_correct-errorflag = 'X'.
*   an error occurred with this record
    ebew_new = t_ebew.
    ebew_old = t_ebew.
    ebew_new-mlmaa = obj_to_correct-mlmaa.
    ebew_new-mlast = obj_to_correct-mlast.
    if p_test is initial and obj_to_correct-corrflag = 'X'.
*   Database update
      update ebew from ebew_new.
      if i_zahl = 0.
*     First record for new document - open document
        call function 'MLHELP_CORRECTION_CREATE'.
      endif.
*     write document record
      call function 'MLHELP_CORRECTION_PROTOCOL'
           exporting
                if_ebew_before = ebew_old
                if_ebew_after  = ebew_new.
*     DB-commit every 50 corrected records
      perform commit_count.
    endif.
    if p_nolist is initial.
*   List output
      if ebew_old-mlmaa ne ebew_new-mlmaa or
         ebew_old-mlast ne ebew_new-mlast.
*       write mlmaa and mlast change line
        perform write_ebew_key using obj_to_correct.
        write: (12) ebew_old-mlmaa,
               (12) ebew_new-mlmaa,
               (12) ebew_old-mlast,
               (12) ebew_new-mlast.
      endif.
      if obj_to_correct-error3 = 'X'.
        perform write_ebew_key using objects_to_correct.
        write text-026.
*       MLAST not equal to T001K-MLAST though mandatory
      endif.
      if obj_to_correct-error4 = 'X'.
        perform write_ebew_key using objects_to_correct.
        write text-037.
*       MLAST = '3' though VPRSV = 'V'
      endif.
      if obj_to_correct-error5 = 'X'.
        perform write_ebew_key using objects_to_correct.
        write text-027.
*       No cost estimate no. assigned.
      endif.
    endif.
  endif.
endform.

***********************************************************************
* FORM OBEW_OUTPUT                                                    *
***********************************************************************
form obew_output changing t_obew type obew
                          obj_to_correct type correct.

  if obj_to_correct-errorflag = 'X'.
*   an error occurred with this record
    obew_new = t_obew.
    obew_old = t_obew.
    obew_new-mlmaa = obj_to_correct-mlmaa.
    obew_new-mlast = obj_to_correct-mlast.
    if p_test is initial and obj_to_correct-corrflag = 'X'.
*   Database update
      update obew from obew_new.
      if i_zahl = 0.
*     First record for new document - open document
        call function 'MLHELP_CORRECTION_CREATE'.
      endif.
*     write document record
      call function 'MLHELP_CORRECTION_PROTOCOL'
           exporting
                if_obew_before = obew_old
                if_obew_after  = obew_new.
*     DB-commit every 50 corrected records
      perform commit_count.
    endif.
    if p_nolist is initial.
*   List output
      if obew_old-mlmaa ne obew_new-mlmaa or
         obew_old-mlast ne obew_new-mlast.
*       write mlmaa and mlast change line
        perform write_obew_key using obj_to_correct.
        write: (12) obew_old-mlmaa,
               (12) obew_new-mlmaa,
               (12) obew_old-mlast,
               (12) obew_new-mlast.
      endif.
      if obj_to_correct-error3 = 'X'.
        perform write_obew_key using objects_to_correct.
        write text-026.
*       MLAST not equal to T001K-MLAST though mandatory
      endif.
      if obj_to_correct-error4 = 'X'.
        perform write_obew_key using objects_to_correct.
        write text-037.
*         MLAST = '3' though VPRSV = 'V'
      endif.
      if obj_to_correct-error5 = 'X'.
        perform write_obew_key using objects_to_correct.
        write text-027.
*       No cost estimate no. assigned.
      endif.
    endif.
  endif.
endform.

***********************************************************************
* FORM QBEW_OUTPUT                                                    *
***********************************************************************
form qbew_output changing t_qbew type qbew
                          obj_to_correct type correct.

  if obj_to_correct-errorflag = 'X'.
*   an error occurred with this record
    qbew_new = t_qbew.
    qbew_old = t_qbew.
    qbew_new-mlmaa = obj_to_correct-mlmaa.
    qbew_new-mlast = obj_to_correct-mlast.
    if p_test is initial and obj_to_correct-corrflag = 'X'.
*   Database update
      update qbew from qbew_new.
      if i_zahl = 0.
*     First record for new document - open document
        call function 'MLHELP_CORRECTION_CREATE'.
      endif.
*     write document record
      call function 'MLHELP_CORRECTION_PROTOCOL'
           exporting
                if_qbew_before = qbew_old
                if_qbew_after  = qbew_new.
*     DB-commit every 50 corrected records
      perform commit_count.
    endif.
    if p_nolist is initial.
*   List output
      if qbew_old-mlmaa ne qbew_new-mlmaa or
         qbew_old-mlast ne qbew_new-mlast.
*       write mlmaa and mlast change line
        perform write_qbew_key using obj_to_correct.
        write: (12) qbew_old-mlmaa,
               (12) qbew_new-mlmaa,
               (12) qbew_old-mlast,
               (12) qbew_new-mlast.
      endif.
      if obj_to_correct-error3 = 'X'.
        perform write_qbew_key using objects_to_correct.
        write text-026.
*       MLAST not equal to T001K-MLAST though mandatory
      endif.
      if obj_to_correct-error4 = 'X'.
        perform write_qbew_key using objects_to_correct.
        write text-037.
*       MLAST = '3' though VPRSV = 'V'
      endif.
      if obj_to_correct-error5 = 'X'.
        perform write_qbew_key using objects_to_correct.
        write text-027.
*       No cost estimate no. assigned.
      endif.
    endif.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM OBJECT_CHECK                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form object_check changing obj_to_correct type correct
                           err_flag       type xfeld.
  clear err_flag.
  if t_ckmlv-bwkey ne obj_to_correct-bwkey.
    clear: t_ckmlv.
    read table t_ckmlv with key
                            bwkey = obj_to_correct-bwkey.
  endif.

  if t_t001k-bwkey ne obj_to_correct-bwkey.
    clear: t001k.
    read table t_t001k with key
                            bwkey = obj_to_correct-bwkey.
    if sy-subrc ne 0.
      perform write_key using obj_to_correct
                              active_table.
      write: 'BWKEY not found in table T001K'.
      err_flag = 'X'.
      exit.
    endif.
  endif.

* Get mara-mtart for t134m read
  if f_mara-matnr ne obj_to_correct-matnr.
    clear: f_mara.
    select single * from  mara into f_mara
                      where matnr = obj_to_correct-matnr.
    if sy-subrc ne 0.
      perform write_key using obj_to_correct
                              active_table.
      write: 'MATNR not found in table MARA'.
      err_flag = 'X'.
      exit.
    endif.
  endif.

  if t_t134m-bwkey ne obj_to_correct-bwkey   or
     t_t134m-mtart ne f_mara-mtart.
    clear: t_t134m.
    read table t_t134m with key
                          bwkey = obj_to_correct-bwkey
                          mtart = f_mara-mtart.
    if sy-subrc ne 0.
      perform write_key using obj_to_correct
                              active_table.
      write:   'entry with key BWKEY =',
               obj_to_correct-bwkey,
               'MTART =',
               f_mara-mtart,
               'not found in table T134M'.
      err_flag = 'X'.
      exit.
    endif.
  endif.

  perform check_mlmaa_at_wertu changing obj_to_correct.
  perform check_mlast_at_mlmaa changing obj_to_correct.
  perform check_mlast_at_t001k_mlasv changing obj_to_correct.
  perform check_vprsv_at_mlast changing obj_to_correct.
  perform check_kalnr_at_mlmaa changing obj_to_correct.

  <tablec>-rec_checked = <tablec>-rec_checked + 1.

  if ( obj_to_correct-errorflag = 'X' ) and not ( p_todout is initial ).
*   if an error occurred and an todo-list output is requested,
*   then write a new todo-list-record.
    move-corresponding obj_to_correct to todo_out.
    move obj_to_correct-kaln1 to todo_out-kalnr.
    todo_out-obtyp = 'MB'.
    append todo_out.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM check_mlmaa_at_wertu                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form check_mlmaa_at_wertu changing obj_to_correct type correct.
  if t_t134m-wertu = 'X' and t_ckmlv-mlbwi ='X'.
    if obj_to_correct-mlmaa <> 'X'.
      obj_to_correct-mlmaa = 'X'.
      obj_to_correct-errorflag = 'X'.
      obj_to_correct-corrflag = 'X'.
      <tablec>-mlmaa_set = <tablec>-mlmaa_set + 1.
    endif.
  else.
    if obj_to_correct-mlmaa = 'X'.
      obj_to_correct-mlmaa = ' '.
      obj_to_correct-corrflag = 'X'.
      obj_to_correct-errorflag = 'X'.
      <tablec>-mlmaa_del = <tablec>-mlmaa_del + 1.
    endif.
  endif.
endform.


*---------------------------------------------------------------------*
*       FORM check_mlast_at_mlmaa                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form check_mlast_at_mlmaa changing obj_to_correct type correct.
  if obj_to_correct-mlmaa = 'X' and obj_to_correct-mlast is initial.
    obj_to_correct-mlast = '2'.
    obj_to_correct-errorflag = 'X'.
    obj_to_correct-corrflag = 'X'.
    <tablec>-error_2 = <tablec>-error_2 + 1.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM check_mlast_at_t001k_mlasv                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form check_mlast_at_t001k_mlasv changing obj_to_correct type correct.
 if obj_to_correct-mlmaa = 'X'.
  if t_t001k-mlasv = 'X'.
    if not obj_to_correct-mlast = t_t001k-mlast.
      obj_to_correct-errorflag = 'X'.
      obj_to_correct-error3 = 'X'.
      <tablec>-error_3 = <tablec>-error_3 + 1.
    endif.
  endif.
 endif.
endform.

*---------------------------------------------------------------------*
*       FORM check_vprsv_at_mlast                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form check_vprsv_at_mlast changing obj_to_correct type correct.

  if ( obj_to_correct-vprsv = 'V' ) and
     ( obj_to_correct-mlast = '3' ) and
     ( ( not obj_to_correct-bwtar is initial ) or "not blank segment
     ( obj_to_correct-bwtty is initial     ) ).   "with split valuation
    select single * from ckmlpp
      where kalnr = obj_to_correct-kaln1
      and   bdatj = obj_to_correct-lfgja
      and   poper = obj_to_correct-lfmon
      and   untper = '000'.
    if not ckmlpp-status = '70'.
*     period status is NOT closing entry performed
      obj_to_correct-errorflag = 'X'.
      obj_to_correct-error4 = 'X'.
      <tablec>-error_4 = <tablec>-error_4 + 1.
    endif.
  endif.
endform.
*---------------------------------------------------------------------*
*       FORM check_kalnr_at_mlmaa                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form check_kalnr_at_mlmaa changing obj_to_correct type correct.
  if obj_to_correct-mlmaa = 'X'.
    if obj_to_correct-kaln1 is initial or
     ( active_table = mbew and
       obj_to_correct-kalnr is initial ).
      obj_to_correct-errorflag = 'X'.
      obj_to_correct-error5 = 'X'.
      <tablec>-error_5 = <tablec>-error_5 + 1.
    endif.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM DB_END_COMMIT                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form db_end_commit.
  if i_zahl ne 0.
    perform write_document.
    call function 'DB_COMMIT'.
    clear: i_zahl.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM commit_count                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form commit_count.
* For every 50 corrections, call DB_COMMIT.
  i_zahl = i_zahl + 1.
  if i_zahl ge 50.
    perform write_document.
    call function 'DB_COMMIT'.
    clear: i_zahl.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM write_document                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form write_document.
* when changes are being posted to the DB, post the changed records doc
  call function 'MLHELP_CORRECTION_POST'
       exporting
            i_update_task = ' '
       importing
            e_belnr       = correct_belnr.
  append correct_belnr.
endform.


***********************************************************************
* FORM WRITE_TABLE_HEADER
***********************************************************************
form write_table_header.
  if p_nolist is initial.
    if not active_table is initial.
      write: / text-020,
               active_table.
      write: / text-013,               "MATNR
               text-014,               "BWKEY
               text-015.               "BWTAR
      case active_table.
        when 'EBEW'.
          write: text-021,             "SSID
                 text-022,             "VBELN
                 text-023.             "ITMNo
        when 'OBEW'.
          write: text-021,             "SSID
                 text-024.             "VenNo
        when 'QBEW'.
          write: text-021,             "SSID
                 text-025.             "PSPNR
      endcase.
      write:   text-016,               "MLMAA old
               text-017,               "MLMAA new
               text-018,               "MLAST old
               text-019.               "MLAST new
      hide ausgz.
      uline.
      hide ausgz.
    endif.
  endif.
endform.



*---------------------------------------------------------------------*
*       FORM WRITE_MBEW_Key                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form write_mbew_key using obj_to_correct type correct.
  write: /(18) obj_to_correct-matnr,
          (8)  obj_to_correct-bwkey,
          (10) obj_to_correct-bwtar.
  hide ausgz.
endform.

*---------------------------------------------------------------------*
*       FORM WRITE_EBEW_Key                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form write_ebew_key using obj_to_correct type correct.
  write: /(18) obj_to_correct-matnr,
          (8)  obj_to_correct-bwkey,
          (10) obj_to_correct-bwtar,
          (5)  obj_to_correct-sobkz, '    ',
          (10) obj_to_correct-vbeln,
          (6)  obj_to_correct-posnr.
  hide ausgz.
endform.

*---------------------------------------------------------------------*
*       FORM WRITE_OBEW_Key                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form write_obew_key using obj_to_correct type correct.
  write: /(18) obj_to_correct-matnr,
          (8)  obj_to_correct-bwkey,
          (10) obj_to_correct-bwtar,
          (5)  obj_to_correct-sobkz, '    ',
          (10) obj_to_correct-lifnr.
  hide ausgz.
endform.

*---------------------------------------------------------------------*
*       FORM WRITE_QBEW_Key                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  OBJ_TO_CORRECT                                                *
*---------------------------------------------------------------------*
form write_qbew_key using obj_to_correct type correct.
  write: /(18) obj_to_correct-matnr,
          (8)  obj_to_correct-bwkey,
          (10) obj_to_correct-bwtar,
          (5)  obj_to_correct-sobkz, '    ',
          (8)  obj_to_correct-pspnr.
  hide ausgz.
endform.


*---------------------------------------------------------------------*
*       FORM write_statistics                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form write_statistics.
  free active_table.
  new-page.
  if p_test is initial.
    write: / text-001.
*     write: / 'Update run'
  else.
    write: / text-002.
*     write: / 'Test run'
  endif.
  uline.
  skip 1.

  write: /
  'TABLE', 10 'CHECKED', 30 'ERROR 1', 46 'ERROR 2', 58 'ERROR 3',
           70 'ERROR 4', 82 'ERROR 5',
  /26 'SET', 38 'DEL', 46 '(corr.)'.

  uline.
  hide ausgz.
  assign mbewc to <tablec>.
  perform write_stat_line.
  assign ebewc to <tablec>.
  perform write_stat_line.
  assign obewc to <tablec>.
  perform write_stat_line.
  assign qbewc to <tablec>.
  perform write_stat_line.
  uline.
  hide ausgz.
  assign totalc to <tablec>.
  perform write_stat_line.
  skip.
  write: / text-032.
*          'CHECKED:  total records checked (not changed!) per table'
  if p_test is initial.
    write: / text-010,
*     write: 'ERROR 1 -SET: MLMAA flags set'.
           / text-011.
*     write: '        -DEL: MLMAA flags deleted'.
  else.
    write: / text-007,
*     write: 'ERROR 1 -SET: MLMAA flags to be set'.
           / text-008.
*     write: '        -DEL: MLMAA flags to be deleted'.
  endif.                               " end if p_test...

  write: / text-030,
*          'ERROR 2: MLAST initial though MLMAA is set -> MLAST := 2'
         / text-028,
*          'ERROR 3: MLAST not equal to T001K-MLAST though mandatory'.
         / text-031,
*          'ERROR 4: MLAST = '3' though VPRSV = 'V'
         / text-029,
*          'ERROR 5: No cost estimate no. assigned'.
         / text-033.
*          'Corr.:   Errors have been / will be corrected.'.
  if p_test is initial.
    if not correct_belnr[] is initial.
      skip.
      write: / text-036.
      loop at correct_belnr.
        format intensified off.
        write: / correct_belnr, ', '.
        ausgz = 'x'.
        format intensified on.
        hide: correct_belnr, ausgz.
      endloop.
      clear ausgz.
    endif.
  endif.                               " end if p_test...
endform.

*---------------------------------------------------------------------*
*       FORM write_stat_line                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

form write_stat_line.
  if <tablec>-table <> 'TOTAL'.
    <tablec>-mlast_change = <tablec>-error_2 + <tablec>-error_4.
    add <tablec>-rec_checked to totalc-rec_checked.
    add <tablec>-mlmaa_set   to totalc-mlmaa_set.
    add <tablec>-mlmaa_del   to totalc-mlmaa_del.
    add <tablec>-error_2     to totalc-error_2.
    add <tablec>-error_3     to totalc-error_3.
    add <tablec>-error_4     to totalc-error_4.
    add <tablec>-error_5     to totalc-error_5.
  endif.
  write: / <tablec>-table, <tablec>-rec_checked, <tablec>-mlmaa_set,
           <tablec>-mlmaa_del, <tablec>-error_2, <tablec>-error_3,
           <tablec>-error_4, <tablec>-error_5.
  hide ausgz.
endform.

*---------------------------------------------------------------------*
*       FORM POPUP_DIALOG                                   "PUE281200*
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form popup_dialog using    default type c
                           text1   type c
                           text2   type c
                           title   type c
                  changing answer  type c.
  if sy-batch is initial.
    call function 'POPUP_TO_CONFIRM_STEP'
         exporting
              defaultoption = default
              textline1     = text1
              textline2     = text2
              titel         = title
         importing
              answer        = answer.
  else.
    answer = default.
    write: / 'Popup:', title, text1, text2, 'Answer = ', answer.
  endif.
endform.
*&---------------------------------------------------------------------*
*&      Form  write_key
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBJ_TO_CORRECT  text
*      -->P_ACTIVE_TABLE  text
*----------------------------------------------------------------------*
form write_key using    p_obj_to_correct type correct
                        p_active_table   like active_table.

   case p_active_table.
     when 'MBEW'.
       perform write_mbew_key using p_obj_to_correct.
     when 'EBEW'.
       perform write_ebew_key using p_obj_to_correct.
     when 'QBEW'.
       perform write_qbew_key using p_obj_to_correct.
     when 'OBEW'.
       perform write_obew_key using p_obj_to_correct.
     when others.
   endcase.

endform.                    " write_key
