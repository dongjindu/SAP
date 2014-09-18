*----------------------------------------------------------------------*
*   YAPP803L_ZTPPWOSUM_MNTNC_F01
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  set_field_wc
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - arbpl
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_nation.
  select distinct nation
    into nation_value-key
    from ztpp_wosum.
    append nation_value to nation_list.
  endselect.
endform.                    " set_field_wc
*&---------------------------------------------------------------------*
*&      Form  set_field_ck
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - bf_usage
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_dealer.
  select distinct dealer
    into dealer_value-key
    from ztpp_wosum.
    append dealer_value to dealer_list.
  endselect.
endform.                    " set_field_ck
*&---------------------------------------------------------------------*
*&      Form  set_field_sa
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - prvbe
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_extc.
  select distinct extc
    into extc_value-key
    from ztpp_wosum.
    append extc_value to extc_list.
  endselect.
endform.                    " set_field_sa
*&---------------------------------------------------------------------*
*&      Form  set_field_ss
*&---------------------------------------------------------------------*
*       Getting Data For Dropdown ListBox - rp_point
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_intc.
*DATA: P_NATION(08),  "Work Center(arbpl)
*      P_DEALER(01),  "Control Key(bf_usage)
*      P_EXTC(10),  "Supply Area(prvbe)
*      P_INTC(02),  "Sort String(rp_point)
*      p_bf(20).  "Backflush point(usr01)
  select distinct intc
    into intc_value-key
    from ztpp_wosum.
    append intc_value to intc_list.
  endselect.
endform.                    " set_field_ss
*&---------------------------------------------------------------------*
*&      Form  call_function_vrm
*&---------------------------------------------------------------------*
*       Calling a Func. For DropDown ListBox
*----------------------------------------------------------------------*
*      -->P_BF_LIST  text
*----------------------------------------------------------------------*
form call_function_vrm using    p_list.
  call function 'VRM_SET_VALUES'
       exporting
            id     = name
            values = p_list.
endform.                    " call_function_vrm
*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       Searching Data From ZTPP_WOSUM
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form search_data.
data: l_wo_ser(10), "Work Order
      l_nation(04),  "Nation
      l_dealer(03),  "Dealer
      l_extc(04),  "External Color
      l_intc(04).  "Internal Color
  concatenate p_wo_ser '%'
    into l_wo_ser.
  concatenate p_nation '%'
    into l_nation.
  concatenate p_dealer '%'
    into l_dealer.
  concatenate p_extc '%'
    into l_extc.
  concatenate p_intc '%'
    into l_intc.
  select *
    into corresponding fields of table it_app803
    from ztpp_wosum
    where wo_ser like l_wo_ser and
          nation like l_nation and
          dealer like l_dealer and
          extc like l_extc and
          intc like l_intc .

  sort it_app803 by wo_ser nation dealer extc intc .
  tc_app803-top_line = 1.
endform.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  insert_new_line
*&---------------------------------------------------------------------*
*       Calculation of Line of Internal Table & Table Control's
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form insert_new_line.
  data: l_curr_line type i,
        l_data_lines type i.
* Checking Authority.
  perform check_authority  using wa_flag.

  check wa_flag = space .
* Reading Table Control's Current Line on the Screen
  get cursor line l_curr_line.
  if sy-subrc <> 0.
    message s000 with 'Set the cursor correctly!!!'.
    exit.
  endif.
* Calculating The Current Line in the Internal Table
  l_curr_line = l_curr_line + tc_app803-top_line - 1.
* Getting The Number of Data of Internal Table
  describe table it_app803 lines l_data_lines.
  "If There isn't data in The Internal Table
  if l_data_lines = 0.
    clear it_app803.
    it_app803-new = 'X'.
    it_app803-mark = 'X'.
    append it_app803.
    "If There is data in The Internal Table
  else.
*   Inserting a New Line Into The Internal Table at The Current Line
*   with Check Fields - New, Mark
    insert initial line into it_app803 index l_curr_line.
    clear it_app803.
    it_app803-new = 'X'.
    it_app803-mark = 'X'.
    modify it_app803 index l_curr_line.
*   Deleting a Mark of The Previous Record of Internal Table
    l_curr_line = l_curr_line + 1.
    read table it_app803 index l_curr_line.
    clear it_app803-mark.
    modify it_app803 index l_curr_line.

  endif.
* Calculating The Current Line on the Screen to Set Curror Field
  wa_current_line = l_curr_line - tc_app803-top_line .

endform.                    " insert_new_line
*&---------------------------------------------------------------------*
*&      Form  delete_data
*&---------------------------------------------------------------------*
*       The Process of Data Deletion
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_data.
  data: l_count type i.

* Checking Authority.
  perform check_authority  using wa_flag.

  check wa_flag = space .

* Reading The Number of The Internal Table's Data
  describe table it_app803 lines l_count.
  if l_count < 1.
    message s000 with 'There is no data to delete!!!'.
    exit.
  endif.
* Getting The Table Control's Current Line
  get cursor line l_count.
  if sy-subrc <> 0.
    message s000 with 'Set Cursor Correctly!!'.
  endif.
* Copying Data To be Deleted into IT_DEL.
  loop at it_app803 where  mark = 'X'.
    move-corresponding it_app803 to it_del .
    append it_del.
*   Deleting Data to be deleted from IT_APP803.
    delete it_app803.
  endloop.
endform.                    " delete_data
*&---------------------------------------------------------------------*
*&      Form  mark_upd_flag
*&---------------------------------------------------------------------*
*       The Process of Update
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form mark_upd_flag.
  data l_line type sy-tabix.

* Checking Authority.
  perform check_authority  using wa_flag.

  check wa_flag = space .

  read table it_app803 with key mark = 'X'.
  l_line = sy-tabix.
  if sy-subrc = 0.

*data: L_WO_SER(10), "Work Order
*      L_NATION(04),  "Nation
*      L_DEALER(03),  "Dealer
*      L_EXTC(04),  "External Color
*      L_INTC(04).  "Internal Color

*   First: Check If there is data in IT_UPD with IT_APP's key fields .
    read table it_upd with key wo_ser = it_app803-wo_ser
                               nation = it_app803-nation
                               dealer = it_app803-dealer
                               extc   = it_app803-extc
                               intc   = it_app803-intc .
    if sy-subrc <> 0.
*     If there is no same data in IT_UPD,
*     Second: Check ZTPP_WOSUM with IT_APP's Key Fields .
      clear it_upd.
      select single *
        into corresponding fields of it_upd
        from ztpp_wosum
        where wo_ser = it_app803-wo_ser and
              nation = it_app803-nation and
              dealer = it_app803-dealer and
              extc   = it_app803-extc   and
              intc   = it_app803-intc .
      if sy-subrc = 0.
*       If there is a same data in ZTPP_WOSUM,
*       Mark The Update Flag in IT_APP803.
        append it_upd .
        it_app803-upd = 'X'.
        modify it_app803 index l_line .
      endif.
    endif.
  else.
    message s000 with 'There is no data to update!!'.
  endif.
endform.                    " mark_upd_flag
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       Data Deletion of ZTPP_WOSUM with The Internal Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_table.
  data: l_text(20).

* Checking Authority.
  perform check_authority  using wa_flag.

  check wa_flag = space .
  commit work.
* Data Deletion
  delete ztpp_wosum from table it_del .
  if sy-subrc <> 0.
    rollback work.
    message s000 with 'Deletion Process Failed!!!'.
  else.
    message s000 with 'Deletion Process has been done.'.
  endif.
* Data Update and Insert
  loop at it_app803 where new = 'X' or upd = 'X' .
    clear ztpp_wosum.
    if it_app803-upd = 'X'.  "For Update
      it_app803-aedat = sy-datum .
      it_app803-aezet = sy-uzeit .
      it_app803-aenam = sy-uname .

      update ztpp_wosum from it_app803 .
    else.                 "For Insert
      it_app803-erdat = sy-datum .
      it_app803-erzet = sy-uzeit .
      it_app803-ernam = sy-uname .
      it_app803-aedat = sy-datum .
      it_app803-aezet = sy-uzeit .
      it_app803-aenam = sy-uname .

      insert into ztpp_wosum values it_app803.
    endif.
    if sy-subrc <> 0.
      rollback work.
      concatenate it_app803-wo_ser
                  it_app803-nation
                  it_app803-dealer
                  it_app803-extc
                  it_app803-intc
        into l_text.
      message s001 with l_text
                        ' : Updating ZTPP_WOSUM Failed!!!'.
    else.
      commit work.
      concatenate it_app803-wo_ser
                  it_app803-nation
                  it_app803-dealer
                  it_app803-extc
                  it_app803-intc
        into l_text.
      message s001 with l_text
                    ' : Updating ZTPP_WOSUM has been well done.'.
    endif.
  endloop.
endform.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  sort_ascending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_ascending.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app803 ascending by (field_name01).
  endif.

endform.                    " sort_ascending
*&---------------------------------------------------------------------*
*&      Form  sort_descending
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form sort_descending.
  data: field_name01(40),
        offset01 type i.
*
  get cursor field field_name01.
*
  if field_name01(06) = 'IT_APP'.
    search field_name01 for '-'.
    offset01 = sy-fdpos + 1.
    field_name01 = field_name01+offset01.
    sort it_app803 descending by (field_name01).
  endif.

endform.                    " sort_descending
*&---------------------------------------------------------------------*
*&      Form  set_field_ID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form set_field_wo_ser.
  select distinct wo_ser
    into wo_ser_value-key
    from ztpp_wosum.
    append wo_ser_value to wo_ser_list.
  endselect.

endform.                    " set_field_ID
*&---------------------------------------------------------------------*
*&      Form  popup_positionieren
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form popup_positionieren.
  data: l_key(23).

  call function 'TABLE_GET_KEY_TO_SET_CUR_ROW'
       exporting
            table             = 'ZTPP_WOSUM'        "Subviews
            f4_programname    = ' '
            f4_formname       = ' '
       importing
            table_key         = l_key
*       TABLES
*            sellist           = <vim_ck_sellist>
*            exclude_fields    = excl_pos_tab
       exceptions
            cancelled_by_user = 01
            table_not_found   = 02.

    read table it_app803 with key l_key binary search
                                        transporting no fields.

    tc_app803-top_line = sy-tabix.

endform.                    " popup_positionieren

*&---------------------------------------------------------------------*
*&      Form  check_authority
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FLAG  text
*----------------------------------------------------------------------*
form check_authority using    pa_flag.
  clear: pa_flag.
* message w000 with text-001.
endform.                    " check_authority
