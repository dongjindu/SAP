************************************************************************
* Program Name      : ZACO03U_MH_UPLOAD
* Author            : Hyesun , Jung
* Creation Date     : 2006.12.05.
* Specifications By : Andy Choi
* Description       : Upload using Excel(DAT) M/H master data and Save
*                     ZTCO_MHA, ZTCO_MHAD, ZTCO_MHAT
*                     Excel file type(=ZTCO_MHAD)
************************************************************************


report zaco03u_mh_upload message-id zmco.

tables: ztco_mha.

* Main internal table
data : begin of it_upload occurs 0,
        pernr like ztco_mhad-pernr,
        kostl(15),
        lgart(1),
        anz01(15),
        anz02(15),
        anz03(15),
        anz04(15),
        anz05(15),
        anz06(15),
        anz07(15),
        anz08(15),
        anz09(15),
        anz10(15),
        anz11(15),
        anz12(15),
        anz13(15),
        anz14(15),
        anz15(15),
        anz16(15),
        anz17(15),
        anz18(15),
        anz19(15),
        anz20(15),
        anz21(15),
        anz22(15),
        anz23(15),
        anz24(15),
        anz25(15),
        anz26(15),
        anz27(15),
        anz28(15),
        anz29(15),
        anz30(15),
        anz31(15).
data : end of  it_upload.


data : it_mhad_temp like ztco_mhad occurs 0 with header line,
       it_mhad      like ztco_mhad occurs 0 with header line,
       it_mha       like ztco_mha  occurs 0 with header line,
       it_mhat      like ztco_mhat occurs 0 with header line.

data : begin of  it_error occurs 0,
        pernr like ztco_mha-pernr,
       end of it_error.

*--- ALV
type-pools: slis.
data : w_fieldcat type slis_t_fieldcat_alv with header line,
       w_eventcat type slis_t_event with header line,
       w_selfield type slis_selfield,
       w_sortcat  type slis_t_sortinfo_alv with header line,
       w_col_pos  type i,
       w_program  like sy-repid,
       w_top_of_page type slis_t_listheader,
       w_line1 type slis_listheader.

data: gt_fieldcat type slis_t_fieldcat_alv,
      gs_layout   type slis_layout_alv,
      gt_sp_group type slis_t_sp_group_alv,
      gt_events   type slis_t_event,
      gt_sorts    type slis_t_sortinfo_alv with header line,
      gs_prnt     type slis_print_alv,
      g_repid     like sy-repid.
*---- ALV

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
selection-screen begin of block b1 with frame title text-100.
parameters :  p_kokrs like csks-kokrs memory id cac obligatory,
              p_gjahr like cobk-gjahr memory id gjr obligatory,
              p_perid like cosp-perbl memory id bpe obligatory,
              p_trun(1) default 'X'.
selection-screen end   of block b1.

parameters: p_file  like rlgrap-filename obligatory
                    default 'c:\temp\mhu.txt'.

at selection-screen on value-request for p_file.
  perform at_sel_screen_on_value_request using p_file 'O'.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
start-of-selection.

* Upload file
  perform upload_file.

* read table ztco_mhad and get cost center ...etc
  perform select_ztco_mhad.

* Create update internal table (it_mhad)
  perform create_update_tab.

* Make it_mha, it_mhat using it_mhad
  perform move_ztco_mha_mhat.

  if not it_error[] is initial .
    perform write_error.
  endif.

  if p_trun = 'X'.
    perform display_list.
  else.
* Update
    perform update_ztco_mha_mhad_mhat.
  endif.
*&---------------------------------------------------------------------*
*&      Form  upload_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form upload_file.
  call function 'WS_UPLOAD'
       exporting
            codepage                = ' '
            filename                = p_file
            filetype                = 'DAT'
*           HEADLEN                 = ' '
*           LINE_EXIT               = ' '
*           TRUNCLEN                = ' '
*           USER_FORM               = ' '
*           USER_PROG               = ' '
*      IMPORTING
*           FILELENGTH              =
       tables
            data_tab                = it_upload
      exceptions
           conversion_error        = 1
           file_open_error         = 2
           file_read_error         = 3
           invalid_table_width     = 4
           invalid_type            = 5
           no_batch                = 6
           unknown_error           = 7
           gui_refuse_filetransfer = 8
           customer_error          = 9
           others                  = 10
            .
  case sy-subrc.
    when 0.
      data l_text(132).
      concatenate p_file ' is loaded'
                  into l_text.
      message s000 with l_text.
    when others.
      message e000 with 'Error during file upload'.
  endcase.

endform.                    " upload_file
*&---------------------------------------------------------------------*
*&      Form  select_ztco_mhad
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form select_ztco_mhad.
  select * into corresponding fields of table  it_mhad_temp
    from ztco_mhad
     for all entries in it_upload
   where kokrs   = p_kokrs
     and gjahr   = p_gjahr
     and perid   = p_perid
     and pernr   = it_upload-pernr
     and empct <> ''
     and ( lgart = '1' or lgart = '2' ).

endform.                    " select_ztco_mhad
*&---------------------------------------------------------------------*
*&      Form  create_update_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form create_update_tab.
  loop at it_upload.
    clear it_mhad_temp.
    read table it_mhad_temp with key pernr = it_upload-pernr.
    if sy-subrc <> 0 .
      it_error-pernr = it_upload-pernr.
      append it_error. clear it_error.
      exit.
    else.
      perform move_first_line.
      perform move_pair_line.
    endif.
  endloop.
endform.                    " create_update_tab
*&---------------------------------------------------------------------*
*&      Form  move_first_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_first_line.
  data : l_cnt(2) type n.

  move-corresponding it_mhad_temp to it_mhad.
  move-corresponding it_upload to it_mhad.
  it_mhad-kostl   = it_upload-kostl.
  it_mhad-skostl  = it_mhad_temp-kostl.
  it_mhad-chnge   = 'M'.
  it_mhad-lgart   = '3'.
  if it_upload-lgart = '1'.
    it_mhad-lgart2 = '1055'.
  else.
    it_mhad-lgart2 = '1056'.
  endif.

  clear : l_cnt, it_mhad-anzhl.
  do 31 times.
    l_cnt = l_cnt + 1.
    perform calcualte_total using l_cnt.
  enddo.

  collect it_mhad. clear it_mhad.

endform.                    " move_first_line


*&---------------------------------------------------------------------*
*&      Form  move_pair_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_pair_line.
  data : l_cnt(2) type n.
  move-corresponding it_mhad_temp to it_mhad.
  move-corresponding it_upload to it_mhad.
  clear : l_cnt, it_mhad-anzhl.
  do 31 times.
    l_cnt = l_cnt + 1.
    perform calculate_by_minus using l_cnt.
    perform calcualte_total    using l_cnt.
  enddo.


  it_mhad-kostl   = it_mhad_temp-kostl.
  it_mhad-skostl  = it_upload-kostl.
  it_mhad-lgart   = '3'.
  it_mhad-chnge   = 'M'.
  if it_upload-lgart = '1'.
    it_mhad-lgart2 = '1055'.
  else.
    it_mhad-lgart2 = '1056'.
  endif.
  collect it_mhad. clear it_mhad.
endform.                    " move_pair_line
*&---------------------------------------------------------------------*
*&      Form  calculate_by_minus
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_CNT  text
*----------------------------------------------------------------------*
form calculate_by_minus using    p_cnt.
  field-symbols: <f_field> .
  data : l_field(15).

  concatenate 'IT_MHAD-ANZ' p_cnt into l_field.
  assign (l_field) to  <f_field> .
  <f_field>  =  <f_field> * -1.

endform.                    " calculate_by_minus
*&---------------------------------------------------------------------*
*&      Form  MOVE_ZTCO_MHA_MHAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form move_ztco_mha_mhat.
  data: l_uzeit like syst-uzeit.
  l_uzeit = sy-uzeit.
  loop at it_mhad.
    move-corresponding it_mhad to it_mha.
    it_mha-srkostl = it_mhad-skostl.
    it_mha-erdat = sy-datum.
    it_mha-erzet = l_uzeit.
    it_mha-ernam = sy-uname.

    select single mosid into ztco_mha-mosid from ztco_mha
       where kokrs = p_kokrs
         and gjahr = p_gjahr
         and perid = p_perid
         and pernr = it_mhad-pernr
         and lgart = '1'.
    it_mha-mosid = ztco_mha-mosid.

    collect it_mha. clear it_mha.
    move-corresponding it_mhad to it_mhat.
    collect it_mhat. clear it_mhat.
  endloop.
endform.                    " MOVE_ZTCO_MHA_MHAT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_MHA_MHAD_MHAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form update_ztco_mha_mhad_mhat.

  delete from ztco_mha   where kokrs = p_kokrs
                           and gjahr = p_gjahr
                           and perid = p_perid
                           and lgart = '3'.

  delete from ztco_mhad  where kokrs = p_kokrs
                           and gjahr = p_gjahr
                           and perid = p_perid
                           and lgart = '3'.

  delete from ztco_mhat  where kokrs = p_kokrs
                           and gjahr = p_gjahr
                           and perid = p_perid
                           and lgart = '3'.

  modify ztco_mha from table it_mha.
  if sy-subrc = 0 .
    commit work.
    message s009.
  endif.

  modify ztco_mhad from table it_mhad.
  if sy-subrc = 0 .
    commit work.
    message s009.
  endif.

  modify ztco_mhat from table it_mhat.
  if sy-subrc = 0 .
    commit work.
    message s009.
  endif.


endform.                    " UPDATE_ZTCO_MHA_MHAD_MHAT
*&---------------------------------------------------------------------*
*&      Form  at_sel_screen_on_value_request
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*      -->P_0058   text
*----------------------------------------------------------------------*
form at_sel_screen_on_value_request using def_path like rlgrap-filename
                                          mode     type c.

  data: tmp_filename like rlgrap-filename.
  data: tmp_mask(80).                  " LIKE GLOBAL_FILEMASK_ALL.
  data: fieldln type i.
  field-symbols: <tmp_sym>.

* Build Filter for Fileselektor

*  IF GLOBAL_FILEMASK_MASK IS INITIAL.
  tmp_mask = ',*.*,*.*.'.
*  ELSE.
*    TMP_MASK = ','.
*    WRITE GLOBAL_FILEMASK_TEXT TO TMP_MASK+1.
*    WRITE ',' TO TMP_MASK+21.
*    WRITE GLOBAL_FILEMASK_MASK TO TMP_MASK+22.
*    WRITE '.' TO TMP_MASK+42.
*    CONDENSE TMP_MASK NO-GAPS.
*  ENDIF.

*  IF NOT GLOBAL_FILEMASK_ALL IS INITIAL.
*    TMP_MASK = GLOBAL_FILEMASK_ALL.
*  ENDIF.
*
  fieldln = strlen( def_path ) - 1.
  assign def_path+fieldln(1) to <tmp_sym>.
  if <tmp_sym> = '/' or <tmp_sym> = '\'.
    clear <tmp_sym>.
  endif.

  call function 'WS_FILENAME_GET'
       exporting
            def_filename     = p_file
            def_path         = def_path
*           MASK             = ',*.*,*.*.'
            mask             = tmp_mask
            mode             = mode
*           TITLE            = ' '
       importing
            filename         = tmp_filename
*         RC               =
       exceptions
            inv_winsys       = 01
            no_batch         = 02
            selection_cancel = 03
            selection_error  = 04.

  if sy-subrc = 0.
    p_file = tmp_filename.
  else.
* IF SY-SUBRC = 01.    "// Does not work, why ???
*   MESSAGELINE = 'Not supported'.
* ENDIF.
  endif.
endform.                    " at_sel_screen_on_value_request
*&---------------------------------------------------------------------*
*&      Form  calcualte_total
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_MHAD_ANZHL  text
*----------------------------------------------------------------------*
form calcualte_total using p_cnt.

  data : l_field(15).
  field-symbols: <f_field> .

  concatenate 'IT_MHAD-ANZ' p_cnt into l_field.
  assign (l_field) to  <f_field> .
  it_mhad-anzhl = it_mhad-anzhl + <f_field>.

endform.                    " calcualte_total
*&---------------------------------------------------------------------*
*&      Form  write_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_error.
  write : '>>> Error list <No data in ZTCO_MHAD LGTAR type 1 or 2) <<<'.
  skip.
  loop at it_error.
    write : / it_error-pernr.
  endloop.
endform.                    " write_error
*&---------------------------------------------------------------------*
*&      Form  display_list
*&---------------------------------------------------------------------*
form display_list.

  perform field_setting tables gt_fieldcat using :
   'PERNR'    'Person'          '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'KOSTL'    'SendCC'          '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'LGART'    'Wtype'           '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'DTYPE'    'Dtype'           '02' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'SCHKZ'    'WkSch'           '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'MOSID'    'EmpGrp'          '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'JOBCD'    'JobCd'           '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'SRKOSTL'  'RecvCC'          '10' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'CHNGE'    'Chg'             '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'ANZSH'    'V.Cls'           '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'EMPCT'    'EmpCat'          '04' 'X' 'L'  ' '  ' '  ' '  ' '  ' ',
   'ANZHL'    'Hours'           '14' ' ' 'R'  ' '  ' '  ' '  ' '  'X'.

  g_repid = sy-repid.

  call function 'REUSE_ALV_GRID_DISPLAY'
       exporting
            i_callback_program = g_repid
            it_fieldcat        = gt_fieldcat
            i_save             = 'A'
       tables
            t_outtab           = it_mha
       exceptions
            program_error      = 1
            others             = 2.

endform.                    " display_list
*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
form field_setting tables p_fieldcat_t like gt_fieldcat using
                                  p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  data: ls_fieldcat type slis_fieldcat_alv.
  clear ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  append ls_fieldcat to gt_fieldcat.

endform.                    " fill_field_category
*ALV
