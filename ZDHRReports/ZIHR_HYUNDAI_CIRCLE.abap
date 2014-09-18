*&----------------------------------------------------------------------
*& Development ID : ZIHR_HYUNDAI_CIRCLE
*& Program ID     : ZAHRU01D
*& Program Name   : [HR] Send employee info to HMA for Hyundai Circle
*& Created by     : Furong
*& Created on     : 08/28/2013
*& Reference Pgm  :
*&
*& Modification Log
*& Date        Developer Issue No  Description
*&======================================================================
*&
*
*&----------------------------------------------------------------------

REPORT  zihr_hyundai_circlet MESSAGE-ID zmfi..

TYPE-POOLS: slis.

TABLES: pa0006.

*- Output Structure
DATA: it_out LIKE TABLE OF zshr_hyundaicircle WITH HEADER LINE.
*DATA: BEGIN OF it_out OCCURS 0,
*         EMPNO(10),
*         FIRST_NAME(25),
*         MID_NAME(40),
*         LAST_NAME(25),
*         address1(35),
*         address2(35),
*         city(20),
*         state_id(2),
*         zipcode(6),
*         email(100),
*         phone1(20),
*         phone2(20),
*         fax(20),
*      END OF it_out.

DATA:  BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zshr_hyundaicircle.
DATA: stat2 LIKE pa0000-stat2.
DATA: END OF it_data.

*DATA : BEGIN OF it_data OCCURS 0,
*         pernr                 TYPE pa0000-pernr,
*         vorna                 TYPE pa0002-vorna,
*         midnm                 TYPE pa0002-midnm,
*         nachn                 TYPE pa0002-nachn,
*         address1(35),
*         address2(35),
*         city(20),
*         state_id(2),
*         zipcode(6),
*         email                 TYPE pa0105-usrid_long,
*         phone1                TYPE pa0006-num02,
*         phone2                TYPE pa0006-num02,
*         fax(20),
*       END OF it_data.

*DATA : BEGIN OF it_file_dlmtd OCCURS 0,
*         record(500) TYPE c,
*       END OF it_file_dlmtd.

DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_fi  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldcat_co  TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE,
       it_fieldcat_det TYPE lvc_t_fcat WITH HEADER LINE. "/Detail

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant.      "for parameter IS_VARIANT

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      g_docking_container    TYPE REF TO cl_gui_docking_container.

DATA: ok_code LIKE sy-ucomm,
      w_cnt TYPE i,
      w_repid  LIKE sy-repid,
      w_dynnr LIKE sy-dynnr.

*DATA: v_file      TYPE string,
*      v_ans       TYPE i.
*
*FIELD-SYMBOLS: <fs> TYPE any.
*
*CONSTANTS: c_dlmtd(1)  TYPE c VALUE ',',
*           c_dfile(30) TYPE c VALUE 'Hyundai_circle.csv'.


SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr FOR it_data-empno.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X' USER-COMMAND chck,
*            p_actv AS CHECKBOX,
            p_kdate TYPE datum DEFAULT sy-datum OBLIGATORY.

SELECTION-SCREEN ULINE MODIF ID chk.
PARAMETERS: p_rfcdes TYPE rfcdest
                     DEFAULT 'WMHR01'
                     MATCHCODE OBJECT vers_rfcdest_sh
                     MODIF ID chk.
SELECTION-SCREEN END OF BLOCK blk1.

*selection-screen begin of block blk2 with frame title text-t02.
*parameters: p_pres radiobutton group serv user-command rad
*                                        modif id chk default 'X',
*            p_appl radiobutton group serv modif id chk.
*selection-screen skip.
*parameters: p_file(1024) type c lower case modif id chk
*                                visible length 45.
*selection-screen end of block blk2.


INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'CHK'.
      IF NOT p_test IS INITIAL.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  CHECK sy-ucomm NE 'CHCK'.
  IF p_test IS INITIAL AND
     p_rfcdes IS INITIAL.
    MESSAGE e208(00) WITH 'Please Enter RFC Destination'.
  ENDIF.

*at selection-screen output.
*  loop at screen.
*    if screen-group1 = 'CHK'.
*      if not p_test is initial.
*        screen-active = 0.
*        modify screen.
*      endif.
*    endif.
*  endloop.
*
*at selection-screen.
*  check sy-ucomm ne 'CHCK'.
*  if p_test is initial and
*     p_file is initial.
*    message e000 with 'Please Enter Filename'.
*  endif.
*
*  v_file = p_file.
*
*at selection-screen on value-request for p_file.
*  if not p_pres is initial.
*    perform browser changing p_file v_ans.
*  else.
*    perform display_unix changing p_file v_ans.
*  endif.

START-OF-SELECTION.
  PERFORM select_data.
  PERFORM modify_data.
  CHECK NOT it_data[] IS INITIAL.

  IF p_test IS INITIAL.
    PERFORM transfer_data.
  ELSE.
    PERFORM display_data.
  ENDIF.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .
  SELECT  b~pernr AS empno b~vorna AS first_name
          b~midnm AS mid_name b~nachn AS last_name
          c~stat2
   INTO CORRESPONDING FIELDS OF TABLE it_data
    FROM pa0002 AS b
       INNER JOIN pa0000 AS c
          ON b~pernr  =  c~pernr
       INNER JOIN pa0001 AS d
          ON c~pernr  =  d~pernr
  WHERE  b~endda GE p_kdate
    AND  b~begda LE p_kdate
    AND  c~endda GE p_kdate
    AND  c~begda LE p_kdate
    AND  d~endda GE p_kdate
    AND  d~begda LE p_kdate
    AND  b~pernr IN s_pernr
**  Furong on 07/31/14
    AND  c~stat2 <> '0'
    and  d~PERSG <> '4'.
** )
  SORT it_data BY empno.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .
  DATA: l_rec TYPE i,
        l_com TYPE pa0006-com01,
        l_num TYPE pa0006-num01.

  DESCRIBE TABLE it_data LINES l_rec.

  LOOP AT it_data.
* Show progress indicator.
    PERFORM show_progress USING 'Processing Data...' l_rec.

* Get Telephone Number
    SELECT *
      FROM pa0006
     UP TO 1 ROWS
     WHERE pernr = it_data-empno
       AND subty = '1'
       AND endda GE p_kdate
       AND begda LE p_kdate.
    ENDSELECT.

    CLEAR: l_com, l_num.
    DO 6 TIMES VARYING l_com FROM pa0006-com01
                             NEXT pa0006-com02
               VARYING l_num FROM pa0006-num01
                             NEXT pa0006-num02.
      IF l_com = 'WORK'.
        it_data-phone1 = l_num.
        EXIT.
      ENDIF.
    ENDDO.

* Get E-Mail Address
    SELECT usrid_long INTO it_data-email
      FROM pa0105
     UP TO 1 ROWS
     WHERE pernr = it_data-empno
       AND endda GE p_kdate
       AND begda LE p_kdate
       AND usrty = '0010'.
    ENDSELECT.

    MODIFY it_data.
    MOVE-CORRESPONDING it_data TO it_out.
    CONCATENATE 'HMM' it_out-empno+2(6) INTO it_out-empno.
    APPEND it_out.
  ENDLOOP.

  SET LOCALE LANGUAGE sy-langu.

  MESSAGE s368(00) WITH 'Total record(s) extracted:' l_rec.

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  CALL SCREEN 0200.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM transfer_data.
  DATA: l_result TYPE zresult,
        l_msg TYPE zmsg.

  CALL FUNCTION 'ZFHR_HYUNDAI_CIRCLE'
    DESTINATION p_rfcdes
    IMPORTING
      e_result                       = l_result
      e_msg                          = l_msg
    TABLES
      it_data                        = it_out
    EXCEPTIONS
      call_function_destination_no_t = 1
      call_function_no_dest          = 2
      call_function_remote_error     = 3
      rfc_no_authority               = 4
      OTHERS                         = 5.

  IF sy-subrc <> 0.
    MESSAGE e368(00) WITH 'Error when calling RFC destination:'
                           p_rfcdes.
  ELSE.
    IF l_result <> 'S'.
      MESSAGE e368(00) WITH 'RFC Retrun error:'
                           l_msg.
    ELSE.
      MESSAGE s368(00) WITH 'Data has been sent to:' p_rfcdes.
    ENDIF.
  ENDIF.
*  data: l_totrec type i.
*
*  loop at it_out.
*    do.
*      assign component sy-index of structure it_OUT to <fs>.
*      if sy-subrc <> 0.
*        exit.
*      endif.
*
*      if not it_file_dlmtd is initial.
*        concatenate it_file_dlmtd-record c_dlmtd <fs>
*               into it_file_dlmtd-record.
*      else.
*        it_file_dlmtd-record = <fs>.
*      endif.
*    enddo.
*
*    append it_file_dlmtd. clear it_file_dlmtd.
*  endloop.
*
*  describe table it_file_dlmtd lines l_totrec.
*
*  case 'X'.
*    when p_appl.
*      open dataset v_file for output in text mode.
*      if sy-subrc <> 0.
*        message id sy-msgid type sy-msgty number sy-msgno
*              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      endif.
*
*      loop at it_file_dlmtd.
*        transfer it_file_dlmtd to v_file.
*      endloop.
*
*      close dataset v_file.
*
*    when p_pres.
*      if not sy-batch is initial.
*        message s000
*           with 'Writing File to Desktop Not Possible'
*                'in Background Mode' ' ' ' '.
*        exit.
*      endif.
*
*      call function 'GUI_DOWNLOAD'
*        exporting
*          filename                = v_file
*        tables
*          data_tab                = it_file_dlmtd
*        exceptions
*          file_write_error        = 1
*          no_batch                = 2
*          gui_refuse_filetransfer = 3
*          invalid_type            = 4
*          no_authority            = 5
*          unknown_error           = 6
*          header_not_allowed      = 7
*          separator_not_allowed   = 8
*          filesize_not_allowed    = 9
*          header_too_long         = 10
*          dp_error_create         = 11
*          dp_error_send           = 12
*          dp_error_write          = 13
*          unknown_dp_error        = 14
*          access_denied           = 15
*          dp_out_of_memory        = 16
*          disk_full               = 17
*          dp_timeout              = 18
*          file_not_found          = 19
*          dataprovider_exception  = 20
*          control_flush_error     = 21
*          others                  = 22.
*
*      if sy-subrc <> 0.
*        if sy-subrc = 15.
*          message s011 with 'Access Denied'.
*        elseif not sy-msgid is initial.
*          message id sy-msgid type sy-msgty number sy-msgno
*          with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        else.
*          message s011 with 'Error when creating the file'.
*        endif.
*      endif.
*  endcase.
*
*  if sy-subrc eq 0.
*    message s000 with 'File is written to:' v_file l_totrec
*                      'Record(s)'.
*  endif.
ENDFORM.                    " TRANSFER_DATA
*&---------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_&1  text
*      -->P_&2  text
*----------------------------------------------------------------------*
FORM show_progress USING    pf_text
                            pf_val.

  STATICS: l_text(50) TYPE c,
           l_baseval  TYPE i,
           l_percent  TYPE i,
           l_counter  TYPE i.

  IF l_text NE pf_text.
    l_text = pf_text.
    CLEAR: l_baseval,
           l_percent,
           l_counter.
  ENDIF.

  IF NOT l_baseval IS INITIAL.
    l_counter = l_counter - 1.
    CHECK l_counter LE 0.
    l_percent = l_percent + 10.
    CHECK l_percent LE 100.
    l_counter = l_baseval.
  ELSE.
    l_baseval = pf_val DIV 10.
    l_counter = l_baseval.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = l_percent
      text       = pf_text.

ENDFORM.                    " SHOW_PROGRESS
*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PBO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv_200 OUTPUT.
  IF g_docking_container IS INITIAL.
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_OUT'.
    PERFORM assign_itab_to_alv.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  DATA:   l_repid LIKE sy-repid,
          l_dynnr LIKE sy-dynnr.

** O 08/26/13 By Furong
*  CREATE OBJECT grid_container
*          EXPORTING container_name = wa_custom_control
*          EXCEPTIONS
*           cntl_error = 1
*           cntl_system_error = 2
*           create_error = 3
*           lifetime_error = 4
*           lifetime_dynpro_dynpro_link = 5.
  l_repid = sy-repid.
  l_dynnr = sy-dynnr.
  CREATE OBJECT g_docking_container
    EXPORTING
      repid     = l_repid
      dynnr     = l_dynnr
      side      = cl_gui_docking_container=>dock_at_bottom
*     RATIO     = 90
      extension = 2000.
** End on 08/26/13
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = l_repid
        txt2  = sy-subrc
        txt1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT alv_grid
*         EXPORTING i_parent = grid_container
         EXPORTING i_parent = g_docking_container
                   i_appl_events = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  DATA : lw_s_dragdrop TYPE lvc_s_dd01. "/ Drag&Drop control settings

  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
  wa_is_layout-info_fname = 'IF'.
  wa_is_layout-ctab_fname = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_sortcat_display.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 4.
*  it_sort-fieldname      = 'DISPO'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname,
        lw_waers LIKE t001-waers,
        l_rqty(9),
        l_datum(8),
        l_cn(2) TYPE n.

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].

  CLEAR: w_cnt,w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_structure_name   = 'ZSHR_HYUNDAICIRCLE'
*     i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                'S' 'EMPNO'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Employee ID',
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'FIRST_NAME'       '',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'First Name',
                                  'E' 'OUTPUTLEN'   '40',

                                   'S' 'MID_NAME'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Middle Name',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'LAST_NAME'         ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Last Name',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'EMAIL'        ' ',
                                   ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Email',
                                  'E' 'OUTPUTLEN'   '40',

                                 'S' 'PHONE1'        ' ',
                                 ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Phone',
                                  'E' 'OUTPUTLEN'   '20'.

ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
FORM setting_fieldcat TABLES   p_fieldcat STRUCTURE it_fieldcat
                      USING    p_gubun
                               p_field
                               p_value.
  DATA : l_col(40).

  FIELD-SYMBOLS <fs>.

* START - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'S'.
    CLEAR: p_fieldcat.

    READ TABLE it_fieldname INTO w_fieldname
                            WITH KEY fieldname  = p_field.
    IF sy-subrc NE 0.
      MESSAGE e000(zz) WITH 'Check field catalog'.
    ENDIF.

    MOVE: w_fieldname-fieldname TO p_fieldcat-fieldname.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' p_field  INTO l_col.
  ASSIGN (l_col) TO <fs>.
  MOVE   p_value TO <fs>.

* END - FIELD ATTRIBUTE SETTING
  IF p_gubun = 'E'.
    ADD 1 TO w_cnt.
    p_fieldcat-col_pos = w_cnt.
    APPEND p_fieldcat.
  ENDIF.
ENDFORM.                    " setting_fieldcat

*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM assign_itab_to_alv.

  CALL METHOD alv_grid->set_table_for_first_display
    EXPORTING
      is_layout            = wa_is_layout
      i_save               = wa_save
      is_variant           = wa_variant
*     i_default            = space
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_out[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*----------------------------------------------------------------------*
***INCLUDE ZRMM_REQUIREMENT_PLAN_PAIO .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  BROWSER
*&---------------------------------------------------------------------*
*       Browse Desktop/PC Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
*FORM browser CHANGING filename answer.
*  DATA  $filename TYPE string.
*  DATA: l_path  TYPE string,
*        l_fpath TYPE string,
*        l_dfile TYPE string.
*
*  l_dfile = c_dfile.
*
*  CALL METHOD cl_gui_frontend_services=>file_save_dialog
*    EXPORTING
*      window_title      = 'Select File Name'
*      default_extension = 'csv'
*      default_file_name = l_dfile
*      file_filter       = 'CSV (*.csv)|*.csv| All (*.*)|*.*'
**     initial_directory = '\\10.121.233.22\Data'
*    CHANGING
*      filename          = $filename
*      path              = l_path
*      fullpath          = l_fpath
*      user_action       = answer
*    EXCEPTIONS
*      cntl_error        = 1
*      error_no_gui      = 2
*      OTHERS            = 3.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSEIF answer = 0.
*    filename = l_fpath.
*  ELSE.
*    MESSAGE s118(ba).
*  ENDIF.
*
*ENDFORM.                    " BROWSER
*&---------------------------------------------------------------------*
*&      Form  display_unix
*&---------------------------------------------------------------------*
*       Display UNIX Directory
*----------------------------------------------------------------------*
*      <--filename  filename
*      <--answer    return code
*----------------------------------------------------------------------*
*FORM display_unix CHANGING filename answer.
*  DATA: BEGIN OF it_filename OCCURS 0,
*          path(1024) TYPE c,
*        END OF it_filename.
*
*  SELECT dirname
*    FROM user_dir
*    INTO TABLE it_filename
*   WHERE aliass = 'DIR_PERF'.
*
*  CALL 'C_SAPGPARAM' ID 'NAME'  FIELD 'DIR_TEMP'
*                     ID 'VALUE' FIELD it_filename-path.
*  APPEND it_filename.
*
*  CALL FUNCTION 'POPUP_WITH_TABLE'
*    EXPORTING
*      endpos_col   = '100'
*      endpos_row   = '10'
*      startpos_col = '1'
*      startpos_row = '1'
*      titletext    = 'Select UNIX Directory'
*    IMPORTING
*      choice       = filename
*    TABLES
*      valuetab     = it_filename
*    EXCEPTIONS
*      break_off    = 1
*      OTHERS       = 2.
*
*  answer = sy-subrc.
*
*  IF sy-subrc = 0.
*    CONCATENATE filename '/' c_dfile INTO filename.
*  ELSE.
*    MESSAGE s549(fibl).
*  ENDIF.
*ENDFORM.                    " display_unix
