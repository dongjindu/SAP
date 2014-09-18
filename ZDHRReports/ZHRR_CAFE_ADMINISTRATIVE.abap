*&---------------------------------------------------------------------*
*& Report  ZHRR_CAFE_ADMINISTRATIVE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zhrr_cafe_administrative MESSAGE-ID zmfi.

TABLES: pa9100, p9100, *pa9100, pa0000, pa0001, hrp1000, cskt.

TYPE-POOLS: slis.

DATA: BEGIN OF gt_pernr OCCURS 0,
         pernr TYPE pernr_d,
      END   OF gt_pernr.
DATA : BEGIN OF gt_data OCCURS 0.
        INCLUDE STRUCTURE p9100.
DATA :  del               TYPE c.
DATA : END   OF gt_data.

DATA : gt_9100            LIKE TABLE OF p9100 WITH HEADER LINE
     , gs_9100            TYPE p9100
     , gt_0001            LIKE TABLE OF p0001 WITH HEADER LINE
     , gs_0001            TYPE p0001
     .
DATA : gt_fieldcat        TYPE slis_t_fieldcat_alv
     , gs_fieldcat        TYPE slis_fieldcat_alv
     , gt_fc1             TYPE slis_t_fieldcat_alv
     , gt_fc5             TYPE slis_t_fieldcat_alv
     , gs_layout          TYPE slis_layout_alv
     , gt_header          TYPE slis_t_listheader
     , gs_print           TYPE slis_print_alv
     .

DATA : gv_msg(80)         TYPE c
     , gv_flag            TYPE c
     , gv_eligi           TYPE nused2
     .
CONSTANTS:
       gc_2               TYPE c VALUE '2',
       gc_3               TYPE c VALUE '3',
       c_active(1)        TYPE c VALUE 1.

FIELD-SYMBOLS: <fs>       TYPE any.


DATA: BEGIN OF it_file OCCURS 0.
        INCLUDE STRUCTURE zshr_cafe_9100.
DATA: END   OF it_file.
DATA: BEGIN OF it_data OCCURS 0.
        INCLUDE STRUCTURE zshr_cafe_balance.
DATA: END   OF it_data.
DATA: is_file             LIKE LINE OF it_file
    , ws_file             LIKE LINE OF it_file
    , wt_file             LIKE TABLE OF ws_file WITH HEADER LINE
    , gt_sort             TYPE slis_t_sortinfo_alv WITH HEADER LINE
    , gs_pay_result       TYPE pay99_result
    , wa_rt               LIKE pc207 OCCURS 0 WITH HEADER LINE
    , wa_arrrs            LIKE pc22z OCCURS 0 WITH HEADER LINE
    , wa_9100             TYPE p9100
    , wa_begda            TYPE dats
    , wa_endda            TYPE dats
    .
DATA: g_molga             TYPE molga VALUE '10'.        "USA
DATA: gt_rgdir            LIKE pc261 OCCURS 0 WITH HEADER LINE.
DATA: gs_rgdir            LIKE pc261.
DATA: gs_500l             TYPE t500l.
DATA: g_return            TYPE zmms0053.

DATA: it_periods          LIKE t549q OCCURS 1
    , is_periods          LIKE LINE OF it_periods
    , get_pabrj           LIKE t549q-pabrj
    , get_pabrp           LIKE t549q-pabrp
    , lv_last_day         TYPE dats
    , gv_balance          TYPE c
    , gv_betrg            TYPE maxbt
    , gv_pp               LIKE t549q-pabrj
    .

CONSTANTS: insert TYPE pspar-actio VALUE 'INS'.
DATA: ls_return           LIKE bapireturn1
    , lv_key              LIKE bapipakey
    , return              LIKE TABLE OF bapireturn WITH HEADER LINE
    .




*----------------------------------------------------------------------*
*                   --- Selection Screen ---                           *
*----------------------------------------------------------------------*
**SELECTION-SCREEN BEGIN OF BLOCK blk0 WITH FRAME TITLE text-t00.
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: s_pernr   FOR pa0000-pernr NO INTERVALS
              , s_stat2   FOR pa0000-stat2 NO INTERVALS
              , s_bukrs   FOR pa0001-bukrs NO INTERVALS
              , s_werks   FOR pa0001-werks NO INTERVALS
              , s_btrtl   FOR pa0001-btrtl NO INTERVALS
              , s_persg   FOR pa0001-persg NO INTERVALS
              , s_persk   FOR pa0001-persk NO INTERVALS
              , s_abkrs   FOR pa0001-abkrs NO INTERVALS
              , s_kostl   FOR pa0001-kostl NO INTERVALS
              , s_sachz   FOR pa0001-sachz NO INTERVALS
              .

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE text-t02.
PARAMETERS p_ene AS CHECKBOX.
PARAMETERS p_es1 AS CHECKBOX.
PARAMETERS p_es2 AS CHECKBOX.
PARAMETERS p_emr AS CHECKBOX.
PARAMETERS p_not AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK blk2.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE text-t03.
PARAMETERS: p_r1 RADIOBUTTON GROUP act DEFAULT 'X'. "Activity
PARAMETERS: p_r2 RADIOBUTTON GROUP act.             "
PARAMETERS: p_r3 RADIOBUTTON GROUP act.             "
PARAMETERS: p_r4 RADIOBUTTON GROUP act.             "
PARAMETERS: p_r5 RADIOBUTTON GROUP act.             "
SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN END OF BLOCK blk1.



INITIALIZATION.



AT SELECTION-SCREEN OUTPUT.



AT SELECTION-SCREEN.



START-OF-SELECTION.

  CLEAR gv_msg.
  PERFORM input_data_check USING gv_msg.
  IF gv_msg IS NOT INITIAL.
    MESSAGE s000 WITH gv_msg DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-Data Select Start
  PERFORM get_team_members.
  IF gv_msg IS NOT INITIAL.
    MESSAGE s000 WITH gv_msg DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM select_data.
*-Data Select End

  IF p_r1 IS NOT INITIAL OR  p_r5 IS NOT INITIAL.
    CLEAR: gv_flag, it_file[], gv_msg.
    LOOP AT gt_9100 INTO gs_9100.
      MOVE-CORRESPONDING gs_9100 TO it_file.
      APPEND it_file.  CLEAR it_file.
    ENDLOOP.
    IF it_file[] IS INITIAL.
      gv_flag = 'N'.
      gv_msg  = 'Not successful !'.
    ELSE.
      CLEAR gv_flag.
      gv_msg  = 'Successful !'.
    ENDIF.
  ELSE.
    IF gv_msg IS NOT INITIAL.
      gv_flag = 'N'.
    ENDIF.
  ENDIF.

  IF gv_flag IS NOT INITIAL.
    MESSAGE s000 WITH gv_msg DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  CASE 'X'.
    WHEN p_r1.
      PERFORM data_edit_process.
      PERFORM display_data1.
    WHEN p_r5.
      PERFORM data_edit_paybalance.
      PERFORM display_data1.
    WHEN p_r2. PERFORM create_emergency_suspend.
    WHEN p_r3. PERFORM create_lift_emer_suspention.
    WHEN p_r4. PERFORM create_enroll.
  ENDCASE.


END-OF-SELECTION.







*&---------------------------------------------------------------------*
*&      Form  GET_TEAM_MEMBERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_team_members .

  CLEAR: gt_pernr, gt_pernr[], gv_msg.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_pernr
    FROM pa0000 AS pa0 INNER JOIN pa0001 AS pa1
                          ON pa0~pernr EQ pa1~pernr
   WHERE pa0~pernr IN s_pernr
     AND pa0~stat2 IN s_stat2
     AND pa1~bukrs IN s_bukrs
     AND pa1~werks IN s_werks
     AND pa1~btrtl IN s_btrtl
     AND pa1~persg IN s_persg
     AND pa1~persk IN s_persk
     AND pa1~abkrs IN s_abkrs
     AND pa1~kostl IN s_kostl
     AND pa1~sachz IN s_sachz
     AND ( pa0~begda <= sy-datum  AND pa0~endda >= sy-datum ).

  IF sy-subrc NE 0.
    gv_msg = 'There are no Data!'.
    EXIT.
  ENDIF.

  SORT gt_pernr BY pernr.
  DELETE ADJACENT DUPLICATES FROM gt_pernr.

ENDFORM.                    " GET_TEAM_MEMBERS



*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data .

  DATA: lv_flag           TYPE c
      .
  CLEAR: gv_msg, gt_9100[].

* Re-design of gt_9100
  LOOP AT gt_pernr.
    CLEAR: gs_9100 , gv_flag, *pa9100, lv_flag
         .
    PERFORM current_it9100_read  USING gt_pernr-pernr *pa9100.
    IF *pa9100 IS NOT INITIAL.
      MOVE-CORRESPONDING *pa9100 TO gs_9100.
    ENDIF.
    IF p_ene = 'X'.
      IF *pa9100-eligi EQ 'E'. "Enrolled and eligible
        lv_flag = 'A'.
      ENDIF.
    ENDIF.
    IF p_es1 = 'X'.
      IF *pa9100-eligi EQ 'S1'. "Enrolled but suspended 1
        lv_flag = 'A'.
      ENDIF.
    ENDIF.
    IF p_es2 = 'X'.
      IF *pa9100-eligi EQ 'S2'. "Enrolled but suspended 2
        lv_flag = 'A'.
      ENDIF.
    ENDIF.
    IF p_emr = 'X'.
      IF *pa9100-eligi EQ 'SE'. "Enrolled but suspende
        lv_flag = 'A'.
      ENDIF.
    ENDIF.
    IF p_not = 'X'.
      IF gv_flag IS NOT INITIAL.
        gs_9100-pernr = gt_pernr-pernr.
        lv_flag = 'A'.
        CLEAR gv_flag.
      ENDIF.
    ENDIF.

    IF lv_flag IS NOT INITIAL.
      APPEND gs_9100 TO gt_9100.
    ENDIF.
  ENDLOOP.

  IF gt_9100[] IS INITIAL.
    gv_msg = 'There are no Data...IT9100'.
    EXIT.
  ENDIF.

  SORT gt_9100 BY pernr endda DESCENDING begda.
  DELETE ADJACENT DUPLICATES FROM gt_9100 COMPARING pernr.

ENDFORM.                    " SELECT_DATA



*&---------------------------------------------------------------------*
*&      Form  UPD_ELIGIBILITY_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0298   text
*----------------------------------------------------------------------*
FORM upd_eligibility_status  USING    p_value.

  BREAK-POINT.

ENDFORM.                    " UPD_ELIGIBILITY_STATUS



*&---------------------------------------------------------------------*
*&      Form  INPUT_DATA_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_MSG  text
*----------------------------------------------------------------------*
FORM input_data_check  USING    p_msg.

  CLEAR p_msg.
*  IF s_pernr-low IS INITIAL.
*    p_msg = 'Please enter "Personnel Number."'.
*    EXIT.
*  ENDIF.
*
*  IF S_PERSG-LOW is initial.
*    p_msg = 'Please enter "Employee group."'.
*    EXIT.
*  ENDIF.
*
*  IF S_PERSK-LOW is initial.
*    p_msg = 'Please enter "Employee subgroup."'.
*    EXIT.
*  ENDIF.

ENDFORM.                    " INPUT_DATA_CHECK



*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA1
*&---------------------------------------------------------------------*
*       Display Data : Display enrollment history
*----------------------------------------------------------------------*
FORM display_data1.

  IF p_r1 = 'X'.
    PERFORM fieldcat_init     USING 'IT_FILE'.
  ELSE.
    PERFORM fieldcat_init     USING 'IT_DATA'.
  ENDIF.
  PERFORM build_layout      USING 'X' 'X' space.
  PERFORM build_comment     USING  gt_header[].
  IF p_r1 = 'X'.
    PERFORM alv_grid_display  TABLES it_file.
  ELSE.
    PERFORM alv_grid_display  TABLES it_data.
  ENDIF.

ENDFORM.                    " DISPLAY_DATA



*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA5
*&---------------------------------------------------------------------*
*       Display Data : Display current balance
*----------------------------------------------------------------------*
FORM display_data5.

  PERFORM fieldcat_init     USING 'IT_DATA'.
  PERFORM build_layout      USING 'X' 'X' space.
  PERFORM build_comment     USING  gt_header[].
  PERFORM alv_grid_display  TABLES it_data.

ENDFORM.                    " DISPLAY_DATA



*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text : Initialize Field Catalog
*----------------------------------------------------------------------*
*      -->PT_FIELDCAT  Field Catalog Value
*----------------------------------------------------------------------*
FORM fieldcat_init  USING    p_file TYPE slis_tabname.

  DATA: ls_fieldcat LIKE LINE OF gt_fieldcat.
  CLEAR : gt_fieldcat,   gt_fc1
        , gt_fieldcat[], gt_fc1[]
        .

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = p_file
      i_inclname             = sy-repid
    CHANGING
      ct_fieldcat            = gt_fc1
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0 OR gt_fc1[] IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  gt_fieldcat[] = gt_fc1[].
* Set Key field.
  LOOP AT gt_fieldcat INTO gs_fieldcat
                     WHERE fieldname = 'PERNR'.
    gs_fieldcat-key = 'X'.
    MODIFY gt_fieldcat FROM gs_fieldcat TRANSPORTING key.
  ENDLOOP.

* Change Description
  PERFORM change_desc USING 'PERNR'  'PersNo.'.
  IF p_r1 = 'X'.
    PERFORM change_desc USING 'ELIGI'  'Eligibility status'.
    PERFORM change_desc USING 'BEGDA'  'Begin date'.
    PERFORM change_desc USING 'ENDDA'  'End date'.
    PERFORM change_desc USING 'AEDTM'  'Change on-date'.
    PERFORM change_desc USING 'UNAME'  'Changed by-'.
    PERFORM change_desc USING 'TEXT1'  'Status details'.
  ENDIF.
  PERFORM change_desc USING 'ENAME'  'Employee name'.
  PERFORM change_desc USING 'KOSTL'  'Cost center'.
  PERFORM change_desc USING 'LTEXT'  'Cost center Description'.
  PERFORM change_desc USING 'PLANS'  'Position'.
  PERFORM change_desc USING 'STEXT'  'Position name'.
  PERFORM change_desc USING 'STELL'  'Job'.
  PERFORM change_desc USING 'STELLT' 'Job name'.
  PERFORM change_desc USING 'ORGEH'  'Organization unit'.
  PERFORM change_desc USING 'ORGEHT' 'Organization name'.
  IF p_r1 = 'X'.
    PERFORM change_desc USING 'BETRG' 'Deduction balance'.
  ENDIF.

ENDFORM.                    " FIELDCAT_INIT



*&---------------------------------------------------------------------*
*&      Form  CHANGE_DESC
*&---------------------------------------------------------------------*
*       Change ALV field description
*----------------------------------------------------------------------*
*      -->P_FIELD   Field Name
*      -->P_DESC    Field Description
*----------------------------------------------------------------------*
FORM change_desc  USING    p_field TYPE c
                           p_desc  TYPE c.

  DATA: gs_fieldcat TYPE slis_fieldcat_alv.

  READ TABLE gt_fieldcat INTO gs_fieldcat
                         WITH KEY fieldname = p_field.
  IF sy-subrc = 0.
    gs_fieldcat-seltext_l    = p_desc.
    gs_fieldcat-seltext_m    = p_desc.
    gs_fieldcat-seltext_s    = p_desc.
    gs_fieldcat-reptext_ddic = p_desc.
    MODIFY gt_fieldcat FROM gs_fieldcat INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " CHANGE_DESC



*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0554   text
*      -->P_0555   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_layout         USING p_cb p_color p_sum.

  CLEAR gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-cell_merge        = space.
  gs_layout-colwidth_optimize = 'X'.
*  gs_layout-default_item      = 'X'.
  gs_layout-list_append       = 'X'.

ENDFORM.                    " BUILD_LAYOUT



*&---------------------------------------------------------------------*
*&      Form  BUILD_COMMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GT_HEADER[]  text
*----------------------------------------------------------------------*
FORM build_comment        USING p_gt_header TYPE slis_t_listheader.

  DATA: ls_line           TYPE slis_listheader
      , ls_color          TYPE slis_specialcol_alv
      , l_date(50)        TYPE c
      , l_text(70)        TYPE c
      , i_lines(5)        TYPE c
      , i_count(5)        TYPE c
      .

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-info = 'Display TM"s enrollment history'.
  IF p_r5 IS NOT INITIAL.
    CLEAR ls_line-info.
    ls_line-info = 'Display TM"s current balance in payroll'.
  ENDIF.
  APPEND ls_line TO p_gt_header.

ENDFORM.                    " BUILD_COMMENT



*&---------------------------------------------------------------------*
*&      Form  ALV_GRID_DISPLAY
*&---------------------------------------------------------------------*
*       Display Data using ALV Grid
*----------------------------------------------------------------------*
FORM alv_grid_display TABLES ft_outtab TYPE table.

  DATA: l_repid TYPE sy-repid.
  DATA: gs_layout TYPE slis_layout_alv.

  CLEAR gt_sort[].
  gt_sort-fieldname = 'PERNR'.
  gt_sort-up = 'X'.
  APPEND gt_sort.

  gt_sort-fieldname = 'ENAME'.
  gt_sort-up = 'X'.
  APPEND gt_sort.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra = 'X'.

  l_repid = sy-repid.

*** print paramter   ****************************************
  gs_print-no_coverpage = 'X'.
  gs_print-no_print_listinfos = 'X'.
  gs_print-no_change_print_params = 'X'.
  gs_print-no_print_selinfos = 'X'.
*************************************************************
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_repid
      is_layout          = gs_layout
      is_print           = gs_print
      it_sort            = gt_sort[]
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = ft_outtab
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " ALV_GRID_DISPLAY



*&---------------------------------------------------------------------*
*&      Form  CREATE_EMERGENCY_SUSPEND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_emergency_suspend .

  LOOP AT gt_9100 INTO gs_9100.
    CLEAR: *pa9100.
    PERFORM current_it9100_read USING gs_9100-pernr *pa9100.
    CHECK gv_flag IS INITIAL.

    IF *pa9100-eligi = 'E'.
      gv_eligi = 'SE'.
      PERFORM create_new_record_it9100 USING gv_eligi.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_EMERGENCY_SUSPEND



*&---------------------------------------------------------------------*
*&      Form  CREATE_LIFT_EMER_SUSPENTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_lift_emer_suspention .

  LOOP AT gt_9100 INTO gs_9100.
    CLEAR: *pa9100.
    PERFORM current_it9100_read USING gs_9100-pernr *pa9100.
    CHECK gv_flag IS INITIAL.

    IF *pa9100-eligi = 'SE'.
      gv_eligi = 'E'.
      PERFORM create_new_record_it9100 USING gv_eligi.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CREATE_LIFT_EMER_SUSPENTION



*&---------------------------------------------------------------------*
*&      Form  CREATE_ENROLL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_enroll .

  LOOP AT gt_9100 INTO gs_9100.
    CLEAR: *pa9100.
    PERFORM current_it9100_read USING gs_9100-pernr *pa9100.
** Furong on 06/26/14 (
*    CHECK gv_flag IS NOT INITIAL.
    IF gv_flag IS INITIAL.
      MESSAGE s000 WITH gs_9100-pernr ' not sucessful'.
      CONTINUE.
    ENDIF.
** )
    gv_eligi = 'E'.
    PERFORM create_new_record_it9100 USING gv_eligi.
  ENDLOOP.

ENDFORM.                    " CREATE_ENROLL



*&---------------------------------------------------------------------*
*&      Form  CURRENT_IT9100_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_*PA9100  text
*----------------------------------------------------------------------*
FORM current_it9100_read  USING p_pernr p_*pa9100.

  CLEAR: p_*pa9100, gv_flag.
*  IF p_r1 IS INITIAL.       "20140611 Add
  SELECT SINGLE * INTO p_*pa9100
    FROM pa9100
   WHERE pernr = p_pernr
     AND ( begda <= sy-datum  AND endda >= sy-datum ).
*  ELSE.
*    SELECT SINGLE * INTO p_*pa9100
*      FROM pa9100
*     WHERE pernr = p_pernr.
*  ENDIF.

  IF sy-subrc NE 0.
    gv_flag = 'N'.
  ENDIF.

ENDFORM.                    " CURRENT_IT9100_READ



*&---------------------------------------------------------------------*
*&      Form  CREATE_NEW_RECORD_IT9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GV_ELIGI  text
*----------------------------------------------------------------------*
FORM create_new_record_it9100  USING    p_eligi.

  DATA : wa_9100          TYPE p9100
       , wa_begda         TYPE dats
       , wa_endda         TYPE dats
       , wv_date(11)      TYPE c
       , lv_flag          TYPE c
       .
  lv_flag = 'E'.
  PERFORM employee_enqueue USING lv_flag gs_9100-pernr.  "Enqueue

  CLEAR wa_9100.
  wa_9100       = gs_9100.
  wa_9100-endda = '99991231'.
  wa_9100-begda = sy-datum.
  wa_begda      = wa_9100-begda.
  wa_endda      = wa_9100-endda.

  CONCATENATE sy-datum+4(2) '/' sy-datum+6(2) '/' sy-datum+0(4) '.'
               INTO wv_date.
  CASE 'X'.
    WHEN p_r2.
      CONCATENATE 'Suspended due to emergency on '
                  wv_date
             INTO wa_9100-text1 SEPARATED BY space.
    WHEN p_r3.
      CONCATENATE 'Previous emergency suspension lifted on '
                  wv_date
             INTO wa_9100-text1 SEPARATED BY space.
    WHEN p_r4.
      CONCATENATE 'Enrolled by Administrator on '
                  wv_date
             INTO wa_9100-text1 SEPARATED BY space.
  ENDCASE.
  wa_9100-uname = sy-uname. "'RFCESS'.
  wa_9100-seqnr = '000'.
  wa_9100-eligi = p_eligi.

  "plus populate any other fields you need to update
  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty            = '9100'
      number           = wa_9100-pernr
      subtype          = wa_9100-subty
      objectid         = wa_9100-objps
*     LOCKINDICATOR    =
      validityend      = wa_endda
      validitybegin    = wa_begda
*     RECORDNUMBER     =
      record           = wa_9100
      operation        = insert
      tclas            = 'A'
*     DIALOG_MODE      = '0'
      nocommit         = 'X'
*     VIEW_IDENTIFIER  =
*     SECONDARY_RECORD =
    IMPORTING
      return           = ls_return
      key              = lv_key.

  IF ls_return IS NOT INITIAL.
    return-type = 'E'.
    CONCATENATE ls_return-id ls_return-number INTO ls_return-id.
    CONDENSE ls_return-id NO-GAPS.
*        return-code = ls_return-id.
    return-message = ls_return-message.
    APPEND return.
    CLEAR gv_msg.
    gv_msg = 'Not successful !'.
    MESSAGE s000 WITH gv_msg.
  ELSE.
    COMMIT WORK.

    return-type = 'S'.
    CONCATENATE 'Success :' lv_key INTO return-message
                SEPARATED BY space.
*        return-message = 'Success!'.
    APPEND return.
    CLEAR gv_msg.
    gv_msg = 'Successful !'.
    MESSAGE s000 WITH gv_msg.

  ENDIF.

  lv_flag = 'D'.
  PERFORM employee_enqueue USING lv_flag gs_9100-pernr.  "Dequeue

ENDFORM.                    " CREATE_NEW_RECORD_IT9100



*&---------------------------------------------------------------------*
*&      Form  EMPLOYEE_ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_PERNR  text
*----------------------------------------------------------------------*
FORM employee_enqueue  USING   p_flag p_pernr.

  IF p_flag = 'E'.
    CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
      EXPORTING
        number = p_pernr.
  ELSE.
    CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
      EXPORTING
        number = p_pernr.
  ENDIF.

ENDFORM.                    " EMPLOYEE_ENQUEUE



*&---------------------------------------------------------------------*
*&      Form  DATA_EDIT_PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_edit_process .

  DATA: ls_file           LIKE LINE  OF it_file
      , lt_file           LIKE TABLE OF ls_file WITH HEADER LINE
      , lt_9100           LIKE TABLE OF p9100   WITH HEADER LINE
      , lv_pernr          TYPE pernr_d
      , lv_ename          TYPE emnam
      , lv_index          LIKE sy-tabix
      .

  lt_file[] = it_file[].
  CLEAR: it_file, it_file[].
  SORT lt_file BY pernr.
  LOOP AT lt_file INTO ls_file.
    CLEAR: lt_9100[], is_file.
    PERFORM hr_read_infotype TABLES lt_9100
                             USING  ls_file-pernr.
    IF lt_9100[] IS NOT INITIAL.
      LOOP AT lt_9100.
        MOVE-CORRESPONDING lt_9100 TO is_file.
        APPEND is_file TO it_file.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT it_file BY pernr endda DESCENDING begda.
  LOOP AT it_file INTO is_file.
    ls_file = is_file.
    PERFORM get_remain_data USING is_file.
    lv_index = sy-tabix.
    IF is_file = ls_file.
      CONTINUE.
    ENDIF.
    it_file-ename  = is_file-ename.
    it_file-kostl  = is_file-kostl.
    it_file-ltext  = is_file-ltext.
    it_file-plans  = is_file-plans.
    it_file-stext  = is_file-stext.
    it_file-stell  = is_file-stell.
    it_file-stellt = is_file-stellt.
    it_file-orgeh  = is_file-orgeh.
    it_file-orgeht = is_file-orgeht.
    MODIFY it_file FROM is_file INDEX lv_index
    TRANSPORTING ename  kostl  ltext  plans  stext
                 stell  stellt orgeh  orgeht.
  ENDLOOP.


ENDFORM.                    " DATA_EDIT_PROCESS
*&---------------------------------------------------------------------*
*&      Form  HR_READ_INFOTYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_FILE_PERNR  text
*      -->P_LT_0001  text
*      -->P_ENDLOOP  text
*----------------------------------------------------------------------*
FORM hr_read_infotype  TABLES   pt_9100
                       USING    p_pernr.

* HR History read
  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
*     TCLAS                 = 'A'
      pernr                 = p_pernr
      infty                 = '9100'
*     BEGDA                 = '18000101'
*     ENDDA                 = '99991231'
*     BYPASS_BUFFER         = ' '
*     LEGACY_MODE           = ' '
*   IMPORTING
*     SUBRC                 =
    TABLES
      infty_tab             = pt_9100
    EXCEPTIONS
      infty_not_found       = 1
      OTHERS                = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.


ENDFORM.                    " HR_READ_INFOTYPE



*&---------------------------------------------------------------------*
*&      Form  GET_REMAIN_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IS_FILE  text
*----------------------------------------------------------------------*
FORM get_remain_data  USING    p_file TYPE zshr_cafe_9100.

  DATA: pa0001            TYPE pa0001.
  CHECK p_file IS NOT INITIAL.
  SELECT SINGLE * INTO pa0001
    FROM pa0001
   WHERE pernr = p_file-pernr
     AND begda <= sy-datum AND endda >= sy-datum.

  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  p_file-ename = pa0001-ename.
  p_file-kostl = pa0001-kostl.
  p_file-plans = pa0001-plans.
  p_file-stell = pa0001-stell.
  p_file-orgeh = pa0001-orgeh.

* Cost Center
  SELECT SINGLE ltext INTO p_file-ltext
    FROM cskt
   WHERE kokrs = 'H201'
     AND kostl = pa0001-kostl
     AND datbi = '99991231'.

* Position
  SELECT SINGLE stext INTO p_file-stext
    FROM hrp1000
   WHERE plvar = '01'
     AND otype = 'S'
     AND objid = pa0001-plans
     AND endda = '99991231'
     AND langu = 'EN'.

* Job
  SELECT SINGLE stext INTO p_file-stellt
    FROM hrp1000
   WHERE plvar = '01'
     AND otype = 'C'
     AND objid = pa0001-stell "pa0001-plans  20140609
     AND endda = '99991231'
     AND langu = 'EN'.

*
  SELECT SINGLE stext INTO p_file-orgeht
    FROM hrp1000
   WHERE plvar = '01'
     AND otype = 'O'
     AND objid = pa0001-orgeh "pa0001-plans  20140609
     AND endda = '99991231'
     AND langu = 'EN'.

ENDFORM.                    " GET_REMAIN_DATA



*&---------------------------------------------------------------------*
*&      Form  DATA_EDIT_PAYBALANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_edit_paybalance .

  SORT it_file BY pernr endda DESCENDING begda.
  LOOP AT it_file INTO is_file.
    PERFORM get_remain_data USING is_file.
    PERFORM payresult_of_last_balance USING is_file-pernr.
    CLEAR it_data.
    MOVE-CORRESPONDING is_file TO it_data.
    IF gv_betrg <> 0.
      it_data-betrg = gv_betrg.
    ENDIF.
    APPEND it_data.
  ENDLOOP.

ENDFORM.                    " DATA_EDIT_PAYBALANCE



*&---------------------------------------------------------------------*
*&      Form  PAYRESULT_OF_LAST_BALANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FILE_PERNR  text
*----------------------------------------------------------------------*
FORM payresult_of_last_balance  USING    p_pernr.

  DATA : lv_relid         LIKE pcl2-relid
       , lv_molga         TYPE molga
       .
  RANGES: s_begda         FOR  p0001-begda
        , s_fpper         FOR  pc261-fpper
        .
  CHECK p_pernr IS NOT INITIAL.

*- Read last payroll results  start
  CLEAR : gt_rgdir, gt_rgdir[].
  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr             = p_pernr
*     BUFFER             =
*     NO_AUTHORITY_CHECK = ' '
    IMPORTING
      molga              = g_molga
    TABLES
      in_rgdir           = gt_rgdir[]
    EXCEPTIONS
      no_record_found    = 1
      OTHERS             = 2.

  if sy-subrc <> 0.
  endif.

** Furong on 07/22/14 checking pay result <> empty (
  CHECK gt_rgdir[] IS NOT INITIAL.
** )

* Delete payroll control records where payroll period is 000000
  DELETE gt_rgdir WHERE fpper EQ '000000'.

* Delete voided payroll data.
  DELETE gt_rgdir WHERE voidr NE space.

  IF NOT s_begda[] IS INITIAL.
    DELETE gt_rgdir WHERE NOT paydt IN s_begda. "System Year
  ENDIF.
* Delete payroll control records based on selection input
  IF  NOT  s_fpper[]  IS INITIAL.
    DELETE gt_rgdir WHERE NOT fpper IN s_fpper. "Payroll Period
  ENDIF.

* Cluster id for US
* Personnel Country Grouping
  CLEAR lv_relid.
  SELECT SINGLE relid INTO lv_relid
                FROM t500l
                WHERE molga = g_molga.
  IF   lv_relid IS INITIAL.
    lv_relid = 'RU'.
  ENDIF.

  CLEAR: gs_pay_result, gs_rgdir, gv_pp.
*  sort last payroll period
  SORT gt_rgdir BY seqnr DESCENDING fpbeg fpend DESCENDING.
  READ TABLE gt_rgdir INTO gs_rgdir INDEX 1.
  gv_pp = gs_rgdir-fpend.

  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
*     CLUSTERID                          =
      employeenumber                     = p_pernr
      sequencenumber                     = gs_rgdir-seqnr
*     READ_ONLY_BUFFER                   = ' '
      read_only_international            = 'X'
*     ARC_GROUP                          = ' '
*     CHECK_READ_AUTHORITY               = 'X'
*     FILTER_CUMULATIONS                 = 'X'
*     CLIENT                             =
*   IMPORTING
*     VERSION_NUMBER_PAYVN               =
*     VERSION_NUMBER_PCL2                =
    CHANGING
      payroll_result                     = gs_pay_result
*   EXCEPTIONS
*     ILLEGAL_ISOCODE_OR_CLUSTERID       = 1
*     ERROR_GENERATING_IMPORT            = 2
*     IMPORT_MISMATCH_ERROR              = 3
*     SUBPOOL_DIR_FULL                   = 4
*     NO_READ_AUTHORITY                  = 5
*     NO_RECORD_FOUND                    = 6
*     VERSIONS_DO_NOT_MATCH              = 7
*     ERROR_READING_ARCHIVE              = 8
*     ERROR_READING_RELID                = 9
*     OTHERS                             = 10
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*- Read last payroll results  end
  CLEAR gv_betrg.
  LOOP AT gs_pay_result-inter-arrrs INTO wa_arrrs.

    IF wa_arrrs-lgart = '3050' AND wa_arrrs-betrg <> 0.
      gv_betrg = gv_betrg + wa_arrrs-betrg.
      EXIT.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " PAYRESULT_OF_LAST_BALANCE
