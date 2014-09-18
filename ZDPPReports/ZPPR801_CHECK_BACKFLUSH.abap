************************************************************************
* Program Name      : ZPPR803_PRDT_PLAN
* Author            : Furong Wang
* Creation Date     : 08/22/2006
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT zppr801_check_backflush MESSAGE-ID zmmm .

TABLES: ztpp_wosum, ztpp_input_plan, mara, eord, ausp.

*---// Internal tables
TYPE-POOLS: vrm.

DATA:  wa_kalid   LIKE kako-kalid .
DATA:  wa_custom_control TYPE scrfname VALUE 'ALV_CONTAINER',
       alv_grid TYPE REF TO cl_gui_alv_grid,
       grid_container TYPE REF TO cl_gui_custom_container.

DATA : it_fieldcat  TYPE lvc_t_fcat WITH HEADER LINE.

DATA:  wa_is_layout TYPE lvc_s_layo,
       w_fieldname  LIKE LINE OF it_fieldcat,
       it_fieldname TYPE slis_t_fieldcat_alv,
       wa_variant TYPE disvariant.

DATA: ok_code      LIKE sy-ucomm,
      w_repid  LIKE sy-repid,
      w_cnt       TYPE   i.

DATA: it_objek LIKE TABLE OF ZTPP_BF_BODY WITH HEADER LINE.
DATA: it_output LIKE TABLE OF ZTPP_BF_BODY WITH HEADER LINE.
DATA: it_eord LIKE TABLE OF eord WITH HEADER LINE.

DATA: char_2(2) TYPE c.

DATA:   name    TYPE vrm_id,
        list    TYPE vrm_values,
        value   LIKE LINE OF list.

*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.

PARAMETERS: p_datum LIKE sy-datum OBLIGATORY DEFAULT sy-datum.
SELECT-OPTIONS: s_comp FOR mara-matnr.
SELECT-OPTIONS: s_fsc FOR ztpp_wosum-fsc  NO-EXTENSION.
SELECT-OPTIONS: s_rp FOR char_2 OBLIGATORY.
SELECT-OPTIONS: s_model FOR ztpp_input_plan-modl NO-EXTENSION.
SELECT-OPTIONS: s_lifnr FOR eord-lifnr.
SELECTION-SCREEN END OF BLOCK bl1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_listbox.

AT SELECTION-SCREEN.
  PERFORM check_data.

START-OF-SELECTION.
*  only for testing
  IF SY-UNAME = '101457' AND SY-MANDT = '120'.
  delete from ZTPP_BF_BODY where MODEL <> ' '.
  commit work.
  ENDIF.
**
  PERFORM process_data.
*  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_subrc    TYPE sy-subrc ,
        l_atnam    TYPE cabn-atnam,
        l_atwrt    TYPE ausp-atwrt,
        l_name     TYPE cabn-atnam,
        l_name1    TYPE cabn-atnam,
        l_atflv    TYPE ausp-atflv,
        l_atwrt_char(14),
        l_temp(06),
        l_datum    TYPE sy-datum,
        l_num(08)  TYPE n.

  DATA:  l_atinn_model LIKE ausp-atinn,
         l_atinn_body LIKE ausp-atinn,
         l_atinn_plan_order LIKE ausp-atinn,
         l_atinn_model_year LIKE ausp-atinn,
         l_atinn_natn_code LIKE ausp-atinn,
         l_atinn_dist_code LIKE ausp-atinn,
         l_atinn_mi LIKE ausp-atinn,
         l_atinn_ocn LIKE ausp-atinn,
         l_atinn_version LIKE ausp-atinn,
         l_atinn_rp06_shop_date LIKE ausp-atinn,
         l_atinn_rp06_serial LIKE ausp-atinn,
         l_atinn LIKE ausp-atinn.

  DATA: l_char15(18),
        l_model_year(1),
        l_natn_code(3),
        l_dist_code(2),
        l_mi(7),
        l_ocn(4),
        l_lines TYPE i,
        l_lines_body TYPE i,
        l_lines_comp TYPE i,
        l_lines_char(10),
        l_rp_st(2) TYPE n,
        l_rp_end(2) TYPE n,
        l_count TYPE i,
        l_plan_order like it_objek-plan_order,
        l_index like sy-tabix.

*  DATA: lt_ppc1 LIKE TABLE OF ppc1_all WITH HEADER LINE.

*  DATA: lt_eord LIKE TABLE OF eord WITH HEADER LINE.

  DATA: BEGIN OF lt_objek OCCURS 0,
          objek    LIKE ausp-objek,
          atlfv    LIKE ausp-atflv,
          rp(2),
        END OF lt_objek.

   DATA: BEGIN OF lt_objek_ccount OCCURS 0,
          objek    LIKE ausp-objek,
          atlfv    LIKE ausp-atflv,
          rp(2),
        END OF lt_objek_ccount.


  DATA: BEGIN OF lt_ppc OCCURS 0,
        orderid LIKE ppc_head-orderid,
        matnr LIKE ppc_mat-matnr,
        confquant LIKE ppc_conf_mat-confquant,
        CONFUNIT LIKE ppc_conf_mat-CONFUNIT,
        conftime LIKE ppc_head-conftime ,
        REPPOINT_EXT like ppc_rp-REPPOINT_EXT,
        END OF lt_ppc.


  RANGES: r_rp FOR lt_objek-rp.
  PERFORM get_source_list.

  l_atflv = l_num = p_datum.
  REFRESH lt_objek.

  IF s_rp-low IS INITIAL.
    l_rp_st = '01'.
    l_rp_end = '18'.
  ELSE.
    l_rp_st = s_rp-low.
    l_rp_end = s_rp-high.
  ENDIF.
  l_count = l_rp_st.
  WHILE l_rp_st <= l_rp_end.
    CONCATENATE 'P_RP' l_rp_st '_SHOP_DATE'  INTO l_name .

    SELECT DISTINCT objek atflv
    APPENDING CORRESPONDING FIELDS OF TABLE lt_objek
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE klart = '002' AND
          au~atflv = l_atflv AND
          ca~atnam = l_name .

    lt_objek-rp = l_rp_st.
    MODIFY lt_objek TRANSPORTING rp WHERE rp = space.
    l_count = l_count + 1.
    l_rp_st = l_count.

  ENDWHILE.

  sort lt_objek by objek.
*  delete adjacent duplicates from lt_objek.

*  DESCRIBE TABLE lt_objek LINES l_lines_body.
  lt_objek_ccount[] = lt_objek[].
  loop at lt_objek_ccount.
    clear lt_objek_ccount-rp.
    modify lt_objek_ccount.
  endloop.

  delete adjacent duplicates from lt_objek_ccount.

  DESCRIBE TABLE lt_objek_ccount LINES l_lines_body.
  free: lt_objek_ccount.

  IF l_lines_body > 0.
    PERFORM read_atinn USING 'P_MODEL' l_atinn_model.
    PERFORM read_atinn USING 'P_BODY_SERIAL' l_atinn_body.
    PERFORM read_atinn USING 'P_PLAN_ORDER' l_atinn_plan_order.
    PERFORM read_atinn USING 'P_MODEL_YEAR' l_atinn_model_year.
    PERFORM read_atinn USING 'P_NATN_CODE' l_atinn_natn_code.
    PERFORM read_atinn USING 'P_DIST_CODE' l_atinn_dist_code.
    PERFORM read_atinn USING 'P_MI' l_atinn_mi.
    PERFORM read_atinn USING 'P_OCN' l_atinn_ocn.
    PERFORM read_atinn USING 'P_VERSION' l_atinn_version.
    PERFORM read_atinn USING 'P_RP06_SHOP_DATE' l_atinn_rp06_shop_date.
    PERFORM read_atinn USING 'P_RP06_SERIAL' l_atinn_rp06_serial.

    LOOP AT lt_objek.
*    it_objek-objek = lt_objek-objek.
      it_objek-rp = lt_objek-rp.

      SELECT SINGLE atwrt INTO it_objek-model
        FROM ausp
        WHERE objek = lt_objek-objek
          AND atinn = l_atinn_model
          AND klart = '002'.

      SELECT SINGLE atwrt INTO it_objek-body
        FROM ausp
        WHERE objek = lt_objek-objek
          AND atinn = l_atinn_body
          AND klart = '002'.

      SELECT SINGLE atwrt INTO l_plan_order
         FROM ausp
         WHERE objek = lt_objek-objek
           AND atinn = l_atinn_plan_order
           AND klart = '002'.

      it_objek-plan_order = l_plan_order
.
      SELECT SINGLE atwrt INTO l_model_year
       FROM ausp
         WHERE objek = lt_objek-objek
           AND atinn = l_atinn_model_year
           AND klart = '002'.

      SELECT SINGLE atwrt INTO l_natn_code
        FROM ausp
          WHERE objek = lt_objek-objek
            AND atinn = l_atinn_natn_code
            AND klart = '002'.

      SELECT SINGLE atwrt INTO l_dist_code
        FROM ausp
          WHERE objek = lt_objek-objek
            AND atinn = l_atinn_dist_code
            AND klart = '002'.

      SELECT SINGLE atwrt INTO l_mi
         FROM ausp
           WHERE objek = lt_objek-objek
             AND atinn = l_atinn_mi
             AND klart = '002'.

      SELECT SINGLE atwrt INTO l_ocn
        FROM ausp
          WHERE objek = lt_objek-objek
            AND atinn = l_atinn_ocn
            AND klart = '002'.

      CONCATENATE l_model_year l_natn_code l_dist_code
                          l_mi INTO it_objek-fsc.
      CONCATENATE it_objek-fsc l_ocn INTO it_objek-fsc
                        SEPARATED BY space.

      IF NOT s_fsc-low IS INITIAL.
        IF it_objek-fsc < s_fsc-low OR
           it_objek-fsc > s_fsc-high.
          CLEAR: it_objek.
          CONTINUE.
        ENDIF.
      ENDIF.

      SELECT SINGLE atwrt INTO it_objek-version
        FROM ausp
          WHERE objek = lt_objek-objek
            AND atinn = l_atinn_version
            AND klart = '002'.

      SELECT SINGLE atflv INTO l_atflv
        FROM ausp
          WHERE objek = lt_objek-objek
            AND atinn = l_atinn_rp06_shop_date
            AND klart = '002'.
      it_objek-tinput_date = l_num = l_atflv.

      SELECT SINGLE atwrt INTO it_objek-tinput_seq
        FROM ausp
          WHERE objek = lt_objek-objek
            AND atinn = l_atinn_rp06_serial
            AND klart = '002'.

      it_objek-posting_date = p_datum.
      it_objek-interface_date = sy-datum.

      CONCATENATE 'P_RP' lt_objek-rp '_ACTUAL_DATE' INTO l_name.
      PERFORM read_atinn USING l_name l_atinn.
      SELECT SINGLE atwrt INTO l_atwrt_char
        FROM ausp
          WHERE objek = lt_objek-objek
            AND atinn = l_atinn
            AND klart = '002'.
      it_objek-shop_date = l_atwrt_char+0(8).
      it_objek-shop_time = l_atwrt_char+8(6).
      APPEND it_objek.
      CLEAR: lt_objek, it_objek.
    ENDLOOP.

    CLEAR: lt_ppc, lt_ppc[].
    SELECT a~orderid
           d~matnr
           b~confquant
           B~CONFUNIT
           a~conftime
           e~REPPOINT_EXT
        INTO TABLE lt_ppc
       FROM ppc_head AS a
        inner join ppc_rp as e
        on a~REPPOINT = e~REPPOINT
       INNER JOIN ppc_conf_mat AS b
        ON a~headid = b~headid
        INNER JOIN ppc_mat_det AS c
        ON b~accid = c~accid
        INNER JOIN ppc_mat AS d
        ON c~matid = d~matid
        FOR ALL ENTRIES IN it_objek
        WHERE orderid = it_objek-plan_order
           AND matnr IN s_comp
           AND postdate = p_datum
           and e~REPPOINT_EXT = it_objek-rp.

    DESCRIBE TABLE lt_ppc LINES l_lines.
    IF l_lines > 0.
      SORT lt_ppc BY orderid.
      SORT it_objek BY plan_order.
      clear: l_plan_order.
      loop at lt_ppc.
        if l_plan_order <> lt_ppc-orderid.
           l_plan_order = lt_ppc-orderid.
           read table it_objek with key plan_order = lt_ppc-orderid
                                    RP = lt_ppc-REPPOINT_ext.
           if sy-subrc = 0.
              it_output = it_objek.
           else.
              clear: lt_ppc, it_objek, it_output.
              continue.
           endif.
         endif.
           READ TABLE it_eord WITH KEY matnr = lt_ppc-matnr.
           IF sy-subrc = 0.
            it_output-vendor = it_eord-lifnr.
            it_output-quantity = lt_ppc-confquant.
            it_output-UNIT = lt_ppc-CONFUNIT.
            it_output-component = lt_ppc-matnr.
            l_char15 = lt_ppc-conftime.
            condense l_char15.
            it_output-bf_date = l_char15+0(8).
            it_output-bf_time = l_char15+8(6).
            collect it_output.
           endif.
          CLEAR:it_output-vendor,it_output-quantity,it_output-component,
                it_output-bf_date, it_output-bf_time, it_eord.

      endloop.

      DESCRIBE TABLE it_output LINES l_lines_comp.
      IF l_lines_comp > 0.
        MODIFY ZTPP_BF_BODY FROM TABLE it_output.
        IF sy-subrc = 0.
          COMMIT WORK.
          data: l_txt1(40),
                l_txt2(40),
                l_txt3(40).
         write: /10 'Total Downloaded Records :'.
         write: 38 l_lines_comp.

         write: /10 'Total Body Number :'.
         write: 38 l_lines_body.

         write: /10 'Posting Date :'.
         write: 38 p_datum.

*          l_lines_char = l_lines_comp.
*          concatenate 'Total Downloaded Records :' l_lines_char
*                      into l_txt1.
*          l_lines_char = l_lines_body.
*          concatenate 'Total Body Number :' l_lines_char
*                      into l_txt2.
*          concatenate 'Posting date :' p_datum
*                      into l_txt3.
*          CALL FUNCTION 'POPUP_TO_INFORM'
*            EXPORTING
*              titel         = 'Successfully Finished'
*              txt1          = l_txt1
*              txt2          = l_txt2
*              TXT3          = l_txt3
**             TXT4          = ' '
                    .

        ELSE.
          ROLLBACK WORK.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE i000 WITH 'No backflush data'.
    ENDIF.
  ELSE.
    MESSAGE i000 WITH 'No data for specified reporting point'.
  ENDIF.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  set_listbox
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox.
  DATA: l_atinn              LIKE cabn-atinn,
        l_atnam              LIKE cabn-atnam,
        l_atwrt              LIKE ausp-atwrt,
        l_atwtb              LIKE cawnt-atwtb.

  CLEAR: name, value, list, list[].

  name = 'S_MODEL'.

  SELECT SINGLE atinn INTO l_atinn
    FROM cabn
   WHERE atnam = 'P_MODEL'.

  SELECT n~atwrt t~atwtb INTO (l_atwrt, l_atwtb)
    FROM cawn AS n INNER JOIN cawnt AS t
      ON n~atinn = t~atinn
     AND n~atzhl = t~atzhl
   WHERE n~atinn = l_atinn
     AND t~spras = sy-langu .
    value-text = l_atwrt.
    value-key  = l_atwrt.
    APPEND value TO list.
  ENDSELECT.

* LIST BOX SETTING
  PERFORM list_box_function USING name.
  IF s_model-low IS INITIAL.
    READ TABLE list INTO value  INDEX 1.
    s_model-low = value-key.
  ENDIF.
ENDFORM.                    " set_listbox_lgort
*&---------------------------------------------------------------------*
*&      Form  displayt_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  CALL SCREEN 0200.
ENDFORM.                    " displayt_data
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
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM build_field_catalog USING 'IT_TAB'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.

  DATA:   w_repid LIKE sy-repid.
  CREATE OBJECT grid_container
          EXPORTING container_name = wa_custom_control
          EXCEPTIONS
           cntl_error = 1
           cntl_system_error = 2
           create_error = 3
           lifetime_error = 4
           lifetime_dynpro_dynpro_link = 5.
  w_repid = sy-repid.
  IF sy-subrc NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              titel = w_repid
              txt2  = sy-subrc
              txt1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT alv_grid
         EXPORTING i_parent = grid_container
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
  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
  wa_is_layout-edit       = ' '.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
*  wa_is_layout-cwidth_opt = 'X'.   "/optimizes the column width
*  if not s_fsc-low is initial.
*     CONCATENATE 'FSC:' s_fsc-low INTO wa_is_layout-GRID_TITLE
*                 SEPARATED BY SPACE.
*  ENDIF.
*  wa_is_layout-info_fname = 'IF'.
*  wa_is_layout-ctab_fname = 'CT'.
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

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0553   text
*----------------------------------------------------------------------*
FORM build_field_catalog USING  value(p_itab).
*
*  DATA: lw_itab TYPE slis_tabname,
*        lw_waers LIKE t001-waers,
*        l_qty(9),
*        l_datum(8),
*        l_cn(2) TYPE n.
*
*  CLEAR: it_fieldcat,  it_fieldcat[],
*         it_fieldname, it_fieldname[].
*  CLEAR: w_repid.
*
*  lw_itab = p_itab.
*
*  w_repid = sy-repid.
*
*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*       EXPORTING
*            i_program_name     = w_repid
*            i_internal_tabname = lw_itab
*            i_inclname         = w_repid
*       CHANGING
*            ct_fieldcat        = it_fieldname.
*
*  PERFORM setting_fieldcat TABLES it_fieldcat USING :
*
*
*                                  'S' 'NATION'      ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Nation',
*                                  'E' 'OUTPUTLEN'   '8',
*
*                                  'S' 'FSC  '       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'FSC',
*                                  'E' 'OUTPUTLEN'   '16',
*
*                                   'S' 'EXTC'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Extc',
*                                  'E' 'OUTPUTLEN'   '4',
*
*                                  'S' 'INTC'       ' ',
*                                  ' ' 'KEY'         ' ',
*                                  ' ' 'COLTEXT'     'Intc',
*                                  'E' 'OUTPUTLEN'   '4'.
*  l_cn = '01'.
*  LOOP AT it_date.
*    WRITE it_date-date TO l_datum MM/DD/YY.
*    CONCATENATE 'QTY' l_cn INTO l_qty.
*    PERFORM setting_fieldcat TABLES it_fieldcat USING :
*
*                                   'S' l_qty        ' ',
*                                   ' ' 'KEY'        ' ',
*                                   ' ' 'COLTEXT'    l_datum,
*                                   'E' 'OUTPUTLEN'   '8'.
*    l_cn = l_cn + 1.
*  ENDLOOP.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv.
*
*  CALL METHOD alv_grid->set_table_for_first_display
*
*   EXPORTING   is_layout        = wa_is_layout
*               i_save           = 'A'
*               is_variant       = wa_variant
**               i_default        = space
**               it_toolbar_excluding = it_toolbar_excluding[]
*     CHANGING  it_fieldcatalog  = it_fieldcat[]
*               it_outtab        = it_tab[].
**               it_sort          = it_sort[].
*
ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Form  list_box_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*----------------------------------------------------------------------*

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
*       FORM list_box_function                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_NAME                                                        *
*---------------------------------------------------------------------*
FORM list_box_function USING p_name.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id              = p_name  " list box
            values          = list
       EXCEPTIONS
            id_illegal_name = 1
            OTHERS          = 2.

ENDFORM.                    " list_box_function
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
*&      Form  check_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_data.
  LOOP AT s_rp.
    IF s_rp-low > '28' OR
      s_rp-low < '00'.
      MESSAGE e000 WITH 'Reporting point not exist'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_data
*&---------------------------------------------------------------------*
*&      Form  read_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0266   text
*      -->P_L_ATINN_MODEL  text
*----------------------------------------------------------------------*
FORM read_atinn USING    pa_char
                         pa_atinn.
  SELECT SINGLE atinn INTO pa_atinn
   FROM cabn
  WHERE atnam = pa_char.

ENDFORM.                    " read_atinn
*&---------------------------------------------------------------------*
*&      Form  get_source_list
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_source_list.
  SELECT * INTO TABLE it_eord
  FROM eord
  WHERE lifnr IN s_lifnr
   AND vdatu <= p_datum
   AND bdatu >= p_datum
   AND notkz = ' '.
  IF sy-subrc = 0.
  ELSE.
    MESSAGE e000 WITH 'No vendor found in source list'.
  ENDIF.

ENDFORM.                    " get_source_list
