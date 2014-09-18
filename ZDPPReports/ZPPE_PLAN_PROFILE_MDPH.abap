************************************************************************
* Program Name      : ZPPE_PLAN_PROFILE_MDPH
* Author            : Furong Wang
* Creation Date     : 05/2010
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date       Developer    RequestNo    Description
* 11172010   sjlee                     Add Condition - Req by Daniel
************************************************************************
REPORT zppe_plan_profile_mdph NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID zmpp .
TYPE-POOLS: slis .
TABLES: marc, cuvtab_valc, ztbm_abycfidt.

DATA: BEGIN OF it_data OCCURS 0,
      matnr LIKE marc-matnr,
      cuobj LIKE marc-cuobj,
      END OF it_data.
** BDC

DATA:  ctumode LIKE ctu_params-dismode VALUE 'N',
       cupdate LIKE ctu_params-updmode VALUE 'A',
       bdc_opt LIKE ctu_params,
       bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
       messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

DATA: w_msg(255),
      w_flag(1),
      w_index LIKE sy-tabix.

** ALV
DATA : it_fieldcat     TYPE lvc_t_fcat WITH HEADER LINE,
       it_fieldname    TYPE slis_t_fieldcat_alv,
       it_sort         TYPE lvc_t_sort WITH HEADER LINE.

DATA : wa_is_layout TYPE lvc_s_layo, "/The Layout Structure
       w_fieldname    LIKE LINE OF it_fieldcat.

DATA: wa_save    TYPE c   VALUE 'A',   "for Parameter I_SAVE
      wa_variant TYPE disvariant,      "for parameter IS_VARIANT
       it_exclude TYPE ui_functions.

DATA: wa_custom_control TYPE        scrfname VALUE 'ALV_CONTAINER',
      alv_grid          TYPE REF TO cl_gui_alv_grid,
      grid_container    TYPE REF TO cl_gui_custom_container.

DATA: ok_code LIKE sy-ucomm,
      w_code LIKE sy-ucomm,
      w_old_code LIKE sy-ucomm,
      w_repid LIKE sy-repid,
      w_cnt   TYPE   i.


SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE text-001.
*# 11172010   + sjlee add Select Condition

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK checkbox WITH FRAME .
PARAMETERS    : p_chk AS CHECKBOX DEFAULT 'X'  USER-COMMAND disp.
SELECTION-SCREEN END OF BLOCK checkbox.

SELECTION-SCREEN BEGIN OF BLOCK inputbox WITH FRAME.
*SELECT-OPTIONS: S_FSC FOR MARC-MATNR OBLIGATORY .
SELECT-OPTIONS: s_fsc FOR marc-matnr MODIF ID mtn ,
                s_zbdat FOR sy-datum MODIF ID dat.

SELECTION-SCREEN END OF BLOCK inputbox.
*# 11172010   -
SELECTION-SCREEN END OF BLOCK block1.



AT SELECTION-SCREEN OUTPUT.


  PERFORM change_screen.

INITIALIZATION.

START-OF-SELECTION.
*# 11172010   + sjlee add Select Condition
  IF p_chk EQ 'X'.
    IF s_zbdat[] IS INITIAL.
      MESSAGE s000 WITH text-i02.
      EXIT.
    ENDIF.
  ELSE.
    IF s_fsc[] IS INITIAL.
      MESSAGE s000 WITH text-i03.
      EXIT.
    ENDIF.
  ENDIF.
*# 11172010   -

  PERFORM get_data.
  IF it_data[] IS INITIAL.
    MESSAGE s001 WITH text-i01.
  ELSE.
    PERFORM process_data.
  ENDIF.
*  CALL SCREEN 0200.

*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM init_data.
ENDFORM.                    " INIT_DATA
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING    program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0305   text
*      -->P_0306   text
*----------------------------------------------------------------------*
FORM bdc_field USING    fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    " bdc_field

*&---------------------------------------------------------------------*
*&      Form  bdc_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0315   text
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode p_flag p_msg.

*  DATA: L_SUBRC LIKE SY-SUBRC,
*  DATA:  MSG(255).

  bdc_opt-dismode = ctumode.
  bdc_opt-updmode  = cupdate.

  REFRESH: messtab.
  CLEAR: p_msg, p_flag.
  CALL TRANSACTION tcode USING bdcdata
                   OPTIONS FROM bdc_opt
*                   MODE   CTUMODE
*                   UPDATE CUPDATE
                   MESSAGES INTO messtab.

  IF sy-subrc <> 0.
    REFRESH bdcdata.
    IF sy-msgid = '00' AND sy-msgno = '352'.
      p_flag = 'S'.
    ELSE.
      p_flag = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = p_msg.
    ENDIF.
  ELSE.

    READ TABLE messtab WITH KEY msgtyp = 'E'.

    IF sy-subrc = 0.
      REFRESH bdcdata.
      p_flag = 'E'.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = sy-msgid
          msgnr               = sy-msgno
          msgv1               = sy-msgv1
          msgv2               = sy-msgv2
          msgv3               = sy-msgv3
          msgv4               = sy-msgv4
        IMPORTING
          message_text_output = p_msg.
*    MESSAGE E002 WITH 'Return Document Creation Failed '
*                       W_RSO MSG.
    ELSE.
      READ TABLE messtab WITH KEY msgtyp = 'A'.

      IF sy-subrc = 0.
        REFRESH bdcdata.
        p_flag = 'E'.
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = p_msg.
*      MESSAGE E003 WITH 'Return Document Creation Failed'
*                         W_RSO W_ODNO MSG.
      ELSE.
        p_flag = 'S'.
        REFRESH bdcdata.

      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " bdc_transaction
*&---------------------------------------------------------------------*
*&      Form  CLEAR_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear_data.
ENDFORM.                    " CLEAR_DATA
*
*&---------------------------------------------------------------------*
*&      Form  READ_NORMAL_CLASS_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_APP245_OBJEK  text
*      -->P_0276   text
*      <--P_IT_TEMP_APP245_SUMINF  text
*----------------------------------------------------------------------*
FORM read_normal_class USING p_objek p_char
                              CHANGING p_value.
  SELECT SINGLE au~atwrt
    INTO p_value
    FROM ausp AS au
      INNER JOIN cabn AS ca ON au~atinn = ca~atinn
    WHERE objek = p_objek      AND
          klart = '002'       AND
          ca~atnam = p_char  .
ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'ST200'.
*  ELSE.
*    SET PF-STATUS 'ST100'.
*  ENDIF.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_alv OUTPUT.
  IF grid_container IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM create_container_n_object.
    PERFORM set_attributes_alv_grid.
    PERFORM build_sortcat_display.
    PERFORM exclude_tb_functions.
    PERFORM build_field_catalog USING 'IT_ITAB1'.
    PERFORM assign_itab_to_alv.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD alv_grid->refresh_table_display.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  w_code = ok_code.
  CASE ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
*    WHEN 'EXEC'.
*      PERFORM PROCESS_DATA.
*    WHEN 'VL03N'.
*      PERFORM CALL_VL03N.
*    WHEN 'VA03'.
*      PERFORM CALL_VA03.
*    WHEN 'MB03'.
*      PERFORM CALL_MB03.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
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
      i_default            = space
*     it_toolbar_excluding = IT_EXCLUDE[]
    CHANGING
      it_fieldcatalog      = it_fieldcat[]
      it_outtab            = it_data[]
      it_sort              = it_sort[].

** ENTER
  CALL METHOD alv_grid->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

** Cursor----
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.
*
*  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.

ENDFORM.                    " assign_itab1_to_alv
*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_attributes_alv_grid.
  CLEAR : wa_is_layout, wa_variant.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  wa_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  wa_is_layout-language   = sy-langu. "/Language Key
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  wa_variant-report       = sy-repid.
  wa_variant-username     = sy-uname.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM build_sortcat_display.

  it_sort-spos           = 1.
  it_sort-fieldname      = 'MATNR'.
  it_sort-up             = 'X'.
  it_sort-subtot         = 'X'.
  APPEND it_sort.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM build_field_catalog USING p_itab.

  DATA: lw_itab TYPE slis_tabname.
*        lw_waers LIKE t001-waers,

  CLEAR: it_fieldcat,  it_fieldcat[],
         it_fieldname, it_fieldname[].
  CLEAR: w_repid.

  lw_itab = p_itab.

  w_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name     = w_repid
      i_internal_tabname = lw_itab
      i_inclname         = w_repid
    CHANGING
      ct_fieldcat        = it_fieldname.

  PERFORM setting_fieldcat TABLES it_fieldcat USING :

                                  'S' 'MATNR'    ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Engine No',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'SERIAL'    ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Serial No',
                                  'E' 'OUTPUTLEN'   '18',


                                  'S' 'QTY'    ' ',
                                  ' ' 'COLTEXT'     'Quantity',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'RSO'     ' ',
                                  ' ' 'COLTEXT'     'Return SO',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'RSO_DATE'     ' ',
                                  ' ' 'COLTEXT'     'Return SO Date',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ODNO'       ' ',
                                  ' ' 'COLTEXT'     'Return Delivery',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'ODNO_DATE'       ' ',
                                  ' ' 'COLTEXT'     'Return Del Date',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'GRNO'       ' ',
                                  ' ' 'COLTEXT'     'Return Matl',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'GRNO_DATE'       ' ',
                                  ' ' 'COLTEXT'     'R Matl Date',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'MESS'       ' ',
                                  ' ' 'COLTEXT'     'Error Message',
                                  'E' 'OUTPUTLEN'   '255'.


ENDFORM.                    "BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_container_n_object.
  CLEAR: w_repid.
  CREATE OBJECT grid_container
    EXPORTING
      container_name              = wa_custom_control
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
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
    EXPORTING
      i_parent      = grid_container
      i_appl_events = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
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

*&---------------------------------------------------------------------*
*&      Form  SELECT_EDIT_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_edit_line.
*
*  DATA: LT_CELLTAB TYPE LVC_T_STYL,
*         W_CELLTAB TYPE LVC_S_STYL,
*         L_INDEX TYPE I,
*         L_MODE TYPE RAW4.
*
*  L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*  LOOP AT IT_ITAB1.
*    L_INDEX = SY-TABIX.
*    REFRESH LT_CELLTAB.
*    IF IT_ITAB1-FIRST = 'X'.
*      L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*    ELSE.
*      L_MODE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    ENDIF.
*
*    W_CELLTAB-FIELDNAME = 'SEL'.
*    W_CELLTAB-STYLE = L_MODE.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'TRUCK_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'RACK_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'ITEM_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'ASSYID'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    W_CELLTAB-FIELDNAME = 'JIT_CALL_NO'.
*    W_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    INSERT W_CELLTAB INTO TABLE LT_CELLTAB.
*
*    INSERT LINES OF LT_CELLTAB INTO TABLE IT_ITAB1-CELLTAB.
*    MODIFY IT_ITAB1 INDEX L_INDEX.
*  ENDLOOP.

ENDFORM.                    " SELECT_EDIT_LINE



*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.
  DATA: l_inrecno LIKE ibin-in_recno,
        l_class LIKE klah-class,
        l_cuobj LIKE inob-cuobj,
        l_profilid LIKE tphvp-profilid,
        l_model(2),
        l_slnid(5),
        l_cn(2) TYPE n,
        l_char(20),
        l_text(20).

  DATA: BEGIN OF lt_ibin_val OCCURS 0,
        atinn LIKE v_ibin_syval-atinn,
        atwrt LIKE  v_ibin_syval-atwrt,
        atnam LIKE cabn-atnam,
        END OF lt_ibin_val.

  DATA: BEGIN OF lt_ausp OCCURS 0,
        atinn LIKE v_ibin_syval-atinn,
        atwrt LIKE  v_ibin_syval-atwrt,
        atnam LIKE cabn-atnam,
        END OF lt_ausp.

  DATA: BEGIN OF lt_tpsvp OCCURS 0,
         profilid  LIKE tpsvp-profilid,
         clint LIKE tpsvp-clint,
         lnpos LIKE tpsvp-lnpos,
         pl_rel LIKE tpsvp-pl_rel,
         atinn LIKE cuvtab_valc-atinn,
         valc LIKE cuvtab_valc-valc,
         END OF lt_tpsvp.

  DATA : scnt TYPE i,
         fcnt TYPE i,
         l_flag like w_flag,
         l_msg  like w_msg,
         l_lines TYPE i,
         l_times TYPE i,
         l_max TYPE i,
         l_index LIKE sy-tabix,
         l_index_to LIKE sy-tabix,
         lt_temp LIKE TABLE OF lt_ibin_val WITH HEADER LINE.

  CLEAR : scnt , fcnt.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = '50'
      text       = 'Data is processing.  PLEASE WAIT ....'.

  LOOP AT it_data.
     move: 'S'   to w_flag,
           space to w_msg.

    SELECT SINGLE profilid INTO l_profilid
      FROM tphvp
      WHERE objekt = it_data-matnr.
    IF sy-subrc = 0.
      MESSAGE i001 WITH it_data-matnr ' is existed'.
      CONTINUE.
    ENDIF.
    REFRESH: lt_ausp, lt_tpsvp, lt_ibin_val.
    CLEAR: l_inrecno,l_cuobj,l_model,l_class,l_cn.

    SELECT SINGLE in_recno INTO l_inrecno
      FROM ibin
      WHERE instance = it_data-cuobj.

    SELECT a~atinn atwrt atnam INTO TABLE lt_ibin_val
     FROM v_ibin_syval AS a
     INNER JOIN cabn AS b
     ON a~atinn = b~atinn
     WHERE in_recno = l_inrecno.

    SELECT SINGLE class cuobj INTO (l_class, l_cuobj)
      FROM inob AS a
      INNER JOIN kssk AS b
      ON a~cuobj = b~objek
      INNER JOIN klah AS c
      ON b~clint = c~clint
      WHERE a~objek = it_data-matnr
       AND a~klart = '300'
       AND obtab = 'MARA'
       AND mafid = 'O'.    "       AND ZAEHL = '10'.

    l_model = l_class+0(2).

    SELECT a~atinn atwrt atnam INTO TABLE lt_ausp
        FROM ausp AS a
        INNER JOIN cabn AS b
        ON a~atinn = b~atinn
        WHERE objek = l_cuobj.

** Fuorng on 05/20/13
    CLEAR: l_lines.
    REFRESH: lt_temp.
    DESCRIBE TABLE lt_ibin_val LINES l_lines.
    lt_temp[] = lt_ibin_val[].
    l_max = l_lines DIV 32 + 1.
    l_index  = 1.
    l_times = 1.
    WHILE l_times =< l_max.
      l_index_to = l_times * 31.
      REFRESH: lt_ibin_val.
      LOOP AT lt_temp FROM l_index TO l_index_to.
        lt_ibin_val = lt_temp.
        APPEND lt_ibin_val.
      ENDLOOP.
      l_index = l_index_to + 1.
      IF l_times = 1.
        l_times = l_times + 1.
** End on 05/20/13

** Call BDC

        PERFORM bdc_dynpro      USING 'SAPMM60X' '0510'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RM60B-MATNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=CHAN'.
        PERFORM bdc_field       USING 'RM60B-MATNR'
                                       it_data-matnr.
        PERFORM bdc_dynpro      USING 'SAPLM60P' '0105'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RM60PROF-PROFTXT(01)'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=PICL'.
        PERFORM bdc_field       USING 'RM60PROF-SELKZ(01)'
                                      'X'.     "record-SELKZ_01_002.
        PERFORM bdc_field       USING 'RM60PROF-PROFTXT(01)'
                                      it_data-matnr.
        PERFORM bdc_field       USING 'RM60PROF-VISUD(01)'
                                      'X'.
        PERFORM bdc_dynpro      USING 'SAPLM60P' '0110'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RM60B-MATNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=NEWL'.
        PERFORM bdc_dynpro      USING 'SAPLM60P' '0110'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RM60REL-ATWTB(17)'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=SICH'.

*# 11182010 Append Success count Fail count by sjlee req. daniel
** Furong on 05/20/13
      ELSE.
        l_times = l_times + 1.
        PERFORM bdc_dynpro      USING 'SAPMM60X' '0510'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RM60B-MATNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=CHAN'.
        PERFORM bdc_field       USING 'RM60B-MATNR'
                                      it_data-matnr.
        PERFORM bdc_dynpro      USING 'SAPLM60P' '0110'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RM60B-MATNR'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=NEWL'.
        PERFORM bdc_dynpro      USING 'SAPLM60P' '0110'.
        PERFORM bdc_field       USING 'BDC_CURSOR'
                                      'RM60REL-ATWTB(01)'.
        PERFORM bdc_field       USING 'BDC_OKCODE'
                                      '=SICH'.
*        PERFORM bdc_field       USING 'RM60REL-ATWTB(01)'
*                                      record-atwtb_01_002.
      ENDIF.
*    DATA : SCNT TYPE I,
*           FCNT TYPE I.
*
*    CLEAR : SCNT , FCNT.
      clear: l_cn.
**

      LOOP AT lt_ibin_val.
        l_cn = l_cn + 1.
        CONCATENATE l_model '_' lt_ibin_val-atnam INTO l_char.

        CONCATENATE 'RM60REL-ATWTB(' l_cn ')' INTO l_text.

        PERFORM bdc_field       USING l_text
                                l_char.
      ENDLOOP.


      PERFORM bdc_transaction USING 'MDPH' l_flag l_msg.
      if l_flag eq 'E'.
        move: l_flag to w_flag,
              l_msg  to w_msg.
      endif.

** On 05/20/13
      WAIT UP TO 3 SECONDS.
    ENDWHILE.
** End on 05/20/13
    IF w_flag = 'S'.
*# 11182010 Success Count by sjlee
      scnt = scnt + 1.
      WAIT UP TO 3 SECONDS.

      SELECT a~profilid clint lnpos pl_rel atinn valc INTO TABLE lt_tpsvp
                                             FROM tphvp AS a
                                             INNER JOIN tpsvp AS b
                                             ON a~profilid = b~profilid
                                             INNER JOIN cuvtab_valc AS c
                                               ON b~clint = c~vtint
                                              AND b~lnpos = c~slnid
                                            WHERE objekt = it_data-matnr.

      LOOP AT lt_tpsvp.
        IF lt_tpsvp-atinn = '4051' OR lt_tpsvp-atinn = '4052'.
        ELSE.
          READ TABLE lt_ausp WITH KEY atinn = lt_tpsvp-atinn
                                          atwrt = lt_tpsvp-valc.
          IF sy-subrc = 0.
            UPDATE tpsvp SET pl_rel = 'X'
               WHERE profilid = lt_tpsvp-profilid
                    AND clint = lt_tpsvp-clint
                    AND lnpos = lt_tpsvp-lnpos.
          ELSE.
            UPDATE tpsvp SET pl_rel = ' '
               WHERE profilid = lt_tpsvp-profilid
                 AND clint = lt_tpsvp-clint
                 AND lnpos = lt_tpsvp-lnpos.
          ENDIF.
        ENDIF.
        CLEAR lt_ausp.
      ENDLOOP.
      COMMIT WORK.
    ELSE.
*# 11182010 Fail Count by sjlee
      fcnt = fcnt + 1.
      MESSAGE w001 WITH 'BDC Error:' w_msg.
    ENDIF.
  ENDLOOP.

  PERFORM end_log USING w_msg fcnt scnt.
ENDFORM.                    "PROCESS_DATA
" PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  PRCESS_SO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM PRCESS_SO.
*  DATA: L_FLAG(1),
*        MSG(255),
*        L_TEXT(40),
*        L_DATE(8).
*  DATA: BEGIN OF LT_VBELN  OCCURS 0,
*          VBELN LIKE VBAK-VBELN,
*          ERZET LIKE VBAK-ERZET,
*          END OF LT_VBELN.
*
*  CONCATENATE P_DATE+4(2) P_DATE+6(2) P_DATE+0(4) INTO L_DATE.
*
*  PERFORM BDC_DYNPRO      USING 'SAPMV45A' '0101'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'VBAK-AUART'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=COPY'.
*  PERFORM BDC_FIELD       USING 'VBAK-AUART'
*                                'ZERE'.
*  PERFORM BDC_DYNPRO      USING 'SAPLV45C' '0100'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=UEBR'.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'LV45C-VBELN'.
*  PERFORM BDC_FIELD       USING 'LV45C-VBELN'
*                                P_VBELN.
*
*  PERFORM BDC_DYNPRO      USING 'SAPMV45A' '4001'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=SICH'.
*  PERFORM BDC_FIELD       USING 'RV45A-KETDAT'
*                                L_DATE.
*  PERFORM BDC_FIELD       USING 'BDC_CURSOR'
*                                'RV45A-KWMENG(01)'.
*
*  LOOP AT IT_ITEM.
*    CONCATENATE 'RV45A-KWMENG(' IT_ITEM-LINE ')' INTO L_TEXT.
*    PERFORM BDC_FIELD       USING L_TEXT
*                                  IT_ITEM-QTY.
*  ENDLOOP.
*
*  PERFORM BDC_DYNPRO      USING 'SAPLSPO2' '0101'.
*  PERFORM BDC_FIELD       USING 'BDC_OKCODE'
*                                '=OPT1'.
*  PERFORM BDC_TRANSACTION USING 'VA01' L_FLAG W_RSO_MESS.
*
*  IF L_FLAG = 'S'.
*    WAIT UP TO 3 SECONDS.
*    SELECT A~VBELN ERZET INTO TABLE LT_VBELN
*     FROM VBAK AS A
*     INNER JOIN VBUP AS B
*     ON A~VBELN = B~VBELN
*     WHERE AUART = 'ZERE'
*       AND VDATU = P_DATE
*       AND ERNAM = SY-UNAME
*       AND LFSTA <> 'C'.
*
*    IF SY-SUBRC <> 0.
*      W_RSO_MESS = 'Return SO Creation Failed, No Dta in VBAK'.
*    ELSE.
*      SORT LT_VBELN DESCENDING BY VBELN ERZET .
*      READ TABLE LT_VBELN INDEX 1.
*      W_RSO = LT_VBELN-VBELN.
**      MESSAGE S001 WITH 'Return SO Successfully Created '
**              W_RSO.
**      PERFORM UPDATE_SO.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.                    " PRCESS_SO
*&---------------------------------------------------------------------*
*&      Form  CALL_MB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM CALL_MB03.
*  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
*            LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
*
*  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
*           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
*                     ET_ROW_NO     = LT_ROW_NO.
*
*  CALL METHOD CL_GUI_CFW=>FLUSH.
*
*  IF SY-SUBRC NE 0.
*    W_REPID = SY-REPID.
*    CALL FUNCTION 'POPUP_TO_INFORM'
*         EXPORTING
*              TITEL = W_REPID
*              TXT2  = SY-SUBRC
*              TXT1  = 'Error found during flushing of ALV Grid Control'
*.
*    EXIT.
*  ENDIF.
**
**  CLEAR: w_select, w_success, w_fail.
*
*  READ TABLE LT_ROWS INDEX 1.
*  IF SY-SUBRC NE 0.
*    MESSAGE E000(ZZ) WITH TEXT-M05.
*  ENDIF.
*  READ TABLE IT_ITAB1 INDEX LT_ROWS-INDEX.
*
*  IF SY-SUBRC = 0.
*    W_INDEX = LT_ROWS-INDEX.
*    IF IT_ITAB1-RSO IS INITIAL.
*      MESSAGE E000(ZZ) WITH TEXT-M06.
*    ENDIF.
*    SET PARAMETER ID 'MBN' FIELD IT_ITAB1-GRNO.
*    CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.
*  ENDIF.
*
*ENDFORM.                                                    " CALL_MB03
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exclude_tb_functions.
  DATA ls_exclude TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO it_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO it_exclude.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.

*# 11172010 + sjlee append logic
  IF p_chk EQ 'X' .
    DATA : lt_ztbm LIKE TABLE OF ztbm_abycfidt WITH HEADER LINE.
    CLEAR :  s_fsc[], s_fsc.

    SELECT mtno INTO CORRESPONDING FIELDS OF TABLE lt_ztbm
      FROM ztbm_abycfidt
      WHERE zresult = 'S'
        AND zbdat   IN s_zbdat.

    LOOP AT lt_ztbm.
      s_fsc-low = lt_ztbm-mtno.
      s_fsc-sign = 'I'.
      s_fsc-option = 'EQ'.
      APPEND s_fsc.
    ENDLOOP.
  ENDIF.
  CHECK NOT s_fsc[] IS INITIAL.
*# 11172010 -

  SELECT matnr cuobj INTO TABLE it_data
  FROM marc
  WHERE matnr IN s_fsc.

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SCREEN
*&---------------------------------------------------------------------*
*       Add Screen Logic 11172010 sjlee
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_screen.
  IF p_chk EQ 'X' .
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'MTN'.
          screen-active = 0.
        WHEN 'DAT'.
          screen-active = 1.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      CASE screen-group1.
        WHEN 'MTN'.
          screen-active = 1.
        WHEN 'DAT'.
          screen-active = 0.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CHANGE_SCREEN
*&---------------------------------------------------------------------*
*&      Form  END_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_MSG  text
*----------------------------------------------------------------------*
FORM end_log USING    p_msg
                      p_fcnt TYPE i
                      p_scnt TYPE i.
  DATA : lv_cnt TYPE i.
  DESCRIBE TABLE it_data LINES lv_cnt.

  WRITE : AT /  text-t01 ,
          AT 60 lv_cnt,
          AT /  text-t02 ,
          AT 60 p_scnt ,
          AT /  text-t03 ,
          AT 60 p_fcnt.

ENDFORM.                    " END_LOG
