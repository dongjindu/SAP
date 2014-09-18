*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM15I_6013F01
*----------------------------------------------------------------------*
FORM get_data.
* set date
  PERFORM set_date.
* select basic data
  PERFORM select_basicdata.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  set_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_date.

  perform get_begdat_month using p_date changing w_1st_date.
  perform get_nxt_month using w_1st_date changing w_2nd_date.
  perform get_nxt_month using w_2nd_date changing w_3rd_date.
  perform get_nxt_month using w_3rd_date changing w_4th_date.
  perform get_nxt_month using w_4th_date changing w_5th_date.
  perform get_nxt_month using w_5th_date changing w_6th_date.
*&---For Display.
  w_mon1 = w_1st_date(6).
  w_mon2 = w_2nd_date(6).
  w_mon3 = w_3rd_date(6).
  w_mon4 = w_4th_date(6).
  w_mon5 = w_5th_date(6).
  w_mon6 = w_6th_date(6).
ENDFORM.                    " set_date
*&---------------------------------------------------------------------*
*&      Form  select_basicdata
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_basicdata.

  perform get_material_ltp.

  perform calculate_requiremnt_quantity.

  perform get_characteristics.

ENDFORM.                    " select_basicdata
*&---------------------------------------------------------------------*
*&      Form  alv_field_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_field_build.
  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[],
          it_list_top_of_page[]     .
* set fields
  PERFORM fieldcat_init.
* set event
  PERFORM eventtab_build USING wa_events[].
* set list heading
  PERFORM comment_build  USING it_list_top_of_page[].
ENDFORM.                    " alv_field_build
*&---------------------------------------------------------------------*
*&      Form  fieldcat_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  'X'             'ZDOCNO'     'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'ZDOCNO'     ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'App.DocNo.'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  'X'             'LOGNO_H'    'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'LOGNO_H'    ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'Log no.'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'MARA_MATNR' 'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'MARA_MATNR' ''
                  ''              ''           '18'
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'Component'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'DOCTYPE'    'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'DOCTYPE'    ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  ''              ''           ''
                  'Doc. Type'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'STAND_YYYYMM'   'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'STAND_YYYYMM'   ''
                  ''              ''               ''
                  ''              ''               ''
                  ''              ''               ''
                  ''              ''               ''
                  'STAND_Month'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'STAND_YYYYMMDD' 'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'STAND_YYYYMMDD' ''
                  ''              ''               ''
                  ''              ''               ''
                  ''              ''               ''
                  ''              ''               ''
                  'STAND_Date'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'MATL'         'IT_ZTMM_6013_01'
                  'ZTMM_6013_01'  'MATL'         ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Mat.Property'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'COAT_QTY'     'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'COAT_QTY'     ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Coating'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'THICK'        'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'THICK'        ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Thick'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'WIDTH'        'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'WIDTH'        ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Width'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'LENGTH'       'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'LENGTH'       ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  ''              ''             ''
                  'Length'.


  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'IN_OUT'        'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'IN_OUT'        ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  'Inner/Outside'.
  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M'             'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M'             ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  w_mon1.
  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M1'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M1'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  w_mon2.
  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M2'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M2'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  w_mon3.
  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M3'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M3'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  w_mon4.
  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M4'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M4'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  w_mon5.
  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M5'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M5'            ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  ''              ''              ''
                  w_mon6.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM'            'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM'            ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M-Confirmed QTY'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W'           ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M_W'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W1'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W1'          ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M W+1'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W2'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W2'          ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M W+2'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W3'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W3'          ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M W+3'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W4'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W4'          ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M W+4'.
  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'M_W5'          'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'M_W5'          ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M W+5'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM1'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM1'           ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M+1:Confirmed QTY'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM2'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM2'           ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M+3:Confirmed QTY'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM3'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM3'           ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M+3:Confirmed QTY'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM4'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM4'           ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M+4:Confirmed QTY'.

  PERFORM catalog
           TABLES it_fieldcat
           USING
                  ''              'CM5'           'IT_ZTMM_6013_01'
                  'ZTMM_6013_01' 'CM5'           ''
                  ''              ''              ''
                  ''              ''              'X'
                  ''              ''              ''
                  ''              ''              ''
                  'M+5:Confirmed QTY'.


ENDFORM.                    " fieldcat_init
*&---------------------------------------------------------------------*
*&      Form  eventtab_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM eventtab_build
                   USING e03_lt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = e03_lt_events.

  READ TABLE e03_lt_events INTO ls_event
               WITH KEY name =  slis_ev_top_of_page.

  IF sy-subrc = 0.
    MOVE c_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  comment_build
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build
               USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line  TYPE slis_listheader.
  DATA: lw_info  TYPE slis_entry,
        lw_name1 LIKE t001w-name1,
        lw_maktx LIKE makt-maktx,
        lw_ekotx LIKE t024e-ekotx,
        lw_eknam LIKE t024-eknam.

*/Begin of Added by Hakchin(20040408)
* title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

  APPEND INITIAL LINE TO lt_top_of_page.

  ls_line-typ = 'S'.
  CONCATENATE 'Requirement Date:'
              p_date
          INTO ls_line-info
          SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.

*/End of Added by Hakchin(20040408)

ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = it_list_top_of_page.

ENDFORM.
*---------------------------------------------------------------------*
*       FORM ps_tb                                                    *
*---------------------------------------------------------------------*
FORM ps_tb USING rt_extab TYPE slis_t_extab.
*  CLEAR: rt_extab[], rt_extab.
  DATA: lv_numbering       TYPE i.
  DATA: lv_numbering_c(10) TYPE c.
*  DESCRIBE TABLE it_ztmm_6013_01 LINES lv_numbering.
  lv_numbering_c = lv_numbering.
* Instanciate PF-STATUS & TITLEBAR.
  DATA: lv_title(80).         " Title
  lv_title = 'IMMGM15 Requirement Info'.
  SET TITLEBAR 'TB' WITH lv_title.

*  SET PF-STATUS 'PS' EXCLUDING 'TRAN'.  "Transfer
  SET PF-STATUS 'PS'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data.

  field-symbols: <fs_mltp> like line of it_mat_ltp.

  sort it_mat_info by matnr.

  loop at it_mat_ltp assigning <fs_mltp>.
    move: 'LS'             to wa_ztmm_6013_02-doctype,
          'H201'           to wa_ztmm_6013_02-bukrs,
          'P001'           to wa_ztmm_6013_02-werks,
          p_date(6)        to wa_ztmm_6013_02-stand_yyyymm,
          p_date           to wa_ztmm_6013_02-stand_yyyymmdd,
          <fs_mltp>-matnr  to wa_ztmm_6013_02-mara_matnr,
          <fs_mltp>-meins  to wa_ztmm_6013_02-meins.
    read table it_mat_info into wa_mat_info
                         with key matnr = <fs_mltp>-matnr binary search.
    if sy-subrc ne 0.
      move: space to wa_ztmm_6013_02-matl,
            space to wa_ztmm_6013_02-coat_qty,
            space to wa_ztmm_6013_02-thick,
            space to wa_ztmm_6013_02-width,
            space to wa_ztmm_6013_02-length,
            space to wa_ztmm_6013_02-in_out.
    else.
      move: wa_mat_info-mprop  to wa_ztmm_6013_02-matl,
            wa_mat_info-mcoat  to wa_ztmm_6013_02-coat_qty,
            wa_mat_info-mthick to wa_ztmm_6013_02-thick,
            wa_mat_info-mwidth to wa_ztmm_6013_02-width,
            wa_mat_info-mleng  to wa_ztmm_6013_02-length,
            wa_mat_info-minout to wa_ztmm_6013_02-in_out.
    endif.
    move: <fs_mltp>-mng01 to wa_ztmm_6013_02-m,
          <fs_mltp>-mng02 to wa_ztmm_6013_02-m1,
          <fs_mltp>-mng03 to wa_ztmm_6013_02-m2,
          <fs_mltp>-mng04 to wa_ztmm_6013_02-m3,
          <fs_mltp>-mng05 to wa_ztmm_6013_02-m4,
          <fs_mltp>-mng06 to wa_ztmm_6013_02-m5.
    append wa_ztmm_6013_02 to it_ztmm_6013_02.
  endloop.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_1714   text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM number_get_next
           USING    value(p_nro_interval) LIKE inri-nrrangenr
                    value(p_nro_object)   LIKE inri-object
           CHANGING value(p_nro_next).
  CLEAR: p_nro_next.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            nr_range_nr             = p_nro_interval
            object                  = p_nro_object
       IMPORTING
            number                  = p_nro_next
       EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            OTHERS                  = 7.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    "number_get_next
*&---------------------------------------------------------------------*
*&      Form  z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_fca_eai_interface_log.
*/ Function Module for Interface Log
*
*Where to be inserted:
* 1. Inbound: When interface table is updated after Standard BDC/BAPI
*             executed.
* 2. Outbound: After calling EAI
*
*====================================================================
*
*Function name : Z_FCA_EAI_INTERFACE_LOG
*
*Import/Export Parameter Structure : ZTCA_IF_LOG
*
*IFDOC   <= Serial No. for Log. Leave as empty
*TCODE   <= Present Transaction Code
*TOTAL   <= Total Execution number
*ZSUCC   <= Successful occurrences(number) for BDC/BAPI Processing
*ERROR   <= Failed occurrences(number) for BDC/BAPI Processing
*ERDAT   <= Created on.
*ERZET   <= Created time.
*ERNAM   <= Creator.
*AEDAT   <= Changed on.
*AEZET   <= Changed time
*AENAM   <= the person who change

  DATA: lv_total TYPE i.
  DESCRIBE TABLE it_ztmm_6013_02 LINES lv_total.

  CHECK NOT lv_total IS INITIAL.
  CLEAR: wa_ztca_if_log.
  LOOP AT it_ztmm_6013_01 ASSIGNING <fs_ztmm_6013_01>.
    IF <fs_ztmm_6013_01>-zzret = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF <fs_ztmm_6013_01>-zzret = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  wa_ztca_if_log-tcode = 'ZMMI69'. "Present Transaction Code
  wa_ztca_if_log-total = lv_total. "Total Execution number
  wa_ztca_if_log-erdat = sy-datum. "Created on.
  wa_ztca_if_log-erzet = sy-uname. "Created time.
  wa_ztca_if_log-ernam = sy-uname. "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log     = wa_ztca_if_log
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*&      Form  dsp_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dsp_log.

* set alv parameters
  PERFORM alv_field_build.


*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK              = ' '
*   I_BYPASSING_BUFFER             =
*   I_BUFFER_ACTIVE                = ' '
     i_callback_program             = w_repid
     i_callback_pf_status_set       = 'PS_TB'
*     i_callback_user_command        = 'USER_COMMAND'
      i_structure_name               = 'ZTMM_6013_01'
"You can ALV w/o Structure. At this time it_fieldcat[] have to be used.
*   IS_LAYOUT                      =
     it_fieldcat                    = it_fieldcat[]
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        = it_sort[]
*   IT_FILTER                      =
*   IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
   i_save                         = 'A'
*   IS_VARIANT                     =
     it_events                      = wa_events[]
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = it_ztmm_6013_01
   EXCEPTIONS
     program_error                  = 1
     OTHERS                         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " dsp_log
*&---------------------------------------------------------------------*
*&      Form  catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM catalog  TABLES   ta_fieldcat
                         STRUCTURE wa_fieldcat
              USING    value(p_key)
                       value(p_fieldname)
                       value(p_tabname)
                       value(p_ref_tabname)
                       value(p_ref_fieldname)
                       value(p_reptext_ddic)
                       value(p_do_sum)
                       value(p_no_zero)
                       value(p_outputlen)
                       value(p_just)
                       value(p_tech)
                       value(p_no_out)
                       value(p_fix_column)
                       value(p_emphasize)
                       value(p_qfieldname)
                       value(p_qtabname)
                       value(p_ctabname)
                       value(p_cfieldname)
                       value(p_lavel).

  TYPE-POOLS: slis.
  DATA :  lt_fieldcat TYPE slis_t_fieldcat_alv,
          ls_fieldcat TYPE slis_fieldcat_alv,
          l_pos TYPE i.

*  l_pos = l_pos + 1.
  ls_fieldcat-col_pos       = l_pos.
  ls_fieldcat-key           = p_key.
  ls_fieldcat-fieldname     = p_fieldname.
  ls_fieldcat-tabname       = p_tabname.
  ls_fieldcat-ref_tabname   = p_ref_tabname.
  ls_fieldcat-ref_fieldname = p_ref_fieldname.
  ls_fieldcat-reptext_ddic  = p_reptext_ddic.
  ls_fieldcat-do_sum        = p_do_sum.
  ls_fieldcat-no_zero       = p_no_zero.
  ls_fieldcat-outputlen     = p_outputlen.
* --> Sort
  ls_fieldcat-just          = p_just.
  ls_fieldcat-tech          = p_tech.
* --> Hiding FIELD
  ls_fieldcat-no_out        = p_no_out.
  ls_fieldcat-fix_column    = p_fix_column.
* --> Intensify
  ls_fieldcat-emphasize     = p_emphasize.
* --> Quantity field
  ls_fieldcat-qfieldname    = p_qfieldname.
  ls_fieldcat-qtabname      = p_qtabname.
* --> Currency field
  ls_fieldcat-ctabname      = p_ctabname.
  ls_fieldcat-cfieldname    = p_cfieldname.
* --> field label
  ls_fieldcat-seltext_m     = p_lavel.
  ls_fieldcat-seltext_l     = p_lavel.
  ls_fieldcat-seltext_s     = p_lavel.
  ls_fieldcat-ddictxt       = 'M'.

*  APPEND ls_fieldcat TO  lt_fieldcat.
  APPEND ls_fieldcat TO  ta_fieldcat.
ENDFORM.                    " catalog
*&---------------------------------------------------------------------*
*&      Form  get_characteristics
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_characteristics.
  data: mcoat1(3) type c,
        mcoat2(3) type c.

  field-symbols: <fs_ausp> like line of it_ausp.

  select objek atwrt atnam
                    into table it_ausp
                    from ausp
                    inner join cabn
                    on cabn~atinn = ausp~atinn
                    and cabn~adzhl = ausp~adzhl
       where ( klart = '001' and atnam = 'ZSTEEL_MATPROPERTY' )
       OR    ( klart = '001' and atnam = 'ZFRONT_FINISHING_THICKNESS' )
       OR    ( klart = '001' and atnam = 'ZBACK_FINISHING_THICKNESS' )
       OR    ( klart = '001' and atnam = 'ZSPEC_THICK' )
       OR    ( klart = '001' and atnam = 'ZSPEC_WIDTH' )
       OR    ( klart = '001' and atnam = 'ZSPEC_LENGTH' )
       OR    ( klart = '001' and atnam = 'ZIN_OR_OUT' ).

  if sy-subrc ne 0.
    exit.
  endif.

  sort it_ausp by objek atnam.

  loop at it_ausp assigning <fs_ausp>.
    at new objek.
      wa_mat_info-matnr = <fs_ausp>-objek.
      append wa_mat_info to it_mat_info.
    endat.
    case <fs_ausp>-atnam.
      when 'ZSTEEL_MATPROPERTY'.
        wa_mat_info-mprop = <fs_ausp>-atwrt.
        modify it_mat_info from wa_mat_info transporting mprop
                                          where matnr = <fs_ausp>-objek.
      when 'ZBACK_FINISHING_THICKNESS'.
        mcoat1 = <fs_ausp>-atwrt.
      when 'ZFRONT_FINISHING_THICKNESS'.
        mcoat2 = <fs_ausp>-atwrt.
        concatenate mcoat1 mcoat2 into wa_mat_info-mcoat.
        modify it_mat_info from wa_mat_info transporting mcoat
                                          where matnr = <fs_ausp>-objek.
      when 'ZSPEC_THICK'.
        wa_mat_info-mthick = <fs_ausp>-atwrt.
        modify it_mat_info from wa_mat_info transporting mthick
                                          where matnr = <fs_ausp>-objek.
      when 'ZSPEC_WIDTH'.
        wa_mat_info-mwidth = <fs_ausp>-atwrt.
        modify it_mat_info from wa_mat_info transporting mwidth
                                          where matnr = <fs_ausp>-objek.
      when 'ZSPEC_LENGTH'.
        wa_mat_info-mleng = <fs_ausp>-atwrt.
        modify it_mat_info from wa_mat_info transporting mleng
                                          where matnr = <fs_ausp>-objek.
      when 'ZIN_OR_OUT'.
        wa_mat_info-minout = <fs_ausp>-atwrt.
        modify it_mat_info from wa_mat_info transporting minout
                                          where matnr = <fs_ausp>-objek.
    endcase.
    clear: wa_mat_info.
  endloop.

ENDFORM.                    " get_characteristics
*&---------------------------------------------------------------------*
*&      Form  get_monday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_KEYDAT  text
*      <--P_W_1ST_DATE  text
*----------------------------------------------------------------------*
FORM get_monday using    p_keydat
                changing p_day.

  CALL FUNCTION 'GET_WEEK_INFO_BASED_ON_DATE'
       EXPORTING
            DATE   = p_keydat
       IMPORTING
            MONDAY = p_day.

ENDFORM.                    " get_monday
*&---------------------------------------------------------------------*
*&      Form  get_begdat_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_KEYDAT  text
*      <--P_W_1ST_DATE  text
*----------------------------------------------------------------------*
FORM get_begdat_month using    p_keydat
                      changing p_begdat.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
       EXPORTING
            IV_DATE             = p_keydat
       IMPORTING
            EV_MONTH_BEGIN_DATE = p_begdat.

ENDFORM.                    " get_begdat_month

*&---------------------------------------------------------------------*
*&      Form  get_nxt_month
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_1ST_DATE  text
*      <--P_W_2ND_DATE  text
*----------------------------------------------------------------------*
FORM get_nxt_month USING    p_keymonth
                   CHANGING p_nxtmonth..

  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
       EXPORTING
            MONTHS  = 1
            OLDDATE = p_keymonth
       IMPORTING
            NEWDATE = p_nxtmonth.

ENDFORM.                    " get_nxt_month

*&---------------------------------------------------------------------*
*&      Form  get_material_ltp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_material_ltp.

  select mara~matnr werks meins into table it_marc
                                from marc
                                inner join mara
                                on mara~matnr = marc~matnr
                                and mara~lvorm = marc~lvorm
                                where dispo    = 'P01'
                                and marc~lvorm eq space.

  loop at it_marc assigning <fs_mat>.
    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
        PLSCN                          = '900'
        MATNR                          = <fs_mat>-matnr
        WERKS                          = <fs_mat>-werks
*   BERID                          =
*   ERGBZ                          =
*   AFIBZ                          =
        INPER                          = c_perkz
* IMPORTING
*   E_MT61D                        =
*   E_MDKP                         =
     TABLES
*   MDPSX                          =
*   MDEZX                          =
       MDSUX                          = it_mdsu
     EXCEPTIONS
       MATERIAL_PLANT_NOT_FOUND       = 1
       PLANT_NOT_FOUND                = 2
       OTHERS                         = 3 .
    if sy-subrc <> 0.
      continue.
    ENDIF.
    append lines of it_mdsu to it_mdsu1.
    wa_mdsu1-matnr = <fs_mat>-matnr.
    modify it_mdsu1 from wa_mdsu1 transporting matnr
                                  where matnr is initial.
    clear wa_mdsu1.
  endloop.


ENDFORM.                    " get_material_ltp
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.

  sort it_ztmm_6013_02 by mara_matnr.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
   EXPORTING
     I_PROGRAM_NAME               = 'ZIMMGM15I_6013'
     I_INTERNAL_TABNAME           = 'WA_ZTMM_6013_02'
*   I_STRUCTURE_NAME             =
*   I_CLIENT_NEVER_DISPLAY       = 'X'
     I_INCLNAME                   = 'ZIMMGM15I_6013TOP'
*   I_BYPASSING_BUFFER           =
*   I_BUFFER_ACTIVE              =
    CHANGING
      CT_FIELDCAT                  = it_fieldcat1
   EXCEPTIONS
     INCONSISTENT_INTERFACE       = 1
     PROGRAM_ERROR                = 2
     OTHERS                       = 3 .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  perform build_catalog_events.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*   I_INTERFACE_CHECK              = ' '
    I_BYPASSING_BUFFER             = 'X'
*   I_BUFFER_ACTIVE                = ' '
     I_CALLBACK_PROGRAM             = 'ZIMMGM15I_6013'
     I_CALLBACK_PF_STATUS_SET       = 'SET_PF_STATUS'
     I_CALLBACK_USER_COMMAND        = 'USER_COMMAND'
     I_STRUCTURE_NAME               = 'WA_ZTMM_6013_02'
    IS_LAYOUT                      = wa_layout
    IT_FIELDCAT                    = it_fieldcat1
*   IT_EXCLUDING                   =
*   IT_SPECIAL_GROUPS              =
*   IT_SORT                        =
*   IT_FILTER                      =
*    IS_SEL_HIDE                    =
*   I_DEFAULT                      = 'X'
      I_SAVE                         = 'A'
*   IS_VARIANT                     =
    IT_EVENTS                      = it_events
*   IT_EVENT_EXIT                  =
*   IS_PRINT                       =
*   IS_REPREP_ID                   =
*   I_SCREEN_START_COLUMN          = 0
*   I_SCREEN_START_LINE            = 0
*   I_SCREEN_END_COLUMN            = 0
*   I_SCREEN_END_LINE              = 0
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      T_OUTTAB                       = it_ztmm_6013_02
   EXCEPTIONS
     PROGRAM_ERROR                  = 1
     OTHERS                         = 2 .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.                    " dsp_data

*---------------------------------------------------------------------*
*       FORM set_pf_status                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EXTAB                                                      *
*---------------------------------------------------------------------*
form set_pf_status using rt_extab type slis_t_extab.

  set pf-status 'PF102' excluding rt_extab.
endform.
*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  R_UCOMM                                                       *
*---------------------------------------------------------------------*
form user_command using r_ucomm like sy-ucomm
                        rs_selfield type slis_selfield.

  data: lv_logno_h TYPE num10,
        lv_zresult LIKE zsca_if_time_stamp_out-zresult,
        lv_message TYPE bapi_msg, "Message text (220)
        w_okcode like sy-ucomm.

  field-symbols: <fs_ztmm_6013_02> like line of it_ztmm_6013_02.

  w_okcode = r_ucomm.

  check w_okcode = 'ZHYS'.
  loop at it_ztmm_6013_02 assigning <fs_ztmm_6013_02>
                          where zmark = 'X'.
    move-corresponding <fs_ztmm_6013_02> to wa_ztmm_6013_01.
    append wa_ztmm_6013_01 to it_ztmm_6013_01.
  endloop.

  PERFORM number_get_next USING    c_nro_nr_09
                                   'ZMMNRO0002'
                          CHANGING w_zdocno.
  COMMIT WORK.

*/Interface with External System

  CALL FUNCTION 'Z_FMM_6013_OUT_REQINFO'
    DESTINATION              c_dest
    TABLES
      ta_ztmm_6013_01      = it_ztmm_6013_01
    EXCEPTIONS
      communication_failure = 1 MESSAGE lv_message
      system_failure        = 2 MESSAGE lv_message.
  IF sy-subrc NE 0.
    lv_zresult = 'E'.  "Result of the Processing
    MESSAGE s999(zmmm) WITH lv_message.
  ELSE.
    lv_zresult = 'S'.  "Result of the Processing
    lv_message = 'Outbound RFC FM Connected!'(002).
    MESSAGE s999(zmmm) WITH lv_message.
  ENDIF.

*/ Modify it_ZTMM_6013_01
  LOOP AT it_ztmm_6013_01 ASSIGNING <fs_ztmm_6013_01>.
    lv_logno_h = lv_logno_h + 1.

    <fs_ztmm_6013_01>-zdocno  = w_zdocno.  "App. Doc. No.
    <fs_ztmm_6013_01>-logno_h = lv_logno_h."Logno Header

    <fs_ztmm_6013_01>-zuser   = sy-uname.  "User name
*    <fs_ZTMM_6013_01>-zsdat   = .  "Send File Created Date
*    <fs_ZTMM_6013_01>-zstim   = .  "Send file Created Time
    <fs_ztmm_6013_01>-zedat   = sy-datum.  "SAP Interface Date
    <fs_ztmm_6013_01>-zetim   = sy-uzeit.  "SAP Interface Time
    <fs_ztmm_6013_01>-zmode   = 'C'.       "Data Characteristic Flag
    <fs_ztmm_6013_01>-zresult = lv_zresult."Result of the Processing
    <fs_ztmm_6013_01>-zmsg    = lv_message."Message text
*    <fs_ZTMM_6013_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ZTMM_6013_01.
  INSERT ztmm_6013_01 FROM TABLE it_ztmm_6013_01.

  PERFORM z_fca_eai_interface_log.
  IF sy-batch IS INITIAL.   "Not Backgroung Processing
    PERFORM dsp_log.    "Display Data Log
  ENDIF.



endform.
*&---------------------------------------------------------------------*
*&      Form  build_catalog_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_catalog_events.

  wa_events1-name = slis_ev_user_command.
  wa_events1-form = 'USER_COMMAND'.
  append wa_events1 to it_events.

  wa_fieldcat1-checkbox = 'X'.
  wa_fieldcat1-input = 'X'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting checkbox input
                                            where fieldname = 'ZMARK'.

  clear wa_fieldcat1.
  wa_fieldcat1-no_out = 'X'.
  wa_fieldcat1-key = space.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting no_out key
                      where fieldname = 'ZDOCNO'
                      OR    fieldname = 'LOGNO_H'
                      OR    fieldname = 'DOCTYPE'
                      OR    fieldname = 'M_W'
                      OR    fieldname = 'M_W1'
                      OR    fieldname = 'M_W2'
                      OR    fieldname = 'M_W3'
                      OR    fieldname = 'M_W4'
                      OR    fieldname = 'M_W5'
                      OR    fieldname = 'CM'
                      OR    fieldname = 'CM1'
                      OR    fieldname = 'CM2'
                      OR    fieldname = 'CM3'
                      OR    fieldname = 'CM4'
                      OR    fieldname = 'CM5'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'STAND_Month'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'STAND_YYYYMM'.

  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'STAND_Date'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'STAND_YYYYMMDD'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'Mat.Property'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'MATL'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'Coating'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'COAT_QTY'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'Thick'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'THICK'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'Width'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'WIDTH'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'Length'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'LENGTH'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = 'Inner/Outside'.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'IN_OUT'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = w_mon1.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'M'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = w_mon2.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'M1'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = w_mon3.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'M2'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = w_mon4.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'M3'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = w_mon5.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'M4'.
  clear wa_fieldcat1.
  wa_fieldcat1-seltext_m = w_mon6.
  wa_fieldcat1-ddictxt   = 'M'.
  modify it_fieldcat1 from wa_fieldcat1
                      transporting seltext_m ddictxt
                      where fieldname = 'M5'.
  clear wa_fieldcat1.
ENDFORM.                    " build_catalog
*&---------------------------------------------------------------------*
*&      Form  calculate_requiremnt_quantity
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_requiremnt_quantity.

  data: w_idx type i.
  field-symbols: <fs_mdsu1> like line of it_mdsu1.

  sort: it_marc by matnr werks,
        it_mdsu1 by matnr dat00.

  w_idx = 1.
  loop at it_marc assigning <fs_mat>.
    clear wa_mat_ltp.
    wa_mat_ltp-matnr = <fs_mat>-matnr.
    wa_mat_ltp-meins = <fs_mat>-meins.
    append wa_mat_ltp to it_mat_ltp.
    loop at it_mdsu1 assigning <fs_mdsu1> from w_idx.
      clear wa_mat_ltp.
      if <fs_mdsu1>-matnr ne <fs_mat>-matnr.
        w_idx = sy-tabix.
        clear w_menge.
        exit.
      endif.
      if <fs_mdsu1>-delkz eq 'WB'.                 "Stock.
        continue.
      endif.
      if <fs_mdsu1>-dat00 le w_1st_date.
        w_menge = w_menge + <fs_mdsu1>-mng03.
        wa_mat_ltp-mng01 = w_menge.
        modify it_mat_ltp from wa_mat_ltp transporting mng01
                          where matnr = <fs_mat>-matnr.
        continue.
      endif.
      case <fs_mdsu1>-dat00.
        when w_2nd_date.
          wa_mat_ltp-mng02 = <fs_mdsu1>-mng03.
          modify it_mat_ltp from wa_mat_ltp transporting mng02
                            where matnr = <fs_mat>-matnr.
        when w_3rd_date.
          wa_mat_ltp-mng03 = <fs_mdsu1>-mng03.
          modify it_mat_ltp from wa_mat_ltp transporting mng03
                            where matnr = <fs_mat>-matnr.
        when w_4th_date.
          wa_mat_ltp-mng04 = <fs_mdsu1>-mng03.
          modify it_mat_ltp from wa_mat_ltp transporting mng04
                            where matnr = <fs_mat>-matnr.
        when w_5th_date.
          wa_mat_ltp-mng05 = <fs_mdsu1>-mng03.
          modify it_mat_ltp from wa_mat_ltp transporting mng05
                            where matnr = <fs_mat>-matnr.
        when w_6th_date.
          wa_mat_ltp-mng06 = <fs_mdsu1>-mng03.
          modify it_mat_ltp from wa_mat_ltp transporting mng06
                            where matnr = <fs_mat>-matnr.
      endcase.
    endloop.
  endloop.
ENDFORM.                    " calculate_requiremnt_quantity
