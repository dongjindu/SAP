*----------------------------------------------------------------------*
*   INCLUDE ZIPP501L_ENGIN_PP_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.
  s_pdate-sign   = 'I'.
  s_pdate-option = 'EQ'.
  s_pdate-low    = sy-datum.
  APPEND s_pdate .
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM at_selection-screen.
*  LOOP AT SCREEN.
*    IF R1 EQ 'X'.
*      IF SCREEN-GROUP1 = 'ABC'.
*        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

*  REFRESH S_PDATE.
*  S_PDATE-LOW = S_PDATE-LOW + 1.
*  IF S_PDATE-HIGH EQ '00000000'.
*    S_PDATE-HIGH = S_PDATE-LOW + 31.
*  ENDIF.
*  S_PDATE-OPTION = 'BT'.
*  APPEND S_PDATE.
  CASE c_mark.
    WHEN r1 .    " Transfer
      IF s_pdate-low LT sy-datum .
        MESSAGE e001 WITH text-301.
      ENDIF.
    WHEN r2 .    " Retransfer
  ENDCASE.
  IF r01 = 'X'.
    s_pdate-sign   = 'I'.
    s_pdate-option = 'EQ'.
    s_pdate-low    = sy-datum.
    APPEND s_pdate .
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
*----> Condition of search date
  IF s_pdate-high IS INITIAL.
    s_date-low    = s_pdate-low.
    s_date-high   = s_pdate-low + 30.
    s_date-sign   = 'I'.
    s_date-option = 'BT'.
    APPEND s_date.
  ELSE.
    s_date-low    = s_pdate-low.
    s_date-high   = s_pdate-high.
    s_date-sign   = 'I'.
    s_date-option = 'BT'.
    APPEND s_date.
  ENDIF.

  CASE c_mark.
    WHEN r1.
*----> Transfer
      IF r01 = 'X'.
        PERFORM set_init_data.
        PERFORM get_eng_plant.
        it_ztppep2[] = it_ztppep2_eng[].
*        clear: IT_ZTPPEP2_ENG[].
        CALL SCREEN 9000.
        it_ztppep2_eng[] = it_ztppep2[].
      ELSE.
        PERFORM select_plaf.
        PERFORM get_eng_plant.
      ENDIF.
    WHEN r2.
*----> Re-transfer
      PERFORM select_ztppep.
      PERFORM get_eng_plant.
  ENDCASE.
*  PERFORM GET_ENG_PLANT.
ENDFORM.                    " READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
  PERFORM display_ztppep.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF
*&---------------------------------------------------------------------*
FORM select_plaf.

*----> Conditon of Production scheduler
  CLEAR : r_plgrp, r_plgrp[].
  r_plgrp-sign   = 'I'.
  r_plgrp-option = 'EQ'.
  r_plgrp-low    = c_sea .
  APPEND r_plgrp.

  r_plgrp-low    = c_sec .
  APPEND r_plgrp.

*----> SELECT PLAF
*  SELECT plnum       "Planned order number
*         psttr       "Order start date in planned order
*         matnr       "Planning material
*         gsmng       "Total planned order quantity
*         meins       "Base unit of measure
*         plgrp       "Production scheduler
*         INTO TABLE it_plaf
*         FROM plaf
*         WHERE pwwrk EQ c_e001  "Plant for planning purposes
*           AND beskz EQ 'E'      "Procurement Type
*           AND dispo EQ c_me1   "MRP controller
*           AND plscn EQ space   "Planning scenario in long-term
*planning
*           AND plgrp IN r_plgrp "ProdScheduler ( 'SEA' or 'SEC' )
*           AND psttr IN s_date  "Order start date in planned order
*           AND plnum IN s_plnum "Planned order number
*           AND matnr IN s_matnr. "Planning material

  SELECT  plnum       "Planned order number
          psttr       "Order start date in planned order
          a~matnr       "Planning material
          gsmng       "Total planned order quantity
          meins       "Base unit of measure
          plgrp       "Production scheduler
          INTO TABLE it_plaf
          FROM plaf AS a
          INNER JOIN marc AS b
          ON a~matnr = b~matnr
          AND a~pwwrk = b~werks
** for E002
*          WHERE pwwrk EQ c_e001  "Plant for planning purposes
          WHERE pwwrk EQ p_werks  "Plant for planning purposes
** End
            AND a~beskz EQ 'E'      "Procurement Type
            AND a~dispo EQ c_me1   "MRP controller
           AND plscn EQ space   "Planning scenario in long-term planning
            AND plgrp IN r_plgrp "ProdScheduler ( 'SEA' or 'SEC' )
            AND psttr IN s_date  "Order start date in planned order
            AND plnum IN s_plnum "Planned order number
            AND a~matnr IN s_matnr "Planning material
            AND b~mmsta = '12'.  " Plant sp matl stats

*----> Choose only new data and cut off old data tranfered
*----> On the line cancelled - 2004.02.19 - Mr. Moon
  LOOP AT it_plaf.
    MOVE-CORRESPONDING it_plaf TO *it_plaf.
    APPEND *it_plaf.
  ENDLOOP.

  SORT *it_plaf BY psttr matnr.

  PERFORM move_plaf_to_itab.

ENDFORM.                    " SELECT_PLAF
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTPPEP
*&---------------------------------------------------------------------*
FORM select_ztppep.
  DATA: l_tabix LIKE sy-tabix.

  SELECT * FROM ztppep2
           INTO TABLE it_ztppep2
           WHERE pdate IN s_pdate
             AND plnum IN s_plnum
             AND pitem IN s_matnr .

  SELECT * FROM ztppep
          APPENDING TABLE it_ztppep2
          WHERE pdate IN s_pdate
            AND plnum IN s_plnum
            AND pitem IN s_matnr .

  LOOP AT it_ztppep2.
    l_tabix = sy-tabix.
    CASE c_mark.
      WHEN r_1.
        it_ztppep2-eflag = 'IR'.
      WHEN r_2.
        it_ztppep2-eflag = 'RP'.
      WHEN r_3.
        it_ztppep2-eflag = 'DL'.
    ENDCASE.
    MODIFY it_ztppep2 INDEX l_tabix.
  ENDLOOP.

ENDFORM.                    " SELECT_ZTPPEP
*&---------------------------------------------------------------------*
*&      Form  MOVE_PLAF_TO_ITAB
*&---------------------------------------------------------------------*
FORM move_plaf_to_itab.
  LOOP AT *it_plaf.
    it_ztppep2-plnum = *it_plaf-plnum.
    it_ztppep2-pdate = *it_plaf-psttr.
    it_ztppep2-pitem = *it_plaf-matnr.
    it_ztppep2-gsmng = *it_plaf-gsmng.
    it_ztppep2-meins = *it_plaf-meins.
    IF *it_plaf-plgrp EQ c_sea.
      it_ztppep2-plgrp = c_sea+2(1).
    ELSEIF *it_plaf-plgrp EQ c_sec.
      it_ztppep2-plgrp = c_sec+2(1).
    ENDIF.
    IF r_1 EQ 'X'.
      it_ztppep2-eflag = 'IR'.
    ELSEIF r_2 EQ 'X'.
      it_ztppep2-eflag = 'RP'.
    ELSEIF r_3 EQ 'X'.
      it_ztppep2-eflag = 'DL'.
    ENDIF.

    APPEND it_ztppep2.
  ENDLOOP.

ENDFORM.                    " MOVE_PLAF_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ZTPPEP
*&---------------------------------------------------------------------*
FORM display_ztppep.
  LOOP AT it_ztppep2_eng.
    MOVE-CORRESPONDING it_ztppep2_eng TO it_list.
*----> Display Description
    SELECT SINGLE maktx
                INTO it_list-maktx
                FROM makt
                WHERE matnr EQ it_list-pitem
                  AND spras EQ sy-langu .
    APPEND it_list.
  ENDLOOP.
ENDFORM.                    " DISPLAY_ZTPPEP
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.
  PERFORM build_fieldcat.
  PERFORM build_event.
  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM call_function.
ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
FORM call_function.
  DATA:   l_print_p TYPE slis_print_alv.  " print setting

  CLEAR  w_program.
  w_program = sy-repid.

*** print paramter   ****************************************
  l_print_p-no_coverpage = 'X'.
  l_print_p-no_print_listinfos = 'X'.
  l_print_p-no_change_print_params = 'X'.
  l_print_p-no_print_selinfos = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_bypassing_buffer       = 'X'
      i_callback_program       = w_program
      i_callback_pf_status_set = 'ALV_PF_STATUS_SET'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_callback_user_command  = 'USER_COMMAND'
      it_fieldcat              = w_fieldcat[]
      it_sort                  = w_sortcat[]
      i_save                   = 'A'
      it_events                = w_eventcat[]
      is_print                 = l_print_p
    TABLES
      t_outtab                 = it_list
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  create_interface_log
*&---------------------------------------------------------------------*
FORM create_interface_log.
*  DESCRIBE TABLE IT_ZTPPEP LINES Z_TOTAL.
*  CHECK Z_TOTAL <> 0.
*  I_ZTCA_IF_LOG-TCODE    = 'ZPPI501'.
**  I_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
**  I_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
*  I_ZTCA_IF_LOG-TOTAL    = Z_TOTAL.
**  I_ZTCA_IF_LOG-ZSUCC    = Z_SUCC.
**  I_ZTCA_IF_LOG-ERROR    = Z_TOTAL - Z_SUCC.
*  I_ZTCA_IF_LOG-ERDAT    = SY-DATUM. "Created on.
*  I_ZTCA_IF_LOG-ERZET    = SY-UZEIT. "Created time.
*  I_ZTCA_IF_LOG-ERNAM    = SY-UNAME. "Created by.
*
*  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
*    EXPORTING
*      I_ZTCA_IF_LOG              = I_ZTCA_IF_LOG
**   IMPORTING
**     E_ZTCA_IF_LOG              =
*   EXCEPTIONS
*     UPDATE_FAILED              = 1
*     NUMBER_RANGE_ERROR         = 2
*     TCODE_DOES_NOT_EXIST       = 3
*     OTHERS                     = 4
*            .
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " create_interface_log
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat.
*  APPEND_FIELDCAT :


*   postion    field    ref field     key q-ref   c-ref
*   Text                len
  PERFORM append_fieldcat USING :
*    W_COL_POS 'FLAG'    'FLAG'        'X'  ''      ''
*    'LOG'                             '3'  '',
    w_col_pos 'PLNUM'   'PLNUM'       'X'  ''      ''
   'PldOrd #'                         '10' '',
   w_col_pos  'PDATE'   'PDATE'       'X'  ''      ''
   'Plan Date'                        '10'  '',
*    W_COL_POS 'PLNUM'   'PLNUM'       'X'  ''      ''
*    'P/O No.'                         '10' '',

** Furong on 01/25/12
    w_col_pos 'PLANT_CD' 'PLANT_CD'   ''   ''      ''
    'Plant'                           '6' '',
** end on 01/25/12

    w_col_pos 'PITEM'   'PITEM'       ''   ''      ''
    'Material #'                      '18' '',
    w_col_pos 'MAKTX'   'MAKTX'       ''   ''      ''
    'Material Description'            '20' '',
    w_col_pos 'GSMNG'   'GSMNG'       ' '  ''      ''
    'Qty'                             '17' 'EA',
    w_col_pos 'MEINS'   'MEINS'       ' '  ''      ''
    'UNIT'                            '4'  '',
    w_col_pos 'PLGRP'   'PLGRP'       ' '  ''      ''
    'TYPE'                            '4'  ''.
*    W_COL_POS 'ZUSER'   'ZUSER'       ' '  ''      ''
*    'Creator'                         '10' '',
*    W_COL_POS 'ZEDAT'   'ZEDAT'       ' '  ''      ''
*    'I/F Date'                        '10' '',
*    W_COL_POS 'ZETIM'   'ZETIM'       ' '   ''      ''
*    'I/F Time'                        '10' '',
*    W_COL_POS 'ZMSG'    'ZMSG'        ' '   ''      ''
*    'Message'                        '50' ''.
ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
FORM build_event.
  w_eventcat-name = 'TOP_OF_PAGE'.
  w_eventcat-form = 'TOP_OF_PAGE'.

  APPEND w_eventcat.

ENDFORM.                    " BUILD_EVENT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
FORM build_sort.
*  W_SORTCAT-SPOS           = 1.
*  W_SORTCAT-FIELDNAME      = 'FLAG'.
*  W_SORTCAT-TABNAME        = 'IT_LIST'.
*  W_SORTCAT-UP             = 'X'.
*  APPEND W_SORTCAT.

  w_sortcat-spos           = 1.
  w_sortcat-fieldname      = 'PDATE'.
  w_sortcat-tabname        = 'IT_LIST'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

  w_sortcat-spos           = 2.
  w_sortcat-fieldname      = 'PLNUM'.
  w_sortcat-tabname        = 'IT_LIST'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.
ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_date(50),
        l_list(50),
        l_date1(10),
        l_date2(10).

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-a01.
  APPEND ls_line TO lt_top_of_page.

**----- User
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = TEXT-A02.
*  LS_LINE-INFO = SY-UNAME.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Date
  IF NOT s_pdate-low IS INITIAL.
    ls_line-typ  = 'S'.
    ls_line-key  = text-a03.   "Date :
    IF NOT s_pdate-high IS INITIAL .
      PERFORM user_date USING    s_pdate-low
                        CHANGING l_date1.
      PERFORM user_date USING    s_pdate-high
                        CHANGING l_date2.
      CONCATENATE text-a04  l_date1 ',' text-a05 l_date2 INTO l_list
                  SEPARATED BY space .
    ELSE.
      PERFORM user_date USING    s_pdate-low
                        CHANGING l_date1.
      l_list = l_date1.
    ENDIF.
    ls_line-info = l_list.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*----- Total Count of Planning data
  DATA : l_lines     TYPE   sy-tabix  ,
         l_text(13)  TYPE   c         .
  DESCRIBE TABLE it_list  LINES l_lines .
  WRITE l_lines    TO    l_text  LEFT-JUSTIFIED .
  ls_line-typ  = 'S'.
  ls_line-key  = text-a02.
  ls_line-info = l_text.
  APPEND ls_line TO lt_top_of_page.

  ls_line-typ  = 'S' .
  ls_line-key  = '  '.
  ls_line-info = '  '.
  APPEND ls_line TO lt_top_of_page.

*----- Total
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Total: '.
*  LS_LINE-INFO = Z_TOTAL.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Success
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Success: '.
*  LS_LINE-INFO = Z_SUCC.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    " COMMENT_BUILD
*&---------------------------------------------------------------------
*         Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------
FORM alv_pf_status_set USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD'. " EXCLUDING RT_EXTAB.

ENDFORM.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     i_logo             = 'Z_HYUNDAI_LOGO'
*     i_logo             = 'ENJOYSAP_LOGO'
      it_list_commentary = w_top_of_page.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  EVENT_BUILD
*&---------------------------------------------------------------------*
FORM event_build USING p_w_eventcat TYPE slis_t_event.
  DATA : l_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = p_w_eventcat.

  READ TABLE p_w_eventcat WITH KEY name = slis_ev_top_of_page
                          INTO l_event.

  IF sy-subrc EQ 0.
    MOVE c_formname_top_of_page TO l_event-form.
    APPEND l_event TO p_w_eventcat.
  ENDIF.
ENDFORM.                    " EVENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZTPPEP
*&---------------------------------------------------------------------*
FORM modify_ztppep.
  CLEAR : *it_ztppep2, *it_ztppep2[].
  CLEAR : *it_ztppep, *it_ztppep[].
  LOOP AT it_eng1.
    MOVE-CORRESPONDING it_eng1 TO *it_ztppep.
    APPEND *it_ztppep.
*    IF *IT_ZTPPEP-FLAG EQ 'S'.
*      PERFORM DELTE_PRDORD_LOGIC.
*    ENDIF.
  ENDLOOP.
  MODIFY ztppep FROM TABLE *it_ztppep.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  LOOP AT it_eng2.
    MOVE-CORRESPONDING it_eng2 TO *it_ztppep2.
    APPEND *it_ztppep2.
*    IF *IT_ZTPPEP2-FLAG EQ 'S'.
*      PERFORM DELTE_PRDORD_LOGIC.
*    ENDIF.
  ENDLOOP.
*  MODIFY ZTPPEP2 FROM TABLE *IT_ZTPPEP2.
  INSERT ztppep2 FROM TABLE *it_ztppep2.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " MODIFY_ZTPPEP
*&-------------------------------------------------------------------
*&      Form  USER_COMMAND
*&-------------------------------------------------------------------
FORM user_command USING ucomm    LIKE sy-ucomm
                         selfield TYPE slis_selfield.
  DATA : sel_field LIKE selfield-sel_tab_field.

  CASE ucomm.
    WHEN '&REL'.
      DESCRIBE TABLE it_ztppep2 LINES z_total.
      PERFORM transfer_pp_to_mes.
      PERFORM modify_ztppep.
      PERFORM create_interface_log.
      PERFORM call_screen_result.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                               " USER_COMMAND1
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
FORM transfer_pp_to_mes.
  DATA: l_msgtxt(100),
        l_tabix LIKE sy-tabix.

  REFRESH: it_eng1, it_eng2.
** Changed by Furong on 11/21/08
*  LOOP AT IT_ZTPPEP2_ENG.
*    IF IT_ZTPPEP2_ENG-PLANT_CD = 'ENG1'.
*      MOVE-CORRESPONDING IT_ZTPPEP2_ENG TO IT_ENG1.
*      APPEND IT_ENG1.
*    ELSE.
*      MOVE-CORRESPONDING IT_ZTPPEP2_ENG TO IT_ENG2.
*      APPEND IT_ENG2.
*    ENDIF.
*  ENDLOOP.
  LOOP AT it_ztppep2_eng.
    MOVE-CORRESPONDING it_ztppep2_eng TO it_eng2.
    APPEND it_eng2.
  ENDLOOP.
** End of change on 11/21/08

  IF NOT it_eng1[] IS INITIAL.
** Send ENG1 to interface
    CALL FUNCTION 'Z_FPP_ENGINE_PP'
      DESTINATION c_dest
      TABLES
        t_ztppep              = it_eng1
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.

    CLEAR z_succ .
    IF sy-subrc NE 0 .
      MESSAGE i001 WITH text-302 .
      CASE c_mark.
        WHEN r1 .    " Transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP WHERE PDATE IN S_DATE.
** End of cahnge
          LOOP AT it_eng1 .
            l_tabix = sy-tabix.
*          IT_ZTPPEP-FLAG = 'E'.
            it_eng1-zresult = 'E'      .
            it_eng1-zmsg    = l_msgtxt .
            it_eng1-zuser   = sy-uname .
            it_eng1-zsdat   = sy-datum .
            it_eng1-zstim   = sy-uzeit .
            MODIFY it_eng1 INDEX l_tabix .
          ENDLOOP.
        WHEN r2 .    " Re-transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP WHERE PDATE IN S_PDATE .
** End of change
          LOOP AT it_eng1 .
            l_tabix = sy-tabix .
            it_eng1-zresult = 'E'      .
            it_eng1-zmsg    = l_msgtxt .
            MODIFY it_eng1 INDEX l_tabix .
          ENDLOOP.
      ENDCASE .

    ELSE.
      CASE c_mark.
        WHEN r1.    " Transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP WHERE PDATE IN S_DATE .
** End of change
          LOOP AT it_eng1.
            l_tabix = sy-tabix.
            IF it_eng1-zzret  = 'E'.
              it_eng1-zresult = 'E'.
              it_eng1-zuser   = sy-uname.
              it_eng1-zsdat   = sy-datum.
              it_eng1-zstim   = sy-uzeit.
              it_eng1-zmode   = 'C'.
              MODIFY it_eng1 INDEX l_tabix.
            ELSE.
              z_succ = z_succ + 1.
              it_eng1-zresult = 'S' .  "IT_ZTPPEP-ZZRET.
              it_eng1-zuser   = sy-uname.
              it_eng1-zsdat   = sy-datum.
              it_eng1-zstim   = sy-uzeit.
              it_eng1-zmode   = 'C'.
              MODIFY it_eng1 INDEX l_tabix.
            ENDIF.
          ENDLOOP.
        WHEN r2 .    " Re-transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP WHERE PDATE IN S_PDATE .
** End of change
          LOOP AT it_eng1.
            l_tabix  = sy-tabix.
            CLEAR it_eng1-zmsg .
            IF it_eng1-zzret  = 'E'.
              it_eng1-zresult = 'E'. "IT_ENG1-ZZRET.
              MODIFY it_eng1 INDEX l_tabix.
            ELSE.
              z_succ = z_succ + 1.
              it_eng1-zresult = 'S' .  "IT_ENG1-ZZRET.
              MODIFY it_eng1 INDEX l_tabix.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.
  ENDIF.

** Send ENG2 to interface
  IF NOT it_eng2[] IS INITIAL.
    CALL FUNCTION 'Z_FPP_ENGINE_PP_2'
      DESTINATION c_dest
      TABLES
        t_ztppep              = it_eng2
      EXCEPTIONS
        communication_failure = 1  MESSAGE l_msgtxt
        system_failure        = 2  MESSAGE l_msgtxt.

    CLEAR z_succ .
    IF sy-subrc NE 0 .
      MESSAGE i001 WITH text-302 .
      CASE c_mark.
        WHEN r1 .    " Transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP2 WHERE PDATE IN S_DATE .
** End of change
          LOOP AT it_eng2 .
            l_tabix = sy-tabix.
*          IT_ZTPPEP-FLAG = 'E'.
            it_eng2-zresult = 'E'      .
            it_eng2-zmsg    = l_msgtxt .
            it_eng2-zuser   = sy-uname .
            it_eng2-zsdat   = sy-datum .
            it_eng2-zstim   = sy-uzeit .
            MODIFY it_eng2 INDEX l_tabix .
          ENDLOOP.
        WHEN r2 .    " Re-transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP2 WHERE PDATE IN S_PDATE .
** End of change
          LOOP AT it_eng2.
            l_tabix = sy-tabix .
            it_eng2-zresult = 'E'      .
            it_eng2-zmsg    = l_msgtxt .
            MODIFY it_eng2 INDEX l_tabix .
          ENDLOOP.
      ENDCASE .

    ELSE.
      CASE c_mark.
        WHEN r1.    " Transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP2 WHERE PDATE IN S_DATE .
** End of change
          LOOP AT it_eng2.
            l_tabix = sy-tabix.
            IF it_eng2-zzret  = 'E'.
              it_eng2-zresult = 'E'.
              it_eng2-zuser   = sy-uname.
              it_eng2-zsdat   = sy-datum.
              it_eng2-zstim   = sy-uzeit.
              it_eng2-zmode   = 'C'.
              MODIFY it_eng2 INDEX l_tabix.
            ELSE.
              z_succ = z_succ + 1.
              it_eng2-zresult = 'S' .  "IT_ZTPPEP-ZZRET.
              it_eng2-zuser   = sy-uname.
              it_eng2-zsdat   = sy-datum.
              it_eng2-zstim   = sy-uzeit.
              it_eng2-zmode   = 'C'.
              MODIFY it_eng2 INDEX l_tabix.
            ENDIF.
          ENDLOOP.
        WHEN r2 .    " Re-transfer
** Changed by Furong on 01/14/10
*          DELETE FROM ZTPPEP2 WHERE PDATE IN S_PDATE .
** End of change
          LOOP AT it_eng2.
            l_tabix  = sy-tabix.
            CLEAR it_eng2-zmsg .
            IF it_eng2-zzret  = 'E'.
              it_eng2-zresult = 'E'. "IT_ENG2-ZZRET.
              MODIFY it_eng2 INDEX l_tabix.
            ELSE.
              z_succ = z_succ + 1.
              it_eng2-zresult = 'S' .  "IT_ENG1-ZZRET.
              MODIFY it_eng2 INDEX l_tabix.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.
  ENDIF.
  REFRESH it_ztppep2.

  LOOP AT it_eng1.
    MOVE-CORRESPONDING it_eng1 TO it_ztppep2.
    APPEND it_ztppep2.
  ENDLOOP.

*  APPEND LINES OF IT_ENG1 TO IT_ZTPPEP2.
  APPEND LINES OF it_eng2 TO it_ztppep2.


ENDFORM.                    " TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
*&      Form  USER_DATE
*&---------------------------------------------------------------------*
FORM user_date USING    p_date
               CHANGING p_l_date1.

  DATA: l_original_date TYPE d.

  l_original_date = p_date.

  CALL 'DATE_CONV_INT_TO_EXT'
  ID 'DATINT' FIELD l_original_date
  ID 'DATEXT' FIELD p_l_date1.

ENDFORM.                    " USER_DATE
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
FORM call_screen_result.
  DATA: l_fail TYPE i.

*  SELECT COUNT(*) FROM ZTPPEP
*         INTO Z_SUCC
*         WHERE FLAG EQ 'S'
*           AND PDATE IN S_DATE .
*           AND ZUSER EQ SY-UNAME .
*           AND ZSDAT EQ SY-DATUM.

  z_fail = z_total - z_succ.

  CALL SCREEN 50 STARTING AT 20 10.

ENDFORM.                    " CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0050 OUTPUT.
  SET PF-STATUS '50'.
  SET TITLEBAR '50'.

ENDMODULE.                 " STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0050 INPUT.
  ok_code = okcode.
  CLEAR okcode.

  CASE ok_code.
    WHEN 'ENTE' OR 'CANC'.
      LEAVE TO SCREEN 0 .
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELTE_PRDORD_LOGIC
*&---------------------------------------------------------------------*
FORM delte_prdord_logic.
  PERFORM bdc_dynpro_processing USING :
                 'X'  'SAPMM61P'        '0101',
                 ' '  'BDC_OKCODE'      '/00'   ,
                 ' '  'RM61P-PLNUM'     *it_ztppep-plnum,

                 'X'  'SAPLM61O'       '0110',
                 ' '  'BDC_OKCODE'     '=DLPL'.

  CALL TRANSACTION 'MD12' USING it_bdcdata MODE p_mode
                          MESSAGES INTO it_msg.

ENDFORM.                    " DELTE_PRDORD_LOGIC
*&--------------------------------------------------------------------
*&      Form  BDC_OPEN_GROUP
*&--------------------------------------------------------------------
FORM bdc_open_group.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = wa_bdcgroup
      keep                = 'X'
      user                = sy-uname
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.

  IF sy-subrc <> 0.
    WRITE: /, ' Error BDC Opening Group: ', sy-uzeit,
           /, ' Return Code: ', sy-subrc.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_OPEN_GROUP

*&--------------------------------------------------------------------
*&      Form  BDC_DYNPRO_PROCESSING
*&--------------------------------------------------------------------
FORM bdc_dynpro_processing USING dy_begin
                                 pg_name
                                 sc_no.
  IF dy_begin = 'X'.
    CLEAR it_bdcdata.
    MOVE  pg_name  TO it_bdcdata-program.
    MOVE  sc_no    TO it_bdcdata-dynpro.
    MOVE  'X'      TO it_bdcdata-dynbegin.
    APPEND it_bdcdata.
  ELSE.
    CLEAR it_bdcdata.
    MOVE  pg_name  TO it_bdcdata-fnam.
    MOVE  sc_no    TO it_bdcdata-fval.
    APPEND it_bdcdata.
  ENDIF.
ENDFORM.                    " BDC_DYNPRO_PROCESSING.

*&--------------------------------------------------------------------
*&      Form  BDC_INSERT_TRANSACTION
*&--------------------------------------------------------------------
FORM bdc_insert_transaction.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode            = p_tcode
    TABLES
      dynprotab        = it_bdcdata
    EXCEPTIONS
      internal_error   = 1
      not_open         = 2
      queue_error      = 3
      tcode_invalid    = 4
      printing_invalid = 5
      posting_invalid  = 6
      OTHERS           = 7.

  IF sy-subrc <> 0.
    WRITE: /, ' Error BDC Insert: ', sy-uzeit,
           /, ' Return Code: ', sy-subrc.
    LEAVE.
  ENDIF.
ENDFORM.                    " BDC_INSERT_TRANSACTION

*&--------------------------------------------------------------------
*&      Form  BDC_CLOSE_GROUP
*&--------------------------------------------------------------------
FORM bdc_close_group.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.

  IF sy-subrc <> 0.
    WRITE: /, ' Error BDC Close: ', sy-uzeit,
           /, ' Return Code: ', sy-subrc.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_fieldcat USING
   p_position  p_field   p_ref_field    p_key  p_qref  p_cref
   p_text     p_len   p_meins.
  p_position = p_position + 1.
  w_fieldcat-col_pos       = p_position.
  w_fieldcat-fieldname     = p_field.
  w_fieldcat-ref_fieldname = p_ref_field.
  w_fieldcat-key           = p_key.
  w_fieldcat-qfieldname    = p_qref.
  w_fieldcat-cfieldname    = p_cref.
  w_fieldcat-quantity      = p_meins.
  w_fieldcat-seltext_l     = p_text.
  w_fieldcat-seltext_m     = p_text.
  w_fieldcat-seltext_s     = p_text.
  w_fieldcat-outputlen     = p_len.
*    W_FIELDCAT-NO_OUT        = .
  APPEND w_fieldcat.
  CLEAR : w_fieldcat.
ENDFORM.                    " APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  set_init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_init_data.
  DATA: it_mara LIKE TABLE OF mara WITH HEADER LINE.
  DATA: l_plgrp(3),
        l_year(2),
        l_dispo LIKE marc-dispo.

  SELECT * INTO TABLE it_mara
    FROM mara
   WHERE matnr IN s_matnr.
  LOOP AT it_mara.
    it_ztppep2-pdate = s_pdate-low.
    it_ztppep2-pitem = it_mara-matnr.
    it_ztppep2-meins = it_mara-meins.

    SELECT SINGLE fevor dispo INTO (l_plgrp, l_dispo)
      FROM marc
      WHERE matnr = it_mara-matnr
** for E002
*        AND WERKS = 'E001'
        AND werks = p_werks
** End
        AND mmsta = '12'.
    IF sy-subrc = 0 AND l_dispo = 'ME1'.
      IF l_plgrp EQ c_sea.
        it_ztppep2-plgrp = c_sea+2(1).
      ELSEIF l_plgrp EQ c_sec.
        it_ztppep2-plgrp = c_sec+2(1).
      ENDIF.
      l_year = s_pdate-low+2(2).
      CONCATENATE l_year s_pdate-low+4(4) sy-uzeit+0(4) INTO
               it_ztppep2-plnum.
      IF r_1 EQ 'X'.
        it_ztppep2-eflag = 'IR'.
      ELSEIF r_2 EQ 'X'.
        it_ztppep2-eflag = 'RP'.
      ELSEIF r_3 EQ 'X'.
        it_ztppep2-eflag = 'DL'.
      ENDIF.
      APPEND it_ztppep2.
    ENDIF.
    CLEAR: it_mara, it_ztppep2.
  ENDLOOP.

ENDFORM.                    " set_init_data
*&---------------------------------------------------------------------*
*&      Module  TB_9000_change_tc_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tb_9000_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_ztppep2 LINES tb_9000-lines.
ENDMODULE.                 " TB_9000_change_tc_attr  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'PF9000'.
  SET TITLEBAR '9000'.
ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TB_9000_modify  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tb_9000_modify INPUT.
  MODIFY it_ztppep2
    INDEX tb_9000-current_line.
ENDMODULE.                 " TB_9000_modify  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_comm_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_comm_9000 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
      DELETE it_ztppep2 WHERE pdate = s_pdate-low.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM save_data.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " user_comm_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  save_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_data.
  LOOP AT it_ztppep2.
    IF it_ztppep2-gsmng <= 0.
      DELETE it_ztppep2.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " save_data

*&---------------------------------------------------------------------*
*&      Form  get_eng_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_eng_plant.
  DATA : it_vm   LIKE TABLE OF zspp_vin_value WITH HEADER LINE.

  CLEAR: it_ztppep2_eng, it_ztppep2_eng[].

  LOOP AT it_ztppep2.
    IF it_ztppep2-plgrp = 'A'.
      it_vm-atnam = 'EN_SPC15'.
    ELSE.
      it_vm-atnam = 'EN_3CSPC01'.
    ENDIF.
    APPEND it_vm.
    MOVE-CORRESPONDING it_ztppep2 TO it_ztppep2_eng.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = it_ztppep2-pitem
        mode         = 'R'
        ctype        = '001'
*       DISPLAY      = 'D'
      TABLES
        val_table    = it_vm
      EXCEPTIONS
        no_data      = 1
        error_mode   = 2
        error_object = 3
        OTHERS       = 4.
    IF sy-subrc <> 0.
    ELSE .
*      READ TABLE IT_VM WITH KEY ATNAM = 'EN_SPC15'.
*      IT_ZTPPEP2_ENG-EN_SPC15  = IT_VM-ATWRT.
      READ TABLE it_vm INDEX 1.
** Furong on 01/24/12
*      it_ztppep2_eng-plant_cd = it_vm-atwrt.
      it_ztppep2_eng-plant_cd = p_werks.
** End on 01/24/12
      it_ztppep2_eng-en_spc15 = it_vm-atwrt.
    ENDIF.
    APPEND it_ztppep2_eng.
*    READ TABLE IT_VM INDEX 1.
*    CLEAR: IT_VM-ATWRT.
*    modify IT_VM INDEX 1.
    REFRESH it_vm.
    CLEAR: it_vm.
  ENDLOOP.

ENDFORM.                    " get_eng_plant
