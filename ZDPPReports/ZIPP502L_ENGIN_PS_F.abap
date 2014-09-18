*----------------------------------------------------------------------*
*   INCLUDE ZIPP502L_ENGIN_PS_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM read_process.
  CASE c_mark.
    WHEN r1.   "Transfer
*      PERFORM READ_CLASSIFCATION.
      PERFORM select_marc .
      PERFORM read_classification .
    WHEN r2.   "Re-transfer
      PERFORM select_ztppes.
  ENDCASE.

ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFCATION
*&---------------------------------------------------------------------*
FORM read_classifcation.
*  DATA: L_MSGTXT(100),
*        L_TABIX LIKE SY-TABIX,
*        L_OBJECTKEY LIKE BAPI1003_KEY-OBJECT.
*
*  CLEAR : IT_MARA , IT_MARA[] .
*  SELECT SINGLE MATNR
*         INTO TABLE IT_MARA
*         FROM MARA
*         WHERE MATNR EQ P_MATNR.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE E001 WITH TEXT-301.
*  ELSE.
** Reading Classification
*    L_OBJECTKEY = P_MATNR.
*    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*         EXPORTING
*              OBJECTKEY       = L_OBJECTKEY
*              OBJECTTABLE     = 'MARA'
*              CLASSNUM        = 'ENG_SPEC_MASTER'
*              CLASSTYPE       = '001'
*         TABLES
*              ALLOCVALUESNUM  = IT_VMASTER1
*              ALLOCVALUESCHAR = IT_VMASTER
*              ALLOCVALUESCURR = IT_VMASTER2
*              RETURN          = RETURN.
*
*    LOOP AT IT_VMASTER.
*      CASE IT_VMASTER-CHARACT.
*        WHEN 'EN_VEH_MODEL'.
*          IT_ZTPPES-EN_VEH_MODEL = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_HEAD'.
*          IT_ZTPPES-EN_HEAD = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC01'.
*          IT_ZTPPES-EN_SPC01 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC02'.
*          IT_ZTPPES-EN_SPC02 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC03'.
*          IT_ZTPPES-EN_SPC03 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC04'.
*          IT_ZTPPES-EN_SPC04 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC05'.
*          IT_ZTPPES-EN_SPC05 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC06'.
*          IT_ZTPPES-EN_SPC06 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC07'.
*          IT_ZTPPES-EN_SPC07 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC08'.
*          IT_ZTPPES-EN_SPC08 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC09'.
*          IT_ZTPPES-EN_SPC09 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC10'.
*          IT_ZTPPES-EN_SPC10 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC11'.
*          IT_ZTPPES-EN_SPC11 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC12'.
*          IT_ZTPPES-EN_SPC12 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC13'.
*          IT_ZTPPES-EN_SPC13 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC14'.
*          IT_ZTPPES-EN_SPC14 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC15'.
*          IT_ZTPPES-EN_SPC15 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC16'.
*          IT_ZTPPES-EN_SPC16 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC17'.
*          IT_ZTPPES-EN_SPC17 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC18'.
*          IT_ZTPPES-EN_SPC18 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC19'.
*          IT_ZTPPES-EN_SPC19 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC20'.
*          IT_ZTPPES-EN_SPC20 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC21'.
*          IT_ZTPPES-EN_SPC21 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC22'.
*          IT_ZTPPES-EN_SPC22 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC23'.
*          IT_ZTPPES-EN_SPC23 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC24'.
*          IT_ZTPPES-EN_SPC24 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC25'.
*          IT_ZTPPES-EN_SPC25 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC26'.
*          IT_ZTPPES-EN_SPC36 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC27'.
*          IT_ZTPPES-EN_SPC37 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC28'.
*          IT_ZTPPES-EN_SPC38 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC29'.
*          IT_ZTPPES-EN_SPC39 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC30'.
*          IT_ZTPPES-EN_SPC30 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC31'.
*          IT_ZTPPES-EN_SPC31 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC32'.
*          IT_ZTPPES-EN_SPC32 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC33'.
*          IT_ZTPPES-EN_SPC33 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC34'.
*          IT_ZTPPES-EN_SPC34 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC35'.
*          IT_ZTPPES-EN_SPC35 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC36'.
*          IT_ZTPPES-EN_SPC36 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC37'.
*          IT_ZTPPES-EN_SPC37 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC38'.
*          IT_ZTPPES-EN_SPC38 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC39'.
*          IT_ZTPPES-EN_SPC39 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC40'.
*          IT_ZTPPES-EN_SPC40 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC41'.
*          IT_ZTPPES-EN_SPC41 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC42'.
*          IT_ZTPPES-EN_SPC42 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC43'.
*          IT_ZTPPES-EN_SPC43 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC44'.
*          IT_ZTPPES-EN_SPC44 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC45'.
*          IT_ZTPPES-EN_SPC45 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC46'.
*          IT_ZTPPES-EN_SPC46 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC47'.
*          IT_ZTPPES-EN_SPC47 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC48'.
*          IT_ZTPPES-EN_SPC48 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC49'.
*          IT_ZTPPES-EN_SPC49 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC50'.
*          IT_ZTPPES-EN_SPC50 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC51'.
*          IT_ZTPPES-EN_SPC51 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC52'.
*          IT_ZTPPES-EN_SPC52 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC53'.
*          IT_ZTPPES-EN_SPC53 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC54'.
*          IT_ZTPPES-EN_SPC54 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC55'.
*          IT_ZTPPES-EN_SPC55 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC56'.
*          IT_ZTPPES-EN_SPC56 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC57'.
*          IT_ZTPPES-EN_SPC57 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC58'.
*          IT_ZTPPES-EN_SPC58 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC59'.
*          IT_ZTPPES-EN_SPC59 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC60'.
*          IT_ZTPPES-EN_SPC60 = IT_VMASTER-VALUE_NEUTRAL.
*      ENDCASE.
*    ENDLOOP.
*
*    CASE C_MARK.
*      WHEN R_1.
*        IT_ZTPPES-EFLAG = 'IR'.
*      WHEN R_2.
*        IT_ZTPPES-EFLAG = 'RP'.
*      WHEN R_3.
*        IT_ZTPPES-EFLAG = 'DL'.
*    ENDCASE.
*
*    IT_ZTPPES-EN_ITEM  = P_MATNR.
**    IT_ZTPPES-ZUSER    = SY-UNAME.
**    IT_ZTPPES-ZSDAT    = SY-DATUM.
**    IT_ZTPPES-ZSTIM    = SY-UZEIT.
**    IT_ZTPPES-ZEDAT    = SY-DATUM.
**    IT_ZTPPES-ZETIM    = SY-UZEIT.
**    IT_ZTPPES-ZMODE    = 'C'.
*    APPEND IT_ZTPPES.
*  ENDIF.

ENDFORM.                    " READ_CLASSIFCATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTPPES
*&---------------------------------------------------------------------*
FORM select_ztppes.
  DATA: l_tabix LIKE sy-tabix.

  SELECT * FROM ztppes
           INTO TABLE it_ztppes
*           WHERE FLAG EQ 'E'
           WHERE en_item  IN s_matnr .

  LOOP AT it_ztppes.
    l_tabix = sy-tabix.
    CASE c_mark.
      WHEN r_1.
        it_ztppes-eflag = 'IR'.
      WHEN r_2.
        it_ztppes-eflag = 'RP'.
      WHEN r_3.
        it_ztppes-eflag = 'DL'.
    ENDCASE.

    MODIFY it_ztppes INDEX l_tabix.
  ENDLOOP.
ENDFORM.                    " SELECT_ZTPPES
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM data_process.
  PERFORM display_ztppes.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZTPPES
*&---------------------------------------------------------------------*
FORM modify_ztppes.
  LOOP AT it_ztppes.
    MOVE-CORRESPONDING it_ztppes TO *it_ztppes.
    APPEND *it_ztppes.
  ENDLOOP.
  MODIFY ztppes FROM TABLE *it_ztppes.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " MODIFY_ZTPPES
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ZTPPES
*&---------------------------------------------------------------------*
FORM display_ztppes.
  DATA l_tabix  TYPE  sy-tabix .
  LOOP AT it_ztppes.
    l_tabix = sy-tabix.
    CLEAR : it_ztppes-zmsg .
    MOVE-CORRESPONDING it_ztppes TO it_list.
    SELECT SINGLE maktx
               INTO it_list-maktx
               FROM makt
               WHERE matnr EQ it_list-en_item
                 AND spras EQ sy-langu .
    APPEND it_list.
    MODIFY it_ztppes INDEX l_tabix.
  ENDLOOP.

ENDFORM.                    " DISPLAY_ZTPPES
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM list_process.
  DESCRIBE TABLE it_ztppes LINES z_total.
  PERFORM build_fieldcat.
  PERFORM build_event.
  PERFORM build_sort.
  PERFORM comment_build USING  w_top_of_page[].
  PERFORM call_function.

ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM build_fieldcat.

*   COL_POS, FIELDNAME, REF_FIELDNAME, KEY,
*   SELTEXT_L,                        OUTPUTLEN, NO_OUT
  append_fieldcat :

    w_col_pos 'EN_ITEM'   'EN_ITEM'   'X'
    'Material #'                     '18' '',
    w_col_pos 'MAKTX'     'MAKTX'     'X'
    'Material Description'           '20' '',
    w_col_pos 'EN_VEH_MODEL' 'EN_VEH_MODEL'  'X'
    'Model'                           '7' '',
    w_col_pos 'EN_HEAD'   'EN_HEAD'    ''
    'Head.'                           '6' ''.

*----> APPEND EN_SPC01 ~ EN_SPC60
  PERFORM build_en_spcno .
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
  w_sortcat-fieldname      = 'EN_ITEM'.
  w_sortcat-tabname        = 'IT_LIST'.
  w_sortcat-up             = 'X'.
  APPEND w_sortcat.

*  W_SORTCAT-SPOS           = 2.
*  W_SORTCAT-FIELDNAME      = 'EN_VEH_MODEL'.
*  W_SORTCAT-TABNAME        = 'IT_LIST'.
*  W_SORTCAT-UP             = 'X'.
*  APPEND W_SORTCAT.

ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader,
        l_list(50).

*----- Title
  CLEAR ls_line.
  ls_line-typ  = 'H'.
  ls_line-info = text-a01.
  APPEND ls_line TO lt_top_of_page.

**----- User
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'User: '.
*  LS_LINE-INFO = SY-UNAME.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Date
  ls_line-typ  = 'S'.
  ls_line-key  = text-a03 .
  WRITE sy-datum  TO  l_list .
  ls_line-info = l_list.
  APPEND ls_line TO lt_top_of_page.

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

**----- Total
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
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
FORM call_function.
  DATA: l_print_p TYPE slis_print_alv.  " print setting

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
ENDFORM.                    " EVENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
FORM create_interface_log.
*  DESCRIBE TABLE IT_ZTPPES LINES Z_TOTAL.
*
*  SELECT COUNT(*) FROM ZTPPES
*         INTO Z_SUCC
*         WHERE FLAG EQ 'S'
*           AND EN_ITEM  EQ P_MATNR
*           AND ZUSER EQ SY-UNAME
*           AND ZSDAT EQ SY-DATUM.
*
*  CHECK Z_TOTAL <> 0.
*  I_ZTCA_IF_LOG-TCODE    = 'ZPPI502'.
*  I_ZTCA_IF_LOG-TOTAL    = Z_TOTAL.
*  I_ZTCA_IF_LOG-ZSUCC    = Z_SUCC.
*  I_ZTCA_IF_LOG-ERROR    = Z_TOTAL - Z_SUCC.
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

ENDFORM.                    " CREATE_INTERFACE_LOG
*&-------------------------------------------------------------------
*&      Form  USER_COMMAND
*&-------------------------------------------------------------------
FORM user_command USING ucomm    LIKE sy-ucomm
                         selfield TYPE slis_selfield.
  DATA : sel_field LIKE selfield-sel_tab_field.

  CASE ucomm.
    WHEN '&RLE'.
      DESCRIBE TABLE it_ztppes LINES z_total.
      PERFORM transfer_pp_to_mes.
      PERFORM modify_ztppes.
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
  CLEAR : z_succ .
  CALL FUNCTION 'Z_FPP_ENGINE_PS'
    DESTINATION c_dest
    TABLES
      t_ztppes              = it_ztppes
    EXCEPTIONS
      communication_failure = 1  MESSAGE l_msgtxt
      system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc NE 0.
    MESSAGE i001 WITH text-302 .
    CASE c_mark .
      WHEN r1  .   " Transfer
        LOOP AT it_ztppes.
          l_tabix = sy-tabix.
          it_ztppes-zresult  = 'E' .
          it_ztppes-zuser    = sy-uname.
          it_ztppes-zsdat    = sy-datum.
          it_ztppes-zstim    = sy-uzeit.
          it_ztppes-zmode    = 'C'.
          it_ztppes-zmsg     = l_msgtxt .
          MODIFY it_ztppes INDEX l_tabix  .
        ENDLOOP.
      WHEN r2 .   " Re-transfer
        LOOP AT it_ztppes.
          l_tabix = sy-tabix.
          it_ztppes-zresult  = 'E' .
          it_ztppes-zmsg     = l_msgtxt .
          MODIFY it_ztppes INDEX l_tabix  .
        ENDLOOP.
    ENDCASE.
  ELSE.
    CASE c_mark .
      WHEN r1.    " Transfer
        LOOP AT it_ztppes.
          l_tabix = sy-tabix.
          IF it_ztppes-zzret = 'E'.
            it_ztppes-zresult  = 'E' .
            it_ztppes-zuser    = sy-uname.
            it_ztppes-zsdat    = sy-datum.
            it_ztppes-zstim    = sy-uzeit.
            it_ztppes-zmode    = 'C'.
            MODIFY it_ztppes INDEX l_tabix.
          ELSE.
            z_succ = z_succ + 1.
            it_ztppes-zresult  = 'S'.
            it_ztppes-zuser    = sy-uname.
            it_ztppes-zsdat    = sy-datum.
            it_ztppes-zstim    = sy-uzeit.
            it_ztppes-zmode    = 'C'.
            MODIFY it_ztppes INDEX l_tabix.
          ENDIF.
        ENDLOOP.
      WHEN r2 .   "Re-transfer
        LOOP AT it_ztppes .
          l_tabix = sy-tabix.
          IF it_ztppes-zzret   = 'E'.
            it_ztppes-zuser    = sy-uname.
            it_ztppes-zresult  = 'E' .
            MODIFY it_ztppes INDEX l_tabix.
          ELSE.
            z_succ = z_succ + 1.
            it_ztppes-zresult  = 'S'.
            it_ztppes-zuser    = sy-uname.
            MODIFY it_ztppes INDEX l_tabix.
          ENDIF.
        ENDLOOP.
    ENDCASE .
  ENDIF.
ENDFORM.                    " TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
FORM call_screen_result.

  z_fail = z_total - z_succ.

  CALL SCREEN 50 STARTING AT 20 10.

ENDFORM.                    " CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT

* --- 2004.02.19 CHANGED ---- Mr. MooN
*&---------------------------------------------------------------------*
*&      Form  SELECT_MARC
*&---------------------------------------------------------------------*
FORM select_marc.
  CLEAR : it_marc ,  it_marc[] .
  SELECT matnr
         fevor
         INTO TABLE it_marc
         FROM marc
** FOR E002
*         WHERE WERKS EQ C_E001
         WHERE werks EQ p_werks
** END
           AND matnr IN s_matnr
           AND fevor IN (c_sea, c_sec).

  SORT it_marc BY fevor matnr .
ENDFORM.                    " SELECT_MARC

*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM read_classification.
  CLEAR : it_ztppes , it_ztppes[].
  LOOP AT it_marc .
    CLEAR : it_vm , it_vm[] .
    MOVE it_marc-matnr   TO  it_ztppes-en_item  .
*----> READ Classification
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        object       = it_marc-matnr
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
      CLEAR it_vm .
      READ TABLE it_vm WITH KEY atnam = 'EN_VEH_MODEL' .
      it_ztppes-en_veh_model = it_vm-atwrt             .

      CLEAR it_vm .
      READ TABLE it_vm WITH KEY atnam = 'EN_HEAD'      .
      it_ztppes-en_head      = it_vm-atwrt             .

      PERFORM assign_en_spc  .
    ENDIF.

    CASE c_mark.
      WHEN r_1.
        it_ztppes-eflag = 'IR'.
      WHEN r_2.
        it_ztppes-eflag = 'RP'.
      WHEN r_3.
        it_ztppes-eflag = 'DL'.
    ENDCASE.
    APPEND it_ztppes .    CLEAR it_ztppes .

  ENDLOOP.

ENDFORM.                    " READ_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EN_SPC
*&---------------------------------------------------------------------*
FORM assign_en_spc.
  FIELD-SYMBOLS <lf_en> .
  DATA : l_enspc(6)        TYPE   c   ,
         l_enspc_no(8)     TYPE   c   ,
         l_en_3cspc(8)     TYPE   c   ,
         l_en_3cspc_no(10)  TYPE   c   ,
         l_ztppes(30)      TYPE   c   ,
         l_ztppes_no(40)   TYPE   c   ,
         l_num(2)          TYPE   n   .
  l_enspc    = 'EN_SPC'     .
  l_en_3cspc = 'EN_3CSPC'   .
  l_ztppes   = 'IT_ZTPPES-' .
  DO  60 TIMES  .
    CLEAR : l_enspc_no  .
    l_num = sy-index .
    CONCATENATE  l_enspc    l_num      INTO l_enspc_no    .
    CONCATENATE  l_ztppes   l_enspc_no INTO l_ztppes_no   .
    CONCATENATE  l_en_3cspc l_num      INTO l_en_3cspc_no .
    CASE it_marc-fevor .
      WHEN c_sea .
        CLEAR it_vm .
        READ TABLE it_vm WITH KEY atnam = l_enspc_no      .
        ASSIGN (l_ztppes_no)  TO  <lf_en> .
        <lf_en> = it_vm-atwrt .
      WHEN c_sec .
        CLEAR it_vm .
        READ TABLE it_vm WITH KEY atnam = l_en_3cspc_no   .
        ASSIGN (l_ztppes_no)  TO  <lf_en> .
        <lf_en> = it_vm-atwrt .
    ENDCASE.
  ENDDO .

ENDFORM.                    " ASSIGN_EN_SPC
*&---------------------------------------------------------------------*
*&      Form  BUILD_EN_SPCNO
*&---------------------------------------------------------------------*
FORM build_en_spcno.
  DATA : l_enspc(6)        TYPE   c   ,
         l_enspc_no(8)     TYPE   c   ,
         l_spc(3)          TYPE   c   ,
         l_spc_no(5)       TYPE   c   ,
         l_num(2)          TYPE   n   .
  l_enspc  = 'EN_SPC'     .
  l_spc    = 'SPC'        .
  DO 60 TIMES .
    CLEAR : l_enspc_no  .
    l_num = sy-index .
    CONCATENATE  l_enspc  l_num      INTO  l_enspc_no  .
    CONCATENATE  l_spc    l_num      INTO  l_spc_no    .
    append_fieldcat
      w_col_pos l_enspc_no  l_enspc_no  ''
      l_spc_no                        '6'  ''.
  ENDDO.
ENDFORM.                    " BUILD_EN_SPCNO
