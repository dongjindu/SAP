****************************************************************
* Program Name  : ZIMMGM19I_SUBPART_REQUEST
* Created by    : Min-su Park
* Created on    : 2003.11.06.
* Pattern       :
* Description   : SUB PART REQUEST(Out bound)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2003.11.06.     Min-su Park    UD1K901873     Initial Coding
***************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZIMMGM19I_SUBPART_REQUESTF01                               *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
*Get data for Processing
  IF     r1 EQ 'X'.
    SELECT * FROM ztmm_end_part
             INTO CORRESPONDING FIELDS OF TABLE it_end_part
            WHERE return_code = space
              AND flag <> 'E'.
  ELSE.
    SELECT * FROM ztmm_end_part
             INTO CORRESPONDING FIELDS OF TABLE it_end_part
            WHERE flag = 'E'.
  ENDIF.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_RFC_WITH_EAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM execute_rfc_with_eai.
  PERFORM exe_rfc.
  PERFORM modify_ztable.
*  PERFORM MODIFY_DISPLAY_ITAB.
ENDFORM.                    " EXECUTE_RFC_WITH_EAI
*&---------------------------------------------------------------------*
*&      Form  EXE_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exe_rfc.
  DATA : lw_msgtxt(100),
         lw_tabix  LIKE sy-tabix.

*RFC function Call
  CHECK NOT it_end_part IS INITIAL.
  CALL FUNCTION 'Z_FMM_END_PART'
    DESTINATION              c_dest
    TABLES
      it_ztmm_end_part       = it_end_part
    EXCEPTIONS
      communication_failure  = 1  MESSAGE lw_msgtxt
      system_failure         = 2  MESSAGE lw_msgtxt.
*Result processing
  LOOP AT it_end_part.
    IF it_end_part-zzret = 'S'.
      it_end_part-flag = it_end_part-zzret.
    ELSE.
      it_end_part-zzret = 'E'.
      it_end_part-flag  = it_end_part-zzret.
    ENDIF.
    MODIFY it_end_part.
  ENDLOOP.
ENDFORM.                    " EXE_RFC
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZTABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_ztable.
  MODIFY ztmm_end_part FROM TABLE it_end_part.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " MODIFY_ZTABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data.
  w_repid = sy-repid.
  CLEAR : it_fieldcat[], wa_events[], wa_list_top_of_page[].
  PERFORM fieldcat_init  USING it_fieldcat[].
  PERFORM eventtab_build USING wa_events[].
  PERFORM comment_build  USING wa_list_top_of_page[].
  PERFORM alv_display.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT[]  text
*----------------------------------------------------------------------*
FORM fieldcat_init USING rt_fieldcat TYPE slis_t_fieldcat_alv.
  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  DATA: pos TYPE i.

*End Part No.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'REQUEST_PART'.
  ls_fieldcat-ref_fieldname = 'REQUEST_PART'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'End Part No'.
  ls_fieldcat-seltext_m     = 'End Part No'.
  ls_fieldcat-seltext_s     = 'End Part No'.
  ls_fieldcat-outputlen     = '18'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Flag
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'FLAG'.
  ls_fieldcat-ref_fieldname = 'FLAG'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Flag'.
  ls_fieldcat-seltext_m     = 'Flag'.
  ls_fieldcat-seltext_s     = 'Flag'.
  ls_fieldcat-outputlen     = '1'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Message
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ZMSG'.
  ls_fieldcat-ref_fieldname = 'ZMSG'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Message'.
  ls_fieldcat-seltext_m     = 'Message'.
  ls_fieldcat-seltext_s     = 'Message'.
  ls_fieldcat-outputlen     = '220'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

ENDFORM.                    " FIELDCAT_INIT
*&---------------------------------------------------------------------*
*&      Form  EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_EVENTS[]  text
*----------------------------------------------------------------------*
FORM eventtab_build USING lt_events TYPE slis_t_event.
  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            i_list_type = 0
       IMPORTING
            et_events   = lt_events.
  READ TABLE lt_events WITH KEY name =  slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE w_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO lt_events.
  ENDIF.
ENDFORM.                    " EVENTTAB_BUILD
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_LIST_TOP_OF_PAGE[]  text
*----------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.
  DATA: ls_line TYPE slis_listheader.
  DATA: info_txt(50).

  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  not used for this type
  ls_line-info = text-h01.
  APPEND ls_line TO lt_top_of_page.

*Date Selection Range Display
  CLEAR info_txt.
*  INFO_TXT+0(4)   = 'Date'    .
  info_txt+5(10)  = sy-datum.
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key  = 'Date:'.
  ls_line-info = info_txt.
  APPEND ls_line TO lt_top_of_page.
ENDFORM.                    " COMMENT_BUILD
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM top_of_page.
*
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'HTMLCNTL_TESTHTM2_SAPLOGO'
*           I_LOGO             = 'ENJOYSAP_LOGO'
            it_list_commentary = wa_list_top_of_page.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program           = w_repid
*     I_STRUCTURE_NAME             =
*     IT_SORT                      = WA_SORT[]
      it_events                    = wa_events[]
      it_fieldcat                  = it_fieldcat[]
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER        =
*   ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                     = it_end_part.
ENDFORM.                    " ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_interface_log.
  CLEAR : wa_ztca_if_log.

  DESCRIBE TABLE it_end_part LINES w_total.
  LOOP AT it_end_part.
    IF it_end_part-flag = 'S'.
      wa_ztca_if_log-zsucc = wa_ztca_if_log-zsucc + 1.
    ELSEIF it_end_part-flag = 'E'.
      wa_ztca_if_log-error = wa_ztca_if_log-error + 1.
    ENDIF.
  ENDLOOP.

  CHECK w_total <> 0.
  wa_ztca_if_log-tcode    = 'ZMMI21'.
*  wa_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
*  wa_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
  wa_ztca_if_log-total    = w_total.
  wa_ztca_if_log-erdat    = sy-datum. "Created on.
  wa_ztca_if_log-erzet    = sy-uzeit. "Created time.
  wa_ztca_if_log-ernam    = sy-uname. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      i_ztca_if_log             = wa_ztca_if_log
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     update_failed              = 1
     number_range_error         = 2
     tcode_does_not_exist       = 3
     OTHERS                     = 4
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CREATE_INTERFACE_LOG
