************************************************************************
* Program name : ZEMMPM23E_ERRORLOG
* Created by   : Min-su Park
* Created on   : 2003.11.03.
* Pattern      :
* Description  :
*   1. Modified Welcome screen-For Inbound delivery-Putaway process    *
*   2. Empty Container management                                      *
*                                                                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.03.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************

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

*Container No.
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ZCONTAINER'.
  ls_fieldcat-ref_fieldname = 'ZCONTAINER'.
  ls_fieldcat-key           = 'X'.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Container'.
  ls_fieldcat-seltext_m     = 'Container'.
  ls_fieldcat-seltext_s     = 'Container'.
  ls_fieldcat-outputlen     = '20'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Delivery
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'VBELN'.
  ls_fieldcat-ref_fieldname = 'VBELN'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Inbound Delivery'.
  ls_fieldcat-seltext_m     = 'Inbound Delivery'.
  ls_fieldcat-seltext_s     = 'Inbound Delivery'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Error Postion
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'EPOSITION'.
  ls_fieldcat-ref_fieldname = 'EPOSITION'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Error Postion'.
  ls_fieldcat-seltext_m     = 'Error Postion'.
  ls_fieldcat-seltext_s     = 'Error Postion'.
  ls_fieldcat-outputlen     = '20'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Purchasing Document Number
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'EBELN'.
  ls_fieldcat-ref_fieldname = 'EBELN'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Purchase No.'.
  ls_fieldcat-seltext_m     = 'Purchase No.'.
  ls_fieldcat-seltext_s     = 'Purchase No.'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Bin Information
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'PARKING_TXT'.
  ls_fieldcat-ref_fieldname = 'PARKING_TXT'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Bin info'.
  ls_fieldcat-seltext_m     = 'Bin info'.
  ls_fieldcat-seltext_s     = 'Bin info'.
  ls_fieldcat-outputlen     = '30'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Error Message
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'TEXT'.
  ls_fieldcat-ref_fieldname = 'TEXT'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Error Message'.
  ls_fieldcat-seltext_m     = 'Error Message'.
  ls_fieldcat-seltext_s     = 'Error Message'.
  ls_fieldcat-outputlen     = '40'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*User
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ERDAT'.
  ls_fieldcat-ref_fieldname = 'ERDAT'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Create Day'.
  ls_fieldcat-seltext_m     = 'Create Day'.
  ls_fieldcat-seltext_s     = 'Create Day'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Entry time
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ERZET'.
  ls_fieldcat-ref_fieldname = 'ERZET'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Create Time'.
  ls_fieldcat-seltext_m     = 'Create Time'.
  ls_fieldcat-seltext_s     = 'Create Time'.
  ls_fieldcat-outputlen     = '10'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.

*Entry time
  CLEAR ls_fieldcat.
  pos = pos + 1.
  ls_fieldcat-col_pos       = pos.
  ls_fieldcat-fieldname     = 'ERNAM'.
  ls_fieldcat-ref_fieldname = 'ERNAM'.
  ls_fieldcat-key           = ''.
  ls_fieldcat-qfieldname    = ''.
  ls_fieldcat-cfieldname    = ''.
  ls_fieldcat-seltext_l     = 'Creator'.
  ls_fieldcat-seltext_m     = 'Creator'.
  ls_fieldcat-seltext_s     = 'Creator'.
  ls_fieldcat-outputlen     = '12'.
  ls_fieldcat-no_out        = ''.
  APPEND ls_fieldcat TO  rt_fieldcat.
ENDFORM.
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
  ls_line-info = text-100.
  APPEND ls_line TO lt_top_of_page.

**Container Selection Range Display
*  CLEAR info_txt.
*  info_txt+0(4)  = 'From'    .
*  info_txt+5(20)  = s_cntain-low .
*  info_txt+26(2) = 'To'      .
*  info_txt+29(20) = s_cntain-high.
*  CLEAR ls_line.
*  ls_line-typ  = 'S'.
*  ls_line-key  = 'Container:'.
*  ls_line-info = info_txt.
*  APPEND ls_line TO lt_top_of_page.
ENDFORM.
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
*&      Form  MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM make_basic_data.
*---
  CLEAR : it_error, it_error[], it_ztmm_error, it_ztmm_error[].

*--- Error
  IF p_error NE space.
    SELECT * FROM ztmm_ct_errlog
             INTO CORRESPONDING FIELDS OF TABLE it_error
            WHERE zcontainer IN s_cntain
              AND erdat IN s_erdat
              AND ernam IN s_ernam
              AND vbeln IN s_vbeln
              AND ebeln IN s_ebeln
              AND flag EQ space.  "if flag is space, not yet processed.
  ENDIF.

*--- Already Processed
  IF p_fixed NE space.
    SELECT * FROM ztmm_ct_errlog
             APPENDING CORRESPONDING FIELDS OF TABLE it_error
            WHERE zcontainer IN s_cntain
              AND erdat IN s_erdat
              AND ernam IN s_ernam
              AND vbeln IN s_vbeln
              AND ebeln IN s_ebeln
              AND flag NE space.     " if flag is 'X', already fixed...
  ENDIF.

*---
  LOOP AT it_error WHERE flag EQ space.
*--- check inbound delivery status
    CLEAR : vbuk.
    SELECT SINGLE gbstk INTO vbuk-gbstk
                        FROM vbuk
                       WHERE vbeln EQ it_error-vbeln
                         AND gbstk EQ 'C'.
    IF sy-subrc EQ 0.     " completed
      MOVE : 'X'     TO it_error-flag,
             c_green TO it_error-linecolor.
    ELSE.
      MOVE : space TO it_error-flag,
             c_red TO it_error-linecolor.
    ENDIF.
    MOVE : sy-datum TO it_error-aedat,
           sy-uzeit TO it_error-aezet,
           sy-uname TO it_error-aenam.
*--- check inbound delivery delete or not...
    CLEAR : likp.
    SELECT SINGLE vbeln INTO likp-vbeln
                        FROM likp
                       WHERE vbeln EQ it_error-vbeln.
    IF sy-subrc NE 0.
      MOVE : 'D' TO it_error-flag.
    ENDIF.
*---
    MODIFY it_error.
    MOVE-CORRESPONDING it_error TO it_ztmm_error.
    APPEND it_ztmm_error.
    CLEAR : it_error, it_ztmm_error.
  ENDLOOP.

*--- modify error log table
  MODIFY ztmm_ct_errlog FROM TABLE it_ztmm_error.
  COMMIT WORK.

*--- delete already completed record from internal table
  IF p_fixed EQ space.
    DELETE it_error WHERE flag NE space.
  ENDIF.
ENDFORM.                    " MAKE_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELD_BUILD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_field_build.
*---
  w_repid = sy-repid.

  CLEAR : it_fieldcat[], wa_events[], wa_list_top_of_page[].

  PERFORM fieldcat_init  USING it_fieldcat[].
  PERFORM eventtab_build USING wa_events[].
  PERFORM comment_build  USING wa_list_top_of_page[].
ENDFORM.                    " ALV_FIELD_BUILD

*---------------------------------------------------------------------*
*       FORM set_status                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  EXTAB                                                         *
*---------------------------------------------------------------------*
FORM set_status USING extab TYPE slis_t_extab.
*---
  SET PF-STATUS 'BASE'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM user_command                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  UCOMM                                                         *
*  -->  SELFIELD                                                      *
*---------------------------------------------------------------------*
FORM user_command USING ucomm LIKE sy-ucomm
                       selfield TYPE slis_selfield.
*---
  DATA : l_vbeln LIKE likp-vbeln.

  READ TABLE it_error  INDEX selfield-tabindex.

  CASE ucomm.
    WHEN 'REFRESH'.
      PERFORM make_basic_data.
    WHEN '&IC1'.
      CHECK sy-subrc EQ 0.
      CLEAR : l_vbeln.
      MOVE : it_error-vbeln TO l_vbeln.
      SET PARAMETER ID 'VL' FIELD l_vbeln.
      SET PARAMETER ID 'VLM' FIELD l_vbeln.
      SET PARAMETER ID 'VLG' FIELD l_vbeln.
      CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
      CLEAR : it_error.
  ENDCASE.
ENDFORM.                    "user_command
