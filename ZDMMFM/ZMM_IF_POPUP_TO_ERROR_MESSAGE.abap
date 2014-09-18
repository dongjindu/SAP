FUNCTION zmm_if_popup_to_error_message.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(XLOGNO) OPTIONAL
*"     REFERENCE(XDOCNO_SHOW) DEFAULT SPACE
*"  TABLES
*"      XMSG STRUCTURE  ZTISMESSAGE OPTIONAL
*"----------------------------------------------------------------------

  DATA: l_seq   LIKE xmsg-msgsq.

  CLEAR: l_seq.
  LOOP AT xmsg.
    ADD 1 TO l_seq.
*    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*      EXPORTING
*        msgid               = xmsg-msgid
*        msgnr               = xmsg-msgno
*        msgv1               = xmsg-msgv1
*        msgv2               = xmsg-msgv2
*        msgv3               = xmsg-msgv3
*        msgv4               = xmsg-msgv4
*      IMPORTING
*        message_text_output = xmsg-msgtx.

    CASE xmsg-msgty.
      WHEN 'A' OR 'E'.
        MOVE: icon_led_red        TO xmsg-micon.
      WHEN 'S'.
        MOVE: icon_led_green      TO xmsg-micon.
      WHEN 'W'.
        MOVE: icon_led_yellow     TO xmsg-micon.
      WHEN 'I'.
        MOVE: icon_led_green      TO xmsg-micon.
      WHEN OTHERS.

    ENDCASE.

    xmsg-msgsq = l_seq.
    xmsg-logno = xlogno.

    MODIFY xmsg.
  ENDLOOP.

  FREE : t_fieldcat.  CLEAR: t_fieldcat.
  mfc 'XMSG'   'MICON'    'ZTISMESSAGE' 'MICON' '' '' ''.
  mfc 'XMSG'   'MSGTX'    'ZTISMESSAGE' 'MSGTX' '' '' ''.
  mfc 'XMSG'   'MSGTY'    'ZTISMESSAGE' 'MSGTY' '' '' ''.
  IF xdocno_show EQ 'X'.
    mfc 'XMSG'   'REFNO'    'ZTISMESSAGE' 'REFNO' '' '' ''.
  ENDIF.

  LOOP AT t_fieldcat.
    IF t_fieldcat-fieldname EQ 'MICON'.
      t_fieldcat-icon = 'X'.
      t_fieldcat-seltext_l = 'Type'.
      t_fieldcat-seltext_m = 'Type'.
      t_fieldcat-seltext_s = 'Type'.
      t_fieldcat-ddictxt   = 'S'.
      t_fieldcat-just = 'C'.
      t_fieldcat-outputlen = 4.
    ELSEIF t_fieldcat-fieldname EQ 'MSGTX'.
      t_fieldcat-outputlen = 70.
*      t_fieldcat-hotspot = 'X'.
    ELSEIF t_fieldcat-fieldname EQ 'MSGTY'.
      t_fieldcat-just = 'C'.
    ENDIF.
    MODIFY t_fieldcat INDEX sy-tabix.
  ENDLOOP.

  g_title = 'Check out message'(001).
  REFRESH: extab.

  APPEND '&NT1'      TO extab.
  APPEND '&ALL'      TO extab.
  APPEND '&SAL'      TO extab.
*  APPEND '&IC1'      TO extab.
*  APPEND '&ETA'      TO extab.
*
  dyname = 'SAPLZ_TI_COMMON_FUNCTIONS'.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title                 = g_title
      i_selection             = 'X'
*      i_allow_no_selection    = 'X'
      i_zebra                 = 'X'
*      i_checkbox_fieldname    = 'BOX'
*      i_linemark_fieldname    = 'BOX'
*      i_scroll_to_sel_line    = 'X'
      i_tabname               = 'XMSG'
      it_fieldcat             = t_fieldcat[]
      is_private              = gs_private
      i_screen_start_column   = 2
      i_screen_start_line     = 4
      i_screen_end_column     = 100
      i_screen_end_line       = 20
      it_excluding            = extab
      i_callback_program      = dyname
      i_callback_user_command = 'USER_POPUP_COMMAND'
    IMPORTING
      es_selfield             = gs_selfield
      e_exit                  = g_exit
    TABLES
      t_outtab                = xmsg[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc <> 0.
    MESSAGE i000(0k) WITH sy-subrc.
  ENDIF.

ENDFUNCTION.
