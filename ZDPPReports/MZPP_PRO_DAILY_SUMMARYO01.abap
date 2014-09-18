*----------------------------------------------------------------------*
*   INCLUDE MZPP_PRO_DAILY_SUMMARYO01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: daynr   LIKE hrvsched-daynr,
        daytxt  LIKE hrvsched-daytxt.

  SET PF-STATUS '100'.

  CALL FUNCTION 'RH_GET_DATE_DAYNAME'
       EXPORTING
            langu  = sy-langu
            date   = sy-datum
       IMPORTING
            daynr  = daynr
            daytxt = daytxt.

  SET TITLEBAR '100' WITH sy-datum daytxt.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  it_c101_change_tc_attr OUTPUT.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*output module for tc 'IT_C101'. do not change this line!
*update lines for equivalent scrollbar
MODULE it_c101_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_act LINES it_c101-lines.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  initial  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE initial OUTPUT.
*current date
*  p_date = sy-datum.
*Get plant

ENDMODULE.                 " initial  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  it_cont2_change_tc_attr OUTPUT.
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*output module for tc 'IT_CONT2'. do not change this line!
*update lines for equivalent scrollbar
MODULE it_cont2_change_tc_attr OUTPUT.
  DESCRIBE TABLE it_wip LINES it_cont2-lines.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  screen_modify  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE screen_modify OUTPUT.
  LOOP AT SCREEN.
    IF sy-tcode EQ 'ZPPAPDS_U'.
      IF screen-group1 EQ 'GR1'.
        screen-input = 1.
      ENDIF.
    ELSEIF sy-tcode EQ 'ZPPAPDS'.
      IF screen-group1 EQ 'GR1'.
        screen-input = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.                 " screen_modify  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TEXT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_text OUTPUT.
  IF editor IS INITIAL.
*   create control container
    CREATE OBJECT textedit_custom_container
        EXPORTING
            container_name = 'CONTAINER1'
        EXCEPTIONS
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      add your handling
    ENDIF.
    mycontainer = 'CONTAINER1'.
*   create calls constructor, which initializes, creats and links
*   TextEdit Control
    CREATE OBJECT editor
          EXPORTING
           parent = textedit_custom_container
           wordwrap_mode =
*               cl_gui_textedit=>wordwrap_off
              cl_gui_textedit=>wordwrap_at_fixed_position
*              cl_gui_textedit=>WORDWRAP_AT_WINDOWBORDER
           wordwrap_position = line_length
           wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
*   to handle different containers
    container_linked = 1.
    REFRESH mytable.
    CASE sy-dynnr.
      WHEN '0100'.
*don't show toolbar and statusbar on this screen
        CALL METHOD editor->set_toolbar_mode
            EXPORTING
               toolbar_mode = editor->true.
        CALL METHOD editor->set_statusbar_mode
            EXPORTING
               statusbar_mode = editor->true.
*STATUSBAR_MODE = editor->TRUE.
*FALSE?  TRUE?
        CASE sy-tcode.
          WHEN 'ZPPAPDS_U'.
            CALL METHOD editor->set_readonly_mode
                         EXPORTING
                            readonly_mode = editor->false.
          WHEN 'ZPPAPDS'.
            CALL METHOD editor->set_readonly_mode
                         EXPORTING
                            readonly_mode = editor->true.
        ENDCASE.
    ENDCASE.
* finally flush
    CALL METHOD cl_gui_cfw=>flush
           EXCEPTIONS
             OTHERS = 1.
    IF sy-subrc NE 0.
*   add your handling
    ENDIF.
  ENDIF.
ENDMODULE.                 " SET_TEXT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  read_text  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE move_text OUTPUT.
*send table to control
  CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
       TABLES
            text_stream = mytable
            itf_text    = itftext.

  CALL METHOD editor->set_text_as_stream
          EXPORTING text = mytable.

ENDMODULE.                 " read_text  OUTPUT
