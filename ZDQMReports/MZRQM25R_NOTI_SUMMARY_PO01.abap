*----------------------------------------------------------------------*
***INCLUDE MZRQM25R_NOTI_SUMMARY_PO01 .
*----------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*& Date        User          Transport           Description
*& 12/07/2004  Shiva         UD1K913400      Changed the text editor
*&                             wordwrap attribute in the custom control.
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9200 OUTPUT.
  SET PF-STATUS 'PF_100'.
*  SET TITLEBAR 'xxx'.
* IMCUST_CONT1 -> Hmma Quality control comm
  if w_editor1 is initial.
    create object w_cust_cont1
           exporting container_name = 'IMCUST_CONT1'
          exceptions
              cntl_error        = 1
              cntl_system_error = 2
              create_error      = 3
              lifetime_error    = 4
              lifetime_dynpro_dynpro_link = 5.

    create object w_editor1
        exporting
            parent = w_cust_cont1
          wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = w_llen
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
* IMcust_cont2 ->Rootcause + Process(Cause of Nonconformity)
    create object w_cust_cont2
           exporting container_name = 'IMCUST_CONT2'
          exceptions
              cntl_error        = 1
              cntl_system_error = 2
              create_error      = 3
              lifetime_error    = 4
              lifetime_dynpro_dynpro_link = 5.

    create object w_editor2
        exporting
            parent = w_cust_cont2
          wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = w_llen
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
*    perform load_text.

* add more text objects 100565 06/30/05
* RC_CONTRL ->Corr act + Process(Cause of Nonconformity)
create object w_cust_cont3
           exporting container_name = 'RC_CONTRL'
          exceptions
              cntl_error        = 1
              cntl_system_error = 2
              create_error      = 3
              lifetime_error    = 4
              lifetime_dynpro_dynpro_link = 5.
create object w_editor3
        exporting
            parent = w_cust_cont3
          wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = w_llen
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
* Corr system -> Corr act + system(cause of Non detection)
create object w_cust_cont4
           exporting container_name = 'CORR_SYSTEM'
          exceptions
              cntl_error        = 1
              cntl_system_error = 2
              create_error      = 3
              lifetime_error    = 4
              lifetime_dynpro_dynpro_link = 5.
create object w_editor4
        exporting
            parent = w_cust_cont4
          wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = w_llen
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
* RC system -> Rootcause + system(cause of Non detection)
create object w_cust_cont5
           exporting container_name = 'RC_SYSTEM'
          exceptions
              cntl_error        = 1
              cntl_system_error = 2
              create_error      = 3
              lifetime_error    = 4
              lifetime_dynpro_dynpro_link = 5.
create object w_editor5
        exporting
            parent = w_cust_cont5
          wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = w_llen
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
* Corr_Act_app -> Were corrective actions applied
create object w_cust_cont6
           exporting container_name = 'CORR_ACT_APP'
          exceptions
              cntl_error        = 1
              cntl_system_error = 2
              create_error      = 3
              lifetime_error    = 4
              lifetime_dynpro_dynpro_link = 5.
create object w_editor6
        exporting
            parent = w_cust_cont6
          wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = w_llen
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.
* METH_VAL_EFF -> Method used to validate effectiveness
create object w_cust_cont7
           exporting container_name = 'METH_VAL_EFF'
          exceptions
              cntl_error        = 1
              cntl_system_error = 2
              create_error      = 3
              lifetime_error    = 4
              lifetime_dynpro_dynpro_link = 5.
create object w_editor7
        exporting
            parent = w_cust_cont7
          wordwrap_mode = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = w_llen
          wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

*end addition
   perform load_text.
  else.
    call method w_editor1->delete_text.
    call method w_editor2->delete_text.

* modification by 100565
call method w_editor3->delete_text.
call method w_editor4->delete_text.
call method w_editor5->delete_text.
call method w_editor6->delete_text.
call method w_editor7->delete_text.
* end modification.
    perform load_text.
  endif.
ENDMODULE.                 " STATUS_9200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  set_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_screen OUTPUT.

  call method w_editor2->set_toolbar_mode
            exporting toolbar_mode = 0.
  call method w_editor2->set_readonly_mode
         exporting readonly_mode = 1.
*&--------------------------------------------------------------------&*
*&    For the text to be in readable format used 64 characters
*&    as length, so please don't change the value. - Shiva.
*&--------------------------------------------------------------------&*
  call method w_editor2->set_toolbar_mode
            exporting toolbar_mode = 0.

  call method w_editor2->set_wordwrap_behavior
         exporting wordwrap_mode = 2
*                 wordwrap_position = '64'
                   wordwrap_position = '45'
                   wordwrap_to_linebreak_mode = 1.
 call method w_editor1->set_toolbar_mode
            exporting toolbar_mode = 0.

  call method w_editor1->set_wordwrap_behavior
         exporting wordwrap_mode = 2
*                  wordwrap_position = '64'
                     wordwrap_position = '45'
                   wordwrap_to_linebreak_mode = 1.
* Modification by 100565
call method w_editor3->set_toolbar_mode
            exporting toolbar_mode = 0.

 call method w_editor3->set_wordwrap_behavior
         exporting wordwrap_mode = 2
*                   wordwrap_position = '64'
  wordwrap_position = '45'
                   wordwrap_to_linebreak_mode = 1.
  call method w_editor4->set_toolbar_mode
            exporting toolbar_mode = 0.

 call method w_editor4->set_wordwrap_behavior
         exporting wordwrap_mode = 2
                   wordwrap_position = '44'
                   wordwrap_to_linebreak_mode = 1.
call method w_editor5->set_toolbar_mode
            exporting toolbar_mode = 0.

 call method w_editor5->set_wordwrap_behavior
         exporting wordwrap_mode = 2
                   wordwrap_position = '44'
                   wordwrap_to_linebreak_mode = 1.
 call method w_editor6->set_toolbar_mode
            exporting toolbar_mode = 0.

 call method w_editor6->set_wordwrap_behavior
         exporting wordwrap_mode = 2
                   wordwrap_position = '64'
                   wordwrap_to_linebreak_mode = 1.
 call method w_editor7->set_toolbar_mode
            exporting toolbar_mode = 0.

 call method w_editor7->set_wordwrap_behavior
         exporting wordwrap_mode = 2
                   wordwrap_position = '52'
                   wordwrap_to_linebreak_mode = 1.

* end modification
  read table it_zsqm_noti_sum_p with key qmnum = qmel-qmnum.
  if not it_zsqm_noti_sum_p-success is initial or
         it_zsqm_noti_sum_p-txt04 eq 'NOCO'.
    SET PF-STATUS 'PF_100' excluding 'SAVE'.
    loop at screen.
      if screen-group1 = 'CGP'.
        screen-input = '0'.
        modify screen.
      endif.
    endloop.
    call method w_editor1->set_readonly_mode
           exporting readonly_mode = 1.
    call method cl_gui_cfw=>flush.
    call method w_editor3->set_readonly_mode
           exporting readonly_mode = 1.
    call method cl_gui_cfw=>flush.

call method w_editor4->set_readonly_mode
           exporting readonly_mode = 1.
    call method cl_gui_cfw=>flush.
    call method w_editor5->set_readonly_mode
           exporting readonly_mode = 1.
    call method cl_gui_cfw=>flush.
call method w_editor6->set_readonly_mode
           exporting readonly_mode = 1.
    call method cl_gui_cfw=>flush.
call method w_editor7->set_readonly_mode
           exporting readonly_mode = 1.
    call method cl_gui_cfw=>flush.


  else.
    if not it_zsqm_noti_sum_p-completed is initial.
      loop at screen.
        if screen-name = 'QMEL-COMPLETED'.
          screen-input = '0'.
          modify screen.
        endif.
      endloop.
    endif.
    call method w_editor1->set_readonly_mode
           exporting readonly_mode = 0.
    call method cl_gui_cfw=>flush.
call method w_editor3->set_readonly_mode
           exporting readonly_mode = 0.
    call method cl_gui_cfw=>flush.
call method w_editor4->set_readonly_mode
           exporting readonly_mode = 0.
    call method cl_gui_cfw=>flush.
call method w_editor5->set_readonly_mode
           exporting readonly_mode = 0.
    call method cl_gui_cfw=>flush.
call method w_editor6->set_readonly_mode
           exporting readonly_mode = 0.
    call method cl_gui_cfw=>flush.
call method w_editor7->set_readonly_mode
           exporting readonly_mode = 0.
    call method cl_gui_cfw=>flush.


  endif.
ENDMODULE.                 " set_screen  OUTPUT
