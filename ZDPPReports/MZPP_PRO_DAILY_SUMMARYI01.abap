*----------------------------------------------------------------------*
*   INCLUDE MZPP_PRO_DAILY_SUMMARYI01                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  LEAVE TO SCREEN 0.

ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN 'EXEC'.
      PERFORM get_data_cont1.
    WHEN 'SAVE'.
      PERFORM save_data.
    WHEN 'PRNT'.
      PERFORM PRINT_REPORT.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  get_text  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_text INPUT.
  CLEAR   : mytable, itftext.
  REFRESH : mytable, itftext.
*   retrieve table from control
  CALL METHOD editor->get_text_as_stream
               IMPORTING text = mytable.

  CALL FUNCTION 'CONVERT_STREAM_TO_ITF_TEXT'
       TABLES
            text_stream = mytable
            itf_text    = itftext.

ENDMODULE.                 " get_text  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_ACT_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_act_modify INPUT.
  DATA : f_twip LIKE it_act-twip.
  CLEAR f_twip.
  IF it_act-model EQ 'TOTAL'.
    CLEAR it_act-twip.
    LOOP AT it_act WHERE model <> 'TOTAL'.
      f_twip = f_twip + it_act-twip.
    ENDLOOP.
    it_act-model = 'TOTAL'.
    it_act-twip = f_twip.
  ENDIF.
  MODIFY it_act INDEX it_c101-current_line.

ENDMODULE.                 " IT_ACT_MODIFY  INPUT
*&---------------------------------------------------------------------*
*&      Module  IT_WIP_MODIFY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE it_wip_modify INPUT.

  DATA : f_dqty LIKE it_wip-dqty,
         f_cretu LIKE it_wip-cretu,
         f_cpass LIKE it_wip-cpass,
         f_sqty  LIKE it_wip-sqty,
         f_shmaw LIKE it_wip-shmaw,
         f_ysum  LIKE it_wip-ysum.

  CLEAR: f_dqty,f_cretu,f_cpass,f_sqty,f_shmaw,f_ysum.

  IF it_wip-model EQ 'TOTAL'.
    CLEAR:it_wip-dqty,it_wip-cretu,it_wip-cpass,it_wip-sqty,
          it_wip-shmaw,it_wip-ysum.

    LOOP AT it_wip WHERE model <> 'TOTAL'.
      f_dqty = f_dqty + it_wip-dqty.
      f_cretu = f_cretu + it_wip-cretu.
      f_cpass = f_cpass + it_wip-cpass.
      f_sqty  = f_sqty  + it_wip-sqty.
      f_shmaw = f_shmaw + it_wip-shmaw.
      f_ysum  = f_ysum  + it_wip-ysum.
    ENDLOOP.

    it_wip-model = 'TOTAL'.
    it_wip-dqty  = f_dqty.
    it_wip-cretu = f_cretu.
    it_wip-cpass = f_cpass.
    it_wip-sqty  = f_sqty.
    it_wip-shmaw = f_shmaw.
    it_wip-ysum  = f_ysum.
  ENDIF.

  MODIFY it_wip INDEX it_cont2-current_line.
ENDMODULE.                 " IT_WIP_MODIFY  INPUT
