*----------------------------------------------------------------------*
* MODULE  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
* TEXT :
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA lv_cnt TYPE p.
  DATA lv_ans(1).

  CLEAR ok_code.
  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN 'EXIT' OR 'CANC' OR 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'PROC'.
      CLEAR: gt_send[]. "Victor 06.30.2011

      IF gv_docnum EQ ''.
        MESSAGE s001 WITH 'select only one docnum '.
      ENDIF.

      CHECK gv_docnum NE ''.
      READ TABLE gt_idoc WITH KEY docnum = gv_docnum.
      CHECK sy-subrc = 0 .

      IF gt_idoc-status = ''.
        LOOP AT gt_zposeg1 WHERE docnum = gv_docnum .
          APPEND gt_zposeg1 TO gt_send.
        ENDLOOP.
        PERFORM popup_to_confirm_1 USING  'S'
                                          'Confirm'
                                          text-m01
                                          ' '
                                          ''
                                 CHANGING lv_ans.
        CHECK lv_ans NE 'N'.
        PERFORM p3000_po_proc
        .
      ELSE.

        SORT gt_zposeg1 BY docnum prdod natn dist wkexc wkinc.
        LOOP AT gt_idoc WHERE status EQ 'E'.
          READ TABLE gt_zposeg1 WITH KEY docnum = gt_idoc-docnum
                                         prdod  = gt_idoc-wo_ser
                                         natn   = gt_idoc-natn
                                         dist   = gt_idoc-dist
                                         wkexc  = gt_idoc-wkexc
                                         wkinc  = gt_idoc-wkinc .
          IF sy-subrc = 0 .
            APPEND gt_zposeg1 TO gt_send.
          ENDIF.
        ENDLOOP.
        IF gt_send[] IS INITIAL.
          MESSAGE s001 WITH 'this data already processed'.
        ELSE.
          PERFORM popup_to_confirm_1 USING  'S'
                                    'Confirm'
                                    text-m01
                                    ' '
                                    ''
                           CHANGING lv_ans.
          CHECK lv_ans NE 'N'.
          PERFORM p3000_po_proc.

        ENDIF.

      ENDIF.

    WHEN 'REJC'.
      READ TABLE gt_data INDEX 1 .
      CHECK gt_data-status IS INITIAL.
      PERFORM popup_to_confirm_1 USING  'S'
                                    'Confirm'
                                        text-m01
                                    ' '
                                    ''
                           CHANGING lv_ans.
      CHECK lv_ans NE 'N'.
      PERFORM p3100_po_rejc.

    WHEN OTHERS.
* §6. Call dispatch to process toolbar functions
      CALL METHOD cl_gui_cfw=>dispatch.
  ENDCASE.

  CALL METHOD cl_gui_cfw=>dispatch.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.                 " user_command_0100  INPUT
*---------------------------------------------------------------------*
*       MODULE CANCEL INPUT                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE cancel INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
