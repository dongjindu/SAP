*----------------------------------------------------------------------*
***INCLUDE MZRQM25R_NOTI_SUMMARY_PI01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9200 INPUT.
  data: w_okcode like sy-ucomm.
  DATA: S_IP_OBJECT LIKE BORIDENT.
  DATA: WA_MANDT LIKE SYST-MANDT,
        WA_SYSID LIKE SYST-SYSID,
        WA_LOGSYS LIKE BORIDENT-LOGSYS.

  w_okcode = ok_code.
  clear ok_code.
  case w_okcode.
    when 'BACK' or 'CANC'.
      leave to screen 0.
    when 'EXIT'.
      leave program.
    when 'SAVE'.
      perform save_text.
** Modified by Haseeb Mohammad on 05-19-2006
      save_flag = 1.
**End of Modification
      leave to screen 0.
**      leave to screen '9100'.
*    leave screen.

*modification by 100565
    when 'LIST'.
      MOVE IT_ZSQM_NOTI_SUM_P-QMNUM TO S_IP_OBJECT-OBJKEY.
      S_IP_OBJECT-OBJTYPE = 'BUS2078'.
      WA_MANDT = SY-MANDT.
      WA_SYSID = SY-SYSID.
      CONCATENATE WA_SYSID WA_MANDT INTO WA_LOGSYS.
      S_IP_OBJECT-LOGSYS = WA_LOGSYS.

*      DATA lo_attachment TYPE REF TO cl_gos_manager.
*     data lo_container TYPE REF TO cl_gui_custom_container.
*
*      CREATE OBJECT lo_attachment.
*      CALL METHOD lo_attachment->start_service_direct
*        EXPORTING
*          ip_service = 'VIEW_ATTA'
*          is_object     = S_IP_OBJECT
*          io_container = lo_container.

      CALL FUNCTION 'GOS_EXECUTE_SERVICE'
        EXPORTING
          IP_SERVICE             = 'VIEW_ATTA'
          IS_OBJECT              = S_IP_OBJECT
          IP_NO_COMMIT           = ''   "" KDM01 Option change
          IP_POPUP               = 'X'
          IP_RWMOD               = 'E'
*     IMPORTING
*       EP_EVENT               =
*       EP_STATUS              =
*       EP_ICON                =
       EXCEPTIONS
         EXECUTION_FAILED       = 1
         OTHERS                 = 2
                .

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CLEAR S_IP_OBJECT.
      CLEAR: WA_MANDT, WA_LOGSYS, WA_SYSID.
    when 'CREA'.
      MOVE IT_ZSQM_NOTI_SUM_P-QMNUM TO S_IP_OBJECT-OBJKEY.
      S_IP_OBJECT-OBJTYPE = 'BUS2078'.
*      S_IP_OBJECT-OBJTYPE = 'BUS2007'.
      WA_MANDT = SY-MANDT.
      WA_SYSID = SY-SYSID.
      CONCATENATE WA_SYSID WA_MANDT INTO WA_LOGSYS.
      S_IP_OBJECT-LOGSYS = WA_LOGSYS.

*      DATA lo_attachment TYPE REF TO cl_gos_document_service.
*
*      CREATE OBJECT lo_attachment.
*      CALL METHOD lo_attachment->create_attachment
*        EXPORTING
*          is_object     = S_IP_OBJECT.
**        IMPORTING
**          ep_attachment = lp_attachment.

      CALL FUNCTION 'GOS_EXECUTE_SERVICE'
        EXPORTING
*        IP_SERVICE             = 'CREATE_ATTA'
          IP_SERVICE             = 'PCATTA_CREA'
          IS_OBJECT              = S_IP_OBJECT
          IP_NO_COMMIT           = ''   "" KDM01 Option change
          IP_POPUP               = 'X'
          IP_RWMOD               = 'E'
*     IMPORTING
*       EP_EVENT               =
*       EP_STATUS              =
*       EP_ICON                =
       EXCEPTIONS
         EXECUTION_FAILED       = 1
         OTHERS                 = 2
                .

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
      CLEAR S_IP_OBJECT.
      CLEAR: WA_MANDT, WA_LOGSYS, WA_SYSID.

* end modification
    when 'PRINT'.
      PERFORM  PRINT_FORM.
  endcase.

ENDMODULE.                 " USER_COMMAND_9200  INPUT
