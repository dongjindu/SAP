*----------------------------------------------------------------------*
***INCLUDE ZFIR0062I01 .
*----------------------------------------------------------------------*
  MODULE exit_command INPUT.

    MESSAGE s169.
    LEAVE TO SCREEN 0.

  ENDMODULE.                    "exit_command INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_RECEIVING  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE check_receiving INPUT.

* Check HR Master
    CASE p_bukrs.
      WHEN '5100'.
        SELECT SINGLE pernr INTO zfit0090-pernr
                            FROM pa0000
                            WHERE pernr =  zfit0090-pernr
                            AND   endda >= sy-datum
                            AND   begda <= sy-datum
                            AND   stat2 =  '3'.
        IF sy-subrc <> 0.
          MESSAGE e177 WITH zfit0090-pernr.
        ENDIF.
    ENDCASE.

    SELECT SINGLE * FROM zfit0090
                    WHERE bukrs = p_bukrs
                    AND   pernr = zfit0090-pernr
                    AND   rcdat = zfit0090-rcdat
                    AND   statu = 'U'.
    IF sy-subrc = 0.
      CLEAR g_okcode.
      MESSAGE e179 WITH zfit0090-pernr zfit0090-rcdat.
    ENDIF.

  ENDMODULE.                 " CHECK_RECEIVING  INPUT
*----------------------------------------------------------------------*
*  MODULE user_command_9100 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  MODULE user_command_9100 INPUT.

    DATA: l_statu LIKE zfit0090-statu,
          l_rcdat LIKE zfit0090-rcdat.
    DATA: l_text(80) TYPE c.
    DATA: l_answer   TYPE c.


    CASE g_okcode.
      WHEN 'SAVE'.
* Data already sent to PDA
        SELECT SINGLE rcdat statu INTO (l_rcdat, l_statu)
                                  FROM zfit0090
                                  WHERE bukrs = p_bukrs
                                  AND   pernr = zfit0090-pernr
                                  AND   statu IN ('L', 'D').
        IF sy-subrc = 0.
          CASE l_statu.
            WHEN 'L'.
              CONCATENATE: 'Receiving data('
                           l_rcdat ') exist.' INTO l_text,
                           l_text 'Do you want to recreate?'
                           INTO l_text SEPARATED BY space.
            WHEN 'D'.
              CONCATENATE 'Receiving data('
                          l_rcdat ') already sent to PDA.' INTO l_text.
              CONCATENATE l_text 'Do you want to recreate?' INTO l_text
                          SEPARATED BY space.
          ENDCASE.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Rerun?'
              text_question         = l_text
              text_button_1         = 'Yes'
              text_button_2         = 'No'
              default_button        = '2'
              display_cancel_button = ' '
            IMPORTING
              answer                = l_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          CASE l_answer.
            WHEN '1'.
            WHEN '2'.
              MESSAGE s169.
              CLEAR g_okcode.
          ENDCASE.
        ENDIF.

        IF g_okcode = 'SAVE'.
          PERFORM modify_zfit0090 USING l_rcdat.
          LEAVE TO SCREEN 0.
        ENDIF.
    ENDCASE.

  ENDMODULE.                 " USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INSPECTION  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE check_inspection INPUT.

* Check HR Master
    CASE p_bukrs.
      WHEN '5100'.
        SELECT SINGLE pernr INTO zfit0091-pernr
                            FROM pa0000
                            WHERE pernr =  zfit0091-pernr
                            AND   endda >= sy-datum
                            AND   begda <= sy-datum
                            AND   stat2 =  '3'.
        IF sy-subrc <> 0.
          MESSAGE e177 WITH zfit0091-pernr.
        ENDIF.
    ENDCASE.

    SELECT SINGLE * FROM zfit0091
                    WHERE bukrs = p_bukrs
                    AND   pernr = zfit0091-pernr
                    AND   isdat = zfit0091-isdat
                    AND   statu = 'U'.
    IF sy-subrc = 0.
      CLEAR g_okcode.
      MESSAGE e179 WITH zfit0091-pernr zfit0091-isdat.
    ENDIF.

  ENDMODULE.                 " CHECK_INSPECTION  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE user_command_9200 INPUT.

    DATA: l_isdat LIKE zfit0091-isdat.

    CASE g_okcode.
      WHEN 'SAVE'.
* Data already sent to PDA
        SELECT SINGLE isdat statu INTO (l_isdat, l_statu)
                                  FROM zfit0091
                                  WHERE bukrs = p_bukrs
                                  AND   pernr = zfit0091-pernr
                                  AND   statu IN ('L', 'D').
        IF sy-subrc = 0.
          CASE l_statu.
            WHEN 'L'.
              CONCATENATE: 'Inspection data('
                           l_isdat ') exist.' INTO l_text,
                           l_text 'Do you want to recreate?'
                           INTO l_text SEPARATED BY space.
            WHEN 'D'.
              CONCATENATE 'Inspection data('
                          l_isdat ') already sent to PDA.' INTO l_text.
              CONCATENATE l_text 'Do you want to recreate?' INTO l_text
                          SEPARATED BY space.
          ENDCASE.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Rerun?'
              text_question         = l_text
              text_button_1         = 'Yes'
              text_button_2         = 'No'
              default_button        = '2'
              display_cancel_button = ' '
            IMPORTING
              answer                = l_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.
          CASE l_answer.
            WHEN '1'.
            WHEN '2'.
              MESSAGE s169.
              CLEAR g_okcode.
          ENDCASE.
        ENDIF.

        IF g_okcode = 'SAVE'.
          PERFORM modify_zfit0091 USING l_isdat.
          LEAVE TO SCREEN 0.
        ENDIF.
    ENDCASE.

  ENDMODULE.                 " USER_COMMAND_9200  INPUT
