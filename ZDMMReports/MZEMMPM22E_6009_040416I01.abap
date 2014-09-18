*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM22E_6009I01                                         *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CASE save_ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      LEAVE PROGRAM.
    WHEN 'CLEA'.
      CLEAR: likp-lifex. "External delivery
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  next  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE next INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CHECK save_ok_code = 'NEXT'.
  CHECK NOT likp-lifex IS INITIAL.  "External ID
  TRANSLATE likp-lifex TO UPPER CASE. "To Uppercase
* BDC Processing
* Making Inbound Deliveries list which are not posted
  CLEAR: it_ideliveries_without_post.
  PERFORM ideliveries_without_post
                 TABLES it_ideliveries_without_post
                 USING likp-lifex. ""Ext.delivery
* Here we call transaction /nLM74(Select Delivery by Others) and
* get BDC log messages in the internal table IT_bdcmsgcoll.

* Application Doc No.
  PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING w_zdocno.     "App. Doc. No.
  COMMIT WORK.

  SORT it_ideliveries_without_post BY vbeln.

  LOOP AT it_ideliveries_without_post
                   INTO wa_ideliveries_without_post.
    IF wa_ideliveries_without_post-kostk = 'A'.
      "until this time, Transfer Order not processed.
      "Total goods movement status
      PERFORM bdc_processing_gto_post TABLES   it_bdcmsgcoll
                                      USING    w_zdocno
                                      CHANGING w_subrc.
    ELSE.
      PERFORM bdc_processing_post TABLES   it_bdcmsgcoll
                                  USING    w_zdocno
                                  CHANGING w_subrc.
    ENDIF.
  ENDLOOP.

  CLEAR: wa_lips, w_qty_message.
  IF it_ideliveries_without_post IS INITIAL.
* If there is no related inbound delivery.
    PERFORM call_message_screen_nodeliv.
    "No delivery found for selection criteria
  ELSE.
    IF w_subrc = 0.
      LOOP AT it_ideliveries_without_post
                       INTO wa_ideliveries_without_post.
        SELECT * INTO CORRESPONDING FIELDS OF wa_lips
          FROM lips
          WHERE vbeln = wa_ideliveries_without_post-vbeln.

          PERFORM qty_comparison USING    wa_lips-vgbel   "ebeln
                                          wa_lips-vgpos   "ebelp
                                 CHANGING w_qty_message.
          IF w_qty_message = 'NE'.
            EXIT.
          ENDIF.
        ENDSELECT.
        IF w_qty_message = 'NE'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF w_qty_message = 'NE'.
        PERFORM call_message_screen
        USING 'ZMMM'    "msgid
              'E'       "lang
              '999'     "msgno
              'The ASN qty is greater than /'               "msgv1
              'less than PO qty !'                          "msgv2
              space                                         "msgv3
              space.                                        "msgv4.
      ELSE.
        PERFORM call_message_screen
        USING 'ZMMM'    "msgid
              'E'       "lang
              '999'     "msgno
              'Success !'                                   "msgv1
              space                                         "msgv2
              space                                         "msgv3
              space.                                        "msgv4.
      ENDIF.
    ENDIF.
  ENDIF.
  CLEAR: likp-lifex.
ENDMODULE.
