*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM32E_6010I01                                         *
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
  CHECK NOT lips-kdmat IS INITIAL.  "Vendor mat. no.

* BDC Processing
* Making Inbound Deliveries item list which are not posted
  CLEAR: it_iditems_without_post.
  PERFORM iditems_without_post
                 TABLES it_iditems_without_post
                 USING lips-kdmat. "Vendor mat. no.

** Here we call transaction /nLT03 and BAPI of /nMIGO_GR
** (Create Transfer Order for Delivery Note & GR) and
** get BDC log messages in the internal table IT_bdcmsgcoll.
* App Doc No
  PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                   w_nro_object    "NRO Object
                          CHANGING w_zdocno.     "App Doc No
  COMMIT WORK.

  SORT it_iditems_without_post BY vbeln posnr.
  CLEAR: w_subrc.
  LOOP AT it_iditems_without_post
                   INTO wa_iditems_without_post.

    IF wa_iditems_without_post-kosta = 'A'.
      "until this time, Transfer Order not processed.
      "Picking status/Putaway status = 'A'.
* Generate TO (/nLT03)
      PERFORM bdc_processing_gto TABLES   it_bdcmsgcoll
                                 USING    w_zdocno
                                 CHANGING w_subrc.

      IF w_subrc = 0.
* Post(GR for Order (/nMIGO_GR) )
        PERFORM post USING w_zdocno.  "BAPI
*        CALL FUNCTION 'Z_FMM_6010_03'
*           DESTINATION 'NONE'
*             EXPORTING
*                  im_zdocno                = w_zdocno
*                  ims_iditems_without_post = wa_iditems_without_post.
*        CALL FUNCTION 'RFC_CONNECTION_CLOSE'
*          EXPORTING
*            destination                = 'NONE'
**       EXCEPTIONS
**         DESTINATION_NOT_OPEN       = 1
**         OTHERS                     = 2
*                  .
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
* Post(GR for Order (/nMIGO_GR) )
      PERFORM post USING w_zdocno.
*      CALL FUNCTION 'Z_FMM_6010_03'
*           DESTINATION 'NONE'
*           EXPORTING
*                im_zdocno                = w_zdocno
*                ims_iditems_without_post = wa_iditems_without_post.
*
*      CALL FUNCTION 'RFC_CONNECTION_CLOSE'
*        EXPORTING
*          destination                = 'NONE'
**       EXCEPTIONS
**         DESTINATION_NOT_OPEN       = 1
**         OTHERS                     = 2
*                .


    ENDIF.
  ENDLOOP.
  IF w_subrc <> 0.
    PERFORM call_message_screen
             USING 'ZMMM'      "msgid
                   'E'       "lang
                   '999'     "msgno
                   'Failure !'                              "msgv1
                   space                                    "msgv2
                   space                                    "msgv3
                   space.                                   "msgv4.
    EXIT.
  ENDIF.

  CLEAR:w_qty_message.    "'EQ': Same, 'NE': Not Same
  IF it_iditems_without_post IS INITIAL.
* If there is no related inbound delivery.
    PERFORM call_message_screen
             USING 'LF'      "msgid
                   'E'       "lang
                   '278'     "msgno
                   space                                    "msgv1
                   space                                    "msgv2
                   space                                    "msgv3
                   space.                                   "msgv4.
    "No delivery found for selection criteria
  ELSE.
    CLEAR: wa_bapiret2.
    READ TABLE it_bapiret2 INTO wa_bapiret2 WITH KEY type = 'E'.
    IF sy-subrc <> 0.  "Error Not Occurred !
      LOOP AT it_iditems_without_post
                       INTO wa_iditems_without_post.

        PERFORM qty_comparison
                 USING    wa_iditems_without_post-vgbel   "ebeln
                          wa_iditems_without_post-vgpos   "ebelp
                 CHANGING w_qty_message.
        IF w_qty_message = 'NE'.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF w_qty_message = 'NE'.
        PERFORM call_message_screen
                 USING 'ZMMM'      "msgid
                       'E'       "lang
                       '999'     "msgno
                       'The ASN qty is greater than /'      "msgv1
                       'less than PO qty !'                 "msgv2
                       space                                "msgv3
                       space.                               "msgv4.

      ELSE.
        PERFORM call_message_screen
                 USING 'ZMMM'      "msgid
                       'E'       "lang
                       '999'     "msgno
                       'Success !'                          "msgv1
                       space                                "msgv2
                       space                                "msgv3
                       space.                               "msgv4.
      ENDIF.
    ELSE. "Error Occurred !
      PERFORM call_message_screen
               USING 'ZMMM'      "msgid
                     'E'       "lang
                     '999'     "msgno
                     'Failure !'                            "msgv1
                     space                                  "msgv2
                     space                                  "msgv3
                     space.                                 "msgv4.
    ENDIF.
  ENDIF.
  CLEAR: lips-kdmat.   "Vendor mat. no.
ENDMODULE.
