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
* Button Click Date & Time
  w_button_click_date = sy-datum.
  w_button_click_time = sy-uzeit.

* BDC Processing
* Making Inbound Deliveries item list which are not posted
  CLEAR: it_iditems_without_post.
  PERFORM iditems_without_post
                 TABLES it_iditems_without_post
                 USING lips-kdmat. "Vendor mat. no. (Unit Load)

** Here we call transaction /nLT03 and BAPI of /nMIGO_GR
** (Create Transfer Order for Delivery Note & GR) and
** get BDC log messages in the internal table IT_bdcmsgcoll.
* App Doc No
  PERFORM number_get_next USING    c_nro_nr_09     "NRO Interval
                                   w_nro_object    "NRO Object
                          CHANGING w_zdocno.     "App Doc No
  COMMIT WORK.

  SORT it_iditems_without_post BY vbeln posnr.
  "Sorted by Inbound Delivery, Item no.

*/Begin of Added by Hakchin(20030127)
  DELETE ADJACENT DUPLICATES FROM it_iditems_without_post
                             COMPARING vbeln.
*/End of Added by Hakchin(20030127)

  LOOP AT it_iditems_without_post
                   INTO wa_iditems_without_post.

    IF wa_iditems_without_post-kosta = 'A'.
      "until this time, Transfer Order not processed.
      "Picking status/Putaway status = 'A'.
* Generate TO (/nLT03)
      PERFORM bdc_processing_lt03_header
                                 TABLES   it_bdcmsgcoll
                                 USING    w_zdocno
                                 CHANGING w_subrc.
      IF w_subrc = 0.
* Post GR (/nVL32N)
* We use BDC instead of BAPI because of BAPI's no Doc-Flow Supporting.
* Reference BAPI: BAPI_GOODSMVT_CREATE

*/Begin of Changed by Hakchin(20040416)
* We use bdc_processing_mb01 instead of bdc_processing_vl32n
* because GR Posting Date Adjusting
        PERFORM bdc_processing_vl32n
                             TABLES   it_bdcmsgcoll
                             USING    w_zdocno
                             CHANGING w_subrc.
*        PERFORM bdc_processing_mb01
*                             TABLES   it_bdcmsgcoll
*                             USING    w_zdocno
*                             CHANGING w_subrc.
*/End of Changed by Hakchin(20040416)
      ELSE.
        EXIT.
      ENDIF.
    ELSE.
* Post GR (/nVL32N)
*/Begin of Changed by Hakchin(20040416)
* We use bdc_processing_mb01 instead of bdc_processing_vl32n
* because GR Posting Date Adjusting
      PERFORM bdc_processing_vl32n
                           TABLES   it_bdcmsgcoll
                           USING    w_zdocno
                           CHANGING w_subrc.
*      PERFORM bdc_processing_mb01
*                           TABLES   it_bdcmsgcoll
*                           USING    w_zdocno
*                           CHANGING w_subrc.
*/End of Changed by Hakchin(20040416)
    ENDIF.
  ENDLOOP.

*/ Log Message Processing
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
*/Begin of Deleted Code
*    CLEAR: wa_bapiret2.
*    READ TABLE it_bapiret2 INTO wa_bapiret2 WITH KEY type = 'E'.
*/End of Deleted Code
*/Begin of Added Code
    CLEAR: wa_bdcmsgcoll.
    READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll WITH KEY msgtyp = 'E'.
*/End of Added Code

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
*/Begin of Changed by Hakchin(20040510)(Need of CK KIM)
*        PERFORM call_message_screen
*                 USING 'ZMMM'      "msgid
*                       'E'       "lang
*                       '999'     "msgno
*                       'The ASN qty is greater than /'      "msgv1
*                       'less than PO qty !'                 "msgv2
*                       space                                "msgv3
*                       space.                               "msgv4.

        PERFORM call_message_screen
                 USING 'ZMMM'      "msgid
                       'E'       "lang
                       '999'     "msgno
                       'Success !'                          "msgv1
                       space                                "msgv2
                       space                                "msgv3
                       space.                               "msgv4.
*/End of Changed by Hakchin(20040510)(Need of CK KIM)
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
