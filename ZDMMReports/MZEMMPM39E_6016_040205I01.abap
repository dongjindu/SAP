*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM39E_6008I01                                         *
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
      CASE sy-dynnr.
        WHEN 0100.
          LEAVE PROGRAM.
        WHEN 0110.
          CLEAR: ltak-refnr, io_cmatnr, io_cqty, io_sqty.
          w_top_line = '1'.
          LEAVE TO SCREEN 0100.
        WHEN 0120.
          CLEAR: ltak-refnr, io_cmatnr, io_cqty, io_sqty.
*          LEAVE TO SCREEN 0100.
          LEAVE TO SCREEN 0110.
        WHEN 0130.
          CLEAR: ltak-refnr, io_cmatnr, io_cqty, io_sqty.
          LEAVE TO SCREEN 0110.
      ENDCASE.
*    WHEN 'CLEA'.
*      CASE sy-dynnr.
*        WHEN 0100.
*          CLEAR: ltak-refnr.  "Group
*          break hakchin.
*      ENDCASE.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  scrolling_in_stoploop  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE scrolling_in_steploop INPUT.
  CHECK ( save_ok_code = 'P++'       OR
          save_ok_code = 'P+'        OR
          save_ok_code = 'P-'        OR
          save_ok_code = 'P--'       OR
          save_ok_code = 'NEXT_LINE' OR
          save_ok_code = 'PREV_LINE' ).

  PERFORM scrolling_in_steploop USING save_ok_code.
ENDMODULE.
*---------------------------------------------------------------------*
*       MODULE set_W_loopc INPUT                                      *
*---------------------------------------------------------------------*
MODULE set_w_loopc INPUT.
*  W_loopc = sy-loopc.   "When Using step loop
  w_loopc = 1.   "Developer fixed it to 1.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ente  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ente INPUT.
  CHECK save_ok_code = 'ENTE'.

  CASE sy-dynnr.
    WHEN 0120.
**** Material Check
      IF wa_ltxx-matnr <> io_cmatnr.
        PERFORM call_message_screen
              USING 'ZMMM'    "msgid
                    'E'       "lang
                    '999'     "msgno
                    'Material is different !'               "msgv1
                    space                                   "msgv2
                    space                                   "msgv3
                    space.                                  "msgv4.
*        CLEAR: io_cmatnr, io_cqty, io_sqty.
*        "Clear Compared matnr, Qty, scanned quantity.
*        LEAVE TO SCREEN 0110.
        LEAVE TO SCREEN 0120.
      ENDIF.

**** Quantity Check
      IF io_cqty IS INITIAL.
        PERFORM call_message_screen
              USING 'ZMMM'    "msgid
                    'E'       "lang
                    '999'     "msgno
                    'Quantity is empty !'                   "msgv1
                    space                                   "msgv2
                    space                                   "msgv3
                    space.                                  "msgv4.
*        CLEAR: io_cmatnr, io_cqty, io_sqty.
*        "Clear Compared matnr, Qty, scanned quantity.
*        LEAVE TO SCREEN 0110.
        LEAVE TO SCREEN 0120.
      ENDIF.

**** Quantity Add for Check
*Scanned Qty = Scanned Qty + Compared Qty.
      io_sqty = io_sqty + io_cqty.
      CLEAR: io_cmatnr, io_cqty.
  ENDCASE.
ENDMODULE.                 " next  INPUT
*&---------------------------------------------------------------------*
*&      Module  clea  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE clea INPUT.
  CHECK save_ok_code = 'CLEA'.
  CASE sy-dynnr.
    WHEN 0100.
      CLEAR: ltak-refnr.  "Group
    WHEN 0120.
      CLEAR: io_cmatnr, io_cqty.
  ENDCASE.
ENDMODULE.                 " clea  INPUT
*&---------------------------------------------------------------------*
*&      Module  call  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE call INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CHECK save_ok_code = 'CALL'.
  CASE sy-dynnr.
    WHEN 0100.
      CHECK NOT ltak-refnr IS INITIAL.
      PERFORM check_refnr USING ltak-refnr. "Check Group
      IF sy-subrc <> 0.
        MESSAGE s172(l3).
        EXIT.
      ENDIF.
      PERFORM get_it_ltxx TABLES it_ltxx
                    USING  ltak-refnr.  "Group : e.g.'F01'

*If No transfer orders found for groups selected
      IF it_ltxx IS INITIAL.
        MESSAGE s172(l3).
        EXIT.
      ELSE.
        CALL SCREEN 0110.
      ENDIF.

    WHEN 0110.
*/ /nLT25 Transaction
*      SUBMIT rllt2500 WITH t5_lgnum     = ltak-lgnum "Warehouse number
*                      WITH t5_refnr-low = ltak-refnr "Group
*                      WITH t5_offta     = 'X'  "CALL open TO items
*                      WITH t5_quita     = space"CALL confirmed TO items
*                      WITH t5_CEACa     = space"All TO items
*                      WITH t5_direk     = 'X'  "Direct
*                      WITH t5_entna     = 'X'  "Pick
*                      WITH t5_aufte     = 'X'  "Allocation
*                      WITH gruig        = 'X'
*                      "New page for control break
*                      WITH dunkl        = 'D'  "D:Background Processing
**                      VIA SELECTION-SCREEN
*                      AND RETURN.

*/ Begin of BDC Processing
* Application Doc No.
      PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                       nro_object    "NRO Object
                              CHANGING w_zdocno.     "App. Doc. No.
      COMMIT WORK.
      CLEAR: w_error_idx.   "Error Index
      LOOP AT it_ltxx INTO wa_ltxx.
* Confirm Transfer Order
        PERFORM bdc_processing_lt12 TABLES   it_bdcmsgcoll
                                    USING    w_zdocno
                                             w_confirmation
                                    CHANGING w_subrc.
        IF w_subrc = 0.
          wa_ltxx-executed_flg = 'X'.
          MODIFY it_ltxx FROM wa_ltxx
*                         INDEX w_top_line
                         TRANSPORTING executed_flg.
        ELSE.
          w_error_idx = w_error_idx + 1.
        ENDIF.
      ENDLOOP.
*/ End of BDC Processing
      IF w_error_idx >= 1.
        PERFORM call_message_screen
        USING 'ZMMM'    "msgid
              'E'       "lang
              '999'     "msgno
              'Some Error Occurred !'                       "msgv1
              'Check Log at /nZMMR99.'                      "msgv2
              space                                         "msgv3
              space.                                        "msgv4.
      ENDIF.
      CLEAR: ltak-refnr.  " Clear Group
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " call  INPUT
*&---------------------------------------------------------------------*
*&      Module  ceac  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ceac INPUT.
  CHECK save_ok_code = 'CEAC'.
  CASE sy-dynnr.
    WHEN 0100.
      CHECK NOT ltak-refnr IS INITIAL.
      PERFORM check_refnr USING ltak-refnr. "Check Group
      IF sy-subrc <> 0.
        MESSAGE s172(l3).
        EXIT.
      ENDIF.


* For Tcode 'ZMME89', We need Source storage type as Criteria.
      IF sy-tcode = 'ZMME89'.  "Transfer Orders for each group(Transfer)
        CHECK NOT ltap-vltyp IS INITIAL.
        PERFORM check_vltyp USING ltap-vltyp. "Check Source storage type
        IF sy-subrc <> 0.
         MESSAGE s999(zmmm) WITH 'There is no Source storage type'(001).
          EXIT.
        ENDIF.
      ENDIF.

*Get IT_LTXX
      PERFORM get_it_ltxx TABLES it_ltxx
                    USING  ltak-refnr.
*                      USING  'F01'.
*If No transfer orders found for groups selected
      IF it_ltxx IS INITIAL.
        MESSAGE s172(l3).
        EXIT.
      ELSE.
        CALL SCREEN 0110.
      ENDIF.

    WHEN 0110.
      CALL SCREEN 0120.

    WHEN 0120.

*/ Begin of Commented by Hakchin(20040119)
**** Material Check
*      IF wa_ltxx-matnr <> io_cmatnr.
*        PERFORM call_message_screen
*              USING 'ZMMM'    "msgid
*                    'E'       "lang
*                    '999'     "msgno
*                    'Material is different !'               "msgv1
*                    space                                   "msgv2
*                    space                                   "msgv3
*                    space.                                  "msgv4.
*        CLEAR: io_cmatnr, io_cqty, io_sqty.
*        "Clear Compared matnr, Qty, scanned quantity.
*        LEAVE TO SCREEN 0110.
*      ENDIF.
*
**** Quantity Check
*      IF io_cqty IS INITIAL.
*        PERFORM call_message_screen
*              USING 'ZMMM'    "msgid
*                    'E'       "lang
*                    '999'     "msgno
*                    'Quantity is empty !'                   "msgv1
*                    space                                   "msgv2
*                    space                                   "msgv3
*                    space.                                  "msgv4.
*        CLEAR: io_cmatnr, io_cqty, io_sqty.
*        "Clear Compared matnr, Qty, scanned quantity.
*        LEAVE TO SCREEN 0110.
*      ENDIF.
*
***** Quantity Add for Check
**Scanned Qty = Scanned Qty + Compared Qty.
*      io_sqty = io_sqty + io_cqty.
*      CLEAR: io_cqty.
*/ End of Commented by Hakchin(20040119)

*/ Begin of Added by Hakchin (20040119)
*** Material Check
      IF NOT io_cmatnr IS INITIAL.
        PERFORM call_message_screen
              USING 'ZMMM'    "msgid
                    'E'       "lang
                    '999'     "msgno
                    'Material should be empty !'            "msgv1
                    space                                   "msgv2
                    space                                   "msgv3
                    space.                                  "msgv4.
        LEAVE TO SCREEN 0120.
      ENDIF.

*** Quantity Check
      IF NOT io_cqty IS INITIAL.
        PERFORM call_message_screen
              USING 'ZMMM'    "msgid
                    'E'       "lang
                    '999'     "msgno
                    'Quantity should be empty !'            "msgv1
                    space                                   "msgv2
                    space                                   "msgv3
                    space.                                  "msgv4.
        LEAVE TO SCREEN 0120.
      ENDIF.

      IF io_sqty <> wa_ltxx-nsola.
        PERFORM call_message_screen
              USING 'ZMMM'    "msgid
                    'E'       "lang
                    '999'     "msgno
                    'Scanned Quantity is different !'       "msgv1
                    space                                   "msgv2
                    space                                   "msgv3
                    space.                                  "msgv4.
        LEAVE TO SCREEN 0120.
      ENDIF.


*/ End of Added by Hakchin (20040119)



*/ Begin of Added by Hakchin (20040119)
*/ Begin of BDC Processing
* Application Doc No.
      PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                       nro_object    "NRO Object
                              CHANGING w_zdocno.     "App. Doc. No.
      COMMIT WORK.

* Confirm Transfer Order
      PERFORM bdc_processing_lt12 TABLES   it_bdcmsgcoll
                                  USING    w_zdocno
                                           w_confirmation
                                  CHANGING w_subrc.
      IF w_subrc = 0.
        wa_ltxx-executed_flg = 'X'.
        MODIFY it_ltxx FROM wa_ltxx
                       INDEX w_top_line
                       TRANSPORTING executed_flg.
        LEAVE TO SCREEN 0110.
      ELSE.
        READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                          WITH KEY msgtyp = 'E'.

        PERFORM call_message_screen
              USING wa_bdcmsgcoll-msgid     "msgid
                    wa_bdcmsgcoll-msgspra   "lang
                    wa_bdcmsgcoll-msgnr     "msgno
                    wa_bdcmsgcoll-msgv1                     "msgv1
                    wa_bdcmsgcoll-msgv2                     "msgv2
                    wa_bdcmsgcoll-msgv3                     "msgv3
                    wa_bdcmsgcoll-msgv4.                    "msgv4.
*/ Begin of commented by Hakchin(20040119)
*        CLEAR: io_cmatnr, io_cqty, io_sqty.
*        "Clear Compared matnr, Qty, scanned quantity.
*        LEAVE TO SCREEN 0110.
*/ End of added by Hakchin(20040119)
*/ Begin of commented by Hakchin(20040119)
        LEAVE TO SCREEN 0120.
*/ End of added by Hakchin(20040119)
      ENDIF.
*/ End of BDC Processing
*/ End of Added by Hakchin (20040119)


*/ Begin of Commented by Hakchin (20040119)
** Go to Screen 0130
*      SET SCREEN 0130.
*    WHEN 0130.
**/ Begin of BDC Processing
** Application Doc No.
*      PERFORM number_get_next USING    nro_nr_09     "NRO Interval
*                                       nro_object    "NRO Object
*                              CHANGING w_zdocno.     "App. Doc. No.
*      COMMIT WORK.
*
** Confirm Transfer Order
*      PERFORM bdc_processing_lt12 TABLES   it_bdcmsgcoll
*                                  USING    w_zdocno
*                                           w_confirmation
*                                  CHANGING w_subrc.
*      IF w_subrc = 0.
*        wa_ltxx-executed_flg = 'X'.
*        MODIFY it_ltxx FROM wa_ltxx
*                       INDEX w_top_line
*                       TRANSPORTING executed_flg.
*        LEAVE TO SCREEN 0110.
*      ELSE.
*        PERFORM call_message_screen
*              USING 'ZMMM'    "msgid
*                    'E'       "lang
*                    '999'     "msgno
*                    'Error occurred !'                      "msgv1
*                    space                                   "msgv2
*                    space                                   "msgv3
*                    space.                                  "msgv4.
*        CLEAR: io_cmatnr, io_cqty, io_sqty.
*        "Clear Compared matnr, Qty, scanned quantity.
*        LEAVE TO SCREEN 0110.
*      ENDIF.
**/ End of BDC Processing

*/ End of Commented by Hakchin (20040119)

  ENDCASE.
ENDMODULE.                 " ceac  INPUT
