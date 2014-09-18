*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM39E_6016I01                                         *
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


*/Begin of Added by Hakchin(200402011)
      IF wa_ltxx-nsolm < io_sqty.  "Scanned Qty
        PERFORM call_message_screen
              USING 'ZMMM'    "msgid
                    'E'       "lang
                    '999'     "msgno
                    'Scanned Quantity is exceeded !'        "msgv1
                    space                                   "msgv2
                    space                                   "msgv3
                    space.                                  "msgv4.
        LEAVE TO SCREEN 0120.
      ENDIF.
*/End of Added by Hakchin(200402011)

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
* For inactivate 'CEAC'.
  w_call_ceac_deletion_flg = 'CEAC'.

  CASE sy-dynnr.
    WHEN 0100.
*/Begin of Added by Hakchin(20040209)
      IF ltak-refnr IS INITIAL.
        MESSAGE s999(zmmm) WITH 'Group is empty!'(003).
        EXIT.
      ENDIF.
*/End of Added by Hakchin(20040209)
*/Begin of Commented by Hakchin(20040209)
*      CHECK NOT ltak-refnr IS INITIAL.
*/End of Commented by Hakchin(20040209)
      PERFORM check_refnr USING ltak-refnr. "Check Group
      IF sy-subrc <> 0.
        MESSAGE s172(l3).
        EXIT.
      ENDIF.

* For Tcode 'ZMME89', We need Source storage type as Criteria.
      IF sy-tcode = 'ZMME89'.  "Transfer Orders for each group(Transfer)
*/Begin of Added by Hakchin(20040209)
        IF ltap-vltyp IS INITIAL.
*          MESSAGE s999(zmmm) WITH 'Source stor.ty. is empty!'(004).
*          EXIT.
          ltap-vltyp = '422'. "Needed by Sunil(20040210)
        ENDIF.
*/End of Added by Hakchin(20040209)
*/Begin of Commented by Hakchin(20040209)
*        CHECK NOT ltap-vltyp IS INITIAL.
*/End of Commented by Hakchin(20040209)
        PERFORM check_vltyp USING ltap-vltyp. "Check Source storage type
        IF sy-subrc <> 0.
         MESSAGE s999(zmmm) WITH 'There is no Source storage type'(001).
          EXIT.
        ENDIF.
      ENDIF.

*/Begin of Added by Hakchin(20040211)  (For tese use)
      IF sy-tcode = 'ZMME88S'."T/O for each group(Pick) with Src.Stor.
        IF ltap-vltyp IS INITIAL.
          MESSAGE s999(zmmm) WITH 'Source stor.ty. is empty!'(004).
          EXIT.
        ELSEIF ltap-vltyp = '422'. "Needed by Sunil(20040211)
          MESSAGE s999(zmmm)
              WITH 'Source stor.ty. 422 is not allowed!'(005).
          EXIT.
        ENDIF.
        PERFORM check_vltyp USING ltap-vltyp. "Check Source storage type
        IF sy-subrc <> 0.
         MESSAGE s999(zmmm) WITH 'There is no Source storage type'(001).
          EXIT.
        ENDIF.
      ENDIF.
*/End of Added by Hakchin(20040211)

*Get IT_LTXX
      PERFORM get_it_ltxx TABLES it_ltxx
                    USING  ltak-refnr.  "Group : e.g.'F01'

*If No transfer orders
      IF it_ltxx IS INITIAL.
*        MESSAGE s172(l3).
        MESSAGE s999(zmmm) WITH
          'There is no data with selection conditions'(002).
        EXIT.
      ELSE.
        CALL SCREEN 0110.
      ENDIF.

    WHEN 0110.

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
* For inactivate 'CALL'.
  w_call_ceac_deletion_flg = 'CALL'.
  CASE sy-dynnr.
    WHEN 0100.
*/Begin of Added by Hakchin(20040209)
      IF ltak-refnr IS INITIAL.
        MESSAGE s999(zmmm) WITH 'Group is empty!'(003).
        EXIT.
      ENDIF.
*/End of Added by Hakchin(20040209)
*/Begin of Commented by Hakchin(20040209)
*      CHECK NOT ltak-refnr IS INITIAL.
*/End of Commented by Hakchin(20040209)
      PERFORM check_refnr USING ltak-refnr. "Check Group
      IF sy-subrc <> 0.
        MESSAGE s172(l3).
        EXIT.
      ENDIF.

* For Tcode 'ZMME89', We need Source storage type as Criteria.
      IF sy-tcode = 'ZMME89'.  "Transfer Orders for each group(Transfer)
*/Begin of Added by Hakchin(20040209)
        IF ltap-vltyp IS INITIAL.
*          MESSAGE s999(zmmm) WITH 'Source stor.ty. is empty!'(004).
*          EXIT.
          ltap-vltyp = '422'. "Needed by Sunil(20040210)
        ENDIF.
*/End of Added by Hakchin(20040209)
*/Begin of Commented by Hakchin(20040209)
*        CHECK NOT ltap-vltyp IS INITIAL.
*/End of Commented by Hakchin(20040209)
        PERFORM check_vltyp USING ltap-vltyp. "Check Source storage type
        IF sy-subrc <> 0.
         MESSAGE s999(zmmm) WITH 'There is no Source storage type'(001).
          EXIT.
        ENDIF.
      ENDIF.

*/Begin of Added by Hakchin(20040211)  (For tese use)
      IF sy-tcode = 'ZMME88S'."T/O for each group(Pick) with Src.Stor.
        IF ltap-vltyp IS INITIAL.
          MESSAGE s999(zmmm) WITH 'Source stor.ty. is empty!'(004).
          EXIT.
        ELSEIF ltap-vltyp = '422'. "Needed by Sunil(20040211)
          MESSAGE s999(zmmm)
              WITH 'Source stor.ty. 422 is not allowed!'(005).
          EXIT.
        ENDIF.
        PERFORM check_vltyp USING ltap-vltyp. "Check Source storage type
        IF sy-subrc <> 0.
         MESSAGE s999(zmmm) WITH 'There is no Source storage type'(001).
          EXIT.
        ENDIF.
      ENDIF.
*/End of Added by Hakchin(20040211)




*Get IT_LTXX
      PERFORM get_it_ltxx TABLES it_ltxx
                    USING  ltak-refnr.
*                      USING  'F01'.

*If No transfer orders found
      IF it_ltxx IS INITIAL.
*        MESSAGE s172(l3).
        MESSAGE s999(zmmm) WITH
          'There is no data with selection conditions'(002).
        EXIT.
      ELSE.
        CALL SCREEN 0110.
      ENDIF.

    WHEN 0110.
      CALL SCREEN 0120.

    WHEN 0120.


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

      IF io_sqty <> wa_ltxx-nsolm.
        PERFORM call_message_screen
              USING 'ZMMM'    "msgid
                    'E'       "lang
                    '999'     "msgno
                    'Scanned Quantity is different !'       "msgv1
                    space                                   "msgv2
                    space                                   "msgv3
                    space.                                  "msgv4
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
        CLEAR: io_sqty.  "Scanned Qty.
        wa_ltxx-executed_flg = 'X'.
        MODIFY it_ltxx FROM wa_ltxx
                       INDEX w_top_line
                       TRANSPORTING executed_flg.

*/Begin of Added by Hakchin(20040205)
        it_ltxx_result = it_ltxx.
        DELETE it_ltxx INDEX w_top_line.
*/End of Added by Hakchin(20040205)

*/Begin of Commented by Hakchin(20040205)
*        LEAVE TO SCREEN 0110.
*/End of Commented by Hakchin(20040205)

*/Begin of Added by Hakchin(20040205)
        DATA: lv_lines TYPE i.
        DATA: lv_top_line LIKE w_top_line.
        DATA: lv_it_ltxx_idx TYPE i.
        DATA: ls_ltxx LIKE LINE OF it_ltxx.
        DESCRIBE TABLE it_ltxx LINES lv_lines.

        IF it_ltxx IS INITIAL.
          LEAVE TO SCREEN 0100. "Go to First Screen
        ELSE.
          IF lv_lines < w_top_line.
            LOOP AT it_ltxx INTO ls_ltxx
                  FROM 1
                  TO lv_lines.
              lv_it_ltxx_idx = lv_it_ltxx_idx + 1.
              IF ls_ltxx-executed_flg = space.
                w_top_line = lv_it_ltxx_idx.
                EXIT.
              ENDIF.
            ENDLOOP.
          ENDIF.

          LEAVE TO SCREEN 0110.   "Go to Second Screen
        ENDIF.
*/End of Added by Hakchin(20040205)

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

        LEAVE TO SCREEN 0120.
      ENDIF.
*/ End of BDC Processing
*/ End of Added by Hakchin (20040119)

  ENDCASE.
ENDMODULE.                 " ceac  INPUT
