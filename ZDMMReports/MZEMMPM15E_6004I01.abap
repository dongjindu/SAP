*----------------------------------------------------------------------*
*   INCLUDE MZEMMPM15E_6004I01                                         *
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
      CLEAR: lein-lenum,  "Storage unit
       lein-letyp,  "Storage unit type
       *ltap-nltyp,
       *ltap-nlber,
       *ltap-nlpla,
       t307t,
       t301t.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  crto  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE crto INPUT.
  save_ok_code = ok_code.
  CLEAR ok_code.
  CHECK save_ok_code = 'CRTO'.
  PERFORM check_lenum USING lein-lenum.     "Check Storage unit
  IF sy-subrc <> 0.
    MESSAGE e209(l3) WITH lein-lenum.
  ENDIF.

  PERFORM check_letyp USING lein-lgnum      "Warehouse number
                            lein-letyp.     "Check Storage unit type
  IF sy-subrc <> 0.
    MESSAGE e204(l3) WITH lein-letyp.
  ENDIF.

* BDC Processing
*Here we call transaction /nLT09(Create transfer order) and
*get BDC log messages in the internal table IT_bdcmsgcoll.

  DATA: lv_zdocno TYPE num10.     "Application Doc no.
  PERFORM number_get_next USING    nro_nr_09     "NRO Interval
                                   nro_object    "NRO Object
                          CHANGING lv_zdocno.    "NRO Next
  COMMIT WORK.

  PERFORM bdc_processing TABLES   it_bdcmsgcoll
                         USING    lv_zdocno
                         CHANGING w_subrc.

  IF w_subrc <> 0.
    READ TABLE it_bdcmsgcoll INTO wa_bdcmsgcoll
                             WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      MESSAGE ID wa_bdcmsgcoll-msgid
              TYPE wa_bdcmsgcoll-msgtyp
              NUMBER wa_bdcmsgcoll-msgnr
              WITH wa_bdcmsgcoll-msgv1 wa_bdcmsgcoll-msgv2
                   wa_bdcmsgcoll-msgv3 wa_bdcmsgcoll-msgv4.
    ELSE.
      MESSAGE e999(zmmm) WITH 'Problem exists! Correct data!'.
    ENDIF.
  ENDIF.
ENDMODULE.                 " crto  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_lenum  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_lenum INPUT.
  PERFORM check_lenum USING lein-lenum.     "Check Storage unit
  IF sy-subrc <> 0.
    MESSAGE e209(l3) WITH lein-lenum.
  ENDIF.
ENDMODULE.                 " check_lenum  INPUT
