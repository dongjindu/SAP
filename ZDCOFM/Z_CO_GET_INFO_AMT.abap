FUNCTION z_co_get_info_amt.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(EKORG) TYPE  EKORG
*"     REFERENCE(MATNR) TYPE  MATNR
*"     REFERENCE(LIFNR) TYPE  LIFNR
*"     REFERENCE(INFNR) TYPE  INFNR
*"     REFERENCE(VALID_DATE) TYPE  DATUM
*"  EXPORTING
*"     REFERENCE(EXP_PREIS) LIKE  CKKALKTAB-GPREIS
*"     REFERENCE(EXP_WAERS) LIKE  CKIBEW-OWAER
*"     REFERENCE(EXP_PEINH) LIKE  CKKALKTAB-PEINH
*"     REFERENCE(EXP_KMEIN) TYPE  KMEIN
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
  DATA: l_knumh TYPE knumh.

  DATA: BEGIN OF lt_cond OCCURS 0,
            kschl      LIKE konp-kschl,
            kzust      LIKE konh-kzust,

            kbetr      LIKE konp-kbetr,

            konwa      LIKE konp-konwa,  "Currency
            kpein      LIKE konp-kpein,  "Unit
            kmein      LIKE konp-kmein,  "UoM
            kumza      LIKE konp-kumza,
            kumne      LIKE konp-kumne,
            meins      LIKE konp-meins,
         END OF lt_cond.

  DATA : $kpein TYPE kpein,
         $kmein TYPE kmein,
         $konwa TYPE konwa.


  SELECT SINGLE knumh
    INTO l_knumh
    FROM a018 AS a
   WHERE a~kappl = 'M'                " Purchasing
     AND a~kschl = 'PB00'             " ZTIR = PB00
     AND a~lifnr = lifnr
     AND a~matnr = matnr
     AND a~ekorg = ekorg
     AND a~esokz = '0'                " Standard
     AND a~datab =< valid_date
     AND a~datbi >= valid_date.

  IF sy-subrc <> 0.
    RAISE NOT_FOUND.
  ELSE.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_cond
      FROM konh INNER JOIN konp
                   ON konh~knumh = konp~knumh
      WHERE konp~knumh  = l_knumh
*       AND konp~loevm_ko = ' '             "deletion
        AND (
             konp~kschl = 'PB00' OR
             konp~kschl = 'ZTIR'
             ) .

    CLEAR: EXP_PREIS.
    LOOP AT lt_cond.

      CASE lt_cond-kschl.
        WHEN 'PB00'.
          $kpein = lt_cond-kpein.
          $kmein = lt_cond-kmein.
          $konwa = lt_cond-konwa.
        WHEN 'ZTIR'.
      ENDCASE.

      EXP_PREIS = EXP_PREIS + lt_cond-kbetr.
    ENDLOOP.

    exp_peinh = $kpein.
    exp_kmein = $kmein.

    EXP_WAERS = $konwa.

** unit conversion; not required during cost estimates.
*    DATA: l_output  TYPE p DECIMALS 4,
*          l_outunit type meins.
*
*    PERFORM unit_converion USING  matnr
*                                  $kpein        "p_input
*                                  $kmein        "p_unit_in
*                                  meins         "p_unit_out
*                        CHANGING  l_output.
*
*    exp_preis = menge *
*             ( exp_preis / l_output ).

  ENDIF.

ENDFUNCTION.
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
FORM unit_converion USING    p_matnr
                             p_input
                             p_unit_in
                             p_unit_out
                    CHANGING p_output.


  IF p_unit_in = p_unit_out.
    p_output = p_input.
  ELSE.
    CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
         EXPORTING
              input                = p_input
              unit_in              = p_unit_in
              unit_out             = p_unit_out
         IMPORTING
              output               = p_output
         EXCEPTIONS
              conversion_not_found = 1
              division_by_zero     = 2
              input_invalid        = 3
              output_invalid       = 4
              overflow             = 5
              type_invalid         = 6
              units_missing        = 7
              unit_in_not_found    = 8
              unit_out_not_found   = 9
              OTHERS               = 10.
*    IF sy-subrc <> 0.
*      BREAK-POINT.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.


    IF sy-subrc <> 0.
*           ALT UoM
      DATA : l_umrez_f TYPE umrez,
             l_umrez_t TYPE umrez.

      CLEAR : l_umrez_f,l_umrez_t.

      SELECT SINGLE umrez umren INTO :
                (l_umrez_f, l_umrez_t) FROM marm
               WHERE matnr = p_matnr
               AND meinh = p_unit_in.

*                l_umrez_t FROM marm
*               WHERE matnr = gt_out-matnr
*               AND meinh = p_unit_out.

      IF l_umrez_f <> 0 AND  l_umrez_t <> 0.
        p_output = p_input * ( l_umrez_f / l_umrez_t ).
*        p_input = p_output.
*        p_unit_in = p_unit_out.
      ELSE.
* error
        p_output = 1.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    " UNIT_CONVERION
