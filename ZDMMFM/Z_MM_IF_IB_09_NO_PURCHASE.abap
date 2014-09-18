FUNCTION z_mm_if_ib_09_no_purchase.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     VALUE(E_RETURN) TYPE  ZMMS0053
*"  TABLES
*"      IT_BODY STRUCTURE  ZMMS_GS004
*"----------------------------------------------------------------------

  DATA : lt_zmmt_gs004 LIKE zmmt_gs004 OCCURS 0 WITH HEADER LINE.
  DATA : ls_zmmt_gs004 LIKE zmmt_gs004.
  DATA : BEGIN OF lt_log OCCURS 0,
          ret_code   TYPE c,
          ret_reason TYPE char20,
          result     TYPE c,
          count      TYPE i,
          msg        TYPE msg,
         END   OF lt_log .

*--------------------------------------
*retrun code  return description
*--------------------------------------
*J           	Module Parts
*A           	BOM error(SPEC)
*B           	BOM error(Parts Number)
*C           	BOM error(SUB Parts)
*D           	BOM error(installation diagram)
*E           	Team or Vender error
*F           	Oversea Developing Parts
*G           	Discontinued Parts
*H           	Mutual Exchange Parts
*I           	ETC
*--------------------------------------

  LOOP AT it_body.

    PERFORM alph_mat2_input CHANGING it_body-matnr.

*// Just Information does not decide result.
    SELECT SINGLE mandt INTO sy-mandt
                    FROM mara
                    WHERE matnr = it_body-matnr .
    IF sy-subrc NE 0.
      it_body-zrslt = 'E'.
      it_body-zmsg  = 'The part number does not exist'.
    ELSE.
      it_body-zrslt = 'S'.
      it_body-zmsg  = 'Success'.
    ENDIF.

    MODIFY it_body.

    MOVE-CORRESPONDING it_body TO lt_zmmt_gs004.

*// Change log ..
    SELECT SINGLE * INTO ls_zmmt_gs004
           FROM zmmt_gs004
           WHERE bukrs = it_body-bukrs
             AND matnr = it_body-matnr.

    IF sy-subrc EQ 0.
      lt_zmmt_gs004-ernam = sy-uname.
      lt_zmmt_gs004-erdat = sy-datum.
      lt_zmmt_gs004-aenam = ls_zmmt_gs004-ernam.
      lt_zmmt_gs004-aedat = ls_zmmt_gs004-erdat.

    ELSE.
      lt_zmmt_gs004-ernam = sy-uname.
      lt_zmmt_gs004-erdat = sy-datum.

    ENDIF.

    APPEND lt_zmmt_gs004. CLEAR lt_zmmt_gs004.

    lt_log-ret_code    = it_body-zcebes.
    CASE lt_log-ret_code.
      WHEN 'J'. lt_log-ret_reason  = 'Module Parts'.
      WHEN 'A'. lt_log-ret_reason  = 'BOM error(SPEC)'.
      WHEN 'B'. lt_log-ret_reason  = 'BOM error(Parts Number)'.
      WHEN 'C'. lt_log-ret_reason  = 'BOM error(SUB Parts)'.
      WHEN 'D'. lt_log-ret_reason  = 'BOM error(installation diagram)'.
      WHEN 'E'. lt_log-ret_reason  = 'Team or Vender error'.
      WHEN 'F'. lt_log-ret_reason  = 'Oversea Developing Parts'.
      WHEN 'G'. lt_log-ret_reason  = 'Discontinued Parts'.
      WHEN 'H'. lt_log-ret_reason  = 'Mutual Exchange Parts'.
      WHEN 'I'. lt_log-ret_reason  = 'ETC'.
    ENDCASE.

    lt_log-result      = it_body-zrslt.
    lt_log-count       = '1'.
    COLLECT lt_log. CLEAR lt_log.

  ENDLOOP.

  MODIFY zmmt_gs004 FROM TABLE lt_zmmt_gs004 .
  IF sy-subrc EQ 0.
    e_return-type    = 'S'.
    e_return-message = 'Sucess'.

  ELSE.
    e_return-type    = 'E'.
    e_return-message = 'Fail to Save'.

  ENDIF.

  LOOP AT lt_log.

    zmmc_cl_if=>if_set_key(   ifkey = 'MMIF_G-POS_01'
                              modle = 'GPOS'
                              centy = 'US'
                              dirct = 'I'
                              logox = ' '
                              ttype = 'S'
                              cparm = '10'
                           ).

    zmmc_cl_if=>if_set_messg( type    = lt_log-result
                              id      = ' '
                              message = lt_log-msg
                             ).


    zmmc_cl_if=>if_set_param( istat = e_return-type
                              ifp01 = lt_log-ret_code
                              ifp02 = lt_log-ret_reason
                              ifp03 = lt_log-count
                              ifp04 = lt_log-msg
                            ).

    zmmc_cl_if=>if_save_data( ).
  ENDLOOP.

ENDFUNCTION.
*&---------------------------------------------------------------------*
*&      Form  alph_mat2_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->C_MATNR    text
*----------------------------------------------------------------------*
FORM alph_mat2_input CHANGING c_matnr TYPE matnr .

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
    EXPORTING
      input        = c_matnr
    IMPORTING
      output       = c_matnr
    EXCEPTIONS
      length_error = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "alph_mat2_input
