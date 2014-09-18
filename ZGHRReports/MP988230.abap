*----------------------------------------------------------------------*
*                                                                      *
*       Input-modules for infotype 9882                                *
*                                                                      *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  ORG_HELP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE org_help INPUT.

  DATA: lv_realo    TYPE realo.
  DATA: lt_fields   TYPE TABLE OF dynpread WITH HEADER LINE.


  CALL FUNCTION 'RH_TYPE_STRUC_HELP'
    EXPORTING
      act_search_otype         = 'O'
      act_search_wegid         = 'O-O_DOWN'
      act_root_ot              = 'O'
      act_plvar                = '01'
    IMPORTING
      selected_objid           = lv_realo
    EXCEPTIONS
      no_active_plvar          = 1
      no_object_selected       = 2
      no_struc_search_possible = 3
      OTHERS                   = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF NOT lv_realo IS INITIAL.
    p9882-orgeh = lv_realo.
  ENDIF.

  PERFORM get_org_name.

  REFRESH lt_fields.
  lt_fields-fieldname = 'GV_STEXT'.
  lt_fields-fieldvalue = gv_stext.
  APPEND lt_fields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_fields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDMODULE.                 " ORG_HELP  INPUT

*----------------------------------------------------------------------*
*  MODULE check_validation
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE check_validation.

  CLEAR: gv_stext.
  SELECT SINGLE stext INTO gv_stext FROM hrp1000
   WHERE plvar = '01'
   AND   otype = 'O'
   AND   objid = p9882-orgeh
   AND   istat = '1'
   AND   begda <= p9882-begda
   AND   endda => p9882-endda
   AND   langu = sy-langu.

  IF sy-subrc <> 0.
    SELECT SINGLE stext INTO gv_stext FROM hrp1000
     WHERE plvar = '01'
     AND   otype = 'O'
     AND   objid = p9882-orgeh
     AND   istat = '1'
     AND   begda <= p9882-begda
     AND   endda => p9882-endda.

    IF sy-subrc <> 0.
      MESSAGE e000(zghrm) WITH p9882-orgeh p9882-begda p9882-endda.
    ENDIF.
  ENDIF.
ENDMODULE.                    "check_validation
