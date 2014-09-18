*----------------------------------------------------------------------*
***INCLUDE MP987000F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LISTBOX_GRADE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P9870_TRACK  text
*----------------------------------------------------------------------*
FORM change_listbox_grade  USING p_track TYPE zdhrtrack.

  TYPE-POOLS: vrm.

  DATA: l_field   TYPE  vrm_id,
        it_values TYPE  vrm_values,
        wa_values LIKE LINE OF it_values.

  DATA: it_dd07v  TYPE TABLE OF dd07v,
        wa_dd07v  LIKE LINE OF it_dd07v.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname    = 'ZDHRGRADE'
    TABLES
      values_tab = it_dd07v.

  IF p_track EQ '2'.
    DELETE it_dd07v WHERE domvalue_l >= 'G4'.
  ENDIF.

  CLEAR: wa_dd07v, it_values.
  LOOP AT it_dd07v INTO wa_dd07v.
    wa_values-key = wa_dd07v-domvalue_l.
    wa_values-text = wa_dd07v-ddtext.
    APPEND wa_values TO it_values.CLEAR wa_values.
  ENDLOOP.

  l_field = 'P9870-GRADE'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_field
      values = it_values.

ENDFORM.                    " CHANGE_LISTBOX_GRADE
