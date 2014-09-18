*----------------------------------------------------------------------*
***INCLUDE ZXQQMO03 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MAKE_DROP_LIST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE make_drop_list OUTPUT.
* DATA: lt_list like TABLE OF VRM_VALUES WITH HEADER LINE.

  DATA: l_name TYPE vrm_id .

  l_name = 'ZSQM_CI_QMEL-QCODEGRP_LOC'.

  SELECT DISTINCT  codegruppe AS key kurztext  AS text
    INTO TABLE xlist
     FROM qpgt
     WHERE katalogart = '#'.

  DELETE xlist WHERE key = ' '.

  sort xlist by key.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = l_name
      values = xlist.

ENDMODULE.                 " MAKE_DROP_LIST  OUTPUT
