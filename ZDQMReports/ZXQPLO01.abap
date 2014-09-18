*----------------------------------------------------------------------*
***INCLUDE ZXQPLO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  CHECK NOT SY-TCODE = 'QA01'.

  LOOP AT SCREEN.
    SCREEN-INPUT = 0.
    MODIFY SCREEN.
  ENDLOOP.


ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_DEFAULT_VALUE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DEFAULT_VALUE OUTPUT.

*-- Vehicle/Engine type : Default catalog type
if ZSQM_CI_QALS-KATART_VH is initial.
ZSQM_CI_QALS-KATART_VH = 'Q'.
endif.

*-- Validation Purpose : Default catalog type
if ZSQM_CI_QALS-KATALOGART is initial.
ZSQM_CI_QALS-KATALOGART = 'P'.
endif.

ENDMODULE.                 " SET_DEFAULT_VALUE  OUTPUT
*&------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  CHECK SY-TCODE = 'QA01'.
  SET CURSOR FIELD 'ZSQM_CI_QALS-CODE'.

ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT
