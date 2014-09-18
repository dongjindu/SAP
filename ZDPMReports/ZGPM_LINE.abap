*----------------------------------------------------------------------*
*   INCLUDE ZGPM_LINE                                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_DEFAULT_LANGU  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DEFAULT_LANGU INPUT.
  CHECK ZTPM_LINE-SPRAS = SPACE.
  MOVE SY-LANGU TO ZTPM_LINE-SPRAS.
ENDMODULE.                 " SET_DEFAULT_LANGU  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TIME_STAMP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TIME_STAMP INPUT.
  IF ZTPM_LINE-ERNAM IS INITIAL.
    MOVE SY-DATUM TO ZTPM_LINE-ERDAT.
    MOVE SY-UZEIT TO ZTPM_LINE-ERZET.
    MOVE SY-UNAME TO ZTPM_LINE-ERNAM.
    MOVE SY-DATUM TO ZTPM_LINE-AEDAT.
    MOVE SY-UZEIT TO ZTPM_LINE-AEZET.
    MOVE SY-UNAME TO ZTPM_LINE-AENAM.
  ELSE.
    MOVE SY-DATUM TO ZTPM_LINE-AEDAT.
    MOVE SY-UZEIT TO ZTPM_LINE-AEZET.
    MOVE SY-UNAME TO ZTPM_LINE-AENAM.
  ENDIF.
ENDMODULE.                 " SET_TIME_STAMP  INPUT
