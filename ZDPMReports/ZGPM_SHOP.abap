*----------------------------------------------------------------------*
*   INCLUDE ZGPM_SHOP                                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  set_time_stamp  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TIME_STAMP INPUT.
  IF ZTPM_SHOP-ERNAM IS INITIAL.
    MOVE SY-DATUM TO ZTPM_SHOP-ERDAT.
    MOVE SY-UZEIT TO ZTPM_SHOP-ERZET.
    MOVE SY-UNAME TO ZTPM_SHOP-ERNAM.
    MOVE SY-DATUM TO ZTPM_SHOP-AEDAT.
    MOVE SY-UZEIT TO ZTPM_SHOP-AEZET.
    MOVE SY-UNAME TO ZTPM_SHOP-AENAM.
  ELSE.
    MOVE SY-DATUM TO ZTPM_SHOP-AEDAT.
    MOVE SY-UZEIT TO ZTPM_SHOP-AEZET.
    MOVE SY-UNAME TO ZTPM_SHOP-AENAM.
  ENDIF.
ENDMODULE.                 " set_time_stamp  INPUT
*&---------------------------------------------------------------------*
*&      Module  set_default_langu  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_DEFAULT_LANGU INPUT.
  CHECK ZTPM_SHOP-SPRAS = SPACE.
  MOVE SY-LANGU TO ZTPM_SHOP-SPRAS.
ENDMODULE.                 " set_default_langu  INPUT
