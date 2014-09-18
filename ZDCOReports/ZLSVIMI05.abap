*----------------------------------------------------------------------*
*   INCLUDE ZLSVIMI05                                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHANGE_LOG_MEGXXX  INPUT
*&---------------------------------------------------------------------*
*       Leave Change log
*----------------------------------------------------------------------*
MODULE CHANGE_LOG_MEGXXX INPUT.
  PERFORM CHANGE_LOG.
ENDMODULE.                 " CHANGE_LOG_MEGXXX  INPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_CHANGE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_CHANGE_LOG.

  IF     STATUS-ACTION = 'U'.
    ZTCO_CC_SKF-AEDAT = SY-DATUM.
    ZTCO_CC_SKF-AEZET = SY-UZEIT.
    ZTCO_CC_SKF-AENAM = SY-UNAME.
  ELSEIF STATUS-ACTION = 'A'.
    ZTCO_CC_SKF-ERDAT = SY-DATUM.
    ZTCO_CC_SKF-ERZET = SY-UZEIT.
    ZTCO_CC_SKF-ERNAM = SY-UNAME.
  ENDIF.

ENDMODULE.                 " CREATE_CHANGE_LOG.
