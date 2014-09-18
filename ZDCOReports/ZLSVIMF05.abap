*----------------------------------------------------------------------*
*   INCLUDE ZLSVIMF05                                                  *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHANGE_LOG
*&---------------------------------------------------------------------*
*       Change Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHANGE_LOG.

  IF     STATUS-ACTION = 'U'.
   ZTCO_CC_PRO_QTY-AEDAT = SY-DATUM.
    ZTCO_CC_PRO_QTY-AEZET = SY-UZEIT.
    ZTCO_CC_PRO_QTY-AENAM = SY-UNAME.
  ELSEIF STATUS-ACTION = 'A'.
    ZTCO_CC_PRO_QTY-ERDAT = SY-DATUM.
    ZTCO_CC_PRO_QTY-ERZET = SY-UZEIT.
    ZTCO_CC_PRO_QTY-ERNAM = SY-UNAME.
  ENDIF.

ENDFORM.                    " CHANGE_LOG
