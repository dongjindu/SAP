*-------------------------------------------------------------------*
*  SELECT-OPTIONS
*-------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS : P_DATUM FOR SY-DATUM OBLIGATORY MODIF ID SD,
                   P_WOSER FOR ZTPP_WOSUM-WO_SER NO INTERVALS  ,
                   P_NATN  FOR ZTPP_WOSUM-NATION NO INTERVALS ,
                   P_DEAL  FOR ZTPP_WOSUM-DEALER NO INTERVALS .
  PARAMETERS     : P_CHECK AS CHECKBOX.

*12. ... NO INTERVALS NO-EXTENSION
  SELECTION-SCREEN END OF BLOCK B1.
*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
  INITIALIZATION .
    GV_REPID = SY-REPID.
    GV_DATUM = SY-DATUM - 7 .
    P_DATUM-LOW  = GV_DATUM.
    P_DATUM-HIGH = SY-DATUM.
    P_DATUM-OPTION = 'BT'.
    P_DATUM-SIGN = 'I'.
    APPENd P_DATUM.

    P_NATN-LOW = 'B28'.
    P_NATN-OPTION = 'EQ'.
    P_NATN-SIGN = 'E'.
    APPENd P_NATN.
*----------------------------------------------------------------------*
*     AT SELECTION-SCREEN                                              *
*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN OUTPUT .
*
*    LOOP AT SCREEN .
*      MODIFY SCREEN.
*    ENDLOOP.
