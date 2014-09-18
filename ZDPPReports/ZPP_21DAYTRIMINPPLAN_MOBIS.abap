************************************************************************
* Program Name      : ZPP_21DAYTRIMINPPLAN_MOBIS
* Author            : Haseeb Mohammad
* Creation Date     : 2007-01-31
* Specifications By : Daniel Kim
* Pattern           :
* Development Request No :UD1K930591
* Addl Documentation:
* Description       : Send 21 day input plan to MOBIS. Every day.
*
*
* Modification Logs
* Date           Developer         Transport     descriptio
* 02-19-2007     Haseeb Mohammad   UD1K930781    Date count correction.
* 03-07-2007     Haseeb Mohammad   UD1K930992    VERSION number if '000'
*                                                change it to '   '.
* 11-17-2010     Haseeb Mohammad   UD1K950224    File format for Weekly
*                                                data changed.
* 12-07-2010     SJ Lee            UD1K950379
************************************************************************

REPORT  ZPP_21DAYTRIMINPPLAN_MOBIS              .

INCLUDE ZPP_21DAYTRIM_TOP.
INCLUDE ZPP_21DAYTRIM_SEL.
INCLUDE ZPP_21DAYTRIM_C01.
INCLUDE ZPP_21DAYTRIM_F01.
INCLUDE ZPP_21DAYTRIM_O01.
INCLUDE ZPP_21DAYTRIM_I01.

*---------------------------------------------------------------------*
* INITIALIZATION .
*---------------------------------------------------------------------*
INITIALIZATION .
  GV_REPID = SY-REPID.
*---------------------------------------------------------------------*
*  START-OF-SELECTION
*---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: PERCENT(3) TYPE N.

  DO 100 TIMES.
    MOVE: SY-INDEX TO PERCENT.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
         EXPORTING
              PERCENTAGE = PERCENT
              TEXT       = TEXT-001
         EXCEPTIONS
              OTHERS     = 1.

  ENDDO.

  PERFORM P2000_GET_DATA.

  IF P_SEND EQ 'X'.
    PERFORM P3000_SAVE_DATA.
  ENDIF.

END-OF-SELECTION.
  IF P_DISP EQ 'X'.
    CALL SCREEN 0100.
  ENDIF.
