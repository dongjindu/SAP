************************************************************************
* Program name : ZEMMGM01E_BOM_REG                                     *
* Created by   : Min-su Park                                           *
* Created on   : 2003.11.10.                                           *
* Pattern      :                                                       *
* Description  : BOM Registration Request Program                      *
* Modification Log                                                     *
* Date            Developer        Request No.    Description          *
* 2003.11.10.     Min-su Park      UD1K901873     Initial Coding       *
*                                                                      *
************************************************************************
*----------------------------------------------------------------------*
*   INCLUDE ZEMMGM01E_BOM_REGEVENT                                     *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*Display adjustment following Radio Button.
  CASE W_SEL.
    WHEN R1.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'AA' OR SCREEN-GROUP1 = 'AA1'.
          SCREEN-INPUT     = 0 .
          SCREEN-INVISIBLE = 1 .
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN R3.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'AA1'.
          SCREEN-INPUT     = 0.
          SCREEN-INVISIBLE = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.

START-OF-SELECTION .
  CALL SCREEN 100.
