************************************************************************
* Program Name      : ZASD03R_RATIO_I
* Author            : jun ho choi
* Creation Date     : 2003.07.18.
* Specifications By : jun ho choi
* Pattern           : 1-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Main. Vendor Sharing Ratio by Items
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZASD03R_RATIO_I NO STANDARD PAGE HEADING
                       MESSAGE-ID ZMSD
                       LINE-SIZE 170.


*
TABLES : ZTSD_RATIO_I.


*
DATA : BEGIN OF IT_RATIO_I OCCURS 0.
       INCLUDE STRUCTURE ZTSD_RATIO_I.
DATA : END OF IT_RATIO_I.

DATA : OK_CODE(4),
       SAVE_OK_CODE(4).

DATA : W_CNT TYPE I,
       W_ZVSEQ LIKE ZTSD_RATIO_I-ZVSEQ,
       W_SAVE(1),
       W_ANSWER(1).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_ZVEND LIKE ZTSD_RATIO_I-ZVEND OBLIGATORY,
             P_ZGROP LIKE ZTSD_RATIO_I-ZGROP OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


*
AT SELECTION-SCREEN ON P_ZGROP.
  IF P_ZGROP EQ 'V' OR
     P_ZGROP EQ 'P' OR
     P_ZGROP EQ 'N' OR
     P_ZGROP EQ 'C' OR
     P_ZGROP EQ 'L'.
  ELSE.
    MESSAGE E000 WITH 'You have to select valid value'.
  ENDIF.

*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.


*
START-OF-SELECTION.
  SET PF-STATUS 'ASD03R'.
  PERFORM GET_DATA.


*
END-OF-SELECTION.
  PERFORM DISPLAY_DATA.


*
AT USER-COMMAND.
  PERFORM USER_COMMAND.


*
INCLUDE ZASD03L_RATIO_I_F01.
INCLUDE ZASD03L_RATIO_I_PBO.
INCLUDE ZASD03L_RATIO_I_PAI.
