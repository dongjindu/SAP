************************************************************************
* Program Name      : ZASD03R_RATIO_USER
* Author            : Chris Li
* Creation Date     : 2005.04.22.
* Specifications By : Chris Li
* Pattern           : 1-2
* Development Request No :UD1K914838
* Addl Documentation:
* Description       : Table ZTSD_RATIO_USER Maintenance
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZASD03R_RATIO_USER NO STANDARD PAGE HEADING
                       MESSAGE-ID ZMSD
                       LINE-SIZE 225.


*
TABLES : ZTSD_RATIO_USER.


*
DATA : BEGIN OF IT_RATIO_USER OCCURS 0.
       INCLUDE STRUCTURE ZTSD_RATIO_USER.
DATA : END OF IT_RATIO_USER.

DATA : OK_CODE(4),
       SAVE_OK_CODE(4).
DATA : W_CREATE.
DATA : W_CNT TYPE I,
       W_ZVSEQ LIKE ZTSD_RATIO_I-ZVSEQ,
       W_SAVE(1),
       W_ANSWER(1).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_ZITEM LIKE ZTSD_RATIO_USER-ZITEM OBLIGATORY,
             P_ZGROP LIKE ZTSD_RATIO_USER-ZGROP OBLIGATORY.
*             P_ZVSEQ LIKE ZTSD_RATIO_USER-ZVSEQ OBLIGATIORY.
SELECTION-SCREEN END OF BLOCK B1.


*
AT SELECTION-SCREEN ON P_ZGROP.
  IF P_ZGROP EQ 'P' OR
     P_ZGROP EQ 'L'.
  ELSE.
    MESSAGE E000 WITH 'Valid value is P or L!'.
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
INCLUDE ZASD03L_RATIO_USER_F01.

INCLUDE ZASD03L_RATIO_USER_PBO.

INCLUDE ZASD03L_RATIO_USER_PAI.
