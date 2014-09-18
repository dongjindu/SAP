************************************************************************
* Program Name      : ZASD03R_VEN_WC
* Author            : jun ho choi
* Creation Date     : 2003.07.18.
* Specifications By : jun ho choi
* Pattern           : 1-2
* Development Request No : UD1K904910
* Addl Documentation:
* Description       : Main. Vendor Warranty Condition
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZASD03R_VEN_WC NO STANDARD PAGE HEADING
                      MESSAGE-ID ZMSD
                      LINE-SIZE 180.


*
TABLES : ZTSD_VEN_WC.


*
DATA : BEGIN OF IT_VEN_WC OCCURS 0.
       INCLUDE STRUCTURE ZTSD_VEN_WC.
DATA : END OF IT_VEN_WC.

DATA : OK_CODE(4),
       SAVE_OK_CODE(4).

DATA : W_CNT TYPE I,
       W_ZVSEQ LIKE ZTSD_VEN_WC-ZVSEQ,
       W_SAVE(1),
       W_ANSWER(1).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_ZVEND LIKE ZTSD_VEN_WC-ZVEND OBLIGATORY.
SELECTION-SCREEN END OF BLOCK B1.


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
INCLUDE ZASD03L_VEN_WC_F01.
INCLUDE ZASD03L_VEN_WC_PBO.
INCLUDE ZASD03L_VEN_WC_PAI.
