************************************************************************
* Program Name      : ZRSD05R_ORDER_PRICE_LIST
* Author            : jun ho choi
* Creation Date     : 2004.03.29.
* Specifications By : jun ho choi
* Pattern           : Report 1-2
* Development Request No : UD1K908791
* Addl Documentation:
* Description       : Order Pricing List
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZRSD05R_ORDER_PRICE_LIST NO STANDARD PAGE HEADING
                                MESSAGE-ID ZMSD.


*
TABLES : ZSSD_OR_PRICE_LIST,
         VBAK,
         VBAP,
         KONV.


*
DATA : IT_OR_PRL TYPE STANDARD TABLE OF ZSSD_OR_PRICE_LIST
                                        WITH NON-UNIQUE
                 DEFAULT KEY INITIAL SIZE 100,
       T_OR_PRL TYPE ZSSD_OR_PRICE_LIST.

DATA : BEGIN OF IT_VBAK OCCURS 0.
       INCLUDE STRUCTURE VBAK.
DATA : END OF IT_VBAK.

DATA : BEGIN OF IT_VBAP OCCURS 0.
       INCLUDE STRUCTURE VBAP.
DATA : END OF IT_VBAP.

DATA : W_CNT TYPE I.

RANGES : S_KUNNR FOR VBAK-KUNNR,
         S_BSTNK FOR VBAK-BSTNK.

DATA : OK_CODE(4),
       SAVE_OK_CODE(4).
DATA : ALV_GRID       TYPE REF TO CL_GUI_ALV_GRID,
       CONTAINER      TYPE REF TO CL_GUI_CUSTOM_CONTAINER.
DATA : GS_VARIANT     TYPE DISVARIANT.
DATA : GS_LAYOUT TYPE LVC_S_LAYO.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_KUNN FOR VBAK-KUNNR+0(3) NO-EXTENSION OBLIGATORY,
                 S_YYMM FOR VBAK-BSTNK+1(4) NO-EXTENSION OBLIGATORY
                                            DEFAULT SY-DATUM+2(4).
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  PERFORM CHECK_INIT.
  PERFORM READ_DATA.
  PERFORM MODIFY_DATA.



*
END-OF-SELECTION.
  PERFORM CALL_SCREEN.





INCLUDE ZRSD05R_ORDER_PRICE_LIST_F01.
INCLUDE ZRSD05R_ORDER_PRICE_LIST_PBO.
INCLUDE ZRSD05R_ORDER_PRICE_LIST_PAI.
