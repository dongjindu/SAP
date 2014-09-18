************************************************************************
* Program Name      : ZESD11R_BILLING_HAC
* Author            : jun ho choi
* Creation Date     : 2004.02.05.
* Specifications By : jun ho choi
* Pattern           : 1-2
* Development Request No : UD1K907011
* Addl Documentation:
* Description       : BILLING HAC
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZESD11R_BILLING_HAC NO STANDARD PAGE HEADING
                           MESSAGE-ID ZMSD
                           LINE-SIZE 170.


*
TABLES : VKDFS, LIKP, LIPS, VBAK, TVROT, VBSS,
         CABN, AUSP, USR01.


*
DATA : BEGIN OF IT_LIST OCCURS 0,
       LIFEX LIKE LIKP-LIFEX,   "Ext. delivery : RC #
       ROUTE LIKE LIKP-ROUTE,   "Route : DESTINATION
       BEZEI LIKE TVROT-BEZEI,  "DESTINATION
       VBELN LIKE LIKP-VBELN,   "Delivery document
       VBELN_B LIKE VBRK-VBELN, "Billing document
       MATNR LIKE LIPS-MATNR,   "Material
       ARKTX LIKE LIPS-ARKTX,   "Description
       LFIMG LIKE LIPS-LFIMG,   "Delivery qty
       VRKME LIKE LIPS-VRKME,   "Sales unit
       NETWR LIKE VBAK-NETWR,   "Net value
       WAERK LIKE VBAK-WAERK,   "Doc. currency
       VBELV LIKE LIPS-VBELV,   "Originating doc
       END OF IT_LIST.

DATA : BEGIN OF IT_LIST_H OCCURS 0,
       FLAG(1),                 "CHECK BOX
       LIFEX LIKE LIKP-LIFEX,   "Ext. delivery : RC #
       END OF IT_LIST_H.

DATA : BEGIN OF IT_LIST_M OCCURS 0,
       LIFEX LIKE LIKP-LIFEX,   "Ext. delivery : RC #
       ROUTE LIKE LIKP-ROUTE,   "Route : DESTINATION
       BEZEI LIKE TVROT-BEZEI,  "DESTINATION
       END OF IT_LIST_M.

DATA : BEGIN OF BDC_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESS_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESS_TAB.

RANGES : R_KUNAG FOR LIKP-KUNAG.

DATA : OK_CODE(4),
       SAVE_OK_CODE(4).

DATA : WWW(1) VALUE 'N', "BDC MODE
       W_CNT TYPE I,
       W_TABIX LIKE SY-TABIX,
       W_FLAG(1),
       W_LIFEX LIKE LIKP-LIFEX,
       W_FIELD(15),
       W_VALUE(10),
       W_RESULT(1),
       W_SAMMG LIKE VBSK-SAMMG,
       W_C_8_GI(8).


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_FKDAT FOR VKDFS-FKDAT OBLIGATORY
                                         NO-EXTENSION NO INTERVALS
                                         DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.


*
TOP-OF-PAGE.
  PERFORM TOP_OF_PAGE.


*
TOP-OF-PAGE DURING LINE-SELECTION.
  PERFORM TOP_OF_PAGE.


*
START-OF-SELECTION.
  SET PF-STATUS 'ESD11R'.
  PERFORM GET_DATA.


*
END-OF-SELECTION.
  PERFORM DISPLAY_DATA.


*
AT USER-COMMAND.
  PERFORM USER_COMMAND.


*
AT LINE-SELECTION.
  PERFORM DISPLAY_DOC.


*
INCLUDE ZESD11L_BILLING_HAC_F01.
