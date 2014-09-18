************************************************************************
* Program Name      : ZESD12R_BILLING_HMA
* Author            : jun ho choi
* Creation Date     : 2004.02.11.
* Specifications By : jun ho choi
* Pattern           : 1-2
* Development Request No : UD1K907011
* Addl Documentation:
* Description       : BILLING HMA
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZESD12R_BILLING_HMA NO STANDARD PAGE HEADING
                           MESSAGE-ID ZMSD
                           LINE-SIZE 160.


*
TABLES : VKDFS, USR01.


*
DATA : BEGIN OF IT_VKDFS OCCURS 0.
       INCLUDE STRUCTURE VKDFS.
DATA : END OF IT_VKDFS.

DATA : BEGIN OF BDC_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA : BEGIN OF MESS_TAB OCCURS 0.
       INCLUDE STRUCTURE BDCMSGCOLL.
DATA : END OF MESS_TAB.

DATA : BEGIN OF W_KUNNR OCCURS 0,
       KUNNR LIKE VKDFS-KUNNR,
       END OF W_KUNNR.

RANGES : R_KUNAG FOR LIKP-KUNAG.

DATA : WWW(1) VALUE 'N', "BDC MODE
       W_RESULT_MSG(100),
       W_CNT TYPE I.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_FKDAT FOR VKDFS-FKDAT OBLIGATORY NO-EXTENSION
                                         DEFAULT SY-DATUM.
SELECTION-SCREEN END OF BLOCK B1.


*
START-OF-SELECTION.
  PERFORM GET_DATA.


*
END-OF-SELECTION.
  PERFORM PROCESS_DATA.


*
INCLUDE ZESD12L_BILLING_HMA_F01.
