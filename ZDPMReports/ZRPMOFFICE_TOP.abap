*----------------------------------------------------------------------*
*   INCLUDE ZRPMOFFICE_TOP                                             *
*----------------------------------------------------------------------*

SET SCREEN 100.

TYPE-POOLS: SBDST, SOI, CNDP.

TYPES: BEGIN OF DOCUMENT_DESCR,
          DOCUMENT_NAME(40), DOCUMENT_ID(64),
       END OF DOCUMENT_DESCR.

TYPES: DOCUMENT_LIST TYPE TABLE OF DOCUMENT_DESCR.

TYPES: T_URL LIKE BAPIURI-URI.

DATA: FCODE LIKE SY-UCOMM.

CLASS C_OI_ERRORS DEFINITION LOAD.
CLASS CL_GUI_CFW  DEFINITION LOAD.
