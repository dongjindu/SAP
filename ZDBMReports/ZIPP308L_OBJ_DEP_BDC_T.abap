*----------------------------------------------------------------------*
*   INCLUDE ZIPP308L_OBJ_DEP_BDC_T                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTBM_ABXODPDT.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: IT_AODP TYPE ZTBM_ABXODPDT OCCURS 0 WITH HEADER LINE.

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        DPID(30),
        EONO(12),
        DDES(30),
        VALD(08),
        STAT(01),
        LIN1(06),
        DPC1(72),
        LIN2(06),
        DPC2(72),
        LIN3(06),
        DPC3(72),
        LIN4(06),
        DPC4(72),
        LIN5(06),
        DPC5(72),
        LIN6(06),
        DPC6(72),
        SYNT(01),
        ZRESULT TYPE ZTBM_ABXODPDT-ZRESULT,
        ZMSG    LIKE CFGNL-MSGLIN,
      END   OF IT_EXCL.

DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 DEFAULT 'X'
                                   USER-COMMAND UCOM,
            P_RDO2 RADIOBUTTON GROUP R1.
SELECTION-SCREEN END   OF BLOCK B2.

* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_ZEDAT LIKE ZTBM_ABXODPDT-ZEDAT DEFAULT SY-DATUM,
  P_ZBTIM LIKE ZTBM_ABXODPDT-ZBTIM.

* EXCEL DATA UPLOAD
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'CU01'.
SELECTION-SCREEN END   OF BLOCK B1.
