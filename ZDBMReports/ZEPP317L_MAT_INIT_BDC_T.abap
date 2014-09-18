*----------------------------------------------------------------------*
*   INCLUDE ZEPP317L_MAT_INIT_BDC_T                                    *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTBM_ABXCHRDT,
        MARC,
        MARA.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: IT_ACHR TYPE ZTBM_ABXCHRDT OCCURS 0 WITH HEADER LINE.

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        MATNR TYPE MARA-MATNR,  "MATERIAL
        WERKS TYPE MARC-WERKS,  "Plant
        MTART TYPE MARA-MTART,  "Material type
        MBRSH TYPE MARA-MBRSH,  "Industry sector
        MAKTX TYPE MAKT-MAKTX,  "Material description
        MEINS TYPE MARA-MEINS,  "Base unit of measure
        SGROU(10),              "SYSTEM GORUP
        MTPOS TYPE MARA-MTPOS_MARA,  "General item category group
        NORMT TYPE MARA-NORMT,  "Industry Standard Description
        KZKFG TYPE MARA-KZKFG,  "Configurable Material
        DISMM TYPE MARC-DISMM,  "MRP type
        DISPO TYPE MARC-DISPO,  "MRP controller
        DISLS TYPE MARC-DISLS,  "Lot size (materials planning)
        BESKZ TYPE MARC-BESKZ,  "Procurement Type
        FHORI TYPE MARC-FHORI,  "Scheduling Margin Key for Floats
        MTVFP TYPE MARC-MTVFP,  "Checking group for availability check
        SATNR TYPE MARA-SATNR,  "Cross-Plant Configurable Material
*       Method for Selecting Alternative Bills of Material
        ALTSL TYPE MARC-ALTSL,
*       Dependent requirements ind. for individual and coll. reqmts
        SBDKZ TYPE MARC-SBDKZ,
        VERKZ TYPE MARC-VERKZ,  "Version indicator
        KLART TYPE RMCLKLART-KLART,  "Class type
        CLASS TYPE RMCLF-CLASS,  "Class number
        MSGTY TYPE SY-MSGTY,
        MESSG LIKE CFGNL-MSGLIN,
      END   OF IT_EXCL.

DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.
DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.
  DATA: BEGIN OF WA_OPT OCCURS 0.
          INCLUDE STRUCTURE CTU_PARAMS.
  DATA: END OF WA_OPT.

*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.


*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_RDO1 RADIOBUTTON GROUP R1 USER-COMMAND UCOM,
            P_RDO2 RADIOBUTTON GROUP R1 DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK B2.
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'MM01'.
SELECTION-SCREEN END   OF BLOCK B1.
