*----------------------------------------------------------------------*
*   INCLUDE ZIPP305L_FSC_219_DATA_BDC_T                                *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ztbm_abxalcdt,ztpp_pmt03bb.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: it_aalc TYPE ztbm_abxalcdt OCCURS 0 WITH HEADER LINE.

* EXCEL UPLOAD
DATA: BEGIN OF it_excl OCCURS 0,
        matnr TYPE mara-matnr,  "MATERIAL
        werks TYPE marc-werks,  "Plant
        stlan TYPE mkal-stlan,  "BOM USAGE
        stlal TYPE mkal-stlal,  "ALTERNATIVE BOM
        aennr TYPE aenr-aennr,  "EO NERBUM
        bmeng(20),  "TYPE STKO-BMENG,  "BASE QUANTITY
        bmein TYPE stko-bmein,  "BASC UNIT OF MEASURE FOR BOM
        stlst TYPE stko-stlst,  "BOM STATUS
        zalcd TYPE ztbm_abxalcdt-alcd,                      "219 CODE
        zdesc TYPE stko-stktx,  "ALTERNATIVE BOM TEXT
        zupct TYPE ztbm_abxalcdt-upct, " CONT.TYPE
        msgty TYPE sy-msgty,
        messg LIKE cfgnl-msglin,
      END   OF it_excl.

DATA: BEGIN OF it_bdc OCCURS 0.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF it_bdc.
DATA: BEGIN OF it_mess OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_mess.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: wa_line_idx TYPE i,
      wa_erro_idx TYPE i.

CONSTANTS: c_datum LIKE sy-datum VALUE '19000101'.

*----------------------------------------------------------------------*
* SELECTION SCREEN LAYOUT
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-200.
PARAMETERS: p_rdo1 RADIOBUTTON GROUP r1 USER-COMMAND ucom DEFAULT 'X',
            p_rdo2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN END   OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-200.
PARAMETERS:
  p_zedat LIKE ztbm_abxalcdt-zedat DEFAULT sy-datum,
  p_zbtim LIKE ztbm_abxalcdt-zbtim.

PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  p_file  LIKE rlgrap-filename DEFAULT 'C:\       .TXT' OBLIGATORY,
  p_filety LIKE rlgrap-filetype DEFAULT 'DAT',
  p_tcode LIKE tstc-tcode DEFAULT 'CS02'.
SELECTION-SCREEN END   OF BLOCK b1.
