*----------------------------------------------------------------------*
*   INCLUDE ZIPP307L_CLASS_BDC_T                                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* STANDARD-TABLE AREA ( TABLE)
*----------------------------------------------------------------------*
TABLES: ZTBM_ABXCLSDT,
        KLAH,
        KSML,
        CABN.
*----------------------------------------------------------------------*
* INTERNAL-TABLE AREA
*----------------------------------------------------------------------*
DATA: IT_ACLS TYPE ZTBM_ABXCLSDT OCCURS 0 WITH HEADER LINE.

* EXCEL UPLOAD
DATA: BEGIN OF IT_EXCL OCCURS 0,
        CLID(18),  " CLASS ID
        CLTY(03),  " CLASS TYPE
        CHID(30),  " CHARACTERISTIC ID
        CHVL(30),  " CHARACTERISTIC VALUE
        EONO(12),  " CHANGE NUMBER
        FLAL(08),  " VALID FROM
        CDES(40),  " CLASS ID DESCRIPTION
        CHDS(30),  " CHARACTERISTIC DESCRIPTION
        STAT(01),  " STATUS
        TVAL(08),  " VALID TO
        ZRESULT TYPE ZTBM_ABXCLSDT-ZRESULT,
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
  P_ZEDAT LIKE ZTBM_ABXCLSDT-ZEDAT DEFAULT SY-DATUM,
  P_ZBTIM LIKE ZTBM_ABXCLSDT-ZBTIM.

* EXCEL DATA UPLOAD
PARAMETERS:
*  P_GROUP LIKE APQI-GROUPID DEFAULT SY-REPID  OBLIGATORY,
  P_FILE  LIKE RLGRAP-FILENAME DEFAULT 'C:\       .TXT' OBLIGATORY,
  P_FILETY LIKE RLGRAP-FILETYPE DEFAULT 'DAT',
  P_TCODE LIKE TSTC-TCODE DEFAULT 'CT04'.
SELECTION-SCREEN END   OF BLOCK B1.
