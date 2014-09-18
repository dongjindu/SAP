
************************************************************************
* Program Name      : ZACO40R_NMHAM
* Author            : Byung Sung Bae
* Creation Date     : 09/10/2003
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No: UD1K902602
* Add documentation :
* Description       : This program is copied from ZACO03R_MHAM
* the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
* Comments
* This report was generated by SAP and changed by programmer
* The naming convension can be different from that
* in HMMA Development Standard Document (v2)

REPORT ZACO03R_MHAM   LINE-SIZE 253 NO STANDARD PAGE HEADING LINE-COUNT
000(003).

INCLUDE <SYMBOL>.
INCLUDE <ICON>.
SELECTION-SCREEN: BEGIN OF BLOCK PROG
                           WITH FRAME TITLE TEXT-F58.

TABLES ZTCO_NMHHRTRANS.
DATA %COUNT-ZTCO_NMHHRTRANS(4) TYPE X.
DATA %LINR-ZTCO_NMHHRTRANS(2).

TABLES AQLDB.

INCLUDE RSAQEXCD.

DATA: BEGIN OF %ST_LISTE OCCURS 100,
          HEAD(1),
          TAB(3),
          LINE(6) TYPE N,
          CONT(1) TYPE N,
          FINT(1),
          FINV(1),
          FCOL(1) TYPE N,
          TEXT(0253),
      END OF %ST_LISTE.

DATA %DATA_SELECTED(1).
DATA %GLFRAME(1)  VALUE 'X' .
DATA %UFLAG(1).
DATA %USTFLAG(1).
DATA %GRST_TEXT(255).
DATA %GLLINE TYPE I.
DATA %TABIX LIKE SY-TABIX.
DATA %PRFLAG(1) TYPE X VALUE '02'.


DATA %PERC(4) TYPE P DECIMALS 3.
DATA %P100(4) TYPE P DECIMALS 3 VALUE '100.000'.
DATA %RANGCT TYPE I.
DATA %RANGCC(8).
DATA %SUBRC LIKE SY-SUBRC.

DATA: BEGIN OF %WA010 OCCURS 10,
            ZTCO_NMHHRTRANS-ACTQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-CURQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-VAEQTY(16) TYPE P DECIMALS 03,
            BEGIN OF ZTCO_NMHHRTRANS,
                  ZTCO_NMHHRTRANS-UNIT LIKE ZTCO_NMHHRTRANS-UNIT,
            END OF ZTCO_NMHHRTRANS,
      END OF %WA010.

DATA: BEGIN OF %W0100 OCCURS 20,
            ZTCO_NMHHRTRANS-UNIT LIKE ZTCO_NMHHRTRANS-UNIT,
            ZTCO_NMHHRTRANS-ACTQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-CURQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-VAEQTY(16) TYPE P DECIMALS 03,
      END OF %W0100.

DATA: BEGIN OF %W0104 OCCURS 20,
            ZTCO_NMHHRTRANS-UNIT LIKE ZTCO_NMHHRTRANS-UNIT,
            ZTCO_NMHHRTRANS-ACTQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-CURQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-VAEQTY(16) TYPE P DECIMALS 03,
      END OF %W0104.

DATA: BEGIN OF %W0103 OCCURS 20,
            ZTCO_NMHHRTRANS-UNIT LIKE ZTCO_NMHHRTRANS-UNIT,
            ZTCO_NMHHRTRANS-ACTQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-CURQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-VAEQTY(16) TYPE P DECIMALS 03,
      END OF %W0103.

DATA: BEGIN OF %W0102 OCCURS 20,
            ZTCO_NMHHRTRANS-UNIT LIKE ZTCO_NMHHRTRANS-UNIT,
            ZTCO_NMHHRTRANS-ACTQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-CURQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-VAEQTY(16) TYPE P DECIMALS 03,
      END OF %W0102.

DATA: BEGIN OF %W0101 OCCURS 20,
            ZTCO_NMHHRTRANS-UNIT LIKE ZTCO_NMHHRTRANS-UNIT,
            ZTCO_NMHHRTRANS-ACTQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-CURQTY(16) TYPE P DECIMALS 03,
            ZTCO_NMHHRTRANS-VAEQTY(16) TYPE P DECIMALS 03,
      END OF %W0101.
SELECT-OPTIONS SP$00001 FOR ZTCO_NMHHRTRANS-GJAHR MEMORY ID GJR.
SELECT-OPTIONS SP$00002 FOR ZTCO_NMHHRTRANS-PERID MEMORY ID VPE.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK DIRECT
                  WITH FRAME TITLE TEXT-F59.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %ALV RADIOBUTTON GROUP FUNC USER-COMMAND OUTBUT
                         DEFAULT 'X' .
SELECTION-SCREEN: COMMENT 4(26) TEXT-F72 FOR FIELD %ALV.
PARAMETERS:       %ALVL TYPE SLIS_VARI.
SELECTION-SCREEN: PUSHBUTTON 72(4) PB%EXCO USER-COMMAND EXPCOL.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %NOFUNC RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 4(26) TEXT-F66 FOR FIELD %NOFUNC
                                         MODIF ID OLD.
PARAMETERS:       %TVIEW RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 34(26) TEXT-F68 FOR FIELD %TVIEW
                                          MODIF ID OLD,
                  END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %GRAPH RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 4(26) TEXT-F61 FOR FIELD %GRAPH
                                         MODIF ID OLD.
PARAMETERS:       %TEXT RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 34(26) TEXT-F69 FOR FIELD %TEXT
                                          MODIF ID OLD,
                  END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %ABC RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 4(26) TEXT-F70 FOR FIELD %ABC
                                         MODIF ID OLD.
PARAMETERS:       %EXCEL RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 34(26) TEXT-F60 FOR FIELD %EXCEL
                                         MODIF ID OLD,
                  END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %EIS RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 4(26) TEXT-F63 FOR FIELD %EIS
                                         MODIF ID OLD.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %XINT RADIOBUTTON GROUP FUNC MODIF ID XIN.
SELECTION-SCREEN: COMMENT 4(26) TEXT-F73 FOR FIELD %XINT
                                         MODIF ID XIN.
PARAMETERS:       %XINTK(30) LOWER CASE MODIF ID XIN.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %DOWN RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 4(26) TEXT-F64 FOR FIELD %DOWN
                                         MODIF ID OLD.
PARAMETERS:       %PATH(132) LOWER CASE MODIF ID OLD.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS:       %SAVE RADIOBUTTON GROUP FUNC MODIF ID OLD.
SELECTION-SCREEN: COMMENT 4(26) TEXT-F62 FOR FIELD %SAVE
                                         MODIF ID OLD.
PARAMETERS:       %LISTID(40) LOWER CASE MODIF ID OLD.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN: END OF BLOCK DIRECT.
SELECTION-SCREEN: END OF BLOCK PROG.

DATA: BEGIN OF %G00 OCCURS 100,
            ZTCO_NMHHRTRANS-GJAHR LIKE ZTCO_NMHHRTRANS-GJAHR,
            ZTCO_NMHHRTRANS-PERID LIKE ZTCO_NMHHRTRANS-PERID,
            ZTCO_NMHHRTRANS-KOSTL LIKE ZTCO_NMHHRTRANS-KOSTL,
            ZTCO_NMHHRTRANS-LSTAR LIKE ZTCO_NMHHRTRANS-LSTAR,
            ZTCO_NMHHRTRANS-ACTQTY LIKE ZTCO_NMHHRTRANS-ACTQTY,
            ZTCO_NMHHRTRANS-CURQTY LIKE ZTCO_NMHHRTRANS-CURQTY,
            ZTCO_NMHHRTRANS-VAEQTY LIKE ZTCO_NMHHRTRANS-VAEQTY,
            ZTCO_NMHHRTRANS-UNIT LIKE ZTCO_NMHHRTRANS-UNIT,
            ZTCO_NMHHRTRANS-ERDAT LIKE ZTCO_NMHHRTRANS-ERDAT,
            ZTCO_NMHHRTRANS-ERZET LIKE ZTCO_NMHHRTRANS-ERZET,
            ZTCO_NMHHRTRANS-ERNAM LIKE ZTCO_NMHHRTRANS-ERNAM,
            ZTCO_NMHHRTRANS-AEDAT LIKE ZTCO_NMHHRTRANS-AEDAT,
            ZTCO_NMHHRTRANS-AEZET LIKE ZTCO_NMHHRTRANS-AEZET,
            ZTCO_NMHHRTRANS-AENAM LIKE ZTCO_NMHHRTRANS-AENAM,
      END OF %G00.
DATA: BEGIN OF %%G00,
            ZTCO_NMHHRTRANS-GJAHR(004),
            ZTCO_NMHHRTRANS-PERID(003),
            ZTCO_NMHHRTRANS-KOSTL(010),
            ZTCO_NMHHRTRANS-LSTAR(006),
            ZTCO_NMHHRTRANS-ACTQTY(020),
            ZTCO_NMHHRTRANS-CURQTY(020),
            ZTCO_NMHHRTRANS-VAEQTY(020),
            ZTCO_NMHHRTRANS-UNIT(003),
            ZTCO_NMHHRTRANS-ERDAT(010),
            ZTCO_NMHHRTRANS-ERZET(008),
            ZTCO_NMHHRTRANS-ERNAM(012),
            ZTCO_NMHHRTRANS-AEDAT(010),
            ZTCO_NMHHRTRANS-AEZET(008),
            ZTCO_NMHHRTRANS-AENAM(012),
      END OF %%G00.
DATA %ZNR TYPE I.
DATA %LZNR TYPE I VALUE 99999.
FIELD-GROUPS HEADER.
DATA %GROUP04.
DATA %%ZTCO_NMHHRTRANS-LSTAR LIKE ZTCO_NMHHRTRANS-LSTAR.
DATA %%%ZTCO_NMHHRTRANS-LSTAR(1).
DATA %GROUP0104.
DATA %GROUP03.
DATA %%ZTCO_NMHHRTRANS-KOSTL LIKE ZTCO_NMHHRTRANS-KOSTL.
DATA %%%ZTCO_NMHHRTRANS-KOSTL(1).
DATA %GROUP0103.
DATA %GROUP02.
DATA %%ZTCO_NMHHRTRANS-PERID LIKE ZTCO_NMHHRTRANS-PERID.
DATA %%%ZTCO_NMHHRTRANS-PERID(1).
DATA %GROUP0102.
DATA %GROUP01.
DATA %%ZTCO_NMHHRTRANS-GJAHR LIKE ZTCO_NMHHRTRANS-GJAHR.
DATA %%%ZTCO_NMHHRTRANS-GJAHR(1).
DATA %GROUP0101.
FIELD-GROUPS %FG01.
DATA %ANY-01.
DATA %EXT-ZTCO_NMHHRTRANS01.
FIELD-GROUPS %FGWRZTCO_NMHHRTRANS01.

CONTROLS TVIEW100 TYPE TABLEVIEW USING SCREEN 100.

AT SELECTION-SCREEN .
PERFORM ALVL_CHECK(RSAQEXCE) USING %ALVL 'G00'.
PERFORM TESTMODE(RSAQEXCE).
PERFORM CHECK_EXPCOL(RSAQEXCE) USING %ALV.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR %ALVL .
PERFORM ALVL_VALUE_REQUEST(RSAQEXCE) USING %ALVL 'G00'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR %XINTK .
PERFORM XINT_VALUE_REQUEST(RSAQEXCE).

AT SELECTION-SCREEN OUTPUT .

PERFORM RINIT(RSAQBRST).
PERFORM SET_EXPCOL(RSAQEXCE) USING %ALV PB%EXCO.
PERFORM ALVL_SET_INVISIBLE(RSAQEXCE).
PERFORM SET_XINT_PARAMS(RSAQEXCE).

INITIALIZATION.
PERFORM INIT_XINT(RSAQEXCE).
PERFORM SET_WWW_FLAGS(RSAQEXCE).
PERFORM INIT_PRINT_PARAMS(RSAQEXCE).

START-OF-SELECTION.

DATA : lt_test TYPE TABLE OF ZTCO_NMHHRTRANS.

SELECT *
       INTO CORRESPONDING FIELDS OF TABLE lt_test
       FROM ZTCO_NMHHRTRANS
       WHERE GJAHR IN SP$00001
         AND PERID IN SP$00002.





INSERT ZTCO_NMHHRTRANS-UNIT INTO %FGWRZTCO_NMHHRTRANS01.
INSERT ZTCO_NMHHRTRANS-ACTQTY INTO %FGWRZTCO_NMHHRTRANS01.
INSERT ZTCO_NMHHRTRANS-CURQTY INTO %FGWRZTCO_NMHHRTRANS01.
INSERT ZTCO_NMHHRTRANS-VAEQTY INTO %FGWRZTCO_NMHHRTRANS01.
INSERT ZTCO_NMHHRTRANS-GJAHR INTO HEADER.
INSERT ZTCO_NMHHRTRANS-PERID INTO HEADER.
INSERT ZTCO_NMHHRTRANS-KOSTL INTO HEADER.
INSERT ZTCO_NMHHRTRANS-LSTAR INTO HEADER.
INSERT %COUNT-ZTCO_NMHHRTRANS INTO HEADER.
INSERT %LINR-ZTCO_NMHHRTRANS INTO HEADER.
INSERT ZTCO_NMHHRTRANS-ACTQTY INTO %FG01.
INSERT ZTCO_NMHHRTRANS-CURQTY INTO %FG01.
INSERT ZTCO_NMHHRTRANS-VAEQTY INTO %FG01.
INSERT ZTCO_NMHHRTRANS-UNIT INTO %FG01.
INSERT ZTCO_NMHHRTRANS-ERDAT INTO %FG01.
INSERT ZTCO_NMHHRTRANS-ERZET INTO %FG01.
INSERT ZTCO_NMHHRTRANS-ERNAM INTO %FG01.
INSERT ZTCO_NMHHRTRANS-AEDAT INTO %FG01.
INSERT ZTCO_NMHHRTRANS-AEZET INTO %FG01.
INSERT ZTCO_NMHHRTRANS-AENAM INTO %FG01.
PERFORM INIT_TEXTHANDLING(RSAQEXCE) USING 'CL_TEXT_IDENTIFIER' ' '
        'SYSTQV000000000000000005'.
PERFORM AUTHORITY_BEGIN(RSAQEXCE) USING 'CL_QUERY_TAB_ACCESS_AUTHORITY'.
PERFORM AUTHORITY(RSAQEXCE) USING 'ZTCO_NMHHRTRANS'
                                  'CL_QUERY_TAB_ACCESS_AUTHORITY'.
PERFORM AUTHORITY_END(RSAQEXCE) USING 'CL_QUERY_TAB_ACCESS_AUTHORITY' .
PERFORM %COMP_LDESC.
SELECT ACTQTY AEDAT AENAM AEZET CURQTY ERDAT ERNAM ERZET GJAHR KOSTL
       LSTAR PERID UNIT VAEQTY
       INTO CORRESPONDING FIELDS OF ZTCO_NMHHRTRANS
       FROM ZTCO_NMHHRTRANS
       WHERE GJAHR IN SP$00001
         AND PERID IN SP$00002.
  %DBACC = %DBACC - 1.
  IF %DBACC = 0.
    STOP.
  ENDIF.
  ADD 1 TO %COUNT-ZTCO_NMHHRTRANS.
  %LINR-ZTCO_NMHHRTRANS = '01'.
  EXTRACT %FG01.
  %EXT-ZTCO_NMHHRTRANS01 = 'X'.
    EXTRACT %FGWRZTCO_NMHHRTRANS01.
ENDSELECT.

END-OF-SELECTION.
SORT AS TEXT BY
        ZTCO_NMHHRTRANS-GJAHR
        ZTCO_NMHHRTRANS-PERID
        ZTCO_NMHHRTRANS-KOSTL
        ZTCO_NMHHRTRANS-LSTAR
        %COUNT-ZTCO_NMHHRTRANS
        %LINR-ZTCO_NMHHRTRANS.
%DIACT = SPACE.
%BATCH = SY-BATCH.
IF %BATCH <> SPACE.
  IF %EIS <> SPACE.
    %DIACT = 'E'.
    IF %EISPROTOCOL = SPACE.
      NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
               LINE-SIZE 0253 LINE-COUNT 0065.
    ELSE.
      NEW-PAGE PRINT ON NO DIALOG
               PARAMETERS %INIT_PRI_PARAMS.
    ENDIF.
  ENDIF.
  IF %ALV <> SPACE.
    %DIACT = 'V'.
    %ALV_LAYOUT = %ALVL.
    NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
             LINE-SIZE 0253 LINE-COUNT 0065.
  ENDIF.
  IF %SAVE <> SPACE.
    %DIACT = 'S'.
    NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
             LINE-SIZE 0253 LINE-COUNT 0065.
  ENDIF.
ELSEIF %CALLED_BY_WWW <> SPACE.
  %DIACT = SPACE.
ELSEIF %CALLED_BY_WWW_ALV <> SPACE.
  %DIACT = 'V'.
ELSE.
  PERFORM INIT_PRINT_PARAMS(RSAQEXCE).
  IF %SAVE  <> SPACE. %DIACT = 'S'. ENDIF.
  IF %XINT  <> SPACE. %DIACT = 'I'. ENDIF.
  IF %TVIEW <> SPACE. %DIACT = 'T'. ENDIF.
  IF %ALV   <> SPACE. %DIACT = 'V'. ENDIF.
  IF %DOWN  <> SPACE. %DIACT = 'D'. ENDIF.
  IF %EIS   <> SPACE. %DIACT = 'E'. ENDIF.
  IF %GRAPH <> SPACE. %DIACT = 'G'. ENDIF.
  IF %EXCEL <> SPACE. %DIACT = 'X'. ENDIF.
  IF %TEXT  <> SPACE. %DIACT = 'W'. ENDIF.
  IF %ABC   <> SPACE. %DIACT = 'A'. ENDIF.
  IF %DIACT <> SPACE AND %DIACT <> 'S' AND %DIACT <> 'W'.
    NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
             LINE-SIZE 0253 LINE-COUNT 0065.
  ENDIF.
  %PATHNAME = %PATH.
  IF %DIACT = 'I'.
    %FUNCTIONKEY = %XINTK.
  ENDIF.
  IF %DIACT = 'V'.
    %ALV_LAYOUT = %ALVL.
  ENDIF.
ENDIF.
FREE MEMORY ID 'AQLISTDATA'.
IF %MEMMODE <> SPACE.
  IF %BATCH <> SPACE.
    NEW-PAGE PRINT ON DESTINATION 'NULL' NO DIALOG
             LINE-SIZE 0253 LINE-COUNT 0065.
  ENDIF.
  %DIACT = '1'.
ENDIF.
%TITEL = ' '.
IF SY-SUBTY O %PRFLAG AND %TITEL = SPACE.
  NEW-PAGE WITH-TITLE.
ENDIF.
%TVSIZE = 0200.
%PLINE = 1.
%PZGR  = 1.
%FIRST = 'X'.
PERFORM %OUTPUT.
%FIRST = SPACE.
IF %DIACT <> SPACE AND %DIACT <> 'S'.
  IF %BATCH = SPACE.
    NEW-PAGE PRINT OFF.
    IF NOT ( %DIACT = 'V' AND %UCOMM = 'PRIN' ).
      NEW-PAGE NO-HEADING NO-TITLE.
      WRITE SPACE.
    ENDIF.
  ENDIF.
ELSE.
  PERFORM PF-STATUS(RSAQEXCE) USING 'XXX X '.
ENDIF.
CLEAR: %TAB, %LINE, %CONT.
IF %DATA_SELECTED = SPACE.
  IF %DIACT = '1'.
    EXPORT EMPTY FROM %EMPTY TO MEMORY ID 'AQLISTDATA'.
    LEAVE.
  ELSE.
    IF %BATCH = SPACE AND
       %CALLED_BY_WWW = SPACE AND
       %CALLED_BY_WWW_ALV = SPACE.
      MESSAGE S260(AQ).
      LEAVE LIST-PROCESSING.
    ELSE.
      IF %CALLED_BY_WWW_ALV = SPACE.
        %DIACT = SPACE.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
IF %DIACT = 'S'.
  PERFORM %SAVE_LIST.
  LEAVE LIST-PROCESSING.
ENDIF.
IF %DIACT = 'V' AND %BATCH <> SPACE.
  NEW-PAGE PRINT OFF.
  PERFORM SET_PRINT_PARAMS(RSAQEXCE).
  PERFORM %DOWNLOAD USING 'ALV'.
  LEAVE.
ENDIF.
IF %DIACT = 'V' AND %CALLED_BY_WWW_ALV <> SPACE.
  PERFORM %DOWNLOAD USING 'ALV'.
  LEAVE.
ENDIF.
IF %DIACT = 'V' AND %UCOMM = 'PRIN'.
  NEW-PAGE PRINT OFF.
  PERFORM SET_PRINT_PARAMS(RSAQEXCE).
  PERFORM %DOWNLOAD USING 'ALV'.
  LEAVE LIST-PROCESSING.
ENDIF.
IF %DIACT = 'P' AND %BATCH <> SPACE.
  PERFORM %DOWNLOAD USING '+DAT'.
  LEAVE LIST-PROCESSING.
ENDIF.
IF %DIACT = 'E' AND %BATCH <> SPACE.
  PERFORM %DOWNLOAD USING 'EIS'.
  LEAVE LIST-PROCESSING.
ENDIF.
IF %DIACT = '1'.
  PERFORM %DOWNLOAD USING '+MEM'.
  LEAVE.
ENDIF.
IF %DIACT = 'X'.
  SET USER-COMMAND 'XXL'.
ELSEIF %DIACT = 'W'.
  SET USER-COMMAND 'TEXT'.
ELSEIF %DIACT = 'V'.
  SET USER-COMMAND 'ALV'.
ELSEIF %DIACT = 'T'.
  SET USER-COMMAND 'VIEW'.
ELSEIF %DIACT = 'G'.
  SET USER-COMMAND 'GRAF'.
ELSEIF %DIACT = 'A'.
  SET USER-COMMAND 'ABCA'.
ELSEIF %DIACT = 'E'.
  SET USER-COMMAND 'EIS'.
ELSEIF %DIACT = 'D'.
  SET USER-COMMAND 'DOWN'.
ELSEIF %DIACT = 'I'.
  SET USER-COMMAND 'XINT'.
ELSEIF %DIACT = 'P'.
  SET USER-COMMAND '+DAT'.
ENDIF.

TOP-OF-PAGE.
PERFORM %TOP-OF-PAGE.

END-OF-PAGE.
PERFORM PAGE_FOOT(RSAQEXCE).
PERFORM %SAVE_PAGE.

TOP-OF-PAGE DURING LINE-SELECTION.
PERFORM %TOP-OF-PAGE.

AT USER-COMMAND.
CASE SY-UCOMM.
WHEN 'EXIT'.
  LEAVE PROGRAM.
WHEN 'RETN'.
  PERFORM RETURN(RSAQEXCE).
WHEN 'CANC'.
  PERFORM RETURN(RSAQEXCE).
WHEN 'WEIT'.
  PERFORM RETURN(RSAQEXCE).
WHEN 'INHA'.
  PERFORM CATALOGUE(RSAQEXCE).
WHEN 'AUSL'.
  PERFORM PICKUP(RSAQEXCE).
WHEN 'AUSW'.
  PERFORM PICKUP(RSAQEXCE).
WHEN 'RCAA'.
  PERFORM RCHAIN(RSAQBRST).
WHEN 'RCAL'.
  PERFORM RCALL(RSAQBRST).
WHEN 'VGLI'.
  PERFORM CHANGE(RSAQEXCE).
WHEN 'VGLE'.
  PERFORM CHANGE(RSAQEXCE).
WHEN 'TOTO'.
  PERFORM CHANGE(RSAQEXCE).
WHEN 'VSTA'.
  PERFORM CHANGE(RSAQEXCE).
WHEN 'VSTE'.
  PERFORM RETURN(RSAQEXCE).
WHEN 'SAVL'.
  PERFORM %SAVE_LIST.
WHEN 'ODRU'.
  PERFORM PRINT_LIST(RSAQEXCE).
WHEN 'COPA'.
  PERFORM PRINT_COVER_PAGE(RSAQEXCE).
WHEN 'TEXT'.
  PERFORM %DOWNLOAD USING 'TEXT'.
WHEN 'ALV'.
  PERFORM %DOWNLOAD USING 'ALV'.
WHEN 'VIEW'.
  PERFORM %VIEW.
WHEN 'XXL'.
  PERFORM %DOWNLOAD USING 'XXL'.
WHEN 'GRAF'.
  PERFORM %DOWNLOAD USING 'GRAF'.
WHEN 'ABCA'.
  PERFORM %DOWNLOAD USING 'ABCA'.
WHEN 'EIS'.
  PERFORM %DOWNLOAD USING 'EIS'.
WHEN 'DOWN'.
  PERFORM %DOWNLOAD USING 'DOWN'.
WHEN 'XINT'.
  PERFORM %DOWNLOAD USING 'XINT'.
ENDCASE.
CLEAR: %CLINE, %ZGR.
CLEAR: %TAB, %LINE, %CONT.
IF %DIACT <> SPACE.
  LEAVE LIST-PROCESSING.
ENDIF.


FORM %COMP_LDESC.

  REFRESH %LDESC.
  REFRESH %GDESC.
  PERFORM LDESC(RSAQEXCE) USING 'G00010000X004       01 X98'
    TEXT-A00 TEXT-B00 TEXT-H00 'ZTCO_NMHHRTRANS-GJAHR'
    ZTCO_NMHHRTRANS-GJAHR 'ZTCO_NMHHRTRANS-GJAHR'.
  PERFORM LDESC(RSAQEXCE) USING 'G00020000X003       02 X98'
    TEXT-A01 TEXT-B01 TEXT-H00 'ZTCO_NMHHRTRANS-PERID'
    ZTCO_NMHHRTRANS-PERID 'ZTCO_NMHHRTRANS-PERID'.
  PERFORM LDESC(RSAQEXCE) USING 'G00030000X010       03  98'
    TEXT-A02 TEXT-B02 TEXT-H00 'ZTCO_NMHHRTRANS-KOSTL'
    ZTCO_NMHHRTRANS-KOSTL 'ZTCO_NMHHRTRANS-KOSTL'.
  PERFORM LDESC(RSAQEXCE) USING 'G00040000X006       04  98'
    TEXT-A03 TEXT-B03 TEXT-H00 'ZTCO_NMHHRTRANS-LSTAR'
    ZTCO_NMHHRTRANS-LSTAR 'ZTCO_NMHHRTRANS-LSTAR'.
  PERFORM LDESC(RSAQEXCE) USING 'G00050029 020     X 00  98'
    TEXT-A04 TEXT-B04 TEXT-H00 'ZTCO_NMHHRTRANS-ACTQTY'
    ZTCO_NMHHRTRANS-ACTQTY 'ZTCO_NMHHRTRANS-ACTQTY'.
  PERFORM LDESC(RSAQEXCE) USING 'G00060050 020     X 00  98'
    TEXT-A05 TEXT-B05 TEXT-H00 'ZTCO_NMHHRTRANS-CURQTY'
    ZTCO_NMHHRTRANS-CURQTY 'ZTCO_NMHHRTRANS-CURQTY'.
  PERFORM LDESC(RSAQEXCE) USING 'G00070071 020M    X 00  98'
    TEXT-A06 TEXT-B06 TEXT-H00 'ZTCO_NMHHRTRANS-VAEQTY'
    ZTCO_NMHHRTRANS-VAEQTY 'ZTCO_NMHHRTRANS-VAEQTY'.
  PERFORM LDESC(RSAQEXCE) USING 'G00080000X003E      00  98'
    TEXT-A07 TEXT-B07 TEXT-H00 'ZTCO_NMHHRTRANS-UNIT'
    ZTCO_NMHHRTRANS-UNIT 'ZTCO_NMHHRTRANS-UNIT'.
  PERFORM LDESC(RSAQEXCE) USING 'G00090000X010       00  98'
    TEXT-A08 TEXT-B08 TEXT-H00 'ZTCO_NMHHRTRANS-ERDAT'
    ZTCO_NMHHRTRANS-ERDAT 'ZTCO_NMHHRTRANS-ERDAT'.
  PERFORM LDESC(RSAQEXCE) USING 'G00100000X008       00  98'
    TEXT-A09 TEXT-B09 TEXT-H00 'ZTCO_NMHHRTRANS-ERZET'
    ZTCO_NMHHRTRANS-ERZET 'ZTCO_NMHHRTRANS-ERZET'.
  PERFORM LDESC(RSAQEXCE) USING 'G00110000X012       00  98'
    TEXT-A10 TEXT-B10 TEXT-H00 'ZTCO_NMHHRTRANS-ERNAM'
    ZTCO_NMHHRTRANS-ERNAM 'ZTCO_NMHHRTRANS-ERNAM'.
  PERFORM LDESC(RSAQEXCE) USING 'G00120000X010       00  98'
    TEXT-A11 TEXT-B11 TEXT-H00 'ZTCO_NMHHRTRANS-AEDAT'
    ZTCO_NMHHRTRANS-AEDAT 'ZTCO_NMHHRTRANS-AEDAT'.
  PERFORM LDESC(RSAQEXCE) USING 'G00130000X008       00  98'
    TEXT-A12 TEXT-B12 TEXT-H00 'ZTCO_NMHHRTRANS-AEZET'
    ZTCO_NMHHRTRANS-AEZET 'ZTCO_NMHHRTRANS-AEZET'.
  PERFORM LDESC(RSAQEXCE) USING 'G00140000X012       00  98'
    TEXT-A13 TEXT-B13 TEXT-H00 'ZTCO_NMHHRTRANS-AENAM'
    ZTCO_NMHHRTRANS-AENAM 'ZTCO_NMHHRTRANS-AENAM'.
  PERFORM GDESC(RSAQEXCE) USING 'G00' 5 20 ' ' ' ' 'X'.

ENDFORM.

FORM %OUTPUT.

DESCRIBE TABLE %PRLIST LINES %MAX_PRLIST.
%HEAD = 'AAA'.
%KEYEMPTY = SPACE.
NEW-PAGE.
PERFORM %OUTPUT_GL.
PERFORM COMPLETE_PAGE(RSAQEXCE).
%HEAD = 'ZZZ'.
PERFORM LAST_PTAB_ENTRY(RSAQEXCE).
NEW-PAGE.
IF %KEYEMPTY <> SPACE.
  MESSAGE S894(AQ).
ENDIF.

ENDFORM.


FORM %TOP-OF-PAGE.

IF SY-UCOMM = 'INHA'. EXIT. ENDIF.
IF SY-UCOMM = 'COPA'. EXIT. ENDIF.
IF %HEAD    = SPACE.  EXIT. ENDIF.
IF %HEAD = 'DDD'.
  PERFORM TVIEWPAGE(RSAQEXCE).
  EXIT.
ENDIF.
IF %HEAD = 'GGG'.
  PERFORM PAGE(RSAQEXCE) USING 'G00' TEXT-GRL 252 %GLFRAME 003.
  SET LEFT SCROLL-BOUNDARY COLUMN 002.
  PERFORM SET_SCROLL_BOUNDARY(RSAQEXCE) USING 002.
  IF %TOTO <> SPACE. EXIT. ENDIF.
ELSE.
  CASE %HEAD.
  ENDCASE.
ENDIF.

ENDFORM.


FORM %NEWLINE.

  %UFLAG = SPACE.
  NEW-LINE.
  WRITE: '|', 252 '|'.
  POSITION 2.

ENDFORM.

FORM %SKIP USING COUNT.

  IF SY-LINNO > 1.
    %UFLAG = SPACE.
    DO COUNT TIMES.
      NEW-LINE.
      FORMAT RESET.
      WRITE: '|', 252 '|'.
    ENDDO.
  ENDIF.

ENDFORM.

FORM %ULINE.

  IF %UFLAG = SPACE.
    IF SY-LINNO > 1.
      ULINE /1(252).
    ENDIF.
    %UFLAG = 'X'.
  ENDIF.

ENDFORM.

FORM %HIDE.

  IF %BATCH <> SPACE AND %DIACT = 'S'.
    PERFORM HIDE(RSAQEXCE).
  ELSE.
    HIDE: %TAB, %LINE, %CONT.
  ENDIF.

ENDFORM.

FORM %HIDE_COLOR.

  IF %BATCH <> SPACE AND %DIACT = 'S'.
    PERFORM HIDE_COLOR(RSAQEXCE).
  ELSE.
    HIDE: %FINT, %FCOL.
  ENDIF.

ENDFORM.

FORM %RCALL USING NAME VALUE.

FIELD-SYMBOLS <FIELD>.

  ASSIGN (NAME) TO <FIELD>.
  READ CURRENT LINE FIELD VALUE <FIELD> INTO VALUE.
  IF SY-SUBRC <> 0.
    VALUE = SPACE.
    EXIT.
  ENDIF.
  IF VALUE = SPACE AND %TAB = 'G00' AND %LDESC-FCUR NA 'FM'.
    READ TABLE %G00 INDEX %LINE.
    IF SY-SUBRC = 0.
      ASSIGN COMPONENT %LDESC-FNAMEINT OF STRUCTURE %G00
                                       TO <FIELD>.
      IF SY-SUBRC = 0.
        WRITE <FIELD> TO VALUE(%LDESC-FOLEN).
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

FORM %SAVE_PAGE.

  IF %BATCH <> SPACE AND %DIACT = 'S'.
    PERFORM SAVE_PAGE(RSAQEXCE) TABLES %ST_LISTE.
  ENDIF.

ENDFORM.

FORM %REPLACE_VAR USING TEXT.

FIELD-SYMBOLS <VAR>.

  ASSIGN TEXT+1(*) TO <VAR>.

ENDFORM.

FORM %SAVE_LIST.

DATA: %SFLAG,
      QREPORT LIKE SY-REPID.

  IF %DIACT = 'S'. %SFLAG = 'X'. ENDIF.
  QREPORT = SY-REPID.
  PERFORM SAVE_LIST(RSAQEXCE) TABLES %ST_LISTE
                              USING QREPORT %SFLAG %LISTID.
  IF %QL_ID <> SPACE.
    %DLFLAG = 'X'.
    %LISTSIZE = 0253.
    PERFORM COMP_SELECTION_SCREEN(RSAQEXCE).
    EXPORT %ST_LISTE %PTAB %LDESC %GDESC %DLFLAG %LISTSIZE
           %SELECTIONS
           %G00
           TO DATABASE AQLDB(AQ) ID %QL_ID.
  ENDIF.

ENDFORM.

FORM %REFRESH.

  CASE %TAB.
  WHEN 'G00'.
    IMPORT %G00 FROM DATABASE AQLDB(AQ) ID %QL_ID.
  ENDCASE.

ENDFORM.

FORM %DOWNLOAD USING CODE.

DATA: QREPORT LIKE SY-REPID.

  PERFORM INIT_DOWNLOAD(RSAQEXCE).
  QREPORT = SY-REPID.
  CASE %TAB.
  WHEN 'G00'.
    PERFORM DOWNLOAD(RSAQEXCE)
            TABLES %G00 USING CODE QREPORT TEXT-GRL.
  WHEN OTHERS.
    MESSAGE S860(AQ).
  ENDCASE.

ENDFORM.

FORM %SET_DATA CHANGING L_LINES TYPE I.

  IMPORT LDATA TO %G00 FROM MEMORY ID 'AQLISTDATA'.
  DESCRIBE TABLE %G00 LINES L_LINES.
  FREE MEMORY ID 'AQLISTDATA'.

ENDFORM.

FORM %GET_DATA TABLES DATATAB STRUCTURE %G00
               USING  FIRST TYPE I
                      LAST  TYPE I.

  APPEND LINES OF %G00 FROM FIRST TO LAST TO DATATAB.

ENDFORM.

FORM %GET_REF_TO_TABLE USING LID         LIKE RSAQLDESC-LID
                             REF_TO_ITAB TYPE REF TO DATA
                             SUBRC       LIKE SY-SUBRC.

  SUBRC = 0.
  CASE LID.
  WHEN 'G00'.
    CREATE DATA REF_TO_ITAB LIKE %G00[].
  WHEN OTHERS.
    SUBRC = 4.
    MESSAGE S860(AQ).
  ENDCASE.

ENDFORM.

FORM %VIEW.

DATA: ANZ TYPE I,
      PROG LIKE SY-REPID.

  PROG = SY-REPID.
  PERFORM INIT_DOWNLOAD(RSAQEXCE).
  CASE %TAB.
  WHEN 'G00'.
    PERFORM GENERATE_VIEW_DYNPRO(RSAQEXCE)
            USING PROG TEXT-GRL.
    DESCRIBE TABLE %G00 LINES ANZ.
    TVIEW100-LINES = ANZ.
    PERFORM INIT_VIEW(RSAQEXCE) TABLES %G00 USING TVIEW100.
    CALL SCREEN 100.
    PERFORM RESET_VIEW_DYNPRO(RSAQEXCE).
  WHEN OTHERS.
    MESSAGE S860(AQ).
  ENDCASE.

ENDFORM.


FORM %OUTPUT_GL.

IF %MAX_PRLIST <> 0.
  READ TABLE %PRLIST WITH KEY TAB = 'GGG'.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.
ENDIF.
SET MARGIN 00.
PERFORM COMPLETE_PAGE(RSAQEXCE).
%NOCHANGE = SPACE.
NEW-PAGE.
REFRESH %WA010.
REFRESH %W0100.
REFRESH %W0100.
REFRESH %W0100.
REFRESH %W0101.
REFRESH %W0101.
REFRESH %W0101.
REFRESH %W0103.
REFRESH %W0103.
REFRESH %W0103.
REFRESH %W0104.
REFRESH %W0104.
REFRESH %W0104.
REFRESH %W0102.
REFRESH %W0102.
REFRESH %W0102.
CLEAR %%ZTCO_NMHHRTRANS-GJAHR.
CLEAR %%ZTCO_NMHHRTRANS-PERID.
%GLLINE   = 0.
%TAB      = 'G00'.
%LINE     = 0.
%CONT     = '0'.
%FINT     = SPACE.
%FCOL     = '0'.
%HEAD     = 'GGG'.
%CLINE    = 0.
%OUTFLAG  = SPACE.
%OUTCOMP  = SPACE.
%OUTTOTAL = SPACE.
%RFLAG    = 'AA'.
IF %DIACT <> SPACE AND %DIACT NA 'SWE'. WRITE SPACE. ENDIF.
FORMAT RESET.
LOOP.
  %DATA_SELECTED = 'X'.
  AT %FG01.
    %ZNR = '01'.
    %ZGR = '01'.
    %CLINE = %CLINE + 1.
    %G00-ZTCO_NMHHRTRANS-GJAHR = ZTCO_NMHHRTRANS-GJAHR.
    %G00-ZTCO_NMHHRTRANS-PERID = ZTCO_NMHHRTRANS-PERID.
    %G00-ZTCO_NMHHRTRANS-KOSTL = ZTCO_NMHHRTRANS-KOSTL.
    %G00-ZTCO_NMHHRTRANS-LSTAR = ZTCO_NMHHRTRANS-LSTAR.
    %G00-ZTCO_NMHHRTRANS-ACTQTY = ZTCO_NMHHRTRANS-ACTQTY.
    %G00-ZTCO_NMHHRTRANS-CURQTY = ZTCO_NMHHRTRANS-CURQTY.
    %G00-ZTCO_NMHHRTRANS-VAEQTY = ZTCO_NMHHRTRANS-VAEQTY.
    %G00-ZTCO_NMHHRTRANS-UNIT = ZTCO_NMHHRTRANS-UNIT.
    %G00-ZTCO_NMHHRTRANS-ERDAT = ZTCO_NMHHRTRANS-ERDAT.
    %G00-ZTCO_NMHHRTRANS-ERZET = ZTCO_NMHHRTRANS-ERZET.
    %G00-ZTCO_NMHHRTRANS-ERNAM = ZTCO_NMHHRTRANS-ERNAM.
    %G00-ZTCO_NMHHRTRANS-AEDAT = ZTCO_NMHHRTRANS-AEDAT.
    %G00-ZTCO_NMHHRTRANS-AEZET = ZTCO_NMHHRTRANS-AEZET.
    %G00-ZTCO_NMHHRTRANS-AENAM = ZTCO_NMHHRTRANS-AENAM.
    IF %FIRST <> SPACE. APPEND %G00. ENDIF.
    %GLLINE = %GLLINE + 1.
    %LZNR = %ZNR.
    IF %DIACT <> SPACE AND %DIACT NA 'SWE'. CONTINUE. ENDIF.
    PERFORM CHECK(RSAQEXCE) USING ' '.
    IF %RFLAG = 'E'. EXIT. ENDIF.
    %GROUP0101 = 'X'.
    %GROUP01 = 'X'.
    %GROUP0102 = 'X'.
    %GROUP02 = 'X'.
IF ZTCO_NMHHRTRANS-GJAHR <> %%ZTCO_NMHHRTRANS-GJAHR OR
%%%ZTCO_NMHHRTRANS-GJAHR = SPACE.
      %%ZTCO_NMHHRTRANS-GJAHR = ZTCO_NMHHRTRANS-GJAHR.
      %%%ZTCO_NMHHRTRANS-GJAHR ='X'.
      CLEAR %%ZTCO_NMHHRTRANS-PERID.
      CLEAR %%%ZTCO_NMHHRTRANS-PERID.
      CLEAR %%ZTCO_NMHHRTRANS-KOSTL.
      CLEAR %%%ZTCO_NMHHRTRANS-KOSTL.
      CLEAR %%ZTCO_NMHHRTRANS-LSTAR.
      CLEAR %%%ZTCO_NMHHRTRANS-LSTAR.
      IF %RFLAG = 'AA'.
      PERFORM %ULINE.
      FORMAT RESET.
      FORMAT INTENSIFIED ON COLOR 7.
      %FINT = 'N'. %FCOL = '7'.
      PERFORM %NEWLINE.
      %GRST_TEXT = TEXT-G01.
      PERFORM GRST(RSAQEXCE) USING ZTCO_NMHHRTRANS-GJAHR %GRST_TEXT.
      PERFORM %HIDE.
      PERFORM %HIDE_COLOR.
      PERFORM %ULINE.
      ENDIF.
    ENDIF.
IF ZTCO_NMHHRTRANS-PERID <> %%ZTCO_NMHHRTRANS-PERID OR
%%%ZTCO_NMHHRTRANS-PERID = SPACE.
      %%ZTCO_NMHHRTRANS-PERID = ZTCO_NMHHRTRANS-PERID.
      %%%ZTCO_NMHHRTRANS-PERID ='X'.
      CLEAR %%ZTCO_NMHHRTRANS-KOSTL.
      CLEAR %%%ZTCO_NMHHRTRANS-KOSTL.
      CLEAR %%ZTCO_NMHHRTRANS-LSTAR.
      CLEAR %%%ZTCO_NMHHRTRANS-LSTAR.
      IF %RFLAG = 'AA'.
      PERFORM %ULINE.
      FORMAT RESET.
      FORMAT INTENSIFIED ON COLOR 7.
      %FINT = 'N'. %FCOL = '7'.
      PERFORM %NEWLINE.
      %GRST_TEXT = TEXT-G02.
      PERFORM GRST(RSAQEXCE) USING ZTCO_NMHHRTRANS-PERID %GRST_TEXT.
      PERFORM %HIDE.
      PERFORM %HIDE_COLOR.
      PERFORM %ULINE.
      ENDIF.
    ENDIF.
IF ZTCO_NMHHRTRANS-KOSTL <> %%ZTCO_NMHHRTRANS-KOSTL OR
%%%ZTCO_NMHHRTRANS-KOSTL = SPACE.
      %%ZTCO_NMHHRTRANS-KOSTL = ZTCO_NMHHRTRANS-KOSTL.
      %%%ZTCO_NMHHRTRANS-KOSTL ='X'.
      CLEAR %%ZTCO_NMHHRTRANS-LSTAR.
      CLEAR %%%ZTCO_NMHHRTRANS-LSTAR.
    ENDIF.
IF ZTCO_NMHHRTRANS-LSTAR <> %%ZTCO_NMHHRTRANS-LSTAR OR
%%%ZTCO_NMHHRTRANS-LSTAR = SPACE.
      %%ZTCO_NMHHRTRANS-LSTAR = ZTCO_NMHHRTRANS-LSTAR.
      %%%ZTCO_NMHHRTRANS-LSTAR ='X'.
    ENDIF.
    IF %RFLAG(1) = 'A'.
    FORMAT RESET.
    %FINT = 'F'. %FCOL = '0'.
    FORMAT COLOR 2. %FCOL = '2'.
    PERFORM %NEWLINE.
    WRITE 002(004) ZTCO_NMHHRTRANS-GJAHR.
    %LINE = %GLLINE.
    PERFORM %HIDE.
    %LINE = 0.
    PERFORM %HIDE_COLOR.
    WRITE 007(003) ZTCO_NMHHRTRANS-PERID.
    WRITE 011(010) ZTCO_NMHHRTRANS-KOSTL.
    WRITE 022(006) ZTCO_NMHHRTRANS-LSTAR.
    WRITE 029(020) ZTCO_NMHHRTRANS-ACTQTY
      UNIT ZTCO_NMHHRTRANS-UNIT.
    WRITE 050(020) ZTCO_NMHHRTRANS-CURQTY
      UNIT ZTCO_NMHHRTRANS-UNIT.
    WRITE 071(020) ZTCO_NMHHRTRANS-VAEQTY
      UNIT ZTCO_NMHHRTRANS-UNIT.
    WRITE 092(003) ZTCO_NMHHRTRANS-UNIT.
    WRITE 096(010) ZTCO_NMHHRTRANS-ERDAT.
    WRITE 107(008) ZTCO_NMHHRTRANS-ERZET.
    WRITE 116(012) ZTCO_NMHHRTRANS-ERNAM.
    WRITE 129(010) ZTCO_NMHHRTRANS-AEDAT.
    WRITE 140(008) ZTCO_NMHHRTRANS-AEZET.
    WRITE 149(012) ZTCO_NMHHRTRANS-AENAM.
    ENDIF.
  ENDAT.
  AT %FGWRZTCO_NMHHRTRANS01.
    CLEAR %W0104.
    %W0104-ZTCO_NMHHRTRANS-UNIT = ZTCO_NMHHRTRANS-UNIT.
    %W0104-ZTCO_NMHHRTRANS-ACTQTY = ZTCO_NMHHRTRANS-ACTQTY.
    %W0104-ZTCO_NMHHRTRANS-CURQTY = ZTCO_NMHHRTRANS-CURQTY.
    %W0104-ZTCO_NMHHRTRANS-VAEQTY = ZTCO_NMHHRTRANS-VAEQTY.
    COLLECT %W0104.
  ENDAT.
  AT END OF ZTCO_NMHHRTRANS-LSTAR.
    %ZGR = '01'.
    PERFORM CHECK(RSAQEXCE) USING 'X'.
    IF %RFLAG = 'E'. EXIT. ENDIF.
    LOOP AT %W0104.
      %W0103 = %W0104.
      COLLECT %W0103.
    ENDLOOP.
    REFRESH %W0104.
  ENDAT.
  AT END OF ZTCO_NMHHRTRANS-KOSTL.
    %ZGR = '01'.
    PERFORM CHECK(RSAQEXCE) USING 'X'.
    IF %RFLAG = 'E'. EXIT. ENDIF.
    LOOP AT %W0103.
      %W0102 = %W0103.
      COLLECT %W0102.
    ENDLOOP.
    REFRESH %W0103.
  ENDAT.
  AT END OF ZTCO_NMHHRTRANS-PERID.
    %ZGR = '01'.
    PERFORM CHECK(RSAQEXCE) USING 'X'.
    IF %RFLAG = 'E'. EXIT. ENDIF.
    IF %GROUP02 <> SPACE.
      CLEAR %GROUP02.
      IF %RFLAG = 'AA'.
      PERFORM RESERVE(RSAQEXCE) USING 2.
      PERFORM %ULINE.
      FORMAT RESET.
      FORMAT INTENSIFIED OFF COLOR 3.
      %FINT = 'F'. %FCOL = '3'.
      PERFORM %NEWLINE.
      %GRST_TEXT = TEXT-Z02.
      PERFORM GRST(RSAQEXCE) USING ZTCO_NMHHRTRANS-PERID %GRST_TEXT.
      PERFORM %HIDE.
      PERFORM %HIDE_COLOR.
      ENDIF.
    ENDIF.
    SORT %W0102 BY ZTCO_NMHHRTRANS-UNIT.
    IF %GROUP0102 <> SPACE.
      CLEAR %GROUP0102.
      IF %RFLAG = 'AA'.
      WRITE 096 '*'.
      REFRESH %WA010.
      DO.
        %SUBRC = 4.
        CLEAR %WA010.
        READ TABLE %W0102 INDEX SY-INDEX.
        IF SY-SUBRC = 0.
          %SUBRC = 0.
          %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT =
          %W0102-ZTCO_NMHHRTRANS-UNIT.
          %WA010-ZTCO_NMHHRTRANS-ACTQTY = %W0102-ZTCO_NMHHRTRANS-ACTQTY.
          %WA010-ZTCO_NMHHRTRANS-CURQTY = %W0102-ZTCO_NMHHRTRANS-CURQTY.
          %WA010-ZTCO_NMHHRTRANS-VAEQTY = %W0102-ZTCO_NMHHRTRANS-VAEQTY.
        ENDIF.
        IF %SUBRC = 4.
          EXIT.
        ENDIF.
        APPEND %WA010.
      ENDDO.
      LOOP AT %WA010.
        IF SY-TABIX <> 1.
          PERFORM %NEWLINE.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 029(020) %WA010-ZTCO_NMHHRTRANS-ACTQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-ACTQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 029(020) %WA010-ZTCO_NMHHRTRANS-ACTQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 050(020) %WA010-ZTCO_NMHHRTRANS-CURQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-CURQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 050(020) %WA010-ZTCO_NMHHRTRANS-CURQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 071(020) %WA010-ZTCO_NMHHRTRANS-VAEQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-VAEQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 071(020) %WA010-ZTCO_NMHHRTRANS-VAEQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        WRITE 092(003) %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        PERFORM %HIDE.
        PERFORM %HIDE_COLOR.
      ENDLOOP.
      ENDIF.
    ENDIF.
    IF %RFLAG = 'AA'. PERFORM %ULINE. ENDIF.
    LOOP AT %W0102.
      %W0101 = %W0102.
      COLLECT %W0101.
    ENDLOOP.
    REFRESH %W0102.
    IF %RFLAG = 'AA'. PERFORM %SKIP USING 1. ENDIF.
  ENDAT.
  AT END OF ZTCO_NMHHRTRANS-GJAHR.
    %ZGR = '01'.
    PERFORM CHECK(RSAQEXCE) USING 'X'.
    IF %RFLAG = 'E'. EXIT. ENDIF.
    IF %GROUP01 <> SPACE.
      CLEAR %GROUP01.
      IF %RFLAG = 'AA'.
      PERFORM RESERVE(RSAQEXCE) USING 2.
      PERFORM %ULINE.
      FORMAT RESET.
      FORMAT INTENSIFIED OFF COLOR 3.
      %FINT = 'F'. %FCOL = '3'.
      PERFORM %NEWLINE.
      %GRST_TEXT = TEXT-Z01.
      PERFORM GRST(RSAQEXCE) USING ZTCO_NMHHRTRANS-GJAHR %GRST_TEXT.
      PERFORM %HIDE.
      PERFORM %HIDE_COLOR.
      ENDIF.
    ENDIF.
    SORT %W0101 BY ZTCO_NMHHRTRANS-UNIT.
    IF %GROUP0101 <> SPACE.
      CLEAR %GROUP0101.
      IF %RFLAG = 'AA'.
      WRITE 096 '**'.
      REFRESH %WA010.
      DO.
        %SUBRC = 4.
        CLEAR %WA010.
        READ TABLE %W0101 INDEX SY-INDEX.
        IF SY-SUBRC = 0.
          %SUBRC = 0.
          %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT =
          %W0101-ZTCO_NMHHRTRANS-UNIT.
          %WA010-ZTCO_NMHHRTRANS-ACTQTY = %W0101-ZTCO_NMHHRTRANS-ACTQTY.
          %WA010-ZTCO_NMHHRTRANS-CURQTY = %W0101-ZTCO_NMHHRTRANS-CURQTY.
          %WA010-ZTCO_NMHHRTRANS-VAEQTY = %W0101-ZTCO_NMHHRTRANS-VAEQTY.
        ENDIF.
        IF %SUBRC = 4.
          EXIT.
        ENDIF.
        APPEND %WA010.
      ENDDO.
      LOOP AT %WA010.
        IF SY-TABIX <> 1.
          PERFORM %NEWLINE.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 029(020) %WA010-ZTCO_NMHHRTRANS-ACTQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-ACTQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 029(020) %WA010-ZTCO_NMHHRTRANS-ACTQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 050(020) %WA010-ZTCO_NMHHRTRANS-CURQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-CURQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 050(020) %WA010-ZTCO_NMHHRTRANS-CURQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 071(020) %WA010-ZTCO_NMHHRTRANS-VAEQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-VAEQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 071(020) %WA010-ZTCO_NMHHRTRANS-VAEQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        WRITE 092(003) %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        PERFORM %HIDE.
        PERFORM %HIDE_COLOR.
      ENDLOOP.
      ENDIF.
    ENDIF.
    IF %RFLAG = 'AA'. PERFORM %ULINE. ENDIF.
    LOOP AT %W0101.
      %W0100 = %W0101.
      COLLECT %W0100.
    ENDLOOP.
    REFRESH %W0101.
    IF %RFLAG = 'AA'. PERFORM %SKIP USING 1. ENDIF.
  ENDAT.
  AT LAST.
    %ZNR = 0.
    %RFLAG = 'AA'.
    %OUTTOTAL = 'X'.
    PERFORM RESERVE(RSAQEXCE) USING 2.
    PERFORM %ULINE.
    FORMAT RESET.
    FORMAT INTENSIFIED ON COLOR 3.
    %FINT = 'N'. %FCOL = '3'.
    %NOCHANGE = 'X'.
    PERFORM %NEWLINE.
    %NOCHANGE = SPACE.
    WRITE (13) TEXT-F02.
    PERFORM %HIDE.
    PERFORM %HIDE_COLOR.
    SORT %W0100 BY ZTCO_NMHHRTRANS-UNIT.
      WRITE 096 '***'.
      REFRESH %WA010.
      DO.
        %SUBRC = 4.
        CLEAR %WA010.
        READ TABLE %W0100 INDEX SY-INDEX.
        IF SY-SUBRC = 0.
          %SUBRC = 0.
          %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT =
          %W0100-ZTCO_NMHHRTRANS-UNIT.
          %WA010-ZTCO_NMHHRTRANS-ACTQTY = %W0100-ZTCO_NMHHRTRANS-ACTQTY.
          %WA010-ZTCO_NMHHRTRANS-CURQTY = %W0100-ZTCO_NMHHRTRANS-CURQTY.
          %WA010-ZTCO_NMHHRTRANS-VAEQTY = %W0100-ZTCO_NMHHRTRANS-VAEQTY.
        ENDIF.
        IF %SUBRC = 4.
          EXIT.
        ENDIF.
        APPEND %WA010.
      ENDDO.
      LOOP AT %WA010.
        IF SY-TABIX <> 1.
          PERFORM %NEWLINE.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 029(020) %WA010-ZTCO_NMHHRTRANS-ACTQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-ACTQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 029(020) %WA010-ZTCO_NMHHRTRANS-ACTQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 050(020) %WA010-ZTCO_NMHHRTRANS-CURQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-CURQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 050(020) %WA010-ZTCO_NMHHRTRANS-CURQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        IF %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT = SPACE.
          WRITE 071(020) %WA010-ZTCO_NMHHRTRANS-VAEQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT NO-ZERO
.
          IF NOT %WA010-ZTCO_NMHHRTRANS-VAEQTY IS INITIAL.
            %KEYEMPTY = 'X'.
          ENDIF.
        ELSE.
          WRITE 071(020) %WA010-ZTCO_NMHHRTRANS-VAEQTY
                UNIT %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        ENDIF.
        WRITE 092(003) %WA010-ZTCO_NMHHRTRANS-ZTCO_NMHHRTRANS-UNIT.
        PERFORM %HIDE.
        PERFORM %HIDE_COLOR.
      ENDLOOP.
    IF %RFLAG = 'AA'. PERFORM %ULINE. ENDIF.
  ENDAT.
ENDLOOP.
%RFLAG = 'AA'.
PERFORM %ULINE.
CLEAR: %CLINE, %ZGR.

ENDFORM.



MODULE %INIT_VIEW OUTPUT.

  CASE %TAB.
  WHEN 'G00'.
    PERFORM INIT_PBO(RSAQEXCE) TABLES %G00 USING TVIEW100 'X'.
  WHEN OTHERS.
    MESSAGE S860(AQ).
  ENDCASE.

ENDMODULE.

MODULE %PBO_VIEW OUTPUT.

  CASE %TAB.
  WHEN 'G00'.
    PERFORM LOOP_PBO(RSAQEXCE) TABLES %G00 USING %%G00 TVIEW100.
  ENDCASE.

ENDMODULE.

MODULE %PAI_VIEW INPUT.

  CASE %TAB.
  WHEN 'G00'.
    PERFORM LOOP_PAI(RSAQEXCE) TABLES %G00 USING %%G00 TVIEW100.
  ENDCASE.

ENDMODULE.

MODULE %OKCODE_VIEW INPUT.

  CASE %TAB.
  WHEN 'G00'.
    PERFORM OKCODE(RSAQEXCE) TABLES %G00 USING TVIEW100.
  ENDCASE.

ENDMODULE.
