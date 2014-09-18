REPORT RFEBKA00 MESSAGE-ID FB
                 LINE-SIZE 132
                 NO STANDARD PAGE HEADING.
*&====================================================================&*
*&    PROGRAM ZTREDI01       ( FROM RFEBKA00 )                        &*
*&====================================================================&*

DATA: BEGIN OF iftab   OCCURS 0,
        AZDAT(8) TYPE C,
        BANKL    LIKE T012-BANKL   ,
        BANKN    LIKE T012K-BANKN  ,    " ACCOUNT NO.
        CODE1(2) type c,
        CODE2(2) type c,
        VGEXT    LIKE FEBEP-VGEXT  ,
        BUtxt    LIKE FEBEP-BUtxt  ,
        KWBTR    LIKE FEBEP-KWBTR  ,    " AMOUNT
        CODE3(10) type c,
        INFO1    like febep-INFO1  ,    " checkno, ...
        info2    like febep-info2  ,
        KWAER    LIKE FEBEP-KWAER  ,    " CURRENCY
    END OF iftab.

DATA: BEGIN OF itab   OCCURS 0,
        BANKL    LIKE T012-BANKL   ,    " BANK KEY
        BANKN    LIKE T012K-BANKN  ,    " ACCOUNT NO.
        AZDAT(8) TYPE C,
        VGEXT    LIKE FEBEP-VGEXT  ,
        BUtxt    LIKE FEBEP-BUtxt  ,
        KWBTR    LIKE FEBEP-KWBTR  ,    " AMOUNT
        INFO1    like febep-INFO1  ,    " checkno, ...
        info2    like febep-info2  ,
        KWAER    LIKE FEBEP-KWAER  ,    " CURRENCY
    END OF itab.

data: BEGIN OF i_febep occurs 0,
        AZDAT(8) TYPE C,
        BANKL    LIKE T012-BANKL   ,    " BANK KEY
        BANKN    LIKE T012K-BANKN  .    " ACCOUNT NO.
        include structure febep.
data: end of i_febep.

data: BEGIN OF i_febko occurs 0,
*       basic information
        BANKL    LIKE T012-BANKL   ,    " BANK KEY
        BANKN    LIKE T012K-BANKN  ,    " ACCOUNT NO.
        KUKEY    LIKE FEBKO-KUKEY  ,    " STAT.NO
        BANKS    LIKE T012-BANKS   ,    " BANK country

        AZDAT    LIKE FEBKO-AZDAT,      " STAT.DATE
        WAERS    LIKE FEBKO-WAERS,      " CURRENCY
        SSBTR    LIKE FEBKO-SSBTR  ,    " OPENING
        SUMSO    LIKE FEBKO-SUMSO  ,    " DEBIT  Total
        SUMHA    LIKE FEBKO-SUMHA  ,    " CREDIT Total
        ESBTR    LIKE FEBKO-ESBTR  ,    " Closing
        ANZES    LIKE FEBKO-ANZES  ,    " NO of RECORDS

*       additional information
        ABSND    LIKE FEBKO-ABSND  ,    " KEY
        AZIDT    LIKE FEBKO-AZIDT  ,    " fiscal year + state no
        EMKEY    LIKE FEBKO-EMKEY  ,    " receipient

        HBKID    LIKE T012K-HBKID  ,    " HOUSE BANK
        HKTID    LIKE T012K-HKTID  ,    " HOUSE ACCOUNT
        HKONT    LIKE T012K-HKONT  ,    " G/L
        VGTYP    LIKE FEBKO-VGTYP  ,    " TYPE
      END OF i_febko.

DATA: BEGIN OF i_febcl OCCURS 40,
        KUKEY      LIKE FEBKO-KUKEY  ,    " STAT.NO
        ESNUM      LIKE FEBEP-ESNUM  ,    " Lineitem #
        CSNUM      LIKE FEBCL-CSNUM  ,    " seq #
        BANKS      LIKE T012-BANKS   ,    " BANK KEY
        BANKN      LIKE T012K-BANKN  ,    " ACCOUNT NO.

        SELFD      LIKE FEBCL-SELFD  ,    " selection field
        SELVON     LIKE FEBCL-SELVON ,    " from value
        SELBIS     LIKE FEBCL-SELBIS ,    " to value

        CLRREF     LIKE FEBEP-XBLNR  ,    " Clearing ref < invoice
        CLRAMT     LIKE FEBEP-KWBTR  ,    " Clearing Amt < adjust
      END   OF i_febcl          .


DATA: BEGIN OF IUNIX  OCCURS 10,
        TEXT(300) TYPE C,
      END OF IUNIX .

DATA: BEGIN OF P_ILOG OCCURS 100,
        A(50) ,
        CHECK(1),
      END   OF P_ILOG.
DATA: ILOOPC  LIKE SY-LOOPC,
      TOT_CNT TYPE I,
      TINDEX(1).
CONTROLS: TAB1 TYPE TABLEVIEW USING SCREEN 100.
data: g_esnum like febep-esnum.

*---------------------------------------------------------------*
*  INCLUDE COMMON DATA                                          *
*---------------------------------------------------------------*
INCLUDE RFEBKA03.
INCLUDE RFEBFR03.                      " DATA FRANCE

TABLES: RFSDO,
        SSCRFIELDS.

*---------------------------------------------------------------*
*  PARAMETERS                                                   *
*---------------------------------------------------------------*
*-BLOCK 1     ????
SELECTION-SCREEN  BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-165.
* NEW CBO
PARAMETERS: P_BUKRS    LIKE FEBKO-BUKRS  memory id buk,   " company
            p_azdat    like febko-azdat  default sy-datum,
            p_ssbtr    like febko-SSBTR,  " initial balance
            P_ADD(1)   TYPE N            DEFAULT 0 no-display.
*           P_EMKEY    LIKE FEBKO-EMKEY  DEFAULT 'andy'.  " receipient
PARAMETERS: P_DFMT(8)  TYPE C            DEFAULT 'YYYYMMDD' no-display.

PARAMETERS: P_BATCH      LIKE RFPDO2-FEBBATCH.
PARAMETERS: EINLESEN     LIKE RFPDO1-FEBEINLES  DEFAULT 'X' no-display,
*           FORMAT       LIKE RFPDO1-FEBFORMAT DEFAULT 'M',
            P_FORMAT     LIKE RLGRAP-FILETYPE DEFAULT 'DAT' no-display,
            P_FILE       LIKE RLGRAP-FILENAME
                              DEFAULT 'C:\TEMP\b.txt',
            AUSZFILE     LIKE RFPDO1-FEBAUSZF NO-DISPLAY     " ??
                        DEFAULT TEXT-001.                    " ??
*           UMSFILE      LIKE RFPDO1-FEBUMSF,
PARAMETERS: PCUPLOAD     LIKE RFPDO1-FEBPCUPLD DEFAULT 'X' no-display.
SELECTION-SCREEN  END OF BLOCK 1.

*-BLOCK 2    ??????
SELECTION-SCREEN  BEGIN OF BLOCK 2 WITH FRAME TITLE TEXT-160.
SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS: PA_XCALL LIKE FEBPDO-XCALL    RADIOBUTTON GROUP 1.
SELECTION-SCREEN COMMENT 03(29) TEXT-161 FOR FIELD PA_XCALL.
PARAMETERS: PA_XBKBU LIKE FEBPDO-XBKBU    DEFAULT 'X'.
SELECTION-SCREEN COMMENT 35(16) TEXT-171 FOR FIELD PA_XBKBU.
PARAMETERS: PA_MODE  LIKE RFPDO-ALLGAZMD NO-DISPLAY.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN  BEGIN OF LINE.
PARAMETERS: PA_XBDC  LIKE FEBPDO-XBINPT   RADIOBUTTON GROUP 1.
SELECTION-SCREEN COMMENT 03(29) TEXT-163 FOR FIELD PA_XBDC.
PARAMETERS: MREGEL   LIKE RFPDO1-FEBMREGEL DEFAULT '1'.
SELECTION-SCREEN COMMENT 35(15) TEXT-164 FOR FIELD MREGEL.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: PA_TEST LIKE RFPDO1-FEBTESTL RADIOBUTTON GROUP 1.
SELECTION-SCREEN COMMENT 03(29) TEXT-168 FOR FIELD PA_TEST.
SELECTION-SCREEN: END OF LINE.

PARAMETERS: VALUT_ON     LIKE RFPDO2-FEBVALUT DEFAULT 'X'.
SELECTION-SCREEN  END OF BLOCK 2.

*-BLOCK 5   ????
SELECTION-SCREEN  BEGIN OF BLOCK 5 WITH FRAME TITLE TEXT-172.
SELECTION-SCREEN: BEGIN OF LINE.
PARAMETERS: PA_XDISP LIKE FEBPDO-XDISP.
SELECTION-SCREEN COMMENT 03(29) TEXT-170 FOR FIELD PA_XDISP.
PARAMETERS: PA_DSART LIKE FDES-DSART.
SELECTION-SCREEN COMMENT 36(15) TEXT-173 FOR FIELD PA_DSART.
PARAMETERS: PA_VERD  LIKE RFFFPDO1-FFDISXVERD.
SELECTION-SCREEN COMMENT 55(15) TEXT-174 FOR FIELD PA_VERD.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN  END OF BLOCK 5.

*-BLOCK 3  ????
SELECTION-SCREEN  BEGIN OF BLOCK 3 WITH FRAME TITLE TEXT-166.
DATA: NUM10(10) TYPE N.
DATA: CHR16(16) TYPE C.
SELECT-OPTIONS: S_FILTER FOR  FEBPDO-FEBFILTER1 NO-DISPLAY.
SELECT-OPTIONS: T_FILTER FOR  FEBPDO-FEBFILTER2 NO-DISPLAY.
SELECTION-SCREEN: BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 01(31) TEXT-176 FOR FIELD PA_BDART.
PARAMETERS: PA_BDART     LIKE FEBPDO-BDART NO-DISPLAY.
PARAMETERS: PA_BDANZ     LIKE FEBPDO-BDANZ NO-DISPLAY.
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN  END OF BLOCK 3.

*-BLOCK 4   ????
SELECTION-SCREEN  BEGIN OF BLOCK 4 WITH FRAME TITLE TEXT-167.
PARAMETERS: BATCH        LIKE RFPDO2-FEBBATCH,
            P_KOAUSZ     LIKE RFPDO1-FEBPAUSZ,   " KONTOAUSZUG DRUCKEN
            P_BUPRO      LIKE RFPDO2-FEBBUPRO,
            P_STATIK     LIKE RFPDO2-FEBSTAT,
            PA_LSEPA     LIKE FEBPDO-LSEPA.

SELECTION-SCREEN  END OF BLOCK 4.


*PARAMETERS: P_KUNNR    LIKE KNA1-KUNNR   DEFAULT '200002'.

*PARAMETERS: XFEBKO-ABSND(15)    : BANK KEY
* XFEBKO-ABSND+15(18) : ACCOUNT KEY
* XFEBKO-ABSND+33(5)  : CURRENCY

*---------------- AT SELECTION-SCREEN ---------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST   FOR P_FILE.
  PERFORM FUNCTION_KD_GET_FILENAME_ON_F4  USING P_FILE.

AT SELECTION-SCREEN ON BLOCK 1.
  PERFORM CHECK_BLOCK1.

AT SELECTION-SCREEN ON BLOCK 2.
  PERFORM CHECK_BLOCK2.

AT SELECTION-SCREEN ON BLOCK 3.
  PERFORM CHECK_BLOCK3.

AT SELECTION-SCREEN ON BLOCK 4.
  PERFORM CHECK_BLOCK4.

AT SELECTION-SCREEN ON BLOCK 5.
  PERFORM CHECK_BLOCK5.

******************************************************************
START-OF-SELECTION.

  PERFORM GET_PRINT_PARAMETERS USING EXECPRI     " IMPORT
                                     PRI_PARAM   " PRINT
                                     ARC_PARAM.  " IMAGE LINK
  PERFORM INITIALIZATION.              " FLAG VAR. INIT
  VGEXT_OK = TRUE.

  IF EINLESEN = 'X'.                   " UPLOAD IMPORT
    PERFORM UPLOAD_FILE.
  ENDIF.

******************************************************************
End-of-selection.
* convert
  PERFORM CONVERSION_ITEM.
* insert
  PERFORM INSERT_BANK_STATEMENT.
  COMMIT WORK.

* after work
  IF P_KOAUSZ = 'X' AND                " PRINT
     EINLESEN = 'X'.                   " DATA IMPORT
    PERFORM PRINT_DATA.
  ENDIF.

* go posting
  CHECK P_BATCH = 'X'.

  IF PA_XDISP = 'X'.                   " CM MEMO RECORD
    PERFORM FINANZDISPO_AVISE_ERZEUGEN." SUBMIT RFEBFD00
  ENDIF.

  PERFORM EXPORT_PRI_PARAMS USING EXECPRI        "<<<<INSERT - 107937
                            PRI_PARAM ARC_PARAM. "<<<<INSERT - 107937

  IF  PA_XDISP  = 'X' AND              " CM MEMO RECORD
      PA_TEST   = 'X'.                 " NO POSTING
  ELSE.
    IF VGEXT_OK = TRUE.
      PERFORM VERBUCHUNG_AUFRUFEN.     " SUBMIT RFEBBU01
    ELSE.
      DESCRIBE TABLE S_KUKEY LINES TFILL_S_KUKEY.
      IF TFILL_S_KUKEY > 0.
        PERFORM SET_PRINT_PARAMETERS USING BATCH  "<<<<INSERT - 107937
                                       PRI_PARAM  "<<<<INSERT - 107937
                                       ARC_PARAM. "<<<<INSERT - 107937
        PERFORM WRITE_WRONG_T028G.
        PERFORM DRUCK_KONTOAUSZUG.     " SELECT DATA
        PERFORM CLOSE_PRINT_PARAMETERS USING BATCH.
        PERFORM DELETE_STATEMENT.
      ENDIF.
    ENDIF.
  ENDIF.

*======================================================================*
END-OF-SELECTION.
*======================================================================*

TOP-OF-PAGE.
  PERFORM BATCH-HEADING(RSBTCHH0).

  WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.

  IF PRINTFLAG = 'A'.
    PERFORM DRUCK_BANKUEBERSCHRIFT.
  ENDIF.

*---------------------------------------------------------------*
*  FORM VERBUCHUNG_AUFRUFEN.                                    *
*---------------------------------------------------------------*
FORM VERBUCHUNG_AUFRUFEN.

*andy
*  DESCRIBE TABLE S_KUKEY LINES TFILL_S_KUKEY.
*  IF TFILL_S_KUKEY = 0 AND EINLESEN = 'X'.
*    EXIT.
*  ENDIF.

  IF BATCH = 'X'.
    JOBNAME(8)     = SY-REPID.
    JOBNAME+8(1)   = '-'.
    JOBNAME+9(14)  = TEXT-002.

    EXPORTID(8)    = SY-REPID.
    EXPORTID+8(8)  = SY-DATUM.
    EXPORTID+16(6) = SY-UZEIT.
    LOOP AT S_KUKEY.
      EXPORTID+23(8) = S_KUKEY-LOW.
      EXIT.
    ENDLOOP.
  ENDIF.

* IF SPOOL = 'X'.                       " QHA  GB
*    CLEAR PRI_PARAM.                   " QHA  GB
*    PRI_PARAM = %_PRINT.               " QHA  GB
*    EXPORT PRI_PARAM TO MEMORY.        " QHA  GB
*    IF SY-SUBRC NE 0.                  " QHA  GB
*       SPOOL = ' '.                    " QHA  GB
*    ENDIF.                             " QHA  GB
* ENDIF.                                " QHA  GB



* VERBUCHUNGSREPORT AUFRUFEN FALLS BUCHUNGEN ERZEUGT WERDEN SOLLEN.
  IF BUBER NE SPACE.
    SUBMIT RFEBBU01 AND RETURN
                    USER SY-UNAME
                    WITH ANWND    =  ANWND
                    WITH S_KUKEY  IN S_KUKEY
                    WITH JOBNAME  =  JOBNAME
                    WITH EXPORTID =  EXPORTID
                    WITH BUBER    =  BUBER
*                   WITH USEREXIT =  USEREXIT                     "30D
*                    WITH SELFD    =  SELFD
*                    WITH SELFDLEN =  SELFDLEN
                    WITH S_FILTER IN S_FILTER
                    WITH T_FILTER IN T_FILTER
                    WITH PA_BDART =  PA_BDART
                    WITH PA_BDANZ =  PA_BDANZ
                    WITH FUNCTION =  FUNCTION
                    WITH MODE     =  MODE
                    WITH MREGEL   =  MREGEL
                    WITH PA_EFART =  EFART
                    WITH P_BUPRO  =  P_BUPRO
*                   WITH SPOOL    =  SPOOL
                    WITH P_STATIK =  P_STATIK
                    WITH VALUT_ON =  VALUT_ON
                    WITH TESTL    =  PA_TEST
                    WITH EXECPRI  = EXECPRI.

*   JOBCOUNT IMPORTIEREN
    IMPORT JOBCOUNT FROM MEMORY ID EXPORTID.

*   WRITE: / 'JOBCOUNT = ', JOBCOUNT.
  ENDIF.
ENDFORM.

*EJECT
*&---------------------------------------------------------------------*
*&      FORM  FINANZDISPO_AVISE_ERZEUGEN
*&---------------------------------------------------------------------*
*       TEXT                                                           *
*----------------------------------------------------------------------*
FORM FINANZDISPO_AVISE_ERZEUGEN.
  LOOP AT S_KUKEY.
    SELECT * FROM FEBKO WHERE KUKEY = S_KUKEY-LOW.
    ENDSELECT.
    IF SY-SUBRC = 0.
      SUBMIT RFEBFD00 AND RETURN
                      USER SY-UNAME
                      WITH P_BUKRS  =  FEBKO-BUKRS
                      WITH P_HBKID  =  FEBKO-HBKID
                      WITH P_HKTID  =  FEBKO-HKTID
                      WITH P_ANWND  =  FEBKO-ANWND          "40A
                      WITH R_AZNUM  =  FEBKO-AZNUM
                      WITH R_AZDAT  =  FEBKO-AZDAT
                      WITH BI-NAME  =  SY-REPID
                      WITH BI-DSART =  PA_DSART
                      WITH P_VERD   =  PA_VERD.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " FINANZDISPO_AVISE_ERZEUGEN

*EJECT
*&---------------------------------------------------------------------*
*&      FORM  INITIALIZATION
*&---------------------------------------------------------------------*
*       FELDER INITIALISIEREN                                          *
*----------------------------------------------------------------------*
FORM INITIALIZATION.

  UPLOAD    = PCUPLOAD.                " PC UPLOAD
* EB_FORMAT = FORMAT.                  " FORMAT
  IF NOT PA_XCALL IS INITIAL.          " BATCH
    FUNCTION = 'C'.
  ENDIF.
  IF NOT PA_XBDC  IS INITIAL.          " BATCH
    FUNCTION = 'B'.
  ENDIF.
  MODE     = PA_MODE.

  IF  PA_XCALL = 'X'
  AND PA_XBKBU = 'X'.                  " ONLY BANK
    BUBER    = '1'.
  ELSE.
    BUBER    = 'A'.
  ENDIF.
  ANWND    = '0001'.                   "ANWENDUNG ZWISCHENSPEICHER
  EFART    = 'E'.                      " EDIT

ENDFORM.                               " INITIALIZATION
*&---------------------------------------------------------------------*
*&      FORM  WRITE_WRONG_T028G
*&---------------------------------------------------------------------*
*       AUSGABE DER FEHLENDEN EINTR?E IN T028G                        *
*----------------------------------------------------------------------*
FORM WRITE_WRONG_T028G.
  DATA: FIRST(1) TYPE C.                                    "HP
  PRINTFLAG = SPACE.
  NEW-PAGE.
  WRITE: /01 SY-VLINE,  TEXT-010,  132 SY-VLINE,
         /01 SY-VLINE,  TEXT-011,  132 SY-VLINE,
         /01 SY-VLINE,  TEXT-012,  132 SY-VLINE,
         /01 SY-VLINE,  TEXT-013,  132 SY-VLINE,
         /01 SY-VLINE,  TEXT-014,  132 SY-VLINE,
         /01 SY-VLINE,  TEXT-015,  132 SY-VLINE,
         /01 SY-VLINE,  TEXT-017,  132 SY-VLINE.
  FIRST = 'X'.                                              "HP
  LOOP AT NOTT028G WHERE VOZPM = '*'.                       "HP
    IF FIRST = 'X'.                                         "HP
      WRITE: /01 SY-VLINE,  TEXT-010,  132 SY-VLINE,        "HP
             /01 SY-VLINE,  TEXT-040,  132 SY-VLINE,        "HP
             /01 SY-VLINE,  TEXT-041,  132 SY-VLINE,        "HP
             /01 SY-VLINE,  TEXT-042,  132 SY-VLINE,        "HP
             /01 SY-VLINE,  TEXT-031,  132 SY-VLINE.        "HP
      "HP
      WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.  "HP
      CLEAR FIRST.                                          "HP
    ENDIF.                                                  "HP
    WRITE: /01     SY-VLINE,                                "HP
            03(08) NOTT028G-VGTYP,                          "HP
            12(27) NOTT028G-VGEXT,                          "HP
            40(03) '+/-',                                   "HP
            44(20) TEXT-032,                                "HP
            65(15) NOTT028G-BANKL,                          "HP
            81(18) NOTT028G-KTONR,                          "HP
           100(05) NOTT028G-AZNUM,                          "HP
           106(08) NOTT028G-KUKEY,                          "HP
           115(05) NOTT028G-ESNUM,                          "HP
           132     SY-VLINE.                                "HP
    DELETE NOTT028G.                                        "HP
  ENDLOOP.                                                  "HP

  SORT NOTT028G.                                            "HP
  LOOP AT NOTT028G.                                         "HP
    AT FIRST.                                               "HP
      WRITE: /01 SY-VLINE,  TEXT-010,  132 SY-VLINE,
             /01 SY-VLINE,  TEXT-030,  132 SY-VLINE,
             /01 SY-VLINE,  TEXT-031,  132 SY-VLINE.

      WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.
    ENDAT.                                                  "HP
* LOOP AT NOTT028G.                                         "HP
    WRITE: /01     SY-VLINE,
            03(08) NOTT028G-VGTYP,
            12(27) NOTT028G-VGEXT,
            40(01) NOTT028G-VOZPM,
            44(20) TEXT-032,
            65(15) NOTT028G-BANKL,
            81(18) NOTT028G-KTONR,
           100(05) NOTT028G-AZNUM,
           106(08) NOTT028G-KUKEY,
           115(05) NOTT028G-ESNUM,
           132     SY-VLINE.
  ENDLOOP.

  WRITE: /01 SY-VLINE, 02 SY-ULINE(130), 132 SY-VLINE.

  MESSAGE S773.
ENDFORM.                               " WRITE_WRONG_T028G
*&---------------------------------------------------------------------*
*&      FORM  EXPORT_PRINT_PARAMETERS
*&---------------------------------------------------------------------*
*       EXPORT PRINT PARAMETERS TO MEMORY IF BATCH RUN                 *
*----------------------------------------------------------------------*
*FORM EXPORT_PRINT_PARAMETERS USING P_BATCH         "<<<<DELETE - 107937
*                             P_PRI_PARAM.          "<<<<DELETE - 107937
FORM EXPORT_PRINT_PARAMETERS USING P_BATCH          "<<<<INSERT - 107937
                             P_PRI_PARAM            "<<<<INSERT - 107937
                             P_ARC_PARAM.           "<<<<INSERT - 107937

  IF  SY-BATCH = 'X' OR P_BATCH = 'X'.
    CLEAR PRI_KEY.
    PRI_KEY-REPID = 'RFEBBU00'.
    LOOP AT S_KUKEY.
      PRI_KEY-KUKEY = S_KUKEY-LOW.
      EXIT.
    ENDLOOP.

*    EXPORT P_PRI_PARAM TO MEMORY ID PRI_KEY.      "<<<<DELETE - 107937
    EXPORT   PRI_PARAM   ARC_PARAM TO MEMORY      "<<<<INSERT - 107937
                                      ID PRI_KEY. "<<<<INSERT - 107937
  ENDIF.
ENDFORM.                               " EXPORT_PRINT_PARAMETERS


*EJECT
*&---------------------------------------------------------------------*
*&      FORM  SET_PRINT_PARAMETERS
*&---------------------------------------------------------------------*
*       SET PRINT PARAMETERS IF PROGRAM RUNS IN BATCH                  *
*----------------------------------------------------------------------*
*FORM SET_PRINT_PARAMETERS USING P_BATCH           "<<<<DELETE - 107937
*                            P_PRI_PARAM.          "<<<<DELETE - 107937
FORM SET_PRINT_PARAMETERS USING P_BATCH"<<<<INSERT - 107937
                            P_PRI_PARAM"<<<<INSERT - 107937
                            P_ARC_PARAM.           "<<<<INSERT - 107937



  DATA: LIST_NAME LIKE PRI_PARAMS-PLIST.

  IF SY-BATCH = 'X' OR P_BATCH = 'X'.
    LIST_NAME     = SY-REPID.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
         EXPORTING
              NO_DIALOG              = 'X'
              LIST_NAME              = LIST_NAME
              MODE                   = 'CURRENT'
         IMPORTING
              OUT_ARCHIVE_PARAMETERS = P_ARC_PARAM  "<<<<INSERT - 107937
              OUT_PARAMETERS         = P_PRI_PARAM.

    NEW-PAGE  PRINT ON  PARAMETERS P_PRI_PARAM
                ARCHIVE PARAMETERS P_ARC_PARAM     "<<<<INSERT - 107937
                                   NO DIALOG.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM CLOSE_PRINT_PARAMETERS                                   *
*---------------------------------------------------------------------*
FORM CLOSE_PRINT_PARAMETERS USING P_BATCH.
  IF SY-BATCH = 'X' OR P_BATCH = 'X'.
    NEW-PAGE  PRINT OFF.
    MESSAGE S640(FV) WITH SY-SPONO.
  ENDIF.
ENDFORM.                               " CLOSE_PRINT_PARAMETERS


*EJECT
*---------------------------------------------------------------*
*  INCLUDE DER FORM-ROUTINEN  F? AUSDRUCK DES KONTOAUSZUGES    *
*---------------------------------------------------------------*
INCLUDE RFEKAP00.

*&---------------------------------------------------------------------*
*&      FORM  DELETE_STATEMENT
*&---------------------------------------------------------------------*
FORM DELETE_STATEMENT.
  SELECT * FROM FEBKO  WHERE KUKEY IN S_KUKEY AND ANWND = '0001'.
    DELETE FROM FEBRE WHERE KUKEY = FEBKO-KUKEY.
    DELETE FROM FEBEP WHERE KUKEY = FEBKO-KUKEY.

*   Andy Add
    DELETE FROM FEBCL WHERE KUKEY = FEBKO-KUKEY.
*   CBO Table
*   FIXIT
*    DELETE FROM ZFEBEP WHERE KUKEY = FEBKO-KUKEY.
*    DELETE FROM ZFEBCL WHERE KUKEY = FEBKO-KUKEY.

    MOVE-CORRESPONDING FEBKO TO FEBVW.
    DELETE FEBVW.
    DELETE FEBKO.
  ENDSELECT.

ENDFORM.                               " DELETE_STATEMENT
*&---------------------------------------------------------------------*
*&      FORM  GET_PRINT_PARAMETERS
*&---------------------------------------------------------------------*
FORM GET_PRINT_PARAMETERS USING    P_EXECPRI     " IMPORT
                                   P_PRI_PARAM   " PRINT PARAM
                                   P_ARC_PARAM.  " IMAGE LINK

  DATA: LIST_NAME LIKE PRI_PARAMS-PLIST.

  IF P_EXECPRI = 'X' OR
     ( ( BATCH = 'X' ) AND ( EINLESEN NE 'X' OR P_KOAUSZ NE 'X' ) ).
    LIST_NAME     = SY-REPID.
    CALL FUNCTION 'GET_PRINT_PARAMETERS'
         EXPORTING
              NO_DIALOG              = 'X'
              LIST_NAME              = LIST_NAME
              MODE                   = 'CURRENT'
         IMPORTING
              OUT_ARCHIVE_PARAMETERS = P_ARC_PARAM  "<<<<INSERT - 107937
              OUT_PARAMETERS         = P_PRI_PARAM.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  EXPORT_PRI_PARAMS
*&---------------------------------------------------------------------*
FORM EXPORT_PRI_PARAMS USING    P_EXECPRI
*                               P_PRI_PARAM.       "<<<<DELETE - 107937
                                P_PRI_PARAM        "<<<<INSERT - 107937
                                P_ARC_PARAM.       "<<<<INSERT - 107937
*  EXPORT PRINT PARAMETERS TO MEMORY                            *

*IF P_EXECPRI = 'X'.                                "<<<<DELETE - 107937
 IF P_EXECPRI = 'X' OR ( ( BATCH = 'X' ) AND        "<<<<INSERT - 107937
    ( EINLESEN NE 'X' OR P_KOAUSZ NE 'X' ) ).       "<<<<INSERT - 107937
    CLEAR PRI_KEY.
    PRI_KEY-REPID = 'RFEBBU00'.
    LOOP AT S_KUKEY.
      PRI_KEY-KUKEY = S_KUKEY-LOW.
      EXIT.
    ENDLOOP.
*   EXPORT P_PRI_PARAM TO MEMORY ID PRI_KEY.       "<<<<DELETE - 107937
    EXPORT   PRI_PARAM   ARC_PARAM TO MEMORY       "<<<<INSERT - 107937
                                   ID PRI_KEY.     "<<<<INSERT - 107937

  ENDIF.

ENDFORM.                               " EXPORT_PRI_PARAMS
*&---------------------------------------------------------------------*
*&      FORM  FORMAT_TITO
*&---------------------------------------------------------------------*
*FORM FORMAT_TITO.
*  MESSAGE E908 WITH TEXT-003.
*ENDFORM.                               " FORMAT_TITO
*&---------------------------------------------------------------------*
*&      FORM  FUNCTION_KD_GET_FILENAME_ON_F4
*&---------------------------------------------------------------------*
FORM FUNCTION_KD_GET_FILENAME_ON_F4
                              USING IFILE LIKE RLGRAP-FILENAME.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            MASK          = 'X'
       CHANGING
            FILE_NAME     = IFILE
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
  IF SY-SUBRC <> 0.
    EXIT.
  ENDIF.

ENDFORM.                               " FUNCTION_KD_GET_FILENAME_ON_F4
*&---------------------------------------------------------------------*
*&      FORM  CHECK_BLOCK1
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK1.

* MOVE AUSZFILE TO AUSZUG-FILE.
* MOVE UMSFILE  TO UMSATZ-FILE.

* IF NOT UMSFILE IS INITIAL AND FORMAT NE 'M'.
*   SET CURSOR FIELD 'UMSFILE'.
*   MESSAGE E621(FV).
* ENDIF.

ENDFORM.                               " CHECK_BLOCK1
*&---------------------------------------------------------------------*
*&      FORM  CHECK_BLOCK2
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK2.

  IF NOT PA_XBDC IS INITIAL.
*   BATCH INPUT ERZEUGEN
    IF MREGEL IS INITIAL.
      SET CURSOR FIELD 'MREGEL'.
      MESSAGE E619(FV).
    ENDIF.
    IF NOT PA_XBKBU IS INITIAL.
      SET CURSOR FIELD 'PA_XBKBU'.
      MESSAGE E611(FV).
    ENDIF.
  ENDIF.


ENDFORM.                               " CHECK_BLOCK2
*&---------------------------------------------------------------------*
*&      FORM  CHECK_BLOCK3
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK3.
*-??  START
* CLEAR T_FILTER.

* LOOP AT T_FILTER.
*   SHIFT T_FILTER-LOW  RIGHT DELETING TRAILING ' '.
*   SHIFT T_FILTER-HIGH RIGHT DELETING TRAILING ' '.
*   MODIFY T_FILTER.
* ENDLOOP.
*-??  END

  CASE PA_BDART.
    WHEN 1.
      IF NOT PA_BDANZ IS INITIAL.
        SET CURSOR FIELD 'PA_BDANZ'.
        MESSAGE E618(FV).
      ENDIF.
    WHEN 2.
      IF PA_BDANZ IS INITIAL.
        SET CURSOR FIELD 'PA_BDANZ'.
        MESSAGE E615(FV).
      ENDIF.
  ENDCASE.

ENDFORM.                               " CHECK_BLOCK3
*&---------------------------------------------------------------------*
*&      FORM  CHECK_BLOCK4
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK4.
  IF SY-BATCH = 'X'.
    IF BATCH NE 'X'.
      BATCH = 'X'.
    ENDIF.
  ENDIF.

*---- PROGRAM STARTED WITH EXEC+PRINT ONLINE
  IF BATCH NE 'X'.
    IF P_BUPRO = 'X' OR P_STATIK = 'X'.
      IF SSCRFIELDS-UCOMM = 'PRIN'.
        EXECPRI = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_BLOCK4
*&---------------------------------------------------------------------*
*&      FORM  CHECK_BLOCK5
*&---------------------------------------------------------------------*
FORM CHECK_BLOCK5.
  IF NOT PA_XDISP IS INITIAL.
*   CALL TRANSAKTION
    IF NOT PA_XCALL IS INITIAL.
      SET CURSOR FIELD 'PA_XDISP'.
      MESSAGE E610(FV).
    ENDIF.
    IF PA_DSART IS INITIAL.
      SET CURSOR FIELD 'PA_DSART'.
      MESSAGE E612(FV).
    ENDIF.
  ENDIF.

ENDFORM.                               " CHECK_BLOCK5
*&---------------------------------------------------------------------*
*&      FORM  PRINT_DATA
*&---------------------------------------------------------------------*
FORM PRINT_DATA.

  DESCRIBE TABLE S_KUKEY LINES TFILL_S_KUKEY.
  IF TFILL_S_KUKEY > 0    AND
     VGEXT_OK     =  TRUE.
    PERFORM SET_PRINT_PARAMETERS USING BATCH     "<<<<INSERT - 107937
                                   PRI_PARAM     "<<<<INSERT - 107937
                                   ARC_PARAM.    "<<<<INSERT - 107937
    PERFORM DRUCK_KONTOAUSZUG.         " SELECT DATA
    PERFORM EXPORT_PRINT_PARAMETERS USING BATCH  "<<<<INSERT - 107937
                                    PRI_PARAM    "<<<<INSERT - 107937
                                    ARC_PARAM.   "<<<<INSERT - 107937

    PERFORM CLOSE_PRINT_PARAMETERS USING BATCH.
  ENDIF.

ENDFORM.                               " PRINT_DATA
*&---------------------------------------------------------------------*
*&      FORM  UPLOAD_FILE
*&---------------------------------------------------------------------*
FORM UPLOAD_FILE.

  IF UPLOAD = 'X'.
    PERFORM UPLOAD_FROM_DISK.
  ELSE.
    PERFORM GET_UNIX_FILE_LIST.
    PERFORM UPLOAD_FROM_UNIX.
    DESCRIBE TABLE itab LINES TOT_CNT.
    IF TOT_CNT = 0.
      MESSAGE E437(DS) WITH TEXT-211.
    ENDIF.
  ENDIF.

ENDFORM.                               " UPLOAD_FILE
*&---------------------------------------------------------------------*
*&      FORM  UPLOAD_FROM_DISK
*&---------------------------------------------------------------------*
FORM UPLOAD_FROM_DISK.

  IF P_FORMAT = 'DAT'.
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              FILENAME            = P_FILE
              FILETYPE            = 'DAT'
         TABLES
              DATA_TAB            = iftab
         EXCEPTIONS
              CONVERSION_ERROR    = 1
              FILE_OPEN_ERROR     = 2
              FILE_READ_ERROR     = 3
              INVALID_TABLE_WIDTH = 4
              INVALID_TYPE        = 5
              NO_BATCH            = 6
              UNKNOWN_ERROR       = 7
              OTHERS              = 8.
  ELSE.
    CALL FUNCTION 'WS_UPLOAD'
         EXPORTING
              FILENAME                = P_FILE
              FILETYPE                = 'ASC'
         TABLES
              DATA_TAB                = iftab
         EXCEPTIONS
              CONVERSION_ERROR        = 1
              FILE_OPEN_ERROR         = 2
              FILE_READ_ERROR         = 3
              INVALID_TABLE_WIDTH     = 4
              INVALID_TYPE            = 5
              NO_BATCH                = 6
              UNKNOWN_ERROR           = 7
              GUI_REFUSE_FILETRANSFER = 8.
  ENDIF.

  CASE SY-SUBRC.
    WHEN 1.      MESSAGE E002 WITH AUSZUG-FILE+2.
    WHEN 2.      MESSAGE E002 WITH AUSZUG-FILE+2.
    WHEN OTHERS.
  ENDCASE.

*  IF P_FORMAT = 'ASC'.
*        PERFORM MOVE_TO_itab.
*  ENDIF.
ENDFORM.                               " UPLOAD_FROM_DISK
*&---------------------------------------------------------------------*
*&      FORM  UPLOAD_FROM_UNIX
*&---------------------------------------------------------------------*
FORM UPLOAD_FROM_UNIX.

*  OPEN DATASET AUSZUG-FILE IN TEXT MODE FOR INPUT.
*  IF SY-SUBRC NE 0.
*    MESSAGE E002 WITH AUSZUG-FILE.
*  ENDIF.
*
*  DO.
*    CLEAR AUSZUG-ZEILE.
*    READ DATASET AUSZUG-FILE INTO i_febko.
*    IF SY-SUBRC NE 0.
*      EXIT.
*    ENDIF.
*    APPEND i_febko.
*  ENDDO.
*  CLOSE DATASET AUSZUG-FILE.

  OPEN DATASET UMSATZ-FILE IN TEXT MODE FOR INPUT.
  IF SY-SUBRC NE 0.
    MESSAGE E002 WITH UMSATZ-FILE.
  ENDIF.

  DO.
    CLEAR IUNIX.
    READ DATASET UMSATZ-FILE INTO IUNIX .
    IF SY-SUBRC NE 0.
      EXIT.
    ENDIF.
    APPEND IUNIX.
  ENDDO.
  CLOSE DATASET UMSATZ-FILE.

  PERFORM MOVE_TO_itab.

ENDFORM.                               " UPLOAD_FROM_UNIX
*&---------------------------------------------------------------------*
*&      FORM  MOVE_TO_itab
*&---------------------------------------------------------------------*
FORM MOVE_TO_itab.

  LOOP AT IUNIX.
    CASE IUNIX(1).
      WHEN '1' .
        PERFORM APPEND_itab_HEAD.
      WHEN '2'.
        PERFORM APPEND_itab_ITEM.
      WHEN '3'.
        PERFORM APPEND_itab_ITEM_CLR.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  GET_AMT
*&---------------------------------------------------------------------*
FORM GET_AMT USING ICURR   LIKE  TCURR-TCURR
                                 ITEMP
                                 IAMOUNT .
  DATA: T_FACTOR    TYPE P DECIMALS 3,
        T_AMOUNT    TYPE P DECIMALS 3.

  CALL FUNCTION 'CURRENCY_CONVERTING_FACTOR'
       EXPORTING
            CURRENCY          = ICURR
       IMPORTING
            FACTOR            = T_FACTOR
       EXCEPTIONS
            TOO_MANY_DECIMALS = 1
            OTHERS            = 2.
  T_AMOUNT      =  ITEMP.
  T_AMOUNT      =  T_AMOUNT / T_FACTOR.
  IAMOUNT       =  T_AMOUNT      .     " return value

ENDFORM.                               " GET_AMT
*&---------------------------------------------------------------------*
*&      FORM  GET_VGINT_INTAG_PFORM
*&---------------------------------------------------------------------*
FORM GET_VGINT_INTAG_PFORM.
* get transaction type information
  SELECT SINGLE * FROM T028B WHERE  BANKL = i_febep-BANKL
                               AND  KTONR = i_febep-BANKN.
  i_febko-VGTYP = T028B-VGTYP.

* get transaction
  IF i_febep-VGEXT > '399'.  " KWBTR < 0.
    SELECT SINGLE * FROM T028G WHERE VGTYP = i_febko-VGTYP
                                 AND VGEXT = i_febep-VGEXT
                                 AND VOZPM = '-'.
  ELSE.
    SELECT SINGLE * FROM T028G WHERE VGTYP = i_febko-VGTYP
                                 AND VGEXT = i_febep-VGEXT
                                 AND VOZPM = '+'.
  ENDIF.

  IF SY-SUBRC <> 0.
    IF i_febep-VGEXT > '399'.  " KWBTR < 0.
      SELECT SINGLE * FROM T028G WHERE VGTYP = i_febko-VGTYP
                                   AND VGEXT = 'OTHER'
                                   AND VOZPM = '-'.
    ELSE.
      SELECT SINGLE * FROM T028G WHERE VGTYP = i_febko-VGTYP
                                   AND VGEXT = 'OTHER'
                                   AND VOZPM = '+'.
    ENDIF.
  endif.

  MOVE: T028G-VGINT TO i_febep-VGINT,
        T028G-INTAG TO i_febep-INTAG,
        T028G-PFORM TO i_febep-PFORM.
  if t028g-vozpm = '-'.
    i_febep-EPVOZ  =  'S'.
  else.
    i_febep-EPVOZ  =  'H'.
  endif.

ENDFORM.                               " GET_VGINT_INTAG_PFORM
*&---------------------------------------------------------------------*
*&      FORM  GET_FEBVW_BANKA
*&---------------------------------------------------------------------*
FORM GET_FEBVW_BANKA.
* >>>
  SELECT SINGLE * FROM T005 WHERE LAND1 = FEBVW-BANKS.

*T005-BNKEY -> for identifying a bank in the R/3 System
*Bank key "2" (managing the bank data using the account number) is
*obsolete and is only supported for reasons of compatability. You should
*choose one of the following alternatives:
*For data medium exchange, it can be useful to be able to enter banks of
*foreign customers/vendors without bank numbers, even if there are bank
*numbers in the country concerned. In such cases, the bank key can be
*assigned internally (bank key "3").

  IF T005-BNKEY = '2'.
    SELECT SINGLE * FROM BNKA WHERE BANKS = FEBVW-BANKS
                              AND   BANKL = FEBVW-ABSND+15(35).
  ELSE.
    SELECT SINGLE * FROM BNKA WHERE BANKS = FEBVW-BANKS
                              AND   BANKL = FEBVW-BANKL.
  ENDIF.
  IF SY-SUBRC = 0.
    FEBVW-BANKA = BNKA-BANKA.
  ENDIF.

ENDFORM.                               " GET_FEBVW_BANKA
*&---------------------------------------------------------------------*
*&      FORM  GET_UNIX_FILE_LIST
*&---------------------------------------------------------------------*
FORM GET_UNIX_FILE_LIST.
  DATA: P_CMD(40).

* P_CMD = 'LS /DACOM/WORK/EDI/SRC/SVCCLI/RECV/'.
  CONCATENATE 'LS' AUSZFILE INTO P_CMD
                            SEPARATED BY SPACE.

  CALL FUNCTION 'RFC_REMOTE_PIPE'
       DESTINATION 'SERVER_EXEC'
       EXPORTING    COMMAND               = P_CMD
                    READ                  = 'X'
       TABLES       PIPEDATA              = P_ILOG
       EXCEPTIONS   SYSTEM_FAILURE        = 1
                    COMMUNICATION_FAILURE = 2.
  CHECK SY-SUBRC = 0.

  TAB1-TOP_LINE       =  1.
  TAB1-CURRENT_LINE   =  1.
  DESCRIBE TABLE P_ILOG LINES TOT_CNT.
  CALL SCREEN 100
       STARTING AT 40 5 ENDING AT 75 13.

  LOOP AT P_ILOG WHERE CHECK = 'X'.
    CONCATENATE '/DACOM/WORK/EDI/SRC/SVCCLI/RECV/' P_ILOG-A
                                                   INTO UMSATZ-FILE.
    EXIT.
  ENDLOOP.
  CHECK SY-SUBRC <> 0.
  MESSAGE S437(DS) WITH TEXT-209.
  STOP.

ENDFORM.                               " GET_UNIX_FILE_LIST
*&---------------------------------------------------------------------*
*&      MODULE  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS '100'.
  SET TITLEBAR '100'.

ENDMODULE.                             " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CHECK SY-UCOMM <> SPACE.
  CASE SY-UCOMM.
    WHEN 'P--'.
      CLEAR SY-UCOMM.
      TAB1-TOP_LINE = 1.
    WHEN 'P-'.
      CLEAR SY-UCOMM.
      TAB1-TOP_LINE = TAB1-TOP_LINE - ILOOPC.
      IF TAB1-TOP_LINE < 1.
        TAB1-TOP_LINE = 1.
      ENDIF.
    WHEN 'P+'.
      CLEAR SY-UCOMM.
      TAB1-TOP_LINE = TAB1-TOP_LINE + ILOOPC.
      IF TAB1-TOP_LINE > TOT_CNT.
        TAB1-TOP_LINE = TOT_CNT.
      ENDIF.
    WHEN 'P++'.
      CLEAR SY-UCOMM.
      TAB1-TOP_LINE = TOT_CNT.
    WHEN 'ENTE'.
      P_FILE = P_ILOG-A.
      CLEAR SY-UCOMM.
      SET SCREEN 0.    LEAVE SCREEN.
    WHEN 'CANC'.
      CLEAR SY-UCOMM.
      SET SCREEN 0.    LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                             " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      MODULE SET_LOOPC
*&---------------------------------------------------------------------*
MODULE SET_LOOPC.
  ILOOPC = SY-LOOPC.
ENDMODULE.                             " SET_LOOPC INPUT
*&---------------------------------------------------------------------*
*&      MODULE  MODIFY_CHECK  INPUT
*&---------------------------------------------------------------------*
MODULE MODIFY_CHECK INPUT.

  CHECK P_ILOG-CHECK = 'X'.
  MODIFY P_ILOG INDEX TAB1-CURRENT_LINE.

ENDMODULE.                             " MODIFY_CHECK  INPUT


*CBO

*UNIX FILE
*>>>>>>
*&---------------------------------------------------------------------*
*&      FORM  APPEND_itab_HEAD
*&---------------------------------------------------------------------*
FORM APPEND_itab_HEAD.

  CLEAR itab.
*  itab-FIELD1   = 'H'.                                  " ???
*  itab-FIELD2   = IUNIX+13(2).                          " ????
*  PERFORM CONVERT_DATE USING: IUNIX+15(6) itab-FIELD3,  " ????
*                              IUNIX+21(6) itab-FIELD4,  " ?????
*                              IUNIX+27(6) itab-FIELD5.  " ?????
*  itab-FIELD6   =  IUNIX+33(6) .                        " ????
*  itab-FIELD7   =  IUNIX+39(1).                         " ????
*  APPEND itab.

ENDFORM.                    " APPEND_itab_HEAD
*&---------------------------------------------------------------------*
*&      FORM  APPEND_itab_ITEM
*&---------------------------------------------------------------------*
FORM APPEND_itab_ITEM.

  CLEAR itab.
*  itab-FIELD1       = 'D'.                             " ???
*  itab-FIELD2       = IUNIX+1(15).                     " ????
*  itab-FIELD3(4)    = SY-DATUM(4).                     " ???? YY
*  itab-FIELD3+4(4)  = IUNIX+16(4).                     " ????
*  itab-FIELD4       = IUNIX+20(2).                     " ????
*  itab-FIELD5       = IUNIX+22(2).                     " ??????
*  itab-FIELD6       = IUNIX+24(10).                    " ????
*  itab-FIELD7       = IUNIX+34(1) .                    " SIGN
*  itab-FIELD8       = IUNIX+35(10).                    " ?????
*  itab-FIELD9       = IUNIX+45(4).                     " ????
*  APPEND itab.

ENDFORM.                    " APPEND_itab_ITEM
*&---------------------------------------------------------------------*
*&      FORM  APPEND_itab_ITEM_CLR
*&---------------------------------------------------------------------*
FORM APPEND_itab_ITEM_CLR.

  CLEAR itab.
*  itab-FIELD1   = 'F'.                                  " ???
*  itab-FIELD2   = IUNIX+1(7) .                          " ????
*  itab-FIELD3   = SPACE.                                " ????
*  itab-FIELD4   = IUNIX+65(13).                         " ?????
*  itab-FIELD5   = IUNIX+15(12).                         " ?????
*  itab-FIELD6   = IUNIX+27(7).                          " ????
*  itab-FIELD7   = IUNIX+34(12).                         " ?????
*  APPEND itab.

ENDFORM.                    " APPEND_itab_ITEM_CLR
*&---------------------------------------------------------------------*
*&      FORM  CONVERT_DATE
*&---------------------------------------------------------------------*
FORM CONVERT_DATE USING    P_IUNIX
                           P_FIELD.
  DATA: TEMP(8),
        TDATUM   LIKE SY-DATUM.

  TEMP = P_IUNIX.

* GMAC : MMDDYY
  IF P_DFMT+6(2) = 'DD'.
    CONCATENATE TEMP+4(2) TEMP+0(2) TEMP+2(2)
              INTO TEMP .
  ELSEIF P_DFMT+0(2) = 'DD'.
    CONCATENATE TEMP+2(2) TEMP+0(2) TEMP+4(2)
              INTO TEMP .
  ELSE.
    CONCATENATE TEMP+0(2) TEMP+2(2) TEMP+4(2)
              INTO TEMP .
  ENDIF.


  CALL FUNCTION 'CONVERT_DATE_INPUT'
       EXPORTING
            INPUT                     = TEMP
       IMPORTING
            OUTPUT                    = TDATUM
       EXCEPTIONS
            PLAUSIBILITY_CHECK_FAILED = 1
            WRONG_FORMAT_IN_INPUT     = 2
            OTHERS                    = 3.

  IF SY-SUBRC <> 0.
*  FIXIT error message
  ENDIF.

  P_FIELD = TDATUM.

ENDFORM.                    " CONVERT_DATE
*&---------------------------------------------------------------------*
*&      FORM  SET_KUKEY
*&---------------------------------------------------------------------*
FORM SET_KUKEY.

  CALL FUNCTION 'GET_SHORTKEY_FOR_FEBKO'
       IMPORTING
            E_KUKEY             = FEBKO-KUKEY
       EXCEPTIONS
            FEBKEY_UPDATE_ERROR = 1.

  CHECK SY-SUBRC = 1.
  MESSAGE E437(DS) WITH 'Error : FEBKEY_UPDATE_ERROR'.

ENDFORM.                               " SET_KUKEY
*&---------------------------------------------------------------------*
*&      FORM  APPEND_S_KUKEY
*&---------------------------------------------------------------------*
FORM APPEND_S_KUKEY.

  REFRESH S_KUKEY.
  S_KUKEY-SIGN   = 'I'.
  S_KUKEY-OPTION = 'EQ'.
  S_KUKEY-LOW    =  FEBKO-KUKEY.
  APPEND S_KUKEY.

ENDFORM.                               " APPEND_S_KUKEY
*&---------------------------------------------------------------------*
*&      FORM  GET_ESNUM
*&---------------------------------------------------------------------*
FORM GET_ESNUM.

  SELECT MAX( ESNUM ) FROM FEBEP INTO FEBEP-ESNUM
                         WHERE KUKEY = FEBKO-KUKEY.
*                         ORDER BY ESNUM DESCENDING.
  IF SY-SUBRC <> 0.
    FEBEP-ESNUM = 0.
  ENDIF.

* get next number.
  FEBEP-ESNUM = FEBEP-ESNUM + 1.

ENDFORM.                               " GET_ESNUM
*---------------------------------------------------------------------*
*       FORM GET_BANK                                                 *
*---------------------------------------------------------------------*
* Using function module GET_BANK_ACCOUNT to determine the correct     *
* house bank and bank account.                                        *
*---------------------------------------------------------------------*
FORM GET_BANK.
*  DATA: UP_ACCT LIKE T012K-BANKN,
*        UP_WAER LIKE T012K-WAERS.
*  UP_ACCT = i_febko-BANKN.
*  UP_WAER = i_febko-WAERS.

  SELECT SINGLE * FROM T012K WHERE BUKRS = P_BUKRS
                               AND BANKN = i_febko-BANKN.
  SELECT SINGLE * FROM T012 WHERE BUKRS = t012k-bukrs
                              AND HBKID = T012K-HBKID.

*  CALL FUNCTION 'GET_BANK_ACCOUNT'
*       EXPORTING
*            I_BANKN                = UP_ACCT
*       IMPORTING
*            E_T012                 = T012
*            E_T012K                = T012K
*       EXCEPTIONS
*            BANK_ACCOUNT_NOT_FOUND = 2
*            MULTIPLE_BANK_ACCOUNT  = 7
*            OTHERS                 = 8.
*    i_febko-BANKL = T012-BANKL.
*    i_febko-BANKN = T012K-BANKN.

  i_febko-banks        = t012-banks.
  i_febko-absnd(15)    = t012-BANKL.
  i_febko-ABSND+15(30) = t012k-BANKN.
  i_febko-ABSND+45(3)  = t012k-WAERS.
  i_febko-hkont        = t012k-hkont.
  i_febko-hbkid        = t012k-hbkid.
  i_febko-hktid        = t012k-hktid.



ENDFORM.                               "GET_BANK
*&---------------------------------------------------------------------*
*&      FORM  CONVERSION_ITEM
*&---------------------------------------------------------------------*
FORM CONVERSION_ITEM.
  data: l_cnt type i.

*  loop at iftab.
*    move-corresponding iftab to itab.
**length:9 .. 0xxxxxxxx
*    write: iftab-bankl to itab-bankl using edit mask '0________'.
*    append itab.
*  endloop.

  clear: i_febep, l_cnt.
  sort itab by BANKN azdat.

  loop at itab.
    clear i_febep.
    l_cnt = l_cnt + 1.
    move-corresponding itab to i_febep.

    i_febep-ESNUM = l_cnt.

* transaction type .. error check???
    PERFORM GET_VGINT_INTAG_PFORM.

    i_febep-BUDAT   =  itab-AZDAT.             " Posting date
    i_febep-BVDAT   =  itab-AZDAT.             " Doc date
    i_febep-VALUT   =  i_febep-bvdat + P_ADD.  " Value date

* get data from file
    if i_febep-vgint = '31'.  "check
      i_febep-CHECT   =  itab-info1.
    endif.

* total debit = incoming
    IF i_febep-EPVOZ  =  'S'. "outgoing
      i_febko-SUMSO  = i_febko-SUMSO + i_febep-KWBTR.
    ELSE.
      i_FEBKO-SUMHA  = i_FEBKO-SUMHA + i_febep-KWBTR.
    ENDIF.

    i_febep-GJAHR     =   itab-AZDAT+0(4).
* ok.
    append i_febep.
    i_febko-waers = itab-KWAER.

* save to header
*   1) daily reconcile .. date, bank account, amount...
*   2) monthy reconcole.. bank account, date, amount... (input)
    at end of bankn.
*     i_febko-bukrs  = p_bukrs.
      i_febko-AZDAT  = p_azdat.        " itab-azdat.
      i_febko-ANZES  = i_febep-esnum.

      i_febko-BANKL  = itab-BANKl.
      i_febko-BANKN  = itab-BANKn.
      perform get_bank.

      append i_febko.
      clear: i_febep, i_febko.
    endat.
  endloop.

ENDFORM.                               " CONVERSION_ITEM
*&---------------------------------------------------------------------*
*&      FORM  CONVERSION_CLEARING
*&---------------------------------------------------------------------*
*FORM CONVERSION_CLEARING.
* TRAN.TYPE : 1-cash applied, 2-cash on acct., 3-transfer,
*             4-cash disburs. 5-reinstatement
* ITEM.TYPE : 1-invoice, 2-adjustment
* SIGN      : 0-debit, 1-credit
*
*  i_febcl-ZITMKY      = itab-FIELD5+0(1).      " Item Format
*  i_febcl-ZITMDS      = itab-FIELD5+1(8).      " Item desc.
*  PERFORM CONVERT_DATE USING: itab-FIELD5+9(6)
*                              i_febcl-ZIDATE.   " item date
*  i_febcl-ZITMNO      = itab-FIELD5+15(6).     " Item #
*  i_febcl-ZINVNO      = itab-FIELD5+67(8).     " Invoice #
*  i_febcl-ZISIGN      = itab-FIELD5+32(1).     " Item sign
*  PERFORM GET_AMT USING: i_febko-WAERS itab-FIELD5+21(11)
*                         i_febcl-ZIAMT.
*  IF i_febcl-ZISIGN = '1'.                       " credit
*     i_febcl-ZIAMT = i_febcl-ZIAMT * -1.
*  ENDIF.
*
*  i_febep-ZCLRNO   = i_febep-ZCLRNO + 1.
*  i_febcl-CSNUM  = i_febep-ZCLRNO.
*  i_febcl-ESNUM  = i_febep-ESNUM.
*
*  i_febcl-KUNNR  =  itab-FIELD2.      " CUST NO
*  i_febcl-CHECT  =  itab-FIELD3.      " CHECK NO
*  i_febcl-FACNO  =  itab-FIELD1.      " GMAC Client no
*
*  APPEND i_febcl.
*
*ENDFORM.                               " CONVERSION_CLEARING
*&---------------------------------------------------------------------*
*&      FORM  CONVERSION_TRAIL
*&---------------------------------------------------------------------*
*FORM CONVERSION_TRAIL.
*
*  PERFORM GET_AMT USING: i_febko-WAERS itab-FIELD5+0(11)
*                         i_febep-ZAAMT.
*  i_febep-ZASIGN  =  itab-FIELD5+11(1).  " a/r sign
*  PERFORM GET_AMT USING: i_febko-WAERS itab-FIELD5+12(11)
*                         i_febep-ZDAMT.
*  i_febep-ZDSIGN  =  itab-FIELD5+23(1).  " adj. sign
*  PERFORM GET_AMT USING: i_febko-WAERS itab-FIELD5+24(11)
*                         i_febep-ZNAMT.
*  i_febep-ZNSIGN  =  itab-FIELD5+35(1).  " net sign
*
*  IF i_febep-ZASIGN = '1'.  " credit amount
*     i_febep-ZAAMT = i_febep-ZAAMT * -1.
*  ENDIF.
*  IF i_febep-ZDSIGN = '1'.  " credit amount
*     i_febep-ZDAMT = i_febep-ZDAMT * -1.
*  ENDIF.
*  IF i_febep-ZNSIGN = '1'.  " credit amount
*     i_febep-ZNAMT = i_febep-ZNAMT * -1.
*  ENDIF.
*
** determine amount
**  IF i_febep-ZNAMT = 0.
**    i_febep-KWBTR = i_febep-CHKAMT.  " No incoming payment
**  ELSE.
**    i_febep-KWBTR = i_febep-ZNAMT.
**  ENDIF.
*  i_febep-KWBTR   = i_febep-ZNAMT.
*  i_febep-KWAER   = i_febko-WAERS.
*
** record no.
*  i_febko-ANZES   = i_febko-ANZES + 1.
*  APPEND i_febep.
*
** collect debit/credit total
*  IF i_febep-ZNSIGN = '1'.  " credit
*      i_febko-SUMSO =  i_febko-SUMSO + i_febep-KWBTR.
*  ELSE.          " debit
*      i_febko-SUMHA =  i_febko-SUMHA + i_febep-KWBTR.
*  ENDIF.
*
*  CLEAR i_febep-ZCLRNO.
*
*ENDFORM.                               " CONVERSION_TRAIL
*&---------------------------------------------------------------------*
*&      FORM  INSERT_BANK_STATEMENT
*&---------------------------------------------------------------------*
FORM INSERT_BANK_STATEMENT.

  loop at i_febko.
    PERFORM febko_insert.

    clear: g_esnum.
*    write:/ 'FEBKO:', febko-kukey.

    LOOP AT i_febep where BANKL = i_febko-bankl
                      and BANKN = i_febko-BANKN.
      PERFORM INSERT_FEBEP.

*      write:/ 'FEBEP:', febep.
    ENDLOOP.

  endloop.
ENDFORM.                               " INSERT_BANK_STATEMENT
*&---------------------------------------------------------------------*
*&      FORM  febko_insert
*&---------------------------------------------------------------------*
FORM febko_insert.
  data: l_year(4) type n.
*  FEBKO-HBKID       =  i_febko-HBKID.               " HOUSE BANK
*  FEBKO-HKTID       =  i_febko-HKTID.               " ACCOUNT ID
*  FEBKO-HKONT       =   i_febko-HKONT.              " BANK GL
*  FEBKO-ANZES       = i_febko-ANZES.                " RECORD NO
*  FEBKO-VGTYP       = i_febko-VGTYP.                " TRAN.TYPE
*  FEBKO-AZDAT       = i_febko-AZDAT.
  move-corresponding i_febko to febko.


  FEBKO-ANWND       = '0001'.
  PERFORM SET_KUKEY.
* FEBKO-EMKEY       = blank

* fiscal year + statement no.
  PERFORM GET_YEAR   USING i_febko-AZDAT  l_year.
  FEBKO-AZIDT(4)    = l_year.
  FEBKO-AZIDT+4(5)  = FEBKO-KUKEY+3(5).


* Get starting balance
  DATA: L_AZNUM like FEBKO-AZNUM, l_ssbtr like febko-ssbtr.
  SELECT AZNUM ESBTR FROM FEBKO  INTO (L_AZNUM, l_ssbtr)
                            WHERE ANWND  = '0001'
                              AND ABSND  = i_febko-ABSND
                            ORDER BY AZNUM DESCENDING.
    EXIT.
  ENDSELECT.
  febko-AZNUM = l_AZNUM + 1.
  if p_ssbtr <> 0.
    i_febko-SSBTR = p_ssbtr.
  else.
    i_febko-SSBTR = l_ssbtr.
  endif.
  IF i_febko-SSBTR >= 0.
    FEBKO-SSVOZ  =  'H'.                           " BEGIN BAL +/-
  ELSE.
    FEBKO-SSVOZ  =  'S'.
  ENDIF.
  FEBKO-SSBTR  =  i_febko-SSBTR.                           " BEGIN
  FEBKO-SUMSO  =  i_febko-SUMSO .                          " DEBIT
  FEBKO-SUMHA  =  i_febko-SUMHA .                          " CREDIT
  FEBKO-ESBTR  =  FEBKO-SSBTR + FEBKO-SUMHA - FEBKO-SUMSO. " END
  IF FEBKO-ESBTR >= 0.
    FEBKO-ESVOZ  =  'H'.                           " END +/-
  ELSE.
    FEBKO-ESVOZ  =  'S'.
  ENDIF.


  FEBKO-EFART       = 'E'.                         " INPUT METHOD
  FEBKO-ASTAT       = '2'.

  FEBKO-KTONR       =   i_febko-BANKN.              " BANK NO
  FEBKO-BUKRS       =   P_BUKRS.                   " COMP.CODE
  FEBKO-EUSER       =  SY-UNAME.                   " USER NAME
  FEBKO-EDATE       =  SY-DATUM.                   " CREATE DATE
  FEBKO-ETIME       =  SY-UZEIT.                   " CREATE TIME

  if p_batch = 'X'.
    INSERT FEBKO.
    IF    SY-SUBRC <> 0. MESSAGE E437(DS) WITH TEXT-206. ENDIF.
  endif.


  MOVE-CORRESPONDING FEBKO TO FEBVW.
  febvw-banks         = i_febko-banks.
  FEBVW-BANKL         = FEBKO-ABSND+0(15).
  PERFORM GET_FEBVW_BANKA.                          " BANK NAME

  if p_batch = 'X'.
    INSERT FEBVW.
    IF    SY-SUBRC <> 0. MESSAGE E437(DS) WITH TEXT-208. ENDIF.
  endif.

*  PERFORM APPEND_S_KUKEY.
*  PERFORM GET_ESNUM.


ENDFORM.                               " febko_insert
*&---------------------------------------------------------------------*
*&      FORM  INSERT_FEBEP
*&---------------------------------------------------------------------*
FORM INSERT_FEBEP.
  CLEAR FEBEP.

  MOVE-CORRESPONDING i_febep TO FEBEP.
* FEBEP-VALUT     =   i_febep-VALUT.     " VALUE DATE
* FEBEP-BUDAT     =   i_febep-BUDAT.     " POSTING DATE
* FEBEP-KWBTR     = i_febep-KWBTR.        " AMOUNT

* KEY
  FEBEP-KUKEY     =  FEBKO-KUKEY.
  FEBEP-ESNUM     =  FEBEP-ESNUM.

  PERFORM GET_YEAR   USING i_febep-BUDAT   FEBEP-GJAHR.

* ASSIGNMENT
  FEBEP-ZUONR+0(8) = FEBEP-KUKEY.
  FEBEP-ZUONR+8(5) = FEBEP-ESNUM.

  concatenate '& - ' i_febep-BUTXT into FEBEP-SGTXT.
  REPLACE '&' WITH febep-vgext INTO febep-SGTXT.


  G_ESNUM     =  G_ESNUM + 1.
  febep-esnum = g_esnum.

  if p_batch = 'X'.

    INSERT FEBEP.

    IF SY-SUBRC <> 0.
      MESSAGE E437(DS) WITH 'Error in INSERT FEBEP'.
    ENDIF.
  endif.

ENDFORM.                             " INSERT_FEBEP
*&---------------------------------------------------------------------*
*&      FORM  INSERT_FEBCL
*&---------------------------------------------------------------------*
*T021R		
*SL-AG	clearing key	
*01	WRBTR	amount
*02	BELNR	doc no
*03	BUDAT	doc.date
*04	MABER	dunn.area
*05	XBLNR	ref.no
*06	PYORD	pay order
*07	SAMNR	inv.list no
*08	BLART	doc type
*09	GSBER	buz.area
*10	MWSKZ	tax code
*11	FILKD	Account Number of the Branch
*12	WAERS	curr key
*13	BSCHL	posting key
*14	BLDAT	doc date
*15	ZUONR	assignment
*16	VBELN	SD doc. No

FORM INSERT_FEBCL.
  MOVE-CORRESPONDING i_febcl TO FEBCL.

* KEY
  FEBCL-KUKEY     =  FEBKO-KUKEY.
  FEBCL-ESNUM     =  FEBEP-ESNUM.

  FEBCL-AGBUK     =  P_BUKRS.
  FEBCL-KOART     =  i_febep-AVKOA.
  FEBCL-AGKON     =  FEBEP-AVKON.
  FEBCL-ESNUM     =  FEBEP-ESNUM.

*  IF i_febcl-ZITMKY = '1'.            " Invoice
*    FEBCL-SELFD   = 'ZUONR'.            " selection field
*    FEBCL-SELVON  = i_febcl-ZINVNO.    " from value
*  ELSE.
*    FEBCL-SELFD   = 'WRBTR'.            " selection field
*    FEBCL-SELVON  = i_febcl-ZIAMT.      " from value
*  ENDIF.

* CHECK i_febcl-ZITMKY = '1'.        " Invoice

  INSERT FEBCL.

  IF SY-SUBRC <> 0.
    MESSAGE E437(DS) WITH 'Error in INSERT FEBCL'.
  ENDIF.

* Insert additional Information into CBO Table
*  MOVE-CORRESPONDING i_febcl TO ZFEBCL.
*  INSERT ZFEBCL.
  IF SY-SUBRC <> 0.
    MESSAGE E437(DS) WITH 'Error in INSERT ZFEBCL'.
  ENDIF.

ENDFORM.                             " INSERT_FEBCL

*&---------------------------------------------------------------------*
*&      FORM  INSERT_FEBCL
*&---------------------------------------------------------------------*
FORM GET_YEAR  USING    F_DATE
               CHANGING F_YEAR.

  SELECT SINGLE * FROM T001
    WHERE BUKRS = P_BUKRS.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
              I_DATE     = F_DATE
              I_PERIV    = T001-PERIV
    IMPORTING
*             E_BUPER    = BUPER   " month
              E_GJAHR    = F_YEAR
    EXCEPTIONS T009_NOTFOUND    = 4
               T009B_NOTFOUND   = 4
               INPUT_FALSE      = 4.

  IF SY-SUBRC = 4.
    F_YEAR = 2000.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
