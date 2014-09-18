************************************************************************
* Program Name      : ZCQM09C_INSPECT_CHAR_UPLOAD
* Author            : SeungLyong, Lee
* Creation Date     : 2003.08.04.
* Specifications By : SeungLyong, Lee
* Pattern           : 2.3 Call Transaction
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Master Inspection Characteristic Uploading
*
*
* Related Table List: QPMK - Inspection characteristic master
*                     QPMT - Master Inspection Characteristics Texts
*                     QPMZ - Assignment table - insp. methods/master
*                            insp. characteristic
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZCQM09C_INSPECT_CHAR_UPLOAD   .



DATA: BEGIN OF IT_INSP OCCURS 0,
*-- SAPMQSDA	0100  QS21 Initial Screen
         WERKS        TYPE WERKS,    "Plant
         MKMNR        TYPE QMERKNR,  "Master Inspection Characteristic
         GUELTIGAB(10),               "Valid-from Date
*-- SAPMQSDA	0101 General Data
         QUANTITAET    LIKE RMQSD-QUANTITAET,  "Quantitative Char.=>C.B
         QUALITAET     LIKE RMQSD-QUALITAET,   "Qualitative char.=>C.B
         LOEKZ         LIKE QPMK-LOEKZ,    "Status of Master Record=>L.B
         KONSISTENT    LIKE QPMK-KONSISTENT,   "Copy Model         =>L.B
*                                               Reference,      Char.
         KURZTEXT      LIKE QPMT-KURZTEXT,     "Short text-CHAR 40
         SORTFELD      LIKE QPMK-SORTFELD,     "Search field
         PRFQL         LIKE QPMK-PRFQL,        "Inspector qualif.
         MERKGEW       LIKE QPMK-MERKGEW,    "Weighting of charac.=>L.B
         DUMMY10       LIKE QPMK-DUMMY10,      "InfoField 1
         DUMMY20       LIKE QPMK-DUMMY20,      "InfoField 2
         DUMMY40       LIKE QPMK-DUMMY40,      "InfoField 3

*-- SAPLQSS0	0100 Control Indicator 1=>All Check Box or Radio Buttons
     PRUEFKAT    LIKE RQMST-PRUEFKAT,   "Reference to Char Attr Required
     TOLERUNTEN  LIKE RQMST-TOLERUNTEN, "Lower Specification Limit
     TOLEROBEN   LIKE RQMST-TOLEROBEN,  "Upper Specification Limit
     SOLLPRUEF   LIKE RQMST-SOLLPRUEF,  "Check Target Value
     STICHPR     LIKE RQMST-STICHPR,    "Sampling Procedure is Required
     ADDPRO      LIKE RQMST-ADDPRO,     "Sample Quantity Is Added
     QSPCMK      LIKE RQMST-QSPCMK,     "SPC Characteristic
     ZERSTPRF    LIKE RQMST-ZERSTPRF,   "Destructive Inspection.
     ESTUKZ5     LIKE RQMST-ESTUKZ5,    "Summarized Char. Recording.
     ESTUKZ3     LIKE RQMST-ESTUKZ3,    "Record Single Results
     ESTUKZ1     LIKE RQMST-ESTUKZ1,    "No Characteristics Recording
     ESTUKZ2     LIKE RQMST-ESTUKZ2,    "Classed Char Recording
     FEHLREC     LIKE RQMST-FEHLREC,    "Defects Recording
                                        "Automatically Called Up
     RZWANG4     LIKE RQMST-RZWANG4,    "Required Characteristic
     RZWANG1     LIKE RQMST-RZWANG1,    "Optional Characteristic
     RZWANG2     LIKE RQMST-RZWANG2,  "Conditional Char After Acceptance
     RZWANG3     LIKE RQMST-RZWANG3,  "Conditional Char After Rejection

*-- SAPLQSS0	0101 Control Indicator 2=>All Check Box or Radio Buttons
    PUMFKZ1    LIKE RQMST-PUMFKZ1,  "Inspection Scope not Verified
    PUMFKZ4    LIKE RQMST-PUMFKZ4,  "Inspection Scope Must Be Adhered To
    PUMFKZ2    LIKE RQMST-PUMFKZ2,  "Sample Scope Can Be Smaller
    PUMFKZ3    LIKE RQMST-PUMFKZ3,  "Inspection Scope Can Be Larger
    DOKUKZ1    LIKE RQMST-DOKUKZ1,  "No Documentation Required
    DOKUKZ2    LIKE RQMST-DOKUKZ2,  "Documentation Required if Rejected
    DOKUKZ3    LIKE RQMST-DOKUKZ3,  "Documentation Required

    LZEITKZ    LIKE RQMST-LZEITKZ,  "Long-Term Inspection
    AUSSLOS    LIKE RQMST-AUSSLOS,  "Char Relevant for Qual. Score
                                    "and Scrap Share
    AENDBELEG  LIKE RQMST-AENDBELEG,  "Create Change Documents
                                      "During Results Recording
    PMMZWANG   LIKE RQMST-PMMZWANG,  "Assignment of Test
                                    "Equipment Required

    MESSWERTE  LIKE RQMST-MESSWERTE,  "Measured Values Must Be Recorded
    FORMELMK   LIKE RQMST-FORMELMK,  "Calculated Characteristic

    DRUCK1     LIKE RQMST-DRUCK1,      "Inspection Char Is Printed
    DRUCK2     LIKE RQMST-DRUCK2,      "Inspection Char Is Not Printed
    DRUCK3     LIKE RQMST-DRUCK3,      "Inspection Char Is Not Printed
                                       "at Skip Stage

*-- SAPMQSDA	0108 Quantitative Data => Only Quantitative
    STELLEN(3),     " LIKE QPMK-STELLEN,       "Decimal places
    MASSEINHSW   LIKE RMQSD-MASSEINHSW,    "Unit of measure
    SOLLWERT(23),     "LIKE QPMK-SOLLWERT,      "Target value
    TOLERANZUN(23),   "LIKE QPMK-TOLERANZUN,    "Lower tol.limit
    TOLERANZOB(23),   "LIKE QPMK-TOLERANZOB,    "Upper specif. limit

*-- SAPMQSDA	0106 Assigned Inspection Catalogs => Both Quanti. Quali
    CODEGRQUAL  LIKE  QPMK-CODEGRQUAL,    "Defect Code for Rejct.
    CODEQUAL    LIKE  QPMK-CODEQUAL,      "Defect Code Group for Gen.Rej

*  Belows fields for Only Qualitative
    KATAB1       LIKE QPMZ-KATAB1,     "Catalog Entry  Selected Set
    KATALGART1   LIKE QPMZ-KATALGART1, "Catalog Type of Assigned Code
                                       "Group or Selected Set
    AUSWMENGE1   LIKE QPMZ-AUSWMENGE1, "CodeGrp    SelSet
    AUSWMGWRK1   LIKE QPMZ-AUSWMGWRK1, "Plant
*  Belows fields for Both Quantirative  and Qualitative
    KATAB2        TYPE QKTTAB,
    KATALGART2    TYPE QKATAUSW,
    AUSWMENGE2    TYPE QCGRAUSW,
    AUSWMGWRK2    TYPE QWERKAUSW,
    KATAB3        TYPE QKTTAB,
    KATALGART3    TYPE QKATAUSW,
    AUSWMENGE3    TYPE QCGRAUSW,
    AUSWMGWRK3    TYPE QWERKAUSW,
    KATAB4        TYPE QKTTAB,
    KATALGART4    TYPE QKATAUSW,
    AUSWMENGE4    TYPE QCGRAUSW,
    AUSWMGWRK4    TYPE QWERKAUSW,
    KATAB5        TYPE QKTTAB,
    KATALGART5    TYPE QKATAUSW,
    AUSWMENGE5    TYPE QCGRAUSW,
    AUSWMGWRK5    TYPE QWERKAUSW,

*-- SAPMQSDA 0103 Assign inspection method. (Added (12/12/2003)by Moon)
      PMTNR         TYPE QPMETHODE,  "/Method
      PMWERK        TYPE QWERKPMET,  "/Plant
      PMVERSION     TYPE QVERSNR,    "/version

 END OF IT_INSP.

DATA: LO_WERKS(4).
DATA: FIELDNAME TYPE STRING,
      FILE_NAME LIKE IBIPPARMS-PATH.
* BDC
DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.
DATA   BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA   END OF IT_MSG.

*//Data(Global Fileds) ;(WA_)  ==> Local? ?? ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)
DATA : WA_REPID LIKE SYST-REPID,
       WA_DYNNR LIKE SYST-DYNNR.


*//Constants
CONSTANTS : CO_MARK TYPE C VALUE 'X'.

*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME TITLE TEXT-T01.
PARAMETERS : P_FILE   LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN ULINE.
PARAMETERS : P_MODE   TYPE TB_BDCMODE DEFAULT 'A' AS LISTBOX
                                        VISIBLE LENGTH 25.

SELECTION-SCREEN END OF BLOCK BLK .

AT SELECTION-SCREEN OUTPUT.
SET TITLEBAR '1000'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  WA_REPID = SY-REPID.
  WA_DYNNR = SY-DYNNR.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            PROGRAM_NAME  = WA_REPID
            DYNPRO_NUMBER = WA_DYNNR
            FIELD_NAME    = ' '
            STATIC        = ' '
            MASK          = ' '
       CHANGING
            FILE_NAME     = P_FILE
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

START-OF-SELECTION.

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_INSP
                          USING    P_FILE.

*  SORT IT_INSP BY WERK MKMNR.

END-OF-SELECTION.
*---
  PERFORM PROCESS_BDC.
*---

*&-----------------------------------------------------------------*
*&      Form  DYNPRO
*&-----------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-PROGRAM,
           VALUE TO BDC_TAB-DYNPRO,
           'X'   TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-FNAM,
           VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.                    "DYNPRO
*&------------------------------------------------------------------*
*&      Form  F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
FORM F_EXCEL_UPLOAD  TABLES   P_TABLE
                     USING    P_FILENAME  LIKE RLGRAP-FILENAME.

*  DATA : LO_ITAB TYPE  KCDE_CELLS OCCURS 0 WITH HEADER LINE.
*  DATA : LO_ITAB TYPE ZPS_CO_ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LO_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LO_INDEX LIKE SY-TABIX.
  DATA : LO_START_COL TYPE I VALUE '1',
         LO_START_ROW TYPE I VALUE '1',
         LO_END_COL   TYPE I VALUE '256',
         LO_END_ROW   TYPE I VALUE '65536'.
  FIELD-SYMBOLS : <LO_FS>.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FILENAME
            I_BEGIN_COL             = LO_START_COL
            I_BEGIN_ROW             = LO_START_ROW
            I_END_COL               = LO_END_COL
            I_END_ROW               = LO_END_ROW
       TABLES
            INTERN                  = LO_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMQM) WITH  TEXT-E01. "'File Upload Failed !'.
    STOP.
  ENDIF.

  DELETE LO_ITAB WHERE ROW LE 4.

  CHECK NOT LO_ITAB[] IS INITIAL.

  SORT LO_ITAB BY ROW COL.

*-- Transfer Data to Internal Table from Excel Input Tables
  LOOP AT LO_ITAB.
    MOVE : LO_ITAB-COL TO LO_INDEX.
    ASSIGN COMPONENT LO_INDEX OF STRUCTURE P_TABLE TO <LO_FS>.
    MOVE : LO_ITAB-VALUE TO <LO_FS>.
    AT END OF ROW.
      APPEND P_TABLE.
      CLEAR P_TABLE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
*&      Form  PROCESS_BDC
*&-----------------------------------------------------------------*
FORM PROCESS_BDC .

  DATA : LW_INSP LIKE IT_INSP.
  DATA : LO_FNAM_RSTXT  LIKE BDCDATA-FNAM,
         LO_FNAM_TXLINE LIKE BDCDATA-FNAM.
  DATA : LO_CNT(2) TYPE N.

  LOOP AT IT_INSP.

    MOVE-CORRESPONDING : IT_INSP TO LW_INSP. "Back up Current Record

    REFRESH BDC_TAB.

*-- SAPMQSDA	0100  QS21 Initial Screen
    PERFORM DYNPRO USING :
       'X'     'SAPMQSDA'               '0100'   ,
       ' '     'BDC_OKCODE'             '=ST',              "'/00',
       ' '     'QPMK-WERKS'             LW_INSP-WERKS,
       ' '     'QPMK-MKMNR'             LW_INSP-MKMNR,
       ' '     'QPMK-GUELTIGAB'         LW_INSP-GUELTIGAB. " or SY-DATUM

*-- SAPMQSDA	0101 General Data
    PERFORM DYNPRO USING :
       'X'      'SAPMQSDA'             '0101',
       ' '      'BDC_OKCODE'           '/00',
*      -- Quantitative or Qualitative Characterist Selection
       ' '      'RMQSD-QUANTITAET'     LW_INSP-QUANTITAET,
       ' '      'RMQSD-QUALITAET'      LW_INSP-QUALITAET,

       ' '      'QPMK-LOEKZ'           LW_INSP-LOEKZ,  " '2'-Released
       ' '      'QPMK-KONSISTENT'      LW_INSP-KONSISTENT,

       ' '      'VSKT-KURZTEXT'         LW_INSP-KURZTEXT,
       ' '      'QPMK-SORTFELD'         LW_INSP-SORTFELD,

       ' '      'QPMK-PRFQL'            LW_INSP-PRFQL,
       ' '      'QPMK-MERKGEW'          LW_INSP-MERKGEW,
       ' '      'QPMK-DUMMY10'          LW_INSP-DUMMY10,
       ' '      'QPMK-DUMMY20'          LW_INSP-DUMMY20,
       ' '      'QPMK-DUMMY40'          LW_INSP-DUMMY40.


*-- SAPLQSS0	0100 Control Indicator 1=>All Check Box or Radio Buttons
    PERFORM DYNPRO USING :
       'X'      'SAPLQSS0'               '0100',
       ' '      'BDC_OKCODE'            '=ENT1'.

    CASE CO_MARK.

      WHEN LW_INSP-QUALITAET.
        PERFORM DYNPRO USING :
         ' '      'RQMST-PRUEFKAT'         LW_INSP-PRUEFKAT.     "

      WHEN LW_INSP-QUANTITAET.
        PERFORM DYNPRO USING :
         ' '      'RQMST-TOLERUNTEN'       LW_INSP-TOLERUNTEN,
         ' '      'RQMST-TOLEROBEN'        LW_INSP-TOLEROBEN,
         ' '      'RQMST-SOLLPRUEF'        LW_INSP-SOLLPRUEF.
    ENDCASE.

    PERFORM DYNPRO USING :
       ' '      'RQMST-STICHPR'          LW_INSP-STICHPR,     "'X'
       ' '      'RQMST-ADDPRO'           LW_INSP-ADDPRO,
       ' '      'RQMST-QSPCMK'           LW_INSP-QSPCMK,
       ' '      'RQMST-ZERSTPRF'         LW_INSP-ZERSTPRF,
       ' '      'RQMST-ESTUKZ5'          LW_INSP-ESTUKZ5,    "'X'
       ' '      'RQMST-ESTUKZ3'          LW_INSP-ESTUKZ3,
       ' '      'RQMST-ESTUKZ1'          LW_INSP-ESTUKZ1,
       ' '      'RQMST-ESTUKZ2'          LW_INSP-ESTUKZ2,
       ' '      'RQMST-FEHLREC'          LW_INSP-FEHLREC,
       ' '      'RQMST-RZWANG4'          LW_INSP-RZWANG4,    "'X'
       ' '      'RQMST-RZWANG1'          LW_INSP-RZWANG1,
       ' '      'RQMST-RZWANG2'          LW_INSP-RZWANG2,
       ' '      'RQMST-RZWANG3'          LW_INSP-RZWANG3.

*-- SAPLQSS0	0101 Control Indicator 2=>All Check Box or Radio Buttons
    PERFORM DYNPRO USING :
       'X'      'SAPLQSS0'              '0101'   ,
       ' '      'BDC_OKCODE'            '=ENT1',
*       ' '      'RQMST-DOKUKZ1'         'X',
*       ' '      'RQMST-PUMFKZ4'         'X',
*       ' '      'RQMST-DRUCK1'          'X'.
      ' '       'RQMST-PUMFKZ1'         LW_INSP-PUMFKZ1,  "
      ' '       'RQMST-PUMFKZ4'         LW_INSP-PUMFKZ4,  "Fixed scope
      ' '       'RQMST-PUMFKZ2'         LW_INSP-PUMFKZ2,  "
      ' '       'RQMST-PUMFKZ3'         LW_INSP-PUMFKZ3,  "
      ' '       'RQMST-DOKUKZ1'         LW_INSP-DOKUKZ1,  "No Docu.
      ' '       'RQMST-DOKUKZ2'         LW_INSP-DOKUKZ2,  "
      ' '       'RQMST-DOKUKZ3'         LW_INSP-DOKUKZ3, "Docu. Required
      ' '       'RQMST-LZEITKZ'         LW_INSP-LZEITKZ,  "Long-Term
      ' '       'RQMST-AUSSLOS'         LW_INSP-AUSSLOS,  "Char Relevant
      ' '       'RQMST-AENDBELEG'       LW_INSP-AENDBELEG,  "
      ' '       'RQMST-PMMZWANG'        LW_INSP-PMMZWANG.  "Assignment

    IF LW_INSP-QUANTITAET = CO_MARK.
      PERFORM DYNPRO USING :
       ' '       'RQMST-MESSWERTE'       LW_INSP-MESSWERTE,  "Measured
       ' '       'RQMST-FORMELMK'        LW_INSP-FORMELMK.  "Calculated
    ENDIF.

    PERFORM DYNPRO USING :
        ' '       'RQMST-DRUCK1'          LW_INSP-DRUCK1,    "Inspection
        ' '       'RQMST-DRUCK2'          LW_INSP-DRUCK2,   "Inspection
        ' '       'RQMST-DRUCK3'          LW_INSP-DRUCK3.   "Inspection



***-- Only Quantitative Data
    IF LW_INSP-QUANTITAET = CO_MARK.

*-- SAPMQSDA	0110	
      PERFORM DYNPRO USING :
        'X'       'SAPMQSDA'             '0110',
        ' '       'BDC_OKCODE'           '=WEIT'.

*-- SAPMQSDA	0108 Quantitative Data => Only Quantitative
      PERFORM DYNPRO USING :
        'X'       'SAPMQSDA'              '0108',
        ' '       'BDC_OKCODE'            '=WEIT',
      ' '       'QPMK-STELLEN'          LW_INSP-STELLEN,  "Decimalplaces
        ' '       'RMQSD-MASSEINHSW'      LW_INSP-MASSEINHSW, "Unit
       ' '       'QFLTP-SOLLWERT'        LW_INSP-SOLLWERT, "Target value
        ' '       'QFLTP-TOLERANZUN'      LW_INSP-TOLERANZUN,"Lower
        ' '       'QFLTP-TOLERANZOB'      LW_INSP-TOLERANZOB. "Upper
    ENDIF.


**// Assigned Inspection Catalogs
    IF LW_INSP-QUANTITAET = CO_MARK.
*-- SAPMQSDA	0101
      PERFORM DYNPRO USING :
        'X'       'SAPMQSDA'              '0101',
        ' '       'BDC_OKCODE'            '=KT'.
    ENDIF.

*-- SAPMQSDA	0106 Assigned Inspection Catalogs => Both Quanti. Quali
    PERFORM DYNPRO USING :
       'X'      'SAPMQSDA'               '0106',
       ' '      'BDC_OKCODE'             '=WEIT'.
    PERFORM DYNPRO USING :
       ' '      'QPMK-CODEGRQUAL'        LW_INSP-CODEGRQUAL,
       ' '      'QPMK-CODEQUAL'          LW_INSP-CODEQUAL.


    IF LW_INSP-QUALITAET = CO_MARK.

*  Belows fields for Only Qualitative
      PERFORM DYNPRO USING :
*         ' '      'QPMZ-KATAB1'       LW_INSP-KATAB1,
*         ' '      'QPMZ-KATALGART1'   LW_INSP-KATALGART1,
         ' '      'QPMZ-AUSWMENGE1'   LW_INSP-AUSWMENGE1,
         ' '      'QPMZ-AUSWMGWRK1'   LW_INSP-AUSWMGWRK1.
    ENDIF.

    IF LW_INSP-QUANTITAET = CO_MARK OR
       LW_INSP-QUALITAET  = CO_MARK.

*  Belows fields for Both Quantirative  and Qualitative
      PERFORM DYNPRO USING :
         ' '      'QPMZ-KATAB2'        LW_INSP-KATAB2,
         ' '      'QPMZ-KATALGART2'    LW_INSP-KATALGART2,
         ' '      'QPMZ-AUSWMENGE2'    LW_INSP-AUSWMENGE2,
         ' '      'QPMZ-AUSWMGWRK2'    LW_INSP-AUSWMGWRK2,

         ' '      'QPMZ-KATAB3'        LW_INSP-KATAB3,
         ' '      'QPMZ-KATALGART3'    LW_INSP-KATALGART3,
         ' '      'QPMZ-AUSWMENGE3'    LW_INSP-AUSWMENGE3,
         ' '      'QPMZ-AUSWMGWRK3'    LW_INSP-AUSWMGWRK3,

         ' '      'QPMZ-KATAB4'        LW_INSP-KATAB4,
         ' '      'QPMZ-KATALGART4'    LW_INSP-KATALGART4,
         ' '      'QPMZ-AUSWMENGE4'    LW_INSP-AUSWMENGE4,
         ' '      'QPMZ-AUSWMGWRK4'    LW_INSP-AUSWMGWRK4,

         ' '      'QPMZ-KATAB5'        LW_INSP-KATAB5,
         ' '      'QPMZ-KATALGART5'    LW_INSP-KATALGART5,
         ' '      'QPMZ-AUSWMENGE5'    LW_INSP-AUSWMENGE5,
         ' '      'QPMZ-AUSWMGWRK5'    LW_INSP-AUSWMGWRK5.
    ENDIF.

    AT END OF MKMNR.

*///// Added - 12/12/2003 by request from Mr. Moon. : Start
      IF NOT LW_INSP-PMTNR IS INITIAL.
*-- SAPMQSDA	0101 General Data
        PERFORM DYNPRO USING :
           'X'      'SAPMQSDA'         '0101',
           ' '      'BDC_OKCODE'       '=ML'.  "/Click Insp. Methods box


*-- SAPMQSDA	0103
        PERFORM DYNPRO USING :
           'X'      'SAPMQSDA'             '0103',
           ' '      'BDC_OKCODE'           '=WEIT',
           ' '      'RMQSD-PMTNR(01)'       LW_INSP-PMTNR,
           ' '      'RMQSD-PMWERK(01)'      LW_INSP-PMWERK,
           ' '      'RMQSD-PMVERSION(01)'   LW_INSP-PMVERSION.

      ENDIF.
*///// Added - 12/12/2003 by request from Mr. Moon. : end


      PERFORM DYNPRO USING :
        'X'       'SAPMQSDA'                '0101',
        ' '       'BDC_OKCODE'              '=BU'.

*- TRANSACTION
      CALL TRANSACTION 'QS21'    USING BDC_TAB
                                 UPDATE 'S'
                                 MODE P_MODE
                              MESSAGES INTO IT_MSG.
      READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.

      IF SY-SUBRC = 0.
        ROLLBACK WORK.
        MESSAGE E000(ZMQM) WITH TEXT-E02. "'BDC Error Found!'.
        EXIT.
      ENDIF.


    ENDAT.

  ENDLOOP.

  IF SY-SUBRC  NE 0.
    MESSAGE E000(ZMQM) WITH TEXT-E02. "'BDC Error Found!'.
    EXIT.
  ENDIF.

  COMMIT WORK.
  MESSAGE S000(ZMQM) WITH TEXT-S01. "'Successfully Processed. BDC'.


ENDFORM.                    " PROCESS_BDC
