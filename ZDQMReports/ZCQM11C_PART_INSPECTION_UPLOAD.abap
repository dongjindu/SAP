************************************************************************
* Program Name      : ZCQM11C_PART_INSPECTION_UPLOAD
* Author            : SeungLyong, Lee
* Creation Date     : 2003.08.08.
* Specifications By : SeungLyong, Lee
* Development Request No : UD1K901760
* Addl Documentation:
* Description       : Part Inspection Plan Uploading
*
*
* Related Table List:
*   MAPL - Assignment of Task Lists to Materials
*   PLMK - Inspection plan characteristics
*   PLMW - MAPL-Dependent Charac. Specifications (Inspection Plan)
*   PLPO - Task list - operation/activity
*   PLKO - Task list - header
*   MARA - Material Master
*   MARC - Plant Data for Material
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************


REPORT  ZCQM11C_PART_INSPECTION_UPLOAD.


*-- Declaration Tables
TABLES : MAPL, PLKO, PLPO, PLMK, MARA, MARC.

* BDC Tables
DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.
DATA   BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA   END OF IT_MSG.

DATA: BEGIN OF IT_QMPLN OCCURS 0,
*-- SAPLCPDI	8010 - QP01 - Initial Screen.
    MATNR       LIKE  RC27M-MATNR,   "Material
    WERKS       LIKE  RC27M-WERKS,   "Plant
    PLNNR       LIKE  RC271-PLNNR,   "Key for task list group =>CLEAR' '
    PLNAL       TYPE  PLNAL,         "Group Counters
*    STTAG       LIKE  RC271-STTAG,   "Key date => sy-datum
*-- SAPLCPDA	1200 - Inspection Plan - Header Details
    KTEXT	LIKE  PLKOD-KTEXT,        "Task list description
    VERWE	LIKE  PLKOD-VERWE,        "Task list usage
    STATU	LIKE  PLKOD-STATU,        "Status
    SLWBEZ	LIKE  PLKOD-SLWBEZ,       "Inspection Point
*    QKZRASTER	LIKE  PLKOD-QKZRASTER,    "Inspection Point
*-- SAPLCPDI	1400 - Operation Overview
    VORNR	LIKE  PLPOD-VORNR,	"Operation Number
*    ARBPL(8)    like plpod-arbpl,       " workcenter
    STEUS	LIKE  PLPOD-STEUS,	"Control key
    LTXA1	LIKE  PLPOD-LTXA1,	"Short text
*-- SAPLCPDO	1200 - Operation Details
    ERFSICHT	LIKE  PLPOD-ERFSICHT,	"Recording View => '12'
                           " 12 - Single values and summarized results
    QPPKTABS	LIKE  PLPOD-QPPKTABS,	"Flow Variants for  =>'2'
                                        "Inspection Point Completion
                                        " 2 - Valuation
*-- SAPLQPAA	0150 - Characteristic Overview
    MERKNR	LIKE  PLMKB-MERKNR,	"Inspection Characteristic
                                        "Number
    VERWMERKM	LIKE  PLMKB-VERWMERKM,	"Master Inspection
                                       "Characteristics
    MKVERSION	LIKE  PLMKB-MKVERSION,	"Version Number of the
                                        "Master Insp. Characteristic
    STICHPRVER	LIKE  PLMKB-STICHPRVER,	"Sampling Procedure
                                        "in Inspection Characteristic

 END OF IT_QMPLN.

**- Structure for IT_QMPLN Modfy for Using At Event
*-- => ST_MATASK must same from MATNR to SLWBEZ of IT_QMPLN Structure.
*-- => ST_OPT    must same from VORNR to QPPKTABS of IT_QMPLN Structure.
DATA : BEGIN OF ST_MATASK,
    MATNR       LIKE  RC27M-MATNR,   "Material
    WERKS       LIKE  RC27M-WERKS,   "Plant
    PLNNR       LIKE  RC271-PLNNR,   "Key for task list group =>CLEAR' '
    PLNAL       TYPE  PLNAL,         "Group Counters
    KTEXT	LIKE  PLKOD-KTEXT,        "Task list description
    VERWE	LIKE  PLKOD-VERWE,        "Task list usage
    STATU	LIKE  PLKOD-STATU,        "Status
    SLWBEZ	LIKE  PLKOD-SLWBEZ,       "Inspection Point
       END OF ST_MATASK.

DATA : BEGIN OF ST_OPT,
    VORNR	LIKE  PLPOD-VORNR,	"Operation Number
*    ARBPL(8)    like plpod-arbpl,       " workcenter
    STEUS	LIKE  PLPOD-STEUS,	"Control key
    LTXA1	LIKE  PLPOD-LTXA1,	"Short text
    ERFSICHT	LIKE  PLPOD-ERFSICHT,	"Recording View => '12'
                           " 12 - Single values and summarized results
    QPPKTABS	LIKE  PLPOD-QPPKTABS,	"Flow Variants for  =>'2'
                                        "Inspection Point Completion
       END OF ST_OPT.


*//Data(Global Fileds) ;(WA_)  == Local? ?? ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)
DATA : WA_REPID LIKE SYST-REPID,
       WA_DYNNR LIKE SYST-DYNNR.

*// Structures
DATA : BEGIN OF ST_GRCNT,
           PLNNR TYPE PLNNR,   "GROUP
           PLNAL TYPE PLNAL,   "GROUP COUNTER.
       END OF ST_GRCNT.

DATA : BEGIN OF ST_OPER,
         PLNNR   TYPE  PLNNR,     "GROUP
         PLNKN   TYPE  PLNKN,     "Nodes
         ZAEHL   TYPE  CIM_COUNT,
         VORNR   TYPE  VORNR,
         STEUS   TYPE  STEUS,
         WERKS   TYPE  WERKS_D,
         LTXA1   TYPE  LTXA1,
       END OF ST_OPER.

**-- Constants
CONSTANTS : CO_MARK  TYPE C VALUE 'X'.

*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : PA_FILE   LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN ULINE.
PARAMETERS : PA_MODE   TYPE TB_BDCMODE DEFAULT 'A' AS LISTBOX
                                         VISIBLE LENGTH 25.

SELECTION-SCREEN END OF BLOCK BLK .


AT SELECTION-SCREEN ON VALUE-REQUEST FOR PA_FILE.
  PERFORM GET_SELECT_EXCEL_FILE.


START-OF-SELECTION.

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_QMPLN
                          USING    PA_FILE.

  PERFORM MODIFY_IT_QMPLN_FOR_BDC.

END-OF-SELECTION.

  PERFORM PROCESS_BDC.


*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
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
    MESSAGE E000(ZMPS) WITH 'File Upload Failed !'(E10).
    STOP.
  ENDIF.
*-- Delete Header line : for Dev. Test - Start
*--    It will be Remark before Delivering
  DELETE LO_ITAB WHERE ROW LE 2.
*-- Delete Header line - end


  CHECK NOT LO_ITAB[] IS INITIAL.

  SORT LO_ITAB BY ROW COL.
  REFRESH P_TABLE.

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

  DATA : LWA_QMPLN   LIKE IT_QMPLN.
  DATA : LWA_QMPLN_T LIKE IT_QMPLN. "/for Back up Previous Header.
  DATA : LV_PLNNR TYPE PLNNR.
  DATA : LV_PLNAL TYPE PLNAL.

*  DATA : LO_FNAM_MKVERSION  LIKE BDCDATA-FNAM,
*         LO_FNAM_MERKNR     LIKE BDCDATA-FNAM,
*         LO_FNAM_VERWMERKM  LIKE BDCDATA-FNAM,
*         LO_FNAM_KURZTEXT   LIKE BDCDATA-FNAM,
*         LO_FNAM_STICHPRVER LIKE BDCDATA-FNAM,
*         LO_FNAM_PMETHODE   LIKE BDCDATA-FNAM,
*         LO_FNAM_QMTB_WERKS LIKE BDCDATA-FNAM.
*
*  DATA : LO_FNAM_ARBPL    LIKE BDCDATA-FNAM,
*         LO_FNAM_STEUS    LIKE BDCDATA-FNAM,
*         LO_FNAM_LTXA1    LIKE BDCDATA-FNAM,
*         LO_FNAM_FLG_SEL  LIKE BDCDATA-FNAM.

*  DATA : LO_CNT(2) TYPE N.
*  DATA : LO_CNTN(2) TYPE N.
*  DATA : LO_CNT_RT(2) TYPE N.
*  DATA : LO_CNT_RT2(2) TYPE N.

  DATA :  LT_GRCNT LIKE ST_GRCNT OCCURS 3 WITH HEADER LINE.
  DATA :  LV_GR_EXIT TYPE C. "/'X'-EXIST ' '-NO EXIST
  DATA :  LT_OPER  LIKE ST_OPER  OCCURS 5 WITH HEADER LINE.
  DATA : LV_ZAEHL TYPE CIM_COUNT.
  DATA : LV_CHAR_CNT(10) TYPE N.
  DATA : LV_OPER_CNT(10) TYPE N.

  DATA : LV_MATNR TYPE MATNR.

  LOOP AT IT_QMPLN.

    MOVE-CORRESPONDING : IT_QMPLN TO LWA_QMPLN. "/Data Back up

**//each Material Number if ~ endif.

    AT NEW MATNR.

      REFRESH BDC_TAB.

**//-- SAPLCPDI	8010 - QP01 - Initial Screen.

      SET PARAMETER ID 'PLN' FIELD ''. "/Clear Key for task list Group
*-- Check up existence of Material in Plant
      PERFORM CHECK_EXIST_MATNR_IN_PLANT USING LWA_QMPLN-MATNR
                                               LWA_QMPLN-WERKS.

      PERFORM DYNPRO  USING:
        'X'   'SAPLCPDI'          '8010',
        ' '   'BDC_OKCODE'        '/00',
        ' '   'RC27M-MATNR'       LWA_QMPLN-MATNR,
        ' '   'RC27M-WERKS'       LWA_QMPLN-WERKS,
        ' '   'RC271-PLNNR'       '',
        ' '   'RC271-STTAG'       SY-DATUM.

*-- check up existence of Group

      CLEAR LV_PLNAL.
      REFRESH LT_GRCNT. CLEAR LT_GRCNT.

      PERFORM CHECK_EXIST_GROUP   TABLES   LT_GRCNT
                                  USING    LWA_QMPLN-MATNR
                                           LWA_QMPLN-WERKS
                                  CHANGING LV_PLNNR.

      CLEAR LT_GRCNT.

**--       Retrieve Operations of Group Counter for Material.
*        PERFORM RETRIEVE_EXIT_OPERATION  TABLES LT_OPER
*                                         USING  LT_GRCNT-PLNNR.

*    - If there is Group of Material, Screen 1200 will be displayed. So
*      It must Select that group counter item and Click Header Icon.

**-     - SAPLCPDI	1200 - Task List Overview : Select Group Counter

      IF LWA_QMPLN-PLNNR IS INITIAL.
*        IF LWA_QMPLN-PLNAL IS INITIAL.
        PERFORM DYNPRO  USING:
          'X'   'SAPLCPDI'          '1200',
          ' '   'BDC_OKCODE'        '=ANLG'.  "/Click New entries.

      ELSE.
*          - search Group Counter and move to top of Table Control
        PERFORM DYNPRO  USING:
          'X'   'SAPLCPDI'        '1200',
          ' '   'BDC_OKCODE'      '/00',  "/move Group counter to top
          ' '   'BDC_CURSOR'      'RC27X-ENTRY_ACT',
          ' '   'RC27X-ENTRY_ACT'  LWA_QMPLN-PLNAL.

*          - Select Group Counter and Click enter
        PERFORM DYNPRO  USING:
          'X'   'SAPLCPDI'          '1200',
          ' '   'BDC_OKCODE'        '=ALD1',  "/Click Header Data.
          ' '   'RC27X-FLG_SEL(01)' 'X'.

      ENDIF.


****// Header Detail for Selected or New Group Counter
      PERFORM DYNPRO  USING:
        'X'   'SAPLCPDA'          '1200',
        ' '   'BDC_OKCODE'        '=VOUE',         "//Click Operation
        ' '   'PLKOD-KTEXT'       LWA_QMPLN-KTEXT, "Short Text
        ' '   'PLKOD-VERWE'       LWA_QMPLN-VERWE, "Usage
        ' '   'PLKOD-STATU'       LWA_QMPLN-STATU, "Status
        ' '   'PLKOD-SLWBEZ'      LWA_QMPLN-SLWBEZ. "Inspection Points

    ENDAT.  "/End of AT NEW MATNR


**_ Operation Overview Screen
    AT NEW VORNR.

      IF  LV_OPER_CNT IS INITIAL.
        PERFORM DYNPRO  USING:     "//Input data of top operarion
           'X'   'SAPLCPDI'           '1400',
           ' '   'BDC_OKCODE'         '/00',
           ' '   'RC27X-FLG_SEL(01)'   CO_MARK,
           ' '   'PLPOD-STEUS(01)'     LWA_QMPLN-STEUS,
           ' '   'PLPOD-LTXA1(01)'     LWA_QMPLN-LTXA1.

        PERFORM DYNPRO  USING:    "//Select Item of top operarion
           'X'   'SAPLCPDI'           '1400',
           ' '   'BDC_OKCODE'         '=VOD1',   "/Click Oper Detail
           ' '   'RC27X-FLG_SEL(01)'   CO_MARK.
      ELSE.
        PERFORM DYNPRO  USING:    "//Deselect Item of top operarion
           'X'   'SAPLCPDI'           '1400',
           ' '   'BDC_OKCODE'         '/00',
           ' '   'RC27X-FLG_SEL(01)'   ' ',
           ' '   'RC27X-ENTRY_ACT'     LV_OPER_CNT.

        PERFORM DYNPRO  USING:     "//Input data of top operarion
           'X'   'SAPLCPDI'           '1400',
           ' '   'BDC_OKCODE'         '/00',
           ' '   'RC27X-FLG_SEL(02)'   CO_MARK,
           ' '   'PLPOD-STEUS(02)'     LWA_QMPLN-STEUS,
           ' '   'PLPOD-LTXA1(02)'     LWA_QMPLN-LTXA1.

        PERFORM DYNPRO  USING:    "//Select Item of top operarion
           'X'   'SAPLCPDI'           '1400',
           ' '   'BDC_OKCODE'         '=VOD1',   "/Click Oper Detail
           ' '   'RC27X-FLG_SEL(02)'   CO_MARK.

      ENDIF.

*--    SAPLCPDO	1200	X  -  Operation Detail

      PERFORM DYNPRO  USING:
           'X'   'SAPLCPDO'      '1200',
           ' '   'BDC_OKCODE'     '=QMUE', "Click Inspection Char
           ' '   'PLPOD-ERFSICHT'   '12',  "/Recording view:
                        "'12' - Single Values and Summarized resuls
           ' '   'PLPOD-QPPKTABS'    '2'. "/Flow variant for
   "                                         inspection point completion
      "                                          '2' -  Valuation

      CLEAR LV_CHAR_CNT.

*      SELECT COUNT( DISTINCT MERKNR ) INTO LV_CHAR_CNT
*         FROM PLMK
*           WHERE PLNTY = 'Q'
*             AND PLNNR = LT_GRCNT-PLNNR
*             AND PLNKN = LT_OPER-PLNKN.



    ENDAT. "/End of AT NEW VORNR.

*****"/  CHARACTERISTIC DATA HANDLING

    IF NOT LV_CHAR_CNT IS INITIAL. "/CHARACTERISTIC exist
      PERFORM DYNPRO  USING:                        "/
          'X'   'SAPLQPAA'        '0150',
          ' '   'BDC_CURSOR'      'RQPAS-ENTRY_ACT',
          ' '   'RQPAS-ENTRY_ACT' LV_CHAR_CNT,
          ' '   'BDC_OKCODE'      '/00'.

      PERFORM DYNPRO  USING:                        "/
      'X'   'SAPLQPAA'             '0150',
      ' '   'BDC_OKCODE'           '/00',
      ' '   'PLMKB-VERWMERKM(02)'   LWA_QMPLN-VERWMERKM,
      ' '   'PLMKB-MKVERSION(02)'   LWA_QMPLN-MKVERSION,
      ' '   'PLMKB-STICHPRVER(02)'   LWA_QMPLN-STICHPRVER.


    ELSE.
      PERFORM DYNPRO  USING:                        "/
          'X'   'SAPLQPAA'             '0150',
          ' '   'BDC_OKCODE'           '/00',
      ' '   'PLMKB-VERWMERKM(01)'   LWA_QMPLN-VERWMERKM,
      ' '   'PLMKB-MKVERSION(01)'   LWA_QMPLN-MKVERSION,
      ' '   'PLMKB-STICHPRVER(01)'   LWA_QMPLN-STICHPRVER.

    ENDIF.

*      SET PARAMETER ID 'WRK' FIELD ''.

    PERFORM DYNPRO  USING:   "/ POP-UP -  Delete Plant.
        'X'   'SAPLQPAA'             '1501',
        ' '   'BDC_OKCODE'           '=ENT1',
        ' '   'PLMKB-QMTB_WERKS'     '   '.    "/Plant Clear


    LV_CHAR_CNT = LV_CHAR_CNT + 1.


    AT END OF VORNR.
      PERFORM DYNPRO  USING:                        "/
          'X'   'SAPLQPAA'             '0150',
          ' '   'BDC_OKCODE'           '=QMBW'.

      PERFORM DYNPRO  USING:
           'X'   'SAPLCPDO'      '1200',
           ' '   'BDC_OKCODE'     '=BACK'.

      LV_OPER_CNT = LV_OPER_CNT + 1.
    ENDAT.


    AT END OF MATNR.
      PERFORM DYNPRO  USING:
       'X'   'SAPLCPDI'              '1400',
       ' '   'BDC_OKCODE'            '=BU'.

      PERFORM PROCESS_TRANSACTION.

      LV_MATNR = LWA_QMPLN-MATNR.

    ENDAT.

  ENDLOOP.

  IF SY-SUBRC  NE 0.
    MESSAGE E000(ZMQM) WITH 'BDC Error Founded!'(E10).
    EXIT.
  ENDIF.

  COMMIT WORK.
  MESSAGE S000(ZMQM) WITH 'Success Processed. BDC'(S01).


ENDFORM.                    " PROCESS_BDC
*&------------------------------------------------------------------*
*&      Form  get_select_Excel_file
*&------------------------------------------------------------------*
FORM GET_SELECT_EXCEL_FILE .
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
            FILE_NAME     = PA_FILE
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
  IF SY-SUBRC < 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " get_select_Excel_file
*&------------------------------------------------------------------*
*&      Form  CHECK_EXIST_MATNR_IN_PLANT
*&------------------------------------------------------------------*

FORM CHECK_EXIST_MATNR_IN_PLANT USING    P_MATNR
                                         P_WERKS.
  DATA : LWA_MATNR TYPE MATNR.

  TRANSLATE P_MATNR TO UPPER CASE.

*CONVERSION_EXIT_ALPHA
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_MATNR
       IMPORTING
            OUTPUT = P_MATNR.

  SELECT SINGLE *
     FROM MARC
       WHERE MATNR = P_MATNR
         AND WERKS = P_WERKS.

  IF SY-SUBRC NE 0.
    STOP.
    MESSAGE E000(ZMQM) WITH TEXT-E01 P_MATNR TEXT-E02 P_WERKS.
*                                          "/No Exist  MATNR in Plant
  ENDIF.

ENDFORM.                    " CHECK_EXIST_MATNR_IN_PLANT
*&-------------------------------------------------------------------*
*&      Form  CHECK_EXIST_GROUP
*&-------------------------------------------------------------------*
FORM CHECK_EXIST_GROUP TABLES   PT_GRCNT  STRUCTURE ST_GRCNT
                       USING    P_MATNR
                                P_WERKS
                       CHANGING P_PLNNR.

  SELECT PLNNR PLNAL INTO CORRESPONDING FIELDS OF TABLE PT_GRCNT
      FROM MAPL
         WHERE MATNR = P_MATNR
           AND WERKS = P_WERKS
           AND PLNTY = 'Q'                "/'Q'- Inspection Plan
           AND LOEKZ = ' '.

  CHECK SY-SUBRC = 0.

  READ TABLE PT_GRCNT INDEX 1.   "/for Existence Check Group.
  MOVE PT_GRCNT-PLNNR TO P_PLNNR.
  CLEAR PT_GRCNT.

ENDFORM.                    " CHECK_EXIST_GROUP
*&-----------------------------------------------------------------*
*&      Form  RETRIEVE_EXIT_OPERATION
*&-----------------------------------------------------------------*
FORM RETRIEVE_EXIT_OPERATION TABLES   PT_OPER STRUCTURE ST_OPER
                             USING    P_PLNNR.

  REFRESH PT_OPER. CLEAR PT_OPER.

  SELECT   PLNNR PLNKN ZAEHL  VORNR  STEUS  WERKS  LTXA1
    INTO CORRESPONDING FIELDS OF TABLE PT_OPER
      FROM PLPO
        WHERE  PLNNR = P_PLNNR
          AND  LOEKZ = ' '.

  CHECK SY-SUBRC = 0.
  SORT PT_OPER BY PLNNR ZAEHL VORNR ASCENDING.

ENDFORM.                    " RETRIEVE_EXIT_OPERATION
*&----------------------------------------------------------------*
*&      Form  PROCESS_TRANSACTION
*&----------------------------------------------------------------*
FORM PROCESS_TRANSACTION.


*- TRANSACTION  ??
  REFRESH IT_MSG.
  CALL TRANSACTION 'QP01'    USING BDC_TAB
                             UPDATE 'S'
                             MODE PA_MODE
                          MESSAGES INTO IT_MSG.

  READ TABLE IT_MSG WITH KEY MSGTYP = 'E'.

  IF SY-SUBRC = 0.
    MESSAGE E002(ZMPMQM) WITH 'BDC Error Founded!'(E10).
    EXIT.
  ENDIF.

ENDFORM.                    " PROCESS_TRANSACTION
*&-----------------------------------------------------------------*
*&      Form  MODIFY_IT_QMPLN_FOR_BDC
*&-----------------------------------------------------------------*
FORM MODIFY_IT_QMPLN_FOR_BDC.
*-- => ST_MATASK must same from MATNR to SLWBEZ of IT_QMPLN Structure.
*-- => ST_OPT    must same from VORNR to QPPKTABS of IT_QMPLN Structure.
  DATA : LV_MATNR TYPE MATNR,
         LV_VORNR LIKE  PLPOD-VORNR.
  DATA : LV_INDEX LIKE SY-TABIX.

  LOOP AT IT_QMPLN.
    LV_INDEX = SY-TABIX.

    IF     LV_MATNR NE IT_QMPLN-MATNR AND  "/NEW Material Number
      NOT IT_QMPLN-MATNR IS INITIAL.

      CLEAR LV_VORNR.    "/for Protect just one operation record of
      " previous Material Inspection.
      LV_MATNR = IT_QMPLN-MATNR.
      CLEAR : ST_MATASK, ST_OPT.
      MOVE-CORRESPONDING : IT_QMPLN TO ST_MATASK,
                           IT_QMPLN TO ST_OPT.
    ENDIF.

    IF     LV_VORNR NE IT_QMPLN-VORNR AND    "/NEW Operation
       NOT IT_QMPLN-VORNR IS INITIAL.

      LV_VORNR = IT_QMPLN-VORNR.
      CLEAR : ST_OPT.
      MOVE-CORRESPONDING IT_QMPLN TO ST_OPT.

    ENDIF.

    MOVE-CORRESPONDING : ST_MATASK TO IT_QMPLN,
                         ST_OPT    TO IT_QMPLN.

    MODIFY IT_QMPLN INDEX LV_INDEX.

  ENDLOOP.

ENDFORM.                    " MODIFY_IT_QMPLN_FOR_BDC
