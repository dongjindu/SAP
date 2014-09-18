************************************************************************
* Program Name      : ZCQM11C_PART_INSP_UPLOAD_BAPI
* PGM Guide Pattern : 2.Conversion - 2.2 Call BAPI function
* Author            : SeungLyong, Lee
* Creation Date     : 2003.10.14.
* Specifications By : SeungLyong, Lee
* Development Request No :
* Addl Documentation:
* Description       : Part Inspection Plan Uploading
*   - Using BAPI Function : BAPI_INSPECTIONPLAN_CREATE
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


REPORT  ZCQM11C_PART_INSPECTION_UPLOAD NO STANDARD PAGE HEADING
                                          LINE-SIZE 300.


TYPE-POOLS: VRM.

*-- Declaration Tables
TABLES : MAPL, PLKO, PLPO, PLMK, MARA, MARC.

*-- material assignment table
DATA : BEGIN OF IT_MAT_ASSIGN OCCURS 0,
        PLNNR  LIKE  RC271-PLNNR,   "Key for task list group =>CLEAR' '
        PLNAL  TYPE  PLNAL,         "Group Counters
        MATNR  TYPE  MATNR,         "/MATERIAL
        WERKS  LIKE  RC27M-WERKS,   "Plant
       END OF IT_MAT_ASSIGN.

DATA: BEGIN OF IT_QMPLN OCCURS 0,
*-- Initial Screen.
    PLNNR       LIKE  RC271-PLNNR,   "Key for task list group
    STTAG       TYPE  CP_STTAG,      "Key date(Validity)
    PLNAL       TYPE  PLNAL,         "Group Counters
    WERKS       LIKE  RC27M-WERKS,   "Plant
*    STTAG       LIKE  RC271-STTAG,   "Key date => sy-datum
*-- Inspection Plan - Header Details
    KTEXT	LIKE  PLKOD-KTEXT,        "Task list description
    VERWE	LIKE  PLKOD-VERWE,        "Task list usage
    STATU	LIKE  PLKOD-STATU,        "Status
    SLWBEZ	LIKE  PLKOD-SLWBEZ,       "Inspection Point
*    QKZRASTER	LIKE  PLKOD-QKZRASTER,    "Inspection Point
*-- - Operation Overview
    VORNR	LIKE  PLPOD-VORNR,	"Operation Number
*    ARBPL(8)    like plpod-arbpl,       " workcenter
    STEUS	LIKE  PLPOD-STEUS,	"Control key
    LTXA1	LIKE  PLPOD-LTXA1,	"Short text
*-- S Operation Details
    ERFSICHT	LIKE  PLPOD-ERFSICHT,	"Recording View => '12'
                           " 12 - Single values and summarized results
*    QPPKTABS	LIKE  PLPOD-QPPKTABS,	"Flow Variants for  =>'2'
    QPPKTABS    TYPE QPPKTABS,
                                        "Inspection Point Completion
                                        " 2 - Valuation
*--  Characteristic Overview
    MERKNR	LIKE  PLMKB-MERKNR,	"Inspection Characteristic
                                        "Number
    VERWMERKM	LIKE  PLMKB-VERWMERKM,	"Master Inspection
                                       "Characteristics
    MKVERSION	LIKE  PLMKB-MKVERSION,	"Version Number of the
                                        "Master Insp. Characteristic
    STICHPRVER	LIKE  PLMKB-STICHPRVER,	"Sampling Procedure
                                        "in Inspection Characteristic

 END OF IT_QMPLN.

*-- Bapi return message table
DATA : IT_RETURN  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

**- Structure for IT_QMPLN Modfy for Using At Event
*-- => ST_GROUP  must same from PLNNR to STTAG  of IT_QMPLN Structure.
*-- => ST_MATASK must same from PLNAL to SLWBEZ of IT_QMPLN Structure.
*-- => ST_OPT    must same from VORNR to QPPKTABS of IT_QMPLN Structure.
DATA: BEGIN OF ST_GROUP OCCURS 0,
       PLNNR       LIKE  RC271-PLNNR,   "Key for task list group
       STTAG       TYPE  CP_STTAG,      "Key date(Validity)
      END OF ST_GROUP.

DATA : BEGIN OF ST_MATASK,
*        PLNNR  LIKE  RC271-PLNNR,   "Key for task list group
        PLNAL  TYPE  PLNAL,         "Group Counters
        WERKS  LIKE  RC27M-WERKS,   "Plant
        KTEXT  LIKE  PLKOD-KTEXT,        "Task list description
        VERWE  LIKE  PLKOD-VERWE,        "Task list usage
        STATU  LIKE  PLKOD-STATU,        "Status
        SLWBEZ LIKE  PLKOD-SLWBEZ,       "Inspection Point
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

*/ Result variables
DATA : WA_SUCCESS_CNT TYPE I,
       WA_FAILED_CNT  TYPE I,
       WA_GRP_CNT_ENTR TYPE I.

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
CONSTANTS : C_MARK  TYPE C VALUE 'X'.


DATA: WA_NAME  TYPE VRM_ID,
      IT_LIST  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST.


* ALV internal table
DATA: BEGIN OF gt_tca01 OCCURS 0,
        plnty   like  tca01-plnty,
      END   OF gt_tca01.


*&----------------------------------------------------------------------
*&     MACRO DEFINITION.
*&----------------------------------------------------------------------
*___Macro Definition
DEFINE make_ranges.
  move:  'I'     to &2-sign,
         'EQ'    to &2-option,
         &1      to &2-low.
  append &2.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&  Define  Ranges
*&---------------------------------------------------------------------*
*&  SELECTION-SCREEN field.
ranges: r_plnty   for tca01-plnty.  "Task List Type


*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : P_FILE   LIKE RLGRAP-FILENAME   OBLIGATORY.
SELECTION-SCREEN SKIP 1.
PARAMETERS : P_FILE_M   LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN ULINE.
PARAMETERS : P_TEST  TYPE BAPIFLAG DEFAULT 'X' AS LISTBOX
                                             VISIBLE LENGTH 14.
SELECTION-SCREEN END OF BLOCK BLK .


AT SELECTION-SCREEN OUTPUT.

  SET TITLEBAR '1000'.

  WA_NAME = 'P_TEST'.
  WA_VALUE-KEY = 'X'. WA_VALUE-TEXT = 'Simulation'.
  APPEND WA_VALUE TO IT_LIST.
  WA_VALUE-KEY = ' '. WA_VALUE-TEXT = 'Write'.
  APPEND WA_VALUE TO IT_LIST.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = WA_NAME
            VALUES = IT_LIST.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM GET_SELECT_EXCEL_FILE  USING P_FILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE_M.
  PERFORM GET_SELECT_EXCEL_FILE  USING P_FILE_M.

START-OF-SELECTION.

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_QMPLN
                          USING    P_FILE.

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_MAT_ASSIGN
                          USING    P_FILE_M.
*  fill blank fields for Internal table
  PERFORM MODIFY_IT_QMPLN_FOR_IT.
  PERFORM MODIFY_IT_MAT_ASSIGN.

END-OF-SELECTION.

  PERFORM CREATE_INSP_PLAN. "/execute BAPI Function

  SET TITLEBAR 'BDC_LOG'.

  PERFORM WRITE_LOG_MESSAGE.


TOP-OF-PAGE.

  PERFORM WRITE_HEADER.

END-OF-PAGE.

*&------------------------------------------------------------------*
*&      Form  F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
FORM F_EXCEL_UPLOAD  TABLES   P_TABLE
                      USING    P_FILENAME  LIKE RLGRAP-FILENAME.



  DATA : LT_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : LW_START_COL TYPE I VALUE '1',
         LW_START_ROW TYPE I VALUE '1',
         LW_END_COL   TYPE I VALUE '256',
         LW_END_ROW   TYPE I VALUE '65536'.
  FIELD-SYMBOLS : <LW_FS>.
  DATA : LW_FIELD_TYPE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FILENAME
            I_BEGIN_COL             = LW_START_COL
            I_BEGIN_ROW             = LW_START_ROW
            I_END_COL               = LW_END_COL
            I_END_ROW               = LW_END_ROW
       TABLES
            INTERN                  = LT_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPS) WITH 'File Upload Failed !'(E10).
    STOP.
  ENDIF.


  CHECK NOT LT_ITAB[] IS INITIAL.

*-- Delete Header line: row from 1 to 2 : for Dev.

  DELETE LT_ITAB WHERE ROW LE 2.
*-- Delete Header line - end

  SORT LT_ITAB BY ROW COL.
  REFRESH P_TABLE.

  LOOP AT LT_ITAB.
    MOVE : LT_ITAB-COL TO LW_INDEX.
    ASSIGN COMPONENT LW_INDEX OF STRUCTURE P_TABLE TO <LW_FS>.

    DESCRIBE FIELD <LW_FS> TYPE LW_FIELD_TYPE.

    IF LW_FIELD_TYPE = 'D'.  "'MM/DD/YYYY"
      CONCATENATE LT_ITAB-VALUE+6(4)    "YEAR  (YYYY)
                  LT_ITAB-VALUE+0(2)    "MONTH (MM)
                  LT_ITAB-VALUE+3(2)    "DAY   (DD)
                              INTO <LW_FS>.
    ELSE.
      MOVE : LT_ITAB-VALUE TO <LW_FS>.
    ENDIF.

    AT END OF ROW.
      APPEND P_TABLE.
      CLEAR P_TABLE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
*&      Form  get_select_Excel_file
*&------------------------------------------------------------------*
FORM GET_SELECT_EXCEL_FILE   USING PW_FILE.
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
            FILE_NAME     = PW_FILE
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

*// === 2011.08.04 insert by Kim_yn.     for ECC6 upgrade === //*
  clear: r_plnty, r_plnty[], gt_tca01, gt_tca01[].

  select plnty
    INTO CORRESPONDING FIELDS OF TABLE gt_tca01
      FROM tca01.

  loop at gt_tca01.
    make_ranges: gt_tca01-plnty    r_plnty.   "
  endloop.
*// === 2011.08.04 insert  End ============================= //*


  SELECT   PLNNR PLNKN ZAEHL  VORNR  STEUS  WERKS  LTXA1
    INTO CORRESPONDING FIELDS OF TABLE PT_OPER
      FROM PLPO
        WHERE  PLNNR = P_PLNNR
          AND  LOEKZ = ' '
          AND  PLNTY  IN r_plnty.     "changed by kim.yn
*          AND  PLNTY  IN ( SELECT PLNTY FROM TCA01 ).  "/Tunning

  CHECK SY-SUBRC = 0.
  SORT PT_OPER BY PLNNR ZAEHL VORNR ASCENDING.

ENDFORM.                    " RETRIEVE_EXIT_OPERATION
*&-----------------------------------------------------------------*
*&      Form  MODIFY_IT_QMPLN_FOR_IT
*&-----------------------------------------------------------------*
FORM MODIFY_IT_QMPLN_FOR_IT.
*-- => ST_MATASK must same from PLNAL to SLWBEZ of IT_QMPLN Structure.
*-- => ST_OPT    must same from VORNR to QPPKTABS of IT_QMPLN Structure.
  DATA : LW_PLNNR TYPE PLNNR,
         LW_PLNAL TYPE PLNAL,
         LW_VORNR LIKE  PLPOD-VORNR.
  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_VORNR_T(4) TYPE C.

  LOOP AT IT_QMPLN.
    LW_INDEX = SY-TABIX.

*-- Task Group Level
    IF     LW_PLNNR NE IT_QMPLN-PLNNR AND  "/NEW Group
      NOT IT_QMPLN-PLNNR IS INITIAL.
*-   Clear for Protect just one task/operation record of previous Group
      CLEAR : LW_PLNAL, LW_VORNR.

      LW_PLNNR = IT_QMPLN-PLNNR.
      CLEAR : ST_GROUP, ST_MATASK, ST_OPT.
      MOVE-CORRESPONDING : IT_QMPLN TO ST_GROUP,
                           IT_QMPLN TO ST_MATASK,
                           IT_QMPLN TO ST_OPT.
    ENDIF.

*-- Task Group counter level
    IF     LW_PLNAL NE IT_QMPLN-PLNAL AND  "/NEW Group Count Number
      NOT IT_QMPLN-PLNAL IS INITIAL.
*-   Clear for Protect just one operation record of previous Group count
      CLEAR LW_VORNR.
      LW_PLNAL = IT_QMPLN-PLNAL.
      CLEAR : ST_MATASK, ST_OPT.
      MOVE-CORRESPONDING : IT_QMPLN TO ST_MATASK,
                           IT_QMPLN TO ST_OPT.
    ENDIF.

*-- Opteration Level
    IF     LW_VORNR NE IT_QMPLN-VORNR AND    "/NEW Operation
       NOT IT_QMPLN-VORNR IS INITIAL.

      LW_VORNR = IT_QMPLN-VORNR.
      CLEAR : ST_OPT.
      MOVE-CORRESPONDING IT_QMPLN TO ST_OPT.

    ENDIF.

    MOVE : LW_PLNNR TO IT_QMPLN-PLNNR.
    MOVE-CORRESPONDING : ST_GROUP  TO IT_QMPLN,
                         ST_MATASK TO IT_QMPLN,
                         ST_OPT    TO IT_QMPLN.

    SHIFT IT_QMPLN-VORNR RIGHT DELETING TRAILING ' '.
    LW_VORNR_T = '0000'.
    OVERLAY IT_QMPLN-VORNR  WITH LW_VORNR_T.

    MODIFY IT_QMPLN INDEX LW_INDEX.

  ENDLOOP.


ENDFORM.                    " MODIFY_IT_QMPLN_FOR_IT
*&------------------------------------------------------------------*
*&      Form  CREATE_INSP_PLAN
*&------------------------------------------------------------------*
FORM CREATE_INSP_PLAN.
  DATA : LW_QMPLN LIKE IT_QMPLN.


*//-- define BAPI interfaces
*<Import> parameters
*- Profile To Be Used
  DATA : LW_PROFILE  TYPE PROFID_STD.
*<Export>
*-     Key for Task List Group
  DATA : LW_GROUP    TYPE BAPI1191_TSK_C-TASK_LIST_GROUP,
*-     Group Counter
         LW_GROUPCOUNTER TYPE BAPI1191_TSK_C-GROUP_COUNTER.
*<Tables>
  DATA: LT_TASK	        LIKE	BAPI1191_TSK_C   "/Task List Header Data
                       OCCURS 0 WITH HEADER LINE,
        LT_MAT_TASKALL	LIKE	BAPI1191_MTK_C   "/Mat.-Task List Assign
                       OCCURS 0 WITH HEADER LINE,
        LT_OPERATION	LIKE	BAPI1191_OPR_C     "/Operations
                       OCCURS 0 WITH HEADER LINE,
        LT_REF_OPER	LIKE	BAPI1191_REF_OPR_C "/Ref. Operation
                       OCCURS 0 WITH HEADER LINE,
        LT_PRTRESOURCE	LIKE	BAPI1191_PRT_C     "/PRT Assignments
                       OCCURS 0 WITH HEADER LINE,
        LT_INSPCHAR	LIKE	BAPI1191_CHA_C   "/Insp. Characteristics
                       OCCURS 0 WITH HEADER LINE,
        LT_TEXT_ALLO	LIKE	BAPI1191_TXT_HDR_C "/Text Assignments
                       OCCURS 0 WITH HEADER LINE,
        LT_TEXT	LIKE	BAPI1012_TXT_C     "/Long Text Lines
                       OCCURS 0 WITH HEADER LINE,
        LT_RETURN	LIKE	BAPIRET2          "/Return Parameter
                       OCCURS 0 WITH HEADER LINE.


  LOOP AT IT_QMPLN.
    CLEAR LW_QMPLN.
*    Data Back up for at event.
    MOVE-CORRESPONDING IT_QMPLN TO LW_QMPLN.

*-- Group
    AT NEW PLNNR.

**    Fill Profile field for default values => Not used - sllee
*      MOVE : LW_QMPLN-WERKS  TO LW_PROFILE-PLANT,             "/Plant
*             'EA'            TO LW_PROFILE-TASK_MEASURE_UNIT, "/Unit
*             LW_QMPLN-VERWE  TO LW_PROFILE-TASK_LIST_USAGE,   "/Usage
*             LW_STATU        TO LW_PROFILE-TASK_LIST_STATUS.  "/Status

    ENDAT.

*-- Group counter : Task List
    AT NEW PLNAL.
      WA_GRP_CNT_ENTR = WA_GRP_CNT_ENTR + 1.

      CLEAR   : LW_PROFILE, LW_GROUP, LW_GROUPCOUNTER.
      REFRESH : LT_TASK, LT_MAT_TASKALL, LT_OPERATION, LT_REF_OPER,
                LT_PRTRESOURCE, LT_INSPCHAR, LT_TEXT_ALLO, LT_TEXT,
                LT_RETURN.
      CLEAR   : LT_TASK, LT_MAT_TASKALL, LT_OPERATION, LT_REF_OPER,
                LT_PRTRESOURCE, LT_INSPCHAR, LT_TEXT_ALLO, LT_TEXT,
                LT_RETURN.

      CLEAR LT_TASK.
      MOVE :
         LW_QMPLN-PLNNR  TO LT_TASK-TASK_LIST_GROUP, "/Group
         LW_QMPLN-PLNAL  TO LT_TASK-GROUP_COUNTER,   "/Group Counter
         LW_QMPLN-STTAG  TO LT_TASK-VALID_FROM,  "/Key date(Valid from)
         LW_QMPLN-VERWE  TO LT_TASK-TASK_LIST_USAGE, "/Task Usage
         LW_QMPLN-WERKS  TO LT_TASK-PLANT,           "/Task Plant
         LW_QMPLN-STATU  TO LT_TASK-TASK_LIST_STATUS, "/Task Status
         'EA '           TO LT_TASK-TASK_MEASURE_UNIT, "/Unit of Mea.
*         'EA '           TO LT_TASK-TASK_MEASURE_UNIT_ISO,
*         1        TO LT_TASK-LOT_SIZE_FROM,
*         99999999 TO LT_TASK-LOT_SIZE_TO,
         LW_QMPLN-KTEXT  TO LT_TASK-DESCRIPTION,   "/Task description
         LW_QMPLN-SLWBEZ TO LT_TASK-IDENT_KEY.     "/Insp. Points
      APPEND LT_TASK.
    ENDAT.

*-- Operations
    AT NEW VORNR.
      CLEAR LT_OPERATION.
      MOVE :
          LW_QMPLN-PLNNR    TO LT_OPERATION-TASK_LIST_GROUP, "/Group
          LW_QMPLN-PLNAL    TO LT_OPERATION-GROUP_COUNTER,"/Gr. Counter
          LW_QMPLN-STTAG    TO LT_OPERATION-VALID_FROM, "/Valid from
          LW_QMPLN-VORNR    TO LT_OPERATION-ACTIVITY, "/Operation Number
          LW_QMPLN-STEUS    TO LT_OPERATION-CONTROL_KEY, "/Control key
*          LW_QMPLN-ARBPL   TO LT_OPERATION-WORK_CNTR, "/Work center
          LW_QMPLN-WERKS    TO LT_OPERATION-PLANT,       "/Plant of W.C
          LW_QMPLN-LTXA1    TO LT_OPERATION-DESCRIPTION, "/Short text
          LW_QMPLN-ERFSICHT TO LT_OPERATION-RECORDING_VIEW,
*                                                      "/Recording View
          1                 TO LT_OPERATION-BASE_QUANTITY,
          'EA'  TO LT_OPERATION-OPERATION_MEASURE_UNIT,
*          'EA '  TO LT_OPERATION-OPERATION_MEASURE_UNIT_ISO,
          1      TO LT_OPERATION-DENOMINATOR,  "/Denominator
          1      TO LT_OPERATION-NOMINATOR,    "/Nominator
          LW_QMPLN-QPPKTABS   "/Flow Variants for Insp Point Completion
             TO LT_OPERATION-INSP_POINT_COMPLT_FLOW_VARIANT.
**   "/Operation ID-unique key(include one non-numerica character)
**      'VORG' + VORNR.
*      CONCATENATE 'VORG'
*                 LT_OPERATION-ACTIVITY
*                     INTO LT_OPERATION-OPERATION_ID.


      APPEND LT_OPERATION.
    ENDAT.


    AT NEW MERKNR.

      CLEAR LT_INSPCHAR.
      MOVE :
        LW_QMPLN-PLNNR  TO LT_INSPCHAR-TASK_LIST_GROUP, "/Group
        LW_QMPLN-PLNAL  TO LT_INSPCHAR-GROUP_COUNTER, "/Gr. Counter
        LW_QMPLN-VORNR  TO LT_INSPCHAR-ACTIVITY,  "/Operation Number
        LW_QMPLN-MERKNR TO LT_INSPCHAR-INSPCHAR, "/Characteristic Number
        LW_QMPLN-STTAG  TO LT_INSPCHAR-VALID_FROM, "/Valid from
        LW_QMPLN-VERWMERKM  TO LT_INSPCHAR-MSTR_CHAR, "/MIC
        LW_QMPLN-WERKS  TO LT_INSPCHAR-PMSTR_CHAR,    "/Plant
        'L'    TO LT_INSPCHAR-CHA_MASTER_IMPORT_MODUS, "/Reference Mode
        'EA'  TO LT_INSPCHAR-MEAS_UNIT,
        1     TO LT_INSPCHAR-SMPL_QUANT, "/Base sample Quantity
        'EA'     TO LT_INSPCHAR-SMPL_UNIT,

        LW_QMPLN-STICHPRVER  TO LT_INSPCHAR-SMPL_PROCEDURE,
*                                                "/Sampling Procedure
*      LW_QMPLN-DESCR  TO LT_INSPCHAR-CHAR_DESCR, "/Short Text
        ' '              TO LT_INSPCHAR-QUANTITATIVE_IND. "/Qual.=' '

*   "/Operation ID-unique key same as LT_OPERATION-OPERATION_ID.
      MOVE : LT_OPERATION-OPERATION_ID TO LT_INSPCHAR-OPERATION_ID.

      APPEND LT_INSPCHAR.
    ENDAT.



    AT END OF PLNAL.
*    Material assignment from IT_MAT_ASSIGN
      LOOP AT IT_MAT_ASSIGN WHERE PLNNR = LW_QMPLN-PLNNR
                              AND PLNAL = LW_QMPLN-PLNAL.
        CLEAR LT_MAT_TASKALL.
        MOVE :
          IT_MAT_ASSIGN-PLNNR   TO LT_MAT_TASKALL-TASK_LIST_GROUP,
          IT_MAT_ASSIGN-PLNAL   TO LT_MAT_TASKALL-GROUP_COUNTER,
          IT_MAT_ASSIGN-MATNR   TO LT_MAT_TASKALL-MATERIAL,
          LW_QMPLN-STTAG        TO LT_MAT_TASKALL-VALID_FROM,
          IT_MAT_ASSIGN-WERKS   TO LT_MAT_TASKALL-PLANT.
        APPEND LT_MAT_TASKALL.
      ENDLOOP.
***/ Execute BAPI : 'BAPI_INSPECTIONPLAN_CREATE'

      CALL FUNCTION 'BAPI_INSPECTIONPLAN_CREATE'
           EXPORTING
                TESTRUN                = P_TEST
*                PROFILE                = LW_PROFILE
           IMPORTING
                GROUP                  = LW_GROUP
                GROUPCOUNTER           = LW_GROUPCOUNTER
           TABLES
                TASK                   = LT_TASK
                MATERIALTASKALLOCATION = LT_MAT_TASKALL
                OPERATION              = LT_OPERATION
                REFERENCEOPERATION     = LT_REF_OPER
                PRODUCTIONRESOURCE     = LT_PRTRESOURCE
                INSPCHARACTERISTIC     = LT_INSPCHAR
                TEXTALLOCATION         = LT_TEXT_ALLO
                TEXT                   = LT_TEXT
                RETURN                 = LT_RETURN.


      CLEAR IT_RETURN.

**      insert division message for each inspection plan
      CONCATENATE LW_QMPLN-PLNNR
                  ':'
                  LW_QMPLN-PLNAL
                  LW_QMPLN-WERKS
                  LW_QMPLN-KTEXT
                     INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.

      READ TABLE LT_RETURN WITH KEY TYPE = 'E'.

      IF SY-SUBRC = 0.  "/Error
        PERFORM BAPI_SERVICE_ROLLBACK.
*        ROLLBACK WORK.
        CONCATENATE IT_RETURN-MESSAGE
                    '=> Failed'
                       INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
        WA_FAILED_CNT = WA_FAILED_CNT + 1.

      ELSE.
        READ TABLE LT_RETURN WITH KEY TYPE = 'A'.
        IF SY-SUBRC = 0.
          PERFORM BAPI_SERVICE_ROLLBACK.
*          ROLLBACK WORK.
          CONCATENATE IT_RETURN-MESSAGE
                      '=> Failed'
                         INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
          WA_FAILED_CNT = WA_FAILED_CNT + 1.

        ELSE.  "Success Check.
          CLEAR LT_RETURN.
          READ TABLE LT_RETURN INDEX 1.

          IF ( LT_RETURN-TYPE IS INITIAL OR          "//Success
               LT_RETURN-TYPE = 'S'           ) AND
             ( NOT LW_GROUP        IS INITIAL   AND
               NOT LW_GROUPCOUNTER IS INITIAL ).
            IF P_TEST IS INITIAL.
              PERFORM BAPI_SERVICE_COMMIT.
            ELSE.
              PERFORM BAPI_SERVICE_ROLLBACK.
            ENDIF.

            CONCATENATE IT_RETURN-MESSAGE
                        '=> Success'
                           INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.

            WA_SUCCESS_CNT = WA_SUCCESS_CNT + 1.
          ELSE.
            PERFORM BAPI_SERVICE_ROLLBACK.
*            ROLLBACK WORK.
            CONCATENATE IT_RETURN-MESSAGE
                        '=> Failed'
                           INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
            WA_FAILED_CNT = WA_FAILED_CNT + 1.
          ENDIF.

        ENDIF.

      ENDIF.

      APPEND IT_RETURN.
      APPEND LINES OF LT_RETURN TO IT_RETURN.
      APPEND INITIAL LINE TO  IT_RETURN.

    ENDAT.

  ENDLOOP.

ENDFORM.                    " CREATE_INSP_PLAN
*&-----------------------------------------------------------------*
*&      Form  WRITE_LOG_MESSAGE
*&-----------------------------------------------------------------*
FORM WRITE_LOG_MESSAGE.

  LOOP AT IT_RETURN WHERE TYPE = ' '.
    WRITE IT_RETURN-MESSAGE.
    NEW-LINE.
  ENDLOOP.

  ULINE.

  LOOP AT IT_RETURN.
    IF    IT_RETURN-TYPE IS INITIAL AND
      NOT IT_RETURN-MESSAGE IS INITIAL.
      WRITE '-------------------------------------------------------'.
      NEW-LINE.
    ENDIF.

    WRITE : IT_RETURN-TYPE,
            IT_RETURN-ID,
            IT_RETURN-NUMBER,
  AT (150)  IT_RETURN-MESSAGE,
*            IT_RETURN-LOG_NO,
*            IT_RETURN-LOG_MSG_NO,
*            IT_RETURN-MESSAGE_V1,
*            IT_RETURN-MESSAGE_V2,
*            IT_RETURN-MESSAGE_V3,
*            IT_RETURN-MESSAGE_V4,
            IT_RETURN-PARAMETER,
*            IT_RETURN-ROW,
            IT_RETURN-FIELD.
*            IT_RETURN-SYSTEM.
    NEW-LINE.
  ENDLOOP.

ENDFORM.                    " WRITE_LOG_MESSAGE
*&------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&------------------------------------------------------------------*
FORM WRITE_HEADER.
  WRITE : 'Inspection plan master uploading Log'(H01).
  ULINE.
  WRITE : 'Total of Inspection group : '(H02) ,
          WA_GRP_CNT_ENTR.
  NEW-LINE.
  WRITE : 'Success entries           : '(H03) ,
          WA_SUCCESS_CNT.
  NEW-LINE.
  WRITE : 'Failed entries            : '(H04),
          WA_FAILED_CNT.
  ULINE.
ENDFORM.                    " WRITE_HEADER
*&-----------------------------------------------------------------*
*&      Form  MODIFY_IT_MAT_ASSIGN
*&-----------------------------------------------------------------*
FORM MODIFY_IT_MAT_ASSIGN.
*-- => ST_MATASK must same from PLNAL to SLWBEZ of IT_QMPLN Structure.
*-- => ST_OPT    must same from VORNR to QPPKTABS of IT_QMPLN Structure.
  DATA : LW_PLNNR TYPE PLNNR,
         LW_PLNAL TYPE PLNAL.
  DATA : LW_INDEX LIKE SY-TABIX.

  LOOP AT IT_MAT_ASSIGN.
    LW_INDEX = SY-TABIX.

*-- Task Group Level
    IF     LW_PLNNR NE IT_MAT_ASSIGN-PLNNR AND  "/NEW Group
      NOT IT_MAT_ASSIGN-PLNNR IS INITIAL.
*-   Clear for Protect just one task/operation record of previous Group
      CLEAR : LW_PLNAL.
      LW_PLNNR = IT_MAT_ASSIGN-PLNNR.
      LW_PLNAL = IT_MAT_ASSIGN-PLNAL.
    ENDIF.

*-- Task Group counter level
    IF     LW_PLNAL NE IT_MAT_ASSIGN-PLNAL AND  "/NEW Group Count Number
      NOT IT_MAT_ASSIGN-PLNAL IS INITIAL.
*-   Clear for Protect just one operation record of previous Group count
      LW_PLNAL = IT_MAT_ASSIGN-PLNAL.
    ENDIF.


    MOVE : LW_PLNNR TO IT_MAT_ASSIGN-PLNNR.
    MOVE : LW_PLNAL TO IT_MAT_ASSIGN-PLNAL.

    MODIFY IT_MAT_ASSIGN INDEX LW_INDEX.

  ENDLOOP.


ENDFORM.                    " MODIFY_IT_MAT_ASSIGN
*&-----------------------------------------------------------------*
*&      Form  BAPI_SERVICE_COMMIT
*&-----------------------------------------------------------------*
FORM BAPI_SERVICE_COMMIT.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
* EXPORTING
*   WAIT          =
* IMPORTING
*   RETURN        = .
ENDFORM.                    " BAPI_SERVICE_COMMIT
*&------------------------------------------------------------------*
*&      Form  BAPI_SERVICE_ROLLBACK
*&------------------------------------------------------------------*
FORM BAPI_SERVICE_ROLLBACK.
  CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
* IMPORTING
*   RETURN        =     .

ENDFORM.                    " BAPI_SERVICE_ROLLBACK
