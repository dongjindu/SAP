************************************************************************
* Program Name      : ZRCO_LABOR_COST
* Author            : Chris Li, Andy Choi
* Creation Date     : 03/15/2005
* Specifications By : Andy Choi/Calvin Kong
* Pattern           :
* Development Request No : UD1K914960
* Addl Documentation:
* Description       : This report generate the lobor cost analysis
*                     result for each cost center based on the actual
*                     cost posting and reglect the support and being
*                     support between cost centers.
*
* Modification Logs
* Date       Developer    RequestNo    Description

*Note 1012176 - Indexes for tables PPOIX and PPOPX
* RPCIPQ00
************************************************************************
* Date        Developer  Request          Description
*04/02/2007   Manju      UD1K940237       Switch signs of Amount & Hour
*                                         for previous period
*10/11/2010   Valerian   UD1K949915       Copy from program ZACOR21 and
*                                         modified:
*                                         - Get Item from G/L
*                                         - Get Total Head Counts and
*                                           Total Hours from Payroll
*07/22/2011   Valerian   UD1K952793       Distribute cost by new cost
*                                         element group
*                                         Distribute hours by salary
*                                         hourly and temporary
**09/22/2011   yn.kim   UP1K920005        ECC6 upgrade and data
*                                         structure Change.
* 06/20/2013  T00303    UD1K957466       U1: Apply Archiving - F1 Help
************************************************************************
REPORT ZRCO_LABOR_COST  MESSAGE-ID ZMCO.


*include zrco_labortop.
*----------------------------------------------------------------------*
*   INCLUDE ZRCO_LABORTOP                                              *
*----------------------------------------------------------------------*
TABLES:   COSS, COSP,COEP, CSKS,
          ZTCO_LABOR_COST1,
          ZSCO_LABOR, PA0002.

RANGES: R_CC    FOR  CSKS-KOSTL.
RANGES: R_CE    FOR  COSS-KSTAR.
RANGES: P_PERBL FOR SY-DATUM.
DATA:   GV_PREV_PERIOD LIKE S016-SPMON.

DATA:   T_CEHIER   LIKE SETHIER   OCCURS 0 WITH HEADER LINE.
DATA:   T_CEVALUES LIKE SETVALUES OCCURS 0 WITH HEADER LINE.

*// == 2011.09.22 change by yn.kim for ECC6. standard changed. ==//*
*// == ORG P/G : RPCPCC0D -- itab => list_table.
DATA: BEGIN OF LIST_TAB OCCURS 0.
        INCLUDE STRUCTURE PNA_CCDATA.
DATA:   SEQNO LIKE PPOIX-SEQNO.
DATA:   RUN_ID TYPE P_EVNUM.
DATA:   FLD_NAME(10) TYPE C.
DATA:   BUKRS TYPE BUKRS.
DATA:   IO TYPE C.
DATA: END OF LIST_TAB.

***DATA:   BEGIN OF list_tab  OCCURS 0.
***        INCLUDE STRUCTURE pna_ccdata.
***DATA:     seqno LIKE ppoix-seqno.
***DATA:     run_id TYPE p_evnum.
***DATA:     fld_name(10) TYPE c.
***DATA:   END OF list_tab .
*// ======================== Changed end ========================//*

DATA: LIST_TABLE LIKE LIST_TAB OCCURS 0 WITH HEADER LINE.

DATA:   RSPAR LIKE RSPARAMS OCCURS 10 WITH HEADER LINE.
RANGES: R_WT FOR T512W-LGART.
DATA:   OK_CODE LIKE SY-UCOMM,
        SAVE_OK LIKE SY-UCOMM.
TYPES:  BEGIN OF S_EE,
         PERNR     LIKE PA0001-PERNR,
         PERSG     LIKE PA0001-PERSG,
         PTEXT     LIKE T501T-PTEXT,
         PERSK     LIKE PA0001-PERSK,
         SCHKZ     LIKE PA0007-SCHKZ,
         BEGDA     LIKE PA0001-BEGDA,
        END OF S_EE.
TYPES:  BEGIN OF S_OUT,
         GRP1      LIKE ZSCO_LABOR-GRP1,
         GRP2      LIKE ZSCO_LABOR-GRP1,
         GRP3      LIKE ZSCO_LABOR-GRP1,
         KOSTL     LIKE CSKS-KOSTL,
         FROCC     LIKE CSKS-KOSTL,
         TOCC      LIKE CSKS-KOSTL,

*         persg     like pa0001-persg,
*         gtext(10) type c,
*         persk     like pa0001-persk,
*         ktext(10) type c,
         EMPCT     TYPE ZEMPCT,            "empl.type

         SCHKZ     LIKE PA0007-SCHKZ,
         KZTXT(10) TYPE C,

         ZCUNT     LIKE PNA_CCDATA-EMP_COUNT,
         REGUL     LIKE PNA_CCDATA-BETRG,
         OVERT     LIKE PNA_CCDATA-BETRG,
         BONUS     LIKE PNA_CCDATA-BETRG,
         ZLEAV     LIKE PNA_CCDATA-BETRG,
         OTHCO     LIKE PNA_CCDATA-BETRG,
         TOTCO     LIKE PNA_CCDATA-BETRG,
         PENSN     LIKE PNA_CCDATA-BETRG,
         HEALTH    LIKE PNA_CCDATA-BETRG,
         WORKC     LIKE PNA_CCDATA-BETRG,
         INSUR     LIKE PNA_CCDATA-BETRG,
         TAX       LIKE PNA_CCDATA-BETRG,
         OTHBE     LIKE PNA_CCDATA-BETRG,
         TOTBE     LIKE PNA_CCDATA-BETRG,
         TCOST     LIKE PNA_CCDATA-BETRG,
         TREGU     LIKE PNA_CCDATA-ANZHL,
         TOVER     LIKE PNA_CCDATA-ANZHL,
         TOTHR     LIKE PNA_CCDATA-ANZHL,
         THOUR     LIKE PNA_CCDATA-ANZHL,
         REGUH     LIKE PNA_CCDATA-BETRG,                   "UD1K952793
         TEMPO     LIKE PNA_CCDATA-BETRG,                   "UD1K952793
         TREGH     LIKE PNA_CCDATA-ANZHL,                   "UD1K952793
         TTEMP     LIKE PNA_CCDATA-ANZHL,                   "UD1K952793
        END OF S_OUT.
TYPES:  BEGIN OF S_DIS.
        INCLUDE TYPE S_OUT.
TYPES:    CLRTB     TYPE LVC_T_SCOL.
TYPES:  END OF S_DIS.
DATA:   IT_EE   TYPE STANDARD TABLE OF S_EE.
DATA:   IT_OUT  TYPE STANDARD TABLE OF S_OUT.
DATA:   IT_OUT_S  TYPE STANDARD TABLE OF S_OUT.
DATA:   WA_OUT  TYPE S_OUT.
DATA:   IT_DIS  TYPE STANDARD TABLE OF S_DIS.
DATA:   IT_OUT1 TYPE STANDARD TABLE OF S_OUT.
DATA:   BEGIN OF IT_COVP OCCURS 0,
*           gjahr   like covp-gjahr,
*           perio   like covp-perio,
           KOSTL   LIKE CSKS-KOSTL,
           OBJNR   LIKE COVP-OBJNR,
           PAROB1  LIKE COVP-PAROB1,
*           beknz   like covp-beknz,
*           sgtxt   like covp-sgtxt,
           KSTAR   LIKE COVP-KSTAR,
           MEINB   LIKE COVP-MEINB,
           WKGBTR  LIKE COVP-WKGBTR,
           MBGBTR  LIKE COVP-MBGBTR,
           PKOST   LIKE CSKS-KOSTL,
        END OF IT_COVP.
DATA: BEGIN OF IT_MHA_SUP  OCCURS 0,
       KOSTL   LIKE ZTCO_MHA-KOSTL,
       SRKOSTL LIKE ZTCO_MHA-SRKOSTL,
*       LGART   like ztco_mha-lgart,
       ANZHL   LIKE ZTCO_MHA-ANZHL,
     END OF IT_MHA_SUP.

DATA:   BEGIN OF IT_CATSCO OCCURS 0,
          COUNTER    LIKE CATSCO-COUNTER,
          STOKZ      LIKE CATSCO-STOKZ,
          WORKDATE   LIKE CATSCO-WORKDATE,
          CATSHOURS  LIKE CATSCO-CATSHOURS,
          SKOSTL     LIKE CATSCO-SKOSTL,
          LSTAR      LIKE CATSCO-LSTAR,
          RKOSTL     LIKE CATSCO-RKOSTL,
        END OF IT_CATSCO.
DATA:   BEGIN OF IT_CC_GRP OCCURS 0,
          GRP1      LIKE ZSCO_LABOR-GRP1,
          GRP2      LIKE ZSCO_LABOR-GRP1,
          GRP3      LIKE ZSCO_LABOR-GRP1.
        INCLUDE STRUCTURE SETVALUES.
DATA:   END OF IT_CC_GRP.
DATA:   BEGIN OF IT_SCHKZ OCCURS 0,
         SCHKZ       LIKE PA0007-SCHKZ,
         PTEXT(10),
        END OF IT_SCHKZ.
DATA:   BEGIN OF IT_PERSK OCCURS 0,
          PERSK      LIKE PA0001-PERSK,
          PTEXT(10),
        END OF IT_PERSK.
DATA:   BEGIN OF IT_PERSG OCCURS 0,
          PERSG      LIKE PA0001-PERSG,
          PTEXT(10),
        END OF IT_PERSG.
DATA:   L_LINES TYPE I.

DATA    C_MARK     TYPE C VALUE 'X'.
* ALV

DATA:   WC_CONTROL        TYPE        SCRFNAME VALUE 'CC_ALV',
        WC_ALV            TYPE REF TO CL_GUI_ALV_GRID,
        WC_CONTAINER      TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

* CLASS DECLARATION
CLASS   LCL_EVENT_RECEIVER DEFINITION DEFERRED. "ALV EVENT HANDLE

DATA :  EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* INTERNAL TABLES FOR ALV GRID
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT ,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT.

* VARIABLES FOR ALV GRID
DATA : WS_LAYOUT TYPE LVC_S_LAYO,
       W_VARIANT   TYPE DISVARIANT,          "for parameter IS_VARIANT
       W_FIELDNAME LIKE LINE OF IT_FIELDCAT,
       W_REPID     LIKE SY-REPID,
       W_CNT       TYPE I,                   "Field count
       W_SAVE      TYPE C   VALUE 'A'.   "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: W_CONTAINER(100),
      W_CONTROL(100),
      W_ALV(100),
      W_ITAB(100),
      W_STRUCTURE LIKE DD02L-TABNAME.

CONSTANTS: C_STRUCTURE(100) VALUE 'ZSCO_AALA_REPORT_'.

RANGES: R_LGART1 FOR T512W-LGART,
        R_LGART2 FOR T512W-LGART,
        R_LGART3 FOR T512W-LGART.

CONSTANTS: USER(5) TYPE          C VALUE 'USR',
           MANAGER(5) TYPE       C VALUE 'MAN',
           ADMINISTRATOR(5) TYPE C VALUE 'ADM'.
* Manager's mode or Administrator mode
DATA: E_MODE(3) TYPE C.

* BEGIN OF UD1K949915
DATA: IT_BSIS LIKE BSIS OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_HEADCNT OCCURS 0,
        KOSTL     TYPE KOSTL,
        EMP_COUNT TYPE PRANZ,
      END OF IT_HEADCNT.

DATA: BEGIN OF IT_HOUR OCCURS 0,
        KOSTL     LIKE CSKS-KOSTL,
        TREGU     LIKE PNA_CCDATA-ANZHL,
        TOVER     LIKE PNA_CCDATA-ANZHL,
        TOTHR     LIKE PNA_CCDATA-ANZHL,
        THOUR     LIKE PNA_CCDATA-ANZHL,
        TREGH     LIKE PNA_CCDATA-ANZHL,                    "UD1K952793
        TTEMP     LIKE PNA_CCDATA-ANZHL,                    "UD1K952793
      END OF IT_HOUR.
* END OF UD1K949915

*- U1 Start
DATA: GR_BUDAT TYPE RANGE OF BUDAT.
*RANGES: GR_BUDAT FOR BKPF-BUDAT.
*- U1 End

****************************************************************
* LOCAL CLASSES: EVEN HANDLING
****************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW
                      E_COLUMN
                      ES_ROW_NO.
ENDCLASS.                    "lcl_event_receiver DEFINITION

****************************************************************
* LOCAL CLASSES:IMPLEMENTATION
****************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DBL_CLICK USING E_COLUMN-FIELDNAME
                                 ES_ROW_NO-ROW_ID.

  ENDMETHOD.                           "handle_double_click
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*end include
TABLES: T512W.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS     P_KOKRS       LIKE TKA01-KOKRS OBLIGATORY DEFAULT 'H201'.
PARAMETERS     P_GJAHR       LIKE COSS-GJAHR  OBLIGATORY MEMORY ID GJR.
SELECT-OPTIONS P_PERIO       FOR  COSS-PERBL  OBLIGATORY NO-EXTENSION
                                              NO INTERVALS. "UD1K949915
*arameters     p_versn       like coss-versn  default '0' obligatory   .
* BEGIN UD1K949915
PARAMETERS :   P_READ  RADIOBUTTON GROUP RD,
               P_CALC  RADIOBUTTON GROUP RD,
               P_SAVE  RADIOBUTTON GROUP RD.
*DATA :   p_read(1) TYPE c,
*         p_calc(1) TYPE c VALUE 'X',
*         p_save(1) TYPE c.
* END UD1K949915
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) TEXT-005 FOR FIELD P_KSGRU.
PARAMETERS     P_KSGRU       LIKE RKSB1-KSGRU.
PARAMETERS:    P_OK(2)   TYPE C.
SELECTION-SCREEN END OF LINE.
*temperary add employee
SELECT-OPTIONS P_KOSTL       FOR  CSKS-KOSTL .
SELECT-OPTIONS P_PERNR       FOR  PA0002-PERNR .
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B4 WITH FRAME TITLE TEXT-003.
PARAMETERS: P_CYCLE(6)    TYPE C  DEFAULT 'HCR101' NO-DISPLAY.
PARAMETERS  P_ATYPE       LIKE CATSD-AWART
                               DEFAULT 'PE20' NO-DISPLAY.
** changed by Furong on 10/26/2006
* BEGIN OF UD1K949915
*PARAMETERS: p_cyc    AS CHECKBOX DEFAULT 'X',
*            p_ex_acc AS CHECKBOX DEFAULT 'X'. "Exclude Accruals
DATA: P_CYC(1)    TYPE C,
      P_EX_ACC(1) TYPE C.
* END OF UD1K949915
** end of change

*- U1 Start
INCLUDE ZIARCH_COMM01.
*- U1 End

SELECTION-SCREEN END OF BLOCK B4.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-004.
* BEGIN OF UD1K949915
*PARAMETERS: p_acc_hr AS CHECKBOX DEFAULT ' ', "Accrual from HR
*            p_acc_co AS CHECKBOX DEFAULT 'X'. "Accrual from CO
DATA: P_ACC_HR(1) TYPE C,
      P_ACC_CO(1) TYPE C.

* END OF UD1K949915
SELECTION-SCREEN END OF BLOCK B3.


*
AT SELECTION-SCREEN OUTPUT.
  IF P_OK = '22' AND E_MODE = 'ADM'.
    LOOP AT SCREEN.
      IF SCREEN-NAME  CS 'P_PERNR'.
        SCREEN-INPUT = 1.
        SCREEN-INVISIBLE = 0.
        MODIFY SCREEN.
      ELSEIF SCREEN-NAME  CS 'P_OK'.
        SCREEN-INPUT = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF SCREEN-NAME  CS 'P_PERNR'.
        SCREEN-INPUT = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
      IF E_MODE <> 'ADM' AND SCREEN-NAME  CS 'P_OK'.
        SCREEN-INPUT = 0.
        SCREEN-INVISIBLE = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*AT SELECTION-SCREEN ON P_OK.
*  IF P_OK = '22'.
*    LOOP AT SCREEN.
*      IF screen-name  CS 'P_PERNR'.
*        screen-input = 1.
*        screen-invisible = 0.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

AT SELECTION-SCREEN .
  PERFORM MAKE_PERRANGE.
  PERFORM READ_CC.
  PERFORM READ_CE.
  PERFORM READ_CC_GROUP.
  PERFORM SET_WAGETYPE.
  PERFORM MAKE_TEXT.

*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
  DATA: L_COUSERTYPE(3) TYPE C.
  GET PARAMETER ID 'ZCOLV1' FIELD L_COUSERTYPE.
  E_MODE = L_COUSERTYPE.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*  check e_mode = manager or e_mode = administrator.
  IF P_EX_ACC = 'X'.
    CLEAR: P_ACC_HR, P_ACC_CO.
  ENDIF.

  CASE C_MARK .

    WHEN P_READ.
      PERFORM READ_OLD_DATA.
    WHEN P_CALC.
      PERFORM DO_CALCULATION.
    WHEN P_SAVE.
      PERFORM CLEANUP_OLD_DATA.                             "UD1K949915
      PERFORM DELETE_OLD_DATA.
      PERFORM DO_CALCULATION.
      PERFORM SAVE_RESULT.

  ENDCASE.

END-OF-SELECTION.
  CALL SCREEN '100'.


*&---------------------------------------------------------------------*
*&      Form  make_perrange
*&---------------------------------------------------------------------*
*      CONVERT THE PERIOD INTO DATE RANGE
*----------------------------------------------------------------------*
FORM MAKE_PERRANGE.
  DATA: L_DATE LIKE SY-DATUM.
  DATA: I_PERIO TYPE I.
  DATA: L_MONTH(02).

* calculation allowed for single period.
  IF P_READ = ' '.
    P_PERIO-HIGH = P_PERIO-LOW.
  ENDIF.

  LOOP AT P_PERIO.
    P_PERBL-SIGN   = 'I'.
    P_PERBL-OPTION = 'BT'.
    L_MONTH = P_PERIO-LOW+1(2).
    CONCATENATE P_GJAHR L_MONTH '01' INTO L_DATE.
    P_PERBL-LOW = L_DATE.

    IF NOT P_PERIO-HIGH IS INITIAL.
      L_MONTH = P_PERIO-HIGH+1(2).
      CONCATENATE P_GJAHR L_MONTH '01' INTO L_DATE.
    ENDIF.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        DAY_IN            = L_DATE
      IMPORTING
        LAST_DAY_OF_MONTH = P_PERBL-HIGH.

    APPEND P_PERBL.

    L_DATE = P_PERBL-LOW - 1.
    GV_PREV_PERIOD = L_DATE(6).

    CLEAR P_PERBL.
  ENDLOOP.

ENDFORM.                    " make_perrange
*&---------------------------------------------------------------------*
*&      Form  READ_CC
*&---------------------------------------------------------------------*
*     Read the cost center if the input is cost center group
*----------------------------------------------------------------------*
FORM READ_CC.
  DATA: T_SETLIST LIKE SETLIST OCCURS 0 WITH HEADER LINE.
  DATA: T_SETHIER LIKE SETHIER OCCURS 0 WITH HEADER LINE.
  DATA: T_SETVALUES LIKE SETVALUES OCCURS 0 WITH HEADER LINE.

* CHECK THE COST CENTER VALUE INPUT
  IF NOT P_KOSTL IS INITIAL.
    R_CC[] = P_KOSTL[].
    EXIT.
  ENDIF.

  CHECK NOT P_KSGRU IS INITIAL.

  CALL FUNCTION 'G_SET_LIST_SELECT'
    EXPORTING
      SETCLASS      = '0101'
      SHORTNAME     = P_KSGRU
      KOKRS         = 'H201'
      KTOPL         = 'HNA1'
    TABLES
      MATCHING_SETS = T_SETLIST.
  IF T_SETLIST[] IS INITIAL.
    MESSAGE E002(SY) WITH 'Cost Center group does not exist'.
    EXIT.
  ELSE.
    READ TABLE T_SETLIST INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      SETID                     = T_SETLIST-SETNAME
    TABLES
      SET_HIERARCHY             = T_SETHIER
      SET_VALUES                = T_SETVALUES
    EXCEPTIONS
      SET_NOT_FOUND             = 1
      ILLEGAL_FIELD_REPLACEMENT = 2
      ILLEGAL_TABLE_REPLACEMENT = 3
      OTHERS                    = 4.

  IF SY-SUBRC <> 0.
    MESSAGE E002(SY) WITH 'Cost Center group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO CC RANGE.
  R_CC-SIGN = 'I'.
  R_CC-OPTION = 'BT'.
  LOOP AT T_SETVALUES.
    R_CC-LOW  = T_SETVALUES-FROM.
    R_CC-HIGH = T_SETVALUES-TO.
    APPEND R_CC.
  ENDLOOP.
  CLEAR R_CC.
ENDFORM.                    " READ_CC
*&---------------------------------------------------------------------*
*&      Form  READ_CC_GROUP
*&---------------------------------------------------------------------*
*       read the cost center group hierarchy of HMMA1
*----------------------------------------------------------------------*
FORM READ_CC_GROUP.
  DATA: T_SETLIST LIKE SETLIST OCCURS 0 WITH HEADER LINE.
  DATA: T_SETHIER LIKE SETHIER OCCURS 0 WITH HEADER LINE.
  DATA: T_SETVALUES LIKE SETVALUES OCCURS 0 WITH HEADER LINE.
  DATA: L_GROUP LIKE RKSB1-KSGRU.
  DATA: L_BEG TYPE I, L_END TYPE I.

  L_GROUP = 'HMMA1'.
  CALL FUNCTION 'G_SET_LIST_SELECT'
    EXPORTING
      SETCLASS      = '0101'
      SHORTNAME     = L_GROUP
      KOKRS         = 'H201'
      KTOPL         = 'HNA1'
    TABLES
      MATCHING_SETS = T_SETLIST.
  IF T_SETLIST[] IS INITIAL.
    MESSAGE E002(SY) WITH 'Cost Center group does not exist'.
    EXIT.
  ELSE.
    READ TABLE T_SETLIST INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      SETID                     = T_SETLIST-SETNAME
    TABLES
      SET_HIERARCHY             = T_SETHIER
      SET_VALUES                = T_SETVALUES
    EXCEPTIONS
      SET_NOT_FOUND             = 1
      ILLEGAL_FIELD_REPLACEMENT = 2
      ILLEGAL_TABLE_REPLACEMENT = 3
      OTHERS                    = 4.

  IF SY-SUBRC <> 0.
    MESSAGE E002(SY) WITH 'Cost Center group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO GROUP TABLE
  L_BEG = 1.
  LOOP AT T_SETHIER.
    IF T_SETHIER-LEVEL = '1'.
      CLEAR: IT_CC_GRP.
      IT_CC_GRP-GRP1 = T_SETHIER-SHORTNAME.
    ENDIF.

    IF T_SETHIER-LEVEL = '2'.
      IT_CC_GRP-GRP2 = T_SETHIER-SHORTNAME.
    ENDIF.

    IF T_SETHIER-LEVEL = '3'.
      IT_CC_GRP-GRP3 = T_SETHIER-SHORTNAME.
    ENDIF.

    IF T_SETHIER-VCOUNT NE 0.
      L_END = L_BEG + T_SETHIER-VCOUNT - 1.
      LOOP AT T_SETVALUES FROM L_BEG TO L_END.
        L_BEG = L_BEG + 1.
        MOVE-CORRESPONDING T_SETVALUES TO IT_CC_GRP.
        APPEND IT_CC_GRP.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " READ_CC_GROUP

*&---------------------------------------------------------------------*
*&      Form  READ_CE
*&---------------------------------------------------------------------*
*   read the cost element group for report
*----------------------------------------------------------------------*
FORM READ_CE.
  DATA: T_SETLIST LIKE SETLIST OCCURS 0 WITH HEADER LINE.
  DATA: T_SETHIER LIKE SETHIER OCCURS 0 WITH HEADER LINE.
  DATA: T_SETVALUES LIKE SETVALUES OCCURS 0 WITH HEADER LINE.
  DATA: L_INDH LIKE SY-TABIX,
        L_INDV LIKE SY-TABIX.

  CALL FUNCTION 'G_SET_LIST_SELECT'
    EXPORTING
      SETCLASS      = '0102'
      SHORTNAME     = 'H201_HR'
      KOKRS         = 'H201'
      KTOPL         = 'HNA1'
    TABLES
      MATCHING_SETS = T_SETLIST.
  IF T_SETLIST[] IS INITIAL.
    MESSAGE E002(SY) WITH 'Cost element group does not exist'.
    EXIT.
  ELSE.
    READ TABLE T_SETLIST INDEX 1.
  ENDIF.

  CALL FUNCTION 'G_SET_TREE_IMPORT'
    EXPORTING
      SETID                     = T_SETLIST-SETNAME
    TABLES
      SET_HIERARCHY             = T_SETHIER
      SET_VALUES                = T_SETVALUES
    EXCEPTIONS
      SET_NOT_FOUND             = 1
      ILLEGAL_FIELD_REPLACEMENT = 2
      ILLEGAL_TABLE_REPLACEMENT = 3
      OTHERS                    = 4.

  IF SY-SUBRC <> 0.
    MESSAGE E002(SY) WITH 'Cost element group does not exist'.
    EXIT.
  ENDIF.
* TRANSFER THE VALUE TO CE RANGE.
  R_CE-SIGN = 'I'.
  R_CE-OPTION = 'BT'.
  LOOP AT T_SETVALUES.
    R_CE-LOW  = T_SETVALUES-FROM.
    R_CE-HIGH = T_SETVALUES-TO.
    APPEND R_CE.
  ENDLOOP.
  CLEAR R_CE.
* SET THE NODE NAME
  L_INDH = L_INDV = 1.

  LOOP AT T_SETHIER.
    IF T_SETHIER-VCOUNT NE 0.
      L_INDH = L_INDV + T_SETHIER-VCOUNT - 1.
      LOOP AT T_SETVALUES FROM L_INDV TO L_INDH.
        L_INDV = L_INDV + 1.
        T_SETVALUES-LFIELDNAME = T_SETHIER-SETID+8(20).
        MODIFY T_SETVALUES.
      ENDLOOP.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  T_CEHIER[] = T_SETHIER[].
  T_CEVALUES[] = T_SETVALUES[].
ENDFORM.                    " READ_CE
*&---------------------------------------------------------------------*
*&      Form  GET_COST
*&---------------------------------------------------------------------*
*   read the labor cost data by standard program
*----------------------------------------------------------------------*
FORM GET_COST.

  PERFORM MAKE_SELECTION.
* BEGIN OF UD1K949915
  DATA: GV_LAST_DAY  TYPE DATUM,
        GV_FIRST_DAY TYPE DATUM.

  CONCATENATE P_GJAHR P_PERIO-LOW+1(2) '01' INTO GV_FIRST_DAY.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      DAY_IN            = GV_FIRST_DAY
    IMPORTING
      LAST_DAY_OF_MONTH = GV_LAST_DAY.

*  REFRESH r_ce.
*  r_ce-low = '0000601200'.
*  r_ce-high = r_ce-low.
*  r_ce-sign = 'I'.
*  r_ce-option = 'BT'.
*  APPEND r_ce.

  SELECT * INTO TABLE IT_BSIS
    FROM BSIS
   WHERE BUKRS = P_KOKRS
     AND HKONT IN R_CE
     AND BUDAT BETWEEN GV_FIRST_DAY AND GV_LAST_DAY
     AND GJAHR = P_GJAHR.

  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE IT_BSIS
    FROM BSAS
   WHERE BUKRS = P_KOKRS
     AND HKONT IN R_CE
     AND BUDAT BETWEEN GV_FIRST_DAY AND GV_LAST_DAY
     AND GJAHR = P_GJAHR.

*- U1 START
  IF P_ARCH EQ 'X'.
    PERFORM ARCHIVE_READ_BSIS USING  GV_FIRST_DAY
                                     GV_LAST_DAY.

    PERFORM ARCHIVE_READ_BSAS USING  GV_FIRST_DAY
                                     GV_LAST_DAY.
  ENDIF.
*- U1 End
* END OF UD1K949915.

  SUBMIT ZRCO_RPCPCC00_01
     WITH SELECTION-TABLE RSPAR
     AND RETURN.
*  IMPORT THE RESULT

*// 2011.09.22 by yn.kim ==> structure [list_table] is changed for ECC6
  IMPORT LIST_TABLE[] FROM MEMORY ID 'LIST'.
  FREE MEMORY.

* BEGIN OF UD1K949915

* BEGIN OF UD1K952793
  DATA: I_PA0001 LIKE PA0001 OCCURS 0 WITH HEADER LINE.
  SELECT * INTO TABLE I_PA0001 FROM PA0001
     WHERE PERNR IN P_PERNR
       AND ENDDA = '99991231'
       AND BUKRS =  P_KOKRS.
  SORT I_PA0001 BY PERNR.
* END OF UD1K952793

* Process working hours
  LOOP AT LIST_TABLE.
    IT_HOUR-KOSTL = LIST_TABLE-KOSTL.

    IF LIST_TABLE-LGART IN R_LGART1.
* BEGIN OF UD1K952793
      READ TABLE I_PA0001 WITH KEY PERNR = LIST_TABLE-PERNR
                               BINARY SEARCH.
      IF SY-SUBRC = 0.
        IF I_PA0001-PERSK = 'U2' OR I_PA0001-PERSK = 'U3' OR
           I_PA0001-PERSK = 'UD'.
          IT_HOUR-TREGU  = LIST_TABLE-ANZHL.
        ELSEIF I_PA0001-PERSK = 'U0'.
          IT_HOUR-TREGH  = LIST_TABLE-ANZHL.
        ENDIF.
      ENDIF.
*      it_hour-tregh  = list_table-anzhl.
* END OF UD1K952793
    ELSEIF LIST_TABLE-LGART IN R_LGART2.
      IT_HOUR-TOVER  = LIST_TABLE-ANZHL.
    ELSEIF LIST_TABLE-LGART IN R_LGART3.
      IT_HOUR-TOTHR  = LIST_TABLE-ANZHL.
    ENDIF.

* BEGIN OF UD1K952793
*   it_hour-thour = it_hour-tregu + it_hour-tover + it_hour-tothr.
* END OF UD1K952793

    COLLECT IT_HOUR. CLEAR IT_HOUR.
  ENDLOOP.

* BEGIN OF UD1K952793
  LOOP AT IT_HOUR.
    SELECT SUM( MENGE ) INTO IT_HOUR-TTEMP
      FROM ZTCO_MHOS
     WHERE KOKRS = P_KOKRS
       AND BDATJ = P_GJAHR
       AND POPER IN P_PERIO
       AND KOSTL = IT_HOUR-KOSTL.

    IT_HOUR-THOUR = IT_HOUR-TREGU + IT_HOUR-TOVER + IT_HOUR-TOTHR +
                    IT_HOUR-TREGH + IT_HOUR-TTEMP.

    MODIFY IT_HOUR INDEX SY-TABIX TRANSPORTING TTEMP THOUR.
  ENDLOOP.
* END OF UD1K952793

* Process head count
  SORT LIST_TABLE BY KOSTL PERNR.
  DELETE ADJACENT DUPLICATES FROM LIST_TABLE
  COMPARING KOSTL PERNR.

  LOOP AT LIST_TABLE.
    IT_HEADCNT-KOSTL     = LIST_TABLE-KOSTL.
    IT_HEADCNT-EMP_COUNT = 1.

    COLLECT IT_HEADCNT.
  ENDLOOP.

  REFRESH LIST_TABLE. CLEAR LIST_TABLE.

  LOOP AT IT_BSIS.
    LIST_TABLE-KOSTL = IT_BSIS-KOSTL.
    LIST_TABLE-BUDAT = IT_BSIS-BUDAT.
    LIST_TABLE-HKONT = IT_BSIS-HKONT.

    IF IT_BSIS-SHKZG = 'H'.
      IT_BSIS-DMBTR = IT_BSIS-DMBTR * -1.
    ENDIF.

    LIST_TABLE-BETRG = IT_BSIS-DMBTR.

    APPEND LIST_TABLE.
  ENDLOOP.
* END OF UD1K949915

  DESCRIBE TABLE LIST_TABLE LINES L_LINES.

  TABLES: ZCOLAB02, PA0001, T528T, T513S, PA0008.
  DATA: I_ZCOLAB02 LIKE ZCOLAB02 OCCURS 0 WITH HEADER LINE,
*       i_pa0001   LIKE pa0001   OCCURS 0 WITH HEADER LINE, "UD1K952793
        I_PA0008   LIKE PA0008   OCCURS 0 WITH HEADER LINE,
        I_T528T    LIKE T528T    OCCURS 0 WITH HEADER LINE,
        I_T513S    LIKE T513S    OCCURS 0 WITH HEADER LINE.
  DATA: L_DATS(8) TYPE C.
  SELECT * INTO TABLE I_PA0001 FROM PA0001
     WHERE BUKRS =  P_KOKRS
       AND PERNR IN P_PERNR
       ORDER BY PERNR  ENDDA.
*  SELECT * INTO TABLE i_t528t FROM t528t
*     WHERE sprsl = sy-langu AND otype = 'S' AND endda = '99991231'.
*  SELECT * INTO TABLE i_t513s FROM t513s
*     WHERE sprsl = sy-langu AND endda = '99991231'.
  SELECT * INTO TABLE I_PA0008 FROM PA0008
    WHERE PERNR IN P_PERNR
    ORDER BY PERNR  ENDDA.

  LOOP AT LIST_TABLE.

*   ANZHL       time
*   BETRG       amt
*   BETRG_D     amt debit
*   BETRG_C     MTD amt credit
*   MTD_ANZHL   MTD time
*   MTD_BETRG   MTD amt
*   MTD_BETRG_D YTD amt debit
*   MTD_BETRG_C YTD amt credit
*   YTD_ANZHL   YTD time
*   YTD_BETRG   YTD headcount
*   YTD_BETRG_D YTD headcount
*   YTD_BETRG_C YTD headcount
*   EMP_COUNT   Headcount

    MOVE-CORRESPONDING LIST_TABLE  TO LIST_TAB.
    CLEAR LIST_TAB-BUDAT.
    COLLECT LIST_TAB.

* save to ztable for further analysis
*    MOVE-CORRESPONDING list_table  TO i_zcolab02.
*
*    LOOP AT i_pa0001 WHERE pernr = list_table-pernr
*                       AND endda >= list_table-budat.
*      EXIT.
*    ENDLOOP.
*    i_zcolab02-persg = i_pa0001-persg.
*    i_zcolab02-persk = i_pa0001-persk.
*
*    LOOP AT i_pa0008 WHERE pernr = list_table-pernr
*                       AND endda >= list_table-budat.
*      EXIT.
*    ENDLOOP.
*    i_zcolab02-trfar = i_pa0008-trfar.
*    i_zcolab02-trfgb = i_pa0008-trfgb.
*    i_zcolab02-trfgr = i_pa0008-trfgr. "grp
*    i_zcolab02-trfst = i_pa0008-trfst. "lev
*
*
*    i_zcolab02-kokrs = p_kokrs.
*    i_zcolab02-gjahr = p_gjahr.
*    l_dats = list_table-budat.
*    i_zcolab02-monat = l_dats+4(2).
*    COLLECT i_zcolab02.  CLEAR i_zcolab02.
  ENDLOOP.

*  delete from zcolab02 where kokrs = p_kokrs
*                         and gjahr = p_gjahr
*                         and monat = p_perio-low.
*  commit work.
*  insert zcolab02 from table i_zcolab02.

ENDFORM.                    " GET_COST
*&---------------------------------------------------------------------*
*&      Form  MAKE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_SELECTION.
*COMPANY CODE
  RSPAR-SELNAME = 'P_BUKRS'. RSPAR-KIND = 'P'.
  RSPAR-LOW  = 'H201'.
  APPEND RSPAR.
  CLEAR: RSPAR.
*PERIOD
  LOOP AT P_PERBL.
    RSPAR-SELNAME = 'P_BUDAT'.
    RSPAR-KIND = 'S'.
    RSPAR-SIGN = P_PERBL-SIGN.
    RSPAR-OPTION = P_PERBL-OPTION.
    RSPAR-LOW  = P_PERBL-LOW.
    RSPAR-HIGH = P_PERBL-HIGH.
    APPEND RSPAR.
    CLEAR: RSPAR.
  ENDLOOP.
*COST CENTER
  LOOP AT R_CC.
    RSPAR-SELNAME = 'P_KOSTL'.
    RSPAR-KIND    = 'S'.
    RSPAR-SIGN    = R_CC-SIGN.
    RSPAR-OPTION  = R_CC-OPTION.
    RSPAR-LOW      = R_CC-LOW.
    RSPAR-HIGH   = R_CC-HIGH.
    APPEND RSPAR.
    CLEAR: RSPAR.
  ENDLOOP.
*GENEAL LEDGER(COST ELEMENT)
  LOOP AT R_CE.
    RSPAR-SELNAME = 'P_HKONT'.
    RSPAR-KIND    = 'S'.
    RSPAR-SIGN    = R_CE-SIGN.
    RSPAR-OPTION  = R_CE-OPTION.
    RSPAR-LOW     = R_CE-LOW.
    RSPAR-HIGH    = R_CE-HIGH.
    APPEND RSPAR.
    CLEAR: RSPAR.
  ENDLOOP.
*WAGE TYPE RADIO BUTTON
  RSPAR-SELNAME = 'WTYPE2'.
  RSPAR-KIND = 'P'.
  RSPAR-LOW  = 'X'.
  APPEND RSPAR.
  CLEAR: RSPAR.
*WAGE TYPE
*  LOOP AT R_WT.
*    RSPAR-SELNAME = 'r_lgart'.
*    RSPAR-KIND    = 'P'.
*    RSPAR-LOW     = R_WT-LOW.
*    APPEND RSPAR.
*    CLEAR: RSPAR.
*  ENDLOOP.

*EXCLUDING ACCRUE VALUE
  RSPAR-SELNAME = 'P_EX_ACC'.
  RSPAR-KIND = 'P'.
*  rspar-low  = space.
* accrual from CO... no need to get data from HR
  IF P_ACC_HR = ' '.
    RSPAR-LOW  = 'X'.
  ELSE.
    RSPAR-LOW  =  P_EX_ACC.
  ENDIF.

  APPEND RSPAR.
  CLEAR: RSPAR.

* add employee temporary
  RSPAR-SELNAME = 'P_PERNR'.
  RSPAR-KIND    = 'S'.
  RSPAR-SIGN    = P_PERNR-SIGN.
  RSPAR-OPTION  = P_PERNR-OPTION.
  RSPAR-LOW     = P_PERNR-LOW.
  RSPAR-HIGH    = P_PERNR-HIGH.
  APPEND RSPAR.
  CLEAR: RSPAR.



ENDFORM.                    " MAKE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  SET_WAGETYPE
*&---------------------------------------------------------------------*
*       Set the wage type, but code did not use this now
*----------------------------------------------------------------------*
FORM SET_WAGETYPE.
  REFRESH R_WT. CLEAR R_WT.
  CLEAR : R_LGART1, R_LGART1[],R_LGART2, R_LGART2[],
          R_LGART3, R_LGART3[].

  R_LGART1-OPTION = 'EQ'. R_LGART1-SIGN   = 'I'.
  R_LGART2-OPTION = 'EQ'. R_LGART2-SIGN   = 'I'.
  R_LGART3-OPTION = 'EQ'. R_LGART3-SIGN   = 'I'.
  R_WT-OPTION     = 'EQ'. R_WT-SIGN       = 'I'.

  TABLES: ZTCO_MH_TIME.
  SELECT * FROM ZTCO_MH_TIME.

    CASE ZTCO_MH_TIME-ZGART.
* Regular
      WHEN '1'.
        R_LGART1-LOW = ZTCO_MH_TIME-LGART. APPEND R_LGART1.
        R_WT-LOW = ZTCO_MH_TIME-LGART. APPEND R_WT.

* Over Time
      WHEN '2'.
        R_LGART2-LOW = ZTCO_MH_TIME-LGART. APPEND R_LGART2.
        R_WT-LOW = ZTCO_MH_TIME-LGART. APPEND R_WT.

* paid leave, holiday
      WHEN '5' OR '9'.
        R_LGART3-LOW = ZTCO_MH_TIME-LGART. APPEND R_LGART3.
        R_WT-LOW = ZTCO_MH_TIME-LGART. APPEND R_WT.
    ENDCASE.

  ENDSELECT.

  CLEAR: R_LGART1, R_LGART2, R_LGART3.
ENDFORM.                    " SET_WAGETYPE
*&---------------------------------------------------------------------*
*&      Form  GET_EE_GROUP
*&---------------------------------------------------------------------*
*     read the emplyess information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_EE_GROUP.
  DATA: PT0007 LIKE PA0007 OCCURS 0 WITH HEADER LINE.
  DATA: SUBRC  LIKE SY-SUBRC.
  DATA: WA_EE  TYPE S_EE.
  DATA: WA_EEL TYPE S_EE.
  DATA: LT_EE  TYPE STANDARD TABLE OF S_EE.
  DATA: LT_T501T LIKE T501T OCCURS 0 WITH HEADER LINE.
  READ TABLE P_PERBL INDEX 1.
* READ THE EMPOLYEE GROUP AND SUBGROUP
  SELECT PERNR PERSG PERSK
   INTO CORRESPONDING FIELDS OF TABLE IT_EE
   FROM PA0001
   FOR ALL ENTRIES IN LIST_TABLE
   WHERE PERNR = LIST_TABLE-PERNR AND
         BEGDA LE P_PERBL-HIGH     AND
         ENDDA GE P_PERBL-HIGH     AND
         SPRPS NE 'X'.
  SORT IT_EE BY PERNR.
  DELETE ADJACENT DUPLICATES FROM IT_EE
     COMPARING PERNR.

* Check if terminated EE EXIST
  LOOP AT IT_EE INTO WA_EE.
    IF WA_EE-PERSG = '5'.
*   READ THE PREVIOUS RECORD EE GROUP.
      SELECT PERNR PERSG PERSK BEGDA
        INTO CORRESPONDING FIELDS OF TABLE LT_EE
        FROM PA0001
        WHERE PERNR = WA_EE-PERNR.
      SORT LT_EE BY BEGDA DESCENDING.
      LOOP AT LT_EE INTO WA_EEL.
        IF WA_EEL-PERSG NE '5'.
          WA_EE-PERSG = WA_EEL-PERSG.
          MODIFY IT_EE FROM WA_EE.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF WA_EE-PERSG NE '1' AND
         WA_EE-PERSG NE '9'.
        WA_EE-PERSG = '1'.  "IF OTHER GROUP, CONSIDER AS GROUP '1'.
        MODIFY IT_EE FROM WA_EE.
      ENDIF.
    ENDIF.
  ENDLOOP.

* READ THE GROUP TEXT
  SELECT * INTO TABLE LT_T501T
    FROM T501T
    FOR ALL ENTRIES IN IT_EE
    WHERE PERSG = IT_EE-PERSG AND
          SPRSL = 'EN'      .
  LOOP AT IT_EE INTO WA_EE.
    CLEAR: LT_T501T.
    READ TABLE LT_T501T WITH KEY PERSG = WA_EE-PERSG.
    IF SY-SUBRC EQ 0.
      WA_EE-PTEXT = LT_T501T-PTEXT.
      MODIFY IT_EE FROM WA_EE.
    ENDIF.
  ENDLOOP.

* READ EE WORK SCHEDULE RULE
  LOOP AT IT_EE INTO WA_EE.
    CLEAR: PT0007, PT0007[].
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        PERNR     = WA_EE-PERNR
        INFTY     = '0007'
        BEGDA     = P_PERBL-HIGH
        ENDDA     = P_PERBL-HIGH
      IMPORTING
        SUBRC     = SUBRC
      TABLES
        INFTY_TAB = PT0007.
    IF SUBRC NE 0.

      MESSAGE E000 WITH 'WORK SCHEDULE ERROR FOR' WA_EE-PERNR.

    ELSE.
      READ TABLE PT0007 INDEX 1.
      WA_EE-SCHKZ = PT0007-SCHKZ.
      MODIFY IT_EE FROM WA_EE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_EE_GROUP
" GET_SUPPORT_COST
*&---------------------------------------------------------------------*
*&      Form  DATA_SUMMARY
*&---------------------------------------------------------------------*
*       summarize the cost data by cost element EE group
*----------------------------------------------------------------------*
FORM DATA_SUMMARY.
  PERFORM CE_SUMMARY.
  PERFORM INCLUDE_CO_ACCRUAL.
  PERFORM GROUP_SUMMARY.
  PERFORM GET_TEXT.
ENDFORM.                    " DATA_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  CE_SUMMARY
*&---------------------------------------------------------------------*
*       SUMMARIZE THE DATA FOR EACH EMPLOYEE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CE_SUMMARY.
  DATA: L_TABIX LIKE SY-TABIX.
  DATA: WA_LIST LIKE LIST_TABLE.

  SORT LIST_TAB BY PERNR KOSTL HKONT.
  CLEAR: WA_OUT.

  LOOP AT LIST_TAB.
    L_TABIX = SY-TABIX + 1.

*   SUMMARIZE THE COST AND WORKING HOURS
    PERFORM SAL_TIME_SUM USING WA_OUT
                               LIST_TAB-HKONT
                               LIST_TAB-BETRG.

*   TIME
    IF LIST_TAB-LGART IN R_LGART1.
      WA_OUT-TREGU  = WA_OUT-TREGU + LIST_TAB-ANZHL.
    ELSEIF LIST_TAB-LGART IN R_LGART2.
      WA_OUT-TOVER  = WA_OUT-TOVER + LIST_TAB-ANZHL.
    ELSEIF LIST_TAB-LGART IN R_LGART3.
      WA_OUT-TOTHR  = WA_OUT-TOTHR + LIST_TAB-ANZHL.
    ENDIF.

*   CHECK IF THE NEXT RECORD IS FOR THE SAME EMPLOYEE
    CLEAR: WA_LIST.
    READ TABLE LIST_TAB INTO WA_LIST INDEX L_TABIX.
    IF WA_LIST-PERNR <> LIST_TAB-PERNR
    OR WA_LIST-KOSTL <> LIST_TAB-KOSTL.

      PERFORM GET_OTHER_FIELDS USING WA_OUT LIST_TAB.
*     "SUM THE SUBTOTAL
      WA_OUT-TOTCO = WA_OUT-REGUL + WA_OUT-OVERT  "LABOR COST SUBTOTAL
                   + WA_OUT-BONUS + WA_OUT-ZLEAV
                   + WA_OUT-OTHCO + WA_OUT-REGUH            "UD1K952793
                   + WA_OUT-TEMPO.                          "UD1K952793
      WA_OUT-TOTBE = WA_OUT-PENSN + WA_OUT-HEALTH
                   + WA_OUT-WORKC + WA_OUT-INSUR  "BENEFIT SUBTOTAL
                   + WA_OUT-TAX   + WA_OUT-OTHBE.
      WA_OUT-THOUR = WA_OUT-TREGU + WA_OUT-TOVER  "WORK HOUR SUBTOTAL
                   + WA_OUT-TOTHR.
      WA_OUT-TCOST = WA_OUT-TOTCO + WA_OUT-TOTBE. "TOTAL OF COST/BENEFIT
*     "SAVE THE SUMMARY RESULT FOR EACH EMPLOYEE

      APPEND WA_OUT TO IT_OUT. CLEAR: WA_OUT.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " CE_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  SAL_TIME_SUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*      -->P_LIST_TABLE  text
*----------------------------------------------------------------------*
FORM SAL_TIME_SUM USING    PA_OUT  STRUCTURE WA_OUT
                           F_HKONT LIKE PNA_CCDATA-HKONT
                           F_BETRG LIKE PNA_CCDATA-BETRG.
*                          pa_list structure pna_ccdata.
  DATA: L_NODE(30).

*GET THE COST ELEMENT GROUP NODE NAME
  PERFORM GET_CENODENAME USING F_HKONT L_NODE.

*SUMMARIZE THE COST AND BENEFIT
  CASE L_NODE.
* BEGIN OF UD1K952793
*    WHEN 'H201_HR_11'.  "REGULAR PAY
*      pa_out-regul  = pa_out-regul + f_betrg.

    WHEN 'H201_HR_16'.  "SALARY PAY
      PA_OUT-REGUL  = PA_OUT-REGUL + F_BETRG.
    WHEN 'H201_HR_17'.  "HOURLY PAY
      PA_OUT-REGUH  = PA_OUT-REGUH + F_BETRG.
* END OF UD1K952793

    WHEN 'H201_HR_12'.  "OVERTIME PAY
      PA_OUT-OVERT  = PA_OUT-OVERT + F_BETRG.

    WHEN 'H201_HR_13'.  "BONUS PAY
      PA_OUT-BONUS  = PA_OUT-BONUS + F_BETRG.

    WHEN 'H201_HR_14'.  "LEAVE PAY
      PA_OUT-ZLEAV  = PA_OUT-ZLEAV + F_BETRG.

    WHEN 'H201_HR_15'.  "OTHER PAY
      PA_OUT-OTHCO  = PA_OUT-OTHCO + F_BETRG.

    WHEN 'H201_HR_18'.  "TEMPORARY                          "UD1K952793
      PA_OUT-TEMPO  = PA_OUT-TEMPO + F_BETRG.               "UD1K952793

    WHEN 'H201_HR_21'.  "PENSION & 401K
      PA_OUT-PENSN  = PA_OUT-PENSN + F_BETRG.

    WHEN 'H201_HR_22'.  "INSURANCE
      PA_OUT-HEALTH = PA_OUT-HEALTH + F_BETRG.

    WHEN 'H201_HR_23'.  "INSURANCE
      PA_OUT-WORKC  = PA_OUT-WORKC + F_BETRG.

    WHEN 'H201_HR_24'.  "INSURANCE
      PA_OUT-INSUR  = PA_OUT-INSUR + F_BETRG.

    WHEN 'H201_HR_25'.  "TAX
      PA_OUT-TAX  = PA_OUT-TAX + F_BETRG.

    WHEN 'H201_HR_26'.  "OTHERS
      PA_OUT-OTHBE  = PA_OUT-OTHBE + F_BETRG.

    WHEN OTHERS.

      PA_OUT-OTHBE  = PA_OUT-OTHBE + F_BETRG.
  ENDCASE.

*SUMMARIZE THE WORKING HOURS
*  case l_node.
*    when 'H201_HR_11'.  "REGULAR WORKING HOUR
*      pa_out-tregu  = pa_out-tregu + pa_list-anzhl.
*
*    when 'H201_HR_12'.  "OVERTIME WORKING HOUR
*      pa_out-tover  = pa_out-tover + pa_list-anzhl.
*
*    when others.        "OTHER WORKING HOUR
*      pa_out-tothr  = pa_out-tothr + pa_list-anzhl.
*  endcase.

ENDFORM.                    " SAL_TIME_SUM
*&---------------------------------------------------------------------*
*&      Form  GET_CENODENAME
*&---------------------------------------------------------------------*
*       FIND THE COST ELEMENT GROUP NODE NAME
*----------------------------------------------------------------------*
*      -->P_PA_LIST_HKONT  text
*----------------------------------------------------------------------*
FORM GET_CENODENAME USING  P_HKONT LIKE PNA_CCDATA-HKONT
                           P_NODE .

  LOOP AT T_CEVALUES.
    IF P_HKONT GE T_CEVALUES-FROM AND
       P_HKONT LE T_CEVALUES-TO.
      P_NODE = T_CEVALUES-LFIELDNAME.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_CENODENAME
*&---------------------------------------------------------------------*
*&      Form  GET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*      -->P_LIST_TABLE  text
*----------------------------------------------------------------------*
FORM GET_OTHER_FIELDS USING  PA_OUT  STRUCTURE WA_OUT
                             PA_LIST STRUCTURE PNA_CCDATA.
  DATA: WA_EE  TYPE S_EE.

  PA_OUT-KOSTL  = PA_LIST-KOSTL.
*Headcount...
*  pa_out-zcunt  = 1.                                       "UD1K949915

*GET THE EE GROUP/SUBGROUP/WORKING SCHEDULE RULE
  CLEAR: WA_EE.
  READ TABLE IT_EE INTO WA_EE WITH KEY PERNR = PA_LIST-PERNR
                   BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    PERFORM GET_EMP_CATEG(ZACOP01) USING WA_EE-PERSG WA_EE-PERSK
                                   CHANGING PA_OUT-EMPCT.
    PA_OUT-SCHKZ  = WA_EE-SCHKZ.

  ELSE.
    PA_OUT-EMPCT = 'X'.

*    pa_out-persg  = wa_ee-persg.
*    pa_out-gtext  = wa_ee-ptext.
*    pa_out-persk  = wa_ee-persk.

    PA_OUT-SCHKZ  = WA_EE-SCHKZ.
  ENDIF.
ENDFORM.                    " GET_OTHER_FIELDS
*&---------------------------------------------------------------------*
*&      Form  GROUP_SUMMARY
*&---------------------------------------------------------------------*
*       DATA SUMMARY BY
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GROUP_SUMMARY.

  CLEAR: IT_OUT1[], WA_OUT, IT_OUT_S.
  IT_OUT_S[] = IT_OUT[].
  SORT IT_OUT BY KOSTL EMPCT SCHKZ.  "persg persk schkz.
  LOOP AT IT_OUT INTO WA_OUT.
    COLLECT WA_OUT INTO IT_OUT1.
  ENDLOOP.
  CLEAR IT_OUT.
  IT_OUT = IT_OUT1.
  CLEAR IT_OUT1.

ENDFORM.                    " GROUP_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  GET_SUPPORT_INFO
*&---------------------------------------------------------------------*
*      GET THE SUPPORT AND BEING SUPPORT COST BT COST CENTERS
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM GET_SUPPORT_INFO.
  RANGES: R_CE  FOR COVP-KSTAR.
  DATA: L_CC    LIKE CSKS-KOSTL.
  DATA: BEGIN OF LT_CC OCCURS 0,
          KOSTL    LIKE CSKS-KOSTL,
          OBJNR    LIKE CSKS-OBJNR,
        END OF LT_CC.

* GET THE COST ELEMENT FOR REGULAR SALARY/WAGE
  LOOP AT T_CEVALUES WHERE LFIELDNAME CA 'H201_HR'.
    R_CE-SIGN   = 'I'.
    R_CE-OPTION = 'BT'.
    R_CE-LOW    = T_CEVALUES-FROM.
    R_CE-HIGH   = T_CEVALUES-TO.
    APPEND R_CE.
  ENDLOOP.

*** UD1K940482 ( start )
*** New Logic for gathering the row data by IG.MOON 5/5/2007

  PERFORM GATHER_ROW_DATA.

*** UD1K940482 ( end )

*** Commented by IG.MOON 5/5/2007
*** UD1K940482 ( start )
***
****READ THE SUPPPORT RECORDS
***  select a~kostl  b~objnr  b~parob1
***         b~kstar  b~meinb
***         sum( b~wkgbtr ) sum( b~mbgbtr )
***    into table it_covp
***    from  csks as a inner join covp as b
***      on  a~objnr = b~objnr   and
***          a~kokrs = b~kokrs
***    where b~refbt  = 'K'       and
***          b~refbn  = 'SUPPORT' and
***          b~kokrs  = p_kokrs   and
***          b~vrgng  = 'KAMV'    and
***          b~perio  in p_perio  and
***          b~gjahr  eq p_gjahr  and
***          b~kstar  in r_ce     and
***        ( b~stflg  ne 'X'       or
***          b~stokz  ne 'X' )    and
***          a~kostl in r_cc
***     group by a~kostl b~objnr b~parob1 b~kstar b~meinb.
*** UD1K940482 ( end )

* DELETE THE RECORD THAT CYCLE IS NOT P_CYCLE
*  if 1 = 2.
*    loop at it_covp.
*      if it_covp-sgtxt cs p_cycle.
*      else.
*        delete it_covp.
*      endif.
*    endloop.
*  endif.

* GET THE PARTNER COST CENTER
  SELECT KOSTL OBJNR INTO TABLE LT_CC
   FROM CSKS
   FOR ALL ENTRIES IN IT_COVP
   WHERE OBJNR = IT_COVP-PAROB1.

* by IG. MOON 5/4/2007
*** UD1K940482
  SORT LT_CC BY OBJNR .

  DATA: L_IDX LIKE SY-TABIX.
  LOOP AT IT_COVP.
    L_IDX = SY-TABIX.
    READ TABLE LT_CC WITH KEY OBJNR = IT_COVP-PAROB1 BINARY SEARCH .
    IF SY-SUBRC EQ 0.
      IT_COVP-PKOST = LT_CC-KOSTL.
      MODIFY IT_COVP INDEX L_IDX TRANSPORTING PKOST.
    ENDIF.
  ENDLOOP.

*NO split regular / OT ; expense .. no split... Andy
  SELECT KOSTL SRKOSTL SUM( ANZHL )
       INTO TABLE IT_MHA_SUP
       FROM ZTCO_MHA
       WHERE KOKRS = P_KOKRS
         AND GJAHR = P_GJAHR
         AND PERID IN P_PERIO
         AND KOSTL IN R_CC
         AND LGART = '3'
       GROUP BY KOSTL SRKOSTL.

ENDFORM.                    " GET_SUPPORT_INFO
*&---------------------------------------------------------------------*
*&      Form  SUPPORT_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUPPORT_SUMMARY.
  EXIT.

  DATA:  L_EXIST .
  DATA:  WA_OUT1 TYPE S_OUT.
  DATA:  LT_OUT TYPE STANDARD TABLE OF S_OUT.
  CLEAR: WA_OUT,
         IT_OUT1.
* APPEND THE SUPPORT COST
*  loop at it_covp.
*    wa_out-kostl = it_covp-kostl.
*    if it_covp-beknz = 'S'.
*      wa_out-frocc = it_covp-pkost.
*    elseif it_covp-beknz = 'H'.
*      wa_out-tocc  = it_covp-pkost.
*    endif.
*    wa_out-regul = - it_covp-wkgbtr.
*    append wa_out to it_out1.
*    clear wa_out.
*  endloop.

* APPEND THE SUPPORT TIME
  LOOP AT IT_CATSCO.
    WA_OUT-KOSTL = IT_CATSCO-SKOSTL.
    WA_OUT-TOCC  = IT_CATSCO-RKOSTL.
    WA_OUT-TREGU = IT_CATSCO-CATSHOURS.
    APPEND WA_OUT TO IT_OUT1.
    CLEAR WA_OUT.
    WA_OUT-KOSTL = IT_CATSCO-RKOSTL.
    WA_OUT-FROCC  = IT_CATSCO-SKOSTL.
    WA_OUT-TREGU = IT_CATSCO-CATSHOURS.
    APPEND WA_OUT TO IT_OUT1.
    CLEAR WA_OUT.
  ENDLOOP.
* DELETE THE ENTRIES THAT NOT IN THE ENTERED CC
  LOOP AT IT_OUT1 INTO WA_OUT.
    CLEAR: L_EXIST.
    LOOP AT R_CC.
      IF WA_OUT-KOSTL GE R_CC-LOW AND
         WA_OUT-KOSTL LE R_CC-HIGH.
        L_EXIST = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF L_EXIST NE 'X'.
      DELETE TABLE IT_OUT1 FROM WA_OUT.
    ENDIF.
  ENDLOOP.

* Sumarize for the same sender or receiver
  SORT IT_OUT1 BY KOSTL FROCC TOCC.
  LOOP AT IT_OUT1 INTO WA_OUT.
    COLLECT WA_OUT INTO LT_OUT.
  ENDLOOP.
* ATTACH THE SUPPORT RESULT TO THE OUTPT TABLE
  APPEND LINES OF LT_OUT TO IT_OUT.

ENDFORM.                    " SUPPORT_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  FINAL_SUMMARY
*&---------------------------------------------------------------------*
FORM FINAL_SUMMARY.
  DATA: WA_OUT1 TYPE S_OUT.
  DATA: WA_DIS  TYPE S_DIS.
  CLEAR: IT_OUT1, WA_OUT.
* for layout test. making some data in internal table
*   perform make_test_data.

* here need to be deleted end.

  SORT IT_OUT BY  KOSTL ASCENDING
                  FROCC ASCENDING
                  TOCC  ASCENDING
                  EMPCT.
*                  gtext descending
*                  ktext descending
*                  kztxt ascending.

  LOOP AT IT_OUT INTO WA_OUT.
*    wa_out-persg = space.
*    wa_out-persk = space.
*    wa_out-schkz = space.
    COLLECT WA_OUT INTO IT_OUT1.
  ENDLOOP.

  CLEAR: IT_OUT.
* ATTACH THE COST CENTER GROUP
  LOOP AT IT_OUT1 INTO WA_OUT.
    PERFORM FIND_CC_GROUP USING WA_OUT.
    MODIFY IT_OUT1 FROM WA_OUT.
  ENDLOOP.
*
  CLEAR IT_OUT.

* MOVE RESULT TO DISPLAY TABLE
  LOOP AT IT_OUT1 INTO WA_OUT.
    MOVE-CORRESPONDING WA_OUT TO WA_DIS.
    APPEND WA_DIS TO IT_DIS.
  ENDLOOP.

* BEGIN OF UD1K949915
  DATA: L_TABIX TYPE TABIX.

*  LOOP AT it_headcnt.
*    READ TABLE it_dis INTO wa_dis
*          WITH KEY kostl = it_headcnt-kostl.
*    IF sy-subrc = 0.
*      wa_dis-zcunt = it_headcnt-emp_count.
*      MODIFY it_dis FROM wa_dis
*      INDEX sy-tabix
*      TRANSPORTING zcunt.
*    ENDIF.
*  ENDLOOP.

  SORT IT_HEADCNT BY KOSTL.
  SORT IT_HOUR BY KOSTL.
  LOOP AT IT_DIS INTO WA_DIS.
    L_TABIX = SY-TABIX.

    READ TABLE IT_HEADCNT WITH KEY KOSTL = WA_DIS-KOSTL
                          BINARY SEARCH.
    IF SY-SUBRC = 0.
      WA_DIS-ZCUNT =  IT_HEADCNT-EMP_COUNT.
    ENDIF.

    READ TABLE IT_HOUR WITH KEY KOSTL = WA_DIS-KOSTL.
    IF SY-SUBRC = 0.
      MOVE-CORRESPONDING IT_HOUR TO WA_DIS.
    ENDIF.

    MODIFY IT_DIS FROM WA_DIS INDEX L_TABIX
           TRANSPORTING ZCUNT TREGU TOVER TOTHR THOUR
                        TREGH TTEMP.                        "UD1K952793

  ENDLOOP.
* END OF UD1K949915
ENDFORM.                    " FINSAL SUMMARY

*&---------------------------------------------------------------------*
*&      Form  GET_SUPPORT_HOUR
*&---------------------------------------------------------------------*
*     read the support hours
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM GET_SUPPORT_HOUR.

  EXIT.
  DATA: LT_CATS  LIKE IT_CATSCO OCCURS 0 WITH HEADER LINE.
  SELECT COUNTER STOKZ WORKDATE CATSHOURS
         SKOSTL  LSTAR RKOSTL
   INTO  TABLE LT_CATS
   FROM  CATSCO
   WHERE WORKDATE IN P_PERBL AND
         KOKRS = 'H201'      AND
       ( SKOSTL IN R_CC  OR
         RKOSTL IN R_CC  )   AND
         LSTAR = 'MAN_HR'.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.
  IT_CATSCO[] = LT_CATS[] .
*  DELETE THE REVERSED DOC
  LOOP AT LT_CATS.
    READ TABLE IT_CATSCO WITH KEY COUNTER = LT_CATS-COUNTER
                                  STOKZ   = 'X'.
    IF SY-SUBRC EQ 0.
      DELETE LT_CATS.
    ENDIF.
  ENDLOOP.
  CLEAR: IT_CATSCO[], IT_CATSCO.
  IT_CATSCO[] = LT_CATS[].
ENDFORM.                    " GET_SUPPORT_HOUR
*&---------------------------------------------------------------------*
*&      Form  MAKE_TEXT
*&---------------------------------------------------------------------*
*    Make the display text in report for employee group,subgroup and
*    working schedule rule name
*----------------------------------------------------------------------*
FORM MAKE_TEXT.
  PERFORM MAKE_PERSG_TEXT USING: '1' 'Active',
                                 '9' 'Inpatriate'.
  PERFORM MAKE_PERSK_TEXT USING: 'U0' 'Hourly',
                                 'U2' 'Salary',
                                 'U3' 'Salary',
                                 'U8' 'Contract'.

  TABLES: ZTCO_MH_WS.
  DATA: WSTXT(3) TYPE C,
        WSKEY(5) TYPE C.
  SELECT * FROM ZTCO_MH_WS.
    WSKEY+1(4) = ZTCO_MH_WS-SCHKZ.
    CONCATENATE 'WS' ZTCO_MH_WS-ANZSH INTO WSTXT.
    PERFORM MAKE_SCHKZ_TEXT USING: WSKEY  WSTXT.
  ENDSELECT.
*  perform make_schkz_text using: ' 1000' 'Standard',
*                                 ' 1001' 'Shift 1',
*                                 ' 1002' 'Shift 2',
*                                 ' 1003' 'Shift 3',
*                                 ' 2001' 'Shift 1',
*                                 ' 2002' 'Shift 2',
*                                 ' 2003' 'Shift 3',
*                                 ' 3001' 'Shift 1',
*                                 ' 3002' 'Shift 2',
*                                 ' 3003' 'Shift 3'.

ENDFORM.                    " MAKE_TEXT
*&---------------------------------------------------------------------*
*&      Form  MAKE_PERSK_TEXT
*&---------------------------------------------------------------------*
FORM MAKE_PERSK_TEXT USING P_1 P_2.
  IT_PERSK-PERSK = P_1.
  IT_PERSK-PTEXT = P_2.
  APPEND IT_PERSK.
ENDFORM.                    "make_persk_text
*&---------------------------------------------------------------------*
*&      Form  MAKE_PERSG_TEXT
*&---------------------------------------------------------------------*
FORM MAKE_PERSG_TEXT USING P_1 P_2.
  IT_PERSG-PERSG = P_1.
  IT_PERSG-PTEXT = P_2.
  APPEND IT_PERSG.
ENDFORM.                    "make_persg_text

*&---------------------------------------------------------------------*
*&      Form  MAKE_SCHKZ_TEXT
*&---------------------------------------------------------------------*
FORM MAKE_SCHKZ_TEXT USING P_1 P_2.
  IT_SCHKZ-SCHKZ = P_1.
  IT_SCHKZ-PTEXT = P_2.
  APPEND IT_SCHKZ.
ENDFORM.                    "make_schkz_text

*&---------------------------------------------------------------------*
*&      Form  GET_TEXT
*&---------------------------------------------------------------------*
FORM GET_TEXT .
  DATA: L_IDX LIKE SY-TABIX.

  LOOP AT IT_OUT INTO WA_OUT.
    CHECK WA_OUT-SCHKZ <> SPACE.

    L_IDX = SY-TABIX.

*ANDY - comment
** GET THE EE GROUP TEXT
** for terminated person, but we have cost for the period
** chang the person group to 'ACTIVE'
*    if wa_out-persg = '5'.
*      wa_out-persg = '1'.
*    endif.
*    clear: it_persg.
*    read table it_persg with key persg = wa_out-persg.
*    if sy-subrc eq 0.
*      wa_out-gtext = it_persg-ptext.
*    else.
*      message i000 with text-010.
*    endif.
** GET THE EE SUBGROUP TEXT
*    clear: it_persk.
*    read table it_persk with key persk = wa_out-persk.
*    if sy-subrc eq 0.
*      wa_out-ktext = it_persk-ptext.
*    else.
*      message e000 with text-010.
*    endif.

* GET THE EE WS RULE TEXT

    CLEAR: IT_SCHKZ.
    READ TABLE IT_SCHKZ WITH KEY SCHKZ = WA_OUT-SCHKZ.
    IF SY-SUBRC EQ 0.
      WA_OUT-KZTXT = IT_SCHKZ-PTEXT.
    ELSE.
      WRITE:/ '***Error in time wage type configuration: ',
              WA_OUT-SCHKZ.
    ENDIF.
    MODIFY IT_OUT INDEX L_IDX FROM WA_OUT TRANSPORTING KZTXT.
  ENDLOOP.


ENDFORM.                    "get_text
*&---------------------------------------------------------------------*
*&      Form  FIND_CC_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_OUT  text
*----------------------------------------------------------------------*
FORM FIND_CC_GROUP USING    PA_OUT STRUCTURE WA_OUT.
  LOOP AT IT_CC_GRP.
    IF PA_OUT-KOSTL GE IT_CC_GRP-FROM AND
       PA_OUT-KOSTL LE IT_CC_GRP-TO.
      PA_OUT-GRP1 = IT_CC_GRP-GRP1.
      PA_OUT-GRP2 = IT_CC_GRP-GRP2.
      PA_OUT-GRP3 = IT_CC_GRP-GRP3.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " FIND_CC_GROUP




*&****ALV FORMS*********************************************************

*&---------------------------------------------------------------------*
*&      Form  DBL_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM DBL_CLICK USING    P_E_COLUMN_FIELDNAME
                        P_ES_ROW_NO_ROW_ID.

ENDFORM.                    " DBL_CLICK
*&---------------------------------------------------------------------*
*&      Module  SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_ALV OUTPUT.

  IF WC_CONTAINER IS INITIAL.
    PERFORM CREATE_CONTAINER .
    PERFORM SET_ALV_LAYOUT .
    PERFORM BUILD_FIELD_CATALOG .
    PERFORM SET_SORT_TOTAL .
    PERFORM START_ALV_DISPLAY.
    PERFORM SSSIGN_EVENT.
  ELSE.
    PERFORM SET_ALV_LAYOUT .
    PERFORM BUILD_FIELD_CATALOG .
    PERFORM SET_SORT_TOTAL .
    PERFORM REFRESH_DISPLAY.
  ENDIF.

ENDMODULE.                 " SET_ALV  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER.
  CREATE OBJECT WC_CONTAINER
    EXPORTING
      CONTAINER_NAME              = 'CC_ALV'
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        TITEL = W_REPID
        TXT2  = SY-SUBRC
        TXT1  = 'The control can not be created'.
  ENDIF.

  CREATE OBJECT WC_ALV
    EXPORTING
      I_PARENT      = WC_CONTAINER
      I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ALV_LAYOUT.

  CLEAR : WS_LAYOUT, W_VARIANT.
*  WS_LAYOUT-ZEBRA           = 'X'.
  WS_LAYOUT-EDIT            = ' '.
  WS_LAYOUT-SEL_MODE        = 'A'.
  WS_LAYOUT-LANGUAGE        = SY-LANGU.
  WS_LAYOUT-CWIDTH_OPT      = 'X'.
  WS_LAYOUT-NO_MERGING      = 'X'.
  WS_LAYOUT-NO_KEYFIX       = 'X'.
  WS_LAYOUT-CTAB_FNAME      = 'CLRTB'.
  W_VARIANT-REPORT            = SY-REPID.
  W_VARIANT-USERNAME          = SY-UNAME.
* BUILD THE CELL COLOR
  PERFORM BUILD_CELL_COLOR.

ENDFORM.                    " SET_ALV_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_CELL_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_CELL_COLOR.
  DATA: CT TYPE LVC_T_SCOL.
  DATA: W_CT  LIKE LINE OF CT.
  DATA: WA_DIS TYPE S_DIS.

  LOOP AT IT_DIS INTO WA_DIS.
    W_CT-FNAME = 'TCOST'.
    W_CT-COLOR-COL = '5'.
    W_CT-COLOR-INT = '1'.
    APPEND W_CT TO CT.
    W_CT-FNAME = 'TOTCO'.
    W_CT-COLOR-COL = '5'.
    APPEND W_CT TO CT.
    W_CT-FNAME = 'TOTBE'.
    W_CT-COLOR-COL = '5'.
    APPEND W_CT TO CT.
    W_CT-FNAME = 'THOUR'.
    W_CT-COLOR-COL = '5'.
    APPEND W_CT TO CT.

    WA_DIS-CLRTB = CT.
    MODIFY IT_DIS FROM WA_DIS.
  ENDLOOP.

ENDFORM.                    " BUILD_CELL_COLOR

*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG.
  DATA: LW_ITAB TYPE SLIS_TABNAME.
  DATA: WA_FC   LIKE LINE OF IT_FIELDCAT.
  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  MOVE: SY-REPID TO W_REPID.
  LW_ITAB = 'ZSCO_LABOR1'.                                  "UD1K952793
* lw_itab = 'ZSCO_LABOR '.                                  "UD1K952793

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE '
    EXPORTING
      I_STRUCTURE_NAME   = LW_ITAB
      I_BYPASSING_BUFFER = 'X'
    CHANGING
      CT_FIELDCAT        = IT_FIELDCAT.

* BEGIN OF UD1K949915
  DELETE IT_FIELDCAT WHERE FIELDNAME = 'FROCC'
                        OR FIELDNAME = 'TOCC'
                        OR FIELDNAME = 'EMPCT'
                        OR FIELDNAME = 'KZTXT'.
* END OF UD1K949915

* SET THE FIELD ATTRIBUTE
  LOOP AT IT_FIELDCAT INTO WA_FC.
    IF WA_FC-FIELDNAME = 'SCHKZ' OR
       WA_FC-FIELDNAME = 'PERSK' OR
       WA_FC-FIELDNAME = 'PERSG'.
      DELETE IT_FIELDCAT INDEX SY-TABIX.
      CONTINUE.
    ENDIF.
    IF WA_FC-FIELDNAME = 'KOSTL' OR
       WA_FC-FIELDNAME = 'FROCC' OR
       WA_FC-FIELDNAME = 'EMPCT' OR
*       wa_fc-fieldname = 'GTEXT' or
*       wa_fc-fieldname = 'PERSK' or
       WA_FC-FIELDNAME = 'SCHKZ' OR
*       wa_fc-fieldname = 'PERSG' or
*       wa_fc-fieldname = 'KTEXT' or
       WA_FC-FIELDNAME = 'GRP1'  OR
       WA_FC-FIELDNAME = 'GRP2'  OR
       WA_FC-FIELDNAME = 'GRP3'  OR
       WA_FC-FIELDNAME = 'KZTXT' OR
       WA_FC-FIELDNAME = 'TOCC'.
      WA_FC-KEY  = 'X'.
      MODIFY IT_FIELDCAT FROM WA_FC.
    ELSE.
      WA_FC-DO_SUM  = 'X'.
      MODIFY IT_FIELDCAT FROM WA_FC.
    ENDIF.

  ENDLOOP.
ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SORT  text
*----------------------------------------------------------------------*
FORM SET_SORT_TOTAL .

  PERFORM FILL_SORT_FILED USING:  '1' 'KOSTL' 'X' ' ' 'X'.
*                                  '2' 'FROCC' 'X' ' ' ' ', "UD1K949915
*                                  '3' 'TOCC'  'X' ' ' ' ', "UD1K949915
*                                  '4' 'EMPCT' 'X' ' ' ' ', "UD1K949915
*                                  '6' 'KZTXT' 'X' ' ' ' '. "UD1K949915
ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DYNNR  text
*----------------------------------------------------------------------*
FORM START_ALV_DISPLAY.
  DATA: LW_DYNNR   LIKE   SY-DYNNR.

  W_STRUCTURE = 'ZSCO_LABOR1'.                              "UD1K952793
* w_structure = 'ZSCO_LABOR'.                               "UD1K952793
  CALL METHOD WC_ALV->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT       = WS_LAYOUT
      I_SAVE          = W_SAVE
      IS_VARIANT      = W_VARIANT
      I_DEFAULT       = SPACE
    CHANGING
      IT_FIELDCATALOG = IT_FIELDCAT
      IT_SORT         = IT_SORT
      IT_OUTTAB       = IT_DIS.

ENDFORM.                    " START_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  SSSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SSSIGN_EVENT.

ENDFORM.                    " SSSIGN_EVENT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR 'TITLE100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  SAVE_OK = OK_CODE.
  CASE SAVE_OK.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  FILL_SORT_FILED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FILL_SORT_FILED USING P_SPOS
                           P_FIELD
                           P_UP
                           P_DOWN
                           P_TOTAL.
  DATA: WA_SORT LIKE LINE OF IT_SORT.

  WA_SORT-SPOS = P_SPOS.
  WA_SORT-FIELDNAME  = P_FIELD.
  WA_SORT-UP         = P_UP.
  WA_SORT-DOWN       = P_DOWN.
  WA_SORT-SUBTOT     = P_TOTAL.
  APPEND WA_SORT TO IT_SORT.
ENDFORM.                    " FILL_SORT_FILED
*&---------------------------------------------------------------------*
*&      Form  make_test_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_TEST_DATA.
  PERFORM ENTER_TEST_DATA USING : '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 1' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 2' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Salary' 'Shift 3' '1' '200',
                                  '0000022001' ' ' ' ' 'Active'
                                  'Hourly' 'Shift 1' '1' '200'.


ENDFORM.                    " make_test_data
*&---------------------------------------------------------------------*
*&      Form  enter_test_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2644   text
*      -->P_2645   text
*      -->P_2646   text
*      -->P_2647   text
*      -->P_2648   text
*      -->P_2649   text
*      -->P_2650   text
*      -->P_2651   text
*----------------------------------------------------------------------*
FORM ENTER_TEST_DATA USING    P_1 P_2 P_3 P_4 P_5 P_6 P_7 P_8.
  WA_OUT-KOSTL = P_1.
  WA_OUT-FROCC = P_2.
  WA_OUT-TOCC = P_3.
*  wa_out-gtext = p_4.
*  wa_out-ktext = p_5.
  WA_OUT-KZTXT = P_6.
  WA_OUT-ZCUNT = P_7.
  WA_OUT-REGUL = P_8.
  APPEND WA_OUT TO IT_OUT.
ENDFORM.                    " enter_test_data
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REFRESH_DISPLAY.
  CALL METHOD WC_ALV->REFRESH_TABLE_DISPLAY.


ENDFORM.                    " REFRESH_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  MAKE_OUT_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM MAKE_OUT_FORMAT.
*  DATA: WA_OUT1 TYPE S_OUT.
*  CLEAR: WA_OUT.
*  LOOP AT IT_OUT INTO WA_OUT1.
*    PERFORM INITAL_DIS_FORMAT USING:
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Standard',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 1',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 2',
*       WA_OUT1-KOSTL 'Inpatriate'  'Salary' 'Shift 3',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Standard',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 1',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 2',
*       WA_OUT1-KOSTL 'Active'  'Salary' 'Shift 3',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Standard',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 1',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 2',
*       WA_OUT1-KOSTL 'Active'  'Hourly' 'Shift 3'.
*
*  ENDLOOP.
** SORT
*  SORT IT_DIS BY KOSTL ASCENDING
*                 FROCC ASCENDING
*                 TOCC  ASCENDING
*                 GTEXT DESCENDING
*                 KTEXT DESCENDING
*                 KZTXT ASCENDING.
* DELETE ADJACENT DUPLICATES FROM IT_DIS
*    COMPARING KOSTL FROCC TOCC GTEXT KTEXT KZTXT.
*
*ENDFORM.                    " MAKE_OUT_FORMAT
*&---------------------------------------------------------------------*
*&      Form  INITAL_DIS_FORMAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OUT_KOSTL  text
*      -->P_2756   text
*      -->P_2757   text
*      -->P_2758   text
*----------------------------------------------------------------------*
*FORM INITAL_DIS_FORMAT USING  P_1 P_2 P_3 P_4 .
*
*   WA_OUT-KOSTL = P_1.
*   WA_OUT-GTEXT = P_2.
*   WA_OUT-KTEXT = P_3.
*   WA_OUT-KZTXT = P_4.
*   APPEND WA_OUT TO IT_OUT2.
*ENDFORM.                    " INITAL_DIS_FORMAT
*&---------------------------------------------------------------------*
*&      Form  do_calculation
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DO_CALCULATION.
* cost center is one : valid on end of month.
  PERFORM GET_COST.
  IF L_LINES NE 0.
    PERFORM GET_EE_GROUP.
    PERFORM DATA_SUMMARY.

*ANDY... FIX IT LATER
    IF P_CYC = 'X'.
      PERFORM GET_SUPPORT_INFO.

      IF 1 = 2.
        PERFORM GET_SUPPORT_HOUR.
        PERFORM SUPPORT_SUMMARY.
      ENDIF.

      PERFORM APPEND_SUPPORT_INFO.

    ENDIF.

    PERFORM FINAL_SUMMARY.
  ENDIF.

ENDFORM.                    " do_calculation
*&---------------------------------------------------------------------*
*&      Form  read_old_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_OLD_DATA.
  DATA: W_DIS LIKE LINE OF IT_DIS.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_OUT
    FROM ZTCO_LABOR_COST1
    WHERE KOKRS   = P_KOKRS
     AND  GJAHR   = P_GJAHR
     AND  PERIOL IN P_PERIO
"     between p_perio-low and p_perio-high
*    and  versn   = p_versn
     AND  KOSTL   IN R_CC
     AND  ACCRL  =  P_EX_ACC.

  CHECK SY-SUBRC EQ 0.

  LOOP AT IT_OUT INTO WA_OUT.
    MOVE-CORRESPONDING WA_OUT TO W_DIS.
    APPEND W_DIS TO IT_DIS.
  ENDLOOP.
ENDFORM.                    " read_old_data
*&---------------------------------------------------------------------*
*&      Form  delete_old_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_OLD_DATA.
  DELETE FROM ZTCO_LABOR_COST1 WHERE KOKRS  = P_KOKRS
                                AND GJAHR  = P_GJAHR
                                AND PERIOL IN P_PERIO
                                AND ACCRL  =  P_EX_ACC.
*                               and perioh = p_perio-high.
ENDFORM.                    " delete_old_data
*&---------------------------------------------------------------------*
*&      Form  save_result
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_RESULT.
  DATA: LT_OUT LIKE ZTCO_LABOR_COST1 OCCURS 0 WITH HEADER LINE.
  DATA: W_DIS  LIKE LINE OF IT_DIS.
  DATA: W_OUT  LIKE LINE OF IT_OUT.
  CLEAR: W_OUT.
  LOOP AT IT_DIS INTO W_DIS.
* loop at it_out_s into w_out.
    MOVE-CORRESPONDING W_DIS TO LT_OUT.
    LT_OUT-KOKRS  = P_KOKRS.
    LT_OUT-GJAHR  = P_GJAHR.
    LT_OUT-PERIOL = P_PERIO-LOW.
** changed by Furong on 10/26/2006
    LT_OUT-ACCRL  = P_EX_ACC.
** end of change
*    lt_out-perioh = p_perio-high.
*   lt_out-versn  = p_versn.
    APPEND LT_OUT.
  ENDLOOP.
  CHECK NOT LT_OUT IS INITIAL.

* modify ztco_labor_cost1 from table lt_out.
  INSERT ZTCO_LABOR_COST1 FROM TABLE LT_OUT
         ACCEPTING DUPLICATE KEYS.

  COMMIT WORK AND WAIT.
ENDFORM.                    " save_result
*&---------------------------------------------------------------------*
*&      Form  append_support_info
*&---------------------------------------------------------------------*
FORM APPEND_SUPPORT_INFO.
  LOOP AT IT_COVP.
    CLEAR: WA_OUT.

*   SUMMARIZE THE COST AND WORKING HOURS
    PERFORM SAL_TIME_SUM USING WA_OUT
                               IT_COVP-KSTAR
                               IT_COVP-WKGBTR.

    WA_OUT-KOSTL = IT_COVP-KOSTL.

    READ TABLE IT_MHA_SUP WITH KEY KOSTL   = IT_COVP-KOSTL
                                   SRKOSTL = IT_COVP-PKOST.
    IF SY-SUBRC = 0.
      WA_OUT-FROCC  = IT_COVP-KOSTL.
      WA_OUT-TOCC   = IT_COVP-PKOST.
    ELSE.
*      read table it_mha_sup with key kostl   = it_covp-pkost
*                                     srkostl = it_covp-kostl.
      WA_OUT-TOCC   = IT_COVP-KOSTL.
      WA_OUT-FROCC  = IT_COVP-PKOST.
    ENDIF.


*     "SUM THE SUBTOTAL
    WA_OUT-TOTCO = WA_OUT-REGUL + WA_OUT-OVERT  "LABOR COST SUBTOTAL
                 + WA_OUT-BONUS + WA_OUT-ZLEAV
                 + WA_OUT-OTHCO + WA_OUT-REGUH              "UD1K952793
                 + WA_OUT-TEMPO.                            "UD1K952793
    WA_OUT-TOTBE = WA_OUT-PENSN + WA_OUT-HEALTH
                 + WA_OUT-WORKC + WA_OUT-INSUR  "BENEFIT SUBTOTAL
                 + WA_OUT-TAX   + WA_OUT-OTHBE.
    WA_OUT-THOUR = WA_OUT-TREGU + WA_OUT-TOVER  "WORK HOUR SUBTOTAL
                 + WA_OUT-TOTHR.
    WA_OUT-TCOST = WA_OUT-TOTCO + WA_OUT-TOTBE. "TOTAL OF COST/BENEFIT

    COLLECT WA_OUT INTO IT_OUT.

  ENDLOOP.

* hours---
  DATA: L_IDX LIKE SY-TABIX.
  LOOP AT IT_OUT INTO WA_OUT.
    CHECK WA_OUT-FROCC <> SPACE.
    L_IDX = SY-TABIX.
    READ TABLE IT_MHA_SUP WITH KEY KOSTL   = WA_OUT-FROCC
                                   SRKOSTL = WA_OUT-TOCC.
    WA_OUT-THOUR = IT_MHA_SUP-ANZHL.
    MODIFY IT_OUT INDEX L_IDX FROM WA_OUT TRANSPORTING THOUR.
  ENDLOOP.

ENDFORM.                    " append_support_info
*&---------------------------------------------------------------------*
*&      Form  include_co_accrual
*&---------------------------------------------------------------------*
FORM INCLUDE_CO_ACCRUAL.
  SORT IT_OUT BY KOSTL EMPCT SCHKZ.

* CO accrual
  DATA: LT_CO_ACC LIKE ZTCO_PAYACC OCCURS 0 WITH HEADER LINE.

  IF P_ACC_CO = 'X' AND P_EX_ACC = ' '.


*should be...yyyymm = S016-SPMON

    SELECT * INTO TABLE LT_CO_ACC
      FROM ZTCO_PAYACC
      WHERE KOKRS = P_KOKRS
        AND GJAHR = GV_PREV_PERIOD(4)
        AND PERID = GV_PREV_PERIOD+4(2)
        AND KOSTL IN R_CC.

    SELECT * APPENDING TABLE LT_CO_ACC
      FROM ZTCO_PAYACC
      WHERE KOKRS = P_KOKRS
        AND GJAHR = P_GJAHR
        AND PERID IN P_PERIO
        AND KOSTL IN R_CC.

    LOOP AT LT_CO_ACC.

*      read table it_out with key kostl = lt_co_acc-kostl
*                                 empct = lt_co_acc-empct
* Begin of changes  - UD1K940237
* Change the sign of Amount & HR for previous Period
      IF  GV_PREV_PERIOD(4) EQ   LT_CO_ACC-GJAHR AND
          GV_PREV_PERIOD+4(2) EQ LT_CO_ACC-PERID.
        LT_CO_ACC-ACC_AMT = LT_CO_ACC-ACC_AMT * -1.
        LT_CO_ACC-ACC_HR  = LT_CO_ACC-ACC_HR * -1.
      ENDIF.

* End of changes - UD1K940237

      PERFORM SAL_TIME_SUM USING WA_OUT
                                 LT_CO_ACC-HKONT
                                 LT_CO_ACC-ACC_AMT.
*   TIME
      IF LT_CO_ACC-LGART IN R_LGART1.
        WA_OUT-TREGU  = WA_OUT-TREGU + LT_CO_ACC-ACC_HR.
      ELSEIF LT_CO_ACC-LGART IN R_LGART2.
        WA_OUT-TOVER  = WA_OUT-TOVER + LT_CO_ACC-ACC_HR.
      ELSEIF LT_CO_ACC-LGART IN R_LGART3.
        WA_OUT-TOTHR  = WA_OUT-TOTHR + LT_CO_ACC-ACC_HR.
      ENDIF.

*     "SUM THE SUBTOTAL
      WA_OUT-TOTCO = WA_OUT-REGUL + WA_OUT-OVERT  "LABOR COST SUBTOTAL
                   + WA_OUT-BONUS + WA_OUT-ZLEAV
                   + WA_OUT-OTHCO + WA_OUT-REGUH            "UD1K952793
                   + WA_OUT-TEMPO.                          "UD1K952793
      WA_OUT-TOTBE = WA_OUT-PENSN + WA_OUT-HEALTH
                   + WA_OUT-WORKC + WA_OUT-INSUR  "BENEFIT SUBTOTAL
                   + WA_OUT-TAX   + WA_OUT-OTHBE.
      WA_OUT-THOUR = WA_OUT-TREGU + WA_OUT-TOVER  "WORK HOUR SUBTOTAL
                   + WA_OUT-TOTHR.
      WA_OUT-TCOST = WA_OUT-TOTCO + WA_OUT-TOTBE. "TOTAL OF COST/BENEFIT


      WA_OUT-KOSTL  = LT_CO_ACC-KOSTL.
      WA_OUT-EMPCT  = LT_CO_ACC-EMPCT.

      COLLECT WA_OUT INTO IT_OUT. CLEAR: WA_OUT.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " include_co_accrual

*** UD1K940478 ( START )
*** by IG.MOON 5/5/2007
FORM GATHER_ROW_DATA.

  CLEAR IT_COVP.
  REFRESH IT_COVP.

  RANGES LR_OBJNR FOR  CSKS-OBJNR.

  DATA: BEGIN OF I_OBJNR OCCURS 0,
          OBJNR LIKE CSKS-OBJNR,
          KOSTL LIKE CSKS-KOSTL,
        END OF I_OBJNR.

  SELECT DISTINCT OBJNR KOSTL INTO TABLE I_OBJNR
    FROM  CSKS
    WHERE KOSTL IN R_CC .

  LOOP AT I_OBJNR .
    LR_OBJNR-SIGN   = 'I'.
    LR_OBJNR-LOW    = I_OBJNR-OBJNR.
    LR_OBJNR-OPTION = 'EQ'.
    APPEND LR_OBJNR.
  ENDLOOP.

  SELECT OBJNR  PAROB1
         KSTAR  MEINB
         SUM( WKGBTR ) AS WKGBTR SUM( MBGBTR ) AS MBGBTR
    INTO CORRESPONDING FIELDS OF TABLE IT_COVP
    FROM  COVP
    WHERE OBJNR IN LR_OBJNR  AND
          KSTAR  IN R_CE     AND
          GJAHR  EQ P_GJAHR  AND
          PERIO  IN P_PERIO  AND
          REFBT  = 'K'       AND
          REFBN  = 'SUPPORT' AND
          KOKRS  = P_KOKRS   AND
          VRGNG  = 'KAMV'    AND
        ( STFLG  NE 'X' AND STOKZ  NE 'X' )
     GROUP BY OBJNR PAROB1 KSTAR MEINB.

  SORT I_OBJNR BY OBJNR.
  LOOP AT IT_COVP .
    READ TABLE I_OBJNR WITH KEY OBJNR = IT_COVP-OBJNR
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_COVP-KOSTL = I_OBJNR-KOSTL .
      MODIFY IT_COVP .
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GATHER_ROW_DATA
*** UD1K940478 ( END )
*&---------------------------------------------------------------------*
*&      Form  cleanup_old_data
*&---------------------------------------------------------------------*
*       cleanup older data (more than one year)
*----------------------------------------------------------------------*
FORM CLEANUP_OLD_DATA.
  DATA: L_YEAR TYPE GJAHR,
        L_PER  TYPE PERBL.

  L_YEAR = SY-DATUM(4).
  L_YEAR = L_YEAR - 1.

  L_PER  = SY-DATUM+4(2).

  DELETE FROM ZTCO_LABOR_COST1 WHERE KOKRS = P_KOKRS
                                 AND ( GJAHR < L_YEAR OR
                              ( GJAHR = L_YEAR AND PERIOL <= L_PER ) ).

ENDFORM.                    " cleanup_old_data
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSIS
*&---------------------------------------------------------------------*
FORM ARCHIVE_READ_BSIS USING   P_FP_DATE TYPE SY-DATUM
                               P_LP_DATE TYPE SY-DATUM.
  DATA: LT_BSIS  LIKE BSIS OCCURS 0 WITH HEADER LINE,
        LT_YBKPF TYPE BKPF OCCURS 10,
        LT_YBSEG TYPE BSEG OCCURS 10,
        LT_SELECTIONS TYPE RSDS_RANGE OCCURS 10.
  DATA: L_ERRORS      TYPE BOOLE.

  L_ERRORS = 'X'.
  PERFORM SET_RANGE_BUDAT USING   P_FP_DATE
                                  P_LP_DATE.

  PERFORM SET_SEL_CONDITION_FOR_BSIS CHANGING LT_SELECTIONS.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      I_SELECTIONS       = LT_SELECTIONS
    TABLES
      E_BKPF             = LT_YBKPF
      E_BSEG             = LT_YBSEG
      E_BSIS             = LT_BSIS
*     E_BSAS             = LT_BSAS
*     E_BSAD             = LT_BSAD
*     E_BSAK             = LT_BSAK
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      NO_INFOSTRUC_FOUND = 1
      SELECTIONS_ERROR   = 2
      OTHERS             = 3.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_BSIS.
      MOVE-CORRESPONDING LT_BSIS TO IT_BSIS.
      APPEND IT_BSIS.
      CLEAR: IT_BSIS,
             LT_BSIS.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ARCHIVE_READ_BSIS
*&---------------------------------------------------------------------*
*&      Form  SET_RANGE_BUDAT
*&---------------------------------------------------------------------*
FORM SET_RANGE_BUDAT  USING   P_FP_DATE TYPE SY-DATUM
                              P_LP_DATE TYPE SY-DATUM.
  DATA: LR_BUDAT TYPE RANGE OF BUDAT WITH HEADER LINE.

  LR_BUDAT-SIGN = 'I'.
  LR_BUDAT-OPTION = 'BT'.
  LR_BUDAT-LOW = P_FP_DATE.
  LR_BUDAT-HIGH = P_LP_DATE.
  APPEND LR_BUDAT.
  GR_BUDAT[] = LR_BUDAT[].
  CLEAR: LR_BUDAT.
ENDFORM.                    " SET_RANGE_BUDAT
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSIS
*&---------------------------------------------------------------------*
FORM SET_SEL_CONDITION_FOR_BSIS
                          CHANGING  PT_SELECTIONS TYPE RSDS_TRANGE.

  PERFORM SET_SELECT_FIELD_OPTION USING    'BKPF'
                                           'BUKRS'
                                           'I'
                                           'EQ'
                                           'P_KOKRS'
                                           'P'
                                  CHANGING PT_SELECTIONS.

  PERFORM SET_SELECT_FIELD_OPTION USING   'BKPF'
                                          'GJAHR'
                                          'I'
                                          'EQ'
                                          'P_GJAHR'
                                          'P'
                                 CHANGING PT_SELECTIONS.

  PERFORM SET_SELECT_FIELD_OPTION USING    'BKPF'
                                           'BUDAT'
                                           ' '
                                           ' '
                                           'GR_BUDAT'
                                           'S'
                                  CHANGING PT_SELECTIONS.


  PERFORM SET_SELECT_FIELD_OPTION USING    'BSEG'
                                           'HKONT'
                                           ' '
                                           ' '
                                           'R_CE'
                                           'S'
                                  CHANGING PT_SELECTIONS.
ENDFORM.                    " SET_SEL_CONDITION_FOR_BSIS
*&---------------------------------------------------------------------*
*&      Form  SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
FORM SET_SELECT_FIELD_OPTION
                         USING    P_TABLENAME
                                  P_FIELDNAME
                                  P_SIGN
                                  P_OPTION
                                  P_SELFIELD
                                  P_PARAM
                         CHANGING PT_SELECTIONS TYPE RSDS_TRANGE.

  DATA:  LS_SELOPT    LIKE RSDSSELOPT,
         LT_SELOPT    TYPE RSDS_SELOPT_T,
         LS_FRANGE    TYPE RSDS_FRANGE,
         LT_FRANGE    TYPE RSDS_FRANGE_T,
         LS_TRANGE    TYPE RSDS_RANGE,
         LT_TRANGE    TYPE RSDS_TRANGE.

  DATA: L_NAME TYPE STRING.

  FIELD-SYMBOLS: <FS_ITAB>  TYPE ANY TABLE,
                 <FS_WA>    TYPE ANY,
                 <FS_VALUE> TYPE ANY.

  LS_TRANGE-TABLENAME = P_TABLENAME.

  CASE P_PARAM.
    WHEN 'P'.
      L_NAME = P_SELFIELD.
      ASSIGN (L_NAME) TO <FS_VALUE>.

      IF NOT <FS_VALUE> IS INITIAL.
        REFRESH: LT_SELOPT.
        LS_FRANGE-FIELDNAME = P_FIELDNAME.
        LS_SELOPT-SIGN = P_SIGN.
        LS_SELOPT-OPTION = P_OPTION.
        LS_SELOPT-LOW = <FS_VALUE>.
        APPEND LS_SELOPT TO LT_SELOPT.

        LS_FRANGE-SELOPT_T = LT_SELOPT[].
        APPEND LS_FRANGE TO LT_FRANGE.
      ENDIF.

    WHEN 'S'.
      CONCATENATE P_SELFIELD
                  '[]'
             INTO L_NAME.
      ASSIGN (L_NAME) TO <FS_ITAB>.

      IF NOT <FS_ITAB> IS INITIAL.
        REFRESH: LT_SELOPT.
        LS_FRANGE-FIELDNAME = P_FIELDNAME.
        LOOP AT <FS_ITAB> ASSIGNING <FS_WA>.
          MOVE-CORRESPONDING <FS_WA> TO LS_SELOPT.
*          LS_SELOPT = <FS_WA>.
          APPEND LS_SELOPT TO LT_SELOPT.
        ENDLOOP.
        LS_FRANGE-SELOPT_T = LT_SELOPT[].
        APPEND LS_FRANGE TO LT_FRANGE.
      ENDIF.
  ENDCASE.

  IF NOT LT_FRANGE[] IS INITIAL .
    LS_TRANGE-FRANGE_T = LT_FRANGE[].
    APPEND LS_TRANGE TO LT_TRANGE.
  ENDIF.

  APPEND LINES OF LT_TRANGE TO PT_SELECTIONS.
ENDFORM.                    " SET_SELECT_FIELD_OPTION
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_BSAS
*&---------------------------------------------------------------------*
FORM ARCHIVE_READ_BSAS USING   P_FP_DATE TYPE SY-DATUM
                               P_LP_DATE TYPE SY-DATUM.
  DATA: LT_BSAS  LIKE BSAS OCCURS 0 WITH HEADER LINE,
        LT_YBKPF TYPE BKPF OCCURS 10,
        LT_YBSEG TYPE BSEG OCCURS 10,
        LT_SELECTIONS TYPE RSDS_RANGE OCCURS 10.
  DATA: L_ERRORS      TYPE BOOLE.

  L_ERRORS = 'X'.
  PERFORM SET_RANGE_BUDAT USING   P_FP_DATE
                                  P_LP_DATE.

  PERFORM SET_SEL_CONDITION_FOR_BSAS CHANGING LT_SELECTIONS.

  CALL FUNCTION 'FI_DOCUMENT_ARCH_AS_ITEMS_READ'
    EXPORTING
      I_SELECTIONS       = LT_SELECTIONS
    TABLES
      E_BKPF             = LT_YBKPF
      E_BSEG             = LT_YBSEG
*     E_BSIS             = LT_BSIS
      E_BSAS             = LT_BSAS
*     E_BSAD             = LT_BSAD
*     E_BSAK             = LT_BSAK
*     E_BSIP             =
*     E_BSIM             =
*     I_ARCH_SEL         =
*     E_FAGLBSAS         =
*     E_FAGLBSIS         =
*     E_BSEG_ADD         =
    EXCEPTIONS
      NO_INFOSTRUC_FOUND = 1
      SELECTIONS_ERROR   = 2
      OTHERS             = 3.
  IF SY-SUBRC EQ 0.
    LOOP AT LT_BSAS.
      MOVE-CORRESPONDING LT_BSAS TO IT_BSIS.
      APPEND IT_BSIS.
      CLEAR: IT_BSIS,
             LT_BSAS.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " ARCHIVE_READ_BSAS
*&---------------------------------------------------------------------*
*&      Form  SET_SEL_CONDITION_FOR_BSAS
*&---------------------------------------------------------------------*
FORM SET_SEL_CONDITION_FOR_BSAS
                          CHANGING  PT_SELECTIONS TYPE RSDS_TRANGE.

  PERFORM SET_SELECT_FIELD_OPTION USING    'BKPF'
                                           'BUKRS'
                                           'I'
                                           'EQ'
                                           'P_KOKRS'
                                           'P'
                                  CHANGING PT_SELECTIONS.

  PERFORM SET_SELECT_FIELD_OPTION USING   'BKPF'
                                          'GJAHR'
                                          'I'
                                          'EQ'
                                          'P_GJAHR'
                                          'P'
                                 CHANGING PT_SELECTIONS.

  PERFORM SET_SELECT_FIELD_OPTION USING    'BKPF'
                                           'BUDAT'
                                           ' '
                                           ' '
                                           'GR_BUDAT'
                                           'S'
                                  CHANGING PT_SELECTIONS.


  PERFORM SET_SELECT_FIELD_OPTION USING    'BSEG'
                                           'HKONT'
                                           ' '
                                           ' '
                                           'R_CE'
                                           'S'
                                  CHANGING PT_SELECTIONS.
ENDFORM.                    " SET_SEL_CONDITION_FOR_BSAS
