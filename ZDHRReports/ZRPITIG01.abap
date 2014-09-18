*----------------------------------------------------------------------*
*   INCLUDE RPITIG01 : Global data for report RPITIG00                 *
*----------------------------------------------------------------------*
* 4.6C (LCP)
* OG 23092002 Note 565658: Do not read infotypes 2001...2003
* QNUL9CK007324 28122000 Note 217573

*-----Constants
CONSTANTS: TRUE(1) TYPE C VALUE '1',   "Boolean TRUE
           FALSE LIKE TRUE VALUE '0'.  "Boolean FALSE
RP-LOWDATE-HIGHDATE.    "LOW-DATE = '18000101', HIGH-DATE = '99991231'
RP-DEF-TIME-PERIOD.     "Define RPTIME_PERIOD as '01' (monthly)
*-----Infotypes
INFOTYPES: 0000,                       "Events
           0001,                       "Org. Assignment
           0002,                       "Personal Data
           0003,                       "Payroll Status
           0007,                       "Work Schedule
           0008,                       "Basic Pay
           0041,                       "Date Specifications
           0052,                       "Wage Maintenance
           0230,                       "Supp. to P0008  PSG
           0237,
           0304.                       "Basic Pay - additional info.
*           2001,                       "Absences       "N565658
*           2002,                       "Attendances    "N565658
*           2003.                       "Substitutions  "N565658

*-----Tables
TABLES: DD03L,                         "Table - Fields
        PCL1,                          "HR Cluster 1
        PCL2,                          "HR Cluster 2
        PERNR,       "Standard Selections for HR Master Data Reporting
        PME01,       "Field String for Feature HDATE
        PME04,       "Field String for Feature in T588M
        PME07,       "Field String for Feature: Ind. Val. & Wage Types
        PME50,       "Field String for Feature in T549B (payscale recl.)
        PME42,       "Field String for Feature DFINF     "WRH PH0K000464
        PTPRG,       "Transfer String for Program SAPLRPTM
        SSCRFIELDS,  "Fields on report selection screen
*       t001,                          "Company codes    "WRH ALRK044773
        T001P,                         "Personnel Areas/Subareas
        T500P,                         "Personnel Areas
        T503,                          "Person Groups / Subgroups
        T510,                          "Pay Scale Groups
        T510A,                         "Pay Scale Types
        T510D,                         "Dynamic pay scale increase
*       t510f,       "Ass. Pay Scale->Time Unit, Curr.   "WRH ALRK044773
        T510G,                         "Pay Scale Areas
        T510M,                         "Grandfathered pay scales
        T510N,                         "Pay scales for annual salaries
        T510R,                         "Pay scale increase (TIG)
        T511,                          "Wage Types
        T512T,                         "Wage Type Texts
        T513S,                         "Job Title
        T528B,                         "Workplace Catalog
        T529A,                         "Personnel Event
        T529T,                         "Personnel Event Texts
        T530,                          "Reasons for Events
        T530E,                         "Reasons for Changes
        T530F,                         "Reasons for Changes Texts
        T530T,                         "Event Reason Texts
        T533,                          "Leave Types
        T533T,                         "Leave Type Texts
        T539A,                         "Default Wage Types for Basic Pay
        T548Y,                         "Date Types
        T548T,                         "Date Type Texts
        T549A,                         "Payroll Subunits
        T549Q,                         "Payroll Periods
        T549R,                         "Period Modifiers
        T550A,                         "Daily Work Schedules
        T550P,                         "Work Break Rules
        T551C,                         "Period Work Schedule Valuation
        T554S,                         "Absence and Attendance Types
        T555B,                         "Time Type Designations
        T588C,                         "Infotype Menus/Info Groups
        T588D,                         "Infogroups for Events
        T588M,                         "Infotype dynpro control
        T591A,                         "Subtype Characteristics
        T591S.                         "Subtype Texts

*-----Common INCLUDES
INCLUDE RPCLST00.
INCLUDE RPC2B200.                      "Data definition for Cluster B2
INCLUDE RPPPXD00.    "R/3 data descriptions for PCL1- and PCL2-buffer
INCLUDE RPPPXD10.                      "R/3 Common-part pcl1(2)-buffer
INCLUDE MPZDAT02.   "Table definitions for RP_BUILD_PSP
INCLUDE RPCFPABS.
INCLUDE MPPDAT00.
INCLUDE RPBDYN10.                      "Dynamic dynpro selection
INCLUDE RPBDYN00.                      "Dynamic dynpro selection
INCLUDE RPUMKC00.    "Routines for calling features and returning data
INCLUDE RPPPXM00.    "Routines for exporting/importing PCL1/PCL2

*----------------------------------------------------------------------*
* Common type- and data definitions                                    *
*----------------------------------------------------------------------*

*-----Variables for rounding the amount
DATA: PACK TYPE P, PACK2 TYPE P, POUT TYPE P, PHILF TYPE P.

DATA: HIRE_DATE        LIKE SY-DATUM,  "First date of employment
      FIRE_DATE        LIKE SY-DATUM,  "Layoff date
      IMPORT_B2_BEGDA  LIKE SY-DATUM,  "Begin date of Cluster B2 import
      IMPORT_B2_ENDDA  LIKE SY-DATUM,  "End date of Cluster B2 import
      RECLASS_DATE     LIKE SY-DATUM,  "Date of reclassif. to next G/L
      PSGRPLVL_BEGDA   LIKE SY-DATUM,  "Begin date of p.s. group/level
      REPNAME          LIKE SY-REPID,  "Report name for BI name check
      RETCD            LIKE SY-SUBRC,  "Return code
      SHOW_PERID       TYPE I,         "Show personnel ID number ?
      TYPE_PS_RECLASS  LIKE RPIXXXXX-PSR_TYPE, "Payscale reclass. type
      OPEN_SESSION_ONCE(1) VALUE '0',  "Switch to open BI-Map only once
      GEN_MODE(1),                     "Generation mode
      EMPLOYEE_AGE     LIKE T510R-SPALT,"Age of employee
      YEARS_OF_SENIORITY LIKE PME50-DAUER, "Number of service years
      PERNR_COUNT      TYPE P,         "Number of personnel numbers
      NO_SCREENS       TYPE P,         "Number of created screens
      NO_REJECTED TYPE P.    "Number of employees rejected due to error

TYPES: LINE_COUNT(2) TYPE P.
DATA: P0008_LINE_COUNT TYPE LINE_COUNT.
DATA: STR_0008 LIKE P0008-INFTY VALUE '0008'.            "WRH ALRK044773
DATA: STR_0052 LIKE P0052-INFTY VALUE '0052'.            "WRH AHRK028021
data: number_of_wagetypes_0008 type i value 20.                "N217573

*-----Data for function module RP_EDIT_NAME
DATA: $EDIT-NAME(40),
      $RET-CODE LIKE SY-SUBRC,
      FORMAT   TYPE P0002-KNZNM, "name format
      $LENGTH(2) TYPE N.

*-----Data for job title
DATA: JOB-TITLE LIKE T513S-STLTX,
      INITIAL-P0001-STELL LIKE P0001-STELL.

*----------------------------------------------------------------------*
* Common structure- and table definitions                              *
*----------------------------------------------------------------------*

*-----Internal structure for payroll periods
TYPES: BEGIN OF I549Q_STRUC,
         T549Q LIKE T549Q,
         CLUSTER_READ TYPE C,
       END OF I549Q_STRUC.

*-----Internal payroll period table
DATA: I549Q_TAB TYPE I549Q_STRUC OCCURS 10.

*-----Internal hire- and layoff date table
DATA: PHIFI_TAB LIKE PHIFI OCCURS 5.
DATA: ENTRY_DATE_TAB   LIKE HIDA OCCURS 5,               "WRH AHRK011379
      LEAVING_DATE_TAB LIKE HIDA OCCURS 5.               "WRH AHRK011379

*-----Internal error message table and work area
DATA: ERR_TAB LIKE HRERROR OCCURS 10.

*-----Internal work schedule table
DATA: IPSP_TAB LIKE PC2BA OCCURS 366.

*-----Internal table IVS_TAB to keep contents of cluster B2 table VS
DATA: IVS_TAB LIKE PC2BH OCCURS 60 WITH HEADER LINE.

*-----Internal table ICVS_TAB to keep contents of cluster B2 table CVS
DATA: ICVS_TAB LIKE PC2BI OCCURS 10 WITH HEADER LINE.

*======= Table Control related structure- and table definitions =======*
*-----Internal structure for Table Control column headings and output
*     control
TYPES: BEGIN OF FIELDNAMES_STRUC,
         TEXT(30),
         TABNAME(10),
         FIELDNAME(10),
         TYP(1),
       END OF FIELDNAMES_STRUC.

*-----Internal table to keep Table Control column headings and output
*     control information
DATA: FIELDNAMES_TAB TYPE FIELDNAMES_STRUC OCCURS 3.

*-----Internal work area to keep Table Control column headings and
*     output control
DATA: FIELDNAMES_WA TYPE FIELDNAMES_STRUC.

*-----Internal structure to keep displayed Table Control data
TYPES: TC_FIELD(30),
       check_field(1),                                    "DB AHRK041998
       BEGIN OF TC_DATA_STRUC,
         FIELD1  TYPE TC_FIELD,
         FIELD2  TYPE TC_FIELD,
         FIELD3  TYPE TC_FIELD,
         FIELD4  TYPE TC_FIELD,
         FIELD5  TYPE TC_FIELD,
         FIELD6  TYPE TC_FIELD,
         FIELD7  TYPE TC_FIELD,
         FIELD8  TYPE TC_FIELD,
         FIELD9  TYPE TC_FIELD,
         FIELD10 TYPE TC_FIELD,
         FIELD11 TYPE TC_FIELD,
         FIELD12 TYPE TC_FIELD,
         FIELD13 TYPE TC_FIELD,
         FIELD14 TYPE TC_FIELD,
         FIELD15 TYPE TC_FIELD,
         FIELD16 TYPE TC_FIELD,
         FIELD17 TYPE TC_FIELD,
         FIELD18 TYPE TC_FIELD,
         FIELD19 TYPE TC_FIELD,
         FIELD20 TYPE TC_FIELD,
         FIELD21 TYPE TC_FIELD,                          "WRH PH4K002071
         CHECK   TYPE CHECK_FIELD,                        "DB AHRK041998
       END OF TC_DATA_STRUC.

*-----Internal table to keep displayed Table Control dat
DATA: P0008_TC_DATA_TAB TYPE TC_DATA_STRUC OCCURS 10.

*-----Internal table to keep selected Table Control data
DATA: SEL_TC_DATA_TAB TYPE TC_DATA_STRUC OCCURS 10.

*-----Internal structure to keep payscale reclassification results
TYPES: BEGIN OF P0008_RESULT_STRUC,
        PERNR     LIKE PERNR-PERNR,    "Personnel number
        SUBTY     LIKE P0008-SUBTY,    "P0008 subtype
        PERID     LIKE P0002-PERID,    "Personnel identification
        NAME      LIKE $EDIT-NAME,     "Employee name
        MOLGA     LIKE T500P-MOLGA,    "Country grouping
        OLD_TYPE  LIKE P0008-TRFAR,    "Pay scale type
        OLD_AREA  LIKE P0008-TRFGB,    "Pay scale area
        OLD_GROUP LIKE P0008-TRFGR,    "Pay scale group
        OLD_PSIND LIKE T503-TRFKZ,     "Pay scale indicator
        OLD_LEVEL LIKE P0008-TRFST,    "Pay scale level
        OLD_BEGDA(10),                 "Begin date of current P0008
        OLD_ENDDA(10),                 "End date of current P0008
        TYPE_RECL LIKE TYPE_PS_RECLASS,"Type of reclassification
        SPALT     LIKE T510R-SPALT,    "Age limit for reclassification
        DAUER     LIKE T510R-DAUER,    "Group membership limit
        ANZHL     LIKE T510R-ANZHL,    "Time-In-Grade limit
        NEW_GROUP LIKE T510R-TRFFG,    "New pay scale group
        NEW_LEVEL LIKE T510R-TRFFS,    "New pay scale level
        XFER_DATE(10),                 "Reclassification date
        NEW_ENDDA(10),                 "End date for new P0008
        EMPL_AGE  LIKE T510R-SPALT,    "Age of employee
        SENIORITY LIKE PME50-DAUER,    "Group membership duration
        FUTURE_REC(1),                 "Indicator for future basic pay
      END OF P0008_RESULT_STRUC.

*-----Internal table to keep payscale reclassification results
DATA: P0008_RESULT_TAB TYPE P0008_RESULT_STRUC OCCURS 10.

*================= Basic Pay related data structures ==================*

DATA: PP0008_TAB LIKE P0008 OCCURS 10.
*DATA: pp0008_wa LIKE p0008.                            " WRH ALRK054526

*-----Internal structure to keep proposed wage types from table T539A
TYPES: BEGIN OF PROPOSED_LGART_STRUC,
         SEQNP LIKE T539A-SEQNP,
         LGART LIKE T539A-LGART,
         LGMOD LIKE T539A-LGMOD,
       END OF PROPOSED_LGART_STRUC.

*-----Internal table to keep proposed wage types from table T539A
DATA: PROPOSED_LGART_TAB TYPE PROPOSED_LGART_STRUC OCCURS 20.

*-----Internal structure to keep wage types of Basic Pay record
TYPES: BEGIN OF WAGE_TYPE_STRUC,
         SEQNR(3),                     "Seq. number to order wage types
         LGART LIKE P0008-LGA01,       "Wage type
         LGMOD LIKE T539A-LGMOD,       "Wage type mode (T539A)
         LGTXT LIKE T512T-LGTXT,       "Wage type text from T512T
         OPKEN LIKE P0008-OPK01,       "Operation indicator from T511
         BETRG LIKE P0008-BET01,       "Amount of wage type
         INDBW LIKE Q0008-INDBW,       "Indicator for indirect eval.
         ANZHL LIKE P0008-ANZ01,
*        eitxt like q0008-eitxt,       "External units   "WRH AHRK028021
*        ein   like t538t-zeinh,       "Internal units   "WRH AHRK028021
*        zeinh like p0008-ein01,                         "WRH AHRK028021
         EITXT LIKE T538T-ETEXT,       "External units   "WRH AHRK028021
         EIN   LIKE P0008-EIN01,       "Internal units   "WRH AHRK028021
         ZEINH LIKE T511-ZEINH,                          "WRH AHRK028021
         MODNA LIKE T511-MODNA,        "Modul name for indirect eval.
         MOD01 LIKE T511-MOD01,                          "WRH AHRK028021
         MOD02 LIKE T511-MOD02,        "Flag to permit wage type change
         KOMBI LIKE T511-KOMBI,
         BTMIN LIKE T511-BTMIN,        "Minimum wage type amount
         BTMAX LIKE T511-BTMAX,        "Maximum wage type amount
         ANMIN LIKE T511-ANMIN,        "Min. amt. wage type number field
         ANMAX LIKE T511-ANMAX,        "Max. amt. wage type number field
         ADSUM(1),
         INDIR TYPE C,
       END OF WAGE_TYPE_STRUC.

*-----Internal table to store indirectly evaluated wage types (T511)
DATA: INDIR_LGART_TAB TYPE WAGE_TYPE_STRUC OCCURS 10.

*-----Internal table to keep old Basic Pay wage types
DATA: OLD_WAGE_TYPE_TAB TYPE WAGE_TYPE_STRUC OCCURS 0.

*-----Internal table to keep new Basic Pay wage types
DATA: NEW_WAGE_TYPE_TAB TYPE WAGE_TYPE_STRUC OCCURS 0.

*-----Internal table to keep Basic Pay wage type line
TYPES: BEGIN OF WAGE_TYPE_LINE,
         LGART LIKE P0008-LGA01,
         BETRG LIKE P0008-BET01,
         ANZHL LIKE P0008-ANZ01,
         EIN   LIKE P0008-EIN01,
         OPKEN LIKE P0008-OPK01,
       END OF WAGE_TYPE_LINE.
TYPES: BEGIN OF WAGE_TYPE_INDBW_TYPE,                    "WRH PH4K002071
         INDBW LIKE P0008-IND01,                         "WRH PH4K002071
       END   OF WAGE_TYPE_INDBW_TYPE.                    "WRH PH4K002071
*-----Internal structure to keep Basic Pay record
TYPES: BEGIN OF BASIC_PAY_STRUC,
         BEGDA LIKE P0008-BEGDA,
         ENDDA LIKE P0008-ENDDA,
         CPIND LIKE P0008-CPIND,                         "WRH AHRK023507
         TRFAR LIKE P0008-TRFAR,
         TRFGB LIKE P0008-TRFGB,
         TRFGR LIKE P0008-TRFGR,
         TRFST LIKE P0008-TRFST,
         STVOR LIKE P0008-STVOR,
         WAERS LIKE P0008-WAERS,                         "WRH AHRK041998
       END OF BASIC_PAY_STRUC.

*-----Internal work area to keep current Basic Pay record
DATA: CURRENT_BASIC_PAY_WA TYPE BASIC_PAY_STRUC.

*-----Internal work area to keep old Basic Pay record
DATA: OLD_BASIC_PAY_WA TYPE BASIC_PAY_STRUC.

*-----Internal table to process evaluated wage types
DATA: EVALUATED_LGART_TAB LIKE PBWLA OCCURS 10.

*-----Internal table for batch-input session
DATA: BATCH_DATA_TAB LIKE BDCDATA OCCURS 20.

*-----Internal table to store the infogroup
DATA: INFOGROUP_TAB LIKE PITGR OCCURS 20.

*============ Data definitions for RP_OPTIONS_INTO_STRING =============*
FIELD-SYMBOLS: <SORT1>,
               <SORT2>,
               <SORT3>,
               <SORT4>,
               <SORT5>,
               <SORT6>.
DATA: BEGIN OF SEL_TAB OCCURS 6.
        INCLUDE STRUCTURE PNPSTRINGT.
DATA  END  OF SEL_TAB.
DATA: BEGIN OF NAMETAB OCCURS 60.
        INCLUDE STRUCTURE DNTAB.
DATA: END OF NAMETAB.
DATA: BEGIN OF FDTAB OCCURS 60,
        FIELDNAME LIKE DNTAB-FIELDNAME,
        DBLENGTH  TYPE P,
        FIELDTEXT LIKE DNTAB-FIELDTEXT,
      END OF FDTAB.
DATA: BEGIN OF SORTFIELDTAB OCCURS 6,
        TEXTE(60),
        TABNAME(10),
        FIELDNAME(10),
        FIELDTEXT(60),
        KEY(1),
     END OF SORTFIELDTAB.
DATA: BEGIN OF SORTDATATAB OCCURS 0,
        PERNR LIKE PERNR-PERNR,
        FIELD1(10),
        FIELD2(10),
        FIELD3(10),
        FIELD4(10),
        FIELD5(10),
        FIELD6(10),
      END OF SORTDATATAB.
DATA: MAX_SORT TYPE I VALUE 6.

*============ Data definitions for HELP_VALUES_GET_WITH_TABLE =========*
DATA: BEGIN OF IHELP_FIELDS OCCURS 20.
        INCLUDE STRUCTURE HELP_VALUE.
DATA: END OF IHELP_FIELDS.
DATA: BEGIN OF HELPTAB OCCURS 100,
         FELD(40),
      END OF HELPTAB.

*============ Data definitions for DYNP_VALUES_UPDATE =================*
DATA: BEGIN OF DYNPRO_TAB OCCURS 2.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPRO_TAB.

*============ Data definitions for SELECT_OPTIONS_RESTRICT ============*
TYPE-POOLS SSCR.
DATA RESTRICT TYPE SSCR_RESTRICT.
DATA OPT_LIST TYPE SSCR_OPT_LIST.
DATA ASSOC    TYPE SSCR_ASS.

*============ Data definitions for Customer-Exit ======================*
DATA: BEGIN OF VAR_ARG_USER_EXIT OCCURS 5.
        INCLUDE STRUCTURE RPITIGVA.
DATA: END OF VAR_ARG_USER_EXIT.
