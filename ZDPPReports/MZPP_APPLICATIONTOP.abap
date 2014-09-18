*&---------------------------------------------------------------------*
*& Include MZPP_APPLICATIONTOP                                         *
*&---------------------------------------------------------------------*
TYPE-POOLS: CXTAB,
            VRM.
*----------------------------------------------------------------------
* TABLES DECLARATION
*----------------------------------------------------------------------
TABLES: CUXREF     ,  "Object Dependency Cross References
        ZTPP_WOSUM ,  "ERP_WO QTY SUMMARY
        ZVPP_MODEL ,     " Vehicle Model master
        ZTBM_ABXOPVDT,   " Opiton Item & Value
        ZTBM_OPTION_ITEM,  " OPTION ITEM LIST
        CRTX,            " Production Resource
        ZTPP_CHANGE,  "Spec & Line Back Change
        ZTPP_PROCESS,  "Vehicle plant Process
        ZTPP_ALCLOG2,  " ALC Log Table for Time-Based
        CABN,    "Characteristic
        CAWN,    "Characteristic values
        CUVTLN,  "Line object of variant table
        CUVTAB,  "Variant table basic data
        CUVTAB_VALC,  "CHAR format values
        CUVTAB_VALN,  "NON-CHAR format values
        CUVTAB_TX,  "Descriptions for Variant Table
        ZTPP_SPEC,  "Spec Table
        MARA,  "General Material Data
        AUSP,  "Characteristic Values
        EQUI,  "Equipment master data
        ZSPP_VM_VALUE,  "Vehicle Value
        ZTPP_NATION_DEF,  "Nation Define
        ZTPP_COMMON_VALS,   "
        ZTPPVN1,  "NATION 1 (Nation Code + Port )
        ZSPPVN1,  "Nation Code + (PP-->ALC)
        ZTBM_ABXPCLDT,  "HPC HEAD COLUMN
        ZTBM_ABXPLIDT,  "HPC HEAD LINE COLUMN
        ZTBM_ABXPLCDT,  "HPC LINE BY CAR
        ZTBM_ABXPLBDT,  "HPC LINE DESCRIPTION
        ZTPP_DAY_SUM,   "Product Quantity per Working Day
*      "Sequence Parts Summary (Bucket: 2 Hourly, Horz.: 3 Days)
        ZTPP_SEQ_SUM01,
*      "Sequence Parts Summary (Bucket: Daily , Horz.: 21 days)
        ZTPP_SEQ_SUM02,
        ZVPP_RP2 ,  " Rate Routing
        ZVPP_BOM ,  " Manufacture BOM
        MAST,       " Material to BOM Link
        MKAL.       " Production Version

DATA: IMAGE_CONTROL TYPE REF TO  CL_GUI_PICTURE,
      CUSTOM_CONTAINER TYPE REF TO  CL_GUI_CUSTOM_CONTAINER.
DATA: START_IMAGE_RESIZE LIKE SMENSAPNEW-CUSTOMIZED.
DATA: IMAGE_CONTROL_CREATED TYPE C.

DATA: BEGIN OF IT_ALC         OCCURS 0.
        INCLUDE STRUCTURE     CUKB    .
DATA:   MODEL(3)              TYPE C  ,
        KNKTX                 LIKE CUKBT-KNKTX,
        CODE(3)               TYPE C  ,
        RP(2)                 TYPE N  ,
        TYPE_ALC              TYPE C  ,
        CHAR_ALC              LIKE CABN-ATNAM,
      END OF IT_ALC .

DATA: BEGIN OF IT_MODEL       OCCURS 0,
        MODL                  TYPE ZPP_MODEL,
      END OF IT_MODEL.


*----------------------------------------------------------------------
* WORKING-AREA DECLARATION (GLOBAL)
*----------------------------------------------------------------------
DATA: WA_PRNT_EA              TYPE I         VALUE 1,
      WA_PRNT_TITLE(40)       TYPE C         ,
      WA_MODEL(4)             TYPE C         ,
      WA_PLANT(4)             TYPE C         ,
      NAME                    TYPE VRM_ID    ,  " Field Name..
**    P_219_xxx
      P_COLUMN01(03)          TYPE C         ,
      P_COLUMN02(03)          TYPE C         ,
      P_COLUMN03(03)          TYPE C         ,
      P_COLUMN04(03)          TYPE C         ,
      P_COLUMN05(03)          TYPE C         ,
      P_COLUMN06(03)          TYPE C         ,
      P_COLUMN07(03)          TYPE C         ,
      P_COLUMN08(03)          TYPE C         ,
      P_COLUMN09(03)          TYPE C         ,
      P_COLUMN10(03)          TYPE C         ,
      P_VALUE01               TYPE AUSP-ATWRT,
      P_VALUE02               TYPE AUSP-ATWRT,
      P_VALUE03               TYPE AUSP-ATWRT,
      P_VALUE04               TYPE AUSP-ATWRT,
      P_VALUE05               TYPE AUSP-ATWRT,
      P_VALUE06               TYPE AUSP-ATWRT,
      P_VALUE07               TYPE AUSP-ATWRT,
      P_VALUE08               TYPE AUSP-ATWRT,
      P_VALUE09               TYPE AUSP-ATWRT,
      P_VALUE10               TYPE AUSP-ATWRT.

DATA: WA_CAR                  LIKE AUSP-ATWRT,  " Car Type - Name
      WA_MI                   LIKE AUSP-ATWRT,  " Basic Model
      WA_OCN                  LIKE AUSP-ATWRT,  " OCN
      WA_VERSION              LIKE AUSP-ATWRT,  " VERSION
      WA_WO_CREATE_DATE       LIKE AUSP-ATWRT,  " Create Date
      WA_WO_MODI_DATE         LIKE AUSP-ATWRT,  " Modify Date
      WA_LC_NO                LIKE AUSP-ATWRT,  " L/C No
      WA_DESTINATION_CODE     LIKE AUSP-ATWRT,  " DESTINATION
      WA_UPDATE_ALC_DATE1     LIKE AUSP-ATWRT,  " ECM Transfer
      WA_NATION               LIKE AUSP-ATWRT,  " Nation
      WA_DEALER               LIKE AUSP-ATWRT,  " Dealer
      WA_PERF_YN              LIKE AUSP-ATWRT,  " Complete flag
      WA_VAL11                LIKE AUSP-ATWRT,  " Self-Certified No
      WA_VAL12                LIKE AUSP-ATWRT,  " Approval Date
      WA_INIT_QTY             LIKE AUSP-ATWRT,  " Initiate Qty
      WA_MOD_QTY              LIKE AUSP-ATWRT,  " Modify Qty
      WA_PLAN_QTY             LIKE AUSP-ATWRT,  " Plan Qty
      WA_FORECAST_QTY         LIKE AUSP-ATWRT,  " Forecast Qty
      WA_MITU_QTY             LIKE AUSP-ATWRT,  " MITU Qty
      WA_SEQ_QTY              LIKE AUSP-ATWRT,  " Sequence Qty
      WA_TRIM_PLANT_NO        LIKE AUSP-ATWRT,  " Plant
      WA_VIN_SPEC             LIKE AUSP-ATWRT,  " V.I.N
      WA_PROD                 LIKE AUSP-ATWRT,  " PROD_FLAG
      WA_PROD_DATE            LIKE AUSP-ATWRT,  " PROD_DATE
      WA_WOC_DATE             LIKE AUSP-ATWRT,  " W/O Creation Date
      WA_WOM_DATE             LIKE AUSP-ATWRT,  " W/O Modify Date
      WA_ALC1DATE             LIKE AUSP-ATWRT,  " ALC Update1
      WA_ALC2DATE             LIKE AUSP-ATWRT,  " ALC Update2
      WA_ECOLOR               LIKE AUSP-ATWRT,  " External Color
      WA_ICOLOR               LIKE AUSP-ATWRT,  " Internal Color

      WA_INIT_FLG             TYPE C       ,
      WA_SCN_FLAG             TYPE C       ,
      WA_SAVE_FLG             TYPE C       ,
      WA_EDIT                 TYPE C       ,
      WA_CHANGE               TYPE C       ,
      WA_ANSWER               TYPE C       ,
      WA_INSERT               TYPE C       ,
      WA_ERR_FLAG             TYPE C       ,
      WA_LINES                TYPE I       ,
      WA_TOTQTY               TYPE I       ,       " Total Quantity
      SV_PROG                 LIKE SY-REPID,
      SV_DYNNR                LIKE SY-DYNNR,
      SV_CODE                 LIKE SY-UCOMM,
      OK_CODE                 LIKE SY-UCOMM,
      WA_FILENAME             LIKE RLGRAP-FILENAME,
      WA_ORDER                LIKE MARA-MATNR       ,
      WA_WO_PACK(05)          TYPE C                ,
      WA_COLOR                LIKE MARA-MATNR       ,
      WA_ATINN                LIKE AUSP-ATINN       ,
      WA_ATWRT                LIKE AUSP-ATWRT       ,
      WA_ATFLV                LIKE AUSP-ATFLV       ,
      WA_DEP                  LIKE CUKB-KNNAM       ,
      WA_NUM(8)               TYPE N                ,
      LINE_COUNT              TYPE I.

*------- Radio-Button Group's Variables ------------------------------
DATA: R01, R02, R03, R04, R05, R06, R07, R08, R09, R10,
      R11, R12, R13, R14, R15, R16, R17, R18, R19, R20,
      R21, R22, R23, R24, R25, R26, R27, R28, R29, R30,
      R31, R32, R33, R34, R35, R36, R37, R38, R39, R40,
      R41, R42, R43, R44, R45, R46, R47, R48, R49, R50,
      R51, R99, R81, R60, R82, R88.

*----------------------------------------------------------------------
* FIELD-SYMBOLS DECLARATION (GLOBAL)
*----------------------------------------------------------------------
FIELD-SYMBOLS: <FIELD1>   TYPE ANY,
               <FIELD2>   TYPE ANY,
               <FIELD3>   TYPE ANY.

*----------------------------------------------------------------------
* INTERNAL-TABLES DECLARATION (GLOBAL)
*----------------------------------------------------------------------
DATA: BEGIN OF IT_ALCU_A      OCCURS 50 ,
        CHECK                 TYPE C,
        CLM(3)                TYPE N,
        VALS(10)              TYPE C,
      END OF IT_ALCU_A              ,
      BEGIN OF IT_MENU        OCCURS 0,
        FCODE                 LIKE RSMPE-FUNC,
      END OF IT_MENU                  ,
      BEGIN OF IT_219         OCCURS 0,
        MARK                  TYPE C,
        NO(3)                 TYPE N,
        219CODE               LIKE AUSP-ATWRT,
        219DESC               LIKE AUSP-ATWRT,
        219VALS               LIKE AUSP-ATWRT,
      END OF IT_219               .

DATA: IT_ALCU_B LIKE TABLE OF IT_ALCU_A  WITH HEADER LINE,
      IT_ALCU_C LIKE TABLE OF IT_ALCU_A  WITH HEADER LINE,
      IT_ALCU_D LIKE TABLE OF IT_ALCU_A  WITH HEADER LINE,
      IT_MARA   LIKE TABLE OF MARA       WITH HEADER LINE,
      IT_VALS_APP207 LIKE TABLE OF ZSPP_VIN_VALUE
                                                WITH HEADER LINE,
      IT_COLOR  LIKE TABLE OF ZSPP_VIN_VALUE
                WITH HEADER LINE,
      IT_RESULT1001 LIKE TABLE OF IT_COLOR WITH HEADER LINE.


DATA: BEGIN OF IT_EXCEL000 OCCURS 0,
        COL01(40),
        COL02(40),
        COL03(40),
        COL04(40),
        COL05(40),
        COL06(40),
        COL07(40),
        COL08(40),
        COL09(40),
        COL10(40),
        COL11(40),
        COL12(40),
        COL13(40),
        COL14(40),
        COL15(40),
        COL16(40),
        COL17(40),
        COL18(40),
        COL19(40),
        COL20(40),
        COL21(40),
      END OF   IT_EXCEL000.
DATA: BEGIN OF IT_ERR OCCURS 0.
        INCLUDE STRUCTURE CUXREF.
DATA:   MARK,
        NO(3)                 TYPE N  ,
        COL(5)                TYPE C  ,
        COLNM(40)             TYPE C  ,
        COLDC(40)             TYPE C  ,
        KNNAM                 LIKE CUKB-KNNAM,
        CTYPE                 TYPE C  ,
      END OF IT_ERR .

*----------------------------------------------------------------------
* CONSTANTS VARIABLE DECLARATION (GLOBAL)
*----------------------------------------------------------------------
CONSTANTS: C_APPLI(5)         VALUE 'PP'   ,
           C_COMPANY(4)       VALUE 'HMMA' ,
           C_JOBS(40)         VALUE 'ZAPP903R_INPUT_PLAN',
           C_KEY2(18)         VALUE 'INPUT_PLAN',
           TRUE               TYPE C VALUE '0',
           FALSE              TYPE C VALUE '1',
           BEGIN OF C_SS2106,
             TAB1             LIKE SY-UCOMM VALUE 'SS2106_FC1',
             TAB2             LIKE SY-UCOMM VALUE 'SS2106_FC2',
             TAB3             LIKE SY-UCOMM VALUE 'SS2106_FC3',
             TAB4             LIKE SY-UCOMM VALUE 'SS2106_FC4',
             TAB5             LIKE SY-UCOMM VALUE 'SS2106_FC5',
             TAB6             LIKE SY-UCOMM VALUE 'SS2106_FC6',
             TAB7             LIKE SY-UCOMM VALUE 'SS2106_FC7',
           END OF C_SS2106.

************* FIELD FOR THE SCREEN 0101 - ORDER COLOR *****************
DATA: WA_CHG_1001_FLG         TYPE C         ,
      WA_INDEXS               LIKE SY-TABIX  ,
      WA_TOTAL                TYPE I         ,  " Total Qty(Today)
      WA_VAL29                LIKE AUSP-ATWRT,
      WA_VAL30                LIKE AUSP-ATWRT,
      WA_VAL31                LIKE AUSP-ATWRT,
      WA_VAL32                LIKE AUSP-ATWRT,
      WA_VAL33                LIKE AUSP-ATWRT,
      WA_VAL34                LIKE AUSP-ATWRT,
      WA_VAL35                LIKE AUSP-ATWRT,
      WA_VAL36                LIKE AUSP-ATWRT,
      WA_VAL37                LIKE AUSP-ATWRT,
      WA_VAL38                LIKE AUSP-ATWRT,
      WA_VAL39                LIKE AUSP-ATWRT,

************* FIELD FOR THE SCREEN 1002 - ORDER COLOR *****************
      WA_CHG_1002_FLG         TYPE C       ,
      WA_INDEX                LIKE SY-TABIX.

DATA: BEGIN OF IT_WOSUM       OCCURS 0.
        INCLUDE STRUCTURE     ZTPP_WOSUM .
DATA:   MARK                  TYPE C,
        ORDER_NO              LIKE AUSP-ATWRT,
        T_DATE                LIKE AUSP-ATWRT,
        FLAG1                 TYPE C,
      END OF IT_WOSUM             .

DATA: WA_WOSUM_KEY            LIKE TABLE OF IT_WOSUM   WITH HEADER LINE,
      WA_WOSUM                LIKE TABLE OF IT_WOSUM   WITH HEADER LINE,
      WA_TOT                  LIKE ZTPP_WOSUM,
      WA_IQTY                 LIKE AUSP-ATWRT,
      WA_MQTY                 LIKE AUSP-ATWRT,
      WA_FLAG                 TYPE C         ,   " For the Initial flag.
      WA_EXTC(3)              TYPE C         ,
      WA_INTC(3)              TYPE C         .

DATA: BEGIN OF WA_219,
        I01(03),  I02(03),  I03(03),  I04(03),  I05(03),
        I06(03),  I07(03),  I08(03),  I09(03),  I10(03),
        I11(03),  I12(03),  I13(03),  I14(03),  I15(03),
        I16(03),  I17(03),  I18(03),  I19(03),  I20(03),
        O01(20),  O02(03),  O03(03),  O04(03),  O05(03),
        O06(20),  O07(03),  O08(03),  O09(03),  O10(03),
        O11(20),  O12(03),  O13(03),  O14(03),  O15(03),
        O16(20),  O17(03),  O18(03),  O19(03),  O20(03),
      END OF WA_219.

************* FIELD FOR THE SCREEN 1003 - ORDER COLOR *****************
INCLUDE MZPP_APPLICATIONTOP_0103 .

************* FIELD FOR THE SCREEN 1004 - ORDER COLOR *****************
INCLUDE MZPP_APPLICATIONTOP_0104 .

************* FIELD FOR THE SCREEN 1005 - ORDER COLOR *****************
DATA: WA_CHG_1005_FLG         TYPE C,
      WA_1005_OCN01(4)        TYPE C,
      WA_1005_OCN02(4)        TYPE C,
      WA_1005_OCN03(4)        TYPE C,
      WA_1005_OCN04(4)        TYPE C,
      WA_1005_OCN05(4)        TYPE C,
      WA_1005_OCN06(4)        TYPE C,
      WA_1005_OCN07(4)        TYPE C,
      WA_1005_OCN08(4)        TYPE C,
      WA_1005_OCN09(4)        TYPE C,
      WA_1005_OCN10(4)        TYPE C,
      WA_1005_OCN11(4)        TYPE C,
      WA_1005_OCN12(4)        TYPE C,
      WA_1005_OCN13(4)        TYPE C,
      WA_1005_OCN14(4)        TYPE C,
      WA_1005_OCN15(4)        TYPE C,
      WA_1005_OCN16(4)        TYPE C,
      WA_1005_OCN17(4)        TYPE C,
      WA_1005_OCN18(4)        TYPE C,
      WA_1005_OCN19(4)        TYPE C,
      WA_1005_OCN20(4)        TYPE C,
      WA_1005_OCN21(4)        TYPE C,
      WA_1005_OCN22(4)        TYPE C.

************* FIELD FOR THE SCREEN 1006 - ORDER COLOR *****************
DATA: WA_CHG_1006_FLG         TYPE C,
      WA_1006_OCN01(4)        TYPE C,
      WA_1006_OCN02(4)        TYPE C,
      WA_1006_OCN03(4)        TYPE C,
      WA_1006_OCN04(4)        TYPE C,
      WA_1006_OCN05(4)        TYPE C,
      WA_1006_OCN06(4)        TYPE C,
      WA_1006_OCN07(4)        TYPE C,
      WA_1006_OCN08(4)        TYPE C,
      WA_1006_OCN09(4)        TYPE C,
      WA_1006_OCN10(4)        TYPE C,
      WA_1006_OCN11(4)        TYPE C,
      WA_1006_OCN12(4)        TYPE C,
      WA_1006_OCN13(4)        TYPE C,
      WA_1006_OCN14(4)        TYPE C,
      WA_1006_OCN15(4)        TYPE C,
      WA_1006_OCN16(4)        TYPE C,
      WA_1006_OCN17(4)        TYPE C,
      WA_1006_OCN18(4)        TYPE C,
      WA_1006_OCN19(4)        TYPE C,
      WA_1006_OCN20(4)        TYPE C,
      WA_1006_OCN21(4)        TYPE C,
      WA_1006_OCN22(4)        TYPE C,
      WA_1006_219I01(4)       TYPE N ,
      WA_1006_219I02(4)       TYPE N ,
      WA_1006_219I03(4)       TYPE N ,
      WA_1006_219I04(4)       TYPE N ,
      WA_1006_219I05(4)       TYPE N ,
      WA_1006_219I06(4)       TYPE N ,
      WA_1006_219I07(4)       TYPE N ,
      WA_1006_219I08(4)       TYPE N ,
      WA_1006_219I09(4)       TYPE N ,
      WA_1006_219I10(4)       TYPE N ,
      WA_1006_219I11(4)       TYPE N ,
      WA_1006_219I12(4)       TYPE N ,
      WA_1006_219I13(4)       TYPE N ,
      WA_1006_219I14(4)       TYPE N ,
      WA_1006_219I15(4)       TYPE N ,
      WA_1006_219I16(4)       TYPE N ,
      WA_1006_219I17(4)       TYPE N ,
      WA_1006_219I18(4)       TYPE N ,
      WA_1006_219I19(4)       TYPE N ,
      WA_1006_219I20(4)       TYPE N ,
      WA_1006_219O01(4)       TYPE C ,
      WA_1006_219O02(4)       TYPE C ,
      WA_1006_219O03(4)       TYPE C ,
      WA_1006_219O04(4)       TYPE C ,
      WA_1006_219O05(4)       TYPE C ,
      WA_1006_219O06(4)       TYPE C ,
      WA_1006_219O07(4)       TYPE C ,
      WA_1006_219O08(4)       TYPE C ,
      WA_1006_219O09(4)       TYPE C ,
      WA_1006_219O10(4)       TYPE C ,
      WA_1006_219O11(4)       TYPE C ,
      WA_1006_219O12(4)       TYPE C ,
      WA_1006_219O13(4)       TYPE C ,
      WA_1006_219O14(4)       TYPE C ,
      WA_1006_219O15(4)       TYPE C ,
      WA_1006_219O16(4)       TYPE C ,
      WA_1006_219O17(4)       TYPE C ,
      WA_1006_219O18(4)       TYPE C ,
      WA_1006_219O19(4)       TYPE C ,
      WA_1006_219O20(4)       TYPE C .

* Range
RANGES : R_ATNAM FOR CABN-ATNAM.

* FIELD FOR THE SCREEN 0108
DATA: BEGIN OF IT_0108  OCCURS 0,
        MARK,
        ORDNO                 LIKE MARA-MATNR,      " Order-No
        ECODE                 TYPE N,               " Error-Code
        EFILE                 TYPE C,               " Error text
        PDATE                 TYPE D,               " Processing Date
        MSG                   TYPE C,               " Message
      END OF IT_0108.

* FIELD FOR THE SCREEN 0109 Order status by model
**********
** Internal table   *
DATA : BEGIN OF  IT_0109 OCCURS 10,
       NATION    LIKE AUSP-ATWRT,             " NATION
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       MODQTY    LIKE ZTPP_WOSUM-MODQTY,      " INPUT QTY
       OCN       LIKE AUSP-ATWRT,             " OCN
       PLANT     LIKE AUSP-ATWRT,             " PLANT
       VER       LIKE AUSP-ATWRT,             " VERSION
       MI        LIKE AUSP-ATWRT,             " mi
       G_DATE    LIKE AUSP-ATWRT,             " GEN DATE
       A_DATE    LIKE AUSP-ATWRT,             " ACL DATE
       FLAG      LIKE AUSP-ATWRT,             " FLAG
       ALCD1     LIKE AUSP-ATWRT,             " ALC UPDATE1
       ALCD2     LIKE AUSP-ATWRT,             " ALC UPDATE2
       219_1     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_2     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_3     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_4     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_5     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_6     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_7     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_8     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_9     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_10    LIKE AUSP-ATWRT,             " 219 SELECTION
       219_19    LIKE AUSP-ATWRT,             " 219 SELECTION
       vin_hmma  LIKE ztpp_vin_if-vin_spec,
       vin_hmc(8),
       vin_dif(18),
       END OF IT_0109.

DATA : BEGIN OF  IT_8081 OCCURS 0,
       NATION    LIKE AUSP-ATWRT,             " NATION
       FSC       LIKE ZTPP_WOSUM-FSC,
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       EXTC      LIKE ZTPP_WOSUM-EXTC,
       INTC      LIKE ZTPP_WOSUM-INTC,
       MODQTY    TYPE ZZQTY_S, "zqty ztpp_wosum-modqty,      " INPUT QTY
       SEQ       TYPE ZZQTY_S , "ztpp_wosum-modqty,
       BIN       TYPE ZZQTY_S , "ztpp_wosum-modqty,
       TIN       TYPE  ZZQTY_S , "ztpp_wosum-modqty,
       SOFF      TYPE  ZZQTY_S , "ztpp_wosum-modqty,
       CGATE     TYPE  ZZQTY_S , "ztpp_wosum-modqty,
*       shipin    type  zzqty_s , "ztpp_wosum-modqty,
       SHIPOUT    TYPE  ZZQTY_S , "ztpp_wosum-modqty,
       NEWQTYC   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY01   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY02   TYPE  ZZQTY_S, "ztsd_sodata-NEWQTY,
       NEWQTY03   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY04   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY05   TYPE  ZZQTY_S, "ztsd_sodata-NEWQTY,
       NEWQTY06   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY07   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY08   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY09   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY10   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY11   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       219_19    LIKE AUSP-ATWRT,             " 219 SELECTION
       END OF IT_8081.

DATA : BEGIN OF  IT_8082 OCCURS 0,
       NATION    LIKE AUSP-ATWRT,             " NATION
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       FSC       LIKE ZTPP_WOSUM-FSC,
       EXTC      LIKE ZTPP_WOSUM-EXTC,
       INTC      LIKE ZTPP_WOSUM-INTC,
       MODQTY    TYPE ZZQTY_S, "zqty ztpp_wosum-modqty,      " INPUT QTY
       SEQ       TYPE ZZQTY_S , "ztpp_wosum-modqty,
       PLANQTY   TYPE ZZQTY_S, "zqty ztpp_wosum-planqty,
       FORECASTQTY     TYPE ZZQTY_S , "ztpp_wosum-forecastqty,
       BIN       TYPE ZZQTY_S , "ztpp_wosum-modqty,
       TIN       TYPE  ZZQTY_S , "ztpp_wosum-modqty,
       SOFF      TYPE  ZZQTY_S , "ztpp_wosum-modqty,
       CGATE     TYPE  ZZQTY_S , "ztpp_wosum-modqty,
*       shipin    type  zzqty_s , "ztpp_wosum-modqty,
       SHIPOUT    TYPE  ZZQTY_S , "ztpp_wosum-modqty,
       NEWQTYC   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY01   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY02   TYPE  ZZQTY_S, "ztsd_sodata-NEWQTY,
       NEWQTY03   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY04   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY05   TYPE  ZZQTY_S, "ztsd_sodata-NEWQTY,
       NEWQTY06   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY07   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY08   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY09   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY10   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       NEWQTY11   TYPE  ZZQTY_S , "ztsd_sodata-NEWQTY,
       219_19    LIKE AUSP-ATWRT,             " 219 SELECTION
       END OF IT_8082.

* INPUT VALUE.
DATA : BEGIN OF  ST_0109_INPUT OCCURS 10,
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       MONTY(4)  TYPE C ,                     " MONTH( MONTH PACK)
       NATION(3) TYPE C ,                     " NATION CODE
       USE(1)    TYPE C,                      " LOCAL OR DOMESTIC
       FULL(2)   TYPE C,                      " FULL OR NOT
       TRAN(2)   TYPE C,                      " Spec Transfer
       QTY       LIKE ZTPP_WOSUM-MODQTY,
       219_1     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_2     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_3     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_4     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_5     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_6     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_7     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_8     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_9     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_10    LIKE AUSP-ATWRT,             " 219 SELECTION
       219_1_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_2_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_3_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_4_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_5_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_6_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_7_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_8_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_9_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_10_V    LIKE AUSP-ATWRT,                         " 219 VALUE
       END OF ST_0109_INPUT.

DATA : BEGIN OF  ST_8081_INPUT OCCURS 0,
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       MONTY(4)  TYPE C ,                     " MONTH( MONTH PACK)
       NATION(3) TYPE C ,                     " NATION CODE
       USE(1)    TYPE C,                      " LOCAL OR DOMESTIC
       COLOR(2)   TYPE C,                      " FULL OR NOT
       TRAN(2)   TYPE C,                      " Spec Transfer
       QTY       LIKE ZTPP_WOSUM-MODQTY,
       219_1     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_2     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_3     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_4     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_5     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_6     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_7     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_8     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_9     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_10    LIKE AUSP-ATWRT,             " 219 SELECTION
       219_1_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_2_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_3_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_4_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_5_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_6_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_7_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_8_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_9_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_10_V    LIKE AUSP-ATWRT,                         " 219 VALUE
       END OF ST_8081_INPUT.

DATA : BEGIN OF  ST_8082_INPUT OCCURS 0,
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       MONTY(4)  TYPE C ,                     " MONTH( MONTH PACK)
       NATION(3) TYPE C ,                     " NATION CODE
       USE(1)    TYPE C,                      " LOCAL OR DOMESTIC
       COLOR(2)   TYPE C,                      " FULL OR NOT
       FYEAR(4)  TYPE C,
       TYEAR(4)  TYPE C,
       FMONTH(2) TYPE C,
       TMONTH(2) TYPE C,
       TRAN(2)   TYPE C,                      " Spec Transfer
       QTY       LIKE ZTPP_WOSUM-MODQTY,
       219_1     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_2     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_3     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_4     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_5     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_6     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_7     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_8     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_9     LIKE AUSP-ATWRT,             " 219 SELECTION
       219_10    LIKE AUSP-ATWRT,             " 219 SELECTION
       219_1_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_2_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_3_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_4_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_5_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_6_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_7_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_8_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_9_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_10_V    LIKE AUSP-ATWRT,                         " 219 VALUE
       END OF ST_8082_INPUT.

DATA : BEGIN OF TL_8081_OUTPUT OCCURS 0,
       T_C(7),
       T_01(7),
       T_02(7),
       T_03(7),
       T_04(7),
       T_05(7),
       T_06(7),
       T_07(7),
       T_08(7),
       T_09(7),
       T_10(7),
       T_11(7),
END OF TL_8081_OUTPUT.

DATA : BEGIN OF TL_8082_OUTPUT OCCURS 0,
       T_C(7),
       T_01(7),
       T_02(7),
       T_03(7),
       T_04(7),
       T_05(7),
       T_06(7),
       T_07(7),
       T_08(7),
       T_09(7),
       T_10(7),
       T_11(7),
END OF TL_8082_OUTPUT.

* FIELD FOR THE SCREEN 0110 Order status by model
**********

** Internal table   *
DATA : BEGIN OF IT_0110 OCCURS 0,
       EXCLR   LIKE AUSP-ATWRT,  " external color
       INCLR   LIKE AUSP-ATWRT,  " internal color
       BODY    LIKE AUSP-ATWRT,  " body number
       MITU    LIKE AUSP-ATWRT,  " mitu
       STATUS  LIKE AUSP-ATWRT,  " STATUS
       RDATE   LIKE AUSP-ATWRT,  " reporting date
       ENGINE  LIKE AUSP-ATWRT,  " engine number
       NATION  LIKE AUSP-ATWRT,  " nation
       SALES   LIKE AUSP-ATWRT,  " Sales Order
       PORDER  LIKE AUSP-ATWRT,  " Plan Order
       END OF IT_0110.

DATA : BEGIN OF ST_0110_INPUT ,
       ORDER  LIKE MARA-MATNR ,"ausp-atwrt,  " order number
       EXCLR  LIKE AUSP-ATWRT,  " external color
       INCLR  LIKE AUSP-ATWRT,  " internal colo
       STATUS LIKE AUSP-ATWRT,  " begin status
       STATUS2 LIKE AUSP-ATWRT, " end status
       MI     LIKE AUSP-ATWRT,  " spec
       OCN    LIKE AUSP-ATWRT,  " ocn
       VER    LIKE AUSP-ATWRT,
       TBD    LIKE AUSP-ATWRT,  " option
       COUNT  TYPE I ,
       END OF ST_0110_INPUT.

* FIELD FOR THE SCREEN 0110 Order status by model
**********

** Internal table   *
DATA : BEGIN OF IT_0111 OCCURS 10,
       MARK      TYPE C,
       CLM(3)    TYPE C,
       CNAME     LIKE CABN-ATNAM,
       CODE1     LIKE AUSP-ATWRT,
       CODE2     LIKE AUSP-ATWRT,
       END OF IT_0111 .

DATA : BEGIN OF IT_0111_C OCCURS 10,
       MARK      TYPE C,
       CLM(3)    TYPE C,
       CNAME     LIKE CABN-ATNAM,
       CODE1     LIKE AUSP-ATWRT,
       CODE2     LIKE AUSP-ATWRT,
       END OF IT_0111_C .

DATA : BEGIN OF ST_0111_INPUT ,
       ORDER1      LIKE MARA-MATNR,   " W/O
       NATION1     LIKE AUSP-ATWRT,
       EXCLR1      LIKE AUSP-ATWRT,
       INCLR1      LIKE AUSP-ATWRT,
       MI1         LIKE AUSP-ATWRT,
       OCN1        LIKE AUSP-ATWRT,
       VER1        LIKE AUSP-ATWRT,
       ORDER2      LIKE MARA-MATNR,   " W/O
       NATION2     LIKE AUSP-ATWRT,
       EXCLR2      LIKE AUSP-ATWRT,
       INCLR2      LIKE AUSP-ATWRT,
       MI2         LIKE AUSP-ATWRT,
       OCN2        LIKE AUSP-ATWRT,
       VER2        LIKE AUSP-ATWRT,
       END OF ST_0111_INPUT.

* FIELD FOR THE SCREEN 0110 Order status by model
**********

** Internal table   *

DATA : BEGIN OF IT_0118 OCCURS 10,
        MODEL LIKE AUSP-ATWRT,
        ORDER LIKE AUSP-ATWRT,
        INIT  TYPE P,
        MODI  TYPE P,
        SEQU  TYPE P,
        MITU  TYPE P,
        PLAN  TYPE P,
        PLANW TYPE P,
        FORE  TYPE P,
        MI    LIKE AUSP-ATWRT,
        OCN   LIKE AUSP-ATWRT,
        VERS  LIKE AUSP-ATWRT,
        MESSAGE LIKE AUSP-ATWRT,
        END OF IT_0118.

** input field **
*data : begin of st_0118_input ,
DATA : BEGIN OF ST_0118_INPUT ,
        PLANT     LIKE AUSP-ATWRT,
        MODEL     LIKE AUSP-ATWRT,
        MPACK     TYPE AUSP-ATWRT,
        END OF ST_0118_INPUT.

* FIELD FOR THE SCREEN 2101 - Emission test vehicle mgm **********
** INTERNAL TABLE *
DATA : G_TC_2101_LINES LIKE SY-LOOPC .

DATA : BEGIN OF IT_2101  OCCURS 10,
       OBJEK      LIKE AUSP-OBJEK,
       BODY       LIKE AUSP-ATWRT,
       VIN        LIKE AUSP-ATWRT,
       ORDER      LIKE AUSP-ATWRT,
       RP         LIKE AUSP-ATWRT,
       SEQDATE    LIKE AUSP-ATWRT,
       S_OFF      LIKE AUSP-ATWRT,
       CABRATOR   LIKE AUSP-ATWRT,
       BAGIGAS    LIKE AUSP-ATWRT,
       TEST       ,
       TESTDATE   LIKE AUSP-ATWRT,
       SEQ(10)    TYPE N,
*       em_rate    type ausp-atwrt,
       MARK ,
       END OF IT_2101.

DATA : BEGIN OF ST_SQL  ,
       OBJEK      LIKE AUSP-OBJEK,
       MODEL      LIKE AUSP-ATWRT,
       BODY       LIKE AUSP-ATWRT,
       VIN        LIKE AUSP-ATWRT,
       ORDER      LIKE AUSP-ATWRT,
       RP         LIKE AUSP-ATWRT,
       SEQDATE    LIKE AUSP-ATWRT,
*            s_off      like ausp-ATWRT,
*            cabrator   like ausp-ATWRT,
*            bagigas    like ausp-atwrt,
*            testdate   like ausp-atwrt,
       END OF ST_SQL.

DATA : BEGIN OF IT_AUSP OCCURS 0.
        INCLUDE STRUCTURE AUSP  .
DATA:   ATNAM  LIKE CABN-ATNAM,
       END OF IT_AUSP.

DATA: BEGIN OF ST_2101 ,
       MODEL     LIKE ZTPP_VEH_MODEL-MODEL,
       CABRATOR  LIKE ZTBM_ABXOPVDT-VALU,
*       cabrator  like ztbm_219_value-value,
       BODY      LIKE AUSP-ATWRT,
       ENGINE    LIKE ZTBM_ABXOPVDT-VALU,
*       engine    like ztbm_219_value-value,
       FDATE     LIKE SY-DATUM,
       TDATE     LIKE SY-DATUM,
      END OF ST_2101.

DATA : IT_BACK2101 LIKE IT_2101 OCCURS 100 WITH HEADER LINE .

*---- LIST BOX DATA
DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST,
      BEGIN OF YLIST     OCCURS 0,
         KEY(40) TYPE C,
         TEXT(80) TYPE C,
      END OF YLIST      .


* FIELD FOR THE SCREEN 2102 - CAL region production status **********
** INTERNAL TABLE *
DATA : BEGIN OF IT_2102 OCCURS 10,
       DEST      LIKE AUSP-ATWRT,
       MODEL     LIKE AUSP-ATWRT,
       DESCR     LIKE AUSP-ATWRT,
       SEQTY     TYPE P DECIMALS 0,
       TEQTY     TYPE P DECIMALS 0,
       RATE      TYPE P DECIMALS 2,
       EM_RATE   TYPE P DECIMALS 2,
       TEXT      LIKE AUSP-ATWRT,
       END OF IT_2102 .

DATA : BEGIN OF ST_2102  ,
       MODEL     LIKE ZTPP_VEH_MODEL-MODEL,
       FDATE     LIKE SY-DATUM,
       TDATE     LIKE SY-DATUM,
       END OF ST_2102.

*data :
* FIELD FOR THE SCREEN 2103 - Duplicated Engine number vehicle status
**********
** INTERNAL TABLE *


DATA : BEGIN OF IT_2103 OCCURS 10,
       SEQ     TYPE I,
       ENGINE  LIKE AUSP-ATWRT,  " engine number
       BODY1   LIKE AUSP-ATWRT,  " body number 1
       BODY2   LIKE AUSP-ATWRT,  " body number 2
       BODY3   LIKE AUSP-ATWRT,  " body number 3
       BODY4   LIKE AUSP-ATWRT,  " body number 4
       BODY5   LIKE AUSP-ATWRT,  " body number 5
       END OF IT_2103.

DATA : G_BODY_NO LIKE AUSP-OBJEK.
DATA: W_FDATE_2103 TYPE SY-DATUM,
      W_TDATE_2103 TYPE SY-DATUM.
* FIELD FOR THE SCREEN 2104 - Duplicated Engine number vehicle status
**********
** INTERNAL TABLE *


DATA : BEGIN OF IT_2104 OCCURS 10,
       MARK,
       BODY_NO LIKE AUSP-ATWRT,  " BODY NUMBER
       CF      LIKE AUSP-ATWRT,  " C/F
       S_OFF   LIKE AUSP-ATWRT,  " SIGN OFF
       ENGINE  LIKE AUSP-ATWRT,  " ENGINE NUMBER
       TM      LIKE AUSP-ATWRT,  " TRIM NUMBER
       KEY     LIKE AUSP-ATWRT,  " KEY NUMBER
       WORDER  LIKE AUSP-ATWRT,  " WORK ORDER (1) CHAR
       STATUS  LIKE AUSP-ATWRT,  " STATUS
       TRIM    LIKE AUSP-ATWRT,  " TRIM PLANT NUMBER
       END OF IT_2104.

DATA : BEGIN OF ST_2104  ,
       MODEL     LIKE ZTPP_VEH_MODEL-MODEL,
       BODY      LIKE AUSP-ATWRT,
       SELC,
       END OF ST_2104.

* FIELD FOR THE SCREEN 2105 - Prod. Results by Progress
**********
** INTERNAL TABLE *

DATA : BEGIN OF IT_2105 OCCURS 10,
       BODY_NO LIKE AUSP-ATWRT,  " BODY NUMBER
       VIN     LIKE AUSP-ATWRT,  " vin
       ORDER   LIKE AUSP-ATWRT,  " ORDER NUMBER
       E_COL   LIKE AUSP-ATWRT,  " External Color
       I_COL   LIKE AUSP-ATWRT,  " Internal Color
       LIFNR   LIKE AUSP-ATWRT,  " Consign vendor
       MI      LIKE AUSP-ATWRT,  " Mi
       OCN     LIKE AUSP-ATWRT,  " OCN
       VER     LIKE AUSP-ATWRT,  " VER
       REDATE  LIKE AUSP-ATWRT,  " REPORTING DATE
       STATUS  LIKE AUSP-ATWRT,  " STATUS
       END OF IT_2105.


DATA : BEGIN OF ST_2105  ,
       PLANT     LIKE AUSP-ATWRT,           " plant name
       MODEL     LIKE ZTPP_VEH_MODEL-MODEL, " Model
       BODY      LIKE AUSP-ATWRT,           " body number
       LINE      LIKE AUSP-ATWRT,           " line
       STATUS    LIKE AUSP-ATWRT,           " status
       SORT(2)   TYPE C,                    " sort option
       ORDER     LIKE AUSP-ATWRT,           " ORDER NUMBER
       ECOL      LIKE AUSP-ATWRT,           " External color
       ICOL      LIKE AUSP-ATWRT,           " Internal color
       PDATE     LIKE SY-DATUM,             " Product date
       RDATE     LIKE AUSP-ATWRT,           " reporting date
       SERIAL    LIKE AUSP-ATWRT,           " Serial number
       219_1_N(3)  TYPE C,                  " 219 option 1
       219_1_V(1)  TYPE C,                  " 219 value  1
       219_2_N(3)  TYPE C,                  " 219 option 2
       219_2_V(1)  TYPE C,                  " 219 value  2
       219_3_N(3)  TYPE C,                  " 219 option 3
       219_3_V(1)  TYPE C,                  " 219 value  3
       219_4_N(3)  TYPE C,                  " 219 option 4
       219_4_V(1)  TYPE C,                  " 219 value  4
       219_5_N(3)  TYPE C,                  " 219 option 5
       219_5_V(1)  TYPE C,                  " 219 value  5
       219_6_N(3)  TYPE C,                  " 219 option 6
       219_6_V(1)  TYPE C,                  " 219 value  6
       219_7_N(3)  TYPE C,                  " 219 option 7
       219_7_V(1)  TYPE C,                  " 219 value  7
       219_8_N(3)  TYPE C,                  " 219 option 8
       219_8_V(1)  TYPE C,                  " 219 value  8
       219_9_N(3)  TYPE C,                  " 219 option 9
       219_9_V(1)  TYPE C,                  " 219 value  9
       219_10_N(3)  TYPE C,                 " 219 option 10
       219_10_V(1)  TYPE C,                 " 219 value  10
       COUNT TYPE I,                        " Count
       END OF ST_2105.

* FIELD FOR THE SCREEN 2202 Rejected vehicle status
**********
** INTERNAL TABLE *
DATA : BEGIN OF IT_2202 OCCURS 10,
         MARK       ,
         SEQ TYPE I ,
         BODY   LIKE AUSP-ATWRT,   " BODY NUMBER
         ORDER  LIKE AUSP-ATWRT,   " ORDER NUMBER
         SPEC   LIKE AUSP-ATWRT,   " SPEC
         ECOLOR LIKE AUSP-ATWRT,   " EXTERNAL COLOR
         ICOLOR LIKE AUSP-ATWRT,   " INTERNAL COLOR
         RDATE  LIKE AUSP-ATWRT,   " RETURNED DATE
         WDATE  LIKE AUSP-ATWRT,   " REWORK DATE
         PERIED LIKE AUSP-ATWRT,   " RETRUN - REWORK DATE
       END OF IT_2202.

* STRUCTURE
DATA : BEGIN OF ST_2202 ,
       PLANT LIKE AUSP-ATWRT,           " PLANT
       USE(1) TYPE C,                   "
       SEL(1) TYPE C,                   " DATA SELECTION
       SPMON  TYPE SPMON,               " MM/YYYY
       SORT(1) TYPE C,                  " SORT
       END OF ST_2202.

* FIELD FOR THE SCREEN 2203 Vehicle Gr processing
**********
** Structure  *
DATA : BEGIN OF ST_2203 ,
       MODEL  LIKE AUSP-ATWRT,   " MODEL
       BODY   LIKE AUSP-ATWRT,   " BODY NUMBER
       FILE   LIKE AUSP-ATWRT,   " FILE TYPE
       ORDER  LIKE AUSP-ATWRT,   " ORDER NUMBER
       ZONE   LIKE AUSP-ATWRT,   " ORDER ZONE
       ECOLOR LIKE AUSP-ATWRT,   " EXTERNAL COLOR
       ICOLOR LIKE AUSP-ATWRT,   " INTERNAL COLOR
       MI     LIKE AUSP-ATWRT,   " mi
       OCN    LIKE AUSP-ATWRT,   " ocn
       VERN   LIKE AUSP-ATWRT,   " version
       STATUS LIKE AUSP-ATWRT,   " Status
       VIN    LIKE AUSP-ATWRT,   " vin
       PACK   LIKE AUSP-ATWRT,   " packing
       ENGINE LIKE AUSP-ATWRT,   " Engine no
       TM     LIKE AUSP-ATWRT,   " TM no
       KEY    LIKE AUSP-ATWRT,   " KEY NUMBER
       01     LIKE AUSP-ATWRT,                              " 01 STAUS
       02     LIKE AUSP-ATWRT,                              " 02 STAUS
       03     LIKE AUSP-ATWRT,                              " 03 STAUS
       04     LIKE AUSP-ATWRT,                              " 04 STAUS
       05     LIKE AUSP-ATWRT,                              " 05 STAUS
       06     LIKE AUSP-ATWRT,                              " 06 STAUS
       07     LIKE AUSP-ATWRT,                              " 07 STAUS
       08     LIKE AUSP-ATWRT,                              " 08 STAUS
       09     LIKE AUSP-ATWRT,                              " 09 STAUS
       10     LIKE AUSP-ATWRT,                              " 10 STAUS
       11     LIKE AUSP-ATWRT,                              " 11 STAUS
       12     LIKE AUSP-ATWRT,                              " 12 STAUS
       13     LIKE AUSP-ATWRT,                              " 13 STAUS
       14     LIKE AUSP-ATWRT,                              " 14 STAUS
       15     LIKE AUSP-ATWRT,                              " 15 STAUS
       16     LIKE AUSP-ATWRT,                              " 16 STAUS
       01_R   LIKE AUSP-ATWRT,                              " 01 STAUS
       02_R   LIKE AUSP-ATWRT,                              " 02 STAUS
       03_R   LIKE AUSP-ATWRT,                              " 03 STAUS
       04_R   LIKE AUSP-ATWRT,                              " 04 STAUS
       05_R   LIKE AUSP-ATWRT,                              " 05 STAUS
       06_R   LIKE AUSP-ATWRT,                              " 06 STAUS
       07_R   LIKE AUSP-ATWRT,                              " 07 STAUS
       08_R   LIKE AUSP-ATWRT,                              " 08 STAUS
       09_R   LIKE AUSP-ATWRT,                              " 09 STAUS
       10_R   LIKE AUSP-ATWRT,                              " 10 STAUS
       11_R   LIKE AUSP-ATWRT,                              " 11 STAUS
       12_R   LIKE AUSP-ATWRT,                              " 12 STAUS
       13_R   LIKE AUSP-ATWRT,                              " 13 STAUS
       14_R   LIKE AUSP-ATWRT,                              " 14 STAUS
       15_R   LIKE AUSP-ATWRT,                              " 15 STAUS
       16_R   LIKE AUSP-ATWRT,                              " 16 STAUS
       01_T   LIKE AUSP-ATWRT,                              " 01 STAUS
       02_T   LIKE AUSP-ATWRT,                              " 02 STAUS
       03_T   LIKE AUSP-ATWRT,                              " 03 STAUS
       04_T   LIKE AUSP-ATWRT,                              " 04 STAUS
       05_T   LIKE AUSP-ATWRT,                              " 05 STAUS
       06_T   LIKE AUSP-ATWRT,                              " 06 STAUS
       07_T   LIKE AUSP-ATWRT,                              " 07 STAUS
       08_T   LIKE AUSP-ATWRT,                              " 08 STAUS
       09_T   LIKE AUSP-ATWRT,                              " 09 STAUS
       10_T   LIKE AUSP-ATWRT,                              " 10 STAUS
       11_T   LIKE AUSP-ATWRT,                              " 11 STAUS
       12_T   LIKE AUSP-ATWRT,                              " 12 STAUS
       13_T   LIKE AUSP-ATWRT,                              " 13 STAUS
       14_T   LIKE AUSP-ATWRT,                              " 14 STAUS
       15_T   LIKE AUSP-ATWRT,                              " 15 STAUS
       16_T   LIKE AUSP-ATWRT,                              " 16 STAUS
       END OF ST_2203.



DATA : BEGIN OF ST_2203_INPUT ,
       BODY   LIKE AUSP-ATWRT,   " BODY NUMBER
       END OF ST_2203_INPUT.

* FIELD FOR THE SCREEN 2204 Enter scrap/special purpose vehicle
**********
** Structure  *

DATA : BEGIN OF ST_2204_INPUT ,
       BODY   LIKE AUSP-ATWRT,   " BODY NUMBER
       DATE   LIKE SY-DATUM  ,   " action date
       END OF ST_2204_INPUT.

DATA : ST_2204_B LIKE ST_2204_INPUT.

DATA : BEGIN OF ST_2204 ,
       LOCL   LIKE AUSP-ATWRT,   " LOCATION
       ORDER  LIKE AUSP-ATWRT,   " order number
       MI     LIKE AUSP-ATWRT,   " MI
       EXCLR  LIKE AUSP-ATWRT,   " INTERNAL COLOR
       INCLR  LIKE AUSP-ATWRT,   " EXTEND COLOR
       TM     LIKE AUSP-ATWRT,   " TM
       OCN    LIKE AUSP-ATWRT,   " OCN
       VER    LIKE AUSP-ATWRT,   " VERSION
       ENGINE LIKE AUSP-ATWRT,   " Engine number
       SBODY       TYPE C    ,   " shop body
       SPAINT      TYPE C    ,   " shop paint
       STRIM       TYPE C    ,   " trim number
       SCRAP  LIKE AUSP-ATWRT,   " scrap action
       CTYPE       TYPE C    ,   " Scrap(S) / Disposal(D)
       USAGE  LIKE AUSP-ATWRT,   " usage code
       CAR    LIKE AUSP-ATWRT,   " usage CAR
       TEXT   LIKE AUSP-ATWRT,   " usage text
       SEQ    LIKE AUSP-ATWRT,   " sequence date
       BDIN   LIKE AUSP-ATWRT,   " body in date
       PAINT_IN    LIKE AUSP-ATWRT, " paint in date
       PAINT_OUT   LIKE AUSP-ATWRT, " paint out date
       TRIM_IN     LIKE AUSP-ATWRT, " trim in date
       SIGN_OFF    LIKE AUSP-ATWRT, " sign off
       C_GATE      LIKE AUSP-ATWRT, " c gate
       VPC         LIKE AUSP-ATWRT, " vpc
       CAUSION(20) TYPE C         , " Causion
       U_DEPT(20),
       END OF ST_2204 .

* FIELD FOR THE SCREEN 2204 Enter scrap/special purpose vehicle
**********
** Structure  *
DATA : BEGIN OF IT_2205 OCCURS  0,
         MODEL TYPE ZMODEL    ,
         OBJEK LIKE AUSP-OBJEK,
         USAGE LIKE AUSP-ATWRT,
         TEXT  LIKE AUSP-ATWRT,
         CAR   LIKE AUSP-ATWRT,
         WORK  LIKE AUSP-ATWRT,
         MI    LIKE AUSP-ATWRT,
         OCN   LIKE AUSP-ATWRT,
         VER   LIKE AUSP-ATWRT,
         EXT   LIKE AUSP-ATWRT,
         INT   LIKE AUSP-ATWRT,
         SHOP  LIKE AUSP-ATWRT,
         STAT  LIKE AUSP-ATWRT,
         SCRAP LIKE AUSP-ATWRT,
         BDIN  LIKE AUSP-ATWRT,
         PAI_I LIKE AUSP-ATWRT,
         PAI_O LIKE AUSP-ATWRT,
         TRIM  LIKE AUSP-ATWRT,
         SIGN  LIKE AUSP-ATWRT,
         GATE  LIKE AUSP-ATWRT,
         VPC   LIKE AUSP-ATWRT,
       END OF IT_2205.

DATA : BEGIN OF ST_2205_INPUT ,
         MODEL  LIKE AUSP-ATWRT,          " MODEL
         PLANT LIKE AUSP-ATWRT,           " PLANT
         BDATE  LIKE SY-DATUM,            " BEGIN DATE
         USAGE(1) TYPE C,                 " USEING
         EDATE  LIKE SY-DATUM,            " END   DATE
       END   OF ST_2205_INPUT.

* FIELD FOR THE SCREEN 2206 - Duplicated Engine number vehicle status
**********
** INTERNAL TABLE *
DATA : BEGIN OF IT_2206 OCCURS  0,
       MODEL    LIKE ZTPP_VEH_MODEL-MODEL,
       NAME     LIKE ZTPP_VEH_MODEL-NAME,
       CAR      TYPE C,
       TOTAL    TYPE I,
       SPEC     TYPE I,
       MATL     TYPE I,
       WORK     TYPE I,
       DRIVE    TYPE I,
       CAUSE    TYPE I,
       EQUIP    TYPE I,
       RETURN   TYPE I,
       TRANS    TYPE I,
       SKD      TYPE I,
       USAGE    TYPE I,
       TEST     TYPE I,
       AS       TYPE I,
       OTHER    TYPE I,
       END OF IT_2206.


DATA : BEGIN OF ST_2206_INPUT ,
       MODEL  LIKE AUSP-ATWRT,          " MODEL
       PLANT LIKE AUSP-ATWRT,           " PLANT
       BDATE  LIKE SY-DATUM,            " BEGIN DATE
       USAGE(1) TYPE C,                 " USEING
       SHOP  LIKE AUSP-ATWRT,           " SHOP
       EDATE  LIKE SY-DATUM,            " END   DATE
       END   OF ST_2206_INPUT.

* FIELD FOR THE SCREEN 3107 HPCC vs ALC code comparison
**********

** Internal table   *
DATA : BEGIN OF  IT_3107 OCCURS  0,
       ORDER    LIKE MARA-MATNR,             " WorK Order
       EXTC      LIKE MARA-MATNR,             " WorK Order Ext-Color
       INTC      LIKE MARA-MATNR,             " WorK Order Int-Color
       OCN       LIKE AUSP-ATWRT,             " OCN
       PLANT     LIKE AUSP-ATWRT,             " PLANT
       VER       LIKE AUSP-ATWRT,             " VERSION
       MI        LIKE AUSP-ATWRT,             " mi
       VIN       LIKE AUSP-ATWRT,             " vin
       COD1      LIKE AUSP-ATWRT,                           " 1st Code
       COD2      LIKE AUSP-ATWRT,                           " 2nd Code
*
       219_1      LIKE AUSP-ATWRT,             " Y
       219_2      LIKE AUSP-ATWRT,             " T
       219_3      LIKE AUSP-ATWRT,             " P
       219_4      LIKE AUSP-ATWRT,             " M
       219_5      LIKE AUSP-ATWRT,             " D
       219_6      LIKE AUSP-ATWRT,             " S
       219_7      LIKE AUSP-ATWRT,             " H
       219_8      LIKE AUSP-ATWRT,             " X
*
       END OF IT_3107.


* INPUT VALUE.
DATA : BEGIN OF  ST_3107_INPUT OCCURS  0,
       ORDNO(14),  "W/O Serial + Nation + Dealer
       EXTC(03),   "External Color
       INTC(03),   "Internal Color
       TABLEA(20), " (Header or Color) and Code(ALC, HPC, 219)
       TABLEB(20), " (Header or Color) and Code(ALC, HPC, 219)
       COLA(3)   TYPE C         ,             " column
       COLB(3)   TYPE N         ,             " column
*
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       TABA      TYPE C         ,             " 219 SELECTION
       TABB      TYPE C         ,             " 219 SELECTION
       UPG(08)   TYPE C         ,             " 219 SELECTION
       TEXTA(40) TYPE C         ,             " 219 SELECTION
       TEXTB(40) TYPE C         ,             " 219 SELECTION
       TEXTU(40) TYPE C         ,             " 219 SELECTION
       CODY      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODT      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODP      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODM      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODD      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODS      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODH      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODX      LIKE AUSP-ATWRT,             " 219 SELECTION

       219_1_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_2_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_3_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_4_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_5_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_6_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_7_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_8_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_9_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_10_V    LIKE AUSP-ATWRT,                         " 219 VALUE
END OF ST_3107_INPUT.

* FIELD FOR THE SCREEN 3109 HPCC vs ALC code comparison
**********

** Internal table   *
DATA : BEGIN OF  IT_3109 OCCURS  0,
        SERI     TYPE I         ,           " SERIAL
        MODEL    TYPE ZMODEL    ,           " MODEL
        CHAR1    LIKE CABN-ATINN,                           " Code 1
        CHAR2    TYPE CABN-ATINN,                           " Code 2
        CLM1     LIKE MARA-MATNR,                           " COLUMN 1
        ENM1(40) TYPE C         ,           " English Name(Description)
        CLM2     LIKE AUSP-ATWRT,                           " COLUMN 2
        UPG      LIKE AUSP-ATWRT,           " UPG
        ENM2(40) TYPE C         ,           " English Name(Description)
        KNM2     LIKE AUSP-ATWRT,           " Korea Name(Description)
        CODY     LIKE AUSP-ATWRT,           " 219 SELECTION
        CODT     LIKE AUSP-ATWRT,           " 219 SELECTION
        CODP     LIKE AUSP-ATWRT,           " 219 SELECTION
        CODM     LIKE AUSP-ATWRT,           " 219 SELECTION
        CODD     LIKE AUSP-ATWRT,           " 219 SELECTION
        CODS     LIKE AUSP-ATWRT,           " 219 SELECTION
        CODH     LIKE AUSP-ATWRT,           " 219 SELECTION
        CODX     LIKE AUSP-ATWRT,           " 219 SELECTION
        EMER     TYPE C         ,
        WTYP     TYPE C         ,
       END OF IT_3109.


* INPUT VALUE.
DATA : BEGIN OF  ST_3109_INPUT OCCURS  0,
       ORDER     LIKE ZTPP_WOSUM-WO_SER,      " Order number
       TABA      TYPE C         ,             " 219 SELECTION
       TABB      TYPE C         ,             " 219 SELECTION
       COLA(3)   TYPE C         ,             " 219 SELECTION
       COLB(3)   TYPE C         ,             " 219 SELECTION
       UPG(08)   TYPE C         ,             " 219 SELECTION
       TEXTA(40) TYPE C         ,             " 219 SELECTION
       TEXTB(40) TYPE C         ,             " 219 SELECTION
       TEXTU(40) TYPE C         ,             " 219 SELECTION
       CODY      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODT      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODP      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODM      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODD      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODS      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODH      LIKE AUSP-ATWRT,             " 219 SELECTION
       CODX      LIKE AUSP-ATWRT,             " 219 SELECTION

       219_1_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_2_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_3_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_4_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_5_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_6_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_7_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_8_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_9_V     LIKE AUSP-ATWRT,                         " 219 VALUE
       219_10_V    LIKE AUSP-ATWRT,                         " 219 VALUE
END OF ST_3109_INPUT.

* FIELD FOR THE SCREEN 0479 Problem vehicle status
**********
** Internal table   *

DATA : BEGIN OF IT_4279 OCCURS 10,
       BODY     LIKE AUSP-ATWRT, " body number
       STATUS   LIKE AUSP-ATWRT, " status
       ORDER    LIKE AUSP-ATWRT, " ORDER NUM
       ECLR     LIKE AUSP-ATWRT, " external color
       ICLR     LIKE AUSP-ATWRT, " internal color
       MI       LIKE AUSP-ATWRT, " mi
       OCN      LIKE AUSP-ATWRT, " ocn
       VER      LIKE AUSP-ATWRT, " version
       PDATE    LIKE AUSP-ATWRT, " product date
       C_GATE   LIKE AUSP-ATWRT, " c/gaet
       VPC      LIKE AUSP-ATWRT, " vpc
       SHIPOUT LIKE AUSP-ATWRT, " shipout
       END OF IT_4279.

DATA : BEGIN OF ST_4279_INPUT ,
       DURA   TYPE I                   ,          " Duration
       COUNT  TYPE I,
       END OF ST_4279_INPUT.

* FIELD FOR THE SCREEN 5290
**********
** Internal table   *
DATA : BEGIN OF IT_5290 OCCURS 10,
        TYPE(03),  "+00(01) 1 : The Day's Job, 2 : The Night's Job
        TIME(16) TYPE C,
        BI       TYPE I                     ,
        PI       TYPE I                     ,
        TC       TYPE I                     ,
        PO       TYPE I                     ,
        PBSI     TYPE I                     ,
        PBSO     TYPE I                     ,
        TRIM     TYPE I                     ,
        CF       TYPE I                     ,
        PLAN     TYPE P DECIMALS 2          ,
        SOFF     TYPE I                     ,
        RATION   TYPE P DECIMALS 2,
        CG       TYPE I                     ,
        VPCI     TYPE I                     ,
        VPCO     TYPE I                     ,
        SHIPIN   TYPE I                     ,
        SHIPOUT  TYPE I                    ,
      END OF IT_5290 .
DATA: IT_5290_SHIFT LIKE IT_5290 OCCURS 10 WITH HEADER LINE."UD1K912931
DATA: IT_5290_1shift LIKE IT_5290 OCCURS 10 WITH HEADER LINE.
DATA: IT_5290_2shift LIKE IT_5290 OCCURS 10 WITH HEADER LINE.
datA: IT_5290_3shift LIKE IT_5290 OCCURS 10 WITH HEADER LINE.

DATA : BEGIN OF ST_5290_INPUT ,
       PLANT  LIKE AUSP-ATWRT,           " PLANT
       DAY    LIKE AUSP-ATWRT,
       UPH_L  TYPE ZVPP_LD-LRATE,
       UPH_H  TYPE ZVPP_LD-LRATE,
** Furong on 06/12/12 for 3 shift
       UPH_T  TYPE ZVPP_LD-LRATE,
** End on 06/12/12
       LQTY   TYPE I,
       HQTY   TYPE I,
** Furong on 06/12/12 for 3 shift
       tQTY   TYPE I,
** End on 06/12/12
       DAYU   TYPE I            ,
       DATE   LIKE SY-DATUM,
       FTIME  TYPE KAPENDZT,   "FIRST SHIFT START TIME IN SECONDS
       TTIME  TYPE KAPENDZT,   "FIRST SHIFT END TIME IN SECONDS
       BTIME  LIKE SY-UZEIT,   "FIRST SHIFT START TIME
       ETIME  LIKE SY-UZEIT,   "FIRST SHIFT END TIME
       FTIME_2  TYPE KAPENDZT,"SECOND SHIFT START TIME IN SECONDS
       TTIME_2  TYPE KAPENDZT,"SECOND SHIFT END TIME IN SECONDS
       BTIME_2  LIKE SY-UZEIT,"SECOND SHIFT START TIME
       ETIME_2  LIKE SY-UZEIT,"SECOND SHIFT END TIME
** Furong on 06/12/12 for 3 shift
       FTIME_3  TYPE KAPENDZT,"SECOND SHIFT START TIME IN SECONDS
       TTIME_3  TYPE KAPENDZT,"SECOND SHIFT END TIME IN SECONDS
       BTIME_3  LIKE SY-UZEIT,"SECOND SHIFT START TIME
       ETIME_3  LIKE SY-UZEIT,"SECOND SHIFT END TIME
** End on 06/12/12
       TYP      LIKE AUSP-ATWRT,
       END OF ST_5290_INPUT.
DATA:  WA_TOTAL_5290  LIKE IT_5290.
DATA:  BEGIN OF IT_5290TIME OCCURS 13,
        SEQ(2)     TYPE N,
        TIME1      LIKE SY-UZEIT,
        TIME2      LIKE SY-UZEIT,
       END OF IT_5290TIME.
* shift break time
DATA: IT_BREAK  LIKE TC37P OCCURS 0 WITH HEADER LINE.
DATA: WA_ARBPL  LIKE CRHD-ARBPL.
* FIELD FOR THE SCREEN 5291
**********
** Internal table   *
DATA : BEGIN OF IT_5291 OCCURS 10,
       PLANT       LIKE ZTPP_ALCLOG1-PLANT,
       TRIM_P(6)   TYPE P DECIMALS 2,
       TRIM_S      LIKE ZTPP_ALCLOG1-POINT07,
       TRIM_R(4)   TYPE C,
       CFIN_P(6)   TYPE P DECIMALS 2,
       CFIN_S      LIKE ZTPP_ALCLOG1-POINT08,
       CFIN_R(4)   TYPE C,
       SOFF_P(6)   TYPE P DECIMALS 2,
       SOFF_S      LIKE ZTPP_ALCLOG1-POINT08,
       SOFF_R(4)   TYPE C,
       END OF IT_5291.

* FIELD FOR THE SCREEN 5291
**********
** Internal table   *

DATA : BEGIN OF IT_5293 OCCURS 10,
        TIME(16)  TYPE C,
        BI       LIKE ZTPP_ALCLOG2-QTY01_D01,
        PI       LIKE ZTPP_ALCLOG2-QTY01_D02,
        TC       LIKE ZTPP_ALCLOG2-QTY01_D03,
        PO       LIKE ZTPP_ALCLOG2-QTY01_D04,
        PBSI     LIKE ZTPP_ALCLOG2-QTY01_D05,
        TRIM     LIKE ZTPP_ALCLOG2-QTY01_N01,
        CF       LIKE ZTPP_ALCLOG2-QTY01_N02,
        PLAN     LIKE ZTPP_ALCLOG2-QTY01_N03,
        SOFF     LIKE ZTPP_ALCLOG2-QTY01_N04,
        RATION   TYPE P DECIMALS 2,
        CG       LIKE ZTPP_ALCLOG2-QTY01_N05,
        VPI      LIKE ZTPP_ALCLOG2-QTY01_N05,
        VPO     LIKE ZTPP_ALCLOG2-QTY01_N05,
      END OF IT_5293 .

DATA : BEGIN OF ST_5293_INPUT ,
       DATE        LIKE SY-DATUM,
       DAY(1)      TYPE C,  " 1 day 2 night 3 total
       LINE        LIKE ZTPP_ALCLOG2-LINE,
       USE(1)      TYPE C,   " d domastic E export
       RATE(1)     TYPE C,
       UPH(6)      TYPE C,
*
       BI          TYPE P    DECIMALS 2,
       PI          TYPE P    DECIMALS 2,
       PO          TYPE P    DECIMALS 2,
       TRIM        TYPE P    DECIMALS 2,
       SOFF        TYPE P    DECIMALS 2,
       END OF ST_5293_INPUT.

* FIELD FOR THE SCREEN 6299
**********
** Internal table   *
DATA : BEGIN OF IT_6299 OCCURS 0,
       ZPART(06)     TYPE C        ,
       ZCOLUMN(30)   TYPE C        ,
       SERIAL        TYPE I        ,
       ZCOMMENT(30)  TYPE C        ,
       AENAM         LIKE SY-UNAME ,
       END OF IT_6299.

DATA : BEGIN OF ST_6299_INPUT ,
       COUNT     TYPE I,
       END OF ST_6299_INPUT.

CONTROLS TC_APP263 TYPE TABLEVIEW USING SCREEN 2301.
CONTROLS: TC_APP220 TYPE TABLEVIEW USING SCREEN 1202.
CONTROLS TC_APP302 TYPE TABLEVIEW USING SCREEN 4102.
CONTROLS: TC_APP272_01 TYPE TABLEVIEW USING SCREEN 3104.
CONTROLS: TC_APP272_02 TYPE TABLEVIEW USING SCREEN 3111.
CONTROLS: TC_APP272_03 TYPE TABLEVIEW USING SCREEN 3112.
CONTROLS: TC_APP239 TYPE TABLEVIEW USING SCREEN 2113.
CONTROLS: TC_APP240 TYPE TABLEVIEW USING SCREEN 2114.
CONTROLS: TC_APP244 TYPE TABLEVIEW USING SCREEN 2115.
CONTROLS: TC_APP245 TYPE TABLEVIEW USING SCREEN 2116.
CONTROLS TC_APP237  TYPE TABLEVIEW USING SCREEN 2107.
CONTROLS: TC_APP219_01 TYPE TABLEVIEW USING SCREEN 1201.
CONTROLS: TC_APP219_02 TYPE TABLEVIEW USING SCREEN 1201.
CONTROLS: TC_APP236_01 TYPE TABLEVIEW USING SCREEN 2118.
CONTROLS: TC_APP236_02 TYPE TABLEVIEW USING SCREEN 2119.
CONTROLS: TC_APP236_03 TYPE TABLEVIEW USING SCREEN 2120.
CONTROLS: TC_APP236_04 TYPE TABLEVIEW USING SCREEN 2121.
CONTROLS: TC_APP236_05 TYPE TABLEVIEW USING SCREEN 2122.
CONTROLS: TC_APP227 TYPE TABLEVIEW USING SCREEN 1209.
CONTROLS: TC_NEW_APP227 TYPE TABLEVIEW USING SCREEN 1210.
CONTROLS: TC_APP223 TYPE TABLEVIEW USING SCREEN 1205.
CONTROLS: TC_APP223_NEW TYPE TABLEVIEW USING SCREEN 1206.
CONTROLS: TC_APP252 TYPE TABLEVIEW USING SCREEN 2201.
CONTROLS: TC_0101 TYPE TABLEVIEW USING SCREEN 0101,
          TC_0102 TYPE TABLEVIEW USING SCREEN 0102,
*         tc_0103 TYPE TABLEVIEW USING SCREEN 0103,
*         tc_0104 TYPE TABLEVIEW USING SCREEN 0104,
          TC_A105 TYPE TABLEVIEW USING SCREEN 0105,
          TC_B105 TYPE TABLEVIEW USING SCREEN 0105,
          TC_C105 TYPE TABLEVIEW USING SCREEN 0105,
          TC_D105 TYPE TABLEVIEW USING SCREEN 0105,
          TC_A106 TYPE TABLEVIEW USING SCREEN 0106,
          TC_B106 TYPE TABLEVIEW USING SCREEN 0106,
          TC_APP250 TYPE TABLEVIEW USING SCREEN 2200,
          TC_C106 TYPE TABLEVIEW USING SCREEN 0106,
          TC_D106 TYPE TABLEVIEW USING SCREEN 0106,
          TC_0107 TYPE TABLEVIEW USING SCREEN 0107,
          TC_A107 TYPE TABLEVIEW USING SCREEN 0107,
          TC_0108 TYPE TABLEVIEW USING SCREEN 0108,
          TC_0109 TYPE TABLEVIEW USING SCREEN 0109,
          TC_0110 TYPE TABLEVIEW USING SCREEN 0110,
          TC_0111 TYPE TABLEVIEW USING SCREEN 0111,
          TC_0111_C TYPE TABLEVIEW USING SCREEN 0111,
          TC_0112 TYPE TABLEVIEW USING SCREEN 0112,
          TC_0113 TYPE TABLEVIEW USING SCREEN 0113,
          TC_0114 TYPE TABLEVIEW USING SCREEN 0114,
          TC_0115 TYPE TABLEVIEW USING SCREEN 0115,
          TC_0116 TYPE TABLEVIEW USING SCREEN 0116,
          TC_0117 TYPE TABLEVIEW USING SCREEN 0117,
          TC_0118 TYPE TABLEVIEW USING SCREEN 0118,
*          tc_0119 TYPE TABLEVIEW USING SCREEN 0119,
          TC_0120 TYPE TABLEVIEW USING SCREEN 0120,
          TC_0201 TYPE TABLEVIEW USING SCREEN 0201,
          TC_0202 TYPE TABLEVIEW USING SCREEN 0202,
          TC_0203 TYPE TABLEVIEW USING SCREEN 0203,
          TC_0204 TYPE TABLEVIEW USING SCREEN 0204,
          TC_0205 TYPE TABLEVIEW USING SCREEN 0205,
          TC_0206 TYPE TABLEVIEW USING SCREEN 0206,
          TC_0207 TYPE TABLEVIEW USING SCREEN 0207,
          TC_0208 TYPE TABLEVIEW USING SCREEN 0208,
          TC_0209 TYPE TABLEVIEW USING SCREEN 0209,
          TC_0210 TYPE TABLEVIEW USING SCREEN 0210,
          TC_0211 TYPE TABLEVIEW USING SCREEN 0211,
          TC_0212 TYPE TABLEVIEW USING SCREEN 0212,
          TC_0213 TYPE TABLEVIEW USING SCREEN 0213,
          TC_0214 TYPE TABLEVIEW USING SCREEN 0214,
          TC_0215 TYPE TABLEVIEW USING SCREEN 0215,
          TC_0216 TYPE TABLEVIEW USING SCREEN 0216,
          TC_0217 TYPE TABLEVIEW USING SCREEN 0217,
          TC_0218 TYPE TABLEVIEW USING SCREEN 0218,
          TC_0219 TYPE TABLEVIEW USING SCREEN 0219,
          TC_0220 TYPE TABLEVIEW USING SCREEN 0220,
          TC_0301 TYPE TABLEVIEW USING SCREEN 0301,
          TC_0302 TYPE TABLEVIEW USING SCREEN 0302,
          TC_0303 TYPE TABLEVIEW USING SCREEN 0303,
          TC_0304 TYPE TABLEVIEW USING SCREEN 0304,
          TC_0305 TYPE TABLEVIEW USING SCREEN 0305,
          TC_0306 TYPE TABLEVIEW USING SCREEN 0306,
          TC_0307 TYPE TABLEVIEW USING SCREEN 0307,
          TC_0308 TYPE TABLEVIEW USING SCREEN 0308,
          TC_0309 TYPE TABLEVIEW USING SCREEN 0309,
          TC_0310 TYPE TABLEVIEW USING SCREEN 0310,
          TC_0311 TYPE TABLEVIEW USING SCREEN 0311,
          TC_0312 TYPE TABLEVIEW USING SCREEN 0312,
          TC_0313 TYPE TABLEVIEW USING SCREEN 0313,
          TC_0314 TYPE TABLEVIEW USING SCREEN 0314,
          TC_0315 TYPE TABLEVIEW USING SCREEN 0315,
          TC_0316 TYPE TABLEVIEW USING SCREEN 0316,
          TC_3107 TYPE TABLEVIEW USING SCREEN 3107,
          TC_3109 TYPE TABLEVIEW USING SCREEN 3109,
          TC_0318 TYPE TABLEVIEW USING SCREEN 0318,
          TC_0319 TYPE TABLEVIEW USING SCREEN 0319,
          TC_0320 TYPE TABLEVIEW USING SCREEN 0320,
          TC_0401 TYPE TABLEVIEW USING SCREEN 0401,
          TC_0402 TYPE TABLEVIEW USING SCREEN 0402,
          TC_0403 TYPE TABLEVIEW USING SCREEN 0403,
          TC_0404 TYPE TABLEVIEW USING SCREEN 0404,
          TC_0405 TYPE TABLEVIEW USING SCREEN 0405,
          TC_0406 TYPE TABLEVIEW USING SCREEN 0406,
          TC_0407 TYPE TABLEVIEW USING SCREEN 0407,
          TC_0408 TYPE TABLEVIEW USING SCREEN 0408,
          TC_0409 TYPE TABLEVIEW USING SCREEN 0409,
          TC_0410 TYPE TABLEVIEW USING SCREEN 0410,
          TC_0411 TYPE TABLEVIEW USING SCREEN 0411,
          TC_0412 TYPE TABLEVIEW USING SCREEN 0412,
          TC_0413 TYPE TABLEVIEW USING SCREEN 0413,
          TC_0414 TYPE TABLEVIEW USING SCREEN 0414,
          TC_0415 TYPE TABLEVIEW USING SCREEN 0415,
          TC_0416 TYPE TABLEVIEW USING SCREEN 0416,
          TC_0417 TYPE TABLEVIEW USING SCREEN 0417,
          TC_0418 TYPE TABLEVIEW USING SCREEN 0418,
          TC_0419 TYPE TABLEVIEW USING SCREEN 0419,
          TC_0420 TYPE TABLEVIEW USING SCREEN 0420,
          TC_0501 TYPE TABLEVIEW USING SCREEN 0501,
          TC_0502 TYPE TABLEVIEW USING SCREEN 0502,
          TC_0503 TYPE TABLEVIEW USING SCREEN 0503,
          TC_0504 TYPE TABLEVIEW USING SCREEN 0504,
          TC_0505 TYPE TABLEVIEW USING SCREEN 0505,
          TC_0506 TYPE TABLEVIEW USING SCREEN 0506,
          TC_0507 TYPE TABLEVIEW USING SCREEN 0507,
          TC_0508 TYPE TABLEVIEW USING SCREEN 0508,
          TC_0509 TYPE TABLEVIEW USING SCREEN 0509,
          TC_0510 TYPE TABLEVIEW USING SCREEN 0510,
          TC_0511 TYPE TABLEVIEW USING SCREEN 0511,
          TC_0512 TYPE TABLEVIEW USING SCREEN 0512,
          TC_0513 TYPE TABLEVIEW USING SCREEN 0513,
          TC_0514 TYPE TABLEVIEW USING SCREEN 0514,
          TC_0515 TYPE TABLEVIEW USING SCREEN 0515,
          TC_0516 TYPE TABLEVIEW USING SCREEN 0516,
          TC_0517 TYPE TABLEVIEW USING SCREEN 0517,
          TC_0518 TYPE TABLEVIEW USING SCREEN 0518,
          TC_0519 TYPE TABLEVIEW USING SCREEN 0519,
          TC_0520 TYPE TABLEVIEW USING SCREEN 0520,
          TC_0601 TYPE TABLEVIEW USING SCREEN 0601,
          TC_0602 TYPE TABLEVIEW USING SCREEN 0602,
          TC_0603 TYPE TABLEVIEW USING SCREEN 0603,
          TC_0604 TYPE TABLEVIEW USING SCREEN 0604,
          TC_0605 TYPE TABLEVIEW USING SCREEN 0605,
          TC_0606 TYPE TABLEVIEW USING SCREEN 0606,
          TC_0607 TYPE TABLEVIEW USING SCREEN 0607,
          TC_0608 TYPE TABLEVIEW USING SCREEN 0608,
          TC_0609 TYPE TABLEVIEW USING SCREEN 0609,
          TC_0610 TYPE TABLEVIEW USING SCREEN 0610,
          TC_0611 TYPE TABLEVIEW USING SCREEN 0611,
          TC_0612 TYPE TABLEVIEW USING SCREEN 0612,
          TC_0613 TYPE TABLEVIEW USING SCREEN 0613,
          TC_0614 TYPE TABLEVIEW USING SCREEN 0614,
          TC_0615 TYPE TABLEVIEW USING SCREEN 0615,
          TC_0616 TYPE TABLEVIEW USING SCREEN 0616,
          TC_0617 TYPE TABLEVIEW USING SCREEN 0617,
          TC_0618 TYPE TABLEVIEW USING SCREEN 0618,
          TC_0619 TYPE TABLEVIEW USING SCREEN 0619,
          TC_0620 TYPE TABLEVIEW USING SCREEN 0620,

          TC_2101 TYPE TABLEVIEW USING SCREEN 2101,
          TC_2102 TYPE TABLEVIEW USING SCREEN 2102,
          TC_2103 TYPE TABLEVIEW USING SCREEN 2103,
          TC_2104 TYPE TABLEVIEW USING SCREEN 2104,
          TC_2105 TYPE TABLEVIEW USING SCREEN 2105,
          TC_2202 TYPE TABLEVIEW USING SCREEN 2202,
          TC_2205 TYPE TABLEVIEW USING SCREEN 2205,
          TC_2206 TYPE TABLEVIEW USING SCREEN 2206,
          TC_4279 TYPE TABLEVIEW USING SCREEN 3211, " 4279 -> 3211
          TC_5290 TYPE TABLEVIEW USING SCREEN 3301, " 5290 -> 3301
          TC_5291 TYPE TABLEVIEW USING SCREEN 3302, " 5291 -> 3302
          TC_5293 TYPE TABLEVIEW USING SCREEN 3303, " 5293 -> 3303
          TC_6299 TYPE TABLEVIEW USING SCREEN 4103. " 6299 -> 4103

*** Start of Definition For APP250 ***
DATA: BEGIN OF IT_APP250 OCCURS 0.
        INCLUDE STRUCTURE ZTPP_CHANGE.
DATA: ORDERNO(20),
      END OF IT_APP250.

DATA: BEGIN OF IT_EXCEL_2200 OCCURS 0,
        COL01(20),
        COL02(20),
        COL03(20),
        COL04(20),
        COL05(20),
        COL06(20),
        COL07(20),
        COL08(20),
        COL09(20),
        COL10(20),
        COL11(20),
      END OF IT_EXCEL_2200.

RANGES: R_CDATE   FOR  SY-DATUM.
DATA: WA_CDATE_ST LIKE SY-DATUM,
      WA_CDATE_EN LIKE SY-DATUM,
      SAVE_OK     TYPE SY-UCOMM.
*** End of Definition For APP250 **************************

*** Start of Definition For APP252 ***

DATA: BEGIN OF IT_APP252 OCCURS 0.
        INCLUDE STRUCTURE ZTPP_CHANGE.
DATA: ORDERNO(20).
DATA: END OF IT_APP252.

DATA: BEGIN OF IT_EXCEL_2201 OCCURS 0,
        COL01(20),  "Serial
        COL02(20),  "Body_NO
        COL03(20),  "Current RP
        COL04(20),  "VIN
        COL05(20),  "ORDER_NO(ORDNO + NATION + DEALER)
        COL06(20),  "Ext.Color
        COL07(20),  "Int.Color
        COL08(20),  "Spec
        COL09(20),  "OCN
        COL10(20),  "Date
        COL11(20),  "Bef.OrdNo
        COL12(20),  "Bef.Nation
        COL13(20),  "Bef.Dealer
        COL14(20),  "Bef.Ext.Color
        COL15(20),  "Bef.Ext.Color
        COL16(20),  "Bef.Spec
        COL17(20),  "Bef.OCN
        COL18(20),  "Bef.VIN
      END OF IT_EXCEL_2201.

DATA: BEGIN OF IT_EXCEL_8081 OCCURS 0,
      COL01(6), "  nation
      COL02(10), " order
      COL24(18), " fsc
      COL03(5), " extc
      COL04(5), " intc
      COL05(8), " modqty
      COL06(8), " seq
      COL07(8), " bin
      COL08(8), " tin
      COL09(8), " soff
      COL10(8), " cgate
*      col11(8), " shipin
      COL11(8), " shipout
      COL12(8), " NEWQTYc
      COL13(8),                                             " NEWQTY01
      COL14(8),                                             " NEWQTY02
      COL15(8),                                             " NEWQTY03
      COL16(8),                                             " NEWQTY04
      COL17(8),                                             " NEWQTY05
      COL18(8),                                             " NEWQTY06
      COL19(8),                                             " NEWQTY07
      COL20(8),                                             " NEWQTY08
      COL21(8),                                             " NEWQTY09
      COL22(8),                                             " NEWQTY10
      COL23(8),                                             " NEWQTY11
      END OF IT_EXCEL_8081.

DATA: BEGIN OF IT_EXCEL_8082 OCCURS 0,
      COL01(6), "  nation
      COL02(10), " order
      COL24(18), " fsc
      COL03(5), " extc
      COL04(5), " intc
      COL05(8), " modqty
      COL06(8), " seq
      COL07(8), " bin
      COL08(8), " tin
      COL09(8), " soff
      COL10(8), " cgate
*      col11(8), " shipin
      COL11(8), " shipout
      COL12(8), " NEWQTYc
      COL13(8),                                             " NEWQTY01
      COL14(8),                                             " NEWQTY02
      COL15(8),                                             " NEWQTY03
      COL16(8),                                             " NEWQTY04
      COL17(8),                                             " NEWQTY05
      COL18(8),                                             " NEWQTY06
      COL19(8),                                             " NEWQTY07
      COL20(8),                                             " NEWQTY08
      COL21(8),                                             " NEWQTY09
      COL22(8),                                             " NEWQTY10
      COL23(8),                                             " NEWQTY11
      END OF IT_EXCEL_8082.

* Parameters
DATA: P_YYYYMM TYPE S021-SPMON.  "Center_YearMonth
*** End of Definition For APP252 **********************

*** Start of Definition For APP223 ***
*     Descriptions
DATA: BEGIN OF WA_DESCRIPTIONS OCCURS 1.
        INCLUDE STRUCTURE VTDESCR.
DATA: END OF WA_DESCRIPTIONS.
*     Characteristics
DATA: BEGIN OF IT_COLUMN OCCURS 0.
        INCLUDE STRUCTURE VTCHARS.
DATA: END OF IT_COLUMN.

DATA: BEGIN OF IT_VTENTRIES OCCURS 0.
*.      Maintain Variant Table
        INCLUDE STRUCTURE VTENTRIES.
DATA:   COLUMN TYPE VTENTRIES-VTLINNOINT ,
        COL_NAME TYPE VTENTRIES-VTCHARACT .
DATA: END OF IT_VTENTRIES.
*
*
DATA: BEGIN OF IT_TABLE_HEADER OCCURS 0.
        INCLUDE STRUCTURE CUVTAB.
DATA: END OF IT_TABLE_HEADER.
DATA: BEGIN OF IT_CUVTLN OCCURS 0.
        INCLUDE STRUCTURE CUVTLN.
DATA: END OF IT_CUVTLN.
DATA: BEGIN OF IT_LINES_OLD OCCURS 0.
        INCLUDE STRUCTURE CUVTLN.
DATA: END OF IT_LINES_OLD.

DATA: WA_LINES_1205 LIKE CUVTLN.
DATA: BEGIN OF IT_LINES_NEW OCCURS 0.
        INCLUDE STRUCTURE CUVTLN.
DATA: END OF IT_LINES_NEW.

DATA: BEGIN OF IT_VALUES_C_OLD OCCURS 0.
        INCLUDE STRUCTURE CUVTAB_VALC.
DATA: END OF IT_VALUES_C_OLD.

DATA: BEGIN OF IT_VALUES_C_NEW OCCURS 0.
        INCLUDE STRUCTURE CUVTAB_VALC.
DATA: END OF IT_VALUES_C_NEW.

DATA: BEGIN OF IT_VALUES_N_OLD OCCURS 0.
        INCLUDE STRUCTURE CUVTAB_VALN.
DATA: END OF IT_VALUES_N_OLD.

DATA: BEGIN OF IT_VALUES_N_NEW OCCURS 0.
        INCLUDE STRUCTURE CUVTAB_VALN.
DATA: END OF IT_VALUES_N_NEW.
*
*
TYPES: BEGIN OF ST_APP223,
*.      FIELDS TO BE DISPLAYED
         MARK,
         LINE TYPE VTENTRIES-VTLINENO,
         CODE(04),            " Code
         DATE(10),            " Date
         CON_COL(25),         " Concatenated Keys
         COL_01(04),          " 1st Column
         COL_02(04),          " 2nd Column
         COL_03(04),          " 3rd Column
         COL_04(04),
         COL_05(04),
         COL_06(04),
         COL_07(04),
         COL_08(04),
         COL_09(04),
         COL_10(04),
         COL_11(04),
         COL_12(04),
         COL_13(04),
         COL_14(04),
         COL_15(04),
         COL_16(04),
         COL_17(04),
         COL_18(04),
         COL_19(04),
         COL_20(04),
         COL_21(04),
         COL_22(04),
         COL_23(04),
         COL_24(04),
         COL_25(04),
         USER(12),
      END OF ST_APP223,
      WA_APP223 TYPE ST_APP223.
DATA: IT_APP223 TYPE ST_APP223 OCCURS 0 WITH HEADER LINE.
DATA: IT_APP223_NEW TYPE ST_APP223 OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_EXCEL_1205 OCCURS 0,
        CODE(10),      " Code
        CON_COL(20),   " Concatenated Key value.
        DATE(10),      " Date when data was changed
        COL_01(15),    " 1st Column
        COL_02(15),    " 2nd Column
        COL_03(15),    " 3rd Column
        COL_04(15),
        COL_05(15),
        COL_06(15),
        COL_07(15),
        COL_08(15),
        COL_09(15),
        COL_10(15),
        COL_11(15),
        COL_12(15),
        COL_13(15),
        COL_14(15),
        COL_15(15),
        COL_16(15),
        COL_17(15),
        COL_18(15),
        COL_19(15),
        COL_20(15),
      END OF IT_EXCEL_1205.

*. PARAMETERS
DATA: " p_company(04),
      P_PATH(20),
      " p_model(03),  "(MODEL (EM(SONATA), CM(SANTAFE)))
      P_PART(01),  "UNIQUE OR COLOR PART OF THE ALC CODE
      P_KEY(03),                                            "(1 ~ 200)
      P_KEY_01(03),
      P_KEY_02(03),
      P_KEY_03(03),
      P_KEY_04(03),
      P_KEY_05(03),
      P_KEY_06(03),
      P_KEY_07(03),
      P_KEY_08(03),
      P_KEY_09(03),
      P_KEY_10(03),
      P_KEY_11(03),
      P_KEY_12(03),
      P_KEY_13(03),
      P_KEY_14(03),
      P_KEY_15(03),
      P_KEY_16(03),
      P_KEY_17(03),
      P_KEY_18(03),
      P_KEY_19(03),
      P_KEY_20(03),
      P_COLUMN(10),
      P_COL_NAME(10),
      P_SORT_SEQ,
      P_CODE(04),
      P_CODE_CHG(04).

*Full Code(ALC) = p_model+'_ALC_'+p_part+'_'+p_key.
DATA: P_FULL_CODE LIKE TABLSTRUCT-VAR_TAB.


* DROPDOWN LIST for Parameter

** P_MODEL(Model)
*DATA: name        TYPE vrm_id,
*      XLIST  TYPE vrm_values,
*      XVALUE LIKE LINE OF XLIST.
*RANGES: r_model FOR ztpp_veh_model-model.

** P_PART(U or C of ALC)
*data: XLIST type vrm_values,
*      XVALUE like line of XLIST.
RANGES: R_PART FOR P_PART.
** P_KEY(1, ... , 200)
*data: XLIST type vrm_values,
*      XVALUE like line of XLIST.
RANGES: R_KEY FOR P_KEY.

*For batch uploading
DATA: P_F_NAME  LIKE  RLGRAP-FILENAME ,
      P_F_TYPE    LIKE  RLGRAP-FILETYPE .

DATA  WA_INIT_1205.
DATA  WA_UPD_1205.
*** End of Definition For APP223 ****************************

*** Start of Definition For APP227 ***
DATA: BEGIN OF IT_APP227 OCCURS 0.
        INCLUDE STRUCTURE ZTPP_SPEC.
DATA: CHECK.
DATA: END OF IT_APP227.
*
DATA: BEGIN OF IT_EXCEL_1209 OCCURS 0,
        KEYCODE(20),  "KEY CODE
        OPDATE(20),  "OPERATION DATE
        OPCOUNT(20),  "OPERATION COUNT
        MARK(20),  "MARKING
        PLANT(20),  "PLANT
        MODEL(20),  "MODEL CODE
        WORDER(20),  "WORK ORDER
        EXTC(20),  "EXTERNAL COLOR
        INTC(20),  "INTERNAL COLOR
        ERDAT(20),  "LAST CHANGED ON
        ERZET(20),  "TIME LAST CHANGE WAS MADE
        ERNAM(20),  "NAME OF PERSON WHO CHANGED OBJECT
      END OF IT_EXCEL_1209.

*
DATA: BEGIN OF IT_NEW_APP227 OCCURS 0,
        KEYCODE TYPE ZTPP_SPEC-KEYCODE,
        OPDATE TYPE ZTPP_SPEC-OPDATE,
        OPCOUNT TYPE ZTPP_SPEC-OPCOUNT,
        MARK TYPE ZTPP_SPEC-MARK,
*
        FORDER TYPE MARA-MATNR,
*
        WORDER TYPE ZTPP_SPEC-WORDER,
        EXTC TYPE ZTPP_SPEC-EXTC,
        INTC TYPE ZTPP_SPEC-INTC,
*
        ZUSER TYPE ZTPP_SPEC-ZUSER,
        ERDAT TYPE ZTPP_SPEC-ERDAT,
        ERZET TYPE ZTPP_SPEC-ERZET,
        ERNAM TYPE ZTPP_SPEC-ERNAM,
        AEDAT TYPE ZTPP_SPEC-AEDAT,
        AEZET TYPE ZTPP_SPEC-AEZET,
        AENAM TYPE ZTPP_SPEC-AENAM,
*
        CHECK,
*
      END OF IT_NEW_APP227.
*
DATA: BEGIN OF IT_ERROR_1210 OCCURS 0,
        MATNR TYPE MARA-MATNR,
        FORDER TYPE MARA-MATNR,
        WORDER TYPE ZTPP_SPEC-WORDER,
        EXTC TYPE ZTPP_SPEC-EXTC,
        INTC TYPE ZTPP_SPEC-INTC,
      END OF IT_ERROR_1210.
*type-pools: vrm.
*
* PARAMETERS  --- SCREEN 1210
DATA: P_OPDATE TYPE ZTPP_SPEC-OPDATE,
      P_OPCOUNT TYPE ZTPP_SPEC-OPCOUNT,
      P_WORDER TYPE ZTPP_SPEC-WORDER,
      P_TOT_COUNT(04) TYPE N.
* PARAMETERS --- SCREEN 1210
DATA: P_KEYCODE TYPE ZTPP_SPEC-KEYCODE,
      P_ERDAT LIKE SY-DATUM,
      P_ERZET LIKE SY-UZEIT,
      P_ERNAM LIKE SY-UNAME.

* internal table for export
DATA: BEGIN OF IT_SPEC OCCURS 0,
        PLANT TYPE ZTPP_SPEC-PLANT,
        OPDATE TYPE ZTPP_SPEC-OPDATE,
        OPCOUNT TYPE ZTPP_SPEC-OPCOUNT,
        WORDER TYPE ZTPP_SPEC-WORDER,
        EXTC TYPE ZTPP_SPEC-EXTC,
        INTC TYPE ZTPP_SPEC-INTC,
      END OF IT_SPEC.

* DROPDOWN LIST
** P_PLANT
*data: " name        type vrm_id,
*      xlist   type vrm_values,
*      xvalue  like line of xlist.
RANGES: R_PLANT FOR ZTPP_SPEC-PLANT.
**     P_MODEL
DATA: " XLIST type vrm_values,
*      " XVALUE like line of XLIST,
*     P_EXTC
      P_EXTC(03),
*     P_INTC
      P_INTC(03).
*ranges: r_model for ztpp_spec-model.

*   ON VALUE-REQUEST
DATA: BEGIN OF REASON_TAB OCCURS 0,
        CODE   LIKE MARA-MATNR,
*        text   LIKE lfa1-name1,
      END OF REASON_TAB.
DATA: BEGIN OF DYNPFIELDS OCCURS 3.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.
*** End of Definition For APP227 *************************

*** Start of Definition For APP237 ***
* inquiry Key
TABLES: ZSPP_APP237.
* inquiry Key Save
DATA: BEGIN OF  SV_KEY,
        MODEL  LIKE  ZSPP_APP237-MODEL,
        GUBUN  LIKE  ZSPP_APP237-GUBUN.
DATA  END OF SV_KEY.

DATA: BEGIN OF IT_EQUNR_2107  OCCURS  0,
        OBJEK  LIKE  EQUI-EQUNR,
      END OF IT_EQUNR_2107.

* - vehicle master charateristics
DATA  BEGIN OF IT_VMV_2107  OCCURS  0.
        INCLUDE STRUCTURE ZSPP_VIN_VALUE.
DATA  END OF IT_VMV_2107.
* - Input Parameters
DATA: BEGIN OF ST_APP237,
        PLANT(05),
        INQS(02),
        DDAY TYPE I,
        BODYNO(06),
        CAUNT TYPE I,
      END OF ST_APP237.

DATA: BEGIN  OF IT_DLY_2107  OCCURS 0,
        BODYNO(10),
        WON(14),
        EXTC    LIKE  ZTPP_WOSUM-EXTC,
        INTC    LIKE  ZTPP_WOSUM-INTC,
        BODYIN  LIKE  SY-DATUM,
        PAINTIN LIKE  SY-DATUM,
        TRIMIN  LIKE  SY-DATUM,
        CFINAL  LIKE  SY-DATUM,
        SOFF    LIKE  SY-DATUM,
        CGATE   LIKE  SY-DATUM,
** Added by furong on 05/19/09
        VPCOUT  LIKE  SY-DATUM,
** End of addition
        DDAY    TYPE  I,
        MODEL   LIKE  AUSP-ATWRT,
      END  OF IT_DLY_2107.
DATA  IT_DLS_2107  LIKE  IT_DLY_2107 OCCURS 0 WITH HEADER LINE.
DATA  IT_EXCEL_2107  LIKE  IT_DLY_2107 OCCURS 0 WITH HEADER LINE.
*
DATA: P_EQUNR_2107    LIKE  EQUI-EQUNR.
DATA  P_STATUS_2107   LIKE  AUSP-ATINN.
DATA  P_ATWRT_2107    LIKE  AUSP-ATWRT.
DATA: WA_MODEL_2107    LIKE  AUSP-ATWRT,
      WA_SERIAL_2107   LIKE  AUSP-ATWRT.
DATA: WA_VMV_LINES_2107  TYPE  I,
      WA_DLY_LINES_2107  TYPE  I.
*** End of Definition For APP237 *****************************


*** Start of Definition For APP221 ***
DATA: BEGIN OF IT_APP221  OCCURS 0,
        SEQ    TYPE I.
INCLUDE   STRUCTURE  ZTPP_NATION_DEF.
DATA  END OF IT_APP221.

DATA: BEGIN OF IS_APP221.
INCLUDE   STRUCTURE  ZTPP_NATION_DEF.
DATA  END OF IS_APP221.
*
DATA: BEGIN OF  IT_FUNC_1203  OCCURS 0,   "GUI tool bar control
         FCODE  LIKE  RSMPE-FUNC,
      END   OF  IT_FUNC_1203.
*
DATA: IT_ZSPPVN1 LIKE ZSPPVN1 OCCURS 0 WITH HEADER LINE.

DATA: G_SEQ_1203  TYPE  I,
      C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination
*** End of Definition For APP221 ***********************************


*** Start of Definition For APP219 ***
DATA: BEGIN OF IT_APP219  OCCURS 0.             " from ztbm_219_desc
DATA:   MODEL TYPE ZTPP_VEH_MODEL-MODEL,   "Model
        NAME_219 TYPE ZTBM_ABXOPVDT-CLNO,  "Column No.
        DESC_219 TYPE ZTBM_ABXOPVDT-CLNM.  "Column Name
        INCLUDE STRUCTURE  ZTBM_ABXOPVDT.
DATA: END OF  IT_APP219.
*
* 219 option table control screen display
DATA: BEGIN OF IT_OPT1_APP219  OCCURS  0.
DATA:   MODEL TYPE ZTPP_VEH_MODEL-MODEL,   "Model
        NAME_219 TYPE ZTBM_ABXOPVDT-CLNO,  "Column No.
        DESC_219 TYPE ZTBM_ABXOPVDT-CLNM.  "Column Name
        INCLUDE STRUCTURE  ZTBM_ABXOPVDT.
DATA: END OF IT_OPT1_APP219.

DATA: BEGIN OF IT_OPT2_APP219  OCCURS  0,
        MODEL    LIKE  ZTBM_OPTION_ITEM-MODEL,
        ZCOLUMN  LIKE  ZTBM_OPTION_ITEM-ZCOLUMN,
        ZCOMMENT LIKE  ZTBM_OPTION_ITEM-ZCOMMENT,
      END OF IT_OPT2_APP219.
*
DATA: G_IT_LINE_APP219    TYPE  I,  "internal table lines
      G_OPT1_LINES_APP219 TYPE  I,
      G_OPT2_LINES_APP219 TYPE  I.
*** End   of Definition For APP219 ************************

*** Start of Definition For APP236 ***
DATA  BEGIN OF IT_VMV_APP236  OCCURS  0.
        INCLUDE STRUCTURE ZSPP_VIN_VALUE.
DATA  END OF IT_VMV_APP236.
* -
DATA: BEGIN OF ST_APP236 ,
        P_PAINT(10),
        L_PAINT(8),
        P_BODY(10),
        L_BODY(8) ,
        P_TRIM(10),
        L_TRIM(8) ,
        P_MGATE(10),
        L_MGATE(8) ,
        USAGE(15).
        INCLUDE STRUCTURE ZSPP_VM_VALUE.
DATA  END OF ST_APP236.
* ------------------------------------------------
DATA: BEGIN OF IT_WIP_APP236  OCCURS 0,
        PROGRESS(2)  TYPE  N,
        SHOP_DAT     LIKE  SY-DATUM,
        ACT_DAT(12)  TYPE  C,
        SERIAL(5)    TYPE  C,
      END OF IT_WIP_APP236.
DATA: G_SHOP_DATE_APP236(20),
      G_SERIAL_APP236(20),
      G_ACT_DATE_APP236(20).
* Engine dupplicate vehicle
DATA: BEGIN OF IT_ENG_APP236  OCCURS 0,
        OBJEK  LIKE  AUSP-OBJEK,
        ENGNO(15),
      END OF IT_ENG_APP236.
* WORK ORDER V/C DATA
DATA  G_CUOBF_APP236   LIKE  EQUI-EQUNR.
*DATA  G_CUOBF_APP236   LIKE  mara-cuobf.  "INSTANCE
DATA  IT_WO_APP236     LIKE  TABLE OF ZSPP_VIN_VALUE   WITH HEADER LINE.
* 219 option values table
DATA: BEGIN OF IT_219_APP236  OCCURS 0,
        CLNO  LIKE  ZTBM_ABXOPVDT-CLNO,
        VALU  LIKE  ZTBM_ABXOPVDT-VALU,
        VANM  LIKE  ZTBM_ABXOPVDT-VANM,
        CLNM  LIKE  ZTBM_ABXOPVDT-CLNM,
*        col  like  ztbm_219_value-serial,
*        val  like  ztbm_219_value-value,
*        valtx like ztbm_219_value-code_name1,
*        coltx like ztbm_219_desc-desc_219,
      END OF IT_219_APP236.
* Order table ; Unique Part
DATA: BEGIN OF IT_PART_APP236  OCCURS 0,
        COL  LIKE  ZTBM_ABXOPVDT-CLNO,
*        col        like  ztbm_219_value-serial,
        CODE(10)   TYPE  C,
        CODETX(40) TYPE  C,
      END OF IT_PART_APP236.
*
DATA: BEGIN OF IT_UPART_APP236  OCCURS 0,
        COL  LIKE  ZTBM_ABXOPVDT-CLNO,
*        col        like  ztbm_219_value-serial,
        CODE(10)   TYPE  C,
        CODETX(40) TYPE  C,
      END OF IT_UPART_APP236.
DATA: BEGIN OF IT_CPART_APP236  OCCURS 0,
        COL  LIKE  ZTBM_ABXOPVDT-CLNO,
*        col        like  ztbm_219_value-serial,
        CODE(10)   TYPE  C,
        CODETX(40) TYPE  C,
      END OF IT_CPART_APP236.

* Order table ; Unique Part
DATA: BEGIN OF IT_UCPART_APP236  OCCURS 0,
        UCGUB(1),
        COL  LIKE  ZTBM_ABXOPVDT-CLNO,
*        col        like  ztbm_219_value-serial,
        CODE(10)   TYPE  C,
        CODETX(40) TYPE  C,
      END OF IT_UCPART_APP236.

* table for Airbag list
DATA: BEGIN OF IT_ABAG_APP236  OCCURS 0,
        AIRBAG(30)   TYPE  C,
        CODE(30)     TYPE  C,
      END OF IT_ABAG_APP236.
* table for RP list
DATA: BEGIN OF IT_RP_APP236 OCCURS 0,
        SH_NAME  TYPE CABN-ATNAM,
        SH_DATE  TYPE AUSP-ATWRT,
        SER_NAME TYPE CABN-ATNAM,
        SER_NUM  TYPE AUSP-ATWRT,
      END OF IT_RP_APP236.

* LIST BOX creation
DATA: LIST_APP236  TYPE VRM_VALUES,
      VALUE_APP236 LIKE LINE OF LIST_APP236.
* GUI Icon control table
DATA: BEGIN OF  IT_FUNC_APP236  OCCURS 0,   "GUI tool bar control
         FCODE  LIKE  RSMPE-FUNC,
      END   OF  IT_FUNC_APP236.
DATA: BEGIN OF ST_KEY_APP236,
       INQOPT(15),
      END OF ST_KEY_APP236.
* undefine screen field
DATA: BEGIN OF ST_ISS_APP236,
       INQOPT(15),
       EQKTX    LIKE  EQKT-EQKTX,
       DUPENG1(15), DUPENG2(15), DUPENG3(15), DUPENG4(15),
       REPORTNO(20),
       APPROVAL(20),
       TCTX(30),               "Problem contents
       APPDAT   LIKE AUSP-ATWRT,
       CCDAT    LIKE AUSP-ATWRT,
       LIFNR    LIKE LFA1-LIFNR,
       LIFNM    LIKE LFA1-NAME1,
      END OF ST_ISS_APP236.
*data: begin of st_code_app236,
*       inqopt  like  st_key_app236-inqopt,
*       MODEL   LIKE  St_app236-MODEL ,
*       bodyno  like  st_app236-bodyno,
*       vin     like  st_app236-vin,
*       engno   like  st_app236-engno,
*       tmno    like  st_app236-tmno,
*     end of st_code_app236.

DATA: P_BODY01_APP236   TYPE  I,
      P_BODY02_APP236   TYPE  I,
      P_PAINT01_APP236  TYPE  I,
      P_PAINT02_APP236  TYPE  I,
      P_TRIM01_APP236   TYPE  C,
      P_TRIM02_APP236   TYPE  C.
*
DATA: G_EQUNR_APP236    LIKE  EQUI-EQUNR,
*     g_equichk_app236  type  c.
      G_CRSR_FLD_APP236(20).
DATA: G_ATTR_APP236(1).
DATA: G_VIN_APP236    LIKE  AUSP-ATINN,
      G_ENGNO_APP236  LIKE  AUSP-ATINN,
      G_TMNO_APP236   LIKE  AUSP-ATINN.
DATA: G_PART_APP236(1),      "Unique, Color
      G_PARTTIT_APP236(30).
DATA: IT_LINES_APP236  TYPE  I.
DATA: WIP_LINES TYPE  I.

* DATA FOR TABSTRIP 'SS2106'
CONTROLS:  SS2106 TYPE TABSTRIP.
DATA:      BEGIN OF G_SS2106,
             SUBSCREEN   LIKE SY-DYNNR,
             PROG        LIKE SY-REPID VALUE 'SAPMZPP_APPLICATION',
             PRESSED_TAB LIKE SY-UCOMM VALUE C_SS2106-TAB1,
           END OF G_SS2106.

*** End   of Definition For APP236 *********************************


*** Start of Definitions For APP246 ***
DATA : BEGIN OF IT_SUM_APP246 OCCURS 0.
        INCLUDE STRUCTURE ZSPP_SUM_APP246.
DATA :   OBJEK TYPE AUSP-OBJEK,
         MODEL TYPE AUSP-ATWRT,
       END OF   IT_SUM_APP246 .

DATA: BEGIN OF IT_DET_APP246 OCCURS 0.
        INCLUDE STRUCTURE ZSPP_DET_APP246.
DATA:   OBJEK TYPE AUSP-OBJEK,
        MODEL TYPE AUSP-ATWRT,
      END OF   IT_DET_APP246.

* Parameters(Screen0110)
DATA: P_BODYNO_APP246 TYPE AUSP-ATWRT,    "P_BODY_SERIAL(09)
      P_LINE_APP246(03),                  "P_TRIM_LINE_NO
      P_PROG_APP246(08),                  "P_RP_STATUS
      P_PROG_APP246_H(08),                  "P_RP_STATUS
      P_STATUS_APP246(10),                " 'S':Summary, 'D':Detail.
      P_WONO_APP246 TYPE MARA-MATNR,      "P_WORK_ORDER
      P_EXTC_APP246(03),                  "P_EXT_COLOR
      P_INTC_APP246(03),                  "P_INT_COLOR
      P_TOTAL_APP246(05) TYPE N.
RANGES: P_PROG FOR AUSP-ATWRT.

DATA: BEGIN OF IT_OBJEK OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        ATWRT TYPE AUSP-ATWRT,
      END OF IT_OBJEK .

DATA:  WA_ALV_CALLED.
*** End   of Definitions For APP246 ********************************

*** Start of Definitions For APP245 ***
DATA: BEGIN OF IT_TEMP_APP245 OCCURS 0,
*
        OBJEK  TYPE AUSP-OBJEK,
        SUMINF TYPE AUSP-ATWRT,
        EXTC   TYPE AUSP-ATWRT,
        INTC   TYPE AUSP-ATWRT,
        DATE   TYPE SY-DATUM,
      END OF IT_TEMP_APP245 .

DATA: BEGIN OF IT_APP245 OCCURS 0,
        SUMINF TYPE AUSP-ATWRT,         "Summary Type
        WONO TYPE AUSP-ATWRT,
        EXTC TYPE AUSP-ATWRT,      "P_EXT_COLOR(03)
        INTC TYPE AUSP-ATWRT,      "P_INT_COLOR(03)
*        objek TYPE ausp-objek,
*
        TOTAL(05) TYPE P,          "Total Quantity
*
        01QTY(05) TYPE P,          "The First Day's QTY
        02QTY(05) TYPE P,          "The Second Day's QTY
        03QTY(05) TYPE P,
        04QTY(05) TYPE P,
        05QTY(05) TYPE P,
        06QTY(05) TYPE P,
        07QTY(05) TYPE P,
        08QTY(05) TYPE P,
        09QTY(05) TYPE P,
        10QTY(05) TYPE P,
        11QTY(05) TYPE P,
        12QTY(05) TYPE P,
        13QTY(05) TYPE P,
        14QTY(05) TYPE P,
        15QTY(05) TYPE P,
        16QTY(05) TYPE P,
        17QTY(05) TYPE P,
        18QTY(05) TYPE P,
        19QTY(05) TYPE P,
        20QTY(05) TYPE P,
        21QTY(05) TYPE P,
        22QTY(05) TYPE P,
        23QTY(05) TYPE P,
        24QTY(05) TYPE P,
        25QTY(05) TYPE P,
        26QTY(05) TYPE P,
        27QTY(05) TYPE P,
        28QTY(05) TYPE P,
        29QTY(05) TYPE P,
        30QTY(05) TYPE P,
        31QTY(05) TYPE P,
*
      END OF IT_APP245.

DATA: BEGIN OF IT_EXCEL_APP245 OCCURS 0,
*
        SUMINF TYPE AUSP-ATWRT,    "Summary Type
        EXTC TYPE AUSP-ATWRT,      "P_EXT_COLOR(03)
        INTC TYPE AUSP-ATWRT,      "P_INT_COLOR(03)
*
        TOTAL(20) ,          "Total Quantity
*
        01QTY(20) ,          "The First Day's QTY
        02QTY(20) ,          "The Second Day's QTY
        03QTY(20) ,
        04QTY(20) ,
        05QTY(20) ,
        06QTY(20) ,
        07QTY(20) ,
        08QTY(20) ,
        09QTY(20) ,
        10QTY(20) ,
        11QTY(20) ,
        12QTY(20) ,
        13QTY(20) ,
        14QTY(20) ,
        15QTY(20) ,
        16QTY(20) ,
        17QTY(20) ,
        18QTY(20) ,
        19QTY(20) ,
        20QTY(20) ,
        21QTY(20) ,
        22QTY(20) ,
        23QTY(20) ,
        24QTY(20) ,
        25QTY(20) ,
        26QTY(20) ,
        27QTY(20) ,
        28QTY(20) ,
        29QTY(20) ,
        30QTY(20) ,
        31QTY(20) ,
*
      END OF IT_EXCEL_APP245.

* Parameters(Screen0110)
DATA: P_LINE_APP245(03),                  "P_TRIM_LINE_NO
      P_PROG_APP245(08),                  "P_RP_STATUS
      P_WONO_APP245(14),                  "P_WORK_ORDER
      P_EXTC_APP245(03),                  "P_EXT_COLOR
      P_INTC_APP245(03),                  "P_INT_COLOR
*
      P_TYPE_APP245(02),  "Summary Type 1:Order No. 2:Option
      P_COLOR_APP245(02),              " O: It is, X: It is not.
      P_SHOP_DATE_APP245 TYPE SY-DATUM,   "P_RPxx_SHOP_DATE
      P_END_DATE_APP245(02) TYPE N.   "The Ending Date of Shop Date

DATA: BEGIN OF IT_DATE OCCURS 0,
        DATE TYPE SY-DATUM,
        NUM(02) TYPE N,
      END OF IT_DATE.
DATA: P_D01_APP245(02) TYPE N,
      P_D02_APP245(02) TYPE N,
      P_D03_APP245(02) TYPE N,
      P_D04_APP245(02) TYPE N,
      P_D05_APP245(02) TYPE N,
      P_D06_APP245(02) TYPE N,
      P_D07_APP245(02) TYPE N,
      P_D08_APP245(02) TYPE N,
      P_D09_APP245(02) TYPE N,
      P_D10_APP245(02) TYPE N,
      P_D11_APP245(02) TYPE N,
      P_D12_APP245(02) TYPE N,
      P_D13_APP245(02) TYPE N,
      P_D14_APP245(02) TYPE N,
      P_D15_APP245(02) TYPE N,
      P_D16_APP245(02) TYPE N,
      P_D17_APP245(02) TYPE N,
      P_D18_APP245(02) TYPE N,
      P_D19_APP245(02) TYPE N,
      P_D20_APP245(02) TYPE N,
      P_D21_APP245(02) TYPE N,
      P_D22_APP245(02) TYPE N,
      P_D23_APP245(02) TYPE N,
      P_D24_APP245(02) TYPE N,
      P_D25_APP245(02) TYPE N,
      P_D26_APP245(02) TYPE N,
      P_D27_APP245(02) TYPE N,
      P_D28_APP245(02) TYPE N,
      P_D29_APP245(02) TYPE N,
      P_D30_APP245(02) TYPE N,
      P_D31_APP245(02) TYPE N.

*** End   of Definitions For APP245 ********************************

*** Start of Definitions For APP244 ***
DATA: BEGIN OF IT_APP244 OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
        MODEL TYPE AUSP-ATWRT, "P_MODEL
        BODYNO TYPE AUSP-ATWRT, "P_MODEL & P_BODY_SERIAL(09)
        VIN TYPE AUSP-ATWRT,                                "P_VIN(17)
        WONO TYPE AUSP-ATWRT,  "P_WORK_ORDER(14)
        EXTC TYPE AUSP-ATWRT,      "P_EXT_COLOR(03)
        INTC TYPE AUSP-ATWRT,      "P_INT_COLOR(03)
        VENDOR(10),    "Not Defined
        MI TYPE AUSP-ATWRT,                                 "P_MI (07)
        OCN TYPE AUSP-ATWRT,                                "P_OCN (04)
        VER TYPE AUSP-ATWRT,  "P_VERSION(03)
        ACT_DATE LIKE SY-DATUM,  "P_RPxx_ACTUAL_DATE(08)
        ACT_TIME LIKE SY-UZEIT,  "P_RPxx_ACTUAL_DATE+08(06)
        SERIAL TYPE AUSP-ATWRT,  "P_RPxx_SERIAL(06)
        PROG TYPE AUSP-ATWRT,      "P_STATUS

      END OF IT_APP244.

DATA: BEGIN OF IT_EXCEL_APP244 OCCURS 0,
        BODYNO TYPE AUSP-ATWRT, "P_MODEL & P_BODY_SERIAL(09)
        VIN TYPE AUSP-ATWRT,                                "P_VIN(17)
        WONO TYPE AUSP-ATWRT,  "P_WORK_ORDER(14)
        EXTC TYPE AUSP-ATWRT,      "P_EXT_COLOR(03)
        INTC TYPE AUSP-ATWRT,      "P_INT_COLOR(03)
        VENDOR(10),    "Not Defined
        MI TYPE AUSP-ATWRT,                                 "P_MI (07)
        OCN TYPE AUSP-ATWRT,                                "P_OCN (04)
        VER TYPE AUSP-ATWRT,  "P_VERSION(03)
        ACT_DATE LIKE SY-DATUM,  "P_RPxx_ACTUAL_DATE(08)
        ACT_TIME LIKE SY-UZEIT,  "P_RPxx_ACTUAL_DATE+08(06)
        PROG TYPE AUSP-ATWRT,      "P_STATUS
        SERIAL TYPE AUSP-ATWRT,  "P_RPxx_SERIAL(06)
*
      END OF IT_EXCEL_APP244.

* Parameters(Screen0110)
DATA: P_PROG_APP244(08),                         "P_RP_STATUS
      P_WONO_APP244(14),                         "P_WORK_ORDER
      P_EXTC_APP244(03),                  "P_EXT_COLOR
      P_INTC_APP244(03),                  "P_INT_COLOR
      P_TOTAL_APP244 TYPE P,
      P_PROD_DATE_APP244   LIKE SY-DATUM.    "P_RPxx_SHOP_DATE
*** End   of Definitions For APP244 ********************************


*** Start of Definitions For APP240 ***
DATA: BEGIN OF IT_APP240 OCCURS 0,
        SERIAL TYPE AUSP-ATWRT,  "P_RPxx_SERIAL(06)
        BODYNO TYPE AUSP-ATWRT,  "P_MODEL & P_BODY_SERIAL(09)
        WONO TYPE AUSP-ATWRT,    "P_WORK_ORDER(14)
        MI TYPE AUSP-ATWRT,                                 " P_MI(07)
        OCN TYPE AUSP-ATWRT,                                " P_OCN(04)
        VER TYPE AUSP-ATWRT,     "P_VERSION(03)
        EXTC TYPE AUSP-ATWRT,    "P_EXT_COLOR(03)
        INTC TYPE AUSP-ATWRT,    "P_INT_COLOR(03)
        ALC TYPE AUSP-ATWRT,     "P_ALC_C_xxx OR P_ALC_U_xxx(05)
        ENG TYPE AUSP-ATWRT,     "P_219_9(02)
        TM TYPE AUSP-ATWRT,      "P_219_7(02)
        TL TYPE AUSP-ATWRT,      "P_219_5(02)
        OBJEK TYPE AUSP-OBJEK,
        RP TYPE AUSP-ATWRT,
      END OF IT_APP240.

DATA: BEGIN OF IT_EXCEL_APP240 OCCURS 0,
        SERIAL(10),  "P_RPxx_SERIAL(06)
        BODYNO(10),  "P_MODEL & P_BODY_SERIAL(09)
        WONO(15),    "P_WORK_ORDER(14)
        MI(07),                                             "P_MI (07)
        OCN(04),                                            "P_OCN (04)
        VER(07),     "P_VERSION(03)
        EXTC(20),    "P_EXT_COLOR(03)
        INTC(20),    "P_INT_COLOR(03)
        ALC(05),     "P_ALC_C_xxx OR P_ALC_U_xxx(05)
        ENG(10),     "P_219_9(02)
        TM(05),      "P_219_7(02)
        TL(05),      "P_219_5(02)
      END OF IT_EXCEL_APP240.

* Parameters(Screen0110)
DATA: P_LINE_APP240(03),   "P_TRIM_LINE_NO
      P_PROG_APP240(08),   "P_RP_STATUS
      P_PROG_APP240_H(08),   "P_RP_STATUS
      P_PART_APP240(10),   "U or C
      P_COLUMN_APP240(03), "P_ALC_U_xxx OR P_ALC_C_xxx
      P_BODYNO_APP240(07), "P_BODY_SERIAL
      P_WONO_APP240(14),   "P_WORK_ORDER
      P_EXTC_APP240(03),   "P_EXT_COLOR
      P_INTC_APP240(03).   "P_INT_COLOR

RANGES: R_PLANT_APP240 FOR AUSP-ATWRT,   "P_TRIM_PLANT_NO
        R_MODEL_APP240 FOR AUSP-ATWRT,   "P_MODEL
        R_LINE_APP240 FOR AUSP-ATWRT,    "P_TRIM_LINE_NO
        R_PROG_APP240 FOR AUSP-ATWRT,    "P_RP_STATUS
        R_PART_APP240 FOR AUSP-ATWRT,    "U or C
        R_COLUMN_APP240 FOR AUSP-ATWRT,  "P_ALC_U_xxx OR P_ALC_C_xxx
        R_BODYNO_APP240 FOR AUSP-ATWRT,  "P_BODY_SERIAL
        R_WONO_APP240 FOR AUSP-ATWRT,    "P_WORK_ORDER
        R_EXTC_APP240 FOR AUSP-ATWRT,    "P_EXT_COLOR
        R_INTC_APP240 FOR AUSP-ATWRT.    "P_INT_COLOR
*** End   of Definitions For APP240 ********************************

*** Start of Definitions For APP239 ***
DATA: BEGIN OF IT_OBJEK_APP239 OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,
      END OF IT_OBJEK_APP239.

RANGES: R_OBJEK_APP239 FOR AUSP-OBJEK,
        R_ATINN_APP239 FOR AUSP-ATINN.

DATA: BEGIN OF IT_CHAR_APP239 OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,  "Key of object to be classified
        ATINN TYPE AUSP-ATINN,  "Internal characteristic
        ATNAM TYPE CABN-ATNAM,  "Characteristic name
        ATWRT TYPE AUSP-ATWRT,  "Characteristic value
        ATFLV TYPE AUSP-ATFLV,
      END OF IT_CHAR_APP239.

DATA: BEGIN OF IT_APP239 OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,      "Key of object to be classified
        MITU_DATE(08),              "Date
        MODEL(03),                  "Model
        BODY_SERIAL(06),            "Body Number
        BODYNO(09),                 "Body Number
        MI(10),                     "Spec
        OCN(10),                    "OCN
        WORKORDER(14),               "Work Order
        EXT_COLOR(03),              "External Color
        INT_COLOR(03),              "Internal Color
        SEQUENCE_SERIAL(04),        "Serial
        SEQUENCE_DATE(10),          "Sequence Date
      END OF IT_APP239.

DATA: BEGIN OF IT_EXCEL_239 OCCURS 0,
        OBJEK TYPE AUSP-OBJEK,      "Key of object to be classified
*        MITU,
        MITU_DATE(10),              "Date
        MODEL(10),                  "Model
        BODY_SERIAL(12),            "Body Number
        MI(10),                     "Spec
        OCN(10),                    "OCN
        WORKORDER(14),               "Work Order
        EXT_COLOR(15),              "External Color
        INT_COLOR(15),              "Internal Color
        SEQUENCE_SERIAL(15),        "Serial
        SEQUENCE_DATE(15),          "Sequence Date
      END OF IT_EXCEL_239.

* Parameters
DATA: P_BODYSER_APP239(06) TYPE N,      "Body Serial
      P_BODYNO_APP239(09),              "Body NO = Model+BodySer
      P_ORDERNO_APP239 TYPE MARA-MATNR, "Order Number
      P_EXT_COLOR_APP239(03),           "External Color
      P_INT_COLOR_APP239(03).           "Internal Color
*** End   of Definitions For APP239 ********************************

*** Start of Definitions For APP272 ***
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* INTERNEL TABLES
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_APP272_01 OCCURS 0,
        P001 TYPE ZWORDER,
        P002 LIKE ZTPP_WOSUM-EXTC,
        P003 LIKE ZTPP_WOSUM-INTC,
        P004(18),
        P005 TYPE ZMODQTY,
        P006 TYPE ZFORECASTQTY,
        P007 TYPE ZPLANQTY,
        P008 TYPE ZSEQQTY ,
        P009 TYPE ZBUKT ,
        P010 TYPE ZBUKT ,
        P011 TYPE ZBUKT ,
        P012 TYPE ZBUKT ,
        P013 TYPE ZBUKT ,
        P014 TYPE ZBUKT ,
        P015 TYPE ZBUKT ,
        P016 TYPE ZBUKT ,
        P017 TYPE ZBUKT ,
        P018 TYPE ZBUKT ,
        P019 TYPE ZBUKT ,
        P020 TYPE ZBUKT ,
        P021 TYPE ZBUKT ,
        P022 TYPE ZBUKT ,
        P023 TYPE C,
        P024 TYPE C,
        P025 TYPE C,
        P026 TYPE C,
        P027 TYPE C,
        P028 TYPE C,
        P029 TYPE C,
        P030 TYPE C,
      END OF IT_APP272_01.

DATA: BEGIN OF IT_EXCEL_APP272 OCCURS 0,
*       p001(10),   "UD1K940253
        P001(14),                                           "UD1K940253
        P002(5),
        P003(5),
        P004(18),
        P005(12),
        P006(12),
        P007(12),
        P008(12),
        P009(12),
        P010(12),
        P011(12),
        P012(12),
        P013(12),
        P014(12),
        P015(12),
        P016(12),
        P017(12),
        P018(12),
        P019(12),
        P020(12),
        P021(12),
        P022(12),
        P023(12),
        P024(12),
        P025(12),
        P026(12),
        P027(12),
        P028(12),
        P029(12),
        P030(12),
      END OF IT_EXCEL_APP272.

DATA: BEGIN OF IT_APP272_DATE OCCURS 0,
      DAY01 LIKE SY-DATUM,
      DAY02 LIKE SY-DATUM,
      DAY03 LIKE SY-DATUM,
      DAY04 LIKE SY-DATUM,
      DAY05 LIKE SY-DATUM,
      DAY06 LIKE SY-DATUM,
      DAY07 LIKE SY-DATUM,
      DAY08 LIKE SY-DATUM,
      DAY09 LIKE SY-DATUM,
      DAY10 LIKE SY-DATUM,
      DAY11 LIKE SY-DATUM,
      DAY12 LIKE SY-DATUM,
      DAY13 LIKE SY-DATUM,
      DAY14 LIKE SY-DATUM,
      DAY15 LIKE SY-DATUM,
END OF IT_APP272_DATE.
*----------------------------------------------------------------------*
* DATAS
*----------------------------------------------------------------------*
DATA: SV_PROG_APP272                 LIKE SY-REPID,
      SV_DYNNR_APP272                LIKE SY-DYNNR.
DATA: P_CARX_APP272 LIKE ZTBM_ABXPLIDT-CARX,
      P_GUBN_APP272 LIKE ZTBM_ABXPLIDT-GUBN,
      P_HPCC_APP272 LIKE ZTBM_ABXPLIDT-HPCC.
DATA: WA_Y_APP272,
      WA_T_APP272,
      WA_P_APP272,
      WA_M_APP272,
      WA_D_APP272,
      WA_S_APP272,
      WA_H_APP272,
      WA_X_APP272,
      WA_CLM1_APP272(4),
      WA_CLM2_APP272(4),
      WA_URGEN_APP272(10).
*** End   of Definitions For APP272 ********************************

*** Start of Definitions For APP301 ***
*     Internal Table - Sequenced Hourly Plan.
DATA: BEGIN OF IT_HOUR_APP301 OCCURS 0.
        INCLUDE STRUCTURE ZTPP_SEQ_SUM01.
DATA:   ORDER LIKE ZTPP_SEQ_SUM01-SERIAL,
        T01(05) TYPE P,  "The Today's Summary
        T02(05) TYPE P,  "The Today's Summary
        T03(05) TYPE P,  "The Today's Summary
        GTOT(7) TYPE P,  " Grand Total
*       sum(05) type p,  "The Total Planned Quantity per Code
      END OF IT_HOUR_APP301.

*     Internal Table - Sequenced Daily Plan.
DATA: BEGIN OF IT_DAY_APP301 OCCURS 0.
        INCLUDE STRUCTURE ZTPP_SEQ_SUM02.
DATA:   ORDER LIKE ZTPP_SEQ_SUM02-SERIAL,
        B_RESULT(05) TYPE P,  "The previous day's Production Results
      END OF IT_DAY_APP301.

*     Internal Table - Sequenced Daily Plan.
DATA: BEGIN OF IT_WEEK_APP301 OCCURS 0.
        INCLUDE STRUCTURE ZTPP_SEQ_SUM03.
DATA:   ORDER LIKE ZTPP_SEQ_SUM03-SERIAL,
        B_RESULT(05) TYPE P,  "The previous day's Production Results
      END OF IT_WEEK_APP301.

* Definition of Parameters
DATA: P_DATE_APP301 LIKE SY-DATUM,   "Basic Date
      P_BT_APP301(02),               "Body or Trim
      P_SL_APP301(02),               "Per Hour or Per Day
      P_PART_APP301(02),             "U or C
      P_COLUMN_APP301(03),           "From 1 To 219
      P_COL_NAME_APP301(20),         "Column's Description
      P_CODE_APP301(05).             "Value of Column
*** End   of Definitions For APP301 ********************************

*** Start of Definitions For APP302 ***
* Definition of Internal Tables
DATA: BEGIN OF IT_WO_APP302 OCCURS 0,
        WONO     TYPE AUSP-OBJEK             ,
        ALC_CODE TYPE ZTPP_SEQ_SUM02-ALC_CODE,
        ALC_VALS TYPE AUSP-ATWRT             ,
      END OF IT_WO_APP302.

DATA: BEGIN OF ST_APP302 ,
        PR(5)    TYPE C                      ,
        D0(5)    TYPE C                      ,
        D1(5)    TYPE C                      ,
        D2(5)    TYPE C                      ,
        GDATE    TYPE D                      ,
        GTIME    TYPE T                      ,
      END OF ST_APP302 .

DATA: BEGIN OF IT_APP302 OCCURS 0,
        WORDER   LIKE MARA-MATNR ,
        ORDER    TYPE ZTPP_SEQ_SUM02-SERIAL,
        MODEL    TYPE ZTPP_SEQ_SUM02-MODEL,
        ALC_CODE TYPE ZTPP_SEQ_SUM02-ALC_CODE,
        ALC_VALS TYPE ZTPP_SEQ_SUM02-ALC_VALS,
        D_1      TYPE ZTPP_SEQ_SUM02-D_1,
        SEQ      TYPE ZTPP_SEQ_SUM02-PBS,
        BODYIN   TYPE ZTPP_SEQ_SUM02-PBS,
        WBS      TYPE ZTPP_SEQ_SUM02-PBS,
        PAINT    TYPE ZTPP_SEQ_SUM02-PBS,
        PRJ      TYPE ZTPP_SEQ_SUM02-PRJ,
        PBS      TYPE ZTPP_SEQ_SUM02-PBS,
        D01      TYPE ZTPP_SEQ_SUM02-D01,
        D02      TYPE ZTPP_SEQ_SUM02-D01,
        D03      TYPE ZTPP_SEQ_SUM02-D02,
        TOT_P(05)    TYPE P,  "Total of Plan Quantity
        B_RESULT(05) TYPE P,  "The previous day's Production Results
        TOT_R(05)  TYPE P,    "Total of Result Quantity
*       today(05)  type p,    "Total of Result Quantity
      END OF IT_APP302.

* Definition of Parameters
DATA: P_DATE_APP302 LIKE SY-DATUM,    "Basic Date
      P_PART_APP302(02),              "U or C
      P_COLUMN_APP302(03),            "From 1 To 200
      P_COL_NAME_APP302(20),          "Column's Description
      P_CODE_APP302 TYPE AUSP-ATWRT.  "Value of Code
*** End   of Definitions For APP302 ********************************

*** Start of Definitions For APP220 ***
DATA: BEGIN OF IT_APP220 OCCURS 0.
        INCLUDE STRUCTURE ZTBM_ABXOPVDT.
DATA:   MODEL TYPE ZTBM_219_VALUE-MODEL,
        SERIAL TYPE ZTBM_219_VALUE-SERIAL,
        VALUE TYPE ZTBM_219_VALUE-VALUE,
        CODE_NAME1 TYPE ZTBM_219_VALUE-CODE_NAME1,
        CODE_NAME2 TYPE ZTBM_219_VALUE-CODE_NAME2,
        ZOPTION TYPE ZTBM_219_VALUE-ZOPTION,
        ALC TYPE ZTBM_219_VALUE-ALC,
        ZLIST TYPE ZTBM_219_VALUE-ZLIST,
        COL01 TYPE ZTBM_219_VALUE-COL01,
        VAL01 TYPE ZTBM_219_VALUE-VAL01,
        COL02 TYPE ZTBM_219_VALUE-COL02,
        VAL02 TYPE ZTBM_219_VALUE-VAL02,
        COL03 TYPE ZTBM_219_VALUE-COL03,
        VAL03 TYPE ZTBM_219_VALUE-VAL03,
        COL04 TYPE ZTBM_219_VALUE-COL04,
        VAL04 TYPE ZTBM_219_VALUE-VAL04,
        COL05 TYPE ZTBM_219_VALUE-COL05,
        VAL05 TYPE ZTBM_219_VALUE-VAL05,
        COL06 TYPE ZTBM_219_VALUE-COL06,
        VAL06 TYPE ZTBM_219_VALUE-VAL06,
        COL07 TYPE ZTBM_219_VALUE-COL07,
        VAL07 TYPE ZTBM_219_VALUE-VAL07,
        COL08 TYPE ZTBM_219_VALUE-COL08,
        VAL08 TYPE ZTBM_219_VALUE-VAL08,
        COL09 TYPE ZTBM_219_VALUE-COL09,
        VAL09 TYPE ZTBM_219_VALUE-VAL09,
        COL10 TYPE ZTBM_219_VALUE-COL10,
        VAL10 TYPE ZTBM_219_VALUE-VAL10,
        ADATE TYPE ZTBM_219_VALUE-ADATE,
        CUSER TYPE ZTBM_219_VALUE-CUSER,
        FLAG TYPE ZTBM_219_VALUE-FLAG,
        ERDAT TYPE ZTBM_219_VALUE-ERDAT,
        ERZET TYPE ZTBM_219_VALUE-ERZET,
        ERNAM TYPE ZTBM_219_VALUE-ERNAM,
        AEDAT TYPE ZTBM_219_VALUE-AEDAT,
        AEZET TYPE ZTBM_219_VALUE-AEZET,
        AENAM TYPE ZTBM_219_VALUE-AENAM.
DATA: END OF IT_APP220.

DATA: BEGIN OF IT_219VAL  OCCURS  0.
        INCLUDE STRUCTURE  ZTBM_219_VALUE.
DATA: END OF IT_219VAL.

DATA: BEGIN OF IS219,
        MODEL(03),
        NAME219   LIKE  ZTBM_ABXOPVDT-CLNO,
        DESC219   LIKE  ZTBM_ABXOPVDT-CLNM.
DATA  END OF IS219.
*** End   of Definitions For APP220 ********************************

*** Start of Definitions For Screen 4104 ***
DATA: P_01_4104,     " Flag: ALC Sequence Summary (Hourly)
      P_02_4104,     " Flag: ALC Sequence Summary (Daily)
      P_03_4104,     " Flag: ALC Sequence Summary (Weekly)
      P_04_4104,     " Flag: Body Input Plan List
      P_05_4104,     " Flag: Trim Input Plan List
      P_06_4104,     " Flag: Monthly Production Result List
      P_07_4104,     " Flag: Vehicle Sequence List
      P_08_4104,     " Flag: Vehicle List Staus
      P_09_4104,     " Flag: Wire Mixture - Houly
      P_10_4104.     " Flag: Wire Mixture - Daily

*** End   of Definitions For Screen 4104 ***************************
** Screen 8088
DATA: P_01_8088,
      P_02_8088,
      P_03_8088,
      P_04_8088,
      P_05_8088,
      P_06_8088,
      P_11_8088,
      P_12_8088,
      P_13_8088,
      P_14_8088,
      P_15_8088.
** End of 8088
*** Start of Definitions For APP263 ***
DATA: BEGIN OF ST_APP263,
        DATE TYPE SY-DATUM   ,  "Prod. Date
        SALE_PART            ,  "Sales Type
        OPT TYPE AUSP-ATWRT  ,  "Option
*
        D01 TYPE SY-DATUM,  "T/C's Field Title
        D02 TYPE SY-DATUM,
        D03 TYPE SY-DATUM,
        D04 TYPE SY-DATUM,
        D05 TYPE SY-DATUM,
        D06 TYPE SY-DATUM,
        D07 TYPE SY-DATUM,
*
      END OF ST_APP263.

DATA: BEGIN OF IT_APP263 OCCURS 0,
        MODEL TYPE AUSP-ATWRT,
        SITE  TYPE AUSP-ATWRT,
        ENG   TYPE AUSP-ATWRT,

        DAILY_TIN_P(05) TYPE P,  "Plan
        DAILY_TIN_R(05) TYPE P,  "Result
        DAILY_TIN_D(05) TYPE P,  "Difference

        MON_TIN_P(05)   TYPE P,  "Plan
        MON_TIN_R(05)   TYPE P,  "Result
        MON_TIN_D(05)   TYPE P,  "Difference

        PBS(05)  TYPE P,  "Status of RP 'B01'
        PREJ(05) TYPE P,
        DAY1(05) TYPE P,  "Plan Quantity of Day + 1
        DAY2(05) TYPE P,  "Plan Quantity of Day + 2
        DAY3(05) TYPE P,  "Plan Quantity of Day + 3
        DAY4(05) TYPE P,  "Plan Quantity of Day + 4
        DAY5(05) TYPE P,  "Plan Quantity of Day + 5
        DAY6(05) TYPE P,  "Plan Quantity of Day + 6
        DAY7(05) TYPE P,  "Plan Quantity of Day + 7

      END OF IT_APP263.

*** End   of Definitions For APP263 ********************************
*
************************************************************************
* INTERNAL TABLES DECLARATION
************************************************************************
DATA: BEGIN OF          IT_APP207  OCCURS 0,
        MARK            TYPE C           ,
        NO              TYPE I           ,
        MATNR           LIKE MARA-MATNR  ,
        PERF            TYPE C           ,
        PROD            TYPE C           ,
        NATION(5)       TYPE C           ,
        WO(9)           TYPE C           ,
        MQTY            LIKE ZTPP_WOSUM-MODQTY,
        PLNT            TYPE C           ,
        MI              LIKE ZTPP_WOSUM2-MI,
        OCN             LIKE ZTPP_WOSUM2-OCN,
        VER             LIKE ZTPP_WOSUM-VERSION,
        M_VER           TYPE C           ,
        M_ROU           TYPE C           ,
        M_MAT           TYPE C           ,
        M_BOM           TYPE C           ,
        RDATE           LIKE SY-DATUM    ,
        PDATE           LIKE SY-DATUM    ,
        HSTATS          TYPE C           ,
      END OF IT_APP207.
*

DATA: BEGIN OF IT_ERROR_MAT_APP207 OCCURS 0,
        MATNR TYPE MARA-MATNR,
        FORDER TYPE MARA-MATNR,
        WORDER TYPE ZTPP_SPEC-WORDER,
        EXTC TYPE ZTPP_SPEC-EXTC,
        INTC TYPE ZTPP_SPEC-INTC,
      END OF IT_ERROR_MAT_APP207.

DATA: BEGIN OF IT_WOSUM_APP207       OCCURS 0.
        INCLUDE STRUCTURE     ZTPP_WOSUM .
DATA:   MARK                  TYPE C,
        ORDER_NO              LIKE AUSP-ATWRT,
        T_DATE                LIKE SY-DATUM,
        FLAG1                 TYPE C,
      END OF IT_WOSUM_APP207             .

DATA: BEGIN OF IT_ALC_APP207         OCCURS 0,
        NO(3)                 TYPE N  ,
        COL(5)                TYPE C  ,
        COLNM(40)             TYPE C  ,
        COLDC(40)             TYPE C  ,
        KNNUM                 LIKE CUOB-KNNUM,
        KNNAM                 LIKE CUKB-KNNAM,
        CTYPE                 TYPE C  ,
      END OF IT_ALC_APP207.

DATA: BEGIN OF IT_HPCS_APP207        OCCURS 0,
        NO(3)                 TYPE N  ,
        COL(5)                TYPE C  ,
        COLNM(40)             TYPE C  ,
        COLDC(40)             TYPE C  ,
        HPCS(5)               TYPE C  ,
      END OF IT_HPCS_APP207.

************************************************************************
* WORKING-AREA VARIABLES DECLARATION
************************************************************************
DATA: WA_WOSUM_APP207          LIKE ZTPP_WOSUM,
      WA_ATINN_APP207          LIKE AUSP-ATINN,
      WA_ATWRT_APP207          LIKE AUSP-ATWRT,
      WA_POINT_APP207          LIKE SY-TABIX  ,
      WA_TOTAL_APP207          TYPE I         ,
      WA_CHANGE_APP207         TYPE C         ,
      WA_FILENAME_APP207       LIKE RLGRAP-FILENAME,
      WA_FSC_APP207            LIKE ZTPP_WOSUM-FSC ,
      WA_ORDER_APP207          LIKE MARA-MATNR,
      WA_COLOR_APP207 LIKE MARA-MATNR,
      WA_COLOR_DIS_APP207(4)   TYPE C   ,  " Color - Display 4 Bit.
      WA_CAR_APP207 LIKE AUSP-ATWRT,  " Car Type - Name
      WA_MI_APP207 LIKE AUSP-ATWRT,  " Model Index
      WA_OCN_APP207 LIKE AUSP-ATWRT,  " O.C.N
      WA_VERS_APP207 LIKE AUSP-ATWRT,  " Version
      WA_CRDAT_APP207 LIKE AUSP-ATWRT,  " Creation Date
      WA_MDDAT_APP207 LIKE AUSP-ATWRT,  " Modify Date
      WA_DEST_APP207 LIKE AUSP-ATWRT,  " Destination Code
      WA_ALDAT_APP207 LIKE AUSP-ATWRT,  " ALC Transport Date
      WA_PERF_APP207 LIKE AUSP-ATWRT,  " Perfect YN
      WA_INITQ_APP207 LIKE AUSP-ATWRT,  " Initial Quantity
      WA_MODQ_APP207 LIKE AUSP-ATWRT,  " Modified Quantity
      WA_SQTY_APP207 LIKE AUSP-ATWRT,  " Sequence Quantity
      WA_PQTY_APP207 LIKE AUSP-ATWRT,  " Plan Quantity
      WA_FQTY_APP207 LIKE AUSP-ATWRT,  " Forecast Quantity
      WA_MQTY_APP207 LIKE AUSP-ATWRT,  " MITU Quantity
      WA_LCNO_APP207 LIKE AUSP-ATWRT,  " L/C No.
      WA_PLNT_APP207 LIKE AUSP-ATWRT,  " Plant (TRIM Plant No)
      WA_VIN_APP207 LIKE AUSP-ATWRT,  " Vin Spec.
      WA_PROD_APP207 LIKE AUSP-ATWRT,  " Production Flag
      WA_RECS_APP207(16) TYPE C         ,
      WA_ZTPP_SPEC_APP207 LIKE ZTPP_SPEC.
*
*TYPE-POOLS: vrm.
*---- LIST BOX DATA
DATA: XNAME_APP207    TYPE VRM_ID.

* PARAMETERS  --- SCREEN 100
DATA: NAME_APP207    TYPE VRM_ID,
      P_WORDER_APP207 TYPE ZTPP_SPEC-WORDER,
      P_MATNR_APP207  LIKE MARA-MATNR      .
* PARAMETERS --- SCREEN 110
DATA: P_KEYCODE_APP207 TYPE ZTPP_SPEC-KEYCODE,
      P_ERDAT_APP207 LIKE SY-DATUM,
      P_ERZET_APP207 LIKE SY-UZEIT,
      P_ERNAM_APP207 LIKE SY-UNAME.

* internal table for export
DATA: BEGIN OF IT_SPEC_APP207 OCCURS 0,
        PLANT TYPE ZTPP_SPEC-PLANT,
        OPDATE TYPE ZTPP_SPEC-OPDATE,
        OPCOUNT TYPE ZTPP_SPEC-OPCOUNT,
        WORDER TYPE ZTPP_SPEC-WORDER,
        EXTC TYPE ZTPP_SPEC-EXTC,
        INTC TYPE ZTPP_SPEC-INTC,
      END OF IT_SPEC_APP207.

DATA: P_EXTC_APP207(03).

*   ON VALUE-REQUEST
DATA: BEGIN OF REASON_TAB_APP207 OCCURS 0,
        CODE   LIKE MARA-MATNR,
*        text   LIKE lfa1-name1,
      END OF REASON_TAB_APP207.
DATA: BEGIN OF DYNPFIELDS_APP207 OCCURS 3.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS_APP207.
*
DATA: IT_WOSUM2_C_APP207 LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.
DATA: WA_CUOBF_APP207 LIKE MARA-CUOBF.
DATA: IT_CONF_APP207 LIKE CONF_OUT OCCURS 0 WITH HEADER LINE,
      IT_CHAR_APP207 LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.
DATA: BEGIN OF IT_MATNR_APP207 OCCURS 0,
      MATNR LIKE MARA-MATNR,
      CUOBF LIKE MARA-CUOBF,
      END OF IT_MATNR_APP207.
*** End   of Definitions For APP207 ********************************
DATA : WA_FLAG_102.
DATA:  W_STATUS(2).
* SCREEN 2108
DATA: IT_2108 TYPE STANDARD TABLE OF  ZTPP_DELAY_CAR.
DATA: WA_2108 LIKE LINE OF IT_2108.
DATA: IT_CABN LIKE CABN OCCURS 0 WITH HEADER LINE.
RANGES: R_ATINN FOR CABN-ATINN.
* SCREEN 2103
DATA: P_FDATE_2103 LIKE SY-DATUM,
       P_TDATE_2103 LIKE SY-DATUM.
*&spwizard: declaration of tablecontrol 'TB_8081' itself
CONTROLS: TB_8081 TYPE TABLEVIEW USING SCREEN 8081.
CONTROLS: TB_8082 TYPE TABLEVIEW USING SCREEN 8082.
