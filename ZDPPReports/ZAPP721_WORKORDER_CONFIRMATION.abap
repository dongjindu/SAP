************************************************************************
* Program Name      : ZAPP721_WORKORDER_CONFIRMATION
* Author            : Bobby
* Creation Date     : 2003.11.20.
* Specifications By : B.H, Kim
* Pattern           : 2.1
* Development Request No : UD1K902031
* Addl Documentation:
* Description       : WorkOrder Confirmation
*
* Modification Logs
* Date       Developer    RequestNo    Description
*09/21/2004   YONGPING                 INFERFACE BUTTON TEXT CHANGE
*08/07/2006   Furong                   make the parallel processing *
*                                      working: commit statement and
*                                      mm02
*08/17/2010   Daniel      UD1K949652   MI digit 8->9 for excel down
************************************************************************
REPORT  ZAPP721_WORKORDER_CONFIRMATION      NO STANDARD PAGE HEADING
                                            MESSAGE-ID ZMPP.

************************************************************************
* TABLES DECLARATION
************************************************************************
TABLES: ZTPP_SPEC,  "Spec Table
        ZVPP_RP2 ,  " Rate Routing
        ZVPP_BOM ,  " Manufacture BOM
        MAST,       " Material to BOM Link
        MKAL,       " Production Version
        MARA.       " Material Master

************************************************************************
* SCREEN CONTROLS DECLARATION
************************************************************************
CONTROLS: TC_100        TYPE TABLEVIEW USING SCREEN 100,
          TC_200        TYPE TABLEVIEW USING SCREEN 200,
          TC_300        TYPE TABLEVIEW USING SCREEN 300,
          TC_301        TYPE TABLEVIEW USING SCREEN 300,
          TC_302        TYPE TABLEVIEW USING SCREEN 300.
*
************************************************************************
* INTERNAL TABLES DECLARATION
************************************************************************
DATA: BEGIN OF          IT_DATA  OCCURS 0,
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
        NOTE            LIKE RSTXT-TXLINE,
      END OF IT_DATA.
*
DATA: BEGIN OF          IT_LOG  OCCURS 0,
        MATNR           LIKE MARA-MATNR  ,
        MESSAGE(30),
      END OF IT_LOG.

DATA: BEGIN OF IT_219         OCCURS 0,
        MARK                  TYPE C,
        NO(3)                 TYPE N,
        219CODE               LIKE ZTBM_ABXOPVDT-VANM,
        219DESC               LIKE ZTBM_ABXOPVDT-CLNM,
        219VALS               LIKE AUSP-ATWRT,
      END OF IT_219               .

DATA: BEGIN OF IT_ERROR_MATNR OCCURS 0,
        MATNR TYPE MARA-MATNR,
        FORDER TYPE MARA-MATNR,
        WORDER TYPE ZTPP_SPEC-WORDER,
        EXTC TYPE ZTPP_SPEC-EXTC,
        INTC TYPE ZTPP_SPEC-INTC,
      END OF IT_ERROR_MATNR.

DATA: BEGIN OF IT_WOSUM       OCCURS 0.
        INCLUDE STRUCTURE     ZTPP_WOSUM .
DATA:   MARK                  TYPE C,
        ORDER_NO              LIKE AUSP-ATWRT,
        T_DATE                LIKE SY-DATUM,
        FLAG1                 TYPE C,
      END OF IT_WOSUM             .

DATA: BEGIN OF IT_ALC         OCCURS 0,
        NO(3)                 TYPE N  ,
        COL(5)                TYPE C  ,
        COLNM(40)             TYPE C  ,
        COLDC(40)             TYPE C  ,
        KNNUM                 LIKE CUOB-KNNUM,
        KNNAM                 LIKE CUKB-KNNAM,
        CTYPE                 TYPE C  ,
      END OF IT_ALC.

DATA: BEGIN OF IT_HID_MENU    OCCURS 0,
        FCODE                 LIKE SY-UCOMM,
      END OF IT_HID_MENU.

DATA: BEGIN OF IT_HPCS        OCCURS 0,
        NO(3)                 TYPE N  ,
        COL(5)                TYPE C  ,
        COLNM(40)             TYPE C  ,
        COLDC(40)             TYPE C  ,
        HPCS(5)               TYPE C  ,
      END OF IT_HPCS.

DATA: IT_MARA           LIKE TABLE OF MARA      WITH HEADER LINE,
      IT_WOSUM_KEY      LIKE TABLE OF IT_WOSUM  WITH HEADER LINE,
      IT_CUKB           LIKE TABLE OF IT_ALC    WITH HEADER LINE,
      IT_COL            LIKE TABLE OF IT_WOSUM  WITH HEADER LINE,
      IT_COLOR          LIKE TABLE OF IT_DATA   WITH HEADER LINE,
      IT_HEADER         LIKE TABLE OF IT_DATA   WITH HEADER LINE,
      IT_VALS           LIKE TABLE OF ZSPP_VIN_VALUE
                                                WITH HEADER LINE,
      IT_HDVALS         LIKE TABLE OF IT_VALS   WITH HEADER LINE,
      IT_CLVALS         LIKE TABLE OF IT_VALS   WITH HEADER LINE,
      IT_RESULT         LIKE TABLE OF IT_VALS   WITH HEADER LINE.

DATA: BEGIN OF IT_ALC_U         OCCURS 0,
       U_NO                 TYPE I  ,
      END OF IT_ALC_U.

DATA: BEGIN OF IT_ALC_C         OCCURS 0,
        C_NO                 TYPE I  ,
      END OF IT_ALC_C.

DATA: BEGIN OF S_ALC_U         OCCURS 0,
       U_NO                 TYPE I  ,
      END OF S_ALC_U.

DATA: BEGIN OF S_ALC_C         OCCURS 0,
       C_NO                 TYPE I  ,
      END OF S_ALC_C.

************************************************************************
* WORKING-AREA VARIABLES DECLARATION
************************************************************************
DATA: WA_WOSUM          LIKE ZTPP_WOSUM,
      WA_ATINN          LIKE AUSP-ATINN,
      WA_ATWRT          LIKE AUSP-ATWRT,
      WA_POINT          LIKE SY-TABIX  ,
      WA_TOTAL          TYPE I         ,
      WA_CHANGE         TYPE C         ,
      WA_FILENAME       LIKE RLGRAP-FILENAME,
      WA_FSC            LIKE ZTPP_WOSUM-FSC ,
      WA_ORDER          LIKE MARA-MATNR,
      WA_COLOR          LIKE MARA-MATNR,
      WA_COLOR_DIS(4)   TYPE C         ,  " Color - Display 4 Bit.
      WA_CAR            LIKE AUSP-ATWRT,  " Car Type - Name
      WA_MI             LIKE AUSP-ATWRT,  " Model Index
      WA_OCN            LIKE AUSP-ATWRT,  " O.C.N
      WA_VERS           LIKE AUSP-ATWRT,  " Version
      WA_CRDAT          LIKE AUSP-ATWRT,  " Creation Date
      WA_MDDAT          LIKE AUSP-ATWRT,  " Modify Date
      WA_DEST           LIKE AUSP-ATWRT,  " Destination Code
      WA_ALDAT          LIKE AUSP-ATWRT,  " ALC Transport Date
      WA_PERF           LIKE AUSP-ATWRT,  " Perfect YN
      WA_INITQ          LIKE AUSP-ATWRT,  " Initial Quantity
      WA_MODQ           LIKE AUSP-ATWRT,  " Modified Quantity
      WA_SQTY           LIKE AUSP-ATWRT,  " Sequence Quantity
      WA_PQTY           LIKE AUSP-ATWRT,  " Plan Quantity
      WA_FQTY           LIKE AUSP-ATWRT,  " Forecast Quantity
      WA_MQTY           LIKE AUSP-ATWRT,  " MITU Quantity
      WA_LCNO           LIKE AUSP-ATWRT,  " L/C No.
      WA_PLNT           LIKE AUSP-ATWRT,  " Plant (TRIM Plant No)
      WA_VIN            LIKE AUSP-ATWRT,  " Vin Spec.
      WA_PROD           LIKE AUSP-ATWRT,  " Production Flag
      WA_FLG            TYPE C         ,
      WA_RECS(16)       TYPE C         ,
      WA_ZTPP_SPEC      LIKE ZTPP_SPEC.
*
TYPE-POOLS: VRM.
*---- LIST BOX DATA
DATA: XNAME    TYPE VRM_ID.

* PARAMETERS  --- SCREEN 100
DATA: P_COMPANY(4),
      NAME    TYPE VRM_ID,
      P_MODEL TYPE ZTPP_SPEC-MODEL,
      P_WORDER TYPE ZTPP_SPEC-WORDER,
      P_MATNR  LIKE MARA-MATNR      .
DATA: P_C219_1 TYPE ZSERIAL,
      P_V219_1 TYPE ZVLAUE,
      P_C219_2 TYPE ZSERIAL,
      P_V219_2 TYPE ZVLAUE,
      P_C219_3 TYPE ZSERIAL,
      P_V219_3 TYPE ZVLAUE.
* PARAMETERS --- SCREEN 110
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
*     P_MODEL
DATA: MODEL_LIST TYPE VRM_VALUES,
      MODEL_VALUE LIKE LINE OF MODEL_LIST,
*     P_EXTC
      P_EXTC(03),
*     P_INTC
      P_INTC(03).
RANGES: R_MODEL FOR ZTPP_SPEC-MODEL.

*   ON VALUE-REQUEST
DATA: BEGIN OF REASON_TAB OCCURS 0,
        CODE   LIKE MARA-MATNR,
*        text   LIKE lfa1-name1,
      END OF REASON_TAB.
DATA: BEGIN OF DYNPFIELDS OCCURS 3.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.
*
DATA: OK_CODE TYPE SY-UCOMM,
      SV_CODE TYPE SY-UCOMM.

DATA: WA_INIT_FLG, WA_FLAG_200.
DATA: W_ALC_FLAG(1).

DATA: IT_WOSUM2_C LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.
DATA: IT_DAILY LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.
DATA: IT_LIST LIKE ZTPP_WOSUM2 OCCURS 0 WITH HEADER LINE.

DATA: F_DATE LIKE SY-DATUM,
      L_DATE LIKE SY-DATUM.

DATA: WA_CUOBF LIKE MARA-CUOBF.
DATA: IT_CONF LIKE CONF_OUT OCCURS 0 WITH HEADER LINE,
      IT_CHAR LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.
DATA: BEGIN OF IT_MATNR OCCURS 0,
      MATNR LIKE MARA-MATNR,
      CUOBF LIKE MARA-CUOBF,
      END OF IT_MATNR.

DATA: IT_ALC_NAME LIKE CABN OCCURS 0 WITH HEADER LINE..

DATA: R01(1),
      R(02).
*---START WSKIM 03/07/2005
DATA :RE_DATA LIKE ZSPP_WO_CONF OCCURS 0 WITH HEADER LINE.
DATA: SND_JOBS TYPE I VALUE 1,
      RCV_JOBS TYPE I VALUE 1,
      WIDTH TYPE I,
      EXCP_FLAG(1) TYPE C,
      TASKNAME(4) TYPE N VALUE '0001',
      ERR_CHK.
*---END
*************** DO NOT USE!!!! *****************************************
DATA: IT_REC                  LIKE TABLE OF MARA ,
      IT_BDCDATA              LIKE TABLE OF BDCDATA    WITH HEADER LINE,
      P_TCODE                 LIKE  TSTC-TCODE                ,
      P_CMODE                 TYPE  C                         ,
      P_PMODE                 TYPE  C   VALUE   'N'           ,
*     wa_filename             LIKE  rlgrap-filename,
      WA_FILETYPE             LIKE  RLGRAP-FILETYPE VALUE 'DAT',
      WA_BDCGROUP             LIKE  SY-UNAME.          " APQI-GROUPID
************************************************************************

*----------------------------------------------------------------------
* CONSTANTS VARIABLES DECLARATION
*----------------------------------------------------------------------
CONSTANTS: C_COMPANY(4)  TYPE C VALUE 'HMMA'.


INCLUDE ZCPP103_COMMON_ROUTINE .

************************************************************************
*              INITIALIZATION                                          *
************************************************************************
INITIALIZATION.

************************************************************************
*              AT SELECTION SCREEN                                     *
************************************************************************
AT SELECTION-SCREEN.

************************************************************************
*              START-OF-SELECTION PROCESSING                           *
************************************************************************
START-OF-SELECTION.
* PERFORM data_select.

  CALL SCREEN 100.

END-OF-SELECTION.

************************************************************************
*              TOP-OF-PAGE                                             *
************************************************************************
TOP-OF-PAGE.


*&---------------------------------------------------------------------*
*&      Form  DATA_SELECT
*&---------------------------------------------------------------------*
FORM DATA_SELECT.
  " Select Data in the Material Master's Classification View.....
  PERFORM DATA_INITIAL.
  " Fill the remain field in the Internal Table IT_DATA...
  PERFORM DATA_FILLING.
ENDFORM.                    " DATA_SELECT

*&---------------------------------------------------------------------*
*&      Form  DATA_INITIAL
*&---------------------------------------------------------------------*
FORM DATA_INITIAL.
  DATA: L_MATNR              LIKE MARA-MATNR.
  DATA: L_CHAR(3),
        L_219_1 LIKE IT_VALS-ATNAM,
        L_219_2 LIKE IT_VALS-ATNAM,
        L_219_3 LIKE IT_VALS-ATNAM.

  IF P_MATNR = SPACE.
    SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_MARA
      FROM MARA
     WHERE MTART = 'WOHD'
*       AND mbrsh = 'A'
       AND KZKFG = SPACE .
  ELSE.
    CONCATENATE P_MATNR     '%'            INTO  L_MATNR .
    SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_MARA
      FROM MARA
     WHERE MATNR LIKE L_MATNR
       AND MTART = 'WOHD'
*       AND mbrsh = 'A'
       AND KZKFG = SPACE .
  ENDIF.

  LOOP AT IT_MARA.
    CLEAR: IT_VALS, IT_VALS[].
    IT_VALS-ATNAM = 'P_PERF_YN'.     APPEND IT_VALS.
    IT_VALS-ATNAM = 'P_PROD_FLAG'.   APPEND IT_VALS.
    IT_VALS-ATNAM = 'P_MODEL'  .     APPEND IT_VALS.
** Changed by Furong on 04/19/11
    IF NOT P_C219_1 IS INITIAL.
** Changed by Furong on 05/06/11
*      L_CHAR = P_C219_1.
*      REPLACE '0' WITH ' ' INTO L_CHAR.
*      REPLACE '0' WITH ' ' INTO L_CHAR.
      WRITE P_C219_1 TO L_CHAR NO-ZERO.
** End of cahnge
      CONDENSE L_CHAR NO-GAPS.
      CONCATENATE 'P_219_' L_CHAR INTO L_219_1.
      IT_VALS-ATNAM = L_219_1.
      APPEND IT_VALS.
    ENDIF.
    IF NOT P_C219_2 IS INITIAL.
** Changed by Furong on 05/06/11
*      L_CHAR = P_C219_2.
*      REPLACE '0' WITH ' ' INTO L_CHAR.
*      REPLACE '0' WITH ' ' INTO L_CHAR.
      WRITE P_C219_2 TO L_CHAR NO-ZERO.
** End
      CONDENSE L_CHAR NO-GAPS.
      CONCATENATE 'P_219_' L_CHAR INTO L_219_2.
      IT_VALS-ATNAM = L_219_2.
      APPEND IT_VALS.
    ENDIF.
    IF NOT P_C219_3 IS INITIAL.
** Changed by Furong on 05/06/11
*      L_CHAR = P_C219_3.
*      REPLACE '0' WITH ' ' INTO L_CHAR.
*      REPLACE '0' WITH ' ' INTO L_CHAR.
      WRITE P_C219_3 TO L_CHAR NO-ZERO.
** End
      CONDENSE L_CHAR NO-GAPS.
      CONCATENATE 'P_219_' L_CHAR INTO L_219_3.
      IT_VALS-ATNAM = L_219_3.
      APPEND IT_VALS.
    ENDIF.
** End of change

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = IT_MARA-MATNR
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = IT_VALS
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    READ TABLE IT_VALS WITH KEY ATNAM = 'P_MODEL'.
    IF SY-SUBRC = 0 AND IT_VALS-ATWRT NE P_MODEL.
      DELETE IT_MARA.
      CONTINUE      .
    ENDIF.
** Changed by Furong on 04/19/11
    IF NOT P_C219_1 IS INITIAL.
      READ TABLE IT_VALS WITH KEY ATNAM = L_219_1.
      IF SY-SUBRC <> 0 OR IT_VALS-ATWRT NE P_V219_1.
        DELETE IT_MARA.
        CONTINUE.
      ENDIF.
    ENDIF.
    IF NOT P_C219_2 IS INITIAL.
      READ TABLE IT_VALS WITH KEY ATNAM = L_219_2.
      IF SY-SUBRC <> 0 OR IT_VALS-ATWRT NE P_V219_2.
        DELETE IT_MARA.
        CONTINUE.
      ENDIF.
    ENDIF.
    IF NOT P_C219_3 IS INITIAL.
      READ TABLE IT_VALS WITH KEY ATNAM = L_219_3.
      IF SY-SUBRC <> 0 OR IT_VALS-ATWRT NE P_V219_3.
        DELETE IT_MARA.
        CONTINUE.
      ENDIF.
    ENDIF.
** End of change

    IT_DATA-MATNR = IT_MARA-MATNR.      CLEAR: IT_VALS.
    READ TABLE IT_VALS WITH KEY ATNAM = 'P_PERF_YN'.
    IT_DATA-PERF  = IT_VALS-ATWRT(1)  . CLEAR: IT_VALS.
    READ TABLE IT_VALS WITH KEY ATNAM = 'P_PROD_FLAG'.
    IT_DATA-PROD  = IT_VALS-ATWRT(1)  . CLEAR: IT_VALS.


    PERFORM READ_MATNR_BASIC_TEXT USING IT_DATA-MATNR IT_DATA-NOTE.


    APPEND IT_DATA               .
  ENDLOOP.
ENDFORM.                    " DATA_INITIAL

*&---------------------------------------------------------------------*
*&      Form  set_parameter
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER.
* Company
  CLEAR P_COMPANY.
  P_COMPANY = 'HMMA'.

* Model
  CLEAR : MODEL_LIST, MODEL_VALUE.
  NAME = 'P_MODEL'.
  PERFORM SET_FIELD02.
  PERFORM CALL_FUNCTION USING MODEL_LIST.
ENDFORM.                    " set_parameter

*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PLANT_LIST  text
*----------------------------------------------------------------------*
FORM CALL_FUNCTION USING    P_LIST.
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = NAME
            VALUES = P_LIST.
ENDFORM.                    " CALL_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD02.
  DATA: L_ATINN              LIKE CABN-ATINN,
        L_ATNAM              LIKE CABN-ATNAM,
        L_ATWTB              LIKE CAWNT-ATWTB,
        L_ATWRT              LIKE AUSP-ATWRT.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    MODEL_VALUE-TEXT = L_ATWTB.  " ZTPP_VEH_MODEL-NAME.
    MODEL_VALUE-KEY  = L_ATWRT.  " ZTPP_VEH_MODEL-MODEL.
    APPEND MODEL_VALUE TO MODEL_LIST .
  ENDSELECT.

  PERFORM LIST_BOX_FUNCTION USING 'P_MODEL'.
  IF P_MODEL IS INITIAL.
    READ TABLE MODEL_LIST INTO MODEL_VALUE  INDEX 1.
    P_MODEL = MODEL_VALUE-KEY.
  ENDIF.
ENDFORM.                    " SET_FIELD02
*&---------------------------------------------------------------------*
*&      Form  SETUP_PARAMETER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETUP_PARAMETER.
  IF P_MODEL <> SPACE.
    CLEAR R_MODEL.
    REFRESH R_MODEL.
    R_MODEL-OPTION = 'EQ'.
    R_MODEL-SIGN = 'I'.
    R_MODEL-LOW = P_MODEL.
    APPEND R_MODEL.
  ELSE.
    CLEAR R_MODEL.
    REFRESH R_MODEL.
  ENDIF.
ENDFORM.                    " SETUP_PARAMETER

*&---------------------------------------------------------------------*
*&      Form  setting_internal_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SETTING_INTERNAL_FIELDS.
* Model
  CLEAR : MODEL_LIST, MODEL_VALUE.
  NAME = 'IT_NEW_APP227-MODEL'.
  PERFORM SET_FIELD02.
  PERFORM CALL_FUNCTION USING MODEL_LIST.
ENDFORM.                    " setting_internal_fields

*&---------------------------------------------------------------------*
*&      Module  user_command_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  SV_CODE = OK_CODE.
  CLEAR: OK_CODE.
  CASE SV_CODE.
    WHEN 'SEL'.  "SEARCH.
      IF WA_CHANGE = 'X'.
        EXIT.
      ENDIF.
      CLEAR: IT_DATA, IT_DATA[].
      PERFORM SETUP_PARAMETER.
      PERFORM DATA_INITIAL.
      PERFORM DATA_FILLING.
    WHEN 'ASEL'.
      IT_DATA-MARK = 'X'.
      MODIFY IT_DATA TRANSPORTING MARK WHERE MARK = SPACE.
    WHEN 'DSEL'.
      IT_DATA-MARK = ' '.
      MODIFY IT_DATA TRANSPORTING MARK WHERE MARK = 'X'  .
    WHEN 'COLOR'.  " Color Workorder's PERF
      CLEAR: IT_ALC,   IT_CONF,   IT_CHAR,  WA_COLOR, WA_COLOR_DIS,
             IT_ALC[], IT_CONF[], IT_CHAR[].
      READ TABLE IT_DATA WITH KEY MARK = 'X'.
      IF SY-SUBRC = 0.
        PERFORM READ_WO_COLOR USING IT_DATA-MATNR.
        PERFORM MOVE_HEADER                      .
      ELSE.
        MESSAGE W001 WITH TEXT-002 .
        EXIT.
      ENDIF.
      PERFORM INSERT_HDVALS    .
      PERFORM CHECK_MULTIPLE   .
      WA_ORDER = IT_DATA-MATNR .
      PERFORM READ_CLASSIFICATION USING WA_ORDER.
      PERFORM DISPLAY_219 .
      PERFORM CHECK_ALCERRORC  USING 'U'  .
      PERFORM CHECK_ALCERRORC  USING 'C'  .
      DESCRIBE TABLE IT_ALC   LINES TC_301-LINES.
      CALL SCREEN 300 .
    WHEN 'ACON'  .     " Selected Item - All confirmation
      LOOP AT IT_DATA WHERE MARK = 'X'.
        WA_ORDER  = IT_DATA-MATNR .
        PERFORM CONFIRMATION_ORDER.
      ENDLOOP.
      CLEAR: IT_DATA, IT_DATA[].
      PERFORM DATA_INITIAL.
      PERFORM DATA_FILLING.
    WHEN 'SAVE'  .
      PERFORM SAVE_CONFIRM.
      WA_CHANGE = ' '.
    WHEN 'CHANGE'.
      IF WA_CHANGE = 'X'.
        WA_CHANGE = ' '.
      ELSE.
        WA_CHANGE = 'X'.
      ENDIF.
    WHEN 'DEXCEL'.              " Excel Download..
      PERFORM DOWNLOAD_EXCEL_HD   .
    WHEN 'FREE'.  " Basic2 -----> Classification
      PERFORM BASIC2_TRANSER_CLASS_SELECTION.
*      PERFORM basic2_transer_class.
    WHEN 'CAL_IPP'.             " Call the IPP202 Program..
      CALL TRANSACTION 'ZPPI202'  .
** Changed by Furong on 10/27/08
    WHEN 'SORT'.
      PERFORM SORT_DATA.
** End of change
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " exit  INPUT

*&---------------------------------------------------------------------*
*&      Module  modify_data  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_DATA INPUT.
  MODIFY IT_DATA INDEX TC_100-CURRENT_LINE.
ENDMODULE.                 " modify_data  INPUT
*&---------------------------------------------------------------------*
*&      Module  worder_search_help  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE WORDER_SEARCH_HELP INPUT.
  DATA: L_DYNAME             LIKE SY-REPID,
        L_DYNUM              LIKE SY-DYNNR.

  CLEAR REASON_TAB. REFRESH REASON_TAB.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = 'P_WORDER'.
  APPEND DYNPFIELDS.
  L_DYNAME = SY-REPID .
  L_DYNUM  = SY-DYNNR .
  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = L_DYNAME
            DYNUMB               = L_DYNUM
            DETERMINE_LOOP_INDEX = 'X'
       TABLES
            DYNPFIELDS           = DYNPFIELDS
       EXCEPTIONS
            OTHERS               = 9.

  READ TABLE DYNPFIELDS WITH KEY FIELDNAME = 'P_WORDER'.
* -- WORK ORDER SEARCH
  DATA: L_MODEL(06),
        L_WORDER(20).

  CLEAR: L_MODEL, L_WORDER, REASON_TAB, REASON_TAB[].
  CONCATENATE P_MODEL '%' INTO L_MODEL.
  CONCATENATE P_WORDER '%' INTO L_WORDER.

  LOOP AT IT_DATA .
    REASON_TAB-CODE = IT_DATA-MATNR .
    APPEND REASON_TAB.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD    = 'CODE'
            DYNPPROG    = SY-CPROG
            DYNPNR      = SY-DYNNR
            DYNPROFIELD = 'P_WORDER'
            VALUE_ORG   = 'S'
       TABLES
            VALUE_TAB   = REASON_TAB.

  IF SY-SUBRC  <> 0.
    MESSAGE I000 WITH 'NOT FOUND....'.
  ENDIF.
ENDMODULE.                 " worder_search_help  INPUT

*&---------------------------------------------------------------------*
*&      Module  wo_c_search_help  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE WO_C_SEARCH_HELP INPUT.
  DATA: L_DYNAM              LIKE SY-REPID,
        L_DYNU               LIKE SY-DYNNR.

  CLEAR REASON_TAB. REFRESH REASON_TAB.
  CLEAR DYNPFIELDS. REFRESH DYNPFIELDS.
  DYNPFIELDS-FIELDNAME = 'IT_NEW_APP227-FORDER'.
  APPEND DYNPFIELDS.
  L_DYNAM  = SY-REPID .
  L_DYNU   = SY-DYNNR .

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME               = L_DYNAM
            DYNUMB               = L_DYNU
            DETERMINE_LOOP_INDEX = 'X'
       TABLES
            DYNPFIELDS           = DYNPFIELDS
       EXCEPTIONS
            OTHERS               = 9.

  READ TABLE DYNPFIELDS WITH KEY FIELDNAME = 'IT_NEW_APP227-FORDER'.
* -- WORK ORDER SEARCH
  SELECT DISTINCT MATNR
    INTO REASON_TAB-CODE
    FROM MARA
    WHERE MTART = 'WOCL' .
    APPEND REASON_TAB.
  ENDSELECT.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD    = 'CODE'
            DYNPPROG    = SY-CPROG
            DYNPNR      = SY-DYNNR
*            dynprofield = 'P_WORDER'
            VALUE_ORG   = 'S'
       TABLES
            VALUE_TAB   = REASON_TAB.

  IF SY-SUBRC  <> 0.
    MESSAGE I000 WITH 'NOT FOUND....'.
  ENDIF.

ENDMODULE.                 " wo_c_search_help  INPUT

*&---------------------------------------------------------------------*
*&      Form  data_filling
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_FILLING.
  DATA: L_ATWRT                    LIKE AUSP-ATWRT,
        L_ATFLV                    LIKE AUSP-ATFLV,
        L_DATE(8)                  TYPE N         ,
        L_RECS                     TYPE I         ,
        L_ATINN01                  LIKE AUSP-ATINN,
        L_ATINN02                  LIKE AUSP-ATINN,
        L_ATINN03                  LIKE AUSP-ATINN,
        L_ATINN04                  LIKE AUSP-ATINN,
        L_ATINN05                  LIKE AUSP-ATINN,
        L_ATINN06                  LIKE AUSP-ATINN,
        L_ATINN07                  LIKE AUSP-ATINN,
        L_ATINN08                  LIKE AUSP-ATINN,
        L_ATINN09                  LIKE AUSP-ATINN,
        L_ATINN10                  LIKE AUSP-ATINN,
        L_ATINN11                  LIKE AUSP-ATINN.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_PERF_YN'
       IMPORTING
            OUTPUT = L_ATINN01.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_UPDATE_ALC_DATE1'
       IMPORTING
            OUTPUT = L_ATINN02.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_UPDATE_ALC_DATE2'
       IMPORTING
            OUTPUT = L_ATINN03.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_MOD_QTY'
       IMPORTING
            OUTPUT = L_ATINN04.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_MI'
       IMPORTING
            OUTPUT = L_ATINN05.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_OCN'
       IMPORTING
            OUTPUT = L_ATINN06.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_VERSION'
       IMPORTING
            OUTPUT = L_ATINN07.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_GEN_DATE'
       IMPORTING
            OUTPUT = L_ATINN08.

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = 'P_HPC_STATUS'
       IMPORTING
            OUTPUT = L_ATINN09.

  CLEAR: WA_TOTAL.
  LOOP AT IT_DATA.
    L_RECS         = L_RECS + 1      .
    IT_DATA-NO     = L_RECS          .
    IT_DATA-WO     = IT_DATA-MATNR(9).
    IT_DATA-NATION = IT_DATA-MATNR+9(5).

    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN01
       AND KLART = '001'  .
    IT_DATA-PERF = L_ATWRT(1) .  CLEAR: L_ATWRT.

    SELECT SINGLE ATFLV INTO L_ATFLV
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN03
       AND KLART = '001'  .

    IF L_ATFLV = 0 OR SY-SUBRC NE 0.
      SELECT SINGLE ATFLV INTO L_ATFLV
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN02
       AND KLART = '001'  .
      IF SY-SUBRC = 0      .
        IT_DATA-PDATE = L_DATE = L_ATFLV .  CLEAR: L_ATFLV.
      ELSE.
        IT_DATA-PDATE = '19000101'       .
      ENDIF.
    ELSE.
      IT_DATA-PDATE = L_DATE = L_ATFLV .  CLEAR: L_ATFLV.
    ENDIF.

    SELECT SINGLE ATFLV INTO L_ATFLV
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN04
       AND KLART = '001'  .
    IT_DATA-MQTY = L_ATFLV .      CLEAR: L_ATFLV.

    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN05
       AND KLART = '001'  .
    IT_DATA-MI   = L_ATWRT .     CLEAR: L_ATWRT.

    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN06
       AND KLART = '001'  .
    IT_DATA-OCN = L_ATWRT .     CLEAR: L_ATWRT.

    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN07
       AND KLART = '001'  .
    IT_DATA-VER = L_ATWRT .     CLEAR: L_ATWRT.

    SELECT SINGLE ATFLV INTO L_ATFLV
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN08
       AND KLART = '001'  .
    IF SY-SUBRC = 0      .
      IT_DATA-RDATE = L_DATE = L_ATFLV .  CLEAR: L_ATFLV.
    ELSE.
      IT_DATA-RDATE = '19000101'       .
    ENDIF.

    SELECT SINGLE ATWRT INTO L_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_DATA-MATNR
       AND ATINN = L_ATINN09
       AND KLART = '001'  .
    IT_DATA-HSTATS = L_ATWRT .     CLEAR: L_ATWRT.

    WA_TOTAL = WA_TOTAL + IT_DATA-MQTY .
    MODIFY IT_DATA.
  ENDLOOP.
  DESCRIBE TABLE IT_DATA LINES TC_100-LINES.
ENDFORM.                    " data_filling

*&---------------------------------------------------------------------*
*&      Module  initialization  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIALIZATION OUTPUT.
  IF WA_INIT_FLG IS INITIAL.
    PERFORM SET_PARAMETER.
    WA_INIT_FLG = 'X'.
  ENDIF.
ENDMODULE.                 " initialization  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0300 OUTPUT.
  SET PF-STATUS 'STATUS300' EXCLUDING IT_HID_MENU.
  SET TITLEBAR 'SCREEN300'.
ENDMODULE.                 " status_0300  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  status_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  DATA: L_RECS               TYPE I.

  " Display Screen Header Text..
  DESCRIBE TABLE IT_DATA     LINES L_RECS.
  IF L_RECS > 0.
    WA_RECS = L_RECS .
    CONDENSE WA_RECS .
    CONCATENATE '(' WA_RECS ')'  INTO WA_RECS .
  ELSE.
    CLEAR: WA_RECS.
  ENDIF.

  SET PF-STATUS 'STATUS100'.
  SET TITLEBAR 'SCREEN100'  WITH  WA_RECS .
ENDMODULE.                 " status_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CONFIRMATION_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CONFIRMATION_ORDER.
  DATA: LT_VALS           LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.
  DATA: L_MATNR           LIKE MARA-MATNR ,
        L_INDEX           LIKE SY-TABIX   ,
        L_FLAG            TYPE C          ,
        LT_COL            LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

*  LOOP AT it_data WHERE mark = 'X'.
  READ TABLE IT_DATA WITH KEY MATNR = WA_ORDER .
  L_INDEX = SY-TABIX.
  CLEAR: LT_VALS, LT_VALS[].
  PERFORM CHECK_SALES_ORDER   USING L_FLAG.
  IF L_FLAG = 'X'.
    EXIT.
  ENDIF.

  " Header Confirm..
  LT_VALS-ATNAM = 'P_PROD_FLAG'.  LT_VALS-ATWRT = 'Y' .  APPEND LT_VALS.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = IT_DATA-MATNR
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = LT_VALS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  CONCATENATE IT_DATA-MATNR '%' INTO L_MATNR .
  " Color Order Confirm..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COLOR
    FROM MARA
   WHERE MATNR LIKE L_MATNR
     AND MTART = 'WOCL'
*     AND mbrsh = 'A'
     AND KZKFG = SPACE .

  LOOP AT IT_COLOR.
    CLEAR: LT_COL, LT_COL[].
*   lt_col-atnam = 'P_PROD_FLAG'. lt_col-atwrt = 'Y'. APPEND lt_col.
    LT_COL-ATNAM = 'P_UPDATE_ALC_DATE1'.
    LT_COL-ATWRT = SY-DATUM.                          APPEND LT_COL.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = IT_COLOR-MATNR
              MODE         = 'W'
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = LT_COL
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    " Insert Record into the Table ZTPP_SPEC.
    PERFORM INSERT_SPEC  USING P_MODEL       IT_COLOR-MATNR(14)
                        IT_COLOR-MATNR+14(2) IT_COLOR-MATNR+16(2).
  ENDLOOP.
  MESSAGE S002 WITH IT_DATA-MATNR TEXT-008 .
ENDFORM.                    " CONFIRMATION_ORDER

*&---------------------------------------------------------------------*
*&      Module  status_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'STATUS200'.
  SET TITLEBAR 'SCREEN200'.
ENDMODULE.                 " status_0200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  initial_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_200 OUTPUT.
  DATA: L_MATNR         LIKE MARA-MATNR.
  CHECK WA_FLAG_200     IS INITIAL.
  READ TABLE IT_DATA  WITH KEY MARK = 'X'.
  CONCATENATE IT_DATA-MATNR '%' INTO L_MATNR.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COLOR
    FROM MARA
   WHERE MATNR LIKE L_MATNR
     AND MTART = 'WOCL'
*     AND mbrsh = 'A'
     AND KZKFG = SPACE .
  LOOP AT IT_COLOR.
    CLEAR: WA_ATWRT.
    SELECT SINGLE ATWRT INTO WA_ATWRT
      FROM AUSP
     WHERE OBJEK = IT_COLOR-MATNR
       AND ATINN = WA_ATINN
       AND KLART = '001'.

    IT_COLOR-PERF = WA_ATWRT(1) .
    MODIFY IT_COLOR.
  ENDLOOP.
  WA_FLAG_200 = 'X'.
ENDMODULE.                 " initial_200  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_200 INPUT.
  SV_CODE = OK_CODE.
  CLEAR: OK_CODE.
  CASE SV_CODE.
    WHEN 'ENTER' OR 'CANCEL' .
      CLEAR: WA_FLAG_200.
      LEAVE TO SCREEN 0 .
  ENDCASE.
ENDMODULE.                 " user_command_200  INPUT
*&---------------------------------------------------------------------*
*&      Form  basic2_transer_class
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM BASIC2_TRANSER_CLASS.
  DATA: L_TABIX              LIKE SY-TABIX,
        L_EXIST              TYPE C,
        L_FREEZ              TYPE C,
        L_PERF               TYPE C,
        L_CNT                TYPE I,    " ALC Error count...
        L_FLAG(1).
*  sort it_data.
  REFRESH: IT_CONF, IT_CHAR.
  CLEAR :  IT_CONF, IT_CHAR.
*---start wskim 03/07/2005
  DATA : WA_DATA LIKE  ZSPP_WO_CONF.

** added by Furong on  08/07/2006

  LOOP AT IT_DATA WHERE MARK = 'X'.
    IT_MATNR-MATNR = IT_DATA-MATNR.
    APPEND IT_MATNR.
    CLEAR: IT_MATNR.

    CONCATENATE IT_DATA-MATNR '%' INTO L_MATNR .
    SELECT MATNR FROM MARA
           APPENDING TABLE IT_MATNR
           WHERE MATNR LIKE L_MATNR
             AND MTART = 'WOCL'.
  ENDLOOP.

  LOOP AT IT_MATNR.
    PERFORM BDC_MM02 USING IT_MATNR-MATNR L_FLAG.
    CLEAR:IT_MATNR, L_FLAG.
  ENDLOOP.

  CLEAR: IT_MATNR, IT_MATNR[].
** end of change

  CLEAR: RCV_JOBS, SND_JOBS.
  LOOP AT IT_DATA WHERE MARK = 'X'.
    MOVE-CORRESPONDING IT_DATA TO WA_DATA.
    DO.
** changed by furong
*      MOVE-CORRESPONDING it_data TO wa_data.
*      IF wa_data-mark EQ space.
*        EXIT.
*      ENDIF.
** end of change

      CALL FUNCTION 'Z_FPP_WORKORDER_FREE_PARALLEL'
        STARTING NEW TASK TASKNAME DESTINATION IN GROUP 'PG_SEQ'
          PERFORMING RETURN_01 ON END OF TASK
             EXPORTING
               LT_DATA                = WA_DATA
               LP_MODEL               = P_MODEL
               LP_R                   = W_ALC_FLAG
             TABLES
               ET_DATA                = RE_DATA
               ET_ALC_NAME            = IT_ALC_NAME
             EXCEPTIONS
               COMMUNICATION_FAILURE  = 1
               SYSTEM_FAILURE         = 2
               RESOURCE_FAILURE       = 3 .
      CASE SY-SUBRC.
        WHEN 0.
          TASKNAME = TASKNAME + 1.
          SND_JOBS = SND_JOBS  + 1.
*          READ TABLE re_data INDEX 1.
*          MOVE-CORRESPONDING  re_data TO it_data.
          CLEAR WA_DATA.
          CLEAR: IT_DATA.
          CLEAR: EXCP_FLAG.
          EXIT.
        WHEN 1 OR 2.
          EXCP_FLAG = 'X'.
*          EXIT.
        WHEN 3.
*Receive reply to asynchronous RFC calls
          IF EXCP_FLAG = SPACE.
            EXCP_FLAG = 'X'.
*First attempt for RESOURCE_Failure handling
            WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '0.01' SECONDS.
          ELSE.
*Second attempt for RESOURCE_Failure handling
            WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '0.1' SECONDS.
          ENDIF.
          IF SY-SUBRC = 0.
            CLEAR EXCP_FLAG. " Reset flag
          ENDIF.
      ENDCASE.
*      CLEAR : wa_data,it_data.
    ENDDO.
** remarked by Furong on 08/07/2006
*    WAIT UNTIL rcv_jobs >= snd_jobs.
*    IF sy-subrc = 0.
*      COMMIT WORK AND WAIT.
*    ENDIF.
** end of change.
  ENDLOOP.

  WAIT UNTIL RCV_JOBS >= SND_JOBS.
  IF SY-SUBRC = 0.
    MESSAGE I002 WITH TEXT-T02 RCV_JOBS SND_JOBS.
    COMMIT WORK AND WAIT.
  ELSE.
    MESSAGE I002 WITH TEXT-T01 RCV_JOBS SND_JOBS.
  ENDIF.

** changed by Furong on 08/07/2006


*    WRITE: 'SY:', sy-subrc,sy-msgid,sy-msgty,sy-msgv1,sy-msgv2,sy-msgv3
  .
*    WRITE: / 'Received jobs =', rcv_jobs.
*    WRITE: / 'Sent jobs =', snd_jobs.
*   loop at it_log.
*     write: /1 it_log-matnr, 20 it_log-message.
*   endloop.

** end of change.

*  LOOP AT it_data WHERE mark = 'X'      .
*    CLEAR: it_alc, it_alc[].  CLEAR: it_char, it_char[].
**wo_color
*    PERFORM find_wo_color TABLES it_matnr
*                          USING  it_data-matnr.
*    LOOP AT it_matnr.
*      CLEAR: l_freez, it_char, it_char[].
*      wa_color = it_matnr-matnr .
*      PERFORM bdc_mm02 USING wa_color l_freez.
*      IF l_freez = 'X'.  CONTINUE.  ENDIF.
*      REFRESH it_conf. CLEAR it_conf .
*      REFRESH it_char. CLEAR it_char .
*      PERFORM find_configure_variant TABLES it_conf
*                                     USING it_matnr-cuobf.
*      LOOP AT it_conf.
*        MOVE-CORRESPONDING it_conf TO it_char.
*        APPEND it_char. CLEAR it_char.
*      ENDLOOP.
*      READ TABLE it_char WITH KEY atnam = 'P_MOD_DATE'.
*      IF sy-subrc = 0.
*        l_tabix   = sy-tabix.
*        it_char-atwrt = sy-datum.
*        MODIFY it_char INDEX l_tabix.
*      ELSE.
*        it_char-atnam = 'P_MOD_DATE'.  it_char-atwrt = sy-datum.
*        APPEND it_char.
*      ENDIF.
*      PERFORM check_alcerror  USING 'C'  .
*      DESCRIBE TABLE it_alc LINES l_cnt  .
*      READ TABLE it_char WITH KEY atnam = 'P_PERF_YN' .
*      IF sy-subrc = 0.
*        l_tabix = sy-tabix.
*        l_exist = 'X'.
*      ELSE.
*        CLEAR: l_exist.
*      ENDIF.
*      CLEAR: it_char.
*      IF l_cnt > 0 .
*        l_perf = it_data-perf  = 'N'.
*        LOOP AT it_alc .
*          CONCATENATE 'P_' it_alc-knnam+6(9) INTO it_char-atnam .
*          it_char-atwrt = '????' .    APPEND it_char  .
*        ENDLOOP.
*        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'N' .
*      ELSE.
*        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'Y' .
*        it_data-perf  = 'Y'.
*      ENDIF.
*      READ TABLE it_char WITH KEY atnam = 'P_PERF_YN' .
*      IF l_exist = 'X'.
*        MODIFY it_char INDEX l_tabix TRANSPORTING atwrt .
*      ELSE.
*        APPEND it_char.
*      ENDIF.
*      DELETE it_char WHERE atnam = 'P_HPC_STATUS'.
*      PERFORM ftp_handling_master TABLES it_char
*                                  USING it_matnr-matnr .
*    ENDLOOP.
**wo_head
*    CLEAR: it_alc, it_alc[].  CLEAR: it_char, it_char[], l_freez.
*    CLEAR: it_conf, it_conf[].
*    wa_order = it_data-matnr .
*    PERFORM bdc_mm02 USING wa_order  l_freez.
*    IF l_freez = 'X'.  CONTINUE.  ENDIF.
*
*    PERFORM find_cuobf_field USING it_data-matnr  CHANGING wa_cuobf.
*    PERFORM find_configure_variant TABLES it_conf USING wa_cuobf.
*
*    " Make the P_VIN_SPEC Characteristics...
*    PERFORM make_vinspec     .
*
*    " Elemenate the P_VIN Characteristics...
*    DELETE it_conf WHERE atnam = 'P_VIN_123'.
*    DELETE it_conf WHERE atnam = 'P_VIN_4'  .
*    DELETE it_conf WHERE atnam = 'P_VIN_5'  .
*    DELETE it_conf WHERE atnam = 'P_VIN_6'  .
*    DELETE it_conf WHERE atnam = 'P_VIN_7'  .
*    DELETE it_conf WHERE atnam = 'P_VIN_8'  .
*    DELETE it_conf WHERE atnam = 'P_VIN_9'  .
*    DELETE it_conf WHERE atnam = 'P_VIN_10' .
*    DELETE it_conf WHERE atnam = 'P_VIN_11' .
**Issue #    ,Requested by BWPARK
**Changed by wskim, ON 01/21/2005
**-----Start
*    DATA : t_name(15),t_num(3).
*    CLEAR :t_name,t_num.
*    LOOP AT it_conf.
**      MOVE-CORRESPONDING it_conf TO it_char.
**      APPEND it_char. CLEAR it_char.
*      SEARCH it_conf-atnam FOR 'P_TECH_SPEC'.
*      IF sy-subrc <> 0.
*        MOVE-CORRESPONDING it_conf TO it_char.
*        APPEND it_char. CLEAR it_char.
*      ELSE.
*        MOVE it_conf-atnam+12(3) TO t_num.
*        IF t_num < '006'.
*          MOVE-CORRESPONDING it_conf TO it_char.
*          APPEND it_char. CLEAR it_char.
*        ENDIF.
*      ENDIF.
*      CLEAR t_num.
**-----End
*    ENDLOOP.
*
*    READ TABLE it_char WITH KEY atnam = 'P_MOD_DATE'.
*    IF sy-subrc = 0.
*      l_tabix   = sy-tabix.
*      it_char-atwrt = sy-datum.
*      MODIFY it_char INDEX l_tabix.
*    ELSE.
*      it_char-atnam = 'P_MOD_DATE'.  it_char-atwrt = sy-datum.
*      APPEND it_char.
*    ENDIF.
*    PERFORM check_alcerror  USING 'U'  .
*    DESCRIBE TABLE it_alc LINES l_cnt  .
*    READ TABLE it_char WITH KEY atnam = 'P_PERF_YN' .
*    IF sy-subrc = 0.
*      l_tabix = sy-tabix.
*      l_exist = 'X'.
*    ELSE.
*      CLEAR: l_exist.
*    ENDIF.
*    CLEAR: it_char.
*    IF l_cnt > 0 .
*      it_data-perf  = 'N'.
*      LOOP AT it_alc .
*        CONCATENATE 'P_' it_alc-knnam+6(9) INTO it_char-atnam.
*        it_char-atwrt = '????' .   APPEND it_char  .
*      ENDLOOP.
*      it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'N' .
*    ELSE.
*      IF l_perf = 'N'.
*        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'N' .
*      ELSE.
*        it_char-atnam = 'P_PERF_YN'.   it_char-atwrt = 'Y' .
*        it_data-perf  = 'Y'.
*      ENDIF.
*    ENDIF.
*    IF l_exist = 'X'.
*      MODIFY it_char INDEX l_tabix TRANSPORTING atwrt .
*    ELSE.
*      APPEND it_char.
*    ENDIF.
*    DELETE it_char WHERE atnam = 'P_PROD_FLAG' .
*    DELETE it_char WHERE atnam = 'P_HPC_STATUS'.
*    PERFORM ftp_handling_master TABLES it_char
*                                USING it_data-matnr.
*
*    MODIFY it_data.
**Issue Number : PP-, Requested by Hur
**Changed on 2004/11/30, by WSKIM
**error
**---Start
*    CLEAR : l_perf.
**---End
*  ENDLOOP.
*  MESSAGE s001 WITH text-011 .
*---end
ENDFORM.                    " basic2_transer_class
*&---------------------------------------------------------------------*
*&      Form  find_cuobf_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIND_CUOBF_FIELD USING    P_MATNR   CHANGING P_CUOBF.
  SELECT SINGLE CUOBF FROM MARA
         INTO P_CUOBF
         WHERE MATNR = P_MATNR.
ENDFORM.                    " find_cuobf_field

*&---------------------------------------------------------------------*
*&      Form  find_configure_variant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIND_CONFIGURE_VARIANT TABLES   IT_CONF    USING    P_CUOBF.
  CALL FUNCTION 'VC_I_GET_CONFIGURATION_IBASE'
       EXPORTING
            INSTANCE           = P_CUOBF
            LANGUAGE           = SY-LANGU
       TABLES
            CONFIGURATION      = IT_CONF
       EXCEPTIONS
            INSTANCE_NOT_FOUND = 1
            OTHERS             = 2.
ENDFORM.                    " find_configure_variant

*&---------------------------------------------------------------------*
*&      Form  find_wo_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIND_WO_COLOR TABLES   IT_MATNR
                   USING    P_MATNR.
  DATA L_MATNR LIKE MARA-MATNR.
  CONCATENATE P_MATNR '%' INTO L_MATNR .
  SELECT MATNR CUOBF FROM MARA
         INTO TABLE IT_MATNR
         WHERE MATNR LIKE L_MATNR
           AND MTART = 'WOCL'     .
  IF SY-SUBRC NE 0.

  ENDIF.
ENDFORM.                    " find_wo_color
*&---------------------------------------------------------------------*
*&      Form  ftp_handling_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FTP_HANDLING_MASTER TABLES   IT_CHAR
                         USING    P_MATNR.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = P_MATNR
            MODE         = 'W'
            CTYPE        = '001'
            DISPLAY      = 'X'
       TABLES
            VAL_TABLE    = IT_CHAR
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC <> 0.
    MESSAGE E001 WITH 'Failed Update '.
  ELSE.
    MESSAGE S001 WITH 'Completed Update successfully'.
  ENDIF.
ENDFORM.                    " ftp_handling_master

*&---------------------------------------------------------------------*
*&      Form  insert_spec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_SPEC  USING PA_MODEL  PA_MATNR  PA_EXTC  PA_INTC.
  CLEAR: WA_ZTPP_SPEC .
  WA_ZTPP_SPEC-MANDT   = SY-MANDT.
  WA_ZTPP_SPEC-KEYCODE = 'AALC'  .
  WA_ZTPP_SPEC-OPCOUNT = '1'     .
  WA_ZTPP_SPEC-MARK    = 'I'     .
  WA_ZTPP_SPEC-PLANT   = '1'     .
  WA_ZTPP_SPEC-MODEL   = PA_MODEL.
  WA_ZTPP_SPEC-WORDER  = PA_MATNR.
  WA_ZTPP_SPEC-EXTC    = PA_EXTC .
  WA_ZTPP_SPEC-INTC    = PA_INTC .
  WA_ZTPP_SPEC-ZUSER   = SY-UNAME.
  WA_ZTPP_SPEC-ERDAT   = SY-DATUM.
  WA_ZTPP_SPEC-ERZET   = SY-UZEIT.
  WA_ZTPP_SPEC-ERNAM   = SY-UNAME.
  MODIFY ZTPP_SPEC FROM WA_ZTPP_SPEC .
ENDFORM.                    " insert_spec

*&---------------------------------------------------------------------*
*&      Form  DELETE_spec
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_SPEC  USING PA_MODEL  PA_MATNR  PA_EXTC  PA_INTC.
  CLEAR: WA_ZTPP_SPEC .
  DELETE FROM ZTPP_SPEC WHERE MODEL  = PA_MODEL
                          AND WORDER = PA_MATNR
                          AND EXTC   = PA_EXTC
                          AND INTC   = PA_INTC .
ENDFORM.                    " insert_spec

*&---------------------------------------------------------------------*
*&      Module  initial_300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INITIAL_300 OUTPUT.
  WA_COLOR_DIS = WA_COLOR+14(4).
  IF WA_FLG IS INITIAL.
    IF WA_COLOR_DIS IS INITIAL.
      PERFORM READ_FIRST_COLOR.
    ENDIF.
    PERFORM GET_DISPLAY   .
    WA_FLG = 'X'     .
  ENDIF.
ENDMODULE.                 " initial_300

*&---------------------------------------------------------------------*
*&      Form  get_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DISPLAY.
  DATA: L_MATERIAL             LIKE MARA-MATNR.

  CLEAR: WA_WOSUM.

  SELECT SINGLE * INTO CORRESPONDING FIELDS OF WA_WOSUM
    FROM ZTPP_WOSUM
   WHERE WO_SER = WA_COLOR(9)
     AND NATION = WA_COLOR+9(3)
     AND DEALER = WA_COLOR+12(2)
     AND EXTC   = WA_COLOR+14(2)
     AND INTC   = WA_COLOR+16(2) .

  SELECT SINGLE MAKTX
         INTO WA_CAR
         FROM MAKT
         WHERE MATNR EQ WA_WOSUM-FSC
           AND SPRAS EQ SY-LANGU.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = WA_ORDER
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = IT_HDVALS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = WA_COLOR
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = IT_CLVALS
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  READ TABLE IT_CLVALS INDEX 1 .
  WA_MI    = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 2 .
  WA_OCN   = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 3 .
  WA_VERS  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 4 .
  WA_CRDAT = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 5 .
  WA_MDDAT = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 6 .
  WA_DEST  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 7 .
  WA_ALDAT = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 8 .
  WA_PERF  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 9 .
  WA_INITQ = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 10.
  WA_MODQ  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 11.
  WA_PQTY  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 12.
  WA_FQTY  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 13.
  WA_MQTY  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_CLVALS INDEX 14.
  WA_SQTY  = IT_CLVALS-ATWRT.    CLEAR: IT_CLVALS.
  READ TABLE IT_HDVALS INDEX 1 .
  WA_LCNO  = IT_HDVALS-ATWRT.    CLEAR: IT_HDVALS.
  READ TABLE IT_HDVALS INDEX 2 .
  WA_PLNT  = IT_HDVALS-ATWRT.    CLEAR: IT_HDVALS.
  READ TABLE IT_HDVALS INDEX 3 .
  WA_VIN   = IT_HDVALS-ATWRT.    CLEAR: IT_HDVALS.
*   READ TABLE IT_HDVALS INDEX 1 .
*   WA_MI    = IT_HDvals-ATWRT.    CLEAR: IT_HDvals.
*   READ TABLE IT_HDVALS INDEX 1 .
*   WA_MI    = IT_HDvals-ATWRT.    CLEAR: IT_HDvals.
  READ TABLE IT_HDVALS INDEX 4 .
  WA_PROD  = IT_HDVALS-ATWRT.    CLEAR: IT_HDVALS.
ENDFORM.                    " get_display

*&---------------------------------------------------------------------*
*&      Module  DISPLAY_CONVERSION_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_CONVERSION_0300 OUTPUT.
  " VALUE CONVERSION
  CLEAR: IT_219-219DESC, IT_219-219CODE.

  SELECT SINGLE CLNM  VANM  INTO (IT_219-219DESC, IT_219-219CODE)
    FROM ZTBM_ABXOPVDT
   WHERE CARX   = P_MODEL(2)
     AND CLNO   = IT_219-NO
     AND VALU   = IT_219-219VALS .
*
*  SELECT SINGLE vanm     INTO it_219-219code
*    FROM ztbm_abxopvdt
*   WHERE carx   = p_model(2)
*     AND clno   = it_219-no
*     AND valu   = it_219-219vals .
ENDMODULE.                 " DISPLAY_CONVERSION_0300  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0300 INPUT.
  SV_CODE = OK_CODE.
  CLEAR: OK_CODE.
  CASE SV_CODE.
    WHEN 'ENTER'.
      CLEAR: WA_FLG.
    WHEN 'P+'  .   " Next Head Data..
      CLEAR: WA_COLOR, WA_FLG, IT_ALC, IT_ALC[].
      PERFORM CLEAR_TABLE.
      PERFORM CLEAR_VARIABLE.
      PERFORM READ_HEADER USING '+'.
      PERFORM CHECK_ALCERRORC  USING 'U'  .
      PERFORM CHECK_ALCERRORC  USING 'C'  .
      CLEAR: IT_219, IT_219[].
      PERFORM READ_CLASSIFICATION USING WA_ORDER.
      PERFORM DISPLAY_219 .
    WHEN 'P-'  .   " Previous Head Data..
      CLEAR: WA_COLOR, WA_FLG, IT_ALC, IT_ALC[].
      PERFORM CLEAR_TABLE.
      PERFORM CLEAR_VARIABLE.
      PERFORM READ_HEADER USING '-'.
      PERFORM CHECK_ALCERRORC  USING 'U'  .
      PERFORM CHECK_ALCERRORC  USING 'C'  .
      CLEAR: IT_219, IT_219[].
      PERFORM READ_CLASSIFICATION USING WA_ORDER.
      PERFORM DISPLAY_219 .
    WHEN 'NCODE'.  " Next Color Data.....
      " Check the change data.. & Saving..
      CLEAR:  WA_FLG.
      CLEAR: IT_HPCS, IT_HPCS[].
*     PERFORM clear_table.
*     PERFORM clear_variable.
      PERFORM READ_NEXT_COLOR .
*    WHEN 'ALC_HD'.
*      CLEAR: it_alc, it_alc[].
*      PERFORM check_alcerror  USING 'U'  .
*    WHEN 'ALC_CL'.
*      CLEAR: it_alc, it_alc[].
*      PERFORM check_alcerror  USING 'C'  .
    WHEN 'CHK_HD'.
      CLEAR: IT_HPCS, IT_HPCS[].
      PERFORM COMPARE_HPCS    USING 'U'  .
    WHEN 'CHK_CL'.
      CLEAR: IT_HPCS, IT_HPCS[].
      PERFORM COMPARE_HPCS    USING 'C'  .
    WHEN 'BACK'  .
      " Check the Saving flag..
      CLEAR: WA_POINT, WA_FLG, IT_ALC, IT_ALC[], IT_HPCS, IT_HPCS[].
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'  .
      WA_FLG = ' '     .
    WHEN 'DEXCEL'      .
      PERFORM DOWNLOAD_EXCEL .
    WHEN 'R219' .
      CLEAR: IT_219, IT_219[].
      PERFORM READ_CLASSIFICATION USING WA_ORDER.
      PERFORM DISPLAY_219 .
    WHEN 'CON' .  " Confirmation
      PERFORM CONFIRMATION_ORDER.
      CLEAR: IT_DATA, IT_DATA[].
      PERFORM DATA_INITIAL.
      PERFORM DATA_FILLING.
  ENDCASE.
  DESCRIBE TABLE IT_219   LINES TC_300-LINES.
  DESCRIBE TABLE IT_ALC   LINES TC_301-LINES.
  DESCRIBE TABLE IT_HPCS  LINES TC_302-LINES.
ENDMODULE.                 " USER_COMMAND_0300  INPUT

*&---------------------------------------------------------------------*
*&      Form  display_219
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_219.
  FIELD-SYMBOLS: <FIELD1>   TYPE ANY,
                 <FIELD2>   TYPE ANY.
  DATA: L_FNAME(50)        TYPE C,
        L_NO(3)            TYPE N.

  CLEAR: IT_219, IT_219[].
  DO   9 TIMES.
    L_NO = L_NO + 1.
    IT_219-NO = L_NO+2(1) .
    CONCATENATE 'P_219_'  L_NO+2(1)  INTO L_FNAME.
    ASSIGN (L_FNAME)      TO <FIELD1> .
    READ TABLE IT_RESULT WITH KEY ATNAM = L_FNAME .
    IT_219-219VALS = IT_RESULT-ATWRT .
    IT_219-219CODE = IT_RESULT-ATWTB .
    IT_219-219DESC = IT_RESULT-ATBEZ .
    APPEND IT_219 .
  ENDDO.

  DO  90 TIMES.
    L_NO = L_NO + 1.
    IT_219-NO = L_NO+1(2) .
    CONCATENATE 'P_219_'  L_NO+1(2)  INTO L_FNAME.
    ASSIGN (L_FNAME)      TO <FIELD1> .
    READ TABLE IT_RESULT WITH KEY ATNAM = L_FNAME .
    IT_219-219VALS = IT_RESULT-ATWRT .
    IT_219-219CODE = IT_RESULT-ATWTB .
    IT_219-219DESC = IT_RESULT-ATBEZ .
    APPEND IT_219 .
  ENDDO.

  DO 120 TIMES.
    L_NO = L_NO + 1.
    IT_219-NO = L_NO      .
    CONCATENATE 'P_219_'  L_NO       INTO L_FNAME.
    ASSIGN (L_FNAME)      TO <FIELD1> .
    READ TABLE IT_RESULT WITH KEY ATNAM = L_FNAME .
    IT_219-219VALS = IT_RESULT-ATWRT .
    IT_219-219CODE = IT_RESULT-ATWTB .
    IT_219-219DESC = IT_RESULT-ATBEZ .
    APPEND IT_219 .
  ENDDO.
ENDFORM.                    " display_219

*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDER  text
*----------------------------------------------------------------------*
FORM READ_CLASSIFICATION USING    PA_ORDER.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = PA_ORDER
*     MODE               = 'R'
      CTYPE              = '001'
    TABLES
      VAL_TABLE          = IT_RESULT
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .
ENDFORM.                    " READ_CLASSIFICATION

*&---------------------------------------------------------------------*
*&      Form  read_wo_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA_MATNR  text
*----------------------------------------------------------------------*
FORM READ_WO_COLOR USING    PA_MATNR.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COL
    FROM ZTPP_WOSUM
   WHERE WO_SER = PA_MATNR(9)
     AND NATION = PA_MATNR+9(3)
     AND DEALER = PA_MATNR+12(2).

  CLEAR: IT_COL-AENAM.
  MODIFY IT_COL TRANSPORTING AENAM WHERE AENAM NE SPACE.
  READ TABLE IT_COL INDEX 1.
  CONCATENATE IT_COL-WO_SER IT_COL-NATION IT_COL-DEALER
                            IT_COL-EXTC   IT_COL-INTC   INTO WA_COLOR .
  IT_COL-AENAM = 'X'.
  MODIFY IT_COL INDEX 1 TRANSPORTING AENAM .
ENDFORM.                    " read_wo_color

*&---------------------------------------------------------------------*
*&      Form  read_next_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_NEXT_COLOR.
  DATA: L_CNT                 TYPE I       ,
        L_INDEX               LIKE SY-TABIX.

  DESCRIBE TABLE IT_COL  LINES  L_CNT   .
  READ TABLE IT_COL WITH KEY AENAM = 'X'.
  L_INDEX = SY-TABIX .
  CASE SY-SUBRC.
    WHEN 0.
      CLEAR: IT_COL-AENAM .
      MODIFY IT_COL       INDEX L_INDEX TRANSPORTING AENAM .
      L_INDEX = L_INDEX + 1.
*     IF l_index > l_cnt. l_index = 1.  ENDIF.
      IF L_INDEX > L_CNT.
        L_INDEX = L_CNT.
        MESSAGE W001 WITH TEXT-007.
      ENDIF.
      READ TABLE IT_COL   INDEX L_INDEX.
      IT_COL-AENAM = 'X'.
      CONCATENATE IT_COL-WO_SER IT_COL-NATION  IT_COL-DEALER
                  IT_COL-EXTC   IT_COL-INTC    INTO WA_COLOR .
      MODIFY IT_COL       INDEX L_INDEX TRANSPORTING AENAM.
    WHEN OTHERS.
      READ TABLE IT_COL   INDEX 1.
      CONCATENATE IT_COL-WO_SER IT_COL-NATION  IT_COL-DEALER
                  IT_COL-EXTC   IT_COL-INTC    INTO WA_COLOR .
      IT_COL-AENAM  = 'X'.
      MODIFY IT_COL       INDEX 1 TRANSPORTING AENAM.
  ENDCASE.
ENDFORM.                    " read_next_color

*&---------------------------------------------------------------------*
*&      Form  read_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_HEADER USING PA_TYPE.
  DATA: L_MAX                 TYPE I       ,
        L_SIZE(07)            TYPE C       ,
        L_POS(07)             TYPE C       ,
        L_RECS(20)            TYPE C       .

  DESCRIBE TABLE IT_HEADER    LINES L_MAX .
  L_SIZE = L_MAX .
  IF WA_POINT = 0.
    WA_POINT = 1 .
  ENDIF.

  CASE PA_TYPE.
    WHEN '-'.
      WA_POINT = WA_POINT - 1.
      IF WA_POINT = 0.
        WA_POINT = 1.
        CONCATENATE '( 1/'  L_SIZE  ')'  INTO L_RECS.
        CONDENSE L_RECS.
        MESSAGE W002 WITH TEXT-004 L_RECS.
      ELSE.
        L_POS = WA_POINT.
        CONCATENATE  L_POS '/'  L_SIZE   INTO L_RECS.
        CONDENSE L_RECS.
        MESSAGE S002 WITH TEXT-006 L_RECS.
      ENDIF.
      READ TABLE IT_HEADER   INDEX WA_POINT.
    WHEN '+'.
      WA_POINT = WA_POINT + 1.
      IF WA_POINT > L_MAX.
        WA_POINT = L_MAX.
        CONCATENATE '(' L_SIZE '/'  L_SIZE  ')'  INTO L_RECS.
        CONDENSE L_RECS.
        MESSAGE W002 WITH TEXT-005 L_RECS .
      ELSE.
        L_POS = WA_POINT.
        CONCATENATE   L_POS '/'  L_SIZE   INTO L_RECS.
        CONDENSE L_RECS.
        MESSAGE S002 WITH TEXT-006 L_RECS .
      ENDIF.
      READ TABLE IT_HEADER   INDEX WA_POINT.
  ENDCASE.
  WA_ORDER = IT_HEADER-MATNR.
  PERFORM READ_WO_COLOR USING IT_HEADER-MATNR.
ENDFORM.                    " read_header

*&---------------------------------------------------------------------*
*&      Form  move_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MOVE_HEADER.
  IT_HEADER[] = IT_DATA[].
  DELETE IT_HEADER WHERE MARK = SPACE.
ENDFORM.                    " move_header

*&---------------------------------------------------------------------*
*&      Form  CHECK_ALCERROR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2273   text
*----------------------------------------------------------------------*
FORM CHECK_ALCERROR USING    PA_TYPE .
  DATA: LT_CUKB              LIKE TABLE OF CUKB        WITH HEADER LINE,
        LT_VALS              LIKE TABLE OF IT_VALS     WITH HEADER LINE,
        L_MATNR              LIKE MARA-MATNR,
        L_CHK(10)            TYPE C         ,
        L_MODEL(3)           TYPE C         ,
*        L_LEN(2)             TYPE N         ,
*        L_MODELLEN           TYPE I         ,
        L_CNT(3)             TYPE N         ,
        L_KNNUM              LIKE CUOB-KNNUM,
        L_KNOBJ              LIKE CUCO-KNOBJ,
        L_KNNAM              LIKE CUKB-KNNAM.

  " Temporary conversion...
  L_MODEL = P_MODEL.
*  L_MODELLEN = STRLEN( P_MODEL ) .
*  L_LEN = 10 - ( 3 - L_MODELLEN ) .

  CASE PA_TYPE.
    WHEN 'U'.
      L_MATNR = WA_ORDER.
      CONCATENATE L_MODEL  '_WOHD'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
*     DELETE it_alc WHERE ctype NE 'U' .
    WHEN 'C'.
      L_MATNR = WA_COLOR.
      CONCATENATE L_MODEL  '_WOCL'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
*     DELETE it_alc WHERE ctype NE 'C' .
  ENDCASE.

  CONCATENATE 'D_' P_MODEL '_ALC_'     INTO L_CHK .

  LOOP AT IT_ALC    .
    SELECT SINGLE KNNAM
      INTO CORRESPONDING FIELDS OF IT_ALC
      FROM CUKB
     WHERE KNNUM = IT_ALC-KNNUM .

    IF IT_ALC-KNNAM(10) NE  L_CHK     .
      DELETE IT_ALC      .
      CONTINUE .
    ENDIF.
    IT_ALC-CTYPE = IT_ALC-KNNAM+10(1) .
    MODIFY IT_ALC.
  ENDLOOP.

  LOOP AT IT_ALC .
    CLEAR: LT_VALS.             " , lt_vals[].
    CONCATENATE 'P_' IT_ALC-KNNAM+6(9) INTO LT_VALS-ATNAM.

    READ TABLE IT_CHAR WITH KEY ATNAM = LT_VALS-ATNAM.
*   READ TABLE lt_vals INDEX 1 .
    IF SY-SUBRC =  0  AND IT_CHAR-ATWRT NE SPACE .
*        lt_CHAR-atwrt NE space  OR lt_CHAR-atwrt = '????' .
      DELETE IT_ALC.
    ELSE.
      L_CNT        = L_CNT + 1           .
      IT_ALC-NO    = L_CNT               .
      IT_ALC-COL   = IT_ALC-KNNAM+12(3) .
      SELECT SINGLE KNKTX INTO IT_ALC-COLNM
        FROM CUKBT
       WHERE KNNUM = IT_ALC-KNNUM
         AND SPRAS = SY-LANGU     .
      CONCATENATE TEXT-003 '(' IT_ALC-KNNAM ')' INTO IT_ALC-COLDC .
      MODIFY IT_ALC .
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_ALCERROR

*&---------------------------------------------------------------------*
*&      Form  CHECK_ALCERRORC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2273   text
*----------------------------------------------------------------------*
FORM CHECK_ALCERRORC USING    PA_TYPE .
  DATA: LT_CUKB              LIKE TABLE OF CUKB        WITH HEADER LINE,
        LT_VALS              LIKE TABLE OF IT_VALS     WITH HEADER LINE,
        L_MATNR              LIKE MARA-MATNR,
        L_CHK(10)            TYPE C         ,
        L_MODEL(3)           TYPE C         ,
*        L_LEN(2)             TYPE N         ,
*        L_MODELLEN           TYPE I         ,
        L_CNT(3)             TYPE N         ,
        L_KNNUM              LIKE CUOB-KNNUM,
        L_KNOBJ              LIKE CUCO-KNOBJ,
        L_KNNAM              LIKE CUKB-KNNAM.

  " Temporary conversion...
  L_MODEL = P_MODEL.
*  L_MODELLEN = STRLEN( P_MODEL ) .
*  L_LEN = 10 - ( 3 - L_MODELLEN ) .

  CASE PA_TYPE.
    WHEN 'U'.
      L_MATNR = WA_ORDER.
      CONCATENATE L_MODEL  '_WOHD'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
*     DELETE it_alc WHERE ctype NE 'U' .
    WHEN 'C'.
      L_MATNR = WA_COLOR.
      CONCATENATE L_MODEL  '_WOCL'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
*     DELETE it_alc WHERE ctype NE 'C' .
  ENDCASE.

  CONCATENATE 'D_' P_MODEL '_ALC_'     INTO L_CHK .

  LOOP AT IT_ALC    .
    SELECT SINGLE KNNAM
      INTO CORRESPONDING FIELDS OF IT_ALC
      FROM CUKB
     WHERE KNNUM = IT_ALC-KNNUM .

    IF IT_ALC-KNNAM(10) NE  L_CHK     .
      DELETE IT_ALC      .
      CONTINUE .
    ENDIF.
    IT_ALC-CTYPE = IT_ALC-KNNAM+10(1) .
    MODIFY IT_ALC.
  ENDLOOP.

  LOOP AT IT_ALC .
    CLEAR: LT_VALS, LT_VALS[].
    CONCATENATE 'P_' IT_ALC-KNNAM+6(9) INTO LT_VALS-ATNAM.
    APPEND LT_VALS.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_MATNR
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = LT_VALS
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    READ TABLE LT_VALS INDEX 1 .

** changed by Furong on 06/21/07 Help desk: 75BA54496A
** Transport request: UD1K940864
*    IF lt_vals-zflag NE space OR lt_vals-atwrt = space
*       OR lt_vals-atwrt = '????' .
    IF  LT_VALS-ATWRT = '????' .
** end of change

      L_CNT        = L_CNT + 1           .
      IT_ALC-NO    = L_CNT               .
      IT_ALC-COL   = IT_ALC-KNNAM+12(3) .
      SELECT SINGLE KNKTX INTO IT_ALC-COLNM
        FROM CUKBT
       WHERE KNNUM = IT_ALC-KNNUM
         AND SPRAS = SY-LANGU     .
      CONCATENATE TEXT-003 '(' IT_ALC-KNNAM ')' INTO IT_ALC-COLDC .
      MODIFY IT_ALC .
    ELSE.
      DELETE IT_ALC.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_ALCERRORC

*&---------------------------------------------------------------------*
*&      Form  CHECK_ALC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2273   text
*----------------------------------------------------------------------*
FORM CHECK_ALC      USING    PA_TYPE .
  DATA: LT_CUKB              LIKE TABLE OF CUKB        WITH HEADER LINE,
        LT_VALS              LIKE TABLE OF IT_VALS     WITH HEADER LINE,
        LT_ALC               LIKE TABLE OF IT_ALC      WITH HEADER LINE,
        L_MATNR              LIKE MARA-MATNR,
*        L_LEN(2)             TYPE N         ,
*        L_MODELLEN           TYPE I         ,
        L_MODEL(4)           TYPE C         ,
        L_CHK(11)            TYPE C         ,
        L_CNT(3)             TYPE N         ,
        L_KNNUM              LIKE CUOB-KNNUM,
        L_KNOBJ              LIKE CUCO-KNOBJ,
        L_KNNAM              LIKE CUKB-KNNAM.

  " Temporary conversion...
  IT_CUKB[] = IT_ALC[].       CLEAR: IT_ALC[].
* IF p_model = 'EM'.   l_model = 'EMF'.   ENDIF.
  L_MODEL = P_MODEL.
*  L_MODELLEN = STRLEN( P_MODEL ) .

*  L_LEN = 10 - ( 3 - L_MODELLEN ) .

  CASE PA_TYPE.
    WHEN 'U'.
      L_MATNR = WA_ORDER.
      CONCATENATE L_MODEL  '_WOHD'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
*     DELETE it_alc WHERE ctype NE 'U' .
    WHEN 'C'.
      L_MATNR = WA_COLOR.
      CONCATENATE L_MODEL  '_WOCL'           INTO  L_KNNAM.
      PERFORM GET_KNOBJ                      USING L_KNNAM  L_KNOBJ.
      PERFORM GET_KNNUM                      USING L_KNOBJ.
*     DELETE it_alc WHERE ctype NE 'C' .
  ENDCASE.

  CONCATENATE 'D_' P_MODEL '_ALC_' INTO L_CHK.

  LOOP AT IT_ALC    .
    SELECT SINGLE KNNAM
      INTO CORRESPONDING FIELDS OF IT_ALC
      FROM CUKB
     WHERE KNNUM = IT_ALC-KNNUM .

    IF IT_ALC-KNNAM(10) NE  L_CHK       .
      DELETE IT_ALC      .
      CONTINUE .
    ENDIF.
    IT_ALC-CTYPE = IT_ALC-KNNAM+10(1) .
    MODIFY IT_ALC.
  ENDLOOP.

  LOOP AT IT_ALC .
    CLEAR: LT_VALS, LT_VALS[].
    CONCATENATE 'P_' IT_ALC-KNNAM+6(9) INTO LT_VALS-ATNAM.
    APPEND LT_VALS .
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_MATNR
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = LT_VALS
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    READ TABLE LT_VALS INDEX 1 .
*   IF lt_vals-zflag NE space
*   OR lt_vals-atwrt = space OR lt_vals-atwrt = '????' .
    L_CNT        = L_CNT + 1           .
    IT_ALC-NO    = L_CNT               .
    IT_ALC-COL   = IT_ALC-KNNAM+12(3) .
    SELECT SINGLE KNKTX INTO IT_ALC-COLNM
      FROM CUKBT
     WHERE KNNUM = IT_ALC-KNNUM
       AND SPRAS = SY-LANGU     .
    CONCATENATE TEXT-003 '(' IT_ALC-KNNAM ')' INTO IT_ALC-COLDC .
    MODIFY IT_ALC .
*   ELSE.
*     DELETE it_alc.
*   ENDIF.
  ENDLOOP.
  LT_ALC[] = IT_ALC[].   IT_ALC[] = IT_CUKB[].   IT_CUKB[] = LT_ALC[].
ENDFORM.                    " CHECK_ALC

*&---------------------------------------------------------------------*
*&      Form  compare_hpcs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2285   text
*----------------------------------------------------------------------*
FORM COMPARE_HPCS USING   PA_TYPE .
  DATA: LT_CUKB              LIKE TABLE OF CUKB        WITH HEADER LINE,
        LT_VALS              LIKE TABLE OF IT_VALS     WITH HEADER LINE,
        L_MODEL(4)           TYPE C         ,
        L_CHAR               TYPE C         ,
        L_NO(3)              TYPE N         ,
        L_MATNR              LIKE MARA-MATNR,
        L_KNNAM              LIKE CUKB-KNNAM.

* IF p_model = 'EM'.  l_model = 'EMF'.  ENDIF.
  L_MODEL = P_MODEL.
  CONCATENATE 'D_' L_MODEL '_ALC_' PA_TYPE '_%'  INTO L_KNNAM.
  CASE PA_TYPE.
    WHEN 'U'.
      L_MATNR = WA_ORDER.
      L_CHAR  = 'P'     .
    WHEN 'C'.
      L_MATNR = WA_COLOR.
      L_CHAR  = 'Q'     .
  ENDCASE.

  CLEAR: IT_CUKB[], IT_CUKB.
  PERFORM CHECK_ALC       USING PA_TYPE.

* SELECT * INTO TABLE lt_cukb
*   FROM cukb
*  WHERE knnam LIKE l_knnam  .

  CLEAR: IT_HPCS, IT_HPCS[].
  LOOP AT IT_CUKB .
    CLEAR: LT_VALS, LT_VALS[].
    CONCATENATE 'P_' IT_CUKB-KNNAM+6(9) INTO LT_VALS-ATNAM.
    APPEND LT_VALS .  L_NO = IT_CUKB-KNNAM+12(3) .
    CONCATENATE 'P_WO_HPC_' L_CHAR L_NO INTO LT_VALS-ATNAM.
    APPEND LT_VALS .
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        OBJECT             = L_MATNR
        CTYPE              = '001'
*       DISPLAY            = 'D'
      TABLES
        VAL_TABLE          = LT_VALS
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

    READ TABLE LT_VALS INDEX 1 .
    IT_HPCS-NO   = IT_CUKB-KNNAM+12(3) .
    IT_HPCS-COL  = LT_VALS-ATWRT       .
    READ TABLE LT_VALS INDEX 2 .
    IT_HPCS-HPCS = LT_VALS-ATWRT       .
    IF IT_HPCS-COL = IT_HPCS-HPCS AND IT_HPCS-COL NE '????' .
    ELSE.
      APPEND IT_HPCS . CLEAR: IT_HPCS .
    ENDIF.
  ENDLOOP.
  SORT IT_HPCS BY NO.

  " Name & Description Appending....
ENDFORM.                    " compare_hpcs

*&---------------------------------------------------------------------*
*&      Form  get_knobj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNOBJ USING    PA_KNNAM  PA_KNOBJ.
  SELECT SINGLE KNOBJ INTO PA_KNOBJ
    FROM CUCO
   WHERE OBTAB = 'MARA'
     AND OBJEK = PA_KNNAM .
ENDFORM.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNNUM USING    PA_KNOBJ.
  SELECT KNNUM APPENDING CORRESPONDING FIELDS OF TABLE IT_ALC
    FROM CUOB
   WHERE KNOBJ = PA_KNOBJ.
ENDFORM.                    " get_KNNUM

*&---------------------------------------------------------------------*
*&      Form  clear_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_TABLE.
  CLEAR: IT_219, IT_HPCS, IT_219[], IT_HPCS[].
ENDFORM.                    " clear_table

*&---------------------------------------------------------------------*
*&      Form  clear_variable
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VARIABLE.
  CLEAR: WA_MI,   WA_CAR,  WA_OCN,   WA_VERS, WA_CRDAT, WA_MDDAT,
         WA_LCNO, WA_DEST, WA_ALDAT, WA_PERF, WA_INITQ, WA_MODQ,
         WA_PQTY, WA_FQTY, WA_MQTY,  WA_SQTY, WA_PLNT,  WA_PROD.
ENDFORM.                    " clear_variable

*&---------------------------------------------------------------------*
*&      Form  insert_hdvals
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_HDVALS.
  CLEAR: IT_HDVALS, IT_HDVALS, IT_CLVALS[], IT_CLVALS[].
  IT_CLVALS-ATNAM = 'P_MI'                     .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_OCN'                    .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_VERSION'                .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_WO_CREATE_DATE'         .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_WO_MODI_DATE'           .  APPEND IT_CLVALS.
  IT_HDVALS-ATNAM = 'P_LC_NO'                  .  APPEND IT_HDVALS.
  IT_CLVALS-ATNAM = 'P_DESTINATION_CODE'       .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_UPDATE_ALC_DATE1'       .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_PERF_YN'                .  APPEND IT_CLVALS.
* it_hdvals-atnam = 'P_MI'                     .  APPEND IT_HDVALS.
* it_hdvals-atnam = 'P_MI'                     .  APPEND IT_HDVALS.
  IT_CLVALS-ATNAM = 'P_INIT_QTY'               .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_MOD_QTY'                .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_PLAN_QTY'               .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_FORECAST_QTY'           .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_MITU_QTY'               .  APPEND IT_CLVALS.
  IT_CLVALS-ATNAM = 'P_SEQ_QTY'                .  APPEND IT_CLVALS.
  IT_HDVALS-ATNAM = 'P_TRIM_PLANT_NO'          .  APPEND IT_HDVALS.
  IT_HDVALS-ATNAM = 'P_VIN_SPEC'               .  APPEND IT_HDVALS.
  IT_HDVALS-ATNAM = 'P_PROD_FLAG'              .  APPEND IT_HDVALS.
ENDFORM.                    " insert_hdvals

*&---------------------------------------------------------------------*
*&      Form  check_multiple
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_MULTIPLE.
  DATA: L_CNT               TYPE I ,
        L_DATA              LIKE TABLE OF IT_DATA      WITH HEADER LINE.

  L_DATA[] = IT_DATA[].  CLEAR: L_CNT.
  LOOP AT L_DATA WHERE MARK = 'X'.
    L_CNT = L_CNT + 1.
  ENDLOOP.

  CLEAR: IT_HID_MENU, IT_HID_MENU[].
  IF L_CNT < 2 .
    IT_HID_MENU-FCODE = 'P+'.     APPEND IT_HID_MENU.
    IT_HID_MENU-FCODE = 'P-'.     APPEND IT_HID_MENU.
  ENDIF.
ENDFORM.                    " check_multiple

*&---------------------------------------------------------------------*
*&      Form  check_sales_order
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_FLAG  text
*----------------------------------------------------------------------*
FORM CHECK_SALES_ORDER USING    PA_FLAG.
  DATA: L_MATNR         LIKE MARA-MATNR,
        L_COLOR         LIKE MARA-MATNR,
        L_MARA          LIKE MARA      ,
        L_VBAK          LIKE VBAK      ,
        LT_SUM          LIKE TABLE OF ZTPP_WOSUM       WITH HEADER LINE,
        LT_COLOR        LIKE TABLE OF MARA             WITH HEADER LINE,
        LT_COL          LIKE TABLE OF ZSPP_VIN_VALUE   WITH HEADER LINE.

  CONCATENATE IT_DATA-MATNR '%' INTO L_MATNR .

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_SUM
    FROM ZTPP_WOSUM
   WHERE WO_SER = IT_DATA-MATNR(9)
     AND NATION = IT_DATA-MATNR+9(3)
     AND DEALER = IT_DATA-MATNR+12(2).

  CLEAR: LT_COL, LT_COL[].
  LT_COL-ATNAM = 'P_SALES_ORDER'.  APPEND LT_COL.

  LOOP AT LT_SUM .
    CONCATENATE IT_DATA-MATNR LT_SUM-EXTC LT_SUM-INTC INTO L_COLOR .
    SELECT SINGLE * INTO L_MARA
      FROM MARA
     WHERE MATNR = L_COLOR
       AND MTART = 'WOCL'
*       AND mbrsh = 'A'
       AND KZKFG = SPACE .
    IF SY-SUBRC NE 0   .
      PA_FLAG = 'X'.
      MESSAGE I002 WITH L_COLOR  TEXT-009.
      EXIT.
    ENDIF.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_COLOR
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = LT_COL
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    READ TABLE LT_COL INDEX 1.
    IF LT_COL-ZFLAG NE SPACE  OR  LT_COL-ATWRT = SPACE.
      PA_FLAG = 'X'.
      MESSAGE I002 WITH L_COLOR        TEXT-009.
      EXIT.
    ELSE.
      IF LT_COL-ATWRT = LT_SUM-SALES .
        SELECT SINGLE * INTO L_VBAK
          FROM VBAK
         WHERE VBELN = LT_SUM-SALES .
        IF SY-SUBRC NE 0.
          MESSAGE I003 WITH L_COLOR        TEXT-009  LT_SUM-SALES.
          EXIT.
        ENDIF.
      ELSE.
        CONDENSE LT_COL-ATWRT.
        MESSAGE I003 WITH L_COLOR        TEXT-010
                          LT_COL-ATWRT   LT_SUM-SALES.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_sales_order

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_EXCEL.
  DATA: L_COUNT               TYPE I,
        BEGIN OF IT_EXCEL1001          OCCURS 0,
          NO(50)              TYPE C,
          219CODE(50)         TYPE C,
          219DESC(50)         TYPE C,
          219VALS(50)         TYPE C,
        END OF IT_EXCEL1001 .

  LOOP AT IT_219.
    MOVE-CORRESPONDING IT_219 TO IT_EXCEL1001.
    APPEND IT_EXCEL1001.
  ENDLOOP.
  DESCRIBE TABLE IT_219 LINES L_COUNT.
  CLEAR: IT_EXCEL1001.
  INSERT       IT_EXCEL1001 INDEX 1    .
  IT_EXCEL1001-NO      = 'COLUMN'       .
  IT_EXCEL1001-219CODE = 'OPTION'       .
  IT_EXCEL1001-219DESC = 'OPTION  NAME' .
  IT_EXCEL1001-219VALS = 'COLUMN NAME'  .
  INSERT       IT_EXCEL1001 INDEX 2     .
  CLEAR: IT_EXCEL1001.
  INSERT       IT_EXCEL1001 INDEX 3    .
  PERFORM GET_WINDOWS_CLIFILE USING    ',*.xls.'
                              CHANGING WA_FILENAME.

  CALL FUNCTION 'WS_DOWNLOAD'
       EXPORTING
            FILENAME = WA_FILENAME
            FILETYPE = 'DAT'
       TABLES
            DATA_TAB = IT_EXCEL1001.
ENDFORM.                    " DOWNLOAD_EXCEL

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_EXCEL_HD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_EXCEL_HD.
  DATA: BEGIN OF IT_EXCEL OCCURS 0,
        COL01(4), "  Seq no
        COL02(4), "  plant
        COL03(5), " nation
        COL04(9), " wo
        COL05(5), " modqty
* by Daniel on 08/17/10 {
*       COL06(8), " basic model
        COL06(9), " basic model
* }
        COL07(4), " ocn
        COL08(3), " version
        COL09(10), " gen date
        COL10(10), " trans date
        COL11(1), " mat
        COL12(1), " rou
        COL13(1), " bom
        COL14(1), " ver
        COL15(4), " spec
        COL16(3), " hpc
        COL17(4), " prod
        END OF IT_EXCEL.

  CLEAR IT_EXCEL.
  REFRESH IT_EXCEL.
  MOVE 'No' TO IT_EXCEL-COL01.
  MOVE 'Plnt' TO IT_EXCEL-COL02.
  MOVE 'Nation' TO IT_EXCEL-COL03.
  MOVE 'WOrder No' TO IT_EXCEL-COL04.
  MOVE 'MODQTY' TO IT_EXCEL-COL05.
  MOVE 'Basic Mod' TO IT_EXCEL-COL06.
  MOVE 'OCN' TO IT_EXCEL-COL07.
  MOVE 'Version' TO IT_EXCEL-COL08.
  MOVE 'Gen Date' TO IT_EXCEL-COL09.
  MOVE 'Tran Date' TO IT_EXCEL-COL10.
  MOVE 'M' TO IT_EXCEL-COL11.
  MOVE 'R' TO IT_EXCEL-COL12.
  MOVE 'B' TO IT_EXCEL-COL13.
  MOVE 'V' TO IT_EXCEL-COL14.
  MOVE 'SPEC' TO IT_EXCEL-COL15.
  MOVE 'HPC' TO IT_EXCEL-COL16.
  MOVE 'Prod' TO IT_EXCEL-COL17.
  APPEND IT_EXCEL.

  LOOP AT IT_DATA.
    CLEAR IT_EXCEL.
    MOVE IT_DATA-NO TO IT_EXCEL-COL01.
    MOVE IT_DATA-PLNT TO IT_EXCEL-COL02.
    MOVE IT_DATA-NATION TO IT_EXCEL-COL03.
    MOVE IT_DATA-WO TO IT_EXCEL-COL04.
    MOVE IT_DATA-MQTY TO IT_EXCEL-COL05.
    MOVE IT_DATA-MI TO IT_EXCEL-COL06.
    MOVE IT_DATA-OCN TO IT_EXCEL-COL07.
    MOVE IT_DATA-VER TO IT_EXCEL-COL08.
    MOVE IT_DATA-RDATE TO IT_EXCEL-COL09.
    MOVE IT_DATA-PDATE TO IT_EXCEL-COL10.
    MOVE IT_DATA-M_MAT TO IT_EXCEL-COL11.
    MOVE IT_DATA-M_ROU TO IT_EXCEL-COL12.
    MOVE IT_DATA-M_BOM TO IT_EXCEL-COL13.
    MOVE IT_DATA-M_VER TO IT_EXCEL-COL14.
    MOVE IT_DATA-PERF TO IT_EXCEL-COL15.
    MOVE IT_DATA-HSTATS TO IT_EXCEL-COL16.
    MOVE IT_DATA-PROD TO IT_EXCEL-COL17.

    APPEND IT_EXCEL.
  ENDLOOP.
*
  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
     FILENAME                      = 'WORDER.XLS'
     FILETYPE                      = 'DAT'
     ITEM                          = ' '
*     FILETYPE_NO_CHANGE            = ' '
*     FILETYPE_NO_SHOW              = ' '
*     SILENT                        = 'S'
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     ACT_FILENAME                  =
*     ACT_FILETYPE                  =
*     FILESIZE                      =
*     CANCEL                        =
    TABLES
      DATA_TAB                      = IT_EXCEL
*   EXCEPTIONS
*     INVALID_FILESIZE              = 1
*     INVALID_TABLE_WIDTH           = 2
*     INVALID_TYPE                  = 3
*     NO_BATCH                      = 4
*     UNKNOWN_ERROR                 = 5
*     GUI_REFUSE_FILETRANSFER       = 6
*     CUSTOMER_ERROR                = 7
*     OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*
*
*
*
*  DATA: l_count               TYPE i,
*        BEGIN OF it_excel1001          OCCURS 0,
*          no(50)              TYPE c,
*          219code(50)         TYPE c,
*          219desc(50)         TYPE c,
*          219vals(50)         TYPE c,
*        END OF it_excel1001 .
*
*  LOOP AT it_219.
*    MOVE-CORRESPONDING it_219 TO it_excel1001.
*    APPEND it_excel1001.
*  ENDLOOP.
*  DESCRIBE TABLE it_219 LINES l_count.
*  INSERT       it_excel1001 INDEX 1    .
*  INSERT       it_excel1001 INDEX 2    .
*  INSERT       it_excel1001 INDEX 3    .
*  INSERT       it_excel1001 INDEX 4    .
*  INSERT       it_excel1001 INDEX 5    .
*  INSERT       it_excel1001 INDEX 6    .
*  INSERT       it_excel1001 INDEX 7    .
*  CLEAR: it_excel1001.
*  INSERT       it_excel1001 INDEX 8    .
*  INSERT       it_excel1001 INDEX 9    .
*  it_excel1001-no      = 'COLUMN'       .
*  it_excel1001-219code = 'OPTION'       .
*  it_excel1001-219desc = 'OPTION  NAME' .
*  it_excel1001-219vals = 'COLUMN NAME'  .
*  INSERT       it_excel1001 INDEX 20    .
*  PERFORM get_windows_clifile USING    ',*.xls.'
*                              CHANGING wa_filename.
*
*  CALL FUNCTION 'WS_DOWNLOAD'
*       EXPORTING
*            filename = wa_filename
*            filetype = 'DAT'
*       TABLES
*            data_tab = it_excel1001.
ENDFORM.                    " DOWNLOAD_EXCEL_HD

*&---------------------------------------------------------------------*
*&      Form  GET_WINDOWS_CLIFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4403   text
*      <--P_WA_FILENAME  text
*----------------------------------------------------------------------*
FORM GET_WINDOWS_CLIFILE USING MASK CHANGING
                         CLIFILE    LIKE RLGRAP-FILENAME .
  DATA WINSYS(3).
  DATA TMP_CLIFILE    LIKE RLGRAP-FILENAME .

  IF CLIFILE IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD TMP_CLIFILE.
    IF SY-SUBRC NE 0.CLEAR  TMP_CLIFILE.ENDIF.
  ELSE.
    TMP_CLIFILE =  CLIFILE.
  ENDIF.
  CALL FUNCTION 'WS_QUERY'
       EXPORTING
            QUERY  = 'WS'
       IMPORTING
            RETURN = WINSYS.

  IF WINSYS(2) NE 'WN'.
    MESSAGE E016(14).
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
       DEF_FILENAME    = TMP_CLIFILE
       DEF_PATH         = TMP_CLIFILE
       MASK             = MASK
       MODE             = 'S'
       TITLE            = SY-TITLE
    IMPORTING
*ESO 11.04.01 d?ut de correction
       FILENAME         = TMP_CLIFILE
*       CLIFILE         = TMP_CLIFILE
*ESO 11.04.01 fin de correction de correction
*       RC               = RC
      EXCEPTIONS
         INV_WINSYS       = 1
         NO_BATCH         = 2
         SELECTION_CANCEL = 3
         SELECTION_ERROR  = 4
         OTHERS           = 5.

  IF SY-SUBRC EQ 0.
    CLIFILE = TMP_CLIFILE.
  ENDIF.
ENDFORM.                               " GET_WINDOWS_CLIFILE

*&---------------------------------------------------------------------*
*&      Module  modify_screen  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MODIFY_SCREEN OUTPUT.
  DATA: L_INPUT              TYPE I.

  IF WA_CHANGE = 'X'.
    L_INPUT   = 1  .
  ELSE.
    L_INPUT   = 0  .
  ENDIF.

  PERFORM READ_FSC      .
  PERFORM READ_MATERIAL .
  PERFORM READ_VERSION  .
  PERFORM READ_ROUTING  .
  PERFORM READ_BOM      .

  LOOP AT SCREEN.
    IF SCREEN-GROUP1 = 'CHG'.
      SCREEN-INPUT  = L_INPUT.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.                 " modify_screen  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SAVE_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SAVE_CONFIRM.
  DATA: L_MATNR           LIKE MARA-MATNR ,
        LT_VALS           LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  LT_VALS-ATNAM = 'P_PROD_FLAG' .   APPEND LT_VALS.

  LOOP AT IT_DATA.
    LT_VALS-ATWRT = IT_DATA-PROD .  MODIFY LT_VALS INDEX 1.

    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = IT_DATA-MATNR
              MODE         = 'W'
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = LT_VALS
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.
*# 11182010 Append material basic text sjlee  +
    PERFORM UPDATE_MATNR_NOTE USING IT_DATA-MATNR IT_DATA-NOTE.
*# 11182010 -



    IF SY-SUBRC = 0 .   " AND IT_DATA-PROD = 'N'.
      CONCATENATE IT_DATA-MATNR '%' INTO L_MATNR .
      " Color Order Confirm..
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_COLOR
        FROM MARA
       WHERE MATNR LIKE L_MATNR
         AND MTART = 'WOCL'
*         AND mbrsh = 'A'
         AND KZKFG = SPACE .

      LOOP AT IT_COLOR.
        CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
             EXPORTING
                  OBJECT       = IT_COLOR-MATNR
                  MODE         = 'W'
                  CTYPE        = '001'
             TABLES
                  VAL_TABLE    = LT_VALS
             EXCEPTIONS
                  NO_DATA      = 1
                  ERROR_MODE   = 2
                  ERROR_OBJECT = 3
                  ERROR_VALUE  = 4
                  OTHERS       = 5.

        " DELETE Record into the Table ZTPP_SPEC.
        PERFORM DELETE_SPEC  USING P_MODEL       IT_COLOR-MATNR(14)
                            IT_COLOR-MATNR+14(2) IT_COLOR-MATNR+16(2).
      ENDLOOP.
    ENDIF.


  ENDLOOP.

  CLEAR: IT_COLOR, IT_COLOR[].
ENDFORM.                    " SAVE_CONFIRM

*&---------------------------------------------------------------------*
*&      Form  BDC_MM02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDER  text
*----------------------------------------------------------------------*
FORM BDC_MM02 USING    PA_MATNR  PA_FLAG.
  DATA: TMP_TEXT(225)      TYPE C,
        L_MAKTX            LIKE MAKT-MAKTX,
        LT_MSG             LIKE TABLE OF BDCMSGCOLL    WITH HEADER LINE.

  SELECT SINGLE MAKTX INTO L_MAKTX
    FROM MAKT
   WHERE MATNR = PA_MATNR
     AND SPRAS = SY-LANGU.

  PERFORM BDC_DYNPRO_PROCESSING USING :
                         'X'  'SAPLMGMM'             '0060',
                         ' '  'BDC_OKCODE'           '=AUSW' ,
                         ' '  'RMMG1-MATNR'           PA_MATNR,

                         'X'  'SAPLMGMM'             '0070',
                         ' '  'BDC_OKCODE'           '=ENTR' ,
                         ' '  'MSICHTAUSW-KZSEL(02)' 'X'     ,

                         'X'  'SAPLMGMM'             '5004',
                         ' '  'MAKT-MAKTX'            L_MAKTX,
                         ' '  'BDC_OKCODE'           '=PB21' ,

                         'X'  'SAPLCEI0'             '0109',
                         ' '  'BDC_OKCODE'           '=BACK' ,

                         'X'  'SAPLMGMM'             '5004',
                         ' '  'BDC_OKCODE'           '=BU'  .

  CALL TRANSACTION 'MM02'  USING IT_BDCDATA           MODE 'N'
                           UPDATE 'S'  MESSAGES INTO  LT_MSG .

  LOOP AT LT_MSG WHERE MSGTYP = 'E' .
    CLEAR: TMP_TEXT .
    PERFORM CREATE_MESSAGE    USING  TMP_TEXT      LT_MSG-MSGID
                                     LT_MSG-MSGNR  LT_MSG-MSGV1
                                     LT_MSG-MSGV2  LT_MSG-MSGV3
                                     LT_MSG-MSGV4  .

    MESSAGE I001 WITH TMP_TEXT     .
    PA_FLAG = 'X' .
  ENDLOOP.
  CLEAR: LT_MSG, LT_MSG[], IT_BDCDATA, IT_BDCDATA[].
ENDFORM.                                                    " BDC_MM02

*&---------------------------------------------------------------------*
*&      Form  list_box_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0617   text
*----------------------------------------------------------------------*
FORM LIST_BOX_FUNCTION USING   P_LIST_NAME .

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = P_LIST_NAME  " list box
            VALUES          = MODEL_LIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " list_box_function

*&---------------------------------------------------------------------*
*&      Form  READ_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FSC.
  SELECT SINGLE FSC INTO WA_FSC
    FROM ZTPP_WOSUM
   WHERE WO_SER = IT_DATA-WO
     AND NATION = IT_DATA-NATION(3)
     AND DEALER = IT_DATA-NATION+3(2) .
ENDFORM.                    " READ_FSC

*&---------------------------------------------------------------------*
*&      Form  READ_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MATERIAL.
  CHECK IT_DATA-M_MAT = SPACE.
  SELECT SINGLE *
    FROM MARA
   WHERE MATNR = WA_FSC .

  IF SY-SUBRC = 0.  IT_DATA-M_MAT = 'Y'.  ENDIF.
ENDFORM.                    " READ_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  READ_VERSION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_VERSION.
  CHECK IT_DATA-M_VER = SPACE.
  SELECT SINGLE *
    FROM MKAL
   WHERE MATNR = WA_FSC
     AND WERKS = 'P001'
     AND VERID = IT_DATA-VER+1(2).

  IF SY-SUBRC = 0.  IT_DATA-M_VER = 'Y'.  ENDIF.
ENDFORM.                    " READ_VERSION

*&---------------------------------------------------------------------*
*&      Form  READ_ROUTING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ROUTING.
  CHECK IT_DATA-M_ROU = SPACE.
  SELECT SINGLE *
    FROM MKAL
   WHERE MATNR = WA_FSC
     AND WERKS = 'P001'
     AND VERID = IT_DATA-VER+1(2).

  CHECK SY-SUBRC = 0 .
  SELECT SINGLE *
    FROM ZVPP_RP2
   WHERE MATNR = WA_FSC
     AND WERKS = 'P001'
     AND PLNTY = MKAL-PLTYG
     AND PLNNR = MKAL-PLNNG .

  IF SY-SUBRC = 0. IT_DATA-M_ROU = 'Y'.  ENDIF.
ENDFORM.                    " READ_ROUTING

*&---------------------------------------------------------------------*
*&      Form  READ_BOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_BOM.
  CHECK IT_DATA-M_BOM = SPACE.
  SELECT SINGLE *
    FROM MKAL
   WHERE MATNR = WA_FSC
     AND WERKS = 'P001'
     AND VERID = IT_DATA-VER+1(2) .

  CHECK SY-SUBRC = 0 .
  SELECT SINGLE *
    FROM MAST
   WHERE MATNR = WA_FSC
     AND WERKS = 'P001'
     AND STLAN = MKAL-STLAN.

  CHECK SY-SUBRC = 0 .
  SELECT SINGLE *
    FROM ZVPP_BOM
   WHERE MATNR = WA_FSC
     AND WERKS = 'P001'
     AND STLAN = MKAL-STLAN
     AND STLNR = MAST-STLNR.

  IF SY-SUBRC = 0. IT_DATA-M_BOM = 'Y'.  ENDIF.
ENDFORM.                    " READ_BOM

*&---------------------------------------------------------------------*
*&      Form  MAKE_VINSPEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_VINSPEC.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_123'.
  IT_CHAR-ATWRT = IT_CONF-ATWRT .  CLEAR: IT_CONF.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_4'  .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_5'  .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_6'  .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_7'  .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_8'  .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_9'  .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_10' .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  CLEAR: IT_CONF. READ TABLE IT_CONF WITH KEY ATNAM = 'P_VIN_11' .
  CONCATENATE IT_CHAR-ATWRT  IT_CONF-ATWRT INTO     IT_CHAR-ATWRT.
  IT_CHAR-ATNAM = 'P_VIN_SPEC'.                    APPEND IT_CHAR.
ENDFORM.                    " MAKE_VINSPEC

*&---------------------------------------------------------------------*
*&      Form  READ_FIRST_COLOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIRST_COLOR.
  SELECT SINGLE MATNR INTO WA_COLOR
    FROM MARA
   WHERE MATNR > WA_COLOR
     AND MTART = 'WOCL'   .

  WA_COLOR_DIS = WA_COLOR+14(4).
ENDFORM.                    " READ_FIRST_COLOR
*&---------------------------------------------------------------------*
*&      Form  return_01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ET_DATA  text
*      -->P_=  text
*      -->P_RE_DATA  text
*      -->P_EXCEPTIONS  text
*      -->P_COMMUNICATION_FAILURE  text
*      -->P_=  text
*      -->P_1  text
*      -->P_SYSTEM_FAILURE  text
*      -->P_=  text
*      -->P_2  text
*      -->P_RESOURCE_FAILURE  text
*      -->P_=  text
*      -->P_3  text
*----------------------------------------------------------------------*
FORM RETURN_01   USING TASKNAME.

  REFRESH : RE_DATA.
  RECEIVE RESULTS FROM FUNCTION 'Z_FPP_WORKORDER_FREE_PARALLEL'
          TABLES
            ET_DATA                   = RE_DATA
          EXCEPTIONS
          COMMUNICATION_FAILURE       = 1
          SYSTEM_FAILURE              = 2
          RESOURCE_FAILURE            = 3
          OTHERS                      = 4.

  IF SY-SUBRC = 0.
    RCV_JOBS  = RCV_JOBS + 1.
    IT_LOG-MESSAGE = 'Succefully processed'.
  ELSE.
    IT_LOG-MESSAGE = 'Process failed'.
  ENDIF.
  IT_LOG-MATNR = RE_DATA-MATNR.
  APPEND IT_LOG.
  CLEAR: IT_LOG.
ENDFORM.                                                    " return_01
*&---------------------------------------------------------------------*
*&      Form  basic2_transer_class_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BASIC2_TRANSER_CLASS_SELECTION.
  CALL SCREEN 0110.

*  if r_01 = 'X'.
*     PERFORM basic2_transer_class.
*  else.
*     PERFORM basic2_transer_class_alc.
*  endif.
ENDFORM.                    " basic2_transer_class_selection
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0110 OUTPUT.
  SET PF-STATUS 'PF0110'.
  SET TITLEBAR 'ST0110'.
ENDMODULE.                 " STATUS_0110  OUTPUT

*&spwizard: declaration of tablecontrol 'ALC_U' itself
CONTROLS: ALC_U TYPE TABLEVIEW USING SCREEN 0110.

*&spwizard: output module for tc 'ALC_U'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE ALC_U_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_ALC_U LINES ALC_U-LINES.
ENDMODULE.

*&spwizard: declaration of tablecontrol 'ALC_C' itself
CONTROLS: ALC_C TYPE TABLEVIEW USING SCREEN 0110.

*&spwizard: output module for tc 'ALC_C'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE ALC_C_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE IT_ALC_C LINES ALC_C-LINES.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0110 INPUT.
  DATA: L_CHAR(3).
  SV_CODE = OK_CODE.
  CLEAR: OK_CODE.
  CASE SV_CODE.
    WHEN 'EXECUTE'.
      IF R01 = 'X'.
        CLEAR: W_ALC_FLAG.
        PERFORM BASIC2_TRANSER_CLASS.
      ELSE.
        W_ALC_FLAG = 'X'.
        LOOP AT IT_ALC_C.
          L_CHAR = IT_ALC_C-C_NO.
          SHIFT L_CHAR LEFT DELETING LEADING SPACE.
          CONCATENATE 'P_ALC_C_' L_CHAR INTO IT_ALC_NAME-ATNAM.
          COLLECT IT_ALC_NAME.
        ENDLOOP.
        LOOP AT IT_ALC_U.
          L_CHAR = IT_ALC_U-U_NO.
          SHIFT L_CHAR LEFT DELETING LEADING SPACE.
          CONCATENATE 'P_ALC_U_' L_CHAR INTO IT_ALC_NAME-ATNAM.
          COLLECT IT_ALC_NAME.
        ENDLOOP.
        IF IT_ALC_NAME IS INITIAL.
          MESSAGE I001 WITH  'No data'.
        ELSE.
          PERFORM BASIC2_TRANSER_CLASS.
        ENDIF.
      ENDIF.
      PERFORM CLEAR_ALC_SELECTION.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*&      Form  basic2_transer_class_PARTIAL
*&---------------------------------------------------------------------*
*New - created by Fuorng
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BASIC2_TRANSER_CLASS_PARTIAL.
ENDFORM.                    " basic2_transer_class_PARTIAL
*&---------------------------------------------------------------------*
*&      Module  init  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE INIT OUTPUT.
  PERFORM CLEAR_ALC_SELECTION.
ENDMODULE.                 " init  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  clear_alc_selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_ALC_SELECTION.
  CLEAR: IT_ALC_U, IT_ALC_U[], IT_ALC_C, IT_ALC_C[],
         IT_ALC_NAME, IT_ALC_NAME[].
ENDFORM.                    " clear_alc_selection
*&---------------------------------------------------------------------*
*&      Module  alc_c_change  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALC_C_CHANGE INPUT.
  MODIFY IT_ALC_C INDEX ALC_C-CURRENT_LINE.
ENDMODULE.                 " alc_c_change  INPUT
*&---------------------------------------------------------------------*
*&      Module  alc_u_change  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALC_U_CHANGE INPUT.
  MODIFY IT_ALC_U INDEX ALC_U-CURRENT_LINE.
ENDMODULE.                 " alc_u_change  INPUT
*&---------------------------------------------------------------------*
*&      Module  ALC_C_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALC_C_CHANGE_FIELD_ATTR OUTPUT.

ENDMODULE.                 " ALC_C_change_field_attr  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  alc_u_move  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALC_U_MOVE OUTPUT.
* move 1 to it_alc_u-u_no.
ENDMODULE.                 " alc_u_move  OUTPUT

*&spwizard: declaration of tablecontrol 'TC_ALC_U' itself
CONTROLS: TC_ALC_U TYPE TABLEVIEW USING SCREEN 0110.

*&spwizard: output module for tc 'TC_ALC_U'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE TC_ALC_U_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE S_ALC_U LINES TC_ALC_U-LINES.
ENDMODULE.

*&spwizard: input module for tc 'TC_ALC_U'. do not change this line!
*&spwizard: modify table
MODULE TC_ALC_U_MODIFY INPUT.
  MODIFY S_ALC_U
    INDEX TC_ALC_U-CURRENT_LINE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  move_sceen_table  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOVE_SCEEN_TABLE_U INPUT.
  MOVE S_ALC_U TO IT_ALC_U.
  APPEND IT_ALC_U.
ENDMODULE.                 " move_sceen_table  INPUT

*&spwizard: declaration of tablecontrol 'TC_ALC_C' itself
CONTROLS: TC_ALC_C TYPE TABLEVIEW USING SCREEN 0110.

*&spwizard: output module for tc 'TC_ALC_C'. do not change this line!
*&spwizard: update lines for equivalent scrollbar
MODULE TC_ALC_C_CHANGE_TC_ATTR OUTPUT.
  DESCRIBE TABLE S_ALC_C LINES TC_ALC_C-LINES.
ENDMODULE.

*&spwizard: input module for tc 'TC_ALC_C'. do not change this line!
*&spwizard: modify table
MODULE TC_ALC_C_MODIFY INPUT.
  MODIFY S_ALC_C
    INDEX TC_ALC_C-CURRENT_LINE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  move_sceen_table  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOVE_SCEEN_TABLE OUTPUT.

ENDMODULE.                 " move_sceen_table  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  move_sceen_table_c  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MOVE_SCEEN_TABLE_C INPUT.
  MOVE S_ALC_C TO IT_ALC_C.
  APPEND IT_ALC_C.
ENDMODULE.                 " move_sceen_table_c  INPUT
*&---------------------------------------------------------------------*
*&      Form  SORT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_DATA.
*  DATA: lt_rows TYPE lvc_t_row WITH HEADER LINE,
*         lt_row_no TYPE lvc_t_roid. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.
  DATA: CURSORFIELD(20).
  GET CURSOR FIELD CURSORFIELD.
*  CALL METHOD alv_grid->get_selected_rows
*           IMPORTING et_index_rows = lt_rows[]
*                     et_row_no     = lt_row_no.
*
*  CALL METHOD cl_gui_cfw=>flush.
*
  CASE CURSORFIELD.
    WHEN 'IT_DATA-MATNR'.
      SORT IT_DATA BY MATNR.
    WHEN 'IT_DATA-NATION'.
      SORT IT_DATA BY NATION.
    WHEN 'IT_DATA-WO'.
      SORT IT_DATA BY WO.
    WHEN 'IT_DATA-MI'.
      SORT IT_DATA BY MI.
    WHEN 'IT_DATA-OCN'.
      SORT IT_DATA BY OCN.
    WHEN 'IT_DATA-RDATE'.
      SORT IT_DATA BY RDATE.
    WHEN 'IT_DATA-PDATE'.
      SORT IT_DATA BY PDATE.
    WHEN OTHERS.
      MESSAGE E000(ZZ) WITH 'Not Valid Colume for Sorting'.
  ENDCASE.

ENDFORM.                    " SORT_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_MATNR_NOTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATA  text
*      11182010 append save note by sjlee
*----------------------------------------------------------------------*
FORM UPDATE_MATNR_NOTE USING P_MATNR P_NOTE.
  DATA : LT_NOTE LIKE TABLE OF TLINE WITH HEADER LINE,
         LV_NAME LIKE THEAD-TDNAME .


  MOVE P_MATNR TO LV_NAME.
  CLEAR LT_NOTE.
  LT_NOTE-TDFORMAT = '*'.
  LT_NOTE-TDLINE   = P_NOTE.
  APPEND LT_NOTE.

  CALL FUNCTION 'CREATE_TEXT'
       EXPORTING
            FID       = 'GRUN'
            FLANGUAGE = SY-LANGU
            FNAME     = LV_NAME
            FOBJECT   = 'MATERIAL'
       TABLES
            FLINES    = LT_NOTE
       EXCEPTIONS
            NO_INIT   = 1
            NO_SAVE   = 2
            OTHERS    = 3.

ENDFORM.                    " UPDATE_MATNR_NOTE

*---------------------------------------------------------------------*
*       FORM READ_MATNR_BASIC_TEXT                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_MATNR                                                       *
*  -->  P_NOTE
*     11182010 read material basic text                               *
*---------------------------------------------------------------------*
FORM READ_MATNR_BASIC_TEXT USING P_MATNR P_NOTE.
  DATA : LT_NOTE LIKE TABLE OF TLINE WITH HEADER LINE,
         LV_NAME LIKE THEAD-TDNAME .

  MOVE P_MATNR TO LV_NAME.
  CALL FUNCTION 'READ_TEXT'
       EXPORTING
            ID                      = 'GRUN'
            LANGUAGE                = SY-LANGU
            NAME                    = LV_NAME
            OBJECT                  = 'MATERIAL'
       TABLES
            LINES                   = LT_NOTE
       EXCEPTIONS
            ID                      = 1
            LANGUAGE                = 2
            NAME                    = 3
            NOT_FOUND               = 4
            OBJECT                  = 5
            REFERENCE_CHECK         = 6
            WRONG_ACCESS_TO_ARCHIVE = 7
            OTHERS                  = 8.

  IF   SY-SUBRC  <>  0.
    CLEAR :  P_NOTE.
  ELSEIF NOT LT_NOTE[] IS INITIAL .

    READ TABLE LT_NOTE INDEX 1.
    P_NOTE = LT_NOTE-TDLINE.
  ENDIF.

ENDFORM.
