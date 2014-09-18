************************************************************************
* Program Name      : ZAPP888_MODULE_CHANGE_TO_WO
* Author            :
* Creation Date     :
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Module Code Change by Work Order
*                     can choose certain module code to
*                     update
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZAPP888_MODULE_CHANGE_TO_WO.
INCLUDE <ICON>.
TABLES: MARA, T001W, MARC,
        ZSPP_APP888_9000.

*---// Internal tables
DATA: BEGIN OF IT_WOHD OCCURS 0,             "Work Order Information
        WOHD    LIKE   MARA-MATNR,           "Work Order Header
        MAKTX   LIKE   MAKT-MAKTX,           "Description
        ATBEZ   LIKE   CABNT-ATBEZ,          "Characteristic
        MODULE  LIKE   MARA-MATNR,           "Module code
        MDLFG(2),                            "Which module
        FSC(18),
        VERSION(2),
      END   OF IT_WOHD.

DATA: BEGIN OF IT_FSC OCCURS 0,              "FSC Infomation
        FSC     LIKE   MARA-MATNR,           "Full Spec Code
        WERKS   LIKE   T001W-WERKS,          "Plant
        STLAN   LIKE   MAST-STLAN,           "Usage
        STLAL   LIKE   MAST-STLAL,           "Alt BOM
        MAKTX   LIKE   MAKT-MAKTX,           "Description
        MODULE  LIKE   MARA-MATNR,           "Module code
        MDLFG(2),                            "Which module
        USEFG,                               "Use or not flag
      END   OF IT_FSC.

DATA: BEGIN OF IT_FSC_TARGET OCCURS 0,       "FSC List by WO
        FSC     LIKE   MARA-MATNR,           "Full Spec Code
        STLAL   LIKE   MAST-STLAL,           "Alt BOM
      END   OF IT_FSC_TARGET.

DATA: BEGIN OF IT_PLANT OCCURS 0,
        WERKS   LIKE   T001W-WERKS,
      END   OF IT_PLANT.

DATA: BEGIN OF IT_CABNT OCCURS 0,
        ATINN   LIKE   CABN-ATINN,
        ATNAM   LIKE   CABN-ATNAM,
        ATBEZ   LIKE   CABNT-ATBEZ,
      END   OF IT_CABNT.

DATA: IT_ITAB LIKE ZSPP_APP725_9100 OCCURS 0 WITH HEADER LINE,
      IT_9000 LIKE ZSPP_APP888_9000 OCCURS 0 WITH HEADER LINE,
      IT_9100 LIKE ZSPP_APP725_9100 OCCURS 0 WITH HEADER LINE.

*---// Work area
DATA: W_MODULE_CNT TYPE I,               "Count of module characteristic
      W_SUBRC      LIKE SY-SUBRC.        "Return value
DATA: WA_SATNR LIKE MARA-SATNR.

*---// Dropdown
TYPE-POOLS: VRM.
DATA: XNAME    TYPE VRM_ID,
      XLIST    TYPE VRM_VALUES,
      XVALUE   LIKE LINE OF XLIST.

*---// Ranges
RANGES: R_WOHD_TARGET FOR MAKT-MAKTX,
        R_MODULE FOR CABNT-ATBEZ.

*---// Constants
CONSTANTS: C_CHECK                  VALUE 'X',
           C_MODULE(8)              VALUE 'MODULE_%',
           C_CAPID LIKE RC29L-CAPID VALUE 'PP01',         "Application
           C_STLAN LIKE MAST-STLAN  VALUE '1',
           C_ICON_EQUAL(4)          VALUE ICON_GREEN_LIGHT,
           C_ICON_DIFF(4)           VALUE ICON_YELLOW_LIGHT,
           C_ICON_ERR(4)            VALUE ICON_RED_LIGHT,
           C_ICON_NONE(4)           VALUE ICON_LIGHT_OUT,
           C_EMPTY_OD LIKE CUKB-KNNAM VALUE 'Z_EMPTY_OD'.

*-----/// ALV Control : START
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

* Declare reference variables, the container and internal table
DATA: WC_CONTROL_9000   TYPE        SCRFNAME VALUE 'CC_9000_ALV',
      WC_ALV_9000       TYPE REF TO CL_GUI_ALV_GRID,
      WC_CONTAINER_9000 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: WC_CONTROL_9100   TYPE        SCRFNAME VALUE 'CC_9100_ALV',
      WC_ALV_9100       TYPE REF TO CL_GUI_ALV_GRID,
      WC_CONTAINER_9100 TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

* Predefine a local class for event handling to allow the
* declaration of a reference variable before the class is defined.
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED. "/ALV Event Handling

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Interal tables for ALV GRID
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

* Global variable for ALV GRID
DATA : W_IS_LAYOUT TYPE LVC_S_LAYO,
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

*-----/// ALV Control : END

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:

    HANDLE_DOUBLE_CLICK
        FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
            IMPORTING E_ROW
                      E_COLUMN
                      ES_ROW_NO,

    HANDLE_USER_COMMAND
        FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
            IMPORTING E_UCOMM,

    HANDLE_DATA_CHANGED
        FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
            IMPORTING ER_DATA_CHANGED
                      E_ONF4
                      E_ONF4_BEFORE
                      E_ONF4_AFTER.
ENDCLASS.

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_DOUBLE_CLICK.
    PERFORM DBL_CLICK_9000 USING E_COLUMN-FIELDNAME
                                 ES_ROW_NO-ROW_ID.

  ENDMETHOD.                           "handle_double_click

  METHOD  HANDLE_DATA_CHANGED.
  ENDMETHOD.

  METHOD  HANDLE_USER_COMMAND.
  ENDMETHOD.
ENDCLASS.


*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_WERKS FOR T001W-WERKS NO-EXTENSION NO INTERVALS
                                        MEMORY ID WRK,

                S_WOHD  FOR MARA-MATNR MODIF ID GR1.

PARAMETERS: P_DATUV LIKE RC29N-DATUV OBLIGATORY DEFAULT SY-DATUM.
SELECTION-SCREEN SKIP.
PARAMETERS: P_ALL RADIOBUTTON GROUP GRP USER-COMMAND UCOM,
            P_PAT RADIOBUTTON GROUP GRP DEFAULT 'X'.
PARAMETERS: P_MODEL(3) AS LISTBOX VISIBLE LENGTH 25.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  1(3) TEXT-TFM .
PARAMETERS: P_FM AS CHECKBOX MODIF ID GRW.
SELECTION-SCREEN COMMENT  11(3) TEXT-TCP.
PARAMETERS: P_CP AS CHECKBOX  MODIF ID GRW.
SELECTION-SCREEN COMMENT  21(3) TEXT-TFC.
PARAMETERS: P_FC AS CHECKBOX  MODIF ID GRW.
SELECTION-SCREEN COMMENT  31(3) TEXT-TRC.
PARAMETERS: P_RC AS CHECKBOX  MODIF ID GRW.
SELECTION-SCREEN COMMENT  41(3) TEXT-TDR.
PARAMETERS: P_DR AS CHECKBOX MODIF ID GRW.
SELECTION-SCREEN COMMENT  51(3) TEXT-TTW.
PARAMETERS: P_TW AS CHECKBOX  MODIF ID GRW.

SELECTION-SCREEN END   OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END   OF BLOCK BL1.

*AT SELECTION-SCREEN on radiobutton group grp.

AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.
  PERFORM MAKE_DROPDOWN_LIST_BOX.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP GRP.
  PERFORM SCREEN_MODIFY.

START-OF-SELECTION.
  PERFORM CHECK_PLANT.
  PERFORM GET_DATA.
  PERFORM PROCESSING_BLANK_WOHD.
  PERFORM DISPLAY_DATA.

*&---------------------------------------------------------------------*
*&      Form  dbl_click_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_COLUMN_FIELDNAME  text
*      -->P_ES_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM DBL_CLICK_9000 USING P_COLUMN_NAME             "Column Name
                          PS_ROW_NO  LIKE SY-TABIX. "Numeric Row ID
  DATA : LW_SEL_INDEX LIKE SY-TABIX.

  MOVE: PS_ROW_NO TO LW_SEL_INDEX.

  CHECK SY-DYNNR EQ '9000'.

  READ TABLE IT_9000 INDEX LW_SEL_INDEX.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  CLEAR: IT_9100, IT_9100[].

  MOVE: IT_9000 TO ZSPP_APP888_9000.

  LOOP AT IT_ITAB WHERE WOHD  EQ IT_9000-WOHD
                    AND FSC   EQ IT_9000-FSC
                    AND WERKS EQ IT_9000-WERKS
                    AND STLAN EQ IT_9000-STLAN
                    AND STLAL EQ IT_9000-STLAL.
    MOVE: IT_ITAB TO IT_9100.
    APPEND IT_9100.
  ENDLOOP.

  SORT IT_ITAB BY WOHD FSC WERKS STLAN STLAL MDLFG.

  PERFORM CREATE_ALV_OBJECT_9100.

  CALL SCREEN 9100.
ENDFORM.                    " dbl_click_9000
*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  CASE C_CHECK.
    WHEN P_ALL.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'GRW'.
          SCREEN-INPUT = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN P_PAT.
      LOOP AT SCREEN.
        IF SCREEN-GROUP1 = 'GRW'.
          SCREEN-INPUT = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM check_rtn.
*  PERFORM check_plant.
*ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA.
  DATA: LW_CONTINUE VALUE 'X'.

  PERFORM GET_WOHD_INFO_INPUT_WO USING LW_CONTINUE.
  PERFORM GET_FSC_INFO_INPUT_WO.

  PERFORM SET_IT_ITAB.
  PERFORM SET_IT_9000.

  IF LW_CONTINUE EQ SPACE.
    CLEAR: IT_9000, IT_9000[].
  ENDIF.
ENDFORM.                    " get_data
*&---------------------------------------------------------------------*
*&      Form  get_wohd_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WOHD_INFO_INPUT_WO USING PW_CONTINUE.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR   LIKE   MARA-MATNR,
          MAKTX   LIKE   MAKT-MAKTX,
          FSC(18),
          VERSION(2),
        END   OF LT_MARA.

  DATA: BEGIN OF LT_AUSP OCCURS 0,
          ATINN   LIKE   AUSP-ATINN,
          ATWRT   LIKE   AUSP-ATWRT,
        END   OF LT_AUSP.

  DATA: BEGIN OF LT_CABN OCCURS 0,
        ATINN   LIKE   CABN-ATINN,
        ATNAM   LIKE   CABN-ATNAM,
      END   OF LT_CABN.

  DATA: W_ATINN1 LIKE CABN-ATINN,
        W_ATINN2 LIKE CABN-ATINN,
        W_ATINN3 LIKE CABN-ATINN,
        W_ATINN4 LIKE CABN-ATINN,
        W_ATINN5 LIKE CABN-ATINN,
        W_ATINN6 LIKE CABN-ATINN,
        W_ATINN7 LIKE CABN-ATINN,
        W_NATION LIKE AUSP-ATWRT,
        W_DEALER LIKE AUSP-ATWRT,
        W_MODEL_YEAR LIKE AUSP-ATWRT,
        W_MI LIKE AUSP-ATWRT,
        W_OCN LIKE AUSP-ATWRT,
        W_VERSION LIKE AUSP-ATWRT.

  DATA: W_FSC LIKE IT_FSC_TARGET-FSC.
  DATA: L_LEN TYPE I,
        L_NEW_DEALER(1),
        L_OLD_DEALER(2).

** Changed by Furong on 04/19/11
  IF NOT P_MODEL IS INITIAL.
    CONCATENATE P_MODEL '_WOHD' INTO WA_SATNR.
    SELECT A~MATNR B~MAKTX
   INTO CORRESPONDING FIELDS OF TABLE LT_MARA
   FROM MARA AS A INNER JOIN MAKT AS B
                     ON A~MATNR EQ B~MATNR
                    AND B~SPRAS EQ SY-LANGU
  WHERE A~MATNR IN S_WOHD
    AND A~MTART EQ 'WOHD'
   AND A~SATNR = WA_SATNR.
  ELSE.
** End
*---// Read Work order header

    SELECT A~MATNR B~MAKTX
      INTO CORRESPONDING FIELDS OF TABLE LT_MARA
      FROM MARA AS A INNER JOIN MAKT AS B
                        ON A~MATNR EQ B~MATNR
                       AND B~SPRAS EQ SY-LANGU
     WHERE A~MATNR IN S_WOHD
       AND A~MTART EQ 'WOHD'.
** Changed by Furong on 04/19/11
  ENDIF.
** End
  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M02.
    CLEAR: PW_CONTINUE. EXIT.
  ENDIF.

*---// Delete special material & tesing car for body * welding
** and get FSC & Version
  SELECT ATINN ATNAM INTO TABLE LT_CABN
   FROM CABN
   WHERE ATNAM = 'P_NATION'
      OR ATNAM = 'P_DEALER'
      OR ATNAM = 'P_MODEL_YEAR'
      OR ATNAM = 'P_MI'
      OR ATNAM = 'P_OCN'
      OR ATNAM = 'P_VERSION'.
  LOOP AT LT_CABN.
    CASE LT_CABN-ATNAM.
      WHEN 'P_NATION'.
        W_ATINN1 = LT_CABN-ATINN.
      WHEN 'P_DEALER'.
        W_ATINN2 = LT_CABN-ATINN.
      WHEN 'P_MODEL_YEAR'.
        W_ATINN3 = LT_CABN-ATINN.
      WHEN 'P_MI'.
        W_ATINN4 = LT_CABN-ATINN.
      WHEN 'P_OCN'.
        W_ATINN5 = LT_CABN-ATINN.
      WHEN 'P_VERSION'.
        W_ATINN6 = LT_CABN-ATINN.
    ENDCASE.
  ENDLOOP.

  LOOP AT LT_MARA.
    IF LT_MARA-MATNR CP '*WOHD*' OR
       LT_MARA-MATNR CP '*WOCL*'.
      DELETE LT_MARA.
      CONTINUE.
    ELSE.
      IF LT_MARA-MATNR+12(2) = 'XX' OR
         LT_MARA-MATNR+12(2) = 'XY'.
        DELETE LT_MARA.
        CONTINUE.
      ENDIF.
    ENDIF.
    SELECT ATINN ATWRT
   INTO CORRESPONDING FIELDS OF TABLE LT_AUSP
   FROM AUSP
    FOR ALL ENTRIES IN LT_CABN
  WHERE OBJEK EQ LT_MARA-MATNR
    AND ATINN EQ LT_CABN-ATINN
    AND KLART EQ '001'.

    LOOP AT LT_AUSP.
      CASE LT_AUSP-ATINN.
        WHEN  W_ATINN1.
          W_NATION = LT_AUSP-ATWRT.
        WHEN  W_ATINN2.
          W_DEALER = LT_AUSP-ATWRT.
        WHEN  W_ATINN3.
          W_MODEL_YEAR = LT_AUSP-ATWRT.
        WHEN  W_ATINN4.
          W_MI = LT_AUSP-ATWRT.
        WHEN  W_ATINN5.
          W_OCN = LT_AUSP-ATWRT.
        WHEN  W_ATINN6.
          W_VERSION = LT_AUSP-ATWRT.
      ENDCASE.
    ENDLOOP.
** Changed by Furong on 10/09/07 for EBOM
*   CONCATENATE w_model_year w_nation w_dealer w_mi INTO w_fsc.
*   CONCATENATE w_fsc w_ocn INTO w_fsc SEPARATED BY space.
    L_LEN = STRLEN( W_MI ).
    IF L_LEN = 7.
      CONCATENATE W_MODEL_YEAR W_NATION W_DEALER W_MI INTO W_FSC.
      CONCATENATE W_FSC W_OCN INTO W_FSC SEPARATED BY SPACE.
    ELSE.
      L_OLD_DEALER =  W_DEALER.
      CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
           EXPORTING
                OLD_DEALER = L_OLD_DEALER
           IMPORTING
                NEW_DEALER = L_NEW_DEALER.
      CONCATENATE W_MODEL_YEAR W_NATION L_NEW_DEALER W_MI INTO W_FSC.
      CONCATENATE W_FSC W_OCN INTO W_FSC.
    ENDIF.
** End of change
    LT_MARA-FSC = W_FSC.
    LT_MARA-VERSION = W_VERSION+1(2).
    MODIFY LT_MARA.
    CLEAR: LT_MARA.
  ENDLOOP.

  READ TABLE LT_MARA INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M02.
    CLEAR: PW_CONTINUE. EXIT.
  ENDIF.

*---// Read Module Characteristic code
  CLEAR: IT_CABNT, IT_CABNT[].

  IF P_ALL = 'X'.
  ELSE.
    IF P_FM = 'X'.
      R_MODULE-SIGN = 'I'.
      R_MODULE-OPTION = 'EQ'.
      R_MODULE-LOW = 'MODULE_FM'.
      APPEND R_MODULE.
      CLEAR R_MODULE.
    ENDIF.
    IF P_CP = 'X'.
      R_MODULE-SIGN = 'I'.
      R_MODULE-OPTION = 'EQ'.
      R_MODULE-LOW = 'MODULE_CP'.
      APPEND R_MODULE.
      CLEAR R_MODULE.
    ENDIF.
    IF P_FC = 'X'.
      R_MODULE-SIGN = 'I'.
      R_MODULE-OPTION = 'EQ'.
      R_MODULE-LOW = 'MODULE_FC'.
      APPEND R_MODULE.
      CLEAR R_MODULE.
    ENDIF.
    IF P_RC = 'X'.
      R_MODULE-SIGN = 'I'.
      R_MODULE-OPTION = 'EQ'.
      R_MODULE-LOW = 'MODULE_RC'.
      APPEND R_MODULE.
      CLEAR R_MODULE.
    ENDIF.
    IF P_DR = 'X'.
      R_MODULE-SIGN = 'I'.
      R_MODULE-OPTION = 'EQ'.
      R_MODULE-LOW = 'MODULE_DR'.
      APPEND R_MODULE.
      CLEAR R_MODULE.
    ENDIF.
    IF P_TW = 'X'.
      R_MODULE-SIGN = 'I'.
      R_MODULE-OPTION = 'EQ'.
      R_MODULE-LOW = 'MODULE_TW'.
      APPEND R_MODULE.
      CLEAR R_MODULE.
    ENDIF.

  ENDIF.
*  SELECT a~atinn atnam atbez
*    INTO CORRESPONDING FIELDS OF TABLE it_cabnt
*    FROM cabnt AS a INNER JOIN cabn AS b
*                       ON a~atinn EQ b~atinn
*                      AND a~adzhl EQ b~adzhl
*   WHERE a~spras = sy-langu
*     AND a~atbez LIKE c_module
*     AND a~lkenz =    ' '
*     AND b~lkenz =    ' '.
*
  SELECT A~ATINN ATNAM ATBEZ
    INTO CORRESPONDING FIELDS OF TABLE IT_CABNT
    FROM CABNT AS A INNER JOIN CABN AS B
                       ON A~ATINN EQ B~ATINN
                      AND A~ADZHL EQ B~ADZHL
   WHERE A~SPRAS = SY-LANGU
     AND A~ATBEZ IN R_MODULE
     AND A~LKENZ =    ' '
     AND B~LKENZ =    ' '.

  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M03.
    CLEAR: PW_CONTINUE. EXIT.
  ENDIF.

  DESCRIBE TABLE IT_CABNT LINES W_MODULE_CNT.

*---// Set Work Order Information
  CLEAR: IT_WOHD, IT_WOHD[].

  LOOP AT LT_MARA.
    CLEAR: LT_AUSP, LT_AUSP[].

    SELECT ATINN ATWRT
      INTO CORRESPONDING FIELDS OF TABLE LT_AUSP
      FROM AUSP
       FOR ALL ENTRIES IN IT_CABNT
     WHERE OBJEK EQ LT_MARA-MATNR
       AND ATINN EQ IT_CABNT-ATINN
       AND KLART EQ '001'.

    LOOP AT IT_CABNT.
      CLEAR: LT_AUSP.

      READ TABLE LT_AUSP WITH KEY ATINN = IT_CABNT-ATINN.

      MOVE: LT_MARA-MATNR       TO IT_WOHD-WOHD,
            LT_MARA-MAKTX       TO IT_WOHD-MAKTX,
            LT_MARA-FSC         TO IT_WOHD-FSC,
            LT_MARA-VERSION     TO IT_WOHD-VERSION,

            IT_CABNT-ATBEZ      TO IT_WOHD-ATBEZ,
            IT_CABNT-ATBEZ+7(2) TO IT_WOHD-MDLFG.

      IF LT_AUSP-ATWRT EQ '-'.
        CLEAR: LT_AUSP-ATWRT.
      ENDIF.

      MOVE: LT_AUSP-ATWRT       TO IT_WOHD-MODULE.

      APPEND IT_WOHD.
    ENDLOOP.
  ENDLOOP.

*---// Set Target FSC List
  CLEAR: IT_FSC_TARGET, IT_FSC_TARGET[].

  LOOP AT LT_MARA.

    MOVE: LT_MARA-FSC   TO IT_FSC_TARGET-FSC,
          LT_MARA-VERSION TO IT_FSC_TARGET-STLAL.

    COLLECT IT_FSC_TARGET.
  ENDLOOP.
ENDFORM.                    " get_wohd_info
*&---------------------------------------------------------------------*
*&      Form  get_fsc_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FSC_INFO_INPUT_WO.
  DATA: LW_STRLEN   TYPE   I,
        LW_TOPMAT   LIKE   CSTMAT.

  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE.

  CLEAR: IT_FSC, IT_FSC[].

  LOOP AT IT_FSC_TARGET.
    LOOP AT IT_PLANT.
      CLEAR: LW_TOPMAT, LT_STB, LT_STB[].

      CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
           EXPORTING
                AUMNG                 = 0
                CAPID                 = C_CAPID
                CUOVS                 = '0'
                DATUV                 = P_DATUV
                MKTLS                 = 'X'
                CUOBJ                 = '999999999999'
                MMORY                 = '0'
                MTNRV                 = IT_FSC_TARGET-FSC
                STPST                 = 0
                STLAN                 = C_STLAN
                STLAL                 = IT_FSC_TARGET-STLAL
                SVWVO                 = 'X'
                WERKS                 = IT_PLANT-WERKS
                VRSVO                 = 'X'
           IMPORTING
                TOPMAT                = LW_TOPMAT
           TABLES
                STB                   = LT_STB
           EXCEPTIONS
                ALT_NOT_FOUND         = 1
                CALL_INVALID          = 2
                MATERIAL_NOT_FOUND    = 3
                MISSING_AUTHORIZATION = 4
                NO_BOM_FOUND          = 5
                NO_PLANT_DATA         = 6
                NO_SUITABLE_BOM_FOUND = 7
                CONVERSION_ERROR      = 8
                OTHERS                = 9.
      IF SY-SUBRC <> 0 OR LW_TOPMAT-STLAL NE IT_FSC_TARGET-STLAL.
        CONTINUE.
      ENDIF.

      LOOP AT LT_STB.
        SELECT SINGLE * FROM MARC WHERE MATNR = LT_STB-IDNRK
                                    AND WERKS = LT_STB-WERKS.
        IF SY-SUBRC NE 0.
          MESSAGE E000(ZZ) WITH TEXT-M01.
        ENDIF.

        CHECK MARC-DISPO = 'M01'.            "Check module or not

        MOVE: IT_FSC_TARGET-FSC   TO IT_FSC-FSC,
              LT_STB-WERKS        TO IT_FSC-WERKS,
              LT_STB-STLAN        TO IT_FSC-STLAN,
              LT_STB-STLAL        TO IT_FSC-STLAL,
              LT_STB-OJTXB        TO IT_FSC-MAKTX,
              LT_STB-IDNRK+3(2)   TO IT_FSC-MDLFG.

        PERFORM CHECK_OD_VALUE USING LT_STB-KNOBJ W_SUBRC.

        CHECK W_SUBRC EQ 0.
        LW_STRLEN = STRLEN( LT_STB-IDNRK ).
** Changed by Furong on 12/18/07
*        IF LW_STRLEN > 10.
*          LW_STRLEN = LW_STRLEN - 2.
*          MOVE: LT_STB-IDNRK(LW_STRLEN) TO IT_FSC-MODULE.
*        ELSE.
*          MOVE: LT_STB-IDNRK TO IT_FSC-MODULE.
*        ENDIF.
        IF IT_FSC_TARGET-FSC+13(1) = ' '.

          IF LW_STRLEN > 10.
            LW_STRLEN = LW_STRLEN - 2.
            MOVE: LT_STB-IDNRK(LW_STRLEN) TO IT_FSC-MODULE.
          ELSE.
            MOVE: LT_STB-IDNRK TO IT_FSC-MODULE.
          ENDIF.
        ELSE.
** Changed by Furong on 05/27/09
*          IF LW_STRLEN > 12.
*            LW_STRLEN = LW_STRLEN - 2.
*            MOVE: LT_STB-IDNRK(LW_STRLEN) TO IT_FSC-MODULE.
*          ELSE.
*            MOVE: LT_STB-IDNRK TO IT_FSC-MODULE.
*          ENDIF.

          IT_FSC-MODULE = LT_STB-IDNRK+0(12).
** End of
        ENDIF.
** End of change
        COLLECT IT_FSC.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " get_fsc_info
*&---------------------------------------------------------------------*
*&      Form  get_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PLANT.
  SELECT WERKS INTO TABLE IT_PLANT
    FROM T001W
   WHERE WERKS IN S_WERKS.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M14.
  ENDIF.
ENDFORM.                    " get_plant
*&---------------------------------------------------------------------*
*&      Form  set_it_itab_input_wo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_IT_ITAB.
  CLEAR: IT_ITAB, IT_ITAB[].

  LOOP AT IT_WOHD.
    CLEAR: IT_FSC.

    LOOP AT IT_FSC WHERE FSC   = IT_WOHD-FSC
                     AND STLAL = IT_WOHD-VERSION
                     AND MDLFG = IT_WOHD-MDLFG.

      PERFORM APPEND_IT_ITAB USING C_ICON_NONE '' IT_WOHD-MDLFG
                                   IT_WOHD-MODULE IT_FSC-MODULE.

      MOVE: C_CHECK TO IT_FSC-USEFG.
      MODIFY IT_FSC.
    ENDLOOP.
  ENDLOOP.

  DATA: LW_WOTXT LIKE MAKT-MAKTX,
        LW_STLAL(03).

*---// Set ATNAM
  LOOP AT IT_CABNT.
    MOVE: IT_CABNT-ATNAM TO IT_ITAB-ATNAM.

    MODIFY IT_ITAB TRANSPORTING ATNAM
     WHERE MDLFG EQ IT_CABNT-ATBEZ+7(2).
  ENDLOOP.
ENDFORM.                    " set_it_itab_input_wo
*&---------------------------------------------------------------------*
*&      Form  append_it_itab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_ICON_DIFF  text
*      -->P_TEXT_M04  text
*----------------------------------------------------------------------*
FORM APPEND_IT_ITAB USING PW_ICON PW_MSG PW_MDLFG PW_MDLWO PW_MDLFSC.
  CLEAR: IT_ITAB.

  MOVE: PW_ICON        TO IT_ITAB-ICON,
        PW_MSG         TO IT_ITAB-MSG,
        IT_WOHD-WOHD   TO IT_ITAB-WOHD,
        IT_FSC-FSC     TO IT_ITAB-FSC,
        IT_FSC-MAKTX   TO IT_ITAB-MAKTX,
        IT_FSC-WERKS   TO IT_ITAB-WERKS,
        IT_FSC-STLAN   TO IT_ITAB-STLAN,
        IT_FSC-STLAL   TO IT_ITAB-STLAL,
        PW_MDLFG       TO IT_ITAB-MDLFG,
        PW_MDLWO       TO IT_ITAB-MDLWO,
        PW_MDLFSC      TO IT_ITAB-MDLFSC.

  APPEND IT_ITAB.
ENDFORM.                    " append_it_itab
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 9000.
ENDFORM.                    " display_data
*&---------------------------------------------------------------------*
*&      Module  status  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  CASE SY-DYNNR.
    WHEN '9000'.
      SET PF-STATUS '9000'.
      SET TITLEBAR  '9000'.
    WHEN '9100'.
      SET PF-STATUS '9100'.
      SET TITLEBAR  '9000'.
  ENDCASE.
ENDMODULE.                 " status  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.
  IF WC_CONTAINER_9000 IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT_9000.
    PERFORM SET_ATTRIBUTES_ALV_GRID_9000.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_9000'.
    PERFORM ASSIGN_ITAB_TO_ALV_9000.
    PERFORM SSSIGN_EVENT_9000.
  ENDIF.
ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT_9000.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  CREATE OBJECT WC_CONTAINER_9000
         EXPORTING CONTAINER_NAME = WC_CONTROL_9000
         EXCEPTIONS
          CNTL_ERROR = 1
          CNTL_SYSTEM_ERROR = 2
          CREATE_ERROR = 3
          LIFETIME_ERROR = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT WC_ALV_9000
         EXPORTING I_PARENT      = WC_CONTAINER_9000
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object_9000
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_9000.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : W_IS_LAYOUT, W_VARIANT.

  W_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  W_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  W_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  W_IS_LAYOUT-CWIDTH_OPT = C_CHECK.  "/optimizes the column width
  W_IS_LAYOUT-NO_MERGING = C_CHECK.  "/Disable cell merging
  W_VARIANT-REPORT       = SY-REPID.
  W_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid_9000
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1088   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  W_REPID = SY-REPID.
  LW_ITAB = P_ITAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :

                                  'S' 'WOHD'       ' ',
                                  'E' 'KEY'        'X',

                                  'S' 'FSC'        ' ',
                                  'E' 'KEY'        'X'.

ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_1225   text
*      -->P_1226   text
*      -->P_1227   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO W_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check filed catalog'.
    ENDIF.

    MOVE: W_FIELDNAME-FIELDNAME TO P_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'P_FIELDCAT-' P_FIELD  INTO L_COL.
  ASSIGN (L_COL) TO <FS>.
  MOVE   P_VALUE TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    IF P_FIELDCAT-COL_POS IS INITIAL.
      ADD 1 TO W_CNT.
      P_FIELDCAT-COL_POS = W_CNT.
    ENDIF.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_9000.
  CALL METHOD WC_ALV_9000->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING I_STRUCTURE_NAME = 'ZSPP_APP888_9000'
               IS_LAYOUT        = W_IS_LAYOUT
               I_SAVE           = W_SAVE
               IS_VARIANT       = W_VARIANT
               I_DEFAULT        = SPACE
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_9000[].
ENDFORM.                    " assign_itab_to_alv_9000
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SSSIGN_EVENT_9000.
*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT EVENT_RECEIVER.

  SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR WC_ALV_9000.
  SET HANDLER EVENT_RECEIVER->HANDLE_DATA_CHANGED  FOR WC_ALV_9000.
  SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR WC_ALV_9000.
ENDFORM.                    " sssign_event_9000
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: SY-UCOMM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'UPDATE'.
      CLEAR SY-UCOMM.
      PERFORM UPDATE_RTN.
  ENDCASE.
ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  set_it_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_IT_9000.
  DATA: LW_APPENDED,
        LW_INDEX(1),
        LW_CHAR(30).
  FIELD-SYMBOLS: <CHAR>.

  SORT IT_ITAB BY WOHD FSC WERKS STLAN STLAL MDLFG.
  LW_INDEX = '1'.
  LOOP AT IT_ITAB.
    CASE IT_ITAB-MDLFG.
      WHEN 'CP'.
        MOVE 'IT_9000-CHAR_CP' TO LW_CHAR.
        ASSIGN (LW_CHAR) TO <CHAR>.
        <CHAR> = IT_ITAB-MDLFSC.
      WHEN 'FM'.
        MOVE 'IT_9000-CHAR_FM' TO LW_CHAR.
        ASSIGN (LW_CHAR) TO <CHAR>.
        <CHAR> = IT_ITAB-MDLFSC.
      WHEN 'FC'.
        MOVE 'IT_9000-CHAR_FC' TO LW_CHAR.
        ASSIGN (LW_CHAR) TO <CHAR>.
        <CHAR> = IT_ITAB-MDLFSC.
      WHEN 'RC'.
        MOVE 'IT_9000-CHAR_RC' TO LW_CHAR.
        ASSIGN (LW_CHAR) TO <CHAR>.
        <CHAR> = IT_ITAB-MDLFSC.
      WHEN 'DR'.
        MOVE 'IT_9000-CHAR_DR' TO LW_CHAR.
        ASSIGN (LW_CHAR) TO <CHAR>.
        <CHAR> = IT_ITAB-MDLFSC.
      WHEN 'TW'.
        MOVE 'IT_9000-CHAR_TW' TO LW_CHAR.
        ASSIGN (LW_CHAR) TO <CHAR>.
        <CHAR> = IT_ITAB-MDLFSC.
    ENDCASE.
    AT END OF STLAL.
      CLEAR: LW_APPENDED.

      MOVE: IT_ITAB-WOHD  TO IT_9000-WOHD,
            IT_ITAB-FSC   TO IT_9000-FSC,
            IT_ITAB-MAKTX TO IT_9000-MAKTX,
            IT_ITAB-WERKS TO IT_9000-WERKS,
            IT_ITAB-STLAN TO IT_9000-STLAN,
            IT_ITAB-STLAL TO IT_9000-STLAL.

      MOVE: C_ICON_NONE  TO IT_9000-ICON,
            ' '          TO IT_9000-MSG.
      APPEND IT_9000.
      CLEAR: IT_9000.
      LW_INDEX = '1'.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " set_it_9000
*&---------------------------------------------------------------------*
*&      Form  check_error
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM CHECK_ERROR USING PW_APPENDED.
  READ TABLE IT_ITAB WITH KEY WOHD  = IT_ITAB-WOHD
                              FSC   = IT_ITAB-FSC
                              WERKS = IT_ITAB-WERKS
                              STLAN = IT_ITAB-STLAN
                              STLAL = IT_ITAB-STLAL
                              ICON  = C_ICON_ERR
                     TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    MOVE: C_ICON_ERR TO IT_9000-ICON,
          TEXT-M09   TO IT_9000-MSG.
    APPEND IT_9000.

    MOVE: C_CHECK TO PW_APPENDED.
  ENDIF.
ENDFORM.                    " check_error
*&---------------------------------------------------------------------*
*&      Form  check_diffrent
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM CHECK_DIFFRENT_01 USING PW_APPENDED.
  CHECK PW_APPENDED IS INITIAL.

  READ TABLE IT_ITAB WITH KEY WOHD  = IT_ITAB-WOHD
                              FSC   = IT_ITAB-FSC
                              WERKS = IT_ITAB-WERKS
                              STLAN = IT_ITAB-STLAN
                              STLAL = IT_ITAB-STLAL
                              ICON  = C_ICON_DIFF
                     TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    MOVE: C_ICON_DIFF TO IT_9000-ICON,
          TEXT-M10    TO IT_9000-MSG.
    APPEND IT_9000.

    MOVE: C_CHECK TO PW_APPENDED.
  ENDIF.
ENDFORM.                    " check_diffrent
*&---------------------------------------------------------------------*
*&      Form  check_diffrent_02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DIFFRENT_02 USING PW_APPENDED.
  CHECK PW_APPENDED IS INITIAL.

  READ TABLE IT_ITAB WITH KEY WOHD  = IT_ITAB-WOHD
                              FSC   = IT_ITAB-FSC
                              WERKS = IT_ITAB-WERKS
                              STLAN = IT_ITAB-STLAN
                              STLAL = IT_ITAB-STLAL
                              ICON  = C_ICON_EQUAL
                     TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    READ TABLE IT_ITAB WITH KEY WOHD  = IT_ITAB-WOHD
                                FSC   = IT_ITAB-FSC
                                WERKS = IT_ITAB-WERKS
                                STLAN = IT_ITAB-STLAN
                                STLAL = IT_ITAB-STLAL
                                ICON  = C_ICON_NONE
                       TRANSPORTING NO FIELDS.
    IF SY-SUBRC EQ 0.
      MOVE: C_ICON_DIFF TO IT_9000-ICON,
            TEXT-M10    TO IT_9000-MSG.
      APPEND IT_9000.

      MOVE: C_CHECK TO PW_APPENDED.
    ENDIF.
  ENDIF.
ENDFORM.                    " check_diffrent_02
*&---------------------------------------------------------------------*
*&      Form  check_equal
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM CHECK_EQUAL USING PW_APPENDED.
  CHECK PW_APPENDED IS INITIAL.

  READ TABLE IT_ITAB WITH KEY WOHD  = IT_ITAB-WOHD
                              FSC   = IT_ITAB-FSC
                              WERKS = IT_ITAB-WERKS
                              STLAN = IT_ITAB-STLAN
                              STLAL = IT_ITAB-STLAL
                              ICON  = C_ICON_EQUAL
                     TRANSPORTING NO FIELDS.
  IF SY-SUBRC EQ 0.
    MOVE: C_ICON_EQUAL TO IT_9000-ICON,
          ' '          TO IT_9000-MSG.
    APPEND IT_9000.

    MOVE: C_CHECK TO PW_APPENDED.
  ENDIF.
ENDFORM.                    " check_equal
*&---------------------------------------------------------------------*
*&      Form  check_blank
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LW_APPENDED  text
*----------------------------------------------------------------------*
FORM CHECK_BLANK USING PW_APPENDED.
  CHECK PW_APPENDED IS INITIAL.


ENDFORM.                    " check_blank
*&---------------------------------------------------------------------*
*&      Module  user_command_9100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR SY-UCOMM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " user_command_9100  INPUT
*&---------------------------------------------------------------------*
*&      Form  create_alv_object_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_ALV_OBJECT_9100.
  IF WC_CONTAINER_9100 IS INITIAL.    "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT_9100.
    PERFORM SET_ATTRIBUTES_ALV_GRID_9100.
    PERFORM BUILD_FIELD_CATALOG_9100 USING 'IT_9100'.
    PERFORM ASSIGN_ITAB_TO_ALV_9100.
    PERFORM SSSIGN_EVENT_9100.
  ELSE.
    PERFORM BUILD_FIELD_CATALOG_9100 USING 'IT_9100'.
    PERFORM ASSIGN_ITAB_TO_ALV_9100.
  ENDIF.
ENDFORM.                    " create_alv_object_9100
*&---------------------------------------------------------------------*
*&      Form  assign_itab_to_alv_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_9100.
  CALL METHOD WC_ALV_9100->SET_TABLE_FOR_FIRST_DISPLAY
     EXPORTING I_STRUCTURE_NAME = 'ZSPP_APP725_9100'
               IS_LAYOUT        = W_IS_LAYOUT
               I_SAVE           = W_SAVE
               IS_VARIANT       = W_VARIANT
               I_DEFAULT        = SPACE
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_9100[].
ENDFORM.                    " assign_itab_to_alv_9100
*&---------------------------------------------------------------------*
*&      Form  sssign_event_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SSSIGN_EVENT_9100.
*/-- Create Object to receive events and link them to handler methods.
*  When the ALV Control raises the event for the specified instance
*  the corresponding method is automatically called.
  CREATE OBJECT EVENT_RECEIVER.

  SET HANDLER EVENT_RECEIVER->HANDLE_DOUBLE_CLICK  FOR WC_ALV_9100.
ENDFORM.                    " sssign_event_9100
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1945   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_9100 USING P_ITAB.
*-- adjust field catalog to suppress the output of already
*   displayed key fields of structure

  DATA: LW_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  W_REPID = SY-REPID.
  LW_ITAB = P_ITAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                  'S' 'WOHD'       ' ',
                                  ' ' 'COL_POS'    '1',
                                  'E' 'KEY'        'X',

                                  'S' 'FSC'        ' ',
                                  ' ' 'COL_POS'    '2',
                                  'E' 'KEY'        'X',

                                  'S' 'STLAL'      ' ',
                                  ' ' 'COL_POS'    '3',
                                  'E' 'KEY'        'X',

                                  'S' 'ICON'       ' ',
                                  ' ' 'COL_POS'    '4',
                                  'E' 'KEY'        'X',

                                  'S' 'MAKTX'      ' ',
                                  'E' 'COL_POS'    '5',

                                  'S' 'WERKS'      ' ',
                                  'E' 'COL_POS'    '6',

                                  'S' 'STLAN'      ' ',
                                  'E' 'COL_POS'    '7',

                                  'S' 'MDLFG'      ' ',
                                  'E' 'COL_POS'    '8',

                                  'S' 'MDLWO'      ' ',
                                  'E' 'COL_POS'    '9',

                                  'S' 'MDLFSC'     ' ',
                                  'E' 'COL_POS'    '10',

                                  'S' 'MSG'        ' ',
                                  'E' 'COL_POS'    '11',

                                  'S' 'ATNAM'      ' ',
                                  'E' 'NO_OUT'     'X'.
ENDFORM.                    " build_field_catalog_9100
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT_9100.
*- Create Container('GRID_CONTAINER') with Custom Control on screen
  CREATE OBJECT WC_CONTAINER_9100
         EXPORTING CONTAINER_NAME = WC_CONTROL_9100
         EXCEPTIONS
          CNTL_ERROR = 1
          CNTL_SYSTEM_ERROR = 2
          CREATE_ERROR = 3
          LIFETIME_ERROR = 4
          LIFETIME_DYNPRO_DYNPRO_LINK = 5.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT WC_ALV_9100
         EXPORTING I_PARENT      = WC_CONTAINER_9100
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object_9100
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid_9100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_9100.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : W_IS_LAYOUT, W_VARIANT.

  W_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
*  w_is_layout-sel_mode   = 'A'.      "/mode for select col and row
  W_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  W_IS_LAYOUT-CWIDTH_OPT = C_CHECK.  "/optimizes the column width
  W_IS_LAYOUT-NO_MERGING = C_CHECK.  "/Disable cell merging
  W_VARIANT-REPORT       = SY-REPID.
  W_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid_9100
*&---------------------------------------------------------------------*
*&      Form  processing_blank_wohd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESSING_BLANK_WOHD.

  LOOP AT IT_9000.   " WHERE icon EQ c_icon_none.
    PERFORM UPDATE_MATERIAL_MASTER USING IT_9000.

    MODIFY IT_9000.
  ENDLOOP.
ENDFORM.                    " processing_blank_wohd
*&---------------------------------------------------------------------*
*&      Form  UPDATE_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_RTN.
*"/Indexes of Selected Rows
  DATA: LT_ROWS   TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows

  CALL METHOD WC_ALV_9000->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD WC_ALV_9000->SET_USER_COMMAND
    EXPORTING
      I_UCOMM = 'UPDATE'.


  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0 OR LT_ROWS-INDEX EQ 0.
    MESSAGE E000(ZZ) WITH TEXT-M14.
  ENDIF.

  DATA: LW_INDEX LIKE SY-TABIX.
  LOOP AT LT_ROWS.
    READ TABLE IT_9000 INDEX LT_ROWS-INDEX.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M01.
    ENDIF.

    CHECK IT_9000-ICON EQ C_ICON_NONE OR
          IT_9000-ICON EQ C_ICON_DIFF.

    MOVE: SY-TABIX TO LW_INDEX.
    PERFORM UPDATE_MATERIAL_MASTER USING IT_9000.

    MODIFY IT_9000 INDEX LW_INDEX.
  ENDLOOP.

  PERFORM BUILD_FIELD_CATALOG USING 'IT_9000'.
  PERFORM ASSIGN_ITAB_TO_ALV_9000.
ENDFORM.                    " UPDATE_RTN
*&---------------------------------------------------------------------*
*&      Form  update_material_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_9000  text
*----------------------------------------------------------------------*
FORM UPDATE_MATERIAL_MASTER USING PW_9000 LIKE ZSPP_APP888_9000.
  DATA: LT_VAL_TABLE LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

  LOOP AT IT_ITAB WHERE WOHD  = PW_9000-WOHD
                    AND FSC   = PW_9000-FSC
                    AND WERKS = PW_9000-WERKS
                    AND STLAN = PW_9000-STLAN
                    AND STLAL = PW_9000-STLAL.
    MOVE: IT_ITAB-ATNAM  TO LT_VAL_TABLE-ATNAM,
          IT_ITAB-MDLFSC TO LT_VAL_TABLE-ATWRT.

    APPEND LT_VAL_TABLE.
  ENDLOOP.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = PW_9000-WOHD
            MODE         = 'W'
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = LT_VAL_TABLE
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.
  IF SY-SUBRC <> 0.
    MOVE: TEXT-M12   TO PW_9000-MSG.

  ELSE.
    MOVE: C_ICON_EQUAL TO PW_9000-ICON,
          TEXT-M13     TO PW_9000-MSG,
          '-'          TO PW_9000-CHECK.


    LOOP AT IT_ITAB WHERE WOHD  = PW_9000-WOHD
                      AND FSC   = PW_9000-FSC
                      AND WERKS = PW_9000-WERKS
                      AND STLAN = PW_9000-STLAN
                      AND STLAL = PW_9000-STLAL.
      MOVE: C_ICON_EQUAL   TO IT_ITAB-ICON,
            TEXT-M13       TO IT_ITAB-MSG,
            IT_ITAB-MDLFSC TO IT_ITAB-MDLWO.

      MODIFY IT_ITAB.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " update_material_master
*&---------------------------------------------------------------------*
*&      Form  get_fsc_info_input_fsc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_FSC_INFO_INPUT_FSC.
  DATA: LW_STRLEN   TYPE   I,
        LW_TOPMAT   LIKE   CSTMAT.

  DATA: BEGIN OF LT_MAST OCCURS 0,
          MATNR   LIKE   MAST-MATNR,
          WERKS   LIKE   MAST-WERKS,
          STLAN   LIKE   MAST-STLAN,
          STLAL   LIKE   MAST-STLAL,
        END   OF LT_MAST.

  DATA: LT_STB TYPE  STPOX OCCURS 0 WITH HEADER LINE.

  CLEAR: IT_FSC, IT_FSC[].

  SELECT A~MATNR B~WERKS B~STLAN B~STLAL
    INTO CORRESPONDING FIELDS OF TABLE LT_MAST
    FROM MARA AS A INNER JOIN MAST AS B
                      ON A~MATNR = B~MATNR
                   INNER JOIN STKO AS C
                      ON B~STLNR = C~STLNR
                     AND B~STLAL = C~STLAL
                     AND C~STLTY = 'M'
                     AND C~STLST = '01'
   WHERE A~MTART EQ 'FERT'
*     AND a~matnr IN s_fsc
     AND B~WERKS IN S_WERKS
     AND B~STLAN EQ C_STLAN.

  LOOP AT LT_MAST.
    CLEAR: LW_TOPMAT, LT_STB, LT_STB[].

    CALL FUNCTION 'CS_BOM_EXPL_MAT_V2'
         EXPORTING
              AUMNG                 = 0
              CAPID                 = C_CAPID
              CUOVS                 = '0'
              DATUV                 = P_DATUV
              MKTLS                 = 'X'
              CUOBJ                 = '999999999999'
              MMORY                 = '0'
              MTNRV                 = LT_MAST-MATNR
              STPST                 = 0
              STLAN                 = LT_MAST-STLAN
              STLAL                 = LT_MAST-STLAL
              SVWVO                 = 'X'
              WERKS                 = LT_MAST-WERKS
              VRSVO                 = 'X'
         IMPORTING
              TOPMAT                = LW_TOPMAT
         TABLES
              STB                   = LT_STB
         EXCEPTIONS
              ALT_NOT_FOUND         = 1
              CALL_INVALID          = 2
              MATERIAL_NOT_FOUND    = 3
              MISSING_AUTHORIZATION = 4
              NO_BOM_FOUND          = 5
              NO_PLANT_DATA         = 6
              NO_SUITABLE_BOM_FOUND = 7
              CONVERSION_ERROR      = 8
              OTHERS                = 9.
    IF SY-SUBRC <> 0 OR LW_TOPMAT-STLAL NE LT_MAST-STLAL.
      CONTINUE.
    ENDIF.

    LOOP AT LT_STB.
      SELECT SINGLE * FROM MARC WHERE MATNR = LT_STB-IDNRK
                                  AND WERKS = LT_STB-WERKS.
      IF SY-SUBRC NE 0.
        MESSAGE E000(ZZ) WITH TEXT-M01.
      ENDIF.

      CHECK MARC-DISPO = 'M01'.            "Check module or not

      PERFORM CHECK_OD_VALUE USING LT_STB-KNOBJ W_SUBRC.

      CHECK W_SUBRC EQ 0.

      MOVE: LT_MAST-MATNR       TO IT_FSC-FSC,
            LT_STB-WERKS        TO IT_FSC-WERKS,
            LT_STB-STLAN        TO IT_FSC-STLAN,
            LT_STB-STLAL        TO IT_FSC-STLAL,
            LT_STB-OJTXB        TO IT_FSC-MAKTX,
            LT_STB-IDNRK+3(2)   TO IT_FSC-MDLFG.
      IF LT_STB-IDNRK+3(2) EQ 'CP'.
** Chnaged by Furong on 05/27/09
*        LW_STRLEN = STRLEN( LT_STB-IDNRK ).
*        LW_STRLEN = LW_STRLEN - 2.
*        MOVE: LT_STB-IDNRK(LW_STRLEN) TO IT_FSC-MODULE.
        IT_FSC-MODULE = LT_STB-IDNRK+0(12).
** End of change
      ELSE.
        MOVE: LT_STB-IDNRK TO IT_FSC-MODULE.
      ENDIF.

      COLLECT IT_FSC.
    ENDLOOP.
  ENDLOOP.

*---// Set target WOHD list
  DATA: LW_SURFIX(3).

  CLEAR: R_WOHD_TARGET, R_WOHD_TARGET[].

  LOOP AT LT_MAST.
    CONCATENATE '0' LT_MAST-STLAL INTO LW_SURFIX.
    CONCATENATE LT_MAST-MATNR LW_SURFIX INTO R_WOHD_TARGET-LOW
      SEPARATED BY SPACE.

    MOVE: 'I'  TO R_WOHD_TARGET-SIGN,
          'EQ' TO R_WOHD_TARGET-OPTION.

    COLLECT R_WOHD_TARGET.
  ENDLOOP.

  READ TABLE R_WOHD_TARGET INDEX 1.
  IF SY-SUBRC NE 0.
    MOVE: 'I'                  TO R_WOHD_TARGET-SIGN,
          'EQ'                 TO R_WOHD_TARGET-OPTION,
          'Z18Z18Z18Z18Z18Z18' TO R_WOHD_TARGET-LOW.

    APPEND R_WOHD_TARGET.
  ENDIF.
ENDFORM.                    " get_fsc_info_input_fsc
*&---------------------------------------------------------------------*
*&      Form  GET_WOHD_INFO_INPUT_FSC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_WOHD_INFO_INPUT_FSC USING PW_CONTINUE.
  DATA: BEGIN OF LT_MARA OCCURS 0,
          MATNR   LIKE   MARA-MATNR,
          MAKTX   LIKE   MAKT-MAKTX,
        END   OF LT_MARA.

  DATA: BEGIN OF LT_AUSP OCCURS 0,
          ATINN   LIKE   AUSP-ATINN,
          ATWRT   LIKE   AUSP-ATWRT,
        END   OF LT_AUSP.

*---// Read Work order header
  SELECT A~MATNR A~MAKTX
    INTO CORRESPONDING FIELDS OF TABLE LT_MARA
    FROM MAKT AS A INNER JOIN MARA AS B
                      ON A~MATNR EQ B~MATNR
   WHERE A~MAKTX IN R_WOHD_TARGET
     AND A~SPRAS EQ SY-LANGU
     AND B~MTART EQ 'WOHD'.

  CHECK SY-SUBRC EQ 0.

  LOOP AT LT_MARA.
    IF LT_MARA-MATNR+12(2) = 'XX' OR
       LT_MARA-MATNR+12(2) = 'XY'.
      DELETE LT_MARA.
    ENDIF.
  ENDLOOP.

*---// Read Module Characteristic code
  CLEAR: IT_CABNT, IT_CABNT[].

  SELECT A~ATINN ATNAM ATBEZ
    INTO CORRESPONDING FIELDS OF TABLE IT_CABNT
    FROM CABNT AS A INNER JOIN CABN AS B
                       ON A~ATINN EQ B~ATINN
                      AND A~ADZHL EQ B~ADZHL
   WHERE A~SPRAS =    SY-LANGU
     AND A~ATBEZ LIKE C_MODULE
     AND A~LKENZ =    ' '
     AND B~LKENZ =    ' '.
  IF SY-SUBRC NE 0.
    MESSAGE S000(ZZ) WITH TEXT-M03.
    CLEAR: PW_CONTINUE. EXIT.
  ENDIF.

  DESCRIBE TABLE IT_CABNT LINES W_MODULE_CNT.

*---// Set Work Order Information
  CLEAR: IT_WOHD, IT_WOHD[].

  LOOP AT LT_MARA.
    CLEAR: LT_AUSP, LT_AUSP[].

    SELECT ATINN ATWRT
      INTO CORRESPONDING FIELDS OF TABLE LT_AUSP
      FROM AUSP
       FOR ALL ENTRIES IN IT_CABNT
     WHERE OBJEK EQ LT_MARA-MATNR
       AND ATINN EQ IT_CABNT-ATINN
       AND KLART EQ '001'.

    LOOP AT IT_CABNT.
      CLEAR: LT_AUSP.

      READ TABLE LT_AUSP WITH KEY ATINN = IT_CABNT-ATINN.

      MOVE: LT_MARA-MATNR       TO IT_WOHD-WOHD,
            LT_MARA-MAKTX       TO IT_WOHD-MAKTX,
            IT_CABNT-ATBEZ      TO IT_WOHD-ATBEZ,
            IT_CABNT-ATBEZ+7(2) TO IT_WOHD-MDLFG.

      IF LT_AUSP-ATWRT EQ '-'.
        CLEAR: LT_AUSP-ATWRT.
      ENDIF.

      MOVE: LT_AUSP-ATWRT       TO IT_WOHD-MODULE.


      APPEND IT_WOHD.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " GET_WOHD_INFO_INPUT_FSC
*&---------------------------------------------------------------------*
*&      Form  check_od_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_OD_VALUE USING PW_KNOBJ PW_SUBRC.
  DATA: LT_CUOB LIKE CUOB OCCURS 0 WITH HEADER LINE.

  DATA: LT_CUKB LIKE CUKB OCCURS 0 WITH HEADER LINE.

  CLEAR: PW_SUBRC.

  SELECT * INTO TABLE LT_CUOB
    FROM CUOB
   WHERE KNTAB =  'STPO'
     AND KNOBJ =  PW_KNOBJ
     AND DATUV <= P_DATUV.
  IF SY-SUBRC NE 0.
    MOVE: '9999999999' TO LT_CUOB-KNNUM.
    APPEND LT_CUOB.
  ENDIF.

  SELECT * INTO TABLE LT_CUKB
    FROM CUKB
     FOR ALL ENTRIES IN LT_CUOB
   WHERE KNNUM =  LT_CUOB-KNNUM
     AND ADZHL =  LT_CUOB-ADZHL
     AND DATUV <= P_DATUV.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  LOOP AT LT_CUKB WHERE NOT KNNAM EQ C_EMPTY_OD.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    MOVE: 4 TO PW_SUBRC.
  ENDIF.
ENDFORM.                    " check_od_value
*&---------------------------------------------------------------------*
*&      Form  MAKE_DROPDOWN_LIST_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX.

  DATA: L_ATINN              LIKE CABN-ATINN,
        L_ATNAM              LIKE CABN-ATNAM,
        L_ATWTB              LIKE CAWNT-ATWTB,
        L_ATWRT              LIKE AUSP-ATWRT.

  CLEAR : XLIST[] , XVALUE.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT N~ATWRT T~ATWTB INTO (L_ATWRT, L_ATWTB)
    FROM CAWN AS N INNER JOIN CAWNT AS T
      ON N~ATINN = T~ATINN
     AND N~ATZHL = T~ATZHL
   WHERE N~ATINN = L_ATINN
     AND T~SPRAS = SY-LANGU .
    XVALUE-TEXT = L_ATWTB.  " ZTPP_VEH_MODEL-NAME.
    XVALUE-KEY  = L_ATWRT.  " ZTPP_VEH_MODEL-MODEL.
    APPEND XVALUE TO XLIST.
  ENDSELECT.
  XVALUE-TEXT = 'For all'.
  XVALUE-KEY  = ' '.
  APPEND XVALUE TO XLIST.

  PERFORM LIST_BOX_FUNCTION USING 'P_MODEL'.

ENDFORM.                    " MAKE_DROPDOWN_LIST_BOX
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4055   text
*----------------------------------------------------------------------*
FORM LIST_BOX_FUNCTION USING   P_LIST_NAME .
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = P_LIST_NAME  " list box
            VALUES          = XLIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.

ENDFORM.                    " LIST_BOX_FUNCTION
