*----------------------------------------------------------------------
* Program ID        : ZR_MM_VEHICLE_SCRAP
* Title             : Scrap Test Vehicle Tracking record
* Created on        : 11/16/2007
* Created by        : Rakesh Gandhi
* Specifications By : Paul Shrewsbury
* Description       : This program Scrap test vehicle tracking records
*----------------------------------------------------------------------
REPORT ZR_MM_VEHICLE_SCRAP MESSAGE-ID ZMCO.
TYPE-POOLS : SLIS,
             ICON.

TABLES: MCHA,
        KLAH,
        CSKS,
        BSEG.

*--------------------------------------------------------------------*
* DATA DECLARATION
*--------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA: BEGIN OF IT_CHARG OCCURS 0   ,
        CHARG LIKE MCHA-CHARG      ,
      END OF IT_CHARG              .

DATA: VIN_NUM        TYPE ZE_VIN        ,
      FULL_SPEC_CD   TYPE ZE_FSC        ,
      ASK_DEPT       TYPE ZE_ASK_DEPT   ,
      USAGE_DEPT     TYPE ZE_USAGE_DEPT ,
      GL_ACT         TYPE ZE_GLACT      .

DATA: BEGIN OF IT_CHARG1 OCCURS 0             ,
        CHARG              LIKE MCHA-CHARG    ,
        VIN_NUM            TYPE ZE_VIN        ,
        FSC                TYPE ZE_FSC        ,
        EXT_CLR(3)         TYPE C             ,
        INT_CLR(3)         TYPE C             ,
        ASK_DEPT           TYPE ZE_ASK_DEPT   ,
        USAGE_DEPT         TYPE ZE_USAGE_DEPT ,
        USAGE_TEXT(30)     TYPE C             ,
        USAGE_DEPT1        TYPE ZE_ASK_DEPT   ,
        SCRAP_DATE(10)     TYPE C             ,
        SCRAP_TAG_NO(10)   TYPE C             ,  " P_SCRAP_APP_DOC
        SCRAP_RES(10)      TYPE C             ,
        CHKBOX(1)          TYPE C             ,
        ICON               TYPE ICON_D        ,
      END OF IT_CHARG1                        ,

      BEGIN OF IT_AUSP_TMP OCCURS 0           ,
        OBJEK LIKE AUSP-OBJEK                 ,
        ATWRT LIKE AUSP-ATWRT                 ,
        ATFLV LIKE AUSP-ATFLV                 ,
      END OF IT_AUSP_TMP                      ,

      BEGIN OF IT_INOB OCCURS 0               ,
        OBJEK LIKE INOB-OBJEK                 ,
      END OF IT_INOB                          ,

      BEGIN OF IT_OBJEK OCCURS 0              ,
        CUOBJ LIKE INOB-CUOBJ                 ,
      END OF IT_OBJEK                         ,

      BEGIN OF IT_CABN OCCURS 0               ,
        ATINN LIKE CABN-ATINN                 ,
        ATNAM LIKE CABN-ATNAM                 ,
        ATFOR LIKE CABN-ATFOR                 ,
      END OF IT_CABN                          ,

      BEGIN OF IT_KSML OCCURS 0               ,
        IMERK LIKE KSML-IMERK                 ,
      END OF IT_KSML                          ,

      BEGIN OF IT_SHOP_DT OCCURS 0,
        DATE LIKE SY-DATUM        ,
      END OF IT_SHOP_DT           ,

      BEGIN OF IT_SHIP_DT OCCURS 0,
        DATE LIKE SY-DATUM        ,
      END OF IT_SHIP_DT           ,

      BEGIN OF IT_VIN OCCURS 0    ,
        VIN(18) TYPE C            ,
      END OF IT_VIN               ,

      BEGIN OF IT_FSC OCCURS 0    ,
        FSC(18) TYPE C            ,
      END OF IT_FSC               ,

      BEGIN OF IT_ASKDEP OCCURS 0 ,
        ASKDEPT(10) TYPE C        ,
      END OF IT_ASKDEP            ,

      BEGIN OF IT_USAGEDEP OCCURS 0,
        USAGE_DEPT(10) TYPE C      ,
      END OF IT_USAGEDEP           ,

      BEGIN OF IT_GLNO OCCURS 0   ,
        GLNO(6) TYPE N            ,
      END OF IT_GLNO              ,

** Changed by Furong on 02/22/08  "UD1K942932
      BEGIN OF IT_SCRAP_RES OCCURS 0   ,
        SCRAPR(10) TYPE N            ,
      END OF IT_SCRAP_RES            ,
** End of change on 02/22/08

      BEGIN OF IT_RETTAB OCCURS 0 .
        INCLUDE STRUCTURE BAPIRET2.
DATA: END OF IT_RETTAB            ,

      BEGIN OF IT_OBJECT OCCURS 0             .
        INCLUDE STRUCTURE BAPI1003_OBJECT_KEYS.
DATA: END OF IT_OBJECT                        ,

*-Holds data for charcateristics with type NUM
      BEGIN OF IT_NUMTAB OCCURS 0                  .
        INCLUDE STRUCTURE BAPI1003_ALLOC_VALUES_NUM.
DATA: END OF IT_NUMTAB                             ,

*-Holds data for charcateristics with type CHAR/DATE
      BEGIN OF IT_CHATAB OCCURS 0                   .
        INCLUDE STRUCTURE BAPI1003_ALLOC_VALUES_CHAR.
DATA: END OF IT_CHATAB                              ,

      BEGIN OF IT_CURTAB OCCURS 0                   .
        INCLUDE STRUCTURE BAPI1003_ALLOC_VALUES_CURR.
DATA: END OF IT_CURTAB                              .

DATA: IT_CHARG3 LIKE IT_CHARG OCCURS 0 WITH HEADER LINE ,
      IT_CHARG2 LIKE IT_CHARG OCCURS 0 WITH HEADER LINE ,
      IT_CLBATCH LIKE CLBATCH OCCURS 0 WITH HEADER LINE ,
      CUSTOM_CONTROL TYPE SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID       TYPE REF TO CL_GUI_ALV_GRID        ,
      GRID_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER     .

*-ALV Data declaration
DATA : GT_FIELDCAT TYPE LVC_T_FCAT WITH HEADER LINE,
       GV_REPID    LIKE SY-REPID              ,
       GV_VARIANT  TYPE DISVARIANT            , " for parmtr IS_VARIANT
       GV_SAVE     TYPE C   VALUE 'A'         , " for Parameter I_SAVE
       GV_CHARG    LIKE MCHA-CHARG            ,
       GV_OBJECT   LIKE BAPI1003_KEY-OBJECT   ,
       OK_CODE     LIKE SY-UCOMM              ,
       GV_DOKAR    LIKE DRAW-DOKAR VALUE 'ICD',
       GV_DOKNR    LIKE DRAW-DOKNR            ,
       GV_TABIX    LIKE SY-TABIX              ,
       GV_TXT(100) TYPE C                     ,
       GV_VIN      LIKE CABN-ATINN            ,
       GV_FSC      LIKE CABN-ATINN            ,
       GV_ASKDEPT  LIKE CABN-ATINN            ,
       GV_USAGEDEP LIKE CABN-ATINN            ,
       GV_GLNO     LIKE CABN-ATINN            ,
       GV_SCRAP_RES LIKE CABN-ATINN           .

DATA : GS_FIELDCAT TYPE LVC_S_FCAT,
       GS_LAYOUT   TYPE LVC_S_LAYO. " The Layout Structure

CONSTANTS: C_MATNR LIKE MARA-MATNR  VALUE 'TEST VEHICLE' ,
           C_WERKS LIKE T001W-WERKS VALUE 'P001'         ,
           C_KLART LIKE KLAH-KLART  VALUE '022'          ,
           C_CLASS LIKE KLAH-CLASS  VALUE 'P_VEHICLE_TRACKING'.

*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
    HANDLE_TOOLBAR FOR EVENT TOOLBAR
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_OBJECT E_INTERACTIVE,

    HANDLE_USER_COMMAND  FOR EVENT USER_COMMAND
                         OF CL_GUI_ALV_GRID
                         IMPORTING E_UCOMM SENDER.
ENDCLASS.                    "LCL_EVENT_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.
  METHOD HANDLE_TOOLBAR.
    CONSTANTS: C_SEPARATOR TYPE I VALUE 3.

    DATA: LS_TOOLBAR  TYPE STB_BUTTON.
*-Append seperator to the normal toolbar
    CLEAR LS_TOOLBAR.
    MOVE C_SEPARATOR TO LS_TOOLBAR-BUTN_TYPE.
    APPEND LS_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*-Append a new button that to the toolbar. Use E_OBJECT of
*-event toolbar. E_OBJECT is of type CL_ALV_EVENT_TOOLBAR_SET.
*-This class has one attribute MT_TOOLBAR which is of table type
*-TTB_BUTTON. The structure is STB_BUTTON
    CLEAR LS_TOOLBAR.
    MOVE 'SCRP_BATCH'     TO LS_TOOLBAR-FUNCTION.
    MOVE  ICON_SCRAP      TO LS_TOOLBAR-ICON.
    MOVE 'SCRAP BATCH'    TO LS_TOOLBAR-QUICKINFO.
    MOVE 'Scrap  Batch'   TO LS_TOOLBAR-TEXT.
    MOVE ' '              TO LS_TOOLBAR-DISABLED.
    APPEND LS_TOOLBAR     TO E_OBJECT->MT_TOOLBAR.
*-Create Document button
    MOVE 'CR_DOC'          TO LS_TOOLBAR-FUNCTION.
    MOVE  ICON_DOCUMENT    TO LS_TOOLBAR-ICON.
    MOVE 'CREATE DOCUMENT' TO LS_TOOLBAR-QUICKINFO.
    MOVE 'Create Document' TO LS_TOOLBAR-TEXT.
    MOVE ' '               TO LS_TOOLBAR-DISABLED.
    APPEND LS_TOOLBAR      TO E_OBJECT->MT_TOOLBAR.
  ENDMETHOD.                    "handle_toolbar

  METHOD HANDLE_USER_COMMAND.
    PERFORM SUB_EVENT_UCOMM USING E_UCOMM.
  ENDMETHOD.                 "handle_user_command
ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION
*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-009.
PARAMETERS: P_BODY RADIOBUTTON GROUP G1 DEFAULT 'X' USER-COMMAND UCOM,
            P_VIN  RADIOBUTTON GROUP G1.

SELECT-OPTIONS: S_BODY FOR MCHA-CHARG NO INTERVALS MODIF ID 127." Obj No
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-010.
SELECT-OPTIONS: S_VIN    FOR VIN_NUM MODIF ID 128     , " VIN No
                S_FSC    FOR FULL_SPEC_CD MODIF ID 128, " Full Spec cod
                S_SHOPDT FOR SY-DATUM MODIF ID 128    , " RP18 shopdate
                S_SHIPDT FOR SY-DATUM MODIF ID 128    , " Ship Out Date
*                S_ASKDEP FOR ASK_DEPT MODIF ID 128    , " Asking Dept
                S_ASKDEP FOR CSKS-KOSTL MODIF ID 128    , " Asking Dept
                S_UDEPT  FOR USAGE_DEPT MODIF ID 128  , " Dest/Using Dep
*               S_GLNO   FOR GL_ACT MODIF ID 128      , " G/L Account No
                S_GLNO   FOR BSEG-HKONT MODIF ID 128  , " G/L Account No
                S_SCRAPR   FOR IT_CHARG1-SCRAP_RES MODIF ID 128      .

SELECTION-SCREEN END OF BLOCK B2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  SY-TITLE = 'Test Vehicle Tracking - Scrap Document'.
  PERFORM GET_CHARACTERISTICS.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF SY-UCOMM <> 'UCOM'.
*-Body Number
    IF P_BODY = 'X'.
      CLEAR: S_VIN[], S_VIN, S_FSC[], S_FSC, S_SHOPDT[], S_SHOPDT,
             S_SHIPDT[], S_SHIPDT, S_ASKDEP[], S_ASKDEP, S_UDEPT[],
             S_UDEPT, S_GLNO[], S_GLNO, S_SCRAPR[], S_SCRAPR.
      IF S_BODY[] IS INITIAL.
        MESSAGE E000 WITH TEXT-011.
      ENDIF.
*-VIN Number
    ELSEIF P_VIN = 'X'.
      CLEAR S_BODY[].
      IF S_VIN[]    IS INITIAL AND S_FSC[]    IS INITIAL AND
         S_SHOPDT[] IS INITIAL AND S_SHIPDT[] IS INITIAL AND
         S_ASKDEP[] IS INITIAL AND S_UDEPT[]   IS INITIAL AND
         S_GLNO[]   IS INITIAL AND  S_SCRAPR[]   IS INITIAL.
        MESSAGE E000 WITH TEXT-016.
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  PERFORM CHANGE_SCREEN_RUNTIME.

*----------------------------------------------------------------------*
* START OF SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM GET_DATA.

  CHECK NOT IT_CHARG[] IS INITIAL.

*-VIN Number
  IF NOT S_VIN[] IS INITIAL.
    REFRESH IT_VIN.
    CLEAR   IT_VIN.
    LOOP AT S_VIN.
      IT_VIN-VIN = S_VIN-LOW.
      APPEND IT_VIN.
      CLEAR  IT_VIN.
    ENDLOOP.
    SORT IT_VIN BY VIN.
  ENDIF.

*-Full Spec Code
  IF NOT S_FSC[] IS INITIAL.
    REFRESH IT_FSC.
    CLEAR   IT_FSC.
    LOOP AT S_FSC.
      IT_FSC-FSC = S_FSC-LOW.
      APPEND IT_FSC.
      CLEAR  IT_FSC.
    ENDLOOP.
    SORT IT_FSC BY FSC.
  ENDIF.

*-Shop Date
  IF NOT S_SHOPDT[] IS INITIAL.
    REFRESH IT_SHOP_DT.
    CLEAR   IT_SHOP_DT.

    LOOP AT S_SHOPDT.
      IT_SHOP_DT-DATE = S_SHOPDT-LOW.
      APPEND IT_SHOP_DT.
      CLEAR  IT_SHOP_DT.
    ENDLOOP.
    SORT IT_SHOP_DT BY DATE.
  ENDIF.

*-Ship out date
  IF NOT S_SHIPDT[] IS INITIAL.
    REFRESH IT_SHIP_DT.
    CLEAR   IT_SHIP_DT.
    LOOP AT S_SHIPDT.
      IT_SHIP_DT-DATE = S_SHIPDT-LOW.
      APPEND IT_SHIP_DT.
      CLEAR  IT_SHIP_DT.
    ENDLOOP.
    SORT IT_SHIP_DT BY DATE.
  ENDIF.

*-Asking Department
  IF NOT S_ASKDEP[] IS INITIAL.
    REFRESH IT_ASKDEP.
    CLEAR   IT_ASKDEP.
    LOOP AT S_ASKDEP.
      IT_ASKDEP-ASKDEPT = S_ASKDEP-LOW.
      APPEND IT_ASKDEP.
      CLEAR  IT_ASKDEP.
    ENDLOOP.
    SORT IT_ASKDEP BY ASKDEPT.
  ENDIF.

*-Destination/Using Department
  IF NOT S_UDEPT[] IS INITIAL.
    REFRESH IT_USAGEDEP.
    CLEAR   IT_USAGEDEP.
    LOOP AT S_UDEPT.
      IT_USAGEDEP-USAGE_DEPT = S_UDEPT-LOW.
      APPEND IT_USAGEDEP.
      CLEAR  IT_USAGEDEP.
    ENDLOOP.
    SORT IT_USAGEDEP BY USAGE_DEPT.
  ENDIF.

*-G/L Account Number
  IF NOT S_GLNO[] IS INITIAL.
    REFRESH IT_GLNO.
    CLEAR   IT_GLNO.
    LOOP AT S_GLNO.
      IT_GLNO-GLNO = S_GLNO-LOW.
      APPEND IT_GLNO.
      CLEAR  IT_GLNO.
    ENDLOOP.
    SORT IT_GLNO BY GLNO.
  ENDIF.

** Changed by Furong on 02/22/08  "UD1K942932
  IF NOT S_SCRAPR[] IS INITIAL.
    REFRESH IT_SCRAP_RES.
    CLEAR   IT_SCRAP_RES.
    LOOP AT S_SCRAPR.
      IT_SCRAP_RES-SCRAPR = S_SCRAPR-LOW.
      APPEND IT_SCRAP_RES.
      CLEAR  IT_SCRAP_RES.
    ENDLOOP.
    SORT IT_SCRAP_RES BY SCRAPR.
  ENDIF.
** End of change

  LOOP AT IT_CHARG.
    PERFORM BUILD_OBJECT_KEY USING IT_CHARG-CHARG.
    PERFORM EXTRACT_ORIGINAL_BATCH.
    IF NOT IT_CLBATCH[] IS INITIAL.
      PERFORM FILL_INTERN_TABLE.
    ENDIF.
  ENDLOOP.

  IF P_VIN = 'X'.
*-Check for deleted records
    IF NOT IT_CHARG1[] IS INITIAL.
      SELECT CHARG
           FROM MCHA
           INTO TABLE IT_CHARG2
           FOR ALL ENTRIES IN IT_CHARG1
           WHERE MATNR = C_MATNR AND
                 WERKS = C_WERKS AND
                 LVORM = 'X'     AND
                 CHARG = IT_CHARG1-CHARG.

      SORT IT_CHARG2 BY CHARG.
      DELETE ADJACENT DUPLICATES FROM IT_CHARG2 COMPARING CHARG.
      LOOP AT IT_CHARG1.
        CLEAR GV_TABIX.
        GV_TABIX = SY-TABIX.
        READ TABLE IT_CHARG2 WITH KEY CHARG = IT_CHARG1-CHARG.
        IF SY-SUBRC = 0.
          DELETE IT_CHARG1 INDEX GV_TABIX.
        ENDIF.
      ENDLOOP.

      LOOP AT IT_CHARG2.
        CLEAR GV_TXT.
        CONCATENATE TEXT-018 IT_CHARG2-CHARG TEXT-019 INTO GV_TXT
                                      SEPARATED BY SPACE.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  TEXTLINE1 = GV_TXT.
      ENDLOOP.
      IF IT_CHARG1[] IS INITIAL.
        EXIT.
      ENDIF.

    ELSE.
      MESSAGE I000 WITH TEXT-001.
      LEAVE LIST-PROCESSING.
    ENDIF.      " IF NOT it_charg1[] IS INITIAL.
  ELSE.
    IF IT_CHARG1[] IS INITIAL.
      MESSAGE I000 WITH TEXT-001.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
*       Subroutine to get data from DB table
*----------------------------------------------------------------------*
FORM GET_DATA.
  REFRESH: IT_CHARG, IT_CHARG2, IT_CHARG3, IT_AUSP_TMP.
  CLEAR:   IT_CHARG, IT_CHARG2, IT_CHARG3, IT_AUSP_TMP.
*-All Batches
  IF P_BODY = 'X'.
    SELECT CHARG
           FROM MCHA
           INTO TABLE IT_CHARG
           WHERE MATNR = C_MATNR AND
                 WERKS = C_WERKS AND
                 CHARG IN S_BODY.

*-Deleted Batches
    SELECT CHARG
           FROM MCHA
           INTO TABLE IT_CHARG2
           WHERE MATNR = C_MATNR AND
                 WERKS = C_WERKS AND
                 LVORM = 'X'     AND
                 CHARG IN S_BODY.

*-If characteristic search is choosen
  ELSEIF P_VIN = 'X'.
*-If VIN Number is entered
    IF NOT S_VIN[] IS INITIAL.
      SELECT OBJEK ATWRT FROM AUSP
                   INTO TABLE IT_AUSP_TMP
                   WHERE ATINN = GV_VIN AND
                         ATWRT IN S_VIN AND
                         KLART = '022'.
    ENDIF.
*-If Full Spec Code is entered
    IF NOT S_FSC[] IS INITIAL.
      SELECT OBJEK ATWRT FROM AUSP
                   APPENDING TABLE IT_AUSP_TMP
                   WHERE ATINN = GV_FSC AND
                         ATWRT IN S_FSC AND
                         KLART = '022'.
    ENDIF.
*-If Asking Department is entered
    IF NOT S_ASKDEP[] IS INITIAL.
      SELECT OBJEK ATWRT FROM AUSP
                   APPENDING TABLE IT_AUSP_TMP
                   WHERE ATINN = GV_ASKDEPT AND
                         ATWRT IN S_ASKDEP  AND
                         KLART = '022'.
    ENDIF.
*-If Destination/Using Department is entered
    IF NOT S_UDEPT[] IS INITIAL.
      SELECT OBJEK ATWRT FROM AUSP
                   APPENDING TABLE IT_AUSP_TMP
                   WHERE ATINN = GV_USAGEDEP AND
                         ATWRT IN S_UDEPT    AND
                         KLART = '022'.
    ENDIF.

** Changed by Furong on 02/22/08  "UD1K942932
    IF NOT S_SCRAPR[] IS INITIAL.
      SELECT OBJEK ATWRT FROM AUSP
                   APPENDING TABLE IT_AUSP_TMP
                   WHERE ATINN = GV_SCRAP_RES AND
                         ATWRT IN S_SCRAPR    AND
                         KLART = '022'.
    ENDIF.

** End of change
*-If Ship or Shop Date is entered
    IF S_VIN[]    IS INITIAL AND S_FSC[]  IS INITIAL AND
       S_ASKDEP[] IS INITIAL AND S_UDEPT[] IS INITIAL AND
       S_SCRAPR[]  IS INITIAL.

      SELECT CHARG
             FROM MCHA
             INTO TABLE IT_CHARG
             WHERE MATNR = C_MATNR AND
                   WERKS = C_WERKS.
    ENDIF.

    IF IT_AUSP_TMP[] IS INITIAL AND IT_CHARG[] IS INITIAL.
      MESSAGE I000 WITH TEXT-001.
      LEAVE LIST-PROCESSING.
    ENDIF.

    IF IT_CHARG[] IS INITIAL.

      SORT IT_AUSP_TMP BY OBJEK.
      DELETE ADJACENT DUPLICATES FROM IT_AUSP_TMP COMPARING OBJEK.

      LOOP AT IT_AUSP_TMP.
        IT_OBJEK-CUOBJ = IT_AUSP_TMP-OBJEK.
        APPEND IT_OBJEK.
        CLEAR  IT_OBJEK.
      ENDLOOP.

      SELECT OBJEK FROM INOB
                 INTO TABLE IT_INOB
                 FOR ALL ENTRIES IN IT_OBJEK
                 WHERE CUOBJ = IT_OBJEK-CUOBJ.

      LOOP AT IT_INOB.
        IT_CHARG3-CHARG = IT_INOB-OBJEK+22(10).
        APPEND IT_CHARG3.
        CLEAR  IT_CHARG3.
      ENDLOOP.

      SELECT CHARG
             FROM MCHA
             INTO TABLE IT_CHARG
             FOR ALL ENTRIES IN IT_CHARG3
             WHERE MATNR = C_MATNR AND
                   WERKS = C_WERKS AND
                   CHARG = IT_CHARG3-CHARG.
    ENDIF.      " IF it_charg[] IS INITIAL
  ENDIF.    " IF p_body = 'X'.

  IF IT_CHARG[] IS INITIAL.
    MESSAGE I000 WITH TEXT-001.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SORT IT_CHARG BY CHARG.
  DELETE ADJACENT DUPLICATES FROM IT_CHARG COMPARING CHARG.

  IF P_BODY = 'X'.
    IF NOT IT_CHARG2[] IS INITIAL.
      SORT IT_CHARG2 BY CHARG.
      DELETE ADJACENT DUPLICATES FROM IT_CHARG2 COMPARING CHARG.
      LOOP AT IT_CHARG.
        CLEAR GV_TABIX.
        GV_TABIX = SY-TABIX.
        READ TABLE IT_CHARG2 WITH KEY CHARG = IT_CHARG-CHARG.
        IF SY-SUBRC = 0.
          DELETE IT_CHARG INDEX GV_TABIX.
        ENDIF.
      ENDLOOP.

      LOOP AT IT_CHARG2.
        CLEAR GV_TXT.
        CONCATENATE TEXT-018 IT_CHARG2-CHARG TEXT-017 INTO GV_TXT
                                      SEPARATED BY SPACE.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  TEXTLINE1 = GV_TXT.
      ENDLOOP.
      IF IT_CHARG[] IS INITIAL.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " get_data
*&---------------------------------------------------------------------
*
*&      Form  PF_STATUS
*&---------------------------------------------------------------------
*       Subroutine to Set PF Status
*----------------------------------------------------------------------
FORM PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.

  SET PF-STATUS 'PF_STATUS'.

ENDFORM.                    " PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  sub_event_ucomm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SUB_EVENT_UCOMM USING E_UCOMM TYPE SY-UCOMM.
  DATA:
*-Internal table for indexes of selected rows
  GI_INDEX_ROWS TYPE LVC_T_ROW ,
*-Information about 1 row
  G_SELECTED_ROW LIKE LVC_S_ROW,
  L_LINES TYPE I,
  L_CHARG LIKE MCHA-CHARG      ,
  L_FLAG(1)   TYPE C           ,
  L_FLAG1(1)  TYPE C           ,
  L_SUCC(2)   TYPE C           ,
  L_FAIL(2)   TYPE C           ,
  L_TEXT(90)  TYPE C           ,
  L_ANSWER(1) TYPE C           ,
  L_KOSTL     LIKE CSKS-KOSTL  .


  DATA: BEGIN OF L_MSG OCCURS 100 ,
          TXT(100) TYPE C         ,
        END OF L_MSG              .

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GI_INDEX_ROWS.

  DESCRIBE TABLE GI_INDEX_ROWS LINES L_LINES.
  IF L_LINES = 0.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
         EXPORTING
              TEXTLINE1 = 'You must choose a valid line'.
    EXIT.
  ELSE.

  ENDIF.

  CASE E_UCOMM.
    WHEN 'SCRP_BATCH'.

      LOOP AT GI_INDEX_ROWS INTO G_SELECTED_ROW.
        READ TABLE IT_CHARG1 INDEX G_SELECTED_ROW-INDEX.
        IF SY-SUBRC = 0.
          CLEAR: GV_CHARG,
                 L_CHARG .
          IF L_FLAG1 = SPACE.
            CLEAR L_ANSWER.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
                 EXPORTING
                      TEXTLINE1      = TEXT-006
                      TEXTLINE2      = ' '
                      TITEL          = 'Check!'
                      CANCEL_DISPLAY = ' '
                 IMPORTING
                      ANSWER         = L_ANSWER.
          ENDIF.
          IF L_ANSWER = 'J' OR L_FLAG1 = '1'.
            L_FLAG1 = '1'.
            GV_CHARG = IT_CHARG1-CHARG.

            IF NOT IT_CHARG1-USAGE_DEPT1 IS INITIAL.
              CLEAR: CSKS,
                     L_KOSTL.
              L_KOSTL = IT_CHARG1-USAGE_DEPT1.
              SELECT KOSTL UP TO 1 ROWS
                                   FROM CSKS
                                   INTO CSKS-KOSTL
                                   WHERE KOSTL = L_KOSTL.
              ENDSELECT.
              IF SY-SUBRC <> 0.
                CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
                     EXPORTING
                          TEXTLINE1 = TEXT-027.
                EXIT.
              ENDIF.
            ENDIF.
            PERFORM BUILD_OBJECT_KEY USING GV_CHARG.
            PERFORM EXTRACT_ORIGINAL_BATCH1.
            PERFORM UPDATE_ORIGINAL_BATCH.
            READ TABLE IT_NUMTAB WITH KEY CHARACT = 'P_SCRAP_DATE'.
            IF SY-SUBRC = 0.
              IF NOT IT_NUMTAB-VALUE_FROM IS INITIAL.
                DELETE IT_NUMTAB INDEX SY-TABIX.
              ENDIF.
            ENDIF.
            PERFORM BAPI_CHANGE.
            PERFORM BAPI_COMMIT.
*-Get number of success and failure records
            CLEAR L_FLAG.
            LOOP AT IT_RETTAB.
              IF IT_RETTAB-TYPE = 'E'.
                L_FLAG = 'X'.
                CONCATENATE GV_CHARG IT_RETTAB-MESSAGE INTO
                                      L_MSG SEPARATED BY ':'.
              ENDIF.
            ENDLOOP.
            IF L_FLAG = 'X'.
              L_FAIL = L_FAIL + 1.
              APPEND L_MSG.
              CLEAR  L_MSG.
            ELSE.
              L_SUCC = L_SUCC + 1.
            ENDIF.
          ELSE.
            EXIT.
          ENDIF.  " IF l_answer = 'J' OR l_flag = '1'.
        ENDIF.    " IF sy-subrc = 0.
      ENDLOOP.    " LOOP AT gi_index_rows INTO g_selected_row.

*-Status of Batches changed
      LOOP AT L_MSG.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  TEXTLINE1 = L_MSG-TXT.
      ENDLOOP.
      IF NOT L_SUCC IS INITIAL.
        CLEAR L_TEXT.
        CONCATENATE TEXT-007 L_SUCC INTO L_TEXT.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  TEXTLINE1 = L_TEXT.
      ENDIF.
      IF NOT L_FAIL IS INITIAL.
        CLEAR L_TEXT.
        CONCATENATE TEXT-008 L_FAIL INTO L_TEXT.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
             EXPORTING
                  TEXTLINE1 = L_TEXT.
      ENDIF.

*-Create Document
    WHEN 'CR_DOC'.
      LOOP AT GI_INDEX_ROWS INTO G_SELECTED_ROW.
        READ TABLE IT_CHARG1 INDEX G_SELECTED_ROW-INDEX.
        IF SY-SUBRC = 0.
          SELECT SINGLE DOKNR
                       FROM DRAW
                       INTO GV_DOKNR
                       WHERE DOKAR = GV_DOKAR         AND
                             DOKNR = IT_CHARG1-CHARG AND
                             DOKVR = '00'            AND
                             DOKTL = '000'.
          IF SY-SUBRC = 0.
            CLEAR L_TEXT.
            CONCATENATE TEXT-013 GV_DOKNR TEXT-014 INTO L_TEXT
                                            SEPARATED BY SPACE.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
                 EXPORTING
                      TEXTLINE1      = L_TEXT               " text-004
                      TEXTLINE2      = TEXT-015
                      TITEL          = 'Check!'
                      CANCEL_DISPLAY = ' '
                 IMPORTING
                      ANSWER         = L_ANSWER.
            IF L_ANSWER = 'J'.
              SET PARAMETER ID 'CV1' FIELD IT_CHARG1-CHARG.
              SET PARAMETER ID 'CV2' FIELD GV_DOKAR.
              SET PARAMETER ID 'CV3' FIELD '00' .
              SET PARAMETER ID 'CV4' FIELD '000'.
              CALL TRANSACTION 'CV02N' AND SKIP FIRST SCREEN.
              CLEAR L_CHARG.
              SELECT SINGLE CHARG
                       FROM MCHA
                       INTO L_CHARG
                       WHERE MATNR = C_MATNR AND
                             WERKS = C_WERKS AND
                             CHARG = GV_CHARG.
              IF NOT L_CHARG IS INITIAL.
                PERFORM BUILD_OBJECT_KEY USING GV_CHARG.
                PERFORM EXTRACT_ORIGINAL_BATCH1.
                PERFORM UPDATE_ORIGINAL_BATCH1.
                PERFORM BAPI_CHANGE.
                PERFORM BAPI_COMMIT.
              ENDIF.
            ENDIF.
          ELSE.
            SET PARAMETER ID 'CV1' FIELD IT_CHARG1-CHARG.
            SET PARAMETER ID 'CV2' FIELD GV_DOKAR.
            SET PARAMETER ID 'CV3' FIELD '00' .
            SET PARAMETER ID 'CV4' FIELD '000'.
            CALL TRANSACTION 'CV01N' AND SKIP FIRST SCREEN.
            CLEAR L_CHARG.
            SELECT SINGLE CHARG
                     FROM MCHA
                     INTO L_CHARG
                     WHERE MATNR = C_MATNR AND
                           WERKS = C_WERKS AND
                           CHARG = GV_CHARG.
            IF NOT L_CHARG IS INITIAL.
              PERFORM BUILD_OBJECT_KEY USING GV_CHARG.
              PERFORM EXTRACT_ORIGINAL_BATCH1.
              PERFORM UPDATE_ORIGINAL_BATCH1.
              PERFORM BAPI_CHANGE.
              PERFORM BAPI_COMMIT.
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE I000 WITH TEXT-003.
        ENDIF.
      ENDLOOP.  " LOOP AT gi_index_rows INTO g_selected_row.

  ENDCASE.
ENDFORM.                    " sub_event_ucomm
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'STATUS_9000'. " EXCLUDING lt_excl.
  SET TITLEBAR  'TITLE_9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*       Subroutine to create custom container control & instance
*----------------------------------------------------------------------*
MODULE CREATE_ALV_OBJECT OUTPUT.

  DATA: WA_RENEWAL_FLG(1) TYPE C.
  IF GRID_CONTAINER IS INITIAL. " Not Created Container for ALV GRID
    PERFORM CREATE_OBJECT.
    PERFORM SET_LAYOUT.
    PERFORM ALV_FIELDCAT.
    PERFORM DISPLAY_ALV.

  ELSEIF NOT GRID_CONTAINER IS INITIAL AND
             WA_RENEWAL_FLG = 'X'.

    CLEAR WA_RENEWAL_FLG.
*-Set layout
    PERFORM SET_LAYOUT.

*-Create field catalog
    PERFORM ALV_FIELDCAT.

*-Display data on ALV GRID Control using method
    PERFORM SET_NEW_TABLE_DATA.
    PERFORM REFRESH_ALV_GRID_DATA_DISP.

    CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                        EXPORTING CONTROL = ALV_GRID.

  ENDIF.

ENDMODULE.                 " create_alv_object  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_object
*&---------------------------------------------------------------------*
*       Subroutine to create ALV Object
*----------------------------------------------------------------------*
FORM CREATE_OBJECT.
*-Create object
  CREATE OBJECT GRID_CONTAINER
      EXPORTING
          CONTAINER_NAME = CUSTOM_CONTROL.

* Create an Instance of ALV Control
  CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = GRID_CONTAINER.

ENDFORM.                    " create_object
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       Set Layout
*----------------------------------------------------------------------*
FORM SET_LAYOUT.
  CLEAR GS_LAYOUT.
  GS_LAYOUT-LANGUAGE   = SY-LANGU.      " Language Key
  GS_LAYOUT-CWIDTH_OPT = 'X'.
  GS_LAYOUT-SEL_MODE   = 'A'.
  GS_LAYOUT-BOX_FNAME  = 'CHKBOX' .
  GS_LAYOUT-EDIT_MODE  = 'X'.

ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcat
*&---------------------------------------------------------------------*
*       Prepare ALV fieldcatalogue
*----------------------------------------------------------------------*
FORM ALV_FIELDCAT.
  DATA: L_POS TYPE I.
  CLEAR:  GS_FIELDCAT,
          GT_FIELDCAT.
  REFRESH GT_FIELDCAT.

  DEFINE APPEND_FIELDCAT.
    L_POS = L_POS + 1.
    CLEAR GS_FIELDCAT.
    GS_FIELDCAT-COL_POS       = L_POS.
    GS_FIELDCAT-KEY           = &1.
    GS_FIELDCAT-FIELDNAME     = &2.
    GS_FIELDCAT-SCRTEXT_M     = &3.        " Column heading
    GS_FIELDCAT-OUTPUTLEN     = &4.        " Column width
    GS_FIELDCAT-DATATYPE      = &5.        " Data type
    GS_FIELDCAT-EDIT          = &6.
    GS_FIELDCAT-FIX_COLUMN    = &7.
    APPEND GS_FIELDCAT TO GT_FIELDCAT.
  END-OF-DEFINITION.

  APPEND_FIELDCAT :

     ' '  'CHARG'          'Body Number'       10 'CHAR' ' ' 'X',
     ' '  'VIN_NUM'        'VIN Number'        18 'CHAR' ' ' 'X',
     ' '  'FSC'            'Full Spec Code'    20 'CHAR' ' ' ' ',
     ' '  'EXT_CLR'        'Exterior Color'    14 'CHAR' ' ' ' ',
     ' '  'INT_CLR'        'Interior Color'    14 'CHAR' ' ' ' ',
     ' '  'ASK_DEPT'       'Asking Dept.'      12 'CHAR' ' ' ' ',
     ' '  'USAGE_DEPT'     'Destination/Using Dept' 22 'CHAR' ' ' ' ',
     ' '  'USAGE_TEXT'     'Usage Text'        30 'CHAR' ' ' ' ',
     ' '  'USAGE_DEPT1'    'Using Dept'        10 'CHAR' 'X' ' ',
     ' '  'SCRAP_DATE'     'Scrap Date'        10 'CHAR' 'X' ' ',
     ' '  'SCRAP_TAG_NO'   'Scrap Tag #'       12 'CHAR' 'X' ' ',
     ' '  'SCRAP_RES'      'Scrap Reversed'    12 'CHAR' 'X' ' '.

  LOOP AT GT_FIELDCAT.
    IF GT_FIELDCAT-FIELDNAME = 'USAGE_DEPT1'.
      GT_FIELDCAT-F4AVAILABL = 'X'.
      GT_FIELDCAT-REF_FIELD = 'KOSTL'.
      GT_FIELDCAT-REF_TABLE = 'CSKS'.
      MODIFY GT_FIELDCAT INDEX SY-TABIX TRANSPORTING F4AVAILABL
                                         REF_FIELD REF_TABLE.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " alv_fieldcat
*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
*       Subroutine to display ALV Grid data
*----------------------------------------------------------------------*
FORM DISPLAY_ALV.
  DATA: MT_EXCLUDING_TOOLBAR  TYPE UI_FUNCTIONS,
        L_FUNC TYPE UI_FUNC.
  REFRESH MT_EXCLUDING_TOOLBAR[].
  APPEND '&ABC'   TO MT_EXCLUDING_TOOLBAR.
  APPEND '&INFO'  TO MT_EXCLUDING_TOOLBAR.
  APPEND '&UMC'   TO MT_EXCLUDING_TOOLBAR.
  APPEND '&SUM'   TO MT_EXCLUDING_TOOLBAR.
  APPEND '&GRAPH' TO MT_EXCLUDING_TOOLBAR.
  APPEND '&ILD' TO MT_EXCLUDING_TOOLBAR.
  APPEND '&LOCAL&APPEND' TO MT_EXCLUDING_TOOLBAR.
  APPEND '&LOCAL&COPY' TO MT_EXCLUDING_TOOLBAR.
  APPEND '&LOCAL&COPY_ROW' TO MT_EXCLUDING_TOOLBAR.
  APPEND '&LOCAL&CUT' TO MT_EXCLUDING_TOOLBAR.
  APPEND '&LOCAL&DELETE_ROW' TO MT_EXCLUDING_TOOLBAR.
  APPEND '&LOCAL&INSERT_ROW' TO MT_EXCLUDING_TOOLBAR.

  SORT IT_CHARG1 BY CHARG.
*-- Display data on ALV GRID Control
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
       EXPORTING I_STRUCTURE_NAME = 'IT_CHARG1'
                 IS_LAYOUT        = GS_LAYOUT
                 I_SAVE           = GV_SAVE
                 IS_VARIANT       = GV_VARIANT
                 I_DEFAULT        = 'X'
                 IT_TOOLBAR_EXCLUDING  = MT_EXCLUDING_TOOLBAR
       CHANGING  IT_FIELDCATALOG  = GT_FIELDCAT[]
                 IT_OUTTAB        = IT_CHARG1[].
*            IT_SORT               = <internal table of type LVC_T_SORT>
*            IT_FILTER             = <internal table of type LVC_T_FILT>

*-Create Object to receive events and link them to handler methods.
*-When ALV Control raises the event for the specified instance,
*-corresponding method is automatically called.
  CREATE OBJECT EVENT_RECEIVER.

  SET HANDLER EVENT_RECEIVER->HANDLE_TOOLBAR       FOR ALV_GRID.
  SET HANDLER EVENT_RECEIVER->HANDLE_USER_COMMAND  FOR ALV_GRID.

*- Call method 'set_toolbar_interactive' to raise event TOOLBAR.
  CALL METHOD ALV_GRID->SET_TOOLBAR_INTERACTIVE.

  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
                      EXPORTING CONTROL = ALV_GRID.

ENDFORM.                    " display_alv
*&---------------------------------------------------------------------*
*&      Form  SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM SET_NEW_TABLE_DATA.

ENDFORM.                    " SET_NEW_TABLE_DATA
*&---------------------------------------------------------------------*
*&      Form  refresh_alv_grid_data_disp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REFRESH_ALV_GRID_DATA_DISP.

ENDFORM.                    " refresh_alv_grid_data_disp
*&---------------------------------------------------------------------*
*&      Module  exit  INPUT
*&---------------------------------------------------------------------*
*       Module to exit from screen
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.

  CASE OK_CODE.
    WHEN 'EXIT'.

      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'RW'.

      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " exit  INPUT
*&---------------------------------------------------------------------*
*&      Form  free_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FREE_ALV_GRID.
  CALL METHOD ALV_GRID->FREE.
  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    GV_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = GV_REPID
              TXT2  = SY-SUBRC
              TXT1  = TEXT-002.
  ENDIF.

ENDFORM.                    " free_alv_grid
*&---------------------------------------------------------------------*
*&      Module  user_command_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  OK_CODE = SY-UCOMM.
  CLEAR SY-UCOMM.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'RW'.

      PERFORM FREE_ALV_GRID.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " user_command_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  create_batch
*&---------------------------------------------------------------------*
*       Subroutine to create batch
*----------------------------------------------------------------------*
FORM CREATE_BATCH.
  CLEAR:   IT_RETTAB  .
  REFRESH: IT_RETTAB  .
  GV_CHARG = IT_CHARG1-CHARG.
*-Create the batch using screen values selected by the user
  CALL FUNCTION 'BAPI_BATCH_CREATE'
       EXPORTING
            MATERIAL             = C_MATNR
            BATCH                = GV_CHARG
            PLANT                = C_WERKS
            BATCHSTORAGELOCATION = SPACE
       TABLES
            RETURN               = IT_RETTAB.

ENDFORM.                    " create_batch
*&---------------------------------------------------------------------*
*&      Form  bapi_commit
*&---------------------------------------------------------------------*
*       Subroutine to Commit BAPI changes
*----------------------------------------------------------------------*
FORM BAPI_COMMIT.
*-Commit the changes
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
ENDFORM.                    " bapi_commit
*&---------------------------------------------------------------------*
*&      Form  update_original_batch
*&---------------------------------------------------------------------*
*       Sub. to update newly created Batch with char name and values
*----------------------------------------------------------------------*
FORM UPDATE_ORIGINAL_BATCH.
  DATA: L_DATE      LIKE SY-DATUM,
        L_DATE1(10) TYPE C       ,
        L_DKTXT LIKE DRAT-DKTXT  ,
        L_DKTXT1(15) TYPE C      .
  READ TABLE IT_CABN WITH KEY ATNAM = 'P_VIN'.      " VIN Number
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM
                                               IT_CHARG1-VIN_NUM.

  READ TABLE IT_CABN WITH KEY ATNAM = 'P_FSC'.  " Full Spec code
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM IT_CHARG1-FSC.

  READ TABLE IT_CABN WITH KEY ATNAM = 'P_EXT_COLOR'.  " Exterior Color
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM
                                                      IT_CHARG1-EXT_CLR.

  READ TABLE IT_CABN WITH KEY ATNAM = 'P_INT_COLOR'.  " Interior Color
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM
                                                      IT_CHARG1-INT_CLR.

  READ TABLE IT_CABN WITH KEY ATNAM = 'P_USAGE_DEPT'.  " Usage Dept
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM
                                                  IT_CHARG1-USAGE_DEPT1.

  READ TABLE IT_CABN WITH KEY ATNAM = 'P_SCRAP_APP_DOC'.  " Scrap Doc No
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM
                                                 IT_CHARG1-SCRAP_TAG_NO.

  READ TABLE IT_CABN WITH KEY ATNAM = 'P_REF_DOC'.  " Reference Document
  IF SY-SUBRC = 0.
    SELECT SINGLE DKTXT FROM DRAT
                  INTO L_DKTXT
                       WHERE DOKAR = GV_DOKAR AND
                             DOKNR = GV_CHARG AND
                             DOKVR = '00'     AND
                             DOKTL = '000'    AND
                             LANGU = SY-LANGU.
    IF NOT L_DKTXT IS INITIAL.
      L_DKTXT1 = L_DKTXT.
      PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM L_DKTXT1.
    ENDIF.
  ENDIF.

*  l_date = sy-datum.
*  PERFORM user_specific_date USING  l_date
*                           CHANGING l_date1.
  READ TABLE IT_CABN WITH KEY ATNAM = 'P_SCRAP_DATE'.  " Scrap Date
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM
                                              IT_CHARG1-SCRAP_DATE.

** Changed by Furong on 02/22/08  "UD1K942932  READ TABLE it_cabn WITH
  READ TABLE IT_CABN WITH KEY ATNAM = 'P_SCRAP_REV'.  " Scrap Reserve
  PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM
                                              IT_CHARG1-SCRAP_RES.
** End of change
ENDFORM.                    " update_original_batch
*&---------------------------------------------------------------------*
*&      Form  get_characteristics
*&---------------------------------------------------------------------*
*       Subroutine to get characterstic names
*----------------------------------------------------------------------*
FORM GET_CHARACTERISTICS.
*-Extract the characteristic names for KLART/CLASS
  CLEAR: KLAH, IT_CABN.
  REFRESH IT_CABN.

  SELECT SINGLE CLINT FROM KLAH
                INTO KLAH-CLINT
                WHERE KLART = C_KLART AND
                      CLASS = C_CLASS.

  IF SYST-SUBRC = 0.
    SELECT IMERK FROM KSML
                  INTO TABLE IT_KSML
                  WHERE CLINT = KLAH-CLINT.
    IF SY-SUBRC = 0.
      SELECT ATINN ATNAM ATFOR
                   FROM CABN
                   INTO TABLE IT_CABN
                   FOR ALL ENTRIES IN IT_KSML
                   WHERE ATINN = IT_KSML-IMERK.
      IF SY-SUBRC = 0.
        LOOP AT IT_CABN.
          IF IT_CABN-ATNAM = 'P_VIN'.               " VIN Number
            GV_VIN = IT_CABN-ATINN.
          ELSEIF IT_CABN-ATNAM = 'P_FSC'.           " Full Spec code
            GV_FSC = IT_CABN-ATINN.
          ELSEIF IT_CABN-ATNAM = 'P_ASK_DEPT'.      " Asking Dept
            GV_ASKDEPT = IT_CABN-ATINN.
          ELSEIF IT_CABN-ATNAM = 'P_USAGE_DEPT'.    " Dest/Using Dept
            GV_USAGEDEP = IT_CABN-ATINN.
          ELSEIF IT_CABN-ATNAM = 'P_GL_ACCOUNT'.    " GL Account
            GV_GLNO = IT_CABN-ATINN.
          ELSEIF IT_CABN-ATNAM = 'P_SCRAP_REV'.    " Scrap Reserve
            GV_SCRAP_RES = IT_CABN-ATINN.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " get_characteristics
*&---------------------------------------------------------------------*
*&      Form  update_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_CABN_ATFOR  text
*      -->P_IT_CABN_ATNAM  text
*      -->P_1727   text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE USING ATFOR ATNAM VALUE.
*-Depending on data format, start building the characteristics table
*-ready for update
  CASE ATFOR.
    WHEN 'NUM'.
      CLEAR IT_NUMTAB.
      READ TABLE IT_NUMTAB WITH KEY CHARACT = ATNAM.
      IF SYST-SUBRC = 0.
        IT_NUMTAB-VALUE_FROM = VALUE.
        MODIFY IT_NUMTAB INDEX SYST-TABIX.
      ELSE.
        IT_NUMTAB-CHARACT = ATNAM.
        IT_NUMTAB-VALUE_FROM = VALUE.
        APPEND IT_NUMTAB.
      ENDIF.
    WHEN 'CURR'.
** Changed by Furong on 02/22/08  "UD1K942932
      CLEAR IT_CURTAB.
      READ TABLE IT_CURTAB WITH KEY CHARACT = ATNAM.
      IF SYST-SUBRC = 0.
        IT_CURTAB-VALUE_FROM = VALUE.
        MODIFY IT_CURTAB INDEX SYST-TABIX.
      ELSE.
        IT_CURTAB-CHARACT = ATNAM.
        IT_CURTAB-VALUE_FROM = VALUE.
        APPEND IT_CURTAB.
      ENDIF.
** End of change
    WHEN 'CHAR' OR 'DATE'.
      CLEAR IT_CHATAB.
      READ TABLE IT_CHATAB WITH KEY CHARACT = ATNAM.
      IF SYST-SUBRC = 0.
        IT_CHATAB-VALUE_NEUTRAL = VALUE.
        MODIFY IT_CHATAB INDEX SYST-TABIX.
      ELSE.
        IT_CHATAB-CHARACT = ATNAM.
        IT_CHATAB-VALUE_NEUTRAL = VALUE.
        APPEND IT_CHATAB.
      ENDIF.
  ENDCASE.

ENDFORM.                    " update_table
*&---------------------------------------------------------------------*
*&      Form  bapi_change
*&---------------------------------------------------------------------*
*       Subroutine to update Batch with characteristic values
*----------------------------------------------------------------------*
FORM BAPI_CHANGE.

* Apply the characteristics to the batch.
  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
       EXPORTING
            OBJECTKEY          = GV_OBJECT
            OBJECTTABLE        = 'MCHA'
            CLASSNUM           = C_CLASS
            CLASSTYPE          = C_KLART
       TABLES
            ALLOCVALUESNUMNEW  = IT_NUMTAB
            ALLOCVALUESCHARNEW = IT_CHATAB
            ALLOCVALUESCURRNEW = IT_CURTAB
            RETURN             = IT_RETTAB.

ENDFORM.                    " bapi_change
*&---------------------------------------------------------------------*
*&      Form  extract_original_batch
*&---------------------------------------------------------------------*
*       Subroutine to get Characteristic values for batches
*----------------------------------------------------------------------*
FORM EXTRACT_ORIGINAL_BATCH.
*-Extract the original characteristic data if exists
  CLEAR:   IT_NUMTAB, IT_CHATAB, IT_CURTAB, IT_RETTAB.
  REFRESH: IT_NUMTAB, IT_CHATAB, IT_CURTAB, IT_RETTAB.

  CALL FUNCTION 'VB_BATCH_GET_DETAIL'
    EXPORTING
      MATNR                    = C_MATNR
      CHARG                    = IT_CHARG-CHARG
      WERKS                    = C_WERKS
      GET_CLASSIFICATION       = 'X'
*   EXISTENCE_CHECK          =
*   READ_FROM_BUFFER         =
*   NO_CLASS_INIT            = ' '
*   LOCK_BATCH               = ' '
* IMPORTING
*   YMCHA                    =
*   CLASSNAME                =
   TABLES
     CHAR_OF_BATCH            = IT_CLBATCH
   EXCEPTIONS
     NO_MATERIAL              = 1
     NO_BATCH                 = 2
     NO_PLANT                 = 3
     MATERIAL_NOT_FOUND       = 4
     PLANT_NOT_FOUND          = 5
     NO_AUTHORITY             = 6
     BATCH_NOT_EXIST          = 7
     LOCK_ON_BATCH            = 8
     OTHERS                   = 9.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " extract_original_batch
*&---------------------------------------------------------------------*
*&      Form  fill_intern_table
*&---------------------------------------------------------------------*
*       Populate internal table with batch and char values
*----------------------------------------------------------------------*
FORM FILL_INTERN_TABLE.
  DATA: L_DATE    LIKE SY-DATUM,
        L_GL_ACCT TYPE ZE_GLACT,
        L_RP18_SHOP_DATE(10) TYPE C,
        L_SHIPOUT_DATE(10)   TYPE C,
        L_SHIP_DATE(10)      TYPE C.

  IT_CHARG1-CHARG = IT_CHARG-CHARG.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_VIN'.
  IF SY-SUBRC = 0.
    IT_CHARG1-VIN_NUM = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-VIN_NUM = SPACE.
  ENDIF.

*-Validations for VIN Number
  IF S_VIN-LOW IS INITIAL AND S_VIN-HIGH IS INITIAL.
  ELSEIF NOT ( S_VIN-LOW IS INITIAL ) AND S_VIN-HIGH IS INITIAL.
    READ TABLE IT_VIN WITH KEY VIN = IT_CHARG1-VIN_NUM BINARY SEARCH.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ELSEIF S_VIN-LOW IS INITIAL AND NOT ( S_VIN-HIGH IS INITIAL ).
    IF IT_CHARG1-VIN_NUM > S_VIN-HIGH.
      EXIT.
    ENDIF.
  ELSEIF NOT ( S_VIN-LOW IS INITIAL AND S_VIN-HIGH IS INITIAL ).
 IF ( IT_CHARG1-VIN_NUM < S_VIN-LOW OR IT_CHARG1-VIN_NUM > S_VIN-HIGH ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_FSC'.
  IF SY-SUBRC = 0.
    IT_CHARG1-FSC = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-FSC = SPACE.
  ENDIF.

*-Validations for Full spec code
  IF S_FSC-LOW IS INITIAL AND S_FSC-HIGH IS INITIAL.
  ELSEIF NOT ( S_FSC-LOW IS INITIAL ) AND S_FSC-HIGH IS INITIAL.
    READ TABLE IT_FSC WITH KEY FSC = IT_CHARG1-FSC BINARY SEARCH.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ELSEIF S_FSC-LOW IS INITIAL AND NOT ( S_FSC-HIGH IS INITIAL ).
    IF IT_CHARG1-FSC > S_FSC-HIGH.
      EXIT.
    ENDIF.
  ELSEIF NOT ( S_FSC-LOW IS INITIAL AND S_FSC-HIGH IS INITIAL ).
    IF ( IT_CHARG1-FSC < S_FSC-LOW OR IT_CHARG1-FSC > S_FSC-HIGH ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_RP18_SHOP_DATE'.
  IF SY-SUBRC = 0.
    L_RP18_SHOP_DATE = IT_CLBATCH-ATWTB.
  ENDIF.

*-Validations for Shop Date
  CLEAR L_DATE.
  IF NOT L_RP18_SHOP_DATE IS INITIAL.
    PERFORM USER_SPECIFIC_DATE1 USING  L_RP18_SHOP_DATE
                                      CHANGING L_DATE.
  ENDIF.
  IF S_SHOPDT-LOW IS INITIAL AND S_SHOPDT-HIGH IS INITIAL.
  ELSEIF NOT ( S_SHOPDT-LOW IS INITIAL ) AND S_SHOPDT-HIGH IS INITIAL.
    READ TABLE IT_SHOP_DT WITH KEY DATE = L_DATE BINARY SEARCH.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ELSEIF S_SHOPDT-LOW IS INITIAL AND NOT ( S_SHOPDT-HIGH IS INITIAL ).
    IF L_DATE > S_SHOPDT-HIGH.
      EXIT.
    ENDIF.
  ELSEIF NOT ( S_SHOPDT-LOW IS INITIAL AND S_SHOPDT-HIGH IS INITIAL ).
    IF ( L_DATE < S_SHOPDT-LOW OR L_DATE > S_SHOPDT-HIGH ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_SHIPOUT_DATE'.
  IF SY-SUBRC = 0.
    L_SHIPOUT_DATE = IT_CLBATCH-ATWTB.
  ENDIF.

*-Validations for Ship out Date
  CLEAR L_DATE.
  IF NOT L_SHIPOUT_DATE IS INITIAL.
    PERFORM USER_SPECIFIC_DATE1 USING  L_SHIPOUT_DATE
                               CHANGING L_DATE.
  ENDIF.
  IF S_SHIPDT-LOW IS INITIAL AND S_SHIPDT-HIGH IS INITIAL.
  ELSEIF NOT ( S_SHIPDT-LOW IS INITIAL ) AND S_SHIPDT-HIGH IS INITIAL.
    READ TABLE IT_SHIP_DT WITH KEY DATE = L_DATE BINARY SEARCH.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ELSEIF S_SHIPDT-LOW IS INITIAL AND NOT ( S_SHIPDT-HIGH IS INITIAL ).
    IF L_DATE > S_SHIPDT-HIGH.
      EXIT.
    ENDIF.
  ELSEIF NOT ( S_SHIPDT-LOW IS INITIAL AND S_SHIPDT-HIGH IS INITIAL ).
    IF ( L_DATE < S_SHIPDT-LOW OR L_DATE > S_SHIPDT-HIGH ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_ASK_DEPT'.
  IF SY-SUBRC = 0.
    IT_CHARG1-ASK_DEPT = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-ASK_DEPT = SPACE.
  ENDIF.

*-Validations for Asking Department
  IF S_ASKDEP-LOW IS INITIAL AND S_ASKDEP-HIGH IS INITIAL.
  ELSEIF NOT ( S_ASKDEP-LOW IS INITIAL ) AND S_ASKDEP-HIGH IS INITIAL.
    READ TABLE IT_ASKDEP WITH KEY ASKDEPT = IT_CHARG1-ASK_DEPT
                                                 BINARY SEARCH.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ELSEIF S_ASKDEP-LOW IS INITIAL AND NOT ( S_ASKDEP-HIGH IS INITIAL ).
    IF IT_CHARG1-ASK_DEPT > S_ASKDEP-HIGH.
      EXIT.
    ENDIF.
  ELSEIF NOT ( S_ASKDEP-LOW IS INITIAL AND S_ASKDEP-HIGH IS INITIAL ).
    IF ( IT_CHARG1-ASK_DEPT < S_ASKDEP-LOW OR
                                  IT_CHARG1-ASK_DEPT > S_ASKDEP-HIGH ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_USAGE_DEPT'.
  IF SY-SUBRC = 0.
    IT_CHARG1-USAGE_DEPT = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-USAGE_DEPT = SPACE.
  ENDIF.

*-Validations for Destination/Using Department
  IF S_UDEPT-LOW IS INITIAL AND S_UDEPT-HIGH IS INITIAL.
  ELSEIF NOT ( S_UDEPT-LOW IS INITIAL ) AND S_UDEPT-HIGH IS INITIAL.
    READ TABLE IT_USAGEDEP WITH KEY USAGE_DEPT = IT_CHARG1-USAGE_DEPT
                                             BINARY SEARCH.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ELSEIF S_UDEPT-LOW IS INITIAL AND NOT ( S_UDEPT-HIGH IS INITIAL ).
    IF IT_CHARG1-USAGE_DEPT > S_UDEPT-HIGH.
      EXIT.
    ENDIF.
  ELSEIF NOT ( S_UDEPT-LOW IS INITIAL AND S_UDEPT-HIGH IS INITIAL ).
    IF ( IT_CHARG1-USAGE_DEPT < S_UDEPT-LOW OR
                                  IT_CHARG1-USAGE_DEPT > S_UDEPT-HIGH ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_GL_ACCOUNT'.
  IF SY-SUBRC = 0.
    L_GL_ACCT = IT_CLBATCH-ATWTB.
  ENDIF.

*-Validations for G/L Account
  IF S_GLNO-LOW IS INITIAL AND S_GLNO-HIGH IS INITIAL.
  ELSEIF NOT ( S_GLNO-LOW IS INITIAL ) AND S_GLNO-HIGH IS INITIAL.
    READ TABLE IT_GLNO WITH KEY GLNO = L_GL_ACCT BINARY SEARCH.
    IF SY-SUBRC <> 0.
      EXIT.
    ENDIF.
  ELSEIF S_GLNO-LOW IS INITIAL AND NOT ( S_GLNO-HIGH IS INITIAL ).
    IF L_GL_ACCT > S_GLNO-HIGH.
      EXIT.
    ENDIF.
  ELSEIF NOT ( S_GLNO-LOW IS INITIAL AND S_GLNO-HIGH IS INITIAL ).
    IF ( L_GL_ACCT < S_GLNO-LOW OR L_GL_ACCT > S_GLNO-HIGH ).
      EXIT.
    ENDIF.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_EXT_COLOR'.
  IF SY-SUBRC = 0.
    IT_CHARG1-EXT_CLR = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-EXT_CLR = SPACE.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_INT_COLOR'.
  IF SY-SUBRC = 0.
    IT_CHARG1-INT_CLR = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-INT_CLR = SPACE.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_USAGE_TEXT'.
  IF SY-SUBRC = 0.
    IT_CHARG1-USAGE_TEXT = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-USAGE_TEXT = SPACE.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_SCRAP_DATE'.
  IF SY-SUBRC = 0.
    IT_CHARG1-SCRAP_DATE = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-SCRAP_DATE = SPACE.
  ENDIF.

  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_SCRAP_APP_DOC'.
  IF SY-SUBRC = 0.
    IT_CHARG1-SCRAP_TAG_NO = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-SCRAP_TAG_NO = SPACE.
  ENDIF.
** Changed by Furong on 02/22/08  "UD1K942932
  READ TABLE IT_CLBATCH WITH KEY ATNAM = 'P_SCRAP_REV'.
  IF SY-SUBRC = 0.
    IT_CHARG1-SCRAP_RES = IT_CLBATCH-ATWTB.
  ELSE.
    IT_CHARG1-SCRAP_RES = SPACE.
  ENDIF.
** End of change ON 02/22/08
  APPEND IT_CHARG1.
  CLEAR  IT_CHARG1.
ENDFORM.                    " fill_intern_table
*&---------------------------------------------------------------------*
*&      Form  build_object_key
*&---------------------------------------------------------------------*
*       Subroutine to build Object Key
*----------------------------------------------------------------------*
*      -->P_CHARG  Batch Number
*----------------------------------------------------------------------*
FORM BUILD_OBJECT_KEY USING P_CHARG TYPE MCHA-CHARG.
*-build the object key
  CLEAR:   IT_OBJECT, IT_RETTAB, GV_OBJECT.
  REFRESH: IT_OBJECT, IT_RETTAB.

  IT_OBJECT-KEY_FIELD = 'MATNR'.
  IT_OBJECT-VALUE_INT = C_MATNR.
  APPEND IT_OBJECT.

  IT_OBJECT-KEY_FIELD = 'WERKS'.
  IT_OBJECT-VALUE_INT = C_WERKS.
  APPEND IT_OBJECT.

  IT_OBJECT-KEY_FIELD = 'CHARG'.
  IT_OBJECT-VALUE_INT = P_CHARG.
  APPEND IT_OBJECT.

  CALL FUNCTION 'BAPI_OBJCL_CONCATENATEKEY'
       EXPORTING
            OBJECTTABLE    = 'MCHA'
       IMPORTING
            OBJECTKEY_CONC = GV_OBJECT
       TABLES
            OBJECTKEYTABLE = IT_OBJECT
            RETURN         = IT_RETTAB.

ENDFORM.                    " build_object_key
*&---------------------------------------------------------------------*
*&      Form  change_screen_runtime
*&---------------------------------------------------------------------*
*       Dynamic screen display
*----------------------------------------------------------------------*
FORM CHANGE_SCREEN_RUNTIME.
  LOOP AT SCREEN.
    IF P_BODY = 'X'.
*-Make VIN Number invisible
      IF SCREEN-GROUP1 = '128'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
*-Make Body Number invisible
    IF P_VIN = 'X'.
      IF SCREEN-GROUP1 = '127'.
        SCREEN-INPUT = '0'.
        SCREEN-INVISIBLE = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " change_screen_runtime
*&---------------------------------------------------------------------*
*&      Form  user_specific_date
*&---------------------------------------------------------------------*
*       Subroutine to get user specific date format
*----------------------------------------------------------------------*
*      -->P_DATA     Date in generic form
*      <--P_EXTDATE  Date in User specific date format
*----------------------------------------------------------------------*
FORM USER_SPECIFIC_DATE USING    P_PDATE
                        CHANGING P_EXTDATE.
  DATA: W_DATFM(1) TYPE C.
  CALL FUNCTION 'ITS_GET_USER_DEFAULTS'
       EXPORTING
            BNAME = SY-UNAME
       IMPORTING
            DATFM = W_DATFM.

  CASE W_DATFM.
    WHEN 1.
      CONCATENATE P_PDATE+6(2) P_PDATE+4(2) P_PDATE+0(4) INTO P_EXTDATE
      SEPARATED BY '.'.

    WHEN 2.
      CONCATENATE P_PDATE+4(2) P_PDATE+6(2) P_PDATE+0(4) INTO P_EXTDATE
      SEPARATED BY '/'.

    WHEN 3.
      CONCATENATE P_PDATE+4(2) P_PDATE+6(2) P_PDATE+0(4) INTO P_EXTDATE
      SEPARATED BY '-'.

    WHEN 4.
      CONCATENATE P_PDATE+0(4) P_PDATE+4(2) P_PDATE+6(2) INTO P_EXTDATE
      SEPARATED BY '.'.

    WHEN 5.
      CONCATENATE P_PDATE+0(4) P_PDATE+4(2) P_PDATE+6(2) INTO P_EXTDATE
      SEPARATED BY '/'.

    WHEN 6.
      CONCATENATE P_PDATE+0(4) P_PDATE+4(2) P_PDATE+6(2) INTO P_EXTDATE
      SEPARATED BY '-'.

  ENDCASE.
ENDFORM.                    " user_specific_date
*&---------------------------------------------------------------------*
*&      Form  extract_original_batch1
*&---------------------------------------------------------------------*
*       Subroutine to get Characteristic values for batches
*----------------------------------------------------------------------*
FORM EXTRACT_ORIGINAL_BATCH1.

*-Extract the original characteristic data if exists
  CLEAR:   IT_NUMTAB, IT_CHATAB, IT_CURTAB, IT_RETTAB.
  REFRESH: IT_NUMTAB, IT_CHATAB, IT_CURTAB, IT_RETTAB.

  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
       EXPORTING
            OBJECTKEY       = GV_OBJECT
            OBJECTTABLE     = 'MCHA'
            CLASSNUM        = C_CLASS
            CLASSTYPE       = C_KLART
       TABLES
            ALLOCVALUESNUM  = IT_NUMTAB
            ALLOCVALUESCHAR = IT_CHATAB
            ALLOCVALUESCURR = IT_CURTAB
            RETURN          = IT_RETTAB.
ENDFORM.                    " extract_original_batch1
*&---------------------------------------------------------------------*
*&      Form  update_original_batch1
*&---------------------------------------------------------------------*
*       Sub. to update newly created Batch with char name and values
*----------------------------------------------------------------------*
FORM UPDATE_ORIGINAL_BATCH1.
  DATA: L_DKTXT LIKE DRAT-DKTXT,
        L_DKTXT1(15) TYPE C.
  READ TABLE IT_CABN WITH KEY ATNAM = 'P_REF_DOC'.  " Reference Document
  IF SY-SUBRC = 0.
    SELECT SINGLE DKTXT FROM DRAT
                  INTO L_DKTXT
                       WHERE DOKAR = GV_DOKAR AND
                             DOKNR = IT_CHARG1-CHARG AND
                             DOKVR = '00'     AND
                             DOKTL = '000'    AND
                             LANGU = SY-LANGU.
    IF NOT L_DKTXT IS INITIAL.
      L_DKTXT1 = L_DKTXT.
      PERFORM UPDATE_TABLE USING IT_CABN-ATFOR IT_CABN-ATNAM L_DKTXT1.
    ENDIF.
  ENDIF.
ENDFORM.                    " update_original_batch1
*&---------------------------------------------------------------------*
*&      Form  user_specific_date1
*&---------------------------------------------------------------------*
*       Subroutine to get user specific date to internal format
*----------------------------------------------------------------------*
*      -->P_DATA     Date in generic form
*      <--P_EXTDATE  Date in User specific date format
*----------------------------------------------------------------------*
FORM USER_SPECIFIC_DATE1 USING    P_PDATE
                         CHANGING P_EXTDATE.
  DATA: W_DATFM(1) TYPE C.
  CALL FUNCTION 'ITS_GET_USER_DEFAULTS'
       EXPORTING
            BNAME = SY-UNAME
       IMPORTING
            DATFM = W_DATFM.

  CASE W_DATFM.
    WHEN 1.
      CONCATENATE P_PDATE+6(4) P_PDATE+3(2) P_PDATE(2) P_PDATE+0(4)
                                                        INTO P_EXTDATE.
    WHEN 2.
      CONCATENATE P_PDATE+6(4) P_PDATE(2) P_PDATE+3(2) P_PDATE+0(4)
                                                        INTO P_EXTDATE.
    WHEN 3.
      CONCATENATE P_PDATE+6(4) P_PDATE(2) P_PDATE+3(2) P_PDATE+0(4)
                                                        INTO P_EXTDATE.
    WHEN 4.
      CONCATENATE P_PDATE+0(4) P_PDATE+5(2) P_PDATE+7(2) INTO P_EXTDATE.

    WHEN 5.
      CONCATENATE P_PDATE+0(4) P_PDATE+5(2) P_PDATE+7(2) INTO P_EXTDATE.
    WHEN 6.
      CONCATENATE P_PDATE+0(4) P_PDATE+5(2) P_PDATE+7(2) INTO P_EXTDATE.
  ENDCASE.

ENDFORM.                    " user_specific_date1
