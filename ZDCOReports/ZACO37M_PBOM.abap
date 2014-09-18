************************************************************************
* Program Name      : ZACO37M_PBOM
* Author            : Hyung Jin Youn
* Creation Date     : 28/08/2003
* Specifications By : Hae-Sung Cho
* Pattern           : Report 1-1
* Development Request No: UD1K902085
* Add documentation :
* Description       :
*              The Business Plan BOMs - BPBOM are required to establish
*              a business plan and it is also required to check
*              PP BOMs before copying them to BPBOM. This program check
*              PP BOMs and replace them to others when new materials are
*              created or missing materials is found. - (the replacement
*              is not done immediately but done once in a month )
*the BDC structures for BATCH INPUT processing
*
* Modifications Log
* Date   Developer   Request ID    Description
*
************************************************************************
REPORT ZACO37M_PBOM MESSAGE-ID ZMCO.


*----------------------------------------------------------------------*
*   Include Program
*----------------------------------------------------------------------*
* For PBO
INCLUDE ZACO37M_1O01.
* For PAI
INCLUDE ZACO37M_1I01.
* For Classes
INCLUDE ZACO37M_ZLCL.


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Tables
TABLES : RM60X, MDPB, ZTCO_BUSPLANBOM.

** Internal table
DATA : BEGIN OF IT_ZTCO_BUSPLANBOM OCCURS 0.
        INCLUDE STRUCTURE ZTCO_BUSPLANBOM.
DATA :  CELLTAB TYPE LVC_T_STYL.
DATA : END OF   IT_ZTCO_BUSPLANBOM.

** For BAPI
DATA : IT_RETURN	        LIKE STANDARD TABLE OF BAPIRET2
                          WITH HEADER LINE .
DATA : GV_BOM_USAGE       LIKE BAPI1080_BGR_C-BOM_USAGE
                          VALUE  '1'.

** For screen
DATA: OK_CODE LIKE SY-UCOMM,
      SAVE_OK LIKE SY-UCOMM.
DATA: GV_CONTAINER TYPE SCRFNAME VALUE 'GRID_CTRL_MAIN',
      GRID_MAIN    TYPE REF TO CL_GUI_ALV_GRID,
      GV_CUSTOM_CONTAINER
                   TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      GV_LAYOUT    TYPE LVC_S_LAYO.
DATA: GV_EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.
RANGES R_DATVE FOR SY-DATUM.

*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_VERSB LIKE RM60X-VERSB  DEFAULT SY-DATUM+2(2) VALUE CHECK
                     OBLIGATORY,
             P_DATVE LIKE SY-DATUM     DEFAULT SY-DATUM
                     OBLIGATORY.
SELECTION-SCREEN END OF BLOCK BL1.


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.
* Set Valid date (For BOM)
  PERFORM SET_VALID_DATE.


*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Set Valid date (For BOM)
  PERFORM SET_VALID_DATE.

AT SELECTION-SCREEN ON P_VERSB.
* Check Numeric Version
*  PERFORM CHECK_NUM_VER.

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Read data from Planned Independent Requirements
* And Check the BOM existence.
  PERFORM READ_AND_CHECK_MAT_BOM.

* Enqueue
  PERFORM ENQUEUE_BUSPLANBOM.

* Grid screen
  CALL SCREEN 100.

* Enqueue
  PERFORM DEQUEUE_BUSPLANBOM.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.


*
*----------------------------------------------------------------------*
* Sub-routines
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SET_VALID_DATE
*&---------------------------------------------------------------------*
*       Set Valid date (For BOM)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_VALID_DATE.

  CLEAR : R_DATVE, R_DATVE[].
  DATA : LV_YEAR LIKE  RPCIMPEX-YEAR.

  LV_YEAR = SY-DATUM(4).
  LV_YEAR+2(2) = P_VERSB.

  CALL FUNCTION 'HR_E_GET_FISC_YEAR_DATES'
       EXPORTING
            FISC_YEAR   = LV_YEAR
       IMPORTING
            FISC_FECINI = R_DATVE-LOW
            FISC_FECFIN = R_DATVE-HIGH
       EXCEPTIONS
            ERROR       = 1
            OTHERS      = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  R_DATVE-OPTION = 'BT'.
  R_DATVE-SIGN = 'I'.
  APPEND R_DATVE.

ENDFORM.                    " SET_VALID_DATE

*&---------------------------------------------------------------------*
*&      Form  READ_AND_CHECK_MAT_BOM
*&---------------------------------------------------------------------*
*       Read data from Planned Independent Requirements
*       And Check the BOM existence.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_AND_CHECK_MAT_BOM.

* Read data (Planned Independent Requirements)
* Only Material Code and Plant
  PERFORM READ_PL_IND_REQ.

* Check PP BOM existence.
  PERFORM CHK_PP_BOM_EXIST.

ENDFORM.                    " READ_AND_CHECK_MAT_BOM

*&---------------------------------------------------------------------*
*&      Form  READ_PL_IND_REQ
*&---------------------------------------------------------------------*
*       Read data (Planned Independent Requirements)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PL_IND_REQ.
* Local Data Definition
  DATA : BEGIN OF IT_L_TMP_ZTCO_BUSPLANBOM OCCURS 0.
          INCLUDE STRUCTURE ZTCO_BUSPLANBOM.
  DATA : END OF   IT_L_TMP_ZTCO_BUSPLANBOM.

  CLEAR : IT_ZTCO_BUSPLANBOM, IT_ZTCO_BUSPLANBOM[].

* Using Index PBIM - V
  SELECT DISTINCT MATNR WERKS
    INTO CORRESPONDING FIELDS OF TABLE IT_L_TMP_ZTCO_BUSPLANBOM
    FROM MDPB
   WHERE VERSB = P_VERSB
     AND PDATU IN R_DATVE .
* Storing DATA
  LOOP AT IT_L_TMP_ZTCO_BUSPLANBOM.
    MOVE-CORRESPONDING IT_L_TMP_ZTCO_BUSPLANBOM
                    TO IT_ZTCO_BUSPLANBOM.
    APPEND IT_ZTCO_BUSPLANBOM.
    CLEAR : IT_L_TMP_ZTCO_BUSPLANBOM,
            IT_ZTCO_BUSPLANBOM.
  ENDLOOP.
ENDFORM.                    " READ_PL_IND_REQ

*&---------------------------------------------------------------------*
*&      Form  CHK_PP_BOM_EXIST
*&---------------------------------------------------------------------*
*       Check PP BOM existence.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_PP_BOM_EXIST.

* Check BOM EXISTENCE
* Usage = '01' : Production
* GV_BOM_USAGE
  LOOP AT IT_ZTCO_BUSPLANBOM.

    CLEAR : IT_RETURN, IT_RETURN[].

    CALL FUNCTION 'BAPI_MAT_BOM_EXISTENCE_CHECK'
      EXPORTING
        MATERIAL              = IT_ZTCO_BUSPLANBOM-MATNR
        PLANT                 = IT_ZTCO_BUSPLANBOM-WERKS
        BOMUSAGE              = GV_BOM_USAGE
        VALID_FROM_DATE       = P_DATVE
*       VALID_TO_DATE         = S_DATVE-HIGH
*       MATERIAL_EVG          =
      TABLES
        RETURN                = IT_RETURN.

* The BAPI "BAPI_MAT_BOM_EXISTENCE_CHECK" generates only "w" type
* messages if fails to find.
* It sends out no message If success to find proper BOM.
    IF IT_RETURN[] IS INITIAL .
* OK
      CALL FUNCTION 'CP_BD_GET_BOM_FOR_MATERIAL'
        EXPORTING
          STTAG_IMP       = P_DATVE
          MATNR_IMP       = IT_ZTCO_BUSPLANBOM-MATNR
          WERKS_IMP       = IT_ZTCO_BUSPLANBOM-WERKS
          STLTY_IMP       = 'M'
*       IMPORTING
*         STLTY_EXP       =
*         STLNR_EXP       =
*         STLAL_EXP       =
        EXCEPTIONS
          NOT_FOUND       = 1
          OTHERS          = 2.
      IF SY-SUBRC <> 0.
* Not Valid
      ELSE.
        IT_ZTCO_BUSPLANBOM-MATNR_CHG = IT_ZTCO_BUSPLANBOM-MATNR.
        MODIFY IT_ZTCO_BUSPLANBOM.
      ENDIF.
    ELSE.
* Non-Existence
    ENDIF.
    CLEAR IT_ZTCO_BUSPLANBOM.
  ENDLOOP.
  CLEAR IT_ZTCO_BUSPLANBOM.

* Check The Data contents
  IF IT_ZTCO_BUSPLANBOM[] IS INITIAL.
    MESSAGE E026.
  ENDIF.

ENDFORM.                    " CHK_PP_BOM_EXIST

*&---------------------------------------------------------------------*
*&      Form  exit_program
*&---------------------------------------------------------------------*
*       EXIT
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXIT_PROGRAM.
  LEAVE PROGRAM.
ENDFORM.                    " exit_program

*&---------------------------------------------------------------------*
*&      Form  switch_edit_mode
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SWITCH_EDIT_MODE.

  IF GRID_MAIN->IS_READY_FOR_INPUT( ) EQ 0.
* set edit enabled cells ready for input
    CALL METHOD GRID_MAIN->SET_READY_FOR_INPUT
                     EXPORTING I_READY_FOR_INPUT = 1.

  ELSE.
* lock edit enabled cells against input (Only When No error)
*    IF GV_EVENT_RECEIVER->GV_ERROR_IN_DATA = 'X'.
*   Do Nothing
*    ELSE.
    CALL METHOD GRID_MAIN->SET_READY_FOR_INPUT
                    EXPORTING I_READY_FOR_INPUT = 0.
*    ENDIF.
  ENDIF.
ENDFORM.                    " switch_edit_mode

*&---------------------------------------------------------------------*
*&      Form  USER_COMM_100
*&---------------------------------------------------------------------*
*       USER_COMMAND for Screen 100
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM USER_COMM_100.
  SAVE_OK = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK.
    WHEN 'EXIT'.
      PERFORM EXIT_PROGRAM.
    WHEN 'SWITCH'.
* Toggle dis/edit button
      PERFORM SWITCH_EDIT_MODE.
    WHEN 'SAVE'.
* Update data
      PERFORM UPDATE_DATA.
    WHEN 'TABL'.
* Read Table DATA
      PERFORM READ_TABLE_DATA.
    WHEN OTHERS.
*     do nothing
  ENDCASE.
* Refresh table to syncronize data
  CALL METHOD GRID_MAIN->REFRESH_TABLE_DISPLAY.
ENDFORM.                    " USER_COMM_100

*&---------------------------------------------------------------------*
*&      Form  SET_CUST_CONT_ATTR
*&---------------------------------------------------------------------*
*       Set the attributes of Customer Container
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_CUST_CONT_ATTR.
* Local Data definition
  DATA: IT_L_EXCLUDE TYPE UI_FUNCTIONS.

* Declaration Container
  IF GV_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT GV_CUSTOM_CONTAINER
           EXPORTING CONTAINER_NAME = GV_CONTAINER.
    CREATE OBJECT GRID_MAIN
           EXPORTING I_PARENT = GV_CUSTOM_CONTAINER.

* Initialize the Style of Grid Control
    PERFORM INIT_STYLE.

* Provide the fieldname of the celltab field
    GV_LAYOUT-STYLEFNAME = 'CELLTAB'.

* set substate of editable cells to deactivated
    CALL METHOD GRID_MAIN->SET_READY_FOR_INPUT
          EXPORTING I_READY_FOR_INPUT = 0.

* Change only
* (The user shall not be able to add new lines).
    PERFORM EXCLUDE_TB_FUNCTIONS CHANGING IT_L_EXCLUDE.

* Call Method for Display
    CALL METHOD GRID_MAIN->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME      = 'ZTCO_BUSPLANBOM'
                   IS_LAYOUT             = GV_LAYOUT
                   IT_TOOLBAR_EXCLUDING  = IT_L_EXCLUDE
         CHANGING  IT_OUTTAB             = IT_ZTCO_BUSPLANBOM[].

* Register EDIT method
    CALL METHOD GRID_MAIN->REGISTER_EDIT_EVENT EXPORTING
                         I_EVENT_ID =
                                CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

* Create OBJ For Event  handler
    CREATE OBJECT GV_EVENT_RECEIVER.
    SET HANDLER GV_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR GRID_MAIN.
    SET HANDLER GV_EVENT_RECEIVER->READ_ERROR_LOG      FOR GRID_MAIN.

  ENDIF.

ENDFORM.                    " SET_CUST_CONT_ATTR

*&---------------------------------------------------------------------*
*&      Form  INIT_STYLE
*&---------------------------------------------------------------------*
*       Initialize style
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_STYLE.
  DATA : IT_L_CELLTAB TYPE LVC_T_STYL WITH HEADER LINE .

  LOOP AT IT_ZTCO_BUSPLANBOM.
    REFRESH IT_L_CELLTAB.

    CLEAR IT_L_CELLTAB.
    IT_L_CELLTAB-FIELDNAME = 'MATNR_CHG'.
    IT_L_CELLTAB-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
    APPEND IT_L_CELLTAB .

* Copy your celltab to the celltab of the current row of gt_outtab.
    INSERT LINES OF IT_L_CELLTAB INTO TABLE IT_ZTCO_BUSPLANBOM-CELLTAB.
    MODIFY IT_ZTCO_BUSPLANBOM.
    CLEAR IT_ZTCO_BUSPLANBOM.
  ENDLOOP.
ENDFORM.                    " INIT_STYLE

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       Change only (Excluding F-code)
*----------------------------------------------------------------------*
*      <--IT_L_EXCLUDE  F-Codes to be Excluded
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS CHANGING IT_L_EXCLUDE TYPE UI_FUNCTIONS.

  DATA WA_L_EXCLUDE TYPE UI_FUNC.

  WA_L_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
  APPEND WA_L_EXCLUDE TO IT_L_EXCLUDE.
  WA_L_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
  APPEND WA_L_EXCLUDE TO IT_L_EXCLUDE.
  WA_L_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
  APPEND WA_L_EXCLUDE TO IT_L_EXCLUDE.
  WA_L_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
  APPEND WA_L_EXCLUDE TO IT_L_EXCLUDE.
  WA_L_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
  APPEND WA_L_EXCLUDE TO IT_L_EXCLUDE.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS

*&---------------------------------------------------------------------*
*&      Form  CHECK_MAT_TYPE
*&---------------------------------------------------------------------*
*       Check Material Type
*----------------------------------------------------------------------*
*      -->P_LV_MATNR_CHG_ROW_ID  Index
*      -->P_LV_CHG_MAT           Material code (Changed)
*      -->P_LV_ORG_MATNR         Original Material
*      -->P_LV_SUB               result
*----------------------------------------------------------------------*
FORM CHECK_MAT_TYPE USING    P_LV_MATNR_CHG_ROW_ID
                             P_LV_CHG_MAT
                             P_LV_ORG_MATNR
                             P_LV_SUB.
  TABLES *MARC.

* Check Material Type Matching
  DATA  WA_L_ZTCO_BUSPLANBOM LIKE IT_ZTCO_BUSPLANBOM.
  CLEAR WA_L_ZTCO_BUSPLANBOM.
  READ TABLE  IT_ZTCO_BUSPLANBOM INTO WA_L_ZTCO_BUSPLANBOM
                                 INDEX P_LV_MATNR_CHG_ROW_ID.

  DATA : LV_OLD_MTART LIKE MARA-MTART,
         LV_NEW_MTART LIKE MARA-MTART.

  CLEAR P_LV_ORG_MATNR.
  P_LV_ORG_MATNR = WA_L_ZTCO_BUSPLANBOM-MATNR.

  SELECT SINGLE  MTART  INTO LV_OLD_MTART
                 FROM MARA
                WHERE MATNR =  P_LV_ORG_MATNR.

  SELECT SINGLE  MTART  INTO LV_NEW_MTART
                 FROM MARA
                WHERE MATNR =  P_LV_CHG_MAT.

  IF LV_OLD_MTART <> LV_NEW_MTART.
    P_LV_SUB = 'W'.
  ENDIF.
* Check The relationship with Plant
  CLEAR *MARC.
  SELECT SINGLE * FROM *MARC
         WHERE MATNR =  P_LV_CHG_MAT
           AND WERKS =  WA_L_ZTCO_BUSPLANBOM-WERKS .
  IF SY-SUBRC  <> 0.
    P_LV_SUB = 'E'.
  ENDIF.
ENDFORM.                    " CHECK_MAT_TYPE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATA
*&---------------------------------------------------------------------*
*       Update data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_DATA.
* Local DATA definition
  DATA : BEGIN OF IT_L_UP_ZTCO_BUSPLANBOM OCCURS 0.
          INCLUDE STRUCTURE ZTCO_BUSPLANBOM.
  DATA : END OF   IT_L_UP_ZTCO_BUSPLANBOM.

  DATA it_busplanbom LIKE ztco_busplanbom OCCURS 0 WITH HEADER LINE.

  CLEAR : IT_L_UP_ZTCO_BUSPLANBOM, IT_L_UP_ZTCO_BUSPLANBOM[],
          it_busplanbom, it_busplanbom[].

* When No data In field of "MATNR_CHG" without changing the data,
* Check the Initial Value
  LOOP AT  IT_ZTCO_BUSPLANBOM  WHERE MATNR_CHG EQ SPACE
                                  OR MATNR_CHG IS INITIAL.
  ENDLOOP.
  IF SY-SUBRC = 0.
    GV_EVENT_RECEIVER->GV_ERROR_IN_DATA = 'X'.
  ENDIF.

* lock edit enabled cells against input (Only When No error)
  IF GV_EVENT_RECEIVER->GV_ERROR_IN_DATA = 'X'.
    MESSAGE E029.
  ELSE.
* Delete all data in Table
    DELETE FROM ZTCO_BUSPLANBOM WHERE MATNR EQ SPACE
                                   OR MATNR NE SPACE.
* Making Internal Table for Insertion
    LOOP AT IT_ZTCO_BUSPLANBOM.
      MOVE-CORRESPONDING IT_ZTCO_BUSPLANBOM
                      TO IT_L_UP_ZTCO_BUSPLANBOM .
      APPEND IT_L_UP_ZTCO_BUSPLANBOM.
      CLEAR  IT_L_UP_ZTCO_BUSPLANBOM.
      CLEAR  IT_ZTCO_BUSPLANBOM.
    ENDLOOP.

*// === 2011.08.04 insert by Kim_yn.     for ECC6 upgrade === //*
* Insertion (No update Because it is a temporary table)
    loop at IT_ZTCO_BUSPLANBOM.
      move-corresponding IT_ZTCO_BUSPLANBOM to it_busplanbom.
      append it_busplanbom.
      clear: it_busplanbom.
    endloop.

    INSERT ZTCO_BUSPLANBOM  FROM TABLE it_busplanbom.
    clear: it_busplanbom, it_busplanbom[].
*// ========================== END ========================== //*

    IF SY-SUBRC <> 0.
      ROLLBACK WORK.
      MESSAGE E029.
    ELSE.
      MESSAGE S009 WITH ' - Inserting'.
    ENDIF.
  ENDIF.
ENDFORM.                    " UPDATE_DATA

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_BUSPLANBOM
*&---------------------------------------------------------------------*
*       ENQUEUE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENQUEUE_BUSPLANBOM.
* Enqueue For Client
  CALL FUNCTION 'ENQUEUE_EZCO_BUSPLANBOM'
    EXPORTING
      MODE_ZTCO_BUSPLANBOM       = 'E'
      MANDT                      = SY-MANDT
*       MATNR                      = IT_ZTCO_BUSPLANBOM-MATNR
*       WERKS                      = IT_ZTCO_BUSPLANBOM-WERKS
*       X_MATNR                    = ' '
*       X_WERKS                    = ' '
*       _SCOPE                     = '2'
*       _WAIT                      = ' '
*       _COLLECT                   = ' '
      EXCEPTIONS
        FOREIGN_LOCK               = 1
        SYSTEM_FAILURE             = 2
        OTHERS                     = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* For View Lock
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE         = 'E'
      TABNAME              = 'ZTCO_BUSPLANBOM'
*     VARKEY               =
*     X_TABNAME            = ' '
*     X_VARKEY             = ' '
*     _SCOPE               = '2'
*     _WAIT                = ' '
*     _COLLECT             = ' '
    EXCEPTIONS
      FOREIGN_LOCK         = 1
      SYSTEM_FAILURE       = 2
      OTHERS               = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ENQUEUE_BUSPLANBOM

*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_BUSPLANBOM
*&---------------------------------------------------------------------*
*       DEQUEUE
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEQUEUE_BUSPLANBOM.
  CALL FUNCTION 'DEQUEUE_EZCO_BUSPLANBOM'
    EXPORTING
      MODE_ZTCO_BUSPLANBOM       = 'E'
      MANDT                      = SY-MANDT
*     MATNR                      =
*     WERKS                      =
*     X_MATNR                    = ' '
*     X_WERKS                    = ' '
*     _SCOPE                     = '3'
*     _SYNCHRON                  = ' '
*     _COLLECT                   = ' '
            .

  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      MODE_RSTABLE       = 'E'
      TABNAME            = 'ZTCO_BUSPLANBOM'
*     VARKEY             =
*     X_TABNAME          = ' '
*     X_VARKEY           = ' '
*     _SCOPE             = '3'
*     _SYNCHRON          = ' '
*     _COLLECT           = ' '
            .

ENDFORM.                    " DEQUEUE_BUSPLANBOM

*&---------------------------------------------------------------------*
*&      Form  READ_TABLE_DATA
*&---------------------------------------------------------------------*
*       Read Table DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_TABLE_DATA.

* Local DATA definition
  DATA : BEGIN OF IT_L_TMP_ZTCO_BUSPLANBOM OCCURS 0.
          INCLUDE STRUCTURE ZTCO_BUSPLANBOM.
  DATA : END OF   IT_L_TMP_ZTCO_BUSPLANBOM.

  CLEAR : IT_L_TMP_ZTCO_BUSPLANBOM, IT_L_TMP_ZTCO_BUSPLANBOM[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_L_TMP_ZTCO_BUSPLANBOM
           FROM ZTCO_BUSPLANBOM.

  CALL FUNCTION 'STC1_POPUP_WITH_TABLE_CONTROL'
       EXPORTING
            HEADER            = 'Table Contents'
            TABNAME           = 'ZTCO_BUSPLANBOM'
            DISPLAY_ONLY      = 'X'
            NO_BUTTON         = SPACE
       TABLES
            TABLE             = IT_L_TMP_ZTCO_BUSPLANBOM
       EXCEPTIONS
            NO_MORE_TABLES    = 1
            TOO_MANY_FIELDS   = 2
            NAMETAB_NOT_VALID = 3
            HANDLE_NOT_VALID  = 4
            OTHERS            = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_TABLE_DATA

*&---------------------------------------------------------------------*
*&      Form  CHECK_NUM_VER
*&---------------------------------------------------------------------*
*       Check Numeric Version
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_NUM_VER.

*  DATA : LV_HTYPE	LIKE	DD01V-DATATYPE.
*
*  CALL FUNCTION 'NUMERIC_CHECK'
*    EXPORTING
*      STRING_IN        = P_VERSB
*    IMPORTING
**   STRING_OUT       =
*      HTYPE            = LV_HTYPE.
*
*  IF LV_HTYPE <> 'NUMC'.
*    MESSAGE E055.
*  ENDIF.

ENDFORM.                    " CHECK_NUM_VER
