************************************************************************
* Program Name      : ZIPP201I_ZTPPVP
* Author            : Jong Oh, Kim
* Creation Date     : 2003.09.16
* Specifications By : JongOh, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K901977
* Addl Documentation:
* Description       : Vehicle Planned Order from PP to ALC
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZIPP201I_ZTPPVP  NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTPPVP,
         ZSPPVP,
         ZTPP_PMT07JB_A,
         EQUI.
*VAL_TABLE	LIKE	ZSPP_VIN_VALUE
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : IT_PMT07JB_IR  LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE,
       IT_PMT07JB_RP  LIKE TABLE OF ZTPP_PMT07JB_A WITH HEADER LINE.

DATA : IT_ZSPPVP_IR   LIKE TABLE OF ZSPPVP  WITH HEADER LINE,
       IT_ZSPPVP_RP   LIKE TABLE OF ZSPPVP  WITH HEADER LINE,
       IT_ZSPPVP_DL   LIKE TABLE OF ZSPPVP  WITH HEADER LINE,
       IT_ZSPPVP_BA   LIKE TABLE OF ZSPPVP  WITH HEADER LINE,
       IT_ZTPPVP      LIKE TABLE OF ZTPPVP  WITH HEADER LINE.

DATA : IT_ZSPPVP      LIKE TABLE OF ZSPPVP  WITH HEADER LINE.
DATA : IT_EQUI        LIKE TABLE OF EQUI  WITH HEADER LINE.
DATA : IT_VM	    LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_FNAME_TX(40),
       WA_SAVELINE_IX     LIKE  SY-INDEX.
DATA : WA_IR         LIKE  SY-TABIX,   "Count IR
       WA_RP         LIKE  SY-TABIX,   "Count RP
       WA_DL         LIKE  SY-TABIX,   "Count DL
       WA_BA         LIKE  SY-TABIX.   "Count BA

DATA : WA_ATINN      TYPE  CABN-ATINN,
       WA_OBJEK      TYPE  AUSP-OBJEK,
       WA_ATWRT_S    TYPE  AUSP-ATWRT,
       WA_ATWRT_E    TYPE  AUSP-ATWRT,
       WA_ATFLV_S    TYPE  AUSP-ATFLV,
       WA_ATFLV_E    TYPE  AUSP-ATFLV.

RANGES: R_DATUM      FOR  SY-DATUM,
        R_ATINN      FOR  CABN-ATINN,
        R_ATNAM      FOR  CABN-ATNAM.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : OK_CODE       LIKE  SY-UCOMM,
       SAVE_OK_CODE  LIKE  SY-UCOMM.
DATA : ALV_GRID               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER           TYPE SCRFNAME VALUE 'CONTAINER'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : GS_VARIANT        TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria
DATA : WA_FIELDCAT     TYPE LVC_S_FCAT.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_GUBB    VALUE  '*',
           C_MITU    VALUE  'M',
           C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination
*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
*PARAMETERS : P_WERKS      LIKE   T001W-WERKS OBLIGATORY MEMORY ID WRK.
 SELECT-OPTIONS : S_DATUM  FOR   SY-DATUM  OBLIGATORY NO-EXTENSION.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) TEXT-100.
PARAMETERS: P_IR AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-101 FOR FIELD P_IR.   "INSERT
PARAMETERS: P_RP AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-102 FOR FIELD P_RP.   "REPLACE
PARAMETERS: P_DL AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-103 FOR FIELD P_DL.   "DELETE
PARAMETERS: P_BA AS CHECKBOX.
SELECTION-SCREEN COMMENT  (12) TEXT-104 FOR FIELD P_BA.   "BACK
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (5) TEXT-200.
PARAMETERS: P_REW  AS CHECKBOX .
SELECTION-SCREEN COMMENT  (15) TEXT-201 FOR FIELD P_REW.   " RE-WORK
SELECTION-SCREEN END OF LINE.

*----------------------------------------------------------------------*
*  MACRO DECLARATION
*----------------------------------------------------------------------*
DEFINE FIELDCAT_COMPOSE.
  IT_FIELDCAT-FIELDNAME = &1.
  IT_FIELDCAT-REPTEXT   = &2.
  IT_FIELDCAT-OUTPUTLEN = 20.
  APPEND IT_FIELDCAT TO GT_FIELDCAT.

END-OF-DEFINITION.

DEFINE SET_FIELDCAT.
  L_FIELDCAT-REPTEXT   = &1.
  L_FIELDCAT-SCRTEXT_L = &1.
  L_FIELDCAT-SCRTEXT_M = &1.
  L_FIELDCAT-SCRTEXT_S = &1.
  L_FIELDCAT-COLDDICTXT = 'L'.
  L_FIELDCAT-OUTPUTLEN = &2.

END-OF-DEFINITION.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.
*  IF IT_ZTPPVP[] IS INITIAL.
*    MESSAGE E001 WITH 'No valid Data exist for selected condition'.
*  ENDIF.
************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  CALL SCREEN 9000.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  DATA L_DATUM_DT   LIKE  SY-DATUM.
  L_DATUM_DT = SY-DATUM + 3.
  S_DATUM-SIGN   = 'I'.
  S_DATUM-OPTION = 'BT'.
  S_DATUM-LOW    = SY-DATUM.
  S_DATUM-HIGH   = L_DATUM_DT.
  APPEND S_DATUM.
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
 IF P_IR EQ SPACE AND P_RP EQ SPACE AND P_DL EQ SPACE AND P_BA EQ SPACE.
    MESSAGE E001 WITH TEXT-211.
  ELSE.
    CLEAR: R_DATUM, R_DATUM[].
    LOOP AT S_DATUM.
      MOVE-CORRESPONDING S_DATUM TO R_DATUM.
      APPEND R_DATUM.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
  CLEAR :   IT_PMT07JB_IR,  IT_PMT07JB_RP,
            IT_ZSPPVP_IR, IT_ZSPPVP_RP,
            IT_ZSPPVP_DL, IT_ZSPPVP_BA,
            IT_ZSPPVP,
            IT_EQUI.
  REFRESH : IT_PMT07JB_IR,  IT_PMT07JB_RP,
            IT_ZSPPVP_IR, IT_ZSPPVP_RP,
            IT_ZSPPVP_DL, IT_ZSPPVP_BA,
            IT_ZSPPVP,
            IT_EQUI.
  CLEAR : WA_IR, WA_RP, WA_DL, WA_BA.
*-----> IR(INSERT)
  PERFORM APPEND_IR.

*-----> RP(REPLACE)
  PERFORM APPEND_RP.

*-----> READ VEHICLE MASTER FOR DL AND BA
  PERFORM READ_VEHICLE_MASTER.

ENDFORM.                    " EXECUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_VEHICLE_MASTER
*&---------------------------------------------------------------------*
FORM READ_VEHICLE_MASTER.
  IF P_DL EQ C_MARK.
    PERFORM READ_DLBA_FROM_VEHICLE.
  ENDIF.

  IF P_BA EQ C_MARK.
    PERFORM READ_DLBA_FROM_VEHICLE.
*    CASE P_REW.
*      WHEN SPACE.
*        PERFORM READ_DLBA_FROM_VEHICLE.

*      WHEN C_MARK.
*        PERFORM REWORK_DL.
*        PERFORM REWORK_BA.
*    ENDCASE.
  ENDIF.
ENDFORM.                    " READ_VEHICLE_MASTER

*&---------------------------------------------------------------------*
*&      Form  APPEND_IR
*&---------------------------------------------------------------------*
FORM APPEND_IR.
  CLEAR : WA_IR.
  IF P_IR EQ C_MARK.
    CASE P_REW.
      WHEN  SPACE.
        PERFORM SELECT_IR_FROM_PMT07JB.

      WHEN C_MARK.      "RE-WORK
        PERFORM REWORK_IR.

    ENDCASE.
    DESCRIBE TABLE IT_ZSPPVP_IR LINES WA_IR.
  ENDIF.
ENDFORM.                    " APPEND_IR
*&---------------------------------------------------------------------*
*&      Form  APPEND_RP
*&---------------------------------------------------------------------*
FORM APPEND_RP.
  CLEAR WA_RP.
  IF P_RP EQ C_MARK.
    CASE P_REW.
      WHEN SPACE.
        PERFORM SELECT_RP_FROM_PMT07JB.

      WHEN C_MARK.    "RE-WORK
        PERFORM REWORK_RP.

    ENDCASE.
    DESCRIBE TABLE IT_ZSPPVP_RP LINES WA_RP.
  ENDIF.
ENDFORM.                    " APPEND_RP

*&---------------------------------------------------------------------*
*&      Form  SELECT_IR_FROM_PMT07JB
*&---------------------------------------------------------------------*
FORM SELECT_IR_FROM_PMT07JB.
  SELECT *
         INTO TABLE IT_PMT07JB_IR
         FROM ZTPP_PMT07JB_A
*         WHERE VHNO NE SPACE     " Serial of VIN
          WHERE MTGU EQ SPACE     " ' '
            AND GUBB EQ C_GUBB    " '*'
            AND SQDT IN R_DATUM.  " Sequence Date
  IF SY-SUBRC EQ 0.
    LOOP AT IT_PMT07JB_IR.
      IF IT_PMT07JB_IR-VINN NE SPACE.
        MOVE-CORRESPONDING IT_PMT07JB_IR TO IT_ZSPPVP_IR.
        MOVE IT_PMT07JB_IR-VINN+11(6)    TO IT_ZSPPVP_IR-VHNO.
        IT_ZSPPVP_IR-FLG = 'IR'.
        APPEND IT_ZSPPVP_IR.
        MOVE-CORRESPONDING IT_ZSPPVP_IR TO IT_ZSPPVP.
        APPEND IT_ZSPPVP.
        CLEAR : IT_ZSPPVP_IR, IT_ZSPPVP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SELECT_IR_FROM_PMT07JB

*&---------------------------------------------------------------------*
*&      Form  REWORK_IR
*&---------------------------------------------------------------------*
FORM REWORK_IR.
  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_ZSPPVP_IR
         FROM ZTPPVP
         WHERE FLG  EQ 'IR'
           AND SQDT IN R_DATUM.
  LOOP AT IT_ZSPPVP_IR.
    MOVE-CORRESPONDING IT_ZSPPVP_IR TO IT_ZSPPVP.
    APPEND IT_ZSPPVP.
    CLEAR IT_ZSPPVP.
  ENDLOOP.
ENDFORM.                    " REWORK_IR

*&---------------------------------------------------------------------*
*&      Form  SELECT_RP_FROM_PMT07JB
*&---------------------------------------------------------------------*
FORM SELECT_RP_FROM_PMT07JB.
  SELECT *
         INTO TABLE IT_PMT07JB_RP
         FROM ZTPP_PMT07JB_A
*         WHERE VHNO NE SPACE      " Serial of VIN
          WHERE MTGU EQ C_MITU     " 'M'
            AND GUBB EQ C_GUBB     " '*'
            AND SQDT IN R_DATUM.   " Sequence Date
  IF SY-SUBRC EQ 0.
    LOOP AT IT_PMT07JB_RP.
      IF IT_PMT07JB_RP-VINN NE SPACE.
        MOVE-CORRESPONDING IT_PMT07JB_RP TO IT_ZSPPVP_RP.
        MOVE IT_PMT07JB_RP-VINN+11(6)    TO IT_ZSPPVP_RP-VHNO.
        IT_ZSPPVP_RP-FLG = 'RP'.
        APPEND IT_ZSPPVP_RP.
        MOVE-CORRESPONDING IT_ZSPPVP_RP TO IT_ZSPPVP.
        APPEND IT_ZSPPVP.
        CLEAR : IT_ZSPPVP_RP, IT_ZSPPVP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " SELECT_RP_FROM_PMT07JB

*&---------------------------------------------------------------------*
*&      Form  REWORK_RP
*&---------------------------------------------------------------------*
FORM REWORK_RP.
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSPPVP_RP
     FROM ZTPPVP
     WHERE FLG  EQ 'RP'
       AND SQDT IN R_DATUM.
  LOOP AT IT_ZSPPVP_RP.
    MOVE-CORRESPONDING IT_ZSPPVP_RP TO IT_ZSPPVP.
    APPEND IT_ZSPPVP.
    CLEAR IT_ZSPPVP.
  ENDLOOP.
ENDFORM.                    " REWORK_RP

*&---------------------------------------------------------------------*
*&      Form  SELECT_DL_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM SELECT_DL_FROM_VEHICLE.
  DATA : L_ATINN  LIKE  CABN-ATINN,
*         L_ATNAM  LIKE  CABN-ATNAM,
         L_ATWRT  LIKE  AUSP-ATWRT.

  PERFORM CLASSIFICATION_VALUE USING 'P_STATUS'
                               CHANGING L_ATWRT.
  IF SY-SUBRC EQ 0 .
    IF L_ATWRT EQ 'T24'.
      PERFORM APPEND_DL.
    ELSE.
      PERFORM CLASSIFICATION_VALUE USING 'P_USAGE_CAR'
                                   CHANGING L_ATWRT.
      IF SY-SUBRC EQ 0.
        PERFORM APPEND_DL.
      ENDIF.
    ENDIF.
  ELSE.
    PERFORM CLASSIFICATION_VALUE USING 'P_USAGE_CAR'
                                 CHANGING L_ATWRT.
    IF SY-SUBRC EQ 0.
      PERFORM APPEND_DL.
    ENDIF.
  ENDIF.

*  DATA:    L_STATUS      LIKE  ZSPP_VIN_VALUE-ATWRT,
*           L_USAGE_CAR   LIKE  ZSPP_VIN_VALUE-ATWRT,
*           L_RP18        LIKE  SY-DATUM . "ZSPP_VIN_VALUE-ATWRT.
*  DATA:  WA_FLG.
*  IF P_DL EQ C_MARK.
*    CLEAR : L_RP18, L_STATUS, L_USAGE_CAR.
*
**----> P_RP18_SHOP_DATE (Sign/Off On)
*    READ TABLE IT_VM WITH KEY ATNAM = 'P_RP18_ACTUAL_DATE'.
*    IF SY-SUBRC EQ 0.
*      L_RP18 = IT_VM-ATWRT(8).
*    ENDIF.
*
*    IF R_DATUM-HIGH IS INITIAL.
*      IF L_RP18 EQ S_DATUM-LOW.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ELSE.
*      IF L_RP18 GE R_DATUM-LOW AND L_RP18 LE R_DATUM-HIGH.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ENDIF.
*
*    IF WA_FLG EQ 'X'.
**----> P_STATUS
*      READ TABLE IT_VM WITH KEY ATNAM = 'P_STATUS'.
*      IF SY-SUBRC EQ 0 AND IT_VM-ATWRT EQ 'T24'.
*        L_STATUS = IT_VM-ATWRT.
*      ENDIF.
**----> P_USAGE_CAR
*      READ TABLE IT_VM WITH KEY ATNAM = 'P_USAGE_CAR'.
*      IF SY-SUBRC EQ 0 AND IT_VM-ATWRT EQ 'S'.
*        L_USAGE_CAR = IT_VM-ATWRT.
*      ENDIF.
*
*      IF L_STATUS EQ 'T24' OR L_USAGE_CAR EQ 'S'.
*        IT_ZSPPVP_DL-FLG   = 'DL'.
**----> P_MODEL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_MODEL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_DL-MODL  = IT_VM-ATWRT.
*        ENDIF.
**----> P_BODY_SERIAL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_SERIAL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_DL-VHNO  = IT_VM-ATWRT.
*        ENDIF.
*
*        APPEND IT_ZSPPVP_DL.
*        MOVE-CORRESPONDING IT_ZSPPVP_DL TO IT_ZSPPVP.
*        APPEND IT_ZSPPVP.
*        CLEAR : IT_ZSPPVP_DL, IT_ZSPPVP.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " SELECT_DL_FROM_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  REWORK_DL
*&---------------------------------------------------------------------*
FORM REWORK_DL.
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSPPVP_DL
     FROM ZTPPVP
     WHERE FLG  EQ 'DL'
       AND SQDT IN S_DATUM.

  LOOP AT IT_ZSPPVP_DL.
    MOVE-CORRESPONDING IT_ZSPPVP_DL TO IT_ZSPPVP.
    APPEND IT_ZSPPVP.
    CLEAR IT_ZSPPVP.
  ENDLOOP.
ENDFORM.                    " REWORK_DL

*&---------------------------------------------------------------------*
*&      Form  SELECT_BA_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM SELECT_BA_FROM_VEHICLE.
  DATA : L_ATINN  LIKE  CABN-ATINN,
         L_ATWRT  LIKE  AUSP-ATWRT.

  PERFORM CLASSIFICATION_VALUE USING 'P_USAGE_CAR'
                               CHANGING L_ATWRT.
  IF SY-SUBRC EQ 0 AND L_ATWRT EQ 'R'.
    MOVE  SY-MANDT TO    IT_ZSPPVP_BA-MANDT.
    MOVE  'BA'     TO    IT_ZSPPVP_BA-FLG.
    PERFORM CLASSIFICATION_VALUE USING 'P_MODEL'
                                 CHANGING IT_ZSPPVP_BA-MODL.
    PERFORM CLASSIFICATION_VALUE USING 'P_BODY_SERIAL'
                                 CHANGING IT_ZSPPVP_BA-VHNO.
    PERFORM CLASSIFICATION_VALUE USING 'P_WORKORDER'
                                 CHANGING IT_ZSPPVP_BA-ORDR.
    PERFORM CLASSIFICATION_VALUE USING 'P_DESTINATION_CODE'
                                 CHANGING IT_ZSPPVP_BA-DIST.
    PERFORM CLASSIFICATION_VALUE USING 'P_INT_COLOR'
                                 CHANGING IT_ZSPPVP_BA-INTC.
    PERFORM CLASSIFICATION_VALUE USING 'P_EXT_COLOR'
                                 CHANGING IT_ZSPPVP_BA-EXTC.
    PERFORM CLASSIFICATION_VALUE USING 'P_VIN'
                                 CHANGING IT_ZSPPVP_BA-VINN.
    PERFORM CLASSIFICATION_VALUE USING 'P_BODY_PLANT_NO'
                                 CHANGING IT_ZSPPVP_BA-PLNT.
    PERFORM CLASSIFICATION_VALUE USING 'P_BODY_LINE_NO'
                                 CHANGING IT_ZSPPVP_BA-LINE.
    PERFORM CLASSIFICATION_VALUE USING 'P_SEQUENCE_SERIAL'
                                 CHANGING IT_ZSPPVP_BA-SSR1.
    PERFORM CLASSIFICATION_VALUE USING 'P_EMMISSION'
                                 CHANGING IT_ZSPPVP_BA-P_EMMISSION.
    PERFORM CLASSIFICATION_VALUE USING 'P_SEQUENCE_DATE'
                                 CHANGING IT_ZSPPVP_BA-SQDT.
    PERFORM CLASSIFICATION_VALUE USING 'P_ENGINE_NO'
                                 CHANGING IT_ZSPPVP_BA-P_ENGINE_NO.
    PERFORM CLASSIFICATION_VALUE USING 'P_KEY_NO'
                                 CHANGING IT_ZSPPVP_BA-P_KEY_NO.
    PERFORM CLASSIFICATION_VALUE USING 'P_TM_NO'
                                 CHANGING IT_ZSPPVP_BA-P_TM_NO.
    APPEND IT_ZSPPVP_BA.
    MOVE-CORRESPONDING IT_ZSPPVP_BA TO IT_ZSPPVP.
    APPEND IT_ZSPPVP.
    CLEAR : IT_ZSPPVP_BA, IT_ZSPPVP.
  ENDIF.

*  DATA : L_USAGE_CAR   LIKE  ZSPP_VIN_VALUE-ATWRT,
*         L_RETURN_DT   LIKE  SY-DATUM,
*         WA_FLG.
*
*  IF P_BA EQ C_MARK.
*    CLEAR : L_USAGE_CAR.
**----> RETURN DATE
*    READ TABLE IT_VM WITH KEY ATNAM = 'P_RETURN_DATE'.
*    IF SY-SUBRC EQ 0.
*      L_RETURN_DT = IT_VM-ATWRT(8).
*    ENDIF.
*
*    IF R_DATUM-HIGH IS INITIAL.
*      IF L_RETURN_DT EQ R_DATUM-LOW.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ELSE.
*      IF L_RETURN_DT GE R_DATUM-LOW AND L_RETURN_DT LE R_DATUM-HIGH.
*        WA_FLG = 'X'.
*      ELSE.
*        CLEAR WA_FLG.
*      ENDIF.
*    ENDIF.
*
*    IF WA_FLG EQ 'X'.
**---> P_USAGE_CAR
*      READ TABLE IT_VM WITH KEY ATNAM = 'P_USAGE_CAR'.
*      IF SY-SUBRC EQ 0 AND IT_VM-ATWRT EQ 'R'.
*        L_USAGE_CAR = IT_VM-ATWRT.
*      ENDIF.
*
*      IF L_USAGE_CAR EQ 'R'.
**---> P_MODEL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_MODEL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-MODL  = IT_VM-ATWRT.
*        ENDIF.
**---> P_BODY_SERIAL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_SERIAL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-VHNO  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_WORK_ORDER
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_WORK_ORDER'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-ORDR  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_DESTINATION_CODE
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_DESTINATION_CODE'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-DIST  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_INT_COLOR
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_INT_COLOR'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-INTC  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_EXT_COLOR
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_EXT_COLOR'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-EXTC  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_VIN
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_VIN'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-VINN  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_VIN
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_VIN'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-VINN  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_BODY_PLANT_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_PLANT_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-PLNT  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_BODY_LINE_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_BODY_LINE_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-LINE  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_SEQUENCE_SERIAL
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_SEQUENCE_SERIAL'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-SSR1  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_EMISSION
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_EMISSION'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_EMMISSION  = IT_VM-ATWRT.
*        ENDIF.
*
**---> P_SEQUENCE_DATE
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_SEQUENCE_DATE'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-SQDT  = IT_VM-ATWRT.
*        ENDIF.
*
*        IT_ZSPPVP_BA-FLG   = 'BA'.
**---> P_ENGINE_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_ENGINE_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_ENGINE_NO  = IT_VM-ATWRT.
*        ENDIF.
**---> P_KEY_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_KEY_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_KEY_NO  = IT_VM-ATWRT.
*        ENDIF.
**---> P_TM_NO
*        READ TABLE IT_VM WITH KEY ATNAM = 'P_TM_NO'.
*        IF SY-SUBRC EQ 0.
*          IT_ZSPPVP_BA-P_TM_NO  = IT_VM-ATWRT.
*        ENDIF.
*        APPEND IT_ZSPPVP_BA.
*        MOVE-CORRESPONDING IT_ZSPPVP_BA TO IT_ZSPPVP.
*        APPEND IT_ZSPPVP.
*        CLEAR : IT_ZSPPVP_BA, IT_ZSPPVP.
*      ENDIF.
*    ENDIF.
*  ENDIF.
ENDFORM.                    " SELECT_BA_FROM_VEHICLE

*&---------------------------------------------------------------------*
*&      Form  REWORK_BA
*&---------------------------------------------------------------------*
FORM REWORK_BA.
  SELECT *
     INTO CORRESPONDING FIELDS OF TABLE IT_ZSPPVP_BA
     FROM ZTPPVP
     WHERE FLG  EQ 'BA'
       AND SQDT IN S_DATUM.

  LOOP AT IT_ZSPPVP_BA.
    MOVE-CORRESPONDING IT_ZSPPVP_BA TO IT_ZSPPVP.
    APPEND IT_ZSPPVP.
    CLEAR IT_ZSPPVP.
  ENDLOOP.
ENDFORM.                    " REWORK_BA

*&---------------------------------------------------------------------*
*&      Form  READ_DLBA_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM READ_DLBA_FROM_VEHICLE.
  DATA : L_ATFOR   TYPE  CABN-ATFOR,
         L_NUM(8) TYPE  N,
         L_INT     TYPE  I.

  IF P_DL EQ C_MARK.
    CLEAR : WA_DL, WA_ATINN, L_ATFOR.
    SELECT SINGLE ATINN
                  ATFOR
                INTO (WA_ATINN, L_ATFOR)
                FROM CABN
                WHERE ATNAM EQ 'P_RP18_ACTUAL_DATE'.
  ELSEIF P_BA EQ C_MARK.
    CLEAR : WA_BA, WA_ATINN, L_ATFOR.
    SELECT SINGLE ATINN
                  ATFOR
                INTO (WA_ATINN, L_ATFOR)
                FROM CABN
                WHERE ATNAM EQ 'P_RETURN_DATE'.
  ENDIF.

  IF L_ATFOR EQ 'CHAR'.
    IF S_DATUM-HIGH IS INITIAL.
      CONCATENATE S_DATUM-LOW '000000' INTO WA_ATWRT_S.
      CONCATENATE S_DATUM-LOW '999999' INTO WA_ATWRT_E.
    ELSE.
      CONCATENATE S_DATUM-LOW  '000000' INTO WA_ATWRT_S.
      CONCATENATE S_DATUM-HIGH '999999' INTO WA_ATWRT_E.
    ENDIF.

    EXEC SQL PERFORMING SELECT_DLBA_FROM_VEHICLE.
      SELECT A.OBJEK
        INTO :WA_OBJEK
        FROM AUSP A, EQUI B
       WHERE B.MANDT = :SY-MANDT
         AND B.EQTYP = 'V'
         AND A.MANDT = :SY-MANDT
         AND A.OBJEK = B.EQUNR
         AND A.ATINN = :WA_ATINN
         AND A.ATWRT BETWEEN :WA_ATWRT_S AND :WA_ATWRT_E
         AND A.KLART = '002'
    ENDEXEC.
  ELSE.
    IF S_DATUM-HIGH IS INITIAL.
      MOVE  : S_DATUM-LOW    TO   L_NUM.
      L_INT = L_NUM.
      WA_ATFLV_S = L_INT.
      WA_ATFLV_E = L_INT.
    ELSE.
      MOVE  : S_DATUM-LOW     TO   L_NUM.
      L_INT = L_NUM.
      WA_ATFLV_S = L_INT.
      MOVE  : S_DATUM-HIGH    TO   L_NUM.
      L_INT = L_NUM.
      WA_ATFLV_E = L_INT.
    ENDIF.
    EXEC SQL PERFORMING SELECT_DLBA_FROM_VEHICLE.
      SELECT A.OBJEK
        INTO :WA_OBJEK
        FROM AUSP A, EQUI B
       WHERE B.MANDT = :SY-MANDT
         AND B.EQTYP = 'V'
         AND A.MANDT = :SY-MANDT
         AND A.OBJEK = B.EQUNR
         AND A.ATINN = :WA_ATINN
         AND A.ATFLV BETWEEN :WA_ATFLV_S AND :WA_ATFLV_E
         AND A.KLART = '002'
    ENDEXEC.
  ENDIF.


*  SELECT *
*         INTO TABLE IT_EQUI
*         FROM EQUI
*         WHERE EQTYP  EQ 'V'.
**           AND LVORM  EQ SPACE.
*  LOOP AT IT_EQUI.
*    CLEAR : IT_VM, IT_VM[].
**-----> READ IT_VM FOR VEHICLE MASTER
*    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
*      EXPORTING
*        OBJECT           = IT_EQUI-EQUNR
**             MODE             = 'R'
*      TABLES
*        VAL_TABLE        = IT_VM
*      EXCEPTIONS
*       NO_DATA          = 1
*       ERROR_MODE       = 2
*       OTHERS           = 3.
*    IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ELSE.
*      PERFORM SELECT_DL_FROM_VEHICLE.
*      PERFORM SELECT_BA_FROM_VEHICLE.
*      DESCRIBE TABLE IT_ZSPPVP_DL LINES WA_DL.
*      DESCRIBE TABLE IT_ZSPPVP_BA LINES WA_BA.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " READ_DLBA_FROM_VEHICLE


*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET TITLEBAR '9000'.
  IF IT_ZSPPVP[] IS INITIAL.
    SET PF-STATUS 'MAIN' EXCLUDING 'ZTRAN'.
  ELSE.
    SET PF-STATUS 'MAIN'.
  ENDIF.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_ALV_GRID OUTPUT.
  IF GS_CUSTOM_CONTAINER IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION.

    CREATE OBJECT GS_CUSTOM_CONTAINER
        EXPORTING CONTAINER_NAME = WA_CONTAINER.

    CREATE OBJECT ALV_GRID
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER.

    PERFORM  BUILD_VARIANT.
    PERFORM  BUILD_LAYOUT.
    PERFORM  BUILD_FIELDCAT.

*-----> SET OBJECT
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'ZSPPVP_S'
        IS_VARIANT                    = GS_VARIANT
        I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
        IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
      CHANGING
        IT_OUTTAB                     = IT_ZSPPVP[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4
            .
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.
ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_CURSOR_FIELD OUTPUT.
  SET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " SET_CURSOR_FIELD  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  READ_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE READ_DATA INPUT.
  PERFORM CHECK_AND_READ_DATA.
ENDMODULE.                 " READ_DATA  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR_FIELD  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_CURSOR_FIELD INPUT.
  CLEAR: WA_FNAME_TX, WA_SAVELINE_IX.
  GET CURSOR FIELD WA_FNAME_TX LINE WA_SAVELINE_IX.
ENDMODULE.                 " GET_CURSOR_FIELD  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.
  CASE SAVE_OK_CODE.
    WHEN 'ZTRAN'.   "ALC TRANSFER
      PERFORM SAVE_AND_TRANSFER.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM BUILD_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " BUILD_VARIANT

*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM BUILD_LAYOUT.
  GS_LAYOUT-ZEBRA  = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN

ENDFORM.                    " BUILD_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.

  DATA: L_STRUCT    LIKE DD02L-TABNAME.


  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.

  L_STRUCT = 'ZSPPVP_S'.
  CLEAR : WA_FIELDCAT, GT_FIELDCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE        = 'X'
            I_STRUCTURE_NAME       = L_STRUCT
       CHANGING
            CT_FIELDCAT            = GT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

*  DELETE GT_FIELDCAT  WHERE FIELDNAME = 'MANDT' OR
*                            FIELDNAME = 'ZUSER' OR
*                            FIELDNAME = 'ZSDAT' OR
*                            FIELDNAME = 'ZSTIM' OR
*                            FIELDNAME = '' OR
*                            FIELDNAME = ''.

  LOOP AT GT_FIELDCAT INTO WA_FIELDCAT.
    PERFORM SET_FIELD_INFO USING WA_FIELDCAT.
    MODIFY GT_FIELDCAT FROM WA_FIELDCAT.
    CLEAR WA_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " BUILD_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_INFO
*&---------------------------------------------------------------------*
FORM SET_FIELD_INFO USING L_FIELDCAT STRUCTURE LVC_S_FCAT.

  CASE L_FIELDCAT-FIELDNAME.
    WHEN 'FLG'.
      SET_FIELDCAT  'FLG' 3.
      L_FIELDCAT-KEY = 'X'.
    WHEN 'MODL'.
      SET_FIELDCAT 'MODEL' 3.
      L_FIELDCAT-KEY = 'X'.
    WHEN 'VHNO'.
      SET_FIELDCAT 'BODY NO.' 7.
      L_FIELDCAT-KEY = 'X'.
    WHEN 'ORDR'.
      SET_FIELDCAT 'WORK ORDER' 9.
    WHEN 'DIST'.
      SET_FIELDCAT 'DIST.' 5.
    WHEN 'INTC'.
*      SET_FIELDCAT 'INT'  3.
    WHEN 'EXTC'.
*      SET_FIELDCAT 'EXT'  3.
    WHEN 'VINN'.
      SET_FIELDCAT 'VIN' 17.
    WHEN 'K01PNO'.
*      SET_FIELDCAT 'Packing No' 8.
    WHEN 'PLNT'.
*      SET_FIELDCAT 'Plant' 2.
    WHEN 'LINE'.
*      SET_FIELDCAT 'Line'  2.
    WHEN 'SSR1'.
      SET_FIELDCAT 'SEQ' 4.
    WHEN 'P_EMMISSION'.
      SET_FIELDCAT 'CAL.' 2.
    WHEN 'EVL1'.
      SET_FIELDCAT 'VAL1' 4.
    WHEN 'EVL2'.
      SET_FIELDCAT 'VAL2' 4.
    WHEN 'EVL3'.
      SET_FIELDCAT 'VAL3' 4.
    WHEN 'EVL4'.
      SET_FIELDCAT 'VAL4' 4.
    WHEN 'EVL5'.
      SET_FIELDCAT 'VAL5' 4.
    WHEN 'P_ENGINE_NO'.
      SET_FIELDCAT 'ENGINE ASSY ID' 15.
    WHEN 'P_KEY_NO'.
      SET_FIELDCAT 'KEY' 6.
    WHEN 'P_TM_NO'.
      SET_FIELDCAT 'KEY' 15.
    WHEN 'CDAT'.
      SET_FIELDCAT 'CREATE DATE' 10.
    WHEN 'CTIM'.
      SET_FIELDCAT 'CREATE TIME' 10.
    WHEN 'ZEDAT'.
      SET_FIELDCAT 'I/F DATE' 10.
  ENDCASE.

ENDFORM.                    " SET_FIELD_INFO
*&---------------------------------------------------------------------*
*&      Form  CHECK_AND_READ_DATA
*&---------------------------------------------------------------------*
FORM CHECK_AND_READ_DATA.
  IF P_IR EQ SPACE AND P_RP EQ SPACE
     AND P_DL EQ SPACE AND P_BA EQ SPACE.
    MESSAGE E001 WITH TEXT-211.
  ELSE.
    IF NOT R_DATUM-HIGH IS INITIAL.
      IF R_DATUM-LOW GT R_DATUM-HIGH.
        MESSAGE E001 WITH TEXT-212.
      ELSE.
        PERFORM READ_DATA.
      ENDIF.
    ELSE.
      PERFORM READ_DATA.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_AND_READ_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
FORM READ_DATA.
  CLEAR : R_DATUM[].
  IF R_DATUM-HIGH IS INITIAL.
    R_DATUM-SIGN = 'I'.
    R_DATUM-OPTION = 'EQ'.
    APPEND R_DATUM.
  ELSE.
    R_DATUM-SIGN = 'I'.
    R_DATUM-OPTION = 'BT'.
    APPEND R_DATUM.
  ENDIF.
  PERFORM EXECUTE_PROCESS.

  CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
  FREE  GS_CUSTOM_CONTAINER.

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
FORM SAVE_AND_TRANSFER.
  DATA : L_MSGTXT(100),
         L_LINESTEXT(10),
         L_LINES      LIKE  SY-TABIX.

*---> Interface date update
  IT_ZSPPVP-ZUSER = SY-UNAME.
  IT_ZSPPVP-ZSDAT = SY-DATUM.
  IT_ZSPPVP-ZSTIM = SY-UZEIT.

  MODIFY IT_ZSPPVP TRANSPORTING ZUSER ZSDAT ZSTIM
                                WHERE MANDT EQ SY-MANDT.

  IF SY-SUBRC EQ 0.
    CALL FUNCTION 'Z_FPP_SET_ZTPPVP'
      DESTINATION C_DEST
      EXPORTING
          IR             =  WA_IR  "IR QTTY
          RP             =  WA_RP  "RP QTTY
          DL             =  WA_DL  "DL QTTY
          BA             =  WA_BA  "BA QTTY
      TABLES
           I_ZSPPVP              = IT_ZSPPVP
      EXCEPTIONS
           COMMUNICATION_FAILURE = 1 MESSAGE L_MSGTXT
           SYSTEM_FAILURE        = 2 MESSAGE L_MSGTXT.
    IF SY-SUBRC NE 0.
      MESSAGE I001 WITH L_MSGTXT.
    ELSE.
      IT_ZSPPVP-ZEDAT = SY-DATUM.
      IT_ZSPPVP-ZETIM = SY-UZEIT.
      MODIFY IT_ZSPPVP TRANSPORTING ZUSER ZSDAT ZSTIM
                                WHERE MANDT EQ SY-MANDT.
      CLEAR : IT_ZTPPVP, IT_ZTPPVP[], L_LINES.
      LOOP AT IT_ZSPPVP WHERE ZZRET EQ 'S'.
        MOVE-CORRESPONDING IT_ZSPPVP TO IT_ZTPPVP.
        APPEND IT_ZTPPVP.
        L_LINES = L_LINES + 1.
        CLEAR IT_ZSPPVP.
      ENDLOOP.
      DESCRIBE TABLE IT_ZTPPVP LINES L_LINES.
      WRITE L_LINES TO L_LINESTEXT.
      MODIFY ZTPPVP FROM TABLE IT_ZTPPVP.
      MESSAGE I002 WITH L_LINESTEXT
                       TEXT-301.
    ENDIF.
  ENDIF.

ENDFORM.                    " SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
*&      Form  SELECT_DLBA_FROM_VEHICLE
*&---------------------------------------------------------------------*
FORM SELECT_DLBA_FROM_VEHICLE.
  CASE C_MARK.
    WHEN P_DL.
      PERFORM SELECT_DL_FROM_VEHICLE.
    WHEN P_BA.
      PERFORM SELECT_BA_FROM_VEHICLE.
  ENDCASE.
ENDFORM.                    " SELECT_DLBA_FROM_VEHICLE
*&---------------------------------------------------------------------*
*&      Form  CLASSIFICATION_VALUE
*&---------------------------------------------------------------------*
FORM CLASSIFICATION_VALUE USING P_ATNAM
                          CHANGING P_ATWRT.

  DATA : L_ATINN    TYPE   CABN-ATINN,
         L_ATFOR    TYPE   CABN-ATFOR,
         L_ATWRT    TYPE   AUSP-ATWRT,
         L_ATFLV    TYPE   AUSP-ATFLV,
         L_INTEGER  TYPE   I.

  SELECT SINGLE ATINN
                ATFOR
         INTO (L_ATINN, L_ATFOR)
         FROM CABN
         WHERE ATNAM EQ P_ATNAM.

  CLEAR P_ATWRT.
  SELECT SINGLE ATWRT
                ATFLV
         INTO (L_ATWRT, L_ATFLV)
         FROM AUSP
         WHERE OBJEK = WA_OBJEK
           AND KLART = '002'
           AND ATINN = L_ATINN.
  CASE L_ATFOR.
    WHEN 'CHAR'.
      P_ATWRT = L_ATWRT.
    WHEN OTHERS.
      L_INTEGER = L_ATFLV.
      WRITE L_INTEGER TO P_ATWRT LEFT-JUSTIFIED NO-GROUPING.
  ENDCASE.
ENDFORM.                    " CLASSIFICATION_VALUE
*&---------------------------------------------------------------------*
*&      Form  APPEND_DL
*&---------------------------------------------------------------------*
FORM APPEND_DL.
  WA_DL = WA_DL + 1.
  MOVE  SY-MANDT TO      IT_ZSPPVP_DL-MANDT.
  MOVE   'DL'    TO      IT_ZSPPVP_DL-FLG.
  PERFORM CLASSIFICATION_VALUE USING 'P_MODEL'
                               CHANGING IT_ZSPPVP_DL-MODL.
  PERFORM CLASSIFICATION_VALUE USING 'P_BODY_SERIAL'
                               CHANGING IT_ZSPPVP_DL-VHNO.
  APPEND IT_ZSPPVP_DL.
  MOVE-CORRESPONDING IT_ZSPPVP_DL TO IT_ZSPPVP.
  APPEND IT_ZSPPVP.
  CLEAR : IT_ZSPPVP_DL, IT_ZSPPVP.
ENDFORM.                    " APPEND_DL
