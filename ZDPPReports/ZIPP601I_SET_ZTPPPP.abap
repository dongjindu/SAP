************************************************************************
* Program Name      : ZIPP601I_SET_ZTPPPP
* Author            : Jong Oh, Kim
* Creation Date     : 2003.09.16
* Specifications By : JongOh, Kim
* Pattern           : 5.2.2
* Development Request No : UD1K907489
* Addl Documentation:
* Description     : Transfer of Press Production Planning from PP to MES
*
* Modification Logs
* Date       Developer    RequestNo    Description
************************************************************************
REPORT ZIPP601I_SET_ZTPPPP  NO STANDARD PAGE HEADING
                          MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTPPPP .  "Press Production Planning From PP(MIP) -> MES

*VAL_TABLE	LIKE	ZSPP_VIN_VALUE
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : IT_ZTPPPP       LIKE TABLE OF ZTPPPP     WITH HEADER LINE ,
       IT_ZTPPPP_TEMP  LIKE TABLE OF ZTPPPP     WITH HEADER LINE ,
       IT_ZSPPPP       LIKE TABLE OF ZSPPPP_RFC WITH HEADER LINE .

DATA : BEGIN OF IT_PRESS OCCURS 0,
*         CHK,
         GSTRP      TYPE  ZTPPPP-GSTRP ,
         ARBPL      TYPE  ZTPPPP-ARBPL ,
         CY_SEQNR   TYPE  ZTPPPP-CY_SEQNR ,
         MATNR      TYPE  ZTPPPP-MATNR ,
         MAKTX      TYPE  MAKT-MAKTX   ,
         AUFNR      TYPE  ZTPPPP-AUFNR ,
         GAMNG      TYPE  ZTPPPP-GAMNG ,
         CSTOK      TYPE  ZTPPPP-CSTOK .
DATA : END OF IT_PRESS.

DATA : IT_BLANK    LIKE TABLE OF IT_PRESS WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA : WA_FNAME_TX(40),
       WA_SAVELINE_IX     LIKE  SY-INDEX.
DATA : WA_IFTOTAL_IX      LIKE  SY-INDEX,
       WA_IFSUCCESS_IX    LIKE  SY-INDEX,
       WA_IFERROR_IX     LIKE  SY-INDEX.

*----------------------------------------------------------------------*
* ALV DECLARATION.
*----------------------------------------------------------------------*
DATA : OK_CODE       LIKE  SY-UCOMM,
       SAVE_OK_CODE  LIKE  SY-UCOMM.
DATA : ALV_GRID1               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER1    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER1           TYPE SCRFNAME VALUE 'CONTAINER1',
       ALV_GRID2               TYPE REF TO CL_GUI_ALV_GRID,
       GS_CUSTOM_CONTAINER2    TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       WA_CONTAINER2           TYPE SCRFNAME VALUE 'CONTAINER2'.
*       GS_APPLICATION         TYPE REF TO LCL_APPLICATION,
DATA : GS_VARIANT        TYPE DISVARIANT ,  "Display Variant
       GS_LAYOUT         TYPE LVC_S_LAYO ,  "Layout
       GS_PRINT          TYPE LVC_S_PRNT ,  "Print control
       GS_SORT           TYPE LVC_S_SORT ,  "Sort Criteria
       GT_SPECIAL_GROUPS TYPE LVC_T_SGRP ,  "Field groups
       GT_TOOLBAR_EXCLUDING TYPE UI_FUNCTIONS , "Exclu Toolbar Std FUNC
       GT_HEADER         TYPE TABLE OF SLIS_LISTHEADER WITH HEADER LINE,
       GT_FIELDCAT       TYPE LVC_T_FCAT ,  "Field Catalog
       GT_SORT           TYPE LVC_T_SORT ,  "Sort criteria
       GT_FILTER         TYPE LVC_T_FILT .  "Filter criteria
DATA : WA_FIELDCAT       TYPE LVC_S_FCAT .

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : IT_BDCDATA     LIKE TABLE OF BDCDATA  WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION(BDC)
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: WA_OPTION_DS   LIKE CTU_PARAMS.   "BDC OPTION

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_MODE    VALUE  'N',
           C_DEST(10) VALUE 'WMPP01'.   "Outbound Interface Destination

CONSTANTS: C_SPP     TYPE   MARC-FEVOR   VALUE  'SPP',
           C_SPB     TYPE   MARC-FEVOR   VALUE  'SPB',
           C_P128    TYPE   MARD-LGORT   VALUE  'P128',
           C_P125    TYPE   MARD-LGORT   VALUE  'P125'.

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
  L_FIELDCAT-COL_POS   = &3.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS: P_WERKS      LIKE T001W-WERKS OBLIGATORY.
SELECT-OPTIONS: S_GSTRP  FOR ZTPPPP-GSTRP OBLIGATORY NO-EXTENSION,
                S_AUFNR  FOR ZTPPPP-AUFNR,
                S_MATNR  FOR ZTPPPP-MATNR,
                S_SEQNR  FOR ZTPPPP-CY_SEQNR.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) TEXT-100.
PARAMETERS: P_SEQ RADIOBUTTON GROUP R .
SELECTION-SCREEN COMMENT  (15) TEXT-101 FOR FIELD P_SEQ.   "SEQ LOGIC
PARAMETERS: P_CHECK  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (25) TEXT-103 FOR FIELD P_CHECK. "Allowance
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  (20) TEXT-104.
PARAMETERS: P_MRP RADIOBUTTON GROUP R .
SELECTION-SCREEN COMMENT  (25) TEXT-102 FOR FIELD P_MRP.   "MRP LOGIC
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.

SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-005.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS      P_IR RADIOBUTTON GROUP RAD1 DEFAULT 'X'.   "IR
SELECTION-SCREEN COMMENT (25) TEXT-006 FOR FIELD P_IR.
PARAMETERS      P_RP RADIOBUTTON GROUP RAD1.               "RP
SELECTION-SCREEN COMMENT (25) TEXT-007 FOR FIELD P_RP.
PARAMETERS      P_DL RADIOBUTTON GROUP RAD1.               "DL
SELECTION-SCREEN COMMENT (20) TEXT-008 FOR FIELD P_DL.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B2.

SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 5.
PARAMETERS       P_TRAN   RADIOBUTTON GROUP RADI DEFAULT 'X'.
SELECTION-SCREEN COMMENT (30) TEXT-002 FOR FIELD P_TRAN.     "Transfer
PARAMETERS       P_RETR   RADIOBUTTON GROUP RADI.
SELECTION-SCREEN COMMENT (25) TEXT-003 FOR FIELD P_RETR.     "Retranfer
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B3.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING  P_PROGRAM
                       P_DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = P_PROGRAM.
  IT_BDCDATA-DYNPRO   = P_DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.

ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING    P_FNAM
                        P_FVAL.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = P_FNAM.
  IT_BDCDATA-FVAL = P_FVAL.
  APPEND IT_BDCDATA.

ENDFORM.                    " BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  P_WERKS = 'P001'.

  S_GSTRP-SIGN   = 'I'.
  S_GSTRP-OPTION = 'EQ'.
  S_GSTRP-LOW    = SY-DATUM .
  APPEND S_GSTRP .

ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
  CASE C_MARK.
    WHEN P_TRAN .
      PERFORM CALL_APP601R .
    WHEN P_RETR .
*----> Select ZTPPPP
      PERFORM SELECT_ZTPPPP.
      CALL SCREEN 9000.
  ENDCASE .
ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_APP601R
*&---------------------------------------------------------------------*
FORM CALL_APP601R.
  SUBMIT  ZAPP601R_PRESS_ZTPPPP AND RETURN
                                WITH   P_WERKS = P_WERKS
                                WITH   P_PANEL = C_SPP
                                WITH   P_BLANK = C_SPB
                                WITH   S_DATUM IN S_GSTRP
                                WITH   S_MATNR IN S_MATNR
                                WITH   P_SEQ   = P_SEQ
                                WITH   P_CHECK = P_CHECK
                                WITH   P_MRP   = P_MRP
                                WITH   P_IR    = P_IR
                                WITH   P_RP    = P_RP
                                WITH   P_DL    = P_DL .
ENDFORM.                    " CALL_APP601R
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTPPPP
*&---------------------------------------------------------------------*
FORM SELECT_ZTPPPP.
  CLEAR : IT_PRESS,   IT_PRESS[],
          IT_BLANK,   IT_BLANK[],
          IT_ZTPPPP,  IT_ZTPPPP[] .
*----> SELECT PRDORD
  SELECT *
        INTO TABLE IT_ZTPPPP
        FROM ZTPPPP
        WHERE PWERK    EQ   P_WERKS      "PLANT
          AND GSTRP    IN   S_GSTRP      "Plan date
          AND CY_SEQNR IN   S_SEQNR      "SEQ No
          AND AUFNR    IN   S_AUFNR      "Prod Order No
          AND MATNR    IN   S_MATNR      "Material No
          AND AUFNR    LT   'PL0000000000' .
*----> SELECT PLDORD FOR BDC ERROR
  SELECT *
         APPENDING CORRESPONDING FIELDS OF TABLE IT_ZTPPPP
         FROM ZTPPPP
        WHERE PWERK    EQ   P_WERKS      "PLANT
          AND GSTRP    IN   S_GSTRP      "Plan date
          AND CY_SEQNR IN   S_SEQNR      "SEQ No
          AND AUFNR    IN   S_AUFNR      "Prod Order No
          AND MATNR    IN   S_MATNR      "Material No
          AND ZRESULT  EQ   'B' .

  SORT IT_ZTPPPP BY GSTRP ARBPL CY_SEQNR MATNR .

  LOOP AT IT_ZTPPPP .
    IF IT_ZTPPPP-FEVOR EQ C_SPP+2(1) .
      MOVE-CORRESPONDING IT_ZTPPPP TO IT_PRESS.
      SELECT SINGLE MAKTX
                 INTO IT_PRESS-MAKTX
                 FROM MAKT
                 WHERE MATNR EQ IT_PRESS-MATNR
                   AND SPRAS EQ SY-LANGU  .
      APPEND IT_PRESS.   CLEAR IT_PRESS.
    ELSEIF IT_ZTPPPP-FEVOR EQ C_SPB+2(1) .
      MOVE-CORRESPONDING IT_ZTPPPP TO IT_BLANK.
      SELECT SINGLE MAKTX
                 INTO IT_BLANK-MAKTX
                 FROM MAKT
                 WHERE MATNR EQ IT_BLANK-MATNR
                   AND SPRAS EQ SY-LANGU  .
      APPEND IT_BLANK.   CLEAR IT_BLANK.
    ENDIF.
  ENDLOOP.

  SORT : IT_PRESS BY GSTRP ARBPL CY_SEQNR MATNR ,
         IT_BLANK BY GSTRP ARBPL CY_SEQNR MATNR .
ENDFORM.                    " SELECT_ZTPPPP
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET TITLEBAR '9000'.
  IF IT_ZTPPPP[] IS INITIAL .
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
  IF GS_CUSTOM_CONTAINER1 IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION .

    CREATE OBJECT GS_CUSTOM_CONTAINER1
        EXPORTING CONTAINER_NAME = WA_CONTAINER1.

    CREATE OBJECT ALV_GRID1
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER1.

    PERFORM  BUILD_VARIANT.
    PERFORM  BUILD_LAYOUT USING C_SPP .
    PERFORM  BUILD_SORT.
    PERFORM  BUILD_FIELDCAT.

*-----> SET OBJECT
    CALL METHOD ALV_GRID1->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'IT_PRESS'
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
        IT_OUTTAB                     = IT_PRESS[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
        IT_SORT                       = GT_SORT
*        IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4      .

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

  IF GS_CUSTOM_CONTAINER2 IS INITIAL.
*-----> CREATE OBJECT
*    CREATE OBJECT GS_APPLICATION .

    CREATE OBJECT GS_CUSTOM_CONTAINER2
        EXPORTING CONTAINER_NAME = WA_CONTAINER2.

    CREATE OBJECT ALV_GRID2
        EXPORTING I_PARENT = GS_CUSTOM_CONTAINER2.

    PERFORM  BUILD_VARIANT.
    PERFORM  BUILD_LAYOUT USING C_SPB .
    PERFORM  BUILD_SORT.
    PERFORM  BUILD_FIELDCAT.

*-----> SET OBJECT
    CALL METHOD ALV_GRID2->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
        I_STRUCTURE_NAME              = 'IT_BLANK'
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
        IT_OUTTAB                     = IT_BLANK[]
        IT_FIELDCATALOG               = GT_FIELDCAT[]
        IT_SORT                       = GT_SORT
*        IT_FILTER                     =
      EXCEPTIONS
        INVALID_PARAMETER_COMBINATION = 1
        PROGRAM_ERROR                 = 2
        TOO_MANY_LINES                = 3
        OTHERS                        = 4      .

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDMODULE.                 " CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_VARIANT
*&---------------------------------------------------------------------*
FORM BUILD_VARIANT.
  CLEAR GS_VARIANT .
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " BUILD_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
FORM BUILD_LAYOUT USING P_IND.
  CLEAR GS_LAYOUT .
  GS_LAYOUT-ZEBRA  = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN

  CASE P_IND .
    WHEN C_SPP.
      GS_LAYOUT-GRID_TITLE = TEXT-301 .
    WHEN C_SPB.
      GS_LAYOUT-GRID_TITLE = TEXT-302 .
  ENDCASE.
ENDFORM.                    " BUILD_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
FORM BUILD_SORT.
  CLEAR : GS_SORT, GT_SORT[].
  GS_SORT-SPOS = 1.
  GS_SORT-FIELDNAME = 'GSTRP'.
  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.

  GS_SORT-SPOS = 2.
  GS_SORT-FIELDNAME = 'ARBPL'.
  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.

  GS_SORT-SPOS = 3.
  GS_SORT-FIELDNAME = 'CY_SEQNR'.
  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.

  GS_SORT-SPOS = 4.
  GS_SORT-FIELDNAME = 'MATNR'.
  GS_SORT-UP        = 'X'.
  APPEND GS_SORT TO GT_SORT.

ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.
  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.
  DATA: LT_FIELDCAT       TYPE LVC_T_FCAT.
  DATA: L_FIELDCAT        TYPE LVC_S_FCAT.
  DATA: L_TABIX           TYPE SY-TABIX  .

  L_STRUCT = 'ZTPPPP'.
  CLEAR : WA_FIELDCAT, GT_FIELDCAT.
  REFRESH: GT_FIELDCAT .

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE        = 'X'
            I_STRUCTURE_NAME       = L_STRUCT
       CHANGING
            CT_FIELDCAT            = LT_FIELDCAT[]
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

*    PERFORM SET_FIELD_INFO USING WA_FIELDCAT
*                                 SY-TABIX .
*---> PLAN DATE
  READ TABLE LT_FIELDCAT INTO L_FIELDCAT
                     WITH KEY FIELDNAME = 'GSTRP'.
  SET_FIELDCAT  'PLAN DATE' 8 1.
  L_FIELDCAT-KEY = 'X'.
  APPEND L_FIELDCAT TO GT_FIELDCAT                .
  CLEAR L_FIELDCAT .

*---> Work Center
  READ TABLE LT_FIELDCAT INTO L_FIELDCAT
                     WITH KEY FIELDNAME = 'ARBPL'.
  SET_FIELDCAT 'W/C' 8 2.
  L_FIELDCAT-KEY = 'X'.
  APPEND L_FIELDCAT TO GT_FIELDCAT                .
  CLEAR L_FIELDCAT .

*---> SEQ No
  READ TABLE LT_FIELDCAT INTO L_FIELDCAT
                     WITH KEY FIELDNAME = 'CY_SEQNR'.
  SET_FIELDCAT 'SEQ No' 14 3.
*  L_FIELDCAT-KEY = 'X'.
  CLEAR L_FIELDCAT-KEY .
  APPEND L_FIELDCAT TO GT_FIELDCAT                .
  CLEAR L_FIELDCAT .

*---> Material
  READ TABLE LT_FIELDCAT INTO L_FIELDCAT
                     WITH KEY FIELDNAME = 'MATNR'.
  SET_FIELDCAT 'Material #' 12 4.
*  L_FIELDCAT-KEY = 'X'.
  CLEAR L_FIELDCAT-KEY .
  APPEND L_FIELDCAT TO GT_FIELDCAT                .
  CLEAR L_FIELDCAT .

*---> Material Description

  L_FIELDCAT-FIELDNAME = 'MAKTX' .
  L_FIELDCAT-TABNAME   = '1'     .
  L_FIELDCAT-DATATYPE  = 'CHAR'  .
  L_FIELDCAT-INTTYPE   = 'C'     .
  L_FIELDCAT-INTLEN    = 40      .
  L_FIELDCAT-REPTEXT   = 'Description' .
  L_FIELDCAT-DOMNAME   = 'TEXT40' .
  L_FIELDCAT-DD_OUTLEN = 40      .
  L_FIELDCAT-REF_TABLE = 'MAKT'  .
  SET_FIELDCAT 'Material Description' 30 5 .
  APPEND L_FIELDCAT TO GT_FIELDCAT                .
  CLEAR L_FIELDCAT .

*---> Prod Order No
  READ TABLE LT_FIELDCAT INTO L_FIELDCAT
                     WITH KEY FIELDNAME = 'AUFNR'.
  SET_FIELDCAT 'PROORD No' 12 6.
*  L_FIELDCAT-KEY = 'X'.
  CLEAR L_FIELDCAT-KEY .
  APPEND L_FIELDCAT TO GT_FIELDCAT                .

*---> Total Order Qty
  READ TABLE LT_FIELDCAT INTO L_FIELDCAT
                     WITH KEY FIELDNAME = 'GAMNG'.
  SET_FIELDCAT 'Order Qty' 13 7.
  L_FIELDCAT-QUANTITY = 'EA' .
  APPEND L_FIELDCAT TO GT_FIELDCAT                .

**---> Prod Order No
*  READ TABLE LT_FIELDCAT INTO L_FIELDCAT
*                     WITH KEY FIELDNAME = 'AUFNR'.
*  SET_FIELDCAT 'PROORD No' 12.
*  L_FILEDCAT-QUANTITY = 'EA' .
*  APPEND L_FIELDCAT TO GT_FIELDCAT                .

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  SET_CURSOR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
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
    WHEN 'ZTRAN'.   "ALC RETRANSFER
      PERFORM SAVE_AND_TRANSFER.
    WHEN OTHERS.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Form  SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
FORM SAVE_AND_TRANSFER.
  DATA: L_MSGTXT(100),
        L_TABIX         LIKE     SY-TABIX.
  DATA: L_ZUSER    LIKE  ZTPPPP-ZUSER ,
        L_ZSDAT    LIKE  ZTPPPP-ZSDAT ,
        L_ZSTIM    LIKE  ZTPPPP-ZSTIM .

  CLEAR : IT_ZSPPPP,      IT_ZSPPPP[] ,
          IT_ZTPPPP_TEMP, IT_ZTPPPP_TEMP[] ,
          WA_IFTOTAL_IX , WA_IFSUCCESS_IX , WA_IFERROR_IX.

  LOOP AT IT_ZTPPPP .
    L_TABIX = SY-TABIX .
    IF IT_ZTPPPP-ZRESULT EQ 'B' .
      IF IT_ZTPPPP-AUFNR+2(1) EQ SPACE .
        PERFORM GENERATE_BDC_DATA_CO01 .
        PERFORM CALL_TANSACTION_CO01   .
      ELSE.
        PERFORM GENERATE_BDC_DATA_CO40 .
        PERFORM CALL_TRANSACTION_CO40  .
      ENDIF.
*      IT_ZTPPPP-ZMODE = 'C'  .
    ELSE.
      CLEAR : IT_ZTPPPP-ZEDAT   ,
              IT_ZTPPPP-ZETIM   ,
              IT_ZTPPPP-ZUSER   ,
              IT_ZTPPPP-ZRESULT ,
              IT_ZTPPPP-ZMSG    .
    ENDIF.

    IF IT_ZTPPPP-ZRESULT NE 'B' .
      MOVE-CORRESPONDING IT_ZTPPPP TO IT_ZSPPPP.
      APPEND IT_ZSPPPP. CLEAR IT_ZSPPPP.
    ENDIF.
      MODIFY IT_ZTPPPP  INDEX  L_TABIX .
  ENDLOOP.

*----> TOTAL COUNT FOR INTERFACE
  DESCRIBE TABLE IT_ZSPPPP LINES WA_IFTOTAL_IX.

  CALL FUNCTION 'Z_FPP_PRESS_PP'
    DESTINATION  C_DEST
    TABLES
      T_ZTPPPP       = IT_ZSPPPP
  EXCEPTIONS
    COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
    SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

  IF SY-SUBRC NE 0.
*    WA_IFERROR_IX = WA_IFTOTAL_IX.
    IT_ZSPPPP-ZZRET = 'E' .
    IT_ZSPPPP-ZRESULT = IT_ZSPPPP-ZZRET .
    IT_ZSPPPP-ZUSER = SY-UNAME.
    IT_ZSPPPP-ZMODE = 'C'.
    IT_ZSPPPP-ZMSG  = L_MSGTXT .
    MODIFY IT_ZSPPPP TRANSPORTING ZZRET ZRESULT ZUSER ZMSG
                                  WHERE MANDT EQ SY-MANDT .
    MESSAGE I001 WITH L_MSGTXT.
  ENDIF.

  LOOP AT IT_ZSPPPP   .
    L_TABIX = SY-TABIX.
    IF IT_ZSPPPP-ZZRET = 'E'.
      WA_IFERROR_IX = WA_IFERROR_IX + 1.
      IT_ZSPPPP-ZRESULT = IT_ZSPPPP-ZZRET .
      IT_ZSPPPP-ZUSER = L_ZUSER .
      IT_ZSPPPP-ZMSG = TEXT-301.
      IT_ZSPPPP-ZMODE = 'U'.
      MODIFY IT_ZSPPPP INDEX L_TABIX.
    ELSE.
      WA_IFSUCCESS_IX = WA_IFSUCCESS_IX + 1.
      IT_ZSPPPP-ZUSER = L_ZUSER .
      IT_ZSPPPP-ZSTIM = L_ZSTIM .
      IT_ZSPPPP-ZMODE = 'U'.
      IT_ZSPPPP-ZRESULT = 'S'   .
      MODIFY IT_ZSPPPP INDEX L_TABIX.
    ENDIF.

    MOVE-CORRESPONDING IT_ZSPPPP TO  IT_ZTPPPP_TEMP.
    APPEND IT_ZTPPPP_TEMP.     CLEAR IT_ZTPPPP_TEMP.
  ENDLOOP.

  UPDATE ZTPPPP FROM TABLE IT_ZTPPPP_TEMP.
  CALL SCREEN 9005 STARTING AT 20 10.
ENDFORM.                    " SAVE_AND_TRANSFER
*&---------------------------------------------------------------------*
*&      Module  STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_9005 OUTPUT.
  SET PF-STATUS '9005' .
  SET TITLEBAR '9005'.

ENDMODULE.                 " STATUS_9005  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_9005 INPUT.
  SAVE_OK_CODE = OK_CODE.
  CLEAR OK_CODE.

  CASE SAVE_OK_CODE.
    WHEN 'ENTE' OR 'CANC'.
      LEAVE TO SCREEN 0 .
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9005  INPUT
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_CO01
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA_CO01.
  DATA : L_DATE_TX(10).
  DATA : L_GAMNG(14).

  WRITE: IT_ZTPPPP-GAMNG TO L_GAMNG LEFT-JUSTIFIED.
  WRITE: IT_ZTPPPP-GSTRP TO L_DATE_TX.
  PERFORM BDC_DYNPRO  USING 'SAPLCOKO1'	'0100'    .
  PERFORM BDC_FIELD   USING :
			'BDC_CURSOR'	    'CAUFVD-MATNR'     ,
			'BDC_OKCODE'	    '/00'              ,
			'CAUFVD-MATNR'	    IT_ZTPPPP-MATNR    ,
			'CAUFVD-WERKS'	    P_WERKS            ,
			'AUFPAR-PP_AUFART'    'PP01'             .

  PERFORM BDC_DYNPRO  USING 'SAPLCOKO1'	'0115'	.
  PERFORM BDC_FIELD   USING :
			'BDC_OKCODE'	    '=FREI'            ,
			'BDC_CURSOR'	    'CAUFVD-GAMNG'     ,
			'CAUFVD-GAMNG'	    L_GAMNG            ,
			'CAUFVD-GLTRP'	    L_DATE_TX          ,
			'CAUFVD-GSTRP'	    L_DATE_TX          ,
			'CAUFVD-TERKZ'	    '3'                .

  PERFORM BDC_DYNPRO  USING 'SAPLCOKO1'	'0115'	.
  PERFORM BDC_FIELD   USING :
			'BDC_OKCODE'	'=BU'                  ,
			'BDC_CURSOR'	'CAUFVD-GAMNG'         .

ENDFORM.                    " GENERATE_BDC_DATA_CO01
*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA_CO40
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA_CO40.

  DATA : L_DATE_TX(10).
  DATA : L_GAMNG(14).

  WRITE: IT_ZTPPPP-GAMNG TO L_GAMNG LEFT-JUSTIFIED.

  PERFORM BDC_DYNPRO  USING 'SAPLCOKO1'   '0150'.
  PERFORM BDC_FIELD   USING :
    'BDC_CURSOR'        'AUFPAR-PP_AUFART' ,
    'BDC_OKCODE'	      '/00' ,
    'AFPOD-PLNUM'       IT_ZTPPPP-AUFNR+2(10),
    'AUFPAR-PP_AUFART'	'PP01' .

  WRITE IT_ZTPPPP-GSTRP    TO    L_DATE_TX.

*----> Release process.
  PERFORM BDC_DYNPRO  USING 'SAPLCOKO1'	'0115' .
  PERFORM BDC_FIELD   USING :
			'BDC_OKCODE'	'=FREI'           ,
			'BDC_CURSOR'	'CAUFVD-GAMNG'    ,
			'CAUFVD-GAMNG'	L_GAMNG           ,
			'CAUFVD-TERKZ'	'3'               .

*----> Convert Planned Order to Production Order
  PERFORM BDC_DYNPRO  USING 'SAPLCOKO1'	'0115' .
  PERFORM BDC_FIELD   USING :
			'BDC_OKCODE'	'=BU'             ,
			'BDC_CURSOR'	'CAUFVD-GAMNG'    .

ENDFORM.                    " GENERATE_BDC_DATA_CO40
*&---------------------------------------------------------------------*
*&      Form  CALL_TANSACTION_CO01
*&---------------------------------------------------------------------*
FORM CALL_TANSACTION_CO01.
  CALL TRANSACTION 'CO01' USING IT_BDCDATA
                          OPTIONS FROM WA_OPTION_DS.
  PERFORM ERROR_TEXT USING 'CO01'.
  CLEAR IT_BDCDATA.
  REFRESH IT_BDCDATA.

ENDFORM.                    " CALL_TANSACTION_CO01
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION_CO40
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_CO40.
  CALL TRANSACTION 'CO40' USING IT_BDCDATA
                          OPTIONS FROM WA_OPTION_DS.
  PERFORM ERROR_TEXT USING 'CO40'.
  CLEAR IT_BDCDATA.
  REFRESH IT_BDCDATA.
ENDFORM.                    " CALL_TRANSACTION_CO40
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
FORM ERROR_TEXT USING    PA_IND.
  DATA : L_MSG    LIKE CFGNL-MSGLIN ,
         L_AUFNR  LIKE AFKO-AUFNR   .
  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = L_MSG
       EXCEPTIONS
            OTHERS  = 1.

  CLEAR : IT_ZTPPPP-ZEDAT   ,
          IT_ZTPPPP-ZETIM   ,
          IT_ZTPPPP-ZUSER   ,
          IT_ZTPPPP-ZRESULT ,
          IT_ZTPPPP-ZMSG    .

  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      MOVE:   SY-UNAME     TO    IT_ZTPPPP-ZUSER    ,
              'B'          TO    IT_ZTPPPP-ZRESULT  ,
              L_MSG        TO    IT_ZTPPPP-ZMSG     .
    WHEN OTHERS.            " 'I', 'S' :SUCCESS
      L_AUFNR = SY-MSGV1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
           EXPORTING
                INPUT  = L_AUFNR
           IMPORTING
                OUTPUT = L_AUFNR.
      MOVE:   SY-UNAME     TO    IT_ZTPPPP-ZUSER    ,
              L_MSG        TO    IT_ZTPPPP-ZMSG     ,
              L_AUFNR      TO    IT_ZTPPPP-AUFNR    .
  ENDCASE.

ENDFORM.                    " ERROR_TEXT
