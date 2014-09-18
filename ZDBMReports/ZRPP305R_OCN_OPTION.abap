************************************************************************
* Program Name      : ZRPP305R_OCN_OPTION
* Author            : Bongsoo, Kim
* Creation Date     : 2003.09.22.
* Specifications By : Bongsoo, Kim
* Pattern           : 1.1
* Development Request No : UD1K902274
* Addl Documentation:
* Description       : OCN different design option of FSC Report
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZRPP305R_OCN_OPTION
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
*  TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: SLIS.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTBM_ABXOCNDT,
         ZTBM_OCN,
         MARA,
         MAKT.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_AOCN OCCURS 0,
        OCNO TYPE ZTBM_OCN-OCNO,
        DATE(10), " TYPE SY-DATUM,
        OPTI(60),
      END   OF IT_AOCN.
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
DATA : WA_FIELDCAT     TYPE LVC_S_FCAT,
       WA_SORT          TYPE LVC_S_SORT.  "Sort criteria.
*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK    VALUE  'X',
           C_GUBB    VALUE  '*',
           C_MITU    VALUE  'M'.
DATA P_POS(02) TYPE N.
*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT  (03) TEXT-001.
SELECTION-SCREEN POSITION 9.
PARAMETERS: P_CAR LIKE ZTBM_OCN-BASE(2) OBLIGATORY.
SELECTION-SCREEN POSITION 12.
PARAMETERS: P_MAKTX LIKE MAKT-MAKTX.
SELECTION-SCREEN POSITION 60.
SELECTION-SCREEN COMMENT  (04) TEXT-002.
SELECTION-SCREEN POSITION 69.
PARAMETERS: P_ZYEAR LIKE ZTBM_OCN-ZYEAR(1) OBLIGATORY.
SELECTION-SCREEN POSITION 72.
PARAMETERS: P_YEAR(04).  "  LIKE MAKT-MAKTX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
SELECTION-SCREEN COMMENT  (06) TEXT-003.
SELECTION-SCREEN POSITION 9.
PARAMETERS: P_NATN LIKE ZTBM_OCN-NATN.
SELECTION-SCREEN POSITION 36.
SELECTION-SCREEN COMMENT  (05) TEXT-004.
SELECTION-SCREEN POSITION 44.
PARAMETERS: P_BASE LIKE ZTBM_OCN-BASE(08).
SELECTION-SCREEN POSITION 60.
SELECTION-SCREEN COMMENT  (06) TEXT-005.
SELECTION-SCREEN POSITION 69.
SELECT-OPTIONS: P_OCNO FOR ZTBM_OCN-OCNO  NO-EXTENSION
                                               NO INTERVALS.
*PARAMETERS: P_OCNO LIKE ZTBM_ABXOCNDT-OCNO.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK B1.


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
* INITIALIZATION
************************************************************************
INITIALIZATION.
  PERFORM INITIALIZATION.
************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN OUTPUT.
  PERFORM SCREEN_MODIFY.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXECUTE_PROCESS.

END-OF-SELECTION.
  CALL SCREEN 9000.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
*  P_CAR = 'DA'.
*  P_MAKTX = 'BONGSOO'.
*  P_ZYEAR = '3'.
*  P_YEAR = '2003'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
FORM SCREEN_MODIFY.
  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'P_MAKTX' OR 'P_YEAR'.
        SCREEN-INPUT = 0.
    ENDCASE.
    MODIFY SCREEN.
    CLEAR SCREEN.
  ENDLOOP.
ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
  DATA: LT_AOCN TYPE ZTBM_OCN OCCURS 0 WITH HEADER LINE.
  DATA: L_CAR(03),
        L_BASE(09),
        L_NATN(06),
        L_OCNO(05),
        L_INDEX(02) TYPE N,
        L_FIELD(20),
        L_OPTI(4),
        L_MOD TYPE I.
  FIELD-SYMBOLS <FS>.
  REFRESH: LT_AOCN, IT_AOCN. CLEAR: IT_AOCN, LT_AOCN.
  IF NOT P_OCNO-LOW IS INITIAL.
    REFRESH P_OCNO.
*    P_OCNO-LOW = L_OCNO.
    P_OCNO-HIGH = '9999'.
    P_OCNO-OPTION = 'BT'.
    P_OCNO-SIGN = 'I'.
    APPEND P_OCNO.
  ELSE.
    REFRESH P_OCNO. CLEAR P_OCNO.
  ENDIF.
  CONCATENATE: P_CAR '%' INTO L_CAR,
               '%' P_BASE INTO L_BASE,
               P_NATN '%' INTO L_NATN.

  SELECT *
       FROM ZTBM_OCN
       INTO TABLE LT_AOCN
       WHERE ZYEAR EQ P_ZYEAR
       AND   BASE  LIKE L_CAR
       AND   NATN  LIKE L_NATN
       AND   BASE  LIKE L_BASE
       AND   OCNO  IN   P_OCNO.
  IF SY-SUBRC EQ 0.
    SORT LT_AOCN BY OCNO.
    LOOP AT LT_AOCN.
      L_OCNO = IT_AOCN-OCNO = LT_AOCN-OCNO.
      WRITE: LT_AOCN-ZSDAT TO IT_AOCN-DATE.
*      IT_AOCN-DATE = LT_AOCN-ZSDAT.
      DO 60 TIMES.
        L_INDEX = SY-INDEX.
        L_MOD = L_INDEX MOD 12.
        CONCATENATE 'LT_AOCN-OP' L_INDEX INTO L_FIELD.
        ASSIGN (L_FIELD) TO <FS>.
        L_OPTI = <FS>.
        IF L_INDEX EQ 1.
          IF L_OPTI IS INITIAL.
            CONCATENATE IT_AOCN-OPTI '----' INTO IT_AOCN-OPTI.
          ELSE.
            CONCATENATE IT_AOCN-OPTI L_OPTI INTO IT_AOCN-OPTI.
          ENDIF.
        ELSE.
          IF L_OPTI IS INITIAL.
            CONCATENATE IT_AOCN-OPTI '----' INTO IT_AOCN-OPTI
                                            SEPARATED BY SPACE.
          ELSE.
            CONCATENATE IT_AOCN-OPTI L_OPTI INTO IT_AOCN-OPTI
                                            SEPARATED BY SPACE.
          ENDIF.
        ENDIF.
        IF L_MOD EQ '0'.
          APPEND IT_AOCN.
*          IT_AOCN-DATE = ' '.
          CLEAR IT_AOCN-OPTI.
        ENDIF.
        CLEAR: L_MOD, L_INDEX, L_FIELD, <FS>, L_OPTI.
      ENDDO.
      AT LAST.
        REFRESH P_OCNO. CLEAR P_OCNO.
        P_OCNO-LOW = L_OCNO.
        P_OCNO-HIGH = '9999'.
        P_OCNO-OPTION = 'BT'.
        P_OCNO-SIGN = 'I'.
        APPEND P_OCNO.
      ENDAT.

      CLEAR: IT_AOCN, LT_AOCN.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_9000 OUTPUT.
  SET PF-STATUS 'MAIN'.
  SET TITLEBAR '9000'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CREATE_ALV_GRID  OUTPUT
*&---------------------------------------------------------------------*
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
    PERFORM  BUILD_SORT USING: '1' 'OCNO' 'X'.
*-----> SET OBJECT
        CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
          EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
            I_STRUCTURE_NAME              = 'IT_AOCN'
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
            IT_OUTTAB                     = IT_AOCN[]
            IT_FIELDCATALOG               = GT_FIELDCAT[]
        IT_SORT                       =  GT_SORT
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
  REFRESH GT_FIELDCAT. CLEAR GT_FIELDCAT.
  PERFORM FIELD_CATALOG USING:
*    ' ' ''  'CHECK' ''  '01'  'X'    ''  ' '  '',
     '1' 'X' ''  'OCNO' ''  '04'  'OCN  '       ''  'C'  '',
     '2' ' ' ''  'OPTI' ''  '60'  'OPTION '     ''  'L'  '',
     '3' ' ' ''  'DATE' ''  '10'  'C/DATE  '    ''  'L'  ''.
*  CLEAR Z_FLAG.
ENDFORM.                    " BUILD_FIELDCAT
*---------------------------------------------------------------------*
*       FORM FIELD_CATALOG                                            *
*---------------------------------------------------------------------*
FORM FIELD_CATALOG USING P_POS P_KEY P_NO_OUT P_FIELDNAME P_DO_SUM
                         P_OUTPUTLEN P_REPTEXT_DDIC P_NO_ZERO P_JUST
                         P_CFIELDNAME.
  DATA : LS_FIELDCAT TYPE LVC_S_FCAT.

  CLEAR LS_FIELDCAT.
  LS_FIELDCAT-COL_POS       = P_POS.               "COLUMN
  LS_FIELDCAT-TABNAME       = 'IT_AOCN'.
  LS_FIELDCAT-CHECKBOX       = ' '.
*  LS_FIELDCAT-CTABNAME      = 'IT_LIST'.
  LS_FIELDCAT-KEY           = P_KEY .            "KEY
  LS_FIELDCAT-NO_OUT        = P_NO_OUT.          "
  LS_FIELDCAT-FIELDNAME     = P_FIELDNAME.       "
  LS_FIELDCAT-DO_SUM        = P_DO_SUM.          "
  LS_FIELDCAT-OUTPUTLEN     = P_OUTPUTLEN.       "
  LS_FIELDCAT-REPTEXT       = P_REPTEXT_DDIC.    "
  LS_FIELDCAT-NO_ZERO       = P_NO_ZERO.
*'L' LEFT 'R' RIGHT 'C' CENTERED
  LS_FIELDCAT-JUST          = P_JUST.        "
  LS_FIELDCAT-CFIELDNAME    = P_CFIELDNAME.  "
*LS_FIELDCAT-CURRENCY = 'KRW'.     "
*LS_FIELDCAT-QUANTITY = 'QUAN.     "
*  LS_FIELDCAT-DATATYPE = 'CURR'.       "
  APPEND LS_FIELDCAT TO  GT_FIELDCAT.
  CLEAR LS_FIELDCAT.

ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE OK_CODE.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_9000 INPUT.

ENDMODULE.                 " USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*&      Module  READ_DATA  INPUT
*&---------------------------------------------------------------------*
MODULE READ_DATA INPUT.
  PERFORM EXECUTE_PROCESS.

  CALL  METHOD GS_CUSTOM_CONTAINER->FREE.
  FREE  GS_CUSTOM_CONTAINER.
ENDMODULE.                 " READ_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
FORM BUILD_SORT USING    P_SPOS
                         P_FIELDNAME
                         P_UP.
    WA_SORT-SPOS = P_SPOS.	     "Sort sequence
    WA_SORT-FIELDNAME = P_FIELDNAME.    "ALV control: Field name of *
*                                        internal table field

    WA_SORT-UP  = P_UP.         "Single-character flag
*    GT_SORT-DOWN         "Single-character flag
*    GT_SORT-GROUP        "New group: Insert page feed, underlin
*    GT_SORT-SUBTOT       "Output subtotal
*    GT_SORT-COMP         "Single-character flag
*    GT_SORT-EXPA         "Single-character flag
*    GT_SORT-SELTEX       "Sort criterion
*    GT_SORT-OBLIGATORY   "Single-character flag
*    GT_SORT-LEVEL        "Natural number
*    GT_SORT-NOGT_SORT-OUT       "Single-character flag
*    GT_SORT-INTOPT       "Internal optimization (INTERNAL USE)
     APPEND WA_SORT TO GT_SORT.
ENDFORM.                    " BUILD_SORT
