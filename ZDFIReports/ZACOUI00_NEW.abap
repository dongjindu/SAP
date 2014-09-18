*---------------------------------------------------------------------*
*  Include           ZACOUI00_NEW
*---------------------------------------------------------------------*
*  Define Variables and Internal Tables & Subroutines for ALV
*---------------------------------------------------------------------*

* Define Variables and Internal Tables for ALV
TYPE-POOLS: SLIS.

INCLUDE <ICON>.
INCLUDE <SYMBOL>.

CONSTANTS: GC_FORMNAME_TOP_OF_PAGE TYPE SLIS_FORMNAME
                                   VALUE 'TOP_OF_PAGE',
           GC_VAR_SAVE       TYPE C VALUE  'A',
           GC_PF_STATUS_SET  TYPE SLIS_FORMNAME VALUE 'PF_STATUS_SET',
           GC_USER_COMMAND   TYPE SLIS_FORMNAME VALUE 'USER_COMMAND',
           GC_TVERS          TYPE CK_TVERS      VALUE '01'.

DATA: GT_LIST_TOP_OF_PAGE  TYPE SLIS_T_LISTHEADER,
      GT_LIST_TOP_OF_PAGE1 TYPE SLIS_T_LISTHEADER,
      GT_FIELDCAT          TYPE SLIS_T_FIELDCAT_ALV,
      GS_FIELDCAT          TYPE SLIS_FIELDCAT_ALV,
      GS_LAYOUT            TYPE SLIS_LAYOUT_ALV,
      GT_EVENTS            TYPE SLIS_T_EVENT,
      GT_SPECIALCOL        TYPE SLIS_T_SPECIALCOL_ALV,
      GS_SPECIALCOL        TYPE SLIS_SPECIALCOL_ALV.

DATA: GV_DEFAULT(1)  TYPE C,
      GS_VARIANT  LIKE DISVARIANT,
      GS_VARIANT1 LIKE DISVARIANT,
      GV_REPID    LIKE SY-REPID.

* for ALV Grid
DATA : GT_EXCLUDE   TYPE UI_FUNCTIONS,
       GT_EXCLUDE1  TYPE UI_FUNCTIONS,
       GS_PRINT     TYPE LVC_S_PRNT,
       CONTAINER    TYPE SCRFNAME VALUE 'G_CUSTOM_CONTAINER',
       CONTAINER1   TYPE SCRFNAME VALUE 'G_CUSTOM_CONTAINER1',
       GS_FCAT      TYPE LVC_S_FCAT,
       GT_FCAT      TYPE LVC_T_FCAT,
       GS_LAYO      TYPE LVC_S_LAYO,
       GS_FCAT1     TYPE LVC_S_FCAT,
       GT_FCAT1     TYPE LVC_T_FCAT,
       GS_LAYO1     TYPE LVC_S_LAYO,
       GS_F4        TYPE LVC_S_F4,
       GT_F4        TYPE LVC_T_F4,
       GS_SORT      TYPE LVC_S_SORT,
       GT_SORT      TYPE LVC_T_SORT,
       GS_SORT_ALV  TYPE SLIS_SORTINFO_ALV,
       GT_SORT_ALV  TYPE SLIS_T_SORTINFO_ALV.

DATA : OK_CODE      TYPE SY-UCOMM,
       SAVE_OK_CODE TYPE SY-UCOMM.

* Define internal tables &sstructures for Possible Entry
DATA : GS_VALUES TYPE SEAHLPRES,
       GT_FIELDS TYPE TABLE OF DFIES WITH HEADER LINE,
       GT_VALUES TYPE TABLE OF SEAHLPRES WITH HEADER LINE,
       GS_FIELDS TYPE DFIES,
       LS_F4     TYPE DDSHRETVAL,
       LS_MODI   TYPE LVC_S_MODI.

* define fields and field-symbols for data-update
FIELD-SYMBOLS : <F4TAB> TYPE LVC_T_MODI.

* reference to custom container: neccessary to bind ALV Control
CLASS CL_GUI_RESOURCES DEFINITION LOAD.
*CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : G_CUSTOM_CONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_GRID              TYPE REF TO CL_GUI_ALV_GRID,
       G_CUSTOM_CONTAINER1 TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_GRID1             TYPE REF TO CL_GUI_ALV_GRID.

DATA: GT_ROW   TYPE LVC_T_ROW,
      GS_ROW   TYPE LVC_S_ROW,
      GT_ROID  TYPE LVC_T_ROID.

* define internal table for BDC
DATA: GT_BDC TYPE TABLE OF BDCDATA    WITH HEADER LINE,
      GT_MSG TYPE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      GS_OPT LIKE CTU_PARAMS.

* for possible entry
DATA: BEGIN OF DYNPFIELDS OCCURS 3.
        INCLUDE STRUCTURE DYNPREAD.
DATA: END OF DYNPFIELDS.

DATA: DYNAME         TYPE PROGNAME,
      DYNUMB         TYPE SYCHAR04,
      EXC_EXCTAB     TYPE SLIS_T_EXTAB,
      POPUP_FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      F              TYPE SLIS_FIELDCAT_ALV,
      SELFIELD       TYPE SLIS_SELFIELD,
      EXITFIELD,
      COLOR_ACTIVE(3)  VALUE 'C50',
      TABIX LIKE SY-TABIX.

* possible entry for reason code
TYPES: BEGIN OF TY_ZTCOUM02,
         RGRP2 TYPE ZRGRP2,
         TEXT  TYPE ZRTEXT,
       END OF TY_ZTCOUM02.

TYPES: BEGIN OF TY_RSN,
         KZUST TYPE KZUST,
         TEXT  TYPE ZRTEXT,
       END OF TY_RSN.

DATA: GT_ZTCOUM02 TYPE TABLE OF TY_ZTCOUM02 WITH HEADER LINE,
      GT_RSN      TYPE TABLE OF TY_RSN      WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Build field catalog for ALV grid
*----------------------------------------------------------------------*
*      -->P_FNAME Field name
*      -->P_TXT   Column heading
*      -->P_LEN   Column width
*      -->P_TYPE  Data type
*----------------------------------------------------------------------*
FORM FILL_FIELD_CATEGORY USING P_POS   TYPE LVC_COLPOS
                               P_FNAME TYPE LVC_FNAME
                               P_TXT   TYPE LVC_TXTCOL
                               P_LEN   TYPE LVC_OUTLEN
                               P_TYPE  TYPE DATATYPE_D.

  CLEAR GS_FCAT.

  GS_FCAT-COL_POS   = P_POS.     " Column position
  GS_FCAT-FIELDNAME = P_FNAME.   " Field name
  GS_FCAT-COLTEXT   = P_TXT.     " Column heading
  GS_FCAT-OUTPUTLEN = P_LEN.     " Column width
  GS_FCAT-DATATYPE  = P_TYPE.    " Data type

  APPEND GS_FCAT TO GT_FCAT.

ENDFORM.                    " FILL_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*       Append excluding functions
*----------------------------------------------------------------------*
*      -->P_TABNAME   Table name
*      -->P_VALUE     Excluding value
*----------------------------------------------------------------------*
FORM APPEND_EXCLUDE_FUNCTIONS TABLES P_TABLE
                              USING P_VALUE.
  DATA LS_EXCLUDE TYPE UI_FUNC.

  LS_EXCLUDE = P_VALUE.
  APPEND LS_EXCLUDE TO P_TABLE.

ENDFORM.                    " APPEND_EXCLUDE_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*       Build field catalog for ALV list
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATEGORY USING P_FIELDNAME TYPE SLIS_FIELDNAME
                                P_KEY       TYPE C
                                P_TEXT      TYPE SCRTEXT_L
                                P_LEN       TYPE OUTPUTLEN
                                P_TYPE      TYPE DATATYPE_D.

  CLEAR GS_FIELDCAT.

  GS_FIELDCAT-FIELDNAME = P_FIELDNAME.
  GS_FIELDCAT-KEY       = P_KEY.
  GS_FIELDCAT-SELTEXT_L = P_TEXT.
  GS_FIELDCAT-OUTPUTLEN = P_LEN.
  GS_FIELDCAT-DATATYPE  = P_TYPE.

  APPEND GS_FIELDCAT TO GT_FIELDCAT.

ENDFORM.                    " BUILD_FIELD_CATEGORY
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
FORM SET_LAYOUT USING    P_EDIT_MODE
                         P_BOX_FNAME
                CHANGING CS_LAYO TYPE SLIS_LAYOUT_ALV.
  CS_LAYO-EDIT_MODE         = P_EDIT_MODE.
  CS_LAYO-NUMC_SUM          = 'X'.
  CS_LAYO-BOX_FIELDNAME     = P_BOX_FNAME.
  CS_LAYO-GROUP_BUTTONS     = 'X'.
  CS_LAYO-GROUP_CHANGE_EDIT = 'X'.
  CS_LAYO-COLTAB_FIELDNAME  = 'TABCOLOR'.
  CS_LAYO-COLWIDTH_OPTIMIZE = 'X'.

ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_EVENTS
*&---------------------------------------------------------------------*
FORM SET_EVENTS CHANGING CT_EVENTS TYPE SLIS_T_EVENT.
  DATA LS_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = CT_EVENTS.

  READ TABLE CT_EVENTS WITH KEY NAME =  SLIS_EV_TOP_OF_PAGE
                            INTO LS_EVENT.
  IF     SY-SUBRC = 0.
    MOVE   GC_FORMNAME_TOP_OF_PAGE TO LS_EVENT-FORM.
    APPEND LS_EVENT TO CT_EVENTS.
  ENDIF.

ENDFORM.                    " SET_EVENTS
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA LS_LINE TYPE SLIS_LISTHEADER.

  CLEAR LS_LINE.
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Date:'.
  CONCATENATE SY-DATUM+0(4) SY-DATUM+4(2) SY-DATUM+6(2)
         INTO LS_LINE-INFO SEPARATED BY '.'.

  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = 'User:'.
  LS_LINE-INFO = SY-UNAME.

  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-KEY  = ''.
  LS_LINE-INFO = ''.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    " COMMENT_BUILD
*&---------------------------------------------------------------------*
*&  FORM TOP_OF_LIST
*&---------------------------------------------------------------------*
FORM TOP_OF_LIST.
  NEW-LINE  NO-SCROLLING.
  WRITE : /1 'PGID: ', 8 SY-REPID, 33 'Date:', SY-DATUM,
          62 'Time:', SY-UZEIT, 92 SY-UNAME.

ENDFORM.                    "TOP_OF_LIST
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = GT_LIST_TOP_OF_PAGE.
ENDFORM.                    "TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  END_OF_PAGE
*&---------------------------------------------------------------------*
FORM ALV_EVENT_END_OF_PAGE.
  DATA LV_PAGE(10).

  NEW-LINE.
  ULINE.

  WRITE: SY-PAGNO TO LV_PAGE,
         /(120) LV_PAGE CENTERED.

ENDFORM.                    "END_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  REFRESH_FIELD
*&---------------------------------------------------------------------*
*       Refresh for display
*----------------------------------------------------------------------*
FORM REFRESH_FIELD.
  CALL METHOD G_GRID->SET_FRONTEND_FIELDCATALOG
       EXPORTING
         IT_FIELDCATALOG = GT_FCAT.

  CALL METHOD G_GRID->SET_FRONTEND_LAYOUT
       EXPORTING
         IS_LAYOUT = GS_LAYO.

  CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
  CALL METHOD CL_GUI_CFW=>FLUSH.

ENDFORM.                    " REFRESH_FIELD
*&---------------------------------------------------------------------*
*&      Form  CREATE_OBJECT
*&---------------------------------------------------------------------*
*       Create custom container control & instance
*----------------------------------------------------------------------*
FORM CREATE_OBJECT.
* create a custom container control for our alv control
  CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
          CONTAINER_NAME = CONTAINER.

* Create an Instance of ALV Control
  CREATE OBJECT G_GRID
        EXPORTING I_PARENT = G_CUSTOM_CONTAINER.

ENDFORM.                    " CREATE_OBJECT

* For BDC
*---------------------------------------------------------------------*
*       Form DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING P_DYNBEGIN P_NAME P_VALUE.
  CLEAR GT_BDC.

  IF P_DYNBEGIN = 'X'.
    GT_BDC-PROGRAM = P_NAME.
    GT_BDC-DYNPRO = P_VALUE.
    GT_BDC-DYNBEGIN = P_DYNBEGIN.
  ELSE.
    GT_BDC-FNAM = P_NAME.
    GT_BDC-FVAL = P_VALUE.
  ENDIF.

  APPEND GT_BDC.

ENDFORM.                    " DYNPRO
*---------------------------------------------------------------------*
*       Form GET_OPT                                                   *
*---------------------------------------------------------------------*
FORM GET_OPT USING P_MODE.
  CLEAR GS_OPT.

  GS_OPT-DISMODE  = P_MODE.
  GS_OPT-UPDMODE  = 'X'.
  GS_OPT-RACOMMIT = 'X'.
  GS_OPT-NOBINPT  = 'X'.

ENDFORM.                    " GET_OPT
*---------------------------------------------------------------------*
*       Form GET_MSG                                                   *
*---------------------------------------------------------------------*
FORM GET_MSG CHANGING P_MSG.
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
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.

ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  DATA_INPUT_ERROR
*&---------------------------------------------------------------------*
*       Error Message Display
*----------------------------------------------------------------------*
FORM DATA_INPUT_ERROR
        USING RR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
              RS_MOD_CELLS    TYPE LVC_S_MODI
              P_MSGTY         TYPE SYMSGTY
              P_MSGV1         TYPE SYMSGV
              P_FIELDNAME     TYPE LVC_FNAME.

* Error Message Display
  CALL METHOD RR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
   EXPORTING
      I_MSGID     = '0K'
      I_MSGNO     = '000'
      I_MSGTY     =  P_MSGTY
      I_MSGV1     =  P_MSGV1
      I_MSGV2     = ' '
      I_MSGV3     = ' '
      I_FIELDNAME =  P_FIELDNAME
      I_ROW_ID    =  RS_MOD_CELLS-ROW_ID.

ENDFORM.                    " DATA_INPUT_ERROR

*&---------------------------------------------------------------------*
*&      Form  POPUP_KALKA
*&---------------------------------------------------------------------*
*       Possible Enter for Costing types
*----------------------------------------------------------------------*
FORM POPUP_KALKA USING PA_KALKA     TYPE CK_KALKA
                       P_FIELDNAME TYPE DYNFNAM.
  DATA: BEGIN OF LT_TCK02 OCCURS 0,
           KALKA TYPE CK_KALKA,
           TXKLA TYPE CK_TXKLA,
         END OF LT_TCK02.

  DATA: BEGIN OF FIELDS_TAB OCCURS 1,
            KALKA TYPE CK_KALKA,
            TXKLA TYPE CK_TXKLA,
            COLOR(3),
         END OF FIELDS_TAB.

  CLEAR: DYNPFIELDS, DYNAME, DYNUMB, EXC_EXCTAB, POPUP_FIELDCAT,
         F, SELFIELD, EXITFIELD, COLOR_ACTIVE, TABIX, FIELDS_TAB.
  REFRESH: DYNPFIELDS, FIELDS_TAB.

  DYNPFIELDS-FIELDNAME = P_FIELDNAME.
  APPEND DYNPFIELDS.

  DYNAME = SY-REPID.
  DYNUMB = SY-DYNNR.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME             = DYNAME
            DYNUMB             = DYNUMB
            TRANSLATE_TO_UPPER = 'X'
       TABLES
            DYNPFIELDS         = DYNPFIELDS
       EXCEPTIONS
            OTHERS             = 9.

  CLEAR LT_TCK02.
  REFRESH LT_TCK02.

  SELECT KALKA TXKLA INTO TABLE LT_TCK02
    FROM TCK02
   WHERE SPRAS = SY-LANGU.

  DELETE LT_TCK02
    WHERE NOT ( KALKA+0(1) = 'U' OR
                KALKA+0(1) = 'M' OR
                KALKA+0(1) = 'B' OR
                KALKA+0(1) = 'R' ).

  SORT LT_TCK02 BY KALKA.

  F-REPTEXT_DDIC  = 'Costing Type'.
  F-FIELDNAME = 'KALKA'.
  F-OUTPUTLEN = 2.
  APPEND F TO POPUP_FIELDCAT.
  CLEAR F.

  F-REPTEXT_DDIC = 'Desc.'.
  F-FIELDNAME = 'TXKLA'.

  DESCRIBE FIELD FIELDS_TAB-TXKLA LENGTH F-OUTPUTLEN.
  APPEND F TO POPUP_FIELDCAT.

* Excluding-Table
  APPEND: '%SC ' TO EXC_EXCTAB,       " Search
          '%SC+' TO EXC_EXCTAB,       " Search+
          '&OUP' TO EXC_EXCTAB,       " Sort Up
          '&ODN' TO EXC_EXCTAB,       " Sort Dn
          '&ILT' TO EXC_EXCTAB,       " Filter
          '&OL0' TO EXC_EXCTAB.

* Popup
  TABIX = SY-TABIX.

  LOOP AT LT_TCK02.
    FIELDS_TAB-KALKA = LT_TCK02-KALKA.
    FIELDS_TAB-TXKLA = LT_TCK02-TXKLA.
    APPEND FIELDS_TAB.
    CLEAR FIELDS_TAB.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
       EXPORTING
            I_LINEMARK_FIELDNAME    = 'COLOR'
            I_TABNAME               = 'FIELDS_TAB'
            IT_FIELDCAT             = POPUP_FIELDCAT
            I_CALLBACK_USER_COMMAND = 'USER_COMMAND_POPUP_LIGHTS_N'
            I_CALLBACK_PROGRAM      = DYNAME
            IT_EXCLUDING            = EXC_EXCTAB
       IMPORTING
            ES_SELFIELD             = SELFIELD
            E_EXIT                  = EXITFIELD
       TABLES
            T_OUTTAB                = FIELDS_TAB.

  READ TABLE FIELDS_TAB INDEX TABIX.
  CLEAR FIELDS_TAB-COLOR.
  MODIFY FIELDS_TAB INDEX TABIX.

  IF EXITFIELD IS INITIAL.
    READ TABLE FIELDS_TAB INDEX SELFIELD-TABINDEX.
    PA_KALKA = FIELDS_TAB-KALKA.

    DYNPFIELDS-FIELDNAME = P_FIELDNAME.
    DYNPFIELDS-FIELDVALUE = FIELDS_TAB-KALKA.
    APPEND DYNPFIELDS.

    DYNAME = SY-REPID.
    DYNUMB = SY-DYNNR.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME     = DYNAME
              DYNUMB     = DYNUMB
         TABLES
              DYNPFIELDS = DYNPFIELDS.

  ENDIF.

ENDFORM.                    " POPUP_KALKA

*&---------------------------------------------------------------------*
*       Possible Enter for Reason
*----------------------------------------------------------------------*
FORM POPUP_RSN USING P_RSN       TYPE KZUST
                     P_FIELDNAME TYPE DYNFNAM.

  DATA: BEGIN OF FIELDS_TAB OCCURS 1,
           KZUST TYPE KZUST,
           TEXT  TYPE ZRTEXT,
           COLOR(3),
         END OF FIELDS_TAB.

  CLEAR: DYNPFIELDS, DYNAME, DYNUMB, EXC_EXCTAB, POPUP_FIELDCAT,
         F, SELFIELD, EXITFIELD, COLOR_ACTIVE, TABIX, FIELDS_TAB.
  REFRESH: DYNPFIELDS, FIELDS_TAB.

  DYNPFIELDS-FIELDNAME = P_FIELDNAME.
  APPEND DYNPFIELDS.

  DYNAME = SY-REPID.
  DYNUMB = SY-DYNNR.

  CALL FUNCTION 'DYNP_VALUES_READ'
       EXPORTING
            DYNAME             = DYNAME
            DYNUMB             = DYNUMB
            TRANSLATE_TO_UPPER = 'X'
       TABLES
            DYNPFIELDS         = DYNPFIELDS
       EXCEPTIONS
            OTHERS             = 9.

  PERFORM GET_REASON_FOR_POSSIBLE_ENTRY.

  F-REPTEXT_DDIC  = 'Reason'.
  F-FIELDNAME = 'KZUST'.
  F-OUTPUTLEN = 3.
  APPEND F TO POPUP_FIELDCAT.
  CLEAR F.

  F-REPTEXT_DDIC = 'Desc.'.
  F-FIELDNAME = 'TEXT'.
  DESCRIBE FIELD FIELDS_TAB-TEXT LENGTH F-OUTPUTLEN.
  APPEND F TO POPUP_FIELDCAT.

* Excluding-Table
  APPEND: '%SC ' TO EXC_EXCTAB,       " Search
          '%SC+' TO EXC_EXCTAB,       " Search+
          '&OUP' TO EXC_EXCTAB,       " Sort Up
          '&ODN' TO EXC_EXCTAB,       " Sort Dn
          '&ILT' TO EXC_EXCTAB,       " Filter
          '&OL0' TO EXC_EXCTAB.

* Popup
  TABIX = SY-TABIX.

  LOOP AT GT_RSN.
    FIELDS_TAB-KZUST = GT_RSN-KZUST.
    FIELDS_TAB-TEXT = GT_RSN-TEXT.
    APPEND FIELDS_TAB.
    CLEAR FIELDS_TAB.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
       EXPORTING
            I_LINEMARK_FIELDNAME    = 'COLOR'
            I_TABNAME               = 'FIELDS_TAB'
            IT_FIELDCAT             = POPUP_FIELDCAT
            I_CALLBACK_USER_COMMAND = 'USER_COMMAND_POPUP_LIGHTS_N'
            I_CALLBACK_PROGRAM      = DYNAME
            IT_EXCLUDING            = EXC_EXCTAB
       IMPORTING
            ES_SELFIELD             = SELFIELD
            E_EXIT                  = EXITFIELD
       TABLES
            T_OUTTAB                = FIELDS_TAB.

  READ TABLE FIELDS_TAB INDEX TABIX.
  CLEAR FIELDS_TAB-COLOR.
  MODIFY FIELDS_TAB INDEX TABIX.

  IF EXITFIELD IS INITIAL.
    READ TABLE FIELDS_TAB INDEX SELFIELD-TABINDEX.
    P_RSN = FIELDS_TAB-KZUST.

    DYNPFIELDS-FIELDNAME = P_FIELDNAME.
    DYNPFIELDS-FIELDVALUE = FIELDS_TAB-KZUST.
    APPEND DYNPFIELDS.

    DYNAME = SY-REPID.
    DYNUMB = SY-DYNNR.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
         EXPORTING
              DYNAME     = DYNAME
              DYNUMB     = DYNUMB
         TABLES
              DYNPFIELDS = DYNPFIELDS.

  ENDIF.

ENDFORM.                    " POPUP_RSN

*&---------------------------------------------------------------------*
*       Possible Enter for Reason in ALV Grid
*----------------------------------------------------------------------*
FORM F4_REASON USING E_FIELDNAME   TYPE LVC_FNAME.
* Fill internal table for possible entry
  CLEAR  : GT_VALUES, GT_FIELDS.
  REFRESH: GT_VALUES, GT_FIELDS.

  PERFORM GET_REASON_FOR_POSSIBLE_ENTRY.

  LOOP AT GT_RSN.
    GT_VALUES-STRING = GT_RSN-KZUST.
    APPEND GT_VALUES.

    GT_VALUES-STRING = GT_RSN-TEXT.
    APPEND GT_VALUES.
  ENDLOOP.

  CLEAR GT_FIELDS.
  REFRESH GT_FIELDS.

  GT_FIELDS-FIELDNAME = E_FIELDNAME.
  GT_FIELDS-POSITION  = 1.
  GT_FIELDS-INTLEN    = 3.
  GT_FIELDS-OUTPUTLEN = 3.
  GT_FIELDS-REPTEXT   = 'Reason Code'.
  APPEND GT_FIELDS.
  CLEAR GT_FIELDS.

  GT_FIELDS-FIELDNAME = 'TEXT'.
  GT_FIELDS-POSITION  = 2.
  GT_FIELDS-INTLEN    = 50.
  GT_FIELDS-OUTPUTLEN = 50.
  GT_FIELDS-REPTEXT = 'Desc.'.
  APPEND GT_FIELDS.
  CLEAR GT_FIELDS.

ENDFORM.                                                    " F4_REASON

*&---------------------------------------------------------------------*
*&      Form  GET_REASON_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*       Get Posssible entry data for reason code
*----------------------------------------------------------------------*
FORM GET_REASON_FOR_POSSIBLE_ENTRY.
  TYPES: BEGIN OF TY_T686D,
           KZUST TYPE KZUST,
           VTEXT TYPE VTEXT,
         END OF TY_T686D.

  DATA LT_T686D TYPE TABLE OF TY_T686D WITH HEADER LINE.

  CLEAR: GT_ZTCOUM02, LT_T686D, GT_RSN.
  REFRESH: GT_ZTCOUM02, LT_T686D, GT_RSN.

  SELECT RGRP2 TEXT INTO TABLE GT_ZTCOUM02
    FROM ZTCOUM02
   WHERE GRP1 <> 'Z'.

  LOOP AT GT_ZTCOUM02.
    GT_RSN-KZUST = GT_ZTCOUM02-RGRP2.
    GT_RSN-TEXT  = GT_ZTCOUM02-TEXT.
    APPEND GT_RSN.
    CLEAR GT_RSN.
  ENDLOOP.

  SELECT KZUST VTEXT
    INTO TABLE LT_T686D
    FROM T686D
   WHERE SPRAS = SY-LANGU
     AND KZUST LIKE 'X%'.

  LOOP AT LT_T686D.
    GT_RSN-KZUST = LT_T686D-KZUST.
    GT_RSN-TEXT  = LT_T686D-VTEXT.

    APPEND GT_RSN.
    CLEAR GT_RSN.
  ENDLOOP.

  SORT GT_RSN BY KZUST.

ENDFORM.                    " GET_REASON_FOR_POSSIBLE_ENTRY
*&---------------------------------------------------------------------*
*&      Form  ALV_VARIANT_F4
*&---------------------------------------------------------------------*
FORM ALV_VARIANT_F4 CHANGING P_VARI.
  DATA: RS_VARIANT LIKE DISVARIANT,
        LV_NOF4 TYPE C.

  CLEAR LV_NOF4.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'PA_VARI'.
      IF SCREEN-INPUT = 0.
        LV_NOF4 = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR RS_VARIANT.
  RS_VARIANT-REPORT   = SY-REPID.
  RS_VARIANT-USERNAME = SY-UNAME.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = RS_VARIANT
            I_SAVE     = 'A'
       IMPORTING
            ES_VARIANT = RS_VARIANT
       EXCEPTIONS
            OTHERS     = 1.

  IF SY-SUBRC = 0 AND LV_NOF4 = SPACE.
    P_VARI = RS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    " ALV_VARIANT_F4
