*&---------------------------------------------------------------------
*& Report      ZRIMNDFTPT
*&---------------------------------------------------------------------
*&  프로그램명 : 무환 Sample 물대(잡이익) 처리.
*&      작성자 : 강석봉 INFOLINK Ltd.
*&      작성일 : 2002.02.27
*&---------------------------------------------------------------------
*&   DESC.     :
*&
*&---------------------------------------------------------------------
*& [변경내용].                                                        *
*&
*&---------------------------------------------------------------------
REPORT ZRIMNDFTPT  MESSAGE-ID ZIM
                   LINE-SIZE 134
                   NO STANDARD PAGE HEADING.

INCLUDE: <ICON>.

*> Table Control Define
CONTROLS: TC_0040    TYPE TABLEVIEW USING SCREEN 0040.
CONTROLS: TC_0070    TYPE TABLEVIEW USING SCREEN 0070.

CLASS LCL_APPLICATION DEFINITION DEFERRED.
CLASS CL_GUI_CFW DEFINITION LOAD.

* CAUTION: MTREEITM is the name of the item structure which must
* be defined by the programmer. DO NOT USE MTREEITM!
TYPES: ITEM_TABLE_TYPE LIKE STANDARD TABLE OF MTREEITM
       WITH DEFAULT KEY.

DATA: G_APPLICATION TYPE REF TO LCL_APPLICATION,
      G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      G_TREE TYPE REF TO CL_GUI_COLUMN_TREE,
      G_OK_CODE TYPE SY-UCOMM.

* Fields on Dynpro 100
DATA: G_EVENT(30),
      G_NODE_KEY     TYPE TV_NODEKEY,
      G_NODE_KEY_OLD TYPE TV_NODEKEY,
      G_ITEM_NAME    TYPE TV_ITMNAME,
      G_HEADER_NAME  TYPE TV_HDRNAME.

*------------------------------------------------------------
*> Class 선언부.
*------------------------------------------------------------
CLASS LCL_APPLICATION DEFINITION.

  PUBLIC SECTION.
    METHODS:
      HANDLE_NODE_DOUBLE_CLICK
        FOR EVENT NODE_DOUBLE_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY,
      HANDLE_HEADER_CLICK
        FOR EVENT HEADER_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING HEADER_NAME,
      HANDLE_EXPAND_NO_CHILDREN
        FOR EVENT EXPAND_NO_CHILDREN
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY,
      HANDLE_ITEM_DOUBLE_CLICK
        FOR EVENT ITEM_DOUBLE_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_BUTTON_CLICK
        FOR EVENT BUTTON_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_LINK_CLICK
        FOR EVENT LINK_CLICK
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME,
      HANDLE_CHECKBOX_CHANGE
        FOR EVENT CHECKBOX_CHANGE
        OF CL_GUI_COLUMN_TREE
        IMPORTING NODE_KEY ITEM_NAME CHECKED.
ENDCLASS.

*---------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS LCL_APPLICATION IMPLEMENTATION.

  METHOD HANDLE_NODE_DOUBLE_CLICK.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
    G_EVENT = 'NODE_DOUBLE_CLICK'.
    G_NODE_KEY = NODE_KEY.
    CLEAR: G_ITEM_NAME, G_HEADER_NAME.
  ENDMETHOD.

  METHOD HANDLE_HEADER_CLICK.
    " this method handles header click event of the tree
    " control instance

    " show the name of the clicked header in a dynpro field
    G_EVENT = 'HEADER_CLICK'.
    G_HEADER_NAME = HEADER_NAME.
    CLEAR: G_NODE_KEY, G_ITEM_NAME.
  ENDMETHOD.

  METHOD  HANDLE_ITEM_DOUBLE_CLICK.
    " this method handles the item double click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    G_EVENT = 'ITEM_DOUBLE_CLICK'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.


    CLEAR G_HEADER_NAME.
  ENDMETHOD.

  METHOD  HANDLE_LINK_CLICK.
    " this method handles the link click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
    G_EVENT = 'LINK_CLICK'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.
    CLEAR G_HEADER_NAME.
  ENDMETHOD.

  METHOD  HANDLE_BUTTON_CLICK.
    " this method handles the button click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
    G_EVENT = 'BUTTON_CLICK'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.
    CLEAR G_HEADER_NAME.
  ENDMETHOD.

  METHOD  HANDLE_CHECKBOX_CHANGE.
    " this method handles the checkbox_change event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
    G_EVENT = 'CHECKBOX_CHANGE'.
    G_NODE_KEY = NODE_KEY.
    G_ITEM_NAME = ITEM_NAME.
    CLEAR  G_HEADER_NAME.
  ENDMETHOD.


  METHOD HANDLE_EXPAND_NO_CHILDREN.
    DATA: NODE_TABLE TYPE TREEV_NTAB,
          NODE TYPE TREEV_NODE,
          ITEM_TABLE TYPE ITEM_TABLE_TYPE,
          ITEM TYPE MTREEITM.

* show the key of the expanded node in a dynpro field
    G_EVENT = 'EXPAND_NO_CHILDREN'.
    G_NODE_KEY = NODE_KEY.
    CLEAR: G_ITEM_NAME, G_HEADER_NAME.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------
* Tables.
*----------------------------------------------------------------------
TABLES: SSCRFIELDS,
        ZTIMIMG00,
        ZTIMIMG11,
        ZVIVHD_IT,
        UF05A,
        BKPF,
        BSIS,
        ZTBKPF,
        ZTBSEG,
        ZTIV,
        ZTIVIT,
        ZTIVHST,
       *ZTIVHST,
        SPOP,
        ZTBL,
        ZTIDS,
        ZTCUCLIV,
        ZTCUCLIVIT,
        ZSIVHDIT,
        LFA1,
        T156,
        T001W,
        ZSIVHST.

*>>>>> LIV BAPI FUNCTION.
TABLES : BAPI_INCINV_CREATE_HEADER,
         BAPI_INCINV_CREATE_ITEM,
         BAPI_INCINV_CREATE_TAX,
         BAPIRET2.

TABLES : BAPI2017_GM_HEAD_01,
         BAPI2017_GM_CODE,
         BAPI2017_GM_HEAD_RET,
         BAPI2017_GM_ITEM_CREATE,
         BAPI2017_GM_SERIALNUMBER,
         BAPIMEPOHEADER,
         BAPIMEPOHEADERX.

SELECTION-SCREEN BEGIN OF BLOCK DBSEL WITH FRAME TITLE TEXT-005.
SELECT-OPTIONS:
  BUKRS FOR ZTIV-BUKRS
            NO-EXTENSION NO INTERVALS OBLIGATORY,
  HBLNO FOR ZTBL-ZFHBLNO,
  IDRNO FOR ZTIDS-ZFIDRNO,
  IDSDT FOR ZTIDS-ZFIDSDT,
  ZFETA FOR ZTBL-ZFETA,
  WERKS FOR ZTIVIT-WERKS,
  TXZ01 FOR ZTIVIT-TXZ01,
  RPTTY FOR ZTBL-ZFRPTTY,
  ERNAM FOR ZTIV-ERNAM,
  CDAT  FOR ZTIV-CDAT.
SELECTION-SCREEN END OF BLOCK DBSEL.

SELECTION-SCREEN BEGIN OF BLOCK STAT  WITH FRAME TITLE TEXT-010.
PARAMETERS: P_CKNO     AS CHECKBOX DEFAULT 'X',   ">전기대상.
            P_CKYES    AS CHECKBOX.               ">전기완료.
SELECTION-SCREEN END OF BLOCK STAT.

SELECTION-SCREEN BEGIN OF BLOCK LISTS WITH FRAME TITLE TEXT-020.
PARAMETERS: P_VARI LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK LISTS.

*----------------------------------------------------------------------
* Types.
*----------------------------------------------------------------------
TYPE-POOLS: SLIS.

TYPES:
* Data which are available on the basic list shown at startup
  BEGIN OF TY_BASICLIST,
           BOX(1)  TYPE C.
        INCLUDE STRUCTURE ZVIVHD_IT.
TYPES: ZFHBLNO LIKE ZTBL-ZFHBLNO,
       ZFETA   LIKE ZTBL-ZFETA,
       ZFVIA   LIKE ZTBL-ZFVIA,
       ZFIDRNO LIKE ZTIDS-ZFIDRNO,
       ZFIDSDT LIKE ZTIDS-ZFIDSDT,
       NAME2   LIKE LFA1-NAME1,
       KOSTL   LIKE ZTBL-KOSTL,
       PS_POSID LIKE ZTBL-PS_POSID,
       ZFUPT   LIKE ZTBL-ZFUPT,
       ZFAMT   LIKE ZTIVIT-ZFIVAMK,
       NAME1   LIKE T001W-NAME1,
END OF TY_BASICLIST,
* Detail data (reprogrammed display of a material document)
BEGIN OF TY_HEADER,
BOX(1)  TYPE C,
BUKRS   LIKE ZTIV-BUKRS,
ZFIVNO  LIKE ZTIV-ZFIVNO,
LIFNR   LIKE ZTIV-LIFNR,
NAME2   LIKE LFA1-NAME1,
ZFRPTTY LIKE ZTIV-ZFRPTTY,
*    TEXT(10),
ZFIDRNO LIKE ZTIDS-ZFIDRNO,
ZFIDSDT LIKE ZTIDS-ZFIDSDT,
ZFETA   LIKE ZTBL-ZFETA,
ZFHBLNO LIKE ZTBL-ZFHBLNO,
ZFBLNO  LIKE ZTIV-ZFBLNO,
KOSTL   LIKE ZTBL-KOSTL,
PS_POSID LIKE ZTBL-PS_POSID,
ZFUPT   LIKE ZTBL-ZFUPT,
ERNAM   LIKE ZTIV-ERNAM,
CDAT    LIKE ZTIV-CDAT,
END OF TY_HEADER,
BEGIN OF TY_LIST.
INCLUDE TYPE TY_HEADER.
TYPES:
ZFIVDNO LIKE ZTIVIT-ZFIVDNO,
TXZ01   LIKE ZTIVIT-TXZ01,
ZFIVAMK LIKE ZTIVIT-ZFIVAMK,
ZFKRW   LIKE ZTIVIT-ZFKRW,
WERKS   LIKE ZTIVIT-WERKS,
NAME1   LIKE T001W-NAME1,
NDFTX   LIKE ZTIVIT-NDFTX,
COLOR TYPE SLIS_T_SPECIALCOL_ALV,
END OF TY_LIST,
BEGIN OF TY_S_TOGGLELIST,
ZFIVNO  LIKE ZTIV-ZFIVNO,
MJAHR LIKE MSEG-MJAHR,
END OF TY_S_TOGGLELIST.


*----------------------------------------------------------------------
* Data.
*----------------------------------------------------------------------
DATA:
* Display variant for the basic list
  GS_VARIANT LIKE DISVARIANT,
* Data of the basic list
  IT_TAB      TYPE STANDARD TABLE OF TY_BASICLIST WITH HEADER LINE,

  IT_ZSIVIT   TYPE STANDARD TABLE OF ZSIVIT WITH HEADER LINE,
* Detail list
  HEADER      TYPE STANDARD TABLE OF TY_HEADER WITH HEADER LINE,
  LIST        TYPE STANDARD TABLE OF TY_LIST WITH HEADER LINE,
  POST_HEADER TYPE STANDARD TABLE OF TY_HEADER WITH HEADER LINE,
  POST_LIST   TYPE STANDARD TABLE OF TY_LIST WITH HEADER LINE,
* List of material documents used for toggling through the documents
* in the detail list.
  GT_TOGGLELIST TYPE STANDARD TABLE OF TY_S_TOGGLELIST,
  ALV_KEYINFO      TYPE SLIS_KEYINFO_ALV,
  ALV_REPID        LIKE SY-REPID,
  GF_CURRENT_LINE  LIKE SY-TABIX,
  ALV_LAYOUT       TYPE SLIS_LAYOUT_ALV,
  DETAIL,
  OPTION(1)       TYPE C,             " 공통 popup Screen에서 사?
  ANTWORT(1)      TYPE C,             " 공통 popup Screen에서 사?
  CANCEL_OPTION   TYPE C,             " 공통 popup Screen에서 사?
  TEXTLEN         TYPE I,             " 공통 popup Screen에서 사?
  W_TABIX          LIKE SY-TABIX,
  OK-CODE          LIKE SY-UCOMM,
  W_SUBRC          LIKE SY-SUBRC,
  W_ROW_MARK       TYPE C,
  W_ERR_CNT        TYPE I,
  W_COUNT          TYPE I,
  W_PROC_CNT       TYPE I,
  W_LINE           TYPE I,
  W_LINE1          TYPE I,
  LINE1(3)         TYPE N,             " 페이지당 LINE COUNT
  F(20)            TYPE C,             " Field Name Alias
  LINE             TYPE I,
  W_ZFIVDNO(5),
  L_CHK(1),
  G_PARAM_LINE     TYPE I,
  W_LOOPLINES      LIKE SY-LOOPC,      " loop counter
  W_ZFIVHST        LIKE ZTIVHST-ZFIVHST,
  OUT_TEXT(70)     TYPE C,
  W_ZFIDSDT        LIKE ZTIDS-ZFIDSDT.

DATA : MATERIALDOCUMENT  TYPE    BAPI2017_GM_HEAD_RET-MAT_DOC,
       MATDOCUMENTYEAR   TYPE    BAPI2017_GM_HEAD_RET-DOC_YEAR,
       GR_DOC            TYPE    BAPI2017_GM_HEAD_RET,
       P_INVOICE,
       P_CREDITMENO.

DATA BEGIN OF IT_TAB_UD OCCURS 0.
        INCLUDE STRUCTURE IT_TAB.
DATA ZFMARK  LIKE      ZSIVHDIT-ZFMARK.
DATA END   OF IT_TAB_UD.

DATA: FC_FLAT      TYPE SLIS_FIELDCAT_ALV OCCURS 0 WITH HEADER LINE.
DATA: FC_HIER      TYPE SLIS_FIELDCAT_ALV OCCURS 0 WITH HEADER LINE.

DATA : BEGIN OF IT_ERR_LIST OCCURS 0.
INCLUDE  STRUCTURE  BDCMSGCOLL.
DATA : ICON         LIKE BAL_S_DMSG-%_ICON,
       MESSTXT(255) TYPE C,
       ZFIVNO       LIKE ZTIV-ZFIVNO,
       ZFSEQ        TYPE I.
DATA : END OF IT_ERR_LIST.

DATA : BEGIN OF IT_ERR_GRP OCCURS 0,
       ZFIVNO       LIKE ZTIV-ZFIVNO,
       END OF IT_ERR_GRP.

DATA: BEGIN OF FIELD_CONTROL,
        TABNAME(10),                 "MKPF or MSEG
        FIELDNAME(10),               "fieldname
        SELECTION(1),                "status for selection screen
        SELECTION_POSITION(2),       "mandatory position on sel screen
        OUTPUT(1),                   "status for output list
        OUTPUT_POSITION(2),          "mandatory position on output list
        CQINDICATOR(1),              "does it need a UNIT/CURR field?
        CQFIELDNAME(10),             "what it's name
        COLOR(1),                    "does it need to be colorized?
        FIAUTH(1),                   "does it require auth. checks?
      END OF FIELD_CONTROL.

*> RUN TIME TABLE.
DATA: BEGIN OF RTT OCCURS 0.
        INCLUDE STRUCTURE FIELD_CONTROL.
DATA: END OF RTT.

DATA:   BEGIN OF RETURN OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   BAPIRET2.
DATA:   END   OF RETURN.

DATA:   BEGIN OF XZSIVHSTIT OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   ZSIVHSTIT.
DATA:   END   OF XZSIVHSTIT.

DATA:   BEGIN OF XBSEG OCCURS 0.   ">> RETURN 내역.
        INCLUDE STRUCTURE   ZSBSEG.
DATA:   END   OF XBSEG.

DATA : IT_ZTIVHSTIT  LIKE  ZTIVHSTIT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVHSTIT  LIKE  ZSIVHSTIT  OCCURS 0 WITH HEADER LINE.
DATA : IT_ZSIVHST    LIKE  ZSIVHST    OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------
* 매크로 선언.
*----------------------------------------------------------------------
DEFINE DEF_FC_BASIC.
*  FIELD_CONTROL-TABNAME            = &1.
*  FIELD_CONTROL-FIELDNAME          = &2,
*  FIELD_CONTROL-selection          = &3.
*  FIELD_CONTROL-selection_position = &4.
*  FIELD_CONTROL-output             = &5.
*  FIELD_CONTROL-output_position    = &6.
*  FIELD_CONTROL-cqindicator        = &7.
*  FIELD_CONTROL-cqfieldname        = &8.
*  FIELD_CONTROL-color              = &9.
*  FIELD_CONTROL-fiauth             = &10.
  FIELD_CONTROL = &1.
  APPEND  FIELD_CONTROL TO RTT.
END-OF-DEFINITION.

*----------------------------------------------------------------------
* Events
*----------------------------------------------------------------------
INITIALIZATION.
  PERFORM P2000_INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_VARI.
  PERFORM P2000_F4_FOR_VARIANT.

AT SELECTION-SCREEN.
  PERFORM P2000_VARIANT_CHECK.

START-OF-SELECTION.
  PERFORM P1000_DATA_SELECTION.

END-OF-SELECTION.
  LINE1 = 100.
  OUT_TEXT = '리스트를 구성 중입니다 %99999%%'.
  REPLACE '%99999%' WITH LINE1 INTO OUT_TEXT.
  PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE1.

  PERFORM P2000_BUILD_RUNTIMETABLE.
  PERFORM P2000_BULID_FIEDLCATALOG.
  PERFORM P2000_PROCESS_LIST.
  PERFORM P3000_OUTPUT_BASIC_LIST.

*&---------------------------------------------------------------------*
*&      Form  P2000_INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_INITIALIZATION.
  DATA: LS_VARIANT LIKE DISVARIANT.

  CLEAR : BUKRS.
  GET PARAMETER ID 'BUK' FIELD BUKRS-LOW.
  IF NOT BUKRS-LOW IS INITIAL.
    BUKRS-SIGN = 'I'.
    BUKRS-OPTION = 'EQ'.
    APPEND BUKRS.
  ENDIF.

  SET TITLEBAR 'ZIMN0'.

  LS_VARIANT-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
       EXPORTING
            I_SAVE     = 'A'
       CHANGING
            CS_VARIANT = LS_VARIANT
       EXCEPTIONS
            NOT_FOUND  = 2.

  IF SY-SUBRC = 0.
    P_VARI = LS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.                    " P2000_INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  P2000_F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_F4_FOR_VARIANT.
  DATA:
    LS_VARIANT_IMP LIKE DISVARIANT,
    LS_VARIANT_EXP LIKE DISVARIANT,
    L_EXIT(1)      TYPE C.

  LS_VARIANT_IMP-REPORT = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT = LS_VARIANT_IMP
            I_SAVE     = 'A'
       IMPORTING
            E_EXIT     = L_EXIT
            ES_VARIANT = LS_VARIANT_EXP
       EXCEPTIONS
            NOT_FOUND  = 2.

  IF SY-SUBRC = 2.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    IF L_EXIT IS INITIAL.
      P_VARI = LS_VARIANT_EXP-VARIANT.
    ENDIF.
  ENDIF.

ENDFORM.                    " P2000_F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  P2000_VARIANT_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_VARIANT_CHECK.
  CLEAR GS_VARIANT.
  GS_VARIANT-REPORT = SY-REPID.

  IF NOT P_VARI IS INITIAL.
    GS_VARIANT-VARIANT = P_VARI.
    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
         EXPORTING
              I_SAVE     = 'A'
         CHANGING
              CS_VARIANT = GS_VARIANT.
  ENDIF.

ENDFORM.                    " P2000_VARIANT_CHECK
*&---------------------------------------------------------------------*
*&      Form  P1000_DATA_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P1000_DATA_SELECTION.

  RANGES: LR_NDFTX       FOR  ZTIVIT-NDFTX.

  DATA:   LS_BASICLIST   TYPE TY_BASICLIST,
          LS_HEADER      TYPE TY_HEADER,
          LS_LIST        TYPE TY_LIST.

  LINE1 = 20.
  OUT_TEXT = '데이타를 검색 중입니다 %99999%%'.
  REPLACE '%99999%' WITH LINE1 INTO OUT_TEXT.
  PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE1.

  IF P_CKYES IS INITIAL AND P_CKNO IS INITIAL.
    MESSAGE S213(ZIM1).  EXIT.
  ENDIF.

*">전기대상.
  IF P_CKNO EQ 'X'.
    " 미입고대상.
    CLEAR : LR_NDFTX.
    MOVE: 'I'      TO   LR_NDFTX-SIGN,
          'EQ'     TO   LR_NDFTX-OPTION,
          'N'      TO   LR_NDFTX-LOW.
    APPEND LR_NDFTX.
    " 분할입고대상.
    CLEAR : LR_NDFTX.
    MOVE: 'I'      TO   LR_NDFTX-SIGN,
          'EQ'     TO   LR_NDFTX-OPTION,
          'P'      TO   LR_NDFTX-LOW.
    APPEND LR_NDFTX.
  ENDIF.
*">전기완료 리스트.
  IF P_CKYES EQ 'X'.
    " 입고완료.
    CLEAR : LR_NDFTX.
    MOVE: 'I'      TO   LR_NDFTX-SIGN,
          'EQ'     TO   LR_NDFTX-OPTION,
          'Y'      TO   LR_NDFTX-LOW.
    APPEND LR_NDFTX.
    " 분할입고대상.
    CLEAR : LR_NDFTX.
    MOVE: 'I'      TO   LR_NDFTX-SIGN,
          'EQ'     TO   LR_NDFTX-OPTION,
          'P'      TO   LR_NDFTX-LOW.
    APPEND LR_NDFTX.
  ENDIF.

  SELECT *
         INTO CORRESPONDING FIELDS OF TABLE IT_TAB
         FROM ZTBL AS B INNER JOIN ZVIVHD_IT AS I
         ON  B~MANDT  = I~MANDT
         AND B~ZFBLNO = I~ZFBLNO
         WHERE I~BUKRS    IN BUKRS
           AND B~ZFHBLNO  IN HBLNO
           AND B~ZFETA    IN ZFETA
           AND B~ZFRPTTY  IN RPTTY
           AND I~WERKS    IN WERKS
           AND I~TXZ01    IN TXZ01
           AND I~ERNAM    IN ERNAM
           AND I~CDAT     IN CDAT
           AND I~ZFGRST   IN LR_NDFTX
           AND I~ZFPOTY   EQ 'S'    ">무환 Sample만.
           AND I~ZFCUST   IN ('Y', 'X').

*">전기대상.
*  IF P_CKNO EQ 'X'.
*   SELECT *
*       APPENDING CORRESPONDING FIELDS OF TABLE IT_TAB
*       FROM ZTBL AS B INNER JOIN ZVIVHD_IT AS I
*       ON  B~MANDT  = I~MANDT
*       AND B~ZFBLNO = I~ZFBLNO
*       WHERE I~BUKRS    IN BUKRS
*         AND B~ZFHBLNO  IN HBLNO
*         AND B~ZFETA    IN ZFETA
*         AND B~ZFRPTTY  IN RPTTY
*         AND I~WERKS    IN WERKS
*         AND I~TXZ01    IN TXZ01
*         AND I~ERNAM    IN ERNAM
*         AND I~CDAT     IN CDAT
*         AND I~ZFGRST   IN ('N', 'P')
*         AND I~ZFPOTY   EQ 'S'    ">무환 Sample만.
*         AND I~ZFCUST   IN ('Y', 'X').
* ENDIF.

  LOOP AT IT_TAB INTO LS_BASICLIST.
    W_TABIX = SY-TABIX.

    IF LS_BASICLIST-ZFCUST EQ 'Y'.
      SELECT SINGLE ZFIDRNO ZFIDSDT
             INTO  (LS_BASICLIST-ZFIDRNO, LS_BASICLIST-ZFIDSDT)
             FROM   ZTIDS
             WHERE  ZFIVNO  EQ   LS_BASICLIST-ZFIVNO
*              WHERE  ZFBLNO  EQ   LS_BASICLIST-ZFBLNO
*              AND    ZFCLSEQ EQ
*                    ( SELECT ZFCLSEQ FROM ZTCUCLIV
*                      WHERE  ZFIVNO  EQ   LS_BASICLIST-ZFIVNO )
             AND    ZFIDRNO IN  IDRNO
             AND    ZFIDSDT IN  IDSDT.
      IF SY-SUBRC NE 0.
        DELETE IT_TAB INDEX W_TABIX. " FROM LS_BASICLIST.
        CONTINUE.
      ENDIF.
    ENDIF.

    SELECT SINGLE NAME1 INTO LS_BASICLIST-NAME1
           FROM   T001W
           WHERE  WERKS EQ LS_BASICLIST-WERKS.

    SELECT SINGLE NAME1 INTO LS_BASICLIST-NAME2
           FROM   LFA1
           WHERE  LIFNR EQ LS_BASICLIST-LIFNR.

    MODIFY IT_TAB INDEX W_TABIX FROM LS_BASICLIST.

  ENDLOOP.

  DESCRIBE TABLE IT_TAB LINES W_LINE.
  IF W_LINE EQ 0.
    MESSAGE S738.
  ENDIF.

ENDFORM.                    " P1000_DATA_SELECTION
*&---------------------------------------------------------------------*
*&      Form  P3000_OUTPUT_BASIC_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P3000_OUTPUT_BASIC_LIST.
  CONSTANTS:
    S TYPE C VALUE SPACE,
    X TYPE C VALUE 'X'.
  DATA:
    L_REPID   LIKE SY-REPID,
    LT_FC     TYPE SLIS_T_FIELDCAT_ALV,
    LS_FC     TYPE SLIS_FIELDCAT_ALV,
    LS_LAYOUT TYPE SLIS_LAYOUT_ALV,
    LS_PRINT  TYPE SLIS_PRINT_ALV,
    LS_SORT   TYPE SLIS_SORTINFO_ALV,
    LT_SORT   TYPE SLIS_T_SORTINFO_ALV.

  L_REPID                     = SY-REPID.
  ALV_REPID                   = SY-REPID.
  LS_PRINT-NO_PRINT_LISTINFOS = 'X'.
  LS_LAYOUT-GROUP_CHANGE_EDIT = 'X'.
  LS_LAYOUT-BOX_FIELDNAME     = 'BOX'.


  LS_LAYOUT-COLTAB_FIELDNAME = 'COLOR'.

  ALV_KEYINFO-HEADER01 = 'ZFIVNO'.
  ALV_KEYINFO-HEADER02 = 'LIFNR'.
  ALV_KEYINFO-HEADER03 = 'ZFBLNO'.
  ALV_KEYINFO-HEADER04 = 'ZFHBLNO'.
  ALV_KEYINFO-HEADER05 = 'ZFIDRNO'.
  ALV_KEYINFO-ITEM01   = 'ZFIVNO'.
  ALV_KEYINFO-ITEM02   = 'LIFNR'.
  ALV_KEYINFO-ITEM03   = 'ZFBLNO'.
  ALV_KEYINFO-ITEM04   = 'ZFHBLNO'.
  ALV_KEYINFO-ITEM05   = 'ZFIDRNO'.

* Sorting information and group change
* CLEAR ls_sort.
* ls_sort-fieldname = 'ZFBLNO'.
* ls_sort-spos      = 1.
* ls_sort-down      = 'X'.
* APPEND ls_sort TO lt_sort.

* CLEAR ls_sort.
* ls_sort-fieldname = 'ZFIVNO'.
* ls_sort-spos      = 2.
* ls_sort-up        = 'X'.
* ls_sort-group     = 'UL'.
* APPEND ls_sort TO lt_sort.

* CLEAR ls_sort.
* ls_sort-fieldname = 'ZFIVDNO'.
* ls_sort-spos      = 3.
* ls_sort-up        = 'X'.
*  ls_sort-group     = 'UL'.
* APPEND ls_sort TO lt_sort.

* CLEAR ls_sort.
* ls_sort-fieldname = 'TXZ01'.
* ls_sort-spos      = 4.
* ls_sort-up        = 'X'.
* APPEND ls_sort TO lt_sort.

* Field catalog
* DEFINE def_fc_basic.
*   clear ls_fc.
*   ls_fc-fieldname   = &1.
*   ls_fc-tabname     = 'BELEGE'. "Compatibility with old field catalogs
*   ls_fc-ref_tabname = &2.
*   ls_fc-qfieldname  = &3.
*   ls_fc-key         = &4.
*   ls_fc-hotspot     = &5.
*   ls_fc-seltext_s   = &6.
*   ls_fc-seltext_m   = &7.
*   ls_fc-seltext_l   = &8.
*    append ls_fc to lt_fc.
*  END-OF-DEFINITION.

* def_fc_basic 'MBLNR' 'MKPF' s       x x text-031 text-032 text-033.
* def_fc_basic 'MJAHR' 'MKPF' s       x s text-034 text-035 text-036.
* def_fc_basic 'ZEILE' 'MSEG' s       x s s        s        s.
* def_fc_basic 'MATNR' 'MSEG' s       s s s        s        s.
* def_fc_basic 'BWART' 'MSEG' s       s s s        s        s.
* def_fc_basic 'WERKS' 'MSEG' s       s s s        s        s.
* def_fc_basic 'LGORT' 'MSEG' s       s s s        s        s.
* def_fc_basic 'SHKZG' 'MSEG' s       s s s        s        s.
* def_fc_basic 'MENGE' 'MSEG' 'MEINS' s s s        s        s.
* def_fc_basic 'MEINS' 'MSEG' s       s s s        s        s.
* def_fc_basic 'BUDAT' 'MKPF' s       s s s        s        s.
* def_fc_basic 'USNAM' 'MKPF' s       s s s        s        s.
* def_fc_basic 'SMBLN' 'MSEG' s       s x text-037 text-038 text-039.
* def_fc_basic 'SJAHR' 'MSEG' s       s s text-041 text-042 text-043.
* def_fc_basic 'SMBLP' 'MSEG' s       s s s        s        s.

* CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM             = L_REPID
      I_CALLBACK_PF_STATUS_SET       = 'STATUS_SET'
      I_CALLBACK_USER_COMMAND        = 'USER_COMMAND_MAIN'
      IS_LAYOUT                      = LS_LAYOUT
*     it_fieldcat                    = lt_fc
      IT_FIELDCAT                    = FC_HIER[]
*     it_sort                        = lt_sort
      I_DEFAULT                      = 'X'
      I_SAVE                         = 'A'
      IS_VARIANT                     = GS_VARIANT
      IS_PRINT                       = LS_PRINT
      I_TABNAME_HEADER               = 'HEADER'
      I_TABNAME_ITEM                 = 'LIST'
      IS_KEYINFO                     = ALV_KEYINFO
    TABLES
*      t_outtab                       = belege.
      T_OUTTAB_HEADER                = HEADER[]
      T_OUTTAB_ITEM                  = LIST[]
    EXCEPTIONS
      PROGRAM_ERROR                  = 1
      OTHERS                         = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  ALV_LAYOUT  =  LS_LAYOUT.

ENDFORM.                    " P3000_OUTPUT_BASIC_LIST
*&---------------------------------------------------------------------*
*&      Form  P2000_BULID_FIEDLCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BULID_FIEDLCATALOG.
  REFRESH FC_FLAT.
  LOOP AT RTT.
    CLEAR FC_FLAT.
    FC_FLAT-FIELDNAME     = RTT-FIELDNAME.
    FC_FLAT-REF_TABNAME   = RTT-TABNAME.
    FC_FLAT-REF_FIELDNAME = RTT-FIELDNAME.
    CASE RTT-CQINDICATOR.
      WHEN 'C'. FC_FLAT-CFIELDNAME = RTT-CQFIELDNAME.
      WHEN 'Q'. FC_FLAT-QFIELDNAME = RTT-CQFIELDNAME.
    ENDCASE.
    FC_FLAT-TABNAME       = 'LIST'.
    IF RTT-OUTPUT_POSITION = '00'.
      FC_FLAT-NO_OUT        = 'X'.
    ENDIF.
    APPEND FC_FLAT.

* Some fields have apendixes (e.g. MATNR is followed by MAKTX)
*    CLEAR fc_flat.
*    CASE rtt-fieldname.
*      WHEN 'MATNR'.
*        fc_flat-fieldname   = 'MAKTX'.
*        fc_flat-tabname     = 'LIST'.
*        fc_flat-ref_tabname = 'MAKT'.
*      WHEN 'WERKS'.
*        fc_flat-fieldname   = 'NAME1'.
*        fc_flat-tabname     = 'LIST'.
*        fc_flat-ref_tabname = 'T001W'.
*      WHEN 'BWART'.
*        fc_flat-fieldname   = 'BTEXT'.
*        fc_flat-tabname     = 'LIST'.
*        fc_flat-ref_tabname = 'T156T'.
*     WHEN 'NPLNR'.                                              "215929
*       fc_flat-fieldname   = 'VORNR'.                           "215929
*       fc_flat-tabname     = 'LIST'.                            "215929
*       fc_flat-ref_tabname = 'RESB'.                            "215929
*     WHEN OTHERS.
*       CONTINUE.
*   ENDCASE.
*   fc_flat-no_out = 'X'.
*   APPEND fc_flat.
  ENDLOOP.
* For the hierarchic ALV, the header fields are extracted
* and activated.
  REFRESH FC_HIER.
  LOOP AT FC_FLAT.
    FC_HIER = FC_FLAT.
    CASE FC_FLAT-FIELDNAME.
      WHEN 'ZFIVNO' OR 'LIFNR' OR 'NAME2' OR 'ZFRPTTY' OR
           'ZFIDRNO' OR 'ZFIDSDT' OR 'ZFETA' OR 'ZFHBLNO' OR
           'ZFBLNO' OR 'ERNAM' OR 'CDAT' OR 'KOSTL' OR
           'PS_POSID'.
*                       OR 'ZFUPT'.
        FC_HIER-TABNAME = 'HEADER'.
        FC_HIER-NO_OUT = ' '.
    ENDCASE.
    APPEND FC_HIER.
  ENDLOOP.

ENDFORM.                    " P2000_BULID_FIEDLCATALOG

*&---------------------------------------------------------------------*
*&      Form  P2000_BUILD_RUNTIMETABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_BUILD_RUNTIMETABLE.

  DEF_FC_BASIC 'ZTIV      ZFIVNO    +01+01           + '.
  DEF_FC_BASIC 'ZTIVIT    ZFIVDNO   +02+02           +X'.
  DEF_FC_BASIC 'ZTIV      LIFNR     +03+03           +X'.
  DEF_FC_BASIC 'LFA1      NAME2     +04+04           +X'.
  DEF_FC_BASIC 'ZTIVIT    TXZ01     +05+05           +X'.
  DEF_FC_BASIC 'ZTIVIT    ZFIVAMK   +06+06CZFKRW     +X'.
  DEF_FC_BASIC 'ZTIVIT    ZFKRW     +07+07           +X'.
  DEF_FC_BASIC 'ZTIVIT    WERKS     +08+08           +X'.
  DEF_FC_BASIC 'T001W     NAME1     +08+08           +X'.
*  DEF_FC_BASIC 'ZTIV      ZFRPTTY   +00+00           +X'.
*  DEF_FC_BASIC 'ZTBL      ZFETA     -00-00           -X'.
  DEF_FC_BASIC 'ZTBL      ZFBLNO    +11+11           -X'.
  DEF_FC_BASIC 'ZTBL      ZFHBLNO   +12+12           -X'.
  DEF_FC_BASIC 'ZTBL      KOSTL     +13+13           -X'.
*  DEF_FC_BASIC 'ZTBL      PS_POSID  +00+00           -X'.
*  DEF_FC_BASIC 'ZTBL      ZFUPT     +00+00           -X'.
*  DEF_FC_BASIC 'ZTIV      ERNAM     +03+03'.
*  DEF_FC_BASIC 'ZTIV      CDAT      +03+03'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  P2000_PROCESS_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_PROCESS_LIST.
  DATA: LINECOLOR TYPE SLIS_SPECIALCOL_ALV OCCURS 0 WITH HEADER LINE.
  DATA: FNAME(20).
  FIELD-SYMBOLS: <F>.

  REFRESH : HEADER, LIST.

  LOOP AT IT_TAB.
* Color information and signs
    REFRESH LINECOLOR.
    LOOP AT RTT WHERE COLOR <> SPACE.
      CONCATENATE 'LIST-' RTT-FIELDNAME INTO FNAME.
      LINECOLOR-FIELDNAME = RTT-FIELDNAME.
      LINECOLOR-COLOR-INT = 0.
      CASE  RTT-FIELDNAME.
        WHEN 'ZFIVAMK' OR 'ZFKRW'.
          LINECOLOR-COLOR-COL = 5.
          APPEND LINECOLOR.
        WHEN 'ZFIVNO' OR 'ZFIVDNO'.
          LINECOLOR-COLOR-COL = 1.
          APPEND LINECOLOR.
      ENDCASE.
*       APPEND linecolor.
    ENDLOOP.

    MOVE-CORRESPONDING:IT_TAB TO LIST,
                       IT_TAB TO HEADER.
    COLLECT HEADER.
*     APPEND  LS_LIST   TO   LIST.

    LIST-COLOR[] = LINECOLOR[].
    APPEND LIST.
  ENDLOOP.

  SORT LIST BY ZFIVNO ZFIVDNO TXZ01 ZFBLNO.

ENDFORM.                    " P2000_PROCESS_LIST

************************************************************************
* Status
************************************************************************
FORM STATUS_SET USING EXTAB TYPE SLIS_T_EXTAB.

  SET TITLEBAR  'ZIMN0'.

  APPEND 'ARCHIVE' TO EXTAB.

  IF DETAIL EQ 'X'.
    APPEND 'DETAIL' TO EXTAB.
    APPEND 'POST'   TO EXTAB.
    APPEND 'UNDO'   TO EXTAB.
    APPEND 'ITEM'   TO EXTAB.
  ENDIF.

  SET PF-STATUS 'STANDARD' EXCLUDING EXTAB.

ENDFORM.

************************************************************************
* Callback for basic list
************************************************************************
FORM USER_COMMAND_MAIN USING R_UCOMM LIKE SY-UCOMM
                        RS_SELFIELD TYPE SLIS_SELFIELD.
  DATA:
    LS_RETURN TYPE TY_BASICLIST.

** Read line. Simple because this list only has a flat table.
*  READ TABLE belege INTO ls_belege
*             INDEX rs_selfield-tabindex.
*  CHECK NOT ls_belege-mblnr IS INITIAL.
  IF RS_SELFIELD-TABNAME = '1'.
    RS_SELFIELD-TABNAME = 'HEADER'.
  ENDIF.

  CASE R_UCOMM.
    WHEN 'DETAIL'.
      PERFORM P2000_DETAIL_LIST.
    WHEN 'ITEM'.   ">잡이익 금액 변경.
      REFRESH : IT_TAB_UD, POST_HEADER.

      LOOP AT HEADER WHERE BOX EQ 'X'.
        LOOP AT IT_TAB WHERE ZFIVNO EQ HEADER-ZFIVNO.
          IF IT_TAB-NDFTX EQ 'X'.
            MESSAGE S416(ZIM1).
            CONTINUE.
          ENDIF.
          MOVE-CORRESPONDING IT_TAB TO IT_TAB_UD.
          IT_TAB_UD-ZFAMT = IT_TAB_UD-ZFIVAMK.
          APPEND IT_TAB_UD.
          ADD 1 TO W_LINE.
        ENDLOOP.
        MOVE-CORRESPONDING HEADER TO POST_HEADER.
        APPEND POST_HEADER.
      ENDLOOP.

      CASE W_LINE.
        WHEN 0.
          MESSAGE S003(OK). EXIT.
        WHEN 1.
        WHEN OTHERS.
*             MESSAGE S035(0K).   EXIT.
      ENDCASE.

      SPOP-TITEL = '과세가격-원화 변경확인'.
      SPOP-TEXTLINE1 = '해당 항목들의 과세가격을 변경합니다.'.
      SPOP-TEXTLINE2 = '변경하시겠습니까?'.

      CALL SCREEN 0040 STARTING AT  01 1
                       ENDING   AT  90 17.

      IF ANTWORT EQ 'Y'.
        DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
        IF W_LINE GT 0.
          CALL SCREEN 0030 STARTING AT  01   1
                           ENDING   AT  102 16.
        ELSE.
          MESSAGE S953.
        ENDIF.
      ELSE.
        MESSAGE S957.   EXIT.
      ENDIF.
      RS_SELFIELD-REFRESH = 'X'.
*       SET SCREEN 0.   LEAVE SCREEN.

    WHEN 'FIDC'.   ">회계문서 조회.
      CLEAR : W_LINE.
      LOOP AT HEADER WHERE BOX EQ 'X'.
        ADD 1 TO W_LINE.
        MOVE-CORRESPONDING HEADER TO LS_RETURN.
      ENDLOOP.
      CASE W_LINE.
        WHEN 0.
          IF RS_SELFIELD-TABNAME EQ 'HEADER'.
            READ TABLE HEADER INDEX RS_SELFIELD-TABINDEX.
          ELSEIF RS_SELFIELD-TABNAME EQ 'LIST'.
            READ TABLE LIST INDEX RS_SELFIELD-TABINDEX.
          ENDIF.
          IF SY-SUBRC NE 0.
            MESSAGE S003(OK). EXIT.
          ENDIF.
          IF RS_SELFIELD-TABNAME EQ 'HEADER'.
            MOVE-CORRESPONDING HEADER TO LS_RETURN.
          ELSEIF RS_SELFIELD-TABNAME EQ 'LIST'.
            MOVE-CORRESPONDING LIST   TO LS_RETURN.
          ENDIF.
        WHEN 1.
        WHEN OTHERS.
          MESSAGE S035(0K).   EXIT.
      ENDCASE.

      SELECT MAX( ZFIVHST ) INTO W_ZFIVHST
             FROM  ZTIVHST
             WHERE ZFIVNO EQ  LS_RETURN-ZFIVNO
             AND   ZFGAIN EQ 'X'
             AND ( CMBLNR IS NULL
             OR    CMBLNR EQ SPACE ).

      SELECT SINGLE * FROM ZTIVHST
             WHERE ZFIVNO  EQ LS_RETURN-ZFIVNO
             AND   ZFIVHST EQ W_ZFIVHST.
      IF SY-SUBRC NE 0.
*          MESSAGE E414(ZIM1) WITH LS_RETURN-ZFIVNO W_ZFIVHST.
        MESSAGE S409(ZIM1).   EXIT.
      ENDIF.

      SELECT SINGLE * FROM ZTIV
             WHERE  ZFIVNO EQ LS_RETURN-ZFIVNO.

      PERFORM FIDOC_SHOW USING ZTIV-BUKRS
                               ZTIVHST-MJAHR
                               ZTIVHST-MBLNR.

    WHEN 'POST' OR 'UNDO'.   ">비용전기.
      REFRESH : POST_LIST, POST_HEADER.
      CLEAR : W_LINE, POST_LIST, POST_HEADER.
      LOOP AT HEADER WHERE BOX EQ 'X'.
        ADD 1 TO W_LINE.
        LOOP AT LIST WHERE ZFIVNO EQ HEADER-ZFIVNO.
          MOVE-CORRESPONDING LIST TO POST_LIST.
          APPEND POST_LIST.
        ENDLOOP.
        MOVE-CORRESPONDING HEADER TO POST_HEADER.
        APPEND POST_HEADER.
      ENDLOOP.
      IF W_LINE EQ 0.
        MESSAGE S003(OK). EXIT.
      ENDIF.
      IF W_LINE GT 1.
         MESSAGE S965.    EXIT.
      ENDIF.

      CLEAR : ANTWORT.
      IF R_UCOMM EQ 'POST'.
         PERFORM P4000_GET_GR_INIVAL.
      ELSE.
         PERFORM P4000_GET_GR_INIT_VALUE.
      ENDIF.

      LOOP AT POST_HEADER.
*-----------------------------------------------------------------------
*>> LOCK OBJECT.
        CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIV'
              EXPORTING
                  ZFIVNO                =     POST_HEADER-ZFIVNO
              EXCEPTIONS
                  OTHERS                = 1.

        IF SY-SUBRC NE 0.         "> ERROR 발생시..
           PERFORM  P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST
                                        USING   'E'
                                                POST_HEADER-ZFIVNO.
           ADD    1    TO    W_ERR_CNT.
           CONTINUE.
        ENDIF.

*-----------------------------------------------------------------------
* 입고처리.
*-----------------------------------------------------------------------
        IF R_UCOMM EQ 'POST'.
           CALL FUNCTION 'ZIM_BAPI_GOODSMVT_CREATE'
                EXPORTING
                    P_ZFIVNO            =   POST_HEADER-ZFIVNO
                    P_CHG_MODE          =   'X'
                    P_MVT_TYPE          =   ZTIVHST-BWART
                    P_BLDAT             =   ZTIVHST-BLDAT
                    P_BUDAT             =   ZTIVHST-BUDAT
                IMPORTING
                    MATERIALDOCUMENT    =   MATERIALDOCUMENT
                    MATDOCUMENTYEAR     =   MATDOCUMENTYEAR
                TABLES
                    RETURN              =   RETURN
                    IT_ZSIVHSTIT        =   IT_ZSIVHSTIT
                EXCEPTIONS
                    MVT_ERROR           =   4.

           IF SY-SUBRC NE 0.           ">> 오류 발생시...
              ROLLBACK WORK.
              IF RETURN[] IS INITIAL.
                 PERFORM  P2000_MESSAGE_MAKE
                                        TABLES  IT_ERR_LIST
                                        USING   'E'  POST_HEADER-ZFIVNO.

              ELSE.
                 PERFORM  P2000_MULTI_MSG_MAKE
                                     TABLES  IT_ERR_LIST
                                     USING   POST_HEADER-ZFIVNO.
              ENDIF.
              ADD    1    TO    W_ERR_CNT.
*-----------------------------------------------------------------------
*>>> UNLOCK.
              CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
                   EXPORTING
                        ZFIVNO      =     POST_HEADER-ZFIVNO.
*-----------------------------------------------------------------------
              CONTINUE.
           ELSE.
              COMMIT WORK.
              MESSAGE S012(MIGO) WITH MATERIALDOCUMENT.
              PERFORM  P2000_MESSAGE_MAKE
                                    TABLES  IT_ERR_LIST
                                    USING   'S'  POST_HEADER-ZFIVNO.
           ENDIF.
        ENDIF.

        IF R_UCOMM EQ 'UNDO'.  ">입고취소처리.
           CALL FUNCTION 'ZIM_BAPI_GOODSMVT_CANCEL'
                EXPORTING
                    P_ZFIVNO            =   POST_HEADER-ZFIVNO
                    P_ZFIVHST           =   ZTIVHST-ZFIVHST
                    MATERIALDOCUMENT    =   ZTIVHST-MBLNR
                    MATDOCUMENTYEAR     =   ZTIVHST-MJAHR
                    GOODSMVT_PSTNG_DATE =   *ZTIVHST-BUDAT
                    GOODSMVT_PR_UNAME   =   SY-UNAME
                IMPORTING
                    GOODSMVT_HEADRET    =   GR_DOC
                TABLES
                    RETURN            =   RETURN
                EXCEPTIONS
                    MVT_ERROR         =   4.

           IF SY-SUBRC NE 0.           ">> 오류 발생시...
              ROLLBACK WORK.
              IF RETURN[] IS INITIAL.
                 PERFORM  P2000_MESSAGE_MAKE
                                      TABLES  IT_ERR_LIST
                                      USING   'E' POST_HEADER-ZFIVNO.
              ELSE.
                 PERFORM  P2000_MULTI_MSG_MAKE
                                       TABLES  IT_ERR_LIST
                                       USING   POST_HEADER-ZFIVNO.
              ENDIF.
              ADD    1    TO    W_ERR_CNT.
              " UNLOCK
              CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
                   EXPORTING
                        ZFIVNO      =     POST_HEADER-ZFIVNO.
*-----------------------------------------------------------------------
              CONTINUE.
           ELSE.
              COMMIT WORK.
              MESSAGE S060(M7) WITH GR_DOC-MAT_DOC.
              PERFORM  P2000_MESSAGE_MAKE
                                   TABLES  IT_ERR_LIST
                                   USING   'S'  POST_HEADER-ZFIVNO.
           ENDIF.
        ENDIF.

*-----------------------------------------------------------------------
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                ZFIVNO             =     POST_HEADER-ZFIVNO.
*-----------------------------------------------------------------------
        ADD 1       TO W_PROC_CNT.
     ENDLOOP.

*      IF R_UCOMM EQ 'POST'.
*       READ TABLE POST_LIST   INDEX 1.
*       MOVE: SY-DATUM         TO  ZTBKPF-BUDAT,
*             SY-DATUM         TO  ZTBKPF-BLDAT,
*             POST_LIST-BUKRS  TO  ZTBKPF-BUKRS,
*             POST_LIST-ZFKRW  TO  ZTBKPF-WAERS,
*             SY-DATUM(4)      TO  ZTBKPF-GJAHR,
*             'KR'             TO  ZTBKPF-BLART,
*             'V0'             TO  ZTBKPF-MWSKZ,
**                POST_LIST-ZFHBLNO TO ZTBKPF-XBLNR,
*            '수입 무환 Sample 잡이익' TO ZTBKPF-BKTXT.
*
*        SPOP-TITEL = '잡이익 처리확인'.
*       SPOP-TEXTLINE1 = '선택한 항목들로 잡이익 처리합니다.'.
*       SPOP-TEXTLINE2 = '회계전표로 전기하시겠습니까?'.
*       CALL SCREEN 0010 STARTING AT  01 1
*                        ENDING   AT  63 14.
*     ELSE.
*        SPOP-TITEL = '전표취소 확인'.
*       SPOP-TEXTLINE1 = '잡이익 취소 처리합니다.'.
*       SPOP-TEXTLINE2 = '전기취소 하시겠습니까?.'.
*       BSIS-BUDAT = SY-DATUM.
*
*       CALL SCREEN 0020 STARTING AT 30 2
*                        ENDING   AT 70 12.
*     ENDIF.

      IF ANTWORT EQ 'Y'.
        DESCRIBE  TABLE IT_ERR_LIST  LINES  W_LINE.
        IF W_LINE GT 0.
          CALL SCREEN 0030 STARTING AT  01   1
                           ENDING   AT  102 16.
        ENDIF.
      ELSE.
        MESSAGE S957.   EXIT.
      ENDIF.

      PERFORM P1000_DATA_SELECTION.
      PERFORM P2000_PROCESS_LIST.

      RS_SELFIELD-REFRESH = 'X'.
*       PERFORM P3000_OUTPUT_BASIC_LIST.

    WHEN 'DOCU' OR '&IC1'.   ">B/L 조회.
      CLEAR : W_LINE.
      LOOP AT HEADER WHERE BOX EQ 'X'.
        ADD 1 TO W_LINE.
        MOVE-CORRESPONDING HEADER TO LS_RETURN.
      ENDLOOP.
      CASE W_LINE.
        WHEN 0.
          IF RS_SELFIELD-TABNAME EQ 'HEADER'.
            READ TABLE HEADER INDEX RS_SELFIELD-TABINDEX.
          ELSEIF RS_SELFIELD-TABNAME EQ 'LIST'.
            READ TABLE LIST INDEX RS_SELFIELD-TABINDEX.
          ENDIF.
          IF SY-SUBRC NE 0.
            MESSAGE S003(OK). EXIT.
          ENDIF.
          IF RS_SELFIELD-TABNAME EQ 'HEADER'.
            MOVE-CORRESPONDING HEADER TO LS_RETURN.
          ELSEIF RS_SELFIELD-TABNAME EQ 'LIST'.
            MOVE-CORRESPONDING LIST   TO LS_RETURN.
          ENDIF.
        WHEN 1.
        WHEN OTHERS.
          MESSAGE S035(0K).   EXIT.
      ENDCASE.

      SET PARAMETER ID 'ZPBLNO'  FIELD LS_RETURN-ZFBLNO.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.

      CALL TRANSACTION 'ZIM23' AND SKIP FIRST SCREEN.

    WHEN 'PUOR'.   ">통관문서 조회.
      CLEAR : W_LINE.
      LOOP AT HEADER WHERE BOX EQ 'X'.
        ADD 1 TO W_LINE.
        MOVE-CORRESPONDING HEADER TO LS_RETURN.
      ENDLOOP.
      CASE W_LINE.
        WHEN 0.
          IF RS_SELFIELD-TABNAME EQ 'HEADER'.
            READ TABLE HEADER INDEX RS_SELFIELD-TABINDEX.
          ELSEIF RS_SELFIELD-TABNAME EQ 'LIST'.
            READ TABLE LIST INDEX RS_SELFIELD-TABINDEX.
          ENDIF.
          IF SY-SUBRC NE 0.
            MESSAGE S003(OK). EXIT.
          ENDIF.
          IF RS_SELFIELD-TABNAME EQ 'HEADER'.
            MOVE-CORRESPONDING HEADER TO LS_RETURN.
          ELSEIF RS_SELFIELD-TABNAME EQ 'LIST'.
            MOVE-CORRESPONDING LIST   TO LS_RETURN.
          ENDIF.
        WHEN 1.
        WHEN OTHERS.
          MESSAGE S035(0K).   EXIT.
      ENDCASE.

      SET PARAMETER ID 'ZPIVNO'  FIELD LS_RETURN-ZFIVNO.
      SET PARAMETER ID 'ZPBLNO'  FIELD ''.
      SET PARAMETER ID 'ZPHBLNO' FIELD ''.

      CALL TRANSACTION 'ZIM33' AND SKIP FIRST SCREEN.


*    WHEN '&IC1'.
**     Double click / F2
*      IF 'BELEGE-SMBLN BELEGE-SJAHR BELEGE-SMBLP'
*             CS rs_selfield-sel_tab_field.
*        PERFORM matdoc_show USING ls_belege-smbln
*                                  ls_belege-sjahr.
*      ELSE.
*        PERFORM matdoc_show USING ls_belege-mblnr
*                                 ls_belege-mjahr.
*      ENDIF.
*    WHEN '9DET'.
*      PERFORM detail_list.
*    WHEN '9RWB'.
*      IF 'BELEGE-SMBLN BELEGE-SJAHR BELEGE-SMBLP'
*            CS rs_selfield-sel_tab_field.
*        PERFORM fidoc_show USING ls_belege-smbln
*                                  ls_belege-sjahr.
*      ELSE.
*        PERFORM fidoc_show USING ls_belege-mblnr
*                                 ls_belege-mjahr.
*      ENDIF.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  fidoc_show
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM FIDOC_SHOW USING    P_BUKRS
                         P_GJAHR
                         P_BELNR.

  SET  PARAMETER ID  'BUK'   FIELD   P_BUKRS.
  SET  PARAMETER ID  'BLN'   FIELD   P_BELNR.
  SET  PARAMETER ID  'GJR'   FIELD   P_GJAHR.

  CALL TRANSACTION 'FB03' AND SKIP  FIRST SCREEN.

ENDFORM.                    " fidoc_show
*&---------------------------------------------------------------------*
*&      Form  P2000_DETAIL_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_DETAIL_LIST.

  DATA: VARIANT_DETAIL      LIKE DISVARIANT.
  DATA:     LS_PRINT  TYPE SLIS_PRINT_ALV.
  DATA: LT_BASE_LIST LIKE LIST[].                           "401421
* The detail ALV may modify the list (sorting). If returned to the
* base list, the original list needs to be restored.
  LT_BASE_LIST[] = LIST[].                                  "401421
  CLEAR VARIANT_DETAIL.
  VARIANT_DETAIL-REPORT = ALV_REPID.
  VARIANT_DETAIL-HANDLE = 'DETA'.

  DETAIL = 'X'.

*  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_CALLBACK_PROGRAM       = ALV_REPID
            I_CALLBACK_PF_STATUS_SET = 'STATUS_SET'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND_MAIN'
            IS_LAYOUT                = ALV_LAYOUT
            IT_FIELDCAT              = FC_FLAT[]
            I_DEFAULT                = 'X'
            I_SAVE                   = 'A'
            IS_VARIANT               = VARIANT_DETAIL
            IS_PRINT                 = LS_PRINT
       TABLES
            T_OUTTAB                 = LIST[]
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR DETAIL.                                             "401421
  LIST[] = LT_BASE_LIST[].                                  "401421

ENDFORM.                    " P2000_DETAIL_LIST
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0010 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

  IF OPTION = '1'.
    SET CURSOR FIELD 'SPOP-OPTION1'.
  ELSE.
    SET CURSOR FIELD 'SPOP-OPTION2'.
  ENDIF.

ENDMODULE.                 " SET_STATUS_SCR0010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE OK_CODE_CLEAR_SCRCOM OUTPUT.
  CLEAR : OK-CODE.
ENDMODULE.                 " OK_CODE_CLEAR_SCRCOM  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_EXIT_SCRCOM INPUT.

  IF SY-DYNNR EQ '0030'.
    IF NOT G_CUSTOM_CONTAINER IS INITIAL.
      " destroy tree container (detroys contained tree control, too)
      CALL METHOD G_CUSTOM_CONTAINER->FREE
        EXCEPTIONS
          CNTL_SYSTEM_ERROR = 1
          CNTL_ERROR        = 2.
      IF SY-SUBRC <> 0.
        MESSAGE A000.
      ENDIF.
      CLEAR G_CUSTOM_CONTAINER.
      CLEAR G_TREE.
      CLEAR G_APPLICATION.
    ENDIF.
  ENDIF.

  ANTWORT = 'C'.
  SET SCREEN 0.   LEAVE SCREEN.

ENDMODULE.                 " USER_EXIT_SCRCOM  INPUT
*&---------------------------------------------------------------------*
*&      Module  COMPANYCODE_CHECK_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE COMPANYCODE_CHECK_SCR0010 INPUT.

  IF ZTBKPF-BUKRS IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUKRS'.
  ELSE.
    PERFORM  P1000_GET_COMPANY_CODE(SAPMZIM02) USING ZTBKPF-BUKRS.
*>> IMG 비용계정코드.
    SELECT SINGLE * FROM ZTIMIMG11
           WHERE    BUKRS  EQ   ZTBKPF-BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE S987 WITH ZTBKPF-BUKRS.
      LEAVE TO SCREEN 0.
    ENDIF.
  ENDIF.

ENDMODULE.                 " COMPANYCODE_CHECK_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  PERIOD_CHECK_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PERIOD_CHECK_SCR0010 INPUT.
  DATA: S_MONAT LIKE BKPF-MONAT.      "Save field for input period

*> 증빙일.
  IF ZTBKPF-BLDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BLDAT'.
  ENDIF.

  IF ZTBKPF-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZTBKPF' 'BUDAT'.
  ENDIF.

*  IF ZSBKPF-ZFBDT IS INITIAL.
*     ZSBKPF-ZFBDT = ZTBKPF-BUDAT.
*  ENDIF.

  CLEAR ZTBKPF-GJAHR.
  S_MONAT = ZTBKPF-MONAT.

  PERFORM PERIODE_ERMITTELN(SAPMZIM02) USING ZTBKPF-BUDAT
                                             ZTBKPF-GJAHR
                                             ZTBKPF-MONAT.

  IF NOT S_MONAT IS INITIAL
  AND    S_MONAT NE ZTBKPF-MONAT
  AND ( SY-BINPT = SPACE AND SY-CALLD = SPACE ).
    MESSAGE W000(F5) WITH S_MONAT ZTBKPF-BUDAT ZTBKPF-MONAT.
  ENDIF.

ENDMODULE.                 " PERIOD_CHECK_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  BELEGART_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE BELEGART_CHECK_SCR0010 INPUT.

  IF ZTBKPF-BLART IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'BLART'.
  ENDIF.

  IF ZTBKPF-MWSKZ IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'ZSBKPF' 'MWSKZ'.
  ENDIF.
*> 문서 종류 체크.
  PERFORM BELEGART_PRUEFEN(SAPFF001)
          USING ZTBKPF-BLART ZTBKPF-GJAHR.
*> 전기년도 체크.
  BKPF-BUKRS = ZTBKPF-BUKRS.
  PERFORM NUMMERNKREIS_LESEN(SAPFF001)
          USING ZTBKPF-GJAHR.

ENDMODULE.                 " BELEGART_CHECK_SCR0060  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0010 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'YES'.
      CLEAR : W_LINE1.
      REFRESH: IT_ERR_LIST.
      LOOP AT POST_HEADER.
        ADD 1 TO W_LINE1.
        LINE1 = ( W_LINE1 / W_LINE ) * 100.
        OUT_TEXT = 'FI POSTING PROGRESS %99999%%'.
        REPLACE '%99999%' WITH LINE1 INTO OUT_TEXT.
        PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE1.
        REFRESH : XBSEG, RETURN, XZSIVHSTIT.
*------------------------------------------------------------------
*> 통관요청 LOCK.
*------------------------------------------------------------------
        CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = POST_HEADER-ZFIVNO
             EXCEPTIONS
                  OTHERS = 1.

        IF SY-SUBRC <> 0.
          MESSAGE S510 WITH SY-MSGV1 '통관/입고요청'
                                      POST_HEADER-ZFIVNO ''
                       RAISING DOCUMENT_LOCKED.

          PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                      USING  'E'
                                             POST_HEADER-ZFIVNO.
          CONTINUE.
        ENDIF.

        SELECT SINGLE * FROM ZTIMIMG11
               WHERE BUKRS EQ POST_HEADER-BUKRS.
        ZTBKPF-BUKRS = POST_HEADER-BUKRS.

        SELECT SINGLE * FROM  ZTIV
               WHERE ZFIVNO EQ POST_HEADER-ZFIVNO.

        SELECT SINGLE  * FROM ZTBL
               WHERE ZFBLNO EQ ZTIV-ZFBLNO.

        SELECT SINGLE * FROM T001W
               WHERE WERKS EQ ZTBL-ZFWERKS.

        IF ZTBL-KOSTL IS INITIAL AND
           ZTBL-PS_POSID IS INITIAL.
          MESSAGE  S413(ZIM1) WITH ZTBL-ZFBLNO.
          PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                      USING  'E'
                                             POST_HEADER-ZFIVNO.
          CONTINUE.
        ENDIF.

        CLEAR : ZTBKPF-WRBTR.
        LOOP AT POST_LIST WHERE ZFIVNO EQ POST_HEADER-ZFIVNO.
          IF POST_LIST-NDFTX EQ 'X'.
            MESSAGE  S411(ZIM1) WITH POST_LIST-ZFIVNO
                                     POST_LIST-ZFIVDNO.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                     USING  'W'
                                            POST_HEADER-ZFIVNO.
            CONTINUE.
          ENDIF.

          IF POST_LIST-ZFIVAMK IS INITIAL.
            MESSAGE  S412(ZIM1) WITH POST_LIST-ZFIVNO
                                     POST_LIST-ZFIVDNO.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                     USING  'W'
                                            POST_HEADER-ZFIVNO.
            CONTINUE.
          ENDIF.
          CLEAR: XBSEG.
          ADD POST_LIST-ZFIVAMK     TO ZTBKPF-WRBTR.
          MOVE: POST_LIST-ZFIVAMK   TO XBSEG-WRBTR,
                ZTBKPF-MWSKZ        TO XBSEG-MWSKZ,
                ZTBL-KOSTL          TO XBSEG-KOSTL,
                ZTBL-PS_POSID       TO XBSEG-PS_POSID,
                POST_LIST-ZFIVDNO   TO W_ZFIVDNO,
                POST_LIST-TXZ01     TO XBSEG-SGTXT,
                '40'                TO XBSEG-NEWBS,
                POST_LIST-ZFIVNO    TO XBSEG-ZFIVNO,
                POST_LIST-ZFIVDNO   TO XBSEG-ZFIVDNO.

          IF ZTBL-ZFUPT EQ 'B'.  ">본사.
            MOVE ZTIMIMG11-ZFIOCAC20 TO XBSEG-NEWKO.
          ELSE.
            MOVE ZTIMIMG11-ZFIOCAC30 TO XBSEG-NEWKO.
          ENDIF.

          CONCATENATE POST_LIST-ZFIVNO '/' POST_LIST-ZFIVDNO
                      INTO XBSEG-ZUONR.
          APPEND XBSEG.
        ENDLOOP.

        IF ZTBKPF-WRBTR EQ 0.
          MESSAGE S410(ZIM1).
          PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                      USING  'E'
                                             POST_HEADER-ZFIVNO.
          CONTINUE.
        ELSE.
          CLEAR: XBSEG.
          MOVE: '50'                TO XBSEG-NEWBS,
                ZTIMIMG11-ZFGAIN    TO XBSEG-NEWKO,
*                  ZTBKPF-MWSKZ        TO XBSEG-MWSKZ,
                ZTBKPF-WRBTR        TO XBSEG-WRBTR,
                ZTBL-KOSTL          TO XBSEG-KOSTL,
                T001W-J_1BBRANCH    TO XBSEG-BUPLA,
                ZTBL-PS_POSID       TO XBSEG-PS_POSID,
                ZTBL-ZFHBLNO        TO XBSEG-ZUONR,
                '수입무환 Sample 잡이익'  TO XBSEG-SGTXT.
*            INSERT XBSEG INDEX 1.
          APPEND XBSEG.
        ENDIF.

        MOVE: ZTBL-ZFHBLNO TO ZTBKPF-XBLNR.

        SET UPDATE TASK LOCAL.

        CALL FUNCTION 'ZIM_GAIN_DOCUMENT_POST'
             EXPORTING
                P_ZFIVNO    =  POST_HEADER-ZFIVNO
                ZTBKPF      =  ZTBKPF
*                 MODE        =  'A'
             IMPORTING
                BUKRS       =  ZTBKPF-BUKRS
                GJAHR       =  ZTBKPF-GJAHR
                BELNR       =  ZTBKPF-BELNR
             TABLES
                XBSEG       =  XBSEG
                RETURN      =  RETURN
             EXCEPTIONS
                POST_ERROR  =  4.
        W_SUBRC = SY-SUBRC.

        IF W_SUBRC EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

        IF RETURN[] IS INITIAL.
          IF W_SUBRC EQ 0.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                        USING  'S'
                                               POST_HEADER-ZFIVNO.
          ELSE.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                        USING  'E'
                                               POST_HEADER-ZFIVNO.
          ENDIF.
        ELSE.
          PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST
                                         USING   POST_HEADER-ZFIVNO.
        ENDIF.

*> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = POST_HEADER-ZFIVNO.

      ENDLOOP.

      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.


ENDMODULE.                 " GET_OK_CODE_SCR0010  INPUT
*&---------------------------------------------------------------------*
*&      Module  P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE P2000_INIT_VALUE_CHECK INPUT.

  IF NOT ( SY-UCOMM EQ 'ENTR' OR
           SY-UCOMM EQ 'YES' ).
    EXIT.
  ENDIF.

  IF UF05A-STGRD IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'UF05A' 'STGRD'.
  ENDIF.

  IF BSIS-BUDAT IS INITIAL.
    PERFORM NO_INPUT(SAPFMMEX) USING 'BSIS' 'BUDAT'.
  ENDIF.

ENDMODULE.                 " P2000_INIT_VALUE_CHECK  INPUT
*&---------------------------------------------------------------------*
*&      Form  p2000_show_bar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_SHOW_BAR USING    TEXT PERC.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = PERC
            TEXT       = TEXT.

ENDFORM.                    " p2000_show_bar
*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_MAKE  TABLES  IT_ERR_LIST  STRUCTURE IT_ERR_LIST
                         USING   P_MSGTY
                                 P_ZFIVNO.

  MOVE : P_MSGTY             TO     IT_ERR_LIST-MSGTYP,
         SY-MSGID            TO     IT_ERR_LIST-MSGID,
         SY-MSGNO            TO     IT_ERR_LIST-MSGNR,
         SY-MSGV1            TO     IT_ERR_LIST-MSGV1,
         SY-MSGV2            TO     IT_ERR_LIST-MSGV2,
         SY-MSGV3            TO     IT_ERR_LIST-MSGV3,
         SY-MSGV4            TO     IT_ERR_LIST-MSGV4,
         P_ZFIVNO            TO     IT_ERR_LIST-ZFIVNO.


  CALL FUNCTION 'MESSAGE_TEXT_BUILD'
       EXPORTING
            MSGID               = IT_ERR_LIST-MSGID
            MSGNR               = IT_ERR_LIST-MSGNR
            MSGV1               = IT_ERR_LIST-MSGV1
            MSGV2               = IT_ERR_LIST-MSGV2
            MSGV3               = IT_ERR_LIST-MSGV3
            MSGV4               = IT_ERR_LIST-MSGV4
       IMPORTING
            MESSAGE_TEXT_OUTPUT = IT_ERR_LIST-MESSTXT.

  CASE IT_ERR_LIST-MSGTYP.
    WHEN 'E' OR 'A'.
      MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
    WHEN 'I'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'S'.
      MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
    WHEN 'W'.
      MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
  ENDCASE.

  APPEND  IT_ERR_LIST.

ENDFORM.                    " P2000_MESSAGE_MAKE
*&---------------------------------------------------------------------*
*&      Form  P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_MULTI_MSG_MAKE TABLES   IT_ERR_LIST STRUCTURE IT_ERR_LIST
                          USING    DOC_NO.

  LOOP AT  RETURN.

    MOVE : RETURN-TYPE         TO     IT_ERR_LIST-MSGTYP,
           RETURN-ID           TO     IT_ERR_LIST-MSGID,
           RETURN-NUMBER       TO     IT_ERR_LIST-MSGNR,
           RETURN-MESSAGE_V1   TO     IT_ERR_LIST-MSGV1,
           RETURN-MESSAGE_V2   TO     IT_ERR_LIST-MSGV2,
           RETURN-MESSAGE_V3   TO     IT_ERR_LIST-MSGV3,
           RETURN-MESSAGE_V4   TO     IT_ERR_LIST-MSGV4,
           RETURN-MESSAGE      TO     IT_ERR_LIST-MESSTXT,
           DOC_NO              TO     IT_ERR_LIST-ZFIVNO.

    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E' OR 'A'.
        MOVE ICON_LED_RED             TO     IT_ERR_LIST-ICON.
      WHEN 'I'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'S'.
        MOVE ICON_LED_GREEN           TO     IT_ERR_LIST-ICON.
      WHEN 'W'.
        MOVE ICON_LED_YELLOW          TO     IT_ERR_LIST-ICON.
    ENDCASE.

    APPEND  IT_ERR_LIST.

  ENDLOOP.

ENDFORM.                    " P2000_MULTI_MSG_MAKE
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0030  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0030 OUTPUT.

  SET TITLEBAR 'POPU' WITH SPOP-TITEL.
  SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0030  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_COLUMN_TREE_SCR0030  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_COLUMN_TREE_SCR0030 OUTPUT.

  IF G_TREE IS INITIAL.
    PERFORM P2000_CREATE_POST_MESSAGE_TREE.
  ENDIF.

ENDMODULE.                 " SET_COLUMN_TREE_SCR0030  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_CREATE_POST_MESSAGE_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_CREATE_POST_MESSAGE_TREE.

  DATA: NODE_TABLE TYPE TREEV_NTAB,
        ITEM_TABLE TYPE ITEM_TABLE_TYPE,
        EVENT TYPE CNTL_SIMPLE_EVENT,
        EVENTS TYPE CNTL_SIMPLE_EVENTS,
        HIERARCHY_HEADER TYPE TREEV_HHDR.

  CREATE OBJECT G_APPLICATION.

* create a container for the tree control
  CREATE OBJECT G_CUSTOM_CONTAINER
    EXPORTING
      " the container is linked to the custom control with the
      " name 'TREE_CONTAINER' on the dynpro
      CONTAINER_NAME = 'TREE_CONTAINER'
    EXCEPTIONS
      CNTL_ERROR = 1
      CNTL_SYSTEM_ERROR = 2
      CREATE_ERROR = 3
      LIFETIME_ERROR = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.
* setup the hierarchy header
  HIERARCHY_HEADER-HEADING = '메시지 종류'.                 "#EC NOTEXT
  " heading
  HIERARCHY_HEADER-WIDTH = 18.         " width: 30 characters

* create a tree control

* After construction, the control contains one column in the
* hierarchy area. The name of this column
* is defined via the constructor parameter HIERACHY_COLUMN_NAME.
  CREATE OBJECT G_TREE
    EXPORTING
      PARENT              = G_CUSTOM_CONTAINER
      NODE_SELECTION_MODE = CL_GUI_COLUMN_TREE=>NODE_SEL_MODE_SINGLE
      ITEM_SELECTION = 'X'
      HIERARCHY_COLUMN_NAME = 'Column1'
      HIERARCHY_HEADER = HIERARCHY_HEADER
    EXCEPTIONS
      CNTL_SYSTEM_ERROR           = 1
      CREATE_ERROR                = 2
      FAILED                      = 3
      ILLEGAL_NODE_SELECTION_MODE = 4
      ILLEGAL_COLUMN_NAME         = 5
      LIFETIME_ERROR              = 6.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'. " process PAI if event occurs
  APPEND EVENT TO EVENTS.

  " item double click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_ITEM_DOUBLE_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " expand no children
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_EXPAND_NO_CHILDREN.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " link click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " button click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_BUTTON_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " checkbox change
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_CHECKBOX_CHANGE.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  " header click
  EVENT-EVENTID = CL_GUI_COLUMN_TREE=>EVENTID_HEADER_CLICK.
  EVENT-APPL_EVENT = 'X'.
  APPEND EVENT TO EVENTS.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* assign event handlers in the application class to each desired event
  SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_EXPAND_NO_CHILDREN FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_BUTTON_CLICK FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_CHECKBOX_CHANGE FOR G_TREE.
  SET HANDLER G_APPLICATION->HANDLE_HEADER_CLICK FOR G_TREE.

* insert two additional columns

* Column2
  CALL METHOD G_TREE->ADD_COLUMN
    EXPORTING
      NAME = 'Column2'
      WIDTH = 50
      HEADER_TEXT = '메시지'
    EXCEPTIONS
      COLUMN_EXISTS                 = 1
      ILLEGAL_COLUMN_NAME           = 2
      TOO_MANY_COLUMNS              = 3
      ILLEGAL_ALIGNMENT             = 4
      DIFFERENT_COLUMN_TYPES        = 5
      CNTL_SYSTEM_ERROR             = 6
      FAILED                        = 7
      PREDECESSOR_COLUMN_NOT_FOUND  = 8.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* add some nodes to the tree control
* NOTE: the tree control does not store data at the backend. If an
* application wants to access tree data later, it must store the
* tree data itself.
  REFRESH : NODE_TABLE, ITEM_TABLE.
  PERFORM P2000_BUILD_MSG_NODE_TABLE USING NODE_TABLE ITEM_TABLE.

  CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE = NODE_TABLE
      ITEM_TABLE = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME = 'MTREEITM'
    EXCEPTIONS
      FAILED = 1
      CNTL_SYSTEM_ERROR = 3
      ERROR_IN_TABLES = 4
      DP_ERROR = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.

* expand the node with key 'Root'
  LOOP AT IT_ERR_GRP.
    G_NODE_KEY = IT_ERR_GRP-ZFIVNO.
    CALL METHOD G_TREE->EXPAND_NODE
      EXPORTING
        NODE_KEY = G_NODE_KEY                               "#EC NOTEXT
      EXCEPTIONS
        FAILED              = 1
        ILLEGAL_LEVEL_COUNT = 2
        CNTL_SYSTEM_ERROR   = 3
        NODE_NOT_FOUND      = 4
        CANNOT_EXPAND_LEAF  = 5.
    IF SY-SUBRC <> 0.
      MESSAGE A000.
    ENDIF.
    CLEAR : G_NODE_KEY.
  ENDLOOP.

ENDFORM.                    " P2000_CREATE_POST_MESSAGE_TREE

*&---------------------------------------------------------------------*
*&      Form  P2000_BUILD_MSG_NODE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM P2000_BUILD_MSG_NODE_TABLE
  USING
    NODE_TABLE TYPE TREEV_NTAB
    ITEM_TABLE TYPE ITEM_TABLE_TYPE.

  DATA: NODE TYPE TREEV_NODE,
        ITEM TYPE MTREEITM,
        SEQ  TYPE I.

  REFRESH : IT_ERR_GRP.
  SORT IT_ERR_LIST BY ZFIVNO.
  CLEAR : SEQ.
  LOOP AT IT_ERR_LIST.
    W_TABIX = SY-TABIX.
    ADD 1 TO SEQ.
    IT_ERR_LIST-ZFSEQ = SEQ.
    MODIFY IT_ERR_LIST INDEX W_TABIX.
    MOVE: IT_ERR_LIST-ZFIVNO TO IT_ERR_GRP-ZFIVNO.
    COLLECT IT_ERR_GRP.
  ENDLOOP.

  LOOP AT IT_ERR_GRP.
*    READ TABLE POST_HEADER WITH KEY ZFIVNO = IT_ERR_LIST-ZFIVNO.
    NODE-NODE_KEY = IT_ERR_GRP-ZFIVNO.                      "#EC NOTEXT
    " Key of the node
    CLEAR : NODE-RELATKEY.
*     NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
    CLEAR : NODE-RELATSHIP.

    NODE-HIDDEN = ' '.
    NODE-DISABLED = ' '.
    NODE-ISFOLDER = 'X'.
    CLEAR NODE-N_IMAGE.
    CLEAR NODE-EXP_IMAGE.
*    NODE-T_IMAGE = '@0B@'.
    NODE-EXPANDER = 'X'. " The node is marked with a '+', although
*                      " it has no children. When the user clicks on the
*                      " + to open the node, the event expand_nc is
*                      " fired. The programmerr can
*                      " add the children of the
*                      " node within the event handler of the expand_nc
*                      " event  (see callback handle_expand_nc).
    APPEND NODE TO NODE_TABLE.
  ENDLOOP.

  LOOP AT IT_ERR_LIST.
    NODE-NODE_KEY = IT_ERR_LIST-ZFSEQ.
* Node is inserted as child of the node with key 'Root'.
    NODE-RELATKEY = IT_ERR_LIST-ZFIVNO.
    NODE-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.

    NODE-HIDDEN = ' '.
    NODE-DISABLED = ' '.
    NODE-ISFOLDER = ' '.
    CLEAR NODE-N_IMAGE.
    CLEAR NODE-EXP_IMAGE.
    NODE-EXPANDER = ' '. " The node is marked with a '+', although
    APPEND NODE TO NODE_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = NODE-NODE_KEY.
    ITEM-ITEM_NAME = 'Column1'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
    CASE IT_ERR_LIST-MSGTYP.
      WHEN 'E'.
        ITEM-TEXT = '오류메시지'.                           "#EC NOTEXT
      WHEN 'A' OR 'X'.
        ITEM-TEXT = '종료메시지'.                           "#EC NOTEXT
      WHEN 'S'.
        ITEM-TEXT = '전기메시지'.                           "#EC NOTEXT
      WHEN 'I'.
        ITEM-TEXT = '정보메시지'.                           "#EC NOTEXT
      WHEN 'W'.
        ITEM-TEXT = '경고메시지'.                           "#EC NOTEXT
    ENDCASE.
    ITEM-T_IMAGE = IT_ERR_LIST-ICON.
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = NODE-NODE_KEY.
    ITEM-ITEM_NAME = 'Column2'.     "
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = IT_ERR_LIST-MESSTXT.                        "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

  ENDLOOP.

* The items of the nodes:

* Node with key 'Child1'
  LOOP AT IT_ERR_GRP.
    READ TABLE POST_HEADER WITH KEY ZFIVNO = IT_ERR_GRP-ZFIVNO.
    CLEAR ITEM.
    ITEM-NODE_KEY = POST_HEADER-ZFIVNO.                    "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column1'.
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = POST_HEADER-ZFIVNO.                         "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.

    CLEAR ITEM.
    ITEM-NODE_KEY = POST_HEADER-ZFIVNO..                   "#EC NOTEXT.
    ITEM-ITEM_NAME = 'Column2'.     "
    ITEM-CLASS = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT. " Text Item
    ITEM-TEXT = POST_HEADER-ZFHBLNO.                        "#EC NOTEXT
    APPEND ITEM TO ITEM_TABLE.
  ENDLOOP.


ENDFORM.                    " P2000_BUILD_MSG_NODE_TABLE
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0030  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0030 INPUT.
  DATA : RETURN_CODE TYPE I.

* CL_GUI_CFW=>DISPATCH must be called if events are registered
* this method calls the event handler method of an event
  CALL METHOD CL_GUI_CFW=>DISPATCH
    IMPORTING RETURN_CODE = RETURN_CODE.
*    if return_code <> cl_gui_cfw=>rc_noevent.
  IF RETURN_CODE <> CL_GUI_CFW=>RC_NOEVENT.
    PERFORM  P2000_MESSAGE_TREE_HANDLER.
    CLEAR : G_EVENT, G_NODE_KEY.
    EXIT.
  ENDIF.


  CASE SY-UCOMM.
    WHEN 'CANC' OR 'YES' OR 'NO'.
      IF NOT G_CUSTOM_CONTAINER IS INITIAL.
        " destroy tree container (detroys contained tree control, too)
        CALL METHOD G_CUSTOM_CONTAINER->FREE
          EXCEPTIONS
            CNTL_SYSTEM_ERROR = 1
            CNTL_ERROR        = 2.
        IF SY-SUBRC <> 0.
          MESSAGE A000.
        ENDIF.
        CLEAR G_CUSTOM_CONTAINER.
        CLEAR G_TREE.
        CLEAR G_APPLICATION.
      ENDIF.
      CLEAR : G_NODE_KEY, G_NODE_KEY_OLD, G_EVENT, G_NODE_KEY.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
*    WHEN 'ENTR'.   EXIT.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

  CLEAR : G_EVENT, G_NODE_KEY.

ENDMODULE.                 " GET_OK_CODE_SCR0030  INPUT

*&---------------------------------------------------------------------*
*&      Form  P2000_MESSAGE_TREE_HANDLER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P2000_MESSAGE_TREE_HANDLER.
  DATA : L_NODE_KEY   TYPE   I.

  CASE G_EVENT.
    WHEN 'ITEM_DOUBLE_CLICK' OR 'NODE_DOUBLE_CLICK'.
      IF G_NODE_KEY EQ 'Root' OR G_NODE_KEY IS INITIAL.
        MESSAGE S903(ZIM1).
        EXIT.
      ELSE.
        L_NODE_KEY = G_NODE_KEY.
        IF L_NODE_KEY GT 10000.
          MESSAGE S905(ZIM1).
          EXIT.
        ELSE.
          READ TABLE IT_ERR_LIST WITH KEY ZFSEQ = L_NODE_KEY.
          IF SY-SUBRC EQ 0.
            CALL FUNCTION 'MASS_MESSAGE_SHOW_LONGTEXT'
                 EXPORTING
                    SPRSL     = SY-LANGU
                    ARBGB     = IT_ERR_LIST-MSGID
                    MSGNR     = IT_ERR_LIST-MSGNR
                    MSGV1     = IT_ERR_LIST-MSGV1
                    MSGV2     = IT_ERR_LIST-MSGV2
                    MSGV3     = IT_ERR_LIST-MSGV3
                    MSGV4     = IT_ERR_LIST-MSGV4
*                    extravars = 'F'
                 EXCEPTIONS
                     NOT_FOUND = 1
                     OTHERS    = 2.
            IF SY-SUBRC NE 0.
            ENDIF.
          ENDIF.
          G_NODE_KEY_OLD = G_NODE_KEY.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " P2000_MESSAGE_TREE_HANDLER
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0020 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.   ANTWORT = 'C'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'YES'.
      CLEAR : W_LINE1.
      REFRESH: IT_ERR_LIST.
      LOOP AT POST_HEADER.
        ADD 1 TO W_LINE1.
        LINE1 = ( W_LINE1 / W_LINE ) * 100.
        OUT_TEXT = '전기취소 PROGRESS %99999%%'.
        REPLACE '%99999%' WITH LINE1 INTO OUT_TEXT.
        PERFORM P2000_SHOW_BAR USING OUT_TEXT LINE1.
        REFRESH : XBSEG, RETURN, XZSIVHSTIT.
*------------------------------------------------------------------
*> 통관요청 LOCK.
*------------------------------------------------------------------
        CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = POST_HEADER-ZFIVNO
             EXCEPTIONS
                  OTHERS = 1.

        IF SY-SUBRC <> 0.
          MESSAGE S510 WITH SY-MSGV1 '통관/입고요청'
                                      POST_HEADER-ZFIVNO ''
                       RAISING DOCUMENT_LOCKED.

          PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                      USING  'E'
                                             POST_HEADER-ZFIVNO.
          CONTINUE.
        ENDIF.

        L_CHK = 'N'.
        LOOP AT POST_LIST WHERE ZFIVNO EQ POST_HEADER-ZFIVNO.
          IF POST_LIST-NDFTX NE 'X'.
            MESSAGE  S415(ZIM1) WITH POST_LIST-ZFIVNO
                                     POST_LIST-ZFIVDNO.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                     USING  'W'
                                            POST_HEADER-ZFIVNO.
            L_CHK = 'Y'.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF L_CHK = 'Y'.
          CONTINUE.
        ENDIF.

        SET UPDATE TASK LOCAL.

        CALL FUNCTION 'ZIM_GAIN_DOCUMENT_CANCEL'
             EXPORTING
                ZFIVNO         =  POST_HEADER-ZFIVNO
                POSTINGDATE    =  BSIS-BUDAT
                REASONREVERSAL =  UF05A-STGRD
*                 MODE        =  'A'
*              IMPORTING
*                 FISCALYEAR_REVERSAL         =  ZTBKPF-GJAHR
*                 INVOICEDOCNUMBER_REVERSAL   =  ZTBKPF-BELNR
             TABLES
                RETURN      =  RETURN
             EXCEPTIONS
                POST_ERROR  =  4.
        W_SUBRC = SY-SUBRC.

        IF W_SUBRC EQ 0.
          COMMIT WORK.
        ELSE.
          ROLLBACK WORK.
        ENDIF.

        IF RETURN[] IS INITIAL.
          IF W_SUBRC EQ 0.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                        USING  'S'
                                               POST_HEADER-ZFIVNO.
          ELSE.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                        USING  'E'
                                               POST_HEADER-ZFIVNO.
          ENDIF.
        ELSE.
          PERFORM  P2000_MULTI_MSG_MAKE  TABLES  IT_ERR_LIST
                                         USING   POST_HEADER-ZFIVNO.
        ENDIF.

*> UNLOCK.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = POST_HEADER-ZFIVNO.

      ENDLOOP.

      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0040 INPUT.

  CASE SY-UCOMM.
    WHEN 'CANC'.
      ANTWORT = 'C'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'ENTR'.   EXIT.
    WHEN 'SELA' OR 'DSEL'.
      LOOP AT IT_TAB_UD.
        W_TABIX = SY-TABIX.
        IF SY-UCOMM EQ 'SELA'.
          IT_TAB_UD-ZFMARK = 'X'.
        ELSEIF SY-UCOMM EQ 'DSEL'.
          CLEAR : IT_TAB_UD-ZFMARK.
        ENDIF.
        MODIFY IT_TAB_UD INDEX W_TABIX.
      ENDLOOP.
    WHEN 'YES'.
      CLEAR : W_LINE1.
      REFRESH: IT_ERR_LIST, IT_ERR_GRP, IT_ZSIVIT.
      LOOP AT IT_TAB_UD WHERE ZFMARK EQ 'X'.
        MOVE: IT_TAB_UD-ZFIVNO TO IT_ERR_GRP-ZFIVNO.
        COLLECT IT_ERR_GRP.
      ENDLOOP.
      IF SY-SUBRC NE 0.
        MESSAGE S951. EXIT.
      ENDIF.
*------------------------------------------------------------------
*> 통관요청 LOCK.
*------------------------------------------------------------------
      LOOP AT IT_ERR_GRP.
        CALL FUNCTION 'ENQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = ZTIV-ZFIVNO
             EXCEPTIONS
                  OTHERS = 1.

        IF SY-SUBRC <> 0.
          MESSAGE S510 WITH SY-MSGV1 '통관/입고요청'
                                      POST_HEADER-ZFIVNO ''
                       RAISING DOCUMENT_LOCKED.

          PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                      USING  'E'
                                             POST_HEADER-ZFIVNO.
          DELETE IT_TAB_UD  WHERE ZFIVNO = IT_ERR_GRP-ZFIVNO.
          DELETE IT_ERR_GRP WHERE ZFIVNO = IT_ERR_GRP-ZFIVNO.
          CONTINUE.
        ENDIF.
      ENDLOOP.

      LOOP AT IT_TAB_UD WHERE ZFMARK EQ 'X'.
        REFRESH : IT_ZSIVIT.
        SELECT SINGLE *
                 FROM   ZTIVIT
                 WHERE  ZFIVNO  EQ  IT_TAB_UD-ZFIVNO
                 AND    ZFIVDNO EQ  IT_TAB_UD-ZFIVDNO.
        MOVE-CORRESPONDING ZTIVIT TO IT_ZSIVIT.
        MOVE IT_TAB_UD-ZFIVAMK TO IT_ZSIVIT-ZFIVAMK.
        APPEND IT_ZSIVIT.

        SET UPDATE TASK LOCAL.

        MODIFY ZTIVIT FROM TABLE IT_ZSIVIT.

        IF SY-SUBRC EQ 0.
          SELECT SUM( ZFIVAMK ) SUM( ZFIVAMT )
          INTO (ZTIV-ZFIVAMK, ZTIV-ZFIVAMT)
          FROM ZTIVIT
          WHERE ZFIVNO = IT_TAB_UD-ZFIVNO.

          UPDATE ZTIV
                 SET: ZFIVAMK = ZTIV-ZFIVAMK,
                      ZFIVAMT = ZTIV-ZFIVAMT
                 WHERE ZFIVNO = IT_TAB_UD-ZFIVNO.
          IF SY-SUBRC NE 0.
            ROLLBACK WORK.
            MESSAGE S417(ZIM1)
                    WITH IT_TAB_UD-ZFIVNO IT_TAB_UD-ZFIVDNO.
            PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                        USING  'E'
                                               IT_TAB_UD-ZFIVNO.
          ELSE.
            COMMIT WORK.
            READ TABLE LIST WITH KEY ZFIVNO  = IT_TAB_UD-ZFIVNO
                                     ZFIVDNO = IT_TAB_UD-ZFIVDNO.
            IF SY-SUBRC EQ 0.
              LIST-ZFIVAMK  =  IT_TAB_UD-ZFIVAMK.
              MODIFY LIST INDEX SY-TABIX.
            ENDIF.
          ENDIF.
        ELSE.
          ROLLBACK WORK.
          MESSAGE S417(ZIM1)
                  WITH IT_TAB_UD-ZFIVNO IT_TAB_UD-ZFIVDNO.
          PERFORM  P2000_MESSAGE_MAKE TABLES IT_ERR_LIST
                                      USING  'E'
                                             IT_TAB_UD-ZFIVNO.
        ENDIF.
      ENDLOOP.

*------------------------------------------------------------------
*> 통관요청 LOCK.
*------------------------------------------------------------------
      LOOP AT IT_ERR_GRP.
        CALL FUNCTION 'DEQUEUE_EZ_IM_ZTIV'
             EXPORTING
                  ZFIVNO = IT_ERR_GRP-ZFIVNO.
      ENDLOOP.

      ANTWORT = 'Y'.
      SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'NO'.     ANTWORT = 'N'.    SET SCREEN 0.   LEAVE SCREEN.
    WHEN 'OPT1'.   ANTWORT = '1'.
    WHEN 'OPT2'.   ANTWORT = '2'.
    WHEN OTHERS.
      EXIT.
  ENDCASE.

ENDMODULE.                 " GET_OK_CODE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0040 OUTPUT.

  DESCRIBE TABLE IT_TAB_UD LINES G_PARAM_LINE.   " LINE 수 GET
  TC_0040-LINES = G_PARAM_LINE.                   " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0040_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0040_SCR0040 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0040-CURRENT_LINE GT TC_0040-LINES.
    EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_TAB_UD   INDEX TC_0040-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
    MOVE-CORRESPONDING IT_TAB_UD   TO ZSIVHDIT.     " DATA MOVE
  ENDIF.

ENDMODULE.                 " IT_TO_TC0040_SCR0040  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0040 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0040-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*&      Module  ITAB_UPDATE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ITAB_UPDATE_SCR0040 INPUT.
* Internal Table Read
  READ TABLE IT_TAB_UD   INDEX TC_0040-CURRENT_LINE.
  W_TABIX    = SY-TABIX.

  IF SY-SUBRC EQ 0.
    MOVE-CORRESPONDING ZSIVHDIT TO IT_TAB_UD.
    MODIFY IT_TAB_UD INDEX W_TABIX.
  ENDIF.

ENDMODULE.                 " ITAB_UPDATE_SCR0040  INPUT
*&---------------------------------------------------------------------*
*&      Form  P4000_GET_GR_INIVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_GET_GR_INIVAL.

  CLEAR  W_ZFIDSDT.
  READ TABLE POST_HEADER INDEX 1.
  SELECT SINGLE * FROM ZTIV
                  WHERE ZFIVNO EQ POST_HEADER-ZFIVNO.

  IF ZTIV-ZFGRST EQ 'Y'.
     MESSAGE E500 WITH POST_HEADER-ZFIVNO.
  ENDIF.

*  IF ZTIV-ZFGRST EQ 'X'.
*    MESSAGE E501 WITH POST_HEADER-ZFIVNO.
* ENDIF.

  IF ZTIV-ZFCUST EQ 'Y'.
     SELECT  MAX( ZFIDSDT )  INTO  W_ZFIDSDT
     FROM    ZTIDS
     WHERE   ZFIVNO   EQ  POST_HEADER-ZFIVNO.
  ELSE.
     CLEAR : W_ZFIDSDT.
  ENDIF.

  MOVE:'전기 데이타' TO SPOP-TITEL,
       ZTIV-ZFIVNO   TO ZTIVHST-ZFIVNO.

  SELECT MAX( ZFIVHST ) INTO ZTIVHST-ZFIVHST
         FROM ZTIVHST
         WHERE ZFIVNO   EQ   ZTIV-ZFIVNO.
  ADD  1  TO  ZTIVHST-ZFIVHST.

*  MOVE 'X'            TO RADIO_NONE.
  IF ZTIVHST-BLDAT IS INITIAL.
     MOVE SY-DATUM    TO ZTIVHST-BLDAT.
  ENDIF.
  IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'Y'. "무환수출입.
     MOVE ZTIMIMG00-ZFMVTY2  TO ZTIVHST-BWART.
  ENDIF.
*> 무환.
  IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "무환.
     MOVE ZTIMIMG00-ZFMVTY3  TO ZTIVHST-BWART.
  ENDIF.
*> 유환.
  IF ZTIV-ZFPOYN = 'Y'.
     MOVE ZTIMIMG00-ZFMVTY1  TO ZTIVHST-BWART.
  ENDIF.
*>> MAX 면허일---> TODAY.
  MOVE W_ZFIDSDT     TO ZTIVHST-BUDAT.

*> 자재내역 SELECT..
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHSTIT
           FROM  ZTIVIT
           WHERE ZFIVNO EQ ZTIV-ZFIVNO
           AND   ZFPOTY EQ 'S'.
  IF SY-SUBRC NE 0.
     MESSAGE E549.
  ENDIF.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVIT
           FROM  ZTIVIT
           WHERE ZFIVNO EQ ZTIV-ZFIVNO
           AND   ZFPOTY EQ 'S'.
  IF SY-SUBRC NE 0.
     MESSAGE E549.
  ENDIF.

  LOOP AT IT_ZSIVHSTIT.
     W_TABIX = SY-TABIX.
     MOVE:  IT_ZSIVHSTIT-GRMENGE   TO   IT_ZSIVHSTIT-CCMENGE,
            ZTIVHST-ZFIVHST        TO   IT_ZSIVHSTIT-ZFIVHST.

     IT_ZSIVHSTIT-GRMENGE = IT_ZSIVHSTIT-CCMENGE
                          - IT_ZSIVHSTIT-GRTOTMN.

     IF IT_ZSIVHSTIT-GRMENGE GT 0.
        IT_ZSIVHSTIT-UMSON = 'X'.
     ELSE.
        CLEAR : IT_ZSIVHSTIT-UMSON.
     ENDIF.
     IT_ZSIVHSTIT-ZFGRST = 'Y'.

     MODIFY IT_ZSIVHSTIT INDEX W_TABIX.
  ENDLOOP.

  SPOP-TEXTLINE1 = '입고 전기데이타를 입력하세요.'.
  SPOP-TEXTLINE2 = '입고 전기하시겠습니까?'.

  CALL SCREEN 0050 STARTING AT 15 3
                   ENDING   AT 56 11.

ENDFORM.                    " P4000_GET_GR_INIVAL
*&---------------------------------------------------------------------*
*&      Module  SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_STATUS_SCR0020 OUTPUT.

   SET TITLEBAR 'POPU' WITH SPOP-TITEL.
   SET PF-STATUS 'POPU'.

ENDMODULE.                 " SET_STATUS_SCR0020  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_INPUT_FIELD_SCR0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_INPUT_FIELD_SCR0020 INPUT.

  IF NOT ( OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR' OR OK-CODE EQ 'DDCL' ).
     EXIT.
  ENDIF.

  IF ZTIVHST-BUDAT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BUDAT'.
  ENDIF.
  IF ZTIVHST-BLDAT IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BLDAT'.
  ENDIF.
  IF ZTIVHST-BWART IS INITIAL.
      PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BWART'.
  ENDIF.

*> 증빙일과 면허일 체크.
  IF NOT ( ZTIV-ZFREQTY EQ 'LO' OR ZTIV-ZFREQTY EQ 'PU' ).
     IF NOT ZTIVHST-BLDAT IS INITIAL.
        IF  ZTIVHST-BLDAT LT W_ZFIDSDT.
            MESSAGE  E560.
        ENDIF.
     ENDIF.
  ENDIF.

  IF ZTIVHST-BWART IS INITIAL.
     MOVE ZTIMIMG00-ZFMVTY1  TO ZTIVHST-BWART.
     IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'Y'. "무환수출입.
        MOVE ZTIMIMG00-ZFMVTY2  TO ZTIVHST-BWART.
     ENDIF.
     IF ZTIV-ZFPOYN = 'N' AND ZTIV-ZFPONMA = 'N'. "무환.
        MOVE ZTIMIMG00-ZFMVTY3  TO ZTIVHST-BWART.
     ENDIF.
  ELSE.
     SELECT SINGLE *
       FROM T156
      WHERE BWART = ZTIVHST-BWART.
     IF SY-SUBRC NE 0.
        MESSAGE E799.
        EXIT.
     ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_INPUT_FIELD_SCR0020  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_OK_CODE_SCR0050  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_OK_CODE_SCR0050 INPUT.

  IF OK-CODE EQ 'CANC' OR OK-CODE EQ 'NO'.
     ANTWORT = 'N'.
     SET SCREEN 0.   LEAVE SCREEN.
  ELSEIF OK-CODE EQ 'YES'.
     IF SY-DYNNR EQ '0070'.
        READ TABLE IT_ZSIVHST WITH KEY ZFMARK = 'X'.
        IF SY-SUBRC EQ 0.
           MOVE-CORRESPONDING IT_ZSIVHST TO ZTIVHST.
        ELSE.
           MESSAGE E962.
        ENDIF.
     ENDIF.
     ANTWORT = 'Y'.
     SET SCREEN 0.   LEAVE SCREEN.
  ELSE.
     ANTWORT = 'N'.
  ENDIF.

ENDMODULE.                 " GET_OK_CODE_SCR0050  INPUT
*&---------------------------------------------------------------------*
*&      Form  P4000_GET_GR_INIT_VALUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM P4000_GET_GR_INIT_VALUE.

  CLEAR : ZTIVHST.
  MOVE '입고취소 전기일자' TO SPOP-TITEL.

  IF *ZTIVHST-BUDAT IS INITIAL.
     MOVE SY-DATUM    TO *ZTIVHST-BUDAT.
  ENDIF.

  SELECT SINGLE * FROM ZTIV
                  WHERE ZFIVNO EQ POST_HEADER-ZFIVNO.
  CASE ZTIV-ZFGRST.
     WHEN 'N'.   MESSAGE E648 WITH POST_HEADER-ZFIVNO '입고대상'.
     WHEN 'X'.   MESSAGE E555.
     WHEN OTHERS.
  ENDCASE.

  CLEAR : ZTIVHST.
  SELECT COUNT( * ) INTO W_COUNT
                    FROM ZTIVHST
                    WHERE ZFIVNO  EQ  POST_HEADER-ZFIVNO
                    AND ( CMBLNR  EQ  SPACE
                    OR    CMBLNR  IS  NULL ).

  CASE W_COUNT.
     WHEN 1.
        SELECT SINGLE * FROM  ZTIVHST
                        WHERE ZFIVNO  EQ  POST_HEADER-ZFIVNO
                        AND ( CMBLNR  EQ  SPACE
                        OR    CMBLNR  IS  NULL ).
        IF SY-SUBRC NE 0.
           MESSAGE E647.
        ENDIF.
     WHEN 0.
        MESSAGE E647.
     WHEN OTHERS.
        REFRESH : IT_ZSIVHST.
           SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZSIVHST
                    FROM ZTIVHST
                    WHERE ZFIVNO  EQ  POST_HEADER-ZFIVNO
                    AND ( CMBLNR  EQ  SPACE
                    OR    CMBLNR  IS  NULL ).
  ENDCASE.

  IF W_COUNT EQ 1.
     CALL SCREEN 0060 STARTING AT 15 3
                      ENDING   AT 56 05.
  ELSEIF W_COUNT GT 1.
     CALL SCREEN 0070 STARTING AT 1  3
                      ENDING   AT 65 15.
  ENDIF.

ENDFORM.                    " P4000_GET_GR_INIT_VALUE
*&---------------------------------------------------------------------*
*&      Module  CHECK_GR_POSTING_DATE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_GR_POSTING_DATE INPUT.

  IF OK-CODE EQ 'YES' OR OK-CODE EQ 'ENTR'.
     IF *ZTIVHST-BUDAT IS INITIAL.
        PERFORM NO_INPUT(SAPFMMEX) USING 'ZTIVHST' 'BUDAT'.
     ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_GR_POSTING_DATE  INPUT
*&---------------------------------------------------------------------*
*&      Module  TOTAL_LINE_GET_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE TOTAL_LINE_GET_SCR0070 OUTPUT.

   DESCRIBE TABLE IT_ZSIVHST LINES G_PARAM_LINE.   " LINE 수 GET
   TC_0070-LINES = G_PARAM_LINE.                   " LINE 수 정의.

ENDMODULE.                 " TOTAL_LINE_GET_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_TO_TC0070_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_TO_TC0070_SCR0070 OUTPUT.

  W_LOOPLINES = SY-LOOPC.                             " LOOPING COUNT
* LINE의 유효성 여부 검증.
  IF TC_0070-CURRENT_LINE GT TC_0070-LINES.
     EXIT FROM STEP-LOOP.
  ENDIF.
*Internal Table Read ( Line별 )
  READ TABLE IT_ZSIVHST   INDEX TC_0070-CURRENT_LINE.
  IF SY-SUBRC = 0.                                   " READ SUCCESS?
     MOVE-CORRESPONDING IT_ZSIVHST   TO ZSIVHST. " DATA MOVE
     MOVE ZTIV-BUKRS                 TO ZTIV-BUKRS.
     MOVE IT_ZSIVHST-ZFMARK          TO W_ROW_MARK.
  ENDIF.

ENDMODULE.                 " IT_TO_TC0070_SCR0070  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  IT_GET_LINE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE IT_GET_LINE_SCR0070 INPUT.

  GET CURSOR LINE LINE FIELD F.        "CURSOR_2 = Nummer der
  LINE = TC_0070-CURRENT_LINE + LINE - 1.

ENDMODULE.                 " IT_GET_LINE_SCR0070  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_SCR0070_MARK_TC_0070  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_SCR0070_MARK_TC_0070 INPUT.

   READ TABLE IT_ZSIVHST INDEX TC_0070-CURRENT_LINE.
   IF SY-SUBRC EQ 0.

      IF NOT ( W_ROW_MARK IS INITIAL ).
         IT_ZSIVHST-ZFMARK = 'X'.
      ELSE.
         CLEAR : IT_ZSIVHST-ZFMARK.
      ENDIF.
      MODIFY IT_ZSIVHST  INDEX SY-TABIX.
   ENDIF.

ENDMODULE.                 " SET_SCR0070_MARK_TC_0070  INPUT
