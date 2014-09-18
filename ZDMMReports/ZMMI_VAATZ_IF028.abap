*&---------------------------------------------------------------------*
*& Report  ZMMI_VAATZ_IF028
*&                                                                     *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*

REPORT ZMMI_VAATZ_IF028 MESSAGE-ID ZMM_VAATZ_G
                        NO STANDARD PAGE HEADING.
*&----------------------------------------------------------------------
*&*
*& TASK-ID : IMM523 &*
*& TASK-TITLE : PO Closing Outbound to Vaatz &*
*& TASK-TYPE : ON-LINE &*
*& PROCESS-NO :  51.02.03.02&*
*& PROCESS-NAME : General Material PO &*
*& PROGRAM-ID : ZHINMMO90400 &*
*& T-CODE : ZHINMMO90400 &*
*& PACKAGE : ZHINMM &*
*& DATE : 2011.04.25 &*
*& DEVELOPER : Bae, Byungsung &*
*& GENERAL-INFORMATION :
*   Send PO Closing Info to Vaatz System &*
*& PROCESS-FLOW :
*   1. Read PO Closing information
*      I/F PO Closing Info was sent successfully already,
*      the PO Closing Info can't be sent again.
*      EKPO-ZZTYPE is I/F result field
*   2. Send PO Closing Info
*   3. Save I/F log &*
*& OTHER-CONSIDERATIONS :
*   If the program is running in background mode,
*   the program will perform PO Closing I/F and
*   display result on screen.
*   &*
*&----------------------------------------------------------------------
*-&*
*& VERSION-NO : #1 &*
*& REQUEST :   &*
*& DEVELOPMENT :  &*
*& REQ.-DATE :  &*
*& FINISH-DATE :  &*
*& DESCRIPTION-OF-CHANGE :  &*
*&----------------------------------------------------------------------
*-&*
TABLES: EKPO,
        ZTMM_VAZ_028_LOG.

*---// Internal Tables & Structure
DATA: BEGIN OF IT_PO OCCURS 0,
        EBELN   LIKE   EKKO-EBELN,
        EBELP   LIKE   EKPO-EBELP,
        WERKS   LIKE   EKPO-WERKS,
        LGORT   LIKE   EKPO-LGORT,
        LIFNR   LIKE   EKKO-LIFNR,
        NAME1   LIKE   LFA1-NAME1,
        MATNR   LIKE   EKPO-MATNR,
        TXZ01   LIKE   EKPO-TXZ01,
        MENGE   LIKE   EKPO-MENGE,
        MEINS   LIKE   EKPO-MEINS,
        NETPR   LIKE   EKPO-NETPR,
        PEINH   LIKE   EKPO-PEINH,
        NETWR   LIKE   EKPO-NETWR,
        WAERS   LIKE   EKKO-WAERS,
        ZREASON LIKE   ZTMM_VAZ_VZ028-ZREASON,
        REQNAM  LIKE   ZTMM_VAZ_VZ028-ERNAM,
        REQDAT  LIKE   ZTMM_VAZ_VZ028-ERDAT,
      END   OF IT_PO.

DATA: BEGIN OF ST_LOG.
        INCLUDE STRUCTURE ZTMM_VAZ_028_LOG.
DATA:   ICON(4),
        LGORT   LIKE   EKPO-LGORT,
        LIFNR   LIKE   EKKO-LIFNR,
        NAME1   LIKE   LFA1-NAME1,
        TXZ01   LIKE   EKPO-TXZ01,
        MENGE   LIKE   EKPO-MENGE,
        MEINS   LIKE   EKPO-MEINS,
        NETPR   LIKE   EKPO-NETPR,
        PEINH   LIKE   EKPO-PEINH,
        NETWR   LIKE   EKPO-NETWR,
        WAERS   LIKE   EKKO-WAERS,
      END   OF ST_LOG.

DATA: IT_LOG LIKE ST_LOG OCCURS 0 WITH HEADER LINE.

DATA: IT_PO_CLOSING LIKE ZMMS_VAZ_VZ028 OCCURS 0 WITH HEADER LINE.

DATA: IT_9050 LIKE ZTMM_VAZ_028_LOG OCCURS 0 WITH HEADER LINE.

DATA : IT_FCODE TYPE TABLE OF SY-UCOMM.

*---// Global variables
DATA: G_TOT_CNT   TYPE I,
      G_IFRESULT  TYPE ZRESULT,
      G_IFFAILMSG(255),
      G_TOTAL(5),
      G_SUCCESS(5),
      G_ERROR(5),
      G_ZSEQ      LIKE ZTMM_VAZ_028_LOG-ZSEQ,
      G_LAST_ZSEQ LIKE ZTMM_VAZ_028_LOG-ZSEQ.

RANGES: R_IF_PO FOR EKPO-EBELN.

*-----/// Grid Control : START
* Control Framework Basic Class
CLASS CL_GUI_CFW      DEFINITION LOAD.

* Splitter Declaration
DATA: WC_SPLITTER_LOG     TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
      WC_SP_CONTAINER_LOG TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      WC_CONTROL_LOG      TYPE        SCRFNAME VALUE 'CC_LOG'.

* Declare for Grid
DATA: WC_CONTAINER_LOG TYPE REF TO CL_GUI_CONTAINER,
      WC_GRID_LOG      TYPE REF TO CL_GUI_ALV_GRID,
      G_LAYOUT_LOG     TYPE LVC_S_LAYO,
      G_VARIANT_LOG    TYPE DISVARIANT,
      IT_SORT_LOG      TYPE LVC_T_SORT   WITH HEADER LINE,
      IT_FILTER_LOG    TYPE LVC_T_FILT   WITH HEADER LINE,
      IT_EXCLUDE_LOG   TYPE UI_FUNCTIONS WITH HEADER LINE.

CLASS LCL_EVENT_RECEIVER DEFINITION DEFERRED.

DATA : EVENT_RECEIVER TYPE REF TO LCL_EVENT_RECEIVER.

* Interal tables for ALV GRID
DATA : IT_ROWS         TYPE LVC_T_ROW  WITH HEADER LINE,
       IT_ROW_NO       TYPE LVC_T_ROID WITH HEADER LINE,
       IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV WITH HEADER LINE.

* Global variable for ALV GRID
DATA : G_FIELDNAME   TYPE LINE OF SLIS_T_FIELDCAT_ALV,
       G_REPID       LIKE SY-REPID,
       G_CNT         TYPE I,                   "Field count
       G_SCROLL      TYPE LVC_S_STBL,
       G_SAVE        TYPE C   VALUE 'A'.       "for Parameter I_SAVE
*/-   Saving Options for Layouts
*SPACE- Layouts cannot be saved.
*'U'  - Only user-defined layouts can be saved.
*'X'  - Only global layouts can be saved.
*'A'  - Both user-defined and global layouts can be saved

DATA: G_CONTAINER(100),
      G_CONTROL(100),
      G_SPLITTER(100),
      G_GRID(100),
      G_ITAB(100),
      G_STRUCTURE LIKE DD02L-TABNAME.

FIELD-SYMBOLS: <FS_CONTAINER> TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
               <FS_CONTROL>   TYPE        SCRFNAME,
               <FS_SPLITTER>  TYPE REF TO CL_GUI_SPLITTER_CONTAINER,
               <FS_GRID>      TYPE REF TO CL_GUI_ALV_GRID,
               <FS_ITAB>      TYPE STANDARD TABLE,
               <FS_ITAB_OLD>  TYPE STANDARD TABLE.

CONSTANTS: C_STRUCTURE(100) VALUE 'ST_'.

****************************************************************
* LOCAL CLASSES: Definition for Event Handling
****************************************************************
CLASS LCL_EVENT_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS: HOTSPOT_LOG
        FOR  EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
             IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO.
*
*    METHODS: double_click_0101
*        FOR  EVENT double_click OF cl_gui_alv_grid
*             IMPORTING e_row e_column es_row_no.
*
*    METHODS: hotspot_click_0102
*        FOR  EVENT hotspot_click OF cl_gui_alv_grid
*             IMPORTING e_row_id e_column_id es_row_no.
*
*    METHODS: hotspot_click_0107
*        FOR  EVENT hotspot_click OF cl_gui_alv_grid
*             IMPORTING e_row_id e_column_id es_row_no.

*    METHODS: data_changed_itab
*        FOR  EVENT data_changed OF cl_gui_alv_grid
*             IMPORTING er_data_changed  e_ucomm.
*
*    METHODS: toolbar_itab
*        FOR  EVENT toolbar OF cl_gui_alv_grid
*             IMPORTING e_object e_interactive.
*
*    METHODS: user_command_itab
*             FOR EVENT user_command OF cl_gui_alv_grid
*             IMPORTING e_ucomm.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_event_receiver DEFINITION
****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

  METHOD HOTSPOT_LOG.
    PERFORM HOTSPOT_LOG USING E_ROW_ID-INDEX E_COLUMN_ID ES_ROW_NO.
  ENDMETHOD.                           "handle_double_click
*
*  METHOD double_click_0101.
*    PERFORM dbl_click_0101 USING e_column-fieldname
*                                 es_row_no-row_id.
*  ENDMETHOD.                           "handle_double_click
*
*  METHOD hotspot_click_0102.
*    PERFORM hotspot_click_0102 USING e_row_id
*                                     e_column_id
*                                     es_row_no.
*  ENDMETHOD.                           "handle_double_click
*
*  METHOD hotspot_click_0107.
*    PERFORM hotspot_click_0107 USING e_row_id
*                                     e_column_id
*                                     es_row_no.
*  ENDMETHOD.                           "handle_double_click

*  METHOD data_changed_itab.
*    PERFORM data_changed_itab USING er_data_changed
*                                    e_ucomm.
*  ENDMETHOD.                    "handle_data_changed
*
*
*  METHOD toolbar_itab.
*    PERFORM toolbar_itab USING e_object e_interactive.
*  ENDMETHOD.                    "handle_toolbar
*
*  METHOD user_command_itab.
*    PERFORM user_command_itab USING  e_ucomm.
*  ENDMETHOD.                           "handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&  Selection Screen
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_WERKS FOR EKPO-WERKS,
                S_EBELN FOR EKPO-EBELN,
                S_MATNR FOR EKPO-MATNR.
SELECTION-SCREEN END OF BLOCK BLOCK1.

INITIALIZATION.

AT SELECTION-SCREEN.
  PERFORM LOCKING_RTN.

START-OF-SELECTION.
  PERFORM READ_DATA.

END-OF-SELECTION.
*& PROGRAM-LOGIC : #0002 Make I/F Log Table &*
*& PROGRAM-DESC : Append internal table IT_LOG for table ZHINMMT9050 &*
  PERFORM MAKE_IF_LOG.
*& END-PROGRAM-LOGIC : &*

  IF SY-BATCH IS INITIAL.     "On-line mode
    READ TABLE IT_LOG INDEX 1.
    IF SY-SUBRC NE 0.
      MESSAGE S005. EXIT.
    ENDIF.

    CALL SCREEN 100.
  ELSE.                       "Batchjob mode
    READ TABLE IT_LOG INDEX 1.
    IF SY-SUBRC NE 0.
      MESSAGE S005. EXIT.
    ENDIF.

    PERFORM SEND_RTN.

    CALL SCREEN 100.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA .
*& PROGRAM-LOGIC : #0001 Read PO Closing INfo for I/F &*
*& PROGRAM-DESC : Read PO Closing infomation for I/F to Vaatz system
*                 EKPO-ZZTYPE is I/F result field.
*                 Read PO Closing what ZZTYPE is 'E' or space. &*
  SELECT B~EBELN B~EBELP B~WERKS B~LGORT C~LIFNR D~NAME1
         B~MATNR B~TXZ01 B~MENGE B~MEINS C~WAERS
         B~NETPR B~PEINH B~NETWR
         A~ZREASON A~ERNAM AS REQNAM A~ERDAT AS REQDAT
    INTO CORRESPONDING FIELDS OF TABLE IT_PO
    FROM ZTMM_VAZ_VZ028 AS A INNER JOIN EKPO AS B
                             ON B~EBELN = A~EBELN
                            AND B~EBELP = A~EBELP
                          INNER JOIN EKKO AS C
                             ON C~EBELN = B~EBELN
                          INNER JOIN LFA1 AS D
                             ON D~LIFNR = C~LIFNR
   WHERE A~WERKS   IN S_WERKS
     AND A~EBELN   IN S_EBELN
     AND A~MATNR   IN S_MATNR
     AND A~ZIF_IND IN (SPACE,'E').
*     AND b~zztype  IN (space,'E')
*     AND b~elikz   = 'X'.

*  SELECT a~ebeln a~ebelp a~werks a~lgort b~lifnr c~name1
*         a~matnr a~txz01 a~menge a~meins b~waers
*         a~netpr a~peinh a~netwr
*    INTO CORRESPONDING FIELDS OF TABLE it_po
*    FROM ekpo AS a INNER JOIN ekko AS b
*                      ON b~ebeln = a~ebeln
*                   INNER JOIN lfa1 AS c
*                      ON c~lifnr = b~lifnr
*   WHERE a~zztype IN (space,'E')
*     AND A~ELIKZ = 'X'
*     AND a~werks IN s_werks
*     AND a~ebeln IN s_ebeln
*     AND a~matnr IN s_matnr.
*& END-PROGRAM-LOGIC : &*
ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_SEQNO_TIMESTAMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_SEQNO.
  " Read I/F Seq NO
  CLEAR: G_ZSEQ, G_LAST_ZSEQ.

  SELECT SINGLE MAX( ZSEQ ) INTO G_LAST_ZSEQ
    FROM ZTMM_VAZ_028_LOG
    WHERE EBELN = IT_LOG-EBELN.

  G_ZSEQ = G_LAST_ZSEQ + 1.
ENDFORM.                    " READ_SEQNO_TIMESTAMP



*&---------------------------------------------------------------------*
*&      Module  STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS OUTPUT.
  SET PF-STATUS 'G0100' EXCLUDING IT_FCODE.
  SET TITLEBAR  'T0100'.
ENDMODULE.                 " STATUS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE EXIT INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT' OR 'CANC'.
      CLEAR: SY-UCOMM.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CREATE_CONTROL OUTPUT.
  PERFORM CREATE_SPLITTER USING 'LOG'.
  PERFORM CREATE_GRID USING 'LOG'  '1' '1'.
ENDMODULE.                 " CREATE_CONTROL  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_SPLITTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0333   text
*----------------------------------------------------------------------*
FORM CREATE_SPLITTER USING P_DYNNR.
  CONCATENATE: 'WC_SP_CONTAINER_' P_DYNNR INTO G_CONTAINER.
  ASSIGN:      (G_CONTAINER)               TO   <FS_CONTAINER>.

  CONCATENATE: 'WC_CONTROL_' P_DYNNR INTO G_CONTROL.
  ASSIGN:      (G_CONTROL)            TO   <FS_CONTROL>.

  CONCATENATE: 'WC_SPLITTER_' P_DYNNR INTO G_SPLITTER.
  ASSIGN:      (G_SPLITTER)            TO   <FS_SPLITTER>.


  IF <FS_CONTAINER> IS INITIAL.
    CREATE OBJECT <FS_CONTAINER>
      EXPORTING
        CONTAINER_NAME              = <FS_CONTROL>
      EXCEPTIONS
        CNTL_ERROR                  = 1
        CNTL_SYSTEM_ERROR           = 2
        CREATE_ERROR                = 3
        LIFETIME_ERROR              = 4
        LIFETIME_DYNPRO_DYNPRO_LINK = 5
        OTHERS                      = 6.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CREATE OBJECT <FS_SPLITTER>
      EXPORTING
        PARENT            = <FS_CONTAINER>
        ROWS              = 1
        COLUMNS           = 1
      EXCEPTIONS
        CNTL_ERROR        = 1
        CNTL_SYSTEM_ERROR = 2
        OTHERS            = 3.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_SPLITTER
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0337   text
*      -->P_0338   text
*      -->P_0339   text
*----------------------------------------------------------------------*
FORM CREATE_GRID USING P_GRID P_ROW P_COLUMN.
  DATA: L_CONTAINER(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO CL_GUI_CONTAINER.

  CONCATENATE: 'WC_CONTAINER_' P_GRID INTO L_CONTAINER.
  ASSIGN:      (L_CONTAINER)          TO   <LFS_CONTAINER>.

  IF <LFS_CONTAINER> IS INITIAL.
    PERFORM CREATE_GRID_CONTAINER USING P_GRID P_ROW P_COLUMN.
    PERFORM BUILD_FIELD_CATALOG   USING P_GRID.
    PERFORM SET_ATTRIBUTE         USING P_GRID.
    PERFORM ASSIGN_ITAB_TO_ALV    USING P_GRID.
    PERFORM ASSIGN_EVENT          USING P_GRID.
  ELSE.
    PERFORM REFRESH_GRID USING P_GRID.
  ENDIF.
ENDFORM.                    " CREATE_GRID
*&---------------------------------------------------------------------*
*&      Form  CREATE_GRID_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*      -->P_PV_ROW  text
*      -->P_PV_COLUMN  text
*----------------------------------------------------------------------*
FORM CREATE_GRID_CONTAINER USING P_GRID P_ROW P_COLUMN.
  DATA: L_CONTAINER(100),
        L_GRID(100).

  FIELD-SYMBOLS: <LFS_CONTAINER> TYPE REF TO   CL_GUI_CONTAINER,
                 <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_SPLITTER_' P_GRID  INTO G_SPLITTER.
  ASSIGN:      (G_SPLITTER)            TO   <FS_SPLITTER>.

  CONCATENATE: 'WC_CONTAINER_' P_GRID INTO L_CONTAINER.
  ASSIGN:      (L_CONTAINER)          TO   <LFS_CONTAINER>.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CALL METHOD <FS_SPLITTER>->GET_CONTAINER
    EXPORTING
      ROW       = P_ROW
      COLUMN    = P_COLUMN
    RECEIVING
      CONTAINER = <LFS_CONTAINER>.

  CREATE OBJECT <LFS_GRID>
    EXPORTING
      I_PARENT          = <LFS_CONTAINER>
    EXCEPTIONS
      ERROR_CNTL_CREATE = 1
      ERROR_CNTL_INIT   = 2
      ERROR_CNTL_LINK   = 3
      ERROR_DP_CREATE   = 4
      OTHERS            = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CASE P_ROW.
    WHEN 1.
      CALL METHOD <FS_SPLITTER>->SET_ROW_HEIGHT
        EXPORTING
          ID                = P_ROW
          HEIGHT            = 20
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
    WHEN 2.
      CALL METHOD <FS_SPLITTER>->SET_ROW_HEIGHT
        EXPORTING
          ID                = P_ROW
          HEIGHT            = 36
        EXCEPTIONS
          CNTL_ERROR        = 1
          CNTL_SYSTEM_ERROR = 2
          OTHERS            = 3.
      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
  ENDCASE.
ENDFORM.                    " CREATE_GRID_CONTAINER
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_GRID.
*--- adjust field catalog to suppress the output of already
*  displayed key fields of structure

  DATA: L_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

*  CALL METHOD <alv>->get_frontend_fieldcatalog
*    IMPORTING
*      et_fieldcatalog = it_fieldcat[].

  PERFORM SET_FIELDNAME USING P_GRID.
  PERFORM SET_SCREEN_FIELDS USING P_GRID.

  CALL METHOD <LFS_GRID>->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = IT_FIELDCAT[].
ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  SET_FIELDNAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_FIELDNAME  USING P_GRID.

  DATA: L_ITAB TYPE SLIS_TABNAME.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].

  MOVE: SY-REPID TO G_REPID.
  CONCATENATE C_STRUCTURE P_GRID INTO L_ITAB.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = G_REPID
            I_INTERNAL_TABNAME = L_ITAB
            I_INCLNAME         = G_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME[].
ENDFORM.                    " SET_FIELDNAME
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS USING P_GRID.
  CASE P_GRID.
    WHEN 'LOG'.
      PERFORM SET_SCREEN_FIELDS_LOG.
  ENDCASE.
ENDFORM.                    " SET_SCREEN_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN_FIELDS_WO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_SCREEN_FIELDS_LOG .
  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :
                                  'S' 'EBELN'       ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'EBELN',
                                  ' ' 'FIX_COLUMN'   'X',
                                  ' ' 'HOTSPOT'      'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'ZSEQ'         ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'ZSEQ',
                                  ' ' 'COLTEXT'      'Seq.',
*                                  ' ' 'OUTPUTLEN'    '5',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'EBELP'          ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'EBELP',
*                                  ' ' 'OUTPUTLEN'    '6',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'ICON'         ' ',
                                  ' ' 'COLTEXT'      'Status',
                                  ' ' 'OUTPUTLEN'    '6',
                                  ' ' 'FIX_COLUMN'   'X',
                                  'E' 'EMPHASIZE'    'C100',

                                  'S' 'REQDAT'       ' ',
                                  ' ' 'COLTEXT'     'Req. Date',
                                  ' ' 'OUTPUTLEN'    '10',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'REQNAM'       ' ',
                                  ' ' 'COLTEXT'     'Requestor',
                                  ' ' 'OUTPUTLEN'    '10',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ZREASON'     ' ',
                                  ' ' 'COLTEXT'     'Reason',
                                  ' ' 'OUTPUTLEN'    '40',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ERDAT'       ' ',
                                  ' ' 'COLTEXT'     'I/F Date',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ERZET'       ' ',
                                  ' ' 'COLTEXT'     'I/F Time',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ZCCORPCD'    ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'ZCCORPCD',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ZCSVCGN'     ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'ZCSVCGN',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ZCPLTCD'    ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'ZCPLTCD',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ZNPATNR'    ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'ZNPATNR',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'TXZ01'    ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'TXZ01',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'ZCMODER'    ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'ZCMODER',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'LGORT'    ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'LGORT',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'LIFNR'    ' ',
                                  ' ' 'REF_TABLE'   'EKKO',
                                  ' ' 'REF_FIELD'   'LIFNR',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'NAME1'    ' ',
                                  ' ' 'REF_TABLE'   'LFA1',
                                  ' ' 'REF_FIELD'   'NAME1',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'MENGE'    ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'MENGE',
                                  ' ' 'QFIELDNAME'  'MEINS',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'MEINS'    ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'MEINS',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'NETPR'        ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'NETPR',
                                  ' ' 'CFIELDNAME'  'WAERS',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'PEINH'        ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'PEINH',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'NETWR'        ' ',
                                  ' ' 'REF_TABLE'   'EKPO',
                                  ' ' 'REF_FIELD'   'NETWR',
                                  ' ' 'CFIELDNAME'  'WAERS',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'WAERS'        ' ',
                                  ' ' 'REF_TABLE'   'EKKO',
                                  ' ' 'REF_FIELD'   'WAERS',
                                  ' ' 'CFIELDNAME'  'WAERS',
                                  'E' 'EMPHASIZE'   'C250',

                                  'S' 'IFFAILMSG'    ' ',
                                  ' ' 'REF_TABLE'   'ZTMM_VAZ_028_LOG',
                                  ' ' 'REF_FIELD'   'IFFAILMSG',
                                  'E' 'EMPHASIZE'   'C250'.

ENDFORM.                    " SET_SCREEN_FIELDS_WO
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0817   text
*      -->P_0818   text
*      -->P_0819   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   PT_FIELDCAT  STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.

  DATA : LV_COL(40).

  FIELD-SYMBOLS <FS>.

* START - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'S'.
    CLEAR: PT_FIELDCAT.

    READ TABLE IT_FIELDNAME INTO G_FIELDNAME
                            WITH KEY FIELDNAME  = P_FIELD.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH 'Check filed catalog:' P_FIELD.
    ENDIF.

    MOVE: G_FIELDNAME-FIELDNAME TO PT_FIELDCAT-FIELDNAME.
    EXIT.
  ENDIF.

* Setting The Field's Attributes
  CONCATENATE 'PT_FIELDCAT-' P_FIELD  INTO LV_COL.
  ASSIGN (LV_COL) TO <FS>.
  MOVE   P_VALUE  TO <FS>.

* END - FIELD ATTRIBUTE SETTING
  IF P_GUBUN = 'E'.
    IF PT_FIELDCAT-COL_POS IS INITIAL.
      ADD 1 TO G_CNT.
      PT_FIELDCAT-COL_POS = G_CNT.
    ENDIF.
    APPEND PT_FIELDCAT.
  ENDIF.
ENDFORM.                    " SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTE USING P_GRID.
  PERFORM SET_LAYOUT                USING P_GRID.
  PERFORM SET_VARIANT               USING P_GRID.
  PERFORM SET_SORT_TOTAL_FIELD      USING P_GRID.
  PERFORM SET_FILTER                USING P_GRID.
ENDFORM.                    " SET_ATTRIBUTE
*&---------------------------------------------------------------------*
*&      Form  SET_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT USING P_GRID.
  DATA: L_GRID(100),
        L_LAYOUT(100).

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_LAYOUT> TYPE LVC_S_LAYO.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'G_LAYOUT_' P_GRID INTO L_LAYOUT.
  ASSIGN:      (L_LAYOUT)         TO   <LFS_LAYOUT>.

  CALL METHOD <LFS_GRID>->GET_FRONTEND_LAYOUT
    IMPORTING
      ES_LAYOUT = <LFS_LAYOUT>.

  IF <LFS_LAYOUT> IS INITIAL.

    CASE P_GRID.
      WHEN 'LOG'.
        <LFS_LAYOUT>-EDIT       = ' '.     " Edit Mode Enable
        <LFS_LAYOUT>-SEL_MODE   = 'A'.     " mode for select col and row
        <LFS_LAYOUT>-LANGUAGE   = SY-LANGU." Language Key
        <LFS_LAYOUT>-TOTALS_BEF = 'X'.     " Upper Total Line
        <LFS_LAYOUT>-ZEBRA      = 'X'.         " Emphasize C250
*        <lfs_layout>-totals_bef = 'X'.        " Upper Total Line
*        <lfs_layout>-no_totline = ' '.        " Disable Total Line
        <LFS_LAYOUT>-INFO_FNAME = 'ROW_COLOR'. " Line color field
    ENDCASE.
  ENDIF.

  CALL METHOD <LFS_GRID>->SET_FRONTEND_LAYOUT
    EXPORTING
      IS_LAYOUT = <LFS_LAYOUT>.
ENDFORM.                    " SET_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  SET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_VARIANT USING P_GRID.
  DATA: L_GRID(100),
        L_VARIANT(100).

  FIELD-SYMBOLS: <LFS_GRID>    TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_VARIANT> TYPE DISVARIANT.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'G_VARIANT_' P_GRID INTO L_VARIANT.
  ASSIGN:      (L_VARIANT)         TO   <LFS_VARIANT>.


  CALL METHOD <LFS_GRID>->GET_VARIANT
    IMPORTING
      ES_VARIANT = <LFS_VARIANT>.

  IF <LFS_VARIANT> IS INITIAL.
    <LFS_VARIANT>-REPORT      = SY-REPID.
    <LFS_VARIANT>-HANDLE      = SPACE.
    <LFS_VARIANT>-LOG_GROUP   = SPACE.
    <LFS_VARIANT>-USERNAME    = SPACE.
    <LFS_VARIANT>-VARIANT     = SPACE.
    <LFS_VARIANT>-TEXT        = SPACE.
*    <lfs_variant>-dependvars  = space.
  ENDIF.
ENDFORM.                    " SET_VARIANT
*&---------------------------------------------------------------------*
*&      Form  SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_SORT_TOTAL_FIELD USING P_GRID.
  DATA: L_GRID(100),
        L_SORT(100),
        LST_SORT LIKE IT_SORT_LOG.

  FIELD-SYMBOLS: <LFS_GRID> TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_SORT> TYPE LVC_T_SORT.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'IT_SORT_' P_GRID '[]' INTO L_SORT.
  ASSIGN:      (L_SORT)               TO   <LFS_SORT>.

  CALL METHOD <LFS_GRID>->GET_SORT_CRITERIA
    IMPORTING
      ET_SORT = <LFS_SORT>.

  CASE P_GRID.
    WHEN 'LOG'.
      REFRESH <LFS_SORT>.

      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'EBELN'.
      APPEND LST_SORT TO <LFS_SORT>.

      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'ZSEQ'.
      APPEND LST_SORT TO <LFS_SORT>.

      CLEAR : LST_SORT.
      LST_SORT-FIELDNAME = 'EBELP'.
      APPEND LST_SORT TO <LFS_SORT>.
  ENDCASE.

  CALL METHOD <LFS_GRID>->SET_SORT_CRITERIA
    EXPORTING
      IT_SORT = <LFS_SORT>.
ENDFORM.                    " SET_SORT_TOTAL_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_FILTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM SET_FILTER  USING P_GRID.
  DATA: L_GRID(100),
        L_FILTER(100).

  FIELD-SYMBOLS: <LFS_GRID>   TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_FILTER> TYPE LVC_T_FILT.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'IT_FILTER_' P_GRID '[]' INTO L_FILTER.
  ASSIGN:      (L_FILTER)               TO   <LFS_FILTER>.

  CALL METHOD <LFS_GRID>->GET_FILTER_CRITERIA
    IMPORTING
      ET_FILTER = <LFS_FILTER>.

  IF <LFS_FILTER> IS INITIAL.
*    CASE pv_grid.
*      WHEN 'KEY_SUM'.
*        CLEAR : it_filter_key_sum. REFRESH it_filter_key_sum.
*    ENDCASE.
  ENDIF.
ENDFORM.                    " SET_FILTER
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV USING P_GRID.
  DATA: L_GRID(100),
        L_ITAB(100),
        L_LAYOUT(100),
        L_VARIANT(100),
        L_SORT(100),
        L_FILTER(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID,
                 <LFS_ITAB>  TYPE STANDARD TABLE,
                 <LFS_LAYOUT> TYPE LVC_S_LAYO,
                 <LFS_VARIANT> TYPE DISVARIANT,
                 <LFS_SORT> TYPE LVC_T_SORT,
                 <LFS_FILTER> TYPE LVC_T_FILT.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  CONCATENATE: 'G_LAYOUT_' P_GRID INTO L_LAYOUT.
  ASSIGN:      (L_LAYOUT)         TO   <LFS_LAYOUT>.

  CONCATENATE: 'G_VARIANT_' P_GRID INTO L_VARIANT.
  ASSIGN:      (L_VARIANT)         TO   <LFS_VARIANT>.

  CONCATENATE: 'IT_'      P_GRID '[]' INTO L_ITAB.
  ASSIGN:      (L_ITAB)               TO   <LFS_ITAB>.

  CONCATENATE: 'IT_SORT_' P_GRID '[]' INTO L_SORT.
  ASSIGN:      (L_SORT)               TO   <LFS_SORT>.

  CONCATENATE: 'IT_FILTER_' P_GRID '[]' INTO L_FILTER.
  ASSIGN:      (L_FILTER)               TO   <LFS_FILTER>.

  CONCATENATE: C_STRUCTURE P_GRID INTO G_STRUCTURE.

  CALL METHOD <LFS_GRID>->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      I_STRUCTURE_NAME              = G_STRUCTURE
      IS_LAYOUT                     = <LFS_LAYOUT>
      I_SAVE                        = G_SAVE
      IS_VARIANT                    = <LFS_VARIANT>
      I_DEFAULT                     = SPACE
    CHANGING
      IT_FIELDCATALOG               = IT_FIELDCAT[]
      IT_FILTER                     = <LFS_FILTER>
      IT_SORT                       = <LFS_SORT>
      IT_OUTTAB                     = <LFS_ITAB>
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM ASSIGN_EVENT USING P_GRID.
  DATA: L_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>  TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

*--  Regist event for Edit
  CALL METHOD <LFS_GRID>->REGISTER_EDIT_EVENT
    EXPORTING
      I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.

  CREATE OBJECT EVENT_RECEIVER.

  CASE P_GRID.
    WHEN 'LOG'.
      SET HANDLER EVENT_RECEIVER->HOTSPOT_LOG FOR <LFS_GRID>.
*    SET HANDLER event_receiver->handle_toolbar FOR <lfs_grid>.
    WHEN 'WO_DETAIL'.
*    SET HANDLER event_receiver->double_click_wo_detail  FOR <lfs_grid>.
*      SET HANDLER event_receiver->click_wo_detail FOR <lfs_grid>.
  ENDCASE.

  CALL METHOD <LFS_GRID>->SET_TOOLBAR_INTERACTIVE.
ENDFORM.                    " ASSIGN_EVENT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_GRID  text
*----------------------------------------------------------------------*
FORM REFRESH_GRID USING P_GRID.
  DATA: L_GRID(100).

  FIELD-SYMBOLS: <LFS_GRID>      TYPE REF TO   CL_GUI_ALV_GRID.

  CONCATENATE: 'WC_GRID_' P_GRID INTO L_GRID.
  ASSIGN:      (L_GRID)          TO   <LFS_GRID>.

  G_SCROLL-ROW = 'X'.
  G_SCROLL-COL = 'X'.

  CALL METHOD <LFS_GRID>->REFRESH_TABLE_DISPLAY
    EXPORTING
*      i_soft_refresh = 'X'
      IS_STABLE      = G_SCROLL.     "## ### ### refresh
ENDFORM.                    " REFRESH_GRID
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  CASE SY-UCOMM.
    WHEN 'BACK'.
      CLEAR SY-UCOMM.
      LEAVE TO SCREEN 0.
    WHEN 'SEND'.
      CLEAR: SY-UCOMM.
      PERFORM SEND_ONLINE_RTN.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  SEND_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_ONLINE_RTN .
  "/Indexes of Selected Rows

  DATA: L_ANS,
        L_SUBRC LIKE SY-SUBRC,
        L_TABIX LIKE SY-TABIX.

  CLEAR: IT_ROWS, IT_ROWS[], IT_ROW_NO, IT_ROW_NO[].
  CLEAR: R_IF_PO, R_IF_PO[].

  CALL METHOD WC_GRID_LOG->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = IT_ROWS[]
      ET_ROW_NO     = IT_ROW_NO[].

  CALL METHOD CL_GUI_CFW=>FLUSH.
  IF SY-SUBRC NE 0.
    G_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = G_REPID
              TXT2  = SY-SUBRC
              TXT1 =
                 'Error founded during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.

  DELETE IT_ROWS WHERE INDEX EQ 0.

  LOOP AT IT_ROWS.
    READ TABLE IT_LOG INDEX IT_ROWS-INDEX.
    IF SY-SUBRC NE 0.
      MESSAGE E002.
    ENDIF.

    MOVE: IT_LOG-EBELN TO R_IF_PO-LOW.

    COLLECT R_IF_PO.
  ENDLOOP.
  IF SY-SUBRC NE 0.
    MESSAGE S016.
    LEAVE TO SCREEN SY-DYNNR.
  ENDIF.

  MOVE: 'I'  TO R_IF_PO-SIGN,
        'EQ' TO R_IF_PO-OPTION.

  MODIFY R_IF_PO TRANSPORTING SIGN OPTION
                 WHERE LOW >= SPACE.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
       EXPORTING
            TITLEBAR       = 'Transfer Confirmation'
            TEXT_QUESTION  = 'Do you want to send PO Closing to G/PRO?'
            TEXT_BUTTON_1  = 'Yes'
            TEXT_BUTTON_2  = 'No'
            DEFAULT_BUTTON = '2'
       IMPORTING
            ANSWER         = L_ANS.

  CHECK L_ANS EQ '1'.

  PERFORM SEND_RTN.

ENDFORM.                    " SEND_RTN
*&---------------------------------------------------------------------*
*&      Form  CALL_EAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_EAI .
  DATA: L_MSGTXT(100).

  CALL FUNCTION 'ZMMF_VAZ_IF_PO_CLOSING'
    DESTINATION 'WMHR01'
*    EXPORTING
*      I_TOT_CNT             = G_TOT_CNT
*      I_CALL_BY             = 'SAP'
*      I_LAST_REC            = 'Y'
    IMPORTING
      E_IFRESULT            = G_IFRESULT
      E_IFFAILMSG           = G_IFFAILMSG
    TABLES
      T_PO_CLOSING          = IT_PO_CLOSING
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT
      OTHERS                = 3.
  CASE SY-SUBRC.
    WHEN 0.
      CASE G_IFRESULT.
*        WHEN 'Z'.
        WHEN 'S'.
*          MOVE: ICON_GREEN_LIGHT TO IT_LOG-ICON,
          MOVE:  G_IFRESULT       TO IT_LOG-IFRESULT,
               G_IFFAILMSG      TO IT_LOG-IFFAILMSG.
          G_SUCCESS = G_SUCCESS + 1.
        WHEN 'E'.
*          MOVE: ICON_RED_LIGHT   TO IT_LOG-ICON,
          MOVE:      G_IFRESULT       TO IT_LOG-IFRESULT,
                G_IFFAILMSG      TO IT_LOG-IFFAILMSG.
        WHEN OTHERS.
*          MOVE: ICON_RED_LIGHT   TO IT_LOG-ICON,
          MOVE:       'E'              TO IT_LOG-IFRESULT.
          CONCATENATE 'Unknown I/F result from EAI. IFRESULT:'
                      G_IFRESULT
                 INTO IT_LOG-IFFAILMSG.
      ENDCASE.
    WHEN 1.
*      MOVE: ICON_RED_LIGHT TO IT_LOG-ICON,
      MOVE:  'E'            TO IT_LOG-IFRESULT.
      CONCATENATE '[Network Err]' L_MSGTXT INTO IT_LOG-IFFAILMSG
        SEPARATED BY SPACE.
    WHEN 2.
*      MOVE: ICON_RED_LIGHT TO IT_LOG-ICON,
      MOVE:      'E'            TO IT_LOG-IFRESULT.
      CONCATENATE '[EAI Err]' L_MSGTXT INTO IT_LOG-IFFAILMSG
        SEPARATED BY SPACE.
    WHEN 3.
*      MOVE: ICON_RED_LIGHT  TO IT_LOG-ICON,
      MOVE:     'E'             TO IT_LOG-IFRESULT,
           'Unknown error' TO IT_LOG-IFFAILMSG.
  ENDCASE.

*& PROGRAM-LOGIC : #0003 Read Seq. No &*
*& PROGRAM-DESC : Seq No is increased by PO No
  PERFORM READ_SEQNO.

*& END-PROGRAM-LOGIC : &*

  MOVE: G_ZSEQ TO IT_LOG-ZSEQ.

  MODIFY IT_LOG TRANSPORTING ICON IFRESULT IFFAILMSG ZSEQ
                WHERE EBELN = IT_LOG-EBELN.

  MOVE: IT_LOG-IFRESULT  TO IT_9050-IFRESULT,
        IT_LOG-IFFAILMSG TO IT_9050-IFFAILMSG,
        IT_LOG-ZSEQ      TO IT_9050-ZSEQ.

  MODIFY IT_9050 TRANSPORTING IFRESULT IFFAILMSG ZSEQ
                WHERE EBELN = IT_LOG-EBELN.
ENDFORM.                    " CALL_EAI
*&---------------------------------------------------------------------*
*&      Form  SEND_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_RTN .
  MOVE '0' TO: G_SUCCESS, G_ERROR, G_TOTAL.

  SORT IT_LOG BY EBELN EBELP.
  LOOP AT IT_LOG WHERE EBELN IN R_IF_PO.
    AT NEW EBELN.
      REFRESH: IT_PO_CLOSING, IT_9050.
    ENDAT.

    MOVE: SY-UNAME TO IT_LOG-ERNAM,
          SY-DATUM TO IT_LOG-ERDAT,
          SY-UZEIT TO IT_LOG-ERZET.

    CLEAR: IT_PO_CLOSING.
    MOVE-CORRESPONDING IT_LOG TO IT_PO_CLOSING.
    APPEND IT_PO_CLOSING.

    CLEAR: IT_9050.
    MOVE-CORRESPONDING IT_LOG TO IT_9050.
    APPEND IT_9050.

    MODIFY IT_LOG.

    G_TOT_CNT = G_TOT_CNT + 1.
    G_TOTAL   = G_TOTAL   + 1.

    AT END OF EBELN.
      PERFORM CALL_EAI.
      PERFORM UPDATE_TABLE.
    ENDAT.
  ENDLOOP.

  G_ERROR = G_TOTAL - G_SUCCESS.
  MESSAGE S003 WITH G_TOTAL G_SUCCESS G_ERROR.

  REFRESH IT_FCODE. CLEAR: IT_FCODE.
  APPEND: 'SEND'  TO IT_FCODE.
ENDFORM.                    " SEND_RTN
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE .
*  IF G_IFRESULT EQ 'Z'.
  IF G_IFRESULT EQ 'S'.
    LOOP AT IT_PO_CLOSING.
      UPDATE EKPO
         SET ZZTYPE = G_IFRESULT
       WHERE EBELN = IT_PO_CLOSING-EBELN
         AND EBELP = IT_PO_CLOSING-EBELP.
      IF SY-SUBRC NE 0.
*        MOVE: ICON_RED_LIGHT        TO IT_LOG-ICON,
        MOVE:     'E'                   TO IT_LOG-IFRESULT,
             'EKPO Update failed.' TO IT_LOG-IFFAILMSG.

        MODIFY IT_LOG TRANSPORTING ICON IFRESULT IFFAILMSG
                      WHERE EBELN = IT_PO_CLOSING-EBELN
                        AND EBELP = IT_PO_CLOSING-EBELP.

        MOVE: 'E'                   TO IT_9050-IFRESULT,
              'EKPO Update failed.' TO IT_9050-IFFAILMSG.

        MODIFY IT_9050 TRANSPORTING IFRESULT IFFAILMSG
                      WHERE EBELN = IT_PO_CLOSING-EBELN
                        AND EBELP = IT_PO_CLOSING-EBELP.
      ENDIF.

      UPDATE ZTMM_VAZ_VZ028
         SET ZIF_IND = G_IFRESULT
       WHERE EBELN = IT_PO_CLOSING-EBELN
         AND EBELP = IT_PO_CLOSING-EBELP.
      IF SY-SUBRC NE 0.
*        MOVE: ICON_RED_LIGHT        TO IT_LOG-ICON,
        MOVE:      'E'                   TO IT_LOG-IFRESULT,
              'ZTMM_VAZ_028_LOG Update failed.' TO IT_LOG-IFFAILMSG.

        MODIFY IT_LOG TRANSPORTING ICON IFRESULT IFFAILMSG
                      WHERE EBELN = IT_PO_CLOSING-EBELN
                        AND EBELP = IT_PO_CLOSING-EBELP.

        MOVE: 'E'                   TO IT_9050-IFRESULT,
              'ZTMM_VAZ_028_LOG Update failed.' TO IT_9050-IFFAILMSG.

        MODIFY IT_9050 TRANSPORTING IFRESULT IFFAILMSG
                      WHERE EBELN = IT_PO_CLOSING-EBELN
                        AND EBELP = IT_PO_CLOSING-EBELP.
      ENDIF.

    ENDLOOP.
  ENDIF.

  INSERT ZTMM_VAZ_028_LOG  FROM TABLE IT_9050 ACCEPTING DUPLICATE KEYS.
  IF SY-SUBRC NE 0.
    ROLLBACK WORK.
    MESSAGE E000 WITH 'ZTMM_VAZ_028_LOG Update failed.'.
  ENDIF.

  COMMIT WORK AND WAIT.
ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  LOCKING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM LOCKING_RTN .
*  PERFORM CHECK_LOCK_OBJECT.
*  PERFORM CHECK_ENQUEUE_READ.
  PERFORM CHECK_BATCHJOB.
ENDFORM.                    " LOCKING_RTN
*&---------------------------------------------------------------------*
*&      Form  CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_LOCK_OBJECT .
  CALL FUNCTION 'ENQUEUE_EZ_ZHINCAS0010'
       EXPORTING
            MODE_ZHINCAS0010 = 'E'
            MANDT            = SY-MANDT
            PROGNAME         = 'ZHINMMO90400'
       EXCEPTIONS
            FOREIGN_LOCK     = 1
            SYSTEM_FAILURE   = 2
            OTHERS           = 3.
  IF SY-SUBRC <> 0.
    IF SY-BATCH EQ 'X'.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      LEAVE PROGRAM.
    ELSE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_ENQUEUE_READ .
  DATA: L_GARG        TYPE SEQG3-GARG,
        L_LOCK_NUMBER LIKE SY-TABIX.

  DATA: LT_LOCK TYPE TABLE OF SEQG3 WITH HEADER LINE.

  MOVE: SY-MANDT       TO L_GARG(3),
        'ZHINMMO90400' TO L_GARG+3.

  CALL FUNCTION 'ENQUEUE_READ'
       EXPORTING
            GCLIENT               = SY-MANDT
            GNAME                 = 'ZHINCAS0010'
            GARG                  = L_GARG
            GUNAME                = ' '
            LOCAL                 = ' '
            FAST                  = ' '
       IMPORTING
            NUMBER                = L_LOCK_NUMBER
       TABLES
            ENQ                   = LT_LOCK
       EXCEPTIONS
            COMMUNICATION_FAILURE = 1
            SYSTEM_FAILURE        = 2
            OTHERS                = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF L_LOCK_NUMBER > 1.
    IF SY-BATCH EQ 'X'.
      MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      LEAVE PROGRAM.
    ELSE.
      MESSAGE ID 'KX' TYPE 'E' NUMBER '972' WITH SY-REPID.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*&      Form  CHECK_BATCHJOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BATCHJOB .
  DATA: LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
       EXPORTING
            ABAP_PROGRAM_NAME             = SY-REPID
            DIALOG                        = 'N'
            STATUS                        = 'R'
       TABLES
            JOBLIST                       = LT_JOBLIST
       EXCEPTIONS
            NO_JOBS_FOUND                 = 1
            PROGRAM_SPECIFICATION_MISSING = 2
            INVALID_DIALOG_TYPE           = 3
            JOB_FIND_CANCELED             = 4
            OTHERS                        = 5.

  IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      MESSAGE S006 WITH SY-REPID LT_JOBLIST-SDLUNAME.
      LEAVE PROGRAM.
    ENDIF.
  ELSE.
    READ TABLE LT_JOBLIST INDEX 1.
    IF SY-SUBRC EQ 0.
      MESSAGE E006 WITH SY-REPID LT_JOBLIST-SDLUNAME.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_BATCHJOB
*&---------------------------------------------------------------------*
*&      Form  READ_CHANGE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CHANGE_DATE USING P_EBELN P_EBELP P_AENAM P_AEDAT P_AEZET.
*  DATA: LT_AUSG LIKE CDRED_MMPUR OCCURS 0 WITH HEADER LINE.
*
*  CALL FUNCTION 'ME_CHANGEDOC_READ2'
*    EXPORTING
*      I_DOCUMENT_CATEGORY = 'F'     "PO
*      I_DOCUMENT_NUMBER   = P_EBELN
*      I_DOCUMENT_ITEM     = P_EBELP
*    TABLES
*      T_AUSG              = LT_AUSG.
*
*
*  DELETE LT_AUSG WHERE NOT ( TABNAME = 'EKPO'  AND
*                             FNAME   = 'LOEKZ' AND
*                             F_NEW   = 'L' ).
*
*  SORT LT_AUSG BY UDATE DESCENDING UTIME DESCENDING.
*
*  READ TABLE LT_AUSG INDEX 1.
*
*  MOVE: LT_AUSG-USERNAME TO P_AENAM,
*        LT_AUSG-UDATE    TO P_AEDAT,
*        LT_AUSG-UTIME    TO P_AEZET.

ENDFORM.                    " READ_CHANGE_DATE
*&---------------------------------------------------------------------*
*&      Form  HOTSPOT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM HOTSPOT_LOG USING P_ROW_ID P_COLUMN_ID P_ROW_NO.

  CHECK NOT P_ROW_ID IS INITIAL.

  READ TABLE IT_LOG INDEX P_ROW_ID.
  IF SY-SUBRC NE 0.
    EXIT.
  ENDIF.

  CASE P_COLUMN_ID.
    WHEN 'EBELN'.
      SET PARAMETER ID 'BES'  FIELD IT_LOG-EBELN.

      CALL TRANSACTION 'ME23N'.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    " HOTSPOT_LOG
*&---------------------------------------------------------------------*
*&      Form  MAKE_IF_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_IF_LOG .
  REFRESH: IT_LOG.

  SORT IT_PO BY EBELN EBELP.
  LOOP AT IT_PO.
    CLEAR: IT_LOG.

*    PERFORM read_change_date USING it_po-ebeln it_po-ebelp
*                                   it_po-aenam it_po-aedat it_po-aezet.

    MOVE: 'HMMA'          TO IT_LOG-ZCCORPCD,
          '300'          TO IT_LOG-ZCSVCGN,
          IT_PO-WERKS    TO IT_LOG-ZCPLTCD,
          IT_PO-EBELN    TO IT_LOG-EBELN,
          IT_PO-EBELP    TO IT_LOG-EBELP,
          IT_PO-MATNR    TO IT_LOG-ZNPATNR,
          IT_PO-REQNAM   TO IT_LOG-ZCMODER,
          IT_PO-LGORT    TO IT_LOG-LGORT,
          IT_PO-LIFNR    TO IT_LOG-LIFNR,
          IT_PO-NAME1    TO IT_LOG-NAME1,
          IT_PO-TXZ01    TO IT_LOG-TXZ01,
          IT_PO-MENGE    TO IT_LOG-MENGE,
          IT_PO-MEINS    TO IT_LOG-MEINS,
          IT_PO-WAERS    TO IT_LOG-WAERS,
          IT_PO-NETPR    TO IT_LOG-NETPR,
          IT_PO-PEINH    TO IT_LOG-PEINH,
          IT_PO-NETWR    TO IT_LOG-NETWR,
          IT_PO-ZREASON  TO IT_LOG-ZREASON,
          IT_PO-REQNAM   TO IT_LOG-REQNAM,
          IT_PO-REQDAT   TO IT_LOG-REQDAT,
          '600'    TO IT_LOG-HOUSE_CODE,
 '1'    TO IT_LOG-PO_COUNT,
  'T'    TO IT_LOG-PROGRESS,
  'C'    TO IT_LOG-STATUS,
  SY-DATUM    TO IT_LOG-ADD_DATE.

    APPEND IT_LOG.
  ENDLOOP.
ENDFORM.                    " MAKE_IF_LOG
