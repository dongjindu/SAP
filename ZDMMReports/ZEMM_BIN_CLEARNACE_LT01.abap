************************************************************************
* Program Name      : ZEMM_BIN_CLEARNACE_LT01
* Author            : Furong Wang
* Creation Date     : 03/2010
* Specifications By :
* Pattern           :
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************

REPORT ZEMM_BIN_CLEARNACE_LT01 NO STANDARD PAGE HEADING
                          LINE-SIZE 132
                          LINE-COUNT 64(1)
                          MESSAGE-ID ZMMM.

TYPE-POOLS: SLIS .
TYPES TRUXS_T_TEXT_DATA(4096) TYPE C OCCURS 0.
DATA: IT_RAW TYPE TRUXS_T_TEXT_DATA.

DATA: BEGIN OF IT_DATA OCCURS 1000,
    LGNUM(3),
    BWLVS(3),
    MATNR(18),
    ANFME(10),
    WERKS(4),
    LGORT(4),
    VLTYP(5),
    VLPLA(10),
    NLTYP(5),
    NLPLA(10),
    FLAG(1),
    MSG(255),

*    LGNUM LIKE LTAK-LGNUM,
*    BWLVS LIKE LTAK-BWLVS,
*    MATNR LIKE LTAP-MATNR,
*    ANFME LIKE RL03T-ANFME,
*    WERKS LIKE LTAP-WERKS,
*    LGORT LIKE LTAP-LGORT,
*    VLTYP LIKE LTAP-VLTYP,
*    VLPLA LIKE LTAP-VLPLA,
*    NLTYP LIKE LTAP-NLTYP,
*    NLPLA LIKE LTAP-NLPLA,
*    FLAG TYPE ZRESULT,
*    MSG TYPE ZMSG,
    END OF IT_DATA.

*** BDC

DATA: W_MODE(1) VALUE 'N',
      W_UPDATE(1) VALUE 'L',
      MESSTAB LIKE TABLE OF BDCMSGCOLL WITH HEADER LINE,
      BDCDATA LIKE BDCDATA    OCCURS 0 WITH HEADER LINE.


** ALV
DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT,      "for parameter IS_VARIANT
       IT_EXCLUDE TYPE UI_FUNCTIONS.

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_CODE LIKE SY-UCOMM,
      W_OLD_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

PARAMETERS: P_FILE  LIKE RLGRAP-FILENAME OBLIGATORY
                    DEFAULT 'c:\temp\bin_clearance.xls'
                    MODIF ID EXL.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM BROWSER CHANGING P_FILE.

START-OF-SELECTION.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
       EXPORTING
            I_LINE_HEADER        = SPACE
            I_TAB_RAW_DATA       = IT_RAW
            I_FILENAME           = P_FILE
       TABLES
            I_TAB_CONVERTED_DATA = IT_DATA[]
       EXCEPTIONS
            CONVERSION_FAILED    = 1
            OTHERS               = 2.

  LOOP AT IT_DATA.
    REFRESH: MESSTAB, BDCDATA.

    PERFORM BDC_DYNPRO      USING 'SAPML03T' '0101'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'LTAP-LGORT'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'LTAK-LGNUM'
                                  IT_DATA-LGNUM.
    PERFORM BDC_FIELD       USING 'LTAK-BWLVS'
                                  IT_DATA-BWLVS.
    PERFORM BDC_FIELD       USING 'LTAP-MATNR'
                                  IT_DATA-MATNR.
    PERFORM BDC_FIELD       USING 'RL03T-ANFME'
                                  IT_DATA-ANFME.
    PERFORM BDC_FIELD       USING 'LTAP-WERKS'
                                  IT_DATA-WERKS.
    PERFORM BDC_FIELD       USING 'LTAP-LGORT'
                                  IT_DATA-LGORT.

    PERFORM BDC_DYNPRO      USING 'SAPML03T' '0102'.

    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'LTAP-NLPLA'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
*perform bdc_field       using 'RL03T-ANFME'
*                              record-ANFME_007.
*perform bdc_field       using 'LTAP-ALTME'
*                              record-ALTME_008.
*perform bdc_field       using 'RL03T-SQUIT'
*                              record-SQUIT_009.
    PERFORM BDC_FIELD       USING 'LTAP-VLTYP'
                                  IT_DATA-VLTYP.
    PERFORM BDC_FIELD       USING 'LTAP-VLPLA'
                                  IT_DATA-VLPLA.
    PERFORM BDC_FIELD       USING 'LTAP-NLTYP'
                                  IT_DATA-NLTYP.
    PERFORM BDC_FIELD       USING 'LTAP-NLPLA'
                                  IT_DATA-NLPLA.

    CALL TRANSACTION 'LT01' USING BDCDATA
                            MODE W_MODE
                            UPDATE W_UPDATE
                            MESSAGES INTO MESSTAB.

    READ TABLE MESSTAB WITH KEY MSGTYP = 'S'.

    IF SY-SUBRC = 0.
      IT_DATA-FLAG = 'S'.
      IT_DATA-MSG = MESSTAB-MSGV1.
    ELSE.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
           EXPORTING
                MSGID               = SY-MSGID
                MSGNR               = SY-MSGNO
                MSGV1               = SY-MSGV1
                MSGV2               = SY-MSGV2
                MSGV3               = SY-MSGV3
                MSGV4               = SY-MSGV4
           IMPORTING
                MESSAGE_TEXT_OUTPUT = IT_DATA-MSG.

*      READ TABLE MESSTAB WITH KEY MSGTYP = 'E'.
      IT_DATA-FLAG = 'E'.
    ENDIF.

    MODIFY IT_DATA.
    CLEAR: IT_DATA.
  ENDLOOP.

  CALL SCREEN 200.

*  END-OF-SELECTION.

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*

FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR BDCDATA.
  BDCDATA-PROGRAM  = PROGRAM.
  BDCDATA-DYNPRO   = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'ST200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  display_alv  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_DATA'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.
ENDMODULE.                 " display_alv  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  W_CODE = OK_CODE.
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = IT_EXCLUDE[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_DATA[]
               IT_SORT          = IT_SORT[].

** ENTER
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.
*
** Cursor----
*  CALL METHOD ALV_GRID->REGISTER_EDIT_EVENT
*                EXPORTING
*                   I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_MODIFIED.
*
*  CREATE OBJECT G_EVENT_RECEIVER.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_DATA_CHANGED FOR ALV_GRID.
*  SET HANDLER G_EVENT_RECEIVER->HANDLE_LEFT_CLICK_RUN FOR ALV_GRID.
*
*  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.
*
ENDFORM.                    " assign_itab1_to_alv
*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
*  WA_IS_LAYOUT-EDIT       = 'X'.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
*  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*  WA_IS_LAYOUT-BOX_FNAME = 'SEL'.
*  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.
ENDFORM.                    " set_attributes_alv_grid

*---------------------------------------------------------------------*
*       FORM build_sortcat_display                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'MATNR'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display

*---------------------------------------------------------------------*
*       FORM build_field_catalog                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_ITAB                                                        *
*---------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME.
*        lw_waers LIKE t001-waers,

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_REPID.

  LW_ITAB = P_ITAB.

  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = LW_ITAB
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = IT_FIELDNAME.

  PERFORM SETTING_FIELDCAT TABLES IT_FIELDCAT USING :


                                  'S' 'LGNUM'    ' ',
*                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'WH No.',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'BWLVS'    ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Mv. Type',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'MATNR'    ' ',
                                  ' ' 'COLTEXT'     'Material No',
                                  'E' 'OUTPUTLEN'   '18',


                                  'S' 'ANFME'     ' ',
                                  ' ' 'COLTEXT'     'Quantity',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'WERKS'     ' ',
                                  ' ' 'COLTEXT'     'Plant',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'LGORT'       ' ',
                                  ' ' 'COLTEXT'     'S Loca',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'VLTYP'       ' ',
                                  ' ' 'COLTEXT'     'SS Type',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'VLPLA'       ' ',
                                  ' ' 'COLTEXT'     'SS Bin',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'NLTYP'       ' ',
                                  ' ' 'COLTEXT'     'D Type',
                                  'E' 'OUTPUTLEN'   '12',

                                 'S' 'NLPLA'       ' ',
                                  ' ' 'COLTEXT'     'D Bin',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'FLAG'       ' ',
                                  ' ' 'COLTEXT'     'Result',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'MSG'       ' ',
                                  ' ' 'COLTEXT'     'Message',
                                  'E' 'OUTPUTLEN'   '255'.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  CLEAR: W_REPID.
  CREATE OBJECT GRID_CONTAINER
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL
          EXCEPTIONS
           CNTL_ERROR = 1
           CNTL_SYSTEM_ERROR = 2
           CREATE_ERROR = 3
           LIFETIME_ERROR = 4
           LIFETIME_DYNPRO_DYNPRO_LINK = 5.
  W_REPID = SY-REPID.
  IF SY-SUBRC NE 0.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'The control can not be created'.
  ENDIF.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  setting_fieldcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0584   text
*      -->P_0585   text
*      -->P_0586   text
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
      MESSAGE E000(ZZ) WITH 'Check field catalog'.
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
    ADD 1 TO W_CNT.
    P_FIELDCAT-COL_POS = W_CNT.
    APPEND P_FIELDCAT.
  ENDIF.
ENDFORM.                    " setting_fieldcat


*---------------------------------------------------------------------*
*       FORM browser                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FILENAME                                                      *
*---------------------------------------------------------------------*
FORM BROWSER CHANGING FILENAME.
  DATA: IT_TFILE TYPE FILETABLE ,
        GD_SUBRC TYPE I.

  CALL  METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
        EXPORTING
          WINDOW_TITLE = 'Select File Name'
          DEFAULT_EXTENSION = '*.*'
          DEFAULT_FILENAME = '*.*'
          FILE_FILTER = '*.*'
          INITIAL_DIRECTORY = 'c:\temp\'
*         MULTISELECTION =
*         WITH_ENCODING =
        CHANGING
          FILE_TABLE = IT_TFILE
          RC = GD_SUBRC.
*         USER_ACTION =
*         FILE_ENCODING =
*         EXCEPTIONS
*         FILE_OPEN_DIALOG_FAILED = 1
*         CNTL_ERROR = 2
*         ERROR_NO_GUI = 3
*         NOT_SUPPORTED_BY_GUI = 4
*         others = 5
  .
  IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE IT_TFILE INTO FILENAME INDEX 1.
  ENDIF.

ENDFORM.                    " BROWSER
