************************************************************************
* Program Name      : ZMMR_MATERIAL_CHANGES
* Creation Date     : 08/27/2010
* Developer         : Furong Wang
* Development Request No :
* Addl Documentation:
* Description       :
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZMMR_MATERIAL_CHANGES NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMMM.

TABLES: MARA, CDHDR, CDPOS.

TYPE-POOLS SLIS .
DATA : BEGIN OF IT_DATA OCCURS 0,
       MATNR LIKE MARA-MATNR,
       MTART LIKE MARA-MTART,
       UDATE LIKE CDHDR-UDATE,
*       LAEDA LIKE MARA-LAEDA,
       MAKTX LIKE MAKT-MAKTX,
  	LVORM LIKE MARA-LVORM,
       MSTAE LIKE MARA-MSTAE,
       VALUE_NEW(40),
       VALUE_OLD(40),
       CHANGENR LIKE CDHDR-CHANGENR,
       END OF IT_DATA.

DATA: IT_MAIL TYPE STANDARD TABLE OF SOLISTI1 INITIAL SIZE 0
                  WITH HEADER LINE.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE.


DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
       W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE
      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS : S_MTART FOR MARA-MTART,
                 S_MATNR FOR MARA-MATNR,
                 S_NEW FOR CDPOS-VALUE_NEW,
                 S_OLD FOR CDPOS-VALUE_OLD,
                 S_LAEDA FOR MARA-LAEDA.

PARAMETERS: P_OBJCLS LIKE CDHDR-OBJECTCLAS,
P_TABNAM  LIKE CDPOS-TABNAME,
P_FNAME  LIKE CDPOS-FNAME,
P_IND  LIKE CDPOS-CHNGIND.
*P_LVORM LIKE MARA-LVORM.
SELECTION-SCREEN END OF BLOCK BLOCK1.

SELECTION-SCREEN BEGIN OF BLOCK BLOCK2 WITH FRAME TITLE TEXT-002.
PARAMETERS: P_SEND AS CHECKBOX. " DEFAULT 'X' USER-COMMAND UCOM.
PARAMETERS: P_EMAIL(40) DEFAULT 'SAIFVAL' MODIF ID MD3.
SELECTION-SCREEN END OF BLOCK BLOCK2.

INITIALIZATION.
  PERFORM INIT_DATA.

*AT SELECTION-SCREEN OUTPUT.
*
*AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM GET_DATA.
  IF IT_DATA[] IS INITIAL.
    MESSAGE S999 WITH TEXT-M01.
  ELSE.
    IF P_SEND = 'X'.
      PERFORM SEND_EMAIL.
    ENDIF.
    PERFORM DISPLAY_DATA.
  ENDIF.
*---------------------------------------------------------------------*
*       FORM get_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM GET_DATA.
  DATA: LT_TEMP LIKE TABLE OF IT_DATA WITH HEADER LINE.
*  DATA: BEGIN OF LT_temp OCCURS 0,
*        MATNR LIKE EINA-MATNR,
*        CHANGENR like cdhdr,
*
*        END OF LT_MATNR.
  DATA: L_COUNT TYPE I.

  SELECT B~MATNR MTART UDATE MAKTX LVORM MSTAE CHANGENR
     INTO CORRESPONDING FIELDS OF TABLE LT_TEMP
     FROM CDHDR AS A
     INNER JOIN MARA AS B
     ON A~OBJECTID = B~MATNR
     INNER JOIN MAKT AS C
     ON B~MATNR = C~MATNR
   WHERE OBJECTCLAS = P_OBJCLS
     AND MTART IN S_MTART
     AND B~MATNR IN S_MATNR
     AND UDATE IN S_LAEDA
*     AND LAEDA IN S_LAEDA
     AND LVORM = ' '.

*  DELETE ADJACENT DUPLICATES FROM LT_TEMP.

  IF SY-SUBRC = 0.
    LOOP AT LT_TEMP.
      MOVE-CORRESPONDING LT_TEMP TO IT_DATA.
      SELECT SINGLE VALUE_NEW VALUE_OLD INTO
       (IT_DATA-VALUE_NEW, IT_DATA-VALUE_OLD)
      FROM CDPOS
      WHERE OBJECTCLAS = P_OBJCLS
        AND OBJECTID = LT_TEMP-MATNR
        AND CHANGENR = LT_TEMP-CHANGENR
        AND TABNAME = P_TABNAM
        AND FNAME = P_FNAME
        AND CHNGIND = P_IND
        AND VALUE_NEW IN S_NEW
        AND VALUE_OLD IN S_OLD.
      IF SY-SUBRC = 0.
        APPEND IT_DATA.
      ENDIF.
      CLEAR: IT_DATA.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  check_input_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_VALUE.

ENDFORM.                    " check_input_value
*&---------------------------------------------------------------------*
*&      Form  check_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_SCREEN.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'P_EXCEL'.
      SCREEN-INPUT = 0.
      SCREEN-INVISIBLE = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " check_screen

*---------------------------------------------------------------------*
*       FORM display_data                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DISPLAY_DATA.
  CALL SCREEN 200.
ENDFORM.                    " display_data
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
  CASE OK_CODE.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
*    WHEN 'INBDEL'.
*      PERFORM DISPLAY_INBOUND_DELIVERY.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*---------------------------------------------------------------------*
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
      IS_LAYOUT            = WA_IS_LAYOUT
      I_SAVE               = WA_SAVE
      IS_VARIANT           = WA_VARIANT
      I_DEFAULT            = SPACE
*     it_toolbar_excluding = it_toolbar_excluding[]
    CHANGING
      IT_FIELDCATALOG      = IT_FIELDCAT[]
      IT_OUTTAB            = IT_DATA[]
      IT_SORT              = IT_SORT[].

ENDFORM.                    " assign_itab_to_alv
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
    EXPORTING
      CONTAINER_NAME              = WA_CUSTOM_CONTROL
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
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
    EXPORTING
      I_PARENT      = GRID_CONTAINER
      I_APPL_EVENTS = 'X'.
ENDFORM.                    " create_container_n_object

*---------------------------------------------------------------------*
*       FORM set_attributes_alv_grid                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

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

                                  'S' 'MATNR'       ' ',
*                                 ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

                                  'S' 'MTART'       ' ',
                                  ' ' 'COLTEXT'     'Type',
                                  'E' 'OUTPUTLEN'   '4',

                                  'S' 'UDATE'       ' ',
                                  ' ' 'COLTEXT'     'Ch. Date',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'MAKTX'       ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'MSTAE'        ' ',
                                  ' ' 'COLTEXT'     'X-Plant',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'VALUE_NEW'       ' ',
                                  ' ' 'COLTEXT'     'New Value',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'VALUE_OLD'       ' ',
                                  ' ' 'COLTEXT'     'Old Value',
                                  'E' 'OUTPUTLEN'   '20',


                                  'S' 'LVORM'       ' ',
                                  ' ' 'COLTEXT'     'Del Ind.',
                                  'E' 'OUTPUTLEN'   '8'.


ENDFORM.
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
*&---------------------------------------------------------------------*
*&      Form  INIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  S_MTART-OPTION = 'EQ'.
  S_MTART-SIGN = 'I'.
  S_MTART-LOW = 'ROH'.
  APPEND S_MTART.

  S_LAEDA-OPTION = 'EQ'.
  S_LAEDA-SIGN = 'I'.
  S_LAEDA-LOW = SY-DATUM - 1.
  APPEND S_LAEDA.

  P_OBJCLS = 'MATERIAL'.
  P_TABNAM = 'MARA'.
  P_FNAME = 'MSTAE'.
  P_IND = 'U'.

ENDFORM.                    " INIT_DATA
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEND_EMAIL.
  DATA: L_SUBJECT(40) TYPE C VALUE 'Changes of Material Master (MSTAE)'.

  DATA:   IT_PACKING_LIST LIKE SOPCKLSTI1 OCCURS 0 WITH HEADER LINE,
          IT_CONTENTS LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          IT_RECEIVERS LIKE SOMLRECI1 OCCURS 0 WITH HEADER LINE,
          IT_ATTACHMENT LIKE SOLISTI1 OCCURS 0 WITH HEADER LINE,
          GD_CNT TYPE I,
          GD_SENT_ALL(1) TYPE C,
          GD_DOC_DATA LIKE SODOCCHGI1,
          GD_ERROR TYPE SY-SUBRC.

  PERFORM POPULATE_DATA_FOR_OUTPUT.

  GD_DOC_DATA-DOC_SIZE = 1.

* Populate the subject/generic message attributes
  GD_DOC_DATA-OBJ_LANGU = SY-LANGU.
  GD_DOC_DATA-OBJ_NAME  = SY-REPID.
  GD_DOC_DATA-OBJ_DESCR = L_SUBJECT.
  GD_DOC_DATA-SENSITIVTY = 'F'.

* Describe the body of the message
  CLEAR IT_PACKING_LIST.
  REFRESH IT_PACKING_LIST.
  IT_PACKING_LIST-TRANSF_BIN = SPACE.
  IT_PACKING_LIST-HEAD_START = 1.
  IT_PACKING_LIST-HEAD_NUM = 0.
  IT_PACKING_LIST-BODY_START = 1.
  DESCRIBE TABLE IT_MAIL LINES IT_PACKING_LIST-BODY_NUM.
  IT_PACKING_LIST-DOC_TYPE = 'RAW'.
  APPEND IT_PACKING_LIST.

* Add the recipients email address
  CLEAR IT_RECEIVERS.
  REFRESH IT_RECEIVERS.
  IT_RECEIVERS-RECEIVER = P_EMAIL.
  IT_RECEIVERS-REC_TYPE = 'U'.  " internet email
*  IT_RECEIVERS-REC_TYPE = 'C'.
  IT_RECEIVERS-COM_TYPE = 'INT'.
  IT_RECEIVERS-NOTIF_DEL = 'X'.
  IT_RECEIVERS-NOTIF_NDEL = 'X'.
  APPEND IT_RECEIVERS.

* Call the FM to post the message to SAPMAIL
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = GD_DOC_DATA
*            PUT_IN_OUTBOX              = 'X'
            COMMIT_WORK                = 'X'
    IMPORTING
      SENT_TO_ALL                = GD_SENT_ALL
    TABLES
      PACKING_LIST               = IT_PACKING_LIST
      CONTENTS_TXT               = IT_MAIL
      RECEIVERS                  = IT_RECEIVERS
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      DOCUMENT_TYPE_NOT_EXIST    = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR            = 5
      X_ERROR                    = 6
      ENQUEUE_ERROR              = 7
      OTHERS                     = 8.

* Store function module return code

  IF sy-subrc = 0.
    SUBMIT rsconn01           "Start the sending process
          WITH mode   = 'INT'
          WITH output = ' '
          AND RETURN.

  ENDIF.

  GD_ERROR = SY-SUBRC.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  POPULATE_DATA_FOR_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPULATE_DATA_FOR_OUTPUT.
*   DATA: L_MESSAGE TYPE SO_TEXT255,

  CLEAR: IT_MAIL,IT_MAIL[].

  MOVE: 'Material' TO IT_MAIL+0(18),
         'Type' TO IT_MAIL+18(5),
         'Ch Date' TO IT_MAIL+23(10),
         'Desciption' TO IT_MAIL+33(40),
         'X-Plant' TO IT_MAIL+73(8),
         'New Value' TO IT_MAIL+81(40),
         'Old Value' TO IT_MAIL+121(40),
         'Del' TO IT_MAIL+161(3).
  APPEND IT_MAIL.

  LOOP AT IT_DATA.
    MOVE: IT_DATA-MATNR TO IT_MAIL+0(18),
          IT_DATA-MTART TO IT_MAIL+18(5),
          IT_DATA-UDATE TO IT_MAIL+23(10),
          IT_DATA-MAKTX TO IT_MAIL+33(40),
          IT_DATA-MSTAE TO IT_MAIL+73(8),
          IT_DATA-VALUE_NEW TO IT_MAIL+81(40),
          IT_DATA-VALUE_OLD TO IT_MAIL+121(40),
          IT_DATA-LVORM  TO IT_MAIL+161(3).
    APPEND IT_MAIL.

    CLEAR: IT_MAIL.
  ENDLOOP.

ENDFORM.                    " POPULATE_DATA_FOR_OUTPUT
