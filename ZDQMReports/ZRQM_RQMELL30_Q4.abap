************************************************************************
* Program Name      : ZRQM_RQMELL30_Q4
* Creation Date     : 03/02/2010
* Development Request No :
* Addl Documentation:
* Description       : Q4 Notification Report
*
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************
REPORT ZRQM_RQMELL30 NO STANDARD PAGE HEADING
                     MESSAGE-ID ZMQM.

TYPE-POOLS SLIS.
TABLES: QMFE.
DATA G_SUBMIT.
DATA: G_FIELDCAT_TAB TYPE SLIS_T_FIELDCAT_ALV.

DATA H_FIELDCAT_WA TYPE SLIS_FIELDCAT_ALV.

DATA: W_REPID LIKE SY-REPID.

DATA: BEGIN OF OBJECT_TAB OCCURS 0.
        INCLUDE STRUCTURE RQMQMFE.
DATA:   OBJNR LIKE RQMQMEL-OBJNR,
        SELECTED,
        PM_SELECTED TYPE PM_SELECTED,
        LIGHTS,
      END OF OBJECT_TAB.

DATA: BEGIN OF IT_OUTPUT OCCURS 0.
        INCLUDE STRUCTURE RQMQMFE.
DATA:   OBJNR LIKE RQMQMEL-OBJNR,
        SELECTED,
        PM_SELECTED TYPE PM_SELECTED,
        LIGHTS,
        CRDATE(10),
        CHDATE(10),
        SCAN(10),
        STPRS(11), " LIKE MBEW-STPRS,
        BZMNG(12), "LIKE QMEL-BZMNG,
        AMOUNT(13), "LIKE MBEW-SALK3,
        USERNAME(30),
      END OF IT_OUTPUT.

DATA : IT_FIELDCAT     TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_FI  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDCAT_CO  TYPE LVC_T_FCAT WITH HEADER LINE,
       IT_FIELDNAME    TYPE SLIS_T_FIELDCAT_ALV,
       IT_SORT         TYPE LVC_T_SORT WITH HEADER LINE,
       IT_FIELDCAT_DET TYPE LVC_T_FCAT WITH HEADER LINE. "/Detail

DATA : WA_IS_LAYOUT TYPE LVC_S_LAYO, "/The Layout Structure
      W_FIELDNAME    LIKE LINE OF IT_FIELDCAT.

DATA: WA_SAVE    TYPE C   VALUE 'A',   "for Parameter I_SAVE

      WA_VARIANT TYPE DISVARIANT.      "for parameter IS_VARIANT

DATA: WA_CUSTOM_CONTROL TYPE        SCRFNAME VALUE 'ALV_CONTAINER',
      ALV_GRID          TYPE REF TO CL_GUI_ALV_GRID,
      GRID_CONTAINER    TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

DATA: OK_CODE      LIKE SY-UCOMM,
      W_CNT       TYPE   I,
      W_NO_DATA(1),
      W_INDEX LIKE SY-TABIX,
      W_STPRS LIKE MBEW-STPRS,
      W_BZMNG LIKE  QMEL-BZMNG,
      W_AMOUNT LIKE MBEW-SALK3,
      W_OBJNR LIKE QMEL-OBJNR.

DATA: W_PARNR LIKE IHPA-PARNR.

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS DY_OFN LIKE RQMLA-STAT_OFN.  "FOR TABLE RQMQMEL.
SELECTION-SCREEN COMMENT 3(10) TEXT-001. " FOR TABLE RQMQMEL.
PARAMETERS DY_RST LIKE RQMLA-STAT_RST.
SELECTION-SCREEN COMMENT 16(10) TEXT-002.
PARAMETERS DY_IAR LIKE RQMLA-STAT_IAR DEFAULT 'X'.
SELECTION-SCREEN COMMENT 29(10) TEXT-003.
PARAMETERS DY_MAB LIKE RQMLA-STAT_MAB.
SELECTION-SCREEN COMMENT 42(10) TEXT-004.
SELECTION-SCREEN COMMENT 60(10) TEXT-005.
PARAMETERS: SELSCHEM LIKE TJ48T-SELID.  " FOR TABLE RQMQMEL.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECT-OPTIONS:  QMDAT FOR SY-DATUM NO-EXTENSION,
                 QMNUM FOR OBJECT_TAB-QMNUM,
                 QMART FOR OBJECT_TAB-QMART.
** Changed by Furong on 03/05/09
SELECT-OPTIONS:  S_FEGRP FOR QMFE-FEGRP,
                 S_FECOD FOR QMFE-FECOD.

PARAMETERS :  PARNR_IN LIKE LFA1-LIFNR MODIF ID Q4.  "IHPA-PARNR.

** End of change
SELECTION-SCREEN SKIP.
PARAMETERS : VARIANT LIKE DISVARIANT-VARIANT DEFAULT '/SCRAP_EMAIL'.

SELECTION-SCREEN END OF BLOCK B1.

INITIALIZATION.
  PERFORM INIT_DATA.

AT SELECTION-SCREEN OUTPUT.
*  CLEAR: S_FEGRP, S_FEGRP[], S_FECOD, S_FECOD[].
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'S_FECOD-LOW' OR
       SCREEN-NAME = 'S_FECOD-HIGH' OR
       SCREEN-NAME = 'QMART-LOW' OR
       SCREEN-NAME = 'QMART-HIGH'.

*       SCREEN-ACTIVE = 1.
      SCREEN-INPUT = 0.
      SCREEN-OUTPUT = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON S_FECOD.
  IF S_FEGRP[] IS INITIAL.
    REFRESH S_FECOD.
  ELSE.
    IF S_FEGRP-LOW IS INITIAL.
      LOOP AT S_FECOD.
        CLEAR: S_FECOD-LOW.
        MODIFY S_FECOD.
      ENDLOOP.
    ENDIF.
    IF S_FEGRP-HIGH IS INITIAL.
      LOOP AT S_FECOD.
        CLEAR: S_FECOD-HIGH.
        MODIFY S_FECOD.
      ENDLOOP.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FEGRP-LOW.
  PERFORM DISPLAY_SEARCH_HELP USING 'W' 'S_FEGRP-LOW' 'S_FECOD-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_FEGRP-HIGH.
  PERFORM DISPLAY_SEARCH_HELP USING 'W' 'S_FEGRP-HIGH' 'S_FECOD-HIGH'.

START-OF-SELECTION.
  W_REPID = SY-REPID.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
       EXPORTING
            I_PROGRAM_NAME     = W_REPID
            I_INTERNAL_TABNAME = 'OBJECT_TAB'
            I_INCLNAME         = W_REPID
       CHANGING
            CT_FIELDCAT        = G_FIELDCAT_TAB.

  READ TABLE G_FIELDCAT_TAB INTO H_FIELDCAT_WA
        WITH KEY FIELDNAME = 'MATKTX'.
  W_INDEX = SY-TABIX.
  H_FIELDCAT_WA-NO_OUT = SPACE.
  MODIFY G_FIELDCAT_TAB FROM H_FIELDCAT_WA INDEX W_INDEX.

  READ TABLE G_FIELDCAT_TAB INTO H_FIELDCAT_WA
        WITH KEY FIELDNAME = 'HERSTELLER'.
  W_INDEX = SY-TABIX.
  H_FIELDCAT_WA-NO_OUT = SPACE.
  MODIFY G_FIELDCAT_TAB FROM H_FIELDCAT_WA INDEX W_INDEX.

  READ TABLE G_FIELDCAT_TAB INTO H_FIELDCAT_WA
         WITH KEY FIELDNAME = 'AUSVN'.
  W_INDEX = SY-TABIX.
  H_FIELDCAT_WA-NO_OUT = 'X'.
  MODIFY G_FIELDCAT_TAB FROM H_FIELDCAT_WA INDEX W_INDEX.

  G_SUBMIT = 'X'.

  EXPORT G_SUBMIT G_FIELDCAT_TAB TO MEMORY ID 'SUBMFE'.

  SUBMIT  RQMELL30
                 WITH DY_OFN = DY_OFN
                 WITH DY_RST = DY_RST
                 WITH DY_IAR  = DY_IAR
                 WITH DY_MAB = DY_MAB
                 WITH SELSCHEM = SELSCHEM
                 WITH  QMDAT IN QMDAT
                 WITH QMNUM IN QMNUM
                 WITH QMART IN QMART
                 WITH PARVW_IN = 'Z5'
                 WITH PARNR_IN = PARNR_IN
                 WITH FEGRP IN S_FEGRP
                 WITH FECOD IN S_FECOD
                 WITH VARIANT = VARIANT
                 EXPORTING LIST TO MEMORY
                 AND RETURN.

  IMPORT OBJECT_TAB FROM MEMORY ID 'SUBMFE'.

  LOOP AT OBJECT_TAB.
    MOVE-CORRESPONDING OBJECT_TAB TO IT_OUTPUT.
    SELECT SINGLE STPRS INTO W_STPRS
      FROM MBEW
      WHERE MATNR = IT_OUTPUT-MATNR
        AND BWKEY = IT_OUTPUT-MAWERK.
    SELECT SINGLE BZMNG INTO W_BZMNG
      FROM QMEL
      WHERE QMNUM = IT_OUTPUT-QMNUM.
    IT_OUTPUT-STPRS = W_STPRS.
    IT_OUTPUT-BZMNG  = W_BZMNG.
    W_AMOUNT = W_BZMNG * W_STPRS.
    IT_OUTPUT-AMOUNT = W_AMOUNT.
*    SELECT SINGLE HERSTELLER INTO IT_OUTPUT-HERSTELLER
*      FROM QMEL
*      WHERE QMNUM = IT_OUTPUT-QMNUM.
    IT_OUTPUT-SCAN = IT_OUTPUT-ANZFEHLER.
    IT_OUTPUT-CRDATE = IT_OUTPUT-ERDAT.
    IT_OUTPUT-CHDATE = IT_OUTPUT-AEDAT.
*      IT_OUTPUT-AMOUNT = IT_OUTPUT-BZMNG * IT_OUTPUT-STPRS.
** Changed by Furong on 09/15/08
    IF IT_OUTPUT-HERSTELLER IS INITIAL.
      SELECT SINGLE OBJNR INTO W_OBJNR
        FROM QMEL
        WHERE QMNUM = IT_OUTPUT-QMNUM.

      SELECT SINGLE PARNR INTO W_PARNR
        FROM IHPA
        WHERE OBJNR = W_OBJNR
          AND PARVW = 'Z1'.
      IF SY-SUBRC = 0.
        SELECT SINGLE NAME_TEXTC INTO IT_OUTPUT-USERNAME
        FROM USER_ADDR
        WHERE BNAME = W_PARNR.
      ENDIF.
      IT_OUTPUT-HERSTELLER = W_PARNR.
    ELSE.
      SELECT SINGLE NAME_TEXTC INTO IT_OUTPUT-USERNAME
      FROM USER_ADDR
      WHERE BNAME = IT_OUTPUT-HERSTELLER.

*      SELECT SINGLE ENAME INTO IT_OUTPUT-USERNAME
*        FROM PA0001
*        WHERE PERNR = W_PARNR.  "IT_OUTPUT-HERSTELLER.
    ENDIF.
** End of change
    APPEND IT_OUTPUT.
    CLEAR: IT_OUTPUT, OBJECT_TAB.
  ENDLOOP.
  SORT IT_OUTPUT BY OTGRP FECOD MATNR.
  CALL SCREEN 200.


*---------------------------------------------------------------------*
*       MODULE status_0200 OUTPUT                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
MODULE STATUS_0200 OUTPUT.
  SET PF-STATUS 'ST200'.
  SET TITLEBAR 'T200'.
ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE DISPLAY_ALV_200 OUTPUT.
  IF GRID_CONTAINER IS INITIAL. "/Not Created Control for ALV GRID
    PERFORM CREATE_CONTAINER_N_OBJECT.
    PERFORM SET_ATTRIBUTES_ALV_GRID.
    PERFORM BUILD_SORTCAT_DISPLAY.
    PERFORM BUILD_FIELD_CATALOG USING 'IT_OUTPUT'.
    PERFORM ASSIGN_ITAB_TO_ALV.
*    PERFORM sssign_event_9000.
  ELSE.
    CALL METHOD ALV_GRID->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDMODULE.                 " DISPLAY_ALV_200  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  create_container_n_object_9000
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT.
  DATA:   W_REPID LIKE SY-REPID.
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

*- If the parameter, i_appl_events, is set, the ALV Grid Control
*  registers all events as application events. If the parameter is not
*  set, all events are registered as system events.
  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " create_container_n_object
*&---------------------------------------------------------------------*
*&      Form  set_attributes_alv_grid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID.
  DATA : LW_S_DRAGDROP TYPE LVC_S_DD01. "/ Drag&Drop control settings

  CLEAR : WA_IS_LAYOUT, WA_VARIANT.

*//-- Set Layout Structure
  WA_IS_LAYOUT-EDIT       = ' '.      "/Edit Mode Enable
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging

*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " set_attributes_alv_grid
*&---------------------------------------------------------------------*
*&      Form  build_sortcat_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_SORTCAT_DISPLAY.

*  it_sort-spos           = 1.
*  it_sort-fieldname      = 'LIFNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 2.
*  it_sort-fieldname      = 'MATNR'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 3.
*  it_sort-fieldname      = 'WERKS'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.
*
*  it_sort-spos           = 4.
*  it_sort-fieldname      = 'DISPO'.
*  it_sort-up             = 'X'.
*  it_sort-subtot         = ' '.
*  APPEND it_sort.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0027   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG USING P_ITAB.

  DATA: LW_ITAB TYPE SLIS_TABNAME,
        LW_WAERS LIKE T001-WAERS,
        L_RQTY(9),
        L_DATUM(8),
        L_CN(2) TYPE N.

  CLEAR: IT_FIELDCAT,  IT_FIELDCAT[],
         IT_FIELDNAME, IT_FIELDNAME[].
  CLEAR: W_CNT,W_REPID.

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

                                 'S' 'OTGRP'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'CodeGrp',
                                  'E' 'OUTPUTLEN'   '10',

                                   'S' 'FECOD'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Prob',
                                  'E' 'OUTPUTLEN'   '5',

                                  'S' 'MATNR'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '18',

*                                  'S' 'ANZFEHLER'   ' ',
                                  'S' 'SCAN'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Scan Events',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'QMNUM'        ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Notification',
                                  'E' 'OUTPUTLEN'   '10',


                                  'S' 'LIFNUM'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Vendor',
                                  'E' 'OUTPUTLEN'   '10',
*
*
*                                  'S' 'STPRS'       ' ',
*                                  ' ' 'COLTEXT'     'Std Cost',
*                                  ' ' 'DECIMALS_O'  '2',
*                                  ' ' 'NO_ZERO'     'X',
*                                  'E' 'OUTPUTLEN'   '11',


                                  'S' 'BZMNG'       ' ',
                                  ' ' 'COLTEXT'     'Act. Qty',
                                  ' ' 'DECIMALS_O'  '2',
                                  ' ' 'NO_ZERO'     'X',
                                  'E' 'OUTPUTLEN'   '12',

*
*                                  'S' 'AMOUNT'       ' ',
*                                  ' ' 'COLTEXT'     'Total Amount',
*                                  ' ' 'DECIMALS_O'  '2',
*                                  ' ' 'NO_ZERO'     'X',
*                                  'E' 'OUTPUTLEN'   '13',


                                  'S' 'QMTXT'         ' ',
                                  ' ' 'COLTEXT'     'Description',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'MATKTX'       ' ',
                               ' ' 'COLTEXT'     'Material Description',
                                  'E' 'OUTPUTLEN'   '30',

*                                  'S' 'DMBTR'       ' ',
*                                  ' ' 'DO_SUM'      'X',
*                                  ' ' 'COLTEXT'     'Trn Amount',
*                                  'E' 'CURRENCY'    lw_waers,

                                  'S' 'FEKTXTCD'       ' ',
                                  ' ' 'COLTEXT'     'Problem Code Text',
                                  'E' 'OUTPUTLEN'   '40',

                                  'S' 'CRDATE'       ' ',
                                  ' ' 'COLTEXT'     'Created on',
*                                  ' ' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'ERZEIT'       ' ',
                                  ' ' 'COLTEXT'     'Created at',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'ERNAM'       ' ',
                                  ' ' 'COLTEXT'     'Created by',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'HERSTELLER'  ' ',
                                  ' ' 'COLTEXT'     'User/MFG',
                                  'E' 'OUTPUTLEN'   '10',

** Changed by Furong on 09/15/08
                                  'S' 'USERNAME'  ' ',
                                  ' ' 'COLTEXT'     'User Name',
                                  'E' 'OUTPUTLEN'   '30',
** End of change on 09/15/08

                                  'S' 'CHDATE'       ' ',
                                  ' ' 'COLTEXT'     'Changed on',
*                                  'E' 'DATATYPE'    'DATS',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'AEZEIT'       ' ',
                                  ' ' 'COLTEXT'     'Changed at',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'AENAM'       ' ',
                                  ' ' 'COLTEXT'     'Changed by',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'STTXT'       ' ',
                                  ' ' 'COLTEXT'     'Status',
                                  'E' 'OUTPUTLEN'   '20'.


ENDFORM.                    " build_field_catalog

*---------------------------------------------------------------------*
*       FORM setting_fieldcat                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_FIELDCAT                                                    *
*  -->  P_GUBUN                                                       *
*  -->  P_FIELD                                                       *
*  -->  P_VALUE                                                       *
*---------------------------------------------------------------------*
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
*       FORM assign_itab_to_alv                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
*               i_default        = space
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_OUTPUT[].
*               it_sort          = it_sort[].

ENDFORM.                    " assign_itab_to_alv
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.
  CASE OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'DISP'.
      PERFORM DISPLAY_NOTIFICATION.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_SEARCH_HELP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0300   text
*      -->P_0301   text
*      -->P_0302   text
*----------------------------------------------------------------------*
FORM DISPLAY_SEARCH_HELP USING    P_KATALOGART
                                  P_FIELDNAME
                                  P_FIELDNAME1.

  DATA:L_REPID                 LIKE D020S-PROG,
       L_DYNNR                 LIKE SY-DYNNR,
       L_QMGRP                 LIKE VIQMEL-QMGRP.

  DATA: I_KATALOGART TYPE QPGR-KATALOGART,
        L_QPK1CD          LIKE QPK1CD,
        I_CODEGRUPPE LIKE  QPGR-CODEGRUPPE,
        I_CODE LIKE  QPCD-CODE VALUE '*' .
  DATA : T_CODEGRPTAB LIKE QPK1CODEGRP OCCURS 0 WITH HEADER LINE.

  DATA : BEGIN OF L_DYNFIELDTAB OCCURS 10.
          INCLUDE STRUCTURE DYNPREAD.
  DATA : END   OF L_DYNFIELDTAB.

  MOVE : SY-REPID TO L_REPID,
         SY-DYNNR TO L_DYNNR.

  IF P_FIELDNAME = 'S_FEGRP-LOW' OR
     P_FIELDNAME = 'S_FEGRP-HIGH'.

    MOVE P_FIELDNAME TO L_DYNFIELDTAB-FIELDNAME.
    APPEND L_DYNFIELDTAB.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        DYNAME                         = L_REPID
        DYNUMB                         = L_DYNNR
*       TRANSLATE_TO_UPPER             = ' '
*       REQUEST                        = ' '
*       PERFORM_CONVERSION_EXITS       = ' '
*       PERFORM_INPUT_CONVERSION       = ' '
*       DETERMINE_LOOP_INDEX           = ' '
      TABLES
        DYNPFIELDS                     = L_DYNFIELDTAB
*     EXCEPTIONS
*       INVALID_ABAPWORKAREA           = 1
*       INVALID_DYNPROFIELD            = 2
*       INVALID_DYNPRONAME             = 3
*       INVALID_DYNPRONUMMER           = 4
*       INVALID_REQUEST                = 5
*       NO_FIELDDESCRIPTION            = 6
*       INVALID_PARAMETER              = 7
*       UNDEFIND_ERROR                 = 8
*       DOUBLE_CONVERSION              = 9
*       STEPL_NOT_FOUND                = 10
*       OTHERS                         = 11
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    READ TABLE L_DYNFIELDTAB INDEX 1.
    L_QMGRP = L_DYNFIELDTAB-FIELDVALUE.

    CLEAR: L_DYNFIELDTAB, L_DYNFIELDTAB[].


    IF L_QMGRP = 'MXTX10' OR L_QMGRP = 'MXTX11' OR
       L_QMGRP = 'MXTX12' OR L_QMGRP = 'MXTX13' OR
       L_QMGRP = 'MXTX15' OR L_QMGRP = 'MXTX19' OR
       L_QMGRP = 'MXTX51' OR L_QMGRP = 'MXTX53'.

      T_CODEGRPTAB = '0'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '4'.
      APPEND T_CODEGRPTAB.
      T_CODEGRPTAB = '9'.
      APPEND T_CODEGRPTAB.
    ELSE.

      T_CODEGRPTAB = '*'.
      APPEND T_CODEGRPTAB.

    ENDIF.

    CLEAR : L_QMGRP.

  ELSE.

    T_CODEGRPTAB = '*'.
    APPEND T_CODEGRPTAB.

  ENDIF.

  I_KATALOGART = P_KATALOGART.

  CALL FUNCTION 'QPK1_GP_CODE_PICKUP'
    EXPORTING
      I_KATALOGART                 = I_KATALOGART
      I_CODEGRUPPE                 = I_CODEGRUPPE
      I_CODE                       = I_CODE
      I_SPRACHE                    = SY-LANGU
      I_WINX1                      = 10
      I_WINX2                      = 68
      I_WINY1                      = 5
      I_WINY2                      = 27
*   I_DISPLAY_MODE               =
*   I_RETURN_IF_ONE              = 'X'
*   I_RETURN_IF_MANY             =
*   I_NO_USAGEINDICATION         =
*   I_NO_AUTHORITY_CHECK         =
    IMPORTING
      E_QPK1CD                     = L_QPK1CD
    TABLES
      T_CODEGRPTAB                 = T_CODEGRPTAB
* EXCEPTIONS
*   NO_MATCH_IN_RANGE            = 1
*   NO_USER_SELECTION            = 2
*   NO_AUTHORIZATION             = 3
*   NO_SELECTION_SPECIFIED       = 4
*   OBJECT_LOCKED                = 5
*   LOCK_ERROR                   = 6
*   OBJECT_MISSING               = 7
*   OTHERS                       = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  MOVE P_FIELDNAME TO L_DYNFIELDTAB-FIELDNAME.
  MOVE  L_QPK1CD-CODEGRUPPE TO L_DYNFIELDTAB-FIELDVALUE.
  APPEND L_DYNFIELDTAB.

  MOVE P_FIELDNAME1 TO L_DYNFIELDTAB-FIELDNAME.
  MOVE  L_QPK1CD-CODE TO L_DYNFIELDTAB-FIELDVALUE.
  APPEND L_DYNFIELDTAB.

*  IF p_fieldname = 'QMGRP'.
*
*    MOVE  'OTGRP' TO l_dynfieldtab-fieldname.
*    MOVE  l_qpk1cd-codegruppe TO l_dynfieldtab-fieldvalue.
*    APPEND l_dynfieldtab.
*
*    MOVE  'OTEIL' TO l_dynfieldtab-fieldname.
*    MOVE  l_qpk1cd-code TO l_dynfieldtab-fieldvalue.
*    APPEND l_dynfieldtab.
*
*  ENDIF.

  IF P_FIELDNAME = 'P_FEGRP' AND L_QPK1CD-CODEGRUPPE = '0'.

    MOVE 'URGRP' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  'CAUS' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    MOVE 'URCOD' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  '04' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF P_FIELDNAME = 'P_FEGRP' AND L_QPK1CD-CODEGRUPPE = '4'.

    MOVE 'URGRP' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  'CAUS' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    MOVE 'URCOD' TO L_DYNFIELDTAB-FIELDNAME.
    MOVE  '02' TO L_DYNFIELDTAB-FIELDVALUE.
    APPEND L_DYNFIELDTAB.

    LOOP AT SCREEN.
      IF SCREEN-NAME = 'URGRP'.
        SCREEN-INPUT = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME               = L_REPID
            DYNUMB               = L_DYNNR
       TABLES
            DYNPFIELDS           = L_DYNFIELDTAB
       EXCEPTIONS
            INVALID_ABAPWORKAREA = 01
            INVALID_DYNPROFIELD  = 02
            INVALID_DYNPRONAME   = 03
            INVALID_DYNPRONUMMER = 04
            INVALID_REQUEST      = 05
            NO_FIELDDESCRIPTION  = 06
            UNDEFIND_ERROR       = 07.

  IF SY-SUBRC <> 0.
*    MESSAGE I009.
    EXIT.
  ENDIF.

ENDFORM.                    " DISPLAY_SEARCH_HELP
*&---------------------------------------------------------------------*
*&      Form  CLEAR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_VARIANT.
  CLEAR: S_FECOD, S_FECOD[].
ENDFORM.                    " CLEAR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  init_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INIT_DATA.
  QMART-SIGN = 'I'.
  QMART-OPTION = 'EQ'.
  QMART-LOW = 'Q4'.
  APPEND QMART.

  SELECT SINGLE LIFNR FROM ZTQM_VENDOR_ID  INTO PARNR_IN WHERE UNAME =
  SY-UNAME.
  IF SY-SUBRC = 0.
*    ZSQM_NOTI_SEL-LIFNUM = WA_VENDOR.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 'Q4'.
        SCREEN-INPUT = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " init_data
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_NOTIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_NOTIFICATION.
  DATA: LT_ROWS TYPE LVC_T_ROW WITH HEADER LINE,
        LT_ROW_NO TYPE LVC_T_ROID. "/Numeric IDs of Selected Rows
  DATA: L_LINE TYPE I.

  CALL METHOD ALV_GRID->GET_SELECTED_ROWS
           IMPORTING ET_INDEX_ROWS = LT_ROWS[]
                     ET_ROW_NO     = LT_ROW_NO.

  CALL METHOD CL_GUI_CFW=>FLUSH.

  IF SY-SUBRC NE 0.
    W_REPID = SY-REPID.
    CALL FUNCTION 'POPUP_TO_INFORM'
         EXPORTING
              TITEL = W_REPID
              TXT2  = SY-SUBRC
              TXT1  = 'Error found during flushing of ALV Grid Control'.
    EXIT.
  ENDIF.
*
*  CLEAR: w_select, w_success, w_fail.

  READ TABLE LT_ROWS INDEX 1.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M12.
  ENDIF.
  READ TABLE IT_OUTPUT INDEX LT_ROWS-INDEX.
  IF IT_OUTPUT-QMNUM = ' '.
    MESSAGE E000(ZZ) WITH TEXT-M01.
  ENDIF.
  SET PARAMETER ID 'IQM' FIELD IT_OUTPUT-QMNUM.
  CALL TRANSACTION 'QM03' AND SKIP FIRST SCREEN.
ENDFORM.                    " DISPLAY_NOTIFICATION
