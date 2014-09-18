************************************************************************
* Program Name      : ZMMR208_GDISSUE_CRIB
* Author            : Furong Wang
* Creation Date     : 01/25/2008
* Specifications By :
* Development Request No :
* Addl Documentation:
* Description       : Monthly Good Receiving
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZMMR208_GDISSUE_CRIB NO STANDARD PAGE HEADING LINE-SIZE 255
                       MESSAGE-ID ZMPP.

TYPE-POOLS: SLIS,VRM.
TABLES: MSEG, MKPF, ZTMM_CRIB_GRP.

DATA: BEGIN OF IT_DATA OCCURS 0,
      LGORT LIKE MSEG-LGORT,
      AREA(3),
      STATION(3),
      SHIFT(3),
      MATNR LIKE MARA-MATNR,
      MAKTX LIKE MAKT-MAKTX,
      BWART LIKE MSEG-BWART,
      BUDAT LIKE MKPF-BUDAT,
      MENGE LIKE MSEG-MENGE,
*      ERFMG LIKE MSEG-ERFMG,
      AMOUNT LIKE MSEG-DMBTR,
      WERKS LIKE MSEG-WERKS,
      LGPBE LIKE MARD-LGPBE,
      END OF IT_DATA.

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

DATA: OK_CODE LIKE SY-UCOMM,
      W_REPID LIKE SY-REPID,
      W_CNT   TYPE   I.

DATA: NAME  TYPE VRM_ID,
      LIST  TYPE VRM_VALUES,
      VALUE LIKE LINE OF LIST,
      W_ENTER(1).

DATA : BEGIN OF VALUE_STA OCCURS 0,
       STATION LIKE ZTMM_CRIB_GRP-STATION,
       END OF VALUE_STA.
*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-T01.
SELECT-OPTIONS: S_DATE FOR SY-DATUM,
            S_LGORT FOR MSEG-LGORT OBLIGATORY,
            S_BWART FOR MSEG-BWART,
            S_AREA FOR ZTMM_CRIB_GRP-AREA,
            S_STA FOR ZTMM_CRIB_GRP-STATION,
            S_SHIFT FOR ZTMM_CRIB_GRP-SHIFT, "AS LISTBOX VISIBLE
            S_MATNR FOR MSEG-MATNR.
SELECTION-SCREEN END OF BLOCK BL1.

INITIALIZATION.
  PERFORM SET_DATA.

*AT SELECTION-SCREEN OUTPUT.
AT SELECTION-SCREEN ON S_AREA.
  PERFORM SCREEN_OUTPUT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_AREA-LOW.
  PERFORM GET_VALUE_AREA.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_SHIFT-LOW.
  PERFORM GET_VALUE_SHIFT.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_STA-LOW.
  PERFORM GET_VALUE_STA.

*AT SELECTION-SCREEN OUTPUT.
*  PERFORM SET_LISTBOX_RSNCODE.

START-OF-SELECTION.
  PERFORM READ_DATA.
  PERFORM DISPLAY_DATA.

*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA.
  DATA: BEGIN OF LT_TMP OCCURS 0,
      LGORT LIKE MSEG-LGORT,
      MATNR LIKE MARA-MATNR,
      MAKTX LIKE MAKT-MAKTX,
      BWART LIKE MSEG-BWART,
      BUDAT LIKE MKPF-BUDAT,
      MENGE LIKE MSEG-MENGE,
*      ERFMG LIKE MSEG-ERFMG,
      VERPR LIKE MBEW-VERPR,
      BKTXT LIKE MKPF-BKTXT,
      SHKZG LIKE MSEG-SHKZG,
      werks like mseg-werks,
      END OF LT_TMP.
  DATA: L_INDEX LIKE SY-TABIX.

  SELECT B~LGORT B~MATNR D~MAKTX B~BWART A~BUDAT
         B~MENGE C~VERPR A~BKTXT B~SHKZG b~werks
    INTO TABLE LT_TMP
    FROM MKPF AS A
    INNER JOIN MSEG AS B
    ON A~MBLNR = B~MBLNR
    AND A~MJAHR = B~MJAHR
    INNER JOIN MBEW AS C
    ON B~MATNR = C~MATNR
    AND B~WERKS = C~BWKEY
    INNER JOIN MAKT AS D
    ON B~MATNR = D~MATNR
    WHERE A~BUDAT IN S_DATE
       AND B~MATNR IN S_MATNR
            AND B~BWART IN S_BWART
      AND B~LGORT IN S_LGORT
      AND D~SPRAS = 'EN'.

  IF LT_TMP[] IS INITIAL.
    MESSAGE I001 WITH TEXT-M01.
  ENDIF.

  LOOP AT LT_TMP.
    MOVE-CORRESPONDING LT_TMP TO IT_DATA.
    IF LT_TMP-SHKZG = 'H'.
*       IT_DATA-ERFMG = - IT_DATA-ERFMG.
      IT_DATA-MENGE = - IT_DATA-MENGE.
      IT_DATA-AMOUNT = - LT_TMP-MENGE * LT_TMP-VERPR.
    ELSE.
      IT_DATA-AMOUNT = LT_TMP-MENGE * LT_TMP-VERPR.
    ENDIF.
** Changed by Furong on 09/04/08
*    IT_DATA-AREA = LT_TMP-BKTXT+0(3).
*    IF IT_DATA-AREA IN S_AREA.
*      IT_DATA-STATION = LT_TMP-BKTXT+4(3).
*      IF IT_DATA-STATION IN S_STA.
*        IT_DATA-SHIFT = LT_TMP-BKTXT+8(3).
*        IF IT_DATA-SHIFT IN S_SHIFT.
*          COLLECT IT_DATA.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    IT_DATA-SHIFT = LT_TMP-BKTXT+0(3).
    IF IT_DATA-SHIFT IN S_SHIFT.
      IT_DATA-AREA = LT_TMP-BKTXT+4(3).
      IF IT_DATA-AREA IN S_AREA.
        IT_DATA-STATION = LT_TMP-BKTXT+8(3).
        IF IT_DATA-STATION IN S_STA.
          COLLECT IT_DATA.
        ENDIF.
      ENDIF.
    ENDIF.

** End of change
    CLEAR: IT_DATA.
  ENDLOOP.
  CLEAR: LT_TMP[].

  LOOP AT IT_DATA.
    L_INDEX = SY-TABIX.
    IF IT_DATA-AREA IN S_AREA AND
       IT_DATA-STATION IN S_STA AND
       IT_DATA-SHIFT IN S_SHIFT.
    ELSE.
      DELETE IT_DATA INDEX L_INDEX.
    ENDIF.
    IF IT_DATA-SHIFT+0(1) = 'O' OR IT_DATA-SHIFT+0(1) = '0'.
      IT_DATA-SHIFT = IT_DATA-SHIFT+1(2).
    ENDIF.
    IF IT_DATA-AREA+0(1) = 'O' OR IT_DATA-AREA+0(1) = '0'.
      IT_DATA-AREA = IT_DATA-AREA+1(2).
    ENDIF.
    IF IT_DATA-STATION+0(1) = 'O' OR IT_DATA-STATION+0(1) = '0'.
      IT_DATA-STATION = IT_DATA-STATION+1(2).
    ENDIF.
** changed by Furong on 06/30/10
   select single LGPBE into it_data-LGPBE
     from mard
     where matnr = IT_DATA-MATNR
       and werks = IT_DATA-WERKS
       and lgort = IT_DATA-LGORT.
** End of change
    MODIFY IT_DATA.  " INDEX L_INDEX
  ENDLOOP.
  SORT IT_DATA BY LGORT SHIFT AREA STATION MATNR.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATA.
  S_DATE-SIGN = 'I'.
  S_DATE-OPTION = 'EQ'.
  S_DATE-LOW = SY-DATUM.
  APPEND S_DATE.
  S_BWART-SIGN = 'I'.
  S_BWART-OPTION = 'BT'.
  S_BWART-LOW = '291'.
  S_BWART-HIGH = '292'.
  APPEND S_BWART.

  SELECT STATION FROM ZTMM_CRIB_GRP
           INTO TABLE VALUE_STA.
  .

ENDFORM.                    " SET_DATA
*&---------------------------------------------------------------------*
*&      Form  display_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
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
*  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'CT'.
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
  IT_SORT-SPOS           = 1.
  IT_SORT-FIELDNAME      = 'LGORT'.
  IT_SORT-UP             = 'X'.
  IT_SORT-SUBTOT         = 'X'.
  APPEND IT_SORT.

ENDFORM.                    " build_sortcat_display
*&---------------------------------------------------------------------*
*&      Form  build_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0402   text
*----------------------------------------------------------------------*
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

                                  'S' 'LGORT'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'StLocation',
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'SHIFT'     ' ',
                                  ' ' 'COLTEXT'     'Shift',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'AREA'       ' ',
                                  ' ' 'COLTEXT'     'Area',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'STATION'       ' ',
                                  ' ' 'COLTEXT'     'Station',
                                  'E' 'OUTPUTLEN'   '8',

                                  'S' 'MATNR'        ' ',
                                  ' ' 'COLTEXT'     'Material',
                                  'E' 'OUTPUTLEN'   '20',

                                  'S' 'MAKTX'        ' ',
                                  ' ' 'COLTEXT'     'Text',
                                  'E' 'OUTPUTLEN'   '35',

                                  'S' 'BWART'       ' ',
                                  ' ' 'COLTEXT'     'Mvt',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'BUDAT'         ' ',
                                  ' ' 'COLTEXT'     'Post Date',
                                  'E' 'OUTPUTLEN'   '12',

                                  'S' 'MENGE'       ' ',
                                  ' ' 'COLTEXT'     'Quantity',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'AMOUNT'       ' ',
                                  ' ' 'COLTEXT'     'Amount',
                                  'E' 'OUTPUTLEN'   '15',

                                  'S' 'LGPBE'       ' ',
                                  ' ' 'COLTEXT'     'Stor. Bin',
                                  'E' 'OUTPUTLEN'   '9'.



ENDFORM.                    " build_field_catalog
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV.

  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY

   EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT
               I_SAVE           = WA_SAVE
               IS_VARIANT       = WA_VARIANT
               I_DEFAULT        = SPACE
*               it_toolbar_excluding = it_toolbar_excluding[]
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_DATA[]
               IT_SORT          = IT_SORT[].

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
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
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
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
**&---------------------------------------------------------------------
**
**&      Form  SET_LISTBOX_RSNCODE
**&---------------------------------------------------------------------
**
**       text
**----------------------------------------------------------------------
**
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------
**
*FORM SET_LISTBOX_RSNCODE.
*  CLEAR: NAME, VALUE, LIST.
*
*  NAME = 'S_SHIFT'.
*
*  MOVE: 'NF'      TO VALUE-KEY,
*        ' '       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  MOVE: 'CM'      TO VALUE-KEY,
*        ' '       TO VALUE-TEXT.
*  APPEND VALUE TO LIST.
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*       EXPORTING
*            ID     = NAME
*            VALUES = LIST.
*
*ENDFORM.                    " SET_LISTBOX_RSNCODE
*&---------------------------------------------------------------------*
*&      Form  get_value_area
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VALUE_AREA.
  DATA : BEGIN OF VALUE_AREA OCCURS 0,
         AREA LIKE ZTMM_CRIB_GRP-AREA,
         END OF VALUE_AREA.

  SELECT AREA FROM ZTMM_CRIB_GRP
         INTO TABLE VALUE_AREA.
  DELETE ADJACENT DUPLICATES FROM VALUE_AREA.

  W_REPID = SY-REPID.

* Set F4 values for Module Code

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'AREA'
            DYNPPROG        = W_REPID
            DYNPNR          = '1000'
            DYNPROFIELD     = 'AREA'
            WINDOW_TITLE    = 'Area Codes'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_AREA
       EXCEPTIONS
            PARAMETER_ERROR = 1.
ENDFORM.                    " get_value_area
*&---------------------------------------------------------------------*
*&      Form  get_value_shift
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VALUE_SHIFT.
  DATA : BEGIN OF VALUE_SHIFT OCCURS 0,
         SHIFT LIKE ZTMM_CRIB_GRP-SHIFT,
         END OF VALUE_SHIFT.

  VALUE_SHIFT-SHIFT = 'NF'.
  APPEND VALUE_SHIFT.
  VALUE_SHIFT-SHIFT = 'CM'.
  APPEND VALUE_SHIFT.

  W_REPID = SY-REPID.

* Set F4 values for Module Code

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'SHIFT'
            DYNPPROG        = W_REPID
            DYNPNR          = '1000'
            DYNPROFIELD     = 'STATION'
            WINDOW_TITLE    = 'Shift'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_SHIFT
       EXCEPTIONS
            PARAMETER_ERROR = 1.

ENDFORM.                    " get_value_shift
*&---------------------------------------------------------------------*
*&      Form  get_value_sta
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_VALUE_STA.
* DATA : BEGIN OF VALUE_STA OCCURS 0,
*        STATION LIKE ZTMM_CRIB_GRP-STATION,
*        END OF VALUE_STA.
  IF  W_ENTER = 'T'.
    W_ENTER = 'F'.
  ELSE.
    SELECT STATION FROM ZTMM_CRIB_GRP
           INTO TABLE VALUE_STA.
  ENDIF.

  W_REPID = SY-REPID.

* Set F4 values for Module Code

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
       EXPORTING
            RETFIELD        = 'STATION'
            DYNPPROG        = W_REPID
            DYNPNR          = '1000'
            DYNPROFIELD     = 'STATION'
            WINDOW_TITLE    = 'Station'
            VALUE_ORG       = 'S'
       TABLES
            VALUE_TAB       = VALUE_STA
       EXCEPTIONS
            PARAMETER_ERROR = 1.

ENDFORM.                    " get_value_sta
*&---------------------------------------------------------------------*
*&      Form  SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SCREEN_OUTPUT.
  W_ENTER = 'T'.
  IF S_AREA[] IS INITIAL.
    SELECT STATION FROM ZTMM_CRIB_GRP
         INTO TABLE VALUE_STA.
  ELSE.
    SELECT STATION FROM ZTMM_CRIB_GRP
         INTO TABLE VALUE_STA
         WHERE AREA IN S_AREA.
  ENDIF.
ENDFORM.                    " SCREEN_OUTPUT
