*----------------------------------------------------------------------*
*   INCLUDE ZRPP113I_APS_2AB1_2F01                                     *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MAKE_DROPDOWN_LIST_BOX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX.
  CLEAR : XLIST[] , XVALUE.

  XVALUE-TEXT = 'EMF'.
  XVALUE-KEY  = '1'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT = 'CRA'.
  XVALUE-KEY  = '2'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT = 'ALL'.
  XVALUE-KEY  = '3'.
  APPEND XVALUE TO XLIST .

  PERFORM LIST_BOX_FUNCTION USING 'P_MODL'.
  READ TABLE XLIST INTO XVALUE  INDEX 1.
  P_MODL = XVALUE-KEY.

ENDFORM.                    " MAKE_DROPDOWN_LIST_BOX
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT
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

  CREATE OBJECT ALV_GRID
         EXPORTING I_PARENT = GRID_CONTAINER
                   I_APPL_EVENTS = 'X'.

ENDFORM.                    " CREATE_CONTAINER_N_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
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
  WA_IS_LAYOUT-EDIT       = ' '.
  WA_IS_LAYOUT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT-CWIDTH_OPT = 'X'.   "/optimizes the column width
  WA_IS_LAYOUT-INFO_FNAME = 'IF'.
*  WA_IS_LAYOUT-CTAB_FNAME = 'CT'.
*  wa_is_layout-no_merging = 'X'.   "/Disable cell merging
  WA_IS_LAYOUT-STYLEFNAME = 'CELLTAB'.
*//-- Set Variant Structure
  WA_VARIANT-REPORT       = SY-REPID.
  WA_VARIANT-USERNAME     = SY-UNAME.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0031   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG  USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME,
        L_MONTH0(9),
        L_MONTH1(9),
        L_MONTH2(9),
        L_MONTH3(9),
        L_MONTH4(9).

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

                                  'S' 'MODL'        ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Model',
                                   'E' 'OUTPUTLEN'   '6',

                                  'S' 'USEE'       ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Sales',
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'NATN'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Nation',
                                  'E' 'OUTPUTLEN'   '8',

  'S' 'ECNM'        ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     'Eng Capa',
  'E' 'OUTPUTLEN'   '25',

  'S' 'EGNM'        ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     'Eng Type',
  'E' 'OUTPUTLEN'   '25',

  'S' 'FUNM'        ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     'Fuel Type',
  'E' 'OUTPUTLEN'   '25',

  'S' 'TMNM'        ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     'T/M',
  'E' 'OUTPUTLEN'   '10',

  'S' 'NGRD'        ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     'Grade',
  'E' 'OUTPUTLEN'   '20',

  'S' 'PLNB0'       ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     V_MONTH0,
  'E' 'OUTPUTLEN'   '10',

  'S' 'PLNB1'       ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     V_MONTH1,
  'E' 'OUTPUTLEN'   '10',

  'S' 'PLNB2'       ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     V_MONTH2,
  'E' 'OUTPUTLEN'   '10',

  'S' 'PLNB3'       ' ',
  ' ' 'KEY'         ' ',
  ' ' 'COLTEXT'     V_MONTH3,
  'E' 'OUTPUTLEN'   '10',

                                  'S' 'PLNB4'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     V_MONTH4,
                                  'E' 'OUTPUTLEN'   '10',

                                  'S' 'TOTAL'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Total',
                                  'E' 'OUTPUTLEN'   '10'.

ENDFORM.                    " BUILD_FIELD_CATALOG
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
*               i_default        = space
*               IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
     CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT[]
               IT_OUTTAB        = IT_PVV02AB_OUT[].

*** ENTER
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
*
*
*  CALL METHOD CL_GUI_CONTROL=>SET_FOCUS
*                        EXPORTING CONTROL = ALV_GRID.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_DATA.
  DATA: L_MONTH(2),
        W_PVV02AB LIKE IT_PVV02AB_OUT,
        W_PVV01rr LIKE IT_PVV01rr_OUT.

  DATA: BEGIN OF LT_TEMP OCCURS 0,
        STDT LIKE ZTPP_PVV02AB-STDT,
        MODL LIKE ZTPP_PVV02AB-MODL,
        USEE LIKE ZTPP_PVV02AB-USEE,
        NATN LIKE ZTPP_PVV02AB-NATN,
        ECNM LIKE ZTPP_PVV02AB-ECNM,
        EGNM LIKE ZTPP_PVV02AB-EGNM,
        FUNM LIKE ZTPP_PVV02AB-FUNM,
        TMNM LIKE ZTPP_PVV02AB-TMNM,
        NGRD LIKE ZTPP_PVV02AB-NGRD,
        PLNB0 TYPE ZRP01TQ,
        PLNB1 TYPE ZRP01TQ,
        PLNB2 TYPE ZRP01TQ,
        PLNB3 TYPE ZRP01TQ,
        PLNB4 TYPE ZRP01TQ,
        END OF LT_TEMP.

  RANGES: R_MODL FOR ZTPP_PVV02AB-MODL,
          R_USEE FOR ZTPP_PVV02AB-USEE,
          R_NATN FOR ZTPP_PVV02AB-NATN,
          R_EXCO FOR ZTPP_PVV02AB-EXCO.

  REFRESH: IT_PVV02AB, IT_PVV02AB_OUT.
  CLEAR: IT_PVV02AB, IT_PVV02AB_OUT, IT_PVV01RR, IT_PVV01RR_OUT,
         W_PVV02AB, W_PVV01RR.

  REFRESH: IT_PVV01RR, IT_PVV01RR_OUT.

  IF P_MODL IS INITIAL.
  ELSE.
    R_MODL-SIGN =  'I'.
    R_MODL-OPTION =  'EQ'.
    R_MODL-LOW = P_MODL.
    APPEND R_MODL.
  ENDIF.

  IF P_USEE IS INITIAL.
  ELSE.
    R_USEE-SIGN =  'I'.
    R_USEE-OPTION =  'EQ'.
    R_USEE-LOW = P_USEE.
    APPEND R_USEE.
  ENDIF.

  IF P_NATN IS INITIAL.
  ELSE.
    R_NATN-SIGN =  'I'.
*    R_NATN-OPTION =  'EQ'.
    R_NATN-OPTION =  'CP'.
    R_NATN-LOW = P_NATN.
    APPEND R_NATN.
  ENDIF.

*  IF P_EXCO IS INITIAL.
*  ELSE.
*    R_EXCO-SIGN =  'I'.
*    R_EXCO-OPTION =  'EQ'.
*    R_EXCO-LOW = P_EXCO.
*    APPEND R_EXCO.
*  ENDIF.

  SELECT * INTO TABLE IT_PVV02AB
    FROM ZTPP_PVV02AB
   WHERE MODL IN R_MODL
     AND USEE IN R_USEE
     AND NATN IN R_NATN
*     AND EXCO IN R_EXCO
     AND ZUPDATE = ' '.

  LOOP AT IT_PVV02AB.
    MOVE-CORRESPONDING IT_PVV02AB TO LT_TEMP.
    COLLECT LT_TEMP.
  ENDLOOP.

  LOOP AT LT_TEMP.
    MOVE-CORRESPONDING LT_TEMP TO IT_PVV02AB_OUT.
    IT_PVV02AB_OUT-TOTAL = IT_PVV02AB_OUT-PLNB0 +
                           IT_PVV02AB_OUT-PLNB1 +
                           IT_PVV02AB_OUT-PLNB2 +
                           IT_PVV02AB_OUT-PLNB3 +
                           IT_PVV02AB_OUT-PLNB4.
    W_PVV02AB-PLNB0 = W_PVV02AB-PLNB0 + IT_PVV02AB_OUT-PLNB0.
    W_PVV02AB-PLNB1 = W_PVV02AB-PLNB1 + IT_PVV02AB_OUT-PLNB1.
    W_PVV02AB-PLNB2 = W_PVV02AB-PLNB2 + IT_PVV02AB_OUT-PLNB2.
    W_PVV02AB-PLNB3 = W_PVV02AB-PLNB3 + IT_PVV02AB_OUT-PLNB3.
    W_PVV02AB-PLNB4 = W_PVV02AB-PLNB4 + IT_PVV02AB_OUT-PLNB4.
    COLLECT IT_PVV02AB_OUT.
    CLEAR: IT_PVV02AB_OUT.
  ENDLOOP.

  W_PVV02AB-MODL = 'Total'.
  W_PVV02AB-TOTAL = W_PVV02AB-PLNB0 +
                           W_PVV02AB-PLNB1 +
                           W_PVV02AB-PLNB2 +
                           W_PVV02AB-PLNB3 +
                           W_PVV02AB-PLNB4.
  APPEND W_PVV02AB TO IT_PVV02AB_OUT.

  READ TABLE IT_PVV02AB INDEX 1.
** Changed by Furong on 01/12/09 , no data
   if sy-subrc = 0.
** End of change
  L_MONTH = IT_PVV02AB-STDT+4(2).
  SELECT SINGLE KTX INTO V_MONTH0
    FROM T247
    WHERE SPRAS = 'E'
      AND MNR  = L_MONTH.
  L_MONTH = L_MONTH + 1.
  IF L_MONTH > 12.
    L_MONTH = '01'.
  ENDIF.
  SELECT SINGLE KTX INTO V_MONTH1
    FROM T247
    WHERE SPRAS = 'E'
      AND MNR  = L_MONTH.
  L_MONTH = L_MONTH + 1.
  IF L_MONTH > 12.
    L_MONTH = '01'.
  ENDIF.
  SELECT SINGLE KTX INTO V_MONTH2
    FROM T247
    WHERE SPRAS = 'E'
      AND MNR  = L_MONTH.
  L_MONTH = L_MONTH + 1.
  IF L_MONTH > 12.
    L_MONTH = '01'.
  ENDIF.
  SELECT SINGLE KTX INTO V_MONTH3
    FROM T247
    WHERE SPRAS = 'E'
      AND MNR  = L_MONTH.
  L_MONTH = L_MONTH + 1.
  IF L_MONTH > 12.
    L_MONTH = '01'.
  ENDIF.
  SELECT SINGLE KTX INTO V_MONTH4
    FROM T247
    WHERE SPRAS = 'E'
      AND MNR  = L_MONTH.
 endif.

  SELECT * INTO TABLE IT_PVV01RR
      FROM ZTPP_PVV01RR
     WHERE MODL IN R_MODL
       AND ZUPDATE = ' '.

  LOOP AT IT_PVV01RR.
    MOVE-CORRESPONDING IT_PVV01RR TO IT_PVV01RR_OUT.
    IT_PVV01RR_OUT-DESC = IT_PVV01RR-MODL.
    IT_PVV01RR_OUT-TOTAL = IT_PVV01RR_OUT-PLNB0 +
                           IT_PVV01RR_OUT-PLNB1 +
                           IT_PVV01RR_OUT-PLNB2 +
                           IT_PVV01RR_OUT-PLNB3 +
                           IT_PVV01RR_OUT-PLNB4.
    W_PVV01rr-PLNB0 = W_PVV01rr-PLNB0 + IT_PVV01RR_OUT-PLNB0.
    W_PVV01rr-PLNB1 = W_PVV01rr-PLNB1 + IT_PVV01RR_OUT-PLNB1.
    W_PVV01rr-PLNB2 = W_PVV01rr-PLNB2 + IT_PVV01RR_OUT-PLNB2.
    W_PVV01rr-PLNB3 = W_PVV01rr-PLNB3 + IT_PVV01RR_OUT-PLNB3.
    W_PVV01rr-PLNB4 = W_PVV01rr-PLNB4 + IT_PVV01RR_OUT-PLNB4.
    APPEND IT_PVV01RR_OUT.
    CLEAR: IT_PVV01RR_OUT.
  ENDLOOP.
  W_PVV01rr-desc = 'Total'.
  W_PVV01rr-TOTAL = W_PVV01rr-PLNB0 +
                           W_PVV01rr-PLNB1 +
                           W_PVV01rr-PLNB2 +
                           W_PVV01rr-PLNB3 +
                           W_PVV01rr-PLNB4.
  APPEND W_PVV01rr TO IT_PVV01rr_OUT.

ENDFORM.                    " SELECT_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_DATA.
  CLEAR IT_EXCEL.
  REFRESH IT_EXCEL.
  MOVE 'Model' TO IT_EXCEL-COL01.
  MOVE 'Sales' TO IT_EXCEL-COL02.
  MOVE 'Nation' TO IT_EXCEL-COL03.
  MOVE 'Eng Capa' TO IT_EXCEL-COL04.
  MOVE 'Eng Type' TO IT_EXCEL-COL05.
  MOVE 'Fuel Type' TO IT_EXCEL-COL06.
  MOVE 'Trim' TO IT_EXCEL-COL07.
  MOVE 'Grade' TO IT_EXCEL-COL08.
  MOVE V_MONTH0 TO IT_EXCEL-COL09.
  MOVE V_MONTH1 TO IT_EXCEL-COL10.
  MOVE V_MONTH2 TO IT_EXCEL-COL11.
  MOVE V_MONTH3 TO IT_EXCEL-COL12.
  MOVE V_MONTH4 TO IT_EXCEL-COL13.
  MOVE 'Total' TO IT_EXCEL-COL14.
  APPEND IT_EXCEL.

  LOOP AT IT_PVV02AB_OUT.
    CLEAR IT_EXCEL.
    MOVE IT_PVV02AB_OUT-MODL TO IT_EXCEL-COL01.
    MOVE IT_PVV02AB_OUT-USEE TO IT_EXCEL-COL02.
    MOVE IT_PVV02AB_OUT-NATN TO IT_EXCEL-COL03.
    MOVE IT_PVV02AB_OUT-ECNM TO IT_EXCEL-COL04.
    MOVE IT_PVV02AB_OUT-EGNM TO IT_EXCEL-COL05.
    MOVE IT_PVV02AB_OUT-FUNM TO IT_EXCEL-COL06.
    MOVE IT_PVV02AB_OUT-TMNM TO IT_EXCEL-COL07.
    MOVE IT_PVV02AB_OUT-NGRD TO IT_EXCEL-COL08.
    MOVE IT_PVV02AB_OUT-PLNB0 TO IT_EXCEL-COL09.
    MOVE IT_PVV02AB_OUT-PLNB1 TO IT_EXCEL-COL10.
    MOVE IT_PVV02AB_OUT-PLNB2 TO IT_EXCEL-COL11.
    MOVE IT_PVV02AB_OUT-PLNB3 TO IT_EXCEL-COL12.
    MOVE IT_PVV02AB_OUT-PLNB4 TO IT_EXCEL-COL13.
    MOVE IT_PVV02AB_OUT-TOTAL TO IT_EXCEL-COL14.
    APPEND IT_EXCEL.
  ENDLOOP.

  CALL FUNCTION 'DOWNLOAD'
   EXPORTING
     FILENAME                      = 'MONTHLY_PLAN.XLS'
     FILETYPE                      = 'DAT'
     ITEM                          = ' '
*     FILETYPE_NO_CHANGE            = ' '
*     FILETYPE_NO_SHOW              = ' '
*     SILENT                        = 'S'
*     COL_SELECT                    = ' '
*     COL_SELECTMASK                = ' '
*     NO_AUTH_CHECK                 = ' '
*   IMPORTING
*     ACT_FILENAME                  =
*     ACT_FILETYPE                  =
*     FILESIZE                      =
*     CANCEL                        =
    TABLES
      DATA_TAB                      = IT_EXCEL
*   EXCEPTIONS
*     INVALID_FILESIZE              = 1
*     INVALID_TABLE_WIDTH           = 2
*     INVALID_TYPE                  = 3
*     NO_BATCH                      = 4
*     UNKNOWN_ERROR                 = 5
*     GUI_REFUSE_FILETRANSFER       = 6
*     CUSTOMER_ERROR                = 7
*     OTHERS                        = 8
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " DOWNLOAD_DATA
*&---------------------------------------------------------------------*
*&      Form  LIST_BOX_FUNCTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0040   text
*----------------------------------------------------------------------*
FORM LIST_BOX_FUNCTION USING   P_LIST_NAME .
  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID              = P_LIST_NAME  " list box
            VALUES          = XLIST
       EXCEPTIONS
            ID_ILLEGAL_NAME = 1
            OTHERS          = 2.

ENDFORM.                    " LIST_BOX_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*      -->P_0206   text
*      -->P_0207   text
*      -->P_0208   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).
  FIELD-SYMBOLS <FS>.

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
ENDFORM.                    " SETTING_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MAKE_DEFAULT_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DEFAULT_MONTH.
  DATA: L_MONTH(2).
  IF V_MONTH0 IS INITIAL.
    L_MONTH = SY-DATUM+4(2).
    SELECT SINGLE KTX INTO V_MONTH0
      FROM T247
      WHERE SPRAS = 'E'
        AND MNR  = L_MONTH.
    L_MONTH = L_MONTH + 1.
    IF L_MONTH > 12.
      L_MONTH = '01'.
    ENDIF.
    SELECT SINGLE KTX INTO V_MONTH1
      FROM T247
      WHERE SPRAS = 'E'
        AND MNR  = L_MONTH.
    L_MONTH = L_MONTH + 1.
    IF L_MONTH > 12.
      L_MONTH = '01'.
    ENDIF.
    SELECT SINGLE KTX INTO V_MONTH2
      FROM T247
      WHERE SPRAS = 'E'
        AND MNR  = L_MONTH.
    L_MONTH = L_MONTH + 1.
    IF L_MONTH > 12.
      L_MONTH = '01'.
    ENDIF.
    SELECT SINGLE KTX INTO V_MONTH3
      FROM T247
      WHERE SPRAS = 'E'
        AND MNR  = L_MONTH.
    L_MONTH = L_MONTH + 1.
    IF L_MONTH > 12.
      L_MONTH = '01'.
    ENDIF.
    SELECT SINGLE KTX INTO V_MONTH4
      FROM T247
      WHERE SPRAS = 'E'
        AND MNR  = L_MONTH.
  ENDIF.
ENDFORM.                    " MAKE_DEFAULT_MONTH
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_N_OBJECT_tot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_CONTAINER_N_OBJECT_TOT.
  DATA:   W_REPID LIKE SY-REPID.
  CREATE OBJECT GRID_CONTAINER_TOT
          EXPORTING CONTAINER_NAME = WA_CUSTOM_CONTROL_TOT
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
              TXT1  = 'The control- Tot can not be created'.
  ENDIF.

  CREATE OBJECT ALV_GRID_TOT
         EXPORTING I_PARENT = GRID_CONTAINER_TOT
                   I_APPL_EVENTS = 'X'.
ENDFORM.                    " CREATE_CONTAINER_N_OBJECT_tot
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_tot
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_ATTRIBUTES_ALV_GRID_TOT.

  CLEAR : WA_IS_LAYOUT_TOT, WA_VARIANT_TOT.

*//-- Set Layout Structure
  WA_IS_LAYOUT_TOT-EDIT       = ' '.
  WA_IS_LAYOUT_TOT-SEL_MODE   = 'A'.      "/mode for select col and row
  WA_IS_LAYOUT_TOT-LANGUAGE   = SY-LANGU. "/Language Key
  WA_IS_LAYOUT_TOT-CWIDTH_OPT = ' '.   "/optimizes the column width
*//-- Set Variant Structure
  WA_VARIANT_TOT-REPORT       = SY-REPID.
  WA_VARIANT_TOT-USERNAME     = SY-UNAME.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_tot

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0031   text
*----------------------------------------------------------------------*
FORM BUILD_FIELD_CATALOG_TOT  USING P_ITAB.
  DATA: LW_ITAB TYPE SLIS_TABNAME,
        L_MONTH0(9),
        L_MONTH1(9),
        L_MONTH2(9),
        L_MONTH3(9),
        L_MONTH4(9).

  CLEAR: IT_FIELDCAT_TOT,  IT_FIELDCAT_TOT[],
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
            CT_FIELDCAT        = IT_FIELDNAME_TOT.

  PERFORM SETTING_FIELDCAT_TOT TABLES IT_FIELDCAT_TOT USING :

                                  'S' 'DESC'        ' ',
                                  ' ' 'KEY'         'X',
                                  ' ' 'COLTEXT'     'Model',
                                   'E' 'OUTPUTLEN'   '68',

                                  'S' 'PLNB0'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     V_MONTH0,
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'PLNB1'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     V_MONTH1,
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'PLNB2'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     V_MONTH2,
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'PLNB3'       ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     V_MONTH3,
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'PLNB4'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     V_MONTH4,
                                  'E' 'OUTPUTLEN'   '6',

                                  'S' 'TOTAL'      ' ',
                                  ' ' 'KEY'         ' ',
                                  ' ' 'COLTEXT'     'Total',
                                  'E' 'OUTPUTLEN'   '7'.

ENDFORM.                    " BUILD_FIELD_CATALOG_tot
*&---------------------------------------------------------------------*
*&      Form  SETTING_FIELDCAT_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT_T0T  text
*      -->P_0206   text
*      -->P_0207   text
*      -->P_0208   text
*----------------------------------------------------------------------*
FORM SETTING_FIELDCAT_TOT TABLES   P_FIELDCAT STRUCTURE IT_FIELDCAT_TOT
                      USING    P_GUBUN
                               P_FIELD
                               P_VALUE.
  DATA : L_COL(40).
  FIELD-SYMBOLS <FS>.

  IF P_GUBUN = 'S'.
    CLEAR: P_FIELDCAT.
    READ TABLE IT_FIELDNAME_TOT INTO W_FIELDNAME
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
ENDFORM.                    " SETTING_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_TOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ASSIGN_ITAB_TO_ALV_TOT.
  CALL METHOD ALV_GRID_TOT->SET_TABLE_FOR_FIRST_DISPLAY

  EXPORTING   IS_LAYOUT        = WA_IS_LAYOUT_TOT
              I_SAVE           = WA_SAVE_TOT
              IS_VARIANT       = WA_VARIANT_TOT
*               i_default        = space
              IT_TOOLBAR_EXCLUDING = IT_EXCLUDE
    CHANGING  IT_FIELDCATALOG  = IT_FIELDCAT_TOT[]
              IT_OUTTAB        = IT_PVV01RR_OUT[].


ENDFORM.                    " ASSIGN_ITAB_TO_ALV_TOT
*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXCLUDE_TB_FUNCTIONS.
  DATA LS_EXCLUDE TYPE UI_FUNC.

* Row manipulation
  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_EXCL_ALL.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_MOVE_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_CUT.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*
**  Sort buttons
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO IT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
  APPEND LS_EXCLUDE TO IT_EXCLUDE.

ENDFORM.                    " EXCLUDE_TB_FUNCTIONS
