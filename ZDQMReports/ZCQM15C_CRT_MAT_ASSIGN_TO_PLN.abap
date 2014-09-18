************************************************************************
* Program Name      : ZCQM15C_CRT_MAT_ASSIGN_TO_PLN
* Author            : SeungLyong, Lee
* Creation Date     : 2004.03.20.
* Specifications By : SeungLyong, Lee
* Pattern           : 2.Conversion - 2.3 Call Transaction
* Development Request No :
* Addl Documentation:
* Description       :
*   - Create ISIR material
*   - Assign material to inspection plan
*   - Using T-Code : 'MM01', 'QP02' : BDC
*
* Related Table List:
*   MARA - Material Master
*   QMAT - Inspection type - material parameters
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************

REPORT  ZCQM15C_CRT_MAT_ASSIGN_TO_PLN  NO STANDARD PAGE HEADING
                                          LINE-SIZE 250.


TYPE-POOLS: VRM.

*-- Declaration Tables
TABLES : MAPL, PLKO, PLPO, PLMK, MARA, MARC.

*-- material assignment table : Excel format
DATA : BEGIN OF IT_CRT_MAT OCCURS 0,
        PLNNR  TYPE  EXTWG,   "Ext. Mat group : Task list group = PLNNR
        PLNAL  TYPE  PLNAL,    "Group Counters
        MATNR  TYPE  MATNR,         "/MATERIAL
        WERKS  LIKE  RC27M-WERKS,   "Plant
        MAKTX  TYPE  MAKTX,
       END OF IT_CRT_MAT.

*-- Material collection table : already created material
DATA : BEGIN OF IT_MATNR OCCURS 0,
        MATNR TYPE MATNR,
        WERKS TYPE WERKS_D,
        QMATV TYPE QMATV,
       END OF IT_MATNR.

*-- Material collection table : will be created material master

*-  internal table for already created material
DATA :  IT_MAT_MM01 LIKE IT_MATNR OCCURS 0 WITH HEADER LINE.


**-- Material collection for already created material
*DATA : IT_MATNR_NO LIKE IT_MATNR OCCURS 0 WITH HEADER LINE.

*-- Inspection type table
DATA : IT_ART LIKE ZSQM_QPART OCCURS 3 WITH HEADER LINE.

*-- Bapi return message table
DATA : IT_RETURN  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.



**- Structure for IT_QMPLN Modfy for Using At Event

*//Data(Global Fileds) ;(WA_)  == Local? ?? ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)
DATA : WA_REPID LIKE SYST-REPID,
       WA_DYNNR LIKE SYST-DYNNR.

*/ Result variables
DATA : WA_SUCCESS_CNT TYPE I,
       WA_FAILED_CNT  TYPE I,
       WA_GRP_CNT_ENTR TYPE I.

*// Structures
DATA : BEGIN OF ST_GRCNT,
           PLNNR TYPE PLNNR,   "GROUP
           PLNAL TYPE PLNAL,   "GROUP COUNTER.
       END OF ST_GRCNT.

**-- Constants
CONSTANTS : C_MARK  TYPE C VALUE 'X'.

*-- Inspection Type Constants
CONSTANTS : C_INSP_TYPE_ISIR    TYPE QPART VALUE '8910',
            C_INSP_TYPE_REGULAR TYPE QPART VALUE '8920',
            C_INSP_TYPE_MS      TYPE QPART VALUE '8930'.


DATA: WA_NAME  TYPE VRM_ID,
      IT_LIST  TYPE VRM_VALUES,
      WA_VALUE LIKE LINE OF IT_LIST.

*-- Work area : Global
DATA : WA_KEY_DATE TYPE DATUM VALUE '20040201'.

* BDC Tables
DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.

DATA   BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA   END OF IT_MSG.

DATA : IT_BDCMSG_COL  LIKE BDCMSGCOLL OCCURS 0 WITH HEADER LINE,
       IT_RET_MSG_COL LIKE BAPIRETURN OCCURS 0 WITH HEADER LINE.

*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : P_FILE_M   LIKE RLGRAP-FILENAME OBLIGATORY.
SELECTION-SCREEN ULINE.
PARAMETERS : P_MODE  TYPE TB_BDCMODE DEFAULT 'N' AS LISTBOX
                                         VISIBLE LENGTH 25..
SELECTION-SCREEN END OF BLOCK BLK .


AT SELECTION-SCREEN OUTPUT.

  SET TITLEBAR '1000'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE_M.
  PERFORM GET_SELECT_EXCEL_FILE  USING P_FILE_M.

START-OF-SELECTION.

  REFRESH : IT_CRT_MAT, IT_MATNR, IT_MAT_MM01.

*- Upload data to internal table from Excel file
  PERFORM F_EXCEL_UPLOAD  TABLES   IT_CRT_MAT
                          USING    P_FILE_M.

*-// Remarked by sllee : change AQM08: download mat. for conversion

**- fill Blank field
*  PERFORM MODIFY_IT_CRT_MAT.

*-// Remarked by sllee : End - 04/30/2004

END-OF-SELECTION.

  CHECK NOT IT_CRT_MAT[] IS INITIAL.

  SORT IT_CRT_MAT BY PLNNR PLNAL MATNR WERKS ASCENDING.

*-- Get material items for creation ('MM01')
  PERFORM GET_MATERIAL_FOR_CREAT.

*-- Create new Material and Inspection Setup : QM View.
  PERFORM CREATE_NEW_MATERIAL.

*-- Append new created material information
  PERFORM APPEND_CRTED_MAT_2_IT_MATNR.

*  PERFORM PROCESS_BDC_QP02.

*-- Change material QM_view if material don't have plant data(QM View)
*    - for already created material which have need another plant data
  PERFORM SET_INSP_QM_VIEW_FOR_PLANT.


*-- Assigned Material to inspection plan : 'QP02'.
  PERFORM ASSIGN_MAT_TO_PLAN.

*
  PERFORM WRITE_LOG_MESSAGE.


TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

END-OF-PAGE.

LOAD-OF-PROGRAM.
*-- fill inspection type table for ISIR function interface
  MOVE : C_INSP_TYPE_ISIR  TO IT_ART-ART. APPEND IT_ART.
  MOVE : C_INSP_TYPE_MS    TO IT_ART-ART. APPEND IT_ART.

*&------------------------------------------------------------------*
*&      Form  get_select_Excel_file
*&------------------------------------------------------------------*
FORM GET_SELECT_EXCEL_FILE   USING PW_FILE.
  WA_REPID = SY-REPID.
  WA_DYNNR = SY-DYNNR.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
       EXPORTING
            PROGRAM_NAME  = WA_REPID
            DYNPRO_NUMBER = WA_DYNNR
            FIELD_NAME    = ' '
            STATIC        = ' '
            MASK          = ' '
       CHANGING
            FILE_NAME     = PW_FILE
       EXCEPTIONS
            MASK_TOO_LONG = 1
            OTHERS        = 2.
  IF SY-SUBRC < 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " get_select_Excel_file
*&------------------------------------------------------------------*
*&      Form  F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
FORM F_EXCEL_UPLOAD  TABLES   P_TABLE
                      USING    P_FILENAME  LIKE RLGRAP-FILENAME.



  DATA : LT_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LW_INDEX LIKE SY-TABIX.
  DATA : LW_START_COL TYPE I VALUE '1',
         LW_START_ROW TYPE I VALUE '1',
         LW_END_COL   TYPE I VALUE '256',
         LW_END_ROW   TYPE I VALUE '65536'.
  FIELD-SYMBOLS : <LW_FS>.
  DATA : LW_FIELD_TYPE.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FILENAME
            I_BEGIN_COL             = LW_START_COL
            I_BEGIN_ROW             = LW_START_ROW
            I_END_COL               = LW_END_COL
            I_END_ROW               = LW_END_ROW
       TABLES
            INTERN                  = LT_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPS) WITH 'File Upload Failed !'(E10).
    STOP.
  ENDIF.


  CHECK NOT LT_ITAB[] IS INITIAL.

*-- Delete Header line: row from 1 to 2 : for Dev.

  DELETE LT_ITAB WHERE ROW LE 2.
*-- Delete Header line - end

  SORT LT_ITAB BY ROW COL.
  REFRESH P_TABLE.

  LOOP AT LT_ITAB.
    MOVE : LT_ITAB-COL TO LW_INDEX.
    ASSIGN COMPONENT LW_INDEX OF STRUCTURE P_TABLE TO <LW_FS>.

    DESCRIBE FIELD <LW_FS> TYPE LW_FIELD_TYPE.

    IF LW_FIELD_TYPE = 'D'.  "'MM/DD/YYYY"
      CONCATENATE LT_ITAB-VALUE+6(4)    "YEAR  (YYYY)
                  LT_ITAB-VALUE+0(2)    "MONTH (MM)
                  LT_ITAB-VALUE+3(2)    "DAY   (DD)
                              INTO <LW_FS>.
    ELSE.
      MOVE : LT_ITAB-VALUE TO <LW_FS>.
    ENDIF.

    AT END OF ROW.
      APPEND P_TABLE.
      CLEAR P_TABLE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " F_EXCEL_UPLOAD
*&-----------------------------------------------------------------*
*&      Form  MODIFY_IT_CRT_MAT
*&-----------------------------------------------------------------*
FORM MODIFY_IT_CRT_MAT.
*-- => ST_MATASK must same from PLNAL to SLWBEZ of IT_QMPLN Structure.
*-- => ST_OPT    must same from VORNR to QPPKTABS of IT_QMPLN Structure.
  DATA : LW_PLNNR TYPE PLNNR,
         LW_PLNAL TYPE PLNAL.
  DATA : LW_INDEX LIKE SY-TABIX.


  LOOP AT IT_CRT_MAT.
    LW_INDEX = SY-TABIX.

*-- Task Group Level
    IF     LW_PLNNR NE IT_CRT_MAT-PLNNR AND  "/NEW Group
      NOT IT_CRT_MAT-PLNNR IS INITIAL.
*-   Clear for Protect just one task/operation record of previous Group
      CLEAR : LW_PLNAL.
      LW_PLNNR = IT_CRT_MAT-PLNNR.
      LW_PLNAL = IT_CRT_MAT-PLNAL.
    ENDIF.

*-- Task Group counter level
    IF     LW_PLNAL NE IT_CRT_MAT-PLNAL AND  "/NEW Group Count Number
      NOT IT_CRT_MAT-PLNAL IS INITIAL.
*-   Clear for Protect just one operation record of previous Group count
      LW_PLNAL = IT_CRT_MAT-PLNAL.
    ENDIF.


    MOVE : LW_PLNNR TO IT_CRT_MAT-PLNNR.
    MOVE : LW_PLNAL TO IT_CRT_MAT-PLNAL.

    MODIFY IT_CRT_MAT INDEX LW_INDEX.

  ENDLOOP.


ENDFORM.                    " MODIFY_IT_CRT_MAT
*&-----------------------------------------------------------------*
*&      Form  WRITE_LOG_MESSAGE
*&-----------------------------------------------------------------*
FORM WRITE_LOG_MESSAGE.

  LOOP AT IT_RETURN WHERE TYPE = ' '.
    WRITE IT_RETURN-MESSAGE.
    NEW-LINE.
  ENDLOOP.

  ULINE.

  LOOP AT IT_RETURN.
    IF    IT_RETURN-TYPE IS INITIAL AND
      NOT IT_RETURN-MESSAGE IS INITIAL.
      WRITE '-------------------------------------------------------'.
      NEW-LINE.
    ENDIF.

    WRITE : IT_RETURN-TYPE,
            IT_RETURN-ID,
            IT_RETURN-NUMBER,
  AT (150)  IT_RETURN-MESSAGE,
*            IT_RETURN-LOG_NO,
*            IT_RETURN-LOG_MSG_NO,
*            IT_RETURN-MESSAGE_V1,
*            IT_RETURN-MESSAGE_V2,
*            IT_RETURN-MESSAGE_V3,
*            IT_RETURN-MESSAGE_V4,
            IT_RETURN-PARAMETER,
*            IT_RETURN-ROW,
            IT_RETURN-FIELD.
*            IT_RETURN-SYSTEM.
    NEW-LINE.
  ENDLOOP.

ENDFORM.                    " WRITE_LOG_MESSAGE
*&------------------------------------------------------------------*
*&      Form  WRITE_HEADER
*&------------------------------------------------------------------*
FORM WRITE_HEADER.
  WRITE : 'Inspection plan : ISIR Material uploading Log'(H01).
  ULINE.
  WRITE : 'Total of Inspection group : '(H02) ,
          WA_GRP_CNT_ENTR.
  NEW-LINE.
  WRITE : 'Success entries           : '(H03) ,
          WA_SUCCESS_CNT.
  NEW-LINE.
  WRITE : 'Failed entries            : '(H04),
          WA_FAILED_CNT.
  ULINE.
ENDFORM.                    " WRITE_HEADER
*&-----------------------------------------------------------------*
*&      Form  PROCESS_BDC_QP02
*&-----------------------------------------------------------------*
FORM PROCESS_BDC_QP02.
  DATA : LT_ART LIKE ZSQM_QPART OCCURS 3 WITH HEADER LINE.
  DATA : LT_MSG LIKE BDCMSGCOLL OCCURS 5 WITH HEADER LINE.

  DATA : LW_RETURN TYPE BAPIRETURN.

  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_MAT_ASSIGN LIKE IT_CRT_MAT.
  DATA : LW_DATE(10) TYPE C.

  DATA : LW_CNT(2) TYPE N  VALUE '02'. "/line count for Inspection type

  DATA : LW_GRP_COUNT(20) TYPE C,  "BDC Field name for group counter
         LW_MATERIAL(20) TYPE C,   "BDC Field name for material
         LW_PLANT(20)    TYPE C.   "BDC Field name for PLANT


  SORT IT_CRT_MAT  BY PLNNR PLNAL ASCENDING.

  WRITE WA_KEY_DATE TO LW_DATE.  "/Key date(Validity)

  LOOP AT IT_CRT_MAT.
    LW_INDEX = SY-TABIX.
    MOVE-CORRESPONDING : IT_CRT_MAT TO LW_MAT_ASSIGN.

*-   AT new Ext. Material Group(Inspection Plan Group)
    AT NEW PLNNR.
      REFRESH BDC_TAB.

      PERFORM DYNPRO  USING:
           'X'   'SAPLCPDI'          '8010',
           ' '   'BDC_OKCODE'        '/00',
           ' '   'RC27M-MATNR'       ' ',
           ' '   'RC271-PLNNR'       LW_MAT_ASSIGN-PLNNR,     "/Group
           ' '   'RC271-STTAG'       LW_DATE.     "/Key date

      PERFORM DYNPRO  USING:
          'X'   'SAPLCPDI'           '1200',
          ' '   'BDC_OKCODE'         '=MTUE'. "/Material Assignment

      PERFORM DYNPRO  USING:              "/CTRL+Page Down
        'X'   'SAPLCZDI'         '4010',
        ' '   'BDC_OKCODE'       '=P++'.

    ENDAT.

    PERFORM DYNPRO  USING:              "/Page Down
      'X'   'SAPLCZDI'         '4010',
      ' '   'BDC_OKCODE'       '=P+'.


*-   Get field name table control for Material assignment
    CONCATENATE 'MAPL-PLNAL('
                 LW_CNT
                 ')'       INTO LW_GRP_COUNT.
    CONCATENATE 'MAPL-MATNR('
                 LW_CNT
                 ')'       INTO LW_MATERIAL.
    CONCATENATE 'MAPL-WERKS('
                 LW_CNT
                 ')'       INTO LW_PLANT.


    PERFORM DYNPRO  USING:
       ' '   LW_GRP_COUNT      LW_MAT_ASSIGN-PLNAL,
       ' '   LW_MATERIAL       LW_MAT_ASSIGN-MATNR,
       ' '   LW_PLANT          LW_MAT_ASSIGN-WERKS.


    AT END OF PLNNR.
      PERFORM DYNPRO  USING:
              'X'   'SAPLCZDI'         '4010',
              ' '   'BDC_OKCODE'       '=BACK'.


      PERFORM DYNPRO  USING:
              'X'   'SAPLCPDI'         '1200',
              ' '   'BDC_OKCODE'       '=BU'.

*  -  Call transaction : 'QP02'.
      REFRESH LT_MSG.
      CALL TRANSACTION 'QP02'    USING BDC_TAB
                                 UPDATE 'S'
                                 MODE P_MODE
                               MESSAGES INTO LT_MSG.

      APPEND INITIAL LINE TO IT_MSG.
      APPEND LINES OF LT_MSG TO IT_MSG.

      READ TABLE LT_MSG WITH KEY MSGTYP = 'E'.
      IF SY-SUBRC = 0.
        ROLLBACK WORK.
        WA_FAILED_CNT = WA_FAILED_CNT + 1.
        MESSAGE W002(ZMPMQM) WITH 'BDC Error Founded!'(E10).
        EXIT.
      ELSE.
        READ TABLE LT_MSG WITH KEY MSGTYP = 'A'.
        IF SY-SUBRC = 0.
          ROLLBACK WORK.
          WA_FAILED_CNT = WA_FAILED_CNT + 1.
          MESSAGE W002(ZMPMQM) WITH 'BDC Error Founded!'(E10).
          EXIT.
        ELSE.
          READ TABLE LT_MSG WITH KEY MSGTYP = 'S'.
          IF SY-SUBRC = 0.
            COMMIT WORK.
            WA_SUCCESS_CNT = WA_SUCCESS_CNT + 1.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDAT.

  ENDLOOP.

ENDFORM.                    " PROCESS_BDC_QP02
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-PROGRAM,
           VALUE TO BDC_TAB-DYNPRO,
           'X'   TO BDC_TAB-DYNBEGIN.
    APPEND BDC_TAB.
  ELSE.
    CLEAR BDC_TAB.
    MOVE : NAME  TO BDC_TAB-FNAM,
           VALUE TO BDC_TAB-FVAL.
    APPEND BDC_TAB.
  ENDIF.
ENDFORM.                    "DYNPRO
*&-----------------------------------------------------------------*
*&      Form  ASSIGN_MAT_TO_PLAN
*&-----------------------------------------------------------------*
FORM ASSIGN_MAT_TO_PLAN.

  DATA : LT_MSG LIKE BDCMSGCOLL OCCURS 5 WITH HEADER LINE.

  DATA : LW_RETURN TYPE BAPIRETURN.

  DATA : LW_INDEX LIKE SY-TABIX.

  DATA : LW_MAT_ASSIGN LIKE IT_CRT_MAT.
  DATA : LW_DATE(10) TYPE C.

  DATA : LW_CNT(2) TYPE N  VALUE '02'. "/line count for Inspection type

  DATA : LW_GRP_COUNT(20) TYPE C,  "BDC Field name for group counter
         LW_MATERIAL(20) TYPE C,   "BDC Field name for material
         LW_PLANT(20)    TYPE C.   "BDC Field name for PLANT

  DATA : BEGIN OF LT_MAPL OCCURS 0,
           PLNNR    TYPE  PLNNR,
           PLNAL    TYPE  PLNAL,
           MATNR    TYPE  MATNR,
           WERKS    TYPE  WERKS_D,
         END OF LT_MAPL.


  SORT IT_CRT_MAT  BY PLNNR PLNAL ASCENDING.

  WRITE WA_KEY_DATE TO LW_DATE.  "/Key date(Validity)

  LOOP AT IT_CRT_MAT.
    LW_INDEX = SY-TABIX.
    MOVE-CORRESPONDING : IT_CRT_MAT TO LW_MAT_ASSIGN.

*-   AT new Ext. Material Group(Inspection Plan Group)
    AT NEW PLNNR.
      REFRESH BDC_TAB.
      REFRESH LT_MAPL.

*      - Get already assigned material to inspection plan for checking
      SELECT PLNNR PLNAL MATNR WERKS
         INTO CORRESPONDING FIELDS OF TABLE LT_MAPL
           FROM MAPL
             WHERE PLNTY = 'Q'
               AND PLNNR = IT_CRT_MAT-PLNNR
               AND DATUV = WA_KEY_DATE.


      PERFORM DYNPRO  USING:
           'X'   'SAPLCPDI'          '8010',
           ' '   'BDC_OKCODE'        '/00',
           ' '   'RC27M-MATNR'       ' ',
           ' '   'RC271-PLNNR'       LW_MAT_ASSIGN-PLNNR,     "/Group
           ' '   'RC271-STTAG'       LW_DATE.     "/Key date

      PERFORM DYNPRO  USING:
          'X'   'SAPLCPDI'           '1200',
          ' '   'BDC_OKCODE'         '=MTUE'. "/Material Assignment

      PERFORM DYNPRO  USING:              "/CTRL+Page Down
        'X'   'SAPLCZDI'         '4010',
        ' '   'BDC_OKCODE'       '=P++'.

      PERFORM DYNPRO  USING:              "/Page Down
        'X'   'SAPLCZDI'         '4010',
        ' '   'BDC_OKCODE'       '=P+'.

    ENDAT.

*-   Check already assigned to inspeciton plan.
    READ TABLE LT_MAPL  WITH KEY MATNR = LW_MAT_ASSIGN-MATNR
                                 WERKS = LW_MAT_ASSIGN-WERKS
                                 PLNAL = LW_MAT_ASSIGN-PLNAL
                                 PLNNR = LW_MAT_ASSIGN-PLNNR.

    IF SY-SUBRC NE 0.  "/ not assigned material and plant

      PERFORM DYNPRO  USING:              "/Page Down
         'X'   'SAPLCZDI'         '4010',
         ' '   'BDC_OKCODE'       '=P+'.

*-   Get field name table control for Material assignment
      CONCATENATE 'MAPL-PLNAL('
                   LW_CNT
                   ')'       INTO LW_GRP_COUNT.
      CONCATENATE 'MAPL-MATNR('
                   LW_CNT
                   ')'       INTO LW_MATERIAL.
      CONCATENATE 'MAPL-WERKS('
                   LW_CNT
                   ')'       INTO LW_PLANT.


      PERFORM DYNPRO  USING:
         ' '   LW_GRP_COUNT      LW_MAT_ASSIGN-PLNAL,
         ' '   LW_MATERIAL       LW_MAT_ASSIGN-MATNR,
         ' '   LW_PLANT          LW_MAT_ASSIGN-WERKS.

    ENDIF.


    AT END OF PLNNR.
      PERFORM DYNPRO  USING:
              'X'   'SAPLCZDI'         '4010',
              ' '   'BDC_OKCODE'       '=BACK'.

      PERFORM DYNPRO  USING:
              'X'   'SAPLCPDI'         '1200',
              ' '   'BDC_OKCODE'       '=BU'.

*  -  Call transaction : 'QP02'.
      REFRESH LT_MSG.
      CALL TRANSACTION 'QP02'    USING BDC_TAB
                                 UPDATE 'S'
                                 MODE P_MODE
                               MESSAGES INTO LT_MSG.

      APPEND INITIAL LINE TO IT_MSG.
      APPEND LINES OF LT_MSG TO IT_MSG.

      READ TABLE LT_MSG WITH KEY MSGTYP = 'E'.
      IF SY-SUBRC = 0.
        ROLLBACK WORK.
        WA_FAILED_CNT = WA_FAILED_CNT + 1.
        MESSAGE W002(ZMPMQM) WITH 'BDC Error Founded!'(E10).
        EXIT.
      ELSE.
        READ TABLE LT_MSG WITH KEY MSGTYP = 'A'.
        IF SY-SUBRC = 0.
          ROLLBACK WORK.
          WA_FAILED_CNT = WA_FAILED_CNT + 1.
          MESSAGE W002(ZMPMQM) WITH 'BDC Error Founded!'(E10).
          EXIT.
        ELSE.
          READ TABLE LT_MSG WITH KEY MSGTYP = 'S'.
          IF SY-SUBRC = 0.
            COMMIT WORK.
            WA_SUCCESS_CNT = WA_SUCCESS_CNT + 1.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDAT.

  ENDLOOP.

ENDFORM.                    " ASSIGN_MAT_TO_PLAN
*&---------------------------------------------------------------------*
*&      Form  GET_MATERIAL_FOR_CREAT
*&---------------------------------------------------------------------*
FORM GET_MATERIAL_FOR_CREAT.
*-- Get only material for created

  SELECT A~MATNR B~WERKS B~QMATV
    INTO CORRESPONDING FIELDS OF TABLE IT_MATNR
      FROM MARA AS A INNER JOIN MARC AS B
        ON A~MATNR = B~MATNR
        FOR ALL ENTRIES IN IT_CRT_MAT
      WHERE A~MATNR = IT_CRT_MAT-MATNR
        AND B~WERKS = IT_CRT_MAT-WERKS
*        AND A~MTART = 'QCIS'
        AND A~LVORM = ' '
        AND B~LVORM = ' '.


  LOOP AT IT_CRT_MAT.
*    - Check material which already created, and collect material.
    READ TABLE IT_MATNR  WITH KEY MATNR = IT_CRT_MAT-MATNR.

    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    CLEAR IT_MAT_MM01 .
    MOVE : IT_CRT_MAT-MATNR TO IT_MAT_MM01-MATNR.
    COLLECT IT_MAT_MM01.

  ENDLOOP.

ENDFORM.                    " GET_MATERIAL_FOR_CREAT
*&-------------------------------------------------------------------*
*&      Form  CREATE_NEW_MATERIAL
*&-------------------------------------------------------------------*
FORM CREATE_NEW_MATERIAL.

  DATA : LT_MSG LIKE BDCMSGCOLL OCCURS 5 WITH HEADER LINE.

  DATA : LW_RETURN TYPE BAPIRETURN.

  DATA : LW_CRT_MAT LIKE IT_CRT_MAT.
  DATA : LW_EXTWG TYPE EXTWG.

*-- create material and set Inspection using any one plant

  LOOP AT IT_MAT_MM01.
    CLEAR IT_CRT_MAT.

    READ TABLE IT_CRT_MAT WITH KEY MATNR = IT_MAT_MM01.

    IF SY-SUBRC NE 0. CONTINUE. ENDIF.

    MOVE : IT_CRT_MAT TO LW_CRT_MAT.
    MOVE : LW_CRT_MAT-PLNNR TO LW_EXTWG.

    CALL FUNCTION 'Z_FQM_CREATE_MAT_FOR_ISIR'
         EXPORTING
              I_MATNR           = LW_CRT_MAT-MATNR
              I_MAKTX           = LW_CRT_MAT-MAKTX
              I_WERKS           = LW_CRT_MAT-WERKS
              I_EXTWG           = LW_EXTWG
              I_BDCMODE         = P_MODE
         IMPORTING
              E_RETURN          = LW_RETURN
         TABLES
              T_ART             = IT_ART
              T_MSG             = LT_MSG
         EXCEPTIONS
              BDC_ERROR_FOUNDED = 1
              OTHERS            = 2.

*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

*-   Collect message from Function to internal message tables
    APPEND LINES OF LT_MSG TO IT_BDCMSG_COL.
    APPEND LW_RETURN       TO IT_RET_MSG_COL.



  ENDLOOP.

ENDFORM.                    " CREATE_NEW_MATERIAL
*&------------------------------------------------------------------*
*&      Form  APPEND_CRTED_MAT_2_IT_MATNR
*&------------------------------------------------------------------*
FORM APPEND_CRTED_MAT_2_IT_MATNR.

  SELECT A~MATNR B~WERKS B~QMATV
    APPENDING CORRESPONDING FIELDS OF TABLE IT_MATNR
      FROM MARA AS A INNER JOIN MARC AS B
        ON A~MATNR = B~MATNR
        FOR ALL ENTRIES IN IT_MAT_MM01
      WHERE A~MATNR = IT_MAT_MM01-MATNR
*        AND B~WERKS = IT_MAT_MM01-WERKS
*        AND A~MTART = 'QCIS'
        AND A~LVORM = ' '
        AND B~LVORM = ' '.

ENDFORM.                    " APPEND_CRTED_MAT_2_IT_MATNR
*&------------------------------------------------------------------*
*&      Form  SET_INSP_QM_VIEW_FOR_PLANT
*&------------------------------------------------------------------*
FORM SET_INSP_QM_VIEW_FOR_PLANT.

  DATA : LW_EXTWG TYPE EXTWG.

  DATA : LT_MSG LIKE BDCMSGCOLL OCCURS 5 WITH HEADER LINE.

  DATA : LW_RETURN TYPE BAPIRETURN.

  LOOP AT IT_CRT_MAT.
*    - Check material which already have plant data of material.
    CLEAR IT_MATNR.

    READ TABLE IT_MATNR  WITH KEY MATNR = IT_CRT_MAT-MATNR
                                  WERKS = IT_CRT_MAT-WERKS.

    IF SY-SUBRC = 0.
      CONTINUE.
    ENDIF.

    MOVE : IT_CRT_MAT TO LW_EXTWG.

*-- create another plant data for material, using 'MM02'.
    CALL FUNCTION 'Z_FQM_SET_MAT_EXTWG_N_QM_VIEW'
         EXPORTING
              I_MATNR           = IT_CRT_MAT-MATNR
              I_WERKS           = IT_CRT_MAT-WERKS
              I_EXTWG           = LW_EXTWG
              I_BDCMODE         = P_MODE
         IMPORTING
              E_RETURN          = LW_RETURN
         TABLES
              T_ART             = IT_ART
              T_MSG             = LT_MSG
         EXCEPTIONS
              BDC_ERROR_FOUNDED = 1
              OTHERS            = 2.

*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.

*-   Collect message from Function to internal message tables
    APPEND LINES OF LT_MSG TO IT_BDCMSG_COL.
    APPEND LW_RETURN       TO IT_RET_MSG_COL.

  ENDLOOP.

ENDFORM.                    " SET_INSP_QM_VIEW_FOR_PLANT
