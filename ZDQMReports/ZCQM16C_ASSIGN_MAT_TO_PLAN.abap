************************************************************************
* Program Name      : ZCQM16C_ASSIGN_MAT_TO_PLAN
* Author            : SeungLyong, Lee
* Creation Date     : 2004.03.20.
* Specifications By : SeungLyong, Lee
* Pattern           : 2.Conversion - 2.3 Call Transaction
* Development Request No :
* Addl Documentation:
* Description       :
*   - Assign material to inspection plan
*   - Using T-Code 'QP02' : BDC
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

REPORT  ZCQM16C_ASSIGN_MAT_TO_PLAN  NO STANDARD PAGE HEADING
                                          LINE-SIZE 250.


TYPE-POOLS: VRM.

*-- Declaration Tables
TABLES : MAPL, PLKO, PLPO, PLMK, MARA, MARC.

*-- material assignment table
DATA : BEGIN OF IT_MAT_ASSIGN OCCURS 0,
        PLNNR  LIKE  RC271-PLNNR,   "Key for task list group =>CLEAR' '
        PLNAL  TYPE  PLNAL,         "Group Counters
        MATNR  TYPE  MATNR,         "/MATERIAL
        WERKS  LIKE  RC27M-WERKS,   "Plant
       END OF IT_MAT_ASSIGN.
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

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_MAT_ASSIGN
                          USING    P_FILE_M.

  PERFORM MODIFY_IT_MAT_ASSIGN.

END-OF-SELECTION.


  PERFORM PROCESS_BDC_QP02.

*
  PERFORM WRITE_LOG_MESSAGE.


TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

END-OF-PAGE.

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
*&      Form  MODIFY_IT_MAT_ASSIGN
*&-----------------------------------------------------------------*
FORM MODIFY_IT_MAT_ASSIGN.
*-- => ST_MATASK must same from PLNAL to SLWBEZ of IT_QMPLN Structure.
*-- => ST_OPT    must same from VORNR to QPPKTABS of IT_QMPLN Structure.
  DATA : LW_PLNNR TYPE PLNNR,
         LW_PLNAL TYPE PLNAL.
  DATA : LW_INDEX LIKE SY-TABIX.

  LOOP AT IT_MAT_ASSIGN.
    LW_INDEX = SY-TABIX.

*-- Task Group Level
    IF     LW_PLNNR NE IT_MAT_ASSIGN-PLNNR AND  "/NEW Group
      NOT IT_MAT_ASSIGN-PLNNR IS INITIAL.
*-   Clear for Protect just one task/operation record of previous Group
      CLEAR : LW_PLNAL.
      LW_PLNNR = IT_MAT_ASSIGN-PLNNR.
      LW_PLNAL = IT_MAT_ASSIGN-PLNAL.
    ENDIF.

*-- Task Group counter level
    IF     LW_PLNAL NE IT_MAT_ASSIGN-PLNAL AND  "/NEW Group Count Number
      NOT IT_MAT_ASSIGN-PLNAL IS INITIAL.
*-   Clear for Protect just one operation record of previous Group count
      LW_PLNAL = IT_MAT_ASSIGN-PLNAL.
    ENDIF.


    MOVE : LW_PLNNR TO IT_MAT_ASSIGN-PLNNR.
    MOVE : LW_PLNAL TO IT_MAT_ASSIGN-PLNAL.

    MODIFY IT_MAT_ASSIGN INDEX LW_INDEX.

  ENDLOOP.


ENDFORM.                    " MODIFY_IT_MAT_ASSIGN
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
  WRITE : 'Inspection plan : Material Assignment uploading Log'(H01).
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

  DATA : LW_MAT_ASSIGN LIKE IT_MAT_ASSIGN.
  DATA : LW_DATE(10) TYPE C.

  DATA : LW_CNT(2) TYPE N  VALUE '02'. "/line count for Inspection type

  DATA : LW_GRP_COUNT(20) TYPE C,  "BDC Field name for group counter
         LW_MATERIAL(20) TYPE C,   "BDC Field name for material
         LW_PLANT(20)    TYPE C.   "BDC Field name for PLANT


  SORT IT_MAT_ASSIGN  BY PLNNR PLNAL ASCENDING.

  WRITE WA_KEY_DATE TO LW_DATE.  "/Key date(Validity)

  LOOP AT IT_MAT_ASSIGN.
    LW_INDEX = SY-TABIX.
    MOVE-CORRESPONDING : IT_MAT_ASSIGN TO LW_MAT_ASSIGN.

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


    PERFORM DYNPRO  USING:              "/Page Down
      'X'   'SAPLCZDI'         '4010',
      ' '   'BDC_OKCODE'       '=P+'.

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
