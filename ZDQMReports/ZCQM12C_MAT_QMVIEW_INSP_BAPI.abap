************************************************************************
* Program Name      : ZCQM12C_MAT_QMVIEW_INSP_BAPI
* Author            : SeungLyong, Lee
* Creation Date     : 2003.08.08.
* Specifications By : SeungLyong, Lee
* Pattern           : 2.Conversion - 2.3 Call Transaction
* Development Request No :
* Addl Documentation:
* Description       : Material Master - QM View: Inspection type
*                     Upload
*   - Update QM View of Material master inspection type and activate it
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


REPORT  ZCQM12C_MAT_QMVIEW_INSP_BAPI NO STANDARD PAGE HEADING
                                          LINE-SIZE 250.


*TYPE-POOLS: VRM.

* BDC Tables
DATA : BEGIN OF BDC_TAB OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA : END OF BDC_TAB.
DATA   BEGIN OF IT_MSG OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA   END OF IT_MSG.


*-- Declaration Tables
TABLES :  MARA, QMAT, MARC.

*-- Interface table format for Excel file of Material Inspection type
DATA: BEGIN OF IT_QMAT OCCURS 0,
        MATNR   TYPE   MATNR,    "/Material
        WERKS   TYPE   WERKS_D,  "/Plant
        EXTWG   TYPE   EXTWG,    "/External Mat. group
        ART     TYPE   QPART,    "/Inspection type
      END OF IT_QMAT.

DATA : BEGIN OF IT_MAT_WEIGHT OCCURS 0,  "/for check weight.
        MATNR   TYPE   MATNR,    "/Material
        BRGEW   TYPE   BRGEW,    "/Gross weight
        NTGEW   TYPE   NTGEW,    "/Net weight
       END OF IT_MAT_WEIGHT.

*-- Bapi return message table
DATA : IT_RETURN  LIKE BAPIRET2 OCCURS 0 WITH HEADER LINE.

**- Structure for IT_QMAT Modfy for Using At Event


*//Data(Global Fileds) ;(WA_)  == Local? ?? ; (LO_)
*                 Flag ;(FL_), Counter;(CT_), Temp;(TMP_)
DATA : WA_REPID LIKE SYST-REPID,
       WA_DYNNR LIKE SYST-DYNNR.

*/ Result variables
DATA : WA_SUCCESS_CNT TYPE I,
       WA_FAILED_CNT  TYPE I,
       WA_TOTAL_ENTR TYPE I.

*// Structures


**-- Constants
CONSTANTS : C_MARK  TYPE C VALUE 'X'.

**-- List box variables
*DATA: WA_NAME  TYPE VRM_ID,
*      IT_LIST  TYPE VRM_VALUES,
*      WA_VALUE LIKE LINE OF IT_LIST.

*#### Selection Screen ####
SELECTION-SCREEN BEGIN OF BLOCK BLK WITH FRAME  TITLE TEXT-T01.
PARAMETERS : P_FILE   LIKE RLGRAP-FILENAME   OBLIGATORY.
SELECTION-SCREEN ULINE.
*PARAMETERS : P_TEST  TYPE BAPIFLAG DEFAULT 'X' AS LISTBOX "/Simulation
*                                             VISIBLE LENGTH 14.
PARAMETERS : PA_MODE   TYPE TB_BDCMODE DEFAULT 'N' AS LISTBOX
                                         VISIBLE LENGTH 25.
SELECTION-SCREEN END OF BLOCK BLK .

AT SELECTION-SCREEN OUTPUT.

  SET TITLEBAR '1000'.

*  WA_NAME = 'P_TEST'.
*  WA_VALUE-KEY = 'X'. WA_VALUE-TEXT = 'Simulation'.
*  APPEND WA_VALUE TO IT_LIST.
*  WA_VALUE-KEY = ' '. WA_VALUE-TEXT = 'Write'.
*  APPEND WA_VALUE TO IT_LIST.
*  CALL FUNCTION 'VRM_SET_VALUES'
*       EXPORTING
*            ID     = WA_NAME
*            VALUES = IT_LIST.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM GET_SELECT_EXCEL_FILE  USING P_FILE.


START-OF-SELECTION.

  PERFORM F_EXCEL_UPLOAD  TABLES   IT_QMAT
                          USING    P_FILE.
  IF IT_QMAT[] IS INITIAL.
    MESSAGE E000(ZMQM)
            WITH 'Entries not founded. Please check File'(E15).
    EXIT.
  ENDIF.
* fill blank fields
  PERFORM MODIFY_IT_QMAT.


END-OF-SELECTION.

*///--Test BAPI Function  -- Start
*  PERFORM MAT_INSP_CONTROL_UPDATE. "/execute BAPI Function
*
*  PERFORM WRITE_LOG_MESSAGE.
*///--Test BAPI Function  -- End

**>>>>>>>>>>>>> Add for Performance by sllee 04/08/2004 - Start
*-- Get material weight to IT_MAT_WEIGHT
  PERFORM GET_MATERIAL_WEIGHT_INFO.
**>>>>>>>>>>>>> Add for Performance by sllee 04/08/2004 - End

  PERFORM PROCESS_BDC.

  PERFORM WRITE_BDC_LOD_MESSAGE.

TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

END-OF-PAGE.

*&------------------------------------------------------------------*
*&      Form  F_EXCEL_UPLOAD
*&------------------------------------------------------------------*
FORM F_EXCEL_UPLOAD  TABLES   P_TABLE
                      USING    P_FILENAME  LIKE RLGRAP-FILENAME.



  DATA : LO_ITAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.
  DATA : LO_INDEX LIKE SY-TABIX.
  DATA : LO_START_COL TYPE I VALUE '1',
         LO_START_ROW TYPE I VALUE '1',
         LO_END_COL   TYPE I VALUE '256',
         LO_END_ROW   TYPE I VALUE '65536'.
  FIELD-SYMBOLS : <LO_FS>.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
       EXPORTING
            FILENAME                = P_FILENAME
            I_BEGIN_COL             = LO_START_COL
            I_BEGIN_ROW             = LO_START_ROW
            I_END_COL               = LO_END_COL
            I_END_ROW               = LO_END_ROW
       TABLES
            INTERN                  = LO_ITAB
       EXCEPTIONS
            INCONSISTENT_PARAMETERS = 1
            UPLOAD_OLE              = 2
            OTHERS                  = 3.


  IF SY-SUBRC NE 0.
    MESSAGE E000(ZMPS) WITH 'File Upload Failed !'(E10).
    STOP.
  ENDIF.


  CHECK NOT LO_ITAB[] IS INITIAL.

*-- Delete Header line: row from 1 to 2
  DELETE LO_ITAB WHERE ROW LE 2.


  SORT LO_ITAB BY ROW COL.
  REFRESH P_TABLE.

  LOOP AT LO_ITAB.
    MOVE : LO_ITAB-COL TO LO_INDEX.
    ASSIGN COMPONENT LO_INDEX OF STRUCTURE P_TABLE TO <LO_FS>.
    MOVE : LO_ITAB-VALUE TO <LO_FS>.
    AT END OF ROW.
      APPEND P_TABLE.
      CLEAR P_TABLE.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " F_EXCEL_UPLOAD
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
*&      Form  CHECK_EXIST_MATNR_IN_PLANT
*&------------------------------------------------------------------*

FORM CHECK_EXIST_MATNR_IN_PLANT USING    P_MATNR
                                         P_WERKS.
  DATA : LWA_MATNR TYPE MATNR.

  TRANSLATE P_MATNR TO UPPER CASE.

*CONVERSION_EXIT_ALPHA
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
       EXPORTING
            INPUT  = P_MATNR
       IMPORTING
            OUTPUT = P_MATNR.

  SELECT SINGLE *
     FROM MARC
       WHERE MATNR = P_MATNR
         AND WERKS = P_WERKS.

  IF SY-SUBRC NE 0.
    STOP.
    MESSAGE E000(ZMQM) WITH TEXT-E01 P_MATNR TEXT-E02 P_WERKS.
*                                          "/No Exist  MATNR in Plant
  ENDIF.

ENDFORM.                    " CHECK_EXIST_MATNR_IN_PLANT
*&-----------------------------------------------------------------*
*&      FORM MODIFY_IT_QMAT
*&-----------------------------------------------------------------*
FORM MODIFY_IT_QMAT.

  DATA: BEGIN OF LW_QMAT OCCURS 0,
        MATNR   TYPE   MATNR,    "/Material
        WERKS   TYPE   WERKS_D,  "/Plant
      END OF LW_QMAT.

  DATA : LW_INDEX LIKE SY-TABIX.

  LOOP AT IT_QMAT.
    LW_INDEX = SY-TABIX.

*-- Task Group Level
    IF     LW_QMAT-MATNR NE IT_QMAT-MATNR AND  "/NEW Material
      NOT  IT_QMAT-MATNR IS INITIAL.
*-   Clear for Protect just one material and plant

      MOVE-CORRESPONDING : IT_QMAT TO LW_QMAT.

    ENDIF.

    MOVE-CORRESPONDING LW_QMAT TO IT_QMAT.
    MODIFY IT_QMAT INDEX LW_INDEX.

  ENDLOOP.


ENDFORM.                    "MODIFY_IT_QMAT
*&------------------------------------------------------------------*
*&      Form  MAT_INSP_CONTROL_UPDATE
*&------------------------------------------------------------------*
FORM MAT_INSP_CONTROL_UPDATE.
  DATA : LW_QMAT LIKE IT_QMAT.

*//-- define BAPI interfaces
*<Import> parameters

*<Export>

*<Tables>
  DATA: LT_RETURN     LIKE   BAPIRET2        "/Return Parameter for BAPI
                         OCCURS 0 WITH HEADER LINE,
        LT_INSP_CTRL  LIKE   BAPI1001004_QMAT "/Inspection Setup Data
                         OCCURS 0 WITH HEADER LINE.

  LOOP AT IT_QMAT.
    CLEAR LW_QMAT.
*    Data Back up for at event.
    MOVE-CORRESPONDING IT_QMAT TO LW_QMAT.


    AT NEW MATNR.
      CLEAR   : LT_RETURN, LT_INSP_CTRL.
      REFRESH : LT_RETURN, LT_INSP_CTRL.
    ENDAT.

    MOVE :
      LW_QMAT-MATNR  TO LT_INSP_CTRL-MATERIAL,  "/Material
      LW_QMAT-WERKS  TO LT_INSP_CTRL-PLANT,     "/Plant
      LW_QMAT-ART    TO LT_INSP_CTRL-INSPTYPE,  "/Inspection Type
      C_MARK         TO LT_INSP_CTRL-IND_INSPTYPE_MAT_ACTIVE. "/Active

    MOVE :
*- Procedure for Calc. Quality Score :'06'-From Usage Decision code
      '06'   TO LT_INSP_CTRL-QUAL_SCORE_PROCEDURE.

    MOVE C_MARK
           TO : LT_INSP_CTRL-IND_SINGLE_UNITS_POSSIBLE,
                LT_INSP_CTRL-IND_INSP_WITH_TSK_LIST,
                LT_INSP_CTRL-IND_AUTO_ASSIGN,
                LT_INSP_CTRL-IND_INSP_BY_CHARAC,
                LT_INSP_CTRL-IND_SKIPS_ALLOWED,
                LT_INSP_CTRL-IND_AUTOMATIC_UD.



*    MOVE : '004'   TO LT_INSP_CTRL-FUNCTION. "/Message Function
**//-- Message Functions
* 003	Delete: Message contains objects to be deleted
* 004	Change: Message contains changes
* 005	Replace: This message replaces previous messages
* 009	Original: First message for process
* 023	Wait/Adjust: Data should not be imported
* 018	Resend

    APPEND LT_INSP_CTRL.


    AT END OF MATNR.

***/ Execute BAPI : 'BAPI_MATINSPCTRL_SAVEREPLICA'

      CALL FUNCTION 'BAPI_MATINSPCTRL_SAVEREPLICA'
           TABLES
                RETURN         = LT_RETURN
                INSPECTIONCTRL = LT_INSP_CTRL.

      CLEAR IT_RETURN.

**      insert division message for each inspection plan
      CONCATENATE LW_QMAT-MATNR
                  ':'
                  LW_QMAT-WERKS
                     INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.


      READ TABLE LT_RETURN WITH KEY TYPE = 'E'.

      IF SY-SUBRC = 0.  "/Error
        ROLLBACK WORK.
        CONCATENATE IT_RETURN-MESSAGE
                    '=> Failed'
                       INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
        WA_FAILED_CNT = WA_FAILED_CNT + 1.

      ELSE.
        READ TABLE LT_RETURN WITH KEY TYPE = 'A'.
        IF SY-SUBRC = 0.
          ROLLBACK WORK.
          CONCATENATE IT_RETURN-MESSAGE
                      '=> Failed'
                         INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
          WA_FAILED_CNT = WA_FAILED_CNT + 1.

        ELSE.  "Success Check.
          CLEAR LT_RETURN.
          READ TABLE LT_RETURN INDEX 1.

          IF ( LT_RETURN-TYPE IS INITIAL OR          "//Success
               LT_RETURN-TYPE = 'S'           ) .
            CONCATENATE IT_RETURN-MESSAGE
                        '=> Success'
                           INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.

            WA_SUCCESS_CNT = WA_SUCCESS_CNT + 1.
          ELSE.
            ROLLBACK WORK.
            CONCATENATE IT_RETURN-MESSAGE
                        '=> Failed'
                           INTO  IT_RETURN-MESSAGE SEPARATED BY SPACE.
            WA_FAILED_CNT = WA_FAILED_CNT + 1.
          ENDIF.

        ENDIF.

      ENDIF.

      APPEND IT_RETURN.
      APPEND LINES OF LT_RETURN TO IT_RETURN.
      APPEND INITIAL LINE TO  IT_RETURN.


    ENDAT.

  ENDLOOP.

ENDFORM.                    " MAT_INSP_CONTROL_UPDATE
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
  WRITE :
    'Material Master QM View : Inspection type uploading Log'(H01).
  ULINE.
  WRITE : 'Total of Material         : '(H02) ,
          WA_TOTAL_ENTR.
  NEW-LINE.
  WRITE : 'Success entries           : '(H03) ,
          WA_SUCCESS_CNT.
  NEW-LINE.
  WRITE : 'Failed entries            : '(H04),
          WA_FAILED_CNT.
  ULINE.
ENDFORM.                    " WRITE_HEADER
*&------------------------------------------------------------------*
*&      Form  process_bdc
*&------------------------------------------------------------------*
FORM PROCESS_BDC.
  DATA : LW_QMAT LIKE IT_QMAT.

  DATA : LW_ART(20) TYPE C,
         LW_AKTIV(20) TYPE C.
  DATA : LW_CNT(2) TYPE N.

  DATA : LT_MSG LIKE IT_MSG OCCURS 0 WITH HEADER LINE.

  REFRESH IT_MSG.

  LOOP AT IT_QMAT.
    CLEAR LW_QMAT.
*    Data Back up for at event.
    MOVE-CORRESPONDING IT_QMAT TO LW_QMAT.


    AT NEW MATNR.
      CLEAR LW_CNT.
      REFRESH BDC_TAB.
**-- Check up existence of Material in Plant
*      PERFORM CHECK_EXIST_MATNR_IN_PLANT USING LW_QMAT-MATNR
*                                               LW_QMAT-WERKS.
*      IF SY-SUBRC NE 0.
*        CONTINUE.
*      ENDIF.

      PERFORM DYNPRO  USING:
        'X'   'SAPLMGMM'          '0060',
        ' '   'BDC_OKCODE'        '/00',
        ' '   'RMMG1-MATNR'       LW_QMAT-MATNR.

      PERFORM DYNPRO  USING:
        'X'   'SAPLMGMM'                  '0070',
        ' '   'BDC_OKCODE'                '=ENTR',
        ' '   'MSICHTAUSW-KZSEL(01)'       C_MARK, "/Basic view 1
        ' '   'MSICHTAUSW-KZSEL(14)'       C_MARK. "/QM View

      PERFORM DYNPRO  USING:
        'X'   'SAPLMGMM'         '0080',
        ' '   'BDC_OKCODE'       '=ENTR',
        ' '   'RMMG1-WERKS'       LW_QMAT-WERKS.
*- Basic view 1
**>>>>>>>>>>>>> Modify for Performance by sllee 04/08/2004 - Start
************************************************************
**temporary by wskim 20040402.
**Check Gross weight and Net weight
*      SELECT SINGLE * FROM MARA WHERE MATNR EQ LW_QMAT-MATNR.
*      IF MARA-BRGEW  < MARA-NTGEW.
*        PERFORM DYNPRO  USING:
*         'X'   'SAPLMGMM'         '5004',
*         ' '   'BDC_OKCODE'       '/00'.
*      ENDIF.
***********************************************************

**  - Check Gross weight and Net weight
*  - IT_MAT_WEIGHT already have weight information of material
      CLEAR IT_MAT_WEIGHT.
      READ TABLE IT_MAT_WEIGHT WITH KEY MATNR = LW_QMAT-MATNR.

      IF IT_MAT_WEIGHT-BRGEW < IT_MAT_WEIGHT-NTGEW.
        PERFORM DYNPRO  USING:
         'X'   'SAPLMGMM'         '5004',
         ' '   'BDC_OKCODE'       '/00'.
      ENDIF.
**>>>>>>>>>>>>> Modify for Performance by sllee 04/08/2004 - End

      PERFORM DYNPRO  USING:
        'X'   'SAPLMGMM'         '5004',
        ' '   'BDC_OKCODE'       '=SP23',
        ' '   'MARA-EXTWG'       LW_QMAT-EXTWG.


*- Quality Management view
      PERFORM DYNPRO  USING:
        'X'   'SAPLMGMM'         '5000',
        ' '   'BDC_OKCODE'       '=PB01'.

*- Inspection type assignment
      PERFORM DYNPRO  USING:
        'X'   'SAPLQPLS'         '0100',
        ' '   'BDC_OKCODE'       '=NEU'.


      PERFORM DYNPRO  USING:
        'X'   'SAPLQPLS'         '0100',
        ' '   'BDC_OKCODE'       '=WEIT'.

    ENDAT.

    LW_CNT = LW_CNT + 1.
*     DATA : LW_ART(20) TYPE C,
*         LW_AKTIV(20) TYPE C.
    CONCATENATE 'RMQAM-ART('
                 LW_CNT
                 ')'       INTO LW_ART.
    CONCATENATE 'RMQAM-AKTIV('
                 LW_CNT
                 ')'       INTO LW_AKTIV.


    PERFORM DYNPRO  USING:
      ' '   LW_ART      LW_QMAT-ART,
      ' '   LW_AKTIV    C_MARK.


    AT END OF MATNR.

      WA_TOTAL_ENTR = WA_TOTAL_ENTR + 1.

      PERFORM DYNPRO  USING:
        'X'   'SAPLMGMM'         '5000',
        ' '   'BDC_OKCODE'       '=BU'.

      SET PARAMETER ID 'MAT' FIELD LW_QMAT-MATNR.

      REFRESH LT_MSG.
      CALL TRANSACTION 'MM02'    USING BDC_TAB
                                 UPDATE 'S'
                                 MODE PA_MODE
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

ENDFORM.                    " process_bdc


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
*&------------------------------------------------------------------*
*&      Form  WRITE_BDC_LOD_MESSAGE
*&------------------------------------------------------------------*
FORM WRITE_BDC_LOD_MESSAGE.

  LOOP AT IT_MSG WHERE MSGTYP = ' '.
    WRITE IT_MSG.
    NEW-LINE.
  ENDLOOP.

  ULINE.

  LOOP AT IT_MSG.
    IF    IT_MSG-MSGTYP IS INITIAL .
      WRITE '-------------------------------------------------------'.
      NEW-LINE.
    ENDIF.

    WRITE : IT_MSG.
    NEW-LINE.
  ENDLOOP.

ENDFORM.                    " WRITE_BDC_LOD_MESSAGE
*&-----------------------------------------------------------------*
*&      Form  GET_MATERIAL_WEIGHT_INFO
*&-----------------------------------------------------------------*
FORM GET_MATERIAL_WEIGHT_INFO.

**>>>>>>>>>>>>> Modify for Performance by sllee 04/08/2004 - Start
  SELECT MATNR BRGEW NTGEW
    INTO CORRESPONDING FIELDS OF TABLE IT_MAT_WEIGHT
      FROM MARA
        FOR ALL ENTRIES IN IT_QMAT
       WHERE MATNR = IT_QMAT-MATNR.

**>>>>>>>>>>>>> Modify for Performance by sllee 04/08/2004 - End
ENDFORM.                    " GET_MATERIAL_WEIGHT_INFO
