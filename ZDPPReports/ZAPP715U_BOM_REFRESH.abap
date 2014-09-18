************************************************************************
* Program Name      : ZAPP715U_BOM_REFRESH
* Author            : JongOh, Kim
* Creation Date     : 2003.12.26
* Specifications By : JongOh, Kim
* Pattern           : 2.1
* Development Request No : UD1K904945
* Addl Documentation:
* Description       : Planned Order BOM Refresh
*
* Modification Logs
* Date        Developer    RequestNo    Description
*
*
************************************************************************
REPORT ZAPP715U_BOM_REFRESH  NO STANDARD PAGE HEADING
                                   LINE-SIZE 1023
                                   MESSAGE-ID ZMPP.
*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : PLAF,      "Planned order
         CABN,      "Characteristic
         AUSP.      "Characteristic Values

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : IT_PLAF   LIKE TABLE OF PLAF WITH HEADER LINE.

DATA : BEGIN OF IT_ERROR OCCURS 0,
         PLNUM   LIKE PLAF-PLNUM,
         ZMSG    LIKE ZSCA_IF_TIME_STAMP-ZMSG.
DATA : END OF IT_ERROR.
*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA: WA_ATINN      TYPE  CABN-ATINN,
      WA_ATFOR      TYPE  CABN-ATFOR.
DATA: WA_PSTTR_FLG.
DATA: WA_OBJEK      TYPE  AUSP-OBJEK,    "Vehicle Master
      WA_ATWRT      TYPE  AUSP-ATWRT,    "P_RP_STATUS
      WA_RPOINT     TYPE  N.             "P_RP_STATUS No

DATA: WA_PLAF_IX    TYPE  SY-TABIX,
      WA_ERROR_IX   TYPE  SY-TABIX.
*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION (BDC DATA)
*----------------------------------------------------------------------*
DATA : IT_BDCDATA     LIKE TABLE OF BDCDATA  WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION(BDC)
*----------------------------------------------------------------------*
*------> BDC MODE ('A' DISPLAY SCREEN, 'N' NO DISPLAY)
DATA: WA_OPTION_DS   LIKE CTU_PARAMS.   "BDC OPTION

*----------------------------------------------------------------------*
*  CONSTANTS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_MARK        VALUE 'X',
           C_MODE        VALUE 'N',
           C_RP_STATUS   TYPE  CABN-ATNAM  VALUE 'P_RP_STATUS'.
*----------------------------------------------------------------------*
*  SELECTION SCREEN DECLARATION
*----------------------------------------------------------------------*
SELECT-OPTIONS : S_PSTTR FOR PLAF-PSTTR,
                 S_PLNUM FOR PLAF-PLNUM.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  PERFORM EXCUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
  CALL FUNCTION 'ENQUEUE_EZPP_COMPPARTVIN'
       EXPORTING
            MODE_ZTPP_COMPPARTVIN = 'E'
            MANDT                 = SY-MANDT.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXCUTE_PROCESS.
  CLEAR : WA_ATINN, WA_ATFOR.

  WA_PSTTR_FLG = 'Y'.
  SET PARAMETER ID 'ZBAT' FIELD WA_PSTTR_FLG.

  PERFORM WRITE_START.

*---> SET BDC MODE
  PERFORM SET_MODE.
*----> SELECT PLAF In condition
  PERFORM SELECT_PLAF.

*----> READ Characteristic number of Planned Order
  PERFORM SELECT_CABN USING 'P_PLAN_ORDER'
                      CHANGING WA_ATINN WA_ATFOR.

*----> Planned Order BOM Refresh
  PERFORM BOM_REFRESH.

ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  WRITE_START
*&---------------------------------------------------------------------*
FORM WRITE_START.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ 'Planned Order BOM Refresh'.
  WRITE :/ '********** BEGIN OF PROCESS ***********'.
  SKIP 2.
ENDFORM.                    " WRITE_START
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF
*&---------------------------------------------------------------------*
FORM SELECT_PLAF.
  CLEAR : IT_PLAF, IT_PLAF[].
  SELECT *
         INTO TABLE IT_PLAF
         FROM PLAF
         WHERE PAART EQ 'PE'         "Order Type
           AND BESKZ EQ 'E'          "Procurement Type
           AND SOBES EQ 'E'          "Special procurement type
           AND KNTTP EQ 'M'          "Acct.assgt.category
           AND PSTTR IN S_PSTTR      "Planned Start date
           AND PLNUM IN S_PLNUM.     "Planned Order No

  DESCRIBE TABLE IT_PLAF  LINES WA_PLAF_IX.
  SORT IT_PLAF BY PLWRK MATNR STALT STLAN VERID PSTTR.

ENDFORM.                    " SELECT_PLAF
*&---------------------------------------------------------------------*
*&      Form  SELECT_CABN
*&---------------------------------------------------------------------*
FORM SELECT_CABN USING P_ATNAM TYPE CABN-ATNAM
                 CHANGING P_ATINN TYPE CABN-ATINN
                          P_ATFOR TYPE CABN-ATFOR.
  SELECT SINGLE ATINN
                ATFOR
              INTO (P_ATINN, P_ATFOR)
              FROM CABN
              WHERE ATNAM EQ P_ATNAM.

ENDFORM.                    " SELECT_CABN
*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJEK
*&---------------------------------------------------------------------*
FORM SELECT_OBJEK USING P_ATINN P_KLART P_ATWRT P_ATFOR
                 CHANGING P_OBJEK.

  DATA : L_NUM(10)  TYPE  N,
         L_INT      TYPE  I,
         L_ATFLV    TYPE  AUSP-ATFLV.

  IF P_ATFOR EQ 'CHAR'.
    SELECT SINGLE OBJEK
                 INTO P_OBJEK
                 FROM AUSP
                 WHERE ATINN EQ P_ATINN
                   AND KLART EQ P_KLART
                   AND ATWRT EQ P_ATWRT.
  ELSE.
    L_NUM = P_ATWRT.
    L_INT = L_NUM.
    L_ATFLV = L_INT.
    SELECT SINGLE OBJEK
                 INTO P_OBJEK
                 FROM AUSP
                 WHERE ATINN EQ P_ATINN
                   AND KLART EQ P_KLART
                   AND ATFLV EQ L_ATFLV.
  ENDIF.


ENDFORM.                    " SELECT_OBJEK
*&---------------------------------------------------------------------*
*&      Form  SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM SELECT_CLASSIFICATION USING P_ATNAM
                           CHANGING P_ATWRT.
  DATA : L_ATINN   TYPE  CABN-ATINN,
         L_ATFOR   TYPE  CABN-ATFOR.

  PERFORM SELECT_CABN USING P_ATNAM
                    CHANGING L_ATINN L_ATFOR.
  PERFORM SELECT_AUSP USING WA_OBJEK L_ATINN '002' L_ATFOR
                    CHANGING P_ATWRT.
ENDFORM.                    " SELECT_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_AUSP
*&---------------------------------------------------------------------*
FORM SELECT_AUSP USING    P_OBJEK P_ATINN P_KLART P_ATFOR
                 CHANGING P_ATWRT.
  DATA : L_NUM(10)  TYPE  N,
         L_INT      TYPE  I,
         L_ATFLV    TYPE  AUSP-ATFLV,
         L_ATWRT    TYPE  AUSP-ATWRT.

  SELECT SINGLE ATWRT
                ATFLV
               INTO (L_ATWRT, L_ATFLV)
               FROM AUSP
               WHERE OBJEK EQ P_OBJEK
                 AND ATINN EQ P_ATINN
                 AND KLART EQ P_KLART.
  IF P_ATFOR EQ 'CHAR'.
    P_ATWRT = L_ATWRT.
  ELSE.
    L_NUM = L_ATFLV.
    L_INT = L_NUM.
    WRITE L_INT TO P_ATWRT LEFT-JUSTIFIED NO-GROUPING.
  ENDIF.

ENDFORM.                    " SELECT_AUSP
*&---------------------------------------------------------------------*
*&      Form  BOM_REFRESH
*&---------------------------------------------------------------------*
FORM BOM_REFRESH.
  DATA : L_TABIX  TYPE  SY-TABIX.
  CLEAR : IT_ERROR, IT_ERROR[], WA_ERROR_IX.
  LOOP AT IT_PLAF.
*----> Read Vehicle Master
    CLEAR WA_OBJEK.
    PERFORM SELECT_OBJEK USING  WA_ATINN '002' IT_PLAF-PLNUM WA_ATFOR
                         CHANGING WA_OBJEK.

    IF SY-SUBRC NE 0.
      WA_ERROR_IX = WA_ERROR_IX + 1.
      MOVE IT_PLAF-PLNUM TO IT_ERROR-PLNUM.
      MOVE TEXT-201      TO IT_ERROR-ZMSG.
      APPEND IT_ERROR.  CLEAR IT_ERROR.
    ELSE.
*----> Check P_RP_STATUS
      PERFORM SELECT_CLASSIFICATION USING C_RP_STATUS
                                    CHANGING WA_ATWRT.
      WA_RPOINT = WA_ATWRT.
      IF WA_RPOINT EQ '00'.
        PERFORM GENERATE_BDC_DATA.
        PERFORM CALL_TRANSACTION.
      ELSE.
        WA_ERROR_IX = WA_ERROR_IX + 1.
        MOVE IT_PLAF-PLNUM  TO  IT_ERROR-PLNUM.
        MOVE TEXT-202       TO  IT_ERROR-ZMSG.
        APPEND IT_ERROR. CLEAR IT_ERROR.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BOM_REFRESH

*&---------------------------------------------------------------------*
*&      Form  SET_MODE
*&---------------------------------------------------------------------*
FORM SET_MODE.
*----> SET BDC MODE OPTION
  CLEAR : WA_OPTION_DS.
  WA_OPTION_DS-DISMODE = C_MODE.
  WA_OPTION_DS-DEFSIZE = 'X'.
  WA_OPTION_DS-UPDMODE = 'S'.
ENDFORM.                    " SET_MODE
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM BDC_DYNPRO USING  P_PROGRAM
                       P_DYNPRO.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-PROGRAM  = P_PROGRAM.
  IT_BDCDATA-DYNPRO   = P_DYNPRO.
  IT_BDCDATA-DYNBEGIN = 'X'.
  APPEND IT_BDCDATA.

ENDFORM.                    " BDC_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
FORM BDC_FIELD USING    P_FNAM
                        P_FVAL.
*  IF P_FVAL <> Nodata.
  CLEAR IT_BDCDATA.
  IT_BDCDATA-FNAM = P_FNAM.
  IT_BDCDATA-FVAL = P_FVAL.
  APPEND IT_BDCDATA.
*  ENDIF.

ENDFORM.                    " BDC_FIELD


*&---------------------------------------------------------------------*
*&      Form  GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
FORM GENERATE_BDC_DATA.
*----> PLANNED NUMBER
  PERFORM BDC_DYNPRO USING  'SAPMM61P'	'0101'.
  PERFORM BDC_FIELD USING : 'BDC_CURSOR'	'RM61P-PLNUM',
                            'BDC_OKCODE'	'/00',
                            'RM61P-PLNUM'	IT_PLAF-PLNUM.
*----> BOM REFRESH
  PERFORM BDC_DYNPRO USING  'SAPLM61O'	'0110'.
  PERFORM BDC_FIELD USING : 'BDC_CURSOR'	'PLAF-MATNR',
                            'BDC_OKCODE'	'=STAL'.

  PERFORM BDC_DYNPRO USING  'SAPLM61Q'	'0115'.
  PERFORM BDC_FIELD USING : 'BDC_OKCODE'	'=BACK',
                            'BDC_CURSOR'	'MDPA-MATNR'.

  PERFORM BDC_DYNPRO USING  'SAPLM61O'	'0110'.
  PERFORM BDC_FIELD USING : 'BDC_CURSOR'	'PLAF-MATNR',
                            'BDC_OKCODE'	'=HZPL'.

ENDFORM.                    " GENERATE_BDC_DATA
*&---------------------------------------------------------------------*
*&      Form  CALL_TRANSACTION
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION.
  CALL TRANSACTION 'MD12' USING IT_BDCDATA
                          OPTIONS FROM WA_OPTION_DS.
  PERFORM ERROR_TEXT.
  CLEAR IT_BDCDATA.
  REFRESH IT_BDCDATA.
ENDFORM.                    " CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  ERROR_TEXT
*&---------------------------------------------------------------------*
FORM ERROR_TEXT.
  DATA : L_MSG    LIKE CFGNL-MSGLIN,
         L_TABIX  TYPE SY-TABIX.

  CALL FUNCTION 'RKC_MSG_STRING'
       EXPORTING
            ID      = SY-MSGID
            MTYPE   = SY-MSGTY
            NUMBER  = SY-MSGNO
            PAR1    = SY-MSGV1
            PAR2    = SY-MSGV2
            PAR3    = SY-MSGV3
            PAR4    = SY-MSGV4
       IMPORTING
            MSG_LIN = L_MSG
       EXCEPTIONS
            OTHERS  = 1.

  CASE SY-MSGTY.
    WHEN 'E' OR 'A' OR 'X' OR 'W'.
      WA_ERROR_IX = WA_ERROR_IX + 1.
      MOVE IT_PLAF-PLNUM  TO IT_ERROR-PLNUM.
      MOVE L_MSG          TO IT_ERROR-ZMSG.
      APPEND IT_ERROR.  CLEAR IT_ERROR.
    WHEN OTHERS.     " 'I', 'S' :SUCCESS

  ENDCASE.
ENDFORM.                    " ERROR_TEXT
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.

  PERFORM WRITE_RESULT.

  CLEAR WA_PSTTR_FLG.
  SET PARAMETER ID 'ZBAT' FIELD WA_PSTTR_FLG.
  CALL FUNCTION 'DEQUEUE_EZPP_COMPPARTVIN'
       EXPORTING
            MODE_ZTPP_COMPPARTVIN = 'E'
            MANDT                 = SY-MANDT.
ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  WRITE_RESULT
*&---------------------------------------------------------------------*
FORM WRITE_RESULT.
  DATA : L_TABIX       LIKE  SY-TABIX,
         L_SUCCESS_IX  LIKE  SY-TABIX.

  L_SUCCESS_IX = WA_PLAF_IX - WA_ERROR_IX.
  WRITE:/ 'Total count of Uploaded Data : ' , WA_PLAF_IX.
  WRITE:/ 'Success count of BOM Refresh : ' , L_SUCCESS_IX.
  WRITE:/ 'Error count of BOM Refresh   : ' , WA_ERROR_IX.
  SKIP 1.
  LOOP AT IT_ERROR.
    AT FIRST.
      WRITE :/ '*********** BEGIN of Detailed Error List **********'.
    ENDAT.
    L_TABIX = SY-TABIX MOD 2.
    IF L_TABIX EQ 0.
      FORMAT INTENSIFIED ON.
    ELSE.
      FORMAT INTENSIFIED OFF.
    ENDIF.
    WRITE:/ IT_ERROR-PLNUM COLOR COL_KEY,
            IT_ERROR-ZMSG  COLOR COL_NORMAL.
    AT LAST.
      WRITE :/ '*********** END of Detailed Error List **********'.
    ENDAT.
  ENDLOOP.

  FORMAT RESET INTENSIFIED ON.
  SKIP 2.
  WRITE :/ '********** END OF PROCESS ***********'.
  WRITE AT: /001(030) 'Processing Time...(Start)' ,
             031(010) SY-DATUM                    ,
             042(010) SY-UZEIT                    .
  WRITE :/ 'Planned Order BOM Refresh'.
  WRITE :/ '********** END OF PROCESS ***********'.
ENDFORM.                    " WRITE_RESULT
