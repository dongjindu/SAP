************************************************************************
* Program Name      : ZIPP302U_BOM_PHANTOM
* Author            : Bongsoo, Kim
* Creation Date     : 2004.01.29.
* Specifications By : Bongsoo, Kim
* Pattern           : 2.1
* Development Request No : UD1K906545
* Addl Documentation:
* Description       : BOM PHANTOM CHECK
*
* Modification Logs
* Date       Developer    RequestNo    Description
*
*
*
************************************************************************
REPORT ZIPP302U_BOM_PHANTOM
                NO STANDARD PAGE HEADING
                LINE-SIZE  1023
                LINE-COUNT 65
                MESSAGE-ID ZMBM.
*----------------------------------------------------------------------*
* TABLES DECLARATION
*----------------------------------------------------------------------*
TABLES: MARA,
        MARC,
        ZTBM_ABXEBMDT.
*----------------------------------------------------------------------*
* INTERNAL TABLES  DECLARATION
*----------------------------------------------------------------------*
DATA: IT_BMDT TYPE ZTBM_ABXEBMDT OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF IT_BDCT OCCURS 0,
        MATNR TYPE ZTBM_ABXEBMDT-MTNO,
        WERKS TYPE ZTBM_ABXEBMDT-PLNT,
        SOBSL TYPE ZTBM_ABXEBMDT-SPPR,
        ZMSG  TYPE CFGNL-MSGLIN,
        MSGTY TYPE SY-MSGTY,
      END   OF IT_BDCT.
*----------------------------------------------------------------------*
* BDC-DATA
*----------------------------------------------------------------------*
DATA: BEGIN OF IT_BDC OCCURS 0.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF IT_BDC.

DATA: BEGIN OF IT_MESS OCCURS 0.
        INCLUDE STRUCTURE BDCMSGCOLL.
DATA: END OF IT_MESS.

DATA: BEGIN OF WA_OPT OCCURS 0.
        INCLUDE STRUCTURE CTU_PARAMS.
DATA: END OF WA_OPT.
*----------------------------------------------------------------------*
* DATA
*----------------------------------------------------------------------*
DATA: WA_LINE_IDX TYPE I,
      WA_ERRO_IDX TYPE I.
*----------------------------------------------------------------------*
* CONSTANS
*----------------------------------------------------------------------*
CONSTANTS WA_SPPR(2) VALUE '50'.
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
* TABLE SELECTION
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-100.
PARAMETERS: P_ZEDAT LIKE ZTBM_ABXEBMDT-ZEDAT DEFAULT SY-DATUM,
            P_ZBTIM LIKE ZTBM_ABXEBMDT-ZBTIM.
SELECTION-SCREEN END   OF BLOCK B1.
*----------------------------------------------------------------------*
* EVENT
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

START-OF-SELECTION.
  PERFORM READ_PROCESS.
  PERFORM DATA_PROCESS.
  PERFORM BDC_PROCESS.
  PERFORM WRITE_PROCESS.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
* BDC MODE, DEFAULT SIZE, UPDATE MODE
  WA_OPT-DEFSIZE = 'X'.
  WA_OPT-DISMODE = 'N'.
  WA_OPT-UPDMODE = 'S'.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.

  SELECT *
       FROM ZTBM_ABXEBMDT
       INTO TABLE IT_BMDT
       WHERE ZEDAT EQ P_ZEDAT
       AND   SPPR  EQ WA_SPPR
       AND   ZRESULT NE 'E' .
  IF SY-SUBRC EQ 0.
    DESCRIBE TABLE IT_BMDT LINES WA_LINE_IDX.
    FORMAT COLOR COL_HEADING INTENSIFIED OFF.
    WRITE: / TEXT-002, WA_LINE_IDX.
    CLEAR WA_LINE_IDX.
    FORMAT COLOR OFF.
  ELSE.
    WRITE: / TEXT-001.
  ENDIF.
ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
* MOVE IT_BMDT TO IT_BDCT COLLECT
  PERFORM MOVE_IT_BMDT_2_IT_BDCT_COLLECT.
* MARC PHANTOM(MRP 2) & NO COSTING(COSTING 1) CHECK
  PERFORM PHANTOM_COSTING_CHECK.
ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MOVE_IT_BMDT_2_IT_BDCT_COLLECT
*&---------------------------------------------------------------------*
FORM MOVE_IT_BMDT_2_IT_BDCT_COLLECT.
  LOOP AT IT_BMDT.
    IT_BDCT-MATNR = IT_BMDT-COMP.
    IT_BDCT-WERKS = IT_BMDT-PLNT.
    IT_BDCT-SOBSL = IT_BMDT-SPPR.
    COLLECT IT_BDCT.
    CLEAR: IT_BDCT, IT_BMDT.
  ENDLOOP.
ENDFORM.                    " MOVE_IT_BMDT_2_IT_BDCT_COLLECT
*&---------------------------------------------------------------------*
*&      Form  PHANTOM_COSTING_CHECK
*&---------------------------------------------------------------------*
FORM PHANTOM_COSTING_CHECK.
  DATA L_TABIX TYPE SY-TABIX.
  DATA: L_SOBSL TYPE MARC-SOBSL,
        L_NCOST TYPE MARC-NCOST.
  LOOP AT IT_BDCT.
    L_TABIX = SY-TABIX.
    SELECT SINGLE SOBSL
                  NCOST
                INTO (L_SOBSL, L_NCOST)
                FROM MARC
                WHERE MATNR EQ IT_BDCT-MATNR
                AND   WERKS EQ IT_BDCT-WERKS
                AND   SOBSL EQ IT_BDCT-SOBSL
                AND   NCOST EQ 'X'.
    IF SY-SUBRC EQ 0.
      DELETE IT_BDCT INDEX L_TABIX.
    ENDIF.
    CLEAR: IT_BDCT, L_SOBSL, L_NCOST.
  ENDLOOP.
ENDFORM.                    " PHANTOM_COSTING_CHECK
*&---------------------------------------------------------------------*
*&      Form  BDC_PROCESS
*&---------------------------------------------------------------------*
FORM BDC_PROCESS.
  DATA L_TABIX TYPE SY-TABIX.
  DESCRIBE TABLE IT_BDCT LINES WA_LINE_IDX.
  FORMAT COLOR COL_HEADING INTENSIFIED OFF.
  WRITE: / TEXT-003, WA_LINE_IDX.
  CLEAR WA_LINE_IDX.
  FORMAT COLOR OFF.
  REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
  LOOP AT IT_BDCT.
    L_TABIX = SY-TABIX.
    PERFORM DYNPRO USING:
       'X' 'SAPLMGMM'    '0060',
       ' ' 'RMMG1-MATNR' IT_BDCT-MATNR,    "
       ' ' 'BDC_OKCODE'  '=AUSW',

       'X' 'SAPLMGMM'    '0070',
       ' ' 'MSICHTAUSW-KZSEL(01)' 'X',    "
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '5004',
       ' ' 'BDC_OKCODE'  '=SP13',

       'X' 'SAPLMGMM'    '0081',
       ' ' 'RMMG1-WERKS' IT_BDCT-WERKS,
       ' ' 'BDC_OKCODE'  '=ENTR',

       'X' 'SAPLMGMM'    '5000',
       ' ' 'MARC-SOBSL'  IT_BDCT-SOBSL,
       ' ' 'BDC_OKCODE'  '=SP26',

       'X' 'SAPLMGMM'    '5000',
       ' ' 'MARC-NCOST'  'X',
       ' ' 'BDC_OKCODE'  '=BU'.
*   CALL TRANSACTION
    CALL TRANSACTION 'MM02'  USING IT_BDC
                             OPTIONS FROM WA_OPT
                             MESSAGES INTO IT_MESS.
    IT_BDCT-MSGTY = SY-MSGTY.
    PERFORM RKC_MSG_STRING CHANGING IT_BDCT-ZMSG.
*   MODIFY IT_BDCT
    MODIFY IT_BDCT INDEX L_TABIX TRANSPORTING MSGTY ZMSG.
*   REFRESH IT_BDC. IT_MESS.
    REFRESH: IT_BDC, IT_MESS. CLEAR: IT_BDC, IT_MESS.
    CLEAR: IT_BDCT.
  ENDLOOP.
ENDFORM.                    " BDC_PROCESS
*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
FORM DYNPRO USING DYNBEGIN NAME VALUE.
  IF DYNBEGIN = 'X'.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-PROGRAM,
          VALUE TO IT_BDC-DYNPRO,
          DYNBEGIN TO IT_BDC-DYNBEGIN.
    APPEND IT_BDC.
  ELSE.
    CLEAR IT_BDC.
    MOVE: NAME TO IT_BDC-FNAM,
          VALUE TO IT_BDC-FVAL.
    APPEND IT_BDC.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  RKC_MSG_STRING
*&---------------------------------------------------------------------*
FORM RKC_MSG_STRING CHANGING P_MSG.
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
            MSG_LIN = P_MSG
       EXCEPTIONS
            OTHERS  = 1.
ENDFORM.                    " RKC_MSG_STRING
*&---------------------------------------------------------------------*
*&      Form  WRITE_PROCESS
*&---------------------------------------------------------------------*
FORM WRITE_PROCESS.
  DESCRIBE TABLE IT_BDCT LINES WA_LINE_IDX.
  LOOP AT IT_BDCT WHERE MSGTY EQ 'E'.
    WA_LINE_IDX = WA_LINE_IDX + 1.
  ENDLOOP.
  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
  WRITE: / TEXT-004, WA_LINE_IDX.
  CLEAR WA_LINE_IDX.
  FORMAT COLOR OFF.
  WRITE: /(20) TEXT-005,
          (10) TEXT-006,
          (15) TEXT-007,
         (100) TEXT-008.
  LOOP AT IT_BDCT WHERE MSGTY EQ 'E'.
    WRITE: /(20) IT_BDCT-MATNR,
            (10) IT_BDCT-WERKS,
            (15) IT_BDCT-SOBSL,
           (100) IT_BDCT-ZMSG.
  ENDLOOP.

ENDFORM.                    " WRITE_PROCESS
