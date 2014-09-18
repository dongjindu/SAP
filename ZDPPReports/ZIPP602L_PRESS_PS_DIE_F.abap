*----------------------------------------------------------------------*
*   INCLUDE ZIPP602L_PRESS_PS_DIE_F                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  P_WERKS = 'P001'.

ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
  DATA : L_NAME1   TYPE   T001W-NAME1 .
  SELECT SINGLE NAME1
               INTO L_NAME1
               FROM T001W
               WHERE WERKS EQ P_WERKS.
  IF SY-SUBRC NE 0.
    MESSAGE E001 WITH TEXT-301 .
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  CASE C_MARK.
    WHEN P_TRAN.         " Transfer
      PERFORM SELECT_MARA_MARC.
    WHEN P_RETR.         " Retransfer
      PERFORM SELECT_ZTPPPS_DIE.
  ENDCASE.
ENDFORM.                    " READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  PERFORM DISPLAY_ZTPPPS.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form SELECT_MARA_MARC
*&---------------------------------------------------------------------*
FORM SELECT_MARA_MARC.
*----> SELECT MARA & MARC
*  SELECT A~MATNR
*         A~WERKS
*         A~EISBE
*         A~BSTMI
*         A~BSTMA
*         A~BSTFE
*         B~GROES
*         B~NTGEW
*         INTO TABLE IT_MARC
*         FROM MARA AS B INNER JOIN MARC AS A
*           ON B~MATNR EQ A~MATNR
*         WHERE A~WERKS EQ P_WERKS
*           AND A~MATNR IN S_MATNR .
*           AND A~FEVOR EQ C_SPD       " Prod Scheduler  'SPD'
*           AND A~DISPO EQ C_DISPO     " MRP controller  'MP1'
*           AND B~MSTAE NE C_MSTAE .   " Material status '99'

  SELECT MATNR
         INTO TABLE IT_MARC
         FROM MARA
         WHERE MATNR IN S_MATNR
           AND MSTAE NE C_MSTAE .   " Material status '02'

*----> APPEND IT_ZSPPPS
  LOOP AT IT_MARC.
    MOVE-CORRESPONDING IT_MARC TO IT_ZSPPPS.
    MOVE P_WERKS               TO IT_ZSPPPS-WERKS.
    CLEAR : MAKT , MARM .

    SELECT SINGLE MAKTX
           INTO MAKT-MAKTX
           FROM MAKT
           WHERE MATNR EQ IT_MARC-MATNR.

*    SELECT SINGLE UMREZ
*           INTO MARM-UMREZ
*           FROM MARM
*           WHERE MATNR EQ IT_MARC-MATNR
*             AND MEINH EQ 'PAL'.

    IT_ZSPPPS-MAKTX = MAKT-MAKTX.
*    IT_ZSPPPS-UMREZ = MARM-UMREZ.

*----> Read Classification
    PERFORM READ_CLASSIFICATION.

    CASE C_MARK.
      WHEN P_IR.
        IT_ZSPPPS-EFLAG = 'IR'.
      WHEN P_RP.
        IT_ZSPPPS-EFLAG = 'RP'.
      WHEN P_DL.
        IT_ZSPPPS-EFLAG = 'DL'.
    ENDCASE.

    MOVE SY-MANDT    TO   IT_ZSPPPS-MANDT .
    APPEND IT_ZSPPPS.     CLEAR IT_ZSPPPS .
  ENDLOOP.

  SORT IT_ZSPPPS BY PRS_DIE_MWC MATNR.

ENDFORM.                    "SELECT_MARA_MARC
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTPPPS_DIE
*&---------------------------------------------------------------------*
FORM SELECT_ZTPPPS_DIE.
  DATA: L_TABIX LIKE SY-TABIX.

  CLEAR : *IT_ZTPPPS, *IT_ZTPPPS[],
           IT_ZSPPPS, IT_ZSPPPS[].
  SELECT *
        INTO TABLE *IT_ZTPPPS
        FROM ZTPPPS_DIE
        WHERE WERKS EQ P_WERKS
          AND MATNR IN S_MATNR.

  LOOP AT *IT_ZTPPPS.
    L_TABIX = SY-TABIX.
    CASE C_MARK.
      WHEN P_IR .
         *IT_ZTPPPS-EFLAG = 'IR'.
      WHEN P_RP .
         *IT_ZTPPPS-EFLAG = 'RP'.
      WHEN P_DL .
         *IT_ZTPPPS-EFLAG = 'DL'.
    ENDCASE.
    MODIFY *IT_ZTPPPS INDEX L_TABIX.
    MOVE-CORRESPONDING *IT_ZTPPPS TO IT_ZSPPPS.
    APPEND IT_ZSPPPS .  CLEAR IT_ZSPPPS .
  ENDLOOP.
ENDFORM.                    " SELECT_ZTPPPS_DIE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ZTPPPS
*&---------------------------------------------------------------------*
FORM DISPLAY_ZTPPPS.
  LOOP AT IT_ZSPPPS.
    MOVE-CORRESPONDING IT_ZSPPPS TO IT_LIST.
    APPEND IT_LIST.
  ENDLOOP.
ENDFORM.                    " DISPLAY_ZTPPPS
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.
  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_EVENT.
  PERFORM BUILD_SORT.
  PERFORM COMMENT_BUILD USING  W_TOP_OF_PAGE[].
  PERFORM CALL_FUNCTION.
ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
FORM CALL_FUNCTION.
  DATA:   L_PRINT_P TYPE SLIS_PRINT_ALV.  " print setting

  CLEAR  W_PROGRAM.
  W_PROGRAM = SY-REPID.

*** print paramter   ****************************************
  L_PRINT_P-NO_COVERPAGE = 'X'.
  L_PRINT_P-NO_PRINT_LISTINFOS = 'X'.
  L_PRINT_P-NO_CHANGE_PRINT_PARAMS = 'X'.
  L_PRINT_P-NO_PRINT_SELINFOS = 'X'.
*************************************************************

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            I_BYPASSING_BUFFER       = 'X'
            I_CALLBACK_PROGRAM       = W_PROGRAM
            I_CALLBACK_PF_STATUS_SET = 'ALV_PF_STATUS_SET'
            I_CALLBACK_TOP_OF_PAGE   = 'TOP_OF_PAGE'
            I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
            IT_FIELDCAT              = W_FIELDCAT[]
            IT_SORT                  = W_SORTCAT[]
            I_SAVE                   = 'A'
            IT_EVENTS                = W_EVENTCAT[]
            IS_PRINT                 = L_PRINT_P
       TABLES
            T_OUTTAB                 = IT_LIST
       EXCEPTIONS
            PROGRAM_ERROR            = 1
            OTHERS                   = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_FUNCTION
*&---------------------------------------------------------------------*
*&      Form  create_interface_log
*&---------------------------------------------------------------------*
FORM CREATE_INTERFACE_LOG.
*  DESCRIBE TABLE IT_ZTPPPS_DIE LINES Z_TOTAL.
*  CHECK Z_TOTAL <> 0.
*  I_ZTCA_IF_LOG-TCODE    = 'ZPPI501'.
**  I_ZTCA_IF_LOG-ZSLNO    = WA_JOB-SLNO.
**  I_ZTCA_IF_LOG-JOBCOUNT = WA_JOB-INT.
*  I_ZTCA_IF_LOG-TOTAL    = Z_TOTAL.
**  I_ZTCA_IF_LOG-ZSUCC    = Z_SUCC.
**  I_ZTCA_IF_LOG-ERROR    = Z_TOTAL - Z_SUCC.
*  I_ZTCA_IF_LOG-ERDAT    = SY-DATUM. "Created on.
*  I_ZTCA_IF_LOG-ERZET    = SY-UZEIT. "Created time.
*  I_ZTCA_IF_LOG-ERNAM    = SY-UNAME. "Created by.
*
*  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
*    EXPORTING
*      I_ZTCA_IF_LOG              = I_ZTCA_IF_LOG
**   IMPORTING
**     E_ZTCA_IF_LOG              =
*   EXCEPTIONS
*     UPDATE_FAILED              = 1
*     NUMBER_RANGE_ERROR         = 2
*     TCODE_DOES_NOT_EXIST       = 3
*     OTHERS                     = 4
*            .
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.

ENDFORM.                    " create_interface_log
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  CLEAR W_COL_POS .
  APPEND_FIELDCAT :
*   postion    field           ref field       key   q-ref   c-ref
*   Text                                   length   quantity
*    W_COL_POS 'WERKS'          'WERKS'         'X'    ''      ''
*    'Plant'                                '5'      ''   ,
    W_COL_POS 'PRS_DIE_MWC'    'PRS_DIE_MWC'   'X'    ''      ''
    'Main W/C'                             '8'      ''   ,
    W_COL_POS 'MATNR'          'MATNR'         'X'    ''      ''
    'Material #'                           '12'     ''   ,
    W_COL_POS 'MAKTX'          'MAKTX'         ''    ''      ''
    'Description'                          '15'     ''   ,
    W_COL_POS 'PRS_DIE_OPN'    'PRS_DIE_OPN'   ''     ''      ''
    'Operation #'                          '3'      ''   ,
    W_COL_POS 'PRS_DIE_PRP'    'PRS_DIE_PRP'   ''     ''      ''
    'Prod. Frequency'                      '5'      ''   ,
    W_COL_POS 'PRS_DIE_PLTN'   'PRS_DIE_PLTN'  ''     ''      ''
    'Pallet Code'                          '12'     ''   ,
    W_COL_POS 'PRS_DIE_TPLT'   'PRS_DIE_TPLT'  ''     ''      ''
    'Total Pallet Qty'                     '5'      ''   ,
    W_COL_POS 'PRS_DIE_PWT'    'PRS_DIE_PWT'   ''     ''      ''
    'Pre-working Time'                     '5'      ''   ,
    W_COL_POS 'PRS_DIE_GBP'    'PRS_DIE_GBP'   ''     ''      ''
    'Die for Panel or Blank'               '1'      ''   ,
    W_COL_POS 'PRS_DIE_VMDL'   'PRS_DIE_VMDL'  ''     ''      ''
    'Body Model'                           '4'      ''   ,
    W_COL_POS 'PRS_DIE_FPQ'    'PRS_DIE_FPQ'   ''     ''      ''
    'Fixed Pin Qty'                        '3'      ''   ,
    W_COL_POS 'PRS_DIE_DIEL'   'PRS_DIE_DIEL'  ''     ''      ''
    'Location of used DIE'                 '10'     ''   ,
    W_COL_POS 'PRS_DIE_SWC'    'PRS_DIE_SWC'   ''     ''      ''
    'Sub W/C'                              '8'      ''   ,
    W_COL_POS 'PRS_DIE_ALW'    'PRS_DIE_ALW'   ''     ''      ''
    'Allowance(%)'                         '5'      ''   ,
    W_COL_POS 'PRS_DIE_SPH'    'PRS_DIE_SPH'   ''     ''      ''
    'STD SPH'                              '5'      ''   ,
    W_COL_POS 'PRS_DIE_CGT'    'PRS_DIE_CGT'   ''     ''      ''
    'DIE Change Time'                      '5'      ''   .
ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENT
*&---------------------------------------------------------------------*
FORM BUILD_EVENT.
  W_EVENTCAT-NAME = 'TOP_OF_PAGE'.
  W_EVENTCAT-FORM = 'TOP_OF_PAGE'.

  APPEND W_EVENTCAT.

ENDFORM.                    " BUILD_EVENT
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
FORM BUILD_SORT.
  W_SORTCAT-SPOS           = 1.
  W_SORTCAT-FIELDNAME      = 'PRS_DIE_MWC'.
  W_SORTCAT-TABNAME        = 'IT_LIST'.
  W_SORTCAT-UP             = 'X'.
  APPEND W_SORTCAT.
*
  W_SORTCAT-SPOS           = 2.
  W_SORTCAT-FIELDNAME      = 'MATNR'.
  W_SORTCAT-TABNAME        = 'IT_LIST'.
  W_SORTCAT-UP             = 'X'.
  APPEND W_SORTCAT.
ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER,
        L_DATE(50),
        L_LIST(50),
        L_DATE1(10),
        L_DATE2(10).

*----- Title
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = TEXT-A01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- PLANT
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = TEXT-A02.
  LS_LINE-INFO = P_WERKS.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- TOTAL COUNT
  DATA : L_LINES      TYPE    SY-TABIX  ,
         L_TEXT_LINE(10) .
  DESCRIBE TABLE IT_LIST LINES L_LINES.
  WRITE L_LINES  TO  L_TEXT_LINE LEFT-JUSTIFIED .
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = TEXT-A03 .
  LS_LINE-INFO = L_TEXT_LINE.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = ' '.
  LS_LINE-INFO = ' '.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Total
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Total: '.
*  LS_LINE-INFO = Z_TOTAL.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Success
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'Success: '.
*  LS_LINE-INFO = Z_SUCC.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

ENDFORM.                    " COMMENT_BUILD
*&---------------------------------------------------------------------
*         Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------
FORM ALV_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD'. " EXCLUDING RT_EXTAB.

ENDFORM.                    " ALV_PF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM TOP_OF_PAGE.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
*           i_logo             = 'Z_HYUNDAI_LOGO'
*           i_logo             = 'ENJOYSAP_LOGO'
            IT_LIST_COMMENTARY = W_TOP_OF_PAGE.

ENDFORM.                    " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  EVENT_BUILD
*&---------------------------------------------------------------------*
FORM EVENT_BUILD USING P_W_EVENTCAT TYPE SLIS_T_EVENT.
  DATA : L_EVENT TYPE SLIS_ALV_EVENT.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
       EXPORTING
            I_LIST_TYPE = 0
       IMPORTING
            ET_EVENTS   = P_W_EVENTCAT.

  READ TABLE P_W_EVENTCAT WITH KEY NAME = SLIS_EV_TOP_OF_PAGE
                          INTO L_EVENT.

  IF SY-SUBRC EQ 0.
    MOVE C_FORMNAME_TOP_OF_PAGE TO L_EVENT-FORM.
    APPEND L_EVENT TO P_W_EVENTCAT.
  ENDIF.
ENDFORM.                    " EVENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZTPPPS_DIE
*&---------------------------------------------------------------------*
FORM MODIFY_ZTPPPS_DIE.
  MODIFY ZTPPPS_DIE FROM TABLE IT_ZTPPPS.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
ENDFORM.                    " MODIFY_ZTPPPS_DIE
*&-------------------------------------------------------------------
*&      Form  USER_COMMAND
*&-------------------------------------------------------------------
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE SLIS_SELFIELD.
  DATA : SEL_FIELD LIKE SELFIELD-SEL_TAB_FIELD.

  CASE UCOMM.
    WHEN '&REL'.
      DESCRIBE TABLE IT_ZSPPPS LINES Z_TOTAL.
      PERFORM TRANSFER_PP_TO_MES.
      PERFORM MODIFY_ZTPPPS_DIE.
*      PERFORM CREATE_INTERFACE_LOG.
      PERFORM CALL_SCREEN_RESULT.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                               " USER_COMMAND1
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
FORM TRANSFER_PP_TO_MES.
  DATA: L_MSGTXT(100),
        L_TABIX LIKE SY-TABIX.

  CLEAR : IT_ZTPPPS , IT_ZTPPPS[].

  CLEAR : IT_ZSPPPS-ZZRET   ,
          IT_ZSPPPS-ZRESULT ,
          IT_ZSPPPS-ZMSG    ,
          Z_SUCC , Z_FAIL   .
  MODIFY IT_ZSPPPS TRANSPORTING ZRESULT ZZRET ZMSG
                      WHERE MANDT EQ SY-MANDT .

*----> Tranfer to MES
  CALL FUNCTION 'Z_FPP_PRESS_PS_DIE'
    DESTINATION  C_DEST
    TABLES
      T_ENGINE = IT_ZSPPPS
  EXCEPTIONS
    COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
    SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

  IF SY-SUBRC NE 0.
    Z_FAIL = Z_TOTAL.
    IF P_TRAN EQ C_MARK.
      IT_ZSPPPS-ZSDAT   = SY-DATUM .
      IT_ZSPPPS-ZSTIM   = SY-UZEIT .
      IT_ZSPPPS-ZMODE = 'C'.
    ELSE.
      IT_ZSPPPS-ZMODE = 'U'.
    ENDIF.

    IT_ZSPPPS-ZZRET   = 'E'.
    IT_ZSPPPS-ZRESULT = 'E'.
    IT_ZSPPPS-ZUSER   = SY-UNAME .
    IT_ZSPPPS-ZMSG    = L_MSGTXT .
    MODIFY IT_ZSPPPS TRANSPORTING ZUSER ZSDAT ZSTIM ZMODE
                                 ZRESULT ZMSG ZZRET
                     WHERE MANDT EQ SY-MANDT .

  ELSE.

    LOOP AT IT_ZSPPPS.
      L_TABIX = SY-TABIX.
      IF P_TRAN EQ C_MARK.
        IT_ZSPPPS-ZSDAT   = SY-DATUM .
        IT_ZSPPPS-ZSTIM   = SY-UZEIT .
        IT_ZSPPPS-ZMODE = 'C'.
      ELSE.
        IT_ZSPPPS-ZMODE = 'U'.
      ENDIF.
      IT_ZSPPPS-ZUSER = SY-UNAME .

      IT_ZSPPPS-ZRESULT = IT_ZSPPPS-ZZRET .
      MODIFY IT_ZSPPPS  INDEX L_TABIX .

      IF IT_ZSPPPS-ZZRET EQ 'E'.
        Z_FAIL = Z_FAIL + 1.
      ENDIF.
    ENDLOOP.
  ENDIF.

*---- TABLE SETTING
  LOOP AT IT_ZSPPPS .
    MOVE-CORRESPONDING IT_ZSPPPS TO IT_ZTPPPS.
    APPEND IT_ZTPPPS .  CLEAR IT_ZTPPPS .
  ENDLOOP.


ENDFORM.                    " TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
*&      Form  USER_DATE
*&---------------------------------------------------------------------*
FORM USER_DATE USING    P_DATE
               CHANGING P_L_DATE1.

*  DATA: L_ORIGINAL_DATE TYPE D.
*
*  L_ORIGINAL_DATE = P_DATE.
*
*  CALL 'DATE_CONV_INT_TO_EXT'
*  ID 'DATINT' FIELD L_ORIGINAL_DATE
*  ID 'DATEXT' FIELD P_L_DATE1.

ENDFORM.                    " USER_DATE
*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM READ_CLASSIFICATION.
  CLEAR : IT_VM, IT_VM[].
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = IT_ZSPPPS-MATNR
*     MODE               = 'R'
      CTYPE              = '001'
*      DISPLAY            = 'D'
    TABLES
      VAL_TABLE          = IT_VM
    EXCEPTIONS
      NO_DATA            = 1
      ERROR_MODE         = 2
      ERROR_OBJECT       = 3
      OTHERS             = 4  .

  IF SY-SUBRC EQ 0 .
    LOOP AT IT_VM .
      CASE IT_VM-ATNAM .
        WHEN 'PRS_DIE_MWC'   .    "Main W/C
          IT_ZSPPPS-PRS_DIE_MWC   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_OPN'   .    "Operation Number
          IT_ZSPPPS-PRS_DIE_OPN   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_PRP'   .    "Prod. Frequency
          IT_ZSPPPS-PRS_DIE_PRP   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_PLTN'  .    "Pallet Code
          IT_ZSPPPS-PRS_DIE_PLTN  = IT_VM-ATWRT .
        WHEN 'PRS_DIE_TPLT'  .    "Total Pallet Qty
          IT_ZSPPPS-PRS_DIE_TPLT  = IT_VM-ATWRT .
        WHEN 'PRS_DIE_PWT'   .    "Pre-working Time
          IT_ZSPPPS-PRS_DIE_PWT   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_GBP'   .    "Die for Panel or Blank
          IT_ZSPPPS-PRS_DIE_GBP   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_VMDL'  .    "Body Model
          IT_ZSPPPS-PRS_DIE_VMDL  = IT_VM-ATWRT .
        WHEN 'PRS_DIE_FPQ'   .    "Fixed Pin Qty
          IT_ZSPPPS-PRS_DIE_FPQ   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_DIEL'  .    "Location of Used Die
          IT_ZSPPPS-PRS_DIE_DIEL  = IT_VM-ATWRT .
        WHEN 'PRS_DIE_SWC'   .    "Sub  W/C
          IT_ZSPPPS-PRS_DIE_SWC   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_ALW'   .    "Allowance (%)
          IT_ZSPPPS-PRS_DIE_ALW   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_SPH'   .    "STD SPH
          IT_ZSPPPS-PRS_DIE_SPH   = IT_VM-ATWRT .
        WHEN 'PRS_DIE_CGT'   .    "DIE Change Time
          IT_ZSPPPS-PRS_DIE_CGT   = IT_VM-ATWRT .
      ENDCASE.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " READ_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
FORM CALL_SCREEN_RESULT.
  Z_SUCC = Z_TOTAL - Z_FAIL.

  CALL SCREEN 50 STARTING AT 20 10.

ENDFORM.                    " CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0050 OUTPUT.
  SET PF-STATUS '50'.
  SET TITLEBAR '50'.

ENDMODULE.                 " STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0050 INPUT.
  OK_CODE = OKCODE.
  CLEAR OKCODE.

  CASE OK_CODE.
    WHEN 'ENTE' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT
