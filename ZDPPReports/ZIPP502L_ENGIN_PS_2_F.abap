*----------------------------------------------------------------------*
*   INCLUDE ZIPP502L_ENGIN_PS_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  CASE C_MARK.
    WHEN R1.   "Transfer
*      PERFORM READ_CLASSIFCATION.
      PERFORM SELECT_MARC .
      PERFORM READ_CLASSIFICATION .
    WHEN R2.   "Re-transfer
      PERFORM SELECT_ZTPPES.
  ENDCASE.

ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFCATION
*&---------------------------------------------------------------------*
FORM READ_CLASSIFCATION.
*  DATA: L_MSGTXT(100),
*        L_TABIX LIKE SY-TABIX,
*        L_OBJECTKEY LIKE BAPI1003_KEY-OBJECT.
*
*  CLEAR : IT_MARA , IT_MARA[] .
*  SELECT SINGLE MATNR
*         INTO TABLE IT_MARA
*         FROM MARA
*         WHERE MATNR EQ P_MATNR.
*
*  IF SY-SUBRC NE 0.
*    MESSAGE E001 WITH TEXT-301.
*  ELSE.
** Reading Classification
*    L_OBJECTKEY = P_MATNR.
*    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
*         EXPORTING
*              OBJECTKEY       = L_OBJECTKEY
*              OBJECTTABLE     = 'MARA'
*              CLASSNUM        = 'ENG_SPEC_MASTER'
*              CLASSTYPE       = '001'
*         TABLES
*              ALLOCVALUESNUM  = IT_VMASTER1
*              ALLOCVALUESCHAR = IT_VMASTER
*              ALLOCVALUESCURR = IT_VMASTER2
*              RETURN          = RETURN.
*
*    LOOP AT IT_VMASTER.
*      CASE IT_VMASTER-CHARACT.
*        WHEN 'EN_VEH_MODEL'.
*          IT_ZTPPES-EN_VEH_MODEL = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_HEAD'.
*          IT_ZTPPES-EN_HEAD = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC01'.
*          IT_ZTPPES-EN_SPC01 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC02'.
*          IT_ZTPPES-EN_SPC02 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC03'.
*          IT_ZTPPES-EN_SPC03 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC04'.
*          IT_ZTPPES-EN_SPC04 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC05'.
*          IT_ZTPPES-EN_SPC05 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC06'.
*          IT_ZTPPES-EN_SPC06 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC07'.
*          IT_ZTPPES-EN_SPC07 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC08'.
*          IT_ZTPPES-EN_SPC08 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC09'.
*          IT_ZTPPES-EN_SPC09 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC10'.
*          IT_ZTPPES-EN_SPC10 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC11'.
*          IT_ZTPPES-EN_SPC11 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC12'.
*          IT_ZTPPES-EN_SPC12 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC13'.
*          IT_ZTPPES-EN_SPC13 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC14'.
*          IT_ZTPPES-EN_SPC14 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC15'.
*          IT_ZTPPES-EN_SPC15 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC16'.
*          IT_ZTPPES-EN_SPC16 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC17'.
*          IT_ZTPPES-EN_SPC17 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC18'.
*          IT_ZTPPES-EN_SPC18 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC19'.
*          IT_ZTPPES-EN_SPC19 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC20'.
*          IT_ZTPPES-EN_SPC20 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC21'.
*          IT_ZTPPES-EN_SPC21 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC22'.
*          IT_ZTPPES-EN_SPC22 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC23'.
*          IT_ZTPPES-EN_SPC23 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC24'.
*          IT_ZTPPES-EN_SPC24 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC25'.
*          IT_ZTPPES-EN_SPC25 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC26'.
*          IT_ZTPPES-EN_SPC36 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC27'.
*          IT_ZTPPES-EN_SPC37 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC28'.
*          IT_ZTPPES-EN_SPC38 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC29'.
*          IT_ZTPPES-EN_SPC39 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC30'.
*          IT_ZTPPES-EN_SPC30 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC31'.
*          IT_ZTPPES-EN_SPC31 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC32'.
*          IT_ZTPPES-EN_SPC32 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC33'.
*          IT_ZTPPES-EN_SPC33 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC34'.
*          IT_ZTPPES-EN_SPC34 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC35'.
*          IT_ZTPPES-EN_SPC35 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC36'.
*          IT_ZTPPES-EN_SPC36 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC37'.
*          IT_ZTPPES-EN_SPC37 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC38'.
*          IT_ZTPPES-EN_SPC38 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC39'.
*          IT_ZTPPES-EN_SPC39 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC40'.
*          IT_ZTPPES-EN_SPC40 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC41'.
*          IT_ZTPPES-EN_SPC41 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC42'.
*          IT_ZTPPES-EN_SPC42 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC43'.
*          IT_ZTPPES-EN_SPC43 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC44'.
*          IT_ZTPPES-EN_SPC44 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC45'.
*          IT_ZTPPES-EN_SPC45 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC46'.
*          IT_ZTPPES-EN_SPC46 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC47'.
*          IT_ZTPPES-EN_SPC47 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC48'.
*          IT_ZTPPES-EN_SPC48 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC49'.
*          IT_ZTPPES-EN_SPC49 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC50'.
*          IT_ZTPPES-EN_SPC50 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC51'.
*          IT_ZTPPES-EN_SPC51 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC52'.
*          IT_ZTPPES-EN_SPC52 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC53'.
*          IT_ZTPPES-EN_SPC53 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC54'.
*          IT_ZTPPES-EN_SPC54 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC55'.
*          IT_ZTPPES-EN_SPC55 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC56'.
*          IT_ZTPPES-EN_SPC56 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC57'.
*          IT_ZTPPES-EN_SPC57 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC58'.
*          IT_ZTPPES-EN_SPC58 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC59'.
*          IT_ZTPPES-EN_SPC59 = IT_VMASTER-VALUE_NEUTRAL.
*        WHEN 'EN_SPC60'.
*          IT_ZTPPES-EN_SPC60 = IT_VMASTER-VALUE_NEUTRAL.
*      ENDCASE.
*    ENDLOOP.
*
*    CASE C_MARK.
*      WHEN R_1.
*        IT_ZTPPES-EFLAG = 'IR'.
*      WHEN R_2.
*        IT_ZTPPES-EFLAG = 'RP'.
*      WHEN R_3.
*        IT_ZTPPES-EFLAG = 'DL'.
*    ENDCASE.
*
*    IT_ZTPPES-EN_ITEM  = P_MATNR.
**    IT_ZTPPES-ZUSER    = SY-UNAME.
**    IT_ZTPPES-ZSDAT    = SY-DATUM.
**    IT_ZTPPES-ZSTIM    = SY-UZEIT.
**    IT_ZTPPES-ZEDAT    = SY-DATUM.
**    IT_ZTPPES-ZETIM    = SY-UZEIT.
**    IT_ZTPPES-ZMODE    = 'C'.
*    APPEND IT_ZTPPES.
*  ENDIF.

ENDFORM.                    " READ_CLASSIFCATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTPPES
*&---------------------------------------------------------------------*
FORM SELECT_ZTPPES.
  DATA: L_TABIX LIKE SY-TABIX.

  SELECT * FROM ZTPPES2
           INTO TABLE IT_ZTPPES2
*           WHERE FLAG EQ 'E'
           WHERE EN_ITEM  IN S_MATNR .

  SELECT * FROM ZTPPES
           APPENDING TABLE IT_ZTPPES2
*           WHERE FLAG EQ 'E'
           WHERE EN_ITEM  IN S_MATNR .

  LOOP AT IT_ZTPPES2.
    L_TABIX = SY-TABIX.
    CASE C_MARK.
      WHEN R_1.
        IT_ZTPPES2-EFLAG = 'IR'.
      WHEN R_2.
        IT_ZTPPES2-EFLAG = 'RP'.
      WHEN R_3.
        IT_ZTPPES2-EFLAG = 'DL'.
    ENDCASE.

    MODIFY IT_ZTPPES2 INDEX L_TABIX.
  ENDLOOP.
ENDFORM.                    " SELECT_ZTPPES
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  PERFORM DISPLAY_ZTPPES.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZTPPES
*&---------------------------------------------------------------------*
FORM MODIFY_ZTPPES.
  CLEAR : *IT_ZTPPES2, *IT_ZTPPES2[].
  CLEAR : *IT_ZTPPES, *IT_ZTPPES[].
  LOOP AT IT_ENG1.
    MOVE-CORRESPONDING IT_ENG1 TO *IT_ZTPPES.
    APPEND *IT_ZTPPES.
  ENDLOOP.
  MODIFY ZTPPES FROM TABLE *IT_ZTPPES.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  LOOP AT IT_ENG2.
    MOVE-CORRESPONDING IT_ENG2 TO *IT_ZTPPES2.
    APPEND *IT_ZTPPES2.
  ENDLOOP.
  MODIFY ZTPPES2 FROM TABLE *IT_ZTPPES2.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " MODIFY_ZTPPES
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ZTPPES
*&---------------------------------------------------------------------*
FORM DISPLAY_ZTPPES.
  DATA L_TABIX  TYPE  SY-TABIX .
  LOOP AT IT_ZTPPES2.
    L_TABIX = SY-TABIX.
    CLEAR : IT_ZTPPES2-ZMSG .
    MOVE-CORRESPONDING IT_ZTPPES2 TO IT_LIST.
    SELECT SINGLE MAKTX
               INTO IT_LIST-MAKTX
               FROM MAKT
               WHERE MATNR EQ IT_LIST-EN_ITEM
                 AND SPRAS EQ SY-LANGU .
    APPEND IT_LIST.
    MODIFY IT_ZTPPES2 INDEX L_TABIX.
  ENDLOOP.

ENDFORM.                    " DISPLAY_ZTPPES
*&---------------------------------------------------------------------*
*&      Form  LIST_PROCESS
*&---------------------------------------------------------------------*
FORM LIST_PROCESS.
  DESCRIBE TABLE IT_ZTPPES2 LINES Z_TOTAL.
  PERFORM BUILD_FIELDCAT.
  PERFORM BUILD_EVENT.
  PERFORM BUILD_SORT.
  PERFORM COMMENT_BUILD USING  W_TOP_OF_PAGE[].
  PERFORM CALL_FUNCTION.

ENDFORM.                    " LIST_PROCESS
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.

*   COL_POS, FIELDNAME, REF_FIELDNAME, KEY,
*   SELTEXT_L,                        OUTPUTLEN, NO_OUT
  APPEND_FIELDCAT :

    W_COL_POS 'EN_ITEM'   'EN_ITEM'   'X'
    'Material #'                     '18' '',
    W_COL_POS 'MAKTX'     'MAKTX'     'X'
    'Material Description'           '20' '',
    W_COL_POS 'EN_VEH_MODEL' 'EN_VEH_MODEL'  'X'
    'Model'                           '7' '',
** Furong on 01/25/12
    W_COL_POS 'PLANT_CD' 'PLANT_CD'  ' '
    'Plant CD'                        '8' '',
** end 01/25/12
    W_COL_POS 'EN_HEAD'   'EN_HEAD'    ''
    'Head.'                           '6' ''.

*----> APPEND EN_SPC01 ~ EN_SPC60
  PERFORM BUILD_EN_SPCNO .
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
*  W_SORTCAT-SPOS           = 1.
*  W_SORTCAT-FIELDNAME      = 'FLAG'.
*  W_SORTCAT-TABNAME        = 'IT_LIST'.
*  W_SORTCAT-UP             = 'X'.
*  APPEND W_SORTCAT.

  W_SORTCAT-SPOS           = 1.
  W_SORTCAT-FIELDNAME      = 'EN_ITEM'.
  W_SORTCAT-TABNAME        = 'IT_LIST'.
  W_SORTCAT-UP             = 'X'.
  APPEND W_SORTCAT.

*  W_SORTCAT-SPOS           = 2.
*  W_SORTCAT-FIELDNAME      = 'EN_VEH_MODEL'.
*  W_SORTCAT-TABNAME        = 'IT_LIST'.
*  W_SORTCAT-UP             = 'X'.
*  APPEND W_SORTCAT.

ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  COMMENT_BUILD
*&---------------------------------------------------------------------*
FORM COMMENT_BUILD USING LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.
  DATA: LS_LINE TYPE SLIS_LISTHEADER,
        L_LIST(50).

*----- Title
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = TEXT-A01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

**----- User
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = 'User: '.
*  LS_LINE-INFO = SY-UNAME.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Date
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = TEXT-A03 .
  WRITE SY-DATUM  TO  L_LIST .
  LS_LINE-INFO = L_LIST.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Total Count of Planning data
  DATA : L_LINES     TYPE   SY-TABIX  ,
         L_TEXT(13)  TYPE   C         .
  DESCRIBE TABLE IT_LIST  LINES L_LINES .
  WRITE L_LINES    TO    L_TEXT  LEFT-JUSTIFIED .
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = TEXT-A02.
  LS_LINE-INFO = L_TEXT.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  LS_LINE-TYP  = 'S' .
  LS_LINE-KEY  = '  '.
  LS_LINE-INFO = '  '.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

**----- Total
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
*&---------------------------------------------------------------------*
*&      Form  CALL_FUNCTION
*&---------------------------------------------------------------------*
FORM CALL_FUNCTION.
  DATA: L_PRINT_P TYPE SLIS_PRINT_ALV.  " print setting

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
ENDFORM.                    " EVENT_BUILD
*&---------------------------------------------------------------------*
*&      Form  CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
FORM CREATE_INTERFACE_LOG.
*  DESCRIBE TABLE IT_ZTPPES2 LINES Z_TOTAL.
*
*  SELECT COUNT(*) FROM ZTPPES2
*         INTO Z_SUCC
*         WHERE FLAG EQ 'S'
*           AND EN_ITEM  EQ P_MATNR
*           AND ZUSER EQ SY-UNAME
*           AND ZSDAT EQ SY-DATUM.
*
*  CHECK Z_TOTAL <> 0.
*  I_ZTCA_IF_LOG-TCODE    = 'ZPPI502'.
*  I_ZTCA_IF_LOG-TOTAL    = Z_TOTAL.
*  I_ZTCA_IF_LOG-ZSUCC    = Z_SUCC.
*  I_ZTCA_IF_LOG-ERROR    = Z_TOTAL - Z_SUCC.
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

ENDFORM.                    " CREATE_INTERFACE_LOG
*&-------------------------------------------------------------------
*&      Form  USER_COMMAND
*&-------------------------------------------------------------------
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE SLIS_SELFIELD.
  DATA : SEL_FIELD LIKE SELFIELD-SEL_TAB_FIELD.
  CASE UCOMM.
    WHEN '&RLE'.
      DESCRIBE TABLE IT_ZTPPES2 LINES Z_TOTAL.
** Added by Furong on 07/14/09
      PERFORM CHECK_DATA.
      IF W_CHECK IS INITIAL.
** End of addition
        PERFORM TRANSFER_PP_TO_MES.
        PERFORM MODIFY_ZTPPES.
        PERFORM CREATE_INTERFACE_LOG.
        PERFORM CALL_SCREEN_RESULT.
      ELSE.
        PERFORM DISPLAY_MESSAGE.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                               " USER_COMMAND1
*&---------------------------------------------------------------------*
*&      Form  TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
FORM TRANSFER_PP_TO_MES.
  DATA: L_MSGTXT(100),
        L_TABIX LIKE SY-TABIX.
  CLEAR : Z_SUCC.

  REFRESH: IT_ENG1, IT_ENG2.

** Changed by Furong on 11/21/08

*  LOOP AT IT_ZTPPES2.
*    IF IT_ZTPPES2-EN_SPC15 = 'ENG1'.
*      MOVE-CORRESPONDING  IT_ZTPPES2 TO IT_ENG1.
*      APPEND IT_ENG1.
*    ELSE.
*      MOVE-CORRESPONDING IT_ZTPPES2 TO IT_ENG2.
**      IT_ENG2-PLANT_CD = IT_ZTPPES2-EN_SPC15.
*      SELECT SINGLE MAKTX INTO IT_ENG2-ITEM_DESC
*      FROM MAKT
*      WHERE MATNR = IT_ZTPPES2-EN_ITEM
*       AND SPRAS = 'E'.
**      IT_ENG2-ENG_MDL_CD = IT_ZTPPES2-EN_ITEM+0(2).
*      APPEND IT_ENG2.
*    ENDIF.
*  ENDLOOP.

  LOOP AT IT_ZTPPES2.
    MOVE-CORRESPONDING IT_ZTPPES2 TO IT_ENG2.
*      IT_ENG2-PLANT_CD = IT_ZTPPES2-EN_SPC15.
    SELECT SINGLE MAKTX INTO IT_ENG2-ITEM_DESC
    FROM MAKT
    WHERE MATNR = IT_ZTPPES2-EN_ITEM
     AND SPRAS = 'E'.
*      IT_ENG2-ENG_MDL_CD = IT_ZTPPES2-EN_ITEM+0(2).
    APPEND IT_ENG2.
  ENDLOOP.

  IF NOT IT_ENG1[] IS INITIAL.
** Send ENG1 to interface
    CALL FUNCTION 'Z_FPP_ENGINE_PS'
      DESTINATION  C_DEST
      TABLES
        T_ZTPPES       = IT_ENG1
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

    IF SY-SUBRC NE 0.
      MESSAGE I001 WITH TEXT-302 .
      CASE C_MARK .
        WHEN R1  .   " Transfer
          LOOP AT IT_ENG1.
            L_TABIX = SY-TABIX.
            IT_ENG1-ZRESULT  = 'E' .
            IT_ENG1-ZUSER    = SY-UNAME.
            IT_ENG1-ZSDAT    = SY-DATUM.
            IT_ENG1-ZSTIM    = SY-UZEIT.
            IT_ENG1-ZMODE    = 'C'.
            IT_ENG1-ZMSG     = L_MSGTXT .
            MODIFY IT_ENG1 INDEX L_TABIX  .
          ENDLOOP.
        WHEN R2 .   " Re-transfer
          LOOP AT IT_ENG1.
            L_TABIX = SY-TABIX.
            IT_ENG1-ZRESULT  = 'E' .
            IT_ENG1-ZMSG     = L_MSGTXT .
            MODIFY IT_ENG1 INDEX L_TABIX  .
          ENDLOOP.
      ENDCASE.
    ELSE.
      CASE C_MARK .
        WHEN R1.    " Transfer
          LOOP AT IT_ENG1.
            L_TABIX = SY-TABIX.
            IF IT_ENG1-ZZRET = 'E'.
              IT_ENG1-ZRESULT  = 'E' .
              IT_ENG1-ZUSER    = SY-UNAME.
              IT_ENG1-ZSDAT    = SY-DATUM.
              IT_ENG1-ZSTIM    = SY-UZEIT.
              IT_ENG1-ZMODE    = 'C'.
              MODIFY IT_ENG1 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG1-ZRESULT  = 'S'.
              IT_ENG1-ZUSER    = SY-UNAME.
              IT_ENG1-ZSDAT    = SY-DATUM.
              IT_ENG1-ZSTIM    = SY-UZEIT.
              IT_ENG1-ZMODE    = 'C'.
              MODIFY IT_ENG1 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
        WHEN R2 .   "Re-transfer
          LOOP AT IT_ENG1 .
            L_TABIX = SY-TABIX.
            IF IT_ENG1-ZZRET   = 'E'.
              IT_ENG1-ZUSER    = SY-UNAME.
              IT_ENG1-ZRESULT  = 'E' .
              MODIFY IT_ENG1 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG1-ZRESULT  = 'S'.
              IT_ENG1-ZUSER    = SY-UNAME.
              MODIFY IT_ENG1 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
      ENDCASE .
    ENDIF.
  ENDIF.
** Send ENG2 to interface
  IF NOT IT_ENG2[] IS INITIAL.
    CALL FUNCTION 'Z_FPP_ENGINE_PS_2'
     DESTINATION  C_DEST
     TABLES
       T_ZTPPES       = IT_ENG2
     EXCEPTIONS
     COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
     SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

    IF SY-SUBRC NE 0.
      MESSAGE I001 WITH TEXT-302 .
      CASE C_MARK .
        WHEN R1  .   " Transfer
          LOOP AT IT_ENG2.
            L_TABIX = SY-TABIX.
            IT_ENG2-ZRESULT  = 'E' .
            IT_ENG2-ZUSER    = SY-UNAME.
            IT_ENG2-ZSDAT    = SY-DATUM.
            IT_ENG2-ZSTIM    = SY-UZEIT.
            IT_ENG2-ZMODE    = 'C'.
            IT_ENG2-ZMSG     = L_MSGTXT .
            MODIFY IT_ENG2 INDEX L_TABIX  .
          ENDLOOP.
        WHEN R2 .   " Re-transfer
          LOOP AT IT_ENG2.
            L_TABIX = SY-TABIX.
            IT_ENG2-ZRESULT  = 'E' .
            IT_ENG2-ZMSG     = L_MSGTXT .
            MODIFY IT_ENG2 INDEX L_TABIX  .
          ENDLOOP.
      ENDCASE.
    ELSE.
      CASE C_MARK .
        WHEN R1.    " Transfer
          LOOP AT IT_ENG2.
            L_TABIX = SY-TABIX.
            IF IT_ENG2-ZZRET = 'E'.
              IT_ENG2-ZRESULT  = 'E' .
              IT_ENG2-ZUSER    = SY-UNAME.
              IT_ENG2-ZSDAT    = SY-DATUM.
              IT_ENG2-ZSTIM    = SY-UZEIT.
              IT_ENG2-ZMODE    = 'C'.
              MODIFY IT_ENG2 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG2-ZRESULT  = 'S'.
              IT_ENG2-ZUSER    = SY-UNAME.
              IT_ENG2-ZSDAT    = SY-DATUM.
              IT_ENG2-ZSTIM    = SY-UZEIT.
              IT_ENG2-ZMODE    = 'C'.
              MODIFY IT_ENG2 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
        WHEN R2 .   "Re-transfer
          LOOP AT IT_ENG2 .
            L_TABIX = SY-TABIX.
            IF IT_ENG2-ZZRET   = 'E'.
              IT_ENG2-ZUSER    = SY-UNAME.
              IT_ENG2-ZRESULT  = 'E' .
              MODIFY IT_ENG2 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG2-ZRESULT  = 'S'.
              IT_ENG2-ZUSER    = SY-UNAME.
              MODIFY IT_ENG2 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
      ENDCASE .
    ENDIF.
  ENDIF.
  REFRESH IT_ZTPPES2.
  LOOP AT IT_ENG1.
    MOVE-CORRESPONDING IT_ENG1 TO IT_ZTPPES2.
    APPEND IT_ZTPPES2.
  ENDLOOP.
*  APPEND LINES OF IT_ENG1 TO IT_ZTPPES2.

  APPEND LINES OF IT_ENG2 TO IT_ZTPPES2.
ENDFORM.                    " TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
FORM CALL_SCREEN_RESULT.

  Z_FAIL = Z_TOTAL - Z_SUCC.

  CALL SCREEN 50 STARTING AT 20 10.

ENDFORM.                    " CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

* --- 2004.02.19 CHANGED ---- Mr. MooN
*&---------------------------------------------------------------------*
*&      Form  SELECT_MARC
*&---------------------------------------------------------------------*
FORM SELECT_MARC.
  CLEAR : IT_MARC ,  IT_MARC[] .
  SELECT MATNR
         FEVOR
         INTO TABLE IT_MARC
         FROM MARC
** for E002
*         WHERE WERKS EQ C_E001
         WHERE WERKS EQ p_werks
** end
           AND MATNR IN S_MATNR
           AND FEVOR IN (C_SEA, C_SEC).

  SORT IT_MARC BY FEVOR MATNR .
ENDFORM.                    " SELECT_MARC

*&---------------------------------------------------------------------*
*&      Form  READ_CLASSIFICATION
*&---------------------------------------------------------------------*
FORM READ_CLASSIFICATION.
  CLEAR : IT_ZTPPES2 , IT_ZTPPES2[].
  LOOP AT IT_MARC .
    CLEAR : IT_VM , IT_VM[] .
    MOVE IT_MARC-MATNR   TO  IT_ZTPPES2-EN_ITEM  .
*----> READ Classification
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        OBJECT             = IT_MARC-MATNR
        MODE               = 'R'
        CTYPE              = '001'
*        DISPLAY            = 'D'
      TABLES
        VAL_TABLE          = IT_VM
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        OTHERS             = 4 .
    IF SY-SUBRC <> 0.
    ELSE .
      CLEAR IT_VM .
      READ TABLE IT_VM WITH KEY ATNAM = 'EN_VEH_MODEL' .
      IT_ZTPPES2-EN_VEH_MODEL = IT_VM-ATWRT             .

      CLEAR IT_VM .
      READ TABLE IT_VM WITH KEY ATNAM = 'EN_HEAD'      .
      IT_ZTPPES2-EN_HEAD      = IT_VM-ATWRT             .

      PERFORM ASSIGN_EN_SPC  .
    ENDIF.

    CASE C_MARK.
      WHEN R_1.
        IT_ZTPPES2-EFLAG = 'IR'.
      WHEN R_2.
        IT_ZTPPES2-EFLAG = 'RP'.
      WHEN R_3.
        IT_ZTPPES2-EFLAG = 'DL'.
    ENDCASE.
** get EN_SPC15 OR EN_SPC01

** Furong on 01/24/12
*    IF IT_MARC-FEVOR EQ C_SEA.
*      READ TABLE IT_VM WITH KEY ATNAM = 'EN_SPC15'.
*    ELSE.
*      READ TABLE IT_VM WITH KEY ATNAM = 'EN_3CSPC01'.
*    ENDIF.
**    IT_ZTPPES2-EN_SPC15      = IT_VM-ATWRT.
*    IT_ZTPPES2-PLANT_CD      = IT_VM-ATWRT.
     IT_ZTPPES2-PLANT_CD = P_WERKS.
** end on 01/24/12

    IF IT_MARC-FEVOR EQ C_SEA.
      IT_ZTPPES2-ENG_MDL_CD = IT_MARC-MATNR+0(2).
    ELSE.
      READ TABLE IT_VM WITH KEY ATNAM = 'EN_3CSPC04'.
      IT_ZTPPES2-ENG_MDL_CD = IT_VM-ATWRT.
    ENDIF.

    APPEND IT_ZTPPES2 .    CLEAR IT_ZTPPES2 .

  ENDLOOP.

ENDFORM.                    " READ_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EN_SPC
*&---------------------------------------------------------------------*
FORM ASSIGN_EN_SPC.
  FIELD-SYMBOLS <LF_EN> .
  DATA : L_ENSPC(6)        TYPE   C   ,
         L_ENSPC_NO(8)     TYPE   C   ,
         L_EN_3CSPC(8)     TYPE   C   ,
         L_EN_3CSPC_NO(10)  TYPE   C   ,
         L_ZTPPES2(30)      TYPE   C   ,
         L_ZTPPES2_NO(40)   TYPE   C   ,
         L_NUM(2)          TYPE   N   .
  L_ENSPC    = 'EN_SPC'     .
  L_EN_3CSPC = 'EN_3CSPC'   .
  L_ZTPPES2   = 'IT_ZTPPES2-' .
  DO  60 TIMES  .
    CLEAR : L_ENSPC_NO  .
    L_NUM = SY-INDEX .
    CONCATENATE  L_ENSPC    L_NUM      INTO L_ENSPC_NO    .
    CONCATENATE  L_ZTPPES2   L_ENSPC_NO INTO L_ZTPPES2_NO   .
    CONCATENATE  L_EN_3CSPC L_NUM      INTO L_EN_3CSPC_NO .
    CASE IT_MARC-FEVOR .
      WHEN C_SEA .
        CLEAR IT_VM .
        READ TABLE IT_VM WITH KEY ATNAM = L_ENSPC_NO      .
        ASSIGN (L_ZTPPES2_NO)  TO  <LF_EN> .
        <LF_EN> = IT_VM-ATWRT .
      WHEN C_SEC .
        CLEAR IT_VM .
        READ TABLE IT_VM WITH KEY ATNAM = L_EN_3CSPC_NO   .
        ASSIGN (L_ZTPPES2_NO)  TO  <LF_EN> .
        <LF_EN> = IT_VM-ATWRT .
    ENDCASE.
  ENDDO .

ENDFORM.                    " ASSIGN_EN_SPC
*&---------------------------------------------------------------------*
*&      Form  BUILD_EN_SPCNO
*&---------------------------------------------------------------------*
FORM BUILD_EN_SPCNO.
  DATA : L_ENSPC(6)        TYPE   C   ,
         L_ENSPC_NO(8)     TYPE   C   ,
         L_SPC(3)          TYPE   C   ,
         L_SPC_NO(5)       TYPE   C   ,
         L_NUM(2)          TYPE   N   .
  L_ENSPC  = 'EN_SPC'     .
  L_SPC    = 'SPC'        .
  DO 60 TIMES .
    CLEAR : L_ENSPC_NO  .
    L_NUM = SY-INDEX .
    CONCATENATE  L_ENSPC  L_NUM      INTO  L_ENSPC_NO  .
    CONCATENATE  L_SPC    L_NUM      INTO  L_SPC_NO    .
    APPEND_FIELDCAT
      W_COL_POS L_ENSPC_NO  L_ENSPC_NO  ''
      L_SPC_NO                        '6'  ''.
  ENDDO.
ENDFORM.                    " BUILD_EN_SPCNO
*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATA.
  DATA: L_FEVOR LIKE MARC-FEVOR.

  CLEAR: W_CHECK, W_EN_ITEM, W_TEXT_SPC10, W_TEXT_SPC14, W_TEXT_SPC02,
         W_TEXT_SPC03.

  LOOP AT IT_ZTPPES2.
    SELECT SINGLE FEVOR INTO L_FEVOR
      FROM MARC
** for E002
*      WHERE WERKS EQ C_E001
      WHERE WERKS EQ P_WERKS
** end
        AND MATNR = IT_ZTPPES2-EN_ITEM.

    IF L_FEVOR EQ C_SEA.
      IF IT_ZTPPES2-EN_SPC10 IS INITIAL.
        W_CHECK = 'E'.
        W_EN_ITEM = IT_ZTPPES2-EN_ITEM.
   W_TEXT_SPC10 = 'The value for SPEC10 (EN_SPC10: Spec for A/S Parts)'
                                                 .
      ENDIF.
      IF IT_ZTPPES2-EN_SPC14 IS INITIAL.
        W_CHECK = 'E'.
        W_EN_ITEM = IT_ZTPPES2-EN_ITEM.
        W_TEXT_SPC14 = 'SPEC14 (EN_SPC14: Workcenter)'.
      ENDIF.
    ELSE.
      IF IT_ZTPPES2-EN_SPC02 IS INITIAL.
        W_CHECK = 'C'.
        W_EN_ITEM = IT_ZTPPES2-EN_ITEM.
        W_TEXT_SPC02 = 'The value for SPEC02 (EN_3CSPC02: workcenter)'.
      ENDIF.
      IF IT_ZTPPES2-EN_SPC03 IS INITIAL.
        W_CHECK = 'C'.
        W_EN_ITEM = IT_ZTPPES2-EN_ITEM.
        W_TEXT_SPC03 = 'SPEC03 (EN_3CSPC03: HMMA 3C SPEC 03)'.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " CHECK_DATA
*&---------------------------------------------------------------------*
*&      Form  display_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_MESSAGE.
  DATA: L_TEXT1(70),
        L_TEXT2(70),
        L_TEXT3(70).

  L_TEXT3 = 'Please update Material Master and try again'.

  CASE W_CHECK.
    WHEN 'E'.
      IF W_TEXT_SPC10 IS INITIAL.
        CONCATENATE W_TEXT_SPC14 'field is missing' INTO L_TEXT1
          SEPARATED BY SPACE.
        CLEAR: L_TEXT2.
      ELSE.
        L_TEXT1 = W_TEXT_SPC10.
        IF W_TEXT_SPC14 IS INITIAL.
          CONCATENATE L_TEXT1 'field is missing' INTO L_TEXT1
        SEPARATED BY SPACE.
        ELSE.
          CONCATENATE L_TEXT1 'and' INTO L_TEXT1 SEPARATED BY SPACE.
          CONCATENATE W_TEXT_SPC14 'fields are missing' INTO L_TEXT2
            SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
    WHEN 'C'.
      IF W_TEXT_SPC02 IS INITIAL.
        CONCATENATE W_TEXT_SPC03 'field is missing' INTO L_TEXT1
          SEPARATED BY SPACE.
        CLEAR: L_TEXT2.
      ELSE.
        L_TEXT1 = W_TEXT_SPC02.
        IF W_TEXT_SPC03 IS INITIAL.
          CONCATENATE L_TEXT1 'field is missing' INTO L_TEXT1
        SEPARATED BY SPACE.
        ELSE.
          CONCATENATE L_TEXT1 'and' INTO L_TEXT1 SEPARATED BY SPACE.
          CONCATENATE W_TEXT_SPC03 'fields are missing' INTO L_TEXT2
            SEPARATED BY SPACE.
        ENDIF.
      ENDIF.
  ENDCASE.
  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      TITEL         = 'Message'
      TXT1          = L_TEXT1
      TXT2          = L_TEXT2
      TXT3          = L_TEXT3
*   TXT4          = ' '
            .

ENDFORM.                    " display_message
