*----------------------------------------------------------------------*
*   INCLUDE ZIPP501L_ENGIN_PP_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  S_PDATE-SIGN   = 'I'.
  S_PDATE-OPTION = 'EQ'.
  S_PDATE-LOW    = SY-DATUM.
  APPEND S_PDATE .
ENDFORM.                    " INITIALIZATION

*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
*  LOOP AT SCREEN.
*    IF R1 EQ 'X'.
*      IF SCREEN-GROUP1 = 'ABC'.
*        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.

*  REFRESH S_PDATE.
*  S_PDATE-LOW = S_PDATE-LOW + 1.
*  IF S_PDATE-HIGH EQ '00000000'.
*    S_PDATE-HIGH = S_PDATE-LOW + 31.
*  ENDIF.
*  S_PDATE-OPTION = 'BT'.
*  APPEND S_PDATE.
  CASE C_MARK.
    WHEN R1 .    " Transfer
      IF S_PDATE-LOW LT SY-DATUM .
        MESSAGE E001 WITH TEXT-301.
      ENDIF.
    WHEN R2 .    " Retransfer

  ENDCASE.
ENDFORM.                    " AT_SELECTION-SCREEN

*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
*----> Condition of search date
  IF S_PDATE-HIGH IS INITIAL.
    S_DATE-LOW    = S_PDATE-LOW.
    S_DATE-HIGH   = S_PDATE-LOW + 30.
    S_DATE-SIGN   = 'I'.
    S_DATE-OPTION = 'BT'.
    APPEND S_DATE.
  ELSE.
    S_DATE-LOW    = S_PDATE-LOW.
    S_DATE-HIGH   = S_PDATE-HIGH.
    S_DATE-SIGN   = 'I'.
    S_DATE-OPTION = 'BT'.
    APPEND S_DATE.
  ENDIF.

  CASE C_MARK.
    WHEN R1.
*----> Transfer
      PERFORM SELECT_PLAF.
    WHEN R2.
*----> Re-transfer
      PERFORM SELECT_ZTPPEP.
  ENDCASE.
  PERFORM GET_ENG_PLANT.
ENDFORM.                    " READ_PROCESS

*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.

  PERFORM DISPLAY_ZTPPEP.

ENDFORM.                    " DATA_PROCESS
*&---------------------------------------------------------------------*
*&      Form  SELECT_PLAF
*&---------------------------------------------------------------------*
FORM SELECT_PLAF.

*----> Conditon of Production scheduler
  CLEAR : R_PLGRP, R_PLGRP[].
  R_PLGRP-SIGN   = 'I'.
  R_PLGRP-OPTION = 'EQ'.
  R_PLGRP-LOW    = C_SEA .
  APPEND R_PLGRP.

  R_PLGRP-LOW    = C_SEC .
  APPEND R_PLGRP.

*----> SELECT PLAF
*  SELECT PLNUM       "Planned order number
*         PSTTR       "Order start date in planned order
*         MATNR       "Planning material
*         GSMNG       "Total planned order quantity
*         MEINS       "Base unit of measure
*         PLGRP       "Production scheduler
*         INTO TABLE IT_PLAF
*         FROM PLAF
*         WHERE PWWRK EQ C_E001  "Plant for planning purposes
*           AND BESKZ EQ 'E'      "Procurement Type
*           AND DISPO EQ C_ME1   "MRP controller
*           AND PLSCN EQ SPACE   "Planning scenario in long-term
*planning
*           AND PLGRP IN R_PLGRP "ProdScheduler ( 'SEA' or 'SEC' )
*           AND PSTTR IN S_DATE  "Order start date in planned order
*           AND PLNUM IN S_PLNUM "Planned order number
*           AND MATNR IN S_MATNR. "Planning material

  SELECT  PLNUM       "Planned order number
          PSTTR       "Order start date in planned order
          A~MATNR       "Planning material
          GSMNG       "Total planned order quantity
          MEINS       "Base unit of measure
          PLGRP       "Production scheduler
          INTO TABLE IT_PLAF
          FROM PLAF AS A
          INNER JOIN MARC AS B
          ON A~MATNR = B~MATNR
          AND A~PWWRK = B~WERKS
** for E002
*          WHERE PWWRK EQ C_E001  "Plant for planning purposes
          WHERE PWWRK EQ p_werks  "Plant for planning purposes
** End
            AND A~BESKZ EQ 'E'      "Procurement Type
            AND A~DISPO EQ C_ME1   "MRP controller
           AND PLSCN EQ SPACE   "Planning scenario in long-term planning
            AND PLGRP IN R_PLGRP "ProdScheduler ( 'SEA' or 'SEC' )
            AND PSTTR IN S_DATE  "Order start date in planned order
            AND PLNUM IN S_PLNUM "Planned order number
            AND A~MATNR IN S_MATNR "Planning material
            AND B~MMSTA = '12'.  " Plant sp matl stats
*----> Choose only new data and cut off old data tranfered
*----> On the line cancelled - 2004.02.19 - Mr. Moon
  LOOP AT IT_PLAF.
    MOVE-CORRESPONDING IT_PLAF TO *IT_PLAF.
    APPEND *IT_PLAF.
  ENDLOOP.

  SORT *IT_PLAF BY PSTTR MATNR.

  PERFORM MOVE_PLAF_TO_ITAB.

ENDFORM.                    " SELECT_PLAF
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTPPEP
*&---------------------------------------------------------------------*
FORM SELECT_ZTPPEP.
  DATA: L_TABIX LIKE SY-TABIX.

  SELECT * FROM ZTPPEP2
           INTO TABLE IT_ZTPPEP2
           WHERE PDATE IN S_PDATE
             AND PLNUM IN S_PLNUM
             AND PITEM IN S_MATNR .

  SELECT * FROM ZTPPEP
           APPENDING TABLE IT_ZTPPEP2
           WHERE PDATE IN S_PDATE
             AND PLNUM IN S_PLNUM
             AND PITEM IN S_MATNR .

  LOOP AT IT_ZTPPEP2.
    L_TABIX = SY-TABIX.
    CASE C_MARK.
      WHEN R_1.
        IT_ZTPPEP2-EFLAG = 'IR'.
      WHEN R_2.
        IT_ZTPPEP2-EFLAG = 'RP'.
      WHEN R_3.
        IT_ZTPPEP2-EFLAG = 'DL'.
    ENDCASE.
    MODIFY IT_ZTPPEP2 INDEX L_TABIX.
  ENDLOOP.

ENDFORM.                    " SELECT_ZTPPEP
*&---------------------------------------------------------------------*
*&      Form  MOVE_PLAF_TO_ITAB
*&---------------------------------------------------------------------*
FORM MOVE_PLAF_TO_ITAB.
  LOOP AT *IT_PLAF.
    IT_ZTPPEP2-PLNUM = *IT_PLAF-PLNUM.
    IT_ZTPPEP2-PDATE = *IT_PLAF-PSTTR.
    IT_ZTPPEP2-PITEM = *IT_PLAF-MATNR.
    IT_ZTPPEP2-GSMNG = *IT_PLAF-GSMNG.
    IT_ZTPPEP2-MEINS = *IT_PLAF-MEINS.
    IF *IT_PLAF-PLGRP EQ C_SEA.
      IT_ZTPPEP2-PLGRP = C_SEA+2(1).
    ELSEIF *IT_PLAF-PLGRP EQ C_SEC.
      IT_ZTPPEP2-PLGRP = C_SEC+2(1).
    ENDIF.
    IF R_1 EQ 'X'.
      IT_ZTPPEP2-EFLAG = 'IR'.
    ELSEIF R_2 EQ 'X'.
      IT_ZTPPEP2-EFLAG = 'RP'.
    ELSEIF R_3 EQ 'X'.
      IT_ZTPPEP2-EFLAG = 'DL'.
    ENDIF.

    APPEND IT_ZTPPEP2.
  ENDLOOP.

ENDFORM.                    " MOVE_PLAF_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ZTPPEP
*&---------------------------------------------------------------------*
FORM DISPLAY_ZTPPEP.
  LOOP AT IT_ZTPPEP2_ENG.
    MOVE-CORRESPONDING IT_ZTPPEP2_ENG TO IT_LIST.
*----> Display Description
    SELECT SINGLE MAKTX
                INTO IT_LIST-MAKTX
                FROM MAKT
                WHERE MATNR EQ IT_LIST-PITEM
                  AND SPRAS EQ SY-LANGU .
    APPEND IT_LIST.
  ENDLOOP.
ENDFORM.                    " DISPLAY_ZTPPEP
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
*  DESCRIBE TABLE IT_ZTPPEP2 LINES Z_TOTAL.
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
*  APPEND_FIELDCAT :


*   postion    field    ref field     key q-ref   c-ref
*   Text                len
  PERFORM APPEND_FIELDCAT USING :

*    W_COL_POS 'FLAG'    'FLAG'        'X'  ''      ''
*    'LOG'                             '3'  '',
    W_COL_POS 'EN_SPC15'  'EN_SPC15'   'X'  ''      ''
   'Eng Plant'                         '5' '',

    W_COL_POS 'PLNUM'   'PLNUM'       'X'  ''      ''
   'PldOrd #'                         '10' '',
   W_COL_POS  'PDATE'   'PDATE'       'X'  ''      ''
   'Plan Date'                        '10'  '',
*    W_COL_POS 'PLNUM'   'PLNUM'       'X'  ''      ''
*    'P/O No.'                         '10' '',
    W_COL_POS 'PITEM'   'PITEM'       ''   ''      ''
    'Material #'                      '18' '',
    W_COL_POS 'MAKTX'   'MAKTX'       ''   ''      ''
    'Material Description'            '20' '',
    W_COL_POS 'GSMNG'   'GSMNG'       ' '  ''      ''
    'Qty'                             '17' 'EA',
    W_COL_POS 'MEINS'   'MEINS'       ' '  ''      ''
    'UNIT'                            '4'  '',
    W_COL_POS 'PLGRP'   'PLGRP'       ' '  ''      ''
    'TYPE'                            '4'  ''.
*    W_COL_POS 'ZUSER'   'ZUSER'       ' '  ''      ''
*    'Creator'                         '10' '',
*    W_COL_POS 'ZEDAT'   'ZEDAT'       ' '  ''      ''
*    'I/F Date'                        '10' '',
*    W_COL_POS 'ZETIM'   'ZETIM'       ' '   ''      ''
*    'I/F Time'                        '10' '',
*    W_COL_POS 'ZMSG'    'ZMSG'        ' '   ''      ''
*    'Message'                        '50' ''.
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
  W_SORTCAT-FIELDNAME      = 'PDATE'.
  W_SORTCAT-TABNAME        = 'IT_LIST'.
  W_SORTCAT-UP             = 'X'.
  APPEND W_SORTCAT.

  W_SORTCAT-SPOS           = 2.
  W_SORTCAT-FIELDNAME      = 'PLNUM'.
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

**----- User
*  LS_LINE-TYP  = 'S'.
*  LS_LINE-KEY  = TEXT-A02.
*  LS_LINE-INFO = SY-UNAME.
*  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Date
  IF NOT S_PDATE-LOW IS INITIAL.
    LS_LINE-TYP  = 'S'.
    LS_LINE-KEY  = TEXT-A03.   "Date :
    IF NOT S_PDATE-HIGH IS INITIAL .
      PERFORM USER_DATE USING    S_PDATE-LOW
                        CHANGING L_DATE1.
      PERFORM USER_DATE USING    S_PDATE-HIGH
                        CHANGING L_DATE2.
      CONCATENATE TEXT-A04  L_DATE1 ',' TEXT-A05 L_DATE2 INTO L_LIST
                  SEPARATED BY SPACE .
    ELSE.
      PERFORM USER_DATE USING    S_PDATE-LOW
                        CHANGING L_DATE1.
      L_LIST = L_DATE1.
    ENDIF.
    LS_LINE-INFO = L_LIST.
    APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

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
*&      Form  MODIFY_ZTPPEP
*&---------------------------------------------------------------------*
FORM MODIFY_ZTPPEP.
  CLEAR : *IT_ZTPPEP2, *IT_ZTPPEP2[].
  CLEAR : *IT_ZTPPEP, *IT_ZTPPEP[].

  LOOP AT IT_ENG1.
    MOVE-CORRESPONDING IT_ENG1 TO *IT_ZTPPEP.
    APPEND *IT_ZTPPEP.
*    IF *IT_ZTPPEP-FLAG EQ 'S'.
*      PERFORM DELTE_PRDORD_LOGIC.
*    ENDIF.
  ENDLOOP.
  MODIFY ZTPPEP FROM TABLE *IT_ZTPPEP.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

  LOOP AT IT_ENG2.
    MOVE-CORRESPONDING IT_ENG2 TO *IT_ZTPPEP2.
    APPEND *IT_ZTPPEP2.
*    IF *IT_ZTPPEP2-FLAG EQ 'S'.
*      PERFORM DELTE_PRDORD_LOGIC.
*    ENDIF.
  ENDLOOP.
  MODIFY ZTPPEP2 FROM TABLE *IT_ZTPPEP2.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " MODIFY_ZTPPEP
*&-------------------------------------------------------------------
*&      Form  USER_COMMAND
*&-------------------------------------------------------------------
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE SLIS_SELFIELD.
  DATA : SEL_FIELD LIKE SELFIELD-SEL_TAB_FIELD.

  CASE UCOMM.
    WHEN '&REL'.
      DESCRIBE TABLE IT_ZTPPEP2 LINES Z_TOTAL.
      PERFORM TRANSFER_PP_TO_MES.
      PERFORM MODIFY_ZTPPEP.
      PERFORM CREATE_INTERFACE_LOG.
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

  REFRESH: IT_ENG1, IT_ENG2.
  LOOP AT IT_ZTPPEP2_ENG.
    IF IT_ZTPPEP2_ENG-EN_SPC15 = 'ENG1'.
      MOVE-CORRESPONDING IT_ZTPPEP2_ENG TO IT_ENG1.
      APPEND IT_ENG1.
    ELSE.
      MOVE-CORRESPONDING IT_ZTPPEP2_ENG TO IT_ENG2.
      APPEND IT_ENG2.
    ENDIF.
  ENDLOOP.

** Send ENG1 to interface
  IF NOT IT_ENG1[] IS INITIAL.
    CALL FUNCTION 'Z_FPP_ENGINE_PP'
      DESTINATION  C_DEST
      TABLES
        T_ZTPPEP       = IT_ENG1
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

    CLEAR Z_SUCC .
    IF SY-SUBRC NE 0 .
      MESSAGE I001 WITH TEXT-302 .
      CASE C_MARK.
        WHEN R1 .    " Transfer
          DELETE FROM ZTPPEP WHERE PDATE IN S_DATE .
          LOOP AT IT_ENG1 .
            L_TABIX = SY-TABIX.
*          IT_ENG1-FLAG = 'E'.
            IT_ENG1-ZRESULT = 'E'      .
            IT_ENG1-ZMSG    = L_MSGTXT .
            IT_ENG1-ZUSER   = SY-UNAME .
            IT_ENG1-ZSDAT   = SY-DATUM .
            IT_ENG1-ZSTIM   = SY-UZEIT .
            MODIFY IT_ENG1 INDEX L_TABIX .
          ENDLOOP.
        WHEN R2 .    " Re-transfer
          DELETE FROM ZTPPEP2 WHERE PDATE IN S_PDATE .
          LOOP AT IT_ENG1 .
            L_TABIX = SY-TABIX .
            IT_ENG1-ZRESULT = 'E'      .
            IT_ENG1-ZMSG    = L_MSGTXT .
            MODIFY IT_ENG1 INDEX L_TABIX .
          ENDLOOP.
      ENDCASE .

    ELSE.
      CASE C_MARK.
        WHEN R1.    " Transfer
          DELETE FROM ZTPPEP WHERE PDATE IN S_DATE .
          LOOP AT IT_ENG1.
            L_TABIX = SY-TABIX.
            IF IT_ENG1-ZZRET  = 'E'.
              IT_ENG1-ZRESULT = 'E'.
              IT_ENG1-ZUSER   = SY-UNAME.
              IT_ENG1-ZSDAT   = SY-DATUM.
              IT_ENG1-ZSTIM   = SY-UZEIT.
              IT_ENG1-ZMODE   = 'C'.
              MODIFY IT_ENG1 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG1-ZRESULT = 'S' .  "IT_ENG1-ZZRET.
              IT_ENG1-ZUSER   = SY-UNAME.
              IT_ENG1-ZSDAT   = SY-DATUM.
              IT_ENG1-ZSTIM   = SY-UZEIT.
              IT_ENG1-ZMODE   = 'C'.
              MODIFY IT_ENG1 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
        WHEN R2 .    " Re-transfer
          DELETE FROM ZTPPEP WHERE PDATE IN S_PDATE .
          LOOP AT IT_ENG1.
            L_TABIX  = SY-TABIX.
            CLEAR IT_ENG1-ZMSG .
            IF IT_ENG1-ZZRET  = 'E'.
              IT_ENG1-ZRESULT = 'E'. "IT_ENG1-ZZRET.
              MODIFY IT_ENG1 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG1-ZRESULT = 'S' .  "IT_ENG1-ZZRET.
              MODIFY IT_ENG1 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.
  ENDIF.

** Send ENG2 to interface
  IF NOT IT_ENG2[] IS INITIAL.
    CALL FUNCTION 'Z_FPP_ENGINE_PP_2'
      DESTINATION  C_DEST
      TABLES
        T_ZTPPEP       = IT_ENG2
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
      SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

    CLEAR Z_SUCC .
    IF SY-SUBRC NE 0 .
      MESSAGE I001 WITH TEXT-302 .
      CASE C_MARK.
        WHEN R1 .    " Transfer
          DELETE FROM ZTPPEP2 WHERE PDATE IN S_DATE .
          LOOP AT IT_ENG2 .
            L_TABIX = SY-TABIX.
*          IT_ENG2-FLAG = 'E'.
            IT_ENG2-ZRESULT = 'E'      .
            IT_ENG2-ZMSG    = L_MSGTXT .
            IT_ENG2-ZUSER   = SY-UNAME .
            IT_ENG2-ZSDAT   = SY-DATUM .
            IT_ENG2-ZSTIM   = SY-UZEIT .
            MODIFY IT_ENG2 INDEX L_TABIX .
          ENDLOOP.
        WHEN R2 .    " Re-transfer
          DELETE FROM ZTPPEP2 WHERE PDATE IN S_PDATE .
          LOOP AT IT_ENG2 .
            L_TABIX = SY-TABIX .
            IT_ENG2-ZRESULT = 'E'      .
            IT_ENG2-ZMSG    = L_MSGTXT .
            MODIFY IT_ENG2 INDEX L_TABIX .
          ENDLOOP.
      ENDCASE .

    ELSE.
      CASE C_MARK.
        WHEN R1.    " Transfer
          DELETE FROM ZTPPEP2 WHERE PDATE IN S_DATE .
          LOOP AT IT_ENG2.
            L_TABIX = SY-TABIX.
            IF IT_ENG2-ZZRET  = 'E'.
              IT_ENG2-ZRESULT = 'E'.
              IT_ENG2-ZUSER   = SY-UNAME.
              IT_ENG2-ZSDAT   = SY-DATUM.
              IT_ENG2-ZSTIM   = SY-UZEIT.
              IT_ENG2-ZMODE   = 'C'.
              MODIFY IT_ENG2 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG2-ZRESULT = 'S' .  "IT_ENG2-ZZRET.
              IT_ENG2-ZUSER   = SY-UNAME.
              IT_ENG2-ZSDAT   = SY-DATUM.
              IT_ENG2-ZSTIM   = SY-UZEIT.
              IT_ENG2-ZMODE   = 'C'.
              MODIFY IT_ENG2 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
        WHEN R2 .    " Re-transfer
          DELETE FROM ZTPPEP2 WHERE PDATE IN S_PDATE .
          LOOP AT IT_ENG2.
            L_TABIX  = SY-TABIX.
            CLEAR IT_ENG2-ZMSG .
            IF IT_ENG2-ZZRET  = 'E'.
              IT_ENG2-ZRESULT = 'E'. "IT_ENG2-ZZRET.
              MODIFY IT_ENG2 INDEX L_TABIX.
            ELSE.
              Z_SUCC = Z_SUCC + 1.
              IT_ENG2-ZRESULT = 'S' .  "IT_ENG2-ZZRET.
              MODIFY IT_ENG2 INDEX L_TABIX.
            ENDIF.
          ENDLOOP.
      ENDCASE.
    ENDIF.
  ENDIF.

  REFRESH IT_ZTPPEP2.
  LOOP AT IT_ENG1.
    MOVE-CORRESPONDING IT_ENG1 TO IT_ZTPPEP2.
    APPEND IT_ZTPPEP2.
  ENDLOOP.

*  APPEND LINES OF IT_ENG1 TO IT_ZTPPEP2.
  APPEND LINES OF IT_ENG2 TO IT_ZTPPEP2.

ENDFORM.                    " TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
*&      Form  USER_DATE
*&---------------------------------------------------------------------*
FORM USER_DATE USING    P_DATE
               CHANGING P_L_DATE1.

  DATA: L_ORIGINAL_DATE TYPE D.

  L_ORIGINAL_DATE = P_DATE.

  CALL 'DATE_CONV_INT_TO_EXT'
  ID 'DATINT' FIELD L_ORIGINAL_DATE
  ID 'DATEXT' FIELD P_L_DATE1.

ENDFORM.                    " USER_DATE
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
FORM CALL_SCREEN_RESULT.
  DATA: L_FAIL TYPE I.

*  SELECT COUNT(*) FROM ZTPPEP2
*         INTO Z_SUCC
*         WHERE FLAG EQ 'S'
*           AND PDATE IN S_DATE .
*           AND ZUSER EQ SY-UNAME .
*           AND ZSDAT EQ SY-DATUM.

  Z_FAIL = Z_TOTAL - Z_SUCC.

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
      LEAVE TO SCREEN 0 .
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*&      Form  DELTE_PRDORD_LOGIC
*&---------------------------------------------------------------------*
FORM DELTE_PRDORD_LOGIC.
  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPMM61P'        '0101',
                 ' '  'BDC_OKCODE'      '/00'   ,
                 ' '  'RM61P-PLNUM'     *IT_ZTPPEP2-PLNUM,

                 'X'  'SAPLM61O'       '0110',
                 ' '  'BDC_OKCODE'     '=DLPL'.

  CALL TRANSACTION 'MD12' USING IT_BDCDATA MODE P_MODE
                          MESSAGES INTO IT_MSG.

ENDFORM.                    " DELTE_PRDORD_LOGIC
*&--------------------------------------------------------------------
*&      Form  BDC_OPEN_GROUP
*&--------------------------------------------------------------------
FORM BDC_OPEN_GROUP.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = SY-MANDT
            GROUP               = WA_BDCGROUP
            KEEP                = 'X'
            USER                = SY-UNAME
       EXCEPTIONS
            CLIENT_INVALID      = 1
            DESTINATION_INVALID = 2
            GROUP_INVALID       = 3
            GROUP_IS_LOCKED     = 4
            HOLDDATE_INVALID    = 5
            INTERNAL_ERROR      = 6
            QUEUE_ERROR         = 7
            RUNNING             = 8
            SYSTEM_LOCK_ERROR   = 9
            USER_INVALID        = 10
            OTHERS              = 11.

  IF SY-SUBRC <> 0.
    WRITE: /, ' Error BDC Opening Group: ', SY-UZEIT,
           /, ' Return Code: ', SY-SUBRC.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_OPEN_GROUP

*&--------------------------------------------------------------------
*&      Form  BDC_DYNPRO_PROCESSING
*&--------------------------------------------------------------------
FORM BDC_DYNPRO_PROCESSING USING DY_BEGIN
                                 PG_NAME
                                 SC_NO.
  IF DY_BEGIN = 'X'.
    CLEAR IT_BDCDATA.
    MOVE  PG_NAME  TO IT_BDCDATA-PROGRAM.
    MOVE  SC_NO    TO IT_BDCDATA-DYNPRO.
    MOVE  'X'      TO IT_BDCDATA-DYNBEGIN.
    APPEND IT_BDCDATA.
  ELSE.
    CLEAR IT_BDCDATA.
    MOVE  PG_NAME  TO IT_BDCDATA-FNAM.
    MOVE  SC_NO    TO IT_BDCDATA-FVAL.
    APPEND IT_BDCDATA.
  ENDIF.
ENDFORM.                    " BDC_DYNPRO_PROCESSING.

*&--------------------------------------------------------------------
*&      Form  BDC_INSERT_TRANSACTION
*&--------------------------------------------------------------------
FORM BDC_INSERT_TRANSACTION.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE            = P_TCODE
       TABLES
            DYNPROTAB        = IT_BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR   = 1
            NOT_OPEN         = 2
            QUEUE_ERROR      = 3
            TCODE_INVALID    = 4
            PRINTING_INVALID = 5
            POSTING_INVALID  = 6
            OTHERS           = 7.

  IF SY-SUBRC <> 0.
    WRITE: /, ' Error BDC Insert: ', SY-UZEIT,
           /, ' Return Code: ', SY-SUBRC.
    LEAVE.
  ENDIF.
ENDFORM.                    " BDC_INSERT_TRANSACTION

*&--------------------------------------------------------------------
*&      Form  BDC_CLOSE_GROUP
*&--------------------------------------------------------------------
FORM BDC_CLOSE_GROUP.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.

  IF SY-SUBRC <> 0.
    WRITE: /, ' Error BDC Close: ', SY-UZEIT,
           /, ' Return Code: ', SY-SUBRC.
    EXIT.
  ENDIF.
ENDFORM.                    " BDC_CLOSE_GROUP
*&---------------------------------------------------------------------*
*&      Form  APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_FIELDCAT USING
   P_POSITION  P_FIELD   P_REF_FIELD    P_KEY  P_QREF  P_CREF
   P_TEXT     P_LEN   P_MEINS.
  P_POSITION = P_POSITION + 1.
  W_FIELDCAT-COL_POS       = P_POSITION.
  W_FIELDCAT-FIELDNAME     = P_FIELD.
  W_FIELDCAT-REF_FIELDNAME = P_REF_FIELD.
  W_FIELDCAT-KEY           = P_KEY.
  W_FIELDCAT-QFIELDNAME    = P_QREF.
  W_FIELDCAT-CFIELDNAME    = P_CREF.
  W_FIELDCAT-QUANTITY      = P_MEINS.
  W_FIELDCAT-SELTEXT_L     = P_TEXT.
  W_FIELDCAT-SELTEXT_M     = P_TEXT.
  W_FIELDCAT-SELTEXT_S     = P_TEXT.
  W_FIELDCAT-OUTPUTLEN     = P_LEN.
*    W_FIELDCAT-NO_OUT        = .
  APPEND W_FIELDCAT.
  CLEAR : W_FIELDCAT.
ENDFORM.                    " APPEND_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  get_eng_plant
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ENG_PLANT.
  DATA : IT_VM   LIKE TABLE OF ZSPP_VIN_VALUE WITH HEADER LINE.



  LOOP AT IT_ZTPPEP2.
    IF IT_ZTPPEP2-PLGRP = 'A'.
      IT_VM-ATNAM = 'EN_SPC15'.
    ELSE.
      IT_VM-ATNAM = 'EN_3CSPC01'.
    ENDIF.
    APPEND IT_VM.
    MOVE-CORRESPONDING IT_ZTPPEP2 TO IT_ZTPPEP2_ENG.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
      EXPORTING
        OBJECT             = IT_ZTPPEP2-PITEM
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
*      READ TABLE IT_VM WITH KEY ATNAM = 'EN_SPC15'.
*      IT_ZTPPEP2_ENG-EN_SPC15  = IT_VM-ATWRT.
      READ TABLE IT_VM INDEX 1.
      IT_ZTPPEP2_ENG-PLANT_CD = IT_VM-ATWRT.
      IT_ZTPPEP2_ENG-EN_SPC15 = IT_VM-ATWRT.
    ENDIF.
    APPEND IT_ZTPPEP2_ENG.
*    READ TABLE IT_VM INDEX 1.
*    CLEAR: IT_VM-ATWRT.
*    modify IT_VM INDEX 1.
    REFRESH IT_VM.
    CLEAR: IT_VM.
  ENDLOOP.

ENDFORM.                    " get_eng_plant
