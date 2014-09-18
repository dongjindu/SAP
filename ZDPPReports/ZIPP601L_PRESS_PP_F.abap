*----------------------------------------------------------------------*
*   INCLUDE ZIPP601L_PRESS_PP_F                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
FORM INITIALIZATION.
  S_GSTRP-SIGN   =  'I'.
  S_GSTRP-OPTION =  'EQ'.
  S_GSTRP-LOW    =  SY-DATUM.
  APPEND S_GSTRP.
ENDFORM.                    " INITIALIZATION
*&---------------------------------------------------------------------*
*&      Form  AT_SELECTION-SCREEN
*----------------------------------------------------------------------*
FORM AT_SELECTION-SCREEN.
*  LOOP AT SCREEN.
*    IF R1 EQ 'X'.
*      IF SCREEN-GROUP1 = 'ABC'.
*        SCREEN-INPUT = '0'.
*        SCREEN-INVISIBLE = '1'.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDIF.
*    IF SCREEN-NAME = 'P_PWERK'.
*      SCREEN-INPUT = '0'.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

  IF S_GSTRP-LOW LT SY-DATUM.
    MESSAGE E001 WITH TEXT-300.
  ENDIF.
ENDFORM.                    " AT_SELECTION-SCREEN
*&---------------------------------------------------------------------*
*&      Form  READ_PROCESS
*&---------------------------------------------------------------------*
FORM READ_PROCESS.
  IF S_GSTRP-HIGH IS INITIAL.
    S_DATE-LOW  = S_GSTRP-LOW.
    S_DATE-HIGH = S_GSTRP-LOW + 30.
    S_DATE-SIGN = 'I'.
    S_DATE-OPTION = 'BT'.
    APPEND S_DATE.
  ELSE.
    S_DATE-LOW  = S_GSTRP-LOW.
    S_DATE-HIGH = S_GSTRP-HIGH.
    S_DATE-SIGN = 'I'.
    S_DATE-OPTION = 'BT'.
    APPEND S_DATE.
  ENDIF.

  IF R1 EQ 'X'.
    PERFORM SELECT_AFKO.
  ELSE.
    PERFORM SELECT_ZTPPPP.
  ENDIF.

ENDFORM.                    " READ_PROCESS
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS
*&---------------------------------------------------------------------*
FORM DATA_PROCESS.
  PERFORM DISPLAY_ZTPPPP.

ENDFORM.                    " DATA_PROCESS
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
*&      Form  CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
FORM CREATE_INTERFACE_LOG.
  DESCRIBE TABLE IT_ZTPPPP LINES Z_TOTAL.
  CHECK Z_TOTAL <> 0.
  I_ZTCA_IF_LOG-TCODE    = 'ZPPI601'.
  I_ZTCA_IF_LOG-TOTAL    = Z_TOTAL.
*  I_ZTCA_IF_LOG-ZSUCC    = Z_SUCC.
*  I_ZTCA_IF_LOG-ERROR    = Z_TOTAL - Z_SUCC.
  I_ZTCA_IF_LOG-ERDAT    = SY-DATUM. "Created on.
  I_ZTCA_IF_LOG-ERZET    = SY-UZEIT. "Created time.
  I_ZTCA_IF_LOG-ERNAM    = SY-UNAME. "Created by.

  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      I_ZTCA_IF_LOG              = I_ZTCA_IF_LOG
*   IMPORTING
*     E_ZTCA_IF_LOG              =
   EXCEPTIONS
     UPDATE_FAILED              = 1
     NUMBER_RANGE_ERROR         = 2
     TCODE_DOES_NOT_EXIST       = 3
     OTHERS                     = 4
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CREATE_INTERFACE_LOG
*&---------------------------------------------------------------------*
*&      Form  SELECT_AFKO
*&---------------------------------------------------------------------*
FORM SELECT_AFKO.
  SELECT A~GSTRP
         A~GAMNG
         A~FEVOR
         A~AUFNR
         A~PLNBEZ
         A~GMEIN
         A~CY_SEQNR
         INTO TABLE IT_AFKO
         FROM AFKO AS A INNER JOIN AFPO AS B
           ON B~AUFNR EQ A~AUFNR
         WHERE B~PWERK    EQ 'P001'
           AND A~GSTRP    EQ S_DATE-LOW
           AND A~CY_SEQNR IN S_SEQNR
           AND A~PLNBEZ   IN S_PLNBEZ
           AND A~AUFNR    IN S_AUFNR.
*           AND A~FEVOR    IN ('SPP', 'SPB').

  SELECT PSTTR
         GSMNG
         PLGRP
         PLNUM
         MATNR
         MEINS
         APPENDING TABLE IT_AFKO
         FROM PLAF
         WHERE PLWRK EQ 'P001'
           AND PSTTR IN S_DATE
           AND PLSCN = ' '
           AND PLGRP IN ('SPP', 'SPB')
           AND MATNR IN S_PLNBEZ.

  LOOP AT IT_AFKO.
    SELECT SINGLE * FROM ZTPPPP
           WHERE PWERK  EQ 'P001'
             AND GSTRP  BETWEEN S_DATE AND S_PSTTR-HIGH.

    IF SY-SUBRC NE 0.
      MOVE-CORRESPONDING IT_AFKO TO *IT_AFKO.
      APPEND *IT_AFKO.
    ENDIF.
  ENDLOOP.

  SORT *IT_AFKO BY GSTRP CY_SEQNR PLNBEZ.

  PERFORM MOVE_AFKO_TO_ITAB.

ENDFORM.                    " SELECT_AFKO
*&---------------------------------------------------------------------*
*&      Form  SELECT_ZTPPPP
*&---------------------------------------------------------------------*
FORM SELECT_ZTPPPP.
  DATA: L_MSGTXT(100),
        L_TABIX LIKE SY-TABIX.

  SELECT * FROM ZTPPPP
           INTO TABLE IT_ZTPPPP
           WHERE FLAG EQ 'E'
             AND PWERK    EQ 'P001'
*             AND ARBPL    EQ P_ARBPL
             AND GSTRP    IN S_GSTRP
             AND MATNR    IN S_PLNBEZ
             AND AUFNR    IN S_AUFNR
             AND CY_SEQNR IN S_SEQNR
             AND GSTRP  BETWEEN S_DATE AND S_PSTTR-HIGH.

  LOOP AT IT_ZTPPPP.
    L_TABIX = SY-TABIX.
    CASE C_MARK.
      WHEN R_1.
        IT_ZTPPPP-EFLAG = 'IR'.
      WHEN R_2.
        IT_ZTPPPP-EFLAG = 'RP'.
      WHEN R_3.
        IT_ZTPPPP-EFLAG = 'DL'.
    ENDCASE.
    MODIFY IT_ZTPPPP INDEX L_TABIX.
  ENDLOOP.
ENDFORM.                    " SELECT_ZTPPPP
*&---------------------------------------------------------------------*
*&      Form  MOVE_AFKO_TO_ITAB
*&---------------------------------------------------------------------*
FORM MOVE_AFKO_TO_ITAB.
  DATA: L_MSGTXT(100),
        L_TABIX LIKE SY-TABIX,
        L_OBJECTKEY LIKE BAPI1003_KEY-OBJECT.

  LOOP AT *IT_AFKO.
    SELECT SINGLE * FROM MARC
           WHERE MATNR EQ *IT_AFKO-PLNBEZ
             AND WERKS EQ 'P001'.

    SELECT SINGLE * FROM MARD
           WHERE MATNR EQ *IT_AFKO-PLNBEZ
             AND WERKS EQ 'P001'
             AND LGORT EQ MARC-LGPRO.

    L_OBJECTKEY = *IT_AFKO-PLNBEZ.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
         EXPORTING
              OBJECTKEY       = L_OBJECTKEY
              OBJECTTABLE     = 'MARA'
              CLASSNUM        = 'PRS_PNL_MASTER'
              CLASSTYPE       = '001'
         TABLES
              ALLOCVALUESNUM  = IT_VMASTER1
              ALLOCVALUESCHAR = IT_VMASTER
              ALLOCVALUESCURR = IT_VMASTER2
              RETURN          = RETURN.

*    IF RETURN-TYPE NE 'E'.
    LOOP AT IT_VMASTER.
      IF IT_VMASTER-CHARACT EQ 'PRS_PNL_MWC'.
        Z_ARBPL = IT_VMASTER-VALUE_NEUTRAL.
      ENDIF.
*      IF IT_VMASTER-CHARACT EQ 'PRS_PNL_DIEN'.
*        Z_DIENO = IT_VMASTER-VALUE_NEUTRAL.
*      ENDIF.
*      IF IT_VMASTER-CHARACT EQ 'PRS_PNL_BLKN'.
*        Z_BLKNO = IT_VMASTER-VALUE_NEUTRAL.
*      ENDIF.
    ENDLOOP.
    CLEAR: L_OBJECTKEY,
           IT_VMASTER.
    REFRESH IT_VMASTER.
*    ENDIF.

    IT_ZTPPPP-PWERK    = 'P001'.
    IT_ZTPPPP-ARBPL    = Z_ARBPL.
    IT_ZTPPPP-GSTRP    = *IT_AFKO-GSTRP.
    IT_ZTPPPP-CY_SEQNR = *IT_AFKO-CY_SEQNR.
    IT_ZTPPPP-GAMNG    = *IT_AFKO-GAMNG.
    IF NOT *IT_AFKO-CY_SEQNR IS INITIAL.
      IT_ZTPPPP-CSTOK    = MARD-LABST.
    ENDIF.
    IF *IT_AFKO-FEVOR EQ 'SPP'.
      IT_ZTPPPP-FEVOR    = 'P'.
    ELSEIF *IT_AFKO-FEVOR EQ 'SPB'.
      IT_ZTPPPP-FEVOR    = 'B'.
    ENDIF.

    CASE C_MARK.
      WHEN R_1.
        IT_ZTPPPP-EFLAG = 'IR'.
      WHEN R_2.
        IT_ZTPPPP-EFLAG = 'RP'.
      WHEN R_3.
        IT_ZTPPPP-EFLAG = 'DL'.
    ENDCASE.
    IT_ZTPPPP-AUFNR    = *IT_AFKO-AUFNR.
    IT_ZTPPPP-MATNR    = *IT_AFKO-PLNBEZ.
    IT_ZTPPPP-ZUSER    = SY-UNAME.
    IT_ZTPPPP-ZSDAT    = SY-DATUM.
    IT_ZTPPPP-ZSTIM    = SY-UZEIT.
    IT_ZTPPPP-ZEDAT    = SY-DATUM.
    IT_ZTPPPP-ZETIM    = SY-UZEIT.
    IT_ZTPPPP-ZMODE    = 'C'.
    APPEND IT_ZTPPPP.
    CLEAR: IT_VMASTER,
           Z_ARBPL,
           MARC,
           MARD.
    REFRESH IT_VMASTER.
  ENDLOOP.

ENDFORM.                    " MOVE_AFKO_TO_ITAB
*&---------------------------------------------------------------------*
*&      Form  MODIFY_ZTPPPP
*&---------------------------------------------------------------------*
FORM MODIFY_ZTPPPP.
  LOOP AT IT_ZTPPPP.
    MOVE-CORRESPONDING IT_ZTPPPP TO *IT_ZTPPPP.
    APPEND *IT_ZTPPPP.
    IF *IT_ZTPPPP-FLAG EQ 'S'.
      PERFORM PRDORD_DELETE_LOGIC.
    ENDIF.
  ENDLOOP.
  MODIFY ZTPPPP FROM TABLE *IT_ZTPPPP.
  IF SY-SUBRC EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " MODIFY_ZTPPPP
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ZTPPPP
*&---------------------------------------------------------------------*
FORM DISPLAY_ZTPPPP.
  LOOP AT IT_ZTPPPP.
    MOVE-CORRESPONDING IT_ZTPPPP TO IT_LIST.
    APPEND IT_LIST.
  ENDLOOP.

ENDFORM.                    " DISPLAY_ZTPPPP
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
FORM BUILD_FIELDCAT.
  APPEND_FIELDCAT :
*   postion    field    ref field     key q-ref   c-ref
*   Text                leg

*    W_COL_POS 'FLAG'    'FLAG'        'X'  ''      ''
*    'LOG'                             '3'  '',
    W_COL_POS 'PWERK'   'PWERK'       'X'  ''      ''
    'Plant'                           '4' '',
    W_COL_POS 'GSTRP'   'GSTRP'       'X'  ''      ''
    'Plan Date.'                      '10' '',
    W_COL_POS 'CY_SEQNR'   'CY_SEQNR'    ''  ''      ''
    'Sequence.'                       '18' '',
    W_COL_POS 'GAMNG'   'GAMNG'       ' '  ''      ''
    'Press Panel Qty.'                '17'  '',
    W_COL_POS 'GMEIN'   'GMEIN'       ''  ''      ''
    'Unit'                            '4' '',
    W_COL_POS 'FEVOR'   'FEVOR'       ''  ''      ''
    'SPP/SPB'                         '8' '',
    W_COL_POS 'AUFNR'   'AUFNR'       ''  ''      ''
    'Panel PrdOrd No.'                '10' '',
    W_COL_POS 'MATNR'   'MATNR'       ''  ''      ''
    'Material.'                       '18' '',
    W_COL_POS 'ZUSER'   'ZUSER'       ' '  ''      ''
    'Creator'                         '10' '',
    W_COL_POS 'ZEDAT'   'ZEDAT'       ' '  ''      ''
    'I/F Date'                        '10' '',
    W_COL_POS 'ZETIM'   'ZETIM'       ' '   ''      ''
    'I/F Time'                        '10' '',
    W_COL_POS 'ZMSG'    'ZMSG'        ' '   ''      ''
    'Message'                        '50' ''.

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
  W_SORTCAT-FIELDNAME      = 'GSTRP'.
  W_SORTCAT-TABNAME        = 'IT_LIST'.
  W_SORTCAT-UP             = 'X'.
  APPEND W_SORTCAT.

  W_SORTCAT-SPOS           = 2.
  W_SORTCAT-FIELDNAME      = 'CY_SEQNR'.
  W_SORTCAT-TABNAME        = 'IT_LIST'.
  W_SORTCAT-UP             = 'X'.
  APPEND W_SORTCAT.

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

*----- User
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'User: '.
  LS_LINE-INFO = SY-UNAME.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*----- Date
  IF S_GSTRP-LOW NE '00000000'.
    LS_LINE-TYP  = 'S'.
    LS_LINE-KEY  = 'Date: '.
    IF S_GSTRP-HIGH NE '00000000'.
     CONCATENATE 'FROM: ' S_GSTRP-LOW '  TO: ' S_GSTRP-HIGH INTO L_LIST.
    ELSE.
      L_LIST = S_GSTRP-LOW.
    ENDIF.
    LS_LINE-INFO = L_LIST.
    APPEND LS_LINE TO LT_TOP_OF_PAGE.
  ENDIF.

*----- Total
  LS_LINE-TYP  = 'S'.
  LS_LINE-KEY  = 'Total: '.
  LS_LINE-INFO = Z_TOTAL.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

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
*&---------------------------------------------------------------------
*         Form  ALV_PF_STATUS_SET
*&---------------------------------------------------------------------
FORM ALV_PF_STATUS_SET USING RT_EXTAB TYPE SLIS_T_EXTAB.
  SET PF-STATUS 'STANDARD' EXCLUDING RT_EXTAB.

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
*&-------------------------------------------------------------------
*&      Form  USER_COMMAND
*&-------------------------------------------------------------------
FORM USER_COMMAND USING UCOMM    LIKE SY-UCOMM
                         SELFIELD TYPE SLIS_SELFIELD.
  DATA : SEL_FIELD LIKE SELFIELD-SEL_TAB_FIELD.

  CASE UCOMM.
    WHEN '&REL'.
      DESCRIBE TABLE IT_ZTPPPP LINES Z_TOTAL.
      PERFORM TRANSFER_PP_TO_MES.
      PERFORM MODIFY_ZTPPPP.
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

  CALL FUNCTION 'Z_FPP_PRESS_PP'
    DESTINATION  C_DEST
    TABLES
      T_ZTPPPP       = IT_ZTPPPP
  EXCEPTIONS
    COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
    SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.


  LOOP AT IT_ZTPPPP.
    L_TABIX = SY-TABIX.
    IF IT_ZTPPPP-ZZRET = 'S'.
      IT_ZTPPPP-FLAG  = IT_ZTPPPP-ZZRET.
      IT_ZTPPPP-ZUSER = SY-UNAME.
      IT_ZTPPPP-ZSDAT = SY-DATUM.
      IT_ZTPPPP-ZSTIM = SY-UZEIT.
      IT_ZTPPPP-ZMODE = 'C'.
      MODIFY IT_ZTPPPP INDEX L_TABIX.
    ELSE.
      IT_ZTPPPP-ZZRET = 'E'.
      IT_ZTPPPP-FLAG  = IT_ZTPPPP-ZZRET.
      IT_ZTPPPP-ZUSER = SY-UNAME.
      IT_ZTPPPP-ZSDAT = SY-DATUM.
      IT_ZTPPPP-ZSTIM = SY-UZEIT.
      IT_ZTPPPP-ZMODE = 'C'.
      MODIFY IT_ZTPPPP INDEX L_TABIX.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " TRANSFER_PP_TO_MES
*&---------------------------------------------------------------------*
*&      Form  CALL_SCREEN_RESULT
*&---------------------------------------------------------------------*
FORM CALL_SCREEN_RESULT.
  DATA: L_FAIL TYPE I.
  SELECT COUNT(*) FROM ZTPPPP
         INTO Z_SUCC
         WHERE FLAG EQ 'S'
           AND GSTRP IN S_GSTRP
           AND ZUSER EQ SY-UNAME
           AND ZSDAT EQ SY-DATUM.

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
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0050  INPUT
*&---------------------------------------------------------------------*
*&      Form  PRDORD_DELETE_LOGIC
*&---------------------------------------------------------------------*
FORM PRDORD_DELETE_LOGIC.
  PERFORM BDC_DYNPRO_PROCESSING USING :
                 'X'  'SAPLCOKO1'        '0110',
                 ' '  'BDC_OKCODE'      '/00'   ,
                 ' '  'RM63E-EQUNR'     *IT_ZTPPPP-AUFNR,
                 ' '  'R62CLORD-FLG_OVIEW'     'X',

                 'X'  'SAPLCOKO1'        '0115',
                 ' '  'BDC_OKCODE'      '=LVMS',

                 'X'  'SAPLCOKO1'        '0115',
                 ' '  'BDC_OKCODE'  '/11'  .

  CALL TRANSACTION 'CO02' USING IT_BDCDATA MODE P_MODE
                          MESSAGES INTO IT_MSG.

ENDFORM.                    " PRDORD_DELETE_LOGIC
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
*&      Form  EXECUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXECUTE_PROCESS.
  CASE C_MARK.
    WHEN R1.   "Transfer
      PERFORM CALL_APP601.
    WHEN R2.   "Retransfer
      PERFORM CALL_IPP601.
  ENDCASE.
*    WHEN P_MRP.
*      PERFORM READ_PROCESS.
*      PERFORM DATA_PROCESS.
*      PERFORM LIST_PROCESS.
*  ENDCASE.
ENDFORM.                    " EXECUTE_PROCESS
*&---------------------------------------------------------------------*
*&      Form  CALL_APP601
*&---------------------------------------------------------------------*
FORM CALL_APP601.
  SUBMIT  ZAPP601R_PRESS_ZTPPPP AND RETURN
                                WITH   P_WERKS = 'P001'
                                WITH   P_PANEL = C_SPP
                                WITH   P_BLANK = C_SPB
                                WITH   S_DATUM IN S_GSTRP
                                WITH   S_MATNR IN S_PLNBEZ
                                WITH   P_SEQ   = P_SEQ
                                WITH   P_MRP   = P_MRP
                                WITH   P_IR    = R_1
                                WITH   P_RP    = R_2
                                WITH   P_DL    = R_3  .
*  WRITE : 'KKK'.
ENDFORM.                    " CALL_APP601
*&---------------------------------------------------------------------*
*&      Form  CALL_IPP601
*&---------------------------------------------------------------------*
FORM CALL_IPP601.
  SUBMIT  ZIPP601I_SET_ZTPPPP AND RETURN
                                WITH   P_WERKS = 'P001'
                                WITH   S_GSTRP IN S_GSTRP
                                WITH   S_AUFNR IN S_AUFNR
                                WITH   S_MATNR IN S_PLNBEZ
                                WITH   P_SEQ   = P_SEQ
                                WITH   P_MRP   = P_MRP
                                WITH   P_TRAN  = R1
                                WITH   P_RETR  = R2
                                WITH   P_IR    = R_1
                                WITH   P_RP    = R_2
                                WITH   P_DL    = R_3 .
ENDFORM.                    " CALL_IPP601
