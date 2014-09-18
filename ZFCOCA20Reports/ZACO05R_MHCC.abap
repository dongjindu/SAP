************************************************************************
* Program Name      : ZACO05R_MHCC
* Author            : Hyung Jin Youn
* Creation Date     : 2003.10.23
* Specifications By : Dong-Hwan Kim
* Pattern           : Report 1-1
* Development Request No : UD1K902723
* Addl Documentation:
* Description       : This program display data in
*                     the table "ZTCO_MHPCPOST" with different select
*                     conditions
* Modification Logs
* Date       Developer    RequestNo    Description
*
************************************************************************
REPORT ZACO05R_MHCC MESSAGE-ID ZMCO.

*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** TYPE-POOLS
TYPE-POOLS: VRM, SLIS.

** Tables
TABLES : ZTCO_MHPCPOST.

** Internal Table
DATA : IT_ZTCO_MHPCPOST LIKE STANDARD TABLE OF ZTCO_MHPCPOST
                        WITH HEADER LINE .

** Global Variables


*&----------------------------------------------------------------------
*  Initialization
*&----------------------------------------------------------------------
INITIALIZATION.


*----------------------------------------------------------------------*
*   Selection Condition                                                *
*----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK BL1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : P_KOKRS LIKE CSKS-KOKRS   MEMORY   ID CAC OBLIGATORY,
               P_GJAHR LIKE ANLP-GJAHR   MEMORY   ID GJR OBLIGATORY.

  SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT 1(31) TEXT-002.
*   From Period.
  PARAMETERS: P_FRPER LIKE RKU01G-PERAB OBLIGATORY.
  SELECTION-SCREEN COMMENT 52(05) TEXT-003.
*   To Period.
  PARAMETERS: P_TOPER LIKE RKU01G-PERBI OBLIGATORY.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: P_SELCON   AS LISTBOX VISIBLE LENGTH 20
                         DEFAULT '0' OBLIGATORY .

  SELECTION-SCREEN END OF BLOCK BL1.

  SELECTION-SCREEN BEGIN OF BLOCK BL2 WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS : S_MHDOC FOR ZTCO_MHPCPOST-MHDOC
                    MATCHCODE OBJECT ZSH_CO_MHDOC.
  SELECT-OPTIONS : S_ERDAT FOR ZTCO_MHPCPOST-ERDAT,
                   S_ERZET FOR ZTCO_MHPCPOST-ERZET,
                   S_ERNAM FOR ZTCO_MHPCPOST-ERNAM,
                   S_AEDAT FOR ZTCO_MHPCPOST-AEDAT,
                   S_AEZET FOR ZTCO_MHPCPOST-AEZET,
                   S_AENAM FOR ZTCO_MHPCPOST-AENAM.
  SELECTION-SCREEN END OF BLOCK BL2.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
* Show ListBox
  PERFORM SHOW_LIST_BOX.

AT SELECTION-SCREEN.
* Check period range
  PERFORM CHECK_PERIOD_RANGE.


*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
* read data
  PERFORM READ_DATA_FR_ZTCO_MHPCPOST.


*----------------------------------------------------------------------*
* END-of-selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
* Call ALV Grid Control.
  PERFORM  CALL_ALV_GRID_CONTROL.


*
*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SHOW_LIST_BOX
*&---------------------------------------------------------------------*
*       List Box
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHOW_LIST_BOX.
  DATA: LV_NAME       TYPE VRM_ID,
        IT_L_LIST       TYPE VRM_VALUES,
        LV_VALUE      LIKE LINE OF IT_L_LIST.

  LV_NAME = 'P_SELCON'.

  LV_VALUE-KEY  = '0'.
  LV_VALUE-TEXT = 'Total'.
  APPEND LV_VALUE TO IT_L_LIST.

  LV_VALUE-KEY  = '1'.
  LV_VALUE-TEXT = 'Posted'.
  APPEND LV_VALUE TO IT_L_LIST.

  LV_VALUE-KEY  = '2'.
  LV_VALUE-TEXT = 'Not Posted'.
  APPEND LV_VALUE TO IT_L_LIST.

  LV_VALUE-KEY  = '3'.
  LV_VALUE-TEXT = 'Reversed'.
  APPEND LV_VALUE TO IT_L_LIST.

  LV_VALUE-KEY  = '4'.
  LV_VALUE-TEXT = 'Not Reversed'.
  APPEND LV_VALUE TO IT_L_LIST.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            ID     = LV_NAME
            VALUES = IT_L_LIST.
ENDFORM.                    " SHOW_LIST_BOX

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PERIOD_RANGE.
  IF P_FRPER > P_TOPER.
    MESSAGE E031.
  ENDIF.
ENDFORM.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_ZTCO_MHPCPOST
*&---------------------------------------------------------------------*
*       Read data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_FR_ZTCO_MHPCPOST.

  CLEAR : IT_ZTCO_MHPCPOST, IT_ZTCO_MHPCPOST[].

  CASE  P_SELCON.
    WHEN '0'. " 'Total'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MHPCPOST
               FROM ZTCO_MHPCPOST
              WHERE GJAHR = P_GJAHR
                AND PERID BETWEEN P_FRPER AND P_TOPER
* Common
                AND  MHDOC IN S_MHDOC
                AND  ERDAT IN S_ERDAT
                AND  ERZET IN S_ERZET
                AND  ERNAM IN S_ERNAM
                AND  AEDAT IN S_AEDAT
                AND  AEZET IN S_AEZET
                AND  AENAM IN S_AENAM .

    WHEN '1' OR '4' . " 'Posted / Not reversed'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MHPCPOST
               FROM ZTCO_MHPCPOST
              WHERE GJAHR = P_GJAHR
                AND PERID BETWEEN P_FRPER AND P_TOPER
                AND RUECK NE SPACE
                AND RMZHL NE SPACE
                AND REVERSED EQ SPACE
* Common
                AND  MHDOC IN S_MHDOC
                AND  ERDAT IN S_ERDAT
                AND  ERZET IN S_ERZET
                AND  ERNAM IN S_ERNAM
                AND  AEDAT IN S_AEDAT
                AND  AEZET IN S_AEZET
                AND  AENAM IN S_AENAM .

    WHEN '2'. " 'Not Posted'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MHPCPOST
               FROM ZTCO_MHPCPOST
              WHERE GJAHR = P_GJAHR
                AND PERID BETWEEN P_FRPER AND P_TOPER
                AND RUECK EQ SPACE
                AND RMZHL EQ SPACE
                AND REVERSED EQ SPACE
* Common
                AND  MHDOC IN S_MHDOC
                AND  ERDAT IN S_ERDAT
                AND  ERZET IN S_ERZET
                AND  ERNAM IN S_ERNAM
                AND  AEDAT IN S_AEDAT
                AND  AEZET IN S_AEZET
                AND  AENAM IN S_AENAM .

    WHEN '3'. " 'Reversed'
      SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_MHPCPOST
               FROM ZTCO_MHPCPOST
              WHERE GJAHR = P_GJAHR
                AND PERID BETWEEN P_FRPER AND P_TOPER
                AND REVERSED NE SPACE
* Common
                AND  MHDOC IN S_MHDOC
                AND  ERDAT IN S_ERDAT
                AND  ERZET IN S_ERZET
                AND  ERNAM IN S_ERNAM
                AND  AEDAT IN S_AEDAT
                AND  AEZET IN S_AEZET
                AND  AENAM IN S_AENAM .

  ENDCASE.

ENDFORM.                    " READ_DATA_FR_ZTCO_MHPCPOST

*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_GRID_CONTROL
*&---------------------------------------------------------------------*
*       ALV Grid Control
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_ALV_GRID_CONTROL.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*   I_INTERFACE_CHECK                 = ' '
*   I_BYPASSING_BUFFER                =
*   I_BUFFER_ACTIVE                   = ' '
*   I_CALLBACK_PROGRAM                = ' '
*   I_CALLBACK_PF_STATUS_SET          = ' '
*   I_CALLBACK_USER_COMMAND           = ' '
*   I_CALLBACK_TOP_OF_PAGE            = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*   I_CALLBACK_HTML_END_OF_LIST       = ' '
      I_STRUCTURE_NAME                  = 'ZTCO_MHPCPOST'
*   I_BACKGROUND_ID                   = ' '
*   I_GRID_TITLE                      =
*   I_GRID_SETTINGS                   =
*   IS_LAYOUT                         =
*   IT_FIELDCAT                       =
*   IT_EXCLUDING                      =
*   IT_SPECIAL_GROUPS                 =
*   IT_SORT                           =
*   IT_FILTER                         =
*   IS_SEL_HIDE                       =
*   I_DEFAULT                         = 'X'
*   I_SAVE                            = ' '
*   IS_VARIANT                        =
*   IT_EVENTS                         =
*   IT_EVENT_EXIT                     =
*   IS_PRINT                          =
*   IS_REPREP_ID                      =
*   I_SCREEN_START_COLUMN             = 0
*   I_SCREEN_START_LINE               = 0
*   I_SCREEN_END_COLUMN               = 0
*   I_SCREEN_END_LINE                 = 0
*   IT_ALV_GRAPHICS                   =
*   IT_ADD_FIELDCAT                   =
*   IT_HYPERLINK                      =
*   I_HTML_HEIGHT_TOP                 =
*   I_HTML_HEIGHT_END                 =
* IMPORTING
*   E_EXIT_CAUSED_BY_CALLER           =
*   ES_EXIT_CAUSED_BY_USER            =
    TABLES
      T_OUTTAB                          = IT_ZTCO_MHPCPOST
    EXCEPTIONS
      PROGRAM_ERROR                     = 1
      OTHERS                            = 2
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " CALL_ALV_GRID_CONTROL
