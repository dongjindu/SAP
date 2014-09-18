************************************************************************
* Program Name      : ZIPP103I_APS_1AA2
* Author            : JongOh, Kim
* Creation Date     : 2003.09.28
* Specifications By : JongOh, Kim
* Pattern           : 1.1
* Development Request No :  UD1K901977
* Addl Documentation:
* Description       : Call RFC for Outbound Interface
*
* Modification Logs
* Date        Developer              RequestNo    Description
* 08/09/2006  Haseeb Mohammad     HelpDesk:-687D654768 Issue log:-
*  UD1K921703 Modified to send the data in packets to EAI so eai can
* efficiently * process the data.
* 04/04/2007  Manju                 UD1K940257    Add WO Pack as *
*                                                 selection Variable
* 09/22/2008 Haseeb UD1K944561  Copied from old version for APS2.
************************************************************************
REPORT ZIPP103I_APS_1AA2 NO STANDARD PAGE HEADING
*                          LINE-SIZE 120
                          MESSAGE-ID ZMPP.

*----------------------------------------------------------------------*
*  EXTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
TABLES : ZTPP_PP_LOG_HEAD,
         ZTPP_PP_LOG_DETA,
         ZTPP_PMT01AA,
         ZSPP_PMT01AA,
         ISELLIST.

*----------------------------------------------------------------------*
*  INTERNAL TABLE DECLARATION
*----------------------------------------------------------------------*
DATA : IT_PMT01AA   LIKE TABLE OF ZTPP_PMT01AA WITH HEADER LINE,
       IT_PMS01AA   LIKE TABLE OF ZSPP_PMT01AA WITH HEADER LINE.

*----------------------------------------------------------------------*
*  GLOBAL VARIABLE DECLARATION
*----------------------------------------------------------------------*
DATA :     WA_CNT          LIKE ZTPP_PP_LOG_DETA-SEQUENCE,
           WA_NUMBER       LIKE ZTPP_PP_LOG_HEAD-LOGKEY,
           WA_ERROR        TYPE C               ,
           WA_FLG          TYPE CHAR1  VALUE 'X'.

*----------------------------------------------------------------------*
*  CONSTANS DECLARATION
*----------------------------------------------------------------------*
CONSTANTS: C_COUNT(4)  VALUE 2000     ,
           C_DEST(10)  VALUE 'WMPP01'.   "Outbound Interface Destination

*----------------------------------------------------------------------
* SELECTION-SCREEN DECLARATION
*----------------------------------------------------------------------
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME .
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: P_RUN          TYPE C AS CHECKBOX  DEFAULT 'X'.
SELECTION-SCREEN COMMENT  (55) TEXT-100 FOR FIELD P_RUN.
SELECTION-SCREEN END OF LINE.
** Changed by Furong on 05/04/12 ; to get dynamic variant
*SELECT-OPTIONS : S_T_PACK FOR ISELLIST-MONTH NO-EXTENSION.
*SELECT-OPTIONS : S_date FOR sy-datum NO-EXTENSION.
SELECT-OPTIONS : S_MONTH FOR ZTPP_PMT01AA-MONT.
*select-options : s_pack for zspp_pmt01aa-pack.
** End on 05/04/12
*parameters  : P_pack like zspp_pmt01aa-pack.
SELECTION-SCREEN: END OF BLOCK B1.

************************************************************************
* INITIALIZAION
************************************************************************
INITIALIZATION.
*  PERFORM INITIALIZATION.

************************************************************************
* AT SELECTION-SCREEN
************************************************************************
AT SELECTION-SCREEN.
*  PERFORM AT_SELECTION-SCREEN.

************************************************************************
* START-OF-SELECTION
************************************************************************
START-OF-SELECTION.
  CHECK P_RUN = 'X'.
  PERFORM EXCUTE_PROCESS.

************************************************************************
* END-OF-SELECTION
************************************************************************
END-OF-SELECTION.
*  PERFORM LIST_PROCESS.

*&---------------------------------------------------------------------*
*&      Form  EXCUTE_PROCESS
*&---------------------------------------------------------------------*
FORM EXCUTE_PROCESS.
  DATA: L_MSGTXT(100),
        L_SIZE      TYPE NUM9,
        L_IDX       TYPE NUM9,
        L_CURR      TYPE NUM9,
        L_COUNT(4)  TYPE N   .

  CLEAR : IT_PMT01AA, IT_PMT01AA[].
  CLEAR : IT_PMS01AA, IT_PMS01AA[].

** Furong on 05/04/12
*  RANGES: S_PACK FOR ZSPP_PMT01AA-PACK.
*
*  S_PACK-OPTION = S_date-OPTION.
*  S_PACK-SIGN = S_date-SIGN.
*  S_PACK-LOW = S_date-LOW+2(4).
*  S_PACK-HIGH = S_date-HIGH+2(4).
*  APPEND S_PACK.
** End on 05/04/12


 SELECT *
         INTO TABLE IT_PMT01AA
         FROM ZTPP_PMT01AA
         WHERE MONT IN S_MONTH  AND
*         WHERE pack eq P_pack   and
              ( ZRESULT EQ 'E' OR ZRESULT EQ SPACE ).

*  SELECT *
*         INTO TABLE IT_PMT01AA
*         FROM ZTPP_PMT01AA
*         WHERE PACK IN S_PACK   AND
**         WHERE pack eq P_pack   and
*              ( ZRESULT EQ 'E' OR ZRESULT EQ SPACE ).

  IF SY-SUBRC EQ 0.
    DESCRIBE TABLE IT_PMT01AA LINES L_SIZE.
* Modified by Haseeb Mohammad to send the data in packets.
    L_IDX = L_SIZE DIV C_COUNT .
    L_IDX = L_IDX + 1          .          " Total RFC Call Count
*End of Change
    LOOP AT IT_PMT01AA.
      L_COUNT = L_COUNT + 1.

      MOVE-CORRESPONDING IT_PMT01AA TO IT_PMS01AA.
      APPEND IT_PMS01AA.
* Modified by haseeb mohammad to send the data in packets
*      IF l_count >= l_size.
      IF L_COUNT >= C_COUNT.
*end of change.
        L_CURR = L_CURR + 1.
        CALL FUNCTION 'Z_FPP_SET_PMT01AA_2'
          DESTINATION C_DEST
          EXPORTING
            TOTAL_C               = L_IDX
            CURR_C                = L_CURR
          TABLES
            I_ZSPP_PMT01AA        = IT_PMS01AA
          EXCEPTIONS
            COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
            SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

        IF SY-SUBRC <> 0.
          " Write Log....
          PERFORM CREATE_LOG USING 'E' 1  TEXT-001  L_MSGTXT .
          PERFORM CREATE_LOG USING 'R' 1  TEXT-001  L_MSGTXT .
          IF SY-BATCH = ' '.
            MESSAGE E001 WITH TEXT-001 .
          ENDIF.
          WA_ERROR = 'X' .          EXIT.
        ELSE.
          LOOP AT IT_PMS01AA WHERE ZZRET = 'E'.
            WA_ERROR = 'X' .
            PERFORM CREATE_LOG USING 'E' 1  TEXT-001  L_MSGTXT .
            PERFORM CREATE_LOG USING 'R' 1  TEXT-001  L_MSGTXT .
            IF SY-BATCH = ' '.
              MESSAGE E001 WITH TEXT-001 .
            ENDIF.
            EXIT.
          ENDLOOP.
          CLEAR: IT_PMS01AA, IT_PMS01AA[].
          IF WA_ERROR = 'X'. EXIT. ENDIF.
        ENDIF.
        CLEAR: WA_FLG, L_COUNT.
      ENDIF.
    ENDLOOP.
    IF WA_ERROR = SPACE.
      L_CURR = L_CURR + 1.
      CALL FUNCTION 'Z_FPP_SET_PMT01AA_2'
        DESTINATION C_DEST
        EXPORTING
          TOTAL_C               = L_IDX
          CURR_C                = L_CURR
        TABLES
          I_ZSPP_PMT01AA        = IT_PMS01AA
        EXCEPTIONS
          COMMUNICATION_FAILURE = 1  MESSAGE L_MSGTXT
          SYSTEM_FAILURE        = 2  MESSAGE L_MSGTXT.

      IF SY-SUBRC <> 0.
        " Write Log....
        PERFORM CREATE_LOG USING 'E' 1  TEXT-001  L_MSGTXT .
        PERFORM CREATE_LOG USING 'R' 1  TEXT-001  L_MSGTXT .
        IF SY-BATCH = ' '.
          MESSAGE E001 WITH TEXT-001 .
        ENDIF.
        WA_ERROR = 'X' .
      ELSE.
        LOOP AT IT_PMS01AA WHERE ZZRET = 'E'.
          WA_ERROR = 'X' .
          PERFORM CREATE_LOG USING 'E' 1  TEXT-001  L_MSGTXT .
          PERFORM CREATE_LOG USING 'R' 1  TEXT-001  L_MSGTXT .
          IF SY-BATCH = ' '.
            MESSAGE E001 WITH TEXT-001 .
          ENDIF.
          EXIT.
        ENDLOOP.
        CLEAR: IT_PMS01AA, IT_PMS01AA[].
      ENDIF.
      CHECK WA_ERROR = SPACE.
      CONCATENATE 'Total Record is' L_SIZE 'Success!!!' INTO L_MSGTXT.
      IF SY-BATCH = ' '.
        MESSAGE S001 WITH TEXT-002 .
      ENDIF.
      PERFORM CREATE_LOG USING 'S' 2  TEXT-002  L_MSGTXT .
    ENDIF.
  ELSE.
    CONCATENATE 'Total Record is' L_SIZE 'Success!!!' INTO L_MSGTXT.
    IF SY-BATCH = ' '.
      MESSAGE S001 WITH TEXT-002 .
    ENDIF.
    PERFORM CREATE_LOG USING 'S' 2  TEXT-002  L_MSGTXT .
  ENDIF.
ENDFORM.                    " EXCUTE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  create_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0177   text
*      -->P_1      text
*      -->P_TEXT_001  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM CREATE_LOG USING    PA_TYPE  PA_STEP  PA_TEXT  PA_KEY .
  WA_CNT = WA_CNT + 1 .
  IF WA_CNT = 1       .
    PERFORM GET_LOGSERIAL.               " Log Number Generation........
    ZTPP_PP_LOG_HEAD-LOGKEY   = WA_NUMBER   .
    ZTPP_PP_LOG_HEAD-PROGRAMM = SY-REPID    .
    ZTPP_PP_LOG_HEAD-LOGTYPE  = PA_TYPE     .
    ZTPP_PP_LOG_HEAD-JOBTYPE  = SY-BATCH    .
    ZTPP_PP_LOG_HEAD-LOGSTEP  = PA_STEP     .
    ZTPP_PP_LOG_HEAD-MSG      = PA_TEXT     .
    ZTPP_PP_LOG_HEAD-LDATE    = SY-DATUM    .
    ZTPP_PP_LOG_HEAD-LTIME    = SY-UZEIT    .
    ZTPP_PP_LOG_HEAD-LUSER    = SY-UNAME    .
    INSERT INTO ZTPP_PP_LOG_HEAD VALUES ZTPP_PP_LOG_HEAD .
  ENDIF.

  " Log Detail Creation
  ZTPP_PP_LOG_DETA-LOGKEY   = WA_NUMBER    .
  ZTPP_PP_LOG_DETA-SEQUENCE = WA_CNT      .
  ZTPP_PP_LOG_DETA-LOGTYPE  = PA_TYPE     .
  ZTPP_PP_LOG_DETA-JOBTYPE  = SY-BATCH    .
  ZTPP_PP_LOG_DETA-LOGSTEP  = PA_STEP     .
  ZTPP_PP_LOG_DETA-KEYDATA  = PA_KEY      .
  INSERT INTO ZTPP_PP_LOG_DETA VALUES ZTPP_PP_LOG_DETA .
ENDFORM.                    " create_log

*&---------------------------------------------------------------------*
*&      Form  get_logserial
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_LOGSERIAL.
  " Log Head Creation..
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR             = '01'
      OBJECT                  = 'ZLOG'
    IMPORTING
      NUMBER                  = WA_NUMBER
    EXCEPTIONS
      INTERVAL_NOT_FOUND      = 1
      NUMBER_RANGE_NOT_INTERN = 2
      OBJECT_NOT_FOUND        = 3
      QUANTITY_IS_0           = 4
      QUANTITY_IS_NOT_1       = 5
      INTERVAL_OVERFLOW       = 6
      BUFFER_OVERFLOW         = 7
      OTHERS                  = 8.
ENDFORM.                    " GET_LOGSERIAL
