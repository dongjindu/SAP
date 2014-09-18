*----------------------------------------------------------------------*
*   INCLUDE RPITIG03: Routines to read tables for RPITIG00             *
*----------------------------------------------------------------------*
* 4.6C (LCP)
* XPSL9CK079128 22Jan02 (N0485401)
* QNUL9CK007324 28122000 Note 217573
*----------------------------------------------------------------------*

*---------------------------------------------------------------------*
*       FORM RE_T001P                                                 *
*---------------------------------------------------------------------*
*       Read table T001P to determine the payscale type and payscale  *
*       area for further calculations (the country modifier (MOLGA)   *
*       will be read from T500P).                                     *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  PREREQUISITE  : P0001                                              *
*---------------------------------------------------------------------*
*  -->  P_WERKS  : Personnel area                                     *
*  -->  P_BTRTL  : Personnel subarea                                  *
*---------------------------------------------------------------------*
*FORM RE_T001P USING P_WERKS LIKE P0001-WERKS            "WRH AHRK041998
*                    P_BTRTL LIKE P0001-BTRTL.           "WRH AHRK041998
FORM RE_T001P USING P_WERKS LIKE P0001-WERKS             "WRH AHRK041998
                    P_BTRTL LIKE P0001-BTRTL             "WRH AHRK041998
                    P_ADD         TYPE C.                "WRH AHRK041998
  DATA: TABLE_ARG(9).
*-Check if information has been previously read
  CHECK T001P-WERKS NE P_WERKS OR T001P-BTRTL NE P_BTRTL.
  SELECT SINGLE * FROM T001P WHERE WERKS EQ P_WERKS
                               AND BTRTL EQ P_BTRTL.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T001P.                         "Clear table work area
  CONCATENATE P_WERKS P_BTRTL INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '001'
                                  TABLE_ARG SPACE SPACE SPACE.
  IF NOT P_ADD IS INITIAL.                               "WRH AHRK041998
    ADD 1 TO NO_REJECTED.                                "WRH AHRK041998
  ENDIF.                                                 "WRH AHRK041998
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T001P.

*&--------------------------------------------------------------------*
*&      Form  RE_T500P                                                *
*&--------------------------------------------------------------------*
*       Read table T500P to determine the country modifier (MOLGA).   *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  PREREQUISITE  : P0001                                              *
*---------------------------------------------------------------------*
*  -->  P_P0001-WERKS  : Personnel area                               *
*---------------------------------------------------------------------*
*FORM RE_T500P USING P_P0001-WERKS LIKE P0001-WERKS.     "WRH AHRK041998
FORM RE_T500P USING P_P0001-WERKS LIKE P0001-WERKS       "WRH AHRK041998
                    P_ADD         TYPE C.                "WRH AHRK041998
  DATA: TABLE_ARG(4).
*-Check if information has been previously read
  CHECK T500P-PERSA NE P_P0001-WERKS.
  SELECT SINGLE * FROM T500P WHERE PERSA EQ P_P0001-WERKS.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T500P.                         "Clear table work area
  TABLE_ARG+0(4) = P_P0001-WERKS.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T500P' TABLE_ARG SPACE SPACE.
  IF NOT P_ADD IS INITIAL.                               "WRH AHRK041998
    ADD 1 TO NO_REJECTED.                                "WRH AHRK041998
  ENDIF.                                                 "WRH AHRK041998
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                    " RE_T500P

*---------------------------------------------------------------------*
*       FORM RE_T503                                                  *
*---------------------------------------------------------------------*
*       Read table T503 to determine the payscale indicator (TRFKZ).  *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  PREREQUISITE  : P0001                                              *
*---------------------------------------------------------------------*
*  -->  P_PERSG  : Employee group                                     *
*  -->  P_PERSK  : Employee subgroup                                  *
*---------------------------------------------------------------------*
*FORM RE_T503 USING P_PERSG LIKE P0001-PERSG             "WRH AHRK041998
*                   P_PERSK LIKE P0001-PERSK.            "WRH AHRK041998
FORM RE_T503 USING P_PERSG LIKE P0001-PERSG              "WRH AHRK041998
                   P_PERSK LIKE P0001-PERSK              "WRH AHRK041998
                   P_ADD   TYPE C.                       "WRH AHRK041998
  DATA: TABLE_ARG(4).
*-Check if information has been previously read
  CHECK T503-PERSG NE P_PERSG OR T503-PERSK NE P_PERSK.
  SELECT SINGLE * FROM T503 WHERE PERSG EQ P_PERSG
                              AND PERSK EQ P_PERSK.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T503.                          "Clear table work area
  CONCATENATE P_PERSG P_PERSK INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '002'
                                  TABLE_ARG SPACE SPACE SPACE.
  IF NOT P_ADD IS INITIAL.                               "WRH AHRK041998
    ADD 1 TO NO_REJECTED.              "Increment number of rejected ees
  ENDIF.                                                 "WRH AHRK041998
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T503.

*---------------------------------------------------------------------*
*       FORM RE_T510A                                                 *
*---------------------------------------------------------------------*
*       Read table T510A to determine the text for the payscale type. *
*       In case of an error: add a message to the error list.         *
*---------------------------------------------------------------------*
*  -->  P_MOLGA  : Employee group                                     *
*  -->  P_TRFAR  : Employee subgroup                                  *
*---------------------------------------------------------------------*
FORM RE_T510A USING P_MOLGA LIKE T510A-MOLGA
                    P_TRFAR LIKE T510A-TRFAR.
  DATA: TABLE_ARG(5).
*-Check if information has been previously read
  CHECK T510A-MOLGA NE P_MOLGA OR T510A-TRFAR NE P_TRFAR.
  SELECT SINGLE * FROM T510A WHERE MOLGA EQ P_MOLGA
                               AND TRFAR EQ P_TRFAR.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T510A.                         "Clear table work area
  CONCATENATE P_MOLGA P_TRFAR INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'M' '001'
                                  'T510A' TABLE_ARG SPACE SPACE.
ENDFORM.                               "RE_T510A

*---------------------------------------------------------------------*
*       FORM RE_T510D                                                 *
*---------------------------------------------------------------------*
*       Read table T510D to determine the information relevant for    *
*       the dynamic payscale increase.                                *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  -->  P_MOLGA  : Country grouping                                   *
*  -->  P_TRFAR  : Pay scale type                                     *
*  -->  P_TRFGB  : Pay scale area                                     *
*  -->  P_TRFKZ  : EE subgroup grouping for collective agreement prov.*
*  -->  P_TRFGR  : Pay scale group                                    *
*  -->  P_TRFST  : Pay scale level                                    *
*  -->  P_LGART  : Wage type                                          *
*  -->  P_BEGDA  : From                                               *
*---------------------------------------------------------------------*
FORM RE_T510D USING P_MOLGA LIKE T510D-MOLGA
                    P_TRFAR LIKE T510D-TRFAR
                    P_TRFGB LIKE T510D-TRFGB
                    P_TRFKZ LIKE T510D-TRFKZ
                    P_TRFGR LIKE T510D-TRFGR
                    P_TRFST LIKE T510D-TRFST
                    P_LGART LIKE T510D-LGART
                    P_BEGDA LIKE T510D-BEGDA.
  DATA: TABLE_ARG(36).
*-Check if information has been previously read
  CHECK T510D-MOLGA NE P_MOLGA
     OR T510D-TRFAR NE P_TRFAR
     OR T510D-TRFGB NE P_TRFGB
     OR T510D-TRFKZ NE P_TRFKZ
     OR T510D-TRFGR NE P_TRFGR
     OR T510D-TRFST NE P_TRFST
     OR T510D-LGART NE P_LGART
     OR T510D-BEGDA NE P_BEGDA.
  SELECT SINGLE * FROM T510D WHERE MOLGA EQ P_MOLGA
                               AND TRFAR EQ P_TRFAR
                               AND TRFGB EQ P_TRFGB
                               AND TRFKZ EQ P_TRFKZ
                               AND TRFGR EQ P_TRFGR
                               AND TRFST EQ P_TRFST
                               AND LGART EQ P_LGART
                               AND BEGDA EQ P_BEGDA.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T510D.                         "Clear table work area
  CONCATENATE P_MOLGA P_TRFAR P_TRFGB P_TRFKZ P_TRFGR P_TRFST
              P_LGART P_BEGDA INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T510D' TABLE_ARG SPACE SPACE.
  ADD 1 TO NO_REJECTED.                "Increment number of rejected ees
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T510D

*&---------------------------------------------------------------------*
*&      Form  RE_510F                                                  *
*&---------------------------------------------------------------------*
*       Determine time unit                                            *
*----------------------------------------------------------------------*
*      -->P_MOLGA  : Country grouping                                  *
*      -->P_TRFAR  : Pay scale type                                    *
*      -->P_TRFGB  : Pay scale area                                    *
*      -->P_TRFKZ  : Pay scale indicator                               *
*      -->P_BEGDA  : Begin date                                        *
*----------------------------------------------------------------------*
*form re_510f using p_molga like t500p-molga             "WRH ALRK044773
*                   p_trfar like p0008-trfar             "WRH ALRK044773
*                   p_trfgb like p0008-trfgb             "WRH ALRK044773
*                   p_trfkz like t503-trfkz              "WRH ALRK044773
*                   p_begda like p0008-begda.            "WRH ALRK044773
* data: table_arg(19).                                   "WRH ALRK044773
*-Check if information has been previously read          "WRH ALRK044773
* check t510f-molga ne p_molga                           "WRH ALRK044773
*    or t510f-trfar ne p_trfar                           "WRH ALRK044773
*    or t510f-trfgb ne p_trfgb                           "WRH ALRK044773
*    or t510f-trfkz ne p_trfkz                           "WRH ALRK044773
*    or t510f-begda gt p_begda                           "WRH ALRK044773
*    or t510f-endda lt p_begda.                          "WRH ALRK044773
* select * from t510f where molga eq p_molga             "WRH ALRK044773
*                       and trfar eq p_trfar             "WRH ALRK044773
*                       and trfgb eq p_trfgb             "WRH ALRK044773
*                       and trfkz eq p_trfkz             "WRH ALRK044773
*                       and endda ge p_begda             "WRH ALRK044773
*                       and begda le p_begda.            "WRH ALRK044773
*   exit.                                                "WRH ALRK044773
* endselect.                                             "WRH ALRK044773
* check sy-subrc ne 0.                                   "WRH ALRK044773
*-Error handling                                         "WRH ALRK044773
* clear t510f.                                           "WRH ALRK044773
* concatenate p_molga p_trfar p_trfgb p_trfkz p_begda    "WRH ALRK044773
* into table_arg separated by space.                     "WRH ALRK044773
* perform append_error_list                              "WRH ALRK044773
*         using pernr-pernr 'PN' 'E' '001'               "WRH ALRK044773
*               'T510F' table_arg space space.           "WRH ALRK044773
* increment number of rejected ees                       "WRH ALRK044773
* add 1 to no_rejected.                                  "WRH ALRK044773
* Process next personnel number                          "WRH ALRK044773
* perform reject_pernr.                                  "WRH ALRK044773
*endform.                    " RE_510F                   "WRH ALRK044773

*---------------------------------------------------------------------*
*       FORM RE_T510G                                                 *
*---------------------------------------------------------------------*
*       Read table T510G to determine the text for the payscale area. *
*       In case of an error: add a message to the error list.         *
*---------------------------------------------------------------------*
*  -->  P_MOLGA  : Country grouping                                   *
*  -->  P_TRFGB  : Pay scale area                                     *
*---------------------------------------------------------------------*
FORM RE_T510G USING P_MOLGA LIKE T510G-MOLGA
                    P_TRFGB LIKE T510G-TRFGB.
  DATA: TABLE_ARG(5).
*-Check if information has been previously read
  CHECK T510G-MOLGA NE P_MOLGA OR T510G-TRFGB NE P_TRFGB.
  SELECT SINGLE * FROM T510G WHERE MOLGA EQ P_MOLGA
                               AND TRFGB EQ P_TRFGB.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T510G.                         "Clear table work area
  CONCATENATE P_MOLGA P_TRFGB INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'M' '001'
                                  'T510G' TABLE_ARG SPACE SPACE.
ENDFORM.                               "RE_T510G

*---------------------------------------------------------------------*
*       FORM RE_T510R                                                 *
*---------------------------------------------------------------------*
*       Read table T510R to determine the payscale reclassification   *
*       limits (age jump, group membership duration, Time-In-Grade)   *
*---------------------------------------------------------------------*
*  -->  P_MOLGA  : Country grouping                                   *
*  -->  P_TRFAR  : Pay scale type                                     *
*  -->  P_TRFGB  : Pay scale area                                     *
*  -->  P_TRFKZ  : EE subgroup grouping for collective agreement prov.*
*  -->  P_TRFGR  : Pay scale group                                    *
*  -->  P_TRFST  : Pay scale level                                    *
*  -->  P_VARGU  : Variable argument                                  *
*  -->  P_EHIRE  : Validity end date of hiring period                 *
*  -->  P_BEGDA  : From                                               *
*  -->  P_ENDDA  : To                                                 *
*---------------------------------------------------------------------*
FORM RE_T510R USING P_MOLGA LIKE T510R-MOLGA
                    P_TRFAR LIKE T510R-TRFAR
                    P_TRFGB LIKE T510R-TRFGB
                    P_TRFKZ LIKE T510R-TRFKZ
                    P_TRFGR LIKE T510R-TRFGR
                    P_TRFST LIKE T510R-TRFST
                    P_VARGU LIKE T510R-VARGU
                    P_EHIRE LIKE T510R-EHIRE
                    P_STVOR LIKE P0008-STVOR             "WRH PH4K002071
                    P_RECDA LIKE SY-DATUM                "WRH PH4K002071
                    P_KEYDA LIKE T510R-ENDDA.            "WRH PH4K002071
*                   p_begda LIKE t510r-begda             "WRH PH4K002071
*                   p_endda LIKE t510r-endda.            "WRH PH4K002071
  DATA: TABLE_ARG(45).
  DATA: SUBRC LIKE SY-SUBRC.                             "WRH PH4K002071
  CLEAR SUBRC.                                           "WRH PH4K002071
*-Check if information has been previously read
* CHECK t510r-molga NE p_molga OR                        "WRH PH4K002071
  IF    T510R-MOLGA NE P_MOLGA OR                        "WRH PH4K002071
        T510R-TRFAR NE P_TRFAR OR
        T510R-TRFGB NE P_TRFGB OR
        T510R-TRFKZ NE P_TRFKZ OR
        T510R-TRFGR NE P_TRFGR OR
        T510R-TRFST NE P_TRFST OR
        T510R-VARGU NE P_VARGU OR
        T510R-EHIRE NE P_EHIRE OR
        T510R-ENDDA LT P_KEYDA OR                        "WRH PH4K002071
        T510R-BEGDA GT P_KEYDA.                          "WRH PH4K002071
*       t510r-endda NE p_endda.                          "WRH PH4K002071
    SELECT * FROM T510R WHERE MOLGA EQ P_MOLGA
                          AND TRFAR EQ P_TRFAR
                          AND TRFGB EQ P_TRFGB
                          AND TRFKZ EQ P_TRFKZ
                          AND TRFGR EQ P_TRFGR
                          AND TRFST EQ P_TRFST
                          AND VARGU EQ P_VARGU
                          AND EHIRE GE P_EHIRE
                          AND ENDDA GE P_KEYDA           "WRH PH4K002071
                          AND BEGDA LE P_KEYDA.          "WRH PH4K002071
*                         AND begda LE p_begda           "WRH PH4K002071
*                         AND endda GE p_begda.          "WRH PH4K002071
      EXIT.
    ENDSELECT.
    SUBRC = SY-SUBRC.                                    "WRH PH4K002071
  ENDIF.
  IF SUBRC NE 0.                                         "WRH PH4K002071
* IF sy-subrc NE 0.                                      "WRH PH4K002071
*---Error handling: Personnel number will be rejected
    CLEAR T510R.                       "Clear table work area
*   CONCATENATE p_molga p_trfar p_trfgb                  "WRH PH4K002071
*               p_trfkz p_trfgr p_trfst                  "WRH PH4K002071
*               p_vargu p_ehire p_endda                  "WRH PH4K002071
*               INTO table_arg SEPARATED BY space.       "WRH PH4K002071
    CONCATENATE P_MOLGA P_TRFAR P_TRFGB                  "WRH PH4K002071
                P_TRFKZ P_TRFGR P_TRFST                  "WRH PH4K002071
                P_VARGU P_EHIRE P_KEYDA                  "WRH PH4K002071
                INTO TABLE_ARG SEPARATED BY SPACE.       "WRH PH4K002071
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                    'T510R' TABLE_ARG SPACE SPACE.
    ADD 1 TO NO_REJECTED.              "Increment number of rejected ees
    PERFORM REJECT_PERNR.              "Process next personnel number
  ELSEIF
*---Check if limits are missing -> error
*   t510r-dauer IS INITIAL AND                           "WRH PH4K002071
*   t510r-spalt IS INITIAL AND                           "WRH PH4K002071
*   t510r-anzhl IS INITIAL.                              "WRH PH4K002071
    ( T510R-DAUER IS INITIAL AND                         "WRH PH4K002071
      T510R-SPALT IS INITIAL AND                         "WRH PH4K002071
      T510R-ANZHL IS INITIAL AND                         "WRH PH4K002071
      P_STVOR     IS INITIAL AND                         "WRH PH4K002071
      P_RECDA     IS INITIAL ) OR                        "WRH PH4K002071
    ( T510R-TRFFG IS INITIAL AND                         "WRH PH4K002071
      T510R-TRFFS IS INITIAL ) OR                        "WRH PH4K002071
    ( T510R-TRFFG IS INITIAL AND                         "WRH PH4K002071
      T510R-TRFFS EQ T510R-TRFST ) OR                    "WRH PH4K002071
    ( T510R-TRFFG EQ T510R-TRFGR AND                     "WRH PH4K002071
      T510R-TRFFS IS INITIAL ).                          "WRH PH4K002071
*---Missing limits from T510R-entry - personnel number is not
*   automatically reclassified
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '016'
                                    TEXT-E01 SPACE SPACE SPACE.
    ADD 1 TO NO_REJECTED.              "Increment number of rejected ees
    PERFORM REJECT_PERNR.              "Process next personnel number
  ENDIF.
ENDFORM.                               "RE_T510R

*---------------------------------------------------------------------*
*       FORM RE_T511                                                  *
*---------------------------------------------------------------------*
*       Read table T511 to determine all indirectly evaluated wage    *
*       types and fill internal table indir_lgart_tab.                *
*       In case of an error: add a message to the error list.         *
*---------------------------------------------------------------------*
*  -->  P_MOLGA            : Country grouping                         *
*  -->  P_ENDDA            : To                                       *
*  <--  P_INDIR_LGART_TAB  : Int. table of indirectly eval. wage types*
*---------------------------------------------------------------------*
FORM RE_T511 USING P_MOLGA LIKE T511-MOLGA
                   P_ENDDA LIKE T511-ENDDA
          CHANGING P_INDIR_LGART_TAB LIKE INDIR_LGART_TAB.
  DATA: TABLE_ARG(9),
        INDIR_LGART_WA TYPE WAGE_TYPE_STRUC.
*-Check if information has been previously read
  CHECK T511-MOLGA NE P_MOLGA OR T511-ENDDA NE P_ENDDA.
*-Fill internal table p_indir_lgart_tab with indirectly evaluated
* wage types from table T511
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE P_INDIR_LGART_TAB
    FROM T511 WHERE MOLGA EQ P_MOLGA
                AND ENDDA GE P_ENDDA
                AND MODNA NE SPACE.
*-Get wage type text for each indirectly evaluated wage type
  LOOP AT P_INDIR_LGART_TAB INTO INDIR_LGART_WA.
    SELECT LGTXT
      INTO CORRESPONDING FIELDS OF INDIR_LGART_WA
      FROM T512T WHERE SPRSL EQ SY-LANGU
                   AND MOLGA EQ P_MOLGA
                   AND LGART EQ INDIR_LGART_WA-LGART.
    ENDSELECT.
    IF SY-SUBRC NE 0.
*-----Error handling for missing T512T entry
      CLEAR T512T.                     "Clear table work area
      CONCATENATE SY-LANGU P_MOLGA INDIR_LGART_WA-LGART
                  INTO TABLE_ARG SEPARATED BY SPACE.
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'M' '001'
                                      'T512T' TABLE_ARG SPACE SPACE.
    ELSE.
      MODIFY P_INDIR_LGART_TAB FROM INDIR_LGART_WA.
    ENDIF.
  ENDLOOP.
ENDFORM.                               "RE_T511

*---------------------------------------------------------------------*
*       FORM RE_T528B                                                 *
*---------------------------------------------------------------------*
*       Read table T528B to determine all information relevant to     *
*       the employee's position/workplace.                            *
*       In case of an error: add a message to the error list.         *
*---------------------------------------------------------------------*
*  -->  P_OTYPE  : Object type                                        *
*  -->  P_PLANS  : Position/work center key                           *
*  -->  P_ENDDA  : To                                                 *
*---------------------------------------------------------------------*
FORM RE_T528B USING P_OTYPE LIKE T528B-OTYPE
                    P_PLANS LIKE T528B-PLANS
                    P_ENDDA LIKE T528B-ENDDA.
  DATA: TABLE_ARG(20).
*-Check if information has been previously read
  CHECK P_OTYPE NE T528B-OTYPE
     OR P_PLANS NE T528B-PLANS
     OR P_ENDDA NE T528B-ENDDA.
  SELECT SINGLE * FROM T528B WHERE OTYPE EQ P_OTYPE
                               AND PLANS EQ P_PLANS
                               AND ENDDA EQ P_ENDDA.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T528B.                         "Clear table work area
  CONCATENATE P_OTYPE P_PLANS P_ENDDA INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                    'T528B' TABLE_ARG SPACE SPACE.
ENDFORM.                               "RE_T528B

*---------------------------------------------------------------------*
*       FORM RE_T529A                                                 *
*---------------------------------------------------------------------*
*       Read table T529A to determine if infotype P0000 (Personal     *
*       Events) should be updated during the batch-input session      *
*       and to determine the infogroup.                               *
*---------------------------------------------------------------------*
*  -->  P_MASSN  : Event type                                         *
*---------------------------------------------------------------------*
FORM RE_T529A USING P_MASSN LIKE T529A-MASSN.
  DATA: TABLE_ARG(2).
*-Check if information has been previously read
  CHECK P_MASSN NE T529A-MASSN.
  SELECT SINGLE * FROM T529A WHERE MASSN EQ P_MASSN.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T529A.                         "Clear table work area
  TABLE_ARG+0(2)  = P_MASSN.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T529A' TABLE_ARG SPACE SPACE.
  ADD 1 TO NO_REJECTED.                "Increment number of rejected ees
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T529A

*&--------------------------------------------------------------------*
*&      Form  RE_T529T                                                *
*&--------------------------------------------------------------------*
*       Read table T529T to determine the text for the personnel event*
*---------------------------------------------------------------------*
*  -->  P_SPRSL  : Language key                                       *
*  -->  P_MASSN  : Personnel event to determine text of               *
*---------------------------------------------------------------------*
FORM RE_T529T USING P_SPRSL LIKE T529T-SPRSL
                    P_MASSN LIKE T529T-MASSN.
*-Check if information has been previously read
  CHECK T529T-SPRSL NE P_SPRSL
     OR T529T-MASSN NE P_MASSN.
  SELECT SINGLE * FROM T529T WHERE SPRSL EQ P_SPRSL
                               AND MASSN EQ P_MASSN.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T529T.
ENDFORM.                    " RE_T529T

*---------------------------------------------------------------------*
*       FORM RE_T530F                                                 *
*---------------------------------------------------------------------*
*       Read table T530F to determine the text for the reason         *
*---------------------------------------------------------------------*
*  -->  P_SPRSL  : Language key                                       *
*  -->  P_INFTY  : Infotype                                           *
*  -->  P_PREAS  : Reason for master data change                      *
*---------------------------------------------------------------------*
FORM RE_T530F USING P_SPRSL LIKE T530F-SPRSL
                    P_INFTY LIKE T530F-INFTY
                    P_PREAS LIKE T530F-PREAS.
*-Check if information has been previously read
  CHECK T530F-SPRSL NE P_SPRSL
     OR T530F-INFTY NE P_INFTY
     OR T530F-PREAS NE P_PREAS.
  SELECT SINGLE * FROM T530F WHERE SPRSL EQ P_SPRSL
                               AND INFTY EQ P_INFTY
                               AND PREAS EQ P_PREAS.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T530F.
ENDFORM.                               "RE_T530F

*---------------------------------------------------------------------*
*       FORM RE_T530T                                                 *
*---------------------------------------------------------------------*
*       Read table T530T to determine the text for the event reason   *
*---------------------------------------------------------------------*
*  -->  P_SPRSL  : Language key                                       *
*  -->  P_MASSN  : Personnel event to determine text of               *
*  -->  P_MASSG  : Reason for event                                   *
*---------------------------------------------------------------------*
FORM RE_T530T USING P_SPRSL LIKE T530T-SPRSL
                    P_MASSN LIKE T530T-MASSN
                    P_MASSG LIKE T530T-MASSG.
*-Check if information has been previously read
  CHECK T530T-SPRSL NE P_SPRSL
     OR T530T-MASSN NE P_MASSN
     OR T530T-MASSG NE P_MASSG.
  SELECT SINGLE * FROM T530T WHERE SPRSL EQ P_SPRSL
                               AND MASSN EQ P_MASSN
                               AND MASSG EQ P_MASSG.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T530T.
ENDFORM.                               "RE_T530T

*---------------------------------------------------------------------*
*       FORM RE_T539A                                                 *
*---------------------------------------------------------------------*
*       Read table T539A to get default wage type(s) and add them to  *
*       the internal table P_PROPOSED_LGART.                          *
*---------------------------------------------------------------------*
*  -->  P_BEGDA            : From                                     *
*  -->  P_BUKRS            : Company code                             *
*  -->  P_WERKS            : Personnel area                           *
*  -->  P_BTRTL            : Personnel subarea                        *
*  -->  P_PERSG            : Employee group                           *
*  -->  P_PERSK            : Employee subgroup                        *
*  -->  P_INFTY            : Infotype                                 *
*  -->  P_SUBTY            : Subtype                                  *
*  -->  P_MOLGA            : Country grouping                         *
*  <--  P_PROPOSED_LGART   : Internal table of proposed wage types    *
*  <--  P_LINE_COUNT TYPE  : Number of loop-lines in the P0008 dynpro *
*---------------------------------------------------------------------*
FORM RE_T539A USING P_BEGDA LIKE P0008-BEGDA
                    P_BUKRS LIKE P0001-BUKRS
                    P_WERKS LIKE P0001-WERKS
                    P_BTRTL LIKE P0001-BTRTL
                    P_PERSG LIKE P0001-PERSG
                    P_PERSK LIKE P0001-PERSK
                    P_INFTY LIKE P0001-INFTY
                    P_SUBTY LIKE P0001-SUBTY
                    P_MOLGA LIKE T001P-MOLGA
           CHANGING P_PROPOSED_LGART LIKE PROPOSED_LGART_TAB
                    P_LINE_COUNT TYPE LINE_COUNT.
  DATA: NUMBER_OF_LGART_IN_P0008(2) TYPE P VALUE 20,
        549BRC(2) TYPE P,
        LGANZ(2) TYPE N,
        GFW_SEP(1),
        GFW_LENGTH(2) TYPE P,
        LGMST(30).
   NUMBER_OF_LGART_IN_P0008 = NUMBER_OF_WAGETYPES_0008.        "N217573

*-Check if information has been previously read
* CHECK p_bukrs NE pme07-bukrs                           "WRH PH4K002071
*    OR p_werks NE pme07-werks                           "WRH PH4K002071
*    OR p_btrtl NE pme07-btrtl                           "WRH PH4K002071
*    OR p_persg NE pme07-persg                           "WRH PH4K002071
*    OR p_persk NE pme07-persk                           "WRH PH4K002071
*    OR p_infty NE pme07-infty                           "WRH PH4K002071
*    OR p_subty NE pme07-subty                           "WRH PH4K002071
*    OR p_molga NE pme07-molga.                          "WRH PH4K002071
  REFRESH P_PROPOSED_LGART.                              "WRH PH4K002071
*-Fill feature PME07
  CLEAR PME07.                                           "WRH PH4K002071
  MOVE P_BUKRS TO PME07-BUKRS.
  MOVE P_WERKS TO PME07-WERKS.
  MOVE P_BTRTL TO PME07-BTRTL.
  MOVE P_PERSG TO PME07-PERSG.
  MOVE P_PERSK TO PME07-PERSK.
  MOVE P_INFTY TO PME07-INFTY.
  MOVE 'A'     TO PME07-TCLAS.                                "N0485401
  IF P_SUBTY IS INITIAL.
    MOVE '0' TO PME07-SUBTY.
  ELSE.
    MOVE P_SUBTY TO PME07-SUBTY.
  ENDIF.
  MOVE P_MOLGA TO PME07-MOLGA.
*-Read feature
  PERFORM RE549D USING 'LGMST' '1' LGMST 549BRC.
*-Get number of loop-lines in P0008 (Basic Pay) dynpro and
* fill internal table PROPOSED_LGART with default wage type(s)
  IF 549BRC NE 0.
    MOVE NUMBER_OF_LGART_IN_P0008 TO P_LINE_COUNT.
  ELSE.
    PERFORM GET_FIRST_WORD(SAPFP500) USING
            LGMST '/' LGANZ GFW_SEP GFW_LENGTH.
    IF LGANZ NE 0.
      MOVE LGANZ TO P_LINE_COUNT.
    ELSE.
      MOVE NUMBER_OF_LGART_IN_P0008 TO P_LINE_COUNT.
    ENDIF.
*---Fill internal table with proposed wage types
    SELECT * INTO CORRESPONDING FIELDS OF TABLE P_PROPOSED_LGART
             FROM T539A WHERE MOLGA EQ P_MOLGA
                          AND LGMST EQ LGMST
                          AND BEGDA LE P_BEGDA
                          AND ENDDA GE P_BEGDA
                        ORDER BY PRIMARY KEY.
  ENDIF.
ENDFORM.                               "RE_T539A

*---------------------------------------------------------------------*
*       FORM RE_T549A                                                 *
*---------------------------------------------------------------------*
*       Read table T549A with the payroll area to get the period      *
*       modifier.                                                     *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  -->  P_ABKRS  : Payroll area                                       *
*---------------------------------------------------------------------*
FORM RE_T549A USING P_ABKRS LIKE T549A-ABKRS.
  DATA: TABLE_ARG(2).
*-Check if information has been previously read
  CHECK T549A-ABKRS NE P_ABKRS.
  SELECT SINGLE * FROM T549A WHERE ABKRS EQ P_ABKRS.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T549A.                         "Clear table work area
  TABLE_ARG+0(2) = P_ABKRS.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T549A' TABLE_ARG SPACE SPACE.
  ADD 1 TO NO_REJECTED.                "Increment number of rejected ees
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T549A

*---------------------------------------------------------------------*
*       FORM RE_T549Q                                                 *
*---------------------------------------------------------------------*
*       Read table T549Q with the period modifier and fill the        *
*       internal table P_I549Q_TAB.                                   *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  -->  P_BEGDA      : From                                           *
*  -->  P_ENDDA      : To                                             *
*  -->  P_PERMO      : Period parameter                               *
*  <--  P_I549Q_TAB  : Internal table of payroll periods              *
*---------------------------------------------------------------------*
FORM RE_T549Q USING P_BEGDA LIKE T549Q-BEGDA
                    P_ENDDA LIKE T549Q-ENDDA
                    P_PERMO LIKE T549Q-PERMO
           CHANGING P_I549Q_TAB LIKE I549Q_TAB.
*-Check if information has been previously read
* check p_begda ne t549q-begda                           "WRH AHRK028021
*    or p_endda ne t549q-endda                           "WRH AHRK028021
*    or p_permo ne t549q-permo.                          "WRH AHRK028021
*-Fill internal table P_I549Q_TAB
  REFRESH P_I549Q_TAB.                                   "WRH PH4K002071
  SELECT * FROM T549Q WHERE PERMO EQ P_PERMO
                        AND BEGDA LE P_ENDDA
                        AND ENDDA GE P_BEGDA
                      ORDER BY PRIMARY KEY.
    APPEND T549Q TO P_I549Q_TAB.
  ENDSELECT.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T549Q.                         "Clear table work area
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T549Q' P_PERMO P_BEGDA P_ENDDA.
  ADD 1 TO NO_REJECTED.                "Increment number of rejected ees
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T549Q

*---------------------------------------------------------------------*
*       FORM RE_T551C                                                 *
*---------------------------------------------------------------------*
*       Read table T551C to do absence evaluation.                    *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  -->  P_MOTPR  : Personnel subarea grouping for daily work schedule *
*  -->  P_ZMODN  : Period work schedule                               *
*  -->  P_DATUM  : To                                                 *
*---------------------------------------------------------------------*
FORM RE_T551C USING P_MOTPR LIKE T551C-MOTPR
                    P_ZMODN LIKE T551C-ZMODN
                    P_DATUM LIKE T551C-ENDDA.
  DATA: TABLE_ARG(16).
*-Check if information has been previously read
  CHECK P_MOTPR NE T551C-MOTPR
     OR P_ZMODN NE T551C-ZMODN
     OR P_DATUM GT T551C-ENDDA
     OR P_DATUM LT T551C-BEGDA.
  SELECT * FROM T551C WHERE MOTPR EQ P_MOTPR
                      AND   ZMODN EQ P_ZMODN
                      AND   ENDDA GE P_DATUM
                      AND   BEGDA LE P_DATUM ORDER BY PRIMARY KEY.
    EXIT.
  ENDSELECT.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T551C.                         "Clear table work area
  CONCATENATE P_MOTPR P_ZMODN P_DATUM INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T551C' TABLE_ARG SPACE SPACE.
  ADD 1 TO NO_REJECTED.                "Increment number of rejected ees
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T551C

*---------------------------------------------------------------------*
*       FORM RE_T554S                                                 *
*---------------------------------------------------------------------*
*       Read table T554S to process absences and attendances.         *
*       In case of an error: add the error to the error list and      *
*                            process the next personnel number.       *
*---------------------------------------------------------------------*
*  -->  P_MOABW  : Personnel subarea grouping for abs./attendance type*
*  -->  P_SUBTY  : Absence or attendance type                         *
*  -->  P_DATUM  : To                                                 *
*---------------------------------------------------------------------*
FORM RE_T554S USING P_MOABW LIKE T554S-MOABW
                    P_SUBTY LIKE T554S-SUBTY
                    P_DATUM LIKE T554S-ENDDA.
  DATA: TABLE_ARG(16).
*-Check if information has been previously read
  CHECK P_MOABW NE T554S-MOABW
     OR P_SUBTY NE T554S-SUBTY
     OR P_DATUM LT T554S-BEGDA
     OR P_DATUM GT T554S-ENDDA.
  SELECT * FROM T554S WHERE MOABW EQ P_MOABW
                      AND   SUBTY EQ P_SUBTY
                      AND   BEGDA LE P_DATUM
                      AND   ENDDA GE P_DATUM.
    EXIT.
  ENDSELECT.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T554S.
  CONCATENATE P_MOABW P_SUBTY P_DATUM INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T554S' TABLE_ARG SPACE SPACE.
  ADD 1 TO NO_REJECTED.                "Increment number of rejected ees
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T554S

*---------------------------------------------------------------------*
*       Form  RE_588C                                                 *
*---------------------------------------------------------------------*
*       Read table T588C to determine the user group                  *
*---------------------------------------------------------------------*
*  -->  P_MENUTYPE  : Menu type                                       *
*  -->  P_MENU      : Infotype menu (in this case the infogroup )     *
*  -->  P_MSGETYPE  : Message type                                    *
*  <--  P_USERGROUP : User group                                      *
*---------------------------------------------------------------------*
FORM RE_588C USING    P_MENUTYPE  LIKE T588C-MNTYP
                      P_MENU      LIKE T588C-MENUE
                      P_MSGETYPE  LIKE T588C-MNREA
             CHANGING P_USERGROUP LIKE T588C-REFBG.
DATA: DEFAULT_USER_GROUP LIKE T588C-REFBG VALUE '00'.
DATA: SUBRC LIKE SY-SUBRC.                               "WRH PH4K002071
  CLEAR SUBRC.                                           "WRH PH4K002071
*-Check if information has been previously read
* CHECK t588c-mntyp NE p_menutype                        "WRH PH4K002071
*    OR t588c-menue NE p_menu.                           "WRH PH4K002071
  IF T588C-MNTYP NE P_MENUTYPE OR                        "WRH PH4K002071
     T588C-MENUE NE P_MENU.                              "WRH PH4K002071
    SELECT SINGLE * FROM T588C WHERE MNTYP EQ P_MENUTYPE
                                 AND MENUE EQ P_MENU.
    SUBRC = SY-SUBRC.                                    "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071
* IF sy-subrc EQ 0.                                      "WRH PH4K002071
  IF SUBRC EQ 0.                                         "WRH PH4K002071
*---Entry exists
    IF NOT T588C-MNBGR IS INITIAL.
*-----Entry is user group dependent -> read user group parameter
      GET PARAMETER ID 'UGR' FIELD P_USERGROUP.
      IF P_USERGROUP IS INITIAL.
*-------No user group defined -> now we might have a problem!
        CASE T588C-MNREA.
*---------Act according to system reaction
          WHEN 'E'.
*-----------Error -> act according to message type
            CASE P_MSGETYPE.
              WHEN 'S'.
                PERFORM APPEND_ERROR_LIST USING PERNR-PERNR
                                                'PG'
                                                'E'
                                                '010'
                                                SPACE
                                                SPACE
                                                SPACE
                                                SPACE.
*---------------Set to default value and continue processing
                MOVE DEFAULT_USER_GROUP TO P_USERGROUP.
              WHEN OTHERS.
*---------------Reject personnel number
                PERFORM APPEND_ERROR_LIST USING PERNR-PERNR
                                                'PG'
                                                'E'
                                                '010'
                                                SPACE
                                                SPACE
                                                SPACE
                                                SPACE.
                CLEAR T588C.
                ADD 1 TO NO_REJECTED.  "Increment number of rejected ees
                PERFORM REJECT_PERNR.  "Process next personnel number
            ENDCASE.
          WHEN 'W'.
*-----------Warning -> act according to message type
            CASE P_MSGETYPE.
              WHEN 'S'.
                PERFORM APPEND_ERROR_LIST USING PERNR-PERNR
                                                'PG'
                                                'S'
                                                '010'
                                                SPACE
                                                SPACE
                                                SPACE
                                                SPACE.
              WHEN OTHERS.
                PERFORM APPEND_ERROR_LIST USING PERNR-PERNR
                                                'PG'
                                                'W'
                                                '010'
                                                SPACE
                                                SPACE
                                                SPACE
                                                SPACE.
            ENDCASE.
            MOVE T588C-REFBG TO P_USERGROUP.
          WHEN OTHERS.
            MOVE T588C-REFBG TO P_USERGROUP.
        ENDCASE.
      ENDIF.
    ELSE.
      MOVE T588C-REFBG TO P_USERGROUP.
    ENDIF.
  ELSE.
*---No table entry found -> clear table and use default usergroup
    CLEAR T588C.
    MOVE DEFAULT_USER_GROUP TO P_USERGROUP.
  ENDIF.
ENDFORM.                               " RE_588C

*---------------------------------------------------------------------*
*       FORM RE_T588D                                                 *
*---------------------------------------------------------------------*
*       Read table T588D to fill the internal table INFOTYPE_TAB with *
*       the infotypes and subtypes to be processed at a later time.   *
*---------------------------------------------------------------------*
*  -->  P_ITYGR         : Infotype group number                       *
*  -->  P_IGMOD         : Infogroup modifier                          *
*  <--  P_INFOGROUP_TAB : Internal table to keep info-/subtypes       *
*---------------------------------------------------------------------*
FORM RE_T588D USING    P_ITYGR LIKE T588D-ITYGR
                       P_IGMOD LIKE T588D-IGMOD
              CHANGING P_INFOGROUP_TAB LIKE INFOGROUP_TAB.
  DATA: USERGROUP LIKE T588C-REFBG,
        INFOGROUP_WA LIKE PITGR,
        TABLE_ARG(13).
*-Check if information has been previously read
* CHECK t588d-itygr NE p_itygr                           "WRH PH4K002071
*    OR t588d-igmod NE p_igmod.                          "WRH PH4K002071
*-Determine user group
  PERFORM RE_588C USING    'G'
                           P_ITYGR
                           SPACE
                  CHANGING USERGROUP.
*-Fill the internal table P_INFOGROUP_TAB from T588D
  SELECT * FROM T588D WHERE ITYGR EQ P_ITYGR
                        AND INFTY NE '0000'
                        AND USERG EQ USERGROUP
                        AND IGMOD EQ P_IGMOD
           ORDER BY PRIMARY KEY.
    CLEAR INFOGROUP_WA.
    MOVE T588D-INFTY TO INFOGROUP_WA-INFTY.
    IF T588D-SUBTY IS INITIAL.
      MOVE '0' TO INFOGROUP_WA-SUBTY.
    ELSE.
      MOVE T588D-SUBTY TO INFOGROUP_WA-SUBTY.
    ENDIF.
    MOVE T588D-ACTIO TO INFOGROUP_WA-OPERA.
    MOVE T588D-ITBLD TO INFOGROUP_WA-ITBLD.
    MOVE 1 TO INFOGROUP_WA-ANZHL.
    IF T588D-ACTIO EQ 'INS'.
      COLLECT INFOGROUP_WA INTO P_INFOGROUP_TAB.
    ELSE.
      APPEND INFOGROUP_WA TO P_INFOGROUP_TAB.
    ENDIF.
  ENDSELECT.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T588D.
  CONCATENATE P_ITYGR USERGROUP P_IGMOD
              INTO TABLE_ARG SEPARATED BY SPACE.
  PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '001'
                                  'T588D' TABLE_ARG SPACE SPACE.
  ADD 1 TO NO_REJECTED.                "Increment number of rejected ees
  PERFORM REJECT_PERNR.                "Process next personnel number
ENDFORM.                               "RE_T588D

*---------------------------------------------------------------------*
*       FORM RE_T591S                                                 *
*---------------------------------------------------------------------*
*       Read table T591S to determine the text for the subtype        *
*---------------------------------------------------------------------*
*  -->  P_SPRSL  : Language key                                       *
*  -->  P_INFTY  : Infotype                                           *
*  -->  P_SUBTY  : Subtype                                            *
*---------------------------------------------------------------------*
FORM RE_T591S USING P_SPRSL LIKE T591S-SPRSL
                    P_INFTY LIKE T591S-INFTY
                    P_SUBTY LIKE T591S-SUBTY.
*-Check if information has been previously read
  CHECK T591S-SPRSL NE P_SPRSL
     OR T591S-INFTY NE P_INFTY
     OR T591S-SUBTY NE P_SUBTY.
  SELECT SINGLE * FROM T591S WHERE SPRSL EQ P_SPRSL
                               AND INFTY EQ P_INFTY
                               AND SUBTY EQ P_SUBTY.
  CHECK SY-SUBRC NE 0.
*-Error handling
  CLEAR T591S.
ENDFORM.                               "RE_T591S



*---------------------------------------------------------------------*
*       FORM RE_0001_TRFKZ_MOLGA                                      *
*---------------------------------------------------------------------*
*       Determine infotype 0001, TRFKZ and MOLGA in a given period    *
*---------------------------------------------------------------------*
*  -->  P_SPRSL  : Language key                                       *
*  -->  P_INFTY  : Infotype                                           *
*  -->  P_SUBTY  : Subtype                                            *
*---------------------------------------------------------------------*
FORM RE_0001_TRFKZ_MOLGA USING P_BEGDA LIKE SY-DATUM     "WRH AHRK041998
                               P_ENDDA LIKE SY-DATUM
                               P_ADD   TYPE C.

  RP_PROVIDE_FROM_LAST P0001 SPACE P_BEGDA P_ENDDA.
*-Check if record exists
  IF PNP-SW-FOUND NE '1'.
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '103'
                                    PERNR-PERNR 'P0001' SPACE SPACE.
    IF NOT P_ADD IS INITIAL.
      ADD 1 TO NO_REJECTED.
    ENDIF.
    PERFORM REJECT_PERNR.
  ENDIF.

*-Determine payscale type and payscale area from table T001P
  PERFORM RE_T001P USING P0001-WERKS P0001-BTRTL P_ADD.

*-Determine MOLGA from T500P
  PERFORM RE_T500P USING P0001-WERKS P_ADD.

*-Determine modifiers from table T503
  PERFORM RE_T503 USING P0001-PERSG P0001-PERSK P_ADD.

ENDFORM.                               "RE_0001_TRFKZ_MOLGA
