*----------------------------------------------------------------------*
* Name: RPITIG00     INTERNATIONAL                                     *
*       This report creates a new Basic Pay (P0008) record to reflect  *
*       a payscale reclassification based on the decision criteria     *
*       contained in table T510R (Dynamic Payscale Reclassifications)  *
*----------------------------------------------------------------------*
* Support Package 4.6C                                                 *
* WRH L9CK045673 12032001 - Entry date and leaving date cleared for    *
*                           each personnel number.                     *
* QNU L9CK007324 28122000 - Note 217573                                *
* XPS L9CK016100 13Jul'00 - (N0310914)                                 *
* 4.6C                                                                 *
* WRH PH0K000464 08121999 - DIVGV and BSGRD as in IT0008, not in BI    *
*                         - Name of Batch Input Map without SY-UZEIT.  *
* 4.6A                                                                 *
* WRH AHRK063507 18111999 - Minor corrections.                         *
* WRH PH9K003008 03051999 - Message classification in the case of a    *
*                           future reclassification date changed.      *
* WRH AHRK049440 08041999 - Problem in determination of reclass. date  *
*                           based on Time-In-Grade decision corrected. *
* DB  AHRK041998 19031999 - Selection screen changed (due to usability)*
* WRH AHRK041998 10031999 - Read infotype 0001, MOLGA, TRFKZ with      *
*                           correct dates.                             *
*                         - Currency conversion corrected.             *
* WRH AHRK029426 19111998 - Begin date of current pay scale information*
*                           in reclassification table corrected.       *
*                         - Problem in determination of reclass. date  *
*                           based on Time-In-Grade decision corrected. *
* WRH AHRK028021 02111998 - Problem in indirect evaluation corrected.  *
*                         - Problem in determination of payroll        *
*                           periods corrected.                         *
*                         - Filling unit field in BDC corrected.       *
* WRH AHRK023507 28081998 - Take infotype Planned Compensation into    *
*                           account for selection                      *
*                         - Date format in error messages corrected    *
*                         - Wage type sorting corrected                *
*                         - Print error message in case of invalid     *
*                           reclassification date                      *
* 4.5A                                                                 *
* WRH PH4K002071 06071998 Several corrections (e.g. wage type sorting) *
* WRH AHRK018189 07051998 Parameter LOOP_LINES changed because of the  *
*                         step loop to table control conversion of the *
*                         IT0008 dynpros.                              *
* WRH AHRK011379 15041998 Form routine DET_HIRE_FIRE replaced by       *
*                         new form routine DET_ENTRY_LEAVING_DATES     *
*                         which uses the new function modules          *
*                         HR_ENTRY_DATE and HR_LEAVING_DATE instead of *
*                         RP_HIRE_FIRE.                                *
* 4.0A                                                                 *
* WRH ALRK054526 30101997 - Problem in determination of BSGRD and      *
*                           DIVGV solved                               *
*                         - several other problems corrected           *
* WRH ALRK044773 18081997 - Because of the new name space the names of *
*                           the of log. database variables PN/... were *
*                           changed to PN-... and the name of the RMAC *
*                           module RP-PROVIDE-FROM-LAST was changed to *
*                           RP_PROVIDE_FROM_LAST (not marked).         *
*                         - Function module RP_ZEINH_GET built in to   *
*                           replace RE_510F, because T510F-ZEINH and   *
*                           feature U510F are not in use anymore.      *
*                         - Decoupling: Table T100 or declarations     *
*                           referring to T100-fields must not be used  *
*                           anymore                                    *
* 3.1H                                                                 *
* XRK P30K129109 24061997 Customerexit for filling Variable Saldo      *
*                         added, other small corrections/enhancements  *
*                         (modified lines are not marked)              *
* 3.1G                                                                 *
* XRK P30K069471 17011997 Initial release                              *
* Date       Developer       Request        Description
* 11/13/06   Manju           UD1K922999     Modified original logic to
*                                           populate next increment
*                                           date / Next Pay scale
*----------------------------------------------------------------------*
REPORT ZHR_RPITIG00 MESSAGE-ID 72.

*-----Global data
INCLUDE ZRPITIG01.
*INCLUDE RPITIG01.

*-----Report selection screen definitions
INCLUDE ZRPITIG02.
*INCLUDE RPITIG02.

*-----Routines to read tables
INCLUDE ZRPITIG03.
*INCLUDE RPITIG03.

*-----Batch input routines
INCLUDE ZRPITIG04.
*INCLUDE RPITIG04.

*---------------------------------------------------------------------*
*                     INITIALIZATION                                  *
*---------------------------------------------------------------------*
INITIALIZATION.
*-Fill table SEL_TAB for RP_OPTIONS_INTO_STRING
  PERFORM FILL_SEL_TAB.
*-Supply text for tab strips
  SEL_OPT = TEXT-SEL.
  STE_OPT = TEXT-STE.
  BI_PARA = TEXT-BTC.

*-Authority check for Batch-Input session
  REPNAME = SY-REPID.
  CALL FUNCTION 'HR_MAPNAME_VERIFY'
       EXPORTING
            MAPNAME    = MAP_NAME
            REPORTNAME = REPNAME
       IMPORTING
            MAPNAME    = MAP_NAME.

  PNPTIMED = 'D'.                      "Propose current day
  RP-SEL-EIN-AUS-INIT.                 "Propose only active employees

*-Preset text on pushbuttons
  MOVE 'Maßnahme/Grund tarifliche Umstufung'(TUM) TO EVNT+4.
  MOVE 'Nur Basisbezug mit Subtyp'(SUB) TO STYP+4.
  MOVE 'Batch-Input-Mappe'(BI1) TO BTCI+4.
  MOVE 'Direkt verbuchen'(ON1) TO ONLN+4.
  MOVE 'Nur Protokoll'(PR1) TO PROT+4.

*-Define option list ONLY_EQ for SELECT-OPTION RCLS_TYP
* ONLY_EQ: only EQ(ual) is allowed as an option
  MOVE 'ONLY_EQ'   TO OPT_LIST-NAME.
  MOVE 'X'         TO OPT_LIST-OPTIONS-EQ.
  APPEND OPT_LIST  TO RESTRICT-OPT_LIST_TAB.
*-Associate SELECT-OPTION RCLS_TYP with option list ONLY_EQ and sign I
  MOVE: 'S'        TO ASSOC-KIND,
        'RCLS_TYP' TO ASSOC-NAME,
        'I'        TO ASSOC-SG_MAIN,
        ' '        TO ASSOC-SG_ADDY,
        'ONLY_EQ'  TO ASSOC-OP_MAIN,
        'ONLY_EQ'  TO ASSOC-OP_ADDY.
  APPEND ASSOC TO RESTRICT-ASS_TAB.
*-Now restrict the SELECT-OPTION RCLS_TYP
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
       EXPORTING
            RESTRICTION                = RESTRICT
*           DB                         = ' '
       EXCEPTIONS
*           TOO_LATE                   = 1
*           REPEATED                   = 2
*           NOT_DURING_SUBMIT          = 3
*           DB_CALL_AFTER_REPORT_CALL  = 4
*           SELOPT_WITHOUT_OPTIONS     = 5
*           SELOPT_WITHOUT_SIGNS       = 6
*           INVALID_SIGN               = 7
*           REPORT_CALL_AFTER_DB_ERROR = 8
*           EMPTY_OPTION_LIST          = 9
*           INVALID_KIND               = 10
*           REPEATED_KIND_A            = 11
            OTHERS                     = 12.

*-Preset default pay scale reclassification types in table RCLS_TYP
  MOVE: 'I'  TO RCLS_TYP-SIGN,
        'EQ' TO RCLS_TYP-OPTION,
        '01' TO RCLS_TYP-LOW.
  APPEND RCLS_TYP.
  MOVE: 'I'  TO RCLS_TYP-SIGN,
        'EQ' TO RCLS_TYP-OPTION,
        '02' TO RCLS_TYP-LOW.
  APPEND RCLS_TYP.
  MOVE: 'I'  TO RCLS_TYP-SIGN,
        'EQ' TO RCLS_TYP-OPTION,
        '03' TO RCLS_TYP-LOW.
  APPEND RCLS_TYP.
  MOVE: 'I'  TO RCLS_TYP-SIGN,
        'EQ' TO RCLS_TYP-OPTION,
        '04' TO RCLS_TYP-LOW.
  APPEND RCLS_TYP.
  MOVE: 'I'  TO RCLS_TYP-SIGN,
        'EQ' TO RCLS_TYP-OPTION,
        '05' TO RCLS_TYP-LOW.
  APPEND RCLS_TYP.

*---------------------------------------------------------------------*
*                     START-OF-SELECTION                              *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  RP-SET-NAME-FORMAT.                  "Read T522F / $$FORMAT = '01'
  CALL FUNCTION 'RP_FETCH_ALTERNATE_PERNR'
       IMPORTING
            ALTER_PERNR = SHOW_PERID
            RETCODE     = RETCD
       EXCEPTIONS
            OTHERS      = 1.

*-Authority check for batch-input session
  CALL FUNCTION 'HR_MAPNAME_VERIFY'
       EXPORTING
            MAPNAME    = MAP_NAME
            REPORTNAME = REPNAME
       IMPORTING
            MAPNAME    = MAP_NAME.

*-Assign generation mode for 'Online' processing
  IF PROC_TYP EQ '2'.
    IF NOT SW_GEN_A IS INITIAL.
      GEN_MODE = 'A'.                  "Display dynpros
    ENDIF.
    IF NOT SW_GEN_N IS INITIAL.
      GEN_MODE = 'N'.                  "Do not display dynpros
    ENDIF.
    IF NOT SW_GEN_E IS INITIAL.
      GEN_MODE = 'E'.                  "Display only errors
    ENDIF.
  ENDIF.

*-Check for reclassification type '02' if a reclass. date is given
  IF NOT REC_DATE IS INITIAL.
*---A reclassification date has been provided
    READ TABLE RCLS_TYP WITH KEY 'IEQ02'.
    IF SY-SUBRC NE 0.
*-----Entry is missing -> add it
      MOVE: 'I'  TO RCLS_TYP-SIGN,
            'EQ' TO RCLS_TYP-OPTION,
            '02' TO RCLS_TYP-LOW.
      APPEND RCLS_TYP.
    ENDIF.
  ENDIF.

*-Assign field-symbols for sort criteria
  PERFORM ASSIGN_SORTFIELDS_TO_ORGS USING SEL_ORG.

*-Move infotype P0008 to internal table INFOGROUP_TAB
  PERFORM MOVE_P0008_TO_INFOGROUP USING    P8_SUBTY
                                  CHANGING INFOGROUP_TAB.

*-Initialize global parameters and tables
  PERFORM INITIALIZE_DATA.                               "WRH PH4K002071

*---------------------------------------------------------------------*
*                        GET PERNR                                    *
*---------------------------------------------------------------------*
GET PERNR.

*-Check for optional select-options
*  check: ps_type, ps_area, ps_group, ps_level.          "WRH AHRK023507

*-Perform pre-processing of the personnel number
* PERFORM pre_process_pernr.                             "WRH PH4K002071
  PERFORM PRE_PROCESS_PERNR USING PN-BEGDA               "WRH PH4K002071
                                  PN-ENDDA.              "WRH PH4K002071
* Fill table SORTDATATAB
  PERFORM FILL_SORTDATATAB.                              "WRH PH4K002071

*-Provide progress information
  PERFORM SHOW_PROGRESS USING TEXT-PER
                              PERNR_COUNT.

*-Determine modifiers from table T503
* PERFORM RE_T503 USING P0001-PERSG P0001-PERSK.         "WRH AHRK041998

*-Determine the employee's age
  PERFORM CALC_EMPLOYEE_AGE USING P0002-GBDAT
                                  PN-ENDDA
                         CHANGING EMPLOYEE_AGE.

*-Determine the employee's seniority
  PERFORM CALC_SENIORITY USING HIRE_DATE
                               PN-ENDDA
                      CHANGING YEARS_OF_SENIORITY.

*-Fill internal table INFOGROUP_TAB with infotypes and subtypes
* according to the event
  PERFORM FILL_INFOGROUP_TAB USING    TUMMASSN+0(2)
                                      P0001              "WRH AHRK041998
                                      T500P-MOLGA
                             CHANGING INFOGROUP_TAB.

*-Determine the payscale reclassifications for the infotypes and
* subtypes contained in the internal table INFOGROUP_TAB
  PERFORM PROCESS_INFOGROUP_TAB USING INFOGROUP_TAB.

*-Process the data according to the processing type (Batch-Input/Online)
  PERFORM PROCESS_DATA USING PROC_TYP.

*---------------------------------------------------------------------*
*                      END-OF-SELECTION                               *
*---------------------------------------------------------------------*
END-OF-SELECTION.

*-Close Batch-Input session (if applicable)
  IF PROC_TYP EQ '1' AND NO_SCREENS GT 0.
    CALL FUNCTION 'BDC_CLOSE_GROUP'.
  ENDIF.

*-Check if any persons were selected
  IF PERNR_COUNT EQ 0.
    MESSAGE ID 'PN' TYPE 'S' NUMBER '050'. EXIT.
  ENDIF.

*-Display list of payscale reclassifications and errors/messages
  PERFORM DISPLAY_PROTOCOL.

*---------------------------------------------------------------------*
*                      AT USER-COMMAND                                *
*---------------------------------------------------------------------*
AT USER-COMMAND.
  CASE SY-UCOMM.
    WHEN 'BTCI'.
      SET PARAMETER ID 'MPN' FIELD MAP_NAME.
      CALL TRANSACTION 'SM35' AND SKIP FIRST SCREEN.
  ENDCASE.

*--------------------------Subroutines--------------------------------*


*---------------------------------------------------------------------*
*       FORM REJECT_PERNR                                             *
*---------------------------------------------------------------------*
*       Rejects the currently processed personnel number.             *
*       Having this as a separate routine allows a reject at any      *
*       level in the coding.                                          *
*---------------------------------------------------------------------*
FORM REJECT_PERNR.
  REJECT.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND_ERROR_LIST                                        *
*---------------------------------------------------------------------*
*       Appends an error to the error list.                           *
*       PARAMETERS:                                                   *
*              P_PERNR: Personnel number where the error occurred     *
*              P_AREA : Message area for the error                    *
*              P_MSGTY: Message type for the error                    *
*              P_MSGNO: Message number of the error                   *
*              P_MSGV1: First message variable                        *
*              P_MSGV2: Second message variable                       *
*              P_MSGV3: Third message variable                        *
*              P_MSGV4: Fourth message variable                       *
*---------------------------------------------------------------------*
FORM APPEND_ERROR_LIST USING P_PERNR LIKE PERNR-PERNR
                             P_AREA  LIKE HRERROR-ARBGB
                             P_MSGTY LIKE HRERROR-MSGTY
                             P_MSGNO LIKE HRERROR-MSGNO
                             P_MSGV1 TYPE ANY
                             P_MSGV2 TYPE ANY
                             P_MSGV3 TYPE ANY
                             P_MSGV4 TYPE ANY.
  DATA: ERR_TAB_WA LIKE HRERROR.
  ERR_TAB_WA-PERNR = P_PERNR.
  ERR_TAB_WA-ARBGB = P_AREA.
  ERR_TAB_WA-MSGTY = P_MSGTY.
  ERR_TAB_WA-MSGNO = P_MSGNO.
  ERR_TAB_WA-MSGV1 = P_MSGV1.
  ERR_TAB_WA-MSGV2 = P_MSGV2.
  ERR_TAB_WA-MSGV3 = P_MSGV3.
  ERR_TAB_WA-MSGV4 = P_MSGV4.
  APPEND ERR_TAB_WA TO ERR_TAB.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  INIT
*&---------------------------------------------------------------------*
*       Perform initializations necessary to process the next person   *
*----------------------------------------------------------------------*
FORM INIT.
*-Initialize PCL2 buffer
  RP-INIT-BUFFER.
*-Reset cluster B2 import dates
  CLEAR: IMPORT_B2_BEGDA, "Reset the start date for importing cluster B2
         IMPORT_B2_ENDDA. "Reset the end date for importing Cluster B2
*-Reset RECLASS_DATE
  CLEAR RECLASS_DATE.
  CLEAR TYPE_PS_RECLASS.                                 "WRH AHRK023507
*-Reset internal tables for Time-In-Grade calculations
* clear  : ivs_tab, icvs_tab, i549q_tab.                 "WRH AHRK028021
* refresh: ivs_tab, icvs_tab, i549q_tab.                 "WRH AHRK028021
  CLEAR  : IVS_TAB, ICVS_TAB.                            "WRH AHRK028021
  REFRESH: IVS_TAB, ICVS_TAB.                            "WRH AHRK028021
ENDFORM.                               " INIT

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_PROTOCOL
*&---------------------------------------------------------------------*
*       Display Table Control                                          *
*----------------------------------------------------------------------*
FORM DISPLAY_PROTOCOL.
  DATA: LINE_COUNT TYPE I,
        DATE_SELECTION(60),            "Header: show selected day/period
        HEADER_2_TEXT(60),             "Header 2
        HEADER_3_TEXT(60),             "Header 3
        CONVERT_DATE(10),
        NO_PERNR_TXT(60),              "Footer: number of selected PERNR
        NO_PERNR(7),                   "Max. no. of 9.999.999 PERNR sel.
        NO_RECLASS_TXT(60),            "Footer: number of reclassific.
        NO_RECLASSIFIED(7),            "Max. no. of 9.999.999 reclassif.
        NO_REJECT_TXT(60),             "Footer: number of rejected empl.
        NUMBER_REJECTED(7),            "Max. no. of 9.999.999 rejects
        START_BATCH(20),               "Text for pushbutton Batch-Input
        SHOW_HISTORY(20),              "Text for pushbutton History
        TITLE LIKE SY-TITLE,                             "WRH AHRK023507
        REPID LIKE SY-REPID,                             "WRH AHRK023507
        RETCODE LIKE SY-SUBRC.

*=Prepare header information
  IF PN-BEGDA EQ PN-ENDDA.
*---Only one day selected
    DATE_SELECTION = 'Tarifliche Umstufungen zum Stichtag &'(STT).
    WRITE PN-BEGDA TO CONVERT_DATE DD/MM/YYYY.
    REPLACE '&' WITH CONVERT_DATE INTO DATE_SELECTION.
  ELSE.
*---Time period selected
    DATE_SELECTION = 'Tarifliche Umstufungen im Zeitraum &1 - &2'(PRD).
    WRITE PN-BEGDA TO CONVERT_DATE DD/MM/YYYY.
    REPLACE '&1' WITH CONVERT_DATE INTO DATE_SELECTION.
    WRITE PN-ENDDA TO CONVERT_DATE DD/MM/YYYY.
    REPLACE '&2' WITH CONVERT_DATE INTO DATE_SELECTION.
  ENDIF.
*-Header 2 and 3
  IF NOT PAST_PS IS INITIAL.
    HEADER_2_TEXT = 'Verpaßte Umstufung(en) auswerten'(HD2).
  ENDIF.
  IF NOT FUT_PS IS INITIAL.
    HEADER_3_TEXT = 'Zukünftige Umstufung(en) auswerten'(HD3).
  ENDIF.

*=Prepare footer information
*-Number of selected personnel numbers
  NO_PERNR = PERNR_COUNT.
  NO_PERNR_TXT  = 'Anzahl selektierter Personalnummern: &'(FT0).
  REPLACE '&' WITH NO_PERNR INTO NO_PERNR_TXT.
*-Number of reclassifications
  DESCRIBE TABLE P0008_RESULT_TAB LINES LINE_COUNT.
  NO_RECLASSIFIED = LINE_COUNT.
  NO_RECLASS_TXT  = 'Anzahl der tariflichen Umstufungen: &'(FT1).
  REPLACE '&' WITH NO_RECLASSIFIED INTO NO_RECLASS_TXT.
*-Number of rejected personnel numbers (due to errors)
  NUMBER_REJECTED = NO_REJECTED.
  NO_REJECT_TXT = 'Anzahl abgelehnter Personalnummern: &'(FT2).
  REPLACE '&' WITH NUMBER_REJECTED INTO NO_REJECT_TXT.

*-Insert column names
  PERFORM FILL_FIELDNAMES.

*-Sort and merge output data according to order criteria
  PERFORM SORT_ORDER_OUTPUT USING P0008_RESULT_TAB
                         CHANGING P0008_TC_DATA_TAB.

*=Prepare pushbuttons
*-Prepare text for Batch-Input pushbutton
  MOVE TEXT-PB1 TO START_BATCH+4.
  WRITE ICON_EXECUTE_OBJECT AS ICON TO START_BATCH+0(4).
*-Prepare text for 'Payment and Deductions' pushbutton
  MOVE TEXT-PB2 TO SHOW_HISTORY+4.
  WRITE ICON_INFORMATION AS ICON TO SHOW_HISTORY+0(4).

  TITLE = SY-TITLE.                                      "WRH AHRK023507
  REPID = SY-REPID.                                      "WRH AHRK023507
  RETCODE = 1.
*=Show appropriate list according to processing type
  CASE PROC_TYP.
    WHEN 1.
*=====Batch-Input
*-----Process returncode until finished
      WHILE RETCODE EQ 1 OR RETCODE EQ 2.
*-------Call Table Control function module
        CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
             EXPORTING
*                 basic_list_title     = sy-title        "WRH AHRK023507
*                 file_name            = sy-repid        "WRH AHRK023507
                  BASIC_LIST_TITLE     = TITLE           "WRH AHRK023507
                  FILE_NAME            = REPID           "WRH AHRK023507
                  HEAD_LINE1           = DATE_SELECTION
                  HEAD_LINE2           = HEADER_2_TEXT
                  HEAD_LINE3           = HEADER_3_TEXT
                  FOOT_NOTE1           = NO_PERNR_TXT
                  FOOT_NOTE2           = NO_RECLASS_TXT
                  FOOT_NOTE3           = NO_REJECT_TXT
                  DYN_PUSHBUTTON_TEXT1 = START_BATCH
                  DYN_PUSHBUTTON_TEXT2 = SHOW_HISTORY
                  ALV_MARKER           = 'CHECK'          "DB AHRK041998
                  current_report       = repid            "OG N0486579
            IMPORTING
                  RETURN_CODE          = RETCODE
             TABLES
                  data_tab             = p0008_tc_data_tab
                  FIELDNAME_TAB        = FIELDNAMES_TAB
                  SELECT_TAB           = SEL_TC_DATA_TAB
                  ERROR_TAB            = ERR_TAB.
        CASE RETCODE.
          WHEN 1.
*-----------Call transaction SM35 to start processing Batch-Input
            SET PARAMETER ID 'MPN' FIELD MAP_NAME.
            CALL TRANSACTION 'SM35' AND SKIP FIRST SCREEN.
          WHEN 2.
*-----------Call 'Payments and Deductions' ABAP/4 report
            DESCRIBE TABLE SEL_TC_DATA_TAB LINES LINE_COUNT.
            IF LINE_COUNT EQ 0.
*-------------No entry marked in Table Control
              MESSAGE ID 'RP' TYPE 'I' NUMBER '016' WITH TEXT-M01.
            ELSE.
*-------------SUBMIT RPLPAY00
              PERFORM SUBMIT_RPLPAY00 TABLES SEL_TC_DATA_TAB.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDWHILE.
    WHEN 2.
*=====Online
*-----Process returncode until finished
      WHILE RETCODE EQ 1.
*-------Call Table Control function module
        CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
             EXPORTING
*                 basic_list_title     = sy-title        "WRH AHRK023507
*                 file_name            = sy-repid        "WRH AHRK023507
                  BASIC_LIST_TITLE     = TITLE           "WRH AHRK023507
                  FILE_NAME            = REPID           "WRH AHRK023507
                  HEAD_LINE1           = DATE_SELECTION
                  HEAD_LINE2           = HEADER_2_TEXT
                  HEAD_LINE3           = HEADER_3_TEXT
                  FOOT_NOTE1           = NO_PERNR_TXT
                  FOOT_NOTE2           = NO_RECLASS_TXT
                  FOOT_NOTE3           = NO_REJECT_TXT
                  DYN_PUSHBUTTON_TEXT1 = SHOW_HISTORY
                  ALV_MARKER           = 'CHECK'          "DB AHRK041998
                  current_report       = repid            "OG N0486579
             IMPORTING
                  RETURN_CODE      = RETCODE
             TABLES
                  data_tab         = p0008_tc_data_tab
                  FIELDNAME_TAB    = FIELDNAMES_TAB
                  SELECT_TAB       = SEL_TC_DATA_TAB
                  ERROR_TAB        = ERR_TAB.
        CASE RETCODE.
          WHEN 1.
*-----------Call 'Payments and Deductions' ABAP/4 report
            DESCRIBE TABLE SEL_TC_DATA_TAB LINES LINE_COUNT.
            IF LINE_COUNT EQ 0.
*-------------No entry marked in Table Control
              MESSAGE ID 'RP' TYPE 'I' NUMBER '016' WITH TEXT-M01.
            ELSE.
*-------------SUBMIT RPLPAY00
              PERFORM SUBMIT_RPLPAY00 TABLES SEL_TC_DATA_TAB.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDWHILE.
    WHEN 3.
*=====Protocol
*-----Process returncode until finished
      WHILE RETCODE EQ 1.
*-------Call Table Control function module
        CALL FUNCTION 'HR_DISPLAY_BASIC_LIST'
             EXPORTING
*                 basic_list_title     = sy-title        "WRH AHRK023507
*                 file_name            = sy-repid        "WRH AHRK023507
                  BASIC_LIST_TITLE     = TITLE           "WRH AHRK023507
                  FILE_NAME            = REPID           "WRH AHRK023507
                  HEAD_LINE1           = DATE_SELECTION
                  HEAD_LINE2           = HEADER_2_TEXT
                  HEAD_LINE3           = HEADER_3_TEXT
                  FOOT_NOTE1           = NO_PERNR_TXT
                  FOOT_NOTE2           = NO_RECLASS_TXT
                  FOOT_NOTE3           = NO_REJECT_TXT
                  DYN_PUSHBUTTON_TEXT1 = SHOW_HISTORY
                  ALV_MARKER           = 'CHECK'          "DB AHRK041998
                  current_report       = repid            "OG N0486579
             IMPORTING
                  RETURN_CODE      = RETCODE
             TABLES
                  data_tab         = p0008_tc_data_tab
                  FIELDNAME_TAB    = FIELDNAMES_TAB
                  SELECT_TAB       = SEL_TC_DATA_TAB
                  ERROR_TAB        = ERR_TAB.
        CASE RETCODE.
          WHEN 1.
*-----------Call 'Payments and Deductions' ABAP/4 report
            DESCRIBE TABLE SEL_TC_DATA_TAB LINES LINE_COUNT.
            IF LINE_COUNT EQ 0.
*-------------No entry marked in Table Control
              MESSAGE ID 'RP' TYPE 'I' NUMBER '016' WITH TEXT-M01.
            ELSE.
*-------------SUBMIT RPLPAY00
              PERFORM SUBMIT_RPLPAY00 TABLES SEL_TC_DATA_TAB.
            ENDIF.
          WHEN OTHERS.
        ENDCASE.
      ENDWHILE.
  ENDCASE.
ENDFORM.                               " DISPLAY_PROTOCOL

*&---------------------------------------------------------------------*
*&      Form  DET_HIRE_FIRE
*&---------------------------------------------------------------------*
*       Determine hire- and layoff dates using Infotypes P0000/P0001.  *
*       If the feature HDATE has been defined, then use it to          *
*       determine the date type for reading Infotype P0041 to get      *
*       the hire date from the Date Specifications.                    *
*----------------------------------------------------------------------*
*  <--  p_hire_date   : determined hire date of the employee
*  <--  p_layoff_date : determined layoff date of the employee
*----------------------------------------------------------------------*
FORM DET_HIRE_FIRE USING P_HIRE_DATE   LIKE SY-DATUM
                         P_LAYOFF_DATE LIKE SY-DATUM.

  DATA: BEGIN OF I0041,
          TYPE LIKE P0041-DAR01,
          DATE LIKE P0041-DAT01,
        END OF I0041.
  DATA: HDATE LIKE P0041-DAR01.

  CALL FUNCTION 'RP_HIRE_FIRE'
       EXPORTING
            BEG       = PN-BEGDA
            END       = PN-ENDDA
       IMPORTING
            HIRE_DATE = P_HIRE_DATE
            FIRE_DATE = P_LAYOFF_DATE
       TABLES
            PP0000    = P0000
            PP0001    = P0001
            PPHIFI    = PHIFI_TAB.

*-Determine date type for hire date using Feature HDATE
  MOVE-CORRESPONDING P0001 TO PME01.
  PME01-MOLGA = T500P-MOLGA.
  PERFORM RE549D USING 'HDATE' SPACE HDATE RETCD.
  IF RETCD NE 0.
    CLEAR HDATE.
  ENDIF.

*-Determine hire date from Infotyp P0041 (Date Specifications)
  CHECK NOT HDATE IS INITIAL.
  PROVIDE * FROM P0041 BETWEEN P_HIRE_DATE AND P_LAYOFF_DATE.
    DO 12 TIMES VARYING I0041 FROM P0041-DAR01 NEXT P0041-DAR02.
      CHECK I0041-TYPE EQ HDATE.
      P_HIRE_DATE = I0041-DATE.
      EXIT.
    ENDDO.
  ENDPROVIDE.

ENDFORM.                               " DET_HIRE_FIRE

*&---------------------------------------------------------------------*
*&      Form  DET_ENTRY_LEAVING_DATES
*&---------------------------------------------------------------------*
*       Determine entry and leaving dates. Depending on the features   *
*       ENTRY and LEAVE infotypes P0000, P0001, P0016 and P0041 are    *
*       used for the calculation.                                      *
*----------------------------------------------------------------------*
*  <--  p_hire_date   : determined entry date of the employee
*  <--  p_layoff_date : determined leaving date of the employee
*----------------------------------------------------------------------*
FORM DET_ENTRY_LEAVING_DATES                             "WRH AHRK011379
               USING    P_PERNR        LIKE PERNR-PERNR
               CHANGING P_ENTRY_DATE   LIKE SY-DATUM
                        P_LEAVING_DATE LIKE SY-DATUM.

  clear p_entry_date.                                    "WRH L9CK045673
  refresh entry_date_tab.                                "WRH L9CK045673

  CALL FUNCTION 'HR_ENTRY_DATE'
       EXPORTING
            PERSNR               = P_PERNR
       IMPORTING
            ENTRYDATE            = P_ENTRY_DATE
       TABLES
            ENTRY_DATES          = ENTRY_DATE_TAB
       EXCEPTIONS
            ENTRY_DATE_NOT_FOUND = 1
            PERNR_NOT_ASSIGNED   = 2
            OTHERS               = 3.

  clear p_leaving_date.                                  "WRH L9CK045673
  refresh leaving_date_tab.                              "WRH L9CK045673

  CALL FUNCTION 'HR_LEAVING_DATE'
       EXPORTING
            PERSNR                 = P_PERNR
       IMPORTING
            LEAVINGDATE            = P_LEAVING_DATE
       TABLES
            LEAVING_DATES          = LEAVING_DATE_TAB
       EXCEPTIONS
            LEAVING_DATE_NOT_FOUND = 1
            PERNR_NOT_ASSIGNED     = 2
            OTHERS                 = 3.

ENDFORM.                               " DET_ENTRY_LEAVING_DATES

*---------------------------------------------------------------------*
*       Form  PRE_PROCESS_PERNR
*---------------------------------------------------------------------*
*       Checks if all prerequisites for further processing are
*       satisfied.
*---------------------------------------------------------------------*
*       Prerequisites:
*       1. Check if employee is currently employed (only those
*          employees will be processed)
*       2. Get all relevant infotypes for the personnel number:
*           P0001 - Org. Assignment (mandatory information)
*           P0002 - Personal Data (mandatory information)
*           P0003 - Payroll Status (mandatory information)
*           P0007 - Work Schedule (mandatory information)
*       3. Check for mandatory table entries (T001P)
*
*       The personnel number is marked as selected once it fulfills
*       the following prerequisites:
*         A. Infotype P0001 exists
*         B. The table entry in T001P exists
*         C. The employee is currently employed
*
*       If any mandatory information is missing, then write out the
*       error to the error list and continue with processing the next
*       personnel number.
*----------------------------------------------------------------------*
*FORM pre_process_pernr.                                 "WRH PH4K002071
FORM PRE_PROCESS_PERNR USING P_BEGDA LIKE SY-DATUM       "WRH PH4K002071
                             P_ENDDA LIKE SY-DATUM.      "WRH PH4K002071
*=====Check for selection of personnel number

* determine Org. Assignment, TRFKZ and MOLGA
  PERFORM RE_0001_TRFKZ_MOLGA USING P_BEGDA              "WRH AHRK041998
                                    P_ENDDA              "WRH AHRK041998
                                    SPACE.               "WRH AHRK041998

*-Read current Org. Assignment
* rp_provide_from_last p0001 space pn-begda pn-endda.    "WRH PH4K002071
* RP_PROVIDE_FROM_LAST P0001 SPACE      "WRH PH4K002071  "WRH AHRK041998
*                      P_BEGDA P_ENDDA. "WRH PH4K002071  "WRH AHRK041998
*-Check if record exists
* IF PNP-SW-FOUND NE '1'.                                "WRH AHRK041998
*   PERFORM APPEND_ERROR_LIST                            "WRH AHRK041998
*           USING PERNR-PERNR '72' 'E' '103'             "WRH AHRK041998
*                 PERNR-PERNR 'P0001' SPACE SPACE.       "WRH AHRK041998
*   PERFORM REJECT_PERNR.                                "WRH AHRK041998
* ENDIF.                                                 "WRH AHRK041998

*-Determine payscale type and payscale area from table T001P
* PERFORM RE_T001P USING P0001-WERKS P0001-BTRTL.        "WRH AHRK041998

*-Determine MOLGA from T500P
* PERFORM RE_T500P USING P0001-WERKS.                    "WRH AHRK041998

*-Determine hire- and layoff dates
* perform det_hire_fire using hire_date fire_date.       "WRH AHRK011379
  PERFORM DET_ENTRY_LEAVING_DATES USING    PERNR-PERNR   "WRH AHRK011379
                                  CHANGING HIRE_DATE     "WRH AHRK011379
                                           FIRE_DATE.    "WRH AHRK011379

*-Check if employee is currently employed and increment personnel count
* IF fire_date LE pn-begda. "Has employee been laid off? "WRH PH4K002071
  IF FIRE_DATE LE P_BEGDA AND                            "WRH PH4K002071
     NOT FIRE_DATE IS INITIAL.                           "WRH PH4K002071
    PERFORM REJECT_PERNR.              "Yes -> process next employee
  ENDIF.
* IF hire_date GT fire_date OR          "WRH AHRK011379  "WRH PH4K002071
*    hire_date GT pn-endda.             "WRH AHRK011379  "WRH PH4K002071
  IF ( HIRE_DATE GT FIRE_DATE AND                        "WRH PH4K002071
       NOT FIRE_DATE IS INITIAL ) OR                     "WRH PH4K002071
     HIRE_DATE GT P_ENDDA.                               "WRH PH4K002071
    PERFORM REJECT_PERNR.                                "WRH AHRK011379
  ENDIF.                                                 "WRH AHRK011379
  ADD 1 TO PERNR_COUNT.                "Personnel number was selected

*-Fill the table SORTDATATAB
* PERFORM fill_sortdatatab.                              "WRH PH4K002071

*=====Read all relevant infotypes for the selected personnel number
*-Read current Personal Data
* rp_provide_from_last p0002 space pn-begda pn-endda.    "WRH PH4K002071
  RP_PROVIDE_FROM_LAST P0002 SPACE P_BEGDA P_ENDDA.      "WRH PH4K002071
*-Check if record exists
  IF PNP-SW-FOUND NE '1'.
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '103'
                                    PERNR-PERNR 'P0002' SPACE SPACE.
    ADD 1 TO NO_REJECTED.
    PERFORM REJECT_PERNR.
  ENDIF.

*-Process name according to national regulations
* PREREQUISITES: P0001, P0002, T001P
  RP-EDIT-NAME P0001 P0002 T001P-MOLGA SPACE.   "using $$FORMAT
  IF $RET-CODE NE 0.
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'M' '109'
                                    PERNR-PERNR SPACE SPACE SPACE.
  ENDIF.

*-Get the employee's Payroll Status
* rp_provide_from_last p0003 space pn-begda pn-endda.    "WRH PH4K002071
  RP_PROVIDE_FROM_LAST P0003 SPACE P_BEGDA P_ENDDA.      "WRH PH4K002071
*-Check if record exists
  IF PNP-SW-FOUND NE '1'.
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '103'
                                    PERNR-PERNR 'P0003' SPACE SPACE.
    ADD 1 TO NO_REJECTED.
    PERFORM REJECT_PERNR.
  ENDIF.

*-Get the employee's Work Schedule
* rp_provide_from_last p0007 space pn-begda pn-endda.    "WRH PH4K002071
  RP_PROVIDE_FROM_LAST P0007 SPACE P_BEGDA P_ENDDA.      "WRH PH4K002071
*-Check if record exists
  IF PNP-SW-FOUND NE '1'.
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '103'
                                    PERNR-PERNR 'P0007' SPACE SPACE.
    ADD 1 TO NO_REJECTED.
    PERFORM REJECT_PERNR.
  ENDIF.
ENDFORM.                               " PRE_PROCESS_PERNR

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_IMPORT_B2_DATE
*&---------------------------------------------------------------------*
*       Determines the date at which to start importing the B2 Cluster.*
*       If a recalculation has taken place, use the recalculation date *
*       of infotype P0003 (Payroll Status) as the start date.          *
*       If no recalculation was performed, use as start date the begin *
*       date of the current P0008 (Basic Pay) record if it lies within *
*       the payroll period to be evaluated or if the P0008 begin date  *
*       lies outside the current payroll period, then use the begin    *
*       date of the current payroll period.                            *
*----------------------------------------------------------------------*
*    PREREQUISITES: 1. Basic Pay infotype (P0008)                      *
*                   2. Payroll status (P0003) to determine recalc. date*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* -> p_begda        : Begin date                                       *
* -> p_endda        : End date                                         *
* <- import_b2_begda: Begin date for importing cluster B2              *
* <- import_b2_endda: End date for importing cluster B2                *
*----------------------------------------------------------------------*
FORM DETERMINE_IMPORT_B2_DATES USING P_BEGDA LIKE SY-DATUM
                                     P_ENDDA LIKE SY-DATUM
                            CHANGING P_IMPORT_B2_BEGDA LIKE SY-DATUM
                                     P_IMPORT_B2_ENDDA LIKE SY-DATUM.
*-Change the import_b2_begda only if it hasn't been previously set
* (e.g. through customer customization)
  IF P_IMPORT_B2_BEGDA IS INITIAL.
*---Check if begin date of current Basic Pay record falls within the
*   current payroll period
    IF ( PSGRPLVL_BEGDA GE P_BEGDA AND
         PSGRPLVL_BEGDA LE P_ENDDA ).
      P_IMPORT_B2_BEGDA = PSGRPLVL_BEGDA.
    ELSE.
*-----Set the begin date of the current payroll period to be the import
*     date
      P_IMPORT_B2_BEGDA = P_BEGDA.
    ENDIF.
  ENDIF.
*-Change the import_b2_endda only if it hasn't been previously set
* (e.g. through customer customization)
  IF P_IMPORT_B2_ENDDA IS INITIAL.
    P_IMPORT_B2_ENDDA = P_ENDDA.
  ENDIF.
*-Check import date against current recalculation date
  IF P0003-BDERR LE P_IMPORT_B2_BEGDA.
    P_IMPORT_B2_BEGDA = P0003-BDERR.
  ELSE.
    IF ( P_BEGDA EQ P_ENDDA OR
         P_ENDDA GE P0003-BDERR ).
      P_IMPORT_B2_ENDDA = P0003-BDERR - 1.
    ENDIF.
  ENDIF.
ENDFORM.                               " DETERMINE_IMPORT_B2_DATES

*&---------------------------------------------------------------------*
*&      Form  GET_CLUSTER_B2
*&---------------------------------------------------------------------*
*       Determine the starting and ending import dates for Cluster B2  *
*       according to infotypes P0003, P0008, and the current payroll   *
*       period, find the corresponding payroll periods, and read the   *
*       contents of the Cluster B2 tables VS and CVS into internal     *
*       tables in preparation for further processing.                  *
*----------------------------------------------------------------------*
*  -->  p_begda : Begin date as provided in the report selection screen
*  -->  p_endda : End date as provided in the report selection screen
*----------------------------------------------------------------------*
FORM GET_CLUSTER_B2 USING P_BEGDA LIKE SY-DATUM
                          P_ENDDA LIKE SY-DATUM.
*-Determine start/ending dates for importing Cluster B2
  PERFORM DETERMINE_IMPORT_B2_DATES USING P_BEGDA
                                          P_ENDDA
                                 CHANGING IMPORT_B2_BEGDA
                                          IMPORT_B2_ENDDA.
*-Determine the payroll periods starting with the date furthest into
* the past and up to the end date provided in the report selection
  PERFORM DETERMINE_PAYROLL_PERIODS USING IMPORT_B2_BEGDA
                                          IMPORT_B2_ENDDA.
*-Read current TIG-relevant information contained in Cluster B2
* (tables VS and CVS) into internal tables IVS_TAB and ICVS_TAB.
  PERFORM READ_CLUSTER_DATA.
ENDFORM.                               " GET_CLUSTER_B2

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_PAYROLL_PERIODS
*&---------------------------------------------------------------------*
*       Determine all appropriate payroll periods starting at the      *
*       import_B2_date and up to the selected end date.                *
*----------------------------------------------------------------------*
*    PREREQUISITES: 1. P0001: determine payroll area                   *
*                   2. T549A: determine period modifier                *
*                   3. T549Q: determine payroll periods                *
*----------------------------------------------------------------------*
*  -->  begda     : Begin date of the current pay scale group/level    *
*                   or the period in which a recalculation occurred.   *
*  -->  endda     : End date as entered in the selection screen.       *
*  <--  I549Q_TAB : internal table containing the payroll periods.     *
*----------------------------------------------------------------------*
FORM DETERMINE_PAYROLL_PERIODS USING BEGIN_IMPORT LIKE SY-DATUM
                                     END_IMPORT   LIKE SY-DATUM.
*-Catch any changes in payroll area by looping over P0001
  PROVIDE * FROM P0001 BETWEEN BEGIN_IMPORT AND END_IMPORT.
*---Using payroll area, determine period modifier from T549A
    PERFORM RE_T549A USING P0001-ABKRS.
*---Fill internal table i549q_tab with appropriate payroll periods
    PERFORM RE_T549Q USING BEGIN_IMPORT
                           END_IMPORT
                           '01'
                  CHANGING I549Q_TAB.
  ENDPROVIDE.
ENDFORM.                               " DETERMINE_PAYROLL_PERIODS

*&---------------------------------------------------------------------*
*&      Form  READ_CLUSTER_DATA
*&---------------------------------------------------------------------*
*       Move TIG-relevant data from cluster B2 (tables VS and CVS)
*       into internal tables for later processing.
*       Table CVS must contain at least one entry for the personnel
*       number to be processed. If CVS is empty, then provide an error
*       message and continue with the next personnel number.
*----------------------------------------------------------------------*
*  -->  I549Q_TAB : internal table for payroll periods to be processed
*  -->  VS, CVS   : TIG-relevant tables in Cluster B2
*  <--  IVS_TAB   : internal table to store contents of table VS
*  <--  ICVS_TAB  : internal table to store contents of table CVS
*----------------------------------------------------------------------*
FORM READ_CLUSTER_DATA.
  DATA: I549Q_WA TYPE I549Q_STRUC.
*-Read cluster B2 for all payroll periods listed in I549Q_TAB
  LOOP AT I549Q_TAB INTO I549Q_WA.
    PERFORM IMPORT_CLUSTER_B2 USING I549Q_WA-T549Q-PABRJ
                                    I549Q_WA-T549Q-PABRP
                                    '1'.
*---Copy contents of VS into IVS_TAB
    APPEND LINES OF VS[] TO IVS_TAB[].
*---Copy contents of current CVS into ICVS_TAB if CVS contains entries
    IF NOT CVS[] IS INITIAL.                             "WRH AHRK029426
      ICVS_TAB[] = CVS[].                                "WRH AHRK029426
    ENDIF.                                               "WRH AHRK029426
  ENDLOOP.
  IF SY-SUBRC NE 0.
*---Error handling
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '016'
                                    TEXT-E02 SPACE SPACE SPACE.
    ADD 1 TO NO_REJECTED.              "Increment number of rejected ees
    PERFORM REJECT_PERNR.              "Process next personnel number
* else.                                                  "WRH AHRK029426
*---Copy contents of current CVS into ICVS_TAB
*   icvs_tab[] = cvs[].                                  "WRH AHRK029426
  ENDIF.
ENDFORM.                               " READ_CLUSTER_DATA

*&---------------------------------------------------------------------*
*&      Form  IMPORT_CLUSTER_B2
*&---------------------------------------------------------------------*
*       Import cluster B2 with the key combination payroll year,       *
*       payroll period, and cluster type.                              *
*----------------------------------------------------------------------*
*  -->  IMP_PABRJ: payroll year (part of the cluster key)
*  -->  IMP_PABRP: payroll period (part of the cluster key)
*  -->  IMP_CLTYP: cluster type (part of the cluster key)
*----------------------------------------------------------------------*
FORM IMPORT_CLUSTER_B2 USING IMP_PABRJ LIKE B2-KEY-PABRJ
                             IMP_PABRP LIKE B2-KEY-PABRP
                             IMP_CLTYP LIKE B2-KEY-CLTYP.
  DATA: CLUSTER_KEY LIKE HRERROR-MSGV1.
*-Check if information has been previously read
  CHECK B2-KEY-PERNR NE PERNR-PERNR OR
        B2-KEY-PABRJ NE IMP_PABRJ   OR
        B2-KEY-PABRP NE IMP_PABRP   OR
        B2-KEY-CLTYP NE IMP_CLTYP.
*-Fill cluster B2 key
  MOVE: PERNR-PERNR TO B2-KEY-PERNR,
        IMP_PABRJ   TO B2-KEY-PABRJ,
        IMP_PABRP   TO B2-KEY-PABRP,
        IMP_CLTYP   TO B2-KEY-CLTYP.
*-Import cluster B2
  RP-IMP-C2-B2.
*-Error handling
  CASE RP-IMP-B2-SUBRC.
    WHEN 0.                            "No error
    WHEN 4.                            "No record available
*     CLUSTER_KEY = B2-KEY.
*     PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'M' '016'
*                                     TEXT-E03 CLUSTER_KEY SPACE SPACE.
*     ADD 1 TO NO_REJECTED.            "Increment number of rejected ees
*     PERFORM REJECT_PERNR.            "Process next personnel number
    WHEN 8.                            "Wrong cluster B2 version
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '101'
                                      SPACE SPACE SPACE SPACE.
      ADD 1 TO NO_REJECTED.            "Increment number of rejected ees
      PERFORM REJECT_PERNR.            "Process next personnel number
    WHEN 12. "Permission denied to read cluster B2
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '102'
                                      SPACE SPACE SPACE SPACE.
      ADD 1 TO NO_REJECTED.            "Increment number of rejected ees
      PERFORM REJECT_PERNR.            "Process next personnel number
    WHEN 16.                           "Cluster has been archived
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '139'
                                      SPACE SPACE SPACE SPACE.
      ADD 1 TO NO_REJECTED.            "Increment number of rejected ees
      PERFORM REJECT_PERNR.            "Process next personnel number
    WHEN 20. "Technical error while checking for archived cluster
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '140'
                                      SPACE SPACE SPACE SPACE.
      ADD 1 TO NO_REJECTED.            "Increment number of rejected ees
      PERFORM REJECT_PERNR.            "Process next personnel number
  ENDCASE.
ENDFORM.                               " IMPORT_CLUSTER_B2

*&---------------------------------------------------------------------*
*&      Form  DETERMINE_PSGRPLVL_BEGDA
*&---------------------------------------------------------------------*
*       Find oldest record that contains the same payscale information
*       as the current Basic Pay record to get the start date for the
*       subsequent payscale reclassification calculations
*----------------------------------------------------------------------*
* POST-CONDITIONS:
*   OLD_BASIC_PAY_WA = CURRENT_BASIC_PAY_WA : only one record exists
*                      with the currently valid payscale information
*   OLD_BASIC_PAY_WA not equal CURRENT_BASIC_PAY_WA : OLD_BASIC_PAY_WA
*                      contains oldest record with same payscale
*                      information as the current Basic Pay record
*----------------------------------------------------------------------*
*  -->  P_P0008_SUBTY    : Subtype of Basic Pay record
*  <--  P_PSGRPLVL_BEGDA : Begin date of oldest Basic Pay record
*                          containing the same payscale information
*                          as the currently valid Basic Pay record
*----------------------------------------------------------------------*
FORM DETERMINE_PSGRPLVL_BEGDA USING P_P0008_SUBTY LIKE P0008-SUBTY
                           CHANGING P_PSGRPLVL_BEGDA LIKE P0008-BEGDA.
  DATA: P0008_WA LIKE P0008,                             "WRH PH4K002071
        SUBRC    LIKE SY-SUBRC.                          "WRH PH4K002071
*-Get the employee's most current Basic Pay record
* rp_provide_from_last p0008 p_p0008_subty               "WRH ALRK054526
*                      pn-begda pn-endda.                "WRH ALRK054526
* PERFORM get_p0008 USING p_p0008_subty. "WRH ALRK054526 "WRH PH4K002071
  PERFORM GET_P0008 USING    P_P0008_SUBTY               "WRH PH4K002071
                             PN-BEGDA                    "WRH PH4K002071
                             PN-ENDDA                    "WRH PH4K002071
                    CHANGING SUBRC.                      "WRH PH4K002071
*-Check if record exists
* IF pnp-sw-found NE '1'.                                "WRH PH4K002071
  IF SUBRC <> 0.                                         "WRH PH4K002071
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '70' 'E' '102'
                                    'P0008' P_P0008_SUBTY
                                    SPACE SPACE.
    ADD 1 TO NO_REJECTED.
    PERFORM REJECT_PERNR.
  ENDIF.
  READ TABLE PP0008_TAB INDEX 1 INTO P0008_WA.           "WRH PH4K002071

*-Typ der Sollbezahlung überprüfen
  IF P0008_WA-CPIND IS INITIAL.                          "WRH AHRK023507
    P0008_WA-CPIND = 'T'.                                "WRH AHRK023507
  ENDIF.                                                 "WRH AHRK023507
  IF NOT ( P0008_WA-CPIND IN CPIND    AND                "WRH AHRK023507
           P0008_WA-TRFAR IN PS_TYPE  AND                "WRH AHRK023507
           P0008_WA-TRFGB IN PS_AREA  AND                "WRH AHRK023507
           P0008_WA-TRFGR IN PS_GROUP AND                "WRH AHRK023507
           P0008_WA-TRFST IN PS_LEVEL ).                 "WRH AHRK023507
    SUBTRACT 1 FROM PERNR_COUNT.                         "WRH AHRK023507
    PERFORM REJECT_PERNR.                                "WRH AHRK023507
  ENDIF.                                                 "WRH AHRK023507

*-Store current Basic Pay record into CURRENT_BASIC_PAY_WA as well as
* into OLD_BASIC_PAY_WA work areas
* MOVE-CORRESPONDING p0008 TO current_basic_pay_wa.      "WRH PH4K002071
  MOVE-CORRESPONDING P0008_WA TO CURRENT_BASIC_PAY_WA.   "WRH PH4K002071
  MOVE CURRENT_BASIC_PAY_WA TO OLD_BASIC_PAY_WA.

*-Loop over Basic Pay records to determine the oldest and newest record
* that contains the same payscale information as the current Basic Pay
* record
  LOOP AT P0008 WHERE SUBTY EQ P_P0008_SUBTY AND
                      TRFAR EQ CURRENT_BASIC_PAY_WA-TRFAR AND
                      TRFGB EQ CURRENT_BASIC_PAY_WA-TRFGB AND
                      TRFGR EQ CURRENT_BASIC_PAY_WA-TRFGR AND
                      TRFST EQ CURRENT_BASIC_PAY_WA-TRFST.
    IF P0008-CPIND IS INITIAL.                           "WRH AHRK023507
      P0008-CPIND = 'T'.                                 "WRH AHRK023507
    ENDIF.                                               "WRH AHRK023507
    IF P0008-CPIND = CURRENT_BASIC_PAY_WA-CPIND.         "WRH AHRK023507
      IF P0008-BEGDA LT OLD_BASIC_PAY_WA-BEGDA.
        MOVE-CORRESPONDING P0008 TO OLD_BASIC_PAY_WA.     "Oldest record
      ENDIF.
      IF P0008-BEGDA GT CURRENT_BASIC_PAY_WA-BEGDA.
        MOVE-CORRESPONDING P0008 TO CURRENT_BASIC_PAY_WA. "Newest record
      ENDIF.
    ENDIF.                                               "WRH AHRK023507
  ENDLOOP.
*=POST-CONDITIONS:
*   OLD_BASIC_PAY_WA = CURRENT_BASIC_PAY_WA : only one record exists
*                      with the currently valid payscale information
*   OLD_BASIC_PAY_WA not equal CURRENT_BASIC_PAY_WA : OLD_BASIC_PAY_WA
*                      contains oldest record with same payscale info.
*                      as the current basic pay record

*-Save begin date of old Basic Pay record for later use
  P_PSGRPLVL_BEGDA = OLD_BASIC_PAY_WA-BEGDA.
ENDFORM.                               " DETERMINE_PSGRPLVL_BEGDA

*&---------------------------------------------------------------------*
*&      Form  CALC_EMPLOYEE_AGE
*&---------------------------------------------------------------------*
*       Calculate the employee's age                                   *
*----------------------------------------------------------------------*
*  -->  p_birthdate    : begin date to compute age of employee
*  -->  p_endda        : end date used to compute age of employee
*  <--  p_employee_age : age of employee
*----------------------------------------------------------------------*
FORM CALC_EMPLOYEE_AGE USING P_BIRTHDATE LIKE P0002-GBDAT
                             P_ENDDA LIKE SY-DATUM
                    CHANGING P_EMPLOYEE_AGE LIKE T510R-SPALT.
  CALL FUNCTION 'COMPUTE_YEARS_BETWEEN_DATES'
       EXPORTING
            FIRST_DATE          = P_BIRTHDATE
            SECOND_DATE         = P_ENDDA
            MODIFY_INTERVAL     = SPACE
       IMPORTING
            YEARS_BETWEEN_DATES = P_EMPLOYEE_AGE.
ENDFORM.                               " CALC_EMPLOYEE_AGE

*&---------------------------------------------------------------------*
*&      Form  CALC_SENIORITY
*&---------------------------------------------------------------------*
*       Determine the employee's seniority (in years)                  *
*----------------------------------------------------------------------*
*  -->  p_hiredate        : begin date to determine employee's seniority
*  -->  p_endda           : end date to determine employee's seniority
*  <--  p_years_seniority : number of years of seniority
*----------------------------------------------------------------------*
FORM CALC_SENIORITY USING P_HIREDATE LIKE SY-DATUM
                          P_ENDDA LIKE SY-DATUM
                 CHANGING P_YEARS_SENIORITY LIKE PME50-DAUER.
  CALL FUNCTION 'COMPUTE_YEARS_BETWEEN_DATES'
       EXPORTING
            FIRST_DATE          = P_HIREDATE
            SECOND_DATE         = P_ENDDA
            MODIFY_INTERVAL     = SPACE
       IMPORTING
            YEARS_BETWEEN_DATES = P_YEARS_SENIORITY.
ENDFORM.                               " CALC_SENIORITY

*&---------------------------------------------------------------------*
*&      Form  FILL_PME50
*&---------------------------------------------------------------------*
*       Fill the structure PME50 to process the feature PRVAR          *
*----------------------------------------------------------------------*
*  -->  p_years_seniority : use years of seniority
*  -->  p0008_subty       : use P0008 subtype
*----------------------------------------------------------------------*
FORM FILL_PME50 USING P_YEARS_SENIORITY LIKE PME50-DAUER
                      T001P             LIKE T001P       "WRH AHRK041998
                      P_P0001           LIKE P0001       "WRH AHRK041998
                      P_P0007           LIKE P0007       "WRH AHRK041998
                      P_P0008_BSGRD     LIKE P0008-BSGRD "WRH PH4K002071
                      P0008_SUBTY       LIKE PME50-SUBTY.
  CLEAR PME50.                                           "WRH PH4K002071
  MOVE-CORRESPONDING T001P TO PME50.   "Personnel Areas/Subareas
  MOVE-CORRESPONDING P0001 TO PME50.   "Org. Assignment
  MOVE-CORRESPONDING P0007 TO PME50.   "Planned Work Schedule
  MOVE P0008_SUBTY TO PME50-SUBTY.
* MOVE p0008-bsgrd TO pme50-bsgrd.                       "WRH PH4K002071
  MOVE P_P0008_BSGRD TO PME50-BSGRD.                     "WRH PH4K002071
  MOVE P_YEARS_SENIORITY TO PME50-DAUER.
ENDFORM.                               " FILL_PME50

*&---------------------------------------------------------------------*
*&      Form  CALC_GRP_MEMBERSHIP_JMP_DATE
*&---------------------------------------------------------------------*
*       Determine the payscale reclassification date based on the      *
*       group membership duration as derived from table T510R.         *
*----------------------------------------------------------------------*
*  -->  p_begda        : start date for determining group membership
*  -->  p_endda        : end date for determining group membership
*  -->  p_duration     : provided group membership duration limit
*  <--  p_reclass_date : date of determined reclassification
*----------------------------------------------------------------------*
FORM CALC_GRP_MEMBERSHIP_JMP_DATE USING P_BEGDA LIKE P0008-BEGDA
                                        P_ENDDA LIKE P0008-ENDDA
                                        P_DURATION LIKE T510R-DAUER
                               CHANGING P_RECLASS_DATE LIKE SY-DATUM.
*-Does the duration limit exist in T510R?
  CHECK NOT P_DURATION IS INITIAL.
*-Calculate date for next payscale jump by adding months to the begin
* date
  PERFORM DAY_PLUS_MONTHS(SAPFP500) USING P_BEGDA
                                          P_DURATION
                                          p_reclass_date.
ENDFORM.                               " CALC_GRP_MEMBERSHIP_JMP_DATE

*&---------------------------------------------------------------------*
*&      Form  CALC_AGE_JUMP_DATE
*&---------------------------------------------------------------------*
*       Determine the payscale reclassification date based on the      *
*       age jump decision criteria as derived from table T510R.        *
*----------------------------------------------------------------------*
*  -->  p_birthdate    : start date for determining age jump
*  -->  p_age_jump     : provided age jump limit
*  <--  p_reclass_date : date of determined reclassification
*----------------------------------------------------------------------*
FORM CALC_AGE_JUMP_DATE USING P_BIRTHDATE LIKE P0002-GBDAT
                              P_AGE_JUMP LIKE T510R-SPALT
                     CHANGING P_RECLASS_DATE LIKE SY-DATUM.
*-Check if age jump limit exists in T510R ?
  CHECK NOT P_AGE_JUMP IS INITIAL.
*-Calculate date for next payscale jump by adding years to the
* employee's birth date
  PERFORM DAY_PLUS_YEARS(SAPFP500) USING P_BIRTHDATE
                                         P_AGE_JUMP
                                         P_RECLASS_DATE.
ENDFORM.                               " CALC_AGE_JUMP_DATE

*&---------------------------------------------------------------------*
*&      Form  CALC_TIME_IN_GRADE_JMP_DATE
*&---------------------------------------------------------------------*
*       Determine the payscale reclassification date based on the      *
*       Time-In-Grade decision criteria as derived from table T510R.   *
*----------------------------------------------------------------------*
*  -->  p_pn_begda     : Begin date for determining the reclass date
*  -->  p_pn_endda     : End date for determining the reclass date
*  <--  p_reclass_date : Date of determined reclassification
*----------------------------------------------------------------------*
FORM CALC_TIME_IN_GRADE_JMP_DATE
                  USING P_PN_BEGDA     LIKE SY-DATUM
                        P_PN_ENDDA     LIKE SY-DATUM
                        P_T510R_ANZHL  LIKE T510R-ANZHL
                        P_P0008        LIKE P0008        "WRH AHRK029426
                        P_TRFKZ        LIKE T503-TRFKZ   "WRH AHRK041998
               CHANGING P_RECLASS_DATE LIKE SY-DATUM.

  DATA: VARIABLE_ARG LIKE IVS_TAB-VARIA,
        VS_IDENTIFIER LIKE IVS_TAB-IDENT,
        USER_EXIT_001_ACTIVATED LIKE FALSE VALUE FALSE.

*-Check if TIG limit exists in T510R
  CHECK NOT P_T510R_ANZHL IS INITIAL.

*-Import cluster B2 (tables VS and CVS for Time-In-Grade calculations)
  PERFORM GET_CLUSTER_B2 USING P_PN_BEGDA P_PN_ENDDA.

*-Determine user-defined variable argument via a customer-exit
* (SMOD/CMOD transaction) to replace the standard var. argument
  PERFORM GET_USR_VARIABLE_ARG CHANGING VS_IDENTIFIER
                                        USER_EXIT_001_ACTIVATED.

*-Check activation of user exit '001'
  IF USER_EXIT_001_ACTIVATED EQ TRUE.
    PERFORM PROCESS_USER_EXIT_001_DATA TABLES   VAR_ARG_USER_EXIT
                                       CHANGING VARIABLE_ARG.
  ELSE.
*---Determine standard variable argument for tables VS and CVS
*   perform get_std_variable_arg using    p0008          "WRH AHRK029426
    PERFORM GET_STD_VARIABLE_ARG USING    P_P0008        "WRH AHRK029426
*                                         T503           "WRH AHRK041998
                                          P_TRFKZ        "WRH AHRK041998
                                 CHANGING VARIABLE_ARG
                                          VS_IDENTIFIER.
  ENDIF.

* loop at ivs_tab. endloop.                              "WRH AHRK029426
* read table icvs_tab index 1.                           "WRH AHRK029426

*-Adjust internal tables IVS_TAB and ICVS_TAB based on the end date for
* importing cluster B2; fill table headers with correct entries
  PERFORM ADJUST_IVS_ICVS_TAB USING IMPORT_B2_ENDDA
                                    VARIABLE_ARG         "WRH AHRK029426
                                    VS_IDENTIFIER.

*-Determine the payscale reclassification date
  PERFORM DET_TIG_RECLASS_DATE USING IVS_TAB-DATUM
                                     VARIABLE_ARG
                                     ICVS_TAB-ANZHL
                                     P_T510R_ANZHL
                                     VS_IDENTIFIER
                            CHANGING P_RECLASS_DATE.
ENDFORM.                               " CALC_TIME_IN_GRADE_JMP_DATE

*&---------------------------------------------------------------------*
*&      Form  APPEND_P0008_result_tab
*&---------------------------------------------------------------------*
*       Append data to Table Control                                   *
*----------------------------------------------------------------------*
*  -->  p_reclass_date : date for next payscale reclassification
*----------------------------------------------------------------------*
FORM APPEND_P0008_RESULT_TAB USING P_RECLASS_DATE LIKE SY-DATUM
                                   P_SUBTY LIKE P0008-SUBTY.
  DATA: PS_OLD_ENDDA LIKE P0008-ENDDA,
        P0008_WA     LIKE P0008,                         "WRH PH4K002071
        SUBRC        LIKE SY-SUBRC,                      "WRH PH4K002071
        RESULT_WA    TYPE P0008_RESULT_STRUC.
*-Does a reclassification date exist? if not -> exit
* CHECK p_reclass_date GT 0.                             "WRH PH4K002071
  CHECK NOT P_RECLASS_DATE IS INITIAL.                   "WRH PH4K002071
* Get basic pay record on reclassification date
  READ TABLE PP0008_TAB INDEX 1 INTO P0008_WA.           "WRH PH4K002071
* determine Org. Assignment, TRFKZ and MOLGA on BEGDA of old basic pay
  PERFORM RE_0001_TRFKZ_MOLGA USING P0008_WA-BEGDA       "WRH AHRK041998
                                    P0008_WA-BEGDA       "WRH AHRK041998
                                    'X'.                 "WRH AHRK041998
  CLEAR RESULT_WA.                                       "WRH PH4K002071
*-Reclassification date must always be at least one day higher than
* old_basic_pay_wa-begda
  PS_OLD_ENDDA = P_RECLASS_DATE - 1.
*-Transfer data to internal result table workarea
  RESULT_WA-PERNR     = PERNR-PERNR.
  RESULT_WA-SUBTY     = P_SUBTY.
  IF SHOW_PERID EQ YES.
    RESULT_WA-PERID   = P0002-PERID.
  ENDIF.
  RESULT_WA-NAME      = $EDIT-NAME.
  RESULT_WA-MOLGA     = T500P-MOLGA.
  RESULT_WA-OLD_TYPE  = OLD_BASIC_PAY_WA-TRFAR.
  RESULT_WA-OLD_AREA  = OLD_BASIC_PAY_WA-TRFGB.
  RESULT_WA-OLD_GROUP = OLD_BASIC_PAY_WA-TRFGR.
  RESULT_WA-OLD_LEVEL = OLD_BASIC_PAY_WA-TRFST.
  RESULT_WA-OLD_PSIND = T503-TRFKZ.
* WRITE old_basic_pay_wa-begda                           "WRH PH4K002071
*    TO result_wa-old_begda DD/MM/YYYY.                  "WRH PH4K002071
* write p0008_wa-begda                   "WRH PH4K002071 "WRH AHRK029426
*    to result_wa-old_begda dd/mm/yyyy.  "WRH PH4K002071 "WRH AHRK029426
  WRITE PSGRPLVL_BEGDA TO RESULT_WA-OLD_BEGDA DD/MM/YYYY."WRH AHRK029426
  WRITE PS_OLD_ENDDA TO RESULT_WA-OLD_ENDDA DD/MM/YYYY.
  RESULT_WA-TYPE_RECL = TYPE_PS_RECLASS.
  RESULT_WA-SPALT     = T510R-SPALT.
  RESULT_WA-DAUER     = T510R-DAUER.
  RESULT_WA-ANZHL     = T510R-ANZHL.
* result_wa-new_group = t510r-trffg.                     "WRH PH4K002071
* result_wa-new_level = t510r-trffs.                     "WRH PH4K002071
  IF T510R-TRFFG IS INITIAL.                             "WRH PH4K002071
    RESULT_WA-NEW_GROUP = OLD_BASIC_PAY_WA-TRFGR.        "WRH PH4K002071
    T510R-TRFFG = OLD_BASIC_PAY_WA-TRFGR.                "WRH PH4K002071
  ELSE.                                                  "WRH PH4K002071
    RESULT_WA-NEW_GROUP = T510R-TRFFG.                   "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071
  IF T510R-TRFFS IS INITIAL.                             "WRH PH4K002071
    RESULT_WA-NEW_LEVEL = OLD_BASIC_PAY_WA-TRFST.        "WRH PH4K002071
    T510R-TRFFS = OLD_BASIC_PAY_WA-TRFST.                "WRH PH4K002071
  ELSE.                                                  "WRH PH4K002071
    RESULT_WA-NEW_LEVEL = T510R-TRFFS.                   "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071
  WRITE P_RECLASS_DATE TO RESULT_WA-XFER_DATE DD/MM/YYYY.
*-TODO: check PA30-logic for correct result_wa-new_endda; it depends
* on customizing in table T582A (field DAVOE)
* WRITE current_basic_pay_wa-endda TO                    "WRH ALRK054526
*       result_wa-new_endda DD/MM/YYYY.                  "WRH ALRK054526
  IF NOT P8_DELIM IS INITIAL.                            "WRH PH4K002071
    WRITE P0008_WA-ENDDA                                 "WRH PH4K002071
          TO RESULT_WA-NEW_ENDDA DD/MM/YYYY.             "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071
  RESULT_WA-EMPL_AGE  = EMPLOYEE_AGE.
  RESULT_WA-SENIORITY = YEARS_OF_SENIORITY.
* Check for future basic pay records
  LOOP AT P0008 WHERE BEGDA > P_RECLASS_DATE.            "WRH PH4K002071
    RESULT_WA-FUTURE_REC = 'X'.                          "WRH PH4K002071
    EXIT.                                                "WRH PH4K002071
  ENDLOOP.                                               "WRH PH4K002071
  APPEND RESULT_WA TO P0008_RESULT_TAB.
ENDFORM.                               " APPEND_P0008_result_tab

*---------------------------------------------------------------------*
*       FORM FILL_FIELDNAMES                                          *
*---------------------------------------------------------------------*
*       Append the names of the columns to the internal 'fieldnames'  *
*       structure. The names can either be supplied by the data       *
*       repository or as a text element.                              *
*---------------------------------------------------------------------*
FORM FILL_FIELDNAMES.
*-Fill sort criteria into header
  REFRESH FIELDNAMES_TAB.                                "WRH PH4K002071
  PERFORM FILL_SORT_HEADER.
*-Personnel number
  PERFORM APPEND_FIELDNAMES USING SPACE 'PERNR' 'PERNR' 'X'.
*-Alternate personnel identification
  IF SHOW_PERID EQ YES.
    PERFORM APPEND_FIELDNAMES USING SPACE 'P0002' 'PERID' SPACE.
  ELSE.
    PERFORM APPEND_FIELDNAMES USING SPACE 'P0002' 'PERID' 'I'.
  ENDIF.
*-Name of employee
  PERFORM APPEND_FIELDNAMES USING TEXT-NAM SPACE SPACE SPACE.
*-Payscale type
  PERFORM APPEND_FIELDNAMES USING SPACE 'P0008' 'TRFAR' SPACE.
*-Payscale area
  PERFORM APPEND_FIELDNAMES USING SPACE 'P0008' 'TRFGB' SPACE.
*-Payscale group
  PERFORM APPEND_FIELDNAMES USING SPACE 'P0008' 'TRFGR' SPACE.
*-Payscale level
  PERFORM APPEND_FIELDNAMES USING SPACE 'P0008' 'TRFST' SPACE.
*-Begin date of current Basic Pay record
  PERFORM APPEND_FIELDNAMES USING TEXT-OPB SPACE SPACE SPACE.
*-End date of current Basic Pay record
  PERFORM APPEND_FIELDNAMES USING TEXT-OPE SPACE SPACE SPACE.
*-Type of payscale reclassification
  PERFORM APPEND_FIELDNAMES USING TEXT-TYP SPACE SPACE SPACE.
*-New payscale group
  PERFORM APPEND_FIELDNAMES USING TEXT-NGR SPACE SPACE SPACE.
*-New payscale level
  PERFORM APPEND_FIELDNAMES USING TEXT-NLV SPACE SPACE SPACE.
*-Reclassification date
  PERFORM APPEND_FIELDNAMES USING TEXT-NPB SPACE SPACE SPACE.
*-End date of proposed reclassification
  PERFORM APPEND_FIELDNAMES USING TEXT-NPE SPACE SPACE SPACE.
*-Future basic pay records
  PERFORM APPEND_FIELDNAMES USING TEXT-FUT               "WRH PH4K002071
                                  SPACE SPACE SPACE.     "WRH PH4K002071
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND_FIELDNAMES                                        *
*---------------------------------------------------------------------*
*       Append the internal table control output control structure    *
*       "FIELDNAMES".                                                 *
*       PARAMETERS:                                                   *
*              P_TEXT : Name for table control column                 *
*              P_TABLE: Table name                                    *
*              P_FIELD: Field name                                    *
*              P_TYPE : Type of table control column                  *
*       'P_TYPE' is a layout control field containing 1 of 4 values:  *
*       space (default), 'F'ixed (all previous columns are fixed and  *
*       key fields), 'I'nvisible, or 'X' (column is a key field).     *
*---------------------------------------------------------------------*
FORM APPEND_FIELDNAMES USING
                       P_TEXT      LIKE FIELDNAMES_WA-TEXT
                       P_TABLE     LIKE FIELDNAMES_WA-TABNAME
                       P_FIELD     LIKE FIELDNAMES_WA-FIELDNAME
                       P_TYPE      LIKE FIELDNAMES_WA-TYP.
  FIELDNAMES_WA-TEXT = P_TEXT.
  FIELDNAMES_WA-TABNAME = P_TABLE.
  FIELDNAMES_WA-FIELDNAME = P_FIELD.
  FIELDNAMES_WA-TYP = P_TYPE.
  APPEND FIELDNAMES_WA TO FIELDNAMES_TAB.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_SEL_TAB
*&---------------------------------------------------------------------*
*       Fill table SEL_TAB for selection of org. units                 *
*----------------------------------------------------------------------*
FORM FILL_SEL_TAB.
  REFRESH NAMETAB.
  CALL FUNCTION 'NAMETAB_GET'
       EXPORTING
            LANGU               = SY-LANGU
            TABNAME             = 'PS0001'
       TABLES
            NAMETAB             = NAMETAB
       EXCEPTIONS
            INTERNAL_ERROR      = 1
            TABLE_HAS_NO_FIELDS = 2
            TABLE_NOT_ACTIV     = 3
            NO_TEXTS_FOUND      = 4
            OTHERS              = 5.

*-Allow everything except the employee's name
  LOOP AT NAMETAB WHERE FIELDNAME+1(4) NE 'NAME'.
    SEL_TAB-SHORTSTRG  = NAMETAB-FIELDNAME.
    SEL_TAB-OPTIONTEXT = NAMETAB-FIELDTEXT.
    SEL_TAB-TABNAME    = 'P0001'.
    SEL_TAB-FIELDNAME  = NAMETAB-FIELDNAME.
    APPEND SEL_TAB.
    MOVE-CORRESPONDING NAMETAB TO FDTAB.
    APPEND FDTAB.
  ENDLOOP.

*-Now sort the internal selection table and free the NAMETAB resources
  SORT SEL_TAB BY OPTIONTEXT.
  FREE NAMETAB.
ENDFORM.                               " FILL_SEL_TAB

*&---------------------------------------------------------------------*
*&      Form  ASSIGN_SORTFIELDS_TO_ORGS
*&---------------------------------------------------------------------*
*       Assign field-symbols to sort criteria                          *
*----------------------------------------------------------------------*
FORM ASSIGN_SORTFIELDS_TO_ORGS USING ASSIGN_STRING LIKE SEL_ORG.
  DATA: ASSIGNFIELD(11) VALUE 'P0001-'.
  DATA: HELP_STRING LIKE SEL_ORG,
        COUNT TYPE I.

  HELP_STRING = ASSIGN_STRING.

  REFRESH SORTFIELDTAB.                                  "WRH PH4K002071
  CLEAR   SORTFIELDTAB.                                  "WRH PH4K002071
  SORTFIELDTAB-TABNAME = 'P0001'.
  SORTFIELDTAB-KEY     = 'X'.
  WHILE NOT HELP_STRING+0(5) IS INITIAL.
*---Fill table SORTFIELDTAB
    READ TABLE FDTAB WITH KEY HELP_STRING+0(5).
    SORTFIELDTAB-FIELDNAME = FDTAB-FIELDNAME.
    SORTFIELDTAB-FIELDTEXT = FDTAB-FIELDTEXT.
    APPEND SORTFIELDTAB.
*---Assign field-symbols
    COUNT = COUNT + 1.
    ASSIGNFIELD+6(5) = FDTAB-FIELDNAME.
    CASE COUNT.
      WHEN 1. ASSIGN (ASSIGNFIELD) TO <SORT1>.
      WHEN 2. ASSIGN (ASSIGNFIELD) TO <SORT2>.
      WHEN 3. ASSIGN (ASSIGNFIELD) TO <SORT3>.
      WHEN 4. ASSIGN (ASSIGNFIELD) TO <SORT4>.
      WHEN 5. ASSIGN (ASSIGNFIELD) TO <SORT5>.
      WHEN 6. ASSIGN (ASSIGNFIELD) TO <SORT6>.
    ENDCASE.
    SHIFT HELP_STRING BY 6 PLACES.
  ENDWHILE.
ENDFORM.                               " ASSIGN_SORTFIELDS_TO_ORGS

*&---------------------------------------------------------------------*
*&      Form  GET_LIMITS_FROM_T510R
*&---------------------------------------------------------------------*
*       There are currently five types of payscale reclassifications   *
*       defined in the standard:                                       *
*       1. using the date for the next payscale jump from the current  *
*          basic pay record - this overrides all other proposed dates  *
*       2. using the duration for group membership from table T510R    *
*       3. using the age jump from table T510R as a decision criteria  *
*       4. using the Time-In-Grade limit from T510R to determine the   *
*          date for the next payscale group/level reclassification     *
*       5. using a combination of limits and determining the earliest  *
*          date for the next payscale group/level reclassification.    *
*                                                                      *
*       If you would like to define payscale reclassification limits   *
*       based on decision criteria other than the standard Payscale    *
*       Type/Area/Indicator/Group/Level, you can use feature PRVAR     *
*       to refine access to the table T510R based on your decision     *
*       tree criteria.                                                 *
*       If there are no limits provided, then an error is written out  *
*       to the error list and the next personnel number is processed.  *
*----------------------------------------------------------------------*
*FORM get_limits_from_t510r USING p_p0008_subty          "WRH PH4K002071
*                           like p0008-subty.            "WRH PH4K002071
FORM GET_LIMITS_FROM_T510R                               "WRH PH4K002071
                USING P_P0008_SUBTY LIKE P0008-SUBTY     "WRH PH4K002071
                      P_P0008_WA LIKE P0008              "WRH PH4K002071
                      P_P0001_WA LIKE P0001              "WRH AHRK041998
                      P_P0007_WA LIKE P0007              "WRH AHRK041998
                      P_T001P_WA LIKE T001P              "WRH AHRK041998
                      P_MOLGA    LIKE T500P-MOLGA        "WRH AHRK041998
                      P_TRFKZ    LIKE T503-TRFKZ.        "WRH AHRK041998

  DATA: T510R_VAR_ARG LIKE T510R-VARGU."Variable argument for T510R
*=Read feature PRVAR to determine the variable argument for reading
* table T510R (for customer-specific payscale reclassification limits)
*-First fill the structure PME50 for processing the feature PRVAR
  PERFORM FILL_PME50 USING YEARS_OF_SENIORITY
                           P_T001P_WA                    "WRH AHRK041998
                           P_P0001_WA                    "WRH AHRK041998
                           P_P0007_WA                    "WRH AHRK041998
                           P_P0008_WA-BSGRD              "WRH PH4K002071
                           P_P0008_SUBTY.
*-Read the feature PRVAR to get the variable argument
  PERFORM RE549D USING 'PRVAR' ' ' T510R_VAR_ARG RETCD.
*-Set to default value on error
  IF RETCD NE 0.
    CLEAR T510R_VAR_ARG.
  ENDIF.
*-Get limits from table T510R
  PERFORM RE_T510R USING
*                        T500P-MOLGA                     "WRH AHRK041998
                         P_MOLGA                         "WRH AHRK041998
                         CURRENT_BASIC_PAY_WA-TRFAR
                         CURRENT_BASIC_PAY_WA-TRFGB
*                        T503-TRFKZ                      "WRH AHRK041998
                         P_TRFKZ                         "WRH AHRK041998
                         CURRENT_BASIC_PAY_WA-TRFGR
                         CURRENT_BASIC_PAY_WA-TRFST
                         T510R_VAR_ARG+0(3)
                         HIRE_DATE
                         P_P0008_WA-STVOR                "WRH PH4K002071
                         REC_DATE                        "WRH PH4K002071
*                        pn-begda                        "WRH PH4K002071
                         PN-ENDDA.
ENDFORM.                               " GET_LIMITS_FROM_T510R

*&---------------------------------------------------------------------*
*&      Form  CHECK_RECLASS_DATE
*&---------------------------------------------------------------------*
*       Check the determined payscale reclassification date against    *
*       the validity period of the currently valid payscale            *
*       information and against the selection period in combination    *
*       with the report selection screen parameters PAST_PS and FUT_PS.*
*       In case of an error, write the error to the error list and     *
*       process the next personnel number.                             *
*----------------------------------------------------------------------*
FORM CHECK_RECLASS_DATE CHANGING P_RECLASS_DATE LIKE SY-DATUM.
  DATA: MSGV1 LIKE SY-MSGV1.                             "WRH AHRK023507
*-Check if payscale reclassification date falls outside the validity
* period of the currently valid payscale information. In addition,
* don't permit reclassification date on the begin date of the current
* payscale group/level combination
  IF P_RECLASS_DATE LE OLD_BASIC_PAY_WA-BEGDA OR
     P_RECLASS_DATE GT CURRENT_BASIC_PAY_WA-ENDDA.
    WRITE P_RECLASS_DATE TO MSGV1 DD/MM/YYYY.            "WRH AHRK023507
    CLEAR P_RECLASS_DATE.
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR          "WRH AHRK023507
                                    '3F' 'E' '354'       "WRH AHRK023507
                                    MSGV1 SPACE          "WRH AHRK023507
                                    SPACE SPACE.         "WRH AHRK023507
    ADD 1 TO NO_REJECTED.                                "WRH AHRK023507
    PERFORM REJECT_PERNR.                                "WRH AHRK023507
  ELSE.
*---Show reclassification date prior to the begin date for the
*   selection period?
    IF P_RECLASS_DATE LT PN-BEGDA AND
       PAST_PS         NE 'X'.
      WRITE P_RECLASS_DATE TO MSGV1 DD/MM/YYYY.          "WRH AHRK023507

      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR
                                      'RP' 'E' '016'
*                               text-e06 p_reclass_date  "WRH AHRK023507
                                      TEXT-E06 MSGV1     "WRH AHRK023507
                                      SPACE SPACE.
      ADD 1 TO NO_REJECTED.            "Increment number of rejected
      PERFORM REJECT_PERNR.
    ENDIF.
*---Show reclassification date past the end date for the
*   selection period?
    IF P_RECLASS_DATE GT PN-ENDDA AND
       FUT_PS         NE 'X'.
      WRITE P_RECLASS_DATE TO MSGV1 DD/MM/YYYY.          "WRH AHRK023507
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR
                                      'RP' 'I' '016'     "WRH PH9K003008
*                                     'RP' 'E' '016'     "WRH PH9K003008
*                               text-e07 p_reclass_date  "WRH AHRK023507
                                      TEXT-E07 MSGV1     "WRH AHRK023507
                                      SPACE SPACE.
      ADD 1 TO NO_REJECTED.            "Increment number of rejected
      PERFORM REJECT_PERNR.
    ENDIF.
  ENDIF.
*=POST-CONDITION: variable "PAR_RECLASS_DATE" is either initial or
*                 falls within the Basic Pay validity period defined
*                 by PN-BEGDA and PN-ENDDA
ENDFORM.                               " CHECK_RECLASS_DATE

*&---------------------------------------------------------------------*
*&      Form  ADJUST_IVS_ICVS_TAB
*&---------------------------------------------------------------------*
*       Adjust the entries in the internal tables IVS_TAB and ICVS_TAB *
*       based on the end date set in the report selection screen.      *
*       All entries in internal table IVS_TAB with a date past the end *
*       date will reduce the total accumulated hours in ICVS_TAB.      *
*       The IVS_TAB entry will then be deleted.                        *
*----------------------------------------------------------------------*
*  -->  p_adjust_date : adjustment date
*  -->  p_identifier  : identifier assigned by RPTIME00 to entries in
*                       tables VS and CVS (Cluster B2)
*----------------------------------------------------------------------*
FORM ADJUST_IVS_ICVS_TAB
            USING P_ADJUST_DATE  LIKE SY-DATUM
                  P_VARIABLE_ARG LIKE PC2BH-VARIA        "WRH AHRK029426
                  P_IDENTIFIER   LIKE IVS_TAB-IDENT.
*-delete non-relevant entries of tables IVS and ICVS
  DELETE IVS_TAB  WHERE VARIA NE P_VARIABLE_ARG OR       "WRH AHRK029426
                        IDENT NE P_IDENTIFIER.           "WRH AHRK029426
  DELETE ICVS_TAB WHERE VARIA NE P_VARIABLE_ARG OR       "WRH AHRK029426
                        IDENT NE P_IDENTIFIER.           "WRH AHRK029426
*-sort tables IVS and ICVS
  SORT IVS_TAB  BY DATUM DESCENDING.                     "WRH AHRK029426
  SORT ICVS_TAB BY ANZHL DESCENDING.                     "WRH AHRK029426
*-Adjust the entries in the internal tables IVS_TAB and ICVS_TAB
  LOOP AT IVS_TAB WHERE DATUM GT P_ADJUST_DATE AND
                        IDENT EQ P_IDENTIFIER.
    LOOP AT ICVS_TAB WHERE VARIA EQ IVS_TAB-VARIA AND
                           IDENT EQ P_IDENTIFIER.
      SUBTRACT IVS_TAB-ANZHL FROM ICVS_TAB-ANZHL.
      MODIFY ICVS_TAB.
      IF ICVS_TAB-ANZHL LE 0.
        DELETE ICVS_TAB.
      ENDIF.
    ENDLOOP.
    DELETE IVS_TAB.
  ENDLOOP.
*-Fill table headers of IVS_TAB and ICVS_TAB with the correct entries
  READ TABLE IVS_TAB  INDEX 1.                           "WRH AHRK029426
  READ TABLE ICVS_TAB INDEX 1.                           "WRH AHRK029426
ENDFORM.                               " ADJUST_IVS_ICVS_TAB

*&---------------------------------------------------------------------*
*&      Form  DET_TIG_RECLASS_DATE
*&---------------------------------------------------------------------*
*       Determines the date for the next payscale reclassification     *
*       by looping over internal table IVS_TAB to reduce the hours     *
*       accumulated until the Time-In-Grade limit set in Table T510R   *
*       is no longer exceeded.                                         *
*       The date on which this condition becomes true is returned as   *
*       the next payscale reclassification date.                       *
*----------------------------------------------------------------------*
*  -->  p_start_date       : start date for loop over internal table IVS
*  -->  p_varia_arg        : variable argument for internal table IVS
*  -->  p_accumulated_hours: total accumulated hours
*  -->  p_tig_limit        : limit from T510R
*  -->  p_identifier       : identifier assigned by RPTIME00 to entries
*                            in tables VS and CVS (Cluster B2)
*  <--  p_reclass_date     : payscale reclassification date
*----------------------------------------------------------------------*
FORM DET_TIG_RECLASS_DATE USING P_START_DATE LIKE SY-DATUM
                                P_VARIA_ARG LIKE IVS_TAB-VARIA
                                P_ACCUMULATED_HOURS LIKE ICVS_TAB-ANZHL
                                P_TIG_LIMIT LIKE T510R-ANZHL
                                P_IDENTIFIER LIKE IVS_TAB-IDENT
                       CHANGING P_RECLASS_DATE LIKE SY-DATUM.
  DATA: REMAINING_HOURS LIKE ICVS_TAB-ANZHL.
*-Check if Time-In-Grade limit has been surpassed
* CHECK P_ACCUMULATED_HOURS GT P_TIG_LIMIT.              "WRH AHRK049440
  CHECK P_ACCUMULATED_HOURS GE P_TIG_LIMIT.              "WRH AHRK049440
*-Sort internal table IVS_TAB descending by date in preparation for
* determining the next payscale reclassification date
  SORT IVS_TAB BY DATUM DESCENDING.

*-Start with total accumulated hours
  REMAINING_HOURS = P_ACCUMULATED_HOURS.

*-Loop over internal table IVS_TAB with the specified identifier
  LOOP AT IVS_TAB WHERE DATUM LE P_START_DATE AND
                        VARIA EQ P_VARIA_ARG  AND
                        IDENT EQ P_IDENTIFIER.
*---Reduce total accumulated hours by hours worked in IVS
    REMAINING_HOURS = REMAINING_HOURS - IVS_TAB-ANZHL.
*---Check if the Time-In-Grade limit is no longer reached
    IF REMAINING_HOURS LT P_TIG_LIMIT.
*-----Keep date as next payscale reclassification date
      P_RECLASS_DATE = IVS_TAB-DATUM.
      EXIT.
    ENDIF.
  ENDLOOP.
ENDFORM.                               " DET_TIG_RECLASS_DATE

*&---------------------------------------------------------------------*
*&      Form  SORT_ORDER_OUTPUT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_ORDER_OUTPUT USING P_P0008_RESULT_TAB LIKE P0008_RESULT_TAB
                    CHANGING P_DATA_P0008 LIKE P0008_TC_DATA_TAB.
  FIELD-SYMBOLS: <F>.
  DATA: SEL_SORT TYPE I,
        NEXT_COL TYPE I,
        P_RESULT_WA TYPE P0008_RESULT_STRUC,
        P_DATA_P0008_WA TYPE TC_DATA_STRUC.

  REFRESH P_DATA_P0008.                                  "WRH PH4K002071
  DESCRIBE TABLE SORTFIELDTAB LINES SEL_SORT.
*-Process table of reclassification results
  LOOP AT P_P0008_RESULT_TAB INTO P_RESULT_WA.
*---Process each line of the table to be displayed
    NEXT_COL = 1.
    READ TABLE SORTDATATAB WITH KEY PERNR = P_RESULT_WA-PERNR.
*---Add sort criteria to the internal table
    DO SEL_SORT TIMES.
      ASSIGN COMPONENT NEXT_COL OF STRUCTURE P_DATA_P0008_WA TO <F>.
      CASE NEXT_COL.
        WHEN 1. <F> = SORTDATATAB-FIELD1.
        WHEN 2. <F> = SORTDATATAB-FIELD2.
        WHEN 3. <F> = SORTDATATAB-FIELD3.
        WHEN 4. <F> = SORTDATATAB-FIELD4.
        WHEN 5. <F> = SORTDATATAB-FIELD5.
        WHEN 6. <F> = SORTDATATAB-FIELD6.
      ENDCASE.
      NEXT_COL = NEXT_COL + 1.
    ENDDO.
*---Add remaining data fields to the internal table
*   DO 14 TIMES.                                         "WRH PH4K002071
    DO 15 TIMES.                                         "WRH PH4K002071
      ASSIGN COMPONENT NEXT_COL OF STRUCTURE P_DATA_P0008_WA TO <F>.
      CASE SY-INDEX.
        WHEN 1.  <F> = P_RESULT_WA-PERNR.
        WHEN 2.  <F> = P_RESULT_WA-PERID.
        WHEN 3.  <F> = P_RESULT_WA-NAME.
        WHEN 4.  <F> = P_RESULT_WA-OLD_TYPE.
        WHEN 5.  <F> = P_RESULT_WA-OLD_AREA.
        WHEN 6.  <F> = P_RESULT_WA-OLD_GROUP.
        WHEN 7.  <F> = P_RESULT_WA-OLD_LEVEL.
        WHEN 8.  <F> = P_RESULT_WA-OLD_BEGDA.
        WHEN 9.  <F> = P_RESULT_WA-OLD_ENDDA.
        WHEN 10. <F> = P_RESULT_WA-TYPE_RECL.
        WHEN 11. <F> = P_RESULT_WA-NEW_GROUP.
        WHEN 12. <F> = P_RESULT_WA-NEW_LEVEL.
        WHEN 13. <F> = P_RESULT_WA-XFER_DATE.
        WHEN 14. <F> = P_RESULT_WA-NEW_ENDDA.
        WHEN 15. <F> = P_RESULT_WA-FUTURE_REC.           "WRH PH4K002071
      ENDCASE.
      NEXT_COL = NEXT_COL + 1.
    ENDDO.
    APPEND P_DATA_P0008_WA TO P_DATA_P0008.
  ENDLOOP.
  SORT P_DATA_P0008.
ENDFORM.                               " SORT_ORDER_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  FILL_SORT_HEADER
*&---------------------------------------------------------------------*
*       Fill sort criteria into header                                 *
*----------------------------------------------------------------------*
FORM FILL_SORT_HEADER.
  LOOP AT SORTFIELDTAB.
    PERFORM APPEND_FIELDNAMES USING SPACE
                                    'P0001'
                                    SORTFIELDTAB-FIELDNAME
                                    SORTFIELDTAB-KEY.
  ENDLOOP.
ENDFORM.                               " FILL_SORT_HEADER

*&---------------------------------------------------------------------*
*&      Form  FILL_SORTDATATAB
*&---------------------------------------------------------------------*
*       Fill the internal table SORTDATATAB with the selected sort     *
*       criteria.                                                      *
*----------------------------------------------------------------------*
FORM FILL_SORTDATATAB.
  SORTDATATAB-PERNR  = PERNR-PERNR.
  SORTDATATAB-FIELD1 = <SORT1>.
  SORTDATATAB-FIELD2 = <SORT2>.
  SORTDATATAB-FIELD3 = <SORT3>.
  SORTDATATAB-FIELD4 = <SORT4>.
  SORTDATATAB-FIELD5 = <SORT5>.
  SORTDATATAB-FIELD6 = <SORT6>.
  APPEND SORTDATATAB.
ENDFORM.                               " FILL_SORTDATATAB

*&--------------------------------------------------------------------*
*&      Form  PROCESS_P0008                                           *
*&--------------------------------------------------------------------*
*       Create a new Basic Pay record for the subtype specified in    *
*       the internal table INFOGROUP_TAB on the date of the next pay  *
*       scale reclassification.                                       *
*---------------------------------------------------------------------*
FORM PROCESS_P0008 USING VALUE(SUBTY)        LIKE P0008-SUBTY
                         VALUE(RECLASS_DATE) LIKE RECLASS_DATE.
*-Check for correct processing type (1 or 2: Batch-Input/Online)
* and existence of a reclassification date
* CHECK ( proc_typ EQ '1' OR proc_typ EQ '2' ) AND       "WRH PH4K002071
*       NOT reclass_date IS INITIAL.                     "WRH PH4K002071
  CHECK NOT RECLASS_DATE IS INITIAL.                     "WRH PH4K002071
*-Fill internal table OLD_WAGE_TYPE_TAB with Basic Pay wage types
* valid on reclassification date
  PERFORM FILL_OLD_WAGE_TYPE_TAB USING    SUBTY
*                                         T500P-MOLGA    "WRH AHRK041998
*                                         pn-begda       "WRH PH4K002071
*                               PN-BEGDA "WRH AHRK023507 "WRH AHRK041998
                                          RECLASS_DATE   "WRH AHRK041998
                                          RECLASS_DATE   "WRH AHRK023507
*                                         pn-endda       "WRH PH4K002071
                                 CHANGING OLD_WAGE_TYPE_TAB.
*-Create a new P0008 (Basic Pay) record
  PERFORM CREATE_NEW_PP0008_RECORD USING SUBTY
*                                        T500P-MOLGA     "WRH AHRK041998
                                         RECLASS_DATE
                                         OLD_WAGE_TYPE_TAB
                                CHANGING NEW_WAGE_TYPE_TAB.
*-Generate entry in the Basic Pay control structure
  PERFORM APPEND_P0008_RESULT_TAB USING RECLASS_DATE     "WRH PH4K002071
                                        SUBTY.           "WRH PH4K002071

  CHECK PROC_TYP EQ '1' OR PROC_TYP EQ '2'.              "WRH PH4K002071
*-Fill Batch-Input session for infotype P0008 (Basic Pay)
  PERFORM GEN_BDCDATA_0008 USING PROC_TYP
                                 reclass_date            "WRH L9CK045673
                                 SUBTY.
ENDFORM.                               " PROCESS_P0008

*---------------------------------------------------------------------*
*  FORM-routines for processing F4-requests on the selection screen   *
*---------------------------------------------------------------------*

*&--------------------------------------------------------------------*
*&      Form  APPEND_IHELP_FIELDS                                     *
*&--------------------------------------------------------------------*
*       Define structure of the F4-popup                              *
*---------------------------------------------------------------------*
*  -->  P_TABNAME   : Name of table                                   *
*  -->  P_FIELDNAME : Name of field                                   *
*  -->  P_SELFLAG   : Selected field                                  *
*---------------------------------------------------------------------*
FORM APPEND_IHELP_FIELDS USING P_TABNAME   LIKE IHELP_FIELDS-TABNAME
                               P_FIELDNAME LIKE IHELP_FIELDS-FIELDNAME
                               P_SELFLAG   LIKE IHELP_FIELDS-SELECTFLAG.
  DATA: IHELP_FIELDS_WA LIKE IHELP_FIELDS.
  IHELP_FIELDS_WA-TABNAME    = P_TABNAME.
  IHELP_FIELDS_WA-FIELDNAME  = P_FIELDNAME.
  IHELP_FIELDS_WA-SELECTFLAG = P_SELFLAG.
  APPEND IHELP_FIELDS_WA TO IHELP_FIELDS.
ENDFORM.                               " APPEND_IHELP_FIELDS

*&--------------------------------------------------------------------*
*&      Form  APPEND_DYNPRO_TAB                                       *
*&--------------------------------------------------------------------*
*       Append a dynpro field and its value to the internal table     *
*       DYNPRO_TAB in preparation for updating the report selection   *
*       screen dynpro (in FORM-routine UPDATE_DYNPRO).                *
*---------------------------------------------------------------------*
*  -->  P_FIELDNAME  : Name of the dynpro field to be updated         *
*  -->  P_FIELDVALUE : Value of the dynpro field to be updated        *
*---------------------------------------------------------------------*
FORM APPEND_DYNPRO_TAB USING P_FIELDNAME  LIKE DYNPREAD-FIELDNAME
                             P_FIELDVALUE TYPE ANY.
  DATA: DYNPRO_TAB_WA LIKE DYNPRO_TAB.
  DYNPRO_TAB_WA-FIELDNAME  = P_FIELDNAME.
  DYNPRO_TAB_WA-FIELDVALUE = P_FIELDVALUE.
  APPEND DYNPRO_TAB_WA TO DYNPRO_TAB.
ENDFORM.                               " APPEND_DYNPRO_TAB

*&--------------------------------------------------------------------*
*&      Form  UPDATE_DYNPRO                                           *
*&--------------------------------------------------------------------*
*       Update the report selection screen dynpro.                    *
*---------------------------------------------------------------------*
*  -->  P_PROG   : Name of the ABAP/4 report                          *
*  -->  P_DYNPRO : Number of the report selection screen dynpro       *
*---------------------------------------------------------------------*
FORM UPDATE_DYNPRO USING P_PROG   LIKE SY-CPROG
                         P_DYNPRO LIKE SY-DYNNR.
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
       EXPORTING
            DYNAME               = P_PROG
            DYNUMB               = P_DYNPRO
       TABLES
            DYNPFIELDS           = DYNPRO_TAB
       EXCEPTIONS
*           INVALID_ABAPWORKAREA = 1
*           INVALID_DYNPROFIELD  = 2
*           INVALID_DYNPRONAME   = 3
*           INVALID_DYNPRONUMMER = 4
*           INVALID_REQUEST      = 5
*           NO_FIELDDESCRIPTION  = 6
*           UNDEFIND_ERROR       = 7
            OTHERS               = 8.

ENDFORM.                               " UPDATE_DYNPRO

*&--------------------------------------------------------------------*
*&      Form  SHOW_SUBTYPE                                            *
*&--------------------------------------------------------------------*
*       Display an F4-Popup for the subtypes of the specified infotype*
*---------------------------------------------------------------------*
*  -->  P_INFOTYPE : Infotype to display subtypes of                  *
*---------------------------------------------------------------------*
FORM SHOW_SUBTYPE USING P_INFOTYPE LIKE P0008-INFTY.
  DATA: TITLE_IN_POPUP(70).
  CLEAR  : IHELP_FIELDS, HELPTAB, DYNPRO_TAB.
  REFRESH: IHELP_FIELDS, HELPTAB, DYNPRO_TAB.
  SELECT * FROM T591A WHERE INFTY EQ P_INFOTYPE.
    APPEND T591A-SUBTY TO HELPTAB.
    PERFORM RE_T591S USING SY-LANGU
                           T591A-INFTY
                           T591A-SUBTY.
    APPEND T591S-STEXT TO HELPTAB.
  ENDSELECT.
  DESCRIBE TABLE HELPTAB LINES SY-TFILL.
  IF SY-TFILL EQ 0.
    MESSAGE S503(SH).
  ELSE.
    TITLE_IN_POPUP = TEXT-F4S.
    REPLACE '&' WITH P_INFOTYPE INTO TITLE_IN_POPUP.
    PERFORM APPEND_IHELP_FIELDS USING 'T591A' 'SUBTY' 'X'.
    PERFORM APPEND_IHELP_FIELDS USING 'T591S' 'STEXT' ' '.
    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
         EXPORTING
              FIELDNAME                 = 'SUBTY'
              TABNAME                   = 'P0008'
              TITLE_IN_VALUES_LIST      = TITLE_IN_POPUP
         IMPORTING
              SELECT_VALUE              = P8_SUBTY
         TABLES
              FIELDS                    = IHELP_FIELDS
              VALUETAB                  = HELPTAB
         EXCEPTIONS
              FIELD_NOT_IN_DDIC         = 01
              MORE_THEN_ONE_SELECTFIELD = 02
              NO_SELECTFIELD            = 03.
    IF SY-SUBRC EQ 0 AND NOT P8_SUBTY IS INITIAL.
      PERFORM RE_T591S USING SY-LANGU
                             P_INFOTYPE
                             P8_SUBTY.
      PERFORM APPEND_DYNPRO_TAB USING 'SUBT_TXT' T591S-STEXT.
      PERFORM UPDATE_DYNPRO USING SY-CPROG SY-DYNNR.
    ENDIF.
  ENDIF.
ENDFORM.                               " SHOW_SUBTYPE

*&--------------------------------------------------------------------*
*&      Form  SHOW_REASON                                             *
*&--------------------------------------------------------------------*
*       Display an F4-Popup for the reasons of the specified infotype *
*---------------------------------------------------------------------*
*  -->  P_INFOTYPE : Infotype to display reasons of                   *
*---------------------------------------------------------------------*
FORM SHOW_REASON USING P_INFOTYPE LIKE P0008-INFTY.
  DATA: TITLE_IN_POPUP(70).
  CLEAR  : IHELP_FIELDS, HELPTAB, DYNPRO_TAB.
  REFRESH: IHELP_FIELDS, HELPTAB, DYNPRO_TAB.
  SELECT * FROM T530E WHERE INFTY EQ P_INFOTYPE.
    APPEND T530E-PREAS TO HELPTAB.
    PERFORM RE_T530F USING SY-LANGU
                           P_INFOTYPE
                           T530E-PREAS.
    APPEND T530F-RTEXT TO HELPTAB.
  ENDSELECT.
  DESCRIBE TABLE HELPTAB LINES SY-TFILL.
  IF SY-TFILL EQ 0.
    MESSAGE S503(SH).
  ELSE.
    TITLE_IN_POPUP = TEXT-F4S.
    REPLACE '&' WITH P_INFOTYPE INTO TITLE_IN_POPUP.
    PERFORM APPEND_IHELP_FIELDS USING 'T530E' 'PREAS' 'X'.
    PERFORM APPEND_IHELP_FIELDS USING 'T530F' 'RTEXT' ' '.
    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE'
         EXPORTING
              FIELDNAME                 = 'PREAS'
              TABNAME                   = 'T530E'
              TITLE_IN_VALUES_LIST      = TITLE_IN_POPUP
         IMPORTING
              SELECT_VALUE              = REASON
         TABLES
              FIELDS                    = IHELP_FIELDS
              VALUETAB                  = HELPTAB
         EXCEPTIONS
              FIELD_NOT_IN_DDIC         = 01
              MORE_THEN_ONE_SELECTFIELD = 02
              NO_SELECTFIELD            = 03.
    IF SY-SUBRC EQ 0 AND NOT REASON IS INITIAL.
      PERFORM RE_T530F USING SY-LANGU
                             P_INFOTYPE
                             REASON.
      PERFORM APPEND_DYNPRO_TAB USING 'REAS_TXT' T530F-RTEXT.
      PERFORM UPDATE_DYNPRO USING SY-CPROG SY-DYNNR.
    ENDIF.
  ENDIF.
ENDFORM.                               " SHOW_REASON

*&--------------------------------------------------------------------*
*&      Form  SHOW_EVENT                                              *
*&--------------------------------------------------------------------*
*       Display an F4-Popup for the personnel events and event reasons*
*---------------------------------------------------------------------*
FORM SHOW_EVENT.
  DATA: SEL_VALUES LIKE HELP_VTAB OCCURS 0 WITH HEADER LINE.
  CLEAR  : IHELP_FIELDS, HELPTAB, DYNPRO_TAB.
  REFRESH: IHELP_FIELDS, HELPTAB, DYNPRO_TAB.
  SELECT * FROM T530.
    APPEND T530-MASSN TO HELPTAB.
    PERFORM RE_T529T USING SY-LANGU
                           T530-MASSN.
    APPEND T529T-MNTXT TO HELPTAB.
    APPEND T530-MASSG TO HELPTAB.
    PERFORM RE_T530T USING SY-LANGU
                           T530-MASSN
                           T530-MASSG.
    APPEND T530T-MGTXT TO HELPTAB.
  ENDSELECT.
  DESCRIBE TABLE HELPTAB LINES SY-TFILL.
  IF SY-TFILL EQ 0.
    MESSAGE S503(SH).
  ELSE.
    PERFORM APPEND_IHELP_FIELDS USING 'T530 ' 'MASSN' ' '.
    PERFORM APPEND_IHELP_FIELDS USING 'T529T' 'MNTXT' ' '.
    PERFORM APPEND_IHELP_FIELDS USING 'T530 ' 'MASSG' 'X'.
    PERFORM APPEND_IHELP_FIELDS USING 'T530T' 'MGTXT' ' '.
    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
         EXPORTING
              FIELDNAME                 = 'MASSG'
              TABNAME                   = 'T530 '
         TABLES
              FIELDS                    = IHELP_FIELDS
              VALUETAB                  = HELPTAB
              SELECT_VALUES             = SEL_VALUES
         EXCEPTIONS
              FIELD_NOT_IN_DDIC         = 01
              MORE_THEN_ONE_SELECTFIELD = 02
              NO_SELECTFIELD            = 03.
    IF SY-SUBRC EQ 0 AND NOT SEL_VALUES IS INITIAL.
      LOOP AT SEL_VALUES.
        CASE SEL_VALUES-FIELDNAME.
          WHEN 'MASSN'.
            MOVE SEL_VALUES-VALUE TO TUMMASSN+0(2).
          WHEN 'MASSG'.
            MOVE SEL_VALUES-VALUE TO TUMMASSN+2(2).
          WHEN 'MNTXT'.
            IF NOT SEL_VALUES-VALUE IS INITIAL.
              PERFORM APPEND_DYNPRO_TAB USING 'EVNT_TXT'
                                              SEL_VALUES-VALUE.
            ENDIF.
          WHEN 'MGTXT'.
            IF NOT SEL_VALUES-VALUE IS INITIAL.
              PERFORM APPEND_DYNPRO_TAB USING 'EVNT_TXT'
                                              SEL_VALUES-VALUE.
            ENDIF.
        ENDCASE.
      ENDLOOP.
      PERFORM UPDATE_DYNPRO USING SY-CPROG SY-DYNNR.
    ENDIF.
  ENDIF.
ENDFORM.                               " SHOW_EVENT

*&--------------------------------------------------------------------*
*&      Form  SUBMIT_RPLPAY00                                         *
*&--------------------------------------------------------------------*
*       Submit the ABAP/4 report RPLPAY00 to get a list of Payments   *
*       and Deductions for the employees marked in the Table Control. *
*---------------------------------------------------------------------*
*  -->  P_SEL_TC_DATA_TAB : Selected entries in the Table Control     *
*---------------------------------------------------------------------*
FORM SUBMIT_RPLPAY00 TABLES P_SEL_TC_DATA_TAB LIKE P0008_TC_DATA_TAB.
FIELD-SYMBOLS: <FIELDNN> LIKE FIELDNAMES_WA-TEXT.
DATA: FIELDNAMES_WA TYPE FIELDNAMES_STRUC,
      FIELDNAME LIKE FIELDNAMES_WA-TEXT,
      TABIX.
RANGES RANGE_PERNR FOR PNPPERNR.
*-Determine Index of 'PERNR' field
  LOOP AT FIELDNAMES_TAB INTO  FIELDNAMES_WA
                         WHERE FIELDNAME EQ 'PERNR'.
    TABIX = SY-TABIX.
    EXIT.
  ENDLOOP.
  CHECK SY-SUBRC EQ 0.
*-Derive the fieldname in table P_SEL_TC_DATA_TAB, ...
  CONCATENATE 'P_SEL_TC_DATA_TAB-FIELD' TABIX INTO FIELDNAME.
  ASSIGN (FIELDNAME) TO <FIELDNN>.
*-loop over table P_SEL_TC_DATA_TAB to fill selection table, and ...
  LOOP AT P_SEL_TC_DATA_TAB.
    MOVE: 'I'        TO RANGE_PERNR-SIGN,
          'EQ'       TO RANGE_PERNR-OPTION,
          <FIELDNN>  TO RANGE_PERNR-LOW.
    APPEND RANGE_PERNR.
  ENDLOOP.
*-submit RPLPAY00 w/ PERNRs
  SUBMIT RPLPAY00
         WITH PNPBEGDA EQ '18000101'
         WITH PNPENDDA EQ '99991231'
         WITH PNPBEGPS EQ '00000000'
         WITH PNPENDPS EQ '00000000'
         WITH PNPPERNR IN RANGE_PERNR
         AND RETURN.
ENDFORM.                    " SUBMIT_RPLPAY00

*&--------------------------------------------------------------------*
*&      Form  SHOW_PROGRESS                                           *
*&--------------------------------------------------------------------*
*       Show progress information                                     *
*---------------------------------------------------------------------*
*  -->  P_PROGRESS_TEXT : Text of progress information                *
*  -->  P_INFORMATION   : progress information                        *
*---------------------------------------------------------------------*
FORM SHOW_PROGRESS USING P_PROGRESS_TEXT LIKE SY-UCOMM
                         P_INFORMATION   LIKE PERNR_COUNT.
  DATA: PROGRESS_TEXT LIKE P_PROGRESS_TEXT,
        COUNTER LIKE PERNR-PERNR.

  MOVE P_PROGRESS_TEXT TO PROGRESS_TEXT.
  MOVE P_INFORMATION TO COUNTER.
  REPLACE '&' WITH COUNTER INTO PROGRESS_TEXT.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            TEXT       = PROGRESS_TEXT
       EXCEPTIONS
            OTHERS     = 1.
ENDFORM.                    " SHOW_PROGRESS

*&--------------------------------------------------------------------*
*&      Form  FILL_INFOGROUP_TAB                                      *
*&--------------------------------------------------------------------*
*       Fill the internal table INFOGROUP_TAB with the infotypes and  *
*       subtypes to be processed.                                     *
*---------------------------------------------------------------------*
*  -->  P_MASSN         : Event                                       *
*  -->  P_MOLGA         : Country modifier                            *
*  <--  P_INFOGROUP_TAB : resulting table of infotypes and subtypes   *
*---------------------------------------------------------------------*
FORM FILL_INFOGROUP_TAB USING    P_MASSN LIKE T529A-MASSN
                                 P_P0001 LIKE P0001      "WRH AHRK041998
                                 P_MOLGA LIKE T001P-MOLGA
                        CHANGING P_INFOGROUP_TAB LIKE INFOGROUP_TAB.
DATA: IGMOD LIKE T588D-IGMOD.
*-Fill internal table INFOGROUP_TAB only for a specified event
  CHECK EVNT+0(4) EQ ICON_CHECKED.
*-Refresh table of infotypes and subtypes
  REFRESH P_INFOGROUP_TAB.                               "WRH PH4K002071
*-Read table T529A with the event to determine the infogroup
  PERFORM RE_T529A USING P_MASSN.
*-Fill feature PME04
  PERFORM FILL_PME04 USING T529A-ITYGR
                           P_P0001                       "WRH AHRK041998
                           P_MOLGA.
*-Read the feature IGMOD to get the infogroup modifier
  PERFORM RE549D USING 'IGMOD' ' ' IGMOD RETCD.
*-Set to default value on error
  IF RETCD NE 0.
    CLEAR IGMOD.
  ENDIF.
*-Read table T588D to fill the internal table INFOGROUP_TAB
  PERFORM RE_T588D USING    T529A-ITYGR
                            IGMOD
                   CHANGING P_INFOGROUP_TAB.
ENDFORM.                    " FILL_INFOGROUP_TAB

*&--------------------------------------------------------------------*
*&      Form  FILL_PME04                                              *
*&--------------------------------------------------------------------*
*       Fill the structure PME04 to process the feature IGMOD         *
*---------------------------------------------------------------------*
*  --> P_ITYGR : Number of infotype group                             *
*  --> P_MOLGA : Country modifier                                     *
*---------------------------------------------------------------------*
FORM FILL_PME04 USING P_ITYGR LIKE T529A-ITYGR
                      P_P0001 LIKE P0001                 "WRH AHRK041998
                      P_MOLGA LIKE T001P-MOLGA.
  CLEAR PME04.                                           "WRH PH4K002071
  MOVE P_ITYGR TO PME04-ITYGR.
  MOVE P_MOLGA TO PME04-MOLGA.
* MOVE P0001-BUKRS TO PME04-BUKRS.                       "WRH AHRK063507
* MOVE P0001-WERKS TO PME04-WERKS.                       "WRH AHRK063507
* MOVE P0001-PERSG TO PME04-PERSG.                       "WRH AHRK063507
* MOVE P0001-PERSK TO PME04-PERSK.                       "WRH AHRK063507
  MOVE P_P0001-BUKRS TO PME04-BUKRS.                     "WRH AHRK063507
  MOVE P_P0001-WERKS TO PME04-WERKS.                     "WRH AHRK063507
  MOVE P_P0001-PERSG TO PME04-PERSG.                     "WRH AHRK063507
  MOVE P_P0001-PERSK TO PME04-PERSK.                     "WRH AHRK063507
ENDFORM.                    " FILL_PME04

*&--------------------------------------------------------------------*
*&      Form  MOVE_P0008_TO_INFOGROUP                                 *
*&--------------------------------------------------------------------*
*       In case a subtype has been defined for infotype P0008, then   *
*       fill the internal table INFOGROUP_TAB with only one entry.    *
*---------------------------------------------------------------------*
*  --> P_P8_SUBTY      : Subtype of infotype P0008 (Basic Pay)        *
*  <-- P_INFOGROUP_TAB : Resulting table of infotypes and subtypes    *
*---------------------------------------------------------------------*
FORM MOVE_P0008_TO_INFOGROUP USING P_P8_SUBTY LIKE P0008-SUBTY
                          CHANGING P_INFOGROUP_TAB LIKE INFOGROUP_TAB.
  DATA: INFOGROUP_WA LIKE PITGR.
*-Fill internal table INFOGROUP_TAB only for a specified subtype
  CHECK STYP+0(4) EQ ICON_CHECKED.
*-Refresh table of infotypes and subtypes
  REFRESH P_INFOGROUP_TAB.                               "WRH PH4K002071
*-Fill workarea of internal table INFOGROUP_TAB and move it into table
* MOVE '0008' TO infogroup_wa-infty.                     "WRH PH4K002071
  MOVE STR_0008 TO INFOGROUP_WA-INFTY.                   "WRH PH4K002071
  MOVE P_P8_SUBTY TO INFOGROUP_WA-SUBTY.
  MOVE 'COP ' TO INFOGROUP_WA-OPERA.
  MOVE 1 TO INFOGROUP_WA-ANZHL.
  APPEND INFOGROUP_WA TO P_INFOGROUP_TAB.
ENDFORM.                    " MOVE_P0008_TO_INFOGROUP

*&--------------------------------------------------------------------*
*&      Form  PROCESS_INFOGROUP_TAB                                   *
*&--------------------------------------------------------------------*
*       Loop over INFOGROUP_TAB to process the individual infotypes   *
*       and subtypes.                                                 *
*---------------------------------------------------------------------*
*      -->P_INFOGROUP_TAB : internal table containing the infotypes   *
*                           and subtypes from the infogroup           *
*---------------------------------------------------------------------*
FORM PROCESS_INFOGROUP_TAB USING P_INFOGROUP_TAB LIKE INFOGROUP_TAB.
DATA: INFOGROUP_WA LIKE PITGR.
  LOOP AT P_INFOGROUP_TAB INTO INFOGROUP_WA.
    CASE INFOGROUP_WA-INFTY.
      WHEN '0008'.
*-------Calculate the next pay scale reclassification date
        PERFORM CALCULATE_PS_JUMP_DATE    USING INFOGROUP_WA-SUBTY
                                       CHANGING RECLASS_DATE.
*-------Now determine the current and the new Basic Pay records and
*       create the Batch-Input session for infotype P0008 (Basic Pay)
        PERFORM PROCESS_P0008 USING INFOGROUP_WA-SUBTY
                                    RECLASS_DATE.
      when others.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'M' '016'
                                        TEXT-E08 INFOGROUP_WA-INFTY
                                        SPACE SPACE.
    endcase.
  ENDLOOP.
ENDFORM.                    " PROCESS_INFOGROUP_TAB

*&--------------------------------------------------------------------*
*&      Form  CALCULATE_PS_JUMP_DATE                                  *
*&--------------------------------------------------------------------*
*       Calculate the date for the next payscale reclassification     *
*       based on the type of reclassification.                        *
*---------------------------------------------------------------------*
*      -->P_INFOGROUP_WA-SUBTY : subtype of the P0008 record          *
*---------------------------------------------------------------------*
FORM CALCULATE_PS_JUMP_DATE USING    VALUE(SUBTY) LIKE P0008-SUBTY
                            CHANGING RECLASS_DATE LIKE P0008-BEGDA.
DATA: COMPARE_DATE LIKE SY-DATUM VALUE '99991231'.
DATA: P0008_WA LIKE P0008.                               "WRH ALRK054526
*-Initialize global variables
  PERFORM INIT.

*-Determine begin date of oldest Basic Pay record with the currently
* valid pay scale information for later use
  PERFORM DETERMINE_PSGRPLVL_BEGDA USING SUBTY
                                CHANGING PSGRPLVL_BEGDA.

*-Read limits and next payscale group & level
* PERFORM get_limits_from_t510r USING subty.             "WRH PH4K002071

*-Does a date for the next payscale jump exist in P0008?
  READ TABLE PP0008_TAB INDEX 1 INTO P0008_WA.           "WRH ALRK054526

* determine Org. Assignment, TRFKZ and MOLGA
  PERFORM RE_0001_TRFKZ_MOLGA USING P0008_WA-BEGDA       "WRH AHRK041998
                                    P0008_WA-BEGDA       "WRH AHRK041998
                                    'X'.                 "WRH AHRK041998
*-Get the employee's Work Schedule
  RP_PROVIDE_FROM_LAST P0007 SPACE P0008_WA-BEGDA        "WRH AHRK041998
                                   P0008_WA-BEGDA.       "WRH AHRK041998

*-Read limits and next payscale group & level
  PERFORM GET_LIMITS_FROM_T510R USING SUBTY              "WRH PH4K002071
                                      P0008_WA           "WRH PH4K002071
                                      P0001              "WRH AHRK041998
                                      P0007              "WRH AHRK041998
                                      T001P              "WRH AHRK041998
                                      T500P-MOLGA        "WRH AHRK041998
                                      T503-TRFKZ.        "WRH AHRK041998
* IF NOT ( current_basic_pay_wa-stvor IS INITIAL ).      "WRH ALRK054526
  IF NOT ( P0008_WA-STVOR IS INITIAL ).                  "WRH ALRK054526
*---The date for the next payscale jump overrides T510R as well as the
*   feature-provided payscale reclassification limits
    TYPE_PS_RECLASS = '01'.  "Reclassification date provided in P0008
*   reclass_date = current_basic_pay_wa-stvor.           "WRH ALRK054526
    RECLASS_DATE = P0008_WA-STVOR.                       "WRH ALRK054526
  ELSE.
*---Now process all the pay scale reclassification types specified in
*   the select option table to find the lowest reclassification date
    LOOP AT RCLS_TYP WHERE LOW NE SPACE AND LOW NE '01'.
      CLEAR RECLASS_DATE.                                "WRH AHRK023507
*-----Act according to the type of pay scale reclassification
      CASE RCLS_TYP-LOW.
        WHEN '02'.     "Use the user-provided reclassification date
          IF NOT REC_DATE IS INITIAL.
            RECLASS_DATE = REC_DATE.
          ENDIF.
        WHEN '03'.     "Use group membership duration from table T510R
          PERFORM CALC_GRP_MEMBERSHIP_JMP_DATE
                                         USING OLD_BASIC_PAY_WA-BEGDA
                                               PN-ENDDA
                                               T510R-DAUER
                                      CHANGING RECLASS_DATE.
        WHEN '04'.   "Use age jump from table T510R
          PERFORM CALC_AGE_JUMP_DATE USING P0002-GBDAT
                                           T510R-SPALT
                                  CHANGING RECLASS_DATE.
        WHEN '05'.  "Use Time-In-Grade information from table T510R
          PERFORM CALC_TIME_IN_GRADE_JMP_DATE
                               USING PN-BEGDA
                                     PN-ENDDA
                                     T510R-ANZHL
                                     P0008_WA            "WRH AHRK029426
                                     T503-TRFKZ          "WRH AHRK041998
                            CHANGING RECLASS_DATE.
        WHEN OTHERS.
**********Room for future enhancement:
*           Permit other types of pay scale reclassifications (standard
*           or customer-specific) via CMOD/SMOD transaction
      ENDCASE.
*-----Check if payscale reclassification date falls outside the validity
*     period of the currently valid payscale information. In addition,
*     don't permit reclassification date on the begin date of the
*     current payscale group/level combination
*     if reclass_date le old_basic_pay_wa-begda or       "WRH AHRK023507
*        reclass_date gt current_basic_pay_wa-endda.     "WRH AHRK023507
*       clear reclass_date.                              "WRH AHRK023507
*     endif.                                             "WRH AHRK023507
*-----Now set COMPARE_DATE to the lower date and set the reclassif. type
      IF NOT RECLASS_DATE IS INITIAL AND
         RECLASS_DATE LE COMPARE_DATE.                   "WRH AHRK023507
*        reclass_date lt compare_date.                   "WRH AHRK023507
        COMPARE_DATE = RECLASS_DATE.
        TYPE_PS_RECLASS = RCLS_TYP-LOW.
      ENDIF.
    ENDLOOP.
*---POST-REQUISITE: COMPARE_DATE should now contain either HIGH-DATE
*                   (in case no reclassification is to occur) or the
*                   lowest reclassification date
*---Now set the reclassification date
*   if compare_date eq high-date.                        "WRH AHRK023507
    IF TYPE_PS_RECLASS IS INITIAL.                       "WRH AHRK023507
      CLEAR RECLASS_DATE.
    ELSE.
      RECLASS_DATE = COMPARE_DATE.
    ENDIF.
  ENDIF.

*-Check the reclassification date prior to adding it to the table ctrl.
  PERFORM CHECK_RECLASS_DATE CHANGING RECLASS_DATE.

*-Generate entry in the Basic Pay control structure
* PERFORM append_p0008_result_tab USING reclass_date     "WRH PH4K002071
*                                       subty.           "WRH PH4K002071
ENDFORM.                    " CALCULATE_PS_JUMP_DATE

*&--------------------------------------------------------------------*
*&      Form  GET_P0008                                               *
*&--------------------------------------------------------------------*
*       Fill the structure PP0008 with the most current Basic Pay     *
*       record of the specified subtype.                              *
*---------------------------------------------------------------------*
*      -->P_P_INFOGROUP_WA-SUBTY : Subtype of the Basic Pay record to *
*                                  be read                            *
*      -->P_P_READ_P0008         : Read infotype P0008 ('0' - no,     *
*                                  '1' - yes)                         *
*---------------------------------------------------------------------*
*form get_p0008 using p_pernr       like pernr-pernr     "WRH ALRK054526
*                     p_p0008-subty like p0008-subty.    "WRH ALRK054526
*FORM get_p0008 USING p_p0008_subty     "WRH ALRK054526  "WRH PH4K002071
*               LIKE p0008-subty.       "WRH ALRK054526  "WRH PH4K002071
FORM GET_P0008  USING    P_SUBTY LIKE P0008-SUBTY        "WRH PH4K002071
                         P_BEGDA LIKE P0008-BEGDA        "WRH PH4K002071
                         P_ENDDA LIKE P0008-ENDDA        "WRH PH4K002071
                CHANGING P_SUBRC LIKE SY-SUBRC.          "WRH PH4K002071
DATA: P0008_WA LIKE P0008.                               "WRH ALRK054526
*local: p0008.                                           "WRH ALRK054526
* CLEAR pp0008_wa.                                       "WRH ALRK054526
  CLEAR P0008_WA.                                        "WRH ALRK054526
  CLEAR P_SUBRC.                                         "WRH PH4K002071
  REFRESH PP0008_TAB.                                    "WRH PH4K002071
*-Get the employee's most current Basic Pay record
* rp_provide_from_last p0008 p_p0008_subty               "WRH PH4K002071
*                            pn-begda pn-endda.          "WRH PH4K002071
  RP_PROVIDE_FROM_LAST P0008 P_SUBTY P_BEGDA P_ENDDA.    "WRH PH4K002071
  IF PNP-SW-FOUND NE '1'.                                "WRH PH4K002071
    P_SUBRC = 8.                                         "WRH PH4K002071
  ELSE.                                                  "WRH PH4K002071
*---Move current record into PP0008 workarea
*   MOVE-CORRESPONDING p0008 TO pp0008_wa.               "WRH ALRK054526
*   APPEND pp0008_wa TO pp0008_tab.                      "WRH ALRK054526
    MOVE-CORRESPONDING P0008 TO P0008_WA.                "WRH ALRK054526
    APPEND P0008_WA TO PP0008_TAB.                       "WRH ALRK054526
  ENDIF.                                                 "WRH PH4K002071
ENDFORM.                    " GET_PP0008

*&--------------------------------------------------------------------*
*&      Form  CREATE_NEW_PP0008_RECORD                                *
*&--------------------------------------------------------------------*
*       Create a new Basic Pay record                                 *
*---------------------------------------------------------------------*
FORM CREATE_NEW_PP0008_RECORD USING    VALUE(SUBTY) LIKE P0008-SUBTY
*                        VALUE(MOLGA) LIKE T500P-MOLGA   "WRH AHRK041998
                                       P_RECLASS_DATE LIKE P0008-BEGDA
                                       P_OLD_WAGE_TAB LIKE
                                                    OLD_WAGE_TYPE_TAB
                              CHANGING NEW_WAGE_TAB LIKE
                                                    NEW_WAGE_TYPE_TAB.
DATA: P8_CHANGED(1) VALUE '0'.
*local: p0008.                                           "WRH ALRK054526
DATA: NEW_WAGE_TYPE_WA       TYPE WAGE_TYPE_STRUC,
      WAGE_TYPE_WA           TYPE WAGE_TYPE_LINE,
      WAGE_TAB_LINE_COUNT    TYPE I,
      P0008_WA               LIKE P0008,                 "WRH ALRK054526
      MAXPOS                 TYPE I,                     "WRH PH4K002071
      WAGE_TYPE_INDBW        TYPE WAGE_TYPE_INDBW_TYPE,  "WRH PH4K002071
      HELP_WA                TYPE WAGE_TYPE_STRUC,       "WRH PH4K002071
      DUMMY_WA               TYPE WAGE_TYPE_STRUC,       "WRH PH4K002071
      POSITION               LIKE SY-TABIX,              "WRH PH4K002071
      HILFINDEX              LIKE SY-TABIX,              "WRH PH4K002071
*     subrc              like sy-subrc, "WRH PH4K002071  "WRH AHRK023507
      MOLGA                  LIKE T500P-MOLGA,           "WRH AHRK041998
      TRFKZ                  LIKE T503-TRFKZ,            "WRH AHRK041998
      INDBW_FLG              TYPE C,                     "WRH AHRK041998
      CURR                   LIKE P0008-WAERS,           "WRH AHRK041998
      SUBRC                  LIKE SY-SUBRC,              "WRH AHRK041998
      PROPOSED_LGART_TAB_WA  TYPE PROPOSED_LGART_STRUC.



* Get basic pay record on reclassification date
* perform get_p0008 using subty         "WRH PH4K002071  "WRH AHRK023507
*                         reclass_date  "WRH PH4K002071  "WRH AHRK023507
*                         reclass_date  "WRH PH4K002071  "WRH AHRK023507
*                   changing subrc.     "WRH PH4K002071  "WRH AHRK023507
* if subrc eq 0.                        "WRH PH4K002071  "WRH AHRK023507
  READ TABLE PP0008_TAB INDEX 1 INTO P0008_WA.           "WRH ALRK054526
* else.                                 "WRH PH4K002071  "WRH AHRK023507
*   clear p0008_wa.                     "WRH PH4K002071  "WRH AHRK023507
* endif.                                "WRH PH4K002071  "WRH AHRK023507

*-Set new pay scale information
  P0008_WA-TRFAR = OLD_BASIC_PAY_WA-TRFAR.               "WRH PH4K002071
  P0008_WA-TRFGB = OLD_BASIC_PAY_WA-TRFGB.               "WRH PH4K002071
  IF NOT T510R-TRFFG IS INITIAL.                         "WRH AHRK028021
    P0008_WA-TRFGR = T510R-TRFFG.                        "WRH PH4K002071
  ELSE.                                                  "WRH AHRK028021
    P0008_WA-TRFGR = OLD_BASIC_PAY_WA-TRFGR.             "WRH AHRK028021
  ENDIF.                                                 "WRH AHRK028021
  P0008_WA-TRFST = T510R-TRFFS.                          "WRH PH4K002071

*=Start building new P0008 record in P0008_WA
*-Clear P0008-STVOR if applicable
* IF pp0008_wa-stvor CN '0 ' AND                         "WRH ALRK054526
*    pp0008_wa-stvor EQ p_reclass_date.                  "WRH ALRK054526
*   CLEAR pp0008_wa-stvor.                               "WRH ALRK054526
* IF p0008_wa-stvor CN '0 ' AND         "WRH ALRK054526  "WRH PH4K002071
*    p0008_wa-stvor EQ p_reclass_date.  "WRH ALRK054526  "WRH PH4K002071
*   CLEAR p0008_wa-stvor.               "WRH ALRK054526  "WRH PH4K002071
* ENDIF.                                                 "WRH PH4K002071

*-Determine Org. Assignment, TRFKZ and MOLGA on BEGDA of old basic pay
  PERFORM RE_0001_TRFKZ_MOLGA USING P0008_WA-BEGDA       "WRH AHRK041998
                                    P0008_WA-BEGDA       "WRH AHRK041998
                                    'X'.                 "WRH AHRK041998
  MOLGA = T500P-MOLGA.                                   "WRH AHRK041998
  TRFKZ = T503-TRFKZ.                                    "WRH AHRK041998
*-Determine Org. Assignment, TRFKZ and MOLGA on reclassification date
  PERFORM RE_0001_TRFKZ_MOLGA USING P_RECLASS_DATE       "WRH AHRK041998
                                    P_RECLASS_DATE       "WRH AHRK041998
                                    'X'.                 "WRH AHRK041998

*-Determine new DIVGV and BSGRD and store results in P0008 workarea
  PERFORM DET_NEW_DIVGV_BSGRD USING    P_RECLASS_DATE
*                                      p0001             "WRH PH4K002071
*                                      p0008             "WRH ALRK054526
                                       P0008_WA          "WRH ALRK054526
                                       MOLGA
                                       TRFKZ             "WRH AHRK041998
                                       T500P-MOLGA       "WRH AHRK041998
                                       T503-TRFKZ
*                             CHANGING pp0008_wa-divgv   "WRH ALRK054526
*                                      pp0008_wa-bsgrd   "WRH ALRK054526
                              CHANGING P0008_WA-DIVGV    "WRH ALRK054526
                                       P0008_WA-BSGRD    "WRH ALRK054526
                                       P8_CHANGED.

*-Get the employee's most current Basic Pay record
* rp_provide_from_last p0008 subty pn-begda pn-endda.    "WRH ALRK054526
*-Start with empty internal table NEW_WAGE_TAB
  REFRESH NEW_WAGE_TAB.

*-Get new infotype 0001
* RP_PROVIDE_FROM_LAST P0001 SPACE      "WRH PH4K002071  "WRH AHRK041998
*                      RECLASS_DATE     "WRH PH4K002071  "WRH AHRK041998
*                      RECLASS_DATE.    "WRH PH4K002071  "WRH AHRK041998

*-Get default wage types from T539A on day of payscale reclassification
  PERFORM RE_T539A USING P_RECLASS_DATE
                         P0001-BUKRS
                         P0001-WERKS
                         P0001-BTRTL
                         P0001-PERSG
                         P0001-PERSK
*                        p0008-infty                     "WRH ALRK054526
                         P0008_WA-INFTY                  "WRH ALRK054526
                         SUBTY
*                        MOLGA                           "WRH AHRK041998
                         T500P-MOLGA                     "WRH AHRK041998
                CHANGING PROPOSED_LGART_TAB
                         P0008_LINE_COUNT.

*-Place current Basic Pay record into wage type workarea
* do 20 times                                            "WRH PH4K002071
  DO NUMBER_OF_WAGETYPES_0008 TIMES                            "N217573
* DO p0008_line_count TIMES                              "WRH PH4K002071
*    varying wage_type_wa from p0008-lga01               "WRH ALRK054526
*                         next p0008-lga02.              "WRH ALRK054526
     VARYING WAGE_TYPE_INDBW FROM P0008_WA-IND01         "WRH PH4K002071
                             NEXT P0008_WA-IND02         "WRH PH4K002071
     VARYING WAGE_TYPE_WA FROM P0008_WA-LGA01            "WRH ALRK054526
                          NEXT P0008_WA-LGA02.           "WRH ALRK054526
    CLEAR NEW_WAGE_TYPE_WA.                              "WRH PH4K002071
    IF WAGE_TYPE_WA-LGART NE SPACE AND
       WAGE_TYPE_WA-LGART CN '0 '.
*-----Wage type exists --> start with empty NEW_WAGE_TYPE_WA
*     CLEAR new_wage_type_wa.                            "WRH PH4K002071
      MOVE-CORRESPONDING WAGE_TYPE_WA TO NEW_WAGE_TYPE_WA.
      MOVE-CORRESPONDING WAGE_TYPE_INDBW TO              "WRH AHRK028021
                         NEW_WAGE_TYPE_WA.               "WRH AHRK028021
      NEW_WAGE_TYPE_WA-LGMOD = 'O'.                      "WRH PH4K002071
*-----Get information on wage type
      PERFORM GET_INFO_LGART USING    NEW_WAGE_TYPE_WA-LGART
                                      P_RECLASS_DATE
*                                     MOLGA              "WRH AHRK041998
                                      T500P-MOLGA        "WRH AHRK041998
                             CHANGING NEW_WAGE_TYPE_WA.
*-----Check if wage type is indirectly evaluated
      IF WAGE_TYPE_INDBW-INDBW NE 'I'.                   "WRH PH4K002071
        CLEAR NEW_WAGE_TYPE_WA-MODNA.                    "WRH PH4K002071
        CLEAR NEW_WAGE_TYPE_WA-INDBW.                    "WRH PH4K002071
      ENDIF.                                             "WRH PH4K002071
      ADD 1 TO WAGE_TAB_LINE_COUNT.                      "WRH PH4K002071
    ENDIF.                                               "WRH PH4K002071
*     IF new_wage_type_wa-betrg EQ 0 AND                 "WRH PH4K002071
*        new_wage_type_wa-modna ne space. "WRH ALRK054526"WRH PH4K002071
*  NOT new_wage_type_wa-modna IS INITIAL. "WRH ALRK054526"WRH PH4K002071
*       new_wage_type_wa-indbw = 'I'.                    "WRH PH4K002071
*     ELSE.                                              "WRH PH4K002071
*       new_wage_type_wa-indbw = space.                  "WRH PH4K002071
*     ENDIF.                                             "WRH PH4K002071
*-----Entry contained in internal table PROPOSED_LGART_TAB ?
*     READ TABLE proposed_lgart_tab                      "WRH PH4K002071
*          INTO  proposed_lgart_tab_wa                   "WRH PH4K002071
*          WITH  KEY lgart = new_wage_type_wa-lgart.     "WRH PH4K002071
*     IF sy-subrc EQ 0.                                  "WRH PH4K002071
*-------Entry found -> delete it in internal table PROPOSED_LGART_TAB
*       DELETE proposed_lgart_tab                        "WRH PH4K002071
*              WHERE lgart = new_wage_type_wa-lgart.     "WRH PH4K002071
*       ENDIF.                                           "WRH ALRK054526
*       MOVE-CORRESPONDING proposed_lgart_tab_wa         "WRH PH4K002071
*                          TO new_wage_type_wa.          "WRH PH4K002071
*     ENDIF.                            "WRH ALRK054526  "WRH PH4K002071

*---Append the work area to the internal wage type table
*   APPEND new_wage_type_wa TO new_wage_type_tab.        "WRH ALRK054526
    APPEND NEW_WAGE_TYPE_WA TO NEW_WAGE_TAB.             "WRH ALRK054526
*---New wage type has been added
*   ADD 1 TO wage_tab_line_count.                        "WRH PH4K002071
*    ELSE.                                               "WRH PH4K002071
*-----No more wage types to process                      "WRH PH4K002071
*     EXIT.                                              "WRH PH4K002071
*   ENDIF.                                               "WRH PH4K002071
  ENDDO.

*-Now process the internal table PROPOSED_LGART_TAB
  LOOP AT    PROPOSED_LGART_TAB
       INTO  PROPOSED_LGART_TAB_WA
       WHERE LGMOD EQ 'F'.
    POSITION = SY-TABIX.                                 "WRH PH4K002071
    READ TABLE NEW_WAGE_TAB INTO NEW_WAGE_TYPE_WA        "WRH PH4K002071
         WITH KEY LGART = PROPOSED_LGART_TAB_WA-LGART.   "WRH PH4K002071
    IF SY-SUBRC NE 0.                                    "WRH PH4K002071
*     proposed wage type not found
      IF PROPOSED_LGART_TAB_WA-LGMOD = 'F'.              "WRH PH4K002071
*       IF wage_tab_line_count GE       "WRH ALRK054526  "WRH PH4K002071
*          p0008_line_count.            "WRH ALRK054526  "WRH PH4K002071
*         EXIT.                         "WRH ALRK054526  "WRH PH4K002071
*       ENDIF.                          "WRH ALRK054526  "WRH PH4K002071
        CLEAR NEW_WAGE_TYPE_WA.                          "WRH ALRK054526
*-------Get information on wage type
        PERFORM GET_INFO_LGART                           "WRH ALRK054526
                    USING    PROPOSED_LGART_TAB_WA-LGART "WRH ALRK054526
                             P_RECLASS_DATE              "WRH ALRK054526
*                            MOLGA      "WRH ALRK054526  "WRH AHRK041998
                             T500P-MOLGA                 "WRH AHRK041998
                    CHANGING NEW_WAGE_TYPE_WA.           "WRH ALRK054526
*-------Check if wage type is indirectly evaluated
        IF NEW_WAGE_TYPE_WA-BETRG EQ 0 AND               "WRH ALRK054526
           NOT NEW_WAGE_TYPE_WA-MODNA IS INITIAL.        "WRH ALRK054526
          NEW_WAGE_TYPE_WA-INDBW = 'I'.                  "WRH ALRK054526
        ELSE.                                            "WRH ALRK054526
          CASE NEW_WAGE_TYPE_WA-KOMBI.                   "WRH ALRK054526
            WHEN '0'.                                    "WRH ALRK054526
*-------------ok, because proposed wage type is directly evaluated and
*             amount and unit must not be entered
            WHEN '7'.                                    "WRH ALRK054526
*-------------warning, because proposed wage type is directly
*             evaluated and entry is optional
              PERFORM APPEND_ERROR_LIST                  "WRH ALRK054526
                      USING PERNR-PERNR                  "WRH ALRK054526
                            'PN' 'W' '016'               "WRH ALRK054526
                            TEXT-E13                     "WRH ALRK054526
                            PROPOSED_LGART_TAB_WA-LGART  "WRH ALRK054526
                            SPACE                        "WRH ALRK054526
                            SPACE.                       "WRH ALRK054526
            WHEN OTHERS.                                 "WRH ALRK054526
*-------------error, because proposed wage type is directly
*             evaluated and entry is mandatory
*             (can not be handled by Batch-Input)
              PERFORM APPEND_ERROR_LIST                  "WRH ALRK054526
                      USING PERNR-PERNR                  "WRH ALRK054526
                            'PN' 'E' '016'               "WRH ALRK054526
                            TEXT-E13                     "WRH ALRK054526
                            PROPOSED_LGART_TAB_WA-LGART  "WRH ALRK054526
                            SPACE                        "WRH ALRK054526
                            SPACE.                       "WRH ALRK054526
              ADD 1 TO NO_REJECTED.                      "WRH ALRK054526
              PERFORM REJECT_PERNR.                      "WRH ALRK054526
          ENDCASE.                                       "WRH ALRK054526
        ENDIF.                                           "WRH ALRK054526
        MOVE-CORRESPONDING PROPOSED_LGART_TAB_WA TO      "WRH ALRK054526
                           NEW_WAGE_TYPE_WA.             "WRH ALRK054526
*       APPEND new_wage_type_wa         "WRH ALRK054526  "WRH PH4K002071
*              TO new_wage_tab.         "WRH ALRK054526  "WRH PH4K002071
        INSERT NEW_WAGE_TYPE_WA INTO NEW_WAGE_TAB        "WRH PH4K002071
                                INDEX POSITION.          "WRH PH4K002071
*-------New wage type has been added
        ADD 1 TO WAGE_TAB_LINE_COUNT.                    "WRH ALRK054526
      ENDIF.                                             "WRH PH4K002071
    ELSE.                                                "WRH PH4K002071
      HILFINDEX = SY-TABIX.                              "WRH PH4K002071
      IF HILFINDEX LT POSITION.                          "WRH PH4K002071
        CLEAR HELP_WA.                                   "WRH PH4K002071
        MODIFY NEW_WAGE_TAB FROM HELP_WA                 "WRH PH4K002071
                            INDEX HILFINDEX.             "WRH PH4K002071
      ELSE.                                              "WRH PH4K002071
        DELETE NEW_WAGE_TAB INDEX HILFINDEX.             "WRH PH4K002071
      ENDIF.                                             "WRH PH4K002071
      MOVE-CORRESPONDING PROPOSED_LGART_TAB_WA TO        "WRH PH4K002071
                         NEW_WAGE_TYPE_WA.               "WRH PH4K002071
      INSERT NEW_WAGE_TYPE_WA INTO NEW_WAGE_TAB          "WRH PH4K002071
                              INDEX POSITION.            "WRH PH4K002071
    ENDIF.                                               "WRH PH4K002071
  ENDLOOP.

*-Set the sequence numbers
  PERFORM SET_SEQNR USING    P0008_LINE_COUNT
*                   CHANGING wage_tab_line_count         "WRH PH4K002071
                    CHANGING MAXPOS                      "WRH PH4K002071
                             NEW_WAGE_TAB.

  IF MAXPOS GT P0008_LINE_COUNT.                         "WRH PH4K002071
    PERFORM APPEND_ERROR_LIST                            "WRH PH4K002071
                   USING PERNR-PERNR '3F' 'E' '351'      "WRH PH4K002071
                         SPACE SPACE SPACE SPACE.        "WRH PH4K002071
    ADD 1 TO NO_REJECTED.                                "WRH PH4K002071
    PERFORM REJECT_PERNR.                                "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071

  POSITION = MAXPOS + 1.                                 "WRH PH4K002071
  DELETE NEW_WAGE_TAB FROM POSITION.                     "WRH PH4K002071

*-Check that amount fields are overwritable and set LGMOD
  CLEAR INDBW_FLG.                                       "WRH AHRK041998
  LOOP AT NEW_WAGE_TAB INTO NEW_WAGE_TYPE_WA.            "WRH PH4K002071
    HILFINDEX = SY-TABIX.                                "WRH PH4K002071
    IF NOT NEW_WAGE_TYPE_WA-LGART IS INITIAL.            "WRH PH4K002071
      IF NOT NEW_WAGE_TYPE_WA-MODNA IS INITIAL.          "WRH AHRK041998
        INDBW_FLG = 'X'.                                 "WRH AHRK041998
      ENDIF.                                             "WRH AHRK041998
      READ TABLE P_OLD_WAGE_TAB INTO HELP_WA             "WRH PH4K002071
                                INDEX HILFINDEX.         "WRH PH4K002071
      IF SY-SUBRC = 0.                                   "WRH PH4K002071
        IF NEW_WAGE_TYPE_WA-MOD02 = 'F' AND              "WRH PH4K002071
           NEW_WAGE_TYPE_WA-MODNA IS INITIAL.            "WRH PH4K002071
          PERFORM GET_INFO_LGART                         "WRH PH4K002071
                      USING    NEW_WAGE_TYPE_WA-LGART    "WRH PH4K002071
                               P_RECLASS_DATE            "WRH PH4K002071
*                              MOLGA    "WRH PH4K002071  "WRH AHRK041998
                               T500P-MOLGA               "WRH AHRK041998
                      CHANGING DUMMY_WA.                 "WRH PH4K002071
          IF NOT DUMMY_WA-MODNA IS INITIAL.              "WRH PH4K002071
*           error: Direct evaluation not possible for a wage type
*                  that has a non-overwritable amount field and
*                  that should be indirectly evaluated according
*                  to table T511 (field MODNA not initial)
            PERFORM APPEND_ERROR_LIST                    "WRH PH4K002071
                    USING PERNR-PERNR '3F' 'E' '352'     "WRH PH4K002071
                          NEW_WAGE_TYPE_WA-LGART         "WRH PH4K002071
                          SPACE SPACE SPACE.             "WRH PH4K002071
            ADD 1 TO NO_REJECTED.                        "WRH PH4K002071
            PERFORM REJECT_PERNR.                        "WRH PH4K002071
          ENDIF.                                         "WRH PH4K002071
        ENDIF.                                           "WRH PH4K002071
        IF HELP_WA-MOD02 = 'F' AND                       "WRH PH4K002071
           NOT HELP_WA-MODNA IS INITIAL AND              "WRH PH4K002071
           NEW_WAGE_TYPE_WA-MODNA IS INITIAL.            "WRH PH4K002071
*         error: A wage type that is directly evaluated cannot
*                be placed on a position where a indirectly
*                evaluated wage type with a non-overwritable
*                amount field has been existing in the old basic
*                pay record
*                --> The wage type should be positioned to the end
*                    if there is space left and LGMOD <> 'F'
          IF NEW_WAGE_TYPE_WA-LGMOD <> 'F'.              "WRH PH4K002071
            IF  MAXPOS < P0008_LINE_COUNT.               "WRH PH4K002071
              POSITION = MAXPOS * 10 + 10.               "WRH PH4K002071
              UNPACK POSITION TO NEW_WAGE_TYPE_WA-SEQNR. "WRH PH4K002071
              APPEND NEW_WAGE_TYPE_WA TO NEW_WAGE_TAB.   "WRH PH4K002071
              MAXPOS = MAXPOS + 1.                       "WRH PH4K002071
              CLEAR NEW_WAGE_TYPE_WA.                    "WRH PH4K002071
              POSITION = HILFINDEX * 10.                 "WRH PH4K002071
              UNPACK POSITION TO NEW_WAGE_TYPE_WA-SEQNR. "WRH PH4K002071
              MODIFY NEW_WAGE_TAB                        "WRH PH4K002071
                     FROM NEW_WAGE_TYPE_WA               "WRH PH4K002071
                     INDEX HILFINDEX.                    "WRH PH4K002071
            ELSE.                                        "WRH PH4K002071
              PERFORM APPEND_ERROR_LIST                  "WRH PH4K002071
                      USING PERNR-PERNR '3F' 'E' '351'   "WRH PH4K002071
                            SPACE SPACE SPACE SPACE.     "WRH PH4K002071
              ADD 1 TO NO_REJECTED.                      "WRH PH4K002071
              PERFORM REJECT_PERNR.                      "WRH PH4K002071
            ENDIF.                                       "WRH PH4K002071
          ELSE.                                          "WRH PH4K002071
            PERFORM APPEND_ERROR_LIST                    "WRH PH4K002071
                    USING PERNR-PERNR '3F' 'E' '353'     "WRH PH4K002071
                          NEW_WAGE_TYPE_WA-LGART         "WRH PH4K002071
                          SPACE SPACE SPACE.             "WRH PH4K002071
            ADD 1 TO NO_REJECTED.                        "WRH PH4K002071
            PERFORM REJECT_PERNR.                        "WRH PH4K002071
          ENDIF.                                         "WRH PH4K002071
        ENDIF.                                           "WRH PH4K002071
        IF ( NOT NEW_WAGE_TYPE_WA-LGART IS INITIAL ) AND "WRH PH4K002071
          ( NEW_WAGE_TYPE_WA-LGART = HELP_WA-LGART ) AND "WRH PH4K002071
           ( NEW_WAGE_TYPE_WA-LGMOD = 'F' ) AND          "WRH PH4K002071
           ( HELP_WA-LGMOD = 'F' ).                      "WRH PH4K002071
          CLEAR NEW_WAGE_TYPE_WA-LGMOD.                  "WRH PH4K002071
          MODIFY NEW_WAGE_TAB                            "WRH PH4K002071
                 FROM NEW_WAGE_TYPE_WA                   "WRH PH4K002071
                 INDEX HILFINDEX.                        "WRH PH4K002071
        ENDIF.                                           "WRH PH4K002071
      ENDIF.                                             "WRH PH4K002071
    ENDIF.                                               "WRH PH4K002071
  ENDLOOP.                                               "WRH PH4K002071

*-Add empty lines if necessary
  IF MAXPOS < WAGE_TAB_LINE_COUNT.                       "WRH PH4K002071
    CLEAR NEW_WAGE_TYPE_WA.                              "WRH PH4K002071
    POSITION = WAGE_TAB_LINE_COUNT - MAXPOS.             "WRH PH4K002071
    DO POSITION TIMES.                                   "WRH PH4K002071
      APPEND NEW_WAGE_TYPE_WA TO NEW_WAGE_TAB.           "WRH PH4K002071
      MAXPOS = MAXPOS + 1.                               "WRH PH4K002071
    ENDDO.                                               "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071

*-Determine currency of new basic pay record
  OLD_BASIC_PAY_WA-WAERS = P0008_WA-WAERS.               "WRH AHRK041998
  PERFORM GET_CURRENCY                                   "WRH AHRK041998
          USING    T500P-MOLGA                           "WRH AHRK041998
                   MOLGA                                 "WRH AHRK041998
                   P0008_WA-TRFAR                        "WRH AHRK041998
                   P0008_WA-TRFGB                        "WRH AHRK041998
                   T503-TRFKZ                            "WRH AHRK041998
                   TRFKZ                                 "WRH AHRK041998
                   P_RECLASS_DATE                        "WRH AHRK041998
                   P0008_WA-BEGDA                        "WRH AHRK041998
                   P0008_WA-WAERS                        "WRH AHRK041998
                   INDBW_FLG                             "WRH AHRK041998
          CHANGING CURR                                  "WRH AHRK041998
                   SUBRC.                                "WRH AHRK041998
  IF SUBRC <> 0.                                         "WRH AHRK041998
    IF SUBRC = 1.                                        "WRH AHRK041998
      PERFORM APPEND_ERROR_LIST                          "WRH AHRK041998
              USING PERNR-PERNR                          "WRH AHRK041998
                    '3F' 'E' '355'                       "WRH AHRK041998
                    SPACE SPACE SPACE SPACE.             "WRH AHRK041998
    ELSEIF SUBRC = 2.                                    "WRH AHRK041998
      PERFORM APPEND_ERROR_LIST                          "WRH AHRK041998
              USING PERNR-PERNR                          "WRH AHRK041998
                    '3F' 'E' '356'                       "WRH AHRK041998
                    SPACE SPACE SPACE SPACE.             "WRH AHRK041998
    ENDIF.                                               "WRH AHRK041998
    ADD 1 TO NO_REJECTED.                                "WRH AHRK041998
    PERFORM REJECT_PERNR.                                "WRH AHRK041998
  ELSE.                                                  "WRH AHRK041998
    P0008_WA-WAERS = CURR.                               "WRH AHRK041998
  ENDIF.                                                 "WRH AHRK041998

*-Perform the indirect evaluation of internal table NEW_WAGE_TYPE_TAB
* perform indbw_lgart                                    "WRH ALRK054526
*         using    p0008-pernr p0008-infty               "WRH ALRK054526
*                  p_reclass_date p0008-endda 'W'        "WRH ALRK054526
*         changing p0008-begda p0008-endda               "WRH ALRK054526
*                  new_wage_tab.                         "WRH ALRK054526
  PERFORM INDBW_LGART                                    "WRH ALRK054526
*         using    p0008_wa             "WRH ALRK054526  "WRH AHRK028021
          USING    STR_0008                              "WRH AHRK028021
                   P_RECLASS_DATE                        "WRH ALRK054526
                   P0008_WA                              "WRH AHRK028021
                   'W'                                   "WRH ALRK054526
          CHANGING NEW_WAGE_TAB.                         "WRH ALRK054526

*-Convert currency
  LOOP AT NEW_WAGE_TAB INTO NEW_WAGE_TYPE_WA.            "WRH AHRK041998
    IF NEW_WAGE_TYPE_WA-MODNA IS INITIAL AND             "WRH AHRK041998
       P0008_WA-WAERS <> OLD_BASIC_PAY_WA-WAERS.         "WRH AHRK041998
      CALL FUNCTION 'CONVERT_TO_LOCAL_CURRENCY'          "WRH AHRK041998
           EXPORTING                                     "WRH AHRK041998
              date             = P_RECLASS_DATE          "WRH AHRK041998
              foreign_amount   = NEW_WAGE_TYPE_WA-BETRG  "WRH AHRK041998
              foreign_currency = OLD_BASIC_PAY_WA-WAERS  "WRH AHRK041998
              local_currency   = P0008_WA-WAERS          "WRH AHRK041998
           IMPORTING                                     "WRH AHRK041998
              local_amount     = NEW_WAGE_TYPE_WA-BETRG  "WRH AHRK041998
           EXCEPTIONS                                    "WRH AHRK041998
              OTHERS           = 1.                      "WRH AHRK041998
      IF SY-SUBRC <> 0.                                  "WRH AHRK041998
        PERFORM APPEND_ERROR_LIST                        "WRH AHRK041998
                USING PERNR-PERNR                        "WRH AHRK041998
                      '3F' 'E' '357'                     "WRH AHRK041998
                      SPACE SPACE SPACE SPACE.           "WRH AHRK041998
        ADD 1 TO NO_REJECTED.                            "WRH AHRK041998
        PERFORM REJECT_PERNR.                            "WRH AHRK041998
      ELSE.                                              "WRH AHRK041998
        MODIFY NEW_WAGE_TAB FROM NEW_WAGE_TYPE_WA.       "WRH AHRK041998
      ENDIF.                                             "WRH AHRK041998
    ENDIF.                                               "WRH AHRK041998
  ENDLOOP.                                               "WRH AHRK041998

  MODIFY PP0008_TAB INDEX 1                              "WRH ALRK054526
                    FROM P0008_WA                        "WRH ALRK054526
                    TRANSPORTING DIVGV                   "WRH ALRK054526
                                 BSGRD                   "WRH ALRK054526
                                 WAERS.                  "WRH AHRK041998
ENDFORM.                    " CREATE_NEW_PP0008_RECORD

*&---------------------------------------------------------------------*
*&      Form  DET_NEW_DIVGV_BSGRD                                      *
*&---------------------------------------------------------------------*
*       Determine new DIVGV and BSGRD
*----------------------------------------------------------------------*
*  -->  VALUE(RECLASS_DATE)  : Reclassification date
*  -->  VALUE(P0001)         : Infotype P0001
*  -->  VALUE(P0008)         : Infotype P0008
*  -->  VALUE(MOLGA)         : Country grouping
*  -->  VALUE(TRFKZ)         : Pay scale indicator
*  <--  P_DIVGV              : Hours worked per accounting period
*  <--  P_BSGRD              : Capacity utilization level
*  <--  P_CHANGED            : Flag to indicate any changes
*----------------------------------------------------------------------*
FORM DET_NEW_DIVGV_BSGRD
         USING    VALUE(RECLASS_DATE) LIKE P0008-BEGDA
*                 value(p0001)        LIKE p0001         "WRH PH4K002071
                  VALUE(P0008)        LIKE P0008
                  VALUE(MOLGA)        LIKE T500P-MOLGA
                  VALUE(TRFKZ)        LIKE T503-TRFKZ
                  VALUE(NEW_MOLGA)    LIKE T500P-MOLGA   "WRH AHRK041998
                  VALUE(NEW_TRFKZ)    LIKE T503-TRFKZ    "WRH AHRK041998
         CHANGING P_DIVGV             LIKE P0008-DIVGV
                  P_BSGRD             LIKE P0008-BSGRD
                  P_CHANGED           TYPE C.
  DATA: TMP_DIVGV LIKE P0008-DIVGV,
        TMP_BSGRD LIKE P0008-BSGRD,
        DFINF(1),                                        "WRH PH0K000464
        STATUS(2) TYPE P.                                "WRH PH0K000464

  CLEAR PME42.                                           "WRH PH0K000464
  MOVE STR_0008 TO PME42-INFTY.                          "WRH PH0K000464
  MOVE TUMMASSN+0(2) TO PME42-MASSN.                           "N0310914
  MOVE TUMMASSN+2(2) TO PME42-MASSG.                           "N0310914
  PERFORM RE549D USING 'DFINF' '1' DFINF STATUS.         "WRH PH0K000464
  IF DFINF = 'X'.                                        "WRH PH0K000464
*---Store values temporarily for later comparison
    MOVE P_DIVGV TO TMP_DIVGV.
    MOVE P_BSGRD TO TMP_BSGRD.
*---Determine DIVGV/BSGRD for current P0008 record
*   if values from P0008 <> P0007 -> something has been changed manually
*   PERFORM get_divgv USING    p0001                     "WRH PH4K002071
    PERFORM GET_DIVGV USING                              "WRH PH4K002071
                               MOLGA
                               P0008-PERNR
                               P0008-BEGDA
                               P0008-ENDDA
                               P0008-TRFAR
                               P0008-TRFGB
                               TRFKZ
                      CHANGING P_BSGRD
                               P_DIVGV.
    IF NOT ( P_DIVGV EQ TMP_DIVGV AND P_BSGRD EQ TMP_BSGRD ).
*-----Current values are not the values proposed (values have been
*     manually changed) -> keep them on the current and the new record
*     (no changes)
      P_DIVGV = TMP_DIVGV.
      P_BSGRD = TMP_BSGRD.
      EXIT.
    ENDIF.
*---Now get DIVGV/BSGRD for new P0008 record (at reclassification date)
*   PERFORM get_divgv USING    p0001                     "WRH PH4K002071
    PERFORM GET_DIVGV USING                              "WRH PH4K002071
*                              MOLGA                     "WRH AHRK041998
                               NEW_MOLGA                 "WRH AHRK041998
                               P0008-PERNR
                               RECLASS_DATE
                               P0008-ENDDA
                               P0008-TRFAR
                               P0008-TRFGB
*                              TRFKZ                     "WRH AHRK041998
                               NEW_TRFKZ                 "WRH AHRK041998
                      CHANGING P_BSGRD
                               P_DIVGV.
    CHECK P_DIVGV NE TMP_DIVGV OR P_BSGRD NE TMP_BSGRD.
    P_CHANGED = YES.
  ENDIF.                                                 "WRH PH0K000464
ENDFORM.                    " DET_NEW_DIVGV_BSGRD

*&--------------------------------------------------------------------*
*&      Form  GET_DIVGV                                               *
*&--------------------------------------------------------------------*
*       Get p0008-divgv and p0008-bsgrd from P0007                    *
*---------------------------------------------------------------------*
*      PREREQUISITES: P0001, P0008                                    *
*---------------------------------------------------------------------*
*      -->VALUE(P0001) : Infotype P0001 (used to fill feature PME01)  *
*      -->VALUE(MOLGA) : Country grouping (used to fill feature PME01)*
*      -->VALUE(PERNR) : Basic Pay begin date (used to read P0007)    *
*      -->VALUE(BEGDA) : Basic Pay begin date (used to read P0007)    *
*      -->VALUE(ENDDA) : Basic Pay end date (used to read P0007)      *
*      -->VALUE(TRFAR) : Pay scale type                               *
*      -->VALUE(TRFGB) : Pay scale area                               *
*      -->VALUE(TRFKZ) : Pay scale indicator                          *
*      <--P_P_BSGRD    : Return value for P0008-BSGRD                 *
*      <--P_P_DIVGV    : Return value for P0008-DIVGV                 *
*---------------------------------------------------------------------*
*FORM get_divgv USING   value(p0001) LIKE p0001          "WRH PH4K002071
FORM GET_DIVGV USING                                     "WRH PH4K002071
                        VALUE(MOLGA) LIKE T500P-MOLGA
*                       value(pernr) like p0008-pernr    "WRH ALRK044773
                        VALUE(P_PERNR) LIKE P0008-PERNR  "WRH ALRK044773
                        VALUE(BEGDA) LIKE P0008-BEGDA
                        VALUE(ENDDA) LIKE P0008-ENDDA
                        VALUE(TRFAR) LIKE P0008-TRFAR
                        VALUE(TRFGB) LIKE P0008-TRFGB
                        VALUE(TRFKZ) LIKE T503-TRFKZ
               CHANGING P_P_BSGRD    LIKE P0008-BSGRD
                        P_P_DIVGV    LIKE P0008-DIVGV.
* data: use_510f(1) type c,                              "WRH ALRK044773
*       tmp_zeinh like t510f-zeinh,                      "WRH ALRK044773
*       subrc like sy-subrc.                             "WRH ALRK044773
  DATA: TMP_ZEINH LIKE T549R-ZEINH,                      "WRH ALRK044773
        TABLE_ARG(19).                                   "WRH ALRK044773

*-Get infotype P0007 record
  PERFORM READ_INFOTYPE(SAPFP50P) USING P_PERNR '0007'
          SPACE SPACE '4' BEGDA ENDDA '1' 'NOP' P0007.
  IF SY-SUBRC EQ 0.
    MOVE P0007-EMPCT TO P_P_BSGRD.
*   MOVE-CORRESPONDING p0001 TO pme01.                   "WRH PH4K002071
    PME01-MOLGA = MOLGA.
*   perform re549d using 'U510F' '2' use_510f subrc.     "WRH ALRK044773
*   if use_510f eq 'X' and subrc eq 0.                   "WRH ALRK044773
*     perform re_510f using molga trfar trfgb            "WRH ALRK044773
*                           trfkz begda.                 "WRH ALRK044773
*     tmp_zeinh = t510f-zeinh.                           "WRH ALRK044773
*   else.                                                "WRH ALRK044773
*     tmp_zeinh = '01'.                                  "WRH ALRK044773
*   endif.                                               "WRH ALRK044773
    CALL FUNCTION 'RP_ZEINH_GET'                         "WRH ALRK044773
         EXPORTING                                       "WRH ALRK044773
              P_MOLGA        = MOLGA                     "WRH ALRK044773
              P_TRFGB        = TRFGB                     "WRH ALRK044773
              P_TRFAR        = TRFAR                     "WRH ALRK044773
              P_TRFKZ        = TRFKZ                     "WRH ALRK044773
              P_DATE         = BEGDA                     "WRH ALRK044773
         IMPORTING                                       "WRH ALRK044773
              P_ZEINH        = TMP_ZEINH                 "WRH ALRK044773
         EXCEPTIONS                                      "WRH ALRK044773
              NO_ENTRY_T549R = 1                         "WRH ALRK044773
              OTHERS         = 2.                        "WRH ALRK044773
    IF SY-SUBRC <> 0.                                    "WRH ALRK044773
*-----Error handling                                     "WRH ALRK044773
      CONCATENATE MOLGA TRFAR TRFGB TRFKZ BEGDA          "WRH ALRK044773
                  INTO TABLE_ARG SEPARATED BY SPACE.     "WRH ALRK044773
      PERFORM APPEND_ERROR_LIST                          "WRH ALRK044773
              USING PERNR-PERNR 'PN' 'E' '001'           "WRH ALRK044773
                    'T549R' TABLE_ARG SPACE SPACE.       "WRH ALRK044773
*     Increment number of rejected ees                   "WRH ALRK044773
      ADD 1 TO NO_REJECTED.                              "WRH ALRK044773
*     Process next personnel number                      "WRH ALRK044773
      PERFORM REJECT_PERNR.                              "WRH ALRK044773
    ENDIF.                                               "WRH ALRK044773
    CASE TMP_ZEINH.                    "pay frequency
      WHEN '01'.                       "monthly
        P_P_DIVGV = P0007-MOSTD.
      WHEN '02'.                       "semimonthly
        P_P_DIVGV = P0007-MOSTD / 2.
      WHEN '03'.                       "weekly
        P_P_DIVGV = P0007-WOSTD.
      WHEN '04'.                       "biweekly
        P_P_DIVGV = P0007-WOSTD * 2.
      WHEN '05'.                       "4-weekly
        P_P_DIVGV = P0007-WOSTD * 4.
      WHEN OTHERS.
        P_P_DIVGV = P0007-MOSTD.
    ENDCASE.
  ENDIF.
ENDFORM.                    " GET_DIVGV

*&---------------------------------------------------------------------*
*&      Form  GET_STD_VARIABLE_ARG
*&---------------------------------------------------------------------*
*       Determine standard variable argument for Cluster B2 tables
*       VS and CVS. This is always used in conjunction with the TIG
*       identifier ITIG.
*----------------------------------------------------------------------*
*      -->P_P0008         : infotype P0008 (Basic Pay)                 *
*      <--P_VARIABLE_ARG  : variable argument for reading VS and CVS   *
*      <--P_VS_IDENTIFIER : standard Time-In-Grade identifier 'ITIG'   *
*----------------------------------------------------------------------*
FORM GET_STD_VARIABLE_ARG
             USING    VALUE(P0008)   LIKE P0008
*                     VALUE(T503)    LIKE T503           "WRH AHRK041998
                      VALUE(P_TRFKZ) LIKE T503-TRFKZ     "WRH AHRK041998
             CHANGING P_VARIABLE_ARG LIKE PC2BH-VARIA
                      P_VS_IDENTIFIER LIKE IVS_TAB-IDENT.
*-Initialize the variable argument
  CLEAR P_VARIABLE_ARG.
*-Fill it with the standard arguments
  P_VARIABLE_ARG+0(2)  = P0008-TRFAR.
  P_VARIABLE_ARG+2(2)  = P0008-TRFGB.
  P_VARIABLE_ARG+4(8)  = P0008-TRFGR.
  P_VARIABLE_ARG+12(2) = P0008-TRFST.
* P_VARIABLE_ARG+14(1) = T503-TRFKZ.                     "WRH AHRK041998
  P_VARIABLE_ARG+14(1) = P_TRFKZ.                        "WRH AHRK041998
*-Also return the standard Time-In-Grade identifier 'ITIG'
  P_VS_IDENTIFIER      = 'ITIG'.
ENDFORM.                    " GET_STD_VARIABLE_ARG

*&---------------------------------------------------------------------*
*&      Form  GET_USR_VARIABLE_ARG
*&---------------------------------------------------------------------*
*       User exit to set up a customer-defined variable argument for
*       processing tables VS and CVS from Cluster B2.
*       The identifier used must differ from the standard Time-In-Grade
*       identifier ITIG.
*----------------------------------------------------------------------*
*  <--  P_VS_IDENTIFIER           : Time-In-Grade identifier
*  <--  P_USER_EXIT_001_ACTIVATED : Activation switch (user exit '001')
*----------------------------------------------------------------------*
FORM GET_USR_VARIABLE_ARG CHANGING P_VS_IDENTIFIER LIKE PC2BH-IDENT
                                   P_USER_EXIT_001_ACTIVATED LIKE TRUE.
DATA: USER_EXIT_001_LINES TYPE P,
      REJECT_PERNR_SWITCH LIKE RPIXXXXX-KR_FELD1.
*-Initialize internal table of structure RPITIGVA
  CLEAR VAR_ARG_USER_EXIT.
  REFRESH VAR_ARG_USER_EXIT.
*-User exit for determining the customer-defined variable argument
  CALL CUSTOMER-FUNCTION '001'
       EXPORTING
            PPERNR             = PERNR-PERNR
            PSGRPLVL_BEGDA     = PSGRPLVL_BEGDA
            EMPLOYEE_AGE       = EMPLOYEE_AGE
            YEARS_OF_SENIORITY = YEARS_OF_SENIORITY
            HIRE_DATE          = HIRE_DATE
            FIRE_DATE          = FIRE_DATE
            WORK_HOURS_LIMIT   = T510R-ANZHL
            AGE_LIMIT          = T510R-SPALT
            SENIORITY_LIMIT    = T510R-DAUER
       IMPORTING
            REJECT_PERNR       = REJECT_PERNR_SWITCH
            VS_IDENTIFIER      = P_VS_IDENTIFIER
       TABLES
            PP0000             = P0000
            PP0001             = P0001
            PP0002             = P0002
            PP0003             = P0003
            PP0007             = P0007
            PP0008             = P0008
            P_ERROR_TAB        = ERR_TAB
            VAR_ARGUMENT       = VAR_ARG_USER_EXIT.
*-Reject personnel number?
  IF NOT REJECT_PERNR_SWITCH IS INITIAL.
    ADD 1 TO NO_REJECTED.
    PERFORM REJECT_PERNR.
  ENDIF.
*-Check for any entries in internal table VAR_ARG_USER_EXIT
  DESCRIBE TABLE VAR_ARG_USER_EXIT LINES USER_EXIT_001_LINES.
  CHECK USER_EXIT_001_LINES NE 0.
*-Internal interface table contains entries -> set activation switch
  P_USER_EXIT_001_ACTIVATED = TRUE.
*-Check that customer Time-In-Grade identifier is not equal to the
* standard Time-In-Grade identifier
  IF P_VS_IDENTIFIER EQ 'ITIG'.
    PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '016'
                                    TEXT-E09 SPACE SPACE SPACE.
    ADD 1 TO NO_REJECTED.
    PERFORM REJECT_PERNR.
  ENDIF.
ENDFORM.                    " GET_USR_VARIABLE_ARG

*&---------------------------------------------------------------------*
*&      Form  PROCESS_USER_EXIT_001_DATA
*&---------------------------------------------------------------------*
*       Process the internal table returned by the user exit '001' and
*       determine the variable argument for tables VS and CVS.
*----------------------------------------------------------------------*
*      -->P_USER_EXIT_001 : Internal table returned by user exit '001' *
*      <--P_VARIABLE_ARG  : Variable argument for tables VS and CVS    *
*----------------------------------------------------------------------*
FORM PROCESS_USER_EXIT_001_DATA
     TABLES   P_USER_EXIT_001 STRUCTURE VAR_ARG_USER_EXIT
     CHANGING P_VARIABLE_ARG  LIKE PC2BH-VARIA.
DATA: OFFSET TYPE I,
      LENGTH TYPE I,
      MAX_VAR_ARG_LENGTH TYPE P,
      VAR_ARG_LENGTH TYPE P.

*-Initialize the variable argument
  CLEAR P_VARIABLE_ARG.

*-Find length of variable argument
  DESCRIBE FIELD P_VARIABLE_ARG LENGTH MAX_VAR_ARG_LENGTH.

*-Process the internal table
  LOOP AT P_USER_EXIT_001.
*---Check for permissible internal table entries
    IF NOT P_USER_EXIT_001-TABNAME   IS INITIAL AND
       NOT P_USER_EXIT_001-FIELDNAME IS INITIAL.
*-----Check for fieldlength
      IF P_USER_EXIT_001-LENGTH IS INITIAL.
        SELECT LENG INTO  P_USER_EXIT_001-LENGTH
                    FROM  DD03L
                    WHERE TABNAME   EQ P_USER_EXIT_001-TABNAME   AND
                          FIELDNAME EQ P_USER_EXIT_001-FIELDNAME.
        ENDSELECT.
        IF SY-SUBRC NE 0.
          PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '016'
                                          TEXT-E10
                                          P_USER_EXIT_001-TABNAME
                                          P_USER_EXIT_001-FIELDNAME
                                          SPACE.
          ADD 1 TO NO_REJECTED.
          PERFORM REJECT_PERNR.
        ENDIF.
      ENDIF.
    ELSEIF
*-----Second permissible way to fill the internal table
               P_USER_EXIT_001-TABNAME   IS INITIAL AND
               P_USER_EXIT_001-FIELDNAME IS INITIAL AND
           NOT P_USER_EXIT_001-LENGTH    IS INITIAL.
    ELSE.
*-----Error in filling the internal table - there exists no other
*     permissible way to fill the internal table
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '016'
                                      TEXT-E11 SPACE SPACE SPACE.
      ADD 1 TO NO_REJECTED.
      PERFORM REJECT_PERNR.
    ENDIF.
*---Check variable argument length against maximal variable arg. length
    VAR_ARG_LENGTH = OFFSET + P_USER_EXIT_001-LENGTH.
    IF VAR_ARG_LENGTH > MAX_VAR_ARG_LENGTH.
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PN' 'E' '016'
                                      TEXT-E12 MAX_VAR_ARG_LENGTH
                                      SPACE SPACE.
      ADD 1 TO NO_REJECTED.
      PERFORM REJECT_PERNR.
    ENDIF.
*---Fill the variable argument
    P_VARIABLE_ARG+OFFSET(P_USER_EXIT_001-LENGTH) =
                                     P_USER_EXIT_001-FIELDVALUE.
*---Adjust the offset
    OFFSET = OFFSET +  P_USER_EXIT_001-LENGTH.
*---Modify the entry
    MODIFY P_USER_EXIT_001.
  ENDLOOP.
ENDFORM.                    " PROCESS_USER_EXIT_001_DATA

*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_DATA
*&---------------------------------------------------------------------*
*       Initialize global parameters and tables
*----------------------------------------------------------------------*
*  -->  text
*  <--  text
*----------------------------------------------------------------------*
FORM INITIALIZE_DATA.                                   " WRH PH4K002071
  REFRESH SORTDATATAB.
  REFRESH P0008_RESULT_TAB.
  REFRESH ERR_TAB.
  CLEAR NO_SCREENS.
  CLEAR NO_REJECTED.
  CLEAR PERNR_COUNT.
  OPEN_SESSION_ONCE = '0'.
  call function 'RP_NUMBER_OF_WAGETYPES_0008'                  "N217573
       importing wt_count = number_of_wagetypes_0008.          "N217573
ENDFORM.                    " INITIALIZE_DATA

*&---------------------------------------------------------------------*
*&      Form  GET_CURRENCY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->  text
*----------------------------------------------------------------------*
FORM get_currency USING    p_molga     LIKE t500l-molga  "WRH AHRK041998
                           p_old_molga LIKE t500l-molga
                           p_trfar     LIKE p0008-trfar
                           p_trfgb     LIKE p0008-trfgb
                           p_trfkz     LIKE t503-trfkz
                           p_old_trfkz LIKE t503-trfkz
                           p_bibegda   LIKE p0008-begda
                           p_old_begda LIKE p0008-begda
                           p_old_waers LIKE p0008-waers
                           p_indbw_flg TYPE c
                  CHANGING p_curr      LIKE p0008-waers
                           p_subrc     LIKE sy-subrc.

  DATA: old_curr LIKE p0008-waers.

  CLEAR p_subrc.
*-Determine currency based on tables T510F and/or T500C
  CALL FUNCTION 'RP_GET_CURRENCY'
       EXPORTING
          molga = p_molga
          trfar = p_trfar
          trfgb = p_trfgb
          trfkz = p_trfkz
          begda = p_bibegda
       IMPORTING
          waers = p_curr
       EXCEPTIONS
          OTHERS.
  IF sy-subrc <> 0.
    IF p_indbw_flg IS INITIAL.
*-----basic pay record contains only directly evaluated wage types
      p_curr = p_old_waers.
    ELSE.
      p_subrc = 1.
    ENDIF.
    EXIT.
  ENDIF.

*-Determine currency based on tables T510F and/or T500C
  CALL FUNCTION 'RP_GET_CURRENCY'
       EXPORTING
*         MOLGA = P_MOLGA                                "WRH AHRK041998
          molga = p_old_molga                            "WRH AHRK041998
          trfar = p_trfar
          trfgb = p_trfgb
*         TRFKZ = P_TRFKZ                                "WRH AHRK041998
          trfkz = p_old_trfkz                            "WRH AHRK041998
          begda = p_old_begda
       IMPORTING
          waers = old_curr
       EXCEPTIONS
          OTHERS.
  IF sy-subrc = 0.
    IF old_curr <> p_old_waers.
*-----currency in T510F/T500C is different from basic pay currency
      IF p_indbw_flg IS INITIAL.
*-------basic pay data contain only directly evaluated wage types
        p_curr = p_old_waers.
      ELSE.
*-------basic pay data contain indirectly evaluated wage types
        IF p_old_waers <> p_curr.
*---------indirectly evaluated wage types not allowed in basic
*         pay record with foreign currency
          p_subrc = 2.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_CURRENCY
