*----------------------------------------------------------------------*
*   INCLUDE RPITIG02: Contains program parameters, select options, and *
*                     'AT SELECTION-SCREEN' processing for RPITIG00.   *
*----------------------------------------------------------------------*
* 4.6A                                                                 *
* DB  AHRK041998 19031999 - Selection screen changed (usability reason)*
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF TABBED BLOCK OWN_PARAMETERS     "DB AHRK041998
                                 FOR 13 LINES.            "DB AHRK041998
  SELECTION-SCREEN TAB (20) SEL_OPT USER-COMMAND SELO     "DB AHRK041998
                           DEFAULT SCREEN 10.             "DB AHRK041998
  SELECTION-SCREEN TAB (20) STE_OPT USER-COMMAND STEO     "DB AHRK041998
                           DEFAULT SCREEN 20.             "DB AHRK041998
  SELECTION-SCREEN TAB (20) BI_PARA USER-COMMAND BIPA     "DB AHRK041998
                           DEFAULT SCREEN 30.             "DB AHRK041998
SELECTION-SCREEN END OF BLOCK OWN_PARAMETERS.             "DB AHRK041998


*-----1. Block: Additional selection criteria
*SELECTION-SCREEN BEGIN OF BLOCK SEL_OPT                  "DB AHRK041998
*                          WITH FRAME TITLE TEXT-SEL.     "DB AHRK041998
SELECTION-SCREEN BEGIN OF screen 10 as subscreen.         "DB AHRK041998
SELECT-OPTIONS: CPIND    FOR P0008-CPIND NO INTERVALS
                                         DEFAULT 'T'.
SELECT-OPTIONS: PS_TYPE  FOR P0008-TRFAR,         "Payscale type
                PS_AREA  FOR P0008-TRFGB,         "Payscale area
                PS_GROUP FOR P0008-TRFGR,         "Payscale group
                PS_LEVEL FOR P0008-TRFST.         "Payscale level
* SELECTION-SCREEN END OF BLOCK SEL_OPT.                  "DB AHRK041998
SELECTION-SCREEN END OF screen 10.                        "DB AHRK041998

*-----2. Block: Critical parameters and org. units
*SELECTION-SCREEN BEGIN OF BLOCK STE_OPT                  "DB AHRK041998
*                          WITH FRAME TITLE TEXT-STE.     "DB AHRK041998
SELECTION-SCREEN BEGIN OF screen 20 as subscreen.         "DB AHRK041998
PARAMETERS: PAST_PS  LIKE RPIXXXXX-KR_FELD1 DEFAULT ' ',
            FUT_PS   LIKE RPIXXXXX-KR_FELD2 DEFAULT ' ',
            P8_DELIM LIKE RPIXXXXX-KR_FELD3 DEFAULT ' '. "WRH PH4K002071
selection-screen skip.                                    "DB AHRK041998
parameters:  REC_DATE LIKE RPIXXXXX-T_DATUM.
SELECT-OPTIONS RCLS_TYP FOR TYPE_PS_RECLASS NO INTERVALS.
selection-screen skip.                                    "DB AHRK041998
*-----Pushbutton group for event/subtype
SELECTION-SCREEN BEGIN OF BLOCK SUB_OPT WITH FRAME TITLE TEXT-STY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON 1(40) EVNT USER-COMMAND EVNT.
PARAMETERS: TUMMASSN LIKE RPIXXXXX-TUMMASSN DEFAULT '1601' MODIF ID TUM.
SELECTION-SCREEN COMMENT 48(25) EVNT_TXT MODIF ID TUM.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON 1(40) STYP USER-COMMAND STYP.
PARAMETERS: P8_SUBTY LIKE P0008-SUBTY DEFAULT '0   ' MODIF ID SUB.
SELECTION-SCREEN COMMENT 48(25) SUBT_TXT MODIF ID SUB.
SELECTION-SCREEN END OF LINE.
PARAMETERS: EVN_TYP(1) DEFAULT 1 NO-DISPLAY. "Event/subtype option
SELECTION-SCREEN END OF BLOCK SUB_OPT.
SELECTION-SCREEN SKIP.
*-----Pushbutton for org. units
SELECTION-SCREEN PUSHBUTTON /3(29) ORGE USER-COMMAND ORGE.
PARAMETERS: SEL_ORG(42) NO-DISPLAY.
*SELECTION-SCREEN END OF BLOCK STE_OPT.                   "DB AHRK041998
SELECTION-SCREEN END OF screen 20.                        "DB AHRK041998

*-----3. Block: Relevant information for Batch-input-session
*SELECTION-SCREEN BEGIN OF BLOCK BI_PARA                  "DB AHRK041998
*                          WITH FRAME TITLE TEXT-BTC.     "DB AHRK041998
SELECTION-SCREEN BEGIN OF screen 30 as subscreen.         "DB AHRK041998
*-----Pushbuttons for selection of processing type
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON 1(25) BTCI USER-COMMAND BTCI.
SELECTION-SCREEN PUSHBUTTON 28(25) ONLN USER-COMMAND ONLN.
SELECTION-SCREEN PUSHBUTTON 55(25) PROT USER-COMMAND PROT.
PARAMETERS: PROC_TYP(1) DEFAULT 1 NO-DISPLAY."Proc. type(BI/online/prot)
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
*-----Additional Batch-Input-Session parameters
PARAMETERS: REASON LIKE P0008-PREAS DEFAULT '02' MODIF ID BON.
*SELECTION-SCREEN COMMENT 37(30) REAS_TXT MODIF ID BON.   "DB AHRK041998
SELECTION-SCREEN COMMENT 41(30) REAS_TXT MODIF ID BON.    "DB AHRK041998
PARAMETERS: MAP_NAME LIKE RPTAXXXX-MAP_NAME DEFAULT SY-REPID
            MODIF ID BTC,
            HOLDDATE LIKE SYST-DATUM DEFAULT '        ' MODIF ID BTC,
            KEEP LIKE RPTXXXXX-KR_FELD5 DEFAULT 'X' MODIF ID BTC.
*-----Radiobuttons to select type of online processing
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: SW_GEN_A LIKE RPTXXXXX-KR_FELD1 RADIOBUTTON GROUP GEN
            MODIF ID NBT.
SELECTION-SCREEN COMMENT 3(27) TEXT-GT1 MODIF ID NBT.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: SW_GEN_N LIKE RPTXXXXX-KR_FELD2 RADIOBUTTON GROUP GEN
            MODIF ID NBT.
SELECTION-SCREEN COMMENT 3(27) TEXT-GT2 MODIF ID NBT.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: SW_GEN_E LIKE RPTXXXXX-KR_FELD3 RADIOBUTTON GROUP GEN
            MODIF ID NBT.
SELECTION-SCREEN COMMENT 3(27) TEXT-GT3 MODIF ID NBT.
SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK BI_PARA.                   "DB AHRK041998
SELECTION-SCREEN END OF screen 30.                        "DB AHRK041998

*---------------------------------------------------------------------*
*                      AT SELECTION-SCREEN                            *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE SSCRFIELDS-UCOMM.
*---Selection of org. units
    WHEN 'ORGE'.
      CALL FUNCTION 'RP_OPTIONS_INTO_STRING'
           EXPORTING
                MAX_CHOSEN_NUMBER       = MAX_SORT
                DELIMITER_SIGN          = '/'
                TEXT_TITLE              = TEXT-OR0
                TEXT_LEFT               = 'Organisationseinheiten'(OR1)
                TEXT_RIGHT              = 'ausgewählt'(OR2)
                STATUS                  = 'ORDER'
           IMPORTING
                RETURN_CODE             = RETCD
           TABLES
                TEXT_SYMBOL_RELATION_TAB   = SEL_TAB
           CHANGING
                STRING_VALUE               = SEL_ORG
           EXCEPTIONS
                TABLE_STRING_INCONSISTENCY = 01
                UNKNOWN_STATUS             = 02.
*---Process data via Batch-Input
    WHEN 'BTCI'.
      PROC_TYP = '1'.
      WRITE ICON_CHECKED AS ICON TO BTCI+0(4).
      MOVE '    ' TO ONLN+0(4).
      MOVE '    ' TO PROT+0(4).
*---Process data online (via transaction PA30)
    WHEN 'ONLN'.
      PROC_TYP = '2'.
      WRITE ICON_CHECKED AS ICON TO ONLN+0(4).
      MOVE '    ' TO BTCI+0(4).
      MOVE '    ' TO PROT+0(4).
*---Provide list of proposed payscale reclassifications
    WHEN 'PROT'.
      PROC_TYP = '3'.
      WRITE ICON_CHECKED AS ICON TO PROT+0(4).
      MOVE '    ' TO BTCI+0(4).
      MOVE '    ' TO ONLN+0(4).
*---Prepare user data entry in event field
    WHEN 'EVNT'.
      EVN_TYP = '1'.
      WRITE ICON_CHECKED AS ICON TO EVNT+0(4).
      MOVE '    ' TO STYP+0(4).
*---Prepare user data entry in subtype field
    WHEN 'STYP'.
      EVN_TYP = '2'.
      WRITE ICON_CHECKED AS ICON TO STYP+0(4).
      MOVE '    ' TO EVNT+0(4).
  ENDCASE.

*---------------------------------------------------------------------*
*                      AT SELECTION-SCREEN OUTPUT                     *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
*-Org. units
  WRITE 'Organisationseinheiten'(OR0) TO ORGE+4(25).
  IF SEL_ORG NE SPACE.
    WRITE ICON_DISPLAY_MORE AS ICON TO ORGE+0(4).
  ELSE.
    WRITE ICON_ENTER_MORE AS ICON TO ORGE+0(4).
  ENDIF.

*-Preset ICON_CHECKED on pushbuttons
  CASE EVN_TYP.
    WHEN '1'.
      WRITE ICON_CHECKED AS ICON TO EVNT+0(4).
      MOVE '    ' TO STYP+0(4).
    WHEN '2'.
      WRITE ICON_CHECKED AS ICON TO STYP+0(4).
      MOVE '    ' TO EVNT+0(4).
  ENDCASE.
  CASE PROC_TYP.
    WHEN '1'.
      WRITE ICON_CHECKED AS ICON TO BTCI+0(4).
      MOVE '    ' TO ONLN+0(4).
      MOVE '    ' TO PROT+0(4).
    WHEN '2'.
      WRITE ICON_CHECKED AS ICON TO ONLN+0(4).
      MOVE '    ' TO BTCI+0(4).
      MOVE '    ' TO PROT+0(4).
    WHEN '3'.
      WRITE ICON_CHECKED AS ICON TO PROT+0(4).
      MOVE '    ' TO BTCI+0(4).
      MOVE '    ' TO ONLN+0(4).
  ENDCASE.

*-Check for activated event radiobutton
  IF EVNT+0(4) EQ ICON_CHECKED.
    LOOP AT SCREEN.
*-----Event-related information ready for data entry
      IF SCREEN-GROUP1 EQ 'TUM'.
        SCREEN-INPUT     = '1'.
        SCREEN-INVISIBLE = '0'.
        MODIFY SCREEN.
      ENDIF.
*-----Subtype-related information will be suppressed
      IF SCREEN-GROUP1 EQ 'SUB'.
        SCREEN-INPUT     = '0'.
        SCREEN-INVISIBLE = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
*---Subtype radiobutton has been activated
    LOOP AT SCREEN.
*-----Event-related information will be suppressed
      IF SCREEN-GROUP1 EQ 'TUM'.
        SCREEN-INPUT     = '0'.
        SCREEN-INVISIBLE = '1'.
        MODIFY SCREEN.
      ENDIF.
*-----Subtype-related information ready for data entry
      IF SCREEN-GROUP1 EQ 'SUB'.
        SCREEN-INPUT     = '1'.
        SCREEN-INVISIBLE = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-Type of processing (protocol/online/Batch-Input)
  CASE PROC_TYP.
    WHEN 1.
*-----Create a Batch-Input session
      LOOP AT SCREEN.
*-------Batch-Input-Session-related information
        IF SCREEN-GROUP1 EQ 'BTC'.
          SCREEN-INPUT     = '1'.
          SCREEN-INVISIBLE = '0'.
          MODIFY SCREEN.
        ENDIF.
*-------Online-related information
        IF SCREEN-GROUP1 EQ 'NBT'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
          MODIFY SCREEN.
        ENDIF.
*-------Batch-Input and online related information
        IF SCREEN-GROUP1 EQ 'BON'.
          SCREEN-INPUT     = '1'.
          SCREEN-INVISIBLE = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN 2.
*-----Process the data online
      LOOP AT SCREEN.
*-------Batch-Input-Session-related information
        IF SCREEN-GROUP1 EQ 'BTC'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
          MODIFY SCREEN.
        ENDIF.
*-------Online-related information
        IF SCREEN-GROUP1 EQ 'NBT'.
          SCREEN-INPUT     = '1'.
          SCREEN-INVISIBLE = '0'.
          MODIFY SCREEN.
        ENDIF.
*-------Batch-Input and online related information
        IF SCREEN-GROUP1 EQ 'BON'.
          SCREEN-INPUT     = '1'.
          SCREEN-INVISIBLE = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN 3.
*-----Write out only a list of proposed payscale reclassifications
      LOOP AT SCREEN.
*-------Batch-Input-Session-related information
        IF SCREEN-GROUP1 EQ 'BTC'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
          MODIFY SCREEN.
        ENDIF.
*-------Online-related information
        IF SCREEN-GROUP1 EQ 'NBT'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
          MODIFY SCREEN.
        ENDIF.
*-------Batch-Input and online related information
        IF SCREEN-GROUP1 EQ 'BON'.
          SCREEN-INPUT     = '0'.
          SCREEN-INVISIBLE = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.

*-Determine texts on report selection screen
*-Subtype
  PERFORM RE_T591S USING SY-LANGU
*                        '0008'                          "WRH PH4K002071
                         STR_0008                        "WRH PH4K002071
                         P8_SUBTY.
  MOVE T591S-STEXT TO SUBT_TXT.
*-Events
  PERFORM RE_T530T USING SY-LANGU
                         TUMMASSN+0(2)
                         TUMMASSN+2(2).
  IF NOT T530T-MGTXT IS INITIAL.
    MOVE T530T-MGTXT TO EVNT_TXT.
  ELSE.
    PERFORM RE_T529T USING SY-LANGU
                           TUMMASSN+0(2).
    MOVE T529T-MNTXT TO EVNT_TXT.
  ENDIF.
*-Reason
  PERFORM RE_T530F USING SY-LANGU
*                        '0008'                          "WRH PH4K002071
                         STR_0008                        "WRH PH4K002071
                         REASON.
  MOVE T530F-RTEXT TO REAS_TXT.

*---------------------------------------------------------------------*
*            AT SELECTION-SCREEN ON VALUE-REQUEST FOR ...             *
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P8_SUBTY.
* PERFORM show_subtype USING '0008'.                     "WRH PH4K002071
  PERFORM SHOW_SUBTYPE USING STR_0008.                   "WRH PH4K002071

AT SELECTION-SCREEN ON VALUE-REQUEST FOR REASON.
* PERFORM show_reason USING '0008'.                      "WRH PH4K002071
  PERFORM SHOW_REASON USING STR_0008.                    "WRH PH4K002071

AT SELECTION-SCREEN ON VALUE-REQUEST FOR TUMMASSN.
  PERFORM SHOW_EVENT.
