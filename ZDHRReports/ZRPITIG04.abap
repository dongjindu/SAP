*----------------------------------------------------------------------*
*   INCLUDE RPITIG04: Routines used for the batch-input session        *
* 4.6C (LCP)
* QNUL9CK007324 28122000 Note 217573
*---------------------------------------------------------------------*
*       FORM APPEND_BATCH_DATA_TAB                                    *
*---------------------------------------------------------------------*
*       Place batch-input data into internal table BATCH_DATA_TAB.    *
*---------------------------------------------------------------------*
*  -->  Fieldname                                                     *
*  -->  Fieldvalue                                                    *
*---------------------------------------------------------------------*
FORM APPEND_BATCH_DATA_TAB USING FNAM LIKE BDCDATA-FNAM
                                 FVAL TYPE ANY.
  DATA: BATCH_DATA_TAB_WA LIKE BDCDATA.
  BATCH_DATA_TAB_WA-FNAM = FNAM.
  BATCH_DATA_TAB_WA-FVAL = FVAL.
  APPEND BATCH_DATA_TAB_WA TO BATCH_DATA_TAB.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM APPEND_BDCDYNP                                           *
*---------------------------------------------------------------------*
*       Place next dynpro into internal table BATCH_DATA_TAB.         *
*---------------------------------------------------------------------*
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*  -->  DYNBEGIN                                                      *
*---------------------------------------------------------------------*
FORM APPEND_BDCDYNP USING PROGRAM  LIKE BDCDATA-PROGRAM
                          DYNPRO   LIKE BDCDATA-DYNPRO
                          DYNBEGIN LIKE BDCDATA-DYNBEGIN.
  DATA: BATCH_DATA_TAB_WA LIKE BDCDATA.
  BATCH_DATA_TAB_WA-PROGRAM  = PROGRAM.
  BATCH_DATA_TAB_WA-DYNPRO   = DYNPRO.
  BATCH_DATA_TAB_WA-DYNBEGIN = DYNBEGIN.
  APPEND BATCH_DATA_TAB_WA TO BATCH_DATA_TAB.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_INDIRECT_EVALUATION
*&---------------------------------------------------------------------*
*       Perform an indirect evaluation on the specified date and       *
*       return the result in the internal table evaluated_lgart_tab.   *
*----------------------------------------------------------------------*
*  -->  p_pernr             : Personnel number
*  -->  p_infty             : Infotype
*  -->  p_subty             : Subtype
*  -->  p_begda             : Begin date for indirect evaluation
*  -->  p_endda             : End date for indirect evaluation
*  <--  evaluated_lgart_tab : Table containing evaluated wage types
*----------------------------------------------------------------------*
FORM CALL_INDIRECT_EVALUATION USING P_PERNR LIKE PERNR-PERNR
                                    P_INFTY LIKE P0008-INFTY
                                    P_SUBTY LIKE P0008-SUBTY
                                    P_BEGDA LIKE SY-DATUM
                                    P_ENDDA LIKE SY-DATUM.
  CALL FUNCTION 'RP_FILL_WAGE_TYPE_TABLE_EXT'
       EXPORTING
            PERNR                        = P_PERNR
            INFTY                        = P_INFTY
            SUBTY                        = P_SUBTY
            BEGDA                        = P_BEGDA
            ENDDA                        = P_ENDDA
       TABLES
            PP0001                       = P0001  "input
            PP0007                       = P0007  "input
            PP0008                       = P0008  "input
            PPBWLA                       = EVALUATED_LGART_TAB  "output
       EXCEPTIONS
            ERROR_AT_INDIRECT_EVALUATION = 1.
  CASE SY-SUBRC.
    WHEN 0.                          "Okay
    WHEN 1.
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'RP' 'E' SY-MSGNO
                                   SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ADD 1 TO NO_REJECTED.        "Increment number of rejected ees
      PERFORM REJECT_PERNR.       "Process next personnel number
  ENDCASE.
ENDFORM.                               " CALL_INDIRECT_EVALUATION

*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       Create either a Batch-Input session for later processing of    *
*       the data or process the data online via transaction PA30.      *
*----------------------------------------------------------------------*
*  -->  p_proc_type : Processing type (1 = Batch-Input, 2 = Online)
*----------------------------------------------------------------------*
FORM PROCESS_DATA USING P_PROC_TYPE LIKE PROC_TYP.
  DATA: BATCH_LINES TYPE P.

*-Check if table BATCH_DATA_TAB contains any entries
  DESCRIBE TABLE BATCH_DATA_TAB LINES BATCH_LINES.
  CHECK BATCH_LINES GT 0.
*-Check if Batch-Input session is already open
  IF OPEN_SESSION_ONCE EQ NO AND
     P_PROC_TYPE EQ '1'.
*---New Batch-Input session -> add time to session name
*   MAP_NAME+8(4) = SY-UZEIT+0(4).                       "WRH PH0K000464
*---Open Batch-Input session
    CALL FUNCTION 'BDC_OPEN_GROUP'
         EXPORTING
              CLIENT   = SY-MANDT
              GROUP    = MAP_NAME
              USER     = SY-UNAME
              KEEP     = KEEP
              HOLDDATE = HOLDDATE.
*---Session has been opened
    OPEN_SESSION_ONCE = YES.
  ENDIF.
*-Final processing of data according to the processing type
  CASE P_PROC_TYPE.
    WHEN '1'.       "Batch-Input
      CALL FUNCTION 'BDC_INSERT'
           EXPORTING
                TCODE     = 'PA30'
           TABLES
                DYNPROTAB = BATCH_DATA_TAB.
    WHEN '2'.      "Online"
      CALL TRANSACTION 'PA30' USING BATCH_DATA_TAB MODE GEN_MODE.
  ENDCASE.
ENDFORM.                               " PROCESS_DATA

*&---------------------------------------------------------------------*
*&      Form  GEN_BDCDATA_0008
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GEN_BDCDATA_0008
                 USING P_PROC_TYPE LIKE PROC_TYP
                       reclass_date like reclass_date    "WRH L9CK045673
                       P_P0008_SUBTY LIKE P0008-SUBTY.
  DATA: BIDATUM(10) TYPE C,
        DATEN LIKE BDCDATA-FVAL,                         "WRH ALRK054526
        P0008_WA LIKE P0008,                             "WRH ALRK054526
        SUBRC LIKE SY-SUBRC,
        MONTHS_TIL_NEXT_JUMP type i,
        PAR_RECLASS_DATE type D,
        l_incdate(10) type c.

  TAbles : T510U.
*-Read internal table PP0008_TAB                         "WRH ALRK054526
  READ TABLE PP0008_TAB INDEX 1 INTO P0008_WA.           "WRH ALRK054526

* Begin of changes - UD1K922999
* Select Next Increase date
  select single * from T510U  where MOLGA = '10'  and
                               TRFAR = P0008_WA-TRFAR and
                               TRFGB = P0008_WA-TRFGB and
                               TRFKZ = '1'            and
                               TRFGR = P0008_WA-TRFGR and
                               TRFST = P0008_WA-TRFST .

  IF T510U-DAUER GT 0 and T510U-TRFFS ne '05'.
*-----Convert years (first 2 characters in field 'DAUER') to months
    MONTHS_TIL_NEXT_JUMP = T510U-DAUER(2) * 12 + T510U-DAUER+2(2).
*-----Calculate date for next payscale jump by adding months to the
*     begin date of the current Basic Pay record
    PERFORM DAY_PLUS_MONTHS(SAPFP500) USING P0008-STVOR
                                            MONTHS_TIL_NEXT_JUMP
                                            PAR_RECLASS_DATE.
    WRITE PAR_RECLASS_DATE DD/MM/YYYY TO l_INCdate.
  else.
    clear l_incdate.
  endif.
* End of changes -  UD1K922999


* determine Org. Assignment, TRFKZ and MOLGA on BEGDA of old basic pay
  PERFORM RE_0001_TRFKZ_MOLGA USING P0008_WA-BEGDA       "WRH AHRK041998
                                    P0008_WA-BEGDA       "WRH AHRK041998
                                    'X'.                 "WRH AHRK041998
*-Start with an empty Batch-Input session
  REFRESH BATCH_DATA_TAB.
*-Determine if infotype P0000 (events) should be updated during the BI.
* Read table T529A only if EVN_TYP equals '1' (user processes event)
  IF EVN_TYP EQ '1'.
    PERFORM RE_T529A USING TUMMASSN+0(2).
  ELSE.
    CLEAR T529A-P0000.
  ENDIF.
*-Get opening screen of transaction PA30
  PERFORM APPEND_BDCDYNP USING 'SAPMP50A' '1000' 'X'.
*-Fill PA30 opening screen with data
  perform append_batch_data_tab using 'RP50G-PERNR' pernr-pernr.
  WRITE RECLASS_DATE DD/MM/YYYY TO BIDATUM.
  PERFORM APPEND_BATCH_DATA_TAB USING 'RP50G-BEGDA' BIDATUM.
  IF T529A-P0000 EQ 'X'.
*---Process infotype P0000 (Events) first
    perform append_batch_data_tab using 'RP50G-CHOIC' '=0000'.
    perform append_batch_data_tab using 'RP50G-SUBTY' tummassn(2).
    perform append_batch_data_tab using 'BDC_OKCODE' '=INS'.
  ELSE.
*---Process only infotype P0008 (Basic Pay)
    perform append_batch_data_tab using 'RP50G-CHOIC' '=0008'.
    perform append_batch_data_tab using 'RP50G-SUBTY' p_p0008_subty.
    perform append_batch_data_tab using 'BDC_OKCODE' '=COP'.
  ENDIF.
*-Opening screen for transaction PA30 filled - increment screen count
  ADD 1 TO NO_SCREENS.

  IF T529A-P0000 EQ 'X'.
*---Fetch subsequent screen and enter data (infotype P0000)
    PERFORM APPEND_BDCDYNP USING 'MP000000' '2000' 'X'.
    perform append_batch_data_tab using 'P0000-MASSG' tummassn+2(2).
*---Finished with data entry - process OK-code field now
    perform append_batch_data_tab using 'BDC_OKCODE' '=UPD'.
*---Dynpro for event filled - increment screen count
    ADD 1 TO NO_SCREENS.
  ENDIF.

*-Fetch Basic Pay (P0008) dynpro and enter data
* perform fetch_dynnr using '0008' p_p0008_subty         "WRH ALRK044773
  PERFORM FETCH_DYNNR USING STR_0008 P_P0008_SUBTY       "WRH ALRK044773
                   CHANGING SUBRC.
  IF SUBRC GT 0.
*   perform error_mess_for_dynnr using '0008' subrc.     "WRH ALRK044773
    PERFORM ERROR_MESS_FOR_DYNNR USING STR_0008 SUBRC.   "WRH ALRK044773
  ENDIF.
  PERFORM APPEND_BDCDYNP USING 'MP000800' BLD-DYNNR 'X'.
* if p0008-stvor cn '0 ' and                             "WRH ALRK054526
*    p0008-stvor eq reclass_date.                        "WRH ALRK054526
  IF P0008_WA-STVOR CN '0 ' ."AND                          "WRH
**    p0008_wa-stvor EQ reclass_date.    "WRH ALRK054526  "WRH
*    P0008_WA-STVOR LE RECLASS_DATE.                    *PH4K002071
*    PERFORM APPEND_BATCH_DATA_TAB USING 'P0008-STVOR' '!'.
    PERFORM APPEND_BATCH_DATA_TAB USING 'P0008-STVOR' l_incdate.
  ENDIF.
* IF t529a-p0000 EQ ' '.                                 "WRH ALRK054526
  IF T529A-P0000 IS INITIAL.                             "WRH ALRK054526
    PERFORM APPEND_BATCH_DATA_TAB USING 'P0008-BEGDA' BIDATUM.
  ENDIF.
  IF NOT P8_DELIM IS INITIAL.                            "WRH PH4K002071
    WRITE P0008_WA-ENDDA DD/MM/YYYY TO BIDATUM.          "WRH PH4K002071
    PERFORM APPEND_BATCH_DATA_TAB                        "WRH PH4K002071
                   USING 'P0008-ENDDA' BIDATUM.          "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071
  IF NOT ( REASON IS INITIAL ).
    PERFORM APPEND_BATCH_DATA_TAB USING 'P0008-PREAS' REASON.
  ENDIF.
  IF P0008_WA-TRFAR <> OLD_BASIC_PAY_WA-TRFAR.
    PERFORM APPEND_BATCH_DATA_TAB                        "WRH PH4K002071
                   USING 'P0008-TRFAR' P0008_WA-TRFAR.   "WRH PH4K002071
  ENDIF.                                                 "WRH PH4K002071
  IF P0008_WA-TRFGB <> OLD_BASIC_PAY_WA-TRFGB.           "WRH PH4K002071
    PERFORM APPEND_BATCH_DATA_TAB                        "WRH PH4K002071
                   USING 'P0008-TRFGB' P0008_WA-TRFGB.   "WRH PH4K002071
  ENDIF.
  IF T510R-TRFFG <> OLD_BASIC_PAY_WA-TRFGR.              "WRH PH4K002071
    PERFORM APPEND_BATCH_DATA_TAB USING 'P0008-TRFGR' T510R-TRFFG.
  ENDIF.                                                 "WRH PH4K002071
  IF T510R-TRFFS <> OLD_BASIC_PAY_WA-TRFST.              "WRH PH4K002071
    PERFORM APPEND_BATCH_DATA_TAB USING 'P0008-TRFST' T510R-TRFFS.
  ENDIF.                                                 "WRH PH4K002071
* WRITE P0008_WA-BSGRD TO DATEN.        "WRH ALRK054526  "WRH PH0K000464
* CONDENSE DATEN.                       "WRH ALRK054526  "WRH PH0K000464
* PERFORM APPEND_BATCH_DATA_TAB         "WRH ALRK054526  "WRH PH0K000464
*         USING 'P0008-BSGRD' DATEN.    "WRH ALRK054526  "WRH PH0K000464
* WRITE P0008_WA-DIVGV TO DATEN.        "WRH ALRK054526  "WRH PH0K000464
* CONDENSE DATEN.                       "WRH ALRK054526  "WRH PH0K000464
* PERFORM APPEND_BATCH_DATA_TAB         "WRH ALRK054526  "WRH PH0K000464
*         USING 'P0008-DIVGV' DATEN.    "WRH ALRK054526  "WRH PH0K000464
*-Fill currency
  IF P0008_WA-WAERS <> OLD_BASIC_PAY_WA-WAERS.           "WRH AHRK041998
    PERFORM APPEND_BATCH_DATA_TAB                        "WRH AHRK041998
            USING 'P0008-WAERS' P0008_WA-WAERS.          "WRH AHRK041998
  ENDIF.                                                 "WRH AHRK041998
*-Fill wage types
  PERFORM LOAD_WAGE_TYPES_INTO_DYNPRO
                          USING BLD-DYNNR
                                P0008_WA-WAERS.          "WRH AHRK041998
*-Finished with data entry - process OK-code field now
  PERFORM APPEND_BATCH_DATA_TAB USING 'BDC_OKCODE' '=UPD'.
*-Basic Pay dynpro filled - increment screen count
  ADD 1 TO NO_SCREENS.
ENDFORM.                               " GEN_BDCDATA_0008

*&---------------------------------------------------------------------*
*&      Form  ERROR_MESS_FOR_DYNNR
*&---------------------------------------------------------------------*
*       An error occurred while determining the next dynpro number.    *
*       Provide an error message and process the next personnel number.*
*----------------------------------------------------------------------*
*  -->  p_infty : Infotype
*  -->  p_subrc : Error return code
*----------------------------------------------------------------------*
FORM ERROR_MESS_FOR_DYNNR USING P_INFTY LIKE P0008-INFTY
                                P_SUBRC LIKE SY-SUBRC.
  DATA: TABLE_ARG(8).
  CASE P_SUBRC.
    WHEN 1.
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '70' 'E' '102'
                                      P_INFTY TEXT-E05 SPACE SPACE.
      ADD 1 TO NO_REJECTED.           "Increment number of rejected
      PERFORM REJECT_PERNR.            "Process next personnel number

    WHEN 2.
      TABLE_ARG+0(4) = P0001-WERKS.
      TABLE_ARG+4(4) = P0001-BTRTL.
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR '72' 'E' '001'
                                      TABLE_ARG SPACE SPACE SPACE.
      ADD 1 TO NO_REJECTED.           "Increment number of rejected
      PERFORM REJECT_PERNR.            "Process next personnel number

    WHEN 3.
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'P0' 'E' '622'
                                      T588M-ZYKLS ERROR_VARKY
                                      SPACE SPACE.
      ADD 1 TO NO_REJECTED.          "Increment number of rejected ees
      PERFORM REJECT_PERNR.           "Process next personnel number
  ENDCASE.
ENDFORM.                    " ERROR_MESS_FOR_DYNNR

*&---------------------------------------------------------------------*
*&      Form  DET_PAGE_NUMBERS
*&---------------------------------------------------------------------*
*       Determine the loop line numbers for the page breaks between    *
*       page 1 and 2 (variable 'START_PAGE_2') and between pages 2 and *
*       3 (variable 'START_PAGE_3').                                   *
*----------------------------------------------------------------------*
*  -->  p_dynpro_number : Number of the dynpro
*  <--  p_loop_lines    : Number of loop lines in the dynpro
*  <--  p_start_page_2  : Loop line number at which page 2 starts
*  <--  p_start_page_3  : Loop line number at which page 3 starts
*----------------------------------------------------------------------*
FORM DET_PAGE_NUMBERS USING P_DYNPRO_NUMBER LIKE BLD-DYNNR
                   CHANGING P_LOOP_LINES TYPE N
                            P_START_PAGE_2 TYPE N
                            P_START_PAGE_3 TYPE N.
*-Determine number of loop lines in dynpro
  CASE P_DYNPRO_NUMBER.
    WHEN '2000'.
*     p_loop_lines = 6.                                  "WRH AHRK018189
      P_LOOP_LINES = 7.                                  "WRH AHRK018189
    WHEN '2001'.        "OED
*     p_loop_lines = 6.                                  "WRH AHRK018189
      P_LOOP_LINES = 7.                                  "WRH AHRK018189
    WHEN '2004'.
      P_LOOP_LINES = 7.
    WHEN '2010'.        "USA
      P_LOOP_LINES = 7.
    WHEN '2022'.        "Japan
*     p_loop_lines = 6.                                  "WRH AHRK018189
      P_LOOP_LINES = 7.                                  "WRH AHRK018189
  ENDCASE.
*-Determine dynpro pages 2 and 3
  P_START_PAGE_2 = P_LOOP_LINES + 1.
  P_START_PAGE_3 = ( 2 * P_LOOP_LINES ) + 1.
ENDFORM.                    " DET_PAGE_NUMBERS

*&---------------------------------------------------------------------*
*&      Form  LOAD_WAGE_TYPES_INTO_DYNPRO
*&---------------------------------------------------------------------*
*       Move the new Basic Pay wage types into the Basic Pay dynpro    *
*----------------------------------------------------------------------*
*  -->  P_BLD-DYNNR: Number of the Basic Pay dynpro                    *
*----------------------------------------------------------------------*
FORM LOAD_WAGE_TYPES_INTO_DYNPRO
                     USING P_BLD-DYNNR LIKE BLD-DYNNR
                           CURR        LIKE P0008-WAERS. "WRH AHRK041998
  data: wage_type_wa type wage_type_struc,
*       curr like t001-waers,                            "WRH ALRK044773
*       CURR LIKE T500C-WAERS,          "WRH ALRK044773  "WRH AHRK041998
        POSITION,                                        "WRH PH4K002071
        START_PAGE_2(2) TYPE N,
        START_PAGE_3(2) TYPE N,
        LOOP_LINES TYPE N,
        LINE_NR(2) TYPE N,
        PAGE(2) TYPE N,
*-------"FIELD" is assigned the actual field name in the program
        FIELD_NAME LIKE BDCDATA-FNAM VALUE 'Q0008-FIELD(  )',
        FIELD_VALUE LIKE BDCDATA-FVAL,
        REMAIN(2) TYPE N,
        P0008_WA1 LIKE P0008   .

*-Determine currency based on tables T510F and/or T500C
*  CALL FUNCTION 'RP_GET_CURRENCY'                       "WRH AHRK041998
*       EXPORTING                                        "WRH AHRK041998
*         MOLGA = T001P-MOLGA                            "WRH AHRK041998
*         TRFAR = CURRENT_BASIC_PAY_WA-TRFAR             "WRH AHRK041998
*         TRFGB = CURRENT_BASIC_PAY_WA-TRFGB             "WRH AHRK041998
*         TRFKZ = T503-TRFKZ                             "WRH AHRK041998
*         BEGDA = RECLASS_DATE        "WRH PH4K002071    "WRH AHRK041998
*       IMPORTING                                        "WRH AHRK041998
*         WAERS = CURR                                   "WRH AHRK041998
*       EXCEPTIONS                                       "WRH AHRK041998
*         MOLGA_NOT_IN_T001P            = 1              "WRH AHRK041998
*         NO_ENTRY_FOUND_IN_TABLE_T001  = 2              "WRH AHRK041998
*         NO_ENTRY_FOUND_IN_TABLE_T500P = 3              "WRH AHRK041998
*         OTHERS                        = 4.             "WRH AHRK041998
*-Determine loop line numbers for pages 2 and 3 of the Basic Pay dynpro
  PERFORM DET_PAGE_NUMBERS USING P_BLD-DYNNR
                        CHANGING LOOP_LINES
                                 START_PAGE_2
                                 START_PAGE_3.
*-Move contents of new_wage_type_tab into Batch-Input session
  loop at new_wage_type_tab into wage_type_wa.
*---Maximum number of entries reached?
*   IF line_nr GT p0008_line_count OR                    "WRH PH4K002071
*      wage_type_wa-lgart IS INITIAL.                    "WRH PH4K002071
*     EXIT.                                              "WRH PH4K002071
*   ENDIF.                                               "WRH PH4K002071
    POSITION = SY-TABIX.                                 "WRH PH4K002071
*   page = sy-tabix DIV loop_lines.                      "WRH PH4K002071
*   remain = sy-tabix MOD loop_lines.                    "WRH PH4K002071
    PAGE = POSITION DIV LOOP_LINES.                      "WRH PH4K002071
    REMAIN = POSITION MOD LOOP_LINES.                    "WRH PH4K002071
    IF REMAIN GT 0.
      PAGE = PAGE + 1.
    ENDIF.
*---Flip over to next dynpro page?
    IF SY-TABIX EQ START_PAGE_2 OR SY-TABIX EQ START_PAGE_3.
      PERFORM APPEND_BATCH_DATA_TAB USING 'BDC_OKCODE' '=P+'.
      PERFORM APPEND_BDCDYNP USING 'MP000800' P_BLD-DYNNR 'X'.
*-----Increment screen count
      ADD 1 TO NO_SCREENS.
    ENDIF.
*---Determine line number and assign it to the field name
*   line_nr = sy-tabix - ( page * loop_lines ) +         "WRH PH4K002071
*             loop_lines.                                "WRH PH4K002071
    LINE_NR = POSITION - ( PAGE * LOOP_LINES ) +         "WRH PH4K002071
              LOOP_LINES.                                "WRH PH4K002071
    FIELD_NAME+12(2) = LINE_NR.
*---Fill wage type
*---Can default wage type (field LGART) be overwritten (LGMOD = 'O')?
*   - yes  -> add wage type
*   - no   -> skip wage type
*   IF wage_type_wa-lgmod EQ 'O'.                        "WRH PH4K002071
    IF ( NOT WAGE_TYPE_WA-LGMOD IS INITIAL ) OR          "WRH PH4K002071
       ( WAGE_TYPE_WA-LGART IS INITIAL ).                "WRH PH4K002071
      FIELD_NAME+6(5)  = 'LGART'.
      PERFORM APPEND_BATCH_DATA_TAB USING FIELD_NAME
                                          wage_type_wa-lgart.
    ENDIF.

    IF ( WAGE_TYPE_WA-MODNA IS INITIAL ) AND             "WRH PH4K002071
       ( NOT WAGE_TYPE_WA-LGART IS INITIAL ).            "WRH PH4K002071

* Begin of changes - UD1K922999
      read table PP0008_TAB index 1 into P0008_WA1.
      select single * from T510 where MOLGA = '10'   and
                                      TRFAR = P0008_WA1-TRFAR  and
                                      TRFGB = P0008_WA1-TRFGB  and
                                      TRFGR = T510U-TRFFG  and
                                      TRFST = T510U-TRFFS  and
                                      ENDDA  >= reclass_date.
*                                      BEGDA >=  reclass_date and
*                                      ENDDA <= P0008_WA1-ENDDA .
*                                      BEGDA >= P0008_WA1-STVOR.

      if sy-subrc eq 0.
*-----Fill amount field
        FIELD_NAME+6(5)  = 'BETRG'.
*     IF wage_type_wa-indbw = ' '.                       "WRH PH4K002071
*      WRITE WAGE_TYPE_WA-BETRG TO FIELD_VALUE CURRENCY CURR.
        WRITE T510-BETRG TO FIELD_VALUE CURRENCY CURR.
*  End  of changes - UD1K922999
*     ELSE.                                              "WRH PH4K002071
*       WRITE '0' TO field_value CURRENCY curr.          "WRH PH4K002071
*     ENDIF.                                             "WRH PH4K002071
        CONDENSE FIELD_VALUE.
        PERFORM APPEND_BATCH_DATA_TAB USING FIELD_NAME
                                            FIELD_VALUE.
      endif.
    ENDIF.
*---Fill number field
    FIELD_NAME+6(5)  = 'ANZHL'.
    write wage_type_wa-anzhl to field_value.
    CONDENSE FIELD_VALUE.
    PERFORM APPEND_BATCH_DATA_TAB USING FIELD_NAME
                                        FIELD_VALUE.
*---Fill unit field
    FIELD_NAME+6(5)  = 'EITXT'.
    PERFORM APPEND_BATCH_DATA_TAB
                         USING FIELD_NAME
*                              wage_type_wa-zeinh.       "WRH AHRK028021
                               WAGE_TYPE_WA-EIN.         "WRH AHRK028021
  ENDLOOP.
ENDFORM.                    " LOAD_WAGE_TYPES_INTO_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  FILL_OLD_WAGE_TYPE_TAB                                   *
*&---------------------------------------------------------------------*
*       Fill the internal wage type table for the currently valid      *
*       Basic Pay record.                                              *
*----------------------------------------------------------------------*
*      --> VALUE(SUBTY)       : Subtype of infotype P0008              *
*      --> VALUE(MOLGA)       : Country grouping                       *
*      --> VALUE(P8_WA)       : Basic Pay work area                    *
*      <-- OLD_WAGE_TAB  : Currently valid wage type table             *
*----------------------------------------------------------------------*
FORM FILL_OLD_WAGE_TYPE_TAB USING    VALUE(SUBTY) LIKE P0008-SUBTY
*                        VALUE(MOLGA) LIKE T500P-MOLGA   "WRH AHRK041998
*                        value(begda) LIKE sy-datum      "WRH PH4K002071
*                        value(endda) LIKE sy-datum      "WRH PH4K002071
                         VALUE(BEGDA) LIKE SY-DATUM      "WRH AHRK023507
                         VALUE(ENDDA) LIKE SY-DATUM      "WRH AHRK023507
                            CHANGING OLD_WAGE_TAB
                                                LIKE OLD_WAGE_TYPE_TAB.
*LOCAL: p0008.                                           "WRH ALRK054526
  DATA: OLD_WAGE_TYPE_WA      TYPE WAGE_TYPE_STRUC,
        WAGE_TYPE_WA          TYPE WAGE_TYPE_LINE,
        WAGE_TAB_LINE_COUNT   TYPE I,
      P0008_WA              LIKE P0008,                  "WRH ALRK054526
      WAGE_TYPE_INDBW       TYPE WAGE_TYPE_INDBW_TYPE,   "WRH PH4K002071
      SUBRC                 LIKE SY-SUBRC,               "WRH AHRK023507
        PROPOSED_LGART_TAB_WA TYPE PROPOSED_LGART_STRUC.

*-Get the employee's most current Basic Pay record
* rp_provide_from_last p0008 subty begda endda.          "WRH ALRK054526

* Get basic pay record on reclassification date
  PERFORM GET_P0008 USING    SUBTY                       "WRH AHRK023507
                             BEGDA                       "WRH AHRK023507
                             ENDDA                       "WRH AHRK023507
                    CHANGING SUBRC.                      "WRH AHRK023507
  IF SUBRC EQ 0.                                         "WRH AHRK023507
    READ TABLE PP0008_TAB INDEX 1 INTO P0008_WA.         "WRH ALRK054526
  ELSE.                                                  "WRH AHRK023507
    CLEAR P0008_WA.                                      "WRH AHRK023507
  ENDIF.                                                 "WRH AHRK023507

*-Start with empty internal table OLD_WAGE_TAB
  REFRESH OLD_WAGE_TAB.
*-Get IT0001 record
* RP_PROVIDE_FROM_LAST P0001 SPACE      "WRH PH4K002071  "WRH AHRK041998
*                      P0008_WA-BEGDA   "WRH PH4K002071  "WRH AHRK041998
*                      P0008_WA-BEGDA.  "WRH PH4K002071  "WRH AHRK041998

* determine Org. Assignment, TRFKZ and MOLGA
  PERFORM RE_0001_TRFKZ_MOLGA USING P0008_WA-BEGDA       "WRH AHRK041998
                                    P0008_WA-BEGDA       "WRH AHRK041998
                                    'X'.                 "WRH AHRK041998

*-Get default wage types from T539A for current Basic Pay record
* PERFORM re_t539a USING p0008-begda                     "WRH ALRK054526
  PERFORM RE_T539A USING P0008_WA-BEGDA                  "WRH ALRK054526
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

* do p0008_line_count times                              "WRH PH4K002071
* do 20 times                                 "WRH PH4K002071  "N217573
  DO NUMBER_OF_WAGETYPES_0008 TIMES                         "N217573
*    VARYING wage_type_wa FROM p0008-lga01               "WRH ALRK054526
*                         NEXT p0008-lga02.              "WRH ALRK054526
     VARYING WAGE_TYPE_INDBW FROM P0008_WA-IND01         "WRH PH4K002071
                             NEXT P0008_WA-IND02         "WRH PH4K002071
     VARYING WAGE_TYPE_WA FROM P0008_WA-LGA01            "WRH ALRK054526
                          NEXT P0008_WA-LGA02.           "WRH ALRK054526
    IF WAGE_TYPE_WA-LGART NE SPACE AND
       WAGE_TYPE_WA-LGART CN '0 '.
*-----Wage type exists --> start with empty OLD_WAGE_TYPE_WA
      CLEAR OLD_WAGE_TYPE_WA.
      MOVE-CORRESPONDING WAGE_TYPE_WA TO OLD_WAGE_TYPE_WA.
      MOVE-CORRESPONDING WAGE_TYPE_INDBW TO              "WRH AHRK028021
                         OLD_WAGE_TYPE_WA.               "WRH AHRK028021
*-----Get information on wage type
      PERFORM GET_INFO_LGART USING    OLD_WAGE_TYPE_WA-LGART
*                                     p0008-begda        "WRH ALRK054526
                                      P0008_WA-BEGDA     "WRH ALRK054526
*                                     MOLGA              "WRH AHRK041998
                                      T500P-MOLGA        "WRH AHRK041998
                             CHANGING OLD_WAGE_TYPE_WA.
*-----Check if wage type is indirectly evaluated
*     IF old_wage_type_wa-betrg EQ 0 AND                 "WRH PH4K002071
*        old_wage_type_wa-modna NE space.                "WRH ALRK054526
*  NOT old_wage_type_wa-modna IS INITIAL. "WRH ALRK054526"WRH PH4K002071
*       old_wage_type_wa-indbw = 'I'.                    "WRH PH4K002071
*     ELSE.                                              "WRH PH4K002071
*       old_wage_type_wa-indbw = space.                  "WRH PH4K002071
*     ENDIF.                                             "WRH PH4K002071
*-----Check if wage type is indirectly evaluated
      IF WAGE_TYPE_INDBW-INDBW NE 'I'.                   "WRH PH4K002071
        CLEAR OLD_WAGE_TYPE_WA-MODNA.                    "WRH PH4K002071
        CLEAR OLD_WAGE_TYPE_WA-INDBW.                    "WRH PH4K002071
      ENDIF.                                             "WRH PH4K002071
*-----Entry contained in internal table PROPOSED_LGART_TAB ?
      READ TABLE PROPOSED_LGART_TAB
           INTO  PROPOSED_LGART_TAB_WA
           WITH  KEY LGART = OLD_WAGE_TYPE_WA-LGART.
*-----If so, then move information to the work area of the current LGA
      IF SY-SUBRC EQ 0.
        MOVE-CORRESPONDING PROPOSED_LGART_TAB_WA TO OLD_WAGE_TYPE_WA.
      ENDIF.
*-----Append the work area in any case to the internal wage type table
*     APPEND old_wage_type_wa TO old_wage_type_tab.      "WRH ALRK054526
      APPEND OLD_WAGE_TYPE_WA TO OLD_WAGE_TAB.           "WRH ALRK054526
*-----Old wage type has been added
      ADD 1 TO WAGE_TAB_LINE_COUNT.
    ENDIF.
  ENDDO.
*-Set the sequence numbers
  PERFORM SET_SEQNR USING    P0008_LINE_COUNT
                    CHANGING WAGE_TAB_LINE_COUNT
                             OLD_WAGE_TAB.
*-Perform the indirect evaluation of internal table OLD_WAGE_TAB
* PERFORM indbw_lgart USING    p0008-pernr               "WRH ALRK054526
*                              p0008-infty               "WRH ALRK054526
*                              p0008-begda               "WRH ALRK054526
*                              p0008-endda               "WRH ALRK054526
*                              'W'                       "WRH ALRK054526
*                     CHANGING p0008-begda               "WRH ALRK054526
*                              p0008-endda               "WRH ALRK054526
*                              old_wage_tab.             "WRH ALRK054526
  PERFORM INDBW_LGART                                    "WRH ALRK054526
*         using    p0008_wa             "WRH ALRK054526  "WRH AHRK028021
          USING    STR_0008                              "WRH AHRK028021
                   P0008_WA-BEGDA                        "WRH ALRK054526
                   P0008_WA                              "WRH AHRK028021
                   'W'                                   "WRH ALRK054526
          CHANGING OLD_WAGE_TAB.                         "WRH ALRK054526
ENDFORM.                    " FILL_OLD_WAGE_TYPE_TAB

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_LGART                                           *
*&---------------------------------------------------------------------*
*       Get T511-information, wage type text and time units for the    *
*       specified wage type.                                           *
*----------------------------------------------------------------------*
*      -->LGART               : Wage type                              *
*      -->BEGDA               : Begin date                             *
*      -->MOLGA               : Country grouping                       *
*      <--P_OLD_WAGE_TYPE_WA  : Workarea of current P0008 record       *
*----------------------------------------------------------------------*
FORM GET_INFO_LGART USING    VALUE(LGART)       LIKE P0008-LGA01
                             VALUE(BEGDA)       LIKE P0008-BEGDA
                             VALUE(MOLGA)       LIKE T500P-MOLGA
                    CHANGING P_OLD_WAGE_TYPE_WA TYPE WAGE_TYPE_STRUC.
  PERFORM RE511(SAPFP50L) USING MOLGA LGART BEGDA.
  CHECK SY-SUBRC EQ 0.
  MOVE-CORRESPONDING T511 TO P_OLD_WAGE_TYPE_WA.
  SELECT SINGLE * FROM T512T WHERE SPRSL EQ SY-LANGU
                             AND   MOLGA EQ MOLGA
                             AND   LGART EQ LGART.
  IF SY-SUBRC EQ 0.
    MOVE T512T-LGTXT TO P_OLD_WAGE_TYPE_WA-LGTXT.
  ELSE.
    MOVE SPACE TO P_OLD_WAGE_TYPE_WA-LGTXT.
  ENDIF.
  PERFORM GET_EITXT(SAPFP50L) USING    P_OLD_WAGE_TYPE_WA-EIN
                              CHANGING P_OLD_WAGE_TYPE_WA-EITXT.
ENDFORM.                    " GET_INFO_LGART

*&---------------------------------------------------------------------*
*&      Form  SET_SEQNR                                                *
*&---------------------------------------------------------------------*
*       Add the sequence number to the lines in internal wage type     *
*       table WAGE_TYPE_TAB.                                           *
*----------------------------------------------------------------------*
*  -->  VALUE(P_LINE_COUNT)  : Number of lines in table WAGE_TYPE_TAB  *
*  <--  P_WAGE_TYPE_COUNT    : Wage type counter                       *
*  <--  P_WAGE_TYPE_TAB      : Internal wage type table                *
*----------------------------------------------------------------------*
FORM SET_SEQNR USING    VALUE(P_LINE_COUNT) TYPE LINE_COUNT
               CHANGING P_WAGE_TYPE_COUNT   TYPE I
                        P_WAGE_TYPE_TAB     LIKE OLD_WAGE_TYPE_TAB.
  DATA:   SEQNR_STEP(2)   TYPE P VALUE 10,
          TB_SEQNR(3)     TYPE P,
          MAX_TB_SEQNR(3) TYPE P,
        POSITION        LIKE SY-TABIX,                   "WRH PH4K002071
          P_WAGE_TYPE_WA  TYPE WAGE_TYPE_STRUC.
*-Add sequence number to existing lines in table P_WAGE_TYPE_TAB
  LOOP AT P_WAGE_TYPE_TAB INTO P_WAGE_TYPE_WA.
    IF P_WAGE_TYPE_WA-LGART NE SPACE AND                 "WRH PH4K002071
       P_WAGE_TYPE_WA-LGART CN '0 '.                     "WRH PH4K002071
      POSITION = SY-TABIX.                               "WRH PH4K002071
    ENDIF.                                               "WRH PH4K002071
    ADD SEQNR_STEP TO TB_SEQNR.
    UNPACK TB_SEQNR TO P_WAGE_TYPE_WA-SEQNR.
    MODIFY P_WAGE_TYPE_TAB FROM P_WAGE_TYPE_WA.
  ENDLOOP.
  P_WAGE_TYPE_COUNT = POSITION.                          "WRH PH4K002071

*-Determine maximum sequence number
* max_tb_seqnr = p_line_count * seqnr_step.              "WRH PH4K002071

*-Add missing lines in internal table P_WAGE_TYPE_TAB
* DO.                                                    "WRH PH4K002071
*   IF tb_seqnr LT max_tb_seqnr.                         "WRH PH4K002071
*     ADD seqnr_step TO tb_seqnr.                        "WRH PH4K002071
*     CLEAR p_wage_type_wa.                              "WRH PH4K002071
*     UNPACK tb_seqnr TO p_wage_type_wa-seqnr.           "WRH PH4K002071
*     APPEND p_wage_type_wa TO p_wage_type_tab.          "WRH PH4K002071
*     ADD 1 TO p_wage_type_count.                        "WRH PH4K002071
*   ELSE.                                                "WRH PH4K002071
*     EXIT.                                              "WRH PH4K002071
*   ENDIF.                                               "WRH PH4K002071
* ENDDO.                                                 "WRH PH4K002071
ENDFORM.                    " SET_SEQNR

*&---------------------------------------------------------------------*
*&      Form  INDBW_LGART                                              *
*&---------------------------------------------------------------------*
*       Perform the indirect evaluation of the wage types contained in *
*       table WAGE_TYPE_TAB.                                           *
*----------------------------------------------------------------------*
*      -->P_P0008_WA       : Most current Basic Pay record             *
*      -->INDBW_BEG_IN     : Begin date                                *
*      -->INDBW_MSG        : Error message type                        *
*      <--P_WAGE_TYPE_TAB  : Internal wage type table                  *
*----------------------------------------------------------------------*
*FORM indbw_lgart
*     USING    indbw_pernr   LIKE pernr-pernr            "WRH ALRK054526
*              indbw_infty   LIKE p0008-infty            "WRH ALRK054526
*              indbw_beg_in  LIKE sy-datum               "WRH ALRK054526
*              indbw_end_in  LIKE sy-datum               "WRH ALRK054526
*              indbw_msg     LIKE hrerror-msgty          "WRH ALRK054526
*     CHANGING indbw_beg_out LIKE sy-datum               "WRH ALRK054526
*              indbw_end_out LIKE sy-datum               "WRH ALRK054526
*              p_wage_type_tab LIKE old_wage_type_tab.   "WRH ALRK054526
FORM INDBW_LGART
*    using p_p0008_wa like line of p0008 "WRH ALRK054526 "WRH AHRK028021
     USING    INDBW_INFTY      LIKE PSKEY-INFTY          "WRH AHRK028021
              INDBW_BEG_IN     LIKE SY-DATUM             "WRH ALRK054526
              INDBW_INFTY_REC                            "WRH AHRK028021
              VALUE(INDBW_MSG) LIKE HRERROR-MSGTY        "WRH ALRK054526
     CHANGING P_WAGE_TYPE_TAB  LIKE OLD_WAGE_TYPE_TAB.   "WRH ALRK054526

  DATA: INDBW_NEC_SUBRC,
      INFTY_REC_0008 LIKE P0008,                         "WRH AHRK028021
      INFTY_REC_0052 LIKE P0052,                         "WRH AHRK028021
        WAGE_TYPE_WA   TYPE WAGE_TYPE_STRUC.
  DATA: BEGIN OF TBINDBW OCCURS 20,
          SEQNR(3).                        "Sequence number
          INCLUDE STRUCTURE PTBINDBW.
  DATA: END OF TBINDBW.

* MOVE indbw_end_in TO indbw_end_out.                    "WRH ALRK054526
* MOVE indbw_beg_in TO indbw_beg_out.                    "WRH ALRK054526
*-Check if indirect evaluation is necessary
  PERFORM CHECK_INDBW_IS_NECESSARY USING    P_WAGE_TYPE_TAB
                                   CHANGING INDBW_NEC_SUBRC.
  CHECK INDBW_NEC_SUBRC EQ YES.
*-Get ready for indirect evaluation by filling int. table TBINDBW
  REFRESH TBINDBW.
  LOOP AT P_WAGE_TYPE_TAB INTO WAGE_TYPE_WA.
*   CHECK wage_type_wa NE space.                         "WRH ALRK054526
    CHECK NOT WAGE_TYPE_WA IS INITIAL.                   "WRH ALRK054526
    MOVE-CORRESPONDING WAGE_TYPE_WA TO TBINDBW.
    APPEND TBINDBW.
  ENDLOOP.
*-Act according to infotype
* CASE indbw_infty.                                      "WRH ALRK054526
* case p_p0008_wa-infty.                "WRH ALRK054526  "WRH AHRK028021
*   when '0008'.                                         "WRH AHRK028021
  CASE INDBW_INFTY.                                      "WRH AHRK028021
    WHEN STR_0008.                                       "WRH AHRK028021
      INFTY_REC_0008 = INDBW_INFTY_REC.                  "WRH AHRK028021
*-----Fill Basic Pay record
      PERFORM FILL_INFTY_RECORD                          "WRH AHRK028021
                   TABLES   P_WAGE_TYPE_TAB              "WRH AHRK028021
                   USING    STR_0008                     "WRH AHRK028021
                   CHANGING INFTY_REC_0008.              "WRH AHRK028021
*-----Get Infotypes
      RP_PROVIDE_FROM_LAST P0001 SPACE                   "WRH PH4K002071
                           INDBW_BEG_IN INDBW_BEG_IN.    "WRH PH4K002071
      RP_PROVIDE_FROM_LAST P0007 SPACE                   "WRH PH4K002071
                           INDBW_BEG_IN INDBW_BEG_IN.    "WRH PH4K002071
      RP_PROVIDE_FROM_LAST P0230 SPACE                   "WRH PH4K002071
                           INDBW_BEG_IN INDBW_BEG_IN.    "WRH PH4K002071
*-----Determine MOLGA from T500P
      PERFORM RE_T500P USING P0001-WERKS 'X'.            "WRH AHRK041998
*-----Evaluate wage types
      CALL FUNCTION 'RP_EVALUATE_INDIRECTLY_P0008'
           EXPORTING
*              ppernr = indbw_pernr                      "WRH ALRK054526
*              ppernr = p_p0008_wa-pernr "WRH ALRK054526 "WRH AHRK028021
               PPERNR = INFTY_REC_0008-PERNR             "WRH AHRK028021
               PMOLGA = T500P-MOLGA
               PBEGDA = INDBW_BEG_IN
               PP0001 = P0001
               PP0007 = P0007
*              pp0008 = p0008                            "WRH ALRK054526
*              pp0008 = p_p0008_wa      "WRH ALRK054526  "WRH AHRK028021
               PP0008 = INFTY_REC_0008                   "WRH AHRK028021
               PP0230 = P0230
*          IMPORTING                                     "WRH ALRK054526
*              pendda = indbw_end_out                    "WRH ALRK054526
           TABLES
               PTBINDBW = TBINDBW
           EXCEPTIONS
               ERROR_AT_INDIRECT_EVALUATION = 1.
*   when '0052'.                                         "WRH AHRK028021
    WHEN STR_0052.                                       "WRH AHRK028021
      INFTY_REC_0052 = INDBW_INFTY_REC.                  "WRH AHRK028021
*-----TODO: Fill Wage Maintenance record
      PERFORM FILL_INFTY_RECORD                          "WRH AHRK028021
                   TABLES   P_WAGE_TYPE_TAB              "WRH AHRK028021
                   USING    STR_0052                     "WRH AHRK028021
                   CHANGING INFTY_REC_0052.              "WRH AHRK028021
      RP_PROVIDE_FROM_LAST P0001 SPACE                   "WRH PH4K002071
                           INDBW_BEG_IN INDBW_BEG_IN.    "WRH PH4K002071
      RP_PROVIDE_FROM_LAST P0007 SPACE                   "WRH PH4K002071
                           INDBW_BEG_IN INDBW_BEG_IN.    "WRH PH4K002071
*     rp_provide_from_last p0052 space  "WRH PH4K002071  "WRH AHRK028021
      RP_PROVIDE_FROM_LAST P0008 P8_SUBTY                "WRH AHRK028021
                           INDBW_BEG_IN INDBW_BEG_IN.    "WRH PH4K002071
      RP_PROVIDE_FROM_LAST P0237 SPACE                   "WRH PH4K002071
                           INDBW_BEG_IN INDBW_BEG_IN.    "WRH PH4K002071
*-----Determine MOLGA from T500P
      PERFORM RE_T500P USING P0001-WERKS 'X'.            "WRH AHRK041998
*-----Evaluate wage types
      CALL FUNCTION 'RP_EVALUATE_INDIRECTLY_P0052'
           EXPORTING
*              ppernr = indbw_pernr                      "WRH ALRK054526
*              ppernr = p_p0008_wa-pernr "WRH ALRK054526 "WRH AHRK028021
               PPERNR = INFTY_REC_0052-PERNR             "WRH AHRK028021
               PMOLGA = T500P-MOLGA
               PBEGDA = INDBW_BEG_IN
               PP0001 = P0001
               PP0007 = P0007
*              pp0008 = p0008                            "WRH ALRK054526
*              pp0008 = p_p0008_wa      "WRH ALRK054526  "WRH AHRK028021
*              pp0052 = p0052                            "WRH AHRK028021
               PP0008 = P0008                            "WRH AHRK028021
               PP0052 = INFTY_REC_0052                   "WRH AHRK028021
               PP0237 = P0237
*          IMPORTING                                     "WRH ALRK054526
*              pendda = indbw_end_out                    "WRH ALRK054526
           TABLES
               PTBINDBW = TBINDBW
           EXCEPTIONS
               ERROR_AT_INDIRECT_EVALUATION = 1.
  ENDCASE.
*-Now process possible errors
  CASE SY-SUBRC.
    WHEN 0.                          "Okay
    WHEN 1.
      PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'RP' 'E' SY-MSGNO
                                   SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ADD 1 TO NO_REJECTED.        "Increment number of rejected ees
      PERFORM REJECT_PERNR.        "Process next personnel number
  ENDCASE.
*-and add amount and unit to all indirectly evaluated wage types
  LOOP AT P_WAGE_TYPE_TAB INTO WAGE_TYPE_WA WHERE LGART NE SPACE
                                              AND INDBW EQ 'I'.
    READ TABLE TBINDBW WITH KEY LGART = WAGE_TYPE_WA-LGART.
    IF SY-SUBRC = 0.                                     "WRH AHRK028021
      MOVE TBINDBW-BETRG TO WAGE_TYPE_WA-BETRG.
      MOVE TBINDBW-ANZHL TO WAGE_TYPE_WA-ANZHL.
*-----Check wage type amounts/amount in number field against T511-limits
      PERFORM CHECK_LIMIT USING WAGE_TYPE_WA
                                INDBW_MSG.
      MODIFY P_WAGE_TYPE_TAB FROM WAGE_TYPE_WA.
    ENDIF.                                               "WRH AHRK028021
  ENDLOOP.
ENDFORM.                    " INDBW_LGART

*&---------------------------------------------------------------------*
*&      Form  CHECK_INDBW_IS_NECESSARY                                 *
*&---------------------------------------------------------------------*
*       Check if an indirectly evaluated wage type exists in the       *
*       internal wage type table.                                      *
*----------------------------------------------------------------------*
*      <--P_INDBW_NEC_SUBRC  : Flag for indirectly evaluated wage type *
*----------------------------------------------------------------------*
FORM CHECK_INDBW_IS_NECESSARY USING    P_WAGE_TAB LIKE OLD_WAGE_TYPE_TAB
                              CHANGING P_INDBW_NEC_SUBRC.
  DATA: WAGE_TYPE_WA TYPE WAGE_TYPE_STRUC.
  MOVE NO TO P_INDBW_NEC_SUBRC.
  LOOP AT P_WAGE_TAB INTO WAGE_TYPE_WA WHERE LGART NE SPACE
                                         AND INDBW EQ 'I'.
    MOVE YES TO P_INDBW_NEC_SUBRC.
    EXIT.
  ENDLOOP.
ENDFORM.                    " CHECK_INDBW_IS_NECESSARY

*&---------------------------------------------------------------------*
*&      Form  CHECK_LIMIT                                              *
*&---------------------------------------------------------------------*
*       Check the wage type amount and the value in the wage type      *
*       number field against the limits set in table T511.             *
*       In case of an error: Act according to message type             *
*          Message type 'E': Error message and reject personnel number *
*          Message type 'W': Warning message                           *
*----------------------------------------------------------------------*
*      -->VALUE(WAGE_TYPE_WA)  : Work area of wage type                *
*      -->VALUE(MSG_TYPE)      : Type of error message                 *
*----------------------------------------------------------------------*
FORM CHECK_LIMIT USING VALUE(WAGE_TYPE_WA) TYPE WAGE_TYPE_STRUC
                       VALUE(MSG_TYPE)     LIKE HRERROR-MSGTY.
  IF WAGE_TYPE_WA-BTMAX NE 0 AND
     WAGE_TYPE_WA-BETRG GT WAGE_TYPE_WA-BTMAX.
    CASE MSG_TYPE.
      WHEN 'E'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'E' '145'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
        ADD 1 TO NO_REJECTED.
        PERFORM REJECT_PERNR.
      WHEN 'W'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'W' '145'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
    ENDCASE.
  ENDIF.
  IF WAGE_TYPE_WA-BTMIN NE 0 AND
     WAGE_TYPE_WA-BETRG LT WAGE_TYPE_WA-BTMIN.
    CASE MSG_TYPE.
      WHEN 'E'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'E' '146'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
        ADD 1 TO NO_REJECTED.
        PERFORM REJECT_PERNR.
      WHEN 'W'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'W' '146'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
    ENDCASE.
  ENDIF.
  IF WAGE_TYPE_WA-ANMAX NE 0 AND
     WAGE_TYPE_WA-ANZHL GT WAGE_TYPE_WA-ANMAX.
    CASE MSG_TYPE.
      WHEN 'E'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'E' '147'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
        ADD 1 TO NO_REJECTED.
        PERFORM REJECT_PERNR.
      WHEN 'W'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'W' '147'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
    ENDCASE.
  ENDIF.
  IF WAGE_TYPE_WA-ANMIN NE 0 AND
     WAGE_TYPE_WA-ANZHL LT WAGE_TYPE_WA-ANMIN.
    CASE MSG_TYPE.
      WHEN 'E'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'E' '148'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
        ADD 1 TO NO_REJECTED.
        PERFORM REJECT_PERNR.
      WHEN 'W'.
        PERFORM APPEND_ERROR_LIST USING PERNR-PERNR 'PG' 'W' '148'
                                        WAGE_TYPE_WA-LGART SPACE
                                        SPACE SPACE.
    ENDCASE.
  ENDIF.
ENDFORM.                    " CHECK_LIMIT

*&---------------------------------------------------------------------*
*&      Form  FILL_INFTY_RECORD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->   text
*----------------------------------------------------------------------*
FORM FILL_INFTY_RECORD                                   "WRH AHRK028021
          TABLES   P_WAGE_TYPE_TAB LIKE OLD_WAGE_TYPE_TAB
          USING    P_INFTY         LIKE PSKEY-INFTY
          CHANGING P_INFTY_REC.

  DATA: INFTY_REC_0008       LIKE P0008,
        WAGE_TYPE_WA_0008    TYPE WAGE_TYPE_LINE,
        WAGE_TYPE_INDBW_0008 TYPE WAGE_TYPE_INDBW_TYPE,
        LOOP_POS             LIKE SY-TABIX.

  CASE P_INFTY.
    WHEN '0008'.
      INFTY_REC_0008 = P_INFTY_REC.
      LOOP_POS = 1.
*     do 20 times                                              "N217573
      DO NUMBER_OF_WAGETYPES_0008 TIMES                     "N217573
         VARYING WAGE_TYPE_WA_0008    FROM INFTY_REC_0008-LGA01
                                      NEXT INFTY_REC_0008-LGA02
         VARYING WAGE_TYPE_INDBW_0008 FROM INFTY_REC_0008-IND01
                                      NEXT INFTY_REC_0008-IND02.
        CLEAR WAGE_TYPE_WA_0008.
        CLEAR WAGE_TYPE_INDBW_0008.
        LOOP AT P_WAGE_TYPE_TAB FROM  LOOP_POS
                                WHERE NOT LGART IS INITIAL.
          LOOP_POS = SY-TABIX + 1.
          WAGE_TYPE_WA_0008-LGART = P_WAGE_TYPE_TAB-LGART.
          WAGE_TYPE_WA_0008-BETRG = P_WAGE_TYPE_TAB-BETRG.
          WAGE_TYPE_WA_0008-ANZHL = P_WAGE_TYPE_TAB-ANZHL.
          WAGE_TYPE_WA_0008-EIN   = P_WAGE_TYPE_TAB-EIN.
          WAGE_TYPE_WA_0008-OPKEN = P_WAGE_TYPE_TAB-OPKEN.
          IF NOT P_WAGE_TYPE_TAB-MODNA IS INITIAL.
            WAGE_TYPE_INDBW_0008-INDBW = 'I'.
          ENDIF.
          EXIT.
        ENDLOOP.
      ENDDO.
      P_INFTY_REC = INFTY_REC_0008.
  ENDCASE.

ENDFORM.                    " FILL_INFTY_RECORD
