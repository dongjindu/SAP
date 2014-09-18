REPORT RFKQSU40 MESSAGE-ID FR

                LINE-SIZE 132
                NO STANDARD PAGE HEADING.

INCLUDE ZINKQSU00.
*INCLUDE INKQSU00.

INITIALIZATION.

  FISCAL = SY-DATUM.
  FISCAL = FISCAL - 1.

  LIFNR-OPTION = 'EQ'.
  LIFNR-SIGN   = 'I'.

  PERFORM FILE_CREATE USING FILE_ERROR.

  SELECT * FROM T005 WHERE ADDRS EQ '004'
                     AND   LAND1 LIKE 'US_'.
    IF COUNTRY = SPACE.
      COUNTRY = T005-LAND1.
    ENDIF.
  ENDSELECT.

  IF SY-SUBRC NE 0.
    SELECT * FROM T005 WHERE ADDRS = '004'.
    ENDSELECT.
    IF SY-SUBRC EQ 0.
      COUNTRY = T005-LAND1.
    ELSE.
      COUNTRY = 'US '.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON BUKRS.
*----------------------------------- either old or new WT functionality
  KD_BUKRS[] = BUKRS[].
  PERFORM CHECK_COMPANY_CODES.

AT SELECTION-SCREEN ON WITHT.

  PERFORM CHECK_WT_TYPES.

*Note 497879 begin
at selection-screen on ext_wtcd.

  if ext_wtcd ne space  or
     cls_wtcd ne space.
    perform check_wt_code.
  endif.
*Note 497879 end
*eject
START-OF-SELECTION.

  COMMIT WORK.                                "PERFORMANCE INORMIX

  IF NOT PRNTFORM IS INITIAL.
    PERFORM FORM_INIT USING PRNTFORM.  "Form can modify PRNTFORM
  ENDIF.

  IF NOT PRNTFORM IS INITIAL AND TESTPRNT > '0'.
    PERFORM FORM_TEST USING TESTPRNT COUNTRY.
  ENDIF.

  IF NOT CREATFIL IS INITIAL.
    PERFORM FILE_INIT USING FILE_ERROR.
  ENDIF.

  IF DETAILRP > 0.
    PAGETYPE = DETAILRP.
  ENDIF.

  PERFORM CREATE_AGENTTAB.

  IF NOT RECO1042 IS INITIAL.
    PERFORM CREATE_FORM1042.
  ENDIF.


  PERFORM CREATE_KTAB. "note 826457

  PERFORM CREATE_T059.

*  PERFORM CREATE_KTAB.  "note 826457        "Creates also RANGES LIFNR

  PERFORM CREATE_RTAB USING FISCAL.

  SORT RTAB.
*note 510748 begin
  delete rtab where qsshh = 0.
*note 510748 end

*eject
END-OF-SELECTION.

  LOOP AT RTAB.
* Note 454478 begin
*    D_X = SY-TABIX MOD 2.
     D_X = 0.
* Note 454478 end
    AT NEW BUKRS.
       SELECT SINGLE * FROM T001
         WHERE BUKRS EQ RTAB-BUKRS.

      PERFORM GET_BUKRS USING RTAB-BUKRS.

      IF DETAILRP NE 0.
        NEW-PAGE WITH-TITLE.
      ENDIF.

      IF NOT PRNTFORM IS INITIAL.
        PERFORM FORM_INIT_AGENT.
      ENDIF.

      IF NOT CREATFIL IS INITIAL.
        PERFORM FILE_INIT_AGENT.
      ENDIF.
    ENDAT.
*-----------------------------------------------------------------

    AT NEW LIFNR.

      PERFORM GET_LIFNR USING RTAB-BUKRS RTAB-LIFNR.

      PERFORM PRINT_AT_LIFNR.

      IF NOT CREATFIL IS INITIAL.
         PERFORM FILE_INIT_RECIPIENT.
      ENDIF.
     ENDAT.

*eject-------------------------------------------------------------
*-- all RTAB lines ------------------------------------------------


PERFORM FIND_X059 USING RTAB-BUKRS RTAB-LIFNR RTAB-QSSKZ.

PERFORM PRINT_DETAIL.

*Note 497879 begin
    if not prntform is initial . "and d_x = 1.
      perform form_at_lifnr.
    endif.
*Note 497879 end

IF NOT PRNTFORM IS INITIAL.
PERFORM FORM_MAIN.
ENDIF.

IF NOT CREATFIL IS INITIAL.
PERFORM FILE_WRITE_Q.
ENDIF.

AT END OF LIFNR.

SUM.

BSAK-DMB21  = RTAB-QSSHH.
BSAK-DMB31  = RTAB-QBSHH.
BSAK-DMB22  = RTAB-ALLOW.
BSAK-DMB23  = RTAB-QSSHH - RTAB-ALLOW.

ADD 1 TO AGENTTAB-COUNT.
ADD 1 TO REC_COUNT.

PERFORM PRINT_END_LIFNR.

ENDAT.

AT END OF BUKRS.

SUM.

IF NOT CREATFIL IS INITIAL AND FILE_ERROR = 0.
PERFORM FILE_END_BUKRS.
ENDIF.

PERFORM PRINT_END_BUKRS.

MODIFY AGENTTAB INDEX AGENTTAB_IDX.

NEW-LINE.

ENDAT.

*Note 497879 begin
     if not prntform is initial and d_x = 0.
       perform form_end_lifnr.
     endif.
*Note 497879 end
ENDLOOP.


IF NOT PRNTFORM IS INITIAL.
PERFORM FORM_AT_LAST.
ENDIF.

IF NOT CREATFIL IS INITIAL.
PERFORM FILE_AT_LAST.
ENDIF.
* Note 454478 begin
* Download the file to PC.
*  if not creatfil is initial.
*    perform download_file_to_pc tables 1042_rec.
*  endif.
* Note 454478 end
IF NOT RECO1042 IS INITIAL.
PERFORM PRINT_1042.
ENDIF.

PERFORM PRINT_LOG.


TOP-OF-PAGE.
  PERFORM PRINT_TOP USING PAGETYPE.

* Forms to handle SAPSCIPT ------------------------------------------
INCLUDE ZINKQSU01.
*  INCLUDE INKQSU01.
* Forms to handle File Output ---------------------------------------
INCLUDE ZINKQSU02.
*INCLUDE INKQSU02 .
* Forms to handle miscellaneous print output ------------------------
INCLUDE ZINKQSU03.
*INCLUDE INKQSU03 .
* Other Forms -------------------------------------------------------
INCLUDE ZINKQSU04.
*INCLUDE INKQSU04.

*----------------------------------------------------------------------*
*    Include for extended functionality
*----------------------------------------------------------------------*

INCLUDE ZFIWT0010.
* INCLUDE FIWT0010.
*&---------------------------------------------------------------------*
*&      Form  CHECK_WT_CODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form check_wt_code.
  if cls_wtcd ne space.               " Old WT functionality
    select * from t059q where qsskz in cls_wtcd.
    endselect.
    if sy-subrc ne '0'.
      set cursor field 'classical_wtcode'.
      message e151(fr).
    endif.
  endif.

  if ext_wtcd ne space.                 " New WT functionality
    if witht ne space.
      select * from t059z  where witht     in witht and
                                 wt_withcd in ext_wtcd.
      endselect.
    else.
      select * from t059z  where wt_withcd in ext_wtcd.
      endselect.
    endif.
  endif.
endform.                    " CHECK_WT_CODE
