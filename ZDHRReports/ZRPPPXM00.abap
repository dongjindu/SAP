*
*LCP XYVN0388630 03Mar2001 unicode
*4.6C
*QICAHRK053949 020899 authorization check for PCL4
*4.0A
* YKMP40K011038 17.09.1997 For importing results on an international
*                          basis, it is important to deactivate
*                          the authority check since the cluster id
*                          in the import statement will be switched
*                          to '$$' for example. This assumes that the
*                          calling program did the check already.
*3.0E
*QICP30K074825 170596 authority-check for simulation mode
*3.0
*LLB P30K067762 - 16.04.96 - Canadian payroll
*QICK11K126112 160294 extended syntax check
*QICK11K094643 150794 run time error during import
*-----------------------------------------------------------------------
* R/3 modules handling the PCL1(2)-buffer
*-----------------------------------------------------------------------
FORM PCL1_EXP_IMP USING OPERATION.
   SAVE-SGART = 'P1'.
   PCL1-AEDTM = SY-DATUM.
   PCL1-UNAME = SY-UNAME.
   PCL1-PGMID = SY-REPID.
   TBUFF = PCL1.
   TBUFF-SGART = SAVE-SGART.
   PERFORM EXP_IMP USING OPERATION.
   PCL1 = TBUFF.
   SY-SUBRC = MAIN-SUBRC.             "this must be the last statement
ENDFORM.  "EXP_IMP_P1

FORM PCL2_EXP_IMP USING OPERATION.
   SAVE-SGART = 'P2'.
   PCL2-AEDTM = SY-DATUM.
   PCL2-UNAME = SY-UNAME.
   PCL2-PGMID = SY-REPID.
   TBUFF = PCL2.
   TBUFF-SGART = SAVE-SGART.
   PERFORM EXP_IMP USING OPERATION.
   PCL2 = TBUFF.
   SY-SUBRC = MAIN-SUBRC.             "this must be the last statement
ENDFORM.  "EXP_IMP_P2

FORM PCL3_EXP_IMP USING OPERATION.    "applicants
   SAVE-SGART = 'P3'.
   PCL3-AEDTM = SY-DATUM.
   PCL3-UNAME = SY-UNAME.
   PCL3-PGMID = SY-REPID.
   TBUFF = PCL3.
   TBUFF-SGART = SAVE-SGART.
   PERFORM EXP_IMP USING OPERATION.
   PCL3 = TBUFF.
   SY-SUBRC = MAIN-SUBRC."this has to be the last statem.
ENDFORM.  "EXP_IMP_P3

FORM PCL4_EXP_IMP USING OPERATION.
   SAVE-SGART = 'P4'.
   PCL4-AEDTM = SY-DATUM.
   PCL4-UNAME = SY-UNAME.
   PCL4-PGMID = SY-REPID.
   TBUFF = PCL4.
   TBUFF-SGART = SAVE-SGART.
   PERFORM EXP_IMP USING OPERATION.
   PCL4 = TBUFF.
   SY-SUBRC = MAIN-SUBRC."this has to be the last statem.
ENDFORM.  "EXP_IMP_P3
*-----------------------------------------------------------------------
* common part dealing with TBUFF-header
*-----------------------------------------------------------------------
FORM EXP_IMP USING OPERATION.

   IF TBUFF-SRTF2 EQ 0.
     PERFORM AUTHORITY_CHECK USING OPERATION.             "QICK11K126112
   ENDIF.                                                 "QICK11K094643
   CHECK MAIN-SUBRC NE 12.
   DIR_KEY-SGART = TBUFF-SGART.
   DIR_KEY+2 = TBUFF.
   READ TABLE BUFFER_DIR WITH KEY DIR_KEY BINARY SEARCH.
   READ-DIR-SUBRC = SY-SUBRC.
*  sy-tabix is not reliable in case of sy-subrc = 8.
   DIR-TABIX = SY-TABIX.
*  ENDIF.                                            (del) QICK11K094643

   CASE OPERATION.
     WHEN 'READ'.
       IF READ-DIR-SUBRC EQ 0.
         IF BUFFER_DIR-NTABX NE 0.
           PERFORM READ_TBUFF.           "data from buffer
         ELSE.
           READ-SUBRC = 4.               "no record found entry
         ENDIF.
       ELSE.
         IF SEQ-INPUT EQ SPACE.
*          data from disk are selected if available
*          and stored in the buffer
           PERFORM READ_PCLX.
         ELSE.
           READ-SUBRC = 4.
         ENDIF.
       ENDIF.
       MAIN-SUBRC = READ-SUBRC.
*      READ-SUBRC = 0 indicates that the TBUFF-work-area has succesfully
*      been filled with the requested data. Otherwise READ-SUBRC = 4.
     WHEN 'UPDATE'.
       APPEND TBUFF.
       TBUFF-TABIX = SY-TABIX.
       PERFORM MAINTAIN_DIR.
       MAIN-SUBRC = 0.
*    WHEN 'INSERT'. only updates are being performed
   ENDCASE.

ENDFORM.   "EXP_IMP


FORM READ_TBUFF.
* IF TBUFF+2(49) NE PCLX(49).
    PPPPP = BUFFER_DIR-NTABX + TBUFF-SRTF2.
    READ TABLE TBUFF INDEX PPPPP.
*   IF SY-SUBRC NE 0.
*     internal error: the directory does not fit to the buffer
*   ENDIF.
* ENDIF.
  READ-SUBRC = 0.
ENDFORM.    "READ_TBUFF

FORM READ_PCLX.  "selects data from disk and stores them in the buffer
*XYVN0388630
*  DATA: BEGIN OF T, "dummy for abnormal end of program   "QICK11K094643
*          P TYPE P,                                      "QICK11K094643
*        END OF T.                                        "QICK11K094643
*  IF TBUFF-SRTF2 NE 0. "should not occur                 "QICK11K094643
*    T = SPACE.         "force abnormal end               "QICK11K094643
*    ADD 1 TO T-P.                                        "QICK11K094643
*  ENDIF. "TBUFF-SRTF2 NE 0.                              "QICK11K094643
*XYVN0388630
  IF TBUFF-SRTF2 <> 0. "should not occur                   "XYVN0388630
    MESSAGE X016(RP) WITH SPACE SPACE SPACE SPACE.         "XYVN0388630
  ENDIF. "TBUFF-SRTF2 NE 0.                                "XYVN0388630
  BUFFER_DIR(47) = DIR_KEY.
* AUX_SRTF2 = TBUFF-SRTF2.                           (del) QICK11K094643
  CASE TBUFF-SGART.
    WHEN 'P1'.
*     SELECT SINGLE * FROM PCL1                      (del) QICK11K094643
      SELECT * FROM PCL1                                  "QICK11K094643
             WHERE RELID EQ BUFFER_DIR-RELID
             AND SRTFD EQ BUFFER_DIR-SRTFD
*            AND SRTF2 EQ AUX_SRTF2.                 (del) QICK11K094643
             ORDER BY PRIMARY KEY.                        "QICK11K094643
        TBUFF = PCL1.
        TBUFF-SGART = SAVE-SGART.
        APPEND TBUFF.                                     "QICK11K094643
        IF TBUFF-SRTF2 EQ 0.                              "QICK11K094643
          TBUFF-TABIX = SY-TABIX.                         "QICK11K094643
        ENDIF. "TBUFF-SRTF2 EQ 0.                         "QICK11K094643
      ENDSELECT. "* FROM PCL1                             "QICK11K094643
    WHEN 'P2'.
*     SELECT SINGLE * FROM PCL2                      (del) QICK11K094643
      SELECT * FROM PCL2                                  "QICK11K094643
             WHERE RELID EQ BUFFER_DIR-RELID
             AND SRTFD EQ BUFFER_DIR-SRTFD
*            AND SRTF2 EQ AUX_SRTF2.                 (del) QICK11K094643
             ORDER BY PRIMARY KEY.                        "QICK11K094643
        TBUFF = PCL2.
        TBUFF-SGART = SAVE-SGART.
        APPEND TBUFF.                                     "QICK11K094643
        IF TBUFF-SRTF2 EQ 0.                              "QICK11K094643
          TBUFF-TABIX = SY-TABIX.                         "QICK11K094643
        ENDIF. "TBUFF-SRTF2 EQ 0.                         "QICK11K094643
      ENDSELECT. "* FROM PCL2                             "QICK11K094643
    WHEN 'P3'.
      SELECT * FROM PCL3                                  "QICK11K094643
             WHERE RELID EQ BUFFER_DIR-RELID
             AND SRTFD EQ BUFFER_DIR-SRTFD
             ORDER BY PRIMARY KEY.                        "QICK11K094643
      TBUFF = PCL3.
      TBUFF-SGART = SAVE-SGART.
      APPEND TBUFF.                                       "QICK11K094643
      IF TBUFF-SRTF2 EQ 0.                                "QICK11K094643
        TBUFF-TABIX = SY-TABIX.                           "QICK11K094643
      ENDIF. "TBUFF-SRTF2 EQ 0.                           "QICK11K094643
    ENDSELECT. "* FROM PCL3                               "QICK11K094643
    WHEN 'P4'.
      SELECT * FROM PCL4                                  "QICK11K094643
             WHERE RELID EQ BUFFER_DIR-RELID
             AND SRTFD EQ BUFFER_DIR-SRTFD
             ORDER BY PRIMARY KEY.
      TBUFF = PCL4.
      TBUFF-SGART = SAVE-SGART.
      APPEND TBUFF.
      IF TBUFF-SRTF2 EQ 0.
        TBUFF-TABIX = SY-TABIX.
      ENDIF. "TBUFF-SRTF2 EQ 0.
    ENDSELECT. "* FROM PCL4
  ENDCASE.
  READ-PCLX-SUBRC = SY-SUBRC.
  IF READ-PCLX-SUBRC EQ 0.
*   APPEND TBUFF.                                    (del) QICK11K094643
*   TBUFF-TABIX = SY-TABIX.                          (del) QICK11K094643
    BUFFER_DIR-NNUXT = TBUFF-SRTF2.
    PERFORM READ_DB_SUB1.
    READ TABLE TBUFF INDEX TBUFF-TABIX.                   "QICK11K094643
  ELSE.
    PERFORM READ_DB_SUB2.              "no record found.
  ENDIF.
  READ-SUBRC = READ-PCLX-SUBRC.
ENDFORM. "READ_PCLX

FORM READ_DB_SUB1.
* IF AUX_SRTF2 EQ 0.                                 (del) QICK11K094643
  BUFFER_DIR-NTABX = TBUFF-TABIX.    "location of the record
  BUFFER_DIR-OTABX = 0.
  BUFFER_DIR-ONUXT = 0.
  PERFORM MOD_DIR.
* ELSE.                                              (del) QICK11K094643
*   MODIFY BUFFER_DIR INDEX DIR-TABIX.               (del) QICK11K094643
* ENDIF.                                             (del) QICK11K094643
ENDFORM. "READ_DB_SUB1

FORM READ_DB_SUB2.
* IF AUX_SRTF2 EQ 0.                                 (del) QICK11K094643
  BUFFER_DIR-NTABX = 0.
  BUFFER_DIR-NNUXT = 0.
* no before-image:
  BUFFER_DIR-OTABX = '77777-'.
  BUFFER_DIR-ONUXT = 0.
  PERFORM MOD_DIR.
* ELSE.                                              (del) QICK11K094643
*   internal error:  handling of extents
* ENDIF.                                             (del) QICK11K094643
ENDFORM. "READ_DB_SUB2


FORM MOD_DIR.
* to be performed only in case of SRTF2 = 0
  CASE READ-DIR-SUBRC.
    WHEN 8. APPEND BUFFER_DIR. DIR-TABIX = SY-TABIX.
    WHEN 4. INSERT BUFFER_DIR INDEX DIR-TABIX.
*   when 0. internal error in directory-maintainance
  ENDCASE.
ENDFORM. "MOD_DIR


FORM MAINTAIN_DIR.    "directory-maintainance when updating the buffer
   IF TBUFF-SRTF2 EQ 0.
     AUX_ONUXT = BUFFER_DIR-NNUXT.
   ENDIF.
   BUFFER_DIR-NNUXT = TBUFF-SRTF2.
   IF TBUFF-SRTF2 EQ 0.
     CASE READ-DIR-SUBRC.
       WHEN 0. "update
         IF BUFFER_DIR-OTABX EQ 0.
           BUFFER_DIR-OTABX = - BUFFER_DIR-NTABX.
           BUFFER_DIR-ONUXT = AUX_ONUXT.
         ENDIF.
         BUFFER_DIR-NTABX = TBUFF-TABIX.
         MODIFY BUFFER_DIR INDEX DIR-TABIX.
       WHEN 4. "insert
         PERFORM ASSIGN_DIR.
         INSERT BUFFER_DIR INDEX DIR-TABIX.
       WHEN 8. "append
         PERFORM ASSIGN_DIR.
         APPEND BUFFER_DIR.
         DIR-TABIX = SY-TABIX.
     ENDCASE.
   ELSE.
     MODIFY BUFFER_DIR INDEX DIR-TABIX.
   ENDIF.
ENDFORM.  "MAINTAIN_DIR


FORM ASSIGN_DIR.   "assign header-values in case of INSERT or APPEND
   BUFFER_DIR(47) = DIR_KEY.
   BUFFER_DIR-NTABX = TBUFF-TABIX.
*  no before-image:
   BUFFER_DIR-OTABX = '77777-'.
   BUFFER_DIR-ONUXT = 0.
ENDFORM. "ASSIGN_DIR




FORM PREPARE_UPDATE USING
            VERBUCHUNG   TYPE C.
*ORM PREPARE_UPDATE USING VERBUCHUNG.
*-----------------------------------------------------------------------
* BEFORE_IMAGE_PCLX and (after-image) TBUFF are prepared
* for delivery to APLZ
*-----------------------------------------------------------------------
* NTABX EQ 0 <==> "no record found" AND "no update"
* OTABX EQ 0 <==> "record found" AND "no update"
*-----------------------------------------------------------------------

  DEL-COUNTER = 0.
  LOOP AT TBUFF.
    TBUFF-TABIX = SY-TABIX.
    DIR_KEY-SGART = TBUFF-SGART.
    DIR_KEY+2 = TBUFF.
    PERFORM LOOP_VALUES.
    IF TBUFF-TABIX GE OTABX-LOW AND TBUFF-TABIX LE OTABX-HIGH.
*     before-image
*     append BEFORE_IMAGE_PCLX and delete entry from TBUFF
*     maintain directory-entry OTABX
      PERFORM FUNCTION_BEFORE_IMAGE.
    ELSE.
      IF TBUFF-TABIX GE NTABX-LOW AND TBUFF-TABIX LE NTABX-HIGH
                                  AND AUX_OTABX NE 0.
*       after-image
*       do nothing else than maintaining directory-entry NTABX
        PERFORM FUNCTION_AFTER_IMAGE.
      ELSE.
*       neither before-image nor after-image means rubbish
        ADD 1 TO DEL-COUNTER.
        DELETE TBUFF.
      ENDIF.
    ENDIF.
  ENDLOOP.   "TBUFF

* delete unused directory-entries
* NTABX = 0 and OTABX = 0, respectively, indicate that no update
* has been made
  LOOP AT BUFFER_DIR WHERE NTABX EQ 0 OR OTABX EQ 0.
    DELETE BUFFER_DIR.
  ENDLOOP.

* final update of the database
  CASE VERBUCHUNG.
    WHEN 'P'. "payroll?
    WHEN 'V'. "update program
      PERFORM UPDATE_DATA(RPPPXV00).
      RP-INIT-BUFFER.
  ENDCASE.

ENDFORM.  "prepare_update


FORM LOOP_VALUES.
  IF TBUFF-SRTF2 EQ 0.
    READ TABLE BUFFER_DIR WITH KEY DIR_KEY BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      DIR-TABIX = SY-TABIX.
      AUX_NTABX = BUFFER_DIR-NTABX.
      IF BUFFER_DIR-OTABX LE 0.
        AUX_OTABX = - BUFFER_DIR-OTABX.
      ELSE.
        AUX_OTABX = '77777-'.
      ENDIF.
    ELSE.
      CLEAR BUFFER_DIR.
      CLEAR AUX_OTABX.
      CLEAR AUX_NTABX.
    ENDIF.
  ENDIF.
  OTABX-LOW  = AUX_OTABX - DEL-COUNTER.
  OTABX-HIGH = OTABX-LOW + BUFFER_DIR-ONUXT.
  NTABX-LOW  = AUX_NTABX - DEL-COUNTER.
  NTABX-HIGH = NTABX-LOW + BUFFER_DIR-NNUXT.
ENDFORM. "LOOP_VALUES


FORM FUNCTION_BEFORE_IMAGE.
  BEFORE_IMAGE_PCLX = TBUFF.
  APPEND BEFORE_IMAGE_PCLX.               " <------/
  IF TBUFF-SRTF2 EQ 0.                    "       /  (sy-tabix)
    BUFFER_DIR-OTABX = SY-TABIX.          " <----/
    MODIFY BUFFER_DIR INDEX DIR-TABIX.
  ENDIF.
  ADD 1 TO DEL-COUNTER.
  DELETE TBUFF.
ENDFORM. "before-image

FORM FUNCTION_AFTER_IMAGE.
  IF TBUFF-SRTF2 EQ 0.
    BUFFER_DIR-NTABX = TBUFF-TABIX.
    MODIFY BUFFER_DIR INDEX DIR-TABIX.
  ENDIF.
ENDFORM. "after-image

FORM AUTHORITY_CHECK USING OPERATION.      "QICK11K126112
* data: authc(1).                     (del) QICP30K074825 "QICK11K126112
* authc = operation.                  (del) QICP30K074825 "QICK11K126112

  IF OPERATION = 'READ' AND                               "YKMP40K011038
     NO_AUTHORITY_CHECK_CLUSTER = 'X'.                    "YKMP40K011038
    MAIN-SUBRC = 0.                                       "YKMP40K011038
    EXIT.                                                 "YKMP40K011038
  ENDIF.                                                  "YKMP40K011038

  IF OPERATION EQ 'READ'.                                 "QICP30K074825
    AUTHORITY-CHECK OBJECT 'P_PCLX'
      ID 'RELID' FIELD TBUFF-RELID
      ID 'AUTHC' FIELD 'R'.                               "QICP30K074825
  ELSE. "operation eq 'UPDATE'                            "QICP30K074825
*   CHECK SAVE-SGART NE 'P4'.          (del) QICAHRK053949 QICP30K074825
    if save-sgart ne 'P4'.                                "QICAHRK053949
      AUTHORITY-CHECK OBJECT 'P_PCLX'                     "QICK11K074825
        ID 'RELID' FIELD TBUFF-RELID                      "QICK11K074825
        ID 'AUTHC' FIELD 'U'.                             "QICP30K074825
      IF SY-SUBRC NE 0.                                   "QICP30K074825
        PCLX_SUBRC = 12.                                  "QICP30K074825
*       second chance for simulation user                 "QICP30K074825
        AUTHORITY-CHECK OBJECT 'P_PCLX'                   "QICK11K074825
          ID 'RELID' FIELD TBUFF-RELID                    "QICK11K074825
          ID 'AUTHC' FIELD 'S'.                           "QICP30K074825
      ENDIF.                                              "QICP30K074825
    else.                                                 "QICAHRK053949
      main-subrc = 0.                                     "QICAHRK053949
    endif.                                                "QICAHRK053949
  ENDIF.                                                  "QICP30K074825
  IF SY-SUBRC EQ 0.
    MAIN-SUBRC = 0.
  ELSE.
    MAIN-SUBRC = 12.
  ENDIF. "SY-SUBRC EQ 0.

ENDFORM. "AUTHORITY_CHECK USING OPERATION
