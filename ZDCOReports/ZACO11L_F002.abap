*----------------------------------------------------------------------*
***INCLUDE ZACO11L_F002 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_ML_MAIN
*&---------------------------------------------------------------------*
*       Read data from Material Ledger Tables
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ML_MAIN.
* Read - MLCD
* PERFORM READ_DATA_FR_MLCD.
* Read ML Tables (Buffers)
  PERFORM READ_ML_BUF_TABLES.

ENDFORM.                    " READ_ML_MAIN

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_MLCD
*&---------------------------------------------------------------------*
*       Read - MLCD
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_FR_MLCD.
  CLEAR : IT_MLCD, IT_MLCD[].
* Index - ZD1
  SELECT *  INTO  TABLE IT_MLCD
            FROM  MLCD
*            FOR ALL ENTRIES IN IT_MATNR
           WHERE
*                KALNR = IT_MATNR-KALNR
                 BDATJ = P_BDATJ
             AND POPER = P_POPER
             AND CURTP = P_CURTP.
  CLEAR IT_MLCD.

  SORT IT_MLCD BY KALNR.

ENDFORM.                    " READ_DATA_FR_MLCD

*&---------------------------------------------------------------------*
*&      Form  READ_ML_BUF_TABLES
*&---------------------------------------------------------------------*
*       Read ML Tables (Buffers)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ML_BUF_TABLES.

* local data definition.
* Clear
  CLEAR : IT_ZTCO_MATLEDGER, IT_ZTCO_MATLEDGER[].

** Selection
* For Space (MVT gr.)
  APPEND INITIAL LINE TO IT_CKMLMV010.

* Make Multi-processing logic up to use Dig. Work processes
  DATA : LV_TOTDIV LIKE SY-TABIX.
  DATA : IT_PAR_BEWARTGRP LIKE TABLE OF CKMLMV010
                          WITH HEADER LINE.
  DATA : LV_CNT      TYPE I.
  DATA : LV_TAB_IND  TYPE SY-TABIX.
  DATA : LV_PRO_NO   TYPE I.
  DATA : LV_TOTALCNT LIKE SY-TABIX.

  DESCRIBE TABLE IT_CKMLMV010 LINES LV_TOTALCNT.
  LV_TOTDIV = LV_TOTALCNT / P_PRONO.

  CLEAR : RCV_JOBS , SND_JOBS, LV_PRO_NO.

  LOOP AT IT_CKMLMV010  .
    LV_TAB_IND = LV_TAB_IND + 1.
    LV_CNT = LV_TAB_IND  MOD LV_TOTDIV.

    MOVE-CORRESPONDING IT_CKMLMV010 TO IT_PAR_BEWARTGRP.
    APPEND IT_PAR_BEWARTGRP.
    CLEAR  IT_PAR_BEWARTGRP.

    IF LV_CNT = 0  OR LV_TAB_IND >= LV_TOTALCNT.
      LV_PRO_NO = LV_PRO_NO + 1.
      CALL FUNCTION 'Z_FCO_READ_ZVCO_MLXXV_BY_MVTGR'
           STARTING NEW TASK  LV_PRO_NO
           DESTINATION IN GROUP DEFAULT
           PERFORMING RETURN_INFO ON END OF TASK
           EXPORTING
                IM_BDATJ              = P_BDATJ
                IM_POPER              = P_POPER
                IM_CURTP              = P_CURTP
           TABLES
                IT_UNI_ZTCO_MATLEDGER = IT_L_TMP
                IT_PAR_BEWARTGRP      = IT_PAR_BEWARTGRP
           EXCEPTIONS
                COMMUNICATION_FAILURE = 1
                SYSTEM_FAILURE        = 2
                RESOURCE_FAILURE      = 3.
      IF SY-SUBRC = 0.
        SND_JOBS = SND_JOBS + 1.
      ELSE.
* Handling of communication and system failure
        MESSAGE E066 WITH LV_PRO_NO.
      ENDIF.
      CLEAR   : IT_PAR_BEWARTGRP, IT_PAR_BEWARTGRP[].
    ENDIF.
  ENDLOOP.

* Receiving results
  WAIT UNTIL RCV_JOBS >= SND_JOBS.

  CLEAR IT_ZTCO_MATLEDGER.

* Check Initial Unit / Curr
* Some data can not have UOM.
  LOOP AT IT_ZTCO_MATLEDGER WHERE MEINS IS INITIAL.
    CLEAR MARA.
    SELECT SINGLE * FROM MARA
                    WHERE MATNR = IT_ZTCO_MATLEDGER-MATNR .
    IT_ZTCO_MATLEDGER-MEINS = MARA-MEINS.
    MODIFY IT_ZTCO_MATLEDGER.
  ENDLOOP.

* Collect Final DATA
  DATA : IT_L_TMP_MAT LIKE STANDARD TABLE OF IT_ZTCO_MATLEDGER
                      WITH HEADER LINE .

  IT_L_TMP_MAT[] = IT_ZTCO_MATLEDGER[].

  CLEAR : IT_ZTCO_MATLEDGER, IT_ZTCO_MATLEDGER[].

  LOOP AT IT_L_TMP_MAT.
    MOVE-CORRESPONDING IT_L_TMP_MAT TO IT_ZTCO_MATLEDGER.
    COLLECT IT_ZTCO_MATLEDGER.
    CLEAR : IT_L_TMP_MAT,
            IT_ZTCO_MATLEDGER.
  ENDLOOP.

  CLEAR : IT_L_TMP_MAT, IT_L_TMP_MAT[].
  FREE  : IT_L_TMP_MAT, IT_L_TMP_MAT[].



** -- test check logic
*  DESCRIBE TABLE  IT_ZTCO_MATLEDGER LINES SY-TFILL.
*  WRITE : SY-TFILL.
*
*  DATA : BEGIN OF ITAB OCCURS 0,
*          LBKUM LIKE ZVCO_MLXXV-LBKUM,
*          MEINS LIKE ZVCO_MLXXV-MEINS,
*          SALK3 LIKE ZVCO_MLXXV-SALK3,
*          WAERS LIKE ZVCO_MLXXV-WAERS,
*         END OF  ITAB.
*
*  LOOP AT IT_ZTCO_MATLEDGER.
*    MOVE-CORRESPONDING IT_ZTCO_MATLEDGER TO ITAB.
*    COLLECT ITAB.
*    CLEAR ITAB.
*  ENDLOOP.
*
*  CLEAR ITAB.


*  CLEAR : RCV_JOBS , SND_JOBS.
*
*    CALL FUNCTION 'Z_FCO_READ_ZVCO_MLXXV_BY_MVTGR'
*         STARTING NEW TASK  IT_CKMLMV010-MLBWG
*         DESTINATION IN GROUP DEFAULT
*         PERFORMING RETURN_INFO ON END OF TASK
*         EXPORTING
*              IM_BDATJ              = P_BDATJ
*              IM_POPER              = P_POPER
*              IM_CURTP              = P_CURTP
*         TABLES
*              IT_UNI_ZTCO_MATLEDGER = IT_L_TMP
*              IT_PAR_BEWARTGRP      =
*         EXCEPTIONS
*              COMMUNICATION_FAILURE = 1
*              SYSTEM_FAILURE        = 2
*              RESOURCE_FAILURE      = 3.
*
*    IF SY-SUBRC = 0.
*      SND_JOBS = SND_JOBS + 1.
*    ELSE.
** Handling of communication and system failure
*      MESSAGE E066 WITH IT_CKMLMV010-MLBWG.
*    ENDIF.
*
** Receiving results
*  WAIT UNTIL RCV_JOBS >= SND_JOBS.
*
*  CLEAR IT_ZTCO_MATLEDGER.
*
ENDFORM.                    " READ_ML_BUF_TABLES

*---------------------------------------------------------------------*
*       FORM RETURN_INFO                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TASKNAME                                                      *
*---------------------------------------------------------------------*
FORM RETURN_INFO USING TASKNAME.

  DATA  : IT_L_TMP LIKE TABLE OF ZTCO_MATLEDGER.

  RECEIVE RESULTS FROM FUNCTION 'Z_FCO_READ_ZVCO_MLXXV_BY_MVTGR'
    IMPORTING IT_UNI_ZTCO_MATLEDGER = IT_L_TMP
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1
      SYSTEM_FAILURE        = 2.

  RCV_JOBS = RCV_JOBS + 1.  "Receiving data

  IF SY-SUBRC = 0.
    APPEND LINES OF IT_L_TMP TO IT_ZTCO_MATLEDGER.
  ELSE.
* Handling of communication and system failure
    MESSAGE E066 WITH TASKNAME.
  ENDIF.
ENDFORM.

*
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_MATLEDGER
*&---------------------------------------------------------------------*
*       Update
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_ZTCO_MATLEDGER.

  CLEAR IT_ZTCO_MATLEDGER.

* Delete data already exist
  DELETE FROM ZTCO_MATLEDGER
        WHERE
              BDATJ                     = P_BDATJ
          AND POPER                     = P_POPER
          AND CURTP                     = P_CURTP.
* Do not check subrc. Let system check.
  COMMIT WORK AND WAIT.

* insertion
  SORT IT_ZTCO_MATLEDGER BY KALNR
                            BDATJ
                            POPER
                            CURTP
                            KATEGORIE
                            BEWARTGRP.
  LOOP AT IT_ZTCO_MATLEDGER.
* LOG
    IT_ZTCO_MATLEDGER-ERDAT = SY-DATUM.
    IT_ZTCO_MATLEDGER-ERZET = SY-UZEIT.
    IT_ZTCO_MATLEDGER-ERNAM = SY-UNAME.
    INSERT INTO ZTCO_MATLEDGER VALUES IT_ZTCO_MATLEDGER.
    IF SY-SUBRC <> 0.
      MESSAGE E030 WITH 'Insert'.
    ENDIF.
    CLEAR IT_ZTCO_MATLEDGER.
  ENDLOOP.

* Success
  MESSAGE S000 WITH 'Data were successfully created' .

ENDFORM.                    "UPDATE_ZTCO_MATLEDGER
