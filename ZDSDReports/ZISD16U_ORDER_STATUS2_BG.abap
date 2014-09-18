************************************************************************
* Author                 : jun ho choi
* Creation Date          : 2003-11-17
* Specifications By      :
* Development Request No : UD1K907438
* Pattern                : 5-2
* Addl documentation     :
* Description            : order status report for HMA/HAC/GLOVIS
*
*
*
* Modification Log
* Date        Developer    Request ID   Description
* 10/25/2004  Shiva        UD1K912648  Formatted the file to place the
*                                      fields in correct position.
* 05/17/2005    chris      UD1K916079  Change the data selection logic
*                                      to pick up the unshipped previous
*                                      vehicles to show their status.
*
**06/30/2005    Furong                 Change check month to forward and
*                                      back. Check vehicles # 'D' or 'S'
* 07/13/2005    Shiva      UD1K916818  Remove the scrap vehicle info.
*                                      Limit to a month before and after
*                                      the current month.
* 07/14/2005    Shiva      UD1K916852  Remove the scrap vehicle info/.
*                                   and get all the previous month info.
* 07/25/2005    Shiva      UD1K917001  Remove the scrap vehicle info/.
* 08/15/2005    Shiva      UD1K917317  Don't remove WO info. from header
*                                even if it has the scrap vehicle info/.
*& 08/16/2005   Shiva      UD1K917327  As per Kevin we don't need to
*                       check equality of modify qty and sequenced qty.
*& 08/25/2005   Shiva      UD1K917420  Remove the present validation for
*                                      "IT_WOSUM" and check only the
*                                       modqty and rp15tq equality.
* 04/08/2007    Manju      UD1K940275   Change Reporting point from
*                                       Rp07 to Rp17.
* 06/05/2007    Furong     UD1K940759  Nation code as selection criteria
*                                      to get the data; can specify
*                                      only NOT shipped Vin or all
************************************************************************
                  REPORT ZISD16U_ORDER_STATUS2 NO STANDARD PAGE HEADING
                                                        MESSAGE-ID ZMSD.


*
                  TABLES : ZTPP_WOSUM,
                           CABN,
                           AUSP.


*
                  DATA : BEGIN OF IT_WOSUM OCCURS 0.
                          INCLUDE STRUCTURE ZTPP_WOSUM.
                  DATA : END OF IT_WOSUM.

                  DATA : BEGIN OF IT_DOWNFILE OCCURS 0,
                          RECORD(190),
                          END OF IT_DOWNFILE.

                  DATA : BEGIN OF IT_DOWNFILE_HMA OCCURS 0,
                              RECORD(190),
                              END OF IT_DOWNFILE_HMA.

                  DATA : BEGIN OF IT_DOWNFILE_HAC OCCURS 0,
                              RECORD(190),
                              END OF IT_DOWNFILE_HAC.

                  DATA : BEGIN OF IT_WOSUM_TEMP OCCURS 0.
                          INCLUDE STRUCTURE ZTPP_WOSUM.
                  DATA : END OF IT_WOSUM_TEMP.

                  DATA : BEGIN OF IT_DOWNFILE_TEMP OCCURS 0,
                               RECORD(200),
                               END OF IT_DOWNFILE_TEMP.

                  DATA : BEGIN OF IT_DOWNFILE_HMA_TEMP OCCURS 0,
                                   RECORD(200),
                                   END OF IT_DOWNFILE_HMA_TEMP.

                  DATA : BEGIN OF IT_DOWNFILE_HAC_TEMP OCCURS 0,
                                   RECORD(200),
                                   END OF IT_DOWNFILE_HAC_TEMP.

                  DATA : BEGIN OF S_WO_SER OCCURS 0,
                         SIGN(1),
                         OPTION(2),
                         LOW LIKE ZTPP_WOSUM-WO_SER,
                         HIGH LIKE ZTPP_WOSUM-WO_SER,
                         END OF S_WO_SER.

                  DATA : BEGIN OF S_ATINN OCCURS 0,
                         SIGN(1),
                         OPTION(2),
                         LOW LIKE AUSP-ATINN,
                         HIGH LIKE AUSP-ATINN,
                         END OF S_ATINN.

                  DATA : BEGIN OF IT_CABN OCCURS 0,
                         ATINN LIKE CABN-ATINN,
                         ATNAM LIKE CABN-ATNAM,
                         END OF IT_CABN.

                  DATA : W_CNT TYPE I,
                         W_N_9(9) TYPE N,
                         W_N_8(8) TYPE N.
                  DATA:  P_DATE   LIKE T093B-ABGJA.
                  DATA:  P_MNUM TYPE I.
                  DATA : " W_DSN_B(20) VALUE '/usr/sap/EDI_SAP/',
                                    W_DSN(90).

                  DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD16_02'.

                  DATA : SND_JOBS TYPE I VALUE 1,
                         RCV_JOBS TYPE I VALUE 1,
                         EXCP_FLAG(1),
                    W_TASKNAME(4) TYPE N VALUE '0001',
   W_CLASSNAME LIKE RZLLITAB-CLASSNAME VALUE 'PG_STS'.

*
           SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
      PARAMETERS : P_DATE1 LIKE SY-DATUM DEFAULT SY-DATUM. " NO-DISPLAY.
         SELECT-OPTIONS: S_NATION FOR ZTPP_WOSUM-NATION. " NO INTERVALS.
                  PARAMETERS: P_FLAG TYPE FLAG.
                  PARAMETERS: P_HACPO TYPE FLAG.
                  SELECTION-SCREEN END OF BLOCK B1.
          SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.

                  PARAMETERS: P_GLOVIS TYPE FLAG.
                  PARAMETERS: P_HMA TYPE FLAG.
                  PARAMETERS: P_HAC TYPE FLAG.
                  SELECTION-SCREEN END OF BLOCK B2.

*
                  START-OF-SELECTION.
                    PERFORM GET_DATA.
                    PERFORM PROCESS_DATA.


*
                  END-OF-SELECTION.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
                  FORM GET_DATA.

                    DATA: P_DATE2 LIKE SY-DATUM.

* IMPORT p_date FROM DATABASE indx(zs) ID variant.
                    CALL FUNCTION 'SG_PS_ADD_MONTH_TO_DATE'
                         EXPORTING
                              MONTHS  = 1
                              OLDDATE = P_DATE1
                         IMPORTING
                              NEWDATE = P_DATE2.

                    P_DATE = P_DATE2+2(4).

*  if r_back = 'X'.
*    p_mnum = - p_month.
*  else.
*    p_mnum = p_month.
*  endif.
                    PERFORM INIT_DATE.

                    SELECT *
                           INTO TABLE IT_WOSUM
                           FROM ZTPP_WOSUM
                     WHERE WO_SER IN S_WO_SER AND   ".
** CHANGED BY FUORNG ON 06/05/07 TRANSPORT #: UD1K940759
                                NATION IN S_NATION AND
                                ( DEALER = 'AA' OR
                                DEALER = 'AB' ).

** REQUESTED BY CATHERINE S. CHANGED BY CHRIS LI
*                                ( NATION = 'B28' OR
*                                NATION = 'B06' ) AND
*                                ( DEALER = 'AA' OR
*                                DEALER = 'AB' ).
** END OF CHANGE ON 12/10/2004.
*
* REQUESTED BY CATHERINE CHANGED BY CHRIS
* CURRENT LOGIC THROW OUT ALL PREVIOUS MONTH ORDERS IF ALL QUANTITY HAS
* BEEN SEQUENSED. BUT FOR SOME CARS MAY NOT BE SHIPPED EVEN THE ORDER IS
* FROM PREVIOUS MONTH. SO WE NEED TO CHECK IF ALL CARS HAS BEEN SHIPPED
* BEFORE WE THROW OUT THE ORDER. AND ALSO WE SHOULD NOT SHOW THE SHIPPED
* CAR'S STATUS.

*                    PERFORM CHECK_VSTATUS .

*                    LOOP AT it_wosum.
*                      IF it_wosum-wo_ser+1(4) < p_date     AND
*                         it_wosum-modqty = it_wosum-seqqty and
*                         it_wosum-seqqty  = it_wosum-rp15tq.
*                        DELETE it_wosum INDEX sy-tabix.
*                      ENDIF.
*                    ENDLOOP.
*&----As per Kevin's request, check mod qty = rp15tq. ---shiva 082505.
                    LOOP AT IT_WOSUM
                         WHERE WO_SER+1(4) < P_DATE.
                      IF IT_WOSUM-MODQTY = IT_WOSUM-RP15TQ.
                        DELETE IT_WOSUM INDEX SY-TABIX.
                      ENDIF.
                    ENDLOOP.

                  ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  INIT_DATE
*&---------------------------------------------------------------------*
                  FORM INIT_DATE.
                    S_WO_SER-SIGN = 'I'.
                    S_WO_SER-OPTION = 'BT'.
                    CONCATENATE 'E' '0000' '000' INTO S_WO_SER-LOW.
                    CONCATENATE 'E' P_DATE 'ZZZ' INTO S_WO_SER-HIGH.
                    APPEND S_WO_SER.

                    CONCATENATE 'D' '0000' '000' INTO S_WO_SER-LOW.
                    CONCATENATE 'D' P_DATE 'ZZZ' INTO S_WO_SER-HIGH.
                    APPEND S_WO_SER.

* VEHICLE MASTER
                    S_ATINN-SIGN = 'I'. S_ATINN-OPTION = 'EQ'.
                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                               WHERE ATNAM EQ 'P_FLEET'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                        WHERE ATNAM EQ 'P_MANUAL_ORDER'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                                 WHERE ATNAM EQ 'P_VIN'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                           WHERE ATNAM EQ 'P_ENGINE_NO'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                              WHERE ATNAM EQ 'P_KEY_NO'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                       WHERE ATNAM EQ 'P_SEQUENCE_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP01_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP02_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.
* Begin of changes - UD1K940275
*                SELECT SINGLE atinn atnam INTO (cabn-atinn, cabn-atnam)
*                                                              FROM cabn
*                                    WHERE atnam EQ 'P_RP07_ACTUAL_DATE'
*.
*                    s_atinn-low = cabn-atinn. APPEND s_atinn.
*                    it_cabn-atinn = cabn-atinn.
*                    it_cabn-atnam = cabn-atnam.
*                    APPEND it_cabn.
                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP17_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.
* End of changes - UD1K940275
                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP18_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP19_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
** changed by Furong on 24/06/05
*        WHERE atnam EQ 'P_RP21_ACTUAL_DATE'.
                                    WHERE ATNAM EQ 'P_RP20_ACTUAL_DATE'.
** end of change
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

** Changed by Fuong on 06/11/07 for summary ship_in, Requested by Daniel
               SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP24_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP26_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.
** end of change 06/11/07

**
                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP25_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                    WHERE ATNAM EQ 'P_RP27_ACTUAL_DATE'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

* REQUESTED BY CATHERINE S. CHANGED BY CHRIS
                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                           WHERE ATNAM EQ 'P_COLOR_SER'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

* END OF CHANGE ON 02/24/2005
*&------Added by shiva to remove scrapped cars.
                SELECT SINGLE ATINN ATNAM INTO (CABN-ATINN, CABN-ATNAM)
                                                              FROM CABN
                                           WHERE ATNAM EQ 'P_USAGE_CAR'.
                    S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
                    IT_CABN-ATINN = CABN-ATINN.
                    IT_CABN-ATNAM = CABN-ATNAM.
                    APPEND IT_CABN.

                  ENDFORM.                    " INIT_DATE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
                  FORM PROCESS_DATA.
                    DESCRIBE TABLE IT_WOSUM LINES W_CNT.
                    IF W_CNT = 0.
                      MESSAGE I000 WITH TEXT-M01.
                      STOP.
                    ENDIF.

                    SORT IT_WOSUM BY NATION DEALER WO_SER EXTC INTC.
                    REFRESH IT_DOWNFILE. CLEAR IT_DOWNFILE.
                    LOOP AT IT_WOSUM.
                      CLEAR W_N_9.

                      PERFORM JOB_PARALLEL.
                    ENDLOOP.

                 WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '100' SECONDS. "?

                    PERFORM DOWNLOAD_FILE.
                  ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  JOB_PARALLEL
*&---------------------------------------------------------------------*
                  FORM JOB_PARALLEL.
                    DO.
                      MOVE-CORRESPONDING IT_WOSUM TO IT_WOSUM_TEMP.

                      REFRESH : IT_DOWNFILE_TEMP,
                                IT_DOWNFILE_HMA_TEMP,
                                IT_DOWNFILE_HAC_TEMP.
                      CALL FUNCTION 'Z_FSD_ORDER_STATUS2'
                            STARTING NEW TASK W_TASKNAME
                         DESTINATION IN GROUP W_CLASSNAME
                      PERFORMING RETURN_01 ON END OF TASK
                            EXPORTING
                           DATE                  = P_DATE
                    IT_WOSUM              = IT_WOSUM_TEMP
                           FLAG           = P_FLAG
                           HACPO          = P_HACPO
                            TABLES
                          S_ATINN               = S_ATINN
                          IT_CABN               = IT_CABN
                 IT_DOWNFILE           = IT_DOWNFILE_TEMP
             IT_DOWNFILE_HMA       = IT_DOWNFILE_HMA_TEMP
             IT_DOWNFILE_HAC       = IT_DOWNFILE_HAC_TEMP
                            EXCEPTIONS
                              COMMUNICATION_FAILURE = 1
                              SYSTEM_FAILURE        = 2
                              RESOURCE_FAILURE      = 3
                              OTHERS                = 4.

                      CASE SY-SUBRC.
                        WHEN 0.
                          W_TASKNAME = W_TASKNAME + 1.
                          SND_JOBS = SND_JOBS  + 1.
                          EXIT.
                        WHEN 1 OR 2.
                          EXCP_FLAG = 'X'.
*       EXIT.
                        WHEN 3.
*       Receive reply to asynchronous RFC calls
                          IF EXCP_FLAG = SPACE.
                            EXCP_FLAG = 'X'.
*         First attempt for RESOURCE_Failure handling
                   WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '0.01' SECONDS.
                          ELSE.
*         Second attempt for RESOURCE_Failure handling
                    WAIT UNTIL RCV_JOBS >= SND_JOBS UP TO '0.1' SECONDS.
                          ENDIF.
                          IF SY-SUBRC = 0.
                            CLEAR EXCP_FLAG. " Reset flag
                          ENDIF.
                      ENDCASE.
                    ENDDO.
                  ENDFORM.                    " JOB_PARALLEL
*&---------------------------------------------------------------------*
*&      Form  RETURN_01
*&---------------------------------------------------------------------*
                  FORM RETURN_01 USING P_TASKNAME.
                    DATA: I_LINES TYPE I.                   "UD1K913472
                    RECEIVE RESULTS FROM FUNCTION 'Z_FSD_ORDER_STATUS2'
                                        TABLES
                                        S_ATINN               = S_ATINN
                                        IT_CABN               = IT_CABN
                               IT_DOWNFILE           = IT_DOWNFILE_TEMP
                           IT_DOWNFILE_HMA       = IT_DOWNFILE_HMA_TEMP
                           IT_DOWNFILE_HAC       = IT_DOWNFILE_HAC_TEMP
                                        EXCEPTIONS
                                          COMMUNICATION_FAILURE = 1
                                          SYSTEM_FAILURE        = 2
                                          RESOURCE_FAILURE      = 3
                                          OTHERS                = 4.

                    CHECK SY-SUBRC = 0.

                   APPEND LINES OF IT_DOWNFILE_TEMP      TO IT_DOWNFILE.
           DESCRIBE TABLE IT_DOWNFILE_HMA_TEMP LINES I_LINES."UD1K913472
                    IF I_LINES NE 0.                        "UD1K913472
               APPEND LINES OF IT_DOWNFILE_HMA_TEMP  TO IT_DOWNFILE_HMA.
                    ENDIF.                                  "UD1K913472
           DESCRIBE TABLE IT_DOWNFILE_HAC_TEMP LINES I_LINES."UD1K913472
                    IF I_LINES NE 0.                        "UD1K913472
               APPEND LINES OF IT_DOWNFILE_HAC_TEMP  TO IT_DOWNFILE_HAC.
                    ENDIF.                                  "UD1K913472
                    RCV_JOBS = RCV_JOBS + 1.
                  ENDFORM.                                  " RETURN_01
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_FILE
*&---------------------------------------------------------------------*
                  FORM DOWNLOAD_FILE.
                    DESCRIBE TABLE IT_DOWNFILE LINES W_N_9.
                    IF W_N_9 <> 0 and P_GLOVIS = 'X'.
                      PERFORM DOWNLOAD_GLOVIS USING W_N_9.
                    ENDIF.

                    DESCRIBE TABLE IT_DOWNFILE_HMA LINES W_N_9.
                    IF W_N_9 <> 0 AND P_HMA = 'X'.
                      PERFORM DOWNLOAD_HMA USING W_N_9.
                    ENDIF.

                    DESCRIBE TABLE IT_DOWNFILE_HAC LINES W_N_9.
                    IF W_N_9 <> 0 and P_HAC = 'X'.
                     PERFORM DOWNLOAD_HAC USING W_N_9.
                    ENDIF.
                  ENDFORM.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_GLOVIS
*&---------------------------------------------------------------------*
                  FORM DOWNLOAD_GLOVIS USING W_N_9.
**  CONCATENATE  '/sapmnt/' sy-sysid '/EDI/'
**               'GS_' sy-datum+2(6)
**               'S.txt'
**               INTO w_dsn.
                    CONCATENATE  '/usr/sap/EDI_SAP/'
                                 'GS_' SY-DATUM+2(6)
                                 'S.txt'
                                 INTO W_DSN.

                    PERFORM MAKE_R1.
                    LOOP AT IT_DOWNFILE.
                      OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
                      TRANSFER IT_DOWNFILE TO W_DSN.
                    ENDLOOP.
                    PERFORM MAKE_R6 USING W_N_9.

                    CLOSE DATASET W_DSN.

                    IF SY-SUBRC = 0.
                      MESSAGE I000 WITH TEXT-M02 '(GLOVIS)'.
                    ELSE.
                      MESSAGE I000 WITH TEXT-M03 '(GLOVIS)'.
                    ENDIF.
                  ENDFORM.                    " DOWNLOAD_GLOVIS
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_HMA
*&---------------------------------------------------------------------*
                  FORM DOWNLOAD_HMA USING W_N_9.
**  CONCATENATE  '/sapmnt/' sy-sysid '/EDI/'
**               'HM_' sy-datum+2(6)
**               'S.txt'
**               INTO w_dsn.
                    CONCATENATE  '/usr/sap/EDI_SAP/'
                                 'HM_' SY-DATUM+2(6)
                                 'S.txt'
                                 INTO W_DSN.

                    PERFORM MAKE_R1.
                    LOOP AT IT_DOWNFILE_HMA.
                      OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
                      TRANSFER IT_DOWNFILE_HMA TO W_DSN.
                    ENDLOOP.
                    PERFORM MAKE_R6 USING W_N_9.

                    CLOSE DATASET W_DSN.

                    IF SY-SUBRC = 0.
                      MESSAGE I000 WITH TEXT-M02 '(HMA)'.
                    ELSE.
                      MESSAGE I000 WITH TEXT-M03 '(HMA)'.
                    ENDIF.
                  ENDFORM.                    " DOWNLOAD_HMA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_HAC
*&---------------------------------------------------------------------*
                  FORM DOWNLOAD_HAC USING W_N_9.
**  CONCATENATE  '/sapmnt/' sy-sysid '/EDI/'
**               'HAC' sy-datum+2(6)
**               'S.txt'
**               INTO w_dsn.
                    CONCATENATE  '/usr/sap/EDI_SAP/'
'HMMAOS' SY-DATUM+2(6)       " HAC -->HMMAOS UD1K913472
                                 'S.txt'
                                 INTO W_DSN.

                    PERFORM MAKE_R1.
                    LOOP AT IT_DOWNFILE_HAC.
                      OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
                      TRANSFER IT_DOWNFILE_HAC TO W_DSN.
                    ENDLOOP.
                    PERFORM MAKE_R6 USING W_N_9.

                    CLOSE DATASET W_DSN.

                    IF SY-SUBRC = 0.
                      MESSAGE I000 WITH TEXT-M02 '(HAC)'.
                    ELSE.
                      MESSAGE I000 WITH TEXT-M03 '(HAC)'.
                    ENDIF.
                  ENDFORM.                    " DOWNLOAD_HAC
*&---------------------------------------------------------------------*
*&      Form  MAKE_R1
*&---------------------------------------------------------------------*
                  FORM MAKE_R1.
                    CLEAR IT_DOWNFILE.
                    IT_DOWNFILE-RECORD+0(3)    = 'O1H'.
                    IT_DOWNFILE-RECORD+3(8)    = SY-DATUM.
                    IT_DOWNFILE-RECORD+11(170) = ' '.

                OPEN DATASET W_DSN IN TEXT MODE FOR OUTPUT. "CHECK DUPL.
                    TRANSFER IT_DOWNFILE TO W_DSN.
                  ENDFORM.                                  " MAKE_R1
*&---------------------------------------------------------------------*
*&      Form  MAKE_R6
*&---------------------------------------------------------------------*
                  FORM MAKE_R6 USING W_N_9.
                    CLEAR IT_DOWNFILE.
                    IT_DOWNFILE-RECORD+0(3)    = 'O1T'.
                    IT_DOWNFILE-RECORD+3(9)    = W_N_9.
                    IT_DOWNFILE-RECORD+12(169) = ' '.


                    OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
                    TRANSFER IT_DOWNFILE TO W_DSN.
                  ENDFORM.                                  " MAKE_R6
*&---------------------------------------------------------------------*
*&      Form  CHECK_VSTATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
                  FORM CHECK_VSTATUS.
                    DATA: L_DATE    LIKE SY-DATUM.
                    DATA: L_ATINN   LIKE AUSP-ATINN.
                    DATA: L_ATINN1  LIKE AUSP-ATINN.
                    DATA: L_ATINN2   LIKE AUSP-ATINN.
                    DATA: L_ATINN3  LIKE AUSP-ATINN.
                    DATA: L_OBJEK   LIKE AUSP-OBJEK.
                    DATA: L_WO      LIKE AUSP-ATWRT.
                    DATA: L_ATWRT    LIKE AUSP-ATWRT.

*   CHECK BACK  n MONTHES ORDER

* delete n months ago order which all qty has been sequenced
* During selected months, check if all vehicles have been shipped
* if the modified qty equals to sequenced qty
*&----As per Kevin we don't need the above check.  - Shiva 08/16/2005
                    SELECT SINGLE ATINN INTO L_ATINN
                    FROM CABN
                    WHERE ATNAM = 'P_WORK_ORDER'.
                    SELECT SINGLE ATINN INTO L_ATINN1
                    FROM CABN
                    WHERE ATNAM = 'P_RP_STATUS'.
                    SELECT SINGLE ATINN INTO L_ATINN2
                    FROM CABN
                    WHERE ATNAM = 'P_EXT_COLOR'.
                    SELECT SINGLE ATINN INTO L_ATINN3
                    FROM CABN
                    WHERE ATNAM = 'P_INT_COLOR'.

                    LOOP AT IT_WOSUM.
                      CONCATENATE IT_WOSUM-WO_SER
                                  IT_WOSUM-NATION
                                  IT_WOSUM-DEALER
                             INTO L_WO.
*                      IF it_wosum-modqty = it_wosum-seqqty.
*      IF it_wosum-wo_ser+1(4) < l_date+2(4).
*        DELETE it_wosum INDEX sy-tabix.
*      ELSEif it_wosum-wo_ser+1(4) lt p_date .
                      IF IT_WOSUM-WO_SER+1(4) LE P_DATE .
                      SELECT SINGLE OBJEK ATWRT INTO (L_OBJEK, L_ATWRT)
                                                              FROM AUSP
                                                                  WHERE
*             OBJEK IN ( SELECT OBJEK FROM AUSP
*                          WHERE ATWRT = L_WO
*                            AND KLART = '002'
*                            AND ATINN = L_ATINN )
                                      OBJEK IN ( SELECT OBJEK FROM AUSP
                                WHERE OBJEK IN ( SELECT OBJEK FROM AUSP
                                                     WHERE ATWRT = L_WO
                                                      AND KLART = '002'
                                                  AND ATINN = L_ATINN )
                                              AND ATWRT = IT_WOSUM-EXTC
                                                      AND KLART = '002'
                                                 AND ATINN = L_ATINN2 )
                                  AND OBJEK IN ( SELECT OBJEK FROM AUSP
                                WHERE OBJEK IN ( SELECT OBJEK FROM AUSP
                                                     WHERE ATWRT = L_WO
                                                      AND KLART = '002'
                                                  AND ATINN = L_ATINN )
                                              AND ATWRT = IT_WOSUM-INTC
                                                      AND KLART = '002'
                                                 AND ATINN = L_ATINN3 )
                                                      AND KLART = '002'
                                                   AND ATINN = L_ATINN1
                                                       AND ATWRT < '25'.
                        IF SY-SUBRC NE 0.
                          DELETE IT_WOSUM.
                        ENDIF.
                      ELSE.
                        DELETE IT_WOSUM.
                      ENDIF.
*                      endif.
                    ENDLOOP.


                  ENDFORM.                    " CHECK_VSTATUS
