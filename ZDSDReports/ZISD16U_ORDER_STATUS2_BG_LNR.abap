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
* Date       Developer    Request ID Description
*
************************************************************************
REPORT ZISD16U_ORDER_STATUS2 NO STANDARD PAGE HEADING
                             MESSAGE-ID ZMSD.


*
TABLES : ZTPP_WOSUM,
         CABN,
         AUSP.


DATA: snd_jobs TYPE i VALUE 1,
      rcv_jobs TYPE i VALUE 1,
      width TYPE i,
      excp_flag(1) TYPE c,
      taskname(4) TYPE n VALUE '0001',
      err_chk,
       z_num(2) TYPE n,
      w_int TYPE i,
      p_plant LIKE marc-werks VALUE 'P001'.

*
DATA : BEGIN OF IT_WOSUM OCCURS 0.
        INCLUDE STRUCTURE ZTPP_WOSUM.
DATA : END OF IT_WOSUM.

DATA : BEGIN OF IT_WOSUM_TEMP OCCURS 0.
        INCLUDE STRUCTURE ZTPP_WOSUM.
DATA : END OF IT_WOSUM_TEMP.

DATA : BEGIN OF IT_DOWNFILE OCCURS 0,
       RECORD(200),
       END OF IT_DOWNFILE.

DATA : BEGIN OF IT_DOWNFILE_HMA OCCURS 0,
       RECORD(200),
       END OF IT_DOWNFILE_HMA.

DATA : BEGIN OF IT_DOWNFILE_HAC OCCURS 0,
       RECORD(200),
       END OF IT_DOWNFILE_HAC.

DATA : BEGIN OF IT_DOWNFILE_TEMP OCCURS 0,
       RECORD(200),
       END OF IT_DOWNFILE_TEMP.

DATA : BEGIN OF IT_DOWNFILE_HMA_TEMP OCCURS 0,
       RECORD(200),
       END OF IT_DOWNFILE_HMA_TEMP.

DATA : BEGIN OF IT_DOWNFILE_HAC_TEMP OCCURS 0,
       RECORD(200),
       END OF IT_DOWNFILE_HAC_TEMP.

DATA : BEGIN OF IT_AUSP OCCURS 0,
       OBJEK LIKE AUSP-OBJEK,
       ATINN LIKE AUSP-ATINN,
       ATNAM LIKE CABN-ATNAM,
       ATWRT LIKE AUSP-ATWRT,
       ATFLV LIKE AUSP-ATFLV,
       END OF IT_AUSP.

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

DATA : W_DSN_B(20) VALUE '/sapmnt/UP2/EDI/',
       W_DSN(90).

DATA : VARIANT LIKE INDX-SRTFD VALUE 'ISD16_01'.


*
SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
PARAMETERS : P_DATE LIKE SY-DATUM+2(4) NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK B1.


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
 IMPORT P_DATE FROM DATABASE INDX(ZS) ID VARIANT.

  PERFORM INIT_DATE.

  SELECT *
         INTO TABLE IT_WOSUM
         FROM ZTPP_WOSUM
        WHERE WO_SER IN S_WO_SER.

  LOOP AT IT_WOSUM.
    IF IT_WOSUM-WO_SER+1(4) < P_DATE AND
       IT_WOSUM-MODQTY = IT_WOSUM-SEQQTY.
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
  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_ORDER_ZONE'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_REGION_PORT'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_COLOR_SER'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_FLEET'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_MANUAL_ORDER'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_VIN'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_SEQUENCE_DATE'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_RP01_ACTUAL_DATE'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_RP02_ACTUAL_DATE'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_RP07_ACTUAL_DATE'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_RP18_ACTUAL_DATE'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_RP19_ACTUAL_DATE'.
  S_ATINN-LOW = CABN-ATINN. APPEND S_ATINN.
  IT_CABN-ATINN = CABN-ATINN.
  IT_CABN-ATNAM = CABN-ATNAM.
  APPEND IT_CABN.

  SELECT SINGLE ATINN INTO CABN-ATINN
         FROM CABN
        WHERE ATNAM EQ 'P_RP20_ACTUAL_DATE'.
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
******
  DO.
    READ TABLE IT_WOSUM INDEX 1.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

*    PERFORM SAPGUI_PROGRESS_INDICATOR USING 1.
    MOVE-CORRESPONDING  IT_WOSUM  TO IT_WOSUM_TEMP.
    APPEND IT_WOSUM_TEMP.

    CALL FUNCTION 'Z_ORDER_PALL'
    STARTING NEW TASK taskname DESTINATION IN GROUP 'PG_BF'
    PERFORMING return_01 ON END OF TASK
    exporting
      IT_WOSUM              =  IT_WOSUM_TEMP
    TABLES
      S_ATINN               =  S_ATINN
      IT_DOWNFILE_o           =  IT_DOWNFILE_TEMP
      IT_DOWNFILE_HMA_o       =  IT_DOWNFILE_HMA_TEMP
      IT_DOWNFILE_HAC_o       =  IT_DOWNFILE_HAC_TEMP
    EXCEPTIONS
          communication_failure       = 1
          system_failure              = 2
          RESOURCE_FAILURE            = 3
          OTHERS                      = 4.

    CASE sy-subrc.
      WHEN 0.
        taskname = taskname + 1.
        snd_jobs = snd_jobs  + 1.
        DELETE IT_WOSUM INDEX 1.
      WHEN 1 OR 2.
        excp_flag = 'X'.
*          EXIT.
      WHEN 3.
*Receive reply to asynchronous RFC calls
        IF excp_flag = space.
          excp_flag = 'X'.
*First attempt for RESOURCE_Failure handling
          WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.01' SECONDS.
        ELSE.
*Second attempt for RESOURCE_Failure handling
          WAIT UNTIL rcv_jobs >= snd_jobs UP TO '0.1' SECONDS.
        ENDIF.
        IF sy-subrc = 0.
          CLEAR excp_flag. " Reset flag
        ENDIF.
    ENDCASE.
*      CLEAR : wa_bfst,it_bfst.
  ENDDO.
  WAIT UNTIL rcv_jobs >= snd_jobs UP TO '100' SECONDS.
*    IF sy-subrc = 0.
*      COMMIT WORK.
*    ELSE.
*      ROLLBACK WORK.
*    ENDIF.

******
  PERFORM DOWNLOAD_FILE.

ENDFORM.                    " PROCESS_DATA
*&---------------------------------------------------------------------*
*&      Form  R2
*&---------------------------------------------------------------------*

FORM return_01 USING taskname.

  RECEIVE RESULTS FROM FUNCTION 'ZLNR_FUNC'
    TABLES
      S_ATINN               =  S_ATINN
      IT_DOWNFILE_o           =  IT_DOWNFILE_TEMP
      IT_DOWNFILE_HMA_o       =  IT_DOWNFILE_HMA_TEMP
      IT_DOWNFILE_HAC_o       =  IT_DOWNFILE_HAC_TEMP
          EXCEPTIONS
          communication_failure       = 1
          system_failure              = 2
          RESOURCE_FAILURE            = 3
          OTHERS                      = 4.


  CHECK sy-subrc = 0.
    APPEND LINES OF IT_DOWNFILE_TEMP  TO IT_DOWNFILE.
    APPEND LINES OF IT_DOWNFILE_HMA_TEMP  TO IT_DOWNFILE_HMA.
    APPEND LINES OF IT_DOWNFILE_HAC_TEMP  TO IT_DOWNFILE_HAC.

  CLEAR     IT_WOSUM_TEMP.
  rcv_jobs  = rcv_jobs + 1.
ENDFORM.                                                    " return_01


FORM DOWNLOAD_FILE.
  DESCRIBE TABLE IT_DOWNFILE LINES W_N_9.
  IF W_N_9 <> 0.
    W_CNT = W_N_9.
    PERFORM DOWNLOAD_GLOVIS USING W_N_9.
  ENDIF.

  DESCRIBE TABLE IT_DOWNFILE_HMA LINES W_N_9.
  IF W_N_9 <> 0.
    W_CNT = W_N_9.
    PERFORM DOWNLOAD_HMA USING W_N_9.
  ENDIF.

  DESCRIBE TABLE IT_DOWNFILE_HAC LINES W_N_9.
  IF W_N_9 <> 0.
    W_CNT = W_N_9.
    PERFORM DOWNLOAD_HAC USING W_N_9.
  ENDIF.
ENDFORM.                    " DOWNLOAD_FILE
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_GLOVIS
*&---------------------------------------------------------------------*
FORM DOWNLOAD_GLOVIS USING W_N_9.
  CONCATENATE  W_DSN_B
               'GLOVIS_sd_vordpsts_' SY-DATUM
               '.txt'
               INTO W_DSN.

  PERFORM MAKE_R1.
  LOOP AT IT_DOWNFILE.
    PERFORM SAPGUI_PROGRESS_INDICATOR USING 2.

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
  CONCATENATE  W_DSN_B
               'HMA_sd_vordpsts_' SY-DATUM
               '.txt'
               INTO W_DSN.

  PERFORM MAKE_R1.
  LOOP AT IT_DOWNFILE_HMA.
    PERFORM SAPGUI_PROGRESS_INDICATOR USING 3.

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
  CONCATENATE  W_DSN_B
               'HAC_sd_vordpsts_' SY-DATUM
               '.txt'
               INTO W_DSN.

  PERFORM MAKE_R1.
  LOOP AT IT_DOWNFILE_HAC.
    PERFORM SAPGUI_PROGRESS_INDICATOR USING 4.

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

  OPEN DATASET W_DSN IN TEXT MODE FOR OUTPUT. "CHECK DUPL.
  TRANSFER IT_DOWNFILE TO W_DSN.
ENDFORM.                                                    " MAKE_R1
*&---------------------------------------------------------------------*
*&      Form  MAKE_R6
*&---------------------------------------------------------------------*
FORM MAKE_R6 USING W_N_9.
  CLEAR IT_DOWNFILE.
  IT_DOWNFILE-RECORD+0(3)    = 'O1T'.
  IT_DOWNFILE-RECORD+3(9)    = W_N_9.

  OPEN DATASET W_DSN IN TEXT MODE FOR APPENDING.
  TRANSFER IT_DOWNFILE TO W_DSN.
ENDFORM.                                                    " MAKE_R6
*&---------------------------------------------------------------------*
*&      Form  SAPGUI_PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
FORM SAPGUI_PROGRESS_INDICATOR USING GUBUN.
  DATA : W_PERC TYPE P DECIMALS 2,
         W_TEXT(50).

  W_PERC = SY-TABIX / W_CNT * 100.
  WRITE W_PERC TO W_TEXT+0(7).
  CASE GUBUN.
    WHEN '1'.
      CONCATENATE W_TEXT 'Make the file with WorkOrder'
                  INTO W_TEXT SEPARATED BY SPACE.
    WHEN '2'.
      CONCATENATE W_TEXT 'Downloading file for GLOVIS'
                  INTO W_TEXT SEPARATED BY SPACE.
    WHEN '3'.
      CONCATENATE W_TEXT 'Downloading file for HMA'
                  INTO W_TEXT SEPARATED BY SPACE.
    WHEN '4'.
      CONCATENATE W_TEXT 'Downloading file for HAC'
                  INTO W_TEXT SEPARATED BY SPACE.
  ENDCASE.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
       EXPORTING
            PERCENTAGE = W_PERC
            TEXT       = W_TEXT.
ENDFORM.                    " SAPGUI_PROGRESS_INDICATOR
