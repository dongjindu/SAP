*----------------------------------------------------------------------*
***INCLUDE ZMMIFTZWF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  bom_mat_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BOM_MAT_INFO.
*  DATA: wa_6022_tmp LIKE wa_6022_01.
*
*  data: it_6022_tmp LIKE TABLE OF wa_6022_01.
*
*  FIELD-SYMBOLS: <fs_comp> LIKE LINE OF it_components1,
*                 <fs_halb> LIKE LINE OF it_halb,
*                 <fs_stpox> LIKE LINE OF it_filter_stpox,
*                 <fs_6022> LIKE LINE OF it_6022_tmp.
*
*  r_mtart-sign = 'E'.
*  r_mtart-option = 'EQ'.
*  r_mtart-low = 'ROH'.
*  APPEND r_mtart.
*  r_mtart-low = 'ROH1'.
*  APPEND r_mtart.
*
*  SORT it_components1 BY material.
*
*  LOOP AT it_components1 ASSIGNING <fs_comp>.
*    READ TABLE it_ztmm_6022_01 INTO wa_6022_tmp
*                           WITH KEY matnr = <fs_comp>-material.
*    IF sy-subrc NE 0.
*      READ TABLE it_mat_info INTO wa_mat_info
*                             WITH KEY matnr = <fs_comp>-material.
*      IF wa_mat_info-mtart EQ 'HALB'.
*        wa_halb-matnr = <fs_comp>-material.
*        wa_halb-werks = <fs_comp>-plant.
*        wa_halb-datuv = <fs_comp>-req_date.
*        wa_halb-bdmng = <fs_comp>-entry_qty.
*        COLLECT wa_halb INTO it_halb.
*        <fs_comp>-phant_item = 'X'.
*      ELSE.
*        wa_6022_01-matnr = <fs_comp>-material.
*        wa_6022_01-maktx = <fs_comp>-matl_desc.
*        wa_6022_01-meins = <fs_comp>-base_uom.
*        wa_6022_01-lbkum = <fs_comp>-req_quan + wa_mat_info-lbkum.
*        wa_6022_01-profl = wa_mat_info-profl.
*        wa_6022_01-mtart = wa_mat_info-mtart.
*        wa_6022_01-ptc   = 'PC'.
*        COLLECT wa_6022_01 INTO it_ztmm_6022_01.
*        CLEAR: wa_mat_info, wa_6022_01,wa_6022_tmp.
*      ENDIF.
*    ELSE.
*      wa_6022_01 = wa_6022_tmp.
*      wa_6022_01-lbkum = <fs_comp>-req_quan.
*      COLLECT wa_6022_01 INTO it_ztmm_6022_01.
*      CLEAR: wa_mat_info, wa_6022_01, wa_6022_tmp.
*    ENDIF.
*  ENDLOOP.
*
*  DELETE it_components1 WHERE phant_item = 'X'.

  DATA: IT_6022_TMP LIKE TABLE OF WA_6022_01.
  FIELD-SYMBOLS: <FS_HALB> LIKE LINE OF IT_HALB,
                 <FS_STPOX> LIKE LINE OF IT_FILTER_STPOX,
                 <FS_6022> LIKE LINE OF IT_6022_TMP.

  DATA: BEGIN OF WA_T460A,
            MATNR LIKE MARC-MATNR,
            WERKS LIKE MARC-WERKS,
            WRK02 LIKE T460A-WRK02,
          END OF WA_T460A.
  DATA: IT_T460A LIKE TABLE OF WA_T460A.


  CLEAR: W_LINES,W_REM, W_NO_TIMES, W_FRM, W_TO,
         W_TASKNAME, W_RCV_JOBS, W_SND_JOBS, W_EXCEP_FLAG,
         WA_6022_TMP.

  DESCRIBE TABLE IT_HALB LINES W_LINES.

  SELECT MATNR MARC~WERKS WRK02 INTO TABLE IT_T460A
                                FROM MARC
                                INNER JOIN T460A
                                ON T460A~WERKS = MARC~WERKS
                                AND T460A~SOBSL = MARC~SOBSL
                               FOR ALL ENTRIES IN IT_HALB
                                WHERE MATNR EQ IT_HALB-MATNR
                                AND  MARC~WERKS EQ IT_HALB-WERKS.
  IF SY-SUBRC NE 0.
  ELSE.
    SORT IT_T460A BY MATNR WERKS.
    LOOP AT IT_HALB ASSIGNING <FS_HALB>.
      READ TABLE IT_T460A INTO WA_T460A
                          WITH KEY MATNR = <FS_HALB>-MATNR
                                   WERKS = <FS_HALB>-WERKS
                                   BINARY SEARCH.
      IF SY-SUBRC NE 0.
      ELSE.
        IF WA_T460A-WRK02 IS INITIAL.
        ELSE.
          <FS_HALB>-WERKS = WA_T460A-WRK02.
          WA_HALB-WERKS       = WA_T460A-WRK02.
          MODIFY IT_HALB FROM WA_HALB TRANSPORTING WERKS
                          WHERE MATNR = <FS_HALB>-MATNR.
          CLEAR: WA_HALB, WA_T460A.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT IT_HALB BY MATNR WERKS DATUV.
  IF W_LINES > W_FREE.
    W_REM = W_LINES MOD W_FREE.
    W_NO_TIMES = W_LINES / W_FREE.
    IF W_REM EQ 0.
    ELSE.
      W_NO_TIMES = W_NO_TIMES + 1.
    ENDIF.
  ELSE.
    W_NO_TIMES = 1.
  ENDIF.
  W_I = 1.
  WHILE W_I <= W_NO_TIMES.
    IF W_I = 1.
      W_FRM = W_I.
    ELSE.
      W_FRM = W_TO + 1.
    ENDIF.
    IF W_LINES > W_FREE.
      W_TO =  W_I * W_FREE .
    ELSE.
      W_TO = W_LINES.
    ENDIF.
    LOOP AT IT_HALB ASSIGNING <FS_HALB> FROM W_FRM TO W_TO.
** change by furong on 05/04/2006

      DO.
        CALL FUNCTION 'Z_FFTZ_EXP_BOM'
          STARTING NEW TASK W_TASKNAME
          DESTINATION IN GROUP 'PG_FTZ'
          PERFORMING FSC_BOM_EXP ON END OF TASK
          EXPORTING
            P_CAPID  = 'PP01'
            P_DATUV  = <FS_HALB>-DATUV
            P_EMENG  = <FS_HALB>-BDMNG
            P_MEHRS  = 'X'
            P_MMORY  = '1'
            P_MTNRV  = <FS_HALB>-MATNR
            P_STLAL  = <FS_HALB>-STLAL                      "'01'
            P_STLAN  = '1'
            P_WERKS  = <FS_HALB>-WERKS
          TABLES
            P_STPOX  = IT_MAIN_STPOX
          EXCEPTIONS
             COMMUNICATION_FAILURE = 1
             SYSTEM_FAILURE        = 2
             RESOURCE_FAILURE      = 3.
        CASE SY-SUBRC.
          WHEN 0.
            W_TASKNAME = W_TASKNAME + 1.
            W_SND_JOBS = W_SND_JOBS + 1.
            EXIT.
          WHEN 1 OR 2.
            W_EXCEP_FLAG = 'X'.
          WHEN 3.
            IF W_EXCEP_FLAG = SPACE.
              W_EXCEP_FLAG = 'X'.
              WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.01' SECONDS.
            ELSE.
              WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.1' SECONDS.
            ENDIF.
            IF SY-SUBRC EQ 0.
              CLEAR W_EXCEP_FLAG.
            ELSE.
*            exit.
            ENDIF.
        ENDCASE.
      ENDDO.
** end of change
    ENDLOOP.

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

    DO.
      WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
      IF W_RCV_JOBS >= W_SND_JOBS.
        EXIT.
      ENDIF.
    ENDDO.



    W_I = W_I + 1.
  ENDWHILE.

** Added on 12/14/2006

  CLEAR: W_SND_JOBS, W_RCV_JOBS.
  LOOP AT IT_HALB_ENG ASSIGNING <FS_HALB>.
    DO.
      CALL FUNCTION 'Z_FFTZ_EXP_BOM'
        STARTING NEW TASK W_TASKNAME
        DESTINATION IN GROUP 'PG_FTZ'
        PERFORMING FSC_BOM_EXP ON END OF TASK
        EXPORTING
          P_CAPID  = 'PP01'
          P_DATUV  = <FS_HALB>-DATUV
          P_EMENG  = <FS_HALB>-BDMNG
          P_MEHRS  = 'X'
          P_MMORY  = '1'
          P_MTNRV  = <FS_HALB>-MATNR
          P_STLAL  = <FS_HALB>-STLAL                        "'01'
          P_STLAN  = '1'
          P_WERKS  = <FS_HALB>-WERKS
        TABLES
          P_STPOX  = IT_MAIN_STPOX
        EXCEPTIONS
           COMMUNICATION_FAILURE = 1
           SYSTEM_FAILURE        = 2
           RESOURCE_FAILURE      = 3.
      CASE SY-SUBRC.
        WHEN 0.
          W_TASKNAME = W_TASKNAME + 1.
          W_SND_JOBS = W_SND_JOBS + 1.
          EXIT.
        WHEN 1 OR 2.
          W_EXCEP_FLAG = 'X'.
        WHEN 3.
          IF W_EXCEP_FLAG = SPACE.
            W_EXCEP_FLAG = 'X'.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.01' SECONDS.
          ELSE.
            WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS UP TO '0.1' SECONDS.
          ENDIF.
          IF SY-SUBRC EQ 0.
            CLEAR W_EXCEP_FLAG.
          ELSE.
*            exit.
          ENDIF.
      ENDCASE.
    ENDDO.
** end of change
  ENDLOOP.

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

  DO.
    WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
    IF W_RCV_JOBS >= W_SND_JOBS.
      EXIT.
    ENDIF.
  ENDDO.


** end of change

  IT_6022_TMP[] = IT_ZTMM_6022_01[].

  LOOP AT IT_FILTER_STPOX ASSIGNING <FS_STPOX>.
    WA_6022_01-MATNR = <FS_STPOX>-IDNRK.
    WA_6022_01-MAKTX = <FS_STPOX>-OJTXP.
    WA_6022_01-MEINS = <FS_STPOX>-MMEIN.
    READ TABLE IT_ZTMM_6022_01 INTO WA_6022_TMP
                           WITH KEY MATNR = <FS_STPOX>-IDNRK
                           TRANSPORTING PROFL.
    IF SY-SUBRC NE 0.
      READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
                             WITH KEY MATNR = <FS_STPOX>-IDNRK.
      IF SY-SUBRC EQ 0.
        WA_6022_01-LBKUM = <FS_STPOX>-MNGKO + WA_MAT_INFO-LBKUM.
        WA_6022_01-PROFL = WA_MAT_INFO-PROFL.
      ENDIF.
    ELSE.
      WA_6022_01-LBKUM = <FS_STPOX>-MNGKO.
      WA_6022_01-PROFL = WA_6022_TMP-PROFL.
    ENDIF.
    WA_6022_01-MTART = <FS_STPOX>-MTART.
    WA_6022_01-PTC   = 'PC'.
    COLLECT WA_6022_01 INTO IT_ZTMM_6022_01.
    CLEAR: WA_MAT_INFO, WA_6022_01.
  ENDLOOP.
  REFRESH IT_6022_TMP.
  LOOP AT IT_MAT_INFO INTO WA_MAT_INFO.
    READ TABLE IT_ZTMM_6022_01 WITH KEY MATNR = WA_MAT_INFO-MATNR
                                           TRANSPORTING NO FIELDS.
    IF SY-SUBRC NE 0.
      IF NOT WA_MAT_INFO-LBKUM IS INITIAL.
        WA_6022_01-MATNR = WA_MAT_INFO-MATNR.
        WA_6022_01-MAKTX = WA_MAT_INFO-MAKTX.
        WA_6022_01-MEINS = WA_MAT_INFO-MEINS.
        WA_6022_01-LBKUM = WA_MAT_INFO-LBKUM.
        WA_6022_01-PROFL = WA_MAT_INFO-PROFL.
        WA_6022_01-MTART = WA_MAT_INFO-MTART.
        WA_6022_01-PTC   =  'PC'.
        APPEND WA_6022_01 TO IT_6022_TMP.
      ENDIF.
    ENDIF.
    CLEAR: WA_6022_01.
  ENDLOOP.
  IF NOT IT_6022_TMP[] IS INITIAL.
    APPEND LINES OF IT_6022_TMP TO IT_ZTMM_6022_01.
  ENDIF.
*&----------Remove color and sum same material(s).
  CLEAR WA_6022_01.
  REFRESH IT_6022_TMP.
  LOOP AT IT_ZTMM_6022_01 ASSIGNING <FS_6022>.
    PERFORM SEPARATE_COLOR USING <FS_6022>-MATNR
                                 <FS_6022>-MTART
                           CHANGING <FS_6022>-MATNR.
    WA_6022_01 = <FS_6022>.
    COLLECT WA_6022_01 INTO IT_6022_TMP.
  ENDLOOP.
  REFRESH IT_ZTMM_6022_01.
  IF NOT IT_6022_TMP[] IS INITIAL.
    APPEND LINES OF IT_6022_TMP TO IT_ZTMM_6022_01.
  ENDIF.
  WA_6022_01-UDATE = SY-DATUM.
  WA_6022_01-UTIME = SY-UZEIT.
  CONCATENATE P_DATE 'T' WA_6022_01-UTIME INTO WA_6022_01-DATE_TIME.
  MODIFY IT_ZTMM_6022_01 FROM WA_6022_01
                         TRANSPORTING DATE_TIME UDATE UTIME
                         WHERE DATE_TIME IS INITIAL AND
                               UDATE     IS INITIAL AND
                               UTIME     IS INITIAL.
  CLEAR WA_6022_01.
  SORT IT_ZTMM_6022_01 BY MATNR.
  REFRESH IT_6022_TMP.
** changed by Furong on 04/24/07: 741F525149

  DATA : BEGIN OF LT_AFFW_TEMP OCCURS 0,
        WEBLNR LIKE AFFW-WEBLNR,
        WEBLPOS LIKE AFFW-WEBLPOS,
        MATNR LIKE MARA-MATNR,
        SHKZG LIKE AFFW-SHKZG,
        ERFMG LIKE AFFW-ERFMG,
        END OF LT_AFFW_TEMP.
*
*data: lt_affw like table of lt_affw_temp with header line.
  DATA : BEGIN OF LT_AFFW OCCURS 0,
        MATNR LIKE MARA-MATNR,
        ERFMG LIKE AFFW-ERFMG,
        END OF LT_AFFW.

*  DATA: LT_AFFW_TEMP TYPE TABLE OF AFFW WITH HEADER LINE.


  SELECT WEBLNR WEBLPOS MATNR SHKZG ERFMG INTO TABLE LT_AFFW_TEMP
  FROM AFFW
  FOR ALL ENTRIES IN IT_ZTMM_6022_01
  WHERE MATNR = IT_ZTMM_6022_01-MATNR.

  LOOP AT LT_AFFW_TEMP.
    MOVE-CORRESPONDING LT_AFFW_TEMP TO LT_AFFW.
    IF LT_AFFW_TEMP-SHKZG = 'H'.
      LT_AFFW-ERFMG = - LT_AFFW_TEMP-ERFMG.
    ENDIF.
    COLLECT LT_AFFW.
    CLEAR: LT_AFFW, LT_AFFW_TEMP.
  ENDLOOP.

  LOOP AT IT_ZTMM_6022_01 INTO WA_6022_01.
    READ TABLE LT_AFFW WITH KEY MATNR = WA_6022_01-MATNR.
    IF SY-SUBRC = 0.
      WA_6022_01-LBKUM = WA_6022_01-LBKUM + LT_AFFW-ERFMG.
      MODIFY IT_ZTMM_6022_01 FROM WA_6022_01.
    ENDIF.
  ENDLOOP.
** end of change
ENDFORM.                    " bom_mat_info

*&---------------------------------------------------------------------*
*&      Form  bom_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM BOM_INFO USING W_TASKNAME.

  RECEIVE RESULTS FROM FUNCTION 'Z_FFTZ_READ_PLANORDER'
          IMPORTING PS_RESULT = WA_RETURN
                    PS_HEADER = WA_HEADER
          TABLES    PIT_COMPONENTS = IT_COMPONENTS
          EXCEPTIONS
                    COMMUNICATION_FAILURE = 1
                    SYSTEM_FAILURE        = 2.
  IF SY-SUBRC NE 0.
    W_EXCEP_FLAG = 'X'.
    EXIT.
  ENDIF.
  W_RCV_JOBS = W_RCV_JOBS + 1.
  DELETE IT_COMPONENTS WHERE PHANT_ITEM = 'X'.
  READ TABLE IT_KEY_WIP1 INTO WA_KEY_WIP
                         WITH KEY PLNUM = WA_HEADER-PLANNEDORDER_NUM
                         TRANSPORTING STATU.
  IF SY-SUBRC NE 0.
  ELSE.
    DELETE IT_COMPONENTS WHERE SORT_STRING > WA_KEY_WIP-STATU.
    APPEND LINES OF IT_COMPONENTS TO IT_COMPONENTS1.
  ENDIF.
  REFRESH IT_COMPONENTS.
ENDFORM.                    " bom_info

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_DATA.
  DATA: LV_LOGNO_H TYPE NUM10.
  DATA: LV_ZRESULT LIKE ZSCA_IF_TIME_STAMP_OUT-ZRESULT.
  DATA: LV_MESSAGE TYPE BAPI_MSG. "Message text (220)
  CONSTANTS : C_DEST(10) VALUE 'WMGM01'.

*/ Call Outbound RFC FM
  CALL FUNCTION 'Z_FMM_6022_OUT_INVREP'
    DESTINATION              C_DEST
    TABLES
      EXT_ZTMM_6022_01      = IT_ZTMM_6022_01
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1 MESSAGE LV_MESSAGE
      SYSTEM_FAILURE        = 2 MESSAGE LV_MESSAGE.

  IF SY-SUBRC NE 0.
    LV_ZRESULT = 'E'.  "Result of the Processing
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ELSE.
    LV_ZRESULT = 'S'.  "Result of the Processing
    LV_MESSAGE = 'Outbound RFC FM Connected!'(002).
    MESSAGE S999(ZMMM) WITH LV_MESSAGE.
  ENDIF.

*/ Modify it_ZTMM_6022_01
* App. Doc. No.
  PERFORM NUMBER_GET_NEXT USING    C_NRO_NR_09
                                   'ZMMNRO0002'
                          CHANGING W_ZDOCNO.
  COMMIT WORK.

  LOOP AT IT_ZTMM_6022_01 ASSIGNING <FS_ZTMM_6022_01>.
*    PERFORM number_get_next USING    '00'
*                                     'ZMMNRO0002'
*                            CHANGING lv_logno_h.
    LV_LOGNO_H = LV_LOGNO_H + 1.

    <FS_ZTMM_6022_01>-ZDOCNO  = W_ZDOCNO.  "App. Doc. No.
    <FS_ZTMM_6022_01>-LOGNO_H = LV_LOGNO_H."Logno Header

    <FS_ZTMM_6022_01>-ZUSER   = SY-UNAME.  "User name
*    <fs_ZTMM_6022_01>-zsdat   = .  "Send File Created Date
*    <fs_ZTMM_6022_01>-zstim   = .  "Send file Created Time
    <FS_ZTMM_6022_01>-ZEDAT   = SY-DATUM.  "SAP Interface Date
    <FS_ZTMM_6022_01>-ZETIM   = SY-UZEIT.  "SAP Interface Time
    <FS_ZTMM_6022_01>-ZMODE   = 'C'.       "Data Characteristic Flag
    <FS_ZTMM_6022_01>-ZRESULT = LV_ZRESULT."Result of the Processing
    <FS_ZTMM_6022_01>-ZMSG    = LV_MESSAGE."Message text
*    <fs_ZTMM_6022_01>-zzret   = .  "Inerface Return Value
  ENDLOOP.

*/ Logging to it_ZTMM_6022_01.
  INSERT ZTMM_6022_01 FROM TABLE IT_ZTMM_6022_01.

ENDFORM.                    " process_data
*&---------------------------------------------------------------------*
*&      Form  display_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_LOG.
  CALL SCREEN 0100.  " Go to Screen 0100
ENDFORM.                    " display_log
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM MASK_COLUMNS TABLES  P_IT_FIELDCAT STRUCTURE IT_FIELDCAT.

* Build the fieldcat according to DDIC structure ZTMM_6022_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_STRUCTURE_NAME = 'ZTMM_6022_01'
       CHANGING
            CT_FIELDCAT      = P_IT_FIELDCAT[].

* Make Column header
  LOOP AT P_IT_FIELDCAT.
    IF P_IT_FIELDCAT-FIELDNAME = 'ZDOCNO'.
      P_IT_FIELDCAT-COLTEXT = 'App.DocNo.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'LOGNO_H'.
      P_IT_FIELDCAT-COLTEXT = 'Log No.'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'PTC'.
      P_IT_FIELDCAT-COLTEXT = 'Product Type Code'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'DATE_TIME'.
      P_IT_FIELDCAT-COLTEXT = 'DATE_TIME'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSDAT'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
    ELSEIF P_IT_FIELDCAT-FIELDNAME = 'ZSTIM'.
      P_IT_FIELDCAT-NO_OUT = 'X'.
*    ELSEIF p_IT_fieldcat-fieldname = 'MATNR'.
*      p_IT_fieldcat-outputlen = 18.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_DESC'.
*      p_IT_fieldcat-checkbox = space.
*    ELSEIF p_IT_fieldcat-fieldname = 'DESC_ZCH_REASON'.
*      p_IT_fieldcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY P_IT_FIELDCAT.
  ENDLOOP.

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  number_get_next
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_NRO_NR_09  text
*      -->P_0610   text
*      <--P_W_ZDOCNO  text
*----------------------------------------------------------------------*
FORM NUMBER_GET_NEXT
           USING    VALUE(P_NRO_INTERVAL) LIKE INRI-NRRANGENR
                    VALUE(P_NRO_OBJECT)   LIKE INRI-OBJECT
           CHANGING VALUE(P_NRO_NEXT).

  CLEAR: P_NRO_NEXT.
  CALL FUNCTION 'NUMBER_GET_NEXT'
       EXPORTING
            NR_RANGE_NR             = P_NRO_INTERVAL
            OBJECT                  = P_NRO_OBJECT
       IMPORTING
            NUMBER                  = P_NRO_NEXT
       EXCEPTIONS
            INTERVAL_NOT_FOUND      = 1
            NUMBER_RANGE_NOT_INTERN = 2
            OBJECT_NOT_FOUND        = 3
            QUANTITY_IS_0           = 4
            QUANTITY_IS_NOT_1       = 5
            INTERVAL_OVERFLOW       = 6
            OTHERS                  = 7.
  IF SY-SUBRC <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " number_get_next
*&---------------------------------------------------------------------*
*&      Form  ps_tb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PS_TB.
* Instanciate PF-STATUS & TITLEBAR.
  DATA: LV_NUMBERING       TYPE I.
  DATA: LV_NUMBERING_C(10) TYPE C.
  DESCRIBE TABLE IT_ZTMM_6022_01 LINES LV_NUMBERING.
  LV_NUMBERING_C = LV_NUMBERING.

  IF W_TITLE IS INITIAL.
    CONCATENATE 'Display Data Processing Log'
                'Select Entries'
                LV_NUMBERING_C
      INTO W_TITLE
      SEPARATED BY SPACE.
  ENDIF.

  CREATE OBJECT CRV_PS
    EXPORTING IM_PS      = 'PS'                "PF-STATUS
              IM_IT_FUNC = IT_FUNC             "Excluding func
              IM_TB      = 'TB'                "TITLEBAR
              IM_TITLE   = W_TITLE.            "TITLE
  CLEAR IT_FUNC.

ENDFORM.                    " ps_tb
*&---------------------------------------------------------------------*
*&      Form  z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Z_FCA_EAI_INTERFACE_LOG.
  DATA: LV_TOTAL TYPE I.
  DESCRIBE TABLE IT_ZTMM_6022_01 LINES LV_TOTAL.

  CHECK NOT LV_TOTAL IS INITIAL.
  CLEAR: WA_ZTCA_IF_LOG.
  LOOP AT IT_ZTMM_6022_01 ASSIGNING <FS_ZTMM_6022_01>.
    IF <FS_ZTMM_6022_01>-ZZRET = 'S'.
      WA_ZTCA_IF_LOG-ZSUCC = WA_ZTCA_IF_LOG-ZSUCC + 1.
    ELSEIF <FS_ZTMM_6022_01>-ZZRET = 'E'.
      WA_ZTCA_IF_LOG-ERROR = WA_ZTCA_IF_LOG-ERROR + 1.
    ENDIF.
  ENDLOOP.

  WA_ZTCA_IF_LOG-TCODE = 'ZMMI75'. "Present Transaction Code
  WA_ZTCA_IF_LOG-TOTAL = LV_TOTAL. "Total Execution number
  WA_ZTCA_IF_LOG-ERDAT = SY-DATUM. "Created on.
  WA_ZTCA_IF_LOG-ERZET = SY-UNAME. "Created time.
  WA_ZTCA_IF_LOG-ERNAM = SY-UNAME. "Created by.
  CALL FUNCTION 'Z_FCA_EAI_INTERFACE_LOG'
    EXPORTING
      I_ZTCA_IF_LOG     = WA_ZTCA_IF_LOG
* IMPORTING
*   E_ZTCA_IF_LOG              =
   EXCEPTIONS
     UPDATE_FAILED              = 1
     NUMBER_RANGE_ERROR         = 2
     TCODE_DOES_NOT_EXIST       = 3
     OTHERS                     = 4.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " z_fca_eai_interface_log
*&---------------------------------------------------------------------*
*&      Form  separate_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MAT_INFO_MATNR  text
*      -->P_WA_MAT_INFO_MTART  text
*      <--P_W_MATNR  text
*----------------------------------------------------------------------*
FORM SEPARATE_COLOR USING    P_MATNR
                             P_MTART
                    CHANGING P_MATNR1.

  DATA: W_MLEN TYPE I,
        W_NCHAR TYPE I,
        W_COLOR(3) TYPE C.

  W_MLEN = STRLEN( P_MATNR ).
*  IF P_MATNR(1) NE 'Z'.
*    IF ( W_MLEN GT 11 AND P_MTART = 'ROH' AND
*                          P_MATNR(1) <> 'B' ).
*      W_NCHAR = W_MLEN - 10.
*      W_COLOR = P_MATNR+10(W_NCHAR).
*      P_MATNR1 = P_MATNR(10).
*    ELSE.
*      P_MATNR1 = P_MATNR.
*    ENDIF.
*  ELSE.
*    IF ( W_MLEN GT 12 AND P_MTART = 'ROH' AND
*                          P_MATNR(2) <> 'B' ).
*      W_NCHAR = W_MLEN - 11.
*      W_COLOR = P_MATNR+11(W_NCHAR).
*      P_MATNR1 = P_MATNR(11).
*    ELSE.
*      P_MATNR1 = P_MATNR.
*    ENDIF.
*  ENDIF.
*

  IF P_MATNR(1) NE 'Z'.
    IF P_MATNR+0(2) = 'CR' OR P_MATNR+0(2) = 'EM'.
      IF ( W_MLEN GT 13 AND P_MTART = 'ROH' AND
                            P_MATNR(1) <> 'B' ).
        W_NCHAR = W_MLEN - 12.
        W_COLOR = P_MATNR+12(W_NCHAR).
        P_MATNR1 = P_MATNR(12).
      ELSE.
        IF ( W_MLEN GT 11 AND P_MTART = 'ROH' AND
                             P_MATNR(1) <> 'B' ).
          IF  P_MATNR+5(2) <> 'M1'.
            W_NCHAR = W_MLEN - 10.
            W_COLOR = P_MATNR+10(W_NCHAR).
            P_MATNR1 = P_MATNR(10).
          ELSE.
            P_MATNR1 = P_MATNR.
          ENDIF.
        ELSE.
          P_MATNR1 = P_MATNR.
        ENDIF.
      ENDIF.
    ELSE.
      IF ( W_MLEN GT 11 AND P_MTART = 'ROH' AND
                               P_MATNR(1) <> 'B' ).
        W_NCHAR = W_MLEN - 10.
        W_COLOR = P_MATNR+10(W_NCHAR).
        P_MATNR1 = P_MATNR(10).
      ELSE.
        P_MATNR1 = P_MATNR.
      ENDIF.
    ENDIF.
  ELSE.
    IF ( W_MLEN GT 12 AND P_MTART = 'ROH' AND
                          P_MATNR(2) <> 'B' ).
      W_NCHAR = W_MLEN - 11.
      W_COLOR = P_MATNR+11(W_NCHAR).
      P_MATNR1 = P_MATNR(11).
    ELSE.
      P_MATNR1 = P_MATNR.
    ENDIF.
  ENDIF.

ENDFORM.                    " separate_color
*&---------------------------------------------------------------------*
*&      Form  collect_component_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLLECT_COMPONENT_DATA.

*  data: it_6022_tmp LIKE TABLE OF wa_6022_01.

  FIELD-SYMBOLS: <FS_COMP> LIKE LINE OF IT_COMPONENTS1,
                 <FS_HALB> LIKE LINE OF IT_HALB.
*                 <fs_stpox> LIKE LINE OF it_filter_stpox,
*                 <fs_6022> LIKE LINE OF it_6022_tmp.

  CLEAR: WA_6022_TMP.

  R_MTART-SIGN = 'E'.
  R_MTART-OPTION = 'EQ'.
  R_MTART-LOW = 'ROH'.
  APPEND R_MTART.
  R_MTART-LOW = 'ROH1'.
  APPEND R_MTART.

  SORT IT_COMPONENTS1 BY MATERIAL.

  LOOP AT IT_COMPONENTS1 ASSIGNING <FS_COMP>.
    READ TABLE IT_ZTMM_6022_01 INTO WA_6022_TMP
                           WITH KEY MATNR = <FS_COMP>-MATERIAL.
    IF SY-SUBRC NE 0.
      READ TABLE IT_MAT_INFO INTO WA_MAT_INFO
                             WITH KEY MATNR = <FS_COMP>-MATERIAL.

** CHANGED BY FURONG ON 09/14/2006
      IF WA_MAT_INFO-MTART EQ 'ROH' OR  WA_MAT_INFO-MTART EQ 'ROH1'.
        WA_6022_01-MATNR = <FS_COMP>-MATERIAL.
        WA_6022_01-MAKTX = <FS_COMP>-MATL_DESC.
        WA_6022_01-MEINS = <FS_COMP>-BASE_UOM.
        WA_6022_01-LBKUM = <FS_COMP>-REQ_QUAN + WA_MAT_INFO-LBKUM.
        WA_6022_01-PROFL = WA_MAT_INFO-PROFL.
        WA_6022_01-MTART = WA_MAT_INFO-MTART.
        WA_6022_01-PTC   = 'PC'.
        COLLECT WA_6022_01 INTO IT_ZTMM_6022_01.
        CLEAR: WA_MAT_INFO, WA_6022_01,WA_6022_TMP.
      ELSE.
        WA_HALB-MATNR = <FS_COMP>-MATERIAL.
        WA_HALB-WERKS = <FS_COMP>-PLANT.
        WA_HALB-DATUV = <FS_COMP>-REQ_DATE.
        WA_HALB-BDMNG = <FS_COMP>-ENTRY_QTY.
        COLLECT WA_HALB INTO IT_HALB.
        <FS_COMP>-PHANT_ITEM = 'X'.
      ENDIF.

*      IF wa_mat_info-mtart EQ 'HALB'.
*        wa_halb-matnr = <fs_comp>-material.
*        wa_halb-werks = <fs_comp>-plant.
*        wa_halb-datuv = <fs_comp>-req_date.
*        wa_halb-bdmng = <fs_comp>-entry_qty.
*        COLLECT wa_halb INTO it_halb.
*        <fs_comp>-phant_item = 'X'.
*      ELSE.
*        wa_6022_01-matnr = <fs_comp>-material.
*        wa_6022_01-maktx = <fs_comp>-matl_desc.
*        wa_6022_01-meins = <fs_comp>-base_uom.
*        wa_6022_01-lbkum = <fs_comp>-req_quan + wa_mat_info-lbkum.
*        wa_6022_01-profl = wa_mat_info-profl.
*        wa_6022_01-mtart = wa_mat_info-mtart.
*        wa_6022_01-ptc   = 'PC'.
*        COLLECT wa_6022_01 INTO it_ztmm_6022_01.
*        CLEAR: wa_mat_info, wa_6022_01,wa_6022_tmp.
*      ENDIF.
** END OF CHANGE
    ELSE.
      WA_6022_01 = WA_6022_TMP.
      WA_6022_01-LBKUM = <FS_COMP>-REQ_QUAN.
      COLLECT WA_6022_01 INTO IT_ZTMM_6022_01.
      CLEAR: WA_MAT_INFO, WA_6022_01, WA_6022_TMP.
    ENDIF.
  ENDLOOP.

  CLEAR: IT_COMPONENTS1, IT_COMPONENTS1[].
*  DELETE it_components1 WHERE phant_item = 'X'.

ENDFORM.                    " collect_component_data
