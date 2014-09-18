************************************************************************
* Program Name      : ZPPR_DELAY_VEH_CREATTION
* Creation Date     : 03/2013
* Development Request No :
* Addl Documentation:
* Description       :
* Modification Logs
* Date            Developer        RequestNo      Description
*
************************************************************************

REPORT ZPPR_DELAY_VEH_CREATION NO STANDARD PAGE HEADING
                     LINE-SIZE 132
                     LINE-COUNT 64(1)
                     MESSAGE-ID ZMPP.

TABLES: ZTPP_DELAY_VEH, ZTPP_DELAY_SECT, T001W.

DATA: BEGIN OF IT_SECTION OCCURS 0.
        INCLUDE STRUCTURE ZTPP_DELAY_SECT.
DATA:   LIMIT_TIME(14),
      END   OF IT_SECTION.

DATA: IT_DELAY      LIKE ZTPP_DELAY_VEH OCCURS 0 WITH HEADER LINE,
      IT_SYS_FDBACK LIKE ZTPP_DELAY_IF  OCCURS 0 WITH HEADER LINE,
      IT_DELAY_IF   LIKE ZTPP_DELAY_IF  OCCURS 0 WITH HEADER LINE,
      IT_DELAY_TEMP LIKE ZTPP_DELAY_VEH OCCURS 0 WITH HEADER LINE,
      IT_DELAY_PREV LIKE ZTPP_DELAY_VEH OCCURS 0 WITH HEADER LINE,
      IT_UM         LIKE ZTSD_UM        OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF IT_WIP OCCURS 0,
        OBJEK         LIKE   AUSP-OBJEK,
        KLART         LIKE   AUSP-KLART,
        RPID          LIKE   AUSP-ATWRT,
        USAGE         LIKE   AUSP-ATWRT,
        WORDER        LIKE   AUSP-ATWRT,
        MI            LIKE   AUSP-ATWRT,
        OCN           LIKE   AUSP-ATWRT,
        DEALER_NO     LIKE ZTPP_VM-DEALER_NO,
        RP21_ATFLV    LIKE AUSP-ATFLV,
        ALLOC_DATE    LIKE SY-DATUM,
        ATINN_INPUT   LIKE   AUSP-ATINN,
        ZITIME(14),
        ZSECID        LIKE   ZTPP_DELAY_SECT-ZSECID,
        SEQ           LIKE   ZTPP_DELAY_SECT-SEQ,
      END   OF IT_WIP.

DATA: BEGIN OF IT_WORK_TIME OCCURS 0,
        SHOP   TYPE   ARBPL.
        INCLUDE STRUCTURE ZSMM_WORKING_TIME.
DATA: END   OF IT_WORK_TIME.

DATA: W_SYS_DATE LIKE SY-DATUM,
      W_SYS_TIME LIKE SY-UZEIT,
      W_DATE LIKE SY-DATUM,
      W_CURR_TIME(14),
      W_FLAG(1).

DATA: V_RP_STATUS LIKE CABN-ATINN,
      V_USAGE_CAR LIKE CABN-ATINN,
      V_WORDER    LIKE CABN-ATINN,
      V_MI        LIKE CABN-ATINN,
      V_OCN       LIKE CABN-ATINN,
      V_DEALER_NO LIKE CABN-ATINN,
      V_RP21      LIKE CABN-ATINN,
      V_EXTC      LIKE CABN-ATINN,
      V_INTC      LIKE CABN-ATINN.

DATA: V_BDATE_B      LIKE   SY-DATUM,
      V_BDATE_P      LIKE   SY-DATUM,
      V_BDATE_T      LIKE   SY-DATUM.

CONSTANTS: C_RP_STATUS LIKE CABN-ATNAM  VALUE 'P_RP_STATUS',
           C_USAGE_CAR LIKE CABN-ATNAM  VALUE 'P_USAGE_CAR',
           C_WORDER    LIKE CABN-ATNAM  VALUE 'P_WORK_ORDER',
           C_MI        LIKE CABN-ATNAM  VALUE 'P_MI',
           C_OCN       LIKE CABN-ATNAM  VALUE 'P_OCN',
           C_DEALER_NO LIKE CABN-ATNAM  VALUE 'P_DEALER_NO',
           C_RP21      LIKE CABN-ATNAM  VALUE 'P_RP21_SHOP_DATE',
           C_EXTC      LIKE CABN-ATNAM  VALUE 'P_EXT_COLOR',
           C_INTC      LIKE CABN-ATNAM  VALUE 'P_INT_COLOR',
           C_WERKS     LIKE T001W-WERKS VALUE 'P001'.

SELECT-OPTIONS: S_ZSECID FOR ZTPP_DELAY_SECT-ZSECID.
PARAMETERS: P_WERKS LIKE T001W-WERKS DEFAULT 'P001' OBLIGATORY.

INITIALIZATION.
  PERFORM INI_DATA.

START-OF-SELECTION.
  CLEAR: W_FLAG.
  PERFORM LOCKING_RTN USING SY-REPID W_FLAG.

  IF W_FLAG = 'E'.
*    PERFORM dequeue_prg.
    MESSAGE E001 WITH TEXT-M03.
  ENDIF.

** Changed by Park On 11/22/13
*  PERFORM CHECK_EXIST_DELAY_VEH USING W_FLAG.
*  IF W_FLAG = 'E'.
*    MESSAGE E001 WITH TEXT-M04.
*  ENDIF.
** End of change 11/22/13

  PERFORM READ_MASTER.
  PERFORM READ_WIP.
  IF IT_WIP[] IS INITIAL.
    MESSAGE E001 WITH TEXT-M05.
  ENDIF.
  PERFORM READ_PREVIOUS_DELAY_VEH.
  PERFORM READ_DELAY_TEMP.
  PERFORM GET_DELAY_VEHICLE.
  PERFORM GET_SYSTEM_FEEDBACK.
  PERFORM GET_SIDE_TRACK.
*  PERFORM get_delay_hour.
  PERFORM UPDATE_OTHERS.
  PERFORM MERGE_DELAY_TEMP.
** Changed by Park On 11/22/13
*  PERFORM APPEND_DELAY_TEMP.
** End of change 11/22/13
  PERFORM UPDATE_FROM_UM.
** Changed by Park On 11/22/13
*  PERFORM GET_ETA_FROM_OPEN_SIDE_TRACK.
** End of change 11/22/13
  PERFORM UPDATE_TABLE.
  PERFORM UPDATE_OPEN_SIDE_TRACK.
*  PERFORM dequeue_prg.
*&---------------------------------------------------------------------*
*&      Form  READ_MASTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_MASTER .
  DATA: L_INDEX(2) TYPE N.

  SELECT * INTO TABLE IT_SECTION
    FROM ZTPP_DELAY_SECT.

  SORT IT_SECTION BY ZRPFR ZRPTO ZSECID SEQ.

  LOOP AT IT_SECTION.
    PERFORM READ_LIMIT_TIME.

    MODIFY IT_SECTION.
  ENDLOOP.

  SELECT SINGLE ATINN INTO V_RP_STATUS
    FROM CABN WHERE ATNAM = C_RP_STATUS.

  SELECT SINGLE ATINN INTO V_USAGE_CAR
    FROM CABN WHERE ATNAM = C_USAGE_CAR.

  SELECT SINGLE ATINN INTO V_WORDER
    FROM CABN WHERE ATNAM = C_WORDER.

  SELECT SINGLE ATINN INTO V_MI
    FROM CABN WHERE ATNAM = C_MI.

  SELECT SINGLE ATINN INTO V_OCN
    FROM CABN WHERE ATNAM = C_OCN.

  SELECT SINGLE ATINN INTO V_DEALER_NO
   FROM CABN WHERE ATNAM = C_DEALER_NO.

  SELECT SINGLE ATINN INTO V_RP21
    FROM CABN WHERE ATNAM = C_RP21.

  SELECT SINGLE ATINN INTO V_EXTC
    FROM CABN WHERE ATNAM = C_EXTC.

  SELECT SINGLE ATINN INTO V_INTC
    FROM CABN WHERE ATNAM = C_INTC.
ENDFORM.                    " READ_MASTER
*&---------------------------------------------------------------------*
*&      Form  READ_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WIP .
  DATA: LT_TIME LIKE AUSP OCCURS 0 WITH HEADER LINE.

  DATA: L_ACTUAL TYPE STRING,
        L_INDEX LIKE SY-TABIX,
        L_TEMP_DATE LIKE SY-DATUM,
        L_DATUM(8) TYPE N.

  RANGES: R_USAGE FOR AUSP-ATWRT.

  SELECT A~OBJEK A~KLART A~ATWRT AS RPID
         B~ATWRT AS USAGE
         C~ATWRT AS WORDER
         D~ATWRT AS MI E~ATWRT AS OCN
         F~ATWRT AS DEALER_NO
         G~ATFLV AS RP21_ATFLV
         H~ATWRT AS EXTC
         I~ATWRT AS INTC
    INTO CORRESPONDING FIELDS OF TABLE IT_WIP
    FROM AUSP AS A LEFT OUTER JOIN AUSP AS B
                     ON B~OBJEK = A~OBJEK
                    AND B~ATINN = V_USAGE_CAR
                    AND B~MAFID = A~MAFID
                    AND B~KLART = A~KLART
                   LEFT OUTER JOIN AUSP AS C
                     ON C~OBJEK = A~OBJEK
                    AND C~ATINN = V_WORDER
                    AND C~MAFID = A~MAFID
                    AND C~KLART = A~KLART
                   LEFT OUTER JOIN AUSP AS D
                     ON D~OBJEK = A~OBJEK
                    AND D~ATINN = V_MI
                    AND D~MAFID = A~MAFID
                    AND D~KLART = A~KLART
                   LEFT OUTER JOIN AUSP AS E
                     ON E~OBJEK = A~OBJEK
                    AND E~ATINN = V_OCN
                    AND E~MAFID = A~MAFID
                    AND E~KLART = A~KLART
                   LEFT OUTER JOIN AUSP AS F
                     ON F~OBJEK = A~OBJEK
                    AND F~ATINN = V_DEALER_NO
                    AND F~MAFID = A~MAFID
                    AND F~KLART = A~KLART
                   LEFT OUTER JOIN AUSP AS G
                     ON G~OBJEK = A~OBJEK
                    AND G~ATINN = V_RP21
                    AND G~MAFID = A~MAFID
                    AND G~KLART = A~KLART
                   LEFT OUTER JOIN AUSP AS H
                     ON H~OBJEK = A~OBJEK
                    AND H~ATINN = V_EXTC
                    AND H~MAFID = A~MAFID
                    AND H~KLART = A~KLART
                   LEFT OUTER JOIN AUSP AS I
                     ON I~OBJEK = A~OBJEK
                    AND I~ATINN = V_INTC
                    AND I~MAFID = A~MAFID
                    AND I~KLART = A~KLART
   WHERE A~KLART EQ '002'
     AND A~ATINN EQ V_RP_STATUS
     AND A~ATWRT IN ('01','02','03','04','05',
                     '06','07','08','09','10',
                     '11','12','13','14','15',
                     '16','17','18','19','20',
                     '21','22','23','24','26').

  R_USAGE-OPTION = 'EQ'.
  R_USAGE-SIGN = 'I'.
  R_USAGE-LOW = 'D'.
  APPEND R_USAGE.
  R_USAGE-LOW = 'S'.
  APPEND R_USAGE.
  R_USAGE-LOW = '2'.
  APPEND R_USAGE.

  DELETE IT_WIP WHERE USAGE = ' '.
  DELETE IT_WIP WHERE USAGE IN R_USAGE.
*    delete it_wip WHERE usage IN ('D','S','2',' ').

*  DELETE it_wip WHERE NOT objek EQ 'INF678936'.
*  DELETE it_wip WHERE NOT ( objek EQ 'INF689936' OR
*                          objek EQ 'INF102031').

  LOOP AT IT_SECTION.
    CONCATENATE 'P_RP' IT_SECTION-ZRPFR '_ACTUAL_DATE' INTO L_ACTUAL.

    IT_WIP-ZSECID = IT_SECTION-ZSECID.
    IT_WIP-SEQ    = IT_SECTION-SEQ.
    SELECT SINGLE ATINN INTO IT_WIP-ATINN_INPUT
      FROM CABN
     WHERE ATNAM = L_ACTUAL.

    CASE IT_SECTION-ZSHIP_FLG.
      WHEN SPACE.
        MODIFY IT_WIP TRANSPORTING ATINN_INPUT ZSECID SEQ
                      WHERE RPID >= IT_SECTION-ZRPFR
                        AND RPID <= IT_SECTION-ZRPTO.
      WHEN 'X'.
        CASE IT_SECTION-ZOPER.
          WHEN '='.
            MODIFY IT_WIP TRANSPORTING ATINN_INPUT ZSECID SEQ
                          WHERE RPID >= IT_SECTION-ZRPFR
                            AND RPID <= IT_SECTION-ZRPTO
                            AND WORDER+9(3) = 'B28'.
          WHEN '<>'.
            MODIFY IT_WIP TRANSPORTING ATINN_INPUT ZSECID SEQ
                          WHERE RPID >= IT_SECTION-ZRPFR
                            AND RPID <= IT_SECTION-ZRPTO
                            AND WORDER+9(3) <> 'B28'.
        ENDCASE.
    ENDCASE.
  ENDLOOP.

  CHECK IT_WIP[] IS NOT INITIAL.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_TIME
    FROM AUSP
     FOR ALL ENTRIES IN IT_WIP
   WHERE OBJEK = IT_WIP-OBJEK
     AND ATINN = IT_WIP-ATINN_INPUT
     AND KLART = IT_WIP-KLART.

  SORT LT_TIME BY OBJEK.

  LOOP AT IT_WIP.
    L_INDEX = SY-TABIX.
    CASE IT_WIP-WORDER+12(2).
      WHEN 'XX'.         "BIW
        IF IT_WIP-RPID > '01'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
      WHEN 'XY'.         "BIP
        IF IT_WIP-RPID > '05'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
      WHEN 'XA'.         "Pilot Car
        IF IT_WIP-RPID > '17'.
          DELETE IT_WIP. CONTINUE.
        ENDIF.
    ENDCASE.

    IT_WIP-ALLOC_DATE = L_DATUM = IT_WIP-RP21_ATFLV.

    READ TABLE LT_TIME WITH KEY OBJEK = IT_WIP-OBJEK
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE: LT_TIME-ATWRT TO IT_WIP-ZITIME.
      MODIFY IT_WIP INDEX L_INDEX.
    ELSE.
      L_TEMP_DATE = W_SYS_DATE - 180.
      CONCATENATE L_TEMP_DATE W_SYS_TIME INTO IT_WIP-ZITIME.
      MODIFY IT_WIP INDEX L_INDEX.
*      DELETE it_wip INDEX l_index.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_WIP
*&---------------------------------------------------------------------*
*&      Form  GET_DELAY_VEHICLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DELAY_VEHICLE.

  DATA: L_DATE LIKE SY-DATUM,
        L_TIME LIKE SY-UZEIT,
        L_WDATE LIKE SY-DATUM,
        L_WTIME LIKE SY-UZEIT,
        L_WKTIME TYPE I,
        L_ARBPL LIKE CRHD-ARBPL,
        L_TARGET_TIME(14),
        L_CURRENT_TIME(14),
        L_INDEX LIKE SY-TABIX.

  DATA: BEGIN OF LT_SIDE_TRACK OCCURS 0,
          MODEL_CODE LIKE ZTPP_SIDE_TRACK-MODEL_CODE,
          BODY_NO    LIKE ZTPP_SIDE_TRACK-BODY_NO,
          ZSTTXT     LIKE ZTPP_SIDE_TRACK-ZSTTXT,
          ZSEQ       LIKE ZTPP_SIDE_TRACK-ZSEQ,
        END OF LT_SIDE_TRACK.

  PERFORM READ_WORKING_TIME.

  DELETE IT_WIP WHERE ZSECID IS INITIAL.

  LOOP AT IT_WIP.
    CLEAR: IT_DELAY, L_TARGET_TIME, L_INDEX.
    CLEAR: IT_DELAY, L_TARGET_TIME.

    L_INDEX = SY-TABIX.

    READ TABLE IT_SECTION WITH KEY ZSECID =  IT_WIP-ZSECID
                                   SEQ    =  IT_WIP-SEQ.
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.

    L_DATE   = IT_WIP-ZITIME+0(8).
    L_TIME   = IT_WIP-ZITIME+8(6).
    L_WKTIME = IT_SECTION-ZDURA * 60.
    L_ARBPL  = IT_SECTION-ZSUPH.
*    PERFORM get_target_time USING l_date l_time l_wktime l_arbpl
*          CHANGING l_target_time.
*
*    IF l_target_time >= w_curr_time.
    IF IT_WIP-ZITIME >= IT_SECTION-LIMIT_TIME.
** Changed by Park On 11/22/13
*      DELETE IT_WIP INDEX L_INDEX.
      CLEAR: LT_SIDE_TRACK, LT_SIDE_TRACK.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE LT_SIDE_TRACK
        FROM ZTPP_SIDE_TRACK
       WHERE MODEL_CODE = IT_WIP-OBJEK(3)
         AND BODY_NO    = IT_WIP-OBJEK+3(6)
         AND ST_STATUS  = 'I'.
      IF SY-SUBRC = 0.
        IT_DELAY-DATUM      = W_DATE.
        IT_DELAY-ZSECID     = IT_SECTION-ZSECID.
        IT_DELAY-SEQ        = IT_SECTION-SEQ.
        IT_DELAY-MODEL_CODE = IT_WIP-OBJEK(3).
        IT_DELAY-BODY_NO    = IT_WIP-OBJEK+3(6).
        IT_DELAY-ZRPFR      = IT_SECTION-ZRPFR.
        IT_DELAY-ZITIME     = IT_WIP-ZITIME.
        IT_DELAY-ZRPTO      = IT_SECTION-ZRPTO.
        IT_DELAY-ZTTIME     = L_TARGET_TIME.
        IT_DELAY-WORDER     = IT_WIP-WORDER.
        IT_DELAY-MI         = IT_WIP-MI.
        IT_DELAY-OCN        = IT_WIP-OCN.
        IT_DELAY-RP_STATUS  = IT_WIP-RPID.
        IT_DELAY-DEALER_NO  = IT_WIP-DEALER_NO.
        IT_DELAY-ALLOC_DATE = IT_WIP-ALLOC_DATE.
        IT_DELAY-ERNAM      = SY-UNAME.
        IT_DELAY-ERDAT      = W_SYS_DATE.
        IT_DELAY-ERZET      = W_SYS_TIME.
        IT_DELAY-AENAM      = SY-UNAME.
        IT_DELAY-AEDAT      = W_SYS_DATE.
        IT_DELAY-AEZET      = W_SYS_TIME.

        SORT LT_SIDE_TRACK BY MODEL_CODE BODY_NO ZSEQ DESCENDING.
        READ TABLE LT_SIDE_TRACK INDEX 1.
        IT_DELAY-ZSTTXT     = LT_SIDE_TRACK-ZSTTXT.
        APPEND IT_DELAY.
      ELSE.
        DELETE IT_WIP INDEX L_INDEX.
      ENDIF.
** End of change 11/22/13

    ELSE.
      IT_DELAY-DATUM      = W_DATE.
      IT_DELAY-ZSECID     = IT_SECTION-ZSECID.
      IT_DELAY-SEQ        = IT_SECTION-SEQ.
      IT_DELAY-MODEL_CODE = IT_WIP-OBJEK(3).
      IT_DELAY-BODY_NO    = IT_WIP-OBJEK+3(6).
      IT_DELAY-ZRPFR      = IT_SECTION-ZRPFR.
      IT_DELAY-ZITIME     = IT_WIP-ZITIME.
      IT_DELAY-ZRPTO      = IT_SECTION-ZRPTO.
      IT_DELAY-ZTTIME     = L_TARGET_TIME.
      IT_DELAY-WORDER     = IT_WIP-WORDER.
      IT_DELAY-MI         = IT_WIP-MI.
      IT_DELAY-OCN        = IT_WIP-OCN.
      IT_DELAY-RP_STATUS  = IT_WIP-RPID.
      IT_DELAY-DEALER_NO  = IT_WIP-DEALER_NO.
      IT_DELAY-ALLOC_DATE = IT_WIP-ALLOC_DATE.
      IT_DELAY-ERNAM      = SY-UNAME.
      IT_DELAY-ERDAT      = W_SYS_DATE.
      IT_DELAY-ERZET      = W_SYS_TIME.
      IT_DELAY-AENAM      = SY-UNAME.
      IT_DELAY-AEDAT      = W_SYS_DATE.
      IT_DELAY-AEZET      = W_SYS_TIME.

      PERFORM GET_DELAY_HOUR USING L_ARBPL
              IT_DELAY-ZITIME IT_DELAY-ZDHOUR.

      IF IT_DELAY-ZDHOUR <= IT_SECTION-ZDURA.
** Changed by Park On 11/22/13
*        CONTINUE.
        CLEAR: LT_SIDE_TRACK, LT_SIDE_TRACK.
        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE LT_SIDE_TRACK
          FROM ZTPP_SIDE_TRACK
         WHERE MODEL_CODE = IT_WIP-OBJEK(3)
           AND BODY_NO    = IT_WIP-OBJEK+3(6)
           AND ST_STATUS  = 'I'.
        IF SY-SUBRC = 0.
          SORT LT_SIDE_TRACK BY MODEL_CODE BODY_NO ZSEQ DESCENDING.
          READ TABLE LT_SIDE_TRACK INDEX 1.
          IT_DELAY-ZSTTXT  = LT_SIDE_TRACK-ZSTTXT.
        ELSE.
          CONTINUE.
        ENDIF.
** End of change 11/22/13
      ENDIF.
      APPEND IT_DELAY.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_DELAY_VEHICLE
*&---------------------------------------------------------------------*
*&      Form  GET_TARGET_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_TARGET_TIME  text
*----------------------------------------------------------------------*
FORM GET_TARGET_TIME USING P_DATE P_TIME P_WKTIME P_ARBPL
      CHANGING P_TARGET_TIME.

  DATA:   L_WDATE LIKE SY-DATUM,
          L_WTIME LIKE SY-UZEIT,
          L_WDATE_C8(8).

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      C_DATE                   = P_DATE
      C_TIME                   = P_TIME
      OPCODE                   = '+'
      WKTIME                   = P_WKTIME
      WERKS                    = P_WERKS
      ARBPL                    = P_ARBPL
    IMPORTING
      T_DATE                   = L_WDATE
      T_TIME                   = L_WTIME
    EXCEPTIONS
      ERROR_OPERATION          = 1
      CANNOT_READ_WORKING_TIME = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH 'Getting work date failed ' IT_WIP-ZSECID
            IT_WIP-OBJEK.
  ENDIF.

  L_WDATE_C8 = L_WDATE.
  CONCATENATE L_WDATE_C8 L_WTIME INTO P_TARGET_TIME.
ENDFORM.                    " GET_TARGET_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_SYSTEM_FEEDBACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SYSTEM_FEEDBACK .
  DATA: L_INDEX LIKE SY-TABIX.

  REFRESH IT_SYS_FDBACK.
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_SYS_FDBACK
    FROM ZTPP_DELAY_IF
     FOR ALL ENTRIES IN IT_DELAY
   WHERE MODEL_CODE	= IT_DELAY-MODEL_CODE
     AND BODY_NO    = IT_DELAY-BODY_NO.
*     AND rp_status >= it_delay-zrpfr
*     AND rp_status <= it_delay-zrpto.

*  SORT it_delay BY model_code body_no.
  SORT IT_SYS_FDBACK BY MODEL_CODE BODY_NO ZLDATIM DESCENDING .
  LOOP AT IT_DELAY.
    L_INDEX = SY-TABIX.
    READ TABLE IT_SYS_FDBACK WITH KEY MODEL_CODE = IT_DELAY-MODEL_CODE
                                      BODY_NO    = IT_DELAY-BODY_NO
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_DELAY-ZLFDBK  = IT_SYS_FDBACK-ZRSN.
      IT_DELAY-ZSYSTEM = IT_SYS_FDBACK-ZSYSTEM.
      MODIFY IT_DELAY INDEX L_INDEX TRANSPORTING ZLFDBK ZSYSTEM.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_SYSTEM_FEEDBACK
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.

** Changed by Park On 11/22/13
*  IF NOT IT_DELAY_TEMP[] IS INITIAL.
*    DELETE ZTPP_DELAY_VEH FROM TABLE IT_DELAY_TEMP.
*  ENDIF.
*  DELETE FROM ZTPP_DELAY_VEH WHERE DATUM = W_DATE.
*  INSERT ZTPP_DELAY_VEH FROM TABLE IT_DELAY ACCEPTING DUPLICATE KEYS.

  MODIFY ZTPP_DELAY_VEH FROM TABLE IT_DELAY.
** End of change 11/22/13

  IF SY-SUBRC = 0.
    COMMIT WORK.
  ELSE.
    MESSAGE E009 WITH 'Table ZTPP_DELAY_VEH deletion error'.
    ROLLBACK WORK.
  ENDIF.

ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  INI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INI_DATA .

  W_SYS_DATE = SY-DATUM.
  W_SYS_TIME = SY-UZEIT.

  PERFORM WORKING_SHOP_DATE.

  CONCATENATE W_SYS_DATE W_SYS_TIME INTO W_CURR_TIME.

ENDFORM.                    " INI_DATA
*&---------------------------------------------------------------------*
*&      Form  LOCKING_RTN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_REPID  text
*----------------------------------------------------------------------*
FORM LOCKING_RTN USING P_REPID P_RESULT.
  PERFORM CHECK_LOCK_OBJECT  USING P_REPID P_RESULT.
  PERFORM CHECK_ENQUEUE_READ USING P_REPID P_RESULT.
  PERFORM CHECK_BATCHJOB     USING P_REPID P_RESULT.
ENDFORM.                    " LOCKING_RTN
**&---------------------------------------------------------------------
*
*&      Form  CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_REPID  text
*----------------------------------------------------------------------*
FORM CHECK_LOCK_OBJECT USING P_REPID P_RESULT.
  CALL FUNCTION 'ENQUEUE_EPROG'
    EXPORTING
      MODE_TRDIR     = 'E'
      PROGRAMM       = P_REPID
      X_PROGRAMM     = ' '
      _SCOPE         = '1'
      _WAIT          = ' '
      _COLLECT       = ' '
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MOVE: 'E' TO P_RESULT.
  ENDIF.
ENDFORM.                    " CHECK_LOCK_OBJECT
*&---------------------------------------------------------------------*
*&      Form  CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_REPID  text
*----------------------------------------------------------------------*
FORM CHECK_ENQUEUE_READ USING P_REPID  P_RESULT.
  DATA: L_GARG        LIKE SEQG3-GARG,
        L_GNAME       LIKE SEQG3-GNAME,
        L_LOCK_NUMBER LIKE SY-TABIX.

  DATA: LT_LOCK TYPE TABLE OF SEQG3 WITH HEADER LINE.

  MOVE: SY-MANDT     TO L_GARG(3),
        P_REPID      TO L_GARG+3,
        P_REPID      TO L_GNAME.

  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      GCLIENT               = SY-MANDT
      GNAME                 = L_GNAME
      GARG                  = L_GARG
      GUNAME                = ' '
      LOCAL                 = ' '
      FAST                  = ' '
    IMPORTING
      NUMBER                = L_LOCK_NUMBER
    TABLES
      ENQ                   = LT_LOCK
    EXCEPTIONS
      COMMUNICATION_FAILURE = 1
      SYSTEM_FAILURE        = 2
      OTHERS                = 3.
  IF SY-SUBRC <> 0.
    P_RESULT = 'E'.
    EXIT.
  ENDIF.

  IF L_LOCK_NUMBER > 1.
    P_RESULT = 'E'.
  ENDIF.
ENDFORM.                    " CHECK_ENQUEUE_READ
*&---------------------------------------------------------------------*
*&      Form  CHECK_BATCHJOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BATCHJOB USING P_REPID  P_RESULT.
  DATA: LT_JOBLIST LIKE TBTCJOB OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
    EXPORTING
      ABAP_PROGRAM_NAME             = P_REPID
      DIALOG                        = 'N'
      STATUS                        = 'R'
    TABLES
      JOBLIST                       = LT_JOBLIST
    EXCEPTIONS
      NO_JOBS_FOUND                 = 1
      PROGRAM_SPECIFICATION_MISSING = 2
      INVALID_DIALOG_TYPE           = 3
      JOB_FIND_CANCELED             = 4
      OTHERS                        = 5.

  IF SY-BATCH EQ 'X'.
    READ TABLE LT_JOBLIST INDEX 2.
    IF SY-SUBRC EQ 0.
      P_RESULT = 'E'.
    ENDIF.
  ELSE.
    READ TABLE LT_JOBLIST INDEX 1.
    IF SY-SUBRC EQ 0.
      P_RESULT = 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    " CHECK_BATCHJOB
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXIST_DELAY_VEH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_EXIST_DELAY_VEH USING P_FLAG.

  SELECT *
    FROM ZTPP_DELAY_VEH
    WHERE DATUM = W_DATE
      AND ZTEMP = ' '.
    IF SY-SUBRC = 0.
      P_FLAG = 'E'.
      EXIT.
    ENDIF.
  ENDSELECT.

ENDFORM.                    " CHECK_EXIST_DELAY_VEH
*&---------------------------------------------------------------------*
*&      Form  READ_DELAY_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DELAY_TEMP .

** Changed by Park On 11/22/13
*  SELECT * INTO TABLE IT_DELAY_TEMP
*    FROM ZTPP_DELAY_VEH
*   WHERE ZTEMP = 'X'
*     AND DATUM = W_DATE.

  SELECT * INTO TABLE IT_DELAY_TEMP
    FROM ZTPP_DELAY_VEH
   WHERE DATUM = W_DATE
     AND ZTEMP = ' '.
** End of change 11/22/13

ENDFORM.                    " READ_DELAY_TEMP
*&---------------------------------------------------------------------*
*&      Form  MERGE_DELAY_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MERGE_DELAY_TEMP .

  DATA: L_INDEX LIKE SY-TABIX.
  SORT IT_DELAY_TEMP BY ZSECID SEQ MODEL_CODE BODY_NO.
  LOOP AT IT_DELAY.
    L_INDEX = SY-TABIX.
    READ TABLE IT_DELAY_TEMP WITH KEY ZSECID     = IT_DELAY-ZSECID
                                      SEQ        = IT_DELAY-SEQ
                                      MODEL_CODE = IT_DELAY-MODEL_CODE
                                      BODY_NO    = IT_DELAY-BODY_NO
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_DELAY-ZETA   = IT_DELAY_TEMP-ZETA.
      IT_DELAY-ZPFDBK = IT_DELAY_TEMP-ZPFDBK.
      IT_DELAY-ZSFDBK = IT_DELAY_TEMP-ZSFDBK.
      IT_DELAY-ERNAM  = IT_DELAY_TEMP-ERNAM.
      IT_DELAY-ERDAT  = IT_DELAY_TEMP-ERDAT.
      IT_DELAY-ERZET  = IT_DELAY_TEMP-ERZET.

      CLEAR: IT_DELAY-ZESOFF.
      IF IT_DELAY-ZETA IS NOT INITIAL.
        READ TABLE IT_SECTION WITH KEY ZSECID = IT_DELAY-ZSECID
                                       SEQ    = IT_DELAY-SEQ.
        IF SY-SUBRC NE 0.
          MESSAGE E000 WITH TEXT-M00.
        ENDIF.

        PERFORM GET_ESTIMATED_SOFF USING IT_DELAY-ZETA
                                         IT_SECTION-ZDRDY
                                         IT_DELAY-ZESOFF.
      ENDIF.

      MODIFY IT_DELAY.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MERGE_DELAY_TEMP
*&---------------------------------------------------------------------*
*&      Form  GET_SIDE_TRACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_SIDE_TRACK .
  DATA: L_INDEX LIKE SY-TABIX.

  DATA: BEGIN OF LT_SIDE_TRACK OCCURS 0,
        MODEL_CODE LIKE ZTPP_SIDE_TRACK-MODEL_CODE,
        BODY_NO LIKE ZTPP_SIDE_TRACK-BODY_NO,
        ZSTTXT LIKE ZTPP_SIDE_TRACK-ZSTTXT,
        ZSEQ LIKE ZTPP_SIDE_TRACK-ZSEQ,
        END OF LT_SIDE_TRACK.

  SELECT MODEL_CODE BODY_NO ZSTTXT ZSEQ
    INTO TABLE LT_SIDE_TRACK
    FROM ZTPP_SIDE_TRACK
     FOR ALL ENTRIES IN IT_DELAY
   WHERE MODEL_CODE	= IT_DELAY-MODEL_CODE
     AND BODY_NO  = IT_DELAY-BODY_NO.

  SORT LT_SIDE_TRACK BY MODEL_CODE BODY_NO ZSEQ DESCENDING.
  LOOP AT IT_DELAY.
    L_INDEX = SY-TABIX.
    READ TABLE LT_SIDE_TRACK WITH KEY MODEL_CODE = IT_DELAY-MODEL_CODE
                                      BODY_NO = IT_DELAY-BODY_NO
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      IT_DELAY-ZSTTXT = LT_SIDE_TRACK-ZSTTXT.
      MODIFY IT_DELAY INDEX L_INDEX TRANSPORTING ZSTTXT.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_SIDE_TRACK
*&---------------------------------------------------------------------*
*&      Form  UPDATE_OTHERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_OTHERS.

  SORT IT_DELAY_PREV BY ZSECID SEQ MODEL_CODE BODY_NO.

  LOOP AT IT_DELAY.
    READ TABLE IT_DELAY_PREV WITH KEY ZSECID     = IT_DELAY-ZSECID
                                      SEQ        = IT_DELAY-SEQ
                                      MODEL_CODE = IT_DELAY-MODEL_CODE
                                      BODY_NO    = IT_DELAY-BODY_NO
                             BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IT_DELAY-ZETA   = IT_DELAY_PREV-ZETA.
      IT_DELAY-ZPFDBK = IT_DELAY_PREV-ZPFDBK.
      IT_DELAY-ZSFDBK = IT_DELAY_PREV-ZSFDBK.

      CLEAR: IT_DELAY-ZESOFF.
      IF IT_DELAY-ZETA IS NOT INITIAL.
        READ TABLE IT_SECTION WITH KEY ZSECID = IT_DELAY-ZSECID
                                       SEQ    = IT_DELAY-SEQ.
        IF SY-SUBRC NE 0.
          MESSAGE E000 WITH TEXT-M00.
        ENDIF.

        PERFORM GET_ESTIMATED_SOFF USING IT_DELAY-ZETA
                                         IT_SECTION-ZDRDY
                                         IT_DELAY-ZESOFF.
      ENDIF.
    ENDIF.

    MODIFY IT_DELAY.
  ENDLOOP.

ENDFORM.                    " UPDATE_OTHERS
*&---------------------------------------------------------------------*
*&      Form  APPEND_DELAY_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM APPEND_DELAY_TEMP.

  SORT IT_DELAY BY ZSECID SEQ MODEL_CODE BODY_NO.

  LOOP AT IT_DELAY_TEMP.
    READ TABLE IT_DELAY WITH KEY ZSECID     = IT_DELAY-ZSECID
                                 SEQ        = IT_DELAY-SEQ
                                 MODEL_CODE = IT_DELAY-MODEL_CODE
                                 BODY_NO    = IT_DELAY-BODY_NO
                        BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR: IT_DELAY.

      MOVE-CORRESPONDING IT_DELAY_TEMP TO IT_DELAY.
      IT_DELAY-ZTEMP = ' '.

      CLEAR: IT_DELAY-ZESOFF.
      IF IT_DELAY-ZETA IS NOT INITIAL.
        READ TABLE  IT_SECTION WITH KEY ZSECID = IT_DELAY-ZSECID
                                        SEQ    = IT_DELAY-SEQ.
        IF SY-SUBRC NE 0.
          MESSAGE E000 WITH TEXT-M00.
        ENDIF.

        PERFORM GET_ESTIMATED_SOFF USING IT_DELAY-ZETA
                                         IT_SECTION-ZDRDY
                                         IT_DELAY-ZESOFF.
      ENDIF.

      APPEND IT_DELAY.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " APPEND_DELAY_TEMP
*&---------------------------------------------------------------------*
*&      Form  GET_DELAY_HOUR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DELAY_HOUR USING P_SHOP P_FR_TIME P_GAP.
  DATA: L_GAP TYPE P DECIMALS 2.

  PERFORM CALCULATE_DELAY_TIME USING:
          P_SHOP P_FR_TIME W_CURR_TIME L_GAP.
  P_GAP = L_GAP.
ENDFORM.                    " GET_DELAY_HOUR
*&---------------------------------------------------------------------*
*&      Form  CALCULATE_DELAY_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_SHOP  text
*      -->P_IT_DELAY_ZITIME  text
*      -->P_W_CURR_TIME  text
*      -->P_L_GAP  text
*----------------------------------------------------------------------*
FORM CALCULATE_DELAY_TIME USING PV_SHOP PV_FROM_TIME
                                PV_TO_TIME PV_TIME_GAP.
  DATA: LV_TABIX LIKE SY-TABIX.

  PERFORM GET_FIRST_TIME_ZONE USING PV_SHOP PV_FROM_TIME LV_TABIX.

  LOOP AT IT_WORK_TIME  FROM LV_TABIX
                       WHERE SHOP  =   PV_SHOP.
    IF IT_WORK_TIME-WOSEC EQ 0 OR IT_WORK_TIME-OPSEC EQ 0.
      IF IT_WORK_TIME-WOFRM <= PV_TO_TIME AND
         IT_WORK_TIME-WOEND >= PV_TO_TIME.
        EXIT.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF IT_WORK_TIME-WOFRM > PV_TO_TIME.
      EXIT.
    ENDIF.

    IF IT_WORK_TIME-WOFRM <= PV_FROM_TIME AND
       IT_WORK_TIME-WOEND >= PV_TO_TIME.
      PERFORM GET_TIME_GAP USING PV_FROM_TIME PV_TO_TIME PV_TIME_GAP.
      EXIT.
    ENDIF.

    IF IT_WORK_TIME-WOFRM <= PV_FROM_TIME AND
       IT_WORK_TIME-WOEND >= PV_FROM_TIME.
      PERFORM GET_TIME_GAP USING PV_FROM_TIME IT_WORK_TIME-WOEND
                                 PV_TIME_GAP.
      CONTINUE.
    ENDIF.

    IF IT_WORK_TIME-WOFRM <= PV_TO_TIME AND
       IT_WORK_TIME-WOEND >= PV_TO_TIME.
      PERFORM GET_TIME_GAP USING IT_WORK_TIME-WOFRM PV_TO_TIME
                                 PV_TIME_GAP.
      EXIT.
    ENDIF.

    PV_TIME_GAP = PV_TIME_GAP + IT_WORK_TIME-OPSEC.
  ENDLOOP.

  PV_TIME_GAP = PV_TIME_GAP / 3600.
ENDFORM.                    " CALCULATE_DELAY_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_FIRST_TIME_ZONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_SHOP  text
*      -->P_PV_FROM_TIME  text
*      -->P_LV_TABIX  text
*----------------------------------------------------------------------*
FORM GET_FIRST_TIME_ZONE USING PV_SHOP PV_FROM_TIME PV_TABIX.
  DATA: LV_FROM_TIME(14).

  READ TABLE IT_WORK_TIME WITH KEY SHOP = PV_SHOP
                          BINARY SEARCH.
  IF SY-SUBRC NE 0.
    MESSAGE E000(ZZ) WITH TEXT-M06.
  ENDIF.

  CONCATENATE PV_FROM_TIME(10) IT_WORK_TIME-WOFRM+10(4)
         INTO LV_FROM_TIME.

  READ TABLE IT_WORK_TIME WITH KEY SHOP  = PV_SHOP
                                   WOFRM = LV_FROM_TIME
                          BINARY SEARCH.
  IF SY-SUBRC NE 0.
    LOOP AT IT_WORK_TIME WHERE SHOP  = PV_SHOP
                           AND WOFRM >= PV_FROM_TIME.
      MOVE: SY-TABIX TO PV_TABIX.
      EXIT.
    ENDLOOP.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M06.
    ENDIF.
  ENDIF.

  IF PV_FROM_TIME >= LV_FROM_TIME.
    PV_TABIX = SY-TABIX.
  ELSE.
    PV_TABIX = SY-TABIX - 1.
  ENDIF.
ENDFORM.                    " GET_FIRST_TIME_ZONE

*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WORKING_TIME .

  PERFORM GET_BASE_TIME.

  CLEAR: IT_WORK_TIME. REFRESH: IT_WORK_TIME.

  PERFORM GET_WORKING_TIME USING 'B' V_BDATE_B.
  PERFORM GET_WORKING_TIME USING 'P' V_BDATE_P.
  PERFORM GET_WORKING_TIME USING 'T' V_BDATE_T.

  SORT IT_WORK_TIME BY SHOP WOFRM WOEND.

ENDFORM.                    " READ_WORKING_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_WORKING_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1871   text
*      -->P_V_BDATE_B  text
*----------------------------------------------------------------------*
FORM GET_WORKING_TIME USING PV_SHOP PV_BDATE.
  DATA: LV_GAP TYPE I.

  DATA: LT_WORK_TIME  LIKE ZSMM_WORKING_TIME OCCURS 0 WITH HEADER LINE.

  CHECK PV_BDATE IS NOT INITIAL.

  LV_GAP = SY-DATUM - PV_BDATE + 1.

  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
    EXPORTING
      I_DATUM              = PV_BDATE
      I_DAY                = LV_GAP
      I_ARBPL              = PV_SHOP
    TABLES
      T_WORKING_TIME       = LT_WORK_TIME
    EXCEPTIONS
      CANNOT_READ_DAYNAME  = 1
      INCORRECT_SHIFT_INFO = 2
      INCORRECT_CAPA_INFO  = 3
      OTHERS               = 4.


*  CALL FUNCTION 'Z_fPP_GET_WORKING_TIME'
*    EXPORTING
*      I_DATUM                     = PV_BDATE
*      I_WERKS                     = C_WERKS
*      I_ARBPL                     = PV_SHOP
*      I_DAY                       = LV_GAP
*    TABLES
*      T_0160                      = LT_WORK_TIME
*    EXCEPTIONS
*      INCORRECT_PLANT_VALUE       = 1
*      INCORRECT_WORK_CENTER_VALUE = 2
*      INCORRECT_TIME_FLAG         = 3
*      NO_WORKING_SCHEDULE         = 4
*      INCORRECT_TIME_ZONE         = 5
*      UNKNOWN_ERROR               = 6
*      OTHERS                      = 7.
  IF SY-SUBRC <> 0.
    MESSAGE E000(ZZ) WITH TEXT-M02 PV_SHOP.
  ENDIF.

  LOOP AT LT_WORK_TIME.
    CLEAR: IT_WORK_TIME.
    MOVE: PV_SHOP TO IT_WORK_TIME-SHOP.
    MOVE-CORRESPONDING LT_WORK_TIME TO IT_WORK_TIME.

    APPEND IT_WORK_TIME.

  ENDLOOP.

ENDFORM.                    " GET_WORKING_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_TIME_GAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PV_FROM_TIME  text
*      -->P_LV_TO_TIME  text
*      -->P_PV_TIME_GAP  text
*----------------------------------------------------------------------*
FORM GET_TIME_GAP USING PV_TIME_F PV_TIME_T PV_TIME_GAP.
  DATA: LV_TIME_F   LIKE   SY-UZEIT,
        LV_TIME_T   LIKE   SY-UZEIT.

  MOVE: PV_TIME_F+8(6) TO LV_TIME_F,
        PV_TIME_T+8(6) TO LV_TIME_T.

  IF LV_TIME_F < LV_TIME_T.
    PV_TIME_GAP = PV_TIME_GAP +
                  ( LV_TIME_T - LV_TIME_F ) *
                  ( IT_WORK_TIME-OPSEC / IT_WORK_TIME-WOSEC ).
  ELSEIF LV_TIME_F > LV_TIME_T.
    PV_TIME_GAP = PV_TIME_GAP +
                  ( 86400 - ( LV_TIME_F - LV_TIME_T ) ) *
                  ( IT_WORK_TIME-OPSEC / IT_WORK_TIME-WOSEC ).
  ELSEIF LV_TIME_F = LV_TIME_T.
    " Time Gap is 0
  ENDIF.
ENDFORM.                    " GET_TIME_GAP
*&---------------------------------------------------------------------*
*&      Form  GET_BASE_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BASE_TIME .
  DATA: LV_BTIME(14),
        L_DATE    LIKE SY-DATUM,
        L_BDATE   LIKE SY-DATUM,
        L_DAY_GAP TYPE I.

  L_DATE = W_SYS_DATE - 200.
  CLEAR: V_BDATE_B, V_BDATE_P, V_BDATE_T.

* " Read minimum base date of Body Shop
  CLEAR: LV_BTIME.
  SELECT SINGLE MIN( ZTTIME ) INTO LV_BTIME
    FROM ZTPP_DELAY_VEH AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                ON A~ZSECID = B~ZSECID
                               AND A~SEQ    = B~SEQ
   WHERE DATUM = W_DATE
     AND ZSUPH = 'B'. " ST_CRITICAL_RP-P_IN_RP.
  IF SY-SUBRC EQ 0.
    MOVE: LV_BTIME(8) TO L_BDATE.
    L_DAY_GAP = W_SYS_DATE - L_BDATE.
    IF L_DAY_GAP > 200.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ELSE.
*    PERFORM get_base_time_from_wip USING 'B' lv_btime.
    IF LV_BTIME IS INITIAL.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ENDIF.
  PERFORM GET_YESTERDAY USING LV_BTIME V_BDATE_B 'B'.

  " Read minimum base date of Paint Shop
  CLEAR: LV_BTIME.
  SELECT SINGLE MIN( ZTTIME ) INTO LV_BTIME
    FROM ZTPP_DELAY_VEH AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                ON A~ZSECID = B~ZSECID
                               AND A~SEQ    = B~SEQ
   WHERE DATUM = W_DATE
     AND ZSUPH = 'P'. " ST_CRITICAL_RP-P_IN_RP.
  IF SY-SUBRC EQ 0.
    MOVE: LV_BTIME(8) TO L_BDATE.
    L_DAY_GAP = W_SYS_DATE - L_BDATE.
    IF L_DAY_GAP > 200.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ELSE.
*    PERFORM get_base_time_from_wip USING 'P' lv_btime.
    IF LV_BTIME IS INITIAL.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ENDIF.
  PERFORM GET_YESTERDAY USING LV_BTIME V_BDATE_P 'P'.

*  " Read minimum base date of Trim Shop
  CLEAR: LV_BTIME.
  SELECT SINGLE MIN( ZTTIME ) INTO LV_BTIME
    FROM ZTPP_DELAY_VEH AS A INNER JOIN ZTPP_DELAY_SECT AS B
                                ON A~ZSECID = B~ZSECID
                               AND A~SEQ    = B~SEQ
   WHERE DATUM = W_DATE
     AND ZSUPH = 'T'. " ST_CRITICAL_RP-P_IN_RP.
  IF SY-SUBRC EQ 0.
    MOVE: LV_BTIME(8) TO L_BDATE.
    L_DAY_GAP = W_SYS_DATE - L_BDATE.
    IF L_DAY_GAP > 200.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ELSE.
*    PERFORM get_base_time_from_wip USING 'T' lv_btime.
    IF LV_BTIME IS INITIAL.
      CONCATENATE L_DATE W_SYS_TIME INTO LV_BTIME.
    ENDIF.
  ENDIF.
  PERFORM GET_YESTERDAY USING LV_BTIME V_BDATE_T 'T'.
ENDFORM.                    " GET_BASE_TIME
*&---------------------------------------------------------------------*
*&      Form  GET_YESTERDATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_BDATE_B  text
*----------------------------------------------------------------------*
FORM GET_YESTERDAY USING PV_BTIME PV_BDATE PV_SHOP.
  " Get yesterday(Working Day Criteria)

  DATA: LV_DATE LIKE SY-DATUM,
        LV_TIME LIKE SY-UZEIT.

  CHECK PV_BTIME IS NOT INITIAL.

  MOVE: PV_BTIME(8)   TO LV_DATE,
        PV_BTIME+8(6) TO LV_TIME.

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      C_DATE                   = LV_DATE
      C_TIME                   = LV_TIME
      OPCODE                   = '-'
      WKTIME                   = 1440
      WERKS                    = C_WERKS
      ARBPL                    = PV_SHOP
    IMPORTING
      T_DATE                   = PV_BDATE
    EXCEPTIONS
      ERROR_OPERATION          = 1
      CANNOT_READ_WORKING_TIME = 2
      OTHERS                   = 3.
  IF SY-SUBRC NE 0.
    PV_BDATE = PV_BDATE - 360.
  ENDIF.
ENDFORM.                    " GET_YESTERDATE
*&---------------------------------------------------------------------*
*&      Form  DEQUEUE_PRG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEQUEUE_PRG .
  CALL FUNCTION 'DEQUEUE_EPROG'
    EXPORTING
      MODE_TRDIR     = 'E'
      PROGRAMM       = SY-CPROG
*     X_PROGRAMM     = ' '
      _SCOPE         = '1'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      FOREIGN_LOCK   = 1
      SYSTEM_FAILURE = 2
      OTHERS         = 3.
ENDFORM.                    " DEQUEUE_PRG
*&---------------------------------------------------------------------*
*&      Form  GET_BASE_TIME_FROM_WIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LV_BTIME  text
*----------------------------------------------------------------------*
FORM GET_BASE_TIME_FROM_WIP USING PV_SHOP PV_BTIME.
  SORT IT_WIP BY ZSECID SEQ ZITIME.

  LOOP AT IT_SECTION WHERE ZSUPH = PV_SHOP.
    READ TABLE IT_WIP WITH KEY ZSECID = IT_SECTION-ZSECID
                               SEQ    = IT_SECTION-SEQ
               BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      IF PV_BTIME IS INITIAL OR
         IT_WIP-ZITIME < PV_BTIME.
        PV_BTIME = IT_WIP-ZITIME.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.                   " GET_BASE_TIME_FROM_WIP
*&---------------------------------------------------------------------*
*&      Form  READ_LIMIT_TIME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_LIMIT_TIME .
  DATA:   L_WDATE  LIKE SY-DATUM,
          L_WTIME  LIKE SY-UZEIT,
          L_WDATE_C8(8),
          L_WKTIME TYPE I,
          L_ARBPL  LIKE CRHD-ARBPL.

  L_WKTIME = IT_SECTION-ZDURA * 60.
  L_ARBPL  = IT_SECTION-ZSUPH.

  CALL FUNCTION 'Z_PP_GET_WKTIME'
    EXPORTING
      C_DATE                   = W_SYS_DATE
      C_TIME                   = W_SYS_TIME
      OPCODE                   = '-'
      WKTIME                   = L_WKTIME
      WERKS                    = 'P001'
      ARBPL                    = L_ARBPL
    IMPORTING
      T_DATE                   = L_WDATE
      T_TIME                   = L_WTIME
    EXCEPTIONS
      ERROR_OPERATION          = 1
      CANNOT_READ_WORKING_TIME = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    MESSAGE E000 WITH 'Getting work date failed ' IT_SECTION-ZSECID.
  ENDIF.

  L_WDATE_C8 = L_WDATE.
  CONCATENATE L_WDATE_C8 L_WTIME INTO IT_SECTION-LIMIT_TIME.
ENDFORM.                    " READ_LIMIT_TIME
*&---------------------------------------------------------------------*
*&      Form  READ_PREVIOUS_DELAY_VEH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PREVIOUS_DELAY_VEH .
  DATA: L_DATUM LIKE SY-DATUM.

  SELECT MAX( DATUM ) INTO L_DATUM
    FROM ZTPP_DELAY_VEH
   WHERE DATUM < W_DATE.

  SELECT * INTO TABLE IT_DELAY_PREV
    FROM ZTPP_DELAY_VEH
   WHERE DATUM = L_DATUM.

ENDFORM.                    " READ_PREVIOUS_DELAY_VEH
*&---------------------------------------------------------------------*
*&      Form  CURRENT_SHOPE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM WORKING_SHOP_DATE .
  DATA: L_DATE LIKE SY-DATUM,
        L_CURR_TIME(14).

  DATA: LT_WORK_TIME  LIKE ZSMM_WORKING_TIME OCCURS 0
                       WITH HEADER LINE.

** Changed by Park On 11/22/13
*  l_date = w_sys_date - 1.
*  CONCATENATE w_sys_date w_sys_time INTO l_curr_time.
*
*  CALL FUNCTION 'Z_FMM_GET_WORKING_TIME'
*    EXPORTING
*      i_datum              = l_date
*      i_day                = 2
*      i_arbpl              = 'T'
*    TABLES
*      t_working_time       = lt_work_time
*    EXCEPTIONS
*      cannot_read_dayname  = 1
*      incorrect_shift_info = 2
*      incorrect_capa_info  = 3
*      OTHERS               = 4.
*  IF sy-subrc <> 0.
*    MESSAGE e000(zz) WITH text-m02 .
*  ENDIF.
*
*  LOOP AT lt_work_time WHERE wofrm <= l_curr_time
*                         AND woend >= l_curr_time.
*    w_date = lt_work_time-datum - 1.
*    EXIT.
*  ENDLOOP.
*  IF sy-subrc NE 0.
*    IF w_sys_time >= '000000'  AND w_sys_time <= '000645'.
*      w_date = w_sys_date - 2.
*    ELSE.
*      w_date = w_sys_date - 1.
*    ENDIF.
*  ENDIF.

  W_DATE = W_SYS_DATE.
** End of change 11/22/13

ENDFORM.                    " CURRENT_SHOPE_DATE
*&---------------------------------------------------------------------*
*&      Form  READ_ZTSD_UM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ZTSD_UM .
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE IT_UM
    FROM ZTSD_UM
     FOR ALL ENTRIES IN IT_DELAY
   WHERE MODEL_CODE = IT_DELAY-MODEL_CODE
     AND BODY_NO    = IT_DELAY-BODY_NO
     AND STATUS IN (SPACE,'F').
ENDFORM.                    " READ_ZTSD_UM
*&---------------------------------------------------------------------*
*&      Form  UPDATE_FROM_UM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_FROM_UM .
  PERFORM READ_ZTSD_UM.

  SORT IT_UM         BY MODEL_CODE BODY_NO.

  LOOP AT IT_DELAY.
    READ TABLE IT_UM WITH KEY MODEL_CODE = IT_DELAY-MODEL_CODE
                              BODY_NO    = IT_DELAY-BODY_NO
                     BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      MOVE: IT_UM-URGENCY  TO IT_DELAY-URGENCY,
            IT_UM-URGCDATE TO IT_DELAY-URGCDATE,
            IT_UM-FLET     TO IT_DELAY-FLET.
    ENDIF.

    MODIFY IT_DELAY.
  ENDLOOP.
ENDFORM.                    " UPDATE_FROM_UM
*&---------------------------------------------------------------------*
*&      Form  GET_ESTIMATED_SOFF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DETAIL_ZETA  text
*      -->P_IT_DETAIL_ZDRDY  text
*      -->P_IT_DETAIL_ZESOFF  text
*----------------------------------------------------------------------*
FORM GET_ESTIMATED_SOFF USING PV_ZETA PV_DAY PV_ZESOFF.
  DATA: LW_DATE    TYPE D,
        LW_INDEX   TYPE I,
        LW_DAYNR   LIKE HRVSCHED-DAYNR,
        LW_DAYFREE LIKE HRVSCHED-NODAY.

  IF PV_DAY EQ 0.
    PV_ZESOFF = PV_ZETA.
    EXIT.
  ENDIF.

  IF PV_ZETA IS INITIAL.
    CLEAR: PV_ZESOFF.
    EXIT.
  ENDIF.

  LW_DATE = PV_ZETA.

  DO.
    LW_DATE  = LW_DATE  + 1.

    CALL FUNCTION 'RH_GET_DATE_DAYNAME'
      EXPORTING
        LANGU               = SY-LANGU
        DATE                = LW_DATE
        CALID               = 'HM'
      IMPORTING
        DAYNR               = LW_DAYNR
        DAYFREE             = LW_DAYFREE
      EXCEPTIONS
        NO_LANGU            = 1
        NO_DATE             = 2
        NO_DAYTXT_FOR_LANGU = 3
        INVALID_DATE        = 4
        OTHERS              = 5.
    IF SY-SUBRC <> 0.
      RAISE CANNOT_READ_DAYNAME.
    ENDIF.

    IF LW_DAYFREE EQ 'X'.
      CONTINUE.
    ENDIF.

    LW_INDEX = LW_INDEX + 1.

    IF LW_INDEX >= PV_DAY OR LW_INDEX >= 200.
      EXIT.
    ENDIF.
  ENDDO.

  PV_ZESOFF = LW_DATE.
ENDFORM.                    " GET_ESTIMATED_SOFF

*&---------------------------------------------------------------------*
*&      Form  GET_OPEN_SIDE_TRACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ETA_FROM_OPEN_SIDE_TRACK .
  DATA: LT_SIDE_TRACK LIKE ZTPP_SIDE_TRACK OCCURS 0 WITH HEADER LINE.

  DATA: LV_TABIX LIKE SY-TABIX.

  SELECT * INTO TABLE LT_SIDE_TRACK
    FROM ZTPP_SIDE_TRACK
   WHERE ST_STATUS EQ 'I'.

  SORT LT_SIDE_TRACK BY MODEL_CODE BODY_NO ZSEQ DESCENDING.
  DELETE ADJACENT DUPLICATES FROM LT_SIDE_TRACK
         COMPARING MODEL_CODE BODY_NO.

  SORT IT_DELAY BY MODEL_CODE BODY_NO.

  LOOP AT LT_SIDE_TRACK.

    READ TABLE IT_DELAY WITH KEY MODEL_CODE = LT_SIDE_TRACK-MODEL_CODE
                                 BODY_NO    = LT_SIDE_TRACK-BODY_NO
                        BINARY SEARCH.
    IF SY-SUBRC EQ 0 AND
       IT_DELAY-ZETA IS INITIAL AND
       IT_DELAY-ZRPFR <= LT_SIDE_TRACK-RP_CSTATUS AND
       IT_DELAY-ZRPTO >= LT_SIDE_TRACK-RP_CSTATUS AND
       LT_SIDE_TRACK-ZETA IS NOT INITIAL.
      MOVE: SY-TABIX TO LV_TABIX.

      MOVE: LT_SIDE_TRACK-ZETA TO IT_DELAY-ZETA.

      READ TABLE IT_SECTION WITH KEY ZSECID = IT_DELAY-ZSECID
                                     SEQ    = IT_DELAY-SEQ.
      IF SY-SUBRC NE 0.
        MESSAGE E000 WITH TEXT-M00.
      ENDIF.

      PERFORM GET_ESTIMATED_SOFF USING IT_DELAY-ZETA
                                       IT_SECTION-ZDRDY
                                       IT_DELAY-ZESOFF.

      MODIFY IT_DELAY INDEX LV_TABIX.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_OPEN_SIDE_TRACK
*&---------------------------------------------------------------------*
*&      Form  UPDATE_OPEN_SIDE_TRACK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_OPEN_SIDE_TRACK .
  DATA: LT_SIDE_TRACK LIKE ZTPP_SIDE_TRACK OCCURS 0 WITH HEADER LINE.

  DATA: L_OBJEK     LIKE AUSP-OBJEK,
        L_RP_STATUS LIKE AUSP-ATWRT,
        L_USAGE     LIKE AUSP-ATWRT.

  SELECT * INTO TABLE LT_SIDE_TRACK
    FROM ZTPP_SIDE_TRACK
   WHERE ST_STATUS EQ 'I'.

  LOOP AT LT_SIDE_TRACK.
    CONCATENATE LT_SIDE_TRACK-MODEL_CODE LT_SIDE_TRACK-BODY_NO
           INTO L_OBJEK.

    SELECT SINGLE A~ATWRT AS RP_STATUS
                  B~ATWRT AS USAGE
      INTO (L_RP_STATUS,L_USAGE)
    FROM AUSP AS A LEFT OUTER JOIN AUSP AS B
                     ON B~OBJEK = A~OBJEK
                    AND B~ATINN = V_USAGE_CAR
                    AND B~MAFID = A~MAFID
                    AND B~KLART = A~KLART
     WHERE A~OBJEK = L_OBJEK
       AND A~KLART EQ '002'
       AND A~ATINN EQ V_RP_STATUS.
    IF SY-SUBRC NE 0.
      MESSAGE E000(ZZ) WITH TEXT-M99.
    ENDIF.

    IF L_RP_STATUS NE LT_SIDE_TRACK-RP_CSTATUS OR
       L_USAGE     EQ 'S'                      OR
       L_USAGE     EQ 'D'.
      UPDATE ZTPP_SIDE_TRACK
        SET: ST_STATUS = 'F'
             AENAM     = SY-UNAME
             AEDAT     = SY-DATUM
             AEZET     = SY-UZEIT
       WHERE MODEL_CODE = LT_SIDE_TRACK-MODEL_CODE
         AND BODY_NO    = LT_SIDE_TRACK-BODY_NO
         AND ZSEQ       = LT_SIDE_TRACK-ZSEQ.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " UPDATE_OPEN_SIDE_TRACK
