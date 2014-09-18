REPORT ZMMIFTZW.
INCLUDE ZMMFWTOP.
*&--------------------------------------------------------------------&*
*&    Program: ZMMIFTZW.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Send FTZ related Work In Progress information.   &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 10/18/2005  Shiva    UD1K918096     initial program.
*&                      UD1K918108     initial program.
*& 10/28/2005  Shiva    UD1K918163     separate color and pass date
*&                                     and time information.
*&--------------------------------------------------------------------&*
TABLES: PLAF.
PARAMETERS: P_DATE LIKE SY-DATUM OBLIGATORY.
SELECT-OPTIONS: S_PLNUM FOR PLAF-PLNUM.
*            p_plnum LIKE plaf-plnum .
SELECTION-SCREEN SKIP.
PARAMETERS: P_LINES AS CHECKBOX.
PARAMETERS: P_TIME TYPE I DEFAULT 10.

R_DT-SIGN = 'I'.
R_DT-OPTION = 'BT'.
CONCATENATE P_DATE '000000' INTO W_DATE_TIME.
R_DT-LOW = W_DATE_TIME.
CLEAR W_DATE_TIME.
CONCATENATE P_DATE '235959' INTO W_DATE_TIME.
R_DT-HIGH = W_DATE_TIME.
APPEND R_DT.
CLEAR: R_DT, W_DATE_TIME.

*For WIP: -> P_RP_STATUS between 1 & 24 or eq 26.
*         -> P_USAGE_CAR(T,D,S):  Don't include 'D' 'S'.
*                                 'T'-> between 1 & 17.

*SELECT mara~matnr mtart profl meins SUM( lbkum ) maktx
*                         INTO TABLE it_mat_info
*                         FROM mara
*                         INNER JOIN mbew
*                         ON mbew~matnr = mara~matnr
*                         AND mbew~lvorm = mara~lvorm
*                         INNER JOIN makt
*                         ON makt~matnr = mara~matnr
*                         WHERE ( mara~lvorm = space    AND
*                                      spras = sy-langu AND
*                                      mtart = 'ROH' )
*                         OR    ( mara~lvorm = space    AND
*                                      spras = sy-langu AND
*                                      mtart = 'ROH1' )
*                         GROUP BY mara~matnr mtart profl meins maktx.
*
SELECT MARA~MATNR MTART PROFL MEINS SUM( LBKUM ) MAKTX
                         INTO TABLE IT_MAT_INFO
                         FROM MARA
                         INNER JOIN MBEW
                         ON MBEW~MATNR = MARA~MATNR
                         AND MBEW~LVORM = MARA~LVORM
                         INNER JOIN MAKT
                         ON MAKT~MATNR = MARA~MATNR
                         WHERE  MARA~LVORM = SPACE    AND
                                SPRAS = SY-LANGU AND
                                 ( MTART = 'ROH'
                                OR MTART = 'ROH1' )
                         GROUP BY MARA~MATNR MTART PROFL MEINS MAKTX
** Fuorng on 05/23/12 for sap tuning
%_HINTS ORACLE 'ORDERED USE_NL(T_00 T_01 T_02) INDEX (T_00 "MARA~T")'.
** End on 05/23/12

SELECT MARA~MATNR MBEW~BWKEY SUM( LBKUM )
                         INTO TABLE IT_MAT_INV
                         FROM MARA
                         INNER JOIN MBEW
                         ON MBEW~MATNR = MARA~MATNR
                         AND MBEW~LVORM = MARA~LVORM
                         INNER JOIN MAKT
                         ON MAKT~MATNR = MARA~MATNR
                         WHERE  MARA~LVORM = SPACE    AND
                                SPRAS = SY-LANGU AND
                                 MTART = 'HALB'
                         GROUP BY MARA~MATNR BWKEY
** Fuorng on 05/23/12 for sap tuning
%_HINTS ORACLE 'ORDERED USE_NL(T_00 T_01 T_02) INDEX (T_00 "MARA~T")'.
** End on 05/23/12

SELECT ATINN ADZHL ATNAM FROM CABN
                         INTO TABLE IT_CABN
                         WHERE ATNAM = 'P_DESTINATION_CODE'
                         OR    ATNAM = 'P_MI'
                         OR    ATNAM = 'P_MODEL_YEAR'
                         OR    ATNAM = 'P_OCN'
                         OR    ATNAM = 'P_PLAN_ORDER'
                         OR    ATNAM = 'P_RP_STATUS'
                         OR    ATNAM = 'P_SEQUENCE_DATE'
                         OR    ATNAM = 'P_USAGE_CAR'
                         OR    ATNAM = 'P_VERSION'.

** GET HALB PARTS TO EXPLORE BOM FOR INVENTORY
LOOP AT IT_MAT_INV.
  IF IT_MAT_INV-LBKUM > 0.
    WA_HALB-MATNR =  IT_MAT_INV-MATNR.
    WA_HALB-WERKS =  IT_MAT_INV-BWKEY.
    WA_HALB-DATUV =  SY-DATUM.
    WA_HALB-BDMNG = IT_MAT_INV-LBKUM.
    APPEND WA_HALB TO IT_HALB .
    CLEAR: WA_HALB.
  ENDIF.
ENDLOOP.

LOOP AT IT_CABN INTO WA_CABN.
  CASE WA_CABN-ATNAM.
    WHEN 'P_DESTINATION_CODE'.
      W_ATINN1 = WA_CABN-ATINN.
      W_ADZHL1 = WA_CABN-ADZHL.
    WHEN 'P_PLAN_ORDER'.
      W_ATINN2 = WA_CABN-ATINN.
      W_ADZHL2 = WA_CABN-ADZHL.
*    when 'P_RP25_ACTUAL_DATE'.
*      w_atinn3 = wa_cabn-atinn.
*      w_adzhl3 = wa_cabn-adzhl.
*    when 'P_RP27_ACTUAL_DATE'.
*      w_atinn4 = wa_cabn-atinn.
*      w_adzhl4 = wa_cabn-adzhl.
    WHEN 'P_RP_STATUS'.
      W_ATINN3 = WA_CABN-ATINN.
      W_ADZHL3 = WA_CABN-ADZHL.
    WHEN 'P_SEQUENCE_DATE'.
      W_ATINN4 = WA_CABN-ATINN.
      W_ADZHL4 = WA_CABN-ADZHL.
    WHEN 'P_USAGE_CAR'.
      W_ATINN5 = WA_CABN-ATINN.
      W_ADZHL5 = WA_CABN-ADZHL.
    WHEN 'P_VERSION'.
      W_ATINN6 = WA_CABN-ATINN.
      W_ADZHL6 = WA_CABN-ADZHL.
    WHEN 'P_MI'.
      W_ATINN11 = WA_CABN-ATINN.
      W_ADZHL11 = WA_CABN-ADZHL.
    WHEN 'P_MODEL_YEAR'.
      W_ATINN12 = WA_CABN-ATINN.
      W_ADZHL12 = WA_CABN-ADZHL.
    WHEN 'P_OCN'.
      W_ATINN13 = WA_CABN-ATINN.
      W_ADZHL13 = WA_CABN-ADZHL.
  ENDCASE.
ENDLOOP.

*IF sy-uname = '101457'. " OR sy-uname ='100698'.
*  SELECT objek atinn atwrt
*              INTO TABLE it_ausp_wip
*              FROM ausp
*              WHERE objek = any ( SELECT objek FROM ausp
*                                  WHERE klart = '002'
*                                    AND atinn = w_atinn2
*                                    AND adzhl = w_adzhl2
*                                    AND atwrt in s_plnum )
**        OR ( klart = '002' AND atinn = w_atinn2 AND adzhl = w_adzhl2 )
**        OR ( klart = '002' AND atinn = w_atinn3 AND adzhl = w_adzhl3 )
**        OR ( klart = '002' AND atinn = w_atinn4 AND adzhl = w_adzhl4 )
**        OR ( klart = '002' AND atinn = w_atinn5 AND adzhl = w_adzhl5 )
**       OR ( klart = '002' AND atinn = w_atinn6 AND adzhl = w_adzhl6 )
*)
*  .
*
*ELSE.
*  SELECT objek atinn atwrt
*              INTO TABLE it_ausp_wip
*              FROM ausp
*              WHERE objek = any ( SELECT objek FROM ausp
*                                  WHERE ( ( klart = '002' AND
*                                            atinn = w_atinn3 AND
*                                            adzhl = w_adzhl3 AND
*                                         atwrt BETWEEN '01' AND '24' )
*                                         OR ( klart = '002' AND
*                                              atinn = w_atinn3 AND
*                                              adzhl = w_adzhl3 AND
*                                              atwrt EQ '26' ) ) )
*      AND ( ( klart = '002' AND atinn = w_atinn1 AND adzhl = w_adzhl1 )
*         OR ( klart = '002' AND atinn = w_atinn2 AND adzhl = w_adzhl2 )
*         OR ( klart = '002' AND atinn = w_atinn3 AND adzhl = w_adzhl3 )
*         OR ( klart = '002' AND atinn = w_atinn4 AND adzhl = w_adzhl4 )
*         OR ( klart = '002' AND atinn = w_atinn5 AND adzhl = w_adzhl5 )
*       OR ( klart = '002' AND atinn = w_atinn6 AND adzhl = w_adzhl6 ) )
*.
*ENDIF.

IF SY-UNAME = '101457'. " or sy-uname = '100698'.
  SELECT OBJEK INTO TABLE IT_AUSP_STATUS
      FROM AUSP
      WHERE KLART = '002'
      AND ATINN = W_ATINN3
      AND ADZHL = W_ADZHL3
      AND ATWRT = '18'.
ELSE.
  SELECT OBJEK INTO TABLE IT_AUSP_STATUS
      FROM AUSP
      WHERE KLART = '002'
      AND ATINN = W_ATINN3
      AND ADZHL = W_ADZHL3
      AND ( ATWRT BETWEEN '01' AND '24'
            OR ATWRT EQ '26' ).
ENDIF.
SELECT OBJEK ATINN ATWRT
              INTO TABLE IT_AUSP_WIP
              FROM AUSP
    FOR ALL ENTRIES IN IT_AUSP_STATUS
    WHERE KLART = '002'
      AND ( ( OBJEK = IT_AUSP_STATUS-OBJEK
      AND ATINN = W_ATINN2
      AND ADZHL = W_ADZHL2 )
          OR ( OBJEK = IT_AUSP_STATUS-OBJEK
      AND ATINN = W_ATINN3
      AND ADZHL = W_ADZHL3 )
          OR ( OBJEK = IT_AUSP_STATUS-OBJEK
      AND ATINN = W_ATINN5
      AND ADZHL = W_ADZHL5 ) ).

IF SY-SUBRC NE 0.
  MESSAGE I999(ZMMM) WITH 'There is no data!'(001).
  EXIT.
ENDIF.
SORT: IT_CABN BY ATINN,
      IT_AUSP_WIP BY OBJEK ATINN.

LOOP AT IT_AUSP_WIP INTO WA_AUSP_WIP.
  AT NEW OBJEK.
    CLEAR W_OBJEK.
    WA_KEY_WIP-OBJEK = WA_AUSP_WIP-OBJEK.
    APPEND WA_KEY_WIP TO IT_KEY_WIP.
    W_OBJEK = WA_AUSP_WIP-OBJEK.
  ENDAT.
  READ TABLE IT_CABN INTO WA_CABN WITH KEY ATINN = WA_AUSP_WIP-ATINN
                                                  BINARY SEARCH.
  IF SY-SUBRC NE 0.
    CLEAR:  WA_KEY_WIP.
    CONTINUE.
  ENDIF.
  CASE WA_CABN-ATNAM.
    WHEN 'P_PLAN_ORDER'.
      WA_KEY_WIP-PLNUM = WA_AUSP_WIP-ATWRT.
      MODIFY IT_KEY_WIP FROM WA_KEY_WIP TRANSPORTING PLNUM
                        WHERE OBJEK = W_OBJEK.
    WHEN 'P_RP_STATUS'.
      WA_KEY_WIP-STATU = WA_AUSP_WIP-ATWRT.
      MODIFY IT_KEY_WIP FROM WA_KEY_WIP TRANSPORTING STATU
                        WHERE OBJEK = W_OBJEK.
    WHEN 'P_USAGE_CAR'.
      WA_KEY_WIP-USAGE = WA_AUSP_WIP-ATWRT.
      MODIFY IT_KEY_WIP FROM WA_KEY_WIP TRANSPORTING USAGE
                        WHERE OBJEK = W_OBJEK.
  ENDCASE.
  CLEAR:  WA_KEY_WIP.
ENDLOOP.

*IF p_plnum IS INITIAL.

IF S_PLNUM IS INITIAL.
ELSE.
*  DELETE it_key_wip WHERE plnum NE p_plnum.
  DELETE IT_KEY_WIP WHERE NOT PLNUM IN S_PLNUM.
ENDIF.
DELETE IT_KEY_WIP WHERE ( USAGE = 'T' AND STATU > 17 )
                  OR USAGE = 'D' OR USAGE = 'S'.

CALL FUNCTION 'SPBT_INITIALIZE'
     EXPORTING
          GROUP_NAME                     = 'PG_FTZ'
     IMPORTING
          MAX_PBT_WPS                    = W_MAX
          FREE_PBT_WPS                   = W_FREE
     EXCEPTIONS
          INVALID_GROUP_NAME             = 1
          INTERNAL_ERROR                 = 2
          PBT_ENV_ALREADY_INITIALIZED    = 3
          CURRENTLY_NO_RESOURCES_AVAIL   = 4
          NO_PBT_RESOURCES_FOUND         = 5
          CANT_INIT_DIFFERENT_PBT_GROUPS = 6
          OTHERS                         = 7.
IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

DESCRIBE TABLE IT_KEY_WIP LINES W_LINES.

** added by furong on 08/29/2006 for deleted plan order
IF W_LINES > 0.
  DATA: LW_AUSP LIKE TABLE OF AUSP WITH HEADER LINE.

  SELECT * INTO TABLE IT_PLAF FROM PLAF.

** Furong on 05/23/12 for sap tuning
SORT IT_PLAF BY PLNUM.
** End on 05/23/12

  LOOP AT IT_KEY_WIP ASSIGNING  <FS_WIP>.
    READ TABLE IT_PLAF WITH KEY PLNUM = <FS_WIP>-PLNUM
** Furong on 05/23/12 for sap tuning
    BINARY SEARCH.
** End on 05/23/12
    IF SY-SUBRC <> 0.
      SELECT * INTO TABLE LW_AUSP
       FROM AUSP
       WHERE OBJEK = <FS_WIP>-OBJEK
         AND KLART = '002'.
      LOOP AT LW_AUSP.
        CASE LW_AUSP-ATINN.
          WHEN W_ATINN1.
            W_DESTN = LW_AUSP-ATWRT.
          WHEN W_ATINN4.
            W_PACK = LW_AUSP-ATFLV.
            WRITE W_PACK TO WA_HALB-DATUV.
          WHEN W_ATINN11.
            W_MODIDX = LW_AUSP-ATWRT.
          WHEN W_ATINN12.
            W_MODYR = LW_AUSP-ATWRT.
          WHEN W_ATINN13.
            W_OCN = LW_AUSP-ATWRT.
          WHEN W_ATINN6.
            W_VERSN = LW_AUSP-ATWRT.
        ENDCASE.
      ENDLOOP.
      W_LEN = STRLEN( W_MODIDX ).
      IF W_LEN > 7.
        W_ODEALER = W_DESTN+3(2).

        CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
             EXPORTING
                  OLD_DEALER = W_ODEALER
             IMPORTING
                  NEW_DEALER = W_NDEALER.

        CONCATENATE W_MODYR W_DESTN+0(3) W_NDEALER W_MODIDX INTO W_FSC.
        CONCATENATE W_FSC W_OCN INTO W_FSC.
      ELSE.
        CONCATENATE W_MODYR W_DESTN W_MODIDX INTO W_FSC.
        CONCATENATE W_FSC W_OCN INTO W_FSC SEPARATED BY SPACE.
      ENDIF.
      SELECT SINGLE WERKS INTO WA_HALB-WERKS
       FROM MARC
      WHERE MATNR = W_FSC.

      WA_HALB-MATNR = W_FSC.
*      wa_halb-datuv = p_date.
      WA_HALB-BDMNG = 1.
      WA_HALB-STLAL = W_VERSN+1(2).
      COLLECT WA_HALB INTO IT_HALB.
      CLEAR: WA_HALB, IT_PLAF.
      CLEAR: LW_AUSP,LW_AUSP[].
      CLEAR:  W_LEN, W_ODEALER, W_NDEALER.
    ENDIF.
  ENDLOOP.

  IF P_LINES = 'X'.
    W_LINES = 1.
  ENDIF.
*  w_lines = 10.
* end of change

  IT_KEY_WIP1[] = IT_KEY_WIP.

  IF P_TIME > 1.
    W_FREE = W_FREE * P_TIME.
  ENDIF.
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
      W_TO = W_I * W_FREE.
    ELSE.
      W_TO = W_LINES.
    ENDIF.
    LOOP AT IT_KEY_WIP ASSIGNING  <FS_WIP> FROM W_FRM TO W_TO.
      DO.
        CALL FUNCTION 'Z_FFTZ_READ_PLANORDER'
             STARTING NEW TASK    W_TASKNAME
             DESTINATION IN GROUP 'PG_FTZ'
             PERFORMING BOM_INFO ON END OF TASK
             EXPORTING
               P_PLNUM        = <FS_WIP>-PLNUM
             TABLES
               P_ITCOMPONENTS = IT_COMPONENTS
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
*          ELSE.
*            EXIT.
            ENDIF.
        ENDCASE.
      ENDDO.
    ENDLOOP.

* Replace WAIT statement for loop

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

    DO.
      WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
      IF W_RCV_JOBS >= W_SND_JOBS.
        EXIT.
      ENDIF.
    ENDDO.


    W_I = W_I + 1.
    PERFORM COLLECT_COMPONENT_DATA.
  ENDWHILE.

  PERFORM BOM_MAT_INFO.
  IF IT_ZTMM_6022_01 IS INITIAL.
    MESSAGE S999(ZMMM) WITH 'There is no data!'(001).
    EXIT.
  ENDIF.
  PERFORM PROCESS_DATA.   "Process Data
  PERFORM Z_FCA_EAI_INTERFACE_LOG.
  IF SY-BATCH IS INITIAL.   "Not Backgroung Processing
    PERFORM DISPLAY_LOG.    "Display Data Log
  ELSE.
    MESSAGE S999(ZMMM) WITH 'Application Doc. No.'
                        W_ZDOCNO
                        'is created !'.
  ENDIF.
ENDIF.
INCLUDE ZIMMGM28I_6022CLA.   "Class Part
INCLUDE ZIMMGM28I_6022O01.   "PBO Part
INCLUDE ZIMMGM28I_6022I01.   "PAI Part

*&---------------------------------------------------------------------*
*&      Form  fsc_bom_exp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_=  text
*      -->P_3  text
*----------------------------------------------------------------------*
FORM FSC_BOM_EXP USING W_TASKNAME.

  DATA: WA_SUB_STPOX LIKE STPOX,
        W_MATNR LIKE MARA-MATNR.

  DATA: IT_SUB_STPOX LIKE TABLE OF WA_SUB_STPOX.
*  data: L_SOBSL like t460a-SOBSL,
  DATA:  L_WRK02 LIKE T460A-WRK02.

  RECEIVE RESULTS FROM FUNCTION 'Z_FFTZ_EXP_BOM'
                        IMPORTING P_MATNR = W_MATNR
                        TABLES    P_STPOX = IT_SUB_STPOX
                       EXCEPTIONS
                             COMMUNICATION_FAILURE  = 1
                             SYSTEM_FIALURE         = 2.
  IF SY-SUBRC NE 0.
    W_EXCEP_FLAG = 'X'.
    EXIT.
  ENDIF.
  W_RCV_JOBS = W_RCV_JOBS + 1.

  LOOP AT IT_SUB_STPOX INTO WA_SUB_STPOX.
    IF WA_SUB_STPOX-MTART = 'HALB'.
      IF WA_SUB_STPOX-SOBSL = 40.
        SELECT SINGLE WRK02 INTO L_WRK02
         FROM T460A
         WHERE WERKS = WA_SUB_STPOX-WERKS
           AND SOBSL = '40'.
*        if sy-subrc = 0.
        IT_HALB_ENG-MATNR = WA_SUB_STPOX-IDNRK.
        IT_HALB_ENG-WERKS = L_WRK02.
        IT_HALB_ENG-DATUV = SY-DATUM.
        IT_HALB_ENG-BDMNG = WA_SUB_STPOX-MNGKO.
        COLLECT IT_HALB_ENG.
      ENDIF.
    ENDIF.
  ENDLOOP.
  DELETE IT_SUB_STPOX WHERE MTART IN R_MTART
                      OR    DUMPS EQ 'X'
                      OR    DUMPS EQ 'x'.
  APPEND LINES OF IT_SUB_STPOX TO IT_FILTER_STPOX.
  REFRESH IT_SUB_STPOX.
ENDFORM.                    " fsc_bom_exp

INCLUDE ZMMIFTZWF01.
