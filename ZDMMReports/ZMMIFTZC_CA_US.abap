REPORT ZMMIFTZC_NEW.
INCLUDE ZMMFZTOP.
INCLUDE ZMMCONTP.
*&--------------------------------------------------------------------&*
*&    Program: ZMMIFTZC.                                              &*
*&    Author : Shiva.                                                 &*
*&    Specification: Send FTZ related consumption information.        &*
*&--------------------------------------------------------------------&*
*& Date        User     Transport       Description
*& 10/13/2006  Furong   UD1K922540      copy ZMMIFTZC, memory not enough
*& 10/13/2006  Furong   UD1K922568      performance issue,
*& 01/24/2007  Furong   UD1K930452      BOM Explosion for HALB (Engine)
*&--------------------------------------------------------------------&*

PARAMETERS: P_DATE   LIKE SY-DATUM OBLIGATORY,
            P_PLNUM  LIKE PLAF-PLNUM,
            P_SRVGRP LIKE RZLLITAB-CLASSNAME OBLIGATORY
                     DEFAULT 'PG_FTZ'.
PARAMETERS: P_TIME TYPE I DEFAULT 10.

INITIALIZATION.
  MESSAGE i000(ZMPP) WITH 'DO NOT USE THIS PROGRAM'.
  LEAVE PROGRAM.

R_DT-SIGN = 'I'.
R_DT-OPTION = 'BT'.
CONCATENATE P_DATE '000000' INTO W_DATE_TIME.
R_DT-LOW = W_DATE_TIME.
CLEAR W_DATE_TIME.
CONCATENATE P_DATE '235959' INTO W_DATE_TIME.
R_DT-HIGH = W_DATE_TIME.
APPEND R_DT.
CLEAR: R_DT, W_DATE_TIME.

CONCATENATE P_DATE+4(2) '/' P_DATE+6(2) '/' P_DATE(4) INTO W_KEY_DATE.
DATA:    W_ADZHL18 LIKE CABN-ADZHL,    "Vesl_dest
W_ATINN18 LIKE CABN-ATINN.
DATA: W_V_DEST(5).
DATA: BEGIN OF IT_AUSP_STATUS_2 OCCURS 0,
        OBJEK LIKE AUSP-OBJEK,
      END OF IT_AUSP_STATUS_2.

SELECT MATNR MTART MEINS FROM MARA
                         INTO TABLE IT_MAT_TYPE
                        WHERE ( MTART = 'HALB' AND LVORM = SPACE )
                        OR    ( MTART = 'ROH'  AND LVORM = SPACE )
                        OR    ( MTART = 'FERT' AND LVORM = SPACE ).

SELECT PLNUM FROM PLAF
             INTO TABLE IT_PLAF.

SELECT ATINN ADZHL ATNAM FROM CABN
                         INTO TABLE IT_CABN
                         WHERE ATNAM = 'P_DESTINATION_CODE'
                         OR    ATNAM = 'P_MI'
                         OR    ATNAM = 'P_MODEL_YEAR'
                         OR    ATNAM = 'P_OCN'
                         OR    ATNAM = 'P_PLAN_ORDER'
                         OR    ATNAM = 'P_RP25_ACTUAL_DATE'
                         OR    ATNAM = 'P_RP27_ACTUAL_DATE'
                         OR    ATNAM = 'P_RP_STATUS'
                         OR    ATNAM = 'P_SALES_ORDER'
                         OR    ATNAM = 'P_SEQUENCE_DATE'
                         OR    ATNAM = 'P_USAGE_CAR'
                         OR    ATNAM = 'P_VERSION'
                         OR    ATNAM =  'P_VESL_DEST'.

LOOP AT IT_CABN INTO WA_CABN.
  CASE WA_CABN-ATNAM.
    WHEN 'P_DESTINATION_CODE'.
      W_ATINN1 = WA_CABN-ATINN.
      W_ADZHL1 = WA_CABN-ADZHL.
    WHEN 'P_MI'.
      W_ATINN2 = WA_CABN-ATINN.
      W_ADZHL2 = WA_CABN-ADZHL.
    WHEN 'P_MODEL_YEAR'.
      W_ATINN3 = WA_CABN-ATINN.
      W_ADZHL3 = WA_CABN-ADZHL.
    WHEN 'P_OCN'.
      W_ATINN4 = WA_CABN-ATINN.
      W_ADZHL4 = WA_CABN-ADZHL.
    WHEN 'P_PLAN_ORDER'.
      W_ATINN5 = WA_CABN-ATINN.
      W_ADZHL5 = WA_CABN-ADZHL.
    WHEN 'P_RP25_ACTUAL_DATE'.
      W_ATINN6 = WA_CABN-ATINN.
      W_ADZHL6 = WA_CABN-ADZHL.
    WHEN 'P_RP27_ACTUAL_DATE'.
      W_ATINN7 = WA_CABN-ATINN.
      W_ADZHL7 = WA_CABN-ADZHL.
    WHEN 'P_RP_STATUS'.
      W_ATINN8 = WA_CABN-ATINN.
      W_ADZHL8 = WA_CABN-ADZHL.
    WHEN 'P_SALES_ORDER'.
      W_ATINN9 = WA_CABN-ATINN.
      W_ADZHL9 = WA_CABN-ADZHL.
    WHEN 'P_SEQUENCE_DATE'.
      W_ATINN10 = WA_CABN-ATINN.
      W_ADZHL10 = WA_CABN-ADZHL.
    WHEN 'P_USAGE_CAR'.
      W_ATINN11 = WA_CABN-ATINN.
      W_ADZHL11 = WA_CABN-ADZHL.
    WHEN 'P_VERSION'.
      W_ATINN12 = WA_CABN-ATINN.
      W_ADZHL12 = WA_CABN-ADZHL.
    WHEN 'P_VESL_DEST'.
      W_ATINN18 = WA_CABN-ATINN.
      W_ADZHL18 = WA_CABN-ADZHL.
  ENDCASE.
ENDLOOP.

IF SY-UNAME = ' '. " '101457'. " OR sy-uname ='100698'.
  SELECT OBJEK ATINN ATWRT
              INTO TABLE IT_AUSP
              FROM AUSP
              WHERE OBJEK = ANY ( SELECT OBJEK FROM AUSP
                                   WHERE  KLART = '002' AND
                                          ATINN = W_ATINN5 AND
                                          ADZHL = W_ADZHL5 AND
                                          ATWRT = P_PLNUM ).
ELSE.
*SELECT objek atinn atwrt atflv
*             INTO TABLE it_ausp
*             FROM ausp
*             WHERE objek = any ( SELECT objek FROM ausp
*                                  WHERE ( ( klart = '002' AND
*                                          atinn = w_atinn8 AND
*                                          adzhl = w_adzhl8 AND
*                                          atwrt = '25' )
*                                    OR  ( klart = '002' AND
*                                          atinn = w_atinn8 AND
*                                          adzhl = w_adzhl8 AND
*                                          atwrt = '27' ) )
*                               AND objek = ANY ( SELECT objek FROM ausp
*                                            WHERE ( klart = '002' AND
*                                                 atinn = w_atinn6 AND
*                                                 adzhl = w_adzhl6 AND
*                                                 atwrt IN r_dt )
*                                             OR ( klart = '002'   AND
*                                                  atinn = w_atinn7 AND
*                                                  adzhl = w_adzhl7 AND
*                                                  atwrt IN r_dt ) ) )
*  AND ( ( klart = '002' AND atinn = w_atinn1 AND adzhl = w_adzhl1 )
*         OR ( klart = '002' AND atinn = w_atinn2 AND adzhl = w_adzhl2 )
*         OR ( klart = '002' AND atinn = w_atinn3 AND adzhl = w_adzhl3 )
*         OR ( klart = '002' AND atinn = w_atinn4 AND adzhl = w_adzhl4 )
*         OR ( klart = '002' AND atinn = w_atinn5 AND adzhl = w_adzhl5 )
*         OR ( klart = '002' AND atinn = w_atinn9 AND adzhl = w_adzhl9 )
*        OR ( klart = '002' AND atinn = w_atinn10 AND adzhl = w_adzhl10
*)
*        OR ( klart = '002' AND atinn = w_atinn11 AND adzhl = w_adzhl11
*)
*   OR ( klart = '002' AND atinn = w_atinn12 AND adzhl = w_adzhl12 ) ).

  SELECT OBJEK INTO TABLE IT_AUSP_STATUS_1
      FROM AUSP
      WHERE KLART = '002'
        AND ( ATINN = W_ATINN6
              AND ADZHL = W_ADZHL6
              AND ATWRT IN R_DT
          OR  ATINN = W_ATINN7
              AND ADZHL = W_ADZHL7
              AND ATWRT IN R_DT ).
  IF IT_AUSP_STATUS_1[] IS INITIAL.
    MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH TEXT-001.
    EXIT.
  ELSE.
** Changed by Furong on 04/30/08
    SELECT OBJEK INTO TABLE IT_AUSP_STATUS_2
        FROM AUSP
        FOR ALL ENTRIES IN IT_AUSP_STATUS_1
        WHERE OBJEK = IT_AUSP_STATUS_1-OBJEK
          AND ATINN = W_ATINN18
          AND ADZHL = W_ADZHL18
          AND KLART = '002'
          AND ATWRT = 'FAPNJ'.
    IF IT_AUSP_STATUS_2[] IS INITIAL.
      MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH TEXT-001.
      EXIT.
    ELSE.
      SELECT OBJEK INTO TABLE IT_AUSP_STATUS
          FROM AUSP
          FOR ALL ENTRIES IN IT_AUSP_STATUS_2
          WHERE OBJEK = IT_AUSP_STATUS_2-OBJEK
            AND ATINN = W_ATINN8
            AND ADZHL = W_ADZHL8
            AND KLART = '002'
            AND ( ATWRT = '25' OR ATWRT = '27' ).
    ENDIF.
*    SELECT OBJEK INTO TABLE IT_AUSP_STATUS
*        FROM AUSP
*        FOR ALL ENTRIES IN IT_AUSP_STATUS_1
*        WHERE OBJEK = IT_AUSP_STATUS_1-OBJEK
*          AND ATINN = W_ATINN8
*          AND ADZHL = W_ADZHL8
*          AND KLART = '002'
*          AND ( ATWRT = '25' OR ATWRT = '27' ).
** End of change
  ENDIF.

  IF IT_AUSP_STATUS[] IS INITIAL.
    MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH TEXT-001.
    EXIT.
  ELSE.
    SELECT OBJEK ATINN ATWRT ATFLV
                  INTO TABLE IT_AUSP
                  FROM AUSP
        FOR ALL ENTRIES IN IT_AUSP_STATUS
        WHERE KLART = '002'
          AND OBJEK = IT_AUSP_STATUS-OBJEK
          AND ( ( ATINN = W_ATINN1 AND ADZHL = W_ADZHL1 )
             OR ( ATINN = W_ATINN2 AND ADZHL = W_ADZHL2 )
             OR ( ATINN = W_ATINN3 AND ADZHL = W_ADZHL3 )
             OR ( ATINN = W_ATINN4 AND ADZHL = W_ADZHL4 )
             OR ( ATINN = W_ATINN5 AND ADZHL = W_ADZHL5 )
             OR ( ATINN = W_ATINN9 AND ADZHL = W_ADZHL9 )
            OR ( ATINN = W_ATINN10 AND ADZHL = W_ADZHL10 )
            OR ( ATINN = W_ATINN11 AND ADZHL = W_ADZHL11 )
            OR ( ATINN = W_ATINN12 AND ADZHL = W_ADZHL12 ) ).
  ENDIF.
  IF SY-SUBRC NE 0.
    MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH TEXT-001.
    EXIT.
  ENDIF.
ENDIF.
LOOP AT IT_AUSP ASSIGNING <FS_AUSP>
                WHERE ATINN EQ W_ATINN11
                AND   ATWRT NE 'P'.
  R_SCRAP-SIGN = 'I'.
  R_SCRAP-OPTION = 'EQ'.
  R_SCRAP-LOW = <FS_AUSP>-OBJEK.
  APPEND R_SCRAP.
ENDLOOP.

IF NOT R_SCRAP[] IS INITIAL.
  DELETE IT_AUSP WHERE OBJEK IN R_SCRAP.
ENDIF.

REFRESH: R_SCRAP.

LOOP AT IT_AUSP ASSIGNING <FS_AUSP>
                WHERE ATINN EQ W_ATINN1.
  IF <FS_AUSP>-ATWRT+0(3) = 'B06'.
  ELSE.
    R_SCRAP-SIGN = 'I'.
    R_SCRAP-OPTION = 'EQ'.
    R_SCRAP-LOW = <FS_AUSP>-OBJEK.
    APPEND R_SCRAP.
  ENDIF.
ENDLOOP.

IF NOT R_SCRAP[] IS INITIAL.
  DELETE IT_AUSP WHERE OBJEK IN R_SCRAP.
ENDIF.

SORT: IT_PLAF BY PLNUM,
      IT_AUSP BY OBJEK.
*&--------------------------------------------------------------------&*
*     FSC = model yr. + Destn.code + model idx. + space + OCN         &*
*&--------------------------------------------------------------------&*
LOOP AT IT_AUSP ASSIGNING <FS_AUSP>.
  CASE <FS_AUSP>-ATINN.
    WHEN W_ATINN1.
      W_DESTN = <FS_AUSP>-ATWRT.
    WHEN W_ATINN2.
      W_MODIDX = <FS_AUSP>-ATWRT.
    WHEN W_ATINN3.
      W_MODYR = <FS_AUSP>-ATWRT.
    WHEN W_ATINN4.
      W_OCN = <FS_AUSP>-ATWRT.
    WHEN W_ATINN5.
      W_PLNUM = <FS_AUSP>-ATWRT.
    WHEN W_ATINN9.
      W_SALORD = <FS_AUSP>-ATWRT.
    WHEN W_ATINN10.
      W_PACK = <FS_AUSP>-ATFLV.
      WRITE W_PACK TO W_DATE.
    WHEN W_ATINN12.
      W_VERSN = <FS_AUSP>-ATWRT.
*    WHEN W_ATINN18.
*      W_V_dest = <FS_AUSP>-ATWRT.
  ENDCASE.
  AT END OF OBJEK.
** Cahnged by furong on 03/12/08 for fsc
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
** end of change on 03/12/08
    WA_EQUI_INFO-EQUNR = <FS_AUSP>-OBJEK.
    WA_EQUI_INFO-FSCOD = W_FSC.
    WA_EQUI_INFO-PLNUM = W_PLNUM.
    WA_EQUI_INFO-CODES = W_DESTN.
    WA_EQUI_INFO-SDATE = W_DATE.
    WA_EQUI_INFO-VERSN = W_VERSN.
    WA_EQUI_INFO-SALOD = W_SALORD.
    APPEND WA_EQUI_INFO TO IT_EQUI_INFO.
    CLEAR: WA_EQUI_INFO, W_MODYR, W_DESTN, W_MODIDX, W_OCN, W_FSC,
           W_LEN, W_ODEALER, W_NDEALER.
  ENDAT.
ENDLOOP.

CLEAR W_LINES.

IF NOT P_PLNUM IS INITIAL.
  DELETE IT_EQUI_INFO WHERE PLNUM NE P_PLNUM.
  DESCRIBE TABLE IT_EQUI_INFO LINES W_LINES.
  IF W_LINES EQ 0.
    MESSAGE ID 'ZMM' TYPE 'I' NUMBER '999' WITH TEXT-002.
    EXIT.
  ENDIF.
ENDIF.
LOOP AT IT_EQUI_INFO ASSIGNING  <FS_EQUI>.
  READ TABLE IT_PLAF WITH KEY PLNUM = <FS_EQUI>-PLNUM
                                      TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
    WA_FSC_INFO-PLNUM = <FS_EQUI>-PLNUM.
    WA_FSC_INFO-MATNR = <FS_EQUI>-FSCOD.
    WA_FSC_INFO-SDATE = <FS_EQUI>-SDATE.
    WA_FSC_INFO-VERSN = <FS_EQUI>-VERSN.
    WA_FSC_INFO-SALOD = <FS_EQUI>-SALOD.
    APPEND WA_FSC_INFO TO IT_FSC_INFO.
    <FS_EQUI>-MARK = 'X'.
  ENDIF.
ENDLOOP.
*&---Get plant info for FSC.
DESCRIBE TABLE IT_FSC_INFO LINES W_LINES.
IF W_LINES NE 0.
  SELECT MATNR WERKS INTO TABLE IT_MARC
                     FROM MARC
                     FOR ALL ENTRIES IN IT_FSC_INFO
                     WHERE MATNR = IT_FSC_INFO-MATNR.
  IF SY-SUBRC NE 0.
  ELSE.
    SORT IT_MARC BY MATNR.
  ENDIF.
ENDIF.
LOOP AT IT_FSC_INFO INTO WA_FSC_INFO.
  WA_HALB-PLNUM = WA_FSC_INFO-PLNUM.
  WA_HALB-VBELN = WA_FSC_INFO-SALOD.
  WA_HALB-MATNR = WA_FSC_INFO-MATNR.
  WA_HALB-PMATNR = WA_FSC_INFO-MATNR.
  READ TABLE IT_MARC INTO WA_MARC WITH KEY MATNR = WA_FSC_INFO-MATNR
                                                   BINARY SEARCH.
  IF SY-SUBRC NE 0.
  ELSE.
    WA_HALB-WERKS = WA_MARC-WERKS.
  ENDIF.
  WA_HALB-MENGE = 1.
  WA_HALB-STLAL = WA_FSC_INFO-VERSN+1(2).
  WA_HALB-DATUV = WA_FSC_INFO-SDATE.
  APPEND  WA_HALB TO IT_HALB.
ENDLOOP.

DELETE IT_EQUI_INFO WHERE MARK = 'X'.

CALL FUNCTION 'SPBT_INITIALIZE'
     EXPORTING
          GROUP_NAME                     = P_SRVGRP
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
DESCRIBE TABLE IT_EQUI_INFO LINES W_LINES.
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
  LOOP AT IT_EQUI_INFO ASSIGNING  <FS_EQUI> FROM W_FRM TO W_TO.
    DO.
      CALL FUNCTION 'Z_FFTZ_READ_PLANORDER'
           STARTING NEW TASK    W_TASKNAME
           DESTINATION IN GROUP P_SRVGRP
           PERFORMING BOM_INFO ON END OF TASK
           EXPORTING
             P_PLNUM        = <FS_EQUI>-PLNUM
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
*        ELSE.
*          EXIT.
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
ENDWHILE.
*&----Add all Parent material( Existing Plan ord + Deleted Plan ord)
LOOP AT IT_HALB INTO WA_HALB.
  WA_PARENT_MAT-PLNUM = WA_HALB-PLNUM.
  WA_PARENT_MAT-VBELN = WA_HALB-VBELN.
  WA_PARENT_MAT-MATNR = WA_HALB-MATNR.
  WA_PARENT_MAT-PLANT = WA_HALB-WERKS.
  WA_PARENT_MAT-MENGE = WA_HALB-MENGE.
  READ TABLE IT_MAT_TYPE INTO WA_MAT_TYPE
                         WITH TABLE KEY MATNR = WA_HALB-MATNR
                                        MTART = 'FERT'
                                       TRANSPORTING MEINS.
  IF SY-SUBRC NE 0.
    WA_PARENT_MAT-MEINS = SPACE.
  ELSE.
    WA_PARENT_MAT-MEINS = WA_MAT_TYPE-MEINS.
  ENDIF.
  COLLECT WA_PARENT_MAT INTO IT_PARENT_MAT.
ENDLOOP.
CLEAR: WA_PARENT_MAT, WA_HALB,WA_MAT_TYPE.
SORT IT_TEMP BY MATCOM-MATERIAL.
LOOP AT IT_TEMP ASSIGNING <FS_COMP>.
READ TABLE IT_MAT_TYPE WITH TABLE KEY MATNR = <FS_COMP>-MATCOM-MATERIAL
                                                          MTART = 'ROH'
                                                 TRANSPORTING NO FIELDS.
*&----Get rid of the color from material number.
  IF SY-SUBRC EQ 0.
    PERFORM SEPARATE_COLOR USING <FS_COMP>-MATCOM-MATERIAL
                                 'ROH'
                        CHANGING W_MATNR_NC
                                 W_COLOR.
    WA_COLOR_PARTS-MATNR = <FS_COMP>-MATCOM-MATERIAL.
    COLLECT WA_COLOR_PARTS INTO IT_COLOR_PARTS.
    CLEAR WA_COLOR_PARTS.
  ELSE.
    W_MATNR_NC = <FS_COMP>-MATCOM-MATERIAL.
    W_COLOR = SPACE.
  ENDIF.
  WA_PLANORD_INFO-PLNUM = <FS_COMP>-PLNUM.
  WA_PLANORD_INFO-VBELN = <FS_COMP>-VBELN.
  WA_PLANORD_INFO-PMATNR = <FS_COMP>-PMATNR.
  WA_PLANORD_INFO-CMATNR = W_MATNR_NC.
  WA_PLANORD_INFO-PLANT  = <FS_COMP>-MATCOM-PLANT.
  WA_PLANORD_INFO-REQQTY = <FS_COMP>-MATCOM-REQ_QUAN.
  WA_PLANORD_INFO-MENGE  = <FS_COMP>-MATCOM-ENTRY_QTY.
  WA_PLANORD_INFO-MEINS  = <FS_COMP>-MATCOM-BASE_UOM.
  COLLECT WA_PLANORD_INFO INTO IT_PLANORD_INFO.
*&----Get HALB material for BOM explosion.
READ TABLE IT_MAT_TYPE WITH TABLE KEY MATNR = <FS_COMP>-MATCOM-MATERIAL
                                                         MTART = 'HALB'
                                                 TRANSPORTING NO FIELDS.
  IF SY-SUBRC NE 0.
  ELSE.
    WA_HALB-PLNUM = <FS_COMP>-PLNUM.
    WA_HALB-VBELN = <FS_COMP>-VBELN.
    WA_HALB-MATNR = <FS_COMP>-MATCOM-MATERIAL.
    WA_HALB-PMATNR = <FS_COMP>-PMATNR.
    WA_HALB-WERKS = <FS_COMP>-MATCOM-PLANT.
    WA_HALB-MENGE = <FS_COMP>-MATCOM-ENTRY_QTY.
    WA_HALB-STLAL = SPACE.
    WA_HALB-DATUV = P_DATE.
    COLLECT WA_HALB INTO IT_HALB.
  ENDIF.
  CLEAR: WA_HALB, WA_PLANORD_INFO, W_MATNR_NC, W_COLOR.
ENDLOOP.
FREE IT_TEMP.
IF <FS_COMP> IS ASSIGNED.
  UNASSIGN <FS_COMP>.
ENDIF.
LOOP AT IT_HALB INTO WA_HALB.
  WA_HALB_EXP-MATNR = WA_HALB-MATNR.
  WA_HALB_EXP-WERKS = WA_HALB-WERKS.
  WA_HALB_EXP-STLAL = WA_HALB-STLAL.
  WA_HALB_EXP-DATUV = WA_HALB-DATUV.
  COLLECT WA_HALB_EXP INTO IT_HALB_EXP.
ENDLOOP.
CLEAR WA_HALB_EXP.
WA_HALB_EXP-MENGE = 1.
MODIFY IT_HALB_EXP FROM WA_HALB_EXP TRANSPORTING MENGE
                         WHERE MENGE IS INITIAL.
*&---------------Explode BOM for HALB & FSC---------------------------&*
PERFORM GET_BOM_INFO.
*&--------------------------------------------------------------------&*
CLEAR WA_PLANORD_INFO.
LOOP AT IT_PARENT_MAT INTO WA_PARENT_MAT.
  WA_PLANORD_INFO-PLNUM  = WA_PARENT_MAT-PLNUM.
  WA_PLANORD_INFO-VBELN  = WA_PARENT_MAT-VBELN.
  WA_PLANORD_INFO-PMATNR = WA_PARENT_MAT-MATNR.
  WA_PLANORD_INFO-CMATNR = WA_PARENT_MAT-MATNR.
  WA_PLANORD_INFO-PLANT  = WA_PARENT_MAT-PLANT.
  WA_PLANORD_INFO-MENGE  = WA_PARENT_MAT-MENGE.
  WA_PLANORD_INFO-MEINS  = WA_PARENT_MAT-MEINS.
  APPEND WA_PLANORD_INFO TO IT_PLANORD_INFO.
*Sales order into to get country Shipped to.
  WA_VBAK-VBELN = WA_PARENT_MAT-VBELN.
  COLLECT WA_VBAK INTO IT_VBAK.
ENDLOOP.
PERFORM GET_MATERIAL_INFO.
PERFORM GET_SALEPART_CCODE USING 'C'.

SORT IT_PLANORD_INFO BY PMATNR CMATNR PLANT.
LOOP AT IT_PLANORD_INFO ASSIGNING <FS_PLANORD>.
  WA_ZTMM_6026_01-PARTNERID = '100300'.
  W_DATE = SY-DATUM.
  W_TIME = SY-UZEIT.
  CONCATENATE W_DATE 'T' W_TIME  INTO WA_ZTMM_6026_01-EFFDATE.
  WA_ZTMM_6026_01-TXNCODE = 'SPNM'.
  WA_ZTMM_6026_01-ORDERNUMRECEIPT = SPACE.
  WA_ZTMM_6026_01-TRANSPORTID     = SPACE.
  CONCATENATE <FS_PLANORD>-PMATNR P_DATE
                                  INTO WA_ZTMM_6026_01-ORDERNUMWORK.
*  wa_ztmm_6026_01-billoflading.
  CONCATENATE P_DATE 'T' '000000' INTO WA_ZTMM_6026_01-TXNDATE.
  CONCATENATE 'SHIPCAR' P_DATE INTO WA_ZTMM_6026_01-ORDERNUMSHIP.
  WA_ZTMM_6026_01-MATNR = <FS_PLANORD>-CMATNR.
  IF <FS_PLANORD>-PMATNR EQ <FS_PLANORD>-CMATNR.
    WA_ZTMM_6026_01-PTC = 'NM'.
  ELSE.
    WA_ZTMM_6026_01-PTC = 'PC'.
  ENDIF.
  WA_ZTMM_6026_01-PTCSRC = C_ERP_SOURCE.
  WA_ZTMM_6026_01-MAKTXSRC = C_ERP_SOURCE.
  WA_ZTMM_6026_01-NAFTACERTIFIED = SPACE.
  WA_ZTMM_6026_01-NAFTACERTIFIEDSC = C_FTZLINK_SOURCE.
  WA_ZTMM_6026_01-MENGE = <FS_PLANORD>-MENGE.
  WA_ZTMM_6026_01-QTYPERLM = <FS_PLANORD>-REQQTY.
  WA_ZTMM_6026_01-MEINSSRC = C_ERP_SOURCE.
  READ TABLE IT_VBPA INTO WA_VBPA WITH KEY VBELN = <FS_PLANORD>-VBELN
                                                   BINARY SEARCH.
  IF SY-SUBRC NE 0.
    WA_ZTMM_6026_01-COUNTRYSHIPTO = SPACE.
  ELSE.
    WA_ZTMM_6026_01-COUNTRYSHIPTO = WA_VBPA-LAND1.
  ENDIF.
  WA_ZTMM_6026_01-ADJPRODUCTNUM = SPACE.
  WA_ZTMM_6026_01-MODEOFTRANSPORT = 'L'.
  WA_ZTMM_6026_01-STATUSCODE    = 'F'.
  WA_ZTMM_6026_01-STATUSCODESRC = ''.
  WA_ZTMM_6026_01-VALIDFLAG        = 'N'.
  WA_ZTMM_6026_01-ASSIGNMENTFLAG   = 'N'.
  WA_ZTMM_6026_01-FIFOFLAG         = 'N'.
  WA_ZTMM_6026_01-DELETEDFLAG      = 'N'.
  WA_ZTMM_6026_01-KEEPDURINGROLLBA = 'N'.
*&----------FIFO HTS value
  READ TABLE IT_MAT_INFO1 INTO WA_MAT_INFO1
                         WITH TABLE KEY MATNR = <FS_PLANORD>-CMATNR
                                        WERKS = <FS_PLANORD>-PLANT.
  IF SY-SUBRC NE 0.
    CONCATENATE <FS_PLANORD>-CMATNR '*' INTO W_C_MATNR.
    LOOP AT IT_COLOR_PARTS INTO WA_COLOR_PARTS
                            WHERE MATNR CP W_C_MATNR.
      W_C_MATNR = WA_COLOR_PARTS-MATNR.
      EXIT.
    ENDLOOP.
    READ TABLE IT_MAT_INFO1 INTO WA_MAT_INFO1
                           WITH TABLE KEY MATNR = W_C_MATNR
                                          WERKS = <FS_PLANORD>-PLANT.
    IF SY-SUBRC NE 0.
      WA_ZTMM_6026_01-STAWN    = SPACE.
      WA_ZTMM_6026_01-STAWNSRC = 'H'.
      WA_ZTMM_6026_01-NTGEW  = SPACE.
      WA_ZTMM_6026_01-NTGEWSRC = C_FTZLINK_SOURCE.
      WA_ZTMM_6026_01-GEWEI = SPACE.
      WA_ZTMM_6026_01-GEWEISRC = C_FTZLINK_SOURCE.
    ELSE.
      IF WA_MAT_INFO1-STAWN IS INITIAL.
        WA_ZTMM_6026_01-STAWN    = SPACE.
        WA_ZTMM_6026_01-STAWNSRC = 'H'.
      ELSE.
        WA_ZTMM_6026_01-STAWN    = WA_MAT_INFO1-STAWN.
        WA_ZTMM_6026_01-STAWNSRC = C_ERP_SOURCE.
      ENDIF.
      WA_ZTMM_6026_01-MAKTX = WA_MAT_INFO1-MAKTX. "Material
      WA_ZTMM_6026_01-MEINS = <FS_PLANORD>-MEINS. "Material
      IF WA_MAT_INFO1-NTGEW IS INITIAL.
        WA_ZTMM_6026_01-NTGEWSRC = C_FTZLINK_SOURCE.
        WA_ZTMM_6026_01-GEWEISRC = C_FTZLINK_SOURCE.
      ELSE.
        WA_ZTMM_6026_01-NTGEW  = WA_MAT_INFO1-NTGEW. "Material
        WA_ZTMM_6026_01-NTGEWSRC = C_ERP_SOURCE.
        WA_ZTMM_6026_01-GEWEI = WA_MAT_INFO1-GEWEI. "Material
        WA_ZTMM_6026_01-GEWEISRC = C_ERP_SOURCE.
      ENDIF.
    ENDIF.
  ELSE.
    IF WA_MAT_INFO1-STAWN IS INITIAL.
      WA_ZTMM_6026_01-STAWN    = SPACE.
      WA_ZTMM_6026_01-STAWNSRC = 'H'.
    ELSE.
      WA_ZTMM_6026_01-STAWN    = WA_MAT_INFO1-STAWN.
      WA_ZTMM_6026_01-STAWNSRC = C_ERP_SOURCE.
    ENDIF.
    WA_ZTMM_6026_01-MAKTX = WA_MAT_INFO1-MAKTX. "Material
    WA_ZTMM_6026_01-MEINS = <FS_PLANORD>-MEINS. "Material
    IF WA_MAT_INFO1-NTGEW IS INITIAL.
      WA_ZTMM_6026_01-NTGEWSRC = C_FTZLINK_SOURCE.
      WA_ZTMM_6026_01-GEWEISRC = C_FTZLINK_SOURCE.
    ELSE.
      WA_ZTMM_6026_01-NTGEW  = WA_MAT_INFO1-NTGEW. "Material
      WA_ZTMM_6026_01-NTGEWSRC = C_ERP_SOURCE.
      WA_ZTMM_6026_01-GEWEI = WA_MAT_INFO1-GEWEI. "Material
      WA_ZTMM_6026_01-GEWEISRC = C_ERP_SOURCE.
    ENDIF.
  ENDIF.
  WA_ZTMM_6026_01-SPICODE1SRC = 'H'.
  WA_ZTMM_6026_01-SPICODE2SRC = 'H'.
  WA_ZTMM_6026_01-LIFNRSRC = C_FTZLINK_SOURCE.
  WA_ZTMM_6026_01-RELFLAGSRC = 'M'.
  WA_ZTMM_6026_01-HTSINDEXSRC = C_FTZLINK_SOURCE.
  WA_ZTMM_6026_01-HTSDESCSRC = 'H'.
  WA_ZTMM_6026_01-HTSNUM2SRC = C_FTZLINK_SOURCE.
  READ TABLE IT_EINA INTO WA_EINA WITH KEY MATNR = <FS_PLANORD>-CMATNR
                                                     BINARY SEARCH.
  IF SY-SUBRC NE 0.
    WA_ZTMM_6026_01-NETPR = 0.
    WA_ZTMM_6026_01-EFFPR = 0.
    WA_ZTMM_6026_01-NETPRUOM = 0.
    WA_ZTMM_6026_01-EFFPRUOM = 0.
  ELSE.
    WA_ZTMM_6026_01-NETPR = WA_EINA-KBETR.
    WA_ZTMM_6026_01-EFFPR = WA_EINA-EFFPR.
    IF NOT WA_EINA-BPUMN IS INITIAL.
 WA_ZTMM_6026_01-NETPRUOM = ( WA_ZTMM_6026_01-NETPR * WA_EINA-BPUMZ ) /
                                                          WA_EINA-BPUMN.
 WA_ZTMM_6026_01-EFFPRUOM = ( WA_ZTMM_6026_01-EFFPR * WA_EINA-BPUMZ ) /
                                                          WA_EINA-BPUMN.
      IF NOT WA_EINA-PEINH IS INITIAL.
    WA_ZTMM_6026_01-NETPRUOM = WA_ZTMM_6026_01-NETPRUOM / WA_EINA-PEINH.
    WA_ZTMM_6026_01-EFFPRUOM = WA_ZTMM_6026_01-EFFPRUOM / WA_EINA-PEINH.
      ENDIF.
    ENDIF.
  ENDIF.
  WA_ZTMM_6026_01-LAND1 = WA_EINA-LAND1.
  WA_ZTMM_6026_01-VALUE2SRC = C_FTZLINK_SOURCE.
  IF WA_EINA-WAERS IS INITIAL.
    WA_ZTMM_6026_01-WAERSSRC = C_FTZLINK_SOURCE.
  ELSE.
    WA_ZTMM_6026_01-WAERS    = WA_EINA-WAERS.
    WA_ZTMM_6026_01-WAERSSRC = C_ERP_SOURCE.
  ENDIF.
  WA_ZTMM_6026_01-ALTVALUESRC = 'I'.
  WA_ZTMM_6026_01-ADVALOREMRATESRC = 'H'.
  WA_ZTMM_6026_01-SPECIFICRATESRC = 'H'.
  WA_ZTMM_6026_01-UOMCONVFACTORSRC = 'I'.
  WA_ZTMM_6026_01-ADDUOMCONVFACSRC = 'I'.
  WA_ZTMM_6026_01-RPTQTYUOMSRC = 'H'.
  WA_ZTMM_6026_01-ADDRPTQTYUOMSRC = 'H'.
  WA_ZTMM_6026_01-DOTINDICATOR     = 'N'.
  WA_ZTMM_6026_01-FCCINDICATOR     = 'N'.
  WA_ZTMM_6026_01-FDAINDICATOR     = 'N'.

  WA_ZTMM_6026_01-COLOR         = '1'.
  WA_ZTMM_6026_01-COUNTRYSHIPTO = 'US'.
  APPEND WA_ZTMM_6026_01 TO IT_ZTMM_6026_01.
  CLEAR: WA_ZTMM_6026_01,WA_MAT_INFO1,WA_VBPA,WA_EINA.
ENDLOOP.

PERFORM FTZ_SUM_NEW.

PERFORM PROCESS_DATA_BY_SECTION.
IF SY-BATCH IS INITIAL.
  PERFORM DSP_LOG.
ENDIF.
INCLUDE ZIMMGM29I_6026CLA.
INCLUDE ZIMMGM29I_6026O01.   "PBO Part
INCLUDE ZIMMGM29I_6026I01.   "PAI Part
INCLUDE ZMMIFTZRF01.

*---------------------------------------------------------------------*
*       FORM bom_info                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  W_TASKNAME                                                    *
*---------------------------------------------------------------------*
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

  WA_PARENT_MAT-PLNUM = WA_HEADER-PLANNEDORDER_NUM.
  WA_PARENT_MAT-MATNR = WA_HEADER-MATERIAL.
  WA_PARENT_MAT-PLANT = WA_HEADER-PROD_PLANT.
  WA_PARENT_MAT-MENGE = WA_HEADER-TOTAL_PLORD_QTY.
  WA_PARENT_MAT-MEINS = WA_HEADER-BASE_UOM.
  WA_PARENT_MAT-VBELN = WA_HEADER-SALES_ORD.
  APPEND WA_PARENT_MAT TO IT_PARENT_MAT.

  DELETE IT_COMPONENTS WHERE PHANT_ITEM = 'X'.
  APPEND LINES OF IT_COMPONENTS TO IT_TEMP.
  WA_TEMP-PLNUM  = WA_HEADER-PLANNEDORDER_NUM.
  WA_TEMP-PMATNR = WA_HEADER-MATERIAL.
  WA_TEMP-VBELN  = WA_HEADER-SALES_ORD.
  MODIFY IT_TEMP FROM WA_TEMP TRANSPORTING PLNUM PMATNR VBELN
                       WHERE PMATNR IS INITIAL.
  REFRESH: IT_COMPONENTS.
  CLEAR: WA_RETURN, WA_HEADER, WA_TEMP, WA_PARENT_MAT.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  get_material_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_MATERIAL_INFO.

  DATA: BEGIN OF WA_A018,
           MATNR LIKE A018-MATNR,
           LIFNR LIKE A018-LIFNR,
           KNUMH LIKE A018-KNUMH,
        END OF WA_A018.
  DATA: BEGIN OF WA_KONP,
          KNUMH LIKE KONP-KNUMH,
          KBETR LIKE KONP-KBETR,
        END OF WA_KONP.
  DATA:IT_A018 LIKE TABLE OF WA_A018,
       IT_KONP LIKE HASHED TABLE OF WA_KONP WITH UNIQUE KEY KNUMH.
  FIELD-SYMBOLS:  <FS_EINA> LIKE LINE OF IT_EINA.
  SELECT MARA~MATNR WERKS PROFL NTGEW GEWEI STAWN MAKTX
                          INTO TABLE IT_MAT_INFO1
                          FROM MARA
                          INNER JOIN MARC
                          ON MARC~MATNR = MARA~MATNR
                          INNER JOIN MAKT
                          ON MAKT~MATNR = MARA~MATNR
*                          for all entries in it_planord_info
*                          where mara~matnr = it_planord_info-cmatnr
                           WHERE ( MTART = 'ROH' AND SPRAS = SY-LANGU )
                           OR  ( MTART = 'ROH1' AND  SPRAS = SY-LANGU )
                           OR  ( MTART = 'FERT' AND  SPRAS = SY-LANGU ).
  IF SY-SUBRC NE 0.
  ENDIF.
  SELECT MATNR EINA~LIFNR EINE~WERKS EKORG WAERS PEINH
               BPUMZ BPUMN EFFPR LAND1
                     INTO TABLE IT_EINA
                     FROM EINA
                     INNER JOIN EINE
                     ON EINE~INFNR = EINA~INFNR
                     INNER JOIN LFA1
                     ON LFA1~LIFNR = EINA~LIFNR
                     FOR ALL ENTRIES IN IT_PLANORD_INFO
                     WHERE MATNR = IT_PLANORD_INFO-CMATNR.
*                     and   eina~lifnr = it_planord_info-lifnr.
  DESCRIBE TABLE IT_EINA LINES W_LINES.
  IF W_LINES = 0.
  ELSE.
    SELECT MATNR LIFNR KNUMH INTO TABLE IT_A018
                             FROM A018
                             FOR ALL ENTRIES IN IT_EINA
                             WHERE MATNR = IT_EINA-MATNR
                             AND   LIFNR = IT_EINA-LIFNR
                             AND   EKORG = IT_EINA-EKORG
                             AND   DATAB <= W_DATE
                             AND   DATBI >= W_DATE
                             AND   KSCHL = 'PB00'.
  ENDIF.
  DESCRIBE TABLE IT_A018 LINES W_LINES.
  IF W_LINES = 0.
  ELSE.
    SORT IT_A018 BY KNUMH.
    SELECT KNUMH KBETR FROM KONP
                       INTO TABLE IT_KONP
                       FOR ALL ENTRIES IN IT_A018
                       WHERE KNUMH = IT_A018-KNUMH
                       AND   KSCHL = 'PB00'.
  ENDIF.
  CLEAR W_LINES.
  SORT: IT_EINA BY MATNR LIFNR,
        IT_A018 BY MATNR LIFNR.
  W_LINES = 1.
  LOOP AT IT_EINA ASSIGNING <FS_EINA>.
    READ TABLE IT_A018 INTO WA_A018 WITH KEY MATNR = <FS_EINA>-MATNR
                                             LIFNR = <FS_EINA>-LIFNR
                                             BINARY SEARCH
                                             TRANSPORTING KNUMH.
    IF SY-SUBRC NE 0.
      <FS_EINA>-KBETR = 0.
      CONTINUE.
    ENDIF.
   READ TABLE IT_KONP INTO WA_KONP WITH TABLE KEY KNUMH = WA_A018-KNUMH.
    IF SY-SUBRC NE 0.
      <FS_EINA>-KBETR = 0.
    ELSE.
      <FS_EINA>-KBETR = WA_KONP-KBETR.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_material_info
*&---------------------------------------------------------------------*
*&      Form  get_bom_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_BOM_INFO.

  DATA: WA_FSC_STPOX LIKE STPOX.
  DATA: IT_FSC_STPOX LIKE TABLE OF WA_FSC_STPOX.
  DATA: BEGIN OF WA_T460A,
          MATNR LIKE MARC-MATNR,
          WERKS LIKE MARC-WERKS,
          WRK02 LIKE T460A-WRK02,
        END OF WA_T460A.
  DATA: IT_T460A LIKE TABLE OF WA_T460A.

  RANGES: R_BOM_TOPMAT FOR MARA-MATNR.

  FIELD-SYMBOLS: <FS_HALB> LIKE LINE OF IT_HALB,
                  <FS_HALB_EXP> LIKE LINE OF IT_HALB_EXP,
                 <FS_COM> LIKE LINE OF IT_FSC_STPOX1,
                 <FS_HALB_ENG> LIKE LINE OF IT_HALB_ENG.

  CLEAR: W_MATNR_NC, W_COLOR,
         W_TASKNAME, W_RCV_JOBS, W_SND_JOBS, W_EXCEP_FLAG,
         W_I, W_NO_TIMES, W_REM, W_FRM, W_TO, W_LINES.

  R_MTART-SIGN = 'E'.
  R_MTART-OPTION = 'EQ'.
  R_MTART-LOW = 'ROH'.
  APPEND R_MTART.
  R_MTART-LOW = 'ROH1'.
  APPEND R_MTART.

  SELECT MATNR MARC~WERKS WRK02 INTO TABLE IT_T460A
                                FROM MARC
                                INNER JOIN T460A
                                ON T460A~WERKS = MARC~WERKS
                                AND T460A~SOBSL = MARC~SOBSL
                               FOR ALL ENTRIES IN IT_HALB_EXP
                                WHERE MATNR EQ IT_HALB_EXP-MATNR
                                AND  MARC~WERKS EQ IT_HALB_EXP-WERKS.
  IF SY-SUBRC NE 0.
  ELSE.
    SORT IT_T460A BY MATNR WERKS.
    LOOP AT IT_HALB_EXP ASSIGNING <FS_HALB_EXP>.
      READ TABLE IT_T460A INTO WA_T460A
                          WITH KEY MATNR = <FS_HALB_EXP>-MATNR
                                   WERKS = <FS_HALB_EXP>-WERKS
                                   BINARY SEARCH.
      IF SY-SUBRC NE 0.
      ELSE.
        IF WA_T460A-WRK02 IS INITIAL.
        ELSE.
          <FS_HALB_EXP>-WERKS = WA_T460A-WRK02.
          WA_HALB-WERKS       = WA_T460A-WRK02.
          MODIFY IT_HALB FROM WA_HALB TRANSPORTING WERKS
                          WHERE MATNR = <FS_HALB_EXP>-MATNR.
          CLEAR: WA_HALB, WA_T460A.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  DESCRIBE TABLE IT_HALB_EXP LINES W_LINES.
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
    LOOP AT IT_HALB_EXP ASSIGNING <FS_HALB_EXP> FROM W_FRM TO W_TO.
      R_BOM_TOPMAT-SIGN = 'I'.
      R_BOM_TOPMAT-OPTION = 'EQ'.
      R_BOM_TOPMAT-LOW = <FS_HALB_EXP>-MATNR.
      COLLECT R_BOM_TOPMAT.
      CLEAR: R_BOM_TOPMAT.
** change by Furong on 05/04/2006
      DO.
        CALL FUNCTION 'Z_FFTZ_EXP_BOM'
          STARTING NEW TASK W_TASKNAME
          DESTINATION IN GROUP P_SRVGRP
          PERFORMING FSC_BOM_EXP ON END OF TASK
          EXPORTING
            P_CAPID  = 'PP01'
            P_DATUV  = <FS_HALB_EXP>-DATUV
            P_EMENG  = <FS_HALB_EXP>-MENGE
            P_MEHRS  = 'X'
            P_MMORY  = '1'
            P_MTNRV  = <FS_HALB_EXP>-MATNR
            P_STLAL  = <FS_HALB_EXP>-STLAL
            P_STLAN  = '1'
            P_WERKS  = <FS_HALB_EXP>-WERKS
            P_PAMATNR = <FS_HALB_EXP>-MATNR
          TABLES
            P_STPOX  = IT_FSC_STPOX
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
*              EXIT.
            ENDIF.
        ENDCASE.
      ENDDO.
** end of change
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
  ENDWHILE.

** Added on 1/24/2007 by Furong

  CLEAR: W_SND_JOBS, W_RCV_JOBS.
  LOOP AT IT_HALB_ENG ASSIGNING <FS_HALB_ENG>.
    R_BOM_TOPMAT-SIGN = 'I'.
    R_BOM_TOPMAT-OPTION = 'EQ'.
    R_BOM_TOPMAT-LOW = <FS_HALB_ENG>-MATNR.
    COLLECT R_BOM_TOPMAT.
    CLEAR: R_BOM_TOPMAT.
** Change by Furong on 05/04/2006
    DO.
      CALL FUNCTION 'Z_FFTZ_EXP_BOM'
        STARTING NEW TASK W_TASKNAME
        DESTINATION IN GROUP P_SRVGRP
        PERFORMING FSC_BOM_EXP ON END OF TASK
        EXPORTING
          P_CAPID  = 'PP01'
          P_DATUV  = <FS_HALB_ENG>-DATUV
          P_EMENG  = <FS_HALB_ENG>-BDMNG
          P_MEHRS  = 'X'
          P_MMORY  = '1'
          P_MTNRV  = <FS_HALB_ENG>-MATNR
          P_STLAL  = <FS_HALB_ENG>-STLAL
          P_STLAN  = '1'
          P_WERKS  = <FS_HALB_ENG>-WERKS
          P_PAMATNR = <FS_HALB_ENG>-PMATNR
        TABLES
          P_STPOX  = IT_FSC_STPOX
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
*              EXIT.
          ENDIF.
      ENDCASE.
    ENDDO.
** end of change
  ENDLOOP.

* Replace WAIT statement for loop

*  WAIT UNTIL w_rcv_jobs >= w_snd_jobs.

  DO.
    WAIT UNTIL W_RCV_JOBS >= W_SND_JOBS.
    IF W_RCV_JOBS >= W_SND_JOBS.
      EXIT.
    ENDIF.
  ENDDO.

** end of change

  CLEAR: R_BOM_TOPMAT.
  CHECK NOT IT_FSC_STPOX1[] IS INITIAL.
*&-----------Take the HALB material out of the main list.
  IF R_BOM_TOPMAT[] IS INITIAL.
  ELSE.
    DELETE IT_PLANORD_INFO WHERE CMATNR IN R_BOM_TOPMAT.
  ENDIF.
  LOOP AT IT_FSC_STPOX1 ASSIGNING <FS_COM>.
    READ TABLE IT_MAT_TYPE WITH TABLE KEY MATNR = <FS_COM>-IDNRK
                                          MTART = 'ROH'
                                                 TRANSPORTING NO FIELDS.
*&----Get rid of the color from material number.
    IF SY-SUBRC EQ 0.
      PERFORM SEPARATE_COLOR USING   <FS_COM>-IDNRK 'ROH'
                            CHANGING W_MATNR_NC
                                     W_COLOR.
      WA_COLOR_PARTS-MATNR = <FS_COM>-IDNRK.
      COLLECT WA_COLOR_PARTS INTO IT_COLOR_PARTS.
    ELSE.
      W_MATNR_NC = <FS_COM>-IDNRK.
      W_COLOR    = SPACE.
    ENDIF.
    WA_BOM_COM-PLNUM = <FS_COM>-PLNUM.
    WA_BOM_COM-VBELN = <FS_COM>-VBELN.
    WA_BOM_COM-MATNR = <FS_COM>-PMATNR.
    WA_BOM_COM-STLAL = <FS_COM>-STLAL1.
    WA_BOM_COM-DATUV = <FS_COM>-DATUV1.
    WA_BOM_COM-IDNRK = W_MATNR_NC.
    WA_BOM_COM-WERKS = <FS_COM>-WERKS.
    WA_BOM_COM-MENGE = <FS_COM>-MENGE.      "perlm
    WA_BOM_COM-MNGKO = <FS_COM>-MNGKO.
    WA_BOM_COM-MEINS = <FS_COM>-MEINS.
    COLLECT WA_BOM_COM INTO IT_BOM_COM.
    CLEAR: WA_BOM_COM, WA_COLOR_PARTS, W_MATNR_NC, W_COLOR.
  ENDLOOP.

  LOOP AT IT_HALB_ENG.
    LOOP AT IT_BOM_COM WHERE MATNR = IT_HALB_ENG-MATNR
                         AND DATUV = IT_HALB_ENG-DATUV.
      IT_BOM_COM-MATNR = IT_HALB_ENG-PMATNR.
      IT_BOM_COM-DATUV = IT_HALB_ENG-PDATUV.
      MODIFY IT_BOM_COM.
    ENDLOOP.
  ENDLOOP.
  SORT: IT_HALB BY MATNR PMATNR WERKS STLAL DATUV.

  LOOP AT IT_HALB ASSIGNING <FS_HALB>.
    LOOP AT IT_BOM_COM INTO WA_BOM_COM
                       WHERE MATNR = <FS_HALB>-MATNR
                       AND   STLAL = <FS_HALB>-STLAL
                       AND   DATUV = <FS_HALB>-DATUV.
      WA_PLANORD_INFO-PLNUM = <FS_HALB>-PLNUM.
      WA_PLANORD_INFO-VBELN = <FS_HALB>-VBELN.
      WA_PLANORD_INFO-PMATNR = <FS_HALB>-PMATNR.
      WA_PLANORD_INFO-CMATNR = WA_BOM_COM-IDNRK.
      WA_PLANORD_INFO-PLANT  = WA_BOM_COM-WERKS.
      WA_PLANORD_INFO-REQQTY = WA_BOM_COM-MENGE.
      WA_PLANORD_INFO-MENGE  =  <FS_HALB>-MENGE * WA_BOM_COM-MNGKO.
      WA_PLANORD_INFO-MEINS = WA_BOM_COM-MEINS.
      APPEND WA_PLANORD_INFO TO IT_PLANORD_INFO.
      CLEAR WA_PLANORD_INFO.
    ENDLOOP.
  ENDLOOP.

*  LOOP AT it_halb_eng ASSIGNING <fs_halb_eng>.
*    LOOP AT it_bom_com INTO wa_bom_com
*                       WHERE matnr = <fs_halb_eng>-matnr
**                       AND   stlal = <fs_halb_eng>-stlal
**                       AND   datuv = <fs_halb_eng>-datuv.
*                       and datuv = sy-datum.
**      wa_planord_info-plnum = <fs_halb_eng>-plnum.
**      wa_planord_info-vbeln = <fs_halb_eng>-vbeln.
*      wa_planord_info-pmatnr = <fs_halb_eng>-pmatnr.
*      wa_planord_info-cmatnr = wa_bom_com-idnrk.
*      wa_planord_info-plant  = wa_bom_com-werks.
*      wa_planord_info-reqqty = wa_bom_com-menge.
*      wa_planord_info-menge  =  <fs_halb_eng>-bdmng * wa_bom_com-mngko.
*      wa_planord_info-meins = wa_bom_com-meins.
*      APPEND wa_planord_info TO it_planord_info.
*      CLEAR wa_planord_info.
*    ENDLOOP.
*  ENDLOOP.

  CLEAR: W_MATNR_NC, W_COLOR,
         W_TASKNAME, W_RCV_JOBS, W_SND_JOBS, W_EXCEP_FLAG,
         W_I, W_NO_TIMES, W_MAX, W_FREE, W_FRM, W_TO.

ENDFORM.                    " get_bom_info
*&---------------------------------------------------------------------*
*&      Form  ftz_sum_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FTZ_SUM_NEW.

  DATA: BEGIN OF WA_SPNM_SUM,
          MATNR  LIKE MARA-MATNR,
          ORDNUM LIKE ZTMM_6026_01-ORDERNUMWORK,
          MENGE LIKE MSEG-MENGE,
        END OF WA_SPNM_SUM.
  DATA: IT_SPNM_SUM LIKE TABLE OF WA_SPNM_SUM.

  LOOP AT IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>.
    WA_SPNM_SUM-MATNR  = <FS_ZTMM_6026_01>-MATNR.
    WA_SPNM_SUM-ORDNUM = <FS_ZTMM_6026_01>-ORDERNUMWORK.
    WA_SPNM_SUM-MENGE  = <FS_ZTMM_6026_01>-MENGE.
    COLLECT WA_SPNM_SUM INTO IT_SPNM_SUM.
    CLEAR WA_SPNM_SUM.
  ENDLOOP.

  REFRESH IT_ZTMM_6026_01_TMP.

*  it_ztmm_6026_01_tmp[] = it_ztmm_6026_01.
*  refresh it_ztmm_6026_01.

  SORT: IT_SPNM_SUM BY MATNR ORDNUM,
        IT_ZTMM_6026_01 BY MATNR ORDERNUMWORK.

  LOOP AT IT_SPNM_SUM INTO WA_SPNM_SUM.
*      read table it_ztmm_6026_01_tmp assigning <fs_ztmm_6026_01>
*                                   with key matnr = wa_spnm_sum-matnr
*                                     ordernumwork = wa_spnm_sum-ordnum
*                                     binary search.
    READ TABLE IT_ZTMM_6026_01 ASSIGNING <FS_ZTMM_6026_01>
                                    WITH KEY MATNR = WA_SPNM_SUM-MATNR
                                      ORDERNUMWORK = WA_SPNM_SUM-ORDNUM
                                      BINARY SEARCH.
    IF SY-SUBRC NE 0.
      CLEAR: WA_ZTMM_6026_01.
      CONTINUE.
    ELSE.
      WA_ZTMM_6026_01 = <FS_ZTMM_6026_01>.
      WA_ZTMM_6026_01-MENGE = WA_SPNM_SUM-MENGE.
      APPEND WA_ZTMM_6026_01 TO IT_ZTMM_6026_01_TMP.
      CLEAR: WA_ZTMM_6026_01.
    ENDIF.
  ENDLOOP.
  IT_ZTMM_6026_01[] = IT_ZTMM_6026_01_TMP[].
  FREE: IT_ZTMM_6026_01_TMP.

ENDFORM.                    " ftz_sum_new
