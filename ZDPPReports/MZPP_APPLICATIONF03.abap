*----------------------------------------------------------------------*
*   INCLUDE MZPP_APPLICATIONF03                                        *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  search_data_app219
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DATA_APP219.
  DATA: L_MODEL TYPE ZTBM_ABXOPVDT-CARX,
        L_NAME  TYPE ZTBM_ABXOPVDT-CLNO,
        L_DESC  TYPE ZTBM_ABXOPVDT-CLNM.
  CLEAR: IT_APP219, IT_APP219[],
         IT_OPT1_APP219, IT_OPT1_APP219[],
         IT_OPT2_APP219, IT_OPT2_APP219[].
  L_MODEL = WA_MODEL+00(02).
  SELECT *
    INTO CORRESPONDING FIELDS OF IT_APP219
    FROM  ZTBM_ABXOPVDT AS AB
    WHERE  AB~CARX  EQ  L_MODEL.
    MOVE WA_MODEL TO IT_APP219-MODEL.
    APPEND IT_APP219.

  ENDSELECT.

  DESCRIBE TABLE  IT_APP219  LINES  G_IT_LINE_APP219.

  IF  G_IT_LINE_APP219  IS  INITIAL.
    EXIT.
  ENDIF.

  REFRESH: IT_OPT1_APP219, IT_OPT2_APP219.
  CLEAR  : IT_OPT1_APP219, IT_OPT2_APP219.

  LOOP AT IT_APP219.
    MOVE-CORRESPONDING IT_APP219 TO IT_OPT1_APP219.
    APPEND IT_OPT1_APP219.
  ENDLOOP.
*
  DESCRIBE TABLE  IT_OPT1_APP219  LINES  G_OPT1_LINES_APP219.
** Change on 08/010/07
  SORT IT_OPT1_APP219 BY MODEL CLNO.
**
ENDFORM.                    " search_data_app219
*&---------------------------------------------------------------------*
*&      Form  VEHICLE_NUMBER_SEARCH
*&---------------------------------------------------------------------*
*       Searching V/M's Data & Setting Screen's Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VEHICLE_NUMBER_SEARCH_APP236.
* CHECK  st_key_app236-inqopt  EQ  'VEH'.
  IF  WA_MODEL         IS  INITIAL.
    MESSAGE S000  WITH 'Input Model code !'.
    G_CRSR_FLD_APP236 = 'WA_MODEL'       .
  ENDIF.

  PERFORM  ITAB_AND_VARIABLE_INIT_APP236.

  CONCATENATE  WA_MODEL ST_APP236-BODYNO  INTO  G_EQUNR_APP236.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = G_EQUNR_APP236
            DISPLAY      = 'D'
       TABLES
            VAL_TABLE    = IT_VMV_APP236
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC NE 0.
    MESSAGE S003 WITH G_EQUNR_APP236 'Vehicle History Not found!'.
    CLEAR ST_APP236.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_VMV_APP236  LINES  IT_LINES_APP236.

  CHECK  IT_LINES_APP236 > 0.
  CLEAR ST_APP236.
* PROGRESS DATA TABLE
  PERFORM  CREATE_TABLE_IT_WIP_APP236.  "PROGRESS INITIAL LOAD
* VEHICLE DATA
  PERFORM  GENERAL_VEH_HISTORY_APP236.  "move data to screen

* 219 option table made
  PERFORM  CREATE_219_OPTION_TABLE.
* Order table made
  PERFORM  CREATE_ORDER_TABLE.
* Airbag  table made
  PERFORM  CREATE_AIRBAG_TABLE.
** Shop Date & Serial
* actual date & time
  PERFORM CREATE_RP_TABLE.
ENDFORM.                    " VEHICLE_NUMBER_SEARCH

*&---------------------------------------------------------------------*
*&      Form  vmdata_to_screen_field
*&---------------------------------------------------------------------*
*       Searching a Value Per Characteristic
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM  VMDATA_TO_SCREEN_FIELD_APP236.
  DATA: W_USAGE,
        W_DATE(10).
  CASE  IT_VMV_APP236-ATNAM.
    WHEN 'P_MODEL'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-MODEL.
    WHEN 'P_BODY_SERIAL'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BODYNO.
    WHEN 'P_WORK_ORDER'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-WON.
    WHEN 'P_EXT_COLOR'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-EXTC.
    WHEN 'P_INT_COLOR'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-INTC.
    WHEN 'P_MI'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-MI.
    WHEN 'P_OCN'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-OCN.
    WHEN 'P_VERSION'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-VERS.
    WHEN 'P_RP_STATUS'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-STATS.
    WHEN 'P_BC_WORK_ORDER'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BCS_WON.
    WHEN 'P_BC_EXT_COLOR'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BCS_EXTC.
    WHEN 'P_BC_INT_COLOR'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BCS_INTC.
    WHEN 'P_BC_MI'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BCS_MI.
    WHEN 'P_BC_OCN'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BCS_OCN.
    WHEN 'P_BC_VERSION'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BCS_VER.
    WHEN 'P_BC_CHANGE_DATE'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-BCS_CDT.
    WHEN 'P_DESTINATION_CODE'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-DESTC.
    WHEN 'P_MITU'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-MITU.
    WHEN 'P_MITU_DATE'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-MITUDAT.
    WHEN 'P_VIN'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-VIN.
    WHEN 'P_COATING'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-COT.
*    WHEN 'P_DELIVERY'.
*      PERFORM  filed_changing_app236  CHANGING  st_app236-deliv.
    WHEN 'P_LC_NO'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-LCN.
    WHEN 'P_SEQUENCE_DATE'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-SEQDAT.
    WHEN 'P_SEQUENCE_SERIAL'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-SEQSER.
    WHEN 'P_SEQUENCE_CODE'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-SEQCOD.
    WHEN 'P_PROBLEM'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-PCT.
    WHEN 'P_ENGINE_NO'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-ENGNO.
    WHEN 'P_TM_NO'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-TMNO.
    WHEN 'P_KEY_NO'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-KEYNO.
    WHEN 'P_CONSIGN_VENDOR'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-LIFNR.
*    WHEN 'P_CONSIGN'.
*      PERFORM  filed_changing_app236  CHANGING  st_app236-cons.
    WHEN 'P_AUTO_POOL_LOC'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-LOT.
    WHEN 'P_TRIM_PLANT_NO'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  P_TRIM01_APP236.
    WHEN 'P_TRIM_LINE_NO'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  P_TRIM02_APP236.
    WHEN 'P_PLAN_ORDER'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-PLNUM.
    WHEN 'P_SALES_ORDER'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  ST_APP236-VBELN.
    WHEN 'P_USAGE_CAR'.
      PERFORM  FILED_CHANGING_APP236  CHANGING  W_USAGE.
      IF W_USAGE = 'S'.
        ST_APP236-USAGE = 'Scrapped Car'.
      ELSEIF W_USAGE = 'D'.
        ST_APP236-USAGE = 'Disposed Car'.
      ELSEIF W_USAGE = 'T'.
        ST_APP236-USAGE = 'Test Car'.
      ELSE.
        CLEAR ST_APP236-USAGE.
      ENDIF.
    WHEN 'P_RP25_ACTUAL_DATE' OR 'P_RP27_ACTUAL_DATE'.
      ST_APP236-DELIV = 'X'.
    WHEN 'P_RP24_ACTUAL_DATE' OR 'P_RP26_ACTUAL_DATE'.
      ST_APP236-CONS = 'X'.
    WHEN 'P_RP02_ACTUAL_DATE'.
      W_DATE = IT_VMV_APP236-ATWRT+0(8).
      CONCATENATE W_DATE+4(2) '/' W_DATE+6(2)
                  '/' W_DATE+0(4) INTO W_DATE.
      ST_APP236-P_BODY = W_DATE.
*    WHEN 'P_RP02_SERIAL'.
*    PERFORM  filed_changing_app236  CHANGING  st_app236-l_body.
    WHEN 'P_RP04_ACTUAL_DATE'.
      W_DATE = IT_VMV_APP236-ATWRT+0(8).
      CONCATENATE W_DATE+4(2) '/' W_DATE+6(2)
                  '/' W_DATE+0(4) INTO W_DATE.
      ST_APP236-P_PAINT = W_DATE.
*    WHEN 'P_RP04_SERIAL'.
*    PERFORM  filed_changing_app236  CHANGING  st_app236-l_paint.
    WHEN 'P_RP17_ACTUAL_DATE'.
      W_DATE = IT_VMV_APP236-ATWRT+0(8).
      CONCATENATE W_DATE+4(2) '/' W_DATE+6(2)
                  '/' W_DATE+0(4) INTO W_DATE.
      ST_APP236-P_TRIM = W_DATE.
*    WHEN 'P_RP17_SERIAL'.
*      PERFORM  filed_changing_app236  CHANGING  st_app236-l_trim.
    WHEN 'P_RP19_ACTUAL_DATE'.
      W_DATE = IT_VMV_APP236-ATWRT+0(8).
      CONCATENATE W_DATE+4(2) '/' W_DATE+6(2)
                  '/' W_DATE+0(4) INTO W_DATE.
      ST_ISS_APP236-CCDAT = W_DATE.

    WHEN 'P_RP23_ACTUAL_DATE'.
      W_DATE = IT_VMV_APP236-ATWRT+0(8).
      CONCATENATE W_DATE+4(2) '/' W_DATE+6(2)
                  '/' W_DATE+0(4) INTO W_DATE.
      ST_APP236-P_MGATE = W_DATE.

  ENDCASE.

* REPORTING POINT DATA
  CHECK  IT_VMV_APP236-ATNAM(4) EQ  'P_RP'.

  DATA: L_PROGRESS(2)  TYPE N.
  CLEAR: G_SHOP_DATE_APP236,
         G_ACT_DATE_APP236,
         G_SERIAL_APP236.
  DO  27 TIMES.
    MOVE   SY-INDEX  TO   L_PROGRESS.
    CONCATENATE 'P_RP' L_PROGRESS '_SHOP_DATE'
      INTO G_SHOP_DATE_APP236.
    CONCATENATE 'P_RP' L_PROGRESS '_SERIAL'
      INTO G_SERIAL_APP236.
    CONCATENATE 'P_RP' L_PROGRESS '_ACTUAL_DATE'
      INTO G_ACT_DATE_APP236.

    IF IT_VMV_APP236-ATNAM  EQ  G_SHOP_DATE_APP236.
      READ TABLE IT_WIP_APP236  WITH KEY PROGRESS =  L_PROGRESS.
      IF SY-SUBRC EQ 0.
        IF IT_VMV_APP236-ATWRT IS INITIAL
           OR IT_VMV_APP236-ATWRT = '00/00/0000'.
          CLEAR: IT_WIP_APP236-SHOP_DAT.
        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_DATEX_INPUT'
               EXPORTING
                    INPUT  = IT_VMV_APP236-ATWRT
               IMPORTING
                    OUTPUT = IT_WIP_APP236-SHOP_DAT.
        ENDIF.
*        MOVE IT_VMV_APP236-atwrt TO  IT_WIP_APP236-shop_dat.
        MODIFY  IT_WIP_APP236  INDEX SY-TABIX.
      ENDIF.
      EXIT.
    ENDIF.
    IF IT_VMV_APP236-ATNAM  EQ  G_ACT_DATE_APP236.
      READ TABLE IT_WIP_APP236  WITH KEY PROGRESS =  L_PROGRESS.
      IF SY-SUBRC EQ 0.
        MOVE IT_VMV_APP236-ATWRT TO  IT_WIP_APP236-ACT_DAT.
        MODIFY  IT_WIP_APP236 INDEX SY-TABIX.
      ENDIF.
      EXIT.
    ENDIF.

    IF IT_VMV_APP236-ATNAM  EQ  G_SERIAL_APP236.
      READ TABLE IT_WIP_APP236  WITH KEY PROGRESS =  L_PROGRESS.
      IF SY-SUBRC EQ 0.
        MOVE IT_VMV_APP236-ATWRT TO  IT_WIP_APP236-SERIAL.
        MODIFY  IT_WIP_APP236 INDEX SY-TABIX.
      ENDIF.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " vmdata_to_screen_field

*&---------------------------------------------------------------------*
*&      Form  FILED_CHANGING
*&---------------------------------------------------------------------*
*       Moving V/M's Date Type Data to a parameter
*----------------------------------------------------------------------*
*      <--P_ST_APP236_MODEL  text
*----------------------------------------------------------------------*
FORM FILED_CHANGING_APP236 CHANGING P_ST_APP236.
  MOVE  IT_VMV_APP236-ATWRT     TO  P_ST_APP236.
ENDFORM.                    " FILED_CHANGING

*&---------------------------------------------------------------------*
*&      Form  KEY_FIELD_ATTR
*&---------------------------------------------------------------------*
*       Setting Required Keys' Attribute
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM KEY_FIELD_ATTR_APP236.
  CHECK OK_CODE EQ 'ATTR'.

  CLEAR: ST_APP236, IT_WIP_APP236, ST_ISS_APP236.
  REFRESH: IT_WIP_APP236.

  CASE ST_KEY_APP236-INQOPT.
    WHEN  'VEH'.
      G_ATTR_APP236 = '1'.
    WHEN  'VIN'.
      G_ATTR_APP236 = '2'.
    WHEN  'ENG'.
      G_ATTR_APP236 = '3'.
    WHEN  'TMN'.
      G_ATTR_APP236 = '4'.
  ENDCASE.
ENDFORM.                    " KEY_FIELD_ATTR

*&---------------------------------------------------------------------*
*&      Form  equi_master_check
*&---------------------------------------------------------------------*
*       Checking V/M's Number
*----------------------------------------------------------------------*
*      -->P_G_EQUNR  text
*      -->P_G_EQUICHK  text
*----------------------------------------------------------------------*
FORM EQUI_MASTER_CHECK_APP236 USING    P_EQUNR .
  SELECT  SINGLE  EQKTX  INTO  ST_ISS_APP236-EQKTX
    FROM  EQKT
    WHERE EQUNR  EQ  P_EQUNR
      AND SPRAS  EQ  SY-LANGU.
  IF SY-SUBRC NE 0.
    MESSAGE  S000 WITH 'Vehicle Master Not Found !'.
    PERFORM CLEAR_TABLE_2106                       .
  ENDIF.
ENDFORM.                    " equi_master_check

*&---------------------------------------------------------------------*
*&      Form  vin_number_search
*&---------------------------------------------------------------------*
*       Searching Data By V/M NO.
*       Setting Screens' Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM VIN_NUMBER_SEARCH_APP236.
  IF  ST_APP236-VIN  IS  INITIAL.
    MESSAGE S000  WITH 'Input VIN No !'.
    G_CRSR_FLD_APP236 = 'ST_APP236-VIN'.
  ENDIF.
*  IF st_key_app236-inqopt EQ st_code_app236-inqopt  AND
*     st_app236-vin  EQ st_code_app236-vin   .
*    EXIT.
*  ENDIF.
*
*  MOVE   st_key_app236-inqopt   TO  st_code_app236-inqopt.
*  MOVE   st_app236-vin      TO  st_code_app236-vin.

  PERFORM  ITAB_AND_VARIABLE_INIT_APP236.

  PERFORM  CONVERSION_ATINN_CALL  USING  'P_VIN'  G_VIN_APP236.

  SELECT   SINGLE   OBJEK
    INTO   G_EQUNR_APP236
    FROM   AUSP
    WHERE  ATINN  EQ   G_VIN_APP236
      AND  ATWRT  EQ   ST_APP236-VIN.

  IF SY-SUBRC NE 0.
    MESSAGE S000  WITH 'VIN number not found !'.
    CLEAR ST_APP236.
*   MOVE  st_code_app236-vin TO  st_app236-vin.
    EXIT.
  ENDIF.

  PERFORM  EQUI_MASTER_CHECK_APP236  USING  G_EQUNR_APP236  .
*                                           g_equichk_app236.

* CHECK  g_equichk_app236  EQ  space.
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = G_EQUNR_APP236
       TABLES
            VAL_TABLE    = IT_VMV_APP236
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  DESCRIBE TABLE IT_VMV_APP236  LINES  IT_LINES_APP236.

  CHECK  IT_LINES_APP236 > 0.
  CLEAR ST_APP236.

* PROGRESS DATA TABLE
  PERFORM  CREATE_TABLE_IT_WIP_APP236.  "PROGRESS INITIAL LOAD
* VEHICLE DATA
  PERFORM  GENERAL_VEH_HISTORY_APP236.  "move data to screen

* 219 option table made
  PERFORM  CREATE_219_OPTION_TABLE.
* Order table made
  PERFORM  CREATE_ORDER_TABLE.
* Airbag  table made
  PERFORM  CREATE_AIRBAG_TABLE.
* Shop Date & Serial
  PERFORM CREATE_RP_TABLE.
*
*  MOVE-CORRESPONDING  st_app236  TO  st_code_app236.
*  MOVE-CORRESPONDING  st_iss_app236  TO  st_code_app236.
*  MOVE-CORRESPONDING  st_key_app236  TO  st_code_app236.
ENDFORM.                    " vin_number_search

*&---------------------------------------------------------------------*
*&      Form  engine_number_search
*&---------------------------------------------------------------------*
*       Searching Data By a Engine No.
*       Setting Screens' Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENGINE_NUMBER_SEARCH_APP236.
  DATA  L_ENG_LINES TYPE I.
  IF  ST_APP236-ENGNO  IS  INITIAL.
    MESSAGE S000  WITH 'Input Engine No !'.
    G_CRSR_FLD_APP236 = 'ST_APP236-ENGNO'.
  ENDIF.
*  IF st_key_app236-inqopt EQ st_code_app236-inqopt      AND
*     st_app236-engno  EQ st_code_app236-engno   .
*    EXIT.
*  ENDIF.
*
*  MOVE   st_key_app236-inqopt   TO  st_code_app236-inqopt.
*  MOVE   st_app236-engno    TO  st_code_app236-engno.

  PERFORM  ITAB_AND_VARIABLE_INIT_APP236.
*
  PERFORM  CONVERSION_ATINN_CALL  USING  'P_ENGINE_NO'  G_ENGNO_APP236.
* DUP ENGINE CHECK ------------------------------------
  REFRESH: IT_ENG_APP236.  CLEAR IT_ENG_APP236.
  SELECT   OBJEK  ATWRT
    INTO   (IT_ENG_APP236-OBJEK, IT_ENG_APP236-ENGNO)
                                  " The biggest SEQUENCE_CODE
    FROM   AUSP
    WHERE  ATINN  EQ   G_ENGNO_APP236
      AND  ATWRT  EQ   ST_APP236-ENGNO.
    APPEND IT_ENG_APP236.
  ENDSELECT.

  DESCRIBE TABLE IT_ENG_APP236  LINES  L_ENG_LINES.

  IF  L_ENG_LINES IS INITIAL.
    MESSAGE S000  WITH 'Engine number not found !'.
*   CLEAR st_app236.
*   MOVE st_code_app236-engno  TO  st_app236-engno.
    EXIT.
  ENDIF.
  SORT IT_ENG_APP236   BY  OBJEK .
  READ TABLE IT_ENG_APP236  INDEX  1.

  G_EQUNR_APP236 = IT_ENG_APP236-OBJEK.

  IF L_ENG_LINES > 1.
    LOOP AT IT_ENG_APP236.
      CASE  SY-TABIX.
        WHEN 1.
          ST_ISS_APP236-DUPENG1 = IT_ENG_APP236-OBJEK.
        WHEN 2.
          ST_ISS_APP236-DUPENG2 = IT_ENG_APP236-OBJEK.
        WHEN 3.
          ST_ISS_APP236-DUPENG3 = IT_ENG_APP236-OBJEK.
        WHEN 4.
          ST_ISS_APP236-DUPENG4 = IT_ENG_APP236-OBJEK.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDLOOP.
  ENDIF.
*------------------------------------------------------
  PERFORM  EQUI_MASTER_CHECK_APP236  USING  G_EQUNR_APP236 .
*                                    g_equichk_app236.

* CHECK  g_equichk_app236  EQ  space.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = G_EQUNR_APP236
       TABLES
            VAL_TABLE    = IT_VMV_APP236
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC NE 0.
    MESSAGE S003 WITH G_EQUNR_APP236 'Vehicle History Not found!'.
    REFRESH: IT_WIP_APP236.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_VMV_APP236  LINES  IT_LINES_APP236.

  CHECK  IT_LINES_APP236 > 0.
* CLEAR st_app236.
* PROGRESS DATA TABLE
  PERFORM  CREATE_TABLE_IT_WIP_APP236.  "PROGRESS INITIAL LOAD

* VEHICLE DATA
  PERFORM  GENERAL_VEH_HISTORY_APP236.  "move data to screen

* 219 option table made
  PERFORM  CREATE_219_OPTION_TABLE.
* Order table made
  PERFORM  CREATE_ORDER_TABLE.
* Airbag  table made
  PERFORM  CREATE_AIRBAG_TABLE.
* Shop Date & Serial
  PERFORM CREATE_RP_TABLE.
*
*  MOVE-CORRESPONDING  st_app236  TO  st_code_app236.
*  MOVE-CORRESPONDING  st_iss_app236  TO  st_code_app236.
*  MOVE-CORRESPONDING  st_key_app236  TO  st_code_app236.
ENDFORM.                    " engine_number_search

*&---------------------------------------------------------------------*
*&      Form  tm_number_search
*&---------------------------------------------------------------------*
*       Searching Data By a T/M No.
*       Setting Screens' Fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TM_NUMBER_SEARCH_APP236.
  IF  ST_APP236-VIN  IS  INITIAL.
    MESSAGE S000  WITH 'Input TM No !'.
    G_CRSR_FLD_APP236 = 'ST_APP236-TMNO'.
  ENDIF.
*  IF st_key_app236-inqopt EQ st_code_app236-inqopt    AND
*     st_app236-tmno  EQ st_code_app236-tmno   .
*    EXIT.
*  ENDIF.
*
*  MOVE   st_key_app236-inqopt   TO  st_code_app236-inqopt.
*  MOVE   st_app236-tmno     TO  st_code_app236-tmno.

  PERFORM  ITAB_AND_VARIABLE_INIT_APP236.

  PERFORM  CONVERSION_ATINN_CALL  USING  'P_TM_NO'  G_TMNO_APP236.

  SELECT   SINGLE   OBJEK
    INTO   G_EQUNR_APP236                " The biggest
    FROM   AUSP
    WHERE  ATINN  EQ   G_TMNO_APP236
      AND  ATWRT  EQ   ST_APP236-TMNO.

  IF SY-SUBRC NE 0.
    MESSAGE S000  WITH 'T/M number not found !'.
*    CLEAR st_app236.
*    MOVE st_code_app236-tmno  TO  st_app236-tmno.
    EXIT.
  ENDIF.

  PERFORM  EQUI_MASTER_CHECK_APP236 USING  G_EQUNR_APP236  .
*                                          g_equichk_app236.

* CHECK  g_equichk_app236  EQ  space.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = G_EQUNR_APP236
       TABLES
            VAL_TABLE    = IT_VMV_APP236
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC NE 0.
    MESSAGE S003 WITH G_EQUNR_APP236 'Vehicle History Not found!'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE IT_VMV_APP236  LINES  IT_LINES_APP236.

  CHECK  IT_LINES_APP236 > 0.
* CLEAR st_app236.
* PROGRESS DATA TABLE
  PERFORM  CREATE_TABLE_IT_WIP_APP236.  "PROGRESS INITIAL LOAD
* VEHICLE DATA
  PERFORM  GENERAL_VEH_HISTORY_APP236.  "move data to screen

* 219 option table made
  PERFORM  CREATE_219_OPTION_TABLE.
* Order table made
  PERFORM  CREATE_ORDER_TABLE.
* Airbag  table made
  PERFORM  CREATE_AIRBAG_TABLE.
* Shop Date & Serial
  PERFORM CREATE_RP_TABLE.
*
*  MOVE-CORRESPONDING  st_app236  TO  st_code_app236.
*  MOVE-CORRESPONDING  st_iss_app236  TO  st_code_app236.
*  MOVE-CORRESPONDING  st_key_app236  TO  st_code_app236.
ENDFORM.                    " tm_number_search

*&---------------------------------------------------------------------*
*&      Form  CONVERSION_ATINN_CALL
*&---------------------------------------------------------------------*
*       Characteristic Name Conversion To Internal No. of Char
*----------------------------------------------------------------------*
*      -->P_0274   text
*      -->P_L_MODEL  text
*----------------------------------------------------------------------*
FORM CONVERSION_ATINN_CALL USING    P_VALUE  "Characteristic Name
                                    P_ATINN. "internal no of char

  CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
       EXPORTING
            INPUT  = P_VALUE
       IMPORTING
            OUTPUT = P_ATINN.
ENDFORM.                    " CONVERSION_ATINN_CALL

*&---------------------------------------------------------------------*
*&      Form  CREATE_TABLE_IT_WIP_APP236
*&---------------------------------------------------------------------*
*       Setting RP's No.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_TABLE_IT_WIP_APP236.
  CLEAR IT_WIP_APP236.
  DO  27 TIMES.
    MOVE  SY-INDEX   TO   IT_WIP_APP236-PROGRESS.
    APPEND  IT_WIP_APP236.
  ENDDO.

ENDFORM.                    " CREATE_TABLE_IT_WIP_APP236
*&---------------------------------------------------------------------*
*&      Form  GENERAL_VEH_HISTORY
*&---------------------------------------------------------------------*
*       Setting Screen's Fields By Internal Table's Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GENERAL_VEH_HISTORY_APP236.
  DATA: L_HEADER TYPE MARA-MATNR.
  LOOP AT IT_VMV_APP236.
    PERFORM  VMDATA_TO_SCREEN_FIELD_APP236.
  ENDLOOP.
* Read Car's Description
  PERFORM READ_CAR_DESC USING    ST_APP236-WON
                        CHANGING ST_ISS_APP236-EQKTX.
ENDFORM.                    " GENERAL_VEH_HISTORY

*&---------------------------------------------------------------------*
*&      Form  219_OPTION_DISPLAY
*&---------------------------------------------------------------------*
*       Calling a Screen For 219 Option Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM 219_OPTION_DISPLAY.
  READ TABLE IT_219_APP236 INDEX 1.
  IF SY-SUBRC EQ 0.
    CALL SCREEN  110  STARTING AT  48  8  ENDING AT 111 23.
    EXIT.
  ELSE.
    MESSAGE S000 WITH 'There is no 219 option information !'.
  ENDIF.
ENDFORM.                    " 219_OPTION_DISPLAY

*&---------------------------------------------------------------------*
*&      Form  option_219_value
*&---------------------------------------------------------------------*
*       Searching 219's Values By Table 'ZTBM_219_VALUE'
*----------------------------------------------------------------------*
*      -->P_IT_WO_MODEL  text
*      -->P_L_COL  text
*      -->P_IT_WO_ATWRT  text
*      <--P_IT_219_APP236_VAL  text
*      <--P_IT_219_APP236_VALTX  text
*----------------------------------------------------------------------*
FORM OPTION_219_VALUE USING    P_CARX  "Model
                               P_CLNO  "Column Value
                               P_VALU  "Code Value
                      CHANGING P_VANM  "Column Desc
                               P_CLNM. "Code Desc
  SELECT SINGLE VANM CLNM
    INTO (P_VANM , P_CLNM)
    FROM ZTBM_ABXOPVDT AS AB
    WHERE AB~CARX  = P_CARX  AND   "Model
          AB~CLNO  = P_CLNO  AND   "Column Value
          AB~VALU  = P_VALU    .   "Code Value
  IF SY-SUBRC <> 0 AND P_VALU <> '-'.
    P_CLNM = 'Not found'.
  ENDIF.

ENDFORM.                    " option_219_value
*&---------------------------------------------------------------------*
*&      Form  order_list_display
*&---------------------------------------------------------------------*
*       Call a Screen For Order List Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ORDER_LIST_DISPLAY.
*
  READ TABLE IT_UCPART_APP236 INDEX 1.

  IF SY-SUBRC EQ 0.
    CALL SCREEN  120  STARTING AT  50  8
                        ENDING AT 111 23.
    EXIT.
  ELSE.
    MESSAGE S000 WITH 'There is no Order information !'.
  ENDIF.
*
ENDFORM.                    " order_list_display
*&---------------------------------------------------------------------*
*&      Form  UNIQUE_PART_DISPLAY
*&---------------------------------------------------------------------*
*       Display of Unique Part's
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UNIQUE_PART_DISPLAY.

  REFRESH  IT_PART_APP236.  CLEAR IT_PART_APP236.

  LOOP AT IT_UCPART_APP236  WHERE  UCGUB EQ 'U'.
    MOVE-CORRESPONDING  IT_UCPART_APP236 TO IT_PART_APP236.
    APPEND IT_PART_APP236.
  ENDLOOP.

  G_PART_APP236 = 'U'.  "Next Color part

ENDFORM.                    " UNIQUE_PART_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  COLOR_PART_DISPLAY
*&---------------------------------------------------------------------*
*       Display of Color Part's
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLOR_PART_DISPLAY.

  REFRESH  IT_PART_APP236.  CLEAR IT_PART_APP236.

  LOOP AT IT_UCPART_APP236  WHERE  UCGUB EQ 'C'.
    MOVE-CORRESPONDING  IT_UCPART_APP236 TO IT_PART_APP236.
    APPEND IT_PART_APP236.
  ENDLOOP.

  G_PART_APP236 = 'C'. "next unique part

ENDFORM.                    " COLOR_PART_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  airbag_display
*&---------------------------------------------------------------------*
*       Calling a Screen For AirBag Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AIRBAG_DISPLAY.

  READ TABLE IT_ABAG_APP236 INDEX 1.
  IF SY-SUBRC EQ 0.
    CALL SCREEN  130  STARTING AT  48  8
                      ENDING AT 111 23.
    EXIT.
  ELSE.
    MESSAGE S000 WITH 'There is no Airbag information !'.
  ENDIF.

ENDFORM.                    " airbag_display
*&---------------------------------------------------------------------*
*&      Form  create_219_option_table
*&---------------------------------------------------------------------*
*       Searching 219 Option Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_219_OPTION_TABLE.
  CHECK NOT  ST_APP236-WON  IS INITIAL.
  CLEAR G_CUOBF_APP236.

  SELECT SINGLE MATNR INTO G_CUOBF_APP236
    FROM MARA
    WHERE MATNR EQ ST_APP236-WON.
  MOVE ST_APP236-WON TO G_CUOBF_APP236.

  CHECK NOT G_CUOBF_APP236 IS  INITIAL.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = G_CUOBF_APP236
*   MODE               = 'R'
      CTYPE              = '001'
      DISPLAY            = 'D'
    TABLES
      VAL_TABLE          = IT_WO_APP236
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

  CHECK SY-SUBRC EQ 0.
  CHECK NOT ST_APP236-MODEL IS INITIAL.

* Create 219 option table 'IT_219_APP236' ---
  REFRESH: IT_219_APP236. CLEAR IT_219_APP236.
************************************************************************
* Because of the Changed Table Relationship, Program Source was changed
* By Tonkey On 01/27/2004
* Reference Table : ZTBM_ABXOPVDT
************************************************************************
* Request No. :
************************************************************************
  DATA: L_FNAME(50)        TYPE C,
        L_NO(3)            TYPE N,
        L_ATNAM            TYPE CABN-ATNAM,
        L_INT              TYPE I,
        L_CHAR(3)          TYPE C VALUE '0'.

  DATA: L_CARX TYPE ZTBM_ABXOPVDT-CARX .
  DATA: L_COL(3)  TYPE  N.

  L_CARX = ST_APP236-MODEL+00(02).
* 219 option value update
  DO 219 TIMES.
    CLEAR IT_219_APP236.
    L_INT = L_INT + 1.
    WRITE L_INT TO L_CHAR LEFT-JUSTIFIED .
    CONCATENATE 'P_219_' L_CHAR
      INTO L_ATNAM.
    CLEAR IT_VMV_APP236.
    READ TABLE IT_VMV_APP236 WITH KEY ATNAM = L_ATNAM.
    MOVE: L_CHAR              TO L_COL                ,
          L_CHAR              TO IT_219_APP236-CLNO   ,
          IT_VMV_APP236-ATWRT TO IT_219_APP236-VALU   .
    CLEAR: IT_219_APP236-VANM,
           IT_219_APP236-CLNM.
    PERFORM OPTION_219_VALUE USING     L_CARX
                                       L_COL
                                       IT_219_APP236-VALU
                             CHANGING  IT_219_APP236-VANM
                                       IT_219_APP236-CLNM.
    APPEND IT_219_APP236.
  ENDDO.
ENDFORM.                    " create_219_option_table

*&---------------------------------------------------------------------*
*&      Form  create_order_table
*&---------------------------------------------------------------------*
*       Searching ALC' Unique & Color Part Inf.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_ORDER_TABLE.
  DATA: L_COL(3)    TYPE  N,
        L_COL_C(03) TYPE C VALUE 0,
        L_ATNAM     TYPE CABN-ATNAM,
        L_COLOR     TYPE MARA-MATNR,
        LT_COLOR LIKE TABLE OF IT_WO_APP236 WITH HEADER LINE.
*
  CLEAR:   IT_UCPART_APP236, IT_PART_APP236.

  CHECK NOT  ST_APP236-WON  IS INITIAL.
  CLEAR G_CUOBF_APP236.
  SELECT SINGLE MATNR  INTO  G_CUOBF_APP236
     FROM  MARA
     WHERE MATNR EQ ST_APP236-WON.

  CHECK NOT G_CUOBF_APP236 IS  INITIAL.
  REFRESH: IT_WO_APP236.
* W/O Selection ----------------------------------------------
  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = G_CUOBF_APP236
*   MODE               = 'R'
      CTYPE              = '001'
*   DISPLAY            = 'D'
    TABLES
      VAL_TABLE          = IT_WO_APP236
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

  CHECK SY-SUBRC EQ 0.
  SORT IT_WO_APP236 BY ATNAM.

* ALC Unique Part
  CLEAR: IT_UPART_APP236, IT_UPART_APP236[].
  L_COL_C = '0'.
  DO 200 TIMES.
    CLEAR IT_UPART_APP236.
    L_COL_C = L_COL_C + 1.
    CONDENSE L_COL_C.
    CONCATENATE 'P_ALC_U_' L_COL_C
      INTO L_ATNAM.
    CLEAR IT_WO_APP236.
    READ TABLE IT_WO_APP236 WITH KEY ATNAM = L_ATNAM.
    MOVE L_COL_C TO IT_UPART_APP236-COL.
    MOVE IT_WO_APP236-ATWRT TO IT_UPART_APP236-CODE.
    APPEND IT_UPART_APP236.
  ENDDO.

*
* ALC Color Part
  CONCATENATE ST_APP236-WON
              ST_APP236-EXTC
              ST_APP236-INTC
    INTO L_COLOR .

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
    EXPORTING
      OBJECT             = L_COLOR
*   MODE               = 'R'
      CTYPE              = '001'
*   DISPLAY            = 'D'
    TABLES
      VAL_TABLE          = LT_COLOR
      EXCEPTIONS
        NO_DATA            = 1
        ERROR_MODE         = 2
        ERROR_OBJECT       = 3
        ERROR_VALUE        = 4
        OTHERS             = 5 .

  CLEAR: IT_CPART_APP236, IT_CPART_APP236[].
  L_COL_C = '0'.
  DO 50 TIMES.
    CLEAR IT_CPART_APP236.
    L_COL_C = L_COL_C + 1.
    CONDENSE L_COL_C.
    CONCATENATE 'P_ALC_C_' L_COL_C
      INTO L_ATNAM.
    CLEAR LT_COLOR.
    READ TABLE LT_COLOR WITH KEY ATNAM = L_ATNAM.
    MOVE L_COL_C TO IT_CPART_APP236-COL.
    MOVE LT_COLOR-ATWRT TO IT_CPART_APP236-CODE.
    APPEND IT_CPART_APP236.
  ENDDO.

ENDFORM.                    " create_order_table
*&---------------------------------------------------------------------*
*&      Form  create_airbag_table
*&---------------------------------------------------------------------*
*       getting Airbag Inf. From a Internal Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_AIRBAG_TABLE.
  REFRESH: IT_ABAG_APP236.  CLEAR IT_ABAG_APP236.
  LOOP AT IT_VMV_APP236  WHERE ATNAM(8) EQ 'P_AIRBAG'.
    MOVE  IT_VMV_APP236-ATNAM   TO  IT_ABAG_APP236-AIRBAG.
    MOVE  IT_VMV_APP236-ATWRT   TO  IT_ABAG_APP236-CODE.
    APPEND  IT_ABAG_APP236.
  ENDLOOP.

ENDFORM.                    " create_airbag_table
*&---------------------------------------------------------------------*
*&      FORM  itab_and_variable_init_APP236
*&---------------------------------------------------------------------*
*       Initialization of Internal Tables
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ITAB_AND_VARIABLE_INIT_APP236.
  REFRESH: IT_WIP_APP236,
           IT_219_APP236,
           IT_ABAG_APP236,
           IT_UCPART_APP236,
           IT_PART_APP236.
  REFRESH: IT_VMV_APP236, IT_WO_APP236.
  REFRESH: IT_UPART_APP236, IT_CPART_APP236.
  CLEAR:   ST_ISS_APP236.
ENDFORM.                    " itab_and_variable_init

*&---------------------------------------------------------------------*
*&      Form  export_to_excel
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM EXPORT_TO_EXCEL.

ENDFORM.                    " export_to_excel
*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX_APP246.
* Plant
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'WA_PLANT' .
  PERFORM SET_FIELD_PLANT   USING NAME  WA_PLANT.

* Model
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'WA_MODEL' .
  PERFORM SET_FIELD_MODEL USING NAME WA_MODEL.

* Status
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_STATUS_APP246'.
  PERFORM SET_FIELD_STATUS_APP246.
  PERFORM CALL_FUNCTION_VRM  USING XLIST.
* Progress
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_PROG_APP246'.
  PERFORM SET_FIELD_PROG.
  PERFORM CALL_FUNCTION_VRM  USING XLIST.
* REQUESTED BY MY.HUR CHANGEDE BY YONGPING
  NAME = 'P_PROG_APP246_H'.                                 "UD1K912914
  PERFORM CALL_FUNCTION_VRM  USING XLIST.                   "UD1K912914
* END OF CHANGE ON 11/09/2004

ENDFORM.                    " make_dropdown_list_box_APP246

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_WONO_APP246.
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_WORK_ORDER' .
    APPEND XVALUE TO XLIST.
  ENDSELECT.
ENDFORM.                    " SET_FIELD_WONO_APP246

*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_SUM_DATA_APP246.
  DATA: L_ERROR ,
        L_TEXT(50) .
  CLEAR L_ERROR.
  PERFORM SET_PARAMETER_FOR_SRCHNG_DATA USING L_ERROR L_TEXT.
  IF L_ERROR <> SPACE.
    CONCATENATE 'Enter The Necessary Parameters!!! -' L_TEXT
      INTO L_TEXT.
    MESSAGE I000 WITH L_TEXT.
    EXIT.
  ENDIF.
  CLEAR: IT_OBJEK, IT_OBJEK[], IT_SUM_APP246, IT_SUM_APP246[].
  PERFORM GET_VEHICLE_MASTER_NO_APP246 TABLES IT_OBJEK.
  PERFORM CREATE_DATA_FOR_SUM_APP246 .
ENDFORM.                    " search_data
*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER_FOR_SRCHNG_DATA USING P_ERROR P_TEXT .
* Progress
  IF P_PROG_APP246 <> SPACE.
  ELSE.
    P_ERROR = 'X'.
    P_TEXT = 'Progress'.
    EXIT.
  ENDIF.
  IF P_PROG_APP246 GT P_PROG_APP246_H AND
     P_PROG_APP246_H <> SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'Wrong Progress Data'.
    EXIT.
  ENDIF.

ENDFORM.                    " SET_PARAMETER_FOR_SRCHNG_DATA

*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER_NO_APP246 TABLES P_IT_OBJEK STRUCTURE IT_OBJEK .
  DATA: LT_OBJEK   LIKE TABLE OF IT_OBJEK              WITH HEADER LINE,
        L_SUBRC    TYPE SY-SUBRC ,
        L_ATINN    TYPE AUSP-ATINN,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_ATFLV_ST TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N.
* r_prog FOR P_PROG_APP246,       "P_RP_STATUS
  PERFORM READ_ATINN      USING   'P_RP_STATUS'   L_ATINN  .
  SELECT OBJEK ATWRT INTO CORRESPONDING FIELDS OF TABLE LT_OBJEK
    FROM AUSP
   WHERE ATINN = L_ATINN
     AND KLART = '002'
* REQUESTED BY MY HUR CHANGED BY YONGPING
*     AND atwrt = p_prog_app246 .            "UD1K912914
      AND ATWRT IN P_PROG.                                  "UD1K912914
* END OF CHANGE ON 11/09/2004

  " Appending the Other Point for the Processing...
  CASE P_PROG_APP246 .
    WHEN '07'.
*      IF p_prog_app246_h LT '17'.                           "UD1K912914
*        SELECT objek atwrt APPENDING TABLE lt_objek
*          FROM ausp
*         WHERE atinn = l_atinn
*           AND klart = '002'
*           AND atwrt IN
*           ('08', '09', '10', '11', '12', '13', '14', '15', '16').
*      ENDIF.                                                "UD1K912914
    WHEN '17'.
*      SELECT objek atwrt APPENDING TABLE lt_objek
*        FROM ausp
*       WHERE atinn = l_atinn
*         AND klart = '002'
*         AND atwrt IN ('10', '11', '12', '13', '14', '15', '16').
    WHEN '24'.
      SELECT OBJEK ATWRT APPENDING TABLE LT_OBJEK
        FROM AUSP
       WHERE ATINN = L_ATINN
         AND KLART = '002'
         AND ATWRT EQ '26'.
    WHEN '25'.
      SELECT OBJEK ATWRT APPENDING TABLE LT_OBJEK
        FROM AUSP
       WHERE ATINN = L_ATINN
         AND KLART = '002'
         AND ATWRT EQ '27'.

  ENDCASE.

  PERFORM READ_ATINN      USING   'P_USAGE_CAR'   L_ATINN  .
  LOOP AT LT_OBJEK.
**    p_model,     "P_MODEL
    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.
    IT_OBJEK-ATWRT = LT_OBJEK-ATWRT.
    IF WA_MODEL   <> SPACE.
      CLEAR L_SUBRC .
      MOVE WA_MODEL  TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
                                     'P_MODEL'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
**    P_BODYNO_APP246,     "P_BODY_SERIAL
    IF P_BODYNO_APP246 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_BODYNO_APP246 TO L_ATWRT.
      PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
                                     'P_BODY_SERIAL'
                                     L_ATWRT
                               CHANGING L_SUBRC.
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
**    P_WONO_APP246,       "P_WORK_ORDER
    IF P_WONO_APP246 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_WONO_APP246 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM USING    IT_OBJEK-OBJEK
                                      'P_WORK_ORDER'
                                      L_ATWRT
                             CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
**    P_EXTC_APP246,       "P_EXT_COLOR
    IF P_EXTC_APP246 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_EXTC_APP246 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
                                     'P_EXT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
**    P_INTC_APP246.       "P_INT_COLOR
    IF P_INTC_APP246 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_INTC_APP246 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
                                     'P_INT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.   CONTINUE.   ENDIF.
    ENDIF.
**    p_column01 ~ 10  "P_219_xxx
    PERFORM CHECK_219_CODE USING    IT_OBJEK-OBJEK
                           CHANGING L_SUBRC .
    IF L_SUBRC <> 0.   CONTINUE.   ENDIF.

    " Eliminate the Scrap / Disposal Car.
    SELECT SINGLE OBJEK INTO LT_OBJEK-OBJEK
      FROM AUSP
     WHERE OBJEK = LT_OBJEK-OBJEK
       AND ATINN = L_ATINN
       AND KLART = '002'
       AND ATWRT IN ('S', 'D').

    IF SY-SUBRC = 0.   CONTINUE.   ENDIF.
    APPEND IT_OBJEK.
  ENDLOOP.

  SORT IT_OBJEK BY OBJEK .
ENDFORM.                    " get_vehicle_master_no
*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DATA_FOR_SUM_APP246.
  DATA: L_RPNO(02) TYPE N          ,
        L_ATNAM    TYPE CABN-ATNAM ,
        L_ATWRT    TYPE AUSP-ATWRT .
  CLEAR: IT_SUM_APP246, IT_SUM_APP246[].
  LOOP AT IT_OBJEK.
    CLEAR IT_SUM_APP246.
*   V/M No.
    MOVE-CORRESPONDING IT_OBJEK TO IT_SUM_APP246.
*   Model
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_SUM_APP246-OBJEK
                                             'P_MODEL'
                                       CHANGING IT_SUM_APP246-MODEL .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_SUM_APP246-OBJEK
                                             'P_BODY_SERIAL'
                                       CHANGING IT_SUM_APP246-BODYNO .
    CONCATENATE IT_SUM_APP246-MODEL IT_SUM_APP246-BODYNO
      INTO IT_SUM_APP246-BODYNO .
*   Work Order(Serial)
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_SUM_APP246-OBJEK
                                                'P_WORK_ORDER'
                                       CHANGING IT_SUM_APP246-WONO.
*   External Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_SUM_APP246-OBJEK
                                                'P_EXT_COLOR'
                                        CHANGING IT_SUM_APP246-EXTC.
*   Internal Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_SUM_APP246-OBJEK
                                                'P_INT_COLOR'
                                       CHANGING IT_SUM_APP246-INTC.
*BEGIN OF CHANGE BY CHRIS ON 09/29/2004
**  Planned Order Number  "ADDING
    L_ATNAM = 'P_PLAN_ORDER'.
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_SUM_APP246-OBJEK
                                             L_ATNAM
                                    CHANGING IT_SUM_APP246-PLNUM.
*END OF CHANGE BY CHRIS ON 09/29/2004

**  Date : B/In
    L_ATNAM = 'P_RP01_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-BIN.
**  Date : P/In
    L_ATNAM = 'P_RP02_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PIN.
**  Date : T/C
    L_ATNAM = 'P_RP03_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-TC.
**  Date : P/OUT
    L_ATNAM = 'P_RP04_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-POUT.
**  Date : PBS/I
    L_ATNAM = 'P_RP05_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PBSI.
**  Date : PBS/OUT
    L_ATNAM = 'P_RP06_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PBSO.
**  Date : T/IN
    L_ATNAM = 'P_RP07_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-TIN.
**  Date : C/F
    L_ATNAM = 'P_RP17_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-CF.
**  Date : S/OFF
    L_ATNAM = 'P_RP18_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-SOFF.
**  Date : C/GATE
    L_ATNAM = 'P_RP19_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-CONTROL.
**  Date : VPC/I
    L_ATNAM = 'P_RP21_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PDII.
**  Date : VPC/O
    L_ATNAM = 'P_RP22_SHOP_DATE'.
    PERFORM READ_SHOP_DATE USING    IT_SUM_APP246-OBJEK
                                    L_ATNAM
                           CHANGING IT_SUM_APP246-PDIO.

**  Date : M/P  --> Not Defined

    APPEND IT_SUM_APP246.
*
  ENDLOOP.
  SORT IT_SUM_APP246 BY BODYNO .
  DESCRIBE TABLE IT_SUM_APP246 LINES P_TOTAL_APP246.
ENDFORM.                    " create_data
*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_APP246_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP246_MI  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASSIFICATION USING    P_VMNO
                                         P_CHAR
                                CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .

ENDFORM.                    " read_normal_classification
*&---------------------------------------------------------------------*
*&      Form  check_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_0827   text
*      -->P_P_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_DATA_OF_VM USING    P_VMNO
                               P_CHAR
                               P_VALUE
                      CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO         AND
          KLART = '002'          AND
          AU~ATWRT = P_VALUE     AND
          CA~ATNAM = P_CHAR      .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " check_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  check_219_code
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_219_CODE USING    P_OBJEK
                    CHANGING P_SUBRC.
  DATA: LC_COLUMN(20) TYPE C,
        LC_VALUE(20)  TYPE C,
        LC_NUM(02)    TYPE N ,
        L_INT         TYPE I ,
        L_ATWRT       TYPE AUSP-ATWRT,
        L_ATNAM       TYPE CABN-ATNAM.
  FIELD-SYMBOLS: <FS_COLUMN> ,
                 <FS_VALUE>  .

  DO 10 TIMES.
    CLEAR P_SUBRC.
*   Defining Column
    LC_NUM = LC_NUM + 1 .
    CONCATENATE 'P_COLUMN' LC_NUM  INTO LC_COLUMN.
    ASSIGN (LC_COLUMN) TO <FS_COLUMN> .
*   Defining 219 Code Name         "ATNAM
    IF NOT <FS_COLUMN> IS INITIAL.
      L_ATNAM = L_INT =  <FS_COLUMN> .   CONDENSE L_ATNAM.
      CONCATENATE 'P_219_' L_ATNAM   INTO L_ATNAM .
    ENDIF.
*   Defining 219 Code's Value      "ATWRT
    CONCATENATE 'P_VALUE' LC_NUM  INTO LC_VALUE .
    ASSIGN (LC_VALUE) TO <FS_VALUE> .
    MOVE <FS_VALUE> TO L_ATWRT .
*
    IF <FS_COLUMN> IS INITIAL  .
      CONTINUE .
    ENDIF.
*
    PERFORM CHECK_DATA_OF_VM USING IT_OBJEK-OBJEK
                                   L_ATNAM
                                   L_ATWRT
                             CHANGING P_SUBRC.
    IF P_SUBRC <> 0.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " check_219_code

*&---------------------------------------------------------------------*
*&      Form  check_from_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_FROM_DATA_OF_VM USING    P_OBJEK
                                    P_ATNAM
                                    P_ATWRT
                           CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_OBJEK         AND
          KLART = '002'           AND
          AU~ATWRT =  P_ATWRT     AND
          CA~ATNAM >= P_ATNAM      .
  P_SUBRC = SY-SUBRC.

ENDFORM.                    " check_from_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  check_to_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_TO_DATA_OF_VM USING    P_OBJEK
                                  P_ATNAM
                                  P_ATWRT
                         CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_OBJEK         AND
          KLART = '002'           AND
          AU~ATWRT =  P_ATWRT     AND
          CA~ATNAM <= P_ATNAM      .
  P_SUBRC = SY-SUBRC.

ENDFORM.                    " check_to_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  set_field_status_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_STATUS_APP246.
  XVALUE-KEY = 'S'.
  XVALUE-TEXT = 'All Point'.
  APPEND XVALUE TO XLIST.

  XVALUE-KEY = 'D'.
  XVALUE-TEXT = 'Detail'.
  APPEND XVALUE TO XLIST.

ENDFORM.                    " set_field_status_app246
*&---------------------------------------------------------------------*
*&      Form  build_variant_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_VARIANT_APP246.
  GS_VARIANT-REPORT = SY-REPID.
ENDFORM.                    " build_variant_APP246
*&---------------------------------------------------------------------*
*&      Form  build_layout_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_LAYOUT_APP246.
  GS_LAYOUT-ZEBRA  = 'X'.       "ZEBRA
  GS_LAYOUT-CWIDTH_OPT = 'X'.   "OPTIMIZE COLUMN WIDTH
  GS_LAYOUT-DETAILINIT = 'X'.   "DISPLAY INITIAL VALUES ON DETAIL SCREEN
ENDFORM.                    " build_layout_APP246
*&---------------------------------------------------------------------*
*&      Form  build_fieldcat_APP246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_FIELDCAT_APP246.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.


  DATA: ZERO_FNAME1(20),
        ZERO_FNAME2(20),
        ZERO_CNT TYPE I.
  IF P_STATUS_APP246 = 'D'.
    L_STRUCT = 'ZSPP_DET_APP246'.
  ELSE.
    L_STRUCT = 'ZSPP_SUM_APP246'.
  ENDIF.
  CLEAR : WA_FIELDCAT, GT_FIELDCAT.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            I_BUFFER_ACTIVE        = 'X'
            I_STRUCTURE_NAME       = L_STRUCT
       CHANGING
            CT_FIELDCAT            = GT_FIELDCAT[]
       EXCEPTIONS
            INCONSISTENT_INTERFACE = 1
            PROGRAM_ERROR          = 2
            OTHERS                 = 3.

  LOOP AT GT_FIELDCAT INTO WA_FIELDCAT.
    IF P_STATUS_APP246 = 'D'.
      PERFORM SET_FIELD_INFO_DET_APP246 USING WA_FIELDCAT.
    ELSE.
      PERFORM SET_FIELD_INFO_SUM_APP246 USING WA_FIELDCAT.
    ENDIF.
    MODIFY GT_FIELDCAT FROM WA_FIELDCAT.
    CLEAR WA_FIELDCAT.
  ENDLOOP.
ENDFORM.                    " build_fieldcat_APP246
*&---------------------------------------------------------------------*
*&      Form  set_field_info_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT  text
*----------------------------------------------------------------------*
FORM SET_FIELD_INFO_SUM_APP246 USING    L_FIELDCAT STRUCTURE LVC_S_FCAT.
  CASE L_FIELDCAT-FIELDNAME.
    WHEN 'BODYNO'.
      SET_FIELDCAT  'BODY NO' 10.
      L_FIELDCAT-KEY = 'X'.
    WHEN 'WONO'.
      SET_FIELDCAT 'Order No.' 20.
    WHEN 'EXTC'.
      SET_FIELDCAT 'Ext.C' 20.
*      l_fieldcat-key = 'X'.
    WHEN 'INTC'.
      SET_FIELDCAT 'Int.C' 20.
    WHEN 'BIN'.
      SET_FIELDCAT 'B/In' 20.
    WHEN 'PIN'.
      SET_FIELDCAT 'P/In'  20.
    WHEN 'TC'.
      SET_FIELDCAT 'T/C'  20.
    WHEN 'POUT'.
      SET_FIELDCAT 'P/Out' 20.
    WHEN 'PBSI'.
      SET_FIELDCAT 'PBS/I' 20.
    WHEN 'TIN'.
      SET_FIELDCAT 'T/In' 20.
    WHEN 'CF'.
      SET_FIELDCAT 'C/F'  20.
    WHEN 'SOFF'.
      SET_FIELDCAT 'S/Off' 20.
    WHEN 'CONTROL'.
      SET_FIELDCAT 'C/Gate' 20.
    WHEN 'PDII'.
      SET_FIELDCAT 'PDI/I' 20.
    WHEN 'PDIO'.
      SET_FIELDCAT 'PDI/O' 20.
    WHEN 'MP'.
      SET_FIELDCAT 'M/P' 20.
  ENDCASE.
ENDFORM.                    " set_field_info_app246
*&---------------------------------------------------------------------*
*&      Form  set_data_for_sum_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATA_FOR_SUM_APP246.
  PERFORM SEARCH_SUM_DATA_APP246.
ENDFORM.                    " set_data_for_sum_app246
*&---------------------------------------------------------------------*
*&      Form  set_data_for_det_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATA_FOR_DET_APP246.
  PERFORM SEARCH_DETAIL_DATA_APP246.
ENDFORM.                    " set_data_for_det_app246
*&---------------------------------------------------------------------*
*&      Form  read_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_SUM_APP246_OBJEK  text
*      -->P_L_ATNAM  text
*      <--P_IT_TEMP_APP246_PIN  text
*----------------------------------------------------------------------*
FORM READ_SHOP_DATE USING    P_OBJEK
                             P_ATNAM
                    CHANGING P_DATE. "p_atflv.
  DATA: L_ATFLV   TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N         .
  SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  P_ATNAM       .

  P_DATE = L_NUM = L_ATFLV .
ENDFORM.                    " read_shop_date
*&---------------------------------------------------------------------*
*&      Form  search_detail_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DETAIL_DATA_APP246.
  DATA: L_ERROR ,
        L_TEXT(50) .
  CLEAR L_ERROR.
  PERFORM SET_PARAMETER_FOR_SRCHNG_DATA USING L_ERROR L_TEXT.
  IF L_ERROR <> SPACE.
    CONCATENATE 'Enter The Necessary Parameters!!! -' L_TEXT
      INTO L_TEXT.
    MESSAGE I000 WITH L_TEXT.
    EXIT.
  ENDIF.
  CLEAR: IT_OBJEK, IT_OBJEK[], IT_DET_APP246, IT_DET_APP246[].
  PERFORM GET_VEHICLE_MASTER_NO_APP246 TABLES IT_OBJEK.
  PERFORM CREATE_DATA_FOR_DET_APP246 .

ENDFORM.                    " search_detail_data
*&---------------------------------------------------------------------*
*&      Form  create_data_for_det
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DATA_FOR_DET_APP246.
  DATA: L_ATNAM    TYPE CABN-ATNAM ,
        L_ATWRT    TYPE AUSP-ATWRT .

  CLEAR: IT_DET_APP246, IT_DET_APP246[].
  LOOP AT IT_OBJEK.
    CLEAR IT_DET_APP246.
*   V/M No & RP Point..
    MOVE-CORRESPONDING IT_OBJEK TO IT_DET_APP246.
    IT_DET_APP246-RP = IT_OBJEK-ATWRT           .
*   Model
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_DET_APP246-OBJEK
                                             'P_MODEL'
                                       CHANGING IT_DET_APP246-MODEL .
*   bodyno TYPE ausp-atwrt, "P_MODEL & P_BODY_SERIAL(09)
    PERFORM READ_NORMAL_CLASSIFICATION USING IT_DET_APP246-OBJEK
                                             'P_BODY_SERIAL'
                                       CHANGING IT_DET_APP246-BODYNO .
    CONCATENATE IT_DET_APP246-MODEL IT_DET_APP246-BODYNO
      INTO IT_DET_APP246-BODYNO .
*   Work Order(Serial)
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_WORK_ORDER'
                                       CHANGING IT_DET_APP246-WONO.
*   External Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_EXT_COLOR'
                                        CHANGING IT_DET_APP246-EXTC.
*   Internal Color
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_INT_COLOR'
                                       CHANGING IT_DET_APP246-INTC.
*   VIN
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_VIN'
                                       CHANGING IT_DET_APP246-VIN.
*   Vendor : Not Defined
*   MI
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_MI'
                                       CHANGING IT_DET_APP246-MI.
*   OCN
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_OCN'
                                       CHANGING IT_DET_APP246-OCN.
*  BEGIN OF CHANGE BY YONGPING ON 09/29/2004
*   Planned Oder Number  "ADDING
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                'P_PLAN_ORDER'
                                       CHANGING IT_DET_APP246-PLNUM.
*  END OF CHANGE BY YONGPING ON 09/29/2004
*   Reporting Date / Time / Serial.
    CONCATENATE 'P_RP' IT_DET_APP246-RP '_ACTUAL_DATE'  INTO L_ATNAM .
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                L_ATNAM
                                       CHANGING L_ATWRT .
    IT_DET_APP246-REP_DATE = L_ATWRT+00(08).
    IT_DET_APP246-REP_TIME = L_ATWRT+08(06).

    CONCATENATE 'P_RP' IT_DET_APP246-RP '_SERIAL'  INTO L_ATNAM.
    PERFORM READ_NORMAL_CLASSIFICATION USING    IT_DET_APP246-OBJEK
                                                L_ATNAM
                                       CHANGING IT_DET_APP246-SERIAL .
*
    APPEND IT_DET_APP246.
  ENDLOOP.

  SORT IT_DET_APP246 BY BODYNO .
  DESCRIBE TABLE IT_DET_APP246 LINES P_TOTAL_APP246.
ENDFORM.                    " create_data_for_det
*&---------------------------------------------------------------------*
*&      Form  set_field_info_det_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_FIELDCAT  text
*----------------------------------------------------------------------*
FORM SET_FIELD_INFO_DET_APP246 USING    L_FIELDCAT STRUCTURE LVC_S_FCAT.
  CASE L_FIELDCAT-FIELDNAME.
    WHEN 'BODYNO'.
      SET_FIELDCAT  'BODY NO' 10.
      L_FIELDCAT-KEY = 'X'.
    WHEN 'VIN'.
      SET_FIELDCAT 'VIN' 20.
    WHEN 'WONO'.
      SET_FIELDCAT 'Order No' 20.
*      l_fieldcat-key = 'X'.
    WHEN 'EXTC'.
      SET_FIELDCAT 'Ext.C' 20.
    WHEN 'INTC'.
      SET_FIELDCAT 'Int.C' 20.
    WHEN 'VENDOR'.
      SET_FIELDCAT 'Vendor'  20.
    WHEN 'OCN'.
      SET_FIELDCAT 'OCN'  20.
    WHEN 'REP_DATE'.
      SET_FIELDCAT 'Reporting Date' 20.
    WHEN 'REP_TIME'.
      SET_FIELDCAT 'Reporting Time' 20.
    WHEN 'SERIAL'.
      SET_FIELDCAT 'Serial' 20.
  ENDCASE.
ENDFORM.                    " set_field_info_det_app246
*&---------------------------------------------------------------------*
*&      Form  call_methord_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_METHOD_DET_APP246.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.

  L_STRUCT = 'ZSPP_DET_APP246'.
*-----> SET OBJECT
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = L_STRUCT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_DET_APP246[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4 .
ENDFORM.                    " call_methord_app246

*&---------------------------------------------------------------------*
*&      Form  call_method_sum_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_METHOD_SUM_APP246.
  DATA: L_STRUCT    LIKE DD02L-TABNAME.

  L_STRUCT = 'ZSPP_SUM_APP246'.
*-----> SET OBJECT
  CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*        I_BYPASSING_BUFFER            =
*        I_BUFFER_ACTIVE               =
*        I_CONSISTENCY_CHECK           =
      I_STRUCTURE_NAME              = L_STRUCT
      IS_VARIANT                    = GS_VARIANT
      I_SAVE                        = 'A'
*        I_DEFAULT                     = 'X'
      IS_LAYOUT                     = GS_LAYOUT
*        IS_PRINT                      =
*        IT_SPECIAL_GROUPS             =
*        IT_TOOLBAR_EXCLUDING          =
*        IT_HYPERLINK                  =
*        IT_ALV_GRAPHICS               =
    CHANGING
      IT_OUTTAB                     = IT_SUM_APP246[]
      IT_FIELDCATALOG               = GT_FIELDCAT[]
*        IT_SORT                       =
*        IT_FILTER                     =
    EXCEPTIONS
      INVALID_PARAMETER_COMBINATION = 1
      PROGRAM_ERROR                 = 2
      TOO_MANY_LINES                = 3
      OTHERS                        = 4.
ENDFORM.                    " call_method_sum_app246

*&---------------------------------------------------------------------*
*&      Form  check_and_read_data_app246
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_AND_READ_DATA_APP246.
  WA_ALV_CALLED = 'X'.
  CASE P_STATUS_APP246.
    WHEN 'S'.  "Summary
      PERFORM SET_DATA_FOR_SUM_APP246.
    WHEN 'D'.  "Detail
      PERFORM SET_DATA_FOR_DET_APP246.
  ENDCASE.

ENDFORM.                    " check_and_read_data_app246

*&---------------------------------------------------------------------*
*&      Form  make_dropdown_list_box_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_DROPDOWN_LIST_BOX_APP245.
* Plant
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'WA_PLANT' .
  PERFORM SET_FIELD_PLANT  USING NAME   WA_PLANT.

** Changed on 05/22/06 by Fuong, Requested by Mr Hur
* Line
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_LINE_APP245'.
*  PERFORM set_field_line.
*  PERFORM call_function_vrm   USING xlist.
** end of change

* Progress
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_PROG_APP245'.
  PERFORM SET_FIELD_PROG.
  PERFORM CALL_FUNCTION_VRM   USING XLIST.

* Color O or X
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_COLOR_APP245'.
  PERFORM SET_FIELD_COLOR_APP245.
  PERFORM CALL_FUNCTION_VRM   USING XLIST.

* Ending Date
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_END_DATE_APP245'.
  PERFORM SET_FIELD_ENDATE_APP245.
  PERFORM CALL_FUNCTION_VRM   USING XLIST.

* Summary Type
  CLEAR: XLIST, XLIST[], XVALUE.
  NAME = 'P_TYPE_APP245'.
  PERFORM SET_FIELD_TYPE_APP245.
  PERFORM CALL_FUNCTION_VRM   USING XLIST.

** Work Order
*  name = 'P_WONO_APP245'.
*  PERFORM set_field_wono_app245.

* External Color
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_EXTC_APP245'.
*  PERFORM set_field_extc.
*  PERFORM call_function_vrm   USING xlist.
*
** Internal Color
*  CLEAR: xlist, xlist[], xvalue.
*  name = 'P_INTC_APP245'.
*  PERFORM set_field_intc.
*  PERFORM call_function_vrm   USING xlist.
* Columnes
  PERFORM SET_COLUMNS   .
ENDFORM.                    " make_dropdown_list_box_APP245

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_WONO_APP245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_WONO_APP245.
  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = 'P_WORK_ORDER' .
    APPEND XVALUE TO XLIST.
  ENDSELECT.
ENDFORM.                    " SET_FIELD_WONO_APP245

*&---------------------------------------------------------------------*
*&      Form  search_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DATA_APP245.
  DATA: L_ERROR ,
        L_TEXT(50) .
  CLEAR L_ERROR.
  PERFORM SET_PARAMETER_FOR_APP245 USING L_ERROR L_TEXT.
  IF L_ERROR <> SPACE.
    CONCATENATE 'Enter The Necessary Parameters!!! -' L_TEXT
      INTO L_TEXT.
    MESSAGE I000 WITH L_TEXT.
    EXIT.
  ENDIF.
  CLEAR: IT_OBJEK, IT_OBJEK[], IT_APP245, IT_APP245[].
  PERFORM GET_VEHICLE_MASTER_NO_APP245 TABLES IT_OBJEK.
  PERFORM CREATE_DATA_APP245 .
  PERFORM MODIFY_DATA_APP245 .
ENDFORM.                    " search_data

*&---------------------------------------------------------------------*
*&      Form  SET_PARAMETER_FOR_SRCHNG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER_FOR_APP245 USING P_ERROR P_TEXT .
* Production Date
  IF P_SHOP_DATE_APP245 <> SPACE.
  ELSE.
    P_ERROR = 'X'.
    P_TEXT = 'Production Date'.
    EXIT.
  ENDIF.
* Ending Date
  IF P_END_DATE_APP245 <> SPACE.
  ELSE.
    P_ERROR = 'X'.
    P_TEXT = 'Ending Date'.
    EXIT.
  ENDIF.

* Progress
  IF P_PROG_APP245 <> SPACE.
  ELSE.
    P_ERROR = 'X'.
    P_TEXT = 'Progress'.
    EXIT.
  ENDIF.

* Summary Type
  IF P_TYPE_APP245 <> SPACE.
  ELSE.
    P_ERROR = 'X'.
    P_TEXT = 'Summary Type'.
    EXIT.
  ENDIF.

* Whether or Not Color
  IF P_COLOR_APP245 <> SPACE.
  ELSE.
    P_ERROR = 'X'.
    P_TEXT = 'Color Yes or No'.
    EXIT.
  ENDIF.

* Column
  IF P_COLUMN01        <> SPACE.
  ELSE.
    IF P_TYPE_APP245 = '2'.
      P_ERROR = 'X'.
      P_TEXT = 'Column'.
      EXIT.
    ENDIF.
  ENDIF.

* date range check
  IF P_SHOP_DATE_APP245+06(02) GT
     P_END_DATE_APP245.
    P_ERROR = 'X'.
    P_TEXT = 'Date range'.
    EXIT.
  ENDIF.
ENDFORM.                    " SET_PARAMETER_FOR_SRCHNG_DATA

*&---------------------------------------------------------------------*
*&      Form  get_vehicle_master_no
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK  text
*----------------------------------------------------------------------*
FORM GET_VEHICLE_MASTER_NO_APP245 TABLES P_IT_OBJEK STRUCTURE IT_OBJEK .
  RANGES: R_DATE   FOR  AUSP-ATFLV .
  DATA: L_SUBRC    TYPE SY-SUBRC ,
        L_ATNAM    TYPE CABN-ATNAM,
        L_ATNAM1   TYPE CABN-ATNAM,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_ATFLV_ST TYPE AUSP-ATFLV,
        L_TEMP(06),
        L_DATUM    TYPE SY-DATUM,
        L_ATFLV_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N.
  DATA: BEGIN OF LT_OBJEK OCCURS 0,
          OBJEK   LIKE AUSP-OBJEK,
        END OF LT_OBJEK.
  CLEAR L_SUBRC .
* Setting Starting Date
  CONCATENATE 'P_RP' P_PROG_APP245 '_SHOP_DATE'  INTO L_ATNAM .
  IF P_PROG_APP245 = '24'.
    CONCATENATE 'P_RP' '26' '_SHOP_DATE'  INTO L_ATNAM1 .
  ELSEIF P_PROG_APP245 = '25'.
    CONCATENATE 'P_RP' '27' '_SHOP_DATE'  INTO L_ATNAM1 .
  ENDIF.

  R_DATE-LOW = L_ATFLV_ST = L_NUM = P_SHOP_DATE_APP245 .

* Setting Ending Date
  CONCATENATE P_SHOP_DATE_APP245+00(06) P_END_DATE_APP245 INTO L_DATUM.

  IF P_SHOP_DATE_APP245 > L_DATUM.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
         EXPORTING
              DATE      = P_SHOP_DATE_APP245
              MONTHS    = '01'
              DAYS      = '00'
              SIGNUM    = '+'
              YEARS     = '00'
         IMPORTING
              CALC_DATE = L_DATUM.
    CONCATENATE L_DATUM+00(06) P_END_DATE_APP245
      INTO L_DATUM.
  ENDIF.
  R_DATE-HIGH = L_ATFLV_EN = L_NUM = L_DATUM .

**P_SHOP_DATE_APP245  " P_RPxx_SHOP_DATE
  SELECT AU~OBJEK
    INTO TABLE LT_OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE KLART = '002'          AND
          AU~ATFLV >= L_ATFLV_ST AND
          AU~ATFLV <= L_ATFLV_EN AND
          CA~ATNAM = L_ATNAM       .
  IF P_PROG_APP245 = '24' OR
     P_PROG_APP245 = '25'.
    SELECT AU~OBJEK
      APPENDING TABLE LT_OBJEK
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE KLART = '002'          AND
            AU~ATFLV >= L_ATFLV_ST AND
            AU~ATFLV <= L_ATFLV_EN AND
            CA~ATNAM = L_ATNAM1       .
  ENDIF.

  LOOP AT LT_OBJEK.
    IT_OBJEK-OBJEK = LT_OBJEK-OBJEK.
*     P_SCRAP_DATE,     "P_MODEL
    R_DATE-SIGN = 'I'. R_DATE-OPTION = 'BT' .
** changed by Furong Wang on 03/02/2007, request no: UD1K930936
*    PERFORM check_data_num   USING it_objek-objek   'P_SCRAP_DATE'
*                                   r_date
*                             CHANGING l_subrc .
    PERFORM CHECK_SCRAP_CAR  USING IT_OBJEK-OBJEK   'P_USAGE_CAR'
                                CHANGING L_SUBRC .
** end of change

    IF L_SUBRC = 0.  CONTINUE.  ENDIF.
**    P_WONO_APP245,       "P_WORK_ORDER
    IF P_WONO_APP245 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_WONO_APP245 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP245 USING    IT_OBJEK-OBJEK
                                              'P_WORK_ORDER'
                                              L_ATWRT
                                      CHANGING L_SUBRC .
      IF L_SUBRC <> 0.  CONTINUE.  ENDIF.
    ENDIF.
**    P_EXTC_APP245,       "P_EXT_COLOR
    IF P_EXTC_APP245 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_EXTC_APP245 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP245 USING IT_OBJEK-OBJEK
                                     'P_EXT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.  CONTINUE.  ENDIF.
    ENDIF.
**    P_INTC_APP245.       "P_INT_COLOR
    IF P_INTC_APP245 <> SPACE.
      CLEAR L_SUBRC .
      MOVE P_INTC_APP245 TO L_ATWRT .
      PERFORM CHECK_DATA_OF_VM_APP245 USING IT_OBJEK-OBJEK
                                     'P_INT_COLOR'
                                     L_ATWRT
                               CHANGING L_SUBRC .
      IF L_SUBRC <> 0.  CONTINUE.  ENDIF.
    ENDIF.
**    P_COLUMN01_APP245 ~ 10  "P_219_xxx
    PERFORM CHECK_219_CODE        USING    IT_OBJEK-OBJEK
                           CHANGING L_SUBRC .
    IF L_SUBRC <> 0.  CONTINUE.  ENDIF.
    APPEND IT_OBJEK.

  ENDLOOP .
  SORT IT_OBJEK BY OBJEK .
ENDFORM.                    " get_vehicle_master_no

*&---------------------------------------------------------------------*
*&      Form  create_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_DATA_APP245.
  DATA: L_RPNO(02) TYPE N          ,
        L_OBJEK    LIKE MARA-MATNR ,
        L_ATNAM    TYPE CABN-ATNAM ,
        L_ATNAM1    TYPE CABN-ATNAM .
  DATA: L_OLD_DEALER(2),
        L_NEW_DEALER(1),
        L_LEN TYPE I.

  CLEAR: IT_TEMP_APP245, IT_TEMP_APP245[].
  LOOP AT IT_OBJEK.
    CLEAR IT_APP245.
*   V/M No.
    MOVE-CORRESPONDING IT_OBJEK TO IT_TEMP_APP245.
**  Summary Type : Order No.
    CASE P_TYPE_APP245 .
      WHEN  '1'. "<-- Order No
*     Work Order(Serial)
        PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                    'P_WORK_ORDER'
                                         CHANGING IT_TEMP_APP245-SUMINF.
        IF P_COLOR_APP245 = 'O'.
*      External Color
         PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                          'P_EXT_COLOR'
                                           CHANGING IT_TEMP_APP245-EXTC.
*      Internal Color
         PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                          'P_INT_COLOR'
                                           CHANGING IT_TEMP_APP245-INTC.
        ENDIF.
**    Date : P_RPxx_SHOP_DATE.
        L_RPNO = P_PROG_APP245 .
        CONCATENATE 'P_RP' L_RPNO '_SHOP_DATE'  INTO L_ATNAM .
        IF L_RPNO = '24'.
          CONCATENATE 'P_RP' '26' '_SHOP_DATE'  INTO L_ATNAM1 .
        ELSEIF L_RPNO = '25'.
          CONCATENATE 'P_RP' '27' '_SHOP_DATE'  INTO L_ATNAM1 .
        ENDIF.

        PERFORM READ_SHOP_DATE_APP245 USING    IT_TEMP_APP245-OBJEK
                                        L_ATNAM L_ATNAM1
                               CHANGING IT_TEMP_APP245-DATE.
**  Summary Type : 219 Code.
      WHEN  '2'.   "<-- 219 Code : P_TYPE_APP245 = '2'.
*---> CASE : Only The First Column's Code & Value
*      PERFORM read_code_inf_app245 USING       it_temp_app245-objek
*                                   p_column01  p_value01
*                            CHANGING it_temp_app245-suminf.
        IT_TEMP_APP245-SUMINF = '219 Option'.
**    Date : P_RPxx_SHOP_DATE.
        L_RPNO = P_PROG_APP245 .
        CONCATENATE 'P_RP' L_RPNO '_SHOP_DATE'  INTO L_ATNAM .
        IF L_RPNO = '24'.
          CONCATENATE 'P_RP' '26' '_SHOP_DATE'  INTO L_ATNAM1 .
        ELSEIF L_RPNO = '25'.
          CONCATENATE 'P_RP' '27' '_SHOP_DATE'  INTO L_ATNAM1 .
        ENDIF.

        PERFORM READ_SHOP_DATE_APP245 USING    IT_TEMP_APP245-OBJEK
                                        L_ATNAM L_ATNAM1
                               CHANGING IT_TEMP_APP245-DATE.
      WHEN '3'.   "<-- FSC Code : P_TYPE_APP245 = '3'.
*     Work Order(Serial)
        PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                    'P_MODEL_YEAR'
                                         CHANGING L_OBJEK .
        IT_TEMP_APP245-SUMINF = L_OBJEK .
        PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                    'P_WORK_ORDER'
                                         CHANGING L_OBJEK .
        CONCATENATE IT_TEMP_APP245-SUMINF L_OBJEK+9(5)
               INTO IT_TEMP_APP245-SUMINF.
        PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                    'P_MI'
                                         CHANGING L_OBJEK .
** Changed by Furong on 12/20/07 for EBOM
        L_LEN = STRLEN( L_OBJEK ).
        IF L_LEN > 7.
          L_OLD_DEALER = IT_TEMP_APP245-SUMINF+4(2).
          CALL FUNCTION 'ZFEB_GET_NEW_DEALER_CODE'
               EXPORTING
                    OLD_DEALER = L_OLD_DEALER
               IMPORTING
                    NEW_DEALER = L_NEW_DEALER.
          CONCATENATE IT_TEMP_APP245-SUMINF+0(4) L_NEW_DEALER
          INTO IT_TEMP_APP245-SUMINF.

          CONCATENATE IT_TEMP_APP245-SUMINF L_OBJEK
          INTO IT_TEMP_APP245-SUMINF.
         PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                      'P_OCN'
                                           CHANGING L_OBJEK .
          CONCATENATE IT_TEMP_APP245-SUMINF L_OBJEK
                 INTO IT_TEMP_APP245-SUMINF.
          CLEAR: L_OLD_DEALER, L_NEW_DEALER, L_LEN.
        ELSE.
          CONCATENATE IT_TEMP_APP245-SUMINF L_OBJEK
                 INTO IT_TEMP_APP245-SUMINF.
         PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                      'P_OCN'
                                           CHANGING L_OBJEK .
          CONCATENATE IT_TEMP_APP245-SUMINF L_OBJEK
                 INTO IT_TEMP_APP245-SUMINF SEPARATED BY SPACE.
        ENDIF.
** End of change
        L_RPNO = P_PROG_APP245 .
*--->>requested by my hur changed by chris
        IF P_COLOR_APP245 = 'O'.
*      External Color
         PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                          'P_EXT_COLOR'
                                           CHANGING IT_TEMP_APP245-EXTC.
*      Internal Color
         PERFORM READ_NORMAL_CLASS_APP245 USING    IT_TEMP_APP245-OBJEK
                                                          'P_INT_COLOR'
                                           CHANGING IT_TEMP_APP245-INTC.
        ENDIF.
*--->>end of change on 04/12/2005
        CONCATENATE 'P_RP' L_RPNO '_SHOP_DATE'  INTO L_ATNAM .
        IF L_RPNO = '24'.
          CONCATENATE 'P_RP' '26' '_SHOP_DATE'  INTO L_ATNAM1 .
        ELSEIF L_RPNO = '25'.
          CONCATENATE 'P_RP' '27' '_SHOP_DATE'  INTO L_ATNAM1 .
        ENDIF.
        PERFORM READ_SHOP_DATE_APP245 USING    IT_TEMP_APP245-OBJEK
                                        L_ATNAM  L_ATNAM1
                               CHANGING IT_TEMP_APP245-DATE.
    ENDCASE.
    APPEND IT_TEMP_APP245.
  ENDLOOP.
  SORT IT_TEMP_APP245 BY SUMINF EXTC INTC DATE .
ENDFORM.                    " create_data

*&---------------------------------------------------------------------*
*&      Form  read_normal_classification
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_APP245_OBJEK  text
*      -->P_1077   text
*      <--P_IT_APP245_MI  text
*----------------------------------------------------------------------*
FORM READ_NORMAL_CLASS_APP245 USING    P_VMNO  P_CHAR
                              CHANGING P_VALUE.
  SELECT SINGLE AU~ATWRT
    INTO P_VALUE
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO      AND
          KLART = '002'       AND
          CA~ATNAM = P_CHAR  .
ENDFORM.                    " read_normal_classification

*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM CHECK_DATA_NUM   USING   P_VMNO  P_CHAR  P_VALUE  CHANGING P_SUBRC.
  RANGES: R_ATFLV     FOR AUSP-ATFLV.

  CLEAR: R_ATFLV, R_ATFLV[].
  R_ATFLV = P_VALUE.     APPEND R_ATFLV.

  SELECT SINGLE OBJEK
           INTO IT_OBJEK-OBJEK
           FROM AUSP AS AU
     INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
          WHERE OBJEK    = P_VMNO      AND
                KLART    = '002'       AND
                AU~ATFLV IN R_ATFLV    AND
                CA~ATNAM =  P_CHAR     .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " CHECK_DATA_NUM

*&---------------------------------------------------------------------*
*&      Form  check_data_of_vm
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_0827   text
*      -->P_P_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_DATA_OF_VM_APP245 USING    P_VMNO
                                      P_CHAR
                                      P_VALUE
                             CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_VMNO         AND
          KLART = '002'          AND
          AU~ATWRT = P_VALUE     AND
          CA~ATNAM = P_CHAR      .
  P_SUBRC = SY-SUBRC.
ENDFORM.                    " check_data_of_vm
*&---------------------------------------------------------------------*
*&      Form  download_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DOWNLOAD_DATA_APP245.
  CLEAR: IT_EXCEL_APP245, IT_EXCEL_APP245[].
  PERFORM SET_HEADER_APP245         TABLES IT_EXCEL_APP245.
  PERFORM SET_BODY_APP245           TABLES IT_EXCEL_APP245.
  PERFORM CALL_FUNC_DOWNLOAD_APP245 TABLES IT_EXCEL_APP245.

ENDFORM.                    " download_data
*&---------------------------------------------------------------------*
*&      Form  set_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP245  text
*----------------------------------------------------------------------*
FORM SET_HEADER_APP245 TABLES   P_IT_EXCEL STRUCTURE IT_EXCEL_APP245.
  WRITE: 'Summary Object' TO P_IT_EXCEL-SUMINF   ,    "Summary Type
         'External Color' TO P_IT_EXCEL-EXTC    ,      "P_EXT_COLOR(03)
         'Internal Color' TO P_IT_EXCEL-INTC    ,      "P_INT_COLOR(03)
*
         'Total'   TO P_IT_EXCEL-TOTAL(20)    ,          "Total Quantity
*
         '1st QTY' TO P_IT_EXCEL-01QTY(20)    ,
         '2nd QTY' TO P_IT_EXCEL-02QTY(20)    ,
         '3rd QTY' TO P_IT_EXCEL-03QTY(20)    ,
         '4th QTY' TO P_IT_EXCEL-04QTY(20)    ,
         '5th QTY' TO P_IT_EXCEL-05QTY(20)    ,
         '6th QTY' TO P_IT_EXCEL-06QTY(20)    ,
         '7th QTY' TO P_IT_EXCEL-07QTY(20)    ,
         '8th QTY' TO P_IT_EXCEL-08QTY(20)    ,
         '9th QTY' TO P_IT_EXCEL-09QTY(20)    ,
         '10th QTY' TO P_IT_EXCEL-10QTY(20)    ,
         '11st QTY' TO P_IT_EXCEL-11QTY(20)    ,
         '12nd QTY' TO P_IT_EXCEL-12QTY(20)    ,
         '13rd QTY' TO P_IT_EXCEL-13QTY(20)    ,
         '14th QTY' TO P_IT_EXCEL-14QTY(20)    ,
         '15th QTY' TO P_IT_EXCEL-15QTY(20)    ,
         '16th QTY' TO P_IT_EXCEL-16QTY(20)    ,
         '17th QTY' TO P_IT_EXCEL-17QTY(20)    ,
         '18th QTY' TO P_IT_EXCEL-18QTY(20)    ,
         '19th QTY' TO P_IT_EXCEL-19QTY(20)    ,
         '20th QTY' TO P_IT_EXCEL-20QTY(20)    ,
         '21st QTY' TO P_IT_EXCEL-21QTY(20)    ,
         '22nd QTY' TO P_IT_EXCEL-22QTY(20)    ,
         '23rd QTY' TO P_IT_EXCEL-23QTY(20)    ,
         '24th QTY' TO P_IT_EXCEL-24QTY(20)    ,
         '25th QTY' TO P_IT_EXCEL-25QTY(20)    ,
         '26th QTY' TO P_IT_EXCEL-26QTY(20)    ,
         '27th QTY' TO P_IT_EXCEL-27QTY(20)    ,
         '28th QTY' TO P_IT_EXCEL-28QTY(20)    ,
         '29th QTY' TO P_IT_EXCEL-29QTY(20)    ,
         '30th QTY' TO P_IT_EXCEL-30QTY(20)    ,
         '31th QTY' TO P_IT_EXCEL-31QTY    .
*
  APPEND P_IT_EXCEL.

ENDFORM.                    " set_header
*&---------------------------------------------------------------------*
*&      Form  set_body
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP245  text
*----------------------------------------------------------------------*
FORM SET_BODY_APP245 TABLES   P_IT STRUCTURE IT_EXCEL_APP245 .
  LOOP AT IT_APP245.
    CLEAR P_IT.
    MOVE-CORRESPONDING IT_APP245 TO P_IT.
    APPEND P_IT.
  ENDLOOP.
ENDFORM.                    " set_body
*&---------------------------------------------------------------------*
*&      Form  call_func_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_EXCEL_APP245  text
*----------------------------------------------------------------------*
FORM CALL_FUNC_DOWNLOAD_APP245 TABLES   P_IT STRUCTURE IT_EXCEL_APP245.
  CALL FUNCTION 'DOWNLOAD'
       EXPORTING
            FILENAME                =
               'Prod Results Per Each Progress.XLS'
            FILETYPE                = 'DAT'
            ITEM                    = ' '
            FILETYPE_NO_CHANGE      = 'X'
            FILETYPE_NO_SHOW        = 'X'
       TABLES
            DATA_TAB                = P_IT
       EXCEPTIONS
            INVALID_FILESIZE        = 1
            INVALID_TABLE_WIDTH     = 2
            INVALID_TYPE            = 3
            NO_BATCH                = 4
            UNKNOWN_ERROR           = 5
            GUI_REFUSE_FILETRANSFER = 6
            OTHERS                  = 7.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " call_func_download
*&---------------------------------------------------------------------*
*&      Form  sort_screen_2116
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SORT_SCREEN_2116   USING PA_STYPE.
  DATA: LW_SCREEN          TYPE TABLE OF CXTAB_COLUMN  WITH HEADER LINE,
        FIELD_NAME01(40),
        OFFSET01 TYPE I.
*
  CLEAR:  FIELD_NAME01.
  LOOP AT TC_APP245-COLS INTO LW_SCREEN.
    IF LW_SCREEN-SELECTED = 'X' .
      FIELD_NAME01 = LW_SCREEN-SCREEN-NAME .
      FIELD_NAME01 = FIELD_NAME01+10       .
      EXIT.
    ENDIF.
  ENDLOOP.

  CASE PA_STYPE.
    WHEN 'A'.
      SORT IT_APP245  ASCENDING BY (FIELD_NAME01).
    WHEN 'D'.
      SORT IT_APP245  DESCENDING BY (FIELD_NAME01).
  ENDCASE.
ENDFORM.                    " sort_screen_2116
*&---------------------------------------------------------------------*
*&      Form  check_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_SHOP_DATE_APP245 USING    P_OBJEK
                                     P_ATNAM
                                     P_ATFLV_ST
                                     P_ATFLV_EN
                            CHANGING P_SUBRC.
  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_OBJEK AND
          KLART = '002'          AND
          AU~ATFLV >= P_ATFLV_ST AND
          AU~ATFLV <= P_ATFLV_EN AND
          CA~ATNAM = P_ATNAM      .
  P_SUBRC = SY-SUBRC.

ENDFORM.                    " check_shop_date
*&---------------------------------------------------------------------*
*&      Form  check_interval_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_L_ATNAM  text
*      -->P_L_ATWRT  text
*      -->P_0857   text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_INTERVAL_DATA USING    P_OBJEK
                                  P_ATNAM
                                  P_ATWRT
                                  P_MARK
                         CHANGING P_SUBRC.
  DATA: LC_FROM    TYPE C VALUE ' ',
        LC_TO      TYPE C VALUE 'X'.
  RANGES: LR_ATWRT FOR  AUSP-ATWRT .

  CLEAR: LR_ATWRT, LR_ATWRT[].
  IF P_MARK = LC_FROM.
    LR_ATWRT-LOW = P_ATWRT .
    LR_ATWRT-OPTION = 'GE' .
    LR_ATWRT-SIGN   = 'I'  .
    APPEND LR_ATWRT.
  ELSE.                     "p_mark = lc_to .
    LR_ATWRT-LOW = P_ATWRT .
    LR_ATWRT-OPTION = 'LE' .
    LR_ATWRT-SIGN   = 'I'  .
    APPEND LR_ATWRT.
  ENDIF.

  SELECT SINGLE OBJEK
    INTO IT_OBJEK-OBJEK
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE OBJEK = P_OBJEK        AND
          KLART = '002'          AND
          AU~ATWRT IN LR_ATWRT   AND
          CA~ATNAM = P_ATNAM      .
  P_SUBRC = SY-SUBRC.

ENDFORM.                    " check_interval_data

*&---------------------------------------------------------------------*
*&      Form  set_field_color_app245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_COLOR_APP245.
  XVALUE-KEY = 'O'.
  XVALUE-TEXT = 'Yes'.
  APPEND XVALUE TO XLIST.
  XVALUE-KEY = 'X'.
  XVALUE-TEXT = 'No'.
  APPEND XVALUE TO XLIST.

ENDFORM.                    " set_field_color_app245
*&---------------------------------------------------------------------*
*&      Form  set_field_endate_app245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_ENDATE_APP245.
  DATA L_NUM(02) TYPE N.
  DO 31 TIMES.
    L_NUM = L_NUM + 1.
    MOVE L_NUM TO XVALUE-KEY.
    APPEND XVALUE TO XLIST.
  ENDDO.

ENDFORM.                    " set_field_endate_app245
*&---------------------------------------------------------------------*
*&      Form  set_field_type_app245
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_TYPE_APP245.
  XVALUE-KEY = '1'.
  XVALUE-TEXT = 'Order No'.
  APPEND XVALUE TO XLIST.
  XVALUE-KEY = '2'.
  XVALUE-TEXT = '219'.
  APPEND XVALUE TO XLIST.
  XVALUE-KEY = '3'.
  XVALUE-TEXT = 'FSC'.
  APPEND XVALUE TO XLIST.
ENDFORM.                    " set_field_type_app245

*&---------------------------------------------------------------------*
*&      Form  SET_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_DATE_APP245.
  DATA : L_DATE        TYPE SY-DATUM,
         L_NUM(02)     TYPE N,
         L_NUM_TOT(02) TYPE N,
         L_COUNT(02)   TYPE N,
         L_HEADER(20)  TYPE C.
  FIELD-SYMBOLS : <FS_FIELD>.
  IF P_SHOP_DATE_APP245 <> SPACE AND
     P_END_DATE_APP245  <> SPACE   .
*   Setting The First Day.
    MOVE: P_SHOP_DATE_APP245 TO L_DATE,
          L_DATE+06(02) TO L_NUM.
*   Caculating The Total Days to be set.
    L_NUM_TOT = P_END_DATE_APP245 - L_NUM + 1 .
    DO 31 TIMES.
      L_COUNT = L_COUNT + 1.
      CONCATENATE 'P_D' L_COUNT '_APP245' INTO L_HEADER .
      ASSIGN (L_HEADER) TO <FS_FIELD>.
*     Setting The Day.
      <FS_FIELD> = L_NUM .
*     Increasing The Day.
      L_DATE = L_DATE + 1.
      L_NUM = L_DATE+06(02).
    ENDDO.

  ENDIF.
ENDFORM.                    " SET_DATE
*&---------------------------------------------------------------------*
*&      Form  read_code_inf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_COLUMN01_APP245  text
*      -->P_P_VALUE01_APP245  text
*      <--P_IT_TEMP_APP245_SUMINF  text
*----------------------------------------------------------------------*
FORM READ_CODE_INF_APP245 USING    P_OBJEK
                            P_COLUMN
                            P_VALUE
                   CHANGING P_SUMINF.
  DATA:   L_ATNAM TYPE CABN-ATNAM ,
          L_ATWRT TYPE AUSP-ATWRT .
  RANGES: LR_ATWRT FOR AUSP-ATWRT .

* Setting Characteristic Name.
  CONCATENATE 'P_219_' P_COLUMN
    INTO L_ATNAM.
* Setting Characteristic Value.
  IF P_VALUE <> '*'.
    LR_ATWRT-OPTION = 'EQ'.
    LR_ATWRT-SIGN   = 'I' .
    LR_ATWRT-LOW    = P_VALUE .
    APPEND LR_ATWRT .
  ELSE.
    CLEAR: LR_ATWRT, LR_ATWRT[].
  ENDIF.

  SELECT SINGLE CA~ATNAM AU~ATWRT
    INTO (L_ATNAM, L_ATWRT)
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  L_ATNAM     AND
          AU~ATWRT IN LR_ATWRT      .
  CONCATENATE L_ATNAM L_ATWRT
    INTO P_SUMINF SEPARATED BY SPACE .

ENDFORM.                    " read_code_inf
*&---------------------------------------------------------------------*
*&      Form  read_shop_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_TEMP_APP245_OBJEK  text
*      -->P_L_ATNAM  text
*      <--P_IT_TEMP_APP245_DATE  text
*----------------------------------------------------------------------*
FORM READ_SHOP_DATE_APP245 USING    P_OBJEK
                             P_ATNAM P_ATNAM1
                    CHANGING P_DATE. "p_atflv.
  DATA: L_ATFLV   TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N         .

  SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  P_ATNAM       .
  IF SY-SUBRC NE 0.
    SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON CA~ATINN = AU~ATINN
    WHERE AU~OBJEK =  P_OBJEK     AND
          AU~KLART =  '002'       AND
          CA~ATNAM =  P_ATNAM1       .
  ENDIF.
  P_DATE = L_NUM = L_ATFLV .

ENDFORM.                    " read_shop_date
*&---------------------------------------------------------------------*
*&      Form  modify_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MODIFY_DATA_APP245.
  DATA: L_FIELD_NAME(30) .
  FIELD-SYMBOLS : <FS_DQTY> .
  CLEAR: IT_DATE, IT_DATE[].
*
  PERFORM SETTING_DATE_APP245 TABLES IT_DATE.
*
  LOOP AT IT_TEMP_APP245.
    CLEAR IT_APP245 .
    MOVE-CORRESPONDING IT_TEMP_APP245 TO IT_APP245 .
    IT_APP245-TOTAL = 1 .
    READ TABLE IT_DATE WITH KEY DATE = IT_TEMP_APP245-DATE .
    CONCATENATE 'IT_APP245-' IT_DATE-NUM 'QTY'
      INTO L_FIELD_NAME .
    ASSIGN (L_FIELD_NAME) TO <FS_DQTY>.
    <FS_DQTY> = 1 .
    COLLECT IT_APP245 .
  ENDLOOP.
  LOOP AT IT_APP245.
    AT LAST.
      SUM.
      IT_APP245-SUMINF = 'Total'.
      APPEND IT_APP245.
      EXIT.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " modify_data
*&---------------------------------------------------------------------*
*&      Form  setting_date
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DATE  text
*----------------------------------------------------------------------*
FORM SETTING_DATE_APP245 TABLES   P_IT_DATE STRUCTURE  IT_DATE .
  DATA: L_DATE    TYPE SY-DATUM,
        L_NUM(02) TYPE N.

  MOVE P_SHOP_DATE_APP245 TO L_DATE.
  L_NUM = '01'.
  DO 31 TIMES.
    CLEAR P_IT_DATE.
    MOVE: L_DATE TO P_IT_DATE-DATE,
          L_NUM  TO P_IT_DATE-NUM .
    APPEND P_IT_DATE.
    L_DATE = L_DATE + 1.
    L_NUM  = L_NUM  + 1.
    IF L_DATE+06(02) > P_END_DATE_APP245.
      EXIT .
    ENDIF.
  ENDDO.
ENDFORM.                    " setting_date

*&---------------------------------------------------------------------*
*&      Form  data_next_COLOR_0111
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_NEXT_COLOR_0111.
  DATA: L_INDEX  TYPE I         ,
        L_MATNR  LIKE MARA-MATNR,
        L_OBJEK  LIKE AUSP-OBJEK,
        L_OBJEK2 LIKE AUSP-OBJEK.

  IF ST_0111_INPUT-EXCLR1 EQ SPACE .
    SELECT SINGLE EXTC INTC
      INTO (ST_0111_INPUT-EXCLR1, ST_0111_INPUT-INCLR1)
      FROM ZTPP_WOSUM
     WHERE WO_SER = ST_0111_INPUT-ORDER1(9)
       AND NATION = ST_0111_INPUT-ORDER1+9(3)
       AND DEALER = ST_0111_INPUT-ORDER1+12(2).
  ELSE.
    CONCATENATE ST_0111_INPUT-ORDER1  ST_0111_INPUT-EXCLR1
                ST_0111_INPUT-INCLR1  INTO  L_MATNR       .
    SELECT SINGLE MATNR INTO L_MATNR
      FROM MARA
     WHERE MTART = 'WOCL'
       AND MATNR > L_MATNR .
    IF SY-SUBRC = 0 .
      ST_0111_INPUT-ORDER1 = L_MATNR(14).
      ST_0111_INPUT-EXCLR1 = L_MATNR+14(2).
      ST_0111_INPUT-INCLR1 = L_MATNR+16(2).
    ELSE.
      SELECT SINGLE MATNR INTO L_MATNR
        FROM MARA
       WHERE MTART = 'WOCL'
         AND MATNR > SPACE .
      ST_0111_INPUT-ORDER1 = L_MATNR(14).
      ST_0111_INPUT-EXCLR1 = L_MATNR+14(2).
      ST_0111_INPUT-INCLR1 = L_MATNR+16(2).
    ENDIF.
  ENDIF .

  PERFORM DATA_SELECT_0111 .
ENDFORM.                    " data_next_COLOR_0111

*&---------------------------------------------------------------------*
*&      Form  set_parameter_3109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_PARAMETER_3109.
* Plant
  CLEAR : XLIST[], XVALUE.
  NAME = 'WA_PLANT'.
  PERFORM SET_FIELD_PLANT   USING NAME  WA_PLANT   .
* Model
  CLEAR : XLIST[], XVALUE.
  NAME = 'WA_MODEL' .
  PERFORM SET_FIELD_MODEL USING NAME   WA_MODEL.
ENDFORM.                    " set_parameter_3109

*&---------------------------------------------------------------------*
*&      Form  data_select_3109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DATA_SELECT_3109.
  DATA: LT_ALCU             LIKE TABLE OF CABN         WITH HEADER LINE,
        LT_ALCC             LIKE TABLE OF CABN         WITH HEADER LINE,
        LT_TECH             LIKE TABLE OF CABN         WITH HEADER LINE,
        LT_HPCP             LIKE TABLE OF CABN         WITH HEADER LINE,
        LT_HPCB             LIKE TABLE OF CABN         WITH HEADER LINE,
        LT_HPCQ             LIKE TABLE OF CABN         WITH HEADER LINE,
        LT_3109             LIKE TABLE OF IT_3109      WITH HEADER LINE,
        L_CNT               TYPE I         ,
        L_SIZE              TYPE I         ,
        L_3NO(3)            TYPE N         ,
        L_CHAR(30)          TYPE C         .

  CLEAR: IT_3109, IT_3109[].
  SELECT * INTO TABLE LT_ALCU
    FROM CABN
   WHERE ATNAM LIKE 'P_ALC_U_%' .

  SELECT * INTO TABLE LT_ALCC
    FROM CABN
   WHERE ATNAM LIKE 'P_ALC_C_%' .

  SELECT * INTO TABLE LT_TECH
    FROM CABN
   WHERE ATNAM LIKE 'P_TECH_SPEC_%'.

  SELECT * INTO TABLE LT_HPCP
    FROM CABN
   WHERE ATNAM LIKE 'P_WO_HPC_P%'.

  SELECT * INTO TABLE LT_HPCB
    FROM CABN
   WHERE ATNAM LIKE 'P_WO_HPC_B%'.

  SELECT * INTO TABLE LT_HPCQ
    FROM CABN
   WHERE ATNAM LIKE 'P_WO_HPC_Q%'.

  LOOP AT LT_ALCU .
    L_CNT = L_3NO = LT_ALCU-ATNAM+8(3).
    IT_3109-SERI  = L_CNT             .
    CONCATENATE 'U' L_3NO  INTO IT_3109-CLM1.
    IT_3109-CHAR1 = LT_ALCU-ATINN     .
    IT_3109-MODEL = WA_MODEL          .
    APPEND IT_3109.     CLEAR: IT_3109.
  ENDLOOP.

  SORT IT_3109 BY SERI .

  LOOP AT IT_3109 .
    L_3NO = IT_3109-CLM1+1(3).
    CONCATENATE 'P_WO_HPC_P' L_3NO INTO L_CHAR.
    READ TABLE LT_HPCP WITH KEY ATNAM = L_CHAR.
    L_CNT = SY-TABIX                          .
    IT_3109-CHAR2 = LT_HPCP-ATINN             .
    CONCATENATE 'P' L_3NO  INTO IT_3109-CLM2  .
    MODIFY IT_3109. DELETE LT_HPCP INDEX L_CNT.
  ENDLOOP.

  CLEAR: IT_3109  .
  LOOP AT LT_HPCP .
    L_CNT = L_3NO = LT_HPCP-ATNAM+10(3).
    IT_3109-SERI  = L_CNT             .
    CONCATENATE 'P' L_3NO  INTO IT_3109-CLM2.
    IT_3109-CHAR2 = LT_HPCP-ATINN     .
    IT_3109-MODEL = WA_MODEL          .
    APPEND IT_3109.     CLEAR: IT_3109.
  ENDLOOP.

  CLEAR: IT_3109  .
  DESCRIBE TABLE IT_3109 LINES L_SIZE.
  LOOP AT LT_HPCB .
    L_CNT = L_3NO = LT_HPCB-ATNAM+10(3).
    IT_3109-SERI  = L_CNT + L_SIZE    .
    CONCATENATE 'B' L_3NO  INTO IT_3109-CLM2.
    IT_3109-CHAR2 = LT_HPCB-ATINN     .
    IT_3109-MODEL = WA_MODEL          .
    APPEND IT_3109.     CLEAR: IT_3109.
  ENDLOOP.

  CLEAR: IT_3109  .
  DESCRIBE TABLE IT_3109 LINES L_SIZE.
  LOOP AT LT_TECH .
    L_CNT = L_3NO = LT_TECH-ATNAM+12(3).
    IT_3109-SERI  = L_CNT + L_SIZE    .
    CONCATENATE 'T' L_3NO  INTO IT_3109-CLM1.
    IT_3109-CHAR1 = LT_TECH-ATINN     .
    IT_3109-MODEL = WA_MODEL          .
    APPEND IT_3109.     CLEAR: IT_3109.
  ENDLOOP.

  LOOP AT LT_ALCC .
    L_CNT = L_3NO = LT_ALCC-ATNAM+8(3).
    LT_3109-SERI  = L_CNT             .
    CONCATENATE 'C' L_3NO  INTO LT_3109-CLM1.
    LT_3109-CHAR1 = LT_ALCC-ATINN     .
    LT_3109-MODEL = WA_MODEL          .
    APPEND LT_3109.     CLEAR: LT_3109.
  ENDLOOP.

  SORT LT_3109 BY SERI .
  LOOP AT LT_3109 .
    L_3NO = LT_3109-CLM1+1(3).
    CONCATENATE 'P_WO_HPC_Q' L_3NO INTO L_CHAR.
    READ TABLE LT_HPCQ WITH KEY ATNAM = L_CHAR.
    L_CNT = SY-TABIX                          .
    LT_3109-CHAR2 = LT_HPCQ-ATINN             .
    CONCATENATE 'Q' L_3NO  INTO LT_3109-CLM2  .
    MODIFY LT_3109. DELETE LT_HPCQ INDEX L_CNT.
  ENDLOOP.

  CLEAR: LT_3109  .
  LOOP AT LT_HPCQ .
    L_CNT = L_3NO = LT_HPCQ-ATNAM+10(3).
    LT_3109-SERI  = L_CNT             .
    CONCATENATE 'Q' L_3NO  INTO LT_3109-CLM2.
    LT_3109-CHAR2 = LT_HPCQ-ATINN     .
    LT_3109-MODEL = WA_MODEL          .
    APPEND LT_3109.     CLEAR: LT_3109.
  ENDLOOP.

  DESCRIBE TABLE IT_3109 LINES L_SIZE.
  SORT LT_3109 BY SERI.  CLEAR: L_CNT.
  LOOP AT LT_3109.
    L_CNT = L_CNT + 1  .
    LT_3109-SERI  = L_CNT + L_SIZE   .
    MODIFY LT_3109.
  ENDLOOP.

  APPEND LINES OF LT_3109 TO IT_3109 .
  SORT IT_3109 BY SERI .
ENDFORM.                    " data_select_3109

*&---------------------------------------------------------------------*
*&      Form  GET_ENM1_3109
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_ATINN_ATINN  text
*      -->P_IT_3109_ENM1  text
*----------------------------------------------------------------------*
FORM GET_ENM1_3109 USING    PA_ATINN  PA_ENM1.
  " First Check the Unique or Color's Characteristic...
  SELECT SINGLE ATNAM INTO L_ATNAM
    FROM CABN
   WHERE ATINN = PA_ATINN .
  IF L_ATNAM(6) = 'P_ALC_'.
    PERFORM READ_TABLE_TEXT  USING IT_3109-MODEL L_ATNAM+6(1)
                                   L_ATNAM+8(3)        PA_ENM1     .
  ELSE.
    SELECT SINGLE ATBEZ INTO PA_ENM1
      FROM CABNT
     WHERE ATINN = PA_ATINN .
  ENDIF.
ENDFORM.                    " GET_ENM1_3109
*&---------------------------------------------------------------------*
*&      Form  219_column_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM 219_COLUMN_VALUE.
  DATA: L_MODEL TYPE ZTBM_ABXOPVDT-CARX .
  CLEAR IS219-DESC219.

  IF IS219-NAME219 > '219'.
    MESSAGE S000 WITH 'It is last 219 Options'.
    MOVE   '219'   TO  IS219-NAME219.
  ENDIF.
  IF IS219-NAME219 < '001'.
    MESSAGE S000 WITH 'It is the first 219 options'.
    MOVE   '001'   TO  IS219-NAME219.
  ENDIF.

  MOVE IS219-MODEL+00(02) TO L_MODEL .

  SELECT  SINGLE  VANM INTO  IS219-DESC219
    FROM  ZTBM_ABXOPVDT
   WHERE  CARX      EQ  L_MODEL
     AND  CLNO      EQ  IS219-NAME219.

ENDFORM.                    " 219_column_value
*&---------------------------------------------------------------------*
*&      Form  prev_column_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PREV_COLUMN_SELECT.
  IS219-NAME219 = IS219-NAME219 -  1.
  PERFORM 219_COLUMN_VALUE.
  PERFORM SEARCH_PRE_VALUE.
*  PERFORM  column_value_select.

ENDFORM.                    " prev_column_select
*&---------------------------------------------------------------------*
*&      Form  next_column_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM NEXT_COLUMN_SELECT.
  IS219-NAME219 = IS219-NAME219 +  1.
  PERFORM 219_COLUMN_VALUE.

  PERFORM  COLUMN_VALUE_SELECT.
ENDFORM.                    " next_column_select
*&---------------------------------------------------------------------*
*&      Form  column_value_select
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLUMN_VALUE_SELECT.
  DATA: L_MODEL TYPE ZTBM_ABXOPVDT-CARX,
        L_CLNO  TYPE ZTBM_ABXOPVDT-CLNO.
  MOVE IS219-MODEL TO L_MODEL .
  SELECT * FROM  ZTBM_ABXOPVDT
    INTO  CORRESPONDING  FIELDS OF TABLE IT_APP220
    WHERE CARX   EQ   L_MODEL
      AND CLNO   EQ   IS219-NAME219.
  IF SY-SUBRC <> 0.
    L_CLNO = IS219-NAME219.
    DO.
      L_CLNO = L_CLNO + 1.
      SELECT * FROM ZTBM_ABXOPVDT
        INTO CORRESPONDING FIELDS OF TABLE IT_APP220
        WHERE CARX EQ L_MODEL AND
              CLNO EQ L_CLNO    .
      IF SY-SUBRC = 0.
        IS219-NAME219 = L_CLNO.
        EXIT.
      ENDIF.

      IF L_CLNO > 219.
        EXIT.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.                    " column_value_select
*&---------------------------------------------------------------------*
*&      Form  call_trans_4104
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CALL_TRANS_4104.
  DATA: L_TYPE LIKE SY-REPID .
  CASE 'X'.
    WHEN P_01_4104.  " ALC Sequence Summary (Hourly)
      CALL TRANSACTION 'ZPPR0201' . " AND SKIP FIRST SCREEN .
    WHEN P_02_4104.  " ALC Sequence Summary (Daily)
      CALL TRANSACTION 'ZPPR0202' . " AND SKIP FIRST SCREEN .
    WHEN P_03_4104.  " ALC Sequence Summary (Weekly)
      CALL TRANSACTION 'ZPPR0203' . " AND SKIP FIRST SCREEN .
    WHEN P_04_4104.  " Body Input Plan List
      CALL TRANSACTION 'ZPPR0207' . " AND SKIP FIRST SCREEN .
    WHEN P_05_4104.  " Trim Input Plan List
      CALL TRANSACTION 'ZPPR0208' . " AND SKIP FIRST SCREEN .
    WHEN P_06_4104.  " Monthly Production Result List
      CALL TRANSACTION 'ZPPR0209' . " AND SKIP FIRST SCREEN .
    WHEN P_07_4104.  " Vehicle Sequence List
      CALL TRANSACTION 'ZPPR0205' . " AND SKIP FIRST SCREEN .
    WHEN P_08_4104.  " Vehicle Status List
      CALL TRANSACTION 'ZPPR0204' . " AND SKIP FIRST SCREEN .
    WHEN P_09_4104.  " Wire Mixture - Hourly
      CALL TRANSACTION 'ZPPR0206' . " AND SKIP FIRST SCREEN .
    WHEN P_10_4104.  " Wire Mixture - Daily
      CALL TRANSACTION 'ZPPR0210' . " AND SKIP FIRST SCREEN .
  ENDCASE.
ENDFORM.                    " call_trans_4104

*&---------------------------------------------------------------------*
*&      Form  get_wono_app302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_WO_APP302  text
*      -->P_P_MODEL_APP302  text
*      -->P_P_PART_APP302  text
*      -->P_P_COLUMN_APP302  text
*----------------------------------------------------------------------*
FORM GET_WONO_APP302 TABLES   PT_WO_APP302 STRUCTURE IT_WO_APP302
                     USING    P_MODEL  P_PART  P_COL  P_CODE     .
  DATA: L_ATNAM TYPE CABN-ATNAM.

  CONCATENATE 'P_ALC_' P_PART '_' P_COL  INTO L_ATNAM .
* Check Column  &
* Get W/O No. and ALC Code's Value
  SELECT AU~OBJEK AU~ATWRT
    INTO (PT_WO_APP302-WONO , PT_WO_APP302-ALC_VALS)
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '001'   AND
          CA~ATNAM = L_ATNAM   .
*   Check Model.
    SELECT SINGLE AU~OBJEK
      INTO PT_WO_APP302-WONO
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE AU~KLART = '001'             AND
            AU~OBJEK = PT_WO_APP302-WONO AND
            CA~ATNAM = 'P_MODEL'         AND
            AU~ATWRT = P_MODEL             .
    IF SY-SUBRC <> 0.
      CONTINUE.
    ENDIF.
    IF P_CODE <> SPACE .
*     Check ALC's Value.
      SELECT SINGLE AU~OBJEK
        INTO PT_WO_APP302-WONO
        FROM AUSP AS AU
          INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
        WHERE AU~KLART = '001'             AND
              AU~OBJEK = PT_WO_APP302-WONO AND
              CA~ATNAM = L_ATNAM           AND
              AU~ATWRT = P_CODE              .
      IF SY-SUBRC <> 0.
        CONTINUE.
      ELSE.
        CONCATENATE P_PART P_COL
          INTO PT_WO_APP302-ALC_CODE .
        APPEND PT_WO_APP302          .
      ENDIF.
    ELSE.
      CONCATENATE P_PART P_COL
        INTO PT_WO_APP302-ALC_CODE .
      APPEND PT_WO_APP302          .
    ENDIF.
  ENDSELECT.
ENDFORM.                    " get_wono_app302
*&---------------------------------------------------------------------*
*&      Form  previous_result_app302
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZTPP_SEQ_SUM02_ALC_CODE  text
*      -->P_ZTPP_SEQ_SUM02_ALC_VALS  text
*      <--P_LT_DAY_B_RESULT  text
*----------------------------------------------------------------------*
FORM PREVIOUS_RESULT_APP302 USING    P_CODE  P_VALS  P_RP
                            CHANGING P_RESULT.
  DATA: L_WO_SER TYPE ZTPP_DAY_SUM-WO_SER,
        L_NATION TYPE ZTPP_DAY_SUM-NATION,
        L_DEALER TYPE ZTPP_DAY_SUM-DEALER,
        L_WDATE  TYPE ZTPP_DAY_SUM-WDATE ,
        L_FIELD(40)                      .
  RANGES: LR_EXTC FOR ZTPP_DAY_SUM-EXTC  ,
          LR_INTC FOR ZTPP_DAY_SUM-INTC  .
  FIELD-SYMBOLS <FS_QTY> TYPE ANY.

  CONCATENATE 'ZTPP_DAY_SUM-RP' P_RP 'Q'
    INTO L_FIELD.
  ASSIGN (L_FIELD) TO <FS_QTY>.
  L_WDATE = P_DATE_APP302 - 1 .
  LOOP AT IT_WO_APP302 WHERE ALC_CODE = P_CODE AND
                             ALC_VALS = P_VALS   .
    L_WO_SER = IT_WO_APP302-WONO+00(09) .
    L_NATION = IT_WO_APP302-WONO+09(03) .
    L_DEALER = IT_WO_APP302-WONO+12(02) .
    CASE P_CODE+00(01).
      WHEN 'U'.  "Unique Part
        CLEAR: LR_EXTC, LR_EXTC[],
               LR_INTC, LR_INTC[].
      WHEN 'C'.  "Color Part
        LR_EXTC-SIGN   = 'I' .
        LR_EXTC-OPTION = 'EQ'.
        LR_EXTC-LOW    = IT_WO_APP302-WONO+14(02).
        APPEND LR_EXTC .
        LR_INTC-SIGN   = 'I' .
        LR_INTC-OPTION = 'EQ'.
        LR_INTC-LOW    = IT_WO_APP302-WONO+14(02).
        APPEND LR_INTC .
    ENDCASE.
  ENDLOOP.
ENDFORM.                    " previous_result_app302

*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_HEADER_APP263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_FIELD_HEADER_APP263.
  ST_APP263-D01 = ST_APP263-DATE + 1.
  ST_APP263-D02 = ST_APP263-DATE + 2.
  ST_APP263-D03 = ST_APP263-DATE + 3.
  ST_APP263-D04 = ST_APP263-DATE + 4.
  ST_APP263-D05 = ST_APP263-DATE + 5.
  ST_APP263-D06 = ST_APP263-DATE + 6.
  ST_APP263-D07 = ST_APP263-DATE + 7.
ENDFORM.                    " SET_FIELD_HEADER_APP263
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_OPT_APP263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*      -->P_3851   text
*----------------------------------------------------------------------*
FORM SET_FIELD_OPT_APP263 USING    P_NAME
                                   P_PARAMETER.
  DATA: L_ATNAM TYPE CABN-ATNAM,
        L_OBJEK TYPE AUSP-OBJEK.

  L_ATNAM = 'P_219_9'.

  SELECT DISTINCT AU~ATWRT
    INTO XVALUE-KEY
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002' AND
          CA~ATNAM = L_ATNAM .
    APPEND XVALUE TO XLIST.
  ENDSELECT.

  PERFORM LIST_BOX_FUNCTION USING P_NAME.

ENDFORM.                    " SET_FIELD_OPT_APP263
*&---------------------------------------------------------------------*
*&      Form  search_data_app263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_DATA_APP263.
  DATA: L_ERROR,
        L_TEXT(40),
        LT_PROD     LIKE IT_APP263 OCCURS 0 WITH HEADER LINE,
        LT_PER_DAY  LIKE IT_APP263 OCCURS 0 WITH HEADER LINE.

  PERFORM CHECKING_PARAMETER_APP263 USING L_ERROR  L_TEXT .
  IF L_ERROR = 'X'.
    CONCATENATE 'Set The Parameter -' L_TEXT
      INTO L_TEXT SEPARATED BY SPACE.
    MESSAGE I000 WITH L_TEXT .
    EXIT.
  ENDIF.

  PERFORM READ_PRODUCTION_APP263 TABLES LT_PROD
                                 USING  ST_APP263-DATE
                                        ST_APP263-OPT
                                        WA_MODEL      .
  PERFORM READ_7_DAYS_PROD_APP263 TABLES LT_PER_DAY
                                  USING  ST_APP263-DATE
                                         ST_APP263-OPT
                                         WA_MODEL      .
* It should be clear to define fields - PBS, P/Rej and Plan QTYs.

  LOOP AT LT_PROD.
    READ TABLE LT_PER_DAY WITH KEY MODEL = LT_PROD-MODEL
                                   SITE  = LT_PROD-SITE
                                   ENG   = LT_PROD-ENG .
    IF SY-SUBRC = 0 .
      MOVE-CORRESPONDING LT_PER_DAY TO LT_PROD.
    ENDIF.
    MODIFY LT_PROD.
  ENDLOOP.
  LOOP AT LT_PER_DAY.
    READ TABLE LT_PROD WITH KEY MODEL = LT_PER_DAY-MODEL
                                SITE  = LT_PER_DAY-SITE
                                ENG   = LT_PER_DAY-ENG .
    IF SY-SUBRC <> 0.
      MOVE-CORRESPONDING LT_PER_DAY TO LT_PROD.
      APPEND LT_PROD.
    ENDIF.
  ENDLOOP.

  CLEAR: IT_APP263, IT_APP263[].
  SORT LT_PROD BY MODEL SITE ENG.
  LOOP AT LT_PROD.
    MOVE-CORRESPONDING LT_PROD TO IT_APP263.
    APPEND IT_APP263.
    AT END OF SITE.
      SUM.
      MOVE-CORRESPONDING LT_PROD TO IT_APP263.
      CLEAR: IT_APP263-MODEL, IT_APP263-SITE, IT_APP263-ENG.
      MOVE 'Sub-Total' TO IT_APP263-ENG.
      APPEND IT_APP263.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " search_data_app263

*&---------------------------------------------------------------------*
*&      Form  CHECKING_PARAMETER_APP263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECKING_PARAMETER_APP263 USING P_ERROR  P_TEXT .
  IF ST_APP263-DATE = SPACE.
    P_ERROR = 'X'.
    P_TEXT = 'DATE'.
    EXIT.
  ENDIF.

  IF WA_MODEL       = SPACE.
    P_ERROR = 'X'.
    P_TEXT  = 'Model'.
    EXIT.
  ENDIF.

ENDFORM.                    " CHECKING_PARAMETER_APP263
*&---------------------------------------------------------------------*
*&      Form  read_production_app263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PROD  text
*      -->P_ST_APP263_DATE  text
*      -->P_ST_APP263_OPT  text
*      -->P_ST_APP263_MODEL  text
*----------------------------------------------------------------------*
FORM READ_PRODUCTION_APP263 TABLES   PT_PROD STRUCTURE IT_APP263
                            USING    P_DATE   "Basis Date
                                     P_OPT    "Engine Part
                                     P_MODEL.
  DATA: LT_PROD_T  LIKE IT_APP263 OCCURS 0 WITH HEADER LINE,
        LT_DAILY_T LIKE IT_APP263 OCCURS 0 WITH HEADER LINE,
        LT_DAILY   LIKE IT_APP263 OCCURS 0 WITH HEADER LINE.

  DATA: L_OBJEK     TYPE AUSP-OBJEK,  "V/M No.
        L_ATFLV_ST  TYPE AUSP-ATFLV,  "From
        L_ATFLV_EN  TYPE AUSP-ATFLV,  "To
        L_PROD_DATE TYPE AUSP-ATFLV,  "Prod. Date
        L_DATE      TYPE SY-DATUM  ,
        L_NUM(08)   TYPE N         ,
        L_SUBRC     TYPE SY-SUBRC  .

  MOVE 'P_RP07_SHOP_DATE' TO L_ATNAM .  "T/IN Prod. Date
  CONCATENATE P_DATE+00(06) '01'
    INTO L_DATE .
  L_ATFLV_ST = L_NUM = L_DATE   . "The First Day of The Month
  L_ATFLV_EN = L_NUM = P_DATE   .
* Check and Get T/IN Prod. Date
  SELECT AU~OBJEK AU~ATFLV
    INTO (L_OBJEK , L_PROD_DATE)
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002'       AND
          CA~ATNAM = L_ATNAM     AND
          AU~ATFLV >= L_ATFLV_ST AND
          AU~ATFLV <= L_ATFLV_EN   .
*
    CLEAR LT_PROD_T .
*
    IF P_MODEL <> SPACE.
*     Check Model
      PERFORM CHECK_CLASS_APP263 USING    L_OBJEK
                                          'P_MODEL'
                                          P_MODEL
                                 CHANGING LT_PROD_T-MODEL
                                          L_SUBRC       .
      IF L_SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ELSE.
*     Get Model
      PERFORM GET_CLASS_APP263 USING    L_OBJEK
                                        'P_MODEL'
                               CHANGING LT_PROD_T-MODEL
                                        L_SUBRC       .
*      if l_subrc <> 0.
*        continue.
*      endif.
    ENDIF.
    IF ST_APP263-OPT <> SPACE.
*     Check Engine
      PERFORM CHECK_CLASS_APP263 USING    L_OBJEK
                                          'P_219_9'
                                          P_OPT
                                 CHANGING LT_PROD_T-ENG
                                          L_SUBRC     .
      IF L_SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ELSE.
*     Get Engine's Code
      PERFORM GET_CLASS_APP263 USING    L_OBJEK
                                        'P_219_9'
                               CHANGING LT_PROD_T-ENG
                                        L_SUBRC     .
*      if l_subrc <> 0.
*        continue.
*      endif.
    ENDIF.
*   Counting Month and Daily Prod. Results
    L_DATE = L_NUM = L_PROD_DATE.
    IF L_DATE = P_DATE.
      LT_PROD_T-DAILY_TIN_R = 1 .
      LT_PROD_T-MON_TIN_R   = 1 .
    ELSE.
      LT_PROD_T-MON_TIN_R   = 1 .
    ENDIF.
*   Get site
    PERFORM GET_CLASS_APP263 USING    L_OBJEK
                                      'P_WORK_ORDER'
                             CHANGING LT_PROD_T-SITE
                                      L_SUBRC      .
    MOVE LT_PROD_T-SITE+00(01) TO LT_PROD_T-SITE .
*
    APPEND LT_PROD_T .
*
  ENDSELECT.

  SORT LT_PROD_T BY MODEL SITE ENG .
  LOOP AT LT_PROD_T.
    CLEAR PT_PROD.
    MOVE-CORRESPONDING LT_PROD_T TO PT_PROD.
    COLLECT PT_PROD.
  ENDLOOP.

ENDFORM.                    " read_production_app263
*&---------------------------------------------------------------------*
*&      Form  check_class_app263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_OBJEK  text
*      -->P_7297   text
*      -->P_P_MODEL  text
*      <--P_LT_PROD_T_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_CLASS_APP263 USING    P_OBJEK
                                 P_ATNAM
                                 P_ATWRT
                        CHANGING PT_ATWRT
                                 P_SUBRC.
  SELECT SINGLE AU~ATWRT
    INTO PT_ATWRT
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002'     AND
          AU~OBJEK = P_OBJEK   AND
          CA~ATNAM = P_ATNAM   AND
          AU~ATWRT = P_ATWRT     .
  MOVE SY-SUBRC TO P_SUBRC .

ENDFORM.                    " check_class_app263
*&---------------------------------------------------------------------*
*&      Form  get_class_app263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_OBJEK  text
*      -->P_7313   text
*      <--P_LT_PROD_T_MODEL  text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM GET_CLASS_APP263 USING    P_OBJEK
                               P_ATNAM
                      CHANGING P_ATWRT
                               P_SUBRC.
  SELECT SINGLE AU~ATWRT
    INTO P_ATWRT
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002'     AND
          AU~OBJEK = P_OBJEK   AND
          CA~ATNAM = P_ATNAM     .
  MOVE SY-SUBRC TO P_SUBRC .

ENDFORM.                    " get_class_app263
*&---------------------------------------------------------------------*
*&      Form  READ_7_DAYS_PROD_APP263
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_PER_DAY  text
*      -->P_ST_APP263_DATE  text
*      -->P_ST_APP263_OPT  text
*      -->P_ST_APP263_MODEL  text
*----------------------------------------------------------------------*
FORM READ_7_DAYS_PROD_APP263 TABLES   PT_PROD STRUCTURE IT_APP263
                             USING    P_DATE
                                      P_OPT
                                      P_MODEL.
  DATA: LT_PROD_T   LIKE IT_APP263 OCCURS 0 WITH HEADER LINE,
        L_OBJEK     TYPE AUSP-OBJEK,
        L_ATNAM     TYPE CABN-ATNAM,
        L_DATE      TYPE SY-DATUM  ,
        L_SUBRC     TYPE SY-SUBRC  ,
*
        L_1DAY      TYPE SY-DATUM  ,
        L_2DAY      TYPE SY-DATUM  ,
        L_3DAY      TYPE SY-DATUM  ,
        L_4DAY      TYPE SY-DATUM  ,
        L_5DAY      TYPE SY-DATUM  ,
        L_6DAY      TYPE SY-DATUM  ,
        L_7DAY      TYPE SY-DATUM  ,
*
        L_NUM(08)   TYPE N         ,
        L_PROD_DATE TYPE AUSP-ATFLV,
        L_ATFLV_ST  TYPE AUSP-ATFLV,
        L_ATFLV_EN  TYPE AUSP-ATFLV.

  L_1DAY = P_DATE + 1. L_2DAY = P_DATE + 2.
  L_3DAY = P_DATE + 3. L_4DAY = P_DATE + 4.
  L_5DAY = P_DATE + 5. L_6DAY = P_DATE + 6.
  L_7DAY = P_DATE + 7.
**D+1, ... , D+7's Prod. Results
  MOVE 'P_RP18_SHOP_DATE' TO L_ATNAM .  "S/OFF Prod. Date
  L_DATE     = P_DATE + 1.
  L_ATFLV_ST = L_NUM = L_DATE.                              "D + 1
  L_DATE     = P_DATE + 7.
  L_ATFLV_EN = L_NUM = L_DATE.                              "D + 7
  SELECT AU~OBJEK AU~ATFLV
    INTO (L_OBJEK , L_PROD_DATE)
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002'       AND
          CA~ATNAM = L_ATNAM     AND
          AU~ATFLV >= L_ATFLV_ST AND
          AU~ATFLV <= L_ATFLV_EN   .
*
    CLEAR LT_PROD_T .
*
    IF P_MODEL <> SPACE.
*     Check Model
      PERFORM CHECK_CLASS_APP263 USING    L_OBJEK
                                          'P_MODEL'
                                          P_MODEL
                                 CHANGING LT_PROD_T-MODEL
                                          L_SUBRC       .
      IF L_SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ELSE.
*     Get Model
      PERFORM GET_CLASS_APP263 USING    L_OBJEK
                                        'P_MODEL'
                               CHANGING LT_PROD_T-MODEL
                                        L_SUBRC       .
      IF L_SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
    IF ST_APP263-OPT <> SPACE.
*     Check Engine
      PERFORM CHECK_CLASS_APP263 USING    L_OBJEK
                                          'P_219_9'
                                          P_OPT
                                 CHANGING LT_PROD_T-ENG
                                          L_SUBRC     .
      IF L_SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ELSE.
*     Get Engine's Code
      PERFORM GET_CLASS_APP263 USING    L_OBJEK
                                        'P_219_9'
                               CHANGING LT_PROD_T-ENG
                                        L_SUBRC     .
      IF L_SUBRC <> 0.
        CONTINUE.
      ENDIF.
    ENDIF.
*   Counting Daily Prod. Results
    L_DATE = L_NUM = L_PROD_DATE.
    CASE L_DATE .
      WHEN L_1DAY. LT_PROD_T-DAY1 = 1.
      WHEN L_2DAY. LT_PROD_T-DAY2 = 1.
      WHEN L_3DAY. LT_PROD_T-DAY3 = 1.
      WHEN L_4DAY. LT_PROD_T-DAY4 = 1.
      WHEN L_5DAY. LT_PROD_T-DAY5 = 1.
      WHEN L_6DAY. LT_PROD_T-DAY6 = 1.
      WHEN L_7DAY. LT_PROD_T-DAY7 = 1.
    ENDCASE.
*   Get site
    PERFORM GET_CLASS_APP263 USING    L_OBJEK
                                      'P_WORK_ORDER'
                             CHANGING LT_PROD_T-SITE
                                      L_SUBRC      .
    MOVE LT_PROD_T-SITE+00(01) TO LT_PROD_T-SITE .
*
    APPEND LT_PROD_T .
*
  ENDSELECT.

  SORT LT_PROD_T BY MODEL SITE ENG .
  LOOP AT LT_PROD_T.
    CLEAR PT_PROD.
    MOVE-CORRESPONDING LT_PROD_T TO PT_PROD.
    COLLECT PT_PROD.
  ENDLOOP.

ENDFORM.                    " READ_7_DAYS_PROD_APP263
*&---------------------------------------------------------------------*
*&      Form  create_rp_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREATE_RP_TABLE.
  DATA: L_NUM(02) TYPE N,
        WA_DATE(30).
  DATA: L_ATNAM   TYPE CABN-ATNAM.

  REFRESH: IT_RP_APP236.  CLEAR IT_RP_APP236.
  DO 28 TIMES.
    L_NUM = L_NUM + 1.
    IT_RP_APP236-SH_NAME = L_NUM .

*    CONCATENATE 'P_RP' l_num '_SHOP_DATE'
    CONCATENATE 'P_RP' L_NUM '_ACTUAL_DATE'

    INTO L_ATNAM.
    CLEAR IT_VMV_APP236.
    READ TABLE IT_VMV_APP236 WITH KEY ATNAM = L_ATNAM.
    MOVE IT_VMV_APP236-ATWRT TO WA_DATE.

    IF NOT WA_DATE IS INITIAL.
      CONCATENATE WA_DATE+4(2) '/' WA_DATE+6(2) '/'
                  WA_DATE+0(4) INTO IT_RP_APP236-SH_DATE.

      CONCATENATE WA_DATE+8(2) ':' WA_DATE+10(2) ':'
                  WA_DATE+12(2) INTO IT_RP_APP236-SER_NUM.
    ELSE.
      CLEAR: IT_RP_APP236-SH_DATE,
             IT_RP_APP236-SER_NUM.
    ENDIF.

*    CONCATENATE 'P_RP' l_num '_SERIAL'
*      INTO l_atnam.
*    CLEAR it_vmv_app236.
*    READ TABLE it_vmv_app236 WITH KEY atnam = l_atnam.
*    MOVE it_vmv_app236-atwrt TO it_rp_app236-ser_num.

    APPEND IT_RP_APP236.
  ENDDO.
ENDFORM.                    " create_rp_table

*&---------------------------------------------------------------------*
*&      Form  get_knobj
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNNAM  text
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNOBJ_APP207 USING    PA_KNNAM  PA_KNOBJ.
  SELECT SINGLE KNOBJ INTO PA_KNOBJ
    FROM CUCO
   WHERE OBTAB = 'MARA'
     AND OBJEK = PA_KNNAM .
ENDFORM.                    " GET_KNOBJ

*&---------------------------------------------------------------------*
*&      Form  get_KNNUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_KNOBJ  text
*----------------------------------------------------------------------*
FORM GET_KNNUM_APP207 USING    PA_KNOBJ.
  SELECT KNNUM APPENDING CORRESPONDING FIELDS OF TABLE IT_ERR
    FROM CUOB
   WHERE KNOBJ = PA_KNOBJ.
ENDFORM.                    " get_KNNUM

*&---------------------------------------------------------------------*
*&      Form  GET_WINDOWS_CLIFILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_4403   text
*      <--P_WA_FILENAME_APP207  text
*----------------------------------------------------------------------*
FORM GET_WINDOWS_CLIFILE_APP207 USING MASK CHANGING
                         CLIFILE    LIKE RLGRAP-FILENAME .
  DATA WINSYS(3).
  DATA TMP_CLIFILE    LIKE RLGRAP-FILENAME .

  IF CLIFILE IS INITIAL.
    SET PARAMETER ID 'GR8' FIELD TMP_CLIFILE.
    IF SY-SUBRC NE 0.CLEAR  TMP_CLIFILE.ENDIF.
  ELSE.
    TMP_CLIFILE =  CLIFILE.
  ENDIF.
  CALL FUNCTION 'WS_QUERY'
       EXPORTING
            QUERY  = 'WS'
       IMPORTING
            RETURN = WINSYS.

  IF WINSYS(2) NE 'WN'.
    MESSAGE E016(14).
  ENDIF.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
       DEF_FILENAME    = TMP_CLIFILE
       DEF_PATH         = TMP_CLIFILE
       MASK             = MASK
       MODE             = 'S'
       TITLE            = SY-TITLE
    IMPORTING
*ESO 11.04.01 d?ut de correction
       FILENAME         = TMP_CLIFILE
*       CLIFILE         = TMP_CLIFILE
*ESO 11.04.01 fin de correction de correction
*       RC               = RC
      EXCEPTIONS
         INV_WINSYS       = 1
         NO_BATCH         = 2
         SELECTION_CANCEL = 3
         SELECTION_ERROR  = 4
         OTHERS           = 5.

  IF SY-SUBRC EQ 0.
    CLIFILE = TMP_CLIFILE.
  ENDIF.
ENDFORM.                               " GET_WINDOWS_CLIFILE

*&---------------------------------------------------------------------*
*&      Form  read_wo_inf_1007
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ORDER  text
*----------------------------------------------------------------------*
FORM READ_WO_INF_1007 USING    P_WO_PACK.
  DATA: L_HEADER LIKE MARA-MATNR,
        L_COLOR  LIKE MARA-MATNR.

  DATA: BEGIN OF LT_WO_H OCCURS 0,
          MATNR TYPE MARA-MATNR,
        END OF LT_WO_H .
  DATA: LT_WO_C LIKE TABLE OF LT_WO_H WITH HEADER LINE.

  IF P_WO_PACK = SPACE.
    MESSAGE S001 WITH 'Set The Parameter - WO Pack!!' .
    EXIT.
  ELSE.
    CONCATENATE P_WO_PACK '%'
      INTO L_HEADER .
  ENDIF.
* Read W/O Header No. with P_PERF_YN = 'N'.
  SELECT DISTINCT MA~MATNR
    INTO LT_WO_H-MATNR
    FROM ( ( MARA AS MA
         INNER JOIN AUSP AS AU ON MA~MATNR = AU~OBJEK )
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
   WHERE MA~MTART = 'WOHD'      AND
         MA~MBRSH = 'A'         AND
         MA~KZKFG = SPACE       AND
         CA~ATNAM = 'P_PERF_YN' AND
         AU~ATWRT = 'N'         AND
         MA~MATNR LIKE L_HEADER    .
    IF SY-SUBRC = 0.
      APPEND LT_WO_H.
    ELSE.
      MESSAGE S001 WITH 'No Data!!'.
      EXIT.
    ENDIF.

*   Read W/O Color No. with P_PERF_YN = 'N'.
    CONCATENATE LT_WO_H-MATNR '%'
      INTO L_COLOR .
    SELECT DISTINCT MA~MATNR
      INTO LT_WO_C-MATNR
      FROM ( ( MARA AS MA
           INNER JOIN AUSP AS AU ON MA~MATNR = AU~OBJEK )
           INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
      WHERE MA~MTART = 'WOCL'      AND
            MA~MBRSH = 'A'         AND
            MA~KZKFG = SPACE       AND
            CA~ATNAM = 'P_PERF_YN' AND
            AU~ATWRT = 'N'         AND
            MA~MATNR LIKE L_COLOR    .
      IF SY-SUBRC = 0 .
        APPEND LT_WO_C.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDSELECT.

  ENDSELECT.

* Search Error Data with W/O Header No.
  CLEAR: IT_ERR, IT_ERR[].
  LOOP AT LT_WO_H .
    PERFORM SEARCH_ERROR_APP207 USING LT_WO_H-MATNR
                                      'U'         .
  ENDLOOP.
* Search Error Data with W/O Color No.
  LOOP AT LT_WO_C.
    PERFORM SEARCH_ERROR_APP207 USING LT_WO_C-MATNR
                                      'C'         .
  ENDLOOP.
  DESCRIBE TABLE IT_ERR LINES TC_A107-LINES.
  MOVE TC_A107-LINES TO WA_TOTQTY.
  IF WA_TOTQTY = 0.
    MESSAGE S001 WITH TEXT-100 .
  ENDIF.
ENDFORM.                    " read_wo_inf_1007
*&---------------------------------------------------------------------*
*&      Form  search_error_app207
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_WO_C_MATNR  text
*----------------------------------------------------------------------*
FORM SEARCH_ERROR_APP207 USING    P_WO
                                  P_TYPE .
  DATA: L_WO_C LIKE MARA-MATNR,
        L_WO_H LIKE MARA-MATNR.
  DATA: LT_CUKB              LIKE TABLE OF CUKB WITH HEADER LINE,
        LT_VALS  LIKE TABLE OF IT_VALS_APP207   WITH HEADER LINE,
        L_MATNR              LIKE MARA-MATNR,
        L_CHK(10)            TYPE C         ,
        L_MODEL              TYPE AUSP-ATWRT,
*        L_LEN(2)             TYPE N         ,
*        L_MODELLEN           TYPE I         ,
        L_CNT(3)             TYPE N         ,
        L_KNNUM              LIKE CUOB-KNNUM,
        L_KNOBJ              LIKE CUCO-KNOBJ,
        L_KNNAM              LIKE CUKB-KNNAM.

* Search Model Type
  SELECT SINGLE AU~ATWRT
    INTO L_MODEL
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~OBJEK = P_WO AND
          AU~KLART = '001'  AND
          CA~ATNAM = 'P_MODEL' .
* Set Work Order Color No.
  L_MATNR = P_WO.

**
  CASE P_TYPE.
    WHEN 'U'.
      CONCATENATE L_MODEL '_WOHD'
        INTO  L_KNNAM.
      PERFORM GET_KNOBJ_APP207 USING L_KNNAM  L_KNOBJ.
*     Read Initial ALC Data For Displaying Error
      PERFORM GET_KNNUM_APP207 USING L_KNOBJ.
    WHEN 'C'.
      CONCATENATE L_MODEL  '_WOCL'
        INTO  L_KNNAM.
      PERFORM GET_KNOBJ_APP207 USING L_KNNAM  L_KNOBJ.
*     Read Initial ALC Data For Displaying Error
      PERFORM GET_KNNUM_APP207 USING L_KNOBJ.
  ENDCASE.

  CONCATENATE 'D_' L_MODEL '_ALC_'     INTO L_CHK .

* Modify Error Data
  LOOP AT IT_ERR    .
    SELECT SINGLE KNNAM
      INTO CORRESPONDING FIELDS OF IT_ERR
      FROM CUKB
     WHERE KNNUM = IT_ERR-KNNUM .

    IF IT_ERR-KNNAM(10) NE  L_CHK     .
      DELETE IT_ERR      .
      CONTINUE .
    ENDIF.
    IT_ERR-CTYPE = IT_ERR-KNNAM+10(1) .
    MODIFY IT_ERR.
  ENDLOOP.

  LOOP AT IT_ERR .
    CLEAR: LT_VALS, LT_VALS[].
    CONCATENATE 'P_' IT_ERR-KNNAM+6(9) INTO LT_VALS-ATNAM.
    APPEND LT_VALS.
    CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
         EXPORTING
              OBJECT       = L_MATNR
              CTYPE        = '001'
         TABLES
              VAL_TABLE    = LT_VALS
         EXCEPTIONS
              NO_DATA      = 1
              ERROR_MODE   = 2
              ERROR_OBJECT = 3
              ERROR_VALUE  = 4
              OTHERS       = 5.

    READ TABLE LT_VALS INDEX 1 .
    IF LT_VALS-ZFLAG NE SPACE OR LT_VALS-ATWRT = SPACE
       OR LT_VALS-ATWRT = '????' .
      L_CNT        = L_CNT + 1           .
      IT_ERR-NO    = L_CNT               .
      IT_ERR-COL   = IT_ERR-KNNAM+12(3) .
      SELECT SINGLE KNKTX INTO IT_ERR-COLNM
        FROM CUKBT
       WHERE KNNUM = IT_ERR-KNNUM
         AND SPRAS = SY-LANGU     .
      IF IT_ERR-OBJKEY = SPACE.
        IT_ERR-OBJKEY = P_WO.
      ENDIF.
      CONCATENATE TEXT-003 '(' IT_ERR-KNNAM ')' INTO IT_ERR-COLDC .
      MODIFY IT_ERR .
    ELSE.
      DELETE IT_ERR.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " search_error_app207

*&---------------------------------------------------------------------*
*&      Form  get_alc_condition_app207
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_RESULT1001  text
*      -->P_3319   text
*----------------------------------------------------------------------*
FORM GET_ALC_CONDITION_APP207 TABLES PA_VALS STRUCTURE  IT_RESULT1001
                              USING  PA_TYPE
                                     PA_WO_H
                                     PA_WO_C.
  DATA: LT_CUKB                 LIKE TABLE OF CUKB  WITH HEADER LINE,
        L_MODEL                 LIKE WA_CAR    ,
        L_LINES                 TYPE I,
        L_CNT                   TYPE I ,
        L_OBJKEY                LIKE CUXREF-OBJKEY,
        L_ERROR                 TYPE C,
        L_KNOBJ                 LIKE CUCO-KNOBJ,
        L_KNNAM                 LIKE CUKB-KNNAM.

*----> ALC Value Check for the existed with Error
*  if wa_model = 'DAF'.
*    l_model = 'EMF'.
*  else.
*    l_model = wa_model  .
*  endif.
  READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_MODEL'.
  MOVE IT_RESULT1001-ATWRT TO L_MODEL.
  CLEAR: IT_ALC, IT_ALC[].

  CASE PA_TYPE.
    WHEN 'U'.
      CONCATENATE L_MODEL '_WOHD' INTO L_KNNAM.
      PERFORM GET_KNOBJ USING L_KNNAM L_KNOBJ.
      PERFORM GET_KNNUM                USING L_KNOBJ.
    WHEN 'C'.
      CONCATENATE L_MODEL '_WOCL' INTO L_KNNAM.
      PERFORM GET_KNOBJ USING L_KNNAM L_KNOBJ.
      PERFORM GET_KNNUM                USING L_KNOBJ.
  ENDCASE.

  LOOP AT IT_ALC  .
    SELECT SINGLE B~KNNAM T~KNKTX
      INTO CORRESPONDING FIELDS OF IT_ALC
      FROM CUKB AS B INNER JOIN CUKBT AS T
        ON B~KNNUM = T~KNNUM
     WHERE B~KNNUM = IT_ALC-KNNUM
       AND T~SPRAS = SY-LANGU   .

    IF  IT_ALC-KNNAM(10) NE 'D_EMF_ALC_' .
      DELETE  IT_ALC .
      CONTINUE .
    ENDIF.
    " Field meaning translate .. LT_CUKB-KNAUS : Characteristic Name..
    "                            LT_CUKB-KNART2: Unique / Head
    CONCATENATE 'P'  IT_ALC-KNNAM+5(10)  INTO  IT_ALC-KNAUS    .
    IT_ALC-KNART2 =  IT_ALC-KNNAM+10(1) .
    MODIFY  IT_ALC .
  ENDLOOP.

  LOOP AT IT_ALC  .
    READ TABLE PA_VALS WITH KEY ATNAM =  IT_ALC-KNAUS .
    IF SY-SUBRC NE 0 OR PA_VALS-ATWRT = SPACE         .
      L_ERROR = 'X'.
      IF PA_TYPE = 'U'.
        IT_ERR-OBJKEY = PA_WO_H.
      ELSE.
        IT_ERR-OBJKEY = PA_WO_C.
      ENDIF.
      CONCATENATE 'ERROR:'  IT_ALC-KNAUS INTO IT_ERR-OBJTYP.
      APPEND IT_ERR.
    ENDIF.
  ENDLOOP.

  SORT IT_ERR BY OBJKEY.   CLEAR: L_LINES, L_CNT.
  READ TABLE IT_ERR INDEX 1.   L_OBJKEY = IT_ERR-OBJKEY.
  LOOP AT IT_ERR.
    IF L_OBJKEY = IT_ERR-OBJKEY..
      L_LINES = L_LINES + 1.
      CONTINUE.
    ELSE.
      L_CNT = L_CNT + 1 .
      IT_ERR-CUCOUNT = L_LINES.
      MODIFY IT_ERR TRANSPORTING CUCOUNT WHERE OBJKEY = L_OBJKEY .
      L_OBJKEY = IT_ERR-OBJKEY.
      L_LINES  = 1.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE IT_ERR LINES WA_TOTQTY.
  IF WA_TOTQTY > 0.
    IT_ERR-CUCOUNT = L_LINES.
    L_CNT = L_CNT + 1 .
    MODIFY IT_ERR TRANSPORTING CUCOUNT WHERE OBJKEY = L_OBJKEY .
  ENDIF.

  WA_TOTQTY = L_CNT.
ENDFORM.                    " get_alc_condition_app207

*&---------------------------------------------------------------------*
*&      Form  clear_parameters_app207
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_PARAMETERS_APP207.
  DATA: L_MATNR TYPE MARA-MATNR.
  MOVE WA_WO_PACK TO L_MATNR.
  PERFORM SET_CAR_NAME USING L_MATNR.
ENDFORM.                    " clear_parameters_app207
*&---------------------------------------------------------------------*
*&      Form  display_app207
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_APP207.
  DATA: L_COLOR              LIKE MARA-MATNR,
        L_HEADER             LIKE AUSP-OBJEK .

  CLEAR: IT_RESULT1001, IT_RESULT1001[].
  READ TABLE IT_ERR WITH KEY MARK = 'X'.
  IF SY-SUBRC <> 0.
    MESSAGE S000 WITH 'No Data!!'.
    EXIT.
  ELSE.
    L_COLOR = IT_ERR-OBJKEY.
    L_HEADER = IT_ERR-OBJKEY+00(14).
  ENDIF.

  CALL FUNCTION 'Z_FPP_HANDLING_MASTER'
       EXPORTING
            OBJECT       = L_COLOR
            CTYPE        = '001'
       TABLES
            VAL_TABLE    = IT_RESULT1001
       EXCEPTIONS
            NO_DATA      = 1
            ERROR_MODE   = 2
            ERROR_OBJECT = 3
            ERROR_VALUE  = 4
            OTHERS       = 5.

  IF SY-SUBRC = 0.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_MODEL' .
    WA_MODEL   = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_MI'    .
    WA_MI      = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_OCN'   .
    WA_OCN     = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_TRIM_PLANT_NO'.
    WA_TRIM_PLANT_NO =
                 IT_RESULT1001-ATWRT.
    CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_WO_CREATE_DATE'.
    WA_WOC_DATE = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_WO_CREATE_DATE'.
    WA_WOM_DATE = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
*    READ TABLE it_result1001 WITH KEY atnam = 'P_LC_NO' .
*    wa_lc_no   = it_result1001-atwrt. CLEAR: it_result1001.
    SELECT SINGLE AU~ATWRT
      INTO WA_LC_NO
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE AU~KLART = '001'     AND
            AU~OBJEK = L_HEADER  AND
            CA~ATNAM = 'P_LC_NO'   .

    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_DESTINATION_CODE'.
    WA_DESTINATION_CODE
               = IT_RESULT1001-ATWRT.
    CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_PERF_YN' .
    WA_PERF_YN = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
*    READ TABLE it_result1001 WITH KEY atnam = 'P_VIN_SPEC'.
*    wa_vin_spec =
*                 it_result1001-atwrt.
    SELECT SINGLE AU~ATWRT
      INTO WA_VIN_SPEC
      FROM AUSP AS AU
        INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
      WHERE AU~KLART = '001'        AND
            AU~OBJEK = L_HEADER     AND
            CA~ATNAM = 'P_VIN_SPEC'   .

    CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_INIT_QTY' .
    WA_INIT_QTY = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = 'P_MOD_QTY' .
    WA_MOD_QTY = IT_RESULT1001-ATWRT. CLEAR: IT_RESULT1001.
  ENDIF.
  DATA: L_FNAME(50)        TYPE C,
        L_NO(3)            TYPE N,
        L_ATNAM            TYPE CABN-ATNAM,
        L_INT              TYPE I,
        L_CHAR(3)          TYPE C VALUE '0'.

  DATA: L_CARX TYPE ZTBM_ABXOPVDT-CARX .
  DATA: L_COL(3)  TYPE  N.
  L_CARX = WA_MODEL+00(02).

  CLEAR: IT_219, IT_219[].
* 219 option value update
  DO 219 TIMES.
    CLEAR IT_219.
    L_INT = L_INT + 1.
    WRITE L_INT TO L_CHAR LEFT-JUSTIFIED .
    CONCATENATE 'P_219_' L_CHAR
      INTO L_ATNAM.
    CLEAR IT_RESULT1001.
    READ TABLE IT_RESULT1001 WITH KEY ATNAM = L_ATNAM.
    MOVE: L_CHAR              TO L_COL         ,
          L_CHAR              TO IT_219-NO     ,
          IT_RESULT1001-ATWRT TO IT_219-219VALS.
    CLEAR: IT_219-219CODE, IT_219-219DESC.
    PERFORM OPTION_219_VALUE USING     L_CARX
                                       L_COL
                                       IT_219-219VALS
                             CHANGING  IT_219-219CODE
                                       IT_219-219DESC.
    APPEND IT_219.

  ENDDO.


ENDFORM.                    " display_app207
*&---------------------------------------------------------------------*
*&      Form  set_car_name
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WO_PACK  text
*----------------------------------------------------------------------*
FORM SET_CAR_NAME USING    P_WO.
  DATA: L_FSC                LIKE ZTPP_WOSUM-FSC.
  DATA: L_WO_SER(10),
        L_NATION(04),
        L_DEALER(03).
  CONCATENATE P_WO+00(09) '%'
    INTO L_WO_SER.
  CONCATENATE P_WO+09(03) '%'
    INTO L_NATION.
  CONCATENATE P_WO+12(02) '%'
    INTO L_DEALER.


  SELECT SINGLE FSC INTO L_FSC
    FROM ZTPP_WOSUM
   WHERE WO_SER LIKE L_WO_SER AND
         NATION LIKE L_NATION AND
         DEALER LIKE L_DEALER   .

  CHECK SY-SUBRC = 0.
  SELECT SINGLE MAKTX
         INTO WA_CAR
         FROM MAKT
         WHERE MATNR EQ L_FSC
           AND SPRAS EQ SY-LANGU.

ENDFORM.                    " set_car_name

*&---------------------------------------------------------------------*
*&      Form  CLEAR_SCREEN_VALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_SCREEN_VALS.
  CLEAR: IT_0109,   IT_6299,   IT_ERR,
         IT_0109[], IT_6299[], IT_ERR[].
  CLEAR: WA_1003_VAL01,  WA_1003_VAL03,  WA_1003_VAL02,  WA_1003_VAL04,
         WA_1003_VAL05,  WA_1003_VAL07,  WA_1003_VAL06,  WA_1003_VAL08,
         WA_1003_VAL09,  WA_1003_VAL11,  WA_1003_VAL10,  WA_1003_VAL12,
         WA_1003_VAL13,  WA_1003_VAL15,  WA_1003_VAL14,  WA_1003_VAL16,
         WA_1003_VAL17,  WA_1003_VAL19,  WA_1003_VAL18,  WA_1003_VAL20,
         WA_1003_VAL21,  WA_1003_VAL23,  WA_1003_VAL22,  WA_1003_VAL24,
         WA_1003_VAL25,  WA_1003_VAL27,  WA_1003_VAL26,  WA_1003_VAL28,
         WA_1003_VAL29,  WA_1003_VAL31,  WA_1003_VAL30,  WA_1003_VAL32,
         WA_1003_VAL33,  WA_1003_VAL35,  WA_1003_VAL34,  WA_1003_VAL36,
         WA_1003_VAL37,  WA_1003_VAL39,  WA_1003_VAL38,  WA_1003_VAL40,
         WA_1003_VAL41,  WA_1003_VAL43,  WA_1003_VAL42,  WA_1003_VAL44,
         WA_1003_VAL45,  WA_1003_VAL47,  WA_1003_VAL46,  WA_1003_VAL48,
         WA_1003_VAL49,                  WA_1003_VAL50.
  CLEAR: WA_1004_COD05,  WA_1004_CHK01,  WA_1004_COD01,
         WA_1004_VAL01,  WA_1004_CHK02,  WA_1004_COD02,  WA_1004_VAL02,
         WA_1004_CHK03,  WA_1004_COD03,  WA_1004_VAL03,  WA_1004_CHK04,
         WA_1004_COD04,  WA_1004_VAL04,  WA_1004_CHK05,  WA_1004_VAL05,
         WA_1004_CHK06,  WA_1004_COD06,  WA_1004_VAL06,  WA_1004_CHK07,
         WA_1004_COD07,  WA_1004_VAL07,  WA_1004_CHK08,  WA_1004_COD08,
         WA_1004_VAL08,  WA_1004_CHK09,  WA_1004_COD09,  WA_1004_VAL09,
         WA_1004_CHK10,  WA_1004_COD10,  WA_1004_VAL10,  WA_1004_CHK11,
         WA_1004_COD11,  WA_1004_VAL11,  WA_1004_CHK12,  WA_1004_COD12,
         WA_1004_VAL12,  WA_1004_CHK13,  WA_1004_COD13,  WA_1004_VAL13,
         WA_1004_CHK14,  WA_1004_COD14,  WA_1004_VAL14,  WA_1004_CHK15,
         WA_1004_COD15,  WA_1004_VAL15,  WA_1004_CHK16,  WA_1004_COD16,
         WA_1004_VAL16,  WA_1004_CHK17,  WA_1004_COD17,  WA_1004_VAL17,
         WA_1004_CHK18,  WA_1004_COD18,  WA_1004_VAL18,  WA_1004_CHK19,
         WA_1004_COD19,  WA_1004_VAL19,  WA_1004_CHK20,  WA_1004_COD20,
         WA_1004_VAL20,  WA_1004_CHK21,  WA_1004_COD21,  WA_1004_VAL21,
         WA_1004_CHK22,  WA_1004_COD22,  WA_1004_VAL22,  WA_1004_CHK23,
         WA_1004_COD23,  WA_1004_VAL23,  WA_1004_CHK24,  WA_1004_COD24,
         WA_1004_VAL24,  WA_1004_CHK25,  WA_1004_COD25,  WA_1004_VAL25,
         WA_1004_CHK26,  WA_1004_COD26,  WA_1004_VAL26,  WA_1004_CHK27,
         WA_1004_COD27,  WA_1004_VAL27,  WA_1004_CHK28,  WA_1004_COD28,
         WA_1004_VAL28,  WA_1004_CHK29,  WA_1004_COD29,  WA_1004_VAL29,
         WA_1004_CHK30,  WA_1004_COD30,  WA_1004_VAL30,  WA_1004_CHK31,
         WA_1004_COD31,  WA_1004_VAL31,  WA_1004_CHK32,  WA_1004_COD32,
         WA_1004_VAL32,  WA_1004_CHK33,  WA_1004_COD33,  WA_1004_VAL33,
         WA_1004_CHK34,  WA_1004_COD34,  WA_1004_VAL34,  WA_1004_CHK35,
         WA_1004_COD35,  WA_1004_VAL35,  WA_1004_CHK36,  WA_1004_COD36,
         WA_1004_VAL36,  WA_1004_CHK37,  WA_1004_COD37,  WA_1004_VAL37,
         WA_1004_CHK38,  WA_1004_COD38,  WA_1004_VAL38,  WA_1004_CHK39,
         WA_1004_COD39,  WA_1004_VAL39,  WA_1004_CHK40,  WA_1004_COD40,
         WA_1004_VAL40,  WA_1004_CHK41,  WA_1004_COD41,  WA_1004_VAL41,
         WA_1004_CHK42,  WA_1004_COD42,  WA_1004_VAL42,  WA_1004_CHK43,
         WA_1004_COD43,  WA_1004_VAL43,  WA_1004_CHK44,  WA_1004_COD44,
         WA_1004_VAL44,  WA_1004_CHK45,  WA_1004_COD45,  WA_1004_VAL45,
         WA_1004_CHK46,  WA_1004_COD46,  WA_1004_VAL46,  WA_1004_CHK47,
         WA_1004_COD47,  WA_1004_VAL47,  WA_1004_CHK48,  WA_1004_COD48,
         WA_1004_VAL48,  WA_1004_CHK49,  WA_1004_COD49,  WA_1004_VAL49,
         WA_1004_CHK50,  WA_1004_COD50,  WA_1004_VAL50,
         WA_1004_E01,  WA_1004_E02,  WA_1004_E03,  WA_1004_E04,
         WA_1004_E05,  WA_1004_E06,  WA_1004_E07,  WA_1004_E08,
         WA_1004_E09,  WA_1004_E10,  WA_1004_E11,  WA_1004_E12,
         WA_1004_E13,  WA_1004_E14,  WA_1004_E15,  WA_1004_E16,
         WA_1004_E17,  WA_1004_E18,  WA_1004_E19,  WA_1004_E20,
         WA_1004_I01,  WA_1004_I02,  WA_1004_I03,  WA_1004_I04,
         WA_1004_I05,  WA_1004_I06,  WA_1004_I07,  WA_1004_I08,
         WA_1004_I09,  WA_1004_I10,  WA_1004_I11,  WA_1004_I12,
         WA_1004_I13,  WA_1004_I14,  WA_1004_I15,  WA_1004_I16,
         WA_1004_I17,  WA_1004_I18,  WA_1004_I19,  WA_1004_I20,
         WA_1004_219I01, WA_1004_219I02, WA_1004_219I03, WA_1004_219I04,
         WA_1004_219I05, WA_1004_219I06, WA_1004_219I07, WA_1004_219I08,
         WA_1004_219I09, WA_1004_219I10, WA_1004_219I11, WA_1004_219I12,
         WA_1004_219I13, WA_1004_219I14, WA_1004_219I15, WA_1004_219I16,
         WA_1004_219I17, WA_1004_219I18, WA_1004_219I19, WA_1004_219I20,
         WA_1004_219O01, WA_1004_219O02, WA_1004_219O03, WA_1004_219O04,
         WA_1004_219O05, WA_1004_219O06, WA_1004_219O07, WA_1004_219O08,
         WA_1004_219O09, WA_1004_219O10, WA_1004_219O11, WA_1004_219O12,
         WA_1004_219O13, WA_1004_219O14, WA_1004_219O15, WA_1004_219O16,
         WA_1004_219O17, WA_1004_219O18, WA_1004_219O19, WA_1004_219O20.
  CLEAR: ST_0109_INPUT.
  CLEAR: ST_0110_INPUT,  IT_0110,  IT_0110[].
  CLEAR: ST_0111_INPUT,  IT_0111,  IT_0111[],  IT_0111_C,  IT_0111_C[].
  CLEAR: ST_0118_INPUT,  IT_0118,  IT_0118[].
  CLEAR: IT_OPT1_APP219, IT_OPT1_APP219[]   .
  CLEAR: IS219,          IT_APP220,            IT_APP220[].
  CLEAR: P_KEY_01,       P_KEY_02,       P_KEY_03,       P_KEY_04,
         P_KEY_05,       P_KEY_06,       P_KEY_07,       P_KEY_08,
         P_KEY_09,       P_KEY_10,       P_KEY_11,       P_KEY_12,
         P_KEY_13,       P_KEY_14,       P_KEY_15,       P_KEY_16,
         P_KEY_17,       P_KEY_18,       P_KEY_19,       P_KEY_20,
         P_PART,         P_KEY,          P_FULL_CODE,    P_COL_NAME,
         IT_APP223,      IT_APP223[].
  CLEAR: ST_2101,        IT_2101,        IT_2101[].
  CLEAR: ST_2102,        IT_2102,        IT_2102[].
  CLEAR: IT_2103,        IT_2103[].
  CLEAR: P_PART_APP240,  P_COLUMN_APP240.
  CLEAR: ST_KEY_APP236,  IT_UPART_APP236,    ST_ISS_APP236,
         IT_219_APP236,  IT_219_APP236[],    IT_UPART_APP236[],
         IT_ABAG_APP236, IT_ABAG_APP236[],   IT_CPART_APP236[],
         ST_APP236,      IT_CPART_APP236,    IT_RP_APP236,
         IT_RP_APP236[].
  CLEAR: ZSPP_APP237,    ST_APP237,          IT_DLS_2107, IT_DLS_2107[].
  CLEAR: IT_APP239,      P_ORDERNO_APP239,   P_BODYSER_APP239,
         IT_APP239[],    P_EXT_COLOR_APP239, P_INT_COLOR_APP239.
  CLEAR: P_PROG_APP244,  P_WONO_APP244,      P_PROD_DATE_APP244,
         P_EXTC_APP244,  P_INTC_APP244,      P_TOTAL_APP244,
         IT_APP244,      IT_APP244[].
  CLEAR: P_LINE_APP245,  P_SHOP_DATE_APP245, P_END_DATE_APP245,
         P_PROG_APP245,  P_TYPE_APP245,      P_COLOR_APP245,
         P_WONO_APP245,  P_EXTC_APP245,      P_INTC_APP245,
         IT_APP245,      IT_APP245[].
  CLEAR: IT_SUM_APP246,  IT_SUM_APP246[],    IT_DET_APP246[],
         IT_DET_APP246,  P_LINE_APP246,      P_PROG_APP246,
         P_WONO_APP246,  P_EXTC_APP246,      P_INTC_APP246,
         P_TOTAL_APP246, P_BODYNO_APP246,    P_STATUS_APP246.
  CLEAR: IT_APP272_01,   IT_APP272_01[],     ZTBM_ABXPLIDT-GUBN,
         ZTBM_ABXPLIDT-HPCC.
  CLEAR: ST_3107_INPUT,  IT_3107,            IT_3107[].
  CLEAR: IT_APP250,      IT_APP250[].
  CLEAR: IT_APP252,      IT_APP252[].
  CLEAR: ST_3109_INPUT,  IT_3109,            IT_3109[].
  CLEAR: ST_4279_INPUT,  IT_4279,            IT_4279[].
  CLEAR: ST_5290_INPUT,  IT_5290,            IT_5290[].
  CLEAR: ST_APP302,      IT_APP302,          IT_APP302[].
ENDFORM.                    " CLEAR_SCREEN_VALS

*&---------------------------------------------------------------------*
*&      Form  read_char_app218
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_HEADER_MATNR  text
*      -->P_9039   text
*      -->P_IT_0118_MODEL  text
*----------------------------------------------------------------------*
FORM READ_CHAR_APP218 USING    P_HEADER
                               P_ATNAM
                               P_ATWRT.
  SELECT SINGLE AU~ATWRT
    INTO P_ATWRT
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~OBJEK = P_HEADER AND
          AU~KLART = '001'    AND
          CA~ATNAM = P_ATNAM    .

ENDFORM.                    " read_char_app218
*&---------------------------------------------------------------------*
*&      Form  read_num_app218
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_HEADER_MATNR  text
*      -->P_9073   text
*      -->P_IT_0118_INIT  text
*----------------------------------------------------------------------*
FORM READ_NUM_APP218 USING    P_HEADER
                              P_ATNAM
                              P_NUM.
  DATA: L_ATFLV   TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N.
  SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
      INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~OBJEK = P_HEADER AND
          AU~KLART = '001'    AND
          CA~ATNAM = P_ATNAM    .
  L_NUM = L_ATFLV.
  CONDENSE L_NUM.
  P_NUM = L_NUM.

ENDFORM.                    " read_num_app218
*&---------------------------------------------------------------------*
*&      Form  clear_parameters_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_TABLES_APP211.
  CLEAR: IT_0111, IT_0111[],
         IT_0111_C, IT_0111_C[],
         ST_0111_INPUT-MI1,
         ST_0111_INPUT-MI2,
         ST_0111_INPUT-OCN1,
         ST_0111_INPUT-OCN2,
         ST_0111_INPUT-VER1,
         ST_0111_INPUT-VER2.

ENDFORM.                    " clear_parameters_app211
*&---------------------------------------------------------------------*
*&      Form  search_two_wo_inf_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_TWO_WO_INF_APP211.
  DATA: L_TYPE .
  IF ST_0111_INPUT-ORDER1 = SPACE  .
    MESSAGE S000 WITH 'Set Parameter - W/O!!'.
    EXIT.
  ENDIF.
* Search Parameters' Description.
  "The First W/O
  IF ST_0111_INPUT-EXCLR1 = SPACE.
    L_TYPE = 'H'.  " W/O Header
  ELSE.
    L_TYPE = 'C'.  " W/O Color
  ENDIF.
  PERFORM SEARCH_DESCRIPTION_APP211 USING ST_0111_INPUT-ORDER1
                                          ST_0111_INPUT-EXCLR1
                                          ST_0111_INPUT-INCLR1
                                          L_TYPE
                                    CHANGING ST_0111_INPUT-MI1
                                             ST_0111_INPUT-OCN1
                                             ST_0111_INPUT-VER1.
  "The Second W/O
  IF ST_0111_INPUT-EXCLR2 = SPACE.
    L_TYPE = 'H'.  " W/O Header
  ELSE.
    L_TYPE = 'C'.  " W/O Color
  ENDIF.
  PERFORM SEARCH_DESCRIPTION_APP211 USING ST_0111_INPUT-ORDER2
                                          ST_0111_INPUT-EXCLR2
                                          ST_0111_INPUT-INCLR2
                                          L_TYPE
                                    CHANGING ST_0111_INPUT-MI2
                                             ST_0111_INPUT-OCN2
                                             ST_0111_INPUT-VER2.
* Search W/O's ALC Code
  CLEAR: IT_0111, IT_0111[].
  PERFORM SEARCH_ALC_CODE_APP211 TABLES IT_0111
                                 USING  ST_0111_INPUT-ORDER1
                                        ST_0111_INPUT-EXCLR1
                                        ST_0111_INPUT-INCLR1
                                        ST_0111_INPUT-ORDER2
                                        ST_0111_INPUT-EXCLR2
                                        ST_0111_INPUT-INCLR2.
* Search W/O's 219 Code
  CLEAR: IT_0111_C, IT_0111_C[].
  PERFORM SEARCH_219_CODE_APP211 TABLES IT_0111_C
                                 USING  ST_0111_INPUT-ORDER1
                                        ST_0111_INPUT-EXCLR1
                                        ST_0111_INPUT-INCLR1
                                        ST_0111_INPUT-ORDER2
                                        ST_0111_INPUT-EXCLR2
                                        ST_0111_INPUT-INCLR2.
ENDFORM.                    " search_two_wo_inf_app211

*&---------------------------------------------------------------------*
*&      Form  read_wosum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_WOSUM.
  CLEAR: WA_WOSUM, WA_WOSUM[].
  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE WA_WOSUM
    FROM ZTPP_WOSUM.
  SORT WA_WOSUM BY WO_SER NATION DEALER EXTC INTC .
ENDFORM.                    " read_wosum
*&---------------------------------------------------------------------*
*&      Form  search_description_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_0111_INPUT_ORDER1  text
*      -->P_ST_0111_INPUT_EXCLR1  text
*      -->P_ST_0111_INPUT_INCLR1  text
*      <--P_ST_0111_INPUT_MI1  text
*      <--P_ST_0111_INPUT_OCN1  text
*      <--P_ST_0111_INPUT_VER1  text
*----------------------------------------------------------------------*
FORM SEARCH_DESCRIPTION_APP211 USING  P_HEADER  P_EXTC  P_INTC  P_TYPE
                            CHANGING  P_MI      P_OCN   P_VER.
  DATA: L_MATNR    TYPE MARA-MATNR,
        L_ATWRT    TYPE AUSP-ATWRT,
        L_TEXT(40) TYPE C         .

  CHECK P_HEADER NE SPACE.
  IF P_TYPE = 'H'.
    CLEAR L_ATWRT.
    PERFORM READ_HEADER_CHAR_INF  USING    P_HEADER
                                           'P_WO_SER'
                                  CHANGING L_ATWRT .
    IF L_ATWRT = SPACE.
      CONCATENATE 'No Data -' P_HEADER '!!'
        INTO L_TEXT .
      MESSAGE S000 WITH L_TEXT .
      EXIT.
    ENDIF.
*   W/O Header Information
    PERFORM READ_HEADER_CHAR_INF  USING    P_HEADER
                                           'P_MI'
                                  CHANGING P_MI .
    PERFORM READ_HEADER_CHAR_INF  USING    P_HEADER
                                           'P_OCN'
                                  CHANGING P_OCN .
    PERFORM READ_HEADER_CHAR_INF  USING    P_HEADER
                                           'P_VERSION'
                                  CHANGING P_VER.
  ELSE.
    CLEAR L_ATWRT.
    CONCATENATE P_HEADER P_EXTC P_INTC
      INTO L_MATNR .
    PERFORM READ_COLOR_CHAR_INF  USING    L_MATNR
                                          'P_WO_SER'
                                 CHANGING L_ATWRT .
    IF L_ATWRT = SPACE.
      CONCATENATE 'No Data -' L_MATNR '!!'
        INTO L_TEXT .
      MESSAGE S000 WITH L_TEXT .
    ENDIF.
*   W/O Color Information
    PERFORM READ_COLOR_CHAR_INF  USING    L_MATNR
                                          'P_MI'
                                 CHANGING P_MI  .
    PERFORM READ_COLOR_CHAR_INF  USING    L_MATNR
                                          'P_OCN'
                                 CHANGING P_OCN  .
    PERFORM READ_COLOR_CHAR_INF  USING    L_MATNR
                                          'P_VERSION'
                                 CHANGING P_VER  .


  ENDIF.
ENDFORM.                    " search_description_app211
*&---------------------------------------------------------------------*
*&      Form  read_header_char_inf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_HEADER  text
*      -->P_4475   text
*      <--P_P_MI  text
*----------------------------------------------------------------------*
FORM READ_HEADER_CHAR_INF USING    P_MATNR
                                   P_ATNAM
                          CHANGING P_ATWRT.
  SELECT SINGLE AU~ATWRT
    INTO P_ATWRT
    FROM ( ( MARA AS MA
         INNER JOIN AUSP AS AU ON MA~MATNR = AU~OBJEK )
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
   WHERE MA~MTART = 'WOHD'              AND
         MA~MBRSH = 'A'                 AND
         MA~KZKFG = SPACE               AND
         CA~ATNAM = P_ATNAM             AND
         MA~MATNR = P_MATNR               .

ENDFORM.                    " read_header_char_inf
*&---------------------------------------------------------------------*
*&      Form  read_color_char_inf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_MATNR  text
*      -->P_4519   text
*      <--P_P_MI  text
*----------------------------------------------------------------------*
FORM READ_COLOR_CHAR_INF USING    P_MATNR
                                  P_ATNAM
                         CHANGING P_ATWRT.
  SELECT SINGLE AU~ATWRT
    INTO P_ATWRT
    FROM ( ( MARA AS MA
         INNER JOIN AUSP AS AU ON MA~MATNR = AU~OBJEK )
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
   WHERE MA~MTART = 'WOCL'              AND
         MA~MBRSH = 'A'                 AND
         MA~KZKFG = SPACE               AND
         CA~ATNAM = P_ATNAM             AND
         MA~MATNR = P_MATNR               .

ENDFORM.                    " read_color_char_inf
*&---------------------------------------------------------------------*
*&      Form  search_alc_code_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_0111  text
*      -->P_ST_0111_INPUT_ORDER1  text
*      -->P_ST_0111_INPUT_EXCLR1  text
*      -->P_ST_0111_INPUT_INCLR1  text
*      -->P_ST_0111_INPUT_ORDER2  text
*      -->P_ST_0111_INPUT_EXCLR2  text
*      -->P_ST_0111_INPUT_INCLR2  text
*----------------------------------------------------------------------*
FORM SEARCH_ALC_CODE_APP211 TABLES   PT_ALC STRUCTURE IT_0111
                            USING    P_HEADER_1
                                     P_EXTC_1
                                     P_INTC_1

                                     P_HEADER_2
                                     P_EXTC_2
                                     P_INTC_2.
  DATA: L_NUM(03)   TYPE C,
        L_ATNAM     TYPE CABN-ATNAM,
        L_ATWRT     TYPE AUSP-ATWRT,
        LS_ALC      LIKE IT_0111   ,
        L_COLOR     TYPE AUSP-OBJEK.
********************************************
* Append ALC Code Table with The First W/O.
********************************************
  IF P_EXTC_1 = SPACE.
*   W/O Header
    L_NUM = 0.
    DO 200 TIMES.
      CLEAR PT_ALC.
      PT_ALC-CLM = L_NUM = L_NUM + 1.
*     CLM
      CONDENSE PT_ALC-CLM.
*     CNAME
      CONCATENATE 'ALC U' L_NUM
        INTO PT_ALC-CNAME.
*     CODE1
      CONCATENATE 'P_ALC_U_' PT_ALC-CLM
        INTO L_ATNAM.
      PERFORM READ_HEADER_CHAR_INF  USING   P_HEADER_1
                                            L_ATNAM
                                   CHANGING PT_ALC-CODE1.
      APPEND PT_ALC.
    ENDDO.

  ELSE.
*   W/O Color
    L_NUM = 0.
    DO 50 TIMES.
      CLEAR PT_ALC.
      PT_ALC-CLM = L_NUM = L_NUM + 1.
*     CLM
      CONDENSE PT_ALC-CLM.
*     CNAME
      CONCATENATE 'ALC C' L_NUM
        INTO PT_ALC-CNAME.
*     CODE1
      CONCATENATE 'P_ALC_C_' PT_ALC-CLM
        INTO L_ATNAM.
      CONCATENATE P_HEADER_1 P_EXTC_1 P_INTC_1
        INTO L_COLOR.
      PERFORM READ_COLOR_CHAR_INF  USING    L_COLOR
                                            L_ATNAM
                                   CHANGING PT_ALC-CODE1.
      APPEND PT_ALC.

    ENDDO.
  ENDIF.

******************************************************
* Modify or Append ALC Code Table with The Second W/O.
******************************************************
  IF P_EXTC_2 = SPACE.
*   W/O Header
    L_NUM = 0.
    DO 200 TIMES.
      CLEAR LS_ALC.
      LS_ALC-CLM = L_NUM = L_NUM + 1.
*     CLM
      CONDENSE LS_ALC-CLM.
*     CNAME
      CONCATENATE 'ALC U' L_NUM
        INTO LS_ALC-CNAME.
*     CODE2
      CONCATENATE 'P_ALC_U_' LS_ALC-CLM
        INTO L_ATNAM.
      PERFORM READ_HEADER_CHAR_INF  USING   P_HEADER_2
                                            L_ATNAM
                                   CHANGING LS_ALC-CODE2.
      READ TABLE PT_ALC WITH KEY CNAME = LS_ALC-CNAME.
      IF SY-SUBRC = 0.
        MOVE LS_ALC-CODE2 TO PT_ALC-CODE2.
        MODIFY PT_ALC INDEX SY-TABIX.
      ELSE.
        APPEND LS_ALC TO PT_ALC.
      ENDIF.
    ENDDO.

  ELSE.
*   W/O Color
    L_NUM = 0.
    DO 50 TIMES.
      CLEAR LS_ALC.
      LS_ALC-CLM = L_NUM = L_NUM + 1.
*     CLM
      CONDENSE LS_ALC-CLM.
*     CNAME
      CONCATENATE 'ALC C' L_NUM
        INTO LS_ALC-CNAME.
*     CODE2
      CONCATENATE 'P_ALC_C_' LS_ALC-CLM
        INTO L_ATNAM.
** Change by furong on 02/12/09
*      CONCATENATE P_HEADER_2 P_EXTC_1 P_INTC_1
*        INTO L_COLOR .
      CONCATENATE P_HEADER_2 P_EXTC_2 P_INTC_2
        INTO L_COLOR .
** End of change on 02/12/09
      PERFORM READ_COLOR_CHAR_INF  USING    L_COLOR
                                            L_ATNAM
                                   CHANGING LS_ALC-CODE2.
      READ TABLE PT_ALC WITH KEY CNAME = LS_ALC-CNAME.
      IF SY-SUBRC = 0.
        MOVE LS_ALC-CODE2 TO PT_ALC-CODE2.
        MODIFY PT_ALC INDEX SY-TABIX.
      ELSE.
        APPEND LS_ALC TO PT_ALC.
      ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.                    " search_alc_code_app211
*&---------------------------------------------------------------------*
*&      Form  search_219_code_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_0111_C  text
*      -->P_ST_0111_INPUT_ORDER1  text
*      -->P_ST_0111_INPUT_EXCLR1  text
*      -->P_ST_0111_INPUT_INCLR1  text
*      -->P_ST_0111_INPUT_ORDER2  text
*      -->P_ST_0111_INPUT_EXCLR2  text
*      -->P_ST_0111_INPUT_INCLR2  text
*----------------------------------------------------------------------*
FORM SEARCH_219_CODE_APP211 TABLES   PT_219 STRUCTURE IT_0111_C
                            USING    P_HEADER_1
                                     P_EXTC_1
                                     P_INTC_1

                                     P_HEADER_2
                                     P_EXTC_2
                                     P_INTC_2.
  DATA: L_NUM(03)   TYPE C,
        L_COL(03)   TYPE N,
        L_ATNAM     TYPE CABN-ATNAM,
        L_ATWRT     TYPE AUSP-ATWRT,
        LS_219      LIKE IT_0111_C ,
        L_COLOR     TYPE AUSP-OBJEK,
        L_MODEL     TYPE AUSP-ATWRT.

********************************************
* Append 219 Code Table with The First W/O.
********************************************
  L_NUM = 0.
  DO 219 TIMES.
    CLEAR PT_219.
    PT_219-CLM = L_NUM = L_NUM + 1.
*   CLM
    CONDENSE PT_219-CLM.
*   CODE1
    CONCATENATE 'P_219_' PT_219-CLM
      INTO L_ATNAM.
    IF P_EXTC_1 = SPACE.
*     W/O Header
      PERFORM READ_HEADER_CHAR_INF  USING   P_HEADER_1
                                            L_ATNAM
                                   CHANGING PT_219-CODE1.
      PERFORM READ_HEADER_CHAR_INF  USING   P_HEADER_1
                                            'P_MODEL'
                                   CHANGING L_MODEL .
    ELSE.
*     W/O Color
      CONCATENATE P_HEADER_1 P_EXTC_1 P_INTC_1
        INTO L_COLOR.
      PERFORM READ_COLOR_CHAR_INF USING    L_COLOR
                                           L_ATNAM
                                  CHANGING PT_219-CODE1.
      PERFORM READ_COLOR_CHAR_INF  USING   L_COLOR
                                           'P_MODEL'
                                  CHANGING L_MODEL .
    ENDIF.
*   CNAME
    MOVE PT_219-CLM TO L_COL.
    PERFORM READ_COLUMN_DESC_APP211 USING L_MODEL+00(02)
                                          L_COL
                                    CHANGING PT_219-CNAME .
    APPEND PT_219.
  ENDDO.

*******************************************************
* Modify or Append 219 Code Table with The Second W/O.
*******************************************************
  L_NUM = 0.
  DO 219 TIMES.
    CLEAR LS_219.
    LS_219-CLM = L_NUM = L_NUM + 1.
*   CLM
    CONDENSE LS_219-CLM.
    CONCATENATE 'P_219_' LS_219-CLM
      INTO L_ATNAM.
    IF P_EXTC_2 = SPACE.
*     W/O Header
*     CODE2
      PERFORM READ_HEADER_CHAR_INF  USING   P_HEADER_2
                                            L_ATNAM
                                   CHANGING LS_219-CODE2.
      PERFORM READ_HEADER_CHAR_INF  USING   P_HEADER_2
                                            'P_MODEL'
                                   CHANGING L_MODEL .
    ELSE.
*     W/O Color
*     CODE2
      CONCATENATE P_HEADER_2 P_EXTC_2 P_INTC_2
        INTO L_COLOR.
      PERFORM READ_COLOR_CHAR_INF USING    L_COLOR
                                           L_ATNAM
                                  CHANGING LS_219-CODE2.
      PERFORM READ_COLOR_CHAR_INF  USING   L_COLOR
                                           'P_MODEL'
                                  CHANGING L_MODEL .
    ENDIF.
*   CNAME
    MOVE LS_219-CLM TO L_COL.
    PERFORM READ_COLUMN_DESC_APP211 USING L_MODEL+00(02)
                                          L_COL
                                    CHANGING LS_219-CNAME .
    READ TABLE PT_219 WITH KEY CLM = LS_219-CLM.
    IF SY-SUBRC = 0.
      MOVE LS_219-CODE2 TO PT_219-CODE2.
      MODIFY PT_219 INDEX SY-TABIX.
    ELSE.
      APPEND LS_219 TO PT_219.
    ENDIF.

  ENDDO.

ENDFORM.                    " search_219_code_app211
*&---------------------------------------------------------------------*
*&      Form  read_column_desc_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_0118_INPUT_MODEL+00(02)  text
*      -->P_PT_219_CLM  text
*      -->P_PT_219_CODE1  text
*      <--P_PT_219_CNAME  text
*----------------------------------------------------------------------*
FORM READ_COLUMN_DESC_APP211 USING    P_CARX
                                      P_CLNO
                             CHANGING P_CNAME.
  SELECT SINGLE VANM  "Column Desc
    INTO P_CNAME
    FROM ZTBM_ABXOPVDT AS AB
    WHERE AB~CARX  = P_CARX  AND  "Model
          AB~CLNO  = P_CLNO    .  "Column Value

ENDFORM.                    " read_column_desc_app211
*&---------------------------------------------------------------------*
*&      Form  delete_same_data_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DELETE_SAME_DATA_APP211.
  LOOP AT IT_0111.
    IF IT_0111-CODE1 = IT_0111-CODE2.
      DELETE IT_0111.
    ENDIF.
  ENDLOOP.
  LOOP AT IT_0111_C.
    IF IT_0111_C-CODE1 = IT_0111_C-CODE2.
      DELETE IT_0111_C.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " delete_same_data_app211
*&---------------------------------------------------------------------*
*&      Form  set_next_header_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_NEXT_HEADER_APP211.
  DATA: L_TOTAL  TYPE SY-TABIX,
        L_TABIX  TYPE SY-TABIX,
        L_WO_SER LIKE WA_WOSUM-WO_SER,
        L_NATION LIKE WA_WOSUM-NATION,
        L_DEALER LIKE WA_WOSUM-DEALER.

  DESCRIBE TABLE WA_WOSUM LINES L_TOTAL.
  READ TABLE WA_WOSUM WITH KEY WO_SER = ST_0111_INPUT-ORDER1+00(09)
                               NATION = ST_0111_INPUT-ORDER1+09(03)
                               DEALER = ST_0111_INPUT-ORDER1+12(02).
*  check sy-subrc = 0.
  MOVE: WA_WOSUM-WO_SER TO L_WO_SER,
        WA_WOSUM-NATION TO L_NATION,
        WA_WOSUM-DEALER TO L_DEALER.
  L_TABIX = SY-TABIX.
  DO.
    L_TABIX = L_TABIX + 1.
    READ TABLE WA_WOSUM INDEX L_TABIX.
    IF WA_WOSUM-WO_SER <> L_WO_SER OR
       WA_WOSUM-NATION <> L_NATION OR
       WA_WOSUM-DEALER <> L_DEALER  .
      CONCATENATE WA_WOSUM-WO_SER
                  WA_WOSUM-NATION
                  WA_WOSUM-DEALER
        INTO ST_0111_INPUT-ORDER1.
      CLEAR: ST_0111_INPUT-EXCLR1,
             ST_0111_INPUT-INCLR1.
      EXIT.
    ENDIF.
    IF L_TABIX = L_TOTAL.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " set_next_header_app211
*&---------------------------------------------------------------------*
*&      Form  set_next_color_app211
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_NEXT_COLOR_APP211.
  DATA: L_TOTAL  TYPE SY-TABIX,
        L_TABIX  TYPE SY-TABIX,
        L_WO_SER LIKE WA_WOSUM-WO_SER,
        L_NATION LIKE WA_WOSUM-NATION,
        L_DEALER LIKE WA_WOSUM-DEALER,
        L_EXTC   LIKE WA_WOSUM-EXTC,
        L_INTC   LIKE WA_WOSUM-INTC.

  DESCRIBE TABLE WA_WOSUM LINES L_TOTAL.
  READ TABLE WA_WOSUM WITH KEY WO_SER = ST_0111_INPUT-ORDER1+00(09)
                               NATION = ST_0111_INPUT-ORDER1+09(03)
                               DEALER = ST_0111_INPUT-ORDER1+12(02)
                               EXTC   = ST_0111_INPUT-EXCLR1
                               INTC   = ST_0111_INPUT-INCLR1 .
*  check sy-subrc = 0.
  MOVE: WA_WOSUM-WO_SER TO L_WO_SER,
        WA_WOSUM-NATION TO L_NATION,
        WA_WOSUM-DEALER TO L_DEALER,
        WA_WOSUM-EXTC   TO L_EXTC,
        WA_WOSUM-INTC   TO L_INTC.
  L_TABIX = SY-TABIX.
  DO.
    L_TABIX = L_TABIX + 1.
    READ TABLE WA_WOSUM INDEX L_TABIX.
    IF WA_WOSUM-WO_SER <> L_WO_SER OR
       WA_WOSUM-NATION <> L_NATION OR
       WA_WOSUM-DEALER <> L_DEALER OR
       WA_WOSUM-EXTC   <> L_EXTC   OR
       WA_WOSUM-INTC   <> L_INTC    .
      CONCATENATE WA_WOSUM-WO_SER
                  WA_WOSUM-NATION
                  WA_WOSUM-DEALER
        INTO ST_0111_INPUT-ORDER1.
      MOVE: WA_WOSUM-EXTC TO ST_0111_INPUT-EXCLR1,
            WA_WOSUM-INTC TO ST_0111_INPUT-INCLR1.
      EXIT.
    ENDIF.
    IF L_TABIX = L_TOTAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.                    " set_next_color_app211
*&---------------------------------------------------------------------*
*&      Form  clear_int_table_app220
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_INT_TABLE_APP220.
  CLEAR: IT_APP220, IT_APP220[].
ENDFORM.                    " clear_int_table_app220
*&---------------------------------------------------------------------*
*&      Form  search_pre_value
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_PRE_VALUE.
  DATA: L_MODEL TYPE ZTBM_ABXOPVDT-CARX,
        L_CLNO  TYPE ZTBM_ABXOPVDT-CLNO.
  MOVE IS219-MODEL TO L_MODEL .
  SELECT * FROM  ZTBM_ABXOPVDT
    INTO  CORRESPONDING  FIELDS OF TABLE IT_APP220
    WHERE CARX   EQ   L_MODEL
      AND CLNO   EQ   IS219-NAME219.
  IF SY-SUBRC <> 0.
    L_CLNO = IS219-NAME219.
    DO.
      L_CLNO = L_CLNO - 1.
      IF L_CLNO <= 0.
        EXIT.
      ENDIF.
      SELECT * FROM ZTBM_ABXOPVDT
        INTO CORRESPONDING FIELDS OF TABLE IT_APP220
        WHERE CARX EQ L_MODEL AND
              CLNO EQ L_CLNO    .
      IF SY-SUBRC = 0.
        IS219-NAME219 = L_CLNO.
        EXIT.
      ENDIF.

    ENDDO.
  ENDIF.

ENDFORM.                    " search_pre_value
*&---------------------------------------------------------------------*
*&      Form  clear_structure_app221
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CLEAR_STRUCTURE_APP221.
  CLEAR: IS_APP221-NAME,
         IS_APP221-DEALER,
         IS_APP221-ADDRESS1,
         IS_APP221-ADDRESS2,
         IS_APP221-ADDRESS3,
         IS_APP221-DRIVE,
         IS_APP221-WEATHER,
         IS_APP221-REGION,
         IS_APP221-SPEC,
         IS_APP221-PORT,
         IS_APP221-PORTNAME,
         IS_APP221-LANGU,
         IS_APP221-WARRANTY,
         IS_APP221-ANTI_RUST,
         IS_APP221-N_CODE.
ENDFORM.                    " clear_structure_app221

*&---------------------------------------------------------------------*
*&      Form  convert_219_text
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0752   text
*      -->P_IT_2101_BAGIGAS  text
*----------------------------------------------------------------------*
FORM CONVERT_219_TEXT USING    PA_219   PA_VALUE .
* entry conversion.
* Engine & Carbrator
  SELECT SINGLE AB~CLNM  INTO PA_VALUE
    FROM ZTBM_ABXOPVDT AS AB
   WHERE AB~CARX  = WA_MODEL(2)
     AND AB~CLNO  = PA_219
     AND AB~VALU  = PA_VALUE .
ENDFORM.                    " convert_219_text

*&---------------------------------------------------------------------*
*&      Form  read_car_desc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ST_APP236_WON  text
*      <--P_ST_ISS_APP236_EQKTX  text
*----------------------------------------------------------------------*
FORM READ_CAR_DESC USING    P_HEADER
                   CHANGING P_DESC  .
  DATA: L_FSC                LIKE ZTPP_WOSUM-FSC.

  SELECT SINGLE FSC INTO L_FSC
    FROM ZTPP_WOSUM
   WHERE WO_SER = P_HEADER(9)
     AND NATION = P_HEADER+9(3)
     AND DEALER = P_HEADER+12(2).

  CHECK SY-SUBRC = 0.
  SELECT SINGLE MAKTX
         INTO P_DESC
         FROM MAKT
         WHERE MATNR EQ L_FSC
           AND SPRAS EQ SY-LANGU.
ENDFORM.                    " read_car_desc

*&---------------------------------------------------------------------*
*&      Form  read_vm_char_NUM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK  text
*      -->P_4228   text
*      <--P_IT_COLL_DEST  text
*----------------------------------------------------------------------*
FORM READ_VM_CHAR_NUM USING    P_OBJEK
                               P_ATNAM
                      CHANGING P_ATFLV.
  SELECT SINGLE AU~ATFLV
    INTO P_ATFLV
    FROM ( AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
    WHERE AU~KLART = '002'   AND
          AU~OBJEK = P_OBJEK AND
          CA~ATNAM = P_ATNAM   .
ENDFORM.                    " read_vm_char_NUM

*&---------------------------------------------------------------------*
*&      Form  read_vm_char_inf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_OBJEK  text
*      -->P_4228   text
*      <--P_IT_COLL_DEST  text
*----------------------------------------------------------------------*
FORM READ_VM_CHAR_INF USING    P_OBJEK
                               P_ATNAM
                      CHANGING P_ATWRT.
  SELECT SINGLE AU~ATWRT
    INTO P_ATWRT
    FROM ( AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
    WHERE AU~KLART = '002'   AND
          AU~OBJEK = P_OBJEK AND
          CA~ATNAM = P_ATNAM   .
ENDFORM.                    " read_vm_char_inf

*&---------------------------------------------------------------------*
*&      Form  read_car_desc_by_model
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MODEL  text
*      <--P_IT_2102_DESCR  text
*----------------------------------------------------------------------*
FORM READ_CAR_DESC_BY_MODEL USING    P_MODEL
                            CHANGING P_ATWTB .
  DATA: L_ATINN TYPE AUSP-ATINN.

  SELECT SINGLE ATINN INTO L_ATINN
    FROM CABN
   WHERE ATNAM = 'P_MODEL'.

  SELECT SINGLE T~ATWTB
    INTO P_ATWTB
    FROM ( CAWN AS N
         INNER JOIN CAWNT AS T ON N~ATINN = T~ATINN AND
                                  N~ATZHL = T~ATZHL     )
   WHERE N~ATINN = L_ATINN
     AND N~ATWRT = P_MODEL
     AND T~SPRAS = SY-LANGU .

ENDFORM.                    " read_car_desc_by_model
*&---------------------------------------------------------------------*
*&      Form  search_data_app237
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZSPP_APP237_MODEL  text
*      -->P_ZSPP_APP237_INQS  text
*      -->P_ZSPP_APP237_DDAY  text
*      -->P_ZSPP_APP237_BODYNO  text
*----------------------------------------------------------------------*
FORM SEARCH_DATA_APP237 USING    P_MODEL     "Model
                                 P_REP_TYPE  "Report Type
                                 P_D_INT     "Delay Interval
                                 P_BODYNO .  "Body No.
  DATA: L_FLAG    ,
        L_TEXT(40).
*  clear l_flag.
* Check Input Parameters.
  PERFORM CHECK_INPUT_PARA_APP237 USING    P_REP_TYPE
                                           P_MODEL
                                           P_D_INT
                                           P_BODYNO
                                  CHANGING L_FLAG
                                           L_TEXT   .
  IF L_FLAG <> SPACE.
    CONCATENATE 'Set The Parameter -' L_TEXT ' !!'
      INTO L_TEXT.
    MESSAGE S000 WITH L_TEXT .
    EXIT.
  ENDIF.
* Create Data
  CLEAR: IT_DLS_2107, IT_DLS_2107[].
  IF P_REP_TYPE = '1'.
*   Delay Time Order
    PERFORM MAKE_DATA_BY_DAY_APP237 TABLES IT_DLS_2107
                                    USING  P_MODEL
                                           P_D_INT   .
  ELSE.
*   Body-No Order
    " Check the Vehicle Master..
    CONCATENATE P_MODEL P_BODYNO INTO L_TEXT.
    SELECT SINGLE EQUNR INTO L_TEXT
      FROM EQUI
     WHERE EQUNR = L_TEXT(10)
       AND EQTYP = 'V'       .
    IF SY-SUBRC = 0.
      PERFORM MAKE_DATA_BY_BODYNO_APP237 TABLES IT_DLS_2107
                                         USING  P_MODEL
                                                P_BODYNO  .
    ELSE.
      MESSAGE W001 WITH TEXT-100.
      CLEAR: IT_DLS_2107, IT_DLS_2107[].
    ENDIF.
  ENDIF.
  DESCRIBE TABLE IT_DLS_2107 LINES ST_APP237-CAUNT .
ENDFORM.                    " search_data_app237
*&---------------------------------------------------------------------*
*&      Form  check_input_para_app237
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_REP_TYPE  text
*      -->P_P_MODEL  text
*      -->P_P_D_INT  text
*      -->P_P_BODYNO  text
*      <--P_L_FLAG  text
*      <--P_L_TEXT  text
*----------------------------------------------------------------------*
FORM CHECK_INPUT_PARA_APP237 USING    P_TYPE
                                      P_MODEL
                                      P_INT
                                      P_BODYNO
                             CHANGING P_ERROR
                                      P_TEXT.
  IF P_TYPE = SPACE.  "Report Type
    P_ERROR = 'X'.
    P_TEXT = 'Sorting'.
    EXIT.
  ENDIF.
  IF P_MODEL = SPACE.  "Model Type
    P_ERROR = 'X'.
    P_TEXT = 'Model'.
    EXIT.
  ENDIF.
  IF P_TYPE = '1'.  "Delay Time Order
    IF P_INT = SPACE.  "Interval
      P_ERROR = 'X'.
      P_TEXT = 'Delay Day'.
      EXIT.
    ENDIF.
  ELSE.             "Body-No Order
    IF P_BODYNO = SPACE.  "Body NO.
      P_ERROR = 'X'.
      P_TEXT = 'Body No'.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " check_input_para_app237
*&---------------------------------------------------------------------*
*&      Form  make_data_by_day_app237
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DLY_2107  text
*      -->P_P_MODEL  text
*      -->P_P_D_INT  text
*----------------------------------------------------------------------*
FORM MAKE_DATA_BY_DAY_APP237 TABLES   PT_TAB STRUCTURE IT_DLY_2107
                             USING    P_MODEL
                                      P_INT .
  DATA: BEGIN OF LT_VM OCCURS 0,
          OBJEK TYPE AUSP-OBJEK,    "V/M No.
          DATE  TYPE SY-DATUM  ,    "Body In's Shop Date
        END OF LT_VM,
        L_DATE    TYPE SY-DATUM  ,
        L_RP_ST   TYPE AUSP-ATWRT,  "P_RP_STATUS
        L_RP_EN   TYPE AUSP-ATWRT,
        L_SHOP    TYPE AUSP-ATFLV,  "P_RP01_SHOP_DATE
        L_DATE_EN TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N         ,
        L_FLAG,
        L_USAGE_CAR TYPE AUSP-ATWRT.

* Set Working Date For Body In's Shop Date .
  L_DATE = SY-DATUM.
  DO P_INT TIMES.
    L_DATE = L_DATE - 1.
    CLEAR L_FLAG.
    PERFORM CHECK_HOLIDAY USING    L_DATE
                          CHANGING L_FLAG.
    IF L_FLAG <> SPACE.
*     If it is a holiday...
      DO.
        CLEAR L_FLAG.
        L_DATE = L_DATE - 1.
        PERFORM CHECK_HOLIDAY USING    L_DATE
                              CHANGING L_FLAG.
        IF L_FLAG = SPACE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDDO.
  L_DATE_EN = L_NUM = L_DATE. " = sy-datum - p_int .

* Read Current Status 'P_RP_STATUS' (From 'Body In' To 'Not Signed Off')
  SELECT DISTINCT AU~OBJEK
    INTO LT_VM-OBJEK
    FROM ( AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
    WHERE AU~KLART = '002'         AND
          AU~ATWRT >= '01'         AND  "Body IN
          AU~ATWRT <= '17'         AND  "Before Sign Off
          CA~ATNAM = 'P_RP_STATUS'   .
    IF SY-SUBRC = 0.
*     Check Body In's Shop Date.
      SELECT SINGLE AU~ATFLV
        INTO L_SHOP
        FROM ( AUSP AS AU
             INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
        WHERE AU~KLART = '002'              AND
              AU~OBJEK = LT_VM-OBJEK        AND
              AU~ATFLV <= L_DATE_EN         AND
              CA~ATNAM = 'P_RP01_SHOP_DATE'   .
      IF SY-SUBRC <> 0.
        CONTINUE.
      ELSE.
        LT_VM-DATE = L_NUM = L_SHOP .
        APPEND LT_VM.
      ENDIF.
    ENDIF.
  ENDSELECT.

  LOOP AT LT_VM.
    CLEAR PT_TAB.
*   Body No.  "P_BODY_SERIAL
    CHECK LT_VM-OBJEK(3) = WA_MODEL .
    PT_TAB-BODYNO = LT_VM-OBJEK+3(6).

** added by Furong on 09/01/2005 filter out scrap/disposal car'
    CLEAR L_USAGE_CAR.
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_USAGE_CAR'
                                 CHANGING L_USAGE_CAR.
    IF L_USAGE_CAR = 'S' OR L_USAGE_CAR = 'D'.
      CONTINUE.
    ENDIF.
** end of addiion
*   Order No.  "P_WORK_ORDER
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_WORK_ORDER'
                                 CHANGING PT_TAB-WON.
*   Ext.C  "P_EXT_COLOR
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_EXT_COLOR'
                                 CHANGING PT_TAB-EXTC.
*   Int.C  "P_INT_COLOR
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_INT_COLOR'
                                 CHANGING PT_TAB-INTC.
*   Body In "RP01
    MOVE LT_VM-DATE TO PT_TAB-BODYIN.
*    perform read_num_inf_by_veh using    lt_vm-objek
*                                         'P_RP01_SHOP_DATE'
*                                changing pt_tab-bodyin.
*   Paint In "RP02
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP02_SHOP_DATE'
                                CHANGING PT_TAB-PAINTIN.
*   Trim In "RP06
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP06_SHOP_DATE'
                                CHANGING PT_TAB-TRIMIN.
*   C/Final "RP17
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP17_SHOP_DATE'
                                CHANGING PT_TAB-CFINAL.
*   Sign Off "RP18
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP18_SHOP_DATE'
                                CHANGING PT_TAB-SOFF.
*   C/Gate "RP19
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP19_SHOP_DATE'
                                CHANGING PT_TAB-CGATE.

** Added by Furong on 05/19/09
*   VPC Out "RP23
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP23_SHOP_DATE'
                                CHANGING PT_TAB-VPCOUT.
** End of change

*   Set Delayed Days
    L_DATE = PT_TAB-BODYIN.
    DO.
      PT_TAB-DDAY = PT_TAB-DDAY + 1.
      L_DATE = L_DATE + 1.
      CLEAR L_FLAG.
      PERFORM CHECK_HOLIDAY USING    L_DATE
                            CHANGING L_FLAG.
      IF L_FLAG <> SPACE.
        DO.
          L_DATE = L_DATE + 1.
          CLEAR L_FLAG.
          PERFORM CHECK_HOLIDAY USING    L_DATE
                                CHANGING L_FLAG.
          IF L_FLAG = SPACE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
      IF L_DATE >= SY-DATUM.
        EXIT.
      ENDIF.
    ENDDO.

    APPEND PT_TAB.
  ENDLOOP.
ENDFORM.                    " make_data_by_day_app237
*&---------------------------------------------------------------------*
*&      Form  make_data_by_bodyno_app237
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_DLY_2107  text
*      -->P_P_MODEL  text
*      -->P_P_BODYNO  text
*----------------------------------------------------------------------*
FORM MAKE_DATA_BY_BODYNO_APP237 TABLES   PT_TAB STRUCTURE IT_DLY_2107
                                USING    P_MODEL
                                         P_BODYNO.
  DATA: BEGIN OF LT_VM OCCURS 0   ,
          OBJEK TYPE AUSP-OBJEK   ,    "V/M No.
          DATE  TYPE SY-DATUM     ,    "Body In's Shop Date
        END OF LT_VM              ,
        L_RP_ST    TYPE AUSP-ATWRT,  "P_RP_STATUS
        L_RP_EN    TYPE AUSP-ATWRT,
        L_BODY(06) TYPE N         ,  "P_BODY_SERIAL
        L_DATE_EN  TYPE AUSP-ATFLV,
        L_NUM(08)  TYPE N         ,
        L_DATE     TYPE SY-DATUM  ,
        L_USAGE_CAR TYPE AUSP-ATWRT,
        L_FLAG                    .
* Set Body Serial
  L_BODY = P_BODYNO - 1.
  CONCATENATE  WA_MODEL  P_BODYNO  INTO LT_VM-OBJEK .

* Read Current Status 'P_RP_STATUS' (From 'Body In' To 'Not Signed Off')
  SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_VM
    FROM ( AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN )
    WHERE AU~OBJEK < LT_VM-OBJEK   AND
          AU~KLART = '002'         AND
          AU~ATWRT >= '01'         AND  "Body IN
          AU~ATWRT <= '17'         AND  "Before Sign Off
          CA~ATNAM = 'P_RP_STATUS'   .

  LOOP AT LT_VM.
    CLEAR PT_TAB.
    CHECK LT_VM-OBJEK(3) = WA_MODEL .
*   Body No.  "P_BODY_SERIAL
    PT_TAB-BODYNO = LT_VM-OBJEK+3(6) .
** added by Furong on 09/01/2005 filter out scrap/disposal car'
    CLEAR L_USAGE_CAR.
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_USAGE_CAR'
                                 CHANGING L_USAGE_CAR.
    IF L_USAGE_CAR = 'S' OR L_USAGE_CAR = 'D'.
      CONTINUE.
    ENDIF.
** end of addiion

*   Order No.  "P_WORK_ORDER
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_WORK_ORDER'
                                 CHANGING PT_TAB-WON.
*   Ext.C  "P_EXT_COLOR
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_EXT_COLOR'
                                 CHANGING PT_TAB-EXTC.
*   Int.C  "P_INT_COLOR
    PERFORM READ_CHAR_INF_BY_VEH USING    LT_VM-OBJEK
                                          'P_INT_COLOR'
                                 CHANGING PT_TAB-INTC.
*   Body In "RP01
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP01_SHOP_DATE'
                                CHANGING PT_TAB-BODYIN.
*   Paint In "RP02
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP02_SHOP_DATE'
                                CHANGING PT_TAB-PAINTIN.
*   Trim In "RP06
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP06_SHOP_DATE'
                                CHANGING PT_TAB-TRIMIN.
*   C/Final "RP17
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP17_SHOP_DATE'
                                CHANGING PT_TAB-CFINAL.
*   Sign Off "RP18
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP18_SHOP_DATE'
                                CHANGING PT_TAB-SOFF.
*   C/Gate "RP19
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP19_SHOP_DATE'
                                CHANGING PT_TAB-CGATE.

** Added by Furong on 05/19/09
*   VPC Out "RP23
    PERFORM READ_NUM_INF_BY_VEH USING    LT_VM-OBJEK
                                         'P_RP23_SHOP_DATE'
                                CHANGING PT_TAB-VPCOUT.
** End of change

*   Set Delayed Days
    L_DATE = PT_TAB-BODYIN.
    DO.
      PT_TAB-DDAY = PT_TAB-DDAY + 1.
      L_DATE = L_DATE + 1.
      CLEAR L_FLAG.
      PERFORM CHECK_HOLIDAY USING    L_DATE
                            CHANGING L_FLAG.
      IF L_FLAG <> SPACE.
        DO.
          L_DATE = L_DATE + 1.
          CLEAR L_FLAG.
          PERFORM CHECK_HOLIDAY USING    L_DATE
                                CHANGING L_FLAG.
          IF L_FLAG = SPACE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
      IF L_DATE >= SY-DATUM.
        EXIT.
      ENDIF.
    ENDDO.

    APPEND PT_TAB.
  ENDLOOP.
ENDFORM.                    " make_data_by_bodyno_app237

*&---------------------------------------------------------------------*
*&      Form  read_char_inf_by_veh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_VM_OBJEK  text
*      -->P_6170   text
*      <--P_IT_DLY_2107_BODYNO  text
*----------------------------------------------------------------------*
FORM READ_CHAR_INF_BY_VEH USING    P_OBJEK
                                   P_ATNAM
                          CHANGING P_ATWRT.
  SELECT SINGLE AU~ATWRT
    INTO P_ATWRT
    FROM AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002'   AND
          AU~OBJEK = P_OBJEK AND
          CA~ATNAM = P_ATNAM   .

ENDFORM.                    " read_char_inf_by_veh
*&---------------------------------------------------------------------*
*&      Form  read_num_inf_by_veh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_VM_OBJEK  text
*      -->P_6202   text
*      <--P_IT_DLY_2107_PAINTIN  text
*----------------------------------------------------------------------*
FORM READ_NUM_INF_BY_VEH USING    P_OBJEK
                                  P_ATNAM
                         CHANGING P_DATE .
  DATA: L_ATFLV   TYPE AUSP-ATFLV,
        L_NUM(08) TYPE N.
  SELECT SINGLE AU~ATFLV
    INTO L_ATFLV
    FROM AUSP AS AU
         INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
    WHERE AU~KLART = '002'   AND
          AU~OBJEK = P_OBJEK AND
          CA~ATNAM = P_ATNAM   .
  P_DATE = L_NUM = L_ATFLV .
ENDFORM.                    " read_num_inf_by_veh
*&---------------------------------------------------------------------*
*&      Form  report_type_app237
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NAME  text
*      -->P_ST_APP237_INQS  text
*----------------------------------------------------------------------*
FORM REPORT_TYPE_APP237 USING    P_NAME
                                 P_PARAMETER.
  XVALUE-TEXT = 'Delay Time Order'.
  XVALUE-KEY  = '1'.
  APPEND XVALUE TO XLIST .

  XVALUE-TEXT = 'Body-No Order'.
  XVALUE-KEY  = '2'.
  APPEND XVALUE TO XLIST .

* LIST BOX SETTING
  PERFORM LIST_BOX_FUNCTION USING P_NAME.

ENDFORM.                    " report_type_app237
*&---------------------------------------------------------------------*
*&      Form  check_holiday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_DATE  text
*      <--P_L_FLAG  text
*----------------------------------------------------------------------*
FORM CHECK_HOLIDAY USING    P_DATE
                   CHANGING P_FLAG.
  DATA: LT_HOLIDAY TYPE ISCAL_DAY OCCURS 0 .
  DATA: L_LINE TYPE I.
  CLEAR: LT_HOLIDAY, LT_HOLIDAY[].
  CALL FUNCTION 'HOLIDAY_GET'
   EXPORTING
*     HOLIDAY_CALENDAR                 = ' '
     FACTORY_CALENDAR                 = 'HM'
     DATE_FROM                        = P_DATE
     DATE_TO                          = P_DATE
*   IMPORTING
*     YEAR_OF_VALID_FROM               =
*     YEAR_OF_VALID_TO                 =
*     RETURNCODE                       =
    TABLES
      HOLIDAYS                         = LT_HOLIDAY
*   EXCEPTIONS
*     FACTORY_CALENDAR_NOT_FOUND       = 1
*     HOLIDAY_CALENDAR_NOT_FOUND       = 2
*     DATE_HAS_INVALID_FORMAT          = 3
*     DATE_INCONSISTENCY               = 4
*     OTHERS                           = 5
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  DESCRIBE TABLE LT_HOLIDAY LINES L_LINE.
  IF L_LINE > 0 .
    P_FLAG = 'X'.
  ENDIF.
ENDFORM.                    " check_holiday

*&---------------------------------------------------------------------*
*&      Form  increase_vm_2203
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INCREASE_VM_2203.
  DATA: L_OBJEK    LIKE AUSP-OBJEK .

  CLEAR: WA_ERR_FLAG .
  CONCATENATE WA_MODEL       ST_2203_INPUT-BODY INTO L_OBJEK.
  SELECT OBJEK INTO L_OBJEK
    FROM ZVPP_CHA UP TO 1 ROWS
   WHERE OBJEK > L_OBJEK
     AND KLART = '002'
     AND ATNAM = 'P_RETURN_DATE'
     ORDER BY OBJEK .
  ENDSELECT.

  IF SY-SUBRC = 0 AND L_OBJEK(3) = WA_MODEL            .
    ST_2203_INPUT-BODY = L_OBJEK+3(6) .
  ELSE.
    WA_ERR_FLAG = 'X'.
    MESSAGE W001 WITH TEXT-010 .
  ENDIF.
ENDFORM.                    " increase_vm_2203

*&---------------------------------------------------------------------*
*&      Form  set_next_vm_app260
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_NEXT_VM_APP260.
  DATA: L_OBJEK    LIKE AUSP-OBJEK .

  CLEAR: WA_ERR_FLAG, WA_CHANGE.
  CONCATENATE WA_MODEL   ST_2204_INPUT-BODY INTO L_OBJEK.
  SELECT OBJEK INTO L_OBJEK
    FROM ZVPP_CHA UP TO 1 ROWS
   WHERE OBJEK > L_OBJEK
     AND KLART = '002'
     AND ATNAM = 'P_USAGE_CAR'
     AND ATWRT = 'S'
     ORDER BY OBJEK .
  ENDSELECT.

  IF SY-SUBRC = 0 AND L_OBJEK(3) = WA_MODEL            .
    ST_2204_INPUT-BODY = L_OBJEK+3(6) .
  ELSE.
    WA_ERR_FLAG = 'X'.
    MESSAGE W001 WITH TEXT-010 .
  ENDIF.
ENDFORM.                    " set_next_vm_app260
*&---------------------------------------------------------------------*
*&      Form  MAKE_PROGRESS_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_PROGRESS_RANGE.                                   "UD1K912914

  CLEAR: P_PROG, P_PROG[].                                  "UD1K912914
  IF P_PROG_APP246 IS INITIAL AND                           "UD1K912914
     P_PROG_APP246_H IS INITIAL.                            "UD1K912914
  ELSE.                                                     "UD1K912914
    IF NOT P_PROG_APP246 IS INITIAL AND                     "UD1K912914
        P_PROG_APP246_H IS INITIAL.                         "UD1K912914
      P_PROG-SIGN   = 'I'.                                  "UD1K912914
      P_PROG-OPTION = 'EQ'.                                 "UD1K912914
      P_PROG-LOW    = P_PROG_APP246.                        "UD1K912914
    ELSEIF P_PROG_APP246 IS INITIAL AND                     "UD1K912914
          NOT P_PROG_APP246_H IS INITIAL.                   "UD1K912914
      P_PROG-SIGN   = 'I'.                                  "UD1K912914
      P_PROG-OPTION = 'EQ'.                                 "UD1K912914
      P_PROG-LOW    = P_PROG_APP246_H.                      "UD1K912914
    ELSE.                                                   "UD1K912914
      P_PROG-SIGN   = 'I'.                                  "UD1K912914
      P_PROG-OPTION = 'BT'.                                 "UD1K912914
      P_PROG-LOW    = P_PROG_APP246.                        "UD1K912914
      P_PROG-HIGH   = P_PROG_APP246_H.                      "UD1K912914
    ENDIF.                                                  "UD1K912914
    APPEND P_PROG.                                          "UD1K912914
  ENDIF.                                                    "UD1K912914


ENDFORM.             " MAKE_PROGRESS_RANGE    "UD1K912914
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DAILY_DELAY_CAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_DAILY_DELAY_CAR.
  CALL SCREEN '2108'.
ENDFORM.                    " DISPLAY_DAILY_DELAY_CAR
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_2108
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA_2108.
  DATA: LT_AUSP      LIKE AUSP  OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF LT_AUSP1  OCCURS 0 ,
          SEQ        LIKE ZTPP_DELAY_CAR-SEQ,
          OBJEK      LIKE AUSP-OBJEK,
        END OF LT_AUSP1.
  DATA: L_DATE       LIKE SY-DATUM,
        L_WOC(20)    TYPE C,
        L_TABIX      LIKE SY-TABIX,
        WA_AUSP      LIKE AUSP.

  CLEAR: IT_2108, WA_2108.
  CLEAR: IT_CABN[], IT_CABN, R_ATINN[], R_ATINN.

* read delayed car
  SELECT SINGLE WDATE INTO L_DATE
      FROM ZTPP_DELAY_CAR.
  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'No data'.
    EXIT.
  ENDIF.
  SELECT SEQ OBJEK INTO CORRESPONDING FIELDS OF TABLE LT_AUSP1
    FROM ZTPP_DELAY_CAR
    WHERE WDATE = L_DATE.
  IF SY-SUBRC NE 0.
    MESSAGE S000 WITH 'No data'.
    EXIT.
  ENDIF.
  SORT LT_AUSP1 BY OBJEK.
* make the range
  L_ATNAM = 'P_WORK_ORDER'   .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_EXT_COLOR'    .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_INT_COLOR'    .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP_STATUS'    .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP01_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP02_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP03_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP04_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP05_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP06_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP07_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP08_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP09_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP10_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP11_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP12_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP13_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP14_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP15_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP16_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.
  L_ATNAM = 'P_RP17_ACTUAL_DATE' .
  PERFORM GET_ATINN USING L_ATNAM.

*  read the characteristics value
  SELECT * INTO TABLE LT_AUSP
    FROM AUSP
    FOR ALL ENTRIES IN LT_AUSP1
    WHERE OBJEK = LT_AUSP1-OBJEK
     AND  ATINN IN R_ATINN
     AND  KLART = '002'.
  SORT LT_AUSP BY OBJEK .


* transfer the values
  CLEAR: WA_2108.
  LOOP AT LT_AUSP.

    READ TABLE IT_CABN WITH KEY ATINN = LT_AUSP-ATINN.

    CASE IT_CABN-ATNAM.
      WHEN 'P_WORK_ORDER'.
        MOVE : LT_AUSP-ATWRT TO WA_2108-WORDER.

      WHEN 'P_RP_STATUS'.
        MOVE : LT_AUSP-ATWRT TO WA_2108-STATUS.

      WHEN 'P_EXT_COLOR'.
        MOVE : LT_AUSP-ATWRT TO WA_2108-EXTC.

      WHEN 'P_INT_COLOR'.
        MOVE : LT_AUSP-ATWRT TO WA_2108-INTC.

      WHEN 'P_RP01_ACTUAL_DATE'.
        WA_2108-RP01  = LT_AUSP-ATWRT   .

      WHEN 'P_RP02_ACTUAL_DATE'.
        WA_2108-RP02  = LT_AUSP-ATWRT   .

      WHEN 'P_RP03_ACTUAL_DATE'.
        WA_2108-RP03  = LT_AUSP-ATWRT   .

      WHEN 'P_RP04_ACTUAL_DATE'.
        WA_2108-RP04  = LT_AUSP-ATWRT   .

      WHEN 'P_RP05_ACTUAL_DATE'.
        WA_2108-RP05  = LT_AUSP-ATWRT   .

      WHEN 'P_RP06_ACTUAL_DATE'.
        WA_2108-RP06  = LT_AUSP-ATWRT   .

      WHEN 'P_RP07_ACTUAL_DATE'.
        WA_2108-RP07  = LT_AUSP-ATWRT   .

      WHEN 'P_RP08_ACTUAL_DATE'.
        WA_2108-RP08  = LT_AUSP-ATWRT   .

      WHEN 'P_RP09_ACTUAL_DATE'.
        WA_2108-RP09  = LT_AUSP-ATWRT   .

      WHEN 'P_RP10_ACTUAL_DATE'.
        WA_2108-RP10  = LT_AUSP-ATWRT   .

      WHEN 'P_RP11_ACTUAL_DATE'.
        WA_2108-RP11  = LT_AUSP-ATWRT   .

      WHEN 'P_RP12_ACTUAL_DATE'.
        WA_2108-RP12  = LT_AUSP-ATWRT   .

      WHEN 'P_RP13_ACTUAL_DATE'.
        WA_2108-RP13  = LT_AUSP-ATWRT   .

      WHEN 'P_RP14_ACTUAL_DATE'.
        WA_2108-RP14  = LT_AUSP-ATWRT   .

      WHEN 'P_RP15_ACTUAL_DATE'.
        WA_2108-RP15  = LT_AUSP-ATWRT   .

      WHEN 'P_RP16_ACTUAL_DATE'.
        WA_2108-RP16  = LT_AUSP-ATWRT   .

      WHEN 'P_RP17_ACTUAL_DATE'.
        WA_2108-RP17  = LT_AUSP-ATWRT   .

    ENDCASE.


    AT END OF OBJEK.
      WA_2108-WDATE = L_DATE.
      WA_2108-OBJEK = LT_AUSP-OBJEK.
      READ TABLE LT_AUSP1 WITH KEY OBJEK = LT_AUSP-OBJEK
                       BINARY SEARCH.
      WA_2108-SEQ   = LT_AUSP1-SEQ.

      SELECT SINGLE FSC INTO WA_2108-FSC
         FROM ZTPP_WOSUM
         WHERE WO_SER = WA_2108-WORDER+0(9)
           AND NATION = WA_2108-WORDER+9(3)
           AND DEALER = WA_2108-WORDER+12(2)
           AND EXTC   = WA_2108-EXTC
           AND INTC   = WA_2108-INTC.

      APPEND WA_2108 TO IT_2108.
      CLEAR: WA_2108, L_WOC.
    ENDAT.

  ENDLOOP.


ENDFORM.                    " GET_DATA_2108
*&---------------------------------------------------------------------*
*&      Form  CREAT_ALV_2108
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CREAT_ALV_2108.
  DATA: LS_LAYOUT TYPE LVC_S_LAYO.
  IF GS_CUSTOM_CONTAINER IS INITIAL.
    CREATE OBJECT GS_CUSTOM_CONTAINER
           EXPORTING CONTAINER_NAME = WA_CC_2108.
    CREATE OBJECT ALV_GRID
           EXPORTING I_PARENT = GS_CUSTOM_CONTAINER.
    PERFORM SET_LAYOUT USING LS_LAYOUT .
    CALL METHOD ALV_GRID->SET_TABLE_FOR_FIRST_DISPLAY
         EXPORTING I_STRUCTURE_NAME = 'ZTPP_DELAY_CAR'
                   IS_LAYOUT         = LS_LAYOUT
         CHANGING  IT_OUTTAB        = IT_2108.
  ENDIF.

ENDFORM.                    " CREAT_ALV_2108
*&---------------------------------------------------------------------*
*&      Form  get_atinn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_ATNAM  text
*----------------------------------------------------------------------*
FORM GET_ATINN USING    P_ATNAM.
  CLEAR: R_ATINN.
  R_ATINN-OPTION = 'EQ'.
  R_ATINN-SIGN   = 'I'.
  SELECT SINGLE ATINN INTO R_ATINN-LOW
   FROM CABN
   WHERE ATNAM = P_ATNAM.
  APPEND R_ATINN.
  IT_CABN-ATNAM = P_ATNAM.
  IT_CABN-ATINN = R_ATINN-LOW.
  APPEND IT_CABN.
ENDFORM.                    " get_atinn
*&---------------------------------------------------------------------*
*&      Form  set_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_LAYOUT  text
*----------------------------------------------------------------------*
FORM SET_LAYOUT USING    PS_LAYOUT TYPE LVC_S_LAYO.

  PS_LAYOUT-ZEBRA  = 'X'.
  PS_LAYOUT-CWIDTH_OPT  = 'X'.

ENDFORM.                    " set_layout
*&---------------------------------------------------------------------*
*&      Form  check_scrap_car
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_OBJEK_OBJEK  text
*      -->P_3889   text
*      <--P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM CHECK_SCRAP_CAR USING    P_VMNO
                              P_CHAR
                     CHANGING P_SUBRC.

  SELECT SINGLE OBJEK
           INTO IT_OBJEK-OBJEK
           FROM AUSP AS AU
     INNER JOIN CABN AS CA ON AU~ATINN = CA~ATINN
          WHERE OBJEK    = P_VMNO      AND
                KLART    = '002'       AND
                ( ATWRT    = 'S' OR ATWRT = 'D' ) AND
                CA~ATNAM =  P_CHAR.
  P_SUBRC = SY-SUBRC.

ENDFORM.                    " check_scrap_car
