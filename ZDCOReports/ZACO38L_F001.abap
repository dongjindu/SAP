*----------------------------------------------------------------------*
***INCLUDE ZACO38L_F001 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CHECK_CCTRGROUP_HI
*&---------------------------------------------------------------------*
*       Check if the Input Value of CCtrgrp on screen is a child of
*       'HMMA1' or HMMA1 itself .
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_CCTRGROUP_HI.
* Read Hierarchy From Object ID, Read CCtr from CCgrp 'HMMA1'
  PERFORM READ_HI_FR_SETID(SAPLZGCO_GLOBAL_FORM)
                            TABLES IT_NODES
                                   IT_VALUES
                            USING  P_KOKRS
                                   '0101'
                                   GV_CCGR_SETID.
* Check whether the CCtrgrp user input is reverted to 'HMMA1' group .
  CLEAR IT_NODES.
  READ TABLE IT_NODES WITH KEY SHORTNAME = P_NCOAL.
  IF SY-SUBRC <> 0.
    MESSAGE E002  WITH P_NCOAL GV_CCGR_SETID.
  ENDIF.

ENDFORM.                    " CHECK_CCTRGROUP_HI

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_GROUP
*&---------------------------------------------------------------------*
*       Read CCtr Group (Search Help)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CCTR_GROUP.
  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS                  = 'X'
      CLASS                    = '0101'
*     CRUSER                   = '*'
      FIELD_NAME               = SPACE
*     SEARCHFLD                = '    '
*     SEARCHFLD_INPUT          = 'X'
      SEARCHFLD_REQUIRED       = ' '
*     SET                      = GV_CCGR_SETID
*     START_COLUMN             = 10
*     START_ROW                = 5
*     TABLE                    = 'CCSS'
*     TYPELIST                 = 'BS'
*     UPDUSER                  = '*'
*     KOKRS                    =
*     KTOPL                    =
    IMPORTING
*     CLASS_NAME               =
      SET_NAME                 = P_NCOAL
*     SET_TITLE                =
*     TABLE_NAME               =
*     SETID                    =
    EXCEPTIONS
      NO_SET_PICKED            = 1
      OTHERS                   = 2.

* No error check for F4  SH
  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_CCTR_GROUP

*&---------------------------------------------------------------------*
*&      Form  READ_CHART_ACC
*&---------------------------------------------------------------------*
*       Read Chart of Account/Asset Master Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CHART_ACC.
* Chart Of account
  CALL FUNCTION 'FC_RFC_KTOPL_GET'
    EXPORTING
      I_BUKRS                             = P_BUKRS
*     I_RCOMP                             =
      I_RLDNR                             = '00'
    IMPORTING
      E_KTOPL                             = GV_KTOPL
    EXCEPTIONS
      BOTH_COMP_CODE_AND_COMPANY          = 1
      NEITHER_COMP_CODE_NOR_COMPANY       = 2
      ORG_DATA_CANNOT_BE_DETERMINED       = 3
      ORG_DATA_ERROR                      = 4
      OTHERS                              = 5.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Asset Master Record Segment
* Read all data with key 'BUKRS'. No improvement can be expected because
* the number of record is about 300
  CLEAR :  IT_ANLA_AUFK[], IT_ANLA_AUFK.

  SELECT   BUKRS ANLN1 ANLN2
           IZWEK
* Read Vehicle Type from Asset master data (2003.08.20)
*           AF~IZWEK AF~AUFNR AF~OBJNR **** deleted
      INTO CORRESPONDING FIELDS OF TABLE IT_ANLA_AUFK
      FROM ANLA
      FOR ALL ENTRIES  IN IT_ZTCO_VEHI_TYPE
     WHERE BUKRS = P_BUKRS
* Except COMMON Vehicle Type (User Input Ex: 'Z')
*       AND IZWEK IN S_IZWEK.
        AND IZWEK = IT_ZTCO_VEHI_TYPE-IZWEK.

* Check Vechicle Type
  IF IT_ANLA_AUFK[] IS INITIAL.
    MESSAGE E023 .
  ENDIF.

ENDFORM.                    " READ_CHART_ACC

*&---------------------------------------------------------------------*
*&      Form  READ_CCTRS
*&---------------------------------------------------------------------*
*       Read CCtrs for retrieval.
*       All CCtrs in Direct/Indirect/Semidirect CCtrs
*       but Engine CCtrs
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CCTRS.

* Read Hierarchy From Object ID, Read CCtr from CCgrp 'HMMA2' (engine)
  PERFORM READ_HI_FR_SETID(SAPLZGCO_GLOBAL_FORM)
                            TABLES IT_NODES
                                   IT_VALUES
                            USING  P_KOKRS
                                   '0101'
                                   GV_CCGR_SETID2.
  CLEAR : IT_NODES_HMMA2,  IT_NODES_HMMA2[],
          IT_VALUES_HMMA2, IT_VALUES_HMMA2[].
  IT_NODES_HMMA2[]  = IT_NODES[].
  IT_VALUES_HMMA2[] = IT_VALUES[].

* Read Hierarchy From Object ID, Read CCtr from CCgrp User input
  PERFORM READ_HI_FR_SETID(SAPLZGCO_GLOBAL_FORM)
                            TABLES IT_NODES
                                   IT_VALUES
                            USING  P_KOKRS
                                   '0101'
                                   P_NCOAL.
  CLEAR : IT_NODES_P_NCOAL,  IT_NODES_P_NCOAL[],
          IT_VALUES_P_NCOAL, IT_VALUES_P_NCOAL[].
  IT_NODES_P_NCOAL[]  = IT_NODES[].
  IT_VALUES_P_NCOAL[] = IT_VALUES[].

* Making an internal table for CCtr to select data
* Get Valid CCtr Only FOR GV_CCGR_SETID - 'HMMA1'
  DATA : LV_DATUM LIKE SY-DATUM.
  CONCATENATE P_GJAHR P_VONPE+1(2) '01' INTO LV_DATUM.
  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
       EXPORTING
            CONTROLLINGAREA = P_KOKRS
            DATE_FROM       = LV_DATUM
            COSTCENTERGROUP = P_NCOAL
       TABLES
            COSTCENTERLIST  = IT_COSTCENTERLIST
            RETURN          = IT_RETURN.
* Message
  PERFORM DIS_BAPI_MESSAGE.

* Check Engin CCtrs
* The Cost Centers which are for Engine Shop should be removed .
  LOOP AT IT_COSTCENTERLIST.
    CLEAR IT_VALUES_HMMA2.
    LOOP AT IT_VALUES_HMMA2 WHERE VFROM =< IT_COSTCENTERLIST-COSTCENTER
                              AND VTO   => IT_COSTCENTERLIST-COSTCENTER.
    ENDLOOP.
    IF SY-SUBRC = 0.
      DELETE IT_COSTCENTERLIST.
    ENDIF.
    CLEAR IT_COSTCENTERLIST.
  ENDLOOP.

ENDFORM.                    " READ_CCTRS

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DIS_BAPI_MESSAGE.
  IF NOT IT_RETURN[] IS INITIAL.
    LOOP AT   IT_RETURN.
      MESSAGE ID     IT_RETURN-ID
              TYPE   IT_RETURN-TYPE
              NUMBER IT_RETURN-NUMBER
              WITH   IT_RETURN-MESSAGE_V1
                     IT_RETURN-MESSAGE_V2
                     IT_RETURN-MESSAGE_V3
                     IT_RETURN-MESSAGE_V4.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DIS_BAPI_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  READ_DEP_PLAN_COST
*&---------------------------------------------------------------------*
*       Read Depreciation COST (PLAN Version)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DEP_PLAN_COST.

  CLEAR : IT_GT_OUTTAB, IT_GT_OUTTAB[].
* First run
* Progress Ind.
  PERFORM PROGRESS_IND USING '30'
                             TEXT-030.
  PERFORM READ_DEP_CUL_DATA  TABLES S_GNJHR1
                                    S_EAUFN1
                                    S_ANLKL1
                             USING  P_PRNAM1
                                    P_POSI1
                                    P_PGSEL1
                                    P_XAUFT1.

**// Mod. by hyung Jin Youn 2004.01.13
*  The Second run will not be used .
*  Only First run should be done
** Second run
** Progress Ind.
*  PERFORM PROGRESS_IND USING '60'
*                             TEXT-031.
*  PERFORM READ_DEP_CUL_DATA  TABLES S_GNJHR2
*                                    S_EAUFN2
*                                    S_ANLKL2
*                             USING  P_PRNAM2
*                                    P_POSI2
*                                    P_PGSEL2
*                                    P_XAUFT2.
**// End of Mod.

  CLEAR IT_GT_OUTTAB.

* Collect data in IT_GT_OUTTAB
* Progress Ind.
  PERFORM PROGRESS_IND USING '80'
                             TEXT-032.
  PERFORM COLLECT_PL_DEP_COST.

ENDFORM.                    " READ_DEP_PLAN_COST

*&---------------------------------------------------------------------*
*&      Form  READ_DEP_CUL_DATA
*&---------------------------------------------------------------------*
*       Planned Dep. Cost Test Run
*----------------------------------------------------------------------*
*      -->SO_GNJHR  Approval year
*      -->SO_EAUFN  Order
*      -->SO_ANLKL  Asset Class
*      -->PA_PRNAM  investment Program
*      -->PA_POSI1  From position
*      -->PA_PGSEL  Select. For Order/WBS/ap.req
*      -->PA_XAUFT  Select orders
*----------------------------------------------------------------------*
FORM READ_DEP_CUL_DATA TABLES   SO_GNJHR  STRUCTURE S_GNJHR1
                                SO_EAUFN  STRUCTURE S_EAUFN1
                                SO_ANLKL  STRUCTURE S_ANLKL1
                       USING    PA_PRNAM
                                PA_POSI1
                                PA_PGSEL
                                PA_XAUFT .

  DATA: IT_L_SELTAB     LIKE TABLE OF RSPARAMS
                        WITH HEADER LINE.
  RANGES : R_BUKRS      FOR T001-BUKRS.

* Company Code data
  CLEAR : R_BUKRS, R_BUKRS[].
  R_BUKRS-LOW    = P_BUKRS.
  R_BUKRS-SIGN   = 'I'.
  R_BUKRS-OPTION = 'EQ'.
  APPEND R_BUKRS. CLEAR R_BUKRS.

* Initialize select conditions on selection screen
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      CURR_REPORT           = 'ZACO38R_DEPR'
*   IMPORTING
*     SP                    =
    TABLES
      SELECTION_TABLE       = IT_L_SELTAB
    EXCEPTIONS
      NOT_FOUND             = 1
      NO_REPORT             = 2
      OTHERS                = 3.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Free Memory ID
  FREE MEMORY ID 'ZMIDCO_001'.

* Submit a report to read planned dep. cost - Parameter passing
  SUBMIT ZACO38R_DEPR
*         VIA SELECTION-SCREEN
*         USING SELECTION-SET 'SS'
*         USING SELECTION-SETS OF PROGRAM 'ZACO38R_DEPR'
         WITH  SELECTION-TABLE IT_L_SELTAB
* Main
         WITH  XEINZEL   = 'X'       "Asset List
         WITH  BUKRS     IN R_BUKRS  "Company Code
         WITH  PA_XANLG  = 'X'       "Select Assets
* Planned Cap. Investments
         WITH  PA_VERSN  = SPACE     "Plan Version
         WITH  PA_XBUDG  = 'X'       "Budget values as basis
                                     " for deprec. simulation
         WITH  PA_XINVP  = 'X'       "Select capital-investment
                                     " program positions
         WITH  PA_PRNAM  = PA_PRNAM  "Inv. program
         WITH  PA_POSI1  = PA_POSI1  "From position
         WITH  SO_GNJHR  IN SO_GNJHR "Approval year
         WITH  PA_PGSEL  = PA_PGSEL  "Select. For Order/WBS/ap.req
         WITH  PA_XAUFT  = PA_XAUFT  "Select orders
         WITH  SO_EAUFN  IN SO_EAUFN "Order
         WITH  PA_XGJBG  = 'X'       "Asset values at fiscal year start
* Selection
         WITH  SO_ANLKL  IN SO_ANLKL "Asset Class
* Setting
         WITH  BEREICH1 = P_AFABER   "DEP. area
* Planning Period
         WITH  PA_GSJHR = P_GJAHR    "Plan Year
         WITH  PA_VONPE = P_VONPE    "Plan Period (from)
         WITH  PA_BISPE = P_BISPE    "Plan Period (To)
* Futher Setting for planning
         WITH  PA_VERSI = P_VERSN    "Plan Version
         WITH  PA_LSTAU = 'X'        "Activity-Type-Indep. planning
         WITH  PA_LSTAR = 'X'        "Activity-Type-Dep. planning
         WITH  PA_UZINS = 'X'        "Transfer Interest
         WITH  PA_PLKST = 'X'        "Planning On Cost Center
         WITH  PA_TESTL = 'X'        "Test Run
*         USER SY-UNAME VIA JOB SY-REPID NUMBER 001
         AND RETURN .

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Get DATA from the report
  CLEAR : GT_OUTTAB, GT_OUTTAB[].

  IMPORT GT_OUTTAB = GT_OUTTAB FROM MEMORY ID 'ZMIDCO_001'.

  APPEND LINES OF GT_OUTTAB  TO IT_GT_OUTTAB.

ENDFORM.                    " READ_DEP_CUL_DATA

*&---------------------------------------------------------------------*
*&      Form  COLLECT_PL_DEP_COST
*&---------------------------------------------------------------------*
*       Collect data in IT_GT_OUTTAB
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM COLLECT_PL_DEP_COST.
* For screening CCtrs
* - Check the CCtrs if they are in the CCtr group user input
*   If not, remove them ,
* - On selection screen, user can select cctr group which dose not
*   contain direct CCtrs. But ONLY direct CCtrs can be considered.
*   Therefore, that ALL cctr have ONLY MCH_HR as Activity Type
*   is a premise.

* For Asset number, The standard program 'RAKOPL02' stores
*  it into the field 'objekt'
* - In program RAKOPL02_ALV_FORMS
*   ANLAV-WRTTP = '0'.
*   if ANLAV-WRTTP NOT IN (1,2,3,4)
*    write anlav-anln0 to gt_outtab-objekt.
*    write anlav-anln2 to text_s.
*    concatenate gt_outtab-objekt '/' text_s into gt_outtab-objekt.

* For Plant Code
* Planned Dep. Cost are calculated with all plant information.
* However,
* The costs of materials assigned to Engine Plant will be gathered to
* Engine CCtrs and the Engine CCtrs were excluded at previous code block
* so the costs for Engine Plant will be excluded during check of CCtrs

* Warnning!
* IT_GT_OUTTAB has no period information therefore it is required to
* move values to proper fields with period information.

  CLEAR IT_GT_OUTTAB.
  CLEAR : IT_ZTCO_PLANDEP, IT_ZTCO_PLANDEP[].

* Choose CCtrs and Planned Dep. Cost
*IT_COSTCENTERLIST
  LOOP AT IT_GT_OUTTAB.
    CLEAR IT_COSTCENTERLIST.
    READ TABLE IT_COSTCENTERLIST
         WITH KEY COSTCENTER = IT_GT_OUTTAB-KOSTL.
    IF SY-SUBRC = 0.
      CLEAR : IT_ZTCO_PLANDEP.
*      MOVE-CORRESPONDING IT_GT_OUTTAB TO IT_ZTCO_PLANDEP.
      PERFORM TRANSFERRING_DATA.
* KOKRS BUKRS KOSTL
      IT_ZTCO_PLANDEP-KOKRS = IT_GT_OUTTAB-KOKRS.
      IT_ZTCO_PLANDEP-BUKRS = IT_GT_OUTTAB-BUKRS.
      IT_ZTCO_PLANDEP-KOSTL = IT_GT_OUTTAB-KOSTL.
* KOART
      IT_ZTCO_PLANDEP-KOART = IT_GT_OUTTAB-KOART.
* WAERS
      IT_ZTCO_PLANDEP-WAERS = IT_GT_OUTTAB-WAERS.
* Asset Number
      SPLIT IT_GT_OUTTAB-OBJEKT AT '/'
       INTO IT_ZTCO_PLANDEP-ANLN1  IT_ZTCO_PLANDEP-ANLN2.
* Fiscal Year
      IT_ZTCO_PLANDEP-GJAHR  =  P_GJAHR.
* Vesion
      IT_ZTCO_PLANDEP-VERSN  =  P_VERSN.
      COLLECT IT_ZTCO_PLANDEP.
      CLEAR   IT_ZTCO_PLANDEP.
    ELSE.
    ENDIF.
    CLEAR IT_GT_OUTTAB.
  ENDLOOP.

  CLEAR   IT_ZTCO_PLANDEP.

* Checking Asset Master (By Vehicle Model)
  CLEAR : IT_ANLA_AUFK.
  LOOP AT IT_ZTCO_PLANDEP.
* Alpha Convertion
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = IT_ZTCO_PLANDEP-ANLN1
         IMPORTING
              OUTPUT = IT_ZTCO_PLANDEP-ANLN1.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = IT_ZTCO_PLANDEP-ANLN2
         IMPORTING
              OUTPUT = IT_ZTCO_PLANDEP-ANLN2.
    MODIFY IT_ZTCO_PLANDEP.
    CLEAR IT_ANLA_AUFK.
    READ TABLE IT_ANLA_AUFK WITH KEY ANLN1 = IT_ZTCO_PLANDEP-ANLN1
                                     ANLN2 = IT_ZTCO_PLANDEP-ANLN2.
    IF SY-SUBRC <> 0.
* Delete record
      DELETE IT_ZTCO_PLANDEP.
    ENDIF.
    CLEAR   IT_ZTCO_PLANDEP.
  ENDLOOP.

  CLEAR   IT_ZTCO_PLANDEP.

ENDFORM.                    " COLLECT_PL_DEP_COST

*&---------------------------------------------------------------------*
*&      Form  UPDATE_TO_ZTCO_PLANDEP
*&---------------------------------------------------------------------*
*       Update Planned Dep. data to ZTCO_PLANDEP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TO_ZTCO_PLANDEP.

* Update
  PERFORM UPDATE_TABLE.
* Retrieval Newest data
  PERFORM READ_DATA_FR_TABLE.

ENDFORM.                    " UPDATE_TO_ZTCO_PLANDEP

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_PLANDE
*&---------------------------------------------------------------------*
*       Enqueue for Mandt, KOKRS, BUKRS, GJAHR, VERSN
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ENQUEUE_ZTCO_PLANDE.
  CALL FUNCTION 'ENQUEUE_EZCO_ZTCO_PLANDE'
    EXPORTING
      MODE_ZTCO_PLANDEP       = 'E'
      MANDT                   = SY-MANDT
      KOKRS                   = P_KOKRS
      BUKRS                   = P_BUKRS
      GJAHR                   = P_GJAHR
      VERSN                   = P_VERSN
*     KOSTL                   =
*     ANLN1                   =
*     ANLN2                   =
*     KOART                   =
*     X_KOKRS                 = ' '
*     X_BUKRS                 = ' '
*     X_GJAHR                 = ' '
*     X_VERSN                 = ' '
*     X_KOSTL                 = ' '
*     X_ANLN1                 = ' '
*     X_ANLN2                 = ' '
*     X_KOART                 = ' '
*     _SCOPE                  = '2'
*     _WAIT                   = ' '
*     _COLLECT                = ' '
    EXCEPTIONS
      FOREIGN_LOCK            = 1
      SYSTEM_FAILURE          = 2
      OTHERS                  = 3
            .
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " ENQUEUE_ZTCO_PLANDE

*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       Update Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM UPDATE_TABLE.

  DATA : LV_ANSWER.

  CLEAR IT_ZTCO_PLANDEP.
  CLEAR ZTCO_PLANDEP.
  SELECT SINGLE * FROM ZTCO_PLANDEP
                 WHERE KOKRS = P_KOKRS
                   AND BUKRS = P_BUKRS
                   AND GJAHR = P_GJAHR
                   AND VERSN = P_VERSN.
* Already Update?
  IF SY-SUBRC = 0.
    PERFORM POPUP_CONFIRM USING LV_ANSWER.
    CASE LV_ANSWER.
      WHEN 'J'.
* Delete all data in table with period, version info.
        DELETE FROM ZTCO_PLANDEP
              WHERE KOKRS = P_KOKRS
                AND BUKRS = P_BUKRS
                AND GJAHR = P_GJAHR
                AND VERSN = P_VERSN.
        PERFORM ERROR_HAN_FOR_TABLE USING TEXT-020.
* Then Insert New DATA
        PERFORM INSERT_DATA.
      WHEN OTHERS.
    ENDCASE.
  ELSE.
* Then Insert New DATA
    PERFORM INSERT_DATA.
  ENDIF.

ENDFORM.                    " UPDATE_TABLE

*&---------------------------------------------------------------------*
*&      Form  POPUP_CONFIRM
*&---------------------------------------------------------------------*
*       Continue?
*----------------------------------------------------------------------*
*  -->  p_answer  OK or Not
*  <--  p2        text
*----------------------------------------------------------------------*
FORM POPUP_CONFIRM USING P_ANSWER.

  DATA LV_TEXTLINE2(120).

  CONCATENATE  P_KOKRS  P_BUKRS  P_GJAHR  P_VERSN
         INTO  LV_TEXTLINE2
         SEPARATED BY '/'.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      DEFAULTOPTION        = 'N'
      TEXTLINE1            = TEXT-008
      TEXTLINE2            = LV_TEXTLINE2
      TITEL                = TEXT-009
*     START_COLUMN         = 25
*     START_ROW            = 6
      CANCEL_DISPLAY       = SPACE
   IMPORTING
      ANSWER               = P_ANSWER.

ENDFORM.                    " POPUP_CONFIRM

*&---------------------------------------------------------------------*
*&      Form  ERROR_HAN_FOR_TABLE
*&---------------------------------------------------------------------*
*       Error check
*----------------------------------------------------------------------*
*      -->P_TEXT   Text for parameters.
*----------------------------------------------------------------------*
FORM ERROR_HAN_FOR_TABLE USING    P_TEXT.
  IF SY-SUBRC <> 0.
    ROLLBACK WORK .
    MESSAGE E030 WITH P_TEXT.
  ENDIF.
ENDFORM.                    " ERROR_HAN_FOR_TABLE

*&---------------------------------------------------------------------*
*&      Form  INSERT_DATA
*&---------------------------------------------------------------------*
*       Insertion
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INSERT_DATA.
* Progress Ind.
  PERFORM PROGRESS_IND USING '95'
                             TEXT-033.
  LOOP AT IT_ZTCO_PLANDEP.
* Log
    IT_ZTCO_PLANDEP-ERDAT = SY-DATUM.
    IT_ZTCO_PLANDEP-ERZET = SY-UZEIT.
    IT_ZTCO_PLANDEP-ERNAM = SY-UNAME.
* Do not use mass insertion method to block run-time error
    INSERT INTO  ZTCO_PLANDEP VALUES IT_ZTCO_PLANDEP.
    PERFORM ERROR_HAN_FOR_TABLE USING TEXT-021.
    CLEAR IT_ZTCO_PLANDEP.
  ENDLOOP.
ENDFORM.                    " INSERT_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_TABLE
*&---------------------------------------------------------------------*
*       Read Newest data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_FR_TABLE.
  CLEAR : IT_DISP_PLANDEP, IT_DISP_PLANDEP[].
  SELECT *
              INTO CORRESPONDING FIELDS OF TABLE IT_DISP_PLANDEP
              FROM  ZTCO_PLANDEP
              WHERE KOKRS = P_KOKRS
                AND BUKRS = P_BUKRS
                AND GJAHR = P_GJAHR
                AND VERSN = P_VERSN.
  CLEAR IT_DISP_PLANDEP.
ENDFORM.                    " READ_DATA_FR_TABLE

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_PERIOD_RANGE.
  IF P_VONPE > P_BISPE.
    MESSAGE E031.
  ENDIF.
* Check Currency IND.
  IF P_CURRT NA 'CTO'.
    MESSAGE E000(ZMCO) WITH P_CURRT ' is not a posible value' .
  ENDIF.
ENDFORM.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  TRANSFERRING_DATA
*&---------------------------------------------------------------------*
*       Transferring AMT data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM TRANSFERRING_DATA.

  FIELD-SYMBOLS : <FS1> TYPE ANY, <FS2> TYPE ANY.
* AMT
  DATA : LV_MAX(2) TYPE N,
         LV_MIN(2) TYPE N.
  DATA : LV_CNT(2) TYPE N.
  DATA : LV_STP(2) TYPE N.
  DATA : LV_FORG(40), LV_FTAR(40).

  LV_STP = 1.
  LV_MIN = P_VONPE.
  LV_MAX = P_BISPE.
  IF LV_MAX > 26. "Maximum: 26.
    LV_MAX = 26.
  ENDIF.

  LV_CNT = LV_MAX - LV_MIN + 1.

  DO LV_CNT TIMES.
    CLEAR : LV_FORG, LV_FTAR.
    CONCATENATE 'IT_ZTCO_PLANDEP-' 'VAL' LV_MIN INTO LV_FTAR.
    CONCATENATE 'IT_GT_OUTTAB-'    'VAL' LV_STP INTO LV_FORG.
    ASSIGN (LV_FTAR) TO <FS2>.
    ASSIGN (LV_FORG) TO <FS1>.

    <FS2> = <FS1>.

    LV_STP = LV_STP + 1.
    LV_MIN = LV_MIN + 1.
  ENDDO.

ENDFORM.                    " TRANSFERRING_DATA

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_IND
*&---------------------------------------------------------------------*
*       Progress IND.
*----------------------------------------------------------------------*
*      -->P_%         %
*      -->P_TEXT      TEXT
*----------------------------------------------------------------------*
FORM PROGRESS_IND USING    P_%
                           P_TEXT.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
    EXPORTING
      PERCENTAGE          = P_%
      TEXT                = P_TEXT
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' '
*     MESSAGEPAR4         = ' '
            .
ENDFORM.                    " PROGRESS_IND

*&---------------------------------------------------------------------*
*&      Form  READ_ZTCO_VEHI_TYPE
*&---------------------------------------------------------------------*
*       Read Vehicle Model DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ZTCO_VEHI_TYPE.
  CLEAR : IT_ZTCO_VEHI_TYPE, IT_ZTCO_VEHI_TYPE[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ZTCO_VEHI_TYPE
           FROM ZTCO_VEHI_TYPE
          WHERE VEHTP IN S_VEHTP.
  CLEAR   IT_ZTCO_VEHI_TYPE.
  IF IT_ZTCO_VEHI_TYPE[] IS INITIAL.
    MESSAGE E020.
  ENDIF.
ENDFORM.                    " READ_ZTCO_VEHI_TYPE

*&---------------------------------------------------------------------*
*&      Form  CHECK_VEH_MODEL
*&---------------------------------------------------------------------*
*       Check Vehicle Model
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_VEH_MODEL.
  CLEAR ZTCO_VEHI_TYPE.
  SELECT SINGLE * FROM  ZTCO_VEHI_TYPE
                  WHERE VEHTP IN S_VEHTP.
  IF SY-SUBRC <> 0.
    MESSAGE E018 WITH 'Vehicle Model'.
  ENDIF.
ENDFORM.                    " CHECK_VEH_MODEL

*&---------------------------------------------------------------------*
*&      Form  CHECK_BEDAE
*&---------------------------------------------------------------------*
*       For Requirement Type
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_BEDAE.
  CLEAR T459A.
  SELECT SINGLE * FROM T459A
                 WHERE BEDAE IN S_BEDAE .
  IF SY-SUBRC <> 0.
    MESSAGE E032.
  ENDIF.
ENDFORM.                    " CHECK_BEDAE

*&---------------------------------------------------------------------*
*&      Form  CHECK_VERSB
*&---------------------------------------------------------------------*
*       For Version (LTP)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_VERSB.
  CLEAR T459V.
  SELECT SINGLE * FROM T459V
                 WHERE SPRAS = SY-LANGU
                   AND VERSB = P_VERSB.

  IF SY-SUBRC <> 0.
    MESSAGE E033.
  ENDIF.
ENDFORM.                    " CHECK_VERSB

*&---------------------------------------------------------------------*
*&      Form  SEARCH_LTP_VR
*&---------------------------------------------------------------------*
*       Searching LTP version
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SEARCH_LTP_VR.
* It is only for Checking FI-PP-CO link
  CLEAR *ZVCO_V_T442C.
  SELECT SINGLE * FROM *ZVCO_V_T442C
          WHERE KOKRS = P_KOKRS
            AND VERSN = P_VERSN
            AND GJAHR = P_GJAHR.
  IF SY-SUBRC <> 0.
    MESSAGE E034.
  ENDIF.
ENDFORM.                    " SEARCH_LTP_VR

*&---------------------------------------------------------------------*
*&      Form  REMV_VM_WO_LTP
*&---------------------------------------------------------------------*
*       Screening Vehicle model without LTP DATA
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM REMV_VM_WO_LTP.

  DATA : IT_L_TMP_ZTCO_VEHI_TYPE
         LIKE STANDARD TABLE OF ZTCO_VEHI_TYPE
         WITH HEADER LINE.
  CLEAR : IT_L_TMP_ZTCO_VEHI_TYPE, IT_L_TMP_ZTCO_VEHI_TYPE[].

* Set range of Delivery/order finish date
  RANGES  R_DATVE FOR SY-DATUM.
  CLEAR : R_DATVE, R_DATVE[].

  CALL FUNCTION 'HR_E_GET_FISC_YEAR_DATES'
       EXPORTING
            FISC_YEAR   = P_GJAHR
       IMPORTING
            FISC_FECINI = R_DATVE-LOW
            FISC_FECFIN = R_DATVE-HIGH
       EXCEPTIONS
            ERROR       = 1
            OTHERS      = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  R_DATVE-OPTION = 'BT'.
  R_DATVE-SIGN = 'I'.
  APPEND R_DATVE. CLEAR R_DATVE.

* read the relationship between Material Code and Vehicle Model
  SELECT DISTINCT
                 MTART VEHTP MATNR
            INTO CORRESPONDING FIELDS OF TABLE IT_L_TMP_ZTCO_VEHI_TYPE
            FROM ZTCO_VEHI_TYPE
           WHERE VEHTP IN S_VEHTP.
  IF IT_L_TMP_ZTCO_VEHI_TYPE[] IS INITIAL.
    MESSAGE E020.
  ENDIF.

  CLEAR IT_L_TMP_ZTCO_VEHI_TYPE.

  LOOP AT IT_L_TMP_ZTCO_VEHI_TYPE.
    CLEAR MDPB.
    SELECT SINGLE * FROM  MDPB
                   WHERE  MATNR LIKE IT_L_TMP_ZTCO_VEHI_TYPE-MATNR
                     AND  WERKS =  P_WERKS
                     AND  BEDAE IN S_BEDAE
                     AND  VERSB =  P_VERSB
                     AND  PDATU IN R_DATVE.
**// Mod. by Hyung Jin Youn 2004.01.13
* All data should be checked - not only monthly data but also
* weekly and daily data.
*                     AND  ENTLI =  '3'. "month
**// End of Mod.
    IF SY-SUBRC <> 0.
      DELETE IT_L_TMP_ZTCO_VEHI_TYPE.
    ENDIF.
    CLEAR IT_L_TMP_ZTCO_VEHI_TYPE.
  ENDLOOP.

  IF IT_L_TMP_ZTCO_VEHI_TYPE[] IS INITIAL.
    MESSAGE E035.
  ENDIF.

* Screening Vehicle model .
  CLEAR S_VEHTP.
* Remove Selected values
  CLEAR : S_VEHTP, S_VEHTP[].
* Message
  DATA : LV_VEH_MOD_TX(100) VALUE ' Vehicle Model : '.

  SORT IT_L_TMP_ZTCO_VEHI_TYPE BY VEHTP.
  LOOP AT  IT_L_TMP_ZTCO_VEHI_TYPE.
* re-orginze Vehicle Model
    ON CHANGE OF IT_L_TMP_ZTCO_VEHI_TYPE-VEHTP.
      S_VEHTP-LOW    = IT_L_TMP_ZTCO_VEHI_TYPE-VEHTP.
      S_VEHTP-OPTION = 'EQ'.
      S_VEHTP-SIGN   = 'I'.
      APPEND S_VEHTP.
      CLEAR  S_VEHTP.
      CONCATENATE LV_VEH_MOD_TX IT_L_TMP_ZTCO_VEHI_TYPE-VEHTP
             INTO LV_VEH_MOD_TX SEPARATED BY '/'.
    ENDON.
    CLEAR IT_L_TMP_ZTCO_VEHI_TYPE.
  ENDLOOP.


  DATA : LV_ANSWER.
  CONCATENATE 'Do you continue only with '
              LV_VEH_MOD_TX
              '?'
         INTO LV_VEH_MOD_TX .


  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR                    = 'Vehicle Model in LTP'
*     DIAGNOSE_OBJECT             =
      TEXT_QUESTION               = LV_VEH_MOD_TX
*     TEXT_BUTTON_1               = 'Ja'(001)
*     ICON_BUTTON_1               = ' '
*     TEXT_BUTTON_2               = 'Nein'(002)
*     ICON_BUTTON_2               = ' '
*     DEFAULT_BUTTON              = '1'
      DISPLAY_CANCEL_BUTTON       = ' '
*     USERDEFINED_F1_HELP         = ' '
*     START_COLUMN                = 25
*     START_ROW                   = 6
*     POPUP_TYPE                  =
    IMPORTING
      ANSWER                      = LV_ANSWER
*   TABLES
*     PARAMETER                   =
   EXCEPTIONS
      TEXT_NOT_FOUND              = 1
      OTHERS                      = 2.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF LV_ANSWER <> '1'.
    MESSAGE E036.
  ENDIF.
ENDFORM.                    " REMV_VM_WO_LTP

*&---------------------------------------------------------------------*
*&      Form  CHK_LTP_AT_QUAN
*&---------------------------------------------------------------------*
*       Rid off without Activity quantity.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHK_LTP_AT_QUAN.

* Refer to DEV ID 'ACO39'
* Making KSPP_DATA & AT
  PERFORM MAKING_KSPP_N_AT.
* Read AT quantity in LTP
  PERFORM READ_LTP_AT_QUAN_2.
* Rid off
  PERFORM DEL_PLANDEP_DATA.

ENDFORM.                    " CHK_LTP_AT_QUAN

*&---------------------------------------------------------------------*
*&      Form  MAKING_KSPP_N_AT
*&---------------------------------------------------------------------*
*       Fill out KSPP data and LSTAR data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKING_KSPP_N_AT.
* P_PCORR and P_LSTAR are used only to read the data about LTP AT
* quantity
* AT
  CLEAR: R_RSLSTAR, R_RSLSTAR[].
  R_RSLSTAR-LOW    = P_LSTAR.
  R_RSLSTAR-SIGN   = 'I'.
  R_RSLSTAR-OPTION = 'EQ'.
  APPEND R_RSLSTAR.
  CLEAR  R_RSLSTAR.
* KSPP
  CLEAR KSPP.
  KSPP-TO_P   = P_BISPE.
  KSPP-FROM_P = P_VONPE.
  KSPP-VERSN  = P_VERSN.
  KSPP-KOKRS  = P_KOKRS.
  KSPP-WERKS  = P_WERKS.
  KSPP-GJAHR  = P_GJAHR.
  KSPP-PCORR  = P_PCORR.
* *** Don NOT change bellow flags
  KSPP-TESTLAUF  = 'X'.
  KSPP-DETAIL_MW = 'X'.
  KSPP-LIST      = 'X'.

ENDFORM.                    " MAKING_KSPP_N_AT

*&---------------------------------------------------------------------*
*&      Form  READ_LTP_AT_QUAN_2
*&---------------------------------------------------------------------*
*       Read AT quantity in LTP
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_LTP_AT_QUAN_2.

  DATA LV_MMID TYPE CHAR30.

  LV_MMID = 'ZMIDCO_003'.

*// Mod. by Hyung Jin Youn 2004.03.15
* It seems that Transaction "KSPP" dose not work properly.
* KSPP dose not output any data with the plant key "P001".
* It is required to check the related configuration later
* * This part is ah-hoc change of source code,
*   Mark this part as comment when the configuration problem is solved.
  CLEAR KSPP-WERKS.
*// End of Mod.


  FREE MEMORY ID LV_MMID.

  CALL FUNCTION 'Z_FCO_LTP_AT_QUANTITY_2'
       EXPORTING
            I_KSPP        = KSPP
            I_MEMID       = LV_MMID
       EXCEPTIONS
            PROCESS_ERROR = 1
            UPD_TASK      = 2
            T442C         = 3
            READ_ERROR    = 4
            OTHERS        = 5.

  IF SY-SUBRC <> 0.
    IF SY-MSGID NE SPACE.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
      MESSAGE E000 WITH 'An error occurs in reading LTP AT Quan.'.
    ENDIF.
  ENDIF.

* Get DATA from FM
  CLEAR : GT_DISPLAYLIST, GT_DISPLAYLIST[].
  IMPORT GT_DISPLAYLIST  FROM MEMORY ID  LV_MMID.
  FREE MEMORY ID LV_MMID.
  CLEAR : GT_DISPLAYLIST.

*// Mod. by Hyung Jin Youn 2004.03.15
* It seems that Transaction "KSPP" dose not work properly.
* KSPP dose not output any data with the plant key "P001".
* It is required to check the related configuration later
* * This part is ah-hoc change of source code,
*   Mark this part as comment when the configuration problem is solved.
  DELETE GT_DISPLAYLIST WHERE WERKS NE P_WERKS.
  CLEAR : GT_DISPLAYLIST.
*// End of Mod.

* Delete records with Not Matched AT - (ex Not Mch_HR )
  DELETE GT_DISPLAYLIST WHERE NOT LSTAR IN R_RSLSTAR.
  CLEAR : GT_DISPLAYLIST.

ENDFORM.                    " READ_LTP_AT_QUAN_2

*&---------------------------------------------------------------------*
*&      Form  DEL_PLANDEP_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DEL_PLANDEP_DATA.
  CLEAR   IT_ZTCO_PLANDEP.
  CLEAR   GT_DISPLAYLIST.

  DATA : LV_SBC.

  LOOP AT IT_ZTCO_PLANDEP.
* Read Inv. Reason Code.
    CLEAR IT_ANLA_AUFK.
    READ TABLE IT_ANLA_AUFK WITH KEY
                                  BUKRS = P_BUKRS
                                  ANLN1 = IT_ZTCO_PLANDEP-ANLN1
                                  ANLN2 = IT_ZTCO_PLANDEP-ANLN2.

**// Mod. As the change of FI - Vehicle Type.
**   2004.01.12 By Hyung Jin youn
** Read Vehicle model
*    CLEAR IT_ZTCO_VEHI_TYPE.
*    READ TABLE IT_ZTCO_VEHI_TYPE
*                            WITH KEY
*                                  IZWEK = IT_ANLA_AUFK-IZWEK.
** Check Vehicle Model(-> Matnr) & CCtr
*    TRANSLATE IT_ZTCO_VEHI_TYPE-MATNR USING '%*_+'.
    CLEAR LV_SBC.

    LOOP AT  IT_ZTCO_VEHI_TYPE
                         WHERE IZWEK = IT_ANLA_AUFK-IZWEK.

      TRANSLATE IT_ZTCO_VEHI_TYPE-MATNR USING '%*_+'.

      LOOP AT  GT_DISPLAYLIST
                        WHERE    KOSTL =    IT_ZTCO_PLANDEP-KOSTL
                          AND    MATNR CP   IT_ZTCO_VEHI_TYPE-MATNR
                          AND    WERKS =    P_WERKS.
        IF   SY-TABIX >= 1
         AND LV_SBC NE 'X'.
          LV_SBC = 'X'.
        ENDIF.
      ENDLOOP.
      CLEAR IT_ZTCO_VEHI_TYPE .
    ENDLOOP.
* Check SUBRC
    IF LV_SBC NE 'X'.
      DELETE IT_ZTCO_PLANDEP.
    ENDIF.
** End Of Mod.

    CLEAR IT_ZTCO_PLANDEP.
  ENDLOOP.
* MEGBTR

ENDFORM.                    " DEL_PLANDEP_DATA

*&---------------------------------------------------------------------*
*&      Form  set_init_val
*&---------------------------------------------------------------------*
*       set init. values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SET_INIT_VAL.
**// Mod By Hyung Jin Youn 2004.01.13
* Adding 'VSE'. So there are two type of Req. Type.
* 'VSF' and 'VSE'
  CLEAR : S_BEDAE, S_BEDAE[].

  S_BEDAE-LOW = 'VSF'.
  S_BEDAE-SIGN = 'I'.
  S_BEDAE-OPTION = 'EQ'.
  APPEND S_BEDAE.
  CLEAR  S_BEDAE.

  S_BEDAE-LOW = 'VSE'.
  S_BEDAE-SIGN = 'I'.
  S_BEDAE-OPTION = 'EQ'.
  APPEND S_BEDAE.
  CLEAR  S_BEDAE.
**// End of Mod.
ENDFORM.                    " set_init_val
