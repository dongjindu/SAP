*----------------------------------------------------------------------*
*   INCLUDE ZACO12L_F001                                               *
*----------------------------------------------------------------------*

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
  CONCATENATE P_GJAHR P_PERAF+1(2) '01' INTO LV_DATUM.
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
*&      Form  READ_ASS_DEP_COST
*&---------------------------------------------------------------------*
*       Read Depreciation Costs from Asset Value Table.
*       ( Also Vehicle Type (Car Type) )
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_ASS_DEP_COST.

*  CLEAR : IT_ANLP[], IT_ANLP.
*
*  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ANLP
*           FROM ANLP INNER JOIN T095B
*             ON T095B~KTOPL = GV_KTOPL
*            AND ANLP~KTOGR  = T095B~KTOGR
*            AND ANLP~AFABER = T095B~AFABE
*           FOR ALL ENTRIES IN IT_COSTCENTERLIST
*          WHERE BUKRS = P_BUKRS
*            AND GJAHR = P_GJAHR
*            AND PERAF = P_PERAF
*            AND KOSTL = IT_COSTCENTERLIST-COSTCENTER
*            AND AFABER = P_AFABER.
*
*  CLEAR IT_ANLP.

* Making Range for CCtr.
  RANGES : R_KOSTL FOR CSKS-KOSTL.
  CLEAR : R_KOSTL, R_KOSTL[].
  LOOP AT IT_COSTCENTERLIST.
    R_KOSTL-SIGN = 'I'.
    R_KOSTL-OPTION = 'EQ'.
    R_KOSTL-LOW  = IT_COSTCENTERLIST-COSTCENTER.
    APPEND R_KOSTL.
    CLEAR  R_KOSTL.
    CLEAR IT_COSTCENTERLIST.
  ENDLOOP.

* Select with
* Depreciation area code : User Input - '01'.
* CCtr                   : User Input - (Selected CCtrs or CCtr. Group)
* Asset number           : The Vechicle type is NOT common type
* Others                 : User Input
  CLEAR : IT_ANLP[], IT_ANLP.

  SELECT * INTO CORRESPONDING FIELDS OF TABLE IT_ANLP
           FROM ANLP INNER JOIN T095B
             ON T095B~KTOPL = GV_KTOPL
            AND ANLP~KTOGR  = T095B~KTOGR
            AND ANLP~AFABER = T095B~AFABE
           FOR ALL ENTRIES IN IT_ANLA_AUFK
          WHERE BUKRS = P_BUKRS
            AND GJAHR = P_GJAHR
            AND PERAF = P_PERAF
            AND KOSTL IN R_KOSTL
            AND AFABER = P_AFABER
            AND ANLN1 = IT_ANLA_AUFK-ANLN1
            AND ANLN2 = IT_ANLA_AUFK-ANLN2.

  CLEAR IT_ANLP.

**-- TEST  Temporary CODE For unit test
**PARAMETERS : P_TEST(1)    .
** Temporary Change of KOSTL
**-- TEST
*  IF P_TEST = 'X'.
*    LOOP AT IT_ANLP.
*      SELECT SINGLE KOSTL INTO IT_ANLP-KOSTL
*                          FROM YCO_TEST
*                         WHERE OLD_KOSTL =  IT_ANLP-KOSTL.
*      IF SY-SUBRC = 0.
*        MODIFY IT_ANLP.
*      ENDIF.
*      CLEAR  IT_ANLP.
*    ENDLOOP.
*  ENDIF.
*  CLEAR  IT_ANLP.
**-- TEST

* Get Currency
  CLEAR T093B.
  SELECT SINGLE * FROM T093B
                 WHERE BUKRS = P_BUKRS
                   AND AFABE = P_AFABER.
  IF SY-SUBRC <> 0.
    MESSAGE E019 WITH 'T093B' P_BUKRS P_AFABER.
  ENDIF.

* Result ITAB from IT_ANLP.
  CLEAR : IT_RES_ANLP, IT_RES_ANLP[].

* Set Vehicle Type (Car Type)
  LOOP AT IT_ANLP.
    CLEAR  IT_ANLA_AUFK.
    READ TABLE  IT_ANLA_AUFK  WITH KEY  BUKRS = IT_ANLP-BUKRS
                                        ANLN1 = IT_ANLP-ANLN1
                                        ANLN2 = IT_ANLP-ANLN2.
* Check Record about Asset Master
    IF SY-SUBRC = 0.
      IT_ANLP-WAERS = T093B-WAERS.
*      IT_ANLP-IZWEK = IT_ANLA_AUFK-IZWEK.
      IT_ANLP-VEHTP = IT_ANLA_AUFK-VEHTP.
      MODIFY IT_ANLP.
* Making result ITAB
      MOVE-CORRESPONDING IT_ANLP TO IT_RES_ANLP.
      COLLECT IT_RES_ANLP.
      CLEAR IT_RES_ANLP.
    ELSE.
*      MESSAGE E010 WITH IT_ANLP-ANLN1 IT_ANLP-ANLN2.
    ENDIF.

** Check Vehicle Type (Car Type)
*    IF  IT_ANLP-IZWEK IS INITIAL.
*      MESSAGE E011 WITH IT_ANLP-ANLN1 IT_ANLP-ANLN2.
*    ENDIF.

    CLEAR IT_ANLP.
  ENDLOOP.

  CLEAR IT_ANLP.
  CLEAR IT_RES_ANLP.

* Check Depreciation cost data
  IF IT_RES_ANLP[] IS INITIAL.
    MESSAGE E022.
  ENDIF.

ENDFORM.                    " READ_ASS_DEP_COST

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

  SELECT   A~BUKRS A~ANLN1 A~ANLN2
           A~IZWEK
           B~VEHTP
* Read Vehicle Type from Asset master data (2003.08.20)
*           AF~IZWEK AF~AUFNR AF~OBJNR **** deleted
      INTO CORRESPONDING FIELDS OF TABLE IT_ANLA_AUFK
*      FROM ANLA
      FROM ANLA AS A INNER JOIN ZTCO_VEHI_TYPE AS B
        ON A~IZWEK = B~IZWEK
     WHERE A~BUKRS = P_BUKRS
* Except COMMON Vehicle Type (User Input Ex: 'Z')
*      AND IZWEK   IN S_IZWEK.
       AND B~VEHTP IN S_VEHTP.

* Check Vechicle Type
  IF IT_ANLA_AUFK[] IS INITIAL.
    MESSAGE E023 .
  ENDIF.

ENDFORM.                    " READ_CHART_ACC

*&---------------------------------------------------------------------*
*&      Form  BUILD_MAT_LIST
*&---------------------------------------------------------------------*
*       Building Material LIST to use in selecting data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM BUILD_MAT_LIST.
* Local Data Definition
  DATA : IT_L_ZTCO_VEHI_TYPE
           LIKE STANDARD TABLE OF ZTCO_VEHI_TYPE
           WITH HEADER LINE .

* Read Mapping Table (Vehicle type and Material Type)
  CLEAR : IT_L_ZTCO_VEHI_TYPE, IT_L_ZTCO_VEHI_TYPE[].
*  SELECT *  INTO CORRESPONDING FIELDS OF TABLE IT_L_ZTCO_VEHI_TYPE
*            FROM ZTCO_VEHI_TYPE.
  SELECT DISTINCT
                 MTART VEHTP MATNR
            INTO CORRESPONDING FIELDS OF TABLE IT_L_ZTCO_VEHI_TYPE
            FROM ZTCO_VEHI_TYPE
           WHERE VEHTP IN S_VEHTP.
  IF IT_L_ZTCO_VEHI_TYPE[] IS INITIAL.
    MESSAGE E020.
  ENDIF.

* Read data from MARA
  DATA : BEGIN OF IT_L_TMP_MARA OCCURS 0.
          INCLUDE STRUCTURE   IT_RS_MARA.
  DATA : END OF  IT_L_TMP_MARA.

  CLEAR : IT_RS_MARA, IT_RS_MARA[].

  LOOP AT IT_L_ZTCO_VEHI_TYPE.
    CLEAR : IT_L_TMP_MARA, IT_L_TMP_MARA[].
    SELECT MATNR MTART
                  INTO CORRESPONDING FIELDS OF TABLE  IT_L_TMP_MARA
                  FROM MARA
                 WHERE MATNR LIKE IT_L_ZTCO_VEHI_TYPE-MATNR
                   AND MTART =    IT_L_ZTCO_VEHI_TYPE-MTART .
    IF NOT IT_L_TMP_MARA[] IS INITIAL.
* Copying Vehicle Model
*     IT_L_TMP_MARA-IZWEK = IT_L_ZTCO_VEHI_TYPE-IZWEK.
      IT_L_TMP_MARA-VEHTP = IT_L_ZTCO_VEHI_TYPE-VEHTP.
*     MODIFY IT_L_TMP_MARA TRANSPORTING IZWEK  WHERE IZWEK EQ SPACE .
      MODIFY IT_L_TMP_MARA TRANSPORTING VEHTP  WHERE VEHTP EQ SPACE .
      APPEND LINES OF IT_L_TMP_MARA  TO IT_RS_MARA.
    ENDIF.
    CLEAR IT_L_ZTCO_VEHI_TYPE.
    CLEAR IT_L_TMP_MARA.
    CLEAR IT_RS_MARA.
  ENDLOOP.

  CLEAR IT_RS_MARA.

  IF IT_RS_MARA[] IS INITIAL .
    MESSAGE E021.
  ENDIF.
ENDFORM.                    " BUILD_MAT_LIST

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_PCC
*&---------------------------------------------------------------------*
*       Read DATA from Product Cost Collector
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_DATA_PCC.
* CO order number is unique each Material Code.
* Under the Item level, there can be more than one records
* In CO view point, Only the Order number level is considered.

* PCC orders in HMMA are created at Plant Level not production level
* So only one PCC order is created each Material Code.
* (2003.10.14)

* Read Order Numbers/OBJ. Key for Order
  PERFORM READ_CO_ORDER_NO.

* Making OBJ. Key for CCtr + AT (For COSS-PAROB/USPOB)
  PERFORM MAKING_CCTR_AT_OBJ_KEY.

* Read Dynamic Fields Name
  PERFORM READ_FIELD_NAME_FROM_DD_COSS.

* Read DATA from PCC
  PERFORM READ_PCC_DATA.

ENDFORM.                    " READ_DATA_PCC

*&---------------------------------------------------------------------*
*&      Form  READ_CO_ORDER_NO
*&---------------------------------------------------------------------*
*        Read Order Numbers
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_CO_ORDER_NO.

* Local DATA definition.
*  DATA : LV_E_PKOSA  LIKE AUFK-AUFNR,
*         LV_E_PROCNR LIKE AUFK-PROCNR,
*         LV_I_PROCNR LIKE AUFK-PROCNR.
  DATA : IT_L_E_VKKS0	LIKE TABLE OF VKKS0
                          WITH HEADER LINE.

* Building ITAB for Order No
  CLEAR : IT_ORDER_DATA, IT_ORDER_DATA[].

  LOOP AT IT_RS_MARA.
*    CLEAR : LV_E_PKOSA, LV_E_PROCNR, LV_I_PROCNR.
*    CALL FUNCTION 'KK_F_PKOSA_LIST_ALL'
*      EXPORTING
*        I_MATNR                 = IT_RS_MARA-MATNR
*        I_WERKS                 = P_WERKS
**       I_PWERK                 =
*        I_PROCNR                = LV_I_PROCNR
*        I_NO_LOEVM              = 'X'
**       I_NO_OLD_PKOSA          = ' '
**       I_POPUP_TITLE           = ' '
**       I_ONLY_PROCESSES        = ' '
**       I_CHECK_PP_DATA         = ' '
**       I_TEXT_NO_PROCESS       = ' '
*      IMPORTING
*        E_PKOSA                 = LV_E_PKOSA
*        E_PROCNR                = LV_E_PROCNR
*      EXCEPTIONS
*        WRONG_INPUT             = 1
*        NONE_PICKED             = 2
*        NO_PROCESS              = 3
*        OTHERS                  = 4.

    CLEAR : IT_L_E_VKKS0, IT_L_E_VKKS0[].
    CALL FUNCTION 'KK_F_PKOSA_FIND'
      EXPORTING
        I_MATNR                     = IT_RS_MARA-MATNR
        I_WERKS                     = P_WERKS
        I_PWERK                     = P_WERKS
*       I_PROCNR                    = ' '
*       I_SA_AUFNR                  = ' '
*       I_FA_AUFNR                  = ' '
*       I_VERID                     = ' '
*       I_STLAN                     = ' '
*       I_STLAL                     = ' '
*       I_PLNTY                     = ' '
*       I_PLNNR                     = ' '
*       I_PLNAL                     = ' '
*       I_DATE                      = '00000000'
*       I_POPUP                     = ' '
*       I_REM                       = ' '
*       I_INCL_LOEKZ                = ' '
*       I_NO_OLD_PKOSA              = ' '
*     IMPORTING
*       E_PROCNR                    =
*       E_VERID                     =
*       E_STLAN                     =
*       E_STLAL                     =
*       E_PLNTY                     =
*       E_PLNNR                     =
*       E_PLNAL                     =
*       E_AUFNR                     =
      TABLES
        E_VKKS0                     = IT_L_E_VKKS0
*       E_PKOSA                     =
      EXCEPTIONS
        NONE_FOUND                  = 1
        WRONG_INPUT                 = 2
        NONE_PICKED                 = 3
        WRONG_RULE                  = 4
        RSH_NOT_VALID               = 5
        WRONG_CHARACTERISTICS       = 6
        NO_RULE                     = 7
        VERSION_NOT_VALID           = 8
        OTHERS                      = 9.

* if No PCC order, Skip the record .
    IF SY-SUBRC <> 0.
      DELETE IT_RS_MARA.
      CONTINUE.
    ENDIF.
    IF IT_L_E_VKKS0[]  IS INITIAL .
      DELETE IT_RS_MARA.
      CONTINUE.
    ENDIF.

    LOOP AT IT_L_E_VKKS0.
* Copying Data
      MOVE-CORRESPONDING IT_RS_MARA TO IT_ORDER_DATA.
*     IT_ORDER_DATA-AUFNR = LV_E_PKOSA.
      IT_ORDER_DATA-AUFNR = IT_L_E_VKKS0-AUFNR.
      IT_ORDER_DATA-OBJNR = IT_L_E_VKKS0-OBJNR.
** Get Object Key
*      CALL FUNCTION 'K_AUFNR_OBJECT_KEY_GET'
*           EXPORTING
*                AUFNR = IT_ORDER_DATA-AUFNR
*                KOKRS = P_KOKRS
*           IMPORTING
*                OBJNR = IT_ORDER_DATA-OBJNR.

* Making ITAB for CO orders
      COLLECT   IT_ORDER_DATA.
      CLEAR     IT_ORDER_DATA.
      CLEAR     IT_L_E_VKKS0.
    ENDLOOP.
    CLEAR     IT_RS_MARA.
  ENDLOOP.

  CLEAR IT_ORDER_DATA.
ENDFORM.                    " READ_CO_ORDER_NO

*&---------------------------------------------------------------------*
*&      Form  MAKING_CCTR_AT_OBJ_KEY
*&---------------------------------------------------------------------*
*       Making OBJ. Key for CCtr + AT (For PAROB/USPOB)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKING_CCTR_AT_OBJ_KEY.

* Making Key Combination : COSS-OBJNR + COSS-PAROB + COSS-USPOB
  DATA : BEGIN OF IT_L_OBJKEY OCCURS 0,
           KOSTL  LIKE CSKS-KOSTL,
           LSTAR  LIKE CSLA-LSTAR,
           OBJKEY LIKE COSS-OBJNR,
         END OF   IT_L_OBJKEY.

* Set OBJECK KEY
* Only for CCtrs which have Depreciation Costs : IT_RES_ANLP
* IF the CCtrs are NOT BOTH in ANLP and in PCC (COSS),
* The CCtrs have no AT quantity or Depreciation Costs.
* So There are no suitable data to be calculated
* The data should be removed .
  CLEAR : IT_L_OBJKEY, IT_L_OBJKEY[].
  LOOP AT IT_RES_ANLP.
    IT_L_OBJKEY-KOSTL = IT_RES_ANLP-KOSTL.
    IT_L_OBJKEY-LSTAR = P_LSTAR.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
         EXPORTING
              KOKRS = P_KOKRS
              KOSTL = IT_L_OBJKEY-KOSTL
              LSTAR = IT_L_OBJKEY-LSTAR
         IMPORTING
              OBJNR = IT_L_OBJKEY-OBJKEY.
    APPEND IT_L_OBJKEY.
    CLEAR  IT_L_OBJKEY.
    CLEAR IT_RES_ANLP.
  ENDLOOP.
  CLEAR IT_L_OBJKEY.

* Making Key Combination to be used in selecting data from COSS
  DATA : IT_L_TMP_OR_DATA LIKE STANDARD TABLE OF  IT_ORDER_DATA
                          WITH HEADER LINE .

  IT_L_TMP_OR_DATA[] = IT_ORDER_DATA[].

  CLEAR : IT_ORDER_DATA, IT_ORDER_DATA[].

  LOOP AT IT_L_TMP_OR_DATA.
    LOOP AT IT_L_OBJKEY.
      CLEAR IT_ORDER_DATA.
      MOVE-CORRESPONDING IT_L_TMP_OR_DATA TO IT_ORDER_DATA.
* PAROB = USPOB.
      IT_ORDER_DATA-PAROB = IT_ORDER_DATA-USPOB
                          = IT_L_OBJKEY-OBJKEY.
      IT_ORDER_DATA-KOSTL = IT_L_OBJKEY-KOSTL.
      IT_ORDER_DATA-LSTAR = IT_L_OBJKEY-LSTAR.
      APPEND IT_ORDER_DATA.
      CLEAR  IT_ORDER_DATA.
      CLEAR  IT_L_OBJKEY.
    ENDLOOP.
    CLEAR IT_L_TMP_OR_DATA.
  ENDLOOP.

  CLEAR IT_ORDER_DATA.
ENDFORM.                    " MAKING_CCTR_AT_OBJ_KEY

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_DATA
*&---------------------------------------------------------------------*
*       Read DATA from PCC with IT_ORDER_DATA.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_PCC_DATA.

  CLEAR : IT_ORDER_DATA.

  CLEAR : IT_COSS, IT_COSS[].

* Selection Condition
* Using Table Key Index
* Value Type : WRTTP = '04' - Actual
* Ledger     : LEDNR = '00' Standard Ledger
* COSS-PAROB = COSS-USPOB

  CLEAR COSS.
  SELECT (IT_ET_FIELDLIST)
           INTO CORRESPONDING FIELDS OF TABLE IT_COSS
           FROM COSS
            FOR ALL ENTRIES IN IT_ORDER_DATA
          WHERE LEDNR = '00'
            AND OBJNR = IT_ORDER_DATA-OBJNR
            AND GJAHR = P_GJAHR
            AND WRTTP = '04'
            AND VERSN = P_VERSN
            AND PAROB = IT_ORDER_DATA-PAROB
            AND USPOB = IT_ORDER_DATA-USPOB.

  CLEAR IT_COSS.

  IF IT_COSS[] IS INITIAL .
    MESSAGE E024 WITH P_VERSN P_GJAHR.
  ENDIF.

* Set OBJECK KEY
* Only for CCtrs which have Depreciation Costs : IT_RES_ANLP
* IF the CCtrs are NOT BOTH in ANLP and in PCC (COSS),
* The CCtrs have no AT quantity or Depreciation Costs.
* So There are no suitable data to be calculated
* The data should be removed .
  LOOP AT IT_COSS.
    CLEAR  IT_ORDER_DATA.
    READ TABLE IT_ORDER_DATA WITH KEY OBJNR = IT_COSS-OBJNR
                                      PAROB = IT_COSS-PAROB
                                      USPOB = IT_COSS-USPOB.
* Moving Additional Information
*   IT_COSS-IZWEK = IT_ORDER_DATA-IZWEK.
    IT_COSS-VEHTP = IT_ORDER_DATA-VEHTP.
    IT_COSS-KOSTL = IT_ORDER_DATA-KOSTL.
    IT_COSS-AUFNR = IT_ORDER_DATA-AUFNR.
    MODIFY IT_COSS.
    CLEAR IT_COSS.
  ENDLOOP.
* Getting rid of unmatched data (CCtr + Vehicle Type)
* The number of key combination : IT_RES_ANLP >= IT_COSS
*  SORT IT_COSS     BY IZWEK KOSTL .
*  SORT IT_RES_ANLP BY IZWEK KOSTL .
  SORT IT_COSS     BY VEHTP KOSTL .
  SORT IT_RES_ANLP BY VEHTP KOSTL .
  LOOP AT IT_RES_ANLP.
    CLEAR IT_COSS.
*    READ TABLE IT_COSS WITH KEY IZWEK = IT_RES_ANLP-IZWEK
*                                KOSTL = IT_RES_ANLP-KOSTL.
    READ TABLE IT_COSS WITH KEY VEHTP = IT_RES_ANLP-VEHTP
                                KOSTL = IT_RES_ANLP-KOSTL.
    IF SY-SUBRC <> 0.
      DELETE IT_RES_ANLP.
    ENDIF.
    CLEAR IT_RES_ANLP.
  ENDLOOP.

  CLEAR IT_RES_ANLP.

ENDFORM.                    " READ_PCC_DATA

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSS
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM READ_FIELD_NAME_FROM_DD_COSS.

  CLEAR : IT_ET_FIELDLIST, IT_ET_FIELDLIST[].

* read DD infor. COSS Key Part
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSS_KEY01'.

* read DD infor. COSS Value Part (Total Quantity)
  PERFORM READ_DD_INFO  TABLES IT_ET_FIELDLIST
                        USING  'ZSCO_COSS_MEG01'.

ENDFORM.                    " READ_FIELD_NAME_FROM_DD_COSS

*&---------------------------------------------------------------------*
*&      Form  READ_DD_INFO
*&---------------------------------------------------------------------*
*       Read DD information
*----------------------------------------------------------------------*
*      -->IT_l_ET_FIELDLIST  Field-List Table
*      -->P_CI_TABNAME       DD name
*----------------------------------------------------------------------*
FORM READ_DD_INFO TABLES   IT_L_ET_FIELDLIST STRUCTURE IT_ET_FIELDLIST
                  USING    P_CI_TABNAME      LIKE GV_CI_TABNAME.
* Local DATA definition
  DATA : IT_L_FDLIST LIKE STANDARD TABLE OF IT_ET_FIELDLIST
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : IT_L_FDLIST,     IT_L_FDLIST[].
  CLEAR GV_CI_TABNAME.
  GV_CI_TABNAME = P_CI_TABNAME.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
       EXPORTING
            IC_TABNAME   = GV_CI_TABNAME
       TABLES
            ET_FIELDLIST = IT_L_FDLIST
       EXCEPTIONS
            NOT_FOUND    = 1
            OTHERS       = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  APPEND LINES OF  IT_L_FDLIST       TO IT_L_ET_FIELDLIST.

ENDFORM.                    " READ_DD_INFO

*&---------------------------------------------------------------------*
*&      Form  CAL_ALC_DEP_COST
*&---------------------------------------------------------------------*
*       Calculating to make an Internal Table (For Report/Posting)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CAL_ALC_DEP_COST.

* Calculating the rate which will be used in POSTing
  PERFORM CAL_ALC_RATE TABLES IT_RATE.

* Making Posting data.
  PERFORM MAKE_IT_POST.

ENDFORM.                    " CAL_ALC_DEP_COST

*&---------------------------------------------------------------------*
*&      Form  CAL_ALC_RATE
*&---------------------------------------------------------------------*
*       Calculating (Rate)
*----------------------------------------------------------------------*
*  -->  IT_L_RATE   :  Rate Internal table (Local)
*  <--  p2
*----------------------------------------------------------------------*
FORM CAL_ALC_RATE TABLES IT_L_RATE STRUCTURE IT_RATE.

* Local DATA definition
  DATA : BEGIN OF IT_L_SUM OCCURS 0,
*          IZWEK   LIKE AUFK-IZWEK,
          VEHTP   LIKE ZTCO_VEHI_TYPE-VEHTP,
          KOSTL   LIKE ANLP-KOSTL,
          MEGXX   LIKE COSS-MEG001,
         END OF IT_L_SUM.

  FIELD-SYMBOLS: <FS1> TYPE ANY.
  DATA : LV_VALNAME(30).

  CLEAR : IT_RES_ANLP.
  CLEAR : IT_COSS.
  CLEAR : IT_L_RATE, IT_L_RATE[].

* Set field name as Period
  CLEAR LV_VALNAME.
  CONCATENATE 'IT_COSS-' 'MEG' P_PERAF INTO LV_VALNAME.
  ASSIGN (LV_VALNAME) TO <FS1>.

* Adding the Quantity from PCC
  LOOP AT   IT_COSS.
* INDV.
    CLEAR IT_L_RATE.
    MOVE-CORRESPONDING  IT_COSS TO IT_L_RATE.
    IT_L_RATE-MEGXX = <FS1>.
    COLLECT IT_L_RATE.
* SUM
    CLEAR IT_L_SUM.
    MOVE-CORRESPONDING  IT_L_RATE TO IT_L_SUM.
    COLLECT IT_L_SUM.
    CLEAR : IT_COSS, IT_L_RATE, IT_L_SUM.
  ENDLOOP.

* RATE
* Rate = [Vehicle Type + CCtr + PCC Order]  /  SUM[Vehicle Type + CCtr]
  CLEAR IT_L_RATE.
  LOOP AT IT_L_RATE.
    CLEAR IT_L_SUM.
*    READ TABLE IT_L_SUM WITH KEY IZWEK = IT_L_RATE-IZWEK
*                                 KOSTL = IT_L_RATE-KOSTL.
    READ TABLE IT_L_SUM WITH KEY VEHTP = IT_L_RATE-VEHTP
                                 KOSTL = IT_L_RATE-KOSTL.
* Cal. rate
    IF IT_L_SUM-MEGXX IS INITIAL.
      CLEAR IT_L_RATE-RATE.
    ELSE.
      IT_L_RATE-RATE = IT_L_RATE-MEGXX / IT_L_SUM-MEGXX.
    ENDIF.

    MODIFY IT_L_RATE.
    CLEAR IT_L_RATE.
  ENDLOOP.

ENDFORM.                    " CAL_ALC_RATE

*&---------------------------------------------------------------------*
*&      Form  MAKE_IT_POST
*&---------------------------------------------------------------------*
*       For Posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MAKE_IT_POST.

** Making IT_POST
* Sorting
*  SORT IT_RATE     BY IZWEK KOSTL AUFNR.
*  SORT IT_RES_ANLP BY IZWEK KOSTL KTNAFG.
  SORT IT_RATE     BY VEHTP KOSTL AUFNR.
  SORT IT_RES_ANLP BY VEHTP KOSTL KTNAFG.

  CLEAR : IT_POST, IT_POST[].

  LOOP AT IT_RES_ANLP.
*    LOOP AT IT_RATE  WHERE IZWEK = IT_RES_ANLP-IZWEK
*                       AND KOSTL = IT_RES_ANLP-KOSTL.
    LOOP AT IT_RATE  WHERE VEHTP = IT_RES_ANLP-VEHTP
                       AND KOSTL = IT_RES_ANLP-KOSTL.
* Copying Depreciation Cost (Original)
      CLEAR IT_POST.
      MOVE-CORRESPONDING  IT_RES_ANLP TO IT_POST.
* Read rate and quantity
      MOVE-CORRESPONDING  IT_RATE     TO IT_POST.
* Collecting
      COLLECT IT_POST.
      CLEAR   IT_POST.
      CLEAR   IT_RATE.
    ENDLOOP.
    CLEAR IT_RES_ANLP.
  ENDLOOP.

** Adjusting Amounts to be posted (IT_POST).
*  SORT   IT_POST
*    BY   IZWEK   KOSTL.
  SORT   IT_POST
    BY   VEHTP   KOSTL.

  LOOP AT IT_POST .
* Calculating Changed Amount
    IT_POST-CHG_NAFAZ = ABS( IT_POST-NAFAZ * IT_POST-RATE ).
* Modification
    MODIFY IT_POST.
* Remove the record if Rate is initial or CHG_NAFAZ is initial.
* There will be no record to be posted.
    IF IT_POST-CHG_NAFAZ IS INITIAL .
      DELETE IT_POST .
    ENDIF.
    CLEAR  IT_POST.
  ENDLOOP.

ENDFORM.                    " MAKE_IT_POST
