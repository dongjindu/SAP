*----------------------------------------------------------------------*
*   INCLUDE ZACO05L_F001                                               *
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR_GROUP
*&---------------------------------------------------------------------*
*       Read CCtr Group (Search Help)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr_group.
  CALL FUNCTION 'K_GROUP_SELECT'
    EXPORTING
*     BUTTONS            = 'X'
      class              = '0101'
*     CRUSER             = '*'
      field_name         = space
*     SEARCHFLD          = '    '
*     SEARCHFLD_INPUT    = 'X'
      searchfld_required = ' '
*     SET                = GV_CCGR_SETID
*     START_COLUMN       = 10
*     START_ROW          = 5
*     TABLE              = 'CCSS'
*     TYPELIST           = 'BS'
*     UPDUSER            = '*'
*     KOKRS              =
*     KTOPL              =
    IMPORTING
*     CLASS_NAME         =
      set_name           = p_ncoal
*     SET_TITLE          =
*     TABLE_NAME         =
*     SETID              =
    EXCEPTIONS
      no_set_picked      = 1
      OTHERS             = 2.

* No error check for F4  SH
  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " READ_CCTR_GROUP

*&---------------------------------------------------------------------*
*&      Form  Read_TKA01
*&---------------------------------------------------------------------*
*       Controlling Area Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_tka01.
  CLEAR tka01.
  SELECT SINGLE * FROM tka01
                 WHERE kokrs = p_kokrs.
  IF sy-subrc <> 0.
    MESSAGE e038 WITH p_kokrs.
  ENDIF.
ENDFORM.                    " Read_TKA01

*&---------------------------------------------------------------------*
*&      Form  READ_CCTR
*&---------------------------------------------------------------------*
*       Read CCtrs for retrieval.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_cctr.
* Making an internal table for CCtr to select data
  DATA : lv_datum LIKE sy-datum.
  CONCATENATE p_gjahr p_frper+1(2) '01' INTO lv_datum.

  CLEAR : it_costcenterlist, it_costcenterlist[],
          it_return,         it_return[].

  CALL FUNCTION 'BAPI_COSTCENTER_GETLIST1'
    EXPORTING
      controllingarea = p_kokrs
      date_from       = lv_datum
      costcentergroup = p_ncoal
    TABLES
      costcenterlist  = it_costcenterlist
      return          = it_return.
* Message
  PERFORM dis_bapi_message.

ENDFORM.                    " READ_CCTR

*&---------------------------------------------------------------------*
*&      Form  DIS_BAPI_MESSAGE
*&---------------------------------------------------------------------*
*       Display BAPI Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_bapi_message.
  IF NOT it_return[] IS INITIAL.
    LOOP AT   it_return.
      MESSAGE ID     it_return-id
              TYPE   it_return-type
              NUMBER it_return-number
              WITH   it_return-message_v1
                     it_return-message_v2
                     it_return-message_v3
                     it_return-message_v4.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " DIS_BAPI_MESSAGE

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_NMHHRTRANS
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue_ztco_nmhhrtrans.

  DATA : lv_perid LIKE ztco_nmhhrtrans-perid.

  lv_perid = p_frper.

  DO 16 TIMES .
    IF lv_perid =< p_toper.
      CALL FUNCTION 'ENQUEUE_EZ_ZTCO_NMHHRTR'
        EXPORTING
          mode_ztco_nmhhrtrans = 'E'
          mandt                = sy-mandt
          gjahr                = p_gjahr
          perid                = lv_perid
*         KOSTL                =
*         LSTAR                =
*         X_GJAHR              = ' '
*         X_PERID              = ' '
*         X_KOSTL              = ' '
*         X_LSTAR              = ' '
          _scope               = '3'
*         _WAIT                = ' '
*         _COLLECT             = ' '
        EXCEPTIONS
          foreign_lock         = 1
          system_failure       = 2
          OTHERS               = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
* Period Counting
    lv_perid = lv_perid  + 1.

  ENDDO.
ENDFORM.                    " ENQUEUE_ZTCO_NMHHRTRANS

*&---------------------------------------------------------------------*
*&      Form  READ_FR_ZTCO_NMHHRTRANS
*&---------------------------------------------------------------------*
*       read data from ZTCO_NMHHRTRANS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_ztco_nmhhrtrans.

  CLEAR : it_ztco_nmhhrtrans, it_ztco_nmhhrtrans[].

  CLEAR ztco_nmhhrtrans.
  SELECT    gjahr
            perid
            kostl
            lstar
            vaeqty
            unit
           FROM ztco_nmhhrtrans
           INTO CORRESPONDING FIELDS OF TABLE it_ztco_nmhhrtrans
           FOR  ALL ENTRIES IN it_costcenterlist
           WHERE gjahr = p_gjahr
             AND perid BETWEEN p_frper AND p_toper
             AND kostl = it_costcenterlist-costcenter
             AND lstar = p_lstar
             AND actqty NE space
             AND curqty NE space
             AND vaeqty NE space.
* Actual M/H <> '0' and Current M/H <> '0' and Variance M/H <> '0'.
* request on 2003.10.27

  CLEAR   it_ztco_nmhhrtrans.

  IF it_ztco_nmhhrtrans[] IS INITIAL.
    MESSAGE e047.
  ENDIF.

ENDFORM.                    " READ_FR_ZTCO_NMHHRTRANS

*&---------------------------------------------------------------------*
*&      Form  CHECK_PERIOD_RANGE
*&---------------------------------------------------------------------*
*       Check Period range
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_period_range.
*NO-DISPLAY / Only One period
  p_toper = p_frper.
*  IF P_FRPER > P_TOPER.
*    MESSAGE E031.
*  ENDIF.
ENDFORM.                    " CHECK_PERIOD_RANGE

*&---------------------------------------------------------------------*
*&      Form  READ_FR_MARA_MARC
*&---------------------------------------------------------------------*
*       Read Matnr / REM indicator
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_fr_mara_marc.
*// Mod. By Hyung Jin Youn 2004.02.13
* include REM profile
  CLEAR : it_marc, it_marc[].
  SELECT       mara~mtart
               marc~matnr marc~werks
               marc~sauft marc~sfepr
          INTO CORRESPONDING FIELDS OF TABLE it_marc
          FROM mara INNER JOIN marc
            ON mara~matnr = marc~matnr
          WHERE mtart IN s_mtart.
*// End of Mod.
  CLEAR it_marc.
ENDFORM.                    " READ_FR_MARA_MARC

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_ORDER
*&---------------------------------------------------------------------*
*       Read PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_pcc_order.
* PCC order can not be created without material master data .
* So it is right way to look up PCC order with material data.
  DATA : it_l_e_vkks0	LIKE TABLE OF vkks0
                        WITH HEADER LINE.
* Clear
  CLEAR   it_marc.
  CLEAR : it_ma_obj, it_ma_obj[].

  LOOP AT it_marc.
    CLEAR : it_l_e_vkks0, it_l_e_vkks0[].
    CALL FUNCTION 'KK_F_PKOSA_FIND'
      EXPORTING
        i_matnr                     = it_marc-matnr
        i_werks                     = it_marc-werks
        i_pwerk                     = it_marc-werks
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
*       I_NO_OLD_PKOSA              = 'X'
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
        e_vkks0                     = it_l_e_vkks0
*       E_PKOSA                     =
      EXCEPTIONS
        none_found                  = 1
        wrong_input                 = 2
        none_picked                 = 3
        wrong_rule                  = 4
        rsh_not_valid               = 5
        wrong_characteristics       = 6
        no_rule                     = 7
        version_not_valid           = 8
        OTHERS                      = 9.

* if No PCC order, Skip the record .
    IF sy-subrc <> 0.
      DELETE it_marc.
      CONTINUE.
    ENDIF.
    IF it_l_e_vkks0[]  IS INITIAL .
      DELETE it_marc.
      CONTINUE.
    ENDIF.

    LOOP AT it_l_e_vkks0.
* Copying Data
      MOVE-CORRESPONDING it_marc TO it_ma_obj.
      it_ma_obj-aufnr = it_l_e_vkks0-aufnr.
      it_ma_obj-objnr = it_l_e_vkks0-objnr.

* Read Production Version  - PROCNR
      CALL FUNCTION 'KK_F_PKOSA_FIND'
        EXPORTING
          i_matnr               = it_marc-matnr
          i_werks               = it_marc-werks
          i_pwerk               = it_marc-werks
          i_procnr              = it_l_e_vkks0-procnr
*         I_NO_OLD_PKOSA        = 'X'
        IMPORTING
          e_verid               = it_ma_obj-verid
        EXCEPTIONS
          none_found            = 1
          wrong_input           = 2
          none_picked           = 3
          wrong_rule            = 4
          rsh_not_valid         = 5
          wrong_characteristics = 6
          no_rule               = 7
          version_not_valid     = 8
          OTHERS                = 9.
* Also the Prod. version should be valid
      CLEAR mkal.
      SELECT SINGLE *  FROM mkal
                      WHERE matnr = it_marc-matnr
                        AND werks = it_marc-werks
                        AND verid = it_ma_obj-verid.

      IF sy-subrc <> 0.
* No assigned Production Version -> MAX Production Version
* request By (Functional Member 2004.02.17)
* Read Max - Production Version
        CLEAR mkal.
        SELECT SINGLE MAX( verid )
                         INTO it_ma_obj-verid
                         FROM mkal
                        WHERE matnr = it_marc-matnr
                          AND werks = it_marc-werks.
      ENDIF.

* Making ITAB for CO orders
      COLLECT   it_ma_obj.
      CLEAR     it_ma_obj.
      CLEAR     it_l_e_vkks0.
    ENDLOOP.
    CLEAR     it_marc.
  ENDLOOP.

  SORT  it_ma_obj BY matnr werks.
  CLEAR it_ma_obj.

* Rid off PCC without Receipt Qty.
  LOOP AT it_ma_obj.
    CLEAR ckmlhd.
    SELECT SINGLE * FROM ckmlhd
                   WHERE matnr = it_ma_obj-matnr
                     AND bwkey = it_ma_obj-werks.
    CLEAR mlcd.
    SELECT SINGLE * FROM mlcd
                   WHERE kalnr = ckmlhd-kalnr
                     AND bdatj = p_gjahr
                     AND poper = p_frper
                     AND categ = 'ZU'
                     AND lbkum NE space.

    IF sy-subrc = 0.
    ELSE.
      DELETE it_ma_obj.
    ENDIF.
    CLEAR it_ma_obj.
  ENDLOOP.

ENDFORM.                    " READ_PCC_ORDER

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_PCC
*&---------------------------------------------------------------------*
*       Read DATA from Product Cost Collector
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_pcc.
* CO order number is unique each Material Code.
* Under the Item level, there can be more than one records
* In CO view point, Only the Order number level is considered.

* PCC orders in HMMA are created at Plant Level not production level
* So only one PCC order is created each Material Code.
* (2003.10.14)

* Making OBJ. Key for CCtr + AT (For COSS-PAROB/USPOB)
  PERFORM making_cctr_at_obj_key.

* Read Dynamic Fields Name
  PERFORM read_field_name_from_dd_coss.

* Read DATA from PCC
  PERFORM read_pcc_data.

* Put In the information about Material Code
  PERFORM put_mat_info.

* Re-orginize by period
  PERFORM re_org_by_per.

ENDFORM.                    " READ_DATA_PCC

*&---------------------------------------------------------------------*
*&      Form  MAKING_CCTR_AT_OBJ_KEY
*&---------------------------------------------------------------------*
*       Make object key from CCtr + AT
*       Build object key combination with PCC order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM making_cctr_at_obj_key.
* Making Key Combination : COSS-OBJNR + COSS-PAROB + COSS-USPOB
  DATA : BEGIN OF it_l_objkey OCCURS 0,
           kostl  LIKE csks-kostl,
           lstar  LIKE csla-lstar,
           objnr  LIKE coss-objnr,
         END OF   it_l_objkey.

  CLEAR   it_ztco_nmhhrtrans.
  CLEAR : it_l_objkey, it_l_objkey[].

* Only CCtrs with Variance Quantity in the table 'ZTCO_NMHHRTRANS'
  SORT it_ztco_nmhhrtrans BY kostl.
  LOOP AT it_ztco_nmhhrtrans.
    ON CHANGE OF it_ztco_nmhhrtrans-kostl.
      MOVE-CORRESPONDING  it_ztco_nmhhrtrans
                      TO  it_l_objkey.
      COLLECT it_l_objkey.
      CLEAR   it_l_objkey.
    ENDON.
    CLEAR it_ztco_nmhhrtrans.
  ENDLOOP.

* Get Object Key
  LOOP AT it_l_objkey.
    CALL FUNCTION 'K_LSTAR_OBJECT_KEY_GET'
      EXPORTING
        kokrs = p_kokrs
        kostl = it_l_objkey-kostl
        lstar = it_l_objkey-lstar
      IMPORTING
        objnr = it_l_objkey-objnr.
    MODIFY it_l_objkey.
    CLEAR  it_l_objkey.
  ENDLOOP.
  CLEAR   it_l_objkey.

* Making Key Combination to be used in selecting data from COSS
  DATA : it_l_tmp_obj     LIKE STANDARD TABLE OF  it_ma_obj
                          WITH HEADER LINE .

  it_l_tmp_obj[] = it_ma_obj[].

  CLEAR : it_ma_obj, it_ma_obj[].

  LOOP AT it_l_tmp_obj.
    LOOP AT it_l_objkey.
      CLEAR it_ma_obj.
      MOVE-CORRESPONDING it_l_tmp_obj TO it_ma_obj.
* PAROB = USPOB.
      it_ma_obj-parob = it_ma_obj-uspob
                      = it_l_objkey-objnr.
      it_ma_obj-kostl = it_l_objkey-kostl.
      it_ma_obj-lstar = it_l_objkey-lstar.
      APPEND it_ma_obj.
      CLEAR  it_ma_obj.
      CLEAR  it_l_objkey.
    ENDLOOP.
    CLEAR it_l_tmp_obj.
  ENDLOOP.

  CLEAR   it_ma_obj.
  CLEAR : it_l_tmp_obj, it_l_tmp_obj[]. FREE it_l_tmp_obj.

ENDFORM.                    " MAKING_CCTR_AT_OBJ_KEY

*&---------------------------------------------------------------------*
*&      Form  READ_FIELD_NAME_FROM_DD_COSS
*&---------------------------------------------------------------------*
*       Read Technical FieldName for COSS
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_field_name_from_dd_coss.

  CLEAR : it_et_fieldlist, it_et_fieldlist[].

* read DD infor. COSS Key Part
  PERFORM read_dd_info  TABLES it_et_fieldlist
                        USING  'ZSCO_COSS_KEY01'.

* read DD infor. COSS Value Part (Total Quantity)
  PERFORM read_dd_info  TABLES it_et_fieldlist
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
FORM read_dd_info TABLES   it_l_et_fieldlist STRUCTURE it_et_fieldlist
                  USING    p_ci_tabname      LIKE gv_ci_tabname.
* Local DATA definition
  DATA : it_l_fdlist LIKE STANDARD TABLE OF it_et_fieldlist
                     WITH HEADER LINE.
* Making FDlist
  CLEAR : it_l_fdlist,     it_l_fdlist[].
  CLEAR gv_ci_tabname.
  gv_ci_tabname = p_ci_tabname.
  CALL FUNCTION 'RECP_DD_TABL_FIELDNAMES_GET'
    EXPORTING
      ic_tabname   = gv_ci_tabname
    TABLES
      et_fieldlist = it_l_fdlist
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  APPEND LINES OF  it_l_fdlist       TO it_l_et_fieldlist.

ENDFORM.                    " READ_DD_INFO

*&---------------------------------------------------------------------*
*&      Form  READ_PCC_DATA
*&---------------------------------------------------------------------*
*       Read PCC data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_pcc_data.

  CLEAR : it_ma_obj.

  CLEAR : it_coss, it_coss[].

* Selection Condition
* Using Table Key Index
* Value Type : WRTTP = '04' - Actual / User input
* Ledger     : LEDNR = '00' Standard Ledger
* COSS-PAROB = COSS-USPOB

  CLEAR coss.
  SELECT (it_et_fieldlist)
           INTO CORRESPONDING FIELDS OF TABLE it_coss
           FROM coss
            FOR ALL ENTRIES IN it_ma_obj
          WHERE lednr = '00'
            AND objnr = it_ma_obj-objnr
            AND gjahr = p_gjahr
            AND wrttp = p_wrttp
            AND versn = p_versn
            AND parob = it_ma_obj-parob
            AND uspob = it_ma_obj-uspob
            AND vrgng IN s_vrgng.

*- U1 Start
  IF p_arch EQ 'X'.
    PERFORM archive_read_coss.
  ENDIF.
*- U1 End

  CLEAR it_coss.

  IF it_coss[] IS INITIAL .
    MESSAGE e024 WITH p_versn p_gjahr.
  ENDIF.

ENDFORM.                    " READ_PCC_DATA

*&---------------------------------------------------------------------*
*&      Form  CAL_PER_COUNT
*&---------------------------------------------------------------------*
*       Calculation STD. - period Counter
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_per_count.
* Cal. the Counter
  gv_percount = p_toper - p_frper + 1.

ENDFORM.                    " CAL_PER_COUNT

*&---------------------------------------------------------------------*
*&      Form  PUT_MAT_INFO
*&---------------------------------------------------------------------*
*       set Material  Information
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM put_mat_info.
* Sorting
  CLEAR it_coss.
  SORT it_ma_obj BY objnr  parob.
  SORT it_coss   BY objnr  parob.
  LOOP AT it_coss.
    CLEAR it_ma_obj.
    READ TABLE it_ma_obj
                        WITH KEY objnr = it_coss-objnr
                                 parob = it_coss-parob
                        BINARY SEARCH.
* Transfer CO objects
    it_coss-kostl = it_ma_obj-kostl.
    it_coss-lstar = it_ma_obj-lstar.
    it_coss-aufnr = it_ma_obj-aufnr.
* Transfer Material Data
    it_coss-matnr = it_ma_obj-matnr.
    it_coss-mtart = it_ma_obj-mtart.
    it_coss-werks = it_ma_obj-werks.
    it_coss-sauft = it_ma_obj-sauft.
    MODIFY it_coss.
    CLEAR it_coss.
  ENDLOOP.

ENDFORM.                    " PUT_MAT_INFO

*&---------------------------------------------------------------------*
*&      Form  RE_ORG_BY_PER
*&---------------------------------------------------------------------*
*       Re-Orginize data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM re_org_by_per.

  CLEAR it_coss.
  CLEAR : it_col_pcc, it_col_pcc[].

  FIELD-SYMBOLS: <fs1> TYPE any.
  DATA : lv_coss_meg(30).  " VALUE 'IT_COSS-'.
  DATA : lv_cnt  LIKE  coss-perbl.

  LOOP AT it_coss.
* Key Part
    it_col_pcc-aufnr =   it_coss-aufnr.
    it_col_pcc-kostl =   it_coss-kostl.
    it_col_pcc-lstar =   it_coss-lstar.
* Unit
    it_col_pcc-meinh =   it_coss-meinh.
* Period Counter : Set From-Period .
    CLEAR lv_cnt.
    lv_cnt = p_frper .
* Value Part
    DO gv_percount TIMES.
* Period
      it_col_pcc-perid = lv_cnt.
* Value
      CLEAR lv_coss_meg.
      CONCATENATE 'IT_COSS-'  'MEG'   lv_cnt
             INTO lv_coss_meg.
      ASSIGN (lv_coss_meg) TO <fs1>.
      it_col_pcc-megxxx  = <fs1>.
* Collect
      COLLECT it_col_pcc.
* Period Counter
      lv_cnt = lv_cnt + 1.
    ENDDO.
    CLEAR it_coss.
    CLEAR it_col_pcc.
  ENDLOOP.

  CLEAR   it_col_pcc.
* Remove records if qunatity value has initial value.
  DELETE  it_col_pcc WHERE megxxx EQ space.

ENDFORM.                    " RE_ORG_BY_PER

*&---------------------------------------------------------------------*
*&      Form  CAL_QUAN_RATIO
*&---------------------------------------------------------------------*
*       Calculate Quantity Ratio
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_quan_ratio.
* In case that there is no COSS quantity . there will be no ratio data
* At that case, The variance  AT Quantity should not be distributed
* (Requested By functional Member 2003.10.16)

* AT Quantity In COSS   : IT_COL_PCC
* Varaince AT Quantity  : IT_ZTCO_NMHHRTRANS
  LOOP AT it_col_pcc.
    CLEAR it_ztco_nmhhrtrans.
    READ TABLE it_ztco_nmhhrtrans WITH KEY perid = it_col_pcc-perid
                                          kostl = it_col_pcc-kostl
                                          lstar = it_col_pcc-lstar.
    IF sy-subrc = 0.
      it_col_pcc-vaeqty = it_ztco_nmhhrtrans-vaeqty.
      it_col_pcc-unit   = it_ztco_nmhhrtrans-unit.
    ELSE. " only Unit def
      it_col_pcc-unit   = 'STD'.
    ENDIF.
    MODIFY it_col_pcc.
    CLEAR  it_col_pcc.
  ENDLOOP.
  CLEAR it_col_pcc.

* Unit Conversion / total.
  DATA : BEGIN OF it_l_total OCCURS 0,
   perid LIKE it_col_pcc-perid,
   kostl LIKE it_col_pcc-kostl,
   lstar LIKE it_col_pcc-lstar,
   tomeg LIKE it_col_pcc-tomeg,
   meinh LIKE it_col_pcc-meinh.
  DATA : END OF it_l_total.

  SORT it_col_pcc BY perid  kostl  lstar.

  LOOP AT it_col_pcc.
* Unit Conversion
* Already AT 'STD' is used in  IT_ZTCO_NMHHRTRANS-UNIT
* Check Previous program -> ZACO03U_MHAM
    IF  it_col_pcc-meinh <> it_col_pcc-unit.
      PERFORM unit_conv USING it_col_pcc-meinh
                              it_col_pcc-unit
                              it_col_pcc-megxxx .
      MODIFY it_col_pcc.
    ENDIF.
* Total
    MOVE-CORRESPONDING it_col_pcc TO it_l_total.
*---start #1 wskim 03/09/05
*    it_l_total-tomeg = it_col_pcc-megxxx.
*     COLLECT  it_l_total.
    IF it_col_pcc-megxxx >= 0.
      it_l_total-tomeg = it_col_pcc-megxxx.
      COLLECT  it_l_total.
      CLEAR    it_l_total.
    ENDIF.
*---end
    CLEAR it_col_pcc.
  ENDLOOP.
  CLEAR    it_l_total.

* Calculate Ratio / Output Quantity
  LOOP AT it_col_pcc.
    CLEAR    it_l_total.
    READ TABLE it_l_total WITH KEY    perid = it_col_pcc-perid
                                      kostl = it_col_pcc-kostl
                                      lstar = it_col_pcc-lstar.
    it_col_pcc-tomeg         = it_l_total-tomeg.
*---Start#1 wskim 03/09/2005:wip error - don't allowed '-' quantity
*   IT_COL_PCC-RATE_%        = IT_COL_PCC-MEGXXX / IT_COL_PCC-TOMEG.
    IF it_col_pcc-megxxx =< 0.
      it_col_pcc-rate_%  = 0.
    ELSE.
      it_col_pcc-rate_%  = it_col_pcc-megxxx / it_col_pcc-tomeg.
    ENDIF.
*---end
    it_col_pcc-megxxx_rate_% = it_col_pcc-rate_% * it_col_pcc-vaeqty.
    MODIFY it_col_pcc.
    CLEAR  it_col_pcc.
  ENDLOOP.
  CLEAR  it_col_pcc.
* No Value , remove it
  DELETE it_col_pcc WHERE megxxx_rate_% EQ space.

ENDFORM.                    " CAL_QUAN_RATIO

*&---------------------------------------------------------------------*
*&      Form  UNIT_CONV
*&---------------------------------------------------------------------*
*       Unit Conversion
*----------------------------------------------------------------------*
*      -->P_UNIT     UNIT
*      -->P_OUTUNIT  Unit For Output
*      -->P_QTY      Quantity
*----------------------------------------------------------------------*
FORM unit_conv USING    p_unit
                        p_outunit
                        p_qty.
  CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
    EXPORTING
      input                = p_qty
*     NO_TYPE_CHECK        = 'X'
*     ROUND_SIGN           = ' '
      unit_in              = p_unit
      unit_out             = p_outunit
    IMPORTING
*     ADD_CONST            =
*     DECIMALS             =
*     DENOMINATOR          =
*     NUMERATOR            =
      output               = p_qty
    EXCEPTIONS
      conversion_not_found = 1
      division_by_zero     = 2
      input_invalid        = 3
      output_invalid       = 4
      overflow             = 5
      type_invalid         = 6
      units_missing        = 7
      unit_in_not_found    = 8
      unit_out_not_found   = 9
      OTHERS               = 10.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_unit = p_outunit.

ENDFORM.                    " UNIT_CONV

*&---------------------------------------------------------------------*
*&      Form  TRANS_IT_ZTCO_nMHPCPOST
*&---------------------------------------------------------------------*
*       Transferring Key Part
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trans_it_ztco_nmhpcpost.

*        PERID   LIKE RKU01G-PERBI, "Period
*        AUFNR   LIKE AFPO-AUFNR,
*        KOSTL   LIKE ANLP-KOSTL,
*        LSTAR   LIKE CSLA-LSTAR,
*
*        MEINH   LIKE COSS-MEINH,
*        MEGXXX_RATE_%
*            RATE_%  TYPE P DECIMALS 6 ,

  CLEAR : it_ztco_nmhpcpost, it_ztco_nmhpcpost[].

  SORT it_ma_obj BY aufnr.

  LOOP AT it_col_pcc.
    it_ztco_nmhpcpost-gjahr   = p_gjahr.
    it_ztco_nmhpcpost-perid   = it_col_pcc-perid.
    it_ztco_nmhpcpost-aufnr   = it_col_pcc-aufnr.
    it_ztco_nmhpcpost-kostl   = it_col_pcc-kostl.
    it_ztco_nmhpcpost-lstar   = it_col_pcc-lstar.
    it_ztco_nmhpcpost-meinh   = it_col_pcc-unit.
    it_ztco_nmhpcpost-varquan = it_col_pcc-megxxx_rate_%.
    it_ztco_nmhpcpost-rate    = it_col_pcc-rate_%.
    it_ztco_nmhpcpost-megxxx  = it_col_pcc-megxxx.
    CLEAR it_ma_obj.
    READ TABLE it_ma_obj WITH KEY aufnr = it_ztco_nmhpcpost-aufnr.
    it_ztco_nmhpcpost-matnr   = it_ma_obj-matnr.
    it_ztco_nmhpcpost-werks   = it_ma_obj-werks.
    it_ztco_nmhpcpost-sauft   = it_ma_obj-sauft.
*// Mod. By Hyung Jin Youn 2004.02.13
* include REM profile
    it_ztco_nmhpcpost-sfepr   = it_ma_obj-sfepr.
    it_ztco_nmhpcpost-verid   = it_ma_obj-verid.
*// End of Mod.
    APPEND it_ztco_nmhpcpost.
    CLEAR  it_ztco_nmhpcpost.
    CLEAR  it_col_pcc.
  ENDLOOP.

  CLEAR it_ztco_nmhpcpost.
ENDFORM.                    " TRANS_IT_ZTCO_nMHPCPOST

*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZTCO_nMHPCPOST
*&---------------------------------------------------------------------*
*       Update Table
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_ztco_nmhpcpost.
* delete ZTCO_NMHHRTRANS
  DELETE FROM ztco_nmhpcpost WHERE gjahr =       p_gjahr
                               AND perid BETWEEN p_frper AND p_toper.

  LOOP AT it_ztco_nmhpcpost.
    CLEAR ztco_nmhpcpost.
    MOVE-CORRESPONDING it_ztco_nmhpcpost TO ztco_nmhpcpost.
* Get Number (MH Doc)
    PERFORM get_number_from_sap.
* Inserting Log
    ztco_nmhpcpost-erdat = sy-datum.
    ztco_nmhpcpost-erzet = sy-uzeit.
    ztco_nmhpcpost-ernam = sy-uname.
* Insertion
    INSERT ztco_nmhpcpost.
    IF sy-subrc <> 0.
      ROLLBACK  WORK.
      MESSAGE e045 WITH 'ZTCO_NMHPCPOST' ztco_nmhpcpost-gjahr
                        it_ztco_nmhpcpost-perid .
    ENDIF.
    CLEAR it_ztco_nmhpcpost.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.                    " UPDATE_ZTCO_nMHPCPOST

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_nMHPCPOST
*&---------------------------------------------------------------------*
*       Enqueue ZTCO_nMHPCPOST
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue_ztco_nmhpcpost.

  DATA : lv_perid LIKE ztco_nmhhrtrans-perid.

  lv_perid = p_frper.

  DO 16 TIMES .
    IF lv_perid =< p_toper.
      CALL FUNCTION 'ENQUEUE_EZCO_NMHPCPOST'
        EXPORTING
          mode_ztco_nmhpcpost = 'E'
          mandt               = sy-mandt
          gjahr               = p_gjahr
          perid               = lv_perid
*         MATNR               =
*         WERKS               =
*         AUFNR               =
*         KOSTL               =
*         LSTAR               =
*         X_GJAHR             = ' '
*         X_PERID             = ' '
*         X_MATNR             = ' '
*         X_WERKS             = ' '
*         X_AUFNR             = ' '
*         X_KOSTL             = ' '
*         X_LSTAR             = ' '
          _scope              = '3'
*         _WAIT               = ' '
*         _COLLECT            = ' '
        EXCEPTIONS
          foreign_lock        = 1
          system_failure      = 2
          OTHERS              = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
* Period Counting
    lv_perid = lv_perid  + 1.

  ENDDO.

ENDFORM.                    " ENQUEUE_ZTCO_nMHPCPOST

*&---------------------------------------------------------------------*
*&      Form  GET_NUMBER_FROM_SAP
*&---------------------------------------------------------------------*
*       Get Number
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_number_from_sap.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'ZCO_NR_MHD'
*     QUANTITY                = '1'
*     SUBOBJECT               = ' '
*     TOYEAR                  = '0000'
*     IGNORE_BUFFER           = ' '
    IMPORTING
      number                  = ztco_nmhpcpost-mhdoc
*     QUANTITY                =
*     RETURNCODE              =
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " GET_NUMBER_FROM_SAP

*&---------------------------------------------------------------------*
*&      Form  POST_PROCESS
*&---------------------------------------------------------------------*
*       Posting
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM post_process.

* Check remained not-posted records .
  PERFORM check_not_post_rc.

  IF gv_new EQ space.
    MESSAGE i000 WITH 'Old datas will be deleted.'.
  ENDIF.

* Read data from ZTCO_NMHHRTRANS
  PERFORM read_fr_ztco_nmhhrtrans.
* Read Material Data
  PERFORM read_fr_mara_marc.
* Read PCC order
  PERFORM read_pcc_order.
* Read Data from PCC
  PERFORM read_data_pcc.
* Calculte the ratio
  PERFORM cal_quan_ratio.
* Transfer data to IT_ZTCO_nMHPCPOST
  PERFORM trans_it_ztco_nmhpcpost.
* Insert  ZTCO_nMHPCPOST.
  PERFORM update_ztco_nmhpcpost.

** POSTING PART <- Important !!
*  PERFORM posting_using_pp_pgm.
*
** Put Results into IT_ZTCO_nMHPCPOST
** from IT_PO_POST & IT_REM_POST & IT_DI_POST
*  PERFORM put_result_into_it_ztco_mhpcpo.


ENDFORM.                    " POST_PROCESS

*&---------------------------------------------------------------------*
*&      Form  REVERSE_PROCESS
*&---------------------------------------------------------------------*
*       Reversing
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM reverse_process.

* Read data to be reversed
  PERFORM read_data_to_reverse.

* Reverse .
*  PERFORM reverse_act_mto_mts.

ENDFORM.                    " REVERSE_PROCESS

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_TO_REVERSE
*&---------------------------------------------------------------------*
*       Read data to be reversed
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_to_reverse.

* Renewal
  CLEAR : it_ztco_nmhpcpost, it_ztco_nmhpcpost[].
* read data : REVERSED = SPACE
  CLEAR ztco_nmhpcpost.
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_nmhpcpost
           FROM ztco_nmhpcpost
          WHERE gjahr = p_gjahr
            AND perid BETWEEN p_frper AND p_toper
            AND reversed = space.

  IF it_ztco_nmhpcpost[] IS INITIAL.
    MESSAGE e054 WITH 'ZTCO_NMHPCPOST' p_gjahr p_frper p_toper.
  ENDIF.

  LOOP AT it_ztco_nmhpcpost.
* Update LOG
    CLEAR: ztco_nmhpcpost.
    MOVE: it_ztco_nmhpcpost TO ztco_nmhpcpost.
    ztco_nmhpcpost-reversed = 'X'.
    ztco_nmhpcpost-aedat    = sy-datum.
    ztco_nmhpcpost-aezet    = sy-uzeit.
    ztco_nmhpcpost-aenam    = sy-uname.
    UPDATE ztco_nmhpcpost.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e050 WITH ztco_nmhpcpost-gjahr
                        ztco_nmhpcpost-perid
                        ztco_nmhpcpost-matnr
                        ztco_nmhpcpost-kostl.
    ENDIF.
  ENDLOOP.

  COMMIT WORK AND WAIT .
ENDFORM.                    " READ_DATA_TO_REVERSE

*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VALUE_S_MTART
*&---------------------------------------------------------------------*
*       'Fert' 'Halb' are default.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_value_s_mtart.
  CLEAR : s_mtart, s_mtart[].

  s_mtart-low  = 'FERT'.
  s_mtart-sign = 'I'.
  s_mtart-option  = 'EQ'.
  APPEND s_mtart. CLEAR s_mtart.

  s_mtart-low = 'HALB'.
  s_mtart-sign = 'I'.
  s_mtart-option  = 'EQ'.
  APPEND s_mtart. CLEAR s_mtart.

ENDFORM.                    " DEFAULT_VALUE_S_MTART

*&---------------------------------------------------------------------*
*&      Form  SET_MFBF_INIT
*&---------------------------------------------------------------------*
*       Set MFBF Variant
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_mfbf_init.
  CLEAR rmuser_tav.
  SELECT SINGLE * FROM rmuser_tav
                 WHERE username = sy-uname
                   AND tcode    = 'MFBF'.
  IF sy-subrc <> 0.
    rmuser_tav-username = sy-uname.
    rmuser_tav-tcode    = 'MFBF'.
    rmuser_tav-tvariant = space.
    rmuser_tav-scenario = 'LAGER'.
    rmuser_tav-type     = 'L'.
    rmuser_tav-zpkt     = space.

    INSERT rmuser_tav.
  ELSE.
    rmuser_tav-tvariant = space.
    rmuser_tav-scenario = 'LAGER'.
    rmuser_tav-type     = 'L'.
    rmuser_tav-zpkt     = space.

    MODIFY rmuser_tav.
  ENDIF.

ENDFORM.                    " SET_MFBF_INIT
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_READ_COSS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM archive_read_coss .

  TYPES: BEGIN OF ty_coss,
         lednr TYPE lednr,
         objnr TYPE j_objnr,
         gjahr TYPE gjahr,
         wrttp TYPE co_wrttp,
         versn TYPE versn,
         parob TYPE parob,
         uspob TYPE uspob,
         vrgng TYPE co_vorgang,
           archivekey TYPE arkey,
           archiveofs TYPE admi_offst.
  TYPES: END OF ty_coss.

  DATA: l_handle    TYPE sytabix,
        lt_coss     TYPE TABLE OF coss WITH HEADER LINE,
        l_archindex LIKE aind_str2-archindex,
        l_gentab    LIKE aind_str2-gentab.

  DATA: lt_inx_coss TYPE TABLE OF ty_coss,
        ls_inx_coss TYPE ty_coss.

* 1. Input the archive infostructure name
  CLEAR l_archindex.
  l_archindex = 'ZCOSS_001'.

* 2. Get the structure table using infostructure
  CLEAR l_gentab.
  SELECT SINGLE gentab INTO l_gentab FROM aind_str2
   WHERE archindex = l_archindex.

  CHECK sy-subrc = 0 AND NOT l_gentab IS INITIAL.

* 3. Get the archived data from structure table
  CLEAR lt_inx_coss[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_inx_coss
    FROM (l_gentab)
     FOR ALL ENTRIES IN it_ma_obj
   WHERE lednr = '00'
     AND objnr = it_ma_obj-objnr
     AND gjahr = p_gjahr
     AND wrttp = p_wrttp
     AND versn = p_versn
     AND parob = it_ma_obj-parob
     AND uspob = it_ma_obj-uspob
     AND vrgng IN s_vrgng.

  CHECK NOT lt_inx_coss[] IS INITIAL.

* 4. Get more archived data looping structure table
  CLEAR: gt_coss, gt_coss[].
  LOOP AT lt_inx_coss INTO ls_inx_coss.
*  4.1 Read information from archivekey & offset
    CLEAR l_handle.
    CALL FUNCTION 'ARCHIVE_READ_OBJECT'
      EXPORTING
        object                    = 'CO_ORDER'
        archivkey                 = ls_inx_coss-archivekey
        offset                    = ls_inx_coss-archiveofs
      IMPORTING
        archive_handle            = l_handle
      EXCEPTIONS
        no_record_found           = 1
        file_io_error             = 2
        internal_error            = 3
        open_error                = 4
        cancelled_by_user         = 5
        archivelink_error         = 6
        object_not_found          = 7
        filename_creation_failure = 8
        file_already_open         = 9
        not_authorized            = 10
        file_not_found            = 11
        error_message             = 12
        OTHERS                    = 13.

    CHECK sy-subrc = 0.

*  4.2 Read table from information
    CLEAR: lt_coss, lt_coss[].
    CALL FUNCTION 'ARCHIVE_GET_TABLE'
      EXPORTING
        archive_handle          = l_handle
        record_structure        = 'COSS'
        all_records_of_object   = 'X'
      TABLES
        table                   = lt_coss
      EXCEPTIONS
        end_of_object           = 1
        internal_error          = 2
        wrong_access_to_archive = 3
        OTHERS                  = 4.

    CHECK sy-subrc = 0 AND NOT lt_coss[] IS INITIAL.

* 5. Append archived data table to finally interal table
    INSERT LINES OF lt_coss INTO TABLE gt_coss.
  ENDLOOP.

  SORT gt_coss.
  DELETE ADJACENT DUPLICATES FROM gt_coss COMPARING ALL FIELDS.

  LOOP AT gt_coss.
    MOVE-CORRESPONDING gt_coss TO it_coss.
    APPEND it_coss.  CLEAR it_coss.
  ENDLOOP.

ENDFORM.                    " ARCHIVE_READ_COSS
