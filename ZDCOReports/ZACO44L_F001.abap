*----------------------------------------------------------------------*
***INCLUDE ZACO44L_F001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT
*&---------------------------------------------------------------------*
*       Check Input Values
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input.
* BOM TYPE
  IF p_bomty NA 'SP'.
    MESSAGE e001 WITH 'Plan Type,' p_bomty.
  ENDIF.
* Material Range
  CLEAR mara.
  SELECT SINGLE *  FROM mara
                  WHERE matnr IN s_matnr.
  IF sy-subrc <> 0.
    MESSAGE e061 .
  ENDIF.
* P_UP
  IF p_up = 'X'.
    MESSAGE w000 WITH 'Qty. Str. Valid Date (from) will be ignored'.
    MESSAGE i000 WITH 'No record will be created.'.
  ENDIF.
ENDFORM.                    " CHECK_INPUT

*&---------------------------------------------------------------------*
*&      Form  SHOW_MESG_OF_REP
*&---------------------------------------------------------------------*
*       Show Message
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_mesg_of_rep.
  DATA : lv_answer.
  DATA : lv_text001(60).
  DATA : lv_text002(60).

  select count( * ) into sy-dbcnt from ZTCO_EBUSPLANBOM
      where GJAHR    = p_gjahr
        and BOMTYPE  = P_BOMTY
        and MATNR   in s_matnr.
  check sy-dbcnt > 0.

  CLEAR lv_text001.
  CONCATENATE text-013 p_gjahr INTO lv_text001
              SEPARATED BY space.

  CLEAR lv_text002.
  CONCATENATE text-014 s_matnr-low 'To' s_matnr-high INTO lv_text002
              SEPARATED BY space.


  CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
    EXPORTING
      defaultoption        = 'N'
      diagnosetext1        = text-011
      diagnosetext2        = text-012
      diagnosetext3        = lv_text001
      textline1            = p_bomty
      textline2            = lv_text002
      titel                = text-010
*     START_COLUMN         = 25
*     START_ROW            = 6
*     CANCEL_DISPLAY       = 'X'
    IMPORTING
      answer               = lv_answer.

  IF lv_answer <> 'J'.
    MESSAGE e043.
  ENDIF.
ENDFORM.                    " SHOW_MESG_OF_REP

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_MKAL
*&---------------------------------------------------------------------*
*       Read data from MKAL (Prodcution Version table)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_fr_mkal.

* -  Progress Ind.
  PERFORM progress_ind USING '70'
                             text-210.

**// Mod. By Hyung Jin Youn 2004.01.16
* Selection part Should be changed
* 'FERT' Type Materials : Prd. ver. should be '0'
* Other  Type Materials : Prd. ver. should not be cared of .
*                         But Only one record should be selected
  CLEAR : it_mkal, it_mkal.
* Index MKAL - 4
  SELECT *  INTO CORRESPONDING FIELDS OF TABLE it_mkal
            FROM mkal
           WHERE
                  (       verid = '0'
                     AND  matnr IN s_matnr
                     AND  exists ( SELECT * FROM mara
                                           WHERE matnr = mkal~matnr
                                             AND mtart = 'FERT' )
                  )
              OR
                  (       matnr IN s_matnr
                     AND  exists ( SELECT * FROM mara
                                           WHERE matnr = mkal~matnr
                                             AND mtart <> 'FERT' )
                  ).

  IF it_mkal[] IS INITIAL.
    MESSAGE e062.
  ENDIF.

* Remove the records with Duplicated Key
*- Production Version should be ignored
* Set Production Version as '0'
  CLEAR it_mkal.
  it_mkal-verid = '0'.
  MODIFY it_mkal TRANSPORTING verid  WHERE verid  NE '0'.

  SORT it_mkal BY matnr  werks  verid.

  DELETE ADJACENT DUPLICATES FROM it_mkal
                             COMPARING  matnr
                                        werks
                                        verid.
  IF it_mkal[] IS INITIAL.
    MESSAGE e062.
  ENDIF.
**// End of Mod.


**// Mod. By Hyung Jin Youn 2004.06.30

** Use explosion data
* Conditions
* Materials with BOM have child materials and those child materials
* also have to be checked.
* 1. Explosion should be done with multi-level
* 2. Only HALB materials should be chosen from the exploded data
* 3. Alt. DUMPS = 'X' - Phantom Material
*    the material has not to be selected.
* 4  For Engine Material : Change PLANT and Re-Explode
*    Material Type       = 'HALB'
*    Procurement type    = 'F'
*    Special procurement = '40'.
* 5  BOM Usage = '1'
* 6  Standard P_BOMTY = 'S' : CAPID = 'PP01'.
*    Standard P_BOMTY = 'P' : CAPID = 'PP01'. all PP01

  DATA : it_tmp_mkal LIKE it_mkal OCCURS 0
                     WITH HEADER LINE .
  DATA : BEGIN OF it_l_mat  OCCURS 0,
          matnr LIKE mara-matnr,
          werks LIKE mkal-werks.
  DATA : END OF it_l_mat.
  DATA : it_l_stb    LIKE STANDARD TABLE OF stpox
                     WITH HEADER LINE .
  DATA : lv_capid    LIKE  tc04-capid.

  CLEAR : it_tmp_mkal, it_tmp_mkal[].
  CLEAR : it_l_mat ,   it_l_mat[].

* Set CAPID : BOM Application
  lv_capid = 'PP01'.

  explode_bom it_mkal.
  CLEAR   it_l_mat.


** For Engine Material : Change Plant from 'P001' to 'E001'
* Explode in E001
*    Material Type       = 'HALB'
*    Procurement type    = 'F'
*    Special procurement = '40'.
  DATA : BEGIN OF wa_l_inf,
          mtart LIKE mara-mtart,
          beskz LIKE marc-beskz,
          sobsl LIKE marc-sobsl,
         END OF wa_l_inf.
  DATA : it_l_e001_mat LIKE it_l_mat OCCURS 0
                       WITH HEADER LINE .

  LOOP AT it_l_mat.
    CLEAR wa_l_inf.
    SELECT SINGLE
           mara~mtart
           marc~beskz
           marc~sobsl
           INTO CORRESPONDING FIELDS OF wa_l_inf
                    FROM marc INNER JOIN mara
                      ON marc~matnr = mara~matnr
                   WHERE marc~matnr = it_l_mat-matnr
                     AND marc~werks = it_l_mat-werks.
    IF    wa_l_inf-mtart = 'HALB'
      AND wa_l_inf-beskz = 'F'
      AND wa_l_inf-sobsl = '40'.
      it_l_mat-werks = 'E001'.
      MODIFY it_l_mat.
* Copy data to TMP ITAB
      MOVE-CORRESPONDING it_l_mat TO it_l_e001_mat.
      COLLECT it_l_e001_mat.
      CLEAR   it_l_e001_mat.
    ENDIF.
    CLEAR it_l_mat.
  ENDLOOP.

  explode_bom it_l_e001_mat.
  CLEAR   it_l_mat.


** Check Component materials (Not Allowed duplicated materials)
  LOOP AT it_l_mat.
    CLEAR it_mkal.
    READ TABLE it_mkal
      WITH KEY matnr = it_l_mat-matnr
               werks = it_l_mat-werks.
    IF sy-subrc = 0.
      DELETE it_l_mat.
    ENDIF.
    CLEAR it_l_mat.
  ENDLOOP.


** Read data from MKAL using Components (No FERT material)
  CHECK NOT it_l_mat[] IS INITIAL .

* All Components of BOM which are supposed to be manufactured in house
* always have production version because creation of production version
* is done before making up BOM .
  CLEAR : it_tmp_mkal, it_tmp_mkal[].

*  select *  into corresponding fields of table it_tmp_mkal
*            from mkal
*            for all entries in it_l_mat
*           where  matnr = it_l_mat-matnr
*             and  werks = it_l_mat-werks.

  LOOP AT it_l_mat.
    SELECT * FROM mkal
          WHERE  matnr = it_l_mat-matnr
            AND  werks = it_l_mat-werks
          ORDER BY verid.
      MOVE-CORRESPONDING mkal TO it_tmp_mkal.
      APPEND it_tmp_mkal.
      EXIT.
    ENDSELECT.
  ENDLOOP.

  IF NOT it_tmp_mkal[] IS INITIAL.
    APPEND LINES OF it_tmp_mkal TO it_mkal.
  ENDIF.


**// End of Mod.

ENDFORM.                    " READ_DATA_FR_MKAL

*&---------------------------------------------------------------------*
*&      Form  CHECK_VALID_ROUTING
*&---------------------------------------------------------------------*
*       Check Routing with From-Date
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_valid_routing.

  DATA :  lv_verwe LIKE plko-verwe.
  DATA :  lv_subrc LIKE sy-subrc.

  CLEAR : it_ztco_ebusplanbom, it_ztco_ebusplanbom[].
  CLEAR   it_mkal.

* Set Usage(Routing)
  CLEAR lv_verwe.
  CASE  p_bomty.
    WHEN 'P'. "Business Plan
      lv_verwe = '10'.
    WHEN 'S'. "Quartery Standard
      lv_verwe = '1'.
  ENDCASE.

  LOOP AT it_mkal.
* Copy data to IT_ZTCO_EBUSPLANBOM
    MOVE it_mkal-matnr TO it_ztco_ebusplanbom-matnr.
    MOVE it_mkal-werks TO it_ztco_ebusplanbom-werks.
    it_ztco_ebusplanbom-gjahr   = p_gjahr.
    it_ztco_ebusplanbom-bomtype = p_bomty.
    it_ztco_ebusplanbom-datuv   = p_datuv.
* Check Routing with Valid-From date "All routing Check
**//Mod. By hyung Jin Youn 2004.01.17
*Business  Plan : Just Check Exsistence of Routing
*Quarter   Plan : Check Exsistence of Routing
*                 and The relationship between routing and Prd. Version
    CLEAR  lv_subrc.
    PERFORM check_routing_each
                          USING it_mkal-plnty
                                it_mkal-plnnr
                                it_mkal-alnal
                                lv_subrc
                                lv_verwe.
    PERFORM check_routing_each
                          USING it_mkal-pltyg
                                it_mkal-plnng
                                it_mkal-alnag
                                lv_subrc
                                lv_verwe.
    PERFORM check_routing_each
                          USING it_mkal-pltym
                                it_mkal-plnnm
                                it_mkal-alnam
                                lv_subrc
                                lv_verwe.
* All blank (routing)
    IF lv_subrc  >= 3.
      it_ztco_ebusplanbom-chk_exist = '2'.
    ENDIF.
* Check Only For 'P' Business Plan BOM
    PERFORM check_routing_for_p USING lv_verwe.

**// End of Mod.

* Append
    APPEND it_ztco_ebusplanbom.
    CLEAR  it_ztco_ebusplanbom.
    CLEAR  it_mkal.
  ENDLOOP.

  CLEAR it_ztco_ebusplanbom.

ENDFORM.                    " CHECK_VALID_ROUTING

*&---------------------------------------------------------------------*
*&      Form  ENQUEUE_ZTCO_EBUSPLANBOM
*&---------------------------------------------------------------------*
*       Enqueue
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enqueue_ztco_ebusplanbom.
  LOOP AT it_mkal.
    CALL FUNCTION 'ENQUEUE_EZCO_ZTCO_EBUSPL'
       EXPORTING
          mode_ztco_ebusplanbom       = 'E'
          mandt                       = sy-mandt
          gjahr                       = p_gjahr
          bomtype                     = p_bomty
          matnr                       = it_mkal-matnr
          werks                       = it_mkal-werks
*         X_GJAHR                     = ' '
*         X_BOMTYPE                   = ' '
*         X_MATNR                     = ' '
*         X_WERKS                     = ' '
*         _SCOPE                      = '2'
*         _WAIT                       = ' '
*         _COLLECT                    = ' '
       EXCEPTIONS
          foreign_lock                = 1
          system_failure              = 2
          OTHERS                      = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CLEAR it_mkal.
  ENDLOOP.

* For View Lock
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      mode_rstable         = 'E'
      tabname              = 'ZTCO_EBUSPLANBOM'
*     VARKEY               =
*     X_TABNAME            = ' '
*     X_VARKEY             = ' '
*     _SCOPE               = '2'
*     _WAIT                = ' '
*     _COLLECT             = ' '
    EXCEPTIONS
      foreign_lock         = 1
      system_failure       = 2
      OTHERS               = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " ENQUEUE_ZTCO_EBUSPLANBOM

*&---------------------------------------------------------------------*
*&      Form  DELETE_DATA_FR_TABLE
*&---------------------------------------------------------------------*
*       Delete data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_data_fr_table.

  DELETE   FROM ztco_ebusplanbom
          WHERE
                gjahr                       = p_gjahr
            AND bomtype                     = p_bomty.

* Delete Each record to check data when an error occurs.
*  loop at it_mkal.
*    delete   from ztco_ebusplanbom
*            where
*                  gjahr                       = p_gjahr
*              and bomtype                     = p_bomty
**            and matnr                       = it_mkal-matnr
**            and werks                       = it_mkal-werks.
** Do not Check subrc "In case of creation
*    clear it_mkal.
*  endloop.
ENDFORM.                    " DELETE_DATA_FR_TABLE

*&---------------------------------------------------------------------*
*&      Form  CHECK_VALID_BOM
*&---------------------------------------------------------------------*
*       Check BOM with Valid-From Date
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_valid_bom.

  DATA : it_l_return      LIKE STANDARD TABLE OF bapiret2
                          WITH HEADER LINE .
  DATA : lv_bom_usage     LIKE bapi1080_bgr_c-bom_usage
                          VALUE  '1'.

*
  SORT it_ztco_ebusplanbom BY  chk_exist.
  LOOP AT it_ztco_ebusplanbom. "  WHERE CHK_EXIST EQ SPACE.

    CLEAR : it_l_return, it_l_return[].
* Check BOM EXISTENCE
* Usage = '1' : Production
    CALL FUNCTION 'BAPI_MAT_BOM_EXISTENCE_CHECK'
      EXPORTING
        material              = it_ztco_ebusplanbom-matnr
        plant                 = it_ztco_ebusplanbom-werks
        bomusage              = lv_bom_usage
        valid_from_date       = it_ztco_ebusplanbom-datuv
*       VALID_TO_DATE         =
*       MATERIAL_EVG          =
      TABLES
        return                = it_l_return.

* The BAPI "BAPI_MAT_BOM_EXISTENCE_CHECK" generates only "w" type
* messages if fails.
* It sends out no message If success to find proper BOM.
* Only Material BOM 'M'
    IF it_l_return[] IS INITIAL .
      CALL FUNCTION 'CP_BD_GET_BOM_FOR_MATERIAL'
        EXPORTING
          sttag_imp       = it_ztco_ebusplanbom-datuv
          matnr_imp       = it_ztco_ebusplanbom-matnr
          werks_imp       = it_ztco_ebusplanbom-werks
          stlty_imp       = 'M'
*       IMPORTING
*         STLTY_EXP       =
*         STLNR_EXP       =
*         STLAL_EXP       =
        EXCEPTIONS
          not_found       = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
* BOM Error  "Not Valid / Not Material BOM
        IF it_ztco_ebusplanbom-chk_exist = '2'.
* ALL error
          it_ztco_ebusplanbom-chk_exist = '3'.
        ELSE.
          it_ztco_ebusplanbom-chk_exist = '1'.
        ENDIF.
        MODIFY it_ztco_ebusplanbom.
      ELSE.
* ALL OK
        IF it_ztco_ebusplanbom-chk_exist = '0'.
**// Mod. by Hyung Jin Youn 2004.02.02
          it_ztco_ebusplanbom-matnr_chg  = it_ztco_ebusplanbom-matnr.
          it_ztco_ebusplanbom-matnr_chg2 = it_ztco_ebusplanbom-matnr.
          MODIFY it_ztco_ebusplanbom.
**// End of Mod.
        ENDIF.
      ENDIF.
    ELSE.
* BOM Error "Non-Existence
      IF it_ztco_ebusplanbom-chk_exist = '2'.
* ALL error
        it_ztco_ebusplanbom-chk_exist = '3'.
      ELSE.
        it_ztco_ebusplanbom-chk_exist = '1'.
      ENDIF.
      MODIFY it_ztco_ebusplanbom.
    ENDIF.

    CLEAR it_ztco_ebusplanbom.
  ENDLOOP.

  CLEAR it_ztco_ebusplanbom.

ENDFORM.                    " CHECK_VALID_BOM

*&---------------------------------------------------------------------*
*&      Form  UPDATE_INSERT_TABLE
*&---------------------------------------------------------------------*
*       Update/Insert data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_insert_table.
* Do not use Mass-Insertion for performance
* Ex>
**  INSERT          ZTCO_EBUSPLANBOM
**    FROM TABLE IT_ZTCO_EBUSPLANBOM.
  TABLES : *ztco_ebusplanbom.

  LOOP AT it_ztco_ebusplanbom.
    it_ztco_ebusplanbom-erdat = sy-datum.
    it_ztco_ebusplanbom-erzet = sy-uzeit.
    it_ztco_ebusplanbom-ernam = sy-uname.
    MODIFY it_ztco_ebusplanbom.
    CLEAR *ztco_ebusplanbom.
    MOVE-CORRESPONDING it_ztco_ebusplanbom TO *ztco_ebusplanbom.
* Only Insertion occurs
* (Always create data with specific key combination)
    INSERT INTO ztco_ebusplanbom VALUES *ztco_ebusplanbom.
*
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e030 WITH 'Insertion'.
    ENDIF.
    CLEAR it_ztco_ebusplanbom.
  ENDLOOP.

  COMMIT WORK AND WAIT.
*  MESSAGE S009 WITH '- Creating Records'.

ENDFORM.                    " UPDATE_INSERT_TABLE

*&---------------------------------------------------------------------*
*&      Form  CALL_MOD_DIS_SCR
*&---------------------------------------------------------------------*
*       Modifiaction / Display (Screen)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_mod_dis_scr.
*  CALL Screen.
*  Submit ZACO44R_PBOM
  CASE p_bomty.
    WHEN 'P'. "Editable -> Maint. Screen
      PERFORM call_edit_screen.
    WHEN 'S'. "Read only -> Report
      PERFORM call_pbom_report.
  ENDCASE.
ENDFORM.                    " CALL_MOD_DIS_SCR

*&---------------------------------------------------------------------*
*&      Form  CALL_PBOM_REPORT
*&---------------------------------------------------------------------*
*       Call report
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_pbom_report.
* Selection criteria for report
*SELECT-OPTIONS SP$00001 FOR ZTCO_EBUSPLANBOM-GJAHR MEMORY ID GJR.
*SELECT-OPTIONS SP$00002 FOR ZTCO_EBUSPLANBOM-BOMTYPE.
*SELECT-OPTIONS SP$00003 FOR ZTCO_EBUSPLANBOM-MATNR MEMORY ID MAT.
*SELECT-OPTIONS SP$00004 FOR ZTCO_EBUSPLANBOM-WERKS MEMORY ID WRK

* Local Data Definition
  RANGES : r_l_gjahr    FOR ztco_ebusplanbom-gjahr,
           r_l_bomtype  FOR ztco_ebusplanbom-bomtype,
           r_l_matnr    FOR ztco_ebusplanbom-matnr.

* Fill up Range tables
  CLEAR : r_l_gjahr, r_l_gjahr[].
  r_l_gjahr-low    = p_gjahr.
  r_l_gjahr-sign   = 'I'.
  r_l_gjahr-option = 'EQ'.
  APPEND  r_l_gjahr.
  CLEAR   r_l_gjahr.

  CLEAR : r_l_bomtype, r_l_bomtype[].
  r_l_bomtype-low  = p_bomty.
  r_l_bomtype-sign = 'I'.
  r_l_bomtype-option = 'EQ'.
  APPEND r_l_bomtype.
  CLEAR  r_l_bomtype.

  CLEAR : r_l_matnr, r_l_matnr[].
* R_L_MATNR[] = S_MATNR[].
*IT_ZTCO_EBUSPLANBOM
  IF s_matnr[] IS INITIAL.
  ELSE.
    LOOP AT it_ztco_ebusplanbom.
      r_l_matnr-low    = it_ztco_ebusplanbom-matnr.
      r_l_matnr-sign   = 'I'.
      r_l_matnr-option = 'EQ'.
      APPEND r_l_matnr.
      CLEAR it_ztco_ebusplanbom.
    ENDLOOP.
  ENDIF.


* Call Report
  SUBMIT zaco44r_pbom WITH sp$00001 IN r_l_gjahr
                      WITH sp$00002 IN r_l_bomtype
                      WITH sp$00003 IN r_l_matnr
*      SP$00004
*                     VIA SELECTION-SCREEN
                      AND RETURN.
ENDFORM.                    " CALL_PBOM_REPORT

*&---------------------------------------------------------------------*
*&      Form  CALL_EDIT_SCREEN
*&---------------------------------------------------------------------*
*       Edit
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_edit_screen.
* Local Data Definition
  DATA : it_l_vimsellist LIKE STANDARD TABLE OF vimsellist
                         WITH HEADER LINE .
* Fill Selection Criteria
  CLEAR : it_l_vimsellist, it_l_vimsellist[].

  IF s_matnr[] IS INITIAL.
* Year
    it_l_vimsellist-viewfield = 'GJAHR'.
    it_l_vimsellist-operator  = 'EQ'.
    it_l_vimsellist-value     = p_gjahr.
    it_l_vimsellist-and_or    = 'AND'.
    APPEND it_l_vimsellist.
    CLEAR  it_l_vimsellist.
* BOM TYPE
    it_l_vimsellist-viewfield = 'BOMTYPE'.
    it_l_vimsellist-operator  = 'EQ'.
    it_l_vimsellist-value     = p_bomty.
*    IT_L_VIMSELLIST-AND_OR    = 'AND'.
    APPEND it_l_vimsellist.
    CLEAR  it_l_vimsellist.
  ELSE.
    SORT it_ztco_ebusplanbom BY matnr .
    LOOP AT it_ztco_ebusplanbom.
* Year
      it_l_vimsellist-viewfield = 'GJAHR'.
      it_l_vimsellist-operator  = 'EQ'.
      it_l_vimsellist-value     = p_gjahr.
      it_l_vimsellist-and_or    = 'AND'.
      APPEND it_l_vimsellist.
      CLEAR  it_l_vimsellist.
* BOM TYPE
      it_l_vimsellist-viewfield = 'BOMTYPE'.
      it_l_vimsellist-operator  = 'EQ'.
      it_l_vimsellist-value     = p_bomty.
      it_l_vimsellist-and_or    = 'AND'.
      APPEND it_l_vimsellist.
      CLEAR  it_l_vimsellist.
* MATNR
      it_l_vimsellist-viewfield = 'MATNR'.
      it_l_vimsellist-operator  = 'EQ'.
      it_l_vimsellist-value     = it_ztco_ebusplanbom-matnr.
      it_l_vimsellist-and_or    = 'OR'.
      AT LAST.
        CLEAR  it_l_vimsellist-and_or.
      ENDAT.
      APPEND it_l_vimsellist.
      CLEAR  it_l_vimsellist.
    ENDLOOP.
  ENDIF.

* Call View
  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action                               = 'S'
*     CORR_NUMBER                          = '          '
*     GENERATE_MAINT_TOOL_IF_MISSING       = ' '
*     SHOW_SELECTION_POPUP                 = ' '
      view_name                            = 'ZVCO_EBUSPLBOM03'
*     NO_WARNING_FOR_CLIENTINDEP           = ' '
*     RFC_DESTINATION_FOR_UPGRADE          = ' '
*     CLIENT_FOR_UPGRADE                   = ' '
*     VARIANT_FOR_SELECTION                = ' '
      complex_selconds_used                = 'X'
   TABLES
      dba_sellist                          = it_l_vimsellist
*     EXCL_CUA_FUNCT                       =
   EXCEPTIONS
     client_reference                     = 1
     foreign_lock                         = 2
     invalid_action                       = 3
     no_clientindependent_auth            = 4
     no_database_function                 = 5
     no_editor_function                   = 6
     no_show_auth                         = 7
     no_tvdir_entry                       = 8
     no_upd_auth                          = 9
     only_show_allowed                    = 10
     system_failure                       = 11
     unknown_field_in_dba_sellist         = 12
     view_not_found                       = 13
     OTHERS                               = 14.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " CALL_EDIT_SCREEN

*&---------------------------------------------------------------------*
*&      Form  READ_DATA_FR_BOMTABLE
*&---------------------------------------------------------------------*
*       Read DATA (IT_ZTCO_EBUSPLANBOM - For Editing)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data_fr_bomtable.

  CLEAR : it_ztco_ebusplanbom, it_ztco_ebusplanbom[].

  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_ztco_ebusplanbom
           FROM ztco_ebusplanbom
          WHERE gjahr   = p_gjahr
            AND bomtype = p_bomty
            AND matnr   IN s_matnr.

  IF it_ztco_ebusplanbom[] IS INITIAL.
    MESSAGE e026.
  ENDIF.

ENDFORM.                    " READ_DATA_FR_BOMTABLE

*&---------------------------------------------------------------------*
*&      Form  CHECK_ROUTING_EACH
*&---------------------------------------------------------------------*
*       Routing (Check Indiv.)
*----------------------------------------------------------------------*
*      -->P_IT_MKAL_PLNTY
*      -->P_IT_MKAL_PLNNR
*      -->P_IT_MKAL_ALNAL
*      -->P_LV_SUBRC
*      -->P_LV_VERWE
*----------------------------------------------------------------------*
FORM check_routing_each USING    p_plnty
                                 p_plnnr
                                 p_alnal
                                 p_lv_subrc
                                 p_lv_verwe.

  CHECK  it_ztco_ebusplanbom-chk_exist EQ space.

  IF  p_plnty EQ space
   OR p_plnnr EQ space
   OR p_alnal EQ space.
    p_lv_subrc = p_lv_subrc + 1.
  ELSE.
    CASE p_bomty.
      WHEN 'P'. "<- Business Plan
*        CLEAR PLKO.
*        SELECT SINGLE * FROM PLKO
*                       WHERE PLNTY = P_PLNTY
*                         AND PLNNR = P_PLNNR
**                         AND PLNAL = P_ALNAL
*                         AND DATUV =< P_DATUV
*                         AND LOEKZ EQ SPACE
*                         AND VERWE = P_LV_VERWE
*                         AND STATU = '4'.
*        IF SY-SUBRC <> 0.
*          IT_ZTCO_EBUSPLANBOM-CHK_EXIST = '2'.
*        ENDIF.

      WHEN 'S'. "<- Quarter Plan
        CALL FUNCTION 'CP_CC_S_TSK_EXISTENCE_CHECK'
          EXPORTING
            i_date_from          = p_datuv
*     I_DATE_TO            = DATE-MAX_GRG
            i_plnty              = p_plnty
            i_plnnr              = p_plnnr
            i_plnal              = p_alnal
          EXCEPTIONS
            task_not_found       = 1
            OTHERS               = 2.
*  SUBRC
        IF sy-subrc <> 0.
* Routing Error " Not exist
          it_ztco_ebusplanbom-chk_exist = '2'.
        ELSE.
* Check the usage of Routing.
* Only Status = '4' " Relesed (General)
* Deletion Ind. = Space
          CLEAR plko.
          SELECT SINGLE * FROM plko
                         WHERE plnty = p_plnty
                           AND plnnr = p_plnnr
                           AND plnal = p_alnal
                           AND loekz EQ space
                           AND verwe = p_lv_verwe
                           AND statu = '4'.
          IF sy-subrc <> 0.
            it_ztco_ebusplanbom-chk_exist = '2'.
          ELSE.
* Check Material Relationship
            CLEAR mapl.
            SELECT SINGLE * FROM mapl
                           WHERE matnr = it_mkal-matnr
                             AND werks = it_mkal-werks
                             AND plnty = p_plnty
                             AND plnnr = p_plnnr
                             AND plnal = p_alnal
                             AND loekz EQ space
                             AND datuv =< p_datuv.
            IF sy-subrc <> 0.
              it_ztco_ebusplanbom-chk_exist = '2'.
            ENDIF.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDIF.

ENDFORM.                    " CHECK_ROUTING_EACH

*&---------------------------------------------------------------------*
*&      Form  CHECK_ROUTING_FOR_P
*&---------------------------------------------------------------------*
*       Check Only For 'P' Business Plan BOM
*----------------------------------------------------------------------*
*      -->P_LV_VERWE  text
*----------------------------------------------------------------------*
FORM check_routing_for_p USING    p_lv_verwe.

* Only For Business Plan
* AND In case that there was no error in Routing Check
  CHECK p_bomty = 'P'.
  CHECK  it_ztco_ebusplanbom-chk_exist EQ space.

* Ignore Internal counter
  CLEAR : mapl, plko.
  SELECT SINGLE * INTO CORRESPONDING FIELDS OF mapl
           FROM mapl INNER JOIN plko
             ON mapl~plnty = plko~plnty
            AND mapl~plnnr = plko~plnnr
            AND mapl~plnal = plko~plnal
*           AND MAPL~ZAEHL = PLKO~ZAEHL
          WHERE mapl~matnr =  it_mkal-matnr
            AND mapl~werks =  it_mkal-werks
            AND mapl~datuv =< p_datuv
            AND mapl~loekz EQ space
            AND plko~loekz EQ space
            AND plko~verwe = p_lv_verwe
            AND plko~statu = '4'.

  IF sy-subrc <> 0.
    it_ztco_ebusplanbom-chk_exist = '2'.
  ENDIF.

ENDFORM.                    " CHECK_ROUTING_FOR_P

*&---------------------------------------------------------------------*
*&      Form  COPY_MAT_AS_ERROR_TYPE
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM copy_mat_as_error_type.
  CLEAR : it_ztco_ebusplanbom.
*
*                           BOM Rep. Mat.  /   Routing Rep. Mat.
* All Ok : CHK_EXIST = '0'        O                   O
* BOM Err: CHK_EXIST = '1'        X                   O
* Rou.Err: CHK_EXIST = '2'        O                   X
* All Err: CHK_EXIST = '3'        X                   X
  LOOP AT it_ztco_ebusplanbom
                        WHERE chk_exist NE space.
    CASE  it_ztco_ebusplanbom-chk_exist.
      WHEN '1'.
        CLEAR it_ztco_ebusplanbom-matnr_chg.
        it_ztco_ebusplanbom-matnr_chg2 = it_ztco_ebusplanbom-matnr.
      WHEN '2'.
        it_ztco_ebusplanbom-matnr_chg = it_ztco_ebusplanbom-matnr.
        CLEAR it_ztco_ebusplanbom-matnr_chg2.
      WHEN '3'.
        CLEAR it_ztco_ebusplanbom-matnr_chg.
        CLEAR it_ztco_ebusplanbom-matnr_chg2.
    ENDCASE.
    MODIFY it_ztco_ebusplanbom.
    CLEAR  it_ztco_ebusplanbom.
  ENDLOOP.

ENDFORM.                    " COPY_MAT_AS_ERROR_TYPE

*&---------------------------------------------------------------------*
*&      Form  PROGRESS_IND
*&---------------------------------------------------------------------*
*       Progress IND.
*----------------------------------------------------------------------*
*      -->P_%         %
*      -->P_TEXT      TEXT
*----------------------------------------------------------------------*
FORM progress_ind USING    p_%
                           p_text.
  CALL FUNCTION 'FI_PROGRESS_INDICATOR'
    EXPORTING
      percentage          = p_%
      text                = p_text
*     MESSAGECLASS        = ' '
*     MESSAGENUMBER       = ' '
*     MESSAGEPAR1         = ' '
*     MESSAGEPAR2         = ' '
*     MESSAGEPAR3         = ' '
*     MESSAGEPAR4         = ' '
            .
ENDFORM.                    " PROGRESS_IND
