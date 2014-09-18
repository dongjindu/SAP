*&---------------------------------------------------------------------*
*&  Include           ZHKPMR0005F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

  DATA : lt_t001k LIKE TABLE OF t001k WITH HEADER LINE ,
         lt_t001  LIKE TABLE OF t001  WITH HEADER LINE ,
         lt_plko  LIKE TABLE OF plko  WITH HEADER LINE ,
         lt_plpo  LIKE TABLE OF plpo  WITH HEADER LINE ,
         lt_plas  LIKE TABLE OF plas  WITH HEADER LINE ,
         lt_plmz  LIKE TABLE OF plmz  WITH HEADER LINE ,
         lt_stpo  LIKE TABLE OF stpo  WITH HEADER LINE .

  DATA : lt_eapl  LIKE TABLE OF eapl  WITH HEADER LINE .


  CLEAR : gt_row[], gt_row .

*. Get row data : Task list - header
  SELECT *
    INTO TABLE lt_plko
    FROM plko
   WHERE werks IN s_werks
     AND plnty EQ 'E'  .

  DELETE lt_plko WHERE loekz = c_x .

  IF lt_plko[] IS NOT INITIAL .

*.. Allocation of task lists to pieces of equipment
    SELECT * INTO TABLE lt_eapl
      FROM eapl
      FOR ALL ENTRIES IN lt_plko
     WHERE plnty EQ lt_plko-plnty
       AND plnnr EQ lt_plko-plnnr
       AND plnal EQ lt_plko-plnal
       AND zaehl EQ lt_plko-zaehl
       AND equnr IN s_equnr .

    SORT lt_eapl BY plnty plnnr plnal zaehl .

    LOOP AT lt_plko .

      READ TABLE lt_eapl WITH KEY plnty = lt_plko-plnty
                                  plnnr = lt_plko-plnnr
                                  plnal = lt_plko-plnal
                                  zaehl = lt_plko-zaehl
                                  BINARY SEARCH .
      IF sy-subrc <> 0 .
        DELETE lt_plko .
      ENDIF .


    ENDLOOP .
  ENDIF .
*.
  IF lt_plko[] IS NOT INITIAL .

*.. Task list - selection of operations/activities
    SELECT * INTO TABLE lt_plas
      FROM plas
      FOR ALL ENTRIES IN lt_plko
     WHERE plnty EQ lt_plko-plnty
       AND plnnr EQ lt_plko-plnnr
       AND plnal EQ lt_plko-plnal .

    DELETE lt_plas WHERE loekz <> space .

*.. Task list - operation/activity
    IF lt_plas[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_plpo
        FROM plpo
        FOR ALL ENTRIES IN lt_plas
       WHERE plnty EQ lt_plas-plnty
         AND plnnr EQ lt_plas-plnnr
         AND plnkn EQ lt_plas-plnkn  .
    ENDIF .

*.. Allocation of bill of material items to operations
    IF lt_plas[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_plmz
        FROM plmz
        FOR ALL ENTRIES IN lt_plas
       WHERE plnty EQ lt_plas-plnty
         AND plnnr EQ lt_plas-plnnr
         AND plnal EQ lt_plas-plnal
         AND plnkn EQ lt_plas-plnkn  .
        DELETE lt_plmz WHERE loekz <> space .
    ENDIF .

**.. BOM item
    IF lt_plmz[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_stpo
        FROM stpo
        FOR ALL ENTRIES IN lt_plmz
       WHERE stlty EQ lt_plmz-stlty
         AND stlnr EQ lt_plmz-stlnr
         AND stlkn EQ lt_plmz-stlkn  .
    ENDIF .


  ENDIF .

*.
  SORT lt_eapl BY plnty plnnr plnal zaehl .
  SORT lt_plas BY plnty plnnr plnal .
  SORT lt_plpo BY plnty plnnr plnkn .
  SORT lt_plmz BY plnty plnnr plnal plnkn .
  SORT lt_stpo BY stlty stlnr stlkn .

  LOOP AT lt_plko .

    CLEAR   gt_row .

    gt_row-werks = lt_plko-werks .


    READ TABLE lt_eapl WITH KEY plnty = lt_plko-plnty
                                plnnr = lt_plko-plnnr
                                plnal = lt_plko-plnal
                                zaehl = lt_plko-zaehl
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_row-zfleq = lt_eapl-equnr .
    ENDIF .

    gt_row-plnty = lt_plko-plnty .
    gt_row-plnnr = lt_plko-plnnr .
    gt_row-plnal = lt_plko-plnal .

    gt_row-datuv = lt_plko-datuv .
    gt_row-andat = lt_plko-andat .
    gt_row-aedat = lt_plko-aedat .
    gt_row-verwe = lt_plko-verwe .
    gt_row-ktext = lt_plko-ktext .

    READ TABLE lt_plas WITH KEY plnty = lt_plko-plnty
                                plnnr = lt_plko-plnnr
                                plnal = lt_plko-plnal
                                BINARY SEARCH .
**+ 1
    IF sy-subrc = 0 . "+ 3
      LOOP AT lt_plas FROM sy-tabix . "+ 2
        IF lt_plas-plnty <> lt_plko-plnty OR
           lt_plas-plnnr <> lt_plko-plnnr OR
           lt_plas-plnal <> lt_plko-plnal .
          EXIT.
        ENDIF .

        gt_row-plnkn = lt_plas-plnkn .

        READ TABLE lt_plpo WITH KEY plnty = lt_plas-plnty
                                    plnnr = lt_plas-plnnr
                                    plnkn = lt_plas-plnkn
                                    BINARY SEARCH .
        IF sy-subrc = 0 .
          gt_row-vornr = lt_plpo-vornr .
          gt_row-ltxa1 = lt_plpo-ltxa1 .
          gt_row-sortl = lt_plpo-sortl .
          gt_row-odatuv = lt_plpo-datuv .
          gt_row-oandat = lt_plpo-andat .
          gt_row-oaedat = lt_plpo-aedat .

        ELSE .
          CLEAR :
          gt_row-vornr ,
          gt_row-ltxa1 ,
          gt_row-sortl ,
          gt_row-odatuv ,
          gt_row-oandat ,
          gt_row-oaedat .

        ENDIF .

        CLEAR :
        gt_row-idnrk1 ,
        gt_row-menge1 ,
        gt_row-meins1 ,
        gt_row-idnrk2 ,
        gt_row-menge2 ,
        gt_row-meins2 ,
        gt_row-idnrk3 ,
        gt_row-menge3 ,
        gt_row-meins3 ,
        gt_row-idnrk4 ,
        gt_row-menge4 ,
        gt_row-meins4 ,
        gt_row-idnrk5 ,
        gt_row-menge5 ,
        gt_row-meins5 .

        READ TABLE lt_plmz WITH KEY plnty = lt_plas-plnty
                                    plnnr = lt_plas-plnnr
                                    plnal = lt_plas-plnal
                                    plnkn = lt_plas-plnkn
                                    BINARY SEARCH .
        IF sy-subrc = 0 . "+ 1
          LOOP AT lt_plmz FROM sy-tabix .
            IF lt_plmz-plnty <> lt_plas-plnty OR
               lt_plmz-plnnr <> lt_plas-plnnr OR
               lt_plmz-plnal <> lt_plas-plnal OR
               lt_plmz-plnkn <> lt_plas-plnkn .
              EXIT .
            ENDIF .

            READ TABLE lt_stpo WITH KEY stlty = lt_plmz-stlty
                                        stlnr = lt_plmz-stlnr
                                        stlkn = lt_plmz-stlkn
                                        BINARY SEARCH .
            IF sy-subrc = 0 .
              LOOP AT lt_stpo FROM sy-tabix .

                IF lt_stpo-stlty <> lt_plmz-stlty OR
                   lt_stpo-stlnr <> lt_plmz-stlnr OR
                   lt_stpo-stlkn <> lt_plmz-stlkn .
                  EXIT .
                ENDIF .

                IF gt_row-idnrk1 IS INITIAL .
                  gt_row-idnrk1 = lt_stpo-idnrk .
                  gt_row-menge1 = lt_stpo-menge .
                  gt_row-meins1 = lt_stpo-meins .
                ELSEIF gt_row-idnrk2 IS INITIAL .
                  gt_row-idnrk2 = lt_stpo-idnrk .
                  gt_row-menge2 = lt_stpo-menge .
                  gt_row-meins2 = lt_stpo-meins .
                ELSEIF gt_row-idnrk3 IS INITIAL .
                  gt_row-idnrk3 = lt_stpo-idnrk .
                  gt_row-menge3 = lt_stpo-menge .
                  gt_row-meins3 = lt_stpo-meins .
                ELSEIF gt_row-idnrk4 IS INITIAL .
                  gt_row-idnrk4 = lt_stpo-idnrk .
                  gt_row-menge4 = lt_stpo-menge .
                  gt_row-meins4 = lt_stpo-meins .
                ELSEIF gt_row-idnrk5 IS INITIAL .
                  gt_row-idnrk5 = lt_stpo-idnrk .
                  gt_row-menge5 = lt_stpo-menge .
                  gt_row-meins5 = lt_stpo-meins .
                ENDIF .

              ENDLOOP .
            ENDIF .

          ENDLOOP .
        ENDIF .  "+ 1

        APPEND gt_row .

      ENDLOOP. "+ 2

    ENDIF . "+ 3

  ENDLOOP .

ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .
  DATA : lt_mpos LIKE TABLE OF mpos WITH HEADER LINE ,
         lt_mmpt LIKE TABLE OF mmpt WITH HEADER LINE ,
         lt_mpla LIKE TABLE OF mpla WITH HEADER LINE ,
         lt_t399g_t LIKE TABLE OF t399g_t WITH HEADER LINE .

  DATA : lt_t006a LIKE TABLE OF t006a WITH HEADER LINE .

  CLEAR : gt_data[], gt_data[] .

  CHECK gt_row[] IS NOT INITIAL .

*.. Maintenance item
  SELECT * INTO TABLE lt_mpos
    FROM mpos
    FOR ALL ENTRIES IN gt_row
   WHERE plnty EQ gt_row-plnty
     AND plnnr EQ gt_row-plnnr
     AND plnal EQ gt_row-plnal .

  IF lt_mpos[] IS NOT INITIAL .

*.. Cycle definitions and MeasPoints for MaintPlan
    SELECT * INTO TABLE lt_mmpt
     FROM mmpt
     FOR ALL ENTRIES IN lt_mpos
    WHERE warpl EQ lt_mpos-warpl .

*.. Maintenance plan
    SELECT * INTO TABLE lt_mpla
     FROM mpla
     FOR ALL ENTRIES IN lt_mpos
    WHERE warpl EQ lt_mpos-warpl .

    IF lt_mpla[] IS NOT INITIAL .
*.. Descriptions for Maintenance Plan Sort Fields
      SELECT * INTO TABLE lt_t399g_t
       FROM t399g_t
       FOR ALL ENTRIES IN lt_mpla
      WHERE plan_sort EQ lt_mpla-plan_sort .
    ENDIF .

  ENDIF .

*.. Assign Internal to Language-Dependent Unit
  SELECT * INTO TABLE lt_t006a
    FROM t006a
   WHERE spras EQ sy-langu .

  SORT lt_mpos BY plnty plnnr plnal .
  SORT lt_mmpt BY warpl .
  SORT lt_mpla BY warpl .
  SORT lt_t399g_t BY plan_sort .
  SORT lt_t006a BY msehi .

  LOOP AT gt_row .
    CLEAR : gt_data .
    CLEAR : lt_mpos, lt_mmpt, lt_mpla , lt_t399g_t .

    READ TABLE lt_mpos WITH KEY plnty = gt_row-plnty
                                plnnr = gt_row-plnnr
                                plnal = gt_row-plnal
                                BINARY SEARCH .

    READ TABLE lt_mmpt WITH KEY warpl = lt_mpos-warpl
                                BINARY SEARCH .

    READ TABLE lt_mpla WITH KEY warpl = lt_mpos-warpl
                                BINARY SEARCH .

    READ TABLE lt_t399g_t WITH KEY plan_sort = lt_mpla-plan_sort
                                BINARY SEARCH .

    MOVE-CORRESPONDING gt_row TO gt_data .
    gt_data-equnr = gt_row-zfleq .
    gt_data-warpl = lt_mpos-warpl .

    IF lt_mmpt-zeieh IS NOT INITIAL AND
       lt_mmpt-zykl1 IS NOT INITIAL .
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          char_unit       = lt_mmpt-zeieh
          decimals        = 0
          exponent        = 0
          fltp_value_si   = lt_mmpt-zykl1
          indicator_value = 'X'
          masc_symbol     = ' '
        IMPORTING
          char_value      = gt_data-zykl1.
    ENDIF .

    READ TABLE lt_t006a WITH KEY msehi = lt_mmpt-zeieh
                                 BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_data-zeieh = lt_t006a-mseh3 .
    else .
      gt_data-zeieh  = lt_mmpt-zeieh .
    ENDIF .

    gt_data-stadt = lt_mpla-stadt .
    gt_data-txt   = lt_t399g_t-txt .

    IF gt_data-idnrk1 IS NOT INITIAL .
      gt_data-check_1 = c_x .
    ENDIF .

    IF gt_data-idnrk2 IS NOT INITIAL .
      gt_data-check_2 = c_x .
    ENDIF .

    IF gt_data-idnrk3 IS NOT INITIAL .
      gt_data-check_3 = c_x .
    ENDIF .

    IF gt_data-idnrk4 IS NOT INITIAL .
      gt_data-check_4 = c_x .
    ENDIF .

    IF gt_data-idnrk5 IS NOT INITIAL .
      gt_data-check_5 = c_x .
    ENDIF .

    PERFORM set_field .
    APPEND gt_data .

  ENDLOOP .

  SORT gt_data BY werks equnr plnty plnnr plnal plnkn vornr .

ENDFORM.                    " MODIFY_DATA
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field .

  CLEAR : gt_celltab[] .

*  IF gt_data-check_1 IS INITIAL .
  PERFORM set_field_enabled USING : 'IDNRK1' ,
                                    'MENGE1'.

*  ELSE .
*
*    PERFORM set_field_disabled USING : 'IDNRK1' ,
*                                       'MENGE1'.
*  ENDIF .
*.
*  IF gt_data-check_2 IS INITIAL .
  PERFORM set_field_enabled USING : 'IDNRK2' ,
                                    'MENGE2'.

*  ELSE .
*
*    PERFORM set_field_disabled USING : 'IDNRK2' ,
*                                       'MENGE2' .
*  ENDIF .
*.
*  IF gt_data-check_3 IS INITIAL .
  PERFORM set_field_enabled USING : 'IDNRK3' ,
                                    'MENGE3' .

*  ELSE .
*
*    PERFORM set_field_disabled USING : 'IDNRK3' ,
*                                       'MENGE3' .
*  ENDIF .
*.
*  IF gt_data-check_4 IS INITIAL .
  PERFORM set_field_enabled USING : 'IDNRK4' ,
                                    'MENGE4' .

*  ELSE .
*
*    PERFORM set_field_disabled USING : 'IDNRK4' ,
*                                       'MENGE4' .
*  ENDIF .
*.
*  IF gt_data-check_5 IS INITIAL .
  PERFORM set_field_enabled USING : 'IDNRK5' ,
                                    'MENGE5' .

*  ELSE .
*
*    PERFORM set_field_disabled USING : 'IDNRK5' ,
*                                       'MENGE5' .
*  ENDIF .
*.

  gt_data-celltab[] =  gt_celltab[] .

ENDFORM.                    " SET_FIELD
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_ENABLED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_enabled  USING p_field.

  CLEAR st_celltab.
  st_celltab-fieldname =  p_field.
  st_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
  INSERT st_celltab INTO TABLE gt_celltab.

ENDFORM.                    " SET_FIELD_ENABLED
*&---------------------------------------------------------------------*
*&      Form  SET_FIELD_DISABLED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_field_disabled  USING p_field.

  CLEAR st_celltab.
  st_celltab-fieldname =  p_field.
  st_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
  INSERT st_celltab INTO TABLE gt_celltab.

ENDFORM.                    " SET_FIELD_DISABLED
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM create_container_object  USING    p_sy_dynnr.

  CREATE OBJECT g_alv_doc_mdat
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = g_alv_doc_mdat->dock_at_left
      extension = 2700 ." 1500.

  CREATE OBJECT g_grid_mdat
    EXPORTING
      i_parent = g_alv_doc_mdat.

  CREATE OBJECT g_event_mdat.


ENDFORM.                    " CREATE_CONTAINER_OBJECT
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid  USING    p_dynnr.

*celltab
  st_lay-stylefname = 'CELLTAB'.
*  st_lay-ctab_fname = 'ALV_COLOR'.
*columns
*  st_lay-edit = 'X'.
*  ST_LAY-CWIDTH_OPT = 'X'.
  st_lay-zebra      = 'X'.

*  st_lay-box_fname  = 'MARK'.
*      ST_LAY-SEL_MODE = 'A'.
*      st_lay-grid_title  = g_title.

*      ST_LAY-GRID_TITLE  = G_TITLE.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_field_catalog  USING    p_dynnr.

  DATA: l_tabname  TYPE tabname ,
        l_tabix    TYPE sytabix .

  l_tabname = 'ZHKPMS0040' .

  CLEAR: gt_field[].
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_bypassing_buffer = 'X'
      i_buffer_active    = 'X'
      i_structure_name   = l_tabname
    CHANGING
      ct_fieldcat        = gt_field.

  LOOP AT gt_field INTO st_field.
    l_tabix = sy-tabix .
    CASE st_field-fieldname.
      WHEN 'ICON'.
        st_field-reptext   = 'Status' .
        st_field-scrtext_m = 'Status' .
        st_field-coltext   = 'Status' .
        st_field-key       = 'X'.
        st_field-outputlen = 4 .
        MODIFY gt_field FROM st_field.
      WHEN 'WERKS'.
        st_field-reptext   = 'Plant' .
        st_field-scrtext_m = 'Plant'.
        st_field-coltext   = 'Plant'.
        st_field-key       = 'Plant'.
        st_field-outputlen = 4 .
        st_field-key       = 'X'.
*        st_field-just      = c_c .
*        st_field-no_out    = c_x .
        MODIFY gt_field FROM st_field.
      WHEN 'EQUNR'.
        st_field-reptext   = 'Equipment' .
        st_field-scrtext_m = 'Equipment' .
        st_field-coltext   = 'Equipment' .
        st_field-key       = 'X'.
        st_field-outputlen = 20 .
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNTY'.
        st_field-reptext   = 'Type' .
        st_field-scrtext_m = 'Type' .
        st_field-coltext   = 'Type' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNNR'.
        st_field-reptext   = 'Group' .
        st_field-scrtext_m = 'Group' .
        st_field-coltext   = 'Group' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNAL'.
        st_field-reptext   = 'Grcounter' .
        st_field-scrtext_m = 'Grcounter' .
        st_field-coltext   = 'Grcounter' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNKN' .
        st_field-reptext   = 'Node' .
        st_field-scrtext_m = 'Node' .
        st_field-coltext   = 'Node' .
        st_field-key       = 'X'.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'KTEXT'.
        st_field-reptext   = 'Desc' .
        st_field-scrtext_m = 'Desc' .
        st_field-coltext   = 'Desc' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'VORNR' .
        st_field-reptext   = 'Op' .
        st_field-scrtext_m = 'Op' .
        st_field-coltext   = 'Op' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
*        st_field-outputlen = 10 .
        MODIFY gt_field FROM st_field.
      WHEN 'LTXA1'.
        st_field-reptext   = 'Op Text' .
        st_field-scrtext_m = 'Op Text' .
        st_field-coltext   = 'Op Text' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.
      WHEN 'WARPL'.
        st_field-reptext   = 'Mnt Plan' .
        st_field-scrtext_m = 'Mnt Plan' .
        st_field-coltext   = 'Mnt Plan' .
        st_field-key       = ' '.
        st_field-edit      = ' '.
        MODIFY gt_field FROM st_field.
      WHEN 'ZYKL1'.
        st_field-reptext   = 'Cycle' .
        st_field-scrtext_m = 'Cycle' ..
        st_field-coltext   = 'Cycle' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        st_field-outputlen = 06 .
        MODIFY gt_field FROM st_field.

      WHEN 'ZEIEH'.
        st_field-reptext   = 'Unit' .
        st_field-scrtext_m = 'Unit' .
        st_field-coltext   = 'Unit' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'STADT'.
        st_field-reptext   = 'Start'.
        st_field-scrtext_m = 'Start'.
        st_field-coltext   = 'Start'.
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'TXT'.
        st_field-reptext   = 'Sort' .
        st_field-scrtext_m = 'Sort' .
        st_field-coltext   = 'Sort' .
        st_field-key       = ' '.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'SORTL'.
        st_field-reptext   = 'PM Type' .
        st_field-scrtext_m = 'PM Type' .
        st_field-coltext   = 'PM Type' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'IDNRK1'.
        st_field-reptext   = 'Comp1' .
        st_field-scrtext_m = 'Comp1' .
        st_field-coltext   = 'Comp1' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 18 .
        MODIFY gt_field FROM st_field.
      WHEN 'MENGE1'.
        st_field-reptext   = 'Qty1' .
        st_field-scrtext_m = 'Qty1' .
        st_field-coltext   = 'Qty1' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 5 .
        MODIFY gt_field FROM st_field.
      WHEN 'MEINS1'.
        st_field-reptext   = 'Unit1' .
        st_field-scrtext_m = 'Unit1' .
        st_field-coltext   = 'Unit1' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'IDNRK2'.
        st_field-reptext   = 'Comp2' .
        st_field-scrtext_m = 'Comp2' .
        st_field-coltext   = 'Comp2' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 18 .
        MODIFY gt_field FROM st_field.
      WHEN 'MENGE2'.
        st_field-reptext   = 'Qty2' .
        st_field-scrtext_m = 'Qty2' .
        st_field-coltext   = 'Qty2' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 05 .
        MODIFY gt_field FROM st_field.
      WHEN 'MEINS2'.
        st_field-reptext   = 'Unit2' .
        st_field-scrtext_m = 'Unit2' .
        st_field-coltext   = 'Unit2' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'IDNRK3'.
        st_field-reptext   = 'Comp3' .
        st_field-scrtext_m = 'Comp3' .
        st_field-coltext   = 'Comp3' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 18 .
        MODIFY gt_field FROM st_field.
      WHEN 'MENGE3'.
        st_field-reptext   = 'Qty3' .
        st_field-scrtext_m = 'Qty3' .
        st_field-coltext   = 'Qty3' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 05 .
        MODIFY gt_field FROM st_field.
      WHEN 'MEINS3'.
        st_field-reptext   = 'Unit3' .
        st_field-scrtext_m = 'Unit3' .
        st_field-coltext   = 'Unit3' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'IDNRK4'.
        st_field-reptext   = 'Comp4' .
        st_field-scrtext_m = 'Comp4' .
        st_field-coltext   = 'Comp4' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 18 .
        MODIFY gt_field FROM st_field.
      WHEN 'MENGE4'.
        st_field-reptext   = 'Qty4' .
        st_field-scrtext_m = 'Qty4' .
        st_field-coltext   = 'Qty4' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 05 .
        MODIFY gt_field FROM st_field.
      WHEN 'MEINS4'.
        st_field-reptext   = 'Unit4' .
        st_field-scrtext_m = 'Unit4' .
        st_field-coltext   = 'Unit4' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'IDNRK5'.
        st_field-reptext   = 'Comp5' .
        st_field-scrtext_m = 'Comp5' .
        st_field-coltext   = 'Comp5' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 18 .
        MODIFY gt_field FROM st_field.
      WHEN 'MENGE5'.
        st_field-reptext   = 'Qty5' .
        st_field-scrtext_m = 'Qty5' .
        st_field-coltext   = 'Qty5' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        st_field-outputlen = 05 .
        MODIFY gt_field FROM st_field.
      WHEN 'MEINS5'.
        st_field-reptext   = 'Unit5' .
        st_field-scrtext_m = 'Unit5' .
        st_field-coltext   = 'Unit5' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'MESG' .
        st_field-reptext   = 'Message' .
        st_field-scrtext_m = 'Message' .
        st_field-coltext   = 'Message' .
        st_field-key       = ' '.
        st_field-outputlen = 30 .
        MODIFY gt_field FROM st_field.
      WHEN OTHERS .
        DELETE gt_field .
    ENDCASE .
  ENDLOOP.


ENDFORM.                    " BUILD_FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Form  EXCLUDING_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excluding_functions  USING pa_dynnr.
  DATA ls_exclude TYPE ui_func.


  CLEAR: gt_exclude, gt_exclude[].
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO gt_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO gt_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FULL.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_SOFT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FILTER .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FIND .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.

**  IF PA_DYNNR <> '0400'.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  ENDIF.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MAXIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MINIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_AVERAGE .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.

ENDFORM.                    " EXCLUDING_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_DYNNR  text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv  USING    p_dynnr.

  CALL METHOD g_grid_mdat->set_table_for_first_display
    EXPORTING      "
      it_toolbar_excluding = gt_exclude
      is_layout            = st_lay
      i_save               = 'x'
    CHANGING
      it_fieldcatalog      = gt_field[]
      it_outtab            = gt_data[].

* Set editable cells to ready for input initially
  CALL METHOD g_grid_mdat->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.

ENDFORM.                    " ASSIGN_ITAB_TO_ALV
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EVENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_event  USING    p_dynnr.

*  SET HANDLER g_event_mdat->hotspot_click_item   FOR g_grid_mdat.
*  SET HANDLER G_EVENT_mdat->DOUBLE_CLICK         FOR G_GRID_MDAT.
*  SET HANDLER g_event_mdat->toolbar_item         FOR g_grid_mdat.
*  SET HANDLER g_event_mdat->user_command_item    FOR g_grid_mdat .
*  SET HANDLER G_EVENT_mdat->DATA_CHANGED         FOR G_GRID_MDAT .

  CALL METHOD g_grid_mdat->set_toolbar_interactive.

  CALL METHOD g_grid_mdat->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.                    " ASSIGN_EVENT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_data  USING p_dynnr.

  DATA: ls_stable TYPE lvc_s_stbl.

  ls_stable-row = 'x'.
  ls_stable-col = 'x'.

  CALL METHOD g_grid_mdat->refresh_table_display
    EXPORTING
      is_stable = ls_stable.

ENDFORM.                    " REFRESH_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATE_CONTAINER_OBJECT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_container_object_0200  USING    p_sy_dynnr.

  CREATE OBJECT g_alv_doc_item
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = g_alv_doc_item->dock_at_left
      extension = 2700. "1500.

*  CREATE OBJECT g_alv_doc_item
*    EXPORTING
*      CONTAINER_NAME              = 'CUSTOM_CONTAINER'
*    EXCEPTIONS
*      CNTL_ERROR                  = 1
*      CNTL_SYSTEM_ERROR           = 2
*      CREATE_ERROR                = 3
*      LIFETIME_ERROR              = 4
*      LIFETIME_DYNPRO_DYNPRO_LINK = 5.
*  IF SY-SUBRC <> 0.
**    MESSAGE A000(TREE_CONTROL_MSG).
*  ENDIF.

  CREATE OBJECT g_grid_item
    EXPORTING
      i_parent = g_alv_doc_item.

  CREATE OBJECT g_event_item.

ENDFORM.                    " CREATE_CONTAINER_OBJECT_0200
*&---------------------------------------------------------------------*
*&      Form  SET_ATTRIBUTES_ALV_GRID_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_attributes_alv_grid_0200 USING    p_dynnr.

  CLEAR st_lay .
*celltab
*  st_lay-stylefname = 'CELLTAB'.
*  st_lay-ctab_fname = 'ALV_COLOR'.
*columns
*  st_lay-edit = 'X'.
*  ST_LAY-CWIDTH_OPT = 'X'.
  st_lay-zebra      = 'X'.

*  st_lay-box_fname  = 'MARK'.
*      ST_LAY-SEL_MODE = 'A'.
*      st_lay-grid_title  = g_title.

*      ST_LAY-GRID_TITLE  = G_TITLE.

ENDFORM.                    " SET_ATTRIBUTES_ALV_GRID_0200
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELD_CATALOG_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_field_catalog_0200  USING    p_dynnr.

  DATA: l_tabname  TYPE tabname ,
        l_tabix    TYPE sytabix .

  l_tabname = 'ZHKPMS0050' .

  CLEAR: gt_field[].
  SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_bypassing_buffer = 'X'
      i_buffer_active    = 'X'
      i_structure_name   = l_tabname
    CHANGING
      ct_fieldcat        = gt_field.

  LOOP AT gt_field INTO st_field.
    l_tabix = sy-tabix .
    CASE st_field-fieldname.
      WHEN 'EQUNR'.
        st_field-reptext   = 'Equipment' .
        st_field-scrtext_m = 'Equipment' .
        st_field-coltext   = 'Equipment' .
        st_field-key       = 'X'.
        st_field-outputlen = 20 .
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNTY'.
        st_field-reptext   = 'Type' .
        st_field-scrtext_m = 'Type' .
        st_field-coltext   = 'Type' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNNR'.
        st_field-reptext   = 'Group' .
        st_field-scrtext_m = 'Group' .
        st_field-coltext   = 'Group' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNAL'.
        st_field-reptext   = 'Grcounter' .
        st_field-scrtext_m = 'Grcounter' .
        st_field-coltext   = 'Grcounter' .
        st_field-key       = ' '.
*        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'PLNKN' .
        st_field-reptext   = 'Node' .
        st_field-scrtext_m = 'Node' .
        st_field-coltext   = 'Node' .
        st_field-key       = 'X'.
        st_field-edit      = ' ' .
        MODIFY gt_field FROM st_field.

      WHEN 'KTEXT'.
        st_field-reptext   = 'Desc' .
        st_field-scrtext_m = 'Desc' .
        st_field-coltext   = 'Desc' .
        st_field-key       = ' '.
        MODIFY gt_field FROM st_field.

      WHEN 'LTXA1'.
        st_field-reptext   = 'Op Text' .
        st_field-scrtext_m = 'Op Text' .
        st_field-coltext   = 'Op Text' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN 'SORTL'.
        st_field-reptext   = 'PM Type' .
        st_field-scrtext_m = 'PM Type' .
        st_field-coltext   = 'PM Type' .
        st_field-key       = ' '.
        st_field-edit      = c_x .
        MODIFY gt_field FROM st_field.

      WHEN OTHERS .
        DELETE gt_field .
    ENDCASE .
  ENDLOOP.

ENDFORM.                    " BUILD_FIELD_CATALOG_0200
*&---------------------------------------------------------------------*
*&      Form  EXCLUDING_FUNCTIONS_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM excluding_functions_0200   USING pa_dynnr.
  DATA ls_exclude TYPE ui_func.


  CLEAR: gt_exclude, gt_exclude[].
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO gt_exclude.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO gt_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_ASC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SORT_DSC.
*  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO gt_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO gt_exclude.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FULL.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_SOFT.
**  APPEND LS_EXCLUDE TO GT_EXCLUDE.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FILTER .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_FIND .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.

**  IF PA_DYNNR <> '0400'.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUBTOT .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**    LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SUM .
**    APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  ENDIF.

**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MAXIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_MINIMUM .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.
**  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_AVERAGE .
**  APPEND LS_EXCLUDE TO IT_EXCLUDE.


ENDFORM.                    " EXCLUDING_FUNCTIONS_0200
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_ITAB_TO_ALV_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_itab_to_alv_0200  USING    p_dynnr.

  CALL METHOD g_grid_item->set_table_for_first_display
    EXPORTING      "
      it_toolbar_excluding = gt_exclude
      is_layout            = st_lay
      i_save               = 'x'
    CHANGING
      it_fieldcatalog      = gt_field[]
      it_outtab            = gt_item[].

* Set editable cells to ready for input initially
  CALL METHOD g_grid_item->set_ready_for_input
    EXPORTING
      i_ready_for_input = 1.


ENDFORM.                    " ASSIGN_ITAB_TO_ALV_0200
*&---------------------------------------------------------------------*
*&      Form  ASSIGN_EVENT_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM assign_event_0200  USING   p_dynnr.

*  SET HANDLER g_event_item->hotspot_click_item   FOR g_grid_item.
*  SET HANDLER G_EVENT_ITEM->DOUBLE_CLICK         FOR G_GRID_item.
  SET HANDLER g_event_item->toolbar_item         FOR g_grid_item.
  SET HANDLER g_event_item->user_command_item    FOR g_grid_item .
*  SET HANDLER G_EVENT_ITEM->DATA_CHANGED         FOR G_GRID_item .

  CALL METHOD g_grid_item->set_toolbar_interactive.

  CALL METHOD g_grid_item->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

ENDFORM.                    " ASSIGN_EVENT_0200
*&---------------------------------------------------------------------*
*&      Form  REFRESH_DATA_0200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM refresh_data_0200   USING p_dynnr.

  DATA: ls_stable TYPE lvc_s_stbl.

  ls_stable-row = 'x'.
  ls_stable-col = 'x'.

  CALL METHOD g_grid_item->refresh_table_display
    EXPORTING
      is_stable = ls_stable.

ENDFORM.                    " REFRESH_DATA_0200
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ZOPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_zope .

  DATA : l_answer .
  DATA : l_valid ,
         l_error .
  DATA : l_check_error .


  CALL METHOD g_grid_mdat->check_changed_data
    IMPORTING
      e_valid = l_valid.

  REFRESH g_rows_t.
  CALL METHOD g_grid_mdat->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  DESCRIBE TABLE g_rows_t LINES sy-tfill .

  IF sy-tfill > 1 .
    MESSAGE s000 WITH 'Please select only one line.' .
    EXIT.
  ENDIF .

  CLEAR : gt_item[], gt_item .

  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_data INDEX g_rows_s-index.
    IF sy-subrc = 0 .

      DO 30 TIMES .
        MOVE-CORRESPONDING gt_data TO gt_item .
        CLEAR : gt_item-sortl ,gt_item-ltxa1 .
        APPEND gt_item .
      ENDDO .

    ENDIF .

  ENDLOOP .

  CALL SCREEN 0200 ."STARTING AT 5 5  .


ENDFORM.                    " PROCESS_ZOPE
*&---------------------------------------------------------------------*
*&      Form  EVENT_TOOLBAR_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_toolbar_item USING p_e_object TYPE REF TO cl_alv_event_toolbar_set
                          p_e_interactive TYPE char1.

  DATA: ls_toolbar TYPE stb_button.


  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.
*
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_INSERT'.
  ls_toolbar-icon      = icon_insert_row.
  ls_toolbar-disabled  = space.
*  LS_TOOLBAR-TEXT      = ''.
*  LS_TOOLBAR-QUICKINFO = ' '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.


  CLEAR ls_toolbar.
  ls_toolbar-butn_type = '3'.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.
*
  CLEAR ls_toolbar.
  ls_toolbar-function  = 'L_DELETE'.
  ls_toolbar-icon      = icon_delete_row .
  ls_toolbar-disabled  = space .
*  LS_TOOLBAR-TEXT      = ' ' .
*  LS_TOOLBAR-QUICKINFO = ' '.
*  LS_TOOLBAR-CHECKED   = SPACE.
  APPEND ls_toolbar TO p_e_object->mt_toolbar.

ENDFORM.                    " EVENT_TOOLBAR_ITEM
*&---------------------------------------------------------------------*
*&      Form  EVENT_UCOMM_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM event_ucomm_item  USING p_e_ucomm.
*
  CASE p_e_ucomm.
    WHEN 'L_INSERT'.
      PERFORM  line_insert .
    WHEN 'L_DELETE' .
      PERFORM  line_delete .
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " EVENT_UCOMM_ITEM
*&---------------------------------------------------------------------*
*&      Form  LINE_INSERT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_insert .
  DATA : lt_item LIKE gt_item OCCURS 0 WITH HEADER LINE .

  REFRESH g_rows_t.
  CALL METHOD g_grid_item->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  LOOP AT gt_item .

    READ TABLE g_rows_t INTO g_rows_s WITH KEY INDEX = sy-tabix .

    IF sy-subrc = 0 .
      CLEAR lt_item .
      MOVE-CORRESPONDING gt_data TO lt_item .
      CLEAR : lt_item-sortl ,lt_item-ltxa1 .
      APPEND lt_item .
    ENDIF .

    APPEND gt_item TO lt_item .

  ENDLOOP .

  gt_item[] = lt_item[] .

  LOOP AT gt_item .
    PERFORM set_field .
    MODIFY gt_item .
  ENDLOOP .

ENDFORM.                    " LINE_INSERT
*&---------------------------------------------------------------------*
*&      Form  LINE_DELETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM line_delete .

  DATA : lt_item LIKE gt_item OCCURS 0 WITH HEADER LINE .

  REFRESH g_rows_t.
  CALL METHOD g_grid_item->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  LOOP AT gt_item .

    READ TABLE g_rows_t INTO g_rows_s WITH KEY INDEX = sy-tabix .

    IF sy-subrc <> 0 .
      APPEND gt_item TO lt_item .
    ENDIF .

  ENDLOOP .

  gt_item[] = lt_item[] .

ENDFORM.                    " LINE_DELETE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ZOPSAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_zopsave .
  DATA : l_answer .
  DATA : l_valid ,
         l_error .
  DATA : l_check_error .
  DATA : l_domname LIKE dd07l-domname .
  DATA : lt_tab    LIKE TABLE OF dd07v WITH HEADER LINE .

  DATA : lt_data LIKE gt_data OCCURS 0 WITH HEADER LINE .

  CALL METHOD g_grid_item->check_changed_data
    IMPORTING
      e_valid = l_valid.

  REFRESH g_rows_t.
  CALL METHOD g_grid_item->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  l_domname = 'ZCPMSORTL' .
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = l_domname
    TABLES
      values_tab      = lt_tab
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_item INDEX g_rows_s-index.
    IF sy-subrc = 0 .
      READ TABLE lt_tab WITH KEY domvalue_l = gt_item-sortl .
      IF sy-subrc <> 0 .
        l_error = c_x .
      ENDIF .
      IF gt_item-ltxa1 IS INITIAL .
        l_error = c_x .
      ENDIF .
    ENDIF .
  ENDLOOP .

  IF l_error IS NOT INITIAL .
    MESSAGE s000 WITH 'Error. Check out.'.
    EXIT.
  ENDIF .


  DATA : lt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE .

  CLEAR :  gt_bdctab[] .
*. Set BDC
  PERFORM dynpro USING:
     'X' 'SAPLCPDI'             '3010' ,
     ' ' 'BDC_OKCODE'           '=VOUE' ,
     ' ' 'RC27E-EQUNR'          gt_data-equnr ,
     ' ' 'RC271-PLNAL'          gt_data-plnal  .


  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_item INDEX g_rows_s-index.
    IF sy-subrc = 0 .

      IF gt_item-sortl  IS NOT INITIAL AND
         gt_item-ltxa1  IS NOT INITIAL .
      ELSE .
        CONTINUE .
      ENDIF .

      PERFORM dynpro USING:
        'X' 'SAPLCPDI'             '3400' ,
        ' ' 'BDC_OKCODE'           '=P+' ,

        'X' 'SAPLCPDI'             '3400' ,
        ' ' 'BDC_OKCODE'           '/00' ,
        ' ' 'RC27X-FLG_SEL(01)'    ' ' ,
        ' ' 'BDC_CURSOR'           'PLPOD-LTXA1(02)' ,
        ' ' 'PLPOD-LTXA1(02)'      gt_item-ltxa1 ,

        'X' 'SAPLCPDI'             '3400' ,
        ' ' 'BDC_OKCODE'           '=VOFL' ,
        ' ' 'RC27X-FLG_SEL(02)'    'X' ,

        'X' 'SAPLCPDO'             '3380' ,
        ' ' 'BDC_OKCODE'           '/00' ,
        ' ' 'PLPOD-SORTL'          gt_item-sortl ,

        'X' 'SAPLCPDO'             '3380' ,
        ' ' 'BDC_OKCODE'           '=BACK' .

    ENDIF .
  ENDLOOP .

  PERFORM dynpro USING:
    'X' 'SAPLCPDI'             '3400' ,
    ' ' 'BDC_OKCODE'           '=BU'  .

  MOVE  : ' ' TO ctu_params-nobinpt,
          's' TO ctu_params-updmode,
          'x' TO ctu_params-defsize,
          c_n TO ctu_params-dismode.

  CALL TRANSACTION 'IA02' USING         gt_bdctab
                          OPTIONS FROM  ctu_params
                          MESSAGES INTO gt_msgtab.


  DESCRIBE TABLE gt_msgtab LINES sy-tabix.
  READ TABLE gt_msgtab INDEX sy-tabix.


  IF gt_msgtab-msgtyp EQ 'S'. " AND
*    ( gt_msgtab-msgnr  BETWEEN '100' AND '100' ) .

    PERFORM get_operation_data .

    SORT gt_data BY werks plnty plnnr plnal  vornr .

    LOOP AT gt_oper .
      READ TABLE gt_data WITH KEY werks = gt_oper-werks
                                  plnty = gt_oper-plnty
                                  plnnr = gt_oper-plnnr
                                  plnal = gt_oper-plnal
                                  vornr = gt_oper-vornr
                                  BINARY SEARCH .
      IF sy-subrc <> 0 .
        MOVE-CORRESPONDING gt_data TO lt_data .
        MOVE-CORRESPONDING gt_oper TO lt_data .
        APPEND lt_data .
      ENDIF .
    ENDLOOP .

    APPEND LINES OF lt_data TO gt_data .

    SORT gt_data BY werks equnr plnty plnnr plnal plnkn vornr .

    lt_return-id     = gt_msgtab-msgid .
    lt_return-number = gt_msgtab-msgnr .
    APPEND lt_return .
    CALL FUNCTION 'COM_READ_MESSAGE_TEXTS'
      EXPORTING
        iv_language = sy-langu
      CHANGING
        ct_return   = lt_return[].

    READ TABLE lt_return INDEX 1.

    MESSAGE s000 WITH lt_return-message.
    .
    LEAVE TO SCREEN 0.
  ELSE.

    lt_return-id     = gt_msgtab-msgid .
    lt_return-number = gt_msgtab-msgnr .
    APPEND lt_return .
    CALL FUNCTION 'COM_READ_MESSAGE_TEXTS'
      EXPORTING
        iv_language = sy-langu
      CHANGING
        ct_return   = lt_return[].

    READ TABLE lt_return INDEX 1.

    MESSAGE s000 WITH lt_return-message.

  ENDIF .

ENDFORM.                    " PROCESS_ZOPSAVE
*&---------------------------------------------------------------------*
*&      Form  DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM dynpro  USING    p_dynbegin
                      p_name
                      p_value.

  IF p_dynbegin EQ c_x .
    CLEAR gt_bdctab.
    MOVE : p_name  TO gt_bdctab-program,
           p_value TO gt_bdctab-dynpro,
           c_x   TO gt_bdctab-dynbegin.
    APPEND gt_bdctab.

  ELSE.
    CLEAR gt_bdctab.
    MOVE : p_name  TO gt_bdctab-fnam,
           p_value TO gt_bdctab-fval.
    APPEND gt_bdctab.

  ENDIF.


ENDFORM.                    " DYNPRO
*&---------------------------------------------------------------------*
*&      Form  GET_OPERATION_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_operation_data .
  DATA : lt_t001k LIKE TABLE OF t001k WITH HEADER LINE ,
         lt_t001  LIKE TABLE OF t001  WITH HEADER LINE ,
         lt_plko  LIKE TABLE OF plko  WITH HEADER LINE ,
         lt_plpo  LIKE TABLE OF plpo  WITH HEADER LINE ,
         lt_plas  LIKE TABLE OF plas  WITH HEADER LINE ,
         lt_plmz  LIKE TABLE OF plmz  WITH HEADER LINE ,
         lt_stpo  LIKE TABLE OF stpo  WITH HEADER LINE .

  DATA : lt_eapl  LIKE TABLE OF eapl  WITH HEADER LINE .


  CLEAR : gt_oper[], gt_oper .

*. Get row data : Task list - header
  SELECT *
    INTO TABLE lt_plko
    FROM plko
   WHERE werks IN s_werks
     AND plnty EQ 'E'
     AND plnnr EQ gt_data-plnnr
     AND plnal EQ gt_data-plnal .

  DELETE lt_plko WHERE loekz = c_x .

  IF lt_plko[] IS NOT INITIAL .

*.. Allocation of task lists to pieces of equipment
    SELECT * INTO TABLE lt_eapl
      FROM eapl
      FOR ALL ENTRIES IN lt_plko
     WHERE plnty EQ lt_plko-plnty
       AND plnnr EQ lt_plko-plnnr
       AND plnal EQ lt_plko-plnal
       AND zaehl EQ lt_plko-zaehl
       AND equnr EQ gt_data-equnr .

    SORT lt_eapl BY plnty plnnr plnal zaehl .

    LOOP AT lt_plko .

      READ TABLE lt_eapl WITH KEY plnty = lt_plko-plnty
                                  plnnr = lt_plko-plnnr
                                  plnal = lt_plko-plnal
                                  zaehl = lt_plko-zaehl
                                  BINARY SEARCH .
      IF sy-subrc <> 0 .
        DELETE lt_plko .
      ENDIF .


    ENDLOOP .
  ENDIF .
*.
  IF lt_plko[] IS NOT INITIAL .

*.. Task list - selection of operations/activities
    SELECT * INTO TABLE lt_plas
      FROM plas
      FOR ALL ENTRIES IN lt_plko
     WHERE plnty EQ lt_plko-plnty
       AND plnnr EQ lt_plko-plnnr
       AND plnal EQ lt_plko-plnal .

    DELETE lt_plas WHERE loekz <> space .

*.. Task list - operation/activity
    IF lt_plas[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_plpo
        FROM plpo
        FOR ALL ENTRIES IN lt_plas
       WHERE plnty EQ lt_plas-plnty
         AND plnnr EQ lt_plas-plnnr
         AND plnkn EQ lt_plas-plnkn  .
    ENDIF .

*.. Allocation of bill of material items to operations
    IF lt_plas[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_plmz
        FROM plmz
        FOR ALL ENTRIES IN lt_plas
       WHERE plnty EQ lt_plas-plnty
         AND plnnr EQ lt_plas-plnnr
         AND plnal EQ lt_plas-plnal
         AND plnkn EQ lt_plas-plnkn  .
    ENDIF .

**.. BOM item
    IF lt_plmz[] IS NOT INITIAL .
      SELECT * INTO TABLE lt_stpo
        FROM stpo
        FOR ALL ENTRIES IN lt_plmz
       WHERE stlty EQ lt_plmz-stlty
         AND stlnr EQ lt_plmz-stlnr
         AND stlkn EQ lt_plmz-stlkn  .
    ENDIF .


  ENDIF .

*.
  SORT lt_eapl BY plnty plnnr plnal zaehl .
  SORT lt_plas BY plnty plnnr plnal .
  SORT lt_plpo BY plnty plnnr plnkn .
  SORT lt_plmz BY plnty plnnr plnal plnkn .
  SORT lt_stpo BY stlty stlnr stlkn .

  LOOP AT lt_plko .

    CLEAR   gt_oper .

    gt_oper-werks = lt_plko-werks .


    READ TABLE lt_eapl WITH KEY plnty = lt_plko-plnty
                                plnnr = lt_plko-plnnr
                                plnal = lt_plko-plnal
                                zaehl = lt_plko-zaehl
                                BINARY SEARCH .
    IF sy-subrc = 0 .
      gt_oper-zfleq = lt_eapl-equnr .
    ENDIF .

    gt_oper-plnty = lt_plko-plnty .
    gt_oper-plnnr = lt_plko-plnnr .
    gt_oper-plnal = lt_plko-plnal .

    gt_oper-datuv = lt_plko-datuv .
    gt_oper-andat = lt_plko-andat .
    gt_oper-aedat = lt_plko-aedat .
    gt_oper-verwe = lt_plko-verwe .
    gt_oper-ktext = lt_plko-ktext .

    READ TABLE lt_plas WITH KEY plnty = lt_plko-plnty
                                plnnr = lt_plko-plnnr
                                plnal = lt_plko-plnal
                                BINARY SEARCH .
**+ 1
    IF sy-subrc = 0 . "+ 3
      LOOP AT lt_plas FROM sy-tabix . "+ 2
        IF lt_plas-plnty <> lt_plko-plnty OR
           lt_plas-plnnr <> lt_plko-plnnr OR
           lt_plas-plnal <> lt_plko-plnal .
          EXIT.
        ENDIF .

        gt_oper-plnkn = lt_plas-plnkn .

        READ TABLE lt_plpo WITH KEY plnty = lt_plas-plnty
                                    plnnr = lt_plas-plnnr
                                    plnkn = lt_plas-plnkn
                                    BINARY SEARCH .
        IF sy-subrc = 0 .
          gt_oper-vornr = lt_plpo-vornr .
          gt_oper-ltxa1 = lt_plpo-ltxa1 .
          gt_oper-sortl = lt_plpo-sortl .
          gt_oper-odatuv = lt_plpo-datuv .
          gt_oper-oandat = lt_plpo-andat .
          gt_oper-oaedat = lt_plpo-aedat .

        ELSE .
          CLEAR :
          gt_oper-vornr ,
          gt_oper-ltxa1 ,
          gt_oper-sortl ,
          gt_oper-odatuv ,
          gt_oper-oandat ,
          gt_oper-oaedat .

        ENDIF .

        CLEAR :
        gt_oper-idnrk1 ,
        gt_oper-menge1 ,
        gt_oper-meins1 ,
        gt_oper-idnrk2 ,
        gt_oper-menge2 ,
        gt_oper-meins2 ,
        gt_oper-idnrk3 ,
        gt_oper-menge3 ,
        gt_oper-meins3 ,
        gt_oper-idnrk4 ,
        gt_oper-menge4 ,
        gt_oper-meins4 ,
        gt_oper-idnrk5 ,
        gt_oper-menge5 ,
        gt_oper-meins5 .

        READ TABLE lt_plmz WITH KEY plnty = lt_plas-plnty
                                    plnnr = lt_plas-plnnr
                                    plnal = lt_plas-plnal
                                    plnkn = lt_plas-plnkn
                                    BINARY SEARCH .
        IF sy-subrc = 0 . "+ 1
          LOOP AT lt_plmz FROM sy-tabix .
            IF lt_plmz-plnty <> lt_plas-plnty OR
               lt_plmz-plnnr <> lt_plas-plnnr OR
               lt_plmz-plnal <> lt_plas-plnal OR
               lt_plmz-plnkn <> lt_plas-plnkn .
              EXIT .
            ENDIF .

            READ TABLE lt_stpo WITH KEY stlty = lt_plmz-stlty
                                        stlnr = lt_plmz-stlnr
                                        stlkn = lt_plmz-stlkn
                                        BINARY SEARCH .
            IF sy-subrc = 0 .
              LOOP AT lt_stpo FROM sy-tabix .

                IF lt_stpo-stlty <> lt_plmz-stlty OR
                   lt_stpo-stlnr <> lt_plmz-stlnr OR
                   lt_stpo-stlkn <> lt_plmz-stlkn .
                  EXIT .
                ENDIF .

                IF gt_oper-idnrk1 IS INITIAL .
                  gt_oper-idnrk1 = lt_stpo-idnrk .
                  gt_oper-menge1 = lt_stpo-menge .
                  gt_oper-meins1 = lt_stpo-meins .
                ELSEIF gt_oper-idnrk2 IS INITIAL .
                  gt_oper-idnrk2 = lt_stpo-idnrk .
                  gt_oper-menge2 = lt_stpo-menge .
                  gt_oper-meins2 = lt_stpo-meins .
                ELSEIF gt_oper-idnrk3 IS INITIAL .
                  gt_oper-idnrk3 = lt_stpo-idnrk .
                  gt_oper-menge3 = lt_stpo-menge .
                  gt_oper-meins3 = lt_stpo-meins .
                ELSEIF gt_oper-idnrk4 IS INITIAL .
                  gt_oper-idnrk4 = lt_stpo-idnrk .
                  gt_oper-menge4 = lt_stpo-menge .
                  gt_oper-meins4 = lt_stpo-meins .
                ELSEIF gt_oper-idnrk5 IS INITIAL .
                  gt_oper-idnrk5 = lt_stpo-idnrk .
                  gt_oper-menge5 = lt_stpo-menge .
                  gt_oper-meins5 = lt_stpo-meins .
                ENDIF .

              ENDLOOP .
            ENDIF .

          ENDLOOP .
        ENDIF .  "+ 1

        APPEND gt_oper .

      ENDLOOP. "+ 2

    ENDIF . "+ 3

  ENDLOOP .

ENDFORM.                    " GET_OPERATION_DATA
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ZADD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_zadd .

  DATA : l_answer .
  DATA : l_valid ,
         l_error .
  DATA : l_check_error .


  CALL METHOD g_grid_mdat->check_changed_data
    IMPORTING
      e_valid = l_valid.

  REFRESH g_rows_t.
  CALL METHOD g_grid_mdat->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  gt_sort[] = gt_data[] .

  SORT gt_sort BY werks plnty plnnr plnal plnkn .

  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_data INDEX g_rows_s-index.
    IF sy-subrc = 0 .
      CLEAR : l_error .

      PERFORM check_zadd_data USING l_error .

      IF l_error IS NOT INITIAL .
        MODIFY gt_data INDEX g_rows_s-index.
        CONTINUE .
      ENDIF .

      PERFORM call_bdc_add_conp.
      MODIFY gt_data INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .

ENDFORM.                    " PROCESS_ZADD
*&---------------------------------------------------------------------*
*&      Form  CHECK_ZADD_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ERROR  text
*----------------------------------------------------------------------*
FORM check_zadd_data  USING    p_error.

  IF gt_data-idnrk1 IS NOT INITIAL  OR
     gt_data-menge1 IS NOT INITIAL .
    SELECT SINGLE mandt INTO sy-mandt
      FROM mara
     WHERE matnr EQ gt_data-idnrk1 .
    IF sy-subrc <> 0 .
      gt_data-icon = '@0A@'."  Red light; negative
      gt_data-mesg = 'Please. Check out Component1.' .
      p_error = c_x .
      EXIT .
    ENDIF .
  ENDIF .

  IF gt_data-idnrk2 IS NOT INITIAL  OR
     gt_data-menge2 IS NOT INITIAL .
    SELECT SINGLE mandt INTO sy-mandt
      FROM mara
     WHERE matnr EQ gt_data-idnrk2 .
    IF sy-subrc <> 0 .
      gt_data-icon = '@0A@'."  Red light; negative
      gt_data-mesg = 'Please. Check out Component2.' .
      p_error = c_x .
      EXIT .
    ENDIF .
  ENDIF .

  IF gt_data-idnrk3 IS NOT INITIAL  OR
     gt_data-menge3 IS NOT INITIAL .

    SELECT SINGLE mandt INTO sy-mandt
      FROM mara
     WHERE matnr EQ gt_data-idnrk3 .
    IF sy-subrc <> 0 .
      gt_data-icon = '@0A@'."  Red light; negative
      gt_data-mesg = 'Please. Check out Component3.' .
      p_error = c_x .
      EXIT .
    ENDIF .
  ENDIF .

  IF gt_data-idnrk4 IS NOT INITIAL  OR
     gt_data-menge4 IS NOT INITIAL .

    SELECT SINGLE mandt INTO sy-mandt
      FROM mara
     WHERE matnr EQ gt_data-idnrk4 .
    IF sy-subrc <> 0 .
      gt_data-icon = '@0A@'."  Red light; negative
      gt_data-mesg = 'Please. Check out Component4.' .
      p_error = c_x .
      EXIT .
    ENDIF .
  ENDIF .

  IF gt_data-idnrk5 IS NOT INITIAL  OR
     gt_data-menge5 IS NOT INITIAL .

    SELECT SINGLE mandt INTO sy-mandt
      FROM mara
     WHERE matnr EQ gt_data-idnrk5 .
    IF sy-subrc <> 0 .
      gt_data-icon = '@0A@'."  Red light; negative
      gt_data-mesg = 'Please. Check out Component5.' .
      p_error = c_x .
      EXIT .
    ENDIF .
  ENDIF .

  IF gt_data-idnrk1 IS NOT INITIAL AND
     gt_data-menge1 IS INITIAL .
    gt_data-icon = '@0A@'."  Red light; negative
    gt_data-mesg = 'Please. Check out Quantity1.' .
    p_error = c_x .
    EXIT .
  ENDIF .


  IF gt_data-idnrk2 IS NOT INITIAL AND
     gt_data-menge2 IS INITIAL .
    gt_data-icon = '@0A@'."  Red light; negative
    gt_data-mesg = 'Please. Check out Quantity2.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  IF gt_data-idnrk3 IS NOT INITIAL AND
     gt_data-menge3 IS INITIAL .
    gt_data-icon = '@0A@'."  Red light; negative
    gt_data-mesg = 'Please. Check out Quantity3.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  IF gt_data-idnrk4 IS NOT INITIAL AND
     gt_data-menge4 IS INITIAL .
    gt_data-icon = '@0A@'."  Red light; negative
    gt_data-mesg = 'Please. Check out Quantity4.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  IF gt_data-idnrk5 IS NOT INITIAL AND
     gt_data-menge5 IS INITIAL .
    gt_data-icon = '@0A@'."  Red light; negative
    gt_data-mesg = 'Please. Check out Quantity5.' .
    p_error = c_x .
    EXIT .
  ENDIF .

  PERFORM get_data_meins .

ENDFORM.                    " CHECK_ZADD_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_MEINS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data_meins .

  SELECT SINGLE meins INTO gt_data-meins1
    FROM mara
   WHERE matnr EQ gt_data-idnrk1 .

  SELECT SINGLE meins INTO gt_data-meins2
    FROM mara
   WHERE matnr EQ gt_data-idnrk2 .

  SELECT SINGLE meins INTO gt_data-meins3
    FROM mara
   WHERE matnr EQ gt_data-idnrk3 .

  SELECT SINGLE meins INTO gt_data-meins4
    FROM mara
   WHERE matnr EQ gt_data-idnrk4 .

  SELECT SINGLE meins INTO gt_data-meins5
    FROM mara
   WHERE matnr EQ gt_data-idnrk5 .

ENDFORM.                    " GET_DATA_MEINS
*&---------------------------------------------------------------------*
*&      Form  CALL_BDC_ADD_CONP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_bdc_add_conp .

  DATA : BEGIN OF lt_zadd OCCURS 0 ,
          idnrk LIKE gt_data-idnrk1 ,
          menge LIKE gt_data-menge1 ,
          meins LIKE mara-meins ,
         END OF lt_zadd .

  DATA : lt_return LIKE bapiret2 OCCURS 0 WITH HEADER LINE .

  DATA : l_count(2) TYPE n ,
         l_field(30) .

  READ TABLE gt_sort WITH KEY werks = gt_data-werks
                              plnty = gt_data-plnty
                              plnnr = gt_data-plnnr
                              plnal = gt_data-plnal
                              BINARY SEARCH .
  IF sy-subrc = 0 .
    LOOP AT gt_sort FROM sy-tabix .
      IF gt_sort-werks <> gt_data-werks OR
         gt_sort-plnty <> gt_data-plnty OR
         gt_sort-plnnr <> gt_data-plnnr OR
         gt_sort-plnal <> gt_data-plnal .
        EXIT .
      ENDIF .

      l_count = l_count + 1 .

      IF gt_sort-vornr = gt_data-vornr .
        EXIT .
      ENDIF .
    ENDLOOP .
  ENDIF .

  IF gt_data-idnrk1 IS NOT INITIAL .
    lt_zadd-idnrk = gt_data-idnrk1 .
    lt_zadd-menge = gt_data-menge1 .
    lt_zadd-meins = gt_data-meins1 .
    APPEND lt_zadd .
  ENDIF .
  IF gt_data-idnrk2 IS NOT INITIAL .
    lt_zadd-idnrk = gt_data-idnrk2 .
    lt_zadd-menge = gt_data-menge2 .
    lt_zadd-meins = gt_data-meins2 .
    APPEND lt_zadd .
  ENDIF .
  IF gt_data-idnrk3 IS NOT INITIAL .
    lt_zadd-idnrk = gt_data-idnrk3 .
    lt_zadd-menge = gt_data-menge3 .
    lt_zadd-meins = gt_data-meins3 .
    APPEND lt_zadd .
  ENDIF .
  IF gt_data-idnrk4 IS NOT INITIAL .
    lt_zadd-idnrk = gt_data-idnrk4 .
    lt_zadd-menge = gt_data-menge4 .
    lt_zadd-meins = gt_data-meins4 .
    APPEND lt_zadd .
  ENDIF .
  IF gt_data-idnrk5 IS NOT INITIAL .
    lt_zadd-idnrk = gt_data-idnrk5 .
    lt_zadd-menge = gt_data-menge5 .
    lt_zadd-meins = gt_data-meins5 .
    APPEND lt_zadd .
  ENDIF .

  DATA : l_menge(10) .
  CLEAR :  gt_bdctab[] .


  CONCATENATE 'RC27X-FLG_SEL(' l_count ')' INTO l_field .
*. Set BDC
  PERFORM dynpro USING:
     'X' 'SAPLCPDI'             '3010' ,
     ' ' 'BDC_OKCODE'           '=VOUE' ,
     ' ' 'RC27E-EQUNR'          gt_data-equnr ,
     ' ' 'RC271-PLNAL'          gt_data-plnal  ,

     'X' 'SAPLCPDI'             '3400' ,
     ' ' 'BDC_OKCODE'           '=MAPM' ,
     ' ' l_field                'X' ,

     'X' 'SAPLCMDI'             '3500' ,
     ' ' 'BDC_OKCODE'           '=PMB5'  ,

     'X' 'SAPLCMDI'             '3500' ,
     ' ' 'BDC_OKCODE'           '=PMB3'  .

  LOOP AT lt_zadd .

    WRITE lt_zadd-menge TO l_menge UNIT lt_zadd-meins .

    PERFORM dynpro USING:
      'X' 'SAPLCMDI'             '3500' ,
      ' ' 'BDC_OKCODE'           '/00' ,
      ' ' 'RIHSTPX-IDNRK(01)'    lt_zadd-idnrk ,
      ' ' 'RIHSTPX-MENGE(01)'    l_menge,

      'X' 'SAPLCMDI'             '3500' ,
      ' ' 'BDC_OKCODE'           '=P+' .
  ENDLOOP .


  PERFORM dynpro USING:
    'X' 'SAPLCMDI'             '3500' ,
    ' ' 'BDC_OKCODE'           '=BU'  .

  MOVE  : ' ' TO ctu_params-nobinpt,
          's' TO ctu_params-updmode,
          'x' TO ctu_params-defsize,
          c_n TO ctu_params-dismode.

  CALL TRANSACTION 'IA02' USING         gt_bdctab
                          OPTIONS FROM  ctu_params
                          MESSAGES INTO gt_msgtab.


  DESCRIBE TABLE gt_msgtab LINES sy-tabix.
  READ TABLE gt_msgtab INDEX sy-tabix.


  IF gt_msgtab-msgtyp EQ 'S'. " AND
*    ( gt_msgtab-msgnr  BETWEEN '100' AND '100' ) .
    gt_data-icon = '@08@' .
    CLEAR gt_data-mesg  .

  ELSE.

    lt_return-id     = gt_msgtab-msgid .
    lt_return-number = gt_msgtab-msgnr .
    APPEND lt_return .
    CALL FUNCTION 'COM_READ_MESSAGE_TEXTS'
      EXPORTING
        iv_language = sy-langu
      CHANGING
        ct_return   = lt_return[].

    READ TABLE lt_return INDEX 1.
    gt_data-icon = '@0A@' .

    gt_data-mesg = lt_return-message.

  ENDIF .

ENDFORM.                    " CALL_BDC_ADD_CONP
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ZSAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_zsave .

  DATA : l_answer .
  DATA : l_valid ,
         l_error .
  DATA : l_check_error .


  CALL METHOD g_grid_mdat->check_changed_data
    IMPORTING
      e_valid = l_valid.

  REFRESH g_rows_t.
  CALL METHOD g_grid_mdat->get_selected_rows
    IMPORTING
      et_index_rows = g_rows_t.
  IF g_rows_t[] IS INITIAL.
    MESSAGE s000 WITH 'not selection line.'.
    EXIT.
  ENDIF.

  LOOP AT g_rows_t INTO g_rows_s.

    READ TABLE gt_data INDEX g_rows_s-index.
    IF sy-subrc = 0 .
      CLEAR : l_error .

      PERFORM check_zsave_data USING l_error .

      IF l_error IS NOT INITIAL .
        MODIFY gt_data INDEX g_rows_s-index.
        CONTINUE .
      ENDIF .

      PERFORM update_pm_type .

      MODIFY gt_data INDEX g_rows_s-index.
    ENDIF .

  ENDLOOP .

ENDFORM.                    " PROCESS_ZSAVE
*&---------------------------------------------------------------------*
*&      Form  CHECK_ZSAVE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_zsave_data  USING    p_error.

  DATA : l_domname LIKE dd07l-domname .
  DATA : lt_tab    LIKE TABLE OF dd07v WITH HEADER LINE .

  l_domname = 'ZCPMSORTL' .
  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = l_domname
    TABLES
      values_tab      = lt_tab
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  READ TABLE lt_tab WITH KEY DOMVALUE_L = gt_data-sortl .
  IF sy-subrc <> 0 .
    gt_data-icon = '@0A@'."  Red light; negative
    gt_data-mesg = 'Please. Check out PM Type.' .
    p_error = c_x .
    EXIT .
  ENDIF .

ENDFORM.                    " CHECK_ZSAVE_DATA
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PM_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_pm_type .

  UPDATE plpo
     SET sortl = gt_data-sortl
   WHERE plnty = gt_data-plnty
     AND plnnr = gt_data-plnnr
     AND plnkn = gt_data-plnkn .

  IF sy-subrc = 0 .
    gt_data-icon = '@08@' .
    CLEAR gt_data-mesg  .
  ELSE .
    gt_data-icon = '@0A@' .
    gt_data-mesg = 'Error. Check out.' .
  ENDIF .

ENDFORM.                    " UPDATE_PM_TYPE
*&---------------------------------------------------------------------*
*&      Form  GET_OTHERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_others .
  DATA : l_valid  .


  CALL METHOD g_grid_mdat->check_changed_data
    IMPORTING
      e_valid = l_valid.

  LOOP AT gt_data .
    IF gt_data-idnrk1 IS NOT INITIAL OR
       gt_data-idnrk2 IS NOT INITIAL OR
       gt_data-idnrk3 IS NOT INITIAL OR
       gt_data-idnrk4 IS NOT INITIAL OR
       gt_data-idnrk5 IS NOT INITIAL .
      PERFORM get_data_meins .
      MODIFY gt_data .
    ENDIF .
  ENDLOOP .

ENDFORM.                    " GET_OTHERS
