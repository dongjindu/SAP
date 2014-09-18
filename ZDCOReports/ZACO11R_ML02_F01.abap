*----------------------------------------------------------------------*
*   INCLUDE ZACO11R_ML02_F01                                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   Macro
*----------------------------------------------------------------------*
DEFINE def_val.
  data :
          &1_lbkum like mlcd-lbkum,
          &1_salk3 like mlcd-salk3,
          &1_rd    like mlcd-salk3.
END-OF-DEFINITION .


*----------------------------------------------------------------------*
*   Data Definition
*----------------------------------------------------------------------*
** Type-Pools
TYPE-POOLS : ckmv0, slis.
TYPE-POOLS : zcot1.

** Tables
TABLES : ckmlpp, ckmlcr, ckmlhd, ckmlct.
TABLES : macku, marc, mara.
TABLES : mlcd_key, mlcd, mlcr, mlpp,
         mlhd, mlit.
TABLES : t001w, mbew.

* For MLCD
DATA : it_kalnr	  TYPE ckmv0_matobj_tbl WITH HEADER LINE.
DATA : it_ot_mlcd LIKE TABLE OF mlcd
                  WITH HEADER LINE ,
       it_ot_mlcd_not_alloc	
                  LIKE TABLE OF mlcd
                  WITH HEADER LINE .
DATA : BEGIN OF it_mlcd OCCURS 1000.
        INCLUDE STRUCTURE mlcd.
DATA : END OF   it_mlcd.

* For Display
DATA : BEGIN OF it_display OCCURS 500,
        bdatj LIKE mlcd-bdatj,
        poper LIKE mlcd-poper,
        mtart LIKE mara-mtart,
        kalnr LIKE mlcd-kalnr,
        matnr LIKE ckmlhd-matnr,
        bwkey LIKE ckmlhd-bwkey,
        bwtar LIKE ckmlhd-bwtar,
        meins LIKE mlcd-meins,
        waers LIKE mlcd-waers.
*Value Part
*         AB	Beginning inventory
def_val ab.
*         ZU	Receipts
def_val zu.
*         VN	Consumption
def_val vn.
*         EB	Ending inventory
def_val eb.
*         VP	Other receipts/consumption
def_val vp.
*         PC	Price changes
def_val pc.
*         NV	Not distributed
def_val nv.
*         KB	Cumulative inventory
def_val kb.
*         NC	Not allocated
def_val nc.
**// Mod. By Hyung Jin Youn 2004.07.01
DATA : bklas LIKE mbew-bklas.
**// End of Mod.
DATA : END OF it_display.
DATA : it_macku LIKE STANDARD TABLE OF macku
                WITH HEADER LINE .

DATA : it_fieldcat          TYPE slis_t_fieldcat_alv,
       wa_fieldcat          LIKE LINE OF it_fieldcat.
DATA : gv_repid LIKE sy-repid.
DATA : gv_col_pos TYPE i.
DATA : it_sort         TYPE slis_t_sortinfo_alv WITH HEADER LINE .
DATA   g_user_command  TYPE slis_formname VALUE 'USER_COMMAND'.
DATA : BEGIN OF it_t001w OCCURS 0.
        INCLUDE STRUCTURE t001w.
DATA : END OF   it_t001w.

* For Summary
DATA : it_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE.
DATA : it_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.
DATA : t_out TYPE zcot1_it_out WITH HEADER LINE .
DATA  :BEGIN OF it_zcor07 OCCURS 0,
        matnr LIKE it_display-matnr,
        zu_lbkum LIKE it_display-zu_lbkum,
       END OF it_zcor07.

*----------------------------------------------------------------------*
* Sub-Routine
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  READ_MLCD
*&---------------------------------------------------------------------*
*       Read MLCD data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mlcd.

* Set KALNR (Cost Estimate Number)
  CLEAR : it_kalnr,   it_kalnr[].
  CLEAR : it_ot_mlcd, it_ot_mlcd[],
          it_ot_mlcd_not_alloc,
          it_ot_mlcd_not_alloc[].

* Read Material Info.

  SELECT * FROM macku
           INTO CORRESPONDING FIELDS OF TABLE it_macku
          WHERE mtart IN ('FERT',  'HALB' ).
*            AND BKLAS IN S_BKLAS.

* Not Include the materials with No-Costing option : UPG-VC
* MARC-NOCOST
  CLEAR : it_t001w, it_t001w[].
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_t001w
           FROM t001w.

  LOOP AT it_macku.
    CLEAR it_t001w.
    READ TABLE it_t001w WITH KEY bwkey = it_macku-bwkey.
    CLEAR marc.
    SELECT SINGLE ncost
                  INTO marc-ncost
                  FROM marc
                 WHERE matnr = it_macku-matnr
                   AND werks = it_t001w-werks.
    IF marc-ncost = 'X'.
      DELETE it_macku.
    ENDIF.
    CLEAR it_macku.
  ENDLOOP.

*
  IF it_macku[] IS INITIAL.
    MESSAGE e000 WITH text-102.
  ENDIF.
* Select CKMLHD (All data)
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_kalnr
           FROM ckmlhd
           FOR ALL ENTRIES IN it_macku
           WHERE matnr = it_macku-matnr
             AND bwkey = it_macku-bwkey
             AND bwtar = it_macku-bwtar.

* Read data
  CALL FUNCTION 'CKMCD_MLCD_READ'
    EXPORTING
      i_from_bdatj            = p_bdatj
      i_from_poper            = p_poper
*     I_TO_BDATJ              =
*     I_TO_POPER              =
*     I_UNTPER                =
*     I_RUN_ID                =
*     I_NO_BUFFER             =
      i_refresh_buffer        = 'X'
      i_online                = 'X'
*     I_NO_MLCD_CREATE        =
    TABLES
      it_kalnr                = it_kalnr
      ot_mlcd                 = it_ot_mlcd
      ot_mlcd_not_alloc       = it_ot_mlcd_not_alloc
    EXCEPTIONS
      data_error              = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    MESSAGE ID   sy-msgid TYPE sy-msgty NUMBER sy-msgno
                          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " READ_MLCD

*&---------------------------------------------------------------------*
*&      Form  MERGE_MLCD_DATA
*&---------------------------------------------------------------------*
*       Merge to IT_MLCD
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM merge_mlcd_data.

  CLEAR : it_mlcd,    it_mlcd[].
  CLEAR : it_display, it_display[].

* Ignore Process category for procurement alt. or consuption alt.
* Ignore Procurement alternative/process
* MLCD-PTYP MLCD-BVALT

*IT_OT_MLCD
*IT_OT_MLCD_NOT_ALLOC

* For Allocated data / Unallocated Data
  PERFORM allocated_data_mlcd.
* For 'AB' 'EB' Space.
  PERFORM ab_eb_space_data_mlcd.
* Read Material Data / Unit / Curr.
  PERFORM read_mat_unit_curr.

ENDFORM.                    " MERGE_MLCD_DATA

*&---------------------------------------------------------------------*
*&      Form  TRANS_VAL_TO_DIS
*&---------------------------------------------------------------------*
*       Transfering Value to Dis. Tab.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM trans_val_to_dis.

*  DATA :  &1_LBKUM LIKE MLCD-LBKUM,
*          &1_SALK3 LIKE MLCD-SALK3,
*          &1_RD    LIKE MLCD-SALK3.

  FIELD-SYMBOLS : <fsval> TYPE ANY.
  DATA : lv_fname(60).

* Key Part
  MOVE-CORRESPONDING it_mlcd TO it_display.

* Qty.
  CLEAR lv_fname.
  CONCATENATE 'IT_DISPLAY' '-' it_mlcd-categ '_LBKUM'
         INTO lv_fname.
  ASSIGN (lv_fname) TO <fsval>.

  <fsval> = it_mlcd-lbkum.

* Amt. / Valuated stock
  CLEAR lv_fname.
  CONCATENATE 'IT_DISPLAY' '-' it_mlcd-categ '_SALK3'
         INTO lv_fname.
  ASSIGN (lv_fname) TO <fsval>.

  <fsval> = it_mlcd-salk3.

* Amt. / Price Difference (ERD+PRD In Category)
  CLEAR lv_fname.
  CONCATENATE 'IT_DISPLAY' '-' it_mlcd-categ '_RD'
         INTO lv_fname.
  ASSIGN (lv_fname) TO <fsval>.

  <fsval> = it_mlcd-estprd
          + it_mlcd-estkdm
          + it_mlcd-mstprd
          + it_mlcd-mstkdm
          + it_mlcd-tpprd.

* Append
  COLLECT it_display.
  CLEAR  it_display.

ENDFORM.                    " TRANS_VAL_TO_DIS

*&---------------------------------------------------------------------*
*&      Form  ALLOCATED_DATA_MLCD
*&---------------------------------------------------------------------*
*       For Allocated data / Unallocated Data
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM allocated_data_mlcd.
** For Allocated Data
* Ending/Beginning Inv/Amt will be calculated later part
  LOOP AT it_ot_mlcd WHERE categ NE 'AB'
                       AND categ NE 'EB'
                       AND categ NE space.
* Clear the values which are not useful.
    CLEAR : it_ot_mlcd-ptyp,  it_ot_mlcd-bvalt.
    CLEAR : it_ot_mlcd-meins, it_ot_mlcd-waers.
* Transfer values
    CLEAR it_mlcd.
    MOVE-CORRESPONDING it_ot_mlcd TO it_mlcd.
* Collect
    COLLECT it_mlcd.
    CLEAR it_ot_mlcd.
  ENDLOOP.

** For Not Allocated Data
* Ending/Beginning Inv/Amt will be calculated later part
  LOOP AT it_ot_mlcd_not_alloc
                     WHERE categ NE 'AB'
                       AND categ NE 'EB'
                       AND categ NE space.
* Clear the values which are not useful.
    CLEAR : it_ot_mlcd_not_alloc-ptyp,  it_ot_mlcd_not_alloc-bvalt.
    CLEAR : it_ot_mlcd_not_alloc-meins, it_ot_mlcd_not_alloc-waers.
* Transfer values
    CLEAR it_mlcd.
    MOVE-CORRESPONDING it_ot_mlcd_not_alloc TO it_mlcd.
* Collect
    COLLECT it_mlcd.
    CLEAR it_ot_mlcd_not_alloc.
  ENDLOOP.

** Trasfer data to Display Tab.
  LOOP AT it_mlcd.
    CASE it_mlcd-categ.
* AB	Beginning inventory
* EB	Ending inventory
* SPACE Value
      WHEN 'AB' OR 'EB' OR space.
*    --> For other Logic
* ZU	Receipts
* VN	Consumption
* VP	Other receipts/consumption
* PC	Price changes
* NV	Not distributed
* KB	Cumulative inventory
* NC	Not allocated
      WHEN OTHERS .
        PERFORM trans_val_to_dis.
    ENDCASE.
* Clear
    CLEAR : it_display, it_mlcd.
  ENDLOOP.

  CLEAR : it_display, it_mlcd.

ENDFORM.                    " ALLOCATED_DATA_MLCD

*&---------------------------------------------------------------------*
*&      Form  AB_EB_SPACE_DATA_MLCD
*&---------------------------------------------------------------------*
*       For 'AB' 'EB'  and Space.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ab_eb_space_data_mlcd.

* For Beginning Inv./Amt.
  PERFORM beginning_inv_amt.

* For Space Value (CATEGORY = SPACE)
* -> 'AB'
  PERFORM cal_space_cat.

* For Ending Inv./Amt.
  PERFORM ending_inv_amt.

ENDFORM.                    " AB_EB_SPACE_DATA_MLCD

*&---------------------------------------------------------------------*
*&      Form  READ_MAT_UNIT_CURR
*&---------------------------------------------------------------------*
*       Read Material Data / Unit / Curr.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_mat_unit_curr.
* Read Material Information with Cost. Est. Number.
* read All data
*  IT_KALNR

  CLEAR : macku, ckmlhd.

  DATA : BEGIN OF it_ckmlhd OCCURS 0,
          kalnr LIKE ckmlhd-kalnr,
          matnr LIKE ckmlhd-matnr,
          bwkey LIKE ckmlhd-bwkey,
          bwtar LIKE ckmlhd-bwtar,
          mtart LIKE macku-mtart ,
         END OF   it_ckmlhd.

* Real Valuation Level.
* Index : Table key
  CLEAR : it_ckmlhd, it_ckmlhd[].
  SELECT   a~kalnr a~matnr a~bwkey  a~bwtar b~mtart
           INTO CORRESPONDING FIELDS OF TABLE it_ckmlhd
           FROM ckmlhd AS a  INNER JOIN macku AS b
             ON a~matnr = b~matnr
            AND a~bwkey = b~bwkey
            AND a~bwtar = b~bwtar
           FOR ALL ENTRIES IN it_display
          WHERE a~kalnr = it_display-kalnr.

  SORT it_ckmlhd BY kalnr.
  LOOP AT it_display.
    CLEAR it_ckmlhd.
    READ TABLE it_ckmlhd WITH KEY
                         kalnr = it_display-kalnr
                         BINARY SEARCH.
    MOVE : it_ckmlhd-matnr TO it_display-matnr,
           it_ckmlhd-bwkey TO it_display-bwkey,
           it_ckmlhd-bwtar TO it_display-bwtar,
           it_ckmlhd-mtart TO it_display-mtart.

    MODIFY it_display.
    CLEAR  it_display.
  ENDLOOP.

* Read Unit/Curr.( From ML data )
  SORT it_ckmlpp            BY kalnr .
  SORT it_ckmlcr            BY kalnr .
  SORT it_mlcd              BY kalnr.
  SORT it_display           BY kalnr.

  LOOP AT  it_display.
* No Period Information is required
    CLEAR it_ckmlpp .
    READ TABLE it_ckmlpp WITH KEY kalnr = it_display-kalnr
                         BINARY SEARCH.
    it_display-meins = it_ckmlpp-meins.

    CLEAR it_ckmlcr .
    READ TABLE it_ckmlcr WITH KEY kalnr = it_display-kalnr
                         BINARY SEARCH.
    it_display-waers = it_ckmlcr-waers.

    MODIFY it_display.
    CLEAR  it_display.
  ENDLOOP.

ENDFORM.                    " READ_MAT_UNIT_CURR

*&---------------------------------------------------------------------*
*&      Form  BEGINNING_INV_AMT
*&---------------------------------------------------------------------*
*       For Beginning Inv./Amt.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM beginning_inv_amt.

  DATA : it_l_ckmlpp LIKE ckmlpp OCCURS 0 WITH HEADER LINE.
  DATA : it_l_ckmlcr LIKE ckmlcr OCCURS 0 WITH HEADER LINE.

* Read CKMLPP
  SELECT * INTO TABLE it_l_ckmlpp
           FROM ckmlpp
           FOR ALL ENTRIES IN it_kalnr
           WHERE kalnr = it_kalnr-kalnr
             AND bdatj = p_bdatj
             AND poper = p_poper.
  CLEAR it_l_ckmlpp.

* Read CKMLCR
  SELECT * INTO TABLE it_l_ckmlcr
           FROM ckmlcr
           FOR ALL ENTRIES IN it_kalnr
           WHERE kalnr = it_kalnr-kalnr
             AND bdatj = p_bdatj
             AND poper = p_poper.
*             AND CURTP = P_CURTP.
  CLEAR it_l_ckmlcr.

* Put data into Disp. Tab.
* Beginning Qty.
  CLEAR it_display.
  LOOP AT  it_l_ckmlpp.
    MOVE :  it_l_ckmlpp-kalnr TO it_display-kalnr,
            it_l_ckmlpp-bdatj TO it_display-bdatj,
            it_l_ckmlpp-poper TO it_display-poper.
* Beginning Qty.
    it_display-ab_lbkum = it_l_ckmlpp-abkumo.
    COLLECT it_display.
    CLEAR   it_display.
  ENDLOOP.

* Put data into Disp. Tab.
* Beginning Amt.
  CLEAR it_display.
  LOOP AT  it_l_ckmlcr.
    MOVE :  it_l_ckmlcr-kalnr TO it_display-kalnr,
            it_l_ckmlcr-bdatj TO it_display-bdatj,
            it_l_ckmlcr-poper TO it_display-poper.
* Beginning Amt.
    it_display-ab_salk3 = it_l_ckmlcr-absalk3.
* Beginning RD.
    it_display-ab_rd = it_l_ckmlcr-abprd_o
                     + it_l_ckmlcr-abkdm_o
                     + it_l_ckmlcr-abprd_mo
                     + it_l_ckmlcr-abkdm_mo.
    COLLECT it_display.
    CLEAR   it_display.
  ENDLOOP.

* Copy To Gloabl
  CLEAR : it_ckmlpp, it_ckmlpp[].
  CLEAR : it_ckmlcr, it_ckmlcr[].

  it_ckmlpp[] =  it_l_ckmlpp[].
  it_ckmlcr[] =  it_l_ckmlcr[].

ENDFORM.                    " BEGINNING_INV_AMT

*&---------------------------------------------------------------------*
*&      Form  CAL_SPACE_CAT
*&---------------------------------------------------------------------*
*       Calculate Values in the Category ' ' (SPACE)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_space_cat.

  DATA : it_l_tmp_mlcd LIKE mlcd OCCURS 0 WITH HEADER LINE .

  CLEAR : it_l_tmp_mlcd, it_l_tmp_mlcd[].

** For Allocate (SAPCE)
  LOOP AT it_ot_mlcd WHERE categ EQ space OR categ EQ 'AB'.
* Clear the values which are not useful.
    CLEAR : it_ot_mlcd-ptyp,  it_ot_mlcd-bvalt.
    CLEAR : it_ot_mlcd-meins, it_ot_mlcd-waers.
* --> AB
* Transfer values
    CLEAR it_l_tmp_mlcd.
    MOVE-CORRESPONDING it_ot_mlcd TO it_l_tmp_mlcd.
    it_l_tmp_mlcd-categ = 'AB'.
* Collect
    COLLECT it_l_tmp_mlcd.
    CLEAR it_ot_mlcd.
  ENDLOOP.

** For Not Allocate (SAPCE)
  LOOP AT it_ot_mlcd_not_alloc
                     WHERE categ EQ space.
* Clear the values which are not useful.
    CLEAR : it_ot_mlcd_not_alloc-ptyp,  it_ot_mlcd_not_alloc-bvalt.
    CLEAR : it_ot_mlcd_not_alloc-meins, it_ot_mlcd_not_alloc-waers.
* --> AB
* Transfer values
    CLEAR it_l_tmp_mlcd.
    MOVE-CORRESPONDING it_ot_mlcd_not_alloc TO it_l_tmp_mlcd.
    it_l_tmp_mlcd-categ = 'AB'.
* Collect
    COLLECT it_l_tmp_mlcd.
    CLEAR it_ot_mlcd_not_alloc.
  ENDLOOP.

** Put data into Disp. Tab.
  LOOP AT it_l_tmp_mlcd.
*
    CLEAR it_display.
    MOVE-CORRESPONDING it_l_tmp_mlcd TO it_display.
* Qty.
    it_display-ab_lbkum  = it_l_tmp_mlcd-lbkum.

* Amt. / Valuated stock
    it_display-ab_salk3  = it_l_tmp_mlcd-salk3.

* Amt. / Price Difference (ERD+PRD In Category)
    it_display-ab_rd     = it_l_tmp_mlcd-estprd
                         + it_l_tmp_mlcd-estkdm
                         + it_l_tmp_mlcd-mstprd
                         + it_l_tmp_mlcd-mstkdm
                         + it_l_tmp_mlcd-tpprd.
* Append
    COLLECT it_display.
    CLEAR   it_display.
    CLEAR   it_l_tmp_mlcd.
  ENDLOOP.

ENDFORM.                    " CAL_SPACE_CAT

*&---------------------------------------------------------------------*
*&      Form  ENDING_INV_AMT
*&---------------------------------------------------------------------*
*       For Ending Inv./Amt.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ending_inv_amt.

  LOOP AT it_display.
* Qty.
    it_display-eb_lbkum =
                          it_display-ab_lbkum
                        + it_display-zu_lbkum
                        - it_display-vn_lbkum
                        + it_display-vp_lbkum
                        + it_display-pc_lbkum
                        + it_display-nv_lbkum
                        + it_display-kb_lbkum
                        + it_display-nc_lbkum.
* Amt.
    it_display-eb_salk3 =
                          it_display-ab_salk3
                        + it_display-zu_salk3
                        - it_display-vn_salk3
                        + it_display-vp_salk3
                        + it_display-pc_salk3
                        + it_display-nv_salk3
                        + it_display-kb_salk3
                        + it_display-nc_salk3.
* RD
    it_display-eb_rd    =
                          it_display-ab_rd
                        + it_display-zu_rd
                        - it_display-vn_rd
                        + it_display-vp_rd
                        + it_display-pc_rd
                        + it_display-nv_rd
                        + it_display-kb_rd
                        + it_display-nc_rd.

    MODIFY it_display.
    CLEAR  it_display.
  ENDLOOP.
ENDFORM.                    " ENDING_INV_AMT

*&---------------------------------------------------------------------*
*&      Form  DIS_ALV_TEMP
*&---------------------------------------------------------------------*
*       Temporary Display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_alv_temp.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
   EXPORTING
*    I_INTERFACE_CHECK              = ' '
*    I_BYPASSING_BUFFER             =
*    I_BUFFER_ACTIVE                = ' '
     i_callback_program             = gv_repid
*    I_CALLBACK_PF_STATUS_SET       = ' '
*    I_CALLBACK_USER_COMMAND        = ' '
*    I_STRUCTURE_NAME               =
*    IS_LAYOUT                      =
     it_fieldcat                    = it_fieldcat[]
*    IT_EXCLUDING                   =
*    IT_SPECIAL_GROUPS              =
*    IT_SORT                        =
*    IT_FILTER                      =
*    IS_SEL_HIDE                    =
*    I_DEFAULT                      = 'X'
     i_save                         = 'A'
*    IS_VARIANT                     =
*    IT_EVENTS                      =
*    IT_EVENT_EXIT                  =
*    IS_PRINT                       =
*    IS_REPREP_ID                   =
*    I_SCREEN_START_COLUMN          = 0
*    I_SCREEN_START_LINE            = 0
*    I_SCREEN_END_COLUMN            = 0
*    I_SCREEN_END_LINE              = 0
*  IMPORTING
*    E_EXIT_CAUSED_BY_CALLER        =
*    ES_EXIT_CAUSED_BY_USER         =
    TABLES
      t_outtab                       = it_display
    EXCEPTIONS
      program_error                  = 1
      OTHERS                         = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DIS_ALV_TEMP

*&---------------------------------------------------------------------*
*&      Form  DIS_ALV_TEMP_GRID
*&---------------------------------------------------------------------*
*       Display
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dis_alv_temp_grid.

  gv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK                 = ' '
*     I_BYPASSING_BUFFER                =
*     I_BUFFER_ACTIVE                   = ' '
      i_callback_program                = gv_repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
      i_callback_user_command           = g_user_command
*     I_CALLBACK_TOP_OF_PAGE            = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*     I_CALLBACK_HTML_END_OF_LIST       = ' '
*     I_STRUCTURE_NAME                  =
*     I_BACKGROUND_ID                   = ' '
*     I_GRID_TITLE                      =
*     I_GRID_SETTINGS                   =
*     IS_LAYOUT                         =
      it_fieldcat                       = it_fieldcat[]
*     IT_EXCLUDING                      =
*     IT_SPECIAL_GROUPS                 =
      it_sort                           = it_sort[]
*     IT_FILTER                         =
*     IS_SEL_HIDE                       =
*     I_DEFAULT                         = 'X'
      i_save                            = 'A'
*     IS_VARIANT                        =
*     IT_EVENTS                         =
*     IT_EVENT_EXIT                     =
*     IS_PRINT                          =
*     IS_REPREP_ID                      =
*     I_SCREEN_START_COLUMN             = 0
*     I_SCREEN_START_LINE               = 0
*     I_SCREEN_END_COLUMN               = 0
*     I_SCREEN_END_LINE                 = 0
*     IT_ALV_GRAPHICS                   =
*     IT_ADD_FIELDCAT                   =
*     IT_HYPERLINK                      =
*     I_HTML_HEIGHT_TOP                 =
*     I_HTML_HEIGHT_END                 =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER           =
*     ES_EXIT_CAUSED_BY_USER            =
    TABLES
      t_outtab                          = it_display
    EXCEPTIONS
      program_error                     = 1
      OTHERS                            = 2
            .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DIS_ALV_TEMP_GRID

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT_INIT
*&---------------------------------------------------------------------*
*       Building Field Cat.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fieldcat_init.
  CLEAR : gv_col_pos, it_fieldcat, it_fieldcat[].
** Key
  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'BDATJ'  'X'            space    space
    space           '4'      'Yr.'          space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'POPER'  'X'            space    space
    space           '3'      'Period'       space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'KALNR'  'X'            space    space
    space           '12'     'Cost est No.' space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'MTART'  'X'            space    space
    space           '4'      'Mtyp'         space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'MATNR'  'X'            space    space
    space           '18'     'Material'     space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'BWKEY'  'X'            space    space
    space           '4'      'Val. Area'    space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'BWTAR'  'X'            space    space
    space           '10'     'Val. Type'    space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'BKLAS'  'X'            space    space
    space           '4'      'Val.Class'    space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'MEINS'  'X'            space    space
    space           '3'      'Unit'         space    space    space
    space.

  PERFORM build_fieldcat  USING
    'IT_DISPLAY'    'WAERS'  'X'            space    space
    space           '5'      'Curr'         space    space    space
    space.


* Values
* AB 1
  PERFORM fcat_by_cate
    USING 'AB_LBKUM'
          'AB_SALK3'
          'AB_RD'
          'Beginning Qty.'
          'Beginning Prelim Val.'
          'Beginning Diff. Val.'.
* ZU 2
  PERFORM fcat_by_cate
    USING 'ZU_LBKUM'
          'ZU_SALK3'
          'ZU_RD'
          'Receipts Qty.'
          'Receipts Prelim Val.'
          'Receipts Diff. Val.'.

* VN 3
  PERFORM fcat_by_cate
    USING 'VN_LBKUM'
          'VN_SALK3'
          'VN_RD'
          'Consumption Qty.'
          'Consumption Prelim Val.'
          'Consumption Diff. Val.'.

* EB 4
  PERFORM fcat_by_cate
    USING 'EB_LBKUM'
          'EB_SALK3'
          'EB_RD'
          'Ending Qty.'
          'Ending Prelim Val.'
          'Ending Diff. Val.'.

* VP 5
  PERFORM fcat_by_cate
    USING 'VP_LBKUM'
          'VP_SALK3'
          'VP_RD'
          'Other Rep/Con Qty.'
          'Other Rep/Con Val.'
          'Other Rep/Con Diff. Val.'.

* PC 6
  PERFORM fcat_by_cate
    USING 'PC_LBKUM'
          'PC_SALK3'
          'PC_RD'
          'Price changes Qty.'
          'Price changes Val.'
          'Price changes Diff. Val.'.

* NV 7
  PERFORM fcat_by_cate
    USING 'NV_LBKUM'
          'NV_SALK3'
          'NV_RD'
          'Not distributed Qty.'
          'Not distributed Val.'
          'Not distributed Diff. Val.'.

* KB 8
  PERFORM fcat_by_cate
    USING 'KB_LBKUM'
          'KB_SALK3'
          'KB_RD'
          'Cul. Inv. Qty.'
          'Cul. Inv. Val.'
          'Cul. Inv. Diff. Val.'.

* NC 9
  PERFORM fcat_by_cate
    USING 'NC_LBKUM'
          'NC_SALK3'
          'NC_RD'
          'Not allocated Qty.'
          'Not allocated Val.'
          'Not allocated Diff. Val.'.


ENDFORM.                    " FIELDCAT_INIT

*&---------------------------------------------------------------------*
*&      Form  Build_FieldCAT
*&---------------------------------------------------------------------*
*       Field_CAT
*----------------------------------------------------------------------*
*      -->P_0065   text
*      -->P_0066   text
*      -->P_0067   text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_SPACE  text
*      -->P_0071   text
*      -->P_0072   text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM build_fieldcat USING    value(p_0100)
                             value(p_0101)
                             value(p_0102)
                             value(p_0103)
                             value(p_0104)
                             value(p_0105)
                             value(p_0106)
                             value(p_0107)
                             value(p_0108)
                             value(p_0109)
                             value(p_0110)
                             value(p_0111).


  ADD 1 TO gv_col_pos.
  wa_fieldcat-tabname     = p_0100.
  wa_fieldcat-fieldname   = p_0101.
  wa_fieldcat-key         = p_0102.
  wa_fieldcat-do_sum      = p_0103.
  wa_fieldcat-cfieldname  = p_0104.
  wa_fieldcat-ctabname    = p_0105.
  wa_fieldcat-outputlen   = p_0106.
  wa_fieldcat-seltext_l   = p_0107.
  wa_fieldcat-datatype    = p_0108.
  wa_fieldcat-qfieldname  = p_0109.
  wa_fieldcat-qtabname    = p_0110.
  wa_fieldcat-col_pos     = gv_col_pos.
*  WA_FIELDCAT-OFFSET      = P_0111.
  wa_fieldcat-decimals_out      = p_0111.
  APPEND wa_fieldcat TO it_fieldcat.
  CLEAR wa_fieldcat.

ENDFORM.                    " Build_FieldCAT

*&---------------------------------------------------------------------*
*&      Form  FCAT_BY_CATE
*&---------------------------------------------------------------------*
*       FCat
*----------------------------------------------------------------------*
*      -->P_1163   text
*      -->P_1164   text
*      -->P_1165   text
*      -->P_1166   text
*      -->P_1167   text
*      -->P_1168   text
*----------------------------------------------------------------------*
FORM fcat_by_cate USING    value(p_1163)
                           value(p_1164)
                           value(p_1165)
                           value(p_1166)
                           value(p_1167)
                           value(p_1168).

  PERFORM build_fieldcat USING
    'IT_DISPLAY'    p_1163      space         'X'
    space            space
    '20'            p_1166
    'QUAN'
    'MEINS'         'IT_DISPLAY'
    '1'.

  PERFORM build_fieldcat USING
    'IT_DISPLAY'    p_1164      space         'X'
    'WAERS'         'IT_DISPLAY'
    '21'            p_1167
    'CURR'
    space           space
    space.

  PERFORM build_fieldcat USING
    'IT_DISPLAY'    p_1165      space         'X'
    'WAERS'         'IT_DISPLAY'
    '21'            p_1168
    'CURR'
    space           space
    space.

ENDFORM.                    " FCAT_BY_CATE

*&---------------------------------------------------------------------*
*&      Form  PRE_REPORT_ADJ
*&---------------------------------------------------------------------*
*       The Preparation for ALV List
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM pre_report_adj.

* Sort IT_DISPLAY.
  SORT it_display BY bdatj poper mtart .


  CLEAR it_display.
  it_sort-fieldname = 'MTART'.
  it_sort-up        = 'X'.
  it_sort-expa      = 'X'.
  it_sort-subtot    = 'X'.
  APPEND it_sort.

ENDFORM.                    " PRE_REPORT_ADJ

*---------------------------------------------------------------------*
*       FORM USER_COMMAND                                             *
*---------------------------------------------------------------------*
*       User Command                                                  *
*---------------------------------------------------------------------*
*       --> R_UCOMM                                                   *
*       --> RS_SELFIELD                                               *
*---------------------------------------------------------------------*
FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.

  CASE r_ucomm.
    WHEN '&IC1'.                             "dobbleclick
* CKM3
      IF  rs_selfield-sel_tab_field = 'IT_DISPLAY-KALNR'
      AND rs_selfield-value NE space.
       READ TABLE it_display INTO it_display INDEX rs_selfield-tabindex.
        CLEAR t001w.
        SELECT SINGLE * FROM t001w
                       WHERE bwkey = it_display-bwkey.
        SET PARAMETER ID 'MAT'  FIELD  it_display-matnr.
        SET PARAMETER ID 'WRK'  FIELD  t001w-werks.
        SET PARAMETER ID 'POPR' FIELD  it_display-poper.
        SET PARAMETER ID 'BDTJ' FIELD  it_display-bdatj.

        CALL TRANSACTION 'CKM3' AND SKIP FIRST SCREEN.
        CLEAR r_ucomm.
      ENDIF.
* PCC Order
      IF  rs_selfield-sel_tab_field = 'IT_DISPLAY-MATNR'
      AND rs_selfield-value NE space.
       READ TABLE it_display INTO it_display INDEX rs_selfield-tabindex.
        CLEAR t001w.
        SELECT SINGLE * FROM t001w
                       WHERE bwkey = it_display-bwkey.
        PERFORM show_pcc_order.
      ENDIF.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SHOW_PCC_ORDER
*&---------------------------------------------------------------------*
*       SHow Pcc order
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_pcc_order.
* IT_DISPLAY
  DATA : it_l_vkks0 LIKE STANDARD TABLE OF vkks0
                    WITH HEADER LINE,
         it_l_pkosa LIKE STANDARD TABLE OF pkosa
                    WITH HEADER LINE.

  CALL FUNCTION 'KK_F_PKOSA_FIND'
   EXPORTING
      i_matnr                     = it_display-matnr
      i_werks                     = t001w-werks
*     I_PWERK                     = ' '
*     I_PROCNR                    = ' '
*     I_SA_AUFNR                  = ' '
*     I_FA_AUFNR                  = ' '
*     I_VERID                     = ' '
*     I_STLAN                     = ' '
*     I_STLAL                     = ' '
*     I_PLNTY                     = ' '
*     I_PLNNR                     = ' '
*     I_PLNAL                     = ' '
*     I_DATE                      = '00000000'
*     I_POPUP                     = ' '
*     I_REM                       = ' '
*     I_INCL_LOEKZ                = ' '
*     I_NO_OLD_PKOSA              = ' '
*   IMPORTING
*     E_PROCNR                    =
*     E_VERID                     =
*     E_STLAN                     =
*     E_STLAL                     =
*     E_PLNTY                     =
*     E_PLNNR                     =
*     E_PLNAL                     =
*     E_AUFNR                     =
    TABLES
      e_vkks0                     = it_l_vkks0
      e_pkosa                     = it_l_pkosa
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

  IF sy-subrc <> 0.
    MESSAGE i000 WITH text-101.
  ELSE.

    CALL FUNCTION 'STC1_POPUP_WITH_TABLE_CONTROL'
      EXPORTING
        header                  = text-101
        tabname                 = 'VKKS0'
        display_only            = 'X'
*       ENDLESS                 =
*       DISPLAY_TOGGLE          =
*       SORT_FORBIDDEN          =
*       MODIFY_CHECK            =
*       INSERT_CHECK            =
*       DELETE_CHECK            =
*       MODIFY_DISP_FIELD       =
*       NO_INSERT               =
*       NO_DELETE               =
*       NO_MOVE                 =
*       NO_UNDO                 =
        no_button               = space
*       X_START                 = 5
*       Y_START                 = 5
*       X_END                   = 80
*       Y_END                   = 25
      TABLES
*       NAMETAB                 =
        table                   = it_l_vkks0
*       FIELDDIF                =
      EXCEPTIONS
        no_more_tables          = 1
        too_many_fields         = 2
        nametab_not_valid       = 3
        handle_not_valid        = 4
        OTHERS                  = 5
              .
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.                    " SHOW_PCC_ORDER

*&---------------------------------------------------------------------*
*&      Form  READ_VAL_CLASS
*&---------------------------------------------------------------------*
*       Read Valuation Class
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_val_class.

  CLEAR it_macku.
  CLEAR it_display.

  LOOP AT it_display.
    CLEAR it_macku.
    READ TABLE  it_macku WITH KEY matnr = it_display-matnr
                                  bwkey = it_display-bwkey
                                  bwtar = it_display-bwtar.
    it_display-bklas = it_macku-bklas.
    MODIFY it_display.
    CLEAR it_display.
  ENDLOOP.

ENDFORM.                    " READ_VAL_CLASS

*&---------------------------------------------------------------------*
*&      Form  SET_BEGINNING_DATA_FOR_NO_TR
*&---------------------------------------------------------------------*
* Data in MLCD could not be generated if any transaction
* is executied at specific period
* Set Beginning Inv. from Ending Inv. data of previous period.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_beginning_data_for_no_tr.

*  MESSAGE e724 RAISING no_data_found.

  CLEAR it_display.
  CLEAR it_kalnr.
  CLEAR mbew.


* IF PAST period and IT_DISPLAY[] is initial. Do not proceed futher.

  CHECK  p_bdatj => sy-datum(4)
    AND  p_poper => sy-datum+4(2)
    AND  it_display[] IS INITIAL.


  DATA : it_l_dis LIKE STANDARD TABLE OF it_display
                  WITH HEADER LINE .

  LOOP AT it_kalnr.
    CLEAR it_l_dis.
    CLEAR it_display.
    READ TABLE  it_display WITH KEY bdatj = p_bdatj
                                    poper = p_poper
                                    kalnr = it_kalnr-kalnr.
    IF sy-subrc <> 0.
* MBEW
      CLEAR mbew.
      SELECT SINGLE * FROM mbew
                     WHERE matnr = it_kalnr-matnr
                       AND bwkey = it_kalnr-bwkey
                       AND bwtar = it_kalnr-bwtar.
* Set Data
*
      MOVE-CORRESPONDING mbew TO it_l_dis.
*  Values
      it_l_dis-ab_lbkum = mbew-lbkum.
      it_l_dis-ab_salk3 = mbew-salk3.
*
      it_l_dis-bdatj = p_bdatj.
      it_l_dis-poper = p_poper.
*
      CLEAR mara .
      SELECT SINGLE
              mtart meins
              INTO (it_l_dis-mtart, it_l_dis-meins)
                    FROM mara
                   WHERE matnr = it_kalnr-matnr.
*
      it_l_dis-kalnr = it_kalnr-kalnr.
*
      CLEAR ckmlct.
      SELECT SINGLE
              waers INTO it_l_dis-waers
                    FROM ckmlct
                   WHERE bwkey = it_l_dis-bwkey
                     AND curtp = p_curtp.
*     Append
      APPEND it_l_dis.
      CLEAR  it_l_dis.
    ENDIF.
    CLEAR it_kalnr.
  ENDLOOP.

* Add itab to Display-ITAB
  IF NOT it_l_dis[] IS INITIAL.
    APPEND LINES OF  it_l_dis  TO it_display.
  ENDIF.

ENDFORM.                    " SET_BEGINNING_DATA_FOR_NO_TR

*&---------------------------------------------------------------------*
*&      Form  CHK_FT_DATE
*&---------------------------------------------------------------------*
*       Check Future Period
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM chk_ft_date.
*Issue number 20050107-002 requested by andy
*changed by  wskim, on 01/07/2005
*-----Start
  IF  p_bdatj > sy-datum(4).
    MESSAGE e000 WITH text-301 p_bdatj p_poper.
*  ELSE.
*    IF  P_POPER > SY-DATUM+4(2).
*      MESSAGE E000 WITH TEXT-301 P_BDATJ P_POPER.
*    ENDIF.
  ENDIF.
*----End
ENDFORM.                    " CHK_FT_DATE

*&---------------------------------------------------------------------*
*&      Form  CAL_NOT_DIS
*&---------------------------------------------------------------------*
*       Not_Distributed
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cal_not_dis.

  DATA : it_l_rsparams LIKE STANDARD TABLE OF rsparams
                       WITH HEADER LINE .
  CLEAR : it_l_rsparams, it_l_rsparams[].
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report           = 'ZACO11U_MLVA'
*   IMPORTING
*     SP                    =
    TABLES
      selection_table       = it_l_rsparams
    EXCEPTIONS
      not_found             = 1
      no_report             = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*
*  RANGES R_MATNR FOR MLKEY-MATNR.

* Clear
  CLEAR : t_out, t_out[].

  SUBMIT zaco11u_mlva
*    VIA SELECTION-SCREEN
    AND RETURN
    WITH SELECTION-TABLE it_l_rsparams
    WITH p_poper = p_poper
    WITH p_bdatj = p_bdatj
*    WITH R_MATNR IN ( 'FERT', 'HALB' )
    WITH p_notdis = 'X'.
*<data_tab>
  IMPORT t_out   = t_out
         FROM MEMORY ID 'HLV'.

  FREE MEMORY ID 'HLV'.

  CLEAR : t_out.

* Cal. ND
  LOOP AT t_out.
    LOOP AT it_display
      WHERE kalnr  = t_out-kalnr
        AND bdatj  = t_out-bdatj
        AND poper  = t_out-poper
        AND matnr  = t_out-matnr
        AND bwkey  = t_out-bwkey
        AND bwtar  = t_out-bwtar.

* Amt.
*      IT_DISPLAY-EB_SALK3 =

* RD
      it_display-eb_rd    = it_display-eb_rd - t_out-sumdif.
      it_display-nv_rd    = it_display-nv_rd - t_out-sumdif.

      MODIFY it_display.
      CLEAR  it_display.
    ENDLOOP.
    CLEAR t_out.
  ENDLOOP.


ENDFORM.                    " CAL_NOT_DIS
