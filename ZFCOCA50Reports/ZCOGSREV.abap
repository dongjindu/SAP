*
* ZCOGSREV
*
* History
*  - Originally developed by Andy Choi
*  - Fixed Incorrect Calculation (Andy)
*
REPORT zcogsrev .

TABLES: t000, t030, t001, bsis, bsim, mbew, mbewh.
TABLES: ce3h201, ce4h201, makt, mara.

RANGES: r_hkont FOR bsis-hkont.

DATA: BEGIN OF it_co_cogs   OCCURS 0,
        matnr     TYPE matnr,
        werks     TYPE werks_d,
*        maktx	type maktx  ,
        bklas	TYPE bklas  ,
*        vkorg	type vkorg  ,
*        vtweg	type vtweg  ,
*        spart	type spart  ,
*        kndnr     type kunde_pa,
        ktgrd     TYPE ktgrd,
*        mtart	type mtart  ,
*        prdha	type prodh_d,
*        mvgr3	type mvgr3  ,
*        mvgr4	type mvgr3  ,
*        mvgr5	type mvgr5  ,
        menge     TYPE rke2_absmg,
*act.
        ac001	TYPE ckml_value_cum,
        ac002	TYPE ckml_value_cum ,
        ac003	TYPE ckml_value_cum ,
        ac004	TYPE ckml_value_cum ,
        ac005	TYPE ckml_value_cum ,
        ac006	TYPE ckml_value_cum ,
        ac007	TYPE ckml_value_cum ,
        ac008	TYPE ckml_value_cum ,
        ac009	TYPE ckml_value_cum ,
        ac010	TYPE ckml_value_cum ,
        ac011	TYPE ckml_value_cum ,
        ac012	TYPE ckml_value_cum ,
        ac013	TYPE ckml_value_cum ,
        ac014	TYPE ckml_value_cum ,
        ac015	TYPE ckml_value_cum ,
        ac016	TYPE ckml_value_cum ,
        ac017	TYPE ckml_value_cum ,
        ac018	TYPE ckml_value_cum ,
        ac019	TYPE ckml_value_cum ,
        ac020	TYPE ckml_value_cum ,
        ac021	TYPE ckml_value_cum ,
        ac022	TYPE ckml_value_cum ,
        ac023	TYPE ckml_value_cum ,
        ac024	TYPE ckml_value_cum ,
        ac025	TYPE ckml_value_cum ,
        ac026	TYPE ckml_value_cum ,
        ac027	TYPE ckml_value_cum ,
        ac028	TYPE ckml_value_cum ,
*std
        sd001	TYPE ckml_value_cum,
        sd002	TYPE ckml_value_cum ,
        sd003	TYPE ckml_value_cum ,
        sd004	TYPE ckml_value_cum ,
        sd005	TYPE ckml_value_cum ,
        sd006	TYPE ckml_value_cum ,
        sd007	TYPE ckml_value_cum ,
        sd008	TYPE ckml_value_cum ,
        sd009	TYPE ckml_value_cum ,
        sd010	TYPE ckml_value_cum ,
        sd011	TYPE ckml_value_cum ,
        sd012	TYPE ckml_value_cum ,
        sd013	TYPE ckml_value_cum ,
        sd014	TYPE ckml_value_cum ,
        sd015	TYPE ckml_value_cum ,
        sd016	TYPE ckml_value_cum ,
        sd017	TYPE ckml_value_cum ,
        sd018	TYPE ckml_value_cum ,
        sd019	TYPE ckml_value_cum ,
        sd020	TYPE ckml_value_cum ,
        sd021	TYPE ckml_value_cum ,
        sd022	TYPE ckml_value_cum ,
        sd023	TYPE ckml_value_cum ,
        sd024	TYPE ckml_value_cum ,
        sd025	TYPE ckml_value_cum ,
        sd026	TYPE ckml_value_cum ,
        sd027	TYPE ckml_value_cum ,
        sd028	TYPE ckml_value_cum ,
        END OF it_co_cogs.

DATA: BEGIN OF it_cogs OCCURS 0,
        ktgrd     TYPE ktgrd,
        bklas	TYPE bklas  ,
        matnr     TYPE matnr,
        werks     TYPE werks_d, "Plant
*        maktx	type maktx  ,
*        vkorg	type vkorg  ,
*        vtweg	type vtweg  ,
*        mtart	type mtart  ,
*        spart	type spart  ,
*        kndnr	type kunde_pa,
        hkont     LIKE bseg-hkont,
        menge     TYPE rke2_absmg,
        sd000     TYPE ckml_value_cum,
        ac000     TYPE ckml_value_cum,
        va000     TYPE ckml_value_cum,
        va000_old TYPE ckml_value_cum,
      END   OF it_cogs.

DATA: BEGIN OF it_cogs_sum   OCCURS 0,
        werks     TYPE werks_d, "Plant
        bklas     TYPE bklas,
        matnr     TYPE matnr,
        hkont     LIKE bseg-hkont,
        menge     TYPE rke2_absmg,
        va000     TYPE ckml_value_cum,
        pbpopo    LIKE ckmlpp-pbpopo,  "Quantity from Sub.Adj
        ndi       TYPE ckml_value_cum,
        nin       TYPE ckml_value_cum,
        cogs      TYPE ckml_value_cum,
        ndis      TYPE ckml_value_cum,
        nins      TYPE ckml_value_cum,
        ovhd      TYPE ckml_value_cum,
      END   OF it_cogs_sum.


*--- ML
TYPE-POOLS: ckmv0.
TYPES:
* Material/valuated sales order/project stock data
  BEGIN OF s_mats,
     kalnr TYPE ckmlhd-kalnr,
     matnr TYPE ckmlhd-matnr,
     bwkey TYPE ckmlhd-bwkey,
     bwtar TYPE ckmlhd-bwtar,
     sobkz TYPE ckmlhd-sobkz,
     vbeln TYPE ckmlhd-vbeln,
     posnr TYPE ckmlhd-posnr,
     pspnr TYPE ckmlhd-pspnr,
     bklas TYPE mbew-bklas,
     mtart TYPE mara-mtart,
     matkl TYPE mara-matkl,
     spart TYPE mara-spart,
     prctr TYPE marc-prctr,
     meins TYPE mara-meins,
  END OF s_mats,
  ty_mats TYPE STANDARD TABLE OF s_mats WITH KEY kalnr,

* Output
  BEGIN OF s_out,
     kalnr TYPE ckmlhd-kalnr,
     bdatj TYPE ckmlpp-bdatj,
     poper TYPE ckmlpp-poper,
     untper TYPE ckmlpp-untper,
     curtp TYPE ckmlcr-curtp,
     matnr TYPE ckmlhd-matnr,
     bwkey TYPE ckmlhd-bwkey,
     bwtar TYPE ckmlhd-bwtar,
     vbeln TYPE ckmlhd-vbeln,
     posnr TYPE ckmlhd-posnr,
     pspnr TYPE ckmlhd-pspnr,
     bklas TYPE mbew-bklas,
     mtart TYPE mara-mtart,
     matkl TYPE mara-matkl,
     spart TYPE mara-spart,
     prctr TYPE marc-prctr,
     meins TYPE ckmlpp-meins,
     status TYPE ckmlpp-status,
     lbkum TYPE ckmlpp-lbkum,
     menge TYPE kkb_ml_menge,
     pbpopo TYPE ckmlpp-pbpopo,
     salk3 TYPE ckmlcr-salk3,
     wert TYPE kkb_ml_bewer,
     stprs TYPE ckmlcr-stprs,
     pvprs TYPE ckmlcr-pvprs,
     peinh TYPE ckmlcr-peinh,
     waers TYPE ckmlcr-waers,
     pbprd_o TYPE ckmlcr-pbprd_o,
     pbkdm_o TYPE ckmlcr-pbkdm_o,
     estprd TYPE ckml_estprd,
     estkdm TYPE ckml_estkdm,
     mstprd TYPE ckml_mstprd,
     mstkdm TYPE ckml_mstkdm,
     estdif TYPE ck_singlelevel_dif,
     mstdif TYPE ck_multilevel_dif,
     prdif TYPE ck_sum_prdif,
     krdif TYPE ck_sum_krdif,
     sumdif TYPE ck_sum_dif,
     pos_type(3),
     pos_type_text(40),
     color(3) TYPE c,
   END OF s_out,
   ty_out TYPE STANDARD TABLE OF s_out WITH KEY kalnr.

TABLES: marv, mlkey.

DATA: t_t001k LIKE t001k OCCURS 0 WITH HEADER LINE,
      t_mats TYPE ty_mats,
      t_mats_portion TYPE ty_mats,
      t_ckmlpp TYPE STANDARD TABLE OF ckmlpp
               WITH KEY kalnr bdatj poper
               WITH HEADER LINE,
      t_ckmlcr TYPE STANDARD TABLE OF ckmlcr
               WITH KEY kalnr bdatj poper curtp
               WITH HEADER LINE,
      t_mlcd TYPE STANDARD TABLE OF mlcd
               WITH KEY kalnr bdatj poper untper categ ptyp bvalt curtp
               WITH HEADER LINE,
      t_mlcd_not_alloc TYPE STANDARD TABLE OF mlcd
               WITH KEY kalnr bdatj poper untper categ ptyp bvalt curtp
               WITH HEADER LINE,
      t_out TYPE ty_out WITH HEADER LINE.

DATA: s_runperiod TYPE ckml_run_period_data,
      s_mats TYPE s_mats.

DATA: h_last_bwkey TYPE bwkey,
      h_sele_lauf TYPE boole_d,
      h_expan TYPE boole_d,
      h_lines TYPE i,
      h_index TYPE sy-tabix,
      h_counter TYPE i,
      h_portion_size TYPE i VALUE 100.


DATA: g_bdatj LIKE mlkey-bdatj,
      g_poper LIKE cki_doc_ml-sl_periode.
*--- ML

DATA: BEGIN OF i_selfld OCCURS 0,
        field(30)  TYPE c,
      END   OF i_selfld.

DATA: BEGIN OF it_value_field OCCURS 0,
        field       LIKE   dd03l-fieldname,
        value_field LIKE   dd03l-fieldname,
        fieldname   LIKE   dd03l-fieldname,
        reptext     LIKE   dd04t-reptext,
      END   OF it_value_field.

*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: p_buk LIKE t001-bukrs MEMORY ID buk  OBLIGATORY,
            p_perbl LIKE ce3h201-perbl           OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
PARAMETERS: p_disp(4) TYPE c  DEFAULT 'LIST'. " no-display.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME .
PARAMETERS:
      cogs_00  LIKE ska1-saknr DEFAULT '0000530910' NO-DISPLAY,
      cogs_df  LIKE ska1-saknr DEFAULT '0000510009' NO-DISPLAY, "FG
      cogs_ds  LIKE ska1-saknr DEFAULT '0000510109' NO-DISPLAY, "SEMI
      cogs_dr  LIKE ska1-saknr DEFAULT '0000510209' NO-DISPLAY, "ROH
      cogs_ff  LIKE ska1-saknr DEFAULT '0000520009' NO-DISPLAY,
      cogs_fs  LIKE ska1-saknr DEFAULT '0000520109' NO-DISPLAY,
      cogs_fr  LIKE ska1-saknr DEFAULT '0000520209' NO-DISPLAY,
*     cogs_oh  LIKE ska1-saknr DEFAULT '0000531400' NO-DISPLAY.
      cogs_oh  LIKE ska1-saknr DEFAULT '0000532000' NO-DISPLAY.


SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: p_run AS CHECKBOX,
            p_budat  TYPE budat,   "  DEFAULT sy-datum,
            p_blart  LIKE bapiache08-doc_type DEFAULT 'SA'.
SELECTION-SCREEN END OF BLOCK b3.

SELECT-OPTIONS: r_matnr FOR mlkey-matnr  MEMORY ID mat  , "NO-DISPLAY,
                r_bklas FOR  mbew-bklas  MODIF ID puk   . "DISPLAY.


*&---------------------------------------------------------------------*
DATA: i_mbewh LIKE mbewh OCCURS 0 WITH HEADER LINE.

*--- ALV
TYPE-POOLS: slis.
DATA : w_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       w_eventcat TYPE slis_t_event WITH HEADER LINE,
       w_selfield TYPE slis_selfield,
       w_sortcat  TYPE slis_t_sortinfo_alv WITH HEADER LINE,
       w_col_pos  TYPE i,
       w_program  LIKE sy-repid,
       w_top_of_page TYPE slis_t_listheader,
       w_line1 TYPE slis_listheader.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gt_sp_group TYPE slis_t_sp_group_alv,
      gt_events   TYPE slis_t_event,
      gt_sorts    TYPE slis_t_sortinfo_alv WITH HEADER LINE,
      gs_prnt     TYPE slis_print_alv,
      g_repid     LIKE sy-repid.
*---- ALV

*---- AC BAPI
DATA:
      obj_type LIKE bapiache02-obj_type,
      obj_key LIKE bapiache02-obj_key,
      obj_sys LIKE bapiache02-obj_sys,

      documentheader  LIKE bapiache08,
      accountgl       LIKE bapiacgl08  OCCURS 0 WITH HEADER LINE,
      currencyamount  LIKE bapiaccr08  OCCURS 0 WITH HEADER LINE,
      return          LIKE bapiret2    OCCURS 0 WITH HEADER LINE,
      extension1      LIKE bapiextc    OCCURS 0 WITH HEADER LINE,

      t_edidd         LIKE edidd       OCCURS 0 WITH HEADER LINE,
      bapi_retn_info  LIKE bapiret2    OCCURS 0 WITH HEADER LINE.

DATA:
  g_objsys        LIKE documentheader-obj_sys,
  g_rc            LIKE sy-subrc  VALUE 0,
  l_budat         LIKE sy-datum,
  g_budat         LIKE sy-datum.
*---------------------------------------------------------------

START-OF-SELECTION.
  PERFORM get_obj_sys USING g_objsys.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_buk.

  IF p_blart = space. p_blart = 'SA'.  ENDIF.
  IF p_budat IS INITIAL.
    CONCATENATE p_perbl(4) p_perbl+5(2) '01' INTO l_budat.

    CALL FUNCTION 'MM_ARRANG_GET_END_OF_MONTH'
         EXPORTING
              i_datum = l_budat
         IMPORTING
              e_datum = g_budat.
    p_budat = g_budat.
  ENDIF.

  PERFORM get_co_cogs.
  PERFORM get_ml_cogs.

*FIXME
  SELECT * INTO TABLE i_mbewh FROM mbewh
     WHERE matnr IN r_matnr
       AND lfgja = g_bdatj
       AND lfmon = g_poper
     ORDER BY bwkey DESCENDING.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE i_mbewh
     FROM mbew
     WHERE matnr IN r_matnr
       AND lfgja = g_bdatj
       AND lfmon = g_poper
     ORDER BY bwkey DESCENDING.

  PERFORM calcluate.

*---------------------------------------------------------------
END-OF-SELECTION.

  IF p_run = 'X'.
    PERFORM process_posting_cogs USING g_rc.
    IF g_rc = 0.
      PERFORM process_posting_ovhd USING g_rc.
    ENDIF.

    IF g_rc <> 0.
      WRITE:/ '*** ERROR ***'.
      LOOP AT return.
        WRITE:/ return-id, return-number, return-message(60).
      ENDLOOP.
    ENDIF.
  ENDIF.

  PERFORM display_out.

*&---------------------------------------------------------------------*
*&      Form  get_master
*&---------------------------------------------------------------------*
FORM get_master USING p_rc.
  SELECT SINGLE * FROM t001
       WHERE bukrs = p_buk.
  IF sy-subrc <> 0.
    p_rc = sy-subrc.
  ELSE.
    REFRESH r_hkont.
    r_hkont-sign = 'I'. r_hkont-option = 'EQ'.

    SELECT * FROM t030
      WHERE ktopl = t001-ktopl
        AND ktosl = 'GBB'
        AND ( komok = 'VAX' OR komok = 'ZAX' ).

      r_hkont-low = t030-konts.  APPEND r_hkont.
    ENDSELECT.

    p_rc = 0.
  ENDIF.
ENDFORM.                    " get_master
*&---------------------------------------------------------------------*
*&      Form  read_value_field
*&---------------------------------------------------------------------*
FORM read_value_field.
  DATA: lw_index(3) TYPE n.

  RANGES: lr_field FOR dd03l-fieldname.

  MOVE: 'A~ARTNR'   TO i_selfld-field.  APPEND i_selfld.
  MOVE: 'A~WERKS'   TO i_selfld-field.  APPEND i_selfld.
*  move: 'D~MAKTX'   to i_selfld-field.  append i_selfld.
  MOVE: 'C~BKLAS'   TO i_selfld-field.  APPEND i_selfld.
*  move: 'A~VKORG'   to i_selfld-field.  append i_selfld.
*  move: 'A~VTWEG'   to i_selfld-field.  append i_selfld.
*  move: 'A~SPART'   to i_selfld-field.  append i_selfld.
*  move: 'A~KNDNR'   to i_selfld-field.  append i_selfld.
  MOVE: 'F~KTGRD'   TO i_selfld-field.  APPEND i_selfld.
*  move: 'E~MTART'   to i_selfld-field.  append i_selfld.
*  move: 'E~PRDHA'   to i_selfld-field.  append i_selfld.
*  move: 'A~MVGR3'   to i_selfld-field.  append i_selfld.
*  move: 'A~MVGR4'   to i_selfld-field.  append i_selfld.
*  move: 'A~MVGR5'   to i_selfld-field.  append i_selfld.

  MOVE  'ABSMG001' TO i_selfld-field.  APPEND i_selfld.
*act
  MOVE  'VV312001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV222001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV242001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV232001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV302001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV252001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV272001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV202001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV282001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV292001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV212001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV262001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV223001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV243001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV342001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV362001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV352001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV332001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV322001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV233001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV303001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV253001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV273001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV203001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV283001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV293001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV213001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV263001' TO i_selfld-field.  APPEND i_selfld.
*std
  MOVE  'VV310001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV220001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV240001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV230001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV300001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV250001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV270001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV200001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV280001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV290001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV210001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV260001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV221001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV241001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV340001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV360001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV350001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV330001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV320001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV231001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV301001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV251001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV271001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV201001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV281001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV291001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV211001' TO i_selfld-field.  APPEND i_selfld.
  MOVE  'VV261001' TO i_selfld-field.  APPEND i_selfld.


*  SELECT fieldname reptext
*    INTO CORRESPONDING FIELDS OF TABLE it_value_field
*    FROM dd03l AS a INNER JOIN dd04t AS b
*      ON a~rollname = b~rollname
*     AND b~ddlanguage = sy-langu
*     AND b~as4local   = 'A'
*     AND b~as4vers    = 0
*   WHERE a~tabname   =  'CE3H201'
*     AND a~fieldname IN lr_field
*     AND a~as4local  =  'A'
*     AND a~as4vers   =  0.
*
**  SORT it_value_field BY reptext.
*  LOOP AT it_value_field.
*    MOVE: sy-tabix TO lw_index.
*
*    MOVE: it_value_field-fieldname TO i_selfld-field.
*    APPEND i_selfld.
*
*    CONCATENATE 'FD' lw_index INTO it_value_field-field.
*    MODIFY it_value_field.
*  ENDLOOP.

*  data: lw_field like it_vc_map-fieldname.
*  loop at it_vc_map.
*    concatenate it_vc_map-fieldname '*' into lw_field.
*
*    loop at it_value_field where fieldname cp lw_field.
*      it_value_field-value_field = it_vc_map-fieldname.
*      modify it_value_field.
*    endloop.
*  endloop.

ENDFORM.                    " read_value_field
*&---------------------------------------------------------------------*
*&      Form  get_co_cogs
*&---------------------------------------------------------------------*
FORM get_co_cogs.
  DATA: lw_field(100),
        lw_index(3) TYPE n.
  DATA: l_cogs LIKE it_co_cogs OCCURS 0 WITH HEADER LINE.

  FIELD-SYMBOLS: <field>.

  PERFORM read_value_field.

  SELECT (i_selfld)
    INTO TABLE l_cogs
    FROM ce4h201 AS a INNER JOIN ce3h201 AS b
                         ON a~paobjnr = b~paobjnr
                        AND b~perbl   = p_perbl
                 LEFT OUTER JOIN mbew AS c
                         ON a~artnr = c~matnr
                        AND a~werks = c~bwkey
                 LEFT OUTER JOIN makt AS d
                         ON a~artnr = d~matnr
                        AND d~spras = sy-langu
                 LEFT OUTER JOIN mara AS e
                         ON a~artnr = e~matnr
                 LEFT OUTER JOIN knvv AS f
                         ON f~kunnr = a~kndnr
                        AND f~vkorg = a~vkorg
                        AND f~vtweg = a~vtweg
                        AND f~spart = a~spart
   WHERE a~aktbo    =  'X'
     AND a~bukrs    =  p_buk
     AND b~paledger =  '01'
     AND b~perbl    =  p_perbl
     AND b~vrgar    =  'F'    "billing
     AND b~plikz    =  '0'
     AND a~artnr    IN r_matnr.
*    and a~kokrs    =  w_kokrs
*    and a~werks    =  p_werks
*    and a~artnr    in s_matnr
*     AND b~versi    =  '0'.

* select only country code...
  LOOP AT l_cogs WHERE ktgrd <> space.
    MOVE l_cogs TO it_co_cogs.
    COLLECT it_co_cogs.
  ENDLOOP.

ENDFORM.                    " calculate_co_cogs
*&---------------------------------------------------------------------*
*&      Form  calc_cogs.
*&---------------------------------------------------------------------*
FORM calc_cogs..
  DATA: lw_field(100),
        lw_index(3) TYPE n,
        l_ac000  TYPE ckml_value_cum,
        l_sd000  TYPE ckml_value_cum,
        l_va000  TYPE ckml_value_cum.


  FIELD-SYMBOLS: <field>.

  CLEAR: it_cogs-ac000, it_cogs-sd000, it_cogs-va000.
  DO 100 TIMES.
    MOVE: sy-index TO lw_index.

    CONCATENATE 'IT_CO_COGS-AC' lw_index INTO lw_field.
    ASSIGN (lw_field) TO <field>.
    IF sy-subrc NE 0.  EXIT.  ENDIF.
    l_ac000 = <field>.

    CONCATENATE 'IT_CO_COGS-SD' lw_index INTO lw_field.
    ASSIGN (lw_field) TO <field>.
    IF sy-subrc NE 0.  EXIT.  ENDIF.
    l_sd000 = <field>.

    it_cogs-ac000 = it_cogs-ac000 + l_ac000.
    it_cogs-sd000 = it_cogs-sd000 + l_sd000.
  ENDDO.
ENDFORM.                    " collect_value_fields

*ALV
*&--------------------------------------------------------------------
*&      Form  make_field_category
*&--------------------------------------------------------------------
FORM field_setting TABLES         p_fieldcat_t LIKE gt_fieldcat
                   USING          p_fieldname       " FIELD name
                                  p_title           " field titlw
                                  p_outputlen       " length
                                  p_key             "
                                  p_just            "
                                  p_noout           "
                                  p_round           "
                                  p_cfield          " currency field nam
                                  p_qfield          " quantity field nam
                                  p_dosum           " make sum
                                  .

  DATA: ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname  = p_fieldname.
*  ls_fieldcat-seltext_s = p_title.
*  ls_fieldcat-seltext_m = p_title.
  ls_fieldcat-seltext_l  = p_title.
  ls_fieldcat-outputlen  = p_outputlen.
  ls_fieldcat-key        = p_key.
  ls_fieldcat-just       = p_just.
  ls_fieldcat-edit       = ''.   "p_edit.
  ls_fieldcat-no_out     = p_noout.
  ls_fieldcat-decimals_out   = p_round.
*  ls_fieldcat-cfieldname = p_cfield.
  ls_fieldcat-currency   = p_cfield.
  ls_fieldcat-qfieldname = p_qfield.
  ls_fieldcat-do_sum     = p_dosum.

  APPEND ls_fieldcat TO p_fieldcat_t.

ENDFORM.                    " fill_field_category
*ALV
*&---------------------------------------------------------------------*
*&      Form  get_obj_sys
*&---------------------------------------------------------------------*
FORM get_obj_sys USING    p_objsys.
*  TABLES: t000.
  SELECT SINGLE logsys INTO p_objsys
         FROM t000
         WHERE mandt EQ sy-mandt.
ENDFORM.                    " get_obj_sys
*&---------------------------------------------------------------------*
*&      Form  call_bapi_post
*&---------------------------------------------------------------------*
FORM call_bapi_post USING p_rc.
  CLEAR p_rc.

  IF p_run = space.
    CALL FUNCTION 'BAPI_ACC_GL_POSTING_CHECK'
         EXPORTING
              documentheader = documentheader
         TABLES
              accountgl      = accountgl
              currencyamount = currencyamount
              return         = return
              extension1     = extension1.
  ELSE.
    CALL FUNCTION 'BAPI_ACC_GL_POSTING_POST'
         EXPORTING
              documentheader = documentheader
         IMPORTING
              obj_type       = obj_type
              obj_key        = obj_key
              obj_sys        = obj_sys
         TABLES
              accountgl      = accountgl
              currencyamount = currencyamount
              return         = return
              extension1     = extension1.
  ENDIF.

  READ TABLE return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    p_rc = 1.
  ENDIF.

  CHECK p_run = 'X'.
  IF p_rc <> 0.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " call_bapi_post
*&---------------------------------------------------------------------*
*&      Form  get_ml_cogs
*&---------------------------------------------------------------------*
FORM get_ml_cogs.
  REFRESH: t_t001k.
  SELECT bwkey bukrs mlbwa FROM t001k
                           INTO CORRESPONDING FIELDS OF TABLE t_t001k
                           WHERE bukrs = p_buk.

  g_bdatj = p_perbl(4).
  g_poper = p_perbl+4(3).
  SELECT SINGLE * FROM marv WHERE bukrs = p_buk.

  LOOP AT t_t001k WHERE mlbwa <> space.

    REFRESH: t_mats, t_mats_portion, t_ckmlpp, t_ckmlcr.
    PERFORM get_materials USING t_t001k
                          CHANGING g_bdatj
                                   g_poper
                                   t_mats.
    DESCRIBE TABLE t_mats LINES h_lines.
    LOOP AT t_mats INTO s_mats.
      h_index = sy-tabix.
      h_counter = h_counter + 1.
      APPEND s_mats TO t_mats_portion.
      IF h_counter >= h_portion_size OR h_index = h_lines.
        PERFORM get_material_periods USING t_mats_portion
                                           t_ckmlpp[]
                                           t_ckmlcr[].
        PERFORM get_mlcd_data USING t_mats_portion
                              CHANGING t_mlcd[]
                                       t_mlcd_not_alloc[].
        PERFORM find_bad_boys USING t_mats_portion
                                    t_ckmlpp[]
                                    t_ckmlcr[]
                                    t_mlcd[]
                                    t_mlcd_not_alloc[]
                              CHANGING t_out[].
        CALL FUNCTION 'CKMS_BUFFER_REFRESH_COMPLETE'.
        REFRESH: t_mats_portion, t_ckmlpp, t_ckmlcr, t_mlcd,
                 t_mlcd_not_alloc.
        CLEAR: h_counter.
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " get_ml_cogs
*&---------------------------------------------------------------------*
*&      Form  get_materials
*&---------------------------------------------------------------------*
FORM get_materials USING    pf_t001k TYPE t001k
                   CHANGING pl_bdatj TYPE bdatj
                            pl_poper TYPE poper
                            pt_mats LIKE t_mats[].

  DATA: lt_kalnr TYPE ckmv0_matobj_tbl,
        ls_mats  TYPE s_mats,
        ls_kalnr TYPE ckmv0_matobj_str.

* Aktuelle Periode als Default / Hausw?rung lesen
*  IF pl_bdatj IS INITIAL OR pl_poper IS INITIAL.
*    IF marv-bukrs <> pf_t001k-bukrs.
*      SELECT SINGLE * FROM marv WHERE bukrs = pf_t001k-bukrs.
*      IF sy-subrc <> 0.
*        CLEAR: marv.
*      ENDIF.
*    ENDIF.
*    IF NOT marv IS INITIAL.
*      pl_bdatj = marv-lfgja.
*      IF pl_poper IS INITIAL.
*        pl_poper = marv-lfmon.
*      ENDIF.
*    ENDIF.
*    CALL FUNCTION 'T001_SINGLE_READ'
*      EXPORTING
**       KZRFB            = ' '
**       MAXTZ            = 0
*        bukrs            = pf_t001k-bukrs
*      IMPORTING
*        wt001            = t001
*      EXCEPTIONS
*        not_found        = 1
*        wrong_call       = 2
*        OTHERS           = 3.
*    IF sy-subrc <> 0.
*      CLEAR: t001.
*    ENDIF.
*  ENDIF.

  REFRESH: pt_mats.

  SELECT h~bwkey h~kalnr h~matnr h~bwtar
         h~sobkz h~vbeln h~posnr h~pspnr
         m~mtart m~matkl m~spart m~meins c~prctr b~bklas
     INTO CORRESPONDING FIELDS OF TABLE pt_mats
     FROM ( ( ckmlhd AS h JOIN mara AS m
            ON h~matnr = m~matnr )
            JOIN marc AS c
            ON h~matnr = c~matnr AND h~bwkey = c~werks )
          JOIN mbew AS b
          ON h~matnr = b~matnr AND h~bwkey = b~bwkey
          AND h~bwtar = b~bwtar
     WHERE h~bwkey = pf_t001k-bwkey
       AND h~mlast = '3'
       AND h~matnr IN r_matnr
       AND b~bklas IN r_bklas.


ENDFORM.                               " get_materials
*&---------------------------------------------------------------------*
*&      Form  get_material_periods
*&---------------------------------------------------------------------*
FORM get_material_periods USING pt_mats LIKE t_mats[]
                                pt_ckmlpp LIKE t_ckmlpp[]
                                pt_ckmlcr LIKE t_ckmlcr[].

  DATA: lt_kalnr TYPE ckmv0_matobj_tbl,
        ls_mats TYPE s_mats,
        ls_kalnr TYPE ckmv0_matobj_str.

  IF pt_mats[] IS INITIAL.
    REFRESH: pt_ckmlpp, pt_ckmlcr.
    EXIT.
  ENDIF.
* Periodens?ze lesen
  REFRESH: lt_kalnr.
  LOOP AT pt_mats INTO ls_mats.
    CLEAR: ls_kalnr.
    ls_kalnr-kalnr = ls_mats-kalnr.
    ls_kalnr-bwkey = ls_mats-bwkey.
    APPEND ls_kalnr TO lt_kalnr.
  ENDLOOP.
  CALL FUNCTION 'CKMS_PERIOD_READ_WITH_ITAB'
    EXPORTING
*     I_REFRESH_BUFFER                =
*     I_READ_ONLY_BUFFER              = ' '
*     I_USE_BUFFER                    = 'X'
*     I_BUILD_SMBEW                   =
      i_bdatj_1                       = g_bdatj
      i_poper_1                       = g_poper
*     I_BDATJ_2                       =
*     I_POPER_2                       =
*     I_BDATJ_3                       =
*     I_POPER_3                       =
*     I_BETWEEN_1_AND_2               =
      i_untper                        = s_runperiod-untper
      i_call_by_reporting             = 'X'
      i_no_chk_periods_complete       = 'X'
    TABLES
      t_kalnr                         = lt_kalnr
      t_ckmlpp                        = pt_ckmlpp
      t_ckmlcr                        = pt_ckmlcr
*     T_MISS_CKMLPP                   =
*     T_MISS_CKMLCR                   =
    EXCEPTIONS
      no_data_found                   = 1
      input_data_inconsistent         = 2
      buffer_inconsistent             = 3
      OTHERS                          = 4.
  IF sy-subrc <> 0 AND
     NOT ( sy-subrc = 1 AND
           NOT ( pt_ckmlpp[] IS INITIAL AND pt_ckmlpp[] IS INITIAL ) ).
*   Probleme
    REFRESH: pt_mats, pt_ckmlpp, pt_ckmlcr.
    EXIT.
  ENDIF.
  SORT: pt_ckmlpp, pt_ckmlcr.

ENDFORM.                    " get_material_periods
*&---------------------------------------------------------------------*
*&      Form  get_mlcd_data
*&---------------------------------------------------------------------*
FORM get_mlcd_data USING pt_mats LIKE t_mats[]
                   CHANGING pt_mlcd LIKE t_mlcd[]
                            pt_mlcd_not_alloc LIKE t_mlcd[].

  DATA: lt_kalnr TYPE ckmv0_matobj_tbl,
        ls_mats TYPE s_mats,
        ls_kalnr TYPE ckmv0_matobj_str.

  REFRESH: lt_kalnr.
  LOOP AT pt_mats INTO ls_mats.
    CLEAR: ls_kalnr.
    ls_kalnr-kalnr = ls_mats-kalnr.
    ls_kalnr-bwkey = ls_mats-bwkey.
    APPEND ls_kalnr TO lt_kalnr.
  ENDLOOP.
  CALL FUNCTION 'CKMCD_MLCD_READ'
      EXPORTING
        i_from_bdatj            = g_bdatj
        i_from_poper            = g_poper
*       I_TO_BDATJ              =
*       I_TO_POPER              =
        i_untper                = s_runperiod-untper
*       I_RUN_ID                =
*       I_NO_BUFFER             =
*       I_REFRESH_BUFFER        =
        i_online                = ' '
*       I_NO_MLCD_CREATE        =
      TABLES
        it_kalnr                = lt_kalnr
        ot_mlcd                 = pt_mlcd
        ot_mlcd_not_alloc       = pt_mlcd_not_alloc
      EXCEPTIONS
        data_error              = 1
        OTHERS                  = 2
              .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  SORT pt_mlcd BY kalnr bdatj poper untper categ curtp.
  SORT pt_mlcd_not_alloc BY kalnr bdatj poper untper curtp.

ENDFORM.                    " get_mlcd_data
*&---------------------------------------------------------------------*
*&      Form  find_bad_boys
*&---------------------------------------------------------------------*
FORM find_bad_boys USING pt_mats LIKE t_mats[]
                         pt_ckmlpp LIKE t_ckmlpp[]
                         pt_ckmlcr LIKE t_ckmlcr[]
                         pt_mlcd LIKE t_mlcd[]
                         pt_mlcd_not_alloc LIKE t_mlcd[]
                   CHANGING pt_out LIKE t_out[].

  DATA: ls_out_ndi TYPE s_out,
        ls_out_cum TYPE s_out,
        ls_out_nin TYPE s_out,
        ls_mats TYPE s_mats,
        ls_ckmlpp TYPE ckmlpp,
        ls_ckmlcr TYPE ckmlcr,
        ls_mlcd TYPE mlcd,
        ls_mlcd_not_alloc TYPE mlcd,
        l_color(3) TYPE c,
        l_ab_menge LIKE mlcd-lbkum,
        l_nin TYPE boole_d,
        l_kalnr_old LIKE mlcd-kalnr.

  LOOP AT pt_mats INTO ls_mats.
    READ TABLE pt_ckmlpp INTO ls_ckmlpp
                         WITH KEY kalnr = ls_mats-kalnr
                                  bdatj = g_bdatj
                                  poper = g_poper
                                  untper = s_runperiod-untper
                                  BINARY SEARCH.
    CHECK sy-subrc = 0.
    CLEAR: ls_out_ndi, ls_out_cum, ls_out_nin.
    MOVE-CORRESPONDING ls_ckmlpp TO ls_out_ndi.
    MOVE-CORRESPONDING ls_ckmlpp TO ls_out_cum.
    MOVE-CORRESPONDING ls_ckmlpp TO ls_out_nin.
    IF ls_ckmlpp-status >= '40'.  "y_einstufig_abgerechnet.
      READ TABLE pt_ckmlcr WITH KEY kalnr = ls_mats-kalnr
                                    bdatj = g_bdatj
                                    poper = g_poper
                                    untper = s_runperiod-untper
                                    BINARY SEARCH
                                    TRANSPORTING NO FIELDS.
      LOOP AT pt_ckmlcr INTO ls_ckmlcr FROM sy-tabix.
        IF ls_ckmlcr-kalnr <> ls_ckmlpp-kalnr OR
           ls_ckmlcr-bdatj <> ls_ckmlpp-bdatj OR
           ls_ckmlcr-poper <> ls_ckmlpp-poper OR
           ls_ckmlcr-untper <> ls_ckmlpp-untper.
          EXIT.
        ENDIF.
*       Kumulierter Bestand
        MOVE-CORRESPONDING ls_ckmlcr TO ls_out_ndi.
        MOVE-CORRESPONDING ls_ckmlcr TO ls_out_cum.
        MOVE-CORRESPONDING ls_ckmlcr TO ls_out_nin.
        ls_out_cum-estprd = ls_ckmlcr-abprd_o + ls_ckmlcr-zuprd_o +
                            ls_ckmlcr-vpprd_o.
        ls_out_cum-estkdm = ls_ckmlcr-abkdm_o + ls_ckmlcr-zukdm_o +
                            ls_ckmlcr-vpkdm_o.
*
        IF ls_ckmlpp-status >= '50'.  "y_mehrstufig_abgerechnet.
          ls_out_cum-mstprd = ls_ckmlcr-abprd_mo + ls_ckmlcr-zuprd_mo.
          ls_out_cum-mstkdm = ls_ckmlcr-abkdm_mo + ls_ckmlcr-zukdm_mo.
        ELSE.
          ls_out_cum-mstprd = ls_ckmlcr-abprd_mo.
          ls_out_cum-mstkdm = ls_ckmlcr-abkdm_mo.
        ENDIF.
*
        ls_out_ndi-estprd = ls_out_cum-estprd.
        ls_out_ndi-estkdm = ls_out_cum-estkdm.
        ls_out_ndi-mstprd = ls_out_cum-mstprd.
        ls_out_ndi-mstkdm = ls_out_cum-mstkdm.
        ls_out_ndi-estprd = ls_out_ndi-estprd -
                            ( ls_ckmlcr-vnprd_ea + ls_ckmlcr-ebprd_ea ).
        ls_out_ndi-estkdm = ls_out_ndi-estkdm -
                            ( ls_ckmlcr-vnkdm_ea + ls_ckmlcr-ebkdm_ea ).
        ls_out_ndi-mstprd = ls_out_ndi-mstprd -
                            ( ls_ckmlcr-vnprd_ma + ls_ckmlcr-ebprd_ma ).
        ls_out_ndi-mstkdm = ls_out_ndi-mstkdm -
                            ( ls_ckmlcr-vnkdm_ma + ls_ckmlcr-ebkdm_ma ).
        ls_out_ndi-sumdif = ls_out_ndi-estprd + ls_out_ndi-estkdm +
                            ls_out_ndi-mstprd + ls_out_ndi-mstkdm.
*       Gibt's eine 'Nicht verrechnet'-Zeile?
        READ TABLE pt_mlcd_not_alloc INTO ls_mlcd_not_alloc
                                     WITH KEY kalnr = ls_ckmlcr-kalnr
                                              bdatj = ls_ckmlcr-bdatj
                                              poper = ls_ckmlcr-poper
                                             untper = ls_ckmlcr-untper
                                              curtp = ls_ckmlcr-curtp
                                              BINARY SEARCH.
        IF sy-subrc = 0.
          l_nin = 'X'.
        ELSE.
          CLEAR: l_nin.
        ENDIF.
        IF NOT ls_out_ndi-sumdif IS INITIAL OR
           NOT l_nin IS INITIAL.
          IF ls_out_ndi-kalnr <> l_kalnr_old.
            l_kalnr_old = ls_out_ndi-kalnr.
            IF l_color = 'C21'.
              l_color = 'C20'.
            ELSE.
              l_color = 'C21'.
            ENDIF.
          ENDIF.
          READ TABLE pt_mats INTO ls_mats
                             WITH KEY kalnr = ls_out_ndi-kalnr.
          IF sy-subrc = 0.
            MOVE-CORRESPONDING ls_mats TO ls_out_cum.
            MOVE-CORRESPONDING ls_mats TO ls_out_ndi.
            MOVE-CORRESPONDING ls_mats TO ls_out_nin.
          ENDIF.
*NDI
          IF NOT ls_out_ndi-sumdif IS INITIAL.
            ls_out_ndi-pos_type = 'NDI'.
            ls_out_ndi-color = l_color.
            ls_out_ndi-pos_type_text = text-006.
            CLEAR: ls_out_ndi-menge, ls_out_ndi-wert.
            ls_out_ndi-prdif = ls_out_ndi-estprd + ls_out_ndi-mstprd.
            ls_out_ndi-krdif = ls_out_ndi-estkdm + ls_out_ndi-mstkdm.
            ls_out_ndi-estdif = ls_out_ndi-estprd + ls_out_ndi-estkdm.
            ls_out_ndi-mstdif = ls_out_ndi-mstprd + ls_out_ndi-mstkdm.
            APPEND ls_out_ndi TO pt_out.
          ENDIF.
*
*          if not p_all is initial.
*          ls_out_cum-pos_type = 'CUM'.
*          ls_out_cum-color = l_color.
*          ls_out_cum-pos_type_text = text-008.
**           Anfangsbestands-Menge (R?kbuchungen) aus MLCD
*          CLEAR: l_ab_menge, ls_out_cum-pbpopo.
*          READ TABLE pt_mlcd TRANSPORTING NO FIELDS
*                             WITH KEY kalnr = ls_ckmlcr-kalnr
*                                      bdatj = ls_ckmlcr-bdatj
*                                      poper = ls_ckmlcr-poper
*                                      untper = ls_ckmlcr-untper
*                                      categ = 'AB'
*                                      curtp = ls_ckmlcr-curtp
*                                      BINARY SEARCH.
*          IF sy-subrc = 0.
*            LOOP AT pt_mlcd FROM sy-tabix INTO ls_mlcd.
*              IF ls_mlcd-kalnr <> ls_ckmlcr-kalnr OR
*                 ls_mlcd-bdatj <> ls_ckmlcr-bdatj OR
*                 ls_mlcd-poper <> ls_ckmlcr-poper OR
*                 ls_mlcd-untper <> ls_ckmlcr-untper OR
*                 ls_mlcd-categ <> 'AB' OR
*                 ls_mlcd-curtp <> ls_ckmlcr-curtp.
*                EXIT.
*              ENDIF.
*              l_ab_menge = l_ab_menge + ls_mlcd-lbkum.
*            ENDLOOP.
*          ENDIF.
*          ls_out_cum-prdif = ls_out_cum-estprd + ls_out_cum-mstprd.
*          ls_out_cum-krdif = ls_out_cum-estkdm + ls_out_cum-mstkdm.
*          ls_out_cum-estdif = ls_out_cum-estprd + ls_out_cum-estkdm.
*          ls_out_cum-mstdif = ls_out_cum-mstprd + ls_out_cum-mstkdm.
*          ls_out_cum-sumdif = ls_out_cum-estprd + ls_out_cum-estkdm +
*                                ls_out_cum-mstprd + ls_out_cum-mstkdm.
*          ls_out_cum-menge = ls_ckmlpp-abkumo + ls_ckmlpp-zukumo +
*                             ls_ckmlpp-vpkumo + l_ab_menge.
*          ls_out_cum-wert = ls_out_cum-menge * ls_out_cum-stprs /
*                            ls_out_cum-peinh.
*          APPEND ls_out_cum TO pt_out.
*          endif.

*          if ( not p_notinc is initial or not p_all is initial ) and
*             not l_nin is initial.
*NIN
          IF NOT l_nin IS INITIAL.
            ls_out_nin-pos_type = 'NIN'.
            ls_out_nin-color = l_color.
            ls_out_nin-pos_type_text = text-007.
            CLEAR: ls_out_nin-menge, ls_out_nin-wert.
            ls_out_nin-estprd = ls_mlcd_not_alloc-estprd.
            ls_out_nin-estkdm = ls_mlcd_not_alloc-estkdm.
            ls_out_nin-mstprd = ls_mlcd_not_alloc-mstprd.
            ls_out_nin-mstkdm = ls_mlcd_not_alloc-mstkdm.
            ls_out_nin-prdif = ls_out_nin-estprd + ls_out_nin-mstprd.
            ls_out_nin-krdif = ls_out_nin-estkdm + ls_out_nin-mstkdm.
            ls_out_nin-estdif = ls_out_nin-estprd + ls_out_nin-estkdm.
            ls_out_nin-mstdif = ls_out_nin-mstprd + ls_out_nin-mstkdm.
            ls_out_nin-sumdif = ls_out_nin-estprd + ls_out_nin-estkdm +
                                  ls_out_nin-mstprd + ls_out_nin-mstkdm.
            APPEND ls_out_nin TO pt_out.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ELSE.
*   Da kommt noch was!
    ENDIF.
  ENDLOOP.


ENDFORM.                               " find_bad_boys
*&---------------------------------------------------------------------*
*&      Form  calcluate
*&---------------------------------------------------------------------*
FORM calcluate.
  REFRESH it_cogs.
  LOOP AT it_co_cogs WHERE matnr <> space.

* check val.class
    CHECK it_co_cogs-bklas IN r_bklas.

    MOVE-CORRESPONDING it_co_cogs TO it_cogs.
    PERFORM calc_cogs.

    it_cogs-va000_old = it_cogs-ac000 - it_cogs-sd000.

    IF it_co_cogs-menge <> 0.
      READ TABLE i_mbewh WITH KEY matnr = it_cogs-matnr
                                  bwkey = it_cogs-werks.
      IF sy-subrc <> 0.
        READ TABLE i_mbewh WITH KEY matnr = it_cogs-matnr.
      ENDIF.

      it_cogs-va000 =
        ( - i_mbewh-stprs + i_mbewh-verpr ) * it_co_cogs-menge.
    ENDIF.

    CASE it_co_cogs-ktgrd.
      WHEN '02'.
        CASE it_cogs-bklas.
          WHEN '7920'.
            it_cogs-hkont = cogs_ff.
          WHEN '7900'.
            it_cogs-hkont = cogs_fs.
          WHEN OTHERS.
            it_cogs-hkont = cogs_fr.
        ENDCASE.
      WHEN OTHERS.
        CASE it_cogs-bklas.
          WHEN '7920'.
            it_cogs-hkont = cogs_df.
          WHEN '7900'.
            it_cogs-hkont = cogs_ds.
          WHEN OTHERS.
            it_cogs-hkont = cogs_dr.
        ENDCASE.
    ENDCASE.


    COLLECT it_cogs. CLEAR: it_cogs.

  ENDLOOP.

* move to summary ITAB and derive G/L
*  loop at it_cogs.
*    move-corresponding it_cogs to it_cogs_sum.
*    append  it_cogs_sum.
*  endloop.

  LOOP AT t_out.
    CLEAR: it_cogs_sum.

    READ TABLE it_cogs WITH KEY bklas = t_out-bklas
                                matnr = t_out-matnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING it_cogs TO it_cogs_sum.
    ELSE.
      it_cogs_sum-bklas  = t_out-bklas.
      it_cogs_sum-matnr  = t_out-matnr.
    ENDIF.

    IF t_out-pos_type = 'NDI'.
      it_cogs_sum-ndi    = t_out-sumdif.
    ELSEIF t_out-pos_type = 'NIN'.
      it_cogs_sum-nin    = t_out-sumdif.
    ENDIF.
    it_cogs_sum-pbpopo   = t_out-pbpopo.

    COLLECT it_cogs_sum.
  ENDLOOP.

  LOOP AT it_cogs_sum.
    it_cogs_sum-cogs = it_cogs_sum-va000.
    it_cogs_sum-nins = it_cogs_sum-nin - it_cogs_sum-va000.
    it_cogs_sum-ndis = it_cogs_sum-ndi.
    it_cogs_sum-ovhd = it_cogs_sum-nins + it_cogs_sum-ndis.

    MODIFY it_cogs_sum.
  ENDLOOP.


  SORT it_cogs     BY bklas	ktgrd matnr.
  SORT it_cogs_sum BY bklas	matnr.
ENDFORM.                    " calcluate
*&---------------------------------------------------------------------*
*&      Form  display_out
*&---------------------------------------------------------------------*
FORM display_out.
  PERFORM field_setting TABLES gt_fieldcat USING :
 'WERKS'     'Plant'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'BKLAS'     'Val.C'          '04' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MATNR'     'Material'       '18' ' ' 'L'  ' '  ' '  '  ' ' '  ' ',
 'MENGE'     'Qty'            '10' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'VA000'     'Var'            '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'PBPOPO'    'Qty SubAdj'     '10' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'NDIS'      'MatOvhd-NDI'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'NINS'      'MatOvhd-NIN'    '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'COGS'      'COGS'           '16' ' ' 'R'  ' '  ' '  '  ' ' '  'X',
 'HKONT'     'Account'        '10' ' ' 'L'  ' '  ' '  '  ' ' '  'X'.

  g_repid = sy-repid.

  IF p_disp = 'GRID'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
         EXPORTING
              i_callback_program = g_repid
              it_fieldcat        = gt_fieldcat
              i_save             = 'A'
         TABLES
              t_outtab           = it_cogs_sum
         EXCEPTIONS
              program_error      = 1
              OTHERS             = 2.
  ELSE.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
         EXPORTING
              i_callback_program = g_repid
              it_fieldcat        = gt_fieldcat
              i_save             = 'A'
         TABLES
              t_outtab           = it_cogs_sum
         EXCEPTIONS
              program_error      = 1
              OTHERS             = 2.

  ENDIF.

ENDFORM.                    " display_out
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_line_cogs
*&---------------------------------------------------------------------*
FORM fill_bapi_line_cogs  USING p_cnt.
  DATA: l_amt LIKE bapiaccr08-amt_doccur,
        l_act LIKE ska1-saknr.

  CASE it_cogs-ktgrd.
    WHEN '02'.
      CASE it_cogs-bklas.
        WHEN '7920'.
          l_act = cogs_ff.
        WHEN '7900'.
          l_act = cogs_fs.
        WHEN OTHERS.
          l_act = cogs_fr.
      ENDCASE.
    WHEN OTHERS.
      CASE it_cogs-bklas.
        WHEN '7920'.
          l_act = cogs_df.
        WHEN '7900'.
          l_act = cogs_ds.
        WHEN OTHERS.
          l_act = cogs_dr.
      ENDCASE.
  ENDCASE.

* change sign
  l_amt = - it_cogs-va000.

  CONCATENATE it_cogs-ktgrd '-' it_cogs-bklas INTO accountgl-item_text.
  accountgl-alloc_nmbr = it_cogs-matnr.
  accountgl-ref_key_3  = it_cogs-menge.

* 1st lineitem
  p_cnt = p_cnt + 1.
  MOVE: p_cnt        TO accountgl-itemno_acc,
        p_cnt        TO currencyamount-itemno_acc,
        cogs_00      TO accountgl-gl_account,
        l_amt        TO currencyamount-amt_doccur.
  APPEND: accountgl, currencyamount.

* 2nd lineitem
  p_cnt = p_cnt + 1.
  l_amt = - l_amt.
  MOVE: p_cnt        TO accountgl-itemno_acc,
        p_cnt        TO currencyamount-itemno_acc,
        l_act        TO accountgl-gl_account,
        l_amt        TO currencyamount-amt_doccur.
  APPEND: accountgl, currencyamount.
ENDFORM.                    " fill_bapi_line
*&---------------------------------------------------------------------*
*&      Form  process_posting_cogs
*&---------------------------------------------------------------------*
FORM process_posting_cogs USING p_rc.
  DATA: l_cnt TYPE i.

  LOOP AT it_cogs WHERE va000 <> 0.
    AT NEW bklas.
* header......................
      PERFORM fill_bapi_header.
      documentheader-ref_doc_no = 'COGS-VAR'.
      CONCATENATE it_cogs-ktgrd '-' it_cogs-bklas
                          INTO documentheader-header_txt.
      CLEAR l_cnt.
    ENDAT.

    PERFORM fill_bapi_line_cogs  USING l_cnt.

    IF l_cnt > 500.
      PERFORM call_bapi_post USING p_rc.
      IF p_rc <> 0. EXIT. ENDIF.

* header......................
      PERFORM fill_bapi_header.
      documentheader-ref_doc_no = 'COGS-VAR'.
      CONCATENATE it_cogs-ktgrd '-' it_cogs-bklas
                          INTO documentheader-header_txt.
      CLEAR l_cnt.

      CONTINUE.
    ENDIF.

    AT END OF bklas.
* posting
      PERFORM call_bapi_post USING p_rc.
      IF p_rc <> 0. EXIT. ENDIF.
    ENDAT.
  ENDLOOP.

ENDFORM.                    " process_posting_cogs
*&---------------------------------------------------------------------*
*&      Form  process_posting_ovhd
*&---------------------------------------------------------------------*
FORM process_posting_ovhd USING    p_rc.
  DATA: l_cnt TYPE i.

  LOOP AT it_cogs_sum WHERE ( ndi <> 0 OR nin <> 0 ).       "ovhd <> 0.
    AT NEW bklas.
* header......................
      PERFORM fill_bapi_header.
      documentheader-ref_doc_no  = 'COGS-OVHD'.
      documentheader-header_txt  = it_cogs_sum-bklas.
      CLEAR l_cnt.
    ENDAT.

    PERFORM fill_bapi_line_ovhd  USING l_cnt.

    IF l_cnt > 330.
      PERFORM call_bapi_post USING p_rc.
      IF p_rc <> 0. EXIT. ENDIF.

* header......................
      PERFORM fill_bapi_header.
      documentheader-ref_doc_no  = 'COGS-OVHD'.
      documentheader-header_txt  = it_cogs_sum-bklas.
      CLEAR l_cnt.

      CONTINUE.
    ENDIF.


    AT END OF bklas.
* posting
      PERFORM call_bapi_post USING p_rc.
      IF p_rc <> 0. EXIT. ENDIF.
    ENDAT.
  ENDLOOP.
ENDFORM.                    " process_posting_ovhd
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_line_ovhd
*&---------------------------------------------------------------------*
FORM fill_bapi_line_ovhd USING    p_cnt.
  DATA: l_amt LIKE bapiaccr08-amt_doccur,
        l_ndi LIKE bapiaccr08-amt_doccur,
        l_nin LIKE bapiaccr08-amt_doccur.

  l_amt = - it_cogs_sum-ovhd.

  l_ndi =   it_cogs_sum-ndis.
  l_nin =   it_cogs_sum-nins.

  accountgl-alloc_nmbr = it_cogs_sum-matnr.
  accountgl-ref_key_3  = it_cogs_sum-pbpopo.

* 1st lineitem
  accountgl-item_text  = 'NDI/NIN'.
  p_cnt = p_cnt + 1.
  MOVE: p_cnt        TO accountgl-itemno_acc,
  p_cnt        TO currencyamount-itemno_acc,
  cogs_00      TO accountgl-gl_account,
  l_amt        TO currencyamount-amt_doccur.
  APPEND: accountgl, currencyamount.

* 2nd lineitem - NDI
  IF it_cogs_sum-ndi <> 0.
    accountgl-item_text  = 'NDI'. "Not Distributed'.
    p_cnt = p_cnt + 1.
    MOVE: p_cnt        TO accountgl-itemno_acc,
        p_cnt        TO currencyamount-itemno_acc,
        cogs_oh      TO accountgl-gl_account,
        l_ndi        TO currencyamount-amt_doccur.
    APPEND: accountgl, currencyamount.
  ENDIF.

* 3rd lineitem - NIN
  IF it_cogs_sum-nin <> 0.
    accountgl-item_text  = 'NIN'. "'Not Included'.
    p_cnt = p_cnt + 1.
    MOVE: p_cnt        TO accountgl-itemno_acc,
        p_cnt        TO currencyamount-itemno_acc,
        cogs_oh      TO accountgl-gl_account,
        l_nin        TO currencyamount-amt_doccur.
    APPEND: accountgl, currencyamount.
  ENDIF.

ENDFORM.                    " fill_bapi_line_ovhd
*&---------------------------------------------------------------------*
*&      Form  fill_bapi_header
*&---------------------------------------------------------------------*
FORM fill_bapi_header.
* Andy Choi
* Notes:306504
*
* amount: +/- sign to determine dr/cr
* quantity without +/- signs.
* call the BAPI and set a COMMIT WORK afterwards
*
* Enter your own value in the field OBJ_TYPE (reference transaction).
*    must maintain this value in the table TTYP.
*
* OBJ_KEY (AWKEY) reference key (document number of the source)
*   AWKEY comprises AWREF and AWORG. AWREF is the source document
*   number and AWORG reflects related organizational units, such as
*   company code and fiscal year (you maintain the structure of AWORG
*   in table TTYP for each reference transaction, AWTYP in field
*   STRUC).
*
* OBJ_SYS (AWSYS) Logical system (logical system of source doc)
*
*Segments for tax items
* In the segments for the tax items, the incorrect required fields were
* described in the documentation. The correct required fields are:
*
*    o  Item number (ITEMNO_ACC)
*    o  Tax code (TAX_CODE)
*    o  Condition type (COND_KEY)
*
* If the tax jurisdiction code is active, you must also make entries in
* the fields TAXJURCODE and TAXJURCODE_DEEP.
* See also note 333424 for the tax items.
*
* Extension: EXIT_SAPLACC4_001

  REFRESH : accountgl, currencyamount, return, extension1.
  CLEAR : documentheader.

*Default: BKPFF: Actg doc.direct inpt
*IDOC reverse -> From original system
*  documentheader-obj_type       = 'Y001'. "Default:BKPFF
*  documentheader-obj_key        = 'CHOI'.
** documentheader-obj_key_r      = space.
*  documentheader-obj_sys        = g_objsys.
  documentheader-username       = sy-uname.
  documentheader-comp_code      = p_buk.
  documentheader-doc_type       = p_blart.
*documentheader-ac_doc_no      = space.
  documentheader-fisc_year      = p_budat(4).
  documentheader-fis_period     = p_budat+4(2).
  documentheader-doc_date       = sy-datum.
  documentheader-pstng_date     = p_budat.
  documentheader-trans_date     = p_budat.

*lineitem currency
  currencyamount-currency    = t001-waers.

ENDFORM.                    " fill_bapi_header
