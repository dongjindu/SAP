*&---------------------------------------------------------------------*
*& Report  ZSAP_MF47_MISSING_PRODVERS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zsap_mf47_missing_prodvers.

*****************************************************************
* This report will check if production versions used in product
* cost controllers are wrongly deleted. Product cost controllers
* with invalid production versions won't be displayed and
* maintained in transaction MF47 (postprocessing list for
* components on line)
* Please read note 702852 for further details!
*****************************************************************

TABLES: mdrr, safk.
TYPES: BEGIN OF ty_sestax,
          mdv01 LIKE sesta-mdv01,
          mdv02 LIKE sesta-mdv02,
          matnr LIKE sesta-matnr,
          werks LIKE sesta-werks,
          verid LIKE sesta-verid,
          stday LIKE sesta-stday,
          enday LIKE sesta-enday,
          aufnr LIKE sesta-aufnr,
          rsnum LIKE sesta-rsnum,
          maktx LIKE makt-maktx,
          cbox  TYPE c,
          pkosa LIKE sesta-pkosa,
          auart LIKE sesta-auart,
          kdauf LIKE rm61b-kdauf,
          kdpos LIKE rm61b-kdpos,
          pspnr LIKE rm61b-pspnr,
          objnr LIKE jest-objnr,
          reptp LIKE t437x-reptp,
       END OF ty_sestax.

TYPES: tty_sestax TYPE ty_sestax OCCURS 0.

TYPES: BEGIN OF ty_mkal_d,
         werks TYPE mkal-werks,
         matnr TYPE mkal-matnr,
         verid TYPE mkal-verid,
         END OF ty_mkal_d.

DATA:
BEGIN OF s_werks_range OCCURS 1,
        sign      TYPE c,
        option(2) TYPE c,
        low    LIKE  sesta-werks,
        high   LIKE  sesta-werks,
     END OF s_werks_range.

DATA:
  sesta       TYPE sesta,
  lw_mkal     TYPE mkal,
  lw_mkal_d   TYPE ty_mkal_d,
  lt_mkal_d   TYPE STANDARD TABLE OF ty_mkal_d,
  lw_sestax   TYPE ty_sestax,
  lt_sestax   TYPE tty_sestax WITH HEADER LINE,
  lw_mdrr     TYPE mdrr,
  l_tabix     TYPE sy-tabix.

* Parameters and select-options
PARAMETERS:     pa_werks LIKE sesta-werks
                              OBLIGATORY MEMORY ID wrk,
                pa_lgort LIKE mdrs-lgort.
SELECT-OPTIONS: so_baugr FOR  sesta-matnr    " Materialnr. Baugruppe
                         MATCHCODE OBJECT mat1,
                so_verid FOR  sesta-verid,
                so_kdauf FOR  mdrr-kdauf,
                so_kdpos FOR  mdrr-kdpos,
                so_pspnr FOR  mdrr-pspel,
                so_matnr FOR  mdrr-matnr    " Materialnr. Komponente
                         MATCHCODE OBJECT mat1.

* Fill storage location
IF pa_lgort IS INITIAL.
  MOVE '%' TO pa_lgort.
ENDIF.

* Fill ranges for plant
MOVE 'I' TO s_werks_range-sign.
MOVE 'EQ'  TO s_werks_range-option.
MOVE pa_werks TO s_werks_range-low.
APPEND s_werks_range.

* Get rsnum-entries for post processing (transaction MF47)
CALL FUNCTION 'RM_SELECT_RSNUM'
     TABLES
          i_werks_range = s_werks_range
          i_matnr_range = so_baugr
          i_verid_range = so_verid
          i_pspel_range = so_pspnr
          i_kdauf_range = so_kdauf
          i_kdpos_range = so_kdpos
          e_sestax      = lt_sestax
     EXCEPTIONS
          no_authority  = 1.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

* Check if there are backlogs. Only keep reservations that belong to
* existing backlogs
LOOP AT lt_sestax INTO lw_sestax.
  l_tabix = sy-tabix.
  SELECT * FROM mdrr INTO lw_mdrr
    WHERE rsnum = lw_sestax-rsnum
     AND  matnr IN   so_matnr
     AND  lgort LIKE pa_lgort.
  ENDSELECT.
  IF sy-subrc <> 0 OR lw_mdrr IS INITIAL.
*   No backlog exists -> delete entry
    DELETE lt_sestax INDEX l_tabix.
  ENDIF.
ENDLOOP.

* Check existence of production versions
LOOP AT lt_sestax INTO lw_sestax.
  IF NOT lw_sestax-verid IS INITIAL.
    SELECT * FROM mkal INTO lw_mkal
      WHERE    werks = lw_sestax-werks AND
               matnr = lw_sestax-matnr AND
               verid = lw_sestax-verid.
    ENDSELECT.
    IF sy-subrc <> 0.
*     No database entry for production version found
      lw_mkal_d-werks = lw_sestax-werks.
      lw_mkal_d-matnr = lw_sestax-matnr.
      lw_mkal_d-verid = lw_sestax-verid.
      APPEND lw_mkal_d TO lt_mkal_d.
      CLEAR lw_mkal_d.
    ENDIF.
  ELSE.
    CONTINUE.
  ENDIF.
ENDLOOP.
* Display the missing production versions
IF NOT lt_mkal_d IS INITIAL.
  LOOP AT lt_mkal_d INTO lw_mkal_d.
    WRITE:
    /5 lw_mkal_d-matnr, 20 lw_mkal_d-werks, 35 lw_mkal_d-verid.
  ENDLOOP.
ELSE.
* No missing production version
  WRITE: / text-004.
ENDIF.

* Page header
TOP-OF-PAGE.
  WRITE: /5 text-001, 20 text-002, 35 text-003.
  WRITE: sy-uline.
