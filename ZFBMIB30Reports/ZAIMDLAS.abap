*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000496473                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 40B          All Support Package Levels                   $*
*$  Release 45B          All Support Package Levels                   $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZAIMDLAS
*& Object Header   PROG ZAIMDLAS
*&-------------------------------------------------------------------*
*& REPORT ZAIMDLAS
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT zaimdlas LINE-SIZE 86
       NO STANDARD PAGE HEADING.

INCLUDE lkbppequ.
INCLUDE rbonrart.

FIELD-SYMBOLS: <label>.

TYPE-POOLS: sscr, im.

TABLES: aufk, proj, imak, impr, bpge.

PARAMETERS: test_run AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN SKIP 1.

PARAMETERS: x_order AS CHECKBOX.
SELECT-OPTIONS: order FOR aufk-aufnr.
PARAMETERS: x_pro AS CHECKBOX.
SELECT-OPTIONS: pro FOR proj-pspid.
PARAMETERS: x_appreq AS CHECKBOX.
SELECT-OPTIONS: appreq FOR imak-posid.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN  BEGIN OF BLOCK cip.
  PARAMETERS:     program LIKE imtp-prnam,
                  app_year LIKE imtp-gjahr.
  SELECT-OPTIONS: position FOR impr-posid
                  NO INTERVALS.
SELECTION-SCREEN END   OF BLOCK cip.

DATA: BEGIN OF  tab_ipgsel OCCURS 0,
        posnr   LIKE imzo-posnr,
        objnr   LIKE imzo-objnr,
      END   OF  tab_ipgsel.

DATA: BEGIN OF  tab_order OCCURS 0,
        aufnr   LIKE aufk-aufnr,
        ktext   LIKE aufk-ktext,
        objnr   LIKE imzo-objnr,
        ippos   LIKE imzo-ippos,
        gjahr   LIKE imzo-gjahr,
        p_prnam LIKE imtp-prnam,
        p_gjahr LIKE imtp-gjahr,
        p_posid LIKE impr-posid,
        p_xaktb LIKE impr-xaktb,
        p_posnr LIKE imzo-posnr,
      END   OF  tab_order.

DATA: BEGIN OF  tab_wbsel OCCURS 0,
        posid   LIKE prps-posid,
        post1   LIKE prps-post1,
        objnr   LIKE imzo-objnr,
        ippos   LIKE imzo-ippos,
        gjahr   LIKE imzo-gjahr,
        p_prnam LIKE imtp-prnam,
        p_gjahr LIKE imtp-gjahr,
        p_posid LIKE impr-posid,
        p_xaktb LIKE impr-xaktb,
        p_posnr LIKE imzo-posnr,
      END   OF  tab_wbsel.

DATA: BEGIN OF  tab_appreq OCCURS 0,
        posnr   LIKE imak-posnr,
        posid   LIKE imak-posid,
        txt50   LIKE imakt-txt50,
        objnr   LIKE imzo-objnr,
        ippos   LIKE imzo-ippos,
        gjahr   LIKE imzo-gjahr,
        p_prnam LIKE imtp-prnam,
        p_gjahr LIKE imtp-gjahr,
        p_posid LIKE impr-posid,
        p_xaktb LIKE impr-xaktb,
        p_posnr LIKE imzo-posnr,
      END   OF  tab_appreq.

DATA: tab_imakt    LIKE imakt    OCCURS 0 WITH HEADER LINE.
DATA: tab_obj      LIKE raip_obj OCCURS 0 WITH HEADER LINE.
DATA: tab_obj_pack LIKE raip_obj OCCURS 0 WITH HEADER LINE.

DATA: restriction TYPE sscr_restrict.

DATA: pack_size TYPE i VALUE 100,
      counter   TYPE i,
      subrc     LIKE sy-subrc,
      tabix     LIKE sy-tabix.

DATA: on  LIKE sy-datar VALUE 'X',
      off LIKE sy-datar VALUE ' '.


INITIALIZATION.

*  Select-Option for program position:
*  only single values allowed.
   PERFORM restriction_create.
   CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
     EXPORTING
       restriction = restriction
     EXCEPTIONS
       OTHERS      = 1.

*  Set labels for parameters and select options.
   PERFORM initial_screen_labels_set.


AT SELECTION-SCREEN ON BLOCK cip.

*  Program definition or approval year indicated.
   IF NOT program  IS INITIAL OR
      NOT app_year IS INITIAL .
*     Program definition and approval year
*     have to be indicated.
      IF program  IS INITIAL OR
         app_year IS INITIAL .
         MESSAGE e076(ap).
*     Program definition and approval year
*     must exist.
      ELSE.
         SELECT COUNT(*) FROM imtp
           WHERE prnam = program
           AND   gjahr = app_year.
         IF sy-dbcnt = 0.
            MESSAGE e003(ap) WITH program app_year.
         ENDIF.
      ENDIF.
*  Neither program definition nor approval year indicated ...
   ELSE.
*     ... but range of positions indicated.
      IF NOT position[] IS INITIAL.
*        Error!
         MESSAGE e076(ap).
      ENDIF.
   ENDIF.


START-OF-SELECTION.

* Selection by program positions?
IF NOT program  IS INITIAL AND
   NOT app_year IS INITIAL .
*  Create selection table TAB_IPGSEL.
   PERFORM tab_ipgsel_create.
ENDIF.

* Select orders to be detached.
IF x_order = on.
*  Selection by investment program positions.
   IF NOT program  IS INITIAL AND
      NOT app_year IS INITIAL .
      IF NOT tab_ipgsel[] IS INITIAL.
         SELECT aufk~aufnr
                aufk~ktext
                imzo~objnr
                imzo~ippos
                imzo~gjahr
                impr~prnam AS p_prnam
                impr~gjahr AS p_gjahr
                impr~posid AS p_posid
                impr~xaktb AS p_xaktb
                imzo~posnr AS p_posnr
                INTO CORRESPONDING FIELDS OF TABLE tab_order
                FROM aufk INNER JOIN imzo ON aufk~objnr = imzo~objnr
                          INNER JOIN impr ON imzo~posnr = impr~posnr
                FOR ALL entries IN tab_ipgsel
                WHERE aufk~aufnr IN ORDER
                and   IMZO~POSNR =  TAB_IPGSEL-POSNR
                AND   imzo~objnr =  tab_ipgsel-objnr.
      ENDIF.
*  Ordinary selection.
   ELSE.
      SELECT aufk~aufnr
             aufk~ktext
             imzo~objnr
             imzo~ippos
             imzo~gjahr
             impr~prnam AS p_prnam
             impr~gjahr AS p_gjahr
             impr~posid AS p_posid
             impr~xaktb AS p_xaktb
             imzo~posnr AS p_posnr
             INTO CORRESPONDING FIELDS OF TABLE tab_order
             FROM aufk INNER JOIN imzo ON aufk~objnr = imzo~objnr
                       INNER JOIN impr ON imzo~posnr = impr~posnr
             WHERE aufk~aufnr IN ORDER.
   ENDIF.
ENDIF.

* Select WBS elements to be detached.
IF x_pro = on.
*  Selection by investment program positions.
   IF NOT program  IS INITIAL AND
      NOT app_year IS INITIAL .
      IF NOT tab_ipgsel[] IS INITIAL.
         SELECT prps~posid
                prps~post1
                imzo~objnr
                imzo~ippos
                imzo~gjahr
                impr~prnam AS p_prnam
                impr~gjahr AS p_gjahr
                impr~posid AS p_posid
                impr~xaktb AS p_xaktb
                imzo~posnr AS p_posnr
                INTO CORRESPONDING FIELDS OF TABLE tab_wbsel
                FROM proj INNER JOIN prps ON proj~pspnr = prps~psphi
                          INNER JOIN imzo ON prps~objnr = imzo~objnr
                          INNER JOIN impr ON imzo~posnr = impr~posnr
                FOR ALL entries IN tab_ipgsel
                WHERE proj~pspid IN pro
                AND   imzo~posnr =  tab_ipgsel-posnr
                AND   imzo~objnr =  tab_ipgsel-objnr.
      ENDIF.
*  Ordinary selection.
   ELSE.
      SELECT prps~posid
             prps~post1
             imzo~objnr
             imzo~ippos
             imzo~gjahr
             impr~prnam AS p_prnam
             impr~gjahr AS p_gjahr
             impr~posid AS p_posid
             impr~xaktb AS p_xaktb
             imzo~posnr AS p_posnr
             INTO CORRESPONDING FIELDS OF TABLE tab_wbsel
             FROM proj INNER JOIN prps ON proj~pspnr = prps~psphi
                       INNER JOIN imzo ON prps~objnr = imzo~objnr
                       INNER JOIN impr ON imzo~posnr = impr~posnr
             WHERE proj~pspid IN pro.
   ENDIF.
ENDIF.

* Select appropriation requests to be detached.
IF x_appreq = on.
*  Selection by investment program positions.
   IF NOT program  IS INITIAL AND
      NOT app_year IS INITIAL .
      IF NOT tab_ipgsel[] IS INITIAL.
         SELECT imak~posnr
                imak~posid
                imzo~objnr
                imzo~ippos
                imzo~gjahr
                impr~prnam AS p_prnam
                impr~gjahr AS p_gjahr
                impr~posid AS p_posid
                impr~xaktb AS p_xaktb
                imzo~posnr AS p_posnr
                INTO CORRESPONDING FIELDS OF TABLE tab_appreq
                FROM imak INNER JOIN imzo ON imak~objnr = imzo~objnr
                          INNER JOIN impr ON imzo~posnr = impr~posnr
                FOR ALL entries IN tab_ipgsel
                WHERE imak~posid IN appreq
                AND   imzo~posnr =  tab_ipgsel-posnr
                AND   imzo~objnr =  tab_ipgsel-objnr.
      ENDIF.
*  Ordinary selection.
   ELSE.
      SELECT imak~posnr
             imak~posid
             imzo~objnr
             imzo~ippos
             imzo~gjahr
             impr~prnam AS p_prnam
             impr~gjahr AS p_gjahr
             impr~posid AS p_posid
             impr~xaktb AS p_xaktb
             imzo~posnr AS p_posnr
             INTO CORRESPONDING FIELDS OF TABLE tab_appreq
             FROM imak INNER JOIN imzo ON imak~objnr = imzo~objnr
                       INNER JOIN impr ON imzo~posnr = impr~posnr
             WHERE   imak~posid  IN appreq.
   ENDIF.
*  Add texts.
   IF NOT tab_appreq[] IS INITIAL.
      SELECT * FROM imakt INTO TABLE tab_imakt
        FOR ALL ENTRIES IN tab_appreq
        WHERE posnr = tab_appreq-posnr
        AND   spras = sy-langu.
      SORT tab_imakt BY posnr.
      LOOP AT tab_appreq.
        READ TABLE tab_imakt
             WITH KEY posnr = tab_appreq-posnr
             BINARY SEARCH.
        IF sy-subrc = 0.
           tab_appreq-txt50 = tab_imakt-txt50.
           MODIFY tab_appreq TRANSPORTING txt50.
        ENDIF.
      ENDLOOP.
   ENDIF.
ENDIF.

* Determine all objects to be processed.
LOOP AT tab_order.
  APPEND tab_order-objnr TO tab_obj.
ENDLOOP.
LOOP AT tab_wbsel.
  APPEND tab_wbsel-objnr TO tab_obj.
ENDLOOP.
LOOP AT tab_appreq.
  APPEND tab_appreq-objnr TO tab_obj.
ENDLOOP.
SORT tab_obj BY objnr.
DELETE ADJACENT DUPLICATES FROM tab_obj
  COMPARING objnr.
* Build Packages and process them.
WHILE NOT tab_obj[] IS INITIAL.
* Re-initialize package.
  REFRESH: tab_obj_pack.
  FREE:    tab_obj_pack.
  LOOP AT tab_obj
    FROM 1 TO pack_size.
    APPEND tab_obj TO tab_obj_pack.
  ENDLOOP.
  DELETE tab_obj
    FROM 1 TO pack_size.
  PERFORM package_process
    TABLES tab_obj_pack.
ENDWHILE.

* Write log.
PERFORM log_write.

END-OF-SELECTION.

FORM tab_ipgsel_create.

* 1. Get all the leaf positions selected by
*    investment program PROGRAM/APP_YEAR and the
*    range POSITION of entry positions and store
*    them in T_IPGSEL_LEAF.
* 2. Get all measures/requests with original
*    assignment to one of the selected leaf positions
*    stored in T_IPGSEL_LEAF and store them in T_IPGSEL_MREQ.
* 3. Get all IMZO keys {POSNR,OBJNR} corresponding
*    to the measures/requests stored in T_IPGSEL_MREQ.

  DATA: l_parnr_initial LIKE impr-parnr VALUE IS INITIAL.
  DATA: l_logdl_initial LIKE imzo-logdl VALUE IS INITIAL.

  DATA: BEGIN OF t_ipgsel_top OCCURS 0,
          posnr LIKE impr-posnr,
        END   OF t_ipgsel_top.

  DATA: BEGIN OF t_ipgsel_leaf OCCURS 0,
          posnr LIKE impr-posnr,
        END   OF t_ipgsel_leaf.

  DATA: BEGIN OF t_ipgsel_mreq OCCURS 0,
          objnr LIKE imzo-objnr,
        END   OF t_ipgsel_mreq.

  DATA: t_impr     LIKE impr OCCURS 100 WITH HEADER LINE.
  DATA: t_impr_tmp LIKE impr OCCURS 100 WITH HEADER LINE.

* Get all entry positions of selected subtrees.
* 1. Selection by program definition and position IDs.
  IF NOT position[] IS INITIAL.
     SELECT posnr FROM impr
       INTO CORRESPONDING FIELDS OF TABLE t_ipgsel_top
       FOR ALL ENTRIES IN position
       WHERE prnam = program
       AND   gjahr = app_year
       AND   posid = position-low.
* 2. Selection by program definition only.
  ELSE.
     SELECT posnr FROM impr
       INTO CORRESPONDING FIELDS OF TABLE t_ipgsel_top
       WHERE prnam = program
       AND   gjahr = app_year
       AND   parnr = l_parnr_initial.
  ENDIF.

* Something to do?
  CHECK NOT t_ipgsel_top[] IS INITIAL.

* Get all positions of selected investment program.
  SELECT * FROM impr INTO TABLE t_impr
    WHERE prnam = program
    AND   gjahr = app_year.
* Get all positions of selected subtrees.
  LOOP AT t_ipgsel_top.
    REFRESH t_impr_tmp. FREE t_impr_tmp.
    t_impr_tmp[] = t_impr[].
    CALL FUNCTION 'AIPA_GET_SET_TO_ENTRY_ELEMENT'
      EXPORTING
        i_posnr       = t_ipgsel_top-posnr
      TABLES
        t_impr        = t_impr_tmp.
*    Investigate subtree positions.
     LOOP AT t_impr_tmp.
*      Is position a leaf?
       SELECT COUNT(*) FROM impr
         WHERE parnr = t_impr_tmp-posnr.
*      Yes, ...
       CHECK sy-dbcnt = 0.
*      ... then collect to the table of
*      selected leaf positions.
       APPEND t_impr_tmp-posnr TO t_ipgsel_leaf.
     ENDLOOP.
  ENDLOOP.

* Something to do?
  CHECK NOT t_ipgsel_leaf[] IS INITIAL.

  SORT t_ipgsel_leaf BY posnr.
  DELETE ADJACENT DUPLICATES FROM t_ipgsel_leaf
    COMPARING posnr.

* Get measures/requests with original assignment to
* the selected leaf positions.
  SELECT DISTINCT objnr FROM imzo
    INTO CORRESPONDING FIELDS OF TABLE t_ipgsel_mreq
    FOR ALL ENTRIES IN t_ipgsel_leaf
    WHERE         posnr =  t_ipgsel_leaf-posnr
    AND           xgenj =  '0'
    AND   (       logdl IS null OR
            ( NOT logdl IS null            AND
                  logdl =  l_logdl_initial )   ).

* Something to do?
  CHECK NOT t_ipgsel_mreq[] IS INITIAL.

  SORT t_ipgsel_mreq BY objnr.
  DELETE ADJACENT DUPLICATES FROM t_ipgsel_mreq
    COMPARING objnr.

* Get all corresponding IMZO keys.
  SELECT DISTINCT posnr objnr FROM imzo
    INTO CORRESPONDING FIELDS OF TABLE tab_ipgsel
    FOR ALL ENTRIES IN t_ipgsel_mreq
    WHERE objnr = t_ipgsel_mreq-objnr.

  SORT tab_ipgsel BY posnr objnr.
  DELETE ADJACENT DUPLICATES FROM tab_ipgsel
    COMPARING posnr objnr.

ENDFORM.


FORM restriction_create.

* Only allow single values within
* select option for program positions.
* For details see function module
* AIPP_GET_SELOP_RESTRICTIONS.

  DATA: l_opt_list TYPE sscr_opt_list,
        l_ass      TYPE sscr_ass.

  CLEAR l_opt_list.
  l_opt_list-name       = 'NOINTERVLS'.
  l_opt_list-options-eq = 'X'.
  APPEND l_opt_list TO restriction-opt_list_tab.

  CLEAR l_ass.
  l_ass-kind    = 'S'.
  l_ass-name    = 'POSITION'.
  l_ass-sg_main = 'I'.
  l_ass-sg_addy = ' '.
  l_ass-op_main = 'NOINTERVLS'.
  l_ass-op_addy = 'NOINTERVLS'.
  APPEND l_ass TO restriction-ass_tab.

ENDFORM.


FORM package_process
  TABLES it_obj STRUCTURE raip_obj.

  DATA: l_obart      LIKE imps-obart,
        l_posnr_iq   LIKE imak-posnr,
        l_objnr      LIKE impr-objnr,
        l_subrc      LIKE sy-subrc,
        l_tabix      LIKE sy-tabix.

  DATA: t_bpge       LIKE bpge     OCCURS 0 WITH HEADER LINE,
        t_bpja       LIKE bpja     OCCURS 0 WITH HEADER LINE,
        t_bpge_upd   LIKE bpge     OCCURS 0 WITH HEADER LINE,
        t_bpja_upd   LIKE bpja     OCCURS 0 WITH HEADER LINE,
        t_obj_ro     LIKE raip_obj OCCURS 0 WITH HEADER LINE,
        t_obj_47     LIKE raip_obj OCCURS 0 WITH HEADER LINE,
        t_imzo_del   LIKE imzo     OCCURS 0 WITH HEADER LINE.

  DATA: BEGIN OF t_req OCCURS 0,
          posnr_iq   LIKE imak-posnr,
        END   OF t_req.

  DATA: BEGIN OF t_prot OCCURS 10,
          file(10)   TYPE c,
          read       LIKE sy-tabix,
          delete     LIKE sy-tabix,
        END   OF t_prot.

* Sort for binary search.
  SORT tab_order  BY objnr p_xaktb.
  SORT tab_wbsel  BY objnr p_xaktb.
  SORT tab_appreq BY objnr p_xaktb.

* Prepare deletion of IMZO-records.
* Get object numbers to package which may
* have 47-budget.
  LOOP AT it_obj.
*   Determine object type.
    CALL FUNCTION 'OBJECT_NUMBER_TYPE_GET'
      EXPORTING
        objnr   = it_obj-objnr
      IMPORTING
        obart   = l_obart.
*   Get IMZO records to be deleted.
    CASE l_obart.
    WHEN objektart_or.
      READ TABLE tab_order
           WITH KEY objnr = it_obj-objnr
           BINARY SEARCH.
      l_subrc = sy-subrc.
      l_tabix = sy-tabix.
      IF l_subrc = 0.
         LOOP AT tab_order
           FROM l_tabix.
           IF tab_order-objnr <> it_obj-objnr.
              EXIT.
           ENDIF.
           CLEAR t_imzo_del.
           t_imzo_del-mandt = sy-mandt.
           t_imzo_del-posnr = tab_order-p_posnr.
           t_imzo_del-objnr = tab_order-objnr.
           t_imzo_del-ippos = tab_order-ippos.
           t_imzo_del-gjahr = tab_order-gjahr.
           APPEND t_imzo_del.
         ENDLOOP.
      ENDIF.
    WHEN objektart_pr.
      READ TABLE tab_wbsel
           WITH KEY objnr = it_obj-objnr
           BINARY SEARCH.
      l_subrc = sy-subrc.
      l_tabix = sy-tabix.
      IF l_subrc = 0.
         LOOP AT tab_wbsel
           FROM l_tabix.
           IF tab_wbsel-objnr <> it_obj-objnr.
              EXIT.
           ENDIF.
           CLEAR t_imzo_del.
           t_imzo_del-mandt = sy-mandt.
           t_imzo_del-posnr = tab_wbsel-p_posnr.
           t_imzo_del-objnr = tab_wbsel-objnr.
           t_imzo_del-ippos = tab_wbsel-ippos.
           t_imzo_del-gjahr = tab_wbsel-gjahr.
           APPEND t_imzo_del.
         ENDLOOP.
      ENDIF.
    WHEN objektart_iq.
      READ TABLE tab_appreq
           WITH KEY objnr = it_obj-objnr
           BINARY SEARCH.
      l_subrc = sy-subrc.
      l_tabix = sy-tabix.
      IF l_subrc = 0.
         LOOP AT tab_appreq
           FROM l_tabix.
           IF tab_appreq-objnr <> it_obj-objnr.
              EXIT.
           ENDIF.
           CLEAR t_imzo_del.
           t_imzo_del-mandt = sy-mandt.
           t_imzo_del-posnr = tab_appreq-p_posnr.
           t_imzo_del-objnr = tab_appreq-objnr.
           t_imzo_del-ippos = tab_appreq-ippos.
           t_imzo_del-gjahr = tab_appreq-gjahr.
           APPEND t_imzo_del.
         ENDLOOP.
      ENDIF.
    ENDCASE.
*   Consider object for deleting 47 records
*   only if connected to at least one program
*   position with budget distribution.
    CASE l_obart.
    WHEN objektart_or.
      READ TABLE tab_order
           WITH KEY objnr   = it_obj-objnr
                    p_xaktb = on
           BINARY SEARCH.
      CHECK sy-subrc = 0.
    WHEN objektart_pr.
      READ TABLE tab_wbsel
           WITH KEY objnr   = it_obj-objnr
                    p_xaktb = on
           BINARY SEARCH.
      CHECK sy-subrc = 0.
    WHEN objektart_iq.
      READ TABLE tab_appreq
           WITH KEY objnr   = it_obj-objnr
                    p_xaktb = on
           BINARY SEARCH.
      CHECK sy-subrc = 0.
    ENDCASE.
*   Appropriation request.
    IF l_obart = objektart_iq.
*      Store internal number.
       CALL FUNCTION 'OBJECT_KEY_GET_IQ'
         EXPORTING
           objnr     = it_obj-objnr
         IMPORTING
           ima_posnr = l_posnr_iq.
       APPEND l_posnr_iq TO t_req.
*   Order/wbs element.
    ELSE.
*      Order/WBS element may have 47-budget.
       APPEND it_obj-objnr TO t_obj_47.
    ENDIF.
  ENDLOOP.
* Determine ROs to requests:
* These may have 47-budget.
  IF NOT t_req[] IS INITIAL.
     SELECT objnr FROM imakz
       INTO CORRESPONDING FIELDS OF TABLE t_obj_ro
       FOR ALL ENTRIES IN t_req
       WHERE posnr = t_req-posnr_iq.
     APPEND LINES OF t_obj_ro TO t_obj_47.
  ENDIF.

  SORT t_obj_47 BY objnr.
  DELETE ADJACENT DUPLICATES FROM t_obj_47
    COMPARING objnr.

* Read all 47 records to package from DB.
  IF NOT t_obj_47[] IS INITIAL.
     SELECT * FROM bpge INTO TABLE t_bpge
       FOR ALL ENTRIES IN t_obj_47
       WHERE objnr = t_obj_47-objnr
       AND   wrttp = l_wibudget.
     SELECT * FROM bpja INTO TABLE t_bpja
       FOR ALL ENTRIES IN t_obj_47
       WHERE objnr = t_obj_47-objnr
       AND   wrttp = l_wibudget.
*    For the 47 records of measures
*    adjust the distributed budget values of
*    the corresponding program positions.
     LOOP AT t_bpge.
       l_objnr = t_bpge-geber.
       CLEAR t_bpge-geber.
       t_bpge-objnr = l_objnr.
       t_bpge-wlgev = 0 - t_bpge-wlges.
       t_bpge-wtgev = 0 - t_bpge-wtges.
       CLEAR t_bpge-wlges.
       CLEAR t_bpge-wtges.
       t_bpge-beltp = '0'.
       CLEAR: t_bpge-kalnr,
              t_bpge-klvar.
       COLLECT t_bpge INTO t_bpge_upd.
     ENDLOOP.
*
     LOOP AT t_bpja.
       l_objnr = t_bpja-geber.
       CLEAR t_bpja-geber.
       t_bpja-objnr = l_objnr.
       t_bpja-wljhv = 0 - t_bpja-wljhr.
       t_bpja-wtjhv = 0 - t_bpja-wtjhr.
       CLEAR t_bpja-wljhr.
       CLEAR t_bpja-wtjhr.
       t_bpja-beltp = '0'.
       CLEAR: t_bpja-kalnr,
              t_bpja-klvar,
              t_bpja-spred.
       COLLECT t_bpja INTO t_bpja_upd.
     ENDLOOP.
  ENDIF.

* Production run.
  CHECK test_run = off.

* Delete IMZO records.
  IF NOT t_imzo_del IS INITIAL.
     DELETE imzo FROM TABLE t_imzo_del.
  ENDIF.

* Delete 47 records.
  IF NOT t_obj_47[] IS INITIAL.
     CALL FUNCTION 'KBPD_DELETE_DATA'
       EXPORTING
         delete_mode    = 'X'   " Production run.
         no_commit      = 'X'   " We make COMMIT on our own.
         geber_all      = 'X'
         wrttp          = l_wibudget
       TABLES
         tab_objnr      = t_obj_47
         tab_prot       = t_prot.
  ENDIF.

* Update distributed budgets of program positions.
  LOOP AT t_bpge_upd.
    UPDATE bpge SET   wlgev = wlgev + t_bpge_upd-wlgev
                      wtgev = wtgev + t_bpge_upd-wtgev
                WHERE lednr = t_bpge_upd-lednr
                AND   objnr = t_bpge_upd-objnr
                AND   posit = t_bpge_upd-posit
                AND   trgkz = t_bpge_upd-trgkz
                AND   wrttp = t_bpge_upd-wrttp
                AND   geber = t_bpge_upd-geber
                AND   versn = t_bpge_upd-versn
                AND   vorga = t_bpge_upd-vorga
                AND   twaer = t_bpge_upd-twaer.
  ENDLOOP.
  LOOP AT t_bpja_upd.
    UPDATE bpja SET   wljhv = wljhv + t_bpja_upd-wljhv
                      wtjhv = wtjhv + t_bpja_upd-wtjhv
                WHERE lednr = t_bpja_upd-lednr
                AND   objnr = t_bpja_upd-objnr
                AND   posit = t_bpja_upd-posit
                AND   trgkz = t_bpja_upd-trgkz
                AND   wrttp = t_bpja_upd-wrttp
                AND   gjahr = t_bpja_upd-gjahr
                AND   geber = t_bpja_upd-geber
                AND   versn = t_bpja_upd-versn
                AND   vorga = t_bpja_upd-vorga
                AND   twaer = t_bpja_upd-twaer.
  ENDLOOP.

ENDFORM.


FORM initial_screen_labels_set.

  LOOP AT SCREEN.
    IF screen-name   CS '%_TEST_RUN' AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Test run'.
    ENDIF.
    IF screen-name   CS '%_X_ORDER'  AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Select orders'.
    ENDIF.
    IF screen-name   CS '%_ORDER' AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Orders'.
    ENDIF.
    IF screen-name   CS '%_X_PRO'    AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Select projects'.
    ENDIF.
    IF screen-name   CS '%_PRO' AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Projects'.
    ENDIF.
    IF screen-name   CS '%_X_APPREQ'  AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Select appropriation requests'.
    ENDIF.
    IF screen-name   CS '%_APPREQ' AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Appropriation requests'.
    ENDIF.
    IF screen-name   CS '%_PROGRAM'  AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Programm definition'.
    ENDIF.
    IF screen-name   CS '%_APP_YEAR' AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Approval year'.
    ENDIF.
    IF screen-name   CS '%_POSITION' AND
       screen-group3 =  'TXT'        .
       ASSIGN (screen-name) TO <label>.
       <label> = 'Subtrees to entry positions'.
    ENDIF.
  ENDLOOP.

ENDFORM.


FORM log_write.

* Header.
  WRITE : /(86)
    'Detaching measures / appropriation requests from investment program
 positions'
    COLOR COL_GROUP INTENSIFIED ON.
  IF test_run = space.
     WRITE : /(86)
       '===> Production run'
       COLOR COL_GROUP INTENSIFIED OFF.
  ELSE.
     WRITE : /(86)
       '===> Test run'
       COLOR COL_GROUP INTENSIFIED OFF.
  ENDIF.
  ULINE.

* Log detached orders.
  IF NOT tab_order[] IS INITIAL.
*    Sort for having a nicer output.
     SORT tab_order BY aufnr p_prnam p_gjahr p_posid.
*    In case of using budget categories:
*    Only one log entry per position.
     DELETE ADJACENT DUPLICATES FROM tab_order
       COMPARING aufnr p_prnam p_gjahr p_posid.
*    Sub header: orders.
     WRITE : /(86)
       'Orders detached from investment program positions'
       COLOR COL_TOTAL INTENSIFIED ON.
     ULINE.
     LOOP AT tab_order.
       WRITE: /(24) tab_order-aufnr COLOR COL_POSITIVE INTENSIFIED ON,
               (22) 'has been detached from' COLOR COL_NORMAL,
               (08) tab_order-p_prnam NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
               (1)  '/' NO-GAP,
               (04) tab_order-p_gjahr NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
               (1)  '-' NO-GAP,
               (24) tab_order-p_posid COLOR COL_KEY INTENSIFIED OFF.
     ENDLOOP.
     ULINE.
  ENDIF.

* Log detached WBS-elements.
  IF NOT tab_wbsel[] IS INITIAL.
*    Sort for having a nicer output.
     SORT tab_wbsel BY posid p_prnam p_gjahr p_posid.
*    In case of using budget categories:
*    Only one log entry per position.
     DELETE ADJACENT DUPLICATES FROM tab_wbsel
       COMPARING posid p_prnam p_gjahr p_posid.
*    Sub header: WBS-elements.
     WRITE : /(86)
       'WBS-elements detached from investment program positions'
       COLOR COL_TOTAL INTENSIFIED ON.
     ULINE.
     LOOP AT tab_wbsel.
       WRITE: /(24) tab_wbsel-posid COLOR COL_POSITIVE INTENSIFIED ON,
               (22) 'has been detached from' COLOR COL_NORMAL,
               (08) tab_wbsel-p_prnam NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
               (1)  '/' NO-GAP,
               (04) tab_wbsel-p_gjahr NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
               (1)  '-' NO-GAP,
               (24) tab_wbsel-p_posid COLOR COL_KEY INTENSIFIED OFF.
     ENDLOOP.
     ULINE.
  ENDIF.

* Log detached appropriation requests.
  IF NOT tab_appreq[] IS INITIAL.
*    Sort for having a nicer output.
     SORT tab_appreq BY posid p_prnam p_gjahr p_posid.
*    In case of using budget categories:
*    Only one log entry per position.
     DELETE ADJACENT DUPLICATES FROM tab_appreq
       COMPARING posid p_prnam p_gjahr p_posid.
*    Sub header: appropriation requests.
     WRITE : /(86)
     'Appropriation requests detached from investment program positions'
       COLOR COL_TOTAL INTENSIFIED ON.
     ULINE.
     LOOP AT tab_appreq.
       WRITE: /(24) tab_appreq-posid COLOR COL_POSITIVE INTENSIFIED ON,
               (22) 'has been detached from' COLOR COL_NORMAL,
               (08) tab_appreq-p_prnam NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
               (1)  '/' NO-GAP,
               (04) tab_appreq-p_gjahr NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
               (1)  '-' NO-GAP,
               (24) tab_appreq-p_posid COLOR COL_KEY INTENSIFIED OFF.
     ENDLOOP.
     ULINE.
  ENDIF.

ENDFORM.
*>>>> END OF INSERTION <<<<<<
