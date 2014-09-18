***********************************************************************
* Program Name      : ZEMMPM_CREATE_MATL_IN_PLAN_21D
* Author            : Bae,
* Creation Date     : 2004.03.25.
* Specifications By :
* Pattern           : Report 1-1
* Development Request No :
* Addl Documentation:
* Description       : Create the procution material requirement (866)
*
* Modification Logs
* Date            Developer        RequestNo      Description
* 2005.06.28      Chris Li         UD1K916695    Using plant P001 + E001
* 2005.08.17      Furong                         to improve performance
*                                                get the res # from
*                                                plaf table then search
*                                                resb by res.#
* 03/22/2006      Furong                         Add a new field for
*                                                pass due requirement
************************************************************************


REPORT zemmpm_create_matl_in_plan_21d MESSAGE-ID zmmm .

TABLES: resb.

*---// Internal tables
DATA: it_21day LIKE ztmm_parts_21day OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF it_resb OCCURS 0,
        matnr   LIKE   resb-matnr,
        werks   LIKE   resb-werks,
        bdter   LIKE   resb-bdter,
        lgort   LIKE   resb-lgort,
        meins   LIKE   resb-meins,
        rsnum   LIKE   resb-rsnum,
        bdmng   LIKE   resb-bdmng,
        sortf   LIKE   resb-sortf,
        prvbe   LIKE   resb-prvbe,
      END   OF it_resb.

DATA: BEGIN OF it_bfd OCCURS 0,
        matnr   LIKE   resb-matnr,
        werks   LIKE   resb-werks,
        bdmng   LIKE   resb-bdmng,
*        enmng   LIKE   resb-enmng,
*        bal     LIKE   resb-enmng,
*        sortf   LIKE   resb-sortf,
*        prvbe   LIKE   resb-prvbe,
      END   OF it_bfd.

DATA: it_resb_21 LIKE TABLE OF it_resb WITH HEADER LINE.

DATA: BEGIN OF it_day OCCURS 21,
        seq     TYPE i,
        datum   LIKE   sy-datum,
      END   OF it_day.

DATA: BEGIN OF it_matnr OCCURS 0,
        matnr   LIKE   mara-matnr,
        werks   LIKE   marc-werks,
        lifnr   LIKE   lfa1-lifnr,
        profl   LIKE   mara-profl,
        tempb   LIKE   mara-tempb,
        raube   LIKE   mara-raube,
        dispo   LIKE   marc-dispo,
*        PRVBE   LIKE   RESB-PRVBE,
*        sortf   LIKE   resb-sortf,
      END   OF it_matnr.

*---// Work area
DATA: w_day_gap TYPE i.

*---// Ranges
RANGES: r_lgort   FOR   resb-lgort.

*---// For Listbox variable
TYPE-POOLS: vrm.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

*---> For body part qty calculation
DATA: BEGIN OF it_input OCCURS 0.
        INCLUDE STRUCTURE ztpp_input_plan.
DATA:   fsc  LIKE mara-matnr.
DATA: END OF it_input.
DATA:  wa_kalid   LIKE kako-kalid .
DATA: wa_lastdate  LIKE sy-datum.
*--->added by chris for body qty


*---// Selection screens
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE text-t01.
PARAMETERS : "p_werks LIKE t001w-werks   OBLIGATORY DEFAULT 'P001',
             p_lgort LIKE resb-lgort AS LISTBOX VISIBLE LENGTH 14,
             p_datum LIKE sy-datum      OBLIGATORY DEFAULT sy-datum.
*SELECT-OPTIONS : s_matnr FOR mara-matnr NO-EXTENSION.
SELECTION-SCREEN END OF BLOCK bl1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_listbox.
  PERFORM screen_modify.

AT SELECTION-SCREEN.
  CHECK sy-ucomm EQ 'ONLI'.
  PERFORM set_days.
  PERFORM check_rtn.
  PERFORM read_data.

START-OF-SELECTION.
  PERFORM set_21days_data.
  PERFORM update_table.

*   adding logic to change the body part qty by using
*   body input plan method. because plan now is scheduled
*   by trim input plan, but the body part requirement
*   should base on the body input plan. Trim part requirement
*   is later than body part requirements
  PERFORM change_body_part_qty.

*&---------------------------------------------------------------------*
*&      Form  screen_modify
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_modify.
*  LOOP AT SCREEN.
*    IF screen-name = 'P_WERKS'.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " screen_modify
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_data.
*  PERFORM read_resb.
*  perform read_resb_21times.
  PERFORM read_resb_plaf.
  PERFORM read_master.
  PERFORM read_resb_bfd.
ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_RESB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_resb.
  SELECT a~matnr a~werks a~bdter a~lgort a~meins
         a~rsnum a~sortf a~prvbe
         SUM( bdmng ) AS bdmng
    INTO CORRESPONDING FIELDS OF TABLE it_resb
    FROM resb AS a INNER JOIN mara AS b
                      ON a~matnr = b~matnr
*                    AND b~mtart = 'ROH'
   WHERE a~bdter >= p_datum
*     AND A~BDTER <= WA_LASTDATE
     AND a~lgort IN r_lgort
*     AND a~werks EQ p_werks "COMMENT BY CHRIS ON 06/28/2005
     AND ( b~mtart = 'ROH' OR
           b~mtart = 'HALB' )
     AND a~xloek EQ space
     AND a~kzear >= ' '
   GROUP by a~matnr a~werks a~bdter a~lgort a~meins
         a~rsnum a~sortf a~prvbe.

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
  SORT it_resb BY matnr.
ENDFORM.                    " READ_RESB
*&---------------------------------------------------------------------*
*&      Form  set_listbox
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox.
  PERFORM set_listbox_lgort.
ENDFORM.                    " set_listbox
*&---------------------------------------------------------------------*
*&      Form  set_listbox_lgort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_listbox_lgort.
  CLEAR: name, value, list.

  name = 'P_LGORT'.

  MOVE: space       TO value-key,
        'All'       TO value-text.
  APPEND value TO list.

  MOVE: 'P500'      TO value-key,
        'JIS'       TO value-text.
  APPEND value TO list.

  MOVE: 'P400'      TO value-key,
        'JIT'       TO value-text.
  APPEND value TO list.

  CALL FUNCTION 'VRM_SET_VALUES'
       EXPORTING
            id     = name
            values = list.
ENDFORM.                    " set_listbox_lgort
*&---------------------------------------------------------------------*
*&      Form  check_rtn
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_rtn.
  IF p_lgort NE space.
    MOVE: 'I'     TO r_lgort-sign,
          'EQ'    TO r_lgort-option,
          p_lgort TO r_lgort-low.

    APPEND r_lgort.
  ENDIF.
ENDFORM.                    " check_rtn
*&---------------------------------------------------------------------*
*&      Form  read_master
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_master.

  LOOP AT it_resb.
    MOVE: it_resb-matnr TO it_matnr-matnr.
    COLLECT it_matnr.
  ENDLOOP.

  READ TABLE it_day INDEX 1.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  LOOP AT it_matnr.
    EXEC SQL.
      SELECT V.MATNR, w.werks, Y.LIFNR, V.PROFL, V.TEMPB, V.RAUBE,
             W.DISPO
        INTO :IT_MATNR
*        FROM MARA V, MARC W, ZVMM_TASK_LIST X, EORD Y
        FROM MARA V, MARC W, EORD Y
       WHERE V.MANDT    =  :SY-MANDT
*         AND V.MTART    =  'ROH'
         AND V.MATNR    =  :IT_MATNR-MATNR
         AND W.MANDT    =  V.MANDT
         AND W.MATNR    =  V.MATNR
         AND W.WERKS    =  'P001'
*         AND W.WERKS    =  :P_WERKS  "COMEMT BY CHRIS ON /6/28/2005
*         AND X.MANDT(+) =  W.MANDT
*         AND X.PLNNR(+) =  'RP'
*         AND X.USR00(+) =  W.VSPVB
         AND Y.MANDT(+) =  W.MANDT
         AND Y.WERKS(+) =  W.WERKS
         AND Y.MATNR(+) =  W.MATNR
         AND Y.VDATU(+) <= :P_DATUM
         AND Y.BDATU(+) >= :P_DATUM
    ENDEXEC.

    IF it_matnr-lifnr EQ space.
      PERFORM read_vendor_from_sa.
    ENDIF.

    IF it_matnr-lifnr EQ space.
      PERFORM read_vendor_from_info.
    ENDIF.

    MODIFY: it_matnr.
  ENDLOOP.

  SORT it_matnr BY matnr.
ENDFORM.                    " read_master
*&---------------------------------------------------------------------*
*&      Form  read_vendor_from_sa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vendor_from_sa.
  SELECT SINGLE lifnr INTO it_matnr-lifnr
    FROM ekko AS a INNER JOIN ekpo AS b
                      ON a~mandt EQ b~mandt
                     AND a~ebeln EQ b~ebeln
   WHERE matnr   EQ it_matnr-matnr
*     AND werks   EQ p_werks "COMMENT BY CHRIS ON 06/28/2005
     AND a~loekz EQ space
     AND b~loekz EQ space
     AND kdatb   <= sy-datum
     AND kdate   >= sy-datum.
ENDFORM.                    " read_vendor_from_sa
*&---------------------------------------------------------------------*
*&      Form  read_vendor_from_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_vendor_from_info.
  DATA: lt_a018 LIKE a018 OCCURS 0 WITH HEADER LINE.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE lt_a018
    FROM a018
   WHERE kappl =  'M'
     AND kschl =  'PB00'
     AND matnr =  it_matnr-matnr
     AND ekorg =  'PU01'
     AND esokz =  '0'
     AND datab <= p_datum
     AND datbi >= p_datum.

  LOOP AT lt_a018.
    SELECT SINGLE a~matnr INTO it_matnr-matnr
      FROM eina AS a INNER JOIN eine AS b
        ON a~infnr = b~infnr
     WHERE a~matnr = it_matnr-matnr
       AND a~lifnr = lt_a018-lifnr
       AND a~loekz = ' '
       AND b~werks = ' '
       AND b~ekorg = 'PU01'
       AND b~loekz = ' '.
    IF sy-subrc NE 0.
      DELETE lt_a018.
    ENDIF.
  ENDLOOP.

  SORT lt_a018 BY datab DESCENDING.

  READ TABLE lt_a018 INDEX 1.

  MOVE: lt_a018-lifnr TO it_matnr-lifnr.
ENDFORM.                    " read_vendor_from_info
*&---------------------------------------------------------------------*
*&      Form  set_21days_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_21days_data.
  DATA: lw_index LIKE sy-tabix.
  DATA: l_matnr  LIKE mara-matnr.

  LOOP AT it_resb.
    CLEAR: it_21day.
    IF it_resb-bdter GT wa_lastdate.
      CONTINUE.
    ENDIF.
    READ TABLE it_day WITH KEY datum = it_resb-bdter
                      BINARY SEARCH.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    READ TABLE it_21day WITH KEY     "werks = p_werks COMMENT BY CHRIS
*                                      ON 06/28/2005
                                matnr = it_resb-matnr.

    IF sy-subrc EQ 0.
      MOVE: sy-tabix TO lw_index.

      PERFORM append_quantity.

      MODIFY it_21day INDEX lw_index.

    ELSE.
      PERFORM append_other_fields.
      PERFORM append_quantity.
      PERFORM append_passdue_qty.
      APPEND it_21day.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_21days_data
*&---------------------------------------------------------------------*
*&      Form  APPEND_QUANTITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_quantity.
  DATA: lw_quantity(50),
        lw_day(2) TYPE n,
        lw_bdmng  LIKE resb-bdmng.

  FIELD-SYMBOLS: <quantity>.

  READ TABLE it_day WITH KEY datum = it_resb-bdter
                    BINARY SEARCH.
  IF sy-subrc EQ 0.
    MOVE: it_resb-bdmng TO lw_bdmng.
  ELSE.
    MOVE: 0             TO lw_bdmng.
  ENDIF.

  lw_day = it_day-seq.

  CONCATENATE 'IT_21DAY-D' lw_day INTO lw_quantity.
  ASSIGN (lw_quantity) TO <quantity>.
  IF sy-subrc NE 0.
    EXIT.
  ENDIF.

  <quantity> = <quantity> + lw_bdmng.
ENDFORM.                    " APPEND_QUANTITY
*&---------------------------------------------------------------------*
*&      Form  APPEND_OTHER_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_other_fields.
  READ TABLE it_matnr WITH KEY matnr = it_resb-matnr
                      BINARY SEARCH.
  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m01.
  ENDIF.

  MOVE: it_resb-werks     TO it_21day-werks,
        it_matnr-matnr    TO it_21day-matnr,
        it_matnr-dispo    TO it_21day-dispo,
        p_datum           TO it_21day-datum,
        it_matnr-profl    TO it_21day-profl,
        it_resb-lgort     TO it_21day-lgort,
        it_matnr-lifnr    TO it_21day-lifnr,
*        it_RESB-PRVBE(1)  TO it_21day-shop,
        it_resb-prvbe     TO it_21day-prvbe,
        it_resb-sortf     TO it_21day-sortf,
        it_resb-meins     TO it_21day-meins,
        sy-uname          TO it_21day-ernam,
        sy-datum          TO it_21day-erdat,
        sy-uzeit          TO it_21day-erzet.
  IF     it_matnr-raube = '11'.
    it_21day-shop = 'B'.
  ELSEIF it_matnr-raube = '12'.
    it_21day-shop = 'P'.
  ELSEIF it_matnr-raube = '13'.
    it_21day-shop = 'T'.
  ELSEIF it_matnr-raube = '14'.
    it_21day-shop = 'E'.
  ENDIF.

ENDFORM.                    " APPEND_OTHER_FIELDS
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_table.
  DELETE FROM ztmm_parts_21day WHERE matnr NE space.

  INSERT ztmm_parts_21day FROM TABLE it_21day ACCEPTING DUPLICATE KEYS.
  IF sy-subrc EQ 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000(zz) WITH text-m03.
  ELSE.
    ROLLBACK WORK.
    MESSAGE e000(zz) WITH text-m04.
  ENDIF.
ENDFORM.                    " UPDATE_TABLE
*&---------------------------------------------------------------------*
*&      Form  change_body_part_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_body_part_qty.

*  reading the body input plan
  PERFORM read_body_plan.
*
*  calculate the body part qty for items with status '00'.
  PERFORM calculate_part_qty_00.

*  calculate the body part qty for items with status space
  PERFORM calculate_body_part_qty.

*  modidy body part qty.
  PERFORM modify_qty.

ENDFORM.                    " change_body_part_qty
*&---------------------------------------------------------------------*
*&      Form  READ_BODY_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_body_plan.

  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE it_input
   FROM ztpp_input_plan
   WHERE ( status = '00' OR
         status = space ) AND
         rd01 GE p_datum  AND
         rd01 LE wa_lastdate.
  IF sy-subrc NE 0.
    MESSAGE s009 WITH 'No input plan data for body part calculation'.
  ENDIF.

* reading body parts
  CLEAR: it_21day , it_21day[].
  SELECT werks matnr datum profl lgort lifnr shop
         prvbe sortf meins ernam erdat erzet
    INTO CORRESPONDING FIELDS OF TABLE it_21day
   FROM ztmm_parts_21day
   WHERE shop  = 'B'
     OR  sortf = '01'
     OR  sortf = '02'.

* reduce the it_resb
  LOOP AT it_resb.
    READ TABLE it_21day WITH KEY matnr = it_resb-matnr.
    IF sy-subrc NE 0.
      DELETE it_resb.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " READ_BODY_PLAN
*&---------------------------------------------------------------------*
*&      Form  calculate_body_part_qty
*&---------------------------------------------------------------------*
*   For blank status cars, there is no planned order yet, so we need
*   to find the same FSC code and find each component qty in FSC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_body_part_qty.
  DATA: l_fsc LIKE mara-matnr.
  DATA: wa_input LIKE it_input.
  DATA: l_tabix  LIKE sy-tabix.
  DATA: BEGIN OF lt_fsc OCCURS 0,
          fsc  LIKE mara-matnr,
          part LIKE mara-matnr,
          tdate LIKE sy-datum,
          rd01 LIKE sy-datum,
          qty  LIKE plaf-gsmng,
          pqty LIKE plaf-gsmng,
          ver  LIKE plaf-verid,
        END OF lt_fsc.
  DATA: BEGIN OF lt_plaf OCCURS 0,
          plnum  LIKE plaf-plnum,
          matnr  LIKE plaf-matnr,
          psttr  LIKE plaf-psttr,
          gsmng  LIKE plaf-gsmng,
          rsnum  LIKE plaf-rsnum,
          verid  LIKE plaf-verid,
        END OF lt_plaf.
  DATA: lt_fsc1 LIKE lt_fsc OCCURS 0 WITH HEADER LINE.
  DATA: lt_input LIKE it_input OCCURS 0 WITH HEADER LINE.
  DATA: wa_fsc LIKE lt_fsc.
  DATA: l_num(2) TYPE n.
  DATA: l_text(14).
  DATA: l_ver(2).
  DATA: l_version(3) TYPE n.
  DATA: l_date  LIKE sy-datum.
  DATA: l_line TYPE i.
  FIELD-SYMBOLS: <field>.
*
* get the shceduled items with status space.
  lt_input[] = it_input[].
  DELETE lt_input WHERE status = '00'.
  DESCRIBE TABLE lt_input LINES l_line .
  IF l_line EQ 0.
    EXIT.
  ENDIF.


* read the fsc code
  LOOP AT lt_input.
    SELECT SINGLE fsc INTO lt_input-fsc
      FROM ztpp_wosum
      WHERE wo_ser  = lt_input-work_order(9)
        AND nation  = lt_input-work_order+9(3)
        AND dealer  = lt_input-work_order+12(2)
        AND extc    = lt_input-extc
        AND intc    = lt_input-intc.
    MODIFY lt_input.
  ENDLOOP.

* summarize by fsc and planed schedule date
  SORT lt_input BY fsc vers rsnum.
  CLEAR: l_fsc.
  LOOP AT lt_input.
    l_tabix = sy-tabix + 1.
    lt_fsc-qty = lt_fsc-qty + 1.
    CLEAR: wa_input .
    READ TABLE lt_input INTO wa_input INDEX l_tabix.
    IF  lt_input-fsc  NE wa_input-fsc   OR
       lt_input-vers  NE wa_input-vers  OR
       lt_input-rd01 NE wa_input-rd01.
      lt_fsc-fsc   = lt_input-fsc.
      lt_fsc-tdate = lt_input-rsnum.
      lt_fsc-rd01  = lt_input-rd01.
      lt_fsc-ver   = lt_input-vers.
      APPEND lt_fsc.
      CLEAR: lt_fsc.
    ENDIF.
  ENDLOOP.
*
* read the planned order
  LOOP AT lt_fsc.
    l_ver = lt_fsc-ver+1(2).
    SELECT plnum matnr psttr gsmng rsnum verid
      APPENDING CORRESPONDING FIELDS OF TABLE lt_plaf
      FROM plaf
      WHERE matnr = lt_fsc-fsc
        AND rsnum NE '0'
        AND psttr GE p_datum
        AND verid = l_ver
        AND stlfx NE 'X'.
*        and psttr le wa_lastdate.
  ENDLOOP.
  SORT lt_plaf BY matnr psttr verid.
  DELETE ADJACENT DUPLICATES FROM lt_plaf
    COMPARING matnr psttr verid.
*
* reduce the it_resb table
  SORT lt_plaf BY rsnum.
  LOOP AT it_resb .
    READ TABLE lt_plaf WITH KEY rsnum = it_resb-rsnum
         BINARY SEARCH.
    IF sy-subrc NE 0.
      DELETE it_resb.
    ENDIF.
  ENDLOOP.
  SORT it_resb BY rsnum matnr.
  SORT lt_plaf BY matnr verid.

* get the base body part qty for each fsc
* total qty = (base qty of each fsc)  *  (fsc qty)
  LOOP AT lt_fsc.
    MOVE-CORRESPONDING lt_fsc TO lt_fsc1.
    CLEAR: lt_plaf.
    l_ver = lt_fsc-ver+1(2).
    READ TABLE lt_plaf WITH KEY matnr = lt_fsc-fsc
                                    verid = l_ver
          BINARY SEARCH .
    IF sy-subrc NE 0.   " no planned order
      CONTINUE.
    ENDIF.
    LOOP AT it_21day.

      CLEAR: it_resb.

      READ TABLE it_resb WITH KEY rsnum = lt_plaf-rsnum
                                  matnr = it_21day-matnr
           BINARY SEARCH.
      IF sy-subrc = 0.
*        MOVE-CORRESPONDING lt_fsc TO lt_fsc1.
        lt_fsc1-part = it_21day-matnr.
        lt_fsc1-pqty = it_resb-bdmng /
                       lt_plaf-gsmng *
                       lt_fsc-qty .
        APPEND lt_fsc1.
*         else.   "no part reservation
*           move-corResponding lt_fsc to lt_fsc1.
*           lt_fsc1-part = it_21day-matnr.
*           lt_fsc1-pqty = 0.
*           append lt_fsc1.
      ENDIF.
*       else.     "no plan order
*         move-corResponding lt_fsc to lt_fsc1.
*         lt_fsc1-part = it_21day-matnr.
*         lt_fsc1-pqty = 0.
*         append lt_fsc1.
*       endif.
    ENDLOOP.
  ENDLOOP.

* sumarize the date-part qty
  SORT lt_fsc1 BY part rd01.
  CLEAR: lt_fsc, lt_fsc[].

  LOOP AT lt_fsc1.
    l_tabix = sy-tabix + 1.
    lt_fsc-part = lt_fsc1-part.
    lt_fsc-rd01 = lt_fsc1-rd01.
    lt_fsc-pqty = lt_fsc-pqty + lt_fsc1-pqty.
    CLEAR: wa_fsc.
    READ TABLE lt_fsc1 INTO wa_fsc INDEX l_tabix.
    IF lt_fsc1-part NE wa_fsc-part OR
       lt_fsc1-rd01 NE wa_fsc-rd01.
      APPEND lt_fsc.
      CLEAR: lt_fsc.
    ENDIF.
  ENDLOOP.

* TRANSFER the part qty TO internal table
  LOOP AT it_21day.
    LOOP AT it_day.
      IF sy-tabix GT 21.
        EXIT.
      ENDIF.
      l_num = it_day-seq.
      CONCATENATE 'IT_21DAY-D' l_num INTO l_text.
      ASSIGN (l_text) TO <field>.
      CLEAR: lt_fsc.
      READ TABLE lt_fsc WITH KEY part = it_21day-matnr
                                 rd01 = it_day-datum.
      IF sy-subrc = 0 .
        <field> = <field> + lt_fsc-pqty.
      ELSE.
*        MOVE 0           TO <FIELD>.
      ENDIF.
    ENDLOOP.
    MODIFY it_21day.
    CLEAR: it_21day.
  ENDLOOP.

ENDFORM.                    " calculate_body_part_qty
*&---------------------------------------------------------------------*
*&      Form  modify_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_qty.
  CHECK NOT it_21day[] IS INITIAL.
  UPDATE ztmm_parts_21day FROM TABLE it_21day.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    MESSAGE s000 WITH 'Body part qty updated successfully'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE s000 WITH 'Body part qty update failed'.
  ENDIF.
ENDFORM.                    " modify_qty
*&---------------------------------------------------------------------*
*&      Form  set_DAYS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_days.
  DATA: l_count TYPE i.
  DATA: l_date LIKE sy-datum.

* reading working calendar
  PERFORM read_shop_calid  USING wa_kalid.
* first is current inputed date
  it_day-seq = 1.
  it_day-datum = p_datum.
  APPEND it_day.
  l_count = 1.
  l_date = p_datum .
  DO 20 TIMES.
    l_count  = l_count + 1.
    l_date   = l_date  + 1.
    PERFORM read_working_date USING '+'  wa_kalid  l_date.
    it_day-seq     = l_count.
    it_day-datum   = l_date .
    APPEND it_day.  CLEAR: it_day.
  ENDDO.
  wa_lastdate = l_date .
ENDFORM.                    " set_DAYS
*&---------------------------------------------------------------------*
*&      Form  READ_SHOP_CALID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_KALID  text
*----------------------------------------------------------------------*
FORM read_shop_calid USING    pa_kalid.
  SELECT SINGLE kalid INTO pa_kalid
    FROM zvpp_capacity
   WHERE arbpl = 'T'   .
ENDFORM.                    " READ_SHOP_CALID
*&---------------------------------------------------------------------*
*&      Form  READ_WORKING_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_WDATE  text
*----------------------------------------------------------------------*
FORM read_working_date USING  pa_type  pa_kalid  pa_wdate.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
       EXPORTING
            correct_option               = pa_type
            date                         = pa_wdate
            factory_calendar_id          = pa_kalid
       IMPORTING
            date                         = pa_wdate
       EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
ENDFORM.                    " READ_WORKING_DATE
*&---------------------------------------------------------------------*
*&      Form  calculate_part_qty_00
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calculate_part_qty_00.
  DATA: lt_input LIKE it_input OCCURS 0 WITH HEADER LINE.
  DATA: BEGIN OF lt_rsnum OCCURS 0,
         plnum LIKE plaf-plnum,
         rsnum LIKE resb-rsnum,
        END OF lt_rsnum.
  DATA: lt_resb  LIKE it_resb OCCURS 0 WITH HEADER LINE.
  DATA: lt_resb1 LIKE it_resb OCCURS 0 WITH HEADER LINE.
  DATA: wa_resb  LIKE it_resb.
  DATA: l_tabix  LIKE sy-tabix.
  DATA: l_num(2) TYPE n.
  DATA: l_text(15).
  DATA: l_line TYPE i.
  FIELD-SYMBOLS: <field>.

* get the items with status '00'.
  lt_input[] = it_input[].
  DELETE lt_input WHERE status = space.
  DESCRIBE TABLE lt_input LINES l_line .
  IF l_line EQ 0.
    EXIT.
  ENDIF.

* read the reservation number from plaf
  SELECT plnum rsnum  INTO TABLE lt_rsnum
    FROM plaf
    FOR ALL ENTRIES IN lt_input
    WHERE plnum = lt_input-plnum.

* get body parts with the correct reservation number
  LOOP AT it_resb.
    CLEAR: lt_rsnum.
    READ TABLE lt_rsnum WITH KEY rsnum = it_resb-rsnum.
    IF sy-subrc EQ 0.
      "**get the body input plan date
      READ TABLE lt_input WITH KEY plnum = lt_rsnum-plnum.
      it_resb-bdter = lt_input-rd01.
      APPEND it_resb TO lt_resb.
    ENDIF.
  ENDLOOP.

  SORT lt_resb BY matnr bdter.
*
* summarize the qty by part and date
  LOOP AT lt_resb.
    l_tabix = sy-tabix + 1.
    lt_resb1-bdmng = lt_resb1-bdmng + lt_resb-bdmng.
    CLEAR: wa_resb.
    READ TABLE lt_resb INTO wa_resb INDEX l_tabix.
    IF lt_resb-matnr NE wa_resb-matnr OR
       lt_resb-bdter NE wa_resb-bdter.
      lt_resb1-matnr = lt_resb-matnr.
      lt_resb1-bdter = lt_resb-bdter.
      APPEND lt_resb1.
      CLEAR: lt_resb1.
    ENDIF.
  ENDLOOP.

* tranfer the qty to internal table it_21day.
  LOOP AT it_21day.
    LOOP AT it_day.
      IF sy-tabix GT 21.
        EXIT.
      ENDIF.
      l_num = it_day-seq.
      CONCATENATE 'IT_21DAY-D' l_num INTO l_text.
      ASSIGN (l_text) TO <field>.
      CLEAR: lt_resb1.
      READ TABLE lt_resb1 WITH KEY matnr = it_21day-matnr
                                   bdter = it_day-datum.
      IF sy-subrc = 0 .
        <field> = <field> + lt_resb1-bdmng.
      ELSE.
*        MOVE 0           TO <FIELD>.
      ENDIF.
    ENDLOOP.
    MODIFY it_21day.
    CLEAR: it_21day.
  ENDLOOP.


ENDFORM.                    " calculate_part_qty_00
*&---------------------------------------------------------------------*
*&      Form  read_resb_21times
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_resb_21times.
  DATA: lw_date LIKE sy-datum.

  LOOP AT it_day.
    CLEAR: it_resb_21, it_resb_21[].
    lw_date = it_day-datum.
    SELECT a~matnr a~werks a~bdter a~lgort a~meins
           a~rsnum a~sortf a~prvbe
           SUM( bdmng ) AS bdmng
      INTO CORRESPONDING FIELDS OF TABLE it_resb_21
      FROM resb AS a INNER JOIN mara AS b
                        ON a~matnr = b~matnr
*                    AND b~mtart = 'ROH'
     WHERE a~bdter = lw_date
*     AND A~BDTER <= WA_LASTDATE
       AND a~lgort IN r_lgort
*     AND a~werks EQ p_werks "COMMENT BY CHRIS ON 06/28/2005
       AND ( b~mtart = 'ROH' OR
             b~mtart = 'HALB' )
       AND a~xloek EQ space
       AND a~kzear >= ' '
     GROUP by a~matnr a~werks a~bdter a~lgort a~meins
           a~rsnum a~sortf a~prvbe.

    IF NOT it_resb_21[] IS INITIAL.
      APPEND LINES OF it_resb_21 TO it_resb.
    ENDIF.

  ENDLOOP.
  IF it_resb[] IS INITIAL.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.

  SORT it_resb BY matnr.

ENDFORM.                    " read_resb_21times
*&---------------------------------------------------------------------*
*&      Form  read_resb_plaf
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_resb_plaf.
  SELECT a~matnr a~werks a~bdter a~lgort a~meins
         a~rsnum a~sortf a~prvbe
         SUM( a~bdmng ) AS bdmng
    INTO CORRESPONDING FIELDS OF TABLE it_resb
    FROM plaf AS c
    INNER JOIN resb AS a
    ON c~rsnum = a~rsnum
    INNER JOIN mara AS b
    ON a~matnr = b~matnr
   WHERE c~dispo IN ('V01', 'ME1')
     and c~psttr >= p_datum
     AND a~lgort IN r_lgort
     AND c~plscn = ' '
     AND ( b~mtart = 'ROH' OR
           b~mtart = 'HALB' )
     AND a~xloek EQ space
     AND a~kzear >= ' '
   GROUP by a~matnr a~werks a~bdter a~lgort a~meins
         a~rsnum a~sortf a~prvbe.

  IF sy-subrc NE 0.
    MESSAGE e000(zz) WITH text-m02.
  ENDIF.
  SORT it_resb BY matnr.
ENDFORM.                    " read_resb_plaf
*&---------------------------------------------------------------------*
*&      Form  append_passdue_qty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_passdue_qty.
  READ TABLE it_bfd WITH KEY matnr = it_resb-matnr
                             werks = it_resb-werks.
  IF sy-subrc = 0.
    it_21day-bfd = it_bfd-bdmng.
  ENDIF.
ENDFORM.                    " append_passdue_qty
*&---------------------------------------------------------------------*
*&      Form  read_resb_bfd
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_resb_bfd.
  DATA: lt_bfd LIKE TABLE OF it_bfd WITH HEADER LINE.

  SELECT a~matnr a~werks
         SUM( a~bdmng ) AS bdmng
    INTO TABLE Lt_BFD
    FROM plaf AS c
    INNER JOIN resb AS a
    ON c~rsnum = a~rsnum
    INNER JOIN mara AS b
    ON a~matnr = b~matnr
   WHERE c~dispo IN ('V01', 'ME1')
     AND c~psttr < p_datum
     AND a~lgort IN r_lgort
     AND c~plscn = ' '
     AND ( b~mtart = 'ROH' OR
           b~mtart = 'HALB' )
     AND a~enmng = 0
     AND a~xloek EQ space
     AND a~kzear >= ' '
   GROUP by a~matnr a~werks a~bdter a~lgort a~meins
         a~rsnum a~sortf a~prvbe.

*  SELECT matnr werks bdmng INTO TABLE lt_bfd
*    FROM resb
*    for all entries in it_matnr
*  WHERE matnr = it_matnr-matnr
*    and bdter < p_datum
*    AND enmng = 0
*    AND xloek EQ space.
*
*  SELECT b~matnr b~werks b~bdmng
*    INTO CORRESPONDING FIELDS OF TABLE lt_bfd
*    FROM ztpp_input_plan AS a
*    INNER JOIN resb AS b
*    ON a~plnum = b~plnum
*     where a~status between '06' and '17'
*     AND b~xloek EQ space
*     and b~enmng = 0.

  LOOP AT lt_bfd.
    it_bfd = lt_bfd.
*    it_bfd-bal = it_bfd-bdmng - it_bfd-enmng.
    COLLECT it_bfd.
    CLEAR: it_bfd, lt_bfd.
  ENDLOOP.

ENDFORM.                    " read_resb_bfd
