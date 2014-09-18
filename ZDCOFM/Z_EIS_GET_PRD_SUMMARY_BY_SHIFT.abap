FUNCTION z_eis_get_prd_summary_by_shift.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(CHECK_DATE) TYPE  SY-DATUM DEFAULT SY-DATUM
*"  TABLES
*"      ZPRDQTYSHIFT STRUCTURE  ZPRDQTYSHIFT
*"----------------------------------------------------------------------

  DATA: BEGIN OF lt_vm OCCURS 0,
          objek  LIKE ausp-objek,
          worder LIKE ausp-atwrt,
          adate  LIKE ausp-atwrt,
        END   OF lt_vm.

  DATA: l_worder     LIKE cabn-atinn,
        l_adate      LIKE cabn-atinn,
        l_rp07_sdate LIKE cabn-atinn,
        l_atflv      LIKE ausp-atflv,
        l_num(8)     TYPE n.

  CONSTANTS: c_worder     LIKE cabn-atnam VALUE 'P_WORK_ORDER',
             c_adate      LIKE cabn-atnam VALUE 'P_RP07_ACTUAL_DATE',
             c_rp07_sdate LIKE cabn-atnam VALUE 'P_RP07_SHOP_DATE'.

  DATA: l_check_date TYPE datum,
        l_check_ydate TYPE datum,
        l_bodyno     LIKE ausp-objek.

  CONSTANTS: l_1st_btime TYPE sy-uzeit  VALUE '050000',
             l_1st_etime TYPE sy-uzeit  VALUE '144459',
             l_2nd_btime TYPE sy-uzeit  VALUE '144500',
             l_2nd_etime TYPE sy-uzeit  VALUE '222959',
             l_3rd_btime TYPE sy-uzeit  VALUE '223000',
             l_3rd_setime TYPE sy-uzeit VALUE '235959',
             l_3rd_sbtime TYPE sy-uzeit VALUE '000000',
             l_3rd_etime TYPE sy-uzeit  VALUE '100000'.

  __cls  gt_zprdqtyshift.

  SELECT SINGLE atinn INTO l_worder
    FROM cabn WHERE atnam = c_worder.

  SELECT SINGLE atinn INTO l_adate
    FROM cabn WHERE atnam = c_adate.

  SELECT SINGLE atinn INTO l_rp07_sdate
    FROM cabn WHERE atnam = c_rp07_sdate.

  l_atflv = l_num = check_date.
  SELECT a~objek
         b~atwrt AS worder
         c~atwrt AS adate
    INTO CORRESPONDING FIELDS OF TABLE lt_vm
    FROM ausp AS a LEFT OUTER JOIN ausp AS b
                      ON b~objek = a~objek
                     AND b~atinn = l_worder
                     AND b~atzhl = a~atzhl
                     AND b~mafid = a~mafid
                     AND b~klart = a~klart
                     AND b~adzhl = a~adzhl
                   LEFT OUTER JOIN ausp AS c
                      ON c~objek = a~objek
                     AND c~atinn = l_adate
                     AND c~atzhl = a~atzhl
                     AND c~mafid = a~mafid
                     AND c~klart = a~klart
                     AND c~adzhl = a~adzhl
   WHERE a~klart EQ '002'
     AND a~atinn EQ l_rp07_sdate
     AND a~atflv EQ l_atflv.

  l_check_ydate = check_date.
  l_check_date = l_check_ydate + 1.

  LOOP AT lt_vm.
    IF lt_vm-worder+12(2) EQ 'XX' OR lt_vm-worder+12(2) EQ 'XY'.
      DELETE lt_vm. CONTINUE.
    ENDIF.

* to add YF to LF for EIS Trim input status
    IF lt_vm-objek(3) EQ 'INF'.
      lt_vm-objek(3) = 'C2F'.
*DELETE lt_vm. CONTINUE.
    ENDIF.
*
    IF ( lt_vm-adate(8) EQ l_check_ydate ) AND ( lt_vm-adate+8(6) BETWEEN l_1st_btime AND l_1st_etime ).
      zprdqtyshift-shift = '1st Shift'.
      zprdqtyshift-p_model = lt_vm-objek(3).
      zprdqtyshift-prdqty = 1.
      COLLECT zprdqtyshift.
      CLEAR zprdqtyshift.
      CONTINUE.
    ENDIF.

    IF ( lt_vm-adate(8) EQ l_check_ydate ) AND ( lt_vm-adate+8(6) BETWEEN l_2nd_btime AND l_2nd_etime ).
      zprdqtyshift-shift = '2nd Shift'.
      zprdqtyshift-p_model = lt_vm-objek(3).
      zprdqtyshift-prdqty = 1.
      COLLECT zprdqtyshift.
      CLEAR zprdqtyshift.
      CONTINUE.
    ENDIF.

    IF ( ( lt_vm-adate(8) EQ l_check_ydate ) AND ( lt_vm-adate+8(6) BETWEEN l_3rd_btime AND l_3rd_setime )
           OR ( ( lt_vm-adate(8) EQ l_check_date ) AND  ( lt_vm-adate+8(6) BETWEEN l_3rd_sbtime AND l_3rd_etime ) ) ).
      zprdqtyshift-shift = '3rd Shift'.
      zprdqtyshift-p_model = lt_vm-objek(3).
      zprdqtyshift-prdqty = 1.
      COLLECT zprdqtyshift.
      CLEAR zprdqtyshift.
      CONTINUE.
    ENDIF.

    IF ( lt_vm-adate(8) EQ l_check_date AND lt_vm-adate+8(6) > '064459' ).
      zprdqtyshift-shift = '3rd Shift'.
      zprdqtyshift-p_model = lt_vm-objek(3).
      zprdqtyshift-prdqty = 1.
      COLLECT zprdqtyshift.
      CLEAR zprdqtyshift.
      CONTINUE.
    ENDIF.
  ENDLOOP.



























*  DATA: lt_ztppvr TYPE TABLE OF ztppvr,
*        ls_ztppvr TYPE ztppvr.

*  CLEAR :  p_model, qty_01, qty_02.
*
*  CLEAR :  p_model,
*           qty_01,         qty_02,         qty_03,
*           qty_04,         qty_05,         qty_06,
*           qty_07,         qty_08,         qty_09,
*           qty_10,         qty_11,         qty_12,
*           qty_13,         qty_14,         qty_15,
*           qty_16,         qty_17,         qty_18,
*           qty_19,         qty_20,         qty_21,
*           qty_22,         qty_23,         qty_24.
*
*
*  SELECT * INTO TABLE lt_ztppvr
*    FROM ztppvr
*   WHERE k04pdat EQ check_date
*     AND p_status = 'T01'
*     AND zresult  = 'S'
*     AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' ).
*
*  SORT lt_ztppvr BY p_status.
*
*  l_check_date = l_check_date + 1.
*  LOOP AT lt_ztppvr INTO ls_ztppvr.
*    IF ls_ztppvr-p_rp_actual_time BETWEEN l_1st_btime AND l_1st_etime.
*      IF check_date EQ ls_ztppvr-p_rp_actual_date.
*        zprdqtyshift-shift = '1st Shift'.
*        zprdqtyshift-p_model = ls_ztppvr-p_model.
*        zprdqtyshift-prdqty = 1.
*        COLLECT zprdqtyshift.
*        CLEAR zprdqtyshift.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
*    IF ls_ztppvr-p_rp_actual_time BETWEEN l_2nd_btime AND l_2nd_etime.
*      zprdqtyshift-shift = '2nd Shift'.
*      zprdqtyshift-p_model = ls_ztppvr-p_model.
*      zprdqtyshift-prdqty = 1.
*      COLLECT zprdqtyshift.
*      CLEAR zprdqtyshift.
*      CONTINUE.
*    ENDIF.
*
*    IF ( ( ls_ztppvr-p_rp_actual_time BETWEEN l_3rd_btime AND l_3rd_setime )
*           OR ( ls_ztppvr-p_rp_actual_time BETWEEN l_3rd_sbtime AND l_3rd_etime ) ).
*      zprdqtyshift-shift = '3rd Shift'.
*      zprdqtyshift-p_model = ls_ztppvr-p_model.
*      zprdqtyshift-prdqty = 1.
*      COLLECT zprdqtyshift.
*      CLEAR zprdqtyshift.
*      CONTINUE.
*    ENDIF.
*
*    IF ( ls_ztppvr-p_rp_actual_date EQ l_check_date AND ls_ztppvr-p_rp_actual_time > '063000' ).
*      zprdqtyshift-shift = '3rd Shift'.
*      zprdqtyshift-p_model = ls_ztppvr-p_model.
*      zprdqtyshift-prdqty = 1.
*      COLLECT zprdqtyshift.
*      CLEAR zprdqtyshift.
*      CONTINUE.
*    ENDIF.
*
*  ENDLOOP.

  SORT zprdqtyshift BY shift ASCENDING
                       p_model DESCENDING.
  DATA $zprdqtyshift LIKE zprdqtyshift OCCURS 2 WITH HEADER LINE.

  LOOP AT zprdqtyshift.
    $zprdqtyshift = zprdqtyshift.
    CLEAR $zprdqtyshift-shift.
    COLLECT $zprdqtyshift.
  ENDLOOP.

  LOOP AT $zprdqtyshift.
    zprdqtyshift = $zprdqtyshift.
    zprdqtyshift-shift = 'Total'.
    APPEND zprdqtyshift.
    AT LAST.
      SUM.
      zprdqtyshift = $zprdqtyshift.
      zprdqtyshift-shift = 'Total'.
      zprdqtyshift-p_model = 'Z'.
      APPEND zprdqtyshift.
    ENDAT.
  ENDLOOP.

  DATA : $ix LIKE sy-tabix,
         $flag.

  __cls $zprdqtyshift.
  LOOP AT zprdqtyshift.
    CHECK zprdqtyshift-shift NE 'Total'.
    CHECK zprdqtyshift-shift NE space.
    AT END OF shift.
      SUM.
      $zprdqtyshift = zprdqtyshift.
      $zprdqtyshift-p_model = 'Z'.
      APPEND $zprdqtyshift.
    ENDAT.
  ENDLOOP.
  LOOP AT zprdqtyshift.
    CHECK zprdqtyshift-shift NE 'Total'.
    CHECK zprdqtyshift-shift NE space.
    IF $ix EQ sy-tabix.
      CONTINUE.
    ENDIF.

    $ix = sy-tabix.
    AT END OF shift.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    READ TABLE $zprdqtyshift WITH KEY shift = zprdqtyshift-shift.
    IF sy-subrc EQ 0.
      zprdqtyshift = $zprdqtyshift.
      ADD 1 TO $ix.
      INSERT zprdqtyshift INDEX $ix.
    ENDIF.
  ENDLOOP.

* by ig {
  DATA: it_modl_val TYPE ztbm_model_val_n OCCURS 10 WITH HEADER LINE.
  DATA $zvalue TYPE zvalue.

  SELECT *
       FROM ztbm_model_val_n
       INTO TABLE it_modl_val
       WHERE zfield EQ '01'.

  SORT it_modl_val BY zvalue.

  LOOP AT zprdqtyshift.
    $ix = sy-tabix.
    $zvalue = zprdqtyshift-p_model(2).
    READ TABLE it_modl_val WITH KEY zvalue = $zvalue BINARY SEARCH.
    IF sy-subrc EQ 0.
      zprdqtyshift-p_model = it_modl_val-zvalnm(2).
    ENDIF.
    MODIFY zprdqtyshift INDEX $ix.
  ENDLOOP.
* }

  READ TABLE zprdqtyshift WITH KEY shift = '1st Shift' p_model = 'UD'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '1st Shift'.
    zprdqtyshift-p_model = 'UD'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '1st Shift' p_model = 'LF'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '1st Shift'.
    zprdqtyshift-p_model = 'LF'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '1st Shift' p_model = 'Z'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '1st Shift'.
    zprdqtyshift-p_model = 'Z'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '2nd Shift' p_model = 'UD'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '2nd Shift'.
    zprdqtyshift-p_model = 'UD'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '2nd Shift' p_model = 'LF'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '2nd Shift'.
    zprdqtyshift-p_model = 'LF'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '2nd Shift' p_model = 'Z'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '2nd Shift'.
    zprdqtyshift-p_model = 'Z'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '3rd Shift' p_model = 'UD'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '3rd Shift'.
    zprdqtyshift-p_model = 'UD'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '3rd Shift' p_model = 'LF'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '3rd Shift'.
    zprdqtyshift-p_model = 'LF'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = '3rd Shift' p_model = 'Z'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = '3rd Shift'.
    zprdqtyshift-p_model = 'Z'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = 'Total' p_model = 'UD'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = 'Total'.
    zprdqtyshift-p_model = 'UD'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = 'Total' p_model = 'LF'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = 'Total'.
    zprdqtyshift-p_model = 'LF'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  READ TABLE zprdqtyshift WITH KEY shift = 'Total' p_model = 'Z'.
  IF sy-subrc NE 0.
    zprdqtyshift-shift = 'Total'.
    zprdqtyshift-p_model = 'Z'.
    zprdqtyshift-prdqty = '0'.
    APPEND zprdqtyshift.
  ENDIF.

  SORT zprdqtyshift BY shift p_model.

  LOOP AT zprdqtyshift.
    $ix = sy-tabix.
    AT END OF shift.
      $flag = true.
    ENDAT.
    CHECK $flag EQ true.
    CLEAR $flag.
    CLEAR zprdqtyshift-shift.
    MODIFY zprdqtyshift INDEX $ix.
  ENDLOOP.

  LOOP AT zprdqtyshift.
    $ix = sy-tabix.
    AT NEW shift.
      $flag = true.
    ENDAT.
    IF $flag EQ true.
      CLEAR $flag.
    ELSE.
      CLEAR zprdqtyshift-shift.
      MODIFY zprdqtyshift INDEX $ix.
    ENDIF.
  ENDLOOP.

  zprdqtyshift-p_model = space.
  MODIFY zprdqtyshift TRANSPORTING p_model WHERE p_model = 'Z'.

*... Begin{ 3Shift, Commented original Source 3Shift 06/26/2012
*  __cls  gt_zprdqtyshift.
*
*  CLEAR :  p_model, qty_01, qty_02.
*
*  CLEAR :  p_model,
*           qty_01,         qty_02,         qty_03,
*           qty_04,         qty_05,         qty_06,
*           qty_07,         qty_08,         qty_09,
*           qty_10,         qty_11,         qty_12,
*           qty_13,         qty_14,         qty_15,
*           qty_16,         qty_17,         qty_18,
*           qty_19,         qty_20,         qty_21,
*           qty_22,         qty_23,         qty_24.
*
*  EXEC SQL PERFORMING GET_VALUE_SHIFT.
*    SELECT
*    p_model,
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'063000'),
*                  least(P_RP_ACTUAL_TIME,'072959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'073000'),
*                  least(P_RP_ACTUAL_TIME,'082959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'083000'),
*                  least(P_RP_ACTUAL_TIME,'092959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'093000'),
*                  least(P_RP_ACTUAL_TIME,'102959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'103000'),
*                  least(P_RP_ACTUAL_TIME,'112959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'113000'),
*                  least(P_RP_ACTUAL_TIME,'121459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'121500'),
*                  least(P_RP_ACTUAL_TIME,'131459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'131500'),
*                  least(P_RP_ACTUAL_TIME,'141459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'141500'),
*                  least(P_RP_ACTUAL_TIME,'151459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'151500'),
*                  least(P_RP_ACTUAL_TIME,'161459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'161500'),
*                  least(P_RP_ACTUAL_TIME,'171459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'171500'),
*                  least(P_RP_ACTUAL_TIME,'181459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'181500'),
*                  least(P_RP_ACTUAL_TIME,'191459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'191500'),
*                  least(P_RP_ACTUAL_TIME,'201459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'201500'),
*                  least(P_RP_ACTUAL_TIME,'211459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'211500'),
*                  least(P_RP_ACTUAL_TIME,'221459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'221500'),
*                  least(P_RP_ACTUAL_TIME,'231459'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'231500'),
*                  least(P_RP_ACTUAL_TIME,'235959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'000000'),
*                  least(P_RP_ACTUAL_TIME,'005959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'010000'),
*                  least(P_RP_ACTUAL_TIME,'015959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'020000'),
*                  least(P_RP_ACTUAL_TIME,'025959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'030000'),
*                  least(P_RP_ACTUAL_TIME,'035959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'040000'),
*                  least(P_RP_ACTUAL_TIME,'045959'), 1, 0)),
*    sum(decode(greatest(P_RP_ACTUAL_TIME,'050000'),
*                  least(P_RP_ACTUAL_TIME,'062959'), 1, 0))
*    INTO :p_model,
*         :qty_01,:qty_02,:qty_03,:qty_04,:qty_05,:qty_06,
*         :qty_07,:qty_08,:qty_09,:qty_10,:qty_11,:qty_12,
*         :qty_13,:qty_14,:qty_15,:qty_16,:qty_17,:qty_18,
*         :qty_19,:qty_20,:qty_21,:qty_22,:qty_23,:qty_24
*
*    FROM ztppvr
*   WHERE MANDT    = :SY-MANDT
*     AND K04PDAT  = :CHECK_DATE
*     AND P_STATUS = 'T01'
*     AND ZRESULT  = 'S'
*     AND NOT ( p_dest_code LIKE '%XX%' OR p_dest_code LIKE '%XY%' )
*     GROUP by p_model
*
*  ENDEXEC.
*
*  LOOP AT gt_zprdqtyshift.
*    zprdqtyshift = gt_zprdqtyshift.
*    COLLECT zprdqtyshift.
*    CLEAR zprdqtyshift.
*  ENDLOOP.
*
*  SORT zprdqtyshift BY shift ASCENDING
*                       p_model DESCENDING.
*  DATA $zprdqtyshift LIKE zprdqtyshift OCCURS 2 WITH HEADER LINE.
*
*  LOOP AT zprdqtyshift.
*    $zprdqtyshift = zprdqtyshift.
*    CLEAR $zprdqtyshift-shift.
*    COLLECT $zprdqtyshift.
*  ENDLOOP.
*
*  LOOP AT $zprdqtyshift.
*    zprdqtyshift = $zprdqtyshift.
*    zprdqtyshift-shift = 'Total'.
*    APPEND zprdqtyshift.
*    AT LAST.
*      SUM.
*      zprdqtyshift = $zprdqtyshift.
*      zprdqtyshift-shift = ''.
*      zprdqtyshift-p_model = ''.
*      APPEND zprdqtyshift.
*    ENDAT.
*  ENDLOOP.
*
*  DATA : $ix LIKE sy-tabix,
*         $flag.
*
*  __cls $zprdqtyshift.
*  LOOP AT zprdqtyshift.
*    CHECK zprdqtyshift-shift NE 'Total'.
*    CHECK zprdqtyshift-shift NE space.
*    AT END OF shift.
*      SUM.
*      $zprdqtyshift = zprdqtyshift.
*      $zprdqtyshift-p_model = ''.
*      APPEND $zprdqtyshift.
*    ENDAT.
*  ENDLOOP.
*  LOOP AT zprdqtyshift.
*    CHECK zprdqtyshift-shift NE 'Total'.
*    CHECK zprdqtyshift-shift NE space.
*    IF $ix EQ sy-tabix.
*      CONTINUE.
*    ENDIF.
*
*    $ix = sy-tabix.
*    AT END OF shift.
*      $flag = true.
*    ENDAT.
*    CHECK $flag EQ true.
*    CLEAR $flag.
*    READ TABLE $zprdqtyshift WITH KEY shift = zprdqtyshift-shift.
*    IF sy-subrc EQ 0.
*      zprdqtyshift = $zprdqtyshift.
*      ADD 1 TO $ix.
*      INSERT zprdqtyshift INDEX $ix.
*    ENDIF.
*  ENDLOOP.
*
** by ig {
*  DATA: it_modl_val TYPE ztbm_model_val_n OCCURS 10 WITH HEADER LINE.
*  DATA $zvalue TYPE zvalue.
*
*  SELECT *
*       FROM ztbm_model_val_n
*       INTO TABLE it_modl_val
*       WHERE zfield EQ '01'.
*
*  SORT it_modl_val BY zvalue.
*
*  LOOP AT zprdqtyshift.
*    $ix = sy-tabix.
*    $zvalue = zprdqtyshift-p_model(2).
*    READ TABLE it_modl_val WITH KEY zvalue = $zvalue BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      zprdqtyshift-p_model = it_modl_val-zvalnm(2).
*    ENDIF.
*    MODIFY zprdqtyshift INDEX $ix.
*  ENDLOOP.
** }
*
*  LOOP AT zprdqtyshift.
*
*    $ix = sy-tabix.
*    AT END OF shift.
*      $flag = true.
*    ENDAT.
*    CHECK $flag EQ true.
*    CLEAR $flag.
*    CLEAR zprdqtyshift-shift.
*    MODIFY zprdqtyshift INDEX $ix.
*  ENDLOOP.
*
*  LOOP AT zprdqtyshift.
*    $ix = sy-tabix.
*    AT NEW shift.
*      $flag = true.
*    ENDAT.
*    IF $flag EQ true.
*      CLEAR $flag.
*    ELSE.
*      CLEAR zprdqtyshift-shift.
*      MODIFY zprdqtyshift INDEX $ix.
*    ENDIF.
*  ENDLOOP.
*...}End
ENDFUNCTION.
