REPORT znew_cycle_analyser LINE-SIZE 132 NO STANDARD PAGE HEADING.

TABLES: ckmlmv003,
        ckmlmv004,
        ckmlmv011,
        ckmlrunperiod,
        ckmlhd,
        ckmlpp.

TYPES: BEGIN OF issue,
         kalnr_prz  LIKE ckmlmv004-kalnr_prz,
         otyp_prz   LIKE ckmlmv004-otyp_prz,
         meinh      LIKE ckmlmv004-meinh,
         in_menge   LIKE ckmlmv004-in_menge,
         neg_issue  TYPE c,
         ptyp       LIKE ckml_s_text_read-ptyp,
         werks      LIKE ckml_s_text_read-werks,
         matnr      LIKE ckml_s_text_read-matnr,
         gener_name LIKE ckml_s_text_read-gener_name,
       END OF issue.

TYPES: BEGIN OF seltab,
         cyclenr LIKE ckmlmv011-cyclenr,
         kalnr   LIKE ckmlmv011-kalnr,
         matnr   LIKE ckmlmv011-matnr,
         bwkey   LIKE ckmlmv011-bwkey,
         kalst   LIKE ckmlmv011-kalst,
         bwtar   LIKE ckmlhd-bwtar,
         sobkz   LIKE ckmlhd-sobkz,
         kalnr_inmat  LIKE ckmlmv004-kalnr_inmat,
         kalnr_prz    LIKE ckmlmv004-kalnr_prz,
         otyp_prz     LIKE ckmlmv004-otyp_prz,
         meinh        LIKE ckmlmv004-meinh,
         in_menge     LIKE ckmlmv004-in_menge,
         kalnr_in     LIKE ckmlmv003-kalnr_in,
         kalnr_bal    LIKE ckmlmv003-kalnr_bal,
         kalnr_out    LIKE ckmlmv003-kalnr_out,
       END OF seltab.

TYPES: BEGIN OF cyclemat,
         kalnr   LIKE ckmlmv011-kalnr,
         cyclenr LIKE ckmlmv011-cyclenr,
         matnr   LIKE ckmlmv011-matnr,
         bwkey   LIKE ckmlmv011-bwkey,
         kalst   LIKE ckmlmv011-kalst,
         bwtar   LIKE ckmlhd-bwtar,
         sobkz   LIKE ckmlhd-sobkz,
         cycle_issue TYPE issue OCCURS 0,
         proc_issue  TYPE issue OCCURS 0,
         csalt_issue TYPE issue OCCURS 0,
         kbkumo  LIKE ckmlpp-lbkum,
         vnkumo_acc LIKE ckmlmv004-in_menge,
         has_neg_issue TYPE c,
         error1 TYPE c,
       END OF cyclemat.

TYPES: BEGIN OF balt_struct,
         kalnr_bal  LIKE ckmlmv003-kalnr_bal,
         ptyp       LIKE ckml_s_text_read-ptyp,
         gener_name LIKE ckml_s_text_read-gener_name,
       END OF balt_struct.


TYPES: bal_list TYPE HASHED TABLE OF balt_struct WITH UNIQUE
                     KEY kalnr_bal.

TYPES: BEGIN OF ty_kalnr_out,
         kalnr          LIKE ckmlmv011-kalnr,
         cyclenr        LIKE ckmlmv011-cyclenr,
         kalnr_bal_list TYPE bal_list,
       END OF ty_kalnr_out.

TYPES: BEGIN OF t_cycles,
         cyclenr        LIKE ckmlmv011-cyclenr,
         total_issues   TYPE i,
         total_receipts TYPE i,
       END OF t_cycles.

TYPES: kalnr_list TYPE HASHED TABLE OF ckmlmv003-kalnr_bal WITH UNIQUE
                     DEFAULT KEY .

DATA: BEGIN OF group_seltab,
        cyclenr   LIKE ckmlmv011-cyclenr,
        kalnr     LIKE ckmlmv011-kalnr,
        kalnr_prz LIKE ckmlmv004-kalnr_prz,
        kalnr_out LIKE ckmlmv003-kalnr_out,
        kalnr_bal LIKE ckmlmv003-kalnr_bal,
        otyp_prz  LIKE ckmlmv004-otyp_prz,
        meinh     LIKE ckmlmv004-meinh,
        in_menge  LIKE ckmlmv004-in_menge,
        cycle_flag TYPE c,
        total_issues TYPE i,
        total_receipts TYPE i,
      END OF group_seltab.

DATA: lt_kalnr_prz LIKE ckmlmv004-kalnr_prz OCCURS 0.
DATA: lt_seltab TYPE seltab OCCURS 0 WITH HEADER LINE.
DATA: lt_cyclemat TYPE HASHED TABLE OF cyclemat WITH UNIQUE KEY kalnr
                                                WITH HEADER LINE.
DATA: lt_kalnr_out TYPE HASHED TABLE OF ty_kalnr_out
                     WITH UNIQUE KEY kalnr
                                          WITH HEADER LINE.
DATA: lt_cycles TYPE HASHED TABLE OF t_cycles WITH UNIQUE KEY cyclenr
                                              WITH HEADER LINE.

DATA: lf_runperiod TYPE ckml_run_period_data.
DATA: wa_issue TYPE issue.
DATA: BEGIN OF wa_kalnr_out,
        kalnr   LIKE ckmlmv011-kalnr,
        cyclenr LIKE ckmlmv011-cyclenr,
      END OF wa_kalnr_out.
DATA: first_record TYPE c VALUE 'X'.
DATA: total_alt_sum TYPE i.
DATA: error_count_1 TYPE i.
DATA: error1_mats TYPE kalnr_list WITH HEADER LINE.
DATA: investigated_mats TYPE i.
DATA: ausgz,
      l_kalnr LIKE ckmlmv011-kalnr,
      wa_kalnr_bal TYPE balt_struct.

DATA: lt_ckml_t_text_read TYPE ckml_t_text_read WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK selection WITH FRAME TITLE text-012.
PARAMETERS:

  p_runtyp LIKE ckmlrunperiod-run_type OBLIGATORY
                                       VALUE CHECK
                                      MEMORY ID ckml_run_type
,
  p_poper LIKE ckmlrunperiod-poper     OBLIGATORY
                                       VALUE CHECK
                                      MEMORY ID mlp
                                       MODIF ID 002,

  p_gjahr LIKE ckmlrunperiod-gjahr     OBLIGATORY
                                       VALUE CHECK
                                      MEMORY ID mlb
                                       MODIF ID 002.

SELECT-OPTIONS: s_cyclnr FOR ckmlmv011-cyclenr.
SELECTION-SCREEN END OF BLOCK selection.

PERFORM initialize_variables.
PERFORM get_run_id.
PERFORM select_kalnr_out CHANGING lt_kalnr_out[].
PERFORM select_dbsort.
PERFORM write_statistics.
PERFORM write_header.


************************************************************************
* END OF PROGRAM
************************************************************************
*----------------------------------------------------------------------*
*       TOP-OF-PAGE                                                    *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  WRITE : 'Cyclenr'.
  WRITE : 'Materialnumber'.
  WRITE : 'Val.area'.
  WRITE : '       Cumulated Stock'.
  WRITE : '        Consumed Stock'.
  WRITE : 'Neg.issue   '.
  WRITE : 'Error   '.
  WRITE : 'Calculationnumber'.

*&---------------------------------------------------------------------*
*&      Form  write_material_line
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_material_line.

  ADD 1 TO investigated_mats.
  IF lt_cyclemat-error1 = '2'.
    FORMAT INTENSIFIED ON HOTSPOT ON COLOR COL_NEGATIVE.
  ELSEIF  lt_cyclemat-error1 = '1'.
    FORMAT INTENSIFIED ON HOTSPOT ON COLOR COL_TOTAL.
  ENDIF.
  WRITE: / lt_cyclemat-cyclenr UNDER 'Cyclenr'.
  WRITE:  lt_cyclemat-matnr UNDER    'Materialnumber',
           lt_cyclemat-bwkey UNDER   'Val.area'.
  WRITE lt_cyclemat-kbkumo UNDER     '       Cumulated Stock' .
  WRITE lt_cyclemat-vnkumo_acc UNDER '        Consumed Stock'.
  WRITE lt_cyclemat-has_neg_issue UNDER 'Neg.issue   '.
  WRITE lt_cyclemat-error1 UNDER 'Error   '.
  WRITE  lt_cyclemat-kalnr UNDER 'Calculationnumber'.
  FORMAT RESET.
  ausgz = 'X'.
  l_kalnr = lt_cyclemat-kalnr.
  HIDE: ausgz, l_kalnr.
  CLEAR: ausgz, l_kalnr.

ENDFORM.                    "write_material_line


*---------------------------------------------------------------------*
*       TOP-OF-PAGE DURING LINE-SELECTION                             *
*---------------------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.

  SET PF-STATUS space.


*---------------------------------------------------------------------*
*       AT USER-COMMAND                                               *
*---------------------------------------------------------------------*
AT USER-COMMAND.

  CASE sy-ucomm.
    WHEN 'EF02'.
*     Verzweigung auf Detailbild
*     Ausgehend von Uebersicht
      CHECK ausgz NE space. "Auswählbare Zeile?
      PERFORM issue_list USING l_kalnr.
    WHEN 'EF12' OR 'RW'.
*     zurück
      IF sy-lsind = 1.
        LEAVE.
      ELSE.
        sy-lsind = sy-lsind - 1.
      ENDIF.
    WHEN 'EF15'.
*     zurück
      LEAVE.
  ENDCASE.
  CLEAR ausgz.

*----------------------------------------------------------------------*
*       FORM INITIALIZE_VARIABLES                                      *
*----------------------------------------------------------------------*
*       ........                                                       *
*----------------------------------------------------------------------*
FORM initialize_variables.

  CLEAR: lf_runperiod,
         wa_issue,
         wa_kalnr_out,
         total_alt_sum,
         error_count_1,
         error1_mats[],
         investigated_mats.

ENDFORM.                    "initialize_variables
*----------------------------------------------------------------------*
*       FORM GET_RUN_ID                                                *
*----------------------------------------------------------------------*
*       ........                                                       *
*----------------------------------------------------------------------*

FORM get_run_id.

  CALL FUNCTION 'CKML_RUN_PERIOD_GET'
     EXPORTING
*           i_run_id         = ' '
          i_run_type       = p_runtyp
*           I_LAST_DAY       = CP_RUNDAY
          i_poper          = p_poper
          i_gjahr          = p_gjahr
          i_appl           = 'ACRU'
     IMPORTING
          es_runperiod     = lf_runperiod
*      exceptions
*           run_not_existent = 1
*           others           = 2
  .

ENDFORM.                    "get_run_id


*----------------------------------------------------------------------*
*       FORM SELECT_CYCLE_BALTS                                        *
*----------------------------------------------------------------------*
*       ........                                                       *
*----------------------------------------------------------------------*
FORM select_kalnr_out CHANGING ct_kalnr_out TYPE ANY TABLE.

  DATA: ls_kalnr_out TYPE ty_kalnr_out.

  SELECT c11a~kalnr c11a~cyclenr
    INTO CORRESPONDING FIELDS OF wa_kalnr_out
    FROM ckmlmv003 AS c03 JOIN ckmlmv011 AS c11a
    ON    c11a~kalnr = c03~kalnr_out
    WHERE c11a~laufid  =  lf_runperiod-run_id
      AND c11a~cyclenr >  0
      AND c11a~kalst   >  0
      AND c11a~cyclenr IN s_cyclnr
    .
    ls_kalnr_out-kalnr = wa_kalnr_out-kalnr.
    ls_kalnr_out-cyclenr = wa_kalnr_out-cyclenr.
    INSERT ls_kalnr_out INTO TABLE ct_kalnr_out.


  ENDSELECT.

ENDFORM.                    "select_kalnr_out

*----------------------------------------------------------------------*
*       FORM SELECT_DBSORT                                             *
*----------------------------------------------------------------------*
*       ........                                                       *
*----------------------------------------------------------------------*
FORM select_dbsort.
  CLEAR group_seltab.


  SELECT c11~cyclenr
         c11~kalnr
         c11~matnr
         c11~bwkey
         c11~kalst
         hd~bwtar
         hd~sobkz
         c04~kalnr_inmat
         c04~kalnr_prz
         c04~otyp_prz
         c04~meinh
         c04~in_menge
         c03~kalnr_in
         c03~kalnr_bal
         c03~kalnr_out
    INTO CORRESPONDING FIELDS OF lt_seltab
    FROM ( (
         ckmlmv011 AS c11
           JOIN
         ckmlhd    AS hd
           ON  hd~kalnr = c11~kalnr )
           JOIN
         ckmlmv004 AS c04
           ON  c04~kalnr_inmat = c11~kalnr
           AND c04~gjahr       = p_gjahr
           AND c04~perio       = p_poper
           AND c04~mgtyp       = '00001' )
*          AND c04~otyp_inmat  = 'MB' )
           LEFT OUTER JOIN
         ckmlmv003 AS c03
           ON c03~kalnr_in     = c04~kalnr_prz
           AND c03~gjahr       = p_gjahr
           AND c03~perio       = p_poper
           AND c03~mgtyp       = '00001'
    WHERE c11~laufid   =  lf_runperiod-run_id
      AND c11~cyclenr  >  0
      AND c11~kalst    >  0
      AND c11~cyclenr  IN s_cyclnr
    ORDER BY C11~CYCLENR c11~kalnr C04~KALNR_PRZ c03~kalnr_out
                c03~kalnr_bal
    .


    PERFORM process_single_seltab.

  ENDSELECT.
* Now look at the last record to finish
  IF sy-subrc = 0.
    PERFORM process_last_seltab.
  ENDIF.
ENDFORM.                    "select_dbsort


*---------------------------------------------------------------------*
*       FORM PROCESS_SINGLE_SELTAB                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_single_seltab.
  IF first_record = 'X'.
    MOVE-CORRESPONDING lt_seltab TO group_seltab.
    MOVE-CORRESPONDING lt_seltab TO lt_cyclemat.
    MOVE-CORRESPONDING lt_seltab TO lt_cycles.
    CLEAR first_record.
  ENDIF.
  IF lt_seltab-cyclenr   <> group_seltab-cyclenr
  OR lt_seltab-kalnr     <> group_seltab-kalnr
  OR lt_seltab-kalnr_prz <> group_seltab-kalnr_prz
  OR lt_seltab-kalnr_out <> group_seltab-kalnr_out
  OR lt_seltab-kalnr_bal <> group_seltab-kalnr_bal
  OR lt_seltab-kalnr_out IS INITIAL.
* Beim Wechsel der Beschaffungsalternative
    PERFORM process_balt.
    IF lt_seltab-cyclenr   <> group_seltab-cyclenr
    OR lt_seltab-kalnr     <> group_seltab-kalnr
    OR lt_seltab-kalnr_prz <> group_seltab-kalnr_prz
    OR lt_seltab-kalnr_out <> group_seltab-kalnr_out.
* Beim Wechsel des produzierten Materials
      PERFORM process_kalnr_out.
      IF lt_seltab-cyclenr   <> group_seltab-cyclenr
      OR lt_seltab-kalnr     <> group_seltab-kalnr
      OR lt_seltab-kalnr_prz <> group_seltab-kalnr_prz.
* Beim Wechsel des Prozesses
        PERFORM process_kalnr_prz.
        IF lt_seltab-cyclenr <> group_seltab-cyclenr
        OR lt_seltab-kalnr   <> group_seltab-kalnr.
* Beim Wechsel des Inputmaterials
          PERFORM process_kalnr.
          IF lt_seltab-cyclenr <> group_seltab-cyclenr.
            PERFORM process_cyclenr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "process_single_seltab

*---------------------------------------------------------------------*
*       FORM PROCESS_LAST_SELTAB                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_last_seltab.
  PERFORM process_kalnr_out.
  PERFORM process_balt.
  PERFORM process_kalnr_prz.
  PERFORM process_kalnr.
  PERFORM process_cyclenr.
ENDFORM.                    "process_last_seltab

*---------------------------------------------------------------------*
*       FORM PROCESS_CYCLENR                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_cyclenr.
  lt_cycles-cyclenr = group_seltab-cyclenr.
  lt_cycles-total_issues = group_seltab-total_issues.
  INSERT TABLE lt_cycles.
  CLEAR: lt_cycles,
         group_seltab-total_issues.
  group_seltab-cyclenr = lt_seltab-cyclenr.
ENDFORM.                    "process_cyclenr

*---------------------------------------------------------------------*
*       FORM PROCESS_KALNR                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_kalnr.
  SELECT SINGLE * FROM ckmlpp
    WHERE bdatj = p_gjahr
    AND   poper = p_poper
    AND  untper = '000'
    AND   kalnr = lt_cyclemat-kalnr.

  lt_cyclemat-kbkumo = ckmlpp-abkumo
                     + ckmlpp-umkumo
                     + ckmlpp-zukumo
                     + ckmlpp-vpkumo.

* Vereinfachung, wenn nur Test 1 durchgeführt wird.
* Bei späterer Einführung weiterer Tests löschen!!!

  PERFORM test1.
  PERFORM write_material_line.
  .
* Ende Vereinfachung
  INSERT TABLE lt_cyclemat.
  CLEAR lt_cyclemat.
  MOVE-CORRESPONDING lt_seltab TO lt_cyclemat.
  group_seltab-kalnr = lt_seltab-kalnr.
ENDFORM.                    "process_kalnr


*---------------------------------------------------------------------*
*       FORM PROCESS_KALNR_PRZ                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_kalnr_prz.
  MOVE-CORRESPONDING group_seltab TO wa_issue.
  IF wa_issue-in_menge > 0.
    lt_cyclemat-has_neg_issue = 'X'.
    wa_issue-neg_issue = 'X'.
  ENDIF.
  IF wa_issue-otyp_prz = 'VA'.
    APPEND wa_issue TO lt_cyclemat-csalt_issue.
  ELSEIF group_seltab-cycle_flag = 'X'.
    APPEND wa_issue TO lt_cyclemat-cycle_issue.
    lt_cyclemat-vnkumo_acc = lt_cyclemat-vnkumo_acc +
                                  ABS( wa_issue-in_menge ).
  ELSE.
    APPEND wa_issue TO lt_cyclemat-proc_issue.
  ENDIF.
  CLEAR: wa_issue,
         group_seltab-cycle_flag.
  group_seltab-kalnr_prz = lt_seltab-kalnr_prz.
  group_seltab-otyp_prz = lt_seltab-otyp_prz.
  group_seltab-meinh = lt_seltab-meinh.
  group_seltab-in_menge = lt_seltab-in_menge.
ENDFORM.                    "process_kalnr_prz

*---------------------------------------------------------------------*
*       FORM PROCESS_KALNR_OUT                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM process_kalnr_out.
  IF NOT  ( group_seltab-kalnr_out IS INITIAL ).
    READ TABLE lt_kalnr_out WITH KEY cyclenr = group_seltab-cyclenr
                                kalnr   = group_seltab-kalnr_out.
    IF sy-subrc = 0.
      group_seltab-cycle_flag = 'X'.
    ENDIF.
  ENDIF.
  group_seltab-kalnr_out = lt_seltab-kalnr_out.
ENDFORM.                    "process_kalnr_out

*---------------------------------------------------------------------*
*       FORM PROCESS_BALT                                             *
*---------------------------------------------------------------------*
*                                                                     *
*---------------------------------------------------------------------*
FORM process_balt.
* Schreibt die Kalkulationsnr. der Beschaffungsalternative in Liste der
* Alternativen
  IF NOT ( group_seltab-kalnr_out IS INITIAL ).
    READ TABLE lt_kalnr_out WITH KEY cyclenr = group_seltab-cyclenr
                                kalnr   = group_seltab-kalnr_out.
    IF sy-subrc = 0.
      READ TABLE lt_kalnr_out-kalnr_bal_list WITH TABLE KEY
           kalnr_bal = group_seltab-kalnr_bal TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        MOVE group_seltab-kalnr_bal TO wa_kalnr_bal.
        INSERT wa_kalnr_bal INTO TABLE lt_kalnr_out-kalnr_bal_list.
        MODIFY TABLE lt_kalnr_out.
      ENDIF.
    ENDIF.
  ENDIF.
  group_seltab-kalnr_bal = lt_seltab-kalnr_bal.
ENDFORM.                    "process_balt


*---------------------------------------------------------------------*
*       FORM TEST1                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM test1.
  IF lt_cyclemat-vnkumo_acc >= lt_cyclemat-kbkumo.
    IF lt_cyclemat-vnkumo_acc = lt_cyclemat-kbkumo.
      lt_cyclemat-error1 = '1'.
    ELSE.
      lt_cyclemat-error1 = '2'.
    ENDIF.
    ADD 1 TO error_count_1.
    INSERT lt_cyclemat-kalnr INTO TABLE error1_mats.
  ENDIF.
ENDFORM.                                                    "test1


*---------------------------------------------------------------------*
*       FORM WRITE_HEADER                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM write_header.
  ULINE.
  WRITE: / 'Run: '.
  HIDE ausgz.
  WRITE:   p_runtyp, p_poper, p_gjahr.
  HIDE ausgz.
  WRITE: /  'Cycles: '.
  HIDE ausgz.
  IF s_cyclnr[] IS INITIAL.
    WRITE '*'.
    HIDE ausgz.
  ELSE.
    LOOP AT s_cyclnr.
      WRITE: s_cyclnr, ' '.
      HIDE ausgz.
    ENDLOOP.
  ENDIF.
  ULINE.
  HIDE ausgz.
ENDFORM.                    "write_header

*---------------------------------------------------------------------*
*       FORM WRITE_STATISTICS                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_statistics.
  IF ( investigated_mats > 0 ).
    ULINE.
    HIDE ausgz.
  ENDIF.
  SKIP.
  WRITE: / 'Total number of investigated materials:'.
  HIDE ausgz.
  WRITE AT 48 investigated_mats.
  HIDE ausgz.
  SKIP.
  WRITE: / 'Sum of errors:'.
  HIDE ausgz.
  WRITE AT 48 error_count_1.
  HIDE ausgz.
ENDFORM.                    "write_statistics

*---------------------------------------------------------------------*
*       FORM ISSUE_LIST                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

FORM issue_list USING i_kalnr LIKE ckmlmv011-kalnr.
  DATA: line_count TYPE i.
  READ TABLE lt_cyclemat WITH TABLE KEY kalnr = i_kalnr.
  SELECT SINGLE * FROM ckmlhd WHERE kalnr = lt_cyclemat-kalnr.

  WRITE: /'Materialnr:', ckmlhd-matnr,
         /'Valuation Area:', ckmlhd-bwkey,
         /'Valuation Type:', ckmlhd-bwtar.
  IF NOT ckmlhd-vbeln IS INITIAL.
    WRITE: /'Sales order:', ckmlhd-vbeln,
           /'Position:', ckmlhd-posnr.
  ENDIF.
  IF NOT ckmlhd-pspnr IS INITIAL.
    WRITE: /'Project:', ckmlhd-pspnr.
  ENDIF.

  HIDE ausgz.
  ULINE.
  HIDE ausgz.
  DESCRIBE TABLE lt_cyclemat-cycle_issue LINES line_count.
  IF line_count > 0.

    WRITE: 'Issues to cycle processes:'.
    LOOP AT lt_cyclemat-cycle_issue INTO wa_issue.
      MOVE wa_issue-kalnr_prz TO
           lt_ckml_t_text_read-kalnr.
      APPEND lt_ckml_t_text_read.
    ENDLOOP.

    CALL FUNCTION 'CKML_MGV_TEXT_READ'
         EXPORTING
              i_language   = sy-langu
         CHANGING
              ct_text_read = lt_ckml_t_text_read[].

    LOOP AT lt_ckml_t_text_read.
      READ TABLE lt_cyclemat-cycle_issue WITH KEY
                    kalnr_prz = lt_ckml_t_text_read-kalnr
                    INTO wa_issue.
      MOVE-CORRESPONDING lt_ckml_t_text_read TO wa_issue.
      WRITE: / wa_issue-kalnr_prz.
      WRITE AT 14 wa_issue-otyp_prz.
      WRITE AT 19 wa_issue-meinh.
      WRITE AT 25 wa_issue-in_menge.
      WRITE AT 46 wa_issue-neg_issue.
      WRITE AT 49 wa_issue-ptyp.
      WRITE AT 54 wa_issue-werks.
      WRITE AT 60 wa_issue-matnr.
      WRITE AT 79 wa_issue-gener_name.
      HIDE ausgz.
    ENDLOOP.
    CLEAR: lt_ckml_t_text_read, lt_ckml_t_text_read[].

    ULINE.
    HIDE ausgz.
    SKIP.
  ENDIF.

  DESCRIBE TABLE lt_cyclemat-proc_issue LINES line_count.
  IF line_count > 0.
    WRITE: / 'Issues to other processes:'.

    LOOP AT lt_cyclemat-proc_issue INTO wa_issue.
      MOVE wa_issue-kalnr_prz TO
           lt_ckml_t_text_read-kalnr.
      APPEND lt_ckml_t_text_read.
    ENDLOOP.

    CALL FUNCTION 'CKML_MGV_TEXT_READ'
         EXPORTING
              i_language   = sy-langu
         CHANGING
              ct_text_read = lt_ckml_t_text_read[].

    LOOP AT lt_ckml_t_text_read.
      READ TABLE lt_cyclemat-proc_issue WITH KEY
                    kalnr_prz = lt_ckml_t_text_read-kalnr
                    INTO wa_issue.
      MOVE-CORRESPONDING lt_ckml_t_text_read TO wa_issue.
      WRITE: / wa_issue-kalnr_prz.
      WRITE AT 14 wa_issue-otyp_prz.
      WRITE AT 19 wa_issue-meinh.
      WRITE AT 25 wa_issue-in_menge.
      WRITE AT 46 wa_issue-neg_issue.
      WRITE AT 49 wa_issue-ptyp.
      WRITE AT 54 wa_issue-werks.
      WRITE AT 60 wa_issue-matnr.
      WRITE AT 79 wa_issue-gener_name.
      HIDE ausgz.
    ENDLOOP.
    CLEAR: lt_ckml_t_text_read, lt_ckml_t_text_read[].

    ULINE.
    HIDE ausgz.
    SKIP.
  ENDIF.

  DESCRIBE TABLE lt_cyclemat-csalt_issue LINES line_count.
  IF line_count > 0.
    WRITE: 'Issues to consumption alternatives:'.

    LOOP AT lt_cyclemat-csalt_issue INTO wa_issue.
      MOVE wa_issue-kalnr_prz TO
           lt_ckml_t_text_read-kalnr.
      APPEND lt_ckml_t_text_read.
    ENDLOOP.

    CALL FUNCTION 'CKML_MGV_TEXT_READ'
         EXPORTING
              i_language   = sy-langu
         CHANGING
              ct_text_read = lt_ckml_t_text_read[].

    LOOP AT lt_ckml_t_text_read.
      READ TABLE lt_cyclemat-csalt_issue WITH KEY
                    kalnr_prz = lt_ckml_t_text_read-kalnr
                    INTO wa_issue.
      MOVE-CORRESPONDING lt_ckml_t_text_read TO wa_issue.
      WRITE: / wa_issue-kalnr_prz.
      WRITE AT 14 wa_issue-otyp_prz.
      WRITE AT 19 wa_issue-meinh.
      WRITE AT 25 wa_issue-in_menge.
      WRITE AT 46 wa_issue-neg_issue.
      WRITE AT 49 wa_issue-ptyp.
      WRITE AT 54 wa_issue-werks.
      WRITE AT 60 wa_issue-matnr.
      WRITE AT 79 wa_issue-gener_name.
      HIDE ausgz.
    ENDLOOP.
    CLEAR: lt_ckml_t_text_read, lt_ckml_t_text_read[].

    ULINE.
    HIDE ausgz.
    SKIP.
  ENDIF.

  READ TABLE lt_kalnr_out WITH TABLE KEY kalnr = i_kalnr.
  DESCRIBE TABLE lt_kalnr_out-kalnr_bal_list LINES line_count.
  IF line_count > 0.
    WRITE: / 'Procurement alternatives:'.

    LOOP AT lt_kalnr_out-kalnr_bal_list INTO wa_kalnr_bal.
      MOVE wa_kalnr_bal-kalnr_bal TO
      lt_ckml_t_text_read-kalnr.
      APPEND lt_ckml_t_text_read.
    ENDLOOP.

    CALL FUNCTION 'CKML_MGV_TEXT_READ'
         EXPORTING
              i_language   = sy-langu
         CHANGING
              ct_text_read = lt_ckml_t_text_read[].

    LOOP AT lt_ckml_t_text_read.
      READ TABLE lt_kalnr_out-kalnr_bal_list WITH TABLE KEY
                    kalnr_bal = lt_ckml_t_text_read-kalnr
                    INTO wa_kalnr_bal.
      MOVE-CORRESPONDING lt_ckml_t_text_read TO wa_kalnr_bal.
      WRITE: / wa_kalnr_bal-kalnr_bal.
      WRITE AT 14 wa_kalnr_bal-ptyp.
      WRITE AT 19 wa_kalnr_bal-gener_name.
      HIDE ausgz.
    ENDLOOP.
    CLEAR: lt_ckml_t_text_read, lt_ckml_t_text_read[].
  ENDIF.

ENDFORM.                    "issue_list
