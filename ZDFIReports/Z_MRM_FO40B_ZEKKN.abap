*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120031469 0000281510                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          All Support Package Levels                   $*
*$  Release 500          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS Z_MRM_FO40B_ZEKKN
*& Object Header   PROG Z_MRM_FO40B_ZEKKN
*&-------------------------------------------------------------------*
*& REPORT Z_MRM_FO40B_ZEKKN
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*&---------------------------------------------------------------------*
*& Report  Z_MRM_FO40B_ZEKKN                                           *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Correction of PO history for invoices, which were created in       *
*&  release 4.0 for limit and service PO items without GR-based IV     *
*&---------------------------------------------------------------------*
* Change history                                                       *
* 22.01.02: Version 0, Responsible: AndSch, note 423650                *
* 15.10.03: Version 1, Responsible: AlKr, Report is completely         *
*           redesigned. Now it works also for the massive update       *
*----------------------------------------------------------------------*

REPORT  z_mrm_fo40b_zekkn             .


RANGES pstyprange FOR ekpo-pstyp.
DATA: allinvrbkp LIKE rbkp OCCURS 0 WITH HEADER LINE,
      allinvrseg LIKE rseg OCCURS 0 WITH HEADER LINE,
      allinvekpo LIKE ekpo OCCURS 0 WITH HEADER LINE,
      allinvekbe LIKE ekbe OCCURS 0 WITH HEADER LINE.
TABLES: rbkp, rseg, ekpo, ekbe.
DATA: s_rseg LIKE rseg,
      s_ekbe LIKE ekbe,
      t_rseg_update LIKE rseg OCCURS 0 WITH HEADER LINE,
      t_ekbe_insert LIKE ekbe OCCURS 0 WITH HEADER LINE,
      t_ekbe_delete LIKE ekbe OCCURS 0 WITH HEADER LINE.

SELECT-OPTIONS:
                 so_gjahr FOR rbkp-gjahr MATCHCODE OBJECT mekkh,
                 so_belnr FOR rbkp-belnr MATCHCODE OBJECT mekkh,
                 so_bukrs FOR rbkp-bukrs MATCHCODE OBJECT mekkh.

PARAMETERS:      p_update AS CHECKBOX DEFAULT ' ',
                 p_user(8) TYPE c.
pstyprange-sign = 'I'.
pstyprange-option = 'EQ'.
pstyprange-low = '1'.                "blanko/Limit item
APPEND pstyprange.
pstyprange-sign = 'I'.
pstyprange-option = 'EQ'.
pstyprange-low = '9'.                "service
APPEND pstyprange.

IF NOT p_update IS INITIAL AND NOT p_user EQ 'SAPUSER'.
  WRITE: 'Update should only be done by SAP'.
  EXIT.
ENDIF.


* Select all invoices posted in 40B, which were not cancelled
SELECT * FROM  rbkp INTO TABLE allinvrbkp
       WHERE
       belnr  IN so_belnr AND
       gjahr  IN so_gjahr AND
       bukrs  IN so_bukrs AND
       saprl  = '40B' AND
       stblg  = space.
IF sy-subrc NE 0.
  WRITE: 'No relevant entries found in table RBKP.'.
  EXIT.
ENDIF.

* Select corresponding invoice items with ZEKKN = '00' assigned to
* blanket or service PO without GR-based IV
SELECT * FROM  rseg INTO TABLE allinvrseg
      FOR ALL ENTRIES IN allinvrbkp
         WHERE  belnr  = allinvrbkp-belnr
         AND    gjahr  = allinvrbkp-gjahr
         AND    zekkn  = '00'
         AND    lfbnr  = space
         AND    pstyp  IN pstyprange.
IF sy-subrc NE 0.
  WRITE: 'No relevant entries found in table RSEG.'.
  EXIT.
ENDIF.

* Select corresponding data with wrong ZEKKN from PO history
SELECT * FROM  ekbe INTO TABLE allinvekbe
          FOR ALL ENTRIES IN allinvrseg
          WHERE  ebeln  = allinvrseg-ebeln
          AND    ebelp  = allinvrseg-ebelp
          AND    zekkn  = '00'
          AND    ( vgabe  = '2' OR
                   vgabe  = '3' )
          AND    gjahr  = allinvrseg-gjahr
          AND    belnr  = allinvrseg-belnr.
IF sy-subrc NE 0.
  WRITE: 'No relevant entries found in table EKBE.'.
  EXIT.
ENDIF.

* Check and correct data.
REFRESH: t_rseg_update, t_ekbe_delete, t_ekbe_insert.
CLEAR: t_rseg_update, t_ekbe_delete, t_ekbe_insert.
LOOP AT allinvrseg INTO s_rseg.
  CLEAR s_ekbe.
  LOOP AT allinvekbe INTO s_ekbe
                     WHERE ebeln = s_rseg-ebeln AND
                           ebelp = s_rseg-ebelp AND
                           gjahr = s_rseg-gjahr AND
                           belnr = s_rseg-belnr AND
                           buzei = s_rseg-buzei.
    APPEND s_ekbe TO t_ekbe_delete.
    s_ekbe-zekkn = '99'.
    APPEND s_ekbe TO t_ekbe_insert.
  ENDLOOP.
  IF sy-subrc = 0.
* There is at least one line with ZEKKN = 00 in the EKBE,
* then RSEG should be also corrected
    s_rseg-zekkn = '99'.
    APPEND s_rseg TO t_rseg_update.
  ENDIF.
ENDLOOP.

* Print the results
FORMAT COLOR 1 ON.
IF p_update IS INITIAL.
  WRITE: / 'Following PO history items should be deleted'.
  ULINE.
ELSE.
  WRITE: /.
  WRITE: / 'Following PO history items were deleted'.
  ULINE.
ENDIF.
FORMAT COLOR OFF.
WRITE: / 'PO number ',
         'PO Item',
         'Invoice   ',
         'Fisc.year',
         'Position',
         'ZEKKN'.
ULINE.
LOOP AT t_ekbe_delete.
  IF p_update = 'X'.
    DELETE ekbe FROM t_ekbe_delete.
    IF sy-subrc <> 0.
      FORMAT COLOR 6 ON.
      WRITE: / 'Error by deleting PO history: ',
               t_ekbe_delete-ebeln,
               t_ekbe_delete-ebelp,
               t_ekbe_delete-belnr,
               t_ekbe_delete-gjahr,
               t_ekbe_delete-buzei.
      FORMAT COLOR OFF.
      CONTINUE.
    ENDIF.
  ENDIF.
  WRITE AT: /0(10) t_ekbe_delete-ebeln,
          12(5) t_ekbe_delete-ebelp,
          20(10) t_ekbe_delete-belnr,
          31(4) t_ekbe_delete-gjahr,
          41(4) t_ekbe_delete-buzei,
          50(2) t_ekbe_delete-zekkn.
ENDLOOP.

FORMAT COLOR 1 ON.
IF p_update IS INITIAL.
  WRITE: /.
  WRITE: / 'Following PO history items should be inserted'.
  ULINE.
ELSE.
  WRITE: /.
  WRITE: / 'Following PO history itemsi were inserted'.
  ULINE.
ENDIF.
FORMAT COLOR OFF.
WRITE: / 'PO number ',
         'PO Item',
         'Invoice   ',
         'Fisc.year',
         'Position',
         'ZEKKN'.
ULINE.
LOOP AT t_ekbe_insert.
  IF p_update = 'X'.
    INSERT INTO ekbe VALUES t_ekbe_insert.
    IF sy-subrc <> 0.
      FORMAT COLOR 6 ON.
      WRITE: / 'Error by inserting PO history: ',
               t_ekbe_insert-ebeln,
               t_ekbe_insert-ebelp,
               t_ekbe_insert-belnr,
               t_ekbe_insert-gjahr,
               t_ekbe_insert-buzei.
      FORMAT COLOR OFF.
      CONTINUE.
    ENDIF.
  ENDIF.
  WRITE AT: /0(10) t_ekbe_insert-ebeln,
           12(5) t_ekbe_insert-ebelp,
           20(10) t_ekbe_insert-belnr,
           31(4) t_ekbe_insert-gjahr,
           41(4) t_ekbe_insert-buzei,
           50(2) t_ekbe_insert-zekkn.
ENDLOOP.

FORMAT COLOR 1 ON.
IF p_update IS INITIAL.
  WRITE: /.
  WRITE: / 'Following invoice document lines should be corrected'.
  ULINE.
ELSE.
  WRITE: / 'Following invoice document lines were corrected'.
  ULINE.
ENDIF.
FORMAT COLOR OFF.
WRITE: / 'Invoice   ',
         'Fisc.year',
         'Position',
         'Old ZEKKN',
         'New ZEKKN'.
ULINE.
LOOP AT t_rseg_update.
  IF p_update = 'X'.
    UPDATE rseg SET zekkn = t_rseg_update-zekkn
            WHERE  belnr = t_rseg_update-belnr AND
                   gjahr = t_rseg_update-gjahr AND
                   buzei = t_rseg_update-buzei.
    IF sy-subrc <> 0.
      FORMAT COLOR 6 ON.
      WRITE: / 'Error by updating RSEG: ',
               t_rseg_update-belnr,
               t_rseg_update-gjahr,
               t_rseg_update-buzei.
      FORMAT COLOR OFF.
      CONTINUE.
    ENDIF.
  ENDIF.
  WRITE AT: /0(10) t_rseg_update-belnr,
           12(4) t_rseg_update-gjahr,
           22(6) t_rseg_update-buzei,
           31(2) '00',
           41(2) t_rseg_update-zekkn.
ENDLOOP.

*>>>> END OF INSERTION <<<<<<
