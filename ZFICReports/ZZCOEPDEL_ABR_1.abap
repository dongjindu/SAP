*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000616912                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 45B          All Support Package Levels                   $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$  Release 470          All Support Package Levels                   $*
*$  Release 500          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZZCOEPDEL_ABR_1
*& Object Header   PROG ZZCOEPDEL_ABR_1
*&-------------------------------------------------------------------*
*& REPORT ZZCOEPDEL_ABR_1
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT ZZCOEPDEL_ABR_1.
TABLES: COBK, COEP, COVP, BSEG, BSIS, BKPF, ANLI.
SELECT-OPTIONS: CO_OBJNR FOR COEP-OBJNR,
                CO_GJAHR FOR COEP-GJAHR,
                CO_KSTAR FOR COEP-KSTAR,
                CO_KOKRS FOR COBK-KOKRS,
                CO_BELNR FOR COBK-BELNR,
                CO_BUZEI FOR COEP-BUZEI.
PARAMETERS: INVMA AS CHECKBOX.
PARAMETERS: TEST AS CHECKBOX DEFAULT 'X'.
DATA: GT_COVP LIKE COVP OCCURS 100 WITH HEADER LINE.
DATA: GT_COEP_DEL LIKE COEP OCCURS 100 WITH HEADER LINE.
DATA: GT_BSEG_UPD LIKE BSEG OCCURS 100 WITH HEADER LINE.
DATA: GD_PSP LIKE PRPS-PSPNR.
DATA: GD_OBJNR_OLD LIKE COEP-OBJNR.
DATA: CNT_COEP TYPE I.
DATA: CNT_BSEG TYPE I.

IF CO_OBJNR[] IS INITIAL.
  CLEAR CO_OBJNR.
  CO_OBJNR-SIGN = 'I'.
  CO_OBJNR-OPTION = 'CP'.
  CO_OBJNR-LOW = 'OR*'.
  APPEND CO_OBJNR.
  CO_OBJNR-LOW = 'PR*'.
  APPEND CO_OBJNR.
ENDIF.

SELECT * FROM COVP INTO TABLE GT_COVP
                   WHERE LEDNR = '00'
                     AND OBJNR IN CO_OBJNR
                     AND GJAHR IN CO_GJAHR
                     AND WRTTP = '11'
                     AND KOKRS IN CO_KOKRS
                     AND BELNR IN CO_BELNR
                     AND BUZEI IN CO_BUZEI
                     AND KSTAR IN CO_KSTAR
                     AND AWTYP = 'AUAK'
                     ORDER BY OBJNR KOKRS BELNR BUZEI.
IF SY-DBCNT = 0.
  WRITE: / 'No statistical CO line-items selected'.
  STOP.
ENDIF.

IF TEST <> SPACE.
  WRITE: / '*** only testrun ***'.
ENDIF.

LOOP AT GT_COVP.
* DB-Update
  IF TEST = SPACE AND
     GT_COVP-OBJNR NE GD_OBJNR_OLD.
    PERFORM DB_UPDATE.
  ENDIF.
  GD_OBJNR_OLD = GT_COVP-OBJNR.

  IF NOT INVMA IS INITIAL.
    SELECT * FROM ANLI WHERE OBJNR = GT_COVP-OBJNR.
      EXIT.
    ENDSELECT.
    CHECK SY-SUBRC = 0.
  ENDIF.

  SELECT SINGLE * FROM BKPF WHERE AWTYP = GT_COVP-AWTYP
                              AND AWKEY = GT_COVP-REFBN.
  CHECK SY-SUBRC = 0.
  SELECT SINGLE * FROM BSEG WHERE BUKRS = BKPF-BUKRS
                              AND BELNR = BKPF-BELNR
                              AND GJAHR = BKPF-GJAHR
                              AND BUZEI = GT_COVP-REFBZ.
  CHECK SY-SUBRC = 0.
  CHECK BSEG-VORGN = 'ZUCO' AND
        NOT BSEG-ANBWA IS INITIAL AND
        NOT BSEG-ANLN1 IS INITIAL.

  IF GT_COVP-OBJNR(2) = 'OR'.
    CHECK BSEG-AUFNR = GT_COVP-OBJNR+2(12).
  ELSEIF GT_COVP-OBJNR(2) = 'PR'.
    CHECK BSEG-PROJK = GT_COVP-OBJNR+2(8).
  ELSE.
    CHECK 1 = 2.
  ENDIF.
  SELECT SINGLE * FROM COEP WHERE KOKRS = GT_COVP-KOKRS
                              AND BELNR = GT_COVP-BELNR
                              AND BUZEI = GT_COVP-BUZEI.
  CHECK SY-SUBRC = 0.
  APPEND COEP TO GT_COEP_DEL.
  APPEND BSEG TO GT_BSEG_UPD.

  WRITE: / 'table COEP:'.
  IF GT_COVP-OBJNR(2) = 'OR'.
    WRITE: ' ORD:', GT_COVP-OBJNR+2(12).
  ELSEIF GT_COVP-OBJNR(2) = 'PR'.
    GD_PSP = GT_COVP-OBJNR+2(8).
    WRITE: ' WBS:', GD_PSP.
  ENDIF.
  WRITE:     GT_COVP-KOKRS,
             GT_COVP-BELNR,
             GT_COVP-BUZEI,
             ' period:', GT_COVP-PERIO,
             '/',       GT_COVP-GJAHR,
             ' value type:', GT_COVP-WRTTP,
             '  value:', GT_COVP-WKGBTR.
  ADD 1 TO CNT_COEP.

  WRITE: /5 'table BSEG:',
            BSEG-BUKRS, BSEG-BELNR, BSEG-GJAHR, BSEG-BUZEI.
  IF GT_COVP-OBJNR(2) = 'OR'.
    WRITE: 'ORD:', BSEG-AUFNR.
  ELSEIF GT_COVP-OBJNR(2) = 'PR'.
    WRITE: 'WBS:', BSEG-PROJK.
  ENDIF.
  ADD 1 TO CNT_BSEG.
ENDLOOP.
* last CO-object
READ TABLE GT_COEP_DEL INDEX 1.
IF SY-SUBRC = 0.
  IF TEST = SPACE.
    PERFORM DB_UPDATE.
  ENDIF.
ENDIF.
ULINE.
WRITE: / 'updated records in table COEP:', CNT_COEP.
WRITE: / 'updated records in table BSEG:', CNT_BSEG.

*---------------------------------------------------------------------*
*       FORM DB_UPDATE                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DB_UPDATE.
  CHECK TEST = SPACE.
  CHECK NOT GT_COEP_DEL[] IS INITIAL.
  CHECK NOT GT_BSEG_UPD[] IS INITIAL.

* update summary records
  LOOP AT GT_COEP_DEL.
    GT_COEP_DEL-WTGBTR = - GT_COEP_DEL-WTGBTR.
    GT_COEP_DEL-WOGBTR = - GT_COEP_DEL-WOGBTR.
    GT_COEP_DEL-WKGBTR = - GT_COEP_DEL-WKGBTR.
    GT_COEP_DEL-WKFBTR = - GT_COEP_DEL-WKFBTR.
    GT_COEP_DEL-PAGBTR = - GT_COEP_DEL-PAGBTR.
    GT_COEP_DEL-PAFBTR = - GT_COEP_DEL-PAFBTR.
    GT_COEP_DEL-MEGBTR = - GT_COEP_DEL-MEGBTR.
    GT_COEP_DEL-MEFBTR = - GT_COEP_DEL-MEFBTR.
    GT_COEP_DEL-MBGBTR = - GT_COEP_DEL-MBGBTR.
    GT_COEP_DEL-MBFBTR = - GT_COEP_DEL-MBFBTR.
    MODIFY GT_COEP_DEL.
  ENDLOOP.
  CALL FUNCTION 'K_DOCUMENT_UPDATE'
       EXPORTING
            I_TOTALS_UPDATE = 'X'
            I_RCL_UPDATE    = ' '
            I_ITEMS_INSERT  = ' '
            I_SAVE_DIRECTLY = 'X'
       TABLES
            T_COEP          = GT_COEP_DEL.

  LOOP AT GT_COEP_DEL.
    SELECT * FROM COBK WHERE KOKRS = GT_COEP_DEL-KOKRS
                         AND BELNR = GT_COEP_DEL-BELNR.
      IF COBK-SUMBZ > 0.
        COBK-SUMBZ = COBK-SUMBZ - 1.
        UPDATE COBK.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

* update BSEG/BSIS
  READ TABLE GT_COEP_DEL INDEX 1.
  LOOP AT GT_BSEG_UPD.
    SELECT * FROM BSEG
                  WHERE BUKRS = GT_BSEG_UPD-BUKRS
                    AND BELNR = GT_BSEG_UPD-BELNR
                    AND GJAHR = GT_BSEG_UPD-GJAHR
                    AND BUZEI = GT_BSEG_UPD-BUZEI.
      IF GT_COEP_DEL-OBJNR(2) = 'OR'.
        BSEG-AUFNR = ' '.
      ELSEIF GT_COEP_DEL-OBJNR(2) = 'PR'.
        BSEG-PROJK = 0.
      ELSE.
        CHECK 1 = 2.
      ENDIF.
      UPDATE BSEG.
    ENDSELECT.
    SELECT * FROM BSIS
                     WHERE BUKRS = GT_BSEG_UPD-BUKRS
                       AND HKONT = GT_BSEG_UPD-HKONT
                       AND AUGDT = GT_BSEG_UPD-AUGDT
                       AND AUGBL = GT_BSEG_UPD-AUGBL
                       AND ZUONR = GT_BSEG_UPD-ZUONR
                       AND GJAHR = GT_BSEG_UPD-GJAHR
                       AND BELNR = GT_BSEG_UPD-BELNR
                       AND BUZEI = GT_BSEG_UPD-BUZEI.
      IF GT_COEP_DEL-OBJNR(2) = 'OR'.
        BSIS-AUFNR = ' '.
      ELSEIF GT_COEP_DEL-OBJNR(2) = 'PR'.
        BSIS-PROJK = 0.
      ELSE.
        CHECK 1 = 2.
      ENDIF.
      UPDATE BSIS.
    ENDSELECT.
  ENDLOOP.

  DELETE COEP FROM TABLE GT_COEP_DEL.
  COMMIT WORK.

  REFRESH GT_COEP_DEL.
  REFRESH GT_BSEG_UPD.
ENDFORM.
