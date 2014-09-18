FORM CD_CALL_ZIREQIT                       .
  IF   ( UPD_ZTREQIT                        NE SPACE )
    OR ( UPD_ICDTXT_ZIREQIT         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZIREQIT_WRITE_DOCUMENT        ' IN UPDATE TASK
        EXPORTING
          OBJECTID                = OBJECTID
          TCODE                   = TCODE
          UTIME                   = UTIME
          UDATE                   = UDATE
          USERNAME                = USERNAME
          PLANNED_CHANGE_NUMBER   = PLANNED_CHANGE_NUMBER
          OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
          PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
          NO_CHANGE_POINTERS      = CDOC_NO_CHANGE_POINTERS
          O_ZTREQIT
                      = *ZTREQIT
          N_ZTREQIT
                      = ZTREQIT
          UPD_ZTREQIT
                      = UPD_ZTREQIT
          UPD_ICDTXT_ZIREQIT
                      = UPD_ICDTXT_ZIREQIT
        TABLES
          ICDTXT_ZIREQIT
                      = ICDTXT_ZIREQIT
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
