FORM CD_CALL_ZTRECST                       .
  IF   ( UPD_ZTRECST                        NE SPACE )
    OR ( UPD_ICDTXT_ZTRECST         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZTRECST_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTRECST
                      = *ZTRECST
          N_ZTRECST
                      = ZTRECST
          UPD_ZTRECST
                      = UPD_ZTRECST
          UPD_ICDTXT_ZTRECST
                      = UPD_ICDTXT_ZTRECST
        TABLES
          ICDTXT_ZTRECST
                      = ICDTXT_ZTRECST
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
