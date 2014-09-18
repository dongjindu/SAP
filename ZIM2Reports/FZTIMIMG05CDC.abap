FORM CD_CALL_ZTIMIMG05                     .
  IF   ( UPD_ZTIMIMG05                      NE SPACE )
    OR ( UPD_ICDTXT_ZTIMIMG05       NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZTIMIMG05_WRITE_DOCUMENT      ' IN UPDATE TASK
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
          O_ZTIMIMG05
                      = *ZTIMIMG05
          N_ZTIMIMG05
                      = ZTIMIMG05
          UPD_ZTIMIMG05
                      = UPD_ZTIMIMG05
          UPD_ICDTXT_ZTIMIMG05
                      = UPD_ICDTXT_ZTIMIMG05
        TABLES
          ICDTXT_ZTIMIMG05
                      = ICDTXT_ZTIMIMG05
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
