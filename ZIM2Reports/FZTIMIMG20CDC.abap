FORM CD_CALL_ZTIMIMG20                     .
  IF   ( UPD_ZTIMIMG20                      NE SPACE )
    OR ( UPD_ICDTXT_ZTIMIMG20       NE SPACE )
  .
    CALL FUNCTION 'ZTIMIMG20_WRITE_DOCUMENT      ' IN UPDATE TASK
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
          O_ZTIMIMG20
                      = *ZTIMIMG20
          N_ZTIMIMG20
                      = ZTIMIMG20
          UPD_ZTIMIMG20
                      = UPD_ZTIMIMG20
          UPD_ICDTXT_ZTIMIMG20
                      = UPD_ICDTXT_ZTIMIMG20
        TABLES
          ICDTXT_ZTIMIMG20
                      = ICDTXT_ZTIMIMG20
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
