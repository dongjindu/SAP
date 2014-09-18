FORM CD_CALL_ZTIMIMG21                     .
  IF   ( UPD_ZTIMIMG21                      NE SPACE )
    OR ( UPD_ICDTXT_ZTIMIMG21       NE SPACE )
  .
    CALL FUNCTION 'ZTIMIMG21_WRITE_DOCUMENT      ' IN UPDATE TASK
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
          O_ZTIMIMG21
                      = *ZTIMIMG21
          N_ZTIMIMG21
                      = ZTIMIMG21
          UPD_ZTIMIMG21
                      = UPD_ZTIMIMG21
          UPD_ICDTXT_ZTIMIMG21
                      = UPD_ICDTXT_ZTIMIMG21
        TABLES
          ICDTXT_ZTIMIMG21
                      = ICDTXT_ZTIMIMG21
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
