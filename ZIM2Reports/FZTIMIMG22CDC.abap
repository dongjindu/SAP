FORM CD_CALL_ZTIMIMG22                     .
  IF   ( UPD_ZTIMIMG22                      NE SPACE )
    OR ( UPD_ICDTXT_ZTIMIMG22       NE SPACE )
  .
    CALL FUNCTION 'ZTIMIMG22_WRITE_DOCUMENT      ' IN UPDATE TASK
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
          O_ZTIMIMG22
                      = *ZTIMIMG22
          N_ZTIMIMG22
                      = ZTIMIMG22
          UPD_ZTIMIMG22
                      = UPD_ZTIMIMG22
          UPD_ICDTXT_ZTIMIMG22
                      = UPD_ICDTXT_ZTIMIMG22
        TABLES
          ICDTXT_ZTIMIMG22
                      = ICDTXT_ZTIMIMG22
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
