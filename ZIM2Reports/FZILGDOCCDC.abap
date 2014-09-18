FORM CD_CALL_ZILGDOC                       .
  IF   ( UPD_ZTLG                           NE SPACE )
    OR ( UPD_ICDTXT_ZILGDOC         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZILGDOC_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTLG
                      = *ZTLG
          N_ZTLG
                      = ZTLG
          UPD_ZTLG
                      = UPD_ZTLG
          UPD_ICDTXT_ZILGDOC
                      = UPD_ICDTXT_ZILGDOC
        TABLES
          ICDTXT_ZILGDOC
                      = ICDTXT_ZILGDOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
