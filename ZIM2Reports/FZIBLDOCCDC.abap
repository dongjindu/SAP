FORM CD_CALL_ZIBLDOC                       .
  IF   ( UPD_ZTBL                           NE SPACE )
    OR ( UPD_ICDTXT_ZIBLDOC         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZIBLDOC_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTBL
                      = *ZTBL
          N_ZTBL
                      = ZTBL
          UPD_ZTBL
                      = UPD_ZTBL
          UPD_ICDTXT_ZIBLDOC
                      = UPD_ICDTXT_ZIBLDOC
        TABLES
          ICDTXT_ZIBLDOC
                      = ICDTXT_ZIBLDOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
