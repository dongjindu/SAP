FORM CD_CALL_ZILODOC                       .
  IF   ( UPD_ZTLLCHD                        NE SPACE )
    OR ( UPD_ZTLLCSG23                      NE SPACE )
    OR ( UPD_ZTREQHD                        NE SPACE )
    OR ( UPD_ZTREQST                        NE SPACE )
    OR ( UPD_ICDTXT_ZILODOC         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZILODOC_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTLLCHD
                      = *ZTLLCHD
          N_ZTLLCHD
                      = ZTLLCHD
          UPD_ZTLLCHD
                      = UPD_ZTLLCHD
          O_ZTLLCSG23
                      = *ZTLLCSG23
          N_ZTLLCSG23
                      = ZTLLCSG23
          UPD_ZTLLCSG23
                      = UPD_ZTLLCSG23
          O_ZTREQHD
                      = *ZTREQHD
          N_ZTREQHD
                      = ZTREQHD
          UPD_ZTREQHD
                      = UPD_ZTREQHD
          O_ZTREQST
                      = *ZTREQST
          N_ZTREQST
                      = ZTREQST
          UPD_ZTREQST
                      = UPD_ZTREQST
          UPD_ICDTXT_ZILODOC
                      = UPD_ICDTXT_ZILODOC
        TABLES
          ICDTXT_ZILODOC
                      = ICDTXT_ZILODOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
