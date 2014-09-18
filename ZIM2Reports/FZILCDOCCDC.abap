FORM CD_CALL_ZILCDOC                       .
  IF   ( UPD_ZTMLCHD                        NE SPACE )
    OR ( UPD_ZTMLCSG2                       NE SPACE )
    OR ( UPD_ZTMLCSG910                     NE SPACE )
    OR ( UPD_ZTREQHD                        NE SPACE )
    OR ( UPD_ZTREQST                        NE SPACE )
    OR ( UPD_ICDTXT_ZILCDOC         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZILCDOC_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTMLCHD
                      = *ZTMLCHD
          N_ZTMLCHD
                      = ZTMLCHD
          UPD_ZTMLCHD
                      = UPD_ZTMLCHD
          O_ZTMLCSG2
                      = *ZTMLCSG2
          N_ZTMLCSG2
                      = ZTMLCSG2
          UPD_ZTMLCSG2
                      = UPD_ZTMLCSG2
          O_ZTMLCSG910
                      = *ZTMLCSG910
          N_ZTMLCSG910
                      = ZTMLCSG910
          UPD_ZTMLCSG910
                      = UPD_ZTMLCSG910
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
          UPD_ICDTXT_ZILCDOC
                      = UPD_ICDTXT_ZILCDOC
        TABLES
          ICDTXT_ZILCDOC
                      = ICDTXT_ZILCDOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
