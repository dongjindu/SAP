FORM CD_CALL_ZITTDOC                       .
  IF   ( UPD_ZTREQHD                        NE SPACE )
    OR ( UPD_ZTREQST                        NE SPACE )
    OR ( UPD_ZTTTHD                         NE SPACE )
    OR ( UPD_ICDTXT_ZITTDOC         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZITTDOC_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTTTHD
                      = *ZTTTHD
          N_ZTTTHD
                      = ZTTTHD
          UPD_ZTTTHD
                      = UPD_ZTTTHD
          UPD_ICDTXT_ZITTDOC
                      = UPD_ICDTXT_ZITTDOC
        TABLES
          ICDTXT_ZITTDOC
                      = ICDTXT_ZITTDOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
