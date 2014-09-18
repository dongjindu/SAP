FORM CD_CALL_ZIOFDOC                       .
  IF   ( UPD_ZTOFF                          NE SPACE )
    OR ( UPD_ZTOFFFTX                       NE SPACE )
    OR ( UPD_ICDTXT_ZIOFDOC         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZIOFDOC_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTOFF
                      = *ZTOFF
          N_ZTOFF
                      = ZTOFF
          UPD_ZTOFF
                      = UPD_ZTOFF
          O_ZTOFFFTX
                      = *ZTOFFFTX
          N_ZTOFFFTX
                      = ZTOFFFTX
          UPD_ZTOFFFTX
                      = UPD_ZTOFFFTX
          UPD_ICDTXT_ZIOFDOC
                      = UPD_ICDTXT_ZIOFDOC
        TABLES
          ICDTXT_ZIOFDOC
                      = ICDTXT_ZIOFDOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
