FORM CD_CALL_ZIINDOC                       .
  IF   ( UPD_ZTINS                          NE SPACE )
    OR ( UPD_ZTINSRSP                       NE SPACE )
    OR ( UPD_ZTINSSG3                       NE SPACE )
    OR ( UPD_ICDTXT_ZIINDOC         NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZIINDOC_WRITE_DOCUMENT        ' IN UPDATE TASK
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
          O_ZTINS
                      = *ZTINS
          N_ZTINS
                      = ZTINS
          UPD_ZTINS
                      = UPD_ZTINS
          O_ZTINSRSP
                      = *ZTINSRSP
          N_ZTINSRSP
                      = ZTINSRSP
          UPD_ZTINSRSP
                      = UPD_ZTINSRSP
          O_ZTINSSG3
                      = *ZTINSSG3
          N_ZTINSSG3
                      = ZTINSSG3
          UPD_ZTINSSG3
                      = UPD_ZTINSSG3
          UPD_ICDTXT_ZIINDOC
                      = UPD_ICDTXT_ZIINDOC
        TABLES
          ICDTXT_ZIINDOC
                      = ICDTXT_ZIINDOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
