FORM CD_CALL_ZIINBDOC                      .
  IF   ( UPD_ZTINSB                         NE SPACE )
    OR ( UPD_ZTINSBRSP                      NE SPACE )
    OR ( UPD_ZTINSBSG3                      NE SPACE )
    OR ( UPD_ICDTXT_ZIINBDOC        NE SPACE )
  .
    CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
    CALL FUNCTION 'ZIINBDOC_WRITE_DOCUMENT       ' IN UPDATE TASK
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
          O_ZTINSB
                      = *ZTINSB
          N_ZTINSB
                      = ZTINSB
          UPD_ZTINSB
                      = UPD_ZTINSB
          O_ZTINSBRSP
                      = *ZTINSBRSP
          N_ZTINSBRSP
                      = ZTINSBRSP
          UPD_ZTINSBRSP
                      = UPD_ZTINSBRSP
          O_ZTINSBSG3
                      = *ZTINSBSG3
          N_ZTINSBSG3
                      = ZTINSBSG3
          UPD_ZTINSBSG3
                      = UPD_ZTINSBSG3
          UPD_ICDTXT_ZIINBDOC
                      = UPD_ICDTXT_ZIINBDOC
        TABLES
          ICDTXT_ZIINBDOC
                      = ICDTXT_ZIINBDOC
    .
  ENDIF.
  CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
