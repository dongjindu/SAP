FORM CD_CALL_ZTIDSHSD                      .
   IF   ( UPD_ZTIDSHSD                       NE SPACE )
     OR ( UPD_ICDTXT_ZTIDSHSD        NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIDSHSD_WRITE_DOCUMENT       ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIDSHSD
                      = *ZTIDSHSD
                  N_ZTIDSHSD
                      = ZTIDSHSD
                  UPD_ZTIDSHSD
                      = UPD_ZTIDSHSD
                  UPD_ICDTXT_ZTIDSHSD
                      = UPD_ICDTXT_ZTIDSHSD
          TABLES  ICDTXT_ZTIDSHSD
                      = ICDTXT_ZTIDSHSD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
