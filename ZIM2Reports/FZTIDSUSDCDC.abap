FORM CD_CALL_ZTIDSUSD                      .
   IF   ( UPD_ZTIDSUSD                       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIDSUSD_WRITE_DOCUMENT       ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIDSUSD
                      = *ZTIDSUSD
                  N_ZTIDSUSD
                      = ZTIDSUSD
                  UPD_ZTIDSUSD
                      = UPD_ZTIDSUSD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
