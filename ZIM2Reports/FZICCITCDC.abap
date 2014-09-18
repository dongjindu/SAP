FORM CD_CALL_ZICCIT                        .
   IF   ( UPD_ZTIVIT                         NE SPACE )
     OR ( UPD_ICDTXT_ZICCIT          NE SPACE )
   .
     CALL FUNCTION 'ZICCIT_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIVIT
                      = *ZTIVIT
                  N_ZTIVIT
                      = ZTIVIT
                  UPD_ZTIVIT
                      = UPD_ZTIVIT
                  UPD_ICDTXT_ZICCIT
                      = UPD_ICDTXT_ZICCIT
          TABLES  ICDTXT_ZICCIT
                      = ICDTXT_ZICCIT
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
