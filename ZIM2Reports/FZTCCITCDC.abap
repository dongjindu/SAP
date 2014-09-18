FORM CD_CALL_ZTCCIT                        .
   IF   ( UPD_ZTCCIT                         NE SPACE )
     OR ( UPD_ICDTXT_ZTCCIT          NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTCCIT_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTCCIT
                      = *ZTCCIT
                  N_ZTCCIT
                      = ZTCCIT
                  UPD_ZTCCIT
                      = UPD_ZTCCIT
                  UPD_ICDTXT_ZTCCIT
                      = UPD_ICDTXT_ZTCCIT
          TABLES  ICDTXT_ZTCCIT
                      = ICDTXT_ZTCCIT
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
