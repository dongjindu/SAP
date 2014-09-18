FORM CD_CALL_ZTIDS                         .
   IF   ( UPD_ZTIDS                          NE SPACE )
     OR ( UPD_ICDTXT_ZTIDS           NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIDS_WRITE_DOCUMENT          ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIDS
                      = *ZTIDS
                  N_ZTIDS
                      = ZTIDS
                  UPD_ZTIDS
                      = UPD_ZTIDS
                  UPD_ICDTXT_ZTIDS
                      = UPD_ICDTXT_ZTIDS
          TABLES  ICDTXT_ZTIDS
                      = ICDTXT_ZTIDS
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
