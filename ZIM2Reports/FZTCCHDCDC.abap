FORM CD_CALL_ZTCCHD                        .
   IF   ( UPD_ZTCCHD                         NE SPACE )
     OR ( UPD_ICDTXT_ZTCCHD          NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTCCHD_WRITE_DOCUMENT         ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTCCHD
                      = *ZTCCHD
                  N_ZTCCHD
                      = ZTCCHD
                  UPD_ZTCCHD
                      = UPD_ZTCCHD
                  UPD_ICDTXT_ZTCCHD
                      = UPD_ICDTXT_ZTCCHD
          TABLES  ICDTXT_ZTCCHD
                      = ICDTXT_ZTCCHD
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
