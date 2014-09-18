FORM CD_CALL_ZTIMIMG04                     .
   IF   ( UPD_ZTIMIMG04                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG04       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG04_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG04
                      = *ZTIMIMG04
                  N_ZTIMIMG04
                      = ZTIMIMG04
                  UPD_ZTIMIMG04
                      = UPD_ZTIMIMG04
                  UPD_ICDTXT_ZTIMIMG04
                      = UPD_ICDTXT_ZTIMIMG04
          TABLES  ICDTXT_ZTIMIMG04
                      = ICDTXT_ZTIMIMG04
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
