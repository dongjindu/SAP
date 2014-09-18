FORM CD_CALL_ZTIMIMG00                     .
   IF   ( UPD_ZTIMIMG00                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG00       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG00_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG00
                      = *ZTIMIMG00
                  N_ZTIMIMG00
                      = ZTIMIMG00
                  UPD_ZTIMIMG00
                      = UPD_ZTIMIMG00
                  UPD_ICDTXT_ZTIMIMG00
                      = UPD_ICDTXT_ZTIMIMG00
          TABLES  ICDTXT_ZTIMIMG00
                      = ICDTXT_ZTIMIMG00
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
