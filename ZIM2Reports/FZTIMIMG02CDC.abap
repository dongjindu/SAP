FORM CD_CALL_ZTIMIMG02                     .
   IF   ( UPD_ZTIMIMG02                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG02       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG02_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG02
                      = *ZTIMIMG02
                  N_ZTIMIMG02
                      = ZTIMIMG02
                  UPD_ZTIMIMG02
                      = UPD_ZTIMIMG02
                  UPD_ICDTXT_ZTIMIMG02
                      = UPD_ICDTXT_ZTIMIMG02
          TABLES  ICDTXT_ZTIMIMG02
                      = ICDTXT_ZTIMIMG02
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
