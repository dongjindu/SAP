FORM CD_CALL_ZTIMIMG01                     .
   IF   ( UPD_ZTIMIMG01                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG01       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG01_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG01
                      = *ZTIMIMG01
                  N_ZTIMIMG01
                      = ZTIMIMG01
                  UPD_ZTIMIMG01
                      = UPD_ZTIMIMG01
                  UPD_ICDTXT_ZTIMIMG01
                      = UPD_ICDTXT_ZTIMIMG01
          TABLES  ICDTXT_ZTIMIMG01
                      = ICDTXT_ZTIMIMG01
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
