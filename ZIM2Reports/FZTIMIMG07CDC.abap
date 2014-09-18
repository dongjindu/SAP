FORM CD_CALL_ZTIMIMG07                     .
   IF   ( UPD_ZTIMIMG07                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG07       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG07_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG07
                      = *ZTIMIMG07
                  N_ZTIMIMG07
                      = ZTIMIMG07
                  UPD_ZTIMIMG07
                      = UPD_ZTIMIMG07
                  UPD_ICDTXT_ZTIMIMG07
                      = UPD_ICDTXT_ZTIMIMG07
          TABLES  ICDTXT_ZTIMIMG07
                      = ICDTXT_ZTIMIMG07
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
