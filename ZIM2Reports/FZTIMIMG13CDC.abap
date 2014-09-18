FORM CD_CALL_ZTIMIMG13                     .
   IF   ( UPD_ZTIMIMG13                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG13       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG13_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG13
                      = *ZTIMIMG13
                  N_ZTIMIMG13
                      = ZTIMIMG13
                  UPD_ZTIMIMG13
                      = UPD_ZTIMIMG13
                  UPD_ICDTXT_ZTIMIMG13
                      = UPD_ICDTXT_ZTIMIMG13
          TABLES  ICDTXT_ZTIMIMG13
                      = ICDTXT_ZTIMIMG13
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
