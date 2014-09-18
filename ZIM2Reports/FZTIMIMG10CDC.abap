FORM CD_CALL_ZTIMIMG10                     .
   IF   ( UPD_ZTIMIMG10                      NE SPACE )
     OR ( UPD_ICDTXT_ZTIMIMG10       NE SPACE )
   .
     CALL FUNCTION 'SWE_REQUESTER_TO_UPDATE'.
     CALL FUNCTION 'ZTIMIMG10_WRITE_DOCUMENT      ' IN UPDATE TASK
        EXPORTING OBJECTID              = OBJECTID
                  TCODE                 = TCODE
                  UTIME                 = UTIME
                  UDATE                 = UDATE
                  USERNAME              = USERNAME
                  PLANNED_CHANGE_NUMBER = PLANNED_CHANGE_NUMBER
                  OBJECT_CHANGE_INDICATOR = CDOC_UPD_OBJECT
                  PLANNED_OR_REAL_CHANGES = CDOC_PLANNED_OR_REAL
                  NO_CHANGE_POINTERS = CDOC_NO_CHANGE_POINTERS
                  O_ZTIMIMG10
                      = *ZTIMIMG10
                  N_ZTIMIMG10
                      = ZTIMIMG10
                  UPD_ZTIMIMG10
                      = UPD_ZTIMIMG10
                  UPD_ICDTXT_ZTIMIMG10
                      = UPD_ICDTXT_ZTIMIMG10
          TABLES  ICDTXT_ZTIMIMG10
                      = ICDTXT_ZTIMIMG10
     .
   ENDIF.
   CLEAR PLANNED_CHANGE_NUMBER.
ENDFORM.
