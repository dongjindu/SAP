*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120061532 0000290373                     $*
*$------------------------------------------------------------------$*
*$ Valid for       :                                                  $*
*$ Software Component   SAP_APPL   SAP Application                    $*
*$  Release 40A          To SAPKH40A25                                $*
*$  Release 40B          All Support Package Levels                   $*
*$  Release 45A          All Support Package Levels                   $*
*$  Release 45B          All Support Package Levels                   $*
*$  Release 46A          To SAPKH46A34                                $*
*$  Release 46B          All Support Package Levels                   $*
*$  Release 46C          All Support Package Levels                   $*
*$------------------------------------------------------------------$*
*$ Changes/Objects Not Contained in Standard SAP System               $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZAIMBPUP
*& Object Header   PROG ZAIMBPUP
*&-------------------------------------------------------------------*
*& REPORT ZAIMBPUP
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
REPORT ZAIMBPUP MESSAGE-ID AP.

TABLES: IMPR, IMTP, BPGE, TAI08.

SELECTION-SCREEN BEGIN OF BLOCK IPG.
PARAMETERS: PROGRAM  LIKE IMTP-PRNAM
                     MEMORY ID IMT,
            POSITION LIKE IMPR-POSID,
            APP_YEAR LIKE IMTP-GJAHR
                     MEMORY ID GJR.
SELECTION-SCREEN END   OF BLOCK IPG.
SELECTION-SCREEN SKIP 1.
PARAMETERS: TEXT LIKE BPBK-SGTEXT DEFAULT 'rollup'.
SELECTION-SCREEN SKIP 1.
PARAMETERS: FROMLEAF AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN SKIP 1.
PARAMETERS: BUDGET RADIOBUTTON GROUP BP,
            PLAN RADIOBUTTON GROUP BP.
SELECT-OPTIONS: SO_VERSN FOR BPGE-VERSN.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: SO_CATEG FOR TAI08-IPPOS.

AT SELECTION-SCREEN ON BLOCK IPG.

*  Programmdefinition O.K.?
  SELECT SINGLE * FROM IMTP
    WHERE PRNAM = PROGRAM
    AND   GJAHR = APP_YEAR.
  IF SY-SUBRC <> 0.
    MESSAGE E003 WITH PROGRAM APP_YEAR.
  ENDIF.

*  Position O.K.?
  IF NOT POSITION IS INITIAL.
    SELECT * FROM IMPR
      WHERE PRNAM = PROGRAM
      AND   POSID = POSITION
      AND   GJAHR = APP_YEAR.
      EXIT.
    ENDSELECT.
    IF SY-SUBRC <> 0.
      MESSAGE E005 WITH PROGRAM POSITION APP_YEAR.
    ENDIF.
  ENDIF.

*  Enqueue.
  CALL FUNCTION 'AIPE_ENQUEUE_INVPROG'
       EXPORTING
            I_PRNAM        = PROGRAM
            I_GJAHR        = APP_YEAR
            I_FLG_SHARED   = ' '
            I_MSG_TYPE     = 'E'
       EXCEPTIONS
            FOREIGN_LOCK   = 1
            SYSTEM_FAILURE = 2
            OTHERS         = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


START-OF-SELECTION.

  SUBMIT ZAIMEWMS AND RETURN
         WITH PROGRAM  =  PROGRAM
         WITH POSITION =  POSITION
         WITH APP_YEAR =  APP_YEAR
         WITH TEXT     =  TEXT
         WITH BUDGET   =  BUDGET
         WITH PLAN     =  PLAN
         WITH SO_VERSN IN SO_VERSN
         WITH SO_CATEG IN SO_CATEG
         WITH ROLLUP   =  'X'
         WITH FROMLEAF =  FROMLEAF
         WITH ORIGINAL =  ' '.

  MESSAGE I012(AP).

END-OF-SELECTION.
*>>>> END OF INSERTION <<<<<<
  ...
