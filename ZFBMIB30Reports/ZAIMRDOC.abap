*$*$--------------------------------------------------------------$*$*
*$ Correction Inst.         0120024545 0000160400                     $*
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
*$  Release MERCURY      w/o Support Packages                         $*
*$*$--------------------------------------------------------------$*$*
*&-------------------------------------------------------------------*
*& Object          REPS ZAIMRDOC
*& Object Header   PROG ZAIMRDOC
*&-------------------------------------------------------------------*
*& REPORT ZAIMRDOC
*&-------------------------------------------------------------------*
*>>>> START OF INSERTION <<<<
*--------------------------------------------------------------------*
* Attributes:
* Type = 1 / Executable Program,
* Application = A / Asset Management.
*--------------------------------------------------------------------*
REPORT ZAIMRDOC.
*--------------------------------------------------------------------*
* The report reverses a document created in program planning
* or budgeting. It does not reverse any other documents,
* not even documents created in budget distribution.
*--------------------------------------------------------------------*
* Program RAIMDNEW has to be run afterwards
* (may be run with NO_LEAFS = X).
*--------------------------------------------------------------------*
INCLUDE LKBPPEQU.

TABLES: BPBK, BPEG, BPEJ, BPTR.

DATA: TAB_BPEG LIKE BPEG OCCURS 100 WITH HEADER LINE.
DATA: TAB_BPEJ LIKE BPEJ OCCURS 100 WITH HEADER LINE.
DATA: SUBRC    LIKE SY-SUBRC.

PARAMETERS: DOC_NR   LIKE BPBK-BELNR,
            ITEMTEXT LIKE BPBK-SGTEXT DEFAULT 'Reversal',
            TEST_RUN AS CHECKBOX.

INITIALIZATION.
  MOVE 'X' TO TEST_RUN.

START-OF-SELECTION.

  SELECT SINGLE * FROM BPBK
    WHERE BELNR = DOC_NR.

  IF SY-SUBRC <> 0.
     MESSAGE E208(00) WITH
       'document not found'.                     "#EC NOTEXT
     STOP.
  ENDIF.
  SELECT * FROM BPEG INTO TABLE TAB_BPEG
    WHERE BELNR = DOC_NR.
  SELECT * FROM BPEJ INTO TABLE TAB_BPEJ
    WHERE BELNR = DOC_NR.

  PERFORM IM_CHECK TABLES   TAB_BPEG
                            TAB_BPEJ
                   CHANGING SUBRC.
  IF SUBRC = 4.
     MESSAGE E208(00) WITH
       'document can not be reversed'.           "#EC NOTEXT
     STOP.
  ENDIF.

  IF TEST_RUN = SPACE.
     PERFORM REVERSE_DOCUMENT TABLES TAB_BPEG
                                     TAB_BPEJ
                              USING  BPBK.
  ENDIF.

  IF TEST_RUN = SPACE.
     MESSAGE I208(00) WITH
       'document reversed'.                      "#EC NOTEXT
  ELSE.
     MESSAGE I208(00) WITH
       'document can be reversed'.               "#EC NOTEXT
  ENDIF.

END-OF-SELECTION.

FORM REVERSE_DOCUMENT TABLES IT_BPEG       STRUCTURE BPEG
                             IT_BPEJ       STRUCTURE BPEJ
                      USING  VALUE(I_BPBK) LIKE BPBK.

  DATA: BEGIN OF T_DUMMY OCCURS 1,
          DUMMYFIELD,
        END   OF T_DUMMY.

  DATA: T_BPGE_ID LIKE BPGE_ID OCCURS 100 WITH HEADER LINE,
        T_BPJA_ID LIKE BPJA_ID OCCURS 100 WITH HEADER LINE,
        T_BPTR_ID LIKE BPTR_ID OCCURS 100 WITH HEADER LINE,
        T_BPGE    LIKE BPGE    OCCURS 100 WITH HEADER LINE,
        T_BPJA    LIKE BPJA    OCCURS 100 WITH HEADER LINE,
        T_BPTR    LIKE BPTR    OCCURS 100 WITH HEADER LINE.

  DATA: L_BPIN    LIKE BPIN.

  CHECK NOT IT_BPEG[] IS INITIAL OR
        NOT IT_BPEJ[] IS INITIAL .

  LOOP AT IT_BPEG.
    CLEAR T_BPGE_ID.
    MOVE-CORRESPONDING IT_BPEG TO T_BPGE_ID.
    T_BPGE_ID-WLGES = 0 - IT_BPEG-WLGES.
    T_BPGE_ID-WTGES = 0 - IT_BPEG-WTGES.
    T_BPGE_ID-ID    = 'U'.
    T_BPGE_ID-EPOS  = 'X'.
    COLLECT T_BPGE_ID.
    CLEAR T_BPTR_ID.
    MOVE-CORRESPONDING T_BPGE_ID TO T_BPTR_ID.
    COLLECT T_BPTR_ID.
  ENDLOOP.

  LOOP AT IT_BPEJ.
    CLEAR T_BPJA_ID.
    MOVE-CORRESPONDING IT_BPEJ TO T_BPJA_ID.
    T_BPJA_ID-WLJHR = 0 - IT_BPEJ-WLJHR.
    T_BPJA_ID-WTJHR = 0 - IT_BPEJ-WTJHR.
    T_BPJA_ID-ID    = 'U'.
    T_BPJA_ID-EPOS  = 'X'.
    COLLECT T_BPJA_ID.
    CLEAR T_BPTR_ID.
    MOVE-CORRESPONDING T_BPJA_ID TO T_BPTR_ID.
    COLLECT T_BPTR_ID.
  ENDLOOP.

* Adjust BPTR entries if necessary.
  IF NOT T_BPTR_ID[] IS INITIAL.
     SELECT * FROM BPTR INTO TABLE T_BPTR
       FOR ALL ENTRIES IN T_BPTR_ID
       WHERE OBJNR = T_BPTR_ID-OBJNR
       AND   POSIT = T_BPTR_ID-POSIT
       AND   TRGKZ = T_BPTR_ID-TRGKZ
       AND   WRTTP = T_BPTR_ID-WRTTP
       AND   GEBER = T_BPTR_ID-GEBER
       AND   VERSN = T_BPTR_ID-VERSN.
     SORT T_BPTR BY OBJNR
                    POSIT
                    TRGKZ
                    WRTTP
                    GEBER
                    VERSN.
     LOOP AT T_BPTR_ID.
       READ TABLE T_BPTR
            WITH KEY OBJNR = T_BPTR_ID-OBJNR
                     POSIT = T_BPTR_ID-POSIT
                     TRGKZ = T_BPTR_ID-TRGKZ
                     WRTTP = T_BPTR_ID-WRTTP
                     GEBER = T_BPTR_ID-GEBER
                     VERSN = T_BPTR_ID-VERSN
       BINARY SEARCH.
       IF SY-SUBRC = 0.
          T_BPTR_ID-ERNAM = T_BPTR-ERNAM.
          T_BPTR_ID-AENAM = SY-UNAME.
          T_BPTR_ID-ERDAT = T_BPTR-ERDAT.
          T_BPTR_ID-AEDAT = SY-DATLO.
       ELSE.
          T_BPTR_ID-ERNAM = SY-UNAME.
          CLEAR T_BPTR_ID-AENAM.
          T_BPTR_ID-ERDAT = SY-DATLO.
          CLEAR T_BPTR_ID-AEDAT.
          T_BPTR_ID-ID    = 'I'.
       ENDIF.
       MODIFY T_BPTR_ID.
     ENDLOOP.
  ENDIF.

* Adjust BPGE entries if necessary.
  IF NOT T_BPGE_ID[] IS INITIAL.
     SELECT * FROM BPGE INTO TABLE T_BPGE
       FOR ALL ENTRIES IN T_BPGE_ID
       WHERE LEDNR = T_BPGE_ID-LEDNR
       AND   OBJNR = T_BPGE_ID-OBJNR
       AND   POSIT = T_BPGE_ID-POSIT
       AND   TRGKZ = T_BPGE_ID-TRGKZ
       AND   WRTTP = T_BPGE_ID-WRTTP
       AND   GEBER = T_BPGE_ID-GEBER
       AND   VERSN = T_BPGE_ID-VERSN
       AND   VORGA = T_BPGE_ID-VORGA
       AND   TWAER = T_BPGE_ID-TWAER.
     SORT T_BPGE BY LEDNR
                    OBJNR
                    POSIT
                    TRGKZ
                    WRTTP
                    GEBER
                    VERSN
                    VORGA
                    TWAER.
     LOOP AT T_BPGE_ID.
       READ TABLE T_BPGE
            WITH KEY LEDNR = T_BPGE_ID-LEDNR
                     OBJNR = T_BPGE_ID-OBJNR
                     POSIT = T_BPGE_ID-POSIT
                     TRGKZ = T_BPGE_ID-TRGKZ
                     WRTTP = T_BPGE_ID-WRTTP
                     GEBER = T_BPGE_ID-GEBER
                     VERSN = T_BPGE_ID-VERSN
                     VORGA = T_BPGE_ID-VORGA
                     TWAER = T_BPGE_ID-TWAER
       BINARY SEARCH.
       IF SY-SUBRC <> 0.
          T_BPGE_ID-ID    = 'I'.
       ENDIF.
       MODIFY T_BPGE_ID.
     ENDLOOP.
  ENDIF.

* Adjust BPJA entries if necessary.
  IF NOT T_BPJA_ID[] IS INITIAL.
     SELECT * FROM BPJA INTO TABLE T_BPJA
       FOR ALL ENTRIES IN T_BPJA_ID
       WHERE LEDNR = T_BPJA_ID-LEDNR
       AND   OBJNR = T_BPJA_ID-OBJNR
       AND   POSIT = T_BPJA_ID-POSIT
       AND   TRGKZ = T_BPJA_ID-TRGKZ
       AND   WRTTP = T_BPJA_ID-WRTTP
       AND   GJAHR = T_BPJA_ID-GJAHR
       AND   GEBER = T_BPJA_ID-GEBER
       AND   VERSN = T_BPJA_ID-VERSN
       AND   VORGA = T_BPJA_ID-VORGA
       AND   TWAER = T_BPJA_ID-TWAER.
     SORT T_BPJA BY LEDNR
                    OBJNR
                    POSIT
                    TRGKZ
                    WRTTP
                    GJAHR
                    GEBER
                    VERSN
                    VORGA
                    TWAER.
     LOOP AT T_BPJA_ID.
       READ TABLE T_BPJA
            WITH KEY LEDNR = T_BPJA_ID-LEDNR
                     OBJNR = T_BPJA_ID-OBJNR
                     POSIT = T_BPJA_ID-POSIT
                     TRGKZ = T_BPJA_ID-TRGKZ
                     WRTTP = T_BPJA_ID-WRTTP
                     GJAHR = T_BPJA_ID-GJAHR
                     GEBER = T_BPJA_ID-GEBER
                     VERSN = T_BPJA_ID-VERSN
                     VORGA = T_BPJA_ID-VORGA
                     TWAER = T_BPJA_ID-TWAER
       BINARY SEARCH.
       IF SY-SUBRC <> 0.
          T_BPJA_ID-ID    = 'I'.
       ENDIF.
       MODIFY T_BPJA_ID.
     ENDLOOP.
  ENDIF.

* Something to do?
  CHECK NOT T_BPGE_ID[] IS INITIAL OR
        NOT T_BPJA_ID[] IS INITIAL OR
        NOT T_BPTR_ID[] IS INITIAL .

* Prepare posting.
  CLEAR L_BPIN.
  CASE BPBK-VORGA.
  WHEN L_VPLAN.
    L_BPIN-TCODE = 'IM35'.
  WHEN L_VBUD.
    L_BPIN-TCODE = 'IM32'.
  WHEN L_VBR0.
    L_BPIN-TCODE = 'IM38'.
  WHEN L_VBN0.
    L_BPIN-TCODE = 'IM30'.
  ENDCASE.
  L_BPIN-DELTA = 'X'.
  L_BPIN-EPOS  = 'X'.

  CALL FUNCTION 'KBPS_INIT'
       EXPORTING
            BP_IMPORT = L_BPIN
       IMPORTING
            BP_IN     = L_BPIN
       EXCEPTIONS
            OTHERS    = 1.

  L_BPIN-SGTEXT = ITEMTEXT.

* Verbuchung.
  CALL FUNCTION 'KBPV_POST_DATA'
       EXPORTING
            DELTA_UPDATE  = 'X'
            DIALOG_UPDATE = 'X'     " Nicht in Update-Task!
            IM_BPIN       = L_BPIN
       TABLES
            TAB_BPCH      = T_DUMMY
            TAB_BPGE      = T_BPGE_ID
            TAB_BPHI      = T_DUMMY
            TAB_BPIG      = T_DUMMY
            TAB_BPIJ      = T_DUMMY
            TAB_BPJA      = T_BPJA_ID
            TAB_BPPE      = T_DUMMY
            TAB_BPTR      = T_BPTR_ID
       EXCEPTIONS
            OTHERS        = 1.

  COMMIT WORK.

ENDFORM.

FORM IM_CHECK TABLES   IT_BPEG STRUCTURE BPEG
                       IT_BPEJ STRUCTURE BPEJ
              CHANGING E_SUBRC LIKE SY-SUBRC.

  DATA: L_OBART LIKE IMPS-OBART.

* Beleg darf nur Programmpositionen enthalten!
  E_SUBRC = 0.
*
  IF E_SUBRC = 0.
     LOOP AT IT_BPEG.
       CALL FUNCTION 'OBJECT_NUMBER_TYPE_GET'
            EXPORTING
                 OBJNR = IT_BPEG-OBJNR
            IMPORTING
                 OBART = L_OBART.
       IF L_OBART <> 'IP'.
          E_SUBRC = 4.
          EXIT.
       ENDIF.
     ENDLOOP.
  ENDIF.
*
  IF E_SUBRC = 0.
     LOOP AT IT_BPEJ.
       CALL FUNCTION 'OBJECT_NUMBER_TYPE_GET'
            EXPORTING
                 OBJNR = IT_BPEJ-OBJNR
            IMPORTING
                 OBART = L_OBART.
       IF L_OBART <> 'IP'.
          E_SUBRC = 4.
          EXIT.
       ENDIF.
     ENDLOOP.
  ENDIF.

ENDFORM.
*>>>> END OF INSERTION <<<<<<
...
*&-------------------------------------------------------------------*
