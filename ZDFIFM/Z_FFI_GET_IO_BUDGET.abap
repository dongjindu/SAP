FUNCTION z_ffi_get_io_budget.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(AUFNR) LIKE  AUFK-AUFNR
*"  TABLES
*"      OUT STRUCTURE  ZFI_IO_BUDGET
*"----------------------------------------------------------------------
  DATA : wa_objnr LIKE aufk-objnr.
  DATA : BEGIN OF it_bpja OCCURS 0.
          INCLUDE STRUCTURE bpja.
  DATA : END OF it_bpja.

  DATA : BEGIN OF it_bpge OCCURS 0.
          INCLUDE STRUCTURE bpge.
  DATA : END OF it_bpge.


  CLEAR : wa_objnr.
  SELECT SINGLE objnr INTO wa_objnr
  FROM aufk
  WHERE aufnr EQ aufnr.
*---Annual
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpja
  FROM bpja
  WHERE lednr EQ '0001'
  AND   objnr EQ wa_objnr.
*--Overall
  SELECT * INTO CORRESPONDING FIELDS OF TABLE it_bpge
  FROM bpge
  WHERE lednr EQ '0001'
  AND   objnr EQ wa_objnr.

  REFRESH : OUT.
  CLEAR   : OUT.
*---Annual
  LOOP AT it_bpja.
    MOVE AUFNR    TO OUT-AUFNR.
    MOVE IT_BPJA-GJAHR TO OUT-GJAHR.
    CASE it_bpja-vorga.
      WHEN 'KSTP'.        "Plan
           MOVE it_bpja-wtjhr TO OUT-PLAN.
      WHEN 'KBUD'.        "Ori Budget
           MOVE it_bpja-wtjhr TO OUT-ORG.
      WHEN 'KBN0'.        "Supplement
           MOVE it_bpja-wtjhr TO OUT-SUPP.
      WHEN 'KBR0'.        "Return
           MOVE it_bpja-wtjhr TO OUT-RET.
    ENDCASE.
    COLLECT OUT.
    CLEAR   OUT.
  ENDLOOP.
*-------------------Overall data process
  LOOP AT it_bpge.
    MOVE AUFNR    TO OUT-AUFNR.
    MOVE '1111' TO OUT-GJAHR.
    CASE it_bpge-vorga.
      WHEN 'KSTP'.        "Plan
        MOVE it_bpge-wtges  TO  OUT-PLAN.
      WHEN 'KBUD'.        "Ori Budget
        MOVE it_bpge-wtges  TO  OUT-ORG.
      WHEN 'KBN0'.        "Supplement
        MOVE it_bpge-wtges  TO  OUT-SUPP.
      WHEN 'KBR0'.        "Return
        MOVE it_bpge-wtges  TO  OUT-RET.
    ENDCASE.
    COLLECT OUT.
    CLEAR   OUT.
  ENDLOOP.


ENDFUNCTION.
