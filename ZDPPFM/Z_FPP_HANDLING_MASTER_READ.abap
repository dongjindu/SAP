FUNCTION Z_FPP_HANDLING_MASTER_READ.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(OBJECT) TYPE  MARA-MATNR
*"     REFERENCE(MODE) TYPE  ZRW DEFAULT 'R'
*"     REFERENCE(CTYPE) LIKE  RMCLF-KLART DEFAULT '002'
*"     REFERENCE(DISPLAY) TYPE  ZMODE DEFAULT 'D'
*"     REFERENCE(P_METHOD) TYPE  CHAR1 DEFAULT 'X'
*"  TABLES
*"      VAL_TABLE STRUCTURE  ZSPP_VIN_VALUE
*"  EXCEPTIONS
*"      NO_DATA
*"      ERROR_MODE
*"      ERROR_OBJECT
*"      ERROR_VALUE
*"----------------------------------------------------------------------
  DATA: l_ctype              LIKE rmclf-klart,
        l_object             LIKE equi-equnr ,
        l_flag               type c          ,
        L_TYPE               LIKE CABN-ATFOR ,
        r_code               TYPE c        .

  l_ctype = ctype.
  CASE L_CTYPE.
    WHEN '001'  .  "material
      SELECT SINGLE matnr INTO l_object
        FROM mara
       WHERE matnr = object.

      IF sy-subrc NE 0.
        loop at val_table.
          val_table-zflag = 'E'.
          modify val_table transporting zflag.
        endloop.
        RAISE error_object.
      ENDIF.

    WHEN '002'  .  "equipment
      SELECT SINGLE EQUNR INTO l_object
        FROM EQUI
       WHERE EQUNR = object.

      IF sy-subrc NE 0.
        loop at val_table.
          val_table-zflag = 'E'.
          modify val_table transporting zflag.
        endloop.
        RAISE error_object.
      ENDIF.
  ENDCASE.

*  PERFORM get_values     TABLES val_table  USING object  display
*                                                     L_CTYPE        .
*FORM GET_VALUES TABLES   PA_VAL_TABLE STRUCTURE IT_VALUES
*                USING    PA_OBJECT    PA_DMODE  PA_KLART  .
*
  DATA: L_VALS(8)           TYPE N            ,
        L_TIME(6)           TYPE N            ,
        L_DATE              TYPE D            ,
        L_DATA              TYPE I            ,
        L_CODE              TYPE C            .
  DATA: L_DEC               TYPE P   DECIMALS 3 .
  data: idx like sy-tabix.
  DATA: BEGIN OF IT_CABN OCCURS 0,
          ATINN like cabn-atinn,
          ATNAM like cabn-atnam,
          ATFOR like cabn-atfor,
          ANZDZ like cabn-ANZDZ,
        END OF IT_CABN.
  DATA: BEGIN OF IT_AUSP OCCURS 0,
          ATINN like ausp-atinn,
          ATWRT like ausp-ATWRT,
          atflv like ausp-atflv,
        END OF IT_AUSP.
  RANGES R_ATINN FOR AUSP-ATINN OCCURS 0.
  R_ATINN-SIGN = 'I'. R_ATINN-OPTION = 'EQ'.

  IMPORT it_cabn = it_cabn FROM MEMORY ID 'MY_CABN'.
  if sy-subrc = 0.
    sort it_cabn by atnam.
    loop at val_table.
      idx = sy-tabix.
      read table it_cabn with key atnam = val_table-atnam
           binary search.
      val_table-atinn = it_cabn-atinn.
      modify val_table index idx transporting atinn.
    endloop.

  else.
* validate characters...
    DESCRIBE TABLE VAL_TABLE LINES L_DATA.
    IF L_DATA > 0.
      LOOP AT VAL_TABLE.
        idx = sy-tabix.
        clear it_cabn.

*        SELECT atinn atnam atfor anzdz appending table it_cabn
*          FROM CABN
*         WHERE ATINN = VAL_TABLE-ATINN .
        IF VAL_TABLE-ATINN IS INITIAL AND VAL_TABLE-ATNAM IS INITIAL.
          L_CODE = 'N' .             " NULL DATA.
        ELSEIF VAL_TABLE-ATINN IS INITIAL.
          SELECT SINGLE atinn atnam atfor anzdz into it_cabn
            FROM CABN
           WHERE ATNAM =  VAL_TABLE-ATNAM
             AND DATUV >= '00000000'.
          IF SY-SUBRC NE 0.
            L_CODE = 'I' .             " INVALID ATNAM .
          ENDIF.
        ELSE.
          SELECT SINGLE atinn atnam atfor anzdz into it_cabn
            FROM CABN
           WHERE ATINN = VAL_TABLE-ATINN.
          IF SY-SUBRC NE 0.
            L_CODE = 'E' .             " INVALID ATINN .
          ENDIF.
        ENDIF.

        IF L_CODE = SPACE.
          R_ATINN-LOW  = IT_CABN-ATINN. APPEND R_ATINN.
          append it_cabn.
          val_table-atinn = it_cabn-atinn.
          modify val_table index idx.
        endif.
      endloop.
    else.
      SELECT atinn atnam atfor anzdz appending table it_cabn
        FROM CABN.
      loop at it_cabn.
        move-corresponding it_cabn to val_table. append val_table.
      endloop.
    ENDIF.

    EXPORT it_cabn FROM it_cabn TO MEMORY ID 'MY_CABN'.
  endif.

  sort it_cabn   by ATINN.


*  sort val_table by atinn.

*-read all AUSP to avoid index reading.
  if p_method = 'X'.
    SELECT atinn ATWRT atflv INTO TABLE IT_AUSP
         FROM AUSP
        WHERE OBJEK EQ OBJECT
          AND KLART = L_CTYPE
*        AND ATINN IN R_ATINN
        order by atinn.
  else.
    SELECT atinn ATWRT atflv INTO TABLE IT_AUSP
         FROM AUSP
        WHERE OBJEK EQ OBJECT
          AND KLART = L_CTYPE
*        AND ATINN IN R_ATINN
        order by atinn.
  endif.

* move value to readable format.
  IF SY-SUBRC = 0.
    loop at val_table.
      idx = sy-tabix.
      read table it_cabn with key ATINN = VAL_TABLE-ATINN binary search.
      read table it_ausp with key ATINN = VAL_TABLE-ATINN binary search.
      if sy-subrc NE 0.
        clear VAL_TABLE-ATWRT.
      else.
        CASE it_cabn-ATFOR .
          WHEN 'CHAR'.
            VAL_TABLE-ATWRT = it_AUSP-ATWRT.
          WHEN 'NUM' .
            IF it_cabn-ANZDZ EQ 0.
              VAL_TABLE-ATWRT = L_VALS = it_AUSP-ATFLV.
            ELSE.
              VAL_TABLE-ATWRT = L_DEC  = it_AUSP-ATFLV.
              CONDENSE VAL_TABLE-ATWRT .
            ENDIF.
          WHEN 'DATE'.
            L_DATE = L_VALS    = it_AUSP-ATFLV.
            IF MODE = 'D' .
              WRITE L_DATE      TO VAL_TABLE-ATWRT.
            ELSE.
              VAL_TABLE-ATWRT = L_VALS   .
            ENDIF.
*       PA_VAL_TABLE-atwrt = L_ausp-atflv.
          WHEN 'CURR'.
*         wa_ausp-atwrt = pa_atwrt.
          WHEN 'TIME'.
            VAL_TABLE-ATWRT = L_TIME = it_AUSP-ATFLV .

*         wa_ausp-atwrt = pa_atwrt.
          WHEN 'UDEF'.
*         wa_ausp-atwrt = pa_atwrt.
          WHEN OTHERS.
            VAL_TABLE-ATWRT = it_AUSP-ATWRT.
        ENDCASE.
      endif.

      MODIFY VAL_TABLE index idx.
    endloop.
  ENDIF.


ENDFUNCTION.
