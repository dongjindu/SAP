*----------------------------------------------------------------------*
*   INCLUDE ZIPP109L_MITU_ORDER_F01                                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BASIC_DATA_SELECT
*&---------------------------------------------------------------------*
FORM basic_data_select.
  PERFORM atinn_conversion USING 'P_MITU'
                           CHANGING p_atinn P_ATFOR.

*READ VIN MASTER
**Changed by Furong on 04/03/09, Requested by Daniel Kim
*  SELECT objek FROM ausp
*               INTO TABLE it_objek
*               WHERE atinn = p_atinn
*                 AND atwrt = 'Y'.

  SELECT objek FROM ausp
               INTO TABLE it_objek
               WHERE atinn = p_atinn
                 and KLART = '002'
                 AND atwrt = 'Y'.

** End of change
  CLEAR p_atinn.
ENDFORM.                    " BASIC_DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  ATINN_CONVERSION
*&---------------------------------------------------------------------*
FORM atinn_conversion USING    P_ATNAM
                      CHANGING p_atinn P_ATFOR.


  CLEAR P_ATWRT.
  SELECT SINGLE ATINN
                ATFOR
                INTO (P_ATINN, P_ATFOR)
            FROM CABN
            WHERE ATNAM EQ P_ATNAM.


* CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
*       EXPORTING
*            input  = p_0006
*       IMPORTING
*            output = p_atinn.

ENDFORM.                    " ATINN_CONVERSION
*&---------------------------------------------------------------------*
*&      Form  DATA_JOIN
*&---------------------------------------------------------------------*
FORM data_join.
*CHARACTERISTIC
  DATA l_atwrt(14).
  LOOP AT it_objek.
    it_t07cw-plnt = '1'.                               "PLANT
    it_t07cw-line = '1'.                               "PRODUCTION LINE
    PERFORM atinn_conversion USING 'P_MODEL'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-modl(3).  "MODEL CODE
    PERFORM atinn_conversion USING 'P_BODY_SERIAL'
                                CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING l_atwrt(6).
* CONCATENATE it_t07cw-modl(3) l_atwrt(6) INTO it_t07cw-vhno. "BODYCODE
    MOVE IT_OBJEK-OBJEK TO IT_T07CW-VHNO.                "BODYCODE
    PERFORM atinn_conversion USING 'P_WORK_ORDER'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING l_atwrt.
    it_t07cw-dist = l_atwrt+9(5).                       "WORKORDER-DIST
    it_t07cw-ordr = l_atwrt(9).     CLEAR l_atwrt.       "WORKORDER-NO
    PERFORM atinn_conversion USING 'P_EXT_COLOR'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                           CHANGING it_t07cw-extc.    "WORKORDER-CO(EXT)
    PERFORM atinn_conversion USING 'P_INT_COLOR'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                           CHANGING it_t07cw-intc.    "WORKORDER-CO(INT)
    PERFORM atinn_conversion USING 'P_MI'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-bmdl.     "MODEL INDEX
    PERFORM atinn_conversion USING 'P_OCN'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-ocnn.     "OCN
    PERFORM atinn_conversion USING 'P_VERSION'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-vers.     "VERSION
    PERFORM atinn_conversion USING 'P_SEQUENCE_DATE'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-seqd .
*                              CHANGING l_atwrt(8).
*    it_t07cw-seqd = l_atwrt(8).  CLEAR l_atwrt.          "SEQUENCE DATE
    PERFORM atinn_conversion USING 'P_SEQUENCE_SERIAL'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-seqs. "SEQUENCE SERIALNO
    PERFORM atinn_conversion USING 'P_SEQUENCE_CODE'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-sqcd.    "SEQUENCE CODE
    PERFORM atinn_conversion USING 'P_MITU_DATE'
                             CHANGING p_atinn p_atfor.
    PERFORM vin_master_select USING p_atinn p_atfor
                              CHANGING it_t07cw-midt.
*                              CHANGING l_atwrt(8).
*    it_t07cw-midt = l_atwrt(8). CLEAR l_atwrt.
"HOLDING(MITU)DATE
    it_t07cw-zuser = sy-uname.                        "CREATED USERID
    it_t07cw-zsdat = sy-datum.                   "SEND FILE CREATED DATE
    it_t07cw-zstim  = sy-uzeit.                  "SEND FILE CREATED TIME
    APPEND it_t07cw. CLEAR it_t07cw.

  ENDLOOP.
ENDFORM.                    " DATA_JOIN
*&---------------------------------------------------------------------*
*&      Form  VIN_MASTER_SELECT
*&---------------------------------------------------------------------*
FORM vin_master_select USING    p_atinn p_atfor
                       CHANGING p_atwrt.
 DATA:   L_ATWRT   TYPE   AUSP-ATWRT,
         L_ATFLV   TYPE   AUSP-ATFLV,
         L_NUMERIC(15) TYPE N,
         L_INTEGER     TYPE I.
*  SELECT SINGLE atwrt FROM ausp
*               INTO p_atwrt
*               WHERE objek = it_objek-objek
*                 AND atinn = p_atinn.

      SELECT SINGLE ATWRT
                    ATFLV
                  INTO (L_ATWRT, L_ATFLV)
                  FROM AUSP
                  WHERE OBJEK EQ it_objek-objek
                    AND ATINN EQ P_ATINN
                    AND KLART EQ '002'.

 IF P_ATFOR EQ 'CHAR'.
    CONDENSE p_ATWRT.
    P_ATWRT = L_ATWRT.
 ELSE.
    L_NUMERIC = L_ATFLV.
    L_INTEGER = L_NUMERIC.
    WRITE L_INTEGER TO P_ATWRT LEFT-JUSTIFIED NO-GROUPING.
 ENDIF.
  CLEAR: p_atinn, P_ATFOR.
ENDFORM.                    " VIN_MASTER_SELECT
*&---------------------------------------------------------------------*
*&      Form  TABLE_UPDATE
*&---------------------------------------------------------------------*
FORM table_update.
  DATA:lt_t07cw LIKE ztpp_pmt07cw OCCURS 0 WITH HEADER LINE.
  data: l_text(60) type c,
        L_INT TYPE I.
  LOOP AT it_pmt07cw_rfc.
    IF it_pmt07cw_rfc-zzret = 'E'.
      lt_t07cw = it_pmt07cw_rfc.
      APPEND lt_t07cw. CLEAR lt_t07cw.
    ENDIF.
  ENDLOOP.
*DELETE ALL
  DELETE FROM ztpp_pmt07cw CLIENT SPECIFIED
         WHERE mandt EQ sy-mandt.
*INSERT NEW ENTRY
  INSERT ztpp_pmt07cw FROM TABLE it_t07cw ACCEPTING DUPLICATE KEYS.
  IF sy-subrc = 0.
    COMMIT WORK.
    describe table it_t07cw lines l_INT.
    WRITE L_INT TO L_TEXT LEFT-JUSTIFIED.
    concatenate 'Created Record Count :' l_text
      into l_text.
    message s001 with l_text.
    message s001 with text-101.
*   WRITE : / TEXT-101.
  ELSE.
    ROLLBACK WORK.
    message w001 with text-102.
*   WRITE : / TEXT-102.
  ENDIF.
ENDFORM.                    " TABLE_UPDATE
*&---------------------------------------------------------------------*
*&      Form  TO_LEGACY
*&---------------------------------------------------------------------*
FORM to_legacy.
  DATA l_msgtxt(100).
*USING RFC (ERORR CHECK)
  it_pmt07cw_rfc[] = it_t07cw[].
*CALL RFC FUNCTION
  CALL FUNCTION 'Z_FPP_SET_PMT07CW'
    DESTINATION c_dest
    TABLES
      t_ztpp_pmt07cw       = it_pmt07cw_rfc
  EXCEPTIONS
   communication_failure = 1  MESSAGE l_msgtxt
   system_failure        = 2  MESSAGE l_msgtxt.

  IF sy-subrc <> 0.
*      MESSAGE I001 WITH '  Failed in transmission to EAI'.
    MESSAGE s001 WITH l_msgtxt.
  ELSE.
    MESSAGE s001 WITH TEXT-001.
  ENDIF.
ENDFORM.                    " TO_LEGACY
*&---------------------------------------------------------------------*
*&      Form  ERROR_DATA_SELECT
*&---------------------------------------------------------------------*
FORM error_data_select.
  SELECT * FROM ztpp_pmt07cw
           INTO TABLE it_t07cw.
  IF it_t07cw[] IS INITIAL.
    MESSAGE e001 WITH TEXT-002.
  ENDIF.
ENDFORM.                    " ERROR_DATA_SELECT
*&---------------------------------------------------------------------*
*&      Form  DATA_WRITE
*&---------------------------------------------------------------------*
FORM data_write.
  FORMAT RESET.
*  IF NOT p_check IS INITIAL.
    WRITE:/5 'REPROCESSING' COLOR 6 INVERSE ON.
*  ELSE.
*    WRITE:/5 'NOMAL PROCESSING' COLOR 1 INVERSE ON.
*  ENDIF.
  WRITE:/3 'IMPORT DATA'.
  LOOP AT it_t07cw.
    WRITE:/3 it_t07cw.
  ENDLOOP.

  SKIP 2.
  WRITE:/3 'EXPORT DATA'.
  LOOP AT it_pmt07cw_rfc.
    IF it_pmt07cw_rfc-zzret = 'S'.
      WRITE:/1 it_pmt07cw_rfc-zzret COLOR 6 INVERSE ON,
            /3 it_pmt07cw_rfc.
    ELSEIF it_pmt07cw_rfc-zzret = 'E'.
      WRITE:/1 it_pmt07cw_rfc-zzret COLOR 1 INVERSE ON,
            /3 it_pmt07cw_rfc.
    ELSE.
      WRITE:/3 it_pmt07cw_rfc.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " DATA_WRITE
