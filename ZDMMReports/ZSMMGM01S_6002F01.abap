*----------------------------------------------------------------------*
*   INCLUDE ZSMMGM01S_6002F01                                          *
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
*       FORM make_IT_zsmm_6002_01                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM make_it_zsmm_6002_01.

  SELECT rkpf~rsnum resb~rspos resb~rsart resb~matnr
         resb~bdmng resb~meins resb~werks resb~lgort
         rkpf~umwrk rkpf~umlgo resb~bdter rkpf~rsdat
         rkpf~kostl rkpf~aufnr rkpf~anln1 resb~saknr
         rkpf~bwart rkpf~usnam
    INTO CORRESPONDING FIELDS OF TABLE it_zsmm_6002_01
    FROM rkpf
      INNER JOIN resb
      ON resb~rsnum = rkpf~rsnum     "Reservation
    WHERE rkpf~rsnum IN s_rsnum AND  "Reservation
          resb~matnr IN s_matnr AND  "Material
          rkpf~kostl IN s_kostl AND  "Cost Center
          rkpf~aufnr IN s_aufnr AND  "Order
          rkpf~anln1 IN s_anln1 AND  "Asset
          resb~saknr IN s_saknr AND  "G/L account number
          rkpf~bwart IN s_bwart AND  "Movement type
          resb~bdter IN s_bdter AND  "Requirements date
          rkpf~rsdat IN s_rsdat AND  "Base date for reservation
          resb~werks IN s_werks AND  "Issue plant
          resb~lgort IN s_lgort AND  "Issue stor. loc.
          resb~umwrk IN s_umwrk AND  "Receiving plant
          resb~umlgo IN s_umlgo AND  "Receiving stor. loc.
          rkpf~usnam IN s_usnam.     "Created by

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  mask_columns
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_it_fcat  text
*----------------------------------------------------------------------*
FORM mask_columns TABLES p_it_fcat STRUCTURE it_fcat.
* Build the fieldcat according to DDIC structure ZSMM_6002_01:
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
       EXPORTING
            i_structure_name = 'ZSMM_6002_01'
       CHANGING
            ct_fieldcat      = p_it_fcat[].

* Make Column header
  LOOP AT p_it_fcat.
    IF p_it_fcat-fieldname = 'WEMPF'.
      p_it_fcat-no_out = 'X'.
    ELSEIF p_it_fcat-fieldname = 'USNAM'.
      p_it_fcat-no_out = 'X'.
    ELSEIF p_it_fcat-fieldname = 'CHARG'.
      p_it_fcat-no_out = 'X'.
    ELSEIF p_it_fcat-fieldname = 'MATNR'.
      p_it_fcat-outputlen = 18.
*    ELSEIF p_it_fcat-fieldname = 'DESC_ZCH_DESC'.
*      p_it_fcat-checkbox = space.
*    ELSEIF p_it_fcat-fieldname = 'DESC_ZCH_REASON'.
*      p_it_fcat-coltext = 'Reason Code Name'.
    ENDIF.
    MODIFY p_it_fcat.
  ENDLOOP.

ENDFORM.                    " mask_columns
*&---------------------------------------------------------------------*
*&      Form  process_form
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_RETURN_CODE  text
*----------------------------------------------------------------------*
FORM process_form USING    value(p_rsnum)
                  CHANGING return_code TYPE i.
* call the generated function module of the form
  CALL FUNCTION func_module_name
       EXPORTING
            control_parameters = control_parameters
            output_options     = output_options
            user_settings      = space
            color              = color
            rsnum              = p_rsnum
*       TABLES
*            imt_zsmm_6002_01   = imt_zsmm_6002_01
       EXCEPTIONS
           formatting_error   = 1
           internal_error     = 2
           send_error         = 3
           user_canceled      = 4
           my_exception       = 5
           OTHERS             = 6.
  CASE sy-subrc.
    WHEN 0.
      return_code = 0.
    WHEN OTHERS.
*      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      return_code = -1.
  ENDCASE.

ENDFORM.
