FUNCTION z_fmm_get_charact.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_MATNR) LIKE  MARA-MATNR
*"  EXPORTING
*"     REFERENCE(ES_ZSMM_CLASS) LIKE  ZSMM_CLASS STRUCTURE  ZSMM_CLASS
*"  EXCEPTIONS
*"      DATA_NOT_FOUND
*"----------------------------------------------------------------------

  DATA : lw_objectkey_imp     LIKE  bapi1003_key-object,
         lw_objecttable_imp   LIKE  bapi1003_key-objecttable
                                    VALUE 'MARA',
         lw_classtype_imp     LIKE  bapi1003_key-classtype
                                    VALUE '001',
         lw_classnum          LIKE  bapi1003_key-classnum,

         "Class BAPI: Assignment List
         lt_alloclist       LIKE  TABLE OF bapi1003_alloc_list
                                           WITH HEADER LINE,
         "Numeric Values
         lt_allocvaluesnum  LIKE  TABLE OF bapi1003_alloc_values_num
                                           WITH HEADER LINE,
         "Alphanumeric Values
         lt_allocvalueschar LIKE  TABLE OF bapi1003_alloc_values_char
                                           WITH HEADER LINE,
         "Currency Values
         lt_allocvaluescurr LIKE  TABLE OF bapi1003_alloc_values_curr
                                           WITH HEADER LINE,
         lt_return   LIKE  TABLE OF bapiret2
                                           WITH HEADER LINE.

  CLEAR: lw_objectkey_imp,   lw_classnum,
         lt_alloclist[],       lt_alloclist,
         lt_allocvaluesnum[],  lt_allocvaluesnum,
         lt_allocvalueschar[], lt_allocvalueschar,
         lt_allocvaluescurr[], lt_allocvaluescurr,
         lt_return[],          lt_return.




  MOVE i_matnr TO lw_objectkey_imp.
* get class number
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp         = lw_objectkey_imp
      objecttable_imp       = lw_objecttable_imp
      classtype_imp         = lw_classtype_imp
*     READ_VALUATIONS       =
      keydate               = sy-datum
      language              = sy-langu
    TABLES
      alloclist             = lt_alloclist
*     ALLOCVALUESCHAR       =
*     ALLOCVALUESCURR       =
*     ALLOCVALUESNUM        =
      return                = lt_return.


  IF lt_alloclist[] IS INITIAL.
    RAISE data_not_found.
  ELSE.
* get class characteristic
    LOOP AT lt_alloclist.
      lw_classnum  =  lt_alloclist-classnum.
* read character detail
      CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
        EXPORTING
          objectkey              = lw_objectkey_imp
          objecttable            = lw_objecttable_imp
          classnum               = lw_classnum
          classtype              = lw_classtype_imp
          keydate                = sy-datum
*   UNVALUATED_CHARS       = ' '
          language               = sy-langu
* IMPORTING
*   STATUS                 =
*   STANDARDCLASS          =
        TABLES
          allocvaluesnum         =  lt_allocvaluesnum
          allocvalueschar        =  lt_allocvalueschar
          allocvaluescurr        =  lt_allocvaluescurr
          return                 =  lt_return.

      DATA : lw_coat_front LIKE lt_allocvalueschar-value_char,
             lw_coat_back  LIKE lt_allocvalueschar-value_char.
      CLEAR: lw_coat_front, lw_coat_back.
*  modfiy characteristic of roh(only characteristic type is char)
      LOOP AT lt_allocvalueschar.
        CASE lt_allocvalueschar-charact.
          WHEN 'ZSTEEL_MATPROPERTY'.  "material property
            es_zsmm_class-zprop       =  lt_allocvalueschar-value_char.
          WHEN 'ZSPEC_THICK'.  "thick
            es_zsmm_class-zthick      =  lt_allocvalueschar-value_char.
          WHEN 'ZSPEC_WIDTH'.  "width
            es_zsmm_class-zwidth      =  lt_allocvalueschar-value_char.
          WHEN 'ZSPEC_LENGTH'.  "length
            es_zsmm_class-zlength     =  lt_allocvalueschar-value_char.
          WHEN 'ZEDGE'.  "edge
            es_zsmm_class-zedge       =  lt_allocvalueschar-value_char.
          WHEN 'ZKG_M'.  "kg/m
            es_zsmm_class-zmkg        =  lt_allocvalueschar-value_char.
          WHEN 'ZFRONT_FINISHING_THICKNESS'.  "front finishing thick
            lw_coat_front  = lt_allocvalueschar-value_char.
          WHEN 'ZBACK_FINISHING_THICKNESS'.  "back finishing thick
            lw_coat_back   = lt_allocvalueschar-value_char.
          WHEN 'ZKIND_OF_STEEL'.  "kind of product
            es_zsmm_class-zproduct    =  lt_allocvalueschar-value_char.
          WHEN 'ZIN_OR_OUT'.  "in or out
            es_zsmm_class-zinout      =  lt_allocvalueschar-value_char.
          WHEN 'ZGRADE'.       "grade
            es_zsmm_class-ZSTLGRADE   =  lt_allocvalueschar-value_char.
          WHEN 'ZSPECIFIC_GRAVITY'.  "density
            es_zsmm_class-zdensity    =  lt_allocvalueschar-value_char.
        ENDCASE.
      ENDLOOP.

      "coating
      CONCATENATE lw_coat_front lw_coat_back
                  INTO es_zsmm_class-zcoating.

    ENDLOOP.

  ENDIF.





ENDFUNCTION.
