FUNCTION Z_FMM_GET_CHARACT_CONV_CHAR1.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_MATNR) LIKE  MARA-MATNR
*"     VALUE(CLASSNUM) LIKE  BAPI1003_KEY-CLASSNUM
*"  TABLES
*"      IT_ALLOCVALUESCHAR STRUCTURE  BAPI1003_ALLOC_VALUES_CHAR
*"----------------------------------------------------------------------

  DATA :
         lw_objectkey    LIKE  bapi1003_key-object,
         lw_objecttable  LIKE  bapi1003_key-objecttable
                               VALUE 'MARA',
         lw_classnum     LIKE  bapi1003_key-classnum,
         lw_classtype    LIKE  bapi1003_key-classtype
                               VALUE '001',

         lt_alloclist    LIKE  TABLE  OF  bapi1003_alloc_list
                               WITH HEADER LINE,
         lt_return2      LIKE  TABLE  OF  bapiret2
                               WITH HEADER LINE,
         lt_allocvaluesnum  LIKE  TABLE  OF  bapi1003_alloc_values_num
                                  WITH HEADER LINE,
         lt_allocvalueschar LIKE  TABLE  OF  bapi1003_alloc_values_char
                                  WITH HEADER LINE,
         lt_allocvaluescurr LIKE  TABLE  OF  bapi1003_alloc_values_curr
                                  WITH HEADER LINE,

         BEGIN OF lt_char  OCCURS 0,
           charact  TYPE  atnam,
           value    TYPE  atwrt,
           desc     TYPE  atbez,
         END   OF lt_char.



  CLEAR : lt_alloclist[], lt_alloclist,
          lt_return2[], lt_return2.

  MOVE i_matnr  TO  lw_objectkey.

* get class
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp         =  lw_objectkey
      objecttable_imp       =  lw_objecttable
      classtype_imp         =  lw_classtype
*        READ_VALUATIONS       =
      keydate               = sy-datum
      language              = sy-langu
    TABLES
      alloclist             =  lt_alloclist
*        ALLOCVALUESCHAR       =
*        ALLOCVALUESCURR       =
*        ALLOCVALUESNUM        =
      return                =  lt_return2.

  LOOP AT lt_alloclist.
    MOVE lt_alloclist-classnum TO lw_classnum.
* get characteristic
    CLEAR : lt_allocvaluesnum[], lt_allocvaluesnum,
            lt_allocvalueschar[], lt_allocvalueschar,
            lt_allocvaluescurr[], lt_allocvaluescurr,
            lt_return2[], lt_return2.
    lw_classnum = CLASSNUM.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey              =  lw_objectkey
        objecttable            =  lw_objecttable
        classnum               =  lw_classnum
        classtype              =  lw_classtype
        keydate                = sy-datum
*     UNVALUATED_CHARS       = ' '
        language               = sy-langu
*   IMPORTING
*     STATUS                 =
*     STANDARDCLASS          =
      TABLES
        allocvaluesnum         =  lt_allocvaluesnum
        allocvalueschar        =  lt_allocvalueschar
        allocvaluescurr        =  lt_allocvaluescurr
        return                 =  lt_return2.
    LOOP AT lt_allocvaluesnum.
      MOVE-CORRESPONDING  lt_allocvaluesnum TO lt_allocvalueschar.
      PERFORM fltp_char_conversion USING lt_allocvaluesnum-value_from
                                         lt_allocvalueschar-value_char.
      APPEND lt_allocvalueschar.
    ENDLOOP.
    LOOP AT lt_allocvaluescurr.
    ENDLOOP.
  ENDLOOP.

  it_allocvalueschar[] = lt_allocvalueschar[].



ENDFUNCTION.
