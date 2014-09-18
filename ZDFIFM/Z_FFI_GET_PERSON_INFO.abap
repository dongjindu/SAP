FUNCTION Z_FFI_GET_PERSON_INFO.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     REFERENCE(I_USNAM) TYPE  USNAM
*"  EXPORTING
*"     VALUE(E_DEPARTMENT) TYPE  AD_DPRTMNT
*"     VALUE(E_NAME_TEXT) TYPE  AD_NAMTEXT
*"----------------------------------------------------------------------
 DATA: l_persnumber  LIKE usr21-persnumber,
       l_addrnumber  LIKE usr21-addrnumber.

* usr21: Assign User Name Address Key
  SELECT single persnumber addrnumber
    FROM usr21
    INTO (l_persnumber, l_addrnumber)
   WHERE bname = i_usnam.

* adcp: Person /Address Assignment
  SELECT single department into e_department
      FROM adcp
     WHERE persnumber = l_persnumber
       AND addrnumber = l_addrnumber.

* adrp: Persons (central address administration)
  select single name_text into e_name_text from adrp
    where persnumber eq l_persnumber.




ENDFUNCTION.
