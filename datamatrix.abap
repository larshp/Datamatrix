class ZDK8LAHVP001 definition
  public
  final
  create public .

public section.
*"* public components of class ZDK8LAHVP001
*"* do not include other source files here!!!

  class-methods BUILD
    returning
      value(RV_MATRIX) type CHAR20K .
  class-methods BUILD_BIN_SEQ
    returning
      value(RV_BIN_SEQ) type STRING .
  class-methods CHAR_TO_BIN
    importing
      !IV_CHAR type C
    returning
      value(RV_BIN_SEQ) type STRING .
  class-methods HEX_TO_BIN
    importing
      !IV_HEX type X
    returning
      value(RV_BIN_SEQ) type STRING .
  class-methods INT_TO_BIN
    importing
      !IV_INT type I
    returning
      value(RV_BIN_SEQ) type STRING .
  class-methods OUTPUT
    importing
      !IV_SIZE type I default 10
      !IV_MATRIX type CLIKE .
  class-methods PLACE_BIT
    importing
      value(IV_X) type I
      value(IV_Y) type I
      !IV_BIT type C
      !IV_SIZE type I
    changing
      !CV_MATRIX type CLIKE .
  class-methods PLACE_WORD
    importing
      !IV_X type I
      !IV_Y type I
      !IV_WORD_NO type I
      !IV_WORDS_BIN type STRING
      !IV_SIZE type I
    changing
      !CV_MATRIX type CLIKE .
  class-methods BIT_XOR
    importing
      !IV_I1 type I
      !IV_I2 type I
    returning
      value(RV_I) type I .
  class-methods MULTIPLY
    importing
      !IV_I1 type I
      !IV_I2 type I
    returning
      value(RV_I) type I .
  class-methods COEFFICIENTS
    importing
      !IV_CHECKWORDS type I default 5
    returning
      value(COEFFICIENTS) type ZINTEGER_TT .
  class-methods INITIALIZE .
  class-methods REED_SOLOMON
    changing
      !CT_DATA type ZINTEGER_TT .
protected section.
*"* protected components of class ZDK8LAHVP001
*"* do not include other source files here!!!
private section.
*"* private components of class ZDK8LAHVP001
*"* do not include other source files here!!!

  class-data LOG type ZINTEGER_TT .
  class-data ALOG type ZINTEGER_TT .
  constants GF type I value 256. "#EC NOTEXT
  constants PP type I value 301. "#EC NOTEXT
ENDCLASS.



CLASS ZDK8LAHVP001 IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>BIT_XOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_I1                          TYPE        I
* | [--->] IV_I2                          TYPE        I
* | [<-()] RV_I                           TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD bit_xor.

  DATA: lv_x1 TYPE x LENGTH 4,
        lv_x2 TYPE x LENGTH 4,
        lv_x  TYPE x LENGTH 4.


  lv_x1 = iv_i1.
  lv_x2 = iv_i2.

  lv_x = lv_x1 BIT-XOR lv_x2.

  rv_i = lv_x.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>BUILD
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_MATRIX                      TYPE        CHAR20K
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD build.

* size: 10  words:  8
* size: 12  words: 12
* ....

  DATA: lv_bin_seq TYPE string,
        lv_tmp     TYPE i,
        lv_dir     TYPE c,
        lv_x       TYPE i,
        lv_y       TYPE i,
        lv_size    TYPE i VALUE 10,      " including borders
        lv_words   TYPE i VALUE 8.


  lv_bin_seq = build_bin_seq( ).  " todo

* initialize
  DO lv_size * lv_size TIMES.
    CONCATENATE rv_matrix '_' INTO rv_matrix.
  ENDDO.
* top
  DO lv_size TIMES.
    lv_tmp = sy-index - 1.
    rv_matrix+lv_tmp(1) = sy-index MOD 2.
*    lv_matrix+lv_tmp(1) = 'X'.
  ENDDO.
* left
  DO lv_size TIMES.
    lv_tmp = ( sy-index - 1 ) * lv_size.
    rv_matrix+lv_tmp(1) = 1.
*    lv_matrix+lv_tmp(1) = 'X'.
  ENDDO.
* bottom
  DO lv_size TIMES.
    lv_tmp = lv_size * ( lv_size - 1 ) + sy-index - 1.
    rv_matrix+lv_tmp(1) = 1.
*    lv_matrix+lv_tmp(1) = 'X'.
  ENDDO.
* right
  DO lv_size TIMES.
    lv_tmp = lv_size * sy-index - 1.
    rv_matrix+lv_tmp(1) = ( sy-index - 1 ) MOD 2.
*    lv_matrix+lv_tmp(1) = 'X'.
  ENDDO.

***************************

* always start at x = 2, y = 6
  lv_x = 2.
  lv_y = 6.
  lv_dir = 'U'. " up

  DO lv_words TIMES. " number of words times

* make an extra step to get inside the matrix again
    IF lv_y <= 1 OR lv_y >= lv_size
        OR lv_x <= 1 OR lv_x >= lv_size.
      CASE lv_dir.
        WHEN 'U'. " up
          lv_x = lv_x + 2.
          lv_y = lv_y - 2.
        WHEN 'D'. " down
          lv_x = lv_x - 2.
          lv_y = lv_y + 2.
      ENDCASE.
    ENDIF.

    place_word( EXPORTING iv_x         = lv_x
                          iv_y         = lv_y
                          iv_word_no   = sy-index
                          iv_words_bin = lv_bin_seq
                          iv_size      = lv_size
                CHANGING  cv_matrix    = rv_matrix ).

    CASE lv_dir.
      WHEN 'U'. " up
        lv_x = lv_x + 2.
        lv_y = lv_y - 2.
      WHEN 'D'. " down
        lv_x = lv_x - 2.
        lv_y = lv_y + 2.
    ENDCASE.

    IF ( lv_y <= 1 AND lv_dir = 'U' )
        OR ( lv_x >= lv_size AND lv_dir = 'U' ).
      lv_x = lv_x + 1.
      lv_y = lv_y + 3.
      lv_dir = 'D'.
    ENDIF.
    IF ( lv_y >= lv_size AND lv_dir = 'D' )
        OR ( lv_x <= 1 AND lv_dir = 'D' ).
      lv_x = lv_x + 5.
      lv_y = lv_y - 1.
      lv_dir = 'U'.
    ENDIF.
  ENDDO.

*  output( iv_size   = lv_size
*          iv_matrix = rv_matrix ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>BUILD_BIN_SEQ
* +-------------------------------------------------------------------------------------------------+
* | [<-()] RV_BIN_SEQ                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD build_bin_seq.

  DATA: lt_data    TYPE zinteger_tt,
        lv_bin_seq TYPE string.

  FIELD-SYMBOLS: <lv_data> LIKE LINE OF lt_data.


*  APPEND 103 TO lt_data. " f
*  APPEND 112 TO lt_data. " o
*  APPEND 112 TO lt_data. " o

    APPEND 99 TO lt_data.  " b
    APPEND 98 TO lt_data.  " a
    APPEND 115 TO lt_data. " r

  zdk8lahvp001=>reed_solomon( CHANGING ct_data = lt_data ).

  LOOP AT lt_data ASSIGNING <lv_data>.
    lv_bin_seq = int_to_bin( <lv_data> ).
    CONCATENATE rv_bin_seq lv_bin_seq INTO rv_bin_seq.
  ENDLOOP.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>CHAR_TO_BIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHAR                        TYPE        C
* | [<-()] RV_BIN_SEQ                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD char_to_bin.

  DATA: x TYPE x,
        val TYPE c.

  FIELD-SYMBOLS: <fs> TYPE any.


  ASSIGN iv_char TO <fs> CASTING TYPE x.
  x = <fs>+1.   " take 2nd byte, can probably be done nicer
* normal latin characters just one byte in UTF-16(sap default unicode codepage)

  x = x + 1.   " why?

  rv_bin_seq = hex_to_bin( x ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>COEFFICIENTS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_CHECKWORDS                  TYPE        I (default =5)
* | [<-()] COEFFICIENTS                   TYPE        ZINTEGER_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD coefficients.

  DATA: new       LIKE coefficients,
        copy      LIKE coefficients,
        lv_new_index TYPE i.

  FIELD-SYMBOLS: <copy> LIKE LINE OF coefficients,
                 <new>  LIKE LINE OF new,
                 <exp>  LIKE LINE OF alog,
                 <res>  LIKE LINE OF coefficients.


  initialize( ).

****************************************************************
* calculate the generator polynomial coefficients

  APPEND 2 TO coefficients.
  APPEND 1 TO coefficients. " x

  DO iv_checkwords - 1 TIMES.
    REFRESH new.
    READ TABLE alog INDEX sy-index + 2 ASSIGNING <exp>.
    APPEND <exp> TO new.
    APPEND 1 TO new. " x

    copy[] = coefficients[].
    REFRESH coefficients.

    LOOP AT new ASSIGNING <new>.
      lv_new_index = sy-tabix.

      LOOP AT copy ASSIGNING <copy>.

* place result in index c_index + res_index - 1
        READ TABLE coefficients INDEX lv_new_index + sy-tabix - 1 ASSIGNING <res>.
        IF sy-subrc <> 0.
          APPEND INITIAL LINE TO coefficients ASSIGNING <res>.
        ENDIF.

* res = res + c * <res_copy>
        <res> = bit_xor( iv_i1 = <res>
                         iv_i2 = multiply( iv_i1  = <new>
                                           iv_i2  = <copy> ) ).
      ENDLOOP.
    ENDLOOP.
  ENDDO.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>HEX_TO_BIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_HEX                         TYPE        X
* | [<-()] RV_BIN_SEQ                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD hex_to_bin.

  DATA: val TYPE c.

  DO 8 TIMES.
    GET BIT sy-index OF iv_hex INTO val.
    CONCATENATE rv_bin_seq val INTO rv_bin_seq.
  ENDDO.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>INITIALIZE
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD initialize.

  DATA: lv_tmp TYPE i,
        lv_i   TYPE i.

  FIELD-SYMBOLS: <log> LIKE LINE OF log,
                 <alog> LIKE LINE OF alog.


  IF NOT log[] IS INITIAL OR NOT alog[] IS INITIAL.
    RETURN.
  ENDIF.

  DO gf TIMES.
    APPEND INITIAL LINE TO log ASSIGNING <log>.
  ENDDO.

  lv_tmp = 1.
  APPEND lv_tmp TO alog.

* calculate the log & antilog/exp table
  lv_i = 1.
  WHILE lv_i < gf.
    lv_tmp = lv_tmp * 2.
    IF lv_tmp >= gf.
      lv_tmp = bit_xor( iv_i1 = lv_tmp
                        iv_i2 = pp ).
    ENDIF.
    APPEND lv_tmp TO alog.

    IF lv_i <> gf - 1.
      READ TABLE log INDEX lv_tmp + 1 ASSIGNING <log>.   " abap starts at one
      <log> = lv_i.
    ENDIF.

    lv_i = lv_i + 1.
  ENDWHILE.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>INT_TO_BIN
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INT                         TYPE        I
* | [<-()] RV_BIN_SEQ                     TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD int_to_bin.

  DATA: x TYPE x.

  x = iv_int.

  rv_bin_seq = hex_to_bin( x ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>MULTIPLY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_I1                          TYPE        I
* | [--->] IV_I2                          TYPE        I
* | [<-()] RV_I                           TYPE        I
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD multiply.

  DATA: lv_index TYPE i.

  FIELD-SYMBOLS: <alog>  LIKE LINE OF alog,
                 <log_x> LIKE LINE OF log,
                 <log_y> LIKE LINE OF log.


  IF iv_i1 = 0 OR iv_i2 = 0.
    rv_i = 0.
    RETURN.
  ENDIF.

  initialize( ).

  READ TABLE log INDEX iv_i1 + 1 ASSIGNING <log_x>.
  ASSERT sy-subrc = 0.
  READ TABLE log INDEX iv_i2 + 1 ASSIGNING <log_y>.
  ASSERT sy-subrc = 0.

  lv_index = ( <log_x> + <log_y> ) MOD ( gf - 1 ).

  READ TABLE alog INDEX lv_index + 1 ASSIGNING <alog>.

  rv_i = <alog>.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>OUTPUT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_SIZE                        TYPE        I (default =10)
* | [--->] IV_MATRIX                      TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD output.

  DATA: lv_x   TYPE i,
        lv_tmp TYPE i,
        lv_y   TYPE i.


  DO iv_size TIMES.
    lv_y = sy-index.
    DO iv_size TIMES.
      lv_x = sy-index.

      lv_tmp = lv_x - 1 + ( ( lv_y - 1 ) * iv_size ).
      WRITE iv_matrix+lv_tmp(1).
    ENDDO.
    WRITE: /.
  ENDDO.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>PLACE_BIT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_X                           TYPE        I
* | [--->] IV_Y                           TYPE        I
* | [--->] IV_BIT                         TYPE        C
* | [--->] IV_SIZE                        TYPE        I
* | [<-->] CV_MATRIX                      TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD place_bit.

  DATA: lv_tmp TYPE i.


  IF iv_x <= 1.
    iv_x = iv_x + iv_size - 2.
    iv_y = iv_y + 4 - ( ( iv_size + 4 ) MOD 8 ) + 2.
  ENDIF.
  IF iv_y <= 1.
    iv_y = iv_y + iv_size - 2.
    iv_x = iv_x + 4 - ( ( iv_size + 4 ) MOD 8 ) + 2.
  ENDIF.

  lv_tmp = iv_x - 1 + ( ( iv_y - 1 ) * iv_size ).
  cv_matrix+lv_tmp(1) = iv_bit.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>PLACE_WORD
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_X                           TYPE        I
* | [--->] IV_Y                           TYPE        I
* | [--->] IV_WORD_NO                     TYPE        I
* | [--->] IV_WORDS_BIN                   TYPE        STRING
* | [--->] IV_SIZE                        TYPE        I
* | [<-->] CV_MATRIX                      TYPE        CLIKE
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD place_word.

  DATA: lv_bit TYPE c,
        lv_tmp TYPE i.


*  lv_bit = iv_word_no.


* bit 1
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ).
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x - 2
                       iv_y      = iv_y - 2
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).
* bit 2
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ) + 1.
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x - 1
                       iv_y      = iv_y - 2
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).
* bit 3
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ) + 2.
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x - 2
                       iv_y      = iv_y - 1
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).
* bit 4
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ) + 3.
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x - 1
                       iv_y      = iv_y - 1
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).
* bit 5
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ) + 4.
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x
                       iv_y      = iv_y - 1
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).
* bit 6
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ) + 5.
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x - 2
                       iv_y      = iv_y
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).
* bit 7
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ) + 6.
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x - 1
                       iv_y      = iv_y
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).
* bit 8
  lv_tmp = ( ( iv_word_no - 1 ) * 8 ) + 7.
  lv_bit = iv_words_bin+lv_tmp(1).
  place_bit( EXPORTING iv_x      = iv_x
                       iv_y      = iv_y
                       iv_bit    = lv_bit
                       iv_size   = iv_size
             CHANGING  cv_matrix = cv_matrix ).

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZDK8LAHVP001=>REED_SOLOMON
* +-------------------------------------------------------------------------------------------------+
* | [<-->] CT_DATA                        TYPE        ZINTEGER_TT
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD reed_solomon.

  DATA: lt_coeff      TYPE zinteger_tt,
        lv_checkwords TYPE i,
        lv_datawords  TYPE i,
        lv_i          TYPE i,
        lv_j          TYPE i,
        lv_k          TYPE i,
        ecc           TYPE zinteger_tt,
        lv_m          TYPE i.

  FIELD-SYMBOLS: <wd_i>  LIKE LINE OF ct_data.

  FIELD-SYMBOLS: <coeff>  LIKE LINE OF lt_coeff,
                 <ecc>    LIKE LINE OF ecc,
                 <ecc_m1> LIKE LINE OF ecc.

*****************

  DESCRIBE TABLE ct_data.
  lv_datawords = sy-tfill.
  ASSERT lv_datawords = 3.
  lv_checkwords = 5.

  initialize( ).

  lt_coeff = coefficients( lv_checkwords ).

  DO lv_checkwords TIMES.
    APPEND 0 TO ecc.
  ENDDO.

  lv_i = 0.
  WHILE lv_i < lv_datawords.

    READ TABLE ecc INDEX lv_checkwords ASSIGNING <ecc>.
    READ TABLE ct_data INDEX lv_i + 1 ASSIGNING <wd_i>.
    lv_m = bit_xor( iv_i1  = <ecc>
                    iv_i2  = <wd_i> ).

    lv_k = lv_checkwords - 1.
    WHILE lv_k > 0.
      READ TABLE lt_coeff INDEX lv_k + 1 ASSIGNING <coeff>.
      READ TABLE ecc INDEX lv_k + 1 ASSIGNING <ecc>.
      READ TABLE ecc INDEX lv_k ASSIGNING <ecc_m1>.
      IF lv_m <> 0 AND <coeff> <> 0.
        <ecc> = bit_xor(
          iv_i1  = <ecc_m1>
          iv_i2  = multiply( iv_i1  = lv_m
                             iv_i2  = <coeff> ) ).
      ELSE.
        <ecc> = <ecc_m1>.
      ENDIF.
      lv_k = lv_k - 1.
    ENDWHILE.

    READ TABLE lt_coeff INDEX 1 ASSIGNING <coeff>.
    READ TABLE ecc INDEX 1 ASSIGNING <ecc>.
    IF lv_m <> 0 AND <coeff> <> 0.
      <ecc> = multiply( iv_i1  = lv_m
                        iv_i2  = <coeff> ).
    ELSE.
      <ecc> = 0.
    ENDIF.

    lv_i = lv_i + 1.
  ENDWHILE.


  lv_i = lv_checkwords.
  WHILE lv_i > 0.
    READ TABLE ecc ASSIGNING <ecc> INDEX lv_i.
    APPEND <ecc> TO ct_data.
    lv_i = lv_i - 1.
  ENDWHILE.

ENDMETHOD.
ENDCLASS.
