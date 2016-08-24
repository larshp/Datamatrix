CLASS zcl_datamatrix DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_DATAMATRIX
*"* do not include other source files here!!!

    TYPES:
      t_text3 TYPE so_text003.

    CLASS-METHODS matrix
      IMPORTING
        !iv_text         TYPE t_text3
      RETURNING
        VALUE(rv_matrix) TYPE char100 .
  PROTECTED SECTION.
*"* protected components of class ZCL_DATAMATRIX
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_DATAMATRIX
*"* do not include other source files here!!!

    TYPES:
      t_integer_table TYPE STANDARD TABLE OF i .

    CLASS-DATA gt_log TYPE t_integer_table .
    CLASS-DATA gt_alog TYPE t_integer_table .
    CONSTANTS c_gf TYPE i VALUE 256.                        "#EC NOTEXT
    CONSTANTS c_pp TYPE i VALUE 301.                        "#EC NOTEXT

    CLASS-METHODS build_bin_seq
      IMPORTING
        !iv_text          TYPE t_text3
      RETURNING
        VALUE(rv_bin_seq) TYPE string .
    CLASS-METHODS hex_to_bin
      IMPORTING
        !iv_hex           TYPE x
      RETURNING
        VALUE(rv_bin_seq) TYPE string .
    CLASS-METHODS int_to_bin
      IMPORTING
        !iv_int           TYPE i
      RETURNING
        VALUE(rv_bin_seq) TYPE string .
    CLASS-METHODS place_bit
      IMPORTING
        VALUE(iv_x) TYPE i
        VALUE(iv_y) TYPE i
        !iv_bit     TYPE c
        !iv_size    TYPE i
      CHANGING
        !cv_matrix  TYPE clike .
    CLASS-METHODS place_word
      IMPORTING
        !iv_x         TYPE i
        !iv_y         TYPE i
        !iv_word_no   TYPE i
        !iv_words_bin TYPE string
        !iv_size      TYPE i
      CHANGING
        !cv_matrix    TYPE clike .
    CLASS-METHODS bit_xor
      IMPORTING
        !iv_i1      TYPE i
        !iv_i2      TYPE i
      RETURNING
        VALUE(rv_i) TYPE i .
    CLASS-METHODS multiply
      IMPORTING
        !iv_i1      TYPE i
        !iv_i2      TYPE i
      RETURNING
        VALUE(rv_i) TYPE i .
    CLASS-METHODS coefficients
      IMPORTING
        !iv_checkwords   TYPE i DEFAULT 5
      EXPORTING
        !et_coefficients TYPE t_integer_table .
    CLASS-METHODS initialize .
    CLASS-METHODS reed_solomon
      CHANGING
        !ct_data TYPE t_integer_table .
ENDCLASS.



CLASS ZCL_DATAMATRIX IMPLEMENTATION.


  METHOD bit_xor.

    DATA: lv_x1 TYPE x LENGTH 4,
          lv_x2 TYPE x LENGTH 4,
          lv_x  TYPE x LENGTH 4.


    lv_x1 = iv_i1.
    lv_x2 = iv_i2.

    lv_x = lv_x1 BIT-XOR lv_x2.

    rv_i = lv_x.

  ENDMETHOD.


  METHOD build_bin_seq.

    DATA: lt_data    TYPE t_integer_table,
          lv_i       TYPE i,
          lo_conv    TYPE REF TO cl_abap_conv_out_ce,
          lv_xstring TYPE xstring,
          lv_bin_seq TYPE string.

    FIELD-SYMBOLS: <lv_data> LIKE LINE OF lt_data.


    lo_conv = cl_abap_conv_out_ce=>create( encoding    = '1160'
                                           ignore_cerr = abap_true ).

    lo_conv->convert( EXPORTING data = iv_text
                      IMPORTING buffer = lv_xstring ).

    WHILE xstrlen( lv_xstring ) > 0.
      lv_i = lv_xstring(1) + 1.
      APPEND lv_i TO lt_data.
      lv_xstring = lv_xstring+1.
    ENDWHILE.

    reed_solomon( CHANGING ct_data = lt_data ).

    LOOP AT lt_data ASSIGNING <lv_data>.
      lv_bin_seq = int_to_bin( <lv_data> ).
      CONCATENATE rv_bin_seq lv_bin_seq INTO rv_bin_seq.
    ENDLOOP.

  ENDMETHOD.


  METHOD coefficients.

    DATA: lt_new       TYPE t_integer_table,
          lt_copy      TYPE t_integer_table,
          lv_new_index TYPE i.

    FIELD-SYMBOLS: <lv_copy> LIKE LINE OF lt_copy,
                   <lv_new>  LIKE LINE OF lt_new,
                   <lv_exp>  LIKE LINE OF gt_alog,
                   <lv_res>  LIKE LINE OF et_coefficients.


    initialize( ).

****************************************************************
* calculate the generator polynomial coefficients

    APPEND 2 TO et_coefficients.
    APPEND 1 TO et_coefficients. " x

    DO iv_checkwords - 1 TIMES.
      CLEAR lt_new.
      READ TABLE gt_alog INDEX sy-index + 2 ASSIGNING <lv_exp>.
      APPEND <lv_exp> TO lt_new.
      APPEND 1 TO lt_new. " x

      lt_copy[] = et_coefficients[].
      CLEAR et_coefficients.

      LOOP AT lt_new ASSIGNING <lv_new>.
        lv_new_index = sy-tabix.

        LOOP AT lt_copy ASSIGNING <lv_copy>.

* place result in index c_index + res_index - 1
          READ TABLE et_coefficients INDEX lv_new_index + sy-tabix - 1 ASSIGNING <lv_res>.
          IF sy-subrc <> 0.
            APPEND INITIAL LINE TO et_coefficients ASSIGNING <lv_res>.
          ENDIF.

* res = res + c * <res_copy>
          <lv_res> = bit_xor( iv_i1 = <lv_res>
                              iv_i2 = multiply( iv_i1  = <lv_new>
                                                iv_i2  = <lv_copy> ) ).
        ENDLOOP.
      ENDLOOP.
    ENDDO.

  ENDMETHOD.


  METHOD hex_to_bin.

    DATA: lv_val TYPE c.


    DO 8 TIMES.
      GET BIT sy-index OF iv_hex INTO lv_val.
      CONCATENATE rv_bin_seq lv_val INTO rv_bin_seq.
    ENDDO.

  ENDMETHOD.


  METHOD initialize.

    DATA: lv_tmp TYPE i,
          lv_i   TYPE i.

    FIELD-SYMBOLS: <lv_log> LIKE LINE OF gt_log.


    IF NOT gt_log[] IS INITIAL OR NOT gt_alog[] IS INITIAL.
      RETURN.
    ENDIF.

    DO c_gf TIMES.
      APPEND INITIAL LINE TO gt_log ASSIGNING <lv_log>.
    ENDDO.

    lv_tmp = 1.
    APPEND lv_tmp TO gt_alog.

* calculate the log & antilog/exp table
    lv_i = 1.
    WHILE lv_i < c_gf.
      lv_tmp = lv_tmp * 2.
      IF lv_tmp >= c_gf.
        lv_tmp = bit_xor( iv_i1 = lv_tmp
                          iv_i2 = c_pp ).
      ENDIF.
      APPEND lv_tmp TO gt_alog.

      IF lv_i <> c_gf - 1.
        READ TABLE gt_log INDEX lv_tmp + 1 ASSIGNING <lv_log>.   " abap starts at one
        <lv_log> = lv_i.
      ENDIF.

      lv_i = lv_i + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD int_to_bin.

    DATA: lv_x TYPE x.


    lv_x = iv_int.

    rv_bin_seq = hex_to_bin( lv_x ).

  ENDMETHOD.


  METHOD matrix.

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


    lv_bin_seq = build_bin_seq( iv_text ).

* initialize
    DO lv_size * lv_size TIMES.
      CONCATENATE rv_matrix '_' INTO rv_matrix.
    ENDDO.
* top
    DO lv_size TIMES.
      lv_tmp = sy-index - 1.
      rv_matrix+lv_tmp(1) = sy-index MOD 2.
    ENDDO.
* left
    DO lv_size TIMES.
      lv_tmp = ( sy-index - 1 ) * lv_size.
      rv_matrix+lv_tmp(1) = 1.
    ENDDO.
* bottom
    DO lv_size TIMES.
      lv_tmp = lv_size * ( lv_size - 1 ) + sy-index - 1.
      rv_matrix+lv_tmp(1) = 1.
    ENDDO.
* right
    DO lv_size TIMES.
      lv_tmp = lv_size * sy-index - 1.
      rv_matrix+lv_tmp(1) = ( sy-index - 1 ) MOD 2.
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

  ENDMETHOD.


  METHOD multiply.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <lv_alog>  LIKE LINE OF gt_alog,
                   <lv_log_x> LIKE LINE OF gt_log,
                   <lv_log_y> LIKE LINE OF gt_log.


    IF iv_i1 = 0 OR iv_i2 = 0.
      rv_i = 0.
      RETURN.
    ENDIF.

    initialize( ).

    READ TABLE gt_log INDEX iv_i1 + 1 ASSIGNING <lv_log_x>.
    ASSERT sy-subrc = 0.
    READ TABLE gt_log INDEX iv_i2 + 1 ASSIGNING <lv_log_y>.
    ASSERT sy-subrc = 0.

    lv_index = ( <lv_log_x> + <lv_log_y> ) MOD ( c_gf - 1 ).

    READ TABLE gt_alog INDEX lv_index + 1 ASSIGNING <lv_alog>.

    rv_i = <lv_alog>.

  ENDMETHOD.


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


  METHOD place_word.

    DATA: lv_bit TYPE c,
          lv_tmp TYPE i.


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


  METHOD reed_solomon.

    DATA: lt_coeff      TYPE t_integer_table,
          lv_checkwords TYPE i,
          lv_datawords  TYPE i,
          lv_i          TYPE i,
          lv_k          TYPE i,
          lt_ecc        TYPE t_integer_table,
          lv_m          TYPE i.

    FIELD-SYMBOLS: <lv_wd_i>  LIKE LINE OF ct_data.

    FIELD-SYMBOLS: <lv_coeff>  LIKE LINE OF lt_coeff,
                   <lv_ecc>    LIKE LINE OF lt_ecc,
                   <lv_ecc_m1> LIKE LINE OF lt_ecc.

*****************

    DESCRIBE TABLE ct_data.
    lv_datawords = sy-tfill.
    ASSERT lv_datawords = 3.
    lv_checkwords = 5.

    initialize( ).

    coefficients( EXPORTING iv_checkwords = lv_checkwords
                  IMPORTING et_coefficients = lt_coeff ).

    DO lv_checkwords TIMES.
      APPEND 0 TO lt_ecc.
    ENDDO.

    lv_i = 0.
    WHILE lv_i < lv_datawords.

      READ TABLE lt_ecc INDEX lv_checkwords ASSIGNING <lv_ecc>.
      READ TABLE ct_data INDEX lv_i + 1 ASSIGNING <lv_wd_i>.
      lv_m = bit_xor( iv_i1  = <lv_ecc>
                      iv_i2  = <lv_wd_i> ).

      lv_k = lv_checkwords - 1.
      WHILE lv_k > 0.
        READ TABLE lt_coeff INDEX lv_k + 1 ASSIGNING <lv_coeff>.
        READ TABLE lt_ecc INDEX lv_k + 1 ASSIGNING <lv_ecc>.
        READ TABLE lt_ecc INDEX lv_k ASSIGNING <lv_ecc_m1>.
        IF lv_m <> 0 AND <lv_coeff> <> 0.
          <lv_ecc> = bit_xor(
            iv_i1  = <lv_ecc_m1>
            iv_i2  = multiply( iv_i1  = lv_m
                               iv_i2  = <lv_coeff> ) ).
        ELSE.
          <lv_ecc> = <lv_ecc_m1>.
        ENDIF.
        lv_k = lv_k - 1.
      ENDWHILE.

      READ TABLE lt_coeff INDEX 1 ASSIGNING <lv_coeff>.
      READ TABLE lt_ecc INDEX 1 ASSIGNING <lv_ecc>.
      IF lv_m <> 0 AND <lv_coeff> <> 0.
        <lv_ecc> = multiply( iv_i1  = lv_m
                             iv_i2  = <lv_coeff> ).
      ELSE.
        <lv_ecc> = 0.
      ENDIF.

      lv_i = lv_i + 1.
    ENDWHILE.


    lv_i = lv_checkwords.
    WHILE lv_i > 0.
      READ TABLE lt_ecc ASSIGNING <lv_ecc> INDEX lv_i.
      APPEND <lv_ecc> TO ct_data.
      lv_i = lv_i - 1.
    ENDWHILE.

  ENDMETHOD.
ENDCLASS.