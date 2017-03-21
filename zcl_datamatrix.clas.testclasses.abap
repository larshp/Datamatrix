
*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
* ================

    METHODS: matrix1 FOR TESTING,
             matrix2 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
* ==============================

  METHOD matrix1.
* =============

    DATA: lv_matrix TYPE char100.

    lv_matrix = zcl_datamatrix=>matrix( 'bar' ).

    cl_abap_unit_assert=>assert_equals(
        exp = '10101010101010111011110001101010101110111010000100' &&
              '11011110111100101100111010100110111011001111111111'
        act = lv_matrix ).

  ENDMETHOD.                    "matrix1

  METHOD matrix2.
* =============

    DATA: lv_matrix TYPE char100.

    lv_matrix = zcl_datamatrix=>matrix( 'foo' ).

    cl_abap_unit_assert=>assert_equals(
        exp = '10101010101010000001111000001010000100111000100100' &&
              '11111111111000000110110011001110011010001111111111'
        act = lv_matrix ).

  ENDMETHOD.                    "matrix2

ENDCLASS.       "lcl_Test
