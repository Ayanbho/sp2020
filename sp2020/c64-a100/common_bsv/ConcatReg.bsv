// Copyright (c) 2016, 2017 Massachusetts Institute of Technology

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

/**
 * This package contains a class of functions `concatRegN` for concatenating
 * `N` registers together into a single register. This file has definitions
 * for `N` = 2 up to `N` = 32
 *
 * This package is created by `gen_ConcatReg.py`. If you want to modify this
 * package, please modify `gen_ConcatReg.py` instead. If you need a wider
 * `concatReg` function, change the value of n in `gen_ConcatReg.py` and run
 * it again.
 *
 * The Bluespec provided BuildVector.bsv provides another example of
 * constructing a function that takes a variable number of arguments
 */
package ConcatReg;

/// Typeclass for creating _concatReg with a variable number of arguments.
typeclass ConcatReg#(type r, numeric type n1, numeric type n2)
  dependencies ((r,n1) determines n2, (r,n2) determines n1);
  // dependencies (r determines (n1,n2));
  function r _concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2);
endtypeclass
/// Base case instance of ConcatReg.
instance ConcatReg#(Reg#(Bit#(n3)), n1, n2) provisos (Add#(n1, n2, n3));
  function Reg#(Bit#(TAdd#(n1,n2))) _concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2);
    return (interface Reg;
        method Bit#(TAdd#(n1,n2)) _read = {r1._read, r2._read};
        method Action _write(Bit#(TAdd#(n1,n2)) x);
          r1._write(truncateLSB(x));
          r2._write(truncate(x));
        endmethod
      endinterface);
  endfunction
endinstance
/// Recursion case instance of ConcatReg.
instance ConcatReg#(function r f(Reg#(Bit#(n3)) r3), n1, n2) provisos (ConcatReg#(r, TAdd#(n1, n2), n3));
  function function r f(Reg#(Bit#(n3)) r3) _concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2);
    return _concatReg(interface Reg;
        method Bit#(TAdd#(n1,n2)) _read = {r1._read, r2._read};
        method Action _write(Bit#(TAdd#(n1,n2)) x);
          r1._write(truncateLSB(x));
          r2._write(truncate(x));
        endmethod
      endinterface);
  endfunction
endinstance

/// This function can concatenate a variable number of registers together.
///
/// This is a wrapper function of `_concatReg` that is intended for users.
/// You will need to use `asReg()` for the third argument and beyond in order
/// for the Bluespec compiler to be able to type check this.
function r concatReg(Reg#(Bit#(n1)) r1, Reg#(Bit#(n2)) r2) provisos(ConcatReg#(r, n1, n2));
  return _concatReg(asReg(r1),asReg(r2));
endfunction

// Automatically generated macros with a set number of registers.
// These don't require asReg when used.
/// Concatenate 2 registers together
function Reg#(Bit#(n)) concatReg2(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2
    ) provisos (
      Add#(n1,n2,n)
    );
  return concatReg(asReg(r1),asReg(r2));
endfunction

/// Concatenate 3 registers together
function Reg#(Bit#(n)) concatReg3(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3
    ) provisos (
      Add#(TAdd#(n1,n2),n3,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3));
endfunction

/// Concatenate 4 registers together
function Reg#(Bit#(n)) concatReg4(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4
    ) provisos (
      Add#(TAdd#(TAdd#(n1,n2),n3),n4,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4));
endfunction

/// Concatenate 5 registers together
function Reg#(Bit#(n)) concatReg5(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5));
endfunction

/// Concatenate 6 registers together
function Reg#(Bit#(n)) concatReg6(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6));
endfunction

/// Concatenate 7 registers together
function Reg#(Bit#(n)) concatReg7(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7));
endfunction

/// Concatenate 8 registers together
function Reg#(Bit#(n)) concatReg8(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8));
endfunction

/// Concatenate 9 registers together
function Reg#(Bit#(n)) concatReg9(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9));
endfunction

/// Concatenate 10 registers together
function Reg#(Bit#(n)) concatReg10(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10));
endfunction

/// Concatenate 11 registers together
function Reg#(Bit#(n)) concatReg11(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11));
endfunction

/// Concatenate 12 registers together
function Reg#(Bit#(n)) concatReg12(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12));
endfunction

/// Concatenate 13 registers together
function Reg#(Bit#(n)) concatReg13(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13));
endfunction

/// Concatenate 14 registers together
function Reg#(Bit#(n)) concatReg14(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14));
endfunction

/// Concatenate 15 registers together
function Reg#(Bit#(n)) concatReg15(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15));
endfunction

/// Concatenate 16 registers together
function Reg#(Bit#(n)) concatReg16(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16));
endfunction

/// Concatenate 17 registers together
function Reg#(Bit#(n)) concatReg17(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17));
endfunction

/// Concatenate 18 registers together
function Reg#(Bit#(n)) concatReg18(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18));
endfunction

/// Concatenate 19 registers together
function Reg#(Bit#(n)) concatReg19(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19));
endfunction

/// Concatenate 20 registers together
function Reg#(Bit#(n)) concatReg20(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20));
endfunction

/// Concatenate 21 registers together
function Reg#(Bit#(n)) concatReg21(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21));
endfunction

/// Concatenate 22 registers together
function Reg#(Bit#(n)) concatReg22(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22));
endfunction

/// Concatenate 23 registers together
function Reg#(Bit#(n)) concatReg23(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23));
endfunction

/// Concatenate 24 registers together
function Reg#(Bit#(n)) concatReg24(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24));
endfunction

/// Concatenate 25 registers together
function Reg#(Bit#(n)) concatReg25(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25));
endfunction

/// Concatenate 26 registers together
function Reg#(Bit#(n)) concatReg26(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25,
      Reg#(Bit#(n26)) r26
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25),n26,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25),asReg(r26));
endfunction

/// Concatenate 27 registers together
function Reg#(Bit#(n)) concatReg27(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25,
      Reg#(Bit#(n26)) r26,
      Reg#(Bit#(n27)) r27
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25),n26),n27,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25),asReg(r26),asReg(r27));
endfunction

/// Concatenate 28 registers together
function Reg#(Bit#(n)) concatReg28(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25,
      Reg#(Bit#(n26)) r26,
      Reg#(Bit#(n27)) r27,
      Reg#(Bit#(n28)) r28
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25),n26),n27),n28,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25),asReg(r26),asReg(r27),asReg(r28));
endfunction

/// Concatenate 29 registers together
function Reg#(Bit#(n)) concatReg29(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25,
      Reg#(Bit#(n26)) r26,
      Reg#(Bit#(n27)) r27,
      Reg#(Bit#(n28)) r28,
      Reg#(Bit#(n29)) r29
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25),n26),n27),n28),n29,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25),asReg(r26),asReg(r27),asReg(r28),asReg(r29));
endfunction

/// Concatenate 30 registers together
function Reg#(Bit#(n)) concatReg30(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25,
      Reg#(Bit#(n26)) r26,
      Reg#(Bit#(n27)) r27,
      Reg#(Bit#(n28)) r28,
      Reg#(Bit#(n29)) r29,
      Reg#(Bit#(n30)) r30
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25),n26),n27),n28),n29),n30,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25),asReg(r26),asReg(r27),asReg(r28),asReg(r29),asReg(r30));
endfunction

/// Concatenate 31 registers together
function Reg#(Bit#(n)) concatReg31(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25,
      Reg#(Bit#(n26)) r26,
      Reg#(Bit#(n27)) r27,
      Reg#(Bit#(n28)) r28,
      Reg#(Bit#(n29)) r29,
      Reg#(Bit#(n30)) r30,
      Reg#(Bit#(n31)) r31
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25),n26),n27),n28),n29),n30),n31,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25),asReg(r26),asReg(r27),asReg(r28),asReg(r29),asReg(r30),asReg(r31));
endfunction

/// Concatenate 32 registers together
function Reg#(Bit#(n)) concatReg32(
      Reg#(Bit#(n1)) r1,
      Reg#(Bit#(n2)) r2,
      Reg#(Bit#(n3)) r3,
      Reg#(Bit#(n4)) r4,
      Reg#(Bit#(n5)) r5,
      Reg#(Bit#(n6)) r6,
      Reg#(Bit#(n7)) r7,
      Reg#(Bit#(n8)) r8,
      Reg#(Bit#(n9)) r9,
      Reg#(Bit#(n10)) r10,
      Reg#(Bit#(n11)) r11,
      Reg#(Bit#(n12)) r12,
      Reg#(Bit#(n13)) r13,
      Reg#(Bit#(n14)) r14,
      Reg#(Bit#(n15)) r15,
      Reg#(Bit#(n16)) r16,
      Reg#(Bit#(n17)) r17,
      Reg#(Bit#(n18)) r18,
      Reg#(Bit#(n19)) r19,
      Reg#(Bit#(n20)) r20,
      Reg#(Bit#(n21)) r21,
      Reg#(Bit#(n22)) r22,
      Reg#(Bit#(n23)) r23,
      Reg#(Bit#(n24)) r24,
      Reg#(Bit#(n25)) r25,
      Reg#(Bit#(n26)) r26,
      Reg#(Bit#(n27)) r27,
      Reg#(Bit#(n28)) r28,
      Reg#(Bit#(n29)) r29,
      Reg#(Bit#(n30)) r30,
      Reg#(Bit#(n31)) r31,
      Reg#(Bit#(n32)) r32
    ) provisos (
      Add#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(TAdd#(n1,n2),n3),n4),n5),n6),n7),n8),n9),n10),n11),n12),n13),n14),n15),n16),n17),n18),n19),n20),n21),n22),n23),n24),n25),n26),n27),n28),n29),n30),n31),n32,n)
    );
  return concatReg(asReg(r1),asReg(r2),asReg(r3),asReg(r4),asReg(r5),asReg(r6),asReg(r7),asReg(r8),asReg(r9),asReg(r10),asReg(r11),asReg(r12),asReg(r13),asReg(r14),asReg(r15),asReg(r16),asReg(r17),asReg(r18),asReg(r19),asReg(r20),asReg(r21),asReg(r22),asReg(r23),asReg(r24),asReg(r25),asReg(r26),asReg(r27),asReg(r28),asReg(r29),asReg(r30),asReg(r31),asReg(r32));
endfunction

/// readOnlyReg function
function Reg#(t) readOnlyReg(t r);
  return (interface Reg;
    method t _read = r;
    method Action _write(t x) = noAction;
    endinterface);
endfunction

endpackage
