/* 
Copyright (c) 2018, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
* Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and / or other materials provided 
 with the distribution.  
* Neither the name of IIT Madras  nor the names of its contributors may be used to endorse or 
  promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--------------------------------------------------------------------------------------------------

Author : Neel Gala
Email id : neelgala@gmail.com
Details:

--------------------------------------------------------------------------------------------------
*/
package stack;
	import RegFile::*;
	interface Ifc_stack#(numeric type width, numeric type depth) ;
		method Action push(Bit#(width) addr) ;
		method Action pop ;
		method Bit#(width) top ;
		method Bool empty ;
    method Action clear ;
	endinterface

	module mkstack(Ifc_stack#(width, depth));
		Reg#(Bit#(TLog#(depth))) top_index <- mkReg(0);
		RegFile#(Bit#(TLog#(depth)), Bit#(width)) array_reg <- mkRegFileFull();
		method Action pop;
			top_index <= top_index - 1;
		endmethod
		method Bit#(width) top;
			return array_reg.sub(top_index - 1);
		endmethod
		method Action push(Bit#(width) addr);
			array_reg.upd(top_index, addr);
			top_index <= top_index + 1;
		endmethod
		method Bool empty;
			return (top_index == 0);
		endmethod
    method Action clear;
      top_index <= 0;
    endmethod
	endmodule

endpackage
