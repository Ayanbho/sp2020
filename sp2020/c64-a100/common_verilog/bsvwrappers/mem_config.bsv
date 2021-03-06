/* 
Copyright (c) 2013, IIT Madras All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions
  and the following disclaimer.  
* Redistributions in binary form must reproduce the above copyright notice, this list of 
  conditions and the following disclaimer in the documentation and/or other materials provided 
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

Author: Neel Gala
Email id: neelgala@gmail.com
Details: 

This module allows you to create the cache data tag arrays in various configurations. You
can define the number of banks and type of concatenation you would like using the various modules
described in this file. These are implemented as Dual-BRAM modules such that 1-port is a dedicated
read and the other is a dedicated write. This allows easy mapping to SRAM libraries having 1r+1w
configuration.

Three primary parameters are available: 
  n_entries: The total number of BRAM entries for the entire instance.
  datawidth: The total number of bits on a read-response.
  banks: the total number of banks present in an instance.

Four different types of modules are available:

  1. mkmem_config_h: This module banks the BRAM instances horizontally. That is the bit-width of
  each bank is now datawidth/banks. The provisos of the module require that datawidth%banks=0. Each
  bank however has the same number of entries. This module does not support byte-enables.

  2. mkmem_config_hbe: This module banks the BRAM instances horizontally. That is the bit-width of
  each bank is now datawidth/banks. The provisos of the module require that datawidth%banks=0. Each
  bank however has the same number of entries. This module supports byte-enables and the provisos
  require that (datawidth/8)%banks=0.

  3. mkmem_config_v: this module banks the BRAM instances vertically,  i.e. the number of entries
  per bank = n_entries/banks. The bit-width of each bank instances is however the same: datawidth.
  The provisos of this module require that: n_entries%banks=0. This module does not support
  byte-enables.

  4. mkmem_config_v: this module banks the BRAM instances vertically,  i.e. the number of entries
  per bank = n_entries/banks. The bit-width of each bank instances is however the same: datawidth.
  The provisos of this module require that: n_entries%banks=0. This module supports byte-enables and
  has an additional proviso of (datawidth%8)=0
--------------------------------------------------------------------------------------------------
*/
package mem_config;
 
  import BRAMCore::*;
  import DReg::*;
  import FIFOF::*;
  import SpecialFIFOs::*;
  import Assert::*;
  import bram_1rw::*;
  import bram_1r1w::*;
  import bram_2rw::*;
  import ecc_hamming :: * ;
  `include "Logger.bsv"
  
  interface Ifc_mem_config2rw#( numeric type n_entries, numeric type datawidth, numeric type banks);
    interface Ifc_mem_config1rw#(n_entries, datawidth, banks) p1;
    interface Ifc_mem_config1rw#(n_entries, datawidth, banks) p2;
  endinterface
  
  module mkmem_config2rw#(parameter Bool ramreg, parameter Bool bypass)
                                                  (Ifc_mem_config2rw#(n_entries, datawidth,  banks))
    provisos(
             Div#(datawidth, banks, bpb), 
             Mul#(bpb, banks, datawidth),
             Add#(a__, bpb, datawidth)
    );
    let v_bpb=valueOf(bpb);
    

    Ifc_bram_2rw#(TLog#(n_entries), bpb, n_entries) ram_single [valueOf(banks)];
    Reg#(Bit#(bpb)) rg_output_p1[valueOf(banks)][2];
    Reg#(Bit#(bpb)) rg_output_p2[valueOf(banks)][2];
    Reg#(Bit#(TLog#(n_entries))) rg_write_index <- mkReg(0);
    Reg#(Bit#(TLog#(n_entries))) rg_read_index <- mkReg(0);
    Reg#(Bit#(bpb)) rg_write_data[valueOf(banks)] ;
    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram_single[i]<-mkbram_2rw;
      rg_output_p1[i] <- mkCReg(2,0);
      rg_output_p2[i] <- mkCReg(2,0);
      rg_write_data[i] <- mkReg(0);
    end

    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output_p1(!ramreg);
        if((rg_read_index == rg_write_index) && bypass) begin
          rg_output_p1[i][0] <= rg_write_data[i];
        end
        else
          rg_output_p1[i][0]<=ram_single[i].response_a;
      endrule
      rule capture_output_reg_p1(ramreg);
        if((rg_read_index == rg_write_index) && bypass) begin
          rg_output_p1[i][1] <= rg_write_data[i];
        end
        else
          rg_output_p1[i][1]<=ram_single[i].response_a;
      endrule
      rule capture_output_p2(!ramreg);
        rg_output_p2[i][0]<=ram_single[i].response_b;
      endrule
      rule capture_output_reg_p2(ramreg);
        rg_output_p2[i][1]<=ram_single[i].response_b;
      endrule
    end

    interface p1 = interface Ifc_mem_config1rw
      method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
        for(Integer i=0;i<valueOf(banks);i=i+1) begin
          if(bank_en[i] == 1)
            ram_single[i].request_a(we, index, data[i*v_bpb+v_bpb-1:i*v_bpb]);
        end
        if(we == 0) begin
          rg_read_index <= index;
        end
      endmethod
      method Bit#(datawidth) read_response;
        Bit#(datawidth) data_resp=0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output_p1[i][1];
        end
        return data_resp;
      endmethod
    endinterface;
    interface p2 = interface Ifc_mem_config1rw
      method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
        for(Integer i=0;i<valueOf(banks);i=i+1) begin
          if(bank_en[i] == 1) begin
            ram_single[i].request_b(we, index, data[i*v_bpb+v_bpb-1:i*v_bpb]);
            if(we == 1)
              rg_write_data[i] <= data[i*v_bpb+v_bpb-1:i*v_bpb];
          end
        end
        if(we == 1) begin
          rg_write_index <= index;
        end
      endmethod
      method Bit#(datawidth) read_response;
        Bit#(datawidth) data_resp=0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output_p2[i][1];
        end
        return data_resp;
      endmethod
    endinterface;
  endmodule
  
  interface Ifc_mem_config2rw_ecc#( numeric type n_entries, numeric type datawidth, numeric type banks);
    interface Ifc_mem_config1rw_ecc#(n_entries, datawidth, banks) p1;
    interface Ifc_mem_config1rw_ecc#(n_entries, datawidth, banks) p2;
  endinterface
  
  module mkmem_config2rw_ecc#(parameter Bool ramreg, parameter Bool bypass)
                                                  (Ifc_mem_config2rw_ecc#(n_entries, datawidth,  banks))
    provisos(
             Div#(datawidth, banks, bpb), 
             Mul#(bpb, banks, datawidth),
             Add#(a__, bpb, datawidth),
             Add#(2, TLog#(bpb), ecc_size),
             Add#(b__, 2, TMul#(2, banks)),
             Add#(TLog#(bpb), c__, 6),
             Add#(d__, bpb, 64)

             // required by bsc
             ,Add#(e__, ecc_size, TMul#(banks, ecc_size))
    );
    let v_bpb=valueOf(bpb);
    let v_banks = valueOf(banks);
    let v_ecc_size = valueOf(ecc_size);
    
    Ifc_bram_2rw#(TLog#(n_entries), TAdd#(bpb,ecc_size), n_entries) ram_single [valueOf(banks)];

    Reg#(Bit#(bpb)) rg_output_p1[valueOf(banks)][2];
    Reg#(Bit#(1)) rg_output_sed_p1 [v_banks][2];
    Reg#(Bit#(1)) rg_output_ded_p1 [v_banks][2];
    Reg#(Bit#(ecc_size)) rg_output_chparity_p1 [v_banks][2];
    Reg#(Bit#(ecc_size)) rg_output_stparity_p1 [v_banks][2];

    Reg#(Bit#(bpb)) rg_output_p2[valueOf(banks)][2];
    Reg#(Bit#(1)) rg_output_sed_p2 [v_banks][2];
    Reg#(Bit#(1)) rg_output_ded_p2 [v_banks][2];
    Reg#(Bit#(ecc_size)) rg_output_chparity_p2 [v_banks][2];
    Reg#(Bit#(ecc_size)) rg_output_stparity_p2 [v_banks][2];

    Reg#(Bit#(TLog#(n_entries))) rg_write_index <- mkReg(0);
    Reg#(Bit#(TLog#(n_entries))) rg_read_index <- mkReg(0);
    Reg#(Bit#(bpb)) rg_write_data[valueOf(banks)] ;

    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram_single[i]<-mkbram_2rw;

      rg_output_p1[i] <- mkCReg(2,0);
      rg_output_sed_p1[i] <- mkCReg(2,0);
      rg_output_ded_p1[i] <- mkCReg(2,0);
      rg_output_chparity_p1[i] <- mkCReg(2,0);
      rg_output_stparity_p1[i] <- mkCReg(2,0);

      rg_output_p2[i] <- mkCReg(2,0);
      rg_output_sed_p2[i] <- mkCReg(2,0);
      rg_output_ded_p2[i] <- mkCReg(2,0);
      rg_output_chparity_p2[i] <- mkCReg(2,0);
      rg_output_stparity_p2[i] <- mkCReg(2,0);

      rg_write_data[i] <- mkReg(0);
    end

    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output_p1(!ramreg);

        let resp = ram_single[i].response_a;
        Bit#(bpb) data = truncateLSB(resp);
        Bit#(ecc_size) parity = truncate(resp);
        let {check_parity, sed_ded} = fn_ecc_detect(data,parity);
        if((rg_read_index == rg_write_index) && bypass) begin
          data = rg_write_data[i];
          sed_ded = 0;
        end

        rg_output_p1[i][0] <= data;
        rg_output_sed_p1[i][0] <= sed_ded[1];
        rg_output_ded_p1[i][0] <= sed_ded[0];
        rg_output_chparity_p1[i][0] <= check_parity;
        rg_output_stparity_p1[i][0] <= parity;
      endrule
      rule capture_output_reg_p1(ramreg);
        let resp = ram_single[i].response_a;
        Bit#(bpb) data = truncateLSB(resp);
        Bit#(ecc_size) parity = truncate(resp);
        let {check_parity, sed_ded} = fn_ecc_detect(data,parity);
        if((rg_read_index == rg_write_index) && bypass) begin
          data = rg_write_data[i];
          sed_ded = 0;
        end

        rg_output_p1[i][1] <= data;
        rg_output_sed_p1[i][1] <= sed_ded[1];
        rg_output_ded_p1[i][1] <= sed_ded[0];
        rg_output_chparity_p1[i][1] <= check_parity;
        rg_output_stparity_p1[i][1] <= parity;
      endrule
      rule capture_output_p2(!ramreg);

        let resp = ram_single[i].response_b;
        Bit#(bpb) data = truncateLSB(resp);
        Bit#(ecc_size) parity = truncate(resp);
        let {check_parity, sed_ded} = fn_ecc_detect(data,parity);
        if((rg_read_index == rg_write_index) && bypass) begin
          data = rg_write_data[i];
          sed_ded = 0;
        end

        rg_output_p2[i][0] <= data;
        rg_output_sed_p2[i][0] <= sed_ded[1];
        rg_output_ded_p2[i][0] <= sed_ded[0];
        rg_output_chparity_p2[i][0] <= check_parity;
        rg_output_stparity_p2[i][0] <= parity;
      endrule
      rule capture_output_reg_p2(ramreg);
        let resp = ram_single[i].response_b;
        Bit#(bpb) data = truncateLSB(resp);
        Bit#(ecc_size) parity = truncate(resp);
        let {check_parity, sed_ded} = fn_ecc_detect(data,parity);
        if((rg_read_index == rg_write_index) && bypass) begin
          data = rg_write_data[i];
          sed_ded = 0;
        end

        rg_output_p2[i][1] <= data;
        rg_output_sed_p2[i][1] <= sed_ded[1];
        rg_output_ded_p2[i][1] <= sed_ded[0];
        rg_output_chparity_p2[i][1] <= check_parity;
        rg_output_stparity_p2[i][1] <= parity;
      endrule
    end

    interface p1 = interface Ifc_mem_config1rw_ecc
      method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
        for(Integer i=0;i<valueOf(banks);i=i+1) begin
          if(bank_en[i] == 1)
            ram_single[i].request_a(we, index, data[i*v_bpb+v_bpb-1:i*v_bpb]);
        end
        if(we == 0) begin
          rg_read_index <= index;
        end
      endmethod
      method Bit#(datawidth) read_response;
        Bit#(datawidth) data_resp=0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output_p1[i][1];
        end
        return data_resp;
      endmethod
      method Bit#(banks) read_sed;
        Bit#(banks) ecc_sed_resp = 0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          ecc_sed_resp[i] = rg_output_sed_p1[i][1];
        end
        return ecc_sed_resp;
      endmethod
      method Bit#(banks) read_ded;
        Bit#(banks) ecc_ded_resp = 0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          ecc_ded_resp[i] = rg_output_ded_p1[i][1];
        end
        return ecc_ded_resp;
      endmethod
      method Bit#(TMul#(banks,ecc_size)) check_parity;
        Bit#(TMul#(banks,ecc_size)) _t=?;
        for (Integer i = 0; i< v_banks; i = i + 1) begin
          _t[i*v_ecc_size + v_ecc_size -1:i*v_ecc_size] = rg_output_chparity_p1[i][1];
        end
        return _t;
      endmethod
      method Bit#(TMul#(banks,ecc_size)) stored_parity;
        Bit#(TMul#(banks,ecc_size)) _t=?;
        for (Integer i = 0; i< v_banks; i = i + 1) begin
          _t[i*v_ecc_size + v_ecc_size -1:i*v_ecc_size] = rg_output_stparity_p1[i][1];
        end
        return _t;
      endmethod
    endinterface;
    interface p2 = interface Ifc_mem_config1rw_ecc
      method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
        if(we == 1) begin
          rg_write_index <= index;
        end
        for(Integer i=0;i<valueOf(banks);i=i+1) begin
          Bit#(bpb) lv_data = data[i*v_bpb+v_bpb-1:i*v_bpb];
          Bit#(ecc_size) lv_ecc = fn_ecc_encode(lv_data);
          if(bank_en[i] == 1) begin
            ram_single[i].request_b(we, index, {lv_data,lv_ecc});
            if(we == 1)
              rg_write_data[i] <= data[i*v_bpb+v_bpb-1:i*v_bpb];
          end
        end
      endmethod
      method Bit#(datawidth) read_response;
        Bit#(datawidth) data_resp=0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output_p2[i][1];
        end
        return data_resp;
      endmethod
      method Bit#(banks) read_sed;
        Bit#(banks) ecc_sed_resp = 0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          ecc_sed_resp[i] = rg_output_sed_p2[i][1];
        end
        return ecc_sed_resp;
      endmethod
      method Bit#(banks) read_ded;
        Bit#(banks) ecc_ded_resp = 0;
        for(Integer i=0;i<valueOf(banks);i=i+1)begin
          ecc_ded_resp[i] = rg_output_ded_p2[i][1];
        end
        return ecc_ded_resp;
      endmethod
      method Bit#(TMul#(banks,ecc_size)) check_parity;
        Bit#(TMul#(banks,ecc_size)) _t=?;
        for (Integer i = 0; i< v_banks; i = i + 1) begin
          _t[i*v_ecc_size + v_ecc_size -1:i*v_ecc_size] = rg_output_chparity_p2[i][1];
        end
        return _t;
      endmethod
      method Bit#(TMul#(banks,ecc_size)) stored_parity;
        Bit#(TMul#(banks,ecc_size)) _t=?;
        for (Integer i = 0; i< v_banks; i = i + 1) begin
          _t[i*v_ecc_size + v_ecc_size -1:i*v_ecc_size] = rg_output_stparity_p2[i][1];
        end
        return _t;
      endmethod
    endinterface;
  endmodule
  
  interface Ifc_mem_config1rw_ecc#( numeric type n_entries, 
                                    numeric type datawidth, 
                                    numeric type banks);
    method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
    method Bit#(datawidth) read_response;
    method Bit#(banks) read_sed;
    method Bit#(banks) read_ded;
    method Bit#(TMul#(banks,TAdd#(2,TLog#(TDiv#(datawidth,banks))))) check_parity;
    method Bit#(TMul#(banks,TAdd#(2,TLog#(TDiv#(datawidth,banks))))) stored_parity;
  endinterface
  
  module mkmem_config1rw_ecc#(parameter Bool ramreg)(Ifc_mem_config1rw_ecc#(n_entries, datawidth,  banks))
    provisos(
             Div#(datawidth, banks, bpb), 
             Mul#(bpb, banks, datawidth),
             Add#(a__, bpb, datawidth),
             Add#(2, TLog#(bpb), ecc_size),
             Add#(b__, 2, TMul#(2, banks)),
             Add#(TLog#(bpb), c__, 6),
             Add#(d__, bpb, 64)

             // required by bsc
             ,Add#(e__, ecc_size, TMul#(banks, ecc_size))
    );
    let v_bpb=valueOf(bpb);
    let v_banks = valueOf(banks);
    let v_ecc_size = valueOf(ecc_size);

    Ifc_bram_1rw#(TLog#(n_entries), TAdd#(bpb,ecc_size), n_entries) ram_single [valueOf(banks)];

    Reg#(Bit#(bpb)) rg_output_data [v_banks][2];
    Reg#(Bit#(1)) rg_output_sed [v_banks][2];
    Reg#(Bit#(1)) rg_output_ded [v_banks][2];
    Reg#(Bit#(ecc_size)) rg_output_chparity [v_banks][2];
    Reg#(Bit#(ecc_size)) rg_output_stparity [v_banks][2];

    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram_single[i]<-mkbram_1rw;
      rg_output_data[i] <- mkCReg(2,0);
      rg_output_sed[i] <- mkCReg(2,0);
      rg_output_ded[i] <- mkCReg(2,0);
      rg_output_chparity[i] <- mkCReg(2,0);
      rg_output_stparity[i] <- mkCReg(2,0);
    end

    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output(!ramreg);
        let resp = ram_single[i].response;
        Bit#(bpb) data = truncateLSB(resp);
        rg_output_data[i][0] <= data;

        Bit#(ecc_size) parity = truncate(resp);
        let {check_parity, sed_ded} = fn_ecc_detect(data,parity);
        rg_output_sed[i][0] <= sed_ded[1];
        rg_output_ded[i][0] <= sed_ded[0];
        rg_output_chparity[i][0] <= check_parity;
        rg_output_stparity[i][0] <= parity;
      endrule
      rule capture_output_reg(ramreg);
        let resp = ram_single[i].response;
        Bit#(bpb) data = truncateLSB(resp);
        rg_output_data[i][1] <= data;

        Bit#(ecc_size) parity = truncate(resp);
        let {check_parity, sed_ded} = fn_ecc_detect(data,parity);
        rg_output_sed[i][1] <= sed_ded[1];
        rg_output_ded[i][1] <= sed_ded[0];
        rg_output_chparity[i][1] <= check_parity;
        rg_output_stparity[i][1] <= parity;
      endrule
    end

    method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
      for(Integer i=0;i<valueOf(banks);i=i+1) begin
        Bit#(bpb) lv_data = data[i*v_bpb+v_bpb-1:i*v_bpb];
        Bit#(ecc_size) lv_ecc = fn_ecc_encode(lv_data);
        if(bank_en[i] == 1)
          ram_single[i].request(we, index, {lv_data,lv_ecc});
      end
    endmethod
    method Bit#(datawidth) read_response;
      Bit#(datawidth) data_resp=0;
      for(Integer i=0;i<valueOf(banks);i=i+1)begin
        data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output_data[i][1];
      end
      return data_resp;
    endmethod
    method Bit#(banks) read_sed;
      Bit#(banks) ecc_sed_resp = 0;
      for(Integer i=0;i<valueOf(banks);i=i+1)begin
        ecc_sed_resp[i] = rg_output_sed[i][1];
      end
      return ecc_sed_resp;
    endmethod
    method Bit#(banks) read_ded;
      Bit#(banks) ecc_ded_resp = 0;
      for(Integer i=0;i<valueOf(banks);i=i+1)begin
        ecc_ded_resp[i] = rg_output_ded[i][1];
      end
      return ecc_ded_resp;
    endmethod
    method Bit#(TMul#(banks,ecc_size)) check_parity;
      Bit#(TMul#(banks,ecc_size)) _t=?;
      for (Integer i = 0; i< v_banks; i = i + 1) begin
        _t[i*v_ecc_size + v_ecc_size -1:i*v_ecc_size] = rg_output_chparity[i][1];
      end
      return _t;
    endmethod
    method Bit#(TMul#(banks,ecc_size)) stored_parity;
      Bit#(TMul#(banks,ecc_size)) _t=?;
      for (Integer i = 0; i< v_banks; i = i + 1) begin
        _t[i*v_ecc_size + v_ecc_size -1:i*v_ecc_size] = rg_output_stparity[i][1];
      end
      return _t;
    endmethod
  endmodule

  interface Ifc_mem_config1rw#( numeric type n_entries, numeric type datawidth, numeric type banks);
    method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
    method Bit#(datawidth) read_response;
  endinterface
  
  module mkmem_config1rw#(parameter Bool ramreg)(Ifc_mem_config1rw#(n_entries, datawidth,  banks))
    provisos(
             Div#(datawidth, banks, bpb), 
             Mul#(bpb, banks, datawidth),
             Add#(a__, bpb, datawidth)
    );
    let v_bpb=valueOf(bpb);
    

    Ifc_bram_1rw#(TLog#(n_entries), bpb, n_entries) ram_single [valueOf(banks)];
    Reg#(Bit#(bpb)) rg_output[valueOf(banks)][2];
    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram_single[i]<-mkbram_1rw;
      rg_output[i] <- mkCReg(2,0);
    end

    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output(!ramreg);
        rg_output[i][0]<=ram_single[i].response;
      endrule
      rule capture_output_reg(ramreg);
        rg_output[i][1]<=ram_single[i].response;
      endrule
    end

    method Action request(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                          Bit#(banks) bank_en);
      for(Integer i=0;i<valueOf(banks);i=i+1) begin
        if(bank_en[i] == 1)
          ram_single[i].request(we, index, data[i*v_bpb+v_bpb-1:i*v_bpb]);
      end
    endmethod
    method Bit#(datawidth) read_response;
      Bit#(datawidth) data_resp=0;
      for(Integer i=0;i<valueOf(banks);i=i+1)begin
        data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output[i][1];
      end
      return data_resp;
    endmethod
  endmodule
  
  interface Ifc_mem_config1r1w#( numeric type n_entries, numeric type datawidth, numeric type banks);
    method Action write(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                                                                              Bit#(banks)  bank_en);
    method Action read(Bit#(TLog#(n_entries)) index);
    method Bit#(datawidth) read_response;
  endinterface
  
  module mkmem_config1r1w#(parameter Bool ramreg, parameter Bool bypass)
                                                  (Ifc_mem_config1r1w#(n_entries, datawidth,  banks))
    provisos(
             Div#(datawidth, banks, bpb), 
             Mul#(bpb, banks, datawidth),
             Add#(a__, bpb, datawidth)
    );
    let v_bpb=valueOf(bpb);
    

    Ifc_bram_1r1w#(TLog#(n_entries), bpb, n_entries) ram_double [valueOf(banks)];
    Reg#(Bit#(bpb)) rg_output[valueOf(banks)][2];
    Reg#(Bit#(TLog#(n_entries))) rg_write_index <- mkReg(0);
    Reg#(Bit#(TLog#(n_entries))) rg_read_index <- mkReg(0);
    Reg#(Bit#(bpb)) rg_write_data[valueOf(banks)] ;
    for(Integer i=0;i<valueOf(banks);i=i+1) begin
      ram_double[i]<-mkbram_1r1w;
      rg_output[i] <- mkCReg(2,0);
      rg_write_data[i] <- mkReg(0);
    end

    for(Integer i=0;i<valueOf(banks);i=i+1)begin
      rule capture_output(!ramreg);
        if((rg_read_index == rg_write_index) && bypass) begin
          rg_output[i][0] <= rg_write_data[i];
        end
        else
          rg_output[i][0]<=ram_double[i].response;
      endrule
      rule capture_output_reg(ramreg);
        if((rg_read_index == rg_write_index) && bypass) begin
          rg_output[i][1] <= rg_write_data[i];
        end
        else
          rg_output[i][1]<=ram_double[i].response;
      endrule
    end

    method Action write(Bit#(1) we, Bit#(TLog#(n_entries)) index, Bit#(datawidth) data, 
                                                                              Bit#(banks)  bank_en);
      for(Integer i=0;i<valueOf(banks);i=i+1) begin
        if(bank_en[i] == 1) begin
          ram_double[i].write(data[i*v_bpb+v_bpb-1:i*v_bpb], index, we);
          rg_write_data[i] <= data[i*v_bpb+v_bpb-1:i*v_bpb];
        end
      end
        rg_write_index <= index;
    endmethod
    method Action read(Bit#(TLog#(n_entries)) index);
      for(Integer i=0;i<valueOf(banks);i=i+1) begin
        ram_double[i].read(index);
      end
      rg_read_index <= index;
    endmethod
    method Bit#(datawidth) read_response;
      Bit#(datawidth) data_resp=0;
      for(Integer i=0;i<valueOf(banks);i=i+1)begin
        data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output[i][1];
      end
      return data_resp;
    endmethod
  endmodule

  
//  
//
//  interface Ifc_mem_config#( numeric type n_entries, numeric type datawidth, numeric type banks);
//    method Action read_request(Bit#(TLog#(n_entries)) index);
//    method ActionValue#(Bit#(datawidth)) read_response;
//    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data);
//  endinterface
//  
//  interface Ifc_mem_config_be#( numeric type n_entries, numeric type datawidth, numeric type banks);
//    method Action read_request(Bit#(TLog#(n_entries)) index);
//    method Bit#(datawidth) read_response;
//    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data,
//        Bit#(TDiv#(datawidth, 8)) we);
//  endinterface
//  // TODO check if single-port BRAMs can be instantiated through a parameter.
//  module mkmem_config_h#(parameter Bool ramreg, parameter String porttype)(Ifc_mem_config#(n_entries, datawidth,  banks))
//    provisos(
//             Div#(datawidth, banks, bpb), 
//             Mul#(bpb, banks, datawidth),
//             Add#(a__, bpb, datawidth)
//    );
//    let v_bpb=valueOf(bpb);
//    
//    staticAssert(porttype=="single" || porttype=="dual","Only supported porttypes are: single, dual");
//
////    BRAM_DUAL_PORT#(Bit#(TLog#(n_entries)), Bit#(bpb)) ram_double [valueOf(banks)];
////    BRAM_PORT#(Bit#(TLog#(n_entries)), Bit#(bpb)) ram_single [valueOf(banks)];
//    Ifc_bram_1r1w#(TLog#(n_entries), bpb, n_entries) ram_double [valueOf(banks)];
//    Ifc_bram_1rw#(TLog#(n_entries), bpb, n_entries) ram_single [valueOf(banks)];
//    Reg#(Bit#(bpb)) rg_output[valueOf(banks)][2];
//    for(Integer i=0;i<valueOf(banks);i=i+1) begin
//      if(porttype=="single")
//        ram_single[i]<-mkbram_1rw;
//      else
//        ram_double[i]<-mkbram_1r1w;
//      rg_output[i] <- mkCReg(2,0);
//    end
//
//    Reg#(Bool) rg_read_req_made <- mkDReg(False);
//    for(Integer i=0;i<valueOf(banks);i=i+1)begin
//      rule capture_output(/*rg_read_req_made &&*/ !ramreg);
//        if(porttype=="single")
//          rg_output[i][0]<=ram_single[i].response;
//        else
//          rg_output[i][0]<=ram_double[i].response;
//      endrule
//      rule capture_output_reg(ramreg);
//        if(porttype=="single")
//          rg_output[i][1]<=ram_single[i].response;
//        else
//          rg_output[i][1]<=ram_double[i].response;
//      endrule
//    end
//
//    method Action read_request(Bit#(TLog#(n_entries)) index);
//      for(Integer i=0;i<valueOf(banks);i=i+1) begin
//        if(porttype=="single")
//          ram_single[i].request(?, index, 0);
//        else
//          ram_double[i].read(index);
//      end
//      rg_read_req_made<=True;
//    endmethod
//    method ActionValue#(Bit#(datawidth)) read_response;
//      Bit#(datawidth) data_resp=0;
//      for(Integer i=0;i<valueOf(banks);i=i+1)begin
//        data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output[i][1];
//      end
//      return data_resp;
//    endmethod
//    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data);
//      for(Integer i=0;i<valueOf(banks);i=i+1)begin
//        if (porttype=="single")
//          ram_single[i].request(data[i*v_bpb+v_bpb-1:i*v_bpb], address, 1);
//        else
//          ram_double[i].write(data[i*v_bpb+v_bpb-1:i*v_bpb], address, 1);
//      end
//    endmethod
//  endmodule
//  
//  module mkmem_config_hbe#(parameter Bool ramreg)(Ifc_mem_config_be#(n_entries, datawidth,  banks))
//    provisos(Div#(datawidth, banks, bpb), 
//             Mul#(bpb, banks, datawidth), 
//             Div#(bpb, 8, bytes), 
//             Div#(datawidth, 8, totalbytes), 
//             Div#(totalbytes, banks, bytes_pbank), 
//             Add#(a__, TDiv#(datawidth, banks), datawidth),
//             // compiler required provisos
//             Add#(b__, bpb, datawidth), // datawidth is atleast bpb wide
//             Mul#(TDiv#(bpb, bytes_pbank), bytes_pbank, bpb)
//    );
//
//    let v_bpb=valueOf(bpb);
//    let bytes_per_bank=valueOf(bytes_pbank);
//    
//    BRAM_DUAL_PORT_BE#(Bit#(TLog#(n_entries)), Bit#(bpb), bytes_pbank) ram [valueOf(banks)];
//    Reg#(Bit#(bpb)) rg_output[valueOf(banks)][2];
//    for(Integer i=0;i<valueOf(banks);i=i+1) begin
//      ram[i]<-mkBRAMCore2BE(valueOf(n_entries), False);
//      rg_output[i] <- mkCReg(2,0);
//    end
//    Reg#(Bool) rg_read_req_made <- mkDReg(False);
//    
//    for(Integer i=0;i<valueOf(banks);i=i+1)begin
//      rule capture_output(rg_read_req_made && !ramreg);
//        rg_output[i][0]<=ram[i].a.read;
//      endrule
//      rule capture_output_reg(ramreg);
//        rg_output[i][1]<=ram[i].a.read;
//      endrule
//    end
//    
//    method Action read_request(Bit#(TLog#(n_entries)) index);
//      for(Integer i=0;i<valueOf(banks);i=i+1)
//        ram[i].a.put(0, index,  ?);
//      rg_read_req_made<=True;
//    endmethod
//    method Bit#(datawidth) read_response;
//      Bit#(datawidth) data_resp=0;
//      for(Integer i=0;i<valueOf(banks);i=i+1)
//        data_resp[i*v_bpb+v_bpb-1 : i*v_bpb]=rg_output[i][1];
//      return data_resp;
//    endmethod
//    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data,
//        Bit#(TDiv#(datawidth, 8)) we);
//      for(Integer i=0;i<valueOf(banks);i=i+1)
//        ram[i].b.put(we[i*bytes_per_bank+bytes_per_bank-1:i*bytes_per_bank], address, 
//                                            data[i*v_bpb+v_bpb-1:i*v_bpb]);
//    endmethod
//  endmodule:mkmem_config_hbe
//  
//  module mkmem_config_v#(parameter Bool ramreg)(Ifc_mem_config#(n_entries, datawidth,  banks))
//    provisos( Log#(n_entries, log_entries), 
//              Div#(n_entries, banks, epb), 
//              Mul#(epb, banks, n_entries), 
//              Log#(epb, log_epb), 
//              Add#(a__, TDiv#(datawidth, banks), datawidth), 
//              Add#(b__, TLog#(TDiv#(n_entries, banks)), TLog#(n_entries)), 
//              Add#(TSub#(log_entries, log_epb), c__, TLog#(n_entries)));
//    Integer entries_per_bank=valueOf(epb);
//    Reg#(Bit#(TLog#(n_entries))) rg_address <- mkReg(0);
//    
//    BRAM_DUAL_PORT#(Bit#(TLog#(TDiv#(n_entries, banks))), Bit#(datawidth)) ram [valueOf(banks)];
//    Reg#(Bit#(datawidth)) rg_output[valueOf(banks)][2];
//    Reg#(Bool) rg_read_req_made <- mkDReg(False);
//    for(Integer i=0;i<valueOf(banks);i=i+1) begin
//      ram[i]<-mkBRAMCore2(valueOf(epb), False);
//      rg_output[i]<- mkCReg(2,0);
//    end
//    
//    for(Integer i=0;i<valueOf(banks);i=i+1)begin
//      rule capture_output(rg_read_req_made && !ramreg);
//        rg_output[i][0]<=ram[i].a.read;
//      endrule
//      rule capture_output_reg(ramreg);
//        rg_output[i][1]<=ram[i].a.read;
//      endrule
//    end
//    
//    method Action read_request(Bit#(TLog#(n_entries)) index);
//      for(Integer i=0;i<valueOf(banks);i=i+1)
//        ram[i].a.put(False, truncate(index),  ?);
//      rg_address<= index;
//      rg_read_req_made<=True;
//    endmethod
//    method ActionValue#(Bit#(datawidth)) read_response;
//      Bit#(datawidth) data_resp [valueOf(banks)];
//      for(Integer i=0;i<valueOf(banks);i=i+1)
//        data_resp[i]=rg_output[i][1];
//      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
//      return data_resp[selection_index];
//    endmethod
//    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data);
//      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
//      ram[selection_index].b.put(True, truncate(address), data);
//    endmethod
//  endmodule:mkmem_config_v
//  
//  module mkmem_config_vbe#(parameter Bool ramreg)(Ifc_mem_config_be#(n_entries, datawidth,  banks))
//    provisos( Log#(n_entries, log_entries), 
//              Div#(n_entries, banks, epb), 
//              Div#(datawidth, 8, we_line), 
//              Mul#(epb, banks, n_entries), 
//              Log#(epb, log_epb), 
//              Add#(a__, TDiv#(datawidth, banks), datawidth), 
//              Add#(b__, TLog#(TDiv#(n_entries, banks)), TLog#(n_entries)), 
//              Add#(TSub#(log_entries, log_epb), c__, TLog#(n_entries)), 
//              Mul#(TDiv#(datawidth, we_line), we_line, datawidth));
//    Integer entries_per_bank=valueOf(epb);
//    Reg#(Bit#(TLog#(n_entries))) rg_address <- mkReg(0);
//    
//    BRAM_DUAL_PORT_BE#(Bit#(TLog#(TDiv#(n_entries, banks))), Bit#(datawidth),  we_line) ram [valueOf(banks)];
//    Reg#(Bit#(datawidth)) rg_output[valueOf(banks)][2];
//    Reg#(Bool) rg_read_req_made <- mkDReg(False);
//    for(Integer i=0;i<valueOf(banks);i=i+1) begin
//      ram[i]<-mkBRAMCore2BE(valueOf(epb), False);
//      rg_output[i]<- mkCReg(2,0);
//    end
//    
//    for(Integer i=0;i<valueOf(banks);i=i+1)begin
//      rule capture_output(rg_read_req_made && !ramreg);
//        rg_output[i][0]<=ram[i].a.read;
//      endrule
//      rule capture_output_reg(ramreg);
//        rg_output[i][1]<=ram[i].a.read;
//      endrule
//    end
//    
//    method Action read_request(Bit#(TLog#(n_entries)) index);
//      for(Integer i=0;i<valueOf(banks);i=i+1)
//        ram[i].a.put(0, truncate(index),  ?);
//      rg_address<= index;
//      rg_read_req_made<=True;
//    endmethod
//    method Bit#(datawidth) read_response;
//      Bit#(datawidth) data_resp [valueOf(banks)];
//      for(Integer i=0;i<valueOf(banks);i=i+1)
//        data_resp[i]=rg_output[i][1];
//      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
//      return data_resp[selection_index];
//    endmethod
//    method Action write_request(Bit#(TLog#(n_entries)) address,  Bit#(datawidth) data,
//        Bit#(TDiv#(datawidth, 8)) we);
//      Bit#(TSub#(log_entries, log_epb)) selection_index=truncateLSB(rg_address);
//      ram[selection_index].b.put(we, truncate(address), data);
//    endmethod
//  endmodule:mkmem_config_vbe
//
////  (*synthesize*)
////  module mkTb(Empty);
////    Ifc_mem_config#(64, 256, 4) myram <- mkmem_config_h;
////    Ifc_mem_config#(64, 256, 4) myram1 <- mkmem_config_v;
////    Ifc_mem_config_be#(64, 256, 4) myram2 <- mkmem_config_hbe;
////    Ifc_mem_config_be#(64, 256, 4) myram3 <- mkmem_config_vbe;
////  endmodule
endpackage
