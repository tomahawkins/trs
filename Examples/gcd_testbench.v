module gcd_testbench;
  reg        reset;
  reg        clock;
  wire       load;
  wire [7:0] dataA;
  wire [7:0] dataB;
  wire       resultReady;
  wire [7:0] resultData;

  gcd dut
    ( .reset(reset)
    , .clock(clock)
    , .load(load)
    , .dataA(dataA)
    , .dataB(dataB)
    , .resultReady(resultReady)
    , .resultData(resultData)
    );

  gcd_stimulus stimulus
    ( .reset(reset)
    , .clock(clock)
    , .load(load)
    , .dataA(dataA)
    , .dataB(dataB)
    , .resultReady(resultReady)
    , .resultData(resultData)
    );

  initial begin
    reset = 0;
    clock = 0;
    #1 reset = 1;
    #1 reset = 0;
    forever begin
      #1 clock = 1;
      #1 clock = 0;
    end
  end

endmodule
