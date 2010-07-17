module sorter_testbench;
  reg  reset;
  reg  clock;
  wire load;
  wire [7:0] in0;
  wire [7:0] in1;
  wire [7:0] in2;
  wire [7:0] in3;
  wire [7:0] in4;
  wire [7:0] in5;
  wire [7:0] in6;
  wire [7:0] in7;
  wire sorted;
  wire [7:0] out0;
  wire [7:0] out1;
  wire [7:0] out2;
  wire [7:0] out3;
  wire [7:0] out4;
  wire [7:0] out5;
  wire [7:0] out6;
  wire [7:0] out7;

  sorter dut
    ( .reset(reset)
    , .clock(clock)
    , .in7(in7)
    , .in6(in6)
    , .in5(in5)
    , .in4(in4)
    , .in3(in3)
    , .in2(in2)
    , .in1(in1)
    , .in0(in0)
    , .load(load)
    , .sorted(sorted)
    , .out0(out0)
    , .out1(out1)
    , .out2(out2)
    , .out3(out3)
    , .out4(out4)
    , .out5(out5)
    , .out6(out6)
    , .out7(out7)
    );

  sorter_stimulus stim
    ( .reset(reset)
    , .clock(clock)
    , .in7(in7)
    , .in6(in6)
    , .in5(in5)
    , .in4(in4)
    , .in3(in3)
    , .in2(in2)
    , .in1(in1)
    , .in0(in0)
    , .load(load)
    , .sorted(sorted)
    , .out0(out0)
    , .out1(out1)
    , .out2(out2)
    , .out3(out3)
    , .out4(out4)
    , .out5(out5)
    , .out6(out6)
    , .out7(out7)
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
