module timer_testbench;
  wire reset = 1'b0;
  wire clock;
  wire start;
  wire [7:0] count;
  wire done;

  timer dut
    ( .reset (reset)
    , .clock (clock)
    , .start (start)
    , .count (count)
    , .done  (done)
    );

  timer_monitor mon
    ( .reset
    , .clock
    , .start
    , .count
    , .done
    );

endmodule

