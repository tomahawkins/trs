module fifo_formalbench;
  wire reset = 1'b0;
  wire clock;
  wire enque, deque;
  wire [3:0] enque_data, deque_data, data0, data1, data2, data3;
  wire is_full, is_empty;

  fifo f0
    ( .reset
    , .clock
    , .enque
    , .deque
    , .enque_data
    , .deque_data
    , .enqued
    , .dequed
    , .is_full
    , .is_empty
    );

  // psl default clock = (posedge clock);

  // psl assmue always stable(data0);

  // psl assmue always stable(data1);

  // psl assmue always stable(data2);

  // psl assmue always stable(data3);

  // psl what_goes_in_may_come_out : assert always
  //     {! is_full && enque && enque_data == data0; enqued} |=>
  //     {(! is_empty)[*]; deque; (! dequed)[*]; dequed && deque_data == data0};

  // psl what_goes_in_can_be_forced_out : assert always ((always deque) -> (
  //     {! is_full && enque && enque_data == data0; enqued} |=>
  //     {[*]; dequed && deque_data == data0} !));

  // psl single_in_single_out : assert always
  //     {is_empty && enque && enque_data == data0; enqued; (! deque)[*]; deque; dequed} |->
  //     (deque_data == data0);

  // psl empty_to_full_to_empty : cover
  //     { is_empty && enque && enque_data == data0
  //     ; (! enqued)[*]
  //     ; enqued && enque && enque_data == data1
  //     ; (! enqued)[*]
  //     ; enqued && enque && enque_data == data2
  //     ; (! enqued)[*]
  //     ; enqued && enque && enque_data == data3
  //     ; (! enqued)[*]
  //     ; enqued && is_full
  //     ; deque
  //     ; (! dequed)[*]
  //     ; dequed && deque_data == data0 && deque
  //     ; (! dequed)[*]
  //     ; dequed && deque_data == data1 && deque
  //     ; (! dequed)[*]
  //     ; dequed && deque_data == data2 && deque
  //     ; (! dequed)[*]
  //     ; dequed && deque_data == data3 && is_empty
  //     };

  // psl immediate_enque : assert always {! is_full && enque} |=> enqued;

  // psl immediate_deque : assert always {! is_empty && deque} |=> dequed;

  // psl concurrent_enque_deque : assert always {! is_empty && enque && deque} |=> (enqued && dequed);


endmodule

