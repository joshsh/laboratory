var a = 2; // global code
  function assign(x)
  {
    a = x;  // statement in a function, not global code
alert("a assigned the value " + x);
  }
