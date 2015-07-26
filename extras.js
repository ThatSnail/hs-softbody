function gettimeofday(st_time_val, st_time_zone) {
    /* Wow, much hacks. */
    var t = (new Date()).getTime();
    var sec = Math.floor(t / 1000);
    var usec = t * 1000 % 1000000;
    writeOffAddr("i32", 4, st_time_val, 0, sec);
    writeOffAddr("i32", 4, st_time_val, 1, usec);
}
