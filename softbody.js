function gettimeofday(st_time_val, st_time_zone) {
    /* Wow, much hacks. */
    var t = (new Date()).getTime();
    var sec = Math.floor(t / 1000);
    var usec = t * 1000 % 1000000;
    writeOffAddr("i32", 4, st_time_val, 0, sec);
    writeOffAddr("i32", 4, st_time_val, 1, usec);
}

// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=[0,new T(function(){return unCStr("Tried to deserialize long string to a Char");})],_1=[0],_2=function(_3){return E(E(_3)[2]);},_4=function(_5,_6){var _7=E(_6);return _7[0]==0?[0]:[1,new T(function(){return B(A(_5,[_7[1]]));}),new T(function(){return _4(_5,_7[2]);})];},_8=new T(function(){return unCStr("Tried to deserialie a non-array to a list!");}),_9=function(_a){return E(E(_a)[4]);},_b=new T(function(){return (function(_c){return [0,new T(function(){return _2(_c);}),function(_d){return (function(_e,_f){return [3,E(_4(new T(function(){return _2(_e);}),_f))];})(_c,_d);},new T(function(){return _9(_c);}),function(_d){return (function(_g,_h){var _i=E(_h);if(_i[0]==3){var _j=function(_k){var _l=E(_k);if(!_l[0]){return E([1,_1]);}else{var _m=B(A(new T(function(){return _9(_g);}),[_l[1]]));if(!_m[0]){return [0,_m[1]];}else{var _n=_j(_l[2]);return _n[0]==0?[0,_n[1]]:[1,[1,_m[1],_n[1]]];}}};return _j(_i[1]);}else{return E([0,_8]);}})(_c,_d);}];})([0,function(_o){return [1,toJSStr([1,_o,_1])];},function(_p){return [1,toJSStr(E(_p))];},function(_q){var _r=E(_q);if(_r[0]==1){var _s=fromJSStr(_r[1]);return _s[0]==0?E(_0):E(_s[2])[0]==0?[1,_s[1]]:E(_0);}else{return E([0,new T(function(){return unCStr("Tried to deserialize a non-string to a Char");})]);}},function(_t){var _u=E(_t);return _u[0]==1?[1,new T(function(){return fromJSStr(_u[1]);})]:E([0,new T(function(){return unCStr("Tried to deserialize a non-JSString to a JSString");})]);}]);}),_v=function(_w,_x){while(1){var _y=E(_w);if(!_y[0]){return E(_x)[0]==0?true:false;}else{var _z=E(_x);if(!_z[0]){return false;}else{if(E(_y[1])[1]!=E(_z[1])[1]){return false;}else{_w=_y[2];_x=_z[2];continue;}}}}},_A=function(_B){return E(E(_B)[1]);},_C=function(_D,_E,_F){var _G=E(_F);return [3,E([1,new T(function(){return A(_A,[_D,_G[1]]);}),[1,new T(function(){return A(_A,[_E,_G[2]]);}),_1]])];},_H=[0,new T(function(){return unCStr("Tried to deserialize a non-array into a pair!");})],_I=function(_J){return E(E(_J)[3]);},_K=function(_L,_M,_N){var _O=E(_N);if(_O[0]==3){var _P=E(_O[1]);if(!_P[0]){return E(_H);}else{var _Q=E(_P[2]);if(!_Q[0]){return E(_H);}else{if(!E(_Q[2])[0]){var _R=A(_I,[_L,_P[1]]);if(!_R[0]){return [0,_R[1]];}else{var _S=A(_I,[_M,_Q[1]]);return _S[0]==0?[0,_S[1]]:[1,[0,_R[1],_S[1]]];}}else{return E(_H);}}}}else{return E(_H);}},_T=function(_U,_V){return [0,function(_d){return _C(_U,_V,_d);},function(_d){return (function(_W,_X,_Y){return [3,E(_4(function(_d){return _C(_W,_X,_d);},_Y))];})(_U,_V,_d);},function(_d){return _K(_U,_V,_d);},function(_d){return (function(_Z,_10,_11){var _12=E(_11);if(_12[0]==3){var _13=function(_14){var _15=E(_14);if(!_15[0]){return E([1,_1]);}else{var _16=_K(_Z,_10,_15[1]);if(!_16[0]){return [0,_16[1]];}else{var _17=_13(_15[2]);return _17[0]==0?[0,_17[1]]:[1,[1,_16[1],_17[1]]];}}};return _13(_12[1]);}else{return E([0,_8]);}})(_U,_V,_d);}];},_18=function(_19){return [0,E(_19)[1]];},_1a=new T(function(){return unCStr("Tried to deserialize a non-Number to a Float");}),_1b=function(_1c){var _1d=E(_1c);if(!_1d[0]){return E([1,_1]);}else{var _1e=E(_1d[1]);if(!_1e[0]){var _1f=_1b(_1d[2]);return _1f[0]==0?[0,_1f[1]]:[1,[1,[0,_1e[1]],_1f[1]]];}else{return E([0,_1a]);}}},_1g=[0,_18,function(_1h){return [3,E(_4(_18,_1h))];},function(_1i){var _1j=E(_1i);return _1j[0]==0?[1,[0,_1j[1]]]:E([0,_1a]);},function(_1k){var _1l=E(_1k);return _1l[0]==3?_1b(_1l[1]):E([0,_8]);}],_1m=new T(function(){return _T(_1g,_1g);}),_1n=[0,1],_1o=function(_1p,_1q){while(1){var _1r=(function(_1s,_1t){var _1u=E(_1t);switch(_1u[0]){case 0:return E(_1s);case 1:return [1,[0,[0,_1u[1]],_1u[2]],_1s];default:_1p=new T(function(){return _1o(_1s,_1u[4]);});_1q=_1u[3];return null;}})(_1p,_1q);if(_1r!=null){return _1r;}}},_1v=function(_1w,_1x){var _1y=E(_1w);return _1y[0]==0?E(_1x):[1,_1y[1],new T(function(){return _1v(_1y[2],_1x);})];},_1z=function(_1A,_1B){while(1){var _1C=(function(_1D,_1E){var _1F=E(_1E);switch(_1F[0]){case 0:return E(_1D);case 1:return [1,[0,[0,_1F[1]],_1F[2]],_1D];default:_1A=new T(function(){return _1z(_1D,_1F[4]);});_1B=_1F[3];return null;}})(_1A,_1B);if(_1C!=null){return _1C;}}},_1G=function(_1H){while(1){var _1I=(function(_1J){var _1K=E(_1J);if(!_1K[0]){return [0];}else{var _1L=_1K[2],_1M=E(_1K[1]),_1N=function(_1O){while(1){var _1P=(function(_1Q){var _1R=E(_1Q);if(!_1R[0]){return [0];}else{var _1S=_1R[2],_1T=E(_1R[1]),_1U=function(_1V){var _1W=E(_1V);return _1W[0]==0?[0]:[1,[0,_1M[1],_1T[1],_1W[1]],new T(function(){return _1U(_1W[2]);})];},_1X=_1U(_1T[2]);if(!_1X[0]){_1O=_1S;return null;}else{return [1,_1X[1],new T(function(){return _1v(_1X[2],new T(function(){return _1N(_1S);},1));})];}}})(_1O);if(_1P!=null){return _1P;}}},_1Y=E(E(_1M[2])[3]);switch(_1Y[0]){case 0:var _1Z=_1N(_1);if(!_1Z[0]){_1H=_1L;return null;}else{return [1,_1Z[1],new T(function(){return _1v(_1Z[2],new T(function(){return _1G(_1L);},1));})];}break;case 1:var _20=_1N([1,[0,[0,_1Y[1]],_1Y[2]],_1]);if(!_20[0]){_1H=_1L;return null;}else{return [1,_20[1],new T(function(){return _1v(_20[2],new T(function(){return _1G(_1L);},1));})];}break;default:if(!E(_1Y[1])){if(_1Y[2]>=0){var _21=_1N(_1z(_1,_1Y));if(!_21[0]){_1H=_1L;return null;}else{return [1,_21[1],new T(function(){return _1v(_21[2],new T(function(){return _1G(_1L);},1));})];}}else{var _22=_1N(_1z(new T(function(){return _1z(_1,_1Y[3]);}),_1Y[4]));if(!_22[0]){_1H=_1L;return null;}else{return [1,_22[1],new T(function(){return _1v(_22[2],new T(function(){return _1G(_1L);},1));})];}}}else{var _23=_1N(_1z(_1,_1Y));if(!_23[0]){_1H=_1L;return null;}else{return [1,_23[1],new T(function(){return _1v(_23[2],new T(function(){return _1G(_1L);},1));})];}}}}})(_1H);if(_1I!=null){return _1I;}}},_24=function(_25){var _26=E(_25);if(!_26[0]){return [0];}else{var _27=E(_26[1]);return [1,[0,_27[1],E(_27[2])[2]],new T(function(){return _24(_26[2]);})];}},_28=function(_29){var _2a=E(_29);if(!_2a[0]){return [0];}else{var _2b=E(_2a[1]);return [1,[0,_2b[1],E(_2b[2])[2]],new T(function(){return _28(_2a[2]);})];}},_2c=function(_2d){var _2e=E(_2d);if(!_2e[0]){return [0];}else{var _2f=E(_2e[1]);return [1,[0,_2f[1],E(_2f[2])[2]],new T(function(){return _2c(_2e[2]);})];}},_2g=function(_2h,_2i){while(1){var _2j=(function(_2k,_2l){var _2m=E(_2l);switch(_2m[0]){case 0:return E(_2k);case 1:return [1,[0,[0,_2m[1]],_2m[2]],_2k];default:_2h=new T(function(){return _2g(_2k,_2m[4]);});_2i=_2m[3];return null;}})(_2h,_2i);if(_2j!=null){return _2j;}}},_2n=[1,I_fromBits([3567587328,232])],_2o=function(_2p){return function(_2q){return _1v(new T(function(){return (function(_2r){var _2s=jsShow(E(_2r)[1]),_2t=_2s;return fromJSStr(_2t);})(_2p);}),_2q);};},_2u=0,_2v=[0,0],_2w=function(_2x,_2y){var _2z=E(_2x);if(!_2z[0]){var _2A=_2z[1],_2B=E(_2y);return _2B[0]==0?_2A==_2B[1]:I_compareInt(_2B[1],_2A)==0?true:false;}else{var _2C=_2z[1],_2D=E(_2y);return _2D[0]==0?I_compareInt(_2C,_2D[1])==0?true:false:I_compare(_2C,_2D[1])==0?true:false;}},_2E=function(_2F,_2G){var _2H=E(_2F);if(!_2H[0]){var _2I=_2H[1],_2J=E(_2G);return _2J[0]==0?_2I<_2J[1]:I_compareInt(_2J[1],_2I)>0;}else{var _2K=_2H[1],_2L=E(_2G);return _2L[0]==0?I_compareInt(_2K,_2L[1])<0:I_compare(_2K,_2L[1])<0;}},_2M=function(_2N,_2O){return !_2w(_2O,_2v)?[0,(function(_2P,_2Q){while(1){var _2R=E(_2P);if(!_2R[0]){_2P=[1,I_fromInt(_2R[1])];continue;}else{var _2S=E(_2Q);if(!_2S[0]){_2P=_2R;_2Q=[1,I_fromInt(_2S[1])];continue;}else{return I_fromRat(_2R[1],_2S[1]);}}}})(_2N,_2O)]:!_2w(_2N,_2v)?!_2E(_2N,_2v)?E(new T(function(){return [0,1/0];})):E(new T(function(){return [0,-1/0];})):E(new T(function(){return [0,0/0];}));},_2T=function(_2U,_2V){while(1){var _2W=E(_2V);switch(_2W[0]){case 0:return [0];case 1:return _2U!=_2W[1]?E(_2W):[0];default:var _2X=_2W[1],_2Y=_2W[2],_2Z=_2W[3],_30=_2W[4],_31=_2Y>>>0;if(((_2U>>>0&((_31-1>>>0^4294967295)>>>0^_31)>>>0)>>>0&4294967295)==_2X){if(!((_2U>>>0&_31)>>>0)){var _32=E(_30);if(!_32[0]){_2V=_2Z;continue;}else{var _33=_2T(_2U,_2Z);return _33[0]==0?E(_32):[2,_2X,_2Y,E(_33),E(_32)];}}else{var _34=_2T(_2U,_30);if(!_34[0]){return E(_2Z);}else{var _35=E(_2Z);return _35[0]==0?E(_34):[2,_2X,_2Y,E(_35),E(_34)];}}}else{return E(_2W);}}}},_36=[0],_37=function(_38,_39,_3a){while(1){var _3b=E(_3a);switch(_3b[0]){case 0:return [0];case 1:var _3c=_3b[1];if(_39!=_3c){return E(_3b);}else{var _3d=B(A(_38,[[0,_39],_3b[2]]));return _3d[0]==0?[0]:[1,_3c,_3d[1]];}break;default:var _3e=_3b[1],_3f=_3b[2],_3g=_3b[3],_3h=_3b[4],_3i=_3f>>>0;if(((_39>>>0&((_3i-1>>>0^4294967295)>>>0^_3i)>>>0)>>>0&4294967295)==_3e){if(!((_39>>>0&_3i)>>>0)){var _3j=E(_3h);if(!_3j[0]){_3a=_3g;continue;}else{var _3k=_37(_38,_39,_3g);return _3k[0]==0?E(_3j):[2,_3e,_3f,E(_3k),E(_3j)];}}else{var _3l=_37(_38,_39,_3h);if(!_3l[0]){return E(_3g);}else{var _3m=E(_3g);return _3m[0]==0?E(_3l):[2,_3e,_3f,E(_3m),E(_3l)];}}}else{return E(_3b);}}}},_3n=function(_3o,_3p){return _2T(E(_3o)[1],_3p);},_3q=function(_3r,_3s,_3t){while(1){var _3u=(function(_3v,_3w,_3x){var _3y=E(_3x);if(!_3y[0]){return E(_3v);}else{var _3z=_37(function(_3A,_3B){return [1,new T(function(){var _3C=E(_3B);return [0,new T(function(){return _3n(_3w,_3C[1]);}),_3C[2],_3C[3]];})];},E(_3y[1])[1],_3v),_3D=_3w;_3t=_3y[2];_3r=_3z;_3s=_3D;return null;}})(_3r,_3s,_3t);if(_3u!=null){return _3u;}}},_3E=function(_3F,_3G,_3H){while(1){var _3I=(function(_3J,_3K,_3L){var _3M=E(_3L);if(!_3M[0]){return E(_3J);}else{var _3N=_37(function(_3O,_3P){return [1,new T(function(){var _3Q=E(_3P);return [0,_3Q[1],_3Q[2],new T(function(){return _3n(_3K,_3Q[3]);})];})];},E(_3M[1])[1],_3J),_3R=_3K;_3H=_3M[2];_3F=_3N;_3G=_3R;return null;}})(_3F,_3G,_3H);if(_3I!=null){return _3I;}}},_3S=function(_3T,_3U){while(1){var _3V=(function(_3W,_3X){var _3Y=E(_3X);switch(_3Y[0]){case 0:return E(_3W);case 1:return [1,[0,_3Y[1]],_3W];default:_3T=new T(function(){return _3S(_3W,_3Y[4]);});_3U=_3Y[3];return null;}})(_3T,_3U);if(_3V!=null){return _3V;}}},_3Z=function(_40,_41){while(1){var _42=(function(_43,_44){var _45=E(_44);switch(_45[0]){case 0:return E(_43);case 1:return [1,[0,_45[1]],_43];default:_40=new T(function(){return _3Z(_43,_45[4]);});_41=_45[3];return null;}})(_40,_41);if(_42!=null){return _42;}}},_46=function(_47,_48){while(1){var _49=(function(_4a,_4b){var _4c=E(_4b);switch(_4c[0]){case 0:return E(_4a);case 1:return [1,[0,[0,_4c[1]],_4c[2]],_4a];default:_47=new T(function(){return _46(_4a,_4c[4]);});_48=_4c[3];return null;}})(_47,_48);if(_49!=null){return _49;}}},_4d=function(_4e){var _4f=E(_4e);if(!_4f[0]){return [0];}else{var _4g=E(_4f[1]),_4h=function(_4i){var _4j=E(_4i);return _4j[0]==0?E(new T(function(){return _4d(_4f[2]);})):[1,[0,_4j[1],_4g[1]],new T(function(){return _4h(_4j[2]);})];};return _4h(_4g[2]);}},_4k=function(_4l){var _4m=E(_4l);switch(_4m[0]){case 0:return E(new T(function(){return _4d(_1);}));case 1:return _4d([1,[0,[0,_4m[1]],_4m[2]],_1]);default:if(!E(_4m[1])){if(_4m[2]>=0){return _4d(_46(_1,_4m));}else{return _4d(_46(new T(function(){return _46(_1,_4m[3]);}),_4m[4]));}}else{return _4d(_46(_1,_4m));}}},_4n=function(_4o,_4p){var _4q=(function(_4r,_4s){return (function(_4t){while(1){var _4u=E(_4t);switch(_4u[0]){case 0:return [0];case 1:return _4r!=_4u[1]?[0]:[1,_4u[2]];default:if(!((_4r>>>0&_4u[2]>>>0)>>>0)){_4t=_4u[3];continue;}else{_4t=_4u[4];continue;}}}})(_4s);})(_4o,_4p);if(!_4q[0]){return [0,_36,_4p];}else{var _4v=E(_4q[1]),_4w=_4v[2],_4x=_4v[3],_4y=_2T(_4o,_4v[1]),_4z=_2T(_4o,_4p),_4A=[0,_4o],_4B=E(_2T(_4o,_4x));switch(_4B[0]){case 0:var _4C=_3q(_4z,_4A,_1);break;case 1:var _4C=_3q(_4z,_4A,[1,[0,_4B[1]],_1]);break;default:if(!E(_4B[1])){if(_4B[2]>=0){var _4D=_3q(_4z,_4A,_3S(_1,_4B));}else{var _4D=_3q(_4z,_4A,_3S(new T(function(){return _3S(_1,_4B[3]);}),_4B[4]));}var _4E=_4D,_4F=_4E;}else{var _4F=_3q(_4z,_4A,_3S(_1,_4B));}var _4C=_4F;}var _4G=_4C,_4H=E(_4y);switch(_4H[0]){case 0:var _4I=_3E(_4G,_4A,_1);break;case 1:var _4J=_4H[1],_4K=[0,_4J],_4L=[1,_4K,_1],_4I=_3E(_4G,_4A,_4L);break;default:var _4M=_4H[1],_4N=_4H[2],_4O=_4H[3],_4P=_4H[4],_4Q=E(_4M);if(!_4Q){var _4R=_4N<0,_4S=_4R;if(!_4S){var _4T=_3Z(_1,_4H),_4U=_3E(_4G,_4A,_4T),_4V=_4U;}else{var _4W=new T(function(){return _3Z(_1,_4O);}),_4X=_3Z(_4W,_4P),_4Y=_3E(_4G,_4A,_4X),_4V=_4Y;}var _4Z=_4V,_50=_4Z;}else{var _51=_3Z(_1,_4H),_52=_3E(_4G,_4A,_51),_50=_52;}var _4I=_50;}var _53=_4I,_54=new T(function(){return _4k(_4x);}),_55=new T(function(){return _4k(_4y);}),_56=[0,_55,_4A,_4w,_54],_57=[1,_56];return [0,_57,_53];}},_58=function(_59){return E(new T(function(){var _5a=hs_wordToWord64(4194982440),_5b=_5a,_5c=hs_wordToWord64(3110813675),_5d=_5c;return [0,_5b,_5d,[0,_5b,_5d,new T(function(){return unCStr("base");}),new T(function(){return unCStr("GHC.Exception");}),new T(function(){return unCStr("ArithException");})],_1];}));},_5e=function(_5f){return E(E(_5f)[1]);},_5g=function(_5h,_5i,_5j){var _5k=B(A(_5h,[_])),_5l=B(A(_5i,[_])),_5m=hs_eqWord64(_5k[1],_5l[1]),_5n=_5m;if(!E(_5n)){return [0];}else{var _5o=hs_eqWord64(_5k[2],_5l[2]),_5p=_5o;return E(_5p)==0?[0]:[1,_5j];}},_5q=new T(function(){return unCStr("arithmetic underflow");}),_5r=new T(function(){return unCStr("arithmetic overflow");}),_5s=new T(function(){return unCStr("Ratio has zero denominator");}),_5t=new T(function(){return unCStr("denormal");}),_5u=new T(function(){return unCStr("divide by zero");}),_5v=new T(function(){return unCStr("loss of precision");}),_5w=function(_5x){return _1v(_5q,_5x);},_5y=function(_5x){return _1v(_5r,_5x);},_5z=function(_5x){return _1v(_5s,_5x);},_5A=function(_5x){return _1v(_5t,_5x);},_5B=function(_5x){return _1v(_5u,_5x);},_5C=function(_5x){return _1v(_5v,_5x);},_5D=function(_5E,_5F,_5G){var _5H=E(_5F);return _5H[0]==0?unAppCStr("[]",_5G):[1,[0,91],new T(function(){return B(A(_5E,[_5H[1],new T(function(){var _5I=function(_5J){var _5K=E(_5J);return _5K[0]==0?E([1,[0,93],_5G]):[1,[0,44],new T(function(){return B(A(_5E,[_5K[1],new T(function(){return _5I(_5K[2]);})]));})];};return _5I(_5H[2]);})]));})];},_5L=function(_5x){return [0,new T(function(){return [0,_58,[0,function(_5M,_5N){switch(E(_5N)){case 0:return E(_5y);case 1:return E(_5w);case 2:return E(_5C);case 3:return E(_5B);case 4:return E(_5A);default:return E(_5z);}},function(_5O){switch(E(_5O)){case 0:return E(_5r);case 1:return E(_5q);case 2:return E(_5v);case 3:return E(_5u);case 4:return E(_5t);default:return E(_5s);}},function(_5P,_5Q){return _5D(function(_5R){switch(E(_5R)){case 0:return E(_5y);case 1:return E(_5w);case 2:return E(_5C);case 3:return E(_5B);case 4:return E(_5A);default:return E(_5z);}},_5P,_5Q);}],_5L,function(_5S){var _5T=E(_5S);return _5g(_5e(_5T[1]),_58,_5T[2]);}];}),_5x];},_5U=new T(function(){return die(new T(function(){return _5L(3);}));}),_5V=[0,0],_5W=function(_5X,_5Y){return !_2w(_5Y,_5V)?(function(_5Z,_60){while(1){var _61=E(_5Z);if(!_61[0]){var _62=E(_61[1]);if(_62==(-2147483648)){_5Z=[1,I_fromInt(-2147483648)];continue;}else{var _63=E(_60);if(!_63[0]){return [0,_62%_63[1]];}else{_5Z=[1,I_fromInt(_62)];_60=_63;continue;}}}else{var _64=_61[1],_65=E(_60);return _65[0]==0?[0,I_toInt(I_rem(_64,I_fromInt(_65[1])))]:[1,I_rem(_64,_65[1])];}}})(_5X,_5Y):E(_5U);},_66=[0,1],_67=function(_68,_69){while(1){var _6a=E(_68);if(!_6a[0]){var _6b=_6a[1],_6c=E(_69);if(!_6c[0]){var _6d=_6c[1],_6e=addC(_6b,_6d);if(!E(_6e[2])){return [0,_6e[1]];}else{_68=[1,I_fromInt(_6b)];_69=[1,I_fromInt(_6d)];continue;}}else{_68=[1,I_fromInt(_6b)];_69=_6c;continue;}}else{var _6f=E(_69);if(!_6f[0]){_68=_6a;_69=[1,I_fromInt(_6f[1])];continue;}else{return [1,I_add(_6a[1],_6f[1])];}}}},_6g=new T(function(){return _67([0,2147483647],_66);}),_6h=function(_6i){var _6j=E(_6i);if(!_6j[0]){var _6k=E(_6j[1]);return _6k==(-2147483648)?E(_6g):_6k<0?[0, -_6k]:E(_6j);}else{var _6l=_6j[1];return I_compareInt(_6l,0)>=0?E(_6j):[1,I_negate(_6l)];}},_6m=function(_6n,_6o){while(1){var _6p=E(_6n);if(!_6p[0]){var _6q=E(_6p[1]);if(_6q==(-2147483648)){_6n=[1,I_fromInt(-2147483648)];continue;}else{var _6r=E(_6o);if(!_6r[0]){return [0,quot(_6q,_6r[1])];}else{_6n=[1,I_fromInt(_6q)];_6o=_6r;continue;}}}else{var _6s=_6p[1],_6t=E(_6o);return _6t[0]==0?[0,I_toInt(I_quot(_6s,I_fromInt(_6t[1])))]:[1,I_quot(_6s,_6t[1])];}}},_6u=[0,0],_6v=[0,-1],_6w=function(_6x){var _6y=E(_6x);if(!_6y[0]){var _6z=_6y[1];return _6z>=0?E(_6z)==0?E(_6u):E(_66):E(_6v);}else{var _6A=I_compareInt(_6y[1],0);return _6A<=0?E(_6A)==0?E(_6u):E(_6v):E(_66);}},_6B=function(_6C,_6D){while(1){var _6E=E(_6C);if(!_6E[0]){var _6F=_6E[1],_6G=E(_6D);if(!_6G[0]){var _6H=_6G[1];if(!(imul(_6F,_6H)|0)){return [0,imul(_6F,_6H)|0];}else{_6C=[1,I_fromInt(_6F)];_6D=[1,I_fromInt(_6H)];continue;}}else{_6C=[1,I_fromInt(_6F)];_6D=_6G;continue;}}else{var _6I=E(_6D);if(!_6I[0]){_6C=_6E;_6D=[1,I_fromInt(_6I[1])];continue;}else{return [1,I_mul(_6E[1],_6I[1])];}}}},_6J=function(_6K,_6L,_6M,_6N){var _6O=_6B(_6L,_6M);return (function(_6P,_6Q){if(!_2w(_6Q,_5V)){var _6R=(function(_6S,_6T){while(1){if(!_2w(_6T,_5V)){var _6U=_6T,_6V=_5W(_6S,_6T);_6S=_6U;_6T=_6V;continue;}else{return E(_6S);}}})(_6h(_6P),_6h(_6Q));return !_2w(_6R,_5V)?[0,_6m(_6P,_6R),_6m(_6Q,_6R)]:E(_5U);}else{return E(new T(function(){return die(new T(function(){return _5L(5);}));}));}})(_6B(_6B(_6K,_6N),_6w(_6O)),_6h(_6O));},_6W=[0,45],_6X=[0,41],_6Y=[0,40],_6Z=function(_70,_71,_72){var _73=function(_74){var _75=new T(function(){return B(A(_70,[[0, -_72]]));});return E(_71)[1]<=6?function(_76){return [1,_6W,new T(function(){return B(A(_75,[_76]));})];}:function(_77){return [1,_6Y,[1,_6W,new T(function(){return B(A(_75,[[1,_6X,_77]]));})]];};};if(_72>=0){var _78=isFloatNegativeZero(_72),_79=_78;return E(_79)==0?B(A(_70,[[0,_72]])):_73(_);}else{return _73(_);}},_7a=function(_7b,_7c){var _7d=E(_7c);switch(_7d[0]){case 0:return [0];case 1:var _7e=_7d[1];return [1,_7e,new T(function(){return B(A(_7b,[[0,_7e],_7d[2]]));})];default:return [2,_7d[1],_7d[2],E(_7a(_7b,_7d[3])),E(_7a(_7b,_7d[4]))];}},_7f=function(_7g){return [0, -E(_7g)[1]];},_7h=function(_7i,_7j){if(_7j<=0){var _7k=function(_7l){var _7m=function(_7n){var _7o=function(_7p){var _7q=function(_7r){var _7s=isFloatNegativeZero(_7j),_7t=_7s,_7u=function(_7v){var _7w=E(E(_7i)[1]);if(!_7w){if(_7j>=0){var _7x=isFloatNegativeZero(_7j),_7y=_7x;return E(_7y)==0?E(_7j):3.141592653589793;}else{return 3.141592653589793;}}else{var _7z=E(_7j);return _7z==0?E(_7w):_7z+_7w;}};if(!E(_7t)){return _7u(_);}else{var _7A=E(_7i)[1],_7B=isFloatNegativeZero(_7A),_7C=_7B;if(!E(_7C)){return _7u(_);}else{return  -B(_7h([0, -_7A],_7j));}}};if(_7j>=0){return _7q(_);}else{var _7D=E(_7i)[1],_7E=isFloatNegativeZero(_7D),_7F=_7E;if(!E(_7F)){return _7q(_);}else{return  -B(_7h([0, -_7D],_7j));}}};if(_7j>0){return _7o(_);}else{var _7G=E(_7i)[1];if(_7G>=0){return _7o(_);}else{return  -B(_7h([0, -_7G],_7j));}}};if(_7j>=0){return new F(function(){return _7m(_);});}else{var _7H=E(_7i)[1];if(_7H<=0){return new F(function(){return _7m(_);});}else{return 3.141592653589793+Math.atan(_7H/_7j);}}};if(!E(_7j)){return E(_7i)[1]<=0?B(_7k(_)):1.5707963705062866;}else{return new F(function(){return _7k(_);});}}else{return Math.atan(E(_7i)[1]/_7j);}},_7I=function(_7J,_7K,_7L){return (function(_7M,_7N){var _7O=E(_7M);if(!_7O[0]){var _7P=E(_7N);return _7P[0]==0?[0]:E(_7P[2])[0]==0?[1,_7P[1],_1]:_1v(_1,_7P);}else{if(!E(_7O[2])[0]){return [1,_7O[1],_7N];}else{var _7Q=E(_7N);return _7Q[0]==0?E(_7O):E(_7Q[2])[0]==0?[1,_7Q[1],_7O]:_1v(_7O,_7Q);}}})(_7K,_7L);},_7R=function(_7S,_7T,_7U,_7V){var _7W=E(_7V);switch(_7W[0]){case 0:return [1,_7T,_7U];case 1:var _7X=_7W[1];if(_7T!=_7X){var _7Y=(_7T>>>0^_7X>>>0)>>>0,_7Z=(_7Y|_7Y>>>1)>>>0,_80=(_7Z|_7Z>>>2)>>>0,_81=(_80|_80>>>4)>>>0,_82=(_81|_81>>>8)>>>0,_83=(_82|_82>>>16)>>>0,_84=(_83^_83>>>1)>>>0&4294967295,_85=_84>>>0;return (_7T>>>0&_85)>>>0==0?[2,(_7T>>>0&((_85-1>>>0^4294967295)>>>0^_85)>>>0)>>>0&4294967295,_84,E([1,_7T,_7U]),E(_7W)]:[2,(_7T>>>0&((_85-1>>>0^4294967295)>>>0^_85)>>>0)>>>0&4294967295,_84,E(_7W),E([1,_7T,_7U])];}else{return [1,_7T,new T(function(){return B(A(_7S,[[0,_7T],_7U,_7W[2]]));})];}break;default:var _86=_7W[1],_87=_7W[2],_88=_7W[3],_89=_7W[4],_8a=_87>>>0;if(((_7T>>>0&((_8a-1>>>0^4294967295)>>>0^_8a)>>>0)>>>0&4294967295)==_86){return (_7T>>>0&_8a)>>>0==0?[2,_86,_87,E(_7R(_7S,_7T,_7U,_88)),E(_89)]:[2,_86,_87,E(_88),E(_7R(_7S,_7T,_7U,_89))];}else{var _8b=(_7T>>>0^_86>>>0)>>>0,_8c=(_8b|_8b>>>1)>>>0,_8d=(_8c|_8c>>>2)>>>0,_8e=(_8d|_8d>>>4)>>>0,_8f=(_8e|_8e>>>8)>>>0,_8g=(_8f|_8f>>>16)>>>0,_8h=(_8g^_8g>>>1)>>>0&4294967295,_8i=_8h>>>0;return (_7T>>>0&_8i)>>>0==0?[2,(_7T>>>0&((_8i-1>>>0^4294967295)>>>0^_8i)>>>0)>>>0&4294967295,_8h,E([1,_7T,_7U]),E(_7W)]:[2,(_7T>>>0&((_8i-1>>>0^4294967295)>>>0^_8i)>>>0)>>>0&4294967295,_8h,E(_7W),E([1,_7T,_7U])];}}},_8j=[0],_8k=function(_8l){return (function(_8m,_8n){return (function(_8o,_8p){while(1){var _8q=E(_8p);if(!_8q[0]){return E(_8o);}else{var _8r=E(_8q[1]),_8s=_7R(_8m,E(_8r[1])[1],_8r[2],_8o);_8p=_8q[2];_8o=_8s;continue;}}})(_8j,_8n);})(_7I,_4(function(_8t){var _8u=new T(function(){var _8v=E(_8t);return [0,_8v[2],_8v[1]];});return [0,new T(function(){return E(E(_8u)[1]);}),[1,new T(function(){return E(E(_8u)[2]);}),_1]];},_8l));},_8w=new T(function(){return err(new T(function(){return unCStr("Maybe.fromJust: Nothing");}));}),_8x=function(_8y,_8z,_8A){return [0,_2u,[0,new T(function(){var _8B=_7a(function(_8C,_8D){var _8E=E(_8D);return [0,_8E[1],new T(function(){var _8F=E(_8E[2]),_8G=E(_8F[1]),_8H=E(_8F[2]),_8I=new T(function(){var _8J=E(_8H[1]),_8K=_8J[1],_8L=E(_8G[1])[1]+_8K;if(_8L>=0){if(_8L<=800){var _8M=[0,[0,_8L],_8J];}else{var _8M=[0,[0,800-(_8L-800)],[0, -_8K]];}var _8N=_8M,_8O=_8N;}else{var _8O=[0,[0, -_8L],[0, -_8K]];}var _8P=_8O,_8Q=_8P,_8R=_8Q,_8S=_8R;return _8S;}),_8T=new T(function(){var _8U=E(_8H[2]),_8V=_8U[1],_8W=E(_8G[2])[1]+_8V;if(_8W>=0){if(_8W<=600){var _8X=[0,[0,_8W],_8U];}else{var _8X=[0,[0,600-(_8W-600)],[0, -_8V]];}var _8Y=_8X,_8Z=_8Y;}else{var _8Z=[0,[0, -_8W],[0, -_8V]];}var _90=_8Z,_91=_90,_92=_91,_93=_92;return _93;});return [0,[0,new T(function(){return E(E(_8I)[1]);}),new T(function(){return E(E(_8T)[1]);})],[0,new T(function(){return E(E(_8I)[2]);}),new T(function(){return E(E(_8T)[2]);})],_8F[3]];}),_8E[3]];},_8z),_94=function(_95,_96){return function(_97){var _98=E(_95),_99=_98[1],_9a=E(new T(function(){var _9b=E(_4n(E(_96)[1],_8B)[1]);if(!_9b[0]){var _9c=E(_8w);}else{var _9c=E(E(_9b[1])[3]);}var _9d=_9c,_9e=_9d;return _9e;}))[1],_9f=E(_97),_9g=_9f[1],_9h=_9f[2],_9i=_9f[3],_9j=new T(function(){return [0,1/E(_9i)[1]];}),_9k=new T(function(){var _9l=E(_99)[1],_9m=E(_9i)[1];return [0,(Math.sqrt(_9l*_9m)+Math.sqrt(_9l*_9m))* -E(_98[3])[1]];}),_9n=new T(function(){var _9o=E(_9g);return [0,new T(function(){return _7f(_9o[1]);}),new T(function(){return _7f(_9o[2]);})];}),_9p=new T(function(){return [0,E(E(_9a)[2])[1]+E(E(_9n)[2])[1]];}),_9q=new T(function(){return [0,E(E(_9a)[1])[1]+E(E(_9n)[1])[1]];}),_9r=new T(function(){return [0,B(_7h(_9p,E(_9q)[1]))];}),_9s=new T(function(){return [0, -(E(_99)[1]*(E(_98[2])[1]-Math.sqrt(Math.pow(E(_9q)[1],2)+Math.pow(E(_9p)[1],2))))/E(_9i)[1]];});return [0,_9g,[0,new T(function(){var _9t=E(E(_9h)[1])[1];return [0,_9t+(E(_9s)[1]*Math.cos(E(_9r)[1])+_9t*E(_9k)[1]*E(_9j)[1])*E(_8y)[1]];}),new T(function(){var _9u=E(E(_9h)[2])[1];return [0,_9u+(E(_9s)[1]*Math.sin(E(_9r)[1])+_9u*E(_9k)[1]*E(_9j)[1])*E(_8y)[1]];})],_9i];};};return _7a(function(_9v,_9w){var _9x=E(_9w);return [0,_9x[1],new T(function(){var _9y=E(_9x[2]),_9z=_9y[2];return [0,_9y[1],[0,new T(function(){return [0,E(E(_9z)[1])[1]+E(new T(function(){return [0,0*E(_8y)[1]];}))[1]];}),new T(function(){return [0,E(E(_9z)[2])[1]+E(new T(function(){return [0,9.81*E(_8y)[1]];}))[1]];})],_9y[3]];}),_9x[3]];},(function(_9A,_9B){return _7a(function(_9C,_9D){var _9E=B(A(_9A,[new T(function(){var _9F=E(_9D);return [0,new T(function(){return _4k(_9F[1]);}),_9C,_9F[2],new T(function(){return _4k(_9F[3]);})];})]));return [0,new T(function(){return _8k(_9E[1]);}),_9E[3],new T(function(){return _8k(_9E[4]);})];},_9B);})(function(_9G){var _9H=E(_9G),_9I=_9H[1],_9J=_9H[4];return [0,_9I,_9H[2],new T(function(){var _9K=function(_9L){var _9M=E(_9L);if(!_9M[0]){return E(new T(function(){var _9N=function(_9O){var _9P=E(_9O);if(!_9P[0]){return E(_9H[3]);}else{var _9Q=E(_9P[1]);return A(_94,[_9Q[1],_9Q[2],new T(function(){return _9N(_9P[2]);})]);}};return _9N(_9J);}));}else{var _9R=E(_9M[1]);return A(_94,[_9R[1],_9R[2],new T(function(){return _9K(_9M[2]);})]);}};return _9K(_9I);}),_9J];},_8B));}),_8A]];},_9S=function(_9T){return E(_2n);},_9U=[0,1],_9V=function(_9W,_9X){var _9Y=E(_9W);return [0,_9Y,new T(function(){var _9Z=_9V(_67(_9Y,_9X),_9X);return [1,_9Z[1],_9Z[2]];})];},_a0=function(_a1,_a2){while(1){var _a3=E(_a1);if(!_a3[0]){var _a4=_a3[1],_a5=E(_a2);if(!_a5[0]){var _a6=_a5[1],_a7=subC(_a4,_a6);if(!E(_a7[2])){return [0,_a7[1]];}else{_a1=[1,I_fromInt(_a4)];_a2=[1,I_fromInt(_a6)];continue;}}else{_a1=[1,I_fromInt(_a4)];_a2=_a5;continue;}}else{var _a8=E(_a2);if(!_a8[0]){_a1=_a3;_a2=[1,I_fromInt(_a8[1])];continue;}else{return [1,I_sub(_a3[1],_a8[1])];}}}},_a9=function(_aa,_ab){var _ac=E(_aa);if(!_ac[0]){var _ad=_ac[1],_ae=E(_ab);return _ae[0]==0?_ad>=_ae[1]:I_compareInt(_ae[1],_ad)<=0;}else{var _af=_ac[1],_ag=E(_ab);return _ag[0]==0?I_compareInt(_af,_ag[1])>=0:I_compare(_af,_ag[1])>=0;}},_ah=function(_ai,_aj){var _ak=E(_ai);if(!_ak[0]){var _al=_ak[1],_am=E(_aj);return _am[0]==0?_al>_am[1]:I_compareInt(_am[1],_al)<0;}else{var _an=_ak[1],_ao=E(_aj);return _ao[0]==0?I_compareInt(_an,_ao[1])>0:I_compare(_an,_ao[1])>0;}},_ap=function(_aq,_ar,_as){if(!_a9(_ar,[0,0])){var _at=function(_au){return !_2E(_au,_as)?[1,_au,new T(function(){return _at(_67(_au,_ar));})]:[0];};return _at(_aq);}else{var _av=function(_aw){return !_ah(_aw,_as)?[1,_aw,new T(function(){return _av(_67(_aw,_ar));})]:[0];};return _av(_aq);}},_ax=function(_ay){return [0,_ay];},_az=function(_aA,_aB){if(_aA<=0){if(_aA>=0){return quot(_aA,_aB);}else{if(_aB<=0){return quot(_aA,_aB);}else{return quot(_aA+1|0,_aB)-1|0;}}}else{if(_aB>=0){if(_aA>=0){return quot(_aA,_aB);}else{if(_aB<=0){return quot(_aA,_aB);}else{return quot(_aA+1|0,_aB)-1|0;}}}else{return quot(_aA-1|0,_aB)-1|0;}}},_aC=function(_aD,_aE){while(1){var _aF=E(_aD);if(!_aF[0]){var _aG=E(_aF[1]);if(_aG==(-2147483648)){_aD=[1,I_fromInt(-2147483648)];continue;}else{var _aH=E(_aE);if(!_aH[0]){return [0,_az(_aG,_aH[1])];}else{_aD=[1,I_fromInt(_aG)];_aE=_aH;continue;}}}else{var _aI=_aF[1],_aJ=E(_aE);return _aJ[0]==0?[0,I_toInt(I_div(_aI,I_fromInt(_aJ[1])))]:[1,I_div(_aI,_aJ[1])];}}},_aK=function(_aL,_aM){var _aN=_aL%_aM;if(_aL<=0){if(_aL>=0){return E(_aN);}else{if(_aM<=0){return E(_aN);}else{var _aO=E(_aN);return _aO==0?0:_aO+_aM|0;}}}else{if(_aM>=0){if(_aL>=0){return E(_aN);}else{if(_aM<=0){return E(_aN);}else{var _aP=E(_aN);return _aP==0?0:_aP+_aM|0;}}}else{var _aQ=E(_aN);return _aQ==0?0:_aQ+_aM|0;}}},_aR=function(_aS,_aT){while(1){var _aU=E(_aS);if(!_aU[0]){var _aV=E(_aU[1]);if(_aV==(-2147483648)){_aS=[1,I_fromInt(-2147483648)];continue;}else{var _aW=E(_aT);if(!_aW[0]){var _aX=_aW[1];return [0,[0,quot(_aV,_aX)],[0,_aV%_aX]];}else{_aS=[1,I_fromInt(_aV)];_aT=_aW;continue;}}}else{var _aY=E(_aT);if(!_aY[0]){_aS=_aU;_aT=[1,I_fromInt(_aY[1])];continue;}else{var _aZ=I_quotRem(_aU[1],_aY[1]);return [0,[1,_aZ[1]],[1,_aZ[2]]];}}}},_b0=function(_b1,_b2){var _b3=E(_b1);if(!_b3[0]){var _b4=_b3[1],_b5=E(_b2);return _b5[0]==0?_b4<=_b5[1]:I_compareInt(_b5[1],_b4)>=0;}else{var _b6=_b3[1],_b7=E(_b2);return _b7[0]==0?I_compareInt(_b6,_b7[1])<=0:I_compare(_b6,_b7[1])<=0;}},_b8=[0,0],_b9=function(_ba,_bb,_bc){var _bd=B(A(_ba,[_bb]));if(!_2w(_bd,_b8)){return _aC(_6B(_bb,_bc),_bd);}else{return E(_5U);}},_be=function(_bf){return E(E(_bf)[1]);},_bg=function(_bh){return E(E(_bh)[7]);},_bi=[1,I_fromBits([2627207168,20116567])],_bj=[0,40587],_bk=function(_bl){var _bm=new T(function(){var _bn=_6J(E(_bl),_1n,_2n,_1n),_bo=_6J(_bi,_1n,_2n,_1n),_bp=_6J(_bn[1],_bn[2],_bo[1],_bo[2]);return B((function(_bq,_br,_bs){var _bt=(function(_bu,_bv,_bw){var _bx=new T(function(){if(!_2w(_bw,_5V)){var _by=_aR(_bv,_bw),_bz=[0,_by[1],_by[2]];}else{var _bz=E(_5U);}return _bz;});return [0,new T(function(){return A(_bg,[(function(_bA){return E(E(_bA)[1]);})(_be(_bu)),new T(function(){return E(E(_bx)[1]);})]);}),new T(function(){return [0,E(E(E(_bx)[2])),E(_bw)];})];})(_bq,_br,_bs),_bB=_bt[1],_bC=E(_bt[2]);if(!_2E(_6B(_bC[1],_1n),_6B(_5V,_bC[2]))){return E(_bB);}else{var _bD=E(_be(_bq)[1]);return new F(function(){return A(_bD[3],[_bB,new T(function(){return B(A(_bD[7],[_1n]));})]);});}})([0,[0,[0,_67,_6B,_a0,function(_bE){var _bF=E(_bE);if(!_bF[0]){var _bG=E(_bF[1]);return _bG==(-2147483648)?E(_6g):[0, -_bG];}else{return [1,I_negate(_bF[1])];}},_6h,_6w,function(_bH){return E(_bH);}],[0,[0,_2w,function(_bI,_bJ){var _bK=E(_bI);if(!_bK[0]){var _bL=_bK[1],_bM=E(_bJ);return _bM[0]==0?_bL!=_bM[1]:I_compareInt(_bM[1],_bL)==0?false:true;}else{var _bN=_bK[1],_bO=E(_bJ);return _bO[0]==0?I_compareInt(_bN,_bO[1])==0?false:true:I_compare(_bN,_bO[1])==0?false:true;}}],function(_bP,_bQ){var _bR=E(_bP);if(!_bR[0]){var _bS=_bR[1],_bT=E(_bQ);if(!_bT[0]){var _bU=_bT[1];return _bS!=_bU?_bS>_bU?2:0:1;}else{var _bV=I_compareInt(_bT[1],_bS);return _bV<=0?_bV>=0?1:2:0;}}else{var _bW=_bR[1],_bX=E(_bQ);if(!_bX[0]){var _bY=I_compareInt(_bW,_bX[1]);return _bY>=0?_bY<=0?1:2:0;}else{var _bZ=I_compare(_bW,_bX[1]);return _bZ>=0?_bZ<=0?1:2:0;}}},_2E,_a9,_ah,_b0,function(_c0,_c1){return !_b0(_c0,_c1)?E(_c0):E(_c1);},function(_c2,_c3){return !_b0(_c2,_c3)?E(_c3):E(_c2);}],function(_c4){return [0,E(E(_c4)),E(_1n)];}],[0,function(_c5){return _67(_c5,_9U);},function(_c6){return _a0(_c6,_9U);},function(_c7){return _ax(E(_c7)[1]);},function(_c8){return [0,(function(_c9){var _ca=E(_c9);return _ca[0]==0?E(_ca[1]):I_toInt(_ca[1]);})(_c8)];},function(_cb){var _cc=_9V(_cb,_9U);return [1,_cc[1],_cc[2]];},function(_cd,_ce){var _cf=_9V(_cd,new T(function(){return _a0(_ce,_cd);}));return [1,_cf[1],_cf[2]];},function(_cg,_ch){return _ap(_cg,_9U,_ch);},function(_ci,_cj,_ck){return _ap(_ci,_a0(_cj,_ci),_ck);}],function(_cl,_cm){return !_2w(_cm,_5V)?_6m(_cl,_cm):E(_5U);},_5W,function(_cn,_co){return !_2w(_co,_5V)?_aC(_cn,_co):E(_5U);},function(_cp,_cq){return !_2w(_cq,_5V)?(function(_cr,_cs){while(1){var _ct=E(_cr);if(!_ct[0]){var _cu=E(_ct[1]);if(_cu==(-2147483648)){_cr=[1,I_fromInt(-2147483648)];continue;}else{var _cv=E(_cs);if(!_cv[0]){return [0,_aK(_cu,_cv[1])];}else{_cr=[1,I_fromInt(_cu)];_cs=_cv;continue;}}}else{var _cw=_ct[1],_cx=E(_cs);return _cx[0]==0?[0,I_toInt(I_mod(_cw,I_fromInt(_cx[1])))]:[1,I_mod(_cw,_cx[1])];}}})(_cp,_cq):E(_5U);},function(_cy,_cz){if(!_2w(_cz,_5V)){var _cA=_aR(_cy,_cz);return [0,_cA[1],_cA[2]];}else{return E(_5U);}},function(_cB,_cC){if(!_2w(_cC,_5V)){var _cD=(function(_cE,_cF){while(1){var _cG=E(_cE);if(!_cG[0]){var _cH=E(_cG[1]);if(_cH==(-2147483648)){_cE=[1,I_fromInt(-2147483648)];continue;}else{var _cI=E(_cF);if(!_cI[0]){var _cJ=_cI[1];return [0,[0,_az(_cH,_cJ)],[0,_aK(_cH,_cJ)]];}else{_cE=[1,I_fromInt(_cH)];_cF=_cI;continue;}}}else{var _cK=E(_cF);if(!_cK[0]){_cE=_cG;_cF=[1,I_fromInt(_cK[1])];continue;}else{var _cL=I_divMod(_cG[1],_cK[1]);return [0,[1,_cL[1]],[1,_cL[2]]];}}}})(_cB,_cC);return [0,_cD[1],_cD[2]];}else{return E(_5U);}},function(_cM){return E(_cM);}],_bp[1],_bp[2]));});return [0,new T(function(){return _67(_bj,_bm);}),new T(function(){return _a0(_bl,_b9(_9S,_6B(_bm,_2n),_bi));})];},_cN=function(_cO,_cP){var _cQ=jsShowI(_cO),_cR=_cQ;return _1v(fromJSStr(_cR),_cP);},_cS=function(_cT,_cU,_cV){if(_cU>=0){return _cN(_cU,_cV);}else{return _cT<=6?_cN(_cU,_cV):[1,_6Y,new T(function(){var _cW=jsShowI(_cU),_cX=_cW;return _1v(fromJSStr(_cX),[1,_6X,_cV]);})];}},_cY=function(_cZ,_d0){var _d1=_b9(_9S,_6B(_a0(_cZ,_bj),_2n),_bi);return !_b0(_bi,_d0)?_67(_d1,_d0):_67(_d1,_bi);},_d2=new T(function(){return [0,toJSStr(_1)];}),_d3=new T(function(){return [0,toJSStr([1,[0,44],_1])];}),_d4=[1,new T(function(){return [0,toJSStr([1,[0,41],_1])];}),_1],_d5=function(_d6,_d7){while(1){var _d8=E(_d6);if(!_d8[0]){return E(_d7)[0]==0?true:false;}else{var _d9=E(_d7);if(!_d9[0]){return false;}else{if(E(_d8[1])[1]!=E(_d9[1])[1]){return false;}else{_d6=_d8[2];_d7=_d9[2];continue;}}}}},_da=function(_db){return E(new T(function(){var _dc=hs_wordToWord64(4053623282),_dd=_dc,_de=hs_wordToWord64(3693590983),_df=_de;return [0,_dd,_df,[0,_dd,_df,new T(function(){return unCStr("base");}),new T(function(){return unCStr("GHC.IO.Exception");}),new T(function(){return unCStr("IOException");})],_1];}));},_dg=new T(function(){return unCStr(": ");}),_dh=[0,125],_di=new T(function(){return unCStr("{handle: ");}),_dj=function(_dk,_dl,_dm,_dn,_do,_dp){var _dq=new T(function(){var _dr=new T(function(){return (function(_ds,_dt){switch(E(_ds)){case 0:return _1v(new T(function(){return unCStr("already exists");}),_dt);case 1:return _1v(new T(function(){return unCStr("does not exist");}),_dt);case 2:return _1v(new T(function(){return unCStr("resource busy");}),_dt);case 3:return _1v(new T(function(){return unCStr("resource exhausted");}),_dt);case 4:return _1v(new T(function(){return unCStr("end of file");}),_dt);case 5:return _1v(new T(function(){return unCStr("illegal operation");}),_dt);case 6:return _1v(new T(function(){return unCStr("permission denied");}),_dt);case 7:return _1v(new T(function(){return unCStr("user error");}),_dt);case 8:return _1v(new T(function(){return unCStr("unsatisified constraints");}),_dt);case 9:return _1v(new T(function(){return unCStr("system error");}),_dt);case 10:return _1v(new T(function(){return unCStr("protocol error");}),_dt);case 11:return _1v(new T(function(){return unCStr("failed");}),_dt);case 12:return _1v(new T(function(){return unCStr("invalid argument");}),_dt);case 13:return _1v(new T(function(){return unCStr("inappropriate type");}),_dt);case 14:return _1v(new T(function(){return unCStr("hardware fault");}),_dt);case 15:return _1v(new T(function(){return unCStr("unsupported operation");}),_dt);case 16:return _1v(new T(function(){return unCStr("timeout");}),_dt);case 17:return _1v(new T(function(){return unCStr("resource vanished");}),_dt);default:return _1v(new T(function(){return unCStr("interrupted");}),_dt);}})(_dl,new T(function(){var _du=E(_dn);return _du[0]==0?E(_dp):_1v(new T(function(){return unCStr(" (");}),new T(function(){return _1v(_du,[1,[0,41],_dp]);},1));},1));},1),_dv=E(_dm);return _dv[0]==0?E(_dr):_1v(_dv,new T(function(){return _1v(_dg,_dr);},1));},1),_dw=E(_do);if(!_dw[0]){var _dx=E(_dk);if(!_dx[0]){return E(_dq);}else{var _dy=E(_dx[1]);return _dy[0]==0?_1v(_di,new T(function(){return _1v(_dy[1],[1,_dh,new T(function(){return _1v(_dg,_dq);})]);},1)):_1v(_di,new T(function(){return _1v(_dy[1],[1,_dh,new T(function(){return _1v(_dg,_dq);})]);},1));}}else{return _1v(_dw[1],new T(function(){return _1v(_dg,_dq);},1));}},_dz=function(_dA){return [0,new T(function(){return [0,_da,[0,function(_dB,_dC,_dD){var _dE=E(_dC);return _dj(_dE[1],_dE[2],_dE[3],_dE[4],_dE[6],_dD);},function(_dF){var _dG=E(_dF);return _dj(_dG[1],_dG[2],_dG[3],_dG[4],_dG[6],_1);},function(_dH,_dI){return _5D(function(_dJ,_dK){var _dL=E(_dJ);return _dj(_dL[1],_dL[2],_dL[3],_dL[4],_dL[6],_dK);},_dH,_dI);}],_dz,function(_dM){var _dN=E(_dM);return _5g(_5e(_dN[1]),_da,_dN[2]);}];}),_dA];},_dO=function(_dP,_){return (function(_dQ,_){return die(new T(function(){return _dz(new T(function(){return (function(_dR){return [0,_36,7,_1,_dR,_36,_36];})(_dQ);}));}));})(_dP,_);},_dS=function(_dT){var _dU=B(A(_dT,[_])),_dV=_dU;return E(_dV);},_dW=function(_dX){return _dS(function(_){var _=0;return eval(_dX);});},_dY=function(_dZ,_e0,_){var _e1=A(_dW,[E(new T(function(){return [0,"(function(k) {return localStorage.getItem(k);})"];}))[1],E(toJSStr(E(_e0))),_]),_e2=_e1;return new T(function(){if(!_dS(function(_){var _=0,_e3=B(A(new T(function(){return _dW("(function(x) {return x === null;})");}),[E(_e2),_])),_e4=_e3;return new T(function(){return (function(_e5){return _e5>0;})(_e4);});})){var _e6=String(_e2),_e7=_e6,_e8=jsParseJSON(_e7),_e9=_e8,_ea=E(_e9),_eb=_ea[0]==0?E([0,new T(function(){return unCStr("Invalid JSON!");})]):A(_I,[_dZ,_ea[1]]);}else{var _eb=E([0,new T(function(){return unCStr("No such value");})]);}return _eb;});},_ec=function(_ed){return E([0,4]);},_ee=[0,_ec,_ec,function(_ef,_eg,_){var _eh=readOffAddr("w32",4,E(_ef)[1],E(_eg)[1]),_ei=_eh;return [0,_ei];},function(_ej,_ek,_el,_){var _=writeOffAddr("w32",4,E(_ej)[1],E(_ek)[1],E(_el)[1]);return _2u;},function(_em,_en,_){var _eo=readOffAddr("w32",4,plusAddr(E(_em)[1],E(_en)[1]),0),_ep=_eo;return [0,_ep];},function(_eq,_er,_es,_){var _=writeOffAddr("w32",4,plusAddr(E(_eq)[1],E(_er)[1]),0,E(_es)[1]);return _2u;},function(_et,_){var _eu=readOffAddr("w32",4,E(_et)[1],0),_ev=_eu;return [0,_ev];},function(_ew,_ex,_){var _=writeOffAddr("w32",4,E(_ew)[1],0,E(_ex)[1]);return _2u;}],_ey=function(_ez){return E(E(_ez)[3]);},_eA=function(_eB,_eC,_eD,_){if(_eC>0){return (function(_eE,_eF,_){while(1){var _eG=E(_eE);if(!_eG){var _eH=B(A(new T(function(){return A(_ey,[_eB,_eD,[0,0]]);}),[_])),_eI=_eH;return [1,_eI,_eF];}else{var _eJ=B(A(new T(function(){return _ey(_eB);}),[_eD,[0,_eG],_])),_eK=_eJ;_eE=_eG-1|0;var _eL=[1,_eK,_eF];_eF=_eL;continue;}}})(_eC-1|0,_1,_);}else{return _1;}},_eM=[0,0,_1],_eN=function(_eO){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _cS(9,_eO,_1);})));},_eP=function(_eQ){while(1){var _eR=(function(_eS){var _eT=E(_eS);if(!_eT[0]){return [0];}else{var _eU=_eT[2],_eV=E(E(_eT[1])[1]);if(_eV==45){_eQ=_eU;return null;}else{return [1,new T(function(){return [0,(function(_eW){var _eX=u_towupper(_eW),_eY=_eX;return _eY>>>0>1114111?_eN(_eY):_eY;})(_eV)];}),new T(function(){return _eP(_eU);})];}}})(_eQ);if(_eR!=null){return _eR;}}},_eZ=function(_f0,_f1){var _f2=E(_f1);if(!_f2[0]){return [0,_1,_1];}else{var _f3=_f2[1];if(!B(A(_f0,[_f3]))){return [0,_1,_f2];}else{var _f4=new T(function(){var _f5=_eZ(_f0,_f2[2]);return [0,_f5[1],_f5[2]];});return [0,[1,_f3,new T(function(){return E(E(_f4)[1]);})],new T(function(){return E(E(_f4)[2]);})];}}},_f6=new T(function(){return unCStr("UTF-32LE");}),_f7=0,_f8=1,_f9=[0,0],_fa=function(_fb,_){return die(new T(function(){return _dz(_fb);}));},_fc=function(_fd,_){return _fa(_fd,_);},_fe=function(_ff,_fg,_fh,_fi,_fj,_fk,_fl,_fm,_fn,_fo,_fp,_fq,_fr,_fs,_ft,_){var _fu=newByteArr(4),_fv=_fu,_fw=_fv,_fx=_fw,_fy=E(_fm)[1],_fz=function(_fA){var _fB=plusAddr(_fg,_fA),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_fC=newByteArr(4),_fD=_fC,_fE=_fD,_fF=_fE,_fG=E(_ft)[1],_fH=function(_fI){var _fJ=plusAddr(_fn,_fI),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_fK=newByteArr(4),_fL=_fK,_fM=_fL,_fN=_fM,_fO=function(_fP){var _fQ=_fN,_=writeOffAddr("w32",4,_fQ,0,_fP),_fR=newByteArr(4),_fS=_fR,_fT=_fS,_fU=_fT,_fV=function(_fW){var _fX=_fU,_=writeOffAddr("w32",4,_fX,0,_fW),_fY=hs_iconv(E(_ff)[1],_fx,_fQ,_fF,_fX),_fZ=_fY,_g0=readOffAddr("w32",4,_fQ,0),_g1=_g0,_g2=readOffAddr("w32",4,_fX,0),_g3=_g2,_g4=new T(function(){if(_fG<32){var _g5=[0,(_g3&4294967295)>>_fG];}else{var _g5=(_g3&4294967295)>=0?E(_f9):E([0,-1]);}var _g6=_g5;return _g6;}),_g7=new T(function(){var _g8=E(_g1);if(!_g8){var _g9=[0,_fg,_fh,_fi,_fj,0,0];}else{if(_fy<32){var _ga=[0,_fg,_fh,_fi,_fj,_fl-((_g8&4294967295)>>_fy)|0,_fl];}else{if((_g8&4294967295)>=0){var _gb=[0,_fg,_fh,_fi,_fj,_fl,_fl];}else{var _gb=[0,_fg,_fh,_fi,_fj,_fl+1|0,_fl];}var _gc=_gb,_gd=_gc,_ga=_gd;}var _ge=_ga,_g9=_ge;}return _g9;});if(_fZ!=E(new T(function(){return [0, -(1&4294967295)>>>0];}))[1]){var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_f7,_g7,new T(function(){return [0,_fn,_fo,_fp,_fq,_fr,_fq-E(_g4)[1]|0];})];}else{var _gf=__hscore_get_errno(),_gg=_gf;switch(E(_gg)){case 7:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_f8,_g7,new T(function(){return [0,_fn,_fo,_fp,_fq,_fr,_fq-E(_g4)[1]|0];})];case 22:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_f7,_g7,new T(function(){return [0,_fn,_fo,_fp,_fq,_fr,_fq-E(_g4)[1]|0];})];case 84:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,new T(function(){return E(E(_g4)[1])==0?1:2;}),_g7,new T(function(){return [0,_fn,_fo,_fp,_fq,_fr,_fq-E(_g4)[1]|0];})];default:var _gh=__hscore_get_errno(),_gi=_gh;return _fc(_gj(new T(function(){return unCStr("iconvRecoder");}),_gi,_36,_36),_);}}};if(_fG<32){return new F(function(){return _fV((_fq-_fs|0)<<_fG>>>0);});}else{return new F(function(){return _fV(0);});}};if(_fy<32){return new F(function(){return _fO((_fl-_fk|0)<<_fy>>>0);});}else{return new F(function(){return _fO(0);});}};if(_fG<32){return new F(function(){return _fH(_fs<<_fG);});}else{return new F(function(){return _fH(0);});}};if(_fy<32){return new F(function(){return _fz(_fk<<_fy);});}else{return new F(function(){return _fz(0);});}},_gk=[0,2],_gl=function(_gm,_gn){while(1){var _go=E(_gm);if(!_go[0]){return E(_gn);}else{_gm=_go[2];var _gp=_gn+1|0;_gn=_gp;continue;}}},_gq=function(_gr,_gs,_){return (function(_gt,_gu,_){var _gv=newByteArr(_gl(_gt,0)+1|0),_gw=_gv,_gx=_gw,_gy=_gx,_gz=_gy,_gA=(function(_gB,_gC,_){while(1){var _gD=E(_gB);if(!_gD[0]){var _=writeOffAddr("i8",1,_gz,_gC,0);return _2u;}else{var _=writeOffAddr("i8",1,_gz,_gC,E(_gD[1])[1]&255);_gB=_gD[2];var _gE=_gC+1|0;_gC=_gE;continue;}}})(_gt,0,_),_gF=_gA,_gG=B(A(_gu,[[0,_gz],_])),_gH=_gG,_=0;return _gH;})(_gr,_gs,_);},_gI=function(_gJ,_gK,_gL,_gM){return _gq(_gJ,function(_gN){return _gq(_gK,function(_gO,_){var _gP=hs_iconv_open(E(_gO)[1],E(_gN)[1]),_gQ=_gP,_gR=E(_gQ);if(_gR==(-1)){var _gS=__hscore_get_errno(),_gT=_gS;return _fc(_gj(new T(function(){return unCStr("mkTextEncoding");}),_gT,_36,_36),_);}else{return [0,new T(function(){return B(A(_gM,[[0,_gR]]));}),_gL,function(_){var _gU=hs_iconv_close(_gR),_gV=_gU;if(E(_gV)==(-1)){var _gW=__hscore_get_errno(),_gX=_gW;return _fc(_gj(new T(function(){return unCStr("Iconv.close");}),_gX,_36,_36),_);}else{return _2u;}},function(_){return _2u;},function(_gY,_){return _2u;}];}});});},_gZ=function(_fd,_){return _fa(_fd,_);},_h0=12,_h1=function(_h2,_h3,_h4,_){var _h5=E(_h3),_h6=E(_h4);return new F(function(){return (function(_h7,_h8,_h9,_ha,_hb,_hc,_hd,_he,_hf,_hg,_hh,_hi,_hj,_){switch(E(_h7)){case 0:return _gZ([0,_36,_h0,new T(function(){return unCStr("recoverDecode");}),new T(function(){return unCStr("invalid byte sequence");}),_36,_36],_);case 1:return [0,[0,_h8,_h9,_ha,_hb,_hc+1|0,_hd],[0,_he,_hf,_hg,_hh,_hi,_hj]];case 2:var _=writeOffAddr("w32",4,_he,_hj,65533),_=0;return [0,[0,_h8,_h9,_ha,_hb,_hc+1|0,_hd],[0,_he,_hf,_hg,_hh,_hi,_hj+1|0]];default:var _hk=readOffAddr("w8",1,plusAddr(_h8,_hc),0),_hl=_hk,_=0;if(_hl>=128){var _hm=56320+(_hl&4294967295)|0;if(_hm>>>0>1114111){return _eN(_hm);}else{var _=writeOffAddr("w32",4,_he,_hj,_hm),_=0;return [0,[0,_h8,_h9,_ha,_hb,_hc+1|0,_hd],[0,_he,_hf,_hg,_hh,_hi,_hj+1|0]];}}else{var _hn=_hl&4294967295;if(_hn>>>0>1114111){return _eN(_hn);}else{var _=writeOffAddr("w32",4,_he,_hj,_hn),_=0;return [0,[0,_h8,_h9,_ha,_hb,_hc+1|0,_hd],[0,_he,_hf,_hg,_hh,_hi,_hj+1|0]];}}}})(_h2,_h5[1],_h5[2],_h5[3],_h5[4],_h5[5],_h5[6],_h6[1],_h6[2],_h6[3],_h6[4],_h6[5],_h6[6],_);});},_ho=function(_){return _gZ([0,_36,_h0,new T(function(){return unCStr("recoverEncode");}),new T(function(){return unCStr("invalid character");}),_36,_36],_);},_hp=function(_hq,_hr,_hs,_){var _ht=E(_hr),_hu=E(_hs);return new F(function(){return (function(_hv,_hw,_hx,_hy,_hz,_hA,_hB,_hC,_hD,_hE,_hF,_hG,_hH,_){var _hI=readOffAddr("w32",4,_hw,_hA),_hJ=_hI,_=0;switch(E(_hv)){case 0:return new F(function(){return _ho(_);});break;case 1:return [0,[0,_hw,_hx,_hy,_hz,_hA+1|0,_hB],[0,_hC,_hD,_hE,_hF,_hG,_hH]];case 2:if(E(_hJ)==63){return [0,[0,_hw,_hx,_hy,_hz,_hA+1|0,_hB],[0,_hC,_hD,_hE,_hF,_hG,_hH]];}else{var _=writeOffAddr("w32",4,_hw,_hA,63),_=0;return [0,[0,_hw,_hx,_hy,_hz,_hA,_hB],[0,_hC,_hD,_hE,_hF,_hG,_hH]];}break;default:var _hK=_hJ;if(56448>_hK){return new F(function(){return _ho(_);});}else{if(_hK>=56576){return new F(function(){return _ho(_);});}else{var _=writeOffAddr("w8",1,plusAddr(_hC,_hH),0,_hK>>>0&255),_=0;return [0,[0,_hw,_hx,_hy,_hz,_hA+1|0,_hB],[0,_hC,_hD,_hE,_hF,_hG,_hH+1|0]];}}}})(_hq,_ht[1],_ht[2],_ht[3],_ht[4],_ht[5],_ht[6],_hu[1],_hu[2],_hu[3],_hu[4],_hu[5],_hu[6],_);});},_hL=2,_hM=function(_hN,_hO,_hP,_hQ,_hR,_hS,_hT,_hU,_hV,_hW,_hX,_hY,_){var _hZ=[0,_hN,_hO,_hP,_hQ,0,0],_i0=function(_i1,_i2,_){while(1){var _i3=(function(_i4,_i5,_){if(_i4<_hS){if((_hW-_i5|0)>=2){var _i6=readOffAddr("w32",4,_hN,_i4),_i7=_i6,_=0,_i8=_i7;if(_i8>=65536){if((_hW-_i5|0)>=4){var _i9=_i8-65536|0,_=writeOffAddr("w8",1,plusAddr(_hT,_i5),0,((_i9>>18)+216|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_hT,_i5+1|0),0,_i9>>10>>>0&255),_=0,_ia=(_i9>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_hT,_i5+2|0),0,((_ia>>8)+220|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_hT,_i5+3|0),0,_ia>>>0&255),_=0,_ib=_i4+1|0,_ic=_i5+4|0;_i1=_ib;_i2=_ic;return null;}else{return [0,_f8,new T(function(){return _i4!=_hS?[0,_hN,_hO,_hP,_hQ,_i4,_hS]:E(_hZ);}),[0,_hT,_hU,_hV,_hW,_hX,_i5]];}}else{var _id=function(_ie){if(56320>_i8){var _=writeOffAddr("w8",1,plusAddr(_hT,_i5),0,_i8>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_hT,_i5+1|0),0,_i8>>>0&255),_=0;return new F(function(){return _i0(_i4+1|0,_i5+2|0,_);});}else{if(_i8>57343){var _=writeOffAddr("w8",1,plusAddr(_hT,_i5),0,_i8>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_hT,_i5+1|0),0,_i8>>>0&255),_=0;return new F(function(){return _i0(_i4+1|0,_i5+2|0,_);});}else{return [0,_hL,new T(function(){return _i4!=_hS?[0,_hN,_hO,_hP,_hQ,_i4,_hS]:E(_hZ);}),[0,_hT,_hU,_hV,_hW,_hX,_i5]];}}};if(55296>_i8){return new F(function(){return _id(_);});}else{return _i8>56319?B(_id(_)):[0,_hL,new T(function(){return _i4!=_hS?[0,_hN,_hO,_hP,_hQ,_i4,_hS]:E(_hZ);}),[0,_hT,_hU,_hV,_hW,_hX,_i5]];}}}else{return [0,_f8,new T(function(){return _i4!=_hS?[0,_hN,_hO,_hP,_hQ,_i4,_hS]:E(_hZ);}),[0,_hT,_hU,_hV,_hW,_hX,_i5]];}}else{return [0,_f7,new T(function(){return _i4!=_hS?[0,_hN,_hO,_hP,_hQ,_i4,_hS]:E(_hZ);}),[0,_hT,_hU,_hV,_hW,_hX,_i5]];}})(_i1,_i2,_);if(_i3!=null){return _i3;}}};return new F(function(){return _i0(_hR,_hY,_);});},_if=true,_ig=function(_ih,_ii,_ij,_ik,_il,_im,_in,_io,_ip,_iq,_ir,_is,_){var _it=[0,_ih,_ii,_ij,_ik,0,0];return (function(_iu,_iv,_){while(1){var _iw=(function(_ix,_iy,_){if(_iy<_iq){if(_ix<_im){if((_ix+1|0)!=_im){var _iz=readOffAddr("w8",1,plusAddr(_ih,_ix),0),_iA=_iz,_=0,_iB=readOffAddr("w8",1,plusAddr(_ih,_ix+1|0),0),_iC=_iB,_=0,_iD=(_iA<<8>>>0&65535)+_iC>>>0&65535;if(_iD>=55296){if(_iD<=57343){if((_im-_ix|0)>=4){var _iE=readOffAddr("w8",1,plusAddr(_ih,_ix+2|0),0),_iF=_iE,_=0,_iG=readOffAddr("w8",1,plusAddr(_ih,_ix+3|0),0),_iH=_iG,_=0;if(_iD<55296){return [0,_hL,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}else{if(_iD>56319){return [0,_hL,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}else{var _iI=(_iF<<8>>>0&65535)+_iH>>>0&65535;if(_iI<56320){return [0,_hL,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}else{if(_iI>57343){return [0,_hL,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}else{var _=writeOffAddr("w32",4,_in,_iy,((((_iD&4294967295)-55296|0)<<10)+((_iI&4294967295)-56320|0)|0)+65536|0),_=0,_iJ=_ix+4|0,_iK=_iy+1|0;_iu=_iJ;_iv=_iK;return null;}}}}}else{return [0,_f7,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}}else{var _=writeOffAddr("w32",4,_in,_iy,_iD&4294967295),_=0,_iJ=_ix+2|0,_iK=_iy+1|0;_iu=_iJ;_iv=_iK;return null;}}else{var _=writeOffAddr("w32",4,_in,_iy,_iD&4294967295),_=0,_iJ=_ix+2|0,_iK=_iy+1|0;_iu=_iJ;_iv=_iK;return null;}}else{return [0,_f7,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}}else{return [0,_f7,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}}else{return [0,_f8,new T(function(){return _ix!=_im?[0,_ih,_ii,_ij,_ik,_ix,_im]:E(_it);}),[0,_in,_io,_ip,_iq,_ir,_iy]];}})(_iu,_iv,_);if(_iw!=null){return _iw;}}})(_il,_is,_);},_iL=function(_iM,_iN,_iO,_iP,_iQ,_iR,_iS,_iT,_iU,_iV,_iW,_iX,_){var _iY=[0,_iM,_iN,_iO,_iP,0,0];return (function(_iZ,_j0,_){while(1){var _j1=(function(_j2,_j3,_){if(_j3<_iV){if(_j2<_iR){if((_j2+1|0)!=_iR){var _j4=readOffAddr("w8",1,plusAddr(_iM,_j2),0),_j5=_j4,_=0,_j6=readOffAddr("w8",1,plusAddr(_iM,_j2+1|0),0),_j7=_j6,_=0,_j8=(_j7<<8>>>0&65535)+_j5>>>0&65535;if(_j8>=55296){if(_j8<=57343){if((_iR-_j2|0)>=4){var _j9=readOffAddr("w8",1,plusAddr(_iM,_j2+2|0),0),_ja=_j9,_=0,_jb=readOffAddr("w8",1,plusAddr(_iM,_j2+3|0),0),_jc=_jb,_=0;if(_j8<55296){return [0,_hL,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}else{if(_j8>56319){return [0,_hL,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}else{var _jd=(_jc<<8>>>0&65535)+_ja>>>0&65535;if(_jd<56320){return [0,_hL,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}else{if(_jd>57343){return [0,_hL,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}else{var _=writeOffAddr("w32",4,_iS,_j3,((((_j8&4294967295)-55296|0)<<10)+((_jd&4294967295)-56320|0)|0)+65536|0),_=0,_je=_j2+4|0,_jf=_j3+1|0;_iZ=_je;_j0=_jf;return null;}}}}}else{return [0,_f7,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}}else{var _=writeOffAddr("w32",4,_iS,_j3,_j8&4294967295),_=0,_je=_j2+2|0,_jf=_j3+1|0;_iZ=_je;_j0=_jf;return null;}}else{var _=writeOffAddr("w32",4,_iS,_j3,_j8&4294967295),_=0,_je=_j2+2|0,_jf=_j3+1|0;_iZ=_je;_j0=_jf;return null;}}else{return [0,_f7,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}}else{return [0,_f7,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}}else{return [0,_f8,new T(function(){return _j2!=_iR?[0,_iM,_iN,_iO,_iP,_j2,_iR]:E(_iY);}),[0,_iS,_iT,_iU,_iV,_iW,_j3]];}})(_iZ,_j0,_);if(_j1!=null){return _j1;}}})(_iQ,_iX,_);},_jg=function(_jh,_ji,_){var _jj=E(_jh),_jk=E(_ji);return _ig(_jj[1],_jj[2],_jj[3],_jj[4],_jj[5],_jj[6],_jk[1],_jk[2],_jk[3],_jk[4],_jk[5],_jk[6],_);},_jl=[1,_jg],_jm=function(_jn,_jo,_){var _jp=E(_jn),_jq=E(_jo);return _iL(_jp[1],_jp[2],_jp[3],_jp[4],_jp[5],_jp[6],_jq[1],_jq[2],_jq[3],_jq[4],_jq[5],_jq[6],_);},_jr=false,_js=function(_){return _2u;},_jt=function(_ju,_){return _2u;},_jv=function(_jw,_jx,_jy,_jz,_jA,_jB,_jC,_jD,_jE,_jF,_jG,_jH,_){var _jI=[0,_jw,_jx,_jy,_jz,0,0],_jJ=function(_jK,_jL,_){if(_jK<_jB){if((_jF-_jL|0)>=4){var _jM=readOffAddr("w32",4,_jw,_jK),_jN=_jM,_=0,_jO=_jN,_jP=function(_jQ){if(56320>_jO){var _=writeOffAddr("w8",1,plusAddr(_jC,_jL),0,_jO>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_jC,_jL+1|0),0,_jO>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_jC,_jL+2|0),0,_jO>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_jC,_jL+3|0),0,_jO>>>0&255),_=0;return new F(function(){return _jJ(_jK+1|0,_jL+4|0,_);});}else{if(_jO>57343){var _=writeOffAddr("w8",1,plusAddr(_jC,_jL),0,_jO>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_jC,_jL+1|0),0,_jO>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_jC,_jL+2|0),0,_jO>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_jC,_jL+3|0),0,_jO>>>0&255),_=0;return new F(function(){return _jJ(_jK+1|0,_jL+4|0,_);});}else{return [0,_hL,new T(function(){return _jK!=_jB?[0,_jw,_jx,_jy,_jz,_jK,_jB]:E(_jI);}),[0,_jC,_jD,_jE,_jF,_jG,_jL]];}}};if(55296>_jO){return new F(function(){return _jP(_);});}else{return _jO>56319?B(_jP(_)):[0,_hL,new T(function(){return _jK!=_jB?[0,_jw,_jx,_jy,_jz,_jK,_jB]:E(_jI);}),[0,_jC,_jD,_jE,_jF,_jG,_jL]];}}else{return [0,_f8,new T(function(){return _jK!=_jB?[0,_jw,_jx,_jy,_jz,_jK,_jB]:E(_jI);}),[0,_jC,_jD,_jE,_jF,_jG,_jL]];}}else{return [0,_f7,new T(function(){return _jK!=_jB?[0,_jw,_jx,_jy,_jz,_jK,_jB]:E(_jI);}),[0,_jC,_jD,_jE,_jF,_jG,_jL]];}};return new F(function(){return _jJ(_jA,_jH,_);});},_jR=function(_jS,_jT,_jU,_jV,_jW,_jX,_jY,_jZ,_k0,_k1,_k2,_k3,_){var _k4=[0,_jS,_jT,_jU,_jV,0,0],_k5=function(_k6,_k7,_){while(1){var _k8=(function(_k9,_ka,_){if(_ka<_k1){if((_jX-_k9|0)>=4){var _kb=readOffAddr("w8",1,plusAddr(_jS,_k9),0),_kc=_kb,_=0,_kd=readOffAddr("w8",1,plusAddr(_jS,_k9+1|0),0),_ke=_kd,_=0,_kf=readOffAddr("w8",1,plusAddr(_jS,_k9+2|0),0),_kg=_kf,_=0,_kh=readOffAddr("w8",1,plusAddr(_jS,_k9+3|0),0),_ki=_kh,_=0,_kj=((((_kc&4294967295)<<24)+((_ke&4294967295)<<16)|0)+((_kg&4294967295)<<8)|0)+(_ki&4294967295)|0,_kk=_kj,_kl=function(_km){if(_kk<=57343){return [0,_hL,new T(function(){return _k9!=_jX?[0,_jS,_jT,_jU,_jV,_k9,_jX]:E(_k4);}),[0,_jY,_jZ,_k0,_k1,_k2,_ka]];}else{if(_kk>1114111){return [0,_hL,new T(function(){return _k9!=_jX?[0,_jS,_jT,_jU,_jV,_k9,_jX]:E(_k4);}),[0,_jY,_jZ,_k0,_k1,_k2,_ka]];}else{var _=writeOffAddr("w32",4,_jY,_ka,_kj),_=0;return new F(function(){return _k5(_k9+4|0,_ka+1|0,_);});}}};if(_kk<0){return new F(function(){return _kl(_);});}else{if(_kk>=55296){return new F(function(){return _kl(_);});}else{var _=writeOffAddr("w32",4,_jY,_ka,_kj),_=0,_kn=_k9+4|0,_ko=_ka+1|0;_k6=_kn;_k7=_ko;return null;}}}else{return [0,_f7,new T(function(){return _k9!=_jX?[0,_jS,_jT,_jU,_jV,_k9,_jX]:E(_k4);}),[0,_jY,_jZ,_k0,_k1,_k2,_ka]];}}else{return [0,_f8,new T(function(){return _k9!=_jX?[0,_jS,_jT,_jU,_jV,_k9,_jX]:E(_k4);}),[0,_jY,_jZ,_k0,_k1,_k2,_ka]];}})(_k6,_k7,_);if(_k8!=null){return _k8;}}};return new F(function(){return _k5(_jW,_k3,_);});},_kp=function(_kq,_kr,_ks,_kt,_ku,_kv,_kw,_kx,_ky,_kz,_kA,_kB,_){var _kC=[0,_kq,_kr,_ks,_kt,0,0],_kD=function(_kE,_kF,_){while(1){var _kG=(function(_kH,_kI,_){if(_kI<_kz){if((_kv-_kH|0)>=4){var _kJ=readOffAddr("w8",1,plusAddr(_kq,_kH),0),_kK=_kJ,_=0,_kL=readOffAddr("w8",1,plusAddr(_kq,_kH+1|0),0),_kM=_kL,_=0,_kN=readOffAddr("w8",1,plusAddr(_kq,_kH+2|0),0),_kO=_kN,_=0,_kP=readOffAddr("w8",1,plusAddr(_kq,_kH+3|0),0),_kQ=_kP,_=0,_kR=((((_kQ&4294967295)<<24)+((_kO&4294967295)<<16)|0)+((_kM&4294967295)<<8)|0)+(_kK&4294967295)|0,_kS=_kR,_kT=function(_kU){if(_kS<=57343){return [0,_hL,new T(function(){return _kH!=_kv?[0,_kq,_kr,_ks,_kt,_kH,_kv]:E(_kC);}),[0,_kw,_kx,_ky,_kz,_kA,_kI]];}else{if(_kS>1114111){return [0,_hL,new T(function(){return _kH!=_kv?[0,_kq,_kr,_ks,_kt,_kH,_kv]:E(_kC);}),[0,_kw,_kx,_ky,_kz,_kA,_kI]];}else{var _=writeOffAddr("w32",4,_kw,_kI,_kR),_=0;return new F(function(){return _kD(_kH+4|0,_kI+1|0,_);});}}};if(_kS<0){return new F(function(){return _kT(_);});}else{if(_kS>=55296){return new F(function(){return _kT(_);});}else{var _=writeOffAddr("w32",4,_kw,_kI,_kR),_=0,_kV=_kH+4|0,_kW=_kI+1|0;_kE=_kV;_kF=_kW;return null;}}}else{return [0,_f7,new T(function(){return _kH!=_kv?[0,_kq,_kr,_ks,_kt,_kH,_kv]:E(_kC);}),[0,_kw,_kx,_ky,_kz,_kA,_kI]];}}else{return [0,_f8,new T(function(){return _kH!=_kv?[0,_kq,_kr,_ks,_kt,_kH,_kv]:E(_kC);}),[0,_kw,_kx,_ky,_kz,_kA,_kI]];}})(_kE,_kF,_);if(_kG!=null){return _kG;}}};return new F(function(){return _kD(_ku,_kB,_);});},_kX=function(_kY,_kZ,_){var _l0=E(_kY),_l1=E(_kZ);return new F(function(){return _jR(_l0[1],_l0[2],_l0[3],_l0[4],_l0[5],_l0[6],_l1[1],_l1[2],_l1[3],_l1[4],_l1[5],_l1[6],_);});},_l2=[1,_kX],_l3=function(_l4,_l5,_){var _l6=E(_l4),_l7=E(_l5);return new F(function(){return _kp(_l6[1],_l6[2],_l6[3],_l6[4],_l6[5],_l6[6],_l7[1],_l7[2],_l7[3],_l7[4],_l7[5],_l7[6],_);});},_l8=function(_){return _2u;},_l9=function(_la,_){return _2u;},_lb=function(_lc,_){return _2u;},_ld=function(_){return _2u;},_gj=function(_le,_lf,_lg,_lh){return _dS(function(_){var _=0,_li=strerror(_lf),_lj=_li,_lk=B(A(E(new T(function(){return _dS(function(_){var _=0,_ll=nMV(new T(function(){return _dS(function(_){var _=0;return (function(_lm,_ln,_){var _lo=_eP(_ln);return !_d5(_lo,new T(function(){return unCStr("UTF16");}))?!_d5(_lo,new T(function(){return unCStr("UTF16BE");}))?!_d5(_lo,new T(function(){return unCStr("UTF16LE");}))?!_d5(_lo,new T(function(){return unCStr("UTF32");}))?!_d5(_lo,new T(function(){return unCStr("UTF32BE");}))?!_d5(_lo,new T(function(){return unCStr("UTF32LE");}))?!_d5(_lo,new T(function(){return unCStr("UTF8");}))?(function(_lp,_lq,_){return [0,_lq,new T(function(){var _lr=new T(function(){var _ls=_eZ(function(_lt){return E(E(_lt)[1])==47?false:true;},_lq);return [0,_ls[1],_ls[2]];});return B(_gI(new T(function(){return E(E(_lr)[1]);}),new T(function(){return _1v(_f6,new T(function(){return E(E(_lr)[2]);},1));}),function(_lu,_lv,_){return new F(function(){return _h1(_lp,_lu,_lv,_);});},function(_lw,_lx,_ly,_){var _lz=E(_lx),_lA=E(_ly);return new F(function(){return _fe(_lw,_lz[1],_lz[2],_lz[3],_lz[4],_lz[5],_lz[6],_f9,_lA[1],_lA[2],_lA[3],_lA[4],_lA[5],_lA[6],_gk,_);});}));}),new T(function(){return B(_gI(_f6,_lq,function(_lu,_lv,_){return new F(function(){return _hp(_lp,_lu,_lv,_);});},function(_lB,_lC,_lD,_){var _lE=E(_lC),_lF=E(_lD);return new F(function(){return _fe(_lB,_lE[1],_lE[2],_lE[3],_lE[4],_lE[5],_lE[6],_gk,_lF[1],_lF[2],_lF[3],_lF[4],_lF[5],_lF[6],_f9,_);});}));})];})(_lm,_ln,_):new T(function(){return B((function(_lG){return [0,new T(function(){return unCStr("UTF-8");}),function(_){return [0,function(_lH,_lI,_){var _lJ=E(_lH),_lK=E(_lI);return new F(function(){return (function(_lL,_lM,_lN,_lO,_lP,_lQ,_lR,_lS,_lT,_lU,_lV,_lW,_){var _lX=[0,_lL,_lM,_lN,_lO,0,0],_lY=function(_lZ,_m0,_){while(1){var _m1=(function(_m2,_m3,_){if(_m3<_lU){if(_m2<_lQ){var _m4=readOffAddr("w8",1,plusAddr(_lL,_m2),0),_m5=_m4,_=0;if(_m5>127){var _m6=function(_m7){var _m8=function(_m9){if(_m5<240){return [0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}else{switch(_lQ-_m2|0){case 1:return [0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];case 2:var _ma=readOffAddr("w8",1,plusAddr(_lL,_m2+1|0),0),_mb=_ma,_=0,_mc=function(_md){var _me=function(_mf){return E(_m5)==244?_mb<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mb>143?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];};if(_m5<241){return _me(_);}else{if(_m5>243){return _me(_);}else{if(_mb<128){return _me(_);}else{return _mb>191?_me(_):[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}}};if(E(_m5)==240){if(_mb<144){return _mc(_);}else{return _mb>191?_mc(_):[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}else{return _mc(_);}break;case 3:var _mg=readOffAddr("w8",1,plusAddr(_lL,_m2+1|0),0),_mh=_mg,_=0,_mi=readOffAddr("w8",1,plusAddr(_lL,_m2+2|0),0),_mj=_mi,_=0,_mk=function(_ml){var _mm=function(_mn){return E(_m5)==244?_mh<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mh>143?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mj<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mj>191?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];};if(_m5<241){return _mm(_);}else{if(_m5>243){return _mm(_);}else{if(_mh<128){return _mm(_);}else{if(_mh>191){return _mm(_);}else{if(_mj<128){return _mm(_);}else{return _mj>191?_mm(_):[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}}}}};if(E(_m5)==240){if(_mh<144){return _mk(_);}else{if(_mh>191){return _mk(_);}else{if(_mj<128){return _mk(_);}else{return _mj>191?_mk(_):[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}}}else{return _mk(_);}break;default:var _mo=readOffAddr("w8",1,plusAddr(_lL,_m2+1|0),0),_mp=_mo,_=0,_mq=readOffAddr("w8",1,plusAddr(_lL,_m2+2|0),0),_mr=_mq,_=0,_ms=readOffAddr("w8",1,plusAddr(_lL,_m2+3|0),0),_mt=_ms,_=0,_mu=function(_mv){var _=writeOffAddr("w32",4,_lR,_m3,(((((_m5&4294967295)-240|0)<<18)+(((_mp&4294967295)-128|0)<<12)|0)+(((_mr&4294967295)-128|0)<<6)|0)+((_mt&4294967295)-128|0)|0),_=0;return new F(function(){return _lY(_m2+4|0,_m3+1|0,_);});},_mw=function(_mx){var _my=function(_mz){return E(_m5)==244?_mp<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mp>143?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mr<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mr>191?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mt<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mt>191?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:B(_mu(_)):[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];};if(_m5<241){return _my(_);}else{if(_m5>243){return _my(_);}else{if(_mp<128){return _my(_);}else{if(_mp>191){return _my(_);}else{if(_mr<128){return _my(_);}else{if(_mr>191){return _my(_);}else{if(_mt<128){return _my(_);}else{return _mt>191?_my(_):B(_mu(_));}}}}}}}};if(E(_m5)==240){if(_mp<144){return _mw(_);}else{if(_mp>191){return _mw(_);}else{if(_mr<128){return _mw(_);}else{if(_mr>191){return _mw(_);}else{if(_mt<128){return _mw(_);}else{return _mt>191?_mw(_):B(_mu(_));}}}}}}else{return _mw(_);}}}};if(_m5<224){return new F(function(){return _m8(_);});}else{if(_m5>239){return new F(function(){return _m8(_);});}else{switch(_lQ-_m2|0){case 1:return [0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];case 2:var _mA=readOffAddr("w8",1,plusAddr(_lL,_m2+1|0),0),_mB=_mA,_=0,_mC=function(_mD){var _mE=function(_mF){var _mG=function(_mH){return _m5<238?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mB<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mB>191?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];};if(E(_m5)==237){if(_mB<128){return _mG(_);}else{return _mB>159?_mG(_):[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}else{return _mG(_);}};if(_m5<225){return _mE(_);}else{if(_m5>236){return _mE(_);}else{if(_mB<128){return _mE(_);}else{return _mB>191?_mE(_):[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}}};if(E(_m5)==224){if(_mB<160){return _mC(_);}else{return _mB>191?_mC(_):[0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}else{return _mC(_);}break;default:var _mI=readOffAddr("w8",1,plusAddr(_lL,_m2+1|0),0),_mJ=_mI,_=0,_mK=readOffAddr("w8",1,plusAddr(_lL,_m2+2|0),0),_mL=_mK,_=0,_mM=function(_mN){var _=writeOffAddr("w32",4,_lR,_m3,((((_m5&4294967295)-224|0)<<12)+(((_mJ&4294967295)-128|0)<<6)|0)+((_mL&4294967295)-128|0)|0),_=0;return new F(function(){return _lY(_m2+3|0,_m3+1|0,_);});},_mO=function(_mP){var _mQ=function(_mR){var _mS=function(_mT){return _m5<238?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mJ<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mJ>191?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mL<128?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:_mL>191?[0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]]:B(_mM(_));};if(E(_m5)==237){if(_mJ<128){return _mS(_);}else{if(_mJ>159){return _mS(_);}else{if(_mL<128){return _mS(_);}else{return _mL>191?_mS(_):B(_mM(_));}}}}else{return _mS(_);}};if(_m5<225){return _mQ(_);}else{if(_m5>236){return _mQ(_);}else{if(_mJ<128){return _mQ(_);}else{if(_mJ>191){return _mQ(_);}else{if(_mL<128){return _mQ(_);}else{return _mL>191?_mQ(_):B(_mM(_));}}}}}};if(E(_m5)==224){if(_mJ<160){return _mO(_);}else{if(_mJ>191){return _mO(_);}else{if(_mL<128){return _mO(_);}else{return _mL>191?_mO(_):B(_mM(_));}}}}else{return _mO(_);}}}}};if(_m5<192){return new F(function(){return _m6(_);});}else{if(_m5>223){return new F(function(){return _m6(_);});}else{if((_lQ-_m2|0)>=2){var _mU=readOffAddr("w8",1,plusAddr(_lL,_m2+1|0),0),_mV=_mU,_=0;if(_mV>=128){if(_mV<192){var _=writeOffAddr("w32",4,_lR,_m3,(((_m5&4294967295)-192|0)<<6)+((_mV&4294967295)-128|0)|0),_=0,_mW=_m2+2|0,_mX=_m3+1|0;_lZ=_mW;_m0=_mX;return null;}else{return [0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}else{return [0,_hL,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}else{return [0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}}}else{var _=writeOffAddr("w32",4,_lR,_m3,_m5&4294967295),_=0,_mW=_m2+1|0,_mX=_m3+1|0;_lZ=_mW;_m0=_mX;return null;}}else{return [0,_f7,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}}else{return [0,_f8,new T(function(){return _m2!=_lQ?[0,_lL,_lM,_lN,_lO,_m2,_lQ]:E(_lX);}),[0,_lR,_lS,_lT,_lU,_lV,_m3]];}})(_lZ,_m0,_);if(_m1!=null){return _m1;}}};return new F(function(){return _lY(_lP,_lW,_);});})(_lJ[1],_lJ[2],_lJ[3],_lJ[4],_lJ[5],_lJ[6],_lK[1],_lK[2],_lK[3],_lK[4],_lK[5],_lK[6],_);});},function(_mY,_mZ,_){return new F(function(){return _h1(_lG,_mY,_mZ,_);});},_ld,_ld,_lb];},function(_){return [0,function(_n0,_n1,_){var _n2=E(_n0),_n3=E(_n1);return new F(function(){return (function(_n4,_n5,_n6,_n7,_n8,_n9,_na,_nb,_nc,_nd,_ne,_nf,_){var _ng=[0,_n4,_n5,_n6,_n7,0,0],_nh=function(_ni,_nj,_){while(1){var _nk=(function(_nl,_nm,_){if(_nm<_nd){if(_nl<_n9){var _nn=readOffAddr("w32",4,_n4,_nl),_no=_nn,_=0,_np=_no;if(_np>127){if(_np>2047){if(_np>65535){if((_nd-_nm|0)>=4){var _=writeOffAddr("w8",1,plusAddr(_na,_nm),0,((_np>>18)+240|0)>>>0&255),_=0,_nq=63>>>0,_=writeOffAddr("w8",1,plusAddr(_na,_nm+1|0),0,(((_np>>12>>>0&_nq)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_na,_nm+2|0),0,(((_np>>6>>>0&_nq)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_na,_nm+3|0),0,(((_np>>>0&_nq)>>>0&4294967295)+128|0)>>>0&255),_=0,_nr=_nl+1|0,_ns=_nm+4|0;_ni=_nr;_nj=_ns;return null;}else{return [0,_f8,new T(function(){return _nl!=_n9?[0,_n4,_n5,_n6,_n7,_nl,_n9]:E(_ng);}),[0,_na,_nb,_nc,_nd,_ne,_nm]];}}else{var _nt=function(_nu){var _nv=function(_nw){if((_nd-_nm|0)>=3){var _=writeOffAddr("w8",1,plusAddr(_na,_nm),0,((_np>>12)+224|0)>>>0&255),_=0,_nx=63>>>0,_=writeOffAddr("w8",1,plusAddr(_na,_nm+1|0),0,(((_np>>6>>>0&_nx)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_na,_nm+2|0),0,(((_np>>>0&_nx)>>>0&4294967295)+128|0)>>>0&255),_=0;return new F(function(){return _nh(_nl+1|0,_nm+3|0,_);});}else{return [0,_f8,new T(function(){return _nl!=_n9?[0,_n4,_n5,_n6,_n7,_nl,_n9]:E(_ng);}),[0,_na,_nb,_nc,_nd,_ne,_nm]];}};if(56320>_np){return new F(function(){return _nv(_);});}else{return _np>57343?B(_nv(_)):[0,_hL,new T(function(){return _nl!=_n9?[0,_n4,_n5,_n6,_n7,_nl,_n9]:E(_ng);}),[0,_na,_nb,_nc,_nd,_ne,_nm]];}};if(55296>_np){return new F(function(){return _nt(_);});}else{return _np>56319?B(_nt(_)):[0,_hL,new T(function(){return _nl!=_n9?[0,_n4,_n5,_n6,_n7,_nl,_n9]:E(_ng);}),[0,_na,_nb,_nc,_nd,_ne,_nm]];}}}else{if((_nd-_nm|0)>=2){var _=writeOffAddr("w8",1,plusAddr(_na,_nm),0,((_np>>6)+192|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_na,_nm+1|0),0,(((_np>>>0&63>>>0)>>>0&4294967295)+128|0)>>>0&255),_=0,_nr=_nl+1|0,_ns=_nm+2|0;_ni=_nr;_nj=_ns;return null;}else{return [0,_f8,new T(function(){return _nl!=_n9?[0,_n4,_n5,_n6,_n7,_nl,_n9]:E(_ng);}),[0,_na,_nb,_nc,_nd,_ne,_nm]];}}}else{var _=writeOffAddr("w8",1,plusAddr(_na,_nm),0,_np>>>0&255),_=0,_nr=_nl+1|0,_ns=_nm+1|0;_ni=_nr;_nj=_ns;return null;}}else{return [0,_f7,new T(function(){return _nl!=_n9?[0,_n4,_n5,_n6,_n7,_nl,_n9]:E(_ng);}),[0,_na,_nb,_nc,_nd,_ne,_nm]];}}else{return [0,_f8,new T(function(){return _nl!=_n9?[0,_n4,_n5,_n6,_n7,_nl,_n9]:E(_ng);}),[0,_na,_nb,_nc,_nd,_ne,_nm]];}})(_ni,_nj,_);if(_nk!=null){return _nk;}}};return new F(function(){return _nh(_n8,_nf,_);});})(_n2[1],_n2[2],_n2[3],_n2[4],_n2[5],_n2[6],_n3[1],_n3[2],_n3[3],_n3[4],_n3[5],_n3[6],_);});},function(_mY,_mZ,_){return new F(function(){return _hp(_lG,_mY,_mZ,_);});},_ld,_ld,_lb];}];})(_lm));}):new T(function(){return B((function(_ny){return [0,new T(function(){return unCStr("UTF-32LE");}),function(_){return [0,_l3,function(_nz,_nA,_){return new F(function(){return _h1(_ny,_nz,_nA,_);});},_l8,_l8,_l9];},function(_){return [0,function(_nB,_nC,_){var _nD=E(_nB),_nE=E(_nC);return new F(function(){return (function(_nF,_nG,_nH,_nI,_nJ,_nK,_nL,_nM,_nN,_nO,_nP,_nQ,_){var _nR=[0,_nF,_nG,_nH,_nI,0,0],_nS=function(_nT,_nU,_){if(_nT<_nK){if((_nO-_nU|0)>=4){var _nV=readOffAddr("w32",4,_nF,_nT),_nW=_nV,_=0,_nX=_nW,_nY=function(_nZ){if(56320>_nX){var _=writeOffAddr("w8",1,plusAddr(_nL,_nU),0,_nX>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_nL,_nU+1|0),0,_nX>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_nL,_nU+2|0),0,_nX>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_nL,_nU+3|0),0,_nX>>24>>>0&255),_=0;return new F(function(){return _nS(_nT+1|0,_nU+4|0,_);});}else{if(_nX>57343){var _=writeOffAddr("w8",1,plusAddr(_nL,_nU),0,_nX>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_nL,_nU+1|0),0,_nX>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_nL,_nU+2|0),0,_nX>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_nL,_nU+3|0),0,_nX>>24>>>0&255),_=0;return new F(function(){return _nS(_nT+1|0,_nU+4|0,_);});}else{return [0,_hL,new T(function(){return _nT!=_nK?[0,_nF,_nG,_nH,_nI,_nT,_nK]:E(_nR);}),[0,_nL,_nM,_nN,_nO,_nP,_nU]];}}};if(55296>_nX){return new F(function(){return _nY(_);});}else{return _nX>56319?B(_nY(_)):[0,_hL,new T(function(){return _nT!=_nK?[0,_nF,_nG,_nH,_nI,_nT,_nK]:E(_nR);}),[0,_nL,_nM,_nN,_nO,_nP,_nU]];}}else{return [0,_f8,new T(function(){return _nT!=_nK?[0,_nF,_nG,_nH,_nI,_nT,_nK]:E(_nR);}),[0,_nL,_nM,_nN,_nO,_nP,_nU]];}}else{return [0,_f7,new T(function(){return _nT!=_nK?[0,_nF,_nG,_nH,_nI,_nT,_nK]:E(_nR);}),[0,_nL,_nM,_nN,_nO,_nP,_nU]];}};return new F(function(){return _nS(_nJ,_nQ,_);});})(_nD[1],_nD[2],_nD[3],_nD[4],_nD[5],_nD[6],_nE[1],_nE[2],_nE[3],_nE[4],_nE[5],_nE[6],_);});},function(_nz,_nA,_){return new F(function(){return _hp(_ny,_nz,_nA,_);});},_l8,_l8,_l9];}];})(_lm));}):new T(function(){return B((function(_o0){return [0,new T(function(){return unCStr("UTF-32BE");}),function(_){return [0,_kX,function(_nz,_nA,_){return new F(function(){return _h1(_o0,_nz,_nA,_);});},_l8,_l8,_l9];},function(_){return [0,function(_o1,_o2,_){var _o3=E(_o1),_o4=E(_o2);return new F(function(){return _jv(_o3[1],_o3[2],_o3[3],_o3[4],_o3[5],_o3[6],_o4[1],_o4[2],_o4[3],_o4[4],_o4[5],_o4[6],_);});},function(_nz,_nA,_){return new F(function(){return _hp(_o0,_nz,_nA,_);});},_l8,_l8,_l9];}];})(_lm));}):new T(function(){return B((function(_o5){return [0,new T(function(){return unCStr("UTF-32");}),function(_){var _o6=nMV(_36),_o7=_o6;return [0,function(_o8,_o9,_){var _oa=E(_o8);return new F(function(){return (function(_ob,_oc,_od,_oe,_of,_og,_oh,_oi,_){var _oj=rMV(_ob),_ok=_oj,_ol=E(_ok);if(!_ol[0]){if((_oh-_og|0)>=4){var _om=readOffAddr("w8",1,plusAddr(_oc,_og),0),_on=_om,_=0,_oo=readOffAddr("w8",1,plusAddr(_oc,_og+1|0),0),_op=_oo,_=0,_oq=readOffAddr("w8",1,plusAddr(_oc,_og+2|0),0),_or=_oq,_=0,_os=readOffAddr("w8",1,plusAddr(_oc,_og+3|0),0),_ot=_os,_=0,_ou=function(_ov){if(E(_on)==255){if(E(_op)==254){if(!E(_or)){if(!E(_ot)){var _=wMV(_ob,[1,_l3]),_ow=E(_oi);return new F(function(){return _kp(_oc,_od,_oe,_of,_og+4|0,_oh,_ow[1],_ow[2],_ow[3],_ow[4],_ow[5],_ow[6],_);});}else{var _=wMV(_ob,_l2),_ox=E(_oi);return new F(function(){return _jR(_oc,_od,_oe,_of,_og,_oh,_ox[1],_ox[2],_ox[3],_ox[4],_ox[5],_ox[6],_);});}}else{var _=wMV(_ob,_l2),_oy=E(_oi);return new F(function(){return _jR(_oc,_od,_oe,_of,_og,_oh,_oy[1],_oy[2],_oy[3],_oy[4],_oy[5],_oy[6],_);});}}else{var _=wMV(_ob,_l2),_oz=E(_oi);return new F(function(){return _jR(_oc,_od,_oe,_of,_og,_oh,_oz[1],_oz[2],_oz[3],_oz[4],_oz[5],_oz[6],_);});}}else{var _=wMV(_ob,_l2),_oA=E(_oi);return new F(function(){return _jR(_oc,_od,_oe,_of,_og,_oh,_oA[1],_oA[2],_oA[3],_oA[4],_oA[5],_oA[6],_);});}};if(!E(_on)){if(!E(_op)){if(E(_or)==254){if(E(_ot)==255){var _=wMV(_ob,_l2),_oB=E(_oi);return new F(function(){return _jR(_oc,_od,_oe,_of,_og+4|0,_oh,_oB[1],_oB[2],_oB[3],_oB[4],_oB[5],_oB[6],_);});}else{return new F(function(){return _ou(_);});}}else{return new F(function(){return _ou(_);});}}else{return new F(function(){return _ou(_);});}}else{return new F(function(){return _ou(_);});}}else{return [0,_f7,[0,_oc,_od,_oe,_of,_og,_oh],_oi];}}else{return new F(function(){return A(_ol[1],[[0,_oc,_od,_oe,_of,_og,_oh],_oi,_]);});}})(_o7,_oa[1],_oa[2],_oa[3],_oa[4],_oa[5],_oa[6],_o9,_);});},function(_nz,_nA,_){return new F(function(){return _h1(_o5,_nz,_nA,_);});},_l8,function(_){return rMV(_o7);},function(_oC,_){var _=wMV(_o7,_oC);return _2u;}];},function(_){var _oD=nMV(_jr),_oE=_oD;return [0,function(_oF,_oG,_){var _oH=E(_oG);return new F(function(){return (function(_oI,_oJ,_oK,_oL,_oM,_oN,_oO,_oP,_){var _oQ=rMV(_oI),_oR=_oQ;if(!E(_oR)){if((_oN-_oP|0)>=4){var _=wMV(_oI,_if),_=writeOffAddr("w8",1,plusAddr(_oK,_oP),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_oK,_oP+1|0),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_oK,_oP+2|0),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_oK,_oP+3|0),0,255),_=0,_oS=E(_oJ);return new F(function(){return _jv(_oS[1],_oS[2],_oS[3],_oS[4],_oS[5],_oS[6],_oK,_oL,_oM,_oN,_oO,_oP+4|0,_);});}else{return [0,_f8,_oJ,[0,_oK,_oL,_oM,_oN,_oO,_oP]];}}else{var _oT=E(_oJ);return new F(function(){return _jv(_oT[1],_oT[2],_oT[3],_oT[4],_oT[5],_oT[6],_oK,_oL,_oM,_oN,_oO,_oP,_);});}})(_oE,_oF,_oH[1],_oH[2],_oH[3],_oH[4],_oH[5],_oH[6],_);});},function(_nz,_nA,_){return new F(function(){return _hp(_o5,_nz,_nA,_);});},_l8,function(_){return rMV(_oE);},function(_oU,_){var _=wMV(_oE,_oU);return _2u;}];}];})(_lm));}):new T(function(){return B((function(_oV){return [0,new T(function(){return unCStr("UTF16-LE");}),function(_){return [0,_jm,function(_oW,_oX,_){return new F(function(){return _h1(_oV,_oW,_oX,_);});},_js,_js,_jt];},function(_){return [0,function(_oY,_oZ,_){var _p0=E(_oY),_p1=E(_oZ);return new F(function(){return (function(_p2,_p3,_p4,_p5,_p6,_p7,_p8,_p9,_pa,_pb,_pc,_pd,_){var _pe=[0,_p2,_p3,_p4,_p5,0,0],_pf=function(_pg,_ph,_){while(1){var _pi=(function(_pj,_pk,_){if(_pj<_p7){if((_pb-_pk|0)>=2){var _pl=readOffAddr("w32",4,_p2,_pj),_pm=_pl,_=0,_pn=_pm;if(_pn>=65536){if((_pb-_pk|0)>=4){var _po=_pn-65536|0,_=writeOffAddr("w8",1,plusAddr(_p8,_pk),0,_po>>10>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_p8,_pk+1|0),0,((_po>>18)+216|0)>>>0&255),_=0,_pp=(_po>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_p8,_pk+2|0),0,_pp>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_p8,_pk+3|0),0,((_pp>>8)+220|0)>>>0&255),_=0,_pq=_pj+1|0,_pr=_pk+4|0;_pg=_pq;_ph=_pr;return null;}else{return [0,_f8,new T(function(){return _pj!=_p7?[0,_p2,_p3,_p4,_p5,_pj,_p7]:E(_pe);}),[0,_p8,_p9,_pa,_pb,_pc,_pk]];}}else{var _ps=function(_pt){if(56320>_pn){var _=writeOffAddr("w8",1,plusAddr(_p8,_pk),0,_pn>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_p8,_pk+1|0),0,_pn>>8>>>0&255),_=0;return new F(function(){return _pf(_pj+1|0,_pk+2|0,_);});}else{if(_pn>57343){var _=writeOffAddr("w8",1,plusAddr(_p8,_pk),0,_pn>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_p8,_pk+1|0),0,_pn>>8>>>0&255),_=0;return new F(function(){return _pf(_pj+1|0,_pk+2|0,_);});}else{return [0,_hL,new T(function(){return _pj!=_p7?[0,_p2,_p3,_p4,_p5,_pj,_p7]:E(_pe);}),[0,_p8,_p9,_pa,_pb,_pc,_pk]];}}};if(55296>_pn){return new F(function(){return _ps(_);});}else{return _pn>56319?B(_ps(_)):[0,_hL,new T(function(){return _pj!=_p7?[0,_p2,_p3,_p4,_p5,_pj,_p7]:E(_pe);}),[0,_p8,_p9,_pa,_pb,_pc,_pk]];}}}else{return [0,_f8,new T(function(){return _pj!=_p7?[0,_p2,_p3,_p4,_p5,_pj,_p7]:E(_pe);}),[0,_p8,_p9,_pa,_pb,_pc,_pk]];}}else{return [0,_f7,new T(function(){return _pj!=_p7?[0,_p2,_p3,_p4,_p5,_pj,_p7]:E(_pe);}),[0,_p8,_p9,_pa,_pb,_pc,_pk]];}})(_pg,_ph,_);if(_pi!=null){return _pi;}}};return new F(function(){return _pf(_p6,_pd,_);});})(_p0[1],_p0[2],_p0[3],_p0[4],_p0[5],_p0[6],_p1[1],_p1[2],_p1[3],_p1[4],_p1[5],_p1[6],_);});},function(_oW,_oX,_){return new F(function(){return _hp(_oV,_oW,_oX,_);});},_js,_js,_jt];}];})(_lm));}):new T(function(){return B((function(_pu){return [0,new T(function(){return unCStr("UTF-16BE");}),function(_){return [0,_jg,function(_oW,_oX,_){return new F(function(){return _h1(_pu,_oW,_oX,_);});},_js,_js,_jt];},function(_){return [0,function(_pv,_pw,_){var _px=E(_pv),_py=E(_pw);return new F(function(){return _hM(_px[1],_px[2],_px[3],_px[4],_px[5],_px[6],_py[1],_py[2],_py[3],_py[4],_py[5],_py[6],_);});},function(_oW,_oX,_){return new F(function(){return _hp(_pu,_oW,_oX,_);});},_js,_js,_jt];}];})(_lm));}):new T(function(){return B((function(_pz){return [0,new T(function(){return unCStr("UTF-16");}),function(_){var _pA=nMV(_36),_pB=_pA;return [0,function(_pC,_pD,_){var _pE=E(_pC);return new F(function(){return (function(_pF,_pG,_pH,_pI,_pJ,_pK,_pL,_pM,_){var _pN=rMV(_pF),_pO=_pN,_pP=E(_pO);if(!_pP[0]){if((_pL-_pK|0)>=2){var _pQ=readOffAddr("w8",1,plusAddr(_pG,_pK),0),_pR=_pQ,_=0,_pS=readOffAddr("w8",1,plusAddr(_pG,_pK+1|0),0),_pT=_pS,_=0,_pU=function(_pV){if(E(_pR)==255){if(E(_pT)==254){var _=wMV(_pF,[1,_jm]),_pW=E(_pM);return _iL(_pG,_pH,_pI,_pJ,_pK+2|0,_pL,_pW[1],_pW[2],_pW[3],_pW[4],_pW[5],_pW[6],_);}else{var _=wMV(_pF,_jl),_pX=E(_pM);return _ig(_pG,_pH,_pI,_pJ,_pK,_pL,_pX[1],_pX[2],_pX[3],_pX[4],_pX[5],_pX[6],_);}}else{var _=wMV(_pF,_jl),_pY=E(_pM);return _ig(_pG,_pH,_pI,_pJ,_pK,_pL,_pY[1],_pY[2],_pY[3],_pY[4],_pY[5],_pY[6],_);}};if(E(_pR)==254){if(E(_pT)==255){var _=wMV(_pF,_jl),_pZ=E(_pM);return _ig(_pG,_pH,_pI,_pJ,_pK+2|0,_pL,_pZ[1],_pZ[2],_pZ[3],_pZ[4],_pZ[5],_pZ[6],_);}else{return _pU(_);}}else{return _pU(_);}}else{return [0,_f7,[0,_pG,_pH,_pI,_pJ,_pK,_pL],_pM];}}else{return new F(function(){return A(_pP[1],[[0,_pG,_pH,_pI,_pJ,_pK,_pL],_pM,_]);});}})(_pB,_pE[1],_pE[2],_pE[3],_pE[4],_pE[5],_pE[6],_pD,_);});},function(_oW,_oX,_){return new F(function(){return _h1(_pz,_oW,_oX,_);});},_js,function(_){return rMV(_pB);},function(_q0,_){var _=wMV(_pB,_q0);return _2u;}];},function(_){var _q1=nMV(_jr),_q2=_q1;return [0,function(_q3,_q4,_){var _q5=E(_q4);return new F(function(){return (function(_q6,_q7,_q8,_q9,_qa,_qb,_qc,_qd,_){var _qe=rMV(_q6),_qf=_qe;if(!E(_qf)){if((_qb-_qd|0)>=2){var _=wMV(_q6,_if),_=writeOffAddr("w8",1,plusAddr(_q8,_qd),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_q8,_qd+1|0),0,255),_=0,_qg=E(_q7);return new F(function(){return _hM(_qg[1],_qg[2],_qg[3],_qg[4],_qg[5],_qg[6],_q8,_q9,_qa,_qb,_qc,_qd+2|0,_);});}else{return [0,_f8,_q7,[0,_q8,_q9,_qa,_qb,_qc,_qd]];}}else{var _qh=E(_q7);return new F(function(){return _hM(_qh[1],_qh[2],_qh[3],_qh[4],_qh[5],_qh[6],_q8,_q9,_qa,_qb,_qc,_qd,_);});}})(_q2,_q3,_q5[1],_q5[2],_q5[3],_q5[4],_q5[5],_q5[6],_);});},function(_oW,_oX,_){return new F(function(){return _hp(_pz,_oW,_oX,_);});},_js,function(_){return rMV(_q2);},function(_qi,_){var _=wMV(_q2,_qi);return _2u;}];}];})(_lm));});})(1,new T(function(){return _dS(function(_){var _=0,_qj=localeEncoding(),_qk=_qj;return (function(_ql,_){var _qm=(function(_qn,_){while(1){var _qo=readOffAddr("i8",1,_ql,_qn),_qp=_qo;if(!E(_qp)){return [0,_qn];}else{var _qq=_qn+1|0;_qn=_qq;continue;}}})(0,_),_qr=_qm,_qs=E(_qr)[1];if(_qs>0){return (function(_qt,_qu,_){while(1){var _qv=readOffAddr("i8",1,_ql,_qu),_qw=_qv;if(_qu>0){var _qx=[1,[0,_qw>>>0&255&4294967295],_qt],_qy=_qu-1|0;_qt=_qx;_qu=_qy;continue;}else{return [1,[0,_qw>>>0&255&4294967295],_qt];}}})(_1,_qs-1|0,_);}else{return _1;}})(_qk,_);});}),_);});})),_qz=_ll;return [0,function(_){return rMV(_qz);},function(_qA,_){var _=wMV(_qz,_qA);return _2u;}];});}))[1],[_])),_qB=_lk,_qC=B((function(_qD,_qE,_){var _qF=(function(_qG,_){while(1){var _qH=readOffAddr("i8",1,_qE,_qG),_qI=_qH;if(!E(_qI)){return [0,_qG];}else{var _qJ=_qG+1|0;_qG=_qJ;continue;}}})(0,_),_qK=_qF;return new F(function(){return (function(_qL,_qM,_qN,_){var _qO=0,_qP=_qO;switch(E(_qP)){case 0:return new F(function(){return (function(_){var _qQ=B(A(_qL,[_])),_qR=_qQ,_qS=jsCatch(function(_){return new F(function(){return new T(function(){return B(A(_qN,[_qR]));})();});},function(_qT,_){var _qU=B(A(_qM,[_qR,_])),_qV=_qU;return die(_qT);}),_qW=_qS,_qX=B(A(_qM,[_qR,_])),_qY=_qX;return _qW;})();});break;case 1:var _qZ=B(A(_qL,[_])),_r0=_qZ,_r1=jsCatch(new T(function(){return B(A(_qN,[_r0]));}),function(_r2,_){var _r3=B(A(_qM,[_r0,_])),_r4=_r3;return die(_r2);}),_r5=_r1,_r6=B(A(_qM,[_r0,_])),_r7=_r6;return _r5;default:var _r8=B(A(_qL,[_])),_r9=_r8,_ra=jsCatch(new T(function(){return B(A(_qN,[_r9]));}),function(_rb,_){var _rc=B(A(_qM,[_r9,_])),_rd=_rc;return die(_rb);}),_re=_ra,_rf=B(A(_qM,[_r9,_])),_rg=_rf;return _re;}})(E(_qD)[2],function(_rh){return E(E(_rh)[3]);},function(_ri,_){var _rj=nMV(_eM),_rk=_rj,_rl=E(_qK)[1],_rm=function(_rn){var _ro=imul(_rn,4)|0;if(_ro>=0){var _rp=nMV(_eM),_rq=_rp,_rr=newByteArr(_ro),_rs=_rr,_rt=function(_ru,_){var _rv=E(_ri),_rw=B(A(_rv[1],[_ru,[0,_rs,[1,_rs,_rq],1,_rn,0,0],_])),_rx=_rw,_ry=E(_rx),_rz=_ry[3],_rA=E(_ry[2]);if(_rA[5]!=_rA[6]){if(E(_ry[1])==1){var _rB=E(_rz),_rC=_rB[2],_rD=_eA(_ee,_rB[6]-_rB[5]|0,[0,_rB[1]],_),_rE=_rD,_=0,_rF=_rt(_rA,_),_rG=_rF;return new T(function(){return _1v(_rE,_rG);});}else{var _rH=B(A(_rv[2],[_rA,_rz,_])),_rI=_rH,_rJ=E(_rI),_rK=E(_rJ[2]),_rL=_rK[2],_rM=_eA(_ee,_rK[6]-_rK[5]|0,[0,_rK[1]],_),_rN=_rM,_=0,_rO=_rt(_rJ[1],_),_rP=_rO;return new T(function(){return _1v(_rN,_rP);});}}else{var _rQ=E(_rz),_rR=_rQ[2],_rS=_eA(_ee,_rQ[6]-_rQ[5]|0,[0,_rQ[1]],_),_rT=_rS,_=0;return _rT;}};return _rt([0,_qE,[0,_rk],0,_rl,0,_rl],_);}else{return E(new T(function(){return err(new T(function(){return unCStr("mallocForeignPtrBytes: size must be >= 0");}));}));}};return _rl>1?_rm(_rl):_rm(1);},_);});})(_qB,_lj,_)),_rU=_qC;return [0,_lg,new T(function(){switch(E(_lf)){case 1:var _rV=6;break;case 2:var _rV=1;break;case 3:var _rV=1;break;case 4:var _rV=18;break;case 5:var _rV=14;break;case 6:var _rV=1;break;case 7:var _rV=3;break;case 8:var _rV=12;break;case 9:var _rV=12;break;case 10:var _rV=1;break;case 11:var _rV=3;break;case 12:var _rV=3;break;case 13:var _rV=6;break;case 15:var _rV=12;break;case 16:var _rV=2;break;case 17:var _rV=0;break;case 18:var _rV=15;break;case 19:var _rV=15;break;case 20:var _rV=13;break;case 21:var _rV=13;break;case 22:var _rV=12;break;case 23:var _rV=3;break;case 24:var _rV=3;break;case 25:var _rV=5;break;case 26:var _rV=2;break;case 27:var _rV=6;break;case 28:var _rV=3;break;case 29:var _rV=15;break;case 30:var _rV=6;break;case 31:var _rV=3;break;case 32:var _rV=17;break;case 33:var _rV=12;break;case 34:var _rV=15;break;case 35:var _rV=2;break;case 36:var _rV=12;break;case 37:var _rV=3;break;case 38:var _rV=15;break;case 39:var _rV=8;break;case 40:var _rV=12;break;case 42:var _rV=1;break;case 43:var _rV=17;break;case 60:var _rV=12;break;case 61:var _rV=1;break;case 62:var _rV=16;break;case 63:var _rV=3;break;case 64:var _rV=1;break;case 66:var _rV=5;break;case 67:var _rV=17;break;case 69:var _rV=8;break;case 70:var _rV=17;break;case 71:var _rV=10;break;case 72:var _rV=15;break;case 74:var _rV=13;break;case 78:var _rV=17;break;case 84:var _rV=12;break;case 87:var _rV=3;break;case 88:var _rV=12;break;case 89:var _rV=12;break;case 90:var _rV=3;break;case 91:var _rV=10;break;case 92:var _rV=15;break;case 93:var _rV=10;break;case 94:var _rV=15;break;case 95:var _rV=15;break;case 96:var _rV=15;break;case 97:var _rV=15;break;case 98:var _rV=2;break;case 99:var _rV=15;break;case 100:var _rV=17;break;case 101:var _rV=1;break;case 102:var _rV=17;break;case 104:var _rV=17;break;case 105:var _rV=3;break;case 106:var _rV=0;break;case 107:var _rV=12;break;case 108:var _rV=5;break;case 109:var _rV=3;break;case 110:var _rV=16;break;case 111:var _rV=1;break;case 112:var _rV=1;break;case 113:var _rV=1;break;case 114:var _rV=0;break;case 115:var _rV=0;break;case 116:var _rV=17;break;case 122:var _rV=6;break;default:var _rV=11;}return _rV;}),_le,_rU,[1,[0,_lf]],_lh];});},_rW=[1,I_fromBits([2808348672,232830643])],_rX=function(_){var _rY=B((function(_){var _rZ=newByteArr(8),_s0=_rZ,_s1=_s0,_s2=_s1,_s3=_s2,_=writeOffAddr("i32",4,_s3,0,0),_=writeOffAddr("i32",4,_s3,1,0),_s4=gettimeofday(_s3,0),_s5=_s4;if(E(_s5)==(-1)){var _s6=__hscore_get_errno(),_s7=_s6;return _fc(_gj(new T(function(){return unCStr("gettimeofday");}),_s7,_36,_36),_);}else{var _s8=readOffAddr("i32",4,_s3,0),_s9=_s8,_sa=readOffAddr("i32",4,_s3,1),_sb=_sa,_=0;return [0,[0,_s9],[0,_sb]];}})(_)),_sc=_rY;return new T(function(){var _sd=E(_sc);if(!_2w(_rW,_b8)){var _se=_67(_6B(_ax(E(_sd[1])[1]),_2n),_aC(_6B(_6B(_ax(E(_sd[2])[1]),_2n),_2n),_rW));}else{var _se=E(_5U);}var _sf=_se,_sg=_sf;return _sg;});},_sh=new T(function(){return unCStr("worldStr");}),_si=new T(function(){return unCStr("clickedLast");}),_sj=[0,0],_sk=function(_sl,_sm){var _sn=E(_sl);if(!_sn){return [0];}else{var _so=E(_sm);return _so[0]==0?[0]:[1,_so[1],new T(function(){return _sk(_sn-1|0,_so[2]);})];}},_sp=[0,1.0e-2],_sq=function(_sr,_ss,_st,_su,_sv,_){var _sw=_rX(_),_sx=_sw,_sy=_dY(_1m,_si,_),_sz=_sy,_sA=new T(function(){var _sB=_bk(_sx);return [0,_sB[1],_sB[2]];});return function(_sC){var _sD=new T(function(){var _sE=E(_sz);if(!_sE[0]){var _sF=E(_sC),_sG=E(_8x(_sp,_sF[1],_sF[2])[2]);}else{var _sH=E(_sE[1]),_sI=E(_sC),_sJ=E(_8x(_sp,_sI[1],_sI[2])[2]),_sG=E((function(_sK,_sL,_sM,_sN){return [0,_2u,[0,new T(function(){return _7a(function(_sO,_sP){var _sQ=E(_sP);return [0,_sQ[1],new T(function(){var _sR=E(_sQ[2]),_sS=_sR[1],_sT=_sR[2],_sU=new T(function(){var _sV=E(_sS);return [0,new T(function(){return _7f(_sV[1]);}),new T(function(){return _7f(_sV[2]);})];});return [0,_sS,[0,new T(function(){return [0,E(E(_sT)[1])[1]+(E(E(_sL)[1])[1]+E(E(_sU)[1])[1])*E(_sK)[1]];}),new T(function(){return [0,E(E(_sT)[2])[1]+(E(E(_sL)[2])[1]+E(E(_sU)[2])[1])*E(_sK)[1]];})],_sR[3]];}),_sQ[3]];},_sM);}),_sN]];})(_sp,[0,_sH[1],_sH[2]],_sJ[1],_sJ[2])[2]);}return _sG;});return [0,function(_){var _sW=E(_sr),_sX=_sW[1],_sY=jsResetCanvas(_sW[2]),_sZ=E(_sD),_t0=_sZ[1],_t1=(function(_t2,_t3,_t4,_){while(1){var _t5=E(_t2);if(!_t5[0]){return _2u;}else{var _t6=E(E(E(_t5[1])[2])[1]),_t7=jsBeginPath(_t3),_t8=E(_t6[2])[1],_t9=E(_t6[1])[1],_ta=jsMoveTo(_t3,_t9+3,_t8),_tb=jsArc(_t3,_t9,_t8,3,0,6.283185307179586),_tc=jsStroke(_t3);_t2=_t5[2];var _td=_;_t4=_td;continue;}}})(new T(function(){return (function(_te){var _tf=E(_te);switch(_tf[0]){case 0:return [0];case 1:return [1,[0,[0,_tf[1]],E(_tf[2])[2]],_1];default:if(!E(_tf[1])){if(_tf[2]>=0){return _2c(_2g(_1,_tf));}else{return _28(_2g(new T(function(){return _2g(_1,_tf[3]);}),_tf[4]));}}else{return _24(_2g(_1,_tf));}}})(_t0);}),_sX,_,_),_tg=_t1,_th=function(_ti){var _tj=E(_ti);return _tj[0]==0?function(_tk,_){return _2u;}:function(_tl,_){var _tm=B(A(new T(function(){var _tn=E(_tj[1]);return (function(_to,_tp){return function(_tq,_){var _tr=E(_tq),_ts=_tr[1],_tt=E(new T(function(){return [0,"fillStyle"];}))[1],_tu=jsGet(_ts,_tt),_tv=_tu,_tw=E(new T(function(){return [0,"strokeStyle"];}))[1],_tx=jsGet(_ts,_tw),_ty=_tx,_tz=E(new T(function(){return [0,(function(_tA){var _tB=E(_tA);if(!_tB[0]){var _tC=jsCat([1,new T(function(){return [0,"rgb("];}),[1,new T(function(){var _tD=String(_tB[1]),_tE=_tD;return [0,_tE];}),[1,_d3,[1,new T(function(){var _tF=String(_tB[2]),_tG=_tF;return [0,_tG];}),[1,_d3,[1,new T(function(){var _tH=String(_tB[3]),_tI=_tH;return [0,_tI];}),_d4]]]]]],E(_d2)[1]),_tJ=_tC;return E(_tJ);}else{var _tK=jsCat([1,new T(function(){return [0,"rgba("];}),[1,new T(function(){var _tL=String(_tB[1]),_tM=_tL;return [0,_tM];}),[1,_d3,[1,new T(function(){var _tN=String(_tB[2]),_tO=_tN;return [0,_tO];}),[1,_d3,[1,new T(function(){var _tP=String(_tB[3]),_tQ=_tP;return [0,_tQ];}),[1,_d3,[1,new T(function(){var _tR=String(_tB[4]),_tS=_tR;return [0,_tS];}),_d4]]]]]]]],E(_d2)[1]),_tT=_tK;return E(_tT);}})(_to)];}))[1],_tU=jsSet(_ts,_tt,_tz),_tV=jsSet(_ts,_tw,_tz),_tW=B(A(_tp,[_tr,_])),_tX=_tW,_tY=jsSet(_ts,_tt,_tv),_tZ=jsSet(_ts,_tw,_ty);return _2u;};})(new T(function(){return [1,0,0,0,E(E(_tn[3])[1])[1]];},1),function(_u0,_){return (function(_u1,_u2,_){var _u3=jsBeginPath(_u2),_u4=B(A(_u1,[[0,_u2],_])),_u5=_u4,_u6=jsStroke(_u2);return _2u;})(new T(function(){var _u7=new T(function(){var _u8=E(_4n(E(_tn[2])[1],_t0)[1]);if(!_u8[0]){var _u9=E(_8w);}else{var _u9=E(E(E(_u8[1])[3])[1]);}var _ua=_u9,_ub=_ua;return _ub;}),_uc=new T(function(){var _ud=E(_4n(E(_tn[1])[1],_t0)[1]);if(!_ud[0]){var _ue=E(_8w);}else{var _ue=E(E(E(_ud[1])[3])[1]);}var _uf=_ue,_ug=_uf;return _ug;});return (function(_uh){var _ui=E(_uh);if(!_ui[0]){return function(_uj,_){return _2u;};}else{var _uk=E(_ui[1]);return function(_ul,_){var _um=E(_ul)[1],_un=jsMoveTo(_um,E(_uk[1])[1],E(_uk[2])[1]);return (function(_uo,_){while(1){var _up=E(_uo);if(!_up[0]){return _2u;}else{var _uq=E(_up[1]),_ur=jsLineTo(_um,E(_uq[1])[1],E(_uq[2])[1]);_uo=_up[2];continue;}}})(_ui[2],_);};}})([1,[0,new T(function(){return [0,E(E(_uc)[1])[1]];}),new T(function(){return [0,E(E(_uc)[2])[1]];})],[1,[0,new T(function(){return [0,E(E(_u7)[1])[1]];}),new T(function(){return [0,E(E(_u7)[2])[1]];})],_1]]);}),E(_u0)[1],_);});}),[_tl,_])),_us=_tm;return new F(function(){return A(new T(function(){return B(_th(_tj[2]));}),[_tl,_]);});};},_ut=B(A(_th,[(function(_uu){var _uv=E(_uu);switch(_uv[0]){case 0:return E(new T(function(){return _1G(_1);}));case 1:return _1G([1,[0,[0,_uv[1]],_uv[2]],_1]);default:if(!E(_uv[1])){if(_uv[2]>=0){return _1G(_1o(_1,_uv));}else{return _1G(_1o(new T(function(){return _1o(_1,_uv[3]);}),_uv[4]));}}else{return _1G(_1o(_1,_uv));}}})(_t0),[0,_sX],_])),_uw=_ut,_ux=jsDrawText(_sX,E(new T(function(){return [0,toJSStr(unAppCStr("Frame #: ",new T(function(){return _cS(0,E(_st)[1],_1);})))];}))[1],20,20),_uy=jsDrawText(_sX,E(new T(function(){return [0,toJSStr(unAppCStr("Time Elapsed: ",new T(function(){var _uz=E(_sA),_uA=E(_su),_uB=_6J(_a0(_cY(_uz[1],_uz[2]),_cY(_uA[1],_uA[2])),_1n,_2n,_1n);return A(_6Z,[_2o,_sj,_2M(_uB[1],_uB[2])[1],_1]);})))];}))[1],20,30),_uC=jsDrawText(_sX,E(new T(function(){return [0,toJSStr(unAppCStr("FPS: ",new T(function(){var _uD=E(_sA),_uE=E(_sv),_uF=_6J(_a0(_cY(_uD[1],_uD[2]),_cY(_uE[1],_uE[2])),_1n,_2n,_1n);return _sk(5,new T(function(){return A(_6Z,[_2o,_sj,1/_2M(_uF[1],_uF[2])[1],_1]);},1));})))];}))[1],20,40),_uG=B(_sq(_sW,_ss,new T(function(){return [0,E(_st)[1]+1|0];}),_su,_sA,_)),_uH=_uG,_uI=_dY(_b,_sh,_),_uJ=_uI,_uK=E(_uJ);if(!_uK[0]){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:91:13-20");}),_);}else{if(!_d5(_uK[1],_sZ[2])){return _2u;}else{var _uL=E(B(A(_uH,[_sZ]))[1]),_uM=jsSetTimeout(10,_uL);return _2u;}}},_sD];};},_uN=[0,0],_uO=new T(function(){return [0,"click"];}),_uP=function(_uQ){return [0,E(_uQ)[1]];},_uR=new T(function(){return unCStr("The given Number can\'t be represented as an Int");}),_uS=new T(function(){return unCStr("Tried to deserialize a non-Number to an Int");}),_uT=function(_uU){var _uV=E(_uU);if(!_uV[0]){return E([1,_1]);}else{var _uW=E(_uV[1]);if(!_uW[0]){var _uX=_uW[1],_uY=_uX&4294967295;if(_uY!=_uX){return E([0,_uR]);}else{var _uZ=_uT(_uV[2]);return _uZ[0]==0?[0,_uZ[1]]:[1,[1,[0,_uY],_uZ[1]]];}}else{return E([0,_uS]);}}},_v0=[0,_uP,function(_v1){return [3,E(_4(_uP,_v1))];},function(_v2){var _v3=E(_v2);if(!_v3[0]){var _v4=_v3[1],_v5=_v4&4294967295;return _v5!=_v4?E([0,_uR]):[1,[0,_v5]];}else{return E([0,_uS]);}},function(_v6){var _v7=E(_v6);return _v7[0]==3?_uT(_v7[1]):E([0,_8]);}],_v8=new T(function(){return [0,toJSStr([1,[0,93],_1])];}),_v9=new T(function(){return [0,toJSStr([1,[0,125],_1])];}),_va=new T(function(){return [0,toJSStr([1,[0,58],_1])];}),_vb=new T(function(){return [0,toJSStr([1,[0,44],_1])];}),_vc=new T(function(){return [0,toJSStr([1,[0,91],_1])];}),_vd=new T(function(){return [0,toJSStr([1,[0,123],_1])];}),_ve=new T(function(){return [0,toJSStr([1,[0,34],_1])];}),_vf=function(_vg,_vh){var _vi=E(_vh);switch(_vi[0]){case 0:return [0,new T(function(){var _vj=jsShow(_vi[1]),_vk=_vj;return [0,_vk];}),_vg];case 1:return [0,new T(function(){var _vl=jsStringify(_vi[1]),_vm=_vl;return [0,_vm];}),_vg];case 2:return !E(_vi[1])?[0,new T(function(){return [0,"false"];}),_vg]:[0,new T(function(){return [0,"true"];}),_vg];case 3:var _vn=E(_vi[1]);return _vn[0]==0?[0,_vc,[1,_v8,_vg]]:[0,_vc,new T(function(){var _vo=_vf(new T(function(){var _vp=function(_vq){var _vr=E(_vq);return _vr[0]==0?E([1,_v8,_vg]):[1,_vb,new T(function(){var _vs=_vf(new T(function(){return _vp(_vr[2]);}),_vr[1]);return [1,_vs[1],_vs[2]];})];};return _vp(_vn[2]);}),_vn[1]);return [1,_vo[1],_vo[2]];})];case 4:var _vt=E(_vi[1]);if(!_vt[0]){return [0,_vd,[1,_v9,_vg]];}else{var _vu=E(_vt[1]);return [0,_vd,[1,new T(function(){return (function(_vv){var _vw=jsStringify(E(_vv)[1]),_vx=_vw;return [0,_vx];})(_vu[1]);}),[1,_va,new T(function(){var _vy=_vf(new T(function(){var _vz=function(_vA){var _vB=E(_vA);if(!_vB[0]){return E([1,_v9,_vg]);}else{var _vC=E(_vB[1]);return [1,_vb,[1,_ve,[1,_vC[1],[1,_ve,[1,_va,new T(function(){var _vD=_vf(new T(function(){return _vz(_vB[2]);}),_vC[2]);return [1,_vD[1],_vD[2]];})]]]]];}};return _vz(_vt[2]);}),_vu[2]);return [1,_vy[1],_vy[2]];})]]];}break;default:return [0,new T(function(){return [0,"null"];}),_vg];}},_vE=function(_vF,_vG){return function(_vH,_){var _vI=B(A(new T(function(){return A(_dW,[E(new T(function(){return [0,"(function(k,v) {localStorage.setItem(k,v);})"];}))[1],E(toJSStr(E(_vG)))]);}),[E((function(_vJ){var _vK=jsCat(new T(function(){var _vL=_vf(_1,_vJ);return [1,_vL[1],_vL[2]];}),E(new T(function(){return [0,toJSStr(_1)];}))[1]),_vM=_vK;return E(_vM);})(B(A(new T(function(){return _A(_vF);}),[_vH])))),_])),_vN=_vI;return _2u;};},_vO=new T(function(){return _vE(new T(function(){return _T(_v0,_v0);}),_si);}),_vP=new T(function(){return _vE(_b,_sh);}),_vQ=new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:125:13-20");}),_vR=function(_vS,_vT,_vU,_vV){return _37(function(_vW,_vX){return [1,new T(function(){var _vY=E(_vX);return [0,new T(function(){return _7R(_7I,_vS,[1,_vU,_1],_vY[1]);}),_vY[2],_vY[3]];})];},_vT,_37(function(_vZ,_w0){return [1,new T(function(){var _w1=E(_w0);return [0,_w1[1],_w1[2],new T(function(){return _7R(_7I,_vT,[1,_vU,_1],_w1[3]);})];})];},_vS,_vV));},_w2=[0,0.5],_w3=[0,70],_w4=[0,[0,3],_w3,_w2],_w5=function(_w6){var _w7=E(_w6);switch(_w7[0]){case 0:return 0;case 1:return 1;default:return _w5(_w7[3])+_w5(_w7[4])|0;}},_w8=new T(function(){return [0,_w5(_8j)+1|0];}),_w9=function(_wa,_wb){var _wc=E(_wa);if(!_wc[0]){return [0,_1,_wb];}else{var _wd=_wc[1],_we=new T(function(){var _wf=E(_wb);return [0,[0,_w8,_wd,_w4],[0,new T(function(){return _vR(E(_w8)[1],E(_wd)[1],_w4,_wf[1]);}),_wf[2]]];}),_wg=new T(function(){var _wh=_w9(_wc[2],new T(function(){return E(E(_we)[2]);}));return [0,_wh[1],_wh[2]];});return [0,[1,new T(function(){return E(E(_we)[1]);}),new T(function(){return E(E(_wg)[1]);})],new T(function(){return E(E(_wg)[2]);})];}},_wi=function(_wj,_wk,_wl,_wm){var _wn=E(_wl);if(!_wn[0]){return E(_wk);}else{var _wo=E(_wm);if(!_wo[0]){return E(_wk);}else{return new F(function(){return A(_wj,[_wn[1],_wo[1],new T(function(){return B(_wi(_wj,_wk,_wn[2],_wo[2]));})]);});}}},_wp=[0,1],_wq=function(_wr,_ws,_wt){var _wu=E(_wt);switch(_wu[0]){case 0:return [1,_wr,_ws];case 1:var _wv=_wu[1];if(_wr!=_wv){var _ww=(_wr>>>0^_wv>>>0)>>>0,_wx=(_ww|_ww>>>1)>>>0,_wy=(_wx|_wx>>>2)>>>0,_wz=(_wy|_wy>>>4)>>>0,_wA=(_wz|_wz>>>8)>>>0,_wB=(_wA|_wA>>>16)>>>0,_wC=(_wB^_wB>>>1)>>>0&4294967295,_wD=_wC>>>0;return (_wr>>>0&_wD)>>>0==0?[2,(_wr>>>0&((_wD-1>>>0^4294967295)>>>0^_wD)>>>0)>>>0&4294967295,_wC,E([1,_wr,_ws]),E(_wu)]:[2,(_wr>>>0&((_wD-1>>>0^4294967295)>>>0^_wD)>>>0)>>>0&4294967295,_wC,E(_wu),E([1,_wr,_ws])];}else{return [1,_wr,_ws];}break;default:var _wE=_wu[1],_wF=_wu[2],_wG=_wu[3],_wH=_wu[4],_wI=_wF>>>0;if(((_wr>>>0&((_wI-1>>>0^4294967295)>>>0^_wI)>>>0)>>>0&4294967295)==_wE){return (_wr>>>0&_wI)>>>0==0?[2,_wE,_wF,E(_wq(_wr,_ws,_wG)),E(_wH)]:[2,_wE,_wF,E(_wG),E(_wq(_wr,_ws,_wH))];}else{var _wJ=(_wr>>>0^_wE>>>0)>>>0,_wK=(_wJ|_wJ>>>1)>>>0,_wL=(_wK|_wK>>>2)>>>0,_wM=(_wL|_wL>>>4)>>>0,_wN=(_wM|_wM>>>8)>>>0,_wO=(_wN|_wN>>>16)>>>0,_wP=(_wO^_wO>>>1)>>>0&4294967295,_wQ=_wP>>>0;return (_wr>>>0&_wQ)>>>0==0?[2,(_wr>>>0&((_wQ-1>>>0^4294967295)>>>0^_wQ)>>>0)>>>0&4294967295,_wP,E([1,_wr,_ws]),E(_wu)]:[2,(_wr>>>0&((_wQ-1>>>0^4294967295)>>>0^_wQ)>>>0)>>>0&4294967295,_wP,E(_wu),E([1,_wr,_ws])];}}},_wR=[0,1],_wS=[0,0],_wT=[0,_wS,_wS],_wU=[0,200],_wV=function(_wW,_wX){while(1){var _wY=E(_wW);if(!_wY){return E(_wX);}else{var _wZ=E(_wX);if(!_wZ[0]){return [0];}else{_wW=_wY-1|0;_wX=_wZ[2];continue;}}}},_x0=[0,[0,function(_x1,_x2){return [0,E(_x1)[1]+E(_x2)[1]];},function(_x3,_x4){return [0,E(_x3)[1]*E(_x4)[1]];},function(_x5,_x6){return [0,E(_x5)[1]-E(_x6)[1]];},_7f,function(_x7){var _x8=E(_x7),_x9=_x8[1];return _x9<0?[0, -_x9]:E(_x8);},function(_xa){var _xb=E(E(_xa)[1]);return _xb==0?E([0,0]):_xb<=0?E([0,-1]):E([0,1]);},function(_xc){return [0,(function(_xd){var _xe=E(_xd);return _xe[0]==0?_xe[1]:I_toNumber(_xe[1]);})(_xc)];}],function(_xf,_xg){return [0,E(_xf)[1]/E(_xg)[1]];},function(_xh){return [0,1/E(_xh)[1]];},function(_xi){var _xj=E(_xi);return _2M(_xj[1],_xj[2]);}],_xk=[0,[0,function(_xl,_xm){return E(_xl)[1]==E(_xm)[1];},function(_xn,_xo){return E(_xn)[1]!=E(_xo)[1]?true:false;}],function(_xp,_xq){var _xr=E(_xp)[1],_xs=E(_xq)[1];return _xr>=_xs?_xr!=_xs?2:1:0;},function(_xt,_xu){return E(_xt)[1]<E(_xu)[1];},function(_xv,_xw){return E(_xv)[1]>=E(_xw)[1];},function(_xx,_xy){return E(_xx)[1]>E(_xy)[1];},function(_xz,_xA){return E(_xz)[1]<=E(_xA)[1];},function(_xB,_xC){var _xD=E(_xB),_xE=E(_xC);return _xD[1]>_xE[1]?E(_xD):E(_xE);},function(_xF,_xG){var _xH=E(_xF),_xI=E(_xG);return _xH[1]>_xI[1]?E(_xI):E(_xH);}],_xJ=function(_xK){return E(E(_xK)[1]);},_xL=function(_xM){return E(E(_xM)[1]);},_xN=function(_xO){return E(E(_xO)[4]);},_xP=function(_xQ,_xR,_xS){var _xT=E(_xR),_xU=E(_xS);return [0,_xT,new T(function(){var _xV=_xJ(_xQ),_xW=_xP(_xQ,_xU,B(A(_xV[3],[new T(function(){return B(A(_xV[1],[_xU,_xU]));}),_xT])));return [1,_xW[1],_xW[2]];})];},_xX=function(_xY,_xZ){var _y0=E(_xZ);if(!_y0[0]){return [0];}else{var _y1=_y0[1];return !B(A(_xY,[_y1]))?[0]:[1,_y1,new T(function(){return _xX(_xY,_y0[2]);})];}},_y2=function(_y3,_y4,_y5,_y6,_y7){return _xX(new T(function(){var _y8=new T(function(){return _xJ(_y4);}),_y9=new T(function(){return A(function(_ya){return E(E(_ya)[2]);},[_y4,new T(function(){return A(function(_yb){return E(E(_yb)[3]);},[_y8,_y6,_y5]);}),new T(function(){return A(_bg,[_y8,[0,2]]);})]);});if(!A(_xN,[_y3,_y6,_y5])){var _yc=function(_yd){return new F(function(){return A(new T(function(){return _xN(_y3);}),[_yd,new T(function(){return A(_xL,[_y8,_y7,_y9]);})]);});};}else{var _yc=function(_ye){return new F(function(){return A(new T(function(){return (function(_yf){return E(E(_yf)[6]);})(_y3);}),[_ye,new T(function(){return A(_xL,[_y8,_y7,_y9]);})]);});};}return _yc;}),(function(_yg,_yh,_yi){var _yj=_xP(_yg,_yh,_yi);return [1,_yj[1],_yj[2]];})(_y4,_y5,_y6));},_yk=[0,6.2831854820251465],_yl=new T(function(){return _wV(1,new T(function(){return _y2(_xk,_x0,_wS,[0,0.39269909262657166],_yk);}));}),_ym=function(_yn,_yo,_yp){var _yq=E(_yn);if(!_yq[0]){return [0,_1,[0,_yo,_yp]];}else{var _yr=_yq[1],_ys=new T(function(){return [0,_w5(_yo)+1|0];}),_yt=new T(function(){var _yu=_ym(_yq[2],new T(function(){return _wq(E(_ys)[1],[0,_8j,[0,[0,new T(function(){return [0,Math.cos(E(_yr)[1])*100+400];}),new T(function(){return [0,Math.sin(E(_yr)[1])*100+200];})],_wT,_wR],_8j],_yo);}),_yp);return [0,_yu[1],_yu[2]];});return [0,[1,_ys,new T(function(){return E(E(_yt)[1]);})],new T(function(){return E(E(_yt)[2]);})];}},_yv=new T(function(){var _yw=_ym(_yl,new T(function(){return _wq(E(_w8)[1],[0,_8j,[0,[0,[0,400],_wU],_wT,_wR],_8j],_8j);}),new T(function(){return unCStr("testWorldBlob");}));return [0,_yw[1],_yw[2]];}),_yx=new T(function(){return E(E(_yv)[1]);}),_yy=new T(function(){return err(new T(function(){return unCStr("Prelude.cycle: empty list");}));}),_yz=function(_yA,_yB){var _yC=_gl(_yB,0);if(_yC>0){var _yD=E(_yA)[1];if(_yD>=0){var _yE=E(_yB);return _yE[0]==0?E(_yy):_yC<0?[0]:_sk(_yC,new T(function(){var _yF=new T(function(){return _1v(_yE,_yF);});return _wV(_yD,_yF);},1));}else{var _yG=E(_yB);if(!_yG[0]){return E(_yy);}else{if(_yC<0){return [0];}else{var _yH=new T(function(){return _1v(_yG,_yH);});return _sk(_yC,_yH);}}}}else{return [0];}},_yI=new T(function(){return _yz(_wp,_yx);}),_yJ=function(_yK){return [0,_2u,_yK];},_yL=function(_yM,_yN,_yO,_yP){return new F(function(){return A(_yO,[new T(function(){var _yQ=E(_yP);return [0,new T(function(){return _vR(E(_yM)[1],E(_yN)[1],[0,[0,0.3],_w3,_w2],_yQ[1]);}),_yQ[2]];})]);});},_yR=[0,50],_yS=function(_yT,_yU,_yV,_yW){return new F(function(){return A(_yV,[new T(function(){var _yX=E(_yW);return [0,new T(function(){return _vR(E(_yT)[1],E(_yU)[1],[0,_wR,_yR,_w2],_yX[1]);}),_yX[2]];})]);});},_yY=function(_yZ,_z0){var _z1=E(_yZ);if(!_z1[0]){return [0,_1,_z0];}else{var _z2=_z1[1],_z3=new T(function(){var _z4=E(_z0),_z5=_z4[1],_z6=new T(function(){return [0,_w5(_z5)+1|0];});return [0,_z6,[0,new T(function(){return _wq(E(_z6)[1],[0,_8j,[0,[0,new T(function(){return [0,Math.cos(E(_z2)[1])*150+400];}),new T(function(){return [0,Math.sin(E(_z2)[1])*150+200];})],_wT,_wR],_8j],_z5);}),_z4[2]]];}),_z7=new T(function(){var _z8=_yY(_z1[2],new T(function(){return E(E(_z3)[2]);}));return [0,_z8[1],_z8[2]];});return [0,[1,new T(function(){return E(E(_z3)[1]);}),new T(function(){return E(E(_z7)[1]);})],new T(function(){return E(E(_z7)[2]);})];}},_z9=new T(function(){var _za=_yY(_yl,new T(function(){return E(E(_yv)[2]);}));return [0,_za[1],_za[2]];}),_zb=new T(function(){return E(E(_z9)[1]);}),_zc=new T(function(){return _yz(_wp,_zb);}),_zd=function(_ze,_zf,_zg){var _zh=E(_ze);if(!_zh[0]){return [0,_1,[0,_zf,_zg]];}else{var _zi=_zh[1],_zj=new T(function(){return [0,_w5(_zf)+1|0];}),_zk=new T(function(){var _zl=_zd(_zh[2],new T(function(){return _wq(E(_zj)[1],[0,_8j,[0,[0,new T(function(){return [0,Math.cos(E(_zi)[1])*100+400];}),new T(function(){return [0,Math.sin(E(_zi)[1])*100+200];})],_wT,_wR],_8j],_zf);}),_zg);return [0,_zl[1],_zl[2]];});return [0,[1,_zj,new T(function(){return E(E(_zk)[1]);})],new T(function(){return E(E(_zk)[2]);})];}},_zm=new T(function(){var _zn=_zd(new T(function(){return _wV(1,new T(function(){return _y2(_xk,_x0,_wS,[0,0.19634954631328583],_yk);}));}),_8j,new T(function(){return unCStr("testWorldCircle");}));return [0,_zn[1],_zn[2]];}),_zo=new T(function(){return E(E(_zm)[1]);}),_zp=new T(function(){return _1v(_zo,_zp);}),_zq=[0,_wR,[0,25],_wS],_zr=[0,0.1],_zs=[0,_yR,_yR,_zr],_zt=[0,5],_zu=new T(function(){return _y2(_xk,_x0,_wS,_wR,_zt);}),_zv=function(_zw,_zx){var _zy=E(_zw);if(!_zy[0]){return [0,_1,_zx];}else{var _zz=new T(function(){var _zA=function(_zB,_zC){var _zD=E(_zB);if(!_zD[0]){return [0,_1,_zC];}else{var _zE=new T(function(){var _zF=E(_zC),_zG=_zF[1],_zH=new T(function(){return [0,_w5(_zG)+1|0];});return [0,_zH,[0,new T(function(){return _wq(E(_zH)[1],[0,_8j,[0,[0,new T(function(){return [0,400+E(_zy[1])[1]*50];}),new T(function(){return [0,50+E(_zD[1])[1]*50];})],_wT,_wR],_8j],_zG);}),_zF[2]]];}),_zI=new T(function(){var _zJ=_zA(_zD[2],new T(function(){return E(E(_zE)[2]);}));return [0,_zJ[1],_zJ[2]];});return [0,[1,new T(function(){return E(E(_zE)[1]);}),new T(function(){return E(E(_zI)[1]);})],new T(function(){return E(E(_zI)[2]);})];}},_zK=_zA(_zu,_zx);return [0,_zK[1],_zK[2]];}),_zL=new T(function(){return E(E(_zz)[1]);}),_zM=new T(function(){var _zN=_zv(_zy[2],new T(function(){return E(B(A(_wi,[function(_zO,_zP,_zQ,_zR){return new F(function(){return A(_zQ,[new T(function(){var _zS=E(_zR);return [0,new T(function(){return _vR(E(_zO)[1],E(_zP)[1],_zs,_zS[1]);}),_zS[2]];})]);});},_yJ,_zL,new T(function(){return _wV(1,_zL);},1),new T(function(){return E(E(_zz)[2]);})]))[2]);}));return [0,_zN[1],_zN[2]];});return [0,[1,_zL,new T(function(){return E(E(_zM)[1]);})],new T(function(){return E(E(_zM)[2]);})];}},_zT=new T(function(){var _zU=_zv(_zu,[0,_8j,new T(function(){return unCStr("testWorldGrid");})]);return [0,_zU[1],_zU[2]];}),_zV=function(_zW){var _zX=E(_zW);if(!_zX[0]){return [0];}else{return _1v(_zX[1],new T(function(){return _zV(_zX[2]);},1));}},_zY=function(_zZ,_A0,_A1){var _A2=E(_A0);if(!_A2[0]){return [0];}else{var _A3=E(_A1);return _A3[0]==0?[0]:[1,new T(function(){return B(A(_zZ,[_A2[1],_A3[1]]));}),new T(function(){return _zY(_zZ,_A2[2],_A3[2]);})];}},_A4=new T(function(){return E(E(_zT)[1]);}),_A5=function(_A6,_A7,_A8){var _A9=E(_A6);if(!_A9[0]){return [0,_1,[0,_A7,_A8]];}else{var _Aa=_A9[1],_Ab=new T(function(){return [0,_w5(_A7)+1|0];}),_Ac=new T(function(){var _Ad=_A5(_A9[2],new T(function(){return _wq(E(_Ab)[1],[0,_8j,[0,[0,new T(function(){return [0,Math.cos(E(_Aa)[1])*100+400];}),new T(function(){return [0,Math.sin(E(_Aa)[1])*100+200];})],_wT,_wR],_8j],_A7);}),_A8);return [0,_Ad[1],_Ad[2]];});return [0,[1,_Ab,new T(function(){return E(E(_Ac)[1]);})],new T(function(){return E(E(_Ac)[2]);})];}},_Ae=new T(function(){var _Af=_A5(_yl,_8j,new T(function(){return unCStr("testWorldRigidSkin");}));return [0,_Af[1],_Af[2]];}),_Ag=new T(function(){return E(E(_Ae)[1]);}),_Ah=new T(function(){return unCStr("testWorldRod");}),_Ai=[0,100],_Aj=new T(function(){return _wq(E(_w8)[1],[0,_8j,[0,[0,_Ai,_Ai],_wT,_wR],_8j],_8j);}),_Ak=new T(function(){return [0,_w5(_Aj)+1|0];}),_Al=new T(function(){return _vR(E(_w8)[1],E(_Ak)[1],[0,_wR,_yR,_wS],new T(function(){return _wq(E(_Ak)[1],[0,_8j,[0,[0,_wU,_wU],_wT,_wR],_8j],_Aj);}));});function _Am(_){return new F(function(){return (function(_){var _An=_rX(_),_Ao=_An,_Ap=jsFind("canvas"),_Aq=_Ap,_Ar=E(_Aq);if(!_Ar[0]){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:110:5-19");}),_);}else{var _As=E(_Ar[1]),_At=_As[1],_Au=jsHasCtx2D(_At),_Av=_Au;if(!E(_Av)){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:111:5-15");}),_);}else{var _Aw=jsGetCtx2D(_At),_Ax=_Aw,_Ay=[0,_Ax,_At],_Az=B(A(_vP,[new T(function(){return unCStr("testWorldRod");}),_])),_AA=_Az,_AB=jsSetCB(_At,E(new T(function(){return [0,"mousedown"];}))[1],function(_AC,_AD){return new F(function(){return A(_vO,[_AD]);});}),_AE=_AB,_AF=jsSetCB(_At,E(new T(function(){return [0,"mouseup"];}))[1],function(_AG,_AH,_){var _AI=B(A(new T(function(){return _dW("(function(k) {localStorage.removeItem(k);})");}),[E(toJSStr(E(_si))),_])),_AJ=_AI;return _2u;}),_AK=_AF,_AL=jsSetCB(_At,E(new T(function(){return [0,"mousemove"];}))[1],function(_AM,_){var _AN=_dY(_1m,_si,_),_AO=_AN;return E(_AO)[0]==0?_2u:B(A(_vO,[_AM,_]));}),_AP=_AL,_AQ=B(A(_vP,[_Ah,_])),_AR=_AQ,_AS=new T(function(){var _AT=_bk(_Ao);return [0,_AT[1],_AT[2]];}),_AU=B(_sq(_Ay,_As,_uN,_AS,_AS,_)),_AV=_AU,_AW=B(A(B(A(_AV,[[0,_Al,_Ah]]))[1],[_])),_AX=_AW,_AY=jsFind("btnRod"),_AZ=_AY,_B0=E(_AZ);if(!_B0[0]){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:130:5-13");}),_);}else{var _B1=function(_B2,_B3,_){var _B4=new T(function(){return E(E(_B3)[2]);}),_B5=jsSetCB(_B2,E(_uO)[1],function(_B6,_B7,_){var _B8=_dY(_b,_sh,_),_B9=_B8,_Ba=E(_B9);if(!_Ba[0]){return _dO(_vQ,_);}else{if(!_v(_Ba[1],_B4)){var _Bb=B(A(_vP,[_B4,_])),_Bc=_Bb,_Bd=B(_sq(_Ay,_As,_uN,_AS,_AS,_)),_Be=_Bd;return new F(function(){return A(B(A(_Be,[_B3]))[1],[_]);});}else{return _2u;}}}),_Bf=_B5;return new T(function(){return E(_Bf)==0?false:true;});},_Bg=B((function(_Bh,_Bi,_Bj,_){var _Bk=jsSetCB(_Bh,E(_uO)[1],function(_Bl,_Bm,_){var _Bn=_dY(_b,_sh,_),_Bo=_Bn,_Bp=E(_Bo);if(!_Bp[0]){return _dO(_vQ,_);}else{if(!_v(_Bp[1],_Bj)){var _Bq=B(A(_vP,[_Bj,_])),_Br=_Bq,_Bs=B(_sq(_Ay,_As,_uN,_AS,_AS,_)),_Bt=_Bs;return new F(function(){return A(B(A(_Bt,[[0,_Bi,_Bj]]))[1],[_]);});}else{return _2u;}}}),_Bu=_Bk;return new T(function(){return E(_Bu)==0?false:true;});})(E(_B0[1])[1],_Al,_Ah,_)),_Bv=_Bg,_Bw=jsFind("btnCircle"),_Bx=_Bw,_By=E(_Bx);if(!_By[0]){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:132:5-13");}),_);}else{var _Bz=B(_B1(E(_By[1])[1],new T(function(){return E(B(A(_wi,[function(_BA,_BB,_BC,_BD){var _BE=new T(function(){var _BF=E(_BD);return [0,[0,_BA,_BB,_zq],[0,new T(function(){return _vR(E(_BA)[1],E(_BB)[1],_zq,_BF[1]);}),_BF[2]]];}),_BG=new T(function(){return B(A(_BC,[new T(function(){return E(E(_BE)[2]);})]));});return [0,[1,new T(function(){return E(E(_BE)[1]);}),new T(function(){return E(E(_BG)[1]);})],new T(function(){return E(E(_BG)[2]);})];},function(_BH){return [0,_1,_BH];},_zo,new T(function(){var _BI=_gl(_zo,0);if(_BI>0){if(!E(_zo)[0]){var _BJ=E(_yy);}else{var _BJ=_BI<0?[0]:_sk(_BI,new T(function(){return _wV(1,_zp);}));}var _BK=_BJ;}else{var _BK=[0];}var _BL=_BK,_BM=_BL;return _BM;}),new T(function(){return E(E(_zm)[2]);})]))[2]);}),_)),_BN=_Bz,_BO=jsFind("btnRigidSkin"),_BP=_BO,_BQ=E(_BP);if(!_BQ[0]){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:134:5-13");}),_);}else{var _BR=B(_B1(E(_BQ[1])[1],new T(function(){return E(B(A(_wi,[function(_BS,_BT,_BU,_BV){return new F(function(){return A(_BU,[new T(function(){var _BW=E(_BV);return [0,new T(function(){return _vR(E(_BS)[1],E(_BT)[1],[0,_zt,_w3,_zr],_BW[1]);}),_BW[2]];})]);});},_yJ,_Ag,new T(function(){return _yz([0,3],_Ag);}),new T(function(){return E(B(A(_wi,[function(_BX,_BY,_BZ,_C0){return new F(function(){return A(_BZ,[new T(function(){var _C1=E(_C0);return [0,new T(function(){return _vR(E(_BX)[1],E(_BY)[1],[0,_zt,[0,60],_zr],_C1[1]);}),_C1[2]];})]);});},_yJ,_Ag,new T(function(){return _yz([0,2],_Ag);}),new T(function(){return E(B(A(_wi,[function(_C2,_C3,_C4,_C5){return new F(function(){return A(_C4,[new T(function(){var _C6=E(_C5);return [0,new T(function(){return _vR(E(_C2)[1],E(_C3)[1],[0,_zt,_yR,_zr],_C6[1]);}),_C6[2]];})]);});},_yJ,_Ag,new T(function(){return _yz(_wp,_Ag);}),new T(function(){return E(E(_Ae)[2]);})]))[2]);})]))[2]);})]))[2]);}),_)),_C7=_BR,_C8=jsFind("btnBlob"),_C9=_C8,_Ca=E(_C9);if(!_Ca[0]){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:136:5-13");}),_);}else{var _Cb=B(_B1(E(_Ca[1])[1],new T(function(){return E(_w9(_yx,new T(function(){return E(B(A(_wi,[_yL,_yJ,_yx,_zc,new T(function(){return E(B(A(_wi,[_yL,_yJ,_yI,_zb,new T(function(){return E(B(A(_wi,[_yL,_yJ,_yx,_zb,new T(function(){return E(B(A(_wi,[_yS,_yJ,_zb,_zc,new T(function(){return E(B(A(_wi,[_yS,_yJ,_yx,_yI,new T(function(){return E(E(_z9)[2]);})]))[2]);})]))[2]);})]))[2]);})]))[2]);})]))[2]);}))[2]);}),_)),_Cc=_Cb,_Cd=jsFind("btnGrid"),_Ce=_Cd,_Cf=E(_Ce);if(!_Cf[0]){return _dO(new T(function(){return unCStr("Pattern match failure in do expression at Main.hs:138:5-13");}),_);}else{var _Cg=B(_B1(E(_Cf[1])[1],new T(function(){return E((function(_Ch,_Ci){while(1){var _Cj=(function(_Ck,_Cl){var _Cm=E(_Ck);if(!_Cm[0]){return [0,_2u,_Cl];}else{_Ch=_Cm[2];_Ci=new T(function(){return E(B(A(_Cm[1],[_Cl]))[2]);});return null;}})(_Ch,_Ci);if(_Cj!=null){return _Cj;}}})(new T(function(){return _zV(new T(function(){return _zY(function(_Cn,_Co){return _zY(function(_Cp,_Cq,_Cr){var _Cs=E(_Cr);return [0,[0,_Cp,_Cq,_zs],[0,new T(function(){return _vR(E(_Cp)[1],E(_Cq)[1],_zs,_Cs[1]);}),_Cs[2]]];},_Cn,_Co);},new T(function(){return _wV(1,_A4);}),_A4);}));}),new T(function(){return E(E(_zT)[2]);}))[2]);}),_)),_Ct=_Cg;return _2u;}}}}}}}})(_);});};
var hasteMain = function() {B(A(_Am, [0]));};window.onload = hasteMain;