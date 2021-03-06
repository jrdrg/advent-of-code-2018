// Generated by BUCKLESCRIPT VERSION 4.0.14, PLEASE EDIT WITH CARE
'use strict';

var $$Map = require("bs-platform/lib/js/map.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_obj = require("bs-platform/lib/js/caml_obj.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Input$Day3 = require("./Input.bs.js");
var Belt_MapInt = require("bs-platform/lib/js/belt_MapInt.js");
var Belt_Option = require("bs-platform/lib/js/belt_Option.js");
var Belt_SetInt = require("bs-platform/lib/js/belt_SetInt.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var Caml_primitive = require("bs-platform/lib/js/caml_primitive.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var compare = Caml_obj.caml_compare;

var TupleMap = $$Map.Make(/* module */[/* compare */compare]);

function getWithDefault(key, f) {
  try {
    return Curry._2(TupleMap[/* find */21], key, f);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return Belt_SetInt.empty;
    } else {
      throw exn;
    }
  }
}

function parseClaim(claim) {
  var matches = claim.match((/#(\d+)\s*@\s*(\d+),(\d+)\:\s*(\d+)x(\d+)/));
  var match = Belt_Option.getWithDefault(matches === null ? undefined : Caml_option.some(matches), /* array */[]);
  if (match.length !== 6) {
    return /* tuple */[
            -1,
            0,
            0,
            0,
            0
          ];
  } else {
    var id = match[1];
    var px = match[2];
    var py = match[3];
    var w = match[4];
    var h = match[5];
    return /* tuple */[
            Caml_format.caml_int_of_string(id),
            Caml_format.caml_int_of_string(px),
            Caml_format.caml_int_of_string(py),
            Caml_format.caml_int_of_string(w),
            Caml_format.caml_int_of_string(h)
          ];
  }
}

function generateKeys(w, h) {
  var area = Caml_int32.imul(w, h);
  return $$Array.init(area, (function (i) {
                var x = Caml_int32.div(i, h);
                var y = Caml_int32.mod_(i, h);
                return /* tuple */[
                        x,
                        y
                      ];
              }));
}

function addClaim(claims, id, pos, size) {
  var py = pos[1];
  var px = pos[0];
  return $$Array.fold_left((function (c, param) {
                var x = param[0] + px | 0;
                var y = param[1] + py | 0;
                var id$1 = id;
                var c$1 = c;
                var key = /* tuple */[
                  x,
                  y
                ];
                var v = Belt_SetInt.add(getWithDefault(key, c$1), id$1);
                return Curry._3(TupleMap[/* add */3], key, v, c$1);
              }), claims, generateKeys(size[0], size[1]));
}

function countOverlapping(claims) {
  var count = /* record */[/* contents */0];
  Curry._2(TupleMap[/* iter */9], (function (_k, _v) {
          count[0] = count[0] + 1 | 0;
          return /* () */0;
        }), Curry._2(TupleMap[/* filter */13], (function (_k, v) {
              return Belt_SetInt.size(v) > 1;
            }), claims));
  return count[0];
}

var claims = $$Array.fold_left((function (c, inp) {
        var match = parseClaim(inp);
        return addClaim(c, match[0], /* tuple */[
                    match[1],
                    match[2]
                  ], /* tuple */[
                    match[3],
                    match[4]
                  ]);
      }), TupleMap[/* empty */0], Input$Day3.input);

var overlapping = countOverlapping(claims);

console.log("Part 1:", overlapping);

function findNonOverlapping(claims) {
  var c = /* record */[/* contents */Belt_MapInt.empty];
  Curry._2(TupleMap[/* iter */9], (function (_k, v) {
          var idCount = Belt_SetInt.size(v);
          return Belt_SetInt.forEach(v, (function (i) {
                        var existing = Caml_primitive.caml_int_max(Belt_MapInt.getWithDefault(c[0], i, 1), idCount);
                        c[0] = Belt_MapInt.set(c[0], i, existing);
                        return /* () */0;
                      }));
        }), claims);
  var noOverlap = Belt_MapInt.findFirstBy(c[0], (function (_k, v) {
          return v === 1;
        }));
  return Belt_Option.mapWithDefault(noOverlap, -1, (function (param) {
                return param[0];
              }));
}

var nonOverlappingId = findNonOverlapping(claims);

console.log("Part 2:", nonOverlappingId);

exports.TupleMap = TupleMap;
exports.getWithDefault = getWithDefault;
exports.parseClaim = parseClaim;
exports.generateKeys = generateKeys;
exports.addClaim = addClaim;
exports.countOverlapping = countOverlapping;
exports.claims = claims;
exports.overlapping = overlapping;
exports.findNonOverlapping = findNonOverlapping;
exports.nonOverlappingId = nonOverlappingId;
/* TupleMap Not a pure module */
